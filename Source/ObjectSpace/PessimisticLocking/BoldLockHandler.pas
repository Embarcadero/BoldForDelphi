unit BoldLockHandler;

interface

uses
  Classes,
  BoldLockRegions,
  BoldSystem,
  BoldDefs,
  BoldGuard,
  BoldDomainElement,
  BoldSubscription,
  BoldHashIndexes,
  BoldLockHolder;

resourcestring
  BOLD_GET_LOCKS_FAILED_ERROR = 'Failed to get locks';

type
  TBoldFailureGetLocksFailed = class;

  EBoldGetLocksFailed = class;

  { TBoldFailureGetLocksFailed }
  TBoldFailureGetLocksFailed = class(TBoldFailureReason)
  private
    fClientIds: TStringList;
    fConflictingRegions: TBoldRegionList;
  protected
    function GetException(const Msg: String): EBoldFailure; override;
  public
    constructor Create(reason: String; Originator: TBoldDomainElement; const ConflictingRegions: TBoldRegionList; const ClientIds: TStringList);
    destructor Destroy; override;
  end;

    { EBoldGetLocksFailed}
  EBoldGetLocksFailed = class(EBoldFailure)
  private
    FClientIds: TStringList;
    fConflictingRegions: TBoldRegionList;
  public
    constructor Create(msg: string; const ClientIds: TStrings; const ConflictingRegions: TBoldRegionList);
    destructor Destroy; override;
    property ClientIds: TStringList read fClientIds;
    property ConflictingRegions: TBoldRegionList read fConflictingRegions;
  end;

  TBoldEmptyLockHolder = class(TBoldAbstractLockHolder)
  private
    fLockList: TBoldLockList;
  protected
    function GetHeldExclusive: TBoldLockList; override;
    function GetHeldShared: TBoldLockList; override;
  public
    constructor Create;
    destructor Destroy; override;
    function Lock(Shared: TBoldLockList; Exclusive: TBoldLockList; HeldLocks, ClientsHoldingRequestedLocks: TStringList): Boolean; override;
    procedure Release(Locks: TBoldLockList); override;
    function EnsureLocks: Boolean; override;
    procedure GetPropagationEvents(EventList: TStringList); override;
  end;

  TBoldPessimisticLockHandler = class(TBoldAbstractPessimisticLockHandler)
  private
    fRequiredShared: TBoldRegionList;
    fRequiredExclusive: TBoldRegionList;
    fRequiredExplicit: TBoldRegionList;
    fKnownRequiredParents: TBoldRegionLookup;
    fKnownRequiredSubregions: TBoldRegionLookup;
    fParentsChangedRegions: TBoldRegionList;
    fSubregionsChangedRegions: TBoldRegionList;
    fLockHolder: TBoldAbstractLockHolder;
    fSubscriber: TBoldPassthroughSubscriber;
    fOnActivityPropgress: TBoldLockManagerProgressEvent;
    fOnActivityStart: TNotifyEvent;
    fOnActivityEnd: TNotifyEvent;
    procedure SignalActivityStart;
    procedure SignalActivityEnd;
    function GetFactory: TBoldRegionFactory;
    procedure _RegionChanged(Region: TBoldRegion; Event: Integer);
    procedure AddHeldLocksToRegionLookup(RegionLookup: TBoldRegionLookup; AddSharedRegions: Boolean);

    procedure EnsureAllRequiredRegions;
    function GetRequiredLocks: Boolean;
    function _AnswerMayCommit(Originator: TObject; OriginalEvent: TBoldEvent;
                              RequestedEvent: TBoldRequestedEvent;
                              const Args: array of const; Subscriber: TBoldSubscriber): Boolean;
    procedure _ReceiveRolledBack(Originator: TObject; OriginalEvent: TBoldEvent;
                                RequestedEvent: TBoldRequestedEvent);
    procedure RequireRegionExplicit(Region: TBoldRegion);
    function IsHeldAsExplicitOrParent(Region: TBoldRegion): Boolean;

    function GetRequiredShared: TBoldRegionList;
    function GetRequiredExclusive: TBoldRegionList;
    function ArePropagationEventsInConflictWithRequiredRegions: Boolean;
    function IsElementInAnyRequiredRegion(Element: TBoldMember): Boolean;
    function ElementListContainsDirtyElements(Elements: TList): Boolean;
    function IsRequiredAsExplicitOrParent(Region: TBoldRegion): Boolean;
    property Factory: TBoldRegionFactory read GetFactory;
  public
    constructor CreateWithLockHolder(System: TBoldSystem; LockHolder: TBoldAbstractLockHolder);
    destructor Destroy; override;
    function LockElement(Element: TBoldDomainElement): Boolean; override;
    function EnsureLocks: Boolean; override;
    procedure ReleaseUnneededRegions; override;
    property RequiredShared: TBoldRegionList read GetRequiredShared;
    property RequiredExclusive: TBoldRegionList read GetRequiredExclusive;
    property OnActivityStart: TNotifyEvent read fOnActivityStart write fOnActivityStart;
    property OnActivityEnd: TNotifyEvent read fOnActivityEnd write fOnActivityEnd;
    property OnProgress: TBoldLockManagerProgressEvent read fOnActivityPropgress write fOnActivityPropgress;
  end;

implementation

uses
  SysUtils,
  BoldUtils,
  BoldLockingDefs,
  BoldObjectSpaceExternalEvents,
  BoldDefaultID,
  BoldIndex,
  HandlesConst,
  BoldElements;

function NewRegionListFromStrings(Locks: TStrings; Factory: TBoldRegionFactory): TBoldRegionList;
var
  i: Integer;
begin
  result := TBoldRegionList.Create;
  for i := 0 to Locks.Count - 1 do
    result.Add(Factory.GetRegionByName(Locks[i]));
end;

procedure RegionListToLockList(RegionList: TBoldRegionList; LockList: TBoldLockList);
var
  i: Integer;
begin
  for i := 0 to RegionList.Count - 1 do
    if RegionList[i].Root.BoldObjectLocator.BoldObjectID.IsStorable then
      LockList.AddLock(RegionList[i].AsString);
end;

{ TBoldPessimisticLockHandler }

constructor TBoldPessimisticLockHandler.CreateWithLockHolder(System: TBoldSystem; LockHolder: TBoldAbstractLockHolder);
begin
  inherited Create(System);
  fRequiredShared := TBoldRegionList.Create;
  fRequiredExclusive := TBoldRegionList.Create;

  fRequiredExplicit := TBoldRegionList.Create;
  fKnownRequiredParents := TBoldRegionLookup.Create;
  fKnownRequiredSubregions := TBoldRegionLookup.Create;

  fParentsChangedRegions := TBoldRegionList.Create;
  fSubregionsChangedRegions := TBoldRegionList.Create;
  fSubscriber := TBoldPassthroughSubscriber.CreateWithReceiveAndAnswer(_ReceiveRolledBack, _AnswerMayCommit);
  System.AddSubscription(fSubscriber, bqMayCommit, bqMayCommit);
  System.AddSubscription(fSubscriber, beRolledBack, beRolledBack);
  System.PessimisticLockHandler := self;

  fLockHolder := LockHolder;
  // this should be replaced with a subscription-mechanism
  Factory.OnRegionChanged := _RegionChanged;
end;

destructor TBoldPessimisticLockHandler.Destroy;
begin
  FreeAndNil(fRequiredShared);
  FreeAndNil(fRequiredExclusive);

  FreeAndNil(fRequiredExplicit);
  FreeAndNil(fKnownRequiredParents);
  FreeAndNil(fKnownRequiredSubregions);

  FreeAndNil(fParentsChangedRegions);
  FreeAndNil(fSubregionsChangedRegions);
  FreeAndNil(fSubscriber);
  inherited;
end;

procedure TBoldPessimisticLockHandler.EnsureAllRequiredRegions;
var
  Expander: TBoldRegionExpander;
  RegionsToExpand: TBoldRegionLookup;
  KnownRequiredOrHeldSubregions: TBoldRegionLookup;
  KnownRequiredOrHeldParentregions: TBoldRegionLookup;
  Guard: IBoldGuard;
  i: integer;
begin
  Guard := TBoldGuard.Create(
    Expander, RegionsToExpand,
    KnownRequiredOrHeldSubregions,
    KnownRequiredOrHeldParentregions);
  Expander := TBoldRegionExpander.Create;
  Expander.OnProgress := OnProgress;

  RegionsToExpand := TBoldRegionLookup.Create;
  KnownRequiredOrHeldSubregions := TBoldRegionLookup.Create;;
  KnownRequiredOrHeldParentRegions := TBoldRegionLookup.Create;;

  SignalActivityStart;
  try

    for i := 0 to fParentsChangedRegions.Count - 1 do
      if IsRequiredAsExplicitOrParent(fParentsChangedRegions[i]) or
         IsHeldAsExplicitOrParent(fParentsChangedRegions[i]) then
          RegionsToExpand.Add(fParentsChangedRegions[i]);


    KnownRequiredOrHeldParentRegions.AddRegionLookup(fKnownRequiredParents);
    // we know that the Explicit regions are ParentRegions...
    AddHeldLocksToRegionLookup(KnownRequiredOrHeldParentRegions, false);

    KnownRequiredOrHeldSubregions.AddRegionLookup(fKnownRequiredSubregions);
    // we know that all held regions are atleast subregions...
    AddHeldLocksToRegionLookup(KnownRequiredOrHeldSubregions, true);

    Expander.ExpandParentRegions(RegionsToExpand, KnownRequiredOrHeldParentRegions, KnownRequiredOrHeldSubregions);

    RegionsToExpand.Clear;
    RegionsToExpand.AddRegionLookup(Expander.NewParentRegions);
    RegionsToExpand.AddRegionList(fSubregionsChangedRegions);


    Expander.ExpandSubregions(RegionsToExpand, KnownRequiredOrHeldSubregions);

    fRequiredExclusive.AddList(fRequiredExplicit);
    fRequiredShared.AddRegionLookup(Expander.NewParentRegions);
    fRequiredShared.AddRegionLookup(Expander.NewSubRegions);

    fKnownRequiredSubregions.AddRegionLookup(Expander.NewSubRegions);
    fKnownRequiredParents.AddRegionLookup(Expander.NewParentRegions);
  finally
    SignalActivityEnd;
  end;

  fRequiredExplicit.Clear;
  fParentsChangedRegions.Clear;
  fSubregionsChangedRegions.Clear;
end;

function TBoldPessimisticLockHandler.IsRequiredAsExplicitOrParent(
  Region: TBoldRegion): Boolean;
begin
  result := (fRequiredExplicit.IndexOf(Region) <> -1) or
            assigned(fKnownRequiredParents.FindByID(Region.AsString));
end;

function TBoldPessimisticLockHandler.GetRequiredLocks: Boolean;
var
  SharedLocks, ExclusiveLocks: TBoldLockList;
  HeldLocks, ClientsHoldingRequestedLocks: TStringList;
  ConflictingRegions: TBoldRegionList;
begin
  SharedLocks := TBoldLockList.Create;
  ExclusiveLocks := TBoldLockList.Create;
  HeldLocks := TStringList.Create;
  ClientsHoldingRequestedLocks := TStringList.Create;
  try
    EnsureAllRequiredRegions;
    RegionListToLockList(fRequiredShared, SharedLocks);
    RegionListToLockList(fRequiredExclusive, ExclusiveLocks);
    result := fLockHolder.Lock(SharedLocks, ExclusiveLocks, HeldLocks, ClientsHoldingRequestedLocks);
    if result then
    begin
      result := not ArePropagationEventsInConflictWithRequiredRegions;
      if not result then
      begin
        SharedLocks.AddList(ExclusiveLocks);
        fLockHolder.Release(SharedLocks);
      end;
    end;
    fRequiredShared.Clear;
    fRequiredExclusive.Clear;
    fRequiredExplicit.Clear;
    fKnownRequiredParents.Clear;
    fKnownRequiredSubregions.Clear;
    if not result then
    begin
      ConflictingRegions := NewRegionListFromStrings(HeldLocks, Factory);
      SetBoldLastFailureReason(TBoldFailureGetLocksFailed.Create(BOLD_GET_LOCKS_FAILED_ERROR, nil, ConflictingRegions, ClientsHoldingRequestedLocks));
      ConflictingRegions.Free;
    end;
  finally
    SharedLocks.Free;
    ExclusiveLocks.Free;
    FreeAndNil(HeldLocks);
    FreeAndNil(ClientsHoldingRequestedLocks);
  end;
end;

function TBoldPessimisticLockHandler.LockElement(Element: TBoldDomainElement): Boolean;
var
  Regions: TBoldRegionList;
  i: Integer;
  Guard: IBoldGuard;
begin
  if (Element is TBoldAttribute) and Element.BoldDirty then
  begin
    result := true;
    exit;
  end;

  Guard := TBoldGuard.Create(Regions);
  Regions := TBoldRegionList.Create;

  Factory.GetRegionsForElement(Element, Regions);
  for i := 0 to Regions.Count - 1 do
    RequireRegionExplicit(Regions[i]);

  if System.InTransaction then
    result := true
  else
    result := GetRequiredLocks;
end;

procedure TBoldPessimisticLockHandler._RegionChanged(Region: TBoldRegion; Event: Integer);
begin
  if (fRequiredExplicit.IndexOf(Region) <> -1) or
     (fRequiredExclusive.IndexOf(Region) <> -1) or
     assigned(fKnownRequiredParents.FindByID(Region.AsString)) or
     assigned(fKnownRequiredSubregions.FindByID(Region.AsString)) or
     fLockHolder.HeldExclusive.Includes(Region.AsString) or
     fLockHolder.HeldShared.Includes(Region.AsString) then
  begin
    if Event = breParentsChanged then
      fParentsChangedRegions.Add(Region)
    else if Event = breSubregionsChanged then
      fSubregionsChangedRegions.Add(Region)
    else
      raise EBoldInternal.CreateFmt('%s._RegionChanged: Unexpected event %d', [classname, Event]);
  end;
end;

function TBoldPessimisticLockHandler._AnswerMayCommit(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent;
  const Args: array of const; Subscriber: TBoldSubscriber): Boolean;
begin
  if (OriginalEvent = bqMayCommit) and (Originator = System) then
    result := GetRequiredLocks
  else
    result := true;
end;

function TBoldPessimisticLockHandler.GetRequiredExclusive: TBoldRegionList;
begin
  EnsureAllRequiredRegions;
  Result := fRequiredExclusive;
end;

function TBoldPessimisticLockHandler.GetRequiredShared: TBoldRegionList;
begin
  EnsureAllRequiredRegions;
  Result := fRequiredShared;
end;

function TBoldPessimisticLockHandler.ArePropagationEventsInConflictWithRequiredRegions: Boolean;
var
  EventList: TStringList;
  i, j: integer;
  ClassName, MemberName, LockName: string;
  ObjectID: TBoldDefaultID;
  EventType: TBoldObjectSpaceSubscriptionType;
  CurrObj: TBoldObject;
  CurrMember: TBoldMember;
  RegionList: TBoldRegionList;
begin
  Result := false;
  EventList := TStringList.Create;
  ObjectID:= TBoldDefaultID.CreateWithClassID(0, False);
  RegionList := TBoldRegionList.Create;
  try
    fLockHolder.GetPropagationEvents(EventList);
    for i:= 0 to EventList.Count - 1 do
    begin
      EventType := TBoldObjectSpaceExternalEvent.DecodeExternalEvent(EventList[i], ClassName, MemberName, LockName, ObjectID);
      case EventType of
        bsClassChanged:;
        bsEmbeddedStateOfObjectChanged:
        begin
          CurrObj := System.EnsuredLocatorByID[ObjectID].EnsuredBoldObject;
          for j:= 0 to CurrObj.BoldMemberCount - 1 do
          begin
            CurrMember := CurrObj.BoldMembers[j];
            if CurrMember.BoldMemberRTInfo.IsStoredInObject then
              result := IsElementInAnyRequiredRegion(CurrMember);
            if result then Break;
          end;
        end;
        bsNonEmbeddedStateOfObjectChanged:
        begin
          CurrObj := System.EnsuredLocatorByID[ObjectID].EnsuredBoldObject;
          CurrMember := CurrObj.BoldMemberByExpressionName[MemberName];
          result := IsElementInAnyRequiredRegion(CurrMember);
        end;
      end;
      if result then Break;
    end;
  finally
    FreeAndNil(EventList);
    FreeAndNil(ObjectID);
    FreeAndNil(RegionList);
  end;
end;

function TBoldPessimisticLockHandler.IsElementInAnyRequiredRegion(
  Element: TBoldMember): Boolean;
var
  RegionList: TBoldRegionList;
  i: integer;
begin
  Result := false;
  RegionList := TBoldRegionList.Create;
  try
    Factory.GetRegionsForElement(Element, RegionList);
    for i:= 0 to RegionList.Count - 1 do
    begin
      result := (fRequiredExclusive.IndexOf(RegionList[i]) <> -1) or (fRequiredShared.IndexOf(RegionList[i]) <> -1);
      if result then Break;
    end;
  finally
    FreeAndNil(RegionList);
  end;
end;

procedure TBoldPessimisticLockHandler.ReleaseUnNeededRegions;
var
  CurrentRegion: TBoldRegion;
  Elements: TList;
  aTraverser: TBoldIndexTraverser;
  UnRequiredLocks, RequiredExclusiveLocks, RequiredSharedLocks: TBoldLockList;
  LockName: string;
  TrueLockHolder: TBoldAbstractLockHolder;
begin
  Elements := TList.Create;
  UnRequiredLocks := TBoldLockList.Create;
  RequiredSharedLocks := TBoldLockList.Create;
  RequiredExclusiveLocks := TBoldLockList.Create;
  TrueLockHolder := fLockHolder;
  try
    aTraverser := fLockHolder.HeldExclusive.CreateTraverser;
    try
      while not aTraverser.EndOfList do
      begin
        LockName := (aTraverser.Item as TBoldLock).Name;
        if LockName <> BOLD_DBLOCK_NAME then
        begin
          CurrentRegion := Factory.GetRegionByName(LockName);
          Elements.Clear;
          CurrentRegion.GetElements(Elements);
          if ElementListContainsDirtyElements(Elements) then
            RequireRegionExplicit(CurrentRegion);
        end;
        aTraverser.Next;
      end;
    finally
      aTraverser.Free;
    end;

    // since the regionExpander will cut the expansion tree with the held locks, we need to fake
    // an empty lockholder for this excersise. The expansion will occur in the getmethod of RequiredExclusive and RequiredShared...
    fLockHolder := TBoldEmptyLockHolder.Create;

    RegionListToLockList(RequiredExclusive, RequiredExclusiveLocks);
    RegionListToLockList(RequiredShared, RequiredSharedLocks);

    FreeAndNil(fLockHolder);
    fLockHolder := TrueLockHolder;

    UnRequiredLocks.AddList(fLockHolder.HeldExclusive);
    UnRequiredLocks.AddList(fLockHolder.HeldShared);
    UnRequiredLocks.RemoveList(RequiredExclusiveLocks);
    UnRequiredLocks.RemoveList(RequiredSharedLocks);

    fLockHolder.Release(UnRequiredLocks);
  finally
    FreeAndNil(Elements);
    FreeAndNil(UnRequiredLocks);
    FreeAndNil(RequiredSharedLocks);
    FreeAndNil(RequiredExclusiveLocks);
    fLockHolder := TrueLockHolder; // Just in case
  end;
end;

function TBoldPessimisticLockHandler.ElementListContainsDirtyElements(
  Elements: TList): Boolean;
var
  i: integer;
  aBoldElement: TBoldDomainElement;
begin
  Result := false;
  i:= 0;
  while (i < Elements.Count) and not Result do
  begin
    aBoldElement := TBoldDomainElement(Elements[i]);
    if (aBoldElement is TBoldMember) then
      Result := (aBoldElement as TBoldMember).BoldDirty
    else if (aBoldElement is TBoldObject) then
      Result := (aBoldElement as TBoldObject).BoldPersistenceState = bvpsModified;
    inc(i);
  end;
end;

procedure TBoldPessimisticLockHandler.RequireRegionExplicit(Region: TBoldRegion);
begin
  fRequiredExplicit.Add(Region);
  fParentsChangedRegions.Add(Region);
  fSubregionsChangedRegions.Add(Region);
end;

function TBoldPessimisticLockHandler.IsHeldAsExplicitOrParent(Region: TBoldRegion): Boolean;

  function IsParent: Boolean;
  var
    i: Integer;
    RegionList: TBoldRegionList;
  begin
    result := false;
    RegionList := TBoldRegionList.Create;
    try
      Region.FillDependentSubregions(RegionList);
      for i := 0 to RegionList.Count - 1 do
      begin
        result := IsHeldAsExplicitOrParent(RegionList[i]);
        if result then
          exit;
      end;
      Region.FillInDependentSubregions(RegionList);
      for i := 0 to RegionList.Count - 1 do
      begin
        result := IsHeldAsExplicitOrParent(RegionList[i]);
        if result then
          exit;
      end;
    finally
      RegionList.Free;
    end;
  end;

begin
  result := fLockHolder.HeldExclusive.Includes(Region.AsString) or
            (fLockHolder.HeldShared.Includes(Region.AsString) and IsParent);
end;

function TBoldPessimisticLockHandler.GetFactory: TBoldRegionFactory;
begin
  if assigned(System) and assigned(System.RegionFactory) and (System.RegionFactory is TBoldRegionFactory) then
    result := System.RegionFactory as TBoldRegionFactory
  else
    result := nil;
end;

procedure TBoldPessimisticLockHandler._ReceiveRolledBack(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  if (Originator = System) and (OriginalEvent = beRolledBack) then
  begin
    fRequiredExplicit.Clear;
    fKnownRequiredParents.Clear;
    fKnownRequiredSubregions.Clear;
    fRequiredExclusive.Clear;
    fRequiredShared.Clear;
  end;
end;

function TBoldPessimisticLockHandler.EnsureLocks: Boolean;
begin
  result := fLockHolder.EnsureLocks;
end;

procedure TBoldPessimisticLockHandler.SignalActivityStart;
begin
  if assigned(OnActivityStart) then
    OnActivityStart(self);
end;

procedure TBoldPessimisticLockHandler.SignalActivityEnd;
begin
  if assigned(OnActivityEnd) then
    OnActivityEnd(self);
end;

procedure TBoldPessimisticLockHandler.AddHeldLocksToRegionLookup(RegionLookup: TBoldRegionLookup; AddSharedRegions: Boolean);
  procedure Add(List: TBoldLockList);
  var
    Guard: IBoldGuard;
    Traverser: TBoldIndexTraverser;
    RegionId: string;
  begin
    Guard := TBoldGuard.CReate(Traverser);
    Traverser := List.CreateTraverser;
    while not Traverser.EndOfList do
    begin
      RegionId := (Traverser.Item as TBoldLock).Name;
      // Only check locks that are region-locks
      if (pos('.', RegionId) <> 0) then
        if not assigned(RegionLookup.FindByID(RegionId)) then
          RegionLookup.Add(Factory.GetRegionByName(RegionId));
      Traverser.Next;
    end;
  end;

begin
  Add(fLockHolder.HeldExclusive);
  if AddSharedRegions then
    Add(fLockHolder.HeldShared);
end;


{ TBoldFailureGetLocksFailed }

constructor TBoldFailureGetLocksFailed.Create(reason: String;
  Originator: TBoldDomainElement; const ConflictingRegions: TBoldRegionList; const ClientIds: TStringList);
begin
  inherited Create(reason, Originator);
  fClientIds := TStringList.Create;
  if Assigned(ClientIds) then
    fClientIds.Assign(ClientIds);
  fConflictingRegions := TBoldRegionList.Create;
  if Assigned(ConflictingRegions) then
    fConflictingRegions.Assign(ConflictingRegions);
end;

destructor TBoldFailureGetLocksFailed.Destroy;
begin
  FreeAndNil(fClientIds);
  FreeAndNil(fConflictingRegions);
  inherited;
end;

function TBoldFailureGetLocksFailed.GetException(
  const Msg: String): EBoldFailure;
begin
  Result := EBoldGetLocksFailed.Create(Msg, fClientIds, fConflictingRegions);
end;

{ EBoldGetLocksFailed }

constructor EBoldGetLocksFailed.Create(msg: string;
  const ClientIds: TStrings; const ConflictingRegions: TBoldRegionList);
begin
  inherited Create(Msg);
  fClientIds := TStringList.Create;
  if assigned(ClientIds) then
    fClientIds.Assign(ClientIds);
  fConflictingRegions := TBoldRegionList.Create;
  fConflictingRegions.Assign(ConflictingRegions);
end;

destructor EBoldGetLocksFailed.Destroy;
begin
  FreeAndNil(fClientIds);
  FreeAndNil(fConflictingRegions);
  inherited;
end;


{ TBoldEmptyLockHolder }

constructor TBoldEmptyLockHolder.Create;
begin
  fLockList := TBoldLockList.Create;
end;

destructor TBoldEmptyLockHolder.Destroy;
begin
  FreeAndNil(fLockList);
  inherited;
end;

function TBoldEmptyLockHolder.EnsureLocks: Boolean;
begin
  result := true;
end;

function TBoldEmptyLockHolder.GetHeldExclusive: TBoldLockList;
begin
  result := fLockList;
end;

function TBoldEmptyLockHolder.GetHeldShared: TBoldLockList;
begin
  result := fLockList;
end;

procedure TBoldEmptyLockHolder.GetPropagationEvents(
  EventList: TStringList);
begin
  inherited;

end;

function TBoldEmptyLockHolder.Lock(Shared, Exclusive: TBoldLockList;
  HeldLocks, ClientsHoldingRequestedLocks: TStringList): Boolean;
begin
  result := true;
end;

procedure TBoldEmptyLockHolder.Release(Locks: TBoldLockList);
begin
end;

end.
