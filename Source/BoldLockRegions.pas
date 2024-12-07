{ Global compiler directives }
{$include bold.inc}
unit BoldLockRegions;

interface

uses
  classes,
  BoldSubscription,
  BoldSystem,
  BoldSystemRT,
  BoldDefs,
  BoldBase,
  BoldDomainElement,
  BoldRegionDefinitions,
  BoldLogHandler,
  BoldIndexableList;

const
  breNone = 0;
  breParentsChanged = 1;
  breSubregionsChanged = 2;

type
  TBoldRegion = class;
  TBoldRegionFactory = class;
  TBoldRegionLookup = class;
  TBoldRegionList = class;

  TBoldRegionEvent = procedure (Sender: TBoldRegion; Event: Integer) of object;

  TBoldRegion = class(TBoldSubscriber)
  private
    fDefinition: TBoldConcreteRegionDefinition;
    fRootLocator: TBoldObjectLocator;
    fFactory: TBoldRegionFactory;
    procedure FillSubregions(RegionList: TBoldRegionList; Dependent: Boolean);
    function GetRoot: TBoldObject;
    procedure PlaceRootSubscriptions;
  protected
    function GetAsString: string;
    procedure Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent); override;
  public
    constructor Create(Definition: TBoldConcreteRegionDefinition; RootLocator: TBoldObjectLocator; Factory: TBoldRegionFactory);
    destructor Destroy; override;
    procedure GetElements(ResultList: TList);
    procedure FillDependentSubregions(RegionList: TBoldRegionList);
    procedure FillIndependentSubregions(RegionList: TBoldRegionList);
    property Definition: TBoldConcreteRegionDefinition read fDefinition;
    property Root: TBoldObject read GetRoot;
    property RootLocator: TBoldObjectLocator read fRootLocator;
    property AsString: string read GetAsString;
    property Factory: TBoldRegionFactory read fFactory;
  end;

  TBoldRegionFactory = class(TBoldAbstractRegionFactory)
  private
    fDefinitions: TBoldRegionDefinitions;
    fLookup: TBoldRegionLookup;
    fOnRegionChanged: TBoldRegionEvent;
    function RegionId(Definition: TBoldRegionCoreDefinition; RootLocator: TBoldObjectLocator): string;
    procedure NotifyRegionChanged(Sender: TBoldRegion; Event: Integer);
    function RootObjectLocatorFromRegionId(RegionId: string): TBoldObjectLocator;
    function CoreDefintionFromRegionId(RegionId: string): TBoldRegionCoreDefinition;
  public
    constructor Create(Definitions: TBoldRegionDefinitions);
    destructor Destroy; override;
    function GetRegion(Definition: TBoldRegionCoreDefinition; RootLocator: TBoldObjectLocator): TBoldRegion;
    procedure GetRegionsForElement(Element: TBoldDomainElement; ResultList: TList); override;
    function GetRegionByName(const RegionName: string): TBoldRegion;
    property OnRegionChanged: TBoldRegionEvent read fOnRegionChanged write fOnRegionChanged;
  end;

  TBoldRegionLookupTraverser = class(TBoldIndexableListTraverser)
  private
    function GetRegion: TBoldRegion;
  public
    property Region: TBoldRegion read GetRegion;
    property Current: TBoldRegion read GetRegion;
  end;

  TBoldRegionLookup = class(TBoldUnOrderedIndexableList)
  private
    class var IX_RegionId: integer;
    procedure ExpandOneLevelRegionsForNavigation(Regions: TBoldRegionLookup; Navigation: TBoldRoleRTInfo; CoreDef: TBoldRegionCoreDefinition; AlreadyExpandedRegions, AlreadyKnownRegions: TBoldregionLookup);
    procedure AddIfNotInLookup(Region: TBoldRegion);
  protected
    function TraverserClass: TBoldIndexableListTraverserClass; override;
  public
    constructor Create;
    function CreateTraverser: TBoldRegionLookupTraverser;
    function GetEnumerator: TBoldRegionLookupTraverser;
    function FindByID(RegionId: string): TBoldRegion;
    procedure AddRegionLookup(Regions: TBoldRegionLookup);
    procedure AddRegionLookupWithFilter(Regions: TBoldRegionLookup; Filter: TBoldRegionLookup);
    procedure AddRegionList(Regions: TBoldRegionList);
    procedure FetchAndExpandOneLevelSubRegions(SubRegions: TBoldRegionLookup; AdditionalRegions: TBoldRegionLookup; AlreadyExpandedRegions, AlreadyKnownRegions: TBoldregionLookup);
    procedure FetchAndExpandOneLevelParentRegions(ParentRegions: TBoldRegionLookup; AlreadyExpandedRegions, AlreadyKnownRegions: TBoldRegionLookup);
    procedure FillObjectList(ObjectList: TBoldObjectList);
  end;

  TBoldOrderedRegionLookup = class(TBoldRegionLookup)
  private
    class var IX_RegionOrder: integer;
    function GetFirstRegion: TBoldRegion;
  public
    constructor Create;
    property FirstRegion: TBoldRegion read GetFirstRegion;
  end;


  TBoldRegionList = class(TList)
  private
    function GetItems(i: Integer): TBoldRegion;
    function GetAsString: string;
  public
    procedure AddList(List: TBoldRegionList);
    procedure AddRegionLookup(Regions: TBoldRegionLookup);
    procedure Assign(List: TBoldRegionList);
    property Items[i: Integer]: TBoldRegion read GetItems; default;
    property AsString: string read GetAsString;
  end;

{  TBoldRegionState =
    (rrSource, rrDepParent, rrIndepParent, rrDepSub, rrIndepSub);

  TBoldRegionExpanderRegionLists = array[TBoldRegionState] of TBoldregionLookup;}

  TBoldRegionExpander = class(TBoldMemoryManagedObject)
  private
    fNewParentRegions: TBoldRegionLookup;
    fNewSubregions: TBoldRegionLookup;
    fToBeSubExpanded: TBoldOrderedRegionLookup;
    fToBeParentExpanded: TBoldOrderedRegionLookup;
    fOnActivityProgress: TBoldLockManagerProgressEvent;
    procedure SignalProgress;
  protected
    procedure Clear;
    procedure ExtractSimilarRegions(Regions: TBoldRegionLookup; Region: TBoldRegion; Result: TBoldRegionLookup);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ExpandParentRegions(RegionsToExpand: TBoldRegionLookup; KnownParentRegions, KnownSubregions: TBoldRegionLookup);
    procedure ExpandSubregions(RegionsToExpand: TBoldRegionLookup; KnownSubregions: TBoldregionLookup);

    procedure ExpandRegionEnclosure(Regions: TBoldRegionLookup);

    property NewParentRegions: TBoldRegionLookup read fNewParentRegions;
    property NewSubRegions: TBoldRegionLookup read fNewSubRegions;
    property OnProgress: TBoldLockManagerProgressEvent read fOnActivityProgress write fOnActivityProgress;

  end;

var
  BoldRegionExpansionDebugLogHandler: TBoldLogHandler = nil;

implementation

uses
  SysUtils,

  BoldCoreConsts,
  BoldDefaultId,
  BoldElements,
  BoldGuard,
  BoldId,
  BoldHashIndexes,
  BoldIndex;

type
  TBoldRegionIndex = class(TBoldStringHashIndex)
  protected
    function ItemAsKeyString(Item: TObject): string; override;
  end;

procedure NavigateAndSubscribe(Obj: TBoldObject; RoleRT: TBoldRoleRtInfo; ResultElement: TBoldIndirectElement; Subscriber: TBoldSubscriber; RequestedEvent: TBoldRequestedEvent);
var
  aMember: TBoldMember;
begin
  if Obj.BoldClassTypeInfo.BoldIsA(RoleRT.ClassTypeInfo) then
  begin
    aMember := Obj.BoldMembers[RoleRT.Index];

    if (aMember is TBoldObjectList) or (aMember is TBoldObjectReference) then
      AMember.GetAsList(ResultElement)
    else
      raise EBoldInternal.CreateFmt(sTriedToNavigateNonAssociation, [Obj.BoldClassTypeInfo.ExpressionName, aMember.BoldMemberRTInfo.ExpressionName]);

    if assigned(Subscriber) then
      aMember.DefaultSubscribe(Subscriber, RequestedEvent);
  end
  else
    resultElement.SetOwnedValue(nil);
end;


procedure DoReverseNavigationAndSubscribe(Obj: TBoldObject; Navigation: TBoldRoleRTInfo; ResultElement: TBoldIndirectElement; Subscriber: TBoldSubscriber; RequestedEvent: TBoldRequestedEvent);
var
  ReverseRole: TBoldRoleRtInfo;
begin
  ReverseRole := Navigation.ClassTypeInfoOfOtherEnd.AllMembers[Navigation.IndexOfOtherEnd] as TBoldRoleRtInfo;
  NavigateAndSubscribe(Obj, ReverseRole, resultElement, Subscriber, RequestedEvent);
end;

procedure DoNavigationAndSubscribe(Obj: TBoldObject; Navigation: TBoldRoleRTInfo; ResultElement: TBoldIndirectElement; Subscriber: TBoldSubscriber; RequestedEvent: TBoldRequestedEvent);
begin
  NavigateAndSubscribe(Obj, Navigation, ResultElement, Subscriber, RequestedEvent);
end;

{ TBoldOrderedRegionLookup }

constructor TBoldOrderedRegionLookup.Create;
begin
  inherited;
  SetIndexVariable(IX_RegionOrder, AddIndex(TBoldIntegerIndex.Create));
end;

function TBoldOrderedRegionLookup.GetFirstRegion: TBoldRegion;
begin
  Result := (Indexes[IX_RegionOrder] as TBoldIntegerIndex).items[0] as TBoldRegion;
end;

{ TBoldRegion }

constructor TBoldRegion.Create(Definition: TBoldConcreteRegionDefinition;
  RootLocator: TBoldObjectLocator; Factory: TBoldRegionFactory);
begin
  inherited Create;
  fRootLocator := RootLocator;
  fFactory := Factory;
  fDefinition := Definition;
  PlaceRootSubscriptions;
end;

destructor TBoldRegion.Destroy;
begin
  fRootLocator := nil;
  inherited;
end;


procedure TBoldRegion.FillDependentSubregions(RegionList: TBoldRegionList);
begin
  FillSubregions(RegionList, true);
end;


procedure TBoldRegion.FillIndependentSubregions(RegionList: TBoldRegionList);
begin
  FillSubregions(RegionList, false);
end;


procedure TBoldRegion.FillSubregions(RegionList: TBoldRegionList; Dependent: Boolean);
var
  i, j: integer;
  Subregion: TBoldSubregionReference;
  Objects: TBoldObjectList;
  ie: TBoldIndirectElement;
begin
  ie := TBoldIndirectElement.Create;
  try
    for i := 0 to Definition.Subregions.Count-1 do
    begin
      Subregion := Definition.Subregions[i];
      if Subregion.IsDependent = Dependent then
      begin
        if assigned(Subregion.SubregionRootNavigation) then
        begin
          DoNavigationAndSubscribe(Root, Subregion.SubregionRootNavigation, ie, self, breSubregionsChanged);
          Objects := ie.value as TBoldObjectList;
          if assigned(Objects) then
          begin
            Objects.EnsureObjects;
            for j := 0 to Objects.Count-1 do
            begin
              RegionList.Add(fFactory.GetRegion(Subregion.SubregionCoreDefinition, Objects.Locators[j]));
            end;
          end;
        end;
      end;
    end;
  finally
    ie.free;
  end;
end;


function TBoldRegion.GetAsString: string;
begin
  if assigned(RootLocator) and assigned(RootLocator.BoldObjectID) then
    result := fFactory.RegionId(Definition.CoreDefinition, RootLocator)
  else
    raise EBoldInternal.CreateFmt(sRegionMissingIDOrLocator, [ClassName]);
end;

procedure TBoldRegion.GetElements(ResultList: TList);
var
  i: Integer;
begin
  ResultList.Add(Root);
  for i := 0 to fDefinition.Elements.Count - 1 do
    ResultList.Add(Root.BoldMembers[fDefinition.Elements[i].Member.index]);
end;

function TBoldRegion.GetRoot: TBoldObject;
begin
  result := fRootLocator.EnsuredBoldObject;
end;

procedure TBoldRegion.PlaceRootSubscriptions;
begin
  Root.AddSmallSubscription(Self, [beDestroying, bePreUpdateId, bePostUpdateId], -1);
end;

procedure TBoldRegion.Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  if (Originator is TBoldObject) and not assigned(TBoldObject(Originator).BoldObjectLocator) and
    not assigned(RootLocator.BoldObject) then
  begin

    fFactory.fLookup.ItemChanged(self);
    fFactory.fLookup.Remove(self);
  end
  else if (Originator = Root) then
  begin
    case OriginalEvent of
      beDestroying:
        fFactory.fLookup.Remove(self);

      bePreUpdateId: begin
        fFactory.fLookup.OwnsEntries := false;
        fFactory.fLookup.Remove(self);
        fFactory.fLookup.OwnsEntries := true;
      end;
      bePostUpdateId: fFactory.fLookup.Add(self);
    end;
  end else
  begin
    CancelAllSubscriptions;
    PlaceRootSubscriptions;
    fFactory.NotifyRegionChanged(self, RequestedEvent);
  end;
end;

function TBoldRegionLookup.FindByID(RegionId: string): TBoldRegion;
begin
  result := (Indexes[IX_RegionId] as TBoldStringHashIndex).FindByString(RegionId) as TBoldRegion;
end;

function TBoldRegionLookup.GetEnumerator: TBoldRegionLookupTraverser;
begin
  result := CreateTraverser as TBoldRegionLookupTraverser;
end;

function TBoldRegionLookup.TraverserClass: TBoldIndexableListTraverserClass;
begin
  result := TBoldRegionLookupTraverser;
end;

{ TBoldRegionFactory }

function TBoldRegionFactory.CoreDefintionFromRegionId(
  RegionId: string): TBoldRegionCoreDefinition;
begin
  result := fDefinitions.CoreDefinition[Copy(RegionId, Pos('.', RegionId)+1, MaxInt)];
  if not assigned(Result) then
    raise EBoldInternal.CreateFmt(sBadRegionID, [classname, 'CoreDefintionFromRegionId', RegionId]); // do not localize
end;

constructor TBoldRegionFactory.Create(Definitions: TBoldRegionDefinitions);
begin
  inherited Create;
  fLookup := TBoldRegionLookup.Create;
  fLookup.OwnsEntries := true;
  fDefinitions := Definitions;
end;

destructor TBoldRegionFactory.Destroy;
begin
  FreeAndNil(fLookup);
  inherited;
end;

function TBoldRegionFactory.GetRegion(
  Definition: TBoldRegionCoreDefinition; RootLocator: TBoldObjectLocator): TBoldRegion;
var
  ConcreteDef: TBoldConcreteRegionDefinition;
begin
  result := fLookup.FindByID(RegionId(Definition, RootLocator));
  if not assigned(result) then
  begin
    ConcreteDef := Definition.ConcreteDefinitions.FindByRootClass(RootLocator.EnsuredBoldObject.BoldClassTypeInfo);
    if not assigned(ConcreteDef) then
      raise EBold.CreateFmt(sBadRegionDefinition, [classname, Definition.Name, RootLocator.EnsuredBoldObject.BoldClassTypeInfo.ExpressionName]);
    result := TBoldRegion.Create(ConcreteDef, RootLocator, Self);
    fLookup.Add(result);
  end;
end;

function TBoldRegionFactory.GetRegionByName(
  const RegionName: string): TBoldRegion;
begin
  result := fLookup.FindByID(RegionName);
  if not assigned(result) then
    result := GetRegion(CoreDefintionFromRegionId(RegionName),
                        RootObjectLocatorFromRegionId(RegionName));
end;

procedure TBoldRegionFactory.GetRegionsForElement(Element: TBoldDomainElement; ResultList: TList);
var
  RegionInclusions: TBoldRegionElementInclusionList;
  i: Integer;
  aMember: TBoldMember;
  anObject: TBoldObject;
  ConcreteDefs: TBoldConcreteRegionDefinitionList;
begin
  if Element is TBoldMember then
  begin
    aMember := Element as TBoldMember;
    RegionInclusions := fDefinitions.RegionInclusionsByMember[aMember.BoldMemberRTInfo];
    for i := 0 to RegionInclusions.Count-1 do
      resultList.Add(GetRegion(RegionInclusions[i].Region.CoreDefinition, aMember.OwningObject.BoldObjectLocator));
  end
  else if Element is TBoldObject then
  begin
    anObject := Element as TBoldObject;
    ConcreteDefs := fDefinitions.ConcreteRegionDefinitionsByRootClass[anObject.BoldClassTypeInfo];
    for i := 0 to ConcreteDefs.Count-1 do
      ResultList.Add(GetRegion(ConcreteDefs[i].CoreDefinition, anObject.BoldObjectLocator));
  end;
end;

procedure TBoldRegionFactory.NotifyRegionChanged(Sender: TBoldRegion; Event: Integer);
begin
  if Assigned(fOnRegionChanged) then
    OnRegionChanged(Sender, Event);
end;

function TBoldRegionFactory.RegionId(Definition: TBoldRegionCoreDefinition;
  RootLocator: TBoldObjectLocator): string;
begin
  result := RootLocator.BoldObjectID.AsString + '.' + Definition.Name;
  if RootLocator.BoldObjectID is TBoldInternalObjectId then
    result := 'i' + Result; 
end;

function TBoldRegionFactory.RootObjectLocatorFromRegionId(RegionId: string): TBoldObjectLocator;
var
  ObjId: TBoldObjectId;
  AsInt: Integer;
begin
  ObjId := nil;
  try
    if RegionId[1] = 'i' then
    begin
      AsInt := StrToIntDef(Copy(RegionId, 2, Pos('.', RegionId)-1), -1);
      if AsInt = -1 then
        raise EBoldInternal.CreateFmt(sBadRegionID, [classname, 'RootObjectFromRegionId', RegionId]); // do not localize
      ObjId := TBoldInternalObjectId.CreateWithClassIDandInternalId(AsInt, 0, false);
    end else
    begin
      AsInt := StrToIntDef(Copy(RegionId, 1, Pos('.', RegionId)-1), -1);
      if AsInt = -1 then
        raise EBoldInternal.CreateFmt(sBadRegionID, [classname, 'RootObjectFromRegionId', RegionId]); // do not localize
      ObjId := TBoldDefaultID.CreateWithClassID(0, false);
      (ObjId as TBoldDefaultId).AsInteger := AsInt;
    end;
    result := System.EnsuredLocatorByID[ObjId];
  finally
    ObjId.Free;
  end;
end;

{ TBoldRegionIndex }

function TBoldRegionIndex.ItemAsKeyString(Item: TObject): string;
begin
  result := (Item as TBoldRegion).AsString;
end;

{ TBoldRegionLookup }

procedure TBoldRegionLookup.AddRegionLookup(Regions: TBoldRegionLookup);
var
  Region: TBoldRegion;
begin
  if not Regions.IsEmpty then
    for Region in Regions do
      AddIfNotInLookup(Region);
end;

constructor TBoldRegionLookup.Create;
begin
  SetIndexVariable(IX_RegionId, AddIndex(TBoldRegionIndex.Create));
end;

function TBoldRegionLookup.CreateTraverser: TBoldRegionLookupTraverser;
begin
  result := TBoldRegionLookupTraverser(inherited CreateTraverser);
  Assert(Result is TBoldRegionLookupTraverser);
end;

procedure TBoldRegionLookup.FillObjectList(ObjectList: TBoldObjectList);
var
  Region: TBoldRegion;
begin
  for Region in self do
    ObjectList.AddLocator(Region.RootLocator);
end;

procedure TBoldRegionLookup.FetchAndExpandOneLevelParentRegions(ParentRegions: TBoldRegionLookup; AlreadyExpandedRegions, AlreadyKnownRegions: TBoldRegionLookup);
var
  ObjectList: TBoldObjectLIst;
  ConcreteDef: TBoldConcreteRegionDefinition;
  i: integer;
  SubRegionRef: TBoldSubRegionReference;
  Navigation: TBoldRoleRTInfo;
  ReverseRole: TBoldRoleRtInfo;
  TempRole: TBoldRoleRTInfo;
  Guard: IBoldGuard;

begin
  Guard := TBoldGuard.Create(Objectlist);
  ObjectList := TBoldObjectList.Create;
  FillObjectList(ObjectList);
  ObjectList.EnsureObjects;
  ConcreteDef := (Any as TBoldRegion).Definition;
  for i := 0 to ConcreteDef.ParentRegions.Count-1 do
  begin
    SubregionRef := ConcreteDef.ParentRegions[i];
    if assigned(SubregionRef.SubregionRootNavigation) then
    begin
      Navigation := SubRegionRef.SubRegionRootNavigation;
      case Navigation.RoleType of
        rtRole: begin
          ReverseRole := Navigation.ClassTypeInfoOfOtherEnd.AllMembers[Navigation.IndexOfOtherEnd] as TBoldRoleRtInfo;
          assert(ReverseRole.RoleType = rtRole);
        end;
        rtLinkRole: begin
          ReverseRole := Navigation.RoleRTInfoOfOtherEnd;
          assert(ReverseRole.RoleType = rtInnerLinkRole);
        end;
        rtInnerLinkRole: begin
          TempRole := Navigation.ClassTypeInfoOfOtherEnd.AllMembers[Navigation.IndexOfOtherEnd] as TBoldRoleRtInfo;
          ReverseRole := Navigation.ClassTypeInfoOfOtherEnd.AllMembers[TempRole.IndexOfLinkObjectRole] as TBoldRoleRTInfo;
          assert(ReverseRole.RoleType = rtLinkRole);
        end;
        else
          raise EBoldInternal.CreateFmt(sUnknownRoleType, [ClassName, Navigation.AsString]);
      end;
      assert(ConcreteDef.RootClass.Conformsto(ReverseRole.ClassTypeInfo));
      assert(ObjectList[0].BoldClassTypeInfo.ConformsTo(ReverseRole.ClassTypeInfo));

      ObjectList[0].BoldSystem.FetchLinksWithObjects(ObjectList, ReverseRole.ExpressionName);
      ExpandOneLevelRegionsForNavigation(ParentRegions, ReverseRole, SubRegionRef.ParentRegion.CoreDefinition, AlreadyExpandedRegions, AlreadyKnownRegions);
    end;
  end;
end;


procedure TBoldRegionLookup.FetchAndExpandOneLevelSubRegions(SubRegions: TBoldRegionLookup; AdditionalRegions: TBoldRegionLookup; AlreadyExpandedRegions, AlreadyKnownRegions: TBoldregionLookup);
  procedure MassiveFetch(System: TBoldSystem; ObjectList: TBoldObjectList; RoleRTInfo: TBoldRoleRTInfo );
  var
    TempObjectList: TBoldObjectLIst;
    DefiningClass: TBoldClassTypeInfo;
    Traverser: TBoldIndexableListTraverser;
    aRegion: TBoldRegion;
    FetchNeeded: Boolean;
    i: integer;
  begin
    TempObjectLIst := ObjectLIst.Clone as TBoldObjectList;
    try
      FetchNeeded := false;
      for i := 0 to ObjectList.Count-1 do
        fetchNeeded := fetchNeeded or
          (not assigned(ObjectList.Locators[i].BOldObject)) or
          (not ObjectLIst[i].BoldMemberAssigned[RoleRTInfo.Index]) or
          (ObjectLIst[i].BoldMembers[RoleRTInfo.Index].BoldPersistenceState = bvpsInvalid);

      if FetchNeeded then
      begin

        DefiningClass := RoleRTINfo.ClassTypeInfo;
        while DefiningClass.FirstOwnMemberIndex > RoleRTInfo.index do
          DefiningClass := DefiningClass.SuperClassTypeInfo;

        Traverser := AdditionalRegions.CreateTraverser;
        try
          while Traverser.MoveNext do
          begin
            aRegion := Traverser.Item as TBoldRegion;
            if aRegion.fDefinition.RootClass.ConformsTo(DefiningClass) then
              TempObjectList.AddLocator(aRegion.RootLocator);
          end;
        finally
          Traverser.Free;
        end;
      end;
      System.FetchLinksWithObjects(TempObjectList, RoleRTInfo.ExpressionName);
    finally
      TempObjectList.Free;
    end;
  end;
var
  ObjectList: TBoldObjectLIst;
  ConcreteDef: TBoldConcreteRegionDefinition;
  i: integer;
  SubRegionRef: TBoldSubregionReference;
begin
  ObjectList := TBoldObjectList.Create;
  try
    FillObjectList(ObjectList);
    ObjectList.EnsureObjects;
    ConcreteDef := (Any as TBoldRegion).Definition;
    for i := 0 to ConcreteDef.Subregions.Count-1 do
    begin
      SubRegionRef := ConcreteDef.Subregions[i];
      if assigned(SubRegionRef.SubRegionRootNavigation) then
      begin
        MassiveFetch(ObjectList[0].BoldSystem, ObjectList, SubRegionRef.SubregionRootNavigation);
        ExpandOneLevelRegionsForNavigation(SubRegions, SubRegionRef.SubRegionRootNavigation, SubRegionRef.SubregionCoreDefinition, AlreadyExpandedRegions, AlreadyKnownRegions);
      end;
    end;
  finally
    ObjectList.Free;
  end;
end;

procedure TBoldRegionLookup.ExpandOneLevelRegionsForNavigation(Regions: TBoldRegionLookup; Navigation: TBoldRoleRTInfo; CoreDef: TBoldRegionCoreDefinition; AlreadyExpandedRegions, AlreadyKnownRegions: TBoldRegionLookup);

  procedure ExpandRegion(Region: TBoldRegion);
  var
    ie: TBoldIndirectElement;
    i, OldRegionsCount: integer;
    Objects: TBoldObjectList;
    NewRegion: TBoldRegion;
  begin
    ie := TBoldIndirectElement.Create;
    try
      DoNavigationAndSubscribe(Region.Root, Navigation, ie, Region, breSubregionsChanged);
      Objects := ie.value as TBoldObjectList;
      if assigned(Objects) then
      begin
        OldRegionsCount := Regions.Count;
        for i := 0 to Objects.Count-1 do
        begin
          NewRegion := Region.Factory.GetRegion(CoreDef, Objects.Locators[i]);
          if not assigned(AlreadyExpandedRegions.FindByID(NewRegion.AsString)) and
             not assigned(AlreadyKnownRegions.FindByID(NewRegion.AsString)) then
            Regions.AddIfNotInLookup(NewRegion);
        end;
        if (Regions.Count > OldRegionsCount) and assigned(BoldRegionExpansionDebugLogHandler) then
          BoldRegionExpansionDebugLogHandler.LogFmt(sLogAddedRegions, [
            Region.Root.BoldClassTypeInfo.ExpressionName,
            Navigation.ExpressionName,
            Regions.Count - OldRegionsCount]);
      end;
    finally
      ie.free;
    end;
  end;

var
  Region: TBoldRegion;
begin
  for Region in self do
    ExpandRegion(Region);
end;

procedure TBoldRegionLookup.AddRegionList(Regions: TBoldRegionList);
var
  i: integer;
begin
  for i := 0 to Regions.Count-1 do
    AddIfNotInLookup(Regions[i]);
end;

procedure TBoldRegionLookup.AddIfNotInLookup(Region: TBoldRegion);
begin
  if not assigned(FindById(Region.AsString)) then
    Add(Region);
end;

procedure TBoldRegionLookup.AddRegionLookupWithFilter(Regions,
  Filter: TBoldRegionLookup);
var
  Region: TBoldRegion;
begin
  for Region in Regions do
    if not assigned(Filter.FindById(Region.AsString)) then
      AddIfNotInLookup(Region);
end;

{ TBoldRegionList }

procedure TBoldRegionList.AddList(List: TBoldRegionList);
var
  i: Integer;
begin
  if assigned(List) then
    for i := 0 to List.Count-1 do
      Add(List[i]);
end;

procedure TBoldRegionList.AddRegionLookup(Regions: TBoldRegionLookup);
var
  Region: TBoldRegion;
begin
  for Region in Regions do
    Add(Region);
end;

procedure TBoldRegionList.Assign(List: TBoldRegionList);
begin
  Clear;
  AddList(List);
end;

{procedure TBoldRegionList.EnsureSubRegionObjects;
var
  ObjectList: TBoldObjectList;
  i: integer;
  Region: TBoldRegion;
  SubRegionReferences: TBoldSubregionReferenceList;
  TempRegionList: TBoldRegionList;
begin
  TempRegionList := TBoldRegionList.Create;
  ObjectList := TBoldObjectList.Create;
  try
    TempRegionList.AddList(self);
    while TempRegionList.Count > 0 do
    begin
      Region := TempRegionList[0];
      for i := TempRegionList.Count-1 downto 0 do
      begin
        if TempRegionList[i].Definition = Region.Definition then
        begin
          ObjectList.Add(TempRegionList[i].Root);
          TempRegionList.Delete(i);
        end;
      end;
      if ObjectList.Count > 1 then
        for i := 0 to Region.Definition.Subregions.Count-1 do
          ObjectList[0].BoldSystem.FetchLinksWithObjects(ObjectList, Region.Definition.Subregions[i].SubregionRootNavigation.ExpressionName);
      ObjectList.Clear;
    end;
    for i := 0 to Count-1 do
    begin
      Items[i].FillDependentSubregions(TempRegionList);
      Items[i].FillInDependentSubregions(TempRegionList);
    end;
    if TempRegionList.Count > 0 then
      TempRegionList.EnsureSubRegionObjects;
  finally
    ObjectList.Free;
    TempRegionList.Free;
  end;
end;
}

function TBoldRegionList.GetAsString: string;
var
  sl: TStringList;
  i: integer;
begin
  result := '';
  sl := TStringList.Create;
  try
    for I := 0 to Count - 1 do
      sl.Add(self[i].Root.DebugInfo);
  finally
    result := sl.CommaText;
    sl.free;
  end;
end;

function TBoldRegionList.GetItems(i: Integer): TBoldRegion;
begin
  result := TObject(inherited Items[i]) as TBoldRegion;
end;

{ TBoldRegionExpander }

procedure TBoldRegionExpander.Clear;
begin
  fNewParentRegions.Clear;
  fNewSubregions.Clear;
  fToBeSubexpanded.Clear;
  fToBeParentExpanded.Clear;
end;

constructor TBoldRegionExpander.Create;
begin
  inherited;
  fNewParentRegions := TBoldRegionLookup.Create;
  fNewSubregions := TBoldRegionLookup.Create;

  fToBeSubExpanded := TBoldOrderedRegionLookup.Create;
  fToBeParentExpanded := TBoldOrderedRegionLookup.Create;
end;

destructor TBoldRegionExpander.Destroy;
begin
  FreeAndNil(fNewParentRegions);
  FreeAndNil(fNewSubregions);
  FreeAndNil(fToBeSubExpanded);
  FreeAndNil(fToBeParentExpanded);
  inherited;
end;

{
procedure TBoldRegionExpander.ExpandSubAndParentRegions(ExplicitRegions: TBoldRegionList);
begin
  Clear;
//  BoldLog.LogFmt('Start Expanding: %s', [DateTimeToStr(now)]);
  fToBeSubExpanded.AddRegionList(ExplicitRegions);
  fToBeParentExpanded.AddRegionList(ExplicitRegions);
  InternalExpandParents;
  InternalExpandSubRegions;
//  BoldLog.LogFmt('Done Expanding: %s', [DateTimeToStr(now)]);
end;
}

procedure TBoldRegionExpander.ExtractSimilarRegions(Regions: TBoldRegionLookup; Region: TBoldRegion; Result: TBoldRegionLookup);
var
  Traverser: TBoldRegionLookupTraverser;
begin
  Result.Clear;
  Traverser := Regions.CreateTraverser;
  try
    while Traverser.MoveNext do
    begin
      while Assigned(Traverser.Region) and (Traverser.Region.Definition = Region.Definition) and (Traverser.Region.Root.BoldClassTypeINfo = Region.Root.BoldClassTypeInfo) do
      begin
        Result.Add(Traverser.Region);
        Regions.Remove(Traverser.Region);
      end;
    end;
  finally
    Traverser.Free;
  end;
end;

procedure TBoldRegionExpander.ExpandParentRegions(
  RegionsToExpand: TBoldRegionLookup; KnownParentRegions, KnownSubregions: TBoldRegionLookup);
var
  SimilarRegions: TBoldRegionLookup;
  LocalNewParentRegions: TBoldRegionLookup;
  Guard: IBoldGuard;
begin
  if assigned(BoldRegionExpansionDebugLogHandler) then
  begin
    BoldRegionExpansionDebugLogHandler.Separator;
    BoldRegionExpansionDebugLogHandler.Log(sLogExpandingParentRegions);
  end;
  Guard := TBoldGuard.Create(SimilarRegions, LocalNewParentRegions);
  fToBeParentExpanded.AddRegionLookup(RegionsToExpand);
  SimilarRegions := TBoldREgionLookup.Create;
  LocalNewParentRegions := TBoldRegionLookup.Create;
  while fToBeParentExpanded.count > 0 do
  begin
    ExtractSimilarRegions(fToBeParentExpanded, fToBeParentExpanded.firstRegion, SimilarRegions);
    SimilarRegions.FetchAndExpandOneLevelParentRegions(LocalNewParentRegions, NewParentRegions, KnownParentRegions);
    fToBeParentExpanded.AddRegionLookup(LocalNewParentREgions);
    NewParentREgions.AddRegionLookup(LocalNewParentRegions);
    fToBeSubExpanded.AddRegionLookupWithFilter(LocalNewParentRegions, KnownSubregions);
    LocalNewParentRegions.Clear;
    SignalProgress;
  end;
end;

procedure TBoldRegionExpander.ExpandSubregions(
  RegionsToExpand: TBoldRegionLookup; KnownSubregions: TBoldregionLookup);
var
  SimilarRegions: TBoldRegionLookup;
  LocalNewSubregions: TBoldRegionLookup;
  Guard: IBoldGuard;
begin
  if assigned(BoldRegionExpansionDebugLogHandler) then
  begin
    BoldRegionExpansionDebugLogHandler.Separator;
    BoldRegionExpansionDebugLogHandler.Log(sLogExpandingSubRegions);
  end;
  Guard := TBoldGuard.Create(SimilarRegions, LocalNewSubregions);
  fToBeSubExpanded.AddRegionLookup(RegionsToExpand);
  SimilarRegions := TBoldRegionLookup.Create;
  LocalNewSubregions := TBoldRegionLookup.Create;
  while fToBeSubExpanded.count > 0 do
  begin
    ExtractSimilarRegions(fToBeSubExpanded, fToBeSubExpanded.FirstRegion, SimilarRegions);
    SimilarRegions.FetchAndExpandOneLevelSubregions(LocalNewSubregions, fToBeSubExpanded, NewSubRegions, KnownSubRegions);
    fToBeSubExpanded.AddRegionLookup(LocalNewSubregions);
    NewSubregions.AddRegionLookup(LocalNewSubregions);
    LocalNewSubregions.Clear;
    SignalProgress;
  end;
end;

procedure TBoldRegionExpander.SignalProgress;
begin
  if assigned(OnProgress) then
    OnProgress(fNewParentRegions.Count + fNewSubregions.Count,
               fToBeSubExpanded.Count + fToBeParentExpanded.Count);
end;

procedure TBoldRegionExpander.ExpandRegionEnclosure(Regions: TBoldRegionLookup);
var
  EmptyRegionLookup: TBoldregionLookup;
  Guard: IBoldGuard;
begin
  Guard := TBoldGuard.Create(EmptyRegionLookup);
  EmptyRegionLookup := TBoldRegionLookup.Create;

  ExpandParentRegions(Regions, EmptyRegionLookup, EmptyRegionLookup);
  Regions.AddRegionLookup(NewParentRegions);
  ExpandSubregions(Regions, EmptyRegionLookup);
  Regions.AddRegionLookup(NewSubRegions);
end;

{ TBoldRegionLookupTraverser }

function TBoldRegionLookupTraverser.GetRegion: TBoldRegion;
begin
  result := inherited Item as TBoldRegion;
end;

initialization
  TBoldOrderedRegionLookup.IX_RegionOrder := -1;
  TBoldRegionLookup.IX_RegionId := -1;
end.

