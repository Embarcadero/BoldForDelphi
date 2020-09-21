unit BoldObjectListControllers;

interface

uses
  Classes,
  BoldSystem,
  BoldObjectSpaceLists,
  BoldDomainElement,
  BoldValueInterfaces,
  BoldId,
  BoldElements,
  BoldDefs,
  BoldSubscription,
  BoldSystemRT;

type
  {forward declarations of all classes}
  TBoldObjectListController = class;
  TBoldClassListController = class;
  TBoldClassList_Proxy = class;
  TBoldObjectList_Proxy = class;

 {---TBoldObjectListController---}
  TBoldObjectListController = class(TBoldAbstractObjectListController)
  private
    FList: TBoldObjectAttributeIndexList;
    FSubscriber: TBoldPassthroughSubscriber;
    procedure _ReceiveObjectDeleted(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const);
    function GetLocatorList: TBoldObjectLocatorList;
    procedure InternalRemoveByIndex(index: Integer);
  protected
    function CreateNew: TBoldElement; override;
    function GetStreamName: string; override;
    function ProxyClass: TBoldMember_ProxyClass; override;
    procedure SubscribeToObjectDeleted(Locator: TBoldObjectLocator); virtual;
  public
    constructor Create(OwningList: TBoldObjectList);
    destructor Destroy; override;
    procedure AddLocator(Locator: TBoldObjectLocator); override;
    procedure AssignContentValue(Source: IBoldValue);
    procedure DropSubscriptions;
    procedure Resubscribe;
    function GetCount: Integer; override;
    function GetLocator(index: Integer): TBoldObjectLocator; override;
    function GetLocatorByQualifiersAndSubscribe(MemberList: TBoldMemberList; Subscriber: TBoldSubscriber): TBoldObjectLocator; override;
    function IncludesLocator(Locator: TBoldObjectLocator): Boolean; override;
    function IndexOfLocator(Locator: TBoldObjectLocator): Integer; override;
    procedure InsertLocator(index: Integer; Locator: TBoldObjectLocator); override;
    procedure MakeDbCurrent; override;
    procedure Move(CurrentIndex: Integer; NewIndex: Integer); override;
    procedure RemoveByIndex(index: Integer); override;
    procedure SetLocator(index: Integer; Locator: TBoldObjectLocator); override;
    procedure FreeContent; override;
    property LocatorList: TBoldObjectLocatorList read GetLocatorList;
  end;

  { TBoldClassListController }
  TBoldClassListController = class(TBoldObjectListController)
  private
    fTimestamp: TBoldTimestampType;
    fAtTimeList: TList;
    function GetAtTimeList: TList;
    procedure FillFromClassList(ObjectList: TBoldObjectList);
    procedure FillFromSystem;
    function ClosestLoadedClassList: TBoldObjectList;
    property AtTimeList: TList read GetAtTimeList;
    function GetIdList(Index: Integer): TBoldObjectID;
    procedure SetFromIdList(List: TBoldObjectIdList; Mode: TBoldDomainElementProxyMode);
  protected
    function GetCanCreateNew: Boolean; override;
    function GetClassTypeInfo: TBoldClassTypeInfo;
    function GetStringrepresentation: String; override;
    procedure SubscribeToObjectDeleted(Locator: TBoldObjectLocator); override;
    property ClassTypeInfo: TBoldClassTypeInfo read GetClassTypeInfo;
    function ProxyClass: TBoldMember_ProxyClass; override;
  public
    constructor Create(OwningList: TBoldObjectList);
    destructor Destroy; override;
    procedure ReceiveClassEvent(BoldObject: TBoldObject; EVENT: TBoldEvent);
    function ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean; override;
    function AtTime(Time: TBoldTimeStampType): TBoldMember; override;
    function HandlesAtTime: Boolean; override;
    procedure MakeDbCurrent; override;
    procedure RemoveByIndex(index: Integer); override;
  end;

  { TBoldObjectList_Proxy }
  TBoldObjectList_Proxy = class(TBoldMember_Proxy)
  private
    function GetObjectListController: TBoldObjectListController;
  protected
    procedure AssignContentValue(Source: IBoldValue); override;
    property ObjectListController: TBoldObjectListController read GetObjectListController;
  end;

  { TBoldClassList_Proxy }
  TBoldClassList_Proxy = class(TBoldMember_Proxy, IBoldObjectIdListRef)
  private
    function GetClassListController: TBoldClassListController;
    // IBoldObjectIdListRef
    procedure SetFromIdList(IdLIst: TBoldObjectIdList);
    function GetIdList(Index: Integer): TBoldObjectID;
    function GetCount: integer;
  protected
    property ClassListController: TBoldClassListController read GetClassListController;
    procedure AssignContentValue(Source: IBoldValue); override;
  end;

implementation

uses
  SysUtils,
  BoldIndexableList,
  BoldDefaultStreamNames,
  BoldValueSpaceInterfaces,
  BoldCoreConsts;

{ TBoldObjectListController }

procedure TBoldObjectListController.AddLocator(Locator: TBoldObjectLocator);
begin
  if not StartModify then
    BoldRaiseLastFailure(OwningList, 'AddLocator', ''); // do not localize

  LocatorList.Add(Locator);
  SubscribeToObjectDeleted(Locator);
  Changed(beItemAdded, [Locator]);

  EndModify;
end;

constructor TBoldObjectListController.Create(OwningList: TBoldObjectList);
begin
  inherited Create(OwningList);
  FList := TBoldObjectLocatorList.Create;
  FSubscriber := TBoldPassthroughSubscriber.CreateWithExtendedReceive(_ReceiveObjectDeleted);
end;

destructor TBoldObjectListController.Destroy;
begin
  FreeAndNil(FList);
  FreeAndNil(FSubscriber);
  inherited;
end;

function TBoldObjectListController.GetCount: Integer;
begin
  Result := LocatorList.Count;
end;

function TBoldObjectListController.GetLocator(index: Integer): TBoldObjectLocator;
begin
  Result := LocatorList[index];
end;

function TBoldObjectListController.GetLocatorByQualifiersAndSubscribe(MemberList: TBoldMemberList; Subscriber: TBoldSubscriber): TBoldObjectLocator;
var
  List: TBoldObjectAttributeIndexList;
begin
  List := TBoldObjectAttributeIndexList(FList);
  if not LocatorList.HasMembersIndex then
  begin
    if assigned(OwningObjectList.BoldRoleRTInfo) and OwningObjectList.BoldRoleRTInfo.IsQualified then
    begin
      // this handles qualified derived associations
      OwningObjectList.EnsureContentsCurrent;
      LocatorList.InitMembersIndex(OwningObjectList, OwningObjectList.BoldRoleRTInfo.Qualifiers);
    end
    else
      raise EBold.CreateFmt(sRolenotQualified, [ClassName]);
  end;
  result := List.GetLocatorByAttributesAndSubscribe(MemberList, Subscriber);
end;

function TBoldObjectListController.GetLocatorList: TBoldObjectLocatorList;
begin
  result := TBoldObjectLocatorList(FList);
end;

function TBoldObjectListController.IncludesLocator(Locator: TBoldObjectLocator): Boolean;
begin
  Result := LocatorList.LocatorInList[Locator];
end;

function TBoldObjectListController.IndexOfLocator(Locator: TBoldObjectLocator): Integer;
begin
  Result := LocatorList.IndexOf(Locator);
end;

procedure TBoldObjectListController.InsertLocator(index: Integer; Locator: TBoldObjectLocator);
begin
  if not StartModify then
    BoldRaiseLastFailure(OwningList, 'InsertLocator', ''); // do not localize

  LocatorList.Insert(index, Locator);
  SubscribeToObjectDeleted(Locator);
  Changed(beItemAdded, [Locator]);

  EndModify;
end;

procedure TBoldObjectListController.Move(CurrentIndex, NewIndex: Integer);
begin
  if not StartModify then
    BoldRaiseLastFailure(OwningList, 'Move', ''); // do not localize

  LocatorList.Move(CurrentIndex, NewIndex);
  Changed(beOrderChanged, []);

  EndModify;
end;

procedure TBoldObjectListController.InternalRemoveByIndex(index: Integer);
var
  Locator: TBoldObjectLocator;
begin
  Locator := LocatorList[Index];
  LocatorList.RemoveByIndex(index);
  Changed(beItemDeleted, [Locator]);
end;

procedure TBoldObjectListController.RemoveByIndex(index: Integer);
begin
  if not StartModify then
    BoldRaiseLastFailure(OwningList, 'RemoveByIndex', ''); // do not localize
  InternalRemoveByIndex(index);
  EndModify;
end;

procedure TBoldObjectListController.SetLocator(index: Integer; Locator: TBoldObjectLocator);
begin
  if not StartModify then
    BoldRaiseLastFailure(OwningList, 'SetLocator', ''); // do not localize

  if Locator = nil then
    RemoveByIndex(index)
  else
  begin
    LocatorList[index] := Locator;
    SubscribeToObjectDeleted(Locator);
    Changed(beItemReplaced, [Locator, Index]);
  end;

  EndModify;
end;

function TBoldObjectListController.GetStreamname: string;
begin
  result := BoldContentName_ObjectIdListRef;
end;

procedure TBoldObjectListController.MakeDbCurrent;
begin
  // do nothing. Object lists are not persistent and therefore always current.
end;

procedure TBoldObjectListController._ReceiveObjectDeleted(
  Originator: TObject; OriginalEvent: TBoldEvent;
  RequestedEvent: TBoldRequestedEvent; const Args: array of const);

  procedure RemoveAllObjectsFromSystem(System: TBoldSystem);
  var
    i: Integer;
  begin
    for i := Count - 1 downto 0 do
      if LocatorList[i].BoldSystem = System then
        InternalRemoveByIndex(i);
  end;

var
  DeletedLocator: TBoldObjectLocator;
begin
  assert(originator is TBoldSystem);
  case RequestedEvent of
    beLocatorDestroying,
    beObjectDeleted:
      begin
        assert(High(Args) = 0);
        assert(Args[0].vType = vtObject);
        DeletedLocator := TBoldObject(Args[0].VObject).BoldObjectLocator;
        if LocatorList.LocatorInList[DeletedLocator] then
        begin
          LocatorList.Remove(DeletedLocator);
          Changed(beItemDeleted, [DeletedLocator]);
        end;
      end;
    beDestroying:
      begin
        RemoveAllObjectsFromSystem(TBoldSystem(Originator));
      end;
  else
    raise EBoldInternal.CreateFmt(sUnknownEvent, [classname]);
  end;
end;

procedure TBoldObjectListController.SubscribeToObjectDeleted(Locator: TBoldObjectLocator);
begin
  if OwningObjectList.SubscribeToObjectsInList then
  begin
    // Object deleted
    Locator.BoldSystem.AddSmallSubscription(FSubscriber, [beObjectDeleted], beObjectDeleted);
    // System Destroyed
    Locator.BoldSystem.AddSmallSubscription(FSubscriber, [beDestroying], beDestroying);
  end;
  if OwningObjectList.SubscribeToLocatorsInList then
    Locator.BoldSystem.AddSmallSubscription(fSubscriber, [beLocatorDestroying], beLocatorDestroying);
end;

procedure TBoldObjectListController.DropSubscriptions;
begin
  FSubscriber.CancelAllSubscriptions;
end;

procedure TBoldObjectListController.Resubscribe;
var
  i: integer;
  LastSystem: TBoldSystem;
  Locator: TBoldObjectLocator;
begin
  LastSystem := nil;
  for i := 0 to Count - 1 do
  begin
    // only add subscriptions once per system for performance.
    Locator := Locatorlist[i];
    if Locator.BoldSystem <> LastSystem then
    begin
      SubscribeToObjectDeleted(LocatorList[i]);
      LastSystem := Locator.BoldSystem;
    end;
  end;
end;

procedure TBoldObjectListController.AssignContentValue(Source: IBoldValue);
var
  s: IBoldObjectIdListRef;
  i: Integer;
begin
  s := nil;
  if not assigned(source) or (source.QueryInterface(IBoldObjectIDListRef, S) = S_OK) then
  begin
    PreChange;
    LocatorList.Clear;
    if assigned(s) then
      for i := 0 to s.Count - 1 do
        LocatorList.Add(BoldSystem.EnsuredLocatorByID[s.IdList[i]]);
    Changed(beValueChanged, []);
  end
  else
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname, 'AssignContentValue']); // do not localize
end;

procedure TBoldObjectListController.FreeContent;
begin
  Locatorlist.Clear;
end;

function TBoldObjectListController.ProxyClass: TBoldMember_ProxyClass;
begin
  Result := TBoldObjectList_Proxy;
end;

{ TBoldClassListController }

function TBoldObjectListController.CreateNew: TBoldElement;
var
  ListtypeInfo: TBoldListTypeInfo;
  ClassTypeInfo: TBoldClassTypeInfo;
begin
  ListTypeInfo := TBoldListTypeInfo(OwningList.Boldtype);
  ClassTypeInfo := TBoldClassTypeInfo(ListTypeInfo.ListElementTypeInfo);
  result := TBoldObjectClass(ClassTypeInfo.ObjectClass).InternalCreateNewWithClassAndSystem(ClassTypeInfo, owningList.BoldSystem, True);
end;

function TBoldClassListController.GetClassTypeInfo: TBoldClassTypeInfo;
var
  listtypeInfo: TBoldLIstTypeInfo;
begin
  ListTypeInfo := TBoldListTypeInfo(OwningList.Boldtype);
  result := TBoldClassTypeInfo(ListTypeInfo.ListElementTypeInfo);
end;

function TBoldClassListController.GetCanCreateNew: Boolean;
begin
  result := true;
  if result and ClassTypeInfo.IsAbstract then
  begin
    SetBoldLastFailureReason(TBoldFailureReason.CreateFmt(sClassIsAbstract, [ClassTypeInfo.ExpressionName], OwningList));
    result := false;
  end;
  if result and ClassTypeInfo.IsLinkClass then
  begin
    SetBoldLastFailureReason(TBoldFailureReason.CreateFmt(sClassIsLinkClass, [ClassTypeInfo.ExpressionName], OwningList));
    result := false;
  end;
end;

function TBoldClassListController.GetStringrepresentation: String;
begin
  result := ClassTypeInfo.ModelName;
end;

procedure TBoldClassListController.FillFromClassList(ObjectList: TBoldObjectList);
var
  i: integer;
begin
  for i := 0 to ObjectList.Count - 1 do
    if ObjectList[i].BoldClassTypeInfo.BoldIsA(ClassTypeinfo) then
      OwningList.Add(ObjectList[i]);
end;

procedure TBoldClassListController.FillFromSystem;
var
  Traverser: TBoldLocatorListTraverser;
  Locator: TBoldObjectLocator;
begin
  Traverser := OwningList.BoldSystem.Locators.CreateTraverser;
  while not Traverser.EndOfList do
  begin
    Locator := Traverser.Locator;
    if assigned(Locator.BoldObject) and not Locator.BoldObject.BoldPersistent
      and (Locator.BoldObject.BoldExistenceState = besExisting)
      and Locator.BoldObject.BoldClassTypeInfo.BoldIsA(ClassTypeinfo) then
    begin
      LocatorList.Add(Locator);
    end;
    Traverser.Next;
  end;
  Traverser.Free;
end;

function TBoldClassListController.ClosestLoadedClassList: TBoldObjectList;
var
  SuperClass: TBoldClassTypeInfo;
begin
  if fTimestamp <> BOLDMAXTIMESTAMP then
  begin
    result := nil;
    exit;
  end;

  SuperClass := ClassTypeInfo.SuperClassTypeInfo;
  while assigned(SuperClass) and (OwningList.BoldSystem.Classes[SuperClass.TopSortedIndex].BoldPersistenceState <> bvpsCurrent) do
    SuperClass := SuperClass.SuperClassTypeInfo;

  if assigned(SuperClass) then
    result := OwningList.BoldSystem.Classes[SuperClass.TopSortedIndex]
  else
    result := nil;
end;

procedure TBoldClassListController.MakeDbCurrent;
var
  SourceList: TBoldObjectList;
begin
  if ClassTypeinfo.Persistent and OwningList.BoldSystem.BoldPersistent then
  begin
    SourceList := ClosestLoadedClassList;

    if assigned(SourceList) then
      FillFromClassList(SourceList)
    else
      DbFetchClassForMember(fTimeStamp)
  end
  else
    FillFromSystem;
  OwningList.BoldPersistenceState := bvpsTransient;
end;

procedure TBoldClassListController.ReceiveClassEvent(BoldObject: TBoldObject; Event: TBoldEvent);
var
  SuperClassTypeInfo: TBoldClassTypeInfo;
  Superclasslist: TBoldObjectList;
begin
  SuperClassTypeInfo := ClassTypeInfo.SuperClassTypeInfo;
  if Assigned(SuperClassTypeInfo) then
  begin
    SuperClassList := BoldSystem.Classes[SuperClassTypeInfo.TopSortedIndex];
    TBoldClassListController(GetControllerForMember(SuperClassList)).ReceiveClassEvent(BoldObject, EVENT);
  end;
  if Owninglist.BoldPersistenceState <> bvpsInvalid then
  begin
    case Event of
      beObjectCreated: AddLocator(BoldObject.BoldObjectLocator);
      beObjectDeleted:
      begin
        // it is a linear operation to delete objects in the classlist
        // invalidating it is also linear, but hopefully it does not have to be done as often.
        // In the persistent case, invalidating has a different effect, since the classlist must be reloaded from db.
        if ClassTypeInfo.Persistent and OwningList.BoldSystem.BoldPersistent then
          LocatorList.Remove(BoldObject.BoldObjectLocator)
        else
          Owninglist.Invalidate;
      end;
      beObjectFetched: OwningList.SendEvent(beObjectFetched);
    else
      raise EBoldInternal.CreateFmt('%s.ReceiveClassEvent: Unknown event', [ClassName]);
    end;
  end
  else
    if Event = beObjectFetched then
    begin
      OwningList.SendEvent(beObjectFetched);
    end;


  case event of
    beObjectCreated: OwningList.SendExtendedEvent(beItemAdded, [BoldObject.BoldObjectLocator]);
    beObjectDeleted: OwningList.SendExtendedEvent(beItemDeleted, [BoldObject.BoldObjectLocator]);
  end;
end;

procedure TBoldClassListController.RemoveByIndex(index: Integer);
begin
  LocatorList[Index].EnsuredBoldObject.Delete;
end;

procedure TBoldClassListController.SubscribeToObjectDeleted;
begin
  // remove behaviour from parent
end;

function TBoldClassListController.HandlesAtTime: Boolean;
begin
  result := true;
end;

function TBoldClassListController.AtTime(
  Time: TBoldTimeStampType): TBoldMember;
var
  i: Integer;
begin
  if Time = BOLDMAXTIMESTAMP then
    result := BoldSystem.Classes[((OwningList.BoldType as TBoldListTypeInfo).ListElementTypeInfo as TBoldClassTypeInfo).TopSortedIndex]
  else
  begin
    result := nil;
    for i := 0 to AtTimeList.Count - 1 do
      if (GetControllerForMember(TBoldObjectList(AtTimeList[i])) as TBoldClassListController).fTimestamp = Time then
        result := TBoldObjectList(AtTimeList[i]);
    if not assigned(result) then
    begin
    result := TBoldObjectList.InternalCreateClassList(BoldSystem, OwningList.BoldType as TBoldListTypeInfo);
    TBoldClassListController(GetControllerForMember(result)).fTimestamp := Time;
      AtTimeList.Add(result);
    end;
  end;
end;

function TBoldClassListController.GetAtTimeList: TList;
begin
  if not assigned(fAtTimeList) then
    fAtTimeList := TList.Create;
  result := fAtTimeList;
end;

destructor TBoldClassListController.Destroy;
var
  i: Integer;
begin
  inherited;
  if assigned(fAtTimeList) then
    for i := 0 to fAtTimeList.Count - 1 do
      TObject(fAtTimeList[i]).Free;
  FreeAndNil(fAtTimeList);
end;

constructor TBoldClassListController.Create(OwningList: TBoldObjectList);
begin
  inherited Create(OwningList);
  fTimestamp := BOLDMAXTIMESTAMP;
end;

function TBoldClassListController.ProxyClass: TBoldMember_ProxyClass;
begin
  Result := TBoldClassList_Proxy;
end;

function TBoldClassListController.GetIdList(Index: Integer): TBoldObjectID;
var
  aLocator: TBoldObjectLocator;
begin
  aLocator := LocatorList[Index];
  result := aLocator.BoldObjectId
end;

{
procedure TBoldClassListController.AddTransientFromSystem;
var
  Traverser: TBoldLocatorListTraverser;
  Locator: TBoldObjectLocator;
begin
  Traverser := OwningList.BoldSystem.Locators.CreateTraverser;
  while not Traverser.EndOfList do
  begin
    Locator := Traverser.Locator;
    if assigned(Locator.BoldObject) and not Locator.BoldObject.BoldPersistent and Locator.BoldObject.BoldClassTypeInfo.BoldIsA(ClassTypeinfo) then
      LocatorList.Add(Locator);
    Traverser.Next;
  end;
  Traverser.Free;
end;
}

procedure TBoldClassListController.SetFromIdList(
  List: TBoldObjectIdList; Mode: TBoldDomainElementProxyMode);

  procedure InternalAddLocator(NewLocator: TBoldObjectLocator);
  begin
    Assert(Assigned(NewLocator));
    if not IncludesLocator(NewLocator) then
    begin
      PreChange;
      LocatorList.Add(NewLocator);
      Changed(beItemAdded, [NewLocator]);
    end;
  end;

  procedure InternalAddId(ID: TBoldObjectId);
  begin
    InternalAddLocator(BoldSystem.EnsuredLocatorByID[ID]);
  end;

  procedure AddTransientFromSystem(List:TBoldObjectIdList);
  var
    Traverser: TBoldLocatorListTraverser;
    Locator: TBoldObjectLocator;
  begin
    Traverser := OwningList.BoldSystem.Locators.CreateTraverser;
    while not Traverser.EndOfList do
    begin
      Locator := Traverser.Locator;
      if assigned(Locator.BoldObject) and not Locator.BoldObject.BoldPersistent and Locator.BoldObject.BoldClassTypeInfo.BoldIsA(ClassTypeinfo) then
        List.Add(Locator.BoldObjectID);
      Traverser.Next;
    end;
    Traverser.Free;
  end;

  var
  I: Integer;
  NewList: TBoldObjectIdlist;
  Locator: TBoldObjectLocator;
  TheObject: TBoldObject;

begin

  if assigned(List) then
    NewList := List.Clone
  else
    NewList := TBoldObjectidList.create;

    if mode = bdepPMIn then
    begin
     // remove objects that have been deleted in memory
      for I := NewList.Count - 1 downto 0 do
      begin
        Locator := BoldSystem.Locators.LocatorByID[NewList[i]];
        if assigned(Locator) and assigned(Locator.BoldObject) and (Locator.BoldObject.BoldExistenceState = besDeleted) then
          NewList.RemoveByIndex(i);
      end;

      // Add New Objects
      for I := 0 to BoldSystem.DirtyObjects.Count - 1 do
      begin
        TheObject := TBoldObject(BoldSystem.DirtyObjects[I]);
         if TheObject.BoldClassTypeInfo.BoldIsA(ClassTypeInfo) and
          ((TheObject.BoldExistenceState = besExisting) and (TheObject.BoldPersistenceState = bvpsModified)) then
          NewList.add(TheObject.BoldObjectLocator.BoldObjectID)
      end;

      AddTransientFromSystem(NewList);
    end;

  // fix§up locator list with minimum impact
  for I := GetCount - 1 downto 0 do
    if not NewList.IdInList[GetLocator(I).BoldObjectID] then
      LocatorList.RemoveByIndex(I);
  for I := 0 to NewList.Count - 1 do
    InternalAddId(NewList[I]);
  NewList.Free;
end;

function TBoldClassListController.ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode;
  out Obj): Boolean;
begin
  if IsEqualGuid(IID, IBoldObjectIdListRef) then
  begin
    result := ProxyClass.create(self.OwningList, Mode).GetInterface(IID, obj);
    if not result then
      raise EBoldInternal.CreateFmt('ProxyClass for %s did not implement IBoldObjectIdListRef', [ClassName]);
  end
  else
    result := inherited ProxyInterface(IID, Mode, Obj);
end;

{ TBoldClassList_Proxy }

procedure TBoldClassList_Proxy.AssignContentValue(Source: IBoldValue);
begin
  if Mode = bdepContents then
    ClassListController.AssignContentValue(Source)
  else
   UnsupportedMode(Mode, 'AssignContentValue'); // do not localize
end;

function TBoldClassList_Proxy.GetClassListController: TBoldClassListController;
begin
  Result := TBoldClassListController(ProxedController);
end;

function TBoldClassList_Proxy.GetCount: integer;
begin
  Result := ClassListController.Count;
end;

function TBoldClassList_Proxy.GetIdList(Index: Integer): TBoldObjectID;
begin
  Result := ClassListController.GetIdList(index);
end;

procedure TBoldClassList_Proxy.SetFromIdList(IdLIst: TBoldObjectIdList);
begin
  if Mode = bdepPMIn then  { TODO : Move in implementation to proxy }
    ClassListController.SetFromIdList(IdLIst, Mode)
  else
    UnsupportedMode(Mode, 'SetFromIdList'); // do not localize
end;

{ TBoldObjectList_Proxy }

procedure TBoldObjectList_Proxy.AssignContentValue(Source: IBoldValue);
begin
  if Mode = bdepContents then
    ObjectListController.AssignContentValue(Source)
  else
   UnsupportedMode(Mode, 'AssignContentValue'); // do not localize
end;

function TBoldObjectList_Proxy.GetObjectListController: TBoldObjectListController;
begin
  Result := TBoldObjectListController(ProxedController);
end;

end.
