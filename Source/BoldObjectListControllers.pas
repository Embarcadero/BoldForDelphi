
{ Global compiler directives }
{$include bold.inc}
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
    FSubscriber: TBoldExtendedPassthroughSubscriber;
    procedure _ReceiveObjectDeleted(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const);
    function GetLocatorList: TBoldObjectLocatorList; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    procedure InternalRemoveByIndex(index: Integer);
  protected
    function CreateNew: TBoldElement; override;
    function GetStreamName: string; override;
    function GetProxy(Member: TBoldMember; Mode: TBoldDomainElementProxyMode): TBoldMember_Proxy; override;
    procedure SubscribeToObjectDeleted(Locator: TBoldObjectLocator); virtual;
    function GetCapacity: integer; override;
    procedure SetCapacity(const Value: integer); override;
  public
    constructor Create(OwningList: TBoldObjectList); reintroduce;
    destructor Destroy; override;
    procedure AddLocator(Locator: TBoldObjectLocator); override;
    procedure AssignContentValue(Source: IBoldValue);
    procedure DropSubscriptions; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
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
    procedure Clear; override;
    property LocatorList: TBoldObjectLocatorList read GetLocatorList;
  end;

{  BoldPersistenceState:
    bvpsInvalid: List contains just objcets that have been loaded or allloadedobjects
    bvpsTransient: Means that all IDs are loaded
    bvpsCurrent: Means all IDs and all Objects are loaded.
}
  { TBoldClassListController }
  TBoldClassListController = class(TBoldObjectListController)
  private
    fTimestamp: TBoldTimestampType;
    fAtTimeList: TList;
    fClassTypeInfo: TBoldClassTypeInfo;
    fSuperclasslist: TBoldObjectList;
    fSuperClassController: TBoldClassListController;
    fLoadedObjectCount: Integer;
    function GetAtTimeList: TList;
    procedure FillFromClassList(ObjectList: TBoldObjectList);
    procedure FillFromSystem;
    property AtTimeList: TList read GetAtTimeList;
    function GetIdList(Index: Integer): TBoldObjectID; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    procedure SetFromIdList(List: TBoldObjectIdList; Mode: TBoldDomainElementProxyMode);
    procedure MarkAsAllIDsLoaded;
    procedure MarkListCurrent;
    procedure CheckStillCurrent;
    procedure SetPersistenceState(APersistenceState: TBoldValuePersistenceState);
  protected
    function GetCanCreateNew: Boolean; override;
    function GetStringrepresentation: String; override;
    procedure SubscribeToObjectDeleted(Locator: TBoldObjectLocator); override;
    property ClassTypeInfo: TBoldClassTypeInfo read fClassTypeInfo;
    function GetProxy(Member: TBoldMember; Mode: TBoldDomainElementProxyMode): TBoldMember_Proxy; override;
  public
    constructor Create(OwningList: TBoldObjectList);
    destructor Destroy; override;
    procedure ReceiveClassEvent(BoldObject: TBoldObject; Event: TBoldEvent);
    function ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean; override;
    function AtTime(Time: TBoldTimeStampType): TBoldMember; override;
    function HandlesAtTime: Boolean; override;
    procedure MakeDbCurrent; override;
    procedure AddLocator(Locator: TBoldObjectLocator); override;
    procedure RemoveByIndex(index: Integer); override;
    property LoadedObjectCount: Integer read fLoadedObjectCount;
    function HasLoadedSuperClass: boolean;
    function ClosestLoadedClassList: TBoldObjectList;
    function IsCurrentOrSuperClassIsCurrent: boolean; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
  end;

  { TBoldObjectList_Proxy }
  TBoldObjectList_Proxy = class(TBoldMember_Proxy)
  private
    function GetObjectListController: TBoldObjectListController; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
  protected
    procedure AssignContentValue(const Source: IBoldValue); override;
    property ObjectListController: TBoldObjectListController read GetObjectListController;
  end;

  { TBoldClassList_Proxy }
  TBoldClassList_Proxy = class(TBoldMember_Proxy, IBoldObjectIdListRef)
  private
    function GetClassListController: TBoldClassListController; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    procedure SetFromIdList(IdLIst: TBoldObjectIdList);
    procedure SetList(IdList: TBoldObjectIdList);
    function GetIdList(Index: Integer): TBoldObjectID; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    function GetCount: integer; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
  protected
    property ClassListController: TBoldClassListController read GetClassListController;
    procedure AssignContentValue(const Source: IBoldValue); override;
  end;

const
  beClassListStateChanged = 31; // this is used so that we can subscribe to BoldPersistenceState of a class list

implementation

uses
  SysUtils,
  BoldIndexableList,
  BoldDefaultStreamNames,  
  BoldValueSpaceInterfaces;

{ TBoldObjectListController }

constructor TBoldObjectListController.Create(OwningList: TBoldObjectList);
begin
  inherited Create(OwningList);
  FList := TBoldObjectLocatorList.Create;
end;

destructor TBoldObjectListController.Destroy;
begin
  FreeAndNil(FList);
  FreeAndNil(FSubscriber);
  inherited;
end;

function TBoldObjectListController.GetLocatorList: TBoldObjectLocatorList;
begin
  result := TBoldObjectLocatorList(FList);
end;

procedure TBoldObjectListController.AddLocator(Locator: TBoldObjectLocator);
begin
  if not StartModify then
    BoldRaiseLastFailure(OwningList, 'AddLocator', '');

{$IFNDEF AllowCrossSystemLists}
  if (BoldSystemCount > 1) and Assigned(BoldSystem) then
  begin
    Assert(Assigned(Locator), 'Locator not Assigned');
    Assert(Assigned(Locator.BoldSystem), 'Locator.BoldSystem not Assigned');
    if Locator.BoldSystem <> BoldSystem then
      SetBoldLastFailureReason(TBoldFailureReason.CreateFmt('TBoldObjectListController.AddLocator: Locator from another system not allowed to be added to %s, Define conditional AllowCrossSystemLists if you want to allow this.', [OwningMember.DisplayName], OwningList));
  end;
{$ENDIF}

  LocatorList.Add(Locator);
  SubscribeToObjectDeleted(Locator);
  Changed(beItemAdded, [Locator]);

  EndModify;
end;

function TBoldObjectListController.GetCapacity: integer;
begin
  result := LocatorList.Capacity;
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
      OwningObjectList.EnsureContentsCurrent;
      LocatorList.InitMembersIndex(OwningObjectList, OwningObjectList.BoldRoleRTInfo.Qualifiers);
    end
    else
      raise EBold.CreateFmt('%s.GetLocatorByQualifiersAndSubscribe: Object list does not have a member index or role is not qualified', [ClassName]);
  end;
  result := List.GetLocatorByAttributesAndSubscribe(MemberList, Subscriber);
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
    BoldRaiseLastFailure(OwningList, 'InsertLocator', '');
{$IFNDEF AllowCrossSystemLists}
  if (BoldSystemCount > 1) and Assigned(BoldSystem) then
  begin
    Assert(Assigned(Locator), 'Locator not Assigned');
    Assert(Assigned(Locator.BoldSystem), 'Locator.BoldSystem not Assigned');
    if Locator.BoldSystem <> BoldSystem then
      SetBoldLastFailureReason(TBoldFailureReason.CreateFmt('TBoldObjectListController.InsertLocator: Locator from another system not allowed to be inserted in %s, Define conditional AllowCrossSystemLists if you want to allow this.', [OwningMember.DisplayName], OwningList));
  end;
{$ENDIF}
  LocatorList.Insert(index, Locator);
  SubscribeToObjectDeleted(Locator);
  Changed(beItemAdded, [Locator]);

  EndModify;
end;

procedure TBoldObjectListController.Move(CurrentIndex, NewIndex: Integer);
begin
  if not StartModify then
    BoldRaiseLastFailure(OwningList, 'Move', '');

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
    BoldRaiseLastFailure(OwningList, 'RemoveByIndex', '');
  InternalRemoveByIndex(index);
  EndModify;
end;

procedure TBoldObjectListController.SetCapacity(const Value: integer);
begin
  LocatorList.Capacity := Value;
end;

procedure TBoldObjectListController.SetLocator(index: Integer; Locator: TBoldObjectLocator);
begin
  if not StartModify then
    BoldRaiseLastFailure(OwningList, 'SetLocator', '');

  if Locator = nil then
    RemoveByIndex(index)
  else
  begin
{$IFNDEF AllowCrossSystemLists}
  if (BoldSystemCount > 1) and Assigned(BoldSystem) then
    begin
      Assert(Assigned(Locator), 'Locator not Assigned');
      Assert(Assigned(Locator.BoldSystem), 'Locator.BoldSystem not Assigned');
      if Locator.BoldSystem <> BoldSystem then
        SetBoldLastFailureReason(TBoldFailureReason.CreateFmt('TBoldObjectListController.SetLocator: Locator from another system not allowed to be inserted in %s, Define conditional AllowCrossSystemLists if you want to allow this.', [OwningMember.DisplayName], OwningList));
    end;
{$ENDIF}
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
end;

procedure TBoldObjectListController._ReceiveObjectDeleted(
  Originator: TObject; OriginalEvent: TBoldEvent;
  RequestedEvent: TBoldRequestedEvent; const Args: array of const);

  procedure RemoveAllObjectsFromSystem(System: TBoldSystem);
  var
    i: Integer;
  begin
  {$IFDEF AllowCrossSystemLists}
    for i := Count - 1 downto 0 do
      if LocatorList[i].BoldSystem = System then
        InternalRemoveByIndex(i);
  {$ELSE}
    LocatorList.Clear;
  {$ENDIF}
  end;

  procedure RemoveLocator(Locator: TBoldObjectLocator);
  begin
    if LocatorList.LocatorInList[Locator] then
    begin
      LocatorList.Remove(Locator);
      Changed(beItemDeleted, [Locator]);
    end;
  end;

var
  DeletedLocator: TBoldObjectLocator;
begin
  assert(originator is TBoldObjectList);
  case OriginalEvent of
    beLocatorDestroying,
    beObjectDeleted:
      begin
        assert(High(Args) = 0);
        assert(Args[0].vType = vtObject);
        Assert(Args[0].VObject is TBoldObjectLocator);
        DeletedLocator := TBoldObjectLocator(Args[0].VObject);
//        Assert(not Assigned(OwningList.Boldtype) or DeletedLocator.BoldObject.BoldClassTypeInfo.BoldIsA(TBoldListTypeInfo(OwningList.Boldtype).ListElementTypeInfo));
        RemoveLocator(DeletedLocator);
      end;
    beDestroying:
      begin
        RemoveAllObjectsFromSystem(TBoldSystem(TBoldObjectList(Originator).OwningElement));
      end;
  else
    raise EBoldInternal.CreateFmt('%s._ReceiveObjectDeleted: Unknown event', [classname]);
  end;
end;

procedure TBoldObjectListController.SubscribeToObjectDeleted(Locator: TBoldObjectLocator);
var
  ClassList: TBoldObjectList;
begin
  with OwningObjectList do
  begin
    if not (SubscribeToLocatorsInList or SubscribeToObjectsInList) then
      exit;
    if Assigned(FSubscriber) then
      exit
    else
      FSubscriber := TBoldExtendedPassthroughSubscriber.CreateWithExtendedReceive(_ReceiveObjectDeleted);

    if Assigned(OwningObjectList.BoldType) then
      ClassList := Locator.BoldSystem.Classes[TBoldClassTypeInfo(TBoldListTypeInfo(OwningObjectList.BoldType).ListElementTypeInfo).TopSortedIndex]
    else
      ClassList := Locator.BoldSystem.Classes[0];

    if SubscribeToLocatorsInList then
    begin
      if SubscribeToObjectsInList then
        ClassList.AddSmallSubscription(FSubscriber, [beLocatorDestroying, beDestroying, beObjectDeleted], beLocatorDestroying)
      else
        ClassList.AddSmallSubscription(fSubscriber, [beLocatorDestroying], beLocatorDestroying);
    end
    else
      if SubscribeToObjectsInList then
        ClassList.AddSmallSubscription(FSubscriber, [beDestroying, beObjectDeleted], beLocatorDestroying);
  end;
end;

procedure TBoldObjectListController.DropSubscriptions;
begin
  FreeAndNil(FSubscriber);
end;

procedure TBoldObjectListController.Resubscribe;
var
  i: integer;
  LastSystem: TBoldSystem;
  Locator: TBoldObjectLocator;
begin
  Locator := (LocatorList.Any) as TBoldObjectLocator;
  if Assigned(Locator) then
  begin
    SubscribeToObjectDeleted(Locator);
    if BoldSystemCount > 1 then
    begin
      LastSystem := Locator.BoldSystem;
      for i := 0 to Count - 1 do
      begin
        Locator := Locatorlist[i];
        if Locator.BoldSystem <> LastSystem then
        begin
          SubscribeToObjectDeleted(LocatorList[i]);
          LastSystem := Locator.BoldSystem;
        end;
      end;
    end;
  end
end;

procedure TBoldObjectListController.Clear;
var
  i: integer;
begin
  with OwningObjectList do
    if MemberHasSubscribers or IsPartOfSystem then
      inherited
    else
      LocatorList.Clear;
  DropSubscriptions;
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
    raise EBold.CreateFmt('%s.AssignContentValue: unknown type of source', [classname]);
end;

procedure TBoldObjectListController.FreeContent;
begin
  Locatorlist.Clear;
end;

function TBoldObjectListController.GetProxy(Member: TBoldMember; Mode: TBoldDomainElementProxyMode): TBoldMember_Proxy;
begin
  Result := TBoldObjectList_Proxy.Create(Member, Mode);
end;

{ TBoldClassListController }

function TBoldObjectListController.CreateNew: TBoldElement;
var
  ClassTypeInfo: TBoldClassTypeInfo;
begin
  ClassTypeInfo :=  TBoldClassTypeInfo(TBoldListTypeInfo(OwningList.Boldtype).ListElementTypeInfo);
  result := TBoldObjectClass(ClassTypeInfo.ObjectClass).InternalCreateNewWithClassAndSystem(ClassTypeInfo, BoldSystem, True);
end;

function TBoldClassListController.GetCanCreateNew: Boolean;
begin
  result := true;
  if result and ClassTypeInfo.IsAbstract then
  begin
    SetBoldLastFailureReason(TBoldFailureReason.CreateFmt('%s is an abstract class', [ClassTypeInfo.ExpressionName], OwningList));
    result := false;
  end;
  if result and ClassTypeInfo.IsLinkClass then
  begin
    SetBoldLastFailureReason(TBoldFailureReason.CreateFmt('%s is a LinkClass', [ClassTypeInfo.ExpressionName], OwningList));
    result := false;
  end;
end;

function TBoldClassListController.GetStringrepresentation: String;
begin
  result := ClassTypeInfo.ModelName;
end;

procedure TBoldClassListController.FillFromClassList(ObjectList: TBoldObjectList);
var
  I: Integer;
  iTopSortedIndex: Integer;
  DestinationList: TBoldObjectList;
  Locator: TBoldObjectLocator;
begin
  iTopSortedIndex := ClassTypeinfo.TopSortedIndex;
  DestinationList := OwningObjectList;
  for I := 0 to ObjectList.Count - 1 do
  begin
    Locator := ObjectList.Locators[I];
    if Assigned(Locator.BoldObject) then
    begin
      if Locator.BoldObject.BoldClassTypeInfo.BoldIsA(ClassTypeInfo) then
        DestinationList.AddLocator(Locator);
    end
    else
    if Locator.BoldObjectID.TopSortedIndexExact and
      ((Locator.BoldObjectID.TopSortedIndex = iTopSortedIndex) or
      (Locator.BoldClassTypeInfo.BoldIsA(ClassTypeinfo))) then
    begin
      DestinationList.AddLocator(Locator);
      iTopSortedIndex := Locator.BoldObjectID.TopSortedIndex;
    end
    else
    if Locator.EnsuredBoldObject.BoldClassTypeInfo.BoldIsA(ClassTypeInfo) then
      DestinationList.AddLocator(Locator);
  end;
end;

procedure TBoldClassListController.FillFromSystem;
var
  Traverser: TBoldLocatorListTraverser;
  Locator: TBoldObjectLocator;
begin
  Traverser := BoldSystem.Locators.CreateTraverser;
  while Traverser.MoveNext do
  begin
    Locator := Traverser.Locator;
    if assigned(Locator.BoldObject) and not Locator.BoldObject.BoldPersistent
      and (Locator.BoldObject.BoldExistenceState = besExisting)
      and Locator.BoldObject.BoldClassTypeInfo.BoldIsA(ClassTypeinfo) then
    begin
      LocatorList.Add(Locator);
    end;
  end;
  Traverser.Free;
end;

function TBoldClassListController.ClosestLoadedClassList: TBoldObjectList;
var
  SuperClass: TBoldClassTypeInfo;
  SuperClassList: TBoldObjectList;
begin
  if fTimestamp <> BOLDMAXTIMESTAMP then
  begin
    result := nil;
    exit;
  end;

  SuperClass := ClassTypeInfo.SuperClassTypeInfo;
  with BoldSystem do
    while assigned(SuperClass) and (Classes[SuperClass.TopSortedIndex].BoldPersistenceState <> bvpsCurrent) do
      SuperClass := SuperClass.SuperClassTypeInfo;

  if assigned(SuperClass) then
    result := BoldSystem.Classes[SuperClass.TopSortedIndex]
  else
    result := nil;
end;

procedure TBoldClassListController.MakeDbCurrent;
var
  SourceList: TBoldObjectList;
begin
  if ClassTypeinfo.Persistent and BoldSystem.BoldPersistent then
  begin
    SourceList := ClosestLoadedClassList;

    if assigned(SourceList) then
      FillFromClassList(SourceList)
    else
      DbFetchClassForMember(fTimeStamp)
  end
  else
    FillFromSystem;
  MarkAsAllIDsLoaded;
end;

procedure TBoldClassListController.MarkAsAllIDsLoaded;
begin
  if fLoadedObjectCount = Count then
    MarkListCurrent
  else
    SetPersistenceState(bvpsTransient);
end;

procedure TBoldClassListController.MarkListCurrent;
begin
  if (OwningList.BoldPersistenceState <> bvpsCurrent) and (fLoadedObjectCount = Count) then
  begin
    SetPersistenceState(bvpsCurrent);
  end;
end;

procedure TBoldClassListController.ReceiveClassEvent(BoldObject: TBoldObject; Event: TBoldEvent);
begin
  if Assigned(fSuperClassController) then
    fSuperClassController.ReceiveClassEvent(BoldObject, Event);
  case Event of
    beObjectCreated:
      begin
        inc(fLoadedObjectCount);
        if Owninglist.BoldPersistenceState <> bvpsInvalid then
        begin
//          Assert(not LocatorList.Includes(BoldObject.BoldObjectLocator), 'Locator already in list on beObjectCreated.');
          AddLocator(BoldObject.BoldObjectLocator)
        end;
        Owninglist.Invalidate;
      end;
    beObjectDeleted:
    begin
      if Owninglist.BoldPersistenceState <> bvpsInvalid then
      begin
        if ClassTypeInfo.Persistent and BoldSystem.BoldPersistent then
          LocatorList.Remove(BoldObject.BoldObjectLocator);
      end;
      Dec(fLoadedObjectCount);
      Owninglist.Invalidate;
      OwningList.SendExtendedEvent(Event, [BoldObject.BoldObjectLocator]);
    end;
    beObjectFetched:
    begin
      inc(fLoadedObjectCount);
      if Owninglist.IsCurrent then
      begin
//        Assert(not LocatorList.Includes(BoldObject.BoldObjectLocator), 'Locator already in list on beObjectFetched.');
        AddLocator(BoldObject.BoldObjectLocator);
        OwningList.SendExtendedEvent(beValueInvalid, [BoldObject.BoldObjectLocator]);
      end
      else
      if (Owninglist.BoldPersistenceState = bvpsTransient) and (fLoadedObjectCount = count) then
      begin
//        Assert(LocatorList.Includes(BoldObject.BoldObjectLocator), 'Locator not already in list on beObjectFetched.');
        MarkListCurrent;
        OwningList.SendExtendedEvent(beValueInvalid, [BoldObject.BoldObjectLocator]);
      end
      else
        Owninglist.Invalidate;
    end;
    beObjectUnloaded:
    begin
      if not BoldObject.BoldObjectIsDeleted then
      begin
        Dec(fLoadedObjectCount);
        if Owninglist.BoldPersistenceState = bvpsCurrent then
        begin
          SetPersistenceState(bvpsTransient);
          OwningList.SendEvent(beValueInvalid);
        end
        else
          Owninglist.Invalidate;
      end;
    end;
  end;
  case event of
    beObjectCreated: OwningList.SendExtendedEvent(beItemAdded, [BoldObject.BoldObjectLocator]);
    beObjectDeleted: OwningList.SendExtendedEvent(beItemDeleted, [BoldObject.BoldObjectLocator]);
    beObjectFetched: OwningList.SendExtendedEvent(beObjectFetched, [BoldObject.BoldObjectLocator]);
    beObjectUnloaded: OwningList.SendExtendedEvent(beObjectUnloaded, [BoldObject.BoldObjectLocator])
  else
    raise EBoldInternal.CreateFmt('%s.ReceiveClassEvent: Unknown event: %d', [ClassName, Event]);
  end;
end;

procedure TBoldClassListController.RemoveByIndex(index: Integer);
begin
  Assert(not LocatorList[index].EnsuredBoldObject.BoldObjectIsDeleted);
  LocatorList[Index].EnsuredBoldObject.Delete;
end;

procedure TBoldClassListController.SubscribeToObjectDeleted;
begin
end;

procedure TBoldClassListController.CheckStillCurrent;
begin
  if OwningList.isCurrent and (fLoadedObjectCount <> Count) then
    SetPersistenceState(bvpsTransient);
end;

function TBoldClassListController.HandlesAtTime: Boolean;
begin
  result := true;
end;

function TBoldClassListController.HasLoadedSuperClass: boolean;
begin
  result := ClosestLoadedClassList <> nil;
end;

function TBoldClassListController.IsCurrentOrSuperClassIsCurrent: boolean;
begin
  result := HasLoadedSuperClass or (OwningObjectList.BoldPersistenceState = bvpsCurrent);
end;

procedure TBoldClassListController.AddLocator(Locator: TBoldObjectLocator);
begin
  LocatorList.Add(Locator);
  CheckStillCurrent;
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
  fClassTypeInfo := TBoldClassTypeInfo(TBoldListTypeInfo(OwningList.Boldtype).ListElementTypeInfo);
  if Assigned(ClassTypeInfo.SuperClassTypeInfo) then
  begin
    fSuperClassList := BoldSystem.Classes[ClassTypeInfo.SuperClassTypeInfo.TopSortedIndex];
    fSuperClassController := TBoldClassListController(GetControllerForMember(fSuperClassList));
  end;
  OwningList.DuplicateMode := bldmError;
end;

function TBoldClassListController.GetProxy(Member: TBoldMember; Mode: TBoldDomainElementProxyMode): TBoldMember_Proxy;
begin
  Result := TBoldClassList_Proxy.Create(Member, Mode);
end;

function TBoldClassListController.GetIdList(Index: Integer): TBoldObjectID;
begin
  result := LocatorList[Index].BoldObjectId;
end;

procedure TBoldClassListController.SetFromIdList(
  List: TBoldObjectIdList; Mode: TBoldDomainElementProxyMode);

var
  BoldSystem: TBoldSystem;

  procedure InternalAddId(ID: TBoldObjectId);
  var
    Locator: TBoldObjectLocator;
  begin
    Locator := BoldSystem.EnsuredLocatorByID[ID];
    if not IncludesLocator(Locator) then
    begin
      PreChange;
      LocatorList.Add(Locator);
      Changed(beItemAdded, [Locator]);
    end;
  end;

{$IFNDEF NoTransientInstancesOfPersistentClass}
  procedure AddTransientFromSystem(List:TBoldObjectIdList);
  var
    Locator: TBoldObjectLocator;
  begin
    if not BoldSystem.Locators.IsEmpty then
    for Locator in BoldSystem.Locators do
      if assigned(Locator.BoldObject) and not Locator.BoldObject.BoldPersistent and Locator.BoldObject.BoldClassTypeInfo.BoldIsA(ClassTypeinfo) then
        List.Add(Locator.BoldObjectID);
  end;
{$ENDIF}

var
  I: Integer;
  NewList: TBoldObjectIdlist;
  Locator: TBoldObjectLocator;
  TheObject: TBoldObject;
begin
  BoldSystem := self.BoldSystem;
  if assigned(List) then
    NewList := List.Clone
  else
    NewList := TBoldObjectidList.create;
  try
    if mode = bdepPMIn then
    begin
      if BoldSystem.BoldDirty then // Search for Deleted objects only if system is dirty.
      for I := NewList.Count - 1 downto 0 do
      begin
        Locator := BoldSystem.Locators.LocatorByID[NewList[i]];
        if assigned(Locator) and assigned(Locator.BoldObject) and (Locator.BoldObject.BoldExistenceState = besDeleted) then
          NewList.RemoveByIndex(i);
      end;
      for I := 0 to BoldSystem.DirtyObjects.Count - 1 do
      begin
        TheObject := TBoldObject(BoldSystem.DirtyObjects[I]);
         if TheObject.BoldClassTypeInfo.BoldIsA(ClassTypeInfo) and
          ((TheObject.BoldExistenceState = besExisting) and (TheObject.BoldPersistenceState = bvpsModified)) then
          NewList.add(TheObject.BoldObjectLocator.BoldObjectID)
      end;
{$IFNDEF NoTransientInstancesOfPersistentClass}
      AddTransientFromSystem(NewList);
{$ENDIF}
    end;
    for I := GetCount - 1 downto 0 do
      if not NewList.IdInList[GetLocator(I).BoldObjectID] then
        LocatorList.RemoveByIndex(I);
    for I := 0 to NewList.Count - 1 do
      InternalAddId(NewList[I]);
  finally
    NewList.Free;
  end;
end;

procedure TBoldClassListController.SetPersistenceState(
  APersistenceState: TBoldValuePersistenceState);
begin
  with OwningList do
  begin
    BoldPersistenceState := APersistenceState;
    SendEvent(beClassListStateChanged);
  end;
end;

function TBoldClassListController.ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode;
  out Obj): Boolean;
begin
  if IsEqualGuid(IID, IBoldObjectIdListRef) then
  begin
    result := GetProxy(self.OwningList, Mode).GetInterface(IID, obj);
    if not result then
      raise EBoldInternal.CreateFmt('ProxyClass for %s did not implement IBoldObjectIdListRef', [ClassName]);
  end
  else
    result := inherited ProxyInterface(IID, Mode, Obj);
end;

{ TBoldClassList_Proxy }

function TBoldClassList_Proxy.GetClassListController: TBoldClassListController;
begin
  Result := TBoldClassListController(ProxedController);
end;

procedure TBoldClassList_Proxy.AssignContentValue(const Source: IBoldValue);
begin
  if Mode = bdepContents then
    ClassListController.AssignContentValue(Source)
  else
   UnsupportedMode(Mode, 'AssignContentValue');
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
  if Mode = bdepPMIn then 
    ClassListController.SetFromIdList(IdLIst, Mode)
  else
    UnsupportedMode(Mode, 'SetFromIdList');
end;

procedure TBoldClassList_Proxy.SetList(IdList: TBoldObjectIdList);
var
  i: integer;
begin
  IdList.Clear;
  for I := 0 to ClassListController.Count - 1 do
    IdList.Add(ClassListController.GetIdList(i));
end;

{ TBoldObjectList_Proxy }

function TBoldObjectList_Proxy.GetObjectListController: TBoldObjectListController;
begin
  Result := TBoldObjectListController(ProxedController);
end;

procedure TBoldObjectList_Proxy.AssignContentValue(const Source: IBoldValue);
begin
  if Mode = bdepContents then
    ObjectListController.AssignContentValue(Source)
  else
   UnsupportedMode(Mode, 'AssignContentValue');
end;

initialization

end.
