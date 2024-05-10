unit maan_UndoRedoTestCaseUtils;

interface

uses
  BoldSystem,
  BoldDefs,
  BoldDomainElement,
  BoldAttributes,
  BoldId,
  BoldValueInterfaces,
  BoldValueSpaceInterfaces,
  BoldFreeStandingValues,
  Boldsubscription,
  Classes,
  UndoTestModelClasses,
  sysUtils,
  BoldUndoHandler;

type

  TLoggingSubscriber = class(TBoldSubscriber)
  private
    FEvents: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Receive(Originator: TObject; OriginalEvent: TBoldEvent;
      RequestedEvent: TBoldRequestedEvent); override;
    procedure SubscribeToElement(Element: TBoldDomainElement);
    procedure Refresh;
    procedure VerifyNoEvents(const Element: TBoldDomainElement);
    procedure VerifySendEvent(const EventType: TBoldEvent; const Element: TBoldDomainElement);
  end;


  function EventTypeToString(const EventType: TBoldEvent): string;
  function BoldValuePersistenceStateToString(const State: TBoldValuePersistenceState): string;

  procedure VerifySetContents(const NewValue, OldValue: IBoldObjectContents); overload;
  procedure VerifySetContents(const NewValue, OldValue: IBoldValue); overload;
  procedure VerifySetContentsX(const Member: TBoldMember);
  procedure VerifyRestoreContentsAndState(const NewValue, OldValue: TBoldObject);
  procedure VerifyAddToCurrentUndoArea(const OldValue: TBoldMember; OwningObject: TboldObject; const System: TBoldSystem);
  procedure VerifyState(const Element: TBoldDomainElement; const State: TBoldValuePersistenceState); overload;
  procedure VerifyAdjustFlag(const Element: TBoldDomainElement; const flagSet: Boolean = true);
  procedure VerifyHasOldValues(const Element: TBoldDomainElement; const flagSet: Boolean = true);  
  procedure VerifyListAdjustedIn(const aList: TBoldMember; const anObject: TBoldObject; const AdjustFlagSet: Boolean);
  procedure VerifyListAdjustedEx(const aList: TBoldMember; const anObject: TBoldObject; const AdjustFlagSet: Boolean);
  procedure VerifyIsNil(const Member: TBoldMember);
  procedure VerifyObjectInBlock(Block: TboldUndoBlock; aId: TBoldObjectId; state: TboldExistenceState);
  procedure VerifyIsInUndoArea(aBlock: TBoldUndoBlock; Member: TBoldMember; memberValue: IBoldValue);

  function CreateTopic(const System: TBoldSystem; const Subscriber: TLoggingSubscriber; const Persistent: Boolean = True): TTopic;
  function CreateBook(const System: TBoldSystem; const Subscriber: TLoggingSubscriber; const Persistent: Boolean = True): TBook;
  function CreateSomeClass(const System: TBoldSystem; const Subscriber: TLoggingSubscriber; const Persistent: Boolean = True): TSomeClass;
  function CreateClassWithLink(const System: TBoldSystem; const Subscriber: TLoggingSubscriber): TClassWithLink;
  function CreateAPersistentClass(const System: TBoldSystem; const Subscriber: TLoggingSubscriber): TAPersistentClass;
  function CreateATransientClass(const System: TBoldSystem; const Subscriber: TLoggingSubscriber): TATransientClass;

  function FetchObject(const System: TBoldSystem; const ObjectId: TBoldObjectId): TBoldObject;
  function RefetchObject(const System: TBoldSystem; const ObjId: TBoldObjectId): TBoldObject;
  procedure FetchClass(const System: TBoldSystem; const aList: TBoldObjectList; const ObjClass: TBoldObjectClass);
  procedure FetchEnsuredClass(const System: TBoldSystem; const aList: TBoldObjectList; const ObjClass: TBoldObjectClass);

  procedure GenerateObjects(const System: TBoldSystem; const ExpressionName: string; const Count: integer);

implementation

uses
  maan_UndoRedo;


function CreateTopic(const System: TBoldSystem; const Subscriber: TLoggingSubscriber; const Persistent: Boolean = True): TTopic;
begin
  Result := System.CreateNewObjectByExpressionName('Topic', Persistent) as TTopic;
  if Assigned(Subscriber) then
  begin
    Subscriber.SubscribeToElement(Result.M_name);
    Subscriber.SubscribeToElement(Result.M_Book);
  end;
  Result.name := 'SomeClass' + IntToStr(Trunc(Random(1000)));
end;

function CreateBook(const System: TBoldSystem; const Subscriber: TLoggingSubscriber; const Persistent: Boolean = True): TBook;
begin
  Result := System.CreateNewObjectByExpressionName('Book', Persistent) as TBook;
  if Assigned(Subscriber) then
  begin
    Subscriber.SubscribeToElement(Result.M_Title);
    Subscriber.SubscribeToElement(Result.M_Topic);
  end;
  Result.Title := 'Book' + IntToStr(Trunc(Random(1000)));
end;

function CreateSomeClass(const System: TBoldSystem; const Subscriber: TLoggingSubscriber; const Persistent: Boolean = True): TSomeClass;
begin
  Result := System.CreateNewObjectByExpressionName('SomeClass', Persistent) as TSomeClass;
  if Assigned(Subscriber) then
  begin
    Subscriber.SubscribeToElement(Result.M_aString);
    Subscriber.SubscribeToElement(Result.M_part);
    Subscriber.SubscribeToElement(Result.M_partof);
    Subscriber.SubscribeToElement(Result.M_parent);
    Subscriber.SubscribeToElement(Result.M_child);
    Subscriber.SubscribeToElement(Result.M_previous);
    Subscriber.SubscribeToElement(Result.M_next);
  end;
  Result.aString := 'SomeClass' + IntToStr(Trunc(Random(1000)));
end;

function CreateClassWithLink(const System: TBoldSystem; const Subscriber: TLoggingSubscriber): TClassWithLink;
begin
  Result := System.CreateNewobjectByExpressionName('ClassWithLink', true) as TClassWithLink;
  if Assigned(Subscriber) then
  begin
    Subscriber.SubscribeToElement(Result.M_aString);
    Subscriber.SubscribeToElement(Result.M_many);
    Subscriber.SubscribeToElement(Result.M_one);
  end;
  Result.aString := 'ClassWithLink' + IntToStr(Trunc(Random(1000)));
end;

function CreateAPersistentClass(const System: TBoldSystem; const Subscriber: TLoggingSubscriber): TAPersistentClass;
begin
  Result := System.CreateNewobjectByExpressionName('APersistentClass') as TAPersistentClass;
  if Assigned(Subscriber) then
  begin
   Subscriber.SubscribeToElement(Result.M_aString);
   Subscriber.SubscribeToElement(Result.M_one);
  end;
  Result.aString := 'APersistentClass' + IntToStr(Trunc(Random(1000)));
end;

function CreateATransientClass(const System: TBoldSystem; const Subscriber: TLoggingSubscriber): TATransientClass;
begin
  Result := System.CreateNewobjectByExpressionName('ATransientClass') as TATransientClass;
  if Assigned(Subscriber) then
  begin
    Subscriber.SubscribeToElement(Result.M_aString);
    Subscriber.SubscribeToElement(Result.m_many);
  end;
  Result.aString := 'ATransientClass' + IntToStr(Trunc(Random(1000)));
end;

function FetchObject(const System: TBoldSystem; const ObjectId: TBoldObjectId): TBoldObject;
var
  objLocator: TBoldObjectLocator;
begin
  objLocator := System.EnsuredLocatorByID[Objectid];
  Result := objLocator.EnsuredBoldObject;
end;

function RefetchObject(const System: TBoldSystem; const ObjId: TBoldObjectId): TBoldObject;
begin
  System.Locators.LocatorByID[ObjId].UnloadBoldObject;
  System.EnsuredLocatorByID[ObjId].EnsureBoldObject;
  Result := System.EnsuredLocatorByID[ObjId].BoldObject;
end;


procedure FetchClass(const System: TBoldSystem; const aList: TBoldObjectList; const ObjClass: TBoldObjectClass);
begin
  System.GetAllInClass(aList, ObjClass);
end;

procedure FetchEnsuredClass(const System: TBoldSystem; const aList: TBoldObjectList; const ObjClass: TBoldObjectClass);
begin
  FetchClass(System, aList, ObjClass);
  aList.EnsureObjects;
end;

function EventTypeToString(const EventType: TBoldEvent): string;
begin
  if (EventType = beValueChanged) then
    Result := 'beValueChanged'
  else if (EventType = beValueInvalid) then
    Result := 'beValueInvalid'
  else if (EventType = beValueIdentityChanged) then
    Result := 'beValueIdentityChanged'
  else if (EventType = beMemberChanged) then
    Result := 'beMemberChanged'
  else if (EventType = beItemAdded) then
    Result := 'beItemAdded'
  else if (EventType = beItemDeleted) then
    Result := 'beItemDeleted'
  else if (EventType = beItemReplaced) then
    Result := 'beItemReplaced'
  else if (EventType = beDirtyListInvalidOrItemDeleted) then
    Result := 'beDirtyListInvalidOrItemDeleted'
  else
    Result := Format('Missing: Event=%s(Add it to the function EventTypeToString)', [IntToStr(EventType)]);
end;

function BoldValuePersistenceStateToString(const State: TBoldValuePersistenceState): string;
begin
  case State of
    bvpsCurrent: Result := 'bvpsCurrent';
    bvpsModified: Result := 'bvpsModified';
    bvpsInvalid: Result := 'bvpsInvalid';
    bvpsTransient: Result := 'bvpsTransient';
  end;
end;

procedure VerifySetContents(const NewValue, OldValue: IBoldObjectContents);
begin
  //TODO: implement
end;

procedure VerifySetContents(const NewValue, OldValue: IBoldValue); overload;
begin
  //TODO: implement
end;

procedure VerifySetContentsX(const Member: TBoldMember);
begin
  //TODO: implement
end;

procedure VerifyRestoreContentsAndState(const NewValue, OldValue: TBoldObject);
begin
end;

procedure VerifyAddToCurrentUndoArea(const OldValue: TBoldMember; OwningObject: TBoldObject; const System: TBoldSystem);
var
  Value: IBoldValue;
  UndoHandler: TBoldUndoHandler;
  ClassExpressionName: string;
  procedure GetValue;
  begin
    UndoHandler.UndoBlocks.CurrentBlock.ValueExists(OwningObject.BoldObjectLocator.BoldObjectID,
        OldValue.BoldMemberRTInfo.index, Value);
  end;
begin
  UndoHandler := (System.UndoHandler as TBoldUndoHandler);
  GetValue;
  Assert(Assigned(Value), 'AddToCurrentUndoAreaFailed');
  ClassExpressionname := OwningObject.BoldClassTypeInfo.ExpressionName;
  if ClassExpressionName = 'Book' then
  begin
    Assert((OwningObject as TBook).ValuesAreEqual(Value, OldValue.AsIBoldValue[bdepContents], OldValue.BoldMemberRTInfo.ExpressionName), 'VerifyAddToCurrentUndoArea failed');
  end
  else if ClassExpressionName = 'Topic' then
  begin
    Assert((OwningObject as TTopic).ValuesAreEqual(Value, OldValue.AsIBoldValue[bdepContents], OldValue.BoldMemberRTInfo.ExpressionName), 'VerifyAddToCurrentUndoArea failed');
  end
  else if ClassExpressionName = 'SomeClass' then
  begin
    Assert((OwningObject as TSomeClass).ValuesAreEqual(Value, OldValue.AsIBoldValue[bdepContents], OldValue.BoldMemberRTInfo.ExpressionName), 'VerifyAddToCurrentUndoArea failed');
  end
  else if ClassExpressionName = 'ClassWithLink' then
  begin
    Assert((OwningObject as TClassWithLink).ValuesAreEqual(Value, OldValue.AsIBoldValue[bdepContents], OldValue.BoldMemberRTInfo.ExpressionName), 'VerifyAddToCurrentUndoArea failed');
  end
  else if ClassExpressionName = 'LinkClass' then
  begin
    Assert((OwningObject as TLinkClass).ValuesAreEqual(Value, OldValue.AsIBoldValue[bdepContents], OldValue.BoldMemberRTInfo.ExpressionName), 'VerifyAddToCurrentUndoArea failed');
  end;
end;

procedure VerifyState(const Element: TBoldDomainElement; const State: TBoldValuePersistenceState);
var
  CurState: TBoldValuePersistenceState;
begin
  if (Element is TBoldObject) then
    CurState := (Element as TBoldObject).BoldPersistenceState
  else if (Element is TBoldmember) then
    CurState := (Element as TBoldMember).BoldPersistenceState
  else
    raise EBold.Create('VerifyState: parameter State not assigned');
  Assert(CurState = State, Format('%s Verify State %s failed', [Element.DisplayName, BoldValuePersistenceStateToString(State)]));
end;

procedure VerifyAdjustFlag(const Element: TBoldDomainElement; const flagSet: Boolean = true);
begin
  if (Element is TBoldObjectList) then
  begin
    Assert((Element as TBoldObjectList).Adjusted = flagSet, Format('VerifyAdjustFlag failed for %s', [Element.DisplayName]));
  end
  else
    Assert(false, Format('VerifyAdjustFlag failed for %s: element is not a TBoldObjectList', [Element.DisplayName]));
end;

procedure VerifyHasOldValues(const Element: TBoldDomainElement; const flagSet: Boolean = true);
begin
  if (Element is TBoldObjectReference) then
  begin
    Assert((Element as TBoldObjectReference).HasOldValues = flagSet, Format('VerifyHasOldValues failed for %s', [Element.DisplayName]));
  end
  else
    Assert(false, Format('VerifyHasOldValues failed for %s: element is not a TBoldObjectReference', [Element.DisplayName]));
end;

procedure VerifyListAdjustedIn(const aList: TBoldMember; const anObject: TBoldObject; const AdjustFlagSet: Boolean);
begin
  Assert(aList is TBoldObjectList);
  Assert((aList as TBoldObjectList).Includes(anObject), 'VerifyListAdjustedIn failed');
  if AdjustFlagSet then
    VerifyAdjustFlag(aList);
end;

procedure VerifyListAdjustedEx(const aList: TBoldMember; const anObject: TBoldObject; const AdjustFlagSet: Boolean);
begin
  Assert(aList is TBoldObjectList);
  Assert(not (aList as TBoldObjectList).Includes(anObject), Format('VerifyListAdjustedEx failed', [aList.DisplayName]));
  if AdjustFlagSet then
    VerifyAdjustFlag(aList);
end;

procedure VerifyIsNil(const Member: TBoldMember);
begin
  Assert(Assigned(Member), Format('%s VerifyIsNil failed', [Member.DisplayName]));
end;

procedure GenerateObjects(const System: TBoldSystem; const ExpressionName: string; const Count: integer);
var
  i: integer;
begin
  if ExpressionName = 'SomeClass' then
  begin
    for i:= 1 to Count do
      CreateSomeClass(System, nil);
  end
  else if ExpressionName = 'Book' then
  begin
   for i:= 1 to Count do
     CreateBook(System, nil);
  end
  else if ExpressionName = 'Topic' then
  begin
    for i:= 1 to Count do
      CreateTopic(System, nil);
  end
  else if ExpressionName = 'ClassWithLink' then
  begin
    for i:= 1 to Count do
      CreateClassWithLink(system, nil);
  end
  else if ExpressionName = 'APersistentClass' then
  begin
    for i:= 1 to Count do
      CreateAPersistentClass(System, nil);
  end
  else if ExpressionName = 'ATransientClass' then
  begin
    for i:= 1 to Count do
      CreateATransientClass(System, nil);
  end;
end;

{ TLoggingSubscriber }

constructor TLoggingSubscriber.Create;
begin
  inherited;
  FEvents := TStringList.Create;
end;

destructor TLoggingSubscriber.Destroy;
begin
  FreeAndNil(FEvents);
  inherited;
end;

procedure TLoggingSubscriber.Receive(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
var
  ObjId, ObjectName, MemberName: string;
begin
  if (Originator is TBoldObject) then
  begin
    ObjectName := (Originator as TBoldObject).BoldClassTypeInfo.ExpressionName;
    ObjId := (Originator as TBoldObject).BoldObjectLocator.BoldObjectID.AsString;
    memberName := '';
  end;
  if (Originator is TBoldMember) then
  begin
    ObjectName := (Originator as TBoldMember).OwningObject.BoldClassTypeInfo.ExpressionName;
    ObjId := (Originator as TBoldMember).OwningObject.BoldObjectLocator.BoldObjectID.AsString;
    MemberName := (Originator as TBoldMember).BoldMemberRTInfo.ExpressionName;
  end;
  FEvents.Add(Format('%s%s:%s:%s', [ObjectName, ObjId, MemberName, EventTypeToString(OriginalEvent)]));
end;

procedure TLoggingSubscriber.Refresh;
begin
  FEvents.Clear;
end;

procedure TLoggingSubscriber.SubscribeToElement(Element: TBoldDomainElement);
begin
  Element.AddSubscription(self, beValueChanged, beValueChanged);
  Element.AddSubscription(self, beValueInvalid, beValueInvalid);
  Element.AddSubscription(self, beValueIdentityChanged, beValueIdentityChanged);
  Element.AddSubscription(self, beMemberChanged, beMemberChanged);
  Element.AddSubscription(self, beItemAdded, beItemAdded);
  Element.AddSubscription(self, beItemDeleted, beItemDeleted);
  Element.AddSubscription(self, beItemReplaced, beItemReplaced);
  Element.AddSubscription(self, beDirtyListInvalidOrItemDeleted, beDirtyListInvalidOrItemDeleted);
end;

procedure TLoggingSubscriber.VerifyNoEvents(const Element: TBoldDomainElement);
var
  ObjectName, oid, MemberName: string;
  i: integer;
  s: string;
  Found: Boolean;
begin
  Found := false;
  if (Element is TBoldObject) then
  begin
    ObjectName := (Element as TBoldObject).BoldClassTypeInfo.ExpressionName;
    oid := (Element as TBoldObject).BoldObjectLocator.BoldObjectID.AsString;
    memberName := '';
  end;
  if (Element is TBoldMember) then
  begin
    ObjectName := (Element as TBoldMember).OwningObject.BoldClassTypeInfo.ExpressionName;
    oid := (Element as TBoldMember).OwningObject.BoldObjectLocator.BoldObjectID.AsString;
    MemberName := (Element as TBoldMember).BoldMemberRTInfo.ExpressionName;
  end;
  s := Format('%s%s:%s', [ObjectName, oid, MemberName]);
  for i:= 0 to FEvents.Count - 1 do
  begin
    Found := (Pos(s, fEvents[i]) = 0);
    if Found then Break;
  end;
  Assert(Found, 'VerifyNoEvent failed');
end;

procedure TLoggingSubscriber.VerifySendEvent(const EventType: TBoldEvent;
  const Element: TBoldDomainElement);
var
  ObjectName, oid, MemberName: string;
  s: string;
begin
  if (Element is TBoldObject) then
  begin
    ObjectName := (Element as TBoldObject).BoldClassTypeInfo.ExpressionName;
    oid := (Element as TBoldObject).BoldObjectLocator.BoldObjectID.AsString;
    memberName := '';
  end;
  if (Element is TBoldMember) then
  begin
    ObjectName := (Element as TBoldMember).OwningObject.BoldClassTypeInfo.ExpressionName;
    oid := (Element as TBoldMember).OwningObject.BoldObjectLocator.BoldObjectID.AsString;
    MemberName := (Element as TBoldMember).BoldMemberRTInfo.ExpressionName;
  end;
  s := Format('%s%s:%s:%s', [ObjectName, oid, MemberName, EventTypeToString(EventType)]);
  Assert((fEvents.IndexOf(s) <> -1), Format('VerifySendEvent %s failed', [s]));
end;

procedure VerifyStateInvalidAdjust(Role: TBoldMember);
begin
  VerifyState(Role, bvpsInvalid);
  VerifyAdjustFlag(Role);
end;

procedure VerifyObjectInBlock(Block: TboldUndoBlock; aId: TBoldObjectId; state: TboldExistenceState);
var
  fsObjectContents: TboldFreeStandingObjectContents;
begin
  fsObjectContents := Block.FSValueSpace.GetFSObjectContentsByObjectId(aId);
  Assert(Assigned(fsObjectContents) and (fsObjectContents.BoldExistenceState = state));
end;

procedure VerifyIsInUndoArea(aBlock: TBoldUndoBlock; Member: TBoldMember; memberValue: IBoldValue);
var
  OldValue: IBoldValue;
  res: Boolean;
begin
  aBlock.ValueExists(Member.OwningObject.BoldObjectLocator.BoldObjectID,
    Member.BoldMemberRTInfo.index, OldValue);
  if (Member.OwningObject is TClassWithLink) then
    res :=  Assigned(OldValue) and (Member.OwningObject as TClassWithLink).ValuesAreEqual(OldValue, memberValue, Member.BoldMemberRTInfo.ExpressionName)
  else if (Member.OwningObject is TSomeClass) then
    res :=  Assigned(OldValue) and (Member.OwningObject as TSomeClass).ValuesAreEqual(OldValue, memberValue, Member.BoldMemberRTInfo.ExpressionName)
  else if (Member.OwningObject is TAPersistentClass) then
    res :=  Assigned(OldValue) and (Member.OwningObject as TAPersistentClass).ValuesAreEqual(OldValue, memberValue, Member.BoldMemberRTInfo.ExpressionName)
  else
    raise Exception.Create('Unknown type');
  Assert(res, Format('%s VerifyIsInUndoArea failed', [member.DisplayName]));
end;


end.



