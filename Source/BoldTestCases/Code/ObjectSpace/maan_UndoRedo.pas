unit maan_UndoRedo;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  TestModel1,
  TestSuite,
  BoldDefs,
  BoldSystem,
  BoldDomainElement,
  BoldAttributes,
  BoldId,
  ActnList,
  BoldHandleAction,
  BoldActions,
  BoldDBActions,
  BoldHandle,
  BoldPersistenceHandle,
  BoldPersistenceHandleDB,
  BoldSubscription,
  BoldHandles,
  BoldSystemHandle,
  BoldAbstractModel,
  BoldModel,
  BoldUMLModelLink,
  BoldUMLRose98Link,
  BoldUndoHandler,
  UndoTestModelClasses,
  BoldFreeStandingValues,
  BoldValueInterfaces,
  BoldValueSpaceInterfaces,
  BoldElements,
  maan_UndoRedoTestCaseUtils,
  TestFrameWork, BoldAbstractPersistenceHandleDB,
  DB,
  BoldAbstractDatabaseAdapter, DBAccess, Uni,
  BoldDatabaseAdapterUniDAC
  ;

type

  TdmUndoRedo = class(TDataModule)
    BoldSystemHandle1: TBoldSystemHandle;
    BoldUMLRose98Link1: TBoldUMLRoseLink;
    BoldModel1: TBoldModel;
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
    BoldSystemHandle2: TBoldSystemHandle;
    BoldSystemTypeInfoHandle2: TBoldSystemTypeInfoHandle;
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
    BoldPersistenceHandleDB2: TBoldPersistenceHandleDB;
    BoldDatabaseAdapterUniDAC1: TBoldDatabaseAdapterUniDAC;
    UniConnection1: TUniConnection;
    UniConnection2: TUniConnection;
    BoldDatabaseAdapterUniDAC2: TBoldDatabaseAdapterUniDAC;
  private
    { Private declarations }
  protected
  public
    { Public declarations }
  end;

  Tmaan_UndoRedoAbstractTestCase = class(TBoldTestCase)
  private
    FUndoHandler: TBoldUndoHandler;
    FFSValueSpace: TBoldFreeStandingValueSpace;
    FBookList, FBookList2: TBookList;
    FTopiclist, FTopicList2: TTopicList;
    FAPersistentClassList: TAPersistentClassList;
    FATransientClassList: TATransientClassList;
  protected
    procedure StoreValue(const Member: TBoldMember);
    procedure StoreObject(const Obj: TBoldObject);
    function GetStoredValueOfMember(const Member: TBoldMember): IBoldValue;
    function GetStoredObjectContents(const Obj: TBoldObject): IBoldObjectContents;
    function GetSystem: TBoldSystem;
    function GetSystem2: TBoldSystem;
    function GetUndohandler: TBoldUndoHandler;
    procedure FetchClass(const System: TBoldSystem; const aList: TBoldObjectList; const ObjClass: TBoldObjectClass);
    function IdCompare(Item1, Item2: TBoldElement): Integer;
  public
    FSomeClassList, FSomeClassList2: TSomeClassList;
    FClassWithLinkList, FClassWithLinkList2: TClassWithLinkList;
    FSubscriber: TLoggingSubscriber;
    procedure SetUp; override;
    procedure TearDown; override;
    class function Suite: ITestSuite; override;
    procedure RefreshSystem;
    procedure UpdateDatabase;
    procedure OpenSystem2;
    procedure SaveAndCloseSystem2;
    procedure TestClassListOrder;
    procedure SetSimpleConfiguration;
    procedure SetTransientConfiguration;
    procedure SetConfigurationForIndirectSingle;
    procedure VerifyIsInRedoArea(Member: TBoldmember; Value: TBoldFreeStandingValue);
    property System: TBoldSystem read GetSystem;
    property System2: TBoldSystem read GetSystem2;
    property UndoHandler: TBoldUndoHandler read GetUndoHandler;
  end;

  Tmaan_FetchRefetchTestCase = class(Tmaan_UndoRedoAbstractTestCase)
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
  published
    procedure TestFetchInvalidAttribute;
    procedure TestFetchCurrentAttribute;
    procedure TestFetchModifiedAttribute;
    procedure TestFetchEmbeddedRoleInvalid;
    procedure TestFetchEmbeddedRoleInvalidAdjust;
    procedure TestFetchEmbeddedRoleCurrent;
    procedure TestFetchNonEmbeddedRoleInvalid;
  end;

  Tmaan_ModifyTestCase = class(Tmaan_UndoRedoAbstractTestCase)
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
  published
    procedure TestModifyAttribute;
    procedure TestModifyNonEmbeddedRoleInsertTransient;
    procedure TestModifyNonEmbeddedRoleInsertCurrent;
    procedure TestModifyNonEmbeddedRoleDeleteTransient;
    procedure TestModifyNonEmbeddedRoleDeleteCurrent;
    procedure TestModifyEmbeddedRoleCurrent;
    procedure TestModifyEmbeddedRoleTransient;
    procedure TestModifyEmbeddedRoleModified;
  end;

  {2.7b Undo block}
  {2.2a Undo will change the effective state of the ObjectSpace to be as if the changes contained in
    the block had not been made}
  Tmaan_UndoTestCase = class(Tmaan_UndoRedoAbstractTestCase)
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
  published
    procedure UndoAttribute;                  {2.7b & 2.2a Undo block}
    procedure UndoEmbeddedRoleModified;       {2.7b & 2.2a Undo block}
    procedure UndoEmbeddedRoleTransient;      {2.7b & 2.2a Undo block}
    procedure UndoObjectCreation;             {2.7b & 2.2a Undo block}
//    procedure UndoObjectDeletion;             {2.7b & 2.2a Undo block}
  end;

  procedure CloseAll;
var
  dmUndoRedo: TdmUndoRedo;

implementation

{$R *.DFM}

uses BoldUndoInterfaces;

procedure CloseAll;
begin
  if dmUndoRedo.BoldSystemHandle1.Active then
  begin
    dmUndoRedo.BoldSystemHandle1.UpdateDatabase;
    dmUndoRedo.BoldSystemHandle1.Active := false;
  end;
end;

procedure EnsureDM;
begin
  if not assigned(dmUndoRedo) then
  begin
    Application.Initialize;
    dmUndoRedo := TdmUndoRedo.Create(Application);
    (dmUndoRedo.BoldSystemHandle1.PersistenceHandle as TBoldPersistenceHandleDb).CreateDataBase;
    (dmUndoRedo.BoldSystemHandle1.PersistenceHandle as TBoldPersistenceHandleDb).CreateDataBaseSchema();
    dmUndoRedo.BoldSystemHandle1.Active := True;
  end else
  begin
    if Assigned(dmUndoRedo.BoldSystemHandle1.System) then
      dmUndoRedo.BoldSystemHandle1.System.Discard;
    dmUndoRedo.BoldSystemHandle1.Active := False;
    dmUndoRedo.BoldSystemHandle1.Active := True;
  end;
end;

{ Tmaan_FetchRefetchTestCase }

class procedure Tmaan_FetchRefetchTestCase.Suit(ASuite: TBoldTestSuite);
begin
  ASuite.AddTest(CreateWithComment('TestFetchInvalidAttribute'));
  ASuite.AddTest(CreateWithComment('TestFetchCurrentAttribute'));
  ASuite.AddTest(CreateWithComment('TestFetchModifiedAttribute'));
  ASuite.AddTest(CreateWithComment('TestFetchEmbeddedRoleInvalid'));
  ASuite.AddTest(CreateWithComment('TestFetchEmbeddedRoleInvalidAdjust'));
  ASuite.AddTest(CreateWithComment('TestFetchEmbeddedRoleCurrent'));
  ASuite.AddTest(CreateWithComment('TestFetchNonEmbeddedRoleInvalid'));
end;

procedure Tmaan_FetchRefetchTestCase.TestFetchInvalidAttribute;
var
  SomeObject: TSomeClass;
  OId: TBoldObjectId;
begin
  GenerateObjects(System, 'SomeClass', 1);
  RefreshSystem;
  FSomeClassList.EnsureObjects;
  SomeObject := FSomeClassList[0] as TSomeClass;
  oid := SomeObject.BoldObjectLocator.BoldObjectId.Clone;
  Someobject.M_aString.Invalidate;
  VerifyState(SomeObject.M_aString, bvpsInvalid );
  StoreObject(SomeObject);
  SomeObject := RefetchObject(System, Oid) as TSomeClass;
  VerifySetContents(SomeObject.AsIBoldObjectContents[bdepContents], GetStoredObjectContents(SomeObject));
  VerifyState(SomeObject.M_aString, bvpsCurrent);
end;

procedure Tmaan_FetchRefetchTestCase.TestFetchCurrentAttribute;
var
  SomeObject: TSomeClass;
begin
  GenerateObjects(System, 'SomeClass', 1);
  RefreshSystem;
  FSomeClassList.EnsureObjects;
  SomeObject := FSomeClassList[0];
  VerifyState(SomeObject.M_aString, bvpsCurrent);
  StoreValue(SomeObject.M_aString);
  OpenSystem2;
  FSomeClassList2.EnsureObjects;
  Assert(not FSomeclasslist2.Empty);
  FSomeclasslist2[0].aString := SomeObject.aString + '231';
  System2.UpdateDatabase;
  FSubscriber.SubscribeToElement(SomeObject.M_aString);
  SomeObject.M_aString.Refetch;
  VerifySetContents(SomeObject.AsIBoldObjectContents[bdepContents], GetStoredObjectContents(SomeObject));
  FSubscriber.VerifySendEvent(beValueChanged, SomeObject.M_aString);
  VerifyState(SomeObject.M_aString, bvpsCurrent);
end;

procedure Tmaan_FetchRefetchTestCase.TestFetchModifiedAttribute;
var
  SomeObject: TSomeClass;
  OId: TBoldObjectId;
  value: string;
begin
  GenerateObjects(System, 'SomeClass', 1);
  RefreshSystem;
  FSomeclassList.EnsureObjects;
  SomeObject := FSomeClassList[0];
  oid := SomeObject.BoldObjectLocator.BoldObjectID.Clone;
  SomeObject.aString := SomeObject.aString + '123';
  value := SomeObject.aString;
  VerifyState(SomeObject.M_aString, bvpsModified);
  StoreValue(SomeObject.M_aString);
  SomeObject.M_aString.ReFetch;
  FSubscriber.SubscribeToElement(SomeObject);
  Assert(SomeObject.aString = value, 'TestFetchModifiedAttribute failed');
  VerifyState(SomeObject.M_aString, bvpsModified);
end;

procedure Tmaan_FetchRefetchTestCase.TestFetchEmbeddedRoleInvalid;
var
  ObjA: TSomeClass;
  ObjBLocator, NewObjBLocator: TBoldObjectLocator;
  objB: TSomeClass;
  NewObjB: TSomeClass;
  EmbeddedIndex: integer;
  Locator: TBoldObjectLocator;
  procedure prepare;
  begin
    RefreshSystem;
    ObjA := FSomeClassList[0];
    EmbeddedIndex := ObjA.M_parent.BoldMemberRTInfo.EmbeddedLinkIndex;
    ObjBLocator := FSomeClassList.Locators[1];
  end;
begin
//  TestClassListOrder;
  {ObjB.parent invalid}
  SetSimpleConfiguration;
  {ObjA.child Invalid}
  Prepare;
  VerifyState(ObjA.M_child, bvpsInvalid);
  ObjB := ObjBLocator.EnsuredBoldObject as TSomeClass; //fetch embedded role
  VerifyState(ObjB.M_parent, bvpsCurrent);
  Assert(ObjB.parent = ObjA);

  {ObjA.child Current, and NewObjB not in ObjA.child}
  Prepare;
  NewObjBLocator := FSomeClassList.Locators[2];
  ObjA.Child.EnsureContentsCurrent;
  VerifyState(ObjA.M_child, bvpsCurrent);
  Locator := ObjBLocator.EmbeddedSingleLinks[EmbeddedIndex];
  Assert(Assigned(Locator));
  Assert(Locator.BoldObjectID.AsString = ObjA.BoldObjectLocator.BoldObjectID.AsString);

  OpenSystem2;
  Assert(FSomeClassList.Locators[0].BoldObjectID.AsString = FSomeClassList2.Locators[0].BoldObjectID.AsString);
  Assert(FSomeClassList.Locators[1].BoldObjectID.AsString = FSomeClassList2.Locators[1].BoldObjectID.AsString);
  Assert(FSomeClassList.Locators[2].BoldObjectID.AsString = FSomeClassList2.Locators[2].BoldObjectID.AsString);
  FSomeClassList2[0].child.Add(FSomeClassList2[2]);
  Assert(FSomeClassList2[2].Parent = FSomeClassList2[0]);
  SaveAndCloseSystem2;

  StoreObject(ObjA);
  FSubscriber.SubscribeToElement(ObjA.M_child);
  NewObjB := NewObjBLocator.EnsuredBoldObject as TSomeClass; //fetch embedded role
  VerifyState(NewObjB.M_parent, bvpsCurrent);
  Assert(NewObjB.parent = ObjA);
  Assert(not Assigned(NewObjBLocator.EmbeddedSingleLinks[EmbeddedIndex]));
  VerifyListAdjustedIn(ObjA.child, NewObjB, true);
  FSubscriber.VerifySendEvent(beItemAdded, ObjA.child);

  {ObjA.child Current, and ObjB in ObjA.child}
  Prepare;
  ObjA.Child.EnsureContentsCurrent;
  VerifyState(ObjA.M_child, bvpsCurrent);
  Assert(not Assigned(ObjBLocator.BoldObject));
  Assert(ObjBLocator.EmbeddedSingleLinks[EmbeddedIndex].BoldObjectID.AsString = ObjA.BoldObjectLocator.BoldObjectID.AsString);

  OpenSystem2;
  Assert(FSomeClassList.Locators[2].BoldObjectID.AsString = FSomeClassList2.Locators[2].BoldObjectID.AsString);
  FsomeClassList2[1].parent := nil;  // remove NonEmbeddedRole from ObjA.child
  SaveAndCloseSystem2;

  StoreObject(ObjA);
  FSubscriber.SubscribeToElement(ObjA.M_child);
  ObjB := ObjBLocator.EnsuredBoldObject as TSomeClass; //fetch embedded role
  objB.M_Parent.Refetch;
  VerifyState(ObjB.M_parent, bvpsCurrent);
  Assert(ObjB.parent = nil);
  VerifyListAdjustedEx(ObjA.child, ObjB , true);
  FSubscriber.VerifySendEvent(beItemDeleted, ObjA.child);
end;

procedure Tmaan_FetchRefetchTestCase.TestFetchEmbeddedRoleInvalidAdjust;
var
  ObjA, ObjB: TSomeClass;
  NewObjALocator: TBoldObjectLocator;
  procedure prepare;
  begin
    RefreshSystem;
    ObjA := FSomeClassList[0];
    ObjB := FSomeClassList[1];
    NewObjALocator := FSomeClassList.Locators[2];
    ObjB.M_parent.Invalidate;
    VerifyState(ObjB.M_parent, bvpsInvalid);
    VerifyHasOldValues(ObjB.M_parent, true);
  end;

  procedure ModifyEmbeddedStateInDb;
  begin
    OpenSystem2;
    FSomeClassList2[1].parent := FSomeClassList2[2];
    SaveAndCloseSystem2;
  end;

  procedure UndoModifyEmbeddedStateInDb;
  begin
    //Undo the changes
    OpenSystem2;
    FSomeClassList2[1].parent := FSomeClassList2[0];
    SaveAndCloseSystem2;
  end;

begin
  SetSimpleConfiguration;
  { ObjA.child Invalid, and ObjB.parent not modified after fetch }
  Prepare;
  ObjB.M_parent.EnsureContentsCurrent;
  VerifyState(ObjA.M_child, bvpsInvalid);
  Assert(ObjB.parent = ObjA);
  VerifyState(ObjB.M_parent, bvpsCurrent);
  VerifyState(ObjA.M_child, bvpsInvalid);

  {ObjA.child Invalid, and ObjB.parent modified after fetch }
  Prepare;
  VerifyState(ObjB.M_child, bvpsInvalid);
  VerifyState((NewObjALocator.EnsuredBoldObject as TSomeClass).M_child, bvpsInvalid);

  ModifyEmbeddedStateInDb;
  ObjB.M_parent.Refetch;
  Assert(ObjB.parent = (NewObjALocator.BoldObject as TSomeClass));
  VerifyState(ObjB.M_parent, bvpsCurrent);
  VerifyState(ObjA.M_child, bvpsInvalid);
  VerifyState((NewObjALocator.BoldObject as TSomeClass).M_child, bvpsInvalid);
  UndoModifyEmbeddedStateInDb;

  {ObjA.child Invalid, NewObjA.child current, NewObjB current}
  Prepare;
  (NewObjALocator.EnsuredBoldObject as TSomeClass).child.EnsureContentsCurrent;
  VerifyState(ObjB.M_child, bvpsInvalid);
  VerifyState((NewObjALocator.EnsuredBoldObject as TSomeClass).M_child, bvpsCurrent);

  ModifyEmbeddedStateInDb;
  FSubscriber.SubscribeToElement((NewObjALocator.BoldObject as TSomeClass).M_child);
  ObjB.M_parent.Refetch;
  Assert(ObjB.parent = (NewObjALocator.BoldObject as TSomeClass));
  VerifyState(ObjB.M_parent, bvpsCurrent);
  VerifyState(ObjA.M_child, bvpsInvalid);
  VerifyState((NewObjALocator.BoldObject as TSomeClass).M_child, bvpsCurrent);
  VerifyListAdjustedIn((NewObjALocator.BoldObject as TSomeClass).child, ObjB, true);
  FSubscriber.VerifySendEvent(beItemAdded, (NewObjALocator.BoldObject as TSomeClass).M_child);
  UndoModifyEmbeddedStateInDb;

  {ObjA.child Current, Old(ObjA = New(ObjB) }
  Prepare;
  ObjA.child.EnsureContentsCurrent;
  VerifyState(ObjA.M_child, bvpsCurrent);

  StoreValue(ObjB.M_parent);
  StoreValue(ObjA.M_child);
  ObjB.M_parent.EnsureCOntentsCurrent;
  VerifySetContents(ObjB.M_parent.AsIBoldValue[bdepContents], GetStoredValueOfMember(ObjB.M_parent));
  VerifyState(ObjB.M_parent, bvpsCurrent);

  VerifyState(ObjA.M_child, bvpsCurrent);
  Assert(ObjA.M_child.Count = 1, 'TestFetchEmbeddedRoleInvalidAdjust failed');
  Assert((GetStoredValueOfMember(ObjA.M_child) as IBoldObjectIdListRef).Count = 1, 'TestFetchEmbeddedRoleInvalidAdjust failed');
  Assert(ObjA.M_child[0].BoldObjectLocator.BoldObjectId.AsString = (GetStoredValueOfMember(ObjA.M_child) as IBoldObjectIdListRef).IdList[0].ASString, 'TestFetchEmbeddedRoleInvalidAdjust failed');

  {ObjA.child Current, NewObjB current, and ObjA <> NewObjA}
  Prepare;
  (NewObjALocator.EnsuredBoldObject as TSomeClass).child.EnsureContentsCurrent;
  ObjA.child.EnsureContentsCurrent;
  VerifyState(ObjA.M_child, bvpsCurrent);
  VerifyState((NewObjALocator.BoldObject as TSomeClass).M_child, bvpsCurrent);

  ModifyEmbeddedStateInDb;
  StoreValue(ObjB.M_parent);
  StoreValue((NewObjALocator.BoldObject as TSomeClass).M_child);
  StoreValue(ObjA.M_child);
  FSubscriber.SubscribeToElement((NewObjALocator.BoldObject as TSomeClass).M_child);
  FSubscriber.SubscribeToElement(ObjA.M_child);
  ObjB.M_parent.Refetch;
  VerifySetContents(ObjB.M_parent.AsIBoldValue[bdepContents], GetStoredValueOfMember(ObjB.M_parent));
  VerifyState(ObjB.M_parent, bvpsCurrent);
  VerifyState(ObjA.M_child, bvpsCurrent);
  FSubscriber.VerifySendEvent(beItemDeleted, ObjA.M_child);
  VerifyListAdjustedEx(ObjA.child, ObjB, true);
  VerifyState((NewObjALocator.BoldObject as TSomeClass).M_child, bvpsCurrent);
  FSubscriber.VerifySendEvent(beItemAdded, (NewObjALocator.BoldObject as TSomeClass).M_child);
  VerifyListAdjustedIn((NewObjALocator.BoldObject as TSomeClass).child, ObjB, true);
  UndoModifyEmbeddedStateInDb;
end;

procedure Tmaan_FetchRefetchTestCase.TestFetchEmbeddedRoleCurrent;
var
  ObjA, ObjB: TSomeClass;
  msg: string;
  procedure prepare;
  begin
    RefreshSystem;
    ObjA := FSomeClassList[0];
    ObjB := FSomeClassList[1];
    VerifyState(ObjB.M_parent, bvpsCurrent);
  end;

  procedure ModifyEmbeddedStateInDb;
  begin
    OpenSystem2;
    FSomeClassList2[1].parent := FSomeClassList2[2];
    SaveAndCloseSystem2;
  end;

  procedure UndoModifyEmbeddedStateInDb;
  begin
    OpenSystem2;
    FSomeClassList2[1].parent := FSomeClassList2[0];
    SaveAndCloseSystem2;
  end;

  procedure FetchEmbeddedRole;
  begin
    ObjB.BoldObjectLocator.EnsureBoldObject;
  end;

begin
  SetSimpleConfiguration;
  {ObjB Invalid, and Old(ObjB = New(ObjB) }
  Prepare;
  VerifyState(ObjA.M_child, bvpsInvalid);
  ModifyEmbeddedStateInDb;
  StoreValue(ObjB.M_parent);
  FSubscriber.SubscribeToElement(ObjB.M_parent);
  ObjB.M_parent.Refetch;
  msg := 'Member is not fetch unless its state is invalid';
  Assert(ObjB.parent = FSomeClassList[2], msg);
  VerifyState(ObjB.M_parent, bvpsCurrent);
  FSubscriber.VerifySendEvent(beValueChanged, ObjB.M_parent);
  VerifyState(ObjA.M_child, bvpsInvalid);
  Exit;
end;

procedure Tmaan_FetchRefetchTestCase.TestFetchNonEmbeddedRoleInvalid;
var
  ObjA, ObjB, ObjA2, ObjB2: TSomeClass;
  ObjBLocator: TBoldObjectLocator;
  EmbeddedIndex: integer;

  procedure prepare;
  begin
    if not dmUndoRedo.BoldSystemHandle1.Active then
      dmUndoRedo.BoldSystemHandle1.Active := true;
    FSomeClassList[0].child.Clear;
    FSomeClassList[1].child.Clear;
    FSomeClassList[2].child.Clear;
    dmUndoRedo.BoldSystemHandle1.UpdateDatabase;
    assert(fSomeClasslist[1].parent = nil);
    FSomeClassList[1].parent := FSomeClassList[0];
    assert(fSomeClasslist[3].parent = nil);
    FSomeClassList[3].parent := FSomeClassList[2];
    dmUndoRedo.BoldSystemHandle1.UpdateDatabase;
    RefreshSystem;
    ObjA := FSomeClassList[0];
    ObjBLocator := FSomeClassList.locators[1];
    ObjA2 := FSomeClassList[2];
    ObjB2 := FSomeClassList[3];
    VerifyState(ObjA.M_child, bvpsInvalid);
    EmbeddedIndex := ObjA.M_parent.BoldMemberRTInfo.EmbeddedLinkIndex ;
  end;

  procedure ModifyEmbeddedStateInDb;
  begin
    OpenSystem2;
    FSomeClassList2[3].parent := FSomeClassList2[0];
    SaveAndCloseSystem2;
  end;

begin
  SetSimpleConfiguration;
  {ObjB.parent invalid}
  Prepare;
  StoreValue(ObjA.M_child);
  ObjA.child.EnsureContentsCurrent;
  VerifySetContents(ObjA.M_child.AsIBoldValue[bdepContents], GetStoredValueOfMember(ObjA.M_child));
  VerifyState(ObjA.M_child, bvpsCurrent);
  Assert(Assigned(ObjBLocator.EmbeddedSingleLinks[EmbeddedIndex]));
  Assert(ObjBLocator.EmbeddedSingleLinks[EmbeddedIndex].BoldObjectID.AsString = ObjA.BoldObjectLocator.BoldObjectID.AsString);

  {ObjB.parent Invalid/Adjust}
  {after fetch ObjB not included in ObjA.child}
  Prepare;
  ObjB := (ObjBLocator.EnsuredBoldObject as TSomeClass);
  assert(objB.Parent <> nil);
  ObjB.M_parent.Invalidate;
  StoreValue(ObjA.M_child);
  VerifyState(ObjB.M_parent, bvpsInvalid);
  VerifyHasOldValues(ObjB.M_parent);

  //change in DB
  OpenSystem2;
  FSomeClassList2.EnsureObjects;

  Assert(FSomeClassList2[1].BoldObjectLocator.BoldObjectId.AsString = ObjB.BoldObjectLocator.BoldObjectId.AsString);
  Assert(FSomeClassList2[1].parent.BoldObjectLocator.BoldObjectId.AsString = ObjA.BoldObjectLocator.BoldObjectId.AsString);
  FSomeClassList2[1].parent := nil;
  System2.UpdateDatabase;
  SaveAndCloseSystem2;

  ObjA.child.EnsureContentsCurrent;
  VerifySetContents(ObjA.M_child.AsIBoldValue[bdepContents], GetStoredValueOfMember(ObjA.M_child));
  VerifyState(ObjA.M_child, bvpsCurrent);
  VerifyState(ObjB.M_parent, bvpsInvalid);
  VerifyHasOldValues(ObjB.M_parent, false);


  {ObjB.parent Invalid/Adjust}
  {after fetch ObjB.parent not modified}
  Prepare;
  ObjB := (ObjBLocator.EnsuredBoldObject as TSomeClass);
  ObjB.M_parent.Invalidate;
  VerifyState(ObjB.M_parent, bvpsInvalid);
  VerifyHasOldValues(ObjB.M_parent);
  StoreValue(ObjA.M_child);
  ObjA.child.EnsureContentsCurrent;
  VerifySetContents(ObjA.M_child.AsIBoldValue[bdepContents], GetStoredValueOfMember(ObjA.M_child));
  VerifyState(ObjA.M_child, bvpsCurrent);
  VerifyState(ObjB.M_parent, bvpsInvalid); // Fetching multi end should not effect invalid single end

  {ObjB2.parent Invalid/Adjust}
  {after fetch ObjB2 included in ObjA.child}
  Prepare;
  VerifyState(ObjA2.M_child,bvpsInvalid);
  VerifyState(ObjB2.M_parent, bvpsCurrent);
  ObjA2.M_child.EnsureContentsCurrent;  //fetch
  VerifyState(ObjA2.M_child, bvpsCurrent);
  VerifyState(ObjB2.M_parent, bvpsCurrent);
  Assert(ObjB2.parent = ObjA2);
  ObjB2.Invalidate;
  VerifyState(ObjB2.M_parent, bvpsInvalid);
  VerifyHasOldValues(ObjB2.M_parent);
  VerifyState(ObjA.M_child, bvpsInvalid);
  FSubscriber.SubscribeToElement(ObjA2.M_child);

  //change in DB
  OpenSystem2;
  FSomeClasslist2.EnsureObjects;
  FSomeClassList2[3].parent := FSomeClassList2[0];
  System2.UpdateDatabase;
  SaveAndCloseSystem2;

  VerifyState(ObjA.M_child, bvpsInvalid);
  VerifyState(ObjB2.M_parent, bvpsInvalid);
  VerifyHasOldValues(ObjB2.M_parent);
//  Assert((ObjB2.M_parent as TBoldObjectReference).Locator = ObjA2.BoldObjectLocator);
  Assert((ObjB2.M_parent.asIBoldValue[bdepContents] as IBoldObjectIdRef).id.AsString = ObjA2.BoldObjectLocator.BoldObjectId.asstring);
  VerifyState(ObjA2.M_child, bvpsCurrent);
  Assert(ObjA2.M_child.indexOf(ObjB2) <> -1);
  ObjA.child.EnsureContentsCurrent;

  Assert(ObjA.child.count = 2);
  VerifyState(ObjA.M_child, bvpsCurrent);
  VerifyState(ObjB2.M_parent, bvpsInvalid);
  Assert(ObjB2.parent = ObjA);
  VerifyListAdjustedEx(ObjA2.M_child, ObjB2, true);
  FSubscriber.VerifySendEvent(beItemDeleted, ObjA2.M_child);

  {ObjB.parent Current}
  {after fetch ObjB not included in ObjA.child}
  Prepare;
  ObjB := (ObjBLocator.EnsuredBoldObject as TSomeClass);
  VerifyState(ObjB.M_parent, bvpsCurrent);
  StoreValue(ObjA.M_child);
  //change in DB
  OpenSystem2;
  FsomeClassList2.EnsureObjects;
  FSomeClassList2[1].parent := nil;
  System2.UpdateDatabase;
  SaveAndCloseSystem2;

  ObjA.child.EnsureContentsCurrent;
  VerifySetContents(ObjA.M_child.AsIBoldValue[bdepContents], GetStoredValueOfMember(ObjA.M_child));
  VerifyState(ObjA.M_child, bvpsCurrent);
  VerifyState(ObjB.M_parent, bvpsInvalid);

  {ObjB.parent Current}
  {after fetch ObjB not modified}
  Prepare;
  ObjB := (ObjBLocator.EnsuredBoldObject as TSomeClass);
  VerifyState(ObjB.M_parent, bvpsCurrent);
  StoreValue(ObjA.M_child);
  ObjA.child.EnsureContentsCurrent;
  VerifySetContents(ObjA.M_child.AsIBoldValue[bdepContents], GetStoredValueOfMember(ObjA.M_child));
  VerifyState(ObjA.M_child, bvpsCurrent);
  VerifyState(ObjB.M_parent, bvpsCurrent);

  {ObjB.parent Current}
  {after fetch ObjB2 included in ObjA.child}
  Prepare;
  ObjA2.M_child.EnsureContentsCurrent;
  VerifyState(ObjA2.M_child, bvpsCurrent);
  VerifyState(ObjB2.M_parent, bvpsCurrent);
  StoreValue(ObjB2.M_parent);
  StoreValue(ObjA.M_child);
  FSubscriber.SubscribeToElement(ObjA2.M_child);
  FSubscriber.SubscribeToElement(ObjB2.M_parent);
  //change in DB
  OpenSystem2;
  FSomeClassList2.EnsureObjects;
  Assert(fSomeClassList2[0].BoldObjectLocator.BoldObjectId.asstring = ObjA.BoldObjectLocator.BoldObjectId.asstring);
  Assert(fSomeClassList2[3].BoldObjectLocator.BoldObjectId.asstring = ObjB2.BoldObjectLocator.BoldObjectId.asstring);
  FSomeClassList2[3].parent := FSomeClassList2[0];
  System2.UpdateDatabase;
  SaveAndCloseSystem2;
  ObjA.child.ensureContentsCurrent;
  VerifySetContents(ObjA.M_child.AsIBoldValue[bdepContents], GetStoredValueOfMember(ObjA.M_child));
  VerifyState(ObjA.M_child, bvpsCurrent);
  VerifySetContents(ObjB2.M_parent.AsIBoldValue[bdepContents], GetStoredValueOfMember(ObjB2.M_parent));
  VerifyState(ObjB2.M_parent, bvpsCurrent);
  Assert(ObjB2.parent = ObjA);
  FSubscriber.VerifySendEvent(beValueChanged, ObjB2.M_parent);
  VerifyListAdjustedEx(ObjA2.M_child, ObjB2, true);
  FSubscriber.VerifySendEvent(beItemDeleted, ObjA2.M_child);

end;

class function Tmaan_FetchRefetchTestCase.Suite: ITestSuite;
begin
  Result := inherited Suite;
  SetCommentForTest(Result, 'TestFetchInvalidAttribute', '');
  SetCommentForTest(Result, 'TestFetchCurrentAttribute', '');
  SetCommentForTest(Result, 'TestFetchModifiedAttribute', '');
  SetCommentForTest(Result, 'TestFetchEmbeddedRoleInvalid', '');
  SetCommentForTest(Result, 'TestFetchEmbeddedRoleInvalidAdjust', '');
  SetCommentForTest(Result, 'TestFetchEmbeddedRoleCurrent', '');
  SetCommentForTest(Result, 'TestFetchNonEmbeddedRoleInvalid', '');
end;

{ Tmaan_UndoRedoAbstractTestCase }

function Tmaan_UndoRedoAbstractTestCase.IdCompare(Item1, Item2: TBoldElement): Integer;
var
  i1,i2: integer;
begin
  i1 := StrToInt(TBoldObject(Item1).BoldObjectLocator.AsString);
  i2 := StrToInt(TBoldObject(Item2).BoldObjectLocator.AsString);

  if  i1 = i2 then
    result := 0
  else
    if i1 < i2 then
      result := -1
    else
      result := 1;
end;

procedure Tmaan_UndoRedoAbstractTestCase.FetchClass(const System: TBoldSystem;
  const aList: TBoldObjectList; const ObjClass: TBoldObjectClass);
begin
  maan_UndoRedoTestCaseUtils.FetchClass(System, aList, ObjClass);
  aList.Sort(IdCompare);
end;

function Tmaan_UndoRedoAbstractTestCase.GetStoredObjectContents(
  const Obj: TBoldObject): IBoldObjectContents;
var
  oc: TBoldFreeStandingObjectContents;
begin
  oc := FFSValueSpace.GetFSObjectContentsByObjectId(obj.BoldObjectLocator.BoldObjectID);
  Result := oc as IBoldObjectContents;
end;

function Tmaan_UndoRedoAbstractTestCase.GetStoredValueOfMember(
  const Member: TBoldMember): IBoldValue;
var
  oc: TBoldFreeStandingObjectContents;
begin
  oc := FFSValueSpace.GetFSObjectContentsByObjectId(Member.OwningObject.BoldObjectLocator.BoldObjectID);
  Result := oc.ValueByIndex[Member.BoldMemberRTInfo.index];
end;

function Tmaan_UndoRedoAbstractTestCase.GetSystem: TBoldSystem;
begin
  if Assigned(dmUndoRedo) then
    Result := dmUndoRedo.BoldSystemHandle1.System
  else
    Result := nil;
end;

function Tmaan_UndoRedoAbstractTestCase.GetSystem2: TBoldSystem;
begin
  Result := dmUndoRedo.BoldSystemHandle2.System;
end;

function Tmaan_UndoRedoAbstractTestCase.GetUndohandler: TBoldUndoHandler;
begin
  Result := (System.UndoHandler as TBoldUndoHandler);
end;

procedure Tmaan_UndoRedoAbstractTestCase.OpenSystem2;
begin
  dmUndoRedo.UniConnection2.Database := dmUndoRedo.UniConnection1.Database;
  dmUndoRedo.BoldSystemHandle2.Active := true;
  FetchClass(System2, FSomeClassList2, TSomeClass);
  FetchClass(System2, FTopicList2, TTopic);
  FetchClass(System2, FBookList2, TBook);
  FetchClass(System2, FClassWithLinkList2, TClassWithLink);
end;

procedure Tmaan_UndoRedoAbstractTestCase.RefreshSystem;
begin
  UpdateDatabase;
  FSubscriber.Refresh;
  dmUndoRedo.BoldSystemHandle1.Active := false;
  FSomeClassList.Clear;
  FBookList.clear;
  fTopicList.Clear;
  FAPersistentClassList.Clear;
  FATransientClassList.Clear;
  FClassWithLinkList.Clear;
  dmUndoRedo.BoldSystemHandle1.Active := true;
  FetchClass(System, FSomeClassList, TSomeClass);
  FetchClass(System, FBookList, TBook);
  FetchClass(System, FTopicList, TTopic);
  FetchClass(System, FClassWithLinkList, TClassWithLink);
end;

procedure Tmaan_UndoRedoAbstractTestCase.SaveAndCloseSystem2;
begin
  if (dmUndoRedo.BoldSystemhandle2.Active) then
  begin
    dmUndoRedo.BoldSystemHandle2.UpdateDatabase;
    dmUndoRedo.BoldSystemHandle2.Active := false;
    FSomeClassList2.Clear;
    FBookList2.clear;
    fTopicList2.Clear;
    FClassWithLinkList2.Clear;
  end;
end;

procedure Tmaan_UndoRedoAbstractTestCase.SetConfigurationForIndirectSingle;
begin
  GenerateObjects(System, 'ClassWithLink', 4);
  UpdateDatabase;
  FetchClass(System, FClassWithLinkList, TClassWithLink);
  FClassWithLinkList[1].one := FClassWithLinkList[0];
  FClassWithLinkList[3].one := FClassWithLinkList[2];
end;

procedure Tmaan_UndoRedoAbstractTestCase.SetSimpleConfiguration;
begin
  GenerateObjects(System, 'SomeClass', 4);
  UpdateDatabase;
  FetchClass(System, FSomeClassList, TSomeClass);
  FSomeClassList[1].parent := FSomeClassList[0];
  FSomeClassList[3].parent := FSomeClassList[2];
end;

procedure Tmaan_UndoRedoAbstractTestCase.SetTransientConfiguration;
begin
  RefreshSystem;
  GenerateObjects(System, 'APersistentClass', 2);
  RefreshSystem;
  FetchEnsuredClass(System, FAPersistentClassList, TAPersistentClass);
  CreateATransientClass(System, nil);
  CreateATransientClass(System, nil);
  FetchClass(System, FATransientClasslist, TATransientClass);
  FATransientClassList[0].many.Add(FAPersistentClassList[0]);
  FATransientClassList[1].many.Add(FAPersistentClassList[1]);
end;

procedure Tmaan_UndoRedoAbstractTestCase.SetUp;
begin
  EnsureDM;
  FUndoHandler := (dmUndoRedo.BoldSystemHandle1.System.UndoHandler as TBoldUndoHandler);
  FSubscriber := TLoggingSubscriber.Create;
  FFSValueSpace := TBoldFreeStandingValueSpace.Create;
  FSomeClassList := TSomeClassList.Create;
  FSomeClassList2 := TSomeClassList.Create;
  FBookList := TBookList.Create;
  FBookList2 := TBookList.Create;
  FTopicList := TTopicList.Create;
  FTopicList2 := TTopicList.Create;
  FAPersistentClassList := TAPersistentClassList.Create;
  FATransientClassList := TATransientClassList.Create;
  FClassWithLinkList := TClassWithLinkList.Create;
  FClassWithLinkList2 := TClassWithLinkList.Create;
end;

procedure Tmaan_UndoRedoAbstractTestCase.StoreObject(
  const Obj: TBoldObject);
var
  oc: TBoldFreeStandingObjectContents;
begin
  (FFSValueSpace as IBoldValueSpace).EnsureObjectContents(Obj.BoldObjectLocator.BoldObjectID);
  oc := FFSValueSpace.GetFSObjectContentsByObjectId(Obj.BoldObjectLocator.BoldObjectID);
  oc.ApplyObjectContents(Obj.AsIBoldObjectContents[bdepContents], true, false);
end;

procedure Tmaan_UndoRedoAbstractTestCase.StoreValue(
  const Member: TBoldMember);
var
  oc: TBoldFreeStandingObjectContents;
  MemberId: TBoldMemberId;
begin
  (FFSValueSpace as IBoldValueSpace).EnsureObjectContents(Member.OwningObject.BoldObjectLocator.BoldObjectID);
  oc := FFSValueSpace.GetFSObjectContentsByObjectId(Member.OwningObject.BoldObjectLocator.BoldObjectID);
  oc.ApplyObjectContents(Member.OwningObject.AsIBoldObjectContents[bdepContents], false, false);
  try
    MemberId := TBoldMemberID.Create(Member.BoldMemberRTInfo.index);
    oc.EnsureMember(MemberId, member.AsIBoldValue[bdepContents].ContentName);
    oc.ValueByIndex[MemberId.MemberIndex].AssignContent(Member.AsIBoldValue[bdepContents]);
  finally
    FreeAndNil(MemberId);
  end;
end;

class function Tmaan_UndoRedoAbstractTestCase.Suite: ITestSuite;
begin
  Result := inherited Suite;
end;

procedure Tmaan_UndoRedoAbstractTestCase.TearDown;
begin
  SaveAndCloseSystem2;
  if dmUndoRedo.BoldSystemHandle1.Active then
  begin
    dmUndoRedo.BoldSystemhandle1.UpdateDAtabase;
    dmUndoRedo.BoldSystemHandle1.Active := false;
  end;
  FreeAndNil(dmUndoRedo);
  FreeAndNil(FSubscriber);
  FreeAndNil(FFSValueSpace);
  FreeAndNil(FSomeClassList);
  FreeAndNil(FSomeClassList2);
  FreeAndNil(FBookList);
  FreeAndNil(FBookList2);
  FreeAndNil(FTopicList);
  FreeAndNil(FTopicList2);
  FreeAndNil(FAPersistentClassList);
  FreeAndNil(FATransientClassList);
  FreeAndNil(FClassWithLinkList);
  FreeAndNil(FClassWithLinkList2);
end;

procedure Tmaan_UndoRedoAbstractTestCase.TestClassListOrder;
var
  i: Integer;
begin
  GenerateObjects(System, 'SomeClass', 4);
  FetchClass(System, FSomeClassList, TSomeClass);
  for i := 0 to 4 - 1 do
  begin
    Assert(FSomeClassList[i].BoldObjectLocator.AsString = IntToStr(i), FSomeClassList[i].BoldObjectLocator.AsString + ' <> ' + IntToStr(i));
    FSomeClassList[i].aString := 'SomeClass' + IntToStr(i);
  end;
  UpdateDatabase;
  RefreshSystem;
  FetchClass(System, FSomeClassList, TSomeClass);
  Assert(FSomeClassList[0].aString = 'SomeClass0');
  Assert(FSomeClassList[1].aString = 'SomeClass1');
  Assert(FSomeClassList[2].aString = 'SomeClass2');
  Assert(FSomeClassList[3].aString = 'SomeClass3');
end;

procedure Tmaan_UndoRedoAbstractTestCase.UpdateDatabase;
begin
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;
end;

procedure Tmaan_UndoRedoAbstractTestCase.VerifyIsInRedoArea(
  Member: TBoldmember; Value: TBoldFreeStandingValue);
var
  ValueInBlock: IBoldValue;
  res: Boolean;
begin
  ValueInBlock := nil;
  res := false;
  Assert(UndoHandler.RedoBlocks.CurrentBlock.ValueExists(Member.OwningObject.BoldObjectLocator.BoldObjectID,
    Member.BoldMemberRTInfo.index, ValueInBlock));
  if Member.OwningObject is TSomeClass then
    res := (Member.OwningObject as TSomeClass).ValuesAreEqual(Value, ValueInBlock, Member.BoldMemberRTInfo.ExpressionName)
  else if Member.OwningObject is TAPersistentClass then
    res := (Member.OwningObject as TAPersistentClass).ValuesAreEqual(Value, ValueInBlock, Member.BoldMemberRTInfo.ExpressionName)
  else if Member.OwningObject is TATransientClass then
    res := (Member.OwningObject as TATransientClass).ValuesAreEqual(Value, ValueInBlock, Member.BoldMemberRTInfo.ExpressionName)
  else if (Member.OwningObject is TClassWithLink) then
    res := (Member.OwningObject as TClassWithLink).ValuesAreEqual(Value, ValueInBlock, Member.BoldMemberRTInfo.ExpressionName)
  ;
  Assert(res, Format('%s VerifyIsInRedoArea failed', [member.DisplayName]));
  Value := nil;
end;

{ Tmaan_ModifyTestCase }

class procedure Tmaan_ModifyTestCase.Suit(ASuite: TBoldTestSuite);
begin
  ASuite.AddTest(CreateWithComment('TestModifyAttribute', '2.7b Undo block'));
  ASuite.AddTest(CreateWithComment('TestModifyNonEmbeddedRoleInsertTransient', '2.7b Undo block'));
  ASuite.AddTest(CreateWithComment('TestModifyNonEmbeddedRoleInsertCurrent', '2.7b Undo block'));
  ASuite.AddTest(CreateWithComment('TestModifyNonEmbeddedRoleDeleteTransient', '2.7b Undo block'));
  ASuite.AddTest(CreateWithComment('TestModifyNonEmbeddedRoleDeleteCurrent', '2.7b Undo block'));
  ASuite.AddTest(CreateWithComment('TestModifyEmbeddedRoleCurrent', '2.7b Undo block'));
  ASuite.AddTest(CreateWithComment('TestModifyEmbeddedRoleTransient', '2.7b Undo block'));
  ASuite.AddTest(CreateWithComment('TestModifyEmbeddedRoleModified', '2.7b Undo block'));
end;

procedure Tmaan_ModifyTestCase.TestModifyAttribute;
var
  NewObject: TSomeClass;
  oc: TBoldFreeStandingObjectContents;
  oid: TBoldObjectId;
  procedure Prepare;
  begin
    RefreshSystem;
  end;
begin
  {Persistent}
  {Create Object}
  Prepare;
  NewObject := CreateSomeClass(system, FSubscriber, true);
  oc := UndoHandler.Undoblocks.CurrentBlock.FSValueSpace.GetFSObjectContentsByObjectId(NewObject.BoldObjectLocator.BoldObjectID);
  Assert(Assigned(oc));
  Assert(oc.BoldExistenceState in [besNotCreated]);

  {Delete Object}
  Prepare;
  NewObject := FSomeClassList[0];
  NewObject.Delete;
  oc := UndoHandler.Undoblocks.CurrentBlock.FSValueSpace.GetFSObjectContentsByObjectId(NewObject.BoldobjectLocator.BoldObjectId);
  Assert(Assigned(oc));
  Assert(oc.BoldExistenceState in [besExisting]);

  {Current attribute}
  GenerateObjects(System, 'SomeClass', 1);
  Prepare;
  Newobject := FSomeClassList[0];
  VerifyState(NewObject.M_aString, bvpsCurrent);
  FSubscriber.SubscribeToElement(NewObject.M_aString);
  StoreValue(NewObject.M_aString);
  NewObject.aString := NewObject.aString + '123';
  VerifyIsInUndoArea(UndoHandler.UndoBlocks.CurrentBlock, NewObject.M_aString, GetStoredValueOfMember(newObject.M_aString));
  FSubscriber.VerifySendEvent(beValueChanged, NewObject.M_aString);
  VerifyState(NewObject.M_aString, bvpsModified);

  {Modified attribute}
  Prepare;
  Newobject := FSomeClassList[0];
  NewObject.aString := NewObject.aString + '123';
  VerifyState(NewObject.M_aString, bvpsModified);
  FSubscriber.SubscribeToElement(NewObject.M_aString);
  StoreValue(NewObject.M_aString);
  NewObject.aString := NewObject.aString + '456';
  FSubscriber.VerifySendEvent(beValueChanged, NewObject.M_aString);
  VerifyState(NewObject.M_aString, bvpsModified);


  {Transient}
  {Create Object}
  Prepare;
  NewObject := CreateSomeClass(system, FSubscriber, false);
  Assert(NewObject.BoldExistenceState in [besExisting]);
  oc := UndoHandler.Undoblocks.CurrentBlock.FSValueSpace.GetFSObjectContentsByObjectId(NewObject.BoldObjectLocator.BoldObjectID);
  Assert(Assigned(oc));
  Assert(oc.BoldExistenceState in [besNotCreated]);

  {Delete Object}
  Assert(NewObject.BoldExistenceState in [besExisting]);
  oid := NewObject.BoldObjectLocator.BoldObjectID.Clone;
  UndoHandler.SetCheckPoint('Delete NewObject');
  NewObject.Delete;
  oc := UndoHandler.Undoblocks.CurrentBlock.FSValueSpace.GetFSObjectContentsByObjectId(oid);
  Assert(Assigned(oc));
  Assert(oc.BoldExistenceState in [besExisting]);

  {transient attribute}
  Prepare;
  NewObject := CreateSomeClass(System, FSubscriber, false);
  VerifyState(NewObject.M_aString,  bvpsTransient);
  FSubscriber.SubscribeToElement(NewObject.M_aString);
  StoreValue(NewObject.M_aString);
  UndoHandler.SetCheckPoint('Block1');
  NewObject.aString := NewObject.aString + '123';
  VerifyIsInUndoArea(UndoHandler.UndoBlocks.CurrentBlock, NewObject.M_aString, GetStoredValueOfMember(NewObject.M_aString));
  FSubscriber.VerifySendEvent(beValueChanged, NewObject.M_aString);
  VerifyState(NewObject.M_aString, bvpsTransient);

  CloseAll;
end;

procedure Tmaan_ModifyTestCase.TestModifyEmbeddedRoleCurrent;
var
  ObjA, ObjB, ObjA2: TSomeClass;
  procedure Prepare;
  begin
    RefreshSystem;
    ObjA := FSomeClassList[0];
    ObjB := FSomeClasslist[1];
    ObjA2 := FSomeClassList[2];
    ObjB.M_parent.EnsureContentsCurrent;
    VerifyState(ObjB.M_parent, bvpsCurrent);
  end;

begin
  SetSimpleConfiguration;

  {ObjA.child Invalid}
  Prepare;
  ObjA.child.EnsureContentsCurrent;
  VerifyState(ObjA.M_child, bvpsCurrent);
  VerifyState(ObjA2.M_child, bvpsInvalid);
  StoreValue(ObjB.M_parent);
  FSubscriber.SubscribeToElement(ObjB.M_parent);
  FSubscriber.SubscribeToElement(ObjA.M_child);
  Assert(ObjB.parent = ObjA);
  ObjB.parent := ObjA2; // modify
  VerifyState(ObjB.M_parent, bvpsModified);
  Assert(ObjB.parent = ObjA2);
  VerifyState(ObjA2, bvpsCurrent);
  FSubscriber.VerifySendEvent(beValueChanged, ObjB.M_parent);
  VerifyIsInUndoArea(UndoHandler.UndoBlocks.CurrentBlock, ObjB.M_parent, GetStoredValueOfMember(ObjB.M_parent));
  VerifyState(ObjA.M_child, bvpsCurrent);
  VerifyState(ObjA2.M_child, bvpsInvalid);
  VerifyListAdjustedEx(ObjA.child, ObjB, false);
  FSubscriber.VerifySendEvent(beItemDeleted, ObjA.M_child);
  ObjB.Parent := ObjA;

  {ObjA.child Current}
  Prepare;
  ObjA.child.EnsureContentsCurrent;
  ObjA2.child.EnsureContentsCurrent;
  VerifyState(ObjA.M_child, bvpsCurrent);
  VerifyState(ObjA2.M_child, bvpsCurrent);
  StoreValue(ObjB.M_parent);
  FSubscriber.SubscribeToElement(ObjB.M_parent);
  FSubscriber.SubscribeToElement(ObjA.M_child);
  FSubscriber.SubscribeToElement(ObjA2.M_child);
  Assert(ObjB.parent = ObjA);
  ObjB.parent := ObjA2; // modify
  VerifyState(ObjB.M_parent, bvpsModified);
  Assert(objB.parent = ObjA2);
  FSubscriber.VerifySendEvent(beValueChanged, ObjB.M_parent);
  VerifyIsInUndoArea(UndoHandler.UndoBlocks.CurrentBlock, ObjB.M_parent, GetStoredValueOfMember(ObjB.M_parent));
  VerifyState(ObjA.M_child, bvpsCurrent);
  VerifyListAdjustedEx(ObjA.child, ObjB, false);
  FSubscriber.VerifySendEvent(beItemDeleted, ObjA.M_child);
  VerifyState(ObjA2.M_child, bvpsCurrent);
  VerifyListAdjustedIn(ObjA2.child, ObjB, false);
  FSubscriber.VerifySendEvent(beItemAdded, ObjA2.M_child);
  ObjB.Parent := ObjA;

  CloseAll;
end;

procedure Tmaan_ModifyTestCase.TestModifyEmbeddedRoleModified;
var
  ObjA, ObjB, ObjA2: TSomeClass;
  procedure Prepare;
  begin
    RefreshSystem;
    ObjA := FSomeClassList[0];
    ObjB := FSomeClasslist[1];
    ObjA2 := FSomeClassList[2];
    ObjB.M_parent.EnsureContentsCurrent;
    ObjB.parent := nil;
    ObjB.parent := ObjA;
    VerifyState(ObjB.M_parent, bvpsModified);
    UndoHandler.SetCheckPoint('ModifyEmbeddedModified');
  end;

begin
  SetSimpleConfiguration;

  {ObjA.child Invalid}
  Prepare;
  ObjA.child.EnsureContentsCurrent;
  VerifyState(ObjA.M_child, bvpsCurrent);
  VerifyState(ObjA2.M_child, bvpsInvalid);
  StoreValue(ObjB.M_parent);
  FSubscriber.SubscribeToElement(ObjB.M_parent);
  FSubscriber.SubscribeToElement(ObjA.M_child);
  Assert(ObjB.parent = ObjA);
  ObjB.parent := ObjA2; // modify
  VerifyState(ObjB.M_parent, bvpsModified);
  Assert(ObjB.parent = ObjA2);
  VerifyState(ObjA2, bvpsCurrent);
  FSubscriber.VerifySendEvent(beValueChanged, ObjB.M_parent);
  VerifyIsInUndoArea(UndoHandler.UndoBlocks.CurrentBlock, ObjB.M_parent, GetStoredValueOfMember(ObjB.M_parent));
  VerifyState(ObjA.M_child, bvpsCurrent);
  VerifyState(ObjA2.M_child, bvpsInvalid);
  VerifyListAdjustedEx(ObjA.child, ObjB, false);
  FSubscriber.VerifySendEvent(beItemDeleted, ObjA.M_child);

  {ObjA.child Current}
  Prepare;
  ObjA.child.EnsureContentsCurrent;
  ObjA2.child.EnsureContentsCurrent;
  VerifyState(ObjA.M_child, bvpsCurrent);
  VerifyState(ObjA2.M_child, bvpsCurrent);
  StoreValue(ObjB.M_parent);
  FSubscriber.SubscribeToElement(ObjB.M_parent);
  FSubscriber.SubscribeToElement(ObjA.M_child);
  FSubscriber.SubscribeToElement(ObjA2.M_child);
  Assert(ObjB.parent = ObjA);
  ObjB.parent := ObjA2; // modify
  VerifyState(ObjB.M_parent, bvpsModified);
  Assert(objB.parent = ObjA2);
  FSubscriber.VerifySendEvent(beValueChanged, ObjB.M_parent);
  VerifyIsInUndoArea(UndoHandler.UndoBlocks.CurrentBlock, ObjB.M_parent, GetStoredValueOfMember(ObjB.M_parent));
  VerifyState(ObjA.M_child, bvpsCurrent);
  VerifyListAdjustedEx(ObjA.child, ObjB, false);
  FSubscriber.VerifySendEvent(beItemDeleted, ObjA.M_child);
  VerifyState(ObjA2.M_child, bvpsCurrent);
  VerifyListAdjustedIn(ObjA2.child, ObjB, false);
  FSubscriber.VerifySendEvent(beItemAdded, ObjA2.M_child);

  CloseAll;
end;

procedure Tmaan_ModifyTestCase.TestModifyEmbeddedRoleTransient;
var
  ObjA1, ObjA2: TATransientClass;
  ObjB1, ObjB2: TAPersistentClass;

begin
  {EmbeddedRole.child Transient}
  GenerateObjects(System, 'APersistentClass', 2);
  RefreshSystem;
  FetchEnsuredClass(System, FAPersistentClassList, TAPersistentClass);
  ObjB1 := FAPersistentClassList[0];
  ObjB2 := FAPersistentClassList[1];
  ObjA1 := CreateATransientClass(System, nil);
  ObjA2 := CreateATransientClass(System, nil);
  ObjA1.many.Add(ObjB1);
  ObjA2.many.Add(ObjB2);
  VerifyState(ObjB1.M_one, bvpsTransient);
  VerifyState(ObjA1.M_many, bvpsTransient);
  VerifyState(ObjA2.M_many, bvpsTransient);
  FSubscriber.SubscribeToElement(ObjA1.M_many);
  FSubscriber.SubscribeToElement(ObjA2.M_many);
  FSubscriber.SubscribeToElement(ObjB1.M_one);
  UndoHandler.SetCheckPoint('ModifyEmbeddedTransient');
  StoreValue(ObjB1.m_one);
  Assert(ObjB1.one = ObjA1);
  ObjB1.one := ObjA2;  //modify
  VerifyState(ObjA1.M_many, bvpsTransient);
  VerifyState(ObjA2.M_many, bvpsTransient);
  VerifyState(ObjB1.M_one, bvpsTransient);
  Assert(objB2.one = ObjA2);
  FSubscriber.VerifySendEvent(beValueChanged, ObjB1.M_one);
  VerifyIsInUndoArea(UndoHandler.UndoBlocks.CurrentBlock, ObjB1.M_one, GetStoredValueOfMember(ObjB1.M_one));
  VerifyListAdjustedEx(ObjA1.M_many, ObjB1, false);
  FSubscriber.VerifySendEvent(beItemDeleted, ObjA1.M_many);
  VerifyListAdjustedIn(ObjA2.M_many, ObjB1, false);
  FSubscriber.VerifySendEvent(beItemAdded, ObjA2.M_many);

  CloseAll;
end;

procedure Tmaan_ModifyTestCase.TestModifyNonEmbeddedRoleDeleteCurrent;
var
  ObjA, ObjB: TSomeClass;
  ObjBLocator: TBoldObjectLocator;
  EmbeddedIndex: integer;

  procedure Prepare;
  begin
    FSomeClassList[1].parent := FsomeClassList[0];
    FSomeClassList[3].parent := FsomeClasslist[2];
    EmbeddedIndex := FsomeClasslist[1].M_parent.BoldMemberRTInfo.EmbeddedLinkIndex;
    RefreshSystem;
    ObjA := FSomeClassList[0];
    ObjBLocator := FSomeClassList.Locators[1];
    ObjA.child.EnsureContentsCurrent;
    VerifyState(ObjA.M_child, bvpsCurrent);
  end;

begin
  SetSimpleConfiguration;
  {ObjB.parent invalid}
  Prepare;
  FSubscriber.SubscribeToElement(ObjA.M_child);
  Assert( not Assigned(ObjBLocator.BoldObject));
  Assert(ObjA.child.IndexOfLocator(ObjBLocator) <> -1);
  ObjA.child.RemoveByIndex(ObjA.child.IndexOfLocator(ObjBLocator)); //modify-delete
  VerifyState((ObjBLocator.BoldObject as TSomeClass).M_parent, bvpsModified);
  VerifyState(ObjA.M_child, bvpsCurrent);
  Assert(ObjA.child.Count = 0, 'ListAdjustedEx failed');
  Assert(not ObjA.child.LocatorInList(ObjBLocator)); // VerifyListAdjustedEx(ObjA.child, ObjB, false);
  FSubscriber.VerifySendEvent(beItemDeleted, ObjA.M_child);

  {ObjB.parent invalid/adjust}
  Prepare;
  ObjB := (ObjBLocator.EnsuredBoldObject as TSomeClass);
  VerifyState(ObjB.M_parent, bvpsCurrent);
  ObjB.M_parent.Invalidate;
  VerifyState(ObjB.M_parent, bvpsInvalid);
  VerifyHasOldValues(ObjB.M_parent, true);
  FSubscriber.SubscribeToElement(ObjA.M_child);
  StoreValue(ObjB.M_parent);
  UndoHandler.SetCheckPoint('aCheckPoint');
  ObjA.child.Remove(ObjB); //modify-delete
  VerifyState(ObjB.M_parent, bvpsModified); //VerifyState(ObjB.M_parent, bvpsInvalid);
  VerifyIsInUndoArea(UndoHandler.UndoBlocks.CurrentBlock, ObjB.M_parent, GetStoredValueOfMember(ObjB.M_parent));
  VerifyState(ObjA.M_child, bvpsCurrent);
  VerifyListAdjustedEx(ObjA.child, ObjB, false);
  FSubscriber.VerifySendEvent(beItemDeleted, ObjA.M_child);

  {ObjB.parent current}
  Prepare;
  ObjB := (ObjBLocator.EnsuredBoldObject as TSomeClass);
  ObjB.parent;
  VerifyState(ObjB.M_parent, bvpsCurrent);
  FSubscriber.SubscribeToElement(ObjA.M_child);
  FSubscriber.SubscribeToElement(ObjB.M_parent);
  StoreValue(ObjB.M_parent);
  ObjA.child.Remove(ObjB); //modify-delete
  VerifyIsNil(ObjB.M_parent);
  FSubscriber.VerifySendEvent(beValueChanged, ObjB.M_parent);
  VerifyState(ObjB.M_parent, bvpsModified);
  VerifyIsInUndoArea(UndoHandler.UndoBlocks.CurrentBlock, ObjB.M_parent, GetStoredValueofMember(objB.M_parent));
  VerifyState(ObjA.M_child, bvpsCurrent);
  VerifyListAdjustedEx(ObjA.child, ObjB, false);
  FSubscriber.VerifySendEvent(beItemDeleted, ObjA.M_child);

  {ObjB.parent modified}
  Prepare;
  ObjB := (ObjBLocator.EnsuredBoldObject as TSomeClass);
  ObjB.parent := nil;
  ObjB.parent := ObjA;
  VerifyState(ObjB.M_parent, bvpsModified);
  FSubscriber.SubscribeToElement(ObjA.M_child);
  FSubscriber.SubscribeToElement(ObjB.M_parent);
  ObjA.child.Remove(ObjB); //modify-delete
  VerifyIsNil(ObjB.M_parent);
  VerifyState(ObjB.M_parent, bvpsModified);  //no state change
  FSubscriber.VerifySendEvent(beValueChanged, ObjB.M_parent);
  VerifyIsInUndoArea(UndoHandler.UndoBlocks.CurrentBlock, ObjB.M_parent, GetStoredValueofMember(ObjB.M_parent));
  VerifyState(ObjA.M_child, bvpsCurrent);
  VerifyListAdjustedEx(ObjA.child, ObjB, false);
  FSubscriber.VerifySendEvent(beItemDeleted, ObjA.M_child);

  CloseAll;
end;

procedure Tmaan_ModifyTestCase.TestModifyNonEmbeddedRoleInsertTransient;
var
  ObjA1, ObjA2: TATransientClass;
  ObjB1, ObjB2: TAPersistentClass;

begin
  {ObjA.child Transient}
  SetTransientConfiguration;
  ObjB1 := FAPersistentClassList[0];
  ObjB2 := FAPersistentClassList[1];
  ObjA1 := FATransientClassList[0];
  ObjA2 := FATransientClassList[1];
  VerifyState(ObjB1.M_one, bvpsTransient);
  VerifyState(ObjA1.M_many, bvpsTransient);
  VerifyState(ObjA2.M_many, bvpsTransient);
  FSubscriber.SubscribeToElement(ObjA1.M_many);
  FSubscriber.SubscribeToElement(ObjB2.M_one);
  FSubscriber.SubscribeToElement(ObjA2.M_many);
  StoreValue(ObjB2.m_one);
  UndoHandler.SetCheckPoint('Check1');
  Assert(ObjB2.one = ObjA2);
  ObjA1.many.Add(ObjB2); //modify
  VerifyState(ObjA1.M_many, bvpsTransient);
  VerifyState(ObjA2.M_many, bvpsTransient);
  VerifyState(ObjB1.M_one, bvpsTransient);
  VerifyState(ObjB2.M_one, bvpsTransient);
  Assert(ObjB2.one = ObjA1);
  FSubscriber.VerifySendEvent(beValueChanged, ObjB2.M_one);
  VerifyIsInUndoArea(UndoHandler.UndoBlocks.CurrentBlock, ObjB2.M_one, GetStoredValueOfMember(ObjB2.M_one));
  VerifyListAdjustedIn(ObjA1.M_many, ObjB2, false);
  FSubscriber.VerifySendEvent(beItemAdded, ObjA1.M_many);
  VerifyListAdjustedEx(ObjA2.M_many, ObjB2, false);
  FSubscriber.VerifySendEvent(beItemDeleted, ObjA2.M_many);
  CloseAll;
end;

procedure Tmaan_ModifyTestCase.TestModifyNonEmbeddedRoleInsertCurrent;
var
  ObjA, ObjB, ObjA2: TSomeClass;
  ObjBLocator: TBoldObjectLocator;
  EmbeddedIndex: integer;
  procedure Prepare;
  begin
    FSomeClassList[1].parent := FSomeClassList[3];
    FSomeClassList[3].parent := FsomeClasslist[2];
    RefreshSystem;
    ObjA := FSomeClassList[0];
    ObjBLocator := FSomeClassList.Locators[1];
    ObjA2 := (FSomeClassList.Locators[2].EnsuredBoldObject as TSomeClass);
    ObjA.child.EnsureContentsCurrent;
    EmbeddedIndex := ObjA.M_parent.BoldMemberRTInfo.EmbeddedLinkIndex;
    VerifyState(ObjA.M_child, bvpsCurrent);
  end;

begin
  SetSimpleConfiguration;
  {ObjB.parent invalid}
  Prepare;
  FSubscriber.SubscribeToElement(ObjA.M_child);
  Assert(not Assigned(ObjBLocator.BoldObject));
  ObjA.child.AddLocator(ObjBLocator); //modify-insert
  VerifyState(ObjA.M_child, bvpsCurrent);
  VerifyState((ObjBLocator.BoldObject as TSomeClass).M_parent, bvpsModified);
  Assert(ObjA.child.LocatorInList(ObjBLocator), 'VerifyListAdjustedIn failed');
  FSubscriber.VerifySendEvent(beItemAdded, ObjA.M_child);

  {ObjB.parent invalid/adjust}
  Prepare;
  ObjB := ObjBLocator.EnsuredBoldObject as TSomeClass;
  VerifyState(ObjB.M_parent, bvpsCurrent);
  Assert(ObjB.parent <> nil);
  ObjB.M_parent.Invalidate;
  VerifyState(ObjB.M_parent, bvpsInvalid);
  VerifyHasOldValues(ObjB.M_parent, true);
  StoreValue(ObjB.M_parent);
  UndoHandler.SetCheckPoint('aCheckPoint');
  FSubscriber.SubscribeToElement(ObjA.M_child);
  ObjA.M_child.Add(ObjB); //modify-insert
  VerifyState(ObjB.M_parent, bvpsModified);
  VerifyIsInUndoArea(UndoHandler.UndoBlocks.CurrentBlock, ObjB.M_parent, GetStoredValueOfMember(ObjB.M_parent));
  VerifyState(ObjA.M_child, bvpsCurrent);
  VerifyListAdjustedIn(ObjA.child, ObjB, false);
  FSubscriber.VerifySendEvent(beItemAdded, ObjA.M_child);

  {ObjB.parent current}
  Prepare;
  ObjB := ObjBLocator.EnsuredBoldObject as TSomeClass;
  ObjB.parent;
  VerifyState(ObjB.M_parent, bvpsCurrent);
  FSubscriber.SubscribeToElement(ObjA.M_child);
  FSubscriber.SubscribeToElement(ObjB.M_parent);
  StoreValue(ObjB.M_parent);
  ObjA.child.Add(ObjB); //modify
  VerifyState(ObjB.M_parent, bvpsModified);
  Assert(ObjB.M_parent.Locator = ObjA.BoldObjectlocator);
  VerifyIsInUndoArea(UndoHandler.UndoBlocks.CurrentBlock, ObjB.M_parent, GetStoredValueOfMember(ObjB.M_parent));
  FSubscriber.VerifySendEvent(beValueChanged, ObjB.M_parent);
  VerifyState(ObjA.M_child, bvpsCurrent);
  VerifyListAdjustedIn(ObjA.child, ObjB, false);
  FSubscriber.VerifySendEvent(beItemAdded, ObjA.M_child);

  {ObjB.parent modified}
  Prepare;
  ObjB := ObjBLocator.EnsuredBoldObject as TSomeClass;
  Assert(objB.parent <> ObjA2);
  ObjB.parent := ObjA2;
  VerifyState(ObjB.M_parent, bvpsModified);
  FSubscriber.SubscribeToElement(ObjA.M_child);
  FSubscriber.SubscribeToElement(ObjB.M_parent);
  UndoHandler.SetCheckPoint('Block1');
  StoreValue(ObjB.M_parent);
  ObjA.child.Add(ObjB);  //modify
  Assert((ObjB.M_parent as TBoldObjectReference).Locator = ObjA.BoldObjectLocator);
  VerifyState(ObjB.M_parent, bvpsModified);
  VerifyIsInUndoArea(UndoHandler.UndoBlocks.CurrentBlock, ObjB.M_parent, GetStoredValueOfMember(ObjB.M_parent));
  FSubscriber.VerifySendEvent(beValueChanged, ObjB.M_parent);
  VerifyState(ObjA.M_child, bvpsCurrent);
  VerifyListAdjustedIn(ObjA.child, ObjB, false);
  FSubscriber.VerifySendEvent(beItemAdded, ObjA.M_child);

  CloseAll;
end;

procedure Tmaan_ModifyTestCase.TestModifyNonEmbeddedRoleDeleteTransient;
var
  ObjA1: TATransientClass;
  ObjB1: TAPersistentClass;

begin
  {ObjA.child Transient}
  SetTransientConfiguration;
  ObjA1 := FATransientClassList[0];
  ObjB1 := FAPersistentClassList[0];
  VerifyState(ObjB1.M_one, bvpsTransient);
  VerifyState(ObjA1.M_many, bvpsTransient);
  FSubscriber.SubscribeToElement(ObjA1.M_many);
  FSubscriber.SubscribeToElement(ObjB1.M_one);
  StoreValue(ObjB1.m_one);
  UndoHandler.SetCheckPoint('Check1');
  ObjA1.many.Remove(ObjB1);  //modify
  VerifyState(ObjA1.M_many, bvpsTransient);
  VerifyState(ObjB1.M_one, bvpsTransient);
  VerifyIsNil(ObjB1.M_one);
  FSubscriber.VerifySendEvent(beValueChanged, ObjB1.M_one);
  VerifyIsInUndoArea(UndoHandler.UndoBlocks.CurrentBlock, ObjB1.M_one, GetStoredValueOfMember(ObjB1.M_one));
  VerifyListAdjustedEx(ObjA1.M_many, ObjB1, false);
  FSubscriber.VerifySendEvent(beItemDeleted, ObjA1.M_many);

  CloseAll;
end;

class function Tmaan_ModifyTestCase.Suite: ITestSuite;
begin
  Result := inherited Suite;
  SetCommentForTest(Result, 'TestModifyAttribute', '2.7b Undo block');
  SetCommentForTest(Result, 'TestModifyNonEmbeddedRoleInsertTransient', '2.7b Undo block');
  SetCommentForTest(Result, 'TestModifyNonEmbeddedRoleInsertCurrent', '2.7b Undo block');
  SetCommentForTest(Result, 'TestModifyNonEmbeddedRoleDeleteTransient', '2.7b Undo block');
  SetCommentForTest(Result, 'TestModifyNonEmbeddedRoleDeleteCurrent', '2.7b Undo block');
  SetCommentForTest(Result, 'TestModifyEmbeddedRoleCurrent', '2.7b Undo block');
  SetCommentForTest(Result, 'TestModifyEmbeddedRoleTransient', '2.7b Undo block');
  SetCommentForTest(Result, 'TestModifyEmbeddedRoleModified', '2.7b Undo block');
end;

{ Tmaan_UndoTestCase }

class procedure Tmaan_UndoTestCase.Suit(ASuite: TBoldTestSuite);
begin
  ASuite.AddTest(CreateWithComment('UndoAttribute', '2.7b & 2.2a Undo block'));
  ASuite.AddTest(CreateWithComment('UndoEmbeddedRoleModified', '2.7b & 2.2a Undo block'));
  ASuite.AddTest(CreateWithComment('UndoEmbeddedRoleTransient', '2.7b & 2.2a Undo block'));
  ASuite.AddTest(CreateWithComment('UndoObjectCreation', '2.7b & 2.2a Undo block'));
end;

class function Tmaan_UndoTestCase.Suite: ITestSuite;
begin
  Result := inherited Suite;
  SetCommentForTest(Result, 'UndoAttribute', '2.7b & 2.2a Undo block');
  SetCommentForTest(Result, 'UndoEmbeddedRoleModified', '2.7b & 2.2a Undo block');
  SetCommentForTest(Result, 'UndoEmbeddedRoleTransient', '2.7b & 2.2a Undo block');
  SetCommentForTest(Result, 'UndoObjectCreation', '2.7b & 2.2a Undo block');
end;

procedure Tmaan_UndoTestCase.UndoAttribute;
var
  SomeObject: TSomeClass;
  SomeObjectId: TBoldObjectId;
  TransientObject: TATransientClass;
  TransientObjectId: TBoldObjectId;
begin
  SetSimpleConfiguration;
  RefreshSystem;

  {transient attribute}
  TransientObject := CreateATransientClass(System, nil);
  TransientObjectId := TransientObject.BoldObjectLocator.BoldObjectId.Clone;
  UndoHandler.SetCheckPoint('UndoAttribute');
  StoreValue(TransientObject.M_aString);
  TransientObject.aString := TransientObject.aString + '123';
  VerifyState(TransientObject.M_aString, bvpsTransient);
  FSubscriber.SubscribeToElement(TransientObject.M_aString);
  UndoHandler.UnDoLatest; //Undo
  System.AssertLinkIntegrity;
  TransientObject := System.Locators.ObjectByID[TransientObjectId] as TATransientClass;
  TransientObject.ValuesAreEqual(TransientObject.M_aString.AsIBoldValue[bdepContents], GetStoredValueOfMember(TransientObject.M_aString), 'aString');
  VerifyState(TransientObject.M_aString, bvpsTransient);
  FSubscriber.VerifySendEvent(beValueChanged, TransientObject.M_aString);

  {Modified attribute}
  RefreshSystem;
  SomeObject := FSomeClassList[0];
  SomeObjectId := SomeObject.BoldObjectLocator.BoldObjectId.Clone;
  StoreValue(SomeObject.M_aString);
  VerifyState(SomeObject.M_aString, bvpsCurrent);
  SomeObject.aString := SomeObject.aString + '123';
  VerifyState(SomeObject.M_aString, bvpsModified);
  FSubscriber.SubscribeToElement(Someobject.M_aString);
  UndoHandler.UndoLatest;  //Undo
  System.AssertLinkIntegrity;
  SomeObject := System.Locators.ObjectById[SomeObjectId] as TSomeClass;
  VerifyState(SomeObject.M_aString, bvpsCurrent);
  Assert( SomeObject.ValuesAreEqual(SomeObject.M_aString.AsIBoldValue[bdepContents], GetStoredValueOfMember(SomeObject.M_aString), 'aString'));
  FSubscriber.VerifySendEvent(beValueChanged, SomeObject.M_aString);

  CloseAll;
end;

procedure Tmaan_UndoTestCase.UndoEmbeddedRoleModified;
var
  ObjA1, ObjA2, ObjB1, ObjB2: TSomeClass;
  ObjA1Id, ObjA2Id, ObjB1Id, ObjB2Id: TBoldObjectId;
  FSValue: TBFSObjectIdRef;
  procedure Prepare;
  begin
    RefreshSystem;
    FSomeClassList[1].parent := FSomeClasslist[0];
    FSomeClassList[3].parent := FSomeClassList[2];
    RefreshSystem;
    ObjA1 := FSomeClassList[0];
    ObjA1Id := ObjA1.BoldObjectLocator.BoldObjectId.Clone;
    ObjB1 := FSomeClassList[1];
    ObjB1Id := ObjB1.BoldObjectLocator.BoldObjectId.Clone;
    ObjA2 := FSomeClassList[2];
    ObjA2Id := ObjA2.BoldObjectLocator.BoldObjectId.Clone;
    ObjB2 := FSomeClassList[3];
    ObjB2Id := ObjB2.BoldObjectLocator.BoldObjectId.Clone;
  end;
  procedure SetObjectsFromIds;
  begin
    ObjA1 := System.Locators.ObjectById[ObjA1Id] as TSomeClass;
    ObjB1 := System.Locators.ObjectById[ObjB1Id] as TSomeClass;
    ObjA2 := System.Locators.ObjectById[ObjA2Id] as TSomeClass;
    ObjB2 := System.Locators.ObjectById[ObjB2Id] as TSomeClass;
  end;
begin
  SetSimpleConfiguration;
  Prepare;
  {NonEmbeddedRole.parent invalid, ObjA1 invalid, ObjA2 invalid}
  FSValue := TBFSObjectIdRef.create;
  try
    StoreValue(ObjB1.M_parent);
    VerifyState(ObjB1.M_parent, bvpsCurrent);
    ObjB1.parent := ObjA2; // modify
    VerifyState(ObjA1.M_child, bvpsInvalid);
    VerifyState(ObjA2.M_child, bvpsInvalid);
    VerifyState(ObjB1.M_parent, bvpsModified);
    FSubscriber.SubscribeToElement(ObjB1.M_parent);
    VerifyState(ObjB1.M_parent, bvpsModified);
    FSValue.AssignContent(ObjB1.M_parent.AsIBoldValue[bdepContents]);
    UndoHandler.UndoLatest; // Undo
    System.AssertLinkIntegrity;
    SetObjectsFromIds;
    VerifyState(ObjB1.M_parent, bvpsCurrent);
    ObjB1.ValuesAreEqual(ObjB1.M_parent.AsIBoldValue[bdepContents], GetStoredValueOfMember(ObjB1.M_parent), 'parent');
    VerifyIsInRedoArea(ObjB1.M_parent, FSValue);
    FSubscriber.VerifySendEvent(beValueChanged, ObjB1.M_parent);
    VerifyState(ObjA2.M_child, bvpsinvalid);
    VerifyState(ObjA1.M_child, bvpsInvalid);
  finally
    FreeAndNil(FSValue);
  end;

  {NonEmbeddedRole.parent invalid, EmbeddeRole1 invalid, ObjA2 current}
  FSValue := TBFSObjectIdRef.create;
  try
    StoreValue(ObjB1.M_parent);
    VerifyState(ObjB1.M_parent, bvpsCurrent);
    ObjB1.parent := ObjA2; // modify
    VerifyState(ObjA1.M_child, bvpsInvalid);
    ObjA2.child.EnsureContentsCurrent;
    VerifyState(ObjA2.M_child, bvpsCurrent);
    VerifyState(ObjB1.M_parent, bvpsModified);
    FSubscriber.SubscribeToElement(ObjB1.M_parent);
    FSubscriber.SubscribeToElement(ObjA2.M_child);
    FSubscriber.SubscribeToElement(ObjA1.M_child);
    VerifyState(ObjB1.M_parent, bvpsModified);
    FSValue.AssignContent(ObjB1.M_parent.AsIBoldValue[bdepContents]);
    Assert(objB1.parent = ObjA2);
    UndoHandler.UndoLatest; // Undo
    System.AssertLinkIntegrity;
    SetObjectsFromIds;
    VerifyState(ObjB1.M_parent, bvpsCurrent);
    Assert(ObjB1.parent = ObjA1);
    VerifyIsInRedoArea(ObjB1.M_parent, FSValue);
    FSubscriber.VerifySendEvent(beValueChanged, ObjB1.M_parent);
    VerifyState(ObjA1.M_child, bvpsInvalid);
    VerifyState(ObjA2.M_child, bvpsCurrent);
    Assert(ObjA2.child.IndexOf(ObjB1) = -1);
    FSubscriber.VerifySendEvent(beItemDeleted, ObjA2.M_child);
  finally
    FreeAndNil(FSValue);
  end;

  {NonEmbeddedRole.parent current, NonEmbeddedRole in parent.child}
  Prepare;
  FSValue := TBFSObjectIdRef.create;
  try
    StoreValue(ObjB2.M_parent);
    VerifyState(ObjB2.M_parent, bvpsCurrent);
    ObjB2.parent := ObjA1; // modify
    VerifyState(ObjB2.M_parent, bvpsModified);
    ObjA1.child.EnsureContentsCurrent;
    ObjA2.child.EnsureContentsCurrent;
    VerifyState(ObjA1.M_child, bvpsCurrent);
    VerifyState(ObjA2.M_child, bvpsCurrent);
    FSubscriber.SubscribeToElement(ObjB2.m_parent);
    FSubscriber.SubscribeToElement(ObjA1.M_child);
    FSubscriber.SubscribeToElement(ObjA2.M_child);
    FSValue := TBFSObjectIdRef.create;
    FSValue.AssignContent(ObjB2.M_parent.AsIBoldValue[bdepContents]);
    UndoHandler.UndoLatest; //Undo
    System.AssertLinkIntegrity;
    SetObjectsFromIds;
    VerifyState(ObjB2.M_parent, bvpsCurrent);
    ObjB2.ValuesAreEqual(ObjB2.M_parent.AsIBoldValue[bdepContents], GetStoredValueOfMember(ObjB2.M_parent), 'parent');
    VerifyIsInRedoArea(ObjB2.M_parent, FSValue);
    FSubscriber.VerifySendEvent(beValueChanged, ObjB2.M_parent);
    VerifyListAdjustedEx(ObjA1.child, ObjB2, false);
    VerifyListAdjustedIn(ObjA2.child, ObjB2, false);
    FSubscriber.VerifySendEvent(beItemDeleted, ObjA1.M_child);
    FSubscriber.VerifySendEvent(beItemAdded, ObjA2.M_child);
  finally
    FreeAndNil(FSValue);
  end;

  Prepare;
  {NonEmbeddedRole.parent invalid/adjust, ObjA1 invalid, ObjA2 current}
  FSValue := TBFSObjectIdRef.create;
  try
    VerifyState(ObjB1.M_parent, bvpsCurrent);
    ObjB1.M_parent.Invalidate;
    VerifyState(ObjB1.M_parent, bvpsInvalid);
    VerifyHasOldValues(ObjB1.M_parent);
    StoreValue(ObjB1.M_parent);
    ObjA2.child.Add(ObjB1); // modify
    FSubscriber.SubscribeToElement(ObjB1.M_parent);
    VerifyState(ObjB1.M_parent, bvpsModified);
    FSValue.AssignContent(ObjB1.M_parent.AsIBoldValue[bdepContents]);
    UndoHandler.UndoLatest; // Undo
    System.AssertLinkIntegrity;
    SetObjectsFromIds;
    VerifyState(ObjB1.M_parent, bvpsInvalid);
    VerifyHasOldValues(ObjB1.M_parent, true);
    ObjB1.ValuesAreEqual(ObjB1.M_parent.AsIBoldValue[bdepContents], GetStoredValueOfMember(ObjB1.M_parent), 'parent');
    VerifyIsInRedoArea(ObjB1.M_parent, FSValue);
    FSubscriber.VerifySendEvent(beValueChanged, ObjB1.M_parent);
  finally
    FreeAndNil(FSValue);
  end;
end;

procedure Tmaan_UndoTestCase.UndoEmbeddedRoleTransient;
var
  ObjA1, ObjA2: TATransientClass;
  ObjB2: TAPersistentClass;
  ObjA1Id, ObjA2Id, ObjB2Id: TBoldObjectId;
  FSValueBeforeUndo: TBFSObjectIdRef;
  HUndo: IBoldUndoHandler;
  blockname: string;
  procedure SetObjectsFromIds;
  begin
    ObjA1 := System.Locators.ObjectById[ObjA1Id] as TATransientClass;
    ObjA2 := System.Locators.ObjectById[ObjA2Id] as TATransientClass;
    ObjB2 := System.Locators.ObjectById[ObjB2Id] as TAPersistentClass;
  end;
begin
  SetTransientConfiguration;
  ObjB2 := FAPersistentClassList[1];
  ObjB2Id := ObjB2.BoldObjectLocator.BoldObjectId.Clone;
  ObjA1 := FATransientClassList[0];
  ObjA1Id := ObjA1.BoldObjectLocator.BoldObjectId.Clone;
  ObjA2 := FATransientClassList[1];
  ObjA2Id := ObjA2.BoldObjectLocator.BoldObjectId.Clone;
  FSValueBeforeUndo := TBFSObjectIdRef.create;
  HUndo := (self.System.UndoHandler as IBoldUndoHandler);
  try
    blockname := HUndo.SetCheckPoint();
    StoreValue(ObjB2.M_one);
    ObjB2.one := ObjA1; // modify
    VerifyState(ObjB2.M_one, bvpsTransient);
    VerifyState(ObjA1.M_many, bvpsTransient);
    VerifyState(ObjA2.M_many, bvpsTransient);
    FSubscriber.SubscribeToElement(ObjB2.m_one);
    FSubscriber.SubscribeToElement(ObjA1.M_many);
    FSubscriber.SubscribeToElement(ObjA2.M_many);
    FSValueBeforeUndo := TBFSObjectIdRef.create;
    FSValueBeforeUndo.AssignContent(ObjB2.M_one.AsIBoldValue[bdepContents]);
    UndoHandler.UndoLatest; //Undo
    System.AssertLinkIntegrity;
    SetObjectsFromIds;
    VerifyState(ObjB2.M_one, bvpsTransient);
    ObjB2.ValuesAreEqual(ObjB2.M_one.AsIBoldValue[bdepContents], GetStoredValueOfMember(ObjB2.M_one), 'one');
    VerifyIsInRedoArea(ObjB2.M_one, FSValueBeforeUndo);
    FSubscriber.VerifySendEvent(beValueChanged, ObjB2.M_one);
    VerifyListAdjustedEx(ObjA1.many, ObjB2, false);
    VerifyListAdjustedIn(ObjA2.many, ObjB2, false);
    FSubscriber.VerifySendEvent(beItemDeleted, ObjA1.M_many);
    FSubscriber.VerifySendEvent(beItemAdded, ObjA2.M_many);
  finally
    FreeAndNil(FSValueBeforeUndo);
    HUndo := nil;
    CloseAll;
  end;
end;

procedure Tmaan_UndoTestCase.UndoObjectCreation;
var
  ObjA1, ObjB1: TSomeClass;
  ObjA1Id, ObjB1Id: TBoldObjectId;
  FSValue: TBFSObjectIdRef;
  HUndo: IBoldUndoHandler;
  blockName: string;
  procedure SetObjectsFromIds;
  begin
    ObjA1 := System.Locators.ObjectById[ObjA1Id] as TsomeClass;
    ObjB1 := System.Locators.ObjectById[ObjB1Id] as TsomeClass;    
  end;
begin
  if not dmUndoRedo.BoldSystemHandle1.Active then
    dmUndoRedo.BoldSystemHandle1.Active := true;
  HUndo:= (Self.System.UndoHandler as IBoldUndoHandler);
  ObjA1 := CreateSomeClass(self.System, nil);
  UpdateDatabase;
  ObjA1Id := ObjA1.BoldObjectLocator.BoldObjectId.Clone;  
  BlockName := HUndo.SetCheckPoint('Undo object creation');
  ObjB1 := CreateSomeClass(self.System, nil);
  ObjB1Id := ObjB1.BoldObjectLocator.BoldObjectId.Clone;
  ObjB1.parent := ObjA1;
  FSValue := TBFSObjectIdRef.create;
  try
    FSValue.AssignContent(ObjB1.M_parent.AsIBoldValue[bdepContents]);
    HUndo.UndoBlock(blockname); // undo
    SetObjectsFromIds;
    Assert(ObjA1.BoldExistenceState = besExisting);
    Assert( not Assigned(ObjB1));
    Assert(ObjA1.child.Count = 0);
    HUndo.RedoBlock(blockname); // redo
    SetObjectsFromIds;
    Assert(ObjA1.BoldExistenceState = besExisting);
    Assert(ObjB1.BoldExistenceState = besExisting);
    ObjB1.ValuesAreEqual(ObjB1.M_parent.AsIBoldValue[bdepContents], FSValue, 'parent');
    Assert(ObjA1.child.count = 1);
  finally
    FreeAndNil(FSValue);
    HUndo := nil;
    CloseAll;
  end;
end;

{
procedure Tmaan_UndoTestCase.UndoObjectDeletion;
begin

end;
}

initialization
  Randomize;
  TestGlobal.RegisterTestCase(Tmaan_FetchRefetchTestCase);
  TestGlobal.RegisterTestCase(Tmaan_ModifyTestCase);
  TestGlobal.RegisterTestCase(Tmaan_UndoTestCase);

finalization
  TestGlobal.UnRegisterTestCase(Tmaan_FetchRefetchTestCase);
  TestGlobal.UnRegisterTestCase(Tmaan_ModifyTestCase);
  TestGlobal.UnRegisterTestCase(Tmaan_UndoTestCase);
end.


