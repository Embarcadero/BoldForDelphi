unit aniv_1;

{$include bold.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  TestSuite, BoldSubscription, BoldHandles, BoldSystemHandle, dmModel1,
  BoldRootedHandles, BoldAbstractListHandle, BoldCursorHandle,
  BoldListHandle, BoldHandle, BoldPersistenceHandle,
  BoldPersistenceHandleDB,
  BoldNumericControlPack, BoldElements, BoldControlPack,
  BoldFloatControlPack, ActnList, BoldHandleAction, BoldActions,
  BoldComServerHandles, BoldClientHandles,
  BoldDEfs,
  BoldLinks, BoldDomainElement,
  BoldObjectRetriever,
  BoldPMappersAttributeDefault,
  BoldServerHandles, BoldDbActions, BoldPersistenceHandleFileXML,
  TestFrameWork,
  BoldPersistenceHandleFile, BoldManipulators, BoldAbstractModel, BoldModel,
  BoldUMLModelLink, BoldUMLXMILink, DB,
  BoldAbstractDatabaseAdapter,   BoldAbstractPersistenceHandleDB,
  BoldDatabaseAdapterUniDAC, DBAccess, Uni,  PostgreSQLUniProvider,
  DAScript, UniScript, System.Actions;

type
  Tdm_aniv_test1 = class(TDataModule)
    BoldSystemHandle1: TBoldSystemHandle;
    BoldListHandle1: TBoldListHandle;
    BoldSystemHandle2: TBoldSystemHandle;
    blhAllSongs: TBoldListHandle;
    blhAllHitlists: TBoldListHandle;
    ActionList1: TActionList;
    BoldSystemHandle3: TBoldSystemHandle;
    BoldPersistenceHandleFileXML1: TBoldPersistenceHandleFileXML;
    BoldManipulator1: TBoldManipulator;
    BoldModel1: TBoldModel;
    BoldUMLXMILink1: TBoldUMLXMILink;
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
    BoldPersistenceHandleDB2: TBoldPersistenceHandleDB;
    UniConnection1: TUniConnection;
    BoldDatabaseAdapterUniDAC1: TBoldDatabaseAdapterUniDAC;
    BoldDatabaseAdapterUniDAC2: TBoldDatabaseAdapterUniDAC;
    UniConnection2: TUniConnection;
    UniScript1: TUniScript;
//    BoldModel1: TBoldModel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TAniv_1_ALL = class(TBoldTestCase)
  public
//    class procedure Suit(ASuite: TBoldTestSuite); override;
  end;

  TAniv_1_ListTraverser = class(TBoldTestCase)
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTraverser;
  end;

  TAniv_Transaction1 = class(TBoldTestCase)
  protected
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure RollbackAttributes;
    procedure RollbackObjectCreate;
    procedure RollbackObjectDelete;
    procedure RollbackAssociation;
    procedure RollbackAssociation2;
  end;

  TAniv_AssocWithLinkClass = class(TBoldTestCase)
  protected
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    procedure SetUp; override;
    procedure TearDown; override;    
  published
    procedure OneMany;
  end;

  TAniv_ParentChild = class(TBoldTestCase)
  protected
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure NonEmbeddedChildMappedAssocs;
  end;

  TAniv_SingleSingleEmbeddedEmbeddedAssoc = class(TBoldTestCase)
  protected
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Fetch;
  end;

  TAniv_FilePersistenceMapper = class(TBoldTestCase)
  private
    procedure Fetch;
  protected
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure WriteAllOnUpdateNew;
    procedure WriteWithUnfetchedMembers;
  end;

  TAniv_AutomaticRemovalOfObjectFromLists = class(TBoldTestCase)
  protected
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure DeleteObject;
  end;

  TAniv_ClassListTests = class(TBoldTestCase)
  protected
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
    procedure TearDown; override;
    procedure SetUp; override;
  published
    procedure DeletedObjectsInMemory;
  end;


  TAniv_InvalidateMembers = class(TBoldTestCase)
  protected
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure InvalidateMember;
    procedure NewObject;
    procedure TransientObject;
    procedure DerivedAttributeTransientObject;
  end;

  TAniv_Discard = class(TBoldTestCase)
  protected
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Discard;
  end;

  TAniv_Links = class(TBoldTestCase)
  private
    fModifyEventReceived: Boolean;
    procedure _ReceiveModify(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
  protected
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure DeleteObjectTest;
    procedure OldValuesAfterDelete;
    procedure DeleteFromInvalidOrdered;
    procedure ReassigningOneManyWithLinkClass;
    procedure FetchSingleWithOrderedMulti;
    procedure RollingBackLinks;
    procedure RollingBackLinks2;
    procedure RollingBackLinks3;
    procedure DiscardSingle;
    procedure DeleteLinkObject;
    procedure DiscardDeletedLinkObject;
    procedure SetSingleWithInvalidOrderedMulti;
    procedure ReuseDeletedLinkObjectOnRelink1ToMany;
    procedure ReuseDeletedLinkObjectOnRelinkManyToManyOrdered;
  end;

  TAniv_ObjectList = class(TBoldTestCase)
  protected
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure ClearWithDestroyedObjects;
  end;

  TAniv_PrioQueue = class(TBoldTestCase)
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
  published
  end;

  TAniv_Various = class(TBoldTestCase)
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TransientAttributeWithInitialValue;
    procedure TestManipulator;
  end;

  TAniv_fmDistributable = class(TBoldTestCase)
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Fetch;
    procedure TimestampCondition;
  end;

  TAniv_XMI = class(TBoldTestCase)
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
  published
    procedure ExportAndImport;
    procedure ExportAndImport2;
  end;

var
  dm_aniv_test1: Tdm_aniv_test1 = nil;

implementation

{$R *.DFM}

uses
  BoldPersistenceController,
  BoldGlobalId,
  TestModel1,
  BoldId,
  BoldDefaultId,
  BoldValueSpaceInterfaces,
  BoldCondition,
  BoldUMLModelDataModule,
  BoldUMLModel,
  BoldUMLTypes,
  BoldSystemComparer,
  BoldSystem,
  BoldIndexableList, BoldAttributes;

const
  FLOATCONST1: Real = 1.1;
  INTEGERCONST1: Integer = 1;
  STRINGCONST1: String = 'a';
  BOOLEANCONST1: Boolean = true;
  CURRENCYCONST1: Currency = 0.1;

  FLOATCONST2: Real = 2.2;
  INTEGERCONST2: Integer = 2;
  STRINGCONST2: String = 'bb';
  BOOLEANCONST2: Boolean = false;
  CURRENCYCONST2: Currency = 2.0;

  FIRST_VALUE = 'First value';
  SECOND_VALUE = 'Second value';

  NEW_STRING = 'new';
  OLD_STRING = 'old';


procedure EnsureDM;
begin
  Ensuredm_Model;
  if not assigned(dm_aniv_test1) then
  begin
    Application.Initialize;
    dm_aniv_test1 := Tdm_aniv_test1.Create(Application);
    (dm_aniv_test1.BoldSystemHandle1.PersistenceHandle as TBoldPersistenceHandleDb).CreateDataBase;
    (dm_aniv_test1.BoldSystemHandle1.PersistenceHandle as TBoldPersistenceHandleDb).CreateDataBaseSchema;
    dm_aniv_test1.BoldDatabaseAdapterUniDAC2.Connection.Database := dm_aniv_test1.BoldDatabaseAdapterUniDAC1.Connection.Database;
//    dm_aniv_test1.IBDatabase2.DatabaseName := dm_aniv_test1.IBDatabase1.DatabaseName;
    dm_aniv_test1.BoldSystemHandle1.Active := True;
    dm_aniv_test1.BoldSystemHandle2.Active := True;
  end;
end;

procedure FreeDm;
begin
  if dm_aniv_test1.BoldSystemHandle1.Active then  
    dm_aniv_test1.BoldSystemHandle1.System.Discard;
  FreeAndNil(dm_aniv_test1)
end;

procedure ClearSystem(BoldSystemHandle: TBoldSystemHandle);
begin
  if BoldSystemHandle.Active then
  begin
    BoldSystemHandle.System.Discard;
    BoldSystemHandle.Active := false;
  end;
  BoldSystemHandle.Active := true;
end;

procedure EmptyDb(BoldSystem: TBoldSystem);
begin
  BoldSystem.Classes[0].DeleteObjects;
  BoldSystem.UpdateDatabase;
end;

function GlobalIdStringForObject(BoldObject: TBoldObject): string;
var
  anIdList : TBoldObjectIdList;
  aTranslationList : TBoldIdTranslationList;
  aPersistenceController : TBoldPersistenceController;
  aGlobalId : TBoldGlobalId;
  anObjectId : TBoldObjectID;

begin
  anIdList := TBoldObjectIdList.Create;
  aTranslationList := TBoldIdTranslationList.Create;
  result := '';
  try
    anObjectId := BoldObject.BoldObjectLocator.BoldObjectId;
    anIdList.Add(anObjectId);

    aPersistenceController := BoldObject.BoldSystem.PersistenceController;
    aPersistenceController.PMTranslateToGlobalIds(anIdList, aTranslationList);
    aGlobalId := aTranslationList.TranslateToNewId[anObjectId] as TBoldGlobalId;
    result := aGlobalID.asString;
  finally
    anIdList.Free;
    aTranslationList.Free;
  end;

end;

{ TSimpleOK }

class procedure TAniv_Transaction1.Suit(ASuite: TBoldTestSuite);
begin
  ASuite.AddTest(CreateWithComment('RollbackAttributes'));
  ASuite.AddTest(CreateWithComment('RollbackObjectCreate'));
  ASuite.AddTest(CreateWithComment('RollbackObjectDelete'));
  ASuite.AddTest(CreateWithComment('RollbackAssociation'));
  ASuite.AddTest(CreateWithComment('RollbackAssociation2'));
end;

procedure TAniv_Transaction1.RollbackAttributes;
var
  anObject: TClassA;
begin
  anObject := TClassA.Create(dm_aniv_test1.BoldSystemHandle1.System);

  anObject.aString := STRINGCONST1;
  anObject.aBoolean := BOOLEANCONST1;
  anObject.aByte := INTEGERCONST1;
  anObject.aCurrency := CURRENCYCONST1;
  anObject.aDate := INTEGERCONST1;
  anObject.aDateTime := FLOATCONST1;
  anObject.aFloat := FLOATCONST1;
  anObject.aInteger := INTEGERCONST1;
  anObject.aShortInt := INTEGERCONST1;
  anObject.aSmallInt := INTEGERCONST1;
  anObject.aTime := Frac(FLOATCONST1);
  anObject.aWord := INTEGERCONST1;
  anObject.aBlob := STRINGCONST1;
  anObject.aBlobContent := STRINGCONST1;

  dm_aniv_test1.BoldSystemHandle1.System.StartTransaction;

  anObject.aString := STRINGCONST2;
  anObject.aBoolean := BOOLEANCONST2;
  anObject.aByte := INTEGERCONST2;
  anObject.aCurrency := CURRENCYCONST2;
  anObject.aDate := INTEGERCONST2;
  anObject.aDateTime := FLOATCONST2;
  anObject.aFloat := FLOATCONST2;
  anObject.aInteger := INTEGERCONST2;
  anObject.aShortInt := INTEGERCONST2;
  anObject.aSmallInt := INTEGERCONST2;
  anObject.aTime := Frac(FLOATCONST2);
  anObject.aWord := INTEGERCONST2;
  anObject.aBlob := STRINGCONST2;
  anObject.aBlobContent := STRINGCONST2;

  dm_aniv_test1.BoldSystemHandle1.System.RollbackTransaction;

  assert(anObject.aString = STRINGCONST1, 'string');
  assert(anObject.aBoolean = BOOLEANCONST1, 'boolean');
  assert(anObject.aByte = INTEGERCONST1, 'byte');
  assert(anObject.aCurrency = CURRENCYCONST1, 'currency');
  assert(anObject.aDate = INTEGERCONST1, 'date');
  assert(anObject.aDateTime = FLOATCONST1, 'datetime');
  assert(anObject.aFloat = FLOATCONST1, 'float');
  assert(anObject.aInteger = INTEGERCONST1, 'integer');
  assert(anObject.aShortInt = INTEGERCONST1, 'shortint');
  assert(anObject.aSmallInt = INTEGERCONST1, 'smallint');
  assert(anObject.aTime = Frac(FLOATCONST1), 'time');
  assert(anObject.aWord = INTEGERCONST1, 'word');
  assert(anObject.aBlob = STRINGCONST1, 'blob');
  assert(anObject.aBlobContent = STRINGCONST1, 'typedblob');

  anObject.Discard;
end;


procedure TAniv_Transaction1.RollbackObjectCreate;
var
  anObject: TClassA;
begin
  dm_aniv_test1.BoldSystemHandle1.System.StartTransaction;

  anObject := TClassA.Create(dm_aniv_test1.BoldSystemHandle1.System);

  dm_aniv_test1.BoldSystemHandle1.System.RollbackTransaction;

  assert(anObject.BoldExistenceState = besNotCreated, 'ExistenceState');
  assert(dm_aniv_test1.BoldListHandle1.List.count = 0, 'allInstances count');
  assert(dm_aniv_test1.BoldSystemHandle1.System.DirtyObjects.count = 0, 'dirty objects');
end;

procedure TAniv_Transaction1.RollbackObjectDelete;
var
  anObject: TClassA;
begin
  anObject := TClassA.Create(dm_aniv_test1.BoldSystemHandle1.System);

  dm_aniv_test1.BoldSystemHandle1.System.StartTransaction;

  anObject.Delete;

  dm_aniv_test1.BoldSystemHandle1.System.RollbackTransaction;

  assert(dm_aniv_test1.BoldListHandle1.List.count = 1, 'allInstances count');
  assert(dm_aniv_test1.BoldSystemHandle1.System.DirtyObjects.count = 1, 'dirty objects');

  anObject.Discard;
end;

procedure TAniv_Transaction1.RollbackAssociation;
var
  anObject1: TClassA;
  anObject2: TClassA;
begin
  anObject1 := TClassA.Create(dm_aniv_test1.BoldSystemHandle1.System);
  anObject2 := TClassA.Create(dm_aniv_test1.BoldSystemHandle1.System);

  dm_aniv_test1.BoldSystemHandle1.System.StartTransaction;

  anObject1.part.Add(anObject2);
  anObject1.parent := anObject2;
  anObject1.next := anObject2;

  dm_aniv_test1.BoldSystemHandle1.System.RollbackTransaction;

  assert(anObject1.part.Count = 0, 'Many-Many');
  assert(anObject2.partof.count = 0, 'Many-Many (reverse)');
  assert(anObject1.parent = nil, 'One-Many');
  assert(anObject2.child.count = 0, 'Many-One');
  assert(anObject1.next = nil, 'One-One');
  assert(anObject2.previous = nil, 'One-One (reverse)');

  anObject1.Discard;
  anObject2.Discard;
end;

procedure TAniv_Transaction1.RollbackAssociation2;
var
  anObject1: TClassA;
  anObject2: TClassA;
begin
  anObject1 := TClassA.Create(dm_aniv_test1.BoldSystemHandle1.System);
  anObject2 := TClassA.Create(dm_aniv_test1.BoldSystemHandle1.System);

  anObject1.part.Add(anObject2);
  anObject1.parent := anObject2;
  anObject1.next := anObject2;

  dm_aniv_test1.BoldSystemHandle1.System.StartTransaction;

  anObject1.part.remove( anObject2 );
  anObject1.parent := nil;
  anObject1.next := nil;

  dm_aniv_test1.BoldSystemHandle1.System.RollbackTransaction;

  assert(anObject1.part.Count = 1, 'Many-Many Count');
   assert(anObject1.part[0] = anObject2, 'Many-Many Identity');
   assert(anObject2.partof.count = 1, 'Many-Many Count(reverse)');
   assert(anObject2.partof[0] = anObject1, 'Many-Many Identity (reverse)');
   assert(anObject1.parent = anObject2, 'One-Many');
  assert(anObject2.child.count = 1, 'Many-One Count');
  assert(anObject2.child[0] = anObject1, 'Many-One Identity');
  assert(anObject1.next = anObject2, 'One-One');
  assert(anObject2.previous = anObject1, 'One-One (reverse)');

  dm_aniv_test1.BoldSystemHandle1.System.Discard;
end;

{ TAniv_AssocWithLinkClass }

procedure TAniv_AssocWithLinkClass.OneMany;
var
  aThing1: TThing;
  aThing2: TThing;
begin
  aThing1 := TThing.Create(dm_aniv_test1.BoldSystemHandle1.System);
  aThing2 := TThing.Create(dm_aniv_test1.BoldSystemHandle1.System);

  aThing1.many.Add(aThing2);
  assert(aThing1.many.Count = 1, 'Adding an object to multilink failed. Wrong count.');
  assert(aThing2.one = aThing1, 'Adding an object to multilink failed. Reverse role wrong.');
  assert(dm_aniv_test1.BoldSystemHandle1.System.DirtyObjects.Count = 3, 'Adding an object to multilink failed. Wrong number of dirty objects.');

  aThing1.many.Remove(aThing2);

  assert(aThing1.many.Count = 0, 'Removing an object from multilink failed. Wrong count.');
  assert(not assigned(aThing2.one), 'Removing an object from multilink failed. Reverse role wrong.');
  assert(dm_aniv_test1.BoldSystemHandle1.System.DirtyObjects.Count = 2, 'Removing an object from multilink failed. Wrong number of dirty objects.');

  aThing2.one := aThing1;
  assert(aThing2.one = aThing1, 'Adding an object to singlelink failed.');
  assert(aThing1.many.count = 1, 'Adding an object to singlelink failed. Reverse role count wrong.');
  assert(aThing1.many[0] = aThing2, 'Adding an object to singlelink failed. Reverse role wrong.');
  assert(dm_aniv_test1.BoldSystemHandle1.System.DirtyObjects.count = 3, 'Adding an object to singlelink failed. Wrong number of dirty objects.');

  aThing2.one := nil;
  assert(aThing2.one = nil, 'Removing an object from singlelink failed.');
  assert(aThing1.many.count = 0, 'Removing an object from singlelink failed. Reverse role count wrong.');
  assert(dm_aniv_test1.BoldSystemHandle1.System.DirtyObjects.Count = 2, 'Removing an object from singlelink failed. Wrong number of dirty objects.');

  aThing2.one := aThing1;
  assert(aThing2.one = aThing1, 'Adding an object to singlelink failed.');
  assert(aThing1.many.count = 1, 'Adding an object to singlelink failed. Reverse role count wrong.');
  assert(aThing1.many[0] = aThing2, 'Adding an object to singlelink failed. Reverse role wrong.');
  assert(dm_aniv_test1.BoldSystemHandle1.System.DirtyObjects.count = 3, 'Adding an object to singlelink failed. Wrong number of dirty objects.');

  aThing2.one := nil;
  assert(aThing2.one = nil, 'Removing an object from singlelink failed.');
  assert(aThing1.many.count = 0, 'Removing an object from singlelink failed. Reverse role count wrong.');
  assert(dm_aniv_test1.BoldSystemHandle1.System.DirtyObjects.Count = 2, 'Removing an object from singlelink failed. Wrong number of dirty objects.');

  aThing2.one := aThing1;
  assert(aThing2.one = aThing1, 'Adding an object to singlelink failed.');
  assert(aThing1.many.count = 1, 'Adding an object to singlelink failed. Reverse role count wrong.');
  assert(aThing1.many[0] = aThing2, 'Adding an object to singlelink failed. Reverse role wrong.');
  assert(dm_aniv_test1.BoldSystemHandle1.System.DirtyObjects.count = 3, 'Adding an object to singlelink failed. Wrong number of dirty objects.');

  aThing2.one := nil;
  assert(aThing2.one = nil, 'Removing an object from singlelink failed.');
  assert(aThing1.many.count = 0, 'Removing an object from singlelink failed. Reverse role count wrong.');
  assert(dm_aniv_test1.BoldSystemHandle1.System.DirtyObjects.Count = 2, 'Removing an object from singlelink failed. Wrong number of dirty objects.');

  dm_aniv_test1.BoldSystemHandle1.System.Discard;
end;

procedure TAniv_AssocWithLinkClass.SetUp;
begin
  inherited;
  EnsureDM;
end;

procedure TAniv_AssocWithLinkClass.TearDown;
begin
  FreeDm;
end;

class procedure TAniv_AssocWithLinkClass.Suit(ASuite: TBoldTestSuite);
begin
  ASuite.AddTest(CreateWithComment('OneMany'));
end;

{ TAniv_AutomaticRemovalOfObjectFromLists }

procedure TAniv_AutomaticRemovalOfObjectFromLists.DeleteObject;
var
  anObjectList: TBoldObjectList;
  anObject: TBoldObject;
begin
  ClearSystem(dm_aniv_test1.BoldSystemHandle1);
  anObjectList := TBusinessClassesRootList.Create;

  anObject := TClassA.Create(dm_aniv_test1.BoldSystemHandle1.System);
  anObjectList.Add(anObject);
  anObject.Delete;

  assert(anObjectList.Count = 0, 'Automatic object removal failed.');
  assert(dm_aniv_test1.BoldSystemHandle1.System.DirtyObjects.Count = 0, 'There are dirty objects');

  anObjectList.Free;
end;

procedure TAniv_AutomaticRemovalOfObjectFromLists.SetUp;
begin
  inherited;
  EnsureDM;
end;

procedure TAniv_AutomaticRemovalOfObjectFromLists.TearDown;
begin
  FreeDm;
end;

class procedure TAniv_AutomaticRemovalOfObjectFromLists.Suit(
  ASuite: TBoldTestSuite);
begin
  ASuite.AddTest(CreateWithComment('DeleteObject'));
end;

class function TAniv_AutomaticRemovalOfObjectFromLists.Suite: ITestSuite;
begin
  Result := inherited Suite;
  SetCommentForTest(Result, 'DeleteObject', '');
end;

{ TAniv_InvalidateMembers }

procedure TAniv_InvalidateMembers.InvalidateMember;
var
  aClassListOne, aClassListTwo: TBoldObjectList;
  anObjectOne, anObjectTwo: TClassA;
begin
  aClassListOne := dm_aniv_test1.BoldSystemHandle1.System.ClassByExpressionName['ClassA'];
  if aClassListOne.Count < 1 then
  begin
    anObjectOne := TClassA.create(dm_aniv_test1.BoldSystemHandle1.System);
  end else
  begin
    while aClassListOne.Count > 1 do
    begin
      aClassListOne[0].Delete;
    end;
    anObjectOne := aClassListOne[0] as TClassA;
  end;

  Assert(anObjectOne.BoldPersistent);
  Assert(anObjectOne.M_aString.BoldPersistent);
  anObjectOne.aString := FIRST_VALUE;
  dm_aniv_test1.BoldSystemHandle1.System.UpdateDatabase;
  aClassListTwo := dm_aniv_test1.BoldSystemHandle2.System.ClassByExpressionName['ClassA'];
  aClassListTwo.Invalidate;
  assert(aClassListTwo.Count = 1, 'Invalidating member: test precondition failed.');
  anObjectTwo := aClassListTwo[0] as TClassA;
  assert(anObjectTwo.aString = FIRST_VALUE, 'Invalidating member: test precondition failed.');

  anObjectOne.AString := SECOND_VALUE;
  dm_aniv_test1.BoldSystemHandle1.System.UpdateDatabase;
  anObjectTwo.M_AString.Invalidate;
  assert(anObjectTwo.AString = SECOND_VALUE, 'Invalidating member failed. Refetched value wrong.');

  dm_aniv_test1.BoldSystemHandle1.System.Discard;
  dm_aniv_test1.BoldSystemHandle2.System.Discard;
  dm_aniv_test1.BoldSystemHandle2.Active := False;
  dm_aniv_test1.BoldSystemHandle2.Active := True;
end;

procedure TAniv_InvalidateMembers.SetUp;
begin
  inherited;
  EnsureDM;
end;

procedure TAniv_InvalidateMembers.TearDown;
begin
  FreeDm;
end;


class procedure TAniv_InvalidateMembers.Suit(ASuite: TBoldTestSuite);
begin
  ASuite.AddTest(CreateWithComment('DerivedAttributeTransientObject'));
  ASuite.AddTest(CreateWithComment('TransientObject'));
  ASuite.AddTest(CreateWithComment('NewObject'));
  ASuite.AddTest(CreateWithComment('InvalidateMember'));
end;

procedure TAniv_Transaction1.SetUp;
begin
  EnsureDM;
  while dm_aniv_test1.BoldListHandle1.Count > 0 do
    dm_aniv_test1.BoldListHandle1.CurrentBoldObject.Delete;
  dm_aniv_test1.BoldSystemHandle1.UpdateDatabase;
  dm_aniv_test1.BoldSystemHandle1.Active := false;
  dm_aniv_test1.BoldSystemHandle1.Active := true;
end;

procedure TAniv_Transaction1.TearDown;
begin
  FreeDm;
end;

procedure TAniv_InvalidateMembers.NewObject;
var
  anObj: TClassA;
begin
  anObj := TClassA.Create(dm_aniv_test1.BoldSystemHandle1.System);
  try
    anObj.M_child.Invalidate;
    assert(false, 'Invalidate should have raised an exception');
  except
    on e: EBold do;
  end;
end;

procedure TAniv_InvalidateMembers.TransientObject;
var
  anObj: TClassA;
begin
  anObj := TClassA.Create(dm_aniv_test1.BoldSystemHandle1.System, false);
  try
    anObj.M_child.Invalidate;
    assert(false, 'Invalidate should have raised an exception');
  except
    on e: EBold do;
  end;
end;

procedure TAniv_InvalidateMembers.DerivedAttributeTransientObject;
var
  anObj: TSomething;
begin
  anObj := TSomething.Create(dm_aniv_test1.BoldSystemHandle1.System, false);
  anObj.aDerivedString;
  anObj.M_aDerivedString.Invalidate;
end;

class function TAniv_InvalidateMembers.Suite: ITestSuite;
begin
  Result := inherited Suite;
  SetCommentForTest(Result, 'DerivedAttributeTransientObject', '');
  SetCommentForTest(Result, 'TransientObject', '');
  SetCommentForTest(Result, 'NewObject', '');
  SetCommentForTest(Result, 'InvalidateMember', '');
end;

{ TAniv_Discard }

procedure TAniv_Discard.Discard;
var
  NewObj,   TransientObj, DeleteObj, ModifiedObj: TClassA;
begin
  DeleteObj := dm_aniv_test1.BoldListHandle1.List[0] as TClassA;
  ModifiedObj :=  dm_aniv_test1.BoldListHandle1.List[1] as TClassA;

  GlobalIdStringForObject(DeleteObj);

  NewObj := TClassA.Create(dm_aniv_test1.BoldSystemHandle1.System);

  TransientObj := TClassA.Create(dm_aniv_test1.BoldSystemHandle1.System, {$IFDEF NoTransientInstancesOfPersistentClass}True{$ELSE}False{$ENDIF});
  DeleteObj.aString := NEW_STRING;
  DeleteObj.Delete;
  ModifiedObj.aString := NEW_STRING;

  NewObj.Discard;
  TransientObj.Discard;
  DeleteObj.Discard;
  ModifiedObj.Discard;

  assert(not dm_aniv_test1.BoldSystemHandle1.System.BoldDirty, 'Discarding objects failed. There are still dirty objects.');
  assert(dm_aniv_test1.BoldListHandle1.Count = 2, 'Discarding objects failed. Some objects were not deleted or undeleted.');
  assert(dm_aniv_test1.BoldListHandle1.List.Includes(DeleteObj), 'Discarding deleted object failed. Does not reappear in all objects list.');
  assert(DeleteObj.aString = OLD_STRING, 'Discarding deleted object failed. Wrong attribute value.');
  assert(ModifiedObj.aString = OLD_STRING, 'Discarding modified object failed. Wrong attribute value.');
end;

procedure TAniv_Discard.SetUp;
var
  i: Integer;
begin
  EnsureDM;
  while dm_aniv_test1.BoldListHandle1.Count > 2 do
    dm_aniv_test1.BoldListHandle1.CurrentBoldObject.Delete;
  while dm_aniv_test1.BoldListHandle1.Count < 2 do
    TClassA.Create(dm_aniv_test1.BoldSystemHandle1.System);
  for i := 0 to dm_aniv_test1.BoldListHandle1.Count-1 do
    (dm_aniv_test1.BoldListHandle1.List[i] as TClassA).aString := OLD_STRING;
  dm_aniv_test1.BoldSystemHandle1.UpdateDatabase;
end;

procedure TAniv_Discard.TearDown;
begin
  FreeDm;
end;

class procedure TAniv_Discard.Suit(ASuite: TBoldTestSuite);
begin
  ASuite.AddTest(CreateWithComment('Discard'));
end;

class function TAniv_Discard.Suite: ITestSuite;
begin
  Result := inherited Suite;
  SetCommentForTest(Result, 'Discard', '');
end;

{ TAniv_Links }

procedure TAniv_Links.DeleteFromInvalidOrdered;
var
  aSong: TSong;
  aHitlist: THitList;
begin
  while dm_aniv_test1.blhAllSongs.Count > 0 do
    dm_aniv_test1.blhAllSongs.CurrentBoldObject.Delete;
  while dm_aniv_test1.blhAllHitlists.Count > 0 do
    dm_aniv_test1.blhAllHitlists.CurrentBoldObject.Delete;
  aSong := TSong.Create(dm_aniv_test1.BoldSystemHandle1.System);
  aHitlist := THitList.Create(dm_aniv_test1.BoldSystemHandle1.System);
  aHitlist.song.Add(aSong);
  assert(aHitList.song.Count=1);
  assert(aSong.Hitlist.Count=1);
  assert(aSong.HitlistSong.Count=1);
  dm_aniv_test1.BoldSystemHandle1.UpdateDatabase;
  dm_aniv_test1.BoldSystemHandle1.Active := false;
  dm_aniv_test1.BoldSystemHandle1.Active := True;

  aSong := dm_aniv_test1.blhAllSongs.CurrentBoldObject as TSong;
  aHitlist := dm_aniv_test1.blhAllHitlists.CurrentBoldObject as THitlist;
  aSong.Delete;
  assert(aHitList.song.count = 0, 'Other end not empty');
end;

procedure TAniv_Links.ReassigningOneManyWithLinkClass;
var
  a1, a2, b: TThing;
begin
  ClearSystem(dm_aniv_test1.BoldSystemHandle1);

  a1 := TThing.Create(dm_aniv_test1.BoldSystemHandle1.System);
  a2 := TThing.Create(dm_aniv_test1.BoldSystemHandle1.System);
  b := TThing.Create(dm_aniv_test1.BoldSystemHandle1.System);

  a1.many.Add(b);
  a2.many.Add(b);
  a1.many.Add(b);

  assert(a1.many.count = 1, 'count of list with object');
  assert(a1.many[0] = b, 'content of list with object');
  assert(a2.many.count = 0, 'count of list without object');
  assert(b.one = a1, 'content of single link of object');
  assert(a1.manyLinkClass[0] = b.oneLinkClass, 'same link object');
  assert(b.oneLinkClass.many = b, 'link object many side');
  assert(b.onelinkclass.one = a1, 'link object one side');
  assert(dm_aniv_test1.BoldSystemHandle1.System.DirtyObjects.count = 4, 'dirty object count');

  ClearSystem(dm_aniv_test1.BoldSystemHandle1);
end;

procedure TAniv_Links.ReuseDeletedLinkObjectOnRelinkManyToManyOrdered;
var
  aSong1: TSong;
  aSong2: TSong;
  aHitlist: THitList;
begin
{$IFDEF ReuseDeletedLinkObjectOnRelink}
  while dm_aniv_test1.blhAllSongs.Count > 0 do
    dm_aniv_test1.blhAllSongs.CurrentBoldObject.Delete;
  while dm_aniv_test1.blhAllHitlists.Count > 0 do
    dm_aniv_test1.blhAllHitlists.CurrentBoldObject.Delete;
  dm_aniv_test1.BoldSystemHandle1.UpdateDatabase;
  aHitlist := THitList.Create(dm_aniv_test1.BoldSystemHandle1.System);
  aSong1 := TSong.Create(dm_aniv_test1.BoldSystemHandle1.System);
  aSong2 := TSong.Create(dm_aniv_test1.BoldSystemHandle1.System);
  aHitlist.song.Add(aSong1);
  aHitlist.song.Add(aSong2);
  dm_aniv_test1.BoldSystemHandle1.UpdateDatabase;
  aHitlist.M_Song.Clear;
  aHitlist.song.Add(aSong1);
  aHitlist.song.Add(aSong2);
  assert(not dm_aniv_test1.BoldSystemHandle1.System.BoldDirty);
  aHitlist.M_Song.Clear;
  aHitlist.song.Add(aSong2);
  aHitlist.song.Add(aSong1);
  assert(dm_aniv_test1.BoldSystemHandle1.System.BoldDirty);
  dm_aniv_test1.BoldSystemHandle1.UpdateDatabase;
  aHitlist.M_Song.Clear;
  aHitlist.song.Add(aSong1);
  aHitlist.song.Add(aSong2);  
  aHitlist.song.Move(1,0);
  assert(not dm_aniv_test1.BoldSystemHandle1.System.BoldDirty);
  dm_aniv_test1.BoldSystemHandle2.System.Discard;
  ClearSystem(dm_aniv_test1.BoldSystemHandle1);
{$ENDIF}
end;

procedure TAniv_Links.ReuseDeletedLinkObjectOnRelink1ToMany;
var
  Thing1, Thing2, Thing3: TThing;
begin
{$IFDEF ReuseDeletedLinkObjectOnRelink}
{
  while dm_aniv_test1.blhAllSongs.Count > 0 do
    dm_aniv_test1.blhAllSongs.CurrentBoldObject.Delete;
  while dm_aniv_test1.blhAllHitlists.Count > 0 do
    dm_aniv_test1.blhAllHitlists.CurrentBoldObject.Delete;
  dm_aniv_test1.BoldSystemHandle1.UpdateDatabase;
  Thing1 := TThing.Create(dm_aniv_test1.BoldSystemHandle1.System);
  Thing2 := TThing.Create(dm_aniv_test1.BoldSystemHandle1.System);
  Thing3 := TThing.Create(dm_aniv_test1.BoldSystemHandle1.System);
  Thing1.Many.Add(Thing1);
  Thing1.Many.Add(Thing2);
  Thing1.Many.Add(Thing3);
  dm_aniv_test1.BoldSystemHandle1.UpdateDatabase;
  Thing1.Many.Clear;
  Thing1.Many.Add(Thing1);
  Thing1.Many.Add(Thing2);
  Thing1.Many.Add(Thing3);
  assert(not dm_aniv_test1.BoldSystemHandle1.System.BoldDirty);
  dm_aniv_test1.BoldSystemHandle1.UpdateDatabase;
//  Thing2.Delete;
  Thing1.Many.Clear;
  Thing1.Many.Add(Thing3);
  Thing1.Many.Add(Thing2);
  assert(dm_aniv_test1.BoldSystemHandle1.System.BoldDirty);
  Thing1.Many.Add(Thing1);
  assert(not dm_aniv_test1.BoldSystemHandle1.System.BoldDirty);
  dm_aniv_test1.BoldSystemHandle1.UpdateDatabase;
  dm_aniv_test1.BoldSystemHandle2.System.Discard;
}
  ClearSystem(dm_aniv_test1.BoldSystemHandle1);

  Thing1 := TThing.Create(dm_aniv_test1.BoldSystemHandle1.System);
  Thing2 := TThing.Create(dm_aniv_test1.BoldSystemHandle1.System);
  Thing3 := TThing.Create(dm_aniv_test1.BoldSystemHandle1.System);
  Thing1.Many2.Add(Thing1);
  Thing1.Many2.Add(Thing2);
  Thing1.Many2.Add(Thing3);
  dm_aniv_test1.BoldSystemHandle1.UpdateDatabase;
  Thing1.Many2.Clear;
  Thing1.Many2.Add(Thing1);
  Thing1.Many2.Add(Thing2);
  Thing1.Many2.Add(Thing3);
  assert(not dm_aniv_test1.BoldSystemHandle1.System.BoldDirty);
  Thing1.Many2.Clear;
  Thing1.Many2.Add(Thing3);
  Thing1.Many2.Add(Thing2);
  Thing1.Many2.Add(Thing1);
  assert(dm_aniv_test1.BoldSystemHandle1.System.BoldDirty);
  Thing1.Many2.Move(0,2);
  Thing1.Many2.Move(0,1);
  assert(not dm_aniv_test1.BoldSystemHandle1.System.BoldDirty);
  ClearSystem(dm_aniv_test1.BoldSystemHandle1);
{$ENDIF}
end;

procedure TAniv_Links.SetUp;
begin
  inherited;
  EnsureDM;
end;

procedure TAniv_Links.TearDown;
begin
  FreeDm;
end;

class procedure TAniv_Links.Suit(ASuite: TBoldTestSuite);
begin
  ASuite.AddTest(CreateWithComment('SetSingleWithInvalidOrderedMulti'));
  ASuite.AddTest(CreateWithComment('DiscardDeletedLinkObject'));
  ASuite.AddTest(CreateWithComment('DeleteLinkObject'));
  ASuite.AddTest(CreateWithComment('DiscardSingle'));
  ASuite.AddTest(CreateWithComment('RollingBackLinks3'));
  ASuite.AddTest(CreateWithComment('RollingBackLinks2'));
  ASuite.AddTest(CreateWithComment('RollingBackLinks'));
  ASuite.AddTest(CreateWithComment('DeleteFromInvalidOrdered'));
  ASuite.AddTest(CreateWithComment('ReassigningOneManyWithLinkClass'));
  ASuite.AddTest(CreateWithComment('FetchSingleWithOrderedMulti'));
  ASuite.AddTest(CreateWithComment('ReuseDeletedLinkObjectOnRelinkManyToManyOrdered'));
  ASuite.AddTest(CreateWithComment('ReuseDeletedLinkObjectOnRelink1ToMany'));
end;

procedure TAniv_Links.FetchSingleWithOrderedMulti;
var
  aThing1, aThing2, aThing3: TThing;
  aThing12, aThing22, aThing32: TThing;
  id1, id2, id3: TBoldObjectId;
begin
  aThing1 := TThing.Create(dm_aniv_test1.BoldSystemHandle1.System);
  aThing2 := TThing.Create(dm_aniv_test1.BoldSystemHandle1.System);
  aThing3 := TThing.Create(dm_aniv_test1.BoldSystemHandle1.System);
  aThing3.many2.Add(aThing1);
  aThing2.one2 := aThing1;

  dm_aniv_test1.BoldSystemHandle1.UpdateDatabase;

  id1 := aThing1.BoldObjectLocator.BoldObjectId.Clone;
  id2 := aThing2.BoldObjectLocator.BoldObjectId.Clone;
  id3 := aThing3.BoldObjectLocator.BoldObjectId.Clone;

{  dm_aniv_test1.BoldSystemHandle1.Active := false;
  dm_aniv_test1.BoldSystemHandle1.Active := True;
  aThing1 := dm_aniv_test1.BoldSystemHandle1.System.EnsuredLocatorByID[id1].EnsuredBoldObject as TThing;
  aThing1.many2.Count;
}
  dm_aniv_test1.BoldSystemHandle2.Active := false;
  dm_aniv_test1.BoldSystemHandle2.Active := True;

  aThing12 := dm_aniv_test1.BoldSystemHandle2.System.EnsuredLocatorByID[id1].EnsuredBoldObject as TThing;
  aThing12.many2.Count;
  aThing32 := dm_aniv_test1.BoldSystemHandle2.System.EnsuredLocatorByID[id3].EnsuredBoldObject as TThing;
  aThing32.many2.Count;

  aThing2.one2 := aThing3;
  dm_aniv_test1.BoldSystemHandle1.UpdateDatabase;

  aThing22 := dm_aniv_test1.BoldSystemHandle2.System.EnsuredLocatorByID[id2].EnsuredBoldObject as TThing;
  aThing22.one2;

  assert(not aThing22.M_one2.BoldDirty);

  dm_aniv_test1.BoldSystemHandle2.System.Discard;
end;

type
  TBoldObjectListAccess = class(TBoldObjectList);
  TBoldSystemAccess = class(TBoldSystem);
  TBoldMemberAccess = class(TBoldMember);

procedure TAniv_Links.OldValuesAfterDelete;
var
  aSong1, aSong2: TSong;
  aHitlist: THitList;
  ahitListsong: ThitListsong;
begin
  aSong1 := TSong.Create(dm_aniv_test1.BoldSystemHandle1.System);
  aHitlist := THitList.Create(dm_aniv_test1.BoldSystemHandle1.System);
  dm_aniv_test1.BoldSystemHandle1.UpdateDatabase;
  Assert(not aSong1.BoldDirty);
  aHitlist.M_Song.Add(aSong1);
  ahitListsong := aHitlist.hitListsong[0];
  Assert(ahitListsong.BoldDirty);
  dm_aniv_test1.BoldSystemHandle1.UpdateDatabase;
  Assert(not aSong1.BoldDirty);
  Assert(not ahitListsong.BoldDirty);
  aSong1.Delete;
  Assert(aSong1.BoldDirty);
  Assert(ahitListsong.BoldDirty);
  Assert(dm_aniv_test1.BoldSystemHandle1.System.BoldDirty);
  dm_aniv_test1.BoldSystemHandle1.UpdateDatabase;
  Assert(TBoldSystemAccess(dm_aniv_test1.BoldSystemHandle1.System).OldValueHandler.IsEmpty);

  aSong1 := TSong.Create(dm_aniv_test1.BoldSystemHandle1.System);
  aSong2 := TSong.Create(dm_aniv_test1.BoldSystemHandle1.System);
  aHitlist.M_Song.Add(aSong1);
  aHitlist.M_Song.Add(aSong2);
  dm_aniv_test1.BoldSystemHandle1.UpdateDatabase;
  Assert(TBoldSystemAccess(dm_aniv_test1.BoldSystemHandle1.System).OldValueHandler.IsEmpty);
  aHitlist.M_Song.Clear;
  aHitlist.M_Song.Add(aSong2);
  Assert(not TBoldSystemAccess(dm_aniv_test1.BoldSystemHandle1.System).OldValueHandler.IsEmpty);  
  Assert(dm_aniv_test1.BoldSystemHandle1.System.BoldDirty);
  aHitlist.M_Song.Add(aSong1);
  Assert(dm_aniv_test1.BoldSystemHandle1.System.BoldDirty);  
  aHitlist.M_Song.Move(0,1);
  Assert(not dm_aniv_test1.BoldSystemHandle1.System.BoldDirty);
end;

procedure TAniv_Links.RollingBackLinks;
var
  AHitList: THitList;
  aSong: TSong;
  aSongId: string;
  aHitListId: String;
  i: integer;
begin
  // the test checks that after the rollback of a link it is sitll possible to do the operation again.
  ClearSystem(dm_aniv_test1.BoldSystemHandle1);
  AHitList := THitList.Create(dm_aniv_test1.BoldSystemHandle1.System);
  aHitlist.Song.AddNew;
  aHitlist.Song.AddNew;
  aHitlist.Song.AddNew;
  aHitlist.Song.AddNew;
  aHitlist.Song.AddNew;
  aSong := TSong.Create(dm_aniv_test1.BoldSystemHandle1.System);
  dm_aniv_test1.BoldSystemHandle1.UpdateDatabase;

  aHitlistId := aHitlist.BoldObjectLocator.BoldObjectId.AsString;
  aSongId := aSong.BoldObjectLocator.BoldObjectId.AsString;
  dm_aniv_test1.BoldSystemHandle1.Active := false;
  dm_aniv_test1.BoldSystemHandle1.Active := true;
  aSong := BoldRetrieveObjectByIdString(dm_aniv_test1.BoldSystemHandle1.System, ASongId) as TSong;
  aHitList := BoldRetrieveObjectByIdString(dm_aniv_test1.BoldSystemHandle1.System, aHitlistId) as THitList;

  dm_aniv_test1.BoldSystemHandle1.System.StartTransaction;
  aHitList.Song.Add(aSong);

  assert(aHitList.Song.Count = 6);
  assert(aSong.HitList.Count = 1);
  assert(aSong.HitListSong[0].Song = aSong);
  assert(aSong.HitListSong[0].HitList = aHitList);
  for i := 0 to 5 do
  begin
    assert(assigned(aHitList.HitListSong[i].song));
    assert(aHitList.HitListSong[i].HitList = aHitList);
  end;
  dm_aniv_test1.BoldSystemHandle1.System.RollBackTransaction;

  assert(aHitList.Song.Count = 5);

  dm_aniv_test1.BoldSystemHandle1.System.StartTransaction;
  aHitList.Song.Add(aSong);
  dm_aniv_test1.BoldSystemHandle1.System.CommitTransaction;
  assert(aHitList.Song.Count = 6);
  assert(aSong.HitList.Count = 1);
  assert(aSong.HitListSong[0].Song = aSong);
  assert(aSong.HitListSong[0].HitList = aHitList);
  for i := 0 to 5 do
  begin
    assert(assigned(aHitList.HitListSong[i].song));
    assert(aHitList.HitListSong[i].HitList = aHitList);
  end;
end;

procedure TAniv_Links.RollingBackLinks2;
var
  AHitList: THitList;
  aHitListId: String;
  i: integer;
begin
  // the test checks that it is possibel to rollback the fetching of a innerlink
  ClearSystem(dm_aniv_test1.BoldSystemHandle1);
  AHitList := THitList.Create(dm_aniv_test1.BoldSystemHandle1.System);
  aHitlist.Song.AddNew;
  aHitlist.Song.AddNew;
  dm_aniv_test1.BoldSystemHandle1.UpdateDatabase;

  aHitlistId := aHitlist.BoldObjectLocator.BoldObjectId.AsString;
  dm_aniv_test1.BoldSystemHandle1.Active := false;
  dm_aniv_test1.BoldSystemHandle1.Active := true;
  aHitList := BoldRetrieveObjectByIdString(dm_aniv_test1.BoldSystemHandle1.System, aHitlistId) as THitList;


  // first we load the innerlinks of the HitListSong-objects, and then roll the fetch back
  dm_aniv_test1.BoldSystemHandle1.System.StartTransaction;

    assert(aHitList.Song.Count = 2);
    for i := 0 to 1 do
    begin
      assert(assigned(aHitList.HitListSong[i].song));
      assert(aHitList.HitListSong[i].HitList = aHitList);
    end;

  dm_aniv_test1.BoldSystemHandle1.System.RollBackTransaction;

  // now lets make sure we are allowed to fetch the innerlinks again
  assert(aHitList.Song.Count = 2);
  for i := 0 to 1 do
  begin
    assert(assigned(aHitList.HitListSong[i].song));
    assert(aHitList.HitListSong[i].HitList = aHitList);
  end;


end;

procedure TAniv_Links.RollingBackLinks3;
var
  MasterDocument: TDocument;
  MD_id: string;
begin
  // the test checks that it is possibel to rollback the fetching of a singlelink when the multilink is already loaded
  ClearSystem(dm_aniv_test1.BoldSystemHandle1);
  MasterDocument := TDocument.Create(dm_aniv_test1.BoldSystemHandle1.System);
  MasterDocument.Parts.AddNew;
  dm_aniv_test1.BoldSystemHandle1.UpdateDatabase;

  MD_id := MasterDocument.BoldObjectLocator.BoldObjectId.AsString;
  dm_aniv_test1.BoldSystemHandle1.Active := false;
  dm_aniv_test1.BoldSystemHandle1.Active := true;
  MasterDocument := BoldRetrieveObjectByIdString(dm_aniv_test1.BoldSystemHandle1.System, MD_id) as TDocument;


  // first we load the Multilöink, then the singlelink and roll it back

  assert(MasterDocument.Parts.Count = 1);

  dm_aniv_test1.BoldSystemHandle1.System.StartTransaction;
  assert(MasterDocument.Parts[0].PartOf = MasterDocument);
  dm_aniv_test1.BoldSystemHandle1.System.RollBackTransaction;

  // now lets make sure we are allowed to fetch the single links again
  assert(MasterDocument.Parts.Count = 1);
  assert(MasterDocument.Parts[0].PartOf = MasterDocument);
end;

procedure TAniv_Links.DiscardSingle;
var
  Obj1, Obj2: TClassA;
  Subscriber: TBoldPassthroughSubscriber;
begin
  Obj1 := TClassA.Create(dm_aniv_test1.BoldSystemHandle1.System);
  Obj2 := TClassA.Create(dm_aniv_test1.BoldSystemHandle1.System);
  dm_aniv_test1.BoldSystemHandle1.UpdateDatabase;
  Obj1.parent := Obj2;
  Subscriber := TBoldPassthroughSubscriber.Create(_ReceiveModify);
  Obj2.M_child.AddSubscription(Subscriber, beCompleteModify, beCompleteModify);
  fModifyEventReceived := false;
  Obj1.Discard;
  Assert(not fModifyEventReceived);

  Subscriber.Free;
end;

procedure TAniv_Links._ReceiveModify(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  assert(OriginalEvent = beCompleteModify);
  assert(RequestedEvent = beCompleteModify);
  fModifyEventReceived := true;
end;

procedure TAniv_Links.DeleteLinkObject;
var
  anObj: TClassA;
begin
  anObj := TClassA.Create(dm_aniv_test1.BoldSystemHandle1.System);
  anObj.part.Add(anObj);
  anObj.partpartpartof[0].Delete;
  assert(anObj.part.Count = 0);
  assert(anObj.partof.Count = 0);
end;

procedure TAniv_Links.DeleteObjectTest;
var
  anObj: TClassA;
  Id: TBoldObjectId;
begin
  anObj := TClassA.Create(dm_aniv_test1.BoldSystemHandle1.System);
  Id := anObj.BoldObjectLocator.BoldObjectID.Clone;
  anObj.Delete;
  Assert(dm_aniv_test1.BoldSystemHandle1.System.Locators.LocatorByID[Id] = nil);
  id.free;
  anObj := TClassA.Create(dm_aniv_test1.BoldSystemHandle1.System);
  dm_aniv_test1.BoldSystemHandle1.UpdateDatabase;
  Id := anObj.BoldObjectLocator.BoldObjectID.Clone;
{$IFDEF CompareToOldValues}
  anObj.aTime := now;
{$ELSE}
  anObj.MarkObjectDirty;
{$ENDIF}
  assert(anObj.BoldDirty);
  anObj.Delete;
  Assert(dm_aniv_test1.BoldSystemHandle1.System.Locators.LocatorByID[Id].BoldObject = anObj);
  assert(anObj.BoldObjectIsDeleted);
  dm_aniv_test1.BoldSystemHandle1.UpdateDatabase;
  Assert(dm_aniv_test1.BoldSystemHandle1.System.Locators.LocatorByID[Id] = nil);
end;

procedure TAniv_Links.DiscardDeletedLinkObject;
var
  Obj1, Obj2: TClassA;
  LinkObj: partpartof;
begin
  Obj1 := TClassA.Create(dm_aniv_test1.BoldSystemHandle1.System);
  Obj2 := TClassA.Create(dm_aniv_test1.BoldSystemHandle1.System);
  Obj1.part.Add(Obj2);
  LinkObj := Obj1.partpartpartof[0];
  dm_aniv_test1.BoldSystemHandle1.UpdateDatabase;

  Obj1.part.Remove(Obj2);
  assert(LinkObj.M_partof.BoldDirty);
  assert(LinkObj.M_part.BoldDirty);
  assert(LinkObj.partof = nil);
  assert(LinkObj.part = nil);

  dm_aniv_test1.BoldSystemHandle1.System.Discard;
  assert(LinkObj.M_partof.BoldPersistenceState = bvpsInvalid);
  assert(LinkObj.M_part.BoldPersistenceState = bvpsInvalid);

  LinkObj.partof;

  assert(Obj1.part.Count = 1);
  assert(Obj1.part[0] = Obj2);
  assert(Obj1.partpartpartof.Count = 1);
  assert(Obj1.partpartpartof[0] = LinkObj);
  assert(Obj2.partof.Count = 1);
  assert(Obj2.partof[0] = Obj1);
  assert(Obj2.partofpartpartof.Count = 1);
  assert(Obj2.partofpartpartof[0] = LinkObj);
end;

procedure TAniv_Links.SetSingleWithInvalidOrderedMulti;
var
  Thing1, Thing2: TThing;
begin
  Thing1 := TThing.Create(dm_aniv_test1.BoldSystemHandle1.System);
  dm_aniv_test1.BoldSystemHandle1.UpdateDatabase;
  Thing1.M_many2.Invalidate;
  Thing2 := TThing.Create(dm_aniv_test1.BoldSystemHandle1.System);
  Thing2.one2 := Thing1;
end;

class function TAniv_Links.Suite: ITestSuite;
begin
  Result := inherited Suite;
  SetCommentForTest(Result, 'SetSingleWithInvalidOrderedMulti', '');
  SetCommentForTest(Result, 'DiscardDeletedLinkObject', '');
  SetCommentForTest(Result, 'DeleteLinkObject', '');
  SetCommentForTest(Result, 'DiscardSingle', '');
  SetCommentForTest(Result, 'RollingBackLinks3', '');
  SetCommentForTest(Result, 'RollingBackLinks2', '');
  SetCommentForTest(Result, 'RollingBackLinks', '');
  SetCommentForTest(Result, 'DeleteFromInvalidOrdered', '');
  SetCommentForTest(Result, 'ReassigningOneManyWithLinkClass', '');
  SetCommentForTest(Result, 'FetchSingleWithOrderedMulti', '');
end;

{ TAniv_ObjectList }

procedure TAniv_ObjectList.ClearWithDestroyedObjects;
var
  anObjectList: TBoldObjectList;
  i: integer;
begin
  ClearSystem(dm_aniv_test1.BoldSystemHandle1);
  anObjectList := TBoldObjectList.Create;
  anObjectList.SubscribeToObjectsInList := false;
  anObjectList.SubscribeToLocatorsInList := True;
  for i := 0 to 15 do
    anObjectList.Add(TClassA.Create(dm_aniv_test1.BoldSystemHandle1.System));
  dm_aniv_test1.BoldSystemHandle1.UpdateDatabase;
  for i := anObjectList.Count-1 downto 0 do
    anObjectList[i].Delete;
  dm_aniv_test1.BoldSystemHandle1.UpdateDatabase;
  anObjectList.Clear;
  anObjectList.Free;
end;

procedure TAniv_ObjectList.SetUp;
begin
  inherited;
  EnsureDM;
end;

procedure TAniv_ObjectList.TearDown;
begin
  FreeDm;
end;

class procedure TAniv_ObjectList.Suit(ASuite: TBoldTestSuite);
begin
  ASuite.AddTest(CreateWithComment('ClearWithDestroyedObjects'));
end;

class function TAniv_ObjectList.Suite: ITestSuite;
begin
  Result := inherited Suite;
  SetCommentForTest(Result, 'ClearWithDestroyedObjects', '');
end;

{ TAniv_PrioQueue }

class procedure TAniv_PrioQueue.Suit(ASuite: TBoldTestSuite);
begin
//  ASuite.AddTest(CreateWithComment('ClearWithDestroyedObjects'));
end;


{ TAniv_Various }

procedure TAniv_Various.SetUp;
begin
  inherited;
  EnsureDM;

end;

class procedure TAniv_Various.Suit(ASuite: TBoldTestSuite);
begin
  ASuite.AddTest(CreateWithComment('TransientAttributeWithInitialValue'));
  ASuite.AddTest(CreateWithComment('TestManipulator'));
end;

class function TAniv_Various.Suite: ITestSuite;
begin
  Result := inherited Suite;
  SetCommentForTest(Result, 'TransientAttributeWithInitialValue', '');
  SetCommentForTest(Result, 'TestManipulator', '');
end;

procedure TAniv_Various.TearDown;
begin
  FreeDM;
end;

procedure TAniv_Various.TestManipulator;
var
  aThing: TThing;
  aClassA: TClassA;
  IdStr: string;
begin
  EnsureDM;
  aThing := TThing.Create(dm_aniv_test1.BoldSystemHandle1.System, true);
  dm_aniv_test1.BoldSystemHandle1.UpdateDatabase;
  idStr := dm_aniv_test1.BoldManipulator1.IdStringForElement(aThing);
  assert(dm_aniv_test1.BoldManipulator1.ElementForIdString(IdStr) = aThing, 'conversion of persistent objects');

  idStr := dm_aniv_test1.BoldManipulator1.IdStringForElement(aThing.M_one);
  assert(dm_aniv_test1.BoldManipulator1.ElementForIdString(IdStr) = aThing.M_One, 'conversion of persistent objects members');

  IdStr := dm_aniv_test1.BoldManipulator1.CreateObject('ClassA');
  assert(dm_aniv_test1.BoldManipulator1.ElementForIdString(IdStr) is TClassA, 'conversion of nonsaved persistent objects');
  aClassA := dm_aniv_test1.BoldManipulator1.ElementForIdString(IdStr) as TClassA;
  assert(aClassA.BoldObjectIsNew);
end;

procedure TAniv_Various.TransientAttributeWithInitialValue;
{$IFNDEF NoTransientInstancesOfPersistentClass}
var
  aThing: TThing;
{$ENDIF}  
begin
  EnsureDM;
  {$IFNDEF NoTransientInstancesOfPersistentClass}
  aThing := TThing.Create(dm_aniv_test1.BoldSystemHandle1.System, false);
  assert(not aThing.BoldDirty, 'Object is dirty');
  assert(not aThing.M_aTransient.BoldDirty, 'Attribute is dirty');
  assert(dm_aniv_test1.BoldSystemHandle1.System.DirtyObjects.Count = 0, 'System has dirty objects');
  {$ENDIF}
end;

{ TAniv_ParentChild }

procedure TAniv_ParentChild.NonEmbeddedChildMappedAssocs;
begin
  TChild1.Create(dm_aniv_test1.BoldSystemHandle1.System).own := TOwn.Create(dm_aniv_test1.BoldSystemHandle1.System);
  dm_aniv_test1.BoldSystemHandle1.UpdateDatabase;
  dm_aniv_test1.BoldSystemHandle1.Active := false;
  dm_aniv_test1.BoldSystemHandle1.Active := true;
  assert((dm_aniv_test1.BoldSystemHandle1.System.ClassByExpressionName['Own'].BoldObjects[0] as TOwn).ChildMapped.Count = 1, 'Non embedded child mapped association didn''t work');
end;

procedure TAniv_ParentChild.SetUp;
begin
  inherited;
  EnsureDM;
  EmptyDb(dm_aniv_test1.BoldSystemHandle1.System);
end;

procedure TAniv_ParentChild.TearDown;
begin
  FreeDm;
end;


class procedure TAniv_ParentChild.Suit(ASuite: TBoldTestSuite);
begin
  ASuite.AddTest(CreateWithComment('NonEmbeddedChildMappedAssocs'));
end;

class function TAniv_ParentChild.Suite: ITestSuite;
begin
  Result := inherited Suite;
  SetCommentForTest(Result, 'NonEmbeddedChildMappedAssocs', '');
end;

{ TAniv_SingleSingleEmbeddedEmbeddedAssoc }

procedure TAniv_SingleSingleEmbeddedEmbeddedAssoc.Fetch;
var
  obj1, Obj2: TClassA;
begin
  dm_aniv_test1.BoldSystemHandle3.Active := false;
  dm_aniv_test1.BoldSystemHandle3.PersistenceHandle := dm_aniv_test1.BoldPersistenceHandleFileXML1;

  // setup of objects
  dm_aniv_test1.BoldSystemHandle3.Active := true;
  EmptyDb(dm_aniv_test1.BoldSystemHandle3.System);
  Obj1 := TClassA.Create(dm_aniv_test1.BoldSystemHandle3.System);
  Obj2 := TClassA.Create(dm_aniv_test1.BoldSystemHandle3.System);
  Obj1.Next := Obj2;
  dm_aniv_test1.BoldSystemHandle3.UpdateDatabase;
  dm_aniv_test1.BoldSystemHandle3.Active := false;

  // actual test
  dm_aniv_test1.BoldSystemHandle3.Active := true;

  dm_aniv_test1.BoldSystemHandle3.System.ClassByExpressionName['ClassA'].EnsureObjects;
  obj1 := dm_aniv_test1.BoldSystemHandle3.System.ClassByExpressionName['ClassA'].BoldObjects[0] as TClassA;
  obj2 := dm_aniv_test1.BoldSystemHandle3.System.ClassByExpressionName['ClassA'].BoldObjects[1] as TClassA;
  Obj1.M_next.EnsureContentsCurrent;
  Obj2.M_next.EnsureContentsCurrent;
  Obj1.M_Previous.EnsureContentsCurrent;
  Obj2.M_Previous.EnsureContentsCurrent;
end;


procedure TAniv_SingleSingleEmbeddedEmbeddedAssoc.SetUp;
begin
  inherited;
  EnsureDM;
end;

procedure TAniv_SingleSingleEmbeddedEmbeddedAssoc.TearDown;
begin
  FreeDm;
end;

class procedure TAniv_SingleSingleEmbeddedEmbeddedAssoc.Suit(
  ASuite: TBoldTestSuite);
begin
  ASuite.AddTest(CreateWithComment('Fetch'));
end;

class function TAniv_SingleSingleEmbeddedEmbeddedAssoc.Suite: ITestSuite;
begin
  Result := inherited Suite;
  SetCommentForTest(Result, 'Fetch', '');
end;

{ TAniv_ClassListTests }

procedure TAniv_ClassListTests.DeletedObjectsInMemory;
var
  obj1: TParentMapped1;
  ObjLIst: TBoldObjectList;
begin
  dm_aniv_test1.BoldSystemHandle1.Active := true;
  EmptyDb(dm_aniv_test1.BoldSystemHandle1.System);
  Obj1 := TParentMapped1.Create(dm_aniv_test1.BoldSystemHandle1.System);
  Obj1 := TParentMapped1.Create(dm_aniv_test1.BoldSystemHandle1.System);
  Obj1 := TParentMapped1.Create(dm_aniv_test1.BoldSystemHandle1.System);
  dm_aniv_test1.BoldSystemHandle1.UpdateDatabase;
  dm_aniv_test1.BoldSystemHandle1.Active := false;
  dm_aniv_test1.BoldSystemHandle1.Active := true;
  ObjList := dm_aniv_test1.BoldSystemHandle1.System.ClassByExpressionName['ParentMapped1'];
  assert( objList.Count = 3);
  ObjList[0].Delete;
  ObjList := dm_aniv_test1.BoldSystemHandle1.System.ClassByExpressionName['Parent'];
  assert( objList.Count = 2);
  dm_aniv_test1.BoldSystemHandle1.UpdateDatabase;
  dm_aniv_test1.BoldSystemHandle1.Active := false;
  dm_aniv_test1.BoldSystemHandle1.Active := true;

  ObjList := dm_aniv_test1.BoldSystemHandle1.System.ClassByExpressionName['Parent'];
  assert( objList.Count = 2);
  ObjList[0].Delete;
  ObjList := dm_aniv_test1.BoldSystemHandle1.System.ClassByExpressionName['ParentMapped1'];
  assert( objList.Count = 1);
  dm_aniv_test1.BoldSystemHandle1.UpdateDatabase;
end;

procedure TAniv_ClassListTests.SetUp;
begin
  inherited;
  EnsureDM;
end;

procedure TAniv_ClassListTests.TearDown;
begin
  FreeDm;
end;


class procedure TAniv_ClassListTests.Suit(ASuite: TBoldTestSuite);
begin
  inherited;
  ASuite.AddTest(CreateWithComment('DeletedObjectsInMemory'));
end;

class function TAniv_ClassListTests.Suite: ITestSuite;
begin
  Result := inherited Suite;
  SetCommentForTest(Result, 'DeletedObjectsInMemory', '');
end;

{ TAniv_FilePersistenceMapper }

procedure TAniv_FilePersistenceMapper.Fetch;
begin

end;

procedure TAniv_FilePersistenceMapper.SetUp;
begin
  inherited;
  EnsureDM;
  dm_aniv_test1.BoldSystemHandle3.PersistenceHandle := dm_aniv_test1.BoldPersistenceHandleFileXML1;
end;

procedure TAniv_FilePersistenceMapper.TearDown;
begin
  FreeDm;
end;

class procedure TAniv_FilePersistenceMapper.Suit(ASuite: TBoldTestSuite);
begin
  ASuite.AddTest(CreateWithComment('WriteAllOnUpdateNew'));
  ASuite.AddTest(CreateWithComment('WriteWithUnfetchedMembers'));
end;

procedure TAniv_FilePersistenceMapper.WriteAllOnUpdateNew;
var
  anObj: TBoldObject;
  i: Integer;
begin
  dm_aniv_test1.BoldSystemHandle3.Active := true;
  EmptyDb(dm_aniv_test1.BoldSystemHandle3.System);

  TClassA.Create(dm_aniv_test1.BoldSystemHandle3.System);
  dm_aniv_test1.BoldSystemHandle3.UpdateDatabase;
  dm_aniv_test1.BoldSystemHandle3.Active := false;
  dm_aniv_test1.BoldSystemHandle3.Active := true;
  anObj := dm_aniv_test1.BoldSystemHandle3.System.ClassByExpressionName['ClassA'].BoldObjects[0];
  for i := 0 to anObj.BoldMemberCount-1 do
    anObj.BoldMembers[i].EnsureContentsCurrent;
end;

procedure TAniv_FilePersistenceMapper.WriteWithUnfetchedMembers;
var
  Obj1, Obj2: TClassA;
begin
  dm_aniv_test1.BoldSystemHandle3.Active := true;
  EmptyDb(dm_aniv_test1.BoldSystemHandle3.System);

  Obj1 := TClassA.Create(dm_aniv_test1.BoldSystemHandle3.System);
  Obj2 := TClassA.Create(dm_aniv_test1.BoldSystemHandle3.System);
  Obj1.parent := Obj2;
  dm_aniv_test1.BoldSystemHandle3.UpdateDatabase;
  dm_aniv_test1.BoldSystemHandle3.Active := false;
  dm_aniv_test1.BoldSystemHandle3.Active := true;

  Obj1 := dm_aniv_test1.BoldSystemHandle3.System.ClassByExpressionName['ClassA'].BoldObjects[0] as TClassA;
  Obj2 := dm_aniv_test1.BoldSystemHandle3.System.ClassByExpressionName['ClassA'].BoldObjects[1] as TClassA;
  Obj1.aString := 'whatever';
  Obj2.aString := 'whatever';
  Obj1.M_previous.Invalidate; //last member in object (highest index)
  Obj2.M_previous.Invalidate;

  dm_aniv_test1.BoldSystemHandle3.UpdateDatabase;
  dm_aniv_test1.BoldSystemHandle3.Active := false;
  dm_aniv_test1.BoldSystemHandle3.Active := true;

  Assert(dm_aniv_test1.BoldSystemHandle3.System.ClassByExpressionName['ClassA'].Count = 2);

  Obj1 := dm_aniv_test1.BoldSystemHandle3.System.ClassByExpressionName['ClassA'].BoldObjects[0] as TClassA;
  Obj2 := dm_aniv_test1.BoldSystemHandle3.System.ClassByExpressionName['ClassA'].BoldObjects[1] as TClassA;

  if not assigned(Obj1.parent) then
  begin
    Obj2 := dm_aniv_test1.BoldSystemHandle3.System.ClassByExpressionName['ClassA'].BoldObjects[0] as TClassA;
    Obj1 := dm_aniv_test1.BoldSystemHandle3.System.ClassByExpressionName['ClassA'].BoldObjects[1] as TClassA;
  end;

  assert(Obj1.parent = obj2, 'single link still assigned');
  assert(Obj2.child.Count = 1, 'multi link count');
  assert(Obj2.child[0] = Obj1, 'multi link ok');

end;

class function TAniv_FilePersistenceMapper.Suite: ITestSuite;
begin
  Result := inherited Suite;
  SetCommentForTest(Result, 'WriteAllOnUpdateNew', '');
  SetCommentForTest(Result, 'WriteWithUnfetchedMembers', '');
end;

{ TAniv_fmDistributable }

procedure TAniv_fmDistributable.Fetch;
var
  anObj: TBoldObject;
  idList: TBoldObjectIdList;
begin
  anObj := TClassA.Create(dm_aniv_test1.BoldSystemHandle1.System);
  dm_aniv_test1.BoldSystemHandle1.UpdateDatabase;

  IdList := TBoldObjectIdList.Create;
  IdList.Add(anObj.BoldObjectLocator.BoldObjectID);

  dm_aniv_test1.BoldPersistenceHandleDB1.PersistenceController.PMFetch(IdList,
    dm_aniv_test1.BoldSystemHandle1.System.AsIBoldvalueSpace[bdepPMIn], nil, fmDistributable, -1);
  FreeAndNil(IdList);  
end;

procedure TAniv_fmDistributable.SetUp;
begin
  inherited;
  EnsureDM;
end;

class procedure TAniv_fmDistributable.Suit(ASuite: TBoldTestSuite);
begin
  ASuite.AddTest(CreateWithComment('TimestampCondition'));
  ASuite.AddTest(CreateWithComment('Fetch'));
end;

class function TAniv_fmDistributable.Suite: ITestSuite;
begin
  Result := inherited Suite;
  SetCommentForTest(Result, 'TimestampCondition', '');
  SetCommentForTest(Result, 'Fetch', '');
end;

procedure TAniv_fmDistributable.TearDown;
begin
  inherited;
  FreeDM;
end;

procedure TAniv_fmDistributable.TimestampCondition;
var
  aCond: TBoldTimestampCondition;
  aList: TBoldObjectList;
begin
  aCond := TBoldTimestampCondition.create;
  aList := TBoldObjectList.Create;
  aCond.TopSortedIndex := 0;
  aCond.Timestamp := 0;
  dm_aniv_test1.BoldSystemHandle1.System.GetAllWithCondition(aList, aCond);
  aCond.Free;
  aList.Free;
end;

class function TAniv_Transaction1.Suite: ITestSuite;
begin
  Result := inherited Suite;
  SetCommentForTest(Result, 'RollbackAttributes', '');
  SetCommentForTest(Result, 'RollbackObjectCreate', '');
  SetCommentForTest(Result, 'RollbackObjectDelete', '');
  SetCommentForTest(Result, 'RollbackAssociation', '');
  SetCommentForTest(Result, 'RollbackAssociation2', '');
end;

{ TAniv_XMI }

procedure TAniv_XMI.ExportAndImport;
var
  aLink: TBoldUMLXMILink;
  aModel: TUMLModel;
  aClass: TUMLClass;
  anAttribute: TUMLAttribute;

begin
  EnsureModelEditDataModule;
  aLink := TBoldUMLXMILink.Create(nil);
  aLink.FileName := 'test.xmi';

  aModel := TUMLModel.Create(dmModelEdit.bshUMLModel.System);
  aClass := TUMLClass.Create(dmModelEdit.bshUMLModel.System);
  aModel.ownedElement.Add(aClass);
  aClass.name := 'TestClass';
  aClass.isActive := true;
  aClass.isRoot := false;
  aClass.isLeaf := true;
  aClass.isAbstract := false;
  aClass.isSpecification := true;
  aClass.stereotypeName := 'foo';
  aClass.SetTaggedValue('SomeTag', 'SomeValue');
  anAttribute := TUMLAttribute.Create(dmModelEdit.bshUMLModel.System);
  aClass.feature.Add(anAttribute);
  anAttribute.name := 'TestAttribute';
  anAttribute.initialValue := '17';
  anAttribute.multiplicity := '2..4, 7..*';
  anAttribute.changeability := ckAddOnly;
  anAttribute.typeName := 'SomeType';
  anAttribute.targetScope := skClassifier;
  anAttribute.ownerScope := skInstance;
  anAttribute.visibility := vkProtected;

  aLink.ExportModel(aModel);
  aModel := TUMLModel.Create(dmModelEdit.bshUMLModel.System);
  aLink.ImportModel(aModel);
  assert(aModel.classes.Count = 1);
  aClass := aModel.classes[0];
  assert(aClass.name = 'TestClass');
  assert(aClass.isActive);
  assert(not aClass.isRoot);
  assert(aClass.isLeaf);
  assert(not aClass.isAbstract);
  assert(aClass.isSpecification);
  assert(aClass.stereotypeName = 'foo');
  assert(assigned(aClass.taggedValue['SomeTag']));
  assert(aClass.taggedValue['SomeTag'].value = 'SomeValue');
  assert(aClass.feature.Count = 1);
  assert(aClass.feature[0] is TUMLAttribute);
  anAttribute := aClass.feature[0] as TUMLAttribute;
  assert(anAttribute.name = 'TestAttribute');
  assert(anAttribute.initialValue = '17');
  assert(anAttribute.multiplicity = '2..4, 7..*');
  assert(anAttribute.changeability = ckAddOnly);
  assert(anAttribute.typeName = 'SomeType');
  assert(anAttribute.targetScope = skClassifier);
  assert(anAttribute.ownerScope = skInstance);
  assert(anAttribute.visibility = vkProtected);
end;

procedure TAniv_XMI.ExportAndImport2;
var
  aModel: TUMLModel;
  aSystem: TBoldSystem;
begin
  EnsureDM;

  dm_aniv_test1.BoldUMLXMILink1.ExportModel(dm_aniv_test1.BoldModel1.EnsuredUMLModel);

  aSystem := TBoldSystem.CreateWithTypeInfo(nil,
    dmModelEdit.BoldSystemTypeInfoHandle1.StaticSystemTypeInfo, nil);
  aModel := TUMLModel.Create(aSystem);
  dm_aniv_test1.BoldUMLXMILink1.ImportModel(aModel);


  FreeDM;
end;

class procedure TAniv_XMI.Suit(ASuite: TBoldTestSuite);
begin
  ASuite.AddTest(CreateWithComment('ExportAndImport2'));
  ASuite.AddTest(CreateWithComment('ExportAndImport'));
end;

class function TAniv_XMI.Suite: ITestSuite;
begin
  Result := inherited Suite;
  SetCommentForTest(Result, 'ExportAndImport2', '');
  SetCommentForTest(Result, 'ExportAndImport', '');
end;

{ TAniv_1_ListTraverser }

procedure TAniv_1_ListTraverser.SetUp;
begin
  inherited;

end;

class procedure TAniv_1_ListTraverser.Suit(ASuite: TBoldTestSuite);
begin
  ASuite.AddTest(CreateWithComment('TestTraverser'));
end;

class function TAniv_1_ListTraverser.Suite: ITestSuite;
begin
  Result := inherited Suite;
  SetCommentForTest(Result, 'TestTraverser', '');
end;

procedure TAniv_1_ListTraverser.TearDown;
begin
  FreeDM;
end;

procedure TAniv_1_ListTraverser.TestTraverser;
var
  List: TBoldUnOrderedIndexableList;
  Traverser: TBoldIndexableListTraverser;
  Traverser2: TBoldIndexableListTraverser;
  i: integer;
  o1,o2,o3,o4: TClassA;
begin
  EnsureDM;
  List := dm_aniv_test1.BoldSystemHandle1.System.Locators;
  Traverser2 := List.CreateTraverser;
  o1 := TClassA.Create(dm_aniv_test1.BoldSystemHandle1.System);
  o2 := TClassA.Create(dm_aniv_test1.BoldSystemHandle1.System);
  o3 := TClassA.Create(dm_aniv_test1.BoldSystemHandle1.System);
  Traverser := List.CreateTraverser;
  Traverser.AutoMoveOnRemoveCurrent := true;
  Traverser2.AutoMoveOnRemoveCurrent := false;
  try
    Check(Traverser2.MoveNext);
    Check(Traverser.MoveNext);
    Check(Traverser.Item = o1.BoldObjectLocator);
    Check(Traverser.MoveNext);
    Check(Traverser.Item = o2.BoldObjectLocator);
    o2.Delete;
    Check(Traverser.Item = o3.BoldObjectLocator);
    Check(not Traverser.MoveNext);
    Check(Traverser.Item = nil);
    Check(Traverser2.MoveNext);
    Check(Traverser2.Item = o3.BoldObjectLocator);
    o4 := TClassA.Create(dm_aniv_test1.BoldSystemHandle1.System);
    Check(Traverser2.MoveNext);
    Check(Traverser2.Item = o4.BoldObjectLocator);
    Check(Traverser.Item = nil);
  finally
    Traverser.Free;
    Traverser2.Free;
  end;
end;

initialization
  TestGlobal.RegisterTestCase(TAniv_1_ListTraverser);
  TestGlobal.RegisterTestCase(TAniv_Transaction1);
  TestGlobal.RegisterTestCase(TAniv_AssocWithLinkClass);
  TestGlobal.RegisterTestCase(TAniv_AutomaticRemovalOfObjectFromLists);
  TestGlobal.RegisterTestCase(TAniv_InvalidateMembers);
  TestGlobal.RegisterTestCase(TAniv_Discard);
  TestGlobal.RegisterTestCase(TAniv_Links);
  TestGlobal.RegisterTestCase(TAniv_ObjectList);
//  TestGlobal.RegisterTestCase(TAniv_PrioQueue);
  TestGlobal.RegisterTestCase(TAniv_Various);
  TestGlobal.RegisterTestCase(TAniv_SingleSingleEmbeddedEmbeddedAssoc);
  TestGlobal.RegisterTestCase(TAniv_ParentChild);
  TestGlobal.RegisterTestCase(TAniv_ClassListTests);
  TestGlobal.RegisterTestCase(TAniv_FilePersistenceMapper);
  TestGlobal.RegisterTestCase(TAniv_fmDistributable);
  TestGlobal.RegisterTestCase(TAniv_XMI);

end.
