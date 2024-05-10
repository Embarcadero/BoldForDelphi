
unit joho_optimistsicLocking;

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
  ActnList,
  TestModel1_OptimisticLocking,
  dmModel1,
  BoldHandleAction,
  BoldActions,
  BoldUMLModelLink,
  BoldUMLRose98Link,
  BoldHandle,
  BoldPersistenceHandle,
  BoldPersistenceHandleDB,
  BoldModel,
  BoldHandles,
  BoldSubscription,
  BoldSystem,
  BoldSystemHandle,
  BoldDefs,
  BoldTaggedValueSupport,
  TestSuite,
  BoldDBActions,
  TestFrameWork,
  BoldAbstractModel, BoldAbstractPersistenceHandleDB,
  DB,
  BoldAbstractDatabaseAdapter, System.Actions, DBAccess, Uni, BoldDatabaseAdapterUniDAC;

type
  TdmjohoOptimisticLocking = class(TDataModule)
    BoldSystemHandle1: TBoldSystemHandle;
    ActionList1: TActionList;
    BoldSystemHandle2: TBoldSystemHandle;
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
    BoldModel1: TBoldModel;
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
    BoldDatabaseAdapterUniDAC1: TBoldDatabaseAdapterUniDAC;
    UniConnection1: TUniConnection;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TLocalTestCase = class(TBoldTestCase)
  public
    procedure SetUp; override;
    procedure TearDown; override;
    procedure DiscardAndCloseSystems;
  end;

  Tjoho_OptimisticLocking = class(TLocalTestCase)
  private
  public
    procedure DeleteAllObjects;
    procedure ResetDataBase;
    class procedure Suit(aSuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
  published
    procedure ChangeAttribute;
    procedure ChangeSingleLink;
    procedure DeleteObject;
    procedure DeleteDeletedObject;
    procedure SingleSingleEmbeddNonEmbedd;
    procedure UnlinkSingleSinglelinks;
    procedure DeleteChangedObject;
    procedure ChangeAndChangeBack;
  end;


var
  dmjohoOptimisticLocking: TdmjohoOptimisticLocking = nil;

implementation

var
  // This construct is used to protect the EnsureDataModule from having to be rewritten.
  TheDataModule: TdmjohoOptimisticLocking absolute dmjohoOptimisticLocking;
  DoneSetup: boolean = false;

{$R *.DFM}

procedure EnsureDataModule;
begin
  Ensuredm_Model;
  if not assigned( TheDataModule ) then
  begin
    theDataModule := TdmjohoOptimisticLocking.Create(Application);
    theDataModule.BoldDatabaseAdapterUniDac1.CreateDatabase;
    theDataModule.BoldPersistenceHandleDB1.CreateDataBaseSchema;
  end;
end;

procedure FreeDataModule;
begin
  FreeAndNil(TheDataModule);
end;

procedure EnsureSetup;
begin
  if DoneSetup then Exit;

  EnsureDataModule;

  // Your global setup code here

  DoneSetup := True;
end;

{ TLocalTestCase }

procedure TLocalTestCase.SetUp;
begin
  inherited;
  EnsureSetup;
  DoneSetup := False;
end;

procedure TLocalTestCase.TearDown;
begin
  DiscardAndCloseSystems;
  FreeDataModule;
end;

{ TYourTestCaseHere }

procedure Tjoho_OptimisticLocking.ChangeAttribute;
var
  Song1, song2: TSong;
begin
  resetDataBase;
  with theDataModule do
  begin
    Song1 := BoldSystemHandle1.System.ClassByExpressionName['Song'][0] as TSong;
    Song2 := BoldSystemHandle2.System.ClassByExpressionName['Song'][0] as TSong;
    Song1.Title := 'anotherSong';
    Song2.Title := 'a third Song';
    BoldSystemHandle1.UpdateDatabase;
    try
      BoldSystemHandle2.UpdateDatabase;
      assert(false, 'Optimistic locking for attribute failed. Did not register');
    except
      on e: EBoldOperationFailedForObjectList do
      begin
        assert(e.ObjectList.Count = 1, 'incorrect number of Failures');
        assert(e.ObjectList[0] = Song2, 'Wrong object failed');
      end;
    end;
  end;
  DiscardAndCloseSystems;
end;

procedure Tjoho_OptimisticLocking.ChangeSingleLink;
var
  Class1, Class2: TClassA;
begin
  resetDataBase;
  with theDataModule do
  begin
    Class1 := BoldSystemHandle1.System.ClassByExpressionName['ClassA'][1] as TClassA;
    Class2 := BoldSystemHandle2.System.ClassByExpressionName['ClassA'][1] as TClassA;
    Assert(Assigned(Class1.previous), 'Class1.previous has to be assigned for test to work.');
    Class1.previous := TClassA.Create(BoldSystemHandle1.System);
    Class2.previous := nil;
    BoldSystemHandle1.UpdateDatabase;
    try
      BoldSystemHandle2.UpdateDatabase;
      assert(false, 'Optimistic locking for SingleLink failed. Did not register');
    except
      on e: EBoldOperationFailedForObjectList do
      begin
        assert(e.ObjectList.Count = 1, 'incorrect number of Failures');
        assert(e.ObjectList[0] = Class2, 'Wrong object failed');
      end;
    end;
  end;
  DiscardAndCloseSystems
end;

procedure Tjoho_OptimisticLocking.DeleteAllObjects;
begin
  with TheDataModule do
  begin
    DiscardAndCloseSystems;
    BoldSystemHandle1.Active := true;
    while BoldSystemHandle1.System.Classes[0].Count > 0 do
      BoldSystemHandle1.System.Classes[0][0].Delete;
    BoldSystemHandle1.UpdateDatabase;
    BoldSystemHandle1.Active := false;
  end;
end;

procedure Tjoho_OptimisticLocking.DeleteObject;
var
  Song1, song2: TSong;
begin
  resetDataBase;
  with theDataModule do
  begin
    Song1 := BoldSystemHandle1.System.ClassByExpressionName['Song'][0] as TSong;
    Song2 := BoldSystemHandle2.System.ClassByExpressionName['Song'][0] as TSong;
    Song1.Delete;
    Song2.Title := 'xxx';
    BoldSystemHandle1.UpdateDatabase;
    try
      BoldSystemHandle2.UpdateDatabase;
      assert(false, 'Optimistic locking for Deleted failed. Did not register');
    except
      on e: EBoldOperationFailedForObjectList do
      begin
        assert(e.ObjectList.Count = 1, 'incorrect number of Failures');
        assert(e.ObjectList[0] = Song2, 'Wrong object failed');
      end;
    end;
  end;
  DiscardAndCloseSystems;
end;

procedure Tjoho_OptimisticLocking.SingleSingleEmbeddNonEmbedd;
var
  A1, A2, Newa1, Newa2: TClassA;
begin
  resetDataBase;
  with theDataModule do
  begin
    A1 := BoldSystemHandle1.System.ClassByExpressionName['ClassA'][1] as TClassA;
    A2 := BoldSystemHandle2.System.ClassByExpressionName['ClassA'][1] as TClassA;
    NewA1 := TClassA.Create(BoldSystemHandle1.System);
    NewA2 := TClassA.Create(BoldSystemHandle2.System);

    NewA1.previous := A1;
    NewA2.previous := A2;
    assert(NewA2.Previous = A2);

    BoldSystemHandle1.UpdateDatabase;
    try
      BoldSystemHandle2.UpdateDatabase;
      assert(false, 'Optimistic locking for SingleSingleEmbeddNonEmbedd failed. Did not register');
    except
      on e: EBoldOperationFailedForObjectList do
      begin
        // the two dirty objects are Song2 and a linkobject
        assert(e.ObjectList.Count = 1, 'incorrect number of Failures');
        assert(e.ObjectList.IndexOf(A2) <> -1, 'ClassA-object did not fail');
      end;
    end;
  end;
  DiscardAndCloseSystems;
end;


procedure Tjoho_OptimisticLocking.DeleteDeletedObject;
var
  Song1, song2: TSong;
  LinkObj: TBoldObject;
  Mode: tBoldOptimisticLockingMode;
begin
  resetDataBase;
  with theDataModule do
  begin
    Song1 := BoldSystemHandle1.System.ClassByExpressionName['Song'][0] as TSong;
    Song2 := BoldSystemHandle2.System.ClassByExpressionName['Song'][0] as TSong;
    Mode := Song1.BoldclassTypeInfo.OptimisticLocking;
    Song1.Delete;
    LinkObj := Song2.hitListsong[0];
    Song2.Delete;
    BoldSystemHandle1.UpdateDatabase;
    try
      BoldSystemHandle2.UpdateDatabase;
      assert(false, 'Optimistic locking for Deleted failed. Did not register');
    except
      on e: EBoldOperationFailedForObjectList do
      begin
        // the two dirty objects are Song2 and a linkobject
        assert(e.ObjectList.Count = 2, 'incorrect number of Failures');
        assert(e.ObjectList.IndexOf(Song2) <> -1, 'Song-object did not fail');
        assert(e.ObjectList.IndexOf(LinkObj) <> -1, 'Linkobject did not fail');
      end;
    end;
  end;
  DiscardAndCloseSystems;
end;

procedure Tjoho_OptimisticLocking.DeleteChangedObject;
var
  Song1, song2: TSong;
begin
  resetDataBase;
  with theDataModule do
  begin
    Song1 := BoldSystemHandle1.System.ClassByExpressionName['Song'][0] as TSong;
    Song2 := BoldSystemHandle2.System.ClassByExpressionName['Song'][0] as TSong;
    Song1.title := 'New title';
    Song2.Delete;
    BoldSystemHandle1.UpdateDatabase;
    try
      BoldSystemHandle2.UpdateDatabase;
      if Song1.BoldClassTypeInfo.OPtimisticLocking = bolmTimeStamp then
        assert(false, 'Optimistic locking for Deleted failed. Did not register');
    except
      on e: EBoldOperationFailedForObjectList do
      begin
        assert(e.ObjectList.Count = 1, 'incorrect number of Failures');
        assert(e.ObjectList[0] = Song2, 'Wrong object failed');
      end;
    end;
  end;
  DiscardAndCloseSystems;
end;

procedure Tjoho_OptimisticLocking.ChangeAndChangeBack;
var
  Song1, song2: TSong;
  oldTitle: String;
begin
  resetDataBase;
  with theDataModule do
  begin
    Song1 := BoldSystemHandle1.System.ClassByExpressionName['Song'][0] as TSong;
    Song2 := BoldSystemHandle2.System.ClassByExpressionName['Song'][0] as TSong;
    OldTitle := Song1.Title;
    Song1.title := 'New title';
    BoldSystemHandle1.UpdateDatabase;
    Song1.title := OldTitle;
    BoldSystemHandle1.UpdateDatabase;
    Song2.Delete;
    try
      BoldSystemHandle2.UpdateDatabase;
      if Song1.BoldClassTypeInfo.OptimisticLocking = bolmTimeStamp then
        assert(false, 'Optimistic locking for Deleted failed. Did not register');
    except
      on e: EBoldOperationFailedForObjectList do
      begin
        if Song1.BoldClassTypeInfo.OptimisticLocking = bolmTimeStamp then
        begin
          assert(e.ObjectList.Count = 1, 'incorrect number of Failures');
          assert(e.ObjectList[0] = Song2, 'Wrong object failed');
        end else
          assert(false, 'Optimistic locking caught an object that should have been OK');
      end;
    end;
  end;
  DiscardAndCloseSystems;
end;



procedure TLocalTestCase.DiscardAndCloseSystems;
begin
  with theDataModule do
  begin
    if BoldSystemHandle2.Active then
    begin
      BoldSystemHandle2.System.Discard;
      BoldSystemHandle2.Active := false;
    end;
    if BoldSystemHandle1.Active then
    begin
      BoldSystemHandle1.System.Discard;
      BoldSystemHandle1.Active := false;
    end;
  end;
end;

procedure Tjoho_OptimisticLocking.ResetDataBase;
begin
  with theDataModule do
  begin
    DeleteAllObjects;
    BoldSystemHandle1.Active := true;
    with TClassA.Create(BoldSystemHandle1.system) do
    begin
      AString := 'classA';
      aInteger := 10;
      next := TClassA.Create(BoldSystemHandle1.System);
    end;
    with TSong.Create(BoldSystemHandle1.System) do
    begin
      Title := 'aSong';
      Hitlist.Add(THitList.Create(BoldSystemHandle1.System));
    end;
    BoldSystemHandle1.UpdateDatabase;
    BoldSystemHandle2.Active := true;
    BoldSystemHandle2.System.Classes[0].EnsureObjects;
    BoldSystemHandle1.System.Classes[0].EnsureObjects;
  end;
end;

class procedure Tjoho_OptimisticLocking.Suit(aSuite: TBoldTestSuite);
begin
  inherited;
  // Testcases are registered with name of published method
  // and a string reference to the tested feature (specification, requirement, bug

  ASuite.AddTest(CreateWithComment('SingleSingleEmbeddNonEmbedd' , ''));
  ASuite.AddTest(CreateWithComment('DeleteDeletedObject' , ''));
  ASuite.AddTest(CreateWithComment('DeleteChangedObject' , ''));
  ASuite.AddTest(CreateWithComment('ChangeAttribute' , ''));
  ASuite.AddTest(CreateWithComment('ChangeSingleLink' , ''));
  ASuite.AddTest(CreateWithComment('DeleteObject' , ''));
  ASuite.AddTest(CreateWithComment('UnlinkSingleSinglelinks' , ''));
  ASuite.AddTest(CreateWithComment('ChangeAndChangeBack' , ''));
end;

procedure Tjoho_OptimisticLocking.UnlinkSingleSinglelinks;
var
  Obj1, Obj2: TClassA;
begin
  // this testcase has nothing to do with optimistic locking...
  // it checks that a non embedded singlelink can be cleared before its embedded counterpart.
  DeleteAllObjects;
  with theDataModule.BoldSystemHandle1 do
  begin
    Active := true;
    Obj1 := TClassA.Create(System);
    Obj2 := TClassA.Create(System);
    Obj1.NExt := Obj2;
    UpdateDatabase;
    DiscardAndCloseSystems;
  end;

  theDataModule.BoldSystemHandle1.Active := true;
  Obj1 := theDataModule.BoldSystemHandle1.System.ClassByExpressionName['ClassA'][0] as TClassA;
  // make sure that the object found has the non-embedded link:

  if not assigned(obj1.M_next.Locator) then
  begin
    DiscardAndCloseSystems;
    theDataModule.BoldSystemHandle1.Active := true;
    Obj1 := theDataModule.BoldSystemHandle1.System.ClassByExpressionName['ClassA'][1] as TClassA;
  end;

  assert(assigned(obj1.M_next.Locator), 'darn, we found the wrong object');
  Obj1.Delete;
  Obj1 := theDataModule.BoldSystemHandle1.System.ClassByExpressionName['ClassA'][0] as TClassA;
  Obj1.Delete;
  DiscardAndCloseSystems;

  // and the other way around...

  theDataModule.BoldSystemHandle1.Active := true;
  Obj1 := theDataModule.BoldSystemHandle1.System.ClassByExpressionName['ClassA'][0] as TClassA;
  // make sure that the object found has the embedded link:

  if assigned(obj1.M_next.Locator) then
  begin
    DiscardAndCloseSystems;
    theDataModule.BoldSystemHandle1.Active := true;
    Obj1 := theDataModule.BoldSystemHandle1.System.ClassByExpressionName['ClassA'][1] as TClassA;
  end;

  assert(not assigned(obj1.M_next.Locator), 'darn, we found the wrong object');
  Obj1.Delete;
  Obj1 := theDataModule.BoldSystemHandle1.System.ClassByExpressionName['ClassA'][0] as TClassA;
  Obj1.Delete;
  DiscardAndCloseSystems;

end;

class function Tjoho_OptimisticLocking.Suite: ITestSuite;
begin
  Result := inherited Suite;
  SetCommentForTest(Result, 'SingleSingleEmbeddNonEmbedd' , '');
  SetCommentForTest(Result, 'DeleteDeletedObject' , '');
  SetCommentForTest(Result, 'DeleteChangedObject' , '');
  SetCommentForTest(Result, 'ChangeAttribute' , '');
  SetCommentForTest(Result, 'ChangeSingleLink' , '');
  SetCommentForTest(Result, 'DeleteObject' , '');
  SetCommentForTest(Result, 'UnlinkSingleSinglelinks' , '');
  SetCommentForTest(Result, 'ChangeAndChangeBack' , '');
end;

initialization
  TestGlobal.RegisterTestCase(Tjoho_OptimisticLocking);

end.
