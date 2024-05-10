
unit joho_dbevol;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Clipbrd,
  Forms,
  Dialogs,
  ActnList,
  BoldDefs,
  BoldHandleAction,
  BoldActions,
  BoldUMLModelLink,
  BoldDbStructureValidator,
  BoldDbValidator,
  BoldDbDataValidator,
  BoldUMLRose98Link,
  BoldHandle,
  BoldPersistenceHandle,
  BoldPersistenceHandleDB,
  BoldModel,
  BoldHandles,
  BoldSubscription,
  BoldSystem,
  BoldValueSpaceInterfaces,
  BoldSystemHandle,
  BoldDbEvolutor,
  BoldDbEvolutorForm,
  TestSuite,
  BoldDBActions,
  BoldAbstractObjectUpgraderHandle,
  BoldObjectUpgraderHandle,
  BoldAbstractModel,
  TestFramework,
  DB,
  BoldAbstractDatabaseAdapter,
  BoldAbstractPersistenceHandleDB,
  DBAccess,
  Uni,
  BoldDatabaseAdapterUniDAC;

type
  TdmTestCase = class(TDataModule)
    BoldSystemHandle1: TBoldSystemHandle;
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
    BoldModel1: TBoldModel;
    BoldUMLRose98Link1: TBoldUMLRoseLink;
    BoldModel2: TBoldModel;
    BoldUMLRose98Link2: TBoldUMLRoseLink;
    bmoMain: TBoldModel;
    bmoAdd: TBoldModel;
    brlMain: TBoldUMLRoseLink;
    brlAdd: TBoldUMLRoseLink;
    bmoRemove: TBoldModel;
    brlRemove: TBoldUMLRoseLink;
    bmoChange: TBoldModel;
    brlChange: TBoldUMLRoseLink;
    bmotempBase: TBoldModel;
    bmotempChange: TBoldModel;
    brlTempBase: TBoldUMLRoseLink;
    brlTempChange: TBoldUMLRoseLink;
    bmoUpgrader1: TBoldModel;
    bmoUpgrader2: TBoldModel;
    brlUpgrader1: TBoldUMLRoseLink;
    brlUpgrader2: TBoldUMLRoseLink;
    BoldObjectUpgraderHandle1: TBoldObjectUpgraderHandle;
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
    BoldDatabaseAdapterUniDAC1: TBoldDatabaseAdapterUniDAC;
    UniConnection1: TUniConnection;
    procedure BoldObjectUpgraderHandle1UpgradeObject(Obj: TBoldObject);
    procedure BoldObjectUpgraderHandle1_UpA_UpgradeObject(
      Obj: TBoldObject);
    procedure BoldObjectUpgraderHandle1_UpB_UpgradeObject(
      Obj: TBoldObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TLocalTestCase = class(TBoldTestCase)
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  Tjoho_dbEvol = class(TLocalTestCase)
  private
    procedure InitializeOldObjects;
    procedure CheckNewObjects;
  public
    class procedure Suit(aSuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
    // If Setup is used, remember to invoke inherited!
    procedure SetUp; override;
  published
    procedure TestCase1;
  end;


  Tjoho_EvolTest = class(TLocalTestCase)
  private
  public
    class procedure Suit(aSuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
    // If Setup is used, remember to invoke inherited!
    procedure SetUp; override;
    procedure InitBaseModel(BaseModel: TBoldModel; UseGeneratedCode: Boolean = true);
    procedure InitModifiedModel(ModifiedModel: TBoldModel; UseGeneratedCode: Boolean = false);
  published
    procedure Add;
    procedure Remove;
    procedure Change;
    procedure Temporal;
    procedure UpgradeObjects;
    procedure BatchUpgradeObjects;
  end;
var
  dmTestCase: TdmTestCase = nil;

implementation

uses ModelEv, ModelEvTemporal, ModEv_Upgrade, BoldBatchUpgrader;

var
  // This construct is used to protect the EnsureDataModule from having to be rewritten.
  TheDataModule: TdmTestCase absolute dmTestCase;
  DoneSetup: boolean = false;

{$R *.DFM}

procedure EnsureDataModule;
begin
  // Uncomment EnsureDM_Model if the model in MAIN is used.
  //Ensuredm_Model;

  if not Assigned(TheDataModule) then
  begin
    TheDataModule := TdmTestCase.Create(Application);
//    TheDataModule.BoldIBAliasAction1.ForceRegenerateAliasAndSchema(dbgQuery);
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
end;

procedure TLocalTestCase.TearDown;
begin
  FreeDataModule;
  DoneSetup := False;
end;


{ TYourTestCaseHere }

procedure Tjoho_dbEvol.CheckNewObjects;
var
  Obj: TBoldObject;
  validator: TBoldDbValidator;
begin
  TheDataModule.BoldPersistenceHandleDB1.BoldModel := TheDataModule.BoldModel2;
  TheDataModule.BoldSystemTypeInfoHandle1.BoldModel := TheDataModule.BoldModel2;
  TheDataModule.BoldSystemHandle1.Active := True;
  with dmTestCase.BoldSystemHandle1.System do
  begin
    Classes[0].EnsureObjects;
    assert(classes[0].Count = 5);
    obj := ClassByExpressionName['ClassA2'][0];
    assert(obj.BoldmemberByExpressionName['Attr1'].AsString = 'obj1');
    obj := ClassByExpressionName['ClassB'][0];
    assert(obj.BoldmemberByExpressionName['Attr2'].AsString = 'obj2');
    obj := ClassByExpressionName['SubClass'][0];
    assert(obj.BoldmemberByExpressionName['SubAttr1'].AsString = 'Subobj1');
    obj := ClassByExpressionName['SubSub'][0];
    assert(obj.BoldmemberByExpressionName['SubAttr2'].AsString = 'Subobj2');

    assert(ClassByExpressionName['MidClass'].Count = 3);
  end;
  TheDataModule.BoldSystemHandle1.Active := false;
  validator :=  TBoldDbStructureValidator.Create(nil);
  Validator.PersistenceHandle := TheDataModule.BoldPersistenceHandleDB1;
  validator.Execute;
//  assert(validator.remedy.Count = 0, validator.remedy.text);
  validator.free;

  validator :=  TBoldDbDataValidator.Create(nil);
  Validator.PersistenceHandle := TheDataModule.BoldPersistenceHandleDB1;
  validator.Execute;
  assert(validator.remedy.Count = 0, validator.remedy.text);
  validator.free;
end;

procedure Tjoho_dbEvol.InitializeOldObjects;
var
  Obj: TBoldObject;
begin
  dmTestCase.BoldSystemHandle1.Active := false;
  dmTestCase.BoldPersistenceHandleDB1.BoldModel := dmTestCase.BoldModel1;
  dmTestCase.BoldSystemTypeInfoHandle1.UseGeneratedCode := false;
  dmTestCase.BoldSystemTypeInfoHandle1.BoldModel := dmTestCase.BoldModel1;
  (dmTestCase.BoldSystemHandle1.PersistenceHandle as TBoldPersistenceHandleDb).CreateDatabase;
  (dmTestCase.BoldSystemHandle1.PersistenceHandle as TBoldPersistenceHandleDb).CreateDataBaseSchema;

  dmTestCase.BoldSystemHandle1.Active := true;

  with dmTestCase.BoldSystemHandle1.System do
  begin
    obj := CreateNewObjectByExpressionName('ClassA');
    obj.BoldMemberByExpressionName['Attr1'].AsString := 'obj1';
    obj := CreateNewObjectByExpressionName('ClassB');
    obj.BoldMemberByExpressionName['Attr2'].AsString := 'obj2';

    obj := CreateNewObjectByExpressionName('MidClass');
    obj.BoldMemberByExpressionName['MidAttr'].AsString := 'Midobj1';
    obj := CreateNewObjectByExpressionName('SubClass');
    obj.BoldMemberByExpressionName['SubAttr1'].AsString := 'Subobj1';
    obj := CreateNewObjectByExpressionName('SubSub');
    obj.BoldMemberByExpressionName['SubAttr2'].AsString := 'Subobj2';
    UpdateDatabase;
  end;
  TheDataModule.BoldSystemHandle1.Active := false;
end;

procedure Tjoho_dbEvol.SetUp;
begin
  inherited;
  EnsureSetup;
  InitializeOldObjects;
end;

class procedure Tjoho_dbEvol.Suit(aSuite: TBoldTestSuite);
begin
  inherited;
  // Testcases are registered with name of published method
  // and a string reference to the tested feature (specification, requirement, bug

  //--Add tests here--
  ASuite.AddTest(CreateWithComment('TestCase1' , ''));
end;

class function Tjoho_dbEvol.Suite: ITestSuite;
begin
  result := inherited Suite;
  SetCommentForTest(Result, 'TestCase1' , '');
end;

procedure Tjoho_dbEvol.TestCase1;
var
  Evolutor: TBoldDataBaseEvolutor;
  EvolutorForm: TfrmBoldDbEvolutor;
begin
  dmTestCase.BoldPersistenceHandleDB1.BoldModel := dmTestCase.BoldModel2;

  Evolutor := TBoldDataBaseEvolutor.Create(dmTestCase.BoldPersistenceHandleDB1);
  EvolutorForm := TfrmBoldDbEvolutor.Create(nil);
  EvolutorForm.Show;
  try
    Evolutor.GenericScript := false;
    Evolutor.CalculateScript;
    Evolutor.GenerateScript(EvolutorForm.SQLScript, EvolutorForm.MappingInfoScript);
    Evolutor.GenerateWarnings(EvolutorForm.Warnings);
    Evolutor.ExecuteScript;
    assert(EvolutorForm.mmoSQLScript.Lines.Count > 0);
    assert(EvolutorForm.mmoActions.Lines.Count > 0);
  finally
    Evolutor.Free;
  //  EvolutorForm.Close;
  end;
  CheckNewObjects;
end;

{ Tjoho_EvolTest }

procedure Tjoho_EvolTest.Add;
var
  anA: TA;
  aB: TB;
  Obj1, Obj2, Obj3: TBoldObject;
  ObjList: TBoldObjectList;
begin
  InitBaseModel(dmTestCase.bmoMain);

  anA := TA.Create(dmTestCase.BoldSystemHandle1.System);
  aB := TB.Create(dmTestCase.BoldSystemHandle1.System);
  aB.aRole.Add(anA);

  dmTestCase.BoldSystemHandle1.UpdateDatabase;

  InitModifiedModel(dmTestCase.bmoAdd);

  assert(dmTestCase.BoldSystemHandle1.System.ClassByExpressionName['A'].Count = 1, 'AllInstances of class A');
  Obj1 := dmTestCase.BoldSystemHandle1.System.ClassByExpressionName['A'].BoldObjects[0];
  assert(dmTestCase.BoldSystemHandle1.System.ClassByExpressionName['B'].Count = 1, 'AllInstances of class B');
  Obj2 := dmTestCase.BoldSystemHandle1.System.ClassByExpressionName['B'].BoldObjects[0];
  assert(dmTestCase.BoldSystemHandle1.System.ClassByExpressionName['NewLinkClass'].Count = 1, 'AllInstances of class NewLinkClass');
  Obj3 := dmTestCase.BoldSystemHandle1.System.ClassByExpressionName['NewLinkClass'].BoldObjects[0];

  assert(Obj1.BoldMemberByExpressionName['NewA3'].asString = '1', 'Default value of new attribute');
  Obj1.BoldMemberByExpressionName['NewA3'].AsString := '12';
  ObjLIst := (Obj1.BoldMemberByExpressionName['B'] as TboldobjectList);
  assert(objLIst.Count = 1, 'preservation of association');
  assert(ObjList.Includes(Obj2), 'preservation of association');
  assert((Obj1.BoldMemberByExpressionName['NewLinkClass'] as TBoldObjectList).Includes(Obj3), 'LinkClass made explicit');
  (Obj1.BoldMemberByExpressionName['aNewRoleB'] as TBoldObjectReference).BoldObject := Obj2;
  assert(Obj2.BoldMemberByExpressionName['NewSuper1'].AsString = '', 'Default value of inherited new attribute');
  dmTestCase.BoldSystemHandle1.System.CreateNewObjectByExpressionName('NewSubClass');
  dmTestCase.BoldSystemHandle1.System.CreateNewObjectByExpressionName('NewClass');
  assert(Obj3.BoldMemberByExpressionName['LinkAttr1'].AsString = 'Hej', 'Default value of new attribute of link class');

  dmTestCase.BoldSystemHandle1.UpdateDatabase;
end;

procedure Tjoho_EvolTest.Change;
var
  anA: TA;
  aB: TB;
  aBSub: TSubB;
  aCSub: TCSub;
  aCSubSub1: TCSubSub1;
  aCSubSub2: TCSubSub2;
  _A, _B, _BSub, _CSub, _CSubSub1, _CSubSub2: TBoldObject;
  TotalNumberOfObjects: integer;
begin
  InitBaseModel(dmTestCase.bmoMain);

  anA := TA.Create(dmTestCase.BoldSystemHandle1.System);
  anA.a1 := 'aaa';
  anA.a2 := 100;

  aB := TB.Create(dmTestCase.BoldSystemHandle1.System);
  aB.b1 := 'bbb';
  aB.b2 := 200;
  aB.aRole.Add(anA);

  aBSub := TSubB.Create(dmTestCase.BoldSystemHandle1.System);
  aBSub.B1 := 'b1';
  aBSub.B2 := 300;
  aBSub.SubB1 := 'bSub';

  aCSub := TCSub.Create(dmTestCase.BoldSystemHandle1.System);
  aCSub.C1 := 'ccc';
  aCSub.CSub1 := 'cSub';

  aCSubSub1 := TCSubSub1.Create(dmTestCase.BoldSystemHandle1.System);
  aCSubSub1.C1 := 'ccc1';
  aCSubSub1.CSub1 := 'cSub1';
  aCSubSub1.B := aB;

  aCSubSub2 := TCSubSub2.Create(dmTestCase.BoldSystemHandle1.System);
  aCSubSub2.C1 := 'ccc2';
  aCSubSub2.CSub1 := 'cSub2';
  aCSubSub2.CSubSub1 := 'cSubSub2';
  aCSubSub2.B := aB;
  TotalNumberOfObjects := dmTestCase.BoldSystemHandle1.System.Classes[0].Count;

  dmTestCase.BoldSystemHandle1.UpdateDatabase;

  InitModifiedModel(dmTestCase.bmoChange);

  // check the objects them selves
  assert(dmTestCase.BoldSystemHandle1.System.ClassByExpressionName['NewA'].Count = 1, 'AllInstances of class A');
  _A := dmTestCase.BoldSystemHandle1.System.ClassByExpressionName['NewA'].BoldObjects[0];

  assert(dmTestCase.BoldSystemHandle1.System.ClassByExpressionName['SubB'].Count = 1, 'AllInstances of class BSub');
  _BSub := dmTestCase.BoldSystemHandle1.System.ClassByExpressionName['SubB'].BoldObjects[0];

  assert(dmTestCase.BoldSystemHandle1.System.ClassByExpressionName['B'].Count = 3, 'AllInstances of class B');
  assert((_A.BoldMemberByExpressionName['B'] as TBoldObjectList).Count = 1, 'Preservation of association');
  _B := (_A.BoldMemberByExpressionName['B'] as TBoldObjectList)[0];
  assert(_b.BoldClassTypeInfo.ExpressionName = 'B');

  assert(dmTestCase.BoldSystemHandle1.System.ClassByExpressionName['CSubSub1'].Count = 1, 'AllInstances of class CSubSub1');
  _CSubSub1 := dmTestCase.BoldSystemHandle1.System.ClassByExpressionName['CSubSub1'].BoldObjects[0];
  assert(dmTestCase.BoldSystemHandle1.System.ClassByExpressionName['CSubSub2'].Count = 1, 'AllInstances of class CSubSub2');
  _CSubSub2 := dmTestCase.BoldSystemHandle1.System.ClassByExpressionName['CSubSub2'].BoldObjects[0];

  assert(dmTestCase.BoldSystemHandle1.System.ClassByExpressionName['CSub'].Count = 2, 'AllInstances of class CSub');

  if dmTestCase.BoldSystemHandle1.System.ClassByExpressionName['CSub'][0] = _CSubSub2 then
    _CSub := dmTestCase.BoldSystemHandle1.System.ClassByExpressionName['CSub'][1]
  else
    _CSub := dmTestCase.BoldSystemHandle1.System.ClassByExpressionName['CSub'][0];

  assert(_CSub.BoldClassTypeInfo.ExpressionName = 'CSub');

  assert(dmTestCase.BoldSystemHandle1.System.ClassByExpressionName['C'].Count = 3, 'AllInstances of class C');

  assert(dmTestCase.BoldSystemHandle1.System.ClassByExpressionName['BusinessClassesRoot'].Count = TotalNumberOfObjects, 'AllInstances of class BCR');

  // Check the attributes of objects
  assert(_A.BoldExistenceState = besExisting, '');
  assert(_A.BoldMemberByExpressionName['a1'].AsString = 'aaa', 'A.a1');
  assert(_A.BoldMemberByExpressionName['a2'].AsString = '100', 'A.a2');
  assert(_A.BoldMemberByExpressionName['NewB1'].AsString = '', 'A.NewB1');
  assert(_A.BoldMemberByExpressionName['SubB1'].AsString = '', 'A.SubB1');

  assert(_B.BoldExistenceState = besExisting, '');
  assert(_B.BoldMemberByExpressionName['NewB1'].AsString = 'bbb', 'B.NewB1');
  assert(_B.BoldMemberByExpressionName['SubB1'].AsString = '', 'B.SubB1');
  assert((_B.BoldMemberByExpressionName['AnotherRole'] as tBoldObjectList).Includes(_a), 'B.anotherRole');

  assert(_BSub.BoldExistenceState = besExisting, '');
  assert(_BSub.BoldMemberByExpressionName['NewB1'].AsString = 'b1', 'BSub.NewB1');
  assert(_BSub.BoldMemberByExpressionName['B2'].AsString = '300', 'BSub.B2');
  assert(_BSub.BoldMemberByExpressionName['SubB1'].AsString = 'bSub', 'BSub.SubB1');
  assert(_BSub.BoldMemberByExpressionName['NewSubB1'].AsString = 'hå', 'BSub.B2');

  assert(_CSub.BoldExistenceState = besExisting, '');
  assert(_CSub.BoldMemberByExpressionName['C1'].AsString = 'ccc', 'CSub.C1');
  assert(_CSub.BoldMemberByExpressionName['CSub1'].AsString = 'cSub', 'CSub.CSub1');

  assert(_CSubSub1.BoldExistenceState = besExisting, '');
  assert(_CSubSub1.BoldMemberByExpressionName['C1'].AsString = 'ccc1', 'CSubSub1.C1');
  assert((_CSubSub1.BoldMemberByExpressionName['B'] as TBoldObjectReference).BoldObject = _B, 'CSubSub1.B');

  assert(_CSubSub2.BoldExistenceState = besExisting, '');
  assert(_CSubSub2.BoldMemberByExpressionName['C1'].AsString = 'ccc2', 'CSubSub2.C1');
  assert(_CSubSub2.BoldMemberByExpressionName['CSub1'].AsString = 'cSub2', 'CSubSub2.CSub1');
  assert(_CSubSub2.BoldMemberByExpressionName['CSubSub1'].AsString = 'cSubSub2', 'CSubSub2.CSubSub1');
  assert((_CSubSub2.BoldMemberByExpressionName['B'] as TBoldObjectReference).BoldObject = _B, 'CSubSub2.B');

  dmTestCase.BoldSystemHandle1.UpdateDatabase;
end;

procedure Tjoho_EvolTest.InitBaseModel(BaseModel: TBoldModel; UseGeneratedCode: Boolean = true);
begin
  dmTestCase.BoldSystemHandle1.Active := false;
  dmTestCase.BoldSystemTypeInfoHandle1.UseGeneratedCode := UseGeneratedCode;
  dmTestCase.BoldSystemTypeInfoHandle1.BoldModel := BaseModel;
  dmTestCase.BoldPersistenceHandleDB1.BoldModel := BaseModel;
  if BaseModel.MOldModel.UseModelVersion then
    dmTestCase.BoldPersistenceHandleDB1.UpgraderHandle := dmTestCase.BoldObjectUpgraderHandle1
  else
    dmTestCase.BoldPersistenceHandleDB1.UpgraderHandle := nil;

  (dmTestCase.BoldSystemHandle1.PersistenceHandle as TBoldPersistenceHandleDb).CreateDatabase;
  (dmTestCase.BoldSystemHandle1.PersistenceHandle as TBoldPersistenceHandleDb).CreateDataBaseSchema;
  dmTestCase.BoldSystemHandle1.Active := true;
end;

procedure Tjoho_EvolTest.InitModifiedModel(ModifiedModel: TBoldModel; UseGeneratedCode: Boolean = false);
var
  Evolutor: TBoldDataBaseEvolutor;
begin
  dmTestCase.BoldSystemHandle1.Active := False;
  dmTestCase.BoldSystemTypeInfoHandle1.UseGeneratedCode := UseGeneratedCode;
  dmTestCase.BoldSystemTypeInfoHandle1.BoldModel := ModifiedModel;
  dmTestCase.BoldPersistenceHandleDB1.BoldModel := ModifiedModel;
  Evolutor := TBoldDataBaseEvolutor.Create(dmTestCase.BoldPersistenceHandleDB1);
  try
    Evolutor.GenericScript := false;
    Evolutor.CalculateScript;
{   // For debugging purposes
    Script1 := tStringList.Create;
    Script2 := TStringList.Create;
    Evolutor.GenerateScript(Script1, Script2);
    ShowMessage( Script1.text);
}
    Evolutor.ExecuteScript;
  finally
    Evolutor.Free;
  end;
  dmTestCase.BoldSystemHandle1.Active := true;

end;

procedure Tjoho_EvolTest.Remove;
var
  anA: TA;
  aB: TB;
begin
  InitBaseModel(dmTestCase.bmoMain);

  anA := TA.Create(dmTestCase.BoldSystemHandle1.System);
  aB := TB.Create(dmTestCase.BoldSystemHandle1.System);
  aB.C.Add(TC.Create(dmTestCase.BoldSystemHandle1.System));
  TCSub.Create(dmTestCase.BoldSystemHandle1.System);
  TCSubSub1.Create(dmTestCase.BoldSystemHandle1.System);
  TCSubSub2.Create(dmTestCase.BoldSystemHandle1.System);
  TSubB.Create(dmTestCase.BoldSystemHandle1.System);

  dmTestCase.BoldSystemHandle1.UpdateDatabase;

  InitModifiedModel(dmTestCase.bmoRemove);

  assert(dmTestCase.BoldSystemHandle1.System.ClassByExpressionName['A'].Count = 1, 'AllInstances of class A');
  assert(dmTestCase.BoldSystemHandle1.System.ClassByExpressionName['B'].Count = 1, 'AllInstances of class B');
  assert(dmTestCase.BoldSystemHandle1.System.ClassByExpressionName['CSubSub1'].Count = 1, 'AllInstances of class C');
  assert(dmTestCase.BoldSystemHandle1.System.ClassByExpressionName['CSubSub2'].Count = 1, 'AllInstances of class C');
  assert(dmTestCase.BoldSystemHandle1.System.ClassByExpressionName['C'].Count = 3, 'AllInstances of class C');

  assert(dmTestCase.BoldSystemHandle1.System.ClassByExpressionName['BusinessClassesRoot'].Count = 5, 'AllInstances of class BusinessClassesRoot');

  dmTestCase.BoldSystemHandle1.System.ClassByExpressionName['BusinessClassesRoot'].EnsureObjects;

end;

procedure Tjoho_EvolTest.SetUp;
begin
  inherited;
  EnsureSetup;
end;

class procedure Tjoho_EvolTest.Suit(aSuite: TBoldTestSuite);
begin
  ASuite.AddTest(CreateWithComment('BatchUpgradeObjects' , '7.8.1'));
  ASuite.AddTest(CreateWithComment('UpgradeObjects' , '7.8.2'));
  ASuite.AddTest(CreateWithComment('Add' , '7.7 (5.1)'));
  ASuite.AddTest(CreateWithComment('Remove' , '7.7 (5.2)'));
  ASuite.AddTest(CreateWithComment('Change' , '7.7 (5.3)'));
  ASuite.AddTest(CreateWithComment('Temporal' , '7.6'));
end;


procedure Tjoho_EvolTest.Temporal;
var
  A: TTempA;
  B: TTempB;
  U: TTempUnversioned;
  firstTime: integer;
  _B, _OldB, _U: TBoldObject;
begin
  InitBaseModel(dmTestCase.bmotempBase);
  a := TTempA.Create(dmTestCase.BoldSystemHandle1.System);
  a.a1 := 'a_a1';
  a.a2 := 1;
  a.a3 := 'a_a3';

  b := TTempB.Create(dmTestCase.BoldSystemHandle1.System);
  b.a1 := 'b_a1';
  b.a2 := 2;
  b.a3 := 'b_a3';
  b.b1 := 'b_b1';
  b.rootAttr := 100;

  u := TTempUnversioned.Create(dmTestCase.BoldSystemHandle1.System);
  u.a1 := 'u_a1';
  u.a2 := 3;
  u.a3 := 'u_a3';
  u.u1 := 'u_u1';
  u.u2 := 4;

  dmTestCase.BoldSystemHandle1.UpdateDatabase;
  firstTime := dmTestCase.BoldSystemHandle1.System.TimeStampOfLatestUpdate;

  a.a1 := 'a_a1_2';
  a.a2 := 5;
  a.a3 := 'a_a3_2';

  b.b1 := 'b_b1_2';
  b.a1 := 'b_a1_2';
  b.a2 := 6;
  b.a3 := 'b_a3_2';

  dmTestCase.BoldSystemHandle1.UpdateDatabase;

  InitModifiedModel(dmTestCase.bmotempChange);

  assert(dmTestCase.BoldSystemHandle1.System.ClassByExpressionName['TempB'].Count = 1, '');
  _B := dmTestCase.BoldSystemHandle1.System.ClassByExpressionName['TempB'][0];
  assert(_B.BoldMemberByExpressionName['b1'].AsString = 'b_b1_2', '');
  assert(_B.BoldMemberByExpressionName['u2'].AsString = '0', '');

  _OldB := _B.AtTime(firstTime);
  assert(_OldB.BoldMemberByExpressionName['b1'].AsString = 'b_b1', '');
  assert(_OldB.BoldMemberByExpressionName['u2'].AsString = '0', '');
  assert(_OldB.BoldMemberByExpressionName['RootAttr'].AsString = '100', '');

  assert(dmTestCase.BoldSystemHandle1.System.ClassByExpressionName['TempUnversioned'].Count = 1, '');
  _U := dmTestCase.BoldSystemHandle1.System.ClassByExpressionName['TempUnversioned'][0];
  assert(_U.BoldMemberByExpressionName['a2'].AsString = '3', '');
  assert(_U.BoldMemberByExpressionName['a3'].AsString = 'u_a3', '');
  assert(_U.BoldMemberByExpressionName['u1'].AsString = 'u_u1', '');
  assert(_U.BoldMemberByExpressionName['a1'].AsString = 'u_a1', '');
  assert(_U.BoldMemberByExpressionName['u2'].AsString = '4', '');

  assert(dmTestCase.BoldSystemHandle1.System.ClassByExpressionName['TempA'].Count = 3, '');

  assert(dmTestCase.BoldSystemHandle1.System.ClassByExpressionName['TempRootClass'].Count = 3, '');

  dmTestCase.BoldSystemHandle1.System.ClassByExpressionName['TempRootClass'].EnsureObjects;
end;

procedure TdmTestCase.BoldObjectUpgraderHandle1UpgradeObject(
  Obj: TBoldObject);
var
  C: TUpC;
begin
  if Obj is TUpC then
  begin
    C := Obj as TUpC;
    if not assigned(C.UpD) then
    begin
      C.Upd := TUpD.Create(C.BoldSystem);
      c.UpD.anAttr := 'New';
    end;
  end;
end;

procedure Tjoho_EvolTest.UpgradeObjects;
var
  Obj: TBoldObject;
  A: TUpA;
  B: TUpB;
  C: TUpC;
  D: TUpD;
  AnotherCurr: Currency;
begin
  InitBaseModel(dmTestCase.bmoUpgrader1, false);

  Obj :=dmTestCase.BoldSystemHandle1.System.CreateNewObjectByExpressionName('UpA');
  Obj.BoldMemberByExpressionName['Name'].asstring := '1';
  Obj :=dmTestCase.BoldSystemHandle1.System.CreateNewObjectByExpressionName('UpB');
  Obj.BoldMemberByExpressionName['Amount'].asstring := '10';
  Obj :=dmTestCase.BoldSystemHandle1.System.CreateNewObjectByExpressionName('UpC');
  Obj.BoldMemberByExpressionName['Name'].asstring := 'UpC';
  Obj :=dmTestCase.BoldSystemHandle1.System.CreateNewObjectByExpressionName('UpD');
  Obj.BoldMemberByExpressionName['anAttr'].asstring := 'UpD';

  dmTestCase.BoldSystemHandle1.UpdateDatabase;

  InitModifiedModel(dmTestCase.bmoUpgrader2, true);
  dmTestCase.BoldPersistenceHandleDB1.UpgraderHandle := dmTestCase.BoldObjectUpgraderHandle1;

  A := dmTestCase.BoldSystemHandle1.System.ClassByExpressionName['UpA'][0] as TUpA;
  assert(A.BoldExistenceState = besExisting, 'A exists');
  B := dmTestCase.BoldSystemHandle1.System.ClassByExpressionName['UpB'][0] as TUpB;
  assert(B.BoldExistenceState = besExisting, 'B exists');
  C := dmTestCase.BoldSystemHandle1.System.ClassByExpressionName['UpC'][0] as TUpC;
  assert(C.BoldExistenceState = besExisting, 'C exists');
  D := dmTestCase.BoldSystemHandle1.System.EvaluateExpressionAsDirectElement('UpD.allInstances->select(anAttr = ''UpD'')->first') as TUpD;
  assert(D.BoldExistenceState = besExisting, 'D exists');

  assert(a.NewName = '1', 'upgrading of A');
  AnotherCurr := 10/8.45;
  assert(b.Euroamount = anotherCurr, 'upgrading of B');
  assert(assigned(c.UpD), 'Upgrading of C');
  assert(c.UpD.anAttr = 'New', 'Upgrading of C');

  assert(dmTestCase.BoldSystemHandle1.System.ClassByExpressionName['UpD'].Count = 2, 'Count of class UpD');
end;

procedure Tjoho_EvolTest.BatchUpgradeObjects;
var
  Obj: TBoldObject;
  A: TUpA;
  B: TUpB;
  C: TUpC;
  D: TUpD;
  AnotherCurr: Currency;
  BatchUpgrader: TBoldBatchUpgrader;
begin
  InitBaseModel(dmTestCase.bmoUpgrader1, false);

  Obj :=dmTestCase.BoldSystemHandle1.System.CreateNewObjectByExpressionName('UpA');
  Obj.BoldMemberByExpressionName['Name'].asstring := '1';
  Obj :=dmTestCase.BoldSystemHandle1.System.CreateNewObjectByExpressionName('UpB');
  Obj.BoldMemberByExpressionName['Amount'].asstring := '10';
  Obj :=dmTestCase.BoldSystemHandle1.System.CreateNewObjectByExpressionName('UpC');
  Obj.BoldMemberByExpressionName['Name'].asstring := 'UpC';
  Obj :=dmTestCase.BoldSystemHandle1.System.CreateNewObjectByExpressionName('UpD');
  Obj.BoldMemberByExpressionName['anAttr'].asstring := 'UpD';

  dmTestCase.BoldSystemHandle1.UpdateDatabase;

  InitModifiedModel(dmTestCase.bmoUpgrader2, true);
  dmTestCase.BoldPersistenceHandleDB1.UpgraderHandle := dmTestCase.BoldObjectUpgraderHandle1;
  BatchUpgrader := TBoldBatchUpgrader.Create(
    dmTestCase.BoldPersistenceHandleDB1.PersistenceControllerDefault.PersistenceMapper,
    dmTestCase.BoldObjectUpgraderHandle1.ObjectUpgrader);
  BatchUpgrader.UpgradeObjects;
  BatchUpgrader.free;
  dmTestCase.BoldPersistenceHandleDB1.UpgraderHandle := nil;

  dmTestCase.BoldSystemHandle1.Active := false;
  dmTestCase.BoldSystemHandle1.Active := true;
  
  A := dmTestCase.BoldSystemHandle1.System.ClassByExpressionName['UpA'][0] as TUpA;
  assert(A.BoldExistenceState = besExisting, 'A exists');
  B := dmTestCase.BoldSystemHandle1.System.ClassByExpressionName['UpB'][0] as TUpB;
  assert(B.BoldExistenceState = besExisting, 'B exists');
  C := dmTestCase.BoldSystemHandle1.System.ClassByExpressionName['UpC'][0] as TUpC;
  assert(C.BoldExistenceState = besExisting, 'C exists');
  D := dmTestCase.BoldSystemHandle1.System.EvaluateExpressionAsDirectElement('UpD.allInstances->select(anAttr = ''UpD'')->first') as TUpD;
  assert(D.BoldExistenceState = besExisting, 'D exists');

  assert(a.NewName = '1', 'upgrading of A');
  AnotherCurr := 10/8.45;
  assert(b.Euroamount = anotherCurr, 'upgrading of B');
  assert(assigned(c.UpD), 'Upgrading of C');
  assert(c.UpD.anAttr = 'New', 'Upgrading of C');

  assert(dmTestCase.BoldSystemHandle1.System.ClassByExpressionName['UpD'].Count = 2, 'Count of class UpD');
end;


procedure TdmTestCase.BoldObjectUpgraderHandle1_UpA_UpgradeObject(
  Obj: TBoldObject);
var
  A: TUpA;
begin
  a := Obj as TUpA;
  a.NewName := IntToStr(a.Name);
end;

procedure TdmTestCase.BoldObjectUpgraderHandle1_UpB_UpgradeObject(
  Obj: TBoldObject);
var
  B: TUpB;
begin
  B := Obj as TUpB;
  b.EuroAmount := b.Amount/8.45;
end;

class function Tjoho_EvolTest.Suite: ITestSuite;
begin
  result := inherited Suite;
  SetCommentForTest(Result, 'BatchUpgradeObjects' , '7.8.1');
  SetCommentForTest(Result, 'UpgradeObjects' , '7.8.2');
  SetCommentForTest(Result, 'Add' , '7.7 (5.1)');
  SetCommentForTest(Result, 'Remove' , '7.7 (5.2)');
  SetCommentForTest(Result, 'Change' , '7.7 (5.3)');
  SetCommentForTest(Result, 'Temporal' , '7.6');
end;

initialization
  TestGlobal.RegisterTestCase(Tjoho_dbEvol);
  TestGlobal.RegisterTestCase(Tjoho_EvolTest);

end.
