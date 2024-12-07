
unit joho_ModelEvolution;

interface

uses
  BoldMappingInfo,
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  BoldtaggedValueSupport,
  Dialogs,
  ActnList,
  BoldSystemRT,
  BoldHandleAction,
  BoldActions,
  BoldUMLModelLink,
  BoldUMLRose98Link,
  BoldHandle,
  BoldPMappersDefault,
  BoldPersistenceHandle,
  BoldPersistenceHandleDB,
  BoldModel,
  BoldHandles,
  BoldSubscription,
  BoldSystemHandle,
  TestSuite,
  BoldDBActions,
  BoldAbstractModel,
  TestFramework, BoldAbstractPersistenceHandleDB,
  DB,
  BoldAbstractDatabaseAdapter,
  BoldDatabaseAdapterUniDAC,
  DBAccess,
  Uni;

type
  TdmModelEvolution = class(TDataModule)
    BoldSystemHandle1: TBoldSystemHandle;
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
    BoldModel1: TBoldModel;
    BoldUMLRose98Link1: TBoldUMLRoseLink;
    ModelWithNewDbNames: TBoldModel;
    BoldSystemTypeInfoHandle2: TBoldSystemTypeInfoHandle;
    bshTest: TBoldSystemHandle;
    BoldModelDeprecated: TBoldModel;
    BoldSystemTypeInfoHandle3: TBoldSystemTypeInfoHandle;
    BoldUMLRose98Link2: TBoldUMLRoseLink;
    BoldSystemHandle2: TBoldSystemHandle;
    bmNoAttr: TBoldModel;
    bstihNoAttr: TBoldSystemTypeInfoHandle;
    bshNoAttr: TBoldSystemHandle;
    bmAttrWithDefault: TBoldModel;
    bstihAttrWithDefault: TBoldSystemTypeInfoHandle;
    bshAttrWithDefault: TBoldSystemHandle;
    bmToBeRem: TBoldModel;
    bstihToBeRem: TBoldSystemTypeInfoHandle;
    bshToBeRem: TBoldSystemHandle;
    bmToBeRemFile: TBoldModel;
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
    BoldPersistenceHandleDB2: TBoldPersistenceHandleDB;
    BoldPersistenceHandleDB3: TBoldPersistenceHandleDB;
    BoldPersistenceHandleDB4: TBoldPersistenceHandleDB;
    BoldPersistenceHandleDB5: TBoldPersistenceHandleDB;
    BoldPersistenceHandleDB6: TBoldPersistenceHandleDB;
    UniConnection1: TUniConnection;
    BoldDatabaseAdapterUniDAC1: TBoldDatabaseAdapterUniDAC;
    BoldDatabaseAdapterUniDAC2: TBoldDatabaseAdapterUniDAC;
    BoldDatabaseAdapterUniDAC3: TBoldDatabaseAdapterUniDAC;
    BoldDatabaseAdapterUniDAC4: TBoldDatabaseAdapterUniDAC;
    BoldDatabaseAdapterUniDAC5: TBoldDatabaseAdapterUniDAC;
    UniConnection6: TUniConnection;
    BoldDatabaseAdapterUniDAC6: TBoldDatabaseAdapterUniDAC;
    UniConnection2: TUniConnection;
    UniConnection3: TUniConnection;
    UniConnection4: TUniConnection;
    UniConnection5: TUniConnection;
    procedure DataModuleCreate(Sender: TObject);
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

  TModelEvolutionTest = class(TLocalTestCase)
  private
    procedure TestMappingInfo(MappingInfo: TBoldDefaultMappingInfo);
  public
    class procedure Suit(aSuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
  published
    procedure TestMappingGeneration;
    procedure TestReadnWriteMapping;
    procedure TestPSDescriptionNames;
    procedure TestPersistence;
    procedure TestDeprecatedInMappingInfo;
    procedure TestDeprecatedNotInRT;
    procedure TestDefaultColumnValues;
    procedure TestTemporal;
    procedure TestToBeRemoved;
    procedure TestToBeRemovedFile;
  end;


var
  dmModelEvolution: TdmModelEvolution = nil;

implementation

uses
  BoldBase,
  BoldLogReceiverInterface,
  BoldLogHandler,
  BoldSQLMappingInfo,
  BoldDefs,
  BoldSystem,
  BoldAttributes,
  ModelEvolTest,
  BoldPMappersSQL;

type
  TMyLogReceiver = class(TBoldNonRefCountedObject, IBoldLogReceiver)
  private
    fLogCount: Integer;
    FEnabled: Boolean;
    procedure SetProgress(const Value: integer);
    procedure SetLogHeader(const Value: string);
    procedure SetProgressMax(const Value: integer);
    procedure Clear;
    procedure Hide;
    procedure Log(const s: string; LogType: TBoldLogType);
    procedure ProgressStep;
    procedure Show;
    procedure Sync;
    procedure ProcessInterruption;
    procedure StartLog(const sessionName: String);
    procedure EndLog;
    procedure SetEnabled(const Value: Boolean);
    function GetEnabled: Boolean;

    property Enabled: Boolean read GetEnabled write SetEnabled;
  end;

var
  // This construct is used to protect the EnsureDataModule from having to be rewritten.
  TheDataModule: TdmModelEvolution absolute dmModelEvolution;

{$R *.DFM}

procedure EnsureDataModule;
  procedure NewDb(systemhandle: TBoldSystemHandle);
  begin
    (systemHandle.PersistenceHandle as TBoldPersistenceHandleDB).CreateDatabase;
    (systemHandle.PersistenceHandle as TBoldPersistenceHandleDB).CreateDataBaseSchema;
  end;

begin
  // Uncomment EnsureDM_Model if the model in MAIN is used.
  //Ensuredm_Model;

  if not Assigned(TheDataModule) then
  begin
    TheDataModule := TdmModelEvolution.Create(Application);
    NewDB(TheDataModule.BoldSystemHandle1);
    NewDB(TheDataModule.BoldSystemHandle2);
    NewDB(TheDataModule.bshAttrWithDefault);
    NewDB(TheDataModule.bshToBeRem);
//   TheDataModule.BoldSystemHandle1.Active := True;
//    TheDataModule.BoldSystemHandle2.Active := True;
//    TheDataModule.bshAttrWithDefault.Active := True;
//    TheDataModule.bshNoAttr.Active := True;
    assert( fileExists(TheDataModule.bmToBeRemFile.ToBeRemovedInfoFileName), 'Some one moved the file with ToBeRemoveInfo');
//    TheDataModule.bshToBeRem.Active := True;
  end;
end;

procedure EnsureSetup;
begin
//  if DoneSetup then Exit;

  EnsureDataModule;

  // Your global setup code here

end;

{ TLocalTestCase }

procedure TLocalTestCase.SetUp;
begin
  inherited;
  EnsureSetup;
end;

procedure TLocalTestCase.TearDown;
begin
  inherited;
  FreeAndNil(ThedataModule);
end;

{ TModelEvolutionTest }

class procedure TModelEvolutionTest.Suit(aSuite: TBoldTestSuite);
begin
  inherited;
  // Testcases are registered with name of published method
  // and a string reference to the tested feature (specification, requirement, bug

//  --Add tests here--
  ASuite.AddTest(CreateWithComment('TestToBeRemoved' , '7.4.1'));
  ASuite.AddTest(CreateWithComment('TestToBeRemovedFile' , '7.4.2'));
  ASuite.AddTest(CreateWithComment('TestMappingGeneration' , '7.1.1'));
  ASuite.AddTest(CreateWithComment('TestReadnWriteMapping' , '7.1.1, 7.2.1, 7.2.2'));
  ASuite.AddTest(CreateWithComment('TestPSDescriptionNames' , '7.1.1, 7.2.1, 7.2.2'));
  ASuite.AddTest(CreateWithComment('TestPersistence' , '7.1.1, 7.2.1, 7.2.2'));
  ASuite.AddTest(CreateWithComment('TestDeprecatedInMappingInfo' , '7.3.1'));
  ASuite.AddTest(CreateWithComment('TestDeprecatedNotInRT' , '7.3.2'));
  ASuite.AddTest(CreateWithComment('TestDefaultColumnValues' , '7.1.2, 7.1.3'));
  ASuite.AddTest(CreateWithComment('TestTemporal' , '7.6.1'));
end;

class function TModelEvolutionTest.Suite: ITestSuite;
begin
  Result := inherited Suite;
  SetCommentForTest(Result, 'TestToBeRemoved' , '7.4.1');
  SetCommentForTest(Result, 'TestToBeRemovedFile' , '7.4.2');
  SetCommentForTest(Result, 'TestMappingGeneration' , '7.1.1');
  SetCommentForTest(Result, 'TestReadnWriteMapping' , '7.1.1, 7.2.1, 7.2.2');
  SetCommentForTest(Result, 'TestPSDescriptionNames' , '7.1.1, 7.2.1, 7.2.2');
  SetCommentForTest(Result, 'TestPersistence' , '7.1.1, 7.2.1, 7.2.2');
  SetCommentForTest(Result, 'TestDeprecatedInMappingInfo' , '7.3.1');
  SetCommentForTest(Result, 'TestDeprecatedNotInRT' , '7.3.2');
  SetCommentForTest(Result, 'TestDefaultColumnValues' , '7.1.2, 7.1.3');
  SetCommentForTest(Result, 'TestTemporal' , '7.6.1');
end;

procedure TModelEvolutionTest.TestDefaultColumnValues;
var
  anObject: TBoldObject;
begin
  TheDataModule.bshNoAttr.Active := True;
  TheDataModule.bshAttrWithDefault.Active := True;
  dmModelEvolution.bshNoAttr.System.CreateNewObjectByExpressionName('TestClass');
  dmModelEvolution.bshNoAttr.UpdateDatabase;

  Assert(dmModelEvolution.bshAttrWithDefault.System.ClassByExpressionName['TestClass'].Count = 1);
  anObject := dmModelEvolution.bshAttrWithDefault.System.ClassByExpressionName['TestClass'].BoldObjects[0];
  assert((anObject.BoldMemberByExpressionName['NewInteger'] as TBAInteger).asInteger = 12, 'Default integer value');
  assert(anObject.BoldMemberByExpressionName['NewString'].asString = 'a default string', 'Default string value');
  TheDataModule.bshNoAttr.Active := False;
  TheDataModule.bshAttrWithDefault.Active := False;
end;

procedure TModelEvolutionTest.TestDeprecatedInMappingInfo;
var
  MappingInfo: TBoldSQLMappingInfo;
  MemberMappings: TBoldMemberMappingArray;
  OSMappings: TBoldObjectStorageMappingArray;
  AIMappings: TBoldAllInstancesMappingArray;

begin
 // the mappings will only be in the database, not in the mnapping derived from memory, so we must activate the PHandle first.
  dmModelEvolution.BoldPersistenceHandleDB6.Active := true;
  MappingInfo := dmModelEvolution.BoldPersistenceHandleDB6.PersistenceControllerDefault.PersistenceMapper.MappingInfo;

  // deprecated classes
  AIMappings := MappingInfo.GetAllInstancesMapping('RemovedMiddleClass');
  assert( length(AIMappings) = 1);
  MemberMappings := MappingInfo.GetMemberMappings('RemovedMiddleClass', 'Attr2');
  assert(length(MemberMappings) = 1);
  MemberMappings := MappingInfo.GetMemberMappings('LowerClass', 'Attr2');
  assert(length(MemberMappings) = 1);

  MemberMappings := MappingInfo.GetMemberMappings('A', 'RemovedMiddleClass');
  assert(length(MemberMappings) = 1);

  OSMappings := MappingInfo.GetObjectStorageMapping('LowerClass');
  Assert( length(OSMappings) = 3);

  // deprecated attributes
  MemberMappings := MappingInfo.GetMemberMappings('UpperClass', 'RemovedAttr');
  assert(length(MemberMappings) = 1);
  MemberMappings := MappingInfo.GetMemberMappings('RemovedMiddleClass', 'RemovedAttr');
  assert(length(MemberMappings) = 1);
  MemberMappings := MappingInfo.GetMemberMappings('LowerClass', 'RemovedAttr');
  assert(length(MemberMappings) = 1);

  // deprecated linkclasses
  AIMappings := MappingInfo.GetAllInstancesMapping('RemovedLinkClass');
  assert( length(AIMappings) = 1);
  MemberMappings := MappingInfo.GetMemberMappings('RemovedLinkClass', 'a1');
  assert(length(MemberMappings) = 1);
  MemberMappings := MappingInfo.GetMemberMappings('RemovedLinkClass', 'b1');
  assert(length(MemberMappings) = 1);

  // deprecated Associations
  AIMappings := MappingInfo.GetAllInstancesMapping('ImplRemLClass');
  assert( length(AIMappings) = 1);
  MemberMappings := MappingInfo.GetMemberMappings('ImplRemLClass', 'a2');
  assert(length(MemberMappings) = 1);
  MemberMappings := MappingInfo.GetMemberMappings('ImplRemLClass', 'b2');
  assert(length(MemberMappings) = 1);


  dmModelEvolution.BoldPersistenceHandleDB6.Active := false;

end;

procedure TModelEvolutionTest.TestDeprecatedNotInRT;
var
  SystemTypeINfo: TBoldSystemTypeInfo;
  ClassTypeInfo: TBoldClassTypeINfo;
  MemberRTInfo: TBoldMemberRTInfo;
begin
  SystemTypeInfo := TheDataModule.BoldSystemTypeInfoHandle3.StaticSystemTypeInfo;
  // deprecated classes
  ClassTypeInfo := SystemTypeInfo.ClassTypeInfoByExpressionName['RemovedMiddleClass'];
  assert(not assigned(ClassTypeInfo));

  ClassTypeInfo := SystemTypeInfo.ClassTypeInfoByExpressionName['LowerClass'];
  MemberRTInfo := ClassTypeINfo.MemberRTInfoByExpressionName['Attr2'];
  assert(not assigned( MemberRTInfo));
  Assert(ClassTypeInfo.SuperClassTypeInfo.ExpressionName = 'UpperClass');

  ClassTypeInfo := SystemTypeInfo.ClassTypeInfoByExpressionName['A'];
  MemberRTInfo := ClassTypeINfo.MemberRTInfoByExpressionName['RemovedMiddleClass'];
  assert(not assigned( MemberRTInfo));


  // deprecated attributes
  ClassTypeInfo := SystemTypeInfo.ClassTypeInfoByExpressionName['UpperClass'];
  MemberRTInfo := ClassTypeINfo.MemberRTInfoByExpressionName['RemovedAttr'];
  assert(not assigned( MemberRTInfo));

  ClassTypeInfo := SystemTypeInfo.ClassTypeInfoByExpressionName['LowerClass'];
  MemberRTInfo := ClassTypeINfo.MemberRTInfoByExpressionName['RemovedAttr'];
  assert(not assigned( MemberRTInfo));

  // deprecated linkclasses
  ClassTypeInfo := SystemTypeInfo.ClassTypeInfoByExpressionName['RemovedLinkClass'];
  assert(not assigned(ClassTypeInfo));

  ClassTypeInfo := SystemTypeInfo.ClassTypeInfoByExpressionName['A'];
  MemberRTInfo := ClassTypeINfo.MemberRTInfoByExpressionName['b1'];
  assert(not assigned( MemberRTInfo));

  ClassTypeInfo := SystemTypeInfo.ClassTypeInfoByExpressionName['B'];
  MemberRTInfo := ClassTypeINfo.MemberRTInfoByExpressionName['a1'];
  assert(not assigned( MemberRTInfo));

  // deprecated Associations
  ClassTypeInfo := SystemTypeInfo.ClassTypeInfoByExpressionName['ImplRemLClass'];
  assert(not assigned(ClassTypeInfo));

  ClassTypeInfo := SystemTypeInfo.ClassTypeInfoByExpressionName['A'];
  MemberRTInfo := ClassTypeINfo.MemberRTInfoByExpressionName['b2'];
  assert(not assigned( MemberRTInfo));

  ClassTypeInfo := SystemTypeInfo.ClassTypeInfoByExpressionName['B'];
  MemberRTInfo := ClassTypeINfo.MemberRTInfoByExpressionName['a2'];
  assert(not assigned( MemberRTInfo));
end;

procedure TModelEvolutionTest.TestMappingGeneration;
var
  MappingInfo: TBoldDefaultMappingInfo;
  PMapper: TBoldSystemDEfaultMapper;
begin
  PMapper := TBoldSystemDefaultMapper.CreateFromMold(dmModelEvolution.BoldModel1.MoldModel, dmModelEvolution.BoldModel1.TypeNameDictionary, nil, dmModelEvolution.BoldPersistenceHandleDB1.SQLDatabaseConfig, nil);
  MappingInfo := PMapper.MappingInfo as TBoldDefaultMappingInfo;

  TestMappingInfo(MappingInfo);
  PMapper.Free;
end;

procedure TModelEvolutionTest.TestMappingInfo(MappingInfo: TBoldDefaultMappingInfo);
var
  OSArr: TBoldObjectStorageMappingArray;
  AIArr: TBoldAllInstancesMappingArray;
  MemberMappings: TBoldMemberMappingArray;


procedure CompareClassMappings(ClassName1, ClassName2: String);
var
  OSArr, OSArr2: TBoldObjectStorageMappingArray;
  AIArr, aiArr2: TBoldAllInstancesMappingArray;
  MemberMappings, MemberMappings2: TBoldMemberMappingArray;
  i, j: integer;
  found: boolean;
begin
  OsArr := MappingInfo.GetObjectStorageMapping(ClassName1);
  OsArr2 := MappingInfo.GetObjectStorageMapping(ClassName2);
  assert(length(OsArr) = length(osArr2), 'Both classes has the same number of OS-mappings');
  for i := 0 to length(OsArr)-1 do
    assert(OsArr[i].CompareMapping(OsArr2[i]), 'ObjectStoragemapping equal');

  AiArr := MappingInfo.GetAllInstancesMapping(ClassName1);
  AiArr2 := MappingInfo.GetAllInstancesMapping(ClassName2);
  assert(length(AIArr) = length(AIArr2), 'Both classes has the same number of AI-mappings');
  for i := 0 to length(AIArr)-1 do
    assert(AIArr[i].CompareMapping(AIArr2[i]), 'AllInstancesmapping equal');

  MemberMappings := MappingInfo.GetMemberMappings(ClassName1, 'Name');
  MemberMappings2 := MappingInfo.GetMemberMappings(ClassName2, 'name');
  assert(length(MemberMappings) = length(MemberMappings2), 'Both classes has the same number of AI-mappings');

  for i := 0 to length(MemberMappings)-1 do
  begin
    found := false;
    for j := 0 to length(MemberMappings)-1 do
      found := found or MemberMappings[i].CompareMapping(MemberMappings2[j]);
    assert(Found, 'MemberMapping equal');
  end;
end;


begin
  // child mapped classes.

  aiArr := MappingInfo.GetAllInstancesMapping('ChildMapped');
  assert(length(aiArr) = 2, 'ChildMapped class in all subclass tables');
  assert((aiArr[0].tablename = 'ChildA') xor (aiArr[1].tablename = 'ChildA'), 'In first child, once');
  assert((aiArr[0].tablename = 'ChildB') xor (aiArr[1].tablename = 'ChildB'), 'In second child, once');

  OSArr := MappingInfo.GetObjectStorageMapping('ChildMapped');
  assert(length(osArr) = 1, 'Childmapped class exists in parent tables');
  assert(osArr[0].tablename = 'BOLD_OBJECT', 'In first parent (root)');

{  MemberMapping := MappingInfo.GetMemberMapping('ChildMapped', 'name');
  assert(not assigned(memberMapping), 'No member mapping for childmapped attributes');
}
  MemberMappings := MappingInfo.GetMemberMappings('ChildA', 'name');
  assert(length(memberMappings) = 1, 'inherited childmapped attributes has 1 mapping');
  assert(membermappings[0].tableName = 'ChildA', 'Child mapped attribute is in our table');
  assert(membermappings[0].Columns = 'name', 'Child mapped attribute has correct columnname');

  // parentmapped classes
  aiArr := MappingInfo.GetAllInstancesMapping('ParentMapped');
  assert(length(aiArr) = 1, 'ParentMapped is only in parent class');
  assert((aiArr[0].tablename = 'Parent') , 'Parentmapped class has correct tablename');

  OSArr := MappingInfo.GetObjectStorageMapping('ParentMapped');
  assert(length(osArr) = 2, 'ParentMapped class exists in parent tables');
  assert((OSArr[0].tablename = 'Parent') xor (OSArr[1].tablename = 'Parent'), 'In Closest parent');
  assert((OSArr[0].tablename = 'BOLD_OBJECT') xor (OSArr[1].tablename = 'BOLD_OBJECT'), 'In root parent');

  MemberMappings := MappingInfo.GetMemberMappings('ParentMapped', 'name');
  assert(length(memberMappings)=1, 'Parent mapped attributes has 1 mapping info');
  assert(membermappings[0].tableName = 'Parent', 'Parant mapped attribute is in parent table');
  assert(membermappings[0].Columns = 'name', 'Parent mapped attribute has correct columnname');

  // formernames for classes

  CompareClassMappings('ParentMapped', 'oldParentClassName');
  CompareClassMappings('ChildMapped', 'oldChildClassName');

  // FormerNames for assocs

  CompareClassMappings('Assoc', 'oldAssoc');

  // Formernames for attributes

  assert(MappingInfo.GetMemberMappings('ChildA', 'name')[0].CompareMapping(MappingInfo.GetMemberMappings('ChildA', 'oldName')[0]),
     'former names for Attributes map to the same as the new name');

  assert(MappingInfo.GetMemberMappings('ParentMapped', 'name')[0].CompareMapping(MappingInfo.GetMemberMappings('ParentMapped', 'oldName')[0]),
      'former names for Attributes map to the same as the new name');

  // both class and attribute changed name...
  assert(MappingInfo.GetMemberMappings('ParentMapped', 'name')[0].CompareMapping(MappingInfo.GetMemberMappings('oldParentClassname', 'oldName')[0]),
     'former names for Attributes map to the same as the new name');
end;

procedure TModelEvolutionTest.TestPersistence;
var
  aSystem: TBoldSystem;
  aParentMapped: TParentMapped;
//  aParent: TParent;
  aChildA: TChildA;
  aChildB: TChildB;
begin
  dmModelEvolution.bshTest.Active := True;
  aSystem := dmModelEvolution.bshTest.System;

  aParentMapped := TParentMapped.Create(aSystem);
  aParentMapped.name := 'ParentMapped';
  TParent.Create(aSystem); //  aParent := TParent.Create(aSystem);
  aChildA := TChildA.Create(aSystem);
  aChildB := TChildB.Create(aSystem);
  aParentMapped.ChildMapped.Add(aChildA);
  aParentMapped.ChildMapped.Add(aChildB);

  aSystem.UpdateDatabase;

  dmModelEvolution.bshTest.Active := False;
  dmModelEvolution.bshTest.Active := True;
  aSystem := dmModelEvolution.bshTest.System;

  assert(aSystem.ClassByExpressionName['ParentMapped'].count = 1, 'AllInstances of parent mapped class');
  assert(aSystem.ClassByExpressionName['Parent'].count = 2, 'AllInstances of parent class');
  assert(aSystem.ClassByExpressionName['ChildMapped'].count = 2, 'AllInstances of child mapped class');
  aParentMapped := aSystem.ClassByExpressionName['ParentMapped'].BoldObjects[0] as TParentMapped;
  assert(aParentMapped.ChildMapped.Count = 2, 'Association');
  assert(aParentMapped.Name = 'ParentMapped', 'Parent mapped attribute');
  dmModelEvolution.bshTest.Active := False;
end;

procedure TModelEvolutionTest.TestPSDescriptionNames;

  procedure CompareMemberMapperPSDescriptions(Mapper1, Mapper2: TBoldMemberSQLMapper);
  var
    i: Integer;
  begin
    if assigned(Mapper1) and assigned(Mapper2) then
    begin
      assert(Mapper1.ColumnDescriptions.Count = Mapper2.ColumnDescriptions.Count, 'Column count');
      for i := 0 to Mapper1.ColumnDescriptions.Count-1 do
        assert(Mapper1.ColumnDescriptions[i].SQLName = Mapper2.ColumnDescriptions[i].SQLName, 'Column Name');
    end;
  end;

  procedure CompareObjectMapperPSDescriptions(Mapper1, Mapper2: TBoldObjectSQLMapper);
  var
    i: Integer;
  begin
    assert(Mapper1.AllTables.Count = Mapper2.AllTables.Count, 'AllTable count');
    for i := 0 to Mapper1.AllTables.Count-1 do
      assert(Mapper1.AllTables[i].SQLName = Mapper2.AllTables[i].SQLName, 'Table name');
    assert((not assigned(Mapper1.MainTable)) or
           (Mapper1.MainTable.SQLName = Mapper2.MainTable.SQLName), 'Main table name');
    for i := 0 to Mapper1.MemberPersistenceMappers.Count-1 do
      CompareMemberMapperPSDescriptions(Mapper1.MemberPersistenceMappers[0] as TBoldMemberSQLMapper,
                                        Mapper2.MemberPersistenceMappers[0] as TBoldMemberSQLMapper);
  end;

var
  i: Integer;
  Mapper1, Mapper2: TBoldSystemDefaultMapper;
begin
  TheDataModule.BoldSystemHandle1.Active := True;
  TheDataModule.BoldSystemHandle2.Active := True;
  Mapper1 := dmModelEvolution.BoldPersistenceHandleDB1.PersistenceControllerDefault.PersistenceMapper;
  dmModelEvolution.BoldPersistenceHandleDB5.Active := True;
  Mapper2 := dmModelEvolution.BoldPersistenceHandleDB5.PersistenceControllerDefault.PersistenceMapper;
  for i := 0 to Mapper1.ObjectPersistenceMappers.Count-1 do
    CompareObjectMapperPSDescriptions(Mapper1.ObjectPersistenceMappers[i] as TBoldObjectSQLMapper,
                                      Mapper2.ObjectPersistenceMappers[i] as TBoldObjectSQLMapper);
  TheDataModule.BoldSystemHandle1.Active := False;
  TheDataModule.BoldSystemHandle2.Active := False;
end;

procedure TModelEvolutionTest.TestReadnWriteMapping;
var
  Mapping: TBoldDefaultMappingInfo;
begin
  Mapping := dmModelEvolution.BoldPersistenceHandleDB1.PersistenceControllerDefault.PersistenceMapper.MappingInfo as TBoldDefaultMappingInfo;
  Mapping.WriteDataToDB(dmModelEvolution.BoldPersistenceHandleDB1.DatabaseInterface);
  Mapping := TBoldDefaultMappingInfo.Create('BOLD', -1, nccDefault);
  Mapping.ReadDataFromDB(dmModelEvolution.BoldPersistenceHandleDB1.DataBaseInterface, true, true);
  TestMappingInfo(Mapping);
  Mapping.Free;
end;

procedure TModelEvolutionTest.TestTemporal;
var
  anObj: TBoldObject;
begin
  TheDataModule.bshAttrWithDefault.Active := True;
  TheDataModule.bshNoAttr.Active := True;
  if dmModelEvolution.bshAttrWithDefault.System.ClassByExpressionName['TestClass'].Count < 1 then
    dmModelEvolution.bshAttrWithDefault.System.CreateNewObjectByExpressionName('TestClass');
  while dmModelEvolution.bshAttrWithDefault.System.ClassByExpressionName['TestClass'].Count > 1 do
    dmModelEvolution.bshAttrWithDefault.System.ClassByExpressionName['TestClass'].BoldObjects[0].Delete;

  anObj := dmModelEvolution.bshAttrWithDefault.System.ClassByExpressionName['TestClass'].BoldObjects[0];
  anObj.BoldMemberByExpressionName['NewString'].asString := 'Some other string value';
  dmModelEvolution.bshAttrWithDefault.UpdateDatabase;

  anObj := dmModelEvolution.bshNoAttr.System.ClassByExpressionName['TestClass'].BoldObjects[0];
  anObj.BoldMemberByExpressionName['AnAttr'].AsString := 'just to make it dirty';
  dmModelEvolution.bshNoAttr.UpdateDatabase;

  dmModelEvolution.bshAttrWithDefault.Active := false;
  dmModelEvolution.bshAttrWithDefault.Active := True;
  anObj := dmModelEvolution.bshAttrWithDefault.System.ClassByExpressionName['TestClass'].BoldObjects[0];
  assert(anObj.BoldMemberByExpressionName['NewString'].asString = 'Some other string value', 'Unknown attribute gets copied to new version of object');
  TheDataModule.bshAttrWithDefault.Active := False;
  TheDataModule.bshNoAttr.Active := False;
end;

procedure TModelEvolutionTest.TestToBeRemoved;
var
  aLog: TMyLogReceiver;
  anObject: TBoldObject;
  aString: string;
begin
  TheDataModule.bshToBeRem.Active := True;
  aLog := TMyLogReceiver.Create;
  BoldLog.RegisterLogReceiver(aLog);
  try
    assert(aLog.fLogCount = 0, 'Ensuring precondition');
    assert(dmModelEvolution.bmToBeRem.MoldModel.GetClassByName('AClassToBeRemoved').EvolutionState = esToBeRemoved, 'EvolutionState of class specified as ToBeRemoved in Model');
    dmModelEvolution.bshToBeRem.System.CreateNewObjectByExpressionName('AClassToBeRemoved'); //anObject := dmModelEvolution.bshToBeRem.System.CreateNewObjectByExpressionName('AClassToBeRemoved');
    assert(aLog.fLogCount = 1, 'Warn for creating instance of To Be Removed class');
    anObject := dmModelEvolution.bshToBeRem.System.CreateNewObjectByExpressionName('TestClass');
    assert(aLog.fLogCount = 1, 'Nothing logged yet...');
    anObject.BoldMemberByExpressionName['AnAttrToBeRemoved'].AsString := 'Some value';
    assert(aLog.fLogCount = 2, 'Warn for writing to To Be Removed attribute');

    dmModelEvolution.bshToBeRem.UpdateDatabase;
    dmModelEvolution.bshToBeRem.Active := false;
    dmModelEvolution.bshToBeRem.Active := true;

    assert(aLog.fLogCount = 2, 'Nothing logged yet...');
    dmModelEvolution.bshToBeRem.System.ClassByExpressionName['AClassToBeRemoved'].BoldObjects[0]; //anObject := dmModelEvolution.bshToBeRem.System.ClassByExpressionName['AClassToBeRemoved'].BoldObjects[0];
    assert(aLog.fLogCount = 3, 'Warn for fetching instance of To Be Removed class');
    anObject := dmModelEvolution.bshToBeRem.System.ClassByExpressionName['TestClass'].BoldObjects[0];
    assert(aLog.fLogCount = 3, 'Nothing logged yet...');
    aString := anObject.BoldMemberByExpressionName['AnAttrToBeRemoved'].AsString;
    assert(aLog.fLogCount = 4, 'Warn for reading To Be Removed attribute');
  finally
    BoldLog.UnregisterLogReceiver(aLog);
    TheDataModule.bshToBeRem.Active := True;
    FreeAndNil(aLog);
  end;
end;

procedure TModelEvolutionTest.TestToBeRemovedFile;
begin
  TheDataModule.bshToBeRem.Active := True;
  assert(dmModelEvolution.bmToBeRemFile.MoldModel.GetClassByName('RemClass').EvolutionState = esToBeRemoved, 'EvolutionState of class read from file');
  assert(dmModelEvolution.bmToBeRemFile.MoldModel.GetClassByName('TestClass').EvolutionState = esNormal, 'EvolutionState of normal class not affected');
  assert(dmModelEvolution.bmToBeRemFile.MoldModel.GetClassByName('TestClass').Attributes.ItemsByName['RemAttribute'].EvolutionState = esToBeRemoved, 'EvolutionState of attribute read from file');
  assert(dmModelEvolution.bmToBeRemFile.MoldModel.Associations.ItemsByName['RemAssociation'].EvolutionState = esToBeRemoved, 'EvolutionState of assocation read from file');
  TheDataModule.bshToBeRem.Active := False;
end;

{ TMyLogReceiver }

procedure TMyLogReceiver.Clear;
begin
end;

procedure TMyLogReceiver.EndLog;
begin
end;


function TMyLogReceiver.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

procedure TMyLogReceiver.Hide;
begin
end;

procedure TMyLogReceiver.Log(const s: string; LogType: TBoldLogType);
begin
  inc(fLogCount);
end;

procedure TMyLogReceiver.ProgressStep;
begin
end;

procedure TMyLogReceiver.SetProgress(const Value: integer);
begin
end;

procedure TMyLogReceiver.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
end;

procedure TMyLogReceiver.SetLogHeader(const Value: string);
begin
end;

procedure TMyLogReceiver.SetProgressMax(const Value: integer);
begin
end;

procedure TMyLogReceiver.Show;
begin
end;

procedure TMyLogReceiver.StartLog(const sessionName: String);
begin
end;

procedure TdmModelEvolution.DataModuleCreate(Sender: TObject);
var
  ToBeRemovedFile: TStringList;
begin
  ToBeremovedFile := TStringList.Create;
  ToBeRemovedFile.Add('RemClass');
  ToBeRemovedFile.Add('TestClass.RemAttribute');
  ToBeRemovedFile.Add('RemAssociation');
  ToBeRemovedFile.SaveToFile('ToBeRemovedInfo.txt');
  toBeRemovedFile.Free;
  assert(bmToBeRemFile.ToBeRemovedInfoFileName = 'ToBeRemovedInfo.txt');
end;

procedure TMyLogReceiver.ProcessInterruption;
begin
  // intentionally left blank
end;

procedure TMyLogReceiver.Sync;
begin
end;

initialization
  TestGlobal.RegisterTestCase(TModelEvolutionTest);
//  --Register testcase classes here--

end.
