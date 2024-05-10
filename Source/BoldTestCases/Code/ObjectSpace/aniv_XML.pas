unit aniv_XML;

{$INCLUDE Bold.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  BoldClientHandles, BoldComClientHandles, BoldComServerHandles,
  BoldPersistenceHandle,
  BoldSOAPClientPersistenceHandles, BoldHandle, BoldServerHandles,
  BoldSOAPServerPersistenceHandles, BoldSubscription, BoldHandles,               
  ActnList, BoldHandleAction, BoldActions, BoldDBActions,
  BoldPersistenceHandleDB,
  BoldAbstractComClientPersistenceHandles,
  BoldSystemHandle, dmModel1, TestSuite,
  BoldCondition, BoldDomainElement,
  TestFrameWork,
  BoldId, BoldDefaultId, DB, BoldAbstractDatabaseAdapter,
  BoldAbstractPersistenceHandleDB,
  DBAccess, Uni, BoldDatabaseAdapterUniDAC, System.Actions;

type
  TdmXMLTest = class(TDataModule)
    BoldSystemHandle1: TBoldSystemHandle;
    BoldSOAPServerPersistenceHandle1: TBoldSOAPServerPersistenceHandle;
    BoldSOAPClientPersistenceHandle1: TBoldSOAPClientPersistenceHandle;
    BoldComServerHandle1: TBoldComServerHandle;
    BoldComConnectionHandle1: TBoldComConnectionHandle;
    BoldSystemHandle2: TBoldSystemHandle;
    ActionList1: TActionList;
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
    BoldDatabaseAdapterUniDAC1: TBoldDatabaseAdapterUniDAC;
    UniConnection1: TUniConnection;
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


  TAniv_XMLStreaming = class(TBoldTestCase)
  private
    procedure CompareIdLists(IdList1, IdList2: TBoldObjectIdList);
    procedure CompareIds(Id1, Id2: TBoldObjectId);
    procedure CompareClassIds(Id1, Id2: TBoldObjectId);
    procedure CompareClassConditions(Cond1, Cond2: TBoldConditionWithClass);
    procedure CompareSQLConditions(SQLCond1, SQLCond2: TBoldSQLCondition);
    procedure CompareMemberIdLists(MemberIdList1, MemberIdList2: TBoldMemberIdList);
    procedure CompareMemberIds(MemberId1, Memberid2: TBoldMemberId);
    procedure CompareCPConditions(CPCond1, CPCond2: TBoldChangePointCondition);
    procedure CompareTSConditions(TSCond1, TSCond2: TBoldTimestampCondition);
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure IdList;
    procedure Conditions;
    procedure ValueSpace;
    procedure SOAPPersistence;
    procedure IndirectSingleLinks;
  end;




var
  dmXMLTest: TdmXMLTest = nil;

implementation

uses
  {$IFDEF OXML}OXmlPDOM{$ELSE}Bold_MSXML_TLB{$ENDIF},
  BoldXMLStreaming,
  BoldDefs,
  TestModel1,
  BoldSystem,
  BoldAttributes,
//  joho_ocl2Sql,
  BoldFreeStandingValues,
  BoldDefaultXMLStreaming,
  BoldDefaultStreamNames;

{$R *.DFM}

procedure EmptyDb(BoldSystem: TBoldSystem);
begin
  while BoldSystem.Classes[0].Count > 0 do
    BoldSystem.Classes[0].BoldObjects[0].Delete;
  BoldSystem.UpdateDatabase;
end;



procedure EnsureDM;
begin
  Ensuredm_Model;
  if not assigned(dmXMLTest) then
  begin
    Application.Initialize;
    dmXMLTest := TdmXMLTest.Create(Application);
    dmXMLTest.BoldDatabaseAdapterUniDac1.CreateDatabase;
    dmXMLTest.BoldPersistenceHandleDB1.CreateDataBaseSchema;
    dmXMLTest.BoldSystemHandle1.Active := True;
//    dmXMLTest.BoldSystemHandle2.Active := True;
  end;
end;


{ TAniv_XMLStreaming }

procedure TAniv_XMLStreaming.CompareClassConditions(Cond1,
  Cond2: TBoldConditionWithClass);
begin

end;

procedure TAniv_XMLStreaming.CompareClassIds(Id1, Id2: TBoldObjectId);
begin
  assert(assigned(id2), 'No id');
  assert(Id1.TopSortedIndex = Id2.TopSortedIndex, 'Wrong topsorted index');
  assert(Id1.TopSortedIndexExact = Id2.TopSortedIndexExact, 'Wrong exactness');
end;

procedure TAniv_XMLStreaming.CompareCPConditions(CPCond1,
  CPCond2: TBoldChangePointCondition);
begin
  CompareIdLists(CPCond1.IdList, CPCond2.IdList);
  CompareMemberIdLists(CPCond1.MemberIdList, CPCond1.MemberIdList);
  assert(CPCond1.StartTime = CPCond2.StartTime, 'Change point condition start time');
  assert(CPCond1.EndTime = CPCond2.EndTime, 'Change point condition end time');
end;

procedure TAniv_XMLStreaming.CompareIdLists(IdList1,
  IdList2: TBoldObjectIdList);
var
  i: Integer;
begin
  assert(IdList1.Count = IdList2.Count, 'Wrong count');
  for i := 0 to IdList1.Count-1 do
    CompareIds(IdList1[i], IdList2[i]);
end;

procedure TAniv_XMLStreaming.CompareIds(Id1, Id2: TBoldObjectId);
begin
  CompareClassIds(Id1, Id2);
  assert(Id1.ClassType = Id2.ClassType, 'Id has wrong class');
  assert(Id1.IsEqual[Id2], 'Id different');
end;

procedure TAniv_XMLStreaming.CompareMemberIdLists(MemberIdList1,
  MemberIdList2: TBoldMemberIdList);
var
  i: Integer;
begin
  assert(MemberIdList1.Count = MemberIdlist2.Count, 'Wrong count');
  for i := 0 to MemberIdList1.Count-1 do
    CompareMemberIds(MemberIdList1[i], MemberidList2[i]);
end;

procedure TAniv_XMLStreaming.CompareMemberIds(MemberId1, Memberid2: TBoldMemberId);
begin
  assert(MemberId1.MemberIndex = MemberId2.MemberIndex, 'Wrong member index');
end;

procedure TAniv_XMLStreaming.CompareSQLConditions(SQLCond1,
  SQLCond2: TBoldSQLCondition);
begin
  CompareClassConditions(SQLCond1, SQLCond2);
  assert(SQLCond1.WhereFragment = SQLCond2.WhereFragment, 'Where-clause wrong');
  assert(SQLCond1.OrderBy = SQLCond2.OrderBy, 'Order-By wrong');
  assert(SQLCond1.Params.IsEqual(SQLCond2.Params), 'Params wrong');
end;

procedure TAniv_XMLStreaming.CompareTSConditions(TSCond1,
  TSCond2: TBoldTimestampCondition);
begin
  CompareClassConditions(TSCond1, TSCond2);
  Assert(TSCond1.Time = TSCond2.Time, 'Timestamp condition time wrong');
end;

procedure TAniv_XMLStreaming.Conditions;
var
  aMgr: TBoldDefaultXMLStreamManager;
  aDoc: TDomDocument;
  aNode: TBoldXMLNode;
  anXML: string;
  ParseError: IXMLDOMParseError;
  SQLCond1, SQLCond2: TBoldSQLCondition;
  CPCond1, CPCond2: TBoldChangePointCondition;
  TSCond1, TSCond2: TBoldTimestampCondition;
  anId: TBoldDefaultId;
begin
  aDoc := TDOMDocument.Create(nil);
  aMgr := TBoldDefaultXMLStreamManager.Create(TBoldDefaultXMLStreamerRegistry.MainStreamerRegistry, dm_Model1.BoldModel1.MoldModel);
  aNode := aMgr.NewRootNode(aDoc, 'ConditionTest');

  SQLCond1 := TBoldSQLCondition.create;
  SQLCond1.TopSortedIndex := 2;
  SQLCond1.WhereFragment := 'a';
  SQLCond1.OrderBy := 'b';
  SQLCond1.Params := TParams.Create(nil);
  SQLCond1.Params.CreateParam(ftString, 'aString', ptInput).AsString := 'string value';
  SQLCond1.Params.CreateParam(ftInteger, 'anInt', ptInput).AsInteger := 17;
  SQLCond1.Params.CreateParam(ftBoolean, 'aNullBool', ptInput);
  aNode.WriteSubNodeObject('SQLCondition', '', SQLCond1);

  CPCond1 := TBoldChangePointCondition.Create;
  CPCond1.IdList := TBoldObjectIdList.Create;
  anId := TBoldDefaultID.CreateWithClassID(2, false);
  anId.AsInteger := 3;
  CPCond1.IdList.Add(anId);
  CPCond1.StartTime := 4;
  CPCond1.EndTime := 6;
  CPCond1.MemberIdList := TBoldMemberIdList.Create;
  CPCond1.MemberIdList.Add(TBoldMemberID.create(7));
  aNode.WriteSubNodeObject('ChangePointCondition', '', CPCond1);

  TSCond1 := TBoldTimestampCondition.create;
  TSCond1.TopSortedIndex := 1;
  TSCond1.Time := 42;
  aNode.WriteSubNodeObject('TimestampCondition', '', TSCond1);

  anXML := aNode.XMLDomElement.ownerDocument.xml;

  aNode.Free;
  aDoc.Free;

  aDoc := TDOMDocument.Create(nil);
  aDoc.async := false;
  aDoc.loadXML(anXML);

  ParseError := aDoc.parseError;
  assert((not Assigned(ParseError)) or (ParseError.errorCode = 0), 'Error reading/parsing XMI file');

  aNode := aMgr.GetRootNode(aDoc, 'ConditionTest');

  SQLCond2 := aNode.ReadSubNodeObject('SQLCondition', '') as TBoldSQLCondition;
  CompareSQLConditions(SQLCond1, SQLCond2);
  CPCond2 := aNode.ReadSubNodeObject('ChangePointCondition', '') as TBoldChangePointCondition;
  CompareCPConditions(CPCond1, CPCond2);
  TSCond2 := aNode.ReadSubNodeObject('TimestampCondition', '') as TBoldTimestampCondition;
  CompareTSConditions(TSCond1, TSCond2);
  // Free
  aDoc.Free;
  aMgr.Free;
  CPCond1.IdList.Free;
  CPCond1.MemberIdList.Free;
  CPCond1.Free;
  TSCond1.Free;
  SQLCond1.Params.Free;
  SQLCond1.Free;
  if Assigned(CPCond2.IdList) then
    CPCond2.IdList.Free;
  if Assigned(CPCond2.MemberIdList) then
    CPCond2.MemberIdList.Free;    
  CPCond2.Free;
  TSCond2.Free;
  SQLCond2.Params.Free;
  SQLCond2.Free;
end;

procedure TAniv_XMLStreaming.IdList;
var
  anId: TBoldDefaultId;
  aTimestampedId: TBoldTimestampedDefaultId;
  anInternalId: TBoldInternalObjectId;
  IdList1, IdList2: TboldObjectIdList;
  aMemberId: TBoldMemberId;
  MemberIdList1, MemberIdList2: TBoldMemberIdList;

  aMgr: TBoldDefaultXMLStreamManager;
  aDoc: TDomDocument;
  aNode: TBoldXMLNode;
  anXML: string;
  ParseError: IXMLDOMParseError;
begin
  aDoc := TDOMDocument.Create(nil);
  anId := TBoldDefaultID.CreateWithClassID(1, True);
  anId.AsInteger := 2;
  aTimestampedId := TBoldTimestampedDefaultId.createWithTimeAndClassId(13, 1, true);
  aTimestampedId.AsInteger := 3;
  anInternalId := TBoldInternalObjectId.CreateWithClassIDandInternalId(4, 1, true);
  IdList1 := TBoldObjectIdList.Create;
  IdList1.Add(anId);
  IdList1.Add(anInternalId);
  IdList1.Add(aTimestampedId);

  aMemberId := TBoldMemberID.create(2);
  MemberIdList1 := TBoldMemberIdList.Create;
  MemberIdList1.Add(aMemberId);

  aMgr := TBoldDefaultXMLStreamManager.Create(TBoldDefaultXMLStreamerRegistry.MainStreamerRegistry, dm_Model1.BoldModel1.MoldModel);
  aNode := aMgr.NewRootNode(aDoc, 'IdListTest');
  aNode.WriteSubNodeObject('ObjectIdList', '', IdList1);
  aNode.WriteSubNodeObject('MemberIdList', BOLDMEMBERIDLISTNAME, MemberIdList1);

  anXML := aNode.XMLDomElement.ownerDocument.xml;

  aNode.Free;
  aMgr.Free;
  aDoc.Free;

  aDoc := TDOMDocument.Create(nil);
  aDoc.async := false;
  aDoc.loadXML(anXML);

  ParseError := aDoc.parseError;
  assert((not Assigned(ParseError)) or (ParseError.errorCode = 0), 'Error reading/parsing XMI file');

  aMgr := TBoldDefaultXMLStreamManager.Create(TBoldDefaultXMLStreamerRegistry.MainStreamerRegistry, dm_Model1.BoldModel1.MoldModel);
  aNode := aMgr.GetRootNode(aDoc, 'IdListTest');
  IdList2 := aNode.ReadSubNodeObject('ObjectIdList', '') as TBoldObjectIdList;

  assert(assigned(IdList2), 'No object id list returned');
  CompareIdLists(IdList1, IdList2);

  MemberIdList2 := aNode.ReadSubNodeObject('MemberIdList', BOLDMEMBERIDLISTNAME) as TBoldMemberIdList;
  assert(assigned(MemberIdList2), 'No member id list returned');
  CompareMemberIdLists(MemberIdList1, MemberIdList2);

  //Free
  aDoc.Free;
  aMgr.Free;
  aTimeStampedId.Free;
  IdList1.Free;
  IdList2.Free;
  MemberIdList1.Free;
  MemberIdList2.Free;
end;

procedure TAniv_XMLStreaming.IndirectSingleLinks;
var
  Thing1, Thing2: TThing;
  id1, id2, id3: TBoldObjectId;
begin
  Thing1 := TThing.Create(dmXMLTest.BoldSystemHandle1.System);
  Thing2 := TThing.Create(dmXMLTest.BoldSystemHandle1.System);
  Thing1.one := Thing2;
  dmXMLTest.BoldSystemHandle1.UpdateDatabase;
  id1 := Thing1.BoldObjectLocator.BoldObjectID.Clone;
  id2 := Thing2.BoldObjectLocator.BoldObjectID.Clone;
  id3 := Thing1.oneLinkClass.BoldObjectLocator.BoldObjectID.Clone;
  dmXMLTest.BoldSystemHandle1.Active := false;
  dmXMLTest.BoldSystemHandle1.Active := true;
  Thing1 := dmXMLTest.BoldSystemHandle1.System.EnsuredLocatorByID[Id1].EnsuredBoldObject as TThing;
  Thing1.one; // fetch indirect single link first
  Thing1.oneLinkClass; // then fetch link object
  assert(Thing1.one.BoldObjectLocator.BoldObjectID.IsEqual[id2]);
  assert(Thing1.oneLinkClass.BoldObjectLocator.BoldObjectID.IsEqual[id3]);
  id1.Free;
  id2.Free;
  id3.Free;
end;

procedure TAniv_XMLStreaming.SetUp;
begin
  inherited;
  EnsureDM;
  dmXMLTest.BoldSystemHandle1.Active := True;
  dmXMLTest.BoldSystemHandle2.Active := True;
end;

procedure TAniv_XMLStreaming.SOAPPersistence;
var
  anObject: TClassA;
  Obj1, Obj2: TClassA;
  IdList1, IdList2: TBoldObjectIdList;
  aCondition: TBoldConditionWithClass;
  aBoldObject: TBoldObject;
begin
  IdList1 := TBoldObjectIdList.Create;
  anObject := TClassA.Create(dmXMLTest.BoldSystemHandle1.System);
  anObject.aString := 'test';
  anObject.aBlob := 'asdaasdfg';
  anObject.aBlobContent := 'sdfgfdf';
  dmXMLTest.BoldSystemHandle1.Updatedatabase;

  IdList1.Add(anObject.BoldObjectLocator.BoldObjectID);

  dmXMLTest.BoldSystemHandle1.Active := false;
  dmXMLTest.BoldSystemHandle1.Active := true;

  aCondition := TBoldConditionWithClass.create;
  aCondition.TopSortedIndex := IdList1[0].TopSortedIndex;
  IdList2 := TBoldObjectIdList.Create;

  dmXMLTest.BoldSOAPClientPersistenceHandle1.PersistenceController.PMFetchIDListWithCondition(IdList2,
    dmXMLTest.BoldSystemHandle1.System.AsIBoldvalueSpace[bdepPMIn], fmNormal, aCondition, -1);
  assert(IdList2.Count = 1, 'FetchIdListWithCondition: returned count');
  assert(IdList2[0].IsEqual[IdList1[0]], 'FetchIdListWithCondition: Wrong object id');

  aBoldObject := dmXMLTest.BoldSystemHandle1.System.EnsuredLocatorByID[IdList2[0]].EnsuredBoldObject;


  assert(assigned(aBoldObject), 'Fetch: Object not fetched');
  assert(aBoldObject is TClassA, 'Fetch: Object of wrong class');
  anObject := aBoldObject as TClassA;
  assert(anObject.aString = 'test', 'Fetch: Wrong attribute value');

  // test for singleSingle Embedd/Embedd over XML/SOAP
  // setup of objects
  dmXMLTest.BoldSystemHandle1.Active := true;
  EmptyDb(dmXMLTest.BoldSystemHandle1.System);
  Obj1 := TClassA.Create(dmXMLTest.BoldSystemHandle1.System);
  Obj2 := TClassA.Create(dmXMLTest.BoldSystemHandle1.System);
  Obj1.Next := Obj2;
  dmXMLTest.BoldSystemHandle1.UpdateDatabase;
  dmXMLTest.BoldSystemHandle1.Active := false;

  // actual test
  dmXMLTest.BoldSystemHandle1.Active := true;

  // this line might go into an endless recursion, thats what the test tests
  dmXMLTest.BoldSystemHandle1.System.ClassByExpressionName['ClassA'].EnsureObjects;
  obj1 := dmXMLTest.BoldSystemHandle1.System.ClassByExpressionName['ClassA'].BoldObjects[0] as TClassA;
  obj2 := dmXMLTest.BoldSystemHandle1.System.ClassByExpressionName['ClassA'].BoldObjects[1] as TClassA;
  Obj1.M_next.EnsureContentsCurrent;
  Obj2.M_next.EnsureContentsCurrent;
  Obj1.M_Previous.EnsureContentsCurrent;
  Obj2.M_Previous.EnsureContentsCurrent;

  //free
  IdList1.Free;
  IdList2.Free;
  aCondition.Free;
end;

class procedure TAniv_XMLStreaming.Suit(ASuite: TBoldTestSuite);
begin
  inherited;
  ASuite.AddTest(CreateWithComment('IndirectSingleLinks'));
  ASuite.AddTest(CreateWithComment('IdList'));
  ASuite.AddTest(CreateWithComment('Conditions'));
  ASuite.AddTest(CreateWithComment('SOAPPersistence'));
  ASuite.AddTest(CreateWithComment('ValueSpace'));
end;


class function TAniv_XMLStreaming.Suite: ITestSuite;
begin
  Result := inherited Suite;
  SetCommentForTest(Result, 'IndirectSingleLinks', '');
  SetCommentForTest(Result, 'IdList', '');
  SetCommentForTest(Result, 'Conditions', '');
  SetCommentForTest(Result, 'SOAPPersistence', '');
  SetCommentForTest(Result, 'ValueSpace', '');
end;

procedure TAniv_XMLStreaming.TearDown;
begin
  if dmXMLTest.BoldSystemHandle1.Active then
  begin
    dmXMLTest.BoldSystemHandle1.System.Discard;
    dmXMLTest.BoldSystemHandle1.Active := false;
  end;
  if dmXMLTest.BoldSystemHandle2.Active then
  begin
    dmXMLTest.BoldSystemHandle2.System.Discard;
    dmXMLTest.BoldSystemHandle2.Active := false;
  end;
  FreeAndNil(dmXMLTest);
end;

procedure TAniv_XMLStreaming.ValueSpace;
const
  FloatConst: Double = 0.0031;
var
  aMgr: TBoldDefaultXMLStreamManager;
  aDoc: TDomDocument;
  aNode: TBoldXMLNode;
  anXML: string;
  ParseError: IXMLDOMParseError;

  aClassA, aClassA2: TClassA;
  anIdlist: TBoldObjectIdList;
  DateTimeConst: TDateTime;
begin
  aDoc := TDOMDocument.Create(nil);
  aMgr := TBoldDefaultXMLStreamManager.Create(TBoldDefaultXMLStreamerRegistry.MainStreamerRegistry, dm_Model1.BoldModel1.MoldModel);
  aMgr.IgnorePersistenceState := false;
  aMgr.PersistenceStatesToBeStreamed := [bvpsModified, bvpsCurrent, bvpsInvalid];
//  aMgr.AdjustLinks := false;
  aNode := aMgr.NewRootNode(aDoc, 'ValueSpaceTest');

  DateTimeConst := EncodeDate(1954, 2, 11) + EncodeTime(11, 43, 20, 11);

  aClassA := TClassA.Create(dmXMLTest.BoldSystemHandle1.System);
  aClassA.aString := 'a';
  aClassA.aBoolean := True;
  aClassA.aByte := 12;
  aClassA.aCurrency := 3.12;
  aClassA.aDate := EncodeDate(2000, 08, 31);
  aClassA.aDateTime := DateTimeConst;
  aClassA.aTime := EncodeTime(23, 59, 59, 0);
  aClassA.aFloat := FloatConst;
  aClassA.aInteger := 17;
  aClassA.aShortInt := 127;
  aClassA.aSmallInt := 13;
  aClassA.aWord := 40000;
  aClassA.aBlob := '8wwwrwvå95ye5vtq90' + #1 + #0 + '43tvuu5vy!"#¤%&/()=``</adsf>';
  aClassA.aBlobContent := 'whatever';
  aClassA.M_aBlobContent.ContentType := 'A';
  aClassA.part.AddNew;
  aClassA.partof.AddNew;
  aClassA.parent := TClassA.Create(aClassA.BoldSystem);
  aClassA.child.AddNew;

  anIdList := TBoldObjectIdList.Create;
  anIdList.Add(aClassA.BoldObjectLocator.BoldObjectID);
  anIdlist.Add(aClassA.part[0].BoldObjectLocator.BoldObjectID);
  anIdlist.Add(aClassA.partof[0].BoldObjectLocator.BoldObjectID);
  anIdList.Add(aClassA.partpartpartof[0].BoldObjectLocator.BoldObjectID);
  anIdList.Add(aClassA.partofpartpartof[0].BoldObjectLocator.BoldObjectID);
  anIdlist.Add(aClassA.parent.BoldObjectLocator.BoldObjectID);
  anIdlist.Add(aClassA.child[0].BoldObjectLocator.BoldObjectID);

  aMgr.WriteValueSpace(dmXMLTest.BoldSystemHandle1.System.AsIBoldvalueSpace[bdepContents], anIdlist, nil, aNode);

  anXML := aNode.XMLDomElement.ownerDocument.xml;
{
  with TStringList.Create do
  begin
     // a way to get to see the contents of the xml document
    Text := anXML;
    SaveToFile('c:\test.xml');
    free;
  end;
}
  aNode.Free;
  aDoc.Free;
  aDoc := TDOMDocument.Create(nil);
  aDoc.async := false;
  aDoc.loadXML(anXML);
  ParseError := aDoc.parseError;
  assert((not Assigned(ParseError)) or (ParseError.errorCode = 0), 'Error reading/parsing XMI file');

  aNode := aMgr.GetRootNode(aDoc, 'ValueSpaceTest');

//  aValueSpace := TBoldFreeStandingValueSpace.Create;
//  dmXMLTest.BoldSystemHandle2.System.AsIBoldvalueSpace[bdepPMIn].StartFetchForAll(anidlist, nil);
  aMgr.ReadValueSpace(dmXMLTest.BoldSystemHandle2.System.AsIBoldvalueSpace[bdepContents], aNode);
//  dmXMLTest.BoldSystemHandle2.System.AsIBoldvalueSpace[bdepPMIn].EndFetchForAll(anIdlist, nil);

  aClassA2 := dmXMLTest.BoldSystemHandle2.System.EnsuredLocatorByID[aClassA.BoldObjectLocator.BoldObjectID].BoldObject as TClassA;

  assert(assigned(aClassA2), 'No object');
  assert(aClassA2.aString = 'a', 'Streaming of string attribute');
  assert(aClassA2.aBoolean = True, 'Streaming of boolean attribute');
  assert(aClassA2.aByte = 12, 'Streaming of byte attribute');
  assert(aClassA2.aCurrency = 3.12, 'Streaming of currency attribute');
  assert(aClassA2.aDate = EncodeDate(2000, 08, 31), 'Streaming of date attribute');
  assert(aClassA2.aDateTime = DateTimeConst, 'Streaming of datetime attribute');
  assert(aClassA2.aTime = EncodeTime(23, 59, 59, 0), 'Streaming of time attribute');
  assert(aClassA2.aFloat = FloatConst, 'Streaming of float attribute');
  assert(aClassA2.aInteger = 17, 'Streaming of integer attribute');
  assert(aClassA2.aShortInt = 127, 'Streaming of shortint attribute');
  assert(aClassA2.aSmallInt = 13, 'Streaming of smallint attribute');
  assert(aClassA2.aWord = 40000, 'Streaming of word attribute');
  assert(aClassA2.aBlob = '8wwwrwvå95ye5vtq90' + #1 + #0 + '43tvuu5vy!"#¤%&/()=``</adsf>', 'Streaming of blob attribute');
  assert(aClassA2.aBlobContent = 'whatever', 'Streaming of typedblob attribute');
  assert(aClassA2.M_aBlobContent.ContentType = 'A', 'Streaming of blob type');
  assert(aClassA.M_part.Locators[0].BoldObjectID.IsEqual[aClassA2.M_part.Locators[0].BoldObjectID], 'streaming of many-many association');
  assert(aClassA.M_partof.Locators[0].BoldObjectID.IsEqual[aClassA2.M_partof.Locators[0].BoldObjectID], 'streaming of many-many association');
  assert(aClassA.M_parent.Locator.BoldObjectID.IsEqual[aClassA2.M_parent.Locator.BoldObjectID], 'streaming of many-one association');
  assert(aClassA.M_child.Locators[0].BoldObjectID.IsEqual[aClassA2.M_child.Locators[0].BoldObjectID], 'streaming of many-one association');

  //Free
  aDoc.Free;
  aMgr.Free;
  anIdList.Free;
end;

procedure TdmXMLTest.DataModuleCreate(Sender: TObject);
begin
  BoldComConnectionHandle1.Connected := true;
end;

initialization
  TestGlobal.RegisterTestCase(TAniv_XMLStreaming);
//  G_TestXmlStreamingOfNodes := True;

end.
