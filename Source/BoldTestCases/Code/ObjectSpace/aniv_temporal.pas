
unit aniv_temporal;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  TestSuite,
  BoldPersistenceHandle,
  BoldPersistenceHandleDB,
  BoldHandle,
  BoldUMLModelLink,
  BoldUMLRose98Link,
  BoldModel,
  BoldHandles,
  BoldSubscription,
  BoldSystemHandle,
  ActnList,
  BoldHandleAction,
  BoldActions,
  TestFrameWork,
  BoldDBActions,
  BoldAbstractModel,
  BoldAbstractPersistenceHandleDB,
  DB,
  BoldAbstractDatabaseAdapter,
  DBAccess,
  Uni,
  BoldDatabaseAdapterUniDAC,
  System.Actions;

type
  TdmTemporalTest = class(TDataModule)
    BoldSystemHandle1: TBoldSystemHandle;
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
    BoldModel1: TBoldModel;
    BoldUMLRose98Link1: TBoldUMLRoseLink;
    ActionList1: TActionList;
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
    BoldDatabaseAdapterUniDAC1: TBoldDatabaseAdapterUniDAC;
    UniConnection1: TUniConnection;
  private
    { Private declarations }
  public
    { Public declarations }
  end;


  TAniv_temporal1 = class(TBoldTestCase)
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure AtTimeAndState;
    procedure TimeStampOfLatestUpdate;
    procedure BoldTime;
    procedure ReadOnlyness;
    procedure Navigation;
    procedure OCLOperations;
    procedure OCLAsSetMerge;
    procedure OCLAsSetWithSelf;
  end;


var
  dmTemporalTest: TdmTemporalTest = nil;

implementation

{$R *.DFM}

uses
  dmModel1,
  TestModel1_Temporal,
  BoldAttributes,
  BoldElements,
  BoldSystem,
  BoldDefs;


var
  SetupDone: boolean = false;
  TimeStampOfUpdate0: TBoldTimestampType;
  TimeStampOfUpdate1: TBoldTimestampType;
  TimeStampOfUpdate2: TBoldTimestampType;
  TimeStampOfUpdate3: TBoldTimestampType;
  Doc1: TDocument = nil;
  Doc2: TDocument = nil;

procedure EnsureDM;
begin
  Ensuredm_Model;
  if not assigned(dmTemporalTest) then
  begin
    Application.Initialize;
    dmTemporalTest := TdmTemporalTest.Create(Application);
    dmTemporalTest.BoldDatabaseAdapterUniDac1.CreateDatabase;
    dmTemporalTest.BoldPersistenceHandleDB1.CreateDataBaseSchema;
    dmTemporalTest.BoldSystemHandle1.Active := True;
  end;
end;

procedure FreeDM;
begin
  dmTemporalTest.BoldSystemHandle1.System.DiscardPersistent(False);
  FreeAndNil(dmTemporalTest);
  SetUpDone := False;
end;

procedure EnsureSetup;
begin
  if SetupDone then
    exit;

  EnsureDM;
  TimeStampOfUpdate0 := dmTemporalTest.BoldSystemHandle1.System.TimeStampOfLatestUpdate;

  Doc1 := TDocument.Create(dmTemporalTest.BoldSystemHandle1.System);
  Doc1.Title := 'FirstTitle';
  dmTemporalTest.BoldSystemHandle1.UpdateDatabase;
  TimeStampOfUpdate1 := dmTemporalTest.BoldSystemHandle1.System.TimeStampOfLatestUpdate;

  Doc2 := TDocument.Create(dmTemporalTest.BoldSystemHandle1.System);
  Doc2.Title := 'SecondTitle';
  Doc2.Partof := Doc1;
  dmTemporalTest.BoldSystemHandle1.UpdateDatabase;
  TimeStampOfUpdate2 := dmTemporalTest.BoldSystemHandle1.System.TimeStampOfLatestUpdate;

  Doc1.Title := 'ThirdTitle';
  Doc2.Title := 'FourthTitle';
  dmTemporalTest.BoldSystemHandle1.UpdateDatabase;
  TimeStampOfUpdate3 := dmTemporalTest.BoldSystemHandle1.System.TimeStampOfLatestUpdate;

  SetupDone := True;
end;

{ TAniv_temporal1 }

procedure TAniv_temporal1.AtTimeAndState;
var
  Doc1_1: TDocument;
  Doc1_2: TDocument;
  Doc1_3: TDocument;
  Doc2_2: TDocument;
  Doc2_3: TDocument;
begin
  EnsureSetup;

  Doc1_1 := Doc1.AtTime(1) as TDocument;
  Doc1_2 := Doc1.AtTime(2) as TDocument;
  Doc1_3 := Doc1.AtTime(3) as TDocument;
  Doc2_2 := Doc2.AtTime(2) as TDocument;
  Doc2_3 := Doc2.AtTime(3) as TDocument;

  assert(Doc1_1.Title = 'FirstTitle', 'AtTime and state of object (1)');
  assert(Doc1_1.Parts.Count = 0, 'AtTime and state of object (2)');
  assert(Doc1_2.Title = 'FirstTitle', 'AtTime and state of object (3)');
  assert(Doc2_2.Title = 'SecondTitle', 'AtTime and state of object (4)');
  assert(Doc1_2.Parts.Count = 1, 'AtTime and state of object (5)');
  assert(Doc1_2.Parts[0] = Doc2_2, 'AtTime and state of object (6)');
  assert(Doc2_2.PartOf = Doc1_2, 'AtTime and state of object (7)');
  assert(Doc1_3.Title = 'ThirdTitle', 'AtTime and state of object (8)');
  assert(Doc2_3.Title = 'FourthTitle', 'AtTime and state of object (9)');
end;

procedure TAniv_temporal1.BoldTime;
var
  Doc1_1: TDocument;
  Doc1_2: TDocument;
  Doc1_3: TDocument;
  Doc2_2: TDocument;
  Doc2_3: TDocument;
begin
  EnsureSetup;

  Doc1_1 := Doc1.AtTime(1) as TDocument;
  Doc1_2 := Doc1.AtTime(2) as TDocument;
  Doc1_3 := Doc1.AtTime(3) as TDocument;
  Doc2_2 := Doc2.AtTime(2) as TDocument;
  Doc2_3 := Doc2.AtTime(3) as TDocument;

  assert(Doc1_1.BoldTime = 1, 'BoldTime of object (1)');
  assert(Doc1_2.BoldTime = 2, 'BoldTime of object (2)');
  assert(Doc1_3.BoldTime = 3, 'BoldTime of object (3)');
  assert(Doc2_2.BoldTime = 2, 'BoldTime of object (4)');
  assert(Doc2_3.BoldTime = 3, 'BoldTime of object (5)');
end;

procedure TAniv_temporal1.Navigation;
var
  Doc1_1: TDocument;
  Doc1_2: TDocument;
  Doc1_3: TDocument;
  Doc2_2: TDocument;
  Doc2_3: TDocument;
begin
  EnsureSetup;

  Doc1_1 := Doc1.AtTime(1) as TDocument;
  Doc1_2 := Doc1.AtTime(2) as TDocument;
  Doc1_3 := Doc1.AtTime(3) as TDocument;
  Doc2_2 := Doc2.AtTime(2) as TDocument;
  Doc2_3 := Doc2.AtTime(3) as TDocument;

  assert(Doc1_2.Parts.Count > 0);
  assert(Doc1_2.Parts[0].BoldTime = 2, 'Navigation to same time');
  assert(Doc2_2.PartOf.BoldTime = 2, 'Navigation to same time');

  assert(Doc1_3.Parts.Count > 0);
  assert(Doc1_3.Parts[0].BoldTime = 3, 'Navigation to same time');
  assert(Doc2_3.PartOf.BoldTime = 3, 'Navigation to same time');
end;

procedure TAniv_temporal1.OCLAsSetMerge;
const
  cn_One = 'One';
var
  Doc: TDocument;
  vIndirectElem: TBoldIndirectElement;

  procedure InternalAdd;
  var
    Doc1: TDocument;
  begin
    Doc1 := TDocument.Create(dmTemporalTest.BoldSystemHandle1.System);
    Doc1.Title := cn_One;
    Doc.Parts.Add(Doc1);
  end;

begin
  Doc := TDocument.Create(dmTemporalTest.BoldSystemHandle1.System);
  InternalAdd;
  InternalAdd;
  vIndirectElem := TBoldIndirectElement.Create;
  try
    Doc.EvaluateExpression('parts.title->asSet->asCommaText', vIndirectElem);
    Check((vIndirectElem.Value as TBAString).AsString = cn_One, 'OCL: AsSetMerge');
  finally
    vIndirectElem.Free;
  end;
end;

procedure TAniv_temporal1.OCLAsSetWithSelf;
var
  Doc: TDocument;
  vIndirectElem: TBoldIndirectElement;
begin
  Doc := TDocument.Create(dmTemporalTest.BoldSystemHandle1.System);
  vIndirectElem := TBoldIndirectElement.Create;
  try
    Doc.EvaluateExpression('self->asSet', vIndirectElem);
    Check(Assigned(vIndirectElem.Value), 'AsSetWithSelf');
  finally
    vIndirectElem.Free;
  end;
end;

procedure TAniv_temporal1.OCLOperations;
var
  Doc1_1: TDocument;
  Doc1_2: TDocument;
  Doc1_3: TDocument;
  Doc2_2: TDocument;
  Doc2_3: TDocument;
begin
  EnsureSetup;

  Doc1_1 := Doc1.AtTime(1) as TDocument;
  Doc1_2 := Doc1.AtTime(2) as TDocument;
  Doc1_3 := Doc1.AtTime(3) as TDocument;
  Doc2_2 := Doc2.AtTime(2) as TDocument;
  Doc2_3 := Doc2.AtTime(3) as TDocument;

  assert((dmTemporalTest.BoldSystemHandle1.System.EvaluateExpressionAsDirectElement('Document.allInstancesAtTime(1)') as TBoldObjectList).Count = 1, 'OCL: allInstancesAtTime');
  assert((dmTemporalTest.BoldSystemHandle1.System.EvaluateExpressionAsDirectElement('Document.allInstancesAtTime(2)') as TBoldObjectList).Count = 2, 'OCL: allInstancesAtTime');
  assert((dmTemporalTest.BoldSystemHandle1.System.EvaluateExpressionAsDirectElement('Document.allInstancesAtTime(2)') as TBoldObjectList).Count = 2, 'OCL: allInstancesAtTime');
  assert((dmTemporalTest.BoldSystemHandle1.System.EvaluateExpressionAsDirectElement('Document.allInstancesAtTime(1)') as TBoldObjectList).Includes(Doc1_1), 'OCL: allInstancesAtTime');
  assert((dmTemporalTest.BoldSystemHandle1.System.EvaluateExpressionAsDirectElement('Document.allInstancesAtTime(2)') as TBoldObjectList).Includes(Doc1_2), 'OCL: allInstancesAtTime');
  assert((dmTemporalTest.BoldSystemHandle1.System.EvaluateExpressionAsDirectElement('Document.allInstancesAtTime(2)') as TBoldObjectList).Includes(Doc2_2), 'OCL: allInstancesAtTime');
  assert((dmTemporalTest.BoldSystemHandle1.System.EvaluateExpressionAsDirectElement('Document.allInstancesAtTime(3)') as TBoldObjectList).Includes(Doc1_3), 'OCL: allInstancesAtTime');
  assert((dmTemporalTest.BoldSystemHandle1.System.EvaluateExpressionAsDirectElement('Document.allInstancesAtTime(3)') as TBoldObjectList).Includes(Doc2_3), 'OCL: allInstancesAtTime');

  assert(Doc1.EvaluateExpressionAsDirectElement('atTime(1)') = Doc1_1, 'OCL: atTime operator');
  assert(Doc1.EvaluateExpressionAsDirectElement('atTime(2)') = Doc1_2, 'OCL: atTime operator');
  assert(Doc1.EvaluateExpressionAsDirectElement('atTime(3)') = Doc1_3, 'OCL: atTime operator');
  assert(Doc2.EvaluateExpressionAsDirectElement('atTime(2)') = Doc2_2, 'OCL: atTime operator');
  assert(Doc2.EvaluateExpressionAsDirectElement('atTime(3)') = Doc2_3, 'OCL: atTime operator');

  assert(Doc1_1.EvaluateExpressionAsString('boldTime', brDefault) = '1', 'OCL: boldTime operator');
  assert(Doc1_2.EvaluateExpressionAsString('boldTime', brDefault) = '2', 'OCL: boldTime operator');
  assert(Doc1_3.EvaluateExpressionAsString('boldTime', brDefault) = '3', 'OCL: boldTime operator');
  assert(Doc2_2.EvaluateExpressionAsString('boldTime', brDefault) = '2', 'OCL: boldTime operator');
  assert(Doc2_3.EvaluateExpressionAsString('boldTime', brDefault) = '3', 'OCL: boldTime operator');
end;

procedure TAniv_temporal1.ReadOnlyness;
var
  Doc1_1: TDocument;
  Doc1_2: TDocument;
  Doc1_3: TDocument;
  Doc2_2: TDocument;
  Doc2_3: TDocument;
begin
  EnsureSetup;

  Doc1_1 := Doc1.AtTime(1) as TDocument;
  Doc1_2 := Doc1.AtTime(2) as TDocument;
  Doc1_3 := Doc1.AtTime(3) as TDocument;
  Doc2_2 := Doc2.AtTime(2) as TDocument;
  Doc2_3 := Doc2.AtTime(3) as TDocument;

  assert(not Doc1_1.CanDelete, 'readonlyness of old version, CanDelete');
  assert(not Doc1_2.CanDelete, 'readonlyness of old version, CanDelete');
  assert(not Doc1_3.CanDelete, 'readonlyness of old version, CanDelete');
  assert(not Doc2_2.CanDelete, 'readonlyness of old version, CanDelete');
  assert(not Doc2_3.CanDelete, 'readonlyness of old version, CanDelete');
  assert(not Doc1_1.M_Title.CanModify, 'readonlyness of old version, CanModify member');
  assert(not Doc1_2.M_Title.CanModify, 'readonlyness of old version, CanModify member');
  assert(not Doc1_3.M_Title.CanModify, 'readonlyness of old version, CanModify member');
  assert(not Doc2_2.M_Title.CanModify, 'readonlyness of old version, CanModify member');
  assert(not Doc2_3.M_Title.CanModify, 'readonlyness of old version, CanModify member');
end;

procedure TAniv_temporal1.SetUp;
begin
  inherited;
  EnsureDM;
end;

procedure TAniv_temporal1.TearDown;
begin
  FreeDM;
  inherited;
end;

class procedure TAniv_temporal1.Suit(ASuite: TBoldTestSuite);
begin
  ASuite.AddTest(CreateWithComment('AtTimeAndState', '2.1, 2.2, 2.3, 2.5'));
  ASuite.AddTest(CreateWithComment('TimeStampOfLatestUpdate', '2.7'));
  ASuite.AddTest(CreateWithComment('BoldTime', '2.6'));
  ASuite.AddTest(CreateWithComment('ReadOnlyness', '2.3'));
  ASuite.AddTest(CreateWithComment('Navigation', '2.4'));
  ASuite.AddTest(CreateWithComment('OCLOperations', '2.5, 2.6'));
end;


procedure TAniv_temporal1.TimeStampOfLatestUpdate;
begin
  EnsureSetup;

  assert(TimeStampOfUpdate0 = -1, 'TimeStampOfLatestUpdate before update');
  assert(TimeStampOfUpdate1 = 1, 'TimeStampOfLatestUpdate after first update');
  assert(TimeStampOfUpdate2 = 2, 'TimeStampOfLatestUpdate after second update');
  assert(TimeStampOfUpdate3 = 3, 'TimeStampOfLatestUpdate after third update');
end;

class function TAniv_temporal1.Suite: ITestSuite;
begin
  Result := inherited Suite;
  SetCommentForTest(Result, 'AtTimeAndState', '2.1, 2.2, 2.3, 2.5');
  SetCommentForTest(Result, 'TimeStampOfLatestUpdate', '2.7');
  SetCommentForTest(Result, 'BoldTime', '2.6');
  SetCommentForTest(Result, 'ReadOnlyness', '2.3');
  SetCommentForTest(Result, 'Navigation', '2.4');
  SetCommentForTest(Result, 'OCLOperations', '2.5, 2.6');
end;

initialization
  TestGlobal.RegisterTestCase(TAniv_temporal1);

end.
