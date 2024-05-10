unit jehoMultiLinks;

interface

uses
  SysUtils,
  Classes,
  Controls,
  Forms,
  BoldElements,
  BoldSystem,
  BoldHandle,
  BoldPersistenceHandle,
  BoldPersistenceHandleDB,
  BoldAbstractModel,
  BoldModel,
  BoldHandles,
  BoldSubscription,
  BoldSystemHandle,
  ActnList,
  BoldHandleAction,
  BoldActions,
  BoldDBActions,
  TestFrameWork,
  TestSuite,
  DB,
  BoldAbstractDatabaseAdapter,
  BoldAbstractPersistenceHandleDB,
  DBAccess,
  Uni,
  BoldDatabaseAdapterUniDAC,
  System.Actions;

type
  TdmjehoMultiLinks = class(TDataModule)
    BoldSystemHandle1: TBoldSystemHandle;
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
    BoldModel1: TBoldModel;
    ActionList1: TActionList;
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
    BoldDatabaseAdapterUniDAC1: TBoldDatabaseAdapterUniDAC;
    UniConnection1: TUniConnection;
    procedure DataModuleDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TjehoBoldTest = class(TBoldTestCase)
  private
  protected
    procedure SaveAndReopen;
    procedure CreateObjects;
    procedure LoadSomeObjects;
    procedure FollowMultilink;
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure MultilinkOrdered;
  end;


var
  dmjehoMultiLinks: TdmjehoMultiLinks;

implementation

var
  // This construct is used to protect the EnsureDataModule from having to be rewritten.
  TheDataModule: TdmjehoMultiLinks absolute dmjehoMultiLinks;
  DoneSetup: boolean = false;

{$R *.DFM}

procedure EnsureDataModule;
begin
//  Ensuredm_Model;
  if not assigned(TheDataModule) then
  begin
    theDataModule := TdmjehoMultiLinks.Create(Application);
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

{ TjehoBoldTest }

procedure TjehoBoldTest.CreateObjects;
var
  SingleSide: TBoldObject;
  List: TBoldObjectList;
  i: integer;
begin
  SingleSide := TheDataModule.BoldSystemHandle1.System.ClassByExpressionName['SingleSide'].AddNew as TBoldObject;
  List := SingleSide.BoldMemberByExpressionName['toMulti'] as TBoldObjectList;
  for i := 0 to 100 do
    List.AddNew;
end;

procedure TjehoBoldTest.FollowMultilink;
var
  SingleSide: TBoldObject;
  ObjectList: TBoldObjectList;
begin
  SingleSide := TheDataModule.BoldSystemHandle1.System.ClassByExpressionName['SingleSide'][0];
  ObjectList := SingleSide.BoldMemberByExpressionName['toMulti'] as TBoldObjectList;
  ObjectList.Move(10, 20);
  (SingleSide.BoldMemberByExpressionName['toMulti'] as TBoldObjectList).EnsureObjects;
//  Assert(Assigned((SingleSide.BoldMemberByExpressionName['toMulti'] as TBoldObjectList).BoldObjects[50]));
end;

procedure TjehoBoldTest.LoadSomeObjects;
var
  o: TBoldObject;
begin
  o := TheDataModule.BoldSystemHandle1.System.ClassByExpressionName['MultiSide'].BoldObjects[20];
end;

procedure TjehoBoldTest.MultilinkOrdered;
begin
  SaveAndReopen;
  CreateObjects;
  SaveAndReopen;
  LoadSomeObjects;
  FollowMultilink;
end;

procedure TjehoBoldTest.SaveAndReopen;
begin
  if TheDataModule.BoldSystemHandle1.Active then
    TheDataModule.BoldSystemHandle1.UpdateDatabase;
  TheDataModule.BoldSystemHandle1.Active := False;
  TheDataModule.BoldSystemHandle1.Active := True;
end;

procedure TjehoBoldTest.SetUp;
begin
  inherited;
  EnsureDataModule;
//  dmjehoMultiLinks := TdmjehoMultiLinks.Create(nil);
end;

class procedure TjehoBoldTest.Suit(ASuite: TBoldTestSuite);
begin
  inherited;
  ASuite.AddTest(CreateWithComment('MultilinkOrdered', 'Ordered Multilinks'));
end;

class function TjehoBoldTest.Suite: ITestSuite;
begin
  Result := inherited Suite;
  SetCommentForTest(Result, 'MultilinkOrdered', 'Ordered Multilinks');
end;

procedure TjehoBoldTest.TearDown;
begin
  inherited;
  FreeDataModule;
end;

procedure TdmjehoMultiLinks.DataModuleDestroy(Sender: TObject);
begin
  BoldSystemHandle1.System.Discard;
end;

initialization
  TestGlobal.RegisterTestCase(TjehoBoldTest);

finalization
  TestGlobal.UnRegisterTestCase(TjehoBoldTest);

end.
