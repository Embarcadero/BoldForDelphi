
unit joho_persistence;

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
  BoldHandleAction,
  BoldActions,
  BoldUMLModelLink,
  BoldUMLRose98Link,
  BoldHandle,
  BoldSystem,
  BoldPersistenceHandle,
  BoldPersistenceHandleDB,
  BoldModel,
  BoldHandles,
  BoldSubscription,
  BoldSystemHandle,
  TestSuite,
  BoldDBActions,
  BoldAbstractModel,
  TestFramework,
  DB,
  BoldAbstractDatabaseAdapter,
  BoldAbstractPersistenceHandleDB,
  DBAccess,
  Uni,
  BoldDatabaseAdapterUniDAC;

type
  TdmjohoPersistence = class(TDataModule)
    BoldSystemHandle1: TBoldSystemHandle;
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
  end;

  TJoho_EmptyObjects = class(TLocalTestCase)
  public
    class procedure Suit(aSuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
  published
    procedure EmptyObjectsInDB;
  end;


var
  dmjoho_persistence: TdmjohoPersistence = nil;

implementation

var
  // This construct is used to protect the EnsureDataModule from having to be rewritten.
  TheDataModule: TdmjohoPersistence absolute dmjoho_persistence;
  DoneSetup: boolean = false;

{$R *.DFM}

procedure EnsureDataModule;
begin
  // Uncomment EnsureDM_Model if the model in MAIN is used.
  //Ensuredm_Model;

  if not Assigned(TheDataModule) then
  begin
    TheDataModule := TdmjohoPersistence.Create(Application);
    TheDataModule.BoldDatabaseAdapterUniDAC1.CreateDatabase;
    TheDataModule.BoldPersistenceHandleDB1.CreateDataBaseSchema;
    TheDataModule.BoldSystemHandle1.Active := True;
  end;
end;

procedure FreeDM;
begin
  TheDataModule.BoldSystemHandle1.System.Discard;
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

{ TYourTestCaseHere }

class procedure TJoho_EmptyObjects.Suit(aSuite: TBoldTestSuite);
begin
  inherited;
  ASuite.AddTest(CreateWithComment('EmptyObjectsInDB' , ''));
end;

procedure TJoho_EmptyObjects.EmptyObjectsInDb;
var
  ARootObj: TBoldObject;
begin

  EnsureDatamodule;

  while dmjoho_persistence.BoldSystemHandle1.System.ClassByExpressionName['BusinessClassesRoot'].Count > 0 do
    dmjoho_persistence.BoldSystemHandle1.System.ClassByExpressionName['BusinessClassesRoot'][0].Delete;

  ARootObj := dmjoho_persistence.BoldSystemHandle1.System.CreateNewObjectByExpressionName('BusinessClassesRoot');
  // make sure it can be saved
  dmjoho_persistence.BoldSystemHandle1.UpdateDatabase;
  aRootObj := nil;
  dmjoho_persistence.BoldSystemHandle1.Active := false;
  dmjoho_persistence.BoldSystemHandle1.Active := true;
  // make sure it can be loaded again
  aRootObj := dmjoho_persistence.BoldSystemHandle1.System.ClassByExpressionName['BusinessClassesRoot'][0];
  assert(AnsiCompareText(aRootObj.BoldClassTypeInfo.ExpressionName, 'BusinessClassesRoot')=0);
end;


procedure TLocalTestCase.TearDown;
begin
  inherited;
  FreeDM;
end;

class function TJoho_EmptyObjects.Suite: ITestSuite;
begin
  Result := inherited Suite;
  SetCommentForTest(Result, 'EmptyObjectsInDB' , '');
end;

initialization
  TestGlobal.RegisterTestCase(TJoho_EmptyObjects);


end.
