unit maan_FLS;

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
  TestSuite, BoldHandle, BoldPersistenceHandle, BoldPersistenceHandleDB,
  BoldSubscription, BoldHandles, BoldSystemHandle,
  ActnList, BoldHandleAction, BoldActions, BoldDBActions,
  maandmSnooper,
  TestFrameWork, BoldAbstractPersistenceHandleDB,
  BoldPersistenceHandleDB_deprecated, DB,
  BoldAbstractDatabaseAdapter, DBAccess, Uni, BoldDatabaseAdapterUniDAC
  ;

type
  TDataModule3 = class(TDataModule)
    BoldsystemHandle1: TBoldSystemHandle;
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
    BoldDatabaseAdapterUniDAC1: TBoldDatabaseAdapterUniDAC;
    UniConnection1: TUniConnection;
    procedure BoldsystemHandle1PreUpdate(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
    fPreUpdate: Boolean;
  public
    { Public declarations }
  end;

  Tmaan_SystemPreUpdate = class(TBoldTestCase)
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
    procedure SetUp; override;
    procedure TearDown; override;    
  published
    procedure SystemPreUpdate;
  end;
var
  DataModule3: TDataModule3;

implementation

uses
  dmModel1,
  TestModel1;
{$R *.DFM}

procedure EnsureDB;
begin
  if not DataModule3.BoldSystemHandle1.Active then
  begin
    DataModule3.BoldDatabaseAdapterUniDac1.CreateDatabase;
    DataModule3.BoldPersistenceHandleDB1.CreateDataBaseSchema;
    DataModule3.BoldSystemHandle1.Active := True;
  end;
end;

procedure EnsureDM;
begin
  Ensuredm_Model;
  if not Assigned(DataModule3) then
    Application.CreateForm(TDataModule3, DataModule3);
  EnsureDB;
end;

procedure FreeDm;
begin
  FreeAndNil(DataModule3);
end;

procedure TDataModule3.BoldsystemHandle1PreUpdate(Sender: TObject);
begin
  fPreUpdate:= True;
end;

procedure TDataModule3.DataModuleCreate(Sender: TObject);
begin
  fPreUpdate := false;
end;


{ Tmaan_SystemPreUpdate }

procedure Tmaan_SystemPreUpdate.SetUp;
begin
  EnsureDm;
end;

procedure Tmaan_SystemPreUpdate.TearDown;
begin
  FreeDm;
end;


class procedure Tmaan_SystemPreUpdate.Suit(ASuite: TBoldTestSuite);
begin
  ASuite.AddTest(CreateWithComment('SystemPreUpdate', 'SystemPreUpdate'));
end;

procedure Tmaan_SystemPreUpdate.SystemPreUpdate;
var
  Object1: TClassA;
begin
  Object1 := TDataGen.NewClassA(datamodule3.BoldSystemHandle1.System);
  Object1.aString:= 'bla bla bla' + DateTimetoStr(Now);
  DataModule3.fPreUpdate := false;
  datamodule3.BoldSystemHandle1.System.UpdateDatabase;
  Assert(DataModule3.fPreUpdate, 'SystemPreUpdate failed');
  DataModule3.fPreUpdate := false;
  Object1.aString := 'bla bla';
  datamodule3.BoldSystemHandle1.OnPreUpdate := nil;
  datamodule3.BoldSystemHandle1.System.UpdateDatabase;
  Assert(not DataModule3.fPreUpdate, 'SystemPreUpdate failed');
end;

class function Tmaan_SystemPreUpdate.Suite: ITestSuite;
begin
  Result := inherited Suite;
  SetCommentForTest(Result, 'SystemPreUpdate', 'SystemPreUpdate');
end;

initialization
  TestGlobal.RegisterTestCase(Tmaan_SystemPreUpdate);

finalization
  TestGlobal.UnRegisterTestCase(Tmaan_SystemPreUpdate);

end.
