unit maanBoldSnooperHandle;

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
  TestSuite, BoldHandles, BoldRootedHandles, BoldAbstractListHandle,
  BoldCursorHandle, BoldListHandle, ActnList, BoldHandleAction,
  BoldActions, BoldDBActions, BoldListenerHandle, BoldIDAdderHandle,
  BoldSystemHandle, BoldPersistenceHandlePassthrough, BoldSnooperHandle,
  BoldSubscription, BoldHandle, BoldPersistenceHandle,
  BoldPersistenceHandleDB,
  maandmSnooper,
  BoldAbstractLockManagerHandle,
  maanLockManagerDummyClasses, BoldLockManagerHandleCom,
  BoldAbstractPropagatorHandle, BoldPropagatorHandleCOM, BoldClientHandles,
  BoldComClientHandles, BoldPersistenceHandlePTWithModel,
  TestFrameWork, DB, BoldAbstractDatabaseAdapter,
  BoldAbstractPersistenceHandleDB, DBAccess, Uni, BoldDatabaseAdapterUniDAC, System.Actions
  ;

type
  TdmBoldSnooperHandle = class(TDataModule)
    BoldSnooperHandle1: TBoldSnooperHandle;
    BoldSystemHandle1: TBoldSystemHandle;
    BoldIdAdderHandle1: TBoldIdAdderHandle;
    BoldListenerHandle1: TBoldListenerHandle;
    DirectBoldHandle: TBoldSystemHandle;
    blhClassA: TBoldListHandle;
    blhThing: TBoldListHandle;
    ActionList1: TActionList;
    BoldPropagatorHandleCOM1: TBoldPropagatorHandleCOM;
    BoldComConnectionHandle1: TBoldComConnectionHandle;
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
    BoldDatabaseAdapterUniDAC1: TBoldDatabaseAdapterUniDAC;
    UniConnection1: TUniConnection;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    { Private declarations }
    fPropagatorHandle: TDummyPropagatorHandle;
    fLockManagerHandle: TDummyLockmanagerHandle;
  public
    { Public declarations }
    property PropagatorHandle: TDummypropagatorHandle read fPropagatorHandle write fPropagatorHandle;
  end;

  TmaanBoldSnooperHandle = class(TBoldTestCase)
  protected
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure EnsureDBLock;
  end;

var
  dmBoldSnooperHandle: TdmBoldSnooperHandle;

implementation

uses
  TestModel1,
  dmModel1,
  BoldSnooper,
  BoldDefs;

{$R *.DFM}

procedure EnsureDB;
begin
  if not dmBoldSnooperHandle.BoldSystemHandle1.Active then
  begin
    dmBoldSnooperHandle.BoldDatabaseAdapterUniDAC1.CreateDatabase;
    dmBoldSnooperHandle.BoldPersistenceHandleDB1.CreateDataBaseSchema;
    dmBoldSnooperHandle.BoldSystemHandle1.Active := True;
    dmBoldSnooperHandle.BoldSnooperHandle1.LockManagerHandle := dmBoldSnooperHandle.fLockManagerHandle;
    dmBoldSnooperHandle.BoldSnooperHandle1.PropagatorHandle :=  dmBoldSnooperHandle.PropagatorHandle; //(dmBoldSnooperHandle.BoldSnooperHandle1.PersistenceController as TBoldSnooper).Propagator := dmBoldSnooperHandle.Propagator;
  end;
end;

procedure EnsureDM;
begin
  Ensuredm_Model;
  if not Assigned(dmBoldSnooperHandle) then
    Application.CreateForm(TdmBoldSnooperHandle, dmBoldSnooperHandle);
  EnsureDB;
end;

procedure TdmBoldSnooperHandle.DataModuleCreate(Sender: TObject);
begin
  PropagatorHandle := TDummyPropagatorHandle.Create(self);
  fLockManagerHandle:= TDummyLockmanagerHandle.Create(self);
end;

{ TmaanBoldSnooperHandle }

procedure TmaanBoldSnooperHandle.SetUp;
begin
  inherited;
  EnsureDM;
  dmBoldSnooperHandle.BoldSnooperHandle1.LockManagerHandle := dmBoldSnooperHandle.fLockmanagerHandle;
end;

class procedure TmaanBoldSnooperHandle.Suit(ASuite: TBoldTestSuite);
begin
  ASuite.AddTest(CreateWithComment('EnsureDBLock', 'EnsureDBLock'));
end;

procedure TmaanBoldSnooperHandle.EnsureDBLock;
var
  Object1: TClassA;
begin
  dmBoldSnooperHandle.fLockManagerHandle.LockManager.InitFlags;
  dmBoldSnooperHandle.BoldSnooperHandle1.CheckDatabaseLock := True;
 (dmBoldSnooperHandle.fLockManagerHandle as TDummyLockManagerHandle).functionResult := false;
  Object1 := TDataGen.NewClassA(dmBoldSnooperHandle.BoldSystemHandle1.System);
  Object1.aString:= 'bla bla bla' + DateTimetoStr(Now);
  try
    dmBoldSnooperHandle.BoldSystemHandle1.System.UpdateDatabase;
    Assert(not (dmBoldSnooperHandle.fLockManagerHandle as TDummyLockManagerHandle).functionResult, 'EnsureDBLock failed');
    Assert(dmBoldSnooperHandle.fLockManagerHandle.LockManager.fEnsureLocks, 'EnsureDBLock failed');
  except on E: Exception do
    Assert((E is EBoldEnsureDatabaseLockError) and dmBoldSnooperHandle.fLockManagerHandle.LockManager.fEnsureLocks, 'EnsureDBLock succeeded');
  end;
  dmBoldSnooperHandle.fLockManagerHandle.LockManager.InitFlags;
  (dmBoldSnooperHandle.fLockManagerHandle as TDummyLockManagerHandle).functionResult := true;
  Object1.aString:= 'bla bla' + DateTimetoStr(Now);
  try
    dmBoldSnooperHandle.BoldSystemHandle1.System.UpdateDatabase;
    Assert(dmBoldSnooperHandle.fLockManagerHandle.LockManager.fEnsureLocks, 'EnsureDBLock succeeded');
    Assert(dmBoldSnooperHandle.fLockManagerHandle.LockManager.fReleaseLocks, 'EnsureDBLock succeeded');
  except
    Assert(False, 'EnsureDBLock failed');
  end;
end;

procedure TdmBoldSnooperHandle.DataModuleDestroy(Sender: TObject);
begin
  FreeAndNil(fLockManagerHandle);
end;

procedure TmaanBoldSnooperHandle.TearDown;
begin
  if Assigned(dmBoldSnooperHandle) then
  begin
    dmBoldSnooperHandle.BoldSystemHandle1.Active := false;
    dmBoldSnooperHandle.DirectBoldHandle.Active := false;
  end;
  FreeAndNil(dmBoldSnooperHandle);
end;

class function TmaanBoldSnooperHandle.Suite: ITestSuite;
begin
  Result := inherited Suite;
  SetCommentForTest(Result, 'EnsureDBLock', 'EnsureDBLock');
end;

initialization
  TestGlobal.RegisterTestCase(TmaanBoldSnooperHandle);

finalization
  TestGlobal.UnRegisterTestCase(TmaanBoldSnooperHandle);

end.


