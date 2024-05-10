unit maan_LockManagerCOMTestCase;

interface

uses
  TestSuite,
  BoldLockingSupportInterfaces_TLB,
  BoldLockManagerCOM,
  BoldComClientHandles,
  BoldLockManagerHandleCom,
  comobj,
  TestFramework
  ;

type
  Tmaan_LockManagerCOMTestCase = class(TBoldTestCase)
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test;
  end;

implementation

uses
  Classes,
  SysUtils,
  BoldUtils,
  BoldPropagatorGUIDs,
  BoldLockingDefs,
  BoldComUtils,
  LockManagerHandlesDataMod,
  ActiveX
  ;

{ Tmaan_LockManagerCOMTestCase }

procedure Tmaan_LockManagerCOMTestCase.SetUp;
begin
  dmLockManagerHandle := TdmLockManagerHandle.Create(nil);
end;

class procedure Tmaan_LockManagerCOMTestCase.Suit(ASuite: TBoldTestSuite);
begin
  ASuite.AddTest(CreateWithComment('Test', 'Get LockManager via TBoldLockManagerHandleCOM'));
end;

class function Tmaan_LockManagerCOMTestCase.Suite: ITestSuite;
begin
  Result := inherited Suite;
  SetCommentForTest(Result, 'Test', 'Get LockManager via TBoldLockManagerHandleCOM');
end;

procedure Tmaan_LockManagerCOMTestCase.TearDown;
begin
  FreeAndNil(dmLockManagerHandle);
end;

procedure Tmaan_LockManagerCOMTestCase.Test;
var
  Locks: TStringList;
  fLockManager: IBoldLockManager;
begin
  Locks := TStringList.Create;
  try
    fLockManager := dmLockManagerHandle.LockManagerHandleCom.LockManager;
    Assert(Assigned(fLockManager));
    fLockManager.ReleaseLocks(1, BoldStringsToVariant(Locks));
    dmLockManagerHandle.BoldComConnectionHandle1.Connected := false;
    Assert(dmLockManagerHandle.LockManagerHandleCom.LockManager = nil);
    dmLockManagerHandle.BoldComConnectionHandle1.Connected := true;
    fLockManager := dmLockManagerHandle.LockManagerHandleCom.LockManager;
    Assert(Assigned(fLockManager), 'Cannot connect to a propagator server');
    if Assigned(fLockManager) then
      fLockManager.ReleaseLocks(1, BoldStringsToVariant(Locks));
    dmLockManagerHandle.LockManagerHandleCom.Active := false;
    Assert(dmLockManagerHandle.LockManagerHandleCom.LockManager = nil);
    dmLockManagerHandle.LockManagerHandleCom.Active := true;
  finally
    dmLockManagerHandle.BoldComConnectionHandle1.Connected := false;
    FreeAndNil(Locks);
  end;
end;

initialization
  TestGlobal.RegisterTestCase(Tmaan_LockManagerCOMTestCase);

finalization
  TestGlobal.UnRegisterTestCase(Tmaan_LockManagerCOMTestCase);

end.
