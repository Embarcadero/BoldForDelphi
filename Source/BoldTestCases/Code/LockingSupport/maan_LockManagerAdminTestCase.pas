unit maan_LockManagerAdminTestCase;

interface

uses
  BoldLockManagerAdmin,
  BoldLockmanager,
  BoldAdvancedPropagator,
  BoldPropagatorServer,
  BoldDefs,
  BoldLockList,
  maanClientNotificationcomps,
  BoldPropagatorInterfaces_TLB,
  comobj,
  ActiveX,
  Sysutils,
  TestSuite,
  TestFrameWork,
  maan_PropagatorServer
  ;

type
  ClientInfoRec = record
    ClientId: TBoldClientId;
    TimeOut: integer;
    LockName: string;
    LockType: TBoldLockType;
    Listener: IBoldListener;
    RegTime: TTimeStamp;
  end;

  Tmaan_LockManagerAdminTestCase = class(TBoldTestCase)
  private
    LockManager: TBoldLockManager;
    LockManagerAdmin: TBoldLockManagerAdmin;
    Propagator: TBoldAdvancedPropagator;
    fClients: array[1..3] of ClientInfoRec;
    FListenerThread: TTestListenerThread2;
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
  windows,
  classes
  ;

{ Tmaan_LockManagerAdminTestCase }

procedure Tmaan_LockManagerAdminTestCase.SetUp;
begin
  with fClients[1] do
  begin
    LockType := bltShared;
    LockName := 'R1:1';
    TimeOut := 100;
    ClientId := 1;
  end;
  with fClients[2] do
  begin
    LockType := bltExclusive;
    LockName := 'R1:1';
    TimeOut := 10000;
    ClientId := 2;
  end;
  {init the propagator}
  CoInitFlags := COINIT_APARTMENTTHREADED;
  FListenerThread := TTestListenerThread2.Create(True, 2);
  FListenerThread.Resume;
  FListenerThread.WaitUntilReady(INFINITE);
  TPropagatorServerTest.FreeSingleton;
  (TPropagatorServerTest.Instance as TPropagatorServerTest).FAdvancedpropagator := nil;
  Propagator := TBoldAdvancedPropagator.Create;
  Propagator.Initialize;
  (TPropagatorServerTest.Instance as TPropagatorServerTest).fAdvancedPropagator := Propagator;
  LockManager := TBoldLockManager.Create(Propagator);
  LockManagerAdmin := TBoldLockManagerAdmin.Create(LockManager);
end;

class procedure Tmaan_LockManagerAdminTestCase.Suit(ASuite: TBoldTestSuite);
begin
  ASuite.AddTest(CreateWithComment('Test', 'Test LockManagerAdmin'));
end;

class function Tmaan_LockManagerAdminTestCase.Suite: ITestSuite;
begin
  Result := inherited Suite;
  SetCommentForTest(Result, 'Test', 'Test LockManagerAdmin');
end;

procedure Tmaan_LockManagerAdminTestCase.TearDown;
begin
  FListenerThread.Quit(True);
  FreeAndNil(FListenerThread);
  FreeAndNil(LockManagerAdmin);
  FreeAndNil(LockManager);
  FreeAndNil(Propagator);
  TBoldPropagatorServer.FreeSingleton;
end;

procedure Tmaan_LockManagerAdminTestCase.Test;
var
  ClientIds: TStringList;
  RequestedLocks, ClientLocks, LockDurations: TStringList;
  res: Boolean;
  aHeldLocks, aClientIds: OleVariant;
  ClientName: string;
  LockTime: TStringList;
  Tempstr: string;
begin
  ClientName := 'TMaan_LockManagerAdminTestCase';
  RequestedLocks:= TStringList.Create;
  RequestedLocks.Add('1.r1');
  RequestedLocks.Add('2.r1');
  with fClients[1] do
  begin
    CoGetInterfaceAndReleaseStream(IStream(FListenerThread.Stream[0]), IID_IBoldListener, Listener);
    Propagator.ClientHandler.RegisterClient(20000, 1000, Listener, ClientName,ClientID, RegTime);
  end;
  with fClients[2] do
  begin
    CoGetInterfaceAndReleaseStream(IStream(FListenerThread.Stream[1]), IID_IBoldListener, Listener);
    Propagator.ClientHandler.RegisterClient(20000, 1000, Listener, ClientName,ClientID, RegTime);
  end;
  LockManagerAdmin.LockManagerSuspended  := True;
  Assert(LockManager.Suspended, 'SuspendLockManager');
  LockManagerAdmin.LockManagerSuspended := false;
  Assert(not LockManager.Suspended, 'ResumeLockManager');
  clientIds := TStringList.Create;

  LockManagerAdmin.ListAllClients(ClientIds);
  Assert(ClientIds.Count = 2, 'ListAllClients');
  Assert(ClientIds.IndexOf(Format('%s=%s', [IntToStr(fClients[1].ClientId), ClientName])) <> -1, 'ListAllClients');
  Assert(ClientIds.IndexOf(Format('%s=%s', [IntToStr(fClients[2].ClientId), ClientName])) <> -1, 'ListAllClients');
  ClientIds.Clear;

  LockManagerAdmin.ListLockingClients(ClientIds);
  Assert(ClientIds.Count = 0, 'ListClients');
  res := LockManager.GetLocks(fClients[1].ClientId, 10000, RequestedLocks, nil, aHeldLocks, aClientIds);
  Assert(res, 'LockManager.GetLocks');
  LockManagerAdmin.ListLockingClients(ClientIds);
  Assert(ClientIds.Count = 1, 'ListLockingClients');
  Assert(ClientIds.IndexOf(Format('%s=%s', [IntToStr(fClients[1].ClientId), ClientName])) <> -1, 'ListLockingClients');

  // LocksForClients
  ClientIds.Clear;
  ClientIds.Add(IntToStr(fClients[1].ClientId));
  ClientIds.Add(IntToStr(fClients[2].ClientId));
  ClientLocks := TStringList.Create;
  LockDurations := TStringList.Create;
  LockTime := TStringList.Create;
  Sleep(2000);
  LockManagerAdmin.LocksForClients( ClientIds, ClientLocks, LockDurations);
  Assert(ClientLocks.Count = 2, 'LockManagerAdmin : LocksForClient');
  Assert(LockDurations.Count = 2, 'LockManagerAdmin : LocksForClient');
  Assert(ClientLocks.IndexOf(Format('%d=%s', [fClients[1].ClientId, '1.r1'])) <> -1, 'LockManagerAdmin : LocksForClient');
  Assert(ClientLocks.IndexOf(Format('%d=%s', [fClients[1].ClientId, '2.r1'])) <> -1, 'LockManagerAdmin : LocksForClient');
  Assert(LockDurations[0] = LockDurations[1], '');
  TempStr := LockDurations.Strings[0];
  LockTime.Delimiter := ':';
  LockTime.DelimitedText := Tempstr;
  Assert(StrToInt(LockTime[0]) = 0);
  Assert(StrToInt(LockTime[1]) = 0);
  Assert(StrToInt(LockTime[2]) > 0);
  Assert(StrToInt(LockTime[3]) = 0);
  LockTime.Clear;

  FreeAndNil(ClientLocks);
  FreeAndNil(LockDurations);
  FreeAndNil(ClientIds);
  FreeAndNil(RequestedLocks);
  FreeAndNil(LockTime);

  Propagator.ClientHandler.UnRegisterClient(fClients[1].ClientId, fClients[1].RegTime);
  Propagator.ClientHandler.UnRegisterClient(fClients[2].ClientId, fClients[2].RegTime);
  fClients[1].Listener := nil;
  FListenerThread.Stream[0] := nil;
  fClients[2].Listener := nil;
  FListenerThread.Stream[1] := nil;
end;

initialization
  TestGlobal.RegisterTestCase(Tmaan_LockManagerAdminTestCase);

finalization
  TestGlobal.UnRegisterTestCase(Tmaan_LockManagerAdminTestCase);

end.
