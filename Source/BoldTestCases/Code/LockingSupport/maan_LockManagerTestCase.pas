unit maan_LockManagerTestCase;

{$INCLUDE Bold.inc}

interface

uses
  {$IFDEF BOLD_DELPHI6_OR_LATER}
  variants,
  {$ENDIF}
  TestSuite,
  BoldLockList,
  BoldLockingDefs,
  BoldDefs,
  sysutils,
  BoldUtils,
  BoldIndexedList,
  BoldLockManager,
  BoldAdvancedPropagator,
  maanClientHandler,
  maanClientNotificationComps,
  BoldPropagatorInterfaces_TLB,
  maan_PropagatorServer,
  windows,
  comobj,
  ActiveX,
  TestFrameWork
  ;

type
  ClientInfoRec = record
    LockName: String;
    Locktype: TBoldLockType;
    ClientId: TBoldClientId;
    TimeOut: integer;
    Listener: IBoldListener;
    RegTime: TTimeStamp;
  end;

  Tmaan_LockManagerTestCase = class(TBoldTestCase)
  private
    fLockManager: TBoldLockManager;
    fPropagator: TBoldAdvancedPropagator;
    fClients: array[1..3] of ClientInfoRec;
    FListenerThread: TTestListenerThread2;
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure GetLocks;
//    procedure LockLost;
    procedure GetLocks2;
  end;

  Tmaan_LockListTestCase = class(TBoldTestCase)
  private
    fLockList: TBoldLockList;
    fClients: array[1..3] of ClientInfoRec;
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test;
  end;

  Tmaan_LockNameListTestCase = class(TBoldTestCase)
  private
    fList: TBoldLockNameList;
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
  dialogs,
  Classes,
  BoldListNodes,
  BoldPropagatorSubscriptions,
  BoldObjectSpaceExternalEvents,
  BoldPropagatorServer, 
  forms
  ;

{ Tmaan_LockManagerTestCase }

procedure Tmaan_LockManagerTestCase.GetLocks2;
var
  RequestedShared, RequestedExclusive: TStringList;
  ClientIds, HeldLocks: OleVariant;
  Lock1, Lock2 : string;
  Res: Boolean;
  GotLockEvent, ClientIdString: String;
begin
  clientIdString := '';
  Lock1 := '1.r1';
  Lock2 := '1.r2';
  GotLockEvent := TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsGotLocks, '', '', '', nil);
  RequestedExclusive := TStringList.Create;
  RequestedExclusive.Add(Lock1);
  RequestedShared := TStringList.Create;
  RequestedShared.Add(Lock2);
  try
    with fClients[1] do
    begin
      CoGetInterfaceAndReleaseStream(IStream(FListenerThread.Stream[0]), IID_IBoldListener, Listener);
      fPropagator.ClientHandler.RegisterClient(100000, 100, Listener, ClientIdString, ClientId, RegTime);
    end;
    with fClients[2] do
    begin
      CoGetInterfaceAndReleaseStream(IStream(FListenerThread.Stream[1]), IID_IBoldListener, Listener);
      fPropagator.ClientHandler.RegisterClient(100000, 100, Listener, ClientIdString, ClientId, RegTime);
    end;
    with fClients[3] do
    begin
      CoGetInterfaceAndReleaseStream(IStream(FListenerThread.Stream[2]), IID_IBoldListener, Listener);
      fPropagator.ClientHandler.RegisterClient(100000, 100, Listener, ClientIdString, ClientId, RegTime);
    end;

    res := fLockManager.GetLocks(fClients[1].ClientID, 500, RequestedExclusive, nil, HeldLocks, ClientIds);
    Sleep(1000);
    Assert(res);
    Assert(FListenerThread.Listener[0].Events.Count = 1, '(1) GetLocks');
    Assert(FListenerThread.Listener[0].Events[0] = GotLockEvent, 'GotLock Message');
    FListenerThread.Listener[0].Events.Clear;

    res := fLockManager.GetLocks(fClients[2].ClientID, 30000, RequestedExclusive, nil, HeldLocks, ClientIds);
    Sleep(1000);
    Assert(not res);
    Assert(VarArrayHighBound(HeldLocks, 1) = 0, 'GetLocks: incorrect list HeldLocks');
    Assert(VarArrayHighBound(ClientIds, 1) = 0, 'GetLocks: incorrect list ClientIds');
    Assert(HeldLocks[0] = Lock1, '(3) GetLocks');
    Assert(ClientIds[0] = Format('%d=%s', [fClients[1].ClientId, ClientIdString]), '(3) GetLocks');

    res := fLockManager.GetLocks(fClients[1].ClientID, 10000, nil, RequestedShared, HeldLocks, ClientIds);
    Assert(res);

    res := fLockManager.GetLocks(fClients[3].ClientID, 10000, RequestedShared, RequestedExclusive, HeldLocks, ClientIds);
    Assert(not res);
    Assert(VarArrayHighBound(HeldLocks, 1) = 1, '(4) GetLocks: incorrect HeldLocks');
    Assert(VarArrayHighBound(ClientIds, 1) = 0, '(4) GetLocks: incorrect ClientIds');
    Assert(HeldLocks[0] = Lock2, '(4) GetLocks: HeldLocks');
    Assert(HeldLocks[1] = Lock1, '(4) GetLocks: HeldLocks');
    Assert(ClientIds[0] = Format('%d=%s', [fClients[1].ClientId, ClientIdString]), 'GetLocks: ClientIds');

  finally
    fPropagator.ClientHandler.UnRegisterClient(fClients[1].ClientId, fClients[1].RegTime);
    fPropagator.ClientHandler.UnRegisterClient(fClients[2].ClientId, fClients[2].RegTime);
    fPropagator.ClientHandler.UnRegisterClient(fClients[3].ClientId, fClients[3].RegTime);
    FListenerThread.Stream[0] := nil;
    fClients[1].Listener := nil;
    FListenerThread.Stream[1] := nil;
    fClients[2].Listener := nil;
    FListenerThread.Stream[2] := nil;
    fClients[3].Listener := nil;
    FreeAndNil(RequestedExclusive);
    FreeAndNil(RequestedShared);
  end;
end;

{procedure Tmaan_LockManagerTestCase.LockLost;
var
  RequestedLocks: TStringList;
  HeldLocks, ClientIds: OleVariant;
  GotlockEvent, LockLostEvent: String;
begin
  GotLockEvent := TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsGotLocks, '', '', '',nil);
  RequestedLocks := TStringList.Create;
  RequestedLocks.Add('1.r1');
  with fClients[1] do
  begin
    CoGetInterfaceAndReleaseStream(IStream(FListenerThread.Stream[0]), IID_IBoldListener, Listener);
    fPropagator.ClientHandler.RegisterClient(100000, 100, Listener, '', ClientId);
  end;
  with fClients[2] do
  begin
    CoGetInterfaceAndReleaseStream(IStream(FListenerThread.Stream[1]), IID_IBoldListener, Listener);
    fPropagator.ClientHandler.RegisterClient(100000, 100, Listener, '', ClientId);
  end;
  fLockManager.GetLocks(fClients[1].ClientID, 500, RequestedLocks, nil, HeldLocks, ClientIds);
  Sleep(1000);
  Assert(FListenerThread.Listener[0].Events.Count = 1, 'GetLocks failed');
  Assert(FListenerThread.Listener[0].Events[0] = GotLockEvent, 'GetLocks failed');
  FListenerThread.Listener[0].Events.Clear;
  fLockManager.GetLocks(fClients[2].ClientID, 500, RequestedLocks, nil, HeldLocks, ClientIds);
  Sleep(1000);
  Assert(FListenerThread.Listener[0].Events.Count = 1, 'Lock Lost failed');
  LockLostEvent := TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsLockLost, '', '', '1.r1', nil);
  Assert(FListenerThread.Listener[0].Events.indexOf(LockLostEvent) <> -1, 'Failed to send LockLost event to timed out client (1)');
end;
}
procedure Tmaan_LockManagerTestCase.SetUp;
begin
  with fClients[1] do
  begin
    TimeOut := 100;
    ClientId := 1;
  end;
  with fClients[2] do
  begin
    TimeOut := 10000;
    ClientId := 2;
  end;
  with fClients[3] do
  begin
    TimeOut := 10000;
    ClientId := 3;
  end;
  {init the propagator}
  CoInitFlags := COINIT_APARTMENTTHREADED;
  FListenerThread := TTestListenerThread2.Create(True, 3);
  FListenerThread.Resume;
  FListenerThread.WaitUntilReady(INFINITE);
  TPropagatorServerTest.FreeSingleton;
  (TPropagatorServerTest.Instance as TPropagatorServerTest).FAdvancedpropagator := nil;
  fPropagator := TBoldAdvancedPropagator.Create;
  fPropagator.Initialize;
  (TPropagatorServerTest.Instance as TPropagatorServerTest).fAdvancedPropagator := fPropagator;
  fLockManager := TBoldLockManager.Create(fPropagator);
end;

class procedure Tmaan_LockManagerTestCase.Suit(ASuite: TBoldTestSuite);
begin
  ASuite.AddTest(CreateWithComment('GetLocks', 'LockManager Getlocks, ReleaseLocks'));
//  ASuite.AddTest(Create('LockLost', 'LockManager send LockLost'));
  ASuite.AddTest(CreateWithComment('GetLocks2', 'ListClientsHoldingRequestedLocks'));
end;

procedure Tmaan_LockManagerTestCase.TearDown;
begin
  FListenerThread.Quit(True);
  FreeAndNil(FListenerThread);
  FreeAndNil(fLockManager);
  FreeAndNil(fPropagator);
  TPropagatorServerTest.FreeSingleton;
end;

procedure Tmaan_LockManagerTestCase.GetLocks;
var
  RequestedExclusiveLocks, RequestedSharedLocks, locks, unacquiredLocks: TStringList;
  HeldLocks, clientIds: OleVariant;
  Res: Boolean;
  testMsg, Lock1, Lock2: string;
  gotLockEvent, ClientIdString: String;
  CurrentTime, OldTime: TTimeStamp;
begin
  ClientIdString := '';
  GotLockEvent := TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsGotLocks, '', '', '', nil);
  RequestedExclusiveLocks := TStringList.Create;
  RequestedSharedLocks := TStringList.Create;
  locks := TStringList.Create;
  UnAcquiredLocks := TStringList.Create;
  UnAcquiredLocks.Add('9.r9');
  Lock1 := '1.r1';
  Lock2 := '2.r2';
  try
    {1}
    testMsg := '(1) Unregistered Client';
    RequestedExclusiveLocks.Add(Lock1);
    RequestedSharedLocks.Add(Lock2);
    with fClients[1] do
      Res := fLockManager.GetLocks(clientId, TimeOut, RequestedExclusiveLocks, RequestedSharedLocks, HeldLocks, ClientIds);
    Assert (not Res, testMsg);
    Assert(HeldLocks = NULL, 'GetLocks: incorrect list HeldLocks');
    Assert(ClientIds = NULL, 'GetLocks: incorrect list ClientIds');

    {2}
    testMsg := '(2) GetLocks: client registered, locks should be acquired';
    with fClients[1] do
    begin
      CoGetInterfaceAndReleaseStream(IStream(FListenerThread.Stream[0]), IID_IBoldListener, Listener);
      fPropagator.ClientHandler.RegisterClient(100000, 100, Listener, ClientIdString, ClientId, RegTime);
    end;
    CurrentTime := DateTimeToTimeStamp(Now);
    with fClients[1] do
      Res := fLockManager.GetLocks(clientId, TimeOut, RequestedExclusiveLocks, RequestedSharedLocks, HeldLocks, ClientIds);
    Sleep(500);
    Application.ProcessMessages;
    Assert(Res, testMsg);
    Assert(FListenerThread.Listener[0].Events.Count = 1, testMsg);
    Assert(fLockManager.HandedLocks.Locks[Lock1].NoSharedLocks = 0, testMsg);
    Assert(fLockManager.HandedLocks.Locks[Lock1].ExclusiveLock = true, testMsg);
    Assert(fLockManager.HandedLocks.Locks[Lock2].NoSharedLocks = 1, testMsg);
    Assert(fLockManager.HandedLocks.Locks[Lock2].ExclusiveLock = false, testMsg);

    OldTime := CurrentTime;
    CurrentTime := DateTimeToTimeStamp(Now);
    with fClients[1] do
    begin
      Assert(fLockManager.HandedLocks.Items[ClientId, Lock1].TimeOut = TimeOut, 'Incorrect timeout');
      Assert(TimeStampToMSecs(fLockManager.HandedLocks.Items[ClientId, Lock1].LockAcquisitionTime) = TimeStampToMSecs(OldTime), 'Incorrect LockAcquisitionTime');
      Assert(fLockManager.HandedLocks.Items[ClientId, Lock1].GetLockDuration(CurrentTime) = (TimeStampToMSecs(CurrentTime) - TimeStampToMSecs(OldTime)), 'Incorrect LockDuration');
      Assert(fLockManager.HandedLocks.Items[ClientId, Lock2].TimeOut = TimeOut, 'Incorrect timeout');
      Assert(TimeStampToMSecs(fLockManager.HandedLocks.Items[ClientId, Lock2].LockAcquisitionTime) = TimeStampToMSecs(OldTime), 'Incorrect LockAcquisitionTime');
      Assert(fLockManager.HandedLocks.Items[ClientId, Lock2].GetLockDuration(CurrentTime) = (TimeStampToMSecs(CurrentTime) - TimeStampToMSecs(OldTime)), 'Incorrect LockDuration');
    end;

    {3}
    testMsg := '(3) GetLocks: more than one client acquire shared locks';
    with fClients[2] do
    begin
      CoGetInterfaceAndReleaseStream(IStream(FListenerThread.Stream[1]), IID_IBoldListener, Listener);
      fPropagator.ClientHandler.RegisterClient(100000, 100, Listener, ClientIdString, ClientId, RegTime);
      Res := fLockManager.GetLocks(ClientId, 10000, nil, RequestedSharedLocks, HeldLocks, ClientIds);
    end;
    Sleep(500);
    Application.ProcessMessages;
    Assert(Res, testMsg);
    Assert(FListenerThread.Listener[1].Events.Count = 1, testMsg);
    Assert(FListenerThread.Listener[1].Events[0] = GotLockEvent, testMsg);
    Assert(fLockManager.HandedLocks.Locks[Lock1].NoSharedLocks = 0, testMsg);
    Assert(fLockManager.HandedLocks.Locks[Lock1].ExclusiveLock = true, testMsg);
    Assert(fLockManager.HandedLocks.Locks[Lock2].NoSharedLocks = 2, testMsg);
    Assert(fLockManager.HandedLocks.Locks[Lock2].ExclusiveLock = false, testMsg);

    {4}
    testMsg := '(4) GetLocks: Client 3 acquires shared locks';
    with fClients[3] do
    begin
      CoGetInterfaceAndReleaseStream(IStream(FListenerThread.Stream[2]), IID_IBoldListener, Listener);
      fPropagator.ClientHandler.RegisterClient(10000, 100, Listener, ClientidString, ClientId, RegTime);
      Res := fLockManager.GetLocks(ClientId, 10000, nil, RequestedSharedLocks, HeldLocks, ClientIds);
    end;
    Sleep(500);
    Application.ProcessMessages;
    Assert(Res, testMsg);
    Assert(fLockManager.HandedLocks.Locks[Lock1].NoSharedLocks = 0, testMsg);
    Assert(fLockManager.HandedLocks.Locks[Lock1].ExclusiveLock = true, testMsg);
    Assert(fLockManager.HandedLocks.Locks[Lock2].NoSharedLocks = 3, testMsg);
    Assert(fLockManager.HandedLocks.Locks[Lock2].ExclusiveLock = false, testMsg);

    {5}
    testMsg := '(5) GetLocks: Client 1 requesting shared locks as exclusive';
    with fClients[1] do
    begin
      FListenerThread.Listener[0].Events.Clear;
      Res := fLockManager.GetLocks(clientId, TimeOut, RequestedSharedLocks, nil, HeldLocks, ClientIds);
    end;
    Sleep(2000);
    Application.ProcessMessages;
    Assert(not Res, testMsg);
    Assert(fLockManager.HandedLocks.Locks[Lock1].NoSharedLocks = 0, testMsg);
    Assert(fLockManager.HandedLocks.Locks[Lock1].ExclusiveLock = true, testMsg);
    Assert(fLockManager.HandedLocks.Locks[Lock2].NoSharedLocks = 3, testMsg);
    Assert(fLockManager.HandedLocks.Locks[Lock2].ExclusiveLock = false, testMsg);
    Assert(VarArrayHighBound(HeldLocks, 1) = 0, '(5) GetLocks: incorrect list HeldLocks');
    Assert(VarArrayHighBound(ClientIds, 1) = 1, '(5) GetLocks: incorrect list ClientIds');
    Assert(HeldLocks[0] = Lock2, '(5) GetLocks: HeldLocks incorrect');
    Assert(ClientIds[0] = Format('%d=%s', [fClients[2].ClientId, ClientIdString]), '(5) GetLocks: ClientIds incorrect');
    Assert(ClientIds[1] = Format('%d=%s', [fClients[3].ClientId, ClientIdString]), '(5) GetLocks: ClientIds incorrect');

    {6}
    TestMsg := '(6) ReleaseLocks: Client 1 releases unacquired Locks';
    with fClients[1] do
      fLockManager.ReleaseLocks(ClientId, UnAcquiredLocks);
    Assert(fLockManager.HandedLocks.Locks[Lock1].NoSharedLocks = 0, testMsg);
    Assert(fLockManager.HandedLocks.Locks[Lock1].ExclusiveLock = true, testMsg);
    Assert(fLockManager.HandedLocks.Locks[Lock2].NoSharedLocks = 3, testMsg);
    Assert(fLockManager.HandedLocks.Locks[Lock2].ExclusiveLock = false, testMsg);

    TestMsg := '(6) ReleaseLocks: Client 2 tries releasing Locks acquired by client 1';
    with fClients[2] do
      fLockManager.ReleaseLocks(ClientId, RequestedExclusiveLocks);
    Assert(fLockManager.HandedLocks.Locks[Lock1].NoSharedLocks = 0, testMsg);
    Assert(fLockManager.HandedLocks.Locks[Lock1].ExclusiveLock = true, testMsg);
    Assert(fLockManager.HandedLocks.Locks[Lock2].NoSharedLocks = 3, testMsg);
    Assert(fLockManager.HandedLocks.Locks[Lock2].ExclusiveLock = false, testMsg);

    TestMsg := '(6) ReleaseLocks: Client 1 releases exclusive Locks';
    with fClients[1] do
      fLockManager.ReleaseLocks(clientId, RequestedExclusiveLocks);
    Assert(fLockManager.HandedLocks.Locks[Lock1].NoSharedLocks = 0, testMsg);
    Assert(fLockManager.HandedLocks.Locks[Lock1].ExclusiveLock = false, testMsg);
    Assert(fLockManager.HandedLocks.Locks[Lock2].NoSharedLocks = 3, testMsg);
    Assert(fLockManager.HandedLocks.Locks[Lock2].ExclusiveLock = false, testMsg);

    {7}
    testMsg := '(7) GetLocks: Client 2 requesting exclusive locks';
    with fClients[2] do
    begin
      FListenerThread.Listener[0].Events.Clear;
      FListenerThread.Listener[1].Events.Clear;
      Res := fLockManager.GetLocks(ClientId, Timeout, RequestedExclusiveLocks, nil, HeldLocks, ClientIds);
    end;
    Sleep(2000);
    Application.ProcessMessages;
    Assert(Res, testMsg);
    Assert(fLockManager.HandedLocks.Locks[Lock1].NoSharedLocks = 0, testMsg);
    Assert(fLockManager.HandedLocks.Locks[Lock1].ExclusiveLock = true, testMsg);
    Assert(fLockManager.HandedLocks.Locks[Lock2].NoSharedLocks = 3, testMsg);
    Assert(fLockManager.HandedLocks.Locks[Lock2].ExclusiveLock = false, testMsg);

    {8}
    testMsg := '(8) ReleaseLocks';
    with fClients[2] do
    begin
      fLockManager.ReleaseAllLocksForClient(ClientID);
      Assert(fLockManager.HandedLocks.Locks[Lock1].NoSharedLocks = 0, testMsg);
      Assert(fLockManager.HandedLocks.Locks[Lock1].ExclusiveLock = false, testMsg);
      Assert(fLockManager.HandedLocks.Locks[Lock2].NoSharedLocks = 2, testMsg);
      Assert(fLockManager.HandedLocks.Locks[Lock2].ExclusiveLock = false, testMsg);
    end;

    {9}
    testMsg := '(9) UnRegisterClient';
    locks.Clear;
    with fClients[3] do
    begin
      fLockManager.HandedLocks.GetLocksByClientId(ClientId, locks);
      Assert(locks.Count > 0, testMsg);
      locks.Clear;
      fPropagator.ClientHandler.UnRegisterClient(ClientId, RegTime);
      Sleep(10);
      fLockManager.HandedLocks.GetLocksByClientId(clientId, locks);
      Assert(locks.Count = 0, testMsg);
    end;

    {10} //calling ensure locks for non acquired shared locks
    res := fLockManager.GetLocks(fclients[2].ClientId, 10000, RequestedExclusiveLocks, RequestedSharedLocks, HeldLocks, ClientIds);
    Assert(res, 'GetLocks failed');
    RequestedSharedLocks.Add('R1:3');
    res := fLockManager.EnsureLocks(fClients[2].ClientId, RequestedExclusiveLocks, RequestedSharedLocks);
    Assert(not res, '(10) EnsureLocks failed');
    Assert(not fLockManager.HandedLocks.Items[fclients[2].clientId, RequestedExclusiveLocks[0]].CanTimeOut);
    Assert(not fLockManager.HandedLocks.Items[fclients[2].clientId, RequestedSharedLocks[0]].CanTimeOut);

    {11} //calling ensure locks for nonacquired exclusive locks
    RequestedSharedLocks.Delete(RequestedSharedLocks.Count - 1);
    RequestedExclusiveLocks.Add('R1.3');
    res := fLockManager.EnsureLocks(fClients[2].ClientId, RequestedExclusiveLocks, RequestedSharedLocks);
    Assert(not res, '(11) EnsureLocks failed');
    Assert(not fLockManager.HandedLocks.Items[fclients[2].clientId, RequestedExclusiveLocks[0]].CanTimeOut);
    Assert(not fLockManager.HandedLocks.Items[fclients[2].clientId, RequestedSharedLocks[0]].CanTimeOut);

    {12} //calling ensure locks for acquired locks
    RequestedExclusiveLocks.Delete(RequestedExclusiveLocks.Count -1);
    res := fLockManager.EnsureLocks(fClients[2].ClientId, RequestedExclusiveLocks, RequestedSharedLocks);    Assert(res, '(12) EnsureLocks failed');
    Assert(not fLockManager.HandedLocks.Items[fclients[2].clientId, RequestedExclusiveLocks[0]].CanTimeOut, 'EnsureLocks failed');
    Assert(not fLockManager.HandedLocks.Items[fclients[2].clientId, RequestedSharedLocks[0]].CanTimeOut, 'EnsureLocks failed');

  finally
    fPropagator.ClientHandler.UnRegisterClient(fClients[1].ClientId, fClients[1].RegTime);
    fPropagator.ClientHandler.UnRegisterClient(fClients[2].ClientId, fClients[2].RegTime);
    fPropagator.ClientHandler.UnRegisterClient(fClients[3].ClientId, fClients[3].RegTime);
    FListenerThread.Stream[0] := nil;
    fClients[1].Listener := nil;
    FListenerThread.Stream[1] := nil;
    fClients[2].Listener := nil;
    FListenerThread.Stream[2] := nil;
    fClients[3].Listener := nil;
    FreeAndNil(RequestedExclusiveLocks);
    FreeAndNil(Locks);
    FreeAndNil(UnAcquiredLocks);
    FreeAndNil(RequestedSharedLocks);
    FreeAndNil(fLockManager);
  end;
end;

class function Tmaan_LockManagerTestCase.Suite: ITestSuite;
begin
  Result := inherited Suite;
  SetCommentForTest(Result, 'GetLocks', 'LockManager Getlocks, ReleaseLocks');
  SetCommentForTest(Result, 'GetLocks2', 'ListClientsHoldingRequestedLocks');
end;

{ Tmaan_LockListTestCase }

procedure Tmaan_LockListTestCase.SetUp;
begin
  fLockList := TBoldLockList.Create;
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
    TimeOut := 100;
    ClientId := 2;
  end;
  with fClients[3] do
  begin
    LockType := bltShared;
    LockName := 'R2:1';
    TimeOut := 100;
    ClientId := 1;
  end;
end;

class procedure Tmaan_LockListTestCase.Suit(ASuite: TBoldTestSuite);
begin
  ASuite.AddTest(CreateWithComment('Test', 'Test lock list'));
end;

class function Tmaan_LockListTestCase.Suite: ITestSuite;
begin
  Result := inherited Suite;
  SetCommentForTest(Result, 'Test', 'Test lock list');
end;

procedure Tmaan_LockListTestCase.TearDown;
begin
  FreeAndNil(fLockList);
end;

procedure Tmaan_LockListTestCase.Test;
var
  ClientIds: TList;
  Locks: TStringList;
  CurrentTime, OldTime: TTimeStamp;
begin
  CurrentTime := DateTimeToTimeStamp(Now);
  with fClients[1] do
    fLockList.AddLock(clientId, TimeOut, CurrentTime, LockName, LockType);
  with fClients[2] do
    fLockList.AddLock(clientId, TimeOut, CurrentTime, LockName, LockType);
  ClientIds := TList.create;
  Locks := TStringList.Create;
  Sleep(1000);
  try
    {1}
    fLockList.GetClientIDsbyLock(fClients[1].LockName, ClientIds);
    Assert(ClientIds.Count = 2);
    Assert(Integer(ClientIds[0]) = fClients[2].ClientId, '');
    Assert(Integer(ClientIds[1]) = fClients[1].ClientId, '');
    {2}
    with fClients[3] do
      fLockList.AddLock(ClientId, TimeOut, CurrentTime, LockName, LockType);
    fLockList.GetLocksByClientId(fClients[1].ClientId, Locks);
    Assert(Locks.Count = 2, '');
    Assert(Locks[0] = fClients[3].LockName, '');
    Assert(Locks[1] = fClients[1].LockName, '');
    OldTime := CurrentTime;
    CurrentTime := DateTimeToTimeStamp(Now);
    with fClients[3] do
    begin
      Assert(fLockList.Items[ClientId, LockName].TimeOut = TimeOut, 'Incorrect timeout');
      Assert(TimeStampToMSecs(fLockList.Items[ClientId, LockName].LockAcquisitionTime) = TimeStampToMSecs(OldTime), 'Incorrect LockAcquisitionTime');
      Assert(fLockList.Items[ClientId, LockName].GetLockDuration(CurrentTime) = (TimeStampToMSecs(CurrentTime) - TimeStampToMSecs(OldTime)), 'Incorrect LockDuration');
    end;
    {3}
    with fClients[1] do
    begin
      Assert(fLockList.Items[ClientId, LockName].LockType = bltShared, '');
      Assert(fLockList.Items[ClientId, LockName].LockType = bltShared, '');
      LockType := bltExclusive;
      fLockList.AddLock(clientId, TimeOut, CurrentTime, LockName, LockType);
      Assert(fLockList.Items[ClientId, LockName].LockType = bltExclusive, '');
    end;
  finally
    FreeAndNil(ClientIds);
    FreeAndNil(Locks);
  end;
end;

{ Tmaan_LockNameListTestCase }

procedure Tmaan_LockNameListTestCase.SetUp;
begin
  fList := TBoldLockNameList.Create(1);
end;

class procedure Tmaan_LockNameListTestCase.Suit(ASuite: TBoldTestSuite);
begin
  ASuite.AddTest(CreateWithComment('Test', 'Test LockNameList'));
end;

class function Tmaan_LockNameListTestCase.Suite: ITestSuite;
begin
  Result := inherited Suite;
  SetCommentForTest(Result, 'Test', 'Test LockNameList');
end;

procedure Tmaan_LockNameListTestCase.TearDown;
begin
  FreeAndNil(fList);
end;

procedure Tmaan_LockNameListTestCase.Test;
var
  Item: TBoldLockNode;
begin
  Item := TBoldLockNode.Create;
  Item.LockType := bltShared;
  Item.ClientId := 1;
  fList.InsertNode('AA', item);
end;

initialization
  TestGlobal.RegisterTestCase(Tmaan_LockManagerTestCase);
  TestGlobal.RegisterTestCase(Tmaan_LockListTestCase);
  TestGlobal.RegisterTestCase(Tmaan_LockNameListTestCase);

finalization
  TestGlobal.UnRegisterTestCase(Tmaan_LockManagerTestCase);
  TestGlobal.UnRegisterTestCase(Tmaan_LockListTestCase);
  TestGlobal.RegisterTestCase(Tmaan_LockNameListTestCase);

end.
