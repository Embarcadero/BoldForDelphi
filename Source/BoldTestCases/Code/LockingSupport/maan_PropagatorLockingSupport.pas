unit maan_PropagatorLockingSupport;

{$INCLUDE Bold.inc}

interface

uses
  {$IFDEF BOLD_DELPHI6_OR_LATER}
  variants,
  {$ENDIF}
  TestSuite,
  BoldAdvancedPropagator,
  maanClientNotificationComps,
  ActiveX,
  TestFrameWork,
  maanClientHandler;

type
  Tmaan_PropagatorLockingSupport = class(TBoldTestCase)
  private
    fAdvancedPropagator: TBoldAdvancedPropagator;
    FListenerThread: TTestListenerThread2;
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure LockingSupportTest;
  end;

implementation

uses
  BoldPriorityListEnlister,
  BoldPropagatorInterfaces_TLB,
  maan_PropagatorServer,
  Sysutils,
  windows,
  comobj,
  dialogs;

{ Tmaan_PropagatorLockingSupport }

procedure Tmaan_PropagatorLockingSupport.LockingSupportTest;
var
  ClientID1, ClientId2: integer;
  RegTime1, RegTime2: TTimeStamp;
  Client1, Client2: IBoldListener;
  Events: OleVariant;
begin
  Events := VarArrayCreate([0, 3], varVariant);
  Events[0] := 'C:Class1';
  Events[1] := 'C:Class2';
  Events[2] := 'C:Class3';
  Events[3] := 'C:Class4';
  CoGetInterfaceAndReleaseStream(IStream(FListenerThread.Stream[0]), IID_IBoldListener, Client1);
  FAdvancedPropagator.ClientHandler.RegisterClient(20000, 5000, Client1, 'lockTestClient', ClientId1, RegTime1);
  FAdvancedPropagator.Enqueuer.AddSubscriptions(ClientId1, Events);
  CoGetInterfaceAndReleaseStream(IStream(FListenerThread.Stream[1]), IID_IBoldListener, Client2);
  FAdvancedPropagator.ClientHandler.RegisterClient(20000, 100, Client2, 'lockTestClient', ClientId2, RegTime2);
  //suspend the ClientNotifier in order to investigate the priority list
  FAdvancedPropagator.ClientNotifierHandler.Suspend;
  FAdvancedPropagator.Enqueuer.SendEvents(ClientID2, Events);
  Sleep(400);
  Assert(assigned(FAdvancedPropagator.PriorityListEnlister.PriorityList.Head), 'PriorityList.Head is not assigned');
  Assert((FAdvancedPropagator.PriorityListEnlister.PriorityList.Head as TBoldClientQueueInfo).ClientID = ClientID1);
  Assert((FAdvancedPropagator.PriorityListEnlister.PriorityList.Head as TBoldClientQueueInfo).Count = 4 );
  FAdvancedPropagator.ClientNotifierHandler.Resume;
  SwitchToThread;
  Sleep(400);
  Assert((FListenerThread.Listener[0].Events.Count = 0) or ((FListenerThread.Listener[0].Events.Count = 1) and (FListenerThread.Listener[0].Events[0] = 'L')), 'LockingSupportTest');
  FAdvancedPropagator.Enqueuer.SendLockEvent(ClientID1, 'E:GetLock', True);
  Sleep(100);
  Assert(((FListenerThread.Listener[0].Events.Count = 5) or (FListenerThread.Listener[0].Events.Count = 6)) and (FListenerThread.Listener[0].Events[FListenerThread.Listener[0].events.Count - 1] = 'E:GetLock'), 'getlock message not received');
end;

procedure Tmaan_PropagatorLockingSupport.SetUp;
begin
  CoInitFlags := COINIT_APARTMENTTHREADED;
  TPropagatorServerTest.Instance;
  FAdvancedPropagator := TBoldAdvancedPropagator.Create;
  (TPropagatorServerTest.Instance as TPropagatorServerTest).fAdvancedPropagator := FAdvancedPropagator;
  FAdvancedPropagator.Initialize;
  FListenerThread := TTestListenerThread2.Create(True, 2);
  FListenerThread.Resume;
  FListenerThread.WaitUntilReady(INFINITE);
end;

class procedure Tmaan_PropagatorLockingSupport.Suit(ASuite: TBoldTestSuite);
begin
  ASuite.AddTest(CreateWithComment('LockingSupportTest', 'Test propagator''s expansions for locking support'));
end;

class function Tmaan_PropagatorLockingSupport.Suite: ITestSuite;
begin
  Result := inherited Suite;
  SetCommentForTest(Result, 'LockingSupportTest', 'Test propagator''s expansions for locking support');
end;

procedure Tmaan_PropagatorLockingSupport.TearDown;
begin
  FreeAndNil(FAdvancedPropagator);
  FListenerThread.Quit(True);
  FreeAndNil(FListenerThread);
  TPropagatorServerTest.FreeSingleton;
end;

initialization
  TestGlobal.RegisterTestCase(Tmaan_PropagatorLockingSupport);

finalization
  TestGlobal.UnRegisterTestCase(Tmaan_PropagatorLockingSupport);


end.

