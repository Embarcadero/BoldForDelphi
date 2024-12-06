unit maanPropagatorCleanUp;

interface

uses
  BoldPropagatorCleanUp,
  BoldClientHandler,
  BoldThreadSafeQueue,
  maanAdvancedPropagator,
  BoldDefs,
  maanClientNotificationComps,
  BoldObjectMarshaler,
  BoldPropagatorServer,
  maan_PropagatorServer,
  BoldAdvancedPropagator,
  forms,
  classes,
  TestSuite,
  SysUtils,
  TestFramework;

type
  TTestableClientHandler = class(TBoldClientHandler)
    procedure EnqueueRemoveClientQueueEvent(const ClientId: TBoldClientId); override;
  end;

  TDummyAdvancedPropagator = class(TBoldAdvancedPropagator)
  protected
    function getClientHandler: TBoldClientHandler; override;
  public
    fClientHandlerTest: TTestableClientHandler;
  end;
  
  TTestCase_PropagatorCleanUp = class(TBoldTestCase)
  private
    AClient: array[1..2] of TBoldClientID;
    ARegTime: array[1..2] of TTimeStamp;
    fListener1: TTestClientListener;
    fClientHandler: TTestableClientHandler;
    fEventQueue: TBoldThreadSafeObjectQueue;
    fCleanUpSubscriber: TBoldCleanUpSubscriber;
    fPropagator : TDummyAdvancedPropagator;
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
    procedure SetUp; override;
    procedure TearDown; override; // cleanup after tests
  published
    procedure CleanUpTest;
    procedure ExtendLeaseTest;
  end;


implementation

uses
  BoldUtils,
  BoldPropagatorInterfaces_TLB,
  windows;

{ TTestCase_PropagatorCleanUp }

procedure TTestCase_PropagatorCleanUp.SetUp;
begin
  TBoldPropagatorServer.FreeSingleton;
  TPropagatorServerTest.Instance;
  fPropagator := TDummyAdvancedPropagator.Create;
  fPropagator.Initialize;
  (TPropagatorServerTest.Instance as TPropagatorServerTest).fAdvancedPropagator := fPropagator;
  fClientHandler := TTestableClientHandler.Create(fClientHandler);
  fEventQueue := TBoldThreadSafeObjectQueue.Create('TSQ-Testcase/Cleanup');
  fPropagator.fClientHandlerTest := fClienthandler;
  fCleanUpSubscriber := TBoldCleanUpSubscriber.Create(fClientHandler);
  // initialize data
  if Assigned(fListener1) then FreeAndNil(fListener1);
  fListener1 := TTestClientListener.Create;
end;

class procedure TTestCase_PropagatorCleanUp.Suit(ASuite: TBoldTestSuite);
begin
  ASuite.AddTest(CreateWithComment('CleanUpTest', ''));
  ASuite.AddTest(CreateWithComment('ExtendLeaseTest', ''));
end;

procedure TTestCase_PropagatorCleanUp.TearDown;
begin
  FreeAndNil(fCleanUpSubscriber);
  FreeAndNil(fClientHandler);
  FreeAndNil(fEventQueue);
  FreeandNil(fPropagator);
  TBoldPropagatorServer.FreeSingleton;
end;

procedure TTestCase_PropagatorCleanUp.CleanUpTest;
var
  ClientIdString: string;
  RegistrationTime: TTimeStamp;
  Initialized: Boolean;
  Status: TBoldClientReceiveStatus;
begin
  // register the client
  fClientHandler.RegisterClient(100, 3, fListener1 as IBoldListener, 'Test Project',AClient[1], ARegTime[1]);
  //wait till lease expires
  Sleep(300);
  Application.ProcessMessages;
  fClientHandler.HasInfoForClient(AClient[1], ClientIdString, RegistrationTime, Initialized, Status);
  Assert(not Initialized, Format('no client info %d', [fClientHandler.NOConnectedClients]));
end;

procedure TTestCase_PropagatorCleanUp.ExtendLeaseTest;
var
  Extended: WordBool;
  ClientIdString: string;
  RegistrationTime: TTimeStamp;
  Initialized: Boolean;
  Status: TBoldClientReceiveStatus;
begin
  // register the client
  fClientHandler.RegisterClient(300, 3, fListener1 as IBoldListener, 'Test Project', AClient[2], ARegTime[2]);
  //wait till lease expires
  Sleep(100);
  fClientHandler.ExtendLease(AClient[2], 400, Extended);
  Sleep(200);
  Application.ProcessMessages;
  fClientHandler.HasInfoForClient(AClient[2], ClientIdString, RegistrationTime, Initialized, Status);
  Assert((Initialized), 'Failed ExtendLease: Client has been removed');
end;

class function TTestCase_PropagatorCleanUp.Suite: ITestSuite;
begin
  result := inherited Suite;
  SetCommentForTest(result, 'CleanUpTest', '');
  SetCommentForTest(result, 'ExtendLeaseTest', '');
end;

{ TTestableClientHandler }

procedure TTestableClientHandler.EnqueueRemoveClientQueueEvent(const ClientId: TBoldClientId);
begin
  // dummy
end;

{ TDummyAdvancedPropagator }

function TDummyAdvancedPropagator.getClientHandler: TBoldClientHandler;
begin
  Result := fClientHandlerTest;
end;

initialization
  TestGlobal.RegisterTestCase(TTestCase_PropagatorCleanUp);

finalization
  TestGlobal.UnRegisterTestCase(TTestCase_PropagatorCleanUp);

end.
