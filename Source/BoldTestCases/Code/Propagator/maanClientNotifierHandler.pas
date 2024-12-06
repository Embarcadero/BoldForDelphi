unit maanClientNotifierHandler;

interface

uses
  TestSuite,
  BoldClientNotifierHandler,
  maanClientNotificationComps,
  BoldOutputQueueHandler,
  BoldPriorityListEnlister,
  BoldClientHandler,
  BoldPropagatorConstants,
  BoldPropagatorInterfaces_TLB, dialogs,
  BoldThread,
  BoldObjectMarshaler,
  classes,
  TestFramework;

type


  TTestableClientNotifier = class(TBoldClientNotifier);

  TMyOutputQueueHandler = class(TBoldOutputQueueHandler)
  protected
    function getPriorityListEnlister: TBoldAbstractPriorityListEnlister; override;
  end;

  TMyPriorityListEnlister = class(TBoldPriorityListEnlister)
  protected
    function GetClientHandler: TBoldClientHandler; override;
  end;

  TTestCase_ClientNotifier = class(TBoldTestCase)
  private
    fListenerThread: TTestListenerThread;
    fClientNotifier: TTestableClientNotifier;
    fClientNotifierHandler: TBoldClientNotifierHandler;
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
    procedure SetUp; override;
    procedure TearDown; override; // cleanup after tests
  published
    procedure Test;
  end;

  TTestCase_ClientNotifierHandler = class(TBoldTestCase)
  private
    fListenerThread: TTestListenerThread2;
    fClientNotifierHandler: TBoldClientNotifierHandler;
    procedure SendEvents;
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
    procedure SetUp; override;
    procedure TearDown; override; // cleanup after tests
  published
    procedure Test;
  end;

var
  fOutputQueueHandler: TMyOutputQueueHandler;
  fPriorityListEnlister: TMyPriorityListEnlister;
  fClientHandler: TBoldClientHandler;
implementation

uses
  SysUtils,
  BoldUtils,
  BoldRev,
  forms,
  comobj,
  ActiveX,
//  SLLog,
//  SLError,
  windows;

{ TTestCase_ClientNotifier }

procedure TTestCase_ClientNotifier.SetUp;
begin
  inherited;
  CoInitFlags := COINIT_APARTMENTTHREADED;
  fOutputQueueHandler := TMyOutputQueueHandler.Create;
  fListenerThread := TTestListenerThread.Create(true);
  fListenerThread.Resume;
  fListenerThread.WaitUntilReady(INFINITE);
  fPriorityListEnlister := TMyPriorityListEnlister.Create;
  fClientHandler := TBoldClientHandler.Create(fClientHandler);
  fClientNotifierHandler := TBoldClientNotifierHandler.Create(1, fClientHandler, fPriorityListEnlister.PriorityList, true);
end;

class procedure TTestCase_ClientNotifier.Suit(ASuite: TBoldTestSuite);
begin
  inherited;
  ASuite.AddTest(CreateWithComment('Test', 'TTestCase_ClientNotifier.Test  //Maan'));
end;

class function TTestCase_ClientNotifier.Suite: ITestSuite;
begin
  result := inherited Suite;
  SetCommentForTest(result, 'Test', 'TTestCase_ClientNotifier.Test  //Maan');
end;

procedure TTestCase_ClientNotifier.TearDown;
begin
  inherited;
  FreeAndNil(fOutputQueueHandler);
  FreeAndNil(fPriorityListEnlister);
  FreeandNil(fClientHandler);
  FListenerThread.Quit(True);
  FreeAndNil(FListenerThread);
  fClientNotifierHandler.Quit(True);
  fClientNotifierHandler.Free;
end;

procedure TTestCase_ClientNotifier.Test;
var
  ClientIdString: string;
  RegistrationTime: TTimeStamp;
  listener: IBoldListener;
  Initialized: Boolean;
  Status: TBoldClientReceiveStatus;
begin
  CoGetInterfaceAndReleaseStream(IStream(fListenerThread.Stream), IID_IBoldListener, listener);
  fClientHandler.RegisterClient(1000*60*5, 1000*5, listener, 'Test Project', AClient[1], ARegTime[1]);
  fClientNotifier := TTestableClientNotifier.Create(fClientNotifierHandler);
  fClientNotifier.Resume;
  fClientNotifier.WaitUntilReady(1000*3);
  // PreCondition: Client[1] must have a ClientQueue & ClientNotifier is set to work
  fOutputQueueHandler.SendEvent(AClient[1], AEvent[1]);
  fClientHandler.HasInfoForClient(AClient[1], ClientIdString, RegistrationTime, Initialized, Status);
  Sleep(100);
  Assert(fClientNotifier.IsAvailable);
  fClientNotifier.SetClientData(TBoldClientNotifierData.CreateInitialized( AClient[1], ARegTime[1], 
                             fOutputQueueHandler.OutputQueues[AClient[1]].AsVarArray));
  fClientNotifier.DoWork;
  while not fClientNotifier.IsAvailable do
    SwitchToThread;
  Assert(fListenerThread.Listener.Events.Count = 1, Format('%s.Test: error Events.Count', [ClassName]));
  Assert(fListenerThread.Listener.Events[0] = AEvent[1], Format('%s.Test: error Events[0]', [ClassName]));
  fClientNotifier.Quit(True);
  FreeAndNil(fClientNotifier);
end;

{ TTestCase_ClientNotifierHandler }

procedure TTestCase_ClientNotifierHandler.SetUp;
begin
  inherited;
  CoInitFlags := COINIT_APARTMENTTHREADED;
  CoInitializeEx(nil, CoInitFlags);
  fListenerThread := TTestListenerThread2.Create(true, 4);
  fListenerThread.Resume;
  fListenerThread.WaitUntilReady(INFINITE);
  fClientHandler:= TBoldClientHandler.Create(fClientHandler);
  fPriorityListEnlister:= TMyPriorityListEnlister.Create;
  fOutputQueueHandler:= TMyOutputQueueHandler.Create;
  fClientNotifierHandler:= TBoldClientNotifierHandler.Create(15, fClientHandler, fPriorityListEnlister.Prioritylist, true);
end;

class procedure TTestCase_ClientNotifierHandler.Suit(ASuite: TBoldTestSuite);
begin
  inherited;
  ASuite.AddTest(CreateWithComment('Test', 'Testing TBoldClientNotifier'));
end;

procedure TTestCase_ClientNotifierHandler.TearDown;
begin
  inherited;
  FListenerThread.Quit(True);
  FreeAndNil(FListenerThread);
  fClientNotifierHandler.Quit(True);
  FreeAndNil(fClientNotifierHandler);
  FreeAndNil(fOutPutQueueHandler);
  FreeAndNil(fPriorityListEnlister);
  FreeAndNil(fClientHandler);
end;

procedure TTestCase_ClientNotifierHandler.Test;
var
  aListener1,
  aListener2,
  aListener3,
  aListener4: IBoldListener;
begin
//  SLInitLog('', True, True, True);
  fClientNotifierHandler.Resume;
  fClientNotifierHandler.WaitUntilReady(TIMEOUT);
  CoGetInterfaceAndReleaseStream(IStream(fListenerThread.Stream[0]), IID_IBoldListener, alistener1);
  fClientHandler.RegisterClient(1000*60*5, 2, aListener1, 'Test Project',AClient[1], ARegTime[1]);
  CoGetInterfaceAndReleaseStream(IStream(fListenerThread.Stream[1]), IID_IBoldListener, alistener2);
  fClientHandler.RegisterClient(1000*60*5, 100, aListener2, 'Test Project',AClient[2], ARegTime[2]);
  CoGetInterfaceAndReleaseStream(IStream(fListenerThread.Stream[2]), IID_IBoldListener, alistener3);
  fClientHandler.RegisterClient(1000*60*5, 70, aListener3, 'Test Project',AClient[3], ARegTime[3]);
  CoGetInterfaceAndReleaseStream(IStream(fListenerThread.Stream[3]), IID_IBoldListener, alistener4);
  fClientHandler.RegisterClient(1000*60*5, 6, aListener4, 'Test Project',AClient[4], ARegTime[4]);
  SendEvents;
  Sleep(1000*10);
  Application.ProcessMessages;
  Assert(TTestClientListener(FListenerThread.Listener[0]).Events.Count = 2, Format('%s.Test: Client #%d',
                [ClassName, AClient[2]]));
  Assert(TTestClientListener(FListenerThread.Listener[1]).Events.Count = 1, Format('%s.Test: Client #%d',
                [ClassName, AClient[2]]));
  Assert(TTestClientListener(FListenerThread.Listener[2]).Events.Count = 3*10{1000}, Format('%s.Test: error Client #%d',
                [ClassName, AClient[3]]));
  Assert(TTestClientListener(FListenerThread.Listener[3]).Events.Count = 10{100}, Format('%s.Test: error Client #%d',
                [ClassName, AClient[4]]));
  CoUnInitialize;
end;

procedure TTestCase_ClientNotifierHandler.SendEvents;
var
  i: integer;
begin
  //send events
  fOutputQueueHandler.SendEvent(AClient[2], AEvent[1]);
  fOutputQueueHandler.SendEvent(AClient[1], AEvent[1]);
  for i:= 1 to 10{1000} do
  begin
    fOutputQueueHandler.SendEvent(AClient[3], AEvent[1]);
    fOutputQueueHandler.SendEvent(AClient[3], AEvent[2]);
    fOutputQueueHandler.SendEvent(AClient[3], AEvent[3]);
  end;

  fOutputQueueHandler.SendEvent(AClient[1], AEvent[2]);

  for i:= 1 to 10{100} do
    fOutputQueueHandler.SendEvent(AClient[4], AEvent[2]);
end;

class function TTestCase_ClientNotifierHandler.Suite: ITestSuite;
begin
  result := inherited Suite;
  SetCommentForTest(result, 'Test', 'Testing TBoldClientNotifier');
end;

{ TMyPriorityListEnlister }

function TMyPriorityListEnlister.GetClientHandler: TBoldClientHandler;
begin
  Result := fClientHandler;
end;

{ TMyOutputQueueHandler }

function TMyOutputQueueHandler.getPriorityListEnlister: TBoldAbstractPriorityListEnlister;
begin
   Result := fPriorityListEnlister;
end;

Initialization
  TestGlobal.RegisterTestCase(TTestCase_ClientNotifier);
  TestGlobal.RegisterTestCase(TTestCase_ClientNotifierHandler);

finalization
  TestGlobal.UnRegisterTestCase(TTestCase_ClientNotifier);
  TestGlobal.UnRegisterTestCase(TTestCase_ClientNotifierHandler);

end.
