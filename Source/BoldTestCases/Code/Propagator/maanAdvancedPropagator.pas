unit maanAdvancedPropagator;

{$INCLUDE Bold.inc}

interface

uses
  {$IFDEF BOLD_DELPHI6_OR_LATER}
  variants,
  {$ENDIF}
  TestSuite,
  BoldDefs,
  classes,
  BoldPropagatorInterfaces_TLB,
  maanClientNotificationComps,
  TestFramework;

type

  TTestCase_AdvancedPropagator = class(TBoldTestCase)
  private
    AClient: array[0..4] of TBoldClientID;
    AEvent: array[0..4] of string;
    aListener0, aListener1, aListener2: IBoldListener;
    varEvents: variant;
    FListenerThread: TTestListenerThread2;
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
    procedure SetUp; override;
    procedure TearDown; override; // cleanup after tests
  published
    procedure Test;
  end;

implementation

uses
  ActiveX,
  windows,
  SysUtils,
  forms,
  dialogs,
  BoldUtils,
  BoldComUtils,
  maan_PropagatorConnection,
  comobj;

{ TTestCase_AdvancedPropagator }

procedure TTestCase_AdvancedPropagator.SetUp;
begin
  EnsureDm;
  dmPropConnection.BoldComConnectionHandle1.Connected := true;
  CoInitFlags := COINIT_APARTMENTTHREADED;
  CoInitializeEx(nil, CoInitFlags);
  FListenerThread := TTestListenerThread2.Create(True, 5);
  FListenerThread.Resume;
  FListenerThread.WaitUntilReady(INFINITE);
  AEvent[0] := 'E:2';
  AEvent[1] := 'I:partof:3';
  AEvent[2] := 'C:ClassA';
  AEvent[3] := 'E:4';
  AEvent[4] := 'I:partpartof:5';
  //register clients
  CoGetInterfaceAndReleaseStream(IStream(FListenerThread.Stream[0]), IID_IBoldListener, aListener0);
  dmPropConnection.BoldPropagatorHandleCOM1.ClientHandler.RegisterClient(100, 3, aListener0, 'Test Project', AClient[0]);
  CoGetInterfaceAndReleaseStream(IStream(FListenerThread.Stream[1]), IID_IBoldListener, aListener1);
  dmPropConnection.BoldPropagatorHandleCOM1.ClientHandler.RegisterClient(100, 6, aListener1, 'Test Project', AClient[1]);
end;

class procedure TTestCase_AdvancedPropagator.Suit(ASuite: TBoldTestSuite);
begin
  ASuite.AddTest(CreateWithComment('Test', 'Make a COM call to TBoldAdvancedPropagator'));
end;

class function TTestCase_AdvancedPropagator.Suite: ITestSuite;
begin
  result := inherited Suite;
  SetCommentForTest(result, 'Test', 'Make a COM call to TBoldAdvancedPropagator');
end;

procedure TTestCase_AdvancedPropagator.TearDown;
begin
  if Assigned(dmPropConnection.BoldPropagatorHandleCOM1.ClientHandler) then
  begin
    dmPropConnection.BoldPropagatorHandleCOM1.ClientHandler.UnRegisterClient(AClient[0]);
    dmPropConnection.BoldPropagatorHandleCOM1.ClientHandler.UnRegisterClient(AClient[1]);
  end;
  dmPropConnection.BoldComConnectionHandle1.Connected := false;
  FreeAndNil(dmPropConnection);
  aListener0 := nil;
  aListener1 := nil;
  aListener2 := nil;
  FListenerThread.Quit(True);
  FreeAndNil(FListenerThread);
  CoUnInitialize;
end;

procedure TTestCase_AdvancedPropagator.Test;
begin
  varEvents := VarArrayCreate([0,1],varOleStr);
  varEvents[0] := AEvent[0];
  varEvents[1] := AEvent[1];
  dmPropConnection.BoldPropagatorHandleCOM1.EventPropagator.AddSubscriptions(AClient[0], varEvents);
  dmPropConnection.BoldPropagatorHandleCOM1.EventPropagator.SendEvents(AClient[1], varEvents);
  Sleep(5000);
  Application.ProcessMessages;
  Assert(Assigned(FListenerThread.Listener[0].Events), 'Events were not sent by propagator');
  Assert(FListenerThread.Listener[0].Events.Count = 2, Format('%s.Test: Client #%d',
                [ClassName, AClient[0]]));
  //since client[0] is expired, registering a new client will give it the same ID as Client[0]
  Sleep(1000);
  Application.ProcessMessages;
  CoGetInterfaceAndReleaseStream(IStream(FListenerThread.Stream[2]), IID_IBoldListener, aListener2);
end;


initialization
  TestGlobal.RegisterTestCase(TTestCase_AdvancedPropagator);

finalization
  TestGlobal.UnRegisterTestCase(TTestCase_AdvancedPropagator);

end.
