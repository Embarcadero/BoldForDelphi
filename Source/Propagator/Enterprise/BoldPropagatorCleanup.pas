unit BoldPropagatorCleanup;

interface

uses
  Windows,
  SysUtils,
  BoldSubscription,
  BoldClientHandler,
  BoldThread,
  BoldDefs,
  BoldPropagatorConstants,
  BoldThreadSafeLog,
  ExtCtrls;

type
  {forward declarrations}
  TBoldCleanUpSubscriber = class;
  TBoldCleanUpThread = class;

  {The CleanUpThread does not only do cleanup, it also clears a client's queue
  (when the client unregisters or is disocnnected) by pushing a bemRemoveClientQueue event into the InBoundQueue}
  TBoldCleanUpThread = class(TBoldNotifiableThread)
  private
    fTimer: TTimer;
    fOwner: TBoldCleanUpSubscriber;
    function getTimer: TTimer;
    procedure OnTimer(Sender: TObject);
    procedure SetTimer;
    procedure DisableTimer;
    function GetOwner: TBoldCleanUpSubscriber;
  protected
    TimingOutClient: TBoldClientInfoRecord;
    function ProcessMessage(var Msg: TMsg): Boolean; override;
    class procedure CreateQueueWindow(var ServerWindow: HWnd); override;
    property Owner: TBoldCleanUpSubscriber read GetOwner ;
  public
    constructor Create(CreateSuspended: Boolean; Owner: TBoldCleanUpSubscriber);
    procedure Execute; override;
    procedure RemoveTimedOutClient;
    property Timer: TTimer read getTimer write fTimer;
  end;

  TBoldCleanupSubscriber = class(TBoldPassthroughSubscriber)
  private
    fCleanUpThread: TBoldCleanUpThread;
    fClientHandler: TBoldClientHandler;
    procedure DoLeaseChanged;
    procedure SetClientHandler(const Value: TBoldClientHandler);
  protected
    property ClientHandler: TBoldClientHandler read fClientHandler write SetClientHandler;
  public
    constructor Create(const aClientHandler: TBoldClienthandler);
    destructor Destroy; override;
    procedure OnGetExtendedEvent(Originator: TObject; OriginalEvent: TBoldEvent;
                RequestedEvent: TBoldRequestedEvent; const Args: array of const);
  end;

implementation

uses
  BoldUtils,
  BoldEnqueuer,
  BoldPropagatorMainForm,
  BoldPropagatorServer,
  Messages,
  Classes,
  PropagatorConsts;

function BoldPropagatorCleanupWndProc(Window: HWND;
  Message, wParam, lParam: Longint): Longint; stdcall;
var
  CleanUpThread: TBoldCleanUpThread;
begin
  CleanUpThread := TBoldCleanUpThread(lParam);
  case Message of
    BM_PROPAGATOR_CLIENT_LEASE_CHANGED:
    begin
      Result := 0;
      try
        CleanUpThread.SetTimer;
      except on E: Exception do
        BoldLogError(sWindowProcError, [E.Message]);
      end;
    end;
  else
    Result := DefWindowProc(Window, Message, wParam, lParam);
  end;
end;

var
  cBoldComEventQueueWindowClass: TWndClass = (
    style: 0;
    lpfnWndProc: @BoldPropagatorCleanupWndProc;
    cbClsExtra: 0;
    cbWndExtra: 0;
    hInstance: 0;
    hIcon: 0;
    hCursor: 0;
    hbrBackground: 0;
    lpszMenuName: nil;
    lpszClassName: 'TBoldPropagatorCleanupWindow'); // do not localize

{ TBoldCleanupSubscriber }

constructor TBoldCleanupSubscriber.Create(const aClientHandler: TBoldClienthandler);
begin
  inherited CreateWithExtendedReceive(OnGetExtendedEvent);
  fCleanUpThread := TBoldCleanUpThread.Create(true, self);
  fCleanUpThread.Resume;
  fCleanUpThread.WaitUntilReady(TIMEOUT);
  ClientHandler := aClienthandler;
end;

destructor TBoldCleanupSubscriber.Destroy;
begin
  CancelAllSubscriptions;
  fCleanUpThread.Quit(True);
  FreeAndNil(fCleanUpThread);
  inherited;
end;

procedure TBoldCleanupSubscriber.DoLeaseChanged;
begin
  try
    PostMessage(fCleanUpThread.QueueWindow, BM_PROPAGATOR_CLIENT_LEASE_CHANGED, 0, Integer(fCleanUpThread));
  except on e: Exception do
      BoldLogError(sLogError, [ClassName, 'DoLeaseChanged', e.Message]); // do not localize
  end;
end;

procedure TBoldCleanupSubscriber.OnGetExtendedEvent(
  Originator: TObject; OriginalEvent: TBoldEvent;
  RequestedEvent: TBoldRequestedEvent; const Args: array of const);
begin
 if (RequestedEvent = BOLD_PROPAGATOR_CLIENT_LEASE_CHANGED) then
  begin
    try
      with fCleanUpThread.TimingOutClient do
      begin
        ClientId := Args[0].VInteger;
        RegistrationTime := MSecsToTimeStamp(Args[1].VExtended^);
        LeaseTimeOut := MSecsToTimeStamp(Args[2].VExtended^);
      end;
      DoLeaseChanged;
    except
      BoldLogError('%s.OnGetExtendedEvent: Bold_propagator_client_lease_changed', [ClassName]); // do not localize
    end;
  end;
end;

procedure TBoldCleanupSubscriber.SetClientHandler(
  const Value: TBoldClientHandler);
begin
  if (fClienthandler <> Value) then
  begin
    fClientHandler := Value;
    fClientHandler.AddSubscription(self, BOLD_PROPAGATOR_CLIENT_LEASE_CHANGED, BOLD_PROPAGATOR_CLIENT_LEASE_CHANGED);
  end;
end;

{ TBoldCleanupThread }

constructor TBoldCleanUpThread.Create(CreateSuspended: Boolean; Owner: TBoldCleanupSubscriber);
begin
  inherited Create(CreateSuspended);
  fOwner := Owner;
end;

class procedure TBoldCleanUpThread.CreateQueueWindow(var ServerWindow: HWnd);
var
  TempClass: TWndClass;
  ClassRegistered: Boolean;
begin
  cBoldComEventQueueWindowClass.hInstance := HInstance;
  ClassRegistered := GetClassInfo(HInstance,
    cBoldComEventQueueWindowClass.lpszClassName,TempClass);
  if not ClassRegistered or (TempClass.lpfnWndProc <> @BoldPropagatorCleanupWndProc) then
  begin
    if ClassRegistered then
      Windows.UnregisterClass(cBoldComEventQueueWindowClass.lpszClassName, HInstance);
    Windows.RegisterClass(cBoldComEventQueueWindowClass);
  end;
  ServerWindow := CreateWindow(cBoldComEventQueueWindowClass.lpszClassName, '', 0,
    0, 0, 0, 0, 0, 0, HInstance, nil);
end;

procedure TBoldCleanUpThread.DisableTimer;
begin
  Timer.Enabled := false;
end;

procedure TBoldCleanUpThread.Execute;
var
  res: integer;
  rMsg: TMsg;
begin
  EnsureMessageQueue;
  InitServerWindow (TRUE);
  SignalReady;
  BoldLogThread('ID=Cleanup'); // do not localize

  while not Terminated do
  begin
    res := Integer(GetMessage(rMsg, 0, 0, 0));
    if res = -1 then
      Terminate
    else if rMsg.Message = WM_QUIT then
      Terminate
    else
      ProcessMessage(rMsg);
  end;
  FreeAndNil(fTimer);
end;

function TBoldCleanUpThread.GetOwner: TBoldCleanUpSubscriber;
begin
  Result := fOwner;
end;

function TBoldCleanUpThread.getTimer: TTimer;
begin
  if not Assigned(fTimer) then
  begin
    fTimer := TTimer.Create(nil);
    fTimer.Enabled := false;
    fTimer.OnTimer := OnTimer;
  end;
  Result := fTimer;
end;

procedure TBoldCleanUpThread.OnTimer(Sender: TObject);
begin
  Assert(GetCurrentThreadId = threadID);
  DisableTimer;
  RemoveTimedOutClient;
end;

function TBoldCleanUpThread.ProcessMessage(var Msg: TMsg): Boolean;
begin
  DispatchMessage(Msg);
  Result := True;
end;

procedure TBoldCleanUpThread.RemoveTimedOutClient;
begin
  fOwner.ClientHandler.RemoveExpiredLease(TimingOutClient.ClientId, TimingOutClient.RegistrationTime);
end;

procedure TBoldCleanUpThread.SetTimer;
var
  Interval: Integer;
  CurrentTime: real;
begin
  try
    if Timer.Enabled then
      DisableTimer;
    CurrentTime := TimeStampToMSecs(DateTimeToTimeStamp(Now));
    Interval := Trunc(TimeStampToMSecs(TimingOutClient.LeaseTimeOut) - CurrentTime + 1);
    if Interval > 0 then
    begin
      Timer.Interval := Interval;
      Timer.Enabled := true;
    end
    else
      RemoveTimedOutClient;
  except on E: Exception do
    BoldlogError('%s.SetTimer: %s', [ClassName, E.Message]); //do not localize
  end;
end;

end.
