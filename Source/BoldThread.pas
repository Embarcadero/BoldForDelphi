
{ Global compiler directives }
{$include bold.inc}
unit BoldThread;

interface

uses
  Classes,
  Windows,
  Syncobjs;

type
  { forward declarations }
  TBoldNotifiableThread = class;

  { TBoldNotifiableThread }
  TBoldNotifiableThread = class(TThread)
  private
    fReadyEvent: TSimpleEvent;
    FQuitWaitTimeOut: integer;
    FQueueWindow: HWnd;
  protected
    procedure SignalReady;
    procedure EnsureMessageQueue;
    function ProcessMessage(var Msg: TMsg): Boolean; virtual;
    class procedure CreateQueueWindow(var ServerWindow: HWnd); virtual;
    procedure InitServerWindow (bInit: boolean);
    procedure DoTerminate; override;
  public
    constructor Create(CreateSuspended: Boolean);
    destructor Destroy; override;
    procedure Execute; override;
    function WaitUntilReady(dwMilliseconds: Cardinal): Boolean;
    function WaitUntilSignaled(dwMilliseconds: Cardinal): LongWord;
    procedure Notify(const Msg: Cardinal);
    function Quit(Wait: Boolean): Boolean; virtual;
    function WaitForQuit: Boolean;
    property QuitWaitTimeOut: integer read FQuitWaitTimeOut write FQuitWaitTimeOut;
    property QueueWindow: HWnd read FQueueWindow;
  end;

  function WaitForObject(iHandle: THandle; iTimeOut: dWord): TWaitResult;

implementation

uses
  SysUtils,
  Messages,

  BoldCoreConsts,
  BoldPropagatorConstants, 
  BoldThreadSafeLog;

function WaitForObject (iHandle : THandle; iTimeOut : dword) : TWaitResult;
begin
  Assert (iHandle <> 0);
  case WaitForSingleObject (iHandle, iTimeout) of
    WAIT_OBJECT_0 :
      Result := wrSignaled;
    WAIT_TIMEOUT :
      Result := wrTimeout;
    WAIT_ABANDONED :
      Result := wrAbandoned;
    else
      Result := wrError;
  end;
end;

{ TBoldNotifiableThread }

constructor TBoldNotifiableThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(True);
  fReadyEvent := TSimpleEvent.Create;
  QuitWaitTimeOut := TIMEOUT * 5;
  ReturnValue := -10;
  Suspended := CreateSuspended;
end;

procedure TBoldNotifiableThread.SignalReady;
begin
  fReadyEvent.SetEvent;
end;

procedure TBoldNotifiableThread.EnsureMessageQueue;
var
  rMsg:TMsg;
begin
  PeekMessage(rMsg, 0, 0, 0, PM_NOREMOVE);
end;

{$Assertions On}

procedure TBoldNotifiableThread.Notify(const Msg: Cardinal);
begin
  Assert(PostThreadMessage(ThreadID, Msg, 0, 0), SysErrorMessage(GetLastError));
end;

function TBoldNotifiableThread.WaitUntilReady(dwMilliseconds: Cardinal): Boolean;
begin
  Result := (fReadyEvent.WaitFor(dwMilliseconds) = wrSignaled);
end;

function TBoldNotifiableThread.WaitUntilSignaled(dwMilliseconds: Cardinal): LongWord;
begin
  Result := WaitForSingleObject(Handle, dwMilliseconds);
end;

function TBoldNotifiableThread.Quit(Wait: Boolean): Boolean;
var
  TimeOut: cardinal;
begin
  InitServerWindow(FALSE);
  Result := false;
  TimeOut := 0;
  if Suspended then
  begin
    Resume;
    WaitUntilReady(TIMEOUT);
    SwitchToThread;
  end;
  if not (Terminated) then
  begin
    PostThreadMessage (ThreadId, WM_QUIT, 0, 0);
    if (Wait) then
      Result := WaitForQuit
    else
      Result := (WaitForObject(Handle, Timeout*2) = wrSignaled);
  end; 
end;

procedure TBoldNotifiableThread.Execute;
var
  rMsg:TMsg;
  res: integer;
begin
  EnsureMessageQueue;
  SignalReady;
  while not Terminated do
  begin
    res :=  Integer(GetMessage(rMsg, 0, 0, 0));
    if res = -1 then
      Terminate
    else if res = 0 then
      Terminate
    else
      ProcessMessage(rMsg);
  end;
end;

function TBoldNotifiableThread.ProcessMessage(var Msg: TMsg):Boolean;
begin
  Result := false;
end;

procedure TBoldNotifiableThread.DoTerminate;
begin
  FreeAndNil(fReadyEvent);
end;

function TBoldNotifiableThread.WaitForQuit: Boolean;
var
  wr : TWaitResult;
begin
  Result := false;
  try
    Assert(ThreadId <> GetCurrentThreadId,
      'Message queue thread cannot be terminated from within its own thread!!!'
    );
    wr := WaitForObject(Handle, timeout*5);
    Result := (wr = wrSignaled);
    if (wr <> wrSignaled) then
    begin
      TerminateThread (Handle, 1);
      BoldLogError(sThreadWasForcedTerminated, [ClassName]);
    end;
  except on E:Exception do
    BoldLogError(sErrorWaitForQuit, [ClassName, E.Message]);
  end;
end;

class procedure TBoldNotifiableThread.CreateQueueWindow(
  var ServerWindow: HWnd);
begin
end;

procedure TBoldNotifiableThread.InitServerWindow(bInit: boolean);
begin
  if (bInit) then
  begin
    if (FQueueWindow = 0) then
      CreateQueueWindow(FQueueWindow);
  end
  else
    if (FQueueWindow <> 0) then
    begin
      SendMessage (QueueWindow, WM_QUIT, 0, 0);
      FQueueWindow := 0;
    end;
end;

destructor TBoldNotifiableThread.Destroy;
begin
  FreeAndNil(fReadyEvent);
  inherited;
end;

end.
