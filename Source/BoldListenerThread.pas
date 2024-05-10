
{ Global compiler directives }
{$include bold.inc}
unit BoldListenerThread;

interface

uses
  Classes,
  Windows,
  BoldListenerCOM,
  Messages,
  BoldThreadSafeQueue,
  BoldPropagatorInterfaces_TLB,
  BoldDefs
  ;

type
  {forward declarations}
  TBoldListenerThread = class;

  { TBoldListenerThread }
  TBoldListenerThread = class(TThread)
  private
    fDoneRegister: THandle;
    fDoneUnregister: THandle;
    fDoneInit: THandle;
    fDoneUnmarshalInterface: THandle;
    fDoneExtendLease: THandle;
    fExtendLeaseSucceeded: THandle;
    fBoldListenerCOM: TBoldListenerCOM;
    fQueue: TBoldThreadSafeStringQueue;
    fLeaseDuration: integer;
    fPollingInterval: integer;
    fAutoExtendLease: Boolean;
    fClientIdentifierString: string;
    fOnMessage: TBoldMessageEvent;
    FInterfaceStream: Pointer;
    FOnPropagatorFailure: TBoldNotifyEventWithErrorMessage;
    FOnExtendLeaseFailed: TBoldExtendLeaseFailureEvent;
    FErrorMessage: string;
    fExtendResult: TBoldExtendLeaseResult;
    fOnThreadError: TBoldMessageEvent;
    fExtendLeaseAfter: integer;
    function GetQueue: TBoldThreadSafeStringQueue;
    procedure setLeaseDuration(Value: integer);
    procedure setPollingInterval(Value: integer);
    procedure setAutoExtendLease(Value: Boolean);
    procedure RegisterWithPropagator;
    procedure UnRegisterWithPropagator;
    procedure SignalInitialized;
    procedure EnsureMessageQueue;
    function getBoldClientID: TBoldClientID;
    procedure setClientIdentifierString(const Value: string);
    procedure SetOnMessage(const Value: TBoldMessageEvent);
    function GetPropagator: IBoldClientHandler;
    procedure SetPropagator(const Value: IBoldClientHandler);
    procedure InternalExtendLease;
    procedure DoPropagatorFailure;
    procedure DoExtendLeaseFailed;
    function GetRegistered: Boolean;
    function GetInitialized: Boolean;
    procedure DoThreadErrorSynchronized(msg: string);
    procedure DoThreadError;
    procedure SetExtendLeaseAfter(const Value: integer);
  protected
    procedure WaitUntilInterfaceUnmarshaled;
    procedure UnMarshalInterface;
  public
    constructor Create(CreateSuspended: Boolean);
    destructor Destroy; override;
    procedure SetQueueNotEmptyNotifyEvent(Event: TNotifyEvent);
    procedure Execute; override;
    procedure DebugGetQueueContents(aList: TStringList);
    procedure Quit(Wait: Boolean);
    function WaitUntilRegistered: Boolean;
    procedure WaitUntilUnregistered;
    procedure WaitUntilInitialized;
    function ExtendLease: Boolean;
    procedure UnRegister;
    procedure DoPropagatorFailureSynchronized;
    procedure DoExtendLeaseFailedSynchronized(res: TBoldExtendLeaseResult; const Msg: string);
    property BoldClientID: TBoldClientID read getBoldClientID;
    property Registered: Boolean read GetRegistered;
    property Initialized: Boolean read GetInitialized;
    property InQueue: TBoldThreadSafeStringQueue read GetQueue;
    property LeaseDuration: integer read fLeaseDuration write setLeaseDuration;
    property ExtendLeaseAfter: integer read fExtendLeaseAfter write SetExtendLeaseAfter;
    property PollingInterval: integer read fPollingInterval write setPollingInterval;
    property AutoExtendLease: Boolean read fAutoExtendLease write setAutoExtendLease;
    property ClientIdentifierString: string read fClientIdentifierString write setClientIdentifierString;
    property OnMessage: TBoldMessageEvent read fOnMessage write SetOnMessage;
    property Propagator: IBoldClientHandler read GetPropagator write SetPropagator;
    property OnPropagatorFailure: TBoldNotifyEventWithErrorMessage read FOnPropagatorFailure write FOnPropagatorFailure;
    property OnExtendLeaseFailed: TBoldExtendLeaseFailureEvent read FOnExtendLeaseFailed write FOnExtendLeaseFailed;
    property OnThreadError: TBoldMessageEvent read fOnThreadError write fOnThreadError;
  end;

implementation

uses
  ActiveX,
  SysUtils,

  BoldCoreConsts,
  BoldPropagatorConstants;

constructor TBoldListenerThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(true);
  fDoneRegister := CreateEvent(nil, True, False, '');
  fDoneUnregister := CreateEvent(nil, True, False, '');
  fDoneInit := CreateEvent(nil, True, False, '');
  fDoneUnMarshalInterface := CreateEvent(nil, True, False, '');
  fDoneExtendLease := CreateEvent(nil, True, False, '');
  fExtendLeaseSucceeded := CreateEvent(nil, True, False, '');
  FreeOnTerminate := false;
  fLeaseDuration := DEFAULT_LEASE_DURATION;
  fPollingInterval := DEFAULT_POLLING_INTERVAL;
  fExtendLEaseAfter := DEFAULT_EXTEND_LEASE_AFTER;
  Suspended := CreateSuspended;
end;

procedure TBoldListenerThread.DebugGetQueueContents(aList: TStringList);
begin
  if Assigned(fBoldListenerCOM) then
    fBoldListenerCOM.DebugGetQueueContents(aList);
end;

destructor TBoldListenerThread.Destroy;
begin
  CloseHandle(fDoneRegister);
  CloseHandle(fDoneUnregister);
  CloseHandle(fDoneUnMarshalInterface);
  CloseHandle(fDoneInit);
  CloseHandle(fDoneExtendLease);
  CloseHandle(fExtendLeaseSucceeded);
  inherited;
end;

procedure TBoldListenerThread.Execute;
var
  rMsg: TMsg;
  Res: Integer;
  InterfaceForRefCounting: IUnknown;
begin
  CoInitialize(nil);
  EnsureMessageQueue;
  SignalInitialized;
  try
    while not Terminated do
    begin
      res := Integer(GetMessage(rMsg, 0, 0, 0));
      if ((res <> -1) and (res <> 0)) then
        if not Assigned(fBoldListenerCOM) then
        begin
          try
            fBoldListenerCOM := TBoldListenerCOM.Create(InQueue, PollingInterval,
                                                      LeaseDuration, ExtendLeaseAfter,
                                                      AutoExtendLease,
                                                      ClientIdentifierString);
            InterfaceForRefCounting := (fBoldListenerCOM as IUnknown);
            fBoldListenerCOM.OnMessage := OnMessage;
            FBoldListenerCOM.OnExtendLeaseFailed := DoExtendLeaseFailedSynchronized;
          except on E: Exception do
            begin
              fErrorMessage := Format(sInitializationLineMissing, [E.Message]);
              DoThreadErrorSynchronized(fErrorMessage);
            end;
          end;
        end;
      try
        if res = -1 then
          Terminate
        else if res = 0 then
          Terminate
        else case rMsg.message of
          BM_THRD_UNREGISTER:
            UnRegisterWithPropagator;
          BM_THRD_REGISTER:
            RegisterWithPropagator;
          BM_UNMARSHAL_INTERFACE:
            UnMarshalInterface;
          BM_EXTEND_LEASE:
            InternalExtendLease;
          else
            DispatchMessage(rMsg);
        end;
      except
        on e: Exception do
        begin
          DoThreadErrorSynchronized(e.Message);
          raise;
        end;
      end;
    end;
  finally
    if Assigned(fBoldListenerCOM) then
      InterfaceForRefCounting := nil;
    FreeAndNil(fQueue);
    CoUninitialize;
  end;
end;

function TBoldListenerThread.ExtendLease: Boolean;
begin
  Result := Initialized and not(Suspended);
  if Result then
    try
      if PostThreadMessage(ThreadID, BM_EXTEND_LEASE, 0, 0) = false then
        Raise EBold.CreateFmt('%s.ExtendLease: %s', [ClassName, SysErrorMessage(GetLastError)]);
      WaitForSingleObject(fDoneExtendLease, 3 * TIMEOUT);
      Result := (WaitForSingleObject(fExtendLeaseSucceeded, 0) = WAIT_OBJECT_0);
    finally
      ResetEvent(fDoneExtendLease);
      ResetEvent(fExtendLeaseSucceeded);
    end;
end;

function TBoldListenerThread.GetQueue: TBoldThreadSafeStringQueue;
begin
  if not Assigned(fQueue) then
    fQueue := TBoldThreadSafeStringQueue.Create('Listener InQueue');
  Result := fQueue;
end;

procedure TBoldListenerThread.RegisterWithPropagator;
begin
  if Assigned(fBoldListenerCOM) then
    fBoldListenerCOM.RegisterWithPropagator;
  SetEvent(fDoneRegister);
end;

procedure TBoldListenerThread.setAutoExtendLease(Value: Boolean);
begin
  fAutoExtendLease := Value;
  if Assigned(fBoldListenerCOM) then
    fBoldListenerCOM.AutoExtendLease := fAutoExtendLease;
end;

procedure TBoldListenerThread.setLeaseDuration(Value: integer);
begin
  fLeaseDuration := Value;
  if Assigned(fBoldListenerCOM) then
    fBoldListenerCOM.LeaseDuration := fLeaseDuration;
end;

procedure TBoldListenerThread.setPollingInterval(Value: integer);
begin
  fPollingInterval := Value;
  if Assigned(fBoldListenerCOM) then
    fBoldListenerCOM.PollingInterval := fPollingInterval;
end;

procedure TBoldListenerThread.SetQueueNotEmptyNotifyEvent(Event: TNotifyEvent);
begin
  if Assigned(fBoldListenerCOM) then
    fBoldListenerCOM.QueueNotEmptyNotifyEvent := Event;
end;

procedure TBoldListenerThread.SignalInitialized;
begin
  SetEvent(fDoneInit);
end;

procedure TBoldListenerThread.Quit(Wait: Boolean);
begin
  Assert(GetCurrentThreadId <> ThreadId);
  if not terminated then
  begin
    if Suspended then
    begin
      Resume;
      WaitUntilInitialized;
    end;
    UnRegister;
    if PostThreadMessage(ThreadID, WM_QUIT, 0, 0) = false then
       Raise EBold.CreateFmt('%s.TerminateThread: %s', [ClassName, SysErrorMessage(GetLastError)]);

    if wait then
    begin
      if not WaitForSingleObject(Handle, 3*TIMEOUT) = WAIT_OBJECT_0 then
        windows.TerminateThread(Handle, 1);
    end;
  end;
end;

procedure TBoldListenerThread.UnRegister;
begin
  Assert(GetCurrentThreadId <> Threadid);
  if Initialized and Registered and not (Suspended)then
  begin
    if PostThreadMessage(self.ThreadID, BM_THRD_UNREGISTER, 0, 0) = false then
      raise EBold.CreateFmt('%s.TerminateThread: %s', [ClassName, SysErrorMessage(GetLastError)]);
    WaitUntilUnregistered;
  end;
end;

procedure TBoldListenerThread.UnRegisterWithPropagator;
begin
  if Assigned(fBoldListenerCOM) then
    fBoldListenerCOM.UnregisterWithPropagator;
  SetEvent(fDoneUnregister);
end;

procedure TBoldListenerThread.WaitUntilInitialized;
begin
  WaitForSingleObject(fDoneInit, INFINITE);
end;

function TBoldListenerThread.WaitUntilRegistered: Boolean;
begin
  Result := (WaitForSingleObject(fDoneRegister, 5 * TIMEOUT) = WAIT_OBJECT_0) and Registered;
  ResetEvent(fDoneRegister);
end;

procedure TBoldListenerThread.WaitUntilUnregistered;
begin
  WaitForSingleObject(fDoneUnregister, 3 * TIMEOUT);
  ResetEvent(fDoneUnregister);
end;

procedure TBoldListenerThread.EnsureMessageQueue;
var
  rMsg: TMsg;
begin
  PeekMessage (rMsg, 0, 0, 0, PM_NOREMOVE);
end;

function TBoldListenerThread.getBoldClientID: TBoldClientID;
begin
  if Assigned(fBoldListenerCOM) then
    Result := fBoldListenerCOM.ClientID
  else
    Result := NOTVALIDCLIENTID;
end;

procedure TBoldListenerThread.setClientIdentifierString(const Value: string);
begin
  fClientIdentifierString := Value;
  if Assigned(fBoldListenerCOM) then
    fBoldListenerCOM.ClientIdentifierString := fClientIdentifierString;
end;

procedure TBoldListenerThread.SetOnMessage(const Value: TBoldMessageEvent);
begin
  fOnMessage := Value;
  if Assigned(fBoldListenerCOM) then
    fBoldListenerCOM.OnMessage := Value;
end;

function TBoldListenerThread.GetPropagator: IBoldClientHandler;
begin
  Result := nil;
  if Assigned(fBoldListenerCOM) then
    Result := fBoldListenerCOM.Propagator;
end;

procedure TBoldListenerThread.SetPropagator(const Value: IBoldClientHandler);
begin
  if Assigned(Value) then
  begin
    CoMarshalInterThreadInterfaceInStream(IID_IBoldClientHandler,Value,IStream(FInterfaceStream));
    if PostThreadMessage(ThreadID, BM_UNMARSHAL_INTERFACE, 0, 0) = False then
      raise EBold.CreateFmt('%s.SetPropagator: %s', [ClassName, SysErrorMessage(GetLastError)]);
    WaitUntilInterfaceUnmarshaled;
  end
  else if Assigned(fBoldListenerCOM) then
    fBoldListenerCOM.Propagator := nil;
end;

procedure TBoldListenerThread.WaitUntilInterfaceUnmarshaled;
begin
  WaitForSingleObject(fDoneUnMarshalInterface, 3*TIMEOUT);
  ResetEvent(fDoneUnMarshalInterface);
end;

procedure TBoldListenerThread.UnMarshalInterface;
var
  Obj: IBoldClientHandler;
begin
  if Assigned(FInterfaceStream) then
  begin
    CoGetInterfaceAndReleaseStream(IStream(FInterfaceStream),IID_IBoldClientHandler, Obj);
    if Assigned(fBoldListenerCOM) then
      fBoldListenerCOM.Propagator := Obj as IBoldClientHandler;
    Obj._AddRef;  {Add the RefCount since we want to keep this interface until the thread is terminated}
    FInterfaceStream := nil;
    SetEvent(fDoneUnMarshalInterface);
  end;
end;

procedure TBoldListenerThread.InternalExtendLease;
var
  res: TBoldExtendLeaseResult;
begin
  res := elrFailed;
  try
    if (Assigned(fBoldListenerCOM) and Registered) then
      res := fBoldListenerCOM.ExtendLease;
    if res <> elrExtended then
    begin
      DoExtendLeaseFailedsynchronized(res, fBoldListenerCOM.ExtendErrorMessage);
    end;
  except
    on E: Exception do
    begin
      FErrorMessage := E.Message;
      DoPropagatorFailureSynchronized;
    end;
  end;
  if res = elrExtended then
    SetEvent(fExtendLeaseSucceeded);
  SetEvent(fDoneExtendLease);
end;

procedure TBoldListenerThread.DoPropagatorFailure;
begin
  if Assigned(FOnPropagatorFailure) then
    FOnPropagatorFailure(self, FErrorMessage);
end;

procedure TBoldListenerThread.DoExtendLeaseFailed;
begin
  if fExtendResult = elrDenied then
    UnRegister;
  if Assigned(fOnExtendLeaseFailed) then
    fOnExtendLeaseFailed(fExtendResult, FErrorMessage);
end;

procedure TBoldListenerThread.DoExtendLeaseFailedSynchronized(res: TBoldExtendLeaseResult; const msg: string);
begin
  fExtendResult := res;
  fErrorMessage := msg;
  Synchronize(DoExtendLeaseFailed);
end;

procedure TBoldListenerThread.DoPropagatorFailureSynchronized;
begin
  Synchronize(DoPropagatorFailure);
end;

function TBoldListenerThread.GetRegistered: Boolean;
begin
  Result := Assigned(fBoldListenerCOM) and fBoldListenerCOM.Registered;
end;

function TBoldListenerThread.GetInitialized: Boolean;
begin
  Result := (WaitForSingleObject(fDoneInit, 0) = WAIT_OBJECT_0);
end;

procedure TBoldListenerThread.DoThreadErrorSynchronized(msg: string);
begin
  fErrorMessage := msg;
  Synchronize(DoThreadError);
end;

procedure TBoldListenerThread.DoThreadError;
begin
  if assigned(OnThreadError) then
    OnThreadError(fErrorMessage);
end;

procedure TBoldListenerThread.SetExtendLeaseAfter(const Value: integer);
begin
  fExtendLeaseAfter := Value;
  if Assigned(fBoldListenerCOM) then
    fBoldListenerCOM.ExtendLeaseAfter := fExtendLeaseAfter;
end;

end.
