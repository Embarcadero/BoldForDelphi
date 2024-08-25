
{ Global compiler directives }
{$include bold.inc}
unit BoldListenerCOM;

interface

uses
  BoldPropagatorInterfaces_TLB,
  BoldDefs,
  ComObj,
  BoldThreadSafeQueue,
  Classes,
  syncobjs,
  extctrls;


type
  {forward declarations}
  TBoldListenerCOM = class;
  TBoldListenerCOMFactory = class;

  { exceptions }
  EBoldPropagatorTypeLib = class(EBold);

  { prototypes }
  TBoldExtendLeaseResult = (elrExtended, elrFailed, elrFailedExpired, elrDenied, elrNotRegistered);
  TBoldExtendLeaseFailureEvent = procedure(res: TBoldExtendLeaseResult; const Msg: string) of object;

  TBoldMessageEvent = function(const aMessage: string): Boolean of object;

  { TBoldListenerCOM }
  TBoldListenerCOM = class(TComObject, IBoldListener, IBoldListenerAdmin)
  private
    fID: TBoldClientID;
    fQueue: TBoldThreadSafeStringQueue;
    fQueueNotEmptyNotifyEvent: TNotifyEvent;
    fPropagator: IBoldClientHandler;
    fLeaseDuration: integer;
    fPollingInterval: integer;
    fAutoExtendLease: Boolean;
    fClientIdentifierString: string;
    fTimer: TTimer;
    fRegistered: Boolean;
    fOnMessage: TBoldMessageEvent;
    fLocker: TCriticalSection;
    FOnExtendLeaseFailed: TBoldExtendLeaseFailureEvent;
    fExtendLeaseAfter: integer;
    fLastSuccessFulExtend: TDateTime;
    fExtendError: string;
    fConnectAllowed: TDateTime;
    procedure PushMessage(aMessage: String);
    procedure DecodeAndPush(Incoming: OleVariant);
    procedure setAutoExtendLease(Value: Boolean);
    procedure OnTimer(Sender: TObject);
    {IBoldListener}
    function  ReceiveEvents(Events: OleVariant): integer; safecall;
    procedure SetLeaseDuration(const Value: integer);
    procedure SetExtendLeaseAfter(const Value: integer);
    function DefaultExtendLeaseInterval: cardinal;
    function LeaseTimeLeft: Integer;
    function Ping: Integer; safecall;
    procedure DisconnectClient(const aMessage: WideString; RemainDisconnected: integer); safecall;
  protected
    procedure Lock;
    procedure UnLock;
    procedure DoExtendLeaseFailed(res: TBoldExtendLeaseResult);
    procedure DoRegisterWithPropagatorFailed(const E: EOleSysError);
  public
    constructor Create(aQueue: TBoldThreadSafeStringQueue;
                       const PollingInterval, LeaseDuration, ExtendLeaseAfter: integer;
                       const AutoExtendLease: Boolean; const ClientIdentifierString: string);
    destructor Destroy; override;
    procedure DebugGetQueueContents(aList: TStringList);
    function ExtendLease: TBoldExtendLeaseResult;
    procedure RegisterWithPropagator;
    procedure UnregisterWithPropagator;
    property Queue: TBoldThreadSafeStringQueue read fQueue;
    property QueueNotEmptyNotifyEvent: TNotifyEvent read fQueueNotEmptyNotifyEvent write fQueueNotEmptyNotifyEvent;
    property LeaseDuration: integer read fLeaseDuration write setLeaseDuration;
    property ExtendLeaseAfter: integer read fExtendLeaseAfter write SetExtendLeaseAfter;
    property PollingInterval: integer read fPollingInterval write fPollingInterval;
    property AutoExtendLease: Boolean read fAutoExtendLease write setAutoExtendLease;
    property ClientIdentifierString: string read fClientIdentifierString write fClientIdentifierString;
    property Registered: Boolean read fRegistered write fRegistered;
    property ClientID: TBoldClientID read fID;
    property ExtendErrorMessage: string read fExtendError;
    property OnMessage: TBoldMessageEvent read fOnMessage write fOnMessage;
    property Propagator: IBoldClientHandler read fPropagator write fPropagator;
    property OnExtendLeaseFailed: TBoldExtendLeaseFailureEvent read FOnExtendLeaseFailed write FOnExtendLeaseFailed;
  end;

  { TBoldListenerCOMFactory }
  TBoldListenerCOMFactory = class(TComObjectFactory)
  public
    constructor Create(ComServer: TComServerObject);
  end;

implementation

uses
  SysUtils,
  Variants,
  BoldPersistenceController,
  Activex,  
  Windows;

{ TBoldListenerCOM }

constructor TBoldListenerCOM.Create( aQueue: TBoldThreadSafeStringQueue;
                        const PollingInterval, LeaseDuration, ExtendLeaseAfter: integer;
                        const AutoExtendLease: Boolean; const ClientIdentifierString: string);
begin
  inherited Create;
  fID := NOTVALIDCLIENTID;
  fQueue := aQueue;
  Assert(Assigned(fQueue), 'COMListeners queue not assigned!');
  fLocker := TCriticalSection.Create;
  self.fPollingInterval := PollingInterval;
  self.fLeaseDuration := LeaseDuration;
  Self.fExtendLeaseAfter := ExtendLeaseAfter;
  self.AutoExtendLease := AutoExtendLease;
  self.fClientIdentifierString := ClientIdentifierString;
  Registered := false;
end;

procedure TBoldListenerCOM.DebugGetQueueContents(aList: TStringList);
begin
  Queue.AppendToStringList(aList);
end;

procedure TBoldListenerCOM.DecodeAndPush(Incoming: OleVariant);
var
  i: Integer;
begin
  if VarIsArray(Incoming) then
    for i := VarArrayLowBound(Incoming, 1) to VarArrayHighBound(Incoming, 1) do
      PushMessage(Incoming[i]);
  if Assigned(QueueNotEmptyNotifyEvent) then
    QueueNotEmptyNotifyEvent(Self);
end;

destructor TBoldListenerCOM.Destroy;
begin
  fPropagator := nil;
  FreeAndNil(fTimer);
  FreeAndNil(fLocker);
  inherited;
end;

procedure TBoldListenerCOM.PushMessage(aMessage: String);
var
  Handled: Boolean;
begin
  Handled := false;
  if  assigned(fOnMessage) then
    Handled := OnMessage(aMessage);
  if not Handled then
    Queue.Enqueue(aMessage);
end;

function TBoldListenerCOM.ReceiveEvents(Events: OleVariant): integer;
begin
  try
    DecodeAndPush(Events);
    Result := S_OK;
  except
    Result := S_FALSE;
  end;
end;

procedure TBoldListenerCOM.RegisterWithPropagator;
var
  res: HResult;
begin
  if Assigned(fPropagator) then
  begin
    if not Registered then
      try
        if (fConnectAllowed = 0) or (now > fConnectAllowed) then
        begin
          Res := fPropagator.RegisterClient(LeaseDuration, PollingInterval, Self as IBoldListener, WideString(ClientIdentifierString), fID);
          Registered := (res = S_OK);
          OleCheck(res);
        end;
      except on E: EOleSysError do
        DoRegisterWithPropagatorFailed(E);
      end;
  end
  else
    UnregisterWithPropagator;
end;

procedure TBoldListenerCOM.UnregisterWithPropagator;
begin
  try
    if Registered and Assigned(fPropagator) then
      try
        fPropagator.UnregisterClient(fID);
        fPropagator._Release;
      except on E: EOleSysError do
        fPropagator._Release;
      end;
  finally
    Registered := false;
    fID := NOTVALIDCLIENTID;
  end;
end;

procedure TBoldListenerCOM.setAutoExtendLease(Value: Boolean);
begin
  if not Assigned(fTimer) then
  begin
    fTimer := TTimer.Create(nil);
    fTimer.Enabled := false;
    fTimer.OnTimer := OnTimer;
  end;
  fAutoExtendLease := Value;
  if fAutoExtendLease then
  begin
    if fTimer.Enabled then
      fTimer.Enabled := false;
    fTimer.Interval := DefaultExtendLeaseInterval;;
    fTimer.Enabled := true;
  end
  else
    fTimer.Enabled := false;
end;

procedure TBoldListenerCOM.OnTimer(Sender: TObject);
begin
  fTimer.Enabled := false;
  if Registered then
  begin
    case ExtendLease of
      elrExtended: begin
        fLastSuccessFulExtend := now;
        fTimer.Interval := DefaultExtendLeaseInterval;
      end;
      elrFailed: begin
        fTimer.Interval := round(LeaseTimeLeft * 0.2);
        if fTimer.Interval > DefaultExtendLeaseInterval div 2 then
          fTimer.Interval := DefaultExtendLeaseInterval div 2;
        if fTimer.Interval < 1000 then
          fTimer.Interval := 1000;
      end;
      elrFailedExpired: fTimer.Interval := DefaultExtendLeaseInterval;
      elrDenied: fTimer.Interval := DefaultExtendLeaseInterval;
    end;
  end;
  fTimer.Enabled := true;
end;

function TBoldListenerCOM.ExtendLease: TBoldExtendLeaseResult;
var
  Extended: WordBool;
begin
  fExtendError := '';
  try
    if Registered then
      fPropagator.ExtendLease(fID, LeaseDuration, Extended);
      if extended then
        result := elrExtended
      else
        result := elrDenied;  
  except
    on E: Exception do
    begin
      fExtendError := e.Message;
      if LeaseTimeLeft > 0 then
        result := elrFailed
      else
        result := elrFailedExpired;
    end;
  end;

  if result in [elrDenied, elrFailedExpired] then
  begin
    if Assigned(fPropagator) then
      fPropagator._Release;
    Registered := false;
    fID := NOTVALIDCLIENTID;
  end;

  if Result <> elrExtended then
    DoExtendLeaseFailed(result);
end;

procedure TBoldListenerCOM.Lock;
begin
  fLocker.Acquire;
end;

procedure TBoldListenerCOM.UnLock;
begin
  fLocker.Release;
end;

procedure TBoldListenerCOM.SetLeaseDuration(const Value: integer);
begin
  if (Value <> fLeaseDuration) then
  begin
    Lock;
    fLeaseDuration := Value;
    UnLock;
  end;
end;

procedure TBoldListenerCOM.DoExtendLeaseFailed(res: TBoldExtendLeaseResult);
begin
  if Assigned(fOnExtendLeaseFailed) then
    FOnExtendLeaseFailed(res, fExtendError);
end;

procedure TBoldListenerCOM.DoRegisterWithPropagatorFailed(
  const E: EOleSysError);
begin
  fID := NOTVALIDCLIENTID;
  Registered := false;
  if Assigned(fPropagator) then
    fPropagator._Release;
end;

procedure TBoldListenerCOM.SetExtendLeaseAfter(const Value: integer);
begin
  if (Value <> fExtendLeaseAfter) then
  begin
    Lock;
    fExtendLeaseAfter := Value;
    UnLock;
  end;
end;

function TBoldListenerCOM.DefaultExtendLeaseInterval: cardinal;
begin
  result := Round(fLeaseDuration * (fExtendLeaseAfter / 100));
end;

function TBoldListenerCOM.LeaseTimeLeft: Integer;
var
  TimePassed: integer;
  h, m, s, ms: word;
begin
  decodeTime(now - fLastSuccessFulExtend, h, m, s, ms);
  TimePassed := (((h * 60 + m) * 60 + s) * 1000) + ms;
  result := LeaseDuration-TimePassed;
end;

procedure TBoldListenerCOM.DisconnectClient(const aMessage: WideString; RemainDisconnected: integer);
var
  MSecs: Comp;
begin
  MSecs := TimeStampToMSecs(DateTimeToTimeStamp(now));
  MSecs := MSecs + RemainDisconnected;
  fConnectAllowed := TimeStampToDateTime(MSecsToTimeStamp(MSecs));
  Queue.Enqueue(format('DISCONNECT:%d:%s', [RemainDisconnected, aMessage]));
  if Assigned(QueueNotEmptyNotifyEvent) then
    QueueNotEmptyNotifyEvent(Self);
end;

function TBoldListenerCOM.Ping: Integer;
begin
  result := 1;
end;

{ TBoldListenerCOMFactory }

constructor TBoldListenerCOMFactory.Create(ComServer: TComServerObject);
begin
  inherited Create(ComServer, TBoldListenerCOM, CLASS_BoldListener, 'TBoldListenerCOM', 'BoldListenerCOM', ciInternal);
end;

end.
