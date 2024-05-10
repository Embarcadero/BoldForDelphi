
{ Global compiler directives }
{$include bold.inc}
unit BoldListenerHandle;

interface

uses
  Classes,
  BoldHandle,
  BoldListenerThread,
  BoldSubscription,
  BoldAbstractDequeuer,
  BoldListenerCOM,
  BoldDefs,
  BoldPropagatorConstants,
  BoldAbstractPropagatorHandle
  ;

type
  {forward declarations}
  TBoldListenerHandle= class;

  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldListenerHandle = class(TBoldHandle)
  private
    fActive: Boolean;
    fBoldListenerThread: TBoldListenerThread;
    fDequeuer: TBoldStringDequeuer;
    fPTSubscriber: TBoldPassThroughSubscriber;
    fAutoStart: Boolean;
    fLeaseDuration: integer;
    fPollingInterval: integer;
    fAutoExtendLease: Boolean;
    FOnRegistrationFailed: TNotifyEvent;
    FOnExtendLeaseFailed: TBoldExtendLeaseFailureEvent;
    FClientIdentifierString: string;
    FPropagatorHandleSubscriber: TBoldPassThroughSubscriber;
    fPropagatorHandle: TBoldAbstractPropagatorHandle;
    fOnThreadError: TBoldMessageEvent;
    fExtendLeaseAfter: integer;
    procedure _Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);    
    function GetListenerThread: TBoldListenerThread;
    function GetConnected: Boolean;
    procedure SetDequeuer(aDequeuer: TBoldStringDequeuer);
    procedure NotifyDequeuer(Sender: TObject);
    procedure setLeaseDuration(Value: integer);
    procedure setPollingInterval(Value: integer);
    procedure setAutoExtendLease(Value: Boolean);
    procedure setClientIdentifierString(const Value: string);
    procedure _ReceivePropagatorHandleEvents(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    procedure SetPropagatorHandle(Value: TBoldAbstractPropagatorHandle);
    procedure SetOnExtendLeaseFailed(const Value: TBoldExtendLeaseFailureEvent);
    function GetIsLoaded: Boolean;
    procedure StopAndFreeListenerThread;
    procedure SetExtendLeaseAfter(const Value: integer);
    procedure Subscribe(const DoSubscribe: Boolean);    
  protected
    function GetHandledObject: TObject; override;
    function GetBoldClientID: TBoldClientID;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;    
    property IsLoaded: Boolean read GetIsLoaded;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure StartListenerThread;
    procedure SuspendListenerThread;
    procedure DebugGetQueueContents(aList: TStringList);
    function ExtendLease: Boolean;
    procedure SetActive(Value: Boolean);
    property BoldClientID: TBoldClientID read GetBoldClientID;
    property Connected: Boolean read GetConnected;
    property ListenerThread: TBoldListenerThread read GetListenerThread;
  published
    property AutoStart: Boolean read fAutoStart write fAutostart;
    property LeaseDuration: integer read fLeaseDuration write setLeaseDuration;
    property ExtendLeaseAfter: integer read fExtendLeaseAfter write SetExtendLeaseAfter;
    property PollingInterval: integer read fPollingInterval write setPollingInterval;
    property AutoExtendLease: Boolean read fAutoExtendLease write setAutoExtendLease;
    property Dequeuer: TBoldStringDequeuer read fDequeuer write SetDequeuer;
    property ClientIdentifierString: string read fClientIdentifierString write setClientIdentifierString;
    property PropagatorHandle: TBoldAbstractPropagatorHandle read fPropagatorHandle write SetPropagatorHandle;
    property OnRegistrationFailed: TNotifyEvent read FOnRegistrationFailed write FOnRegistrationFailed;
    property OnExtendLeaseFailed: TBoldExtendLeaseFailureEvent read FOnExtendLeaseFailed write SetOnExtendLeaseFailed;
    property OnThreadError: TBoldMessageEvent read fOnThreadError write fOnThreadError;
  end;

implementation

uses
  Messages,
  SysUtils,
  Windows,

  BoldCoreConsts,
  BoldUtils,
  BoldEnvironment,
  BoldThreadSafeQueue;

{ TBoldListenerHandle }

constructor TBoldListenerHandle.Create(Owner: TComponent);
begin
  inherited;
  FClientIdentifierString := '';
  fPTSubscriber := TBoldPassthroughSubscriber.Create(_Receive);  
  fPropagatorHandleSubscriber := TBoldPassThroughSubscriber.Create(_ReceivePropagatorHandleEvents);
  fAutoStart := True;
  fLeaseDuration := DEFAULT_LEASE_DURATION;
  fExtendLeaseAfter := DEFAULT_EXTEND_LEASE_AFTER;
  fPollingInterval := DEFAULT_POLLING_INTERVAL;
  fActive := false;
end;

procedure TBoldListenerHandle.DebugGetQueueContents(aList: TStringList);
begin
  fBoldListenerThread.DebugGetQueueContents(aList);
end;

procedure TBoldListenerHandle._Receive(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  if (Originator = fDequeuer) and (RequestedEvent = beDestroying) then
    Dequeuer := nil;
  if (Originator = fPropagatorHandle) and (RequestedEvent = beDestroying) then
  begin
    fPropagatorHandle := nil;
    if Assigned(fBoldListenerThread) then
      fBoldListenerThread.OnPropagatorFailure := nil;
  end;
end;

destructor TBoldListenerHandle.Destroy;
begin
  StopAndFreeListenerThread;
  FreeAndNil(fPTSubscriber);  
  FreeAndNil(fPropagatorHandleSubscriber);
  inherited;
end;

function TBoldListenerHandle.GetBoldClientID: TBoldClientID;
begin
  Result := ListenerThread.BoldClientID;
end;

function TBoldListenerHandle.GetConnected: Boolean;
begin
  Result := ListenerThread.Registered;
end;

procedure TBoldListenerHandle.SetDequeuer(aDequeuer: TBoldStringDequeuer);
begin
  if aDequeuer <> fDequeuer then
  begin
    fDequeuer := aDequeuer;
    if Assigned(fDequeuer) then
      fDequeuer.Queue := ListenerThread.InQueue;
  end;
end;

function TBoldListenerHandle.GetHandledObject: TObject;
begin
  Result := ListenerThread;
end;

function TBoldListenerHandle.GetListenerThread: TBoldListenerThread;
begin
  if not Assigned(fBoldListenerThread) then
  begin
    fBoldListenerThread := TBoldListenerThread.Create(true);
    fBoldListenerThread.LeaseDuration := LeaseDuration;
    fBoldListenerThread.PollingInterval := PollingInterval;
    fBoldListenerThread.AutoExtendLease := AutoExtendLease;
    fBoldListenerThread.ClientIdentifierString := fClientIdentifierString;
    fBoldListenerThread.OnExtendLeaseFailed := FOnExtendLeaseFailed;
  end;
  Result := fBoldListenerThread;
end;

procedure TBoldListenerHandle.StopAndFreeListenerThread;
begin
  if Assigned(fBoldListenerThread) then
    ListenerThread.Quit(true);
  FreeAndNil(fBoldListenerThread);
end;

procedure TBoldListenerHandle.StartListenerThread;
begin
  if not Assigned(fPropagatorHandle) then
    raise EBold.CreateFmt(sPropagatorHandleNotAssigned, [ClassName]);
  if not ListenerThread.Registered then
  begin
    ListenerThread.Resume;
    ListenerThread.WaitUntilInitialized;
    if not (fPropagatorHandle.Connected) then
      raise EBold.CreateFmt('%s.StartListenerThread: PropagatorHandle not connected', [ClassName]);
    ListenerThread.Propagator := fPropagatorHandle.ClientHandler;
    ListenerThread.OnPropagatorFailure := fPropagatorHandle.DoPropagatorCallFailed;
    ListenerThread.OnThreadError := OnThreadError;
    PostThreadMessage(ListenerThread.ThreadID, BM_THRD_REGISTER, 0, 0);
    if ListenerThread.WaitUntilRegistered then
      ListenerThread.SetQueueNotEmptyNotifyEvent(NotifyDequeuer)
    else
    begin
      ListenerThread.Suspend;
      if Assigned(FOnRegistrationFailed) then
        FOnRegistrationFailed(self);
    end;
  end;
end;

procedure TBoldListenerHandle.NotifyDequeuer(Sender: TObject);
begin
  if Assigned(Dequeuer) then
    Dequeuer.QueueNotEmpty;
  BoldEffectiveEnvironment.TriggerQueueMechanism;
end;

procedure TBoldListenerHandle.setLeaseDuration(Value: integer);
begin
  fLeaseDuration := Value;
  ListenerThread.LeaseDuration := fLeaseDuration;
end;

procedure TBoldListenerHandle.setPollingInterval(Value: integer);
begin
  fPollingInterval := Value;
  ListenerThread.PollingInterval := fPollingInterval;
end;

procedure TBoldListenerHandle.setAutoExtendLease(Value: Boolean);
begin
  fAutoExtendLease := Value;
  ListenerThread.AutoExtendLease := fAutoExtendLease;
end;

function TBoldListenerHandle.ExtendLease: Boolean;
begin
  Result := ListenerThread.ExtendLease;
end;

procedure TBoldListenerHandle.SetActive(Value: Boolean);
begin
  if (Value <> fActive) then
  begin
    fActive := Value;
    if fActive then
    begin
      if Assigned(fPropagatorHandle) and fPropagatorHandle.Connected then
        StartListenerThread;
    end
    else
      SuspendListenerThread;
  end;
end;

procedure TBoldListenerHandle.SuspendListenerThread;
begin
  if Assigned(fBoldListenerThread) and not(ListenerThread.Suspended) then
  begin
    ListenerThread.UnRegister;
    ListenerThread.Propagator := nil;
    ListenerThread.Suspend;
  end;
end;

procedure TBoldListenerHandle.setClientIdentifierString(
  const Value: string);
begin
  FClientIdentifierString := Value;
  if Assigned(fBoldListenerThread) then
    fBoldListenerThread.ClientIdentifierString := FClientIdentifierString;
end;

procedure TBoldListenerHandle._ReceivePropagatorHandleEvents( Originator: TObject; OriginalEvent: TBoldEvent;
  RequestedEvent: TBoldRequestedEvent);
begin
  case OriginalEvent of
    beDestroying, beDisconnecting: begin
      if Assigned(fBoldListenerThread) and IsLoaded then
      begin
        fBoldListenerThread.UnRegister;
        fBoldListenerThread.Propagator := nil;
      end;
    end;
    beConnected: if (fActive and AutoStart) then
                   StartListenerThread;
    beDisconnected: SuspendListenerThread;
    beModified: if Assigned(fBoldListenerThread) then
                  fBoldListenerThread.OnPropagatorFailure := fPropagatorHandle.DoPropagatorCallFailed;
  end;
end;

procedure TBoldListenerHandle.SetPropagatorHandle(Value: TBoldAbstractPropagatorHandle);
begin
  if Value <> fPropagatorHandle then
  begin
    if fActive then
      raise EBold.CreateFmt(sCannotChangeHandleWhenActive, [ClassName]);
    fPropagatorHandleSubscriber.CancelAllSubscriptions;
    Subscribe(False);
    fPropagatorHandle := Value;
    Subscribe(True);
    if Assigned(fPropagatorHandle) then
    begin
      fPropagatorHandle.AddSubscription(fPropagatorHandleSubscriber, beConnected, 0);
      fPropagatorHandle.AddSubscription(fPropagatorHandleSubscriber, beDisConnected, 0);
      fPropagatorHandle.AddSubscription(fPropagatorHandleSubscriber, beDisconnecting, 0);
      fPropagatorHandle.AddSubscription(fPropagatorHandleSubscriber, beModified, 0);
    end
    else if Assigned(fBoldListenerThread) then
      fBoldListenerThread.OnPropagatorFailure := nil;
  end;
end;

procedure TBoldListenerHandle.SetOnExtendLeaseFailed(const Value: TBoldExtendLeaseFailureEvent);
begin
  FOnExtendLeaseFailed := Value;
  if Assigned(fBoldListenerThread) then
    fBoldListenerThread.OnExtendLeaseFailed := FOnExtendLeaseFailed;
end;

function TBoldListenerHandle.GetIsLoaded: Boolean;
begin
  Result := not(csDesigning in ComponentState) and not(csLoading in ComponentState);
end;

procedure TBoldListenerHandle.SetExtendLeaseAfter(const Value: integer);
begin
  if (value < 10) or (value > 90) then
    raise EBold.CreateFmt(sValueOutOfRange, [ClassName]);
  fExtendLeaseAfter := Value;
  ListenerThread.ExtendLeaseAfter := Value;
end;

procedure TBoldListenerHandle.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent = fDequeuer) and (Operation = opRemove) then
    fDequeuer := nil;
  if (AComponent = fPropagatorHandle) and (Operation = opRemove) then
  begin
    if Assigned(fBoldListenerThread) then
      fBoldListenerThread.OnPropagatorFailure := nil;
    fPropagatorHandle := nil;
  end;
end;

procedure TBoldListenerHandle.Subscribe(const DoSubscribe: Boolean);
begin
  if DoSubscribe then
  begin
    if not (csDesigning in ComponentState) then
    begin
      if Assigned(fDequeuer) then
        fDequeuer.AddSmallSubscription(fPTSubscriber, [beDestroying], beDestroying);
      if Assigned(fPropagatorHandle) then
        fPropagatorHandle.AddSmallSubscription(fPTSubscriber, [beDestroying], beDestroying);
    end;
  end
  else
    fPTSubscriber.CancelAllSubscriptions;
end;


end.
