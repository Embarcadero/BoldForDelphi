
{ Global compiler directives }
{$include bold.inc}
unit BoldEnqueuer;

interface

uses
  Variants,
  BoldThreadSafeQueue,
  BoldDefs,
  SyncObjs,
  BoldClientHandler,
  BoldPropagatorConstants,
  BoldLoggableCriticalSection,
  BoldThreadSafeLog,
  windows,
  classes
;

type
  TBoldExternalEventType = (bemEvent, bemSubscription, bemCancelSubscription, bemRemoveClientQueue,
                            bemGetLock, bemLockLost);

  {forward declarations}
  TBoldEnqueuer = class;
  TBoldExternalEvent = class;

  {TBoldExternalEvent}
  TBoldExternalEvent = class
  private
    fEventType: TBoldExternalEventType;
    fEventName: string;
    fBoldClientID: TBoldClientID;
  public
    constructor Create(BoldClientID: TBoldClientID; EventType: TBoldExternalEventType; EventName: string);
    property EventType: TBoldExternalEventType read fEventType write fEventType;
    property EventName: string read fEventName write fEventName;
    property BoldClientID: TBoldClientID read fBoldClientID write fBoldClientID;
  end;

  TBoldExternalEventThreadSafeObjectQueue = class(TBoldThreadSafeObjectQueue)
  public
    procedure EnqueueVarArray(ClientId: TBoldClientId; EventType: TBoldExternalEventType; Events: OleVariant);
  end;

  {TBoldEnqueuer}
  TBoldEnqueuer = class
  private
    FInQueue: TBoldExternalEventThreadSafeObjectQueue;
    FClientHandler: TBoldClientHandler;
    fEnabled: Boolean;
    fLock: TBoldLoggableCriticalSection;
    function getInQueue: TBoldThreadSafeObjectQueue;
    procedure SetEnabled(Value: Boolean);
    function GetEnabled: Boolean;
    function CheckStatus(ClientId: TBoldClientId; VarArray: OLEVariant): HResult;
  public
    constructor Create(ClientHandler: TBoldClientHandler);
    destructor Destroy; override;
    function SendLockEvent(BoldClientID: Integer; Event: string; const AcquiredLock: Boolean): Boolean;
    {IBoldEventReceiver}
    function SendEvents(BoldClientID: Integer; Events: OleVariant): HResult; stdcall;
    function AddSubscriptions(BoldClientID: Integer; Subscriptions: OleVariant): HResult; stdcall;
    function CancelSubscriptions(BoldClientID: Integer; Subscriptions: OleVariant): HResult; stdcall;
    property InQueue: TBoldThreadSafeObjectQueue read getInQueue;
    property ClientHandler: TBoldClientHandler read FClientHandler write FClientHandler;
    property Enabled: Boolean read GetEnabled write SetEnabled;
  end;


implementation

uses
  SysUtils,
  BoldUtils
  ;

  {TBoldExternalEvent}
constructor TBoldExternalEvent.Create(BoldClientID: TBoldClientID; EventType: TBoldExternalEventType;
                                        EventName: string);
begin
  inherited Create;
  self.BoldClientID := BoldClientID;
  self.EventType := EventType;
  self.EventName := EventName;
end;

  {TBoldEnqueuer}

constructor TBoldEnqueuer.Create(ClientHandler: TBoldClientHandler);
begin
  inherited Create;
  FClientHandler := ClientHandler;
  fLock := TBoldLoggableCriticalSection.Create('EQ');
  fLock.Acquire;
  try
    fEnabled := false;
    fInQueue := TBoldExternalEventThreadSafeObjectQueue.Create('InQ');
    fEnabled := true;
  finally
    fLock.Release;
  end;
end;

function  TBoldEnqueuer.SendEvents(BoldClientID: Integer; Events: OleVariant): HResult; stdcall;
begin
  fLock.Acquire;
  try
    result := CheckStatus(BoldClientId, Events);
    try
      {if client is registered then enqueue events}
      if (Result = S_OK) or (Result = W_CLIENT_NOT_RECEIVING) then
        FInQueue.EnqueueVarArray(BoldClientId, bemEvent, Events);
    except on E: Exception do
      begin
        Result := E_FAIL;
        BoldLogError('%s.SendEvents ClientId=%d: %s', [ClassName, BoldClientId, E.Message]);
      end;
    end;
  finally
    fLock.Release;
  end;
end;

function  TBoldEnqueuer.AddSubscriptions(BoldClientID: Integer; Subscriptions: OleVariant): HResult; stdcall;
begin
  fLock.Acquire;
  try
    result := CheckStatus(BoldClientId, Subscriptions);
    try
      {if client is registered then enqueue subscriptions}
      if (Result = S_OK) or (Result = W_CLIENT_NOT_RECEIVING) then
        FInQueue.EnqueueVarArray(BoldClientId, bemSubscription, Subscriptions);
    except on E: Exception do
      begin
        Result := E_FAIL;
        BoldLogError('%s.AddSubscriptions ClientId=%d: %s', [ClassName, BoldClientId, E.Message]);
      end;
    end;
  finally
    fLock.Release;
  end;
end;

function  TBoldEnqueuer.CancelSubscriptions(BoldClientID: Integer; Subscriptions: OleVariant): HResult; stdcall;
begin
  fLock.Acquire;
  try
    result := CheckStatus(BoldClientId, Subscriptions);
    try
      if (Result = S_OK) or (Result = W_CLIENT_NOT_RECEIVING) then
        FInQueue.EnqueueVarArray(BoldClientId, bemCancelSubscription, Subscriptions);
    except on E: Exception do
      begin
        Result := E_FAIL;
        BoldLogError('%s.CancelSubscriptions ClientId=%d: %s', [ClassName, BoldClientId, E.Message]);
      end;
    end;
  finally
    fLock.Release;
  end;
end;

function TBoldEnqueuer.getInQueue: TBoldThreadSafeObjectQueue;
begin
  Result := fInQueue;
end;

destructor TBoldEnqueuer.Destroy;
begin
  FInQueue.Clear;
  FreeAndNil(fInQueue);
  FreeAndNil(fLock);
  inherited;
end;


function TBoldEnqueuer.SendLockEvent(BoldClientID: Integer;
  Event: string; const AcquiredLock: Boolean): Boolean;
begin
  fLock.Acquire;
  try
    Result := false;
    {if client is registered then enqueue event}
    if fEnabled and ClientHandler.IsRegistered(BoldClientID) then
    begin
      try
        if AcquiredLock then
          FInQueue.Enqueue(TBoldExternalEvent.Create(BoldClientID, bemGetLock, Event))
        else
          FInQueue.Enqueue(TBoldExternalEvent.Create(BoldClientID, bemLockLost, Event));
        Result := True;
      except on E: Exception do
        BoldLogError('%s.SendLockEvent ClientId=%d: %s', [ClassName, BoldClientId, E.Message]);
      end;
    end;
  finally
    fLock.Release;
  end;
end;

procedure TBoldEnqueuer.SetEnabled(Value: Boolean);
begin
  fLock.Acquire;
  try
    if (Value <> fEnabled) then
    begin
      fEnabled := Value;
      if not fEnabled and Assigned(FInQueue) then
        FInQueue.Clear;
    end;
  finally
    fLock.Release;
  end;
end;

function TBoldEnqueuer.GetEnabled: Boolean;
begin
  fLock.Acquire;
  try
    result := fEnabled;
  finally
    fLock.Release;
  end;
end;

function TBoldEnqueuer.CheckStatus(ClientId: TBoldClientId; VarArray: OLEVariant): HResult;
var
  ClientIdString: String;
  RegTime: TTimeStamp;
  Initialized: Boolean;
  Status: TBoldClientReceiveStatus;
begin
  if not fEnabled then
    result := E_ENQUEUER_NOT_ENABLED
  else if not ClientHandler.IsRegistered(ClientId) then
    result := E_CLIENT_NOT_REGISTERED
  else if not VarIsArray(VarArray) then
    result := E_INVALID_PARAMETER
  else
  begin
    ClientHandler.HasInfoForClient(ClientId, ClientIdString, RegTime, Initialized, Status);
    if Status = crsNotReceiving then
      result := W_CLIENT_NOT_RECEIVING
    else
      result := S_OK;
    result := S_OK;

  end;
end;

{ TBoldExternalEventThreadSafeObjectQueue }

procedure TBoldExternalEventThreadSafeObjectQueue.EnqueueVarArray(
    ClientId: TBoldClientId; EventType: TBoldExternalEventType; Events: OleVariant);
var
  i: integer;
begin
  Lock;
  try
    for i := VarArrayLowBound(Events, 1) to VarArrayHighBound(Events, 1) do
      UnsafeEnqueue(TBoldExternalEvent.Create(ClientID, EventType, Events[i]));
  finally
    UnLock;
  end;
end;

end.
