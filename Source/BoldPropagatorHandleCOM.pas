
{ Global compiler directives }
{$include bold.inc}
unit BoldPropagatorHandleCOM;

interface

uses
  BoldAbstractPropagatorHandle,
  BoldComClientHandles,
  BoldPropagatorInterfaces_TLB,
  BoldSubscription,
  BoldDefs,
  comobj,
  Classes
  ;

type
  {forward declarations}
  TBoldPropagatorHandleCOM = class;

  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldPropagatorHandleCOM = class(TBoldAbstractPropagatorHandle)
  private
    FActive: Boolean;
    FConnectionHandle: TBoldComConnectionHandle;
    FPassthroughSubscriber: TBoldPassthroughSubscriber;
    FOnPropagatorCallFailed: TBoldNotifyEventWithErrorMessage;
    FEventPropagator: IBoldEventPropagator;
    FClientHandler: IBoldClientHandler;
    procedure SetActive(Value: Boolean);
    function GetEventPropagatorObject: IBoldEventPropagator;
    procedure DoConnect;
    procedure DoDisconnect;
    procedure SetConnectionHandle(const Value: TBoldComConnectionHandle);
    function GetClientHandlerObject: IBoldClientHandler;
    procedure SetOnPropagatorCallFailed(const Value: TBoldNotifyEventWithErrorMessage);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetClientHandler: IBoldClientHandler; override;
    function GetEventPropagator: IBoldEventPropagator; override;
    procedure _Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    function GetConnected: Boolean; override;
    procedure SetConnected(const Value: Boolean); override;
    property ClientHandlerObject: IBoldClientHandler read GetClientHandlerObject;
    property EventPropagatorObject: IBoldEventPropagator read GetEventPropagatorObject;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure DoPropagatorCallFailed(Sender: TObject; const ErrorMessage: string); override;
    property Connected: Boolean read GetConnected write SetConnected;
  published
    property Active: Boolean read FActive write SetActive;
    property ConnectionHandle: TBoldComConnectionHandle read FConnectionHandle write SetConnectionHandle;
    property OnPropagatorCallFailed: TBoldNotifyEventWithErrorMessage read FOnPropagatorCallFailed write SetOnPropagatorCallFailed;
  end;

implementation

uses
  SysUtils,
  BoldUtils,
  BoldComUtils,
  BoldPropagatorConstants,
  BoldComClient,
  ActiveX;

{ TBoldPropagatorHandleCOM }

constructor TBoldPropagatorHandleCOM.Create(Owner: TComponent);
begin
  inherited;
  fPassthroughSubscriber := TBoldPassthroughSubscriber.Create(_Receive);
  fActive := True;
end;

destructor TBoldPropagatorHandleCOM.Destroy;
begin
  DoDisconnect;
  Active := false;
  fPassthroughSubscriber.CancelAllSubscriptions;
  FreeAndNil(fPassthroughSubscriber);
  inherited;
end;

procedure TBoldPropagatorHandleCOM.DoPropagatorCallFailed(Sender: TObject; const ErrorMessage: string);
begin
  if Assigned(FOnPropagatorCallFailed) then
    FOnPropagatorCallFailed(Sender, ErrorMessage);
end;

procedure TBoldPropagatorHandleCOM.DoConnect;
begin
  SendEvent(self, beConnected);
end;

procedure TBoldPropagatorHandleCOM.DoDisconnect;
begin
  SendEvent(self, beDisconnected);
  FEventPropagator := nil;
  FClientHandler := nil;
end;

function TBoldPropagatorHandleCOM.GetClientHandler: IBoldClientHandler;
begin
  Result := nil;
  if fActive then
  try
    Result := (ClientHandlerObject as IBoldClientHandler);
  except on E: Exception do
    DoPropagatorCallFailed(self, E.Message);
  end;
end;

function TBoldPropagatorHandleCOM.GetClientHandlerObject: IBoldClientHandler;
begin
  if Connected then
  begin
    if not Assigned(FClientHandler) then
    begin
      if Assigned(ConnectionHandle.BoldProvider) then
        FClientHandler := ConnectionHandle.BoldProvider.GetObject(CLIENT_HANDLER_OBJECT_NAME) as IBoldClientHandler
      else
        DoPropagatorCallFailed(self, Format('Failed to get IBoldClientHandler interface: %s', [SysErrorMessage(ConnectionHandle.ECode)]));
    end;
    Result := FClientHandler;
  end
  else
    Result := nil;
end;

function TBoldPropagatorHandleCOM.GetConnected: Boolean;
begin
  Result := Active and Assigned(ConnectionHandle) and ConnectionHandle.Connected;
end;

function TBoldPropagatorHandleCOM.GetEventPropagator: IBoldEventPropagator;
begin
  Result := nil;
  if fActive then
  try
    Result := (EventPropagatorObject as IBoldEventPropagator);
  except on E: Exception do
    DoPropagatorCallFailed(self, E.Message);
  end;
end;

function TBoldPropagatorHandleCOM.GetEventPropagatorObject: IBoldEventPropagator;
begin
  if Connected then
  begin
    if not Assigned(FEventPropagator)then
    begin
      if Assigned(ConnectionHandle.BoldProvider) then
        FEventPropagator := ConnectionHandle.BoldProvider.GetObject(ENQUEUER_OBJECT_NAME) as IBoldEventPropagator
      else
        DoPropagatorCallFailed(self,Format('Failed to get IBoldEventPropagator interface: %s', [SysErrorMessage(ConnectionHandle.ECode)]));
    end;
    Result := FEventPropagator;
  end
  else
    Result := nil;
end;

procedure TBoldPropagatorHandleCOM.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent=FConnectionHandle) and (Operation=opRemove) then
    FConnectionHandle := nil;
end;

procedure TBoldPropagatorHandleCOM.SetActive(Value: Boolean);
begin
  if Value <> FActive then
  begin
    if Value then
    begin
      FActive := Value;
      if not (csDesigning in ComponentState) and not (csLoading in ComponentState) and Connected then
        DoConnect;
    end
    else
    begin
      if not (csDesigning in ComponentState) and not (csLoading in ComponentState) and Connected then
        DoDisconnect;
      FActive := Value;
    end;
  end;
end;

procedure TBoldPropagatorHandleCOM.SetConnectionHandle(
  const Value: TBoldComConnectionHandle);
begin
  if (Value <> FConnectionHandle) then
  begin
    fPassthroughSubscriber.CancelAllSubscriptions;
    if Connected then
      DoDisconnect;
    FConnectionHandle := Value;
    if Assigned(FConnectionHandle) then
    begin
      FConnectionHandle.AddSmallSubscription(fPassthroughsubscriber, [bceDisconnected, bceConnected], 0);
      FConnectionHandle.AddSmallSubscription(fPassthroughSubscriber, [beDestroying], 0);
      if Connected then
        DoConnect;
    end;
  end;
end;

procedure TBoldPropagatorHandleCOM.SetOnPropagatorCallFailed(
  const Value: TBoldNotifyEventWithErrorMessage);
begin
  FOnPropagatorCallFailed := Value;
  SendEvent(self, beModified);
end;

procedure TBoldPropagatorHandleCOM._Receive(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  case OriginalEvent of
    bceConnected:
      if Active then
      begin
        SendEvent(Self, bceConnected);
        DoConnect;
      end;
    bceDisconnected:
      if Active then
      begin
        SendEvent(Self, bceDisconnected);
        DoDisconnect;
      end;
    beDestroying:
      ConnectionHandle := nil;
  end;
end;

procedure TBoldPropagatorHandleCOM.SetConnected(const Value: Boolean);
begin
  if (Connected <> Value) then
  begin
    if Value then
    begin
      FConnectionHandle.Connected := True;
      DoConnect;
    end
    else
    begin
      FConnectionHandle.Connected := false;
      DoDisconnect;
    end;
  end;
end;

end.
