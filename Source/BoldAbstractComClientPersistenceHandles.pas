
{ Global compiler directives }
{$include bold.inc}
unit BoldAbstractComClientPersistenceHandles;

interface

uses
  BoldSubscription,
  BoldComClientHandles,
  BoldAbstractComPersistenceControllerProxy,
  BoldPersistenceHandle;

type
  TBoldAbstractComClientPersistenceHandle = class(TBoldPersistenceHandle)
  private
    FConnectionHandle: TBoldComConnectionHandle;
    FInitialized: Boolean;
    FObjectName: string;
    FSubscriber: TBoldPassthroughSubscriber;
    function GetProxyController: TBoldAbstractComPersistenceControllerProxy;
    function GetSubscriber: TBoldPassthroughSubscriber;
    procedure Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    property ProxyController: TBoldAbstractComPersistenceControllerProxy read GetProxyController;
    property Subscriber: TBoldPassthroughSubscriber read GetSubscriber;
  protected
    procedure SetActive(Value: Boolean); override;
    procedure SetConnectionHandle(Value: TBoldComConnectionHandle);
    procedure SetObjectName(const Value: string);
    property Initialized: Boolean read FInitialized;
  published
    destructor Destroy; override;
    property ConnectionHandle: TBoldComConnectionHandle read FConnectionHandle write SetConnectionHandle;
    property ObjectName: string read FObjectName write SetObjectName;
  end;

implementation

uses
  BoldComClient;

destructor TBoldAbstractComClientPersistenceHandle.Destroy;
begin
  if Assigned(FSubscriber) then
    FSubscriber.Free;
  inherited;
end;

function TBoldAbstractComClientPersistenceHandle.GetProxyController: TBoldAbstractComPersistenceControllerProxy;
begin
  Result := PersistenceController as TBoldAbstractComPersistenceControllerProxy;
end;

function TBoldAbstractComClientPersistenceHandle.GetSubscriber: TBoldPassthroughSubscriber;
begin
  if not Assigned(FSubscriber) then
    FSubscriber := TBoldPassthroughSubscriber.Create(Receive);
  Result := FSubscriber;
end;

procedure TBoldAbstractComClientPersistenceHandle.Receive(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  case OriginalEvent of
    bceHandleInit:
    begin
      FInitialized := True;
      if Active then
      begin
        ProxyController.Connect(ConnectionHandle.BoldProvider, ObjectName);
        SendEvent(Self,bceHandleInit);
      end;
    end;
    bceHandleTerm:
    begin
      if Initialized and Active then
      begin
        SendEvent(Self,bceHandleTerm);
        FInitialized := False;
      end;
      ProxyController.Disconnect;
    end;
  end;
end;

procedure TBoldAbstractComClientPersistenceHandle.SetActive(Value: Boolean);
begin
  if Value <> Active then
  begin
    if Value then
    begin
      inherited;
      if Initialized then
      begin
        ProxyController.Connect(ConnectionHandle.BoldProvider, ObjectName);
        SendEvent(Self,bceHandleInit);
      end;
    end
    else
    begin
      if Initialized then
      begin
        SendEvent(Self,bceHandleTerm);
        ProxyController.Disconnect;
      end;
      inherited;
    end;
  end;
end;

procedure TBoldAbstractComClientPersistenceHandle.SetConnectionHandle(
  Value: TBoldComConnectionHandle);
begin
  if Value <> ConnectionHandle then
  begin
    if Assigned(ConnectionHandle) then
    begin
      Subscriber.CancelAllSubscriptions;
      Receive(Self,bceHandleTerm,bceHandleTerm);
    end;
    FConnectionHandle := Value;
    if Assigned(ConnectionHandle) then
    begin
      ConnectionHandle.AddSubscription(Subscriber, bceHandleTerm, 0);
      ConnectionHandle.AddSubscription(Subscriber,bceHandleInit, 0);
      if ConnectionHandle.Connected then
        Receive(Self,bceHandleInit,bceHandleInit);
    end;
  end;
end;

procedure TBoldAbstractComClientPersistenceHandle.SetObjectName(const Value: string);
begin
  if Value <> ObjectName then
  begin
    FObjectName := Value;
    if Initialized and Active then
    begin
      ProxyController.Disconnect;
      ProxyController.Connect(ConnectionHandle.BoldProvider,ObjectName);
      SendEvent(Self,bceHandleChange);
    end;
  end;
end;

end.
