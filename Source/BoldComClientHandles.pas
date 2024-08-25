
{ Global compiler directives }
{$include bold.inc}
unit BoldComClientHandles;

interface

uses
  Classes,
  BoldSubscription,
  BoldClientHandles,
  BoldComConnection,
  BoldComClient;

type
  { forward declarations }
  TBoldComConnectionHandle = class;
  TBoldComImportHandle = class;
  TBoldComClientObjectHandle = class;

  {-- TBoldComConnectionHandle --}
  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldComConnectionHandle = class(TBoldClientHandle)
  private
    FAfterConnect: TNotifyEvent;
    FAfterDisconnect: TNotifyEvent;
    FAutoConnect: Boolean;
    FBeforeConnect: TNotifyEvent;
    FBeforeDisconnect: TNotifyEvent;
    FBoldProvider: IBoldProvider;
    FConnection: TBoldComClientConnection;
    FHandlesInitialized: Boolean;
    FOnConnectFailed: TNotifyEvent;
    FServerCLSID: TGUID;
    FServerEvents: Boolean;
    FServerHost: string;
    FServerName: string;
    FSubscriber: TBoldExtendedPassthroughSubscriber;
    FThreaded: Boolean;
    FECode: HResult;
    function GetBoldProvider: IBoldProvider;
    function GetConnected: Boolean;
    function GetServerCLSID: string;
    function GetSubscriber: TBoldExtendedPassthroughSubscriber;
    procedure ReceiveExtended(Originator: TObject; OriginalEvent: TBoldEvent;
      RequestedEvent: TBoldRequestedEvent; const Args: array of const);
    procedure SetConnected(Value: Boolean);
    procedure SetServerCLSID(const Value: string);
    procedure SetServerEvents(Value: Boolean);
    procedure SetServerHost(const Value: string);
    procedure SetServerName(const Value: string);
    procedure SetThreaded(Value: Boolean);
    property Subscriber: TBoldExtendedPassthroughSubscriber read GetSubscriber;
  protected
    function GetHandledObject: TObject; override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property BoldProvider: IBoldProvider read GetBoldProvider;
    property Connected: Boolean read GetConnected write SetConnected;
    property ECode: HResult read FECode;    
  published
    property AfterConnect: TNotifyEvent read FAfterConnect write FAfterConnect;
    property AfterDisconnect: TNotifyEvent read FAfterDisconnect write FAfterDisconnect;
    property AutoConnect: Boolean read FAutoConnect write FAutoConnect default False;
    property BeforeConnect: TNotifyEvent read FBeforeConnect write FBeforeConnect;
    property BeforeDisconnect: TNotifyEvent read FBeforeDisconnect write FBeforeDisconnect;
    property OnConnectFailed: TNotifyEvent read FOnConnectFailed write FOnConnectFailed;
    property ServerCLSID: string read GetServerCLSID write SetServerCLSID;
    property ServerEvents: Boolean read FServerEvents write SetServerEvents default True;
    property ServerHost: string read FServerHost write SetServerHost;
    property ServerName: string read FServerName write SetServerName;
    property Threaded: Boolean read FThreaded write SetThreaded default False;
  end;

  TBoldComImportHandle = class(TBoldImportHandle)
  private
    FConnectionHandle: TBoldComConnectionHandle;
    FConnectionSubscriber: TBoldPassthroughSubscriber;
    function GetConnectionSubscriber: TBoldPassthroughSubscriber;
  protected
    procedure ConnectionClosing; virtual;
    procedure ConnectionOpened; virtual;
    procedure ConnectionSubscriber_Receive(Originator: TObject;
      OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    procedure DoConnect;
    procedure DoDisconnect;
    function GetConnected: Boolean; virtual;
    function GetEffectiveActive: Boolean; virtual;
    function GetEffectiveConnectionHandle: TBoldComConnectionHandle; virtual;
    function GetHandledObject: TObject; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetActive(Value: Boolean); override;
    procedure SetConnectionHandle(Value: TBoldComConnectionHandle); virtual;
    procedure SetObjectName(const Value: string); override;
    property ConnectionSubscriber: TBoldPassthroughSubscriber read GetConnectionSubscriber;
  public
    destructor Destroy; override;
    property Connected: Boolean read GetConnected;
    property EffectiveActive: Boolean read GetEffectiveActive;
    property EffectiveConnectionHandle: TBoldComConnectionHandle read GetEffectiveConnectionHandle;
  published
    property Active;
    property ConnectionHandle: TBoldComConnectionHandle read FConnectionHandle write SetConnectionHandle;
    property ObjectName;
  end;

  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldComClientObjectHandle = class(TBoldComImportHandle)
  private
    FComObject: IUnknown;
    function GetComObject: IUnknown;
  protected
    procedure ConnectionClosing; override;
  public
    property ComObject: IUnknown read GetComObject;
  end;

implementation

uses
  SysUtils,
  ComObj,
  BoldComUtils;

{-- TBoldComConnectionHandle -----------------------------------------------------}

constructor TBoldComConnectionHandle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConnection := TBoldComClientConnection.Create;
  FConnection.AddSmallSubscription(Subscriber,[bceConnecting,
    bceConnectFailed,bceConnected,bceDisconnecting,bceDisconnected],0);
  FServerEvents := True;
end;

destructor TBoldComConnectionHandle.Destroy;
begin
  if assigned(fConnection) then
    Connected := False;
  FreeAndNil(FConnection);
  FreeAndNil(FSubscriber);
  inherited Destroy;
end;

function TBoldComConnectionHandle.GetBoldProvider: IBoldProvider;
begin
  if not Assigned(FBoldProvider) then
  begin
    if Connected then
      FECode := FConnection.BoldServer.QueryInterface(IBoldProvider,FBoldProvider);
  end;
  Result := FBoldProvider;
end;

function TBoldComConnectionHandle.GetConnected: Boolean;
begin
  Result := assigned(FConnection) and (FConnection.ConnectionState = bceConnected);
end;

function TBoldComConnectionHandle.GetHandledObject: TObject;
begin
  Result := FConnection;
end;

function TBoldComConnectionHandle.GetServerCLSID: string;
begin
  if (FServerCLSID.D1 <> 0) or (FServerCLSID.D2 <> 0) or (FServerCLSID.D3 <> 0) then
    Result := GUIDToString(FServerCLSID)
  else
    Result := '';
end;

function TBoldComConnectionHandle.GetSubscriber: TBoldExtendedPassthroughSubscriber;
begin
  if not Assigned(FSubscriber) then
    FSubscriber := TBoldExtendedPassthroughSubscriber.CreateWithExtendedReceive(ReceiveExtended);
  Result := FSubscriber;
end;

procedure TBoldComConnectionHandle.Loaded;
begin
  inherited Loaded;
  if FAutoConnect and not (csDesigning in ComponentState) then
    Connected := True;
end;

procedure TBoldComConnectionHandle.ReceiveExtended(Originator: TObject; OriginalEvent: TBoldEvent;
  RequestedEvent: TBoldRequestedEvent; const Args: array of const);

  function GetHResult(const VR: TVarRec): Integer;
  begin
    case VR.VType of
      vtInteger: result := VR.VInteger;
      else
        raise Exception.Create('unknown type in GetHResult');
    end;
  end;

begin
  case OriginalEvent of
    bceConnected:
    begin
      SendEvent(Self,OriginalEvent);
      SendEvent(Self,bceHandleInit);
      if Assigned(AfterConnect) then AfterConnect(Self);
      FHandlesInitialized := True;
    end;
    bceDisconnected:
    begin
      SendEvent(Self,OriginalEvent);
      if Assigned(AfterDisconnect) then AfterDisconnect(Self);
    end;
    bceConnecting:
    begin
      SendEvent(Self,OriginalEvent);
    end;
    bceConnectFailed:
    begin
      SendEvent(Self,OriginalEvent);
      FECode := GetHResult(Args[0]);
      if Assigned(OnConnectFailed) then OnConnectFailed(Self);
    end;
    bceDisconnecting:
    begin
      if Assigned(BeforeDisconnect) then BeforeDisconnect(Self);
      SendEvent(Self,OriginalEvent);
      if FHandlesInitialized then
      begin
        SendEvent(Self,bceHandleTerm);
        FHandlesInitialized := False;
      end;
      FBoldProvider := nil;
    end;
  end;
end;


procedure TBoldComConnectionHandle.SetConnected(Value: Boolean);
var
  ClassID: TGUID;
begin
  if Value then
  begin
    if FConnection.ConnectionState <> bceDisconnected then Exit;
    if ServerCLSID = '' then
    begin
      if ServerName = '' then
        raise EBoldCom.Create('Cannot connect, no server specified.');
      try
        ClassID := ProgIDToClassId(ServerName);
      except
        on Exception do
          raise EBoldCom.Create('Cannot connect, invalid server name.');
      end;
    end
    else
      ClassID := FServerCLSID;
    if Assigned(BeforeConnect) then BeforeConnect(Self);
    FConnection.Connect(ServerHost,ClassID,Threaded,ServerEvents);
  end
  else
  begin
    if FConnection.ConnectionState <> bceConnected then
      Exit;
    FConnection.Disconnect;
  end;
end;

procedure TBoldComConnectionHandle.SetServerCLSID(const Value: string);
begin
  if Value <> ServerCLSID then
  begin
    if FConnection.ConnectionState <> bceDisconnected then
      raise EBoldCom.Create('Cannot change ServerCLSID on active connection.');
    if Value = '' then
      FillChar(FServerCLSID, SizeOf(FServerCLSID), 0)
    else begin
      FServerCLSID := StringToGUID(Value);
      if not (csReading in ComponentState) then
      begin
        try
          FServerName := ClassIDToProgID(FServerCLSID);
        except
          on Exception do FServerName := '';
        end;
      end;
    end;
  end;
end;

procedure TBoldComConnectionHandle.SetServerEvents(Value: Boolean);
begin
  if Value <> FServerEvents then
  begin
    if Connected then
      FConnection.Reconnect(Value);
    FServerEvents := Value;
  end;
end;

procedure TBoldComConnectionHandle.SetServerHost(const Value: string);
begin
  if Value <> FServerHost then
  begin
    if FConnection.ConnectionState <> bceDisconnected then
      raise EBoldCom.Create('Cannot change ServerHost on active connection.');
    FServerHost := Value;
  end;
end;

procedure TBoldComConnectionHandle.SetServerName(const Value: string);
begin
  if Value <> ServerName then
  begin
    if FConnection.ConnectionState <> bceDisconnected then
      raise EBoldCom.Create('Cannot change ServerName on active connection.');
    FServerName := Value;
    if (FServerName <> '') and not (csReading in ComponentState) then
    begin
      try
        FServerCLSID := ProgIDToClassID(Value);
      except
        on Exception do FillChar(FServerCLSID, SizeOf(FServerCLSID), 0);
      end;
    end;
  end;
end;

procedure TBoldComConnectionHandle.SetThreaded(Value: Boolean);
begin
  if Value <> FThreaded then
  begin
    if FConnection.ConnectionState <> bceDisconnected then
      raise EBoldCom.Create('Cannot change Threaded on active connection.');
    FThreaded := Value;
  end;
end;

{-- TBoldComImportHandle ------------------------------------------------------}

destructor TBoldComImportHandle.Destroy;
begin
  FreeAndNil(FConnectionSubscriber);
  inherited;
end;

procedure TBoldComImportHandle.ConnectionOpened;
begin
end;

procedure TBoldComImportHandle.ConnectionClosing;
begin
end;

procedure TBoldComImportHandle.DoConnect;
begin
  ConnectionSubscriber_Receive(Self,bceHandleInit,bceHandleInit);
end;

procedure TBoldComImportHandle.DoDisconnect;
begin
  ConnectionSubscriber_Receive(Self,bceHandleTerm,bceHandleTerm);
end;

function TBoldComImportHandle.GetConnected: Boolean;
begin
  Result := EffectiveActive and Assigned(EffectiveConnectionHandle) and
    EffectiveConnectionHandle.Connected;
end;

function TBoldComImportHandle.GetEffectiveActive: Boolean;
begin
  Result := Active;
end;

function TBoldComImportHandle.GetEffectiveConnectionHandle: TBoldComConnectionHandle;
begin
  Result := ConnectionHandle;
end;

function TBoldComImportHandle.GetHandledObject: TObject;
begin
  Result := nil;
end;

function TBoldComImportHandle.GetConnectionSubscriber: TBoldPassthroughSubscriber;
begin
  if not Assigned(FConnectionSubscriber) then
    FConnectionSubscriber := TBoldPassthroughSubscriber.Create(ConnectionSubscriber_Receive);
  Result := FConnectionSubscriber;
end;

procedure TBoldComImportHandle.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = ConnectionHandle) then
    ConnectionHandle := nil;
end;

procedure TBoldComImportHandle.ConnectionSubscriber_Receive(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  case OriginalEvent of
    bceHandleInit:
    begin
      if EffectiveActive then
      begin
        ConnectionOpened;
        SendEvent(Self,bceHandleInit);
      end;
    end;
    bceHandleTerm:
    begin
      if EffectiveActive then
      begin
        SendEvent(Self,bceHandleTerm);
        ConnectionClosing;
      end;
    end;
  end;
end;

procedure TBoldComImportHandle.SetActive(Value: Boolean);
begin
  if Value <> Active then
  begin
    if Value then
    begin
      inherited;
      if Connected then
        DoConnect;
    end
    else
    begin
      if Connected then
        DoDisconnect;
      inherited;
    end;
  end;
end;

procedure TBoldComImportHandle.SetConnectionHandle(Value: TBoldComConnectionHandle);
begin
  if Value <> ConnectionHandle then
  begin
    ConnectionSubscriber.CancelAllSubscriptions;
    if Connected then
      DoDisconnect;
    FConnectionHandle := Value;
    if Assigned(ConnectionHandle) then
    begin
      ConnectionHandle.AddSubscription(ConnectionSubscriber, bceHandleInit, 0);
      ConnectionHandle.AddSubscription(ConnectionSubscriber, bceHandleTerm, 0);
      if Connected then
        DoConnect;
    end;
  end;
end;

procedure TBoldComImportHandle.SetObjectName(const Value: string);
begin
  if Value <> ObjectName then
  begin
    if Connected then
    begin
      DoDisconnect;
      inherited;
      DoConnect;
    end
    else
      inherited;
  end;
end;

{-- TBoldComClientObjectHandle ------------------------------------------------------}

procedure TBoldComClientObjectHandle.ConnectionClosing;
begin
  FComObject := nil;
end;

function TBoldComClientObjectHandle.GetComObject: IUnknown;
begin
  if not Assigned(FComObject) then
  begin
    if Connected then
      FComObject := EffectiveConnectionHandle.BoldProvider.GetObject(ObjectName);
  end;
  Result := FComObject;
end;

end.
