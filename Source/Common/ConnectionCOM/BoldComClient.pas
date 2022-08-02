
{ Global compiler directives }
{$include bold.inc}
unit BoldComClient;

interface

uses
  BoldContainers,
  BoldSubscription,
  BoldClient,
  BoldComEventQueue,
  BoldIndexableList,
  BoldComConnection,
  BoldComObj;

const
  bceConnecting = 24;
  bceConnectFailed = 25;
  bceConnected = 26;
  bceDisconnecting = 27;
  bceDisconnected = 28;
  bceHandleInit = 29;
  bceHandleTerm = 30;
  bceHandleChange = 31;

type
  { forward declarations }
  TBoldComClientSubscriber = class;
  TBoldComClientInterface = class;
  TBoldComClientConnection = class;
  TBoldComClientEventQueue = class;
  TBoldComClient = class;

  {-- TBoldComClientSubscriber --}
  TBoldComClientSubscriber = class(TBoldSubscriber)
  private
    fSubscriberId: integer;
    function GetClientId: string;
  public
    constructor create;
    destructor Destroy; override;
    property ClientId: string read GetClientId;
    property SubscriberId: Integer read fSubscriberId;
  end;

  TBoldComClientPassthroughSubscriber = class(TBoldComClientSubscriber)
  private
    FOnReceive: TBoldEventHandler;
  public
    constructor Create(receiveFunc: TBoldEventHandler);
    procedure Receive(Originator: TObject; OriginalEvent: TBoldEvent;
      RequestedEvent: TBoldRequestedEvent); override;
  end;

  {-- TBoldComClientInterface --}
  TBoldComClientInterface = class(TBoldAutoInterfacedObject, IBoldClient)
  private
    FOwner: TBoldComClientConnection;
  protected
    function _Release: Integer; override;
    { IBoldClient }
    function OnServerEvent(Event: Integer; Data: OleVariant): OleVariant; safecall;
  public
    constructor Create(Owner: TBoldComClientConnection);
    destructor Destroy; override;
  end;

  {-- TBoldComClientConnection --}
  TBoldComClientConnection = class(TBoldSubscribableObject)
  private
    FBoldServer: IBoldServer;
    FClientInterface: TBoldComClientInterface;
    FConnectClientInterface: Boolean;
    FConnectionState: Integer;
    procedure Connected;
    procedure ConnectFailed(const res: HResult);
    procedure Connecting;
    procedure Disconnected;
    procedure Disconnecting;
    procedure ServerDisconnect;
  public
    constructor Create;
    destructor Destroy; override;
    function Connect(const HostName: string; const CLSID: TGUID;
      Threaded, ConnectClientInterface: Boolean): Boolean;
    function Disconnect: Boolean;
    function Reconnect(ConnectClientInterface: Boolean): Boolean;
    property BoldServer: IBoldServer read FBoldServer;
    property ConnectionState: Integer read FConnectionState;
  end;

  {-- TBoldComClientEventQueue --}
  TBoldComClientEventQueue = class(TBoldComEventQueue)
  public
    procedure HandleEvent(const EventData: TBoldComEventData); override;
  end;

  TBoldComClientSubscriberList = class(TBoldUnOrderedIndexableList)
  private
    function GetSubscriberById(SubscriberId: integer): TBoldComClientSubscriber;
  public
    constructor create;
    property SubscriberById[SubscriberId: integer]: TBoldComClientSubscriber read GetSubscriberById;
  end;

  {-- TBoldComClient --}
  TBoldComClient = class(TBoldClient)
  private
    FClientId: string;
    FUnusedSubscriberIds: TBoldIntegerArray;
    fQuaranteenedSubscriberIds: TBoldIntegerArray;
    FConnections: TBoldObjectArray;
    fSubscribers: TBoldComClientSubscriberList;
    FEventQueue: TBoldComClientEventQueue;
    fSubscriberIdcounter: integer;
    procedure AddConnection(Connection: TBoldComClientConnection);
    function GetConnectionByIndex(Index: Integer): TBoldComClientConnection;
    function GetConnectionCount: Integer;
    procedure RemoveConnection(Connection: TBoldComClientConnection);
  public
    constructor Create;
    destructor Destroy; override;
    class function Instance: TBoldComClient;
    procedure CancelSubscriptions(SubscriberId: Integer; Connection: TBoldComClientConnection = nil);
    procedure RemoveSubscriber(Subscriber: TBoldComClientSubscriber);
    procedure UnquaranteenSubscriberId(SubscriberId: integer);
    procedure AddSubscriber(Subscriber: TBoldComclientSubscriber);
    function GetSubscriber(SubscriberId: integer): TBoldComClientSubscriber;
    property ClientId: string read FClientId;
    property ConnectionCount: Integer read GetConnectionCount;
    property Connections[Index: Integer]: TBoldComClientConnection read GetConnectionByIndex;
    property EventQueue: TBoldComClientEventQueue read FEventQueue;
  end;

implementation

uses
  Windows,
  SysUtils,
  ActiveX,
  Variants,
  ComObj,
  BoldIndex,
  BoldHashIndexes,
  BoldGUIDUtils,
  BoldComUtils,
  BoldComThreads;

type
  TBoldProtectedAccessSubscriber = class(TBoldSubscriber)
  end;

var
  G_BoldComClient: TBoldComClient = nil;
  IX_COMClientSubscriber: integer = -1;

type
  TBoldComClientSubscriberHashIndex = class(TBoldCardinalHashIndex)
  protected
    function ItemAsKeyCardinal(Item: TObject): cardinal; override;
  public
    function FindSubscriberBySubscriberId(SubscriberId: integer): TBoldComClientSubscriber;
  end;

  
{-- TBoldComClientSubscriber --------------------------------------------------}

constructor TBoldComClientSubscriber.create;
begin
  inherited;
  TBoldComClient.Instance.AddSubscriber(self);
end;

destructor TBoldComClientSubscriber.Destroy;
begin
  TBoldComClient.Instance.RemoveSubscriber(Self);
  inherited;
end;

function TBoldComClientSubscriber.GetClientId: string;
begin
  Result := TBoldComClient.Instance.ClientId;
end;

constructor TBoldComClientPassthroughSubscriber.Create(receiveFunc: TBoldEventHandler);
begin
  inherited Create;
  FOnReceive := receiveFunc;
end;

procedure TBoldComClientPassthroughSubscriber.Receive(Originator: TObject; OriginalEvent: TBoldEvent;
  RequestedEvent: TBoldRequestedEvent);
begin
  if Assigned(FOnReceive) then
    FOnReceive(Originator,OriginalEvent,RequestedEvent);
end;

{-- TBoldComClientInterface ---------------------------------------------------}

constructor TBoldComClientInterface.Create(Owner: TBoldComClientConnection);
begin
  inherited Create(BoldComConnectionTypeLibrary,IBoldClient);
  FOwner := Owner;
end;

destructor TBoldComClientInterface.Destroy;
begin
  inherited Destroy;
end;

function TBoldComClientInterface._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
end;

function TBoldComClientInterface.OnServerEvent(Event: Integer;
  Data: OleVariant): OleVariant;
var
  EventData: TBoldComEventData;
  I,J,Count: Integer;
begin
  Result := True;
  if Event = EVENT_SUBSCRIPTION then
  begin
    Count := (VarArrayHighBound(Data, 1) + 1) div 4;
    J := 0;
    for I := 0 to Count - 1 do
    begin
      EventData.SubscriberId := Data[J];
      EventData.Originator := Data[J+1];
      EventData.OriginalEvent := Data[J+2];
      EventData.RequestedEvent := Data[J+3];
      TBoldComClient.Instance.EventQueue.Push(EventData);
      Inc(J,4);
    end;
  end
  else if Event = EVENT_DISCONNECT then
    FOwner.ServerDisconnect
  else
    Result := False;
end;

{-- TBoldComClientConnection --------------------------------------------------}

constructor TBoldComClientConnection.Create;
begin
  inherited Create;
  FClientInterface := TBoldComClientInterface.Create(Self);
  FConnectionState := bceDisconnected;
  TBoldComClient.Instance.AddConnection(Self);
end;

destructor TBoldComClientConnection.Destroy;
begin
  if (ConnectionState = bceConnected) then
    Disconnect;
  TBoldComClient.Instance.RemoveConnection(Self);
  FClientInterface.Free;
  inherited Destroy;
end;

procedure TBoldComClientConnection.ServerDisconnect;
begin
  if ConnectionState = bceConnected then
    Disconnect;
end;

function TBoldComClientConnection.Connect(const HostName: string;
  const CLSID: TGUID; Threaded, ConnectClientInterface: Boolean): Boolean;
var
  res: HResult;
begin
  Result := True;
  if (ConnectionState = bceConnected) then Exit;
  if (ConnectionState <> bceDisconnected) then
    raise EBoldCom.Create('Connection state error.');
  FConnectClientInterface := ConnectClientInterface;
  Connecting;
  try
    if Threaded then
    begin
      with TBoldComConnectionThread.Create(HostName,CLSID,IBoldServer) do
      begin
        try
          Connect(FBoldServer);
          res := ConnectRes;
        finally
          Free;
        end;
      end;
    end
    else
    begin
      if HostName <> '' then
        BoldCreateRemoteComObject(HostName,CLSID,IBoldServer,FBoldServer, res)
      else
        BoldCreateComObject(CLSID,IBoldServer,FBoldServer, res);
    end;
  except
    on Exception do FBoldServer := nil;
  end;
  if Assigned(FBoldServer) then
    Connected
  else
    ConnectFailed(res);
  Result := Assigned(FBoldServer);
end;

function TBoldComClientConnection.Reconnect(ConnectClientInterface: Boolean): Boolean;
begin
  Result := False;
  if (FConnectionState <> bceConnected) then Exit;
  FConnectClientInterface := ConnectClientInterface;
  if FConnectClientInterface then
    FBoldServer.Connect(TBoldComClient.Instance.ClientId,0,FClientInterface)
  else
    FBoldServer.Connect(TBoldComClient.Instance.ClientId,0,nil);
  Result := True;
end;

procedure TBoldComClientConnection.Connected;
begin
  try
    if FConnectClientInterface then
      FBoldServer.Connect(TBoldComClient.Instance.ClientId,0,FClientInterface)
    else
      FBoldServer.Connect(TBoldComClient.Instance.ClientId,0,nil);
  except
    on E:Exception do
    begin
      CoDisconnectObject(FClientInterface,0);
      FBoldServer := nil;
      if (E is EOleSysError) then
        ConnectFailed((E as EOleSysError).ErrorCode);
      raise;
    end;
  end;
  FConnectionState := bceConnected;
  SendEvent(bceConnected);
end;

procedure TBoldComClientConnection.ConnectFailed(const res: HResult);
begin
  FConnectionState := bceDisconnected;
  SendExtendedEvent(bceConnectFailed, [res]);
end;

procedure TBoldComClientConnection.Connecting;
begin
  FConnectionState := bceConnecting;
  SendEvent(bceConnecting);
end;

function TBoldComClientConnection.Disconnect: Boolean;
begin
  Result := True;
  if (ConnectionState = bceDisconnected) then Exit;
  if (ConnectionState <> bceConnected) then
    raise EBoldCom.Create('Connection state error.');
  try
    Disconnecting;
  finally
    Disconnected;
  end;
end;

procedure TBoldComClientConnection.Disconnected;
begin
  FConnectionState := bceDisconnected;
  SendEvent(bceDisconnected);
end;

procedure TBoldComClientConnection.Disconnecting;
begin
  FConnectionState := bceDisconnecting;
  try
    SendEvent(bceDisconnecting);
  finally
    CoDisconnectObject(FClientInterface,0);
    {wrapping FBoldServer.Disconnect in a try-except block, this is just a hack to prevent AVs on WIN NT/98/95}
    try
      FBoldServer.Disconnect;
      FBoldServer := nil;
    except
      FBoldServer := nil;
    end;
  end;
end;

{-- TBoldComClientEventQueue --------------------------------------------------}

procedure TBoldComClientEventQueue.HandleEvent(const EventData: TBoldComEventData);
var
  Subscriber: TBoldComClientsubscriber;
begin
  Subscriber := TBoldComClient.Instance.GetSubscriber(EventData.SubscriberID);
  if assigned(Subscriber) then
    TBoldProtectedAccessSubscriber(Subscriber).Receive(nil, EventData.OriginalEvent, EventData.RequestedEvent)
  else
    if EventData.OriginalEvent = beServerSubscriberRemoved then
      TBoldComClient.Instance.UnquaranteenSubscriberId(EventData.SubscriberId);
end;

{-- TBoldComClient ------------------------------------------------------------}

constructor TBoldComClient.Create;
begin
  inherited;
  FClientId := BoldCreateGUIDAsString;
  FConnections := TBoldObjectArray.Create(4,[]);
  FEventQueue := TBoldComClientEventQueue.Create(pmSingleEvent);
  fSubscribers := TBoldComClientSubscriberList.Create;
  fSubscribers.OwnsEntries := false;
  FUnusedSubscriberIds := TBoldIntegerArray.Create(0, []);
  fQuaranteenedSubscriberIds := TBoldIntegerArray.Create(0, []);
end;

destructor TBoldComClient.Destroy;
begin
  FEventQueue.Free;
  FConnections.Free;
  fSubscribers.Free;
  FUnusedSubscriberIds.Free;
  fQuaranteenedSubscriberIds.Free;

  if G_BoldComClient = Self then
    G_BoldComClient := nil;
  inherited;
end;

procedure TBoldComClient.AddConnection(Connection: TBoldComClientConnection);
begin
  FConnections.Add(Connection);
end;

procedure TBoldComClient.CancelSubscriptions(SubscriberId: Integer; Connection: TBoldComClientConnection = nil);
var
  I: Integer;
  Conn: TBoldComClientConnection;
begin
  if Assigned(Connection) and Assigned(Connection.BoldServer) and
    (Connection.ConnectionState = bceConnected) then
    Connection.BoldServer.Execute('CancelSubscriptions',SubscriberId)
  else
  begin
    for I := 0 to ConnectionCount - 1 do
    begin
      Conn := Connections[I];
      if Assigned(Conn) and Assigned(Conn.BoldServer) and
        (Conn.ConnectionState = bceConnected) then
        Conn.BoldServer.Execute('CancelSubscriptions',SubscriberId)
    end;
  end;
end;

procedure TBoldComClient.RemoveSubscriber(Subscriber: TBoldComClientSubscriber);
var
  I: Integer;
  Connection: TBoldComClientConnection;
begin
  if ConnectionCount > 0 then
  begin
    fQuaranteenedSubscriberIds.Add(Subscriber.SubscriberId);
    for I := 0 to ConnectionCount - 1 do
    begin
      Connection := Connections[I];
      if Assigned(Connection) and Assigned(Connection.BoldServer) and
        (Connection.ConnectionState = bceConnected) then
        Connection.BoldServer.Execute('RemoveSubscriber', Subscriber.SubscriberId)
    end;
  end
  else
    FUnusedSubscriberIds.Add(Subscriber.SubscriberId);
    
  fSubscribers.remove(Subscriber);
end;

function TBoldComClient.GetConnectionByIndex(Index: Integer): TBoldComClientConnection;
begin
  Result := FConnections[Index] as TBoldComClientConnection;
end;

function TBoldComClient.GetConnectionCount: Integer;
begin
  Result := FConnections.Count;
end;

class function TBoldComClient.Instance: TBoldComClient;
begin
  if not Assigned(G_BoldComClient) then
    G_BoldComClient := TBoldComClient.Create;
  Result := G_BoldComClient;
end;

procedure TBoldComClient.RemoveConnection(Connection: TBoldComClientConnection);
begin
  FConnections.Remove(Connection);
end;

function TBoldComClient.GetSubscriber(SubscriberId: integer): TBoldComClientSubscriber;
begin
  result := fSubscribers.SubscriberById[SubscriberId];
end;

{ TBoldComClientSubscriberList }

constructor TBoldComClientSubscriberList.create;
begin
  inherited;
  SetIndexCapacity(1);
  SetIndexVariable(IX_COMClientSubscriber, AddIndex(TBoldComClientSubscriberHashIndex.Create));
end;

function TBoldComClientSubscriberList.GetSubscriberById(SubscriberId: integer): TBoldComClientSubscriber;
begin
  Result := TBoldComClientSubscriberHashIndex(Indexes[IX_COMClientSubscriber]).FindSubscriberBySubscriberId(SubscriberId);
end;

procedure TBoldComClient.AddSubscriber(Subscriber: TBoldComclientSubscriber);
begin
  if FUnusedSubscriberIds.Count > 0 then
  begin
    Subscriber.fSubscriberId := FUnusedSubscriberIds[FUnusedSubscriberIds.count-1];
    FUnusedSubscriberIds.Delete(FUnusedSubscriberIds.Count-1);
  end
  else
  begin
    Subscriber.fSubscriberId := fSubscriberIDCounter;
    fSubscriberIDCounter := fSubscriberIDCounter+1;
  end;

  if fSubscriberIDCounter = MAXINT then
    fSubscriberIDCounter := 0;
  fSubscribers.Add(Subscriber)
end;

{ TBoldComClientSubscriberHashIndex }

function TBoldComClientSubscriberHashIndex.FindSubscriberBySubscriberId(SubscriberId: integer): TBoldComClientSubscriber;
begin
  result := TBoldComClientSubscriber(find(SubscriberId));
end;

function TBoldComClientSubscriberHashIndex.ItemAsKeyCardinal(Item: TObject): cardinal;
begin
  assert(Item is TBoldComClientSubscriber);
  result := TBoldComClientSubscriber(Item).SubscriberId;
end;

procedure TBoldComClient.UnquaranteenSubscriberId(SubscriberId: integer);
var
  i: integer;
begin
  for i := 0 to fQuaranteenedSubscriberIds.Count-1 do
  begin
    if fQuaranteenedSubscriberIds[i] = SubscriberId then
    begin
      FUnusedSubscriberIds.Add(fQuaranteenedSubscriberIds[i]);
      fQuaranteenedSubscriberIds.delete(i);
      break;
    end;
  end;
end;

initialization

finalization
  G_BoldComClient.Free;

end.
