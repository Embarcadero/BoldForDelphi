
{ Global compiler directives }
{$include bold.inc}

unit BoldComServer;

interface

uses
  ComObj,
  Classes,
  Windows,
  ActiveX,
  BoldContainers,
  BoldSubscription,
  BoldServer,
  BoldComEventQueue,
  BoldIndexableList,
  BoldComObj,
  BoldComConnection,
  BoldThreadedComObjectFactory
  ;

const
  // Server events
  bseConnectionAdded = 24;
  bseConnectionRemoved = 25;

type
  { forward declarations }
  TBoldComServerSubscriber = class;
  TBoldComServerEventQueue = class;
  TBoldComServerInterface = class;
  TBoldComServerProvider = class;
  TBoldComServerEventSenderThread = class;
  TBoldComServerConnection = class;
  TBoldComServerConnectionFactory = class;
  TBoldComServerThreadedConnectionFactory = class;
  TBoldComServer = class;

  TBoldComServerConnectionClass = class of TBoldComServerConnection;

  TBoldComServerConnectionEvent = procedure(Sender: TBoldComServerConnection; var Disconnect: Boolean);

  {-- TBoldComServerSubscriber --}
  TBoldComServerSubscriber = class(TBoldSubscriber)
  private
    FOwner: TBoldComServerConnection;
    FSubscriberId: Integer;
  protected
    procedure Receive(Originator: TObject; OriginalEvent: TBoldEvent;  RequestedEvent: TBoldRequestedEvent); override;
  public
    constructor Create(Owner: TBoldComServerConnection; SubscriberId: Integer);
    destructor Destroy; override;
    class function GetSubscriber(const ClientId: string; SubscriberId: Integer): TBoldComServerSubscriber;
    property SubscriberId: Integer read FSubscriberId;
  end;
  {$EXTERNALSYM TBoldComServerSubscriber}

  {-- TBoldComServerEventQueue --}
  TBoldComServerEventQueue = class(TBoldComEventQueue)
  private
    FOwner: TBoldComServerConnection;
  public
    constructor Create(Owner: TBoldComServerConnection);
    procedure Pop; override;
    function GetAllEvents: OleVariant;
  end;
  {$EXTERNALSYM TBoldComServerEventQueue}

  {-- TBoldComServerInterface --}
  TBoldComServerInterface = class(TBoldAggregatedAutoInterfacedObject, IBoldServer)
  private
    FOwner: TBoldComServerConnection;
  protected
    { IBoldServer }
    function Connect(const ClientId: WideString; Flags: Integer; const Client: IBoldClient): WordBool; safecall;
    function Disconnect: WordBool; safecall;
    function Execute(const Name: WideString; Params: OleVariant): OleVariant; safecall;
  public
    constructor Create(Owner: TBoldComServerConnection);
  end;
  {$EXTERNALSYM TBoldComServerInterface}

  {-- TBoldComServerProvider --}
  TBoldComServerProvider = class(TBoldAggregatedAutoInterfacedObject, IBoldProvider)
  private
    FOwner: TBoldComServerConnection;
  protected
    { IBoldProvider }
    function CreateObject(const ClassName: WideString): IUnknown; safecall;
    function GetObject(const ObjectName: WideString): IUnknown; safecall;
    function Get_ObjectInfo: OleVariant; safecall;
  public
    constructor Create(Owner: TBoldComServerConnection);
  end;
  {$EXTERNALSYM TBoldComServerProvider}

  TBoldComServerSubscriberList = class(TBoldUnorderedIndexableList)
  private
    function GetSubscriberById(SubscriberId: integer): TBoldComServerSubscriber;
  public
    constructor create;
    property SubscriberById[SubscriberId: integer]: TBoldComServerSubscriber read GetSubscriberById;
  end;

  TBoldComServerEventSenderThread = class(TThread)
  private
    FOwner: TBoldComServerConnection;
    FClientInterfaceCookie: Cardinal;
  protected
    procedure Execute; override;
    procedure SendEventFailure;
  public
    constructor Create(aOwner: TBoldComServerConnection; aClientInterfaceCookie: Cardinal);
    property ClientInterfaceCookie: Cardinal read FClientInterfaceCookie;
  end;

  {-- TBoldComServerConnection --}
  TBoldComServerConnection = class(TComObject)
  private
    FActive: Boolean;
    FClientId: string;
    FClientInterface: IBoldClient; //TODO WARNING! Contains InterfaceCookie
//TODO    FSenderThread: TBoldComServerEventSenderThread; //NEW
//TODO    FHasClientInterface: Boolean; //NEW//TODO    FClientInterfaceCookie: DWORD; //NEW
    FConnected: Boolean;
    FDestroying: Boolean;
    FEventQueue: TBoldComServerEventQueue;
    FInterface: TBoldComServerInterface;
    FProvider: TBoldComServerProvider;
    FSubscribers: TBoldComServerSubscriberList;
    fOnSendEventFailure: TBoldComServerConnectionEvent;
    procedure CancelSubscriptions(SubscriberId: Integer);
    function ClientConnect(const ClientId: string; Flags: Integer; const Client: IBoldClient): Boolean;
    function ClientDisconnect: Boolean;
    function Execute(const Name: string; Params: OleVariant): OleVariant;
    function GetSubscriber(SubscriberId: Integer): TBoldComServerSubscriber;
    procedure RemoveSubscriber(SubscriberId: Integer);
    procedure PushEvent(SubscriberId: integer; Originator: TObject; OriginalEvent, RequestedEvent: TBoldevent);
//TODO    function GetClientInterface: IBoldClient;
//TODO    function GetClientInterface(out aClient: IBoldClient): Boolean;
//TODO    function DoSendQueuedEvents: Boolean; //NEW
//TODO    procedure DoSendEventFailure; //NEW
    property Subscribers: TBoldComServerSubscriberList read FSubscribers;

  public
    destructor Destroy; override;
    procedure Disconnect;
    procedure Initialize; override;
    function ObjQueryInterface(const IID: TGUID; out Obj): HResult; override;
    procedure SendQueuedEvents;
    property Active: Boolean read FActive;
    property ClientId: string read FClientId;
    property ClientInterface: IBoldClient read FClientInterface; //TODO WARNING! Contains InterfaceCookie
    property Connected: Boolean read FConnected;
    property EventQueue: TBoldComServerEventQueue read FEventQueue;
    property OnSendEventFailure: TBoldComServerConnectionEvent read fOnSendEventFailure write fOnSendEventFailure;
  end;
  {$EXTERNALSYM TBoldComServerConnection}

  {-- TBoldComServerConnectionFactory --}
  TBoldComServerConnectionFactory = class(TComObjectFactory)
  public
    constructor Create(ComServer: TComServerObject; const ClassID: TGUID;
      const Name, Description: string);
    procedure UpdateRegistry(Register: Boolean); override;
  end;
  {$EXTERNALSYM TBoldComServerConnectionFactory}

  {-- TBoldComServerThreadedConnectionFactory --}
  TBoldComServerThreadedConnectionFactory = class(TBoldThreadedComObjectFactory)
  public
    constructor Create(ComServer: TComServerObject; const ClassID: TGUID;
      const Name, Description: string);
    procedure UpdateRegistry(Register: Boolean); override;
  end;
  {$EXTERNALSYM TBoldComServerThreadedConnectionFactory}

  {-- IBoldComServerObjectProvider --}
  IBoldComServerObjectProvider = interface
    ['{DCAB6586-0AD5-4DC1-9185-765DE13B5A2D}']
    function CreateObject(const ClassId: TGUID; const ClassName: string): IUnknown;
    function GetObject(const ClassId: TGUID; const ObjectName: string): IUnknown;
    procedure GetObjectInfo(const ClassId: TGUID; ObjectNames, ClassNames: TStrings);
  end;

  {-- TBoldComServer --}
  TBoldComServer = class(TBoldServer)
  private
    FObjectProviders: TBoldInterfaceArray;
    FConnections: TBoldObjectArray;
    procedure AddConnection(Connection: TBoldComServerConnection);
    function CreateObject(const ClassId: TGUID; const ClassName: string): IUnknown;
    function GetConnectionById(const ClientId: string): TBoldComServerConnection;
    function GetConnectionByIndex(Index: Integer): TBoldComServerConnection;
    function GetConnectionCount: Integer;
    function GetObject(const ClassId: TGUID; const ObjectName: string): IUnknown;
    procedure GetObjectInfo(const ClassId: TGUID; ObjectNames, ClassNames: TStrings);
    procedure RemoveConnection(Connection: TBoldComServerConnection);
    procedure SendQueuedEvents;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    class function Instance: TBoldComServer;
    function RegisterObjectProvider(const Provider: IBoldComServerObjectProvider): Integer;
    function RevokeObjectProvider(ObjectProvider: IBoldComServerObjectProvider; Cookie: Integer): Boolean;
    property ConnectionCount: Integer read GetConnectionCount;
    property Connections[Index: Integer]: TBoldComServerConnection read GetConnectionByIndex;
  end;
  {$EXTERNALSYM TBoldComServer}

var
  //Set to true to enable client callbacks in separate worker threads.
  BoldAsynchronousClientCallbackOnServerEvent: Boolean = False;

{$DEFINE BOLDCOMCALLBACKDEBUG}
{$IFDEF BOLDCOMCALLBACKDEBUG}
//Functions used for performance testing of callbacks
function CallStats: string;
procedure ClearStats;
{$ENDIF}

implementation

uses
  SysUtils,
  BoldIndex,
  BoldHashIndexes,
  BoldComUtils,
  Variants,
  BoldApartmentThread;

const
  CLSID_StdGlobalInterfaceTable: TGUID = '{00000323-0000-0000-C000-000000000046}'; //NEW (Missing in ActiveX)

type
  IGlobalInterfaceTable = interface(IUnknown) //Incorrect implementation in ActiveX in D6
    ['{00000146-0000-0000-C000-000000000046}']
    function RegisterInterfaceInGlobal(const pUnk: IUnknown; const riid: TIID;
                                       out dwCookie: DWORD): HResult; stdcall;
    function RevokeInterfaceFromGlobal(dwCookie: DWORD): HResult; stdcall;
    function GetInterfaceFromGlobal(dwCookie: DWORD; const riid: TIID;
                                    out ppv): HResult; stdcall;
  end;

  TBoldEventSenderThreadList = class(TBoldCardinalHashIndex)
  protected
    function ItemAsKeyCardinal(Item: TObject): Cardinal; override;
  public
    function FindEventSenderThreadByCookie(ClientInterfaceCookie: Cardinal): TBoldComServerEventSenderThread;
    function FindClientInterfaceByCookie(ClientInterfaceCookie: Cardinal): IBoldClient;
    procedure ConnectClient(const ServerConnection: TBoldComServerConnection; const Client: IBoldClient; var ClientInterfaceCookie: Cardinal);
    procedure DisconnectClient(var ClientInterfaceCookie: Cardinal);
  end;

var
  G_GlobalInterfaceTable: IGlobalInterfaceTable;
  G_BoldEventSenderThreadList: TBoldEventSenderThreadList;

function GlobalInterfaceTable: IGlobalInterfaceTable;
begin
  if not Assigned(G_GlobalInterfaceTable) then
    OleCheck(CoCreateInstance(CLSID_StdGlobalInterfaceTable, nil, CLSCTX_INPROC_SERVER, IGlobalInterfaceTable, G_GlobalInterfaceTable));
  Result := G_GlobalInterfaceTable;
end;

function BoldEventSenderThreadList: TBoldEventSenderThreadList;

begin
  if not Assigned(G_BoldEventSenderThreadList) then
    G_BoldEventSenderThreadList := TBoldEventSenderThreadList.Create;;
  Result := G_BoldEventSenderThreadList;
end;

{$IFDEF BOLDCOMCALLBACKDEBUG}
var
  CallCount: Integer = 0;
  CallTime: Double = 0;
  CallMax: Double = 0;
  CallMin: Double = 999;

function CallStats: string;
begin
  if CallCount>0 then
    Result := Format('Total:%.1fs Min:%.0fms Max:%.0fms Avg:%.0fms Count:%d',
                     [CallTime, CallMin*1000, CallMax*1000, CallTime/CallCount*1000, CallCount]);
end;

procedure ClearStats;
begin
  CallTime := 0;
  CallMin := 0;
  CallMax := 0;
  CallCount := 0;
end;
{$ENDIF}

var
  G_BoldComServer: TBoldComServer = nil;
  IX_COMServerSubscriber: integer = -1;

type
  TBoldComServerSubscriberHashIndex = class(TBoldCardinalHashIndex)
  protected
    function ItemAsKeyCardinal(Item: TObject): cardinal; override;
  public
    function FindSubscriberBySubscriberId(SubscriberId: integer): TBoldComServerSubscriber;
  end;

{-- TBoldComServerSubscriber --------------------------------------------------}

constructor TBoldComServerSubscriber.Create(Owner: TBoldComServerConnection; SubscriberId: Integer);
begin
  inherited Create;
  FOwner := Owner;
  FSubscriberId := SubscriberId;
  FOwner.Subscribers.Add(Self);
end;

destructor TBoldComServerSubscriber.Destroy;
begin
  FOwner.Subscribers.Remove(Self);
  inherited;
end;

procedure TBoldComServerSubscriber.Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  FOwner.PushEvent(SubscriberId, Originator, OriginalEvent, RequestedEvent);
end;

class function TBoldComServerSubscriber.GetSubscriber(const ClientId: string;
  SubscriberId: Integer): TBoldComServerSubscriber;
var
  Connection: TBoldComServerConnection;
  Subscriber: TBoldComServerSubscriber;
begin
  Connection := TBoldComServer.Instance.GetConnectionById(ClientId);
  if Assigned(Connection) and (SubscriberId <> 0) then
  begin
    Subscriber := Connection.GetSubscriber(SubscriberId);
    if not Assigned(Subscriber) then
      Subscriber := TBoldComServerSubscriber.Create(Connection,SubscriberId);
  end
  else
    Subscriber := nil;
  Result := Subscriber;
end;

{-- TBoldComServerEventQueue --------------------------------------------------}

constructor TBoldComServerEventQueue.Create(Owner: TBoldComServerConnection);
begin
  inherited Create(pmAllEvents);
  FOwner := Owner;
end;

function TBoldComServerEventQueue.GetAllEvents: OleVariant;
var
  Item: PBoldComEventQueueItem;
  DataBlocks: Integer;
  I, J: Integer;
begin
  //NOTE This method can be called from a worker thread and must be threadsafe!
  Datablocks := Count;
  if (Datablocks > 0) then
  begin
    Result := VarArrayCreate([0,(DataBlocks*4)-1],varInteger);
    J := 0;
    for I := 0 to DataBlocks-1 do
    begin
      Item := PopItem;
      Assert(Assigned(Item));
      Result[J]   := Item^.EventData.SubscriberId;
      Result[J+1] := Item^.EventData.Originator;
      Result[J+2] := Item^.EventData.OriginalEvent;
      Result[J+3] := Item^.EventData.RequestedEvent;
      Inc(J,4);
      FreeMem(Item, sizeof(TBoldComEventQueueItem));
    end;
  end
  else
    Result := Unassigned;
end;

procedure TBoldComServerEventQueue.Pop;
begin
  TBoldComServer.Instance.SendQueuedEvents;
end;

{-- TBoldComServerInterface ---------------------------------------------------}

constructor TBoldComServerInterface.Create(Owner: TBoldComServerConnection);
begin
  inherited Create(Owner, BoldComConnectionTypeLibrary, IBoldServer);
  FOwner := Owner;
end;

{ IBoldServer }

function TBoldComServerInterface.Connect(const ClientId: WideString;
  Flags: Integer; const Client: IBoldClient): WordBool;
begin
  Result := FOwner.ClientConnect(ClientId,Flags,CLient);
end;

function TBoldComServerInterface.Disconnect: WordBool;
begin
  Result := FOwner.ClientDisconnect;
end;

function TBoldComServerInterface.Execute(const Name: WideString;
  Params: OleVariant): OleVariant;
begin
  Result := FOwner.Execute(Name,Params);
end;

{-- TBoldComServerProvider ----------------------------------------------------}

constructor TBoldComServerProvider.Create(Owner: TBoldComServerConnection);
begin
  inherited Create(Owner, BoldComConnectionTypeLibrary, IBoldProvider);
  FOwner := Owner;
end;

{ IBoldProvider }

function TBoldComServerProvider.CreateObject(const ClassName: WideString): IUnknown;
begin
  Result := TBoldComServer.Instance.CreateObject(FOwner.Factory.ClassID,ClassName);
end;

function TBoldComServerProvider.GetObject(const ObjectName: WideString): IUnknown;
begin
  Result := TBoldComServer.Instance.GetObject(FOwner.Factory.ClassID,ObjectName);
end;

function TBoldComServerProvider.Get_ObjectInfo: OleVariant;
var
  ObjNames,ClsNames: TStringList;
begin
  ObjNames := TStringList.Create;
  ClsNames := TStringList.Create;
  try
    TBoldComServer.Instance.GetObjectInfo(FOwner.Factory.ClassID,ObjNames,ClsNames);
    Result := VarArrayCreate([0,1],varVariant);
    Result[0] := BoldStringsToVariant(ObjNames);
    Result[1] := BoldStringsToVariant(ClsNames);
  finally
    ClsNames.Free;
    ObjNames.Free;
  end;
end;

{-- TBoldComServerConnection --------------------------------------------------}

destructor TBoldComServerConnection.Destroy;
begin
  if FDestroying then Exit;
  FDestroying := True;
  FConnected := False;
  FActive := False;
  TBoldComServer.Instance.RemoveConnection(Self);
  BoldEventSenderThreadList.DisconnectClient(Cardinal(FClientInterface));
//  if Assigned(FSenderThread) then
//  begin
//    FSenderThread.Terminate;
//    FSenderThread.FOwner := nil;
//    FSenderThread.Resume;
//  end;
//  if Assigned(FClientInterface) then
//  begin
//    GlobalInterfaceTable.RevokeInterfaceFromGlobal(FClientInterfaceCookie);
//    FClientInterfaceCookie := 0;
//  end;
  FClientInterface := nil;
  FClientId := '';
  RemoveSubscriber(-1);
  FSubscribers.Free;
  FProvider.Free;
  FInterface.Free;
  FEventQueue.Free;
  inherited Destroy;
end;

procedure TBoldComServerConnection.Initialize;
begin
  inherited;
  FEventQueue := TBoldComServerEventQueue.Create(Self);
  FInterface := TBoldComServerInterface.Create(Self);
  FProvider := TBoldComServerProvider.Create(Self);
  FSubscribers := TBoldComServerSubscriberList.Create;
  fSubscribers.OwnsEntries := false;
  TBoldComServer.Instance.AddConnection(Self);
  FActive := True;
end;

function TBoldComServerConnection.ObjQueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if Active then
  begin
    Result := inherited ObjQueryInterface(IID,Obj);
    if Result <> 0 then
    begin
      if FProvider.GetInterface(IID,Obj) then
        Result := S_OK
      else if FInterface.GetInterface(IID,Obj) then
        Result := S_OK;
    end;
  end
  else
  begin
    Pointer(Obj) := nil;
    Result := E_FAIL;
  end;
end;

procedure {TBoldComServerConnection.}DoSendEventFailure(Self: TBoldComServerConnection);
var
  AskDisconnect: Boolean;
begin
  with Self do
  begin
    AskDisconnect := True;
    if Assigned(FOnSendEventFailure) then
      OnSendEventFailure(Self, AskDisconnect);
    if AskDisconnect then
      ClientDisconnect;
  end;
end;

function {TBoldComServerConnection.}DoSendQueuedEvents(Self: TBoldComServerConnection): Boolean;
var
  Data: OleVariant;
  vClient: IBoldClient;
{$IFDEF BOLDCOMCALLBACKDEBUG}
  CounterStart, CounterStop, CounterFreq: Int64; //TEST
  ElapsedTime: Double; //TEST
{$ENDIF}
begin
  //NOTE This method can be called from a worker thread and must be threadsafe!
  Result := False;
  if Self.Connected and (Cardinal(Self.FClientInterface)<>0) then
  begin
    Data := Self.FEventQueue.GetAllEvents;
    if not VarIsEmpty(Data) then
    begin

      vClient := BoldEventSenderThreadList.FindClientInterfaceByCookie(Cardinal(Self.FClientInterface));
      if Assigned(vClient) then
      begin
{$IFDEF BOLDCOMCALLBACKDEBUG}
        QueryPerformanceCounter(CounterStart);
{$ENDIF}

        vClient.OnServerEvent(EVENT_SUBSCRIPTION,Data);

{$IFDEF BOLDCOMCALLBACKDEBUG}
        QueryPerformanceCounter(CounterStop); 
        QueryPerformanceFrequency(CounterFreq);
        ElapsedTime := ((CounterStop-CounterStart)/CounterFreq);
        CallTime := CallTime+ElapsedTime;
        if ElapsedTime<CallMin then
          CallMin := ElapsedTime;
        if ElapsedTime>CallMax then
          CallMax := ElapsedTime;
        InterlockedIncrement(CallCount);
{$ENDIF}

      end;
      Result := True;
    end;
  end
  else
    Self.FEventQueue.Clear;
end;


procedure TBoldComServerConnection.SendQueuedEvents;
var
  vSenderThread: TBoldComServerEventSenderThread;
begin
  if FEventQueue.Count>0 then
  begin
    if BoldAsynchronousClientCallbackOnServerEvent then
    begin
      vSenderThread := BoldEventSenderThreadList.FindEventSenderThreadByCookie(Cardinal(FClientInterface));
      if Assigned(vSenderThread) then
        vSenderThread.Resume;
    end
    else
    begin
      try
        DoSendQueuedEvents(Self);
      except
        DoSendEventFailure(Self);
      end;
    end;
  end;
end;

procedure TBoldComServerConnection.CancelSubscriptions(SubscriberId: Integer);
var
  Subscriber: TBoldComServerSubscriber;
  Traverser: TBoldIndexableListTraverser;
begin
  if SubscriberId < 0 then
  begin
    // cancel all subscriptions...
    Traverser := Subscribers.CreateTraverser;
    try
      while Traverser.MoveNext do
      begin
        Subscriber := TBoldComServerSubscriber(Traverser.item);
        Subscriber.CancelAllSubscriptions;
      end;
    finally
      Traverser.Free;
    end;
  end
  else
  begin
    Subscriber := Subscribers.SubscriberById[SubscriberId];
    if assigned(Subscriber) then
      Subscriber.CancelAllSubscriptions;
  end;
end;

procedure TBoldComServerConnection.RemoveSubscriber(SubscriberId: Integer);
var
  Subscriber: TBoldComServerSubscriber;
  TempSubscriberId: Integer;
begin
  if SubscriberId < 0 then
  begin
    // remove all subscribers
    while Subscribers.Count > 0 do
    begin
      Subscriber := Subscribers.Any as TBoldComServerSubscriber;
      TempSubscriberId := Subscriber.SubscriberId;
      Subscriber.Free;
      PushEvent(TempSubscriberId, nil, beServerSubscriberRemoved, beServerSubscriberRemoved);
    end;
  end
  else
  begin
    Subscriber := GetSubscriber(SubscriberId);
    if Assigned(Subscriber) then
      Subscriber.Free;
    PushEvent(SubscriberId, nil, beServerSubscriberRemoved, beServerSubscriberRemoved);
  end;
end;

procedure TBoldComServerConnection.PushEvent(SubscriberId: integer; Originator: TObject; OriginalEvent, RequestedEvent: TBoldevent);
var
  EventData: TBoldComEventData;
begin
  EventData.SubscriberId := SubscriberId;
  EventData.Originator := Integer(Originator);
  EventData.OriginalEvent := OriginalEvent;
  EventData.RequestedEvent := RequestedEvent;
  EventQueue.Push(EventData);
end;


function TBoldComServerConnection.ClientConnect(const ClientId: string;
  Flags: Integer; const Client: IBoldClient): Boolean;
var
  C: TBoldComServerConnection;
begin
  C := TBoldComServer.Instance.GetConnectionById(ClientId);
  if Active and (not Assigned(C) or (C = Self)) then
  begin
    FClientId := ClientId;
    BoldEventSenderThreadList.ConnectClient(Self, Client, Cardinal(FClientInterface)); //NEW
    FConnected := True;
    Result := True;
  end
  else
    Result := False;
end;

function TBoldComServerConnection.ClientDisconnect: Boolean;
begin
  if Connected then
  begin
    FConnected := False;
    RemoveSubscriber(-1);
    FEventQueue.Clear;
    BoldEventSenderThreadList.DisconnectClient(Cardinal(FClientInterface));
//    if Assigned(FClientInterface) then
//    begin
//      GlobalInterfaceTable.RevokeInterfaceFromGlobal(FClientInterfaceCookie);
//      FClientInterfaceCookie := 0;
//    end;
    FClientInterface := nil;
    FClientId := '';
    Result := True;
  end
  else
    Result := False;
end;

procedure TBoldComServerConnection.Disconnect;
begin
  // disconnect all objects
  CoDisconnectObject(self, 0);
end;

function TBoldComServerConnection.Execute(const Name: string; Params: OleVariant): OleVariant;
begin
  if CompareText(Name,'CancelSubscriptions') = 0 then
    CancelSubscriptions(Params)
  else if CompareText(Name,'RemoveSubscriber') = 0 then
    RemoveSubscriber(Params);
end;

function TBoldComServerConnection.GetSubscriber(SubscriberId: Integer): TBoldComServerSubscriber;
begin
  Result := Subscribers.SubscriberById[SubscriberId];
end;

{-- TBoldServerConnectionFactory ----------------------------------------------}

constructor TBoldComServerConnectionFactory.Create(ComServer: TComServerObject;
  const ClassID: TGUID; const Name, Description: string);
begin
  inherited Create(ComServer, TBoldComServerConnection,
    ClassID, Name, Description, ciMultiInstance);
end;

procedure TBoldComServerConnectionFactory.UpdateRegistry(Register: Boolean);
var
  Unk: IUnknown;
  Reg: ICatRegister;
  Inf: ICatInformation;
  CatDesc: PWideChar;
  CatInfo: TCategoryInfo;
begin
  inherited;
  Unk := ComObj.CreateComObject(CLSID_StdComponentCategoryMgr);
  if Assigned(Unk) then
  begin
    Unk.QueryInterface(ICatRegister,Reg);
    Unk.QueryInterface(ICatInformation,Inf);
    if not Assigned(Reg) or not Assigned(Inf) then Exit;
  end;
  if Register then
  begin
    if Inf.GetCategoryDesc(CATID_BoldServer,409,CatDesc) <> S_OK then
    begin
      // create the category
      CatInfo.catid := CATID_BoldServer;
      CatInfo.lcid := 409;
      StringToWideChar('BoldServer Classes',CatInfo.szDescription,127);
      Reg.RegisterCategories(1,@CatInfo);
    end;
    if Assigned(CatDesc) then
      CoTaskMemFree(CatDesc);
    // register class
    Reg.RegisterClassImplCategories(ClassID,1,@CATID_BoldServer);
  end
  else
  begin
    // unregister class
    Reg.UnRegisterClassImplCategories(ClassID,1,@CATID_BoldServer);
  end;
end;

{-- TBoldComServer ------------------------------------------------------------}

constructor TBoldComServer.Create;
begin
  inherited Create;
  FConnections := TBoldObjectArray.Create(4,[]);
  FObjectProviders := TBoldInterfaceArray.Create(4,[bcoDataOwner]);
end;

destructor TBoldComServer.Destroy;
begin
  FreeAndNil(FObjectProviders);
  FreeAndNil(FConnections);
  if G_BoldComServer = Self then
    G_BoldComServer := nil;
  inherited;
end;

procedure TBoldComServer.AddConnection(Connection: TBoldComServerConnection);
begin
  FConnections.Add(Connection);
end;

function TBoldComServer.CreateObject(const ClassId: TGUID; const ClassName: string): IUnknown;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FObjectProviders.Count - 1 do
  begin
    Result := (FObjectProviders[I] as IBoldComServerObjectProvider).CreateObject(ClassId,ClassName);
    if Assigned(Result) then Break;
  end;
end;

function TBoldComServer.GetObject(const ClassId: TGUID; const ObjectName: string): IUnknown;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FObjectProviders.Count - 1 do
  begin
    Result := (FObjectProviders[I] as IBoldComServerObjectProvider).GetObject(ClassId,ObjectName);
    if Assigned(Result) then Break;
  end;
end;

procedure TBoldComServer.GetObjectInfo(const ClassId: TGUID; ObjectNames, ClassNames: TStrings);
var
  I: Integer;
begin
  for I := 0 to FObjectProviders.Count - 1 do
    (FObjectProviders[I] as IBoldComServerObjectProvider).GetObjectInfo(
      ClassId,ObjectNames,ClassNames);
end;

function TBoldComServer.GetConnectionById(const ClientId: string): TBoldComServerConnection;
var
  Connection: TBoldComServerConnection;
  I: Integer;
begin
  Result := nil;
  for I := 0 to FConnections.Count - 1 do
  begin
    Connection := TBoldComServerConnection(FConnections[I]);
    if AnsiCompareText(Connection.ClientId,ClientId) = 0 then
    begin
      Result := Connection;
      Break;
    end;
  end;
end;

function TBoldComServer.GetConnectionByIndex(Index: Integer): TBoldComServerConnection;
begin
  Result := FConnections[Index] as TBoldComServerConnection;
end;

function TBoldComServer.GetConnectionCount: Integer;
begin
  Result := FConnections.Count;
end;

class function TBoldComServer.Instance: TBoldComServer;
begin
  if not Assigned(G_BoldComServer) then
    G_BoldComServer := TBoldComServer.Create;
  Result := G_BoldComServer;
end;

function TBoldComServer.RegisterObjectProvider(const Provider: IBoldComServerObjectProvider): Integer;
begin
  Result := FObjectProviders.Add(Provider);
end;

procedure TBoldComServer.RemoveConnection(Connection: TBoldComServerConnection);
begin
  FConnections.Remove(Connection);
end;

function TBoldComServer.RevokeObjectProvider(ObjectProvider: IBoldComServerObjectProvider; Cookie: Integer): Boolean;
begin
  FObjectProviders.Remove(ObjectProvider);
  Result := True;
end;

procedure TBoldComServer.SendQueuedEvents;
var
  I: Integer;
begin
  I := 0;
  while (I<FConnections.Count) do
  begin
    TBoldComServerConnection(FConnections[I]).SendQueuedEvents;
    Inc(I);
  end;
end;

{ TBoldComServerThreadedConnectionFactory }

constructor TBoldComServerThreadedConnectionFactory.Create(
  ComServer: TComServerObject; const ClassID: TGUID; const Name,
  Description: string);
begin
  inherited Create(ComServer, TBoldComServerConnection,
    ClassID, Name, Description, ciMultiInstance, batSTA);
end;

procedure TBoldComServerThreadedConnectionFactory.UpdateRegistry(
  Register: Boolean);
var
  Unk: IUnknown;
  Reg: ICatRegister;
  Inf: ICatInformation;
  CatDesc: PWideChar;
  CatInfo: TCategoryInfo;
begin
  inherited;
  Unk := ComObj.CreateComObject(CLSID_StdComponentCategoryMgr);
  if Assigned(Unk) then
  begin
    Unk.QueryInterface(ICatRegister,Reg);
    Unk.QueryInterface(ICatInformation,Inf);
    if not Assigned(Reg) or not Assigned(Inf) then Exit;
  end;
  if Register then
  begin
    if Inf.GetCategoryDesc(CATID_BoldServer,409,CatDesc) <> S_OK then
    begin
      // create the category
      CatInfo.catid := CATID_BoldServer;
      CatInfo.lcid := 409;
      StringToWideChar('BoldServer Classes',CatInfo.szDescription,127);
      Reg.RegisterCategories(1,@CatInfo);
    end;
    if Assigned(CatDesc) then
      CoTaskMemFree(CatDesc);
    // register class
    Reg.RegisterClassImplCategories(ClassID,1,@CATID_BoldServer);
  end
  else
  begin
    // unregister class
    Reg.UnRegisterClassImplCategories(ClassID,1,@CATID_BoldServer);
  end;
end;

{ TBoldComServerSubscriberList }

constructor TBoldComServerSubscriberList.create;
begin
  inherited;
  SetIndexCapacity(1);
  SetIndexVariable(IX_COMServerSubscriber, AddIndex(TBoldComServerSubscriberHashIndex.Create));
end;

function TBoldComServerSubscriberList.GetSubscriberById(SubscriberId: integer): TBoldComServerSubscriber;
begin
  Result := TBoldComServerSubscriberHashIndex(Indexes[IX_COMServerSubscriber]).FindSubscriberBySubscriberId(SubscriberId);
end;

{ TBoldComServerSubscriberHashIndex }

function TBoldComServerSubscriberHashIndex.FindSubscriberBySubscriberId(SubscriberId: integer): TBoldComServerSubscriber;
begin
  result := TBoldComServerSubscriber(find(SubscriberId));
  assert(not assigned(Result) or (result is TBoldComServerSubscriber));
end;

function TBoldComServerSubscriberHashIndex.ItemAsKeyCardinal(Item: TObject): cardinal;
begin
  Assert(Item is TBoldComServerSubscriber);
  Result := TBoldComServerSubscriber(Item).SubscriberId;
end;

//NEW Class
{ TBoldComServerEventSenderThread }

constructor TBoldComServerEventSenderThread.Create(aOwner: TBoldComServerConnection; aClientInterfaceCookie: Cardinal);
begin
  FOwner := aOwner;
  FClientInterfaceCookie := aClientInterfaceCookie;
  FreeOnTerminate := True;
  inherited Create(True);
end;

procedure TBoldComServerEventSenderThread.SendEventFailure;
begin
  if not Terminated then
    DoSendEventFailure(FOwner);
end;

procedure TBoldComServerEventSenderThread.Execute;
begin
  CoInitialize(nil);
  repeat
    try
      while (not Terminated) and {FOwner.}DoSendQueuedEvents(FOwner) do {loop};
    except
      if not Terminated then
      Synchronize(SendEventFailure);
    end;
    if not Terminated then
      Suspend;
  until Terminated;
  CoUninitialize;
end;

{ TBoldEventSenderThreadList }
procedure TBoldEventSenderThreadList.ConnectClient(const ServerConnection: TBoldComServerConnection; const Client: IBoldClient; var ClientInterfaceCookie: Cardinal);
var
  vSenderThread: TBoldComServerEventSenderThread;
begin
  if ClientInterfaceCookie<>0 then
    DisconnectClient(ClientInterfaceCookie);
  if Assigned(Client) then
  begin
    OleCheck(GlobalInterfaceTable.RegisterInterfaceInGlobal(Client, IID_IBoldClient, ClientInterfaceCookie));
    vSenderThread := TBoldComServerEventSenderThread.Create(ServerConnection, ClientInterfaceCookie);
    Add(vSenderThread);
  end;
end;

procedure TBoldEventSenderThreadList.DisconnectClient(var ClientInterfaceCookie: Cardinal);
var
  vSenderThread: TBoldComServerEventSenderThread;
begin
  if ClientInterfaceCookie<>0 then
  begin
    vSenderThread := FindEventSenderThreadByCookie(ClientInterfaceCookie);
    if Assigned(vSenderThread) then
    begin
      Remove(vSenderThread);
      vSenderThread.Terminate;
      vSenderThread.FOwner := nil;
      vSenderThread.FClientInterfaceCookie := 0;
      vSenderThread.Resume;
    end;
    OleCheck(GlobalInterfaceTable.RevokeInterfaceFromGlobal(ClientInterfaceCookie));
    ClientInterfaceCookie := 0;
  end;
end;

function TBoldEventSenderThreadList.FindClientInterfaceByCookie(ClientInterfaceCookie: Cardinal): IBoldClient;
begin
  Result := nil;
  if ClientInterfaceCookie<>0 then
    OleCheck(GlobalInterfaceTable.GetInterfaceFromGlobal(ClientInterfaceCookie, IID_IBoldClient, Result));
end;

function TBoldEventSenderThreadList.FindEventSenderThreadByCookie(ClientInterfaceCookie: Cardinal): TBoldComServerEventSenderThread;
begin
  Result := TBoldComServerEventSenderThread(Find(ClientInterfaceCookie));
  Assert(not Assigned(Result) or (Result is TBoldComServerEventSenderThread));
end;

function TBoldEventSenderThreadList.ItemAsKeyCardinal(Item: TObject): Cardinal;
begin
  Assert(Item is TBoldComServerEventSenderThread);
  Result := TBoldComServerEventSenderThread(Item).ClientInterfaceCookie;
end;

initialization

finalization
  FreeAndNil(G_BoldComServer); //CHANGED
  FreeAndNil(G_BoldEventSenderThreadList); //NEW
end.

