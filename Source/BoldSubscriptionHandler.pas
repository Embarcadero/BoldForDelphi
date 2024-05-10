
{ Global compiler directives }
{$include bold.inc}
unit BoldSubscriptionHandler;

interface

uses
  Windows,
  Classes,
  BoldGuard,
  BoldPropagatorsubscriptions,
  BoldDefs,
  contnrs,
  BoldThreadSafeQueue,
  BoldEnqueuer,
  BoldAbstractOutputQueueHandler,
  BoldThread;

type
  {forward declarations}
  TBoldSubscriptionHandler = class;
  TBoldDequeuer = class;

  TClientInfoChangedEvent =  procedure (const ClientId: TBoldClientId; const EventsCount, QueuesCount: integer) of object;
  TGlobalInfoChangedEvent =  procedure (const InQCount, AddedEventsCount, SentEventsCount: integer) of object;

  TBoldDequeuer = class(TBoldNotifiableThread)
  private
    fSubscriptionHandler: TBoldSubscriptionHandler;
    fInQueue: TBoldThreadSafeObjectQueue;
    fOutQueueHandler: TBoldAbstractOutputQueueHandler;
    fOnDoneDequeue: TNotifyEvent;
    fOnStartDequeue: TNotifyEvent;
    fOnClientInfoChanged: TClientInfoChangedEvent;
    fOnGlobalInfoChanged: TGlobalInfoChangedEvent;
    fInQueueCountLast: integer;
    procedure NotifyQueueNotEmpty(Queue:  TBoldThreadSafeQueue);
    procedure DoStartDequeue;
    procedure DoDoneDequeue;
    procedure Dequeue;
    function GetInQueueCount: integer;
    function GetInQueueCountPeak: integer;
  protected
    function GetOutQueueHandler: TBoldAbstractOutputQueueHandler; virtual;
    property OutQueuehandler: TBoldAbstractOutputQueueHandler read GetOutQueueHandler;
  public
    constructor Create(InQueue: TBoldThreadSafeObjectQueue);
    destructor Destroy; override;
    procedure Execute; override;
    procedure EnqueueRemoveClientQueueEvent(const ClientID: TBoldClientId);
    property OnStartDequeue: TNotifyEvent read fOnStartDequeue write fOnStartDequeue;
    property OnDoneDequeue: TNotifyEvent read fOnDoneDequeue write fOnDoneDequeue;
    property OnClientInfoChanged: TClientInfoChangedEvent read fOnClientInfoChanged write fOnClientInfoChanged;
    property OnGlobalInfoChanged: TGlobalInfoChangedEvent read fOnGlobalInfoChanged write fOnGlobalInfoChanged;
    property InQueueCount: integer read GetInQueueCount;
    property InQueueCountPeak: integer read GetInQueueCountPeak;
    property InQueueCountLast: integer read fInQueueCountLast;
  end;

  TBoldSubscriptionHandler = class
  private
    fSubscriptions: TBoldSubscriptionList;
    fOutQueueHandler: TBoldAbstractOutputQueueHandler;
    fSentEventsCount: integer;
    fAddedSubscriptionsCount: integer;
    function getOutQueueHandler: TBoldAbstractOutputQueueHandler;
    procedure AddSubscription(BoldClientId: TBoldClientID; EventName: string);
    procedure RemoveSubscription(EventName: string);
    procedure SendEvent(EventName: string; ClientIDs: TList);
    function GetAddedSubscriptionsCount: integer;
    function GetSentEventsCount: integer;
  protected
    property OutQueueHandler: TBoldAbstractOutputQueueHandler read getOutQueueHandler write fOutQueueHandler;
    procedure GetClientsSubscribedToEvent(const EventName: string; ClientIds: TList);
    procedure GetEventsByClientID(const ClientID: TBoldClientID; const events: TStringList);
    procedure RemoveQueueForClient(const ClientId: TBoldClientId);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ProcessExternalEvent(BoldExternalEvent: TBoldExternalEvent);
    procedure RemoveClient(BoldClientID: TBoldClientID);
    function GetEventCountByClientID(ClientId: TBoldClientId): integer;
    function QueueCountForClient(ClientId: TBoldClientId): integer;
    property AddedSubscriptionsCount: integer read GetAddedSubscriptionsCount;
    property SentEventsCount: integer read GetSentEventsCount;
  end;

implementation

uses
  SysUtils,

  BoldCoreConsts,
  Boldutils,
  BoldOutputQueueHandler,
  BoldPropagatorConstants,
  BoldThreadSafeLog;

  {TBoldDequeuer}
constructor TBoldDequeuer.Create(InQueue: TBoldThreadSafeObjectQueue);
begin
  inherited Create(True);
  self.fInQueue := InQueue;
  InQueue.OnQueueNotEmpty := NotifyQueueNotEmpty;
  fSubscriptionHandler := TBoldSubscriptionHandler.Create;
  fSubscriptionHandler.OutQueueHandler := OutQueueHandler;
end;

procedure TboldDequeuer.Dequeue;
var
  Events: TObjectList;
  Guard: IBoldGuard;
  i: integer;
  event: TBoldExternalEvent;
begin
  Guard := TBoldGuard.Create(Events);
  Events := TObjectList.Create(false);
  try
    fInQueueCountLast := FInQueue.Count;
    while not FInQueue.Empty do
    begin
      fInQueue.DequeueList(events, 50);
      for i := 0 to Events.Count-1 do
      begin
        event := TBoldExternalEvent(Events[i]);
        try
          FSubscriptionHandler.ProcessExternalEvent(event);
        finally
          FreeAndNil(event);
        end;
      end;
      Events.Clear;
    end;
  except on E: EOutOfMemory do
    BoldLogError(sLogError, [ClassName, 'Dequeue', E.Message]); // do not localize
  end;
end;

destructor TBoldDequeuer.Destroy;
begin
  FreeAndNil(fSubscriptionHandler);
  FreeAndNil(fOutQueueHandler);
  inherited;
end;

procedure TBoldDequeuer.DoDoneDequeue;
begin
  if Assigned(fOnDoneDequeue) then
    OnDoneDequeue(self);
end;

procedure TBoldDequeuer.DoStartDequeue;
begin
  if Assigned(fOnStartDequeue) then
    OnStartDequeue(self);
end;

procedure TBoldDequeuer.Execute;
var
  res: integer;
  rMsg: TMsg;
  ClientSubscriptions: integer;
  ClientId: TBoldClientId;
begin
  EnsureMessageQueue;
  SignalReady;
  BoldLogThread('ID=Dequeuer');

  while not Terminated do
  begin
    res := Integer(getMessage(rMsg, 0, 0, 0));
    try
      if res = -1 then
        Terminate
      else if res = 0 then
        Terminate
      else if rMsg.message = BM_QUEUE_NOT_EMPTY then
      begin
        DoStartDequeue;
        Dequeue;
        DoDoneDequeue;
      end
      else if rMsg.message = BM_REQUEST_GLOBAL_INFO then
      begin
        if Assigned(fOnGlobalInfoChanged) then
          OnGlobalInfoChanged(FInQueue.Count, FSubscriptionHandler.AddedSubscriptionsCount,
              FSubscriptionHandler.SentEventsCount);
      end
      else if rMsg.Message = BM_REQUEST_CLIENT_INFO then
      begin
        if Assigned(fOnClientInfoChanged) then
        begin
          ClientId := rMsg.lParam;
          if (rMsg.wParam <> 0) then
            ClientSubscriptions := fSubscriptionHandler.GetEventCountByClientID(ClientId)
          else
            ClientSubscriptions := -1;
          OnClientInfoChanged(ClientId, ClientSubscriptions, FSubscriptionHandler.QueueCountForClient(ClientID));
        end;
      end
      else
        DispatchMessage(rMsg);
    except
      on e: Exception do
      begin
        if (res=0) or (res=-1) then
          Windows.Beep(440, 500)
        else if (rMsg.Message = BM_REQUEST_CLIENT_INFO) or (rMsg.MEssage = BM_REQUEST_GLOBAL_INFO) then
        begin
          Windows.Beep(440*3, 500);
        end else if (rMsg.Message = BM_QUEUE_NOT_EMPTY) then
        begin
          Windows.Beep(440*3, 900);
          Windows.Beep(440*3, 900);
        end else if (rMsg.MEssage = BM_REMOVE_CLIENT_QUEUE) then
        begin
          Windows.Beep(440, 900);
          Windows.Beep(440, 900);
        end;
      end;
    end;
  end;
end;

function TBoldDequeuer.GetInQueueCount: integer;
begin
  result := fInQueue.Count;
end;

function TBoldDequeuer.GetOutQueueHandler: TBoldAbstractOutputQueueHandler;
begin
  if not Assigned(fOutQueueHandler) then
    fOutQueueHandler := TBoldOutputQueueHandler.Create;
  Result := fOutQueueHandler;
end;

procedure TBoldDequeuer.NotifyQueueNotEmpty(Queue: TBoldThreadSafeQueue);
begin
  try
    self.Notify(BM_QUEUE_NOT_EMPTY);
  except on E: Exception do
    BoldLogError(sLogError, [ClassName, 'NotifyQueueNotEmpty', E.Message]); // do not localize
  end;
end;


procedure TBoldDequeuer.EnqueueRemoveClientQueueEvent(const ClientID: TBoldClientId);
begin
  fInQueue.Enqueue(TBoldExternalEvent.Create(ClientId, bemRemoveClientQueue, ''));
end;

function TBoldDequeuer.GetInQueueCountPeak: integer;
begin
  result := fInQueue.MaxCount;
end;

{ TBoldSubscriptionHandler }

procedure TBoldSubscriptionHandler.AddSubscription(
  BoldClientId: TBoldClientID; EventName: string);
begin
  FSubscriptions.AddSubscription(BoldClientID, EventName);
  inc(fAddedSubscriptionsCount);
end;

constructor TBoldSubscriptionHandler.Create;
begin
  inherited Create;
  fSubscriptions := TBoldSubscriptionList.Create;
end;

destructor TBoldSubscriptionHandler.Destroy;
begin
  FreeAndNil(fSubscriptions);
  inherited;
end;

function TBoldSubscriptionHandler.GetAddedSubscriptionsCount: integer;
begin
  result := fAddedSubscriptionsCount;
end;

procedure TBoldSubscriptionHandler.GetClientsSubscribedToEvent(
  const EventName: string; ClientIds: TList);
begin
  fSubscriptions.GetClientsSubscribedToEvent(EventName, ClientIds);
end;

function TBoldSubscriptionHandler.GetEventCountByClientID(
  ClientId: TBoldClientId): integer;
begin
  Result := fSubscriptions.GetEventCountByClientID(ClientId);
end;

procedure TBoldSubscriptionHandler.GetEventsByClientID(
  const ClientID: TBoldClientID; const events: TStringList);
begin
  fSubscriptions.GetEventsByClientID(ClientId, Events);
end;

function TBoldSubscriptionHandler.getOutQueueHandler: TBoldAbstractOutputQueueHandler;
begin
  if Assigned(fOutQueueHandler) then
    Result := fOutQueueHandler
  else
    raise EBold.CreateFmt('%s.getQueueHandler: AbstractOutputQueueHandler is not assigned.', [ClassName]);
end;

function TBoldSubscriptionHandler.GetSentEventsCount: integer;
begin
  Result := fSentEventsCount;
end;

procedure TBoldSubscriptionHandler.ProcessExternalEvent(
  BoldExternalEvent: TBoldExternalEvent);
var
  Clients: TBoldSortedIntegerList;
  EventNameWithoutParameters: string;
  p: integer;
begin
  try
    EventNameWithoutParameters := BoldExternalEvent.EventName;
    p := pos(PROPAGATOR_PARAMETER_DELIMITER_CHAR, EventNameWithoutParameters);
    if p <> -1 then
      delete(EventNameWithoutParameters, p, maxint);

    case BoldExternalEvent.EventType of
      bemEvent: begin
        Clients := TBoldSortedIntegerList.Create;
        try
          FSubscriptions.GetAllSubscribedClientsExcept(EventNameWithoutParameters,
            BoldExternalEvent.BoldClientID, Clients);
          SendEvent(BoldExternalEvent.EventName, Clients);
        finally
          FreeAndNil(Clients);
        end;
        FSubscriptions.RemoveEvent(EventNameWithoutParameters);
        FSubscriptions.AddSubscription(BoldExternalEvent.BoldClientID, EventNameWithoutParameters);
      end;
      bemSubscription: begin
        AddSubscription(BoldExternalEvent.BoldClientID, EventNameWithoutParameters);
      end;
      bemCancelSubscription: begin
        RemoveSubscription(EventNameWithoutParameters);
      end;
      bemRemoveClientQueue: begin
        RemoveQueueForClient(BoldExternalEvent.BoldClientId);
      end;
      bemGetLock: begin
        OutQueueHandler.SendEventAndFlushQueue(BoldExternalEvent.BoldClientID, BoldExternalEvent.EventName);
      end;
      bemLockLost: begin
        Clients := TBoldSortedIntegerList.Create;
        try
          Clients.Add(BoldExternalEvent.BoldClientId);
          SendEvent(BoldExternalEvent.EventName, Clients);
        finally
          FreeAndNil(Clients);
        end;
      end;
    end;
  except on E: Exception do
    BoldLogError(sLogError, [ClassName, 'ProcessExternalEvent', E.Message]); // do not localize
  end;
end;

function TBoldSubscriptionHandler.QueueCountForClient(
  ClientId: TBoldClientId): integer;
begin
  Result := OutQueueHandler.QueueCountForClient(ClientId);
end;

procedure TBoldSubscriptionHandler.RemoveClient(
  BoldClientID: TBoldClientID);
begin
  FSubscriptions.RemoveClient(BoldClientID);
  OutQueueHandler.ClearQueueForClient(BoldClientID);
end;

procedure TBoldSubscriptionHandler.RemoveQueueForClient(
  const ClientId: TBoldClientId);
begin
  FOutQueueHandler.ClearQueueForClient(ClientId);
  FSubscriptions.RemoveClient(ClientId);
end;

procedure TBoldSubscriptionHandler.RemoveSubscription(EventName: string);
begin
  FSubscriptions.RemoveEvent(EventName);
end;

procedure TBoldSubscriptionHandler.SendEvent(EventName: string;
  ClientIDs: TList);
var
  i: integer;
begin
  if Assigned(ClientIDs) then
  begin
    for i:= 0 to ClientIDs.Count - 1 do
      OutQueueHandler.SendEvent(TBoldClientID(ClientIDs[i]), EventName);
    inc(fSentEventsCount, ClientIDs.Count);
  end;
end;

end.
