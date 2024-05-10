unit maanSubscriptionHandler;

interface

uses
  TestSuite,
  BoldAbstractOutputQueueHandler,
  BoldThreadSafeQueue,
  BoldDefs,
  BoldSubscriptionHandler,
  classes,
  TestFramework;

type
  {forward declarations}
  TTestCase_Dequeuer = class;
  TTestCase_SubscriptionHandler = class;
  TTestOutputQueueHandler = class;
  TTestableSubscriptionHandler = class;

  TTestableSubscriptionHandler = class(TboldSubscriptionhandler)
  end;

  TTestableDequeuer = class(TBoldDequeuer)
  private
    fOutputQueueHandler: TTestOutputQueueHandler;
  protected
    function GetOutQueueHandler: TBoldAbstractOutputQueueHandler; override;
  public  
    destructor Destroy; override;
  end;

  TTestOutputQueueHandler = class(TBoldAbstractOutputQueueHandler)
  public
    procedure SendEvent(const ClientID: TBoldClientID; EventName: string); override;
    procedure ClearQueueForClient(const ClientID: TBoldClientID); override;
    function QueueCountForClient(const ClientID: TBoldClientID): integer; override;
    procedure SendEventAndFlushQueue(const ClientID: TBoldClientID; EventName: string); override;
  end;

  TTestCase_Dequeuer = class(TBoldTestCase)
  private
    fThreadSafeQueue: TBoldThreadSafeObjectQueue;
    fDequeuer: TTestableDequeuer;
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
    procedure SetUp; override;
    procedure TearDown; override; // cleanup after tests
    property ThreadSafeQueue: TBoldThreadSafeObjectQueue read fThreadSafeQueue;
    property Dequeuer: TTestableDequeuer read fDequeuer;
  published
    procedure SendEvent;
    procedure ClearQueueForClient;
  end;

  TTestCase_SubscriptionHandler = class(TBoldTestCase)
  private
    fSubscriptionHandler: TTestableSubscriptionHandler;
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
    procedure SetUp; override;
    procedure TearDown; override; // cleanup after tests
    property SubscriptionHandler: TTestableSubscriptionHandler read fSubscriptionHandler write fSubscriptionHandler;
  published
    procedure SendEvent;
    procedure CancelSubscription;
    procedure RemoveClient;
  end;

implementation

uses
  SysUtils,
  BoldUtils,
  BoldEnqueuer,
  BoldPropagatorConstants,
  windows;

var
  Client: array [1..5] of TBoldClientID;
  Event: array [1..5] of string;
  Log_SendEvent: TList;
  Log_ClearQueueForClient: TList;

{ TMaan_TestCaseDequeuer }

procedure TTestCase_Dequeuer.SetUp;
var
  i: integer;
begin
  inherited;
  for i:= 1 to 5 do
    Client[i] := i;
  Event[1] := 'E: 2';
  Event[2] := 'I: partof: 3';
  Event[3] := 'C: ClassA';
  fThreadSafeQueue := TBoldThreadSafeObjectQueue.Create('TSQ-Testcase/Dequeuer'); //create a ThreadSafeQueue
  fDequeuer := TTestableDequeuer.Create(ThreadSafeQueue);
end;

class procedure TTestCase_Dequeuer.Suit(ASuite: TBoldTestSuite);
begin
  inherited;
  ASuite.AddTest(CreateWithComment('SendEvent', 'Dequeuer calls the OutputQueueHandler''s SendEvent'));
  ASuite.AddTest(CreateWithComment('ClearQueueForClient', 'Dequeuer calls the OutputQueueHandler''s ClearQueueForClient'));
end;

procedure TTestCase_Dequeuer.TearDown;
var
  i: integer;
  ev: TBoldExternalEvent;
begin
  inherited;
  FreeAndNil(fThreadSafeQueue);
  fDequeuer.Quit(true);
  FreeAndNil(fDequeuer);
  if Assigned(Log_SendEvent) then
    for i:= 0 to Log_SendEvent.Count - 1 do
    begin
      ev := TBoldExternalEvent(Log_SendEvent[i]) ;
      FreeAndNil(ev);
    end;
  FreeAndNil(Log_SendEvent);
  if Assigned(Log_ClearQueueForClient) then
    for i:= 0 to Log_ClearQueueForClient.Count - 1 do
    begin
      ev := TBoldExternalEvent(Log_ClearQueueForClient[i]);
      FreeAndNil(ev);
    end;
  FreeAndNil(Log_ClearQueueForClient);
end;

procedure TTestCase_Dequeuer.SendEvent;
begin
  Dequeuer.Resume;
  Dequeuer.Suspended := false;
  Dequeuer.WaitUntilReady(INFINITE);
  Dequeuer.Priority := tpTimeCritical;

  fThreadSafeQueue.Enqueue(TBoldExternalEvent.Create(Client[1], bemSubscription, Event[1]));
  fThreadSafeQueue.Enqueue(TBoldExternalEvent.Create(Client[2], bemSubscription, Event[1]));
  fThreadSafeQueue.Enqueue(TBoldExternalEvent.Create(Client[3], bemSubscription, Event[1]));
  fThreadSafeQueue.Enqueue(TBoldExternalEvent.Create(Client[2], bemEvent, Event[1]));
  Sleep(0);
  assert(assigned( Log_SendEvent), 'Log not assigned');
  Assert(Log_SendEvent.Count = 2, '');
  Assert((TBoldExternalEvent(Log_SendEvent[0]).BoldClientID = Client[1]) and
          (TBoldExternalEvent(Log_SendEvent[0]).EventName = Event[1]), 'TestDequeuer.SendEvent');
  Assert((TBoldExternalEvent(Log_SendEvent[1]).BoldClientID = Client[3]) and
          (TBoldExternalEvent(Log_SendEvent[1]).EventName = Event[1]), 'TestDequeuer.SendEvent');
end;

procedure TTestCase_Dequeuer.ClearQueueForClient;
begin
  Dequeuer.Priority := tpTimeCritical;
  Dequeuer.Resume;
  // send a bemRemoveClientEvent
  fThreadSafeQueue.Enqueue(TBoldExternalEvent.Create(Client[2], bemRemoveClientQueue, ''));
  Sleep(0);
  assert(assigned( Log_ClearQueueForClient), 'Log_ClearQueueForClient not assigned');
  Assert(Log_ClearQueueForClient.Count = 1, '');
  Assert((TBoldExternalEvent(Log_ClearQueueForClient[0]).BoldClientID = Client[2]) and
          (TBoldExternalEvent(Log_ClearQueueForClient[0]).EventName = ''), 'TestDequeuer.ClearQueueForClient');
end;

{ TMaan_TestCaseSubscriptionHandler }

procedure TTestCase_SubscriptionHandler.SendEvent;
var
  SubscribedClients: TList;
  Ev: TBoldExternalEvent;
begin
  SubscribedClients := TList.Create;
  try
    // Precondition/Action: Send subscriptions and event
    Ev := TBoldExternalEvent.Create(Client[1], bemSubscription, Event[1]);
    SubscriptionHandler.ProcessExternalEvent(Ev);
    FreeAndNil(Ev);
    Ev := TBoldExternalEvent.Create(Client[2], bemSubscription, Event[1]);
    SubscriptionHandler.ProcessExternalEvent(Ev);
    FreeAndNil(Ev);
    Ev := TBoldExternalEvent.Create(Client[3], bemSubscription, Event[1]);
    SubscriptionHandler.ProcessExternalEvent(Ev);
    FreeAndNil(Ev);
    Ev := TBoldExternalEvent.Create(Client[3], bemEvent, Event[1]);
    SubscriptionHandler.ProcessExternalEvent(Ev);
    FreeAndNil(Ev);
    //PostCondition1: 2 events fired
    assert(assigned( Log_SendEvent), 'Log not assigned');
    Assert(Log_SendEvent.Count = 2,'HandlesSubscriptions failed: Incorrect event count');
    Assert((TBoldExternalEvent(Log_SendEvent[0]).BoldClientID = Client[1]) and
            (TBoldExternalEvent(Log_SendEvent[0]).EventName = Event[1]), Format('SendEvent failed fo client %d', [Client[2]]));
    Assert((TBoldExternalEvent(Log_SendEvent[1]).BoldClientID = Client[2]) and
            (TBoldExternalEvent(Log_SendEvent[1]).EventName = Event[1]), Format('SendEvent failed fo client %d', [Client[1]]));

    // PostCondition2: Remove subscription after sending event
    SubscriptionHandler.GetClientsSubscribedToEvent(Event[1], SubscribedClients);
    Assert(SubscribedClients.Count =  1, Format('%s.SendEvent: too many subscribed clients', [ClassName]));
    Assert(TBoldClientID(SubscribedClients[0]) = Client[3], 'Client[3] not subscribed');
  finally
    FreeAndNil(SubscribedClients);
  end;
end;

procedure TTestCase_SubscriptionHandler.CancelSubscription;
var
  SubscribedClients: TList;
  SubscrEvent: TBoldExternalEvent;
begin
  SubscribedClients := TList.Create;
  try
    // PreCondition: subscriptions to Event[2] for clients 2, 3 and 4
    SubscrEvent := TBoldExternalEvent.Create(Client[4], bemSubscription, Event[2]);
    SubscriptionHandler.ProcessExternalEvent(SubscrEvent);
    FreeAndNil(SubscrEvent);
    SubscrEvent := TBoldExternalEvent.Create(Client[3], bemSubscription, Event[2]);
    SubscriptionHandler.ProcessExternalEvent(SubscrEvent);
    FreeAndNil(SubscrEvent);
    SubscrEvent := TBoldExternalEvent.Create(Client[2], bemSubscription, Event[2]);
    SubscriptionHandler.ProcessExternalEvent(SubscrEvent);
    FreeAndNil(SubscrEvent);
    //Action: cancel subscriptions to Event[2]
    SubscrEvent := TBoldExternalEvent.Create(Client[2], bemCancelSubscription, Event[2]);
    SubscriptionHandler.ProcessExternalEvent(SubscrEvent);
    FreeAndNil(SubscrEvent);
    //PostCondition: no subscriptions for Event[2]
    SubscriptionHandler.GetClientsSubscribedToEvent(Event[2], SubscribedClients);
    Assert(SubscribedClients.Count = 0, 'CancelSubscription failed');
  finally
    FreeAndNil(SubscribedClients);
  end;
end;

procedure TTestCase_SubscriptionHandler.RemoveClient;
var
  events: TStringList;
  SubscrEvent : TBoldExternalEvent;
begin
  events := TStringList.Create;
  try
    //Precondition: add a subscription for Client[4]
    SubscrEvent := TBoldExternalEvent.Create(Client[4], bemSubscription, Event[3]);
    SubscriptionHandler.ProcessExternalEvent(SubscrEvent);
    FreeAndNil(SubscrEvent);
    SubscrEvent := TBoldExternalEvent.Create(Client[4], bemSubscription, Event[2]);
    SubscriptionHandler.ProcessExternalEvent(SubscrEvent);
    FreeAndNil(SubscrEvent);
    // Action: remove subscriptions for Client[4]
    SubscriptionHandler.RemoveClient(Client[4]);

    //PostCondition: no subscriptions for Client[4]
    SubscriptionHandler.GetEventsByClientID(Client[4], events);
    Assert(events.Count = 0, Format('%s.Removeclient: Error', [ClassName]));
  finally
    FreeAndNil(events);
  end;
end;

procedure TTestCase_SubscriptionHandler.SetUp;
var
  i: integer;
begin
  inherited;
  fSubscriptionHandler := TTestableSubscriptionHandler.Create;
  SubscriptionHandler.OutQueueHandler := TTestOutputQueueHandler.Create;
  for i:= 1 to 5 do
    Client[i] := i;
  Event[1] := 'E: 2';
  Event[2] := 'I: partof: 3';
  Event[3] := 'C: ClassA';
end;

class procedure TTestCase_SubscriptionHandler.Suit(ASuite: TBoldTestSuite);
begin
  inherited;
  ASuite.AddTest(CreateWithComment('SendEvent',
                 ' Test relationship TBoldSubscriptionHandler->TBoldAbstractOutputQueueHandler '));
  ASuite.AddTest(CreateWithComment('CancelSubscription',
                 ' Test relationship TBoldSubscriptionHandler->TBoldAbstractOutputQueueHandler '));
  ASuite.AddTest(CreateWithComment('RemoveClient',
                 ' Test relationship TBoldSubscriptionHandler->TBoldAbstractOutputQueueHandler '));
end;

procedure TTestCase_SubscriptionHandler.TearDown;
var
  lOutQueueHandler: TBoldAbstractOutputQueueHandler;
  ev: TBoldExternalEvent;
  i: integer;
begin
  inherited;
  if Assigned(Log_SendEvent) then
    for i:= 0 to Log_SendEvent.Count - 1 do
    begin
      ev := TBoldExternalEvent(Log_SendEvent[i]) ;
      FreeAndNil(ev);
    end;
  FreeAndNil(Log_SendEvent);

  if Assigned(Log_ClearQueueForClient) then
    for i:= 0 to Log_ClearQueueForClient.Count - 1 do
    begin
      ev := TBoldExternalEvent(Log_ClearQueueForClient[i]);
      FreeAndNil(ev);
    end;
  FreeAndNil(Log_ClearQueueForClient);
  //Free
  lOutQueueHandler := fSubscriptionHandler.OutQueueHandler;
  FreeAndNil(lOutQueueHandler);
  FreeAndNil(fSubscriptionHandler);
end;

{ TTestOutputQueueHandler }

procedure TTestOutputQueueHandler.ClearQueueForClient(const
  ClientID: TBoldClientID);
begin
  if not Assigned(Log_ClearQueueForClient) then
    Log_ClearQueueForClient := TList.Create;
  Log_ClearQueueForClient.Add(Pointer(TBoldExternalEvent.create(ClientID, bemRemoveClientQueue, '')));
end;

function TTestOutputQueueHandler.QueueCountForClient(
  const ClientID: TBoldClientID): integer;
begin
 Result := -1;
end;

procedure TTestOutputQueueHandler.SendEvent(const ClientID: TBoldClientID; EventName: string);
begin
//  writeln(Format('ClientID: %d || SendToClient: %s', [BoldClientID, EventName]));
  if not Assigned(Log_SendEvent) then
    Log_SendEvent := TList.Create;
  Log_SendEvent.Add(Pointer(TBoldExternalEvent.Create(ClientID, bemEvent, EventName)));
end;

procedure TTestOutputQueueHandler.SendEventAndFlushQueue(
  const ClientID: TBoldClientID; EventName: string);
begin
  inherited;
end;

{ TTestableDequeuer }

destructor TTestableDequeuer.Destroy;
begin
  FreeAndNil(fOutputQueueHandler);
  inherited;
end;

function TTestableDequeuer.GetOutQueueHandler: TBoldAbstractOutputQueueHandler;
begin
  if not Assigned(foutputQueueHandler) then
    fOutputQueueHandler := TTestOutputQueueHandler.Create;
  Result := fOutputQueueHandler;
end;

class function TTestCase_Dequeuer.Suite: ITestSuite;
begin
  result := inherited Suite;
  SetCommentForTest(result, 'SendEvent', 'Dequeuer calls the OutputQueueHandler''s SendEvent');
  SetCommentForTest(result, 'ClearQueueForClient', 'Dequeuer calls the OutputQueueHandler''s ClearQueueForClient');
end;

class function TTestCase_SubscriptionHandler.Suite: ITestSuite;
begin
  result := inherited Suite;
  SetCommentForTest(result, 'SendEvent',
                 ' Test relationship TBoldSubscriptionHandler->TBoldAbstractOutputQueueHandler ');
  SetCommentForTest(result, 'CancelSubscription',
                 ' Test relationship TBoldSubscriptionHandler->TBoldAbstractOutputQueueHandler ');
  SetCommentForTest(result, 'RemoveClient',
                 ' Test relationship TBoldSubscriptionHandler->TBoldAbstractOutputQueueHandler ');
end;

initialization
  TestGlobal.RegisterTestCase(TTestCase_SubscriptionHandler);
  TestGlobal.RegisterTestCase(TTestCase_Dequeuer);

finalization
  TestGlobal.UnRegisterTestCase(TTestCase_SubscriptionHandler);
  TestGlobal.UnRegisterTestCase(TTestCase_Dequeuer);
end.
