unit maanEnqueuer;

interface

uses
  TestSuite,
  SysUtils,
  Classes,
  BoldPropagatorInterfaces_TLB,
  BoldPropagatorSubscriptions,
  BoldDefs,
  Controls,
  maan_PropagatorConnection,
  Forms,
  TestFramework;

type

  TTestCase_Enqueuer = class(TBoldTestCase)
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test;
  end;

  TTestCase_BoldSubscriptionList = class(TBoldTestCase)
  private
    fLinkedList: TBoldSubscriptionList;
    AClientID: array [1..4] of TBoldClientID;
    AEvent: array [1..3] of string;
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;    
    procedure SetUp; override;
    procedure TearDown; override;
    property LinkedList: TBoldSubscriptionList read fLinkedList write fLinkedList;
  published
    procedure AddSubscription;
    procedure GetClientIdsByEvents;
    procedure GetAllClientsExcept;
    procedure GetEventsByClientID;
    procedure RemoveClient;
    procedure RemoveSubscription;
  end;

implementation

uses
  ActiveX,
  BoldUtils,
  dialogs,
  windows;

class procedure TTestCase_Enqueuer.Suit(ASuite: TBoldTestSuite);
begin
  ASuite.AddTest(CreateWithComment('Test', 'Make a COM call to TBoldEnqueuer'));
end;

procedure TTestCase_Enqueuer.SetUp;
begin
  inherited;
  maan_PropagatorConnection.EnsureDm;
  dmPropConnection.BoldComConnectionHandle1.Connected := True;
end;

procedure TTestCase_Enqueuer.Test;
var
  list: TStringList;
  ClientID: TBoldClientID;
  TestOK: boolean;
begin
  list := TStringList.Create;
  try
    list.Add('A1');
    list.Add('A2');
    list.Add('A3');
    list.Add('A4');
    ClientID := 901;
    TestOk:= true;
    try
      dmPropConnection.BoldPropagatorHandleCOM1.EventPropagator.SendEvents(ClientID, StringListToVarArray(list));
      dmPropConnection.BoldPropagatorHandleCOM1.EventPropagator.AddSubscriptions(ClientID, StringListToVarArray(list));
      dmPropConnection.BoldPropagatorHandleCOM1.EventPropagator.CancelSubscriptions(ClientID, StringListToVarArray(list));
    except
      TestOk := false;
    end;
  finally
    FreeAndNil(list);
  end;
  Assert(TestOK, 'IEventReceiver.SendEvents');
end;


  {TMaan_TestCaseBoldSubscriptionList}
class procedure TTestCase_BoldSubscriptionList.Suit(ASuite: TBoldTestSuite);
begin
  ASuite.AddTest(CreateWithComment('AddSubscription', 'ListOperation: AddSubscription' ));
  ASuite.AddTest(CreateWithComment('GetClientIdsByEvents', 'ListOperation: GetClientIDsByEvents' ));
  ASuite.AddTest(CreateWithComment('GetAllClientsExcept', 'ListOperation: GetAllClientsExcept' ));
  ASuite.AddTest(CreateWithComment('GetEventsByClientID', 'ListOperation: GetEventsByClientID' ));
  ASuite.AddTest(CreateWithComment('RemoveClient', 'ListOperation: RemoveClient' ));
  ASuite.AddTest(CreateWithComment('RemoveSubscription', 'ListOperation: RemoveSubscription' ));
end;

procedure TTestCase_BoldSubscriptionList.SetUp;
var
  i: integer;
begin
  for i:= 1 to 4 do
    AClientID[i] := i;
  AEvent[1] := 'E:2';
  AEvent[2] := 'C: ClassA';
  AEvent[3] := 'I: partof: 3';
  LinkedList := TBoldSubscriptionList.Create;
  //PreCondition/Action: add subscriptions
  LinkedList.AddSubscription(AClientID[1], AEvent[1]);
  LinkedList.AddSubscription(AClientID[2], AEvent[1]);
  LinkedList.AddSubscription(AClientID[4], AEvent[2]);
  LinkedList.AddSubscription(AClientID[4], AEvent[1]);
end;


procedure TTestCase_BoldSubscriptionList.AddSubscription;
var
  ClientIDs: TList;
begin
  //PostCondition:
  Assert(Assigned(LinkedList.Clients[1].Next), Format('AddSubscription: Client %d not found', [1]));
  Assert(Assigned(LinkedList.Clients[2].Next), Format('AddSubscription: Client %d not found', [2]));
  Assert(Assigned(LinkedList.Clients[4].Next), Format('AddSubscription: Client %d not found', [4]));

  Assert(Assigned(LinkedList.Events[AEvent[1]]), Format('AddSubscription: Event %s not found', [AEvent[1]]));
  Assert(Assigned(LinkedList.Events[AEvent[2]]), Format('AddSubscription: Event %s not found', [AEvent[2]]));

  try
    //PostCondition: Only Client[4] is subscribed to AEvent[2]
    ClientIds := TList.Create;
    LinkedList.GetClientsSubscribedToEvent(AEvent[2], ClientIds);
    Assert(Integer(ClientIDs[0]) = AClientId[4], 'Client not subscribed for event');

    //PostCondition: No clients are registered for AEvent[3]
    ClientIds.Clear;
    LinkedList.GetClientsSubscribedToEvent(AEvent[3], ClientIds);
    Assert((ClientIds.Count = 0), 'Call to GetClientIdsByEvent failed');
  finally
    FreeAndNil(ClientIds);
  end;
end;

procedure TTestCase_BoldSubscriptionList.GetClientIDsByEvents;
var
  ClientIDs: TList;
begin
  ClientIds := TList.Create;
  try
    //PreCondition: add Event[1] for clients 0,1 and 2 -->> in AddSubscription
    LinkedList.GetClientsSubscribedToEvent(AEvent[1], ClientIds);
    //PostCondition:
    Assert(Integer(ClientIDs[0]) = AClientID[4], Format('GetClientIDsByEvents: Client %d not subscribed ', [AClientId[4]]));
    Assert(Integer(ClientIDs[1]) = AClientID[2], Format('GetClientIDsByEvents: Client %d not subscribed ', [AClientId[2]]));
    Assert(Integer(ClientIDs[2]) = AClientID[1], Format('GetClientIDsByEvents: Client %d not subscribed ', [AClientId[1]]));
  finally
    FreeAndNil(ClientIds);
  end;
end;

procedure TTestCase_BoldSubscriptionList.GetAllClientsExcept;
var
  ClientIDs: TBoldSortedIntegerList;
begin
  ClientIds := TBoldSortedIntegerList.Create;
  try
    //PreCondition:
    LinkedList.GetAllSubscribedClientsExcept(AEvent[1], AClientID[4], ClientIds);
    //PostCondition:
    Assert(ClientIDs.Count = 2, 'Call to GetClientIDsByEvent failed');
    Assert(Integer(ClientIDs[0]) = AClientID[1], 'Client not subscribed for event');
    Assert(Integer(ClientIDs[1]) = AClientID[2], 'Client not subscribed for event');
  finally
    FreeAndNil(ClientIds);
  end;
end;


procedure TTestCase_BoldSubscriptionList.GetEventsByClientID;
var
  events: TStringList;
begin
  events := TStringList.Create;
  try
    //PostCondition: AClientID[4] is subscribed to events AEvent[1] and AEvent[2]
    LinkedList.GetEventsByClientID(AClientID[4], events);
    Assert(((events[0] = AEvent[2]) and (events[1] = AEvent[1])) or
              ((events[0] = AEvent[1]) and (events[1] = AEvent[2]))  , 'Client not subscribed for events');
    //PostCondition: AClientID[3] is not subscribed to any events
    events.Clear;
    LinkedList.getEventsByClientID(AClientID[3], events);
    Assert((events.count = 0), 'Call to GetEventsByClientID failed');
  finally
    FreeAndNil(Events);
  end;
end;

procedure TTestCase_BoldSubscriptionList.RemoveClient;
var
  ClientIDs: TList;
  events: TStringList;
begin
  events := TStringList.Create;
  ClientIds := TList.Create;
  try
    // Action:
    LinkedList.RemoveClient(AClientID[4]);
    LinkedList.RemoveClient(1234);
    // PostCondition: no subscriptions for AClientID[4]
    LinkedList.GetEventsByClientID(AClientID[4], events);
    Assert(events.count = 0, 'Call to RemoveSubscriptionsForClient failed');
    LinkedList.GetClientsSubscribedToEvent(AEvent[1], ClientIds);
    Assert(ClientIds.Count = 2, 'too many clients');
    Assert(Integer(ClientIDs[0]) = AClientID[2], 'Client not subscribed for event');
    Assert(Integer(ClientIDs[1]) = AClientID[1], 'Client not subscribed for event');
  finally
    FreeAndNil(events);
    FreeAndNil(ClientIds);
  end;
end;

procedure TTestCase_BoldSubscriptionList.RemoveSubscription;
var
  ClientIDs: TList;
begin
  ClientIds := TList.Create;
  try
    // PreCondition:
    LinkedList.AddSubscription(AClientID[4], AEvent[2]);
    LinkedList.AddSubscription(AClientID[4], AEvent[3]);
    //Action: remove AEvent[1]
    LinkedList.RemoveEvent(AEvent[1]);
    LinkedList.RemoveEvent('E:junkjunk:morejunk');

    // PostCondition: no clients subscribed to AEvent[1]
    LinkedList.GetClientsSubscribedToEvent(AEvent[1], ClientIds);
    Assert(ClientIds.Count = 0, 'Call to GetClientIDsByEvent failed');
  finally
    FreeAndNil(ClientIds);
  end;
end;

procedure TTestCase_BoldSubscriptionList.TearDown;
begin
  // Free the list
  FreeAndNil(fLinkedList);
  inherited;
end;

procedure TTestCase_Enqueuer.TearDown;
begin
  dmPropConnection.BoldComConnectionHandle1.Connected := false;
  FreeAndNIl(dmPropConnection);
end;

class function TTestCase_Enqueuer.Suite: ITestSuite;
begin
  result := inherited Suite;
  SetCommentForTest(result, 'Test', 'Make a COM call to TBoldEnqueuer');
end;

class function TTestCase_BoldSubscriptionList.Suite: ITestSuite;
begin
  result := inherited Suite;
  SetCommentForTest(result, 'AddSubscription', 'ListOperation: AddSubscription' );
  SetCommentForTest(result, 'GetClientIdsByEvents', 'ListOperation: GetClientIDsByEvents' );
  SetCommentForTest(result, 'GetAllClientsExcept', 'ListOperation: GetAllClientsExcept' );
  SetCommentForTest(result, 'GetEventsByClientID', 'ListOperation: GetEventsByClientID' );
  SetCommentForTest(result, 'RemoveClient', 'ListOperation: RemoveClient' );
  SetCommentForTest(result, 'RemoveSubscription', 'ListOperation: RemoveSubscription' );
end;

initialization
  TestGlobal.RegisterTestCase(TTestCase_Enqueuer);
  TestGlobal.RegisterTestCase(TTestCase_BoldSubscriptionList);

finalization
  TestGlobal.UnRegisterTestCase(TTestCase_Enqueuer);
  TestGlobal.UnRegisterTestCase(TTestCase_BoldSubscriptionList);
end.
