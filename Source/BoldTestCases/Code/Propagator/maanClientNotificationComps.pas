unit maanClientNotificationComps;

{$INCLUDE Bold.inc}

interface

uses
  {$IFDEF BOLD_DELPHI6_OR_LATER}
  variants,
  {$ENDIF}
  TestSuite,
  BoldDefs,
  BoldOutputQueueHandler,
  BoldPriorityListEnlister,
  BoldClientQueue,
  BoldClientHandler,
  BoldPropagatorInterfaces_TLB,
  BoldThread,
  SysUtils,
//  SLError,
  classes,
  TestFramework;

type

  TTestCase_OutputQueueHandler = class;
  TTestPriorityListEnlister = class;
  TTestCase_PriorityListEnlister = class;
{  TTestCase_ClientNotifierHandler = class;
  TTestCase_ClientNotification = class;
 }
  TTestableOutputQueueHandler = class(TBoldOutputQueueHandler)
  protected
    function getPriorityListEnlister: TBoldAbstractPriorityListEnlister; override;
  end;

  TTestCase_OutputQueueHandler = class(TBoldTestCase)
  private
    fPriorityList: TList;
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
    procedure SetUp; override;
    procedure TearDown; override; // cleanup after tests
    property PriorityList: TList read fPriorityList write fPriorityList;
  published
    procedure SendEvent;
    procedure OutputQueuesTest;
    procedure ClearQueueForClient;
  end;

  TTestPriorityListEnlister = class(TBoldAbstractPriorityListEnlister)
  private
    fPriorityList: Tlist;
  public
    destructor Destroy; override;
    procedure EnlistQueue(ClientQueue: TBoldClientQueue); override;
    procedure FlushQueue(ClientQueue: TBoldClientQueue); override;
    property PriorityList: TList read fPriorityList write fPriorityList;
  end;

  TTestablePriorityListEnlister = class(TBoldPriorityListEnlister)
  protected
    function getClientHandler: TBoldClientHandler; override;
  end;

  TTestCase_PriorityListEnlister = class(TBoldTestCase)
  private
    fNotificationList: TList;
    procedure SendEvents;
    procedure RegisterClients;
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
    procedure SetUp; override;
    procedure TearDown; override; // cleanup after tests
    procedure OnPriorityChanged(Sender: TObject);
    property NotificationList: TList read fNotificationList write fNotificationList;
  published
    procedure EnlistQueue;
    procedure ChangePriority;
//    procedure RemoveClientQueueInfoIfFirstInList;
  end;

  TTestClientListener = class(TInterfacedObject, IBoldListener)
  private
    fEvents: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    function ReceiveEvents(Events: OleVariant) : integer; safecall;
    property Events: TStringList read fEvents write fEvents;
  end;

  TTestListenerThread = class(TBoldNotifiableThread)
  private
    FListener: TTestClientListener;
    FStream: Pointer;
  public
    procedure Execute; override;
    property Stream: Pointer read FStream write FStream;
    property Listener: TTestClientListener read fListener;
  end;

  TTestListenerThread2 = class(TBoldNotifiableThread)
  private
    FListeners: TList;
    FStreams: TList;
    fCount: Integer;
    function getStream(Index: integer): Pointer;
    procedure setStream(Index: integer; Value: Pointer);
    function getListener(Index: integer): TTestClientlistener;
  public
    constructor Create( CreateSuspended: Boolean; const ListenerCount: integer);
    procedure Execute; override;
    property Stream[Index: integer]: Pointer read getStream write setStream;
    property Listener[Index: integer]: TTestClientListener read getListener;
  end;

  TTestClientThread = class(TBoldNotifiableThread)
  public
    procedure Execute; override;
  end;

var
  AClient: array[1..10] of TBoldClientID;
  AEvent: array[1..10] of string;
  ARegTime: array[1..10] of TTimeStamp;
  fListeners: TList;
  fPriorityListEnlister: TBoldAbstractPriorityListEnlister;
  fClientHandler: TBoldClientHandler;
  fOutputQueueHandler: TTestableOutputQueueHandler;
implementation

uses
  dialogs,
  windows,
  comobj,
  ActiveX,
  BoldUtils;

procedure InitializeArrays;
var
  i: integer;
begin
  for i:= 1 to 10 do
    AClient[i] := i;
  AEvent[1] := 'E:2';
  AEvent[2] := 'I:partof:3';
  AEvent[3] := 'C:ClassA';
  AEvent[4] := 'E:4';
  AEvent[5] := 'I:partpartof:5';
end;

{ TTestCase_OutputQueueHandler }

procedure TTestCase_OutputQueueHandler.ClearQueueForClient;
begin
  //PreCondition: non-empty OutputQueue for Client[1]
  fOutputQueueHandler.SendEvent(AClient[1], AEvent[1]);
  fOutputQueueHandler.SendEvent(AClient[1], AEvent[2]);

  //Action: Clear OutputQueue of Client[1]
  fOutputQueueHandler.ClearQueueForClient(AClient[1]);

  //PostCondition: a ClientQueue is created for clients 1 and 2, containing the event messages
  Assert(fOutputQueueHandler.OutputQueues[AClient[1]].Count = 0,
            Format('%s.SendEvent: OutputQueue Count error for client %d.', [ClassName, AClient[1]]));
end;

procedure TTestCase_OutputQueueHandler.OutputQueuesTest;
var
  i: integer;
begin
  for i:= 1 to INITIAL_ARRAY_SIZE + 50 do
    fOutputQueueHandler.SendEvent(i, AEvent[1]);
  for i:= 1 to INITIAL_ARRAY_SIZE + 50 do
  begin
    Assert(fOutputQueueHandler.OutputQueues[i].Count = 1,
            Format('%s.SendEvent: OutputQueue Count error for client %d.', [ClassName, i]));
    Assert(fOutputQueueHandler.OutputQueues[i].Dequeue = AEvent[1],
            Format('%s.SendEvent: Event mismatch for client %d', [ClassName, i]));
  end;
end;

procedure TTestCase_OutputQueueHandler.SendEvent;
begin
  //Action: call SendEvent to simulate event sending for clients 1 and 2
  fOutputQueueHandler.SendEvent(AClient[1], AEvent[1]);
  //Postcondition: EnlistQueue is called for AClient[1]
  Assert((fPriorityListEnlister as TTestPriorityListEnlister).PriorityList.Count = 1, 'EnlistQueue not properly called');
  Assert(Integer((fPriorityListEnlister as TTestPriorityListEnlister).PriorityList[0]) = AClient[1], 'EnlistQueue error');
  fOutputQueueHandler.SendEvent(AClient[1], AEvent[2]);
  fOutputQueueHandler.SendEvent(AClient[2], AEvent[1]);
  //Postcondition: EnlistQueue is called for AClient[2]
  Assert((fPriorityListEnlister as TTestPriorityListEnlister).PriorityList.Count = 2, 'EnlistQueue not properly called');
  Assert(Integer((fPriorityListEnlister as TTestPriorityListEnlister).PriorityList[1]) = AClient[2], 'EnlistQueue error');

  //PostCondition1: a ClientQueue is created for clients 1 and 2, containing the event messages
  Assert(fOutputQueueHandler.OutputQueues[AClient[1]].Count = 2,
            Format('%s.SendEvent: OutputQueue Count error for client %d.', [ClassName, AClient[1]]));
  Assert(fOutputQueueHandler.OutputQueues[AClient[1]].BoldClientID = AClient[1],
            Format('%s.SendEvent: OutputQueue BoldClientID mismatch for client %d.', [ClassName, AClient[1]]));
  Assert(fOutputQueueHandler.OutputQueues[AClient[1]].Dequeue = AEvent[1],
            Format('%s.SendEvent: Event mismatch for client %d', [ClassName, AClient[1]]));
  Assert(fOutputQueueHandler.OutputQueues[AClient[1]].Dequeue = AEvent[2],
            Format('%s.SendEvent: Event mismatch for client %d', [ClassName, AClient[1]]));

  Assert(fOutputQueueHandler.OutputQueues[AClient[2]].Count = 1,
            Format('%s.SendEvent: OutputQueue Count error for client %d.', [ClassName, AClient[2]]));
  Assert(fOutputQueueHandler.OutputQueues[AClient[2]].BoldClientID = AClient[2],
            Format('%s.SendEvent: OutputQueue BoldClientID mismatch for client %d.', [ClassName, AClient[2]]));
  Assert(fOutputQueueHandler.OutputQueues[AClient[2]].Dequeue = AEvent[1],
            Format('%s.SendEvent: Event mismatch for client %d', [ClassName, AClient[2]]));
end;

procedure TTestCase_OutputQueueHandler.SetUp;
begin
  fOutputQueueHandler := TTestableOutputQueueHandler.Create;
  fPriorityListEnlister := TTestPriorityListEnlister.Create;
  fPriorityList := TList.Create;
  (fPriorityListEnlister as TTestPriorityListEnlister).PriorityList := fPriorityList;
end;

class procedure TTestCase_OutputQueueHandler.Suit(ASuite: TBoldTestSuite);
begin
  ASuite.AddTest(CreateWithComment('SendEvent', 'TBoldOutputQueueHandler.SendEvent'));
  ASuite.AddTest(CreateWithComment('OutputQueuesTest', 'TBoldOutputQueueHandler.OutputQueuesLoadTest'));
  ASuite.AddTest(CreateWithComment('ClearQueueForClient', 'TBoldOutputQueueHandler.ClearQueueForClient'));
end;

class function TTestCase_OutputQueueHandler.Suite: ITestSuite;
begin
  result := inherited Suite;
  SetCommentForTest(result, 'SendEvent', 'TBoldOutputQueueHandler.SendEvent');
  SetCommentForTest(result, 'OutputQueuesTest', 'TBoldOutputQueueHandler.OutputQueuesLoadTest');
  SetCommentForTest(result, 'ClearQueueForClient', 'TBoldOutputQueueHandler.ClearQueueForClient');
end;

procedure TTestCase_OutputQueueHandler.TearDown;
begin
  FreeAndNil(fOutputQueueHandler);
  FreeAndNil(fPriorityListEnlister);
end;

{ TTestPriorityListEnlister }

destructor TTestPriorityListEnlister.Destroy;
begin
  FreeAndNil(fPriorityList);
  inherited;
end;

procedure TTestPriorityListEnlister.EnlistQueue(
  ClientQueue: TBoldClientQueue);
begin
  PriorityList.Add(Pointer(TBoldClientQueue(ClientQueue).BoldClientID));
end;

procedure TTestPriorityListEnlister.FlushQueue(
  ClientQueue: TBoldClientQueue);
begin
  PriorityList.Add(Pointer(TBoldClientQueue(ClientQueue).BoldClientID));
end;

{ TTestCase_PriorityListEnlister }

procedure TTestCase_PriorityListEnlister.ChangePriority;
var
  QueueInfo: TBoldClientQueueInfo;
  Queue: variant;
  Count: integer;
begin
  SendEvents;
  //PostCondition1: AClient[2] is at the head of the PriorityList
  QueueInfo := TBoldClientQueueInfo((fPriorityListEnlister as TTestablePriorityListEnlister).PriorityList.Head);
  Queue := QueueInfo.QueueAsVarArray;
  count := VarArrayHighBound(Queue, 1) + 1;
  Assert( Count = 1,
          Format('%s.EnlistQueue: error ClientQueue.count', [ClassName]));

  Assert( (QueueInfo.ClientID = AClient[2]) and
          (Queue[0] = AEvent[1]),
          Format('%s.EnlistQueue: error PriorityList.Head', [ClassName]));

  //PostCondition2: OnPriorityChanged has been called once
  Assert(NotificationList.Count = 2, Format('%s.OnPriorityChanged: event handler not executed', [ClassName]));
end;

procedure TTestCase_PriorityListEnlister.EnlistQueue;
var
  QueueInfo: TBoldClientQueueInfo;
  Queue: variant;
  count: integer;
  event: string;
begin
  //Action: SendEvent for Client 1
  fOutputQueueHandler.SendEvent(AClient[1], AEvent[1]);
  //PostCondition: ClientQueue created for Client1 and contains 1 event
  QueueInfo := TBoldClientQueueInfo((fPriorityListEnlister as TTestablePriorityListEnlister).PriorityList.Head);
  Queue := QueueInfo.QueueAsVarArray;
  Count := VarArrayHighBound(Queue, 1) + 1 ;
  Assert( Count = 1,
          Format('%s.EnlistQueue: error ClientQueue.count', [ClassName]));
  event := Queue[0];
  Assert( (QueueInfo.ClientID = AClient[1]) and
          (event = AEvent[1]),
          Format('%s.EnlistQueue: error PriorityList.Head', [ClassName]));
end;

procedure TTestCase_PriorityListEnlister.OnPriorityChanged(
  Sender: TObject);
begin
  NotificationList.Add(Pointer(NotificationList.Count  + 1));
end;

procedure TTestCase_PriorityListEnlister.RegisterClients;
begin
  //register clients 1 & 2 with ClientHandler
  fClientHandler.RegisterClient(1000*60*5, 1000*5, TTestClientListener.Create,'Test Project' ,AClient[1], ARegTime[1]);
  fClientHandler.RegisterClient(1000*60*5, 1000*3, TTestClientListener.Create, 'Test Project' , AClient[2], ARegTime[2]);
  fClientHandler.RegisterClient(1000*60*5, 1000*3, TTestClientListener.Create, 'Test Project' , AClient[3], ARegTime[3]);
end;

{procedure TTestCase_PriorityListEnlister.RemoveClientQueueInfoIfFirstInList;
var
  i: integer;
begin
  SendEvents;
  // Clear events for AClient[2]
  for i:= 1 to fOutputQueueHandler.OutputQueues[1].Count do
    fOutputQueueHandler.OutputQueues[2].Dequeue;
  //send a new event
  fOutputQueueHandler.SendEvent(AClient[3], AEvent[1]);
  //PostCondition1: AClient[2] is at the head of the PriorityList
  Assert( TBoldClientQueueInfo((fPriorityListEnlister as TTestablePriorityListEnlister).PriorityList.Head).ClientID = AClient[3],
          Format('%s.EnlistQueue: error PriorityList.Head', [ClassName]));
end;
}
procedure TTestCase_PriorityListEnlister.SendEvents;
begin
  //send events
  fOutputQueueHandler.SendEvent(AClient[1], AEvent[1]);
  fOutputQueueHandler.SendEvent(AClient[1], AEvent[2]);
  fOutputQueueHandler.SendEvent(AClient[2], AEvent[1]);
end;

procedure TTestCase_PriorityListEnlister.SetUp;
begin
  inherited;
  fOutputQueueHandler := TTestableOutputQueueHandler.Create;
  fPriorityListEnlister := TTestablePriorityListEnlister.Create;
  fNotificationList := TList.Create;
  fClientHandler := TBoldClientHandler.Create(fClientHandler);
  (fPriorityListEnlister as TTestablePriorityListEnlister).PriorityList.OnHeadChanged := OnPriorityChanged;
  // register clients 1 & 2
  RegisterClients;
end;

class procedure TTestCase_PriorityListEnlister.Suit(ASuite: TBoldTestSuite);
begin
  inherited;
  ASuite.AddTest(CreateWithComment('EnlistQueue', 'TTestCase_PriorityListEnlister.EnlistQueue  //Maan'));
  ASuite.AddTest(CreateWithComment('ChangePriority', 'TTestCase_PriorityListEnlister.ChangePriority  //Maan'));
end;

class function TTestCase_PriorityListEnlister.Suite: ITestSuite;
begin
  result := inherited Suite;
  SetCommentForTest(result, 'EnlistQueue', 'TTestCase_PriorityListEnlister.EnlistQueue  //Maan');
  SetCommentForTest(result, 'ChangePriority', 'TTestCase_PriorityListEnlister.ChangePriority  //Maan');
end;

procedure TTestCase_PriorityListEnlister.TearDown;
begin
  inherited;
  FreeAndNil(fClientHandler);
  FreeAndNil(fOutputQueueHandler);
  FreeAndNil(fPriorityListEnlister);
  FreeAndNil(fNotificationList);
end;

{ TTestClientListener }

constructor TTestClientListener.Create;
begin
  inherited Create;
  fEvents := TStringList.Create;
end;

destructor TTestClientListener.Destroy;
begin
  FreeAndNil(fEvents);
  inherited;
end;

function TTestClientListener.ReceiveEvents(Events: OleVariant): integer;
var
  i: integer;
begin
  if not Assigned(fEvents) then
    fEvents := TStringList.Create;
  for i := VarArrayLowBound(Events, 1) to VarArrayHighBound(Events, 1) do
    fEvents.Add(Events[i]);
end;

{ TTestClientThread }

procedure TTestClientThread.Execute;
var
  i: integer;
  rMsg: TMsg;
  res: integer;
begin
  EnsureMessageQueue;
  fListeners:= TList.Create;
  CoInitializeEx(nil, CoInitFlags);
  for i:= 1 to 10 do
    fListeners.Add(TTestClientListener.Create);
  SignalReady;
  while not Terminated do
  begin
    if PeekMessage(rMsg, 0, 0, 0, PM_NOREMOVE) then
    begin
      res := Integer(getMessage(rMsg, 0, 0, 0));
      if res = -1 then //error
        Terminate
      else if res = 0 then // terminated
        Terminate
      else
        DispatchMessage(rMsg);
    end;
  end;
  CoUninitialize;
end;

{ TTestListenerThread }

procedure TTestListenerThread.Execute;
var
  rMsg: TMsg;
  res: integer;
  aListener: IBoldListener;
begin
try
  EnsureMessageQueue;
  CoInitializeEx(nil, CoInitFlags);
  FListener := TTestClientListener.Create;
  aListener := FListener as IBoldListener;
  CoMarshalInterThreadInterfaceInStream(IID_IBoldListener, aListener, IStream(FStream));
  SignalReady;
  while not Terminated do
  begin
    res := Integer(getMessage(rMsg, 0, 0, 0));
    if res = -1 then //error
      Terminate
    else if res = 0 then // terminated
      Terminate
    else
      DispatchMessage(rMsg);
  end;
  FStream := nil;
  aListener := nil;
  CoUninitialize;
except on E: Exception do
//  SLLogLastException(E, '%s.Execute', [ClassName]);
end;
end;

{ TTestListenerThread2 }

constructor TTestListenerThread2.Create(CreateSuspended: Boolean;
  const ListenerCount: integer);
begin
  inherited Create(CreateSuspended);
  fCount := ListenerCount;
end;

procedure TTestListenerThread2.Execute;
var
  rMsg: TMsg;
  res: integer;
  aListeners: TInterfaceList ;
  i: integer;
  clientListener: TTestClientListener;
  aListener: IBoldListener;
  aStream: Pointer;
begin
try
  EnsureMessageQueue;
  aListeners := TInterfaceList.Create;
  FListeners := TList.Create;
  FStreams := TList.Create;
  CoInitializeEx(nil, CoInitFlags);
  for i:= 0 to fCount - 1 do
  begin
    ClientListener := TTestClientListener.Create;
    FListeners.Add(ClientListener);
    aListener := ClientListener as IBoldListener;
    aListeners.Add(aListener);
    FStreams.Add(nil);
    aStream := FStreams[i];
    CoMarshalInterThreadInterfaceInStream(IID_IBoldListener, aListener, IStream(aStream));
    FStreams[i] := aStream;
  end;
  SignalReady;
  while not Terminated do
  begin
    res := Integer(getMessage(rMsg, 0, 0, 0));
    if res = -1 then //error
      Terminate
    else if res = 0 then // terminated
      Terminate
    else
      DispatchMessage(rMsg);
  end;
  for i:= 0 to fCount - 1 do
  begin
    FStreams[i] := nil;
    aListeners[i] := nil;
  end;
  FreeAndNil(aListeners);
  FreeAndNil(FListeners);
  FreeAndNil(FStreams);
  CoUninitialize;
except on E: Exception do
//  SLLogLastException(E, '%s.Execute', [ClassName]);
end;
end;

function TTestListenerThread2.getListener(Index: integer): TTestClientListener;
begin
  Result := TTestClientListener(FListeners[Index]);
end;

function TTestListenerThread2.getStream(Index: integer): Pointer;
begin
  Result := FStreams[Index];
end;

procedure TTestListenerThread2.setStream(Index: integer; Value: Pointer);
begin
  FStreams[Index] := Value;
end;

{ TTestableOutputQueueHandler }

function TTestableOutputQueueHandler.getPriorityListEnlister: TBoldAbstractPriorityListEnlister;
begin
  Result := fPriorityListEnlister;
end;

{ TTestablePriorityListEnlister }

function TTestablePriorityListEnlister.getClientHandler: TBoldClientHandler;
begin
  Result := fClienthandler;
end;

Initialization
  InitializeArrays;
  TestGlobal.RegisterTestCase(TTestCase_OutputQueueHandler);
  TestGlobal.RegisterTestCase(TTestCase_PriorityListEnlister);

finalization
  TestGlobal.UnRegisterTestCase(TTestCase_OutputQueueHandler);
  TestGlobal.UnregisterTestCase(TTestCase_PriorityListEnlister);
end.
