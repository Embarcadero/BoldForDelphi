
{ Global compiler directives }
{$include bold.inc}
unit BoldOutputQueueHandler;

interface

uses
  BoldDefs,
  BoldAbstractOutputQueueHandler,
  BoldClientQueue,
  BoldPriorityListEnlister,
  BoldThreadSafeQueue,
  BoldContainers,
  BoldSubscription;

const
  INITIAL_ARRAY_SIZE = 100;
  ArrayIncrement = 100;
type
  {forward declarations}
  TBoldOutputQueueHandler = class;

  TBoldOutputQueueHandler = class(TBoldAbstractOutputQueueHandler)
  private
    fOutputQueues: TBoldObjectArray;
    function getOutputQueue(Index: TBoldClientID): TBoldClientQueue;
  protected
    function getPriorityListEnlister: TBoldAbstractPriorityListEnlister; virtual;
    property PriorityListEnlister: TBoldAbstractPriorityListEnlister read GetPrioritylistEnlister;
  public
    constructor Create; 
    destructor Destroy; override;
    procedure SendEvent(const ClientID: TBoldClientID; EventName: string); override;
    procedure ClearQueueForClient(const ClientID: TBoldClientID); override;
    function QueueCountForClient(const ClientID: TBoldClientID): integer; override;
    procedure SendEventAndFlushQueue(const ClientID: TBoldClientID; EventName: string); override;
    procedure OnQueueNotEmpty(Queue:  TBoldThreadSafeQueue);
    property OutputQueues[Index: TBoldClientID]: TBoldClientQueue read getOutputQueue;
  end;

implementation

uses
  windows,
  SysUtils,
  BoldUtils,
  BoldPropagatorConstants,
  BoldPropagatorServer,
  BoldThreadSafeLog;


{ TBoldOutputQueueHandler }

procedure TBoldOutputQueueHandler.ClearQueueForClient(
  const ClientID: TBoldClientID);
begin
  OutputQueues[ClientID].Clear;
end;

constructor TBoldOutputQueueHandler.Create;
begin
  inherited;
  fOutputQueues := TBoldObjectArray.Create(INITIAL_ARRAY_SIZE, [bcoDataOwner, bcoThreadSafe]);
end;

destructor TBoldOutputQueueHandler.Destroy;
begin
  FreeAndNil(fOutputQueues);
  inherited;
end;

function TBoldOutputQueueHandler.getOutputQueue(
  Index: TBoldClientID): TBoldClientQueue;
var
  count, i: integer;
  ClientQueue: TBoldClientQueue;
begin
  if (Index >= fOutputQueues.Count) then
  begin
    count := (Index - fOutputQueues.Count) + 1;
    if count < ArrayIncrement then
      count := ArrayIncrement;
    for i:= 0 to count do
      fOutputQueues.Add(nil);
  end;
  if not Assigned(fOutputQueues[Index]) then
  begin
    ClientQueue := TBoldClientQueue.Create(format('CliQ[%d]', [Index]));
    ClientQueue.OnQueueNotEmpty := OnQueueNotEmpty;
    ClientQueue.BoldClientID := Index;
    fOutputQueues[Index] := ClientQueue;
  end;
  Result := fOutputQueues[Index] as TBoldClientQueue;
end;

function TBoldOutputQueueHandler.getPriorityListEnlister: TBoldAbstractPriorityListEnlister;
begin
  if Assigned(TBoldPropagatorServer.Instance.AdvancedPropagator) then
    Result := TBoldPropagatorServer.Instance.AdvancedPropagator.PriorityListEnlister
  else
    Result := nil;
end;

procedure TBoldOutputQueueHandler.OnQueueNotEmpty(
  Queue: TBoldThreadSafeQueue);
begin
  try
    if Assigned(PriorityListEnlister) then
      PriorityListEnlister.EnlistQueue(Queue as TBoldClientQueue);
  except
  end;
end;

function TBoldOutputQueueHandler.QueueCountForClient(const ClientID: TBoldClientID): integer;
begin
  result := OutputQueues[ClientID].Count;
end;

procedure TBoldOutputQueueHandler.SendEvent(const ClientID: TBoldClientID; EventName: string);
begin
  try
    OutputQueues[ClientID].Enqueue(EventName);
  except On E: Exception do
    BoldLogError('%s.SendEvent: Client = %d', [ClassName, ClientId]);
  end;
end;

procedure TBoldOutputQueueHandler.SendEventAndFlushQueue(const ClientID: TBoldClientID;
  EventName: string);
begin
  OutputQueues[ClientID].Enqueue(EventName);
  PriorityListEnlister.FlushQueue(OutputQueues[ClientID]);
end;

end.
