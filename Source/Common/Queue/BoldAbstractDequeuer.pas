unit BoldAbstractDequeuer;

interface

uses
  BoldSubscription,
  BoldThreadSafeQueue;

type
  { Forward declarations }
  TBoldAbstractDequeuer = class;

  { TBoldAbstractDequeuer }
  TBoldAbstractDequeuer = class(TBoldSubscribableComponent)
  private
    fQueue: TBoldThreadSafeStringQueue;
  protected
    procedure HandleMessage(aMsg: String); virtual; abstract;
    procedure DequeueMessages(Sender: TObject);
  public
    destructor Destroy; override;
    procedure QueueNotEmpty;
    procedure DequeueAll;
    property Queue: TBoldThreadSafeStringQueue read fQueue write fQueue;
  end;

implementation

uses
  BoldQueue;

{ TBoldDequeuerHandle }

destructor TBoldAbstractDequeuer.Destroy;
begin
  TBoldQueueable.RemoveFromPreDisplayQueue(Self);
  inherited;
end;

procedure TBoldAbstractDequeuer.DequeueMessages(Sender: TObject);
begin
  DequeueAll;
end;

procedure TBoldAbstractDequeuer.QueueNotEmpty;
begin
  TBoldQueueable.AddToPreDisplayQueue(DequeueMessages, nil, Self);
end;

procedure TBoldAbstractDequeuer.DequeueAll;
var
  aMsg: String;
begin
  if Assigned(Queue) then
  begin
    aMsg := Queue.Dequeue;
    while aMsg <> '' do
    begin
      HandleMessage(aMsg);
      aMsg := Queue.Dequeue;
    end;
  end;
end;

end.
