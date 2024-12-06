
{ Global compiler directives }
{$include bold.inc}
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
  protected
    procedure DequeueMessages(Sender: TObject);
  public
    destructor Destroy; override;
    procedure QueueNotEmpty;
    procedure DequeueAll; virtual; abstract;
  end;

  TBoldObjectDequeuer = class(TBoldAbstractDequeuer)
  private
    fQueue: TBoldThreadSafeObjectQueue;
  protected
    procedure HandleMessage(const AMessage: TObject); virtual; abstract;
  public
    procedure DequeueAll; override;
    property Queue: TBoldThreadSafeObjectQueue read fQueue write fQueue;
  end;

  TBoldStringDequeuer = class(TBoldAbstractDequeuer)
  private
    fQueue: TBoldThreadSafeStringQueue;
  protected
    procedure HandleMessage(const AMessage: String); virtual; abstract;
  public
    procedure DequeueAll; override;
    property Queue: TBoldThreadSafeStringQueue read fQueue write fQueue;
  end;

implementation

uses
  Classes,
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

{ TBoldObjectDequeuer }

procedure TBoldObjectDequeuer.DequeueAll;
begin
  if not Assigned(Queue) or Queue.Empty then
    exit;
  while not Queue.Empty do
    HandleMessage(Queue.Dequeue);
end;

{ TBoldStringDequeuer }

procedure TBoldStringDequeuer.DequeueAll;
begin
  if not Assigned(Queue) or Queue.Empty then
    exit;
  while not Queue.Empty do
    HandleMessage(Queue.Dequeue);
end;

end.
