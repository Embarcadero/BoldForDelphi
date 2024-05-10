
{ Global compiler directives }
{$include bold.inc}
unit BoldPriorityQueue;

interface

uses
  Classes,
  BoldBase,
  BoldLoggableCriticalSection;

type
  TBoldPriorityQueueItem = class;
  TBoldPriorityQueue = class;

  TBoldPriorityQueueItem = class(TBoldMemoryManagedObject)
  public
    function HasHigherPriorityThan(Item: TBoldPriorityQueueItem): Boolean; virtual; abstract;
  end;

  TBoldPriorityQueue = class(TBoldMemoryManagedObject)
  private
    fHeap: TList;
    fOnHeadChanged: TNotifyEvent;
    fLocker: TBoldLoggableCriticalSection;
    procedure SiftUp(pos: Integer);
    procedure SiftDown(pos: Integer);
    function RightChild(pos: Integer): Integer;
    function LeftChild(pos: Integer): Integer;
    function Parent(pos: Integer): Integer;
    procedure swap(pos1, pos2: Integer);
    function GetCount: integer;
    function GetItems(pos: Integer): TBoldPriorityQueueItem;
    procedure InternalRemoveHead;
    procedure SetItems(pos: Integer; const Value: TBoldPriorityQueueItem);
    property Items[pos: Integer]: TBoldPriorityQueueItem read GetItems write SetItems;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Item: TBoldPriorityQueueItem);
    procedure RemoveHead;
    procedure RemoveAndFreeHead;
    function ChopHead: TBoldPriorityQueueItem;
    property Head: TBoldPriorityQueueItem index 1 read GetItems;
    property OnHeadChanged: TNotifyEvent read fOnHeadChanged write fOnHeadChanged;
    property Count: integer read GetCount;
  end;

implementation

uses
  SysUtils;

{ TBoldPriorityQueue }

procedure TBoldPriorityQueue.Add(Item: TBoldPriorityQueueItem);
begin
  fLocker.Acquire;
  try
    fHeap.Add(Item);
    SiftUp(fHeap.Count);
  finally
    flocker.Release;
  end;
end;

function TBoldPriorityQueue.ChopHead: TBoldPriorityQueueItem;
begin
  fLocker.Acquire;
  try
    result := Head;
    RemoveHead;
  finally
    flocker.Release;
  end;
end;

constructor TBoldPriorityQueue.Create;
begin
  fHeap := TList.Create;
  fLocker := TBoldLoggableCriticalSection.Create('PriQ');
end;

destructor TBoldPriorityQueue.Destroy;
begin
  inherited;
  FreeAndNil(fHeap);
  FreeAndNil(fLocker);
end;

function TBoldPriorityQueue.GetCount: integer;
begin
  fLocker.Acquire;
  try
    Result := fHeap.Count;
  finally
    fLocker.Release;
  end;
end;

function TBoldPriorityQueue.GetItems(pos: Integer): TBoldPriorityQueueItem;
begin
  if pos <= fHeap.Count then
    result := TBoldPriorityQueueItem(fHeap[pos-1])
  else
    result := nil;
end;

procedure TBoldPriorityQueue.InternalRemoveHead;
var
  OldHead: TBoldPriorityQueueItem;
begin
  if Assigned(Head) then
  begin
    OldHead := Head;
    fHeap.Exchange(0, fHeap.Count-1);
    fHeap.Delete(fHeap.Count-1);
    SiftDown(1);
    if (Head <> OldHead) and assigned(fOnHeadChanged) then
      OnHeadChanged(self);
  end;
end;

function TBoldPriorityQueue.LeftChild(pos: Integer): Integer;
begin
  result := pos*2;
end;

function TBoldPriorityQueue.Parent(pos: Integer): Integer;
begin
  result := pos div 2;
end;

procedure TBoldPriorityQueue.RemoveAndFreeHead;
var
  Obj: TBoldPriorityQueueItem;
begin
  fLocker.Acquire;
  try
    Obj:= Head;
    InternalRemoveHead;
    FreeAndNil(Obj);
  finally
    fLocker.Release;
  end;
end;

procedure TBoldPriorityQueue.RemoveHead;
begin
  fLocker.Acquire;
  try
    InternalRemoveHead;
  finally
    fLocker.Release;
  end;
end;

function TBoldPriorityQueue.RightChild(pos: Integer): Integer;
begin
  result := pos*2+1;
end;

procedure TBoldPriorityQueue.SetItems(pos: Integer;
  const Value: TBoldPriorityQueueItem);
begin
  fLocker.Acquire;
  try
    fHeap[pos] := Value;
  finally
    fLocker.Release;
  end;  
end;

procedure TBoldPriorityQueue.SiftDown(pos: Integer);
var
  childpos: Integer;
begin
  childpos := LeftChild(pos);
  if not assigned(Items[childpos]) then
    exit;

  if assigned(Items[RightChild(pos)]) then
  begin
    if Items[RightChild(pos)].HasHigherPriorityThan(Items[LeftChild(pos)]) then
      childpos := RightChild(pos);
  end;

  if Items[childpos].HasHigherPriorityThan(Items[pos]) then
  begin
    swap(pos, childpos);
    siftdown(childpos);
  end;
end;

procedure TBoldPriorityQueue.SiftUp(pos: Integer);
var
  parentpos: Integer;
begin
  parentpos := parent(pos);
  if parentpos = 0 then
  begin
    if assigned(OnHeadChanged) then
      OnHeadChanged(self);
    exit;
  end;
  if Items[pos].HasHigherPriorityThan(Items[Parentpos]) then
  begin
    swap(pos, parentpos);
    siftup(parentpos);
  end;
end;

procedure TBoldPriorityQueue.swap(pos1, pos2: Integer);
begin
  flocker.Acquire;
  try
    fHeap.Exchange(pos1-1, pos2-1);
  finally
    fLocker.Release;
  end;
end;

end.
