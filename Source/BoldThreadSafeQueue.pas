
{ Global compiler directives }
{$include bold.inc}
unit BoldThreadSafeQueue;

interface

uses
  Classes,
  Contnrs,
  BoldBase,
  BoldLoggableCriticalSection;

type
  { forward declarations }
  TBoldThreadSafeQueue = class;
  TBoldThreadSafeQueueEntry = class;
  TBoldThreadSafeStringQueue = class;

  TBoldQueueEvent = procedure(Queue:  TBoldThreadSafeQueue) of Object;

  {ring queue with marker}

  { TBoldThreadSafeQueueEntry }
  TBoldThreadSafeQueueEntry = class(TBoldMemoryManagedObject)
  private
    fPrev: TBoldThreadSafeQueueEntry;
    fNext: TBoldThreadSafeQueueEntry;
  protected
    constructor CreateAsMarker;
    constructor CreateAfter(Entry: TBoldThreadSafeQueueEntry);
  public
    destructor Destroy; override;
    property Prev: TBoldThreadSafeQueueEntry read fPrev;
    property Next: TBoldThreadSafeQueueEntry read fNext;
  end;

  { TBoldThreadSafeQueue }
  TBoldThreadSafeQueue = class(TBoldMemoryManagedObject)
  private
    fLocker: TBoldLoggableCriticalSection;
    fMarker: TBoldThreadSafeQueueEntry;
    fOnQueueNotEmpty: TBoldQueueEvent;
    function GetEmpty: Boolean;
  protected
    function GetCount: integer; virtual;
    procedure Lock;
    procedure Unlock;
    procedure NotifyQueueNotEmpty;
    function UnsafeIsEmpty: Boolean; {not threadsafe, for internal use only}
  public
    constructor Create(const Name: String);
    destructor Destroy; override;
    procedure Clear;
    property Count: integer read GetCount;
    property Empty: Boolean read GetEmpty;
    property OnQueueNotEmpty: TBoldQueueEvent read fOnQueueNotEmpty write fOnQueueNotEmpty;
  end;

  { TBoldThreadSafeObjectQueue }
  TBoldThreadSafeObjectQueue = class(TBoldThreadSafeQueue)
  private
    fOwnsObjects: boolean;
    fCount: integer;
    fMaxCount: integer;
    function GetMaxCount: integer;
    procedure SetOwnsObjects(const Value: boolean);
  protected
    function GetCount: integer; override;
    procedure UnsafeEnqueue(anObject: TObject);
    function UnsafeDequeue: TObject;
  public
    constructor Create(const Name: String; OwnsObjects: Boolean = true);
    procedure Enqueue(anObject: TObject);
    procedure DequeueList(ResultList: TObjectList; Max: integer);
    function Dequeue: TObject; // returns nil if queue empty
    property OwnsObjects: boolean read fOwnsObjects write SetOwnsObjects;
    property MaxCount: integer read GetMaxCount;
  end;

  { TBoldThreadSafeInterfaceQueue }
  TBoldThreadSafeInterfaceQueue = class(TBoldThreadSafeQueue)
  public
    procedure Enqueue(const anInterface: IInterface);
    function Dequeue: IInterface; // returns nil if queue empty
  end;

  { TBoldThreadSafeStringQueue }
  TBoldThreadSafeStringQueue = class(TBoldThreadSafeQueue)
  public
    procedure Enqueue(const aString: string);
    procedure EnqueueList(aList: TStrings);    
    function Dequeue: string; // returns '' string if queue empty
    procedure AppendToStringList(aList: TStrings);
  end;

implementation

uses
  SysUtils;

type
  { TBoldThreadSafeQueueObjectEntry }
  TBoldThreadSafeQueueObjectEntry = class(TBoldThreadSafeQueueEntry)
  private
    fAnObject: TObject;
    fOwnsObject: boolean;
  public
    constructor CreateAfter(anObject: TObject; Entry: TBoldThreadSafeQueueEntry; OwnsObject: boolean);
    destructor Destroy; override;
    function GetObject: TObject;
    property AnObject: TObject read fAnObject;
  end;

  { TBoldThreadSafeQueueStringEntry }
  TBoldThreadSafeQueueStringEntry = class(TBoldThreadSafeQueueEntry)
  private
    faString: string;
  public
    constructor CreateAfter(const aString: string; Entry: TBoldThreadSafeQueueEntry);
    property aString: string read fAString;
  end;

  { TBoldThreadSafeQueueInterfaceEntry }
  TBoldThreadSafeQueueInterfaceEntry = class(TBoldThreadSafeQueueEntry)
  private
    fanInterface: IInterface;
  public
    constructor CreateAfter(anInterface: IInterface; Entry: TBoldThreadSafeQueueEntry);
    property anInterface: IInterface read fanInterface;
  end;

  { TBoldThreadSafeQueue }

constructor TBoldThreadSafeQueue.Create(const Name: String);
begin
  inherited create;
  fLocker := TBoldLoggableCriticalSection.Create(Name);
  fMarker := TBoldThreadSafeQueueEntry.CreateAsMarker;
end;

destructor TBoldThreadSafeQueue.Destroy;
begin
  Clear;
  FreeAndNil(fMarker);
  FreeAndNil(fLocker);
  inherited;
end;

function TBoldThreadSafeQueue.GetCount: integer;
var
  Entry: TBoldThreadSafeQueueEntry;
begin
  Result := 0;
  Lock;
  try
    Entry := fMarker;
    while (Entry.Next <> fMarker) do
    begin
      Entry := Entry.Next;
      INC(Result);
    end;
  finally
    Unlock;
  end;
end;

function TBoldThreadSafeQueue.GetEmpty: Boolean;
begin
  Lock;
  try
    result := fMarker.Next = fMarker;
  finally
    Unlock;
  end;
end;

function TBoldThreadSafeQueue.UnsafeIsEmpty: Boolean;
begin
  Result := fMarker = fMarker.Next;
end;

procedure TBoldThreadSafeQueue.Lock;
begin
  fLocker.Acquire;
end;

procedure TBoldThreadSafeQueue.NotifyQueueNotEmpty;
begin
  if @fOnQueueNotEmpty <> nil then
    fOnQueueNotEmpty(self);
end;

procedure TBoldThreadSafeQueue.Unlock;
begin
  fLocker.Release;
end;

procedure TBoldThreadSafeQueue.Clear;
begin
  Lock;
  try
    while not UnsafeIsEmpty do
      FMarker.Next.Free;
  finally
    Unlock;
  end;
end;

{ TBoldThreadSafeQueueEntry }

constructor TBoldThreadSafeQueueEntry.CreateAsMarker;
begin
  inherited;
  fPrev := Self;
  fNext := Self;
end;

constructor TBoldThreadSafeQueueEntry.CreateAfter(
  Entry: TBoldThreadSafeQueueEntry);
begin
  fPrev := Entry;
  fNext := Entry.Next;
  Entry.fNext := self;
  fNext.fPrev := self;
end;

destructor TBoldThreadSafeQueueEntry.Destroy;
begin
  fPrev.fNext := fNext;
  fNext.fPrev := fPrev;
  inherited;
end;

{ TBoldThreadSafeQueueObjectEntry }

constructor TBoldThreadSafeQueueObjectEntry.CreateAfter(anObject: TObject;
  Entry: TBoldThreadSafeQueueEntry; OwnsObject: boolean);
begin
  inherited CreateAfter(Entry);
  fAnObject := anObject;
  fOwnsObject := OwnsObject;
end;

destructor TBoldThreadSafeQueueObjectEntry.Destroy;
begin
  if fOwnsObject then
    anObject.Free;
  inherited;
end;

function TBoldThreadSafeQueueObjectEntry.GetObject: TObject;
begin
  Result := anObject;
  fAnObject := nil;
end;

{ TBoldThreadSafeObjectQueue }

constructor TBoldThreadSafeObjectQueue.Create(const Name: String; OwnsObjects: Boolean);
begin
  inherited Create(Name);
  fOwnsObjects := OwnsObjects;
end;

function TBoldThreadSafeObjectQueue.Dequeue: TObject;
begin
  Lock;
  try
    result := UnsafeDequeue;
  finally
    Unlock;
  end;
end;

procedure TBoldThreadSafeObjectQueue.DequeueList(ResultList: TObjectList; Max: integer);
begin
  Lock;
  try
    while not UnsafeIsEmpty and (ResultList.Count < max) do
      ResultList.Add(UnsafeDequeue);
  finally
    UnLock;
  end;
end;

procedure TBoldThreadSafeObjectQueue.Enqueue(anObject: TObject);
begin
  assert(anObject <> nil);
  Lock;
  try
    UnsafeEnqueue(anObject);
  finally
    Unlock;
  end;
end;

function TBoldThreadSafeObjectQueue.GetCount: integer;
begin
  Lock;
  try
    result := fCount;
  finally
    UnLock
  end;
end;

function TBoldThreadSafeObjectQueue.GetMaxCount: integer;
begin
  Lock;
  try
    result := fMaxCount;
  finally
    UnLock;
  end;
end;

procedure TBoldThreadSafeObjectQueue.SetOwnsObjects(const Value: boolean);
begin
  Lock;
  try
    fOwnsObjects := Value;
  finally
    Unlock;
  end;
end;

function TBoldThreadSafeObjectQueue.UnsafeDequeue: TObject;
var
  Head: TBoldThreadSafeQueueEntry;
begin
  Head := fMarker.Prev;
  if UnsafeIsEmpty then
    Result := nil
  else
  begin
    Assert(Head is TBoldThreadSafeQueueObjectEntry);
    Result := TBoldThreadSafeQueueObjectEntry(Head).GetObject;
    Head.Free;
    dec(fCount);
  end;
end;

procedure TBoldThreadSafeObjectQueue.UnsafeEnqueue(anObject: TObject);
var
  WasEmpty: Boolean;
begin
  WasEmpty := UnsafeIsEmpty;
  TBoldThreadSafeQueueObjectEntry.CreateAfter(anObject, fMarker, OwnsObjects);
  inc(fCount);
  if fCount > fMaxCount then
    fMaxCount := fCount;

  if (WasEmpty) then
    NotifyQueueNotEmpty;
end;

{ TBoldThreadSafeQueueStringEntry }

constructor TBoldThreadSafeQueueStringEntry.CreateAfter(const aString: string;
  Entry: TBoldThreadSafeQueueEntry);
begin
  inherited CreateAfter(Entry);
  faString := aString;
end;

{ TBoldThreadSafeStringQueue }

procedure TBoldThreadSafeStringQueue.AppendToStringList(
  aList: TStrings);
var
  Entry: TBoldThreadSafeQueueEntry;
begin
  Lock;
  try
    Entry := fMarker;
    while (Entry.Next <> fMarker) do
    begin
      Entry := Entry.Next;
      Assert(Entry is TBoldThreadSafeQueueStringEntry);
      aList.Add(TBoldThreadSafeQueueStringEntry(Entry).aString);
    end;
  finally
    Unlock;
  end;
end;

function TBoldThreadSafeStringQueue.Dequeue: string;
var
  Head: TBoldThreadSafeQueueEntry;
begin
  Lock;
  try
    Head := fMarker.Prev;
    if UnsafeIsEmpty then
      Result := ''
    else
    begin
      Assert(Head is TBoldThreadSafeQueueStringEntry);
      Result := TBoldThreadSafeQueueStringEntry(Head).aString;
      Head.Free;
    end;
  finally
    Unlock;
  end;
end;

procedure TBoldThreadSafeStringQueue.Enqueue(const aString: string);
var
  WasEmpty: Boolean;
begin
  assert(aString <> '');
  Lock;
  try
    WasEmpty := UnsafeIsEmpty;
    TBoldThreadSafeQueueStringEntry.CreateAfter(aString, fMarker);
    if (WasEmpty) then
      NotifyQueueNotEmpty;
  finally
    Unlock;
  end;
end;

procedure TBoldThreadSafeStringQueue.EnqueueList(aList: TStrings);
var
  WasEmpty: Boolean;
  i: integer;
  s: string;
begin
  if (aList.Count = 0) then exit;
  Lock;
  try
    WasEmpty := UnsafeIsEmpty;
    for I := 0 to aList.Count - 1 do
    begin
      s := aList[i];
      assert(s <> '');
      TBoldThreadSafeQueueStringEntry.CreateAfter(s, fMarker);
    end;
    if (WasEmpty) then
      NotifyQueueNotEmpty;
  finally
    Unlock;
  end;
end;

{ TBoldThreadSafeInterfaceQueue }

function TBoldThreadSafeInterfaceQueue.Dequeue: IInterface;
var
  Head: TBoldThreadSafeQueueEntry;
begin
  Lock;
  try
    Head := fMarker.Prev;
    if UnsafeIsEmpty then
      Result := nil
    else
    begin
      Assert(Head is TBoldThreadSafeQueueInterfaceEntry);
      Result := TBoldThreadSafeQueueInterfaceEntry(Head).anInterface;
      Head.Free;
    end;
  finally
    Unlock;
  end;
end;

procedure TBoldThreadSafeInterfaceQueue.Enqueue(const anInterface: IInterface);
var
  WasEmpty: Boolean;
begin
  assert(anInterface <> nil);
  Lock;
  try
    WasEmpty := UnsafeIsEmpty;
    TBoldThreadSafeQueueinterfaceEntry.CreateAfter(anInterface, fMarker);
    if (WasEmpty) then
      NotifyQueueNotEmpty;
  finally
    Unlock;
  end;
end;  

{ TBoldThreadSafeQueueInterfaceEntry }

constructor TBoldThreadSafeQueueInterfaceEntry.CreateAfter(
  anInterface: IInterface; Entry: TBoldThreadSafeQueueEntry);
begin
  inherited CreateAfter(Entry);
  fanInterface := anInterface;
end;

end.
