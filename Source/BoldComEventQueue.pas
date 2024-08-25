
{ Global compiler directives }
{$include bold.inc}
unit BoldComEventQueue;


interface

uses
  SyncObjs, //NEW
  BoldBase;

type
  { forward declarations }
  TBoldComEventQueue = class;

  TBoldComEventQueuePopMode = (pmSingleEvent, pmAllEvents);

{//}  TBoldComEventData = record
{//}    SubscriberId,
{//}    Originator,
{//}    OriginalEvent,
{//}    RequestedEvent: Integer;
{//}  end;

{//}  PBoldComEventQueueItem = ^TBoldComEventQueueItem;
{//}  TBoldComEventQueueItem = record
{//}    EventData: TBoldComEventData;
{//}    Next: PBoldComEventQueueItem;
{//}  end;

  TBoldComEventQueueEvent = procedure(const EventData: TBoldComEventData) of object;

  {-- TBoldComEventQueue --}
  TBoldComEventQueue = class(TBoldMemoryManagedObject)
  private
    FCount: Integer;
    FHead: PBoldComEventQueueItem;
    FOnEvent: TBoldComEventQueueEvent;
    FPopMode: TBoldComEventQueuePopMode;
    FTail: PBoldComEventQueueItem;
//TODO    FCriticalSection: TCriticalSection; //TODO Remove comment when interface change is allowed
    class procedure CreateQueueWindow;
    class procedure FreeQueueWindow;
  protected
    function PopItem: PBoldComEventQueueItem;
  public
    constructor Create(PopMode: TBoldComEventQueuePopMode);
    destructor Destroy; override;
    procedure Clear;
    procedure HandleEvent(const EventData: TBoldComEventData); virtual;
    procedure Pop; virtual;
    procedure Push(const EventData: TBoldComEventData);
    property Count: Integer read FCount;
    property OnEvent: TBoldComEventQueueEvent read FOnEvent write FOnEvent;
    property PopMode: TBoldComEventQueuePopMode read FPopMode;
  end;

  TBoldComEventQueue2 = class(TBoldComEventQueue)
    FCriticalSection: TCriticalSection;
  end;


implementation

uses
  BoldEnvironment,
  SysUtils, //NEW
  Windows;

var
  G_FCriticalSection: TCriticalSection = nil; //TODO Remove when interface change is allowed

function FCriticalSection: TCriticalSection; //TODO Remove when interface change is allowed
begin
  if not Assigned(G_FCriticalSection) then
    G_FCriticalSection := TCriticalSection.Create;
  Result := G_FCriticalSection;
end;


const
  BM_POPEVENTQUEUE = $8FFF;
  BM_DESTROYWINDOW = $8FFE;

function BoldComEventQueueWndProc(Window: HWND;
  Message, wParam, lParam: Longint): Longint; stdcall;
var
  ComEventQueue: TBoldComEventQueue;
begin
  case Message of
    BM_POPEVENTQUEUE:
    begin
      Result := 0;
      ComEventQueue := TBoldComEventQueue(lParam);
      ComEventQueue.Pop;
    end;
    BM_DESTROYWINDOW:
    begin
      Result := 0;
      TBoldComEventQueue.FreeQueueWindow;
    end;
  else
    Result := DefWindowProc(Window, Message, wParam, lParam);
  end;
end;

var
  BoldComEventQueueWindow: HWND = 0;
  BoldComEventQueueWindowCount: Integer = 0;
  BoldComEventQueueWindowClass: TWndClass = (
    style: 0;
    lpfnWndProc: @BoldComEventQueueWndProc;
    cbClsExtra: 0;
    cbWndExtra: 0;
    hInstance: 0;
    hIcon: 0;
    hCursor: 0;
    hbrBackground: 0;
    lpszMenuName: nil;
    lpszClassName: 'TBoldComEventQueueWindow');

constructor TBoldComEventQueue.Create(PopMode: TBoldComEventQueuePopMode);
begin
  inherited Create;
//TODO  FCriticalSection := TCriticalSection.Create; //TODO Remove comment when interface change is allowed
  FPopMode := PopMode;
  Inc(BoldComEventQueueWindowCount);
  if BoldComEventQueueWindowCount = 1 then
    CreateQueueWindow;
end;

destructor TBoldComEventQueue.Destroy;
begin
  Clear;
  if not BoldEnvironmentsFinalized then
    BoldEffectiveEnvironment.ProcessMessages;
  Dec(BoldComEventQueueWindowCount);
  if BoldComEventQueueWindowCount = 0 then
    SendMessage(BoldComEventQueueWindow, BM_DESTROYWINDOW, 0, 0);
//TODO  FreeAndNil(FCriticalSection); //TODO Remove comment when interface change is allowed
  inherited Destroy;
end;

class procedure TBoldComEventQueue.CreateQueueWindow;
var
  TempClass: TWndClass;
  ClassRegistered: Boolean;
begin
  BoldComEventQueueWindowClass.hInstance := HInstance;
  ClassRegistered := GetClassInfo(HInstance,
    BoldComEventQueueWindowClass.lpszClassName,TempClass);
  if not ClassRegistered or (TempClass.lpfnWndProc <> @BoldComEventQueueWndProc) then
  begin
    if ClassRegistered then
      Windows.UnregisterClass(BoldComEventQueueWindowClass.lpszClassName, HInstance);
    Windows.RegisterClass(BoldComEventQueueWindowClass);
  end;
  BoldComEventQueueWindow := CreateWindow(BoldComEventQueueWindowClass.lpszClassName, '', 0,
    0, 0, 0, 0, 0, 0, HInstance, nil);
end;

class procedure TBoldComEventQueue.FreeQueueWindow;
begin
  if BoldComEventQueueWindow <> 0 then
  begin
    DestroyWindow(BoldComEventQueueWindow);
    BoldComEventQueueWindow := 0;
  end;
end;

procedure TBoldComEventQueue.Clear;
var
  Item: PBoldComEventQueueItem;
begin
  Item := PopItem;
  while Assigned(Item) do
  begin
  FreeMem(Item, sizeof(TBoldComEventQueueItem));
  Item := PopItem;
  end;
  FCount := 0;
end;

procedure TBoldComEventQueue.HandleEvent(const EventData: TBoldComEventData);
begin
  if Assigned(FOnEvent) then FOnEvent(EventData);
end;

procedure TBoldComEventQueue.Pop;
var
  Item: PBoldComEventQueueItem;
begin
  Item := PopItem;
  if PopMode = pmAllEvents then
  begin
    while Assigned(Item) do
    begin
      try
        HandleEvent(Item^.EventData);
      finally
        FreeMem(Item, sizeof(TBoldComEventQueueItem));
//OLD        Dispose(Item);
      end;
      Item := PopItem;
    end;
  end
  else if Assigned(Item) then
  begin
    try
      HandleEvent(Item^.EventData);
    finally
        FreeMem(Item, sizeof(TBoldComEventQueueItem));
//OLD      Dispose(Item);
    end;
  end;
end;

function TBoldComEventQueue.PopItem: PBoldComEventQueueItem;
begin
  Result := nil;
  if Assigned(FHead) then
  begin
    FCriticalSection.Enter; //NEW
    Result := FHead;
    FHead := Result^.Next;
    if not Assigned(FHead) then
      FTail := nil;
    Dec(FCount);
    FCriticalSection.Leave; //NEW
  end;
end;

procedure TBoldComEventQueue.Push(const EventData: TBoldComEventData);
var
  Item: PBoldComEventQueueItem;
  PostMessagePopEventQueueMessage: Boolean; //NEW
begin
  GetMem(Item, sizeof(TBoldComEventQueueItem));
//OLD  New(Item);
  Move(EventData,Item^.EventData,SizeOf(EventData));
  Item^.Next := nil;
  FCriticalSection.Enter; //NEW
  if not Assigned(FHead) then
    FHead := Item;
  if Assigned(FTail) then
    FTail^.Next := Item;
  FTail := Item;
  Inc(FCount);
  PostMessagePopEventQueueMessage := (popMode=pmSingleEvent) or (FCount=1); //NEW
  FCriticalSection.Leave; //NEW
  if PostMessagePopEventQueueMessage then //NEW
    PostMessage(BoldComEventQueueWindow,BM_POPEVENTQUEUE,0,NativeInt(Self)); //NEW
//OLD  case popMode of
//OLD    pmSingleEvent: PostMessage(BoldComEventQueueWindow,BM_POPEVENTQUEUE,0,Integer(Self));
//OLD    pmAllEvents: if Count = 1 then PostMessage(BoldComEventQueueWindow,BM_POPEVENTQUEUE,0,Integer(Self));
//OLD  end;
end;

initialization
finalization
  FreeAndNil(G_FCriticalSection); //TODO Remove when interface change is allowed
end.
