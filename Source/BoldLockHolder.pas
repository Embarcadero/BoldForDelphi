
{ Global compiler directives }
{$include bold.inc}
unit BoldLockHolder;

interface

uses
  Classes,
  Variants,
  BoldBase,
  BoldListenerThread,
  BoldAbstractDequeuer,
  BoldLockingSupportInterfaces_TLB,
  BoldHashIndexes,
  BoldDefs,
  SyncObjs;

type
  TBoldLock = class;
  TBoldLockList = class;
  TBoldLockHolder = class;

  TBoldLock = class(TBoldMemoryManagedObject)
  private
    fName: string;
  public
    constructor Create(const Name: string);
    property Name: string read fName;
  end;

{  TBoldDatabaseLock = class(TBoldLock)
  public
    constructor Create;
    property Name;
    function AsOLEVariant: OleVariant;
  end;
}
  TBoldLockList = class(TBoldStringHashIndex)
  protected
    function ItemAsKeyString(Item: TObject): string; override;
  public
    destructor Destroy; override;
    procedure Add(Item: TObject); override;
    procedure AddList(List: TBoldLockList);
    procedure AddLock(const Name: string);
    function AsOLEVariant: OleVariant;
    function Includes(const Name: string): Boolean;
    procedure RemoveList(List: TBoldLockList);
    procedure RemoveLock(const Name: string);
  end;

  TBoldAbstractLockHolder = class(TBoldMemoryManagedObject)
  protected
    function GetHeldExclusive: TBoldLockList; virtual; abstract;
    function GetHeldShared: TBoldLockList; virtual; abstract;
  public
    function Lock(Shared: TBoldLockList; Exclusive: TBoldLockList; HeldLocks, ClientsHoldingRequestedLocks: TStringList): Boolean; virtual; abstract;
    procedure Release(Locks: TBoldLockList); virtual; abstract;
    function EnsureLocks: Boolean; virtual; abstract;
    procedure GetPropagationEvents(EventList: TStringList); virtual; abstract;
    property HeldExclusive: TBoldLockList read GetHeldExclusive;
    property HeldShared: TBoldLockList read GetHeldShared;
  end;

  TBoldLockHolder = class(TBoldAbstractLockHolder)
  private
    fHeldExclusive: TBoldLockList;
    fHeldShared: TBoldLockList;
    fListener: TBoldListenerThread;
    fLockManager: IBoldLockManager;
    fTimeOut: Integer;
    fWakeUpEvent: TSimpleEvent;
    fDequeuer: TBoldAbstractDequeuer;
    procedure WaitForWakeup;
    procedure WakeUp;
    function _ListenerMessage(const Msg: string): Boolean;
  protected
    function GetHeldExclusive: TBoldLockList; override;
    function GetHeldShared: TBoldLockList; override;
  public
    constructor Create(Listener: TBoldListenerThread; Dequeuer: TBoldAbstractDequeuer; LockManager: IBoldLockManager);
    destructor Destroy; override;
    function Lock(Shared: TBoldLockList; Exclusive: TBoldLockList; HeldLocks, ClientsHoldingRequestedLocks: TStringList): Boolean; override;
    procedure Release(Locks: TBoldLockList); override;
    function EnsureLocks: Boolean; override;
    function LockDatabase: Boolean; 
    procedure GetPropagationEvents(EventList: TStringList); override;
    property TimeOut: Integer read fTimeOut write fTimeOut;
    property LockManager: IBoldLockManager read fLockManager write fLockManager;
  end;

implementation

uses
  ComObj,
  SysUtils,

  BoldCoreConsts,
  BoldUtils,
  BoldIndex,
  BoldLockingDefs,
  BoldObjectSpaceExternalEvents,
  BoldDefaultID
  ;

{ TBoldLockHolder }

constructor TBoldLockHolder.Create(Listener: TBoldListenerThread; Dequeuer: TBoldAbstractDequeuer; LockManager: IBoldLockManager);
begin
  fHeldExclusive := TBoldLockList.Create;
  fHeldShared := TBoldLockList.Create;
  fListener := Listener;
  fTimeOut := THIRTY_MINUTES;
  fListener.OnMessage := _ListenerMessage;
  FWakeUpEvent := TSimpleEvent.Create;
  fLockManager := LockManager;
  fDequeuer := Dequeuer;
end;

destructor TBoldLockHolder.Destroy;
begin
  FreeAndNil(fHeldExclusive);
  FreeAndNil(fHeldShared);
  FreeAndNil(fWakeUpEvent);
  inherited;
end;

function TBoldLockHolder.EnsureLocks: Boolean;
begin
  result := assigned(LockManager);
  if not result then
    exit;

  if (fHeldExclusive.Count > 0) or (fHeldShared.Count > 0) then
    result := fLockManager.EnsureLocks(fListener.BoldClientID, fHeldExclusive.AsOLEVariant, fHeldShared.AsOleVariant);
end;

function TBoldLockHolder.GetHeldExclusive: TBoldLockList;
begin
  Result := fHeldExclusive;
end;

function TBoldLockHolder.GetHeldShared: TBoldLockList;
begin
  Result := fHeldShared;
end;

procedure TBoldLockHolder.GetPropagationEvents(EventList: TStringList);
begin
  fListener.InQueue.AppendToStringList(EventList);
end;

function TBoldLockHolder.Lock(Shared: TBoldLockList; Exclusive: TBoldLockList; HeldLocks, ClientsHoldingRequestedLocks: TStringList): Boolean;
var
  vClientsHoldingRequestedLocks, vHeldLocks: OleVariant;
begin
  result := assigned(LockManager);
  if not result then
    exit;
  Shared.RemoveList(fHeldShared);
  Shared.RemoveList(fHeldExclusive);
  Exclusive.RemoveList(fHeldExclusive);
  Shared.RemoveList(Exclusive);
  if (Exclusive.Count > 0) or (Shared.Count > 0) then
  begin
    result := LockManager.GetLocks(fListener.BoldClientID, fTimeOut, Exclusive.AsOLEVariant, Shared.AsOleVariant, vHeldLocks, vClientsHoldingRequestedLocks);
    if result then
    begin
      WaitForWakeup;
      fHeldShared.AddList(Shared);
      fHeldExclusive.AddList(Exclusive);
      fHeldShared.RemoveList(Exclusive);
    end
    else
    begin
        BoldVariantToStrings(vClientsHoldingRequestedLocks, ClientsHoldingRequestedLocks);
        BoldVariantToStrings(vHeldLocks, HeldLocks);
    end;
  end;
end;

function TBoldLockHolder.LockDatabase: Boolean;
var
  SharedLocks: TBoldLockList;
  ExclusiveLocks: TBoldLockList;
  Conflicts: TStringList;
  ConflictingUsers: TStringList;
begin
  if not assigned(fDequeuer) then
    raise EBold.CreateFmt(sNoDequeuerAvailable, [classname]);

  SharedLocks := TBoldLockList.Create;
  ExclusiveLocks := TBoldLockList.Create;
  Conflicts := TStringList.Create;
  ConflictingUsers := TStringList.Create;
  try
    ExclusiveLocks.AddLock(BOLD_DBLOCK_NAME);
    result := Lock(SharedLocks, ExclusiveLocks, Conflicts, ConflictingUsers);
    if result then
      fHeldExclusive.AddLock(BOLD_DBLOCK_NAME);
    fDequeuer.DequeueAll;
  finally
    SharedLocks.Free;
    ExclusiveLocks.Free;
    Conflicts.Free;
    ConflictingUsers.Free;
  end;
end;

procedure TBoldLockHolder.Release(Locks: TBoldLockList);
begin
  if assigned(LockManager) then
  begin
    LockManager.ReleaseLocks(fListener.BoldClientID, Locks.AsOLEVariant);
    fHeldExclusive.RemoveList(Locks);
    fHeldShared.RemoveList(Locks);
  end;
end;

procedure TBoldLockHolder.WaitForWakeup;
begin
  if fWakeUpEvent.WaitFor(Timeout) <> wrSignaled then
    raise EBold.CreateFmt(sOperationTimedOut, [ClassName]);
  fWakeUpEvent.ResetEvent;
end;

procedure TBoldLockHolder.WakeUp;
begin
  fWakeUpEvent.SetEvent;
end;

function TBoldLockHolder._ListenerMessage(const Msg: string): Boolean;
var
  ClassName, MemberName, LockName: string;
begin
  if (TBoldObjectSpaceExternalEvent.DecodeExternalEvent(Msg, ClassName, MemberName, LockName, nil) = bsGotLocks) then
  begin
    WakeUp;
    result := true;
  end else
    result := false;
end;

{ TBoldLock }

constructor TBoldLock.Create(const Name: string);
begin
  fName := Name;
end;

{ TBoldLockList }

procedure TBoldLockList.Add(Item: TObject);
begin
  if not (Item is TBoldLock) then
    raise EBold.CreateFmt(sWrongItemType, [classname, Item.classname]);
  inherited;
end;

procedure TBoldLockList.AddList(List: TBoldLockList);
var
  aTraverser: TBoldIndexTraverser;
begin
  if List.Count = 0 then
    exit;
  aTraverser := List.CreateTraverser;
  try
    while aTraverser.MoveNext do
      AddLock(TBoldLock(aTraverser.Item).Name);
  finally
    aTraverser.Free;
  end;
end;

procedure TBoldLockList.AddLock(const Name: string);
begin
  if not Includes(Name) then
    Add(TBoldLock.Create(Name));
end;

function TBoldLockList.AsOLEVariant: OleVariant;
var
  aTraverser: TBoldIndexTraverser;
  i: Integer;
begin
  result := VarArrayCreate([0, Count-1], varOleStr);
  aTraverser := CreateTraverser;
  try
    i := 0;
    while aTraverser.MoveNext do
    begin
      result[i] := (aTraverser.Item as TBoldLock).Name;
      inc(i);
    end;
  finally
    aTraverser.Free;
  end;
end;

destructor TBoldLockList.destroy;
begin
  Clear(True);
  inherited;
end;

function TBoldLockList.Includes(const Name: string): Boolean;
begin
  result := assigned(FindByString(Name));
end;

function TBoldLockList.ItemAsKeyString(Item: TObject): string;
begin
  result := (Item as TBoldLock).Name;
end;

procedure TBoldLockList.RemoveList(List: TBoldLockList);
var
  aTraverser: TBoldIndexTraverser;
begin
  if (count = 0) or (List.Count = 0) then
    exit;
  if List.Count > Count then
  begin
    aTraverser := CreateTraverser;
    while aTraverser.MoveNext do // double while loop is needed as removing from traversing list moves to next item automatically on remove
      while Assigned(aTraverser.Item) and List.Includes((aTraverser.Item as TBoldLock).Name) do
        RemoveLock((aTraverser.Item as TBoldLock).Name);
    aTraverser.Free;
  end else
  begin
    aTraverser := List.CreateTraverser;
    while aTraverser.MoveNext do
    begin
      RemoveLock((aTraverser.Item as TBoldLock).Name);
    end;
    aTraverser.Free;
  end;
end;

procedure TBoldLockList.RemoveLock(const Name: string);
var
  Item: TObject;
begin
  Item := FindByString(Name);
  if Assigned(Item) then
  begin
    Remove(Item);
    FreeAndNil(Item);
  end;
end;

{ TBoldDatabaseLock }
{
function TBoldDatabaseLock.AsOLEVariant: OleVariant;
begin
  result := VarArrayCreate([0, 0], varOleStr);
  result[0] := Name;
end;

constructor TBoldDatabaseLock.Create;
begin
  inherited Create(TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsDBLock, '', '', '', nil));
end;
}

end.
