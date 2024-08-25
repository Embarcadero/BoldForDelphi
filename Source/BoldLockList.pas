{ Global compiler directives }
{$include bold.inc}
unit BoldLockList;

interface

uses
  BoldIndexList,
  BoldIndexedList,
  BoldListNodes,
  BoldIndexableList,
  BoldGuard,
  BoldHashIndexes,
  BoldDefs,
  SysUtils,
  BoldUtils,
  BoldLockingDefs,
  Classes;

type
  {forward declarations}
  TBoldLockNameIndexNode = class;
  TBoldLockNameList = class;
  TBoldLockList = class;
  TBoldLockNode = class;

  TBoldLockType = (bltShared, bltExclusive);

  TBoldLockIndex = class(TBoldStringHashIndex)
  protected
    function ItemAsKeyString(Item: TObject):string; override;
  end;

  TBoldLockNameHashList = class(TBoldUnorderedIndexableList)
  private
    function GetItembyLockName(LockName: string): TBoldLockNameIndexNode;
  public
    constructor Create;
    procedure Add(Item: TBoldLockNameIndexNode);
    property ItemsByLockName[LockName: string]: TBoldLockNameIndexNode read GetItembyLockName;
  end;

  TBoldClientIdIndex = class(TBoldStringHashIndex)
  protected
    function ItemAsKeyString(Item: TObject): string; override;
  end;

  TBoldClientIdHashList = class(TBoldIndexableList)
  private
    fOwnerIndexNode: TBoldLockNameIndexNode;
    function GetItembyClientId(ClientId: TBoldClientID): TBoldLockNode;
  public
    constructor Create(OwnerIndexNode: TBoldLockNameIndexNode);
    property ItemsbyClientID[ClientId: TBoldClientID]: TBoldLockNode read GetItembyClientId ;
  end;

  TBoldLockNameIndexNode = class(TBoldIndexNode)
  private
    fLockName: string;
    fNoSharedLocks: integer;
    fExclusiveLock: Boolean;
    fClients: TBoldClientIdHashList;
    function getClients: TBoldClientIdHashList;
  public
    destructor Destroy; override;
    property LockName: string read fLockName write fLockName;
    property NoSharedLocks: integer read fNoSharedLocks write fNoSharedLocks;
    property ExclusiveLock: Boolean read fExclusiveLock write fExclusiveLock;
    property Clients: TBoldClientIdHashList read getClients;
  end;

  TBoldLockNameList = class(TBoldIndexList)
  private
    fLocks: TBoldLockNameHashList;
  protected
    function GetItem(Key: variant): TBoldIndexNode; override;
    procedure SetItem(Key: variant; Value: TBoldIndexNode); override;
    function GetCount: integer; override;
    function InsertINode(Key: variant): TBoldIndexNode; override;
  public
    constructor Create(const IndexOrder: integer); override;
    destructor Destroy; override;
    procedure DeleteNodes; override;
    procedure InsertNode(Key: variant; NewNode: TBoldAbstractLinkNode); override;
    procedure DeleteINodeByKey(Key: variant); override;
  end;

  TBoldLockNode = class(TBoldAbstractLinkNode)
  private
    fClientId: TBoldClientId;
    fTimeOut: integer;
    fLockAcquisitionTime: TTimeStamp;
    fCanTimeOut: Boolean;
    fLocktype: TBoldLockType;
    fOwnerList: TBoldClientIdHashList;
    fNextNodes,
    fPreviousNodes: array [0..1] of TBoldMultiIndexedListNode;
    procedure setLockType(const Value: TBoldLockType);
    function GetHasTimedOut: Boolean;
  protected
    function GetNext(Index: integer): TBoldMultiIndexedListNode; override;
    procedure SetNext(Index: integer; Value: TBoldMultiIndexedListNode); override;
    function GetPrevious(Index: integer): TBoldMultiIndexedListNode; override;
    procedure SetPrevious(Index: integer; Value: TBoldMultiIndexedListNode); override;
    function GetNumberOfIndices: integer; override;
  public
    constructor Create;
    procedure Remove; override;
    procedure AddToList(OwnerList: TBoldClientIdHashList);
    procedure EnsureLock(Ensure: Boolean);
    function GetLockDuration(const CurrentTime: TTimeStamp): comp;
    property ClientId: TBoldClientId read fClientId write fClientId;
    property TimeOut: Integer read fTimeOut write fTimeOut;
    property LockAcquisitionTime: TTimeStamp read fLockAcquisitionTime write fLockAcquisitionTime;
    property LockType: TBoldLockType read fLockType write setLockType;
    property CanTimeOut: Boolean read fCanTimeOut;
    property HasTimedOut: Boolean read GetHasTimedOut;
  end;

  TBoldLockList = class(TBoldAbstractMultiIndexedList)
  private
    fClientIDList: TBoldClientIDList;
    fLockNameList: TBoldLockNameList;
    fClientIDIndexOrder: integer;
    fLockNameIndexOrder: integer;
    function GetClient(Index: TBoldClientId): TBoldIndexNode;
    function GetLock(Index: string): TBoldLockNameIndexNode;
    function GetItem(ClientId: TBoldClientID; LockName: string): TBoldLockNode;
  protected
    function GetIndexList(Index: integer): TBoldIndexList; override;
    procedure SetIndexList(Index: integer; const Value: TBoldIndexList); override;
  public
    constructor Create;
    destructor Destroy; override;
    function AddLock(const ClientId: TBoldClientID; const TimeOut: integer; const CurrentTime: TTimeStamp;
              const LockName: string; const LockType: TBoldLockType): TBoldLockNode;
    procedure RemoveClient(const ClientID: TBoldClientID);
    procedure RemoveLock(const LockName: string);
    procedure RemoveLockForClient(const ClientID: TBoldClientID; const LockName: string);
    procedure GetLocksByClientId(const ClientId: TBoldClientID; const locks: TStringList);
    procedure GetClientIDsbyLock(const LockName: string; const ClientIDs: TList);
    property Clients[Index: TBoldClientId]: TBoldIndexNode read getClient;
    property Locks[Index: string]: TBoldLockNameIndexNode read getLock;
    property Items[ClientId: TBoldClientID; LockName: string]: TBoldLockNode read getItem; default;
    property ClientIdIndexOrder: integer read fClientIdIndexOrder;
    property LockNameIndexOrder: integer read fLockNameIndexOrder;    
  end;

implementation

var
  IX_LockName: integer = -1;
  IX_ClientID: integer = -1;

{ TBoldLockNameIndex }

function TBoldLockIndex.ItemAsKeyString(Item: TObject): string;
begin
  assert(item is TBoldLockNameIndexNode, 'Element is not of type TBoldLockNameIndexNode');
  Result := TBoldLockNameIndexNode(Item).LockName;
end;

{ TBoldLockNameHashList }

procedure TBoldLockNameHashList.Add(Item: TBoldLockNameIndexNode);
begin
  inherited Add(Item);
end;

constructor TBoldLockNameHashList.Create;
begin
  inherited;
  SetIndexCapacity(1);
  SetIndexVariable(IX_LockName, AddIndex(TBoldLockIndex.Create));
end;

{
function TBoldLockNameHashList.GetItem(Index: Integer): TBoldLockNameIndexNode;
begin
  Result := TBoldLockNameIndexNode(inherited Items[index]);
end;
}

function TBoldLockNameHashList.GetItembyLockName(
  LockName: string): TBoldLockNameIndexNode;
begin
  Result := TBoldLockNameIndexNode(TBoldLockIndex(Indexes[IX_LockName]).FindByString(LockName));
end;

  { TBoldLockList }

function TBoldLockList.AddLock(const ClientId: TBoldClientID; const TimeOut: integer; const CurrentTime: TTimeStamp;
  const LockName: string; const LockType: TBoldLockType): TBoldLockNode;
var
  Keys: array [0..1] of variant;
begin
  Result := getItem(ClientId, LockName);
  if Assigned(Result) then
  begin
    if (LockType = bltExclusive) and (Result.LockType = bltShared) then
    begin
      Result.LockType := bltExclusive;
      Result.LockAcquisitionTime := CurrentTime;
    end;
  end
  else
  begin
    Keys[0] := ClientId;
    Keys[1] := LockName;
    Result := TBoldLockNode.Create;
    Result.ClientId := ClientId;
    Result.LockAcquisitionTime := CurrentTime;
    Result.TimeOut := TimeOut;
    Result.LockType := LockType;
    AddNode(Keys, Result);
  end;
end;

constructor TBoldLockList.Create;
begin
  inherited Create;
  fClientIDIndexOrder := AddIndex(TBoldClientIDList);
  fLockNameIndexOrder := AddIndex(TBoldLockNameList);
end;

destructor TBoldLockList.Destroy;
begin
  FreeAndNil(fClientIdList);
  FreeAndNil(fLockNameList);
  inherited;
end;

function TBoldLockList.getClient(Index: TBoldClientId): TBoldIndexNode;
begin
  Result := fClientIdList[Index];
end;


procedure TBoldLockList.GetClientIDsbyLock(const LockName: string;
  const ClientIDs: TList);
var
  aNode: TBoldLockNode;
  LockNode : TBoldLockNameIndexNode;
begin
  if Assigned(ClientIds) then
  begin
    LockNode := getLock(LockName);
    if Assigned(LockNode) then
    begin
      aNode := LockNode.Next as TBoldLockNode;
      while Assigned(aNode) do
      begin
        ClientIds.Add(Pointer(aNode.ClientID));
        aNode := aNode.Next[LockNameIndexOrder] as TBoldLockNode;
      end;
    end;
  end;
end;

function TBoldLockList.GetIndexList(Index: integer): TBoldIndexList;
begin
  if Index = 0 then
    Result := fClientIDList
  else if Index = 1 then
    Result := fLockNameList
  else
    Result := nil;
end;

function TBoldLockList.GetItem(ClientId: TBoldClientId; LockName: string): TBoldLockNode;
var
  LockNode: TBoldLockNameIndexNode;
begin
  Result := nil;
  LockNode := getLock(LockName);
  if Assigned(LockNode) then
    Result := LockNode.Clients.ItemsbyClientID[ClientId];
end;

function TBoldLockList.GetLock(Index: string): TBoldLockNameIndexNode;
var
  iNode: TBoldIndexNode;
begin
  iNode := fLockNameList[Index];
  if Assigned(iNode) then
    Result := iNode as TBoldLockNameIndexNode
  else
    Result := nil;
end;

procedure TBoldLockList.GetLocksByClientId(const ClientId: TBoldClientID;
  const locks: TStringList);
var
  aNode: TBoldLockNode;
  iNode: TBoldMultiIndexedListNode;
begin
  if Assigned(locks) then
  begin
    aNode := getClient(ClientID).Next as TBoldLockNode;
    while Assigned(aNode) do
    begin
      iNode := aNode.Previous[LockNameIndexOrder];
      while not (iNode is TBoldIndexNode) do
        iNode := (iNode as TBoldLockNode).Previous[LockNameIndexOrder];
      locks.Add((iNode as TBoldLockNameIndexNode).LockName);
      aNode := aNode.Next[ClientIdIndexOrder] as TBoldLockNode;
    end;
  end;
end;

procedure TBoldLockList.RemoveClient(const ClientID: TBoldClientID);
begin
  RemoveKey(ClientIDIndexOrder, ClientID);
end;

procedure TBoldLockList.RemoveLock(const LockName: string);
begin
  RemoveKey(LockNameIndexOrder, LockName);
end;

procedure TBoldLockList.RemoveLockForClient(const ClientID: TBoldClientID;
  const LockName: string);
var
  CurrentNode: TBoldLockNode;
begin
  CurrentNode := Items[ClientID, LockName];
  if Assigned(CurrentNode) then
  begin
    CurrentNode.Remove;
    FreeAndNil(CurrentNode);
  end;
end;

procedure TBoldLockList.SetIndexList(Index: integer;
  const Value: TBoldIndexList);
begin
  if Index = 0 then
    fClientIDList := Value as TBoldClientIDList
  else if Index = 1 then
    fLockNameList := Value as TBoldLockNameList;
end;

{ TBoldLockNameList }

constructor TBoldLockNameList.Create(const IndexOrder: integer);
begin
  inherited;
  fLocks := TBoldLockNameHashList.Create;
end;

procedure TBoldLockNameList.DeleteINodeByKey(Key: variant);
var
  node: TObject;
begin
 inherited;
 node := fLocks.GetItemByLockName(string(Key));
 if Assigned(node) then
   fLocks.Remove(node);
end;

procedure TBoldLockNameList.DeleteNodes;
var
  Traverser: TBoldIndexableListTraverser;
  Guard: IBoldGuard;
begin
  Guard := TBoldGuard.Create(Traverser);
  Traverser := fLocks.CreateTraverser;
  while Traverser.MoveNext do
  begin
    if Traverser.Item is TBoldIndexNode then
      DeleteINode(Traverser.Item as TBoldIndexNode);
  end;
end;

destructor TBoldLockNameList.Destroy;
begin
  DeleteNodes;
  FreeAndNil(fLocks);
  inherited;
end;

function TBoldLockNameList.GetCount: integer;
begin
  Result := fLocks.Count;
end;

{
function TBoldLockNameList.GetINode(Index: integer): TBoldIndexNode;
begin
  Result := fLocks.Items[Index];
end;
}
function TBoldLockNameList.GetItem(Key: variant): TBoldIndexNode;
begin
  Result := fLocks.GetItembyLockName(string(Key)) ;
end;

function TBoldLockNameList.InsertINode(Key: variant): TBoldIndexNode;
var
  iNode: TBoldLockNameIndexNode;
  aKey : string;
begin
  aKey := string(Key);
  if not Assigned(fLocks.ItemsByLockName[aKey]) then
  begin
    iNode := TBoldLockNameIndexNode.Create;
    with iNode do
    begin
      LockName := aKey;
      Next := nil;
      ExclusiveLock := false;
      NoSharedLocks := 0;
    end;
    fLocks.Add(iNode);
  end;
  Result := fLocks.ItemsByLockName[aKey];
end;

procedure TBoldLockNameList.InsertNode(Key: variant;
  NewNode: TBoldAbstractLinkNode);
var
  iNode: TBoldLockNameIndexNode;
  CurrentNode, NewLockNode: TBoldLockNode;
begin
  NewLockNode := NewNode as TBoldLockNode;
  iNode := Items[Key] as TBoldLockNameIndexNode;
  if not Assigned(iNode) then
    iNode := InsertINode(Key) as TBoldLockNameIndexNode;
  CurrentNode := iNode.Clients.GetItembyClientId(NewLockNode.ClientId);
  if Assigned(CurrentNode) then
  begin
    CurrentNode.Remove;
    FreeAndNil(CurrentNode);
  end
  else
  begin
    NewNode.Previous[IndexOrder] := iNode;
    NewNode.Next[IndexOrder] := iNode.Next;
    if Assigned(iNode.Next) then
      iNode.Next.Previous[IndexOrder] := NewNode;
    iNode.Next := NewNode;
    NewLockNode.AddToList(iNode.Clients);
  end;
end;

procedure TBoldLockNameList.SetItem(Key: variant;
  Value: TBoldIndexNode);
begin
  Assert(Value is TBoldLockNameIndexNode);
  fLocks.Add(Value as TBoldLockNameIndexNode);
end;

{ TBoldLockNode }

procedure TBoldLockNode.AddToList(OwnerList: TBoldClientIdHashList);
begin
  Assert(Assigned(OwnerList));
  fOwnerList := OwnerList;
  fOwnerList.Add(self);
  case LockType of
    bltShared:
      Inc(fOwnerList.fOwnerIndexNode.fNoSharedLocks);
    bltExclusive:
      fOwnerList.fOwnerIndexNode.ExclusiveLock := True;
  end;
end;

constructor TBoldLockNode.Create;
begin
  inherited;
  fCanTimeOut := LOCKS_CAN_TIMEOUT;
end;

procedure TBoldLockNode.EnsureLock(Ensure: Boolean);
begin
  fCanTimeOut := not Ensure;
end;

function TBoldLockNode.GetHasTimedOut: Boolean;
var
  CurrentTime: TTimeStamp;
  LockAcquisitionDuration: comp;
begin
  Result := fCanTimeOut;
  if Result then
  begin
    CurrentTime := DateTimetoTimeStamp(Now);
    LockAcquisitionDuration := TimeStampToMSecs(CurrentTime) - TimeStampToMSecs(LockAcquisitionTime);
    Result := (Int(LockAcquisitionDuration) >= TimeOut);
  end;  
end;

function TBoldLockNode.GetLockDuration(const CurrentTime: TTimeStamp): comp;
begin
  Result := TimeStampToMSecs(CurrentTime) - TimeStampToMSecs(LockAcquisitionTime);
end;

function TBoldLockNode.GetNext(Index: integer): TBoldMultiIndexedListNode;
begin
  Assert((Index >= 0) and (Index < NumberOfIndices));
  Result := fNextNodes[index];
end;

function TBoldLockNode.GetNumberOfIndices: integer;
begin
  Result := 2;
end;

function TBoldLockNode.GetPrevious(
  Index: integer): TBoldMultiIndexedListNode;
begin
  Assert((Index >= 0) and (Index < NumberOfIndices));
  Result := fPreviousNodes[Index];
end;

procedure TBoldLockNode.Remove;
begin
  if Assigned(fOwnerlist) then
    fOwnerList.Remove(self);
  case Locktype of
    bltShared: Dec(fOwnerList.fOwnerIndexNode.fNoSharedLocks);
    bltExclusive: fOwnerList.fOwnerIndexNode.ExclusiveLock := false;
  end;
  inherited;
end;

procedure TBoldLockNode.SetLockType(const Value: TBoldLockType);
var
  LockNameIndexNode: TBoldLockNameIndexNode;
begin
  if (fLockType <> Value) then
  begin
    fLockType := Value;
    if Assigned(IndexNode[1]) then
    begin
      LockNameIndexNode := IndexNode[1] as TBoldLockNameIndexNode;
      case fLockType of
        bltShared:
          begin
            Inc(LockNameIndexNode.fNoSharedLocks);
            LockNameIndexNode.ExclusiveLock := false;
          end;
        bltExclusive:
          begin
            LockNameIndexNode.ExclusiveLock := True;
            Dec(LockNameIndexNode.fNoSharedLocks);
          end;
      end;
    end;
  end;
end;

procedure TBoldLockNode.SetNext(Index: integer;
  Value: TBoldMultiIndexedListNode);
begin
  fNextNodes[Index] := Value;
end;

procedure TBoldLockNode.setPrevious(Index: integer;
  Value: TBoldMultiIndexedListNode);
begin
  fPreviousNodes[Index] := Value;
end;

{ TBoldClientIndex }

function TBoldClientIdIndex.ItemAsKeyString(Item: TObject): string;
begin
  assert(item is TBoldLockNode, 'Element is not of type TBoldLockNode');
  Result := IntToStr(TBoldLockNode(Item).ClientId);
end;

{ TBoldClientIdHashList }

constructor TBoldClientIdHashList.Create(OwnerIndexNode: TBoldLockNameIndexNode);
begin
  OwnsEntries := false;
  SetIndexCapacity(1);
  SetIndexVariable(IX_ClientID, AddIndex(TBoldClientIDIndex.Create));
  fOwnerIndexNode := OwnerIndexNode;
end;

function TBoldClientIdHashList.GetItembyClientId(
  ClientId: TBoldClientID): TBoldLockNode;
begin
  Result := TBoldLockNode(TBoldClientIdIndex(Indexes[IX_ClientID]).FindByString(IntToStr(ClientId)));
end;

{ TBoldLockNameIndexNode }

destructor TBoldLockNameIndexNode.Destroy;
begin
  FreeAndNil(fClients);
  inherited;
end;

function TBoldLockNameIndexNode.GetClients: TBoldClientIdHashList;
begin
  if not Assigned(fClients) then
    fClients := TBoldClientIdHashList.Create(self);
  Result := fClients;
end;

end.
