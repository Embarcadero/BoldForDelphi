
{ Global compiler directives }
{$include bold.inc}
unit BoldIndexedList;

interface

uses
  Variants,
  BoldIndexList,
  BoldListNodes,
  BoldHashIndexes,
  BoldIndexableList,
  BoldGuard,
  BoldContainers
  ;

const
  Initial_Client_Count = 1000;
  ClientIdArrayInc = 100;

type

  {forward declarations}
  TBoldClientIDList = class;
  TBoldEventNameList = class;
  TBoldExternalEventHashTable = class;
  TBoldExternalEventIndex = class;

  TBoldClientIDList = class(TBoldIndexList)
  private
    fClients: TBoldObjectArray;
  protected
    function GetItem(Key: variant): TBoldIndexNode; override;
    procedure SetItem(Key: variant; Value: TBoldIndexNode); override;
    function GetCount: integer; override;
    function InsertINode(Key: variant): TBoldIndexNode; override;
    function GetClient(Index: Integer): TObject;
    procedure SetClient(Index: Integer; Value: TObject);
    property Clients[Index: Integer]: TObject read getClient write setClient;
  public
    constructor Create(const IndexOrder: integer); override;
    destructor Destroy; override;
    procedure DeleteNodes; override;
  end;

  TBoldExternalEventIndex = class(TBoldStringHashIndex)
  protected
    function ItemAsKeyString(Item: TObject): string; override;
  end;

  TBoldExternalEventHashTable = class(TBoldUnorderedIndexableList)
  private
    class var IX_EventName: integer;
    function GetItemByEventName(EventName: string): TBoldEventNameIndexNode;
  public
    constructor Create;
    procedure Add(Item: TBoldEventNameIndexNode);
    property ItemsByEventName[ExpressionName: string]: TBoldEventNameIndexNode read GetItemByEventName;
  end;

  TBoldEventNameList = class(TBoldIndexList)
  private
    fEvents: TBoldExternalEventHashTable;
  protected
    function GetItem(Key: variant): TBoldIndexNode; override;
    procedure SetItem(Key: variant; Value: TBoldIndexNode); override;
    function GetCount: integer; override;
    function InsertINode(Key: variant): TBoldIndexNode; override;
  public
    constructor Create(const IndexOrder: integer); override;
    destructor Destroy; override;
    procedure DeleteNodes; override;
    procedure DeleteINodeByKey(Key: variant); override;
  end;

implementation

uses
  SysUtils,
  BoldUtils,
  dialogs,
  BoldPropagatorSubscriptions,
  BoldDefs;


{TBoldClientIDList}
constructor TBoldClientIDList.Create;
begin
  inherited Create(IndexOrder);
  fClients := TBoldObjectArray.Create(Initial_Client_Count, [bcoDataOwner, bcoThreadSafe]);
end;

procedure TBoldClientIDList.DeleteNodes;
var
  i: integer;
begin
  for i := 0 to Count-1 do
    if fClients[i] is TBoldIndexNode then
      DeleteINode(fClients[i] as TBoldIndexNode);
end;

destructor TBoldClientIDList.Destroy;
begin
  DeleteNodes;
  FreeAndNil(fClients);
  inherited;
end;

function TBoldClientIDList.GetClient(Index: Integer): TObject;
var
  cnt, increment, i: integer;
begin
  if (Index < fClients.Count) then
    Result := fClients[Index]
  else
    begin
      cnt := (Index - fClients.Count) + 1;
      increment := ClientIDArrayInc;
      while (cnt > increment) do
        Inc(increment, ClientIDArrayInc);
      for i:= 0 to increment do
        fClients.Add(nil);
      Result := fClients[Index]
    end;
end;

function TBoldClientIDList.GetCount: integer;
begin
  Result := fClients.Count;
end;

function TBoldClientIDList.GetItem(Key: variant): TBoldIndexNode;
var
  aKey: integer;
begin
  Assert(VarType(Key) = varInteger, Format('%s.getItem: Key must be of type TBoldIntegerKey', [ClassName]));
  aKey := Integer(Key);
  if not Assigned(Clients[aKey]) then
    fClients[aKey] := TBoldIndexNode.Create;
  Result := Clients[aKey] as TBoldIndexNode ;
end;

function TBoldClientIDList.InsertINode(Key: variant): TBoldIndexNode;
var
  aKey: integer;
begin
  aKey := Integer(Key);
  if not Assigned(Clients[aKey]) then
    Clients[aKey] := TBoldIndexNode.Create;
  Result := Clients[aKey] as TBoldIndexNode;
end;

procedure TBoldClientIDList.SetClient(Index: Integer;
  Value: TObject);
begin
  Clients[Index] := Value;
end;

procedure TBoldClientIDList.SetItem(Key: variant; Value: TBoldIndexNode);
var
  aKey: integer;
begin
  Assert(VarType(Key) = varInteger, Format('%s.setItem: Key must be of type TBoldIntegerKey', [ClassName]));
  aKey := Integer(Key);
  if not Assigned(Clients[aKey]) then
    Clients[aKey] := Value
  else
    (Clients[aKey] as TBoldIndexNode).Next := Value.Next;
end;

  {TBoldExternalEventIndex}
function  TBoldExternalEventIndex.ItemAsKeyString(Item: TObject): string;
begin
  assert(item is TBoldEventNameIndexNode, 'Element is not of type TBoldEventNameIndexNode');
  Result := TBoldEventNameIndexNode(Item).EventName;
end;

  {TBoldExternalEventHashTable}
constructor TBoldExternalEventHashTable.Create;
begin
  inherited ;
  SetIndexCapacity(1);
  SetIndexVariable(IX_EventName, AddIndex(TBoldExternalEventIndex.Create));
end;

function TBoldExternalEventHashTable.GetItemByEventName(eventName: string): TBoldEventNameIndexNode;
begin
  Result := TBoldEventNameIndexNode(TBoldExternalEventIndex(Indexes[IX_EventName]).FindByString(EventName));
end;

procedure TBoldExternalEventHashTable.Add(Item: TBoldEventNameIndexNode);
begin
  inherited Add(Item);
end;

  {TBoldEventNameList}
constructor TBoldEventNameList.Create;
begin
  inherited Create(IndexOrder);
  fEvents := TBoldExternalEventHashTable.Create;
end;

destructor TBoldEventNameList.Destroy;
begin
  DeleteNodes;
  FreeAndNil(fEvents);
  inherited ;
end;

function TBoldEventNameList.GetCount: integer;
begin
  Result := fEvents.Count;
end;

function TBoldEventNameList.GetItem(Key: variant): TBoldIndexNode;
begin
  Assert(VarIsStr(Key), Format('%s.getItem: Key is not TBoldStringKey', [ClassName]));
  Result := fEvents.GetItemByEventName(string(Key));
end;

function TBoldEventNameList.InsertINode(Key: variant): TBoldIndexNode;
var
  iNode: TBoldEVentNameIndexNode;
  aKey : string;
begin
  aKey := string(Key);
  if not Assigned(fEvents.ItemsByEventName[aKey]) then
  begin
    iNode := TBoldEventNameIndexNode.Create;
    iNode.EventName := aKey;
    iNode.Next := nil;
    fEvents.Add(iNode);
  end;
  Result := fEvents.ItemsByEventName[aKey];
end;

procedure TBoldEventNameList.DeleteINodeByKey(Key: variant);
var
  node: TObject;
begin
  inherited;
  node := fEvents.GetItemByEventName(string(Key));
  if Assigned(node) then
    fEvents.Remove(node);
end;

procedure TBoldEventNameList.SetItem(Key: variant; Value: TBoldIndexNode);
begin
  Assert(Value is TBoldEventNameIndexNode);
  fEvents.Add(Value as TBoldEventNameIndexNode);
end;

procedure TBoldEventNameList.DeleteNodes;
var
  Traverser: TBoldIndexableListTraverser;
  Guard: IBoldGuard;
begin
  Guard := TBoldGuard.Create(Traverser);
  Traverser := fEvents.CreateTraverser;
  Traverser.AutoMoveOnRemoveCurrent := false;
  while Traverser.MoveNext do
  begin
    if Traverser.Item is TBoldIndexNode then
      DeleteINode(Traverser.Item as TBoldIndexNode)
  end;
end;

initialization
  TBoldExternalEventHashTable.IX_EventName := -1;

end.
