unit BoldPropagatorSubscriptions;

interface

uses
  BoldListNodes,
  classes,
  BoldDefs,
  BoldIndexList,
  BoldIndexedList;

type
  {forward declarations}
  TBoldSubscriptionList = class;
  TBoldSubscriptionNode = class;

  TBoldSortedIntegerList = class(TList)
  public
    function Add(Value: Integer): integer;
  end;

  TBoldSubscriptionNode = class(TBoldAbstractLinkNode)
  private
    fClientID: TBoldClientID;
    fNextNodes,
    fPreviousNodes : array [0..1] of TBoldMultiIndexedListNode;
  protected
    function getNext(Index: integer): TBoldMultiIndexedListNode; override;
    procedure setNext(Index: integer; Value: TBoldMultiIndexedListNode); override;
    function getPrevious(Index: integer): TBoldMultiIndexedListNode; override;
    procedure setPrevious(Index: integer; Value: TBoldMultiIndexedListNode); override;
    function getNumberOfIndices: integer; override;
  public
    constructor Create(const ClientID: TBoldClientID);
    property ClientID: TBoldClientId read fClientID;
  end;

  TBoldSubscriptionList = class(TBoldAbstractMultiIndexedList)
  private
    fClientIDList: TBoldClientIDList;
    fEventNameList: TBoldEventNameList;
    ClientIDIndexOrder: integer;
    EventNameIndexOrder: integer;
    function getClient(Index: TBoldClientID): TBoldIndexNode;
    function getEvent(Index: string): TBoldEventNameIndexNode;
  protected
    function getIndexList(Index: integer): TBoldIndexList; override;
    procedure setIndexList(Index: integer; const Value: TBoldIndexList); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddSubscription(const ClientID: TBoldClientID; const EventName: string);
    procedure RemoveClient(const ClientId: TBoldClientId);
    procedure RemoveEvent(const EventName: string);
    procedure GetEventsByClientID(const ClientID: TBoldClientID; const events: TStringList);
    function GetEventCountByClientID(const ClientID: TBoldClientID): integer;
    procedure GetClientsSubscribedToEvent(const EventName: string; const ClientIds: TList);
    procedure GetAllSubscribedClientsExcept(const EventName: string; const BoldClientID: TBoldClientID;
      const ClientIds: TBoldSortedIntegerList);
    property Clients[Index: TBoldClientID]: TBoldIndexNode read getClient;
    property Events[Index: string]: TBoldEventNameIndexNode read getEvent;
  end;

implementation

uses
  Sysutils,
  BoldUtils,
  PropagatorConsts;

{ TBoldSortedIntegerList }

function TBoldSortedIntegerList.Add(Value: Integer): integer;
var
  High, Low, i: integer;
  Item: integer;
begin
  Result := -1;
  Low := 0;
  High := Count;
  while ((High - Low) > 1) do
  begin
    i := (High + Low) div 2;
    Item := Integer(Items[i]);
    if Value = Item then
      Exit
    else if Value < Item then
      High := i
    else
      Low := i;
  end;
  if (Count > 0) then
  begin
    if Integer(Items[Low]) < Value then
    begin
      Insert(Low + 1, Pointer(Value));
      Result := Low + 1;
    end
    else if Integer(Items[Low]) > Value then
    begin
      Insert(Low, Pointer(Value));
      Result := Low;
    end;
  end
  else
    Result := inherited Add(Pointer(Value));
end;

  {TBoldSubscriptions}
constructor TBoldSubscriptionList.Create;
begin
  inherited Create;
  ClientIDIndexOrder := AddIndex(TBoldClientIDList);
  EventNameIndexOrder := AddIndex(TBoldEventNameList);
end;

procedure TBoldSubscriptionList.AddSubscription(const ClientID: TBoldClientID; const EventName: string);
var
  Keys: array [0..1] of variant;
begin
  Keys[0] := ClientId;
  Keys[1] := EventName;
  AddNode(Keys, TBoldSubscriptionNode.Create(ClientId));
end;

procedure TBoldSubscriptionList.RemoveClient(const ClientId: TBoldClientId);
begin
  RemoveKey(ClientIDIndexOrder, ClientID);
end;

procedure TBoldSubscriptionList.RemoveEvent(const EventName: string);
begin
  RemoveKey(EventNameIndexOrder, EventName);
end;

procedure TBoldSubscriptionList.GetEventsByClientID(const ClientID: TBoldClientID; const events: TStringList);
var
  aNode: TBoldSubscriptionNode;
  iNode: TBoldMultiIndexedListNode;
begin
  if Assigned(events) then
  begin
    aNode := getClient(ClientID).Next as TBoldSubscriptionNode;
    while Assigned(aNode) do
    begin
      iNode := aNode.Previous[EventNameIndexOrder];
      while not (iNode is TBoldIndexNode) and assigned(iNode) do
        iNode := (iNode as TBoldSubscriptionNode).Previous[EventNameIndexOrder];
      if assigned(iNode) then
        events.Add((iNode as TBoldEventNameIndexNode).EventName);
      aNode := aNode.Next[ClientIdIndexOrder] as TBoldSubscriptionNode;
    end;
  end;
end;

procedure TBoldSubscriptionList.GetClientsSubscribedToEvent(const EventName: string; const ClientIds: TList);
var
  aNode: TBoldSubscriptionNode;
  eventNode : TBoldEventNameIndexNode;
begin
  if Assigned(ClientIds) then
  begin
    eventNode := getEvent(EventName);
    if Assigned(eventNode) then
    begin
      aNode := eventNode.Next as TBoldSubscriptionNode;
      while Assigned(aNode) do
      begin
        ClientIds.Add(Pointer(aNode.ClientID));
        aNode := aNode.Next[EventNameIndexOrder] as TBoldSubscriptionNode;
      end;
    end;
  end;
end;

function TBoldSubscriptionList.getClient(
  Index: TBoldClientID): TBoldIndexNode;
begin
  Result := fClientIdList[Index];
end;

function TBoldSubscriptionList.getEvent(
  Index: string): TBoldEventNameIndexNode;
var
  iNode: TBoldIndexNode;
begin
  iNode := fEventNameList[Index];
  if Assigned(iNode) then
    Result := iNode as TBoldEventNameIndexNode
  else
    Result := nil;
end;

{ TBoldSubscriptionNode }

constructor TBoldSubscriptionNode.Create(const ClientID: TBoldClientID);
begin
  inherited Create;
  fClientID := ClientID;
end;

procedure TBoldSubscriptionList.GetAllSubscribedClientsExcept(const EventName: string;
  const BoldClientID: TBoldClientID; const ClientIds: TBoldSortedIntegerList);
var
  aNode: TBoldSubscriptionNode;
  aEvent: TBoldEventNameIndexNode;
begin
  if not Assigned(ClientIds) then
    raise EBold.CreateFmt(sClientIDsNotAssigned, [ClassName, 'GetAllSubscribedClientsExcept']); // do not localize
  aEvent := getEvent(EventName);
  if Assigned(aEvent) then
  begin
    aNode := getEvent(EventName).Next as TBoldSubscriptionNode;
    while Assigned(aNode) do
    begin
      if (aNode.ClientID <> BoldClientID) then
        ClientIds.Add(aNode.ClientID);
      aNode := aNode.Next[EventNameIndexOrder] as TBoldSubscriptionNode;
    end;
  end;
end;


function TBoldSubscriptionList.getIndexList(
  Index: integer): TBoldIndexList;
begin
  if Index = 0 then
    Result := fClientIDList
  else if Index = 1 then
    Result := fEventNameList
  else
    Result := nil;
end;

procedure TBoldSubscriptionList.setIndexList(Index: integer;
  const Value: TBoldIndexList);
begin
  if Index = 0 then
    fClientIDList := Value as TBoldClientIDList
  else if Index = 1 then
    fEventNameList := Value as TBoldEventNameList;
end;

destructor TBoldSubscriptionList.Destroy;
begin
  FreeAndNil(fClientIdList);
  FreeAndNil(fEventNameList);
  inherited;
end;

function TBoldSubscriptionNode.getNext(
  Index: integer): TBoldMultiIndexedListNode;
begin
  Assert((Index >= 0) and (Index < NumberOfIndices));
  Result := fNextNodes[Index];
end;

function TBoldSubscriptionNode.getNumberOfIndices: integer;
begin
  Result := 2;
end;

function TBoldSubscriptionNode.getPrevious(
  Index: integer): TBoldMultiIndexedListNode;
begin
  Assert((Index >= 0) and (Index < NumberOfIndices));
  Result := fPreviousNodes[Index];
end;

procedure TBoldSubscriptionNode.setNext(Index: integer;
  Value: TBoldMultiIndexedListNode);
begin
  fNextNodes[Index] := Value;
end;

procedure TBoldSubscriptionNode.setPrevious(Index: integer;
  Value: TBoldMultiIndexedListNode);
begin
  fPreviousNodes[Index] := Value;
end;

function TBoldSubscriptionList.GetEventCountByClientID(const ClientID: TBoldClientID): integer;
var
  aNode: TBoldSubscriptionNode;
  iNode: TBoldMultiIndexedListNode;
begin
  Result := 0;
  aNode := getClient(ClientID).Next as TBoldSubscriptionNode;
  while Assigned(aNode) do
  begin
    iNode := aNode.Previous[EventNameIndexOrder];
    while not (iNode is TBoldIndexNode) and assigned(iNode) do
      iNode := (iNode as TBoldSubscriptionNode).Previous[EventNameIndexOrder];
    if assigned(iNode) then
      inc(result);
    aNode := aNode.Next[ClientIdIndexOrder] as TBoldSubscriptionNode;
  end;
end;

end.
