
{ Global compiler directives }
{$include bold.inc}
unit BoldIndexList;

interface

uses
  BoldListNodes,
  BoldIndexableList,
  BoldGuard,
  BoldBase;

type
  {forward declarations}
  TBoldIndexList = class;
  TBoldAbstractMultiIndexedList = class;
  TBoldIndexListClass =  class of TBoldIndexList;

  TBoldIndexList = class
  private
    fIndexOrder: integer;
  protected
    function GetItem(Key: variant): TBoldIndexNode; virtual; abstract;
    procedure SetItem(Key: variant; Value: TBoldIndexNode); virtual; abstract;
    function GetCount: integer; virtual; abstract;
    function InsertINode(Key: variant): TBoldIndexNode; virtual; abstract;
    procedure DeleteINode(iNode: TBoldIndexNode);
  public
    constructor Create(const IndexOrder: integer); virtual;
    procedure InsertNode(Key: variant; NewNode: TBoldAbstractLinkNode); virtual;
    procedure DeleteINodeByKey(Key: variant); virtual;
    procedure DeleteNodes; virtual; abstract;
    procedure RemoveKey(Key: variant);
    property Items[Key: variant]: TBoldIndexNode read getItem write setItem; default;
    property Count: integer read getCount;
    property IndexOrder: integer read fIndexOrder;
  end;

  TBoldAbstractMultiIndexedList = class
  private
    fNumberOfIndices: integer;
  protected
    function GetIndexList(Index: integer): TBoldIndexList; virtual; abstract;
    procedure SetIndexList(Index: integer; const Value: TBoldIndexList); virtual; abstract;
    property Indices[Index: integer]: TBoldIndexList read getIndexList write setIndexList;
  public
    constructor Create;
    function AddIndex(IndexListClass: TBoldIndexListClass): integer;
    procedure AddNode(Key:array of variant; NewNode:TBoldAbstractLinkNode);
    procedure RemoveKey(IndexOrder:integer; Key: variant);
    property NumberOfIndices: integer read fNumberOfIndices;
  end;

implementation

uses
  SysUtils,
  BoldUtils;

{ TBoldIndexList }

procedure TBoldIndexList.InsertNode(Key: variant; NewNode: TBoldAbstractLinkNode);
var
  iNode: TBoldIndexNode;
begin
  iNode := Items[Key];
  if not Assigned(iNode) then
    iNode := InsertINode(Key);
  NewNode.Previous[IndexOrder] := iNode;
  NewNode.Next[IndexOrder] := iNode.Next;
  if Assigned(iNode.Next) then
    iNode.Next.Previous[IndexOrder] := NewNode;
  iNode.Next := NewNode;
end;


constructor TBoldIndexList.Create(const IndexOrder: integer);
begin
  inherited Create;
  fIndexOrder := IndexOrder
end;

procedure TBoldIndexList.DeleteINodeByKey(Key: variant);
begin
end;

procedure TBoldIndexList.RemoveKey(Key: variant);
var
  iNode: TBoldIndexNode;
  aNode: TBoldAbstractLinkNode;
begin
  iNode := Items[Key];
  while (Assigned(iNode) and Assigned(iNode.Next)) do
  begin
    aNode := iNode.Next;
    aNode.Remove;
    FreeAndNil(aNode);
  end;
  if Assigned(iNode) then
  begin
    iNode.next := nil;
    DeleteINodeByKey(Key);
  end;
end;

procedure TBoldIndexList.DeleteINode(iNode: TBoldIndexNode);
var
  aNode, nextNode: TBoldAbstractLinkNode;
begin
  if Assigned(iNode.Next) then
  begin
    aNode := iNode.Next;
    while Assigned(aNode.Next[IndexOrder]) do
    begin
      nextNode := TBoldAbstractLinkNode(aNode.Next[IndexOrder]);
      aNode.Remove;
      FreeAndNil(aNode);
      aNode := nextNode;
    end;
    aNode.Remove;
    FreeAndNil(aNode);
    iNode.Next := nil;
  end;
end;

{ TBoldAbstractMultiIndexedList }

function TBoldAbstractMultiIndexedList.AddIndex(
  IndexListClass: TBoldIndexListClass): integer;
begin
  Indices[fNumberOfIndices] := IndexListClass.Create(fNumberOfIndices);
  Result := fNumberOfIndices;
  Inc(fNumberOfIndices);
end;

procedure TBoldAbstractMultiIndexedList.AddNode(
  Key: array of variant; NewNode: TBoldAbstractLinkNode);
var
  i: integer;
begin
  for i:= 0 to NumberOfIndices - 1 do
  begin
    Indices[i].InsertNode(Key[i], NewNode);
  end;
end;

constructor TBoldAbstractMultiIndexedList.Create;
begin
  inherited;
  fNumberOfIndices := 0;
end;

procedure TBoldAbstractMultiIndexedList.RemoveKey(IndexOrder: integer;
  Key: variant);
begin
  Indices[IndexOrder].RemoveKey(Key);
end;

end.
