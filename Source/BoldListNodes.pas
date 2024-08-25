{ Global compiler directives }
{$include bold.inc}
unit BoldListNodes;

interface

type
  TBoldMultiIndexedListNode = class;
  TBoldAbstractLinkNode = class;
  TBoldIndexNode = class;

  TBoldMultiIndexedListNode = class
  end;

  TBoldIndexNode = class(TBoldMultiIndexedListNode)
  private
    fNext: TBoldAbstractLinkNode;
  public
    constructor Create;
    property Next: TBoldAbstractLinkNode read fNext write fNext;
  end;

  TBoldEventNameIndexNode = class(TBoldIndexNode)
  private
    fEventName: string;
  public
    property EventName: string read fEventName write fEventName;
  end;

  TBoldAbstractLinkNode = class(TBoldMultiIndexedListNode)
  protected
    function GetNext(Index: integer): TBoldMultiIndexedListNode; virtual; abstract;
    procedure SetNext(Index: integer; Value: TBoldMultiIndexedListNode); virtual; abstract;
    function GetPrevious(Index: integer): TBoldMultiIndexedListNode; virtual; abstract;
    procedure SetPrevious(Index: integer; Value: TBoldMultiIndexedListNode); virtual; abstract;
    function GetIndexNode(Index: integer): TBoldIndexNode; virtual;
    function GetNumberOfIndices: integer; virtual; abstract;
  public
    procedure InsertAfter(const Index: integer; NewNode: TBoldAbstractLinkNode);
    procedure InsertBefore(const Index: integer; NewNode: TBoldAbstractLinkNode);
    procedure Remove; virtual;
    property Next[Index: integer]: TBoldMultiIndexedListNode read getNext write setNext;
    property Previous[Index: integer]: TBoldMultiIndexedListNode read getPrevious write setPrevious;
    property IndexNode[Index: integer]: TBoldIndexNode read getIndexNode;
    property NumberOfIndices: integer read getNumberOfIndices;
  end;

implementation

uses

  SysUtils,
  BoldUtils,
  BoldDefs;

  {TBoldIndexNode}
constructor TBoldIndexNode.Create;
begin
  inherited Create;
  fNext := nil;
end;

  {TBoldAbstractLinkNode}
function TBoldAbstractLinkNode.getIndexNode(Index: integer): TBoldIndexNode;
var
  aNode: TBoldMultiIndexedListNode;
begin
  aNode := self;
  while (Assigned(aNode) and (aNode is TBoldAbstractLinkNode)) do
  begin
    aNode := (aNode as TBoldAbstractLinkNode).Previous[Index];
  end;
  if Assigned(aNode) then
    Assert(aNode is TBoldIndexNode,Format('%s.geIndexNode: aNode is not a TBoldIndexNode', [ClassName]));
  Result := (aNode as TBoldIndexNode);
end;

procedure TBoldAbstractLinkNode.InsertAfter(const Index: integer; NewNode: TBoldAbstractLinkNode);
begin
  NewNode.Previous[Index] := self;
  NewNode.Next[Index] := Next[Index];
  (Next[Index] as TBoldAbstractLinkNode).Previous[Index] := NewNode;
  Next[Index] := NewNode;
end;

procedure TBoldAbstractLinkNode.InsertBefore(const Index: integer; NewNode: TBoldAbstractLinkNode);
begin
  NewNode.Next[Index] := self;
  NewNode.Previous[Index] := Previous[Index];
  if (Previous[Index] is TBoldAbstractLinkNode) then
    (Previous[index] as TBoldAbstractLinkNode).Next[Index] := NewNode
  else
    (Previous[Index] as TBoldIndexNode).Next := NewNode;
  Previous[Index] := NewNode;
end;

procedure TBoldAbstractLinkNode.Remove;
var
  Index: integer;
  Prev: TBoldMultiIndexedListNode;
begin
  for Index := 0 to NumberOfIndices - 1 do
  begin
    if Assigned(Next[Index]) then
      (Next[Index] as TBoldAbstractLinkNode).Previous[Index] := Previous[Index];
    Prev := Previous[Index];
    if Assigned(Prev) then
      if (Prev is TBoldAbstractLinkNode) then
        (Prev as TBoldAbstractLinkNode).Next[Index] := Next[Index]
      else if (Prev is TBoldIndexNode) then
        (Prev as TBoldIndexNode).Next := TBoldAbstractLinkNode(Next[Index]);
    Next[Index] := nil;
    Previous[Index] := nil;
  end;
end;

end.
