
{ Global compiler directives }
{$include bold.inc}
unit BoldIndexableList;

interface

uses
  Classes,
  BoldBase,
  BoldIndex;

type
  {---Forward declaration---}
  TBoldIndexableList = class;
  TBoldUnOrderedIndexableList = class;
  TBoldIndexableListTraverser = class;
  TBoldIndexableListTraverserClass = class of TBoldIndexableListTraverser;
  
  TBoldIndexableListOptions = (iloOwnsEntries, iloDestroying, iloKnowsSupportsNil, iloSupportsNil);
  TBoldIndexableListOptionsSet = set of TBoldIndexableListOptions;

  TBoldIndexableListTraverser = class(TBoldMemoryManagedObject)
  private
    fIndexTraverser: TBoldIndexTraverser;
    fCurrentIndex: integer;
    function GetAutoMoveOnRemoveCurrent: boolean;
    procedure SetAutoMoveOnRemoveCurrent(const Value: boolean);
  protected
    function GetItem: TObject;
  public
    constructor Create(IndexTraverser: TBoldIndexTraverser);
    destructor Destroy; override;
    function MoveNext: Boolean;
    property Item: TObject read GetItem;
    property CurrentIndex: integer read fCurrentIndex;
    property Current: TObject read GetItem;
    property AutoMoveOnRemoveCurrent: boolean read GetAutoMoveOnRemoveCurrent write SetAutoMoveOnRemoveCurrent;
  end;


  {---TBoldUnOrderedIndexableList---}
  TBoldUnOrderedIndexableList = class(TBoldNonRefCountedObject)
  private
    fIndexes: array of TBoldIndex;
    fOptions: TBoldIndexableListOptionsSet;
    fFirstNilSupportingIndex: Integer;
    function GetCount: Integer;
    function GetIndex(Index: Integer): TBoldIndex;
    procedure SetIndex(I: Integer; const Value: TBoldIndex);
    function GetAny: TObject;
    function GetOwnsEntries: boolean;
    procedure SetOwnsEntries(const Value: boolean);
    function FirstNilSupportingIndex: TBoldIndex;
    function FirstAssignedIndex: TBoldIndex;
    function GetKnowsSupportsNil: Boolean;
    function GetSupportsNil: Boolean;
    procedure SetKnowsSupportsNil(const Value: Boolean);
    procedure SetSupportsNil(const Value: Boolean);
    procedure CalculateSupportsNil;
    function GetAssignedIndexCount: integer;
    function GetIndexCount: integer;
    function GetCapacity: integer;
    procedure SetCapacity(const Value: integer);
    function GetIsEmpty: boolean;
  protected
    function GetDebugInfo: string; override;  
    function AddIndex(BoldIndex: TBoldIndex): integer;
    procedure AddToAllIndexes(Item: TObject);
    procedure RemoveAndFreeIndex(var BoldIndex: TBoldIndex; DestroyObjects: Boolean);
    function RemoveFromAllIndexes(Item: TObject): boolean;
    procedure SetIndexCapacity(NewCapacity: integer);
    function TraverserClass: TBoldIndexableListTraverserClass; virtual;
    property IndexCount: integer read GetIndexCount;
    property AssignedIndexCount: integer read GetAssignedIndexCount;
    property Indexes[I: Integer]: TBoldIndex read GetIndex write SetIndex;
    property KnowsSupportsNil: Boolean read GetKnowsSupportsNil write SetKnowsSupportsNil;
    property SupportsNil: Boolean read GetSupportsNil write SetSupportsNil;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Item: TObject);
    procedure Clear;
    procedure ItemChanged(Item: TObject);
    function Remove(Item: TObject): boolean;
    function CreateTraverser: TBoldIndexableListTraverser;
    function GetEnumerator: TBoldIndexableListTraverser;
    property Count: integer read GetCount;
    property Any: TObject read GetAny;
    property OwnsEntries: boolean read GetOwnsEntries write SetOwnsEntries;
    property Capacity: integer read GetCapacity write SetCapacity;
    property IsEmpty: boolean read GetIsEmpty;
  end;

  TBoldIndexableList = class(TBoldUnOrderedIndexableList)
  private
    fIndexIndex: TBoldIntegerIndex;
    function GetItem(Index: integer): TObject;
    procedure SetItem(Index: integer; value: TObject);
    procedure AddToAllNonOrderedIndexes(item: TObject);
    procedure RemoveFromAllNonOrderedIndexes(item: TObject);
    function GetUnorderedIndexCount: integer;
//    procedure _DebugInfo(Index: Integer);
  protected
    property Items[I: Integer]: TObject read GetItem write SetItem;
    property UnorderedIndexCount: integer read GetUnorderedIndexCount;
  public
    constructor Create;
    procedure Move(CurIndex, NewIndex: Integer);
    procedure RemoveByIndex(Index: Integer);
    procedure Sort(Compare: TIntegerIndexSortCompare);
    procedure Exchange(Index1, Index2: integer);
    function IndexOf(Item: TObject): integer;
    procedure Insert(Index: Integer; Item: TObject);
    function Includes(Item: TObject): boolean;
    procedure RemoveList(AList: TBoldIndexableList);
  end;


procedure SetIndexVariable(var Index: integer; GivenPosition: integer);

implementation

uses
  SysUtils,

  BoldCoreConsts,
  BoldDefs;

procedure SetIndexVariable(var Index: integer; GivenPosition: integer);
begin
  assert((index = -1) or (index = GivenPosition), 'Erroneous index construction, please debug!');
  Index := GivenPosition
end;

{ TBoldUnOrderedIndexableList }

function TBoldUnOrderedIndexableList.GetIndex(Index: Integer): TBoldIndex;
begin
  Result := FIndexes[Index]
end;

procedure TBoldUnOrderedIndexableList.SetSupportsNil(const Value: Boolean);
begin
  if value then
    Include(fOptions, iloSupportsNil)
  else
    Exclude(fOptions, iloSupportsNil);
end;

function TBoldUnOrderedIndexableList.GetKnowsSupportsNil: Boolean;
begin
  result := iloKnowsSupportsNil in fOptions;
end;

procedure TBoldUnOrderedIndexableList.SetKnowsSupportsNil(
  const Value: Boolean);
begin
  if value then
    Include(fOptions, iloKnowsSupportsNil)
  else
    Exclude(fOptions, iloKnowsSupportsNil);
end;

function TBoldUnOrderedIndexableList.GetIndexCount: integer;
begin
  Result := Length(fIndexes);
end;

function TBoldUnOrderedIndexableList.GetIsEmpty: boolean;
begin
  result := Count = 0;
end;

function TBoldUnOrderedIndexableList.FirstNilSupportingIndex: TBoldIndex;
var
  i: integer;
begin
  if fFirstNilSupportingIndex > -1 then begin
    result := Indexes[fFirstNilSupportingIndex];
  end else begin
    result := nil;
    for i := 0 to IndexCount-1 do
      if Assigned(Indexes[i]) and Indexes[i].SupportsNilItems then
      begin
        result := Indexes[i];
        fFirstNilSupportingIndex := i;
        break;
      end;
  end;
end;

procedure TBoldUnOrderedIndexableList.CalculateSupportsNil;
begin
  SupportsNil := FirstNilSupportingIndex <> nil;
  KnowsSupportsNil := true;
end;

function TBoldUnOrderedIndexableList.GetSupportsNil: Boolean;
begin
  if not knowsSupportsNil then
    CalculateSupportsNil;
  result := iloSupportsNil in fOptions;
end;

(*
procedure TBoldIndexableList._DebugInfo(Index: Integer);
var
  Info: string;
  I: Integer;
begin
  Info := Format('GetItem(%d): Self.Classname:%s Count:%d fIndexIndex.Count:%d SupportsNil:%s IndexCount:%d'#13#10, [Index, Classname, Count, fIndexIndex.Count, BoolToStr(SupportsNil, True), IndexCount]);
  for i := 0 to IndexCount-1 do
    if Assigned(Indexes[i]) then
      Info := Info+Format('Indexes[%d] Classname:%s Count:%d SupportsNilItems:%s '#13#10, [I, Indexes[i].ClassName, Indexes[i].Count, BoolToStr(Indexes[i].SupportsNilItems, True)])
    else
      Info := Info+Format('Indexes[%d] = nil '#13#10, [I]);
//  asm nop; end; //Breakpoint here!
end;
*)

function TBoldIndexableList.GetItem(Index: integer): TObject;
begin
  if (index < 0) or (index>=fIndexIndex.FastCount) then
    raise EBold.CreateFmt('%s.GetItem: Index %d out of bounds, count is %d', [classname, Index, fIndexIndex.FastCount]);//_DebugInfo(Index);
  Result := fIndexIndex.items[index];
end;

function TBoldUnOrderedIndexableList.GetOwnsEntries: boolean;
begin
  result := iloOwnsEntries in fOptions;
end;

procedure TBoldUnOrderedIndexableList.SetOwnsEntries(const Value: boolean);
begin
  if value then
    Include(fOptions, iloOwnsEntries)
  else
    Exclude(fOptions, iloOwnsEntries);
end;

constructor TBoldUnOrderedIndexableList.Create;
begin
  inherited;
  fFirstNilSupportingIndex := -1;  
  OwnsEntries := True;
  SupportsNil := false;
  KnowsSupportsNil := true;
end;

destructor TBoldUnOrderedIndexableList.Destroy;
var
  i: integer;
  BoldIndex: TBoldIndex;
  IndicesToGo: integer;
begin
  Include(fOptions, iloDestroying);

  IndicesToGo := AssignedIndexCount;
  for i := IndexCount-1 downto 0 do
  begin
    BoldIndex := Indexes[i];
    if assigned(BoldIndex) then
    begin
      RemoveAndFreeIndex(BoldIndex, OwnsEntries and (IndicesToGo = 1));
      dec(IndicesToGo);
    end;
  end;
  inherited;
end;

function TBoldUnOrderedIndexableList.GetCapacity: integer;
var
  i: integer;
begin
  result := MaxInt;
  for i := 0 to IndexCount - 1 do
    if Assigned(Indexes[i]) and (Indexes[i].Capacity < result) then
      result :=  Indexes[i].Capacity;
end;

procedure TBoldUnOrderedIndexableList.SetCapacity(const Value: integer);
var
  i: integer;
begin
  for i := 0 to IndexCount - 1 do
    if Assigned(Indexes[i]) then
      Indexes[i].Capacity := Value;
end;

function TBoldUnOrderedIndexableList.GetCount: Integer;
begin
  if IndexCount > 0 then
  begin
    if SupportsNil then
      result := FirstNilSupportingIndex.Count
    else
      result := Indexes[0].Count
  end
  else
    Result := 0;
end;

function TBoldUnOrderedIndexableList.GetDebugInfo: string;
begin
  result := Format('%s.count=%d', [ClassName, count]); 
end;

function TBoldUnOrderedIndexableList.AddIndex(BoldIndex: TBoldIndex): integer;
var
  SourceIndex: TBoldIndex;
begin
  Result := IndexCount;
  SetLength(fIndexes, Result+1);
  fIndexes[Result] := BoldIndex;
  if IndexCount > 1 then
  begin
    SourceIndex := FirstNilSupportingIndex;
    if not assigned(SourceIndex) then
      SourceIndex := FirstAssignedIndex;
    SourceIndex.FillEmptyIndex(BoldIndex);
  end;
  if BoldIndex.SupportsNilItems then
  begin
    SupportsNil := true;
    KnowsSupportsNil := true;
  end;
end;

procedure TBoldUnOrderedIndexableList.RemoveAndFreeIndex(var BoldIndex: TBoldIndex; DestroyObjects: Boolean);
var
  i: Integer;
begin
  if not (ilodestroying in fOptions) and (AssignedIndexCount = 1) then
    raise EBold.CreateFmt(sCannotRemoveLastIndex, [classname]);
  if Assigned(BoldIndex) then
  begin
    if BoldIndex.SupportsNilItems then
      KnowsSupportsNil := false;
    BoldIndex.Clear(DestroyObjects);
    for i := 0 to IndexCount - 1 do
      if fIndexes[i] =  BoldIndex then
      begin
        fIndexes[i] := nil;
        if i <= fFirstNilSupportingIndex then
          fFirstNilSupportingIndex := -1;
        break;
      end;
    FreeAndNil(BoldIndex);
  end;
end;

procedure TBoldUnOrderedIndexableList.SetIndexCapacity(NewCapacity: integer);
begin
   // No longer used
end;

procedure TBoldUnOrderedIndexableList.AddToAllIndexes(Item: TObject);
var
  i: integer;
begin
  for i := 0 to IndexCount-1 do
    if Assigned(Indexes[i]) then
      Indexes[i].Add(Item);
end;

function TBoldUnOrderedIndexableList.RemoveFromAllIndexes(Item: TObject): boolean;
var
  i: integer;
begin
  if Assigned(Item) then
  begin
    result := (IndexCount > 0);
    for i := 0 to IndexCount-1 do
      if Assigned(Indexes[i]) then
        result := Indexes[i].Remove(Item) and result;
  end
  else
    result := false;
end;

procedure TBoldUnOrderedIndexableList.Add(Item: TObject);

  procedure InternalRaise;
  begin
    raise EBold.CreateFmt('%s.Add(nil): This list does not support nil-pointers', [ClassName]);
  end;

begin
  if not assigned(item) and not SupportsNil then
    InternalRaise;
  AddToAllIndexes(Item);
end;

function TBoldUnOrderedIndexableList.Remove(Item: TObject): boolean;
begin
  result := RemoveFromAllIndexes(item);
  if OwnsEntries then
    item.free;
end;

procedure TBoldUnOrderedIndexableList.ItemChanged(Item: TObject);
var
  i: integer;
begin
  for i := 0 to IndexCount - 1 do
    if Assigned(Indexes[i]) then
      Indexes[i].ItemChanged(Item);
end;

procedure TBoldUnOrderedIndexableList.Clear;
var
  i: Integer;
  IndicesToGo: integer;
begin
  if count = 0 then
    exit;
  IndicesToGo := AssignedIndexCount;
  for i := 0 to IndexCount-1 do
    if assigned(Indexes[i]) then
    begin
      Indexes[i].Clear(OwnsEntries and (IndicesToGo=1)); // only free from one (last) index
      dec(IndicesToGo);
    end;
end;

procedure TBoldUnOrderedIndexableList.SetIndex(I: Integer; const Value: TBoldIndex);
begin
  if value.Count <> 0 then
    raise EBold.CreateFmt(sCannotSetNonEmptyIndex, [ClassName, value.ClassName]);
  if Assigned(fIndexes[i]) and TBoldIndex(fIndexes[i]).SupportsNilItems then
    KnowsSupportsNil := false;
  fIndexes[i] := Value;
  if Value.SupportsNilItems then
  begin
    SupportsNil := true;
    KnowsSupportsNil := true;
  end;
  if i >= fFirstNilSupportingIndex then
    fFirstNilSupportingIndex := -1;
  if AssignedIndexCount > 1 then
    FirstAssignedIndex.FillEmptyIndex(Value);
end;

function TBoldUnOrderedIndexableList.GetAny: TObject;
begin
  if IndexCount > 0 then
    result := FirstAssignedIndex.Any
  else
    result := nil
end;

{ TBoldUnOrderedIndexableList }

procedure TBoldIndexableList.RemoveByIndex(Index: Integer);
var
  Item: Tobject;
begin
  if (index < 0) or (index>=fIndexIndex.FastCount) then
    raise EBold.CreateFmt('%s.RemoveByIndex: Index %d out of bounds, count is %d', [classname, Index, fIndexIndex.FastCount]);//_DebugInfo(Index);
  Item := fIndexIndex.Items[Index];
  RemoveFromAllNonOrderedIndexes(Item);
  fIndexIndex.RemoveByIndex(Index);
  if OwnsEntries then
    Item.Free;
end;

procedure TBoldIndexableList.Move(CurIndex, NewIndex: Integer);
begin
  fIndexIndex.Move(CurIndex, newIndex);
end;

procedure TBoldIndexableList.SetItem(Index: integer; value: TObject);
var
  temp: TObject;
begin
  if (index < 0) or (index>=fIndexIndex.FastCount) then
    raise EBold.CreateFmt('%s.SetItem: Index %d out of bounds, count is %d', [classname, Index, fIndexIndex.FastCount]);//_DebugInfo(Index);
  if Value = fIndexIndex.items[index] then
    exit;
  RemoveFromAllNonOrderedIndexes(fIndexIndex.items[index]);

  if OwnsEntries then
  begin
    temp := fIndexIndex.Items[index];
    fIndexIndex.Items[index] := Value;
    temp.free;
  end
  else
    fIndexIndex.Items[Index] := value;

  AddToAllNonOrderedIndexes(value);
end;

procedure TBoldIndexableList.Insert(Index: Integer; Item: TObject);
begin
  if (index < 0) or (index>fIndexIndex.FastCount) then
    raise EBold.CreateFmt('%s.Insert: Index %d out of bounds, count is %d', [classname, Index, fIndexIndex.FastCount]);//_DebugInfo(Index);
  fIndexIndex.Insert(Index, Item);
  AddToAllNonOrderedIndexes(item);
end;

function TBoldIndexableList.IndexOf(Item: TObject): integer;
begin
  result := fIndexIndex.IndexOf(Item);
end;

function TBoldIndexableList.Includes(Item: TObject): boolean;
begin
  result := IndexOf(Item) <> -1;
end;

procedure TBoldIndexableList.Exchange(Index1, Index2: integer);
begin
  fIndexIndex.Exchange(Index1, Index2);
end;

procedure TBoldIndexableList.Sort(Compare: TIntegerIndexSortCompare);
begin
  fIndexIndex.Sort(Compare);
end;

procedure TBoldIndexableList.AddToAllNonOrderedIndexes(item: TObject);
var
  i: integer;
begin
  if assigned(item) then
    for i := 0 to IndexCount -1 do
      if Indexes[i] <> fIndexIndex then
        Indexes[i].add(Item);
end;

constructor TBoldIndexableList.Create;
begin
  inherited;
  fIndexIndex := TBoldIntegerIndex.Create;
  AddIndex(fIndexIndex);
end;

procedure TBoldIndexableList.RemoveFromAllNonOrderedIndexes(item: TObject);
var
  i: integer;
begin
  if assigned(item) then
    for i := 0 to IndexCount -1 do
      if assigned(Indexes[i]) and (Indexes[i] <> fIndexIndex) then
        Indexes[i].Remove(item);
end;

procedure TBoldIndexableList.RemoveList(AList: TBoldIndexableList);
var
  i,j: integer;
begin
  for I := 0 to AList.Count - 1 do
  begin
    j := IndexOf(AList.Items[i]);
    if j <> -1 then
      self.RemoveByIndex(j);
  end;
end;

function TBoldIndexableList.GetUnorderedIndexCount: integer;
begin
  result := IndexCount-1;
end;

function TBoldUnOrderedIndexableList.CreateTraverser: TBoldIndexableListTraverser;
var
  i: integer;
begin
  result := nil;
  for i := 0 to IndexCount-1 do
    if Assigned(Indexes[i]) and Indexes[i].SupportsTraverser then
    begin
      result := TraverserClass.Create(Indexes[i].CreateTraverser);
      exit;
    end;
end;

function TBoldUnOrderedIndexableList.GetEnumerator: TBoldIndexableListTraverser;
begin
  result := CreateTraverser;
end;

function TBoldUnOrderedIndexableList.TraverserClass: TBoldIndexableListTraverserClass;
begin
  result := TBoldIndexableListTraverser;
end;

function TBoldUnOrderedIndexableList.GetAssignedIndexCount: integer;
var
  i: integer;
begin
  result := 0;
  for i := 0 to IndexCount-1 do
    if assigned(Indexes[i]) then
      inc(result);
end;

function TBoldUnOrderedIndexableList.FirstAssignedIndex: TBoldIndex;
var
  i: integer;
begin
  result := nil;
  for i := 0 to IndexCount - 1 do
    if assigned(Indexes[i]) then
    begin
      result := Indexes[i];
      exit;
    end;
end;

{ TBoldIndexableListTraverser }

constructor TBoldIndexableListTraverser.Create(IndexTraverser: TBoldIndexTraverser);
begin
  inherited Create;
  fIndexTraverser := IndexTraverser;
  fCurrentIndex := 0;
end;

destructor TBoldIndexableListTraverser.Destroy;
begin
  inherited;
  FreeAndNil(fIndexTraverser);
end;

function TBoldIndexableListTraverser.GetAutoMoveOnRemoveCurrent: boolean;
begin
  result := fIndexTraverser.AutoMoveOnRemoveCurrent;
end;

procedure TBoldIndexableListTraverser.SetAutoMoveOnRemoveCurrent(
  const Value: boolean);
begin
  fIndexTraverser.AutoMoveOnRemoveCurrent := Value;
end;

function TBoldIndexableListTraverser.GetItem: TObject;
begin
  result := fIndexTraverser.Item;
end;

function TBoldIndexableListTraverser.MoveNext: Boolean;
begin
  result := fIndexTraverser.MoveNext;
end;

end.
