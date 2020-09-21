unit BoldIndexableList;

interface

uses
  Classes,
  BoldBase,
  BoldContainers,
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
  protected
    function GetItem: TObject;
    function GetEol: Boolean;
  public
    constructor Create(IndexTraverser: TBoldIndexTraverser);
    destructor Destroy; override;
    procedure Next;
    property Item: TObject read GetItem;
    property EndOfList: Boolean read GetEol;
    property CurrentIndex: integer read fCurrentIndex;
  end;


  {---TBoldUnOrderedIndexableList---}
  TBoldUnOrderedIndexableList = class(TBoldNonRefCountedObject)
  private
    fIndexes: TList;
    fOptions: TBoldIndexableListOptionsSet;
    function GetCount: Integer;
    function GetIndex(Index: Integer): TBoldIndex;
    function GetIndexCount: Integer;
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
  protected
    function AddIndex(BoldIndex: TBoldIndex): integer;
    procedure AddToAllIndexes(Item: TObject);
    procedure RemoveAndFreeIndex(var BoldIndex: TBoldIndex; DestroyObjects: Boolean);
    procedure RemoveFromAllIndexes(Item: TObject);
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
    procedure Remove(Item: TObject);
    function CreateTraverser: TBoldIndexableListTraverser;
    property Count: integer read GetCount;
    property Any: TObject read GetAny;
    property OwnsEntries: boolean read GetOwnsEntries write SetOwnsEntries;
  end;

  TBoldIndexableList = class(TBoldUnOrderedIndexableList)
  private
    fIndexIndex: TBoldIntegerIndex;
    function GetItem(Index: integer): TObject;
    procedure SetItem(Index: integer; value: TObject);
    procedure AddToAllNonOrderedIndexes(item: TObject);
    procedure RemoveFromAllNonOrderedIndexes(item: TObject);
    function GetUnorderedIndexCount: integer;
  protected
    property Items[I: Integer]: TObject read GetItem write SetItem;
    property UnorderedIndexCount: integer read GetUnorderedIndexCount;
  public
    constructor Create;
    procedure Move(CurIndex, NewIndex: Integer);
    procedure RemoveByIndex(Index: Integer);
    procedure Sort(Compare: TListSortCompare);
    procedure Exchange(Index1, Index2: integer);
    function IndexOf(Item: TObject): integer;
    procedure Insert(Index: Integer; Item: TObject);
  end;


procedure SetIndexVariable(var Index: integer; GivenPosition: integer);

implementation

uses
  BoldCommonConst,
  SysUtils,
  BoldDefs;

const
  DEFAULTINDEXCAPACITY = 1;

procedure SetIndexVariable(var Index: integer; GivenPosition: integer);
begin
  assert((index = -1) or (index = GivenPosition), 'Erroneous index construction, please debug!');
  Index := GivenPosition
end;

{ TBoldUnOrderedIndexableList }
constructor TBoldUnOrderedIndexableList.Create;
begin
  inherited;
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

  FreeAndNil(fIndexes);
  inherited;
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


function TBoldUnOrderedIndexableList.GetIndex(Index: Integer): TBoldIndex;
begin
  if Assigned(fIndexes) then
    Result := TBoldIndex(FIndexes[Index])
  else
    Result := nil;
end;

function TBoldUnOrderedIndexableList.GetIndexCount: Integer;
begin
  if Assigned(fIndexes) then
    Result := FIndexes.Count
  else
    Result := 0;
end;

function TBoldUnOrderedIndexableList.AddIndex(BoldIndex: TBoldIndex): integer;
var
  SourceIndex: TBoldIndex;
begin
  if not Assigned(fIndexes) then
    SetIndexCapacity(DEFAULTINDEXCAPACITY);
  Result := FIndexes.Add(BoldIndex);
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
begin
  if not (ilodestroying in fOptions) and (AssignedIndexCount = 1) then
    raise EBold.CreateFmt(sCannotRemoveLastIndex, [classname]);
  if Assigned(BoldIndex) then
  begin
    if BoldIndex.SupportsNilItems then
      KnowsSupportsNil := false;

    if DestroyObjects then
      BoldIndex.Clear(DestroyObjects);
    fIndexes[fIndexes.IndexOf(BoldIndex)] := nil;
    FreeAndNil(BoldIndex);
  end;
end;

procedure TBoldUnOrderedIndexableList.SetIndexCapacity(NewCapacity: integer);
begin
  if not Assigned(fIndexes) then
    fIndexes := TList.Create;
  fIndexes.Capacity := NewCapacity;
end;

procedure TBoldUnOrderedIndexableList.AddToAllIndexes(Item: TObject);
var
  i: integer;
begin
  // ordered indexes must get nil-pointers too.
  for i := 0 to IndexCount-1 do
    if Assigned(Indexes[i]) then
      Indexes[i].Add(Item);
end;

procedure TBoldUnOrderedIndexableList.RemoveFromAllIndexes(Item: TObject);
var
  i: integer;
begin
  if Assigned(Item) then
    for i := 0 to IndexCount-1 do
      if Assigned(Indexes[i]) then
        Indexes[i].Remove(Item);
end;

procedure TBoldUnOrderedIndexableList.Add(Item: TObject);
begin
  if not assigned(item) and not SupportsNil then
    raise EBold.CreateFmt(sNilPointersNotSupported, [ClassName]);
  AddToAllIndexes(Item);
end;

procedure TBoldUnOrderedIndexableList.Remove(Item: TObject);
begin
  RemoveFromAllIndexes(item);
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
  IndicesToGo := AssignedIndexCount;
  // if we own the entries, remove them with the last index to be removed.
  for i := 0 to IndexCount-1 do
    if assigned(Indexes[i]) then
    begin
      Indexes[i].Clear(OwnsEntries and (IndicesToGo=1));
      dec(IndicesToGo);
    end;
end;

procedure TBoldUnOrderedIndexableList.SetIndex(I: Integer; const Value: TBoldIndex);
begin
  if assigned(fIndexes) then
  begin
    if value.Count <> 0 then
      raise EBold.CreateFmt(sCannotSetNonEmptyIndex, [ClassName, value.ClassName]);
    fIndexes[i] := Value;
    if Value.SupportsNilItems then
    begin
      SupportsNil := true;
      KnowsSupportsNil := true;
    end;
    if AssignedIndexCount > 1 then
      FirstAssignedIndex.FillEmptyIndex(Value);
//    AddAllToIndex(Value);
  end;
end;

function TBoldUnOrderedIndexableList.GetAny: TObject;
begin
  if IndexCount > 0 then
    result := FirstAssignedIndex.Any
  else
    result := nil

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

{ TBoldUnOrderedIndexableList }

function TBoldIndexableList.GetItem(Index: integer): TObject;
begin
  Result := fIndexIndex.items[index];
end;

procedure TBoldIndexableList.RemoveByIndex(Index: Integer);
var
  Item: Tobject;
begin
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
  fIndexIndex.Insert(Index, Item);
  AddToAllNonOrderedIndexes(item);
end;

function TBoldIndexableList.IndexOf(Item: TObject): integer;
begin
  result := fIndexIndex.IndexOf(Item);
end;

procedure TBoldIndexableList.Exchange(Index1, Index2: integer);
begin
  fIndexIndex.Exchange(Index1, Index2);
end;

procedure TBoldIndexableList.Sort(Compare: TListSortCompare);
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


function TBoldUnOrderedIndexableList.TraverserClass: TBoldIndexableListTraverserClass;
begin
  result := TBoldIndexableListTraverser;
end;

function TBoldUnOrderedIndexableList.FirstNilSupportingIndex: TBoldIndex;
var
  i: integer;
begin
  result := nil;
  for i := 0 to IndexCount-1 do
    if Assigned(Indexes[i]) and Indexes[i].SupportsNilItems then
    begin
      result := Indexes[i];
      break;
    end;
end;

function TBoldUnOrderedIndexableList.GetKnowsSupportsNil: Boolean;
begin
  result := iloKnowsSupportsNil in fOptions;
end;

function TBoldUnOrderedIndexableList.GetSupportsNil: Boolean;
begin
  if not KnowsSupportsNil then
    CalculateSupportsNil;
  result := iloSupportsNil in fOptions;
end;

procedure TBoldUnOrderedIndexableList.SetKnowsSupportsNil(
  const Value: Boolean);
begin
  if value then
    Include(fOptions, iloKnowsSupportsNil)
  else
    Exclude(fOptions, iloKnowsSupportsNil);
end;

procedure TBoldUnOrderedIndexableList.SetSupportsNil(const Value: Boolean);
begin
  if value then
    Include(fOptions, iloSupportsNil)
  else
    Exclude(fOptions, iloSupportsNil);
end;

procedure TBoldUnOrderedIndexableList.CalculateSupportsNil;
begin
  SupportsNil := FirstNilSupportingIndex <> nil;
  KnowsSupportsNil := true;
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


function TBoldIndexableListTraverser.GetEol: Boolean;
begin
  result := fIndexTraverser.EndOfList;
end;

function TBoldIndexableListTraverser.GetItem: TObject;
begin
  result := fIndexTraverser.Item;
end;

procedure TBoldIndexableListTraverser.Next;
begin
  fIndexTraverser.Next;
  Inc(fCurrentIndex);
end;

end.
