{ Global compiler directives }
{$include bold.inc}
unit BoldIndex;

interface

uses
  BoldBase,
  Classes,
  Contnrs;

const
  {$IFDEF BOLD_DELPHI16_OR_LATER}
  MaxListSize = MaxInt div 16;
  {$ELSE}
  MaxListSize = Classes.MaxListSize;
  {$ENDIF}

type
  { forward declaration of classes }
  TBoldIndexTraverser = class;
  TBoldHashIndexTraverser = class;
  TBoldIndex = class;
  TBoldHashIndex = class;
  TBoldIntegerIndex = class;

  PItemEntry = ^TBoldHashIndexItemEntryRec;
  PPItemEntry = ^PItemEntry;

  TIntegerIndexSortCompare = function (Item1, Item2: TObject): Integer;  

  { TBoldHashIndexItemEntry }
  TBoldHashIndexItemEntryRec = record
    Next: PItemEntry;
    Item: TObject;
  end;

  // Make class so it shows up in AQTime
  TBoldItemRecBloc = class
  private
    fEntries: array[0..4094] of  TBoldHashIndexItemEntryRec;
    function GetFirstEntry: PItemEntry;
  public
    constructor Create;
    property FirstEntry: PItemEntry read GetFirstEntry;
  end;

  TBoldHashIndexItemEntryRecHandler = class
  private
    fCount: Integer;
    fBlocks: TObjectList;
    fFirstFree: PItemEntry;
  public
    function GetRec: PItemEntry;
    procedure ReturnRec(rec: PItemEntry);
  public
    constructor Create;
    destructor Destroy; override;
    property Count: Integer read fCount;
  end;

  { TBoldIndexTraverser }
  TBoldIndexTraverser = class(TBoldMemoryManagedObject)
  private
    fAutoMoveOnRemoveCurrent: boolean;
    procedure ItemDestroyed(AItem: TObject); virtual; abstract;
    procedure Clear; virtual;
  protected
    function GetItem: TObject; virtual; abstract;
  public
    procedure AfterConstruction; override;
    function MoveNext: Boolean; virtual; abstract;
    property Item: TObject read GetItem;
    property AutoMoveOnRemoveCurrent: boolean read fAutoMoveOnRemoveCurrent write fAutoMoveOnRemoveCurrent;
  end;

  { TBoldIndex }
  TBoldIndex = class (TBoldMemoryManagedObject)
  protected
    function GetAny: TObject; virtual; abstract;
    function GetCount: Integer; virtual; abstract;
    function Match(const Key; Item: TObject):Boolean; virtual; abstract;
    function GetSupportsTraverser: Boolean; virtual;
    function GetSupportsNilItems: Boolean; virtual;
    function GetCapacity: integer; virtual; abstract;
    procedure SetCapacity(const Value: integer); virtual; abstract;
  public
    procedure FillEmptyIndex(BoldIndex: TBoldIndex); virtual; abstract;
    procedure Add(Item: TObject); virtual; abstract;
    procedure Clear(DestroyObjects: Boolean=false); virtual; abstract;
    function Find(const Key):TObject; virtual; abstract;
    procedure FindAll(const Key; Result: TList); virtual; abstract;
    function IsCorrectlyIndexed(Item: TObject): boolean; virtual; abstract;
    procedure ItemChanged(Item: TObject); virtual;
    function Remove(Item: TObject): boolean; virtual; abstract;
    procedure RemoveChanged(Item: TObject); virtual; abstract;
    function CreateTraverser: TBoldIndexTraverser; virtual;
    function GetAndRemoveAny: TObject; virtual;
    property Count: integer read GetCount;
    property Any: TObject read GetAny;
    property SupportsTraverser: Boolean read GetSupportsTraverser;
    property SupportsNilItems: Boolean read GetSupportsNilItems;
    property Capacity: integer read GetCapacity write SetCapacity;    
  end;

  TObjectStaticArray = array[0..MaxListSize - 1] of TObject;
  PObjectStaticArray = ^TObjectStaticArray;

  { TBoldIntegerIndex }
  TBoldIntegerIndex = class(TBoldIndex)
  strict private
    FObjectStaticArray: PObjectStaticArray;
    FCount: Integer;
    FCapacity: Integer;
    function GetItems(index: integer): TObject;
    procedure SetItems(index: integer; const Value: TObject);
    procedure Grow;
  private
    procedure RangeError(Index: integer);
  protected
    function GetAny: TObject; override;
    function GetCount: Integer; override;
    function Match(const key; Item: TObject):Boolean; override;
    function GetSupportsNilItems: Boolean; override;
    function GetCapacity: integer; override;
    procedure SetCapacity(const Value: integer); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure FillEmptyIndex(BoldIndex: TBoldIndex); override;
    procedure Add(Item: TObject); override;
    procedure Clear(DestroyObjects: Boolean=false); override;
    procedure ItemChanged(Item: TObject); override;
    function Find(const Key):TObject; override;
    procedure FindAll(const Key; Result: TList); override;
    function IsCorrectlyIndexed(Item: TObject): boolean; override;
    function Remove(Item: TObject): boolean; override;
    procedure RemoveChanged(Item: TObject); override;
    procedure Move(CurIndex, NewIndex: Integer);
    procedure Sort(Compare: TIntegerIndexSortCompare);
    procedure Insert(Index: Integer; Item: TObject);
    procedure Exchange(Index1, Index2: integer);
    procedure RemoveByIndex(Index: Integer);
    function GetAndRemoveAny: TObject; override;
    function IndexOf(Item: TObject): integer;
    property Items[index: integer]: TObject read GetItems write SetItems;
    function Includes(Item: TObject): boolean;
    property FastCount: Integer read fCount;
  end;

  { TBoldHashIndexTraverser }
  TBoldHashIndexTraverser = class(TBoldIndexTraverser)
  private
    fHashIndex: TBoldHashIndex;
    fCurrentItem: PItemEntry;
    fBucketIndex: integer;
    function FirstItemOfNextBucket: PItemEntry;
    procedure ItemDestroyed(AItem: TObject); override;
    procedure Clear; override;
  protected
    function GetItem: TObject; override;
  public
    constructor Create(HashIndex: TBoldHashIndex);
    destructor Destroy; override;
    function MoveNext: Boolean; override;
  end;

  { TBoldHashIndexOptions }
  TBoldHashIndexOptions = packed record
    AutoResize: Boolean;
    PendingResize: Boolean;
    IsResizing: Boolean;
    RehashOnChange: Boolean
  end;

  { TBoldHashIndex }
  TBoldHashIndex = class(TBoldIndex)
  private
    fOptions: TBoldHashIndexOptions;
    fBucketArray: array of PItemEntry;
    fItemCount: integer;
    fLastIndexForAny: integer;
    fTraverserList: TList;
    procedure InsertEntry(Entry: PItemEntry);
    function MinimumBucketCount: integer;
    function MaximumBucketCount: integer;
    function PrefferedBucketCount: integer;
    procedure SetAutoResize(Value: boolean);
    procedure RemoveTraverser(ATraverser: TBoldIndexTraverser);
    function GetTraverserCount: Integer;
    function EnsuredTraverserList: TList;
    function GetTraverser(AIndex: integer): TBoldIndexTraverser;
  protected
    function IndexForHash(Hash: Cardinal): integer;
    function GetCount: Integer; override;
    function HashItem(Item: TObject): Cardinal; virtual; abstract;
    function Hash(const Key): Cardinal; virtual; abstract;
    function GetAny: TObject; override;
    function GetSupportsTraverser: Boolean; override;
    property Traversers[AIndex: integer]: TBoldIndexTraverser read GetTraverser;
    property TraverserCount: Integer read GetTraverserCount;
    function GetCapacity: integer; override;
    procedure SetCapacity(const Value: integer); override;
    property Options: TBoldHashIndexOptions read fOptions;
  public
    constructor Create;
    destructor Destroy; override;
    procedure FillEmptyIndex(BoldIndex: TBoldIndex); override;
    procedure Add(Item: TObject); override;
    procedure Clear(DestroyObjects: Boolean=false); override;
    function Find(const Key): TObject; override;
    procedure FindAll(const Key; Result: TList); override;
    function IsCorrectlyIndexed(Item: TObject): boolean; override;
    procedure ItemChanged(Item: TObject); override;
    function Remove(Item: TObject): boolean; override;
    procedure RemoveChanged(Item: TObject); override;
    procedure Resize;
    function CreateTraverser: TBoldIndexTraverser; override;
    property AutoResize: Boolean read fOptions.AutoResize write SetAutoResize;
    procedure AssertIndex;
  end;

var
  G_HashIndexItemEntryRecHandler: TBoldHashIndexItemEntryRecHandler;

implementation

uses
  System.Types,
  SysUtils,
  BoldDefs;

const
  AVERAGEBUCKETLENGTH = 2;
  MINIMUMHASHBUCKETS = 8;

function TBoldIntegerIndex.GetItems(index: integer): TObject;
begin
  // No range check, checked by caller
  Result := FObjectStaticArray^[Index];
end;

function TBoldItemRecBloc.GetFirstEntry: PItemEntry;
begin
  Result := @fEntries[0];
end;

function TBoldHashIndexItemEntryRecHandler.GetRec: PItemEntry;
var
  Block: TBoldItemRecBloc;
begin
   INC(fCount);
   if not Assigned(fFirstFree) then
   begin
     Block := TBoldItemRecBloc.Create;
     fFirstFree := Block.FirstEntry;
     fBlocks.Add(Block)
   end;
   Result := fFirstFree;
   fFirstFree := Result.Next;
end;

procedure TBoldHashIndexItemEntryRecHandler.ReturnRec(rec: PItemEntry);
begin
  DEC(fCount);
  rec.Next := fFirstFree;
  fFirstFree := rec;
end;

procedure ReturnItemEntry(Entry:PItemEntry; DestroyObjects: Boolean=false);
begin
  if not Assigned(Entry) then
    Exit;
  If assigned(Entry.Next) then
    ReturnItemEntry(Entry.Next, DestroyObjects);
  if DestroyObjects then
    FreeAndNil(Entry.Item);
  G_HashIndexItemEntryRecHandler.ReturnRec(entry);
end;

{ TBoldIndex }

function TBoldIndex.CreateTraverser: TBoldIndexTraverser;
begin
  result := nil;
end;

function TBoldIndex.GetAndRemoveAny: TObject;
begin
  result := Any;
  if Assigned(result) then
  begin
    if Count = 1 then
      Clear
    else
      Remove(result);
  end;
end;

function TBoldIndex.GetSupportsNilItems: Boolean;
begin
  result := false;
end;

function TBoldIndex.GetSupportsTraverser: Boolean;
begin
  result := false;
end;

procedure TBoldIndex.ItemChanged(Item: TObject);
begin
  if not IsCorrectlyIndexed(Item) then
  begin
    RemoveChanged(Item);
    Add(Item);
  end;
end;

{ TBoldHashIndex }
constructor TBoldHashIndex.Create;
begin
  fOptions.AutoResize := True;
  fOptions.RehashOnChange := true;
end;

destructor TBoldHashIndex.Destroy;
begin
  Clear;
  assert(TraverserCount = 0, 'unreleased traversers on hashindex');
  FreeAndNil(fTraverserList);  
  inherited
end;

function TBoldHashIndex.EnsuredTraverserList: TList;
begin
  if not Assigned(fTraverserList) then
    fTraverserList := TList.Create;
  result := fTraverserList;
end;

procedure TBoldHashIndex.SetAutoResize(Value: boolean);
begin
  fOptions.AutoResize := Value;
  if Value then Resize;
end;

function TBoldHashIndex.GetCapacity: integer;
begin
  result := MaxInt; // Capacity not supported so return MaxInt
end;

procedure TBoldHashIndex.SetCapacity(const Value: integer);
begin
// do nothing
end;

function TBoldHashIndex.GetCount: Integer;
begin
  Result := fItemCount;
end;

function TBoldHashIndex.PrefferedBucketCount: integer;
begin
  Result := MINIMUMHASHBUCKETS shl 2 + fItemCount div AVERAGEBUCKETLENGTH;
end;

function TBoldHashIndex.MinimumBucketCount: integer;
begin
  Result := PrefferedBucketCount div 2;
end;

function TBoldHashIndex.MaximumBucketCount: integer;
begin
  Result := PrefferedBucketCount * 4;
end;

function TBoldHashIndex.IndexForHash(Hash: Cardinal): integer;
begin
  result := hash mod Cardinal(Length(fBucketArray));
end;

procedure TBoldHashIndex.InsertEntry(Entry: PItemEntry);
var
  BucketIndex: integer;
begin
  BucketIndex := IndexForHash(HashItem(Entry.Item));
  Entry.next := fBucketArray[BucketIndex];
  fBucketArray[BucketIndex] := Entry;
  Inc(fItemCount);
end;

procedure TBoldHashIndex.Resize;
var
  TempChain: PItemEntry;
  Last: PPItemEntry;
  Current: PItemEntry;
  i, oldBucketCount: integer;
begin
  oldBucketCount := Length(fBucketArray);
  if fOptions.IsResizing then
    Exit;
  if ((oldBucketCount >= MinimumBucketCount) and (oldBucketCount <= MaximumBucketCount)) then
  begin
    fOptions.PendingResize := false;
    exit;
  end;
  if (TraverserCount > 0) and (fItemCount > 0) then
  begin
    fOptions.PendingResize := true;
    exit;
  end;
  try
    fOptions.IsResizing := true;
    TempChain := nil;
    Last := @TempChain;

    for i := 0 to Length(fBucketArray) - 1 do
    begin
      Current := fBucketArray[i];
      if Assigned(Current) then
      begin
        Last^ := Current;
        while Assigned(Current.Next) do
          Current := Current.Next;
        Last := @Current.Next;
      end;
    end;
    SetLength(fBucketArray, PrefferedBucketCount);
    for i := 0 to PrefferedBucketCount - 1 do
      fBucketArray[i] := nil;
    fItemCount := 0;
    while Assigned(TempChain) do
    begin
      Current := TempChain;
      TempChain := TempChain.Next;
      InsertEntry(Current);
    end;

  finally
    fOptions.IsResizing := False;
  end;
end;

function TBoldHashIndex.Remove(Item: TObject): boolean;
var
  Pre: PPItemEntry;
  ToBeRemoved: PItemEntry;
  i: integer;
begin
  result := false;
  if Length(fBucketArray) = 0 then
    exit;
  Pre := @(fBucketArray[IndexForHash(HashItem(Item))]);
  while Assigned(Pre^) do
  begin
    if Pre^.Item = Item then
    begin
      for I := 0 to TraverserCount - 1 do
        Traversers[i].ItemDestroyed(Item);
      ToBeRemoved := Pre^;
      Pre^ := ToBeRemoved.Next;
      ToBeRemoved.next := nil;
      ReturnItemEntry(ToBeRemoved);
      Dec(fItemCount);
      result := true;
      break;
    end
    else
      Pre := @Pre^.Next;
  end;
  if Result and AutoResize then
    Resize;
end;

procedure TBoldHashIndex.Add(Item: TObject);
var
  NewEntry: PItemEntry;
begin
  if assigned(item) then
  begin
    if Length(fBucketArray) = 0 then
    begin
      Resize;
      Assert(Length(fBucketArray) > 0);
    end;
    NewEntry := G_HashIndexItemEntryRecHandler.GetRec;
    NewEntry.Item := Item;
    InsertEntry(NewEntry);
  end;
  if AutoResize then
    Resize;
end;

function TBoldHashIndex.Find(const Key): TObject;
var
  Current: PItemEntry;
begin
  Result := nil;
  if Length(fBucketArray) = 0 then
    Exit;
  Current := fBucketArray[IndexForHash(Hash(Key))];
  while Assigned(current) and (not Match(Key, Current.Item)) do
    Current := Current.Next;
  if Assigned(Current) then
    Result := Current.Item
end;

function TBoldHashIndex.IsCorrectlyIndexed(Item: TObject): boolean;
var
  Current: PItemEntry;
begin
  Current := fBucketArray[IndexForHash(HashItem(Item))];
  while Assigned(Current) and (Item <> Current.Item)  do
    Current := Current.Next;
  Result := Assigned(Current);
end;

procedure TBoldHashIndex.ItemChanged(Item: TObject);
var
  StoredAutoResize: boolean;
begin
  if not IsCorrectlyIndexed(Item) then
  begin
    StoredAutoResize := AutoResize;
    AutoResize := false;
    try
      RemoveChanged(Item);
      Add(Item);
    finally
      AutoResize := StoredAutoResize;
    end;
  end;
end;

procedure TBoldHashIndex.RemoveChanged(Item: TObject);
var
  i,j: integer;
  Pre: PPItemEntry;
  ToBeRemoved: PItemEntry;
begin
  for i := 0 to Length(fBucketArray) - 1 do
  begin
    Pre := @(fBucketArray[i]);
    while Assigned(Pre^) do
    begin
      if Pre^.Item = Item then
      begin
        for j := 0 to TraverserCount - 1 do
          Traversers[j].ItemDestroyed(Item);
        ToBeRemoved := Pre^;
        Pre^ := ToBeRemoved.Next;
        ToBeRemoved.Next := nil;
        ReturnItemEntry(ToBeRemoved);
        Dec(fItemCount);
        break;
      end
      else
        Pre := @(Pre^.Next);
    end;
  end;
  if AutoResize then
    Resize;
end;

procedure TBoldHashIndex.FindAll(const Key; Result: TList);
var
  current: PItemEntry;
begin
  Assert(Assigned(Result), 'Trying to find Entries to insert in an unassigned list');
  if Length(fBucketArray) = 0 then
    Exit;
  Current := fBucketArray[IndexForHash(Hash(Key))];
  while Assigned(Current) do
  begin
    if Match(Key, Current.Item) then
      result.Add(Current.Item);
    Current := Current.Next;
  end;
end;

procedure TBoldHashIndex.Clear(DestroyObjects: Boolean=false);
var
  i: integer;
begin
  if fItemCount = 0 then
    exit;
  for i := 0 to Length(fBucketArray) - 1 do
    ReturnItemEntry(fBucketArray[i], DestroyObjects);
  for i := 0 to TraverserCount -1 do
    Traversers[i].Clear;
  SetLength(fBucketArray, 0);
  fItemCount := 0;
end;

function TBoldHashIndex.GetAny: TObject;
var
  BucketIndex: integer;
begin
  result := nil;
  if Count = 0 then
    Exit;
  BucketIndex := fLastIndexForAny;
  while not assigned(result) do
  begin
    if BucketIndex >= Length(fBucketArray) then
      BucketIndex := 0;
    if assigned(fBucketArray[BucketIndex]) then
    begin
      result := fBucketArray[BucketIndex]^.Item;
      fLastIndexForAny := BucketIndex;
    end;
    inc(BucketIndex);
  end;
end;

procedure TBoldHashIndex.FillEmptyIndex(BoldIndex: TBoldIndex);
var
  i: integer;
  Current: PItemEntry;
begin
  for i := 0 to Length(fBucketArray) - 1 do
  begin
    Current := fBucketArray[i];
    while assigned(Current) do
    begin
      BoldIndex.Add(Current.Item);
      Current := Current.Next;
    end;
  end;
end;

function TBoldHashIndex.GetSupportsTraverser: Boolean;
begin
  result := true;
end;

function TBoldHashIndex.GetTraverser(AIndex: integer): TBoldIndexTraverser;
begin
  result := TBoldIndexTraverser(fTraverserList[AIndex]);
end;

function TBoldHashIndex.GetTraverserCount: Integer;
begin
  if Assigned(fTraverserList) then
    result := fTraverserList.Count
  else
    result := 0;
end;

function TBoldHashIndex.CreateTraverser: TBoldIndexTraverser;
begin
  result := TBoldHashIndexTraverser.Create(self);
  EnsuredTraverserList.Add(result);
end;

procedure TBoldHashIndex.RemoveTraverser(ATraverser: TBoldIndexTraverser);
begin
  if TraverserCount <> 0 then
    fTraverserList.Remove(ATraverser);
  if (TraverserCount = 0) and fOptions.PendingResize then
    Resize;
end;

procedure TBoldHashIndex.AssertIndex;
{$IFOPT C+}
var
  i: integer;
  Pre: PPItemEntry;
  Start: PItemEntry;
  Cnt: integer;
{$ENDIF}
begin
{$IFOPT C+}
  Cnt := 0;
  for i := 0 to Length(fBucketArray) - 1 do
  begin
    Pre := @(fBucketArray[i]);
    Start := Pre^;
    while Assigned(Pre^) do
    begin
      Inc(cnt);
      Assert(IsCorrectlyIndexed(Pre^.Item));
      Pre := @(Pre^.Next);
      Assert(pre^ <> Start);
    end;
  end;
  Assert(Count=Cnt, Format('%d, %d',[Count, cnt]));
{$ENDIF}
end;

{ TBoldIntegerIndex }

procedure TBoldIntegerIndex.SetCapacity(const Value: integer);
begin
  if Value <> FCapacity then
  begin
    if ((Value = 0) or (Value > Capacity)) and (Value >= fCount) then // only allow growth or set to 0, do not allow reduction in capacity
    begin
      Assert(Value >= fCount);
      ReallocMem(FObjectStaticArray, Value * SizeOf(TObject));
      FCapacity := Value;
    end;
  end;
end;

procedure TBoldIntegerIndex.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else
    if FCapacity > 8 then
      Delta := 16
    else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

procedure TBoldIntegerIndex.Add(Item: TObject);
var
  Index: Integer;
begin
  Index := FCount;
  if Index = FCapacity then
    Grow;
  FObjectStaticArray^[Index] := Item;
  Inc(FCount);
end;

procedure TBoldIntegerIndex.Clear(DestroyObjects: Boolean=false);
var
  i: integer;
  temp: TObject;
begin
  if DestroyObjects then
  begin
    for i := fCount - 1 downto 0 do
    begin
      temp := FObjectStaticArray^[i];
      FObjectStaticArray^[i] := nil;
      temp.Free;
    end;
  end;
  fCount := 0;
  SetCapacity(0);
end;

constructor TBoldIntegerIndex.Create;
begin
end;

function TBoldIntegerIndex.Find(const Key): TObject;
begin
  if Cardinal(Key) < Cardinal(FCount) then
    result := FObjectStaticArray^[Cardinal(Key)]
  else
    result := nil;
end;

procedure TBoldIntegerIndex.FindAll(const Key; Result: TList);
var
  res: TObject;
begin
  res := Find(Key);
  if assigned(res) then
    result.Add(res);
end;

function TBoldIntegerIndex.GetCapacity: integer;
begin
  result := fCapacity;
end;

function TBoldIntegerIndex.GetCount: Integer;
begin
  result := fCount;
end;

function TBoldIntegerIndex.GetAny: TObject;
begin
  if fCount > 0 then
    result := FObjectStaticArray[fCount-1]
  else
    result := nil;
end;

function TBoldIntegerIndex.GetAndRemoveAny: TObject;
begin
  if fCount > 0 then
  begin
    result := FObjectStaticArray[fCount-1];
    RemoveByIndex(fCount-1);
  end
  else
    result := nil;
end;

function TBoldIntegerIndex.IsCorrectlyIndexed(Item: TObject): boolean;
begin
  result := true;
end;

function TBoldIntegerIndex.Match(const Key; Item: TObject): Boolean;
begin
  if Cardinal(Key) < Cardinal(fCount) then
    Result := item = FObjectStaticArray[Cardinal(key)]
  else
    Result := False;
end;

procedure TBoldIntegerIndex.RangeError(Index: integer);
begin
  raise EBold.Create('Index out of range ' + IntToStr(Index));
end;

function TBoldIntegerIndex.IndexOf(Item: TObject): integer;
begin
  Result := FCount-1;
  while (Result >= 0) and (FObjectStaticArray^[Result] <> Item) do
    Dec(Result);
end;

function TBoldIntegerIndex.Includes(Item: TObject): boolean;
begin
  Result := IndexOf(Item) <> -1;
end;

procedure TBoldIntegerIndex.RemoveByIndex(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then
    RangeError(Index);
  Dec(FCount);
  if Index < FCount then
    System.Move(FObjectStaticArray^[Index + 1], FObjectStaticArray^[Index],
      (FCount - Index) * SizeOf(TObject));
end;

function TBoldIntegerIndex.Remove(Item: TObject): boolean;
var
  Index: Integer;
begin
  result := false;
  if FCount > 0 then
  begin
    Index := IndexOf(Item);
    if Index >= 0 then
    begin
      RemoveByIndex(Index);
      result := true;
    end;
  end;
end;

procedure TBoldIntegerIndex.RemoveChanged(Item: TObject);
begin
  Remove(item);
end;

procedure TBoldIntegerIndex.SetItems(index: integer; const Value: TObject);
begin
  // No range check, checked by caller
  FObjectStaticArray^[index] := value;
end;

procedure TBoldIntegerIndex.Exchange(Index1, Index2: integer);
var
  Item: TObject;
begin
  if (Index1 < 0) or (Index1 >= FCount) then
    RangeError(Index1);
  if (Index2 < 0) or (Index2 >= FCount) then
    RangeError(Index2);
  Item := FObjectStaticArray^[Index1];
  FObjectStaticArray^[Index1] := FObjectStaticArray^[Index2];
  FObjectStaticArray^[Index2] := Item;
end;

procedure TBoldIntegerIndex.Insert(Index: Integer; Item: TObject);
begin
  if (Index < 0) or (Index > FCount) then
    RangeError(Index);
  if FCount = FCapacity then
    Grow;
  if Index < FCount then
    System.Move(FObjectStaticArray^[Index], FObjectStaticArray^[Index + 1],
      (FCount - Index) * SizeOf(TObject));
  FObjectStaticArray^[Index] := Item;
  Inc(FCount);
end;

procedure TBoldIntegerIndex.Move(CurIndex, NewIndex: Integer);
var
  Item: Pointer;
begin
  if CurIndex <> NewIndex then
  begin
    if (NewIndex < 0) or (NewIndex >= FCount) then
      RangeError(NewIndex);
    if (CurIndex < 0) or (CurIndex >= FCount) then
      RangeError(CurIndex);
    Item := FObjectStaticArray^[CurIndex];
    FObjectStaticArray^[CurIndex] := nil;
    RemoveByIndex(CurIndex);
    Insert(NewIndex, nil);
    FObjectStaticArray^[NewIndex] := Item;
  end;
end;

procedure QuickSort(SortList: PObjectStaticArray; L, R: Integer;
  SCompare: TIntegerIndexSortCompare);
var
  I, J: Integer;
  P, T: TObject;
begin
  repeat
    I := L;
    J := R;
    P := SortList^[(L + R) shr 1];
    repeat
      while SCompare(SortList^[I], P) < 0 do
        Inc(I);
      while SCompare(SortList^[J], P) > 0 do
        Dec(J);
      if I <= J then
      begin
        T := SortList^[I];
        SortList^[I] := SortList^[J];
        SortList^[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(SortList, L, J, SCompare);
    L := I;
  until I >= R;
end;

procedure TBoldIntegerIndex.Sort(Compare: TIntegerIndexSortCompare);
begin
  if (FObjectStaticArray <> nil) and (fCount > 0) then
    QuickSort(FObjectStaticArray, 0, fCount - 1, Compare);
end;

procedure TBoldIntegerIndex.FillEmptyIndex(BoldIndex: TBoldIndex);
var
  i: integer;
begin
  for i := 0 to fCount - 1 do
    BoldIndex.Add(items[i])
end;

procedure TBoldIntegerIndex.ItemChanged(Item: TObject);
begin
end;

destructor TBoldIntegerIndex.Destroy;
begin
  Clear;
  SetCapacity(0);
  inherited;
end;

function TBoldIntegerIndex.GetSupportsNilItems: Boolean;
begin
  result := true;
end;

{ TBoldHashIndexTraverser }

function TBoldHashIndexTraverser.FirstItemOfNextBucket: PItemEntry;
begin
  result := nil;
  while not Assigned(Result) and (fBucketIndex < (Length(fHashIndex.fBucketArray)-1)) do
  begin
    inc(fBucketIndex);
    result := fHashIndex.fBucketArray[fBucketIndex];
  end;
end;

procedure TBoldHashIndexTraverser.Clear;
begin
  fCurrentItem := nil;
end;

constructor TBoldHashIndexTraverser.Create(HashIndex: TBoldHashIndex);
begin
  inherited Create;
  fHashIndex := HashIndex;
  fBucketIndex := -1;
end;

destructor TBoldHashIndexTraverser.Destroy;
begin
  fHashIndex.RemoveTraverser(self);
  inherited;
end;

function TBoldHashIndexTraverser.GetItem: TObject;
begin
  if Assigned(fCurrentItem) then
  begin
    result := fCurrentItem.Item;
    Assert(Result is TObject);
  end
  else
    result := nil;
end;

function TBoldHashIndexTraverser.MoveNext: boolean;
begin
  if Assigned(fCurrentItem) then
  begin
    if Assigned(fCurrentItem.Next) then
      fCurrentItem := fCurrentItem.Next
    else
      fCurrentItem := FirstItemOfNextBucket;
  end
  else
  begin
    fCurrentItem := FirstItemOfNextBucket;
  end;
 result := Assigned(fCurrentItem);
end;

procedure TBoldHashIndexTraverser.ItemDestroyed(AItem: TObject);
begin
  if AutoMoveOnRemoveCurrent and Assigned(fCurrentItem) and (fCurrentItem.Item = AItem) then
  begin
    fCurrentItem := fCurrentItem.Next;
    if not Assigned(fCurrentItem) then
      fCurrentItem := FirstItemOfNextBucket;
  end;
end;

{ TBoldHashIndexItemEntryRecHandler }

constructor TBoldHashIndexItemEntryRecHandler.Create;
begin
  fBlocks := TObjectList.Create;
  fBlocks.OwnsObjects := True;
end;

destructor TBoldHashIndexItemEntryRecHandler.Destroy;
begin
  FreeAndNil(fBlocks);
  if fCount <> 0 then
    raise Exception.Create('TBoldHashIndexItemEntryRecHandler: Not all records freed');
  inherited;
end;

{ TBoldItemRecBloc }

constructor TBoldItemRecBloc.Create;
var
  I: Integer;
begin
  for I :=low(fEntries) to high(fEntries)- 1 do
    fEntries[i].Next := @fEntries[i+1];
  fEntries[high(fEntries)].Next := nil;
end;

{ TBoldIndexTraverser }

procedure TBoldIndexTraverser.AfterConstruction;
begin
  inherited;
  fAutoMoveOnRemoveCurrent := true;
end;

procedure TBoldIndexTraverser.Clear;
begin
// nothing
end;

initialization
  G_HashIndexItemEntryRecHandler := TBoldHashIndexItemEntryRecHandler.Create;

finalization
  FreeAndNil(G_HashIndexItemEntryRecHandler);
end.
