unit BoldIndex;

interface

uses
  BoldBase,
  Classes;

type
  { forward declaration of classes }
  TBoldHashIndexBucketEntry = class;
  TBoldIndexTraverser = class;
  TBoldHashIndexTraverser = class;
  TBoldIndex = class;
  TBoldHashIndex = class;
  TBoldIntegerIndex = class;

  PItemEntry = ^TBoldHashIndexItemEntryRec;
  PPItemEntry = ^PItemEntry;

  PPItemEntryList = ^TPItemEntryList;
  TPItemEntryList = array[0..MaxListSize - 1] of PItemEntry;

  { TBoldHashIndexItemEntry }
  TBoldHashIndexItemEntryRec = record
    Next: PItemEntry;
    Item: TObject;
  end;


    { TBoldHashIndexBucketEntry }
  TBoldHashIndexBucketEntry = class(TBoldMemoryManagedObject)
    Next: PItemEntry;
  end;

  { TBoldIndexTraverser }
  TBoldIndexTraverser = class(TBoldMemoryManagedObject)
  protected
    function GetItem: TObject; virtual; abstract;
    function GetEol: Boolean; virtual; abstract;
  public
    procedure Next; virtual; abstract;
    property Item: TObject read GetItem;
    property EndOfList: Boolean read GetEol;
  end;

  { TBoldIndex }
  TBoldIndex = class (TBoldMemoryManagedObject)
  protected
    function GetAny: TObject; virtual; abstract;
    function GetCount: Integer; virtual; abstract;
    function Match(const Key; Item: TObject):Boolean; virtual; abstract;
    function GetSupportsTraverser: Boolean; virtual;
    function GetSupportsNilItems: Boolean; virtual;
  public
    procedure FillEmptyIndex(BoldIndex: TBoldIndex); virtual; abstract;
    procedure Add(Item: TObject); virtual; abstract;
    procedure Clear(DestroyObjects: Boolean=false); virtual; abstract;
    function Find(const Key):TObject; virtual; abstract;
    procedure FindAll(const Key; Result: TList); virtual; abstract;
    function IsCorrectlyIndexed(Item: TObject): boolean; virtual; abstract;
    procedure ItemChanged(Item: TObject); virtual;
    procedure Remove(Item: TObject); virtual; abstract;
    procedure RemoveChanged(Item: TObject); virtual; abstract;
    function CreateTraverser: TBoldIndexTraverser; virtual;
    property Count: integer read GetCount;
    property Any: TObject read GetAny;
    property SupportsTraverser: Boolean read GetSupportsTraverser;
    property SupportsNilItems: Boolean read GetSupportsNilItems;
  end;

  { TBoldIntegerIndex }
  TBoldIntegerIndex = class(TBoldIndex)
  private
    fList: TLIst;
    function GetItems(index: integer): TObject;
    procedure SetItems(index: integer; const Value: TObject);
  protected
    function GetAny: TObject; override;
    function GetCount: Integer; override;
    function Match(const key; Item: TObject):Boolean; override;
    function GetSupportsNilItems: Boolean; override;
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
    procedure Remove(Item: TObject); override;
    procedure RemoveChanged(Item: TObject); override;
    procedure Move(CurIndex, NewIndex: Integer);
    procedure Sort(Compare: TListSortCompare);
    procedure Insert(Index: Integer; Item: TObject);
    procedure Exchange(Index1, Index2: integer);
    procedure RemoveByIndex(Index: Integer);
    function IndexOf(Item: TObject): integer;
    property Items[index: integer]: TObject read GetItems write SetItems;
  end;

  { TBoldHashIndexTraverser }
  TBoldHashIndexTraverser = class(TBoldIndexTraverser)
  private
    fHashIndex: TBoldHashIndex;
    fCurrentItem: PItemEntry;
    fNextItem: PItemEntry;
    fBucketIndex: integer;
    function FirstItemOfNextBucket: PItemEntry;
  protected
    function GetItem: TObject; override;
    function GetEol: Boolean; override;
  public
    constructor Create(HashIndex: TBoldHashIndex);
    destructor Destroy; override;
    procedure Next; override;
  end;

  { TBoldHashIndexOptions }
  TBoldHashIndexOptions = packed record
    AutoResize: Boolean;
    PendingResize: Boolean;
    IsResizing: Boolean;
    TraverserCount: Byte;
  end;

  { TBoldHashIndex }
  TBoldHashIndex = class(TBoldIndex)
  private
    fOptions: TBoldHashIndexOptions;
    fBucketCount: integer; // Number of hashbuckets
    fBucketArray: PPItemEntryList;
    fItemCount: integer; // number of Items in table
    fLastIndexForAny: integer;
    function GetBucketArray: PPItemEntryList;
    procedure InsertEntry(Entry: PItemEntry);
    function MinimumBucketCount: integer;
    function MaximumBucketCount: integer;
    function PrefferedBucketCount: integer;
    procedure SetAutoResize(Value: boolean);
    procedure DecreaseTraverser;
    property BucketArray: PPItemEntryList read GetBucketArray;
  protected
    function PBucketForHash(Hash: Cardinal): PPItemEntry;
    function PBucketForIndex(Index: Integer): PPItemEntry;
    function GetCount: Integer; override;
    function HashItem(Item: TObject): Cardinal; virtual; abstract;
    function Hash(const Key): Cardinal; virtual; abstract;
    function GetAny: TObject; override;
    function GetSupportsTraverser: Boolean; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure FillEmptyIndex(BoldIndex: TBoldIndex); override;
    procedure Add(Item: TObject); override;
    procedure Clear(DestroyObjects: Boolean=false); override;
    function Find(const Key): TObject; override;
    procedure FindAll(const Key; Result: TList); override;
    function IsCorrectlyIndexed(Item: TObject): boolean; override;
    procedure Remove(Item: TObject); override;
    procedure RemoveChanged(Item: TObject); override;
    procedure Resize;
    function CreateTraverser: TBoldIndexTraverser; override;
    property AutoResize: Boolean read fOptions.AutoResize write SetAutoResize;
    procedure AssertIndex;
  end;

procedure ReturnItemEntry(Entry:PItemEntry; DestroyObjects: Boolean=false);

implementation

uses
  SysUtils,
  BoldDefs,
  BoldMemoryManager;

const
  AVERAGEBUCKETLENGTH = 2;
  MINIMUMHASHBUCKETS = 8;

// Utilityfunction
procedure ReturnItemEntry(Entry:PItemEntry; DestroyObjects: Boolean=false);
begin
  if not Assigned(Entry) then
    Exit;
  If assigned(Entry.Next) then
    ReturnItemEntry(Entry.Next, DestroyObjects);
  if DestroyObjects then
    FreeAndNil(Entry.Item);
  BoldMemoryManager_.DeAllocateMemory(entry, sizeof(TBoldHashIndexItemEntryRec))
end;

{ TBoldIndex }

function TBoldIndex.CreateTraverser: TBoldIndexTraverser;
begin
  result := nil;
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
  fBucketCount := MinimumBucketCount;
  fOptions.AutoResize := True;
end;

destructor TBoldHashIndex.Destroy;
begin
  Clear;
  assert(fOptions.TraverserCount = 0, 'unreleased traversers on hashindex');
  inherited
end;

procedure TBoldHashIndex.SetAutoResize(Value: boolean);
begin
  fOptions.AutoResize := Value;
  if Value then Resize;
end;

function TBoldHashIndex.GetCount: Integer;
begin
  Result := fItemCount;
end;

procedure TBoldHashIndex.Resize;
//FIXME: Check this code thouroughly before used!!!
var
  TempChain: PItemEntry;
  Last: PPItemEntry;
  Current: PItemEntry;
  i: integer;
begin
   if not fOptions.IsResizing and
     ((fBucketCount < MinimumBucketCount) or (fBucketCount > MaximumBucketCount)) then
  begin
    if fOptions.TraverserCount > 0 then
    begin
      fOptions.PendingResize := true;
      exit;
    end;
    try
      fOptions.IsResizing := true;

      // collect all entries in one chain starting in TempChain
      TempChain := Nil;
      Last := @TempChain;

      for i := 0 to fBucketCount - 1 do
      begin
        Current := PBucketForIndex(i)^;
        if Assigned(Current) then
        begin
          Last^ := Current;
          while Assigned(Current.Next) do
            Current := Current.Next;
          Last := @Current.Next;
        end;
      end;
      BoldMemoryManager_.DeAllocateMemory(fBucketArray, fBucketCount*sizeof(PItemEntry));
      fBucketArray := nil;
      // BucketArray will be recreated through GetBucketArray
      // calculate new size
      fBucketCount := PrefferedBucketCount;
      fItemCount := 0;
      // reinsert all the entries;
      while Assigned(TempChain) do
      begin
        // take first.next out of the chain and add it
        Current := TempChain;
        TempChain := TempChain.Next;
        InsertEntry(Current);
      end;

    finally
      fOptions.IsResizing := False;
    end;
  end;
end;

procedure TBoldHashIndex.Remove(Item: TObject);
var
  Pre: PPItemEntry;
  ToBeRemoved: PItemEntry;
begin
  Pre := PBucketForHash(HashItem(Item));
  while Assigned(Pre^) do
  begin
    if Pre^.Item = Item then
    begin
      ToBeRemoved := Pre^;
      Pre^ := ToBeRemoved.Next;
      ToBeRemoved.next := nil;
      ReturnItemEntry(ToBeRemoved);
      Dec(fItemCount);
      break;
    end
    else
      Pre := @Pre^.Next;
    end;
  if AutoResize then
    Resize;
end;

procedure TBoldHashIndex.InsertEntry(Entry: PItemEntry);
// inserts the new element first in the hashbucket
var
  Bucket: PPItemEntry;
begin
  Bucket := PBucketForHash(HashItem(Entry.Item));
  Entry.next := Bucket^;
  Bucket^ := Entry;
  Inc(fItemCount);
end;

procedure TBoldHashIndex.Add(Item: TObject);
var
  NewEntry: PItemEntry;
begin
  if assigned(item) then
  begin
    NewEntry := BoldMemoryManager_.AllocateMemory(sizeof(TBoldHashIndexItemEntryRec));
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
  Current := PBucketForHash(Hash(Key))^;
  while Assigned(current) and (not Match(Key, Current.Item)) do
    Current := Current.Next;
  if Assigned(Current) then
    Result := Current.Item
  else
    Result := nil;
end;

function TBoldHashIndex.IsCorrectlyIndexed(Item: TObject): boolean;
var
  Current: PItemEntry;
begin
  Current := PBucketForHash(HashItem(Item))^;
  while Assigned(Current) and (Item <> Current.Item)  do
    Current := Current.Next;
  Result := Assigned(Current);
end;

procedure TBoldHashIndex.RemoveChanged(Item: TObject);
var
  i: integer;
  Pre: PPItemEntry;
  ToBeRemoved: PItemEntry;
begin
  for i := 0 to fBucketCount - 1 do
  begin
    Pre := PBucketForIndex(i);
    while Assigned(Pre^) do
    begin
      if Pre^.Item = Item then
      begin
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

  Current := PBucketForHash(Hash(Key))^;
  while Assigned(Current) do
  begin
    if Match(Key, Current.Item) then
      result.Add(Current.Item);
    Current := Current.Next;
  end;
end;

function TBoldHashIndex.PrefferedBucketCount: integer;
begin
  Result := MINIMUMHASHBUCKETS shl 2 + fItemCount div AVERAGEBUCKETLENGTH;
end;

function TBoldHashIndex.MinimumBucketCount: integer;
begin
  Result := PrefferedBucketCount div 2;
end;

function TBoldHashIndex.GetBucketArray: PPItemEntryList;
begin
  if not Assigned(fBucketArray) then
    fBucketArray := BoldMemoryManager_.AllocateMemoryZeroFill(fBucketCount*sizeof(PItemEntry));
  result := fBucketArray;
end;

procedure TBoldHashIndex.Clear(DestroyObjects: Boolean=false);
var
  i: integer;
begin
//  inherited;
  if Assigned(fBucketArray) then
    for i := 0 to fBucketCount - 1 do
      ReturnItemEntry(PBucketForIndex(i)^, DestroyObjects);
  BoldMemoryManager_.DeAllocateMemory(fBucketArray, fBucketCount*sizeof(PItemEntry));
  fBucketArray := nil;
  fBucketCount := MinimumBucketCount;
  fItemCount := 0;
end;

function TBoldHashIndex.GetAny: TObject;
var
  BucketIndex: integer;
begin
  result := nil;
  if Count > 0 then
  begin
    BucketIndex := fLastIndexForAny;

    while not assigned(result) do
    begin
      if assigned(PBucketForHash(BucketIndex)^) then
      begin
        result := PBucketForHash(BucketIndex)^.Item;
        fLastIndexForAny := BucketIndex;
      end;

      inc(BucketIndex);
      if BucketIndex > fBucketCount then
        BucketIndex := 0;
    end;
  end;
end;

procedure TBoldHashIndex.FillEmptyIndex(BoldIndex: TBoldIndex);
var
  i: integer;
  Current: PItemEntry;
begin
  for i := 0 to fBucketCount - 1 do
  begin
    Current := PBucketForHash(i)^;
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

function TBoldHashIndex.CreateTraverser: TBoldIndexTraverser;
begin
  result := TBoldHashIndexTraverser.Create(self);
  fOptions.TraverserCount := fOptions.TraverserCount + 1;
end;

procedure TBoldHashIndex.DecreaseTraverser;
begin
  fOptions.TraverserCount := fOptions.TraverserCount - 1;
  if (fOptions.TraverserCount = 0) and fOptions.PendingResize then
    Resize;
end;

function TBoldHashIndex.MaximumBucketCount: integer;
begin
  Result := PrefferedBucketCount * 4;
end;

function TBoldHashIndex.PBucketForHash(
  Hash: Cardinal): PPItemEntry;
var
  index: integer;
begin
  index := hash mod Cardinal(fBucketCount);
  result := PBucketForIndex(index);
end;

function TBoldHashIndex.PBucketForIndex(Index: Integer): PPItemEntry;
begin
  Assert((index>=0) and (Index< fBucketCount));
  result := @(BucketArray[Index]);
end;

procedure TBoldHashIndex.AssertIndex;
var
  i: integer;
  Pre: PPItemEntry;
  Start: PItemEntry;
  {$IFOPT C+} // Assertions
  Cnt: integer;
  {$ENDIF}
begin
	{$IFOPT C+} // Assertions
  Cnt := 0;
	{$ENDIF}
  for i := 0 to fBucketCount - 1 do
  begin
    Pre := PBucketForIndex(i);
    Start := Pre^;
    while Assigned(Pre^) do
    begin
    	{$IFOPT C+} // Assertions
      Inc(cnt);
      {$ENDIF}
      Assert(IsCorrectlyIndexed(Pre^.Item));
      Pre := @(Pre^.Next);
      Assert(pre^ <> Start);
    end;
  end;
	{$IFOPT C+} // Assertions
  Assert(Count = Cnt, Format('%d, %d', [Count, cnt]));
  {$ENDIF}
end;

{ TBoldIntegerIndex }

procedure TBoldIntegerIndex.Add(Item: TObject);
begin
  fList.Add(item);
end;

procedure TBoldIntegerIndex.Clear(DestroyObjects: Boolean=false);
var
  i: integer;
  temp: TObject;
begin
  if DestroyObjects then
  begin
    for i := 0 to Count - 1 do
    begin
      temp := fList[i];
      fList[i] := nil;
      temp.Free;
    end;
  end;
  fList.Clear;
end;

constructor TBoldIntegerIndex.Create;
begin
  fList := TList.Create;
end;

function TBoldIntegerIndex.Find(const Key): TObject;
begin
  // Note: Count is actually an integer
  if Cardinal(Key) < Cardinal(Count) then
    result := fList[Cardinal(Key)]
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

function TBoldIntegerIndex.GetCount: Integer;
begin
  result := fList.Count;
end;

function TBoldIntegerIndex.GetAny: TObject;
begin
  if Count > 0 then
    result := fList[0]
  else
    result := nil;
end;

function TBoldIntegerIndex.IsCorrectlyIndexed(Item: TObject): boolean;
begin
  result := true;
end;

function TBoldIntegerIndex.Match(const Key; Item: TObject): Boolean;
begin
  // Note: Count is actually an integer
  if Cardinal(Key) < Cardinal(Count) then
    Result := item = fList[Cardinal(key)]
  else
    Result := False;
end;

procedure TBoldIntegerIndex.Remove(Item: TObject);
begin
  fList.Remove(item);
end;

procedure TBoldIntegerIndex.RemoveChanged(Item: TObject);
begin
  fList.Remove(item);
end;

function TBoldIntegerIndex.GetItems(index: integer): TObject;
begin
  result := fList[index];
end;

procedure TBoldIntegerIndex.SetItems(index: integer; const Value: TObject);
begin
  fList[index] := value;
end;

procedure TBoldIntegerIndex.Exchange(Index1, Index2: integer);
begin
  fList.Exchange(index1, index2);
end;

function TBoldIntegerIndex.IndexOf(Item: TObject): integer;
begin
  result := fList.IndexOf(item);
end;

procedure TBoldIntegerIndex.Move(CurIndex, NewIndex: Integer);
begin
  fList.Move(curIndex, NewIndex);
end;

procedure TBoldIntegerIndex.Sort(Compare: TListSortCompare);
begin
  fList.Sort(Compare);
end;

procedure TBoldIntegerIndex.Insert(Index: Integer; Item: TObject);
begin
  fList.Insert(index, item);
end;

procedure TBoldIntegerIndex.RemoveByIndex(Index: Integer);
begin
  fList.Delete(index);
end;

procedure TBoldIntegerIndex.FillEmptyIndex(BoldIndex: TBoldIndex);
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    BoldIndex.Add(items[i])
end;

procedure TBoldIntegerIndex.ItemChanged(Item: TObject);
begin
  // do nothing
end;

destructor TBoldIntegerIndex.Destroy;
begin
  FreeAndNil(fList);
  inherited;
end;

function TBoldIntegerIndex.GetSupportsNilItems: Boolean;
begin
  result := true;
end;

{ TBoldHashIndexTraverser }

constructor TBoldHashIndexTraverser.Create(HashIndex: TBoldHashIndex);
begin
  inherited Create;
  fHashIndex := HashIndex;
  fBucketIndex := -1;
  fNextItem := FirstItemOfNextBucket;
  Next;
end;

destructor TBoldHashIndexTraverser.Destroy;
begin
  inherited;
  fHashIndex.DecreaseTraverser;
end;

function TBoldHashIndexTraverser.FirstItemOfNextBucket: PItemEntry;
begin
  result := nil;
  while not Assigned(Result) and (fBucketIndex < (fHashIndex.fBucketCount-1)) do
  begin
    inc(fBucketIndex);
    result := fHashIndex.PBucketForIndex(fBucketIndex)^;
  end;
end;

function TBoldHashIndexTraverser.GetEol: Boolean;
begin
  result := not assigned(fCurrentItem);
end;

function TBoldHashIndexTraverser.GetItem: TObject;
begin
  result := fCurrentItem.Item;
end;

procedure TBoldHashIndexTraverser.Next;
begin
  fCurrentItem := fNextItem;
  if assigned(fNextItem) and assigned(fNextItem.Next) then
    fNExtItem := fNextItem.Next
  else
    fNextItem := FirstItemOfNextBucket;
end;

end.
