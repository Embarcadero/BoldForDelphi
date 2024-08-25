
{ Global compiler directives }
{$include bold.inc}
unit BoldContainers;

interface

uses
  BoldDefs,
  BoldBase;

type
  { forward declarations }
  TBoldContainer = class;
  TBoldArray = class;
  TBoldPointerArray = class;
  TBoldObjectArray = class;
  TBoldInterfaceArray = class;
  TBoldIntegerArray = class;

  { Exceptions }
  EBoldContainerError = class(EBold);

  TBoldContainerOption = (bcoDataOwner, bcoThreadSafe);
  TBoldContainerOptions = set of TBoldContainerOption;
  TBoldArraySortCompare = function (Item1, Item2: Pointer): Integer;

  PByteArray = ^TByteArray;
  TByteArray = array [0..MaxInt - 1] of Byte;

  {-- TBoldContainer --}
  TBoldContainer = class(TBoldNonRefCountedObject)
  private
    FOptions: TBoldContainerOptions;
  protected
    function GetCount: Integer; virtual; abstract;
    procedure SetCount(Value: Integer); virtual; abstract;
  public
    constructor Create(Options: TBoldContainerOptions);
    procedure Clear; virtual; abstract;
    property Count: Integer read GetCount write SetCount;
    property Options: TBoldContainerOptions read FOptions;
  end;

  {-- TBoldArray --}
  TBoldArray = class(TBoldContainer)
  private
    FArray: PByteArray;
    FCapacity: Integer;
    FCount: Integer;
    procedure AddItems(const Items; NumItems: Integer);
    procedure EnsureCapacity;
    function GetCapacity: Integer;
    procedure MoveItems(FromIndex, ToIndex, NumItems: Integer);
    procedure SetCapacity(Value: Integer);
  protected
    function Add(const Item): Integer;
    function AddArray(BoldArray: TBoldArray): Integer;
    procedure Dispose(Index: Integer); virtual;
    procedure Get(Index: Integer; var Item);
    function GetCount: Integer; override;
    function GetGrowDelta: Integer; virtual;
    function GetItemSize: Integer; virtual; abstract;
    function IndexOf(const Item): Integer;
    procedure Insert(Index: Integer; const Item);
    procedure Put(Index: Integer; const Item);
    function Remove(const Item): Integer;
    function RemoveWithNil(const Item): Integer;
    procedure SetCount(Value: Integer); override;
    procedure Slice(StartIndex, NumItems: Integer; BoldArray: TBoldArray);
  public
    constructor Create(InitialCapacity: Integer; Options: TBoldContainerOptions);
    destructor Destroy; override;
    procedure Clear; override;
    procedure Delete(Index: Integer);
    procedure DeleteRange(FromIndex, ToIndex: integer);
    procedure Exchange(Index1, Index2: Integer);
    procedure Move(FromIndex, ToIndex: Integer);
    procedure Pack;
    procedure Sort(Compare: TBoldArraySortCompare; SortMode: TBoldSortMode =
        BoldDefaultSortMode); overload;
    procedure Sort(CompareFunc: TBoldArraySortCompare; FirstIndex, LastIndex:
        Integer; SortMode: TBoldSortMode = BoldDefaultSortMode); overload;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property ItemSize: Integer read GetItemSize;
  end;

  { TBoldPointerArray }
  TBoldPointerArray = class(TBoldArray)
  private
    function Get(Index: Integer): Pointer;
    procedure Put(Index: Integer; Item: Pointer);
  protected
    function GetItemSize: Integer; override;
  public
    function Add(Item: Pointer): Integer;
    function IndexOf(Item: Pointer): Integer;
    procedure Insert(Index: Integer; Item: Pointer);
    function Remove(Item: Pointer): Integer;
    function RemoveWithNil(Item: Pointer): Integer;
    property Items[Index: Integer]: Pointer read Get write Put; default;
  end;

  TBoldArrayTraverser = class(TBoldMemoryManagedObject)
  private
    FIndex: Integer;
    FArray: TBoldObjectArray;
  protected
    property Index: Integer read fIndex;
    property ObjectArray: TBoldObjectArray read fArray;    
  public
    constructor Create(AArray: TBoldObjectArray);
    function GetCurrent: TObject;
    function MoveNext: Boolean;
  end;

  { TBoldObjectArray }
  TBoldObjectArray = class(TBoldArray)
  private
    function Get(Index: Integer): TObject;
    procedure Put(Index: Integer; Item: TObject);
  protected
    procedure Dispose(Index: Integer); override;
    function GetItemSize: Integer; override;
  public
    function GetEnumerator: TBoldArrayTraverser;
    function Add(Item: TObject): Integer;
    function IndexOf(Item: TObject): Integer;
    procedure Insert(Index: Integer; Item: TObject);
    function Remove(Item: TObject): Integer;
    function RemoveWithNil(Item: TObject): Integer;
    property Items[Index: Integer]: TObject read Get write Put; default;
  end;

  { TBoldInterfaceArray }
  TBoldInterfaceArray = class(TBoldArray)
  private
    function Get(Index: Integer): IUnknown;
    procedure Put(Index: Integer; const Item: IUnknown);
  protected
    procedure Dispose(Index: Integer); override;
    function GetItemSize: Integer; override;
  public
    function Add(const Item: IUnknown): Integer;
    function IndexOf(const Item: IUnknown): Integer;
    procedure Insert(Index: Integer; const Item: IUnknown);
    function Remove(const Item: IUnknown): Integer;
    function RemoveWithNil(const Item: IUnknown): Integer;
    property Items[Index: Integer]: IUnknown read Get write Put; default;
  end;

  { TBoldIntegerArray }
  TBoldIntegerArray = class(TBoldArray)
  private
    function Get(Index: Integer): integer;
    procedure Put(Index: Integer; const Item: integer);
  protected
    procedure Dispose(Index: Integer); override;
    function GetItemSize: Integer; override;
  public
    function Add(const Item: integer): Integer;
    function IndexOf(const Item: integer): Integer;
    procedure Insert(Index: Integer; const Item: integer);
    function Remove(const Item: integer): Integer;
    property Items[Index: Integer]: integer read Get write Put; default;
  end;

resourcestring
  ECapacityLessThanCount = 'New capacity cannot be less than count';
  ECountLessThanCount = 'New count cannot be less than count';
  EIndexOutOfBounds = 'Index out of bounds';

implementation

uses
  SysUtils;

{-- TBoldContainer ------------------------------------------------------------}

constructor TBoldContainer.Create(Options: TBoldContainerOptions);
begin
  inherited Create;
  FOptions := Options;
end;

{-- TBoldArray ----------------------------------------------------------------}

constructor TBoldArray.Create(InitialCapacity: Integer; Options: TBoldContainerOptions);
begin
  inherited Create(Options);
  SetCapacity(InitialCapacity);
end;

destructor TBoldArray.Destroy;
begin
  Clear;
  inherited;
end;

procedure TBoldArray.Sort(CompareFunc: TBoldArraySortCompare; FirstIndex,
    LastIndex: Integer; SortMode: TBoldSortMode = BoldDefaultSortMode);

  //////////////////////////////////////////////////////////////////////////////
  // Insertion Sort:                                                          //
  // stable, inplace, but only fast on small lists                            //
  //////////////////////////////////////////////////////////////////////////////
  procedure InsertSort(L, R: Integer; SCompare: TBoldArraySortCompare);
  var
    I, J: Integer;
    T: Pointer;
  begin
    GetMem(T, ItemSize);
    for I := L + 1 to R do begin
      if SCompare(@FArray^[I * ItemSize],
                  @FArray^[(I - 1) * ItemSize]) < 0 then
      begin
        J := I;
        Get(J, T^);
        while (J > L) and
              (SCompare(T, @FArray^[(J - 1) * ItemSize]) < 0) do
        begin
          System.Move(FArray^[(J - 1) * ItemSize],
                      FArray^[J * ItemSize],
                      ItemSize);
          Dec(J);
        end;
        Put(J, T^);
      end;
    end;
    FreeMem(T, ItemSize);
  end;

  //////////////////////////////////////////////////////////////////////////////
  // Quick Sort:                                                              //
  // fast, inplace (without help array),                                      //
  // but NOT stable (sorting changes within same elements)                    //
  //////////////////////////////////////////////////////////////////////////////
  procedure QuickSort(Left, Right: Integer; SCompare: TBoldArraySortCompare);
  var
    TempSize: Integer;
    I, J: Integer;
    P: Pointer;
  begin
    TempSize := ItemSize;
    GetMem(P, TempSize);
    try
      repeat
        I := Left;
        J := Right;
        System.Move(FArray^[((Left + Right) shr 1) * TempSize], P^, TempSize);
        repeat
          while SCompare(@FArray^[I * ItemSize], P) < 0 do
            Inc(I);
          while SCompare(@FArray^[J * ItemSize], P) > 0 do
            Dec(J);
          if I <= J then
          begin
            if I <> J then
            begin
              Exchange(I, J);
            end;
            Inc(I);
            Dec(J);
          end;
        until I > J;
        if Left < J then
          QuickSort(Left, J, SCompare);
        Left := I;
      until I >= Right;
    finally
      FreeMem(P);
    end;
  end;
{
  //////////////////////////////////////////////////////////////////////////////
  // Merge Sort - Inplace Variant:                                            //
  // http://thomas.baudel.name/Visualisation/VisuTri/inplacestablesort.html   //
  // stable, inplace, but slower than Quicksort and normal Mergesort          //
  //////////////////////////////////////////////////////////////////////////////
  function Lower(Left, Right, Val: Integer; SCompare: TBoldElementCompare):
      Integer;
  var
    iLen: Integer;
    iHalf: Integer;
    iMid: Integer;
  begin
    iLen := Right - Left;
    while iLen > 0 do begin
      iHalf := iLen div 2;
      iMid := Left + iHalf;
      if SCompare(Elements[iMid], Elements[Val]) < 0 then begin
        Left := iMid + 1;
        iLen := iLen - iHalf - 1;
      end else begin
        iLen := iHalf;
      end;
    end;
    Result := Left;
  end;

  function Upper(Left, Right, Val: Integer; SCompare: TBoldElementCompare):
      Integer;
  var
    iLen: Integer;
    iHalf: Integer;
    iMid: Integer;
  begin
    iLen := Right - Left;
    while iLen > 0 do begin
      iHalf := iLen div 2;
      iMid := Left + iHalf;
      if SCompare(Elements[Val], Elements[iMid]) < 0 then begin
        iLen := iHalf;
      end else begin
        Left := iMid + 1;
        iLen := iLen - iHalf - 1;
      end;
    end;
    Result := Left;
  end;

  function GCD(M, N: Integer): Integer;
  var
    T: Integer;
  begin
    while (N <> 0) do begin
      T := M mod N;
      M := N; N := T;
    end;
    Result := M;
  end;

  procedure Rotate(Left, Middle, Right: Integer; SCompare: TBoldElementCompare);
  var
    N: Integer;
    SavedElement: TBoldElement;
    Shift: Integer;
    P1, P2: Integer;
  begin
    if (Left <> Middle) and (Right <> Middle) then begin
      N := GCD(Right - Left, Middle - Left);
      while N <> 0 do begin
        Dec(N);
        SavedElement := Elements[Left + N];
        Shift := Middle - Left;
        P1 := Left + N;
        P2 := Left + N + Shift;
        while (P2 <> Left + N)  do begin
          Elements[P1] := Elements[P2];
          P1 := P2;
          if Right - P2 > Shift then begin
            Inc(P2, Shift);
          end else begin
            P2 := Left + (Shift - (Right - P2));
          end;
        end;
        Elements[P1] := SavedElement;
      end;
    end;
  end;

  procedure MergeInplace(Left, Pivot, Right, Len1, Len2: Integer; SCompare:
      TBoldElementCompare);
  var
    iFirstCut, iSecondCut: Integer;
    iLen11, iLen22: Integer;
    iNewMid: Integer;
  begin
    if (Len1 <> 0) and (Len2 <> 0) then begin
      if Len1 + Len2 = 2 then begin
        if SCompare(Elements[Pivot], Elements[Left]) < 0 then begin
          if Pivot < Left then begin
            Move(Pivot, Left);
            Move(Left - 1, Pivot);
          end else begin
            Move(Left, Pivot);
            Move(Pivot - 1, Left);
          end;
        end;
      end else begin
        if Len1 > Len2 then begin
          iLen11 := Len1 div 2;
          iFirstCut := Left + iLen11;
          iSecondCut := Lower(Pivot, Right, iFirstCut, SCompare);
          iLen22 := iSecondCut - Pivot;
        end else begin
          iLen22 := Len2 div 2;
          iSecondCut := Pivot + iLen22;
          iFirstCut := Upper(Left, Pivot, iSecondCut, SCompare);
          iLen11 := iFirstCut - Left;
        end;
        Rotate(iFirstCut, Pivot, iSecondCut, SCompare);
        iNewMid := iFirstCut + iLen22;
        MergeInplace(Left, iFirstCut, iNewMid, iLen11, iLen22, SCompare);
        MergeInplace(iNewMid, iSecondCut, Right, Len1 - iLen11, Len2 - iLen22, SCompare);
      end;
    end;
  end;

  procedure MergeSortInplace(Left, Right: Integer; SCompare: TBoldElementCompare);
  var
    Middle: Integer;
  begin
    if Right - Left < 8 then begin
      InsertSort(Left, Right, SCompare);
    end else begin
      Middle := (Left + Right) div 2;
      MergeSortInplace(Left, Middle, SCompare);
      MergeSortInplace(Middle, Right, SCompare);
      MergeInplace(Left, Middle, Right, Middle - Left, Right - Middle, SCompare);
    end;
  end;
}
  //////////////////////////////////////////////////////////////////////////////
  // Merge Sort:                                                              //
  // http://www.iti.fh-flensburg.de/lang/algorithmen/sortieren/merge/merge.htm//
  // fastest, stable,                                                         //
  // but not fully inplace (help array with only n/2 is needed)               //
  //////////////////////////////////////////////////////////////////////////////
  procedure MergeSort(Left, Right: Integer; SCompare: TBoldArraySortCompare);
  var
    HelpArray: PByteArray;

    procedure DoMergeSort(Left, Right: Integer; SCompare: TBoldArraySortCompare);
    var
      m: Integer;
      i, j, k: Integer;
    begin
      if Left < Right then begin
        if Right - Left < 8 then begin
          InsertSort(Left, Right, SCompare);
        end else begin
          m := (Left + Right) div 2;
          DoMergeSort(Left, m, SCompare);
          DoMergeSort(m + 1, Right, SCompare);

          j := m + 1;
          // Copy first half of elements in help array
          System.Move(FArray^[Left * ItemSize],
                      HelpArray^[0],
                      (j - Left) * ItemSize);

          i := 0;
          k := Left;
          // Copy back the next largest element
          while (k < j) and (j <= Right) do begin
            if SCompare(@HelpArray^[i * ItemSize],
                        @FArray^[J * ItemSize]) <= 0 then
            begin
              System.Move(HelpArray^[i * ItemSize],
                          FArray^[k * ItemSize],
                          ItemSize);
              Inc(i);
            end else begin
              System.Move(FArray^[j * ItemSize],
                          FArray^[k * ItemSize],
                          ItemSize);
              Inc(j);
            end;
            Inc(k);
          end;

          // Copy back the rest of help array if existing
          if k < j then begin
            System.Move(HelpArray^[i * ItemSize],
                        FArray^[k * ItemSize],
                        (j - k) * ItemSize);
          end;
        end;
      end;
    end;

  var
    TempSize: Integer;
  begin
    TempSize := ((Count + 1) div 2) * ItemSize;
    GetMem(HelpArray, TempSize);
    try
      DoMergeSort(FirstIndex, LastIndex, CompareFunc);
    finally
      FreeMem(HelpArray);
    end;
  end;

begin
  if Assigned(Self) and (Count > 1) then begin
    case SortMode of
      smQuickSort: QuickSort(FirstIndex, LastIndex, CompareFunc);
      smMergeSort: MergeSort(FirstIndex, LastIndex, CompareFunc);
      smMergeSortInplace: begin
//        MergeSortInplace(FirstIndex, LastIndex, CompareFunc);
        raise EBoldInternal.Create('TBoldArray.Sort: smMergeSortInplace is not supported here');
      end;
    end;
  end;
end;

procedure TBoldArray.Sort(Compare: TBoldArraySortCompare; SortMode:
    TBoldSortMode = BoldDefaultSortMode);
begin
  Sort(Compare, 0, Count - 1, SortMode);
end;

function TBoldArray.Add(const Item): Integer;
begin
  Result := FCount;
  EnsureCapacity;
  System.Move(Item,FArray^[Result*ItemSize],ItemSize);
  Inc(FCount);
end;

function TBoldArray.AddArray(BoldArray: TBoldArray): Integer;
begin
  AddItems(BoldArray.FArray,BoldArray.Count);
  Result := BoldArray.Count;
end;

procedure TBoldArray.AddItems(const Items; NumItems: Integer);
var
  RequiredCapacity: Integer;
begin
  RequiredCapacity := FCount + NumItems;
  if RequiredCapacity > FCapacity then
    SetCapacity(RequiredCapacity);
  System.Move(Items,FArray^[FCount * ItemSize], NumItems * ItemSize);
  Inc(FCount,NumItems);
end;

procedure TBoldArray.Clear;
var
  I: Integer;
begin
  if (bcoDataOwner in Options) then
    for I := 0 to Count - 1 do Dispose(I);
  SetCount(0);
  SetCapacity(0);
end;

procedure TBoldArray.MoveItems(FromIndex, ToIndex, NumItems: Integer);
begin
  if (FromIndex < 0) or (ToIndex < 0) or
     (FromIndex + NumItems > FCapacity) or
     (ToIndex + NumItems > FCapacity) then
  begin
    raise Exception.Create('Bad parameters to TBoldArray.MoveItems');
  end;

  System.Move(FArray^[FromIndex * ItemSize],
              FArray^[ToIndex * ItemSize],
              NumItems * ItemSize);
end;

procedure TBoldArray.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then
    raise EBoldContainerError.Create(EIndexOutOfBounds);
  if (bcoDataOwner in Options) then
    Dispose(Index);
  Dec(FCount);
  if Index < FCount then
    MoveItems(Index + 1, Index, FCount - Index);
end;

procedure TBoldArray.Dispose(Index: Integer);
begin
end;

procedure TBoldArray.EnsureCapacity;
begin
  if FCount = FCapacity then
    SetCapacity(FCapacity + GetGrowDelta);
end;

procedure TBoldArray.Exchange(Index1, Index2: Integer);
var
  Item: Pointer;
  TempSize: Integer;
begin
  if (Index1 < 0) or (Index1 >= Count) then
    raise EBoldContainerError.Create(EIndexOutOfBounds);
  if (Index2 < 0) or (Index2 >= Count) then
    raise EBoldContainerError.Create(EIndexOutOfBounds);
  if Index1 = Index2 then Exit;
  TempSize := ItemSize;
  GetMem(Item, TempSize);
  try
    System.Move(FArray^[Index1 * TempSize], Item^, TempSize);
    System.Move(FArray^[Index2 * TempSize], FArray^[Index1 * TempSize], TempSize);
    System.Move(Item^, FArray^[Index2 * TempSize], TempSize);
  finally
    FreeMem(Item);
  end;
end;

procedure TBoldArray.Get(Index: Integer; var Item);
begin
  if (Index < 0) or (Index >= FCount) then
    raise EBoldContainerError.Create(EIndexOutOfBounds);
  System.Move(FArray^[Index * ItemSize], Item, ItemSize);
end;

function TBoldArray.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

function TBoldArray.GetCount: Integer;
begin
  Result := FCount;
end;

function TBoldArray.GetGrowDelta: Integer;
begin
  if FCapacity >= 32 then
    Result := FCapacity div 4
  else
    Result := 8;
end;

function TBoldArray.IndexOf(const Item): Integer;
var
  TempSize: Integer;
begin
  Result := 0;
  TempSize := ItemSize;
  while (Result < FCount) and not CompareMem(@(FArray^[Result * TempSize]), @Item, TempSize) do
    Inc(Result);
  if Result = FCount then
    Result := -1;
end;

procedure TBoldArray.Insert(Index: Integer; const Item);
begin
  if (Index < 0) or (Index > FCount) then
    raise EBoldContainerError.Create(EIndexOutOfBounds);
  EnsureCapacity;
  if Index < FCount then
    MoveItems(Index, Index + 1, FCount - Index);
  System.Move(Item, FArray^[Index * ItemSize], ItemSize);
  Inc(FCount);
end;

procedure TBoldArray.Move(FromIndex, ToIndex: Integer);
var
  Item: Pointer;
begin
  if FromIndex <> ToIndex then
  begin
    GetMem(Item, ItemSize);
    try
      Get(FromIndex, Item^);
      Dec(FCount);
      if FromIndex < FCount then
        MoveItems(FromIndex + 1, FromIndex, FCount - FromIndex);
      Insert(ToIndex, Item^);
    finally
      FreeMem(Item);
    end;
  end;
end;

procedure TBoldArray.Pack;
begin
  SetCapacity(FCount);
end;

procedure TBoldArray.Put(Index: Integer; const Item);
begin
  if (Index < 0) or (Index >= FCount) then
    raise EBoldContainerError.Create(EIndexOutOfBounds);
  if (bcoDataOwner in Options) then
    Dispose(Index);
  System.Move(Item, FArray^[Index * ItemSize], ItemSize);
end;

function TBoldArray.Remove(const Item): Integer;
begin
  Result := IndexOf(Item);
  if Result <> -1 then
    Delete(Result);
end;

function TBoldArray.RemoveWithNil(const Item): Integer;
begin
  Result := IndexOf(Item);
  if Result <> -1 then
  begin
    if (bcoDataOwner in Options) then
      Dispose(Result);
    FillChar(FArray^[Result * ItemSize], ItemSize, 0);
  end;
end;

procedure TBoldArray.SetCapacity(Value: Integer);
begin
  if (Value < FCount) then
    raise EBoldContainerError.Create(ECapacityLessThanCount);
  if Value <> FCapacity then
  begin
    ReallocMem(FArray, Value * ItemSize);
    FCapacity := Value;
  end;
end;

procedure TBoldArray.SetCount(Value: Integer);
begin
  if (Value < 0) then
    raise EBoldContainerError.Create(ECountLessThanCount);
  if Value > FCapacity then
    SetCapacity(Value);
  if Value > FCount then
    FillChar(FArray^[FCount * ItemSize], (Value - FCount) * ItemSize, 0);
  FCount := Value;
end;

procedure TBoldArray.Slice(StartIndex, NumItems: Integer; BoldArray: TBoldArray);
begin
  if (StartIndex < 0) or (StartIndex >= FCount) then
    raise EBoldContainerError.Create(EIndexOutOfBounds);
  if NumItems > 0 then
  begin
    if StartIndex + NumItems <= FCount then
      BoldArray.AddItems(FArray^[StartIndex * ItemSize], NumItems)
    else
      raise EBoldContainerError.Create(EIndexOutOfBounds);
  end;
end;

{-- TBoldPointerArray ---------------------------------------------------------}

function TBoldPointerArray.Add(Item: Pointer): Integer;
begin
  Result := inherited Add(Item);
end;

function TBoldPointerArray.Get(Index: Integer): Pointer;
begin
  inherited Get(Index,Result);
end;

function TBoldPointerArray.GetItemSize: Integer;
begin
  Result := SizeOf(Pointer);
end;

function TBoldPointerArray.IndexOf(Item: Pointer): Integer;
begin
  Result := inherited IndexOf(Item);
end;

procedure TBoldPointerArray.Insert(Index: Integer; Item: Pointer);
begin
  inherited Insert(Index, Item);
end;

procedure TBoldPointerArray.Put(Index: Integer; Item: Pointer);
begin
  inherited Put(Index, Item);
end;

function TBoldPointerArray.Remove(Item: Pointer): Integer;
begin
  Result := inherited Remove(Item);
end;

function TBoldPointerArray.RemoveWithNil(Item: Pointer): Integer;
begin
  Result := inherited RemoveWithNil(Item);
end;

{-- TBoldObjectArray ----------------------------------------------------------}

function TBoldObjectArray.Add(Item: TObject): Integer;
begin
  Result := inherited Add(Item);
end;

procedure TBoldObjectArray.Dispose(Index: Integer);
begin
  Get(Index).Free;
end;

function TBoldObjectArray.Get(Index: Integer): TObject;
begin
  inherited Get(Index,Result);
end;

function TBoldObjectArray.GetEnumerator: TBoldArrayTraverser;
begin
  result := TBoldArrayTraverser.Create(self);
end;

function TBoldObjectArray.GetItemSize: Integer;
begin
  Result := SizeOf(TObject);
end;

function TBoldObjectArray.IndexOf(Item: TObject): Integer;
begin
  Result := inherited IndexOf(Item);
end;

procedure TBoldObjectArray.Insert(Index: Integer; Item: TObject);
begin
  inherited Insert(Index, Item);
end;

procedure TBoldObjectArray.Put(Index: Integer; Item: TObject);
begin
  inherited Put(Index, Item);
end;

function TBoldObjectArray.Remove(Item: TObject): Integer;
begin
  Result := inherited Remove(Item);
end;

function TBoldObjectArray.RemoveWithNil(Item: TObject): Integer;
begin
  Result := inherited RemoveWithNil(Item);
end;

{-- TBoldInterfaceArray -------------------------------------------------------}

function TBoldInterfaceArray.Add(const Item: IUnknown): Integer;
begin
  Result := inherited Add(Item);
  if (bcoDataOwner in Options) then
    Item._AddRef;
end;

procedure TBoldInterfaceArray.Dispose(Index: Integer);
var
  Unk: IUnknown;
begin
  Unk := Get(Index);
  if Assigned(Unk) then
    Unk._Release;
end;

function TBoldInterfaceArray.Get(Index: Integer): IUnknown;
var
  Ptr: Pointer;
begin
  inherited Get(Index, Ptr);
  Result := IUnknown(Ptr);
end;

function TBoldInterfaceArray.GetItemSize: Integer;
begin
  Result := SizeOf(Pointer);
end;

function TBoldInterfaceArray.IndexOf(const Item: IUnknown): Integer;
begin
  Result := inherited IndexOf(Item);
end;

procedure TBoldInterfaceArray.Insert(Index: Integer; const Item: IUnknown);
begin
  inherited Insert(Index,Item);
  if (bcoDataOwner in Options) then
    Item._AddRef;
end;

procedure TBoldInterfaceArray.Put(Index: Integer; const Item: IUnknown);
begin
  inherited Put(Index,Item);
  if (bcoDataOwner in Options) then
    Item._AddRef;
end;

function TBoldInterfaceArray.Remove(const Item: IUnknown): Integer;
begin
  Result := inherited Remove(Item);
end;

function TBoldInterfaceArray.RemoveWithNil(const Item: IUnknown): Integer;
begin
  Result := inherited RemoveWithNil(Item);
end;


{-- TBoldIntegerArray ---------------------------------------------------------}

function TBoldIntegerArray.Add(const Item: integer): Integer;
begin
  result := inherited add(item);
end;

procedure TBoldIntegerArray.Dispose(Index: Integer);
begin
end;

function TBoldIntegerArray.Get(Index: Integer): integer;
begin
  inherited Get(Index, Result);
end;

function TBoldIntegerArray.GetItemSize: Integer;
begin
  Result := SizeOf(integer);
end;

function TBoldIntegerArray.IndexOf(const Item: integer): Integer;
begin
  Result := 0;
  while (Result < Count) and (Item = Get(Result)) do
    Inc(Result);
  if Result = Count then
    Result := -1;
end;

procedure TBoldIntegerArray.Insert(Index: Integer; const Item: integer);
begin
  inherited Insert(Index, Item);
end;

procedure TBoldIntegerArray.Put(Index: Integer; const Item: integer);
begin
  inherited Put(Index, Item);
end;

function TBoldIntegerArray.Remove(const Item: integer): Integer;
begin
  Result := IndexOf(Item);
  if Result <> -1 then
    Delete(Result);
end;

procedure TBoldArray.DeleteRange(FromIndex, ToIndex: integer);
var
  i: integer;
begin
  if (FromIndex < 0) or (FromIndex >= FCount) then
    raise EBoldContainerError.Create(EIndexOutOfBounds);
  if (ToIndex < 0) or (ToIndex >= FCount) then
    raise EBoldContainerError.Create(EIndexOutOfBounds);
  if (bcoDataOwner in Options) then
    for i := FromIndex to ToIndex do
      Dispose(i);
  Dec(FCount, (toIndex - FromIndex + 1));
  if ToIndex < FCount then
    MoveItems(ToIndex + 1, FromIndex, FCount - ToIndex);
end;

{ TBoldArrayTraverser }

constructor TBoldArrayTraverser.Create(AArray: TBoldObjectArray);
begin
  inherited Create;
  FIndex := -1;
  FArray := AArray;
end;

function TBoldArrayTraverser.GetCurrent: TObject;
begin
  result := fArray[Index];
end;

function TBoldArrayTraverser.MoveNext: Boolean;
begin
  Result := Index < fArray.Count - 1;
  if Result then
    Inc(FIndex);
end;

end.
