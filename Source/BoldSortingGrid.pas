
{ Global compiler directives }
{$include bold.inc}
unit BoldSortingGrid;

interface

uses
  controls,
  classes,
  grids,
  Windows,
  BoldSystem,
  BoldElements,
  BoldSubscription,
  BoldControlPack,
  BoldSortedHandle,
  BoldListHandle,
  BoldGrid;

type
  TSortMethod = (smElement, smString, smDefault);

  TBoldSortingGrid = class(TBoldgrid)
  private
    fComparer: TBoldComparer;
    FOrderCol: integer;
    FOrderDescending: Boolean;
    fLastOrderCol: integer;
    fEnableSorting: Boolean;
    fLastMouseDown: TPoint;
    fOnPrepareSorting: TNotifyEvent;
    fUnsavedObjectsLast: Boolean;
    fDefaultSortMethod: TSortMethod;
    fPreparingToSort: integer;
    fUseIntelligentFetching: Boolean;
    procedure SetOrderCol(const Value: integer);
    procedure SetOrderDescending(const Value: Boolean);
    function GetComparer: TBoldComparer;
    procedure ComparerSubscribe(Element: TBoldElement; Subscriber: TBoldSubscriber);
    function ComparerCompare(Item1, Item2: TBoldElement): Integer;
    procedure PrepareSort(list: TBoldList);
    function GetListHandle: TBoldListHandle;
    procedure SetEnableSorting(const Value: Boolean);
    function GetStringFromElement(el: TBoldElement): String;
    function AdjustStringForNumericCompare(s: String): String;
    procedure SetUnsavedObjectsLast(const Value: Boolean);
    procedure SetDefaultSortMethod(const Value: TSortMethod);
    function GetEffectiveSortMethod(Col: integer): TSortMethod;
    procedure FetchIntelligently;
  protected
    procedure DrawSortIndicator(ARect: TRect);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(BUTTON: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Resort;
    procedure _AfterMakeListUptoDate(Follower: TBoldFollower); override;
    function GetSortMode(SortCol: integer): TSortMethod; virtual;
    property Comparer: TBoldComparer read GetComparer;
    property ListHandle: TBoldListHandle read GetListHandle;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    procedure DrawCell(ACol, aRow: Longint; ARect: TRect; AState: TGridDrawState); override;
    property OrderCol: integer read FOrderCol write SetOrderCol;
    property OrderDescending: Boolean read FOrderDescending write SetOrderDescending;
    property EnableSorting: Boolean read fEnableSorting write SetEnableSorting default true;
    property EffectiveSortMethod[Col: integer]: TSortMethod read GetEffectiveSortMethod;
    property UseIntelligentFetching: Boolean read fUseIntelligentFetching write fUseIntelligentFetching;
  published
    property OnPrepareSorting: TNotifyEvent read fOnPrepareSorting write fOnPrepareSorting;
    property UnsavedObjectsLast: Boolean read fUnsavedObjectsLast write SetUnsavedObjectsLast;
    property DefaultSortMethod: TSortMethod read fDefaultSortMethod write SetDefaultSortMethod;
  end;

implementation

uses
  SysUtils,
  Graphics,
  BoldDefs;

{ TBoldSortingGrid }

function TBoldSortingGrid.AdjustStringForNumericCompare(s: String): String;
var
  IsInteger: Boolean;
  i: integer;
begin
  IsInteger := length(s) > 0;
  for i := 1 to length(s) do
    IsInteger := IsInteger and (s[i] in [' ', '0'..'9']);
  if IsInteger then
  begin
    result := trim(s);
    result := StringOfChar('0', 20-length(result))+Result;
  end
  else
    result := s;
end;

function TBoldSortingGrid.ComparerCompare(Item1, Item2: TBoldElement): Integer;
var
  ie1, ie2: TBoldIndirectElement;
  Obj1, Obj2: TBoldObject;
  s1, s2: String;
begin
  if UnsavedObjectsLast and (Item1 is TBoldObject) and (Item2 is TBoldObject) then
  begin
    Obj1 := Item1 as TBoldObject;
    Obj2 := Item2 as TBoldObject;
    if Obj2.BoldObjectIsNew and Obj1.BoldObjectIsNew then
    begin
      result := StrToIntDef(Obj1.BoldObjectLocator.BoldObjectId.AsString, 0) - StrToIntDef(Obj2.BoldObjectLocator.BoldObjectId.AsString, 0);
      exit;
    end
    else if Obj1.BoldObjectIsNew then
    begin
      result := 1;
      exit;
    end else if Obj2.BoldObjectIsNew then
    begin
      result := -1;
      exit;
    end;
  end;
  case EffectiveSortMethod[FOrderCol] of
    smElement:
    begin
      ie1 := TBoldIndirectElement.create;
      ie2 := TBoldIndirectElement.create;
      try
        Item1.EvaluateExpression(Columns[orderCol].BoldProperties.Expression, ie1);
        Item2.EvaluateExpression(Columns[orderCol].BoldProperties.Expression, ie2);
        if assigned(ie1.value) and assigned(ie2.value) then
          result := ie1.Value.CompareTo(ie2.Value)
        else if assigned(ie1.value) then
          result := 1
        else if assigned(ie1.value) then
          result := -1
        else
          result := 0;
      finally
        ie1.Free;
        ie2.Free;
      end;
    end;
    smString:
    begin
      s1 := GetStringFromElement(item1);
      s2 := GetStringFromElement(item2);
      result := AnsiCompareText(s1, s2);
    end
    else result := 0;
  end;

  if OrderDescending then
    result := -result;
end;

procedure TBoldSortingGrid.ComparerSubscribe(Element: TBoldElement; Subscriber: TBoldSubscriber);
begin
  Columns[orderCol].BoldProperties.SubscribeToElement(element, Subscriber);
end;

constructor TBoldSortingGrid.create(aOwner: TComponent);
begin
  inherited;
  fEnableSorting := true;
  fOrderCol := -1;
  fLastOrderCol := -1;
  fUnsavedObjectsLast := false;
  fDefaultSortMethod := smElement;
end;

destructor TBoldSortingGrid.destroy;
begin
  FreeAndNil(fComparer);
  inherited;
end;

procedure TBoldSortingGrid.DrawCell(ACol, aRow: Integer; ARect: TRect;
  AState: TGridDrawState);
begin
  inherited;
  if (aRow = 0) and (aCol = OrderCol) then
    DrawSortIndicator(aRect);
end;

procedure TBoldSortingGrid.DrawSortIndicator(ARect: TRect);
var
  x, y: Integer;
begin
  x := ARect.Right - 5;
  y := ((ARect.Bottom - ARect.Top) div 2);

  if (not OrderDescending) then
  begin
    Canvas.Pen.Color := cl3DDkShadow;
    Canvas.MoveTo(x, y - 3);
    Canvas.LineTo(x - 7, y - 3);
    Canvas.LineTo(x - 4, y + 3);
    Canvas.Pen.Color := clWhite;
    Canvas.MoveTo(x - 3, y + 3);
    Canvas.LineTo(x, y - 3);
  end
  else
  begin
    Canvas.Pen.Color := clWhite;
    Canvas.MoveTo(x - 7, y + 3);
    Canvas.LineTo(x, y + 3);
    Canvas.LineTo(x - 3, y - 3);
    Canvas.Pen.Color := cl3DDkShadow;
    Canvas.MoveTo(x - 4, y - 3);
    Canvas.LineTo(x - 7, y + 3);
  end;
end;

function TBoldSortingGrid.GetComparer: TBoldComparer;
begin
  if not assigned(fComparer) then
  begin
    fComparer := TBoldComparer.Create(self);
    fComparer.Name := name+'_Comparer';
    fComparer.OnSubscribe := ComparerSubscribe;
    fComparer.OnCompare := ComparerCompare;
    fComparer.OnPrepareSort := PrepareSort;
  end;
  result := fComparer;
end;

function TBoldSortingGrid.GetListHandle: TBoldListHandle;
begin
  if BoldHandle is TBoldListHandle then
    result := BoldHandle as TBoldListHandle
  else
    result := nil;
end;

function TBoldSortingGrid.GetSortMode(SortCol: integer): TSortMethod;
begin
  result := smDefault;
end;


function TBoldSortingGrid.GetStringFromElement(el: TBoldElement): String;
begin
  result := AdjustStringForNumericCompare(Columns[fOrderCol].BoldProperties.GetAsString(el));
  if fLastOrderCol <> -1 then
    result := result + AdjustStringForNumericCompare(Columns[fLastOrderCol].BoldProperties.GetAsString(el));
end;

procedure TBoldSortingGrid.MouseDown(BUTTON: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  fLastMouseDown.x := x;
  fLastMouseDown.y := y;
end;

procedure TBoldSortingGrid.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Col: integer;
begin
  inherited;
  if (FixedRows = 0) or (csDesigning in ComponentState) or (y > RowHeights[0]) then
    exit;
  if not EnableSorting then
    exit;
  if (x <> fLastMouseDown.x) or (y <> fLastMouseDown.y) then
    exit;
    
  Col := 0;
  while (x > 0) and (Col < ColCount) do
  begin
    if (Col < FixedCols) or (Col >= LeftCol) then
    begin
      dec(x, ColWidths[Col]);
    end;
    inc(col);
  end;
  if (x <= FixedCols) and (Col <= ColCount) then
  begin
    if OrderCol = Col-1 then
    begin
      OrderDescending := not OrderDescending;
    end
    else
    begin
      fLastOrderCol := OrderCol;
      OrderCol := Col-1;
      OrderDescending := false;
    end;
    ReSort;
  end;
end;

procedure TBoldSortingGrid.Resort;
begin
  if (orderCol <> -1) and not (csDesigning in ComponentState) then
  begin
    if assigned(fOnPrepareSorting) then
      fOnPrepareSorting(self);

    FetchIntelligently;

    if assigned(ListHandle) and
      assigned(ListHandle.BoldComparer) and
      (ListHandle.BoldComparer.Owner is tBoldSortingGrid) then
      ListHandle.BoldComparer := nil;

    if assigned(ListHandle) and not assigned(ListHandle.BoldComparer) and EnableSorting then
      ListHandle.BoldComparer := Comparer;
  end
  else
  begin
    if assigned(ListHandle) and (ListHandle.BoldComparer = Comparer) then
      ListHandle.BoldComparer := nil;
  end;
end;

procedure TBoldSortingGrid.SetEnableSorting(const Value: Boolean);
begin
  fEnableSorting := Value;
  if not fEnableSorting then
  begin
    fOrderCol := -1;
    fLastOrderCol := -1;
    ReSort;
  end;
end;

procedure TBoldSortingGrid.SetUnsavedObjectsLast(const Value: Boolean);
begin
  fUnsavedObjectsLast := Value;
  ReSort;
end;

procedure TBoldSortingGrid.SetOrderCol(const Value: integer);
begin
  FOrderCol := Value;
  ReSort;
end;

procedure TBoldSortingGrid.SetOrderDescending(const Value: Boolean);
begin
  FOrderDescending := Value;
  ReSort;
end;


procedure TBoldSortingGrid.SetDefaultSortMethod(const Value: TSortMethod);
begin
  if Value = smDefault then
    raise EBold.Create('Value "Default" not valid for this property');
  fDefaultSortMethod := Value;
  ReSort;
end;

function TBoldSortingGrid.GetEffectiveSortMethod(Col: integer): TSortMethod;
begin
  result := GetSortMode(Col);
  if result = smDefault then
    Result := DefaultSortMethod;
end;

procedure TBoldSortingGrid.PrepareSort(list: TBoldList);
begin
  FetchIntelligently;
end;

procedure TBoldSortingGrid._AfterMakeListUptoDate(Follower: TBoldFollower);
begin
  inherited;
  if (fPreparingToSort = 0) and (Follower.SubFollowerCount = 0) then
  begin
    fLastOrderCol := -1;
    OrderCol := -1;
  end;
end;

procedure TBoldSortingGrid.FetchIntelligently;
begin
  if UseIntelligentFetching then
  begin
    inc(fPreparingToSort);
    DisplayAllCells;
    dec(fPreparingToSort);
  end;
end;

end.
