
{ Global compiler directives }
{$include bold.inc}
unit BoldVclUtils;

interface

uses
  grids,
  controls,
  comctrls;

  {porcedures for manipulating a grid}
  procedure ExchangeRows(var StringGrid: TStringGrid; a, b: Integer); overload;
  procedure ExchangeRows(var ListView: TListView; a, b: integer); overload;
  procedure ShiftUp(var StringGrid: TStringGrid; StartRow: Integer);
  procedure ShiftDown(var StringGrid: TStringGrid; StartRow: Integer);
  procedure AppendRow(var StringGrid: TStringGrid);
  procedure DeleteRow(var StringGrid: TStringGrid; ARow: Integer);
  procedure SelectCell(var StringGrid: TStringGrid; const Col, Row: integer);
  function IsEmptyStr(const str: string): Boolean;

implementation

uses
  Sysutils,
  BoldUtils,
  classes;


procedure ExchangeRows(var StringGrid: TStringGrid; a, b: Integer);
var
  i: Integer;
begin
  if ((a < StringGrid.RowCount) AND (b < StringGrid.RowCount) AND (a <> 0) AND (b <> 0))  then
    for i:= 0 to StringGrid.ColCount - 1 do
      StringGrid.Cols[i].Exchange(a,b);
end;

procedure ExchangeRows(var ListView: TListView; a, b: integer);
var
  temp: string;
  i: integer;
  item1, item2: TListItem;
begin
  if ((a >= 0) and (b >= 0) and (a < ListView.Items.Count) and (b < ListView.Items.Count)) then
  begin
    ListView.Items.BeginUpdate;
    item1 := ListView.Items[a];
    item2 := ListView.Items[b];
    temp := item1.Caption;
    item1.Caption := item2.Caption;
    item2.caption := temp;
    for i:= 0 to ListView.Columns.Count - 2 do
    begin
      temp := item1.SubItems[i];
      item1.SubItems[i] := item2.SubItems[i];
      item2.SubItems[i] := temp;
    end;
    ListView.Items.EndUpdate;
  end;
end;

procedure ShiftUp(var StringGrid: TStringGrid; StartRow: Integer);
var
  i: integer;
begin
  if ((StartRow >= StringGrid.FixedRows) and (StartRow < StringGrid.RowCount)) then
    for i:= StartRow to StringGrid.RowCount - 2 do
      ExchangeRows(StringGrid,i, i+1);
end;

procedure ShiftDown(var StringGrid: TStringGrid; StartRow: Integer);
var
  i: Integer;
begin
  if ((StartRow >= StringGrid.FixedRows) AND (StartRow < StringGrid.RowCount)) then
  begin
    AppendRow(StringGrid);
    for i := (StringGrid.RowCount -1) downto (StartRow + 1) do
       ExchangeRows(StringGrid,i, i - 1);
  end;
end;

procedure SelectCell(var StringGrid: TStringGrid; const Col, Row: integer);
var
 t: TGridRect;
begin
  t.Left := Col ;
  t.Right := Col ;
  t.Top := Row;
  t.Bottom := Row;
  StringGrid.Selection := t;
end;

procedure AppendRow(var StringGrid: TStringGrid);
begin
  if not IsEmptyStr(Trim(StringGrid.Rows[StringGrid.RowCount - 1].Text)) then
  begin
    StringGrid.RowCount := StringGrid.RowCount + 1;
    StringGrid.Rows[StringGrid.RowCount - 1].Text:= '';
  end;
end;

procedure DeleteRow(var StringGrid: TStringGrid; ARow: Integer);
begin
  if (StringGrid.FixedRows < StringGrid.RowCount - 1) then
  begin
    ShiftUp(StringGrid, ARow);
    StringGrid.RowCount := StringGrid.RowCount - 1;
  end
  {trying to delete the first row}
  else if (ARow = 1) then
    StringGrid.Rows[1].Text := '';
end;

function IsEmptyStr(const str: string): Boolean;
begin
  Result := (length(trim(str)) = 0);
end;

end.
