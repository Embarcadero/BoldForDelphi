
{ Global compiler directives }
{$include bold.inc}
unit BoldWAStringGridManager;

interface

uses
  grids,
  controls,
  classes,
  stdctrls,
  BoldContainers;

type
  TWinControlClass = class of TWinControl;
  TGridColumnControl = class
  public
    Control: TWinControl;
    ControlClass: TWinControlClass;
    AssociatedColumn: integer;
    destructor Destroy; override;
  end;
  TStringGridManager = class
  private
    fDoExchangeRow: Boolean;
    fStringGrid: TStringGrid;
    fNumberOfColumns: integer;
    fNumberOfEditableColumns: integer;
    fControls: TBoldObjectArray;
  public
    constructor Create(aStringGrid: TStringGrid);
    procedure AdjustGridCellHeight(ctrl: TWinControl);
    procedure SetStringGrid(aStringGrid: TStringGrid);
    procedure addCtrlForColumn(aControl: TWinControl; aClass: TWinControlClass; ACol: integer);
    function GetCtrlForColumn(ACol: integer;var aControl: TWinControl; var aClass: TWinControlClass): Boolean;
    procedure StringGridOnSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure EditCell(var StringGrid: TStringGrid; ACol, ARow: Integer; ctrl: TWinControl);
    procedure ControlOnChange(Sender: TObject);
    procedure ControlOnExit(Sender: TObject);
    procedure GetCtrlSelection(Ctrl: TWinControl; Hide: Boolean);
    procedure Add;
    procedure Delete;
    procedure MoveDown;
    procedure MoveUp;
    procedure Edit;
  end;

implementation

uses
  SysUtils,
  windows,
  BoldVclUtils;

destructor TGridColumnControl.Destroy;
begin
  Control := nil;
  ControlClass := nil;
  AssociatedColumn := 0;
  inherited destroy;
end;

constructor TStringGridManager.Create(aStringGrid: TStringGrid);
begin
  inherited Create;
  fNumberOfColumns := 0;
  fNumberOfEditableColumns := 0;
  SetStringGrid(aStringGrid);
  if not Assigned(fControls) then
    fControls := TBoldObjectArray.Create(100,[bcoDataOwner])
  else
    fControls.Clear  ;
end;

procedure TStringGridManager.SetStringGrid(aStringGrid: TStringGrid);
begin
  fStringGrid := nil;
  fStringGrid := aStringGrid;
  fStringGrid.OnSelectCell := StringGridOnSelectCell;
  fNumberOfColumns := fStringGrid.ColCount;
end;

procedure TStringGridManager.addCtrlForColumn(aControl: TWinControl; aClass: TWinControlClass; ACol: integer);
var
  rec: TGridColumnControl;
begin
  Inc(fNumberOfEditableColumns);
  rec := TGridColumnControl.Create;
  rec.Control := aControl;
  rec.ControlClass := aClass;
  rec.AssociatedColumn := ACol;
  fControls.Add(rec);
  AdjustGridCellHeight(aControl);
  if (aControl is TComboBox) then
  begin
    (aControl as TComboBox).OnChange := ControlOnChange;
    (aControl as TComboBox).OnExit := ControlOnExit;
  end
  else if (aControl is TEdit) then
  begin
    (aControl as TEdit).OnChange := ControlOnChange;
    (aControl as TEdit).OnExit := ControlOnExit;
  end;
end;

procedure TStringGridManager.StringGridOnSelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
var
  Ctrl: TWinControl;
  CtrlClass: TWinControlClass;
begin
  if not fDoExchangeRow then
  begin
    if fNumberOfEditableColumns > 0 then
    begin
      if GetCtrlForColumn(ACol, Ctrl, CtrlClass) then
        EditCell(fStringGrid, ACol, ARow, (Ctrl as CtrlClass));
    end;
  end;
  CanSelect := True;
end;

function TStringGridManager.GetCtrlForColumn(ACol: integer; var aControl: TWinControl; var aClass: TWinControlClass): Boolean;
type
  ptrRec = TGridColumnControl;
var
  i: integer;
begin
  Result := false;
  for i:= 0 to fControls.Count -1 do
    if ((ptrRec(fControls.Items[i]).AssociatedColumn) = ACol) then
    begin
      aControl := ptrRec(fControls.Items[i]).Control;
      aClass := ptrRec(fControls.Items[i]).ControlClass;
      Result := True;
      Break;
    end;
end;

procedure TStringGridManager.EditCell(var StringGrid: TStringGrid; ACol, ARow: Integer;
                    ctrl: TWinControl);
var
  R: TRect;
  ChangeEvent: TNotifyEvent;
begin
  {Size and position the combo box to fit the cell}
  R := fStringGrid.CellRect(ACol, ARow);
  R.Left := R.Left + fStringGrid.Left;
  R.Right := R.Right + fStringGrid.Left;
  R.Top := R.Top + fStringGrid.Top;
  R.Bottom := R.Bottom + fStringGrid.Top;
  Ctrl.Left := R.Left + 1;
  Ctrl.Top := R.Top + 1;
  Ctrl.Width := (R.Right + 1) - R.Left;
  Ctrl.Height := (R.Bottom + 1) - R.Top;
  {Show the control}
  Ctrl.Visible := true;
  if (Ctrl is TComboBox) then
    with (Ctrl as TComboBox) do
    begin
      ChangeEvent := OnChange;
      OnChange := nil;
      if (IsEmptyStr(fStringGrid.Cells[ACol, ARow]) and (CompareText(Ctrl.Name, 'cbAccessTypes') = 0))
      then
        ItemIndex := 0
      else
        ItemIndex := Items.IndexOf(fStringGrid.Cells[ACol, ARow]);
      SetFocus;
      OnChange := ChangeEvent;
    end
  else if (Ctrl is TEdit) then
    with (Ctrl as TEdit) do
    begin
      ChangeEvent := OnChange;
      OnChange := nil;
      Text := fStringGrid.Cells[ACol, ARow];
      SetFocus;
      OnChange := ChangeEvent;
    end;
end;

procedure TStringGridManager.AdjustGridCellHeight(ctrl: TWinControl);
begin
 {The combobox height is not settable, so we will}
 {instead size the grid to fit the combobox or whatever!}
 fStringGrid.DefaultRowHeight := ctrl.Height;
 {Hide the control}
 ctrl.Visible := false;
end;

procedure TStringGridManager.ControlOnChange(Sender: TObject);
begin
  getCtrlSelection((Sender as TWinControl), false);
end;

procedure TStringGridManager.ControlOnExit(Sender: TObject);
begin
  getCtrlSelection((Sender as TWinControl), True);
  if (fStringGrid.Col < fNumberOfColumns - 1) then
    fStringGrid.SetFocus;
end;

procedure TStringGridManager.GetCtrlSelection(Ctrl: TWinControl; Hide: Boolean);
begin
  if (Ctrl is TComboBox) then
    with (Ctrl as TComboBox) do
    begin
      if ((ItemIndex = -1) and not IsEmptyStr(Text)) then
        fStringGrid.Cells[fStringGrid.Selection.right, fStringGrid.Selection.top] := Text
      else
        fStringGrid.Cells[fStringGrid.Selection.right, fStringGrid.Selection.top] := Items[ItemIndex];
    end
  else if (Ctrl is TEdit) then
    with (Ctrl as TEdit) do
      fStringGrid.Cells[fStringGrid.Selection.Right, fStringGrid.Selection.top] := Text;
  Ctrl.Visible := not Hide;
end;

procedure TStringGridManager.Add;
var
  i: integer;
begin
  for i:= 0 to fControls.Count - 1 do
    if (fControls.Items[i] as TGridColumnControl).Control.Visible then
    begin
      ControlOnExit((fControls.Items[i] as TGridColumnControl).Control);
      Break;
    end;
  AppendRow(fStringGrid);
  SelectCell(fStringGrid, 0, fStringGrid.RowCount - 1);
  Edit;
end;

procedure TStringGridManager.Delete;
var
  i: integer;
begin
  for i:= 0 to fControls.Count - 1 do
    if (fControls.Items[i] as TGridColumnControl).Control.Visible then
    begin
      ControlOnExit((fControls.Items[i] as TGridColumnControl).Control);
      Break;
    end;
  DeleteRow(fStringGrid, fStringGrid.Selection.Top);
end;

procedure TStringGridManager.MoveDown;
var
  i: integer;
begin
  for i:= 0 to fControls.Count - 1 do
  begin
    (fControls.Items[i] as TGridColumnControl).Control.Visible := false;
    fDoExchangeRow := true;
    ExchangeRows(fStringGrid, fStringGrid.Selection.Top, fStringGrid.Selection.Top + 1);
    fDoExchangeRow := false;
  end;
end;

procedure TStringGridManager.MoveUp;
var
  i: integer;
begin
  for i:= 0 to fControls.Count - 1 do
  begin
    (fControls.Items[i] as TGridColumnControl).Control.Visible := false;
    fDoExchangeRow := true;
    ExchangeRows(fStringGrid, fStringGrid.Selection.Top, fStringGrid.Selection.Top - 1);
    fDoExchangeRow := false;
  end;
end;

procedure TStringGridManager.Edit;
var
  Ctrl: TWinControl;
  CtrlClass: TWinControlClass;
  ACol, ARow: integer;
begin
  if fNumberOfEditableColumns > 0 then
  begin
    aCol:= fStringGrid.Col;
    aRow := fStringGrid.Row;
    if GetCtrlForColumn(aCol, Ctrl, CtrlClass) then
      EditCell(fStringGrid, aCol, aRow, (Ctrl as CtrlClass));
  end;
end;

end.
