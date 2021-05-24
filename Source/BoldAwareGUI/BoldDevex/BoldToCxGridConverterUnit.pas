unit BoldToCxGridConverterUnit;

interface

uses
  BoldGrid,
  Classes,
  cxGridBoldSupportUnit,
  cxGridPopupMenu,
  SysUtils;

type
  TBoldToCxGridConverter = class(TComponent)
  private
    procedure ConvertGrid(aBoldGrid : TBoldGrid);
    procedure CopyBoldColumn(aBoldColumn : TBoldGridColumn; aCxGridBoldColumn : TCxGridBoldColumn);
  protected
    { Protected declarations }
  public
    constructor Create(AOwner : TComponent); override;
    { Public declarations }
  published
    { Published declarations }
  end;

const
  Ctrlf = #13#10;

procedure Register;

implementation
uses Dialogs, cxGrid;

procedure Register;
begin
  RegisterComponents('AT Core', [TBoldToCxGridConverter]);
end;

{ TBoldToCxGridConverter }

procedure TBoldToCxGridConverter.CopyBoldColumn(aBoldColumn: TBoldGridColumn;aCxGridBoldColumn: TCxGridBoldColumn);
begin
    aCxGridBoldColumn.DataBinding.BoldProperties.Expression := aBoldColumn.BoldProperties.Expression;
    aCxGridBoldColumn.Caption := aBoldColumn.Title.Caption;
    try
      aCxGridBoldColumn.name := 'col' + AnsiUpperCase(aBoldColumn.BoldProperties.Expression[1]) + Copy(aBoldColumn.BoldProperties.Expression, 2, 1000);
    except
      aCxGridBoldColumn.name := 'Column' + IntToStr(aCxGridBoldColumn.Index);
    end;
end;

constructor TBoldToCxGridConverter.Create(AOwner: TComponent);
var
  I : Integer;
  vGrid : TBoldGrid;
begin
  inherited;
  for I := 0 to AOwner.ComponentCount - 1 do
  begin
    if AOwner.Components[I] is TBoldGrid then
    begin
      vGrid :=  AOwner.Components[I] as TBoldGrid;
      if MessageDlg('Convert ' + vGrid.Name + ' into a cxBoldGrid ?',mtWarning,[mbYes,mbNo],0) = 6 then
        ConvertGrid(vGrid);
    end;
  end;
end;


procedure TBoldToCxGridConverter.ConvertGrid(aBoldGrid : TBoldGrid);
var
  vCxGrid : TCxGrid;
  vCxGridBoldTableView : TcxGridBoldTableView;
  vCol : Integer;
begin

  vCxGrid := TcxGrid.Create(aBoldGrid.Owner);
  vCxGrid.Name := 'cx'+ aBoldGrid.Name;
  vCxGrid.Parent := aBoldGrid.Parent;
  vCxGrid.Left := aBoldGrid.Left;
  vCxGrid.Top := aBoldGrid.Top;
  vCxGrid.Width := aBoldGrid.Width;
  vCxGrid.Height := aBoldGrid.Height;
  vCxGrid.Align := aBoldGrid.Align;
  vCxGrid.Anchors := aBoldGrid.Anchors;

  vCxGridBoldTableView := vCxGrid.CreateView(TcxGridBoldTableView) as TcxGridBoldTableView;
  vCxGridBoldTableView.Name := vCxGrid.Name + 'BoldTableView';

  vCxGridBoldTableView.DataController.BoldHandle := aBoldGrid.BoldHandle;
  vCxGrid.Levels[0].GridView := vCxGridBoldTableView;
  vCxGridBoldTableView.OptionsData.Editing := False;
  vCxGridBoldTableView.OptionsView.GroupByBox := False;
  vCxGridBoldTableView.OptionsBehavior.CellHints := True;
  vCxGridBoldTableView.OptionsSelection.CellSelect := False;

  for vCol := 1 to aBoldGrid.ColCount - 1 do
    CopyBoldColumn(aBoldGrid.Columns[vCol],vCxGridBoldTableView.CreateItem as TcxGridBoldColumn );

  ShowMessage('Successfully converted ' + aBoldGrid.name + ' into a TcxBoldGrid' + CtrLf +
              'Remember to remove old boldgrid and this converter component.' + Ctrlf +
              'Add a TcxGridPopupMenu component if you want to use menus on the grids header.');

end;

end.
