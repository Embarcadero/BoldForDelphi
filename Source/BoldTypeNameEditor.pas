
{ Global compiler directives }
{$include bold.inc}
unit BoldTypeNameEditor;

interface

uses
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  Buttons,
  Menus,
  Grids,
  BoldTypeNameHandle,
  BoldTypeNameDictionary, ImgList, System.ImageList;

type
  TBoldTypeNameEditorForm = class(TForm)
    StringGrid1: TStringGrid;
    btnAdd: TButton;
    btnDelete: TButton;
    btnOK: TButton;
    btnCancel: TButton;
    btnDown: TSpeedButton;
    btnUp: TSpeedButton;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    SaveToFile1: TMenuItem;
    SaveDialog1: TSaveDialog;
    OpenDialog1: TOpenDialog;
    LoadFromFile1: TMenuItem;
    PopupMenu1: TPopupMenu;
    Copyrow1: TMenuItem;
    Edit1: TMenuItem;
    MergeDefaultsmappings1: TMenuItem;
    ImageList1: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnDownClick(Sender: TObject);
    procedure btnUpClick(Sender: TObject);
    procedure Copyrow1Click(Sender: TObject);
    procedure SaveToFile1Click(Sender: TObject);
    procedure LoadFromFile1Click(Sender: TObject);
    procedure MergeDefaultsmappings1Click(Sender: TObject);
  private
    { Private declarations }
    procedure MoveRow(Direction: integer);
  public
    procedure SaveToDictionary(Dictionary: TBoldTypeNameDictionary);
    procedure LoadFromDictionary(Dictionary: TBoldTypeNameDictionary);
    { Public declarations }
  end;

implementation

uses
  SysUtils,
  BoldUtils;

{$R *.dfm}

procedure TBoldTypeNameEditorForm.FormCreate(Sender: TObject);
begin
  StringGrid1.Cells[0, 0] := 'ModelName';
  StringGrid1.Cells[1, 0] := 'ExpressionName';
  StringGrid1.Cells[2, 0] := 'DelphiName';
  StringGrid1.Cells[3, 0] := 'ContentName';
  StringGrid1.Cells[4, 0] := 'PMapper';
  StringGrid1.Cells[5, 0] := 'Accessor';
  StringGrid1.Cells[6, 0] := 'NativeType';
  StringGrid1.Cells[7, 0] := 'UnitName';
  StringGrid1.Cells[8, 0] := 'ComType';
  StringGrid1.Cells[9, 0] := 'IDLType';
  StringGrid1.Cells[10, 0] := 'ValueInterface';
  StringGrid1.Cells[11, 0] := 'VI-accessor';
  StringGrid1.Cells[12, 0] := 'VI-NativeType';
end;

procedure TBoldTypeNameEditorForm.btnAddClick(Sender: TObject);
begin
  StringGrid1.RowCount := StringGrid1.RowCount + 1;
  StringGrid1.Row := StringGrid1.RowCount - 1;
  StringGrid1.Col := 0;
  Stringgrid1.Cells[2, StringGrid1.RowCount - 1] := 'TXX<Name>';
  Stringgrid1.Cells[3, StringGrid1.RowCount - 1] := '<Name>';
  Stringgrid1.Cells[4, StringGrid1.RowCount - 1] := 'TBoldPM<Name>';
  Stringgrid1.Cells[5, StringGrid1.RowCount - 1] := 'as<Name>';
  Stringgrid1.Cells[6, StringGrid1.RowCount - 1] := 'T<Name>';
  Stringgrid1.Cells[10, StringGrid1.RowCount - 1] := 'IBold<Name>Content';
  StringGrid1.SetFocus;
end;

procedure TBoldTypeNameEditorForm.btnDeleteClick(Sender: TObject);
var
  i, j: integer;
begin
  if StringGrid1.RowCount > 2 then
  begin
    for i := StringGrid1.Row to StringGrid1.RowCount - 1 do
      for j := 0 to StringGrid1.ColCount - 1 do
        StringGrid1.Cells[j, i] := StringGrid1.Cells[j, i + 1];
    StringGrid1.RowCount := StringGrid1.RowCount - 1;
  end;
end;


procedure TBoldTypeNameEditorForm.btnDownClick(Sender: TObject);
begin
  MoveRow(1);
end;

procedure TBoldTypeNameEditorForm.MoveRow(Direction: integer);
var
  OldPos: integer;
  OldRow: TStrings;
  i: integer;
begin
  if ((StringGrid1.row + Direction) >= 1) and
   ((StringGrid1.row + Direction) < StringGrid1.RowCount) then
  begin
    OldPos := StringGrid1.Row;
    Oldrow := TStringList.Create;
    OldRow.AddStrings(StringGrid1.Rows[OldPos]);
    for i := 0 to StringGrid1.ColCount - 1 do
      StringGrid1.cells[i, OldPos] := StringGrid1.cells[i, OldPos+Direction];
    for i := 0 to StringGrid1.ColCount - 1 do
      StringGrid1.cells[i, OldPos+Direction] := OldRow[i];
    OldRow.Free;
    StringGrid1.Row := OldPos+Direction;
  end;
end;

procedure TBoldTypeNameEditorForm.btnUpClick(Sender: TObject);
begin
  MoveRow(-1);
end;

procedure TBoldTypeNameEditorForm.Copyrow1Click(Sender: TObject);
var
  OldPos: integer;
  i: integer;
begin
  OldPos := StringGrid1.Row;
  btnAddClick(self);
  for i := 2 to StringGrid1.ColCount - 1 do
    StringGrid1.Cells[i, StringGrid1.row] := StringGrid1.Cells[i, OldPos];
end;

procedure TBoldTypeNameEditorForm.SaveToFile1Click(Sender: TObject);
var
  Dictionary: TBoldTypeNameDictionary;
begin
  if SaveDialog1.Execute then
  begin
    Dictionary := TBoldTypeNameDictionary.Create(nil);
    SavetoDictionary(Dictionary);
    Dictionary.SaveToFile(SaveDialog1.FileName);
    Dictionary.Free;
  end;
end;

procedure TBoldTypeNameEditorForm.LoadFromDictionary(Dictionary: TBoldTypeNameDictionary);
var
  i: integer;
begin
  StringGrid1.RowCount := Dictionary.count+1;
  for i := 0 to Dictionary.Count-1 do
  begin
    StringGrid1.Cells[0, i + 1] := Dictionary.Mapping[i].ModelName;
    StringGrid1.Cells[1, i + 1] := Dictionary.Mapping[i].ExpressionName;
    StringGrid1.Cells[2, i + 1] := Dictionary.Mapping[i].DelphiName;
    StringGrid1.Cells[3, i + 1] := Dictionary.Mapping[i].ContentsName;
    StringGrid1.Cells[4, i + 1] := Dictionary.Mapping[i].MapperName;
    StringGrid1.Cells[5, i + 1] := Dictionary.Mapping[i].Accessor;
    StringGrid1.Cells[6, i + 1] := Dictionary.Mapping[i].NativeType;
    StringGrid1.Cells[7, i + 1] := Dictionary.Mapping[i].UnitName;
    StringGrid1.Cells[8, i + 1] := Dictionary.Mapping[i].ComType;
    StringGrid1.Cells[9, i + 1] := Dictionary.Mapping[i].IdlType;
    StringGrid1.Cells[10, i + 1] := Dictionary.Mapping[i].ValueInterface;
    StringGrid1.Cells[11, i + 1] := Dictionary.Mapping[i].ValueInterfaceAccessor;
    StringGrid1.Cells[12, i + 1] := Dictionary.Mapping[i].ValueInterfaceNativeType;
  end;
end;

procedure TBoldTypeNameEditorForm.SaveToDictionary(Dictionary: TBoldTypeNameDictionary);
var
  i: integer;
begin
  Dictionary.Clear;
  for i := 1 to StringGrid1.RowCount - 1 do
  begin
    with Dictionary.AddMapping do
    begin
      ModelName := StringGrid1.Cells[0, i];
      ExpressionName := StringGrid1.Cells[1, i];
      DelphiName := StringGrid1.Cells[2, i];
      ContentsName := StringGrid1.Cells[3, i];
      MapperName := StringGrid1.Cells[4, i];
      Accessor := StringGrid1.Cells[5, i];
      NativeType := StringGrid1.Cells[6, i];
      UnitName := StringGrid1.Cells[7, i];
      ComType := StringGrid1.Cells[8, i];
      IDLType := StringGrid1.Cells[9, i];
      ValueInterface := StringGrid1.Cells[10, i];
      ValueInterfaceAccessor := StringGrid1.Cells[11, i];
      ValueInterfaceNativeType := StringGrid1.Cells[12, i];
    end;
  end;
end;

procedure TBoldTypeNameEditorForm.LoadFromFile1Click(Sender: TObject);
var
  Dictionary: TBoldTypeNameDictionary;
begin
  if OpenDialog1.Execute then
  begin
    Dictionary := TBoldTypeNameDictionary.Create(nil);
    Dictionary.LoadFromFile(OpenDialog1.FileName);
    LoadFromDictionary(Dictionary);
    Dictionary.Free;
  end;
end;


procedure TBoldTypeNameEditorForm.MergeDefaultsmappings1Click(Sender: TObject);
var
  DefaultMappings: TBoldTypeNameDictionary;
  CurrentMappings: TBoldTypeNameDictionary;
  Mapping: TBoldtypeNameMapping;
  i, j: integer;
begin
  DefaultMappings := TBoldTypeNameDictionaryClass.Create(nil);
  CurrentMappings := TBoldTypeNameDictionary.create(nil);
  try
    DefaultMappings.AddDefaultMappings;
    SaveToDictionary(CurrentMappings);
    for i := 0 to DefaultMappings.Count - 1 do
    begin
      Mapping := nil;
      if DefaultMappings.Mapping[i].ModelName <> '' then
        Mapping := CurrentMappings.ExactMappingForModelName[DefaultMappings.Mapping[i].modelName]
      else
      begin
        for j := 0 to CurrentMappings.Count - 1 do
          if CompareText(DefaultMappings.Mapping[i].ExpressionName, CurrentMappings.Mapping[j].ExpressionName) = 0 then
          begin
            Mapping := CurrentMappings.Mapping[j];
            break;
          end;
      end;
      if assigned(Mapping) then
        Mapping.Assign(DefaultMappings.Mapping[i])
      else
        CurrentMappings.AddMapping.Assign(DefaultMappings.Mapping[i]);
    end;
    LoadFromDictionary(CurrentMappings);
  finally
    DefaultMappings.Free;
    CurrentMappings.Free;
  end;
end;

end.
