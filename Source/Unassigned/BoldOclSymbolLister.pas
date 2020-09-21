unit BoldOclSymbolLister;

interface

uses
  BoldOclClasses, BoldOclSymbolImplementations,
  Clipbrd,
  BoldSystemRT, BoldElements,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, BoldHandles, BoldSystemHandle, BoldSubscription,
  BoldAbstractModel, BoldModel, Grids;

type
  TForm1 = class(TForm)
    BoldModel1: TBoldModel;
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
    sgSymbols: TStringGrid;
    procedure FormCreate(Sender: TObject);
  private
    function AsClipBoardText: string;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

function TForm1.AsClipBoardText: string;
var
  Col, Row: integer;
begin
  Result := '';
  for row := 0 to sgSymbols.RowCount - 1 do
  begin
    for col := 0 to sgSymbols.ColCount - 1 do
      Result := Result + sgSymbols.Cells[Col, Row] + #9;
    Result := Result + #13#10;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  SymTab: TBoldSymbolDictionary;
  Symbol: TBoldOclSymbol;
  i, j: integer;
  errors: boolean;
  temp: String;

  function GetTypeName(aType: TBoldElementTypeInfo): string;
  begin
    if assigned(aType) then
    begin
      if (aType is TBoldClassTypeInfo) and
        not assigned((aType as TBoldClassTypeInfo).SuperClassTypeInfo) then
        Result := '<any class>'
      else
        Result := aType.ExpressionName
    end
    else
      result := '<any>';
  end;

begin
  SymTab :=TBoldSymbolDictionary.Create(BoldSystemTypeInfoHandle1.StaticSystemTypeInfo, nil, errors);

  sgSymbols.Cells[0, 0] := 'Name';
  sgSymbols.Cells[1, 0] := 'Result';
  sgSymbols.Cells[2, 0] := 'Context';
  sgSymbols.Cells[3, 0] := 'Parameter 1';
  sgSymbols.Cells[4, 0] := 'Parameter 2';
  sgSymbols.DefaultColWidth := 80;
  sgSymbols.ColWidths[1] := 400;
  width := 4*80+400+40;

  InitializeSymbolTable(symtab);
  sgSymbols.RowCount := SymTab.Count+1;
  for i := 0 to SymTab.Count-1 do
  begin
    Symbol := SymTab.Symbols[i];
    sgSymbols.Cells[0, i+1] := Symbol.SymbolName;
    case Symbol.DeduceMethod of
      tbodNo: temp := GetTypeName(symbol.ResultType);
      tbodCopyLoopVar: Temp := 'Type of loop variable';
      tbodCopyArg1: Temp := 'Same as source element';
      tbodCopyArg1Elem: Temp := 'Same as list elements of first argument';
      tbodCopyArg2: Temp := 'Same as parameter 1';
      tbodCopyArg3: Temp := 'Same as parameter 2';
      tbodLCC: Temp := 'Least common supertype of source element and parameter 1';
      tbodLCC23: Temp := 'Least common supertype of parameter 1 and 2';
      tbodListofArg2: Temp := 'List containing elements of the same type as parameter 1';
      tbodObjectlist: Temp := 'Object list';
      tbodType: Temp := 'Meta type';
      tbodTypecast: Temp := 'Type of parameter 1';
      tbodArg1AsList: Temp := 'Same as source element';
      tbodListFromArg2: Temp := 'List containing elements of the same type as refered by parameter 1';
    end;
    sgSymbols.Cells[1, i+1] := Temp;

    if sgSymbols.ColCount < Symbol.NumberOfArgs+2 then
      sgSymbols.ColCount := Symbol.NumberOfArgs+2;

    for j := 0 to Symbol.NumberOfArgs-1 do
      sgSymbols.Cells[j+2, i+1] := GetTypeName(Symbol.FormalArguments[j]);
  end;
  SymTab.Free;

  ClipBoard.AsText := AsClipBoardtext;
end;

end.
