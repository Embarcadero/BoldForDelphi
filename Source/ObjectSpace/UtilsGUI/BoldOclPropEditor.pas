unit BoldOclPropEditor;

interface

uses
  Windows,
  Messages,
  Dialogs,
  Classes,
  Graphics,
  Controls,
  Forms,
  ClipBrd,
  StdCtrls,
  BoldHandles,
  BoldAbstractModel,
  BoldSystemRT,
  BoldElements,
  BoldOcl,
  ExtCtrls,
  Grids,
  ComCtrls,
  ImgList,
  Menus,
  ShellAPI;

type
  TBoldOclPropEditForm = class(TForm)
    ExpBottomPanel: TPanel;
    TabControlPanel: TPanel;
    ExprEditPageControl: TPageControl;
    ExpressionPage: TTabSheet;
    pnlLeftSide: TPanel;
    ExpParserPanel: TPanel;
    EditPanel: TPanel;
    EditMemo: TMemo;
    pnlEditButtons: TPanel;
    Syntaxcb: TCheckBox;
    Clear: TButton;
    RemoveLast: TButton;
    ImageList1: TImageList;
    PopupMenu1: TPopupMenu;
    Copymessagestoclipboard1: TMenuItem;
    ParserMessages: TMemo;
    Splitter1: TSplitter;
    ParserMsg: TLabel;
    Splitter2: TSplitter;
    pnlRightSide: TPanel;
    SelectBox: TListBox;
    pnlShowTypes: TPanel;
    Typescb: TCheckBox;
    imgModelErrors: TImage;
    pnlOKCancel: TPanel;
    btnOK: TButton;
    btnCancel: TButton;
    btnShowInfo: TButton;
    procedure SelectBoxDblClick(Sender: TObject);
    procedure EditMemoEnter(Sender: TObject);
    procedure SelectBoxKeyPress(Sender: TObject; var Key: Char);
    procedure EditMemoChange(Sender: TObject);
    procedure ClearClick(Sender: TObject);
    procedure RemoveLastClick(Sender: TObject);
    procedure SyntaxcbClick(Sender: TObject);
    procedure TypescbClick(Sender: TObject);
    procedure EditMemoMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure EditMemoKeyPress(Sender: TObject; var Key: Char);
    procedure OKClick(Sender: TObject);
    procedure CancelClick(Sender: TObject);
    procedure EditMemoKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SelectBoxDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure SelectBoxKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Copymessagestoclipboard1Click(Sender: TObject);
    procedure imgModelErrorsClick(Sender: TObject);
    procedure InfoBrowserClick(Sender: TObject);
  private
    { Private declarations }
    fContext: TBoldElementTypeInfo;
    fSystemTypeInfo: TBoldSystemTypeInfo;
    fOclEvaluator: TBoldOcl;
    fVariables: TBoldExternalVariableList;
    procedure SetOclExpr(Expr: String);
    function GetOclExpr: String;
    procedure SetContext(Context: TBoldElementTypeInfo);
    procedure SetSelection;
    procedure CleanParserMsg;
    procedure MemberHelp(Ocl: String);
    Procedure OperationHelp(Ocl: String);
    function CheckOcl(Ocl: String): TBoldElementTypeInfo;
    function GetOclEvaluator:TBoldOcl;
    procedure WMGetMinMaxInfo(Var Msg: TWMGetMinMaxINfo); message WM_GETMINMAXINFO;
    procedure SetVariables(const Value: TBoldExternalVariableList);
  public
    { Public declarations }
    ShowSyntaxErrors: Boolean;
    ShowTypes: Boolean;
    Property OclEvaluator: TBoldOCL read GetOclEvaluator;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Context: TBoldElementTypeInfo read fContext write SetContext;
    property SystemTypeInfo: TBoldSystemTypeInfo read fSystemTypeInfo;
    property OclExpr: String read GetOclExpr write SetOclExpr;
    property Variables: TBoldExternalVariableList read fVariables write SetVariables;
  end;

implementation

uses
  SysUtils,
  BoldDefs,
  BoldRegistry,
  BoldOclError,
  BoldSystem,
  BoldCoreConsts,
  BoldOclClasses;

{$R *.dfm}

const
  OCLInfoURL = 'http://info.borland.com/techpubs/delphi/boldfordelphi/?Mech_OclAndSubscription.htm';
  OCLInfoLocal = 'doc\oclinfo.html';

type
  ModeType = (Arrow, Dot, unKnown);

procedure TBoldOclPropEditForm.CleanParserMsg;
begin
  ParserMessages.lines.clear;
end;

constructor TBoldOclPropEditForm.Create(AOwner: TComponent);
begin
  inherited;
  ExpressionPAge.TabVisible := false;
  ExprEditPageControl.ActivePage := ExpressionPage;
  SetFocusedControl(EditMemo);
  fOclEvaluator := nil;
end;

destructor TBoldOclPropEditForm.Destroy;
begin
  FreeAndNil(fOclEvaluator);
  inherited;
end;

function TBoldOclPropEditForm.GetOclEvaluator;
begin
  if not assigned(fOclEvaluator) then
    fOclEvaluator := TBoldOcl.Create(SystemTypeInfo, nil);
  result := fOclEvaluator;
end;

procedure TBoldOclPropEditForm.SetContext(Context: TBoldElementTypeInfo);
var
  OldSystemTypeInfo: TBoldSystemTypeInfo;
//  j, i: integer;
begin
  OldSystemTypeInfo := SystemTypeInfo;
  fContext := Context;
//  ContextCombo.Items.Clear;
  if assigned(Context) then
  begin
    fSystemTypeInfo := Context.SystemTypeInfo as TBoldSystemTypeInfo;
    Caption:= 'OCL expression editor - Context: ' + Context.AsString; // do not localize
  end
  else
  begin
    Caption:= 'OCL expression editor - Context: <unknown>'; // do not localize
    CleanParserMsg;
    ParserMessages.lines.Add(sNoContextNoSupport);
  end;

  if (not assigned(SystemTypeInfo)) and (TBoldSystem.DefaultSystem <> nil) then
    fSystemTypeInfo := TBoldSystem.DefaultSystem.BoldType as tBoldSystemTypeInfo;

{
  if assigned( Model) then
  begin
    ContextCombo.Items.AddObject(Model.AsString, Model);
    for i := 0 To Model.RTInfo.TopSortedClasses.Count - 1 do
    begin
      ContextCombo.Items.AddObject(Model.RTInfo.TopSortedClasses[i].AsString, Model.RTInfo.TopSortedClasses[i]);
      for j := 0 to Model.RTInfo.TopSortedClasses[i].AllMembers.Count - 1 do
        ContextCombo.Items.AddObject(Model.RTInfo.TopSortedClasses[i].AllMembers[j].AsString,
                                      Model.RTInfo.TopSortedClasses[i].AllMembers[j]);
    end;
    for i := 0 to Model.AttributeTypes.Count - 1 do
      ContextCombo.Items.AddObject(Model.AttributeTypes[i].AsString, Model.AttributeTypes[i]);
    for i := 0 to Model.ListTypes.Count - 1 do
      if Assigned(TBoldListTypeInfo(Model.ListTypes[i]).Element) then
        ContextCombo.Items.AddObject(Model.ListTypes[i].AsString, Model.ListTypes[i]);

  end;
  for i := 0 to ContextCombo.Items.Count - 1 do
    if ContextCombo.Items.Objects[i] = Context then
      ContextCombo.ItemIndex := i;
}

  if OldSystemTypeInfo <> fSystemTypeInfo then
  begin
    fOclEvaluator.Free;
    fOclEvaluator := TBoldOcl.Create(SystemTypeInfo, nil);
  end;

  EditMemoChange(nil);
  imgModelErrors.visible := assigned(SystemTypeInfo) and (SystemTypeInfo.InitializationLog.count > 0);
end;

procedure TBoldOclPropEditForm.SetOclExpr(Expr: String);
var
  i: integer;
begin
  while Expr <> '' do
  begin
    if pos(BOLDCR, Expr) = 0 then
    begin
      EditMemo.Lines.Add(Expr);
      Expr := '';
    end
    else
    begin
      EditMemo.Lines.Add(copy(Expr, 1, pos(BOLDCR, Expr)));
      Delete(Expr, 1, pos(BOLDCR, Expr));
    end;
  end;
//  EditMemo.Text := EditMemo.Text + ' ';
  i := Length(EditMemo.Text);
  EditMemo.SelStart := i - 2;
  EditMemoChange(nil);
end;

function TBoldOclPropEditForm.GetOclExpr: String;
var
  i: integer;
begin
  with EditMemo do
  begin
    result := '';
    For i := 0 to Lines.Count - 1 do
      Result := Result + Lines[i] + BOLDCR;
  end;
  Delete(result, length(result), 1);
end;

function TBoldOClPropEditForm.CheckOcl(Ocl: String): TBoldElementTypeInfo;

  procedure ShowError(Ocl: String; ErrorPointer: String; message: string);
  var
    i: integer;
  begin
    ParserMessages.lines.text := Ocl;
    i := 0;
    while length(ErrorPointer) > length(ParserMessages.Lines[i]) do
    begin
      system.Delete(ErrorPointer, 1, length(ParserMessages.lines[i])+2);
      inc(i);
    end;
    ParserMessages.lines.Add(message);
    if length(ErrorPointer) > 10 then
    begin
      delete(ErrorPointer, length(ErrorPointer)-8, 7);
      insert('-------', errorPointer, length(ErrorPointer));
    end else
      ErrorPointer := ErrorPointer + '-------';

    ParserMessages.lines.Insert(i+1, ErrorPointer);
  end;

begin

  CleanParserMsg;
  if Ocl = '' then
  begin
    Result := Context;
    ParserMessages.lines.Add(sEmptyExpression);
    exit;
  end;

  try
    Result := OclEvaluator.ExpressionType(Ocl, Context, true, Variables);
  except
    on e: EBoldOclAbort do
    begin
      Result := nil;
      ParserMessages.lines.BeginUpdate;
      if (Pos('SSYacc', e.Message) > 0) or (Pos('SSLex', e.Message) > 0) then // do not localize
      begin
        if ShowSyntaxErrors then
          ShowError(e.Ocl, e.ErrorPointer, e.Message)
        else
          ParserMessages.lines.add(sSyntaxError);
      end else
        ShowError(e.Ocl, e.ErrorPointer, e.Message);
      ParserMessages.lines.EndUpdate;
      exit;
    end;
    on e: EBoldOclInternalError do
    begin
      Result := nil;
      e.Ocl := 'Internal Error: ' + e.ocl; // do not localize
      ShowError(e.Ocl, e.ErrorPointer, e.Message);
      exit;
    end;
    on e: EBoldOclError do
    begin
      Result := nil;
      ShowError(e.Ocl, e.ErrorPointer, e.Message);
      exit;
    end;
    on e:Exception do
    begin
      Result := nil;
      ParserMessages.lines.Add(e.message);
      exit;
    end;
  end;
  ParserMessages.lines.BeginUpdate;
  ParserMessages.lines.Add(sSyntaxOK);
  ParserMessages.lines.Add('');
  if assigned(result) then
    ParserMessages.lines.Add(Format(sCurrentTypeIsX, [Result.AsString]))
  else
    ParserMessages.lines.Add(sCurrentTypeIsUnknown);
  ParserMessages.SelStart := 0;
  ParserMessages.SelLength := 0;
  ParserMessages.lines.EndUpdate;
end;

procedure TBoldOclPropEditForm.OperationHelp(Ocl: String);
const
  prefix: array [false..true] of string=('->', '.');
var
  ExpressionType: TBoldElementTypeInfo;
  i, j: integer;
  Symbol: TBoldOCLSymbol;
  Addition: String;
  ListExprType: TBoldListTypeInfo;
begin
  if ocl = '' then
    exit;
  ExpressionType := CheckOcl(Ocl);
  if not assigned(ExpressionType) then
    exit;

  if assigned(OclEvaluator.SymbolTable) then
  begin
     for i := 0 to OclEvaluator.SymbolTable.Count - 1 do
     begin
      Symbol := OclEvaluator.SymbolTable.Symbols[i];
      if (not Assigned(Symbol.FormalArguments[0]) or
        ExpressionType.ConformsTo(Symbol.formalArguments[0]) or
        ((ExpressionType is TBoldListTypeInfo) and
         assigned(TBoldListTypeInfo(ExpressionType).ListElementTypeInfo) and
         TBoldListTypeInfo(ExpressionType).ListElementTypeInfo.ConformsTo(Symbol.FormalArguments[0]))) and symbol.IsPostFix then
      begin
        Addition := 'O | ' + prefix[Symbol.IsDotNotation] + Symbol.SymbolName; // do not localize
        if Symbol.NumberofArgs > 1 then
        begin
          Addition := Addition + '(';
          for j := 1 to Symbol.NumberofArgs - 1 do
            if assigned(Symbol.FormalArguments[j]) then
            begin
              if Symbol.FormalArguments[j] is TBoldListTypeInfo then
              begin
                ListExprType := Symbol.FormalArguments[j] as TBoldListTypeInfo;
                if Assigned(LIstExprType.ListElementTypeInfo) then
                  Addition := Addition + '«List<' + (Symbol.FormalArguments[j] as TBoldListTypeInfo).ListElementTypeInfo.ExpressionName + '>»,' // do not localize
                else
                  Addition := Addition + '«List<AnyArg>»,'      // do not localize // do not localize
              end
              else
                Addition := Addition + '«' + Symbol.FormalArguments[j].ExpressionName + '»,'
            end
            else
              Addition := Addition + '«AnyArg»,'; // do not localize
          Delete(Addition, Length(Addition), 1);
          Addition := Addition + ')';
        end;
        SelectBox.Items.AddObject(Addition +  ' ', Symbol);
      end;
    end;
  end;
end;

procedure TBoldOclPropEditForm.MemberHelp(Ocl: String);
const
  prefix: array[false..true] of string = ('', '.');
var
  I: Integer;
  ClassInfo: TBoldClasstypeInfo;
  SystemInfo: TBoldSystemtypeInfo;
  ExpressionType: TBoldElementTypeInfo;
  Attr: TBoldAttributeRTInfo;
  Role: TBoldRoleRTInfo;
  ExprName: string;
begin
  ExpressionType := CheckOcl(ocl);
  if not assigned(ExpressionType) then
    exit;

  if ExpressionType is TBoldListTypeInfo then
    ExpressionType := (ExpressionType as TBoldListTypeInfo).ListElementTypeInfo;

  if ExpressionType is TBoldClasstypeInfo then
  begin
    ClassInfo := ExpressionType as TBoldClasstypeInfo;
    for I := 0 to ClassInfo.AllMembers.Count - 1 do
      if ClassInfo.AllMembers[I] is TBoldAttributeRTInfo then
      begin
        Attr := ClassInfo.AllMembers[I] as TBoldAttributeRTInfo;
        Exprname := 'M | ' + Prefix[ocl <> ''] + Attr.ExpressionName; // do not localize
        if ShowTypes then
          SelectBox.Items.Add(Exprname + ': ' + Attr.BoldType.ExpressionName)
        else
          SelectBox.Items.Add(Exprname + ' ');
      end else if ClassInfo.AllMembers[I] is TBoldRoleRTInfo then
      begin
        Role := ClassInfo.AllMembers[I] as TBoldRoleRTInfo;
        if role.IsNavigable then
        begin
          Exprname := 'A | ' + Prefix[ocl <> ''] + Role.ExpressionName; // do not localize
          if ShowTypes then
          begin
            if role.IsMultiRole then
              SelectBox.Items.Add(ExprName + ': List<' + Role.ClassTypeInfoOfOtherEnd.Expressionname + '>') // do not localize // do not localize
            else
              SelectBox.Items.Add(ExprName + ': ' + Role.ClassTypeInfoOfOtherEnd.Expressionname)
          end
          else
            SelectBox.Items.Add(ExprName + ' ');
        end;
      end;
  end
  else if ExpressionType is TBoldSystemTypeInfo then
  begin
    SystemInfo := ExpressionType as TBoldSystemTypeInfo;
    for i := 0 to SystemInfo.TopSortedClasses.Count - 1 do
      with SystemInfo.TopSortedClasses[i] do
        if ShowTypes then
          SelectBox.Items.Add('C | ' + ExpressionName + ': ' + DelphiName) // do not localize
        else
          SelectBox.Items.Add('C | ' + ExpressionName + ' '); // do not localize
  end;
end;

procedure TBoldOclPropEditForm.SelectBoxDblClick(Sender: TObject);
var
  Addition: String;
begin
  if not assigned(Context) then
    exit;
  if SelectBox.ItemIndex = -1 then
    SelectBox.ItemIndex := 0;
  Addition := SelectBox.Items[SelectBox.ItemIndex];

  if pos(' | ', Addition) <> 0 then
    Delete(Addition, 1, Pos(' | ', Addition) + 2);

  Addition := Copy(Addition, 1, Pos(' ', Addition) - 1);

  if Pos(':', Addition) = Length(Addition) then
    Addition := Copy(Addition, 1, Length(Addition) - 1);

  EditMemo.Lines[EditMemo.Lines.Count - 1] :=
    EditMemo.Lines[EditMemo.Lines.Count - 1] +
    Addition;

  EditMemoChange(Sender);
end;

procedure TBoldOclPropEditForm.EditMemoEnter(Sender: TObject);
begin
//  Context := DefaultBoldSystem.BoldSystemRtInfo;
  EditMemoChange(Sender);
end;

procedure TBoldOclPropEditForm.SelectBoxKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key = BOLDCR then
    SelectBoxDblClick(Sender);
end;

procedure TBoldOclPropEditForm.EditMemoChange(Sender: TObject);
var
  Ocl: String;
begin
  if not assigned(Context) then
    exit;
{  For i := 0 to EditMemo.Lines.Count - 1 do
    Ocl := Ocl + ' ' + Trim(EditMemo.Lines[i]);
  Ocl := trim(Ocl);
}
  Ocl := trim(EditMemo.Text);

  SelectBox.Items.Clear;
  MemberHelp(Ocl);
  OperationHelp(Ocl);

{  if Ocl = '' then
  begin
    Mode := Dot;
    trimOcl := OclExpr;
  end else if Copy(Ocl, Length(Ocl) - 1, 2) = '->' then
  begin
    Mode := Arrow;
    TrimOcl := Copy(Ocl, 1, Length(Ocl)-2);
  end else if OclExpr[Length(Ocl)] = '.' then
  begin
    Mode := Dot;
    trimOcl := Copy(Ocl, 1, Length(Ocl) - 1);
  end else
    Mode := unKnown;
}
end;

procedure TBoldOclPropEditForm.ClearClick(Sender: TObject);
begin
  EditMemo.Lines.Clear;
  EditMemoChange(Sender)
end;

procedure TBoldOclPropEditForm.RemoveLastClick(Sender: TObject);
var
  line: string;

procedure RemovePrefix;
begin
  if line = '' then
    exit;
  if line[length(line)] = '.' then
    delete(line, length(line), 1)
  else if Copy(line, Length(line) - 1, 2) = '->' then
    delete(line, length(line) - 1, 2);
end;

begin
  // rewrite this entire procedure...
  Line := trim(EditMemo.Text);
  if Line = '' then exit;
  while (line <> '') and
        (not (line[length(line)] in ['.', ' ']) and
        (Copy(line, Length(line) - 1, 2) <> '->')) do
    delete(line, length(line), 1);
  RemovePrefix;
  Editmemo.Text := trim(Line);
end;

procedure TBoldOclPropEditForm.SyntaxcbClick(Sender: TObject);
begin
  ShowSyntaxErrors := syntaxcb.Checked;
  EditMemoChange(Sender);
end;

procedure TBoldOclPropEditForm.TypescbClick(Sender: TObject);
var
  OldIndex: Integer;
  OldTop: Integer;
begin
  OldIndex := SelectBox.ItemIndex;
  OldTop := SelectBox.TopIndex;
  ShowTypes := Typescb.checked;
  EditMemoChange(Sender);
  SelectBox.ItemIndex := OldIndex;
  SelectBox.TopIndex := OldTop;
end;

procedure TBoldOclPropEditForm.SetSelection;
var
  i, j: integer;
begin
  if EditMemo.SelLength = 0 then
  begin
    i := EditMemo.SelStart;
    while (i > 0) and not (EditMemo.Text[i]  in ['«', '»']) do
      dec(i);

    if (i = 0)or (EditMemo.Text[i] = '»') then
      exit;

    j := i + 1;
    while (j <= Length(EditMemo.Text)) and not (EditMemo.Text[j] in ['«', '»']) do
      Inc(j);

    if (j > Length(EditMemo.Text)) or (EditMemo.Text[j] = '«') then
      exit;

    EditMemo.SelStart := i - 1;
    EditMemo.SelLength := j - i + 1;
  end;
end;

procedure TBoldOclPropEditForm.EditMemoMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SetSelection;
end;

procedure TBoldOclPropEditForm.EditMemoKeyPress(Sender: TObject; var Key: Char);
begin
  SetSelection;
  if Key = BOLDESC then
    ModalResult := mrCancel;
end;

procedure TBoldOclPropEditForm.OKClick(Sender: TObject);
begin
  Modalresult := mrOK;
end;

procedure TBoldOclPropEditForm.CancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TBoldOclPropEditForm.EditMemoKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
   SetSelection;
end;

{
procedure TBoldOclPropEditForm.AddVarClick(Sender: TObject);
begin
   VariableList.Items.Add('NewVariable');
   VariableList.ItemIndex := VariableList.Items.Count - 1;
   VariableNameEdit.Text := VariableList.Items[VariableList.ItemIndex];
   VariableNameEdit.AutoSelect := true;
   SetFocusedControl(VariableNameEdit);
end;

procedure TBoldOclPropEditForm.VariablenameEditChange(Sender: TObject);
begin
  VariableNameEdit.AutoSelect := false;
end;

procedure TBoldOclPropEditForm.VariablenameEditExit(Sender: TObject);
begin
  if not VariableNameEdit.readOnly then
    VariableList.Items[VariableList.ItemIndex] := VariableNameEdit.Text;
end;

procedure TBoldOclPropEditForm.VariablenameEditEnter(Sender: TObject);
begin
  variableNameEdit.ReadOnly := variableList.ItemIndex = -1;
end;

procedure TBoldOclPropEditForm.VariableListClick(Sender: TObject);
begin
  if not (variableList.ItemIndex = -1) then
    VariableNameEdit.Text := VariableList.Items[VariableList.ItemIndex];
end;

procedure TBoldOclPropEditForm.RemoveVariableClick(Sender: TObject);
var
  OldIndex: Integer;
begin
  if VariableList.ItemIndex <> -1 then
  begin
    OldIndex := VariableList.ItemIndex;
    VariableList.Items.Delete(VariableList.ItemIndex);
    if OldIndex < VariableList.Items.Count then
      VariableList.ItemIndex := OldIndex
    else if VariableList.Items.Count > 0 then
      VariableList.ItemIndex := OldIndex - 1;
  end;
end;

procedure TBoldOclPropEditForm.ContextComboChange(Sender: TObject);
begin
  Context := TBoldElementTypeInfo(ContextCombo.Items.Objects[ContextCombo.ItemIndex]);
end;
}

procedure TBoldOclPropEditForm.SelectBoxDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  StringfromSelect: String;
begin
  with (Control as TListBox) do
  begin
    Canvas.FillRect(Rect);
    StringFromSelect := (Control as TListBox).items[Index];
    case StringFromSelect[1] of
      'M': ImageList1.Draw(Canvas, Rect.Left + 2, Rect.Top, 0); //Member
      'C': ImageList1.Draw(Canvas, Rect.Left + 2, Rect.Top, 1); //Class
      'A': ImageList1.Draw(Canvas, Rect.Left + 2, Rect.Top, 2); //Assoc/role
      'E': ImageList1.Draw(Canvas, Rect.Left + 2, Rect.Top, 3); //Method
      'O': ImageList1.Draw(Canvas, Rect.Left + 2, Rect.Top, 4); //Operation (OCL)
    end;
    if pos(' | ', StringFromSelect) <> 0 then
      Delete(StringFromSelect, 1, Pos(' | ', StringFromSelect) + 2);

    Canvas.TextOut(ImageList1.Width + Rect.Left + 2, Rect.Top, StringfromSelect);
	end;
end;

procedure TBoldOclPropEditForm.WMGetMinMaxInfo(Var Msg: TWMGetMinMaxINfo);
begin
  with Msg.MinMaxInfo^ do
  begin
    ptMinTrackSize.x := 525;
    ptMintrackSize.y := 264;
  end;
  Msg.Result := 0;
end;

procedure TBoldOclPropEditForm.SelectBoxKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  Symbol: TObject;
begin
  if (Key = 112) and (SelectBox.ItemIndex <> -1) then
  begin
    Symbol := SelectBox.Items.Objects[SelectBox.ItemIndex];
    if symbol is TBoldOclSymbol then
    begin
      SelectBox.HelpContext := (Symbol as TBoldOclSymbol).HelpContext;
    end
    else
      SelectBox.HelpContext := 0;
  end;
end;

procedure TBoldOclPropEditForm.SetVariables(const Value: TBoldExternalVariableList);
begin
  fVariables := Value;
  EditMemoChange(nil);
end;

procedure TBoldOclPropEditForm.Copymessagestoclipboard1Click(Sender: TObject);
var
  s: string;
begin
  if Assigned(Context) then
    s := 'Context: ' + Context.ExpressionName + BOLDCRLF      // do not localize
  else
    s := '';
  ClipBoard.AsText := s +
                      'OCL Expression: ' + BOLDCRLF +         // do not localize
                      EditMemo.Text + BOLDCRLF + BOLDCRLF +
                      'Parser messages: ' + BOLDCRLF +        // do not localize
                      ParserMessages.lines.text;
end;

procedure TBoldOclPropEditForm.imgModelErrorsClick(Sender: TObject);
begin
  Showmessage(Format(sModelContainsErrors, [BoldCRLF, BoldCRLF, SystemTypeInfo.InitializationLog.text]));
end;

procedure TBoldOclPropEditForm.InfoBrowserClick(Sender: TObject);
var
  URL: String;
begin
  if (GetBoldBasePath <> '') and fileexists(GetBoldBasePath+'\'+OCLInfoLocal) then
    URL := GetBoldBasePath+'\'+OCLInfoLocal
  else
    URL := OCLInfoURL;

  ShellExecute(0, 'open', PChar(URL), '', '', SW_SHOWMAXIMIZED); // do not localize
end;

end.
