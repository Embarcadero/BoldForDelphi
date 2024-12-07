{ Global compiler directives }
{$include bold.inc}
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
  ShellAPI,
  System.ImageList;

type
  TBoldOclPropEditForm = class(TForm)
    ExpBottomPanel: TPanel;
    TabControlPanel: TPanel;
    ExprEditPageControl: TPageControl;
    ExpressionPage: TTabSheet;
    pnlClient: TPanel;
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
    ParserMsg: TLabel;
    Splitter2: TSplitter;
    imgModelErrors: TImage;
    pnlOKCancel: TPanel;
    btnOK: TButton;
    btnCancel: TButton;
    btnShowInfo: TButton;
    VariablesPageControl: TPageControl;
    OclVariablesTabSheet: TTabSheet;
    pnlOclVariables: TPanel;
    VariablesListBox: TListBox;
    ExpressionTypePageControl: TPageControl;
    ExpressionTypeTabSheet: TTabSheet;
    pnlRightSide: TPanel;
    SelectBox: TListBox;
    pnlFilterPersistence: TPanel;
    Derivedcb: TCheckBox;
    SplitterRight: TSplitter;
    SplitterLeft: TSplitter;
    filterCombo: TComboBox;
    MRUPopupMenu: TPopupMenu;
    Persistentcb: TCheckBox;
    Transientcb: TCheckBox;
    btnSetFont: TButton;
    FontDialog1: TFontDialog;
    procedure btnSetFontClick(Sender: TObject);
    procedure SelectBoxDblClick(Sender: TObject);
    procedure EditMemoEnter(Sender: TObject);
    procedure SelectBoxKeyPress(Sender: TObject; var Key: Char);
    procedure EditMemoChange(Sender: TObject);
    procedure ClearClick(Sender: TObject);
    procedure RemoveLastClick(Sender: TObject);
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
    procedure SyntaxcbClick(Sender: TObject);
    procedure FilterClick(Sender: TObject);
    procedure filterComboChange(Sender: TObject);
    procedure filterComboExit(Sender: TObject);
    procedure VariableTypescbClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    fContext: TBoldElementTypeInfo;
    fSystemTypeInfo: TBoldSystemTypeInfo;
    fOclEvaluator: TBoldOcl;
    fVariables: TBoldExternalVariableList;
    fCompleteSelectList: TStrings;
    fMRUExpressionList: TStringList;
    FOnExpressionChanged: TNotifyEvent;
    procedure SetOclExpr(Expr: String);
    function GetOclExpr: String;
    procedure SetContext(Context: TBoldElementTypeInfo);
    procedure SetSelection;
    procedure CleanParserMsg;
    procedure MemberHelp(Ocl: String);
    procedure OperationHelp(Ocl: String);
    procedure ApplyFilter;
    procedure UpdateVariables;
    function CheckOcl(Ocl: String): TBoldElementTypeInfo;
    function GetOclEvaluator: TBoldOcl;
    procedure WMGetMinMaxInfo(Var Msg: TWMGetMinMaxINfo); message WM_GETMINMAXINFO;
    procedure SetVariables(const Value: TBoldExternalVariableList);
    procedure AddExpressionToMRUList;
    procedure FetchMRUExpressionList;
    procedure MRUPopUpMenuClick(Sender: TObject);
    function HasVariables: boolean;
    procedure UpdateWidth;
    procedure SetOclEvaluator(const Value: TBoldOCL);
    procedure SaveFontSettings(const Font: TFont);
    procedure LoadFontSettings(const Font: TFont);
  public
    { Public declarations }
    ShowSyntaxErrors: Boolean;
    ShowDerived, ShowPersistent, ShowTransient: Boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property OclEvaluator: TBoldOCL read GetOclEvaluator write SetOclEvaluator;
    property Context: TBoldElementTypeInfo read fContext write SetContext;
    property SystemTypeInfo: TBoldSystemTypeInfo read fSystemTypeInfo;
    property OclExpr: String read GetOclExpr write SetOclExpr;
    property Variables: TBoldExternalVariableList read fVariables write SetVariables;
    property OnExpressionChanged: TNotifyEvent read FOnExpressionChanged write FOnExpressionChanged;
  end;

implementation

uses
  SysUtils,
  StrUtils,

  BoldCoreConsts,
  BoldUtils,
  BoldDefs,
  BoldRegistry,
  BoldOclError,
  BoldSystem,
  BoldOclClasses,
  BoldBase;

{$R *.dfm}

const
  OCLInfoURL = 'https://www.omg.org/spec/OCL/2.0/PDF';
  OCLInfoLocal = 'doc\oclinfo.html';
  cMRUExpressionRegKey = '\MRU';
  cMRUExpressionRegKeyName = 'OCL Expression';
  cFontSettingRegKey = '\OCLEditorFont';
  cFontSettingRegKeyName = 'FontName';
  cFontSettingRegKeyStyle = 'FontStyle';
  cFontSettingRegKeySize = 'FontSize';
  cHiddenOCLVariables: array[0..3] of string = ('Nil', 'True', 'False', 'TimeStampNow');

type
  ModeType = (Arrow, Dot, unKnown);

procedure TBoldOclPropEditForm.CleanParserMsg;
begin
  ParserMessages.lines.clear;
end;

procedure TBoldOclPropEditForm.ClearClick(Sender: TObject);
begin
  EditMemo.Lines.Clear;
  EditMemoChange(Sender);
end;

constructor TBoldOclPropEditForm.Create(AOwner: TComponent);
begin
  inherited;
  fCompleteSelectList := TStringList.Create;
  ShowDerived := true;
  ShowPersistent := true;
  ShowTransient := true;
  ExprEditPageControl.ActivePage := ExpressionPage;
  SetFocusedControl(EditMemo);
end;

destructor TBoldOclPropEditForm.destroy;
begin
  fCompleteSelectList.free;
  inherited;
end;

function TBoldOclPropEditForm.GetOclEvaluator;
begin
  result := fOclEvaluator;
  if not Assigned(result) and Assigned(SystemTypeInfo) then
    result := SystemTypeInfo.Evaluator as TBoldOcl
end;

procedure TBoldOclPropEditForm.SetOclEvaluator(const Value: TBoldOCL);
begin
  fOclEvaluator := Value;
end;

procedure TBoldOclPropEditForm.SetContext(Context: TBoldElementTypeInfo);
begin
  fContext := Context;
  if assigned(Context) then
  begin
    fSystemTypeInfo := Context.SystemTypeInfo as TBoldSystemTypeInfo;
    Caption:= 'OCL expression editor - Context: ' + Context.AsString;
  end
  else
  begin
    Caption:= 'OCL expression editor - Context: <unknown>';
    CleanParserMsg;
    ParserMessages.lines.Add(sNoContextNoSupport);
  end;

  if (not assigned(SystemTypeInfo)) and (TBoldSystem.DefaultSystem <> nil) then
    fSystemTypeInfo := TBoldSystem.DefaultSystem.BoldType as tBoldSystemTypeInfo;

  filterCombo.ItemIndex := 0;

  UpdateWidth;

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

  EditMemoChange(nil);
  imgModelErrors.visible := assigned(SystemTypeInfo) and (SystemTypeInfo.InitializationLog.count > 0);
end;

procedure TBoldOclPropEditForm.SetOclExpr(Expr: String);
var
  i: integer;
begin
  EditMemo.Text := Expr;
  i := Length(EditMemo.Text);
  EditMemo.SelStart := i - 2;
  EditMemoChange(nil);
end;

function TBoldOclPropEditForm.GetOclExpr: String;
begin
  result := EditMemo.Text;
end;

function TBoldOclPropEditForm.HasVariables: boolean;
begin
  result := (Assigned(fVariables) and (fVariables.count > 0)) or (Assigned(OclEvaluator) and (OclEvaluator.VariableCount > Length(cHiddenOCLVariables)));
end;

procedure TBoldOclPropEditForm.AddExpressionToMRUList;
var
  lRegistry: TBoldRegistry;
begin
  if fMRUExpressionList.IndexOf(EditMemo.Text) = -1 then
  begin
    lRegistry := TBoldRegistry.Create;
    try
      fMRUExpressionList.insert(0, EditMemo.Text);
      if fMRUExpressionList.Count > 10 then
      begin
        fMRUExpressionList.Delete(10);
      end;
      if lRegistry.OpenKey(cMRUExpressionRegKey) then
      begin
        lRegistry.WriteString(cMRUExpressionRegKeyName, fMRUExpressionList.CommaText);
        lRegistry.CloseKey;
      end;
    finally
      lRegistry.free;
    end;
  end;
end;

type
  TFilterType = (ftAll, ftAttribute, ftSingleLink, ftMultilink, ftOperation, ftType, ftText);

procedure TBoldOclPropEditForm.ApplyFilter;

  function ExtractFilterType(const AString: string): TFilterType;
  var
    c: char;
  begin
    result := ftAll;
    if (copy(AString, 3,1) = '|') and (Length(AString) > 4) then
    begin
      c := AString[1];
      case c of
        '1': result := ftAttribute;
        '2': result := ftSingleLink;
        '3': result := ftMultiLink;
        '4': result := ftType;
        '5': result := ftOperation;
      end;
    end;
  end;

var
  i: integer;
  vElementType: TFilterType;
  vFilterType: TFilterType;
  vInclude: boolean;
  vFilterText: string;
begin
  case filterCombo.ItemIndex of
     -1: vFilterType := ftText;
      Ord(ftAttribute)..Ord(ftType): vFilterType := TFilterType(filterCombo.ItemIndex);
    else
      vFilterType := ftAll;
  end;
  vFilterText :=  Trim(FilterCombo.Text);

  for I := 0 to fCompleteSelectList.Count - 1 do
  begin
    vInclude := vFilterType = ftAll;
    if vFilterType <> ftAll then
    begin
      vElementType := ExtractFilterType(fCompleteSelectList[i]);
      case vFilterType of
        ftAll: vInclude := true;
        ftAttribute..ftOperation: vInclude := vElementType = vFilterType;
        ftType: vInclude := true;
        ftText: vInclude := (vFilterText = '') or AnsiContainsText(Copy(fCompleteSelectList[i], 5, MaxInt), vFilterText);
      end;
    end;
    if vInclude then
      SelectBox.Items.Add(fCompleteSelectList[i]);
  end;
end;

procedure TBoldOclPropEditForm.LoadFontSettings(const Font: TFont);
var
  lRegistry: TBoldRegistry;
begin
  lRegistry := TBoldRegistry.Create;
  try
    if lRegistry.OpenKey(cFontSettingRegKey) then
    begin
      Font.Name := lRegistry.ReadString(cFontSettingRegKeyName, Font.Name);
      Font.Size := lRegistry.ReadInteger(cFontSettingRegKeySize, Font.Size);
      Font.Style := TFontStyles(Byte(lRegistry.ReadInteger(cFontSettingRegKeyStyle, Byte(Font.Style))));
      lRegistry.CloseKey;
    end;
  finally
    lRegistry.Free;
  end;
end;

procedure TBoldOclPropEditForm.SaveFontSettings(const Font: TFont);
var
  lRegistry: TBoldRegistry;
begin
  lRegistry := TBoldRegistry.Create;
  try
    if lRegistry.OpenKey(cFontSettingRegKey) then
    begin
      lRegistry.WriteString(cFontSettingRegKeyName, Font.Name);
      lRegistry.WriteInteger(cFontSettingRegKeySize, Font.Size);
      lRegistry.WriteInteger(cFontSettingRegKeyStyle, Byte(Font.Style));
      lRegistry.CloseKey;
    end;
  finally
    lRegistry.Free;
  end;
end;

procedure TBoldOclPropEditForm.btnSetFontClick(Sender: TObject);
begin
  FontDialog1.Font := EditMemo.Font;
  if FontDialog1.Execute then
  begin
    EditMemo.Font := FontDialog1.Font;
    SaveFontSettings(EditMemo.Font);
  end;
end;

function TBoldOclPropEditForm.CheckOcl(Ocl: String): TBoldElementTypeInfo;

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
    ParserMessages.lines.Add(sCurrentTypeIsX + Result.AsString)
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
        Addition := '5 | ' + prefix[Symbol.IsDotNotation] + Symbol.SymbolName;
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
                  Addition := Addition + '«List<' + (Symbol.FormalArguments[j] as TBoldListTypeInfo).ListElementTypeInfo.ExpressionName + '>»,'
                else
                  Addition := Addition + '«List<AnyArg>»,'
              end
              else
                Addition := Addition + '«' + Symbol.FormalArguments[j].ExpressionName + '»,'
            end
            else
              Addition := Addition + '«AnyArg»,';
          Delete(Addition, Length(Addition), 1);
          Addition := Addition + ')';
        end;
        if Assigned(Symbol.ResultType) then
          fCompleteSelectList.AddObject(Addition +  ' : ' + Symbol.ResultType.ExpressionName , Symbol)
        else
          fCompleteSelectList.AddObject(Addition +  ' ', Symbol);
      end;
    end;
  end;
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
  Line := trim(EditMemo.Text);
  if Line = '' then exit;
  while (line <> '') and
        (not CharInSet(line[length(line)], ['.', ' ']) and
        (Copy(line, Length(line) - 1, 2) <> '->')) do
    delete(line, length(line), 1);
  RemovePrefix;
  Editmemo.Text := trim(Line);
end;

procedure TBoldOclPropEditForm.MemberHelp(Ocl: String);
const
  prefix: array[false..true] of string = ('', '.');
var
  I: Integer;
  ClassInfo: TBoldClasstypeInfo;
  SystemInfo: TBoldSystemtypeInfo;
  Member: TBoldMemberRTInfo;
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
    for I := 0 to ClassInfo.AllMembersCount - 1 do
    begin
      Member := ClassInfo.AllMembers[I];
      if not ShowDerived and Member.IsDerived then continue;
      if not ShowPersistent and Member.Persistent  then continue;
      if not ShowTransient and not (Member.Persistent or Member.IsDerived) then continue;
      if ClassInfo.AllMembers[I] is TBoldAttributeRTInfo then
      begin
        Attr := ClassInfo.AllMembers[I] as TBoldAttributeRTInfo;
        Exprname := '1 | ' + Prefix[ocl <> ''] + Attr.ExpressionName;
        fCompleteSelectList.Add(Exprname + ': ' + Attr.BoldType.ExpressionName)
      end else if ClassInfo.AllMembers[I] is TBoldRoleRTInfo then
      begin
        Role := ClassInfo.AllMembers[I] as TBoldRoleRTInfo;
        if role.IsNavigable then
        begin
          if role.IsMultiRole then
            Exprname := '3 | ' + Prefix[ocl <> ''] + Role.ExpressionName
          else
            Exprname := '2 | ' + Prefix[ocl <> ''] + Role.ExpressionName;
          if role.IsMultiRole then
            fCompleteSelectList.Add(ExprName + ': List<' + Role.ClassTypeInfoOfOtherEnd.Expressionname + '>')
          else
            fCompleteSelectList.Add(ExprName + ': ' + Role.ClassTypeInfoOfOtherEnd.Expressionname)
        end;
      end;
    end;
  end
  else if ExpressionType is TBoldSystemTypeInfo then
  begin
    SystemInfo := ExpressionType as TBoldSystemTypeInfo;
    for i := 0 to SystemInfo.TopSortedClasses.Count - 1 do
      with SystemInfo.TopSortedClasses[i] do
        fCompleteSelectList.Add('4 | ' + ExpressionName + ': ' + DelphiName);
  end;
end;

procedure TBoldOclPropEditForm.MRUPopUpMenuClick(Sender: TObject);
var
  lMenuItem: TMenuItem;
begin
  lMenuItem := (Sender as TMenuItem);
  EditMemo.Text := fMRUExpressionList[lMenuItem.Parent.IndexOf(lMenuItem)];
end;

procedure TBoldOclPropEditForm.SelectBoxDblClick(Sender: TObject);
var
  Addition: String;
  vListBox: TListBox;
begin
  if not assigned(Context) then
    exit;
  if not(Sender is TListBox) then
    exit;

  vListBox := Sender as TListBox;
  if vListBox.ItemIndex = -1 then
    vListBox.ItemIndex := 0;
  Addition := vListBox.Items[vListBox.ItemIndex];

  if pos(' | ', Addition) <> 0 then
    Delete(Addition, 1, Pos(' | ', Addition) + 2);

  Addition := Copy(Addition, 1, Pos(' ', Addition) - 1);

  if Pos(':', Addition) = Length(Addition) then
    Addition := Copy(Addition, 1, Length(Addition) - 1);

  EditMemo.Lines[EditMemo.Lines.Count - 1] :=
    EditMemo.Lines[EditMemo.Lines.Count - 1] +
    Addition;

  if filterCombo.ItemIndex = -1 then
    filterCombo.Clear;

  EditMemoChange(Sender);
end;

procedure TBoldOclPropEditForm.EditMemoEnter(Sender: TObject);
begin
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

  VariablesListBox.Items.BeginUpdate;
  SelectBox.Items.BeginUpdate;
  fCompleteSelectList.BeginUpdate;
  try
    SelectBox.Clear;
    fCompleteSelectList.Clear;
    MemberHelp(Ocl);
    OperationHelp(Ocl);
    ApplyFilter;
    UpdateVariables;
  finally
    VariablesListBox.Items.EndUpdate;
    SelectBox.Items.EndUpdate;
    fCompleteSelectList.EndUpdate;
  end;
  if Assigned(fOnExpressionChanged) then
    OnExpressionChanged(self);

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

procedure TBoldOclPropEditForm.SyntaxcbClick(Sender: TObject);
begin
  ShowSyntaxErrors := syntaxcb.Checked;
  EditMemoChange(Sender);
end;

procedure TBoldOclPropEditForm.FilterClick(Sender: TObject);
var
  OldIndex: Integer;
  OldTop: Integer;
begin
  OldIndex := SelectBox.ItemIndex;
  OldTop := SelectBox.TopIndex;
  ShowDerived := Derivedcb.Checked;
  ShowPersistent := Persistentcb.Checked;
  ShowTransient := Transientcb.Checked;
  EditMemoChange(Sender);
  SelectBox.ItemIndex := OldIndex;
  SelectBox.TopIndex := OldTop;
end;

procedure TBoldOclPropEditForm.UpdateVariables;

  procedure AddVariable(const AName: string; AValueType: TBoldElementTypeInfo);
  var
    s: string;
  begin
    if not Assigned(AValueType) then
    begin
      VariablesListBox.Items.Add('5 | ' + AName + ': ' + 'Nil');
      exit;
    end;
    if AValueType is TBoldListTypeInfo then
      s := '3 | '
    else
    if AValueType is TBoldAttributeTypeInfo then
      s := '1 | '
    else
    if AValueType is TBoldClassTypeInfo then
      s := '2 | '
    else
    if AValueType is TBoldSystemTypeInfo then
      s := '4 | '
    else
    if AValueType is TBoldTypeTypeInfo then
      s := '5 | '
    else
      Assert(False, 'Unhandled ValueType: ' + AValueType.ClassName);
    s := s + AName + ': ' + AValueType.ExpressionName;
//    if VariablesListBox.Items.IndexOf(s) = -1 then
      VariablesListBox.Items.Add(s);
  end;

var
  i: integer;
begin
  VariablesPageControl.Visible := HasVariables;
  VariablesListBox.Items.BeginUpdate;
  VariablesListBox.Clear;
  try
    if Assigned(fVariables) then
      for I := 0 to Variables.Count - 1 do
        AddVariable(Variables[i].Name, Variables[i].ValueType);
    for i := 0 to OclEvaluator.VariableCount - 1 do
      with OclEvaluator.Variables[i] as TBoldOCLVariableBinding do
        if AnsiIndexText(VariableName, cHiddenOCLVariables) = -1 then
          AddVariable(VariableName, BoldType);
  finally
    VariablesListBox.Items.EndUpdate;
  end;
end;

procedure TBoldOclPropEditForm.UpdateWidth;
begin
  if not visible then
  begin
    if HasVariables then
      Width := 900
    else
      Width := 720;
  end;
end;

procedure TBoldOclPropEditForm.VariableTypescbClick(Sender: TObject);
var
  OldIndex: Integer;
  OldTop: Integer;
begin
  OldIndex := VariablesListBox.ItemIndex;
  OldTop := VariablesListBox.TopIndex;
  EditMemoChange(Sender);
  VariablesListBox.ItemIndex := OldIndex;
  VariablesListBox.TopIndex := OldTop;
end;

procedure TBoldOclPropEditForm.SetSelection;
var
  i, j: integer;
begin
  if EditMemo.SelLength = 0 then
  begin
    i := EditMemo.SelStart;
    while (i > 0) and not CharInSet(EditMemo.Text[i], ['«', '»']) do
      dec(i);

    if (i = 0)or (EditMemo.Text[i] = '»') then
      exit;

    j := i + 1;
    while (j <= Length(EditMemo.Text)) and not CharInSet(EditMemo.Text[j], ['«', '»']) do
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

procedure TBoldOclPropEditForm.FetchMRUExpressionList;
var
  lRegistry: TBoldRegistry;
  lIndex: Integer;
  lMenuItem: TMenuItem;
begin
  lRegistry := TBoldRegistry.Create;
  try
    if lRegistry.OpenKey(cMRUExpressionRegKey) then
    begin
      fMRUExpressionList.CommaText := lRegistry.ReadString(cMRUExpressionRegKeyName, '');
      lRegistry.CloseKey;
    end;
  finally
    lRegistry.free;
  end;
  for lIndex := 0 to fMRUExpressionList.Count - 1 do
  begin
    lMenuItem := TMenuItem.Create(Self);
    lMenuItem.Caption := '&' + IntToStr(lIndex+1) + ': ' + fMRUExpressionList[lIndex];
    lMenuItem.OnClick := MRUPopUpMenuClick;
    MRUPopupMenu.Items.Add(lMenuItem);
  end;
end;

procedure TBoldOclPropEditForm.filterComboChange(Sender: TObject);
begin
  EditMemoChange(Sender);
end;

procedure TBoldOclPropEditForm.filterComboExit(Sender: TObject);
begin
  if (FilterCombo.ItemIndex = -1) and (FilterCombo.Text = '') then
  begin
    FilterCombo.ItemIndex := 0;
    EditMemoChange(Sender);
  end;
end;

procedure TBoldOclPropEditForm.FormCreate(Sender: TObject);
begin
  fMRUExpressionList := TStringList.Create;
  FetchMRUExpressionList;
  LoadFontSettings(EditMemo.Font);
end;

procedure TBoldOclPropEditForm.FormDestroy(Sender: TObject);
begin
  fMRUExpressionList.free;
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
  AddExpressionToMRUList;
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
  s: string;
  r: TRect;
  vXOffset: integer;
begin
  with (Control as TListBox) do
  begin
    Canvas.FillRect(Rect);
    StringFromSelect := (Control as TListBox).items[Index];
    case StringFromSelect[1] of
      '1': ImageList1.Draw(Canvas, Rect.Left + 2, Rect.Top, 0);
      '2': ImageList1.Draw(Canvas, Rect.Left + 2, Rect.Top, 2);
      '3': ImageList1.Draw(Canvas, Rect.Left + 2, Rect.Top, 3);
      '4': ImageList1.Draw(Canvas, Rect.Left + 2, Rect.Top, 1);
      '5': ImageList1.Draw(Canvas, Rect.Left + 2, Rect.Top, 4);
    end;
    if pos(' | ', StringFromSelect) <> 0 then
      Delete(StringFromSelect, 1, Pos(' | ', StringFromSelect) + 2);

    vXOffset := 2 + ImageList1.Width;
    if not ((odFocused in State) or (odSelected in State)) then
    begin
      s := StringFromSelect;

      if pos(':', s) <> 0 then
        Delete(s, Pos(':', s), MaxInt);

      if pos(' ', s) <> 0 then
        Delete(s, Pos(' ', s), MaxInt);

      if pos('(', s) <> 0 then
        Delete(s, Pos('(', s), MaxInt);

      r := Rect;
      r.Left := r.Left + vXOffset;
      Canvas.Font.Color := clBlack;
      DrawText(Canvas.Handle,
        PChar(S),
        Length(S),
        r,
        DT_LEFT or DT_WORDBREAK or DT_CALCRECT);

      DrawText(Canvas.Handle,
        PChar(S),
        Length(S),
        r,
        DT_LEFT or DT_WORDBREAK);

      vXOffset := r.Right;
      Canvas.Font.Color := $00A0A0A0;;
      StringFromSelect := Copy(StringfromSelect, Length(s)+1, MaxInt);
    end;

    Canvas.TextOut(Rect.Left + vXOffset, Rect.Top, StringFromSelect);
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
  UpdateWidth;
end;

procedure TBoldOclPropEditForm.Copymessagestoclipboard1Click(Sender: TObject);
var
  s: string;
begin
  if Assigned(Context) then
    s := 'Context: ' + Context.ExpressionName + BOLDCRLF
  else
    s := '';
  ClipBoard.AsText := s +
                      'OCL Expression: ' + BOLDCRLF +
                      EditMemo.Text + BOLDCRLF + BOLDCRLF +
                      'Parser messages: ' + BOLDCRLF +
                      ParserMessages.lines.text;

end;

procedure TBoldOclPropEditForm.imgModelErrorsClick(Sender: TObject);
begin
  Showmessage(sModelContainsErrors + BoldCRLF + BoldCRLF+ SystemTypeInfo.InitializationLog.text);
end;

procedure TBoldOclPropEditForm.InfoBrowserClick(Sender: TObject);
var
  URL: String;
begin
  if (GetBoldBasePath <> '') and fileexists(GetBoldBasePath+'\'+OCLInfoLocal) then
    URL := GetBoldBasePath+'\'+OCLInfoLocal
  else
    URL := OCLInfoURL;

  ShellExecute(0, 'open', PChar(URL), '', '', SW_SHOWMAXIMIZED);
end;

end.