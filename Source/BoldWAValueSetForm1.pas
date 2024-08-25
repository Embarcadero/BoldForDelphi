
{ Global compiler directives }
{$include bold.inc}
unit BoldWAValueSetForm1;

interface

uses
  Windows,
  Messages,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ComCtrls,
  StdCtrls,
  Buttons,
  ToolWin,
  BoldWAInterfaces,
  ActnList, System.Actions;

type
  TStageType = (stBegin, stIntermediate, stFinal);
  TEditMode = (emInsert, emEdit, emNone);

  TValueSetForm1 = class(TForm, IWizardForm)
    PageControl1: TPageControl;
    tsClassDef: TTabSheet;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    Label1: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    Label10: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    edExpressionName: TEdit;
    edDelphiName: TEdit;
    edValuePrefix: TEdit;
    edUnitName: TEdit;
    edModelName: TEdit;
    tsValues: TTabSheet;
    GroupBox2: TGroupBox;
    ToolBar1: TToolBar;
    bbInsert: TBitBtn;
    bbDelete: TBitBtn;
    bbEdit: TBitBtn;
    bbDown: TBitBtn;
    bbUp: TBitBtn;
    ListViewValues: TListView;
    ActionList1: TActionList;
    ActionInsert: TAction;
    ActionDelete: TAction;
    ActionEdit: TAction;
    ToolButton1: TToolButton;
    procedure ListViewValuesKeyPress(Sender: TObject; var Key: Char);
    procedure InsertValue;
    function AddValueToList(const Name, Reps: string): Boolean;
    procedure EditValue;
    procedure DeleteValue;
    procedure setUnitGeneratorIntf(Intf: IUnitGenerator);
    function getUnitGeneratorIntf: IUnitGenerator;

    procedure MoveUp;
    procedure MoveDown;
    procedure edExpressionNameChange(Sender: TObject);
    procedure ActionInsertExecute(Sender: TObject);
    procedure ActionDeleteExecute(Sender: TObject);
    procedure tsClassDefShow(Sender: TObject);
    procedure ActionEditExecute(Sender: TObject);
    procedure ListViewValuesDblClick(Sender: TObject);
    procedure bbDownClick(Sender: TObject);
    procedure bbUpClick(Sender: TObject);
    procedure edDelphiNameChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure edUnitNameChange(Sender: TObject);
    procedure tsValuesShow(Sender: TObject);
  private
    { Private declarations }
    fStage: TStageType;
    fSteppedBack: Boolean;
    fUnitGenerator: IUnitGenerator;
    fEditMode: TEditMode;
    fEnablenext: TEnableNextEvent;
    fUnitName_Locked,
    fDelphiName_Locked: Boolean;
    procedure setEnableNext(Value: TEnableNextEvent);
    procedure EnableNextBtn(const Enable: Boolean);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent);  override;
    procedure AssignParent(aParent: TWinControl); 
    procedure ClearParent;
    procedure Initialize;
    function Next: integer;
    function Back: integer;
    procedure Cancel;
    procedure Finish;
    function GetSteppedBack: Boolean;
    function IsValidClassDef: Boolean;
    property Stage: TStageType read fStage;
    property SteppedBack: Boolean read GetSteppedBack default false ;
    property UnitGenerator: IUnitGenerator read getUnitGeneratorIntf write setUnitGeneratorIntf;
    property EnableNext: TEnableNextEvent write setEnableNext;
  end;

var
  ValueSetForm1: TValueSetForm1;

implementation

uses
  SysUtils,
  BoldUtils,
  BoldWAdmTemplates,
  BoldVclUtils,
  BoldWAValueSetDlg;

{$R *.dfm}

function BooleanToStr(value: Boolean): string;
begin
  if value then
    Result := 'true'
  else
    Result := 'false';
end;

constructor TValueSetForm1.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  PageControl1.ActivePage := tsClassDef;
  tsClassDef.TabVisible := false;
  tsValues.TabVisible := false;
  fUnitName_Locked := true;
  fDelphiName_Locked := true;
end;

procedure TValueSetForm1.ListViewValuesKeyPress(Sender: TObject;
  var Key: Char);
begin
  if (Key = #13) then
  ActionEditExecute(Sender);
end;

procedure TValueSetForm1.InsertValue;
var
  NewItem: TListItem;
  InputDlg: TValueSetDlg;
  new_Name, new_Reps: string;
  Value_prefix: string;
begin
  fEditMode := emInsert;
  Value_Prefix := Trim(edValuePrefix.Text);
  NewItem := ListViewValues.Items.Add;
  NewItem.SubItems.Add('');
  ListViewValues.Selected := NewItem;
  InputDlg := TValueSetDlg.Create(self);
  Inputdlg.Caption := 'Insert Value';
  try
    if InputDlg.Edit(NewItem.Caption, NewItem.SubItems[0], new_name, new_Reps) then
      with NewItem do
      begin
        if not (IsEmptyStr(Value_Prefix)) and (Pos(Value_Prefix, New_Name) <> 1) then
          New_Name := Format('%s%s',[Value_prefix, New_Name]);
        Caption := New_Name;
        SubItems[0] := New_Reps;
      end
    else
      NewItem.Delete;
  finally
    FreeAndNil(InputDlg);
  end;
end;

function TValueSetForm1.AddValueToList(const Name, Reps: string): Boolean;
begin
  Result := true;
  if fEditMode in [emEdit, emInsert] then
  begin
    ListViewValues.Selected.Caption := Name;
    ListViewValues.Selected.SubItems[0] := Reps;
  end;
end;

procedure TValueSetForm1.DeleteValue;
var
  item: TListItem;
  index: integer;
begin
  if Assigned(ListViewValues.Selected) and (ListViewValues.Items.Count <> 0) then
    begin
      item := ListViewValues.Selected;
      index := ListViewValues.Items.IndexOf(item);
      ListViewValues.Items.Delete(index);
    end;
end;

procedure TValueSetForm1.EditValue;
var
  InputDlg: TValueSetDlg;
  new_Name, new_Reps: string;
begin
  InputDlg := TValueSetDlg.Create(self);
  InputDlg.Caption := 'Edit Value';
  try
    if InputDlg.Edit(ListViewValues.Selected.Caption, ListViewValues.Selected.SubItems[0], New_Name, New_Reps) then
      with ListViewValues.Selected do
      begin
        Caption := New_Name;
        SubItems[0] := New_Reps;
      end;
  finally
    FreeAndNil(InputDlg);
  end;
end;

procedure TValueSetForm1.setUnitGeneratorIntf(Intf: IUnitGenerator);
begin
  fUnitGenerator := Intf;
end;

function TValueSetForm1.getUnitGeneratorIntf: IUnitGenerator;
begin
  Result := fUnitGenerator;
end;

procedure TValueSetForm1.MoveDown;
var
  position: integer;
begin
  position := ListViewValues.Selected.Index;
  if position < ListViewValues.Items.Count - 1 then
    ExchangeRows(ListViewValues, position, position + 1);
end;

procedure TValueSetForm1.MoveUp;
var
  position: integer;
begin
  position := ListViewValues.Selected.Index;
  if (position > 0) then
    ExchangeRows(ListViewValues, position, position - 1);
end;

procedure TValueSetForm1.AssignParent(aParent: TWinControl);
begin
  PageControl1.Parent := aParent;
  if (aParent is TForm) then
    (aParent as TForm).helpcontext := helpcontext;
end;

procedure TValueSetForm1.ClearParent;
begin
  PageControl1.Parent := self;
end;

function TValueSetForm1.Next: integer;
begin
  if PageControl1.ActivePage = tsClassDef then
  begin
    Result := wfaNext;
    if IsValidClassDef then
    begin
      PageControl1.ActivePage := tsValues;
      EnableNextBtn(ListViewValues.Items.Count > 0);
      Result := wfaLast;
    end;
    Exit;
  end;
  if (MessageDlg('The Wizard will now generate code for your new attribute!' + #13 +
     '                               Continue? ',mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
    Result := wfaFinish
  else
    Result := wfaIgnore;
end;

procedure TValueSetForm1.Finish;
var
  UnitName,
  ExpressionName,
  DelphiName,
  ValuePrefix: string;
  Rep: PChar;
  i, j, NumberOfValues: Integer;
  Values: TStringList;
begin
  ExpressionName := Trim(edExpressionName.Text);
  UnitName := Trim(edUnitName.Text) ;
  DelphiName := Trim(edDelphiName.Text);
  ValuePrefix := Trim(edValuePrefix.Text);
  NumberOfValues := ListViewValues.Items.Count;
  with attrdatamodule.ValueSetTemplate do
  begin
    Variables.Add('UNITNAME', UnitName, []);
    Variables.Add('EXPRESSIONNAME', ExpressionName, []);
    Variables.Add('DELPHINAME', DelphiName, []);
    Variables.Add('VALUECOUNT', InttoStr(NumberOfValues), []);
    Values := TStringList.Create;
    try
      Variables.Add('VALUES', BooleanToStr(ListViewValues.Items.Count > 0), []);
      for i:= 0 to ListViewValues.Items.Count - 1 do
      begin
        Values.CommaText := ListViewValues.Items[i].SubItems[0];
        Variables.Add(format('VALUENAME.%d',[i]), ListViewValues.Items[i].Caption, []);
        Variables.Add(format('VALUEREPRESENTATIONCOUNT.%d',[i]), IntToStr(Values.Count), []);
        for j:= 0 to  Values.Count - 1 do
        begin
          Rep := PChar(Values[j]);
          Variables.Add(format('VALUEREPRESENTATION.%d.%d',[i,j]), AnsiExtractQuotedStr(Rep, ''''), []);
        end;
      end;
    finally
      FreeAndNil(Values);
    end;
  end;
  UnitGenerator.GenerateUnit(UnitName, attrdatamodule.ValueSetTemplate);
end;

function TValueSetForm1.Back: integer;
begin
  if (PageControl1.ActivePage = tsValues) then
  begin
    Result := wfaPrevious;
    PageControl1.ActivePage := tsClassDef;
    Exit;
  end;
  fSteppedBack := true;
  Result := wfaBack;
end;

procedure TValueSetForm1.Cancel;
begin
  fSteppedBack := false;
end;

function TValueSetForm1.GetSteppedBack: Boolean;
begin
  Result := fSteppedBack;
end;

procedure TValueSetForm1.setEnableNext(Value: TEnableNextEvent);
begin
  fEnableNext := Value;
end;

procedure TValueSetForm1.EnableNextBtn(const Enable: Boolean);
begin
  if Assigned(fEnableNext) then
    fEnableNext(Enable);
end;

procedure TValueSetForm1.Initialize;
begin
  Align := alClient;
  self.TabOrder := 0;
  PageControl1.ActivePage := tsClassDef;
end;

function TValueSetForm1.IsValidClassDef: Boolean;
begin
  Result := not (IsEmptyStr(Trim(edExpressionName.Text)) OR IsEmptyStr(Trim(edDelphiName.Text)));
end;


procedure TValueSetForm1.edExpressionNameChange(Sender: TObject);
begin
  EnableNextBtn(IsValidClassDef);
  if fDelphiName_Locked then
    edDelphiName.Text := 'T' + edExpressionName.Text;
  edModelName.Text := LowerCase(edExpressionName.Text) ;
  if fUnitName_Locked then
    edUnitName.Text := edExpressionName.Text;
end;

procedure TValueSetForm1.ActionInsertExecute(Sender: TObject);
begin
  fEditMode := emInsert;
  InsertValue;
  EnableNextBtn(ListViewValues.Items.Count > 0);  
end;

procedure TValueSetForm1.ActionDeleteExecute(Sender: TObject);
begin
  if Assigned(ListViewValues.Selected) then
    if (Messagedlg(Format('Delete value "%s" ?',[listViewValues.Selected.Caption]), mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
    begin
      DeleteValue;
      EnableNextBtn(ListViewValues.Items.Count > 0);
    end;
end;

procedure TValueSetForm1.tsClassDefShow(Sender: TObject);
begin
  edExpressionName.SetFocus;
  tsClassDef.Parent.HelpContext := tsClassDef.HelpContext;
end;

procedure TValueSetForm1.ActionEditExecute(Sender: TObject);
begin
  if Assigned(ListViewValues.Selected) then
    EditValue;
end;

procedure TValueSetForm1.ListViewValuesDblClick(Sender: TObject);
begin
  ActionEditExecute(Sender);
end;

procedure TValueSetForm1.bbDownClick(Sender: TObject);
begin
  if ListViewValues.Items.Count > 1 then
    MoveDown;
end;

procedure TValueSetForm1.bbUpClick(Sender: TObject);
begin
  if ListViewValues.Items.Count > 1 then
    MoveUp;
end;

procedure TValueSetForm1.edDelphiNameChange(Sender: TObject);
begin
  fDelphiName_Locked := ('T' + Trim(edExpressionName.text) = Trim(edDelphiName.Text));
  EnableNextBtn(IsValidClassDef);
end;

procedure TValueSetForm1.FormDestroy(Sender: TObject);
begin
  if Assigned(UnitGenerator) then
    UnitGenerator := nil;
end;

procedure TValueSetForm1.edUnitNameChange(Sender: TObject);
begin
  fUnitName_Locked := (Trim(edExpressionName.text) = Trim(edUnitName.Text));
end;

procedure TValueSetForm1.tsValuesShow(Sender: TObject);
begin
  bbInsert.SetFocus;
  tsValues.Parent.HelpContext := tsValues.HelpContext;  
end;

end.
