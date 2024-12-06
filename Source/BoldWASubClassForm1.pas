
{ Global compiler directives }
{$include bold.inc}
unit BoldWASubClassForm1;

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
  ExtCtrls,
  BoldWACustomAttr,
  BoldWCodeInformer,
  BoldWAInterfaces;

type
  TSubClassForm1 = class(TForm, IWizardForm)
    PageControl1: TPageControl;
    tsClassDef: TTabSheet;
    Panel1: TPanel;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label3: TLabel;
    Label6: TLabel;
    Label4: TLabel;
    Label7: TLabel;
    Label5: TLabel;
    Label2: TLabel;
    cbParent: TComboBox;
    edExpressionname: TEdit;
    edDelphiName: TEdit;
    edUnitname: TEdit;
    edModelName: TEdit;
    tsOverride: TTabSheet;
    GroupBox2: TGroupBox;
    ListViewOverride: TListView;
    MemoBoldClasses: TMemo;
    procedure cbParentChange(Sender: TObject);
    procedure edExpressionnameChange(Sender: TObject);
    procedure tsClassDefShow(Sender: TObject);
    procedure tsOverrideShow(Sender: TObject);
    procedure edDelphiNameChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure edUnitnameChange(Sender: TObject);
  private
    { Private declarations }
    fSteppedBack: Boolean;
    fUnitGenerator: IUnitGenerator;
    fBoldClassParser: TClassParser;
    fEnableNext: TEnableNextEvent;
    fUnitName_Locked,
    fDelphiName_Locked: Boolean;
    function getMethodCount: Integer;
    function GetSteppedBack: Boolean;
    procedure GetNewAttribute(var NewAttribute: TCustomAttribute);
    procedure SetEnableNext(Value: TEnableNextEvent);
    procedure EnableNextBtn(const Enable: Boolean);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure AssignParent(aParent: TWinControl);
    procedure ClearParent;
    procedure Initialize;
    function Next: integer;
    function Back: integer;
    procedure Cancel;
    procedure Finish;
    procedure setUnitGeneratorIntf(Intf: IUnitGenerator);
    function getUnitGeneratorIntf: IUnitGenerator;
    function IsValidClassDef: Boolean;
    property MethodCount: Integer read getMethodCount;
    property UnitGenerator: IUnitGenerator read getUnitGeneratorIntf write setUnitGeneratorIntf;
    property SteppedBack: Boolean read GetSteppedBack default false;
    property EnableNext: TEnableNextEvent write setEnableNext;
  end;

var
  SubClassForm1: TSubClassForm1;

implementation

uses
  SysUtils,
  BoldUtils,
  BoldWAdmTemplates,
  BoldWAMethodInfo,
  BoldWAClassInfo,
  BoldVclUtils;

{$R *.dfm}
constructor TSubClassForm1.create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  tsClassDef.TabVisible := false;
  tsOverride.TabVisible := false;
  fUnitName_Locked := true;
  fDelphiName_Locked := true;
end;

procedure TSubClassForm1.Initialize;
var
  i: Integer;
  Item: TClassInfo;
begin
  Align := alClient;
  fBoldClassParser := TClassParser.Create(TStringStream.Create(MemoBoldClasses.Lines.Text));
  fBoldClassParser.Start;
  i:= 0;
  while fBoldClassParser.getClasses(Item, i) do
    if Assigned(Item) then cbParent.Items.Add(Item.DelphiName);
  PageControl1.ActivePage := tsClassDef;
end;

function TSubClassForm1.Back: integer;
begin
  if (PageControl1.ActivePage = tsOverride) then
  begin
    Result := wfaPrevious;
    PageControl1.ActivePage := tsClassDef;
    tsClassDef.Visible := true;
    Exit;
  end;
  fSteppedBack := true;
  Result := wfaBack;
end;

procedure TSubClassForm1.Cancel;
begin
  fSteppedBack := false;
end;

procedure TSubClassForm1.setEnableNext(Value: TEnableNextEvent);
begin
  fEnableNext := Value;
end;

procedure TSubClassForm1.EnableNextBtn(const Enable: Boolean);
begin
  if Assigned(fEnablenext) then
    fEnableNext(Enable);
end;

procedure TSubClassForm1.setUnitGeneratorIntf(Intf: IUnitGenerator);
begin
  fUnitGenerator := Intf;
end;

function TSubClassForm1.getUnitGeneratorIntf: IUnitGenerator;
begin
  Result := fUnitGenerator;
end;

function TSubClassForm1.GetSteppedBack: Boolean;
begin
  Result := fSteppedBack;
end;

procedure TSubClassForm1.cbParentChange(Sender: TObject);
var
  ClassName: string;
  i: integer;
  CurrentClass: TClassInfo;
  Item: TMethodInfo;
  ListItem: TListItem;
begin
  EnableNextBtn(IsValidClassDef);
  ClassName := cbParent.Text;
  if IsEmptyStr(Trim(ClassName)) then Exit;
  i:= 0;
  CurrentClass := fBoldClassParser.getClassbyName(ClassName);
  ListViewOverride.Items.BeginUpdate;
  ListViewOverride.Items.Clear;
  while CurrentClass.getMethods(stAll,Item,i)  do
    if Assigned(Item) then
    begin
      ListItem := ListViewOverride.Items.Add;
      ListItem.Caption := Item.methodTypeAsString + ' ' +item.Name + item.Params;
      if (Item.methodType = mtFunction) then
        ListItem.Caption := Listitem.Caption + ': '+ item.ReturnType;
    end;
  ListViewOverride.Items.EndUpdate;
end;

function TSubClassForm1.getMethodCount: Integer;
var i, count: integer;
begin
  Count := 0;
  for i:= 0 to ListViewOverride.Items.Count - 1 do
    if ListViewOverride.Items[i].Checked then Inc(Count);
  Result := Count;
end;

procedure TSubClassForm1.AssignParent(aParent: TWinControl);
begin
  PageControl1.Parent := aParent;
  if (aParent is TForm) then
    (aParent as TForm).helpcontext := helpcontext;
end;

procedure TSubClassForm1.ClearParent;
begin
  PageControl1.Parent := self;
end;

function TSubClassForm1.Next: integer;
begin
  if PageControl1.ActivePage = tsClassDef then
  begin
    Result := wfaNext;
    if IsValidClassDef then
    begin
      PageControl1.ActivePage := tsOverride;
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

procedure TSubClassForm1.Finish;
var
  NewAttribute: TCustomAttribute;
begin
  NewAttribute := TCustomAttribute.Create;
  try
    getNewAttribute(NewAttribute);
    with attrdatamodule.SubClassedAttrTemplate do
    begin
      Variables.Add('UNITNAME', NewAttribute.UnitName, []);
      Variables.Add('EXPRESSIONNAME', NewAttribute.ExpressionName, []);
      Variables.Add('DELPHINAME', NewAttribute.DelphiName, []);
      Variables.Add('SUPERCLASS', NewAttribute.Parent, []);
      Variables.Add('METHODCOUNT', InttoStr(NewAttribute.Methods.Count), []);
    end;
    NewAttribute.AssignMethodsToTemplate(attrdatamodule.SubClassedAttrTemplate);
    UnitGenerator.GenerateUnit(NewAttribute.UnitName, attrdatamodule.SubClassedAttrTemplate);
  finally
    FreeAndNil(NewAttribute);
  end;
end;

function TSubClassForm1.IsValidClassDef: Boolean;
begin
  Result := not (IsEmptyStr(trim(cbParent.Text)) OR IsEmptyStr(Trim(edExpressionName.Text)) OR IsEmptyStr(edDelphiName.Text));
end;

procedure TSubClassForm1.edExpressionnameChange(Sender: TObject);
begin
  EnableNextBtn(IsValidClassDef);
  if fDelphiName_Locked then
    edDelphiName.Text := 'T' + edExpressionName.Text;
  edModelName.Text := LowerCase(edExpressionName.Text) ;
  if fUnitName_Locked then
    edUnitName.Text := edExpressionName.Text;
end;

procedure TSubClassForm1.tsClassDefShow(Sender: TObject);
begin
  cbParent.SetFocus;
  tsClassDef.Parent.HelpContext := tsClassDef.HelpContext;
end;

procedure TSubClassForm1.tsOverrideShow(Sender: TObject);
begin
  ListViewOverride.SetFocus;
  tsOverride.Parent.HelpContext := tsOverride.HelpContext;
end;

procedure TSubClassForm1.GetNewAttribute(var NewAttribute: TCustomAttribute);
var
  NewMethod: TMethodInfo;
  CurrentClass: TClassInfo;
  i: Integer;
begin
  with NewAttribute do
  begin
    ExpressionName := Trim(edExpressionName.Text);
    UnitName := trim(edUnitname.Text);
    DelphiName := Trim(edDelphiName.Text);
    Parent := Trim(cbParent.Text);
    Methods.Clear;
    CurrentClass := fBoldClassParser.getClassbyName(Parent);
    for i:= 0 to ListViewOverride.Items.Count - 1 do
      if ListViewOverride.Items[i].Checked then
      begin
        NewMethod := TMethodInfo.Create;
        NewMethod.MethodType :=  CurrentClass.Methods[i].MethodType;
        NewMethod.Name := CurrentClass.Methods[i].Name;
        NewMethod.Params := CurrentClass.Methods[i].Params ;
        NewMethod.ReturnType := CurrentClass.Methods[i].ReturnType ;
        NewMethod.Visibility := CurrentClass.Methods[i].Visibility ;
        NewMethod.mDirectives := [mdOverride];
        Methods.Add(NewMethod);
      end;
  end;
end;

procedure TSubClassForm1.edDelphiNameChange(Sender: TObject);
begin
  fDelphiName_Locked := ('T' + Trim(edExpressionName.text) = Trim(edDelphiName.Text));
  EnableNextBtn(IsValidClassDef);
end;

procedure TSubClassForm1.FormDestroy(Sender: TObject);
begin
  if Assigned(UnitGenerator) then
    UnitGenerator := nil;
end;

procedure TSubClassForm1.edUnitnameChange(Sender: TObject);
begin
  fUnitName_Locked := (Trim(edExpressionName.text) = Trim(edUnitName.Text));
end;

end.
