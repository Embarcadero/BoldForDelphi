
{ Global compiler directives }
{$include bold.inc}
unit BoldWAMainForm;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  ExtCtrls,
  BoldTemplateExpander,
  StdCtrls,
  Grids,
  dialogs,
  BoldDefsDT,
  BoldWAInterfaces,
  Buttons;

type
  TUserSelection = (usValueSetAttr, usSubClassAttr, usCustomAttr, usNone);

  TMainForm = class(TForm)
    pnBottom: TPanel;
    btnCancel: TButton;
    btnNext: TButton;
    Panel1: TPanel;
    GroupBox1: TGroupBox;
    rbValueSet: TRadioButton;
    rbSubClass: TRadioButton;
    rbCustom: TRadioButton;
    Button2: TButton;
    Image1: TImage;
    Bevel1: TBevel;
    Label1: TLabel;
    StaticText1: TStaticText;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    BitBtn1: TBitBtn;
    procedure btnCancelClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    fWizFormInterface: IWizardForm;
  public
    { Public declarations }
    fUnitGenerator: IUnitGenerator;
    ShowPreviousForm: Boolean;
    PreviousSelection: TUserSelection;
    function DisplayWizForm(CallIntf: IWizardForm; title: string): integer;
    procedure Initialize;
    procedure Next(Selection: TUserSelection);
    function GetWizFormIntf(Selection: TUserSelection): IWizardForm;
    function TUserSelectionToStr(Selection: TUserSelection): string;
  end;

var
  MainForm: TMainForm;

implementation

uses
  BoldUtils,
  BoldWAInputFormUnit,
  BoldWAClassInfo,
  BoldWACustomAttrForm1,
  BoldWASubClassForm1,
  BoldWAValueSetForm1,
  BoldWAdmTemplates;

var
  ValueSetForm: TValueSetForm1;
  SubClassForm: TSubClassForm1;
  CustomAttrForm: TCustomAttrForm1;

{$R *.dfm}

procedure TMainForm.Initialize;
begin
  ShowPreviousForm := false;
  PreviousSelection := usNone;
end;

procedure TMainForm.btnCancelClick(Sender: TObject);
begin
  if (MessageDlg('Exit the Bold Attribute Wizard?',mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
  begin
    Close;
  end;
end;

procedure TMainForm.btnNextClick(Sender: TObject);
var
  Selection: TUserSelection;
begin
  if rbValueSet.Checked then
    Selection := usValueSetAttr
  else if rbSubClass.Checked then
    Selection := usSubClassAttr
  else if rbCustom.Checked then
    Selection := usCustomAttr
  else
    Exit;
  Next(Selection);
end;

function TMainForm.TUserSelectionToStr(Selection: TUserSelection): string;
begin
  case Selection of
    usValueSetAttr: Result := 'Attribute derived from TBAValueSet';
    usSubClassAttr: Result := 'Subclass an existing Bold attribute';
    usCustomAttr: Result := 'Create a custom attribute';
  else
    Result := '';
  end;
end;

function TMainForm.DisplayWizForm(CallIntf: IWizardForm; title: string): integer;
begin
  InputForm.Caption := title;
  CallIntf.AssignParent(InputForm);
  CallIntf.UnitGenerator := fUnitGenerator;
  InputForm.WizFormIntf := CallIntf;
  if not CallIntf.SteppedBack then
    CallIntf.Initialize;
  InputForm.ShowModal;
  Result := InputForm.FormAction;
  CallIntf.ClearParent;
  ShowPreviousForm := (CallIntf.SteppedBack);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  HelpFile := ATTRIBUTEWIZARDHELPFILE;
  attrdatamodule := Tattrdatamodule.Create(MainForm);
  Initialize;
  InputForm := TInputForm.Create(MainForm);
end;

procedure TMainForm.Next(Selection: TUserSelection);
var
  Res: integer;
begin
  Hide;
  fWizFormInterface := GetWizFormIntf(Selection);
  res := DisplayWizForm(fWizFormInterface, TUserSelectionToStr(Selection));
  if res in [wfaFinish, wfaCancel] then
    Close
  else
    Show;
end;

function TMainForm.GetWizFormIntf(Selection: TUserSelection): IWizardForm;
begin
  case Selection of
    usValueSetAttr:
      begin
        if not ((PreviousSelection = Selection) and (ShowPreviousForm)) then
          ValueSetForm := TValueSetForm1.Create(MainForm);
        ValueSetForm.Visible := false;
        Result := IWizardForm(ValueSetForm);
      end;
    usSubClassAttr:
      begin
        if not ((PreviousSelection = Selection) and (ShowPreviousForm)) then
          SubClassForm := TSubClassForm1.Create(MainForm);
        SubClassForm.Visible := false;
        Result := IWizardForm(SubClassForm);
      end;
    usCustomAttr:
      begin
        if not ((PreviousSelection = Selection) and (ShowPreviousForm)) then
          CustomAttrForm := TCustomAttrForm1.Create(MainForm);
        CustomAttrForm.Visible := false;
        Result := IWizardForm(CustomAttrForm);
      end;
    else
      begin
        Result := nil;
      end;
  end;
  PreviousSelection := Selection;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if Assigned(fUnitGenerator) then
    fUnitGenerator := nil;
  if Assigned(fWizFormInterface) then
    fWizFormInterface := nil;
  FreeAndNil(attrdatamodule);
  FreeAndNil(InputForm);
end;

end.
