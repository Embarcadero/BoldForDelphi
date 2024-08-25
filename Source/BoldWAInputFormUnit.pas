
{ Global compiler directives }
{$include bold.inc}
unit BoldWAInputFormUnit;

interface

uses
  Windows,
  Messages,
  Classes,
  Graphics,
  Controls,                                                            
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  ComCtrls,
  BoldWAInterfaces, Buttons;

type
  TInputForm = class(TForm)
    pnBottom: TPanel;
    btnCancel: TButton;
    btnNext: TButton;
    btnBack: TButton;
    pnLeft: TPanel;
    Image1: TImage;
    BitBtn1: TBitBtn;
    procedure btnCancelClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnBackClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
    fWizFormIntf: IWizardForm;
    fFormAction: integer;
    procedure EnableNextBtn(const Enable: Boolean);
    procedure assignWizFormIntf(value: IWizardForm);
  public
    { Public declarations }
    property WizFormIntf: IWizardForm read fWizFormIntf write assignWizFormIntf;
    property FormAction: integer read fFormAction write fFormAction;
  end;

var
  InputForm: TInputForm;

implementation

uses
  SysUtils,
  BoldUtils;

{$R *.dfm}

procedure TInputForm.btnCancelClick(Sender: TObject);
begin
  if (MessageDlg('Exit the Bold Attribute Wizard?',mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
  begin
    WizFormIntf.Cancel;
    FormAction := wfaCancel;
    Close;
  end;
end;

procedure TInputForm.btnNextClick(Sender: TObject);
begin
  FormAction := WizFormIntf.Next;
  if formAction = wfaLast then
    btnNext.Caption := '&Finish >'
  else if formAction = wfafinish then
  begin
    WizFormIntf.Finish;
    Close;
  end;
end;

procedure TInputForm.btnBackClick(Sender: TObject);
begin
  FormAction := WizFormIntf.Back;
  if FormAction = wfaBack then
    Close
  else if FormAction = wfaPrevious then
    btnNext.Caption := '&Next >';
end;

procedure TInputForm.EnableNextBtn(const Enable: Boolean);
begin
  btnNext.Enabled := Enable;
end;

procedure TInputForm.assignWizFormIntf(Value: IWizardForm);
begin
  fWizFormIntf := value;
  fWizFormIntf.EnableNext := EnableNextBtn;
  if not fWizFormIntf.SteppedBack then
    btnNext.Enabled := false;
end;

procedure TInputForm.FormDestroy(Sender: TObject);
begin
  if Assigned(fWizFormIntf) then
    fWizFormIntf := nil;
end;

procedure TInputForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if not (FormAction in [wfaBack, wfaFinish, wfaCancel]) then
    btnCancelClick(Sender)
  else
    CanClose := true;
end;

end.
