unit FMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, BoldSubscription, BoldHandle, BoldClientHandles,
  BoldComClientHandles, BoldHandlesCom, BoldRootedHandlesCom,
  BoldAbstractListHandleCom, BoldCursorHandleCom, BoldListHandleCom,
  dmSystem;

type
  TForm2 = class(TForm)
    Label3: TLabel;
    lbValidate: TLabel;
    btnValidate: TButton;
    btnFetchAccounts: TButton;
    edAccountNumber: TEdit;
    blhcAccounts: TBoldListHandleCom;
    procedure btnFetchAccountsClick(Sender: TObject);
    procedure btnValidateClick(Sender: TObject);
    procedure edAccountNumberChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses AccountClasses_TLB, comobj;

{$R *.DFM}

procedure TForm2.btnFetchAccountsClick(Sender: TObject);
var
  i: integer;
  Valid: Boolean;
  Element: IAccount ;
begin
  Screen.Cursor := crHourGlass;
  Valid := false;
  if not blhcAccounts.Active then
    blhcAccounts.Active := true;
  blhcAccounts.First;
  for i:= 0 to blhcAccounts.Count - 1 do
  begin
    Element := blhcAccounts.List.Elements[i] as IAccount;
    if (Element).Number = WideString(trim(edAccountNumber.Text)) then
    begin
      Valid := true;
      Break;
    end;
  end;
  Screen.Cursor := crArrow;
  if Valid then
    lbValidate.Caption := 'Account Number is valid.'
  else
    lbValidate.Caption := 'Invalid Account Number.'
end;

procedure TForm2.btnValidateClick(Sender: TObject);
var
  obj: IUnknown;
  ValidatorService: IAccountValidator;
  Valid: WordBool;
begin
  if not DM.ClientObjectHandle.Active then
    Dm.ClientObjectHandle.Active := true;
  obj := DM.ClientObjectHandle.ComObject;
  olecheck(obj.QueryInterface(IID_IAccountValidator, ValidatorService));
  ValidatorService.Validate(WideString(Trim(edAccountNumber.Text)), Valid);
  if Valid then
    lbValidate.Caption := 'Account Number is valid.'
  else
    lbValidate.Caption := 'Invalid Account Number.'
end;

procedure TForm2.edAccountNumberChange(Sender: TObject);
begin
  lbValidate.Caption := 'Validate !!'
end;

end.
