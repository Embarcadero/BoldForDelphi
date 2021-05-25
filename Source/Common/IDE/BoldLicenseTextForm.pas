
{ Global compiler directives }
{$include bold.inc}
unit BoldLicenseTextForm;

interface

uses
  Windows, Classes, Controls, Forms, 
  BoldRegistry,
  Dialogs,
  StdCtrls, ExtCtrls;

type
  TfrmLicenseText = class(TForm)
    Panel1: TPanel;
    rbAgree: TRadioButton;
    rbNotAgree: TRadioButton;
    mmoLicenseTextTemplate: TMemo;
    Button1: TButton;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    fProduct: String;
  public
    { Public declarations }
    procedure InitializeAndShow(License: String; Product: string);
  end;

var
  frmLicenseText: TfrmLicenseText;

implementation

{$R *.DFM}

uses
  SysUtils,
  BoldUtils;


procedure TfrmLicenseText.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  var
    Reg: TBoldRegistry;
begin
  CanClose := rbAgree.Checked or rbNotAgree.Checked;
  if not CanClose then
    ShowMessage('You must first select either "I Agree" or "I do not Agree"')
  else
  begin
    reg := TBoldRegistry.Create;
    if Reg.OpenKey('\License_agreed') then
      reg.WriteBool(fProduct, rbAgree.Checked);
    reg.free;
  end;
end;

procedure TfrmLicenseText.InitializeAndShow(License: String; Product: string);
begin
  Caption := '';
  fProduct := Product;
  mmoLicenseTextTemplate.lines.CommaText := License;
  ShowModal;
end;

procedure TfrmLicenseText.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;


end.
