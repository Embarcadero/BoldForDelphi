
{ Global compiler directives }
{$include bold.inc}
unit BoldUMLAddTV;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, BoldUMLModel;

type
  TfrmBoldUMLAddTV = class(TForm)
    GroupBox1: TGroupBox;
    btOK: TButton;
    btCancel: TButton;
    Label1: TLabel;
    Label2: TLabel;
    edTool: TEdit;
    edValue: TEdit;
    Label3: TLabel;
    edName: TEdit;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
  public
  end;

  procedure ShowAddTVDialog(SuggestedTool: String; Owner: TUMLModelElement);

var
  frmBoldUMLAddTV: TfrmBoldUMLAddTV;

implementation
var
  Tool, TagName, Value: String;
{$R *.dfm}

procedure ShowAddTVDialog(SuggestedTool: String; Owner: TUMLModelElement);
var
  DialogResult: TModalResult;
  Tag: String;
begin
  frmBoldUMLAddTV := TfrmBoldUMLAddTV.Create(nil);
  frmBoldUMLAddTV.edTool.Text := SuggestedTool;
  DialogResult := frmBoldUMLAddTV.ShowModal;
  if mrOK = DialogResult then
  begin
    if Tool = '<Default>' then
      Tool := '';
    if Tool <> '' then
      TagName := '.' + TagName;
    Tag := Tool + TagName;
    Owner.SetTaggedValue(Tag, Value);
  end;
end;

procedure TfrmBoldUMLAddTV.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Tool := edTool.Text;
  TagName := edName.Text;
  Value := edValue.Text;
  Action := caFree;
end;

procedure TfrmBoldUMLAddTV.FormShow(Sender: TObject);
begin
  edName.SetFocus;
end;

end.
