unit BoldQueryAliasDialog;

interface

uses
  Classes,
  Controls,
  Forms,
  StdCtrls,
  Buttons,
  BoldAliasUtils;

type
  TBoldQueryAliasDialog = class(TForm)
    GroupBox1: TGroupBox;
    ComboBoxAlias: TComboBox;
    BtnBrowse: TButton;
    BtnOK: TButton;
    BtnCancel: TButton;
    procedure BtnOKClick(Sender: TObject);
    procedure ComboBoxAliasChange(Sender: TObject);
    procedure BtnBrowseClick(Sender: TObject);
  private
    FAliasName: string;
  public
    function Execute: Boolean;
    property AliasName: string read FAliasName write FAliasName;
  end;

implementation

uses
  SysUtils,
  BoldUtils;

function TBoldQueryAliasDialog.Execute: Boolean;
begin
  TBoldAliasUtils.GetAliasNames(ComboBoxAlias.Items);
  ComboBoxAlias.Text := AliasName;
  ComboBoxAliasChange(nil);
  Result := ShowModal = mrOk;
end;

procedure TBoldQueryAliasDialog.BtnOKClick(Sender: TObject);
begin
  if TBoldAliasUtils.ValidateAlias(ComboBoxAlias) then
    AliasName := ComboBoxAlias.Text
  else
    ModalResult := mrNone;
end;

procedure TBoldQueryAliasDialog.ComboBoxAliasChange(Sender: TObject);
begin
  BtnOK.Enabled := Length(ComboBoxAlias.Text) > 0;
end;

procedure TBoldQueryAliasDialog.BtnBrowseClick(Sender: TObject);
begin
  TBoldAliasUtils.BrowseForPath(ComboBoxAlias);
end;

end.
