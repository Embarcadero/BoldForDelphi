unit BoldEvolutionAliasDialog;

interface

uses
  Classes,
  Controls,
  Forms,
  StdCtrls,
  Buttons,
  BoldDefs,
  DBTables,
  ExtCtrls,
  BoldAliasUtils;

type
  TBoldEvolutionAliasDialog = class(TForm)
    pnlMode: TPanel;
    cmbScriptMode: TComboBox;
    Label2: TLabel;
    pnlAlias: TPanel;
    Label1: TLabel;
    ComboBoxAlias: TComboBox;
    pnlButtons: TPanel;
    BtnOK: TButton;
    BtnCancel: TButton;
    procedure BtnOKClick(Sender: TObject);
    procedure ComboBoxAliasChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FAliasName: string;
    function GetGenericScript: Boolean;
  public
    function Execute: Boolean;
    procedure HideAlias;
    property AliasName: string read FAliasName write FAliasName;
    property GenericScript: Boolean read GetGenericScript;
  end;

implementation

uses
  SysUtils,
  BoldUtils;

{$R *.dfm}

function TBoldEvolutionAliasDialog.Execute: Boolean;
begin
  TBoldAliasUtils.GetAliasNames(ComboBoxAlias.Items);
  ComboBoxAlias.Text := AliasName;
  ComboBoxAliasChange(nil);
  Result := ShowModal = mrOk;
end;

procedure TBoldEvolutionAliasDialog.BtnOKClick(Sender: TObject);
begin
  if not pnlAlias.Visible or (pnlAlias.Visible and TBoldAliasUtils.ValidateAlias(ComboBoxAlias)) then
    AliasName := ComboBoxAlias.Text
  else
    ModalResult := mrNone;
end;

procedure TBoldEvolutionAliasDialog.ComboBoxAliasChange(Sender: TObject);
begin
  BtnOK.Enabled := not pnlAlias.visible or (Length(ComboBoxAlias.Text) > 0);
end;

procedure TBoldEvolutionAliasDialog.FormCreate(Sender: TObject);
begin
  cmbScriptMode.ItemIndex := 0;
end;

function TBoldEvolutionAliasDialog.GetGenericScript: Boolean;
begin
  result := cmbScriptMode.ItemIndex = 1;
end;

procedure TBoldEvolutionAliasDialog.HideAlias;
begin
  pnlAlias.visible := false;
end;

end.
