unit BoldAliasDialog;

interface

uses
  Classes,
  Controls,
  Forms,
  StdCtrls,
  Buttons,
  BoldDefs,
  BoldAliasUtils, ExtCtrls;

type
  TBoldAliasDialogMode =(admAlias, admCreateDb, admValidateDB, admEvolveDb, admEvolveDBAliasOK);

  TBoldAliasDialog = class(TForm)
    Label2: TLabel;
    pnlButtons: TPanel;
    BtnOK: TButton;
    BtnCancel: TButton;
    gbAlias: TGroupBox;
    cmbAlias: TComboBox;
    gbCreateStyle: TGroupBox;
    cmbCreateStyle: TComboBox;
    gbScriptMode: TGroupBox;
    cmbScriptMode: TComboBox;
    BtnBrowse: TButton;
    procedure BtnOKClick(Sender: TObject);
    procedure cmbAliasChange(Sender: TObject);
    procedure BtnBrowseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FAliasName: string;
    function GetGenerationMode: TBoldDataBaseGenerationMode;
    function GetUseTransactions: boolean;
    function GetGenericScript: Boolean;
 public
    function Execute: Boolean;
    procedure SetDialogMode(Mode: TBoldAliasDialogMode);
    property AliasName: string read FAliasName write FAliasName;
    property GenerationMode: TBoldDataBaseGenerationMode read GetGenerationMode;
    property UseTransactions: Boolean read GetUseTransactions;
    property GenericScript: Boolean read GetGenericScript;
  end;

implementation

uses
  SysUtils,
  BoldUtils;

{$R *.dfm}

function TBoldAliasDialog.Execute: Boolean;
begin
  TBoldAliasUtils.GetAliasNames(cmbAlias.Items);
  cmbAlias.Text := AliasName;
  cmbAliasChange(nil);
  Result := ShowModal = mrOk;
end;

function TBoldAliasDialog.GetGenerationMode: TBoldDataBaseGenerationMode;
begin
  if cmbCreateStyle.ItemIndex = 2 then
    result := dbgQuery
  else
    result := dbgTable;
end;

function TBoldAliasDialog.GetUseTransactions: boolean;
begin
  result := cmbCreateStyle.ItemIndex = 0;
end;

procedure TBoldAliasDialog.BtnOKClick(Sender: TObject);
begin
  if not gbAlias.Visible then
    AliasName := ''
  else if TBoldAliasUtils.ValidateAlias(cmbAlias) then
    AliasName := cmbAlias.Text
  else
    ModalResult := mrNone;
end;

procedure TBoldAliasDialog.cmbAliasChange(Sender: TObject);
begin
  BtnOK.Enabled := not gbAlias.Visible or (Length(cmbAlias.Text) > 0);
end;

procedure TBoldAliasDialog.BtnBrowseClick(Sender: TObject);
begin
  TBoldAliasUtils.BrowseForPath(cmbAlias);
end;

procedure TBoldAliasDialog.FormCreate(Sender: TObject);
begin
  cmbCreateStyle.ItemIndex := 2;
  cmbScriptMode.ItemIndex := 0;
end;

function TBoldAliasDialog.GetGenericScript: Boolean;
begin
  result := cmbScriptMode.ItemIndex = 1;
end;

procedure TBoldAliasDialog.SetDialogMode(Mode: TBoldAliasDialogMode);
var
  h: integer;
  i: integer;
begin
  if mode <> admCreateDB then
    gbCreateStyle.Visible := false;
  if not (mode in [admEvolveDB, admEvolveDBAliasOK]) then
    gbScriptMode.Visible := false;

  if mode=admEvolveDbAliasOK then
    gbAlias.Visible := false;

  case mode of
    admAlias: Caption := 'Select alias';
    admCreateDb: Caption := 'Create database';
    admValidateDB: Caption := 'Validate database';
    admEvolveDb,
    admevolveDBAliasOK: Caption := 'Evolve Database';
  end;
  h := 0;
  for i := 0 to ComponentCount-1 do
    if (Components[i] is TCustomControl) and (Components[i] as TCustomControl).Visible then
      h := h + (Components[i] as TCustomControl).Height;
  ClientHeight := h;

end;

end.
