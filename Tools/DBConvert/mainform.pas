unit mainform;

interface

uses
  SysUtils,
  Classes,
  Controls,
  Forms,
  StdCtrls,
  DBTables,
  Db;

type
  TfrmMain = class(TForm)
    SourceDB: TDatabase;
    TargetDB: TDatabase;
    cmdConvert: TButton;
    cmdClose: TButton;
    gbxSource: TGroupBox;
    cmbSource: TComboBox;
    SourceSystemPrefix: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    gbxTarget: TGroupBox;
    cmbTarget: TComboBox;
    TargetSystemPrefix: TEdit;
    Label5: TLabel;
    Label6: TLabel;
    gbxSettings: TGroupBox;
    Label1: TLabel;
    cmbLogLevel: TComboBox;
    Label2: TLabel;
    cmbTargetBoldVersion: TComboBox;
    OnlyLog: TCheckBox;
    procedure cmdConvertClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cmdCloseClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses DBConverter;

{$R *.DFM}

procedure TfrmMain.cmdConvertClick(Sender: TObject);
begin
  if cmbSource.Text = '' then
    raise Exception.Create('Missing alias for source DB');
  if cmbTarget.Text = '' then
    raise Exception.Create('Missing alias for target DB');

  if cmbSource.Text = cmbTarget.Text then
    raise Exception.Create( 'Converting from one db to the same is not such a good idea...' );

  SourceDB.AliasName := cmbSource.Text;
  TargetDB.AliasName := cmbTarget.Text;
  try
    with TBoldDbCopyConverter.Create(SourceDB, TargetDB) do
    try
      // If TBoldBatchMover has been subclassed, this is a good place to inform the converter:
      // MoverClass := TMyBatchMover
      PerformCopy( OnlyLog.Checked, self.SourceSystemPrefix.Text, Self.TargetSystemPrefix.Text, cmbLogLevel.ItemIndex, cmbTargetBoldVersion.ItemIndex = 1 );
    finally
      Free;
    end;
  finally
    SourceDB.Close;
    TargetDB.Close;
  end;

  BringToFront;
  cmdClose.SetFocus;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Session.GetAliasNames(cmbSource.Items);
  Session.GetAliasNames(cmbTarget.Items);
  cmbLogLevel.ItemIndex := 0;
  cmbTargetBoldVersion.ItemIndex := 1;
end;

procedure TfrmMain.cmdCloseClick(Sender: TObject);
begin
  Close;
end;

end.
