
{ Global compiler directives }
{$include bold.inc}
unit BoldDbEvolutorForm;

interface

uses
  Classes,
  Controls,
  Forms,
  StdCtrls,
  ExtCtrls,
  ComCtrls,
  Dialogs,
  ImgList,
  BoldDefs,
  BoldLoghandler,
  BoldLogReceiverInterface,
  BoldAbstractPersistenceHandleDB;

type
  TfrmBoldDbEvolutor = class(TForm, IBoldLogReceiver)
    PageControl1: TPageControl;
    tsActions: TTabSheet;
    tsSQLScript: TTabSheet;
    mmoActions: TMemo;
    mmoSQLScript: TMemo;
    btnCancel: TButton;
    btnExecute: TButton;
    ProgressBar1: TProgressBar;
    tsMappingInfo: TTabSheet;
    mmoMappingInfoScript: TMemo;
    tsWarnings: TTabSheet;
    mmoWarnings: TMemo;
    ImageList1: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    fSQLScript: TStringList;
    fMappingInfoScript: TStringList;
    fWarnings: TStringList;
    fSessionName: String;
    procedure SqlScriptChange(Sender: TObject);
    procedure MappingInfoScriptChange(Sender: TObject);
    procedure SetProgress(const Value: integer);
    procedure SetLogHeader(const Value: string);
    procedure SetProgressMax(const Value: integer);
    procedure Clear;
    procedure Hide;
    procedure Log(const s: string; LogType: TBoldLogType = ltInfo);
    procedure ProgressStep;
    procedure Show;
    procedure Sync;
    procedure StartLog(const SessionName: string);
    procedure EndLog;
    procedure ProcessInterruption;
    function GetMappingInfoScript: TStrings;
    function GetSQLScript: TStrings;
    function GetWarnings: TStrings;
    procedure UpdateWarningCount(Sender: TObject);
  public
    constructor create(Owner: TComponent); override;
    destructor destroy; override;
    class procedure EvolveDB(PersistenceHandle: TBoldAbstractPersistenceHandleDB; GenerateGenericScript: Boolean);
    property SQLScript: TStrings read GetSQLScript;
    property MappingInfoScript: TStrings read GetMappingInfoScript;
    property Warnings: TStrings read GetWarnings;

    { Public declarations }
  end;

implementation

uses
  SysUtils,
  BoldUtils,
  BoldDbEvolutor,
  BoldGuard;

{$R *.DFM}

{ TfrmBoldDbEvolutor }

procedure TfrmBoldDbEvolutor.Clear;
begin
end;

procedure TfrmBoldDbEvolutor.EndLog;
begin
  Log(format('%s: Done %s', [formatDateTime('c', now), fSessionName]));
end;

procedure TfrmBoldDbEvolutor.Hide;
begin
  inherited Hide;
end;

procedure TfrmBoldDbEvolutor.Log(const s: string; LogType: TBoldLogType = ltInfo);
begin
  mmoActions.lines.Add(s);
  mmoActions.refresh;
  mmoSQLScript.Refresh;
end;

procedure TfrmBoldDbEvolutor.ProgressStep;
begin
  ProgressBar1.StepIt;
end;

procedure TfrmBoldDbEvolutor.SetProgress(const Value: integer);
begin
  ProgressBar1.Position := Value;
  ProgressBar1.Refresh;
end;

procedure TfrmBoldDbEvolutor.SetLogHeader(const Value: string);
begin
end;

procedure TfrmBoldDbEvolutor.SetProgressMax(const Value: integer);
begin
  ProgressBar1.Max := Value;
  ProgressBar1.Refresh;
end;

procedure TfrmBoldDbEvolutor.Show;
begin
  inherited Show;
end;

procedure TfrmBoldDbEvolutor.SqlScriptChange(Sender: TObject);
begin
  mmoSQLScript.Lines.Assign(Sender as TStrings);
end;

procedure TfrmBoldDbEvolutor.MappingInfoScriptChange(Sender: TObject);
begin
  mmoMappingInfoScript.Lines.Assign(Sender as TStrings);
end;

procedure TfrmBoldDbEvolutor.StartLog(const SessionName: String);
begin
  Log(format('%s: Starting %s', [formatDateTime('c', now), sessionName]));
  fSessionName := SessionName;
  Show;
end;

procedure TfrmBoldDbEvolutor.FormCreate(Sender: TObject);
var
  LogReceiver: IBoldLogReceiver;
begin
  GetInterface(IBoldLogReceiver, LogReceiver);
  BoldLog.RegisterLogReceiver(LogReceiver);
  PageControl1.ActivePage := tsActions;
  fSQLScript := TStringList.Create;
  fMappingInfoScript := TStringList.Create;
  fSQLScript.OnChange := SqlScriptChange;
  fMappingInfoScript.OnChange := MappingInfoScriptChange;
end;

procedure TfrmBoldDbEvolutor.FormDestroy(Sender: TObject);
var
  LogReceiver: IBoldLogReceiver;
begin
  GetInterface(IBoldLogReceiver, LogReceiver);
  BoldLog.UnRegisterLogReceiver(LogReceiver);
  FreeAndNil(fSQLScript);
  FreeAndNil(fMappingInfoScript);
end;

procedure TfrmBoldDbEvolutor.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

function TfrmBoldDbEvolutor.GetMappingInfoScript: TStrings;
begin
  result := fMappingInfoScript; //mmoMappingInfoScript.Lines;
end;

function TfrmBoldDbEvolutor.GetSQLScript: TStrings;
begin
  result := fSQLScript; //mmoSQLScript.Lines;
end;

function TfrmBoldDbEvolutor.GetWarnings: TStrings;
begin
  result := fWarnings;
end;

constructor TfrmBoldDbEvolutor.create(Owner: TComponent);
begin
  inherited;
  fWarnings := TStringList.Create;
  fWarnings.OnChange := UpdateWarningCount;
end;

destructor TfrmBoldDbEvolutor.destroy;
begin
  FreeAndNil(fWarnings);
  inherited;            
end;

procedure TfrmBoldDbEvolutor.UpdateWarningCount(Sender: TObject);
begin
  mmoWarnings.Lines.BeginUpdate;
  mmoWarnings.lines.Clear;
  mmoWarnings.lines.AddStrings(fWarnings);
  mmoWarnings.Lines.EndUpdate;
  tsWarnings.Caption := format('Warnings (%d)', [fWarnings.Count]);
end;

procedure TfrmBoldDbEvolutor.ProcessInterruption;
begin
end;

procedure TfrmBoldDbEvolutor.Sync;
begin
  Application.ProcessMessages;
end;

class procedure TfrmBoldDbEvolutor.EvolveDB(PersistenceHandle: TBoldAbstractPersistenceHandleDB; GenerateGenericScript: Boolean);
var
  Evolutor: TBoldDataBaseEvolutor;
  EvolutorForm: TfrmBoldDbEvolutor;
  Modalresult: Integer;
  ExecutedStatements: TStringList;
  i: integer;
  BoldGuard: IBoldGuard;
begin
  BoldGuard := TBoldGuard.Create(Evolutor, ExecutedStatements);
  Evolutor := TBoldDataBaseEvolutor.Create(PersistenceHandle);
  EvolutorForm := TfrmBoldDbEvolutor.Create(nil);
  EvolutorForm.Show;
  try
    BoldLog.StartLog('Detecting changes');
    Evolutor.GenericScript := GenerateGenericScript;
    try
      Evolutor.CalculateScript;
      Evolutor.GenerateScript(
        EvolutorForm.SQLScript,
        EvolutorForm.MappingInfoScript);
      Evolutor.GenerateWarnings(EvolutorForm.Warnings);
    except
      on e: Exception do
      begin
        showMessage('Failed to detect changes: ' + e.message);
        exit;
      end;
    end;

    BoldLog.EndLog;
    if EvolutorForm.SQLScript.Count = 0 then
    begin
      ShowMessage('No update needed!');
      EvolutorForm.Close;
      EvolutorForm := nil;
    end
    else
    begin
      ShowMessage(
        'Please inspect the planned actions and decide if you want to execute them' + BOLDCRLF +
        BOLDCRLF +
        'Make sure you back up your critical data before upgrading the database!!!');
      EvolutorForm.Hide;
      ModalResult := EvolutorForm.ShowModal;
      EvolutorForm := nil;
      if ModalResult = mrOK then
      begin
        try
          BoldLog.StartLog('Update database');
          Evolutor.ExecuteScript;
          BoldLog.EndLog;
          ShowMessage('Database successfully updated');
        except
          on e: Exception do
          begin
            showMessage('Failed to Evolve database: ' + e.message + BOLDCRLF + BOLDCRLF +
                        'See LogWindow for executed statements');

            ExecutedStatements := TStringList.Create;
            BoldLog.Separator;
            BoldLog.Log('The following statements were executed before evolution failed: ');
            BoldLog.Separator;
            Evolutor.GenerateExecutedStatements(ExecutedStatements);
            for i := 0 to ExecutedStatements.Count - 1 do
              BoldLog.Log(ExecutedStatements[i]);
            BoldLog.Separator;
            BoldLog.Show;
          end;
        end;
      end;
    end;
  finally
    if assigned(EvolutorForm) then
      EvolutorForm.Close;
  end;
end;

initialization
end.
