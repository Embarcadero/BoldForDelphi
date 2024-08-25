
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
  BoldAbstractPersistenceHandleDB,
  System.ImageList;

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
    SaveDialog1: TSaveDialog;
    btnSaveScript: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnSaveScriptClick(Sender: TObject);
  private
    { Private declarations }
    fSQLScript: TStringList;
    fMappingInfoScript: TStringList;
    fWarnings: TStringList;
    fSessionName: String;
    FEnabled: Boolean;
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
    procedure SetEnabled(const Value: Boolean);
    function GetEnabled: Boolean;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    class procedure EvolveDB(PersistenceHandle: TBoldAbstractPersistenceHandleDB; GenerateGenericScript: Boolean);
    property SQLScript: TStrings read GetSQLScript;
    property MappingInfoScript: TStrings read GetMappingInfoScript;
    property Warnings: TStrings read GetWarnings;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    { Public declarations }
  end;

implementation

uses
  SysUtils,

  BoldCoreConsts,
  BoldUtils,
  BoldDbEvolutor,
  BoldIsoDateTime,
  BoldGuard;

{$R *.DFM}

{ TfrmBoldDbEvolutor }

procedure TfrmBoldDbEvolutor.btnSaveScriptClick(Sender: TObject);
var
  sl: TStringList;
begin
  SaveDialog1.FileName := AsIsoDateTime(now);
  if SaveDialog1.Execute then
  begin
    sl := TStringList.Create;
    try
      sl.Add('-- Mapping Info Script');
      sl.AddStrings(MappingInfoScript);
      sl.Add('-- SQL Script');
      sl.AddStrings(SQLScript);
      SQLScript.SaveToFile(SaveDialog1.FileName);
    finally
      sl.free;
    end;
  end;
end;

procedure TfrmBoldDbEvolutor.Clear;
begin
end;

procedure TfrmBoldDbEvolutor.EndLog;
begin
  Log(format(sDone, [AsIsoDateTime(now), fSessionName])); // do not localize
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

procedure TfrmBoldDbEvolutor.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
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
  Log(format(sStarting, [AsIsoDateTime(now), sessionName]));
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

function TfrmBoldDbEvolutor.GetEnabled: Boolean;
begin
  Result := FEnabled;
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

constructor TfrmBoldDbEvolutor.Create(Owner: TComponent);
begin
  inherited;
  fWarnings := TStringList.Create;
  fWarnings.OnChange := UpdateWarningCount;
end;

destructor TfrmBoldDbEvolutor.Destroy;
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
  tsWarnings.Caption := format(sWarnings, [fWarnings.Count]);
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
    BoldLog.StartLog(sDetectingChanges);
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
        showMessage(Format(sFailedToDetectChanges, [e.message]));
        exit;
      end;
    end;

    BoldLog.EndLog;
    if EvolutorForm.SQLScript.Count = 0 then
    begin
      ShowMessage(sNoUpdateNeeded);
      EvolutorForm.Close;
      EvolutorForm := nil;
    end
    else
    begin
      ShowMessage(Format(sInspectChanges, [BOLDCRLF, BOLDCRLF]));
      EvolutorForm.Hide;
      ModalResult := EvolutorForm.ShowModal;
      EvolutorForm := nil;
      if ModalResult = mrOK then
      begin
        try
          BoldLog.StartLog(sUpdateDatabase);
          Evolutor.ExecuteScript;
          BoldLog.EndLog;
          ShowMessage(sUpdateSuccessful);
        except
          on e: Exception do
          begin
            ShowMessage(Format(sEvolveFailed, [e.Message, BOLDCRLF, BOLDCRLF]));

            ExecutedStatements := TStringList.Create;
            BoldLog.Separator;
            BoldLog.Log(sExecutedStatements);
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

end.
