
{ Global compiler directives }
{$include bold.inc}
unit BoldLogHandlerForm;

interface

uses
  Forms,
  BoldDefs,
  BoldLogHandler,
  BoldLogReceiverInterface,
  BoldLogForm;

type
  { forward declarations }
  TBoldLogHandlerReceiver = class;

  { TBoldLogHandlerReceiver }
  TBoldLogHandlerReceiver = class(TInterfacedObject, IBoldLogReceiver)
  private
    fLogForm: TBoldLogForm;
    fSessionName: String;
    function GetLogForm: TBoldLogForm;
    procedure HideProgressBar;
  protected
    property LogForm: TBoldLogForm read GetLogForm;
    procedure SetProgress(const Value: integer);
    procedure SetLogHeader(const Value: string);
    procedure SetProgressMax(const Value: integer);
    procedure ProcessInterruption;
  public
    destructor destroy; override;
    procedure Clear;
    procedure Hide;
    procedure Log(const s: string; LogType: TBoldLogType = ltInfo);
    procedure ProgressStep;
    procedure Show;
    procedure Sync;
    procedure StartLog(const SessionName: String);
    procedure EndLog;
    procedure SaveLog;
  end;

implementation

uses
  SysUtils,
  BoldUtils,
  BoldRev;

var
  LogHandlerForm: TBoldLogHandlerReceiver;

{ TBoldLogHandlerReceiver }

procedure TBoldLogHandlerReceiver.Clear;
begin
  LogForm.Clear;
end;

destructor TBoldLogHandlerReceiver.destroy;
begin
  FreeAndNil(fLogForm);
  inherited;
end;

procedure TBoldLogHandlerReceiver.EndLog;
begin
  Log(format('%s: Done %s', [formatDateTime('c', now), fSessionName]));
  Hideprogressbar;
end;

function TBoldLogHandlerReceiver.GetLogForm: TBoldLogForm;
begin
  if not Assigned(fLogForm) then
    fLogForm := TBoldLogForm.Create(nil);
  Result := fLogForm;
end;

procedure TBoldLogHandlerReceiver.Hide;
begin
  LogForm.Hide;
end;

procedure TBoldLogHandlerReceiver.Log(const s: string; LogType: TBoldLogType = ltInfo);
begin
  LogForm.AddLog(s);
  LogForm.Refresh;
end;

procedure TBoldLogHandlerReceiver.ProgressStep;
begin
  LogForm.ProgressBar1.StepIt;
end;

procedure TBoldLogHandlerReceiver.SaveLog;
begin
  LogForm.SaveLog;
end;

procedure TBoldLogHandlerReceiver.SetProgress(const Value: integer);
begin
  LogForm.ProgressBar1.Position := Value;
end;

procedure TBoldLogHandlerReceiver.SetLogHeader(const Value: string);
begin
  LogForm.AddLog(Value);
end;

procedure TBoldLogHandlerReceiver.SetProgressMax(const Value: integer);
begin
  if Value > 0 then
    LogForm.Progressbar1.Max := Value;
end;

procedure TBoldLogHandlerReceiver.Show;
begin
  LogForm.Show;
  LogForm.UpdateView;
end;

procedure TBoldLogHandlerReceiver.StartLog(const SessionName: String);
begin
  LogForm.Caption := 'Logging Activity: ' + SessionName;
  fSessionName := SessionName;
  Log(format('%s: Starting %s', [formatDateTime('c', now), SessionName]));
end;

procedure TBoldLogHandlerReceiver.ProcessInterruption;
begin
  Application.ProcessMessages;
end;

procedure TBoldLogHandlerReceiver.HideProgressBar;
begin
  LogForm.ProgressBar1.Position := 0;
end;

procedure TBoldLogHandlerReceiver.Sync;
begin
  Application.ProcessMessages;
end;

initialization

  LogHandlerForm := TBoldLogHandlerReceiver.Create;
  BoldLog.RegisterLogReceiver(LogHandlerForm as IBoldLogReceiver);

finalization
  BoldLog.UnregisterLogReceiver(LogHandlerForm as IBoldLogReceiver);

end.
