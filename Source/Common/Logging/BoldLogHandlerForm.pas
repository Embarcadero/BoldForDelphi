unit BoldLogHandlerForm;

interface

uses
  Forms,
  Classes,
  BoldUtils,
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
    fLogFormNotifier: TBoldPassthroughNotifier;
    fSessionName: String;
    function GetLogForm: TBoldLogForm;
    procedure HideProgressBar;
    function GetLogFormNotifier: TBoldPassthroughNotifier;
  protected
    property LogForm: TBoldLogForm read GetLogForm;
    property LogFormNotifier: TBoldPassthroughNotifier read GetLogFormNotifier;
    procedure SetProgress(const Value: integer);
    procedure SetLogHeader(const Value: string);
    procedure SetProgressMax(const Value: integer);
    procedure ProcessInterruption;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
  public
    destructor Destroy; override;
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
  BoldCommonConst;

var
  LogHandlerForm: TBoldLogHandlerReceiver;

{ TBoldLogHandlerReceiver }

procedure TBoldLogHandlerReceiver.Clear;
begin
  LogForm.Clear;
end;

destructor TBoldLogHandlerReceiver.Destroy;
begin
  FreeAndNil(fLogForm);
  FreeAndNil(fLogFormNotifier);
  inherited;
end;

procedure TBoldLogHandlerReceiver.EndLog;
begin
  Log(format(sLogDone, [formatDateTime('c', now), fSessionName]));
  Hideprogressbar;
end;

function TBoldLogHandlerReceiver.GetLogForm: TBoldLogForm;
begin
  if not Assigned(fLogForm) then
  begin
    fLogForm := TBoldLogForm.Create(nil);
    fLogForm.FreeNotification(LogFormNotifier);
  end;
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
  LogForm.AddLog('==================================');
  LogForm.AddLog('==='+Value);
  LogForm.AddLog('==================================');
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
  LogForm.Caption := Format(sLogFormCaption, [SessionName]);
  fSessionName := SessionName;
  Log(format(sLogStarting, [formatDateTime('c', now), SessionName])); // do not localize
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

procedure TBoldLogHandlerReceiver.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (AComponent = fLogForm) and (Operation = opRemove) then
    fLogForm := nil;
end;

function TBoldLogHandlerReceiver.GetLogFormNotifier: TBoldPassthroughNotifier;
begin
  if not assigned(fLogFormNotifier) then
    fLogFormNotifier := TBoldPassthroughNotifier.CreateWithEvent(Notification);
  result := fLogFormNotifier;
end;

initialization
  LogHandlerForm := TBoldLogHandlerReceiver.Create;
  BoldLog.RegisterLogReceiver(LogHandlerForm as IBoldLogReceiver);

finalization
  BoldLog.UnregisterLogReceiver(LogHandlerForm as IBoldLogReceiver);

end.
