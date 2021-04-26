
{ Global compiler directives }
{$include bold.inc}
unit BoldSmallLogFrame;

interface

uses
  Graphics,
  Classes,
  Controls,
  Forms,
  ComCtrls,
  StdCtrls,
  ExtCtrls,
  BoldLogReceiverInterface,
  BoldLogHandler,
  BoldDefs;

type
  TBoldLogFrame = class(TFrame, IBoldLogReceiver)
    pgLog: TProgressBar;
    WarningPanel: TPanel;
    WarningIndicator: TShape;
    InfoPanel: TPanel;
    lblLogText: TLabel;
    lblLogMainHeader: TLabel;
    Timer1: TTimer;
    Panel1: TPanel;
    btnStop: TButton;
    lblTimeLeft: TLabel;
    lblTotTime: TLabel;
    lblLogHeader: TLabel;
    procedure Timer1Timer(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
  private
    { Private declarations }
    fStartTime: TDateTime;
    fSessionName: string;
    fHighestSeverity: TBoldLogType;
    procedure SetProgress(const Value: integer);
    procedure SetLogHeader(const Value: string);
    procedure SetProgressMax(const Value: integer);
    procedure Clear;
    procedure Hide;
    procedure Log(const s: string; LogType: TBoldLogType = ltInfo);
    procedure ProgressStep;
    procedure Show;
    procedure Sync; 
    procedure StartLog(const SessionName: String);
    procedure EndLog;
    procedure WarningIndicatorClick(Sender: TObject);
    procedure CalculateTimeLeft;
    procedure ProcessInterruption;
  public
    constructor create(Owner: TComponent); override;
    destructor destroy; override;
    class function CreateSmallLogForm(Caption: string): TForm;
    { Public declarations }
  end;

implementation

uses
  SysUtils,
  BoldUtils;

type
  TExposedShape = class(TShape)
  end;

{$R *.DFM}

procedure TBoldLogFrame.CalculateTimeLeft;
var
  TimeLeft, UsedTime: TDateTime;
  DonePart: Double;
  PartLeft: Double;
begin
  if (pgLog.Max <> 0) and (pgLog.Position <> 0) then
  begin
    UsedTime := now - fStartTime;
    DonePart := pgLog.Position / pgLog.Max;
    PartLeft := 1 - DonePart;
    TimeLeft := (PartLeft * usedTime) / DonePart;
    lblTimeLeft.Caption := 'Time left: ' + FormatDateTime('hh:mm:ss', timeLeft);
    lblTotTime.Caption := 'Tot time: ' + FormatDateTime('hh:mm:ss', TimeLeft + UsedTime);
    Refresh;
  end;
end;

procedure TBoldLogFrame.Clear;
begin
  lblLogMainHeader.Caption := '';
  lblLogHeader.Caption := '';
  lblLogText.Caption := '';
  pgLog.Position := 0;
  lblTimeLeft.Caption := '';
  WarningPanel.visible := false;
end;

constructor TBoldLogFrame.create(Owner: TComponent);
begin
  inherited;
  lblLogText.Caption := '';
  lblLogMainHeader.Caption := '';
  lblLogHeader.Caption := '';
  lblTimeLeft.Caption := ''; 
  BoldLog.RegisterLogReceiver(self);
  TExposedShape(WarningIndicator).OnClick := WarningIndicatorClick;
end;

destructor TBoldLogFrame.destroy;
begin
  BoldLog.UnRegisterLogReceiver(self);
  inherited;
end;

procedure TBoldLogFrame.EndLog;
begin
  lblLogText.Caption := 'Done...';
  pgLog.Position := pgLog.Max + 1;
  if not WarningPanel.Visible then
    Timer1.Enabled := true;
  btnStop.Enabled := false;

  if fHighestSeverity = ltWarning then
    lblLogText.Caption := lblLogText.Caption + '  there were warnings, click the yellow icon for details...'
  else if fHighestSeverity = ltError then
    lblLogText.Caption := lblLogText.Caption + '  there were errors, click the red icon for details...';
end;


procedure TBoldLogFrame.Hide;
begin
  visible := false;
end;

procedure TBoldLogFrame.Log(const s: string; LogType: TBoldLogType = ltInfo);
begin
  if LogType in [ltInfo, ltWarning, ltError] then
  begin
    lblLogText.caption := trim(s);
    Refresh;
  end;
  if LogType = ltWarning then
  begin
    WarningIndicator.Brush.Color := clYellow;
    WarningPanel.Visible := true;
  end;
  if LogType = ltError then
  begin
    WarningIndicator.Brush.Color := clRed;
    WarningPanel.Visible := true;
  end;
  if (LogType in [ltWarning, ltError]) and (LogType > fHighestSeverity) then
    fHighestSeverity := LogType;
end;

procedure TBoldLogFrame.ProgressStep;
begin
  pgLog.StepIt;
  CalculateTimeLeft;
end;

procedure TBoldLogFrame.SetProgress(const Value: integer);
begin
  pgLog.position := Value;
  CalculateTimeLeft;
end;

procedure TBoldLogFrame.SetLogHeader(const Value: string);
begin
  lblLogHeader.caption := Value;
  Refresh;
end;

procedure TBoldLogFrame.SetProgressMax(const Value: integer);
begin
  pgLog.Max := value;
end;

procedure TBoldLogFrame.Show;
begin
  Visible  := true;
end;

procedure TBoldLogFrame.StartLog(const SessionName: String);
begin
  Clear;
  lblLogMainHeader.Caption := SessionName;
  fSessionName := SessionName;
  Refresh;
  Show;
  fStartTime := now;
  Timer1.Enabled := false; 
  btnStop.Enabled := true;
  fHighestSeverity := ltInfo;
end;

procedure TBoldLogFrame.WarningIndicatorClick(Sender: TObject);
begin
  BoldLog.Show;
end;

procedure TBoldLogFrame.Timer1Timer(Sender: TObject);
begin
  Clear;
  Hide;
end;

procedure TBoldLogFrame.btnStopClick(Sender: TObject);
begin
  BoldLog.InterruptProcess;
  btnStop.Enabled := false;
end;

procedure TBoldLogFrame.ProcessInterruption;
begin
  application.ProcessMessages;
end;

procedure TBoldLogFrame.Sync;
begin
  Application.ProcessMessages;
end;

class function TBoldLogFrame.CreateSmallLogForm(Caption: string): TForm;
var
  Frame: TBoldLogFrame;
begin
  result := TForm.Create(nil);
  Result.BorderIcons := [];
  result.Position := poScreenCenter;
  Frame := TBoldLogFrame.Create(result);
  Frame.Parent := result;
  result.ClientHeight := frame.Height;
  result.ClientWidth := frame.Width;
  result.Caption := Caption;
  result.Show;
end;

initialization
end.
