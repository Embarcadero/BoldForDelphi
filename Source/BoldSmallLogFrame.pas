
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
  Messages,
  BoldDefs;

const
  WM_REFRESHGUI = WM_USER + 201;
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
    fProgressPosition: integer;
    fEnabled: Boolean;
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
    procedure DoRefresh;
    procedure RefreshGUI(var Message: TMessage); message WM_REFRESHGUI;
    procedure SetEnabled(const Value: Boolean);
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    class function CreateSmallLogForm(Caption: string): TForm;
    property Enabled: Boolean read fEnabled write SetEnabled;
    { Public declarations }
  end;

implementation

uses
  Windows,
  SysUtils,

  BoldCoreConsts,
  BoldUtils;

const
  c100ms = 1/24/60/60/10; // do not refresh more than once per 100 ms
  cProgressBarPrecision = 0.05; // ignore less than 5 percent movement

var
  _lastRefresh: TDateTime;

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
    lblTimeLeft.Caption := Format(sLogTimeLeft, [FormatDateTime('hh:mm:ss', timeLeft)]); // do not localize
    lblTotTime.Caption := Format(sLogTotalTime, [FormatDateTime('hh:mm:ss', TimeLeft + UsedTime)]); // do not localize
    DoRefresh;
  end;
end;

procedure TBoldLogFrame.Clear;
begin
  lblLogMainHeader.Caption := '';
  lblLogHeader.Caption := '';
  lblLogText.Caption := '';
  pgLog.Position := 0;
  fProgressPosition := 0;
  lblTimeLeft.Caption := '';
  WarningPanel.visible := false;
end;

constructor TBoldLogFrame.Create(Owner: TComponent);
begin
  inherited;
  lblLogText.Caption := '';
  lblLogMainHeader.Caption := '';
  lblLogHeader.Caption := '';
  lblTimeLeft.Caption := '';
  BoldLog.RegisterLogReceiver(self);
  TExposedShape(WarningIndicator).OnClick := WarningIndicatorClick;
end;

destructor TBoldLogFrame.Destroy;
begin
  BoldLog.UnRegisterLogReceiver(self);
  inherited;
end;

procedure TBoldLogFrame.DoRefresh;
begin
  if now - _lastRefresh > c100ms then
  begin
    _lastRefresh := now;
    Refresh;
  end;
end;

procedure TBoldLogFrame.EndLog;
begin
  lblLogText.Caption := sLogSmallDone;
  pgLog.Position := pgLog.Max + 1;
  fProgressPosition := pgLog.Max + 1;
  if not WarningPanel.Visible then
    Timer1.Enabled := true;
  btnStop.Enabled := false;

  if fHighestSeverity = ltWarning then
    lblLogText.Caption := Format(sLogSmallWarnings, [sLogSmallDone])
  else if fHighestSeverity = ltError then
    lblLogText.Caption := Format(sLogSmallErrors, [sLogSmallDone]);
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
    DoRefresh;
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
  if pgLog.Max = 0 then
    exit;
  Inc(fProgressPosition);
  if ((fProgressPosition-pgLog.Position) / pgLog.Max) > cProgressBarPrecision then
  begin
    pgLog.Position := fProgressPosition;
    CalculateTimeLeft;
  end;
end;

procedure TBoldLogFrame.RefreshGUI(var Message: TMessage);
begin
  DoRefresh;
  Show;
end;

procedure TBoldLogFrame.SetProgress(const Value: integer);
begin
  pgLog.position := Value;
  fProgressPosition := Value;
  CalculateTimeLeft;
end;

procedure TBoldLogFrame.SetEnabled(const Value: Boolean);
begin
  fEnabled := Value;
end;

procedure TBoldLogFrame.SetLogHeader(const Value: string);
begin
  lblLogHeader.caption := Value;
  DoRefresh;
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
  DoRefresh;
  PostMessage(Handle, WM_REFRESHGUI, 0, 0);
//  Show;
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
  Application.ProcessMessages;
end;

procedure TBoldLogFrame.Sync;
begin
  if Visible then
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

end.
