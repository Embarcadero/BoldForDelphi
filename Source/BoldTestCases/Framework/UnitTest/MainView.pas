unit MainView;

interface

uses
  Windows,
  SysUtils,
  Classes,
  clipbrd,
  Graphics,
  Controls,
  Forms,
  StdCtrls,
  TestSuite,
  ExtCtrls,
  ComCtrls, Grids;

type
  TErrorLevel = (elNone, elOK, elFault);

  TMainViewForm = class(TForm)
    StatusBar: TStatusBar;
    Panel1: TPanel;
    Label1: TLabel;
    Label7: TLabel;
    btnRun: TButton;
    cmbName: TComboBox;
    grdTestLog: TStringGrid;
    Button1: TButton;
    Animate1: TAnimate;
    Button2: TButton;
    procedure btnRunClick(Sender: TObject);
    procedure StatusBarDrawPanel(StatusBar: TStatusBar;
      Panel: TStatusPanel; const Rect: TRect);
    procedure FormResize(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    FErrorLevel: TErrorLevel;
    fTotalRunsInfoString: String;
    fErrorInfoString: String;
    fFailureInfoString: String;
    fNotFoundInfoString: String;
    fAutoActivate: boolean;
    fTimer: TTimer;
    procedure ShowResults(TestSuite: TBoldTestSuite);
    procedure UpdateInfoStrings(TestSuite: TBoldTestSuite);
    procedure Clear;
    procedure RunTests(const TestName: string; NotifyFault: TNotifyFault);
    procedure ShowFault(TestCase: TBoldTestCase);
    procedure SetStatusBarPanelBevels(VisiblePanels: Integer);
    procedure SetStatusBarPanelCount(VisiblePanels: integer);
    procedure UpdateStatusPanels(VisiblePanels: integer);
    procedure LoadTestCases;
    procedure AddLogTitle;
    procedure SetAutoActivate(const Value: boolean);
    procedure CLickRunButton(Sender: TObject);
  public
    { Public declarations }
    property AutoActivate: boolean read fAutoActivate write SetAutoActivate;
  end;

var
  MainViewForm: TMainViewForm;

implementation
uses
  BoldLogHandler;

{$R *.DFM}

procedure TMainViewForm.btnRunClick(Sender: TObject);
begin
  btnRun.Caption := 'Working';
  Animate1.Play(0, Animate1.FrameCount-1, 0);
  Clear;
  RunTests(cmbName.Text, ShowFault);
  Animate1.Stop;
  BtnRun.Caption := 'RUN';
end;

procedure TMainViewForm.Clear;
begin
  UpdateStatusPanels(0);
  fTotalRunsInfoString := '';
  fErrorInfoString := '';
  fFailureInfoString := '';
  fNotFoundInfoString := '';
  FErrorLevel := elNone;
  grdTestLog.RowCount := 2;
  AddLogTitle;
end;


procedure TMainViewForm.RunTests(const TestName: string; NotifyFault: TNotifyFault);
begin
  Screen.Cursor := crHourGlass;
  TestGlobal.RunTests(TestName, NotifyFault);
  Screen.Cursor := crDefault;
end;

procedure TMainViewForm.ShowResults(TestSuite: TBoldTestSuite);
begin
  UpdateInfoStrings(TestSuite);
  with TestSuite do
  begin
    if ProblemCount = 0 then //All OK
    begin
      FErrorLevel := elOK;
      UpdateStatusPanels(1);
    end
    else
    begin
      UpdateStatusPanels(4);
      FErrorLevel := elFault;
    end;
  end;
  StatusBar.Invalidate;
  Application.ProcessMessages;
end;

procedure TMainViewForm.ShowFault(TestCase: TBoldTestCase);
begin
  with TestCase do
  begin
    if grdTestLog.Cells[0, grdTestLog.RowCount-1] <> '' then
      grdTestLog.RowCount := GrdTestLog.RowCount+1;
    grdTestLog.Rows[grdTestLog.RowCount-1].Commatext := TestGlobal.CommaLine(ClassName,
                                         Name,
                                         ErrorMsg[TestResult],
                                         IntToStr(ElapsedTestTime)+'ms',
                                         testMessage,
                                         Comment);
    while grdTestLog.TopRow + grdTestLog.VisibleRowCount < grdTestLog.RowCount do
      grdTestLog.TopRow := grdTestLog.TopRow+1;
  end;
  ShowResults(TestCase.OwningSuite);
end;

procedure TMainViewForm.SetStatusBarPanelBevels(VisiblePanels: Integer);
var
  i: Integer;
begin
  with StatusBar do
    for i := 0 to Panels.Count-1 do
      if i < VisiblePanels then
        Panels[i].Bevel := pbLowered
      else
        Panels[i].Bevel := pbNone;
end;

procedure TMainViewForm.StatusBarDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel; const Rect: TRect);
var
  InfoMessage: String;
  XPos: Integer;
  BackgroundColor: TColor;
begin
  with Statusbar.Canvas do
    case fErrorLevel of
      elNone: FillRect(rect);

      elOK: begin
        Brush.Color := clGreen;
        Font.Color := clWhite;
        FillRect(rect);
        InfoMessage := fTotalRunsInfoString;
        TextOut(Rect.Left + 4, Rect.Top-1, InfoMessage);

        InfoMessage := 'All passed!';
        XPos := (Width - TextWidth(InfoMessage)) div 2;
        TextOut(XPos, Rect.Top-1, InfoMessage);
      end;

      elFault: begin //Display problem reports
        BackgroundColor := clBlue; //Avoid compiler warning
        case Panel.Index of
          0: begin
            InfoMessage := fTotalRunsInfoString;
            BackgroundColor := clGreen;
          end;
          1: begin
            InfoMessage := fErrorInfoString;
            BackgroundColor := clRed;
          end;
          2: begin
            InfoMessage := fFailureInfoString;
            BackgroundColor := clMaroon;
          end;
          3: begin
            InfoMessage := fNotFoundInfoString;
            BackgroundColor := clBlue;
          end;
        end;
        Brush.Color := BackgroundColor;
        Font.Color := clWhite;
        FillRect(rect);
        TextOut(Rect.Left + 4, Rect.Top-1, InfoMessage);
      end;
    end;
end;

procedure TMainViewForm.FormResize(Sender: TObject);
begin
  case FErrorLevel of
    elOK: UpdateStatusPanels(1);
    elFault: UpdateStatusPanels(4);
  end;
end;

procedure TMainViewForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Close;
end;

procedure TMainViewForm.FormShow(Sender: TObject);
begin
  cmbName.SetFocus;
end;

procedure TMainViewForm.FormCreate(Sender: TObject);
begin
{  lblHeader.Font.Assign(mmoTestLog.Font);
  lblHeader.Caption := TestGlobal.ErrorLine('SUITE', 'Name', 'Status', 'Time', 'Msg',  'Comment');
  }
  Clear;
  LoadTestCases;
end;

procedure TMainViewForm.SetStatusBarPanelCount(VisiblePanels: integer);
var
  i,
  w: Integer;
begin
  if VisiblePanels = 0 then Exit;
  with StatusBar do
  begin
    w := Width div VisiblePanels;
    for i := 0 to Panels.Count-1 do
      if i < VisiblePanels then
        Panels[i].Width := w
      else
        Panels[i].Width := 0;
  end;
end;

procedure TMainViewForm.UpdateStatusPanels(VisiblePanels: integer);
begin
  if Visible then
  begin
    SetStatusBarPanelBevels(VisiblePanels);
    SetStatusBarPanelCount(VisiblePanels);
  end;
end;

procedure TMainViewForm.UpdateInfoStrings(TestSuite: TBoldTestSuite);
begin
  with TestSuite do
  begin
    fTotalRunsInfoString := 'Runs: ' + IntToStr(Runs);
    fErrorInfoString := 'Errors: ' + IntToStr(Errors);
    fFailureInfoString := 'Failures: ' + IntToStr(Failures);
    fNotFoundInfoString := 'Not found: ' + IntToStr(NotFound);
  end;
end;

procedure TMainViewForm.LoadTestCases;
begin
  with cmbName do
  begin
    Items.Assign(TestGlobal.TestCaseList);
    ItemIndex := 0;
  end;
end;

procedure TMainViewForm.AddLogTitle;
begin
  GrdTestlog.Rows[0].CommaText := 'SUITE, Name, Status, Time, Msg,  Comment';
end;

procedure TMainViewForm.SetAutoActivate(const Value: boolean);
begin
  fAutoActivate := Value;
  if Value then
  begin
    fTimer := TTimer.Create(Self);
    fTimer.Interval := 50;
    fTimer.OnTimer := ClickRunButton;
    fTimer.Enabled := True;
  end;
end;


procedure TMainViewForm.CLickRunButton(Sender: TObject);
begin
  fTimer.Enabled := False;
  btnRun.Click;
end;

procedure TMainViewForm.Button1Click(Sender: TObject);
var
  row, col: integer;
  s: String;
begin
  s := '';
  for Row := GrdTestLog.FixedRows to grdTestlog.RowCount-1 do
  begin
    for col := 0 to grdTestLog.ColCount-1 do
      s := s + grdTestLog.Cells[col, row] + #9;
    s := s + #13#10;
  end;
  ClipBoard.AsText := s;
end;

procedure TMainViewForm.Button2Click(Sender: TObject);
begin
  BoldLog.show;
end;

end.
