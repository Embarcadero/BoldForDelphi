unit fBatchUpgraderGui;

interface

uses
  SysUtils,
  Classes,
  Controls,
  Forms,
  StdCtrls,
  BoldBatchUpgrader, ComCtrls, ExtCtrls, BoldTrackBar, BoldHandles,
  BoldSystemHandle, BoldSubscription, BoldVariableHandle, BoldLabel;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    lblStatus: TLabel;
    Timer1: TTimer;
    dtpExecuteTime: TDateTimePicker;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    lblReport: TLabel;
    Button1: TButton;
    BoldLabel1: TBoldLabel;
    BoldVariableHandle1: TBoldVariableHandle;
    BoldSystemHandle1: TBoldSystemHandle;
    BoldVariableHandle2: TBoldVariableHandle;
    BoldVariableHandle3: TBoldVariableHandle;
    tbIntervalTime: TBoldTrackBar;
    tbBatchSize: TBoldTrackBar;
    tbSleepTime: TBoldTrackBar;
    BoldLabel2: TBoldLabel;
    BoldLabel3: TBoldLabel;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    fBatchUpgrader: TBoldBatchUpgrader;
    fActive: boolean;
    procedure CopyValuesFromGuiToComponents;
    procedure GoToSleep;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses dPersistence, dSystemTypeInfo;

{$R *.DFM}

procedure TForm1.CopyValuesFromGuiToComponents;
begin
  timer1.Interval := tbSleepTime.Position*1000;
  fBatchUpgrader.BatchSize := tbBatchSize.Position;
  fBatchUpgrader.MaxExecuteTime := Frac(dtpExecuteTime.time);
  fBatchUpgrader.IntervalBetweenBatches := tbIntervalTime.Position*1000;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  fBatchUpgrader := TBoldBatchUpgrader.Create(
    dmPersistence.BoldPersistenceHandleDB1.PersistenceControllerDefault.PersistenceMapper,
    dmPersistence.ObjectUpgrader.ObjectUpgrader);
end;

procedure TForm1.GoToSleep;
begin
  CopyValuesFromGuiToComponents;
  Timer1.Enabled := true;
  lblStatus.Caption := 'Sleeping';
  Button1.Caption := 'Stop';
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := false;
  lblStatus.Caption := 'Upgrading';
  lblStatus.Refresh;
  CopyValuesFromGuiToComponents;
  dmPersistence.BoldPersistenceHandleDB1.Active := true;
  fBatchUpgrader.UpgradeObjects;
  lblReport.Caption := format('Upgraded: %d  AutoUpgraded: %d', [
    fBatchUpgrader.UpgradedObjects,
    fBatchUpgrader.AutoUpgradedObjects]);
  dmPersistence.BoldPersistenceHandleDB1.Active := false;
  GoToSleep;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  fActive := not fActive;
  if fActive then
    GoToSleep
  else
  begin
    Timer1.Enabled := false;
    Button1.Caption := 'Start';
  end;
end;

end.
