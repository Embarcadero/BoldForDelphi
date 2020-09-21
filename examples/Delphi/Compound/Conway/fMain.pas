unit fMain;

interface

uses
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Dialogs,
  Forms,
  StdCtrls,
  ExtCtrls,
  ComCtrls,
  BoldSubscription,
  BoldHandle,
  BoldAbstractModel,
  BoldModel,
  BoldHandles,
  BoldSystemHandle,
  ConwayClasses,
  BoldReferenceHandle,
  BoldPropertiesController,
  BoldTrackBar,
  BoldLabel,
  BoldMemo,
  BoldCheckBox;

type
  TfrmMain = class(TForm)
    BoldSystemHandle1: TBoldSystemHandle;
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
    BoldModel1: TBoldModel;
    btnTick: TButton;
    btnClear: TButton;
    btnStart: TButton;
    Timer1: TTimer;
    refGame: TBoldReferenceHandle;
    BoldLabel3: TBoldLabel;
    Label4: TLabel;
    BoldMemo1: TBoldMemo;
    Label1: TLabel;
    Label2: TLabel;
    BoldLabel1: TBoldLabel;
    btbFontSize: TBoldTrackBar;
    bpcFontSize: TBoldPropertiesController;
    Label3: TLabel;
    BoldLabel2: TBoldLabel;
    BoldTrackBar1: TBoldTrackBar;
    bpcTimerInterval: TBoldPropertiesController;
    BoldLabel4: TBoldLabel;
    bcbCollecting: TBoldCheckBox;
    procedure btnTickClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    function GetGame: TGame;
  private
    { Private declarations }
    property Game: TGame read GetGame;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation      

{$R *.DFM}

procedure TfrmMain.btnTickClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  Game.Tick;
  Screen.Cursor := crDefault;
end;

procedure TfrmMain.btnClearClick(Sender: TObject);
begin
  Game.ClearCells;
end;

procedure TfrmMain.btnStartClick(Sender: TObject);
begin
  Timer1.Enabled := not Timer1.Enabled;
  if Timer1.Enabled then
    (Sender as TButton).Caption := 'Stop ticking'
  else
    (Sender as TButton).Caption := 'Start ticking'
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  StringList: TStringList;
begin
  refGame.Value := TGame.Create(nil);
  StringList := TStringList.Create;
  StringList.LoadFromFile('instructions.txt');
  ShowMessage(StringList.Text);
  StringList.Free;
end;

function TfrmMain.GetGame: TGame;
begin
  Result := refGame.Value as TGame;
end;

end.

