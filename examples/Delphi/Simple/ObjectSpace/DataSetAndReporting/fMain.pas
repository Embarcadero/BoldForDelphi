unit fMain;

interface

uses
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  Menus,
  ExtCtrls,
  StdCtrls,
  Grids,
  BoldSystemDebuggerForm,
  BoldSubscription,
  BoldHandles,
  BoldRootedHandles,
  BoldAbstractListHandle,
  BoldCursorHandle,
  BoldListHandle,
  BoldExpressionHandle,
  BoldNavigator,
  BoldGrid,
  BoldListBox,
  BoldEdit, BoldNavigatorDefs;

type
  TfrmMain = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Debug1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    SystemDebugger1: TMenuItem;
    N2: TMenuItem;
    About1: TMenuItem;
    BoldGrid1: TBoldGrid;
    Label1: TLabel;
    btnGenerateReport: TButton;
    BoldNavigator1: TBoldNavigator;
    blhPersons: TBoldListHandle;
    BoldGrid2: TBoldGrid;
    Label2: TLabel;
    BoldNavigator2: TBoldNavigator;
    blhBuildings: TBoldListHandle;
    blhPersonHome: TBoldExpressionHandle;
    BoldEdit1: TBoldEdit;
    Label3: TLabel;
    BoldListBox1: TBoldListBox;
    blhResidents: TBoldListHandle;
    Label4: TLabel;
    UpdateDB1: TMenuItem;
    procedure Exit1Click(Sender: TObject);
    procedure SystemDebugger1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure About1Click(Sender: TObject);
    procedure btnGenerateReportClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
  dMain, FfrmReport;

{$R *.DFM}

procedure TfrmMain.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.SystemDebugger1Click(Sender: TObject);
begin
  TBoldSystemDebuggerFrm.CreateWithSystem(self, dmMain.bshMain.System).show;
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := true;
  if dmMain.bshMain.Active and dmMain.bshMain.system.BoldDirty then
  begin
    if MessageDlg('You have dirty objects. Do you want to quit anyway?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      dmMain.bshMain.system.Discard
    else
      CanClose := false;
  end
end;

procedure TfrmMain.About1Click(Sender: TObject);
begin
  ShowMessage('Yet another project powered by Bold technology');
end;

procedure TfrmMain.btnGenerateReportClick(Sender: TObject);
begin
  with TfrmReport.Create(Self) do
  begin
    bdsBuilding.Active := True;
    bdsResident.Active := True;
    qrMain.Preview;
  end;
end;

end.
