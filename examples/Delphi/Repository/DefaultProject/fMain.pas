unit fMain;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  Menus,
  BoldSystemDebuggerForm, StdCtrls;

type
  TfrmMain = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Debug1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    SystemDebugger1: TMenuItem;
    N2: TMenuItem;
    Updatedatabase1: TMenuItem;
    About1: TMenuItem;
    Memo1: TMemo;
    procedure Exit1Click(Sender: TObject);
    procedure SystemDebugger1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure About1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
  dMain;

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

end.
