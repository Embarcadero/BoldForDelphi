unit fMain;

interface

uses
  SysUtils,
  Classes,
  Controls,
  Forms,
  Dialogs,
  Menus,
  StdCtrls,
  ExtCtrls,
  ComCtrls,
  BoldSystemDebuggerForm,
  BoldSubscription,
  BoldHandles,
  BoldRootedHandles,
  BoldAbstractListHandle,
  BoldExpressionHandle,
  BoldCursorHandle,
  BoldListHandle,
  BoldAFPPluggable,
  BoldPageControl,
  BoldEdit,
  BoldNavigator,
  BoldListBox,
  BoldComboBox,
  BoldNavigatorDefs;

type
  TfrmMain = class(TForm)
    GroupBox1: TGroupBox;
    blhAllAccounts: TBoldListHandle;
    BoldListBox1: TBoldListBox;
    BoldNavigator1: TBoldNavigator;
    BoldEdit1: TBoldEdit;
    BoldEdit2: TBoldEdit;
    BoldEdit3: TBoldEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    GroupBox2: TGroupBox;
    BoldListBox2: TBoldListBox;
    blhAllRequests: TBoldListHandle;
    btnRun: TButton;
    BoldPlaceableAFP1: TBoldPlaceableAFP;
    btnTransfer: TButton;
    btnModifyCredit: TButton;
    btnCloseAccount: TButton;
    Label4: TLabel;
    BoldPageControl1: TBoldPageControl;
    tsTransfer: TTabSheet;
    tsClose: TTabSheet;
    tsModifyCredit: TTabSheet;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    BoldComboBox1: TBoldComboBox;
    BoldComboBox2: TBoldComboBox;
    BoldEdit4: TBoldEdit;
    behTransfer: TBoldExpressionHandle;
    behClose: TBoldExpressionHandle;
    behModifyCredit: TBoldExpressionHandle;
    BoldComboBox3: TBoldComboBox;
    BoldComboBox4: TBoldComboBox;
    BoldEdit5: TBoldEdit;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    btnDelete: TButton;
    Button1: TButton;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    Debug1: TMenuItem;
    SystemDebugger1: TMenuItem;
    N1: TMenuItem;
    procedure Exit1Click(Sender: TObject);
    procedure SystemDebugger1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnRunClick(Sender: TObject);
    procedure btnTransferClick(Sender: TObject);
    procedure btnModifyCreditClick(Sender: TObject);
    procedure btnCloseAccountClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
  BoldSystem,
  BankingClasses,
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

procedure TfrmMain.btnRunClick(Sender: TObject);
begin
  dmMain.bshMain.System.StartTransaction;
  try

    while blhAllRequests.List.Count > 0 do
    begin
      (blhAllRequests.list[0] as TRequest).Perform;
      (blhAllRequests.list[0] as TRequest).Delete;
    end;

    dmMain.bshMain.System.CommitTransaction;
  except
    dmMain.bshMain.System.RollbackTransaction;
    raise; // Insert more advanced error message here
  end;
end;

procedure TfrmMain.btnTransferClick(Sender: TObject);
begin
  TTransfer.Create(dmMain.bshMain.System);
end;

procedure TfrmMain.btnModifyCreditClick(Sender: TObject);
begin
  TModifyCredit.Create(dmMain.bshMain.System);
end;

procedure TfrmMain.btnCloseAccountClick(Sender: TObject);
begin
  TClose.Create(dmMain.bshMain.System);
end;

procedure TfrmMain.btnDeleteClick(Sender: TObject);
begin
  blhAllRequests.CurrentBoldObject.Delete;
end;

end.
