unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  BoldHandle, BoldClientHandles, BoldComClientHandles, BoldSubscription,
  BoldAbstractLockManagerAdminHandle, BoldLockManagerAdminHandleCom,
  StdCtrls, ExtCtrls, ComCtrls, Buttons, Menus;

type

  TFMain = class(TForm)
    BitBtn1: TBitBtn;
    PageControl1: TPageControl;
    tsLockManagerStatus: TTabSheet;
    tsClients: TTabSheet;
    tsLocks: TTabSheet;
    Label1: TLabel;
    lbclients: TListBox;
    cbShowLocks: TCheckBox;
    lvLocks: TListView;
    rgList: TRadioGroup;
    rgLockManagerStatus: TRadioGroup;
    Label2: TLabel;
    ListView2: TListView;
    pmClient: TPopupMenu;
    Kill1: TMenuItem;
    ShowLocks1: TMenuItem;
    pmLock: TPopupMenu;
    Free1: TMenuItem;
    CheckBox2: TCheckBox;
    Viewclientsholdinglock1: TMenuItem;
    ListBox2: TListBox;
    BitBtn2: TBitBtn;
    lbClientName: TLabel;
    lbClient: TLabel;
    Label3: TLabel;
    edServerName: TEdit;
    btnConnect: TButton;
    procedure BitBtn1Click(Sender: TObject);
    procedure lbclientsClick(Sender: TObject);
    procedure cbShowLocksClick(Sender: TObject);
    procedure rgListClick(Sender: TObject);
    procedure tsLockManagerStatusShow(Sender: TObject);
    procedure ShowLocks1Click(Sender: TObject);
    procedure Kill1Click(Sender: TObject);
    procedure tsClientsShow(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FMain: TFMain;

implementation

uses
  dmLockManagerAdmin;

{$R *.DFM}

procedure TFMain.BitBtn1Click(Sender: TObject);
begin
  dmMain.GetClients;
end;

procedure TFMain.lbclientsClick(Sender: TObject);
var
  i: integer;
  clientInfo: TClientInfo;
  lockinfo: TLockInfo;
  item: TListItem;
begin

  clientinfo := lbclients.Items.Objects[lbclients.ItemIndex] as TClientInfo;
  lbClientName.Caption := lbclients.Items[lbclients.ItemIndex];
  lvLocks.Items.BeginUpdate;
  lvLocks.Items.Clear;
  for i:= 0 to clientinfo.Locks.count - 1 do
  begin
    item := lvLocks.Items.Add;
    lockinfo := (clientinfo.Locks.Objects[i] as Tlockinfo);
    item.Caption := lockinfo.LockName;
    item.SubItems.Add(lockinfo.Duration);
  end;
  lvLocks.Items.EndUpdate;
end;

procedure TFMain.cbShowLocksClick(Sender: TObject);
begin
  lvLocks.Visible := cbShowLocks.Checked;
  lbclient.Visible := lvLocks.Visible;
  lbclientName.Visible := lvLocks.Visible;
  lvLocks.Items.BeginUpdate;
  lvLocks.Items.Clear;
  lvLocks.Items.EndUpdate;
  if lvLocks.Visible then
    lbClientsClick(self);
end;

procedure TFMain.rgListClick(Sender: TObject);
begin
  dmMain.ViewAll := (rgList.ItemIndex = 0);
end;

procedure TFMain.tsLockManagerStatusShow(Sender: TObject);
begin
  if dmMain.LockManagerSuspended then
    rgLockManagerStatus.ItemIndex := 1
  else
    rgLockManagerStatus.ItemIndex := 0;
end;

procedure TFMain.ShowLocks1Click(Sender: TObject);
begin
  cbShowLocksClick(sender);
end;

procedure TFMain.Kill1Click(Sender: TObject);
var
  clientId: integer;
begin
  if (lbclients.ItemIndex <> -1) then
  begin
    if (lbclients.Items.Names[lbclients.ItemIndex] <> '') then
      ClientId := StrToInt(trim(lbclients.Items.Names[lbclients.ItemIndex]))
    else
      ClientId := StrToInt(trim(lbclients.Items[lbclients.ItemIndex]));
    dmMain.KillClient(ClientId);
  end;
end;

procedure TFMain.tsClientsShow(Sender: TObject);
begin
  dmMain.ViewAll := (rgList.ItemIndex = 0);
end;

procedure TFMain.btnConnectClick(Sender: TObject);
begin
  if not dmMain.BoldComconnectionHandle1.Connected then
  begin
    dmMain.BoldComConnectionHandle1.Connected := True;
    dmMain.BoldLockManagerAdminHandleCom1.Active := True;
    btnConnect.Caption := '&Disconnect';
  end
  else
  begin
    dmMain.BoldLockManagerAdminHandleCom1.Active := False;
    dmMain.BoldComConnectionHandle1.Connected := False;
    btnConnect.Caption := '&Connect';
  end;
end;

end.
