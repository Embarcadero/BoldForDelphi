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
  BoldSystemDebuggerForm, StdCtrls, ExtCtrls, BoldNavigator, Grids,
  BoldGrid, BoldSubscription, BoldHandles, BoldRootedHandles,
  BoldAbstractListHandle, BoldCursorHandle, BoldListHandle,
  BoldNavigatorDefs;

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
    blhPersons: TBoldListHandle;
    BoldGrid1: TBoldGrid;
    BoldNavigator1: TBoldNavigator;
    blhProducts: TBoldListHandle;
    BoldGrid2: TBoldGrid;
    BoldNavigator2: TBoldNavigator;
    blhOrders: TBoldListHandle;
    blhOrderItems: TBoldListHandle;
    BoldGrid3: TBoldGrid;
    BoldGrid4: TBoldGrid;
    BoldNavigator3: TBoldNavigator;
    Button1: TButton;
    blhDiscounts: TBoldListHandle;
    BoldGrid5: TBoldGrid;
    Button2: TButton;
    Label1: TLabel;
    Button3: TButton;
    procedure Exit1Click(Sender: TObject);
    procedure SystemDebugger1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure About1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
  dSystem, ModelEvClasses;

{$R *.DFM}

procedure TfrmMain.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.SystemDebugger1Click(Sender: TObject);
begin
  TBoldSystemDebuggerFrm.CreateWithSystem(self, dmSystem.SystemHandle.System).show;
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := true;
  if dmSystem.SystemHandle.Active and dmSystem.SystemHandle.system.BoldDirty then
  begin
    if MessageDlg('You have dirty objects. Do you want to quit anyway?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      dmSystem.SystemHandle.system.Discard
    else
      CanClose := false;
  end
end;

procedure TfrmMain.About1Click(Sender: TObject);
begin
  ShowMessage('Yet another project powered by Bold technology');
end;

procedure TfrmMain.Button1Click(Sender: TObject);
var
  OrderItem: TOrderItem;
begin
  if assigned(blhOrders.CurrentBoldObject) and assigned(blhProducts.CurrentBoldObject) then
  begin
    OrderItem := TOrderItem.Create(dmSystem.SystemHandle.System);
    OrderItem.Order := blhOrders.CurrentBoldObject as TOrder;
    OrderItem.Product := blhProducts.CurrentBoldObject as TProduct;
  end;
end;

procedure TfrmMain.Button2Click(Sender: TObject);
begin
  if assigned(blhOrders.CurrentBoldObject) and assigned(blhDiscounts.CurrentBoldObject) then
    (blhOrders.CurrentBoldObject as TOrder).theDiscount := blhDiscounts.CurrentBoldObject as TDiscount;
end;

procedure TfrmMain.Button3Click(Sender: TObject);
begin
  dmSystem.SystemHandle.UpdateDataBase;
  dmSystem.SystemHandle.Active := false;
  dmSystem.SystemHandle.Active := true;
end;

end.
