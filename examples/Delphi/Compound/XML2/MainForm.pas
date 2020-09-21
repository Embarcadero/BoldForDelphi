unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  BoldSubscription, BoldHandles, BoldRootedHandles, BoldAbstractListHandle,
  BoldCursorHandle, BoldListHandle, ExtCtrls, BoldNavigatorDefs,
  BoldNavigator, StdCtrls, Grids, BoldGrid, BoldActions, ActnList,
  BoldHandleAction, BoldEdit,
  BoldExpressionHandle,
  BoldDerivedHandle,
  BoldElements,
  BoldReferenceHandle,
  BoldCheckBox,
  BoldAFPPluggable,
  BoldImage,
  BoldDBActions, BoldIBDatabaseAction;

type
  TfrmMain = class(TForm)
    blhAllCompany: TBoldListHandle;
    blhAllPerson: TBoldListHandle;
    grdCompany: TBoldGrid;
    grdPerson: TBoldGrid;
    Label1: TLabel;
    Label2: TLabel;
    BoldNavigator1: TBoldNavigator;
    BoldNavigator2: TBoldNavigator;
    ActionList1: TActionList;
    Button1: TButton;
    Button2: TButton;
    BoldActivateSystemAction1: TBoldActivateSystemAction;
    BoldUpdateDBAction1: TBoldUpdateDBAction;
    blhCompanyOffices: TBoldListHandle;
    BoldGrid1: TBoldGrid;
    BoldNavigator3: TBoldNavigator;
    Label3: TLabel;
    GroupBox1: TGroupBox;
    BoldEdit1: TBoldEdit;
    BoldEdit2: TBoldEdit;
    BoldEdit3: TBoldEdit;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    btnAddAddress: TButton;
    AddAddressAction: TAction;
    BoldEdit4: TBoldEdit;
    Label7: TLabel;
    BoldEdit5: TBoldEdit;
    BoldEdit6: TBoldEdit;
    Label8: TLabel;
    Label9: TLabel;
    behAddress: TBoldExpressionHandle;
    Label10: TLabel;
    blhDepartments: TBoldListHandle;
    BoldGrid2: TBoldGrid;
    BoldNavigator4: TBoldNavigator;
    Label11: TLabel;
    blhEmployees: TBoldListHandle;
    grdEmployees: TBoldGrid;
    BoldPlaceableAFP1: TBoldPlaceableAFP;
    biLogo: TBoldImage;
    Label12: TLabel;
    Button3: TButton;
    LoadLogFromFile: TAction;
    dlgSelectFile: TOpenDialog;
    Button5: TButton;
    Button6: TButton;
    blhFiltered: TBoldListHandle;
    BoldGrid3: TBoldGrid;
    blhSearchPerson: TBoldListHandle;
    BoldGrid4: TBoldGrid;
    BoldIBDatabaseAction1: TBoldIBDatabaseAction;
    procedure AddAddressActionExecute(Sender: TObject);
    procedure AddAddressActionUpdate(Sender: TObject);
    procedure LoadLogFromFileExecute(Sender: TObject);
    procedure Button6Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
  MainDataModule,
  OrgChartClasses;

{$R *.DFM}

procedure TfrmMain.AddAddressActionExecute(Sender: TObject);
var
  CurOffice: TOffice;
begin
  CurOffice := blhCompanyOffices.CurrentBoldObject as TOffice;
  if Assigned(CurOffice) and not Assigned(CurOffice.address) then
    CurOffice.address := TAddress.Create(dmMain.OrgChartSystem.System);
end;

procedure TfrmMain.AddAddressActionUpdate(Sender: TObject);
begin
  AddAddressAction.Enabled := dmMain.OrgChartSystem.Active;
end;

procedure TfrmMain.LoadLogFromFileExecute(Sender: TObject);
begin
  if Assigned(blhAllCompany.CurrentBoldObject) then
  begin
    dlgSelectFile.Execute;
    biLogo.LoadFromFile(dlgSelectFile.FileName);
  end;
end;

procedure TfrmMain.Button6Click(Sender: TObject);
begin
  dmMain.GenerateData;
end;

end.
