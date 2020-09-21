// /usr/local/cvsroot/tp/boldfordelphi/examples/Delphi/Compound/Bios/ClientGui/Mainform.pas,v 1.1 2003/01/21 07:12:14 jhogstrom Exp
unit mainform;
{$DEFINE BOLDCOMCLIENT}

interface

uses
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  Menus,
  StdCtrls,
  ExtCtrls,
  ComCtrls,
  Grids,
  BoldHandlesCom,
  BuildingsAndOwners_TLB,
  BoldGridCom,
  BoldEditCom,
  BoldNavigatorCom,
  BoldLabelCom,
  BoldListBoxCom,
  BoldControlPackCom,
  BoldCheckBoxCom,
  BoldStringControlPackCom,
  BoldSubscription,
  BoldComObjectSpace_TLB, BoldClientElementSupport, BoldComClient,
  BoldListHandleCom,
  BoldPersistenceNotifier,
  BoldAbstractListHandleCom,
  BoldCursorHandleCom,
  BoldExpressionHandleCom,
  BoldRootedHandlesCom,
  BoldHandle, BoldClientHandles, BoldComClientHandles;

type
  Tallform = class(TForm)
    BuildingPopup: TPopupMenu;
    newBuilding: TMenuItem;
    Delete1: TMenuItem;
    PersonPopup: TPopupMenu;
    NewPerson: TMenuItem;
    DeletePerson: TMenuItem;
    PersonGroup: TGroupBox;
    SingleLinkPopup: TPopupMenu;
    MenuItem5: TMenuItem;
    Label2: TLabel;
    Label3: TLabel;
    Label1: TLabel;
    FirstName: TLabel;
    btnUpdateDB: TButton;
    bedFirstName: TBoldEditCom;
    PersonCount: TBoldEditCom;
    bedPersonHome: TBoldEditCom;
    blbPersonOwnedBuildings: TBoldListBoxCom;
    MultiLinkPopup: TPopupMenu;
    MenuItem1: TMenuItem;
    bedLastName: TBoldEditCom;
    Label6: TLabel;
    blhAllPerson: TBoldListHandleCom;
    blhAllBuilding: TBoldListHandleCom;
    blhOwnedBuildingsHandle: TBoldListHandleCom;
    blhOwners: TBoldListHandleCom;
    behHome: TBoldExpressionHandleCom;
    Showinownwindow1: TMenuItem;
    Showinownwindow2: TMenuItem;
    ShowInOwnWindow3: TMenuItem;
    bedAssets: TBoldEditCom;
    Label7: TLabel;
    bcbRich: TBoldCheckBoxCom;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    bgrPerson: TBoldGridCom;
    pbdbNotification: TProgressBar;
    PageControl1: TPageControl;
    tabBuilding: TTabSheet;
    tabResidentialBuilding: TTabSheet;
    bgrResidentialBuilding: TBoldGridCom;
    BuildingGroup: TGroupBox;
    Owners: TLabel;
    Resi: TLabel;
    Label5: TLabel;
    bedAddress2: TBoldEditCom;
    blbBuildingResidents: TBoldListBoxCom;
    blbBuildingOwners2: TBoldListBoxCom;
    btnChargeRent: TButton;
    bgrBuilding: TBoldGridCom;
    GroupBox1: TGroupBox;
    Label4: TLabel;
    Label9: TLabel;
    bedAddress: TBoldEditCom;
    blbBuildingOwners: TBoldListBoxCom;
    blhResidents: TBoldListHandleCom;
    blhAllResidentialBuilding: TBoldListHandleCom;
    blhOwners2: TBoldListHandleCom;
    rgNameRepresentation: TRadioGroup;
    BoldPersistenceProgressNotifier1: TBoldPersistenceProgressNotifier;
    Button1: TButton;
    Label8: TLabel;
    Label10: TLabel;
    procedure newBuildingClick(Sender: TObject);
    procedure DeleteCurrentObject(Sender: TObject);
    procedure NewPersonClick(Sender: TObject);
    procedure Remove(Sender: TObject);
    procedure btnUpdateDBClick(Sender: TObject);
    procedure SingleItemRemove(Sender: TObject);
    procedure PopupPopup(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ShowInOwnWindow(Sender: TObject);
    function ReadOnlyRendererObserverMayModify(
      Element: IBoldElement; representation: Integer;
      Expression: String; Subscriber: TBoldComClientSubscriber): Boolean;
    function RendPerResidentRendererObserverMayModify(
      Element: IBoldElement; representation: Integer;
      Expression: String; Subscriber: TBoldComClientSubscriber): Boolean;
    procedure rgNameRepresentationClick(Sender: TObject);
    procedure btnChargeRentClick(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure NewBuilding1Click(Sender: TObject);
    procedure BoldSystemActivator1SystemActivated(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    function CurrentBuildingHandle: TBoldListHandleCom;
    procedure EnsureObjects;
  public
    { Public declarations }
  end;

var
  allform: Tallform;

implementation

uses
//  BoldGUI,
  DMClient, Renderers;

{$R *.DFM}

var
  Currentpopupcomponent: TComponent;

procedure Tallform.newBuildingClick(Sender: TObject);
begin
  CurrentBuildingHandle.List.AddNew;
end;

procedure Tallform.NewPersonClick(Sender: TObject);
begin
  DMClientSystem.SystemHandle.System.CreateNewObject('Person', true );
end;

procedure Tallform.DeleteCurrentObject(Sender: TObject);
var
  BoldObject: IBoldObject;
begin
  if (CurrentPopupComponent is TBoldListBoxCom) and
     Assigned(TBoldListBoxCom(CurrentPopupComponent).BoldHandle) then
    BoldObject := TBoldListBoxCom(CurrentPopupComponent).BoldHandle.CurrentBoldObject
  else if (CurrentPopupComponent is TBoldGridCom) and
          Assigned(TBoldGridCom(CurrentPopupComponent).BoldHandle) then
    BoldObject := TBoldGridCom(CurrentPopupComponent).BoldHandle.CurrentBoldObject
  else
    BoldObject := nil;

  if assigned(BoldObject) then
    BoldObject.Delete;
end;

procedure Tallform.Remove(Sender: TObject);
begin
  with Currentpopupcomponent as TBoldListBoxCom do
    if Assigned(BoldHandle) then
      BoldHandle.RemoveCurrentElement;
end;

procedure Tallform.btnUpdateDBClick(Sender: TObject);
begin
  DMClientSystem.SystemHandle.UpdateDatabase;
end;

procedure Tallform.SingleItemRemove(Sender: TObject);
begin
  with Currentpopupcomponent as TBoldEditCom do
     (BoldHandle.Value as IBoldObjectReference).BoldObject := nil;
end;

procedure Tallform.PopupPopup(Sender: TObject);
begin
   if (sender is TMenuItem) then
     Currentpopupcomponent  := ActiveControl
   else if sender is TPopupMenu then
     Currentpopupcomponent := TPopupMenu(sender).popupcomponent
   else
     Currentpopupcomponent := nil;
end;

procedure Tallform.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  Canclose := true;
  if DMClientSystem.SystemHandle.SystemActive and
    DMClientSystem.SystemHandle.System.BoldDirty then
    case (Messagedlg('There are unsaved changes on the server, do you want to save these to the db before disconnecting?',
                  mtConfirmation, [mbYes, mbNo, mbCancel], 0)) of
      mrYes: DMClientSystem.SystemHandle.UpdateDatabase;
      mrCancel: CanClose := false;
    end;
end;

procedure Tallform.ShowInOwnWindow(Sender: TObject);
var
  BoldObject: IBoldObject;
begin
   if (CurrentPopupComponent is TBoldListBoxCom) and
    Assigned(TBoldListBoxCom(CurrentPopupComponent).BoldHandle) then
    BoldObject := TBoldListBoxCom(CurrentPopupComponent).BoldHandle.CurrentBoldObject
  else  if (CurrentPopupComponent is TBoldGridCom) and
    Assigned(TBoldGridCom(CurrentPopupComponent).BoldHandle) then
    BoldObject := TBoldGridCom(CurrentPopupComponent).BoldHandle.CurrentBoldObject
  else
    BoldObject := nil;

//  if assigned(BoldObject) then
//    AutoFormProviderRegistry.FormForElement(BoldObject).Show;
end;

function Tallform.ReadOnlyRendererObserverMayModify(
  Element: IBoldElement; representation: Integer; Expression: String;
  Subscriber: TBoldComClientSubscriber): Boolean;
begin
  Result := True;
end;

function Tallform.RendPerResidentRendererObserverMayModify(
  Element: IBoldElement; representation: Integer; Expression: String;
  Subscriber: TBoldComClientSubscriber): Boolean;
begin
  result := false;
  with Element as IResidential_Building do
    Result := Residents.Count <> 0;
end;

procedure Tallform.rgNameRepresentationClick(Sender: TObject);
begin
  with blbBuildingOwners.BoldRowProperties do
    case rgNameRepresentation.ItemIndex of
      0: Expression  := 'firstName';
      1: Expression  := 'lastName';
      2: Expression  := 'firstName + '' '' + lastName';
    end;
end;

procedure Tallform.btnChargeRentClick(Sender: TObject);
begin
  (blhAllResidentialBuilding.CurrentBoldObject as IResidential_Building).ChargeRent;
end;

procedure Tallform.CheckBox1Click(Sender: TObject);
begin
{$IFNDEF BOLDCOMCLIENT}
  if CheckBox1.Checked then
    blhAllPerson.BoldFilter := DMClientSystem.IsRichFilter
  else
    blhAllPerson.BoldFilter := nil;
{$ENDIF}
end;

procedure Tallform.CheckBox2Click(Sender: TObject);
begin
{$IFNDEF BOLDCOMCLIENT}
  if CheckBox2.Checked then
    blhAllPerson.BoldComparer := DMClientSystem.NameComparer
  else
    blhAllPerson.BoldComparer := nil;
{$ENDIF}
end;

procedure Tallform.NewBuilding1Click(Sender: TObject);
begin
//  IBuilding.Create(nil);
end;

function Tallform.CurrentBuildingHandle: TBoldListHandleCom;

begin
  if PageControl1.ActivePage = tabBuilding then
    result := blhAllBuilding
  else
    result := blhAllResidentialBuilding;
end;

procedure Tallform.EnsureObjects;
begin
  Randomize;
//  (blhAllPerson.list as IBoldObjectList).EnsureObjects;
//  (blhAllBuilding.list as IBoldObjectList).EnsureObjects;
end;

procedure Tallform.BoldSystemActivator1SystemActivated(Sender: TObject);
begin
  EnsureObjects;
end;

procedure Tallform.Button1Click(Sender: TObject);
begin
  DMClientSystem.BoldComConnectionHandle1.Connected := true;
  Button1.Enabled := false;
end;

procedure Tallform.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  DMClientSystem.BoldComConnectionHandle1.Connected := false;
end;

end.


