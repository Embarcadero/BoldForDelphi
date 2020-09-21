{$UNDEF BOLDCOMCLIENT}

unit mainform;

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
  BoldSystem,
  BoldHandles,
  BuildingsAndOwners,
  BoldGrid,
  BoldEdit,
  BoldNavigator,
  BoldLabel,
  BoldListBox,
  BoldControlPack,
  BoldCheckBox,
  BoldAFP,
  BoldAFPDefault,
  BoldStringControlPack,
  BoldSubscription,
  BoldElements,
  BoldListHandle,
  BoldPersistenceNotifier,
  BoldAbstractListHandle,
  BoldCursorHandle,
  BoldExpressionHandle,
  BoldRootedHandles,
  BoldIBAliasCreator;

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
    bedFirstName: TBoldEdit;
    PersonCount: TBoldEdit;
    bedPersonHome: TBoldEdit;
    blbPersonOwnedBuildings: TBoldListBox;
    MultiLinkPopup: TPopupMenu;
    MenuItem1: TMenuItem;
    bedLastName: TBoldEdit;
    Label6: TLabel;
    blhAllPerson: TBoldListHandle;
    blhAllBuilding: TBoldListHandle;
    blhOwnedBuildingsHandle: TBoldListHandle;
    blhOwners: TBoldListHandle;
    behHome: TBoldExpressionHandle;
    Showinownwindow1: TMenuItem;
    Showinownwindow2: TMenuItem;
    ShowInOwnWindow3: TMenuItem;
    bedAssets: TBoldEdit;
    Label7: TLabel;
    bcbRich: TBoldCheckBox;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    bgrPerson: TBoldGrid;
    pbdbNotification: TProgressBar;
    PageControl1: TPageControl;
    tabBuilding: TTabSheet;
    tabResidentialBuilding: TTabSheet;
    bgrResidentialBuilding: TBoldGrid;
    BuildingGroup: TGroupBox;
    Owners: TLabel;
    Resi: TLabel;
    Label5: TLabel;
    bedAddress2: TBoldEdit;
    blbBuildingResidents: TBoldListBox;
    blbBuildingOwners2: TBoldListBox;
    btnChargeRent: TButton;
    bgrBuilding: TBoldGrid;
    GroupBox1: TGroupBox;
    Label4: TLabel;
    Label9: TLabel;
    bedAddress: TBoldEdit;
    blbBuildingOwners: TBoldListBox;
    blhResidents: TBoldListHandle;
    blhAllResidentialBuilding: TBoldListHandle;
    blhOwners2: TBoldListHandle;
    rgNameRepresentation: TRadioGroup;
    BoldPersistenceProgressNotifier1: TBoldPersistenceProgressNotifier;
    BoldSystemActivator1: TBoldSystemActivator;
    BoldIBAliasCreator1: TBoldIBAliasCreator;
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
      Element: TBoldElement; representation: Integer;
      Expression: String; Subscriber: TBoldSubscriber): Boolean;
    function RendPerResidentRendererObserverMayModify(
      Element: TBoldElement; representation: Integer;
      Expression: String; Subscriber: TBoldSubscriber): Boolean;
    procedure rgNameRepresentationClick(Sender: TObject);
    procedure btnChargeRentClick(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure NewBuilding1Click(Sender: TObject);
    procedure BoldSystemActivator1SystemActivated(Sender: TObject);


  private
    { Private declarations }
    function CurrentBuildingHandle: TBoldListHandle;
    procedure EnsureObjects;
  public
    { Public declarations }
  end;

var
  allform: Tallform;

implementation

uses
  BoldGUI,
  DmCore, Renderers;

{$R *.DFM}

var
  Currentpopupcomponent: TComponent;

procedure Tallform.newBuildingClick(Sender: TObject);
begin
  CurrentBuildingHandle.List.AddNew;
end;

procedure Tallform.NewPersonClick(Sender: TObject);
begin
  TPerson.Create(nil);
end;

procedure Tallform.DeleteCurrentObject(Sender: TObject);
var
  BoldObject: TBoldObject;
begin
  if (CurrentPopupComponent is TBoldListBox) and
     Assigned(TBoldListBox(CurrentPopupComponent).BoldHandle) then
    BoldObject := TBoldListBox(CurrentPopupComponent).BoldHandle.CurrentBoldObject
  else if (CurrentPopupComponent is TBoldGrid) and
          Assigned(TBoldGrid(CurrentPopupComponent).BoldHandle) then
    BoldObject := TBoldGrid(CurrentPopupComponent).BoldHandle.CurrentBoldObject
  else
    BoldObject := nil;

  if assigned(BoldObject) then
    BoldObject.Delete;
end;

procedure Tallform.Remove(Sender: TObject);
begin
  with Currentpopupcomponent as TBoldListBox do
    if Assigned(BoldHandle) then
      BoldHandle.RemoveCurrentElement;
end;

procedure Tallform.btnUpdateDBClick(Sender: TObject);
begin
  DMSystem.SystemHandle.UpdateDatabase;
end;

procedure Tallform.SingleItemRemove(Sender: TObject);
begin
  with Currentpopupcomponent as TBoldEdit do
   if BoldHandle.Value is TBoldObjectReference then
     TBoldObjectReference(BoldHandle.Value).BoldObject := nil;
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
  if not DMSystem.SystemHandle.Active then
    Exit;
  if (DMSystem.SystemHandle.System.BoldDirty) then
    if Messagedlg('You have unsaved changes, quit anyway?',
                  mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      DMSystem.SystemHandle.System.Discard
    else
      Canclose := False;
end;

procedure Tallform.ShowInOwnWindow(Sender: TObject);
var
  BoldObject: TBoldObject;
begin
   if (CurrentPopupComponent is TBoldListBox) and
    Assigned(TBoldListBox(CurrentPopupComponent).BoldHandle) then
    BoldObject := TBoldListBox(CurrentPopupComponent).BoldHandle.CurrentBoldObject
  else  if (CurrentPopupComponent is TBoldGrid) and
    Assigned(TBoldGrid(CurrentPopupComponent).BoldHandle) then
    BoldObject := TBoldGrid(CurrentPopupComponent).BoldHandle.CurrentBoldObject
  else
    BoldObject := nil;

  if assigned(BoldObject) then
    AutoFormProviderRegistry.FormForElement(BoldObject).Show;
end;

function Tallform.ReadOnlyRendererObserverMayModify(
  Element: TBoldElement; representation: Integer; Expression: String;
  Subscriber: TBoldSubscriber): Boolean;
begin
  Result := True;
end;

function Tallform.RendPerResidentRendererObserverMayModify(
  Element: TBoldElement; representation: Integer; Expression: String;
  Subscriber: TBoldSubscriber): Boolean;
begin
  result := false;
  if element is TResidential_Building then
    with Element as TResidential_Building do
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
  if blhAllResidentialBuilding.CurrentBoldObject is TResidential_Building then
    (blhAllResidentialBuilding.CurrentBoldObject as TResidential_Building).ChargeRent;
end;

procedure Tallform.CheckBox1Click(Sender: TObject);
begin
{$IFNDEF BOLDCOMCLIENT}
  if CheckBox1.Checked then
    blhAllPerson.BoldFilter := DMSystem.IsRichFilter
  else
    blhAllPerson.BoldFilter := nil;
{$ENDIF}
end;

procedure Tallform.CheckBox2Click(Sender: TObject);
begin
{$IFNDEF BOLDCOMCLIENT}
   if CheckBox2.Checked then
    blhAllPerson.BoldComparer := DMSystem.NameComparer
  else
    blhAllPerson.BoldComparer := nil;
{$ENDIF}
end;

procedure Tallform.NewBuilding1Click(Sender: TObject);
begin
  TBuilding.Create(nil);
end;

function Tallform.CurrentBuildingHandle: TBoldListHandle;

begin
  if PageControl1.ActivePage = tabBuilding then
    result := blhAllBuilding
  else
    result := blhAllResidentialBuilding;
end;

procedure Tallform.EnsureObjects;
begin
  Randomize;
  (blhAllPerson.list as TBoldObjectList).EnsureObjects;
  (blhAllBuilding.list as TBoldObjectList).EnsureObjects;
end;

procedure Tallform.BoldSystemActivator1SystemActivated(Sender: TObject);
begin
  EnsureObjects;
end;

end.


