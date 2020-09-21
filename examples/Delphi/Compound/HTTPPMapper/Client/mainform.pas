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
  ActnList,
  BoldSubscription,
  BoldElements,
  BoldSystem,
  BoldHandles,
  BoldListHandle,
  BoldAbstractListHandle,
  BoldCursorHandle,
  BoldExpressionHandle,
  BoldRootedHandles,
  BoldPersistenceNotifier,
  BoldAFP,
  BoldAFPDefault,
  BoldControlPack,
  BoldStringControlPack,
  BoldCheckBox,
  BoldGrid,
  BoldEdit,
  BoldNavigator,
  BoldLabel,
  BoldListBox,
  BoldHandleAction,
  BoldActions,
  BoldDBActions,
  BoldAFPPluggable,
  BoldReferenceHandle
  ;

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
    bsrRentPerResident: TBoldAsStringRenderer;
    bedAssets: TBoldEdit;
    Label7: TLabel;
    bcbRich: TBoldCheckBox;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    bsrResidentsTotalAssets: TBoldAsStringRenderer;
    bsrAddress: TBoldAsStringRenderer;
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
    ActionList1: TActionList;
    BoldActivateSystemAction1: TBoldActivateSystemAction;
    Button1: TButton;
    BoldUpdateDBAction1: TBoldUpdateDBAction;
    HighRentRenderer: TBoldAsStringRenderer;
    BoldPlaceableAFP1: TBoldPlaceableAFP;
    procedure newBuildingClick(Sender: TObject);
    procedure DeleteCurrentObject(Sender: TObject);
    procedure NewPersonClick(Sender: TObject);
    procedure Remove(Sender: TObject);
    procedure SingleItemRemove(Sender: TObject);
    procedure PopupPopup(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ShowInOwnWindow(Sender: TObject);
    function ReadOnlyRendererObserverMayModify(
      Element: TBoldElement; representation: Integer;
      Expression: String; Subscriber: TBoldSubscriber): Boolean;
    function bsrRentPerResidentGetAsString(Element: TBoldElement;
      representation: Integer; Expression: String): string;
    function RendPerResidentRendererObserverMayModify(
      Element: TBoldElement; representation: Integer;
      Expression: String; Subscriber: TBoldSubscriber): Boolean;
    procedure bsrRentPerResidentHoldsChangedValue(
      Element: TBoldElement; representation: Integer;
      Expression: String; Subscriber: TBoldSubscriber);
    procedure bsrRentPerResidentReleaseChangedValue(
      Element: TBoldElement; representation: Integer;
      Expression: String; Subscriber: TBoldSubscriber);
    procedure bsrRentPerResidentSubscribe(Element: TBoldElement;
      representation: Integer; Expression: String;
      Subscriber: TBoldSubscriber);
    procedure bsrRentPerResidentSetAsString(Element: TBoldElement;
      value: string; representation: Integer; Expression: String);
    procedure rgNameRepresentationClick(Sender: TObject);
    procedure btnChargeRentClick(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    function bsrRentPerResidentMayModify(element: TBoldElement;
      representation: Integer; Expression: String;
      Subscriber: TBoldSubscriber): Boolean;
    function bsrRentPerResidentValidateCharacter(
      element: TBoldElement; value: String; representation: Integer;
      Expression: String): Boolean;
    function bsrRentPerResidentValidateString(
      element: TBoldElement; value: String; representation: Integer;
      Expression: String): Boolean;
    function bsrResidentsTotalAssetsGetAsString(element: TBoldElement;
      representation: Integer; Expression: String): String;
    procedure bsrResidentsTotalAssetsSubscribe(element: TBoldElement;
      representation: Integer; Expression: String;
      Subscriber: TBoldSubscriber);
    procedure bsrAddressSetColor(element: TBoldElement;
      var aColor: TColor; representation: Integer; Expression: String);
    procedure bsrAddressSetFont(element: TBoldElement;
      aFont: TFont; representation: Integer; Expression: String);
    procedure NewBuilding1Click(Sender: TObject);
    procedure BoldActivateSystemAction1SystemOpened(Sender: TObject);
    procedure HighRentRendererSetColor(Element: TBoldElement;
      var AColor: TColor; Representation: Integer; Expression: String);
    procedure HighRentRendererSetFont(Element: TBoldElement; AFont: TFont;
      Representation: Integer; Expression: String);
   { function BoldPlaceableAFP1GetFormClass(
      Element: TBoldElement): TFormClass;
    function BoldPlaceableAFP1RetrieveHandle(
      Form: TForm): TBoldReferenceHandle;}


  private
    { Private declarations }
    function CurrentBuildingHandle: TBoldListHandle;
  public
    { Public declarations }
  end;

var
  allform: Tallform;

implementation

uses
  BuildingClasses,
  BoldGUI,
  datamod;

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
  if not datamodule1.BoldSystemHandle1.Active then
    Exit;
  if (datamodule1.BoldSystemHandle1.System.BoldDirty) then
    if Messagedlg('You have unsaved changes, quit anyway?',
                  mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      datamodule1.BoldSystemHandle1.System.Discard
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

function Tallform.bsrRentPerResidentGetAsString(
  Element: TBoldElement; representation: Integer;
  Expression: String): string;
begin
  result := '';
  if element is TResidential_Building then
    with Element as TResidential_Building do
      if M_TotalRent.IsNull then
        Result := '***'
      else if Residents.Count = 0 then
        Result := 'No Residents'
      else
         Result := CurrToStr(TotalRent/Residents.Count);
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

procedure Tallform.bsrRentPerResidentHoldsChangedValue(
  Element: TBoldElement; representation: Integer; Expression: String;
  subscriber: TBoldsubscriber);
begin
  if element is TResidential_Building then
    with Element as TResidential_Building do
      M_TotalRent.RegisterModifiedValueHolder(subscriber);
end;

procedure Tallform.bsrRentPerResidentReleaseChangedValue(
  Element: TBoldElement; representation: Integer; Expression: String;
  subscriber: TBoldSubscriber);
begin
  if element is TResidential_Building then
    with Element as TResidential_Building do
      M_TotalRent.UnRegisterModifiedValueHolder(Subscriber);
end;

procedure Tallform.bsrRentPerResidentSubscribe(
  Element: TBoldElement; representation: Integer; Expression: String;
  Subscriber: TBoldSubscriber);
begin
  if element is TResidential_Building then
    with Element do
    begin
      SubscribeToExpression('totalRent', Subscriber, False);
      SubscribeToExpression('residents', Subscriber, False);
    end;
end;

procedure Tallform.bsrRentPerResidentSetAsString(
  Element: TBoldElement; value: string; representation: Integer;
  Expression: String);
var
  v: string;
begin
  v := value; {avoid name clash}
  if element is TResidential_Building then
    with Element as TResidential_Building do
      TotalRent := StrToCurr(v) * Residents.Count;
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
  if CheckBox1.Checked then
    blhAllPerson.BoldFilter := datamodule1.IsRichFilter
  else
    blhAllPerson.BoldFilter := nil;
end;

procedure Tallform.CheckBox2Click(Sender: TObject);
begin
   if CheckBox2.Checked then
    blhAllPerson.BoldComparer := datamodule1.NameComparer
  else
    blhAllPerson.BoldComparer := nil;
end;

function Tallform.bsrRentPerResidentMayModify(
  element: TBoldElement; representation: Integer;
  Expression: String; Subscriber: TBoldSubscriber): Boolean;
begin
  Result := False;
  if element is TResidential_Building then
    with element as TResidential_Building do
      Result := Residents.Count > 0;
end;

function Tallform.bsrRentPerResidentValidateCharacter(
  element: TBoldElement; value: String; representation: Integer;
  Expression: String): Boolean;
begin
  Result := value[1] in ['0'..'9', '-', '+', 'e', 'E', DecimalSeparator];
end;

function Tallform.bsrRentPerResidentValidateString(
  element: TBoldElement; value: String; representation: Integer;
  Expression: String): Boolean;
begin
  try
    StrToCurr(value);
    Result := True;
  except
    Result := False;
  end;
end;

function Tallform.bsrResidentsTotalAssetsGetAsString(
  element: TBoldElement; representation: Integer;
  Expression: String): String;
var
  i: integer;
  sum: Currency;
begin
  Sum := 0;
  if element is TResidential_Building then
    with Element as TResidential_Building do
      for i := 0 to Residents.Count-1 do
        Sum := Sum + Residents[i].Assets;
  Result := CurrToStr(Sum);
end;

procedure Tallform.bsrResidentsTotalAssetsSubscribe(element: TBoldElement;
  representation: Integer; Expression: String;
  Subscriber: TBoldSubscriber);
var
  i: integer;
begin
  if element is TResidential_Building then
    with Element as TResidential_Building do
    begin
      SubscribeToExpression('residents', subscriber, true);
      for i := 0 to Residents.Count-1 do
        Residents[i].SubscribeToExpression('assets', subscriber, false);
    end;
end;

procedure Tallform.bsrAddressSetColor(
  element: TBoldElement; var aColor: TColor; representation: Integer;
  Expression: String);
begin
  if Assigned(element) then
    with element as TBuilding do
    begin
      if Pos('Bold', Address) > 0 then
        aColor := clAqua;
    end;
end;

procedure Tallform.bsrAddressSetFont(element: TBoldElement;
  aFont: TFont; representation: Integer; Expression: String);
begin
  if Assigned(element) then
    with (element as TBuilding).M_Address do
    begin
      if Pos( 'Bold', AsString ) > 0 then
        aFont.Style := aFont.Style + [fsBold];
      if Pos( 'Rose', AsString ) > 0 then
        aFont.Color := clRed;
      if Pos( 'Select', AsString ) > 0 then
        aFont.Color := clGreen;
    end;
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

procedure Tallform.BoldActivateSystemAction1SystemOpened(Sender: TObject);
begin
  Randomize;
end;

procedure Tallform.HighRentRendererSetColor(Element: TBoldElement;
  var AColor: TColor; Representation: Integer; Expression: String);
begin
  if element is TResidential_Building then
  begin
    if (TResidential_Building(element).totalRent) >= 1500 then
      aColor := clSilver
    else
      aColor := clWhite;
  end;
end;

procedure Tallform.HighRentRendererSetFont(Element: TBoldElement;
  AFont: TFont; Representation: Integer; Expression: String);
begin
  if element is TResidential_Building then
  begin
    if (TResidential_Building(element).totalRent) >= 1500 then
      AFont.Color := clRed
    else
      AFont.Color := clWindowText;
  end;

end;

{function Tallform.BoldPlaceableAFP1GetFormClass(
  Element: TBoldElement): TFormClass;
begin
   result := nil;
  if (Element is TPerson) then
    Result := TPersonAutoForm;
end;

function Tallform.BoldPlaceableAFP1RetrieveHandle(
  Form: TForm): TBoldReferenceHandle;
begin
  Result := nil;
  if (form is TPersonAutoForm) then
    Result := (Form as TPersonAutoForm).brhPerson;
end;}

end.


