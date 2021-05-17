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
  Actions,
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
  BoldAFP,
  BoldReferenceHandle,
  BoldNavigatorDefs,
  BoldDebugActions,
  BoldUndoActions,
  BoldCaptionController,
  BoldAction;

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
    Button2: TButton;
    BoldUpdateDBAction1: TBoldUpdateDBAction;
    HighRentRenderer: TBoldAsStringRenderer;
    btnCheckpoint: TButton;
    BoldCreateDatabaseAction1: TBoldCreateDatabaseAction;
    GroupBox2: TGroupBox;
    btnUnDo: TButton;
    btnRedo: TButton;
    GroupBox3: TGroupBox;
    btnUpdateDB: TButton;
    BoldFailureDetectionAction1: TBoldFailureDetectionAction;
    BoldUndoAction1: TBoldUndoAction;
    BoldRedoAction1: TBoldRedoAction;
    BoldSystemDebuggerAction1: TBoldSystemDebuggerAction;
    BoldNavigator1: TBoldNavigator;
    BoldNavigator2: TBoldNavigator;
    BoldNavigator3: TBoldNavigator;
    StatusBar1: TStatusBar;
    MainMenu1: TMainMenu;
    Opensystem1: TMenuItem;
    CreateDB1: TMenuItem;
    Opensystem2: TMenuItem;
    UpdateDB1: TMenuItem;
    ogglelog1: TMenuItem;
    Systemdebugger1: TMenuItem;
    Edit1: TMenuItem;
    Undo1: TMenuItem;
    Redo1: TMenuItem;
    BoldModelEditorAction: TAction;
    BoldModelEditorAction1: TMenuItem;
    Log1: TMenuItem;
    BoldLogOCLAction1: TBoldLogOCLAction;
    BoldLogSQLAction1: TBoldLogSQLAction;
    BoldLogPMAction1: TBoldLogPMAction;
    oggleOCLlogs1: TMenuItem;
    ogglePMCallslogs1: TMenuItem;
    oggleSQLlogs1: TMenuItem;
    BoldSetCheckPointAction1: TBoldSetCheckPointAction;
    BoldCaptionController1: TBoldCaptionController;
    actChargeRent: TBoldAction;
    BoldLogFormAction1: TBoldLogFormAction;
    ListBox1: TListBox;
    Label8: TLabel;
    procedure newBuildingClick(Sender: TObject);
    procedure DeleteCurrentObject(Sender: TObject);
    procedure NewPersonClick(Sender: TObject);
    procedure Remove(Sender: TObject);
    procedure SingleItemRemove(Sender: TObject);
    procedure PopupPopup(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ShowInOwnWindow(Sender: TObject); procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure NewBuilding1Click(Sender: TObject);
    function bsrRentPerResidentGetAsString(aFollower: TBoldFollower): string;
    procedure bsrRentPerResidentHoldsChangedValue(aFollower: TBoldFollower);
    function bsrRentPerResidentMayModify(aFollower: TBoldFollower): Boolean;
    procedure bsrRentPerResidentReleaseChangedValue(aFollower: TBoldFollower);
    procedure bsrRentPerResidentSetAsString(aFollower: TBoldFollower;
      const NewValue: string);
    procedure rgNameRepresentationClick(Sender: TObject);
    procedure bsrRentPerResidentSubscribe(aFollower: TBoldFollower;
      Subscriber: TBoldSubscriber);
    function bsrRentPerResidentValidateCharacter(aFollower: TBoldFollower;
      const Value: string): Boolean;
    function bsrRentPerResidentValidateString(aFollower: TBoldFollower;
      const Value: string): Boolean;
    function bsrResidentsTotalAssetsGetAsString(
      aFollower: TBoldFollower): string;
    procedure bsrResidentsTotalAssetsSubscribe(aFollower: TBoldFollower;
      Subscriber: TBoldSubscriber);
    procedure bsrAddressSetColor(aFollower: TBoldFollower; var AColor: TColor);
    procedure bsrAddressSetFont(aFollower: TBoldFollower; AFont: TFont);
    procedure HighRentRendererSetColor(aFollower: TBoldFollower;
      var AColor: TColor);
    procedure HighRentRendererSetFont(aFollower: TBoldFollower; AFont: TFont);
    procedure CheckBox3Click(Sender: TObject);
    procedure BoldModelEditorActionExecute(Sender: TObject);
    procedure actChargeRentExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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
  PersonAutoFormUnit,
  datamod,
  BoldUMLModelEdit;

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

procedure Tallform.FormCreate(Sender: TObject);
begin
  Randomize;
  datamodule1.BoldSystemHandle1.System.UndoHandlerInterface.Enabled := true;
  BoldCaptionController1.TrackControl := self;
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

function Tallform.bsrRentPerResidentGetAsString(
  aFollower: TBoldFollower): string;
begin
  result := '';
  if AFollower.element is TResidential_Building then
    with TResidential_Building(AFollower.Element) do
      if M_TotalRent.IsNull then
        Result := '***'
      else if Residents.Count = 0 then
        Result := 'No Residents'
      else
         Result := CurrToStr(TotalRent/Residents.Count);
end;

procedure Tallform.bsrRentPerResidentHoldsChangedValue(
  aFollower: TBoldFollower);
begin
  if AFollower.element is TResidential_Building then
    TResidential_Building(AFollower.Element).M_TotalRent.RegisterModifiedValueHolder(AFollower.subscriber);
end;

function Tallform.bsrRentPerResidentMayModify(
  aFollower: TBoldFollower): Boolean;
begin
  Result := False;
  if AFollower.element is TResidential_Building then
    Result := TResidential_Building(AFollower.element).Residents.Count > 0;
end;

procedure Tallform.bsrRentPerResidentReleaseChangedValue(
  aFollower: TBoldFollower);
begin
  if AFollower.element is TResidential_Building then
    TResidential_Building(AFollower.Element).M_TotalRent.UnRegisterModifiedValueHolder(AFollower.Subscriber);
end;

procedure Tallform.bsrRentPerResidentSetAsString(aFollower: TBoldFollower;
  const NewValue: string);
var
  v: string;
begin
  v := NewValue; {avoid name clash}
  if aFollower.element is TResidential_Building then
    with TResidential_Building(aFollower.Element) do
      TotalRent := StrToCurr(v) * Residents.Count;
end;

procedure Tallform.bsrRentPerResidentSubscribe(aFollower: TBoldFollower;
  Subscriber: TBoldSubscriber);
begin
  if AFollower.element is TResidential_Building then
    with AFollower.Element do
    begin
      SubscribeToExpression('totalRent', Subscriber, False);
      SubscribeToExpression('residents', Subscriber, False);
    end;
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

procedure Tallform.CheckBox3Click(Sender: TObject);
begin
  datamodule1.BoldSystemHandle1.System.UndoHandlerInterface.Enabled := (Sender as TCheckBox).Checked;
end;

function Tallform.bsrRentPerResidentValidateCharacter(aFollower: TBoldFollower;
  const Value: string): Boolean;
begin
  Result := value[1] in ['0'..'9', '-', '+', 'e', 'E', FormatSettings.DecimalSeparator];
end;

function Tallform.bsrRentPerResidentValidateString(aFollower: TBoldFollower;
  const Value: string): Boolean;
begin
  try
    StrToCurr(value);
    Result := True;
  except
    Result := False;
  end;
end;

function Tallform.bsrResidentsTotalAssetsGetAsString(
  aFollower: TBoldFollower): string;
var
  i: integer;
  sum: Currency;
begin
  Sum := 0;
  if AFollower.element is TResidential_Building then
    with TResidential_Building(AFollower.Element)  do
      for i := 0 to Residents.Count - 1 do
        Sum := Sum + Residents[i].Assets;
  Result := CurrToStr(Sum);
end;

procedure Tallform.bsrResidentsTotalAssetsSubscribe(aFollower: TBoldFollower;
  Subscriber: TBoldSubscriber);
var
  i: integer;
begin
  if AFollower.element is TResidential_Building then
    with TResidential_Building(AFollower.Element) do
    begin
      SubscribeToExpression('residents', subscriber, true);
      for i := 0 to Residents.Count - 1 do
        Residents[i].SubscribeToExpression('assets', subscriber, false);
    end;
end;

procedure Tallform.BoldModelEditorActionExecute(Sender: TObject);
begin
  BoldUMLModelEdit.UMLModelEditor.ShowEditFormForBoldModel(DataModule1.BoldModel1);
end;

procedure Tallform.bsrAddressSetColor(aFollower: TBoldFollower;
  var AColor: TColor);
begin
  if Assigned(AFollower.element) then
    with TBuilding(AFollower.element) do
    begin
      if Pos('Bold', Address) > 0 then
        aColor := clAqua;
    end;
end;

procedure Tallform.bsrAddressSetFont(aFollower: TBoldFollower; AFont: TFont);
begin
  if Assigned(AFollower.element) then
    with TBuilding(AFollower.element).M_Address do
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

procedure Tallform.actChargeRentExecute(Sender: TObject);
begin
  if blhAllResidentialBuilding.CurrentBoldObject is TResidential_Building then
    TResidential_Building(blhAllResidentialBuilding.CurrentBoldObject).ChargeRent;
end;

procedure Tallform.HighRentRendererSetColor(aFollower: TBoldFollower;
  var AColor: TColor);
begin
  if AFollower.element is TResidential_Building then
  begin
    if (TResidential_Building(AFollower.element).totalRent) >= 1500 then
      aColor := clSilver
    else
      aColor := clWhite;
  end;
end;

procedure Tallform.HighRentRendererSetFont(aFollower: TBoldFollower;
  AFont: TFont);
begin
  if aFollower.element is TResidential_Building then
  begin
    if (TResidential_Building(aFollower.element).totalRent) >= 1500 then
      AFont.Color := clRed
    else
      AFont.Color := clWindowText;
  end;
end;

end.


