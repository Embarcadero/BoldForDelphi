
unit joho_ClientGuiAndHandles;

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
  StdCtrls,
  ComCtrls,
  ExtCtrls,

  // General bold units
  BoldDefs,
  BoldSubscription,


  // Connection handles
  BoldClientHandles,
  BoldComServerHandles,
  BoldComServerElementHandles,

  // COM aware object space
  BoldComObjectSpace_TLB,
  BoldComClient,

  // Bold handles
  BoldHandle,
  BoldSystemHandle,
  BoldRootedHandles,
  BoldAbstractListHandle,
  BoldCursorHandle,
  BoldListHandle,
  BoldHandles,

  // Persistence handles
  BoldPersistenceHandle,
  BoldPersistenceHandleDB,

  //COM Handles
  BoldComClientHandles,
  BoldHandlesCom,
  BoldSystemHandleCom,
  BoldRootedHandlesCom,
  BoldCursorHandleCom,
  BoldListHandleCom,

  // Bold components
  BoldEdit,
  BoldAbstractListHandleCom,
  BoldAFPPluggable,
  BoldServerHandles,
  BoldListBox,

  // Utility components

  // COM Components
  BoldEditCom,
  BoldListBoxCom,
  BoldLabelCom,
  BoldComboBoxCom,
  BoldTreeViewCom,
  BoldTrackBarCom,
  BoldCheckBoxCom,
  BoldCaptionControllerCom,
  BoldProgressBarCom,
  BoldNavigatorCom,
  BoldMemoCom,
  BoldGridCom,

  // COM Controlpacks
  BoldControlPackCom,
  BoldStringControlPackCom,
  BoldCheckboxStateControlPackCom,
  BoldNumericControlPackCom, Grids,
  // businessClasses
  TestModel1, BoldNavigatorDefs, BoldVariableHandleCom, BoldVariableHandle,
  BoldAbstractPersistenceHandleDB, BoldPersistenceHandleDB_deprecated;

type
  Tjoho_ComGuiForm = class(TForm)
    Core: TGroupBox;
    BoldSystemHandle1: TBoldSystemHandle;
    blhClasses: TBoldListHandle;
    BoldListBox1: TBoldListBox;
    LblClasses: TLabel;
    GroupBox1: TGroupBox;
    BoldComServerHandle1: TBoldComServerHandle;
    BoldComServerElementHandle1: TBoldComServerElementHandle;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    blhLongHitLists: TBoldListHandle;
    blhSongs: TBoldListHandle;
    bcehLongHitLists: TBoldComServerElementHandle;
    bcehSongs: TBoldComServerElementHandle;
    BoldListBox2: TBoldListBox;
    BoldListBox3: TBoldListBox;
    BoldPlaceableAFP1: TBoldPlaceableAFP;
    bchcLongHitLists: TBoldCursorHandleCom;
    GroupBox5: TGroupBox;
    BoldComConnectionHandle1: TBoldComConnectionHandle;
    BoldSystemHandleCom1: TBoldSystemHandleCom;
    BoldListBoxCom1: TBoldListBoxCom;
    bchcSongs: TBoldCursorHandleCom;
    BoldListBoxCom2: TBoldListBoxCom;
    BoldEditCom1: TBoldEditCom;
    BoldEditCom2: TBoldEditCom;
    BoldEdit1: TBoldEdit;
    BoldEdit2: TBoldEdit;
    GroupBox6: TGroupBox;
    ClientOwnedHitLists: TBoldListHandleCom;
    BoldListBoxCom3: TBoldListBoxCom;
    BoldListBoxCom4: TBoldListBoxCom;
    BoldEditCom3: TBoldEditCom;
    BoldEditCom4: TBoldEditCom;
    ClientOwnedSongs: TBoldListHandleCom;
    Button1: TButton;
    BoldTrackBarCom1: TBoldTrackBarCom;
    BoldTreeViewCom1: TBoldTreeViewCom;
    BoldComboBoxCom1: TBoldComboBoxCom;
    BoldLabelCom1: TBoldLabelCom;
    blhcAllClassA: TBoldListHandleCom;
    BoldListBoxCom5: TBoldListBoxCom;
    BoldLabelCom2: TBoldLabelCom;
    BoldCheckBoxCom1: TBoldCheckBoxCom;
    BoldCaptionControllerCom1: TBoldCaptionControllerCom;
    Edit1: TEdit;
    BoldProgressBarCom1: TBoldProgressBarCom;
    BoldMemoCom1: TBoldMemoCom;
    GroupBox7: TGroupBox;
    bceRenderedString: TBoldEditCom;
    BoldAsStringRendererCom1: TBoldAsStringRendererCom;
    BoldAsCheckBoxStateRendererCom1: TBoldAsCheckBoxStateRendererCom;
    bccRenderedCheckbox: TBoldCheckBoxCom;
    BoldAsIntegerRendererCom1: TBoldAsIntegerRendererCom;
    cpbRenderedInteger: TBoldProgressBarCom;
    BoldGridCom1: TBoldGridCom;
    BoldNavigatorCom1: TBoldNavigatorCom;
    BoldVariableHandleCom2: TBoldVariableHandleCom;
    bedcDynamicVariableHandle: TBoldEditCom;
    BoldVariableHandleCom1: TBoldVariableHandleCom;
    BoldEditCom5: TBoldEditCom;
    bedcStaticVariableHandle: TBoldEditCom;
    BoldComServerElementHandle2: TBoldComServerElementHandle;
    BoldVariableHandle1: TBoldVariableHandle;
    procedure Button1Click(Sender: TObject);
    procedure BoldAsStringRendererCom1Subscribe(Element: IBoldElement;
      Representation: Integer; Expression: String;
      Subscriber: TBoldComClientSubscriber);
    function BoldAsStringRendererCom1GetAsString(Element: IBoldElement;
      Representation: Integer; Expression: String): String;
    procedure BoldAsCheckBoxStateRendererCom1Subscribe(
      Element: IBoldElement; Representation: Integer; Expression: String;
      Subscriber: TBoldComClientSubscriber);
    function BoldAsCheckBoxStateRendererCom1GetAsCheckBoxState(
      Element: IBoldElement; Representation: Integer;
      Expression: String): TCheckBoxState;
    procedure BoldAsIntegerRendererCom1Subscribe(Element: IBoldElement;
      Representation: Integer; Expression: String;
      Subscriber: TBoldComClientSubscriber);
    function BoldAsIntegerRendererCom1GetAsInteger(Element: IBoldElement;
      Representation: Integer; Expression: String): Integer;
  private
    { Private declarations }
  public
    SwedenTop3,
    SwedenTop5,
    UKTop5: THitList;
    Ooops,
    AreYouStillHavingFun,
    MeraMal,
    Bound4DaReload,
    HeatOfAsia,
    TocasMiracle,
    BadTouch: TSong;
    procedure InitializeObjects;

    { Public declarations }
  end;


function GUI: TJoho_ComGuiForm;
procedure FreeGUI;

implementation

uses
  dmModel1,
  joho_ClientGuiAndHandles_ServerCode;

var
  fGui: Tjoho_ComGuiForm;

function GUI: TJoho_ComGuiForm;
begin
  if not assigned( fGui ) then
  begin
    fGui := Tjoho_ComGuiForm.Create(nil);
    fGui.InitializeObjects;
    fGui.Show;
  end;
  result := fGui;
end;

procedure FreeGUI;
begin
  FreeAndNil(fGUI);
end;

{$R *.DFM}

procedure Tjoho_ComGuiForm.Button1Click(Sender: TObject);
begin
  BoldComConnectionHandle1.Connected := true;
end;

procedure Tjoho_ComGuiForm.BoldAsStringRendererCom1Subscribe(
  Element: IBoldElement; Representation: Integer; Expression: String;
  Subscriber: TBoldComClientSubscriber);
begin
  Element.SubscribeToExpression('name', Subscriber.ClientId, Subscriber.SubscriberId, False, True);
end;

function Tjoho_ComGuiForm.BoldAsStringRendererCom1GetAsString(
  Element: IBoldElement; Representation: Integer;
  Expression: String): String;
begin
  Result := '';
  if Assigned(Element) then
    Result := Element.EvaluateExpressionAsString('name', brDefault) + Element.EvaluateExpressionAsString('name', brDefault);
end;

procedure Tjoho_ComGuiForm.BoldAsCheckBoxStateRendererCom1Subscribe(
  Element: IBoldElement; Representation: Integer; Expression: String;
  Subscriber: TBoldComClientSubscriber);
begin
  Element.SubscribeToExpression('title', Subscriber.ClientID, Subscriber.SubscriberID, False, True);
end;

function Tjoho_ComGuiForm.BoldAsCheckBoxStateRendererCom1GetAsCheckBoxState(
  Element: IBoldElement; Representation: Integer;
  Expression: String): TCheckBoxState;
begin
  Result := cbGrayed;
  if Assigned(Element) then
    if Element.EvaluateExpressionAsString('title', brDefault) = 'Bad Touch' then
      Result := cbChecked
    else
      Result := cbUnchecked;
end;

procedure Tjoho_ComGuiForm.BoldAsIntegerRendererCom1Subscribe(
  Element: IBoldElement; Representation: Integer; Expression: String;
  Subscriber: TBoldComClientSubscriber);
begin
  Element.SubscribeToExpression('aInteger', Subscriber.ClientID, Subscriber.SubscriberID, False, True);
end;

function Tjoho_ComGuiForm.BoldAsIntegerRendererCom1GetAsInteger(
  Element: IBoldElement; Representation: Integer;
  Expression: String): Integer;
begin
  Result := 0;

  if Assigned(Element) then
    Result := StrToInt(Element.EvaluateExpressionAsString('aInteger', brDefault)) * 2;
end;

procedure Tjoho_ComGuiForm.InitializeObjects;
begin
  if BoldSystemHandle1.Active then
  begin
    BoldSystemHandle1.system.Discard;
    BoldSystemHandle1.Active := false;
  end;
  BoldSystemHandle1.Active := true;

  Ooops := TSong.Create(BoldSystemHandle1.System);
  Ooops.Title := 'Oops... I did it again';
  AreYouStillHavingFun := TSong.Create(BoldSystemHandle1.System);
  AreYouStillHavingFun.Title := 'Are You Still Having Fun';
  MeraMal := TSong.Create(BoldSystemHandle1.System);
  MeraMal.Title := 'Mera Mål';
  Bound4DaReload := TSong.Create(BoldSystemHandle1.System);
  Bound4DaReload.Title := 'Bound 4 da reload';
  HeatOfAsia := TSong.Create(BoldSystemHandle1.System);
  HeatOfAsia.Title := 'Heat of Asia';
  TocasMiracle := TSong.Create(BoldSystemHandle1.System);
  TocasMiracle.Title := 'Toca''s Miracle';
  BadTouch := TSong.Create(BoldSystemHandle1.System);
  BadTouch.Title := 'Bad Touch';

  SwedenTop3 := THitList.Create(BoldSystemHandle1.System);
  SwedenTop3.Name := 'Sweden HitList';
  SwedenTop3.song.add( Ooops );
  SwedenTop3.song.add( AreYouStillHavingFun );
  SwedenTop3.song.add( MeraMal );

  UKTop5 := THitList.Create(BoldSystemHandle1.System);
  UKTop5.Name := 'UK Top 5';
  UKTop5.song.Add( Ooops );
  UKTop5.song.Add( Bound4DaReload);
  UKTop5.song.Add( HeatOfAsia );
  UKTop5.song.Add( TocasMiracle );
  UKTop5.song.Add( BadTouch );

  SwedenTop5 := THitList.Create(BoldSystemHandle1.System);
  SwedenTop5.Name := 'Sweden HitList 5';
  SwedenTop5.song.add( Ooops );
  SwedenTop5.song.add( AreYouStillHavingFun );
  SwedenTop5.song.add( MeraMal );
  SwedenTop5.song.add( HeatOfAsia );
  SwedenTop5.song.add( BadTouch );

  TClassA.Create(BoldSystemHandle1.System).Parent := TclassA.Create(BoldSystemHandle1.System);
  TClassA.Create(BoldSystemHandle1.System);
end;

end.
