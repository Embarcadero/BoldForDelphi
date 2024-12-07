
{ Global compiler directives }
{$include bold.inc}
unit BoldUMLModelValidationFormCx;

interface

uses
  Windows,
  Messages,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  Grids,
  ComCtrls,
  ClipBrd,
  ToolWin,
  Classes,
  BoldListHandle,
  Boldhandles,
  BoldUMLAttributes,
  BoldUMLModel,
  BoldSubscription,
  BoldReferenceHandle,
  BoldRootedHandles,
  BoldAbstractListHandle,
  BoldCursorHandle,
  BoldControlPack,
  BoldStringcontrolPack,
  BoldSystem,
  BoldModel,
  BoldElements,
  BoldPlaceableSubscriber,
  Menus,
  ExtCtrls,
  ImgList, System.ImageList, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, cxStyles, cxCustomData, cxFilter, cxData,
  cxDataStorage, cxEdit, cxNavigator, dxDateRanges, dxScrollbarAnnotations,
  cxTextEdit, cxDropDownEdit, cxGridCustomTableView, cxGridTableView,
  cxGridBoldSupportUnit, cxGridCustomView, cxClasses, cxGridLevel, cxGrid,
  System.Actions, Vcl.ActnList, Vcl.StdActns, BoldExpressionHandle;

type
  TBoldUMLElementClickedEvent = procedure (element: TUMLModelElement) of object;
  TBoldValidationCallBack = procedure (ModelHandle: TBoldModel) of object;

  TfrmValidationCx = class(TForm)
    behModel: TBoldReferenceHandle;
    blhViolations: TBoldListHandle;
    StatusBar1: TStatusBar;
    ToolBar1: TToolBar;
    btReCheck: TToolButton;
    PopupMenuValidation: TPopupMenu;
    mnuCopyLogtoClipBoard: TMenuItem;
    N1: TMenuItem;
    ilCutCopyPaste: TImageList;
    BoldPlaceableSubscriber1: TBoldPlaceableSubscriber;
    glViolations: TcxGridLevel;
    ViolationsGrid: TcxGrid;
    tvViolations: TcxGridBoldTableView;
    tvViolationsDescription: TcxGridBoldColumn;
    tvViolationsSeverity: TcxGridBoldColumn;
    tvViolationsModelElement: TcxGridBoldColumn;
    tvViolationsClass: TcxGridBoldColumn;
    actionListValidation: TActionList;
    actCopyValidation: TEditCopy;
    ToolButton1: TToolButton;
    actRecheck: TAction;
    Recheck1: TMenuItem;
    Timer1: TTimer;
    behViolations: TBoldExpressionHandle;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure mnuCopyLogtoClipBoardClick(Sender: TObject);
    procedure BoldPlaceableSubscriber1Receive(sender: TBoldPlaceableSubscriber;
      Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    procedure tvViolationsDblClick(Sender: TObject);
    procedure actCopyValidationExecute(Sender: TObject);
    procedure actRecheckExecute(Sender: TObject);
    procedure actRecheckUpdate(Sender: TObject);
    procedure tvViolationsCustomDrawCell(Sender: TcxCustomGridTableView;
      ACanvas: TcxCanvas; AViewInfo: TcxGridTableDataCellViewInfo;
      var ADone: Boolean);
    procedure actCopyValidationUpdate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FOnElementClick: TBoldUMLElementClickedEvent;
    fValidationProc: TBoldValidationCallBack;
    fBoldModel: TBoldModel;
    function GetUMLModel: TUMLModel;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor CreateWithModel(ModelComponent: TBoldModel; Owner: TComponent);
    destructor Destroy; override;
    procedure Validate;
    property BoldModel: TBoldModel read fBoldModel;
    property UmlModel: TUMLModel read GetUMLModel;
    property OnElementClick: TBoldUMLElementClickedEvent read FOnElementClick;
    property ValidationProc: TBoldValidationCallBack read fValidationProc write fValidationProc;
  end;

function EnsureValidationForm(ABoldModel: TBoldModel; Owner: TComponent; OnElementClick: TBoldUMLElementClickedEvent): TfrmValidationCx;

var
  G_ValidationFormDefaultOwner: TComponent;

implementation

uses
  SysUtils,
  BoldQueue,
  BoldEnvironment,
  BoldUtils,
  BoldDefs;

var
  G_ValidationForm: TfrmValidationCx;

{$R *.dfm}

function EnsureValidationForm(ABoldModel: TBoldModel; Owner: TComponent; OnElementClick: TBoldUMLElementClickedEvent): TfrmValidationCx;
begin
  if assigned(G_ValidationForm) and (G_ValidationForm.BoldModel  <> ABoldModel) then
  begin
    G_ValidationForm.Release;
    G_ValidationForm := nil;
  end;
  if not Assigned(G_ValidationForm) then
    G_ValidationForm := TfrmValidationCx.CreateWithModel(ABoldModel, Owner);
  Result := G_ValidationForm;
  if assigned(OnElementClick) then
    result.fOnElementClick := OnElementclick;
end;

procedure TfrmValidationCx.tvViolationsCustomDrawCell(
  Sender: TcxCustomGridTableView; ACanvas: TcxCanvas;
  AViewInfo: TcxGridTableDataCellViewInfo; var ADone: Boolean);
begin
  if AViewInfo.GridRecord.Values[1] = 'Error' then
    ACanvas.SetBrushColor($008080FF);
end;

procedure TfrmValidationCx.tvViolationsDblClick(Sender: TObject);
begin
  if (assigned(OnElementClick)) and (Assigned(blhViolations.CurrentBoldObject)) then
    OnElementClick((blhViolations.CurrentBoldObject as TViolation).modelElement);
end;

procedure TfrmValidationCx.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Hide;
  Action := caHide;
end;

procedure TfrmValidationCx.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = vk_f5 then
    Validate;
end;

procedure TfrmValidationCx.actCopyValidationExecute(Sender: TObject);
begin
  with tvViolations.DataController do
    Clipboard.AsText := Values[FocusedRecordIndex, 0];
end;

procedure TfrmValidationCx.actCopyValidationUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := tvViolations.DataController.RecordCount > 0;
end;

procedure TfrmValidationCx.actRecheckExecute(Sender: TObject);
begin
  Validate;
end;

procedure TfrmValidationCx.actRecheckUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(ValidationProc) and Assigned(BoldModel);
end;

procedure TfrmValidationCx.BoldPlaceableSubscriber1Receive(
  sender: TBoldPlaceableSubscriber; Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  Timer1.Enabled := true;
end;

procedure TfrmValidationCx.Validate;
begin
  if Assigned(ValidationProc) then
    ValidationProc(BoldModel)
  else
    raise EBold.Create('No validator registered');

  if UMLModel.Validator.HighestSeverity = sNone then
    ShowMessage('Model validated OK!');
end;

constructor TfrmValidationCx.CreateWithModel(ModelComponent: TBoldModel; Owner: Tcomponent);
begin
  inherited create(Owner);
  fBoldModel := ModelComponent;
  fBoldModel.FreeNotification(Self);
  behModel.Value := ModelComponent.EnsuredUMLModel;
  behModel.StaticSystemHandle := ModelComponent.SystemHandle;
  BoldPlaceableSubscriber1.BoldHandle := behViolations;
end;

procedure TfrmValidationCx.mnuCopyLogtoClipBoardClick(Sender: TObject);
begin
  tvViolations.CopyToClipboard(True);
end;

function TfrmValidationCx.GetUMLModel: TUMLModel;
begin
  if Assigned(BoldModel) then
    result := BoldModel.EnsuredUMLModel
  else
    result := nil;
end;

destructor TfrmValidationCx.Destroy;
begin
  if G_ValidationForm = self then
    G_ValidationForm := nil;
  inherited;
end;

procedure TfrmValidationCx.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (aComponent = BoldModel) and (operation = opRemove) then
    fBoldModel := nil;
end;

procedure TfrmValidationCx.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := false;
  visible := (blhViolations.Count > 0);
end;

end.
