
{ Global compiler directives }
{$include bold.inc}
unit BoldUMLModelValidationForm;

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
  BoldGrid,
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
  ImgList, System.ImageList;

type
  TBoldUMLElementClickedEvent = procedure (sender: TComponent; element: TUMLModelElement) of object;
  TBoldValidationCallBack = procedure (ModelHandle: TBoldModel) of object;

  TfrmValidation = class(TForm)
    BoldGrid1: TBoldGrid;
    behModel: TBoldReferenceHandle;
    blhViolations: TBoldListHandle;
    StatusBar1: TStatusBar;
    ToolBar1: TToolBar;
    btReCheck: TToolButton;
    btStayOnTop: TToolButton;
    PopupMenu1: TPopupMenu;
    mnuCopyLogtoClipBoard: TMenuItem;
    N1: TMenuItem;
    Copy1: TMenuItem;
    Cut1: TMenuItem;
    Paste1: TMenuItem;
    ilCutCopyPaste: TImageList;
    BoldPlaceableSubscriber1: TBoldPlaceableSubscriber;
    procedure BoldGrid1DblClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btStayOnTopClick(Sender: TObject);
    procedure btReCheckClick(Sender: TObject);
    procedure mnuCopyLogtoClipBoardClick(Sender: TObject);
    procedure Cut1Click(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure Paste1Click(Sender: TObject);
    procedure BoldPlaceableSubscriber1Receive(sender: TBoldPlaceableSubscriber;
      Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    procedure FormResize(Sender: TObject);
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

  function EnsureValidationForm(ABoldModel: TBoldModel; Owner: TComponent; OnElementClick: TBoldUMLElementClickedEvent): TfrmValidation;

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
  G_ValidationForm: TfrmValidation;

{$R *.dfm}

function EnsureValidationForm(ABoldModel: TBoldModel; Owner: TComponent; OnElementClick: TBoldUMLElementClickedEvent): TfrmValidation;
begin
  if assigned(G_ValidationForm) and (G_ValidationForm.BoldModel  <> ABoldModel) then
  begin
    G_ValidationForm.Release;
    G_ValidationForm := nil;
  end;
  if not Assigned(G_ValidationForm) then
    G_ValidationForm := TfrmValidation.CreateWithModel(ABoldModel, Owner);
  Result := G_ValidationForm;
  if assigned(OnElementClick) then
    result.fOnElementClick := OnElementclick;
end;

procedure TfrmValidation.BoldGrid1DblClick(Sender: TObject);
begin
  if (assigned(OnElementClick)) and (Assigned(blhViolations.CurrentBoldObject)) then
    OnElementClick(Self, (blhViolations.CurrentBoldObject as TViolation).modelElement);
end;

procedure TfrmValidation.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Hide;
  Action := caHide;
end;

procedure TfrmValidation.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = vk_f5 then
    Validate;
end;

procedure TfrmValidation.FormResize(Sender: TObject);
begin
  btStayOnTop.Visible := Parent = nil;
end;

procedure TfrmValidation.btStayOnTopClick(Sender: TObject);
begin
  if (Sender as TToolButton).Down then
    FormStyle := fsStayOnTop
  else
    FormStyle := fsNormal;
end;

procedure TfrmValidation.BoldPlaceableSubscriber1Receive(
  sender: TBoldPlaceableSubscriber; Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  if blhViolations.Count > 0 then
    show;
end;

procedure TfrmValidation.btReCheckClick(Sender: TObject);
begin
  Validate;
end;

procedure TfrmValidation.Validate;
begin
  if Assigned(ValidationProc) then
    ValidationProc(BoldModel)
  else
    raise EBold.Create('No validator registered');

  if UMLModel.Validator.HighestSeverity = sNone then
    ShowMessage('Model validated OK!');
end;

constructor TfrmValidation.CreateWithModel(ModelComponent: TBoldModel; Owner: Tcomponent);
begin
  inherited create(Owner);
  BoldGrid1.BoldHandle := blhViolations;
  fBoldModel := ModelComponent;
  fBoldModel.FreeNotification(Self);
  behModel.Value := ModelComponent.EnsuredUMLModel;
end;

procedure TfrmValidation.mnuCopyLogtoClipBoardClick(Sender: TObject);
begin
  ClipBoard.AsText := BoldGrid1.AsClipBoardText;
end;

function TfrmValidation.GetUMLModel: TUMLModel;
begin
  if Assigned(BoldModel) then  
    result := BoldModel.EnsuredUMLModel
  else
    result := nil;
end;

procedure TfrmValidation.Cut1Click(Sender: TObject);
begin
  if Assigned(ActiveControl) then
    SendMessage(ActiveControl.handle, WM_CUT, 0, 0);
end;

procedure TfrmValidation.Copy1Click(Sender: TObject);
begin
  if Assigned(ActiveControl) then
    SendMessage(ActiveControl.handle, WM_COPY, 0, 0);
end;

procedure TfrmValidation.Paste1Click(Sender: TObject);
begin
  if Assigned(ActiveControl) then
    SendMessage(ActiveControl.handle, WM_PASTE, 0, 0);
end;

destructor TfrmValidation.Destroy;
begin
  if G_ValidationForm = self then
    G_ValidationForm := nil;
  inherited;
end;

procedure TfrmValidation.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (aComponent = BoldModel) and (operation = opRemove) then
    fBoldModel := nil;
end;

end.
