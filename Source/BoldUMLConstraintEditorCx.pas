
{ Global compiler directives }
{$include bold.inc}
unit BoldUMLConstraintEditorCx;

interface

uses
  Messages,
  Classes,
  Controls,
  Forms,
  ExtCtrls,
  Grids,
  ToolWin,
  ComCtrls,
  StdCtrls,
  Menus,
  ImgList,
  BoldGrid,
  BoldSubscription,
  BoldHandles,
  BoldRootedHandles,
  BoldAbstractListHandle,
  BoldCursorHandle,
  BoldListHandle,
  BoldReferenceHandle,
  BoldUMLModel,
  BoldPropertiesController, System.ImageList, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, cxStyles, cxCustomData, cxFilter,
  cxData, cxDataStorage, cxEdit, cxNavigator, dxDateRanges,
  dxScrollbarAnnotations, Data.DB, cxDBData, cxTextEdit, cxGridCustomTableView,
  cxGridTableView, cxGridBoldSupportUnit, cxGridLevel, cxClasses,
  cxGridCustomView, cxGridDBTableView, cxGrid, cxButtonEdit, System.Actions,
  Vcl.ActnList, BoldEditOCLAction;

type
  TfrmBoldUMLConstraintEditorCx = class(TForm)
    blhElementConstraint: TBoldListHandle;
    ToolBar1: TToolBar;
    BoldGrid1: TBoldGrid;
    brhCurrentElement: TBoldReferenceHandle;
    btAdd: TToolButton;
    btRemove: TToolButton;
    PopupMenu2: TPopupMenu;
    mnuCut: TMenuItem;
    mnuCopy: TMenuItem;
    mnuPaste: TMenuItem;
    BoldPropertiesController1: TBoldPropertiesController;
    ImageList1: TImageList;
    gridConstraintsDBTableView1: TcxGridDBTableView;
    lvConstraints: TcxGridLevel;
    gridConstraints: TcxGrid;
    tvConstraints: TcxGridBoldTableView;
    tvConstraintsConstraints: TcxGridBoldColumn;
    tvConstraintsname: TcxGridBoldColumn;
    tvConstraintsbody: TcxGridBoldColumn;
    ActionList1: TActionList;
    actEditMessage: TAction;
    actEditConstraint: TAction;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btAddClick(Sender: TObject);
    procedure btRemoveClick(Sender: TObject);
    procedure mnuCutClick(Sender: TObject);
    procedure mnuCopyClick(Sender: TObject);
    procedure mnuPasteClick(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure actEditMessageExecute(Sender: TObject);
    procedure actEditConstraintExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    procedure InitElements(CurrentElement: TUMLModelElement);
  end;

  procedure ShowTheConstraintEditor(aUMLElement: TUMLModelElement; Owner: TComponent);
  procedure EnsureFreeConstraintEditor;

implementation

uses
  SysUtils,
  BoldDefs,
  BoldQueue,
  BoldUMLModelSupport,
  BoldOclPropEditor,
  BoldUMLModelEditorHandlesDataModule;

{$R *.dfm}

var
  G_ConstraintEditor: TfrmBoldUMLConstraintEditorCx;

procedure ShowTheConstraintEditor(aUMLElement: TUMLModelElement; Owner: TComponent);
begin
  if not Assigned(G_ConstraintEditor) then
    G_ConstraintEditor := TfrmBoldUMLConstraintEditorCx.Create(Owner);
  G_ConstraintEditor.InitElements(aUMLElement);
  G_ConstraintEditor.Show;
end;

procedure EnsureFreeConstraintEditor;
begin
  FreeAndNil(G_ConstraintEditor);
end;

procedure TfrmBoldUMLConstraintEditorCx.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caHide;
end;

procedure TfrmBoldUMLConstraintEditorCx.InitElements(CurrentElement: TUMLModelElement);
begin
  brhCurrentElement.Value := CurrentElement;
  blhElementConstraint.RootHandle := brhCurrentElement;
end;

procedure TfrmBoldUMLConstraintEditorCx.actEditMessageExecute(Sender: TObject);
begin
  dmBoldUMLModelEditorHandles.EditOclExpression(blhElementConstraint.Value as TUMLModelElement, 'name', brhCurrentElement.Value as TUMLModelElement);
end;

procedure TfrmBoldUMLConstraintEditorCx.actEditConstraintExecute(Sender: TObject);
begin
  dmBoldUMLModelEditorHandles.EditOclExpression(blhElementConstraint.Value as TUMLModelElement, 'body', brhCurrentElement.Value as TUMLModelElement);
end;

procedure TfrmBoldUMLConstraintEditorCx.btAddClick(Sender: TObject);
var
  NewConstraint: TUMLConstraint;
  Element: TUMLModelElement;
begin
  Element := brhCurrentElement.Value as TUMLModelElement;
  NewConstraint := TUMLConstraint.Create(Element.BoldSystem);
  TBoldUMLSupport.EnsureBoldTaggedValues(NewConstraint);
  NewConstraint.constrainedElement.Add(Element);
  if Element is TUMLNamespace then
    NewConstraint.namespace_ := TUMLNamespace(Element)
  else
    NewConstraint.namespace_ := Element.namespace_;
  NewConstraint.name := TBoldUMLSupport.UniqueName(NewConstraint, 'NewConstraint');
end;

procedure TfrmBoldUMLConstraintEditorCx.btRemoveClick(Sender: TObject);
var
  CurrConstr: TUMLConstraint;
begin
  if Assigned(blhElementConstraint.CurrentBoldObject) then
  begin
    CurrConstr := blhElementConstraint.CurrentBoldObject as TUMLConstraint;
    CurrConstr.UnLinkAll;
    if CurrConstr.CanDelete then
      CurrConstr.Delete
    else
      raise EBold.Create('Constraint cannot be removed.');
  end;
end;

procedure TfrmBoldUMLConstraintEditorCx.mnuCutClick(Sender: TObject);
begin
  if Assigned(ActiveControl) then
    ActiveControl.Perform(WM_CUT, 0, 0);
end;

procedure TfrmBoldUMLConstraintEditorCx.mnuCopyClick(Sender: TObject);
begin
  if Assigned(ActiveControl) then
    ActiveControl.Perform(WM_COPY, 0, 0);
end;

procedure TfrmBoldUMLConstraintEditorCx.mnuPasteClick(Sender: TObject);
begin
  if Assigned(ActiveControl) then
    ActiveControl.Perform(WM_PASTE, 0, 0);
end;

procedure TfrmBoldUMLConstraintEditorCx.FormDeactivate(Sender: TObject);
begin
  TBoldQueueable.ApplyAllMatching(self);
end;

procedure TfrmBoldUMLConstraintEditorCx.FormDestroy(Sender: TObject);
begin
  if self = G_ConstraintEditor then
    G_ConstraintEditor := nil;
end;

end.
