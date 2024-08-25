
{ Global compiler directives }
{$include bold.inc}
unit BoldUMLConstraintEditor;

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
  BoldPropertiesController, System.ImageList;

type
  TfrmBoldUMLConstraintEditor = class(TForm)
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
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btAddClick(Sender: TObject);
    procedure btRemoveClick(Sender: TObject);
    procedure mnuCutClick(Sender: TObject);
    procedure mnuCopyClick(Sender: TObject);
    procedure mnuPasteClick(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
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
  BoldUMLModelSupport;

{$R *.dfm}

var
  G_ConstraintEditor: TfrmBoldUMLConstraintEditor;

procedure ShowTheConstraintEditor(aUMLElement: TUMLModelElement; Owner: TComponent);
begin
  if not Assigned(G_ConstraintEditor) then
    G_ConstraintEditor := TfrmBoldUMLConstraintEditor.Create(Owner);
  G_ConstraintEditor.InitElements(aUMLElement);
  G_ConstraintEditor.Show;
end;

procedure EnsureFreeConstraintEditor;
begin
  FreeAndNil(G_ConstraintEditor);
end;

procedure TfrmBoldUMLConstraintEditor.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caHide;
end;

procedure TfrmBoldUMLConstraintEditor.InitElements( CurrentElement: TUMLModelElement);
begin
  brhCurrentElement.Value := CurrentElement;
  blhElementConstraint.RootHandle := brhCurrentElement;
end;

procedure TfrmBoldUMLConstraintEditor.btAddClick(Sender: TObject);
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

procedure TfrmBoldUMLConstraintEditor.btRemoveClick(Sender: TObject);
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

procedure TfrmBoldUMLConstraintEditor.mnuCutClick(Sender: TObject);
begin
  if Assigned(ActiveControl) then
    ActiveControl.Perform(WM_CUT, 0, 0);
end;

procedure TfrmBoldUMLConstraintEditor.mnuCopyClick(Sender: TObject);
begin
  if Assigned(ActiveControl) then
    ActiveControl.Perform(WM_COPY, 0, 0);
end;

procedure TfrmBoldUMLConstraintEditor.mnuPasteClick(Sender: TObject);
begin
  if Assigned(ActiveControl) then
    ActiveControl.Perform(WM_PASTE, 0, 0);
end;

procedure TfrmBoldUMLConstraintEditor.FormDeactivate(Sender: TObject);
begin
  TBoldQueueable.ApplyAllMatching(self);
end;

end.
