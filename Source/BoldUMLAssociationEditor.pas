unit BoldUMLAssociationEditor;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BoldSubscription, BoldHandles,
  BoldRootedHandles, BoldExpressionHandle, Vcl.ExtCtrls, Vcl.StdCtrls,
  BoldElements, BoldControlPack, BoldStringControlPack,
  BoldCheckboxStateControlPack, BoldCheckBox, BoldComboBox, BoldEdit, BoldLabel,
  Vcl.ComCtrls, BoldReferenceHandle, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, cxContainer, cxEdit, cxTextEdit, cxMaskEdit,
  cxDropDownEdit, cxBoldEditors, cxLabel, BoldPropertyMapper, cxCheckBox,
  BoldVariantControlPack, BoldAbstractListHandle, BoldCursorHandle,
  BoldListHandle, cxButtonEdit;

type
  TBoldUMLAssociationEditForm = class(TForm)
    behAssociation: TBoldExpressionHandle;
    OKBtn: TButton;
    CancelBtn: TButton;
    behAssociationEnd1: TBoldExpressionHandle;
    behAssociationEnd2: TBoldExpressionHandle;
    brhAssociation: TBoldReferenceHandle;
    PageControl2: TPageControl;
    TabSheet1: TTabSheet;
    Panel1: TPanel;
    PageControl1: TPageControl;
    AssociationEnd1: TTabSheet;
    btAssoEndShowDeriExprEditor1: TButton;
    AssociationEnd2: TTabSheet;
    cmbAssoEndClass: TcxBoldComboBox;
    tbxAssoEndDerivationOCL: TcxBoldTextEdit;
    cmbAssoEndDeleteAction: TcxBoldComboBox;
    cmbMultiplicity: TcxBoldComboBox;
    cmbAssoEndChangeability: TcxBoldComboBox;
    cmbAssoEndVisibility: TcxBoldComboBox;
    cmbAggregationKind: TcxBoldComboBox;
    tbxAssociationEndStereotype: TcxBoldTextEdit;
    tbxAssociationEndColumnName: TcxBoldTextEdit;
    tbxAssociationEndExpressionName: TcxBoldTextEdit;
    tbxAssociationEndDelphiName: TcxBoldTextEdit;
    tbxAssociationEndName: TcxBoldTextEdit;
    lblAssoEndDerivationOCL: TcxLabel;
    lblAssoEndDeleteAction: TcxLabel;
    lbMultiplicity: TcxLabel;
    lblAssoEndChangeability: TcxLabel;
    lblAssoEndVisibility: TcxLabel;
    Label2: TcxLabel;
    lblStereotype: TcxLabel;
    lblAssociationEndClass: TcxLabel;
    lblAssociationEndColumnName: TcxLabel;
    lblAssociationEndExpressionName: TcxLabel;
    lblAssociationEndDelphiName: TcxLabel;
    lblAssociationEndName: TcxLabel;
    tbxAssociationStereotype: TcxBoldTextEdit;
    cmbAssociationLinkClass: TcxBoldComboBox;
    tbxAssociationName: TcxBoldTextEdit;
    lblAssociationStereotype: TcxLabel;
    lblAssociationClass: TcxLabel;
    lblAssociationName: TcxLabel;
    btAssoEndShowDeriExprEditor2: TButton;
    cxBoldComboBox1: TcxBoldComboBox;
    cxBoldTextEdit1: TcxBoldTextEdit;
    cxBoldComboBox2: TcxBoldComboBox;
    cxBoldComboBox3: TcxBoldComboBox;
    cxBoldComboBox4: TcxBoldComboBox;
    cxBoldComboBox5: TcxBoldComboBox;
    cxBoldComboBox6: TcxBoldComboBox;
    cxBoldTextEdit3: TcxBoldTextEdit;
    cxBoldTextEdit4: TcxBoldTextEdit;
    cxBoldTextEdit5: TcxBoldTextEdit;
    cxBoldTextEdit6: TcxBoldTextEdit;
    cxBoldTextEdit7: TcxBoldTextEdit;
    cxLabel1: TcxLabel;
    cxLabel2: TcxLabel;
    cxLabel3: TcxLabel;
    cxLabel5: TcxLabel;
    cxLabel6: TcxLabel;
    cxLabel7: TcxLabel;
    cxLabel8: TcxLabel;
    cxLabel9: TcxLabel;
    cxLabel10: TcxLabel;
    cxLabel11: TcxLabel;
    cxLabel12: TcxLabel;
    cxLabel13: TcxLabel;
    BoldPropertyMapper1: TBoldPropertyMapper;
    BoldCheckBox5: TcxBoldCheckBox;
    BoldCheckBox3: TcxBoldCheckBox;
    BoldCheckBox1: TcxBoldCheckBox;
    bcbAssociationEndEmbed: TcxBoldCheckBox;
    bcbAssociationEndOrdered: TcxBoldCheckBox;
    bcbAssociationEndNavigable: TcxBoldCheckBox;
    bcbPersistent: TcxBoldCheckBox;
    bcbAssociationDerived: TcxBoldCheckBox;
    brPersistent: TBoldAsVariantRenderer;
    brDerived: TBoldAsVariantRenderer;
    cxLabel14: TcxLabel;
    cxLabel15: TcxLabel;
    blhQualifierAttributes1: TBoldListHandle;
    blhQualifierAttributes2: TBoldListHandle;
    brQualifier: TBoldAsVariantRenderer;
    cxBoldButtonEdit1: TcxBoldButtonEdit;
    cxBoldButtonEdit2: TcxBoldButtonEdit;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btAssoEndShowDeriExprEditor1Click(Sender: TObject);
    procedure btAssoEndShowDeriExprEditor2Click(Sender: TObject);
    procedure btAssociationEndConstraintEditor2Click(Sender: TObject);
    procedure brPersistentSetAsVariant(aFollower: TBoldFollower;
      const NewValue: Variant);
    procedure brDerivedSetAsVariant(aFollower: TBoldFollower;
      const NewValue: Variant);
    procedure brQualifierSetAsVariant(aFollower: TBoldFollower;
      const NewValue: Variant);
    procedure cxBoldButtonEdit1PropertiesButtonClick(Sender: TObject;
      AButtonIndex: Integer);
    procedure btAssociationConstraintEditorClick(Sender: TObject);
    procedure btAssociationEndConstraintEditor1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses
  BoldUMLModelEditorHandlesDataModule,
  BoldUMLModel,
  BoldDefaultTaggedValues,
  BoldUMLConstraintEditorCx,
  BoldUMLModelDataModule,
  BoldUMLQualifierEditor;

{$R *.dfm}

procedure TBoldUMLAssociationEditForm.brDerivedSetAsVariant(
  aFollower: TBoldFollower; const NewValue: Variant);
begin
  AFollower.Value.AsVariant := NewValue;
  if NewValue then
    (behAssociation.Value as TUMLAssociation).persistent := false;
end;

procedure TBoldUMLAssociationEditForm.brPersistentSetAsVariant(
  aFollower: TBoldFollower; const NewValue: Variant);
begin
  AFollower.Value.AsVariant := NewValue;
  if NewValue then
    (behAssociation.Value as TUMLAssociation).derived := false;
end;

procedure TBoldUMLAssociationEditForm.brQualifierSetAsVariant(
  aFollower: TBoldFollower; const NewValue: Variant);
begin
  if VarIsNull(NewValue) then
    (aFollower.Element as TUMLAssociationEnd).M_qualifier.Clear
  else
//    (aFollower.Element as TUMLAssociationEnd).M_qualifier[0].M_attributeLink.
end;

procedure TBoldUMLAssociationEditForm.btAssociationConstraintEditorClick(
  Sender: TObject);
begin
  ShowTheConstraintEditor(behAssociation.Value as TUMLModelElement, Self);
end;

procedure TBoldUMLAssociationEditForm.btAssociationEndConstraintEditor1Click(
  Sender: TObject);
begin
  ShowTheConstraintEditor(behAssociationEnd1.Value as TUMLModelElement, Self);
end;

procedure TBoldUMLAssociationEditForm.btAssociationEndConstraintEditor2Click(
  Sender: TObject);
begin
  ShowTheConstraintEditor(behAssociationEnd2.Value as TUMLModelElement, Self);
end;

procedure TBoldUMLAssociationEditForm.btAssoEndShowDeriExprEditor1Click(
  Sender: TObject);
var
  Assoc: TUMLAssociationEnd;
begin
  Assoc := behAssociationEnd1.Value as TUMLAssociationEnd;
  dmBoldUMLModelEditorHandles.EditOclExpression(Assoc, TAG_DERIVATIONOCL, Assoc.otherEnd.type_);
end;

procedure TBoldUMLAssociationEditForm.btAssoEndShowDeriExprEditor2Click(
  Sender: TObject);
var
  Assoc: TUMLAssociationEnd;
begin
  Assoc := behAssociationEnd2.Value as TUMLAssociationEnd;
  dmBoldUMLModelEditorHandles.EditOclExpression(Assoc, TAG_DERIVATIONOCL, Assoc.otherEnd.type_);
end;

procedure TBoldUMLAssociationEditForm.cxBoldButtonEdit1PropertiesButtonClick(
  Sender: TObject; AButtonIndex: Integer);
var
  QualifierEditForm: TBoldUMLQualifierEditForm;
  Assoc: TUMLAssociationEnd;
  BlockName: string;
begin
  QualifierEditForm := TBoldUMLQualifierEditForm.Create(nil);
  try
    Assoc := (Sender as TcxBoldButtonEdit).DataBinding.BoldHandle.Value as TUMLAssociationEnd;
    QualifierEditForm.brhAssociationEnd.Value := Assoc;
    BlockName := Assoc.BoldSystem.UndoHandlerInterface.SetCheckPoint('Edit Qualifier');
    if QualifierEditForm.ShowModal = mrCancel then
    begin
      Assoc.BoldSystem.UndoHandlerInterface.UndoBlock(BlockName);
      if Assoc.BoldSystem.UndoHandlerInterface.RedoList.IndexOf(BlockName) <> -1 then
        Assoc.BoldSystem.UndoHandlerInterface.RedoList.RemoveBlock(BlockName);
    end;
  finally
    QualifierEditForm.free;
  end;
end;

procedure TBoldUMLAssociationEditForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

end.
