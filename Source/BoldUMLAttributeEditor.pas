unit BoldUMLAttributeEditor;

interface

uses Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Forms,
  Vcl.Controls, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, BoldComboBox,
  BoldCheckBox, BoldEdit, BoldLabel, BoldStringControlPack, BoldSubscription,
  BoldElements, BoldControlPack, BoldCheckboxStateControlPack, BoldHandles,
  BoldRootedHandles, BoldExpressionHandle, BoldReferenceHandle, Vcl.ComCtrls,
  BoldPropertiesController,
  BoldUMLModel, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters,
  cxContainer, cxEdit, cxTextEdit, cxMaskEdit, cxDropDownEdit, cxBoldEditors,
  BoldPropertyMapper, cxGroupBox, cxLabel, BoldToCxConverterUnit, cxCheckBox,
  BoldVariantControlPack;

type
  TBoldUMLAttributeEditForm = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    behAttribute: TBoldExpressionHandle;
    btAttributeShowDerivExprEditor: TButton;
    brhAttribute: TBoldReferenceHandle;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    BoldPropertyMapper1: TBoldPropertyMapper;
    cmbTVDPWrite: TcxBoldComboBox;
    cmbTVDPRead: TcxBoldComboBox;
    lblDPWrite: TcxLabel;
    lblDPRead: TcxLabel;
    gbDelphiFeatures: TcxGroupBox;
    cmbAttributeVisibility: TcxBoldComboBox;
    cmbAttributePMapperName: TcxBoldComboBox;
    cmbAttributeBoldType: TcxBoldComboBox;
    tbxAttributeStereotype: TcxBoldTextEdit;
    tbxAttributeName: TcxBoldTextEdit;
    tbxAttributeLength: TcxBoldTextEdit;
    tbxAttributeInitialValue: TcxBoldTextEdit;
    tbxAttributeExpressionName: TcxBoldTextEdit;
    tbxAttributeDerivationOCL: TcxBoldTextEdit;
    tbxAttributeDelphiName: TcxBoldTextEdit;
    tbxAttributeColumnName: TcxBoldTextEdit;
    lblAttributeVisibility: TcxLabel;
    lblAttributeStereotype: TcxLabel;
    lblAttributePMapperName: TcxLabel;
    lblAttributeName: TcxLabel;
    lblAttributeLength: TcxLabel;
    lblAttributeInitialValue: TcxLabel;
    lblAttributeExpressionName: TcxLabel;
    lblAttributeDerivationOCL: TcxLabel;
    lblAttributeDelphiName: TcxLabel;
    lblAttributeColumnName: TcxLabel;
    lblAttributeBoldType: TcxLabel;
    bcbDPHasField: TcxBoldCheckBox;
    bcbAttributeReverseDerive: TcxBoldCheckBox;
    bcbAttributePersistent: TcxBoldCheckBox;
    bcbAttributeDerived: TcxBoldCheckBox;
    bcbAttributeDelayedFetch: TcxBoldCheckBox;
    bcbAttributeAllowNull: TcxBoldCheckBox;
    brPersistent: TBoldAsVariantRenderer;
    brDerived: TBoldAsVariantRenderer;
    cmbTVAttributeKind: TcxBoldComboBox;
    lblAttributeKind: TcxLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btAttributeShowDerivExprEditorClick(Sender: TObject);
    procedure tbxAttributeInitialValuePropertiesValidate(Sender: TObject;
      var DisplayValue: Variant; var ErrorText: TCaption; var Error: Boolean);
    procedure tbxAttributeColumnNamePropertiesChange(Sender: TObject);
    procedure cmbAttributePMapperNamePropertiesChange(Sender: TObject);
    procedure btAttributeConstraintEditorClick(Sender: TObject);
    procedure brPersistentSetAsVariant(aFollower: TBoldFollower;
      const NewValue: Variant);
    procedure brDerivedSetAsVariant(aFollower: TBoldFollower;
      const NewValue: Variant);
    procedure cmbTVAttributeKindPropertiesChange(Sender: TObject);
  private
  public
    { Public declarations }
  end;

implementation

uses
  BoldUMLModelEditorHandlesDataModule,
  BoldDefaultTaggedValues,
  BoldUtils,
  BoldUMLModelSupport,
  BoldUMLModelDataModule,
  BoldSystem,
  BoldSystemRT,
  BoldUMLConstraintEditorCx,
  System.Variants;

{$R *.dfm}

procedure TBoldUMLAttributeEditForm.brDerivedSetAsVariant(
  aFollower: TBoldFollower; const NewValue: Variant);
begin
  AFollower.Value.AsVariant := NewValue;
  if NewValue then
    (behAttribute.Value as TUMLAttribute).persistent := false;
end;

procedure TBoldUMLAttributeEditForm.brPersistentSetAsVariant(
  aFollower: TBoldFollower; const NewValue: Variant);
begin
  AFollower.Value.AsVariant := NewValue;
  if NewValue then
    (behAttribute.Value as TUMLAttribute).derived := false;
end;

procedure TBoldUMLAttributeEditForm.btAttributeConstraintEditorClick(
  Sender: TObject);
begin
  ShowTheConstraintEditor(behAttribute.Value as TUMLModelElement, Self);
end;

procedure TBoldUMLAttributeEditForm.btAttributeShowDerivExprEditorClick(
  Sender: TObject);
var
  Attr: TUMLAttribute;
begin
  Attr := behAttribute.Value as TUMLAttribute;
  dmBoldUMLModelEditorHandles.EditOclExpression(Attr, TAG_DERIVATIONOCL, Attr.Owner);
end;

procedure TBoldUMLAttributeEditForm.cmbAttributePMapperNamePropertiesChange(
  Sender: TObject);
begin
  with Sender as TcxCustomComboBox do
  if CompareText(Text, '<Default>') = 0 then
    Style.Font.Color := clGray
  else
    Style.Font.Color := clWindowText;
end;

procedure TBoldUMLAttributeEditForm.cmbTVAttributeKindPropertiesChange(
  Sender: TObject);
begin
  if TUMLAttribute(behAttribute.Value).taggedValue['Bold.AttributeKind'].value = 'Bold' then
    cmbAttributeBoldType.Properties.BoldLookupListHandle := dmBoldUMLModelEditorHandles.bchTypesForAttribute
  else
    cmbAttributeBoldType.Properties.BoldLookupListHandle := dmBoldUMLModelEditorHandles.blhAllDataTypes;
end;

procedure TBoldUMLAttributeEditForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TBoldUMLAttributeEditForm.tbxAttributeColumnNamePropertiesChange(
  Sender: TObject);
begin
  with (Sender as TcxBoldTextEdit) do
  if CompareText(Text, '<Name>') = 0 then
    Style.Font.Color := clGray
  else
    Style.Font.Color := clWindowText;
end;

procedure TBoldUMLAttributeEditForm.tbxAttributeInitialValuePropertiesValidate(
  Sender: TObject; var DisplayValue: Variant; var ErrorText: TCaption;
  var Error: Boolean);
var
  Attribute: TUMLAttribute;
  AttributeType: TBoldAttributeTypeInfo;
  Test: TBoldAttribute;
begin
  Attribute := TUMLAttribute(behAttribute.Value);
  AttributeType := Attribute.BoldSystem.BoldSystemTypeInfo.AttributeTypeInfoByExpressionName[Attribute.typeName];
  if Assigned(AttributeType) then
  begin
    Test := TBoldMemberFactory.CreateMemberFromBoldType(AttributeType) as TBoldAttribute;
    try
      try
        Test.SetAsVariant(DisplayValue);
      except
        on e: exception do
        begin
          Error := true;
          ErrorText := e.Message;
        end;
      end;
    finally
      Test.free;
    end;
  end
  else
    Error := true;
end;

end.
