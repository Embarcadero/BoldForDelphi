
{ Global compiler directives }
{$include bold.inc}
unit BoldAwareGuiComReg;

{$DEFINE BOLDCOMCLIENT} {Clientified 2002-08-05 13:13:02}

interface

procedure Register;

implementation

uses
  Classes,
  DesignIntf,
  BoldDefs,
  BoldAbstractPropertyEditors,
  BoldComPropertyEditors,
  BoldGridPropertyEditorsCom,
{$IFNDEF BOLDCOMCLIENT}
  BoldHandlesCom,
  BoldExceptionHandlersCom,
  BoldAbstractListHandleCom,
  BoldComboBoxPropertyEditors,
  BoldControlPackPropertyEditors,

  BoldMLRenderersCom,
  BoldDataSetCom,
  BoldDataSetPropertyEditors,
  BoldDragDropTargetCom,
{$ENDIF}
  BoldPropertiesControllerPropertyEditorsCom,
  BoldPropertiesControllerCom,
  BoldStringsPropertyControllerCom,
  BoldControlPackCom,
  BoldGenericListControlPackCom,
  BoldStringControlPackCom,
  BoldCheckboxStateControlPackCom,
  BoldNumericControlPackCom,
  BoldFloatControlPackCom,
  BoldViewerControlPackCom,
  BoldNodeControlPackCom,
  BoldListBoxCom,
  BoldNavigatorCom,
  BoldLabelCom,
  BoldCaptionControllerCom,
  BoldCheckBoxCom,
  BoldEditCom,
  BoldGridCom,
  BoldProgressBarCom,
  BoldTrackBarCom,
  BoldMemoCom,
  BoldTreeViewCom,
  BoldXCVTreeViewCom,
  BoldImageCom,
  BoldComboBoxCom,
  BoldPageControlCom,
  BoldIDEConsts;

{$R BoldAwareGuiComReg.res}

procedure RegisterEditors;
begin
  RegisterPropertyEditor(TypeInfo(TBoldSubscribeCom), nil, '', TBoldElementCOMSubscribeMethodProperty);
  RegisterPropertyEditor(TypeInfo(TBoldHoldsChangedValueCom), nil, '', TBoldCOMMethodNoPurposeProperty);
  RegisterPropertyEditor(TypeInfo(TBoldReleaseChangedValueCom), nil, '', TBoldCOMMethodNoPurposeProperty);
  RegisterPropertyEditor(TypeInfo(TBoldMayModifyCom), nil, '', TBoldCOMMethodNoPurposeProperty);

  {$IFNDEF BOLDCOMCLIENT}


  RegisterPropertyEditor(TypeInfo(TBoldElementHandleCom), TPersistent, 'BoldHandle', TBoldComponentPropertyIndicateMissing);
  RegisterPropertyEditor(TypeInfo(TBoldAbstractListHandleCom), TBoldComboBoxCom, 'BoldListHandleCom', TBoldComponentPropertyIndicateMissing);


  RegisterPropertyEditor(TypeInfo(integer), TBoldFollowerControllerCom, 'Representation', TBoldRepresentationProperty);
  RegisterPropertyEditor(TypeInfo(TBoldRendererCom), TBoldFollowerControllerCom, 'Renderer', TBoldRendererComponentProperty);




  RegisterPropertyEditor(TypeInfo(TBoldExpression), TBoldFollowerControllerCom, 'Expression', TBoldOCLExpressionForFollowerControllersProperty);

  RegisterPropertyEditor(TypeInfo(TBoldSingleFollowerControllerCom), nil, '', TBoldSingleFollowerControllerEditor);
  RegisterPropertyEditor(TypeInfo(TBoldTreeFollowerControllerCom), nil, '', TBoldTreeFollowerControllerEditor);
  RegisterPropertyEditor(TypeInfo(TBoldGetAsStringCom), nil, '', TBoldGetAsStringMethodProperty);
  RegisterPropertyEditor(TypeInfo(TBoldGetAsCheckBoxStateCom), nil, '', TBoldGetAsCheckBoxStateMethodProperty);
  RegisterPropertyEditor(TypeInfo(TBoldGetAsIntegerEventCom), nil, '', TBoldGetAsIntegerEventMethodProperty);
  RegisterPropertyEditor(TypeInfo(TBoldGetAsFloatEventCom), nil, '', TBoldGetAsFloatEventMethodProperty);
  RegisterPropertyEditor(TypeInfo(TBoldGetAsViewerCom), nil, '', TBoldGetAsViewerMethodProperty);
  RegisterPropertyEditor(TypeInfo(String), TBoldNodeDescriptionCom, 'ContextTypeName', TBoldTypeNameSelectorPropertyForTreeFollowerController);

  RegisterPropertyEditor(TypeInfo(String), TBoldGenericListPartCom, 'ControllerExpression', TBoldOCLExpressionForGenericListPart);
  RegisterPropertyEditor(TypeInfo(String), TBoldGenericListPartCom, 'ElementExpression', TBoldOCLExpressionForGenericListPart);

  RegisterPropertyEditor(TypeInfo(String), TBoldComboBoxCom, 'BoldSetValueExpression', TBoldOCLExpressionForComboBoxSetValueExpression);
  RegisterPropertyEditor(TypeInfo(String), TBoldDropTargetCom, 'NodeSelectionExpression', TBoldOCLExpressionForOCLComponent);
    {Renderer editors}
  RegisterComponentEditor(TBoldAsStringRendererCom, TBoldAsStringRendererEditor);
  RegisterComponentEditor(TBoldAsCheckBoxStateRendererCom, TBoldAsCheckboxStateRendererEditor);
  RegisterComponentEditor(TBoldAsIntegerRendererCom, TBoldAsIntegerRendererEditor);
  RegisterComponentEditor(TBoldAsFloatRendererCom, TBoldAsFloatRendererEditor);
    {GUI Component editors}
  RegisterComponentEditor(TBoldCustomTreeViewCom, TBoldNodeDescriptionEditor);
  RegisterComponentEditor(TBoldCustomEditCom, TBoldOCLComponentEditor);
  RegisterComponentEditor(TBoldCustomCheckBoxCom, TBoldOCLComponentEditor);
  RegisterComponentEditor(TBoldCustomListBoxCom, TBoldOCLComponentEditor);
  RegisterComponentEditor(TBoldCustomLabelCom, TBoldOCLComponentEditor);
  RegisterComponentEditor(TBoldCustomCaptionControllerCom, TBoldOCLComponentEditor);
  RegisterComponentEditor(TBoldProgressBarCom, TBoldOCLComponentEditor);
  RegisterComponentEditor(TBoldTrackBarCom, TBoldOCLComponentEditor);
  RegisterComponentEditor(TBoldCustomMemoCom, TBoldOCLComponentEditor);
  RegisterComponentEditor(TBoldImageCom, TBoldOCLComponentEditor);

  {$ENDIF}
  RegisterComponentEditor(TBoldCustomGridCom, TBoldColumnsEditorCom);
  RegisterComponentEditor(TBoldPropertiesControllerCom,TBoldPropertiesControllerComponentEditorCom);
  RegisterPropertyEditor(TypeInfo(String), TBoldDrivenPropertyCom, 'PropertyName', TPropertyNamePropertyCom);
  RegisterPropertyEditor(TypeInfo(TComponent), TBoldDrivenPropertyCom, 'VCLComponent', TVCLComponentPropertyCom);
{$IFNDEF BOLDCOMCLIENT}
  RegisterComponentEditor(TBoldAbstractDataset, TBoldDataSetEditor);
{$ENDIF}
end;

procedure RegisterComponentsOnPalette;
begin
{$IFNDEF BOLDCOMCLIENT}
  RegisterComponents(BOLDPAGENAME_MISC,
  [
    TBoldDataSetCom,
    TBoldExceptionHandlerCom
  ]);
{$ENDIF}

{$IFDEF BOLDCOMCLIENT}
  RegisterComponents('Bold COM Controls',
{$ELSE}
  RegisterComponents(BOLDPAGENAME_CONTROLS,
{$ENDIF}
  [
    TBoldLabelCom,
    TBoldEditCom,
    TBoldListBoxCom,
    TBoldComboBoxCom,
    TBoldNavigatorCom,
    TBoldCaptionControllerCom,
    TBoldCheckBoxCom,
    TBoldGridCom,
    TBoldProgressBarCom,
    TBoldTrackBarCom,
    TBoldMemoCom,
    TBoldPageControlCom,
    {TBoldRichEditCom,}
    TBoldTreeViewCom,
    TBoldXCVTreeViewCom,
    TBoldImageCom,
    TBoldPropertiesControllerCom,
    TBoldStringsPropertyControllerCom,
    TBoldAsStringRendererCom,
    TBoldAsCheckBoxStateRendererCom,
    TBoldAsIntegerRendererCom,
    TBoldAsFloatRendererCom,
    {$IFNDEF BOLDCOMCLIENT}
    TBoldAsMLStringRendererCom,
    TBoldDropTargetCom,
    {$ENDIF}
    TBoldAsViewerRendererCom
  ]);
end;

procedure Register;
begin
    RegisterComponentsOnPalette;
    RegisterEditors;
end;

end.
