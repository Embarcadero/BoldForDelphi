unit BoldAwareGuiComReg;

{$DEFINE BOLDCOMCLIENT} {Clientified 2002-08-05 13:13:02}

interface

procedure Register;

implementation

uses
  BoldRev,
  Classes,
  DesignIntf,
  BoldDefs,
  BoldAbstractPropertyEditors,
  BoldComPropertyEditors,
  BoldGridPropertyEditorsCom,
{$IFNDEF BOLDCOMCLIENT} // uses
  BoldHandlesCom, // IFNDEF BOLDCOMCLIENT
  BoldExceptionHandlersCom, // IFNDEF BOLDCOMCLIENT
  BoldAbstractListHandleCom, // IFNDEF BOLDCOMCLIENT
  BoldComboBoxPropertyEditors, // IFNDEF BOLDCOMCLIENT
  BoldControlPackPropertyEditors, // IFNDEF BOLDCOMCLIENT

  BoldMLRenderersCom, // fixthis  // IFNDEF BOLDCOMCLIENT
  BoldDataSetCom, // IFNDEF BOLDCOMCLIENT
  BoldDataSetPropertyEditors, // IFNDEF BOLDCOMCLIENT
  BoldDragDropTargetCom, // IFNDEF BOLDCOMCLIENT
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
  //TBoldRendererCom
  RegisterPropertyEditor(TypeInfo(TBoldSubscribeCom), nil, '', TBoldElementCOMSubscribeMethodProperty);
  RegisterPropertyEditor(TypeInfo(TBoldHoldsChangedValueCom), nil, '', TBoldCOMMethodNoPurposeProperty);
  RegisterPropertyEditor(TypeInfo(TBoldReleaseChangedValueCom), nil, '', TBoldCOMMethodNoPurposeProperty);
  RegisterPropertyEditor(TypeInfo(TBoldMayModifyCom), nil, '', TBoldCOMMethodNoPurposeProperty);

  {$IFNDEF BOLDCOMCLIENT} // register editors

  // Register property editors
  // All properties of type TBoldElementHandleCom named BoldHandle will be displayed RED if prop not set
  RegisterPropertyEditor(TypeInfo(TBoldElementHandleCom), TPersistent, 'BoldHandle', TBoldComponentPropertyIndicateMissing);
  RegisterPropertyEditor(TypeInfo(TBoldAbstractListHandleCom), TBoldComboBoxCom, 'BoldListHandleCom', TBoldComponentPropertyIndicateMissing);


  RegisterPropertyEditor(TypeInfo(integer), TBoldFollowerControllerCom, 'Representation', TBoldRepresentationProperty);
  RegisterPropertyEditor(TypeInfo(TBoldRendererCom), TBoldFollowerControllerCom, 'Renderer', TBoldRendererComponentProperty);

  // Note: registering for TPersistent screws up, as all string-properties will get an ellipsis!
  // v the below line doesn't work, but is left as a reminder.
  //  RegisterPropertyEditor(TypeInfo(TBoldExpression), TPersistent, '', TBoldOCLExpressionProperty);
  // ^ the above line doesn't work, but is left as a reminder.

  RegisterPropertyEditor(TypeInfo(TBoldExpression), TBoldFollowerControllerCom, 'Expression', TBoldOCLExpressionForFollowerControllersProperty);

  RegisterPropertyEditor(TypeInfo(TBoldSingleFollowerControllerCom), nil, '', TBoldSingleFollowerControllerEditor);
  RegisterPropertyEditor(TypeInfo(TBoldTreeFollowerControllerCom), nil, '', TBoldTreeFollowerControllerEditor);

  //TBoldAsStringRendererCom
  RegisterPropertyEditor(TypeInfo(TBoldGetAsStringCom), nil, '', TBoldGetAsStringMethodProperty);
  //TBoldAsCheckBoxRenderer
  RegisterPropertyEditor(TypeInfo(TBoldGetAsCheckBoxStateCom), nil, '', TBoldGetAsCheckBoxStateMethodProperty);
  //TBoldAsIntegerRendererCom
  RegisterPropertyEditor(TypeInfo(TBoldGetAsIntegerEventCom), nil, '', TBoldGetAsIntegerEventMethodProperty);
  //TBoldAsFloatRendererCom
  RegisterPropertyEditor(TypeInfo(TBoldGetAsFloatEventCom), nil, '', TBoldGetAsFloatEventMethodProperty);
  //TBoldAsViewerRendererCom
  RegisterPropertyEditor(TypeInfo(TBoldGetAsViewerCom), nil, '', TBoldGetAsViewerMethodProperty);
  RegisterPropertyEditor(TypeInfo(String), TBoldNodeDescriptionCom, 'ContextTypeName', TBoldTypeNameSelectorPropertyForTreeFollowerController);

  RegisterPropertyEditor(TypeInfo(String), TBoldGenericListPartCom, 'ControllerExpression', TBoldOCLExpressionForGenericListPart);
  RegisterPropertyEditor(TypeInfo(String), TBoldGenericListPartCom, 'ElementExpression', TBoldOCLExpressionForGenericListPart);

  RegisterPropertyEditor(TypeInfo(String), TBoldComboBoxCom, 'BoldSetValueExpression', TBoldOCLExpressionForComboBoxSetValueExpression);
  RegisterPropertyEditor(TypeInfo(String), TBoldDropTargetCom, 'NodeSelectionExpression', TBoldOCLExpressionForOCLComponent);

  // Register Component editors
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
{$IFNDEF BOLDCOMCLIENT} // registerEditors
  RegisterComponentEditor(TBoldAbstractDataset, TBoldDataSetEditor);
{$ENDIF}
end;

procedure RegisterComponentsOnPalette;
begin
{$IFNDEF BOLDCOMCLIENT} // RegisterComponents
  RegisterComponents(BOLDPAGENAME_MISC,
  [
    TBoldDataSetCom,
    TBoldExceptionHandlerCom
  ]);
{$ENDIF}

{$IFDEF BOLDCOMCLIENT} // RegisterComponents
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
    //Renderers
    TBoldAsStringRendererCom,
    TBoldAsCheckBoxStateRendererCom,
    TBoldAsIntegerRendererCom,
    TBoldAsFloatRendererCom,
    {$IFNDEF BOLDCOMCLIENT} // Register Components
    TBoldAsMLStringRendererCom,
    TBoldDropTargetCom,
    {$ENDIF}
    TBoldAsViewerRendererCom
  ]);
end;

procedure Register;
begin
  begin
    RegisterComponentsOnPalette;
    RegisterEditors;
  end;
end;

end.
