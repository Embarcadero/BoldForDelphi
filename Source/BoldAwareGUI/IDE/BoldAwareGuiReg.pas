unit BoldAwareGuiReg;

{$UNDEF BOLDCOMCLIENT}

interface

procedure Register;

implementation

uses
  SysUtils,
  Classes,
  DesignIntf,
  BoldDefs,
  BoldAbstractPropertyEditors,
  BoldPropertyEditors,
  BoldGridPropertyEditors,
{$IFNDEF BOLDCOMCLIENT} // uses
  BoldHandles, // IFNDEF BOLDCOMCLIENT
  BoldExceptionHandlers, // IFNDEF BOLDCOMCLIENT
  BoldAbstractListHandle, // IFNDEF BOLDCOMCLIENT
  BoldComboBoxPropertyEditors, // IFNDEF BOLDCOMCLIENT
  BoldControlPackPropertyEditors, // IFNDEF BOLDCOMCLIENT

  BoldMLRenderers, // fixthis  // IFNDEF BOLDCOMCLIENT
  BoldDataSet, // IFNDEF BOLDCOMCLIENT
  BoldDataSetPropertyEditors, // IFNDEF BOLDCOMCLIENT
  BoldDragDropTarget, // IFNDEF BOLDCOMCLIENT
{$ENDIF}
  BoldPropertiesControllerPropertyEditors,
  BoldPropertiesController,
  BoldStringsPropertyController,
  BoldControlPack,
  BoldGenericListControlPack,
  BoldStringControlPack,
  BoldCheckBoxStateControlPack,
  BoldNumericControlPack,
  BoldFloatControlPack,
  BoldViewerControlPack,
  BoldNodeControlPack,
  BoldListBox,
  BoldNavigator,
  BoldLabel,
  BoldCaptionController,
  BoldCheckBox,
  BoldEdit,
  BoldGrid,
  BoldProgressBar,
  BoldTrackbar,
  BoldMemo,
  BoldTreeView,
  BoldXCVTreeView,
  BoldImage,
  BoldComboBox,
  BoldPageControl,
  BoldIDEConsts;

{$R BoldAwareGUIReg.res}

procedure RegisterEditors;
begin
  //TBoldRenderer
  RegisterPropertyEditor(TypeInfo(TBoldSubscribe), nil, '', TBoldElementSubscribeMethodProperty);
  RegisterPropertyEditor(TypeInfo(TBoldHoldsChangedValue), nil, '', TBoldHoldsChangedValueMethodProperty);
  RegisterPropertyEditor(TypeInfo(TBoldReleaseChangedValue), nil, '', TBoldReleaseChangedValueMethodProperty);
  RegisterPropertyEditor(TypeInfo(TBoldMayModify), nil, '', TBoldMayModifyMethodProperty);

  {$IFNDEF BOLDCOMCLIENT} // register editors

  // Register property editors
  // All properties of type TBoldElementHandle named BoldHandle will be displayed RED if prop not set
  RegisterPropertyEditor(TypeInfo(TBoldElementHandle), TPersistent, 'BoldHandle', TBoldComponentPropertyIndicateMissing); // do not localize
  RegisterPropertyEditor(TypeInfo(TBoldAbstractListHandle), TBoldComboBox, 'BoldListHandle', TBoldComponentPropertyIndicateMissing); // do not localize


  RegisterPropertyEditor(TypeInfo(integer), TBoldFollowerController, 'Representation', TBoldRepresentationProperty); // do not localize
  RegisterPropertyEditor(TypeInfo(TBoldRenderer), TBoldFollowerController, 'Renderer', TBoldRendererComponentProperty); // do not localize

  // Note: registering for TPersistent screws up, as all string-properties will get an ellipsis!
  // v the below line doesn't work, but is left as a reminder.
  //  RegisterPropertyEditor(TypeInfo(TBoldExpression), TPersistent, '', TBoldOCLExpressionProperty);
  // ^ the above line doesn't work, but is left as a reminder.

  RegisterPropertyEditor(TypeInfo(TBoldExpression), TBoldFollowerController, 'Expression', TBoldOCLExpressionForFollowerControllersProperty); // do not localize

  RegisterPropertyEditor(TypeInfo(TBoldSingleFollowerController), nil, '', TBoldSingleFollowerControllerEditor);
  RegisterPropertyEditor(TypeInfo(TBoldTreeFollowerController), nil, '', TBoldTreeFollowerControllerEditor);

  //TBoldAsStringRenderer
  RegisterPropertyEditor(TypeInfo(TBoldGetAsString), nil, '', TBoldGetAsStringMethodProperty);
  //TBoldAsCheckBoxRenderer
  RegisterPropertyEditor(TypeInfo(TBoldGetAsCheckBoxState), nil, '', TBoldGetAsCheckBoxStateMethodProperty);
  //TBoldAsIntegerRenderer
  RegisterPropertyEditor(TypeInfo(TBoldGetAsIntegerEvent), nil, '', TBoldGetAsIntegerEventMethodProperty);
  //TBoldAsFloatRenderer
  RegisterPropertyEditor(TypeInfo(TBoldGetAsFloatEvent), nil, '', TBoldGetAsFloatEventMethodProperty);
  //TBoldAsViewerRenderer
  RegisterPropertyEditor(TypeInfo(TBoldGetAsViewer), nil, '', TBoldGetAsViewerMethodProperty);
  RegisterPropertyEditor(TypeInfo(String), TBoldNodeDescription, 'ContextTypeName', TBoldTypeNameSelectorPropertyForTreeFollowerController); // do not localize

  RegisterPropertyEditor(TypeInfo(String), TBoldGenericListPart, 'ControllerExpression', TBoldOCLExpressionForGenericListPart); // do not localize
  RegisterPropertyEditor(TypeInfo(String), TBoldGenericListPart, 'ElementExpression', TBoldOCLExpressionForGenericListPart);  // do not localize

  RegisterPropertyEditor(TypeInfo(String), TBoldComboBox, 'BoldSetValueExpression', TBoldOCLExpressionForComboBoxSetValueExpression); // do not localize
  RegisterPropertyEditor(TypeInfo(String), TBoldDropTarget, 'NodeSelectionExpression', TBoldOCLExpressionForOCLComponent); // do not localize

  // Register Component editors
    {Renderer editors}
  RegisterComponentEditor(TBoldAsStringRenderer, TBoldAsStringRendererEditor);
  RegisterComponentEditor(TBoldAsCheckboxStateRenderer, TBoldAsCheckboxStateRendererEditor);
  RegisterComponentEditor(TBoldAsIntegerRenderer, TBoldAsIntegerRendererEditor);
  RegisterComponentEditor(TBoldAsFloatRenderer, TBoldAsFloatRendererEditor);
    {GUI Component editors}
  RegisterComponentEditor(TBoldCustomTreeView, TBoldNodeDescriptionEditor);
  RegisterComponentEditor(TBoldCustomEdit, TBoldOCLComponentEditor);
  RegisterComponentEditor(TBoldCustomCheckbox, TBoldOCLComponentEditor);
  RegisterComponentEditor(TBoldCustomListBox, TBoldOCLComponentEditor);
  RegisterComponentEditor(TBoldCustomLabel, TBoldOCLComponentEditor);
  RegisterComponentEditor(TBoldCustomCaptionController, TBoldOCLComponentEditor);
  RegisterComponentEditor(TBoldProgressBar, TBoldOCLComponentEditor);
  RegisterComponentEditor(TBoldTrackBar, TBoldOCLComponentEditor);
  RegisterComponentEditor(TBoldCustomMemo, TBoldOCLComponentEditor);
  RegisterComponentEditor(TBoldImage, TBoldOCLComponentEditor);

  {$ENDIF}
  RegisterComponentEditor(TBoldCustomGrid, TBoldColumnsEditor);
  RegisterComponentEditor(TBoldPropertiesController,TBoldPropertiesControllerComponentEditor);
  RegisterPropertyEditor(TypeInfo(String), TBoldDrivenProperty, 'PropertyName', TPropertyNameProperty);  // do not localize
  RegisterPropertyEditor(TypeInfo(TComponent), TBoldDrivenProperty, 'VCLComponent', TVCLComponentProperty); // do not localize
{$IFNDEF BOLDCOMCLIENT} // registerEditors
  RegisterComponentEditor(TBoldAbstractDataset, TBoldDataSetEditor);
{$ENDIF}
end;

procedure RegisterComponentsOnPalette;
begin
{$IFNDEF BOLDCOMCLIENT} // RegisterComponents
  RegisterComponents(BOLDPAGENAME_MISC,
  [
    TBoldDataSet,
    TBoldExceptionHandler
  ]);
{$ENDIF}

{$IFDEF BOLDCOMCLIENT} // RegisterComponents
  RegisterComponents(BOLDPAGENAME_COMCONTROLS,
{$ELSE}
  RegisterComponents(BOLDPAGENAME_CONTROLS,
{$ENDIF}
  [
    TBoldLabel,
    TBoldEdit,
    TBoldListBox,
    TBoldComboBox,
    TBoldNavigator,
    TBoldCaptionController,
    TBoldCheckBox,
    TBoldGrid,
    TBoldProgressBar,
    TBoldTrackBar,
    TBoldMemo,
    TBoldPageControl,
    {TBoldRichEdit,}
    TBoldTreeView,
    TBoldXCVTreeView,
    TBoldImage,
    TBoldPropertiesController,
    TBoldStringsPropertyController,
    //Renderers
    TBoldAsStringRenderer,
    TBoldAsCheckboxStateRenderer,
    TBoldAsIntegerRenderer,
    TBoldAsFloatRenderer,
    {$IFNDEF BOLDCOMCLIENT} // Register Components
    TBoldAsMLStringRenderer,
    TBoldDropTarget,
    {$ENDIF}
    TBoldAsViewerRenderer
  ]);
end;

procedure Register;
begin
	RegisterComponentsOnPalette;
  RegisterEditors;
end;

end.
