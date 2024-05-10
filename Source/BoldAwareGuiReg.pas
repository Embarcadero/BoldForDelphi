
{ Global compiler directives }
{$include bold.inc}
unit BoldAwareGuiReg;

{$UNDEF BOLDCOMCLIENT}

interface

procedure Register;

implementation

uses
  ActnList,
  Actions,
  Classes,
  DesignIntf,
  BoldDefs,
  BoldAbstractPropertyEditors,
  BoldPropertyEditors,
  BoldGridPropertyEditors,
{$IFNDEF BOLDCOMCLIENT}
  BoldHandles,
  BoldExceptionHandlers,
  BoldAbstractListHandle,
  BoldComboBoxPropertyEditors,
  BoldControlPackPropertyEditors,

  BoldMLRenderers,
  BoldDragDropTarget,
{$ENDIF}
  BoldPropertiesControllerPropertyEditors,
  BoldPropertiesController,
  BoldStringsPropertyController,
  BoldPropertyMapper,
  BoldPropertyMappingPropertyEditors,
  BoldControlPack,
  BoldVariantControlPack,
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
  BoldAction,
  BoldIDEConsts;

{$R BoldAwareGUIReg.res}

procedure RegisterEditors;
begin
  RegisterPropertyEditor(TypeInfo(TBoldSubscribe), nil, '', TBoldElementSubscribeMethodProperty);
  RegisterPropertyEditor(TypeInfo(TBoldHoldsChangedValue), nil, '', TBoldHoldsChangedValueMethodProperty);
  RegisterPropertyEditor(TypeInfo(TBoldReleaseChangedValue), nil, '', TBoldReleaseChangedValueMethodProperty);
  RegisterPropertyEditor(TypeInfo(TBoldMayModify), nil, '', TBoldMayModifyMethodProperty);

  {$IFNDEF BOLDCOMCLIENT}


  RegisterPropertyEditor(TypeInfo(TBoldElementHandle), TPersistent, 'BoldHandle', TBoldComponentPropertyIndicateMissing);
  RegisterPropertyEditor(TypeInfo(TBoldAbstractListHandle), TBoldComboBox, 'BoldListHandle', TBoldComponentPropertyIndicateMissing);


  RegisterPropertyEditor(TypeInfo(integer), TBoldFollowerController, 'Representation', TBoldRepresentationProperty);
  RegisterPropertyEditor(TypeInfo(TBoldRenderer), TBoldFollowerController, 'Renderer', TBoldRendererComponentProperty);




  RegisterPropertyEditor(TypeInfo(TBoldExpression), TBoldFollowerController, 'Expression', TBoldOCLExpressionForFollowerControllersProperty);

  RegisterPropertyEditor(TypeInfo(TBoldSingleFollowerController), nil, '', TBoldSingleFollowerControllerEditor);
  RegisterPropertyEditor(TypeInfo(TBoldTreeFollowerController), nil, '', TBoldTreeFollowerControllerEditor);
  RegisterPropertyEditor(TypeInfo(TBoldGetAsString), nil, '', TBoldGetAsStringMethodProperty);
  RegisterPropertyEditor(TypeInfo(TBoldGetAsCheckBoxState), nil, '', TBoldGetAsCheckBoxStateMethodProperty);
  RegisterPropertyEditor(TypeInfo(TBoldGetAsIntegerEvent), nil, '', TBoldGetAsIntegerEventMethodProperty);
  RegisterPropertyEditor(TypeInfo(TBoldGetAsFloatEvent), nil, '', TBoldGetAsFloatEventMethodProperty);
  RegisterPropertyEditor(TypeInfo(TBoldGetAsViewer), nil, '', TBoldGetAsViewerMethodProperty);
  RegisterPropertyEditor(TypeInfo(String), TBoldNodeDescription, 'ContextTypeName', TBoldTypeNameSelectorPropertyForTreeFollowerController);

  RegisterPropertyEditor(TypeInfo(String), TBoldGenericListPart, 'ControllerExpression', TBoldOCLExpressionForGenericListPart);
  RegisterPropertyEditor(TypeInfo(String), TBoldGenericListPart, 'ElementExpression', TBoldOCLExpressionForGenericListPart);

  RegisterPropertyEditor(TypeInfo(String), TBoldComboBox, 'BoldSetValueExpression', TBoldOCLExpressionForComboBoxSetValueExpression);
  RegisterPropertyEditor(TypeInfo(String), TBoldDropTarget, 'NodeSelectionExpression', TBoldOCLExpressionForOCLComponent);
  RegisterPropertyEditor(TypeInfo(TBoldGetAsVariant), nil, '', TBoldGetAsVariantMethodProperty);
    {Renderer editors}
  RegisterComponentEditor(TBoldAsVariantRenderer, TBoldAsVariantRendererEditor);
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
  RegisterPropertyEditor(TypeInfo(String), TBoldDrivenProperty, 'PropertyName', TPropertyNameProperty);
  RegisterPropertyEditor(TypeInfo(TComponent), TBoldDrivenProperty, 'VCLComponent', TVCLComponentProperty);

  RegisterComponentEditor(TBoldPropertyMapper,TBoldPropertyMapperComponentEditor);
  RegisterPropertyEditor(TypeInfo(String), TBoldPropertyMapping, 'VCLProperty', TVCLPropertyProperty);
  RegisterPropertyEditor(TypeInfo(TComponent), TBoldPropertyMapping, 'VCLComponent', TVCLComponentProperty);
end;

procedure RegisterComponentsOnPalette;
begin
{$IFNDEF BOLDCOMCLIENT}
  RegisterComponents(BOLDPAGENAME_MISC,
  [
    TBoldExceptionHandler
  ]);
{$ENDIF}

{$IFDEF BOLDCOMCLIENT}
  RegisterComponents('Bold COM Controls',
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
    TBoldPropertyMapper,
    TBoldAsStringRenderer,
    TBoldAsCheckboxStateRenderer,
    TBoldAsIntegerRenderer,
    TBoldAsVariantRenderer,
    TBoldAsFloatRenderer,
    {$IFNDEF BOLDCOMCLIENT}
    TBoldAsMLStringRenderer,
    TBoldDropTarget,
    {$ENDIF}
    TBoldAsViewerRenderer
  ]);
end;

procedure RegisterActionsInDelphi;
begin
  RegisterActions(BOLDACTIONGROUPNAME,
                  [
                   TBoldAction
                  ], nil);
end;


procedure Register;
begin
    RegisterComponentsOnPalette;
    RegisterEditors;
  RegisterActionsInDelphi;
end;

end.
