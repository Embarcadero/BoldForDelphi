unit BoldHandleReg;

interface

procedure Register;

implementation

uses
  SysUtils,
  BoldUtils,
  ActnList,
  Classes,
  DesignIntf,
  BoldElements,
  BoldDefs,
  BoldIDEConsts,
  BoldDefsDT,
  BoldOclVariables,
  BoldAbstractPropertyEditors,
  BoldPropertyEditors,
  BoldHandlePropEditor,
  BoldPlaceableSubscriber,
  BoldActions, BoldHandleAction, BoldListActions, BoldUndoActions,
  BoldSystem,
  BoldAbstractModel,
  BoldHandles,
  BoldDerivedHandle,
  BoldExpressionHandle,
  BoldRootedHandles,
  BoldCursorHandle,
  BoldListHandle,
  BoldVariableHandle,
  BoldSystemHandle,
  BoldSQLHandle,
  BoldFilteredHandle,
  BoldSortedHandle,
  BoldReferenceHandle,
  BoldVariableDefinition,
  BoldOclRepository,
  BoldTypeNameSelector,
  BoldModelAwareComponentEditor,
  BoldUnloaderHandle
  ;

{.$R BoldHandleReg.res}


procedure RegisterActionsInDelphi;
begin
                          // marco
//  RegisterActions(BOLDACTIONGROUPNAME,
//                  [
//                   TBoldUpdateDBAction,
//                   TBoldActivateSystemAction,
//                   TBoldFailureDetectionAction,
//                   TBoldListHandleAddNewAction,
//                   TBoldListHandleDeleteAction,
//                   TBoldListHandleFirstAction,
//                   TBoldListHandleLastAction,
//                   TBoldListHandleNextAction,
//                   TBoldListHandlePrevAction,
//                   TBoldListHandleMoveUpAction,
//                   TBoldListHandleMoveDownAction,
//                   TBoldSetCheckPointAction,
//                   TBoldUndoAction,
//                   TBoldRedoAction
//                  ], nil);
end;

procedure RegisterComponentsOnPalette;
begin
  RegisterComponents(BOLDPAGENAME_HANDLES,
    [
    TBoldSystemHandle,
    TBoldSystemTypeInfoHandle,
    TBoldExpressionHandle,
    TBoldDerivedhandle,
    TBoldVariableHandle,
    TBoldListHandle,
    TBoldSQLHandle,
    TBoldCursorHandle,
    TBoldReferenceHandle,
    TBoldOclVariables,
    TBoldUnloaderHandle
    ]);

  RegisterComponents(BOLDPAGENAME_MISC,
    [
    TBoldFilter,
    TBoldComparer,
    TBoldPlaceableSubscriber
    ]);

  RegisterComponents(BOLDPAGENAME_DEPRECATED,
    [
      TBoldVariableDefinition
    ]);

end;

procedure RegisterEditors;
begin
  RegisterPropertyEditor(TypeInfo(TBoldAbstractModel), TBoldSystemTypeInfoHandle, 'BoldModel', TBoldComponentPropertyIndicateMissing);  // do not localize
  RegisterPropertyEditor(TypeInfo(TBoldSystemTypeInfoHandle), TBoldAbstractSystemHandle, 'SystemTypeInfoHandle', TBoldComponentPropertyIndicateMissing);   // do not localize

  RegisterPropertyEditor(TypeInfo(TBoldElementHandle), TBoldRootedHandle, 'RootHandle', TBoldRootedHandleRootHandlePropertyEditor);    // do not localize
  RegisterPropertyEditor(TypeInfo(TBoldElementFilter), nil, '', TBoldElementFilterMethodProperty);
  RegisterPropertyEditor(TypeInfo(TBoldElementSubscribe), nil, '', TBoldElementSubscribeMethodProperty);
  RegisterPropertyEditor(TypeInfo(TBoldElementCompare), nil, '', TBoldElementCompareMethodProperty);
  //TBoldPlaceableSubscriber
  RegisterPropertyEditor(TypeInfo(TBoldSubscribeToElementEvent), nil, '', TBoldElementSubscribeMethodProperty);

  RegisterComponentEditor(TBoldListHandle, TBoldOCLComponentEditor);
  RegisterComponentEditor(TBoldExpressionHandle, TBoldOCLComponentEditor);

  RegisterPropertyEditor(TypeInfo(TBoldExpression), TBoldListHandle, 'Expression', TBoldOCLExpressionForOCLComponent);                // do not localize
  RegisterPropertyEditor(TypeInfo(TBoldExpression), TBoldListHandle, 'MutableListExpression', TBoldOCLExpressionForOCLComponent);     // do not localize
  RegisterPropertyEditor(TypeInfo(TBoldExpression), TBoldExpressionHandle, 'Expression', TBoldOCLExpressionForOCLComponent);          // do not localize

  RegisterPropertyEditor(TypeInfo(string), TBoldReferenceHandle, 'StaticValueTypeName', TBoldTypeNameSelectorPropertyForAllTypes);    // do not localize
  RegisterPropertyEditor(TypeInfo(string), TBoldRootedHandle, 'RootTypeName', TBoldTypeNameSelectorPropertyForAllTypes);              // do not localize
  RegisterPropertyEditor(TypeInfo(string), TBoldVariableHandle, 'ValueTypeName', TBoldTypeNameSelectorPropertyForVariableHandle);     // do not localize
  RegisterPropertyEditor(TypeInfo(string), TBoldDerivedHandle, 'ValueTypeName', TBoldTypeNameSelectorPropertyForAllTypes);            // do not localize
  RegisterPropertyEditor(TypeInfo(string), TBoldSQLHandle, 'ClassExpressionName', TBoldTypeNameSelectorForSQLHandle);                 // do not localize
  // Propeditor för listhandle.expression
  // Propeditor för Exprhandle.expression

  RegisterComponents(BOLDPAGENAME_MISC, [TBoldOclRepository]);
  RegisterPropertyEditor(TypeInfo(string), TBoldOclDefinition, 'Context', TBoldTypeNameSelectorForOclDefinition);  // do not localize
  RegisterPropertyEditor(TypeInfo(string), TBoldOclDefinition, 'Expression', TBoldOclExpressionForOclDefinition);  // do not localize

  RegisterComponentEditor(TBoldOclVariables, TBoldOclVariablesEditor);
  RegisterComponentEditor(TBoldOCLRepository, TBoldOCLRepositoryEditor);
  RegisterComponentEditor(TBoldSystemHandle, TBoldModelAwareComponentEditor);
  RegisterComponentEditor(TBoldSystemTypeInfoHandle, TBoldModelAwareComponentEditor);
  RegisterComponentEditor(TBoldDerivedHandle, TBoldModelAwareComponentEditor);
  RegisterComponentEditor(TBoldVariableHandle, TBoldModelAwareComponentEditor);
  RegisterComponentEditor(TBoldSQLHandle, TBoldModelAwareComponentEditor);
  RegisterComponentEditor(TBoldCursorHandle, TBoldModelAwareComponentEditor);
  RegisterComponentEditor(TBoldReferenceHandle, TBoldModelAwareComponentEditor);
end;

procedure Register;
begin
  RegisterComponentsOnPalette;
  RegisterEditors;
  RegisterActionsInDelphi;
end;

end.


