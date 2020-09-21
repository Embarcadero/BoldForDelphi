unit HandlesConst;

interface

resourcestring
  sIllegalTypeSelected = '%s.GetStaticBoldType: Only lists and attributes are allowed as types (expr: %s)';
  sNoSystemHandle = '%s.%s: %s has no SystemHandle';
  sCircularReference = '%s.%s: %s can not be linked to %s. Circular reference';
  sNameNotUnique = 'Invalid Name: %s Not Unique';

// BoldAbstractListHandle
  sNoPreviousElement = '%s: No previous element';
  sNoNextElement = '%s: No next element';
  sNoCurrentElement = '%s.RemoveCurrentElement: No current element';
  sCannotRemoveCurrentFromImmutable = '%s: Can not remove current Element from an immutable list (in %s)';
  sCurrentElementNotBoldObject = '%s.CurrentBoldObject: Current element is not a TBoldObject';
  // BoldSQLHandle
  sSystemHandleNotActive = '%s.ExecuteSQL: Systemhandle is not active';
  sCannotCreateListDueToInvalidType = '%s.EnsureList: Unable to create list (%s), can''t find valid type';

// BoldHandlePropEditor
  sComnponentNotOCLDefinition = '%s.GetContextType: Component is not a TBoldOclDefinition';

// BoldManipulators
  sCannotSetMultiLinkFromValue = 'Can''t set Multilink from value';
  sCannotSetObjectFromValue = 'Can''t set Object from value';
  sElementLacksTypeInfo = '%s.DefaultTagForElement: Element lacks type information';
  sUnknownMapping = 'Unknown mapping: %s';

// BoldSystemHandle
  sCannotUpdateDatebaseWithoutSystem = '%s.UpdateDatabase (%s) cannot be invoked without a system';
  sUnableToActivateSystemWithoutTypeInfoHandle = 'Unable to activate a system without a SystemTypeInfoHandle (%s)';
  sUnableToFindTypeInfoHandle = 'Unable to find a SystemTypeInfo. Possible misstakes: Forgot to connect the SystemTypeInfoHandle to a Model, CreationOrder of the forms/dataModules';
  sUnableToActivateSystem = 'Unable to activate system. Initialization errors in StaticSystemTypeInfo:%s%s';
  sPersistenceHandleChangedOnRunningSystem = 'PersistenceHandle was changed on a running system.';
  sModelChangedOnRunningSystem = 'The model changed in a running system';
  sRegionDefinitionsRemovedFromRunningSystem = 'RegionDefinitions were removed from a running system.';
  sPanicShutDown = '%s%sSystem Panic shut down. %d objects discarded';

//BoldVariableHandle
  sOnlyListsAndAttributeTypesAllowed = '%s.GetStaticBoldType: Only lists and attributes are allowed as types (expr: %s)';
  sValueTypeNameInvalid = '%s.Getvalue: The ValueTypeName of %s does not seem to valid (%s)';

//BoldXMLProducers
  sManipulatorNotAssigned = '%s.%s: BoldManipulator property not assigned';

//BoldLockingHandles
  sCannotActivateWithoutListener = '%s.Activate: Cannot activate Locking without a listener. Set the Listener property of the %0:s';
  sCannotActivateWithoutLockManager = '%s.Activate: Cannot activate Locking without a LockManager. Set the LockManager property of the %0:s';

//BoldHandles
  sNotAllowedOnActiveHandle = '%s: Not allowed to change the systemTypeInfoHandle on an active system';

//BoldActions
  sUpdateDB = 'Update DB';
  sOpenSystem = 'Open system';
  sCloseSystem = 'Close system';
  sThereAreDirtyObjects = 'There are dirty objects. Do you want to save them before shutting down';
  sClosingWithDirtyObjects = 'Closing system with dirty objects';

//BoldRootedHandles
  sInternalRootHandle_CircRef = '%s: Cannot set handle - circular reference';

//BoldListActions
  sNext = 'Next';
  sPrev = 'Prev';
  sFirst = 'First';
  sLast = 'Last';
  sUnknownDeleteMode = 'Unknown delete mode';
  sDelete = 'Delete';
  sAddNew = 'Add New';
  sMoveUp = 'Move up';
  sMoveDown = 'Move Down';

//BoldUndoActions
  sSetCheckPoint = 'Set Check-point';
  sUndo = 'Undo';
  sRedo = 'Redo';

//BoldCursorHandle
  sIndexOutOfBounds = '%s.SetCurrentIndex: Index out of bounds. Valid range from -1 to %d. Attempted to set %d';

//BoldOCLVariables
  sIllegalCharsInName = 'Invalid variable name, only alphanum characters and underscore valid';

//BoldExpressionHandle
  sDeriveAndSubscribeFailed = '%s.DeriveAndSubscribe (%s): Failed with message: %s';

//BoldOCLRepository
  sRepositoryHasNoSystemHandle = '*** OclRepository %s%s has no SystemHandle';
  sSystemHandleHasNoTypeInfo = '*** SystemHandle of OclRepository %s%s has no TypeInfo';


implementation

end.