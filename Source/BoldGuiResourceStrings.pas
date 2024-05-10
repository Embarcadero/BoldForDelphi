unit BoldGuiResourceStrings;

interface

uses
  BoldCoreConsts;

resourcestring
  SCantGetIntegerValue = 'Can''t get integer value from element (%s)';
  SCantSetIntegerValue = 'Can''t set integer value om element (%s)';
  SBoldInvalidName =  '''%s'' is not a valid name';

  SBoldInactiveFollowerNoRenderData = 'Inactive Follower has no renderer data';

  SRepresentationDefault = 'Default';
  SRepresentationShort = 'Short';
  SRepresentationLong = 'Long';

  SCannotDragOverMultipleObjects = 'Can''t DragOver multiple objects';
  SLinkAlreadyAssigned = 'Link already assigned';
  SCannotChangeStateWithModifiedValue = 'Can''t Change State with Modified Value';

  SValueReadOnly = 'Can''t change value. Value is read only';

//BoldCheckBox
  sStateNotModifiable = '%s.State: Not modifiable';

//BoldComboBox
  sHandleMustBeReferenceHandle = 'The BoldHandle property must be a TBoldReferenceHandle when BoldSelectChangeAction is bdscSetReference';
  sChangeActionCannotBeSetReference = 'The BoldSelectChangeAction property can not be bdscSetReference when BoldHandle is not a TBoldReferenceHandle';

// BoldDataSet
  sNoRecordsFound = 'No records found!';
  sCannotInitializeFieldsWithoutContext = 'Cannot initialize fields - dataset %s has no context';
  sInvalidFieldExpression = 'Invalid field expression %s';
  sInvalidFieldName = 'Invalid fieldname %s';

// BoldEdit, BoldMemo, BoldRichEdit
  sTextNotModifiable = '%s.Text: Not modifiable';

//BoldGrid
  sCannotCreateColumnOutsideCollection = '%s.Create: Cannot create TBoldGridColumn outside a TBoldGridColumns';
  sCaptionClassName = 'Class name';
  sCaptionType = 'Type';
  sCaptionAsString = 'AsString';
  sClosePopup = '&Close Popup';

// BoldGridRTColEditor
  sNewColumn = '<new column>';
  sExecuteGridNotAssigned = '%s.Execute: BoldCustomGrid not assigned';

// BoldImage
  sUnknownFileFormat = '%s.LoadFromFile: File format unknown: File: ''%s''';

// BoldImageBitmap
  sBitMapImage = 'Bitmap image';

// BoldImageJPeg
  sJpegImage = 'JPEG image';

// Navigator hints
  SNavHintFirst = sFirst;
  SNavHintPrior = sPrev;
  SNavHintNext = sNext;
  SNavHintLast = sLast;
  SNavHintNew = sAddNew;
  SNavHintDelete = sDelete;
  SNavHintMoveUp = sMoveUp;
  SNavHintMoveDown = sMoveDown;
  sDeleteQuestion = 'Delete "%1:s"?';
  sUnlinkQuestion = 'Unlink "%1:s" from "%2:s"?';
  sRemoveQuestion = 'Remove "%1:s" from the list?';

// BoldPropertiesController
  sCannotSetIntegerProperty = 'Could not set the integer %s property to value %s. (%s)';
  sCannotSetProperty = 'Could not set the %s property to value %s. (%s)';

// BoldStringsPropertyController
  sStringsControlledByOtherMeans = 'Strings property controlled by "%s" changed by other means';

// BoldCheckBoxStageControlPack
  sCannotSetValue = '%s: Can''t set value';

//BoldControlPack
  sClassHasNoSubfollowers = '%s: This class has no subfollowers';
  sCannotModifyValue = '%s.DefaultHoldsChangedValue: Can''t Modify Value';
  sReplaceNotImplemented = '%s.DefaultDragDrop: Replace not implemented yet';
  sUnknownRequestedEvent = '%s.Receive: Unknown RequestedEvent (%d)';
  sFollowerStateError = '%s.%s: Follower state error';
  sCannotDisplayInThisOrder = '%s.Display: Can not display because there is an owning follower that must be displayed before';
  sDisplayError = '%s%soccured when displaying component %s';
  sControllerNotAssigned = '%s.GetAssertedController: Controller not assigned';

//BoldDateTimeControlPack
  sCannotGetDateTimeValueFromElement = 'Can''t get datetime value from element (%s)';
  sCannotSetDateTimeValueOnElement = 'Can''t set datetime value on element (%s)';

//BoldFloatControlPack
  sCannotGetFloatValueFromElement = 'Can''t get float value from element (%s)';
  sCannotSetFloatValueOnElement = 'Can''t set float value on element (%s)';

//BoldGenericListControlPack
  sCouldNotGetController = '%s%soccured when getting controller for component %s';

//BoldListListControlPack
  sDraggedObjectsNotCleared = '%s.DefaultStartDrag: TBoldGUIHandler.DraggedObjects not cleared';

//BoldMLRenderers
  sCannotSetStringValue = '%s.DefaultSetAsString: Can''t set string value';

//BoldNodeControlPack
  sInvalidNodeName = '%s.SetDefaultNodeDescriptionName: ''%s'' is not a valid name';

//BoldStringControlPack
  sStringValidationFailedExtended = 'String validation failed for %s: %2:s';
  sStringValidationFailed = 'String validation failed';
  sUnknownReason = 'Unknown reason';

//BoldViewerControlPack
  sViewerNotAvailable = 'Viewer not available (%s)';

//BoldComboBoxPropertyEditors
  sComponentNotComboBox = '%s.GetContextType: Incoming component is not a BoldComboBox';

//BoldControlPackPropertyEditors
  sInvalidPropertyValue = 'Invalid property value';
  sEditNodeDescriptions = '&Edit Node Descriptions...';

//BoldPropertiesControllerPropertyEditors
  sEditDrivenProperties = 'Edit driven properties...';

//BoldGridPropertyEditors
  sEditColumns = '&Edit Columns';
  sCreateDefaultColumns = 'Create Default Columns';
  sClearAllColumns = 'Clear all Columns';

//BoldDataSetPropertyEditors
  sFieldsEditor = 'Fields Editor...';
  sCreateDefaultFields = 'Create default fields';
  sClearAllFields = 'Clear all fields';

//BoldAFPDefault
  sClearLinkFirst = 'Clear the link first';
  sCreatingNewNotSupported = 'Creating new objects like this only supported for relations to non-subclassed classes';
  sAreYouSureToDelete = 'Are you sure you want to delete this object??';
  sAreYouSureToUnlink = 'Are you sure you want to unlink this object??';
  sNavigate = 'Navigate';
  sCreateNew = 'Create new...';
  sAddExisting = 'Add existing...';
  sUnlink = 'Unlink';
  sHistoricObject = 'Historic object - Timestamp %d';
  sClose = 'Close';
  sApply = '&Apply';
  sCancel = '&Cancel';
  sOK = '&OK';
  sIllegalApplyPolicy = 'Illegal applypolicy';
  sIncludeChangesInNonEmbeddedLinks = 'Include changes in non embedded links';
  sObjectNotVersioned = '  This object is not versioned, no history can be shown';
  sToDisableTab_1 = '  To disable this tab, set the variable ';
  sToDisableTab_2 = '  "BoldShowHistoryTabInAutoForms" to false';

implementation

end.
