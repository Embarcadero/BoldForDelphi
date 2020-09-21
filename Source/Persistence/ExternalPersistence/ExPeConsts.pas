unit ExPeConsts;

interface

resourcestring
  sKeyTypeNotAutoHandled = 'Keytype not handled automatically: %s.%s';

  //BoldAbstractExternalPersistenceHandle
  sValueNotCurrency = '%s.CurrencyValueForObject: The value (%s) is not a currency';
  sValueNotDate = '%s.DateValueForObject: The value (%s) is not a date';
  sValueNotInteger = '%s.IntValueForObject: The value (%s) is not an integer';
  sKeyNotInteger = '%s.KeyIntForObject: The key is not an integer';
  sKeyNotString = '%s.KeyStringForObject: The key is not a string';
  sValueNotString = '%s.StringValueForObject: The value (%s) is not is not a string';

//BoldAbstractPartiallyExternalPC
  sNotSupportedWithMultipleKeys = '%s.ExternalKeysToInternalSQL: Automatic SQL-generation only supported for classes with 1 (one) external key (class %s has multiple)';
  sNotSupportedWithNoKeys = '%s.ExternalKeysToInternalSQL: Automatic SQL-generation only supported for classes with 1 (one) external key (class %s has none!)';
  sNoSuchRole = '%s.FindMoldRoleByName: There is no role called %s in class %s';

//BoldAbstractPartiallyExternalPH
  sInvalidClassName = '%s.GetObjectIdByExternalKey: Invalid class name (%s)';

//BoldExternalPersistenceControllerDataSet
  sUnknownDataType = 'Unknown data type';
  sLinkToUnconfiguredTable = 'External link to unconfigured table %s';
  sUnknownVarTypeLoadingID = 'Unknown vartype when loading an external ID for multilink %s.%s';
  sUnknownVarTypeLoadingSingleID = 'Unknown vartype when loading an external ID for Singlelink %s.%s';
  sUnknownVarTypeLoadingObject = 'Unknown vartype when loading an external ID for %s';

  sRoleHasNoColumnNames = 'Role %s does not have any column names!';
  sRoleEndCountMismatch = 'Role %s does not have an equal amount of columns on both ends!';
  sObjectNoLongerInDB = 'Object no longer exists in database';

//BoldExternalPersistenceControllerEventDriven
  sAssignKeyValueRequiresOneKey = 'AssignKeyValue only supported automatically for classes with one external key: %s';
  sCreateObjectsNotAllowed = 'Creating new objects of type %s not supported/allowed';
  sDeleteObjectsNotAllowed = 'Deleting objects of type %s not supported/allowed';
  sReadObjectNotImplementedForClass = 'Event ReadObject is not implemented for class %s';
  sGetExternalKeyNotImplementedForClass = 'Getting external keys for class %s not implemented';
  sModifyObjectsNotAllowed = 'Modifying objects of type %s not supported/allowed';

//BoldExternalPersistenceHandlesReg
  sEditConfiguration = 'Edit configuration';
  sCannotCreateNameNow = 'Can not create name for eventhandler. Assign an expressionname for the config-item first';
  sBoldModelNotAssigned = 'BoldModel is not assigned!';

implementation


end.