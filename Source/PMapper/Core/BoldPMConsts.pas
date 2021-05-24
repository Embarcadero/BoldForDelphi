unit BoldPMConsts;

interface

resourcestring
  sCallToAbstractMethodOnCustomMapper = '%s.%s: This method is abstract, implement custom method as %s.%s';
  sIllegalColumnIndex = '%s.%s: Illegal Column Index (%d)';
  sSomeColumnsNotInTable = '%s.%s: Some columns not found in table (%d:%s)';
  sLogIdAsString = 'Id: %s';
  sCommittingToDB = 'Committing changes to database';
  sRollingBackDB = 'Rolling back database changes';

//BoldPMappersDefault
  sOptimisticLockingFailedOnTimeStamp= 'Optimistic Locking failed on timestamp for the following Objects';
  sOptimisticLockingFailedForNonExisting = 'Optimistic Locking failed for the following objects of type %s because they did not exist in the database:';
  sOptimisticLockFailedLog = '%s: Id %s';
  sOptimisticLockingFailed = 'Optimistic Locking Failed for %s.%s (ID: %s) Column %d [%s]';
  sTypeAndIDColumnMissing = '%s.PMTemporalUpdate: Unable to find either type or ID column in SQL-statement (%s)';
  sUninvitedObjectReturnedFromDB = '%s.PMMultiPurposeRetrieveExactIdList: Database returned object we didn''t ask for (ID: %d)';
  sIDExactnessFailure = '%s.PMMultiPurposeRetrieveExactIdList: Got an Id with no or only approx class!';
  sLogFetchedXobjectsOfY = 'Fetched %d objects of class %s';
  sLogXObjectsOfYMissing = '%d objects of type %s were missing';
  sLogFetchedXObjectsOfYFromTableZ = 'Fetched %d IDs in class %s from table %s';
  sUnknownConditionType = 'Unknown type of condition (%s)';
  sCannotGetID = 'Can''t get ID (%s: %s)!';
  sUnableToFindDBIDForX = 'Unable to find databaseId for %s';
  sNotSupportedOnMember = '%s.GetChangePoints: not supported on this member';
  sUnsupportedMappingChange = 'Database Mapping has changed in an unsupported way for %s.%s. Number of columns has changed';
  sUnableToFindMappingForX = 'Unable to find database mapping for %s.%s';
  sColumnNotFoundInTable = '%s.VersionFromQuery: Column not found in table (%s)';

// BoldPSDescriptionsDefault
  sLogWritingFirstClock = 'Writing First Clock';
  sUnknownGenerationMode = '%s.%s: unknown database generation mode';
  sLogWritingFirstID = 'Writing First ID';
  sLogWritingFirstTimeStamp = 'Writing First TimeStamp';
  sLogWritingTableNames = 'Writing TableNames';
  sLogInitializingDefaultPS = 'Initializing Default Persistent Storage';
  sCommittingInitialData = 'Committing changes to initial data';

//BoldSQLSymbold
  sUnableToFindSymbolForX = 'Unable to find SqlSymbol for "%s"';
  sArgToOrderByMustBeMember = 'Argument to OrderBy must be a Member';
  sArgToOrderByMustHaveExactlyOneColumn = 'Argument to OrderBy must have exactly 1 column';
  sOrderByDescendingNotImplemented = 'OrderbyDescending not yet implemented';

//BoldPMappers
  sUnableToFindPMapperForClass = 'Unable to find PersistenceMapper (%s) for class %s';
  sConditionMustBeConditionWithClass = '%s.PMFetchClassWithCondition: Condition must be a ConditionWithClass, not a %s';
  sSuperClassNotVersioned = 'Class %s is versioned, but not it''s superclass %s';
  sSuperClassNotPersistent = 'Class %s has a nonpersistent superclass %s';
  sUnableToFindPMapperForX = 'Unable to find persistence mapper for %s.%s (type: %s, mapper: %s). possible reasons: ';
  sUnableReason1 = '* Typo';
  sUnableReason2 = '* The mapper is not correctly installed in the initialization clause';
  sUnableReason3 = '* The unit with the mapper is not included in the project';
  sUnableToFindPMapperForXY = 'Unable to find PersistenceMapper for %s.%s (mapper: %s)';
  sDuplicatePMappersForXInY = 'Duplicate memberMappers called "%s" in class %s';
  sObjectIDListIsEmpty = '%s.CommonSuperClassObjectMapper: ObjectIdList is empty';

//BoldPMappersAttributeDefault
  sAttributeMustNotAllowNullIfEmptyStringsStoreAsNull = 'String attribute must not allow NULL in the model if persistencemapper stores empty strings as NULL (%s.%s)';

// BoldPMappersSQL
  sNoDatabase = '%s: No database';
  sUnimplemented = 'unimplemented';
  sIllegalCall = '%s.%s: illegal call';

//BoldDbDataValidator
  sDBMustBeOpened = 'Database must be opened before Structurevalidation is performed!';
  sDBNotOpened = '// Database is not opened. Unable to perform any validation!';
  sProcessingClass = 'Processing class %s';
  sColumnsHaveUnsupportedType = '// Required column(s) %s has unsupported type(s)';
  sAddMissingEntries = '// add missing entries into %s';
  sLogObjectsMissingInParentTable = 'The following objects exists in %s, but not in parent table %s';
  sLogObjectsHaveDifferentType = 'The following objects have different type in table %s and %s';
  sLogObjectsMissingInChildtable = 'The following objects exists in %s, but not in child table %s';
  sLogLinkObjectsAreDupes = 'The following Linkobjects are duplicates:';
  sLogLinkObjectsLinkUnexistingObjects = 'The following Linkobjects (class %s) have links to nonexisting objects:';
  sCommentRemoveSpaceLinkObjects = '// Clean Linkobjects (%s) with space pointers';
  sLogBrokenLinkObjects = 'The following Linkobjects (class %s) have empty links in one direction:';
  sCommentRemoveBrokenLinkObjects = '// Clean Linkobjects (%s) with broken links';
  sLogObjectsWithBrokenLinks = 'The following objects of class %s have invalid links in %s:';
  sCommentCleanRelation = '// Clean relation (%s.%s) ';
  sLogObjectsWithWrongLinks = 'The following objects of class %s have singlelinks (%s) pointing to objects that don''t point back (they might point elsewhere):';
  sLogObjectsWithIllegalType = 'The following objects are in table %s, but with an illegal type:';
  sCommentRemoveObjectsWithIllegaltype = '// Remove objects with illegal type in table %s';

//BoldDbEvolutorScript
  sAddTable = 'Add table %s';
  sAddColumn = 'Add column %s.%s';
  sAddInstanceOfClassToTable = 'Add Instances of class %s to table %s';
  sMoveDataFromXtoY = 'Move data from %s.%s to %s.%s (dbtypes: %s)';
  sDeleteInstancesOfClassFromTable = 'Delete instances of %s from table %s';
  sDropIndex = 'Drop index %s';
  sDropColumn = 'Drop column %s.%s';
  sDropTable = 'Drop table %s';

//BoldDBEvolutor
  sPersistenceHandleActive = '%s.CalculateScript: PersistenceHandle %s is active. Unable to execute';
  sInitializingScript = 'Initializing Script';
  sUnableToFindDBID = '%s.TranslateClassExpressionName: Unable to find source dbid for %s';
  sMemberChangedMapper = 'Member %s.%s changed mapper (%s->%s). Column[s] (%s) in table %s will be dropped and (%s) will be created in table %s, data loss!';
  sClassBecameAbstract = 'Class %s is concrete in old model, but %s is Abstract in new model';
  sUnableToHandleInstancesOfAbstract = 'ERROR: There are instances of class %s, but %s is abstract in new model';
  sDataStoredInXForYWillBeLost = 'Data stored in column %s.%s for member %s.%s will be lost';
  sClassNoLongerExists = ' (class no longer exists)';
  sNewNameForClass = ' (class now called %s)';

//BoldPMappersLinkDefault
  sChildMappedLinkClassesNotSupported = '%s.%s: ChildMapped LinkObjects (%s) are not supported!';
  sOptimisticLockingFailedForTheFollowing = 'Optimistic Locking failed for %s.%s for the following objects';
  sCannotCallOnTransientClass = '%s.CompareField: Can not be called for this class, it is not stored';
  sLogFetchIDs = 'Fetched %4d ids for %4d nonembedded links %s.%s';

//BoldDbValidator
  sClassInDBNotInModel = 'Database contains a class %s with BoldType %d that is not in the model...';
  sClassWithoutDBID = 'Model contains a class %s that does not have a database id';
  sCorrectClassesWithNoID = 'There are classes with no database ID. Do you want to correct this now?';
  sAddingBoldDBType = 'Adding BoldDbType %d for %s';
  sDatabaseValidation = 'Database validation';
  sInconsistenciesFound = 'Inconsistencies found';
  sDBValidationFailed = 'Database validation failed: %s';
  sCannotValidateWithoutPHandle = 'Unable to perform validation, missing a PersistenceHandle';

//BoldPSdescriptionsSQL
  sUnknownPSParamsType = '%s.CreatePersistentStorage: Unknown type of PSParams: %s';
  sCreatingTables = 'Creating tables';
  sContinueDeleteBoldTables = 'Persistent Storage Seems to Contain a Bold Database. Continuing Will Permanently Destroy Data. Continue?';
  sDeleteTable = 'Delete Table';
  sDeleteNonBoldTable = 'Table %s does not seem to be a Bold table. Do you want to delete it.';
  sCleaningPS = 'Cleaning Persistent Storage';
  sCleaningPSAborted = 'Cleaning of Persistent Storage Aborted';
  sDeletingTableX = 'Deleting Table: %s';
  sKeepingTableX = 'Keeping Table: %s';
  sErrorDeletingTable = 'Error Deleting Table %s: %s';
  sAddingColumn = 'Adding column: %s';
  sAddingPrimaryIndex = 'Adding Primary index: %s';
  sCreatingTableX = 'Creating Table: %s';
  sLocatingTableX = 'Locating Table: %s';
  sTableXNotPresent = 'Table %s not Present';
  sAddingColumnInfo = 'Adding column: %s [%s, %d]';
  sColumnHasDefaultDBValue = 'Column %s has a default db value (%s)';
  sUnsupportedInTableCreationMode = 'This is not supported when generating the database using TTable, please use TQuery-method instead if this default value is required';
  sAddingIndex = 'Adding Index: %s on %s';

//BoldListenerThread
  sInitializationLineMissing = '%s. You must add the following line to the initialization section of the application: TBoldListenerCOMFactory.Create(ComServer)';

//BoldPersistenceControllerPassThrough
  sNextControllerMissing = '%s.getNextPersistenceController: NextPersistenceController not assigned';

//BoldDBInterfaces
  sCreateParamNotImplemented = '%s.Createparam: Not supported yet... override in this subclass needed';

//BoldIDAdderHandle
  sPropertyHasMoved = '%s.%s has been moved to component (%s.%s). Old value was "%s"';

//BoldPersistenceHandleDBreg
  sSchemaGenerated = 'Database schema generated';
  sSchemaGenerationFailed = 'Database schema generation failed: %s%s%s';
  sGenerateSchema = 'Generate Database Schema...';

//BoldListenerhandle
  sPropagatorHandleNotAssigned = '%s.StartListenerThread: PropagatorHandle not assigned';
  sPropagatorHandleNotConnected = '%s.StartListenerThread: PropagatorHandle not connected';
  sCannotChangeHandleWhenActive = '%s.SetPropagatorHandle: Can''t change handle on active listener';
  sValueOutOfRange = '%s.SetExtendLeaseAfter: Value must be between 10 and 90';

//BoldAbstractObjectUpgrader
  sDisplayNameUnassigned = '<unassigned>';
  sUpgradeIfOlderThan = 'Upgrade %s if older than %d';

//BoldDBEvolutorForm
  sDone = '%s: Done %s';
  sStarting = '%s: Starting %s';
  sWarnings = 'Warnings (%d)';
  sDetectingChanges = 'Detecting changes';
  sFailedToDetectChanges = 'Failed to detect changes: %s';
  sNoUpdateNeeded = 'No update needed!';
  sUpdateDatabase = 'Update database';
  sUpdateSuccessful = 'Database successfully updated';
  sInspectChanges = 'Please inspect the planned actions and decide if you want to execute them%s%sMake sure you back up your critical data before upgrading the database!!!';
  sEvolveFailed = 'Failed to Evolve database: %s%s%sSee LogWindow for executed statements';
  sExecutedStatements = 'The following statements were executed before evolution failed: ';

//BoldCustomBlobMapper
  sMemberNameTooLong = '%s.CreateFromMold: Too long MemberName (%s) - %d characters allowed';
  sVersionedClassesNotSupported = '%s.CreateFromMold: Versioned classes (%s) currently not supported by this blobmapper';
  sObjectNotInValueSpace = '%s.GetBlobValue: Trying to get blob value for %s.%s, but the object (ID: %s) is not in the ValueSpace';
  sValueNotInValueSpace = '%s.GetBlobValue: Trying to get blob value for %s.%s, but the value (ID: %s) is not in the ValueSpace';
  sValueNotBlob = '%s.GetBlobValue: Trying to get blob value for %s.%s, but the value was not a blob...';
  sCustomCompareRequired = '%s.CompareField: needs a custom Compare to do this';


//BoldSQLMappingInfo
  sMultipleDBTypes = '%s.AddTypeIdMapping: The class %s has multiple db types (%d and %d)';
  sLogWritingMappingToDB = 'Writing mapping information to database';

//BoldSQLNodes
  sChildMappedClassesNotSupported = 'ChildMapped classes not supported: %s';
  sExternalVarsCanOnlyBereferencedOnce = 'external variables (and self) can currently only be referenced once';

//BoldDbStructureValidator
  sMissingID = 'Missing ID';
  sAnIDWasMissing = 'A databaseId was missing for %s. Do you want to add an unused ID?';
  sCheckingTable = 'Checking table %s';
  sColumnMissing = 'Column missing: %s.%s (SQLType: %s)';
  sIndexMissing = 'Index missing: %s.(%s)';
  sNullValuesFound = '%d Null-values found in Table %s, Column %s: ';
  sTableDoesNotExist = 'Table %s does not exist';

implementation

end.

