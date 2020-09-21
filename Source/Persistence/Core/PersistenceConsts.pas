unit PersistenceConsts;

interface

resourcestring
  sGenerateSchema = 'Generate Database Schema';

//BoldPersistenceNotifier
  sFetchingObjects = 'Fetching objects';
  sRetrievingIDs = 'Retrieving object IDs';
  sUpdatingDB = 'Updating database';


//BoldPersistenceController
  sPMTimeForTimeStampNotSupported = 'PMTimeForTimestamp not supported by %s';
  sPMTimeStampForTimeNotSupported = 'PMTimestampForTime not supported by %s';

//BoldPersistenceHandle
  sNotAllowedOnActiveHandle = '%s Not allowed on active PersistenceHandle';
  sOptimisticLockingFailedForNObjects = 'Optimistic locking failed for %d objects';

//BoldAbstractPersistenceHandleDB
  sCannotGenerateWhenHandleIsActive = 'Can not generate database schema when the PersistenceHandle is Active';
  sModelComponentMissing = '%s.CreateDataBaseSchema: Can not create database schema for %s without a Model-component';
  sCreateSchema = 'Create DatabaseSchema';
  sSchemaGenerationAborted = 'Generation of Database Schema Aborted (%s)';
  sCannotGetPControllerWithoutModel = '%s.GetPersistenceController: Can not get a PersistenceController without a Model';
  sCannotActivate_UpgraderMismatch = '%s.SetActive: Cannot activate, there is an UpgraderHandle but the Model does not have UseModelVersion true';
  sClockStringFormatError = '%s.SetClockLogGranularity: string is not properly formatted. Should be <hrs>:<mins>:<secs>.<msecs>';
  sCannotSetWhenHandleIsActive = '%s.%s: Cannot set this property when the handle (%s) is active';
  sSQLDatabaseConfigMissing = '%s: Unable to %s. There is no SQLDatabaseConfig available';

//BoldPersistenceControllerDefault
  sNotActive = '%s.%s: Not Active';

//BoldPersistenceHandleDB
  sNoDatabaseAdapterAvailable = '%s: Unable to %s. There is no DatabaseAdapter available';
  sCannotActivateWithoutDBAdapter = '%s.SetActive: Can not set persistence handle to active since it is not connected to a database adapter';

//BoldPersistenceHandleFile
  sObjectNotInFile = '%s.PMFetch: Trying to fetch an object that does not exist in file (%s)';
  sSQLNotSpokenHere = 'This filehandler does not understand SQL, ignoring condition and orderby...';
  sUnknownConditionType = 'Unknown conditiontype: %s';
  sPreconditionsNotSupported = '%s.PMUpdate: Preconditions (%s) not supported in this component';

//BoldPersistenceHandleFileXML
  sModelRequired = '%s.CreatePersistenceController: Unable to create, model is missing.';
  sXMLParseError = 'Error reading/parsing XML file';

//BoldSnooper
  sEnqueuerNotEnabled = 'Call to %s failed. Propagator Enqueuer not enabled';
  sClientNotRegistered = 'Call to %s failed. Client not registered with propagator';
  sInvalidParameter = 'Call to %s failed. Invalid parameter';
  sClientNotReceivingEvents = 'Call to %s OK, but client is currently not receiving events';
  sCallFailed = 'Call to %s failed. Error: %s';
  sCannotAcquireLock = '%s.%s: Cannot acquire Database Lock';
  sLockManagerNotAssigned = '%s.GetLockManager: LockManager not assigned';

//BoldBatchUpgrader
  sUpgradeNotSupported = '%s.Create: The SystemPersistenceMapper does not support object upgrading!';
  sObjectIDListNotExact = '%s.UpgradeObjectIdList: ObjectIdlist not Exact';
  sObjectIDListNotHomogenous = '%s.UpgradeObjectIdList: ObjectIdlist not homogenous';
  sDBMustBeOpened = '%s.UpgradeObjects: The database must be opened first';

//BoldSnooperHandle
  sBoldModelNotAssigned = '%s.CreatePersistenceController: BoldModel not Assigned';
  sPropagatorHandleNotAssigned = '%s.PropagatorHandle not assigned';
  sPropertyMoved = '%s.%s has been moved to component (%s.%s). Old value was "%s"';



implementation

end.