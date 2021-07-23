unit BDEConsts;

interface

resourcestring
//BoldBDEBatchInterfaces
  sOnlyTestedWithInformix = 'Batch operations has only been tested with Informix%s';
  sBatchFailure = '%sBatch operation failed: %s%sSQL: %s';

//BoldBDEInterfaces
  sSQLFailure = '%s%sSQL: %s';

//BoldDatabaseAdapterBDE
  sAdapterNotConnected = '%s.GetDatabaseInterface: The adapter is not connected to a database';

//BoldPersistenceHandleBDE
  sCreatedNewAdapter = 'Created a new DatabaseAdapterBDE';
  sPropertiesCannotBeTransferred = 'The persistencehandle is connected to a %s, properties can only be transfered to a TBoldDatabaseAdapterBDE';
  sCreatedNewDB = 'Created a new Database';      

implementation

end.
