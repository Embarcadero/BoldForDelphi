{ Global compiler directives }
{$include bold.inc}
unit BoldFireDACConsts;

interface

resourcestring
//BoldDatabaseAdapterFireDAC
  sAdapterNotConnected = '%s.GetDatabaseInterface: The adapter is not connected to an FireDAC connection';
  sCreatedNewAdapter = 'Created a new DatabaseAdapterFireDAC';
  sCanOnlyTransferToFireDACAdapter = 'The persistencehandle is connected to a %s, properties can only be transfered to a TBoldDatabaseAdapterFireDAC';
  sCreatedNewDB = 'Created a new FireDACDatabase';
  sCouldNotTransferConnectionString = 'Connection string settings could not be transferred to the new FireDAC connection: ';
  sTransferManually = 'Please transfer these manually!';

implementation

end.
