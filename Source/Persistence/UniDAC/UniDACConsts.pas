{ Global compiler directives }
{$include bold.inc}
unit UniDACConsts;

interface

resourcestring
//BoldDatabaseAdapterUniDAC
  sAdapterNotConnected = '%s.GetDatabaseInterface: The adapter is not connected to an UniDAC connection';
  sCreatedNewAdapter = 'Created a new DatabaseAdapterUniDAC';
  sCanOnlyTransferToUniDACAdapter = 'The persistencehandle is connected to a %s, properties can only be transfered to a TBoldDatabaseAdapterUniDAC';
  sCreatedNewDB = 'Created a new UniDACDatabase';
  sCouldNotTransferConnectionString = 'Connection string settings could not be transferred to the new UniDAC connection: ';
  sTransferManually = 'Please transfer these manually!';

implementation

end.
