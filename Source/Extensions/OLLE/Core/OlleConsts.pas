unit OlleConsts;

interface

resourcestring
//BoldOLLEController
  sCannotChangePersistenceWhenActive = '%s: Can not change Persistent-property when the OLLE system is active';

//BoldOLLEDistributableObjectHandlers
  sObjectNotForeign = '%s.EnsureForeignInfo: Object is not a foreign object';
  sWrongOwner = '%s.EnsureForeignInfo: Wrong owner';
  sObjectsAlreadyBeingCheckedIn = '%s.StartCheckIn: Already checking in objects for this persistent storage';
  sObjectNotOwned = '%s.%s: Object is not an owned object';
  sCannotFailIndividualObjects = '%s.VerifyAssociations: Failing inidividual objects not implemented';
  sUnresolvedLink = 'Operation failed: Unresolved link';
  sMissingObjectsNotImplemented = 'Missing Objects not implemented';
  sMemberNotSingleLink = '%.ResolveBrokenLink: Member is not a singlelink';
  sSynchInProgress = '%s.GetSynch: There is already an ongoing synch that must either be acknowledged or failed.';

// BoldOLLEHandlesComponentEditor
  sGenerateOLLEDB = 'Generate OLLE database';
  sNonexistingAction = 'Nonexisting action';

implementation

end.
