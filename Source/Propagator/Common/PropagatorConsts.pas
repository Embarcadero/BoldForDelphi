unit PropagatorConsts;

interface

resourcestring
  sLogError = '%s.%s Error: %s)';
  sClientIDsNotAssigned = '%s.%s: ClientIds is not assigned';

//BoldAdvancedPropagator
  sInitializing = '%s.Initialize: Starting..........................................';
  sDestroying = '%s.Destroy: Closing down peacefully..........................................';

//BoldClientHandler
  sLogErrorAndID = '%s.%s Error: [ID=%d] %s)';
  sLoggingID = 'Log In: %s [ID=%d]';
  sExtendLeaseFailed = 'ExtendLease failed: [ID=%d] Client already disconnected ';
  sLogOff = 'Log Off: %s [ID=%d] [Pkg: %d Ev: %d Int: %s Login: %s (%s ago) Status: %s]';
  sLeaseExpired = 'Lease Expired: %s [ID=%d] [Pkg: %d Ev: %d Int: %s Last: %s ago Login: %s (%s ago) Status: %s]';
  sClientDisconnected = 'Disconnected: %s [ID=%d] Login: %s (%s ago) Status: %s';
  sClientDisconnected_Long = 'Disconnected: %s [ID=%d] [Pkg: %d Ev: %d Int: %s Last: %s ago Login: %s (%s) Status: %s]';
  sErrorCode = '  ErrorCode=%d';
  sInvalidListener = '%s.ValidateClientInterface: ClientListener not valid for Client %s; Error=%s';
  sListenerInterfaceMissing = '%s.ValidateClientInterface Error: [ID=%s] Listener interface missing';
  sClientHasRecovered = '!!! Client %s (%d) has recovered and received %d messages';
  sOK = 'OK';
  sNotReceiving = 'Not Receiving';
  sRecovered = 'Recovered';
  sUnknown = '<Unknown>';

//BoldClientNotifierHandler
  sFailedToSendToClient = '%s.SendToClient: could not send events to client';
  sDisconnectMsg = ' [Disconnected]';
  sClientFailure = '%s.Execute: Client %s (ID=%d)%s failed to receive messages (%d msgs): %s. RegistrationTime: %s (%s ago), LeaseTimeout: %s (%s left)';

//BoldLockManagerAdmin
  sLocksNotAssigned = '%s.LocksForClients: Locks is not assigned';
  sLockDurationNotAssigned = '%s.LocksForClients: LockDurations is not assigned';

//BoldPropagatorCleanup  
  sWindowProcError = 'Error in WindowProc %s';

//BoldPropagatorMainForm
  sFormCaption = '%s Console';
  sClientID = 'ClientID';
  sName = 'Name';
  sRegTime = 'RegTime';
  sTimeOut = 'Timeout';
  sSubscriptions = 'Subscriptions';
  sQueue = 'Queue';
  sLongestInt = 'Longest Int';
  sLast = 'Last (ago)';
  sPkg = 'Pkg';
  sEv = 'Ev';
  sStatus = 'Status';
  sLost = 'Lost';
  sFile = 'File: %s';
  sNoINIFile = 'INI file: N/A';
  sINIFile = 'INI file:';
  sStarted = 'Started: %s';
  sUptime = 'Uptime: %d days %s';
  sHangon = 'Hang on...';
  sCli = 'Cli';
  sTot = 'Tot';
  sSubs = 'Subs';
  sInQ = 'InQ';
  sInQPeak = 'InQ^';
  sOutQ = 'OutQ';
  sAdded = 'Added';
  sSent = 'Sent';
  sBytesPerSubscription = '%0.2f bytes per subscription';
  sMemAllocated = 'Allocated %6.2f Mb';
  sRefresh = 'Refresh';
  sDL = 'DL?';


//BoldPropagatorServer
  sInvalidCommandLineArgs = 'Invalid command line argument %s';
  sAppIDNotFound = 'AppId not found, run DCOMCNFG to add appid to registry';

//BoldPropagatorUIManager  
  sPropagatorMustBeShutDown = 'Propagator must be shut down';

//BoldSubscriptionHandler
  sOutputQueueHandlerNotAssigned = '%s.getQueueHandler: AbstractOutputQueueHandler is not assigned.';


implementation

end.
