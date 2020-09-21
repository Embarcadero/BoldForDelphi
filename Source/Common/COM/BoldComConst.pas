unit BoldComConst;

interface

resourcestring
  sCannotLoadLibrary = 'Unable to load type library (%s)';
  sXMLRequestNotAssigned = '%s.Get: XMLRequest not assigned';
  sUnableToLoadTypeLibBoldSoap = '%s.Create: Unable to load type library LIBID_BoldSOAP';
  sNoAdapterRegistered = 'No adapter registered for %s';

// BoldApartmentThread
  spciInfoNotInitialized = 'ApartmentThreadWndProc: pciInfo not initialized';

//BoldComAdapter
  sUnsupportedInterface = '%s: Unsupported interface';
  sNoAdaptee = '%s: No adaptee';
  sCannotAdaptNonSubscribables = '%s.SetAdaptee: Can not adapt objects that are not subscribable (such as %s)';

// BoldThreadedComObjectFactory
  sApartmentThreadTimedOut = 'Apartment thread timed out';

// BoldComClient
  sConnectionStateError = 'Connection state error.';

// BoldComClientHandles
  sUnknownGetHResult = 'unknown type in GetHResult';
  sCannotConnectNoServer = 'Cannot connect, no server specified.';
  sCannotConnectBadServerName = 'Cannot connect, invalid server name.';
  sCannotChangePropertyWhenActive = 'Cannot change %s on active connection.';

// BoldComServerHandles
  sCannotChangePropertyAtRuntime = '%s: Cannot change %s at run-time';
  sInvalidCLSID = '%s: Invalid CLSID';
  sInvalidName = '%s: Invalid Name';

// BoldXMLDispatcher
  sXMLRequestIsNil = '%s.DispatchAction: request is nil';

//BoldXMLDispatcherEditor
  sEditActions = 'Edit actions...';

//BoldCOMConnection
  sUnableToLoadTypeLib = 'Unable to load type library (BoldComConnection)';

//BoldComServer
  sCallStats = 'Total:%.1fs Min:%.0fms Max:%.0fms Avg:%.0fms Count:%d';

//BoldComClientHandles
  sUnknownHResultType = 'Unknown type in GetHResult';
  sUnspecifiedServer = 'Cannot connect, no server specified.';
  sInvalidServerName = 'Cannot connect, invalid server name.';

//BoldComServerHandles
  sCannotChangeCLSIDAtRT = '%s: Cannot change CLSID at run-time';
  sCannotChangeDescAtRT = '%s: Cannot change Description at run-time';
  sCannotChangeNameAtRT = '%s: Cannot change Name at run-time';

//BoldCOMEditors
  sGenerateServerCode = 'This will generate server code, continue?';
  sCannotGenerateServerCode = 'Can''t generate code, no class(es) defined.';
  sCaptionGenerateServerCode = 'Generate server code...';

//BoldObjectNamePropertyEditor
  sObjectsInX = 'Objects in %s';
  sQueryStartServer = 'Would you like to start the server %s to get the list of exported objects?';
  sQueryConnectToServer = 'Connect to server %s and retrieve exported objects?';
  sCannotFindConnectionHandle = 'Cannot find a ConnectionHandle.';  

//BoldHTTPClientPersistenceHandle
  sModelRequired = '%s.CreatePersistenceController: Can not get a PersistenceController without a Model';

//BoldHTTPServerPersistenceHandlePassthrough
  sPersistenceHandleNotAssigned = '%s.%s: PersistenceHandle not assigned';

//BoldSOAPPersistenceControllerProxy
  sFailedToConnect = 'Failed connecting to COM object ''%s''.';
  sCOMObjectNotPController = 'COM object ''%s'' is not a persistence controller.';
  sNotConnected = '%s.%s: Not connected.';

//BoldPersistenceOperationXMLStreaming
  sInvalidSOAPData = 'Invalid SOAP data : %s';
  sEmptySOAPData = 'Empty SOAP data.';
  sError = 'Error: %s';
  sErrorNoMessage = 'Error: empty error message';

//BoldPersistenceControllerSOAPAdapterCore
  sUnknownOperation = '%s.Get: Unrecognized operation %s';

//BoldComObjectSpace
  sUnableToLoadComObjectSpace = 'Unable to load type library (BoldComObjectSpace)';

//BoldComObjectSpaceAdapters
  sRoleDoesNotExist = '%.%s: Role %s does not exist';
  sUnknownDataFormat = '%s.BoldMemberValues: Unknown data format';

implementation

end.
