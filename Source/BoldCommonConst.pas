unit BoldCommonConst;

interface

resourcestring
  sAbstractError = '%s.%s: Abstract error';
  sNotImplemented = '%s.%s: not implemented';
  sFailureUnknown = 'Reason unknown';
  sInternalError = 'Internal error?';
  sOperationNotSupported = '%s.%s: Not supported!';
  sVariablesMayOnlyBeReferencedOnce = 'external variables (and self) can currently only be referenced once';

//BoldBase
  sRefCountNotNilOnDestruction = '%s: Attempt to destroy object with refcount <> 0';

// BoldPortTemp
  sBadInputFormat = 'Bad input format'; // FIXME better message
  sInvalidBoxedValue = '%s.%s: Invalid value - %s';
  sParseNotSupported = 'Parse not supported for type %s';
  sIllegalBooleanValue = '%s in not a legal boolean value';

// BoldIndexableList
  sCannotRemoveLastIndex = '%s.RemoveAndfreeIndex: Can not remove the last index unless during destruction';
  sNilPointersNotSupported = '%s.Add(nil): This list does not support nil-pointers';
  sCannotSetNonEmptyIndex = '%s.SetIndex: Can not set an index that is not empty (%s)';

// BoldSubscription
  sIllegalConstructorForW32 = '%s.Create: This constructor should not be called in Win32';

// import
  sMultipleClassesFound = 'Found multiple classes with name %s, don''t know which one to use';
  sLogSettingValue = 'Setting %s.%s to %s';
  sContainmentWarning = 'Warning: containment by value, but not aggregate';

//BoldTaggedValueSupport
  sUnknownValue = '%s.%s: Unknown value';

//BoldLogHandlerForm
  sLogDone = '%s: Done %s';
  sLogFormCaption = 'Logging Activity: %s';
  sLogStarting = '%s: Starting %s';

//BoldSmallLogFrame
  sLogTimeLeft = 'Time left: %s';
  sLogTotalTime = 'Tot time: %s';
  sLogSmallDone = 'Done...';
  sLogSmallWarnings = '%s  there were warnings, click the yellow icon for details...';
  sLogSmallErrors = '%s  there were errors, click the red icon for details...';

//BoldXMLRequests
  sSOAPActionNotSet = '%s.%s: Action not set';
  sCannotSetPropertyWhenReadOnly = '%s.%s: cannot set BoldIds in ReadOnly mode';
  sCannotPerformInReadOnlyMode = '%s.%s: cannot perform this operation in ReadOnly mode';
  sSetActionInvalidArgs = '%s.SetAction: Invalid arguments "ActionPath" or "ActionName"';
  sSOAPDOMDocumentMissing = '%s.EnsureRoot: The XMLRequest has no DomDocument.';
  sSOAPBadConstructorToUse = '%s.Create: Use the constructors CreateFromXML or CreateInitialized';

//BoldDeriver
  sCannotReverseDerive = '%s: can''t reverse derive';
  sUnknownMessageReceived = '%s.Receive: Unknown message received (%d)';

// BoldSubscription
  sAnswerNotImplemented = '%s.Answer: You have subscribed to a query without implementing the virtual Answer method... (triggered by: %s)';
  sQueueNotAllocated = '%s.DelayTillAfterNotification: Queue not allocated';

// BoldCollections
  sDuplicateName = 'There is already an item with name "%s"';

// BoldUtils
  sCannotCreateDirectory = 'Cannot create directory';

// BoldXMLStreaming
  sCannotNestPushFloat = 'Nested calls to PushFloatSettings not allowed';
  sPushNestMismatch = 'Not allowed to call PopFloatSettins without previous call to PushFloatSettings';
  sStreamerNotFound = '%s.GetStreamer: streamer for "%s" not found';
  sStreamerNotConnected = '%s.%s: Streamer is not connected to a Document';
  sDocumentHasNoRootNode = '%s.GetRootNode: Document does not have root node';
  sWrongTagName = '%s.GetRootNode: Wrong tag name, is "%s", should be "%s"';
  sDocumentHasRootNode = '%s.NewRootNode: Document already has root node';

//BoldThread
  sThreadTerminated = '%s.WaitForQuit: thread was force terminated';

// ValueSpace
// BoldEcoSpaceExternalEvents
  sInvalidEvent = 'Invalid event: %s';
  sInvalidID = 'Invalid ID, %s is not an valid integer.';

// BoldDefaultXMLStreaming
  sInvalidIndex = '%s.%s: Not a valid index';
  sUnrecognizedClassName = '%s.%s: Unrecognized class name %s';

//BoldEnvironment*
  sNotRunningInIDE = '%s: Not running in IDE';
  sUnknownDisplayMode = '%s.ApplicationEventsOnIdle: Unknown displaymode';

//BoldIDEMenus
  sCompanyHomePage = '&BoldSoft Home Page';
  sProductHomePage = 'Bold for &Delphi Home Page';
  sHelp = '&Help';
  sAbout = '&About';

//BoldModelAwareComponentEditor
  sNoModel = '%s does not seem to be connected to a model';

//BoldOTAFileHandler
  sLogCreatingOTAFileHandler = 'Creating an OTA filehandler for file: %s';
  sLogGettingModule = 'Getting a module for %s';
  sLogModuleWasOpen = '%s was already open';
  sLogHadToOpenModule = 'had to open %s';
  sUnableToOpenSourceEditor = 'Unable to open Source Editor for %s';
  sUnableToPositionCursor = 'Unable to position cursor in %s (line %d)';
  sModuleReadOnly = '%s is readonly!';
  sUnableToCreateReader = 'Unable to create reader: %s';
  sFailedToCloseModule = 'Failed to Close %s: %s';

//BoldFileHandler
  sUnableToCreateFileHandle = 'Unable to create filehandler for %s: %s';
  sFileHandlerMask = 'Pascal Unit (*.pas)|*.pas|Include files (*.inc)|*.inc|Text files (*.txt)|*.txt|All Files (*.*)|*.*';
  sFileSaveProblem = 'File most likely not saved properly: %s';
  sSaved = 'Saved %s';

//BoldOTASupport
  sUnableToGetModule = 'Unable to get module for %s';

//BoldPropertyEditors
  sEditOCL = '&Edit Ocl';
  sComponentNotIBoldOCLComponent = '%s.GetOclComponent: %s (%s) is not an IBoldOclComponent';
  sCouldNotGetIBoldOCLComponent = '%s.%s: Could not get interface IBoldOclComponent';
  sNeedContextToEditType = 'Can not edit type names without a context';
  sUnexpectedClass = '%s.EnsureComponentType: Expected %s but got %s.';

//BoldLogForm
  sClearAndContinue = 'Error: "%s". Clear log and continue?';
  sSaveLogAs = 'Save Log As';
  sLogFiles = 'Log files';
  sAllFiles = 'All files';
  sTextFiles = 'Text files';

//BoldLogHandler
  sTryingToAbort = 'Trying to abort process';
  sProcessStopped = 'Process stopped';
  sUnknownTypeInGetString = 'unknown type in GetString';

//BoldRose2000Support, BoldRose98Support
  sClassNameNotUnique = 'Found multiple classes with name %s, don''t know which one to use';
  sSettingValue = 'Setting %s.%s to %s';
  sUnknownVisibility = 'Unknown visibility, public is set.';
  sContainmentByValueButNotAggregate = 'Warning: containment by value, but not aggregate';

//BoldThread
  sThreadWasForcedTerminated = '%s.WaitForQuit: thread was force terminated';
  sErrorWaitForQuit = '%s.WaitForQuit: %s';

//BoldTemplateExpander
  sUnterminatedLoop = 'Unterminated loop in template: %s';
  sUnterminatedCase = 'Unterminated case in template: %s';
  sUnterminatedMacro = 'Unterminated Macro in template: %s';

//BoldQueryUserDlg
  sDefaultQuery = 'What do you think?';

//BoldLogHandlerSimple
  sSessionStart = 'Session: %s';

//BoldPerformanceCounter
  sCallCount = '%-35s seconds: %8.4f  calls: %5d ';
  sPercentCount = '%s %4.1f percent of %s';


//BoldMemoryManager
  sMemoryManagerCalledInFinalization = 'Attempt to allocate with BoldMemoryManager during finalization';
  sMemoryManagerDestroyed = 'MemoryManager destroyed';
  sMemMgrSize = 'Size: %3d  InUse: %10d(%4.0f%%) Free: %10d';
  sMemMgrAllocated = 'Allocated';
  sMemMgrInUse = 'InUse';
  sMemMgrOverHead = 'Overhead';
  sMemMgrDisabled = 'The Bold Memorymanager has been disabled. ';
  sMemMgrDisabledReason = '(caused by compilerdirective BOLD_DISABLEMEMORYMANAGER)';
  sMemMgrTotalBigBlocks = 'Total Big blks';
  sMemMgrBigBlockCount = 'Big block count';
  sMemMgrNonBold = 'Non-Bold';
  sMemMgrNonBoldCount = 'Non-Bold Count';

//BoldDirectoryTraverser
  sInvalidDrive = '%s.Execute: Invalid drive (%s)';
  sPathDoesNotExist = '%s.Execute: Path does not exist (%s)';

//BoldAbout
  sVersionInfoNotAvailable = 'No version information for this program is available!';

implementation

end.
