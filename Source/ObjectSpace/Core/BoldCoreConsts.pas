unit BoldCoreConsts;

interface

resourcestring
  sUnknownTypeOfSource = '%s.%s: unknown type of source';
  sNotImplemented = 'not implemented';
  sMethodNotImplemented = '%s.%s: not supported/implemented';
  sRepresentationNotSupported = '%s: representation not supported %d';
  sUnknownCompareType = '%s.%s: Unknown comparetype';

  // BoldModelValidator errors
  sModelNameEmpty = 'Model must have a name';
  sModelUnknownMapper = 'Unknown system persistence mapper %s in model %s';
  sClassNameEmpty = 'Class must have a name';
  sClassNameExists = 'Duplicate class name %s';
  sClassIdExists = 'Duplicate class id: %d in %s and %s';
  sClassUnknownMapper = 'Unknown object persistence mapper %s in class %s';
  sDatabaseNameEmpty = '%s must have a database name';
  sDatabaseNameInvalid = 'Invalid database name %s in %s';
  sDatabaseNameSQLReserved = 'Database name %s in %s is a reserved word in SQL';
  sDelphiNameEmpty = '%s must have a delphi name';
  sDelphiNameInvalid = 'Invalid delphi name %s in %s';
  sDelphiNameReserved = 'Delphi name %s in %s is a reserved word';
  sDelphiNameExists = 'Duplicate delphi name %s in %s and %s';
  sExpressionNameEmpty = '%s must have an expression name';
  sExpressionNameInvalid = 'Invalid expression name %s in %s';
  sExpressionNameOCLReserved = 'Expression name %s in %s is a reserved word in OCL';
  sExpressionNameExists = 'Duplicate expression name %s in %s and %s';
  sTableNameEmpty = '%s must have a table name';
  sTableNameInvalid = 'Invalid table name %s in %s';
  sTableNameSQLReserved = 'Table name %s in %s is a reserved word in SQL';
  sTableNameExists = 'Duplicate table name %s in %s and %s';
  sColumnNameEmpty = '%s must have a column name';
  sColumnNameInvalid = 'Invalid column name %s in %s';
  sColumnNameSQLReserved = 'Column name %s in %s is a reserved word in SQL';
  sColumnNameExists = 'Duplicate column name %s in %s and %s';
  sMemberNameEmpty = 'Member must have a name in %s';
  sMemberNameExists = 'Duplicate member name %s in %s';
  sMethodNameEmpty = 'Method must have a name in %s';
  sAttributeUnknownType = 'Attribute %s has unknown type';
  sAttributeTypeHasNoMapping = 'Attribute %s: %s has no mapping to delphitype';
  sAttributeUnknownMapper = 'Attribute %s has unknown persistence mapper';
  sAttributeCantStore = 'Attribute %s can''t be stored, incompatible persistence mapper';
  sRoleUnknownClass = 'Role %s in association %s not associated with any class';
  sRoleUnknownMapper = 'Unknown role persistence mapper %s in class %s';
  sRoleCantStore = 'Role persistence mapper %s cannot store %s in class %s';
  sInvalidRoleOrderedandSingle = 'Role %s in class %s may not be non-multi and ordered';
  sAssociationNeedsTwoRoles = 'Association %s must have two roles';
  sAssociationM2MNeedsClass = 'Multi to multi association %s must have association class';
  sAssociationRolesNeedClass = 'Non-embedded roles %s and %s need association class';
  sAssociationOnlyM2MNeedsClass = 'Association %s is not multi-multi, cannot have association class';
  sAssociationClassCannotBePart = 'Association class for %s cannot be one of the classes in the association';
  sPersistentSubClass = 'Persistent class %s has non-persistent superclass';
  sParentMappedClass = 'Parent mapped class %s must have superclass';
  sChildrenMappedClass = 'Children mapped class %s must be abstract';
  sImportedMappedClass = 'Imported mapped class %s must be imported';
  sTopClassChildMapped = 'Rootclass (%s) may not be childmapped';
  sRecursiveMapping = 'Recursive mapping between %s and %s';
  sParentMappedWithNotNullAttribute = 'Parent mapped class %s with NotNull attributes';
  sMemberInfoClassUnknown = 'Unknown member info class %s in %s';
  sDerivedAndPersistent = 'Derived attribute %s is not allowed to be persistent';
  sTimestampRequiresXFiles = 'Using timestamps requires using XFiles';
  sGlobalIdRequiresXFiles = 'Using global IDs requires using XFiles';

  // BoldModelValidator warnings


  //Bold
  sInternalError = 'Internal Error';
  sVirtualMethodCalled = 'Pure virtual class method called!';
  sIllegalConstructorCall = 'Can''t create TBoldSystem like this, use CreateWithrtinfo instead';
  sIllegalClassConstructorCall = 'Can''t create TBoldClass like this, use InternalCreate instead';
  sIllegalDestructorCall = 'Bold use error, can''t destroy BoldObject directly use Id.Unload to unload from memory, and Delete to Delete object.';
  sDeletingWithDirtyObjects = 'Deleting TBold with dirty objects';
  sNoDefaultBold = 'No default bold or default Bold not active';
  sNoClassNamed = 'Invalid Expression, system has no class named: %s';
  sExpressionSyntaxError = 'Syntax error in Expression';
  sCreateWithoutClass = 'Can''t create bold object without a class';
  sCreateAbstractClass = 'Can''t instansiate abstract class';
  sCreateImportedClass = 'Can''t instansiate imported class';
  sCreateTransientClass = 'Transient classes not supported yet';
  sCreateBadOwner = '%s.Create(OwningElement): OwningElement must be a TBoldSystem';
  sNotInFetchState = 'member not in fetch state';
  sUnknownState = 'Internal error, unknown state';
  sUndoStateError = 'Internal error in Undo';
  sDeleteWithLinks = 'Can''t delete object with links to others';
  sNoMemberNamed = 'Class %s has no member named %s';
  sMemberNotPartofSystem = 'Member not part of system';
  sNullValueNotAllowed = 'Null Value Not Allowed for Attribute';
  sNullEqualError = 'NullEqual is void of meaning';
  sRelinkBadCall = 'Internal error: Only Callable for TBoldSinglelink and TBoldMultiLink';
  sCantCreateNew = '%s.CreateNew: can''t create new element';
  sElementNota = '%s.%s: element not a %s';
  sRelinkInternalError = '(Relink) internal error';
  sDuplicateValue = '%s.%s: Duplicate value in list';
  sNotaLinkRole = 'Not a link role';
  sTypeError = 'type error';
  sObjectNotInMultiLink = 'Attempt to remove object not in multilink';
  sCallToAbstractMethodOnCustomMapper = '%s.%s: This method is abstract, implement custom method as %s.%s';

//BoldAttributes
  sInvalidValue = 'Invalid value';
  sRangeError = '%s: Value Outside Range: Range is %d--%d. Attempted to set %d';
  sValuesNotInitialized = '%s: Values not properly initalized';
  sStringValidationFailed = 'String validation failed';
  sStringTooLong = 'String too long';
  sCannotAssignXtoY = '%s.SetStringRepresentation: Can not assign a ''%s'' to a ''%s''';
  sCannotFindValueSetValue = 'ValidateString said OK, but SetStringRepresentation can not find a valuesetvalue';
  sUnknownStringValue = 'Unknown stringvalue ''%s'', Allowed values: %s';
  sNoLegalValuesAvailable = '%s.SetEmptyValue: There are no legal values, and null is not allowed for this attribute (%s)';
  sBadDataInDB = 'Illegal value in valueset. Bad data in database?';
  sCannotRead = 'Illegal value in valueset. Cannot read.';
  sInvalidConstraint = 'Invalid Constraint: %s %s%s';
  sUnknownConstraintType = 'unknown type of constraint expression: %s (in context: %s)';
  sConstraintNotBoolean = 'Constraint is not Boolean: %s (in context %s)';

//BoldDerivedValueSet
  sNoClassCalledX = '%s.Create: No class called %s';
  sNoAttributeCalledX = '%s.Create: No attribute called %s.%s';
  sXIsNotAnInteger = '%s.Create: %s.%s is not an integer';
  sXIsNotAnAttribute = '%s.Create: %s.%s is not an Attribute';
  sNoValuesInValueSetList = '%s.Create: No values in valuesetlist: %s';

//BoldLinks
  sRolenotQualified = '%s.GetLocatorByQualifiers: Object list does not have a member index or role is not qualified';
  sOtherEndMustBeSingle = 'Cannot call compare if OtherEnd is not a single role';
  sUnexpectedControllerType = '%s.SetFromId: Unexpected type of controller: %s';
  sCannotChangeLinkObjectSingleLink = '%s.SetLocator: Cannot change a Link Object Single Link';
  sInvalidForListOfLinkObjects = '%s.%s: Illegal operation for list of link objects';
  sLocatedAbstractError = '%s.%s: Abstract error';
  sCannotSetLinkObjectReference = '%s.SetLocator: Cannot set a link object reference directy';

//BoldOCLEvaluator
  sUnknownTypeOfMember = 'unknown type of member: %s';
  sUnknownTypeOfMemberOf = 'unknown type of memberof: %s';

//BoldOCLLightWeightNodes
  sExternalVarsCanOnlyBeReferencedOnce = 'external variables (and self) can currently only be referenced once';

//BoldOCLSemantics
  sOnlyRolesCanBeQualified = '%d: Only roles may be qualified';
  sXIsNotAQualifiedRole = '%d: %s.%s is not a qualified role';
  sMethodNotoperatingOnClass = '%d: Method (%s) is not operating on a class...';
  sArgumentsDoNotConform = '%d: In "%s", one of the arguments must conform to the other (%s and %s does not)';

//BoldOCLSymbolImplementation
  sCannotCompareEnumLiterals = '%d: Enum literals can not be compared to other enum literals (%s and %s)';
  sUnableToGetAllInstances = '0: Unable to get allInstances. This evaluator has no system';
  sUnableToGetAllLoadedObjects = '0: Unable to get allLoadedObjects. This evaluator has no system';
  sAllInstancesAtTimeOnlyAllowedOnClasses = '0: AllInstancesAtTime only allowed on classes';

//BoldElementList
  sOperationNotAllowed = '%s.%s: Operation is not allowed';
  sCannotSetElementsInTypeLists = 'Can not set elements in TypeLists';

//BoldExternalObjectSpaceEventHandler
  sEventHandlerNotConnected = '%s.HandleMessage: The Eventhandler (%s) is not connected to a systemhandle. Unable to handle messages';
  sSystemHandleNotActive = '%s.HandleMessage: The systemhandle (%s) is not active. Unable to handle messages';
  sClassNotInSystem = 'Cannot find the class %s in the system.';

//BoldMLAttributes
  sBootStrapProblem = 'BootStrap-problem, Probably the Languagename is an MLString... Not allowed, sorry';
  sNeedClassCalledX = 'For MultiLanguage to work, you need a class called "%s"';
  sNeedMemberCalledX = 'For MultiLanguage to work, Class %s needs a member (string) called "%s"';
  sMustBeTBAString = 'For MultiLanguage to work, %s must be a TBAString, now it is a %s';
  sNeedIntegerMemberX = 'For MultiLanguage to work, Class %s needs a member (integer) called "%s"';
  sMemberXMustBeInteger = 'For MultiLanguage to work, %s must be a TBAInteger, now it is a %s';
  sInvalidLanguageName = '%s: Invalid languagename %s';
  sMLValueSetsRequireMLStrings = 'TBAMLValueSets can only use TBAMLStrings, not %s';
  sProxyClassDidntImplementInterface = 'ProxyClass for %s did not implement IBoldBlobContent';

//BoldObjectListControllers
  sUnknownEvent = '%s._ReceiveObjectDeleted: Unknown event';
  sClassIsAbstract = '%s is an abstract class';
  sClassIsLinkClass = '%s is a LinkClass';

//BoldOptimisticLockingSupport
  sRelatedRoleNotLoaded = '%s.GetRegionsForRemoteMember: The related role (of %s) is not loaded. Unable to ensure optimistic locking consistency';
  sRelatedObjectNotLoaded = '%s.GetRegionsForRemoteMember: The related object (of %s) is not loaded. Unable to ensure optimistic locking consistency';

//BoldSystemPersistenceHandler
  sCannotFetchWithLinksFromMultipleSystems = 'Can''t Fetch with link objects from multiple systems';
  sNoCommonSuperClass = '%s.FetchLinksWithObjects: Objects have no common superClass';
  sNoRoleCalledX = '%s.FetchLinksWithObjects: class %s has no role called %s';
  sNoPersistenceController = 'Unable to fetch object IDs. No PersistenceController...';
  sNoSuchClassInModel = '%s.GetAllInClassWithSQL: "%s" is not a class in the model';
  sClassParameterMissing = '%s.GetAllInClassWithSQL: Must not be called without a class-parameter';
  sCannotUpdateWhileInTransaction = 'Can not update the database while in a transaction';
  sForeignObjectInUpdate = '%s.UpdateDatabaseWithList: Objectlist contains objects that belong to another system';
  sRequiredLocksNotHeld = '%s.UpdateDatabaseWithList: Not allowed to update. No longer holding the required locks.';
  sStartUpdateFailed = 'StartUpdate failed';

//BoldTypeList
  sCannotInsertTypes = '%s.InsertNew: Types can not be inserted like this';
  sCannotAddTypes = '%s.InternalAddNew: Types can not be added like this';
  sAbstractError_InterfaceNotSupported = 'Abstract error: %s.ProxyClass (IBoldValue not supported!)';
  sCannotAddElement = 'Can not add element: %s';
  sCannotCreateNewInTypeLists = 'Can not create new in Typelists';

//BoldElements
  sCannotGetEvaluatorWithoutType = '%s.GetEvaluator: Element has no type, can not get evaluator';
  sMemberAlreadyModified = '%s: Member already under modification';
  sAssignNotSupported = '%s.Assign does not support assigning from %s';
  sAssignNilNotSupported = '%s.Assign does not support assigning nil';
  sCompareNotSupported = '%s: Cannot compare with a %s';
  sCompareNilNotSupported = '%s: Cannot compare to nil';
  sInvalidCompareType = '%s: Comparetype ''%s'' not supported when comparing to %s';
  sTriedToChangeImmutableElement = '%s: Tried to change the value of an immutable element from ''%s'' to ''%s''';
  sCannotSubscribeToMutableMetaElements = '%s.DefaultSubscribe: Subscription on mutable MetaElements is not possible';
  sMetaElementGetAsListNotImplemented = 'TBoldMetaElement.GetAsList has not been implemented yet.';
  sNewValueAlreadyOwned = '%s.SetReferenceValue: New value alread owned!';
  sOCLExpressionError = 'OCL Expression: %s Error: %s';

//BoldComponentValidator
  sValidatingHeader = 'Validating %s';
  sNoContext = '*** No context for %s';
  sValidationError = '*** Error in %s: %s';
  sValidationExpression = '*** Expression: %s';
  sComponentValidation = 'Component validation';

//BoldComponentValidatorIDE
  sValidateAllForms = 'Validate All Forms';
  sValidateCurrentForm = 'Validate Current Form';
  sValidatingForm = 'Validating form %s';
  sFailedToValidate = 'Failed to validate %s. %s';
  sNothingToValidate = 'Nothing to validate in %s';
  sValidatingAllOpenModules = 'Validating all open modules';
  sLookingForDefaultProject = 'Starting looking up project group with default project';
  sDefaultProject = 'Default project: %s';
  sLookingForAnyProject = 'Starting looking for any project';
  sFoundProject = 'Found project: %s';
  sNoValidateableModuleAvailable = 'No module available for validation';

//BoldOCL
  sExpressionNotComplete = '%d: Expression not complete';
  sTypesMissingFromEvaluator = 'This evaluator can not be used for evaluation, since some types are missing';
  sCannotFindOCLDefinitionWithoutRepository = '0: Can not find OCL definition for %s, no repository installed';
  sInvalidForSQLEvaluation = 'Root %s: %s is not allowed for SQL-evaluation';

//BoldOCLClasses
  sMissingOCLType = 'Missing required OCL-type: %s. Update your TypeNameHandle';
  sMissingDelphiType = 'Missing DelphiType of %s (please install %s)';
  sTypeMustBeX = 'The %s type must BE %s (was: %s). Update your TypeNameHandle';
  sTypeMustInheritFromX = 'The %s type must inherit from %s (%s doesn''t).';

//BoldSSLexU, BoldSSYaccU
  sFileDoesNotExist = 'File does not exist: %s';

//BoldOclLightWeightNodeMaker
  sPSResultMustBeObjectList = 'Result of PS-evaluation must be an objectlist (was type: %s)';
  sPSEvalrequiresSystem = 'PS-evaluation can not be performed without a system';
  sCollectionLiteralsNotSupported = 'OLWNodes does not support CollectionLiterals';
  sTransientMembersCannoteUsed = 'Transient members (%s) can not be used in OLWNodes';
  sMethodsNotSupported = 'OLWNodes does not support methods';
  sUseBooleanOperations = 'Can not compare two booleans to each other with "=" or "<>", use boolean operations instead';
  sUseBooleanOperationsWithLiterals = 'Can not compare boolean values to literals when converting to SQL, use boolean operations instead';
  sEnumNameNotValid = 'EnumName (%s) not valid for %s';
  sEnumComparedToNonEnum = 'Enum (%s) compared to a non Enum (%s)';
  sTypeNotSupported = 'OLWNodes does not support the type %s';
  sLoopVariablesMustBeClassType = 'LoopVariables can only have class type, not %s';

//BoldRegularExpression
  sMetaCharacterInUse = 'MetaCharacter ''%s'' is already in use';

//BoldLockHolder
  sNoDequeuerAvailable = '%s.LockDatabase: there is no dequeuer available';
  sOperationTimedOut = '%s.WaitForWakeUp: Operation timed out';
  sWrongItemType = '%s.Add: Item should be TBoldLock, but is %s';

//BoldLockRegions
  sTriedToNavigateNonAssociation = 'Tried to navigate %s.%s that is not an association';
  sRegionMissingIDOrLocator = '%s.GetAsString: Region is missing either a locator or an ID';
  sBadRegionID = '%s.%s: Erroneous RegionId %s';
  sBadRegionDefinition = '%s.GetRegion: Erroneous region definitions. The region %s does not have %s as root class.';
  sUnknownRoleType = '%s.FetchAndExpandOneLevelParentRegions: unknown roletype of role %s';
  sLogAddedRegions = '%s.%s Added Regions: %d';
  sLogExpandingParentRegions = 'Expanding Parent regions';
  sLogExpandingSubRegions = 'Expanding Subregions';

// BoldRegionDefinitionParser
  sExpectedToken = 'Expected ''%s''';
  sClassNameExpected = 'Class name expected';
  sMemberNameExpected = 'Member name expected';
  sRegionNameExpected = 'Region name expected';
  sXIsNotAMember = '%s is not a member of %s';
  sMemberIsNotARole = 'Member %s.%s is not a role, unable to navigate';
  sMultipleDefinitions = 'Multiple definitions of %s[%s]';
  sUnknownClassName = 'Unknown class name: ''%s''';
  sReferencedRegionNotDefined = 'Referenced region (%s) not defined. Used by %s[%s], role %s';

//BoldOCLPropEditor
  sNoContextNoSupport = 'No Context, No design support';
  sEmptyExpression = 'Empty Expression';
  sSyntaxError = 'Unable to complete parse, syntax error';
  sSyntaxOK = 'Syntax is OK';
  sCurrentTypeIsX = 'Current type is: %s';
  sCurrentTypeIsUnknown = 'Current type is: unknown';
  sModelContainsErrors = 'The model contains errors: %s%s%s';

//BoldUnloader
  sNeedSystemToActivate = '%s: Attempt to set Active without BoldSystem';

//BoldUndoHandler
  sBlockNameInUse = '%s: An Undo/Redo block named %s is already defined';
  sCannotUndoInTransaction = '%s: the Undo-mechanism can only be invoked outside a transaction';
  sInvalidBlockName = '%s is not a valid blockname for this operation';
  sCannotMoveToTop = '%s Can''t be moved to top';
  sCannotMoveBlock = 'can''t move Block';
  sCannotRenameBlock = 'Can''t rename block';
  sCannotMergeBlocks = 'Can''t merge blocks';
  sNoSuchBlock = 'There is no block named %s';
  sParameterNotDefined = '%s.GetDependantBlocks: parameter DependantBlocks not assigned';
  sNoSuchBlockIndex = 'There is no block with index %d';

//BoldSystemRT
  sCRCDiffers = 'Generated CRC differs from Model CRC (expected %s, found %s). Please regenerate code.';
  sGeneratedCodeNotRegistered = 'Generated code for %s not registered with framework, ensure that it is included in project';
  sInvalidAssociation = 'Invalid association: %s.%s does not point to a class';
  sCannotFindAttributeMapping = 'Unable to find Mapping for %s.%s: %s';
  sUnableToFindBoldTypeForAttribute = 'Unable to find BoldType for %s.%s (ExpressionType: %s)';
  sAttributeHasNoDelphiType = 'Attribute %s.%s: %s has no registered DelphiType';
  sMultiplicityConstraintMessage = 'Role %s must have %s %d %s';
  sAtMost = 'at most';
  sAtLeast = 'at least';
  sErrorInstallingAttribute = 'Error installing %s, already mapped to %s';
  sErrorInstallingAttribute_MissingSuperType = 'Error installing %s, Super class (%s) not registered';

//BoldSystem
  sReasonUnknown = 'Reason unknown';
  sFailureMessage = '%s.%s failed: %s';
  sBoldObjectAssigned = '%s.Destroy: BoldObject assigned';
  sIllegalConstruction = '%s.Create: Can''t create TBoldSystem like this, use CreateWithTypeInfo instead';
  sNoSuchClass = '%s.%s: System contains no class named: %s';
  sCannotCreateInexact = 'Can not create objects with approximate type. ID: %s Class: %s';
  sClassDoesNotBelongHere = '%s.GetAllInClass: %s does not belong to this system';
  sDestructionNestingMismatch = '%s.AllowObjectDestruction: Called without a previous matching call to DelayObjectDestruction';
  sObjectNotDestroyable = '%s.DestroyObject: Object is not destroyable. Either modified or existing.';
  sUnmatchedCommit = '%s.CommitTransaction: Unmatched call to commit. Transaction not started.';
  sCommitNotAllowed = 'Transaction not allowed to be commited';
  sUnmatchedRollback = '%s.RollbackTransaction: Unmatched call to rollback. Transaction not started.';
  sDestroy_TransactionNesting = '%s.Destroy; TransactionNesting = %d (%s)';
  sDestroy_RollbackAreaAssigned = '%s.Destroy; RollBackArea still assigned (%s)';
  sDestroy_DirtyObjects = '%s.Destroy: Destroying system with dirty objects (%s)';
  sNotAllowedInTransaction = '%s.SetTransactionMode: Not allowed while inside a transaction';
  sCannotChangeLockHandler = '%s.SetPessimisticLockHandler: Can not change lock handler on a running system';
  sIndexOutOfRange = '%s.%s: Index out of range (%d but max is %d)';
  sCannotInstansiateAbstractClass = '%s.InitializeObject: Can''t instansiate abstract class';
  sCannotInstansiateImportedClass = '%s.InitializeObject: Can''t instansiate imported class';
  sNoDefaultSystem = '%s.Create: unable to find a default system';
  sOwningElementMustBeSystem = '%s.Create: OwningElement must be a TBoldSystem';
  sGeneratedCodeNotUsed = '%s.Create: The system does not use generated code...';
  sGeneratedCode_HowToFix = 'You must use BoldSystem.CreateNewObjectByExpressionName (or tell the SystemTypeInfoHandle to use the generated code)';
  sNoClassInformation = '%s.Create: Unknown error, unable to find class information for %s';
  sCannotCreateAssociationClass = '%s.Create: Cannot create instance of association class';
  sIllegalDirectDestruction = '%s.Destroy: Can''t destroy a BoldObject directly. Call Id.Unload to unload from memory or Delete to delete object';
  sNoPersistentMembers = '%s.MarkObjectDirty: There are no persistent members';
  sObjectIsreadOnly = 'Object is read only';
  sObjectHasRelations = 'Object is related to other objects';
  sPreconditionNotMet = 'StartDelete precondition not met';
  sNoSuchMember = '%s: No member named %s';
  sCannotMakePersistent = '%s: Can''t make object persistent';
  sCannotMakeLinkPersistent = '%s: Can''t make link object persistent. Linked object is transient.';
  sToBeRemovedAccessed = '%s: Accessed member flagged as "ToBeRemoved": %s.%s';
  sToBeRemovedModified = '%s: Modified member flagged as "ToBeRemoved": %s.%s';
  sToBeRemovedObjectAccessed = '%s: Accessed an object flagged as "ToBeRemoved": %s';
  sCannotCreateOfType = 'System does not allow creating objects of type %s';
  sObjectIsDirty = 'Object is dirty';
  sCannotInvalidateDirtyObject = '%s.Invalidate: Can''t invalidate dirty object, use discard first';
  sCannotDiscardInTransaction = 'Can not discard an object while the system inside a transaction';
  sInterfaceNotImplemented = 'ProxyClass for %s did not implement IBoldObjectContents';
  sValidate_MemberNotAssigned = 'Unable to access member %s.%s: %0:s is not assigned!';
  sValidate_NoSuchMember = 'Class %s has no member %s. (Generated code might be out of sync with model)';
  sValidate_MemberIndexOutOfSynch = 'Member indexes in generated code is out of sync with model (%s.%s has %d in code and %d in model';
  sValidate_InvalidMemberType = 'Invalid member type for %s.%s. Expected %s, was %s (Generated code might be out of sync with model)';
  sFailedToDerive = 'Failed to derive %s: %s';
  sOCLExpression = 'OCL expression: %s';
  sPossiblyBadConformance = 'Possible Reason: %s does not conform to %s';
  sMemberIsImmutable = 'Member is immutable';
  sMemberIsreadOnly = 'Member is read only';
  sMemberIsreadOnlyDerived = 'Member is read-only derived';
  sMemberIsHistory = 'Member belongs to historical object';
  sCannotInvalidateTransient = '%s.Invalidate: Can''t invalidate transient member: %s';
  sCannotDiscardUnsavedSingleLinks = '%s.Discard: This is not allowed for single members on objects that are not saved, discard the whole object';
  sProxyClassDidNotImplementInterface = 'ProxyClass(%s) for %s did not implement %s';
  sIsEqualToValueNotImplemented = 'TBoldMember.IsEqualToValue: not implemented for this memberclass';
  sStartModifyPreconditionNotMet = 'StartModify precondition not met';
  sNullValueAccess = '%s: Attempt to access Value of Attribute that is null';
  sObjectIDIsInternal = '%s.MakeDbCurrent: Attribute belongs to object with internal ID';
  sFailure_Invalid = '%s is not a valid %s';
  sIllegalInitialValue = 'Illegal InitialValue (%s) for attribute %s. ErrorMessage: %s%s';
  sUnknownObjectType = '%s.CompareToAs: unknown type of object (%s)';
  sCannotSetSuchReference = 'Can not set a %s in a %s-reference';
  sItemNotInList = '%s.Remove: Item not in list';
  sCannotCreateNewElement = 'Can''t create new element';
  sDuplicateInList = '%s.DuplicateControl: Duplicate value in list';
  sUnknownDuplicationMode = '%s.DuplicateControl: Unknown duplication mode';
  sSourceNotBoldMemberList = '%s.Assign: Source is not a BoldMemberList (%s)';
  sElementNotBoldMember = '%s.%s: Element not a TBoldMember';
  sOnlyAllowedOnEmptyLists = '%s.SetCloneMembers: Only allowed on empty lists';
  sCannotFindSystem = '%s.CreateTypedList: Can not find system';
  sClassIsNotBusinessClass = '%s.CreateTypedList: %s is not a business class';
  sElementNotBoldObject = '%s.%s: Element not a TBoldObject';
  sElementIsNil = '%s.SetElement: Element is nil';
  sListNotObjectList = '%s.AddList: List not a TBoldObjectList';
  sSourceNotObjectList = '%s.Assign: Source is not a TBoldObjectList (%s)';
  sItemNotAllowedInList = 'Can not put a %s in a %sList';
  sCannotCreateController = '%s.InitializeMember: Cannot create controller. Unknown Type.';
  sCanOnlyChangeForStandAloneLists = '%s.%s: You can only change this property for stand-alone object lists';
  sObjectRefMustBePartOfObject = 'AssignContentValue, ObjectReference must be part of Object';
  sCannotRecyclePartOfObject = 'TBoldAttribute.RecycleValue: Can''t recycle part of object';
  sInvalidBoldType = '%s.CreateMemberFromBoldType: Invalid BoldType (%s)';
  sInvalidBoldType_Nil = '%s.CreateMemberFromBoldType: Invalid BoldType (nil)';
  sDelphiClassNotInstalled = '%s.CreateMemberFromBoldType: Delphi class for type %s not installed (looking for %s)';
  sUnableToFindType = '%s.CreateMemberFromExpressionName: Unable to find a type for "%s"';
  sNotEnoughMembers = '%s.EnsureMember(%d): not enough members in TBoldObject';
  sUnexpectedStreamType = '%s.EnsureMember: %s was expected to stream as %s, but does stream as %s, check TypeNameHandle settings';

//BoldOCLGraphicRTDebug
  sIllegalExpressionEncountered = 'Runtime OCL Debugger: Illegal expression encountered, Please try to fix it';
  sInComponent = 'In component: %s';
  sIncorrectMessage = 'Incorrect OCL-expression: %s%sIn Component: %s%sMessage: %s%sContext: %s';

//BoldAttributeWizard
  sBoldAttributeWizard = 'Bold Attribute Wizard';
  sUnableToCreateUnit = 'Unable to create unit %s, check unit name (Reason: %s)';

//BoldObjectUpgrader
  sMissingTypeInfo = '%s.GetBoldSystem: Missing System Type Info';
  sMissingPersistenceController = '%s.GetBoldSystem: Missing Persistence Controller';
  sCannotReleaseInOperation = '%s.ReleaseBoldSystem: can not release the system while in an upgrade operation';

implementation

end.
