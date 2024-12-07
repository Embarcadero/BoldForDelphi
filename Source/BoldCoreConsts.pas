unit BoldCoreConsts;

// Use this unit for all constants in Bold to avoid name conflicts

interface

const
  sUnknownTypeOfSource = '%s.%s: unknown type of source';
  sMethodNotImplemented = '%s.%s: not supported/implemented';
  sRepresentationNotSupported = '%s: representation not supported %d';
  sUnknownCompareType = '%s.%s: Unknown comparetype';

// General
  sNotImplemented = '%s.%s: not implemented';
  BoldDateTimeFormat = 'yyyy-mm-dd hh:nn:ss';

// BoldBase
  sRefCountNotNilOnDestruction = '%s: Attempt to destroy object with refcount <> 0';

// BoldIndexableList
  sCannotRemoveLastIndex = '%s.RemoveAndfreeIndex: Can not remove the last index unless during destruction';
  sNilPointersNotSupported = '%s.Add(nil): This list does not support nil-pointers';
  sCannotSetNonEmptyIndex = '%s.SetIndex: Can not set an index that is not empty (%s)';

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

// Bold
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
  sIsEqualUnknownType = '%s.IsEqual: Unknown comparetype';
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
  sStringIsNotAnsiString = 'String contains invalid characters';

// BoldAttributes
  sSetDataValue = 'SetDataValue';
  sAssignContentValue = 'AssignContentValue';
  sAssignValue = 'AssignValue';
  sValueNotSet = '%s: Value not set';
  sInvalidValue = '%s: Invalid value %s';
  sRangeError = 'Value outside range, range is %d to %d. Attempted to set %d';
  sValuesNotInitialized = '%s: Values not properly initalized';
  sStringValidationFailed = 'String validation failed';
  ssStringValidationFailedWithClass = '%s.EnsureValidString: String validation failed';
  sStringTooLong = 'String too long. Max allowed: %d, actual: %d.';
  sCannotAssignXtoY = '%s.SetStringRepresentation: Can not assign a ''%s'' to a ''%s''';
  sCannotFindValueSetValue = 'ValidateString said OK, but SetStringRepresentation can not find a valuesetvalue';
  sUnknownStringValue = 'Unknown stringvalue ''%s'', Allowed values: %s';
  sNoLegalValuesAvailable = '%s.SetEmptyValue: There are no legal values, and null is not allowed for this attribute (%s)';
  sBadDataInDB = 'Illegal value in valueset. Bad data in database?';
  sCannotRead = 'Illegal value in valueset. Cannot read.';
  sInvalidConstraint = 'Invalid Constraint: %s %s%s';
  sUnknownConstraintType = 'unknown type of constraint expression: %s (in context: %s)';
  sConstraintNotBoolean = 'Constraint is not Boolean: %s (in context %s)';
  sUnknownIntRepr = '%s.%s: unknown integer representation of source: %s';
  sUnknownStringRepr = '%s.%s: unknown string representation of source: %s';
  sObjectDestroyedAfterFinalization = 'TBoldMemoryManagedObject.FreeInstance: Attempt to destroy object after BoldBase Finalization. Inspect/revert recent Uses clause changes.';

// BoldDerivedValueSet
  sNoClassCalledX = '%s.Create: No class called %s';
  sNoAttributeCalledX = '%s.Create: No attribute called %s.%s';
  sXIsNotAnInteger = '%s.Create: %s.%s is not an integer';
  sXIsNotAnAttribute = '%s.Create: %s.%s is not an Attribute';
  sNoValuesInValueSetList = '%s.Create: No values in valuesetlist: %s';

// BoldObjectListControllers
  sCanNotInsertNil = '%s.%s: Can not insert nil';
  sCrossSystemNotAllowed = '%s.%s: Locator from another system not allowed to be inserted in %s, Define conditional AllowCrossSystemLists if you want to allow this.';

//BoldLinks
  sRolenotQualified = '%s.GetLocatorByQualifiers: Object list does not have a member index or role is not qualified';
  sOtherEndMustBeSingle = 'Cannot call compare if OtherEnd is not a single role';
  sUnexpectedControllerType = '%s.SetFromId: Unexpected type of controller: %s';
  sCannotChangeLinkObjectSingleLink = '%s.SetLocator: Cannot change a Link Object Single Link';
  sInvalidForListOfLinkObjects = '%s.%s: Illegal operation for list of link objects';
  sLocatedAbstractError = '%s.%s: Abstract error';
  sCannotSetLinkObjectReference = '%s.SetLocator: Cannot set a link object reference directy';

// BoldOCLEvaluator
  sUnknownTypeOfMember = 'unknown type of member: %s';
  sUnknownTypeOfMemberOf = 'unknown type of memberof: %s';

// BoldOCLLightWeightNodes
  sExternalVarsCanOnlyBeReferencedOnce = 'external variables (and self) can currently only be referenced once';

// BoldOCLSemantics
  sOnlyRolesCanBeQualified = '%d: Only roles may be qualified';
  sXIsNotAQualifiedRole = '%d: %s.%s is not a qualified role';
  sMethodNotoperatingOnClass = '%d: Method (%s) is not operating on a class...';
  sArgumentsDoNotConform = '%d: In "%s", one of the arguments must conform to the other (%s and %s does not)';

// BoldOCLSymbolImplementation
  sCannotCompareEnumLiterals = '%d: Enum literals can not be compared to other enum literals (%s and %s)';
  sUnableToGetAllInstances = '0: Unable to get allInstances. This evaluator has no system';
  sUnableToGetAllLoadedObjects = '0: Unable to get allLoadedObjects. This evaluator has no system';
  sAllInstancesAtTimeOnlyAllowedOnClasses = '0: AllInstancesAtTime only allowed on classes';

// BoldElementList
  sOperationNotAllowed = '%s.%s: Operation is not allowed';
  sCannotSetElementsInTypeLists = 'Can not set elements in TypeLists';

// BoldExternalObjectSpaceEventHandler
  sEventHandlerNotConnected = '%s.HandleMessage: The Eventhandler (%s) is not connected to a systemhandle. Unable to handle messages';
  sSystemHandleNotActive = '%s.%s: The systemhandle (%s) is not active.';
  sClassNotInSystem = 'Cannot find the class %s in the system.';

// BoldMLAttributes
  sBootStrapProblem = 'BootStrap-problem, Probably the Languagename is an MLString... Not allowed, sorry';
  sNeedClassCalledX = 'For MultiLanguage to work, you need a class called "%s"';
  sNeedMemberCalledX = 'For MultiLanguage to work, Class %s needs a member (string) called "%s"';
  sMustBeTBAString = 'For MultiLanguage to work, %s must be a TBAString, now it is a %s';
  sNeedIntegerMemberX = 'For MultiLanguage to work, Class %s needs a member (integer) called "%s"';
  sMemberXMustBeInteger = 'For MultiLanguage to work, %s must be a TBAInteger, now it is a %s';
  sInvalidLanguageName = '%s: Invalid languagename %s';
  sMLValueSetsRequireMLStrings = 'TBAMLValueSets can only use TBAMLStrings, not %s';
  sProxyClassDidntImplementInterface = 'ProxyClass for %s did not implement IBoldBlobContent';

// BoldObjectListControllers
  sUnknownEvent = '%s._ReceiveObjectDeleted: Unknown event';
  sClassIsAbstract = '%s is an abstract class';
  sClassIsLinkClass = '%s is a LinkClass';

// BoldOptimisticLockingSupport
  sRelatedRoleNotLoaded = '%s.GetRegionsForRemoteMember: The related role (of %s) is not loaded. Unable to ensure optimistic locking consistency';
  sRelatedObjectNotLoaded = '%s.GetRegionsForRemoteMember: The related object (of %s) is not loaded. Unable to ensure optimistic locking consistency';

// BoldSystemPersistenceHandler
  sUpdateDbRentry = 'TBoldSystemPersistenceHandler.UpdateDatabaseWithList: Already updating db.';
  sCannotFetchWithLinksFromMultipleSystems = 'Can''t Fetch with link objects from multiple systems';
  sNoCommonSuperClass = '%s.FetchLinksWithObjects: Objects have no common superClass';
  sNoRoleCalledX = '%s.FetchLinksWithObjects: class %s has no role called %s';
  sNoPersistenceController = '%s. No PersistenceController...';
  sNoSuchClassInModel = '%s.GetAllInClassWithSQL: "%s" is not a class in the model';
  sClassParameterMissing = '%s.GetAllInClassWithSQL: Must not be called without a class-parameter';
  sCannotUpdateWhileInTransaction = 'Can not update the database while in a transaction';
  sRequiredLocksNotHeld = '%s.UpdateDatabaseWithList: Not allowed to update. No longer holding the required locks.';
  sStartUpdateFailed = 'StartUpdate failed';

// BoldTypeList
  sCannotInsertTypes = '%s.InsertNew: Types can not be inserted like this';
  sCannotAddTypes = '%s.InternalAddNew: Types can not be added like this';
  sAbstractError_InterfaceNotSupported = 'Abstract error: %s.ProxyClass (IBoldValue not supported!)';
  sCannotAddElement = 'Can not add element: %s';
  sCannotCreateNewInTypeLists = 'Can not create new in Typelists';

// BoldElements
  sCannotGetEvaluatorWithoutType = '%s.GetEvaluator: Element has no type, can not get evaluator';
  sMemberAlreadyModified = '%s: Member already under modification by %s';
  sAssignNotSupported = '%s.Assign does not support assigning from %s';
  sAssignNilNotSupported = '%s.Assign does not support assigning nil';
  sCompareNotSupported = '%s: Cannot compare with a %s';
  sCompareNilNotSupported = '%s: Cannot compare to nil';
  sInvalidCompareType = '%s: Comparetype ''%s'' not supported when comparing to %s';
  sTriedToChangeImmutableElement = '%s: Tried to change the value of an immutable element from ''%s'' to ''%s''';
  sCannotSubscribeToMutableMetaElements = '%s.DefaultSubscribe: Subscription on mutable MetaElements is not possible';
  sMetaElementGetAsListNotImplemented = 'TBoldMetaElement.GetAsList has not been implemented yet.';
  sNewValueAlreadyOwned = '%s.SetReferenceValue: New value alread owned!';
  sOCLExpressionError = 'OCL Expression: %s%sError: %s';

// BoldComponentValidator
  sValidatingHeader = 'Validating %s';
  sNoContext = 'No context for %s';
  sValidationError = 'Error in %s: %s';
  sValidationExpression = 'Expression: %s';
  sComponentValidation = 'Component validation';
  sOKCheckMessage = '%s ok, checked %d expressions.';
  sErrorCheckMessage = '%s failed, checked %d expressions.';

// BoldComponentValidatorIDE
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

// BoldOCL
  sExpressionNotComplete = '%d: Expression not complete';
  sTypesMissingFromEvaluator = 'This evaluator can not be used for evaluation, since some types are missing';
  sCannotFindOCLDefinitionWithoutRepository = '0: Can not find OCL definition for %s, no repository installed';
  sInvalidForSQLEvaluation = 'Root %s: %s is not allowed for SQL-evaluation';

// BoldOCLClasses
  sMissingOCLType = 'Missing required OCL-type: %s. Update your TypeNameHandle';
  sMissingDelphiType = 'Missing DelphiType of %s (please install %s)';
  sTypeMustBeX = 'The %s type must BE %s (was: %s). Update your TypeNameHandle';
  sTypeMustInheritFromX = 'The %s type must inherit from %s (%s doesn''t).';

// BoldOclLightWeightNodeMaker
  sPSResultMustBeObjectList = 'Result of PS-evaluation must be an objectlist (was type: %s)';
  sCollectionLiteralsNotSupported = 'OLWNodes does not support CollectionLiterals';
  sTransientMembersCannoteUsed = 'Transient members (%s) can not be used in OLWNodes';
  sMethodsNotSupported = 'OLWNodes does not support methods';
  sUseBooleanOperations = 'Can not compare two booleans to each other with "=" or "<>", use boolean operations instead';
  sUseBooleanOperationsWithLiterals = 'Can not compare boolean values to literals when converting to SQL, use boolean operations instead';
  sEnumNameNotValid = 'EnumName (%s) not valid for %s';
  sEnumComparedToNonEnum = 'Enum (%s) compared to a non Enum (%s)';
  sTypeNotSupported = 'OLWNodes does not support the type %s';
  sLoopVariablesMustBeClassType = 'LoopVariables can only have class type, not %s';

// BoldLockHolder
  sNoDequeuerAvailable = '%s.LockDatabase: there is no dequeuer available';
  sOperationTimedOut = '%s.WaitForWakeUp: Operation timed out';
  sWrongItemType = '%s.Add: Item should be TBoldLock, but is %s';

// BoldLockRegions
  sTriedToNavigateNonAssociation = 'Tried to navigate %s.%s that is not an association';
  sRegionMissingIDOrLocator = '%s.GetAsString: Region is missing either a locator or an ID';
  sBadRegionID = '%s.%s: Erroneous RegionId %s';
  sBadRegionDefinition = '%s.GetRegion: Erroneous region definitions. The region %s does not have %s as root class.';
  sUnknownRoleType = '%s.FetchAndExpandOneLevelParentRegions: unknown roletype of role %s';
  sUnknownMoldRoleType = '%s.GetMulti: Unknown roletype for %s.%s';
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

// BoldOCLPropEditor
  sNoContextNoSupport = 'No Context, No design support';
  sEmptyExpression = 'Empty Expression';
  sSyntaxError = 'Unable to complete parse, syntax error';
  sSyntaxOK = 'Syntax is OK';
  sCurrentTypeIsX = 'Current type is: ';
  sCurrentTypeIsUnknown = 'Current type is: unknown';
  sModelContainsErrors = 'The model contains errors: ';

// BoldBld
  sErrorOnPos = '%s Line: %d Position: %d';
  sUnexpectedEOF = 'BLD Reader: Unexpected EOF';
  sBadCharacter = 'BLD Reader: Bad character %s';
  sMoldSyntaxError = 'BLD Reader: Syntax error';
  sAKeywordExpected = 'BLD Reader: ''%s'' expected';
  sQuotedStringExpected = 'BLD Reader: Quoted string expected';
  sKeyWordTokenExpected = 'BLD Reader: KEYWORD expected';
  sIntegerExpected = 'BLD Reader: Integer expected';
  sBooleanExpected = 'BLD Reader: Boolean expected';
  sMoldElementNotFound = 'TypeTableByMoldElement: moldElementClass not found in typetable';
  sNameNotFoundInTypeTable = 'TypeTableByName: %s not found in typetable';

// BoldUnloader
  sNeedSystemToActivate = '%s: Attempt to set Active without BoldSystem';

// BoldUndoHandler
  sBlockNameInUse = '%s.AddBlock: a block named %s already exists';
  sCannotUndoInTransaction = '%s.%s: the Undo-mechanism can only be invoked outside a transaction';
  sInvalidBlockName = '%s is not a valid blockname for this operation';
  sCannotMoveToTop = '%s Can''t be moved to top';
  sCannotMoveBlock = 'can''t move Block';
  sCannotRenameBlock = 'Can''t rename block, %s already exists.';
  sCannotMergeBlocks = 'Can''t merge blocks';
  sNoSuchBlock = 'There is no block named %s';
  sParameterNotDefined = '%s.GetDependantBlocks: parameter DependantBlocks not assigned';
  sNoSuchBlockIndex = 'There is no block with index %d';

// BoldSystemRT
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

// BoldSystem
  sReasonUnknown = 'Reason unknown';
  sFailureMessage = '%s.%s failed: %s';
  sObjectFromAnotherSystem = '%s Objects from wrong system passed to %s.';
  sBoldObjectAssigned = '%s.Destroy: BoldObject assigned';
  sLocatorNotFound = '%s.%s. Locator not found for ID: %s';
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
  sIndexOutOfRange =  '%s[Id=%s].%s: Index out of range (%d but max is %d)';
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
  sDeniedCascadeDelete = 'Related object via associationEnd: %s has denied cascade delete.';
  sDeniedDeleteFromProhibit = 'Object is related to other objects via associationEnd: %s and has DeleteAction set to daProhibit.';
  sDeniedCascadeDeleteLink = 'LinkObject: %s denied cascade delete.';
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
  sValidate_InvalidMemberType = 'Invalid member type for %s.%s. Expected %s, was %s. Member is %s (Generated code might be out of sync with model)';
  sValidate_WrongMember = 'Class %s member at index %d is %s, expected %s. (Generated code might be out of sync with model)';
  sValidate_InternalError = 'Internal Error: Member %s.%s is not assigned';
  sFailedToDerive = 'Failed to derive %s: %s (ID:%s)';
  sPossiblyBadConformance = 'Possible Reason: %s does not conform to %s';
  sCannotModifyMember = 'Can not modify member %s, owning object is deleted.';
  sMemberIsImmutable = 'Member is immutable';
  sMemberIsreadOnly = 'Member is read only';
  sMemberIsreadOnlyDerived = 'Member is read-only derived';
  sMemberIsHistory = 'Member belongs to historical object';
  sCannotInvalidateTransient = '%s.Invalidate: Can''t invalidate transient member: %s';
  sCannotDiscardUnsavedSingleLinks = '%s.Discard: This is not allowed for single members on objects that are not saved, discard the whole object';
  sProxyClassDidNotImplementInterface = 'ProxyClass for %s did not implement %s';
  sIsEqualToValueNotImplemented = 'TBoldMember.IsEqualToValue: not implemented for this memberclass';
  sStartModifyPreconditionNotMet = 'StartModify precondition not met';
  sNullValueAccess = '%s: Attempt to access Value of Attribute that is null';
  sNullAccessWithOwner = '%s: Attempt to access Value of Attribute that is null. OwningObject: %s';
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
  sElementNotBoldObject = '%s.%s: Element %s is not a TBoldObject';
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
  sUnableToFindType = '%s.CreateMemberFromExpressionName: Unable to find a type for "%s"';
  sUnexpectedStreamType = '%s.EnsureMember: %s was expected to stream as %s, but does stream as %s, check TypeNameHandle settings';

// BoldOCLGraphicRTDebug
  sIllegalExpressionEncountered = 'Runtime OCL Debugger: Illegal expression encountered, Please try to fix it';
  sInComponent = 'In component: %s';
  sIncorrectMessage = 'Incorrect OCL-expression: %s%sIn Component: %s%sMessage: %s%sContext: %s';

// BoldAttributeWizard
  sBoldAttributeWizard = 'Bold Attribute Wizard';
  sUnableToCreateUnit = 'Unable to create unit %s, check unit name (Reason: %s)';

// BoldObjectUpgrader
  sMissingTypeInfo = '%s.GetBoldSystem: Missing System Type Info';
  sMissingPersistenceController = '%s.GetBoldSystem: Missing Persistence Controller';
  sCannotReleaseInOperation = '%s.ReleaseBoldSystem: can not release the system while in an upgrade operation';

// BoldBatchUpgrader
  sNoSupportForObjectUpgrade = '%s.Create: The SystemPersistenceMapper does not support object upgrading!';
  sObjectListNotExact = '%s.UpgradeObjectIdList: ObjectIdlist not Exact';
  sObjectListNotHomogenous = '%s.UpgradeObjectIdList: ObjectIdlist not homogenous';

// BoldFreeStandingValues
  sAbstractError = '%s.%s: Abstract error';

// BoldUtils
  sCannotCreateDirectory = 'Cannot create directory';

// BoldSubscription
  sAnswerNotImplemented = '%s.Answer: You have subscribed to a query without implementing the virtual Answer method... (triggered by: %s)';
  sQueueNotAllocated = '%s.DelayTillAfterNotification: Queue not allocated';

// BoldLogHandler
  sTryingToAbort = 'Trying to abort process';
  sProcessStopped = 'Process stopped';
  sUnknownTypeInGetString = 'unknown type in GetString';

// BoldXMLStreaming
  sCannotNestPushFloat = 'Nested calls to PushFloatSettings not allowed';
  sPushNestMismatch = 'Not allowed to call PopFloatSettins without previous call to PushFloatSettings';
  sStreamerNotFound = '%s.GetStreamer: streamer for "%s" not found';
  sStreamerNotConnected = '%s.%s: Streamer is not connected to a Document';
  sDocumentHasNoRootNode = '%s.GetRootNode: Document does not have root node';
  sWrongTagName = '%s.GetRootNode: Wrong tag name, is "%s", should be "%s"';
  sDocumentHasRootNode = '%s.NewRootNode: Document already has root node';

// BoldDefaultXMLStreaming
  sInvalidIndex = '%s.%s: Not a valid index';
  sUnrecognizedClassName = '%s.%s: Unrecognized class name %s';

// BoldEnvironment*
  sNotRunningInIDE = '%s: Not running in IDE';
  sUnknownDisplayMode = '%s.ApplicationEventsOnIdle: Unknown displaymode';

// BoldObjectSpaceExternalEvents
  sInvalidEvent = 'Invalid event: %s';
  sInvalidID = 'Invalid ID, %s is not an valid integer.';

// BoldCollections
  sDuplicateName = 'There is already an item with name "%s"';

// BoldDeriver
  sCannotReverseDerive = '%s: can''t reverse derive';
  sUnknownMessageReceived = '%s.Receive: Unknown message received (%d)';

// BoldPerformanceCounter
  sCallCount = '%-35s seconds: %8.4f  calls: %5d ';
  sPercentCount = '%s %4.1f percent of %s';

// BoldFileHandler
  sUnableToCreateFileHandle = 'Unable to create filehandler for %s: %s';
  sFileHandlerMask = 'Pascal Unit (*.pas)|*.pas|Include files (*.inc)|*.inc|Text files (*.txt)|*.txt|All Files (*.*)|*.*';
  sFileSaveProblem = 'File most likely not saved properly: %s';
  sSaved = 'Saved %s';

// BoldOTAFileHandler
  sLogCreatingOTAFileHandler = 'Creating an OTA filehandler for file: %s';
  sLogGettingModule = 'Getting a module for %s';
  sLogModuleWasOpen = '%s was already open';
  sLogHadToOpenModule = 'had to open %s';
  sUnableToOpenSourceEditor = 'Unable to open Source Editor for %s';
  sUnableToPositionCursor = 'Unable to position cursor in %s (line %d)';
  sModuleReadOnly = '%s is readonly!';
  sUnableToCreateReader = 'Unable to create reader: %s';
  sFailedToCloseModule = 'Failed to Close %s: %s';

//BoldOTASupport
  sUnableToGetModule = 'Unable to get module for %s';

// BoldLogForm
  sClearAndContinue = 'Error: "%s". Clear log and continue?';
  sSaveLogAs = 'Save Log As';
  sLogFiles = 'Log files';
  sAllFiles = 'All files';
  sTextFiles = 'Text files';

// BoldLogHandlerForm
  sLogDone = '%s: Done %s';
  sLogFormCaption = 'Logging Activity: %s';
  sLogStarting = '%s: Starting %s';

// BoldLogHandlerSimple
  sSessionStart = 'Session: %s';

// BoldRose2000Support, BoldRose98Support
  sClassNameNotUnique = 'Found multiple classes with name %s, don''t know which one to use';
  sSettingValue = 'Setting %s.%s to %s';
  sUnknownVisibility = 'Unknown visibility, public is set.';
  sContainmentByValueButNotAggregate = 'Warning: containment by value, but not aggregate';

// BoldSmallLogFrame
  sLogTimeLeft = 'Time left: %s';
  sLogTotalTime = 'Tot time: %s';
  sLogSmallDone = 'Done...';
  sLogSmallWarnings = '%s  there were warnings, click the yellow icon for details...';
  sLogSmallErrors = '%s  there were errors, click the red icon for details...';

// BoldTemplateExpander
  sUnterminatedLoop = 'Unterminated loop in template: %s';
  sUnterminatedCase = 'Unterminated case in template: %s';
  sUnterminatedMacro = 'Unterminated Macro in template: %s';

// BoldThread
  sThreadWasForcedTerminated = '%s.WaitForQuit: thread was force terminated';
  sErrorWaitForQuit = '%s.WaitForQuit: %s';

// BoldXMLRequests
  sSOAPActionNotSet = '%s.%s: Action not set';
  sCannotSetPropertyWhenReadOnly = '%s.%s: cannot set BoldIds in ReadOnly mode';
  sCannotPerformInReadOnlyMode = '%s.%s: cannot perform this operation in ReadOnly mode';
  sSetActionInvalidArgs = '%s.SetAction: Invalid arguments "ActionPath" or "ActionName"';
  sSOAPDOMDocumentMissing = '%s.EnsureRoot: The XMLRequest has no DomDocument.';
  sSOAPBadConstructorToUse = '%s.Create: Use the constructors CreateFromXML or CreateInitialized';

// BoldComAdapter
  sNoAdapterRegistered = 'No adapter registered for %s';
  sCannotLoadLibrary = 'Unable to load type library (%s)';
  sXMLRequestNotAssigned = '%s.Get: XMLRequest not assigned';
  sUnableToLoadTypeLibBoldSoap = '%s.Create: Unable to load type library LIBID_BoldSOAP';

// BoldPropertyEditors
  sEditOCL = '&Edit Ocl';
  sComponentNotIBoldOCLComponent = '%s.GetOclComponent: %s (%s) is not an IBoldOclComponent';
  sCouldNotGetIBoldOCLComponent = '%s.%s: Could not get interface IBoldOclComponent';
  sNeedContextToEditType = 'Can not edit type names without a context';
  sUnexpectedClass = '%s.EnsureComponentType: Expected %s but got %s.';

// BoldModelAwareComponentEditor
  sNoModel = '%s does not seem to be connected to a model';

// BoldIDEMenus
  sCompanyHomePage = '&BoldSoft Home Page';
  sProductHomePage = 'Bold for &Delphi Home Page';
  sHelp = '&Help';
  sAbout = '&About';

// BoldAbout
  sVersionInfoNotAvailable = 'No version information for this program is available!';
  sURLBoldForDelphi = 'http://www.boldfordelphi.com';
  sURLSupport = 'http://www.forum.boldfordelphi.com';

// BoldDatabaseAdapter FireDAC/UniDac
  sFireDAC = 'FireDAC';
  sUniDAC = 'UniDAC';
  sAdapterNotConnected = '%s.GetDatabaseInterface: The adapter is not connected to an %s connection';
  sCreatedNewAdapter = 'Created a new DatabaseAdapterUniDAC';
  sCanOnlyTransferToUniDACAdapter = 'The persistencehandle is connected to a %s, properties can only be transfered to a TBoldDatabaseAdapterUniDAC';
  sCreatedNewDB = 'Created a new UniDACDatabase';
  sCouldNotTransferConnectionString = 'Connection string settings could not be transferred to the new UniDAC connection:%s%s%sPlease transfer these manually!';

// BoldDirectoryTraverser
  sInvalidDrive = '%s.Execute: Invalid drive (%s)';
  sPathDoesNotExist = '%s.Execute: Path does not exist (%s)';

// BoldMemoryManager
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

// BoldQueryUserDlg
  sDefaultQuery = 'What do you think?';

// BoldMeta
  sRecursiveAssignment = 'Attempt to make recursive assignment';
  sClassIsRelation = 'Class is already relation class for another association';
  sCannotTrimAfterLinkRolesEnsured = 'Can not trim removed elements after the linkroles have been ensured...';
  sMemberHasNoDispID = 'Member has no Dispid';

// BoldTypeNameHandleReg
  sEditTypeNames = 'Edit type names';

// BokdPMappersSQL
  sIllegalColumnIndex = '%s.%s: Illegal Column Index (%d)';
  sSomeColumnsNotInTable = '%s.%s: Some columns not found in table (%d:%s)';
  sLogIdAsString = 'Id: %s';

// BoldDBValidator
  sClassInDBNotInModel = 'Database contains a class %s with BoldType %d that is not in the model...';
  sClassWithMissingID = 'Model contains a class %s that does not have a database id';
  sCorrectClassWithNoID = 'There are classes with no database ID. Do you want to correct this now?';
  sAddBoldDBTType = 'Adding BoldDbType %d for %s';
  sDBValidation = 'Database validation';
  sFoundInconsistencies = 'Inconsistencies found';
  sDBValidationDone = 'Database validation finished';
  sDBValidationFailed = 'Database validation failed: %s';
  sMissingPSHandle = 'Unable to perform validation, missing a PersistenceHandle';
  sClassWithoutDBID = 'Model contains a class %s that does not have a database id';
  sCorrectClassesWithNoID = 'There are classes with no database ID. Do you want to correct this now?';

// BoldDBEvolutorScript
  sCommittingToDB = 'Committing changes to database';
  sRollingBackDB = 'Rolling back database changes';
  sAddTable = 'Add table %s';
  sAddColumn = 'Add column %s.%s';
  sAddInstanceOfClassToTable = 'Add Instances of class %s to table %s';
  sMoveDataFromXtoY = 'Move data from %s.%s to %s.%s (dbtypes: %s)';
  sDeleteInstancesOfClassFromTable = 'Delete instances of %s from table %s';
  sDropIndex = 'Drop index %s';
  sDropColumn = 'Drop column %s.%s';
  sDropTable = 'Drop table %s';

// BoldDBEvolutor
  sPersistenceHandleActive = '%s.CalculateScript: PersistenceHandle %s is active. Unable to execute';
  sInitializingScript = 'Initializing Script';
  sUnableToFindDBID = '%s.TranslateClassExpressionName: Unable to find source dbid for %s';
  sMemberChangedMapper = 'Member %s.%s changed mapper (%s->%s). Column[s] (%s) in table %s will be dropped and (%s) will be created in table %s, data loss!';
  sClassBecameAbstract = 'Class %s is concrete in old model, but %s is Abstract in new model';
  sUnableToHandleInstancesOfAbstract = 'ERROR: There are instances of class %s, but %s is abstract in new model';
  sDataStoredInXForYWillBeLost = 'Data stored in column %s.%s for member %s.%s will be lost';
  sClassNoLongerExists = ' (class no longer exists)';
  sNewNameForClass = ' (class now called %s)';

// BoldPSDescriptionsSQL
  sCommittingMetaData = 'Committing changes to metadata';
  sRollBackMetaData = 'Rolling back changes to metadata';
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

// BoldPMappersDefault
  sOptimisticLockingFailedOnTimeStamp= 'Optimistic Locking failed on timestamp for the following Objects';
  sOptimisticLockingFailedForNonExisting = 'Optimistic Locking failed for the following objects of type %s because they did not exist in the database:';
  sOptimisticLockFailedLog = '%s: Id %s';
  sOptimisticLockingFailed = 'Optimistic Locking Failed for %s.%s (ID: %s) Column %d [%s] ValueInDb:%s InMemTimestamp: %d';
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

// BoldPMappersLinkDefault
  sChildMappedLinkClassesNotSupported = '%s.%s: ChildMapped LinkObjects (%s) are not supported!';
  sOptimisticLockingFailedForTheFollowing = 'Optimistic Locking failed for %s.%s for the following objects';
  sCannotCallOnTransientClass = '%s.CompareField: Can not be called for this class, it is not stored';
  sLogFetchIDs = 'Fetched %4d ids for %4d nonembedded links %s.%s';

// BoldPMappers
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

// BoldPMappersAttributeDefault
  sAttributeMustNotAllowNullIfEmptyStringsStoreAsNull = 'String attribute must not allow NULL in the model if persistencemapper stores empty strings as NULL (%s.%s)';

// BoldPSDescriptionsDefault
  sLogWritingFirstClock = 'Writing First Clock';
  sUnknownGenerationMode = '%s.%s: unknown database generation mode';
  sLogWritingFirstID = 'Writing First ID';
  sLogWritingFirstTimeStamp = 'Writing First TimeStamp';
  sLogWritingTableNames = 'Writing TableNames';
  sLogInitializingDefaultPS = 'Initializing Default Persistent Storage';
  sCommittingInitialData = 'Committing changes to initial data';

// BoldListenerThread
  sInitializationLineMissing = '%s. You must add the following line to the initialization section of the application: TBoldListenerCOMFactory.Create(ComServer)';

// BoldPersistenceControllerPassThrough
  sNextControllerMissing = '%s.getNextPersistenceController: NextPersistenceController not assigned';

// BoldDBInterfaces
  sCreateParamNotImplemented = '%s.Createparam: Not supported yet... override in this subclass needed';

// BoldIDAdderHandle
  sPropertyHasMoved = '%s.%s has been moved to component (%s.%s). Old value was "%s"';

// BoldListenerhandle
  sPropagatorHandleNotAssigned = '%s.%s: PropagatorHandle not assigned';
  sCannotChangeHandleWhenActive = '%s.SetPropagatorHandle: Can''t change handle on active listener';
  sValueOutOfRange = '%s.SetExtendLeaseAfter: Value must be between 10 and 90';

// BoldAbstractObjectUpgrader
  sDisplayNameUnassigned = '<unassigned>';
  sUpgradeIfOlderThan = 'Upgrade %s if older than %d';

// BoldSQLSymbold
  sArgToOrderByMustBeMember = 'Argument to OrderBy must be a Member';
  sArgToOrderByMustHaveExactlyOneColumn = 'Argument to OrderBy must have exactly 1 column';

// BoldPersistenceHandleDBreg
  sSchemaGenerated = 'Database schema generated';
  sSchemaGenerationFailed = 'Database schema generation failed: %s%s%s';
  sGenerateSchema = 'Generate Database Schema';

// BoldPMappersSQL
  sNoDatabase = '%s: No database';
  sUnimplemented = 'unimplemented';
  sIllegalCall = '%s.%s: illegal call';

// BoldPersistenceHandleDB
  sNoDatabaseAdapterAvailable = '%s: Unable to %s. There is no DatabaseAdapter available';
  sCannotGenDBWithActiveHandle = 'Cannot generate database schema when the PersistenceHandle is Active';
  sMissingModelWhenGenDB = '%s.CreateDataBaseSchema: Cannot create database schema for %s without a Model-component';
  sMissingModelWhenGetController = '%s.CreatePersistenceController: Can not get a PersistenceController without a Model';
  sMissingDBConfigWhenGetController = '%s.CreatePersistenceController: Can not get a PersistenceController without SQLDataBaseConfig';
  sCannotActivatePersistanceHandle = '%s.SetActive: Cannot activate, there is an UpgraderHandle but the Model does not have UseModelVersion true';
  sWrongTimeFormat = '%s.SetClockLogGranularity: string is not properly formatted. Should be <hrs>:<mins>:<secs>.<msecs>';
  sCannotSetEvolutionSupport = '%s.SetEvolutionSupport: Cannot set this property when the handle (%s) is active';
  sMissingSQLDatabaseConfig = '%s: Unable to %s. There is no SQLDatabaseConfig available';

// BoldDbDataValidator
  sDBMustBeOpened = 'Database must be opened before Structurevalidation is performed!';
  sProcessingClass = 'Processing class %s';
  sColumnsHaveUnsupportedType = '-- Required column(s) %s has unsupported type(s)';
  sAddMissingEntries = '-- add missing entries into %s';
  sLogObjectsMissingInParentTable = 'The following objects exists in %s, but not in parent table %s';
  sLogObjectsHaveDifferentType = 'The following objects have different type in table %s and %s';
  sLogObjectsMissingInChildtable = 'The following objects exists in %s, but not in child table %s';
  sLogLinkObjectsAreDupes = 'The following Linkobjects are duplicates:';
  sLogLinkObjectsLinkUnexistingObjects = 'The following Linkobjects (class %s) have links to nonexisting objects:';
  sCommentRemoveSpaceLinkObjects = '-- Clean Linkobjects (%s) with space pointers';
  sLogBrokenLinkObjects = 'The following Linkobjects (class %s) have empty links in one direction:';
  sCommentRemoveBrokenLinkObjects = '-- Clean Linkobjects (%s) with broken links';
  sLogObjectsWithBrokenLinks = 'The following objects of class %s have %d invalid links in %s:';
  sCommentCleanRelation = '-- Clean relation (%s.%s) ';
  sLogObjectsWithWrongLinks = 'The following %d objects of class %s have singlelinks (%s) pointing to objects that don''t point back (they might point elsewhere):';
  sLogObjectsWithIllegalType = 'The following invalid types occur in the objects listed below:';
  sCommentRemoveObjectsWithIllegaltype = '-- Remove objects with illegal type in table %s';

// BoldDBEvolutorForm
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

// BoldCustomBlobMapper
  sMemberNameTooLong = '%s.CreateFromMold: Too long MemberName (%s) - %d characters allowed';
  sVersionedClassesNotSupported = '%s.CreateFromMold: Versioned classes (%s) currently not supported by this blobmapper';
  sObjectNotInValueSpace = '%s.GetBlobValue: Trying to get blob value for %s.%s, but the object (ID: %s) is not in the ValueSpace';
  sValueNotInValueSpace = '%s.GetBlobValue: Trying to get blob value for %s.%s, but the value (ID: %s) is not in the ValueSpace';
  sValueNotBlob = '%s.GetBlobValue: Trying to get blob value for %s.%s, but the value was not a blob...';
  sCustomCompareRequired = '%s.CompareField: needs a custom Compare to do this';

// BoldSQLMappingInfo
  sMultipleDBTypes = '%s.AddTypeIdMapping: The class %s has multiple db types (%d and %d)';
  sLogWritingMappingToDB = 'Writing mapping information to database';

// BoldSQLNodes
  sChildMappedClassesNotSupported = 'ChildMapped classes not supported: %s';

// BoldDbStructureValidator
  sMissingID = 'Missing ID';
  sAnIDWasMissing = 'A databaseId was missing for %s. Do you want to add an unused ID?';
  sCheckingTable = 'Checking table %s';
  sColumnMissing = 'Column missing: %s.%s (SQLType: %s)';
  sTableDoesNotExist = 'Table %s does not exist';
  sColumnSizeMismatch = 'Column %s in table %s has wrong size %d, should be %d';
  sColumnAllowsNull = 'Column %s in table %s allows null but the model does not';
  sColumnNotAllows = 'Column %s in table %s does not allow null but the model does';
  sNullValuesFound = '%d Null-values found in Table %s, Column %s: ';

// BoldISODateTime
  sInvalidDateFormatFormat = 'ParseISODate: Invalid date format %s. Should be YYYY-MM-DD';
  sInvalidDateFormatLargeMonth = 'ParseISODate: Invalid date format %s. month > 12';
  sInvalidDateFormatSmallMonth = 'ParseISODate: Invalid date format %s. month < 1';
  sInvalidDateFormatSmallDay = 'ParseISODate: Invalid date format %s. date < 1';
  sInvalidDateFormatBadDay = 'ParseISODate: Invalid date format %s. there is only %d days in month %d';
  sInvalidDateTimeFormat = 'ParseISODateTime: Invalid datatime format %s. Should be YYYY-MM-DD HH:MM[:SS][:ZZZ]';
  sInvalidTimeFormat = 'ParseISOTime: Invalid time format %s. Should be HH:MM[:SS:ZZZ]';
  sInvalidTimeFormatLargeHour = 'ParseISOTime: Invalid time format %s. h > 23';
  sInvalidTimeFormatLargeMinute = 'ParseISOTime: Invalid time format %s. m > 59';
  sInvalidTimeFormatLargeSecond = 'ParseISOTime: Invalid time format %s. s > 59';

// BoldOLLEController
  sCannotChangePersistenceWhenActive = '%s: Can not change Persistent-property when the OLLE system is active';

// BoldOLLEDistributableObjectHandlers
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

// BoldFreeStandingValueFactories
  sNoClassregisteredForName = '%s.CreateInstance: No freestanding class registered for name %s';

// BoldDerivedHandle
  sIllegalTypeSelected = '%s.GetStaticBoldType: Only lists and attributes are allowed as types (expr: %s)';

// BoldSQLHandle
  sNoSystemHandle = '%s.%s: %s has no SystemHandle';

// BoldExpressionHandle
  sCircularReference = '%s.%s: %s can not be linked to %s. Circular reference';

// BoldOclRepository
  sNameNotUnique = 'Invalid Name: %s Not Unique';

// BoldAbstractListHandle
  sNoPreviousElement = '%s.Prior: No previous element';
  sNoNextElement = '%s.Next: No next element';
  sNoCurrentElement = '%s.RemoveCurrentElement: No current element (%s)';
  sCannotRemoveCurrentFromImmutable = '%s: Can not remove current Element from an immutable list (in %s)';
  sCurrentElementNotBoldObject = '%s.CurrentBoldObject: Current element is not a TBoldObject (%s)';
  sCannotCreateListDueToInvalidType = '%s.EnsureList: Unable to create list (%s), can''t find valid type';
  sListNotAssigned = '%s:%s List not assigned.';

// BoldHandlePropEditor
  sComnponentNotOCLDefinition = '%s.GetContextType: Component is not a TBoldOclDefinition';

// BoldManipulators
  sCannotSetMultiLinkFromValue = 'Can''t set Multilink from value';
  sCannotSetObjectFromValue = 'Can''t set Object from value';
  sElementLacksTypeInfo = '%s.DefaultTagForElement: Element lacks type information';
  sUnknownMapping = 'Unknown mapping: %s';

// BoldSystemHandle
  sCannotUpdateDatebaseWithoutSystem = '%s.UpdateDatabase (%s) cannot be invoked without a system';
  sUnableToActivateSystemWithoutTypeInfoHandle = 'Unable to activate a system without a SystemTypeInfoHandle (%s)';
  sUnableToFindTypeInfoHandle = 'Unable to find a SystemTypeInfo. Possible misstakes: Forgot to connect the SystemTypeInfoHandle to a Model, CreationOrder of the forms/dataModules';
  sUnableToActivateSystem = 'Unable to activate system. Initialization errors in StaticSystemTypeInfo:%s%s';
  sPersistenceHandleChangedOnRunningSystem = 'PersistenceHandle was changed on a running system.';
  sModelChangedOnRunningSystem = 'The model changed in a running system';
  sRegionDefinitionsRemovedFromRunningSystem = 'RegionDefinitions were removed from a running system.';
  sPanicShutDown = '%s%sSystem Panic shut down. %d objects discarded';

// BoldVariableHandle
  sOnlyListsAndAttributeTypesAllowed = '%s.GetStaticBoldType: Only lists and attributes are allowed as types (expr: %s)';
  sValueTypeNameInvalid = '%s.Getvalue: The ValueTypeName of %s does not seem to valid (%s)';

// BoldXMLProducers
  sManipulatorNotAssigned = '%s.%s: BoldManipulator property not assigned';

// BoldLockingHandles
  sCannotActivateWithoutListener = '%s.Activate: Cannot activate Locking without a listener. Set the Listener property of the %0:s';
  sCannotActivateWithoutLockManager = '%s.Activate: Cannot activate Locking without a LockManager. Set the LockManager property of the %0:s';
  sCannotActivateWithoutSystemHandle = '%s.Activate: Cannot activate Locking without a SystemHandle. Set the Systemhandle property of the %0:s';
  sCannotActivateWithInActiveSystem =  '%s.Activate: Cannot activate Locking. The system is not active.';

// BoldHandles
  sNotAllowedOnActiveHandle = '%s: Not allowed to change the systemTypeInfoHandle on an active system';

// BoldActions
  sUpdateDB = 'Update DB';
  sCreateDB  = 'Create DB';
  sOpenSystem = 'Open system';
  sCloseSystem = 'Close system';
  sThereAreDirtyObjects = 'There are dirty objects. Do you want to save them before shutting down';
  sClosingWithDirtyObjects = 'Closing system with dirty objects';

// BoldRootedHandles
  sInternalRootHandle_CircRef = '%s: Cannot set handle - circular reference';

// BoldListActions
  sNext = 'Next';
  sPrev = 'Prev';
  sFirst = 'First';
  sLast = 'Last';
  sUnknownDeleteMode = '%s.%s: Unknown delete mode';
  sDelete = 'Delete';
  sAddNew = 'Add New';
  sMoveUp = 'Move up';
  sMoveDown = 'Move Down';

// BoldUndoActions
  sSetCheckPoint = 'Set Check-point';
  sUndo = 'Undo';
  sRedo = 'Redo';

// BoldCursorHandle
  sIndexOutOfBounds = '%s.SetCurrentIndex: Index out of bounds. Valid range from -1 to %d. Attempted to set %d';

// BoldOCLVariables
  sIllegalCharsInName = 'Invalid variable name, only alphanum characters and underscore valid';

// BoldExpressionHandle
  sDeriveAndSubscribeFailed = '%s.DeriveAndSubscribe (%s): Failed with message: %s';

// BoldOCLRepository
  sRepositoryHasNoSystemHandle = '*** OclRepository %s%s has no SystemHandle';
  sSystemHandleHasNoTypeInfo = '*** SystemHandle of OclRepository %s%s has no TypeInfo';

// BoldGen
  sLogGeneratingInPath = 'Generating in path: %s';
  sLogGeneratingFile = 'Generating file: %s';
  sLogInitializingVars = 'Initializing variables';
  sLogExpandingTemplate = 'Expanding template';
  sLogConsiderNameChange1 = 'You should consider naming your model and base unit';
  sLogConsiderNameChange2 = 'to avoid filename conflicts with other projects';
  sLogNoDelphiMappingForType = 'No Delphimapping for type %s';
  sLogNoCOMMappingForType = 'No COM/IDL mapping for type %s, No attribute generated for %s.%s';
  sLogNoNativeMappingForType = 'No native mapping for type %s used in attribute %s.%s, only Bold attribute generated';
  sNoValueTypeMappingForType = 'No Valueinterface mapping for type %s used in persistent attribute %s.%s';
  sProcessingClassXFileY = 'Processing class %s, file %s';
  sMoveToComponent_NoSuperClass = 'MoveClassTreeToComponent: No SuperClass';
  sMoveToComponent_NoMoldModel = 'MoveClassTreeToComponent: No MoldModel';
  sCollidingFileName = 'WARNING! class %s has a file name that collides with another component (%s)!';
  sNoTemplateForPersistenceInterfaces = 'No template defined for PersistenceInterfaces';

// BoldNameExpander
  sNameHasInvalidChars = 'Name has invalid characters';
  sNameTooLong = 'Name is too long';
  sNameCannotBeEmpty = 'Name can not be empty';
  sInvalidFirstChar = 'Name must begin with an alpha-character or underscore';

// BoldExternalPersistenceControllerDataSet
  sKeyTypeNotAutoHandled = 'Keytype not handled automatically: %s.%s';
  sUnknownDataType = 'Unknown data type';
  sLinkToUnconfiguredTable = 'External link to unconfigured table %s';
  sUnknownVarTypeLoadingID = 'Unknown vartype when loading an external ID for multilink %s.%s';
  sUnknownVarTypeLoadingSingleID = 'Unknown vartype when loading an external ID for Singlelink %s.%s';
  sUnknownVarTypeLoadingObject = 'Unknown vartype when loading an external ID for %s';
  sRoleHasNoColumnNames = 'Role %s does not have any column names!';
  sRoleEndCountMismatch = 'Role %s does not have an equal amount of columns on both ends!';
  sObjectNoLongerInDB = 'Object no longer exists in database';

// BoldAbstractExternalPersistenceHandle
  sValueNotCurrency = '%s.CurrencyValueForObject: The value (%s) is not a currency';
  sValueNotDate = '%s.DateValueForObject: The value (%s) is not a date';
  sValueNotInteger = '%s.IntValueForObject: The value (%s) is not an integer';
  sKeyNotInteger = '%s.KeyIntForObject: The key is not an integer';
  sKeyNotString = '%s.KeyStringForObject: The key is not a string';
  sValueNotString = '%s.StringValueForObject: The value (%s) is not is not a string';

// BoldAbstractPartiallyExternalPC
  sNotSupportedWithMultipleKeys = '%s.ExternalKeysToInternalSQL: Automatic SQL-generation only supported for classes with 1 (one) external key (class %s has multiple)';
  sNotSupportedWithNoKeys = '%s.ExternalKeysToInternalSQL: Automatic SQL-generation only supported for classes with 1 (one) external key (class %s has none!)';
  sNoSuchRole = '%s.FindMoldRoleByName: There is no role called %s in class %s';

// BoldAbstractPartiallyExternalPH
  sInvalidClassName = '%s.GetObjectIdByExternalKey: Invalid class name (%s)';

//BoldExternalPersistenceControllerEventDriven
  sAssignKeyValueRequiresOneKey = 'AssignKeyValue only supported automatically for classes with one external key: %s';
  sCreateObjectsNotAllowed = 'Creating new objects of type %s not supported/allowed';
  sDeleteObjectsNotAllowed = 'Deleting objects of type %s not supported/allowed';
  sReadObjectNotImplementedForClass = 'Event ReadObject is not implemented for class %s';
  sGetExternalKeyNotImplementedForClass = 'Getting external keys for class %s not implemented';
  sModifyObjectsNotAllowed = 'Modifying objects of type %s not supported/allowed';

//BoldExternalPersistenceHandlesReg
  sEditConfiguration = 'Edit configuration';
  sCannotCreateNameNow = 'Can not create name for eventhandler. Assign an expressionname for the config-item first';
  sBoldModelNotAssigned = '%s.%s: BoldModel not Assigned';

// BoldSnooperHandle
  sPropertyMoved = '%s.%s has been moved to component (%s.%s). Old value was "%s"';
  sSnooperNoModel = 'Snooper has no Model';

// BoldClientHandler
  sLogError = '%s.%s Error: %s';
  sLogErrorAndID = '%s.%s Error: [ID=%d] %s)';
  sClientIDsNotAssigned = '%s.%s: ClientIds is not assigned';
  sLoggingID = 'Log In: %s [ID=%d]';
  sOK = 'OK';
  sExtendLeaseFailed = 'ExtendLease failed: [ID=%d] Client already disconnected ';
  sLogOff = 'Log Off: %s [ID=%d] [Pkg: %d Ev: %d Int: %s Login: %s (%s ago) Status: %s]';
  sLeaseExpired = 'Lease Expired: %s [ID=%d] [Pkg: %d Ev: %d Int: %s Last: %s ago Login: %s (%s ago) Status: %s]';
  sClientDisconnected = 'Disconnected: %s [ID=%d] Login: %s (%s ago) Status: %s';
  sClientDisconnected_Long = 'Disconnected: %s [ID=%d] [Pkg: %d Ev: %d Int: %s Last: %s ago Login: %s (%s) Status: %s]';
  sErrorCode = '  ErrorCode=%d';
  sInvalidListener = '%s.ValidateClientInterface: ClientListener not valid for Client %s; Error=%s';
  sListenerInterfaceMissing = '%s.ValidateClientInterface Error: [ID=%s] Listener interface missing';
  sClientHasRecovered = '!!! Client %s (%d) has recovered and received %d messages';
  sNotReceiving = 'Not Receiving';
  sRecovered = 'Recovered';
  sUnknown = '<Unknown>';

// BoldAdvancedPropagator
  sInitializing = '%s.Initialize: Starting..........................................';
  sDestroying = '%s.Destroy: Closing down peacefully..........................................';

// BoldUMLAttributes
  sWrongValue = '%s.%s: Wrong value.';

// BoldUMLConverter
  sConvertingModelToMold = 'Converting UMLModel to Mold';
  sConvertingModelToUML = 'Converting MoldModel to UML';

// BoldUMLModelSupport
  sFlatteningModel = 'Flattening model';
  sBoldifyingModel = 'Boldifying Model';
  sCannotBoldifyIfRootClassHasSuperClass = 'Can''t boldify model where root class has superclass';
  sCanOnlyBeCalledIfBoldified = 'TBoldUMLBoldify.GetRootClass: can only be called on boldified model';
  sModel = 'Model';

// BoldAdvancedPropagator
  sQueueError = '%s.DoneDequeue: error = %s';

// BoldAFP
  sMissingBoldHandle = '%s.PostEnsureComponents: BoldHandle not assigned';

// BoldApartmentThread
  sPCIInfoNotInitialized = 'ApartmentThreadWndProc: pciInfo not initialized';

// BoldCheckBox
  sStateNotModifiable = '%s.State: Not modifiable';

// BoldComboBoxPropertyEditors
  sComponentNotComboBox = '%s.GetContextType: Incoming component is not a BoldComboBox';

// BoldClientNotifierHandler
  sTimerAccessed = '%s.getTimer: Timer should be accessed from %s''s thread';
  sSenderNotInQueue = '%s.OnPriorityChanged: Sender is not a TBoldPriorityQueue';
  sCouldNotSendEvents = '%s.SendToClient: could not send events to client';
  sExecuteError = '%s.Execute: Client %s (ID=%d)%s failed to receive messages (%d msgs): %s. RegistrationTime: %s (%s ago), LeaseTimeout: %s (%s left)';

// BoldCodePlugins
  sGenerateCode = 'Generate Code';
  sUMLCodeImage = 'UMLPluginGenCodeImage';
  sUMLIDLImage = 'UMLPluginGenIDLImage';
  sUMLBorlandIDL = 'Generate Borland compatible IDL';
  sUMLGenIDLImage = 'UMLPluginGenMIDLImage';
  sUMLGenMSIDL = 'Generate Microsoft (MIDL) compatible IDL';
  sModelError = 'Errors in model, can not generate code';
  sCodeGeneration = 'Code generation: %s';
  sUMLGenGUIDS = 'UMLPluginGenGUIDs';
  sUMLGenCOMIDL_GUIDS = 'Generate COM/IDL GUIDs';
  sUMLPluginGenInterface = 'UMLPluginGenPersistenceInterfaceImage';
  sUMLGenInterface = 'Generate Persistence Interfaces';

// BoldConstraintValidator
  sConstraints = 'Constraints';

implementation

end.

