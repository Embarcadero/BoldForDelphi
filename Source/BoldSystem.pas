{ Global compiler directives }
{$include bold.inc}

unit BoldSystem;

interface

uses
  Classes,
  Db, // Needed for GetAllInClassWithSQL...
  BoldBase,
  BoldStreams,
  BoldSystemRT,
  BoldDefs,
  BoldContainers,
  BoldIndexableList,
  BoldSubscription,
  BoldElements,
  BoldUpdatePrecondition,
  BoldCondition,
  BoldPersistenceController,
  BoldId,
  BoldDomainElement,
  BoldIndex,
  BoldvalueInterfaces,
  BoldValueSpaceInterfaces,
  BoldUndoInterfaces,
  BoldFreeStandingValues,
  BoldDeriver;

const
  {Query events}
  bqBaseSystem = bqMaxSubscription + 1;
  {$IFNDEF BOLD_NO_QUERIES}
  {General}
  bqMayRead = bqBaseSystem + 0;


  bqMaySetValue = bqBaseSystem + 1;
  {TBoldAttribute}
  bqMaySetToNull = bqBaseSystem + 2;
  {TBoldList and TBoldObjectReference}
  bqMayClear = bqBaseSystem + 3;

  {TBoldList}
  bqMayInsert = bqBaseSystem + 4;
  bqMayRemove = bqBaseSystem + 5;
  bqMayMove = bqBaseSystem + 6;
  bqMayReplace = bqBaseSystem + 7;
  {Object creation/deletion (sent by system)}
  bqMayCreateObject = bqBaseSystem + 8;
  bqMayDeleteObject = bqBaseSystem + 9;
  {$ENDIF}
  bqMaxSystem = bqBaseSystem + 9;

type
  {forward declarations of all classes}
  TBoldObjectLocator = class;
  TBoldSystemLocatorList = class;

  TBoldSystem = class;
  TBoldObject = class;
  TBoldMember = class;
  TBoldAttribute = class;
  TBoldFailureReason = class;
  TBoldObjectReference = class;
  TBoldList = class;
  TBoldMemberList = class;
  TBoldObjectList = class;

  TBoldLocatorListTraverser = class;

  TBoldSystemExtension = class;
  TBoldAbstractPessimisticLockHandler = class;
  TBoldAbstractOptimisticLockHandler = class;
  TBoldAbstractUndoHandler = class;
  TBoldAbstractOldValueHandler = class;
  TBoldAbstractSystemPersistenceHandler = class;
  TBoldAbstractRegionFactory = class;
  TBoldAbstractController = class;
  TBoldListController = class;
  TBoldAbstractObjectListController = class;
  TBoldAbstractObjectReferenceController = class;
  TBoldObjectReferenceController = class;

  TBoldMemberFactory = class;
  TBoldMember_Proxy = class;
  TBoldAttribute_Proxy = class;
  TBoldObjectClass = class of TBoldObject;
  TBoldAttributeClass = class of TBoldAttribute;
  TBoldMemberClass = class of TBoldMember;
  TBoldListClass = class of TBoldList;
  TBoldObjectListClass = class of TBoldObjectList;

  TBoldMember_ProxyClass = class of TBoldMember_Proxy;
  TBoldMemberDeriver = class;

  { exceptions }
  EBoldOperationFailedForObjectList = class;
  EBoldFailure = class;
  EBoldAccessNullValue = class(EBold);
  EBoldFailedToDerive = class(EBold);

  { other types }
  TBoldSystemTransactionMode = (stmUnsafe, stmNormal);
  TBoldListDupMode = (bldmMerge, bldmAllow, bldmError);
  TBoldTransactionState = (btsNone, btsInTransaction, btsRollingBack);

  TBoldPrepareListOperation = procedure(List: TBoldList) of object;
  TBoldElementCompare = function (Item1, Item2: TBoldElement): Integer of object;
  TBoldOptimisticLockingFailedEvent = procedure(UpdateList, FailureList: TBoldObjectList; const FailureReason: String) of object;
  TBoldCreateApproximateObjectError = procedure(Obj: TBoldObject) of object;

  { TBoldSystemExtension }
  TBoldSystemExtension = class(TBoldSubscribableNonRefCountedObject)
  private
    fSystem: TBoldSystem;
  public
    constructor Create(System: TBoldSystem); virtual;
    property System: TBoldSystem read fSystem;
  end;

  { TBoldAbstractPessimisticLockHandler }
  TBoldAbstractPessimisticLockHandler = class(TBoldSystemExtension)
  public
    function LockElement(Element: TBoldDomainElement): Boolean; virtual; abstract;
    procedure ReleaseUnneededRegions; virtual; abstract;
    function EnsureLocks: Boolean; virtual; abstract;
  end;

  { TBoldAbstractOptimisticLockHandler }
  TBoldAbstractOptimisticLockHandler = class(TBoldSystemExtension)
  private
    function GetOldValues: IBoldValueSpace;
  public
    procedure AddOptimisticLocks(ObjectList: TBoldObjectlist; PreCondition: TBoldOptimisticLockingPrecondition); virtual; abstract;
    procedure EnsureEnclosure(Obj: TBoldObject; Enclosure: TBoldObjectList; ValidateOnly: Boolean; var ListIsEnclosure: Boolean); virtual; abstract;
    property OldValues: IBoldValueSpace read GetOldValues;
  end;

  { TBoldAbstractUndoHandler }
  TBoldAbstractUndoHandler = class(TBoldSystemExtension)
  protected
   class function GetControllerForMember(Member: TBoldMember): TBoldAbstractController;
   procedure DeleteObject(BoldObject: TBoldObject);
    function GetEnabled: Boolean; virtual; abstract;
    procedure SetEnabled(value: Boolean); virtual; abstract;
  public
    procedure HandleMember(const ObjectContents: IBoldObjectContents; MemberIndex: integer; const MemberValue: IBoldValue); virtual; abstract;
    procedure HandleObject(const Obj: IBoldObjectContents; RegardAsExisting: Boolean); virtual; abstract;
    procedure PrepareUpdate(const ObjectList: TBoldObjectList); virtual; abstract;
    procedure ApplytranslationList(IdTranslationList: TBoldIdTranslationList); virtual; abstract;
    property Enabled: Boolean read GetEnabled write SetEnabled;
  end;

  { TBoldAbstractOldValueHandler }
  TBoldAbstractOldValueHandler = class(TBoldSystemExtension)
  protected
    function GetOldValues: IBoldValueSpace; virtual; abstract;
    function GetIsEmpty: Boolean; virtual; abstract;
    procedure PurgeEqualValues; virtual; abstract;
    class function NewValueInValueSpace(BoldMember: TBoldMember; const ValueSpace: IBoldValueSpace): IBoldValue;
    class procedure CopyMemberToValueSpace(BoldMember: TBoldMember; const ValueSpace: IBoldValueSpace);
    class procedure CopyObjectToValueSpace(BoldObject: TBoldObject; const ValueSpace: IBoldValueSpace);
  public
    procedure MemberValuePreChange(BoldMember: TBoldMember); virtual; abstract;
    procedure MemberPersistenceStatePreChange(BoldMember: TBoldMember; NewState: TBoldValuePersistenceState); virtual; abstract;
    procedure ObjectExistenceChange(BoldObject: TBoldObject); virtual; abstract;
    procedure ObjectExistencePersistenceStateChange(BoldObject: TBoldObject; NewState: TBoldValuePersistenceState); virtual; abstract;
    property OldValues: IBoldValueSpace read GetOldValues;
    property IsEmpty: Boolean read GetIsEmpty;
  end;


  { TBoldAbstractSystemPersistenceHandler }
  TBoldAbstractSystemPersistenceHandler = class(TBoldSystemExtension)
  private
    fOnPreUpdate: TNotifyEvent;
    fOnPostUpdate: TNotifyEvent;
  protected
    function GetTimeStampOfLatestUpdate: TBoldTimeStampType; virtual; abstract;
    function GetTimeOfLatestUpdate: TDateTime; virtual; abstract;
  public
    function EnsureEnclosure(ObjectList: TBoldObjectList; ValidateOnly: Boolean): Boolean; virtual; abstract;
    procedure FetchLinksWithObjects(ObjectList: TBoldObjectList; const LinkName: string;FetchObjectsInLink: Boolean = True{; const FetchedObjectList: TBoldObjectList = nil});virtual; abstract;
    procedure FetchMembersWithObjects(aBoldObjectList: TBoldObjectList; aBoldMemberIdList: TBoldMemberIdList); overload; virtual; abstract;
    procedure FetchMembersWithObjects(aBoldObjectList: TBoldObjectList; AMemberCommaList: string); overload; virtual; abstract;
    procedure FetchObjectById(BoldObjectId: TBoldObjectId); virtual; abstract;
    procedure FetchMember(Member: TBoldMember); virtual; abstract;
    procedure FetchList(FetchList: TBoldObjectList); virtual; abstract;
    procedure FetchClass(ClassList: TBoldObjectList; Time: TBoldTimestampType); virtual; abstract;
    procedure GetAllWithCondition(aList: TBoldObjectList; Condition: TBoldCondition); virtual; abstract;
    procedure GetAllInClassWithSQL(aList: TBoldObjectList; AClass: TBoldObjectClass; WhereClause, OrderByClause: String; Params: TParams; JoinInheritedTables: Boolean; MaxAnswers: integer; Offset: integer);virtual; abstract;
    procedure GetAllInClassWithRawSQL(aList: TBoldObjectList; AClass: TBoldObjectClass; SQL: String; Params: TParams; MaxAnswers: integer; Offset: integer);virtual; abstract;
    procedure UpdateDatabaseWithList(ObjectList: TBoldObjectList);  virtual; abstract;
    function GetTimeForTimestamp(Timestamp: TBoldTimestampType): TDateTime; virtual; abstract;
    function GetTimestampForTime(ClockTime: TDateTime): TBoldTimestampType; virtual; abstract;
    property OnPreUpdate: TNotifyEvent read fOnPreUpdate write fOnPreUpdate;
    property OnPostUpdate: TNotifyEvent read fOnPostUpdate write fOnPostUpdate;
    property TimeStampOfLatestUpdate: TBoldTimeStampType read GetTimeStampOfLatestUpdate;
    property TimeOfLatestUpdate: TDateTime read GetTimeOfLatestUpdate;
    procedure EndFetchForAll(ObjectList: TBoldObjectList; MemberIdList: TBoldMemberIdList);
    procedure EndUpdateForAll(ObjectList: TBoldObjectList; Translationlist: TBoldIdTranslationlist);
    function StartUpdateForAll(ObjectList: TBoldObjectList): Boolean;
    function CanEvaluateInPS(sOCL: string; aContext: TBoldElementTypeInfo = nil; const aVariableList: TBoldExternalVariableList = nil): Boolean; virtual; abstract;
  end;

  { TBoldAbstractRegionFactory }
  TBoldAbstractRegionFactory = class(TBoldMemoryManagedObject)
  private
    fSystem: TBoldSystem;
  protected
    property System: TBoldSystem read fSystem;
  public
    procedure GetRegionsForElement(Element: TBoldDomainElement; ResultList: TList); virtual; abstract;
  end;

  IBoldTransactionHandler = Interface
  ['{F24ACD01-B888-4A0A-9291-0A725BE1A7F9}']
//    property DirtyObjects: TBoldObjectList read GetDirtyObjects;
    function GetValueSpace: IBoldValueSpace;
    property Valuespace: IBoldValueSpace read GetValueSpace;
    procedure Commit;
    procedure Rollback;
  end;

  IBoldDirtyObjectTracker = Interface
  ['{792458C4-548A-4134-A11C-35F54E3B8EA5}']
    procedure StartTracking;
    procedure StopTracking;
    function GetDirtyObjects: TBoldObjectList;
    procedure DiscardChanges;
    procedure WriteChangesToDb;
    procedure ClearObjects;
    property DirtyObjects: TBoldObjectList read GetDirtyObjects;
  end;

  TBoldTransactionHandler = class(TBoldRefCountedObject, IBoldTransactionHandler)
  private
    fBoldSystem: TBoldSystem;
    fSubscriber: TBoldExtendedPassthroughSubscriber;
    fTransactionNesting: integer;
  protected
    procedure Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const); virtual;
    function CanCommitOrRollback: boolean;
    function GetValueSpace: IBoldValueSpace;
    property Valuespace: IBoldValueSpace read GetValueSpace;
    procedure Commit;
    procedure Rollback;
  public
    constructor Create(ABoldSystem: TBoldSystem);
    destructor Destroy; override;
    procedure BeforeDestruction; override;
  end;

  TBoldDirtyObjectTracker = class(TBoldRefCountedObject, IBoldDirtyObjectTracker)
  private
    fBoldSystem: TBoldSystem;
    fDirtyObjects: TBoldObjectList;
    fSubscriber: TBoldExtendedPassthroughSubscriber;
  protected
    function GetDirtyObjects: TBoldObjectList;
    procedure CheckStillDirty;
    procedure Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const); virtual;
  public
    constructor Create(ABoldSystem: TBoldSystem);
    destructor Destroy; override;
    procedure BeforeDestruction; override;
    procedure StartTracking;
    procedure StopTracking;
    procedure ClearObjects;
    procedure DiscardChanges;
    procedure WriteChangesToDb;
    property DirtyObjects: TBoldObjectList read GetDirtyObjects;
  end;

{$IFDEF ACCESSSTATISTICS} //// START PATCH ACCESSSTATISTICS ////////////////////
    TArrayOfArrayOfIntegers = array of array of Integer;
{$ENDIF} //// END PATCH ACCESSSTATISTICS ///////////////////////////////////////

  {---TBoldSystem---}
  TBoldSystem = class(TBoldDomainElement)
  private
    fTransactionMode: TBoldSystemTransactionMode;
    fBoldSystemTypeInfo: TBoldSystemTypeInfo;
    fClasses: TBoldMemberList; {of TBoldClass}
    fDelayedDestructionCount: integer;
    fDelayedDestructionList: TList;
    fDirtyObjects: TList; {of TBoldObject}
    FDiscardCount: Integer;
    fFetchNesting: Integer;
    fEvaluator: TBoldEvaluator;
    fLocators: TBoldSystemLocatorList;
    fPersistenceController: TBoldPersistenceController;
    fRollbackArea: TBoldFreeStandingValueSpace;
    fValidValueArea: TBoldFreeStandingValueSpace;
    fTransactionNesting: Integer;
    fTransactionRollbackOnly: Boolean;
    fTransactionList: TBoldDomainElementCollection;
    fPessimisticLockHandler: TBoldAbstractPessimisticLockHandler;
    fOptimisticLockHandler: TBoldAbstractOptimisticLockHandler;
    fOldValueHandler: TBoldAbstractOldValueHandler;
    fRegionFactory: TBoldAbstractRegionFactory;
    fOptimisticLockingFailed: TBoldOptimisticLockingFailedEvent;
    fSystemPersistenceHandler: TBoldAbstractSystemPersistenceHandler;
    fUndoHandler: TBoldAbstractUndoHandler;
    fOnCreateApproximateObjectError: TBoldCreateApproximateObjectError;
    fDerivedMembers: TList;
    fPersistenceControllerSubscriber: TBoldPassthroughSubscriber;
    FDeletingObjectsDepth: Integer;
{$IFNDEF NoAutoSubscription}
    fMembersReadDuringDerivation: TList;
    fMembersReadDuringDerivationIndexArray: array of Integer;
{$ENDIF}
    fSystemProxyCache: array[TBoldDomainElementProxyMode] of IBoldvalueSpace;
    procedure SetIsDefault(Value: boolean);
    function GetIsDefault: boolean;
    property RollBackAreaAssigned: Boolean index befRollbackAreaAssigned read GetElementFlag write SetElementFlag;
{$IFNDEF NoObjectSpaceTransactions}
    procedure CopyMemberToRollBackBuffer(BoldMember: TBoldMember);
    function CanCommit: Boolean;
{$ENDIF}
    procedure CopyObjectToRollBackBuffer(BoldObject: TBoldObject);
    procedure AddToTransaction(DomainElement: TBoldDomainElement);
    procedure IncrementDeletingObjectsDepth;
    procedure DecrementDeletingObjectsDepth;
    procedure DestroyObject(BoldObject: TBoldObject);
    procedure DirtyAsObjectList(ObjectList: TBoldObjectList);
    function FindClassByExpressionName(const ExpressionName: string): TBoldObjectList;
    function GetClassByExpressionName(const ExpressionName: string): TBoldObjectList;
    function GetClassByObjectClass(AObjectClass: TBoldObjectClass): TBoldObjectList;
    function GetClassByIndex(index: Integer): TBoldObjectList;
    function GetDirtyObjects: TList; {of TBoldObject}
    function GetDirtyObjectsAsBoldList(AClassType: TBoldObjectClass): TBoldObjectList; overload;
    function GetDirtyObjectsAsBoldList(AClassType: TBoldClassTypeInfo): TBoldObjectList; overload;
    function GetDirtyObjectsAsBoldListByClassExpressionName(const AClass: string): TBoldObjectList;
    function GetAllDirtyObjectsAsBoldList: TBoldObjectList;
    function GetEnsuredLocatorByID(ObjectID: TBoldObjectId): TBoldObjectLocator;
    function GetTimeForTimestamp(Timestamp: TBoldTimestampType): TDateTime;
    function GetTimestampForTime(ClockTime: TDateTime): TBoldTimestampType;
    procedure MarkObjectClean(BoldObject: TBoldObject); {called by TBoldObject}
    procedure MarkObjectDirty(BoldObject: TBoldObject); {called by TBoldObject}
    procedure MarkObjectPossiblyCleaner(BoldObject: TBoldObject);
    function GetAsIBoldvalueSpace(Mode: TBoldDomainElementProxyMode): IBoldvalueSpace;
    procedure SetTransactionMode(const Value: TBoldSystemTransactionMode);
    procedure SetPessimisticLockHandler(const Value: TBoldAbstractPessimisticLockHandler);
    function GetOnPreUpdate: TNotifyEvent;
    procedure SetOnPreUpdate(const Value: TNotifyEvent);
    function GetTimeStampOfLatestUpdate: TBoldTimeStampType;
    function GetTimeOfLatestUpdate: TDateTime;
    function GetUndoHandler: TBoldAbstractUndoHandler;
    function GetUndoHandlerInterface: IBoldUndoHandler;
    function CanCreateObject(ClassTypeInfo: TBoldClassTypeInfo): boolean;
    function CanDeleteObject(anObject: TBoldObject): boolean;
    procedure MemberDerivationBegin(Member: TBoldMember);
    procedure MemberDerivationEnd(Member: TBoldMember);
    function GetIsProcessingTransactionOrUpdatingDatabase: Boolean;
    procedure ReceiveFromPersistenceController(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    function GetIsDiscarding: Boolean;
    function GetIsFetching: Boolean;
    property DirtyObjectsInvalid: Boolean index befDirtyObjectsInvalid read GetElementFlag write SetElementFlag;
  protected
    function GetBoldDirty: Boolean; override;
    function GetBoldType: TBoldElementTypeInfo; override;
    function GetEvaluator: TBoldEvaluator; override;
    function GetStringRepresentation(Representation: TBoldRepresentation): string; override;
    procedure ReceiveEventFromOwned(originator: TObject; originalEvent: TBoldEvent; const Args: array of const); override;
    function ReceiveQueryFromOwned(Originator: TObject; OriginalEvent: TBoldEvent; const Args: array of const; Subscriber: TBoldSubscriber): Boolean; override;
    property SystemPersistenceHandler: TBoldAbstractSystemPersistenceHandler read fSystemPersistenceHandler;
    property OldValueHandler: TBoldAbstractOldValueHandler read fOldValueHandler;
  public
{$IFDEF ACCESSSTATISTICS} //// START PATCH ACCESSSTATISTICS ////////////////////
    fAccessStats: TArrayOfArrayOfIntegers;
    fDeriveStats: TArrayOfArrayOfIntegers;
    fInvalidateStats: TArrayOfArrayOfIntegers;
    fModifyStats: TArrayOfArrayOfIntegers;
{$ENDIF} //// END PATCH ACCESSSTATISTICS ///////////////////////////////////////
    constructor CreateWithTypeInfo(AOwningElement: TBoldDomainElement; SystemTypeInfo: TBoldSystemTypeInfo; PersistenceController: TBoldPersistenceController; RegionFactory: TBoldAbstractRegionFactory = nil); reintroduce;
    destructor Destroy; override;
    function AssertLinkIntegrity: Boolean;
    class function DefaultSystem: TBoldSystem;
    procedure MakeDefault;
    procedure EnsureCanDestroy;
    procedure AllowObjectDestruction;
    function CanEvaluateInPS(sOCL: string; aContext: TBoldElementTypeInfo = nil; const aVariableList: TBoldExternalVariableList = nil): Boolean;
    procedure CommitTransaction(MinimalMode: TBoldSystemTransactionMode = stmNormal);
    function CreateExistingObjectByID(BoldObjectID: TBoldObjectId): TBoldObject;
    function CreateNewObjectByExpressionName(const ExpressionName: string; Persistent: Boolean = True): TBoldObject;
    function CreateNewObjectFromClassTypeInfo(aClassTypeInfo: TBoldClassTypeInfo; Persistent: Boolean = True): TBoldObject;
    procedure DefaultSubscribe(Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent = breReEvaluate); override;
    procedure DelayObjectDestruction;
    procedure Discard; override;
    function IsDerivingMembers: boolean;
    function CurrentDerivedMember: TBoldMember;
    function EnsureEnclosure(ObjectList: TBoldObjectList; ValidateOnly: Boolean): Boolean;
    procedure FetchLinksWithObjects(ObjectList: TBoldObjectList; const LinkName: string;FetchObjectsInLink: Boolean = True{; const FetchedObjectList: TBoldObjectList = nil});
    procedure FetchMembersWithObjects(aBoldObjectList: TBoldObjectList; aBoldMemberIdList: TBoldMemberIdList); overload;
    procedure FetchMembersWithObjects(aBoldObjectList: TBoldObjectList; const AMemberCommaList: string); overload;
    procedure FetchMembersWithObject(ABoldObject: TBoldObject; const AMemberCommaList: string);
    procedure FetchIdList(FetchIdList: TBoldObjectIdList; AFetchObjects: boolean = true);
    procedure GetAllInClass(aList: TBoldObjectList; AClass: TBoldObjectClass);
    procedure GetAllWithCondition(aList: TBoldObjectList; Condition: TBoldCondition);
    procedure GetAllInClassWithSQL(aList: TBoldObjectList; AClass: TBoldObjectClass; WhereClause, OrderByClause: String; Params: TParams = nil; JoinInheritedTables: Boolean = true; MaxAnswers: integer = -1; Offset: integer = -1);
    procedure GetAllInClassWithRawSQL(aList: TBoldObjectList; AClass: TBoldObjectClass; SQL: String; Params: TParams = nil; MaxAnswers: integer = -1; Offset: integer = -1);
    procedure GetAsList(ResultList: TBoldIndirectElement); override;
    function InTransaction: boolean;
    procedure RollbackTransaction(MinimalMode: TBoldSystemTransactionMode = stmNormal);
    function CreateTransactionHandler(MinimalMode: TBoldSystemTransactionMode = stmNormal): IBoldTransactionHandler;
    procedure StartTransaction(MinimalMode: TBoldSystemTransactionMode = stmNormal);
    function TryCommitTransaction(MinimalMode: TBoldSystemTransactionMode = stmNormal): Boolean;
    procedure UpdateDatabase;
    procedure UpdateDatabaseWithList(ObjectList: TBoldObjectList);
    procedure UpdateDatabaseWithObjects(const aObjects: array of TBoldObject);
    function ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean; override;
    procedure CheckIntegrity;
    procedure DiscardPersistent(ADiscardTransientLinks: boolean = true);
    procedure DiscardTransient;
    function EnsureLocatorByID(ObjectID: TBoldObjectId; out ACreated: boolean): TBoldObjectLocator;
    function ContainsDirtyObjectsOfClass(AClassType: TBoldObjectClass): boolean;
    procedure RemoveDeletedObjects(IDList: TBoldObjectIdList);
    function GetClassTypeForID(ObjectID: TBoldObjectID): TBoldClassTypeInfo;
    function CreateDirtyObjectTracker: IBoldDirtyObjectTracker;
    property IsDefault: Boolean read GetIsDefault write SetIsDefault;
    property IsDestroying: Boolean index befIsDestroying read GetElementFlag write SetElementFlag;
    property IsCommitting: Boolean index befIsCommitting read GetElementFlag write SetElementFlag;
    property IsRollingBack: Boolean index befIsRollingBack read GetElementFlag write SetElementFlag;
    property IsUpdatingDatabase: Boolean index befIsUpdatingDatabase read GetElementFlag write SetElementFlag;
    property IsProcessingTransactionOrUpdatingDatabase: Boolean read GetIsProcessingTransactionOrUpdatingDatabase;
    property IsFetching: Boolean read GetIsFetching;
    property IsDiscarding: Boolean read GetIsDiscarding;
    property BoldSystemTypeInfo: TBoldSystemTypeInfo read fBoldSystemTypeInfo;
    property ClassByExpressionName[const ExpressionName: string]: TBoldObjectList read GetClassByExpressionName;
    property ClassByObjectClass[ObjectClass: TBoldObjectClass]: TBoldObjectList read GetClassByObjectClass;
    property Classes[index: Integer]: TBoldObjectList read GetClassByIndex;
    property DirtyObjects: TList read GetDirtyObjects;
    property DirtyObjectsAsBoldListByClass[AClassType: TBoldObjectClass]: TBoldObjectList read GetDirtyObjectsAsBoldList;
    property DirtyObjectsAsBoldListByClassTypeInfo[AClassType: TBoldCLassTypeInfo]: TBoldObjectList read GetDirtyObjectsAsBoldList;
    property DirtyObjectsAsBoldListByClassExpressionName[const AClass: string]: TBoldObjectList read GetDirtyObjectsAsBoldListByClassExpressionName;
    property DirtyObjectsAsBoldList: TBoldObjectList read GetAllDirtyObjectsAsBoldList;
    property EnsuredLocatorByID[ObjectID: TBoldObjectId]: TBoldObjectLocator read GetEnsuredLocatorByID;
    property Locators: TBoldSystemLocatorList read fLocators;
    property PessimisticLockHandler: TBoldAbstractPessimisticLockHandler read fPessimisticLockHandler write SetPessimisticLockHandler;
    property OptimisticLockHandler: TBoldAbstractOptimisticLockHandler read fOptimisticLockHandler;
    property RegionFactory: TBoldAbstractRegionFactory read fRegionFactory;
    property TransactionNesting: Integer read fTransactionNesting;
    property PersistenceController: TBoldPersistenceController read fPersistenceController;
    property AsIBoldvalueSpace[Mode: TBoldDomainElementProxyMode]: IBoldvalueSpace read GetAsIBoldValueSpace;
    property TimeForTimestamp[Timestamp: TBoldTimestampType]: TDateTime read GetTimeForTimestamp;
    property TimestampForTime[ClockTime: TDateTime]: TBoldTimestampType read GetTimestampForTime;
    property TimeStampOfLatestUpdate: TBoldTimeStampType read GetTimeStampOfLatestUpdate;
    property TimeOfLatestUpdate: TDateTime read GetTimeOfLatestUpdate;
    property TransactionMode: TBoldSystemTransactionMode read fTransactionMode write SetTransactionMode;
    property OnPreUpdate: TNotifyEvent read GetOnPreUpdate write SetOnPreUpdate;
    property OnOptimisticLockingFailed: TBoldOptimisticLockingFailedEvent read fOptimisticLockingFailed write fOptimisticLockingFailed;
    property UndoHandler: TBoldAbstractUndoHandler read GetUndoHandler;
    property UndoHandlerInterface: IBoldUndoHandler read GetUndoHandlerInterface;
    property OnCreateApproximateObjectError: TBoldCreateApproximateObjectError read fOnCreateApproximateObjectError write fOnCreateApproximateObjectError;
  end;

    { TBoldLocatorListTraverser }
  TBoldLocatorListTraverser = class(TBoldIndexableListTraverser)
  private
    function GetLocator: TBoldObjectLocator;
  public
    property Locator: TBoldObjectLocator read GetLocator;
    property Current: TBoldObjectLocator read GetLocator;
  end;

  {---TBoldSystemLocatorList---}
  TBoldSystemLocatorList = class(TBoldUnOrderedIndexableList)
  private
    class var IX_BoldObjectId: integer;
    function GetLocatorByID(ObjectID: TBoldObjectId): TBoldObjectLocator;
    function GetObjectByID(ObjectID: TBoldObjectId): TBoldObject;
    function GetObjectByIDString(const ID: string): TBoldObject;
    function GetLocatorByIDString(const ID: string): TBoldObjectLocator;
  protected
    function TraverserClass: TBoldIndexableListTraverserClass; override;
  public
    constructor Create;
    function AssertIntegrity: Boolean;
    function GetEnumerator: TBoldLocatorListTraverser;
    procedure UpdateID(Locator: TBoldObjectLocator; NewObjectID: TBoldObjectId; AllowInternal: Boolean = false);
    function CreateTraverser: TBoldLocatorListTraverser;
    property LocatorByID[ObjectID: TBoldObjectId]: TBoldObjectLocator read GetLocatorByID;
    property ObjectByID[ObjectID: TBoldObjectId]: TBoldObject read GetObjectByID;
    property LocatorByIdString[const ID: string]: TBoldObjectLocator read GetLocatorByIDString;
    property ObjectByIdString[const ID: string]: TBoldObject read GetObjectByIDString;
  end;

  TBoldLocatorArray = array of TBoldObjectLocator;

  {---TBoldObjectLocator---}
  TBoldObjectLocator = class(TBoldMemoryManagedObject)
  private
    fBoldSystem: TBoldSystem;
    fBoldObject: TBoldObject;
    fBoldObjectID: TBoldObjectId;
    fEmbeddedSingleLinks: TBoldLocatorArray;
    constructor CreateWithObjectId(BoldSystem: TBoldSystem; BoldObjectID: TBoldObjectId);
    constructor CreateWithClassID(BoldSystem: TBoldSystem; TopSortedIndex: integer; Exact: Boolean);
    procedure AddToLocators;
    procedure FetchBoldObject;
    procedure EmbeddedSingleLinksToObject;
    procedure EmbeddedSingleLinksFromObject;
    procedure FreeEmbeddedSingleLinksOfOtherEnd;
    function GetAsString: string;
    function GetEnsuredBoldObject: TBoldObject;
    function GetObjectIsPersistent: Boolean;
    function GetEmbeddedSingleLinks(EmbeddedIndex: integer): TBoldObjectLocator;
    procedure SetEmbeddedSingleLinks(EmbeddedIndex: integer; const Value: TBoldObjectLocator);
    function GetClassTypeInfo: TBoldClassTypeInfo;
    procedure TryShrinkEmbeddedLinks;
  protected
    function GetDebugInfo: string; override;
  public
    destructor Destroy; override;
    function AtTime(Time: TBoldTimeStampType): TBoldObjectLocator;
    procedure DiscardBoldObject(ADiscardTransientLinks: boolean = true);
    procedure EnsureBoldObject;
    function Hash: Cardinal;
    procedure UnloadBoldObject;
    property AsString: string read GetAsString;
    property BoldObject: TBoldObject read FBoldObject;
    property BoldObjectID: TBoldObjectId read fBoldObjectID;
    property BoldSystem: TBoldSystem read FBoldSystem;
    property BoldClassTypeInfo: TBoldClassTypeInfo read GetClassTypeInfo;
    property EnsuredBoldObject: TBoldObject read GetEnsuredBoldObject;
    property ObjectIsPersistent: Boolean read GetObjectIsPersistent;
    property EmbeddedSingleLinks[EmbeddedIndex: integer]: TBoldObjectLocator read GetEmbeddedSingleLinks write SetEmbeddedSingleLinks;
  end;

  {---TBoldObject---}
  TBoldObject = class(TBoldDomainElement)
  private
    fBoldClassTypeInfo: TBoldClassTypeInfo;
    fMemberArray: array of TBoldMember;
    fDeriverArray: array of TBoldMemberDeriver;
    FBoldObjectLocator: TBoldObjectLocator;
    fTimeStamp: TBoldTimeStampType;
    constructor InternalCreateWithClassAndLocator(ClassTypeInfo: TBoldClassTypeInfo; Locator: TBoldObjectLocator);
    procedure CalculateMemberModified;
    function CanUnload: Boolean;
    procedure DoStartDelete;
{$IFNDEF NoMayUpdate}
    procedure DoStartUpdate;
    procedure PrepareUpdateMembers;
{$ENDIF}
    procedure EndCreate;
    procedure EndDelete;
    procedure EndFetchMembers(MemberIdList: TBoldMemberIdList);
    procedure EndReCreate;
    procedure EndUpdate(Translationlist: TBoldIdTranslationlist);
    procedure EndUpdateMembers(Translationlist: TBoldIdTranslationlist);
    procedure FailDelete;
    procedure InternalUnLinkAll(AUnlinkPersistent: boolean = true);
    procedure InternalDiscard(ADiscardPersistentLinks: boolean = true);
    function GetBoldExistenceState: TBoldExistenceState;
    function GetBoldMemberCount: Integer;
    function GetBoldMembers(index: Integer): TBoldMember;
    function GetBoldMemberDeriver(Member: TBoldMember): TBoldMemberDeriver;
    function GetBoldMemberIfAssigned(index: Integer): TBoldMember;
    function GetBoldPersistenceState: TBoldValuePersistenceState;
    function GetBoldSystem: TBoldSystem; reintroduce;
    function GetBoldTime: TBoldTimestampType;
    function GetGlobalId: string;
    function GetIsModified: Boolean;
    function GetObjectHasSubscribers: Boolean;
    procedure InitializeObject(System: TBoldSystem; ClassTypeInfo: TBoldClassTypeInfo; Locator: TBoldObjectLocator; Persistent: Boolean);
    procedure InitializeMember(Member: TBoldMember; MemberRTInfo: TBoldMemberRTInfo; IsNewObject: Boolean);
{$IFNDEF NoMayUpdate}
    function MayUpdateMembers: Boolean;
{$ENDIF}
    procedure MemberBecomingClean(BoldMember: TBoldMember);
    procedure MemberBecomingModified(BoldMember: TBoldMember);
    procedure MemberChangingValidity(BoldMemberRtInfo: TBoldMemberRtInfo; NewValue: TBoldValuePersistenceState);
    procedure SetBoldExistenceState(Value: TBoldExistenceState);
    procedure SetBoldPersistenceState(Value: TBoldValuePersistenceState);
    procedure SetIsReadOnly(NewValue: Boolean);
    procedure SetGlobalId(const NewValue: string);
    procedure SetTimeStamp(NewValue: TBoldTimeStampType);
    function StartDelete: Boolean;
    function InternalCanDelete(CheckedObjects: TBoldDomainElementCollection; Cascade: Boolean): Boolean;
    function GetAsIBoldObjectContents(Mode: TBoldDomainElementProxyMode): IBoldObjectContents;
    function GetBoldMemberAssigned(Index: integer): Boolean;
    function SafeGetBoldMemberAssigned(Index: integer): Boolean;
    function GetBoldObjectIsNew: Boolean;
    function GetBoldObjectIsDeleted: Boolean;
    function GetBoldObjectExists: Boolean;
    property InDirtyList: Boolean index befInDirtyList read GetElementFlag write SetElementFlag;
    property MemberModified: Boolean index befMemberModified read GetElementFlag write SetElementFlag;
    property MemberModifiedKnown: Boolean index befMemberModifiedKnown read GetElementFlag write SetElementFlag;
    property IsEffectiveInvalid: Boolean index befIsEffectiveInvalid read GetElementFlag write SetElementFlag;
    property IsEffectiveInvalidKnown: Boolean index befIsEffectiveInvalidKnown read GetElementFlag write SetElementFlag;
    property InDelayDestructionList: Boolean index befInDelayDestructionList read GetElementFlag write SetElementFlag;
    function GetBoldMemberByExpressionName(const Name: string): TBoldMember;
    function GetBoldMemberIndexByExpressionName(const Name: string): Integer;
    function CalculateEffectiveInvalid: Boolean;
    function GetEffectiveInvalid: Boolean;
    procedure FreeDerivers;
  protected
    procedure CompleteCreate; virtual;
    procedure CompleteUpdate; virtual;
    procedure CompleteRecreate; virtual;
    function GetTimeStamp: TBoldTimeStampType;
    function GetBoldDirty: Boolean; override;
    function GetBoldType: TBoldElementTypeInfo; override;
    function GetDisplayName: String; override;
    function GetEvaluator: TBoldEvaluator; override;
    function GetStringRepresentation(Representation: TBoldRepresentation): string; override;
    function GetDeriveMethodForMember(Member: TBoldMember): TBoldDeriveAndResubscribe; overload; virtual;
    function GetDeriveMethodForMember(MemberIndex: Integer): TBoldDeriveAndResubscribe;  overload; virtual;
    function GetReverseDeriveMethodForMember(Member: TBoldMember): TBoldReverseDerive; overload; virtual;
    function GetReverseDeriveMethodForMember(MemberIndex: Integer): TBoldReverseDerive; overload; virtual;
    function MayDelete: Boolean; virtual;
    function MayUpdate: Boolean; virtual;
    function GetDeletingOrDeletingByDiscard: Boolean;
    procedure PrepareDelete; virtual;
    procedure PrepareUpdate; virtual;
    procedure PrepareDiscard; virtual;
    procedure InternalPrepareDeleteOrDeleteByDiscard; virtual;
    procedure StateError(S: String); override;
    function ValidateMember(const ObjectDelphiName, MemberDelphiName: String; GeneratedMemberIndex: integer; MemberClass: TBoldMemberClass): Boolean;
    procedure ToBeRemovedMemberAccessed(MemberRTInfo: TBoldMemberRTInfo); virtual;
    procedure ToBeRemovedMemberModified(MemberRTInfo: TBoldMemberRTInfo); virtual;
    procedure ToBeRemovedClassAccessed; virtual;
    function InternalCanDeleteObject: Boolean; virtual;
    procedure BeforeDiscard; virtual;
    procedure AfterDiscard; virtual;
    procedure ReceiveEventFromOwned(originator: TObject; originalEvent: TBoldEvent; const Args: array of const); override;
    function ReceiveQueryFromOwned(Originator: TObject; OriginalEvent: TBoldEvent; const Args: array of const; Subscriber: TBoldSubscriber): Boolean; override;
  public
    constructor Create(AOwningElement: TBoldDomainElement; Persistent: Boolean = True); reintroduce;
    constructor InternalCreateNewWithClassAndSystem(ClassTypeInfo: TBoldClassTypeInfo; aSystem: TBoldSystem; Persistent: Boolean);
    destructor Destroy; override;
    function AtTime(Time: TBoldTimestampType): TBoldObject;
    procedure BoldMakePersistent;
    function CanDelete: Boolean;
    function CanUpdate: Boolean;
    function CheckLinks(index: Integer): Boolean;
    procedure DefaultSubscribe(Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent = breReEvaluate); override;
    procedure Delete;
    procedure Discard; override;
    procedure DiscardPersistentMembers;
    procedure GetAsList(ResultList: TBoldIndirectElement); override;
    procedure Invalidate; override;
    function CompareToAs(CompType: TBoldCompareType; BoldElement: TBoldElement): Integer; override;
    function IsEqualAs(CompareType: TBoldCompareType; BoldElement: TBoldElement): Boolean; override;
    procedure ReRead;
    procedure SubscribeToStringRepresentation(Representation: TBoldRepresentation; Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent = breReEvaluate); override;
    procedure UnLinkAll;
    procedure UnLinkAllPersistent;
{$IFNDEF CompareToOldValues}
    procedure MarkObjectDirty;
{$ENDIF}
    procedure ClearTouched;
    function ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean; override;
    function FindBoldMemberByExpressionName(const Name: string): TBoldMember;
    property BoldClassTypeInfo: TBoldClassTypeInfo read fBoldClassTypeInfo;
    property BoldExistenceState: TBoldExistenceState read GetBoldExistenceState;
    property BoldMemberByExpressionName[const name: string]: TBoldMember read GetBoldMemberByExpressionName;
    property BoldMemberCount: Integer read GetBoldMemberCount;
    property BoldMemberIndexByExpressionName[const name: string]: Integer read GetBoldMemberIndexByExpressionName;
    property BoldMemberAssigned[index: Integer]: Boolean read GetBoldMemberAssigned;
    property BoldMemberIfAssigned[index: Integer]: TBoldMember read GetBoldMemberIfAssigned;
    property BoldMembers[index: Integer]: TBoldMember read GetBoldMembers;
    property BoldObjectLocator: TBoldObjectLocator read FBoldObjectLocator;
    property BoldPersistenceState: TBoldValuePersistenceState read GetBoldPersistenceState;
    property BoldSystem: TBoldSystem read GetBoldSystem;
    property BoldTime: TBoldTimestampType read GetBoldTime; // Time in versioning extension
    property BoldTimeStamp: TBoldTimeStampType read GetTimeStamp; // TimeStamp of last change to the object
    property ObjectHasSubscribers: Boolean read GetObjectHasSubscribers;
    property AsIBoldObjectContents[Mode: TBoldDomainElementProxyMode]: IBoldObjectContents read GetAsIBoldObjectContents;
    property BoldObjectIsNew: Boolean read GetBoldObjectIsNew;
    property BoldObjectIsDeleted: Boolean read GetBoldObjectIsDeleted;
    property BoldObjectExists: Boolean read GetBoldObjectExists;
    property Touched: Boolean index befTouched read GetElementFlag;
    property IsCreating: Boolean index befCreating read GetElementFlag write SetElementFlag;
    property Discarding: Boolean index befDiscarding read GetElementFlag write SetElementFlag;
    property Deleting: Boolean index befDeleting read GetElementFlag write SetElementFlag;
    property DeletingOrDeletingByDiscard: Boolean read GetDeletingOrDeletingByDiscard;
    property IsHistoricVersion: Boolean index befIsHistoricVersion read GetElementFlag;
    property IsReadOnly: Boolean index befObjectReadOnly read GetElementFlag write SetElementFlag;
    property EffectiveInvalid: Boolean read GetEffectiveInvalid;
  end;

   { TBoldMember }
  TBoldMember = class(TBoldDomainElement)
  private
    fBoldMetaType: TBoldMetaElement;
    procedure AdjustOldValues(Translationlist: TBoldIdTranslationlist); virtual;
    procedure CalculateDerivedMemberWithExpression(Subscriber: TBoldSubscriber);
    procedure CalculateDerivedMemberWithDeriveMethod(Subscriber: TBoldSubscriber);
    procedure DeriveMember(Subscriber: TBoldSubscriber);
    procedure ReverseDeriveMember();
    procedure EndUpdate(Translationlist: TBoldIdTranslationlist);
    function GetOwningObject: TBoldObject;
    function GetBoldSystem: TBoldSystem; reintroduce;
    function GetBoldMemberRTInfo: TBoldMemberRTInfo;
    procedure _NotifyOutOfDate;
    procedure DoSetInitialValue; virtual;
    function GetDeriver: TBoldMemberDeriver;
    function GetElementTypeInfoForType: TBoldElementTypeInfo; virtual;
    function GetIsReadOnly(Flag: TBoldElementFlag): Boolean;
    procedure InitializeStateToInvalid;
    procedure InitializeStateToModified;
    procedure InitializeStateToTransient;
    procedure InitializeStateToCurrent;
    function InternalMayUpdate: Boolean; virtual;
    function IsInvalid: Boolean;
    function GetIsCurrent: Boolean;
    procedure MakeDbCurrent; virtual;
    procedure ObjectBecomingPersistent;
    procedure InternalDiscard;
    function GetAsIBoldValue(Mode: TBoldDomainElementProxyMode): IBoldValue;
    function GetController: TBoldAbstractController; virtual;
    procedure AssignContentValueFromElement(source: TBoldElement); virtual;
    property IsInitializing: Boolean index befIsInitializing read GetElementFlag write SetElementFlag;
    property HasDeriver: Boolean index befHasDeriver read GetElementFlag write SetElementFlag;
    function GetDeriverState: TBoldDeriverState;
    procedure SetDeriverState(value: TBoldDeriverState);
    function GetOldValue: IBoldvalue;
    constructor CreateAsObjectPart(OwningObject: TBoldObject; MemberRTInfo: TBoldMemberRTInfo);
    procedure InitializeNonObjectOwned(ElementTypeInfo: TBoldElementTypeInfo);
    procedure PreDiscard; virtual;
    property DeriverState: TBoldDeriverState read GetDeriverState write SetDeriverState;
  protected
    function FindASystem: TBoldSystem;
    procedure DoStartModify;
    procedure Changed(Event: TBoldEvent; const Args: array of const);
    procedure CompleteModify; virtual;
    procedure CompleteUpdate; virtual;
    procedure EndModify;
    procedure FailModify;
    procedure FreeContent; virtual;
    procedure AssignCloneValue(AClone: TBoldMember); virtual;
    function GetBoldDirty: Boolean; override;
    function GetBoldPersistenceState: TBoldValuePersistenceState;
    function GetPSStateIsInvalid: Boolean;
    function GetBoldType: TBoldElementTypeInfo; override;
    function GetDisplayName: String; override;
    function GetEvaluator: TBoldEvaluator; override;
    function GetStreamName: string; virtual;
    function GetFreeStandingClass: TBoldFreeStandingElementClass; virtual;
    procedure Initialize; virtual;
    function MayModify: Boolean; virtual;
    function MayUpdate: Boolean; virtual;
    procedure PreChange;
    procedure PrepareModify; virtual;
    procedure PrepareUpdate; virtual;
    procedure SetBoldPersistenceState(Value: TBoldValuePersistenceState);
    procedure StateError(S: String); override;
    function StartModify: Boolean; virtual;
    function RetrieveProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj; const InterfaceName: string): Boolean;
    function GetProxy(Mode: TBoldDomainElementProxyMode): TBoldMember_Proxy; virtual; abstract;
    function CloneIfPossible: TBoldElement; override;
    function GetIsPartOfSystem: Boolean; override;
  public
    constructor Create; reintroduce;
    constructor CreateWithTypeInfo(ElementTypeInfo: TBoldElementTypeInfo); override;
    destructor Destroy; override;
    function AtTime(Time: TBoldTimestampType): TBoldMember; virtual;
    function CanModify: Boolean;
    function CanUpdate: Boolean;
    function MemberHasSubscribers: Boolean;
    function Clone: TBoldMember; virtual;
    function IsEqualToValue(const Value: IBoldValue): Boolean; virtual;
    function StoreInUndo: Boolean;
    procedure Discard; override;
    procedure DoEnsureContentsCurrent;
    procedure DoMarkTouched;
    procedure EnsureContentsCurrent;
    procedure Refetch;
    procedure GetAsList(ResultList: TBoldIndirectElement); override;
    procedure GetAsValue(ResultElement: TBoldIndirectElement); override;
    procedure Invalidate; override;
    procedure MarkMemberDirty;
    function CanRead(Subscriber: TBoldSubscriber): Boolean;
    function ObserverMayModify(Observer: TObject): Boolean; override;
    function ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean; override;
    property BoldMemberRTInfo: TBoldMemberRTInfo read GetBoldMemberRTInfo;
    property BoldSystem: TBoldSystem read GetBoldSystem;
    property BoldPersistenceState: TBoldValuePersistenceState read GetBoldPersistenceState write SetBoldPersistenceState;
    property BoldPersistenceStateIsInvalid: Boolean read GetPSStateIsInvalid;
    property IsCurrent: Boolean read GetIsCurrent;
    property Derived: Boolean index befDerived read GetElementFlag;
    property Deriver: TBoldMemberDeriver read GetDeriver;
    property OwningObject: TBoldObject read GetOwningObject;
    property AsIBoldValue[Mode: TBoldDomainElementProxyMode]: IBoldValue read GetAsIBoldValue;
    property OldValue: IBoldValue read GetOldValue;
    property Touched: Boolean index befTouched read GetElementFlag;
    property IsPreFetched: Boolean index befPreFetched read GetElementFlag write SetElementFlag;
    property OwnedByObject: Boolean index befOwnedByObject read GetElementFlag write SetElementFlag;
    property IsReadOnly: Boolean index befMemberReadOnly read GetIsReadOnly write SetElementFlag;
  end;

  { TBoldMember_Proxy }
  TBoldMember_Proxy = class(TBoldDomainElement_Proxy, IBoldValue, IBoldStreamable)
  private
    fProxedMember: TBoldMember;
    function GetProxedController: TBoldAbstractController;
  protected
    property ProxedMember: TBoldMember read fProxedMember;
    property ProxedController: TBoldAbstractController read GetProxedController;
    function GetContentName: String;
    function GetStreamName: String;
    function GetFreeStandingClass: TBoldFreeStandingElementClass;
    function GetContentType: TBoldValueContentType;
    procedure AssignContent(const Source: IBoldValue);
    procedure AssignContentValue(const Source: IBoldValue); virtual; abstract;
    function GetStringRepresentation(representation:integer): String;
    function GetContentAsString: String;
    function GetBoldPersistenceState: TBoldValuePersistenceState;
    procedure SetBoldPersistenceState(Value: TBoldValuePersistenceState);
    constructor Create(ProxedMember: TBoldMember; Mode:  TBoldDomainElementProxyMode);
  public
    class function MakeProxy(ProxedMember: TBoldMember; Mode:  TBoldDomainElementProxyMode): TBoldMember_Proxy; virtual;
    procedure Retarget(ProxedMember: TBoldMember; Mode:  TBoldDomainElementProxyMode);
  end;

  { TBoldMemberFactory }
  TBoldMemberFactory = class(TBoldMemoryManagedObject)
  public
    class function CreateMemberFromBoldType(BoldType: TBoldElementTypeInfo): TBoldMember;
    class function CreateMemberFromExpressionName(SystemTypeInfo: TBoldSystemTypeInfo; const Name: String): TBoldMember;
  end;

  {---TBoldAttribute---}
  TBoldAttribute = class(TBoldMember)
  private
    function GetBoldAttributeRTInfo: TBoldAttributeRTInfo;
    function GetElementTypeInfoForType: TBoldElementTypeInfo; override;
    function GetIsNull: Boolean;
    procedure MakeDbCurrent; override;
    procedure NullFailure;
  protected
    procedure AssignValue(const Source: IBoldValue); virtual; abstract;
    procedure AssignContentValue(const Source: IBoldValue); virtual; abstract;
    procedure DoSetInitialValue; override;
    property ContentIsNull: Boolean index befIsNull read GetElementFlag;
    class function EitherIsNull(Attribute1, Attribute2: TBoldAttribute): Boolean;
    procedure EnsureNotNull;
    procedure FormatFailure(const value, ExpectedDataType: String);
    function GetAttributeTypeInfoForType: TBoldElementTypeInfo; virtual;
    function GetStreamName: string; override;
    function StringCompare(CompareType: TBoldCompareType; const s1, s2: string): integer;
    function NullBiggest(BoldElement: TBoldElement): Integer;
    function NullSmallest(BoldElement: TBoldElement): Integer;
    procedure SetContentToNull;
    procedure SetToNonNull;
  public
    function GetAsVariant: Variant; override;
    procedure SetAsVariant(const Value: Variant); override;
    function CompareToAs(CompType: TBoldCompareType; BoldElement: TBoldElement): Integer; override;
    procedure DefaultSubscribe(Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent = breReEvaluate); override;
    function CanSetToNull(Subscriber: TBoldSubscriber): Boolean;
    function IsEqualToValue(const Value: IBoldValue): Boolean; override;
    procedure RecycleValue;
    procedure SetToNull; virtual;
    procedure Assign(Source: TBoldElement); override;
    function ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean; override;
    procedure SubscribeToStringRepresentation(Representation: TBoldRepresentation; Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent = breReEvaluate); override;
    procedure SetEmptyValue; virtual;
    function ValidateVariant(const Value: Variant; Representation: TBoldRepresentation = brDefault): Boolean; override;
    function IsVariantTypeCompatible(const Value: Variant): Boolean; virtual;
    property BoldAttributeRTInfo: TBoldAttributeRTInfo read GetBoldAttributeRTInfo;
    property IsNull: Boolean read GetIsNull;
  end;

  { TBoldAttribute_Proxy }
  TBoldAttribute_Proxy = class(TBoldMember_Proxy, IBoldNullableValue, IBoldStringRepresentable)
  private
    function GetProxedAttribute: TBoldAttribute;
  protected
    procedure AssignContentValue(const Source: IBoldValue); override;
    procedure SetContentToNull;
    function GetContentIsNull: Boolean;
    property ProxedAttribute: TBoldAttribute read GetProxedAttribute implements IBoldNullableValue;
    function GetStringRepresentation(representation:integer): String;
    procedure SetStringRepresentation(Representation: integer; const NewValue: String);
    function GetContentAsString: String; virtual;
  end;

  { TBoldFailureReason }
  TBoldFailureReason = class(TObject)
  private
    FMessageFormatStr: String;
    fOriginator: TBoldElement;
    fReason: string;
    fSubscriber: TBoldPassThroughSubscriber;
    procedure ReceiveOriginatorDestroy(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
  protected
    function GetException(const Msg: String): EBoldFailure; virtual;
  public
    constructor Create(AReason: String; Originator: TBoldElement);
    constructor CreateFmt(Reason: string; const args: array of const; Originator: TBoldElement);
    destructor Destroy; override;
    property MessageFormatStr: String read fMessageFormatStr write fMessageFormatStr;
    property Originator: TBoldElement read fOriginator;
    property Reason: string read fReason;
  end;

  {---TBoldObjectReference---}
  TBoldObjectReference = class(TBoldMember)
  private
    fObjectReferenceController: TBoldAbstractObjectReferenceController;
    procedure AdjustOldValues(Translationlist: TBoldIdTranslationlist); override;
    function GetBoldObject: TBoldObject;
    function GetBoldRoleRTInfo: TBoldRoleRTInfo;
    function InternalMayUpdate: Boolean; override;
    procedure InternalSetLocator(NewLocator: TBoldObjectLocator);
    procedure MakeDbCurrent; override;
    procedure SetBoldObject(NewObject: TBoldObject);
    function GetController: TBoldAbstractController; override;
    property ReferenceController: TBoldAbstractObjectReferenceController read fObjectReferenceController;
    function VerifyClass(aLocator: TBoldObjectLocator): Boolean;
    procedure AssignContentValueFromElement(source: TBoldElement); override;
    function GetOldEmbeddingOtherEndId: TBoldObjectId;
    procedure PreDiscard; override;
    procedure DoSetInitialValue; override;
    function GetLocator: TBoldObjectLocator;
    procedure SetLocator(NewLocator: TBoldObjectLocator);
    function GetIsEmpty: boolean;
  protected
    function GetStreamName: String; override;
    function GetFreeStandingClass: TBoldFreeStandingElementClass; override;
    function GetStringRepresentation(Representation: TBoldRepresentation): string; override;
    procedure Initialize; override;
    procedure SetStringRepresentation(Representation: TBoldRepresentation; const Value: string); override;
    function GetProxy(Mode: TBoldDomainElementProxyMode): TBoldMember_Proxy; override;
  public
    constructor CreateTypedReference(ObjectClass: TBoldObjectClass);
    destructor Destroy; override;
    procedure Assign(Source: TBoldElement); override;
    function CanClear(Subscriber: TBoldSubscriber): Boolean;
    function CanSet(NewObject: TBoldObject; Subscriber: TBoldSubscriber): Boolean;
    function CanSetLocator(NewLocator: TBoldObjectLocator; Subscriber: TBoldSubscriber): Boolean;
    procedure Clear;
    function CompareToAs(CompareType: TBoldCompareType; BoldElement: TBoldElement): Integer; override;
    procedure DefaultSubscribe(Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent = breReEvaluate); override;
    procedure GetAsList(ResultList: TBoldIndirectElement); override;
    function IsEqualAs(CompareType: TBoldCompareType; BoldElement: TBoldElement): Boolean; override;
    function IsEqualToValue(const Value: IBoldValue): Boolean; override;
    function ObserverMayModify(Observer: TObject): Boolean; override;
    function ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean; override;
    procedure SubscribeToStringRepresentation(Representation: TBoldRepresentation; Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent = breReEvaluate); override;
    property BoldObject: TBoldObject read GetBoldObject write SetBoldObject;
    property BoldRoleRTInfo: TBoldRoleRTInfo read GetBoldRoleRTInfo;
    property Locator: TBoldObjectLocator read GetLocator write SetLocator;
    property OldEmbeddingOtherEndId: TBoldObjectId read GetOldEmbeddingOtherEndId;
    property HasOldValues: Boolean index befHasOldValues read GetElementFlag write SetElementFlag;
    property IsEmpty: boolean read GetIsEmpty;
  end;

  TBoldListEnumerator = class
  private
    FIndex: Integer;
    FList: TBoldList;
  protected
    property Index: Integer read fIndex;
    property List: TBoldList read fList;
  public
    constructor Create(AList: TBoldList);
    function GetCurrent: TBoldElement;
    function MoveNext: Boolean;
    property Current: TBoldElement read GetCurrent;
  end;

  { TBoldList }
  TBoldList = class(TBoldMember)
  private
    fListController: TBoldListController;
    function GetController: TBoldAbstractController; override;
    function GetDuplicateMode: TBoldListDupMode;
    procedure PrepareClear; virtual;
    procedure SetDuplicateMode(NewMode: TBoldListDupMode);
    function GetFirst: TBoldElement;
    function GetLast: TBoldElement;
    function GetEmpty: Boolean;
    function DefaultCompare(Item1, Item2: TBoldElement): Integer;
    function GetCapacity: integer; virtual;
    procedure SetCapacity(const Value: integer); virtual;
  protected
    procedure AddElement(Element: TBoldElement); virtual; abstract;
    procedure AllocateData; virtual;
    function CreateNew: TBoldElement; virtual;
    function DuplicateControl: Boolean;
    procedure EnsureCanCreateNew;
    procedure FreeData; virtual; abstract;
    function GetCount: Integer; virtual; abstract;
    function GetElement(index: Integer): TBoldElement; virtual; abstract;
    function GetStringRepresentation(Representation: TBoldRepresentation): string; override;
    function IncludesElement(Item: TBoldElement): Boolean; virtual; abstract;
    function IndexOfElement(Item: TBoldElement): Integer; virtual; abstract;
    procedure Initialize; override;
    procedure InsertElement(index: Integer; Element: TBoldElement); virtual; abstract;
    function InternalAddNew: TBoldElement; virtual; abstract;
    function GetCanCreateNew: Boolean; virtual;
    procedure SetElement(index: Integer; Value: TBoldElement); virtual; abstract;
    procedure InternalClear; virtual; abstract;
    property ListController: TBoldListController read fListController write fListController;
  public
    constructor CreateWithTypeInfo(ElementTypeInfo: TBoldElementTypeInfo); override;
    destructor Destroy; override;
    function GetEnumerator: TBoldListEnumerator;
    procedure Add(Element: TBoldElement);
    procedure AddList(List: TBoldList); virtual;
    procedure RemoveList(List: TBoldList); virtual;
    procedure IntersectList(List: TBoldList); virtual;
    function AddNew: TBoldElement;
    procedure AddToStrings(Representation: TBoldRepresentation; S: TStrings);
    function CanClear(Subscriber: TBoldSubscriber): Boolean;
    function CanInsert(index: Integer; Element: TBoldElement; Subscriber: TBoldSubscriber): Boolean; virtual;
    function CanMove(CurIndex, NewIndex: Integer; Subscriber: TBoldSubscriber = nil): Boolean; virtual;
    function CanRemove(index: Integer; Subscriber: TBoldSubscriber): Boolean; virtual;
    function CanSet(index: Integer; Item: TBoldElement; Subscriber: TBoldSubscriber): Boolean; virtual;
    procedure Clear;
    function CompareToAs(CompType: TBoldCompareType; BoldElement: TBoldElement): Integer; override;
    procedure DefaultSubscribe(Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent = breReEvaluate); override;
    procedure EnsureRange(FromIndex: integer = 0; ToIndex: integer = MaxInt); virtual;
    procedure GetAsList(ResultList: TBoldIndirectElement); override;
    function Includes(Item: TBoldElement): Boolean;
    function IncludesAny(aList: TBoldList): Boolean;
    function IncludesAll(aList: TBoldList): Boolean;
    function IndexOf(Item: TBoldElement): Integer;
    procedure Insert(index: Integer; Element: TBoldElement);
    procedure InsertNew(index: Integer); virtual; abstract;
    procedure Move(CurIndex, NewIndex: Integer); virtual; abstract;
    procedure MakeContentsImmutable;
    procedure Remove(Item: TBoldElement; ARaiseIfNotFound: boolean = true); virtual;
    procedure RemoveByIndex(index: Integer); virtual; abstract;
    procedure Sort(CompareFunc: TBoldElementCompare; FirstIndex, LastIndex:
        Integer; SortMode: TBoldSortMode = BoldDefaultSortMode); overload;
    procedure Sort(CompareFunc: TBoldElementCompare; SortMode: TBoldSortMode =
        BoldDefaultSortMode); overload;
    procedure Sort; overload;
    procedure ToStrings(Representation: TBoldRepresentation; S: TStrings);
    procedure ToStringsWithNil(Representation: TBoldRepresentation; S: TStrings; nilString: string);
    function HasDuplicates: boolean;
    function AsCommaText(AIncludeType: boolean = true; Representation: TBoldRepresentation = brDefault; const ASeparator: string = ','): string;
    function AsDebugCommaText(const ASeparator: string = ','): string;
    property CanCreateNew: Boolean read GetCanCreateNew;
    property Count: Integer read GetCount;
    property DuplicateMode: TBoldListDupMode read GetDuplicateMode write SetDuplicateMode;
    property Elements[index: Integer]: TBoldElement read GetElement write SetElement; default;
    property Empty: Boolean read GetEmpty;
    property First: TBoldElement read GetFirst;
    property Last: TBoldElement read GetLast;
    property Capacity: integer read GetCapacity write SetCapacity;
  end;

  TBoldMemberListEnumerator = class(TBoldListEnumerator)
  public
    function GetCurrent: TBoldMember;
    property Current: TBoldMember read GetCurrent;
  end;

  {---TBoldMemberList---}
  TBoldMemberList = class(TBoldList)
  private
    faList: TList;
    FCloneMembers: Boolean;
    function CheckAdd(NewMember: TBoldMember): Boolean;
    function CheckInsert(index: Integer; NewMember: TBoldMember): Boolean;
    function CheckReplace(index: Integer; NewMember: TBoldMember): Boolean;
    procedure SetCloneMembers(const Value: Boolean);
    function GetBoldMember(index: Integer): TBoldMember;
    procedure SetBoldMember(index: Integer; Value: TBoldMember);
    procedure InternalAddWithoutCloning(Item: TBoldMember);
    function GetFirst: TBoldMember;
    function GetLast: TBoldMember;
    function GetCapacity: integer; override;
    procedure SetCapacity(const Value: integer); override;
    property List: TList read faList;
  protected
    procedure AddElement(Element: TBoldElement); override;
    procedure AllocateData; override;
    function CreateNew: TBoldElement; override;
    procedure FreeData; override;
    function GetCanCreateNew: Boolean; override;
    function GetCount: Integer; override;
    function GetElement(index: Integer): TBoldElement; override;
    function GetStreamName: String; override;
    function GetStringRepresentation(Representation: TBoldRepresentation): string; override;
    procedure SetStringRepresentation(Representation: integer; const NewValue: String); override;
    function IncludesElement(Item: TBoldElement): Boolean; override;
    function IncludesValue(Item: TBoldElement): Boolean;
    function IndexOfElement(Item: TBoldElement): Integer; override;
    procedure Initialize; override;
    procedure InsertElement(index: Integer; Element: TBoldElement); override;
    procedure SetElement(index: Integer; Value: TBoldElement); override;
    function InternalAddNew: TBoldElement; override;
    function GetProxy(Mode: TBoldDomainElementProxyMode): TBoldMember_Proxy; override;
    procedure InternalClear; override;
    procedure AssignCloneValue(AClone: TBoldMember); override;
  public
    function GetEnumerator: TBoldMemberListEnumerator;
    procedure Add(Item: TBoldMember);
    procedure Assign(Source: TBoldElement); override;
    function IndexOf(Item: TBoldMember): Integer;
    function IndexOfFirstEqualElement(Item: TBoldMember): Integer;
    procedure Insert(index: Integer; Item: TBoldMember);
    procedure InsertNew(index: Integer); override;
    procedure Move(CurIndex, NewIndex: Integer); override;
    procedure RemoveByIndex(index: Integer); override;
    property BoldMembers[index: Integer]: TBoldMember read GetBoldMember write SetBoldMember; default;
    property First: TBoldMember read GetFirst;
    property Last: TBoldMember read GetLast;
    property CloneMembers: Boolean read FCloneMembers write SetCloneMembers;
  end;

  TBoldObjectListEnumerator = class(TBoldListEnumerator)
  public
    function GetCurrent: TBoldObject;
    property Current: TBoldObject read GetCurrent;
  end;

  TBoldObjectListLocatorEnumerator = class(TBoldListEnumerator)
  public
    function GetCurrent: TBoldObjectLocator;
    property Current: TBoldObjectLocator read GetCurrent;
  end;

  {---TBoldObjectList ---}
  TBoldObjectList = class(TBoldList)
  private
    function CheckAdd(NewLocator: TBoldObjectLocator): Boolean;
    function CheckInsert(index: Integer; NewLocator: TBoldObjectLocator): Boolean;
    function CheckReplace(index: Integer; NewLocator: TBoldObjectLocator): Boolean;
    function GetBoldRoleRTInfo: TBoldRoleRTInfo;
    function GetObjectListController: TBoldAbstractObjectListController;
    procedure MakeDbCurrent; override;
    function GetSubscribeToObjectsInList: Boolean;
    procedure SetSubscribeToObjectsInList(const Value: Boolean);
    function GetSubscribeToLocatorsInList: Boolean;
    procedure SetSubscribeToLocatorsInList(const Value: Boolean);
    procedure InternalRemoveByIndex(index: Integer);
    procedure AssignContentValueFromElement(source: TBoldElement); override;
    procedure PrepareClear; override;
    function GetBoldObject(index: Integer): TBoldObject;
    function GetElementTypeInfoForType: TBoldElementTypeInfo; override;
    function GetLocator(index: Integer): TBoldObjectLocator;
    procedure SetBoldObject(index: Integer; NewObject: TBoldObject);
    procedure SetLocator(index: Integer; NewLocator: TBoldObjectLocator);
    function VerifyClass(aLocator: TBoldObjectLocator): Boolean;
    function GetListElementTypeInfo: TBoldClassTypeInfo;
    function GetFirst: TBoldObject;
    function GetLast: TBoldObject;
  protected
    procedure AddElement(Element: TBoldElement); override;
    procedure AllocateData; override;
    procedure FreeData; override;
    function GetCount: Integer; override;
    function GetElement(index: Integer): TBoldElement; override;
    function GetStreamName: String; override;
    function GetFreeStandingClass: TBoldFreeStandingElementClass; override;
    function IncludesElement(Item: TBoldElement): Boolean; override;
    function IndexOfElement(Item: TBoldElement): Integer; override;
    function GetEvaluator: TBoldEvaluator; override;
    procedure Initialize; override;
    procedure InsertElement(index: Integer; Element: TBoldElement); override;
    procedure SetElement(index: Integer; Value: TBoldElement); override;
    function GetProxy(Mode: TBoldDomainElementProxyMode): TBoldMember_Proxy; override;
    procedure FreeContent; override;
    function InternalAddNew: TBoldElement; override;
    procedure InternalClear; override;
    procedure AssignCloneValue(AClone: TBoldMember); override;
    property ObjectListController: TBoldAbstractObjectListController read GetObjectListController;
  public
    function GetEnumerator: TBoldObjectListEnumerator;
    function GetLocatorEnumerator: TBoldObjectListLocatorEnumerator;
    constructor InternalCreateClassList(System: TBoldSystem; ListTypeInfo: TBoldListTypeInfo);
    constructor CreateRootClassList(ABoldSystem: TBoldSystem = nil);
    procedure Add(BoldObject: TBoldObject);
    procedure AddList(List: TBoldList); override;
    procedure AddLocator(NewLocator: TBoldObjectLocator);
    procedure Assign(Source: TBoldElement); override;
    function AtTime(Time: TBoldTimestampType): TBoldMember; override;
    function CreateObjectIdList(WithoutDuplicates: boolean = false): TBoldObjectIdList;
    procedure EnsureObjects;
    procedure EnsureRange(FromIndex: integer = 0; ToIndex: integer = MaxInt); override;
    procedure FillFromIDList(ObjectIdList: TBoldObjectIdList; BoldSystem: TBoldSystem);
    procedure RemoveDeletedObjects;
    function FilterOnType(BoldClassTypeInfo: TBoldClassTypeInfo; IncludeSubclasses: boolean = true): TBoldObjectList;
    function GetByIndex(MemberList: TBoldMemberList): TBoldObject;
    function GetByIndexAndSubscribe(MemberList: TBoldMemberList; Subscriber: TBoldSubscriber): TBoldObject;
    function Includes(BoldObject: TBoldObject): Boolean;
    function IndexOf(BoldObject: TBoldObject): Integer;
    function IndexOfLocator(Locator: TBoldObjectLocator): Integer;
    procedure InsertNew(index: Integer); override;
    procedure Insert(index: Integer; BoldObject: TBoldObject);
    procedure InsertLocator(index: Integer; Locator: TBoldObjectLocator);
    function LocatorInList(NewLocator: TBoldObjectLocator): Boolean;
    function CanInsert(index: Integer; Element: TBoldElement; Subscriber: TBoldSubscriber): Boolean; override;
    function CanInsertLocator(index: Integer; Locator: TBoldObjectLocator; Subscriber: TBoldSubscriber): Boolean;
    function CanSet(index: Integer; Element: TBoldElement; Subscriber: TBoldSubscriber): Boolean; override;
    function CanSetLocator(index: Integer; Locator: TBoldObjectLocator; Subscriber: TBoldSubscriber): Boolean;
    procedure Move(CurIndex, NewIndex: Integer); override;
    function ObserverMayModify(Observer: TObject): Boolean; override;
    function ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean; override;
    procedure RemoveByIndex(index: Integer); override;
    procedure RemoveLocator(ALocator: TBoldObjectLocator);
    procedure DeleteObjects;
    function BeginUpdate: boolean;
    procedure EndUpdate;
    function IsEqualToValue(const Value: IBoldValue): Boolean; override;
    function LeastCommonClassType: TBoldClassTypeInfo;
    function Clone: TBoldMember; overload; override;
    function Clone(ACopyDuplicateMode: boolean; ASubscribeToObjectsInList: boolean): TBoldObjectList; reintroduce; overload;
    property BoldObjects[index: Integer]: TBoldObject read GetBoldObject write SetBoldObject; default;
    property BoldRoleRTInfo: TBoldRoleRTInfo read GetBoldRoleRTInfo;
    property Locators[index: Integer]: TBoldObjectLocator read GetLocator write SetLocator;
    property SubscribeToObjectsInList: Boolean read GetSubscribeToObjectsInList write SetSubscribeToObjectsInList;
    property SubscribeToLocatorsInList: Boolean read GetSubscribeToLocatorsInList write SetSubscribeToLocatorsInList;
    property Adjusted: Boolean index befAdjusted read GetElementFlag write SetElementFlag;
    property ListElementTypeInfo: TBoldClassTypeInfo read GetListElementTypeInfo;
    property First: TBoldObject read GetFirst;
    property Last: TBoldObject read GetLast;
  end;

  TBoldLinkUnlinkMode = (blulNone, blulMarkModified, blulMarkAdjusted);

  {---TBoldAbstractController---}
  TBoldAbstractController = class(TBoldMemoryManagedObject)
  protected
    function GetRoleRTInfo: TBoldRoleRTInfo;
    function GetBoldSystem: TBoldSystem;
    function GetOwningObject: TBoldObject;
    procedure Changed(Event: TBoldEvent; const Args: array of const);
    function GetStreamName: string; virtual; abstract;
    function GetFreeStandingClass: TBoldFreeStandingElementClass; virtual;
    function GetOwningMember: TBoldMember; virtual; abstract;
    function LocatorForID(ObjectId: TBoldObjectId): TBoldObjectLocator;
    function AssertedLocatorForID(ObjectId: TBoldObjectId): TBoldObjectLocator;
    procedure PreChange;
    function StartModify: Boolean;
    class function GetControllerForMember(Member: TBoldMember): TBoldAbstractController;
    function NewValueInOptimisticLocking: IBoldValue;
    procedure DbFetchOwningMember;
    procedure DbFetchClassForMember(Timestamp: TBoldTimestampType);
    procedure EndModify;
  public
    function AssertIntegrity: Boolean; virtual;
    procedure Unlink(OldLocator: TBoldObjectLocator; Mode: TBoldLinkUnlinkMode); virtual;
    procedure LinkTo(NewLocator: TBoldObjectLocator; updateOrderNo: Boolean; Mode: TBoldLinkUnlinkMode); virtual;
    function ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean; virtual;
    property StreamName: String read GetStreamName;
    property OwningMember: TBoldMember read GetOwningMember;
    property OwningObject: TBoldObject read GetOwningObject;
    property RoleRTInfo: TBoldRoleRTInfo read GetRoleRTInfo;
    property BoldSystem: TBoldSystem read GetBoldSystem;
  end;

  {---TBoldListController---}
  TBoldListController = class(TBoldAbstractController)
  private
    fOwningList: TBoldList;
  protected
    function GetOwningMember: TBoldMember; override;
    property OwningList: TBoldList read fOwningList;
    function GetCount: Integer; virtual; abstract;
    function GetCanCreateNew: Boolean; virtual;
    function CreateNew: TBoldElement; virtual;
    function GetStringrepresentation: String; virtual;
    function GetCapacity: integer; virtual;
    procedure SetCapacity(const Value: integer); virtual;
    function GetIsEmpty: Boolean; virtual;
  public
    constructor Create(OwningList: TBoldList); virtual;
    function AssertIntegrity: Boolean; override;
    procedure AddElement(Element: TBoldElement); virtual; abstract;
    function GetElement(index: Integer): TBoldElement; virtual; abstract;
    function IncludesElement(Item: TBoldElement): Boolean; virtual; abstract;
    function IndexOfElement(Item: TBoldElement): Integer; virtual; abstract;
    procedure InsertElement(index: Integer; Element: TBoldElement); virtual; abstract;
    procedure Move(CurrentIndex: Integer; NewIndex: Integer); virtual; abstract;
    procedure RemoveByIndex(index: Integer); virtual; abstract;
    procedure SetElement(index: Integer; Value: TBoldElement); virtual; abstract;
    property CanCreateNew: Boolean read GetCanCreateNew;
    property Count: integer read GetCount;
    property IsEmpty: Boolean read GetIsEmpty;
    property Capacity: integer read GetCapacity write SetCapacity;
    property Elements[index: Integer]: TBoldElement read GetElement write SetElement; default;
  end;

  {---TBoldAbstractObjectListController---}
  TBoldAbstractObjectListController = class(TBoldListController)
  private
  protected
    function GetDebugInfo: string; override;
    function GetObjectList: TBoldObjectList;
    property OwningObjectList: TBoldObjectList read GetObjectList;
    function GetProxy(Member: TBoldMember; Mode: TBoldDomainElementProxyMode): TBoldMember_Proxy; virtual; abstract;
    procedure PrepareClear; virtual;
  public
    procedure AddElement(Element: TBoldElement); override;
    procedure AddLocator(Element: TBoldObjectLocator); virtual; abstract;
    function AtTime(Time: TBoldTimestampType): TBoldMember; virtual;
    function GetElement(index: Integer): TBoldElement; override;
    function GetLocator(index: Integer): TBoldObjectLocator; virtual; abstract;
    function GetLocatorByQualifiersAndSubscribe(MemberList: TBoldMemberList; Subscriber: TBoldSubscriber): TBoldObjectLocator; virtual; abstract;
    function HandlesAtTime: Boolean; virtual;
    function IncludesElement(Item: TBoldElement): Boolean; override;
    function IncludesLocator(Item: TBoldObjectLocator): Boolean; virtual; abstract;
    function IndexOfElement(Item: TBoldElement): Integer; override;
    function IndexOfLocator(Item: TBoldObjectLocator): Integer; virtual; abstract;
    procedure InsertElement(index: Integer; Element: TBoldElement); override;
    procedure InsertLocator(index: Integer; Element: TBoldObjectLocator); virtual; abstract;
    procedure MakeDbCurrent; virtual; abstract;
    procedure SetElement(index: Integer; Value: TBoldElement); override;
    procedure SetLocator(index: Integer; Value: TBoldObjectLocator); virtual; abstract;
    procedure FreeContent; virtual;
    procedure Clear; virtual;
  end;

  {---TBoldAbstractObjectReferenceController---}
  TBoldAbstractObjectReferenceController = class(TBoldAbstractController)
  private
    fOwningReference: TBoldObjectReference;
  protected
    function GetOwningMember: TBoldMember; override;
    function MayUpdate: Boolean; virtual;
    procedure PreDiscard; virtual;
    function OtherEndControllerForLinkObject(Obj: TBoldObject): TBoldAbstractObjectReferenceController;
    function ControllerForLinkRole: TBoldAbstractObjectReferenceController;
    function ControllerForMainRole: TBoldAbstractObjectReferenceController;
    function GetProxy(Member: TBoldMember; Mode: TBoldDomainElementProxyMode): TBoldMember_Proxy; virtual; abstract;
  public
    constructor Create(Owner: TBoldObjectReference); virtual;
    function GetLocator: TBoldObjectLocator; virtual; abstract;
    procedure SetLocator(NewLocator: TBoldObjectLocator); virtual; abstract;
    procedure MakeDbCurrent; virtual; abstract;
    property OwningReference: TBoldObjectReference read fOwningReference;
  end;

  { TBoldObjectReferenceController }
  TBoldObjectReferenceController = class(TBoldAbstractObjectReferenceController)
  private
    fLocator: TBoldObjectLocator;
    fSubscriber: TBoldPassThroughSubscriber;
    procedure ObjectChangeReceive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
  protected
    function GetStreamName: string; override;
    function GetFreeStandingClass: TBoldFreeStandingElementClass; override;
    function GetProxy(Member: TBoldMember; Mode: TBoldDomainElementProxyMode): TBoldMember_Proxy; override;
  public
    constructor Create(Owner: TBoldObjectReference); override;
    destructor Destroy; override;
    procedure AssignContentValue(const Source: IBoldValue);
    function GetLocator: TBoldObjectLocator; override;
    procedure SetLocator(NewLocator: TBoldObjectLocator); override;
    procedure MakeDbCurrent; override;
  end;

  { EBoldOperationFailedForObjectList }
  EBoldOperationFailedForObjectList = class(EBold)
  private
    FObjectList: TBoldObjectList;
  public
    constructor Create (const msg: string; args: array of const; IdList: TBoldObjectIdList; System: TBoldSystem);
    destructor Destroy; override;
    property ObjectList: TBoldObjectList read FObjectList;
  end;

  { EBoldFailure }
  EBoldFailure = class(EBold)
  private
    fReasonObject: TBoldFailureReason;
  public
    destructor Destroy; override;
    property ReasonObject: TBoldFailureReason read fReasonObject write fReasonObject;
  end;

  {$IFNDEF T2H}
  {---TBoldLocatorHashIndex---}
  TBoldLocatorHashIndex = class(TBoldHashIndex)
  protected
    function ItemAsLocator(Item: TObject): TBoldObjectLocator; virtual;
    function HashItem(Item: TObject): Cardinal; override;
    function Match(const Key; Item:TObject):Boolean; override;
    function Hash(const Key): Cardinal; override;
  public
    function FindLocatorByLocator(BoldObjectLocator: TBoldObjectLocator): TBoldObjectLocator;
  end;
  {$ENDIF}

  TBoldMemberDeriver = class(TBoldAbstractDeriver)
  strict private
    fDerivedMember: TBoldMember;
  strict protected
    procedure SetInternalDeriverState(const Value: TBoldDeriverState); override;
    function GetInternalDeriverState: TBoldDeriverState; override;
    procedure DoNotifyOutOfDate; override;
    function GetDerivedObject: TObject; override;
    procedure DoDeriveAndSubscribe(subscribe: Boolean); override;
    procedure DoReverseDerive; override;
    function GetCanReverseDerive: Boolean; override;
  public
    constructor Create(Member: TBoldMember);
    destructor Destroy; override;
    property DerivedMember: TBoldMember read fDerivedMember;
    property CanReverseDerive: Boolean read GetCanReverseDerive;
  end;

  IBoldSystemObjectContents = interface
  ['{FB4110D9-C06D-4E92-B6A9-242D8836BC79}']
    function EnsureMemberAndGetValueByIndex(Member: TBoldMember): IBoldValue;
  end;

  TBoldSystemFreeStandingObjectContents = class(TBoldFreeStandingObjectContents, IBoldSystemObjectContents)
    function EnsureMemberAndGetValueByIndex(Member: TBoldMember): IBoldValue;
  end;

function GetBoldLastFailureReason: TBoldFailureReason;
procedure SetBoldLastFailureReason(const Value: TBoldFailureReason);
procedure BoldRaiseLastFailure(originator: TBoldElement; MethodName: String = ''; DefaultMessage: String = '');
procedure BoldClearLastFailure;

function GetTransactionInfo(aSystem: TBoldSystem): string;

function BoldSystemCount: integer;
function BoldSystems(Index: integer): TBoldSystem;

function TopSortedIndex2ClassName(ATopSortedIndex: integer): string;
function VerifyLocatorType(ALocator: TBoldObjectLocator; AExpectedClassType: TBoldClassTypeInfo; ARaise: boolean = true): Boolean;

var
  G_LastFailureReason: TBoldFailureReason = nil;

implementation

uses
  SysUtils,
  Variants,
  Windows,

  BoldCoreConsts,
  BoldLogHandler,
  BoldGuard,
  BoldLinks,
  BoldObjectListControllers,
  BoldOcl,
  Typinfo,
  BoldOptimisticLockingSupport,
  BoldSystemPersistenceHandler,
  BoldSystemOldValueHandler,
  BoldExternalizedReferences,
  BoldDefaultId,
  BoldTaggedValueSupport,
  BoldUndoHandler,
  {$IFDEF BOLDJSON}
  BoldObjectRepresentationJson,
  {$ENDIF}
  {$IFDEF EXPRESSION_OVERRIDES}
  AttracsExpressionOverrides,
  {$ENDIF}
{$IFDEF SpanFetch}
  AttracsSpanFetchManager,
{$ENDIF}
{$IFDEF Attracs}
{$IFDEF BOLD_PERFORMANCE_COUNTERS}
  BoldSystemPerf,
{$ENDIF}
{$ENDIF}
   BoldDefaultStreamNames,
   BoldAttributes,
   BoldFreeStandingValueFactories,
   BoldMath,
   System.Types;
//  BoldJSONWriter;

var
  G_DefaultBoldSystem: TBoldSystem = nil;
  _BoldSystemList: TList;

procedure BoldSystemActivated(ABoldSystem: TBoldSystem);
begin
  if not Assigned(_BoldSystemList) then
    _BoldSystemList := TList.Create;
  _BoldSystemList.Add(ABoldSystem);
end;

procedure BoldSystemDeActivated(ABoldSystem: TBoldSystem);
begin
  if Assigned(_BoldSystemList) then
    _BoldSystemList.Remove(ABoldSystem);
end;

function BoldSystems(Index: integer): TBoldSystem;
begin
  if (Index < 0) or (Index > BoldSystemCount-1) then
    raise EBold.Create('Index out of bounds in function BoldSystems(Index: integer): TBoldSystem;');
  result := TBoldSystem(_BoldSystemList[Index]);
end;

function BoldSystemCount: integer;
begin
  if Assigned(_BoldSystemList) then
    result := _BoldSystemList.Count
  else
    result := 0;
end;

var
  _BoldSystemInternalLog: TStringList;

procedure _BoldInternalLog(const AMessage: string);
var
  i: integer;
begin
  if not Assigned(_BoldSystemInternalLog) then
  begin
    _BoldSystemInternalLog := TStringList.Create;
    _BoldSystemInternalLog.Sorted := true;
    _BoldSystemInternalLog.Duplicates := dupIgnore;
  end;
  i := _BoldSystemInternalLog.Count;
    _BoldSystemInternalLog.Add(AMessage);
  if _BoldSystemInternalLog.Count > i then
    BoldLog.Log(AMessage);
end;

procedure LogDerivationSideEffects(BoldMember: TBoldMember);
var
  vBoldSystem: TBoldSystem;
  vMessage: string;
begin
  vBoldSystem := BoldMember.BoldSystem;
  Assert(Assigned(vBoldSystem));
  Assert(vBoldSystem.CurrentDerivedMember <> nil);
  if vBoldSystem.CurrentDerivedMember.GetBoldMemberRTInfo.IsReverseDerived then
    vMessage := Format('Reverse Derivation side effects detected, derived member: %s. Dirty member: %s.', [vBoldSystem.CurrentDerivedMember.BoldMemberRTInfo.ExpressionName, BoldMember.BoldMemberRTInfo.ExpressionName])
  else
    vMessage := Format('Derivation side effects detected, derived member: %s. Dirty member: %s.', [vBoldSystem.CurrentDerivedMember.BoldMemberRTInfo.ExpressionName, BoldMember.BoldMemberRTInfo.ExpressionName]);
  _BoldInternalLog(vMessage);
end;

procedure LogDerivationDeleteSideEffects(BoldObject: TBoldObject);
var
  vBoldSystem: TBoldSystem;
  vMessage: string;
begin
  vBoldSystem := BoldObject.BoldSystem;
  Assert(Assigned(vBoldSystem));
  Assert(vBoldSystem.CurrentDerivedMember <> nil);
  if vBoldSystem.CurrentDerivedMember.GetBoldMemberRTInfo.IsReverseDerived then
    vMessage := Format('Reverse Derivation side effects detected, derived member: %s. Deleted object: %s.', [vBoldSystem.CurrentDerivedMember.BoldMemberRTInfo.ExpressionName, BoldObject.BoldClassTypeInfo.AsString])
  else
    vMessage := Format('Derivation side effects detected, derived member: %s. Deleted object: %s.', [vBoldSystem.CurrentDerivedMember.BoldMemberRTInfo.ExpressionName, BoldObject.BoldClassTypeInfo.AsString ]);
  _BoldInternalLog(vMessage);
end;

procedure LogDerivationCreateSideEffects(BoldObject: TBoldObject);
var
  vBoldSystem: TBoldSystem;
  vMessage: string;
begin
  vBoldSystem := BoldObject.BoldSystem;
  Assert(Assigned(vBoldSystem));
  Assert(vBoldSystem.CurrentDerivedMember <> nil);
  if vBoldSystem.CurrentDerivedMember.GetBoldMemberRTInfo.IsReverseDerived then
    vMessage := Format('Reverse Derivation side effects detected, derived member: %s. Created object: %s.', [vBoldSystem.CurrentDerivedMember.BoldMemberRTInfo.ExpressionName, BoldObject.BoldClassTypeInfo.AsString])
  else
    vMessage := Format('Derivation side effects detected, derived member: %s. Created object: %s.', [vBoldSystem.CurrentDerivedMember.BoldMemberRTInfo.ExpressionName, BoldObject.BoldClassTypeInfo.AsString]);
  _BoldInternalLog(vMessage);
end;

function MemberCanBeModified(MemberRtInfo: TBoldMemberRtInfo; BoldSystem: TBoldSystem): Boolean;
begin
  Result :=  assigned(MemberRtInfo) and
     (MemberRtInfo.IsStoredInObject or
     (MemberRtInfo.IsMultiRole and
     (assigned(BoldSystem.PersistenceController) and
     BoldSystem.PersistenceController.MultilinksAreStoredInObject)));
end;

function GetTransactionInfo(aSystem: TBoldSystem): string;
begin
  Result := 'GetTransactionInfo raised exception';
  try
    Result := Format('TransactionNesting:%d TransactionRollbackOnly:%s TransactionMode:%d',
                   [aSystem.fTransactionNesting,
                    BoolToStr(aSystem.fTransactionRollbackOnly, True),
                    Ord(aSystem.fTransactionMode)]);
    Result := Result + Format(' TransactionList.Count:%d', [aSystem.fTransactionList.Count]);
  except
    {Eat}
  end;
end;

function TopSortedIndex2ClassName(ATopSortedIndex: integer): string;
begin
  result := TBoldSystem.DefaultSystem.BoldSystemTypeInfo.TopSortedClasses[ATopSortedIndex].ExpressionName;
end;

{ TBoldDirtyObjectTracker }

constructor TBoldDirtyObjectTracker.Create(ABoldSystem: TBoldSystem);
begin
  inherited Create;
  fBoldSystem := ABoldSystem;
  fSubscriber := TBoldExtendedPassthroughSubscriber.CreateWithExtendedReceive(Receive);
  fDirtyObjects := TBoldObjectList.Create;
  fDirtyObjects.SubscribeToObjectsInList := false;
  fDirtyObjects.SubscribeToLocatorsInList := true;
  fDirtyObjects.DuplicateMode := TBoldListDupMode.bldmMerge
end;

procedure TBoldDirtyObjectTracker.BeforeDestruction;
begin
  if not fDirtyObjects.Empty then
    DiscardChanges;
  inherited;
end;

destructor TBoldDirtyObjectTracker.Destroy;
begin
  fDirtyObjects.free;
  fSubscriber.free;
  inherited;
end;

procedure TBoldDirtyObjectTracker.DiscardChanges;
var
  i: integer;
  BoldObject: TBoldObject;
begin
  if fBoldSystem.InTransaction then
    raise EBold.CreateFmt('%s.DiscardChanges: System is in transaction, discard not allowed.', [ClassName]);
  while not fDirtyObjects.Empty do
  begin
    i := fDirtyObjects.Count;
    BoldObject := fDirtyObjects.Last;
    BoldObject.Discard;
    if i = fDirtyObjects.Count then
      if not BoldObject.BoldDirty then
        fBoldSystem.MarkObjectClean(BoldObject);
  end;
end;

function TBoldDirtyObjectTracker.GetDirtyObjects: TBoldObjectList;
begin
  result := fDirtyObjects;
end;

procedure TBoldDirtyObjectTracker.Receive(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent;
  const Args: array of const);
var
  BoldObject: TBoldObject;
begin
  BoldObject := Args[0].VObject as TBoldObject;
  case OriginalEvent of
    beDirtyListInvalidOrItemDeleted:
    begin
      // this is needed to catch the case when member becomes clean and thus object also becomes clean
      // which is only the case if there are no other dirty members, which we check via BoldObject.MemberModified
      if not fDirtyObjects.Empty and not BoldObject.BoldObjectIsNew and fBoldSystem.DirtyObjectsInvalid then
      begin
        if not BoldObject.MemberModifiedKnown then
          BoldObject.CalculateMemberModified;
        if not BoldObject.MemberModified then
          fDirtyObjects.Remove(BoldObject, false);
      end;
    end;
    beObjectBecomingDirty:
    begin
      fDirtyObjects.Add(BoldObject);
    end;
    beObjectBecomingClean:
    begin
      // When object is deleted and then written to db then beLocatorDestroying is sent which removes object from dirty list automatically
      // in this case the call to Remove will do nothing since it's already removed, and we check for this via InDelayDestructionList, to avoid needless call to remove.
      if not fDirtyObjects.Empty and not BoldObject.InDelayDestructionList then
        fDirtyObjects.Remove(BoldObject, false);
    end;
  end;
end;

procedure TBoldDirtyObjectTracker.CheckStillDirty;
var
  i: integer;
begin
  for i := fDirtyObjects.Count-1 downto 0 do
    if not fDirtyObjects[i].BoldDirty then
      fDirtyObjects.RemoveByIndex(i);
end;

procedure TBoldDirtyObjectTracker.ClearObjects;
begin
  fDirtyObjects.Clear;
end;

procedure TBoldDirtyObjectTracker.StartTracking;
begin
  fBoldSystem.AddSmallSubscription(fSubscriber, [beObjectBecomingClean, beObjectBecomingDirty, beDirtyListInvalidOrItemDeleted], beDefaultRequestedEvent);
  CheckStillDirty;
end;

procedure TBoldDirtyObjectTracker.StopTracking;
begin
  fSubscriber.CancelAllSubscriptions;
end;

procedure TBoldDirtyObjectTracker.WriteChangesToDb;
begin
  if not fDirtyObjects.Empty then
    fDirtyObjects.Locators[0].BoldSystem.UpdateDatabaseWithList(fDirtyObjects)
end;

function TBoldObject.GetBoldExistenceState: TBoldExistenceState;
begin
  result := TBoldExistenceState(GetInternalState(BoldExistenceStateMask, BoldESShift));
end;

function TBoldObject.GetBoldPersistenceState: TBoldValuePersistenceState;
begin
  result := TBoldValuePersistenceState(GetInternalState(BoldPersistenceStateMask, BoldPSShift));
end;

function TBoldSystem.GetUndoHandler: TBoldAbstractUndoHandler;
begin
  if not Assigned(fUndoHandler) then
    fUndoHandler := TBoldUndoHandler.Create(self);
  Result := fUndoHandler;
end;

function TBoldSystem.GetUndoHandlerInterface: IBoldUndoHandler;
begin
  Result := UndoHandler as IBoldUndoHandler;
end;

function EnsureObjectInFsValueSpace(BoldObject: TBoldObject; ValueSpace: TBoldFreeStandingValueSpace; out ACreated: boolean): TBoldFreeStandingObjectContents; overload;
begin
  Assert(Assigned(ValueSpace));
  result := ValueSpace.GetEnsuredFSObjectContentsByObjectId(BoldObject.BoldObjectLocator.BoldObjectId, ACreated);
  if ACreated then
  begin
    result.BoldExistenceState := BoldObject.BoldExistenceState;
    result.BoldPersistenceState := BoldObject.BoldPersistenceState;
    result.TimeStamp := BoldObject.GetTimeStamp;
  end;
end;

function EnsureObjectInFsValueSpace(BoldObject: TBoldObject; ValueSpace: TBoldFreeStandingValueSpace): TBoldFreeStandingObjectContents; overload;
var
  lCreated: boolean;
begin
  result := EnsureObjectInFsValueSpace(BoldObject, ValueSpace, lCreated);
end;

type
  {---TBoldLocatorIdHashIndex---}
  TBoldLocatorIdHashIndex = class(TBoldHashIndex)
  protected
    function ItemAsLocator(Item: TObject): TBoldObjectLocator; {virtual;} // no need to be virtual until we actually override it somewhere
    function HashItem(Item: TObject): Cardinal; override;
    function Match(const Key; Item:TObject):Boolean; override;
    function Hash(const Key): Cardinal; override;
  public
    function FindLocatorById(BoldObjectId: TBoldObjectId): TBoldObjectLocator;
  end;

  { TBoldSystem_Proxy }
  TBoldSystem_Proxy = class(TBoldDomainElement_Proxy, IBoldValueSpace)
  private
    fProxedSystem: TBoldSystem;
    procedure AllObjectIds(resultList: TBoldObjectIdList; OnlyLoaded: Boolean);
    procedure ApplytranslationList(IdTranslationList: TBoldIdTranslationList);
    procedure ApplyValueSpace(const ValueSpace: IBoldValueSpace; IgnorePersistenceState: Boolean);
    procedure EnsureObjectContents(ObjectId: TBoldObjectId);
    procedure EnsureObjectId(ObjectId: TBoldObjectId);
    procedure ExactifyIDs(TranslationList: TBoldIdTranslationList);
    function GetHasContentsForId(ObjectId: TBoldObjectId): boolean;
    function GetObjectContentsByObjectId(ObjectId: TBoldObjectId): IBoldObjectContents;
    function GetEnsuredObjectContentsByObjectId(ObjectId: TBoldObjectId): IBoldObjectContents;
    function GetEnsuredObjectContentsByObjectIdAndCheckIfCreated(ObjectId: TBoldObjectId; out aBoldObjectContents: IBoldObjectContents): boolean;
    function IdCount: integer;
    function IsEmpty: boolean;
  protected
    property ProxedSystem: TBoldSystem read fProxedSystem;
  public
    constructor Create(ProxedSystem: TBoldSystem; Mode:  TBoldDomainElementProxyMode);
  end;

  { TBoldObject_Proxy }
  TBoldObject_Proxy = class(TBoldDomainElement_Proxy, IBoldObjectContents)
  private
    fProxedObject: TBoldObject;
    procedure EnsureMember(MemberId: TBoldMemberId; const ContentName: string);
    function EnsureMemberAndGetValueByIndex(MemberIndex: Integer; const ContentName: string): IBoldValue;
    function GetBoldExistenceState: TBoldExistenceState; {$IFDEF BOLD_INLINE}inline;{$ENDIF}
    function GetBoldMemberCount: Integer; {$IFDEF BOLD_INLINE}inline;{$ENDIF}
    function GetBoldPersistenceState: TBoldValuePersistenceState; {$IFDEF BOLD_INLINE}inline;{$ENDIF}
    function GetGlobalId: string; {$IFDEF BOLD_INLINE}inline;{$ENDIF}
    function GetIsModified: Boolean; {$IFDEF BOLD_INLINE}inline;{$ENDIF}
    function GetIsReadOnly: Boolean; {$IFDEF BOLD_INLINE}inline;{$ENDIF}
    function GetObjectId: TBoldObjectId; {$IFDEF BOLD_INLINE}inline;{$ENDIF}
    function GetValueByIndex(I: Integer): IBoldValue;
    function GetValueByMemberId(MemberId: TBoldMemberID):IBoldValue; {$IFDEF BOLD_INLINE}inline;{$ENDIF}
    function GetTimeStamp: TBoldTimeStampType; {$IFDEF BOLD_INLINE}inline;{$ENDIF}
    procedure SetBoldExistenceState(Value: TBoldExistenceState); {$IFDEF BOLD_INLINE}inline;{$ENDIF}
    procedure SetBoldPersistenceState(Value: TBoldValuePersistenceState); {$IFDEF BOLD_INLINE}inline;{$ENDIF}
    procedure SetGlobalId(const NewValue: string); {$IFDEF BOLD_INLINE}inline;{$ENDIF}
    procedure SetIsReadOnly(NewValue: Boolean); {$IFDEF BOLD_INLINE}inline;{$ENDIF}
    procedure SetTimeStamp(NewValue: TBoldTimeStampType); {$IFDEF BOLD_INLINE}inline;{$ENDIF}
  protected
    constructor Create(ProxedObject: TBoldObject; Mode:  TBoldDomainElementProxyMode);
    property ProxedObject: TBoldObject read fProxedObject;
  end;

  { TBoldObjectReference_Proxy }
  TBoldObjectReference_Proxy = class(TBoldMember_Proxy)
  protected
    procedure AssignContentValue(const Source: IBoldValue); override;
  end;


function GetValueFromValuespace(const ValueSpace: IBoldValueSpace; Id: TBoldObjectId; MemberIndex: integer): IBoldValue;
var
  MemberId: TBoldMemberId;
  ObjectContents: IBoldObjectContents;
  G: IBoldGuard;
begin
  G := TBoldGuard.Create(MemberId);
  Result := nil;
  MemberId := TBoldMemberId.Create(MemberIndex);
  ObjectContents := ValueSpace.ObjectContentsByObjectId[Id];
  if Assigned(ObjectContents) then
     Result := ObjectContents.ValueByMemberId[MemberId]
end;

function GetBoldLastFailureReason: TBoldFailureReason;
begin
  result := G_LastFailureReason;
end;

procedure SetBoldLastFailureReason(const Value: TBoldFailureReason);
begin
  if assigned(G_LastFailureReason) then
    G_LastFailureReason.Free;
  G_LastFailureReason := value;
end;

procedure BoldRaiseLastFailure(originator: TBoldElement; MethodName: String; DefaultMessage: String);
var
  OriginatorStr: String;
  MessageStr: String;
  MessageFormatStr: String;
  failure: EBoldFailure;
begin
  if assigned(G_LastFailureReason) then
  begin
    if assigned(G_LastFailureReason.originator) then
      Originator := G_LastFailureReason.originator;
    MessageStr := G_LastFailureReason.Reason;
    MessageFormatStr := G_LastFailureReason.MessageFormatStr;
  end;

  if assigned(Originator) then
    OriginatorStr := originator.DisplayName
  else
    OriginatorStr := '';

  if MessageStr = '' then
    MessageStr := DefaultMessage;
  if MessageStr = '' then
    MessageStr := sReasonUnknown;

  if MessageFormatStr = '' then
    MessageFormatStr := sFailureMessage;

  if Assigned(G_LastFailureReason) then
    Failure := G_LastFailureReason.GetException(Format(MessageFormatStr, [OriginatorStr, MethodName, MessageStr]))
  else
    Failure := EBoldFailure.CreateFmt(MessageFormatStr, [OriginatorStr, MethodName, MessageStr]);

  if assigned(G_LastFailureReason) then
  begin
    Failure.reasonObject := G_LastFailureReason;
    G_LastFailureReason := nil;
  end;

  raise Failure;
end;

procedure BoldClearLastFailure;
begin
  if assigned(G_LastFailureReason) then
    SetBoldLastFailureReason(nil);
end;

  {---TBoldLocatorHashIndex---}

function TBoldLocatorIdHashIndex.ItemAsLocator(Item: TObject): TBoldObjectLocator;
begin
  Assert(Item is TBoldObjectLocator);
  Result := TBoldObjectLocator(Item);
end;

function TBoldLocatorIdHashIndex.HashItem(Item: TObject): Cardinal;
begin
  Result := ItemAsLocator(Item).BoldObjectID.Hash;
end;

function TBoldLocatorIdHashIndex.Match(const Key; Item:TObject):Boolean;
begin
  Result := TBoldObjectId(Key).Isequal[ItemAsLocator(Item).BoldObjectId];
end;

function TBoldLocatorIdHashIndex.FindLocatorById(boldObjectId:TboldObjectId): TBoldObjectLocator;
begin
  Result := TBoldObjectLocator(Find(boldObjectId));
end;


function TBoldSystemLocatorList.AssertIntegrity: Boolean;
var
  Traverser: TBoldLocatorListTraverser;
begin
  Result := True;
  Traverser := CreateTraverser;
  while Traverser.MoveNext do
  begin
    Assert(Traverser.Locator is TBoldObjectLocator);
    Assert(Traverser.Locator.BoldObjectId is TBoldObjectId);
    Assert(Traverser.Locator.BoldObject is TBoldObject);
  end;
  Traverser.Free;
end;

constructor TBoldSystemLocatorList.Create;
begin
  inherited;
  SetIndexCapacity(1);
  IX_BoldObjectId := -1;
  SetIndexVariable(IX_BoldObjectId, AddIndex(TBoldLocatorIdHashIndex.Create));
end;

{$IFDEF ATINDEXDEBUG}
//PATCH
function TBoldSystemLocatorList.GetDebugInfo: string;
begin
  Result := 'TBoldSystemLocatorList.Count:'+IntToStr(Count)+#13#10+
            (Indexes[0] as TBoldLocatorIdHashIndex).GetDebugInfo;
end;
{$ENDIF}

function TBoldSystemLocatorList.GetEnumerator: TBoldLocatorListTraverser;
begin
  result := CreateTraverser;
end;

function TBoldSystemLocatorList.GetLocatorByID(ObjectID: TBoldObjectId): TBoldObjectLocator;
begin
  Result := TBoldObjectLocator(TBoldHashIndex(Indexes[IX_BoldObjectId]).Find(ObjectID));
end;

function TBoldSystemLocatorList.GetObjectByID(ObjectID: TBoldObjectId): TBoldObject;
var
  Locator: TBoldObjectLocator;
begin
  Locator := GetLocatorByID(ObjectID);
  if Assigned(Locator) then
    Result := Locator.BoldObject
  else
    Result := nil;
end;

function TBoldSystemLocatorList.GetObjectByIDString(
  const ID: string): TBoldObject;
var
  ObjectID: TBoldDefaultID;
begin
  ObjectId := TBoldDefaultID.CreateWithClassID(0, false);
  try
    ObjectId.AsInteger := StrToInt(Id);
    result := GetObjectByID(ObjectId);
  finally
    ObjectID.free;
  end;
end;

function TBoldSystemLocatorList.GetLocatorByIDString(const ID: string): TBoldObjectLocator;
var
  ObjectID: TBoldDefaultID;
begin
  ObjectId := TBoldDefaultID.CreateWithClassID(0, false);
  try
    ObjectId.AsInteger := StrToInt(Id);
    result := GetLocatorByID(ObjectId);
  finally
    ObjectID.free;
  end;
end;

function TBoldSystem.GetEnsuredLocatorByID(ObjectID: TBoldObjectId): TBoldObjectLocator;
begin
  Result := Locators.GetLocatorByID(ObjectID);
  if not Assigned(Result) then
  begin
    Result := TBoldObjectLocator.CreateWithObjectId(Self, ObjectID)
  end;
end;

function TBoldSystem.EnsureLocatorByID(ObjectID: TBoldObjectId; out ACreated: boolean): TBoldObjectLocator;
begin
  Result := Locators.GetLocatorByID(ObjectID);
  ACreated := not Assigned(Result);
  if ACreated then
    Result := TBoldObjectLocator.CreateWithObjectId(Self, ObjectID)
end;

constructor TBoldObjectLocator.CreateWithObjectId(BoldSystem: TBoldSystem; BoldObjectID: TBoldObjectId);
begin
  inherited Create;
  fBoldSystem := BoldSystem;
  fBoldObjectID := BoldObjectId.Clone;
  AddToLocators;
end;

constructor TBoldObjectLocator.CreateWithClassID(BoldSystem: TBoldSystem; TopSortedIndex: integer; Exact: Boolean);
begin
  inherited Create;
  FBoldSystem := BoldSystem;
  fBoldObjectID := TBoldInternalObjectId.CreateWithClassID(TopSortedIndex, Exact);
  AddToLocators;
end;

function TBoldObjectLocator.GetAsString: string;
begin
  Result := fBoldObjectID.AsString;
end;

function TBoldObjectLocator.GetDebugInfo: string;
begin
  if Assigned(FBoldObject) then
    result := FBoldObject.DebugInfo
  else
    result := AsString;
end;

destructor TBoldObjectLocator.Destroy;
begin
  if Assigned(FBoldObject) then
    raise EBoldInternal.CreateFmt(sBoldObjectAssigned, [ClassName]);
  if not BoldSystem.IsDestroying then
    BoldSystem.Locators.RemoveFromAllIndexes(Self);
  FreeAndNil(fBoldObjectID);
  inherited;
end;

procedure TBoldObjectLocator.EnsureBoldObject;
begin
  if not Assigned(FBoldObject) then
    FetchBoldObject;
end;

procedure TBoldObjectLocator.UnloadBoldObject;
begin
  if Assigned(BoldObject) then
  begin
    BoldClearLastFailure;
    if not BoldObject.CanUnload then
      BoldRaiseLastFailure(nil, 'TBoldObjectLocator.UnloadBoldObject', '');

    if not BoldSystem.isDestroying then
      BoldObject.SendEvent(beObjectUnloaded);
    BoldSystem.MarkObjectClean(BoldObject);
    BoldObject.FreePublisher;
    BoldObject.FBoldObjectLocator := nil;
    if not BoldSystem.isDestroying then
    begin
      EmbeddedSingleLinksFromObject;
      FreeEmbeddedSingleLinksOfOtherEnd;
    end;
    FreeAndNil(fBoldObject);
  end;
end;

procedure TBoldObjectLocator.DiscardBoldObject(ADiscardTransientLinks: boolean = true);
begin
  if Assigned(BoldObject) then
  begin
    if ADiscardTransientLinks then
      BoldObject.Discard
    else
      BoldObject.DiscardPersistentMembers;
  end;
end;

procedure TBoldObjectLocator.FetchBoldObject;
begin
  if not BoldObjectID.NonExisting then
  begin
    if not BoldObjectID.IsStorable then
      raise EBoldInternal.CreateFmt('%s.FetchBoldObject: Can''t fetch Internal object', [Classname]);
    BoldSystem.fSystemPersistenceHandler.FetchObjectById(BoldObjectID);
  end;
end;

function TBoldObjectLocator.GetEnsuredBoldObject: TBoldObject;
begin
  if Assigned(Self) then
  begin
    EnsureBoldObject;
    Result := BoldObject;
  end
  else
  begin
    Result := nil
  end;
end;

function TBoldObjectLocator.Hash: Cardinal;
begin
  result := Cardinal(self);
end;

{ TBoldSystem }

function TBoldSystem.GetAsIBoldvalueSpace(Mode: TBoldDomainElementProxyMode): IBoldvalueSpace;
begin
  if not Assigned(fSystemProxyCache[Mode]) then
    ProxyInterface(IBoldValueSpace, Mode, fSystemProxyCache[Mode]);
  result := fSystemProxyCache[Mode];
end;

constructor TBoldSystem.CreateWithTypeInfo(AOwningElement: TBoldDomainElement; SystemTypeInfo: TBoldSystemTypeInfo; PersistenceController: TBoldPersistenceController; RegionFactory: TBoldAbstractRegionFactory = nil);
var
  I: Integer;
  ListTypeInfo: TBoldListTypeInfo;
  ListClass: TBoldObjectListClass;

{$IFDEF ACCESSSTATISTICS} //// START PATCH ACCESSSTATISTICS ////////////////////
  procedure InitAccessStats;
  var
    I: Integer;
    ClassTypeInfo: TBoldClassTypeInfo;
  begin
    SetLength(fAccessStats, SystemTypeInfo.TopSortedClasses.Count);
    SetLength(fDeriveStats, SystemTypeInfo.TopSortedClasses.Count);
    SetLength(fInvalidateStats, SystemTypeInfo.TopSortedClasses.Count);
    SetLength(fModifyStats, SystemTypeInfo.TopSortedClasses.Count);
    for I := 0 to SystemTypeInfo.TopSortedClasses.Count-1 do
    begin
      ClassTypeInfo := SystemTypeInfo.TopSortedClasses[I];
      SetLength(fAccessStats[I], ClassTypeInfo.AllMembersCount);
      SetLength(fDeriveStats[I], ClassTypeInfo.AllMembersCount);
      SetLength(fInvalidateStats[I], ClassTypeInfo.AllMembersCount);
      SetLength(fModifyStats[I], ClassTypeInfo.AllMembersCount);
    end;
  end;
{$ENDIF} //// END PATCH ACCESSSTATISTICS ///////////////////////////////////////

begin
  inherited CreateWithOwner(AOwningElement);
  fTransactionMode := stmNormal;
  fOldValueHandler := TBoldOldValueHandler.Create(Self);
  fBoldSystemTypeInfo := SystemTypeInfo;
  SetElementFlag(befpersistent, SystemTypeInfo.Persistent and assigned(PersistenceController));
  Assert((PersistenceController = nil) or (PersistenceController is TBoldPersistenceController));
  fPersistenceController := PersistenceController;
  fPersistenceControllerSubscriber := TBoldPassthroughSubscriber.Create(ReceiveFromPersistenceController);
  if Assigned(fPersistenceController) then
    fPersistenceController.SubscribeToPersistenceEvents(fPersistenceControllerSubscriber, [beDestroying, bpeStartFetch, bpeEndFetch]);
  fDirtyObjects := TList.Create;
  DirtyObjectsInvalid := False;
  fClasses := TBoldMemberList.Create;
  fClasses.fBoldMetaType := BoldSystemTypeInfo.ListTypeInfoByElement[nil];
  fLocators := TBoldSystemLocatorList.Create;
  fLocators.OwnsEntries := True;
  fDelayedDestructionList := TList.Create;
  fTransactionList := TBoldDomainElementCollection.Create;
  fDerivedMembers := TList.Create;
{$IFNDEF NoAutoSubscription}
  fMembersReadDuringDerivation := TList.Create;
{$ENDIF}
  for I := 0 to SystemTypeInfo.TopSortedClasses.Count - 1 do
  begin
    ListTypeInfo := SystemTypeInfo.TopSortedClasses[i].ListTypeInfo;
    ListClass := TBoldObjectListClass(ListTypeInfo.ListClass);
    fClasses.InternalAddWithoutCloning(ListClass.InternalCreateClassList(self, ListTypeInfo));
  end;
  fClasses.MakeImmutable;
  fOptimisticLockHandler := TBoldOptimisticLockHandler.Create(self);
  fSystemPersistenceHandler := TBoldSystemPersistenceHandler.Create(self);
  fRegionFactory := RegionFactory;
  if assigned(fRegionFactory) then
    fRegionFactory.fSystem := Self;
  fRollbackArea := TBoldFreeStandingValueSpace.create;
  fValidValueArea := TBoldFreeStandingValueSpace.create;


{$IFDEF ACCESSSTATISTICS} //// START PATCH ACCESSSTATISTICS ////////////////////
  InitAccessStats;
{$ENDIF} //// END PATCH ACCESSSTATISTICS ///////////////////////////////////////
  BoldSystemActivated(self);
end;

function TBoldSystem.IsDerivingMembers: boolean;
begin
  result := fDerivedMembers.Count > 0;
end;

function TBoldSystem.CurrentDerivedMember: TBoldMember;
begin
  if IsDerivingMembers then
    result := fDerivedMembers[fDerivedMembers.count-1]
  else
    result := nil;
end;

function TBoldMember.GetPSStateIsInvalid: Boolean;
begin
  result := (StateAndFlagBank and BoldPersistenceStateMask) = (Cardinal(bvpsInvalid) shl BoldPSShift);
end;

function TBoldObject.GetBoldMemberCount: Integer;
begin
  Result := BoldClassTypeInfo.AllMembersCount;
end;

function TBoldObject.GetBoldMemberAssigned(Index: integer): Boolean;
begin
  if Cardinal(index) >= Cardinal(Length(fMemberArray)) then
    raise EBold.CreateFmt('%s.GetBoldMemberAssigned: Index out of range (%d but max is %d)', [ClassName, Index, BoldMemberCount-1]);
  result := assigned(fMemberArray[Index]);
end;

destructor TBoldSystem.Destroy;
var
  Traverser: TBoldLocatorListTraverser;
  i: integer;
  bo: TBoldObject;
  bm: TBoldMember;
  Locator: TBoldObjectLocator;
begin
  // CheckIntegrity;
  EnsureCanDestroy;
  IsDestroying := True;
  PrepareToDestroy;
  IsDefault := False;
  FreeAndNil(fEvaluator);
  FreeAndNil(fClasses);

  for i := 0 to length(fSystemProxyCache) -1 do
    fSystemProxyCache[TBoldDomainElementProxyMode(i)] := nil;

  Traverser := Locators.CreateTraverser;
  while Traverser.MoveNext do
  begin
    if assigned(Traverser.Locator.BoldObject) then
    begin
      bo := Traverser.Locator.BoldObject;
      for i := bo.BoldMemberCount - 1 downto 0 do
      begin
        if bo.BoldMemberAssigned[i] then
        begin
          bm := bo.BoldMembers[i];
          if bm.Derived and (not bm.BoldPersistenceStateIsInvalid) then
          begin
            bm.Invalidate;
            bo.GetBoldMemberDeriver(bm).MarkSubscriptionOutOfDate;
          end;
        end;
      end;
    end;
  end;
  Traverser.Free;

  if Assigned(fLocators) then
    for Locator in fLocators do
      if assigned(Locator.BoldObject) then
        Locator.UnloadBoldObject;

  FreeAndNil(fRollbackArea);
  FreeAndNil(fValidValueArea);
  FreeAndNil(fPersistenceControllerSubscriber);
  FreeAndNil(fLocators);
  FreeAndNil(fDirtyObjects);
  FreeAndNil(fDelayedDestructionList);
  FreeAndNil(fTransactionList);
  FreeAndNil(fOldValueHandler);
  FreeAndNil(fOptimisticLockHandler);
  FreeAndNil(fSystemPersistenceHandler);
  FreeAndNil(fUndoHandler);
  FreeAndNil(fDerivedMembers);
{$IFNDEF NoAutoSubscription}
  FreeAndNil(fMembersReadDuringDerivation);
{$ENDIF}
  BoldSystemDeActivated(self);
  inherited Destroy;
end;

class function TBoldSystem.DefaultSystem: TBoldSystem;
begin
  Result := G_DefaultBoldSystem
end;

function TBoldSystem.GetIsFetching: Boolean;
begin
  Result := fFetchNesting > 0;
end;

function TBoldSystem.GetClassByIndex(index: Integer): TBoldObjectList;
begin
  if Assigned(fClasses) then
  begin
    Result := TBoldObjectList(fClasses[index]);
    Assert(Result is TBoldObjectList);
  end
  else
    Result := nil;
end;

function TBoldSystem.CreateNewObjectByExpressionName(const ExpressionName: string; Persistent: Boolean = True): TBoldObject;
var
  ClassTypeInfo: TBoldClassTypeInfo;
begin
  ClassTypeInfo := BoldSystemTypeInfo.ClassTypeInfoByExpressionName[ExpressionName];
  if Assigned(ClassTypeInfo) then
    Result := TBoldObjectClass(ClassTypeInfo.ObjectClass).InternalCreateNewWithClassAndSystem(ClassTypeInfo, Self, Persistent)
  else
    raise EBold.CreateFmt(sNoSuchClass, [ClassName, 'CreateNewObjectByExpressionName', ExpressionName]); // do not localize
end;

function TBoldSystem.CreateNewObjectFromClassTypeInfo(aClassTypeInfo: TBoldClassTypeInfo; Persistent: Boolean = True): TBoldObject;
begin
  Assert(Assigned(aClassTypeInfo), 'aClassTypeInfo param is nil in CreateNewObjectFromClassTypeInfo.');
  Result := TBoldObjectClass(aClassTypeInfo.ObjectClass).InternalCreateNewWithClassAndSystem(aClassTypeInfo, Self, Persistent);
end;

function TBoldSystem.CreateExistingObjectByID(BoldObjectID: TBoldObjectId): TBoldObject;
var
  ClassTypeInfo: TBoldClassTypeInfo;
  Locator: TBoldObjectLocator;
begin
  ClassTypeInfo := BoldSystemTypeInfo.TopSortedClasses[BoldObjectID.TopSortedIndex];

  if not BoldObjectID.TopSortedIndexExact then
    if ClassTypeInfo.IsAbstract or not assigned(OnCreateApproximateObjectError) then
      raise EBold.CreateFmt(sCannotCreateInexact,
        [BoldObjectId.AsString, ClassTypeInfo.ModelName]);

  Locator := EnsuredLocatorByID[BoldObjectID];

  if not Locator.BoldObjectID.TopSortedIndexExact then
    Locators.UpdateID(Locator, BoldObjectID);
  if assigned(Locator.BoldObject) then
    Result := Locator.BoldObject
  else
    Result := TBoldObjectClass(ClassTypeInfo.ObjectClass).InternalCreateWithClassAndLocator(ClassTypeInfo, Locator);

  if not BoldObjectID.TopSortedIndexExact and
    assigned(OnCreateApproximateObjectError) then
    OnCreateApproximateObjectError(result);
end;

function TBoldSystem.FindClassByExpressionName(const ExpressionName: string): TBoldObjectList;
var
  ClassTypeInfo: TBoldClassTypeInfo;
begin
  ClassTypeInfo := BoldSystemTypeInfo.ClassTypeInfoByExpressionName[ExpressionName];
  if not Assigned(ClassTypeInfo) then
    Result := nil
  else
    Result := Classes[ClassTypeInfo.TopSortedIndex];
end;

function TBoldSystem.GetClassByExpressionName(const ExpressionName: string): TBoldObjectList;
begin
  Result := FindClassByExpressionName(ExpressionName);
  if not Assigned(Result) then
    raise EBold.CreateFmt(sNoSuchClass, [ClassName, 'GetClassByExpressionName', ExpressionName]); // do not localize
end;

function TBoldSystem.GetClassByObjectClass(
  AObjectClass: TBoldObjectClass): TBoldObjectList;
var
  ClassTypeInfo: TBoldClassTypeInfo;
begin
  ClassTypeInfo := BoldSystemTypeInfo.TopSortedClasses.ItemsByObjectClass[AObjectClass];
  if not Assigned(ClassTypeInfo) then
    raise EBold.CreateFmt('%s.GetClassByObjectClass: System has no class : %s', [ClassName, AObjectClass.ClassName]);
  Result := Classes[ClassTypeInfo.TopSortedIndex];
end;

procedure TBoldSystem.MarkObjectDirty(BoldObject: TBoldObject); {called by TBoldObject}
begin
  if IsFetching then
    raise EBold.CreateFmt('%s.MarkObjectDirty: Object %s modified while fetching, not allowed.', [ClassName, BoldObject.DebugInfo]);
  if Self.BoldPersistent and BoldObject.BoldPersistent then
  begin
    if not BoldObject.InDirtyList then
    begin
      fDirtyObjects.Add(BoldObject);
      BoldObject.InDirtyList := True;
    end;
    BoldObject.SendExtendedEvent(beObjectBecomingDirty, [BoldObject]);
    if not DirtyObjectsInvalid then
      SendExtendedEvent(beDirtyListInvalidOrItemDeleted, [BoldObject]);
    end;
end;

procedure TBoldSystem.MarkObjectClean(BoldObject: TBoldObject); {called by TBoldObject}
begin
  if BoldObject.InDirtyList and BoldObject.BoldPersistent then
    begin
      fDirtyObjects.Remove(BoldObject);
      BoldObject.InDirtyList := False;
      BoldObject.SendExtendedEvent(beObjectBecomingClean, [BoldObject]);
    if not DirtyObjectsInvalid then
      SendExtendedEvent(beDirtyListInvalidOrItemDeleted, [BoldObject]);
  end;
end;

procedure TBoldSystem.GetAllInClass(aList: TBoldObjectList; AClass: TBoldObjectClass);
var
  classTypeinfo: TBoldClassTypeInfo;
  ClassList: TBoldObjectList;
begin
  ClassTypeInfo := BoldSystemTypeInfo.TopSortedClasses.ItemsByObjectClass[AClass];
  if assigned(ClassTypeInfo) then
  begin
    ClassList := classes[ClassTypeInfo.TopSortedIndex];
    alist.AddList(ClassList);
  end
  else
    raise EBold.CreateFmt(sClassDoesNotBelongHere, [className, aClass.ClassName]);
end;

procedure TBoldSystem.GetAllWithCondition(aList: TBoldObjectList; Condition: TBoldCondition);
begin
  SystemPersistenceHandler.GetAllWithCondition(AList, Condition);
end;

procedure TBoldSystem.GetAllInClassWithSQL(aList: TBoldObjectList; AClass: TBoldObjectClass; WhereClause, OrderByClause: String; Params: TParams = nil; JoinInheritedTables: Boolean = true; MaxAnswers: integer = -1; Offset: integer = -1);
begin
  SystemPersistenceHandler.GetAllInClassWithSQL(AList, AClass, WhereClause, OrderByClause, Params, JoinInheritedTables, MAxAnswers, Offset);
end;

procedure TBoldSystem.GetAllInClassWithRawSQL(aList: TBoldObjectList; AClass: TBoldObjectClass; SQL: String; Params: TParams = nil; MaxAnswers: integer = -1; Offset: integer = -1);
begin
  SystemPersistenceHandler.GetAllInClassWithRawSQL(AList, AClass, SQL, Params, MaxAnswers, Offset);
end;

procedure TBoldSystem.FetchLinksWithObjects(ObjectList: TBoldObjectList; const LinkName: string;FetchObjectsInLink: Boolean = True{; const FetchedObjectList: TBoldObjectList = nil});
begin
  SystemPersistenceHandler.FetchLinksWithObjects(ObjectList, LinkName,FetchObjectsInLink{, FetchedObjectList});
end;

procedure TBoldSystem.FetchMembersWithObjects(aBoldObjectList: TBoldObjectList; aBoldMemberIdList: TBoldMemberIdList);
begin
  SystemPersistenceHandler.FetchMembersWithObjects(aBoldObjectList, aBoldMemberIdList);
end;

procedure TBoldSystem.FetchMembersWithObjects(aBoldObjectList: TBoldObjectList; const AMemberCommaList: string);
begin
  SystemPersistenceHandler.FetchMembersWithObjects(aBoldObjectList, AMemberCommaList);
end;

procedure TBoldSystem.FetchMembersWithObject(ABoldObject: TBoldObject; const AMemberCommaList: string);
var
  BoldObjectList: TBoldObjectList;
begin
  BoldObjectList := TBoldObjectList.Create;
  try
    BoldObjectList.Add(ABoldObject);
    FetchMembersWithObjects(BoldObjectList, AMemberCommaList);
  finally
    BoldObjectList.free;
  end;
end;

procedure TBoldSystem.FetchIdList(FetchIdList: TBoldObjectIdList; AFetchObjects: boolean = true);
var
  lObjectList: TBoldObjectList;
begin
  if FetchIdList.Count > 0 then
  begin
    lObjectList := TBoldObjectList.Create;
    try
      lObjectList.FillFromIDList(FetchIdList, self);
      if AFetchObjects then
        SystemPersistenceHandler.FetchList(lObjectList);
    finally
      lObjectList.free;
    end;
  end;
end;

function TBoldList.GetEmpty: Boolean;
begin
  result := count = 0;
end;

procedure TBoldSystem.DirtyAsObjectList(ObjectList: TBoldObjectList);
var
  i: Integer;
begin
  if ObjectList.BeginUpdate then
  try
    StartTransaction;
    try
      if ObjectList.Empty or (ObjectList.DuplicateMode = bldmAllow) then
        with ObjectList.ObjectListController do
          for I := 0 to DirtyObjects.Count - 1 do
            AddLocator(TBoldObject(DirtyObjects[i]).BoldObjectLocator)
      else
        for I := 0 to DirtyObjects.Count - 1 do
          ObjectList.AddLocator(TBoldObject(DirtyObjects[i]).BoldObjectLocator);
      CommitTransaction;
    except
      RollbackTransaction;
      raise;
    end;
  finally
    ObjectList.EndUpdate;
  end;
end;

procedure TBoldSystem.UpdateDatabase;
var
  g: IBoldGuard;
  List: TBoldObjectList;
begin
  if not BoldDirty then
    exit;
  if IsUpdatingDatabase then
    raise EBold.CreateFmt(sUpdateDbRentry, []);
  g := TBoldGuard.Create(List);
  List := TBoldObjectList.Create;// BoldSystemTypeInfo.RootClassTypeInfo.ListTypeInfo.CreateElement as TBoldObjectList;
  list.SetInternalState(BoldDuplicateModeMask, BoldDMShift, Integer(bldmAllow));
  list.SubscribeToObjectsInList := false;
  list.Capacity := DirtyObjects.Count;
  DirtyAsObjectList(list);
  UpdateDatabaseWithList(List);
end;

procedure TBoldSystem.UpdateDatabaseWithList(ObjectList: TBoldObjectList);
begin
  if IsUpdatingDatabase then
    raise EBold.CreateFmt(sUpdateDbRentry, []);
  if BoldSystemCount > 1 then
  begin
    for var BO in ObjectList do
      if BO.BoldSystem <> self then
        raise EBold.CreateFmt(sObjectFromAnotherSystem, [classname, 'UpdateDatabaseWithList']);
  end;
  SystemPersistenceHandler.UpdateDatabaseWithList(ObjectList);
  if (DirtyObjects.Count > 0) then
    fOldValueHandler.PurgeEqualValues
  else
  begin
//    fOldValueHandler.PurgeEqualValues;
//    if not fOldValueHandler.IsEmpty then
//      Assert(fOldValueHandler.IsEmpty);
// optimization: if there are no dirty objects do not PurgeEqualValue, just destroy OldValues instead.
    FreeAndNil(fOldValueHandler);
    fOldValueHandler := TBoldOldValueHandler.Create(Self);
  end;
end;

procedure TBoldSystem.UpdateDatabaseWithObjects(
  const aObjects: array of TBoldObject);
var
  i: integer;
  List: TBoldObjectList;
begin
  if high(aObjects) = -1 then exit;
  List := TBoldObjectList.Create;
  list.SubscribeToObjectsInList := false;
  try
    for i := low(aObjects) to high(aObjects) do
      if aObjects[i] <> nil then
        List.Add(aObjects[i]);
    UpdateDatabaseWithList(List);
  finally
    List.Free;
  end;
end;

procedure TBoldSystem.DiscardPersistent(ADiscardTransientLinks: boolean);
var
  LocalDirtyObjects: TList;
  i: integer;
begin
  DelayObjectDestruction;
  try
    while DirtyObjects.Count > 0 do
    begin
      LocalDirtyObjects := DirtyObjects;
      i := LocalDirtyObjects.Count - 1;
      repeat
        i := MinIntValue([i, LocalDirtyObjects.Count - 1]);
        TBoldObject(LocalDirtyObjects[i]).BoldObjectLocator.DiscardBoldObject(ADiscardTransientLinks);
        dec(i);
        while i >= LocalDirtyObjects.Count do
          dec(i);
      until (i < 0) or (LocalDirtyObjects.Count = 0);
    end;
  finally
    AllowObjectDestruction;
  end;
end;

procedure TBoldSystem.DiscardTransient;
var
  g: IBoldGuard;
  Traverser: TBoldLocatorListTraverser;
  Locator: TBoldObjectLocator;
begin
  g := TBoldGuard.Create(Traverser);
  Traverser := Locators.CreateTraverser;
//  DelayObjectDestruction;
  try
    while Traverser.MoveNext do
    begin
      Locator := Traverser.Locator;
      if assigned(Traverser.Locator.BoldObject) and not Locator.BoldObject.BoldPersistent then
        Locator.DiscardBoldObject;
    end;
  finally
//    AllowObjectDestruction;
  end;
end;

procedure TBoldSystem.Discard;
begin
  DiscardPersistent;
  DiscardTransient;
  if assigned(PessimisticLockHandler) then
    PessimisticLockHandler.ReleaseUnneededRegions;
  if UndoHandlerInterface.Enabled then
    UndoHandlerInterface.ClearAllUndoBlocks;
end;

procedure TBoldSystem.DefaultSubscribe(Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent);
begin
end;

procedure TBoldSystem.GetAsList(ResultList: TBoldIndirectElement);
begin
  ResultList.SetReferenceValue(fClasses);
end;

function TBoldSystem.EnsureEnclosure(ObjectList: TBoldObjectList; ValidateOnly: Boolean): Boolean;
begin
  Result := SystemPersistenceHandler.EnsureEnclosure(ObjectList, ValidateOnly);
end;

function TBoldSystem.GetDirtyObjects: TList;
var
  i: Integer;
begin
  if DirtyObjectsInvalid then
  begin
    i := fDirtyObjects.Count-1;
    while (i >= 0) and (i < fDirtyObjects.Count) do
    begin
      if assigned(fDirtyObjects[i]) and not TBoldObject(fDirtyObjects[i]).BoldDirty then
        MarkObjectClean(TBoldObject(fDirtyObjects[i]));
      dec(i);
    end;
    fDirtyObjects.Pack;
    DirtyObjectsInvalid := False;
  end;
  result := fDirtyObjects;
end;

function TBoldSystem.GetDirtyObjectsAsBoldList(AClassType: TBoldObjectClass): TBoldObjectList;
var
  i: integer;
begin
  if not Assigned(AClassType) then
    AClassType := TBoldObjectClass(BoldSystemTypeInfo.RootClassTypeInfo.ObjectClass);
  result := TBoldObjectList.CreateWithTypeInfo(BoldSystemTypeInfo.ClassTypeInfoByClass[AClassType]);
  result.DuplicateMode := bldmAllow;
  result.SubscribeToObjectsInList := false;
  with result.ObjectListController, GetDirtyObjects do
    for I := 0 to Count - 1 do
      if TBoldObject(Items[i]) is AClassType then
        AddLocator(TBoldObject(Items[i]).BoldObjectLocator);
  result.DuplicateMode := bldmMerge;
end;

function TBoldSystem.GetDirtyObjectsAsBoldList(AClassType: TBoldClassTypeInfo): TBoldObjectList;
var
  i: integer;
begin
  if not Assigned(AClassType) then
    AClassType := BoldSystemTypeInfo.RootClassTypeInfo;
  result := TBoldObjectList.CreateWithTypeInfo(AClassType);
  result.DuplicateMode := bldmAllow;
  result.SubscribeToObjectsInList := false;
  with result.ObjectListController, GetDirtyObjects do
    for I := 0 to Count - 1 do
      if TBoldObject(Items[i]).BoldType = AClassType then
        AddLocator(TBoldObject(Items[i]).BoldObjectLocator);
  result.DuplicateMode := bldmMerge;
end;

function TBoldSystem.GetDirtyObjectsAsBoldListByClassExpressionName(
  const AClass: string): TBoldObjectList;
var
  ClassTypeInfo: TBoldClassTypeInfo;
begin
  result := nil;
  ClassTypeInfo := BoldSystemTypeInfo.ClassTypeInfoByExpressionName[AClass];
  if Assigned(ClassTypeInfo) then
    result := GetDirtyObjectsAsBoldList(ClassTypeInfo);
end;

function TBoldSystem.GetAllDirtyObjectsAsBoldList: TBoldObjectList;
begin
  result := GetDirtyObjectsAsBoldList(nil);
end;

procedure TBoldSystem.MarkObjectPossiblyCleaner(BoldObject: TBoldObject);
begin
  if not DirtyObjectsInvalid then
  begin
    DirtyObjectsInvalid := True;
    SendExtendedEvent(beDirtyListInvalidOrItemDeleted, [BoldObject]);
  end;
end;

procedure TBoldSystem.MemberDerivationBegin(Member: TBoldMember);
begin
  fDerivedMembers.Add(Member);
{$IFNDEF NoAutoSubscription}
  if fDerivedMembers.count > Length(fMembersReadDuringDerivationIndexArray) then
    SetLength(fMembersReadDuringDerivationIndexArray, fDerivedMembers.count);
  fMembersReadDuringDerivationIndexArray[fDerivedMembers.count-1] := fMembersReadDuringDerivation.Count;
{$ENDIF}
{$IFDEF SendDerivationEvents}
  SendExtendedEvent(beDerivationBegin, [Member]);
{$ENDIF}
end;

procedure TBoldSystem.MemberDerivationEnd(Member: TBoldMember);
{$IFNDEF NoAutoSubscription}
var
  vAccessedMember: TBoldMember;
  i: Integer;
{$ENDIF}
begin
{$IFNDEF NoAutoSubscription}
  for i := fMembersReadDuringDerivation.Count - 1 downto fMembersReadDuringDerivationIndexArray[fDerivedMembers.count-1] do
  begin
    vAccessedMember := TBoldMember(fMembersReadDuringDerivation[i]);
    if Assigned(vAccessedMember) then
      vAccessedMember.DefaultSubscribe(Member.Deriver, beDefaultRequestedEvent);  // place the subscription
    fMembersReadDuringDerivation.Delete(i);
  end;
{$ENDIF}
  fDerivedMembers.Remove(Member);
{$IFDEF SendDerivationEvents}
  SendExtendedEvent(beDerivationEnd, [Member]);
{$ENDIF}
end;

function TBoldSystem.GetBoldDirty: Boolean;
begin
  result := DirtyObjects.Count > 0;
end;

function TBoldSystem.GetStringRepresentation(
  Representation: TBoldRepresentation): string;
begin
//  if Representation = brJson then
//    Result := TBoldJSONwriter.BoldElementToJsonString(Self)
//  else
  result := BoldSystemTypeInfo.ExpressionName;
end;

procedure TBoldSystem.AllowObjectDestruction;
var
  i: Integer;
  anObject: TBoldObject;
  aList: TList;
begin
  if fDelayedDestructionCount <= 0 then
    raise EBold.CreateFmt(sDestructionNestingMismatch, [classname]);
  dec(fDelayedDestructionCount);
  if fDelayedDestructionCount = 0 then
  begin
    while fDelayedDestructionList.Count > 0 do begin
      aList := TList.Create;
      try
        aList.Assign(fDelayedDestructionList);
        fDelayedDestructionList.count := 0;
        for i := aList.Count - 1 downto 0 do begin
          Assert(TObject(aList[i]) is TBoldObject);
          anObject := TBoldObject(aList[i]);
          if (anObject.BoldPersistenceState <> bvpsModified) and
             (anObject.BoldExistenceState <> besExisting) then
          begin
            DestroyObject(anObject);
          end;
        end;
      finally
        aList.Free;
      end;
    end;
  end;
end;

procedure TBoldSystem.DelayObjectDestruction;
begin
  inc(fDelayedDestructionCount);
end;

procedure TBoldSystem.DestroyObject(BoldObject: TBoldObject);
var
  aLocator: TBoldObjectLocator;
begin
  if not Assigned(BoldObject) then
    Exit;
  if (BoldObject.BoldPersistenceState = bvpsModified) or
     (BoldObject.BoldExistenceState = besExisting) then
    raise EBold.CreateFmt(sObjectNotDestroyable, [classname]);

  BoldObject.FreeDerivers;
  if (fDelayedDestructionCount > 0) then
  begin
    if not BoldObject.InDelayDestructionList then
    begin
      fDelayedDestructionList.Add(BoldObject);
      BoldObject.InDelayDestructionList := true;
    end;
  end
  else
  begin
    aLocator := BoldObject.BoldObjectLocator;
    // If we got an exception duriung destoy, we might get an half uninitialized BoldObject without Locator next time.
    if Assigned(aLocator) then //PATCH
    begin
      TBoldClassListController(Classes[aLocator.BoldClassTypeInfo.TopSortedIndex].ObjectListController).ReceiveClassEvent(BoldObject, beLocatorDestroying, [aLocator]);
      SendExtendedEvent(beLocatorDestroying, [aLocator]);
      aLocator.UnloadBoldObject;
      Locators.Remove(aLocator);
    end;
  end;
end;

function TBoldObjectList.GetObjectListController: TBoldAbstractObjectListController;
begin
  Result := TBoldAbstractObjectListController(ListController);
  Assert(result is TBoldAbstractObjectListController);
end;

procedure TBoldSystem.ReceiveEventFromOwned(originator: TObject;
  originalEvent: TBoldEvent; const Args: array of const);
var
  ClassList: TBoldObjectList;
begin
  if Assigned(fClasses) then
  begin
    ClassList := nil;
    if (originator is TBoldMember) then
    begin
      if not Assigned(TBoldMember(Originator).OwningObject) then
        exit;
      ClassList := Classes[TBoldMember(Originator).OwningObject.BoldClassTypeInfo.TopSortedIndex];
    end
    else
    if (originator is TBoldObject) then
      ClassList := Classes[TBoldObject(Originator).BoldClassTypeInfo.TopSortedIndex];
    TBoldClassListController(ClassList.ObjectListController).ReceiveClassEvent(Originator as TBoldDomainElement, OriginalEvent, Args);
    if (originalEvent in beBroadcastMemberEvents) and ((Originator is TBoldObject) or (Originator is TBoldMember) and (TBoldMember(Originator).OwnedByObject)) then
      Publisher.SendExtendedEvent(Originator, originalEvent, Args);
  end;
end;

procedure TBoldSystem.ReceiveFromPersistenceController(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  case OriginalEvent of
    beDestroying: fPersistenceController := nil;
    bpeStartFetch: Inc(fFetchNesting);
    bpeEndFetch:
    begin
      Dec(fFetchNesting);
      Assert(fFetchNesting >= 0, 'Negative fetch nesting')
    end
    else
      raise Exception.CreateFmt('Unexpected event %d in TBoldSystem.ReceiveFromPersistenceController', [OriginalEvent]);
  end;
end;

function TBoldSystem.ReceiveQueryFromOwned(Originator: TObject;
  OriginalEvent: TBoldEvent; const Args: array of const;
  Subscriber: TBoldSubscriber): Boolean;
begin
  result := SendQuery(OriginalEvent, Args, Subscriber, Originator);
end;

function TBoldSystem.CanCreateObject(ClassTypeInfo: TBoldClassTypeInfo): boolean;
begin
{$IFDEF BOLD_NO_QUERIES}
  result := true;
{$ELSE}
  result := SendQuery(bqMayCreateObject, [ClassTypeInfo], nil);
{$ENDIF}
end;

function TBoldMember.GetIsPartOfSystem: Boolean;
begin
  result := OwnedByObject or (OwningElement is TBoldSystem);
end;

function TBoldMember.GetBoldMemberRTInfo: TBoldMemberRTInfo;
begin
  if OwnedByObject then
  begin
    assert(fBoldMetaType is TBoldMemberRtInfo);
    result := TBoldMemberRTInfo(fBoldMetaType);
  end
  else
    result := nil;
end;

function TBoldMember.GetIsReadOnly(Flag: TBoldElementFlag): Boolean;
begin
  result := GetElementFlag(Flag) or
    (assigned(BoldMemberRTInfo) and BoldMemberRTInfo.IsStoredInObject and OwningObject.IsReadOnly);
end;

function TBoldMember.GetOwningObject: TBoldObject;
begin
  if OwnedByObject then
  begin
    Result := TBoldObject(OwningElement);
    Assert(result is TBoldObject);
  end
  else
  begin
    Assert(not (OwningElement is TBoldObject));
    Result := nil;
  end;
end;

function TBoldMember.GetBoldSystem: TBoldSystem;
begin
  if OwnedByObject then
  begin
    Assert(OwningElement is TBoldObject);
    Result := TBoldObject(OwningElement).BoldSystem
  end
  else
  begin
    result := OwningElement as TBoldSystem;
  end;
end;

function TBoldSystem.CanDeleteObject(anObject: TBoldObject): boolean;
begin
{$IFDEF BOLD_NO_QUERIES}
  result := true;
{$ELSE}
  result := SendQuery(bqMayDeleteObject, [AnObject], nil);
{$ENDIF}
end;

function TBoldSystem.GetBoldType: TBoldElementTypeInfo;
begin
  result := BoldSystemTypeInfo;
end;

function TBoldSystem.GetEvaluator: TBoldEvaluator;
begin
  if not assigned(fEvaluator) then
    fEvaluator := TBoldOcl.Create(BoldSystemTypeINfo, self);
  result := fEvaluator;
end;

procedure TBoldSystem.MakeDefault;
begin
  G_DefaultBoldSystem := Self;
end;

function TBoldSystem.GetIsDefault: Boolean;
begin
  Result := Self = G_DefaultBoldSystem;
end;

function TBoldSystem.GetIsProcessingTransactionOrUpdatingDatabase: Boolean;
begin
  Result := IsCommitting or IsRollingBack or IsUpdatingDatabase;
end;

procedure TBoldSystem.SetIsDefault(Value: Boolean);
begin
  if Value then
  begin
    G_DefaultBoldSystem := Self
  end
  else
  if (self = G_DefaultBoldSystem) then
    G_DefaultBoldSystem := nil;
end;

function TBoldSystem.InTransaction: boolean;
begin
  Result := fTransactionNesting <> 0;
end;

procedure TBoldSystem.StartTransaction(MinimalMode: TBoldSystemTransactionMode = stmNormal);
begin
{$IFNDEF NoObjectSpaceTransactions}
  if MinimalMode <= TransactionMode then
  begin
    if not InTransaction then
    begin
      Assert(not RollbackAreaAssigned);
      RollbackAreaAssigned := true;
      DelayObjectDestruction;
    end;
    inc(fTransactionNesting);
  end;
{$ENDIF}
end;

function TBoldSystem.CreateTransactionHandler(MinimalMode: TBoldSystemTransactionMode = stmNormal): IBoldTransactionHandler;
begin
  result := TBoldTransactionHandler.Create(self);
end;

procedure TBoldSystem.CommitTransaction(MinimalMode: TBoldSystemTransactionMode = stmNormal);

  function OrdernoDiffers(const Value: IBoldValue; member: TBoldMember):Boolean;
  begin
    Result := (member is TBoldObjectReference) and
      (TBoldObjectReference(member).GetController is TBoldDirectSingleLinkController) and
           ((Value as IBoldObjectIdRef).OrderNo <>  TBoldDirectSingleLinkController(TBoldObjectReference(member).GetController).OrderNo);
  end;

  procedure HandleOldValues;
  var
    O, M: integer;
    ObjectIDs: TBoldObjectIdList;
    G: IBoldGuard;
    ObjectContents: TBoldFreeStandingObjectContents;
    Value: IBoldValue;
    BoldObject: TBoldObject;
    BoldMember: TBoldMember;
    RegardAsExisting: Boolean;
    ObjIdRef: IBoldObjectIdRef;
  begin
    if FRollBackArea.IsEmpty then
      exit;
    G := TBoldGuard.Create(ObjectIds);
    ObjectIds := TBoldObjectIdList.Create;
    FRollBackArea.AllObjectIds(ObjectIDs, false);
    for O := 0 to ObjectIds.Count - 1 do
    begin
      ObjectContents := FRollBackArea.GetFSObjectContentsByObjectId(ObjectIds[O]);
      BoldObject := Locators.GetObjectByID(ObjectIds[O]);
      assert(assigned(BoldObject));
      RegardAsExisting := (((BoldObject.BoldPersistenceState = bvpsCurrent) and
        (BoldObject.BoldExistenceState = besExisting)) or
         ((BoldObject.BoldPersistenceState = bvpsModified) and
        (BoldObject.BoldExistenceState = besDeleted)))
         and
        (ObjectContents.BoldExistenceState = besNotCreated);
      Assert(not RegardAsExisting);
      UndoHandler.HandleObject(ObjectContents, RegardAsExisting);
      for M := ObjectContents.MemberCount - 1 downto 0 do
      begin
        Value := ObjectContents.ValueByIndex[m];
        if Assigned(Value) and BoldObject.BoldMemberAssigned[M] and BoldObject.BoldMembers[M].StoreInUndo then
        begin
          BoldMember := BoldObject.BoldMembers[M];
          if (Value.BoldPersistenceState = bvpsInvalid) then
            if not ((Value.QueryInterface(IBoldObjectIdRef, ObjIdRef) = S_OK) and (ObjIdRef.Id <> nil)) then
              Value := GetValueFromValuespace(fValidValueArea, ObjectIds[O], M);
          if Assigned(Value) and (not BoldMember.IsEqualToValue(Value) or OrdernoDiffers(Value, BoldMember)) then
            UndoHandler.HandleMember(ObjectContents, M, Value);
          Value := nil;
        end;
      end;
      if (BoldObject.BoldExistenceState = besDeleted)  then
      begin
        for M := 0 to BoldObject.BoldMemberCount - 1 do
          if BoldObject.BoldMemberAssigned[M] and BoldObject.BoldMembers[M].StoreInUndo  then
          begin
            Value := BoldObject.BoldMembers[M].AsIBoldValue[bdepContents];
            if (not BoldObject.BoldMembers[M].BoldPersistenceStateIsInvalid) then
              UndoHandler.HandleMember(ObjectContents, M, Value);
            Value := nil;
          end;
      end;
    end;
  end;

begin
{$IFNDEF NoObjectSpaceTransactions}
  if MinimalMode <= TransactionMode then
  begin
    if not InTransaction then
      raise EBold.CreateFmt(sUnmatchedCommit, [classname]);

    if fTransactionNesting = 1 then
    begin
      fTransactionRollbackOnly := fTransactionRollbackOnly or not CanCommit;
      fTransactionList.Clear;
      if not fTransactionRollbackOnly then
      begin
        Dec(fTransactionNesting); //PATCH If we get to this point we MUST decrease the counter to zero so we don't get stuck forever.
        IsCommitting := True;
        try
          if UndoHandlerInterface.Enabled and (TBoldUndoHandler(UndoHandler).UndoState=busNormal) then
            HandleOldValues;
        finally //PATCH To strengten Transaction against exceptions
          try  //PATCH To strengten Transaction against exceptions
            fRollbackArea.Clear;
          finally
            try
              fValidValueArea.Clear;
            finally
              RollbackAreaAssigned := false;
              AllowObjectDestruction;  //PATCH Make sure this is called even if we get an exception when destroying valuespaces.
              IsCommitting := false;
            end;
          end;
        end;
      end
      else
        BoldRaiseLastFailure(self, 'CommitTransaction',  sCommitNotAllowed); // do not localize
    end
    else
      dec(fTransactionNesting);
  end;
{$ENDIF}
end;

procedure TBoldSystem.RollbackTransaction(MinimalMode: TBoldSystemTransactionMode = stmNormal);
begin
{$IFNDEF NoObjectSpaceTransactions}
  if MinimalMode <= TransactionMode then
  begin
    if not InTransaction then
      raise EBold.CreateFmt(sUnmatchedRollback, [classname]);

    if fTransactionNesting = 1 then
    begin
      IsRollingBack := True;
      try
        fTransactionRollbackOnly := True; //PATCH
        RollbackAreaAssigned := false;
        AsIBoldvalueSpace[bdepContents].ApplyValueSpace(fRollbackArea, false);
      finally
        dec(fTransactionNesting);
        IsRollingBack := False;
        fRollbackArea.clear;
        fValidValueArea.clear;
        AllowObjectDestruction;
        fTransactionList.Clear;
        fTransactionRollbackOnly := false;
        SendEvent(beRolledBack);
      end;
    end
    else
      dec(fTransactionNesting);
  end;
{$ENDIF}
end;

{$IFNDEF NoObjectSpaceTransactions}
function TBoldSystem.CanCommit: Boolean;
{$IFNDEF BOLD_NO_QUERIES}
var
  g: IBoldGuard;
  Traverser: TBoldDomainElementCollectionTraverser;
{$ENDIF}
begin
{$IFNDEF BOLD_NO_QUERIES}
  g := TBoldGuard.Create(Traverser);
  try
    result := SendQuery(bqMayCommit, [], nil);
    Traverser := fTransactionList.CreateTraverser;
    while result and Traverser.MoveNext do
    begin
      result := Traverser.item.CanCommit;
    end;
  except
    result := false;
  end;
{$ELSE}
  result := true;
{$ENDIF}
end;
{$ENDIF}

function TBoldSystemFreeStandingObjectContents.EnsureMemberAndGetValueByIndex(
  Member: TBoldMember): IBoldValue;

  function CreateInstance(
    Member: TBoldMember): TBoldFreeStandingValue;
  var
    ElementClass: TBoldFreeStandingElementClass;
  begin
    ElementClass := Member.GetFreeStandingClass;
    if Assigned(ElementClass) then
      result := ElementClass.Create as TBoldFreeStandingValue
    else
      raise EBold.createFmt('%s.CreateInstance: No freestanding class registered for name %s', [classname, Member.DisplayName]);
  end;

var
  Index: integer;
begin
  Index := Member.BoldMemberRTInfo.index;
  EnsureMemberListLength(Index);
  if not assigned(fMemberlist[Index]) then
    fMemberlist[Index] := CreateInstance(Member);
  Result :=  fMemberlist[Index];
end;

class function TBoldAbstractOldValueHandler.NewValueInValueSpace(BoldMember: TBoldMember; const ValueSpace: IBoldValueSpace): IBoldValue;
var
  ObjectContents: IBoldObjectContents;
  MemberIndex: Integer;
  BoldSystemObjectContents: IBoldSystemObjectContents;
begin
  ObjectContents := ValueSpace.EnsuredObjectContentsByObjectId[BoldMember.OwningObject.BoldObjectLocator.BoldObjectId];
  MemberIndex := BoldMember.BoldMemberRTInfo.Index;
  if assigned(ObjectContents.ValueByIndex[MemberIndex]) then
    result := nil
  else
  begin
    if Supports(ObjectContents, IBoldSystemObjectContents, BoldSystemObjectContents) then
      result := BoldSystemObjectContents.EnsureMemberAndGetValueByIndex(BoldMember)
    else
      result := ObjectContents.EnsureMemberAndGetValueByIndex(MemberIndex, BoldMember.GetStreamName);
  end;
end;

class procedure TBoldAbstractOldValueHandler.CopyMemberToValueSpace(BoldMember: TBoldMember; const ValueSpace: IBoldValueSpace);

  function GetMemberFromContents(const ObjectContents: IBoldObjectContents): IBoldValue;
  var
    BoldSystemObjectContents: IBoldSystemObjectContents;
  begin
    if Supports(ObjectContents, IBoldSystemObjectContents, BoldSystemObjectContents) then
      result := BoldSystemObjectContents.EnsureMemberAndGetValueByIndex(BoldMember)
    else
      result := ObjectContents.EnsureMemberAndGetValueByIndex(BoldMember.BoldMemberRTInfo.Index, BoldMember.GetStreamName);
  end;
var
  ObjectContents: IBoldObjectContents;
  Member: IBoldValue;
begin
  Assert(Assigned(Valuespace));
  Assert(Assigned(BoldMember));
  Assert(Assigned(BoldMember.OwningObject));
  Member := nil;
  if ValueSpace.GetEnsuredObjectContentsByObjectIdAndCheckIfCreated(BoldMember.OwningObject.BoldObjectLocator.BoldObjectId, ObjectContents) then
  with BoldMember.OwningObject do
  begin
    ObjectContents.BoldExistenceState := BoldExistenceState;
    ObjectContents.BoldPersistenceState := BoldPersistenceState;
    ObjectContents.TimeStamp := GetTimeStamp;
    Member := GetMemberFromContents(ObjectContents);
  end
  else
  if not assigned(ObjectContents.ValueByIndex[BoldMember.BoldMemberRTInfo.Index]) then
  begin
    Member := GetMemberFromContents(ObjectContents);
  end;
  if Assigned(Member) then
    Member.AssignContent(BoldMember.AsIBoldValue[bdepContents]);
end;

class procedure TBoldAbstractOldValueHandler.CopyObjectToValueSpace(BoldObject: TBoldObject; const ValueSpace: IBoldValueSpace);
var
  aBoldObjectContents: IBoldObjectContents;
begin
  Assert(Assigned(ValueSpace));
  if ValueSpace.GetEnsuredObjectContentsByObjectIdAndCheckIfCreated(BoldObject.BoldObjectLocator.BoldObjectId, aBoldObjectContents) then
  begin
    aBoldObjectContents.BoldExistenceState := BoldObject.BoldExistenceState;
    aBoldObjectContents.BoldPersistenceState := BoldObject.BoldPersistenceState;
  end;
end;

{$IFNDEF NoObjectSpaceTransactions}
procedure TBoldSystem.CopyMemberToRollBackBuffer(BoldMember: TBoldMember);
var
  Value: IBoldValue;
  FSObjectContents: TBoldFreeStandingObjectContents;
  Created: boolean;
begin
  if RollBackAreaAssigned then
  begin
    FSObjectContents := EnsureObjectInFsValueSpace(BoldMember.OwningObject, fRollbackArea, Created);
    if Created then
      Value := nil
    else
      Value := FSObjectContents.ValueByIndex[BoldMember.BoldMemberRTInfo.Index];
    if Assigned(Value) then
    begin
      if (Value.BoldPersistenceState = bvpsInvalid) then
      begin
        FSObjectContents := EnsureObjectInFsValueSpace(BoldMember.OwningObject, fValidValueArea, Created);
        TBoldSystemFreeStandingObjectContents(FSObjectContents).EnsureMemberAndGetValueByIndex(BoldMember).AssignContent(BoldMember.AsIBoldValue[bdepContents]);;
      end;
    end
    else
    begin
      Assert (not(
              (BoldMember is TBOldObjectReference) and
              (BoldMember.BoldPersistenceStateIsInvalid) and
              TBOldObjectReference(BoldMember).HasOldValues and
              ((TBOldObjectReference(BoldMember).fObjectReferenceController as TBoldDirectSingleLinkController).GetLocator = nil)
              ));
              Value := BoldMember.AsIBoldValue[bdepContents];
              TBoldSystemFreeStandingObjectContents(FSObjectContents).EnsureMemberAndGetValueByIndex(BoldMember).AssignContent(Value);
    end;
  end;
end;
{$ENDIF}

procedure TBoldSystem.CopyObjectToRollBackBuffer(BoldObject: TBoldObject);
begin
  if RollBackAreaAssigned and not IsFetching then
    EnsureObjectInFsValueSpace(BoldObject, fRollBackArea);
end;


procedure TBoldSystem.AddToTransaction(DomainElement: TBoldDomainElement);
begin
  if RollbackAreaAssigned and not fTransactionList.Includes(DomainElement) then
    fTransactionList.Add(DomainElement);
end;

function TBoldSystem.TryCommitTransaction(MinimalMode: TBoldSystemTransactionMode = stmNormal): Boolean;
begin
  try
    CommitTransaction(MinimalMode);
    result := true;
  except
    RollbackTransaction(MinimalMode);
    result := false;
  end;
end;

function TBoldSystem.ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean;
begin
  if IsEqualGuid(IID, IBoldValueSpace) then
  begin
    result := TBoldSystem_Proxy.Create(self, Mode).GetInterface(IID, obj);
    if not result then
      raise EBoldInternal.CreateFmt('ProxyClass for %s did not implement IBoldValueSpace', [ClassName]);
  end
  else
    result := inherited ProxyInterface(IID, Mode, Obj);
end;

procedure TBoldSystem.CheckIntegrity;
var
  Traverser: TBoldLocatorListTraverser;
  i: integer;
  bo: TBoldObject;
  bm: TBoldMember;
begin
  Traverser := Locators.CreateTraverser;
  while Traverser.MoveNext do
  begin
    if assigned(Traverser.Locator.BoldObject) then
    begin

      if not (Traverser.Locator.BoldObject is TBoldObject) then
        raise Exception.Create('broken object');
      bo := Traverser.Locator.BoldObject;
      for i := 0 to bo.BoldMemberCount - 1 do
      begin

        if bo.BoldMemberAssigned[i] then
        begin

          bm := bo.BoldMembers[i];
          if not (bm is TBoldMember) then
            raise Exception.Create('broken member');
        end;
      end;
    end;
  end;
  Traverser.Free;

end;

procedure TBoldSystem.IncrementDeletingObjectsDepth;
begin
  Inc(FDeletingObjectsDepth);
  if FDeletingObjectsDepth = 1 then begin
//    SendEvent(beStartObjectDeletion);
  end;
end;

procedure TBoldSystem.DecrementDeletingObjectsDepth;
begin
  if FDeletingObjectsDepth = 1 then begin
//    SendEvent(beEndObjectDeletion);
  end;
  Dec(FDeletingObjectsDepth);
end;

{ EnsureCanDestroy will raise an exception if there are constraints
  that prohibits a destruction. }
procedure TBoldSystem.EnsureCanDestroy;
begin
  if InTransaction then
    raise EBold.CreateFmt(sDestroy_TransactionNesting, [ClassName, fTransactionNesting, BoldSystemTypeInfo.Modelname]);
  if RollbackAreaAssigned then
    raise EBold.CreateFmt(sDestroy_RollbackAreaAssigned, [ClassName, BoldSystemTypeInfo.Modelname]);
  if BoldDirty then
    raise EBold.CreateFmt(sDestroy_DirtyObjects, [ClassName, BoldSystemTypeInfo.Modelname]);
end;

function TBoldSystem.GetTimeForTimestamp(Timestamp: TBoldTimestampType): TDateTime;
begin
  Result := SystemPersistenceHandler.GetTimeForTimestamp(Timestamp);
end;

function TBoldSystem.GetTimestampForTime(ClockTime: TDateTime): TBoldTimestampType;
begin
   Result := SystemPersistenceHandler.GetTimestampForTime(ClockTime);
end;

procedure TBoldSystem.SetTransactionMode(const Value: TBoldSystemTransactionMode);
begin
  if value <> fTransactionMode then
  begin
    if InTransaction then
      raise EBold.CreateFmt(sNotAllowedInTransaction, [classname]);
    fTransactionMode := Value;
  end;
end;

procedure TBoldSystem.SetPessimisticLockHandler(const Value: TBoldAbstractPessimisticLockHandler);
begin
  if assigned(fPessimisticLockHandler) then
    raise EBold.CreateFmt(sCannotChangeLockHandler, [classname]);
  fPessimisticLockHandler := Value;
end;

function TBoldSystem.GetOnPreUpdate: TNotifyEvent;
begin
  Result := SystemPersistenceHandler.OnPreUpdate;
end;

procedure TBoldSystem.SetOnPreUpdate(const Value: TNotifyEvent);
begin
  SystemPersistenceHandler.OnPreUpdate := Value;
end;

function TBoldSystem.GetTimeStampOfLatestUpdate: TBoldTimeStampType;
begin
  Result := SystemPersistenceHandler.TimeStampOfLatestUpdate
end;

function TBoldSystem.GetTimeOfLatestUpdate: TDateTime;
begin
  Result := SystemPersistenceHandler.TimeOfLatestUpdate
end;

function TBoldSystem.AssertLinkIntegrity: Boolean;
var
  Traverser: TBoldLocatorListTraverser;
  i: integer;
  bo: TBoldObject;
  G: IBoldGuard;
begin
  G := TBoldGuard.Create(Traverser);
  Traverser := Locators.CreateTraverser;
  while Traverser.MoveNext do
  begin
    if assigned(Traverser.Locator.BoldObject) then
    begin
      bo := Traverser.Locator.BoldObject;
      for i := 0 to bo.BoldMemberCount - 1 do
        if bo.BoldMemberAssigned[i] and (bo.BoldMembers[i].BoldPersistenceState <> bvpsInvalid)
          and (bo.BoldMembers[i].GetController <> nil) then
          bo.BoldMembers[i].GetController.AssertIntegrity;
    end;
  end;
  Result := True;
end;

function TBoldSystem.CanEvaluateInPS(sOCL: string;
    aContext: TBoldElementTypeInfo = nil;
    const aVariableList: TBoldExternalVariableList = nil): Boolean;
begin
  Result := SystemPersistenceHandler.CanEvaluateInPS(sOCL, aContext, aVariableList);
end;

function TBoldSystem.GetIsDiscarding: Boolean;
begin
  Result := FDiscardCount > 0;
end;

function TBoldSystem.ContainsDirtyObjectsOfClass(AClassType: TBoldObjectClass): boolean;
var
  i: integer;
  List: TList;
begin
  result := false;
  List := DirtyObjects;
  for I := List.Count - 1 downto 0 do
  begin
    if TBoldObject(List[i]) is AClassType then
    begin
      result := true;
      exit;
    end;
  end;
end;

procedure TBoldSystem.RemoveDeletedObjects(IDList: TBoldObjectIdList);
var
  i: integer;
  TranslationList: TBoldIdTranslationList;
begin
  TranslationList := TBoldIdTranslationList.Create;
  try
    PersistenceController.PMExactifyIDs(IdList, TranslationList, true);
    for I := TranslationList.Count - 1 downto 0 do
      if TranslationList.NewIds[i].NonExisting then
        IdList.Remove(IdList.IDByID[TranslationList.OldIds[i]]);
  finally
    TranslationList.Free;
  end;
end;

function TBoldSystem.GetClassTypeForID(ObjectID: TBoldObjectID): TBoldClassTypeInfo;
begin
  result := nil;
  if Assigned(ObjectId) then
    result := BoldSystemTypeInfo.TopSortedClasses[ObjectId.TopSortedIndex];
end;

function TBoldSystem.CreateDirtyObjectTracker: IBoldDirtyObjectTracker;
begin
  result := TBoldDirtyObjectTracker.Create(Self);
  result.StartTracking;
end;

{ TBoldObject }

function TBoldObject.GetDeriveMethodForMember(Member: TBoldMember): TBoldDeriveAndResubscribe;
begin
  Result := GetDeriveMethodForMember(Member.BoldmemberRTInfo.Index);
end;

function TBoldObject.GetDeriveMethodForMember(MemberIndex: Integer): TBoldDeriveAndResubscribe;
begin
  result := nil;
end;

function TBoldObject.GetReverseDeriveMethodForMember(Member: TBoldMember): TBoldReverseDerive;
begin
  result := GetReverseDeriveMethodForMember(Member.BoldmemberRTInfo.Index);
end;

function TBoldObject.GetReverseDeriveMethodForMember(MemberIndex: Integer): TBoldReverseDerive;
begin
  result := nil;
end;

function TBoldObject.GetBoldMembers(index: Integer): TBoldMember;

  procedure InternalRaise;
  var
    ID: String;
  begin
    if Assigned(FBoldObjectLocator) and Assigned(FBoldObjectLocator.BoldObjectID) then
      ID := FBoldObjectLocator.BoldObjectID.AsString
    else
      ID := '(NoID)';
    raise EBold.CreateFmt(sIndexOutOfRange, [ClassName, id, 'GetBoldMembers', Index, BoldMemberCount]); // do not localize
  end;

var
  MemberRTInfo: TBoldMemberRTInfo;
begin
  if Cardinal(index) >= Cardinal(Length(fMemberArray)) then
    InternalRaise;

  Result := fMemberArray[index];
  if not assigned(result) then
  begin
    if IsHistoricVersion and
      BoldClassTypeInfo.AllMembers[index].IsNonVersionedInVersionedClass then
      result := AtTime(BOLDMAXTIMESTAMP).BoldMembers[index]
    else
    begin
      MemberRTInfo := BoldClassTypeInfo.AllMembers[Index];
      result := TBoldMemberClass(MemberRTInfo.MemberClass).CreateAsObjectPart(Self, MemberRTInfo);
      fMemberArray[index] := result;
      InitializeMember(result, MemberRTInfo, GetElementFlag(befObjectWasCreatedNew));
    end;
  end;
end;

function TBoldObject.GetBoldMemberDeriver(Member: TBoldMember): TBoldMemberDeriver;
var
  index: integer;
  procedure InternalRaise;
  begin
    raise EBold.CreateFmt('%s[Id=%s].GetBoldMemberDeriver: Index out of range (%d but max is %d)', [ClassName, self.FBoldObjectLocator.BoldObjectID.AsString, Index, BoldClassTypeInfo.DerivedMemberCount-1]);
  end;
begin
  index := Member.BoldMemberRTInfo.DeriverIndex;
  if Cardinal(index) >= Cardinal(Length(fDeriverArray)) then
  begin
    if Cardinal(index) >= Cardinal(BoldClassTypeInfo.DerivedMemberCount) then
      InternalRaise;
    SetLength(fDeriverArray, BoldClassTypeInfo.DerivedMemberCount);
  end;
  result := fDeriverArray[index];
  if not Assigned(Result) then
  begin
    Result := TBoldMemberDeriver.Create(Member);
    fDeriverArray[index] := Result;
    Member.HasDeriver := True;
  end;
end;

function TBoldObject.SafeGetBoldMemberAssigned(Index: integer): Boolean;
begin
  result := assigned(fMemberArray[Index]);
end;

function TBoldObject.GetBoldMemberIfAssigned(index: Integer): TBoldMember;
begin
  if Cardinal(Index) >= Cardinal(Length(fMemberArray)) then
  begin
    var ID: String;
    if Assigned(FBoldObjectLocator) and Assigned(FBoldObjectLocator.BoldObjectID) then
      ID := FBoldObjectLocator.BoldObjectID.AsString
    else
      ID := '(NoID)';
    raise EBold.CreateFmt(sIndexOutOfRange, [ClassName, id, 'GetBoldMemberAssigned', Index, BoldMemberCount]); // do not localize
  end;
  Result := fMemberArray[index];
end;

function TBoldObject.GetBoldObjectIsDeleted: Boolean;
begin
  result := BoldExistenceState = besDeleted;
end;

function TBoldObject.GetBoldObjectIsNew: Boolean;
begin
  result := (BoldExistenceState = besExisting) and (BoldPersistenceState = bvpsModified);
  result := result or (BoldPersistenceState = bvpsTransient);
end;

function TBoldObject.GetStringRepresentation(Representation: TBoldRepresentation): string;
begin
//  if Representation = brJson then
//    Result := TBoldJSONwriter.BoldElementToJsonString(Self)
//  else
  if BoldClassTypeInfo.defaultstringrepresentation <> '' then
    result := EvaluateExpressionAsString(BoldClassTypeInfo.defaultstringrepresentation, brDefault)
  else
  begin
    if (Representation < BoldMemberCount) and
       (BoldMembers[Representation] is TBoldAttribute) then
        Result := BoldMembers[Representation].AsString
    else
      Result := Format('%s:%s', [BoldObjectLocator.AsString, BoldClassTypeInfo.ModelName]);
  end;
end;

procedure TBoldObject.SubscribeToStringRepresentation(Representation: TBoldRepresentation; Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent = breReEvaluate);
begin
  if BoldClassTypeInfo.defaultstringrepresentation <> '' then
    SubscribeToExpression(BoldClassTypeInfo.defaultstringrepresentation, Subscriber, false)
  else
  begin
    if Representation < BoldMemberCount then
      if BoldMembers[Representation] is TBoldAttribute then
        TBoldAttribute(BoldMembers[Representation]).DefaultSubscribe(Subscriber, RequestedEvent);
  end;
end;

function TBoldObject.GetBoldSystem: TBoldSystem;
begin
  Result := TBoldSystem(OwningElement);
  Assert(result is TBoldSystem);
end;

function TBoldObject.GetAsIBoldObjectContents(Mode: TBoldDomainElementProxyMode): IBoldObjectContents;
begin
  ProxyInterface(IBoldObjectContents, Mode, result);
end;

procedure TBoldObject.InitializeObject(System: TBoldSystem; ClassTypeInfo: TBoldClassTypeInfo; Locator: TBoldObjectLocator; Persistent: Boolean);
begin
  if ClassTypeInfo.IsAbstract then
    raise EBold.CreateFmt(sCannotInstansiateAbstractClass, [ClassTypeInfo.ExpressionName]);
  if ClassTypeInfo.IsImported then
    raise EBold.CreateFmt(sCannotInstansiateImportedClass, [ClassTypeInfo.ExpressionName]);
  fBoldClassTypeInfo := ClassTypeInfo;
  SetElementFlag(befObjectWasCreatedNew, not Assigned(Locator));
  SetElementFlag(befPersistent, System.BoldPersistent and ClassTypeInfo.Persistent and Persistent);

  SetLength(fMemberArray, BoldMemberCount);
  fTimeStamp := -1;

  if Assigned(Locator) then
  begin
    FBoldObjectLocator := Locator;
    BoldObjectLocator.FBoldObject := Self;
    BoldObjectLocator.EmbeddedSingleLinksToObject;
    SetElementFlag(befIsHistoricVersion, BoldObjectLocator.BoldObjectID.TimeStamp <> BOLDMAXTIMESTAMP);
  end
  else
  begin
    FBoldObjectLocator := TBoldObjectLocator.CreateWithClassID(System, ClassTypeInfo.TopSortedIndex, True);
    BoldObjectLocator.FBoldObject := Self;
  end;
  if ClassTypeInfo.ToBeRemoved then
    ToBeRemovedClassAccessed;
end;

procedure TBoldMember.InitializeStateToModified;
begin
  SetInternalState(BoldPersistenceStateMask, BoldPSShift, Integer(bvpsModified));
end;

procedure TBoldMember.InitializeStateToInvalid;
begin
  SetInternalState(BoldPersistenceStateMask, BoldPSShift, Integer(bvpsInvalid));
end;

function TBoldMember.GetBoldPersistenceState: TBoldValuePersistenceState;
begin
  result := TBoldValuePersistenceState((StateAndFlagBank and BoldPersistenceStateMask) shr BoldPSShift);
end;

procedure TBoldMember.InitializeStateToTransient;
begin
  SetInternalState(BoldPersistenceStateMask, BoldPSShift, Integer(bvpsTransient));
end;

procedure TBoldMember.InitializeStateToCurrent;
begin
  SetInternalState(BoldPersistenceStateMask, BoldPSShift, Integer(bvpsCurrent));
end;

function TBoldAttribute.GetAttributeTypeInfoForType: TBoldElementTypeInfo;
begin
  result := FindASystem.BoldSystemTypeInfo.AttributeTypeInfoByDelphiName[Self.ClassName]
end;

function TBoldAttribute.GetBoldAttributeRTInfo: TBoldAttributeRTInfo;
begin
  Result := TBoldAttributeRTInfo(BoldMemberRTInfo);
  Assert((not Assigned(result)) or (result is TBoldAttributeRTInfo));
end;

procedure TBoldObject.InitializeMember(Member: TBoldMember; MemberRTInfo: TBoldMemberRTInfo; IsNewObject: Boolean);
begin
  Member.IsInitializing:= true;
  try
    if IsNewobject then
    begin {New object}
      if MemberCanBeModified(MemberRtInfo, BoldSystem) then
      begin
        if Member.BoldPersistent then
          Member.InitializeStateToModified
        else
          Member.InitializeStateToTransient;
        Member.DoSetInitialValue;
      end
      else if Member.Derived then
        Member.InitializeStateToInvalid
      else if Member.BoldPersistent then // Non-embedded Role starts as current since noone has associations to a new object
      begin
        Member.InitializeStateToCurrent;
      end
      else
      begin
        Member.InitializeStateToTransient;
        Member.DoSetInitialValue;
      end;
    end
    else
    begin {Old object}
      if Member.BoldPersistent then
        Member.InitializeStateToInvalid
      else if Member.Derived then
        Member.InitializeStateToInvalid
      else if MemberRTInfo.IsRole and
        (TBoldRoleRTInfo(MemberRTInfo).roleType in [rtInnerLinkRole, rtLinkRole]) then
        Member.InitializeStateToInvalid
      else
      begin
        Member.InitializeStateToTransient;
        if Assigned(Member.BoldMemberRTInfo) and Member.BoldMemberRTInfo.IsAttribute and TBoldAttribute(Member).BoldAttributeRtInfo.HasInitialValue then
          Member.DoSetInitialValue;
      end;
    end;

    if MemberRTInfo.ToBeRemoved and not IsNewObject then
      ToBeRemovedMemberAccessed(MemberRTInfo);
  finally
    Member.IsInitializing:= false;
  end;
end;

constructor TBoldObject.InternalCreateWithClassAndLocator(ClassTypeInfo: TBoldClassTypeInfo; Locator: TBoldObjectLocator);
begin
  SetInternalState(BoldExistenceStateMask, BoldESShift, Integer(besExisting));
  SetInternalState(BoldPersistenceStateMask, BoldPSShift, Integer(bvpsInvalid));
  Assert(Assigned(Locator.BoldSystem));
  inherited CreateWithOwner(Locator.BoldSystem);
  InitializeObject(Locator.BoldSystem, ClassTypeInfo, Locator, True);
  EndReCreate;
  {$IFDEF ATTRACS} {$IFDEF BOLD_PERFORMANCE_COUNTERS}
  BoldSystemPerfObject.BoldObject_InternalCreateWithClassAndLocator(ClassTypeInfo.ModelName);
  {$ENDIF} {$ENDIF}
end;

constructor TBoldObject.Create(AOwningElement: TBoldDomainElement; Persistent: Boolean = True);
var
  System: TBoldSystem;
  aClass: TBoldClassTypeInfo;
begin
  IsEffectiveInvalidKnown := false;
  SetInternalState(BoldExistenceStateMask, BoldESShift, Integer(besNotCreated));
  SetInternalState(BoldPersistenceStateMask, BoldPSShift, Integer(bvpsCurrent));
  if AOwningElement = nil then
  begin
    System := TBoldSystem.DefaultSystem;
    if not assigned(system) then
      raise EBold.CreateFmt(sNoDefaultSystem, [ClassName]);
  end
  else if AOwningElement is TBoldSystem then
    System := TBoldSystem(AOwningElement)
  else
    raise EBold.CreateFmt(sOwningElementMustBeSystem, [ClassName]);

  aClass := System.BoldSystemTypeInfo.TopSortedClasses.ItemsByObjectClass[ClassType];
  if not assigned(aClass) then
  begin
    if not System.BoldSystemTypeInfo.UseGeneratedCode then
      raise EBold.CreateFmt(sGeneratedCodeNotUsed + BOLDCRLF + sGeneratedCode_HowToFix, [ClassName])
    else
      raise EBold.CreateFmt(sNoClassInformation, [ClassName, Classname])
  end;
  if aClass.IsLinkClass then
    raise EBold.CreateFmt(sCannotCreateAssociationClass, [classname]);

  InternalCreateNewWithClassAndSystem(aClass, System, Persistent);
end;

procedure TBoldObject.ReceiveEventFromOwned(originator: TObject; originalEvent: TBoldEvent; const Args: array of const);
begin
  if (BoldExistenceState = besExisting) and
     (originalEvent in beValueEvents) then
    SendExtendedEvent(beMemberChanged, [Originator]);
  if (BoldExistenceState = besExisting) and
     (originalEvent in beBroadcastMemberEvents) then
    BoldSystem.ReceiveEventFromOwned(Originator, OriginalEvent, Args); // broadcast member events through BoldSystem
end;

function TBoldObject.ReceiveQueryFromOwned(Originator: TObject;
  OriginalEvent: TBoldEvent; const Args: array of const;
  Subscriber: TBoldSubscriber): Boolean;
begin
  result := SendQuery(OriginalEvent, Args, Subscriber, Originator);
end;

destructor TBoldObject.Destroy;
var
  I: Integer;
  aLocator: TBoldObjectLocator;
begin
  if BoldExistenceState = besNotCreated then
  begin
    if assigned(OwningElement) then
    begin
      if assigned(BoldObjectLocator) then
      begin
        aLocator := BoldObjectLocator;
        aLocator.FBoldObject := nil;
        FBoldObjectLocator := nil;
        BoldSystem.Locators.remove(aLocator);
      end;
      BoldSystem.MarkObjectClean(self);
    end;
  end
  else
  begin
    if Assigned(BoldObjectLocator) then
      raise EBold.CreateFmt(sIllegalDirectDestruction, [ClassName]);
  end;

  PrepareToDestroy;
  FreeDerivers;
  for i := 0 to Length(fMemberArray) - 1 do
    FreeAndNil(fMemberArray[i]);
  SetLength(fMemberArray, 0);
  inherited Destroy;
end;

procedure TBoldObject.DefaultSubscribe(Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent);
begin
// do nothing
end;

procedure TBoldObject.InternalUnLinkAll(AUnlinkPersistent: boolean);
var
  i, MemberIndex: Integer;
  RoleRTInfo: TBoldRoleRTInfo;
  Saved: boolean;
begin
  Saved := BoldObjectLocator.BoldObjectID.IsStorable;
  for i := 0 to BoldClassTypeInfo.AllRoles.Count - 1 do
  begin
    RoleRTInfo := BoldClassTypeInfo.AllRoles[i];
    MemberIndex := RoleRTInfo.index;
    if not Saved and not SafeGetBoldMemberAssigned(MemberIndex) then
      continue; // can skip nil members if not persistent
    if RoleRTInfo.IsDerived then
    begin
      if SafeGetBoldMemberAssigned(MemberIndex) then
        BoldMembers[MemberIndex].Invalidate;
    end
    else if not AUnlinkPersistent and not RoleRTInfo.Persistent then
    begin
      // do not discard transient roles
    end
    else if RoleRTInfo.isMultiRole then
    begin
      if (RoleRTInfo.RoleType = rtRole) and
        ((BoldMembers[MemberIndex] as TBoldObjectList).Count <> 0) then
        (BoldMembers[MemberIndex] as TBoldObjectList).Clear;
    end
    else if RoleRTInfo.isSingleRole then
    begin
      if (RoleRTInfo.RoleType = rtRole) and
        assigned((BoldMembers[MemberIndex] as TBoldObjectReference).Locator) then
        (BoldMembers[MemberIndex] as TBoldObjectReference).Clear;
    end;
  end;
end;

procedure TBoldObject.UnLinkAll;
begin
  InternalUnLinkAll(True);
end;

procedure TBoldObject.UnLinkAllPersistent;
begin
  InternalUnLinkAll(False);
end;

{$IFNDEF CompareToOldValues}
procedure TBoldObject.MarkObjectDirty;
var
  i: integer;
begin
  if BoldDirty then
    exit;
  for i := 0 to BoldMemberCount - 1 do
  begin
    if SafeGetBoldMemberAssigned(i) and
      BoldMembers[i].BoldMemberRTInfo.IsStoredInObject and
      (BoldMembers[i].BoldPersistenceState = bvpsCurrent) then
    begin
      BoldMembers[i].MarkMemberDirty;
      exit;
    end;
  end;

  for i := 0 to BoldMemberCount - 1 do
  begin
    if BoldMembers[i].BoldMemberRTInfo.IsStoredInObject and
      (BoldMembers[i].BoldPersistenceState = bvpsCurrent) then
    begin
      BoldMembers[i].MarkMemberDirty;
      exit;
    end;
  end;

  raise EBold.CreateFmt(sNoPersistentMembers, [classname]);
end;
{$ENDIF}

procedure TBoldObject.ReRead;
begin
//  Invalidate;
// patch - do not invalidate, it invalidates all members, just fetch instead
// fetching will update only members with fresh value from db, leaving unomdified members valid
  BoldSystem.SystemPersistenceHandler.FetchObjectById(BoldObjectLocator.BoldObjectID);
end;

{check than no links exist except for member "index"}
function TBoldObject.CheckLinks(index: Integer): Boolean;
var
  I: Integer;
  MemberRTInfo: TBoldmemberRTInfo;
begin
  Result := True;
  for I := 0 to BoldClassTypeInfo.AllMembersCount - 1 do
  begin
    MemberRTInfo := BoldClassTypeInfo.AllMembers[i];
    if (I <> index) and not MemberRTInfo.IsDerived then
    begin
      if MemberRTInfo.IsMultiRole then
      begin
        if ((MemberRTInfo as TBoldRoleRTInfo).RoleType = rtRole) and
          ((BoldMembers[I] as TBoldObjectList).Count <> 0) then
        begin
          Result := False;
          break;
        end;
      end
      else if MemberRTInfo.IsSingleRole then
      begin
        if ((MemberRTInfo as TBoldRoleRTInfo).RoleType = rtRole) and
          assigned((BoldMembers[I] as TBoldObjectReference).Locator) then
        begin
          Result := False;
          break;
        end;
      end;
    end;
  end;
end;

function TBoldObject.InternalCanDelete(CheckedObjects: TBoldDomainElementCollection; Cascade: Boolean): Boolean;

  function CheckEmpty(Member: TBoldMember): Boolean;
  begin
    result := false;
    if Member is TBoldObjectList then
      result := TBoldObjectList(Member).Count = 0
    else if member is TBoldObjectReference then
      result := not assigned(TBoldObjectReference(member).BoldObject);
  end;

  function CascadeCanDelete(Member: TBoldMember): Boolean;
  var
    i: integer;
    list: TBoldObjectList;
  begin
    result := true;
    if Member is TBoldObjectList then
    begin
      list := TBoldObjectList(Member);
      List.EnsureObjects;
      for i := 0 to list.Count - 1 do
        result := result and list[i].InternalCanDelete(CheckedObjects, Cascade);
    end
    else if member is TBoldObjectReference then
    begin
      if assigned(TBoldObjectReference(member).BoldObject) then
        result := TBoldObjectReference(member).BoldObject.InternalCanDelete(CheckedObjects, Cascade);
    end;
  end;

var
  i: integer;
  MemberRTInfo: TBoldMemberRTInfo;
  RoleRTInfo: TBoldRoleRTInfo;
begin
  result := not IsReadOnly;

  if CheckedObjects.Includes(self) then
    exit;
  CheckedObjects.Add(self);

  if not result then
  begin
    SetBoldLastFailureReason(TBoldFailureReason.Create(sObjectIsreadOnly, self));
    exit;
  end;

  result := MayDelete;
  if not result then
    exit;

  for i := 0 to BoldMemberCount - 1 do
  begin
    MemberRTInfo := BoldClassTypeInfo.AllMembers[i];
    if MemberRTInfo.IsRole then
    begin
      Assert(MemberRTInfo is TBoldRoleRTInfo);
      RoleRTInfo := TBoldRoleRTInfo(MemberRTInfo);
      if BoldSystem.BoldPersistent or SafeGetBoldMemberAssigned(i) then
      begin
        if not RoleRTInfo.IsDerived and (RoleRTInfo.RoleType = rtRole) then
        begin
          case RoleRTInfo.DeleteAction of
            daCascade:
            begin
              result := not cascade or (result and CascadeCanDelete(BoldMembers[i]));
              if not result then
                SetBoldLastFailureReason(TBoldFailureReason.CreateFmt(sDeniedCascadeDelete, [BoldMembers[i].DisplayName], self));
            end;
            daAllow: ;
            daProhibit:
            begin
              result := result and CheckEmpty(BoldMembers[i]);
              if not result then
                SetBoldLastFailureReason(TBoldFailureReason.CreateFmt(sDeniedDeleteFromProhibit, [BoldMembers[i].DisplayName], self));
            end;
          end;
        end
        else
        begin
          if RoleRTInfo.RoleType = rtLinkRole then
          begin
             result := result and CascadeCanDelete(BoldMembers[i]);
             if not result then
               SetBoldLastFailureReason(TBoldFailureReason.CreateFmt(sDeniedCascadeDeleteLink, [BoldMembers[i].DisplayName], self));
          end;
        end;
        if not result then
          break;
      end;
    end;
  end;

  if result then
    result := {$IFNDEF BOLD_NO_QUERIES}SendQuery(bqMayDelete, [], nil) and {$ENDIF} BoldSystem.CanDeleteObject(self)
  else
    SetBoldLastFailureReason(TBoldFailureReason.Create(sObjectHasRelations, self));
end;

function TBoldObject.InternalCanDeleteObject: Boolean;
begin
  Result := True;
end;

function TBoldObject.CanDelete: Boolean;
var
  list: TBoldDomainElementCollection;
  g: IBoldGuard;
begin
  g := TBoldGuard.Create(List);
  list := TBoldDomainElementCollection.Create;
  result := InternalCanDelete(list, true);
end;

procedure TBoldObject.Delete;

  procedure CascadeDelete(Member: TBoldMember);
  var
    i: integer;
    list: TBoldObjectList;
    obj: TBoldObject;
  begin
    if Member is TBoldObjectList then
    begin
      list := TBoldObjectList(Member);
      list.EnsureObjects;
      for i := list.Count - 1 downto 0 do
      begin
        obj := list[i];
        list.RemoveByIndex(i);
        obj.Delete;
      end;
    end
    else if member is TBoldObjectReference then
    begin
      Obj := TBoldObjectReference(member).BoldObject;
      TBoldObjectReference(member).BoldObject := nil;
      if assigned(obj) then
        Obj.Delete;
    end;
  end;

  procedure ClearRelation(Member: TBoldMember);
  begin
    if Member is TBoldObjectList then
      TBoldObjectList(Member).Clear
    else if member is TBoldObjectReference then
      TBoldObjectReference(member).Clear;
  end;

  procedure PreFetchLinks;
  var
    i: integer;
    vRoleRTInfo: TBoldRoleRTInfo;
    LinkList: TStringList;
  begin
    if not BoldPersistent then
      exit;
    LinkList := TStringList.Create;
    try
      for i := 0 to BoldClassTypeInfo.AllRoles.Count - 1 do
      begin
        vRoleRTInfo := BoldClassTypeInfo.AllRoles[i];
        if not vRoleRTInfo.IsDerived and (vRoleRTInfo.RoleType in [rtRole, rtInnerLinkRole]) then
          case vRoleRTInfo.DeleteAction of
            daCascade, daAllow:
              LinkList.Add(vRoleRTInfo.ExpressionName);
        end;
      end;
    finally
      BoldSystem.FetchMembersWithObject(Self, LinkList.CommaText);
//      BoldSystem.FetchLinksWithObjects(AList, LinkList.CommaText);
      LinkList.free;
    end;
  end;

  procedure DoDelete;
  var
    RoleRTInfo: TBoldRoleRTInfo;
    i: integer;
  begin
//    PreFetchLinks;
    for i := 0 to BoldClassTypeInfo.AllRoles.Count - 1 do
    begin
      RoleRTInfo := BoldClassTypeInfo.AllRoles[i];
        if not RoleRTInfo.IsDerived and
           (RoleRTInfo.RoleType in [rtRole, rtInnerLinkRole]) then
        begin
          case RoleRTInfo.DeleteAction of
            daCascade:
            begin
            if BoldPersistent or BoldMemberAssigned[RoleRTInfo.index] then
              CascadeDelete(BoldMembers[RoleRTInfo.index]);
            end;
            daAllow:
            begin
            if BoldPersistent or BoldMemberAssigned[RoleRTInfo.index] then
              ClearRelation(BoldMembers[RoleRTInfo.index]);
            end;
            daProhibit: ;
          end;
        end;
      end;
    end;

{$IFNDEF NoAutoSubscription}
  procedure ProcessDeleteInDerivation;
  var
    i: integer;
  begin
    // Clear any Members of deleted object that were acccesed during derivation
    // and were supposed to be subscribed to in MemberDerivationEnd
    with BoldSystem.fMembersReadDuringDerivation  do
      for i := 0 to Count - 1 do
        if (Items[i] <> nil) and (TBoldMember(Items[i]).OwningObject = self) then
          Items[i] := nil;
    LogDerivationDeleteSideEffects(self);
  end;
{$ENDIF}
begin
  if (not Deleting) and (not DeletingOrDeletingByDiscard) then begin
    BoldSystem.DelayObjectDestruction;
    Deleting := True;
    try
      BoldClearLastFailure;
      if InternalCanDeleteObject then begin
        BoldSystem.IncrementDeletingObjectsDepth;
        try
          BoldSystem.StartTransaction(stmNormal);
          try
            if not StartDelete then
              BoldRaiseLastFailure(self, 'Delete', sPreconditionNotMet); // do not localize
            DoDelete;
            EndDelete;
            {$IFNDEF NoAutoSubscription}
            if BoldSystem.IsDerivingMembers then
              ProcessDeleteInDerivation;
            {$ENDIF}
            BoldSystem.CommitTransaction(stmNormal);
          except
            FailDelete;
            BoldSystem.RollbackTransaction(stmNormal);
            raise;
          end;
        finally
          BoldSystem.DecrementDeletingObjectsDepth;
        end;
      end;
    finally
      Deleting := False;
      BoldSystem.AllowObjectDestruction;
    end;
  end;
end;

function TBoldObject.GetDeletingOrDeletingByDiscard: Boolean;
begin
  Result := Deleting or
           (Discarding and BoldObjectIsNew) or
           BoldObjectIsDeleted;
end;

function TBoldObject.FindBoldMemberByExpressionName(const Name: string): TBoldMember;
var
  index: Integer;
begin
  index := BoldMemberIndexByExpressionName[Name];
  if index = -1 then
    Result := nil
  else
    Result := BoldMembers[index]
end;

function TBoldObject.GetBoldMemberByExpressionName(const Name: string): TBoldMember;
begin
  Result := FindBoldMemberByExpressionName(Name);
  if not Assigned(Result) then
    raise EBold.CreateFmt(sNoSuchMember, [ClassName, Name]);
end;

function TBoldObject.GetBoldMemberIndexByExpressionName(const Name: string): Integer;
var
  Member: TBoldMemberRTInfo;
begin
  Member := BoldClassTypeInfo.MemberRTInfoByExpressionName[Name];
  if Member = nil then
    Result := -1
  else
    Result := Member.index;
end;

function TBoldObject.CompareToAs(CompType: TBoldCompareType; BoldElement: TBoldElement): Integer;
begin
  if (BoldElement = self) or ((BoldElement is TBoldObjectReference) and (TBoldObjectReference(BoldElement).BoldObject = self)) then
    result := 0
  else
    result := -1;
end;

function TBoldObject.IsEqualAs(CompareType: TBoldCompareType; BoldElement: TBoldElement): Boolean;
begin
  case CompareType of
    ctDefault:
    begin
      if BoldElement is TBoldObjectReference then
        BoldElement := TBoldObjectReference(BoldElement).BoldObject;
      Result := BoldElement = Self;
    end;
  else
    raise EBold.CreateFmt('%s.IsEqualAs: Unknown CompareType', [ClassName]);
  end;
end;

procedure TBoldObject.GetAsList(ResultList: TBoldIndirectElement);
var
  NewList: TBoldList;
begin
  Assert(BoldType.SystemTypeInfo is TBoldSystemTypeInfo);
  NewList := BoldClassTypeInfo.ListTypeInfo.CreateElement as TBoldList;
  NewList.Add(Self);
  NewList.MakeImmutable;
  ResultList.SetOwnedValue(NewList);
end;

procedure TBoldObject.EndCreate;
begin
  SetBoldExistenceState(besExisting);
  if BoldPersistent then
    SetBoldPersistenceState(bvpsModified)
  else
    SetBoldPersistenceState(bvpsTransient);
  try
    IsCreating := true;
    try
      CompleteCreate;
      SendEvent(beCompleteModify);
    finally
      IsCreating := false;
    end;
  except
    SetBoldExistenceState(besNotCreated);
    SetBoldPersistenceState(bvpsCurrent);
    raise;
  end;
  BoldSystem.AddToTransaction(self);
end;

procedure TBoldObject.EndReCreate;
begin
  SetBoldExistenceState(besExisting);
  SetBoldPersistenceState(bvpsCurrent);
  CompleteRecreate;
  Sendevent(beObjectFetched);
end;

procedure TBoldObject.EndUpdate(Translationlist: TBoldIdTranslationlist);
begin
  SetBoldPersistenceState(bvpsCurrent);
end;

function TBoldObject.StartDelete: Boolean;
var
  list: TBoldDomainElementCollection;
  g: IBoldGuard;
begin
  g := TBoldGuard.Create(List);
  if not (BoldExistenceState = besExisting) then
    StateError('StartDelete');

  list := TBoldDomainElementCollection.Create;
  result := InternalCanDelete(list, false);

  if result and assigned(BoldSystem.PessimisticLockHandler) then
    result := BoldSystem.PessimisticLockHandler.LockElement(self);
  if result then
    DoStartDelete;
end;

procedure TBoldObject.EndDelete;
begin
  SetBoldExistenceState(besDeleted);
  case BoldPersistenceState of
    bvpsModified: SetBoldPersistenceState(bvpsCurrent);
    bvpsTransient: ;
    bvpsCurrent: SetBoldPersistenceState(bvpsModified);
  end;
  if BoldPersistenceState <> bvpsModified then
    BoldSystem.DestroyObject(self);
  BoldSystem.AddToTransaction(self);
end;

procedure TBoldObject.FailDelete;
begin
end;

function TBoldObject.GetBoldDirty: Boolean;
begin
  if BoldPersistenceState = bvpsModified then
    result := True
  else if BoldExistenceState <> besExisting then
    result := False
  else
  begin
    if not MemberModifiedKnown then
      CalculateMemberModified;
    result := MemberModified;
  end;
end;

function TBoldObject.CalculateEffectiveInvalid: Boolean;
var
  i: Integer;
  Member: TBoldMemberRTInfo;
begin

  if GetElementFlag(befObjectWasCreatedNew) then
  begin
    result := false;
    exit;
  end;

  for i := 0 to BoldMemberCount - 1 do
  begin
    if (not SafeGetBoldMemberAssigned(i) or (BoldMembers[i].BoldPersistenceState = bvpsInvalid)) then
    begin
      Member := BoldClassTypeInfo.AllMembers[i];
      if (not Member.DelayedFetch) and (Member.Persistent) then
      begin
        result := True;
        exit;
      end;
    end;
  end;
  result := False;
end;

function TBoldObject.GetEffectiveInvalid: Boolean;
begin
  if not IsEffectiveInvalidKnown then
  begin
    IsEffectiveInvalid := CalculateEffectiveInvalid;
    IsEffectiveInvalidKnown := true;
  end;
  result := IsEffectiveInvalid;
end;

procedure TBoldObject.FreeDerivers;
var
  i: integer;
  Deriver: TBoldMemberDeriver;
begin
  for I := 0 to Length(fDeriverArray) - 1 do
  begin
    Deriver := fDeriverArray[i];
    if Assigned(Deriver) then
    begin
      Deriver.DerivedMember.HasDeriver := false;
      Deriver.Free;
      fDeriverArray[i] := nil;
    end;
  end;
  SetLength(fDeriverArray, 0);
end;


procedure TBoldObject.MemberBecomingModified(BoldMember: TBoldMember);
begin
  MemberModified := true;
  MemberModifiedKnown := true;
  BoldSystem.MarkObjectDirty(self);
  BoldMember.SendExtendedEvent(beMemberBecomingDirty, [BoldMember]);
  if BoldSystem.IsDerivingMembers then
    LogDerivationSideEffects(BoldMember);
end;

procedure TBoldObject.MemberBecomingClean(BoldMember: TBoldMember);
begin
  if MemberModified then
  begin
    MemberModifiedKnown := False;
    BoldMember.SendExtendedEvent(beMemberBecomingClean, [BoldMember]);
    BoldSystem.MarkObjectPossiblyCleaner(self);
  end;
end;

procedure TBoldObject.MemberChangingValidity(BoldMemberRtInfo: TBoldMemberRtInfo; NewValue: TBoldValuePersistenceState);
begin
  if (not IsEffectiveInvalidKnown) then
    Exit;
  if (BoldMemberRtInfo.DelayedFetch) or (not BoldMemberRtInfo.Persistent) then
    Exit;
  if (NewValue = bvpsInvalid) then
  begin
    IsEffectiveInvalid := true;
    IsEffectiveInvalidKnown := true;
  end
  else
    if IsEffectiveInvalidKnown then
      IsEffectiveInvalidKnown := false;
end;

procedure TBoldObject.CalculateMemberModified;
var
  i: Integer;
begin
  MemberModifiedKnown := True;
  MemberModified := false;
  if BoldPersistent then
    for i := 0 to BoldMemberCount - 1 do
    begin
      if SafeGetBoldMemberAssigned(i) then
        MemberModified := BoldMembers[i].BoldDirty
      else
        MemberModified := GetElementFlag(befObjectWasCreatedNew) and
                          MemberCanBeModified(BoldClassTypeInfo.AllMembers[i], BoldSystem) and
                          Assigned(BoldSystem.PersistenceController);
      if MemberModified then
        Exit;
    end;
end;

procedure TBoldObject.CompleteCreate;
begin
end;

procedure TBoldObject.CompleteReCreate;
begin
end;


procedure TBoldObject.CompleteUpdate;
begin
end;


function TBoldObject.MayUpdate: Boolean;
begin
  result := True;
end;

procedure TBoldObject.PrepareUpdate;
begin
end;

function TBoldObject.MayDelete: Boolean;
begin
  result := True;
end;

procedure TBoldObject.PrepareDelete;
begin
end;

procedure TBoldObject.PrepareDiscard;
begin
  if BoldObjectIsNew then begin
    InternalPrepareDeleteOrDeleteByDiscard;
  end;
end;

procedure TBoldObject.InternalPrepareDeleteOrDeleteByDiscard;
begin
  // intentionally left blank
end;

procedure TBoldObject.DoStartDelete;
begin
  PrepareDelete;
  SendEvent(bePrepareDelete);
end;

{$IFNDEF NoMayUpdate}
procedure TBoldObject.DoStartUpdate;
begin
//  SendEvent(bePrepareUpdate);
  PrepareUpdate;
  PrepareUpdateMembers;
end;
{$ENDIF}

function TBoldObject.GetIsModified: Boolean;
begin
  result := BoldPersistenceState = bvpsModified;
end;

{$IFNDEF NoMayUpdate}
function TBoldObject.MayUpdateMembers: Boolean;

  function MemberProhibitsUpdate(Member: TBoldMember): Boolean;
  begin
    result :=  assigned(Member)
               and Member.BoldPersistent
               and (Member.BoldDirty  or BoldSystem.BoldSystemTypeInfo.UpdateWholeObjects)
               and not Member.CanUpdate;
  end;

var
  i: Integer;
begin
  for i := 0 to BoldMemberCount - 1 do
  begin
    if SafeGetBoldMemberAssigned(i) and MemberProhibitsUpdate(BoldMembers[i]) then
    begin
      result := False;
      exit;
    end;
  end;
  result := True;
end;
{$ENDIF}

procedure TBoldObject.EndUpdateMembers(Translationlist: TBoldIdTranslationlist);
var
  i: Integer;
  Member: TBoldMember;
begin
  for i := 0 to BoldMemberCount - 1 do
    if SafeGetBoldMemberAssigned(i) then
    begin
      Member := BoldMembers[i];
      if Member.BoldPersistent and
       (BoldSystem.BoldSystemTypeInfo.UpdateWholeObjects or Member.BoldDirty) then
      begin
        Member.EndUpdate(Translationlist);
        Member.CompleteUpdate;
      end;
    end;
end;

{ // trying to fetch a member that is current or modified should be allowed, but later ignored...
// the below code is left here for some time until it will be removed...

function TBoldObject.MayFetchMembers(MemberIdList: TBoldMemberIdList): Boolean;

  function MemberProhibitsFetch(Member: TBoldMember): Boolean;
  begin
    result :=  assigned(Member)
               and ((Member.BoldPersistenceState = bvpsInvalid) or FetchEvenIfCurrent)
               and false; // not Member.CanFetch;
  end;

var
  i: Integer;
  aMember: TBoldMember;
begin
  if assigned(MemberIdList) then
  begin
    for i := 0 to MemberIdList.Count - 1 do
      if MemberProhibitsFetch(BoldMembers[MemberIdList[i].MemberIndex]) then
      begin
        result := False;
        exit;
      end;
  end
  else
  begin
    for i := 0 to BoldMemberCount - 1 do
    begin
      aMember := BoldMembers[i];
      if aMember.BoldPersistent and not aMember.BoldMemberRTInfo.DelayedFetch
      and MemberProhibitsFetch(BoldMembers[i]) then
      begin
        result := False;
        exit;
      end;
    end;
  end;
  result := True;
end;
}
{procedure TBoldObject.DoStartFetchMembers(MemberIdList: TBoldMemberIdList);

  procedure IfNecessaryDoStartFetchMember(Member: TBoldMember);
  begin
    if assigned(Member) and Member.BoldPersistent
    and ((Member.BoldPersistenceState = bvpsInvalid)) then
    begin
      if Member.BoldPersistenceState = bvpsModified then
        raise EBold.Create('Cannot fetch modified member. Call discard first.');
    end;
  end;

var
  i: Integer;
  aMember: TBoldMember;
begin
  if assigned(MemberIdList) then
    for i := 0 to MemberIdList.Count - 1 do
      IfNecessaryDoStartFetchMember(BoldMembers[MemberIdList[i].MemberIndex])
  else
    for i := 0 to BoldMemberCount - 1 do
    begin
      aMember := BoldMembers[i];
      if not aMember.BoldMemberRTInfo.DelayedFetch then
        IfNecessaryDoStartFetchMember(aMember);
    end;
end;
}

procedure TBoldObject.EndFetchMembers(MemberIdList: TBoldMemberIdList);

  procedure IfNecessaryEndFetchMember(aBoldMember: TBoldMember);
  begin
    if Assigned(aBoldMember) and (aBoldMember.BoldPersistenceState = bvpsInvalid) then
    begin
      aBoldMember.BoldPersistenceState := bvpsCurrent;
    end;
  end;

var
  i: Integer;
  MemberRtInfo: TBoldMemberRTInfo;
begin
  if assigned(MemberIdList) then
  begin
    for i := 0 to MemberIdList.Count - 1 do
      IfNecessaryEndFetchMember(BoldMembers[MemberIdList[i].MemberIndex]);
  end
  else
  begin
    {$IFDEF ATTRACS} {$IFDEF BOLD_PERFORMANCE_COUNTERS}
    BoldSystemPerfObject.BoldObject_EndFetchMembers;
    {$ENDIF} {$ENDIF}

    for i := 0 to BoldMemberCount - 1 do
    begin
      memberRtInfo := BoldClassTypeInfo.AllMembers[i];
      if not memberRtInfo.DelayedFetch and
        not MemberRtInfo.IsDerived then
      begin
        if BoldMembers[i].BoldPersistent then
          IfNecessaryEndFetchMember(BoldMembers[i]);
      end;
    end;
  end;
end;

procedure TBoldObject.BeforeDiscard;
begin
  Inc(BoldSystem.FDiscardCount);
end;

procedure TBoldObject.BoldMakePersistent;
var
  i: Integer;
begin
  if not BoldPersistent then
  begin
    if BoldPersistenceState <> bvpsTransient then
      StateError('BoldMakePersistent'); // do not localize
    if not Assigned(BoldClassTypeInfo) or (not BoldClassTypeInfo.Persistent) or
       (not BoldSystem.BoldPersistent) then
      raise EBold.CreateFmt(sCannotMakePersistent, [ClassName]);

    if BoldClassTypeInfo.IsLinkClass then
      for i := BoldClassTypeInfo.FirstOwnMemberIndex to BoldMemberCount - 1 do
        if BoldMembers[i].BoldMemberRTInfo.IsSingleRole and
           (TBoldRoleRTInfo(BoldMembers[i].BoldMemberRTInfo).RoleType = rtInnerLinkRole) and
           not (BoldMembers[i] as TBoldObjectReference).Locator.ObjectIsPersistent then
          raise EBold.CreateFmt(sCannotMakeLinkPersistent, [Classname]);

    SetElementFlag(befPersistent, true);
    SetBoldPersistenceState(bvpsModified);
    for i := 0 to BoldMemberCount - 1 do
      BoldMembers[i].ObjectBecomingPersistent;
  end;
end;

procedure TBoldObject.SetIsReadOnly(NewValue: Boolean);
begin
  if NewValue <> IsReadOnly then
    SetElementFlag(befObjectReadOnly, NewValue);
end;

function TBoldObject.GetTimeStamp: TBoldTimeStampType;
begin
  result := fTimeStamp;
end;

procedure TBoldObject.SetTimeStamp(NewValue: TBoldTimeStampType);
var
  OldTimeStamp: TBoldTimeStampType;
begin
  if fTimeStamp <> NewValue then
  begin
    OldTimeStamp := fTimeStamp;
  fTimeStamp := NewValue;
    if OldTimeStamp <> -1 then // this will not send event on initial fetch, only on subsequent ones
      SendExtendedEvent(beObjectTimestampChanged, [OldTimeStamp, NewValue]);
  end;
end;

function TBoldObject.GetObjectHasSubscribers: Boolean;
var
  i: integer;
begin
  result := HasSubscribers;
  i := 0;
  while not result and (i < BoldMemberCount) do
  begin
    result := BoldMemberAssigned[i] and BoldMembers[i].HasSubscribers;
    inc(i);
  end;
end;

procedure TBoldObject.StateError(S: String);
begin
  inherited StateError(format('%s ID:%s (ExistenceState: %s PersistenceState: %s)',
    [s, BoldObjectLocator.AsString, GetEnumName(TypeInfo(TBoldExistenceState), Ord(BoldExistenceState)),
    GetEnumName(TypeInfo(TBoldValuePersistenceState), Ord(BoldPersistenceState))]));
end;

procedure TBoldObject.ToBeRemovedMemberAccessed(MemberRTInfo: TBoldMemberRTInfo);
begin
  BoldLog.LogFmt(sToBeRemovedAccessed, [dateTimeToStr(now), BoldClassTypeInfo.ExpressionName, MemberRTInfo.ExpressionName]);
end;

procedure TBoldObject.ToBeRemovedMemberModified(MemberRTInfo: TBoldMemberRTInfo);
begin
  BoldLog.LogFmt(sToBeRemovedModified, [dateTimeToStr(now), BoldClassTypeInfo.ExpressionName, MemberRTInfo.ExpressionName]);
end;

procedure TBoldObject.ToBeRemovedClassAccessed;
begin
  BoldLog.LogFmt(sToBeRemovedObjectAccessed, [dateTimeToStr(now), BoldClassTypeInfo.ExpressionName]);
end;

constructor TBoldObject.InternalCreateNewWithClassAndSystem(
  ClassTypeInfo: TBoldClassTypeInfo; aSystem: TBoldSystem; Persistent: Boolean);
begin
{$IFDEF NoTransientInstancesOfPersistentClass}
  if not Persistent and ClassTypeInfo.Persistent and aSystem.BoldPersistent then
    raise EBold.Create('Transient instance of persistent class not allowed due to conditional define NoTransientInstancesOfPersistentClass.');
{$ENDIF}
  IsEffectiveInvalidKnown := false;
  if not aSystem.CanCreateObject(ClassTypeInfo) then
    BoldRaiseLastFailure(aSystem, 'InternalCreateNewWithClassAndSystem', // do not localize
      format(sCannotCreateOfType, [ClassTypeInfo.ExpressionName]));
  aSystem.StartTransaction(stmNormal);
  try
    SetInternalState(BoldExistenceStateMask, BoldESShift, Integer(besNotCreated));
    SetInternalState(BoldPersistenceStateMask, BoldPSShift, Integer(bvpsCurrent));
    inherited CreateWithOwner(aSystem);
    InitializeObject(aSystem, ClassTypeInfo, nil, Persistent);
    EndCreate;
    aSystem.CommitTransaction(stmNormal);
    if BoldSystem.IsDerivingMembers then
      LogDerivationCreateSideEffects(self);
  except
    aSystem.RollbackTransaction(stmNormal);
    raise;
  end;
  {$IFDEF ATTRACS}{$IFDEF BOLD_PERFORMANCE_COUNTERS}
  BoldSystemPerfObject.BoldObject_InternalCreateNewWithClassAndSystem(ClassTypeInfo.ModelName,Persistent);
  {$ENDIF} {$ENDIF}
end;

function TBoldObject.GetBoldType: TBoldElementTypeInfo;
begin
  result := fBoldClassTypeInfo;
end;

function TBoldObject.CanUpdate: Boolean;
begin
  result := MayUpdate;
{$IFNDEF BOLD_NO_QUERIES}
    result := result and SendQuery(bqMayUpdate, [], nil);
{$ENDIF}
end;

function TBoldObject.GetDisplayName: String;
begin
  result := BoldType.AsString; // not inherited as typically we want ClassName instead of BusinessClasses.ClassName
  if Assigned(BoldObjectLocator) then
    result := '['+BoldObjectLocator.AsString + ']' + result;
end;

function TBoldObject.GetEvaluator: TBoldEvaluator;
begin
  result := BoldSystem.Evaluator;
end;

function TBoldObject.CanUnload: Boolean;
begin
  result := not BoldDirty;
  if not result then
    SetBoldLastFailureReason(TBoldFailureReason.Create(sObjectIsDirty, self));
end;

procedure TBoldObject.Invalidate;
var
  i: Integer;
begin
  if BoldDirty then
    raise EBold.CreateFmt(sCannotInvalidateDirtyObject, [classname]);

  for i := 0 to BoldClassTypeInfo.AllMembersCount - 1 do
    if BoldClassTypeInfo.AllMembers[i].Persistent and BoldMembers[i].BoldPersistent then
      BoldMembers[i].Invalidate;

  {$IFDEF ATTRACS} {$IFDEF BOLD_PERFORMANCE_COUNTERS}
  BoldSystemPerfObject.BoldObject_Invalidate;
  {$ENDIF} {$ENDIF}
end;

procedure TBoldObject.InternalDiscard(ADiscardPersistentLinks: boolean);
var
  i: Integer;
begin
  if (not Discarding) and (not Deleting) then begin
      BoldSystem.DelayObjectDestruction;
      Discarding := True;
      BeforeDiscard;
      try
        if BoldSystem.fTransactionNesting <> 0 then
          raise EBold.Create(sCannotDiscardInTransaction);

        PrepareDiscard;
        if not BoldObjectLocator.BoldObjectID.IsStorable then
        begin
          if ADiscardPersistentLinks then
            UnlinkAll
          else
            UnLinkAllPersistent;
        end;

        for i := 0 to BoldMemberCount - 1 do
          if BoldMemberAssigned[i] then
          begin
            // if ADiscardPersistentLinks = false then do not discard non persistent roles
            if ADiscardPersistentLinks or not (BoldMembers[i].BoldMemberRTInfo.IsRole and not BoldMembers[i].BoldPersistent) then
              BoldMembers[i].InternalDiscard;
          end;

        if BoldObjectIsNew then
        begin
          SetBoldExistenceState(besDeleted);
          SetBoldPersistenceState(bvpsCurrent);
          BoldSystem.DestroyObject(self);
        end
        else if (BoldPersistenceState = bvpsModified) and
                (BoldExistenceState = besDeleted) then
        begin
          SetBoldExistenceState(besExisting);
          SetBoldPersistenceState(bvpsCurrent);
        end;
    finally
      AfterDiscard;
      Discarding := False;
      BoldSystem.AllowObjectDestruction;
    end;
  end;
end;

procedure TBoldObject.Discard;
begin
  InternalDiscard(true);
end;

procedure TBoldObject.DiscardPersistentMembers;
begin
  InternalDiscard(false);
end;

procedure TBoldObject.SetBoldExistenceState(Value: TBoldExistenceState);
begin
  if Value <> BoldExistenceState then
  begin
    BoldSystem.fOldvalueHandler.ObjectExistenceChange(self);
    BoldSystem.CopyObjectToRollbackBuffer(self);
    SetInternalState(BoldExistenceStateMask, BoldESShift, integer(Value));
    if BoldPersistenceState <> bvpsInvalid then
      if Value = besExisting then
        SendEvent(beObjectCreated)
      else
        SendEvent(beObjectDeleted);
  end;
end;

procedure TBoldObject.SetBoldPersistenceState(Value: TBoldValuePersistenceState);
var
  OldValue: TBoldValuePersistenceState;
begin
  OldValue := BoldPersistenceState;
  BoldSystem.fOldvalueHandler.ObjectExistencePersistenceStateChange(self, Value);
  if Value <> OldValue then
  begin
    SetInternalState(BoldPersistenceStateMask, BoldPSShift, integer(Value));
    if Value = bvpsModified then
      BoldSystem.MarkObjectDirty(self)
    else if (OldValue = bvpsModified) then
      BoldSystem.MarkObjectPossiblyCleaner(self);
  end;
end;

function TBoldObject.ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean;
begin
  if IsEqualGuid(IID, IBoldObjectContents) then
  begin
    result := TBoldObject_Proxy.Create(self, Mode).GetInterface(IID, obj);
    if not result then
      raise EBoldInternal.CreateFmt(sInterfaceNotImplemented, [ClassName]);
  end
  else
    result := inherited ProxyInterface(IID, Mode, Obj);
end;

procedure TBoldObject.AfterDiscard;
begin
  Dec(BoldSystem.FDiscardCount);
end;

function TBoldObject.AtTime(Time: TBoldTimestampType): TBoldObject;
begin
  result := BoldObjectLocator.AtTime(time).EnsuredBoldObject;
end;

function TBoldObject.GetBoldTime: TBoldTimestampType;
begin
  result := BoldObjectLocator.BoldObjectID.TimeStamp;
end;

{$IFNDEF NoMayUpdate}
procedure TBoldObject.PrepareUpdateMembers;
var
  i: integer;
begin
  for i := 0 to BoldMemberCount - 1 do
    if SafeGetBoldMemberAssigned(i) and BoldMembers[i].BoldDirty then
      BoldMembers[i].PrepareUpdate;
end;
{$ENDIF}

function TBoldObject.GetBoldObjectExists: Boolean;
begin
  result := BoldExistenceState = besExisting;
end;

function TBoldObject.ValidateMember(const ObjectDelphiName, MemberDelphiName: String; GeneratedMemberIndex: integer; MemberClass: TBoldMemberClass): Boolean;

  procedure InternalRaise(ExceptionClass: ExceptClass; const Msg: string; const Args: array of const);
  begin
    raise ExceptionClass.CreateFmt(Msg, args)
  end;

var
  Member: TBoldMember;
  MemberRTInfo: TBoldMemberRTInfo;
begin
  if not assigned(self) then
    InternalRaise(EBold, sValidate_MemberNotAssigned, [ObjectDelphiName, MemberDelphiName, ObjectDelphiName]);

  if (self.ClassType <> BoldClassTypeInfo.ObjectClass) then
    InternalRaise(EBold, 'Object class %s does not match the BoldClassTypeInfo %s.', [self.ClassType.ClassName, BoldClassTypeInfo.ObjectClass.ClassName]);

  if Cardinal(GeneratedMemberIndex) >= Cardinal(BoldMemberCount) then
  begin
    MemberRTInfo := BoldClassTypeInfo.AllMembers.ItemsByDelphiName[MemberDelphiName] as TBoldMemberRTInfo;
    if not assigned(MemberRTInfo) then
      InternalRaise(EBold, sValidate_NoSuchMember, [ObjectDelphiName, MemberDelphiName]);

    InternalRaise(EBold, sValidate_MemberIndexOutOfSynch, [ObjectDelphiName, MemberDelphiName, GeneratedMemberIndex, MemberRTInfo.index]);
  end;

  Member := BoldMembers[GeneratedMemberIndex];
  if not assigned(Member) then
    InternalRaise(EBoldInternal, sValidate_InternalError, [ObjectDelphiName, MemberDelphiName]);

  if not (Member is MemberClass) then
    InternalRaise(EBold, sValidate_InvalidMemberType, [ObjectDelphiName, MemberDelphiName, MemberClass.ClassName, Member.ClassName, Member.DebugInfo]);

  MemberRTInfo := Member.BoldMemberRTInfo;

  if MemberRTInfo.DelphiName <> MemberDelphiName then
    InternalRaise(EBold, sValidate_WrongMember, [ObjectDelphiName, GeneratedMemberIndex, MemberDelphiName, MemberRTInfo.DelphiName]);

  result := true;
end;

procedure TBoldObject.ClearTouched;
var
  i: integer;
begin
  if Touched then
  begin
    SetElementFlag(befTouched, false);
    for i := 0 to BoldMemberCount - 1 do
      if BoldMemberAssigned[i] then
        BoldMembers[i].SetElementFlag(befTouched, false);
  end;
end;

function TBoldObject.GetGlobalId: string;
begin

end;

procedure TBoldObject.SetGlobalId(const NewValue: string);
begin

end;

{ TBoldMember }

function TBoldMember.GetStreamName: string;
begin
  raise EBoldInternal.Createfmt('%s.GetStreamName: Method is abstract, please implement', [classname]);
end;

function TBoldMember.GetFreeStandingClass: TBoldFreeStandingElementClass;
begin
  raise EBoldInternal.Createfmt('%s.GetFreeStandingClass: Method is abstract, please implement', [classname]);
end;

function TBoldMember.GetDeriverState: TBoldDeriverState;
begin
  result := TBoldDeriverState(GetInternalState(BoldDerivationStateMask, BoldDSShift));
end;

procedure TBoldMember.SetDeriverState(value: TBoldDeriverState);
begin
  SetInternalState(BoldDerivationStateMask, BoldDSShift, integer(Value));
end;

constructor TBoldMember.CreateAsObjectPart(OwningObject: TBoldObject; MemberRTInfo: TBoldMemberRTInfo);
begin
  inherited CreateWithOwner(OwningObject);
  fBoldMetaType := MemberRTInfo;
  OwnedByObject := True;
  SetElementFlag(befPersistent, OwningObject.BoldPersistent and MemberRTInfo.Persistent);
  if MemberRTInfo.IsDerived then
  begin
    DeriverState := bdsSubscriptionOutOfDate;
    SetElementFlag(befDerived, true);
  end
  else
    SetElementFlag(befDerived, false);
  Initialize;
end;

procedure TBoldMember.DeriveMember(Subscriber: TBoldSubscriber);
var
  InTransaction: boolean;
begin
  if IsPartOfSystem then
    BoldSystem.MemberDerivationBegin(self);
  try
    InTransaction := BoldSystem.InTransaction;
    if not InTransaction then
      BoldSystem.StartTransaction();
    try
    if BoldmemberRTInfo.IsDerived and
      (BoldMemberRTInfo.DeriveExpression <> '') then
      CalculateDerivedMemberWithExpression(Subscriber)
    else
      CalculateDerivedMemberWithDeriveMethod(Subscriber);
    IsPreFetched := false;
      if not InTransaction then
        BoldSystem.CommitTransaction();
    except
      if not InTransaction then
        BoldSystem.RollbackTransaction();
      raise;
    end;
  finally
    if IsPartOfSystem then
      BoldSystem.MemberDerivationEnd(self);
  end;
end;

destructor TBoldMember.Destroy;
{$IFNDEF NoAutoSubscription}
// Clear any references to member in list of accesed members during derivation
  procedure ClearDerivationReferences;
  var
    i: integer;
  begin
    with BoldSystem.fMembersReadDuringDerivation  do
      for i := 0 to Count - 1 do
        if Items[i] = self then
//        if (Items[i] <> nil) and (TBoldMember(Items[i]) = self) then
          Items[i] := nil;
  end;
{$ENDIF}
begin
{$IFNDEF NoAutoSubscription}
  if Assigned(OwningObject) and BoldSystem.IsDerivingMembers then
    ClearDerivationReferences;
{$ENDIF}
  PrepareToDestroy;
  inherited Destroy;
end;

procedure TBoldMember.CalculateDerivedMemberWithDeriveMethod(
  Subscriber: TBoldSubscriber);
var
  DeriveMethod: TMethod;
  {$IFDEF EXPRESSION_OVERRIDES_DOUBLE_DERIVE}
  Expression, s: string;
  ie: TBoldIndirectElement;
  nonMatch: boolean;

  function DerivationMatches(oclValue: TBoldElement): boolean;
  var
    selfasList, valueAsList: TBoldObjectList;
    i: Integer;
  begin
    if (self is TBoldObjectReference) then
    begin
      if oclValue = nil then
        Result := ((self as TBoldObjectReference).Locator = nil)
      else if oclValue is TBoldObject then
        Result := oclvalue = (self as TBoldObjectReference).BoldObject
      else if oclValue is TBoldObjectReference then
         Result := (oclvalue as TBoldObjectReference).Locator = (self as TBoldObjectReference).Locator
      else
        Result := false;
    end
    else if (self is TBoldObjectList) and (oclValue is TBoldObjectList)  then
    begin
      selfAsList := self as  TBoldObjectList;
      valueAsList := oclValue as  TBoldObjectList;
      if selfAsList.Count <> valueAsList.Count then
        Result := false
      else
      begin
        Result := true;
        for I := 0 to selfAsList.Count - 1 do
          if selfAsList[i] <> valueAsList[i] then
            Result := false;
      end;
    end
    else if IsEqual(oclValue) then
      Result := true
    else if (self is TBAFLoat) and
         (oclValue is TBAFLoat) and
         (abs((self as TBAFLoat).AsFloat - (oclValue as TBAFLoat).asFloat) < 0.011) then
      result := true
    else
      result := false;
  end;
{$ENDIF}
begin
  DeriveMethod.Code := BoldmemberRTInfo.Derivemethod;
  if DeriveMethod.Code = nil then
  begin
    DeriveMethod := TMethod(OwningObject.GetDeriveMethodForMember(Self));
    BoldmemberRTInfo.Derivemethod :=  DeriveMethod.Code;
    if BoldmemberRTInfo.Derivemethod = nil then
      raise EBoldInternal.CreateFmt('Derivation method not found for %s, check model.', [displayName]);
  end
  else
    DeriveMethod.Data := OwningObject;
  TBoldDeriveAndResubscribe(DeriveMethod)(self, Subscriber);
  {$IFDEF EXPRESSION_OVERRIDES_DOUBLE_DERIVE}
  Expression := TExpressionOverride.GetOclDerivationForCodeDerived(self.BoldMemberRTInfo);
  if Expression <> '' then
    begin
      ie := TBoldIndirectElement.Create;
      try
        try
          OwningObject.evaluateAndSubscribeToExpression(Expression, subscriber, ie, false);
          if Assigned(ie.Value) and (ie.value is TBoldmember) then
            TBoldMember(ie.value).EnsureContentsCurrent;
          nonMatch := true;

          if not DerivationMatches(ie.Value) then
            raise EBold.Create(Format('OclDerivationOverride for %s.%s gives %s, code gives %s',
            [BoldMemberRTInfo.ClassTypeInfo.ExpressionName, BoldMemberRTInfo.ExpressionName, ie.Value.AsString, AsString]));
          nonmatch := false;
        except
          on e: Exception do
          begin
            if not nonmatch then
            begin
              if Assigned(OwningObject) then
                s := 'ID:' + OwningObject.BoldObjectLocator.AsString
              else
                s := 'not owned.'
              s := format('Failed to derive %s: %s' + BOLDCRLF +
                          'OCL expression: %s' + BOLDCRLF +
                          'Owner:  %s' + BOLDCRLF +
                          'Error: %s', [DisplayName, BoldType.AsString, Expression, s, e.Message]);
              if assigned(ie.value) and not ie.value.BoldType.conformsto(boldtype) then
                s := format(s + BOLDCRLF+
                            'Possible Reason: %s does not conform to %s', [ie.value.BoldType.AsString, boldtype.AsString]);
              raise EBoldFailedToDerive.Create(s);
            end;
          end;
        end;
      finally
          FreeAndNil(ie);
      end;
    end;
  {$ENDIF}
end;

procedure TBoldMember.CalculateDerivedMemberWithExpression(Subscriber: TBoldSubscriber);
var
  ie: TBoldIndirectElement;
  Expression: string;
  s: string;
begin
  ie := TBoldIndirectElement.Create;
  try
    try
      {$IFDEF EXPRESSION_OVERRIDES}
      Expression := TExpressionOverride.GetOclDerivationOverride(self.BoldMemberRTInfo);
      if Expression = '' then
      {$ENDIF}
      Expression := BoldMemberRTInfo.DeriveExpression;
      OwningObject.evaluateAndSubscribeToExpression(Expression, subscriber, ie, false);
      if Assigned(ie.Value) and (ie.value is TBoldmember) then
        TBoldMember(ie.value).EnsureContentsCurrent;
      AssignContentValueFromElement(ie.Value);
    except
      on e: Exception do
      begin
        s := format(sFailedToDerive + BOLDCRLF +
                    sOCLExpressionError, [DisplayName, BoldType.AsString, OwningObject.BoldObjectLocator.AsString, Expression, BOLDCRLF, e.Message]);
        if assigned(ie.value) and not ie.value.BoldType.conformsto(boldtype) then
        begin
          s := format(s + BOLDCRLF + sPossiblyBadConformance, [ie.value.BoldType.AsString, boldtype.AsString]);
        end;
        raise EBold.Create(s);
      end;
    end;
  finally
    FreeAndNil(ie);
  end;
end;

function TBoldMember.ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean;
begin
  if IsEqualGuid(IID, IBoldValue) or IsEqualGuid(IID, IBoldStreamable) then
    Result := RetrieveProxyInterface(IID, Mode, obj, 'IBoldValue/IBoldStreamable')
  else
    result := inherited ProxyInterface(IID, Mode, Obj);
end;

function TBoldMember.ObserverMayModify(Observer: TObject): Boolean;
begin
  result := inherited ObserverMayModify(Observer) and CanModify;
end;

procedure TBoldMember.GetAsList(ResultList: TBoldIndirectElement);
var
  NewList: TBoldList;
begin
  NewList := TBoldMemberList.Create;
  Assert(BoldType.SystemTypeInfo is TBoldSystemTypeInfo);
  NewList.fBoldMetaType := TBoldSystemTypeInfo(BoldType.SystemTypeInfo).ListTypeInfoByElement[BoldType];
  NewList.Add(Self);
  NewList.MakeImmutable;
  NewList.MakeContentsImmutable;
  ResultList.SetOwnedValue(NewList);
end;

procedure TBoldMember.GetAsValue(ResultElement: TBoldIndirectElement);
begin
  ResultElement.SetOwnedValue(Clone);
end;

function TBoldMember.StartModify: Boolean;
begin
  if not (BoldPersistenceState in [bvpsCurrent, bvpsModified, bvpsTransient, bvpsInvalid]) then
    StateError('StartModify');
  if IsInitializing then
  begin
    result := true;
    exit;
  end;
  result := CanModify;
  if result and assigned(BoldSystem) and assigned(OwningObject) and
     assigned(BoldSystem.PessimisticLockHandler) and
     not BoldMemberRTInfo.IsDerived then
    result := BoldSystem.PessimisticLockHandler.LockElement(self);
  if result then
  begin
    if assigned(OwningObject) and not BoldSystem.InTransaction and StoreInUndo and BoldSystem.UndoHandler.Enabled then
      BoldSystem.UndoHandler.HandleMember(OwningObject.AsIBoldObjectContents[bdepContents], BoldMemberRTInfo.Index, AsIBoldValue[bdepContents]);
    DoStartModify;
  end;
end;


procedure TBoldMember.EndModify;
{$IFDEF CompareToOldValues}
var
  vOldValue: IBoldValue;
  function OrdernoDiffers(const Value: IBoldValue; member: TBoldMember):Boolean;
  begin
    Result := (member is TBoldObjectReference) and
      (TBoldObjectReference(member).GetController is TBoldDirectSingleLinkController) and
      ((Value as IBoldObjectIdRef).OrderNo <>  TBoldDirectSingleLinkController(TBoldObjectReference(member).GetController).OrderNo);
  end;
{$ENDIF}
var
{$IFDEF ACCESSSTATISTICS} //// START PATCH ACCESSSTATISTICS ////////////////////
  G_BoldMemberRTInfo: TBoldMemberRTInfo;
  G_OwningElement: TBoldDomainElement;
  G_BoldSystem: TBoldSystem;
{$ENDIF} //// END PATCH ACCESSSTATISTICS ///////////////////////////////////////
  System: TBoldSystem;
begin
  System := BoldSystem;
{$IFNDEF CompareToOldValues}
  if (BoldPersistenceState in [bvpsCurrent, bvpsInvalid]) and MemberCanBeModified(BoldMemberRTinfo, System) then
    BoldPersistenceState := bvpsModified;
{$ELSE}
  if (BoldPersistenceState <> bvpsTransient) and OwnedByObject and BoldPersistent then
  begin
    vOldValue := OldValue;
    if Assigned(vOldValue) and IsEqualToValue(vOldValue) and not OrdernoDiffers(vOldValue, self) then
    begin
      vOldValue := nil;
      BoldPersistenceState := bvpsCurrent
    end
    else
    begin
      if (BoldPersistenceState <> bvpsModified) and MemberCanBeModified(BoldMemberRTinfo, System) then
        BoldPersistenceState := bvpsModified;
    end;
  end;
{$ENDIF}
  if Derived and not (DeriverState in bdsIsDeriving) then
    Deriver.ReverseDerive;
  if not IsInitializing then
  begin
    CompleteModify;
    SendExtendedEvent(beCompleteModify, [self]);
  end;
  if OwnedByObject then
  begin
    System.AddToTransaction(OwningObject);
    System.AddToTransaction(self);
  end;
  {$IFDEF ACCESSSTATISTICS} //// START PATCH ACCESSSTATISTICS //////////////////
  //The messy code is for getting better performance /FH
  if OwnedByObject then
  begin
    G_BoldMemberRTInfo := TBoldMemberRTInfo(fBoldMetaType);
    G_OwningElement := OwningElement;
    if (G_OwningElement<>nil) then
    begin
      if (G_OwningElement is TBoldObject) then
      begin
        G_BoldSystem := TBoldSystem(TBoldObject(G_OwningElement).OwningElement);
        Inc(G_BoldSystem.fModifyStats[G_BoldMemberRTInfo.ClassTypeInfo.TopSortedIndex, G_BoldMemberRTInfo.index]);
      end
      else
      begin
        Inc(TBoldSystem(G_OwningElement).fModifyStats[G_BoldMemberRTInfo.ClassTypeInfo.TopSortedIndex, G_BoldMemberRTInfo.index]);
      end;
    end;
  end;
  {$ENDIF} //// END PATCH ACCESSSTATISTICS /////////////////////////////////////
end;

procedure TBoldMember.FailModify;
begin
end;

procedure TBoldMember.EndUpdate(Translationlist: TBoldIdTranslationlist);
begin
  AdjustOldValues(Translationlist);
  BoldPersistenceState := bvpsCurrent;
end;

procedure TBoldMember.AdjustOldValues(Translationlist: TBoldIdTranslationlist);
begin
end;

function TBoldMember.GetBoldDirty: Boolean;
begin
  result := BoldPersistenceState = bvpsModified;
end;

function TBoldMember.MayUpdate: Boolean;
begin
  result := True;
end;

function TBoldMember.MayModify: Boolean;
begin
  result := True;
end;

procedure TBoldMember.PrepareModify;
begin
end;

procedure TBoldMember.CompleteModify;
begin
end;

procedure TBoldMember.CompleteUpdate;
begin
end;


procedure TBoldMember.PrepareUpdate;
begin
end;

procedure TBoldMember.DoStartModify;
begin
  if assigned(OwningObject) then
    OwningObject.BoldSystem.fOldValueHandler.MemberValuePreChange(self);
{$IFNDEF NoPrepareModify}
  PrepareModify;
  SendEvent(bePrepareModify);
{$ENDIF}
  if assigned(BoldMemberRTInfo) and BoldMemberRTInfo.ToBeRemoved and assigned(OwningObject) then
    OwningObject.ToBeRemovedMemberModified(BoldMemberRTInfo);
end;

function TBoldMember.CanModify: Boolean;
begin
  result := Mutable;
  if not result then
  begin
    SetBoldLastFailureReason(TBoldFailureReason.Create(sMemberIsImmutable, self));
    exit;
  end;

  if assigned(OwningObject) and OwningObject.BoldObjectIsDeleted and not Derived and not OwningObject.Discarding then
  begin
    Result := false;
    SetBoldLastFailureReason(TBoldFailureReason.CreateFmt(sCannotModifyMember, [DisplayName], self));
    exit;
  end;

  result := not IsReadOnly;
  if not result then
  begin
    SetBoldLastFailureReason(TBoldFailureReason.Create(sMemberIsreadOnly, self));
    exit;
  end;

  if Derived then
     Result := (DeriverState in bdsIsDeriving) or (Deriver.CanReverseDerive);

  if not result then
  begin
    SetBoldLastFailureReason(TBoldFailureReason.Create(sMemberIsreadOnlyDerived, self));
    exit;
  end;

  if assigned(OwningObject) then
  begin
    if OwningObject.IsHistoricVersion then
    begin
      Result := false;
      SetBoldLastFailureReason(TBoldFailureReason.Create(sMemberIsHistory, self));
      exit;
    end;
  end;

  result := result and MayModify;
{$IFNDEF BOLD_NO_QUERIES}
    result := result and SendQuery(bqMayModify, [], nil);
{$ENDIF}
end;

function TBoldMember.CanUpdate: Boolean;
begin
  result := InternalMayUpdate and MayUpdate;
{$IFNDEF BOLD_NO_QUERIES}
    result := result and SendQuery(bqMayUpdate, [], nil);
{$ENDIF}
end;

procedure TBoldMember.DoMarkTouched;
begin
  SetElementFlag(befTouched, true);
  if assigned(owningObject) then
    OwningObject.SetElementFlag(befTouched, true);
end;

procedure TBoldMember.EnsureContentsCurrent;
begin
  if (StateAndFlagBank and BoldPersistenceStateMask) = (Cardinal(bvpsInvalid) shl BoldPSShift) then
    DoEnsureContentsCurrent;
  if not GetElementFlag(befTouched) then
    DoMarkTouched;
{$IFNDEF NoAutoSubscription}
  if IsPartOfSystem and BoldSystem.IsDerivingMembers and (BoldSystem.CurrentDerivedMember <> self) and Mutable and not BoldSystem.IsCommitting then
    BoldSystem.fMembersReadDuringDerivation.Add(self);
{$ENDIF}
end;

procedure TBoldMember.DoEnsureContentsCurrent;
{$IFDEF ACCESSSTATISTICS} //// START PATCH ACCESSSTATISTICS ////////////////////
var
  G_BoldMemberRTInfo: TBoldMemberRTInfo;
  G_OwningElement: TBoldDomainElement;
  G_BoldSystem: TBoldSystem;
{$ENDIF} //// END PATCH ACCESSSTATISTICS ///////////////////////////////////////
begin
  if not GetElementFlag(befEnsuringCurrent) then
  begin
    if Derived then
    begin
    {$IFDEF SpanFetch}
      if IsPartOfSystem and not IsPrefetched then
      begin
        AttracsSpanFetchManager.PrefetchDerivedMember(self);
        IsPrefetched := true;
      end;
    {$ENDIF}
    end;

    SetElementFlag(befEnsuringCurrent, true);
    try
      if Derived then
      begin
        if DeriverState <> bdsCurrent then
        begin
          Deriver.EnsureCurrent;
          {$IFDEF ACCESSSTATISTICS} //// START PATCH ACCESSSTATISTICS //////////
          //The messy code is for getting better performance /FH
          if OwnedByObject then
          begin
            G_BoldMemberRTInfo := TBoldMemberRTInfo(fBoldMetaType);
            G_OwningElement := OwningElement;
            if (G_OwningElement<>nil) then
            begin
              if (G_OwningElement is TBoldObject) then
              begin
                G_BoldSystem := TBoldSystem(TBoldObject(G_OwningElement).OwningElement);
                Inc(G_BoldSystem.fDeriveStats[G_BoldMemberRTInfo.ClassTypeInfo.TopSortedIndex, G_BoldMemberRTInfo.index]);
              end
              else
              begin
                Inc(TBoldSystem(G_OwningElement).fDeriveStats[G_BoldMemberRTInfo.ClassTypeInfo.TopSortedIndex, G_BoldMemberRTInfo.index]);
              end;
            end;
          end;
          {$ENDIF} //// END PATCH ACCESSSTATISTICS /////////////////////////////
        end;
        if not (DeriverState in bdsIsDeriving) then
          BoldPersistenceState := bvpsTransient;
      end
      else
        MakeDbCurrent;
    finally
      SetElementFlag(befEnsuringCurrent, false);
    end;
  end;
  {$IFDEF ACCESSSTATISTICS} //// START PATCH ACCESSSTATISTICS //////////////////
  //The messy code is for getting better performance /FH
  if OwnedByObject then
  begin
    G_BoldMemberRTInfo := TBoldMemberRTInfo(fBoldMetaType);
    G_OwningElement := OwningElement;
    if (G_OwningElement<>nil) then
    begin
      if (G_OwningElement is TBoldObject) then
      begin
        G_BoldSystem := TBoldSystem(TBoldObject(G_OwningElement).OwningElement);
        Inc(G_BoldSystem.fAccessStats[G_BoldMemberRTInfo.ClassTypeInfo.TopSortedIndex, G_BoldMemberRTInfo.index]);
      end
      else
      begin
        Inc(TBoldSystem(G_OwningElement).fAccessStats[G_BoldMemberRTInfo.ClassTypeInfo.TopSortedIndex, G_BoldMemberRTInfo.index]);
      end;
    end;
  end;
  {$ENDIF} //// END PATCH ACCESSSTATISTICS /////////////////////////////////////

end;

procedure TBoldMember.Invalidate;
var
  aLinkMember: TBoldMember;
{$IFDEF ACCESSSTATISTICS} //// START PATCH ACCESSSTATISTICS ////////////////////
  G_BoldMemberRTInfo: TBoldMemberRTInfo;
  G_OwningElement: TBoldDomainElement;
  G_BoldSystem: TBoldSystem;
{$ENDIF} //// END PATCH ACCESSSTATISTICS ///////////////////////////////////////
begin
  if not (BoldPersistenceState in [bvpsCurrent, bvpsInvalid, bvpsTransient]) then
    StateError('Invalidate');

  if assigned(BoldMemberRTInfo) and
    not BoldMemberRTInfo.IsDerived and
   (BoldPersistenceState = bvpsTransient) then
    raise EBold.CreateFmt(sCannotInvalidateTransient, [classname, displayname]);

  if BoldPersistenceState in [bvpsCurrent, bvpsTransient] then
  begin
    if not Derived and OwnedByObject and OwningObject.BoldObjectIsNew then
      raise EBold.CreateFmt('%s.Invalidate: Can''t invalidate member of a new unsaved object: %s', [classname, displayname]);
    BoldPersistenceState := bvpsInvalid;
    if HasDeriver then
      Deriver.MarkOutOfdate;
    FreeContent;
    SendEvent(beValueInvalid);
    {$IFDEF ACCESSSTATISTICS} //// START PATCH ACCESSSTATISTICS //////////////////
    //The messy code is for getting better performance /FH
    if OwnedByObject then
    begin
      G_BoldMemberRTInfo := TBoldMemberRTInfo(fBoldMetaType);
      G_OwningElement := OwningElement;
      if (G_OwningElement<>nil) then
      begin
        if (G_OwningElement is TBoldObject) then
        begin
          G_BoldSystem := TBoldSystem(TBoldObject(G_OwningElement).OwningElement);
          Inc(G_BoldSystem.fInvalidateStats[G_BoldMemberRTInfo.ClassTypeInfo.TopSortedIndex, G_BoldMemberRTInfo.index]);
        end
        else
        begin
          Inc(TBoldSystem(G_OwningElement).fInvalidateStats[G_BoldMemberRTInfo.ClassTypeInfo.TopSortedIndex, G_BoldMemberRTInfo.index]);
        end;
      end;
    end;
    {$ENDIF} //// END PATCH ACCESSSTATISTICS /////////////////////////////////////
    ///
    // n:n-associations: always invalidate LinkRole with MainRole.
    // On OSS-Sync for example only the MainRole gets invalid.
    // Likewise in TBoldObject.Invalidate, because the LinkRole is not persistent.
    if (BoldMemberRTInfo is TBoldRoleRTInfo) and
       (TBoldRoleRTInfo(BoldMemberRTInfo).IndexOfLinkObjectRole <> -1) then
    begin
      aLinkMember := OwningObject.BoldMembers[TBoldRoleRTInfo(BoldMemberRTInfo).IndexOfLinkObjectRole];
      if (aLinkMember.BoldPersistenceState <> bvpsModified) and
         ((aLinkMember.BoldPersistenceState <> bvpsTransient) or
          (aLinkMember.BoldMemberRTInfo.IsDerived)) then
      begin
        aLinkMember.Invalidate;
      end;
    end;
  end;
end;

function TBoldMember.CanRead(Subscriber: TBoldSubscriber): Boolean;
begin
  result := true;
//  result := SendQuery(bqMayRead, [], Subscriber);
end;

procedure TBoldMember.SetBoldPersistenceState(
  Value: TBoldValuePersistenceState);
var
  OldPState: TBoldValuePersistenceState;
  OwningObj: TBoldObject;
  rtInfo: TBoldMemberRtInfo;
begin
  OldPState := BoldPersistenceState;
  if Value <> OldPState then
  begin
    OwningObj := OwningObject;
    RtInfo :=  BoldMemberRTInfo;
    if Assigned(OwningObj) then
      OwningObj.BoldSystem.fOldValueHandler.MemberPersistenceStatePreChange(self, Value);
    SetInternalState(BoldPersistenceStateMask, BoldPSShift, integer(value));
    if Assigned(OwningObj) and (Value = bvpsInvalid) and RtInfo.IsSingleRole and (not Derived) and (not TBoldObjectReference(self).BoldRoleRTInfo.IsIndirect) then
    begin
      if OwningObj.Discarding then
        TBoldObjectReference(Self).HasOldValues := False
      else
//  Original Bold code was:
//      TBoldObjectReference(self).HasOldValues := ((TBoldObjectReference(self).fObjectReferenceController as TBoldDirectSingleLinkController).GetLocator <> nil);
// But this assert can fail:
//     Assert(TBoldObjectReference(self).fObjectReferenceController is TBoldDirectSingleLinkController, TBoldObjectReference(self).fObjectReferenceController.CLassName );
// Controller can also be TBoldLinkObjectReferenceController instead of TBoldDirectSingleLinkController. Both descend from TBoldAbstractObjectReferenceController
// It is not clear if intention here was to only handle TBoldDirectSingleLinkController, or it handle all TBoldAbstractObjectReferenceController descendants ?
// TBoldAbstractObjectReferenceController is used, but needs to be tested
      TBoldObjectReference(self).HasOldValues := ((TBoldObjectReference(self).fObjectReferenceController as TBoldAbstractObjectReferenceController).GetLocator <> nil);
    end;
    if not derived and assigned(OwningObj) then
    begin
      if (value = bvpsInvalid) or (OldPState = bvpsInvalid) then
        OwningObj.MemberChangingValidity(RtInfo, value);
      if Value = bvpsModified then
        OwningObj.MemberBecomingModified(self)
      else if OldPState = bvpsModified then
        OwningObj.MemberBecomingClean(self);
    end;
  end;
end;

procedure TBoldMember.MakeDbCurrent;
begin
end;

function TBoldMember.GetDeriver: TBoldMemberDeriver;
begin
  Result := OwningObject.GetBoldMemberDeriver(self)
end;

procedure TBoldMember._NotifyOutOfDate;
begin
  Invalidate;
end;

procedure TBoldMember.StateError(S: String);
var
  MemberName: String;
begin
  if Assigned(BoldMemberRTInfo) then
     Membername := '(' + BoldMemberRTInfo.ExpressionName + ') '
  else
     Membername := '';
  inherited StateError(format('%s %s(PersistenceState: %s)',
    [s, MemberName,
    GetEnumName(TypeInfo(TBoldValuePersistenceState), Ord(BoldPersistenceState))]));
end;

function TBoldMember.GetEvaluator: TBoldEvaluator;
begin
  if IsPartOfSystem then
    result := BoldSystem.Evaluator
  else
    result := inherited GetEvaluator;
end;

function TBoldMember.GetBoldType: TBoldElementTypeInfo;
begin
  if OwnedByObject then
  begin
    assert(fBoldMetaType is TBoldMemberRtInfo);
    result := TBoldMemberRTInfo(fBoldMetaType).BoldType;
  end
  else
  begin
    assert(not assigned(fBoldMetaType) or (fBoldMetaType is TBoldElementTypeInfo));
    result := TBoldElementTypeInfo(fBoldMetaType);
  end;
end;

procedure TBoldMember.AssignCloneValue(AClone: TBoldMember);
begin
  AClone.AssignContentValueFromElement(self);
end;

function TBoldMember.Clone: TBoldMember;
begin
  if assigned(BoldType) then
    Result := TBoldMemberFactory.CreateMemberFromBoldType(BoldType)
  else
    result := TBoldMemberClass(classtype).Create;
  try
    AssignCloneValue(result);
    Result.BoldPersistenceState := bvpsTransient;
  except
    Result.Free;
    raise;
  end;
end;

procedure TBoldMember.Initialize;
begin
end;

procedure TBoldMember.InitializeNonObjectOwned(ElementTypeInfo: TBoldElementTypeInfo);
begin
  SetElementFlag(befPersistent, false);
  if assigned(ElementTypeInfo) then
    fBoldMetaType := ElementTypeInfo
  else
    fBoldMetaType := GetElementTypeInfoForType;
  Initialize;
end;

constructor TBoldMember.Create;
begin
  inherited CreateWithOwner(nil);
  InitializeNonObjectOwned(nil);
end;

procedure TBoldMember.InternalDiscard;
{$IFNDEF NoAutoSubscription}
  procedure ClearDerivationReferences;
  var
    i: integer;
  begin
    // Clear any references to member from the list of members acccesed during derivation
    with BoldSystem.fMembersReadDuringDerivation  do
      for i := 0 to Count - 1 do
        if (Items[i] <> nil) and (TBoldMember(Items[i]) = self) then
          Items[i] := nil;
  end;
{$ENDIF}
begin
  if BoldPersistenceState in [bvpsModified, bvpsTransient] then
  begin
    PreDiscard;
{$IFNDEF NoAutoSubscription}
    if Assigned(BoldSystem) and BoldSystem.IsDerivingMembers then
      ClearDerivationReferences;
{$ENDIF}
    case BoldPersistenceState of
      bvpsModified: begin
        if not OwningObject.BoldObjectLocator.BoldObjectID.IsStorable then
          BoldPersistenceState := bvpsCurrent
        else
          BoldPersistenceState := bvpsInvalid;
        FreeContent;
        SendEvent(beValueInvalid);
      end;
      bvpsTransient: if not Derived and mutable and not OwningObject.Discarding then
        DoSetInitialValue;
    end;
  end;
end;

procedure TBoldMember.Discard;
begin
  if OwningObject.BoldObjectLocator.BoldObjectID.IsStorable then
    InternalDiscard
  else
    raise EBold.CreateFmt(sCannotDiscardUnsavedSingleLinks, [ClassName]);
end;

procedure TBoldMember.PreDiscard;
begin
end;

function TBoldMember.GetAsIBoldValue(Mode: TBoldDomainElementProxyMode): IBoldValue;
begin
  self.ProxyInterface(IBoldValue, Mode, result);
end;

function TBoldMember.GetController: TBoldAbstractController;
begin
  result := nil;
end;

procedure TBoldMember.MarkMemberDirty;
begin
  BoldPersistenceState := bvpsModified;
end;

procedure TBoldMember.ObjectBecomingPersistent;
begin
  assert(assigned(BoldMemberRTInfo));
  if BoldMemberRTInfo.Persistent then
  begin
    SetElementFlag(befPersistent, True);
    BoldPersistenceState := bvpsModified;
  end;
end;

function TBoldMember.InternalMayUpdate: Boolean;
begin
  result := True;
end;

procedure TBoldMember.PreChange;
begin
{$IFNDEF NoObjectSpaceTransactions}
  if assigned(OwningObject) and
      not BoldMemberRTInfo.IsDerived
      and not IsInitializing
      and not BoldSystem.IsUpdatingDatabase
      and not BoldSystem.IsRollingBack
      and not BoldSystem.IsFetching then
    BoldSystem.CopyMemberToRollbackBuffer(self);
{$ENDIF}
end;

procedure TBoldMember.DoSetInitialValue;
begin

end;

function TBoldMember.AtTime(Time: TBoldTimestampType): TBoldMember;
begin
  if assigned(OwningObject) then
    result := OwningObject.AtTime(Time).BoldMembers[BoldMemberRTInfo.index]
  else
    result := self;
end;

procedure TBoldMember.FreeContent;
begin
end;

function TBoldMember.CloneIfPossible: TBoldElement;
begin
  Result := Clone;
end;

function TBoldMember.RetrieveProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode;
  out Obj; const InterfaceName: string): Boolean;
begin
  result := GetProxy(Mode).GetInterface(IID, obj);
  if not result then
    raise EBoldInternal.CreateFmt(sProxyClassDidNotImplementInterface, [ClassName, InterfaceName]);
end;

procedure TBoldMember.ReverseDeriveMember;
var
  ReverseDeriveMethod: TMethod;
  InTransaction: boolean;
begin
  InTransaction := BoldSystem.InTransaction;
  if not InTransaction then
    BoldSystem.StartTransaction();
  try
  ReverseDeriveMethod.Code := BoldmemberRTInfo.ReverseDeriveMethod;
  if ReverseDeriveMethod.Code = nil then
  begin
    ReverseDeriveMethod := TMethod(OwningObject.GetReverseDeriveMethodForMember(Self));
    BoldmemberRTInfo.ReverseDeriveMethod :=  ReverseDeriveMethod.Code;
  end
  else
    ReverseDeriveMethod.Data := OwningObject;
  TBoldReverseDerive(ReverseDeriveMethod)(self);
    if not InTransaction then
      BoldSystem.CommitTransaction();
  except
    if not InTransaction then
      BoldSystem.RollbackTransaction();
    raise;
  end;
end;

procedure TBoldMember.AssignContentValueFromElement(source: TBoldElement);
begin
  if not assigned(source) then
  else if source is TBoldMember then
    AsIBoldValue[bdepContents].AssignContent(TBoldMember(source).AsIBoldValue[bdepContents])
  else
    raise EBold.CreateFmt(sUnknownTypeOfSource + ' %s', [classname, 'AssignContentValuefromElement', Source.BoldType.AsString]); // do not localize
end;

function TBoldMember.GetOldValue: IBoldValue;
var
  ObjectContents: IBoldObjectContents;
begin
  result := nil;
  if OwnedByObject and not OwningObject.BoldObjectIsNew then
  begin
    ObjectContents := BoldSystem.fOldValueHandler.OldValues.GetObjectContentsByObjectId(OwningObject.BoldObjectLocator.BoldObjectID) as IBoldObjectContents;
    if assigned(ObjectContents) then
    begin
      result := ObjectContents.ValueByIndex[BoldMemberRTInfo.index];
    end;
  end;
end;

function TBoldMember.MemberHasSubscribers: Boolean;
begin
  Result := HasSubscribers;
end;

function TBoldMember.StoreInUndo: Boolean;
begin
  Result := Assigned(BoldMemberRTinfo) and BoldMemberRTinfo.StoreInUndo;
end;

function TBoldMember.IsEqualToValue(const Value: IBoldValue): Boolean;
begin{ TODO : Make part of IBoldValue }
  raise Ebold.Create(sIsEqualToValueNotImplemented);
end;

function TBoldAttribute.IsEqualToValue(const Value: IBoldValue): Boolean;
var
  MemberOfSameType: TBoldMember;
  G: IBoldGuard;
{$IFDEF StringAttributeComparison}
  s: IBoldStringRepresentable;
{$ENDIF}
begin
  Assert(Assigned(Value), ClassName + '.IsEqualToValue: Value can not be nil.');
{$IFDEF StringAttributeComparison}
  if Value.QueryInterface(IBoldStringRepresentable, S) = S_OK then
    result := StringCompare(ctDefault, Self.AsString, s.StringRepresentation[brDefault]) = 0
  else
{$ENDIF}
  begin{ TODO : Make part of IBoldValue }
  // Messy
    G := TBoldGuard.Create(MemberOfSameType);
    if assigned(BoldType) then
      MemberOfSameType := TBoldMemberFactory.CreateMemberFromBoldType(BoldType)
    else
      MemberOfSameType := TBoldMemberClass(classtype).Create;
    MemberOfSameType.AsIBoldValue[bdepContents].AssignContent(Value);
    MemberOfSameType.BoldPersistenceState := bvpsTransient;
    Result := IsEqual(MemberOfSameType);
  end;
end;

function TBoldAttribute.IsVariantTypeCompatible(const Value: Variant): Boolean;
begin
  result := true;
end;

function TBoldObjectReference.IsEqualToValue(const Value: IBoldValue): Boolean;
var
  IdRef: IBoldObjectIdRef;
  IdRefPair: IBoldObjectIdRefPair;
  ValueId : TBoldObjectId;
begin
  Assert(Assigned(Value), ClassName + '.IsEqualToValue: Value can not be nil.');
  if Supports(Value, IBoldObjectIdRef,IdRef) then
    ValueId := Idref.id
  else if Supports(Value, IBoldObjectIdRefPair,IdRefPair) then
  begin
    if (BoldRoleRTInfo.RoleType = rtRole) and Assigned(BoldRoleRTInfo.LinkClassTypeInfo) then
      ValueId := IdRefPair.Id2
    else
      ValueId := IdRefPair.Id1
  end
  else
    raise EBold.Create('Internal error');
  if Assigned(Locator) and Assigned(ValueId) then
    Result := Locator.BoldObjectID.IsEqual[ValueId]
  else
    Result := (Locator = nil) and (ValueId = nil);
end;

procedure TBoldMember.Refetch;
begin
  MakeDbCurrent;
end;

{ TBoldAttribute }

procedure TBoldAttribute.DefaultSubscribe(Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent);
begin
  if mutable then
    AddSmallSubscription(Subscriber, [beValueChanged, beValueInvalid], RequestedEvent);
end;

function TBoldAttribute.GetIsNull: Boolean;
begin
  EnsureContentsCurrent;
  BoldClearLastFailure;
  if not CanRead(nil) then
    BoldRaiseLastFailure(self, 'GetIsNull', '');
  Result := ContentIsNull;
end;

procedure TBoldAttribute.SetToNull;
begin
  BoldClearLastFailure;
  if not CanSetToNull(nil) then
    BoldRaiseLastFailure(self, 'SetToNull', '');

  if not IsNull then
  begin
    if not StartModify then
      BoldRaiseLastFailure(self, 'SetToNull', sStartModifyPreconditionNotMet); // do not localize
    try
      SetContentToNull;
      EndModify;
    except
      FailModify;
      raise;
    end;
  end;
end;


function UnicodeCompareLen(CaseSensitive: Boolean; s1, s2 : string; n : Integer) : Integer;
const
   CSTR_EQUAL = 2;
begin
  if CaseSensitive then
    Result:=CompareStringEx(nil, 0, PWideChar(s1), n, PWideChar(s2), n, nil, nil, 0)-CSTR_EQUAL
  else
    Result:=CompareStringEx(nil, NORM_IGNORECASE, PWideChar(s1), n, PWideChar(s2), n, nil, nil, 0)-CSTR_EQUAL
end;

function UnicodeCompareText(CaseSensitive: Boolean; const s1, s2 : UnicodeString) : Integer;
var
   n1, n2, dn : Integer;
begin
   if S1<>'' then begin
      if S2<>'' then begin
         n1:=Length(s1);
         n2:=Length(s2);
         dn:=n1-n2;
         if dn<0 then begin
            Result:=UnicodeCompareLen(CaseSensitive, s1, s2, n1);
            if Result=0 then
               Result:=-1;
         end else begin
            Result:=UnicodeCompareLen(CaseSensitive, S1, s2, n2);
            if (Result=0) and (dn>0) then
               Result:=1;
         end;
      end else Result:=1;
   end else if S2<>'' then
      Result:=-1
   else Result:=0;
end;

function TBoldAttribute.StringCompare(CompareType: TBoldCompareType; const s1, s2: string): integer;
begin
  case CompareType of
    ctDefault, ctCaseInsensitive:
      Result := UnicodeCompareText(false, s1, s2);
    ctAsString, ctCaseSensitive:
      Result := UnicodeCompareText(true, s1, s2);
  else
    raise EBold.CreateFmt('%s.StringCompare Unsupported CompareType %s in StringCompare.', [ClassName, GetEnumName(TypeInfo(TBoldCompareType), Ord(CompareType))]);
  end;
end;

function TBoldAttribute.CanSetToNull(Subscriber: TBoldSubscriber): Boolean;
begin
  result := ((not Assigned(BoldAttributeRTInfo)) or BoldAttributeRTInfo.AllowNull);
{$IFNDEF BOLD_NO_QUERIES}
  Result := result and SendQuery(bqMaySetToNull, [], Subscriber);
{$ENDIF}
  if not result then
    nullfailure;
end;

function TBoldAttribute.ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean;
begin
  if IsEqualGuid(IID, IBoldNullableValue) then
    Result := RetrieveProxyInterface(IID, Mode, obj, 'IBoldNullableValue')
  else
    result := inherited ProxyInterface(IID, Mode, Obj);
end;

procedure TBoldAttribute.SubscribeToStringRepresentation(Representation: TBoldRepresentation; Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent);
begin
  DefaultSubscribe(Subscriber, RequestedEvent);
end;

function TBoldAttribute.ValidateVariant(const Value: Variant;
  Representation: TBoldRepresentation): Boolean;
begin
  result := true;
  if VarIsNull(Value) then
    result := CanSetToNull(nil)
  else if not IsVariantTypeCompatible(Value) then
    result := inherited ValidateVariant(Value, Representation);
end;

procedure TBoldAttribute.SetToNonNull;
begin
  SetElementFlag(befIsNull, False);
end;

procedure TBoldAttribute.EnsureNotNull;
begin
  if IsNull then
  begin
{$IFNDEF NoNilAttributeExceptions}
    if OwnedByObject then
      raise EBoldAccessNullValue.CreateFmt(sNullAccessWithOwner, [DisplayName, OwningObject.BoldObjectLocator.AsString])
    else
      raise EBoldAccessNullValue.CreateFmt(sNullValueAccess, [DisplayName]);
{$ENDIF}
  end;
end;

procedure TBoldAttribute.MakeDbCurrent;
begin
  if OwningObject.BoldObjectLocator.BoldObjectID.IsStorable then
    OwningObject.BoldSystem.SystemPersistenceHandler.FetchMember(self)
  else
    raise EBoldInternal.CreateFmt(sObjectIDIsInternal, [BoldType.AsString]);
end;

class function TBoldAttribute.EitherIsNull(Attribute1, Attribute2: TBoldAttribute): Boolean;
begin
  Result := Attribute1.IsNull or Attribute2.IsNull;
end;

function TBoldAttribute.NullBiggest(BoldElement: TBoldElement): Integer;
begin
  Result := -1 * NullSmallest(BoldElement);
end;

function TBoldAttribute.NullSmallest(BoldElement: TBoldElement): Integer;
begin
  if BoldElement is TBoldAttribute then
  begin
    if not (IsNull xor TBoldAttribute(BoldElement).IsNull) then
      Result := 0
    else if IsNull then
      Result := -1
    else
      Result := 1;
  end
  else
  begin
    Result := 0;
    CompareError(BoldElement);
  end;
end;

function TBoldAttribute.CompareToAs(CompType: TBoldCompareType; BoldElement: TBoldElement): Integer;
begin
  Result := 0;
  CompareTypeError(CompType, BoldElement);
end;

function TBoldMember.GetElementTypeInfoForType: TBoldElementTypeInfo;
begin
  result := nil;
end;

function TBoldMember.FindASystem: TBoldSystem;
begin
  if IsPartOfSystem then
    result := BoldSystem
  else
    result := TBoldSystem.DefaultSystem;
end;

procedure TBoldMember.Changed(Event: TBoldEvent;
  const Args: array of const);
begin
  if not IsInvalid and not IsInitializing then //setting initial value should not send events
    SendExtendedEvent(Event, Args);
end;

function TBoldMember.IsInvalid: Boolean;
begin
  result := BoldPersistenceState = bvpsInvalid;
end;

function TBoldMember.GetIsCurrent: Boolean;
begin
  result := BoldPersistenceState = bvpsCurrent;
end;

function TBoldMember.GetDisplayName: String;
begin
  if assigned(BoldMemberRTInfo) then
    result := BoldMemberRTInfo.ExpressionName
  else
    result := inherited GetDisplayName; // ClassName;
  if OwnedByObject then
    result := OwningObject.DisplayName + '.' + result;
end;

procedure TBoldAttribute.NullFailure;
var
  CurrentFailureReason: TBoldFailureReason;
begin
  CurrentFailureReason := GetBoldLastFailureReason;
  if not assigned(CurrentFailureReason) then
    SetBoldLastFailureReason(TBoldFailureReason.Create('Null value not allowed', self));
end;

procedure TBoldAttribute.FormatFailure(const value, ExpectedDataType: String);
begin
  SetBoldLastFailureReason(TBoldFailureReason.CreateFmt(sFailure_Invalid, [value, expectedDataType], self));
end;

function TBoldAttribute.GetAsVariant: Variant;
begin
  if IsNull then
    Result := Null
  else
    Result := inherited GetAsVariant;
end;

procedure TBoldAttribute.SetAsVariant(const Value: Variant);
begin
  if VarIsNull(Value) then
    SetToNull
  else
    inherited;
end;

procedure TBoldAttribute.SetContentToNull;
begin
  if not ContentIsNull then
  begin
    PreChange;
{$IFDEF NoNilAttributeExceptions}
    FreeContent;
{$ENDIF}
    SetElementFlag(befIsNull, True);
    Changed(beValueChanged, []);
  end;
end;

procedure TBoldAttribute.DoSetInitialValue;
var
  InitialValue: string;
begin
  if Assigned(BoldAttributeRTInfo) then
  begin
    if BoldAttributeRtInfo.HasInitialValue then
    begin
      InitialValue := BoldAttributeRTInfo.InitialValue;
      if CompareText(InitialValue, '<NULL>') = 0 then
        AssignContentValue(nil)
      else
        try
          AsString := InitialValue;
        except
          on e: Exception do
          begin
            raise EBold.CreateFmt(sIllegalInitialValue, [InitialValue, BoldAttributeRTInfo.AsString, BOLDCRLF, e.message]);
          end;
        end;
    end
    else
    begin
      SetEmptyValue;
    end;
  end;
end;

function TBoldAttribute.GetStreamName: string;
begin
  Assert(Assigned(BoldMemberRTInfo));
  Result := BoldMemberRTInfo.StreamName;
end;

procedure TBoldAttribute.Assign(Source: TBoldElement);
begin
  if not assigned(Source) then
  begin
    if not assigned(BoldAttributeRTInfo) or BoldAttributeRTInfo.allowNull then
      SetToNull
    else
      SetEmptyValue;
  end
  else
    inherited assign(Source);
end;

procedure TBoldAttribute.SetEmptyValue;
begin
  raise Ebold.CreateFmt('%s.SetEmptyValue: Operation not implemented', [classname]);
end;

{ TBoldObjectReference }

destructor TBoldObjectReference.Destroy;
begin
  PrepareToDestroy;
  FreeAndNil(fObjectReferenceController);
  inherited Destroy;
end;

procedure TBoldObjectReference.DefaultSubscribe(Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent);
begin
  if mutable then
    AddSmallSubscription(Subscriber, [beValueChanged, beValueInvalid], RequestedEvent);
end;

function TBoldObjectReference.GetIsEmpty: boolean;
begin
  result := not Assigned(Locator);
end;

function TBoldObjectReference.GetLocator: TBoldObjectLocator;
begin
  EnsureContentsCurrent;
  BoldClearLastFailure;
  if not CanRead(nil) then
    BoldRaiseLastFailure(self, 'GetLocator', '');
  Result := ReferenceController.GetLocator;
end;

function TBoldObjectReference.GetBoldObject: TBoldObject;
var
  aLocator: TBoldObjectLocator;
begin
  aLocator := Locator; {CanRead, EnsureContentsCurrent called by GetLocator}
  if not Assigned(aLocator) or aLocator.BoldObjectID.NonExisting then
    Result := nil
  else
  begin
    aLocator.EnsureBoldObject;
    Result := aLocator.BoldObject;
  end;
end;

function TBoldObjectReference.GetController: TBoldAbstractController;
begin
  result := fObjectReferenceController;
end;

procedure TBoldObjectReference.SetLocator(NewLocator: TBoldObjectLocator);
begin
  EnsureContentsCurrent;
  InternalSetLocator(NewLocator);
end;

procedure TBoldObjectReference.InternalSetLocator(NewLocator: TBoldObjectLocator);
begin
  if (NewLocator <> ReferenceController.GetLocator) then
  begin
    BoldClearLastFailure;
    if not CanSetLocator(NewLocator, nil) then
      BoldRaiseLastFailure(self, 'SetLocator', '');
    ReferenceController.SetLocator(NewLocator);
  end;
end;

procedure TBoldObjectReference.SetBoldObject(NewObject: TBoldObject);
begin
  if Assigned(NewObject) then
    Locator := NewObject.BoldObjectLocator
  else
    Clear;
end;

procedure TBoldObjectReference.Clear;
begin
  if not Assigned(Locator) then
    exit;
  BoldClearLastFailure;
  if not canClear(nil) then
    BoldRaiseLastFailure(self, 'Clear', '');
  Locator := nil;
end;

function TBoldObjectReference.GetStringRepresentation(Representation: TBoldRepresentation): string;
begin
//  if Representation = brJson then
//    Result := TBoldJSONwriter.BoldElementToJsonString(Self)
//  else
  if Assigned(BoldObject) then
    Result := BoldObject.GetStringRepresentation(Representation)
  else
    Result := '';
end;

procedure TBoldObjectReference.SetStringRepresentation(Representation: TBoldRepresentation; const Value: string);
begin
  if Assigned(BoldObject) then
    BoldObject.SetStringRepresentation(Representation, Value);
end;

function TBoldObjectReference.IsEqualAs(CompareType: TBoldCompareType; BoldElement: TBoldElement): Boolean;
var
  CompareElement: TBoldElement;
begin
  CompareElement := BoldElement;
  if CompareElement is TBoldObjectReference then
    CompareElement := TBoldObjectReference(CompareElement).BoldObject;

  if assigned(BoldObject) then
  begin
    case CompareType of
      ctDefault: Result := BoldObject.IsEqualAs(CompareType, BoldElement);
    else
      raise EBold.CreateFmt('%s.IsEqualAs: Unknown Comparetype', [ClassName]);
    end;
  end
  else
    result := not assigned(CompareElement);
end;

procedure TBoldObjectReference.GetAsList(ResultList: TBoldIndirectElement);
var
  NewList: TBoldObjectList;
begin
  Assert(BoldType.SystemTypeInfo is TBoldSystemTypeInfo);
  NewList := TBoldObjectList.CreateWithTypeInfo(BoldType.ListTypeInfo);
  if Assigned(Locator) and Assigned(Locator.BoldObjectID) then
  begin
    NewList.Capacity := 1;
    NewList.ObjectListController.AddLocator(Locator);
  end;
  NewList.MakeImmutable;
  ResultList.SetOwnedValue(NewList);
end;

function TBoldObjectReference.ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean;
begin
  assert(assigned(ReferenceController));
  result := ReferenceController.ProxyInterface(IId, Mode, Obj);
  if not result then
    result := inherited ProxyInterface(IId, Mode, Obj);
end;

procedure TBoldObjectReference.SubscribeToStringRepresentation(
  Representation: TBoldRepresentation; Subscriber: TBoldSubscriber;
  RequestedEvent: TBoldEvent = breReEvaluate);
begin
  if mutable then
    AddSmallSubscription(Subscriber, [beValueChanged], breReSubscribe);
  if Assigned(BoldObject) then
    BoldObject.SubscribeToStringRepresentation(Representation, Subscriber, RequestedEvent)
end;

function TBoldObjectReference.GetStreamName: String;
begin
  Result := ReferenceController.StreamName;
end;

function TBoldObjectReference.GetFreeStandingClass: TBoldFreeStandingElementClass;
begin
  result := ReferenceController.GetFreeStandingClass;
end;

procedure TBoldObjectReference.Assign(Source: TBoldElement);
begin
  if not assigned(Source) then
    BoldObject := nil
  else if source is TBoldObject then
    BoldObject := TBoldObject(Source)
  else if source is TBoldObjectReference then
    Locator := TBoldObjectReference(Source).Locator
  else
    Inherited assign(source);
end;

function TBoldObjectReference.CompareToAs(CompareType: TBoldCompareType; BoldElement: TBoldElement): Integer;
var
  CompareObj: TBoldObject;
begin
  if comparetype <> ctDefault then
    result := inherited CompareToAs(CompareType, BoldElement)
  else
  begin
    if BoldElement is TBoldObject then
      CompareObj := TBoldObject(BoldElement)
    else if BoldElement is TBoldObjectReference then
      CompareObj := TBoldObjectreference(BoldElement).BoldObject
    else if not assigned(BoldElement) then
      CompareObj := nil
    else
      raise EBold.CreateFmt(sUnknownObjectType, [ClassName, BoldElement.ClassName]);

    if CompareObj = BoldObject then // covers the case of both = nil
      result := 0
    else if not assigned(CompareObj) then
      result := 1
    else if not assigned(BoldObject) then
      Result := -1
    else
      result := BoldObject.CompareToAs(CompareType, CompareObj);
  end;
end;

function TBoldObjectReference.CanSetLocator(NewLocator: TBoldObjectLocator; Subscriber: TBoldSubscriber): Boolean;
begin
  result := VerifyClass(NewLocator);
{$IFNDEF BOLD_NO_QUERIES}
   result := result and SendQuery(bqMaySetValue, [NewLocator], Subscriber)
{$ENDIF}
end;

function VerifyLocatorType(ALocator: TBoldObjectLocator; AExpectedClassType: TBoldClassTypeInfo; ARaise: boolean = true): Boolean;
var
  KnownClass: TBoldClassTypeInfo;
begin
  if Assigned(AExpectedClassType) and assigned(aLocator) then
  begin
    Assert(AExpectedClassType is TBoldClassTypeInfo);
    KnownClass := aLocator.BoldClassTypeInfo;
    Assert(Assigned(KnownClass));
    if KnownClass.ConformsTo(AExpectedClassType) then
      result := true
    else if aLocator.BoldObjectId.TopSortedIndexExact then
      result := false
    else
      result := AExpectedClassType.ConformsTo(knownclass);
    if not result and ARaise then
      raise EBold.CreateFmt('Locator is: %s, expected: %s ', [KnownClass.ExpressionName, AExpectedClassType.ExpressionName]);
  end
  else
    result := true;
end;

function TBoldObjectReference.VerifyClass(aLocator: TBoldObjectLocator): Boolean;
var
  AllowedClass, KnownClass: TBoldClassTypeInfo;
begin
  if Assigned(BoldType) and assigned(aLocator) then
  begin
    Assert(BoldType is TBoldClassTypeInfo);
    Assert(BoldType.SystemTypeInfo is TBoldSystemTypeInfo);
    AllowedClass := TBoldClassTypeInfo(BoldType);
    KnownClass := aLocator.BoldClassTypeInfo;
    if KnownClass.ConformsTo(AllowedClass) then
      result := true
    else if aLocator.BoldObjectId.TopSortedIndexExact then
      result := false
    else // if it looks wrong, but the classID is inexact, then we will allow it if it _could_ be right...
      result := allowedClass.ConformsTo(knownclass);
    if not result then
      SetBoldLastFailureReason(TBoldFailureReason.CreateFmt(sCannotSetSuchReference, [KnownClass.ExpressionName, AllowedClass.ExpressionName], self));
  end
  else
    result := true;
end;

function TBoldObjectReference.CanSet(NewObject: TBoldObject; Subscriber: TBoldSubscriber): Boolean;
begin
  result := CanSetLocator(NewObject.BoldObjectLocator, Subscriber);
end;

procedure TBoldObjectReference.MakeDbCurrent;
begin
  ReferenceController.MakeDbCurrent;
end;

constructor TBoldObjectReference.CreateTypedReference(ObjectClass: TBoldObjectClass);
var
  aSystem: TBoldSystem;
begin
  inherited CreateWithOwner(nil);
  aSystem := TBoldSystem.DefaultSystem;
  if assigned(aSystem) then
    InitializeNonObjectOwned(aSystem.BoldSystemTypeInfo.TopSortedClasses.ItemsByObjectClass[ObjectClass])
  else
    InitializeNonObjectOwned(nil);
  HasOldValues := false;
end;

procedure TBoldObjectReference.Initialize;
begin
  inherited;
  if not assigned(BoldMemberRTInfo) or BoldMemberRTInfo.IsDerived then
    fObjectReferenceController := TBoldObjectReferenceController.Create(self)
  else
  begin
    Assert(BoldMemberRTInfo is TBoldRoleRTInfo);
    with TBoldRoleRTInfo(BoldMemberRTInfo) do
    begin
      if RoleType = rtInnerLinkRole then
        fObjectReferenceController := TBoldLinkObjectSingleLinkController.Create(Self)
      else if RoleType = rtLinkRole then
        fObjectReferenceController := TBoldLinkObjectReferenceController.Create(Self)
      else if IsIndirect then
        fObjectReferenceController := TBoldIndirectSingleLinkController.Create(Self)
      else if OtherEndOrdered then
        fObjectReferenceController := TBoldOrderedDirectSingleLinkController.Create(Self)
      else
        fObjectReferenceController := TBoldUnOrderedDirectSingleLinkController.Create(Self)
    end;
  end;
end;

procedure TBoldObjectReference.PreDiscard;
begin
  ReferenceController.PreDiscard;
end;

function TBoldObjectReference.ObserverMayModify(Observer: TObject): Boolean;
begin
  result := inherited ObserverMayModify(Observer) and
    (not assigned(BoldRoleRTInfo) or
    (BoldRoleRTInfo.RoleType = rtRole))
end;

function TBoldObjectReference.GetBoldRoleRTInfo: TBoldRoleRTInfo;
begin
  Assert((BoldMemberRTInfo = nil) or (BoldMemberRTInfo is TBoldRoleRTInfo));
  Result := TBoldRoleRTInfo(BoldMemberRTInfo)
end;

function TBoldObjectReference.CanClear(Subscriber: TBoldSubscriber): Boolean;
begin
{$IFDEF BOLD_NO_QUERIES}
  result := true;
{$ELSE}
  result := SendQuery(bqMayClear, [], Subscriber);
{$ENDIF}
end;

function TBoldObjectReference.InternalMayUpdate: Boolean;
begin
  result := ReferenceController.MayUpdate;
end;

function TBoldObjectReference.GetProxy(Mode: TBoldDomainElementProxyMode): TBoldMember_Proxy;
begin
  result := ReferenceController.GetProxy(self, Mode);
end;

procedure TBoldObjectReference.AssignContentValueFromElement(source: TBoldElement);
begin
  Assert(ReferenceController is TBoldObjectReferenceController);
  if not assigned(Source) then
    TBoldObjectReferenceController(ReferenceController).fLocator := nil
  else if (Source is TBoldObject) then
    (ReferenceController as TBoldObjectReferenceController).fLocator := TBoldObject(Source).BoldObjectlocator
  else if (Source is TBoldObjectReference) then
    (ReferenceController as TBoldObjectReferenceController).fLocator := TBoldObjectReference(Source).Locator
  else
    inherited;
end;

procedure TBoldObjectReference.AdjustOldValues(Translationlist: TBoldIdTranslationlist);
var
  OldRef: IBoldObjectIdRef;
  OldObjectContents: IBoldObjectContents;
  OldLink: IBoldValue;
  OldValues: IBoldValueSpace;

  procedure GetOldValueAsIdLists(IdList1, IdList2: TBoldObjectIdList; const Oldvalue: IBoldValue);
  var
    i: integer;
    IdList: IBoldObjectIdListRef;
    IdListPair: IBoldObjectIdListRefPair;
  begin
    if OldLink.QueryInterface(IBoldObjectIdListRef, IdList) = S_OK then
    begin
      for i := 0 to IdList.Count - 1 do
        IdList1.Add(IdList.IdList[i]);
    end
    else if OldLink.QueryInterface(IBoldObjectIdListRefPair, IdListPair) = S_OK then
    begin
      for i := 0 to IdListPair.Count - 1 do
      begin
        IdList1.Add(IdListPair.IdList1[i]);
        IdList2.Add(IdListPair.IdList2[i]);
      end;
    end;
  end;

  procedure RemoveIdFromLink(LinkId: TBoldObjectId; const OldLink: IBoldValue);
  var
    IdRef: IBoldObjectIdRef;
    IdRefPair: IBoldObjectIdRefPair;
    FreestandingList: IBoldFreeStandingIdList;
    FreestandingListPair: IBoldFreeStandingIdListPair;
  begin
    if OldLink.QueryInterface(IBoldObjectIdref, IdRef) = S_OK then
      IdRef.SetFromId(nil, false)
    else if OldLink.QueryInterface(IBoldObjectIdrefPair, IdRefPair) = S_OK then
      IdRefPair.SetFromIds(nil, nil)
    else if OldLink.QueryInterface(IBoldFreeStandingIdList, FreestandingList) = S_OK then
      FreeStandingList.RemoveId(LinkId)
    else if OldLink.QueryInterface(IBoldFreeStandingIdListPair, FreestandingListPair) = S_OK then
      FreeStandingListPair.RemoveId(LinkId)
    else
      raise EBoldInternal.CreateFmt('%s.AdjustOldValues (RemoveIdFromLink): Unknown type of link', [Classname]);
  end;

  procedure AddIdToLink(Id1, Id2: TBoldObjectId; const OldLink: IBoldValue);
  var
    IdRef: IBoldObjectIdRef;
    IdRefPair: IBoldObjectIdRefPair;
    FreestandingList: IBoldFreeStandingIdList;
    FreestandingListPair: IBoldFreeStandingIdListPair;
  begin
    if OldLink.QueryInterface(IBoldObjectIdref, IdRef) = S_OK then
      IdRef.SetFromId(TranslationList.TranslateToNewId[Id1], false)
    else if OldLink.QueryInterface(IBoldObjectIdrefPair, IdRefPair) = S_OK then
      IdRefPair.SetFromIds(TranslationList.TranslateToNewId[Id1], TranslationList.TranslateToNewId[Id2])
    else if OldLink.QueryInterface(IBoldFreeStandingIdList, FreestandingList) = S_OK then
      FreeStandingList.AddId(TranslationList.TranslateToNewId[Id1])
    else if OldLink.QueryInterface(IBoldFreeStandingIdListPair, FreestandingListPair) = S_OK then
      FreeStandingListPair.AddIds(TranslationList.TranslateToNewId[Id1], TranslationList.TranslateToNewId[Id2])
    else
      raise EBoldInternal.CreateFmt('%s.AdjustOldValues (RemoveIdFromLink): Unknown type of link', [Classname]);
  end;

var
  OtherInnerLink: TBoldObjectReference;
  RemoteId: TBoldObjectId;

begin
  inherited;


  Oldvalues := OwningObject.BoldSystem.fOldValueHandler.Oldvalues;
  if BoldRoleRTInfo.IsStoredInObject then
  begin
    OldRef := OldValue as IBoldObjectIdRef;
    if assigned(OldRef) and Assigned(OldRef.Id) then
    begin
      OldObjectContents := OldValues.ObjectContentsByObjectId[Oldref.Id];
      if assigned(OldObjectContents) then
      begin
        OldLink := OldObjectContents.ValueByIndex[BoldRoleRTInfo.IndexOfOtherEnd];
        if assigned(OldLink) and (OldLink.BoldPersistenceState <> bvpsInvalid) {?} then
          RemoveIdFromLink(OwningObject.BoldObjectLocator.BoldObjectId, OldLink);
      end;
    end;
    if assigned(Locator) then
    begin
      OldObjectContents := OldValues.ObjectContentsByObjectId[Locator.BoldObjectId];
      if assigned(OldObjectContents) then
      begin
        OldLink := OldObjectContents.ValueByIndex[BoldRoleRTInfo.IndexOfOtherEnd];
        if assigned(OldLink) then
        begin
          RemoteId := nil;
          if (BoldRoleRTInfo.RoleType = rtInnerLinkRole) then
          begin
            OtherInnerLink := OwningObject.BoldMembers[BoldRoleRTInfo.OtherIndexInLinkClass] as TBoldObjectReference;
            if assigned(OtherInnerLInk) and assigned(OtherInnerLInk.Locator) then
              RemoteId := OtherInnerLInk.Locator.BoldObjectId;
          end;
          AddIdToLink(OwningObject.BoldObjectLocator.BoldObjectId, RemoteId, OldLink);
        end;
      end;
    end;
  end;
end;

procedure TBoldObjectReference.DoSetInitialValue;
begin
  if BoldRoleRTInfo.RoleType = rtRole then
    BoldObject := nil;
end;

function TBoldObjectReference.GetOldEmbeddingOtherEndId: TBoldObjectId;
var
  OldRef: IBoldValue;
  OldIdRef: IBoldObjectIdRef;
  OldIdRefPair: IBoldObjectIdRefPair;
begin
  result := nil;
  OldRef := OldValue;
  if assigned(OldRef) and BoldRoleRTInfo.RoleRTInfoOfOtherEnd.IsStoredInObject then
  begin
    if OldRef.QueryInterface(IBoldObjectIdRef, OldIdRef) = S_OK then
      result := OldIdRef.Id
    else if OldRef.QueryInterface(IBoldObjectIdRefPair, OldIdRefPair) = S_OK then
      result := OldIdRefPair.Id1;
  end;
end;

{ TBoldListEnumerator }

constructor TBoldListEnumerator.Create(AList: TBoldList);
begin
  inherited Create;
  FIndex := -1;
  FList := AList;
end;

function TBoldListEnumerator.GetCurrent: TBoldElement;
begin
  Result := List[FIndex];
end;

function TBoldListEnumerator.MoveNext: Boolean;
begin
  Result := Index < List.Count - 1;
  if Result then
    Inc(FIndex);
end;

{ TBoldList }

destructor TBoldList.Destroy;
begin
  PrepareToDestroy;
  FreeAndNil(fListController);
  inherited Destroy;
  FreeData;
end;

procedure TBoldList.DefaultSubscribe(Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent);
begin
  if mutable then
    AddSmallSubscription(Subscriber, [beItemAdded, beItemDeleted, beItemReplaced, beOrderChanged, beValueInvalid], RequestedEvent);
end;

procedure TBoldList.AllocateData;
begin
  raise EBold.CreateFmt(sLocatedAbstractError, [ClassName, 'AllocateData']); // do not localize
end;

function TBoldList.GetController: TBoldAbstractController;
begin
  result := fListController;
end;

function TBoldList.GetStringRepresentation(Representation: TBoldRepresentation): string;
begin
  {$IFDEF BOLDJSON}
  if Representation = brJson then
    Result := {TBoldJSONwriter.}BoldElementToJsonString(Self)
  else
  {$ENDIF}
    result := ListController.GetStringRepresentation;
end;

procedure TBoldList.Clear;
begin
  if count = 0 then
    exit;
  BoldClearLastFailure;
  PrepareClear;
  if mutable and not CanClear(nil) then
    BoldRaiseLastFailure(self, 'Clear', '');

  if assigned(BoldSystem) then
    BoldSystem.StartTransaction;
  try
    InternalClear;
    if assigned(BoldSystem) then
      BoldSystem.CommitTransaction;
  except
    if assigned(BoldSYstem) then
      BoldSystem.RollbackTransaction;
    raise;
  end;
end;

function TBoldList.IndexOf(Item: TBoldElement): Integer;
begin
  result := IndexOfElement(Item);
end;

function TBoldList.Includes(Item: TBoldElement): Boolean;
begin
  Result := IncludesElement(Item);
end;

function TBoldList.IncludesAny(aList: TBoldList): Boolean;
var
  i: integer;
begin
  for i := 0 to aList.Count - 1 do
  begin
    if includes(aList[i]) then
    begin
      result := true;
      exit;
    end;
  end;
  result := false;
end;

function TBoldList.IncludesAll(aList: TBoldList): Boolean;
var
  i: integer;
begin
  result := true;
  for i := 0 to aList.Count - 1 do
  begin
    if not includes(aList[i]) then
    begin
      result := false;
      exit;
    end;
  end;
end;

procedure TBoldList.SetCapacity(const Value: integer);
begin
  ListController.Capacity := Value
end;

function TBoldList.GetDuplicateMode: TBoldListDupMode;
begin
  result := TBoldListDupMode(GetInternalState(BoldDuplicateModeMask, BoldDMShift));
end;

procedure TBoldList.SetDuplicateMode(NewMode: TBoldListDupMode);
var
  i, j: integer;
  OldMode: TBoldListDupMode;
begin
  OldMode := GetDuplicateMode;
  if (NewMode <> OldMode) and (NewMode = bldmMerge) then
  begin
    // TODO: This is very inefficient, improve it
    i := 0;
    while i < Count do
    begin
      j := i + 1;
      while j < Count do
      begin
        if Elements[i].IsEqual(Elements[j]) then
          RemoveByIndex(j)
        else
          inc(j);
      end;
      inc(i);
    end;
  end;
  SetInternalState(BoldDuplicateModeMask, BoldDMShift, Integer(NewMode));
end;

function TBoldList.HasDuplicates: boolean;
var
  i, j: integer;
  SortedList: TBoldList;
  G: IBoldGuard;
begin
  result := false;
  if Count < 2 then
    exit;
  if DuplicateMode = bldmAllow then
  begin
    if Count < 10 then // actually test to determine the correct number
    begin
      // implementation fast enough for few elements
      i := 0;
      while i < Count do
      begin
        j := i + 1;
        while j < Count do
        begin
          if Elements[i].IsEqual(Elements[j]) then
          begin
            result := true;
            exit;
          end
          else
            inc(j);
        end;
        inc(i);
      end;
    end
    else
    begin
    // Clone, sort, then compare neighbouring values only
      G := TBoldGuard.Create(SortedList);
//      SortedList := Clone as TBoldList;
      if Assigned(BoldType) then
        SortedList := TBoldMemberFactory.CreateMemberFromBoldType(BoldType) as TBoldList
      else
        SortedList := TBoldMemberClass(ClassType).Create as TBoldList;
      if SortedList is TBoldMemberList then
        TBoldMemberList(SortedList).CloneMembers := true; // if CloneMembers = true then sorting messes up the list.
      SortedList.AddList(self);
      SortedList.Sort;
      for i := 1 to SortedList.Count - 1 do
        if SortedList[i].IsEqual(SortedList[i-1]) then
        begin
          result := true;
          exit;
        end;
    end;
  end;
end;

procedure TBoldList.Remove(Item: TBoldElement; ARaiseIfNotFound: boolean);
var
  I: Integer;
begin
  if not mutable then
    MutableError('Remove');
  EnsureContentsCurrent;
  I := IndexOf(Item);
  if I <> -1 then
    RemoveByIndex(I)
  else
  if ARaiseIfNotFound then
    raise EBold.CreateFmt(sItemNotInList, [ClassName]);
end;

procedure TBoldList.Insert(index: Integer; Element: TBoldElement);
begin
  if not mutable then MutableError('InsertElementToList');
  InsertElement(index, Element);
end;

procedure TBoldList.Add(Element: TBoldElement);
begin
  if not mutable then
    MutableError('AddToList');
  AddElement(Element);
end;

procedure TBoldList.AddList(List: TBoldList);
var
  I: Integer;
  vBoldSystem: TBoldSystem;
begin
  if not mutable then MutableError('AddListToList');
  i := List.Count;
  if i > 0 then
  begin
    vBoldSystem := BoldSystem;
    if Assigned(vBoldSystem) then
      vBoldSystem.StartTransaction(stmNormal);
    try
      if (i > 4) then
      begin
        if DuplicateMode = bldmAllow then
          Capacity := Count + i;
        if i > Count then
          Capacity := i;
      end;
      for I := 0 to i - 1 do
        AddElement(List[I]);
      if Assigned(vBoldSystem) then
        vBoldSystem.CommitTransaction(stmNormal);
    except
      if Assigned(vBoldSystem) then
        vBoldSystem.RollbackTransaction(stmNormal);
      raise;
    end;
  end;
end;

procedure TBoldList.RemoveList(List: TBoldList);
var
  i, j: Integer;
  vBoldSystem: TBoldSystem;
begin
  if not Mutable then MutableError('TBoldList.RemoveList'); // do not localize
  EnsureContentsCurrent;
  if Empty or List.Empty then
    exit;
  vBoldSystem := BoldSystem;
  if Assigned(vBoldSystem) then
    vBoldSystem.StartTransaction(stmNormal);
  try
    if List.Count < count then
    for i := List.Count - 1 downto 0 do
    begin
      // IndexOf() is faster in BoldList, BoldMemberList, yet slower in BoldObjectList so we reimplement this method there
      j := IndexOf(List[i]);
      if j <> -1 then
        RemoveByIndex(j);
    end
    else
    begin
      i := count - 1;
      repeat
        if List.Includes( Elements[i] ) then
        begin
          RemoveByIndex(i);
          i := count - 1; // this a safeguard in case removing 1 element consequently removes others too
        end
        else
          dec(i);
      until i = -1
    end;
    if Assigned(vBoldSystem) then
      vBoldSystem.CommitTransaction(stmNormal);
  except
    if Assigned(vBoldSystem) then
      vBoldSystem.RollbackTransaction(stmNormal);
    raise;
  end;
end;

procedure TBoldList.IntersectList(List: TBoldList);
var
  i: Integer;
  vBoldSystem: TBoldSystem;
begin
  if not Mutable then MutableError('TBoldList.RemoveList'); // do not localize
  EnsureContentsCurrent;
  vBoldSystem := BoldSystem;
  if Assigned(vBoldSystem) then
    vBoldSystem.StartTransaction(stmNormal);
  try
    i := count - 1;
    repeat
      if not List.Includes( Elements[i] ) then
      begin
        RemoveByIndex(i);
        i := count - 1; // this a safeguard in case removing 1 element consequently removes others too
      end
      else
        dec(i);
    until i = -1;
    if Assigned(vBoldSystem) then
      vBoldSystem.CommitTransaction(stmNormal);
  except
    if Assigned(vBoldSystem) then
      vBoldSystem.RollbackTransaction(stmNormal);
    raise;
  end;
end;

function TBoldList.GetCanCreateNew: Boolean;
begin
  result := CanModify and assigned(ListController) and ListController.CanCreateNew;
end;

function TBoldList.GetCapacity: integer;
begin
  result := ListController.Capacity;
end;

procedure TBoldList.EnsureCanCreateNew;
begin
  BoldClearLastFailure;
  if not CanCreateNew then
    BoldRaiseLastFailure(self, 'EnsureCanCreateNew', sCannotCreateNewElement); // do not localize
end;

function TBoldList.CreateNew: TBoldElement;
begin
  EnsureCanCreateNew;
  result := ListController.CreateNew;
end;

constructor TBoldList.CreateWithTypeInfo(ElementTypeInfo: TBoldElementTypeInfo);
begin
  if Assigned(ElementTypeInfo) and not (ElementTypeInfo is TBoldListTypeInfo) then
    ElementTypeInfo := ElementTypeInfo.ListTypeInfo;
  inherited;
end;

function TBoldList.AddNew: TBoldElement;
begin
  result := InternalAddNew;
end;

procedure TBoldList.ToStrings(Representation: TBoldRepresentation; S: TStrings);
begin
  S.Clear;
  AddToStrings(Representation, S);
end;

procedure TBoldList.AddToStrings(Representation: TBoldRepresentation; S: TStrings);
var
  aExpressionement: TBoldElement;
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    aExpressionement := Elements[I];
    S.AddObject(aExpressionement.StringRepresentation[Representation], aExpressionement);
  end;
end;

procedure TBoldList.ToStringsWithNil(Representation: TBoldRepresentation; S: TStrings; nilString: string);
begin
  S.Clear;
  S.AddObject(nilString, nil);
  AddToStrings(Representation, S);
end;

function TBoldList.AsCommaText(AIncludeType: boolean; Representation: TBoldRepresentation; const ASeparator: string): string;
var
  i: integer;
  s: string;
begin
  result := '';
  for I := 0 to Count - 1 do
  begin
    if Assigned(Elements[i]) then
    begin
      s := Elements[i].StringRepresentation[Representation];
      if AIncludeType then
       s := Elements[i].DebugInfo + ':' + s;
    end
    else
      s := 'nil';
    if i < count-1 then
      s := s + ASeparator;
    result := result + s;
  end;
end;

function TBoldList.AsDebugCommaText(const ASeparator: string = ','): string;
var
  i: integer;
  s: string;
begin
  result := '';
  for I := 0 to Count - 1 do
  begin
    if Assigned(Elements[i]) then
      s := Elements[i].DebugInfo
    else
      s := 'nil';
    if i < count-1 then
      s := s + ASeparator;
    result := result + s;
  end;
end;

procedure TBoldList.GetAsList(ResultList: TBoldIndirectElement);
begin
  ResultList.SetReferenceValue(Self);
end;

procedure TBoldList.EnsureRange(FromIndex, ToIndex: integer);
begin
end;

function TBoldList.canInsert(index: Integer; Element: TBoldElement; Subscriber: TBoldSubscriber): Boolean;
begin
{$IFDEF BOLD_NO_QUERIES}
  result := true;
{$ELSE}
  result := SendQuery(bqMayInsert, [index, Element], Subscriber);
{$ENDIF}
end;

function TBoldList.CanRemove(index: Integer; Subscriber: TBoldSubscriber): Boolean;
begin
{$IFDEF BOLD_NO_QUERIES}
  result := true;
{$ELSE}
  result := SendQuery(bqMayRemove, [index], Subscriber);
{$ENDIF}
end;

function TBoldList.CanMove(CurIndex, NewIndex: Integer; Subscriber: TBoldSubscriber = nil): Boolean;
begin
  result := (NewIndex >= 0) and (NewIndex < Count);
{$IFNDEF BOLD_NO_QUERIES}
  result := result and SendQuery(bqMayMove, [CurIndex, NewIndex], Subscriber);
{$ENDIF}
end;

function TBoldList.CanSet(index: Integer; Item: TBoldElement; Subscriber: TBoldSubscriber): Boolean;
begin
{$IFDEF BOLD_NO_QUERIES}
  result := true;
{$ELSE}
  result := SendQuery(bqMayReplace, [index, Item], Subscriber);
{$ENDIF}
end;

constructor TBoldMember.CreateWithTypeInfo(ElementTypeInfo: TBoldElementTypeInfo);
begin
  inherited CreateWithOwner(nil);
  InitializeNonObjectOwned(ElementTypeInfo);
end;

procedure TBoldList.Sort(CompareFunc: TBoldElementCompare; FirstIndex,
    LastIndex: Integer; SortMode: TBoldSortMode = BoldDefaultSortMode);

  //////////////////////////////////////////////////////////////////////////////
  // Insertion Sort:                                                          //
  // stable, inplace, but only fast on small lists                            //
  //////////////////////////////////////////////////////////////////////////////
  procedure InsertSort(Left, Right: Integer; SCompare: TBoldElementCompare);
  var
    I, J: Integer;
    T: TBoldElement;
  begin
    for I := Left + 1 to Right do begin
      if SCompare(Elements[I], Elements[I - 1]) < 0 then begin
        J := I;
        T := Elements[J];
        while (J > Left) and (SCompare(T, Elements[J - 1]) < 0) do begin
          Elements[J] := Elements[J - 1];
          Dec(J);
        end;
        Elements[J] := T;
      end;
    end;
  end;

  //////////////////////////////////////////////////////////////////////////////
  // Quick Sort:                                                              //
  // fast, inplace (without help array),                                      //
  // but NOT stable (sorting changes within same elements)                    //
  //////////////////////////////////////////////////////////////////////////////
  procedure QuickSort(Left, Right: Integer; SCompare: TBoldElementCompare);
  var
    I, J: Integer;
    P: TBoldElement;
  begin
    repeat
      I := Left;
      J := Right;
      P := Elements[(Left + Right) shr 1];
      repeat
        while SCompare(Elements[I], P) < 0 do
          Inc(I);
        while SCompare(Elements[J], P) > 0 do
          Dec(J);
        if I <= J then
        begin
          if I <> J then
          begin
            Move(I, J);
            Move(J - 1, I);
          end;
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if Left < J then
        QuickSort(Left, J, SCompare);
      Left := I;
    until I >= Right;
  end;

  //////////////////////////////////////////////////////////////////////////////
  // Merge Sort - Inplace Variant:                                            //
  // http://thomas.baudel.name/Visualisation/VisuTri/inplacestablesort.html   //
  // stable, inplace, but slower than Quicksort and normal Mergesort          //
  //////////////////////////////////////////////////////////////////////////////
  function Lower(Left, Right, Val: Integer; SCompare: TBoldElementCompare):
      Integer;
  var
    iLen: Integer;
    iHalf: Integer;
    iMid: Integer;
  begin
    iLen := Right - Left;
    while iLen > 0 do begin
      iHalf := iLen div 2;
      iMid := Left + iHalf;
      if SCompare(Elements[iMid], Elements[Val]) < 0 then begin
        Left := iMid + 1;
        iLen := iLen - iHalf - 1;
      end else begin
        iLen := iHalf;
      end;
    end;
    Result := Left;
  end;

  function Upper(Left, Right, Val: Integer; SCompare: TBoldElementCompare):
      Integer;
  var
    iLen: Integer;
    iHalf: Integer;
    iMid: Integer;
  begin
    iLen := Right - Left;
    while iLen > 0 do begin
      iHalf := iLen div 2;
      iMid := Left + iHalf;
      if SCompare(Elements[Val], Elements[iMid]) < 0 then begin
        iLen := iHalf;
      end else begin
        Left := iMid + 1;
        iLen := iLen - iHalf - 1;
      end;
    end;
    Result := Left;
  end;

  function GCD(M, N: Integer): Integer;
  var
    T: Integer;
  begin
    while (N <> 0) do begin
      T := M mod N;
      M := N; N := T;
    end;
    Result := M;
  end;

  procedure Rotate(Left, Middle, Right: Integer; SCompare: TBoldElementCompare);
  var
    N: Integer;
    SavedElement: TBoldElement;
    Shift: Integer;
    P1, P2: Integer;
  begin
    if (Left <> Middle) and (Right <> Middle) then begin
      N := GCD(Right - Left, Middle - Left);
      while N <> 0 do begin
        Dec(N);
        SavedElement := Elements[Left + N];
        Shift := Middle - Left;
        P1 := Left + N;
        P2 := Left + N + Shift;
        while (P2 <> Left + N)  do begin
          Elements[P1] := Elements[P2];
          P1 := P2;
          if Right - P2 > Shift then begin
            Inc(P2, Shift);
          end else begin
            P2 := Left + (Shift - (Right - P2));
          end;
        end;
        Elements[P1] := SavedElement;
      end;
    end;
  end;

  procedure MergeInplace(Left, Pivot, Right, Len1, Len2: Integer; SCompare:
      TBoldElementCompare);
  var
    iFirstCut, iSecondCut: Integer;
    iLen11, iLen22: Integer;
    iNewMid: Integer;
  begin
    if (Len1 <> 0) and (Len2 <> 0) then begin
      if Len1 + Len2 = 2 then begin
        if SCompare(Elements[Pivot], Elements[Left]) < 0 then begin
          if Pivot < Left then begin
            Move(Pivot, Left);
            Move(Left - 1, Pivot);
          end else begin
            Move(Left, Pivot);
            Move(Pivot - 1, Left);
          end;
        end;
      end else begin
        if Len1 > Len2 then begin
          iLen11 := Len1 div 2;
          iFirstCut := Left + iLen11;
          iSecondCut := Lower(Pivot, Right, iFirstCut, SCompare);
          iLen22 := iSecondCut - Pivot;
        end else begin
          iLen22 := Len2 div 2;
          iSecondCut := Pivot + iLen22;
          iFirstCut := Upper(Left, Pivot, iSecondCut, SCompare);
          iLen11 := iFirstCut - Left;
        end;
        Rotate(iFirstCut, Pivot, iSecondCut, SCompare);
        iNewMid := iFirstCut + iLen22;
        MergeInplace(Left, iFirstCut, iNewMid, iLen11, iLen22, SCompare);
        MergeInplace(iNewMid, iSecondCut, Right, Len1 - iLen11, Len2 - iLen22, SCompare);
      end;
    end;
  end;

  procedure MergeSortInplace(Left, Right: Integer; SCompare: TBoldElementCompare);
  var
    Middle: Integer;
  begin
    if Right - Left < 8 then begin
      InsertSort(Left, Right, SCompare);
    end else begin
      Middle := (Left + Right) div 2;
      MergeSortInplace(Left, Middle, SCompare);
      MergeSortInplace(Middle, Right, SCompare);
      MergeInplace(Left, Middle, Right, Middle - Left, Right - Middle, SCompare);
    end;
  end;

  //////////////////////////////////////////////////////////////////////////////
  // Merge Sort:                                                              //
  // http://www.iti.fh-flensburg.de/lang/algorithmen/sortieren/merge/merge.htm//
  // fastest, stable,                                                         //
  // but not fully inplace (help array with only n/2 is needed)               //
  //////////////////////////////////////////////////////////////////////////////
  procedure DoMergeSort(var HelpArray: array of Pointer; Left, Right: Integer;
      SCompare: TBoldElementCompare);
  var
    m: Integer;
    i, j, k: Integer;
  begin
    if Left < Right then begin
      if Right - Left < 4 then begin
        InsertSort(Left, Right, SCompare);
      end else begin
        m := (Left + Right) div 2;
        DoMergeSort(HelpArray, Left, m, SCompare);
        DoMergeSort(HelpArray, m + 1, Right, SCompare);

        i := 0;
        j := Left;
        // Copy first half of elements in help array
        while j <= m do begin
          HelpArray[i] := Elements[j];
          Inc(i);
          Inc(j);
        end;

        i := 0;
        k := Left;
        // Copy back the next largest element
        while (k < j) and (j <= Right) do begin
          if SCompare(HelpArray[i], Elements[j]) <= 0 then begin
            Elements[k] := HelpArray[i];
            Inc(i);
          end else begin
            Elements[k] := Elements[j];
            Inc(j);
          end;
          Inc(k);
        end;

        // Copy back the rest of help array if existing
        while k < j do begin
          Elements[k] := HelpArray[i];
          Inc(k);
          Inc(i);
        end;
      end;
    end;
  end;

  procedure MergeSort(Left, Right: Integer; SCompare: TBoldElementCompare);
  var
    HelpArray: array of Pointer;
  begin
    SetLength(HelpArray, (Count + 1) div 2);
    DoMergeSort(HelpArray, FirstIndex, LastIndex, CompareFunc);
    SetLength(HelpArray, 0);
  end;

begin
  if Assigned(Self) and (Count > 1) then begin
    case SortMode of
      smQuickSort: QuickSort(FirstIndex, LastIndex, CompareFunc);
      smMergeSort: MergeSort(FirstIndex, LastIndex, CompareFunc);
      smMergeSortInplace: MergeSortInplace(FirstIndex, LastIndex, CompareFunc);
    end;
  end;
end;

procedure TBoldList.Sort(CompareFunc: TBoldElementCompare; SortMode:
    TBoldSortMode = BoldDefaultSortMode);
begin
  Sort(CompareFunc, 0, Count - 1, SortMode);
end;

procedure TBoldList.Sort;
begin
  Sort(DefaultCompare, 0, Count - 1);
end;

function TBoldList.DuplicateControl: Boolean;
begin
  case DuplicateMode of
    bldmAllow:
      Result := True;
    bldmMerge:
      Result := False;
    bldmError:
      raise EBold.CreateFmt(sDuplicateInList, [ClassName]);
  else
    raise EBoldInternal.CreateFmt(sUnknownDuplicationMode, [ClassName]);
  end;
end;

procedure TBoldList.Initialize;
begin
  inherited;
  AllocateData;
end;

function TBoldList.CompareToAs(CompType: TBoldCompareType; BoldElement: TBoldElement): Integer;
begin
  if BoldElement is TBoldList then
  begin
    if TBoldList(BoldElement).Count <> Count then
      result := -1
    else
    if IncludesAll(TBoldList(BoldElement)) then
      result := 0
    else
      result := -1
  end
  else
    Result := inherited CompareToAs(CompType, BoldElement);
end;

function TBoldList.CanClear(Subscriber: TBoldSubscriber): Boolean;
begin
{$IFDEF BOLD_NO_QUERIES}
  result := true;
{$ELSE}
  result := SendQuery(bqMayClear, [], Subscriber);
{$ENDIF}
end;

function TBoldList.DefaultCompare(Item1, Item2: TBoldElement): Integer;
begin
  Result := Item1.CompareTo(Item2);
end;

function TBoldList.GetEnumerator: TBoldListEnumerator;
begin
  result := TBoldListEnumerator.Create(Self);
end;

function TBoldList.GetFirst: TBoldElement;
begin
  if empty then
    result := nil
  else
    result := Elements[0];
end;

function TBoldList.GetLast: TBoldElement;
begin
  if empty then
    result := nil
  else
    result := Elements[count-1];
end;

procedure TBoldList.PrepareClear;
begin
end;

procedure TBoldList.MakeContentsImmutable;
var
  I :integer;
begin
  for I := 0 to Count - 1 do
    Elements[I].MakeImmutable;
end;

{ TBoldMemberListEnumerator }

function TBoldMemberListEnumerator.GetCurrent: TBoldMember;
begin
  Result := List[Index] as TBoldMember;
end;

{ TBoldMemberList }

procedure TBoldMemberList.AllocateData;
begin
  faList := TList.Create;
end;

procedure TBoldMemberList.FreeData;
var
  I: Integer;
begin
  if CloneMembers then
    for I := 0 to Count - 1 do
      Elements[I].Free;
  FreeAndNil(faList);
end;

procedure TBoldMemberList.Assign(Source: TBoldElement);
var
  SourceList: TBoldMemberList;
  I: Integer;
begin
  if not (Source is TBoldMemberList) then
    raise EBold.CreateFmt(sSourceNotBoldMemberList, [ClassName, Source.ClassName]);

  SourceList := TBoldMemberList(Source);
  for I := 0 to SourceList.Count - 1 do
    Add(SourceList[I]);
end;

procedure TBoldMemberList.AddElement(Element: TBoldElement);
begin
  EnsureContentsCurrent;
  BoldClearLastFailure;
  if not CanInsert(-1, Element, nil) then
    BoldRaiseLastFailure(self, 'AddElement', ''); // do not localize
  if Element is TBoldMember then
    Add(TBoldMember(Element))
  else
    raise EBold.CreateFmt(sElementNotBoldMember, [ClassName, 'AddElement']); // do not localize
end;

procedure TBoldMemberList.InsertElement(index: Integer; Element: TBoldElement);
const
  cnMethod = 'InsertElement';
begin
  EnsureContentsCurrent;
  BoldClearLastFailure;
  if not CanInsert(index, Element, nil) then
    BoldRaiseLastFailure(self, cnMethod, ''); // do not localize
  if Element is TBoldMember then
    Insert(index, TBoldMember(Element))
  else
    raise EBold.CreateFmt(sElementNotBoldMember, [ClassName, cnMethod]); // do not localize
end;

function TBoldMemberList.GetElement(index: Integer): TBoldElement;
begin
  EnsureContentsCurrent;
  BoldClearLastFailure;
  if not CanRead(nil) then
    BoldRaiseLastFailure(self, 'GetElement', '');
  Result := TBoldElement(List[index]);
end;

function TBoldMemberList.GetEnumerator: TBoldMemberListEnumerator;
begin
  result := TBoldMemberListEnumerator.Create(self)
end;

function TBoldMemberList.GetFirst: TBoldMember;
begin
  result := inherited first as TBoldMember;
end;

function TBoldMemberList.GetLast: TBoldMember;
begin
  result := inherited last as TBoldMember;
end;

procedure TBoldMemberList.SetElement(index: Integer; Value: TBoldElement);
begin
  EnsureContentsCurrent;
  if CheckReplace(index, Value as TBoldMember) then
  begin
    BoldClearLastFailure;
    if not CanSet(index, Value, nil) then
      BoldRaiseLastFailure(self, 'SetElement', '');
    if CloneMembers then
    begin
      TBoldMember(List[index]).Free;
      List[index] := TBoldMember(Value).Clone;
    end
    else
      List[index] := Value;
  end
  else
    Move(IndexOf(Value as TBoldMember), index);
end;

function TBoldMemberList.GetCount: Integer;
begin
  EnsureContentsCurrent;
  BoldClearLastFailure;
  if not CanRead(nil) then
    BoldRaiseLastFailure(self, 'GetCount', '');
  Result := List.Count;
end;

function TBoldMemberList.IndexOf(Item: TBoldMember): Integer;
begin
  Result := IndexOfElement(Item);
end;

procedure TBoldMemberList.RemoveByIndex(index: Integer);
var
  temp: TBoldMember;
begin
  if not mutable then MutableError(IntToStr(index));
  EnsureContentsCurrent;
  temp := TBoldMember(falist[index]);
  faList.Delete(index);
  if CloneMembers then
    temp.Free;
  Changed(beItemDeleted, [index]);
end;

function TBoldMemberList.GetBoldMember(index: Integer): TBoldMember;
begin
  EnsureContentsCurrent;
  Result := TBoldMember(Elements[index]);
  Assert(Result is TBoldMember);
end;

procedure TBoldMemberList.SetBoldMember(index: Integer; Value: TBoldMember);
begin
  Elements[index] := Value;
end;

procedure TBoldMemberList.Add(Item: TBoldMember);
begin
  if CheckAdd(Item) then
  begin
    if CloneMembers then
      List.Add(TBoldMember(Item.Clone))
    else
      List.Add(Item)
  end;
end;

procedure TBoldMemberList.Move(CurIndex, NewIndex: Integer);
begin
  if not mutable then MutableError('MoveElementInList');
  EnsureContentsCurrent;
  List.Move(CurIndex, NewIndex);
  Changed(beOrderChanged, []);
end;

procedure TBoldMemberList.InternalAddWithoutCloning(Item: TBoldMember);
begin
  List.Add(Item);
end;

procedure TBoldMemberList.Insert(index: Integer; Item: TBoldMember);
begin
  if assigned(Item) then
  begin
    if CloneMembers then
    begin
      if CheckInsert(index, Item) then
        List.Insert(index, TBoldMember(Item.Clone));
    end
    else
    begin
      if CheckInsert(index, Item) then
        List.Insert(Index, Item)
      else
        Move(IndexOf(Item), index);
    end;
  end;
end;

function TBoldMemberList.GetStreamName: String;
begin
  raise EBoldFeatureNotImplementedYet.CreateFmt('%s.GetStreamName: Not Implemented', [ClassName]);
end;

function TBoldMemberList.GetStringRepresentation(Representation: TBoldRepresentation): string;
var
  sl: TStringList;
begin
//  if Representation = brJson then
//    Result := TBoldJSONwriter.BoldElementToJsonString(Self)
//  else
//  begin
  sl := TStringList.Create;
  try
    ToStrings(Representation, sl);
    result := sl.text;
  finally
    sl.free;
  end;
end;

procedure TBoldMemberList.SetStringRepresentation(Representation: integer; const NewValue: String);
var
  sl: TStringList;
  ElementTypeInfo: TBoldElementTypeInfo;
  Element: TBoldElement;
  i: integer;
begin
  ElementTypeInfo := (BoldType as TBoldListTypeInfo).ListElementTypeInfo;
  self.Clear;
  sl := TStringList.Create;
  try
    Sl.CommaText := NewValue;
    for I := 0 to sl.count-1 do
    begin
      Element := ElementTypeInfo.CreateElement;
      try
        Element.AsString := sl[i];
        self.AddElement(Element);
      finally
        if CloneMembers then
          Element.free;
      end;
    end;
  finally
    sl.free;
  end;
end;

function TBoldMemberList.IndexOfElement(Item: TBoldElement): Integer;
begin
  if assigned(Item) and (Item is TBoldMember) then
    result := List.IndexOf(Item)
  else
    result := -1;
end;

function TBoldMemberList.IncludesElement(Item: TBoldElement): Boolean;
begin
  result := IndexOfElement(Item) <> -1;
end;

function TBoldMemberList.IndexOfFirstEqualElement(Item: TBoldMember): Integer;
begin
  if assigned(Item) then
  for result := 0 to List.Count - 1 do
  begin
    if TBoldElement(List[result]).BoldType.ConformsTo(Item.BoldType) and TBoldElement(List[result]).IsEqual(Item) then
      exit;
  end;
  result := -1;
end;

function TBoldMemberList.IncludesValue(Item: TBoldElement): Boolean;
var
  i: integer;
begin
  result := false;
  for I := 0 to Count - 1 do
    if Elements[i].IsEqual(Item) then
    begin
      result := true;
      exit;
    end;
end;

procedure TBoldMemberList.SetCapacity(const Value: integer);
begin
  faList.Capacity := Value;
end;

procedure TBoldMemberList.SetCloneMembers(const Value: Boolean);
begin
  if (count = 0) or (fCloneMembers = Value) then
    FCloneMembers := Value
  else
    raise EBold.CreateFmt(sOnlyAllowedOnEmptyLists, [ClassName]);
end;

function TBoldMemberList.CheckAdd(NewMember: TBoldMember): Boolean;
begin
  if (not Assigned(NewMember)) then
    Result := False // Adding nil does nothing
  else
    result := (DuplicateMode = bldmAllow) or not IncludesValue(NewMember) or DuplicateControl;
end;

function TBoldMemberList.CheckInsert(index: Integer; NewMember: TBoldMember): Boolean;
begin
  assert(assigned(NewMember), 'nil not allowed, should have been filtered out before');
  result := (DuplicateMode = bldmAllow) or not IncludesValue(NewMember) or DuplicateControl;
end;

function TBoldMemberList.CheckReplace(index: Integer; NewMember: TBoldMember): Boolean;
begin
  if (not Assigned(NewMember)) or (not Includes(NewMember)) then
  begin
    Result := True;
    exit;
  end;
  if IndexOf(NewMember) = index then
    Result := False
  else
    Result := DuplicateControl;
end;

procedure TBoldMemberList.Initialize;
begin
  inherited;
  DuplicateMode := bldmAllow;
  FCloneMembers := true;
end;

function TBoldMemberList.CreateNew: TBoldElement;
begin
  EnsureCanCreateNew;
  Assert(BoldType is TBoldListTypeInfo);
  result := TBoldMemberFactory.CreateMemberFromBoldType(TBoldListTypeInfo(BoldType).ListElementTypeInfo);
end;

function TBoldMemberList.GetCanCreateNew: Boolean;
begin
  result := assigned(BoldType) and (BoldType is TBoldListTypeInfo) and
    (TBoldListTypeInfo(BoldType).ListElementTypeInfo is TBoldAttributeTypeInfo);
end;

function TBoldMemberList.GetCapacity: integer;
begin
  result := faList.Capacity;
end;

function TBoldMemberList.InternalAddNew: TBoldElement;
begin
  result := CreateNew;
  Assert(result is TBoldMember);
  InternalAddWithoutCloning(TBoldMember(result));
end;

procedure TBoldMemberList.InsertNew(index: Integer);
var
  Elem: TBoldElement;
begin
  Elem := nil;
  try
    Elem := CreateNew;
    Assert(Elem is TBoldMember);
    Insert(index, TBoldMember(Elem));
    if CloneMembers then
      FreeAndNil(Elem);
  except
    FreeAndNil(Elem);
    raise
  end;
end;

function TBoldMemberList.GetProxy(Mode: TBoldDomainElementProxyMode): TBoldMember_Proxy;
begin
  raise EBoldInternal.Create('No proxy for TBoldMemberList yet');
end;

procedure TBoldMemberList.InternalClear;
var
  i: integer;
begin
  for I := Count - 1 downto 0 do
      RemoveByIndex(I);
end;

procedure TBoldMemberList.AssignCloneValue(AClone: TBoldMember);
var
  Dest: TBoldMemberList;
  i: integer;
begin
  Dest := (AClone as TBoldMemberList);
  Dest.SetCapacity(Count);
  for I := 0 to Count-1 do
    if Dest.CloneMembers then
      Dest.List.Add(TBoldMember(List[i]).Clone)
    else
      Dest.List.Add(TBoldMember(List[i]));
end;

{ TBoldObjectListEnumerator }

function TBoldObjectListEnumerator.GetCurrent: TBoldObject;
begin
  Result := List[Index] as TBoldObject;
end;

{ TBoldObjectListLocatorEnumerator }

function TBoldObjectListLocatorEnumerator.GetCurrent: TBoldObjectLocator;
begin
  Result := TBoldObjectList(List).Locators[Index] as TBoldObjectLocator;
end;

{ TBoldObjectList }

(*constructor TBoldObjectList.CreateTypedList(ObjectClass: TBoldObjectClass; ABoldSystem: TBoldSystem);
var
  aSystem: TBoldSystem;
  ClassTypeInfo: TBoldClassTypeInfo;
begin
  inherited CreateWithOwner(ABoldSystem);
  if Assigned(ABoldSystem) then
    aSystem := ABoldSystem
  else
  aSystem := TBoldSystem.DefaultSystem;
  if not assigned(aSystem) then
    raise EBold.CreateFmt(sCannotFindSystem, [classname]);
  ClassTypeInfo := aSystem.BoldSystemTypeInfo.TopSortedClasses.ItemsByObjectClass[ObjectClass];
  if not assigned(ClassTypeInfo) then
    raise EBold.CreateFmt(sClassIsNotBusinessClass, [classname, ObjectClass.ClassName]);
  InitializeNonObjectOwned(ClassTypeInfo.ListTypeInfo);
end;*)

constructor TBoldObjectList.CreateRootClassList(ABoldSystem: TBoldSystem);
var
  aSystem: TBoldSystem;
begin
  if Assigned(ABoldSystem) then
    aSystem := ABoldSystem
  else
  aSystem := TBoldSystem.DefaultSystem;
  if not assigned(aSystem) then
    raise EBold.CreateFmt('%s.CreateRootClassList: Can not find system', [classname]);
  CreateWithTypeInfo(aSystem.BoldSystemTypeInfo.RootClassTypeInfo);
end;

procedure TBoldObjectList.AllocateData;
begin
end;

function TBoldObjectList.InternalAddNew: TBoldElement;
var
  Obj: TBoldObject;
  aSystem: TBoldSystem;
begin
  aSystem := FindASystem;
  if not assigned(aSystem) then
    raise EBold.CreateFmt('%s.InternalAddNew: Can not find system', [classname]);
  aSystem.StartTransaction;
  try
    Result := CreateNew;
    Obj := result as TBoldObject;
    EnsureContentsCurrent;
    ObjectListController.AddLocator(Obj.BoldObjectLocator);
    aSystem.CommitTransaction;
  except
    aSystem.RollBackTransaction;
    raise;
  end;
end;

procedure TBoldObjectList.InsertNew(index: Integer);
var
  Obj: TBoldObject;
  aSystem: TBoldSystem;
begin
  aSystem := FindASystem;
  if not assigned(aSystem) then
    raise EBold.CreateFmt('%s.InsertNew: Can not find system', [classname]);
  aSystem.StartTransaction;
  try
    Obj := CreateNew as TBoldObject;
    Insert(index, Obj);
    aSystem.CommitTransaction;
  except
    aSystem.RollBackTransaction;
    raise;
  end;
end;

procedure TBoldObjectList.FreeData;
begin
end;

function TBoldObjectList.GetEvaluator: TBoldEvaluator;
begin
  result := nil;
  if IsPartOfSystem then
    result := BoldSystem.Evaluator
  else
  if not Empty then
    result := Locators[0].BoldSystem.Evaluator
  else
    result := inherited GetEvaluator;
end;

function TBoldObjectList.ObserverMayModify(Observer: TObject): Boolean;
begin
  result := inherited ObserverMayModify(Observer) and
    (not assigned(BoldRoleRTInfo) or
    (BoldRoleRTInfo.RoleType = rtRole))
end;

procedure TBoldObjectList.AddElement(Element: TBoldElement);
begin
  if assigned(Element) then
  begin
    if Element is TBoldObject then
      AddLocator(TBoldObject(Element).BoldObjectLocator)
    else
    if Element is TBoldObjectReference then
      AddLocator(TBoldObjectReference(Element).Locator)
    else
    if Element is TBoldObjectList then
      AddList(TBoldObjectList(Element))
    else
      raise EBold.CreateFmt(sElementNotBoldObject, [ClassName, 'AddElement', Element.ClassName]); // do not localize
  end;
end;

procedure TBoldObjectList.AddList(List: TBoldList);
var
  i: Integer;
  ObjectList: TBoldObjectList;
  vDuplicateMode: TBoldListDupMode;
  vBoldSystem: TBoldSystem;
begin
  if not mutable then
    MutableError('AddListToList'); // do not localize
  if not (List is TBoldObjectList) then
    raise EBold.CreateFmt(sListNotObjectList, [ClassName]);
  i := List.Count;
  if i = 1 then
    AddLocator(TBoldObjectList(List).Locators[0])
  else
  if i > 0 then
  begin
    BeginUpdate;
    try
    vBoldSystem := BoldSystem;
    if Assigned(vBoldSystem) then
      vBoldSystem.StartTransaction(stmNormal);
    try
      if (i > 4) then
      begin
        if DuplicateMode = bldmAllow then
          Capacity := Count + i;
        if i > Count then
          Capacity := i;
      end;
      vDuplicateMode := DuplicateMode;
        ObjectList := List.Clone as TBoldObjectList; // make a clone to avoid problem if destination affects source
      try
          // if destination is empty and source has no duplicates then we don't have to check for duplicates
        if (Count = 0) and (List.DuplicateMode <> bldmAllow) then
          DuplicateMode := bldmAllow;
          for I := 0 to ObjectList.Count-1 do
            AddLocator(ObjectList.Locators[i]);
      finally
        SetInternalState(BoldDuplicateModeMask, BoldDMShift, Integer(vDuplicateMode));
          ObjectList.free;
      end;
      if Assigned(vBoldSystem) then
        vBoldSystem.CommitTransaction(stmNormal);
    except
      if Assigned(vBoldSystem) then
        vBoldSystem.RollbackTransaction(stmNormal);
      raise;
    end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TBoldObjectList.InsertElement(index: Integer; Element: TBoldElement);
begin
  if Assigned(Element) then
  begin
    if Element is TBoldObject then
      InsertLocator(index, TBoldObject(Element).BoldObjectLocator)
    else
      raise EBold.CreateFmt(sElementNotBoldObject, [ClassName, 'InsertElement', Element.ClassName]); // do not localize
  end;
end;

function TBoldObjectList.GetElement(index: Integer): TBoldElement;
begin
  Result := GetLocator(index).EnsuredBoldObject;
end;

procedure TBoldObjectList.SetElement(index: Integer; Value: TBoldElement);
begin
  if Value is TBoldObject then
    SetLocator(index, TBoldObject(Value).BoldObjectLocator)
  else if not assigned(Value) then
    raise EBold.CreateFmt(sElementIsNil, [classname])
  else
    raise EBold.CreateFmt(sElementNotBoldObject, [classname, 'SetElement', Value.ClassName]); // do not localize
end;

function TBoldObjectList.IndexOfElement(Item: TBoldElement): Integer;
begin
  if Item is TBoldObject then
    result := IndexOfLocator(TBoldObject(Item).BoldObjectLocator)
  else
    result := -1;
end;

function TBoldObjectList.IncludesElement(Item: TBoldElement): Boolean;
begin
  if Item is TBoldObject then
    result := LocatorInList(TBoldObject(Item).BoldObjectLocator)
  else
    result := False;
end;

function TBoldObjectList.GetCount: Integer;
begin
  EnsureContentsCurrent;
  BoldClearLastFailure;
  if not CanRead(nil) then
    BoldRaiseLastFailure(self, 'GetCount', '');
  result := ListController.GetCount;
end;

function TBoldObjectList.GetLocator(index: Integer): TBoldObjectLocator;
begin
  EnsureContentsCurrent;
  BoldClearLastFailure;
  if not CanRead(nil) then
    BoldRaiseLastFailure(self, 'GetLocator', '');
  result := ObjectListController.GetLocator(index);
end;

procedure TBoldObjectList.Assign(Source: TBoldElement);
var
  SourceList: TBoldObjectList;
  I: Integer;
begin
  if not (Source is TBoldObjectList) then
    raise EBold.CreateFmt(sSourceNotObjectList, [ClassName, Source.ClassName]);
  SourceList := TBoldObjectList(Source);
  if assigned(BoldSystem) then
    BoldSystem.StartTransaction;
  try
    InternalClear;
    Capacity := SourceList.Count;
    if DuplicateMode = bldmAllow then
      with ObjectListController do // using With ObjectListController skips the duplicate check on Insert
        for i := SourceList.Count - 1 downto 0 do
          InsertLocator(0, SourceList.Locators[I])
    else
      for i := SourceList.Count - 1 downto 0 do
        InsertLocator(0, SourceList.Locators[I]);
    if assigned(BoldSystem) then
      BoldSystem.CommitTransaction;
  except
    if assigned(BoldSystem) then
      BoldSystem.RollbackTransaction;
    raise;
  end;
end;

procedure TBoldObjectList.SetLocator(index: Integer; NewLocator: TBoldObjectLocator);
begin
  EnsureContentsCurrent;
  if CheckReplace(index, NewLocator) then
  begin
  BoldClearLastFailure;
    if not CanSetLocator(index, NewLocator, nil) then
      BoldRaiseLastFailure(self, 'SetLocator', '');
    ObjectListController.SetLocator(index, NewLocator);
  end
  else
    Move(IndexOfLocator(NewLocator), index);
end;

function TBoldObjectList.GetBoldObject(index: Integer): TBoldObject;
begin
  result := TBoldObject(GetElement(index));
  Assert(result is TBoldObject);
end;

procedure TBoldObjectList.SetBoldObject(index: Integer; NewObject: TBoldObject);
begin
  SetElement(index, NewObject);
end;

function TBoldObjectList.CheckAdd(NewLocator: TBoldObjectLocator): Boolean;
begin
  if not Assigned(NewLocator) then
    Result := False
  else if OwnedByObject and (BoldSystem <> NewLocator.BoldSystem) then
    result := false
  else
    Result := (DuplicateMode = bldmAllow) or not ObjectListController.IncludesLocator(NewLocator) or DuplicateControl;
end;

function TBoldObjectList.CheckReplace(index: Integer; NewLocator: TBoldObjectLocator): Boolean;
begin
  if not assigned(NewLocator) then
    result := false
  else if OwnedByObject and (BoldSystem <> NewLocator.BoldSystem) then
    result := false
  else if not ObjectListController.IncludesLocator(NewLocator) then
    Result := True
  else if IndexOfLocator(NewLocator) = index then
    Result := False
  else
    Result := DuplicateControl;
end;

function TBoldObjectList.CheckInsert(index: Integer;
  NewLocator: TBoldObjectLocator): Boolean;
begin
  assert(assigned(NewLocator), 'nil not allowed, should have been filtered out before');
  if OwnedByObject and (BoldSystem <> NewLocator.BoldSystem) then
    result := false
  else
  Result := (DuplicateMode = bldmAllow) or not ObjectListController.IncludesLocator(NewLocator) or DuplicateControl;
end;

procedure TBoldObjectList.AddLocator(NewLocator: TBoldObjectLocator);
begin
  EnsureContentsCurrent;
  if CheckAdd(NewLocator) then
  begin
    BoldClearLastFailure;
    if not CanInsertLocator(-1, NewLocator, nil) then
      BoldRaiseLastFailure(self, 'AddLocator', '');
    ObjectListController.AddLocator(NewLocator);
  end;
end;

procedure TBoldObjectList.Add(BoldObject: TBoldObject);
begin
  AddElement(BoldObject);
end;

procedure TBoldObjectList.Move(CurIndex, NewIndex: Integer);
begin
  if CurIndex = NewIndex then
    exit;
  if not mutable then MutableError('MoveElementInList');
  BoldClearLastFailure;
  if not CanMove(CurIndex, NewIndex, nil) then
    BoldRaiseLastFailure(self, 'Move', '');
  EnsureContentsCurrent;
  ListController.Move(CurIndex, NewIndex);
end;

procedure TBoldObjectList.InsertLocator(index: Integer; Locator: TBoldObjectLocator);
begin
  EnsureContentsCurrent;
  if Assigned(Locator) then
  begin
    if CheckInsert(index, Locator) then
    begin
    BoldClearLastFailure;
      if not CanInsertLocator(index, Locator, nil) then
        BoldRaiseLastFailure(self, 'InsertLocator', '');
      ObjectListController.InsertLocator(index, Locator)
    end
    else
      Move(IndexOfLocator(Locator), index);
  end;
end;

procedure TBoldObjectList.Insert(index: Integer; BoldObject: TBoldObject);
begin
  InsertElement(index, BoldObject);
end;

function TBoldObjectList.IndexOf(BoldObject: TBoldObject): Integer;
begin
  result := IndexOfElement(BoldObject);
end;

procedure TBoldObjectList.InternalRemoveByIndex(index: Integer);
begin
  EnsureContentsCurrent;
  ListController.RemoveByIndex(index);
end;

procedure TBoldObjectList.RemoveByIndex(index: Integer);
begin
  if not mutable then MutableError(IntToStr(index));
  BoldClearLastFailure;
  if not CanRemove(index, nil) then
    BoldRaiseLastFailure(self, 'RemoveByIndex', '');
  InternalRemoveByIndex(index);
end;

procedure TBoldObjectList.RemoveLocator(ALocator: TBoldObjectLocator);
var
  i: integer;
begin
  i := IndexOfLocator(ALocator);
  if i <> -1 then
     RemoveByIndex(i);
end;

function TBoldObjectList.IndexOfLocator(Locator: TBoldObjectLocator): Integer;
begin
  BoldClearLastFailure;
  if not CanRead(nil) then
    BoldRaiseLastFailure(self, 'IndexOfLocator', '');
  EnsureContentsCurrent;
  result := ObjectListController.IndexOfLocator(Locator);
end;

function TBoldObjectList.Includes(BoldObject: TBoldObject): Boolean;
begin
  result := IncludesElement(BoldObject);
end;

function TBoldObjectList.LeastCommonClassType: TBoldClassTypeInfo;
var
  i: integer;
begin
  Result := nil;
  if Empty then
    exit;
  Result := Locators[0].BoldClassTypeInfo;
  for i := 1 to Count - 1 do
    Result := Result.LeastCommonSuperClass(Locators[i].BoldClassTypeInfo);
end;

function TBoldObjectList.LocatorInList(NewLocator: TBoldObjectLocator): Boolean;
begin
  BoldClearLastFailure;
  if not CanRead(nil) then
    BoldRaiseLastFailure(self, 'LocatorInList', '');
  EnsureContentsCurrent;
  result := ObjectListController.IncludesLocator(NewLocator);
end;

function TBoldObjectList.CreateObjectIdList(WithoutDuplicates: boolean): TBoldObjectIdList;
var
  I: Integer;
begin
  Result := TBoldObjectIdList.Create;
  Result.Capacity := Count;
  for I := 0 to Count - 1 do
    if WithoutDuplicates and (DuplicateMode = bldmAllow) then
      Result.AddIfNotInList(Locators[I].BoldObjectID)
    else
      Result.Add(Locators[I].BoldObjectID);
end;

procedure TBoldObjectList.FillFromIDList(ObjectIdList: TBoldObjectIdList; BoldSystem: TBoldSystem);
var
  I: Integer;
begin
  if ObjectIdList.Count > 0 then
  begin
    Assert(ObjectListController is TBoldObjectListController, 'TBoldObjectList.FillFromIDList: Unsupported Controller: ' + ObjectListController.ClassName);
    Capacity := ObjectIdList.Count;
    if BeginUpdate then
    try
    for I := 0 to ObjectIdList.Count - 1 do
      AddLocator(BoldSystem.EnsuredLocatorByID[ObjectIdList[I]]);
    finally
      EndUpdate;
    end;
  end;
end;

function TBoldObjectList.GetFirst: TBoldObject;
begin
  result := inherited First as TBoldObject;
end;

function TBoldObjectList.GetLast: TBoldObject;
begin
  result := inherited Last as TBoldObject;
end;

function TBoldObjectList.GetListElementTypeInfo: TBoldClassTypeInfo;
begin
  if Assigned(BoldType) then
    result := (BoldType as TBoldListTypeInfo).ListElementTypeInfo as TBoldClassTypeInfo
  else
    result := nil;
end;

function TBoldObjectList.FilterOnType(BoldClassTypeInfo: TBoldClassTypeInfo; IncludeSubclasses: boolean): TBoldObjectList;
var
  I: Integer;
  iTopSortedIndex: Integer;
begin
  result := TBoldObjectList.CreateWithTypeInfo(BoldClassTypeInfo);
  result.DuplicateMode := bldmAllow;
  iTopSortedIndex := BoldClassTypeInfo.TopSortedIndex;
  for I := 0 to Count - 1 do
  begin
    if (Locators[I].BoldObjectID.TopSortedIndex = iTopSortedIndex) or (IncludeSubclasses
      and Locators[I].BoldClassTypeInfo.BoldIsA(BoldClassTypeInfo)) then
      Result.AddLocator(Locators[I]);
  end;
end;

procedure TBoldObjectList.RemoveDeletedObjects;
var
  idList: TBoldObjectIdList;
  i: integer;
begin
  if not Assigned(BoldSystem) then
    raise EBold.CreateFmt('%s.RemoveDeletedObjects: BoldSystem is needed to remove deleted objects.', [ClassName]);
  EnsureContentsCurrent;
  idList := CreateObjectIdList;
  try
    BoldSystem.RemoveDeletedObjects(IdList);
    for I := count -1 downto 0 do
      if not idList.Includes(self[i]) then
        self.RemoveByIndex(i);
  finally
    idList.Free;
  end;
end;

function TBoldObjectList.GetByIndexAndSubscribe(MemberList: TBoldMemberList; Subscriber: TBoldSubscriber): TBoldObject;
var
  Locator: TBoldObjectLocator;
begin
  EnsureContentsCurrent;
  BoldClearLastFailure;
  if not CanRead(nil) then
    BoldRaiseLastFailure(self, 'GetByIndex', '');
  Locator := ObjectListController.GetLocatorByQualifiersAndSubscribe(MemberList, Subscriber);
  if Assigned(Locator) then
    result := Locator.EnsuredBoldObject
  else
    Result := nil;
end;

function TBoldObjectList.GetByIndex(MemberList: TBoldMemberList): TBoldObject;
begin
  result := GetByIndexAndSubscribe(MemberList, nil);
end;

function TBoldObjectList.VerifyClass(aLocator: TBoldObjectLocator): Boolean;
var
  AllowedClass, KnownClass: TBoldClassTypeInfo;
begin
  if Assigned(BoldType) and assigned(aLocator) then
  begin
    Assert(BoldType is TBoldListTypeInfo);
    Assert(TBoldListTypeInfo(BoldType).ListElementTypeInfo is TBoldClassTypeInfo);
    Assert(BoldType.SystemTypeInfo is TBoldSystemTypeInfo);
    AllowedClass := TBoldClassTypeInfo(TBoldListTypeInfo(BoldType).ListElementTypeInfo);
    if AllowedClass.TopSortedIndex = 0 then // it's the root class so we allow anything
    begin
      Result := True;
    end
    else
    begin
      KnownClass := aLocator.BoldClassTypeInfo;
      if KnownClass.ConformsTo(AllowedClass) then
        Result := true
      else if aLocator.BoldObjectId.TopSortedIndexExact then
        Result := false
      else
        Result := AllowedClass.ConformsTo(knownclass);
      if not result then
        SetBoldLastFailureReason(TBoldFailureReason.CreateFmt(sItemNotAllowedInList,  [aLocator.debuginfo, self.debuginfo], self));
    end;
  end
  else
    result := true;
end;

function TBoldObjectList.ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean;
begin
  assert(assigned(ListController));
  result := ListController.ProxyInterface(IId, Mode, Obj);
  if not result then
    result := inherited ProxyInterface(IId, Mode, Obj);
end;

procedure TBoldObjectList.MakeDbCurrent;
begin
  ObjectListController.MakeDbCurrent;
end;

function TBoldObjectList.GetStreamName: String;
begin
  Result := ObjectListController.StreamName;
end;

function TBoldObjectList.GetFreeStandingClass: TBoldFreeStandingElementClass;
begin
  Result := ObjectListController.GetFreeStandingClass;
end;

procedure TBoldObjectList.EnsureRange(FromIndex, ToIndex: integer);

  procedure CheckObjects(min, max: integer);
  var
  FetchList: TBoldObjectList;
  i: integer;
  aSystem: TBoldSystem;
  Locator: TBoldObjectLocator;
    FullRange: boolean;
  begin
    aSystem := self.BoldSystem;
    FetchList := nil;
    FullRange := Count = max-min+1;
    if FullRange then
    begin
      for i := min to max do
      begin // first pass check if empty
        if not assigned(aSystem) then
          aSystem := Locators[i].BoldSystem;
        if Assigned(Locators[i].BoldObject) then
        begin
          FullRange := false;
          break
    end;
      end;
      if FullRange and assigned(aSystem) then
      begin
        aSystem.SystemPersistenceHandler.FetchList(self);
        exit;
      end;
    end;
    try
      aSystem := nil;
      for i := min to max do
      begin
        Locator := Locators[i];
        if (not assigned(Locator.BoldObject) or Locator.BoldObject.EffectiveInvalid) and not Locator.BoldObjectID.NonExisting then
        begin
          if not assigned(aSystem) then
          begin
            aSystem := Locator.BoldSystem;
            FetchList := TBoldObjectList.Create;
            FetchList.SubscribeToObjectsInList := false;
            FetchList.DuplicateMode := bldmAllow;
            FetchList.Capacity := max-min+1;
          end;
          assert(Locator.BoldSystem = aSystem);
          FetchList.AddLocator(Locator);
        end;
      end;
      if assigned(aSystem) then
        aSystem.SystemPersistenceHandler.FetchList(FetchList);
    finally
      FreeAndNil(FetchList);
    end;
  end;

  function RestrictRange(value, min, max: integer): integer; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
  begin
    if value < min then
      result := min
    else if value > max then
      result := max
    else
      result := value;
  end;
var
  vCount, x,y: integer;
begin
  vCount := count;
  if vCount = 0 then
    exit;
  x := RestrictRange(FromIndex, 0, vCount - 1);
  if ToIndex = -1 then
    y := vCount - 1
  else
  y := RestrictRange(ToIndex, 0, vCount - 1);
  CheckObjects(x,y);
end;

procedure TBoldObjectList.EnsureObjects;
begin
  EnsureRange(0, Count - 1);
end;

function TBoldObjectList.CanInsertLocator(index: Integer; Locator: TBoldObjectLocator; Subscriber: TBoldSubscriber): Boolean;
begin
  result := VerifyClass(Locator);
{$IFNDEF BOLD_NO_QUERIES}
  result := result and SendQuery(bqMayInsert, [index, Locator], Subscriber);
{$ENDIF}
end;

function TBoldObjectList.CanInsert(index: Integer; Element: TBoldElement; Subscriber: TBoldSubscriber): Boolean;
begin
  result := (not assigned(Element)) or
            ((Element is TBoldObject) and
             CanInsertLocator(index, TBoldObject(Element).BoldObjectLocator, Subscriber));
end;

function TBoldObjectList.CanSet(index: Integer; Element: TBoldElement; Subscriber: TBoldSubscriber): Boolean;
begin
  result := assigned(Element) and
            (Element is TBoldObject) and
            CanSetLocator(index, TBoldObject(Element).BoldObjectLocator, Subscriber);
end;

function TBoldObjectList.CanSetLocator(index: Integer; Locator: TBoldObjectLocator; Subscriber: TBoldSubscriber): Boolean;
begin
  result := VerifyClass(Locator);
{$IFNDEF BOLD_NO_QUERIES}
  result := result and SendQuery(bqMayReplace, [index, Locator], Subscriber);
{$ENDIF}
end;

constructor TBoldObjectList.InternalCreateClassList(System: TBoldSystem; ListTypeInfo: TBoldListTypeInfo);
begin
  inherited CreateWithOwner(system);
  InitializeNonObjectOwned(ListTypeInfo);
  SetInternalState(BoldDuplicateModeMask, BoldDMShift, Integer(bldmMerge{bldmError}));
  InitializeStateToInvalid;
end;

procedure TBoldObjectList.Initialize;
begin
  inherited Initialize;
  DuplicateMode := bldmMerge;
  if not assigned(BoldMemberRTInfo) then
  begin
    if assigned(OwningElement) and (OwningElement is TBoldSystem) then
      ListController := TBoldClassListController.Create(Self)
    else
    begin
      ListController := TBoldObjectListController.Create(self);
      SubscribeToObjectsInList := true;
    end;
  end
  else
  begin
    if BoldMemberRTInfo is TBoldRoleRTInfo then
      if BoldMemberRTInfo.IsDerived then
        ListController := TBoldObjectListController.Create(self)
      else if TBoldRoleRTInfo(BoldMemberRTInfo).RoleType = rtLinkRole then
        ListController := TBoldLinkObjectListController.Create(Self)
      else if TBoldRoleRTInfo(BoldMemberRTInfo).IsIndirect then
        ListController := TBoldIndirectMultiLinkController.Create(Self)
      else
        ListController := TBoldDirectMultiLinkController.Create(Self)
    else
      raise EBold.CreateFmt(sCannotCreateController, [ClassName]);
  end;
end;

function TBoldObjectList.GetBoldRoleRTInfo: TBoldRoleRTInfo;
begin
  Result := TBoldRoleRTInfo(BoldMemberRTInfo);
  Assert((Result = nil) or (Result is TBoldRoleRTInfo));
end;

function TBoldObjectList.GetSubscribeToObjectsInList: Boolean;
begin
  result := GetElementflag(befSubscribeToObjectsInList);
end;

procedure TBoldObjectList.SetSubscribeToObjectsInList(const Value: Boolean);
begin
  if ListController.ClassType = TBoldObjectListController then
  begin
    if value <> SubscribeToObjectsInList then
    begin
      SetElementFlag(befSubscribeToObjectsInList, value);
      (ListController as TBoldObjectListController).DropSubscriptions;
      (ListController as TBoldObjectListController).Resubscribe;
    end;
  end
  else
    raise EBold.CreateFmt(sCanOnlyChangeForStandAloneLists, [ClassName, 'SetSubscribeToObjectsInList']); // do not localize
end;

function TBoldObjectList.GetSubscribeToLocatorsInList: Boolean;
begin
  result := GetElementflag(befSubscribeToLocatorsInList);
end;

procedure TBoldObjectList.SetSubscribeToLocatorsInList(const Value: Boolean);
begin
  if ListController.ClassType = TBoldObjectListController then
  begin
    if value <> SubscribeToLocatorsInList then
    begin
      SetElementFlag(befSubscribeToLocatorsInList, value);
      (ListController as TBoldObjectListController).DropSubscriptions;
      (ListController as TBoldObjectListController).Resubscribe;
    end;
  end
  else
    raise EBold.CreateFmt(sCanOnlyChangeForStandAloneLists, [ClassName, 'SetSubscribeToLocatorsInList']); // do not localize
end;

procedure TBoldObjectList.AssignCloneValue(AClone: TBoldMember);
var
    I: Integer;
  Source: TBoldObjectListController;
  MultiLinkController: TBoldIndirectMultiLinkController;
  LinkObjectListController: TBoldLinkObjectListController;
  DirectMultiLinkController: TBoldDirectMultiLinkController;
  Destination: TBoldObjectListController;
begin
  TBoldObjectList(AClone).Capacity := self.Count;
  Assert(TBoldObjectList(AClone).ObjectListController is TBoldObjectListController);
  Destination := (TBoldObjectList(AClone).ObjectListController as TBoldObjectListController);
  if (ObjectListController is TBoldObjectListController) then
  begin
    Source := ObjectListController as TBoldObjectListController;
    for I := 0 to Source.Count - 1 do
      Destination.LocatorList.Add(Source.LocatorList[i]);
  end
  else
  if (ObjectListController is TBoldDirectMultiLinkController) then
  begin
    DirectMultiLinkController := ObjectListController as TBoldDirectMultiLinkController;
    for I := 0 to DirectMultiLinkController.Count - 1 do
      Destination.LocatorList.Add(DirectMultiLinkController.GetLocator(i));
  end
  else
  if (ObjectListController is TBoldIndirectMultiLinkController) then
  begin
    MultiLinkController := ObjectListController as TBoldIndirectMultiLinkController;
    for I := 0 to MultiLinkController.Count - 1 do
      Destination.LocatorList.Add(MultiLinkController.GetLocator(i));
  end
  else
  if (ObjectListController is TBoldLinkObjectListController) then
  begin
    LinkObjectListController := (ObjectListController as TBoldLinkObjectListController);
    for I := 0 to LinkObjectListController.Count - 1 do
      Destination.LocatorList.Add(LinkObjectListController.GetLocator(i));
  end
  else
    raise EBold.CreateFmt('%s.Clone: Unknown controller: ', [ClassName, ObjectListController.ClassName]);
end;

function TBoldObjectList.Clone(ACopyDuplicateMode: boolean; ASubscribeToObjectsInList: boolean): TBoldObjectList;
begin
  if Assigned(BoldType) then
    Result := TBoldMemberFactory.CreateMemberFromBoldType(BoldType) as TBoldObjectList
  else
    Result := TBoldMemberClass(ClassType).Create as TBoldObjectList;
  if assigned(BoldSystem) then
    BoldSystem.StartTransaction;
  try
    Result.BoldPersistenceState := bvpsTransient;
    TBoldObjectList(Result).SubscribeToObjectsInList := ASubscribeToObjectsInList;
    if Count > 0 then
    begin
      Result.SetInternalState(BoldDuplicateModeMask, BoldDMShift, Integer(bldmAllow));
      AssignCloneValue(result);
    end;
    if ACopyDuplicateMode then
      Result.SetInternalState(BoldDuplicateModeMask, BoldDMShift, Integer(DuplicateMode));
    if assigned(BoldSystem) then
      BoldSystem.CommitTransaction;
  except
    Result.Free;
    if assigned(BoldSystem) then
      BoldSystem.RollbackTransaction;
    raise;
  end;
end;

function TBoldObjectList.Clone: TBoldMember;
begin
  result := Clone(True, SubscribeToObjectsInList);
end;

function TBoldObjectList.GetProxy(Mode: TBoldDomainElementProxyMode): TBoldMember_Proxy;
begin
  result := ObjectListController.GetProxy(Self, Mode);
end;

function TBoldObjectList.AtTime(Time: TBoldTimestampType): TBoldMember;
begin
  if ObjectListController.HandlesAtTime then
    result := ObjectListController.AtTime(Time)
  else
    result := inherited AtTime(Time);
end;

function TBoldObjectList.BeginUpdate: boolean;
begin
//  result := ObjectListController.StartModify;
//  SendEvent(beBeginUpdate);
  result := true;
end;

procedure TBoldObjectList.EndUpdate;
begin
//  ObjectListController.EndModify;
//  SendEvent(beEndUpdate);
end;

procedure TBoldObjectList.FreeContent;
begin
  inherited;
  ObjectListController.FreeContent;
end;

procedure TBoldObjectList.AssignContentValueFromElement(source: TBoldElement);
var
  SourceList: TBoldObjectList;
begin
  if (source is TBoldObjectList) then
  begin
    SourceList := source as TBoldObjectList;
    Clear;
    if SourceList.Count = 0 then
      exit;
    Capacity := SourceList.Count;
    AddList(SourceList);
  end
  else
    inherited;
end;

procedure TBoldObjectList.PrepareClear;
begin
  ObjectListController.PrepareClear;
end;

procedure TBoldObjectList.InternalClear;
begin
  ObjectListController.Clear;
end;

procedure TBoldObjectList.DeleteObjects;
var
  vBoldSystem: TBoldSystem;

  procedure FetchClass(AList: TBoldObjectList; AClass: TBoldClassTypeInfo);
  var
    i: integer;
    vRoleRTInfo: TBoldRoleRTInfo;
  begin
    for i := 0 to AClass.AllRoles.Count - 1 do
    begin
      vRoleRTInfo := AClass.AllRoles[i];
      if not vRoleRTInfo.IsDerived then
      begin
        case vRoleRTInfo.DeleteAction of
          daCascade, daAllow:
          begin
            vBoldSystem.FetchLinksWithObjects(AList, vRoleRTInfo.ExpressionName);
          end;
        end;
      end;
    end;
  end;

var
  vCommonClass: TBoldClassTypeInfo;
  vSubClass: TBoldClassTypeInfo;
  vSubclasses: TList;
  vList: TBoldObjectList;
  i: integer;
begin
  EnsureObjects;
  if empty then
    exit;
  vBoldSystem := BoldObjects[0].BoldSystem;
  if vBoldSystem.BoldPersistent then
  begin
    vCommonClass := LeastCommonClassType;
    FetchClass(self, vCommonClass);
    if vCommonClass.HasSubclasses then
    begin
      vSubclasses := TList.Create;
      try
        for I := 0 to Count - 1 do
          begin
          vSubClass := Locators[i].BoldObject.BoldClassTypeInfo;
          if vSubClass <> vCommonClass then
          begin
            if vSubclasses.IndexOf(vSubClass) = -1 then
            begin
              vSubclasses.Add(vSubClass);
              vList := FilterOnType(vSubClass, true);
              try
                FetchClass(vList, vSubClass);
              finally
                vList.free;
              end;
            end;
          end;
        end;
      finally
        vSubclasses.free;
      end;
    end;
  end;
  vList := nil;
  vBoldSystem.StartTransaction();
  try
    vList := self.Clone as TBoldObjectList;
    vList.SubscribeToObjectsInList := false;
    vList.SubscribeToLocatorsInList := false;
    for I := vList.Count - 1 downto 0 do
      if not vList[i].BoldObjectIsDeleted then
        vList[i].Delete
  finally
    vList.free;
    vBoldSystem.CommitTransaction();
  end;
end;

function TBoldObjectList.IsEqualToValue(const Value: IBoldValue): Boolean;
var
  IdListRef: IBoldObjectIdListRef;
  IdListRefPair: IBoldObjectIdListRefPair;
  i: integer;
begin
  result := false;
  Assert(Assigned(Value), ClassName + '.IsEqualToValue: Value can not be nil.');
  if Supports(Value, IBoldObjectIdListRef, IdListRef) then
  begin
    result := IdListRef.Count = count;
    if result then
    for I := 0 to Count - 1 do
    begin
      result := result and Locators[i].BoldObjectID.IsEqual[IdListRef.IdList[i]];
      if not result then
        exit;
    end;
  end
  else if Supports(Value, IBoldObjectIdListRefPair, IdListRefPair) then
  begin
    result := IdListRefPair.count = count;
    if result then
    for I := 0 to Count - 1 do
    begin
      result := result and Locators[i].BoldObjectID.IsEqual[IdListRefPair.IdList2[i]];
      if not result then
        exit;
    end;
  end;
end;

{ TBoldListController }

constructor TBoldListController.Create(OwningList: TBoldList);
begin
  fOwningList := OwningList;
end;

function TBoldListController.AssertIntegrity: Boolean;
var
  i: integer;
begin
  for i := 0 to Count-1 do
  begin
    self[i].DebugInfo;
    Assert(self[i] is TBoldElement);
  end;
  result := true;
end;

function TBoldListController.CreateNew: TBoldElement;
begin
  raise EBold.Create(sCannotCreateNewElement);
end;

function TBoldListController.GetCanCreateNew: Boolean;
begin
  result := false;
end;

function TBoldListController.GetCapacity: integer;
begin
  result := MaxInt;
end;

function TBoldListController.GetOwningMember: TBoldMember;
begin
  Result := OwningList;
end;

function TBoldListController.GetStringrepresentation: String;
begin
  OwningList.EnsureContentsCurrent;
  Result := IntToStr(Count);
end;

procedure TBoldListController.SetCapacity(const Value: integer);
begin
  // nothing
end;

function TBoldListController.GetIsEmpty: Boolean;
begin
  result := Count = 0;
end;

{ TBoldAbstractObjectListController }

procedure TBoldAbstractObjectListController.AddElement(Element: TBoldElement);
begin
  Assert(Element is TBoldObject);
  AddLocator(TBoldObject(Element).BoldObjectLocator);
end;

function TBoldAbstractObjectListController.AtTime(Time: TBoldTimestampType): TBoldMember;
begin
  raise EBoldInternal.CreateFmt('%s.AtTime: Illegal call. Does not handle AtTime.', [classname]);
end;

procedure TBoldAbstractObjectListController.Clear;
var
  i: integer;
begin
  for I := Count - 1 downto 0 do
    RemoveByIndex(I);
end;

procedure TBoldAbstractObjectListController.FreeContent;
begin
end;

function TBoldAbstractObjectListController.GetDebugInfo: string;
begin
  if Assigned(OwningObjectList) then
    result := Format('%s(%s)', [ClassName, OwningObjectList.DebugInfo])
  else
    result := inherited GetDebugInfo
end;

function TBoldAbstractObjectListController.GetElement(index: Integer): TBoldElement;
var
  aLocator: TBoldObjectLocator;
begin
  aLocator := GetLocator(index);
  aLocator.EnsureBoldObject;
  result := aLocator.BoldObject;
end;

function TBoldAbstractObjectListController.GetObjectList: TBoldObjectList;
begin
  result := TBoldObjectList(inherited OwningList);
  Assert(result is TBoldObjectList);
end;

function TBoldAbstractObjectListController.HandlesAtTime: Boolean;
begin
  result := false;
end;

function TBoldAbstractObjectListController.IncludesElement(Item: TBoldElement): Boolean;
begin
  Assert(Item is TBoldObject);
  result := IncludesLocator(TBoldObject(Item).BoldObjectLocator);
end;

function TBoldAbstractObjectListController.IndexOfElement(Item: TBoldElement): Integer;
begin
  Assert(Item is TBoldObject);
  result := IndexOfLocator(TBoldObject(Item).BoldObjectLocator);
end;

procedure TBoldAbstractObjectListController.InsertElement(index: Integer; Element: TBoldElement);
begin
  Assert(Element is TBoldObject);
  InsertLocator(index, TBoldObject(Element).BoldObjectLocator);
end;

procedure TBoldAbstractObjectListController.PrepareClear;
begin
end;

procedure TBoldAbstractObjectListController.SetElement(index: Integer; Value: TBoldElement);
begin
  Assert(Value is TBoldObject);
  SetLocator(index, TBoldObject(Value).BoldObjectLocator);
end;

{ TBoldAbstractObjectReferenceController }

function TBoldAbstractObjectReferenceController.ControllerForLinkRole: TBoldAbstractObjectReferenceController;
var
  LinkMember: TBoldMember;
begin
  LinkMember := OwningReference.OwningObject.BoldMembers[OwningReference.BoldRoleRtInfo.Index + 1];
  result := (LinkMember as TBoldObjectReference).ReferenceController;
end;

function TBoldAbstractObjectReferenceController.ControllerForMainRole: TBoldAbstractObjectReferenceController;
var
  LinkMember: TBoldMember;
begin
  LinkMember := OwningReference.OwningObject.BoldMembers[OwningReference.BoldRoleRtInfo.IndexOfMainRole];
  result := (LinkMember as TBoldObjectReference).ReferenceController;
end;

constructor TBoldAbstractObjectReferenceController.Create(Owner: TBoldObjectReference);
begin
  fOwningReference := Owner;
end;


function TBoldAbstractObjectReferenceController.GetOwningMember: TBoldMember;
begin
  Result := OwningReference;
end;

function TBoldAbstractObjectReferenceController.MayUpdate: Boolean;
begin
  result := True;
end;

function TBoldAbstractObjectReferenceController.OtherEndControllerForLinkObject(Obj: TBoldObject): TBoldAbstractObjectReferenceController;
var
  OtherEndMember: TBoldMember;
begin
  OtherEndMember := Obj.BoldMembers[OwningReference.BoldRoleRtInfo.OtherIndexInLinkClass];
  result := (OtherEndMember as TBoldObjectReference).ReferenceController;
end;

procedure TBoldAbstractObjectReferenceController.PreDiscard;
begin
end;

function TBoldObjectReferenceController.GetProxy(Member: TBoldMember; Mode: TBoldDomainElementProxyMode): TBoldMember_Proxy;
begin
  result := TBoldObjectReference_Proxy.MakeProxy(Member, Mode);
end;

{ TBoldAbstractController }

function TBoldAbstractController.GetBoldSystem: TBoldSystem;
begin
  result := OwningMember.BoldSystem;
end;

function TBoldAbstractController.AssertIntegrity: Boolean;
begin
  Result := True;
end;

procedure TBoldAbstractController.Changed(Event: TBoldEvent;
  const Args: array of const);
begin
  OwningMember.Changed(Event, args);
end;

procedure TBoldAbstractController.DbFetchOwningMember;
begin
 if OwningMember.OwningObject.BoldObjectLocator.BoldObjectID.IsStorable then
    OwningMember.OwningObject.BoldSystem.SystemPersistenceHandler.FetchMember(OwningMember)
  else
    raise EBoldInternal.CreateFmt('%s.DbFetchOwningMember: Member belongs to object with internal ID', [OwningMember.BoldType.AsString]);
end;

procedure TBoldAbstractController.EndModify;
begin
  OwningMember.EndModify;
end;

function TBoldAbstractController.NewValueInOptimisticLocking: IBoldValue;
begin
  result := TBoldAbstractOldValueHandler.NewValueInValueSpace(OwningMember, OwningMember.OwningObject.BoldSystem.fOldValueHandler.OldValues);
end;

class function TBoldAbstractController.GetControllerForMember(Member: TBoldMember): TBoldAbstractController;
begin
  result := Member.GetController;
end;

function TBoldAbstractController.GetFreeStandingClass: TBoldFreeStandingElementClass;
begin
  raise EBoldInternal.Createfmt('%s.GetFreeStandingClass: Method is abstract, please implement', [classname]);
end;

function TBoldAbstractController.GetOwningObject: TBoldObject;
begin
  result := OwningMember.OwningObject;
end;

function TBoldAbstractController.GetRoleRTInfo: TBoldRoleRTInfo;
begin
  result := OwningMember.BoldMemberRtInfo as TBoldRoleRTInfo;
end;

procedure TBoldAbstractController.linkto(NewLocator: TBoldObjectLocator; updateOrderNo: Boolean; Mode: TBoldLinkUnlinkMode);
begin
end;

function TBoldAbstractController.LocatorForID(
  ObjectId: TBoldObjectId): TBoldObjectLocator;
begin
  if ObjectId = nil then
    Result := nil
  else
    Result := OwningMember.BoldSystem.EnsuredLocatorById[ObjectId];
end;

procedure TBoldAbstractController.PreChange;
begin
  OwningMember.PreChange;
end;

function TBoldAbstractController.ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean;
begin
  result := false;
end;

function TBoldAbstractController.StartModify: Boolean;
begin
  result := Owningmember.StartModify;
end;

procedure TBoldAbstractController.Unlink(OldLocator: TBoldObjectLocator; Mode: TBoldLinkUnlinkMode);
begin
end;

function TBoldAbstractController.AssertedLocatorForID(ObjectId: TBoldObjectId): TBoldObjectLocator;
begin
  if not Assigned(ObjectId) then
    raise EBoldInternal.Create('Id is nil');
  Result := LocatorForId(ObjectId);
end;

procedure TBoldAbstractController.DbFetchClassForMember(TimeStamp: TBoldTimestampType);
begin
  OwningMember.BoldSystem.SystemPersistenceHandler.FetchClass((OwningMember as TBoldObjectList), TimeStamp);
end;

{ TBoldObjectReferenceController }

procedure TBoldObjectReferenceController.AssignContentValue(const Source: IBoldValue);
var
  s: IBoldObjectIdRef;
begin
  if assigned(source) and (source.QueryInterface(IBoldObjectIDRef, S) = S_OK) then
  begin
    if Assigned(OwningReference) then
      fLocator := OwningReference.BoldSystem.EnsuredLocatorByID[s.Id]
    else
     raise EBold.Create(sObjectRefMustBePartOfObject);
  end
  else
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname, 'AssignContentValue']); // do not localize
end;

constructor TBoldObjectReferenceController.Create(Owner: TBoldObjectReference);
begin
  inherited;
  fSubscriber := TBoldPassthroughSubscriber.Create(ObjectChangeReceive);
end;

destructor TBoldObjectReferenceController.Destroy;
begin
  FreeAndNil(fSubscriber);
  inherited;
end;

function TBoldObjectReferenceController.GetFreeStandingClass: TBoldFreeStandingElementClass;
begin
  result := TBFSObjectIdRef;
end;

function TBoldObjectReferenceController.GetLocator: TBoldObjectLocator;
begin
  result := fLocator;
end;

function TBoldObjectReferenceController.GetStreamName: string;
begin
  result := BoldContentName_ObjectIdRef;
end;

procedure TBoldObjectReferenceController.MakeDbCurrent;
begin
  raise EBoldInternal.CreateFmt('%s.MakeDBcurrent: This is only possible for link-controllers', [ClassName]);
end;

procedure TBoldObjectReferenceController.ObjectChangeReceive(
  Originator: TObject; OriginalEvent: TBoldEvent;
  RequestedEvent: TBoldRequestedEvent);
begin
  if (Originator = fLocator.BoldObject) and (OriginalEvent = beDestroying) then
    fLocator := nil;
end;

procedure TBoldObjectReferenceController.SetLocator(NewLocator: TBoldObjectLocator);
begin
  if not StartModify then
    BoldRaiseLastFailure(OwningReference, 'SetLocator', '');

  fLocator := NewLocator;
  Changed(beValueChanged, [NewLocator]);
  EndModify;
end;

{ TBoldLocatorHashIndex }

function TBoldLocatorHashIndex.FindLocatorByLocator(
  BoldObjectLocator: TBoldObjectLocator): TBoldObjectLocator;
begin
  Result := TBoldObjectLocator(Find(BoldObjectLocator));
end;

function TBoldLocatorHashIndex.Hash(const Key): Cardinal;
begin
  Result := TBoldObjectLocator(Key).Hash;
end;

function TBoldLocatorHashIndex.HashItem(Item: TObject): Cardinal;
begin
  Result := ItemAsLocator(Item).Hash;
end;

function TBoldLocatorHashIndex.ItemAsLocator(Item: TObject): TBoldObjectLocator;
begin
  Result := TBoldObjectLocator(Item);
  Assert(result is TBoldObjectLocator);
end;

function TBoldLocatorHashIndex.Match(const Key; Item: TObject): Boolean;
begin
  Result := TBoldObjectLocator(Key) = (ItemAsLocator(Item));
end;

function TBoldObjectList.GetElementTypeInfoForType: TBoldElementTypeInfo;
var
  aSystem: TBoldSystem;
begin
  result := nil;
  //TODO: check (System.BoldSystemTypeInfo.UseGeneratedCode)
  if (ClassType <> TBoldObjectList) then // this will not work with systems that have no generated code
  begin
    aSystem := FindASystem;
    if assigned(aSystem) then
      result := aSystem.BoldSystemTypeInfo.ListTypes.ItemByListClass[self.ClassType]
  end;
end;

function TBoldObjectList.GetEnumerator: TBoldObjectListEnumerator;
begin
  result := TBoldObjectListEnumerator.Create(self)
end;

function TBoldObjectList.GetLocatorEnumerator: TBoldObjectListLocatorEnumerator;
begin
  result := TBoldObjectListLocatorEnumerator.Create(self)
end;

function TBoldAttribute.GetElementTypeInfoForType: TBoldElementTypeInfo;
begin
  if FindASystem <> nil then
    result := GetAttributeTypeInfoForType
  else
    result := nil;
end;

procedure TBoldAttribute.RecycleValue;
begin
  if Assigned(OwningObject) then
    raise EBold.Create(sCannotRecyclePartOfObject);
  PrepareToDestroy;
  SetElementFlag(befImmutable, False);
end;

{ TBoldFailureReason }

constructor TBoldFailureReason.create(AReason: String;
  Originator: TBoldElement);
begin
  fReason := AReason;
  fOriginator := Originator;
  if assigned(fOriginator) then
  begin
    fSubscriber := TBoldPassthroughSubscriber.create(ReceiveOriginatorDestroy);
    Originator.AddSmallSubscription(fSubscriber, [bedestroying], bedestroying);
  end;
end;

constructor TBoldFailureReason.CreateFmt(Reason: string;
  const args: array of const; Originator: TBoldElement);
begin
  Create(Format(Reason, Args), Originator);
end;

destructor TBoldFailureReason.Destroy;
begin
  inherited;
  FreeAndNil(fSubscriber);
end;

function TBoldFailureReason.GetException(const Msg: String): EBoldFailure;
begin
  Result := EBoldFailure.Create(Msg);
end;

procedure TBoldFailureReason.ReceiveOriginatorDestroy(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  if (OriginalEvent = beDestroying) and (Originator = self.Originator) then
    self.fOriginator := nil;
end;

{ EBoldFailure }

destructor EBoldFailure.Destroy;
begin
  freeAndNil(fReasonObject);
  inherited;
end;

{ TBoldMemberFactory }

class function TBoldMemberFactory.CreateMemberFromBoldType(BoldType: TBoldElementTypeInfo): TBoldMember;
begin
  if not assigned(BoldType) then
    raise EBold.CreateFmt(sInvalidBoldType_Nil, [ClassName]);
  if BoldType is TBoldListTypeInfo then
    result := TBoldMemberClass(TBoldListTypeInfo(BoldType).ListClass).CreateWithTypeInfo(BoldType)
  else
  begin
    if (BoldType.ElementClass = nil) then
      raise EBold.CreateFmt(sInvalidBoldType, [ClassName, BoldType.ClassName]);
    result := BoldType.CreateElement as TBoldMember;
  end;
end;

class function TBoldMemberFactory.CreateMemberFromExpressionName(SystemTypeInfo: TBoldSystemTypeInfo; const Name: String): TBoldMember;
var
  BoldType: TBoldElementTypeInfo;
begin
  BoldType := SystemTypeInfo.ElementTypeInfoByExpressionName[Name];
  if not assigned(BoldType) then
    raise EBold.CreateFmt(sUnableToFindType, [classname, name]);
  result := CreateMemberFromBoldType(BoldType);
end;

function TBoldObjectLocator.GetObjectIsPersistent: Boolean;
begin
  result := not assigned(BoldObject) or BoldObject.BoldPersistent;
end;

procedure TBoldObjectLocator.AddToLocators;
begin
  Assert(Assigned(self.boldObjectId));
  BoldSystem.Locators.Add(Self);
end;

function TBoldObjectLocator.AtTime(Time: TBoldTimeStampType): TBoldObjectLocator;
var
  NewId: TBoldObjectId;
begin
  if Time = BoldObjectID.TimeStamp then
    result := self
  else if BoldObjectID is TBoldDefaultId then
  begin
    NewId := (BoldObjectID as TBoldDefaultId).CloneWithTimestamp(Time);
    try
      result := BoldSystem.EnsuredLocatorByID[NewId];
    finally
      NewId.Free;
    end;
  end
  else
    result := nil;
end;

{ TBoldMember_Proxy }

procedure TBoldMember_Proxy.AssignContent(const Source: IBoldValue);
begin
  Assert(assigned(Source), 'TBoldMember_Proxy.AssignContent: Source = nil.');
  AssignContentValue(Source);
  Assert ((Mode <> bdepPMIn) or (Source.BoldPersistenceState = bvpsCurrent));
  ProxedMember.SetBoldPersistenceState(Source.BoldPersistenceState);
end;

constructor TBoldMember_Proxy.Create(ProxedMember: TBoldMember;
  Mode: TBoldDomainElementProxyMode);
begin
  inherited Create(Mode);
  fProxedMember := ProxedMember;
end;

function TBoldMember_Proxy.GetBoldPersistenceState: TBoldValuePersistenceState;
begin
  result := ProxedMember.BoldPersistenceState;
end;

function TBoldMember_Proxy.GetContentType: TBoldValueContentType;
begin
  result := ProxedMember.GetFreeStandingClass.ContentType;
end;

function TBoldMember_Proxy.GetStringRepresentation(
  representation: integer): String;
begin
  result := ProxedMember.AsString;
end;

function TBoldMember_Proxy.GetContentAsString: String;
begin
  result := GetStringRepresentation(brDefault);
end;

function TBoldMember_Proxy.GetContentName: String;
begin
  result := ProxedMember.GetStreamName;
end;

function TBoldMember_Proxy.GetFreeStandingClass: TBoldFreeStandingElementClass;
begin
  result := ProxedMember.GetFreeStandingClass;
end;

function TBoldMember_Proxy.GetStreamName: String;
begin
  result := ProxedMember.GetStreamName;
end;

class function TBoldMember_Proxy.MakeProxy(ProxedMember: TBoldMember;
  Mode: TBoldDomainElementProxyMode): TBoldMember_Proxy;
begin
  Result := Self.Create(ProxedMember, Mode);
end;

procedure TBoldMember_Proxy.Retarget(ProxedMember: TBoldMember;
  Mode: TBoldDomainElementProxyMode);
begin
  inherited Retarget(Mode);
  fProxedMember := ProxedMember;
end;

function TBoldMember_Proxy.GetProxedController: TBoldAbstractController;
begin
  Result := ProxedMember.GetController;
end;

procedure TBoldMember_Proxy.SetBoldPersistenceState(Value: TBoldValuePersistenceState);
begin
  ProxedMember.SetBoldPersistenceState(Value);
end;

{ TBoldAttribute_Proxy }

function TBoldAttribute_Proxy.GetProxedAttribute: TBoldAttribute;
begin
  result := TBoldAttribute(ProxedMember);
end;

procedure TBoldAttribute_Proxy.AssignContentValue(const Source: IBoldValue);
begin
  ProxedAttribute.AssignContentValue(Source);
end;

procedure TBoldAttribute_Proxy.SetContentToNull;
begin
  ProxedAttribute.SetContentToNull;
end;

procedure TBoldAttribute_Proxy.SetStringRepresentation(Representation: integer;
  const NewValue: String);
begin
  ProxedAttribute.StringRepresentation[Representation] := NewValue;
end;

function TBoldAttribute_Proxy.GetContentAsString: String;
begin
  Result := GetStringRepresentation(brDefault);
end;

function TBoldAttribute_Proxy.GetContentIsNull;
begin
  Result := ProxedAttribute.ContentIsNull;
end;

function TBoldAttribute_Proxy.GetStringRepresentation(
  representation: integer): String;
begin
  result := ProxedAttribute.StringRepresentation[Representation];
end;

{ TBoldSystem_Proxy }

procedure TBoldSystem_Proxy.AllObjectIds(resultList: TBoldObjectIdList;
  OnlyLoaded: Boolean);
var
  Traverser: TBoldLocatorListTraverser;
begin
  Traverser := ProxedSystem.Locators.CreateTraverser;
  while Traverser.MoveNext do
  begin
    if not OnlyLoaded or assigned(Traverser.Locator.BoldObject) then
      ResultList.Add(Traverser.Locator.BoldObjectID);
  end;
  Traverser.Free;
end;

procedure TBoldSystem_Proxy.ApplytranslationList(
  IdTranslationList: TBoldIdTranslationList);
var
  I: Integer;
  Locator: TBoldObjectLocator;
  Id: TBoldObjectId;
begin
  if IdTranslationList.Count = 0 then
    exit;
  for I := 0 to IdTranslationList.Count - 1 do
  begin
    Id := IdTranslationList.OldIDs[I];
    if Assigned(Id) then
    begin
      Locator := ProxedSystem.Locators.LocatorByID[Id];
      if not Assigned(Locator) then
        raise EBoldInternal.CreateFmt(sLocatorNotFound, [ClassName, 'ApplytranslationList', Id.AsString]);
      Id := IdTranslationList.NewIDs[I];
      if Assigned(Id) then
        ProxedSystem.Locators.UpdateId(Locator, Id)
    end
    else
    begin
      ProxedSystem.EnsuredLocatorByID[IdTranslationList.NewIDs[I]];
    end;
  end;
  ProxedSystem.UndoHandler.ApplytranslationList(IdTranslationList);
  ProxedSystem.fOldValueHandler.OldValues.ApplytranslationList(IdTranslationList);
end;

procedure TBoldSystem_Proxy.ApplyValueSpace(const ValueSpace: IBoldValueSpace;
  IgnorePersistenceState: Boolean);

procedure ApplyObjectContents(BoldObject: TBoldObject; const ObjectContents: IBoldObjectContents);
  var
    i: Integer;
    aValue: IBoldValue;
    bDirtyOrNewOrDeleted: Boolean;
    aMember: TBoldMember;
  begin
    bDirtyOrNewOrDeleted := BoldObject.BoldDirty or BoldObject.BoldObjectIsNew or BoldObject.BoldObjectIsDeleted;
    if bDirtyOrNewOrDeleted then begin
    BoldObject.SetBoldExistenceState(ObjectContents.BoldExistenceState);
    BoldObject.SetBoldPersistenceState(ObjectContents.BoldPersistenceState);
    end;
    for i := BoldObject.BoldMemberCount - 1 downto 0 do
    begin
      aValue := ObjectContents.valueByIndex[i];
      if Assigned(aValue) then begin
        aMember := BoldObject.BoldMembers[i];
        if bDirtyOrNewOrDeleted or aMember.BoldDirty or (aMember.OldValue <> nil) then begin
          aMember.AsIBoldValue[bdepContents].AssignContent(aValue);
          aMember.SendEvent(beValueInvalid);
        end;
      end;
    end;
  end;

var
  i, j: Integer;
  anIdList: TBoldObjectIdList;
  ObjectId: TBoldObjectId;
  BoldObject: TBoldObject;
begin
  anIdList := TBoldObjectIdList.Create;
  try
    ValueSpace.AllObjectIds(anIdList, true);
    for i := 0 to anIdList.Count - 1 do
    begin
      ObjectId := anIdList[i];
      BoldObject := ProxedSystem.Locators.ObjectByID[ObjectId];
      if not assigned(BoldObject) then
        BoldObject := ProxedSystem.CreateExistingObjectByID(ObjectId);
      ApplyObjectContents(BoldObject, ValueSpace.ObjectContentsByObjectId[ObjectId]);
    end;

    // Send the beValueInvalid event to all members that become invalid. Make sure you do this after the
    // first loop is finished so that all members that may be needed have the correct value
    if not IgnorePersistenceState then begin
      for i := 0 to anIdList.Count - 1 do begin
        ObjectId := anIdList[i];
        BoldObject := ProxedSystem.Locators.ObjectByID[ObjectId];
        if BoldObject.BoldObjectExists then begin
          for j := 0 to BoldObject.BoldMemberCount - 1 do begin
            if Assigned(ValueSpace.ObjectContentsByObjectId[ObjectId].valueByIndex[j]) and
               (BoldObject.BoldMembers[j].BoldPersistenceState = bvpsInvalid) then
            begin
              BoldObject.BoldMembers[j].SendEvent(beValueInvalid);
            end;
          end;
        end;
      end;
    end;
  finally
    anIdList.Free;
  end;
end;

constructor TBoldSystem_Proxy.Create(ProxedSystem: TBoldSystem;
  Mode: TBoldDomainElementProxyMode);
begin
  inherited Create(Mode);
  fProxedSystem := ProxedSystem;
end;

procedure TBoldSystem_Proxy.EnsureObjectContents(ObjectId: TBoldObjectId);
begin
  if not assigned(ProxedSystem.Locators.ObjectByID[ObjectId]) then
    ProxedSystem.CreateExistingObjectByID(ObjectID);
end;

procedure TBoldSystem_Proxy.EnsureObjectId(ObjectId: TBoldObjectId);
var
  ObjectLocator: TBoldObjectLocator;
begin
  ObjectLocator := ProxedSystem.GetEnsuredLocatorById(ObjectId);
  if not ObjectLocator.BoldObjectID.TopSortedIndexExact and
    ObjectId.TopSortedIndexExact then
    ProxedSystem.Locators.UpdateID(ObjectLocator, ObjectId);
end;

procedure TBoldSystem_Proxy.ExactifyIDs(
  TranslationList: TBoldIdTranslationList);
var
  i: Integer;
  ObjectLocator: TBoldObjectLocator;
begin
  for i := 0 to TranslationList.Count - 1 do
  begin
    ObjectLocator := ProxedSystem.Locators.LocatorByID[TranslationList.OldIds[i]];
    if Assigned(ObjectLocator) then
      ProxedSystem.Locators.UpdateId(ObjectLocator, TranslationList.NewIds[i]);
  end;
end;

function TBoldSystem_Proxy.GetEnsuredObjectContentsByObjectId(
  ObjectId: TBoldObjectId): IBoldObjectContents;
begin
  result := GetObjectContentsByObjectId(ObjectId);
  if not assigned(result) then
    result := ProxedSystem.CreateExistingObjectByID(ObjectID).AsIBoldObjectContents[Mode];
end;

function TBoldSystem_Proxy.GetEnsuredObjectContentsByObjectIdAndCheckIfCreated(
  ObjectId: TBoldObjectId;
  out aBoldObjectContents: IBoldObjectContents): boolean;
begin
  aBoldObjectContents := GetObjectContentsByObjectId(ObjectID);
  result := not Assigned(aBoldObjectContents);
  if result then
    aBoldObjectContents := ProxedSystem.CreateExistingObjectByID(ObjectID).AsIBoldObjectContents[Mode];
end;

function TBoldSystem_Proxy.GetHasContentsForId(ObjectId: TBoldObjectId): boolean;
begin
  result := Assigned(ProxedSystem.Locators.ObjectByID[ObjectId])
end;

function TBoldSystem_Proxy.GetObjectContentsByObjectId(
  ObjectId: TBoldObjectId): IBoldObjectContents;
var
  Locator: TBoldObjectLocator;
begin
  Locator := ProxedSystem.Locators.LocatorByID[Objectid];
  if assigned(Locator) and assigned(locator.boldobject) then
    result := Locator.BoldObject.AsIBoldObjectContents[Mode]
  else
    result := nil;
end;

function TBoldSystem_Proxy.IdCount: integer;
begin
  result := ProxedSystem.Locators.Count;
end;

function TBoldSystem_Proxy.IsEmpty: boolean;
var
  Traverser: TBoldLocatorListTraverser;
begin
  result := true;
  Traverser := ProxedSystem.Locators.CreateTraverser;
  while Traverser.MoveNext do
  begin
    if assigned(Traverser.Locator.BoldObject) then
    begin
      result := false;
      break
    end;
  end;
  Traverser.Free;
end;

{ TBoldObject_Proxy }

constructor TBoldObject_Proxy.Create(ProxedObject: TBoldObject;
  Mode: TBoldDomainElementProxyMode);
begin
  inherited Create(Mode);
  fProxedObject := ProxedObject;
end;

procedure TBoldObject_Proxy.EnsureMember(MemberId: TBoldMemberId;
  const ContentName: string);
begin
  EnsureMemberAndGetValueByIndex(MemberId.MemberIndex, ContentName);
end;

function TBoldObject_Proxy.EnsureMemberAndGetValueByIndex(MemberIndex: Integer;
  const ContentName: string): IBoldValue;
var
  Member: TBoldMember;
begin
  Member := ProxedObject.BoldMembers[MemberIndex];
  if not SameText(Member.GetStreamName, ContentName) then
    raise EBold.CreateFmt(sUnexpectedStreamType,
      [classname, Member.DisplayName, ContentName, Member.GetStreamName]);
  Result := GetValueByIndex(MemberIndex);
end;

function TBoldObject_Proxy.GetBoldExistenceState: TBoldExistenceState;
begin
   Result := ProxedObject.BoldExistenceState
end;

function TBoldObject_Proxy.GetBoldMemberCount: Integer;
begin
  Result := ProxedObject.BoldmemberCount;
end;

function TBoldObject_Proxy.GetBoldPersistenceState: TBoldValuePersistenceState;
begin
  Result := ProxedObject.BoldPersistenceState;
end;

function TBoldObject_Proxy.GetGlobalId: string;
begin
  Result := ProxedObject.GetGlobalId;
end;

function TBoldObject_Proxy.GetIsModified: Boolean;
begin
  Result := ProxedObject.GetIsModified;
end;

function TBoldObject_Proxy.GetIsReadOnly: Boolean;
begin
  Result := ProxedObject.IsReadOnly;
end;

function TBoldObject_Proxy.GetObjectId: TBoldObjectId;
begin
  result := ProxedObject.BoldObjectLocator.BoldObjectId;
end;

procedure TBoldSystemLocatorList.UpdateID(Locator: TBoldObjectLocator; NewObjectID: TBoldObjectId; AllowInternal: Boolean);
begin
  if (NewObjectID.IsStorable or AllowInternal) or NewObjectID.NonExisting then
  begin
    if assigned(Locator.BoldObject) then
      Locator.BoldObject.SendEvent(bePreUpdateId);
    RemoveFromAllIndexes(Locator);
    Locator.fBoldObjectID.Free;
    Locator.fBoldObjectID := NewObjectid.Clone;
    AddToAllIndexes(Locator);
    if assigned(Locator.BoldObject) then
      Locator.BoldObject.SendEvent(bePostUpdateId);
  end
  else
    raise EBoldInternal.CreateFmt('%s.UpdateId: Tried to update ID to an internal ID', [ClassName]);
end;

function TBoldObject_Proxy.GetTimeStamp: TBoldTimeStampType;
begin
  Result := ProxedObject.GetTimeStamp;
end;

function TBoldObject_Proxy.GetValueByIndex(I: Integer): IBoldValue;
var
  Member: TBoldMember;

  function IncludeInPmOut: boolean;
  begin
    Result := (not Member.BoldPersistenceStateIsInvalid) and
              (not Member.Derived) and
              ((Member.BoldMemberRTInfo is TBoldAttributeRTInfo) or
              (TBoldRoleRTInfo(Member.BoldMemberRTInfo).RoleType in [rtRole, rtInnerLinkRole]));
  end;
begin
  Member := ProxedObject.GetBoldMemberIfAssigned(i);
  if not ASsigned(Member) and ProxedObject.BoldObjectIsNew then
     Member := ProxedObject.BoldMembers[i];

  if Assigned(Member) and ((Mode <> bdepPmOut) or IncludeInPmOut) then
    Result := Member.AsIBoldValue[Mode]
  else
    Result := nil;
end;

function TBoldObject_Proxy.GetValueByMemberId(MemberId: TBoldMemberID): IBoldValue;
begin
   result := GetValueByIndex(MemberID.MemberIndex);
end;

procedure TBoldObject_Proxy.SetBoldExistenceState(Value: TBoldExistenceState);
begin
  ProxedObject.SetBoldExistenceState(Value);
end;

procedure TBoldObject_Proxy.SetBoldPersistenceState(Value: TBoldValuePersistenceState);
begin
  ProxedObject.SetBoldPersistenceState(Value);
end;

procedure TBoldObject_Proxy.SetGlobalId(const NewValue: string);
begin
   ProxedObject.SetGlobalId(NewValue);
end;

procedure TBoldObject_Proxy.SetIsReadOnly(NewValue: Boolean);
begin
  ProxedObject.SetIsReadOnly(NewValue);
end;

procedure TBoldObject_Proxy.SetTimeStamp(NewValue: TBoldTimeStampType);
begin
  ProxedObject.SetTimeStamp(NewValue);
end;

{ EBoldOperationFailedForObjectList }

constructor EBoldOperationFailedForObjectList.Create(const msg: string;
  args: array of const; IdList: TBoldObjectIdList; System: TBoldSystem);
var
  RootClassTypeInfo: TBoldClassTypeInfo;
begin
  inherited createFmt(msg, args);
  RootClassTypeInfo := System.BoldSystemTypeInfo.TopSortedClasses[0];
  fObjectList := TBoldMemberFactory.CreateMemberFromBoldType(RootClassTypeInfo.ListTypeInfo) as TBoldObjectList;
  fObjectLIst.FillFromIDList(IdList, System);
end;

destructor EBoldOperationFailedForObjectList.Destroy;
begin
  FreeAndNil(FObjectList);
  inherited;
end;

{ TBoldLocatorListTraverser }

function TBoldLocatorListTraverser.GetLocator: TBoldObjectLocator;
begin
  result := TBoldObjectLocator(item);
  Assert(result is TBoldObjectLocator);
end;

function TBoldSystemLocatorList.CreateTraverser: TBoldLocatorListTraverser;
begin
  result := inherited CreateTraverser as TBoldLocatorListTraverser;
end;

function TBoldSystemLocatorList.TraverserClass: TBoldIndexableListTraverserClass;
begin
  result := TBoldLocatorListTraverser;
end;

{ TBoldMemberDeriver }

constructor TBoldMemberDeriver.Create(Member: TBoldMember);
begin
  inherited Create;
  fDerivedMember := Member;
end;

destructor TBoldMemberDeriver.Destroy;
begin
  inherited;
end;

procedure TBoldMemberDeriver.DoDeriveAndSubscribe(subscribe: Boolean);
begin
  if Subscribe then
    DerivedMember.DeriveMember(Self)
  else
    DerivedMember.DeriveMember(nil);
end;

procedure TBoldMemberDeriver.DoNotifyOutOfDate;
begin
  DerivedMember._NotifyOutOfDate;
end;

function TBoldMemberDeriver.GetDerivedObject: TObject;
begin
  Result := fDerivedMember;
end;

function TBoldMemberDeriver.GetInternalDeriverState: TBoldDeriverState;
begin
  result := DerivedMember.GetDeriverState;
end;

procedure TBoldMemberDeriver.SetInternalDeriverState(const Value: TBoldDeriverState);
begin
  DerivedMember.DeriverState := Value;
end;

{ TBoldAbstractOptimisticLockHandler }

function TBoldAbstractOptimisticLockHandler.GetOldValues: IBoldValueSpace;
begin
  result := System.fOldValueHandler.Oldvalues;
end;

{ TBoldSystemExtension }

constructor TBoldSystemExtension.Create(System: TBoldSystem);
begin
  fSystem := System;
end;

function TBoldObjectLocator.GetClassTypeInfo: TBoldClassTypeInfo;
begin
  if Assigned(BoldObjectId) then
    result := BoldSystem.BoldSystemTypeInfo.TopSortedClasses[BoldObjectId.TopSortedIndex]
  else
    result := nil;
end;

function TBoldObjectLocator.GetEmbeddedSingleLinks(EmbeddedIndex: integer): TBoldObjectLocator;
begin
  if embeddedindex < Length(fEmbeddedSingleLinks) then
    Result := fEmbeddedSingleLinks[EmbeddedIndex]
  else
    Result := nil;
end;

procedure TBoldObjectLocator.SetEmbeddedSingleLinks(EmbeddedIndex: integer; const Value: TBoldObjectLocator);
begin
  if embeddedindex >= Length(fEmbeddedSingleLinks) then
    SetLength(fEmbeddedSingleLinks, EmbeddedIndex+1);
  fEmbeddedSingleLinks[EmbeddedIndex] := Value;
  if Value = nil then
    TryShrinkEmbeddedLinks;
end;

procedure TBoldObjectLocator.TryShrinkEmbeddedLinks;
var
  NewLength: integer;
  OldLength: integer;
begin
  OldLength := Length(fEmbeddedSingleLinks);
  NewLength := OldLength;
  while (NewLength > 0) and (not Assigned(fEmbeddedSingleLinks[NewLength-1])) do
    Dec(NewLength);
  if (NewLength = 0) or (NewLength < (OldLength - 3)) then
    SetLength(fEmbeddedSingleLinks, NewLength);
end;

procedure TBoldObjectLocator.EmbeddedSingleLinksFromObject;
var
  i: integer;
  ObjectReferece: TBoldObjectReference;
  RoleRtInfo: TBoldRoleRTInfo;
begin
  for i := 0 to BoldObject.BoldClassTypeInfo.AllRoles.Count - 1 do
  begin
    RoleRtInfo := TBoldRoleRTInfo(BoldObject.BoldClassTypeInfo.AllRoles[i]);
    if (RoleRtInfo.EmbeddedLinkIndex <> -1) and BoldObject.BoldMemberAssigned[RoleRtInfo.index] then
    begin
      Assert(BoldObject.BoldMembers[RoleRtInfo.Index] is TBoldObjectReference);
      ObjectReferece := TBoldObjectReference(BoldObject.BoldMembers[RoleRtInfo.index]);
      if (ObjectReferece.BoldPersistenceState = bvpsCurrent) and
        (RoleRtInfo.indexOfOtherEnd <> -1) and
        (ObjectReferece.Locator <> nil) and
        (ObjectReferece.Locator.BoldObject <> nil) and
        ObjectReferece.Locator.BoldObject.BoldMemberAssigned[RoleRtInfo.indexOfOtherEnd] then
        EmbeddedSingleLinks[RoleRtInfo.EmbeddedLinkIndex] := ObjectReferece.Locator;
    end;
  end;
end;

procedure TBoldObjectLocator.EmbeddedSingleLinksToObject;
var
  i: integer;
  ObjRef: TBoldObjectReference;
  RoleRtInfo: TBoldRoleRTInfo;
begin
  if Length(fEmbeddedSingleLinks) > 0 then
  begin
    for i := 0 to BoldObject.BoldClassTypeInfo.AllRoles.Count - 1 do
    begin
      RoleRtInfo := BoldObject.BoldClassTypeInfo.AllRoles[i];
      if (RoleRtInfo.EmbeddedLinkIndex <> -1) and (EmbeddedSingleLinks[RoleRtInfo.EmbeddedLinkIndex] <> nil) then
      begin
        Assert(BoldObject.BoldMembers[RoleRtInfo.index] is TBoldObjectReference);
        ObjRef := TBoldObjectReference(BoldObject.BoldMembers[RoleRtInfo.Index]);
        (ObjRef.AsIBoldValue[bdepInternalInitialize] as IBoldObjectIdRef).SetFromId(EmbeddedSingleLinks[RoleRtInfo.EmbeddedLinkIndex].BoldObjectID, false);
        assert((ObjRef.AsIBoldValue[bdepContents] as IBoldObjectIdRef).BoldPersistenceState = bvpsInvalid);
        ObjRef.HasOldValues := True;
      end;
    end;
    SetLength(fEmbeddedSingleLinks, 0);
  end;
end;

procedure TBoldObjectLocator.FreeEmbeddedSingleLinksOfOtherEnd;
var
  i,m: integer;
  SingleLink: TBoldObjectReference;
  MultiLink: TBoldObjectList;
  RoleInfo: TBoldRoleRTInfo;
begin
  for i := 0 to BoldObject.BoldClassTypeInfo.AllRoles.Count - 1 do
  begin
    RoleInfo := BoldObject.BoldClassTypeInfo.AllRoles[i];
    if (RoleInfo.RoleType = rtLinkRole) and
      (RoleInfo.IndexOfMainRole=RoleInfo.index) and
      BoldObject.BoldMemberAssigned[RoleInfo.Index] and
      (BoldObject.BoldMembers[RoleInfo.Index].BoldPersistenceState = bvpsCurrent) then
    begin
      if BoldObject.BoldMembers[RoleInfo.Index] is TBoldObjectReference then
      begin
        SingleLink := TBoldObjectReference(BoldObject.BoldMembers[RoleInfo.Index]);
        if Assigned(SingleLink.Locator) and (Length(SingleLink.Locator.fEmbeddedSingleLinks) > 0) then
          SingleLink.Locator.EmbeddedSingleLinks[RoleInfo.RoleRTInfoOfOtherEnd.EmbeddedLinkIndex] := nil;
      end
      else
      begin
        multiLink := BoldObject.BoldMembers[RoleInfo.Index] as TBoldObjectList;
        for m := 0 to Multilink.Count - 1 do
          if Length(Multilink.Locators[m].fEmbeddedSingleLinks) > 0 then
            Multilink.Locators[m].EmbeddedSingleLinks[RoleInfo.RoleRTInfoOfOtherEnd.EmbeddedLinkIndex] := nil;
      end;
    end;
  end;
end;

{ TBoldAbstractUndoHandler }

procedure TBoldAbstractUndoHandler.DeleteObject(BoldObject: TBoldObject);
begin
  if BoldObject.BoldPersistenceState <> bvpsModified then
    System.DestroyObject(BoldObject);
  System.AddToTransaction(BoldObject);
end;

class function TBoldAbstractUndoHandler.GetControllerForMember(Member: TBoldMember): TBoldAbstractController;
begin
  Result := Member.GetController;
end;

{ TBoldObjectReference_Proxy }

procedure TBoldObjectReference_Proxy.AssignContentValue(const Source: IBoldValue);
begin
  Assert(ProxedController is TBoldObjectReferenceController);
  if Mode = bdepContents then
    TBoldObjectReferenceController(ProxedController).AssignContentValue(Source)
  else
    UnsupportedMode(Mode, 'AssignContentValue');
end;

function TBoldLocatorIdHashIndex.Hash(const Key): Cardinal;
begin
  if TObject(Key) = nil then
    raise EBold.CreateFmt('%s.Hash: Nil is not a valid key value.', [ClassName]);
  Result := TBoldObjectId(Key).Hash;
end;

{ TBoldAbstractSystemPersistenceHandler }

procedure TBoldAbstractSystemPersistenceHandler.EndFetchForAll(
  ObjectList: TBoldObjectList; MemberIdList: TBoldMemberIdList);
var
  i: Integer;
  anObject: TBoldObject;
begin
  for i := 0 to ObjectList.Count - 1 do
  begin
    anObject := ObjectList.Locators[i].BoldObject;
    if assigned(anObject) then
      anObject.EndFetchMembers(MemberIdList);
  end;
end;

procedure TBoldAbstractSystemPersistenceHandler.EndUpdateForAll(
  ObjectList: TBoldObjectList;
  Translationlist: TBoldIdTranslationlist);
var
  i: Integer;
  anObject: TBoldObject;
begin
  for i := 0 to ObjectList.Count - 1 do
  begin
    anObject := ObjectList[i];
    if assigned(anObject) then
    begin
      if anObject.GetIsModified or System.BoldSystemTypeInfo.UpdateWholeObjects then
        anObject.EndUpdate(translationList);
      anObject.EndUpdateMembers(TranslationList);
      anObject.CompleteUpdate;
      if anObject.BoldObjectIsDeleted then
        system.DestroyObject(anObject)
    end;
  end;
end;

function TBoldAbstractSystemPersistenceHandler.StartUpdateForAll(ObjectList: TBoldObjectList): Boolean;
{$IFNDEF NoMayUpdate}
  function MayUpdateForAll: Boolean;

    function ObjectProhibitsUpdate(anObject: TBoldObject): Boolean;
    begin
      result := ((anObject.BoldDirty or System.BoldSystemTypeInfo.UpdateWholeObjects)
                 and not anObject.CanUpdate)
                or not anObject.MayUpdateMembers;
    end;

  var
    i: Integer;
  begin
    for i := 0 to ObjectList.Count - 1 do
      if ObjectProhibitsUpdate(ObjectList[i]) then
      begin
        result := False;
        exit;
      end;
    result := True;
  end;

  Procedure DoStartUpdateForAll;

    procedure IfNecessaryDoStartUpdateObject(anObject: TBoldObject);
    begin
      if anObject.BoldDirty or System.BoldSystemTypeInfo.UpdateWholeObjects then
        anObject.DoStartUpdate;
    end;

  var
    i: Integer;
    anObject: TBoldObject;
  begin
    for i := 0 to ObjectList.Count - 1 do
    begin
      anObject := ObjectList[i];
      if assigned(anObject) then
        IfNecessaryDoStartUpdateObject(anObject);
    end;
  end;
{$ENDIF}
begin
{$IFNDEF NoMayUpdate}
  result := MayUpdateForAll;
  if result then
    DoStartUpdateForAll;
{$ELSE}
  result := true;
{$ENDIF}
end;

procedure TBoldMemberDeriver.DoReverseDerive;
begin
  SetDeriverState(bdsReverseDeriving);
  if CanReversederive then
    DerivedMember.ReverseDeriveMember;
  case deriverstate of
    bdsReverseDeriving: SetDeriverState(bdsOutOfDate);
    bdsReverseDerivingSubscriptionOutOfDate: SetDeriverState(bdsSubscriptionOutOfDate);
  end;
end;

function TBoldMemberDeriver.GetCanReverseDerive: Boolean;
begin
  Result := fDerivedMember.GetBoldMemberRTInfo.IsReverseDerived;
end;

{ TBoldTransactionHandler }

procedure TBoldTransactionHandler.BeforeDestruction;
begin
  if CanCommitOrRollback then
    Rollback;
  inherited;
end;

constructor TBoldTransactionHandler.Create(ABoldSystem: TBoldSystem);
begin
  fBoldSystem := ABoldSystem;
  fSubscriber := TBoldExtendedPassthroughSubscriber.CreateWithExtendedReceive(Receive);
  fBoldSystem.AddSmallSubscription(fSubscriber, [beValueChanged, beDestroying]);
  fBoldSystem.StartTransaction();
  fTransactionNesting := fBoldSystem.fTransactionNesting;
end;

destructor TBoldTransactionHandler.Destroy;
begin
  FreeAndNil(fSubscriber);
  inherited;
end;

function TBoldTransactionHandler.GetValueSpace: IBoldValueSpace;
begin
  result := fBoldSystem.fRollbackArea;
end;

procedure TBoldTransactionHandler.Receive(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent;
  const Args: array of const);
begin

end;

function TBoldTransactionHandler.CanCommitOrRollback: boolean;
begin
  result := (fTransactionNesting = fBoldSystem.fTransactionNesting);
end;

procedure TBoldTransactionHandler.Commit;
begin
  if not CanCommitOrRollback then
    raise EBold.CreateFmt(sUnmatchedCommit, [classname]);
  fBoldSystem.CommitTransaction();
end;

procedure TBoldTransactionHandler.Rollback;
begin
  if not CanCommitOrRollback then
    raise EBold.CreateFmt(sUnmatchedRollback, [classname]);
  fBoldSystem.RollbackTransaction();
end;

procedure InitDebugMethods;
begin
  exit;
  TBoldSystemLocatorList.Create.LocatorByIDString[''];
  TBoldSystemLocatorList.Create.ObjectByIDString[''];
  TopSortedIndex2ClassName(0);
end;

initialization
  BoldFreeStandingObjectContentsClass := TBoldSystemFreeStandingObjectContents;
  InitDebugMethods;

finalization
  FreeAndNil(_BoldSystemList);
  FreeAndNil(_BoldSystemInternalLog);

end.
