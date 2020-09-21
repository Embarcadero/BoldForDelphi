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
  TBoldAbstractTransActionHandler = class;
  TBoldAbstractSystemPersistenceHandler = class;
  TBoldAbstractRegionFactory = class;

  // Controllers
  TBoldAbstractController = class;
  TBoldListController = class;
  TBoldAbstractObjectListController = class;
  TBoldAbstractObjectReferenceController = class;
  TBoldObjectReferenceController = class; { TODO : Move out }

  TBoldMemberFactory = class;

  // Proxies
  TBoldMember_Proxy = class;
  TBoldAttribute_Proxy = class;

  // Meta classes
  TBoldObjectClass = class of TBoldObject;
  TBoldAttributeClass = class of TBoldAttribute;
  TBoldMemberClass = class of TBoldMember;
  TBoldListClass = class of TBoldList;
  TBoldObjectListClass = class of TBoldObjectList;

  TBoldMember_ProxyClass = class of TBoldMember_Proxy;

  { exceptions }
  EBoldOperationFailedForObjectList = class;
  EBoldFailure = class;
  EBoldAccessNullValue = class(EBold);

  { other types }
  TBoldSystemTransactionMode = (stmUnsafe, stmNormal);
  TBoldListDupMode = (bldmMerge, bldmAllow, bldmError);
  TBoldTransactionState = (btsNone, btsInTransaction, btsRollingBack);

  TBoldPrepareListOperation = procedure(List: TBoldList) of object;
  TBoldElementCompare = function (Item1, Item2: TBoldElement): Integer of object;
  TBoldOptimisticLockingFailedEvent = procedure(UpdateList, FailureList: TBoldObjectList; const FailureReason: String) of object;
  TBoldCreateApproximateObjectError = procedure(Obj: TBoldObject) of object;

  { TBoldSystemExtension }
  TBoldSystemExtension = class(TBoldNonRefcountedObject)
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
  public
    procedure HandleMember(ObjectContents: IBoldObjectContents; MemberIndex: integer; MemberValue: IBoldValue); virtual; abstract;
    procedure HandleObject(Obj: IBoldObjectContents; RegardAsExisting: Boolean); virtual; abstract;
    procedure PrepareUpdate(const ObjectList: TBoldObjectList); virtual; abstract;
    procedure ApplytranslationList(IdTranslationList: TBoldIdTranslationList); virtual; abstract;
  end;

  { TBoldAbstractOldValueHandler }
  TBoldAbstractOldValueHandler = class(TBoldSystemExtension)
  protected
    function GetOldValues: IBoldValueSpace; virtual; abstract;
    function GetIsEmpty: Boolean; virtual; abstract;
    procedure PurgeEqualValues; virtual; abstract;
    class function NewValueInValueSpace(BoldMember: TBoldMember; ValueSpace: IBoldValueSpace): IBoldValue;
    class procedure CopyMemberToValueSpace(BoldMember: TBoldMember; ValueSpace: IBoldValueSpace);
    class procedure CopyObjectToValueSpace(BoldObject: TBoldObject; ValueSpace: IBoldValueSpace);
  public
    procedure MemberValuePreChange(BoldMember: TBoldMember); virtual; abstract;
    procedure MemberPersistenceStatePreChange(BoldMember: TBoldMember; NewState: TBoldValuePersistenceState); virtual; abstract;
    procedure ObjectExistenceChange(BoldObject: TBoldObject); virtual; abstract;
    procedure ObjectExistencePersistenceStateChange(BoldObject: TBoldObject; NewState: TBoldValuePersistenceState); virtual; abstract;
    property OldValues: IBoldValueSpace read GetOldValues;
    property IsEmpty: Boolean read GetIsEmpty;
  end;

  { TBoldAbstractTransActionHandler }
  TBoldAbstractTransActionHandler = class(TBoldAbstractOldValueHandler)
  protected
    function GetTransactionMode: TBoldSystemTransactionMode;  virtual; abstract;
    function GetTransactionState: TBoldTransactionState; virtual; abstract;
  public
    procedure StartTransaction(MinimalMode: TBoldSystemTransactionMode = stmNormal); virtual; abstract;
    procedure CommitTransaction(MinimalMode: TBoldSystemTransactionMode = stmNormal); virtual; abstract;
    procedure RollbackTransaction(MinimalMode: TBoldSystemTransactionMode = stmNormal); virtual; abstract;
    property TransactionMode: TBoldSystemTransactionMode read GetTransactionMode;
    property TransactionState: TBoldTransactionState read GetTransactionState;
  end;

  { TBoldAbstractSystemPersistenceHandler }
  TBoldAbstractSystemPersistenceHandler = class(TBoldSystemExtension)
  private
    fOnPreUpdate: TNotifyEvent;
  protected
    function GetTimeStampOfLatestUpdate: TBoldTimeStampType; virtual; abstract;
  public
    function EnsureEnclosure(ObjectList: TBoldObjectList; ValidateOnly: Boolean): Boolean; virtual; abstract;
    procedure FetchLinksWithObjects(ObjectList: TBoldObjectList; const LinkName: string; FetchedObjects: TBoldObjectList); virtual; abstract;
    procedure FetchObjectById(BoldObjectId: TBoldObjectId); virtual; abstract;
    procedure FetchMember(Member: TBoldMember); virtual; abstract;
    procedure FetchList(FetchList: TBoldObjectList); virtual; abstract;
    procedure FetchClass(ClassList: TBoldObjectList; Time: TBoldTimestampType); virtual; abstract;
    procedure GetAllWithCondition(aList: TBoldObjectList; Condition: TBoldCondition); virtual; abstract;
    procedure GetAllInClassWithSQL(aList: TBoldObjectList; AClass: TBoldObjectClass; WhereClause, OrderByClause: String; Params: TParams; JoinInheritedTables: Boolean; MaxAnswers: integer; Offset: integer);virtual; abstract;
    procedure UpdateDatabaseWithList(ObjectList: TBoldObjectList);  virtual; abstract;
    function GetTimeForTimestamp(Timestamp: TBoldTimestampType): TDateTime; virtual; abstract;
    function GetTimestampForTime(ClockTime: TDateTime): TBoldTimestampType; virtual; abstract;
    property OnPreUpdate: TNotifyEvent read fOnPreUpdate write fOnPreUpdate;
    property TimeStampOfLatestUpdate: TBoldTimeStampType read GetTimeStampOfLatestUpdate;
    procedure EndFetchForAll(ObjectList: TBoldObjectList; MemberIdList: TBoldMemberIdList);
    procedure EndUpdateForAll(ObjectList: TBoldObjectList; Translationlist: TBoldIdTranslationlist);
    function StartUpdateForAll(ObjectList: TBoldObjectList): Boolean;
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

  {---TBoldSystem---}
  TBoldSystem = class(TBoldDomainElement)
  private
    fTransactionMode: TBoldSystemTransactionMode;
    fBoldSystemTypeInfo: TBoldSystemTypeInfo;
    fClasses: TBoldMemberList; {of TBoldClass}
    fDelayedDestructionCount: integer;
    fDelayedDestructionList: TList;
    fDirtyObjects: TList; {of TBoldObject}
    fDirtyObjectsInvalid: Boolean;
    fEvaluator: TBoldEvaluator;
    fLocators: TBoldSystemLocatorList;
    FNewDirtyList: TBoldObjectList;
    fNewModifiedList: TBoldObjectList;
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
    procedure SetIsdefault(Value: boolean);
    function GetIsDefault: boolean;
    function RollBackAreaAssigned: boolean;
    procedure CopyMemberToRollBackBuffer(BoldMember: TBoldMember);
    procedure CopyObjectToRollBackBuffer(BoldObject: TBoldObject);
    procedure AddToTransaction(DomainElement: TBoldDomainElement);
    function CanCommit: Boolean;
    procedure DestroyObject(BoldObject: TBoldObject);
    procedure DirtyAsObjectList(ObjectList: TBoldObjectList);
    function FindClassByExpressionName(const ExpressionName: string): TBoldObjectList;
    function GetClassByExpressionName(const ExpressionName: string): TBoldObjectList;
    function GetClassByIndex(index: Integer): TBoldObjectList;
    function GetDirtyObjects: TList; {of TBoldObject}
    function GetEnsuredLocatorByID(ObjectID: TBoldObjectId): TBoldObjectLocator;
    function GetTimeForTimestamp(Timestamp: TBoldTimestampType): TDateTime;
    function GetTimestampForTime(ClockTime: TDateTime): TBoldTimestampType;
    procedure MarkObjectClean(BoldObject: TBoldObject); {called by TBoldObject}
    procedure MarkObjectDirty(BoldObject: TBoldObject); {called by TBoldObject}
    procedure MarkObjectPossiblyCleaner(BoldObject: TBoldObject); {called by TBoldObject}
    function GetAsIBoldvalueSpace(Mode: TBoldDomainElementProxyMode): IBoldvalueSpace;
    procedure SetTransactionMode(const Value: TBoldSystemTransactionMode);
    procedure SetPessimisticLockHandler(const Value: TBoldAbstractPessimisticLockHandler);
    function GetOnPreUpdate: TNotifyEvent;
    procedure SetOnPreUpdate(const Value: TNotifyEvent);
    function GetTimeStampOfLatestUpdate: TBoldTimeStampType;
    function GetUndoHandler: TBoldAbstractUndoHandler;
    function GetUndoHandlerInterface: IBoldUndoHandler;
    function CanCreateObject(ClassTypeInfo: TBoldClassTypeInfo): boolean;
    function CanDeleteObject(anObject: TBoldObject): boolean;
    procedure DiscardPersistent;
    procedure DiscardTransient;
  protected
    function GetBoldDirty: Boolean; override;
    function GetBoldType: TBoldElementTypeInfo; override;
    function GetDisplayName: String; override;
    function GetEvaluator: TBoldEvaluator; override;
    function GetStringRepresentation(Representation: TBoldRepresentation): string; override;
    procedure ReceiveEventFromOwned(originator: TObject; originalEvent: TBoldEvent); override;
    property SystemPersistenceHandler: TBoldAbstractSystemPersistenceHandler read fSystemPersistenceHandler;
   public
    constructor Create(AOwningElement: TBoldDomainElement); override;
    constructor CreateWithTypeInfo(AOwningElement: TBoldDomainElement; SystemTypeInfo: TBoldSystemTypeInfo; PersistenceController: TBoldPersistenceController; RegionFactory: TBoldAbstractRegionFactory = nil);
    destructor Destroy; override;
    function AssertLinkIntegrity: Boolean;
    class function DefaultSystem: TBoldSystem;
    procedure MakeDefault;
    procedure EnsureCanDestroy;
    procedure AllowObjectDestruction;
    procedure CommitTransaction(MinimalMode: TBoldSystemTransactionMode = stmNormal);
    function CreateExistingObjectByID(BoldObjectID: TBoldObjectId): TBoldObject;
    function CreateNewObjectByExpressionName(const ExpressionName: string; Persistent: Boolean = True): TBoldObject;
    procedure DefaultSubscribe(Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent = breReEvaluate); override;
    procedure DelayObjectDestruction;
    procedure Discard;
    function EnsureEnclosure(ObjectList: TBoldObjectList; ValidateOnly: Boolean): Boolean;
    procedure FetchLinksWithObjects(ObjectList: TBoldObjectList; const LinkName: string; FetchedObjects: TBoldObjectList = nil);
    procedure GetAllInClass(aList: TBoldObjectList; AClass: TBoldObjectClass);
    procedure GetAllWithCondition(aList: TBoldObjectList; Condition: TBoldCondition);
    procedure GetAllInClassWithSQL(aList: TBoldObjectList; AClass: TBoldObjectClass; WhereClause, OrderByClause: String; Params: TParams = nil; JoinInheritedTables: Boolean = true; MaxAnswers: integer = -1; Offset: integer = -1);
    procedure GetAsList(ResultList: TBoldIndirectElement); override;
    function InTransaction: boolean;
    procedure RollbackTransaction(MinimalMode: TBoldSystemTransactionMode = stmNormal);
    procedure StartTransaction(MinimalMode: TBoldSystemTransactionMode = stmNormal);
    function TryCommitTransaction(MinimalMode: TBoldSystemTransactionMode = stmNormal): Boolean;
    procedure UpdateDatabase;
    procedure UpdateDatabaseWithList(ObjectList: TBoldObjectList);
    function ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean; override;
    property IsDefault: Boolean read GetIsDefault write SetIsDefault;
    property BoldSystemTypeInfo: TBoldSystemTypeInfo read fBoldSystemTypeInfo;
    property ClassByExpressionName[const ExpressionName: string]: TBoldObjectList read GetClassByExpressionName;
    property Classes[index: Integer]: TBoldObjectList read GetClassByIndex;
    property DirtyObjects: TList read GetDirtyObjects;
    property EnsuredLocatorByID[ObjectID: TBoldObjectId]: TBoldObjectLocator read GetEnsuredLocatorByID;
    property Locators: TBoldSystemLocatorList read fLocators;
    property PessimisticLockHandler: TBoldAbstractPessimisticLockHandler read fPessimisticLockHandler write SetPessimisticLockHandler;
    property OptimisticLockHandler: TBoldAbstractOptimisticLockHandler read fOptimisticLockHandler;
    property RegionFactory: TBoldAbstractRegionFactory read fRegionFactory;
    property NewDirtyList: TBoldObjectList read FNewDirtyList write FNewDirtyList;
    property NewModifiedList: TBoldObjectList read FNewModifiedList write FNewModifiedList;
    property PersistenceController: TBoldPersistenceController read fPersistenceController;
    property AsIBoldvalueSpace[Mode: TBoldDomainElementProxyMode]: IBoldvalueSpace read GetAsIBoldValueSpace;
    property TimeForTimestamp[Timestamp: TBoldTimestampType]: TDateTime read GetTimeForTimestamp;
    property TimestampForTime[ClockTime: TDateTime]: TBoldTimestampType read GetTimestampForTime;
    property TimeStampOfLatestUpdate: TBoldTimeStampType read GetTimeStampOfLatestUpdate;
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
    property locator: TBoldObjectLocator read GetLocator;
  end;

  {---TBoldSystemLocatorList---}
  TBoldSystemLocatorList = class(TBoldUnOrderedIndexableList)
  private
    function GetLocatorByID(ObjectID: TBoldObjectId): TBoldObjectLocator;
    function GetObjectByID(ObjectID: TBoldObjectId): TBoldObject;
  protected
    function TraverserClass: TBoldIndexableListTraverserClass; override;
  public
    constructor Create;
    procedure UpdateID(Locator: TBoldObjectLocator; NewObjectID: TBoldObjectId; AllowInternal: Boolean = false);
    function CreateTraverser: TBoldLocatorListTraverser;
    property LocatorByID[ObjectID: TBoldObjectId]: TBoldObjectLocator read GetLocatorByID;
    property ObjectByID[ObjectID: TBoldObjectId]: TBoldObject read GetObjectByID;
  end;

  {---TBoldObjectLocator---}
  TBoldObjectLocator = class(TBoldMemoryManagedObject)
  private
    fBoldSystem: TBoldSystem;
    fBoldObject: TBoldObject;
    fBoldObjectID: TBoldObjectId;
    fEmbeddedSingleLinks: TBoldObjectArray;
    constructor CreateWithObjectId(BoldSystem: TBoldSystem; BoldObjectID: TBoldObjectId);
    constructor CreateWithClassID(BoldSystem: TBoldSystem; TopSortedIndex: integer; Exact: Boolean);
    procedure FetchBoldObject;
    procedure EmbeddedSingleLinksToObject;
    procedure EmbeddedSingleLinksFromObject;
    procedure FreeEmbeddedSingleLinksOfOtherEnd;
    function GetAsString: string;
    function GetEnsuredBoldObject: TBoldObject;
    function GetObjectIsPersistent: Boolean;
    function GetEmbeddedSingleLinks(EmbeddedIndex: integer): TBoldObjectLocator;
    procedure SetEmbeddedSingleLinks(EmbeddedIndex: integer; const Value: TBoldObjectLocator);
    //procedure TypeAtLeast(TopSortedIndex: integer; Exact: Boolean);
  public
    destructor Destroy; override;
    function AtTime(Time: TBoldTimeStampType): TBoldObjectLocator;
    procedure DiscardBoldObject;
    procedure EnsureBoldObject;
    function Hash: Cardinal;
    procedure UnloadBoldObject;
    property AsString: string read GetAsString;
    property BoldObject: TBoldObject read FBoldObject;
    property BoldObjectID: TBoldObjectId read fBoldObjectID;
    property BoldSystem: TBoldSystem read FBoldSystem;
    property EnsuredBoldObject: TBoldObject read GetEnsuredBoldObject;
    property ObjectIsPersistent: Boolean read GetObjectIsPersistent;
    property EmbeddedSingleLinks[EmbeddedIndex: integer]: TBoldObjectLocator read GetEmbeddedSingleLinks write SetEmbeddedSingleLinks;
  end;

  {---TBoldObject---}
  TBoldObject = class(TBoldDomainElement)

  private
    fBoldClassTypeInfo: TBoldClassTypeInfo;
    fDynamicData: PPointerList; // Defined in Classes for TList
    FBoldObjectLocator: TBoldObjectLocator;
    constructor InternalCreateWithClassAndLocator(ClassTypeInfo: TBoldClassTypeInfo; Locator: TBoldObjectLocator);
    procedure CalculateMemberModified;
    function CanUnload: Boolean;
    procedure PrepareUpdateMembers;
    procedure DoStartDelete;
    procedure DoStartUpdate;
    procedure EndCreate;
    procedure EndDelete;
    procedure EndFetchMembers(MemberIdList: TBoldMemberIdList);
    procedure EndReCreate;
    procedure EndUpdate(Translationlist: TBoldIdTranslationlist);
    procedure EndUpdateMembers(Translationlist: TBoldIdTranslationlist);
    procedure FailDelete;
    function FindBoldMemberByExpressionName(const Name: string): TBoldMember;
    function GetBoldExistenceState: TBoldExistenceState;
    function GetBoldMemberCount: Integer;
    function GetBoldMembers(index: Integer): TBoldMember;
    function GetBoldPersistenceState: TBoldValuePersistenceState;
    function GetBoldSystem: TBoldSystem;
    function GetBoldTime: TBoldTimestampType;
    function GetGlobalId: string;
    function GetIsModified: Boolean;
    function GetIsReadOnly: Boolean; // implements method in IBoldObjectContents
    function GetObjectHasSubscribers: Boolean;
    function GetTimeStamp: TBoldTimeStampType;
    procedure InitializeObject(System: TBoldSystem; ClassTypeInfo: TBoldClassTypeInfo; Locator: TBoldObjectLocator; Persistent: Boolean);
    function CreateMemberByIndex(Index: integer): TBoldMember;
    procedure InitializeMember(Member: TBoldMember; MemberRTInfo: TBoldMemberRTInfo; IsNewObject: Boolean);
    function MayUpdateMembers: Boolean;
    procedure MemberBecomingClean(BoldMember: TBoldMember);
    procedure MemberBecomingModified(BoldMember: TBoldMember);
    procedure SetBoldExistenceState(Value: TBoldExistenceState);
    procedure SetBoldPersistenceState(Value: TBoldValuePersistenceState);
    procedure SetIsReadOnly(NewValue: Boolean);
    procedure SetGlobalId(const NewValue: string);
    procedure SetTimeStamp(NewValue: TBoldTimeStampType);
    function StartDelete: Boolean;
    function InternalCanDelete(CheckedObjects: TBoldDomainElementCollection; Cascade: Boolean): Boolean;
    function GetAsIBoldObjectContents(Mode: TBoldDomainElementProxyMode): IBoldObjectContents;
    function GetBoldMemberAssigned(Index: integer): Boolean;
    function GetBoldObjectIsNew: Boolean;
    function GetBoldObjectIsDeleted: Boolean;
    function GetBoldObjectExists: Boolean;
    function GetDynamicDataSize: Integer;
    property InDirtyList: Boolean index befInDirtyList read GetElementFlag write SetElementFlag;
    property IsReadOnly: Boolean read GetIsReadOnly;
    property MemberModified: Boolean index befMemberModified read GetElementFlag write SetElementFlag;
    property MemberModifiedKnown: Boolean index befMemberModifiedKnown read GetElementFlag write SetElementFlag;
    property BoldStoresTimeStamp: boolean index befStoresTimeStamp read GetElementFlag;
    function GetBoldMemberByExpressionName(const Name: string): TBoldMember;
    function GetBoldMemberIndexByExpressionName(const Name: string): Integer;
    //function GetEffectiveInvalid: Boolean;
  protected
    procedure CompleteCreate; virtual;
    procedure CompleteUpdate; virtual;
    function GetBoldDirty: Boolean; override;
    function GetBoldType: TBoldElementTypeInfo; override;
    function GetDisplayName: String; override;
    function GetEvaluator: TBoldEvaluator; override;
    function GetStringRepresentation(Representation: TBoldRepresentation): string; override;
    function GetDeriveMethodForMember(Member: TBoldMember): TBoldDeriveAndResubscribe; virtual;
    function GetReverseDeriveMethodForMember(Member: TBoldMember): TBoldReverseDerive; virtual;
    function MayDelete: Boolean; virtual;
    function MayUpdate: Boolean; virtual;
    procedure PrepareDelete; virtual;
    procedure PrepareUpdate; virtual;
    procedure StateError(S: String); override;
    function ValidateMember(const ObjectDelphiName, MemberDelphiName: String; GeneratedMemberIndex: integer; MemberClass: TBoldMemberClass): Boolean;
    procedure ToBeRemovedMemberAccessed(MemberRTInfo: TBoldMemberRTInfo); virtual;
    procedure ToBeRemovedMemberModified(MemberRTInfo: TBoldMemberRTInfo); virtual;
    procedure ToBeRemovedClassAccessed; virtual;
  public
    // must be public so that links can call it when creating linkclasses
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
    procedure Discard;
    procedure GetAsList(ResultList: TBoldIndirectElement); override;
    procedure Invalidate;
    function IsEqualAs(CompareType: TBoldCompareType; BoldElement: TBoldElement): Boolean; override;
    procedure ReceiveEventFromOwned(originator: TObject; originalEvent: TBoldEvent); override;
    procedure ReRead;
    procedure SubscribeToStringRepresentation(Representation: TBoldRepresentation; Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent = breReEvaluate); override;
    procedure UnLinkAll;
    procedure MarkObjectDirty;
    procedure ClearTouched;
    function ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean; override;
    property BoldClassTypeInfo: TBoldClassTypeInfo read fBoldClassTypeInfo;
    property BoldExistenceState: TBoldExistenceState read GetBoldExistenceState;
    property BoldMemberByExpressionName[const name: string]: TBoldMember read GetBoldMemberByExpressionName;
    property BoldMemberCount: Integer read GetBoldMemberCount;
    property BoldMemberIndexByExpressionName[const name: string]: Integer read GetBoldMemberIndexByExpressionName;
    property BoldMemberAssigned[index: Integer]: Boolean read GetBoldMemberAssigned;
    property BoldMembers[index: Integer]: TBoldMember read GetBoldMembers;
    property BoldObjectLocator: TBoldObjectLocator read FBoldObjectLocator;
    property BoldPersistenceState: TBoldValuePersistenceState read GetBoldPersistenceState;
    property BoldSystem: TBoldSystem read GetBoldSystem;
    property BoldTime: TBoldTimestampType read GetBoldTime;
    property ObjectHasSubscribers: Boolean read GetObjectHasSubscribers;
    property AsIBoldObjectContents[Mode: TBoldDomainElementProxyMode]: IBoldObjectContents read GetAsIBoldObjectContents;
    property BoldObjectIsNew: Boolean read GetBoldObjectIsNew;
    property BoldObjectIsDeleted: Boolean read GetBoldObjectIsDeleted;
    property BoldObjectExists: Boolean read GetBoldObjectExists;
    property Touched: Boolean index befTouched read GetElementFlag;
  end;

   { TBoldMember }
  TBoldMember = class(TBoldDomainElement)
  private
    fBoldMetaType: TBoldMetaElement;
    procedure AdjustOldValues(Translationlist: TBoldIdTranslationlist); virtual;
    procedure CalculateDerivedMemberWithExpression(DerivedObject: TObject; Subscriber: TBoldSubscriber);
    procedure EndUpdate(Translationlist: TBoldIdTranslationlist);
    function GetOwningObject: TBoldObject;
    function GetBoldSystem: TBoldSystem;
    function GetBoldMemberRTInfo: TBoldMemberRTInfo;
    procedure _NotifyOutOfDate;
    procedure DoSetInitialValue; virtual;
    function FindASystem: TBoldSystem;
    function GetDeriver: TBoldEventPluggedDeriver;
    function GetElementTypeInfoForType: TBoldElementTypeInfo; virtual;
    function GetIsPartOfSystem: Boolean;
    function GetIsReadOnly(Flag: TBoldElementFlag): Boolean;
    procedure InitializeStateToInvalid;
    procedure InitializeStateToModified;
    procedure InitializeStateToTransient;
    function InternalMayUpdate: Boolean; virtual;
    function IsInvalid: Boolean;
    procedure MakeDbCurrent; virtual;
    procedure ObjectBecomingPersistent;
    procedure InternalDiscard;
    function GetAsIBoldValue(Mode: TBoldDomainElementProxyMode): IBoldValue;
    function GetController: TBoldAbstractController; virtual;
    procedure MarkMemberDirty;
    procedure AssignContentValueFromElement(source: TBoldElement); virtual;
    property HasDeriver: Boolean index befHasDeriver read GetElementFlag write SetElementFlag;
    property OwnedByObject: Boolean index befOwnedByObject read GetElementFlag write SetElementFlag;
    property IsReadOnly: Boolean index befMemberReadOnly read GetIsReadOnly write SetElementFlag;
    function GetDeriverState: TBoldDeriverState;
    procedure SetDeriverState(value: TBoldDeriverState);
    function GetOldvalue: IBoldvalue;
    constructor InternalCreate(OwningObject: TBoldObject; MemberRTInfo: TBoldMemberRTInfo; SetInitialValue: Boolean);
    procedure PreDiscard; virtual;
    property DeriverState: TBoldDeriverState read GetDeriverState write SetDeriverState;
  protected
    procedure DoStartModify;
    procedure Changed(Event: TBoldEvent; const Args: array of const);
    procedure CompleteModify; virtual;
    procedure CompleteUpdate; virtual;
    procedure EndModify;
    procedure FailModify;
    procedure FreeContent; virtual;
    function GetBoldDirty: Boolean; override;
    function GetBoldPersistenceState: TBoldValuePersistenceState;
    function GetBoldType: TBoldElementTypeInfo; override;
    function GetDisplayName: String; override;
    function GetEvaluator: TBoldEvaluator; override;
    function GetStreamName: string; virtual;
    procedure InitializeMember(AOwningElement: TBoldDomainElement; ElementTypeInfo: TBoldElementTypeInfo); virtual;
    function MayModify: Boolean; virtual;
    function MayUpdate: Boolean; virtual;
    procedure PreChange;
    procedure PrepareModify; virtual;
    procedure PrepareUpdate; virtual;
    procedure SetBoldPersistenceState(Value: TBoldValuePersistenceState);
    procedure StateError(S: String); override;
    function StartModify: Boolean; virtual;
    function RetrieveProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj; const InterfaceName: string): Boolean;
    function ProxyClass: TBoldMember_ProxyClass; virtual; abstract;
    function CloneIfPossible: TBoldElement; override;
  public
    constructor Create; reintroduce;
    constructor CreateWithTypeInfo(ElementTypeInfo: TBoldElementTypeInfo);
    destructor Destroy; override;
    function AtTime(Time: TBoldTimestampType): TBoldMember; virtual;
    function CanModify: Boolean;
    function CanUpdate: Boolean;
    function MemberHasSubscribers: Boolean;
    function Clone: TBoldMember;
    function IsEqualToValue(Value: IBoldValue): Boolean; virtual;
    function StoreInUndo: Boolean; { TODO : Move to RTInfo }
    procedure Discard;
    procedure EnsureContentsCurrent;
    procedure Refetch;
    procedure GetAsList(ResultList: TBoldIndirectElement); override;
    procedure GetAsValue(ResultElement: TBoldIndirectElement); override;
    procedure Invalidate;
    function CanRead(Subscriber: TBoldSubscriber): Boolean;
    function ObserverMayModify(Observer: TObject): Boolean; override;
    function ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean; override;
    property BoldMemberRTInfo: TBoldMemberRTInfo read GetBoldMemberRTInfo;
    property BoldSystem: TBoldSystem read GetBoldSystem;
    property BoldPersistenceState: TBoldValuePersistenceState read GetBoldPersistenceState write SetBoldPersistenceState;
    property Derived: Boolean index befDerived read GetElementFlag;
    property Deriver: TBoldEventPluggedDeriver read GetDeriver;
    property IsPartOfSystem: Boolean read GetIsPartOfSystem;
    property OwningObject: TBoldObject read GetOwningObject;
    property AsIBoldValue[Mode: TBoldDomainElementProxyMode]: IBoldValue read GetAsIBoldValue;
    property OldValue: IBoldValue read GetOldValue;
    property Touched: Boolean index befTouched read GetElementFlag;
  end;

  { TBoldMember_Proxy }
  TBoldMember_Proxy = class(TBoldDomainElement_Proxy, IBoldValue, IBoldStreamable)
  private
    function GetProxedMember: TBoldMember;
    function GetProxedController: TBoldAbstractController;
  protected
    property ProxedMember: TBoldMember read GetProxedMember;
    property ProxedController: TBoldAbstractController read GetProxedController;
    function GetContentName: String;
    function GetStreamName: String;
    procedure AssignContent(Source: IBoldValue);
    procedure AssignContentValue(Source: IBoldValue); virtual; abstract;
    function GetBoldPersistenceState: TBoldValuePersistenceState;
    procedure SetBoldPersistenceState(Value: TBoldValuePersistenceState);
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
    procedure AssignValue(Source: IBoldValue); virtual; abstract;
    procedure AssignContentValue(Source: IBoldValue); virtual; abstract;
    procedure DoSetInitialValue; override;
    function GetContentIsNull: Boolean;
    class function EitherIsNull(Attribute1, Attribute2: TBoldAttribute): Boolean;
    procedure EnsureNotNull;
    procedure FormatFailure(const value, ExpectedDataType: String);
    function GetStreamName: string; override;
    function NullBiggest(BoldElement: TBoldElement): Integer;
    function NullSmallest(BoldElement: TBoldElement): Integer;
    procedure SetContentToNull;
    procedure SetToNonNull;
    property ContentIsNull: Boolean read GetContentIsNull;
  public
    function CompareToAs(CompType: TBoldCompareType; BoldElement: TBoldElement): Integer; override;
    procedure DefaultSubscribe(Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent = breReEvaluate); override;
    function CanSetToNull(Subscriber: TBoldSubscriber): Boolean;
    function IsEqualToValue(Value: IBoldValue): Boolean; override;
    procedure RecycleValue;
    procedure SetToNull; virtual;
    procedure Assign(Source: TBoldElement); override;
    function ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean; override;
    procedure SubscribeToStringRepresentation(Representation: TBoldRepresentation; Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent = breReEvaluate); override;
    procedure SetEmptyValue; virtual;
    property BoldAttributeRTInfo: TBoldAttributeRTInfo read GetBoldAttributeRTInfo;
    property IsNull: Boolean read GetIsNull;
  end;

  { TBoldAttribute_Proxy }
  TBoldAttribute_Proxy = class(TBoldMember_Proxy, IBoldNullableValue)
  private
    function GetProxedAttribute: TBoldAttribute;
  protected
    procedure AssignContentValue(Source: IBoldValue); override;
    procedure SetContentToNull;
    function GetContentIsNull: Boolean;
    property ProxedAttribute: TBoldAttribute read GetProxedAttribute implements IBoldNullableValue;
  end;

  { TBoldFailureReason }
  TBoldFailureReason = class(TObject)
  private
    FMessageFormatStr: String;
    fOriginator: TBoldDomainElement;
    fReason: string;
    fSubscriber: TBoldPassThroughSubscriber;
    procedure ReceiveOriginatorDestroy(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
  protected
    function GetException(const Msg: String): EBoldFailure; virtual;
  public
    constructor create(reason: String; Originator: TBoldDomainElement);
    constructor CreateFmt(Reason: string; const args: array of const; Originator: TBoldDomainElement);
    destructor Destroy; override;
    property MessageFormatStr: String read fMessageFormatStr write fMessageFormatStr;
    property Originator: TBoldDomainElement read fOriginator;
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
  protected
    function GetStreamName: String; override;
    function GetStringRepresentation(Representation: TBoldRepresentation): string; override;
    procedure InitializeMember(AOwningElement: TBoldDomainElement; ElementTypeInfo: TBoldElementTypeInfo); override;
    procedure SetStringRepresentation(Representation: TBoldRepresentation; Value: string); override;
    function ProxyClass: TBoldMember_ProxyClass; override;
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
    function IsEqualToValue(Value: IBoldValue): Boolean; override;
    function ObserverMayModify(Observer: TObject): Boolean; override;
    function ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean; override;
    procedure SubscribeToStringRepresentation(Representation: TBoldRepresentation; Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent = breReEvaluate); override;
    property BoldObject: TBoldObject read GetBoldObject write SetBoldObject;
    property BoldRoleRTInfo: TBoldRoleRTInfo read GetBoldRoleRTInfo;
    property Locator: TBoldObjectLocator read GetLocator write SetLocator;
    property OldEmbeddingOtherEndId: TBoldObjectId read GetOldEmbeddingOtherEndId;
    property HasOldValues: Boolean index befHasOldValues read GetElementFlag write SetElementFlag;
  end;

  { TBoldList }
  TBoldList = class(TBoldMember)
  private
    fListController: TBoldListController;
    function GetController: TBoldAbstractController; override;
    function GetDuplicateMode: TBoldListDupMode;
    procedure PrepareClear; virtual;
    procedure SetDuplicateMode(NewMode: TBoldListDupMode);
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
    procedure InitializeMember(AOwningElement: TBoldDomainElement; ElementTypeInfo: TBoldElementTypeInfo); override;
    procedure InsertElement(index: Integer; Element: TBoldElement); virtual; abstract;
    function InternalAddNew: TBoldElement; virtual; abstract;
    function GetCanCreateNew: Boolean; virtual;
    procedure SetElement(index: Integer; Value: TBoldElement); virtual; abstract;
    procedure InternalClear; virtual; abstract;
    property ListController: TBoldListController read fListController write fListController;
  public
    destructor Destroy; override;
    procedure Add(Element: TBoldElement);
    procedure AddList(List: TBoldList); virtual;
    function AddNew: TBoldElement;
    procedure AddToStrings(Representation: TBoldRepresentation; S: TStrings);
    function CanClear(Subscriber: TBoldSubscriber): Boolean;
    function CanInsert(index: Integer; Element: TBoldElement; Subscriber: TBoldSubscriber): Boolean; virtual;
    function CanMove(CurIndex, NewIndex: Integer; Subscriber: TBoldSubscriber = nil): Boolean; virtual;
    function CanRemove(index: Integer; Subscriber: TBoldSubscriber): Boolean; virtual;
    function CanSet(index: Integer; Item: TBoldElement; Subscriber: TBoldSubscriber): Boolean; virtual;
    procedure Clear;
    procedure DefaultSubscribe(Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent = breReEvaluate); override;
    procedure EnsureRange(FromIndex: integer; ToIndex: integer); virtual;
    procedure GetAsList(ResultList: TBoldIndirectElement); override;
    function Includes(Item: TBoldElement): Boolean;
    function IndexOf(Item: TBoldElement): Integer;
    procedure Insert(index: Integer; Element: TBoldElement);
    procedure InsertNew(index: Integer); virtual; abstract;
    procedure Move(CurIndex, NewIndex: Integer); virtual; abstract;
    procedure MakeContentsImmutable;
    procedure Remove(Item: TBoldElement); virtual;
    procedure RemoveByIndex(index: Integer); virtual; abstract;
    procedure Sort(CompareFunc: TBoldElementCompare);
    procedure ToStrings(Representation: TBoldRepresentation; S: TStrings);
    procedure ToStringsWithNil(Representation: TBoldRepresentation; S: TStrings; nilString: string);
    property CanCreateNew: Boolean read GetCanCreateNew;
    property Count: Integer read GetCount;
    property DuplicateMode: TBoldListDupMode read GetDuplicateMode write SetDuplicateMode;
    property Elements[index: Integer]: TBoldElement read GetElement write SetElement; default;
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
    procedure InternalAddWithoutCloning(Item: TBoldMember); // FIXME remove when TBoldClass no longer member
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
    function IncludesElement(Item: TBoldElement): Boolean; override;
    function IndexOfElement(Item: TBoldElement): Integer; override;
    procedure InitializeMember(AOwningElement: TBoldDomainElement; ElementTypeInfo: TBoldElementTypeInfo); override;
    procedure InsertElement(index: Integer; Element: TBoldElement); override;
    procedure SetElement(index: Integer; Value: TBoldElement); override;
    function InternalAddNew: TBoldElement; override;
    function ProxyClass: TBoldMember_ProxyClass; override;
    procedure InternalClear; override;
  public
    procedure Add(Item: TBoldMember);
    procedure Assign(Source: TBoldElement); override;
    function IndexOf(Item: TBoldMember): Integer;
    procedure Insert(index: Integer; Item: TBoldMember);
    procedure InsertNew(index: Integer); override;
    procedure Move(CurIndex, NewIndex: Integer); override;
    procedure RemoveByIndex(index: Integer); override;
    property BoldMembers[index: Integer]: TBoldMember read GetBoldMember write SetBoldMember; default;
    property CloneMembers: Boolean read FCloneMembers write SetCloneMembers;
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
    property ObjectListController: TBoldAbstractObjectListController read GetObjectListController;
  protected
    procedure AddElement(Element: TBoldElement); override;
    procedure AllocateData; override;
    procedure FreeData; override;
    function GetCount: Integer; override;
    function GetElement(index: Integer): TBoldElement; override;
    function GetStreamName: String; override;
    function IncludesElement(Item: TBoldElement): Boolean; override;
    function IndexOfElement(Item: TBoldElement): Integer; override;
    procedure InitializeMember(AOwningElement: TBoldDomainElement; ElementTypeInfo: TBoldElementTypeInfo); override;
    procedure InsertElement(index: Integer; Element: TBoldElement); override;

    procedure SetElement(index: Integer; Value: TBoldElement); override;
    function ProxyClass: TBoldMember_ProxyClass; override;
    procedure FreeContent; override;
    function InternalAddNew: TBoldElement; override;
    procedure InternalClear; override;
  public
    constructor InternalCreateClassList(System: TBoldSystem; ListTypeInfo: TBoldListTypeINfo); // used by ClassListController
    constructor CreateTypedList(ObjectClass: TBoldObjectClass);
    procedure Add(BoldObject: TBoldObject);
    procedure AddList(List: TBoldList); override;
    procedure AddLocator(NewLocator: TBoldObjectLocator);
    procedure Assign(Source: TBoldElement); override;
    function AtTime(Time: TBoldTimestampType): TBoldMember; override;
    function CreateObjectIdList: TBoldObjectIdList;
    procedure EnsureObjects;
    procedure EnsureRange(FromIndex: integer; ToIndex: integer); override;
    procedure FillFromIDList(ObjectIdList: TBoldObjectIdList; BoldSystem: TBoldSystem);
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
    property BoldObjects[index: Integer]: TBoldObject read GetBoldObject write SetBoldObject; default;
    property BoldRoleRTInfo: TBoldRoleRTInfo read GetBoldRoleRTInfo;
    property Locators[index: Integer]: TBoldObjectLocator read GetLocator write SetLocator;
    property SubscribeToObjectsInList: Boolean read GetSubscribeToObjectsInList write SetSubscribeToObjectsInList;
    property SubscribeToLocatorsInList: Boolean read GetSubscribeToLocatorsInList write SetSubscribeToLocatorsInList;
    property Adjusted: Boolean index befAdjusted read GetElementFlag write SetElementFlag;
  end;

  TBoldLinkUnlinkMode = (blulNone, blulMarkModified, blulMarkAdjusted);

  {---TBoldAbstractController---}
  TBoldAbstractController = class(TBoldMemoryManagedObject)
  protected
    procedure Changed(Event: TBoldEvent; const Args: array of const);
    function GetStreamName: string; virtual; abstract;
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
  end;

  {---TBoldListController---}
  TBoldListController = class(TBoldAbstractController)
  private
    fOwningList: TBoldList;
    function GetBoldSystem: TBoldSystem;
  protected
    function GetOwningMember: TBoldMember; override;
    property OwningList: TBoldList read fOwningList;
    function GetCount: Integer; virtual; abstract;
    function GetCanCreateNew: Boolean; virtual;
    function CreateNew: TBoldElement; virtual;
    function GetStringrepresentation: String; virtual;
  public
    constructor Create(OwningList: TBoldList);
    procedure AddElement(Element: TBoldElement); virtual; abstract;
    function GetElement(index: Integer): TBoldElement; virtual; abstract;
    function IncludesElement(Item: TBoldElement): Boolean; virtual; abstract;
    function IndexOfElement(Item: TBoldElement): Integer; virtual; abstract;
    procedure InsertElement(index: Integer; Element: TBoldElement); virtual; abstract;
    procedure Move(CurrentIndex: Integer; NewIndex: Integer); virtual; abstract;
    procedure RemoveByIndex(index: Integer); virtual; abstract;
    procedure SetElement(index: Integer; Value: TBoldElement); virtual; abstract;
    property BoldSystem: TBoldSystem read GetBoldSystem;
    property CanCreateNew: Boolean read GetCanCreateNew;
    property Count: integer read GetCount;
  end;

  {---TBoldAbstractObjectListController---}
  TBoldAbstractObjectListController = class(TBoldListController)
  private
  protected
    function GetObjectList: TBoldObjectList;
    property OwningObjectList: TBoldObjectList read GetObjectList;
    function ProxyClass: TBoldMember_ProxyClass; virtual; abstract;
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
    function ProxyClass: TBoldMember_ProxyClass; virtual; abstract;
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
    function ProxyClass: TBoldMember_ProxyClass; override;
  public
    constructor Create(Owner: TBoldObjectReference); override;
    destructor Destroy; override;
    procedure AssignContentValue(Source: IBoldValue);
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

function GetBoldLastFailureReason: TBoldFailureReason;
procedure SetBoldLastFailureReason(const Value: TBoldFailureReason);
procedure BoldRaiseLastFailure(originator: TBoldDomainElement; const MethodName: String; const DefaultMessage: String);
procedure BoldClearLastFailure;

implementation

uses
  SysUtils,
  BoldMemoryManager,
  BoldLogHandler,
  BoldGuard,
  BoldLinks,
  BoldObjectListControllers,
  BoldOcl,
  Typinfo,
  BoldOptimisticLockingSupport, // could be avoided with factory
  BoldSystemPersistenceHandler, // could be avoided with factory
  BoldSystemOldValueHandler, // could be avoided with factory, will need mechanism for selectiing several
  BoldExternalizedReferences,
  BoldDefaultId,
  BoldTaggedValueSupport,
  BoldUndoHandler,
  BoldDefaultStreamNames,
  BoldCoreConsts;

var
  G_ExternalDerivers: TBoldExternalizedReferenceList;
  G_LastFailureReason: TBoldFailureReason = nil;
  G_DefaultBoldSystem: TBoldSystem = nil;
  IX_BoldObjectId: integer = -1;

// Utility functions
// If object exists, just return it, otherwise create it and set existance and persistencestate.

function EnsureObjectInFsValueSpace(BoldObject: TBoldObject; ValueSpace: TBoldFreeStandingValueSpace): TBoldFreeStandingObjectContents;
var
  ObjectId: TBoldObjectId;
begin
  Assert(assigned(ValueSpace));
  ObjectId := BoldObject.BoldObjectLocator.BoldObjectID;
  Result := ValueSpace.GetFSObjectContentsByObjectId(ObjectID);
  if not assigned(Result) then
  begin
    Result := ValueSpace.GetEnsuredFSObjectContentsByObjectId(ObjectId);
    Result.BoldExistenceState := BoldObject.BoldExistenceState;
    Result.BoldPersistenceState := BoldObject.BoldPersistenceState;
  end;
end;

type
  {---TBoldLocatorIdHashIndex---}
  TBoldLocatorIdHashIndex = class(TBoldHashIndex)
  protected
    function ItemAsLocator(Item: TObject): TBoldObjectLocator; virtual;
    function HashItem(Item: TObject): Cardinal; override;
    function Match(const Key; Item:TObject):Boolean; override;
    function Hash(const Key): Cardinal; override;
  public
    function FindLocatorById(BoldObjectId: TBoldObjectId): TBoldObjectLocator;
  end;

  { TBoldSystem_Proxy }
  TBoldSystem_Proxy = class(TBoldDomainElement_Proxy, IBoldValueSpace)
  private
    function GetProxedSystem: TBoldSystem;
    // IBoldValueSpace
    procedure AllObjectIds(resultList: TBoldObjectIdList; OnlyLoaded: Boolean);
    procedure ApplytranslationList(IdTranslationList: TBoldIdTranslationList);
    procedure ApplyValueSpace(ValueSpace: IBoldValueSpace; IgnorePersistenceState: Boolean);
    procedure EnsureObjectContents(ObjectId: TBoldObjectId);
    procedure EnsureObjectId(ObjectId: TBoldObjectId);
    procedure ExactifyIDs(TranslationList: TBoldIdTranslationList);
    function GetHasContentsForId(ObjectId: TBoldObjectId): boolean;
    function GetObjectContentsByObjectId(ObjectId: TBoldObjectId): IBoldObjectContents;
    function GetEnsuredObjectContentsByObjectId(ObjectId: TBoldObjectId): IBoldObjectContents;
  protected
    property ProxedSystem: TBoldSystem read GetProxedSystem;
  end;

  { TBoldObject_Proxy }
  TBoldObject_Proxy = class(TBoldDomainElement_Proxy, IBoldObjectContents)
  private
    function GetProxedObject: TBoldObject;
    // IBoldObjectContents
    procedure EnsureMember(MemberId: TBoldMemberId; const ContentName: string);
    function GetBoldExistenceState: TBoldExistenceState;
    function GetBoldMemberCount: Integer;
    function GetBoldPersistenceState: TBoldValuePersistenceState;
    function GetGlobalId: string;
    function GetIsModified: Boolean;
    function GetIsReadOnly: Boolean;
    function GetObjectId: TBoldObjectId;
    function GetValueByIndex(I: Integer): IBoldValue;
    function GetValueByMemberId(MemberId: TBoldMemberID):IBoldValue;
    function GetTimeStamp: TBoldTimeStampType;
    procedure SetBoldExistenceState(Value: TBoldExistenceState);
    procedure SetBoldPersistenceState(Value: TBoldValuePersistenceState);
    procedure SetGlobalId(const NewValue: string);
    procedure SetIsReadOnly(NewValue: Boolean);
    procedure SetTimeStamp(NewValue: TBoldTimeStampType);
  protected
    property ProxedObject: TBoldObject read GetProxedObject;
  end;

  { TBoldObjectReference_Proxy }
  TBoldObjectReference_Proxy = class(TBoldMember_Proxy)
  protected
    procedure AssignContentValue(Source: IBoldValue); override;
  end;

  TBoldMemberDeriver = class(TBoldEventPluggedDeriver)
  protected
    procedure SetInternalDeriverState(const Value: TBoldDeriverState); override;
    function GetInternalDeriverState: TBoldDeriverState; override;
  end;

// Utility functions

function GetValueFromValuespace(ValueSpace: IBoldValueSpace; Id: TBoldObjectId; MemberIndex: integer): IBoldValue;
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

procedure BoldRaiseLastFailure(originator: TBoldDomainElement; const MethodName: String; const DefaultMessage: String);
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

constructor TBoldSystemLocatorList.Create;
begin
  inherited;
  SetIndexCapacity(1);
  SetIndexVariable(IX_BoldObjectId, AddIndex(TBoldLocatorIdHashIndex.Create));
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

function TBoldSystemLocatorList.GetLocatorByID(ObjectID: TBoldObjectId): TBoldObjectLocator;
begin
  Result := TBoldLocatorIdHashIndex(Indexes[IX_BoldObjectId]).FindLocatorById(ObjectID);
end;

function TBoldSystem.GetEnsuredLocatorByID(ObjectID: TBoldObjectId): TBoldObjectLocator;
begin
  Result := Locators.GetLocatorByID(ObjectID);
  if not Assigned(Result) then
  begin
    Result := TBoldObjectLocator.CreateWithObjectId(Self, ObjectID)
  end;
end;

constructor TBoldObjectLocator.CreateWithObjectId(BoldSystem: TBoldSystem; BoldObjectID: TBoldObjectId);
begin
  inherited Create;
  fBoldSystem := BoldSystem;
  fBoldObjectID := BoldObjectId.Clone;
  BoldSystem.Locators.Add(Self);
end;

constructor TBoldObjectLocator.CreateWithClassID(BoldSystem: TBoldSystem; TopSortedIndex: integer; Exact: Boolean);
begin
  inherited Create;
  FBoldSystem := BoldSystem;
  fBoldObjectID := TBoldInternalObjectId.CreateWithClassID(TopSortedIndex, Exact);
  BoldSystem.Locators.Add(Self);
end;

function TBoldObjectLocator.GetAsString: string;
begin
  Result := fBoldObjectID.AsString;
end;

destructor TBoldObjectLocator.Destroy;
begin
  if Assigned(FBoldObject) then
    raise EBoldInternal.CreateFmt(sBoldObjectAssigned, [ClassName]);
  FreeAndNil(fBoldObjectID);
  FreeAndNil(fEmbeddedSingleLinks);
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
      BoldRaiseLastFailure(nil, 'TBoldObjectLocator.UnloadBoldObject', ''); // do not localize

    BoldSystem.MarkObjectClean(BoldObject);
    BoldObject.FreePublisher;
    BoldObject.FBoldObjectLocator := nil;
    EmbeddedSingleLinksFromObject;
    FreeEmbeddedSingleLinksOfOtherEnd;
    FreeAndNil(fBoldObject);
  end;
end;

procedure TBoldObjectLocator.DiscardBoldObject;
begin
  if Assigned(BoldObject) then
    BoldObject.Discard;
end;

procedure TBoldObjectLocator.FetchBoldObject;
begin
  if BoldObjectID.IsStorable then
    BoldSystem.fSystemPersistenceHandler.FetchObjectById(BoldObjectID)
  else
    raise EBoldInternal.CreateFmt('%s.FetchBoldObject: Can''t fetch Internal object', [Classname]);
end;

function TBoldObjectLocator.GetEnsuredBoldObject: TBoldObject;
begin
  if not assigned(self) then
    result := nil
  else
  begin
    EnsureBoldObject;
    result := BoldObject;
  end;
end;

function TBoldObjectLocator.Hash: Cardinal;
begin
  result := Cardinal(self);
end;

{ TBoldSystem }
constructor TBoldSystem.Create(AOwningElement: TBoldDomainElement);
begin
  raise EBold.CreateFmt(sIllegalConstruction, [ClassName]);
end;

function TBoldSystem.GetAsIBoldvalueSpace(Mode: TBoldDomainElementProxyMode): IBoldvalueSpace;
begin
  ProxyInterface(IBoldValueSpace, Mode, result);
end;

constructor TBoldSystem.CreateWithTypeInfo(AOwningElement: TBoldDomainElement; SystemTypeInfo: TBoldSystemTypeInfo; PersistenceController: TBoldPersistenceController; RegionFactory: TBoldAbstractRegionFactory = nil);
var
  I: Integer;
  ListTypeInfo: TBoldListTypeInfo;
  ListClass: TBoldObjectListClass;
begin
  inherited Create(AOwningElement);
  fTransactionMode := stmNormal;
  fOldValueHandler := TBoldOldValueHandler.Create(Self);
  fBoldSystemTypeInfo := SystemTypeInfo;
  SetElementFlag(befpersistent, SystemTypeInfo.Persistent and assigned(PersistenceController));
  fPersistenceController := PersistenceController;
  fDirtyObjects := TList.Create;
  fDirtyObjectsInvalid := False;
  fClasses := TBoldMemberList.Create;
  fClasses.fBoldMetaType := BoldSystemTypeInfo.ListTypeInfoByElement[nil];
  fLocators := TBoldSystemLocatorList.Create;
  fLocators.OwnsEntries := True;
  fDelayedDestructionList := TList.Create;
  fTransactionList := TBoldDomainElementCollection.Create;

  for I := 0 to SystemTypeInfo.TopSortedClasses.Count - 1 do
  begin
    ListTypeInfo := SystemTypeInfo.ListTypeInfoByElement[SystemTypeInfo.TopSortedClasses[i]];
    ListClass := TBoldObjectListClass(ListTypeInfo.ListClass);
    fClasses.InternalAddWithoutCloning(ListClass.InternalCreateClassList(self, ListTypeInfo));
  end;

  fClasses.MakeImmutable;
  fOptimisticLockHandler := TBoldOptimisticLockHandler.Create(self);
  fSystemPersistenceHandler := TBoldSystemPersistenceHandler.Create(self);
  fRegionFactory := RegionFactory;

  if assigned(fRegionFactory) then
    fRegionFactory.fSystem := Self;
end;

destructor TBoldSystem.Destroy;
var
  Traverser: TBoldLocatorListTraverser;
  i: integer;
  bo: TBoldObject;
begin
  EnsureCanDestroy;
  PrepareToDestroy;
  IsDefault := False;
  FreeAndNil(fEvaluator);
  FreeAndNil(fClasses); // will destroy classes

  // discard all derived members first
  Traverser := Locators.CreateTraverser;
  while not Traverser.EndOfList do
  begin
    if assigned(Traverser.Locator.BoldObject) then
    begin
      bo := Traverser.Locator.BoldObject;
      for i := 0 to bo.BoldMemberCount - 1 do
      begin
        if bo.BoldMemberAssigned[i] and bo.BoldMembers[i].Derived then
        begin
          if (bo.BoldMembers[i].BoldPersistenceState <> bvpsInvalid) then
          begin
            bo.BoldMembers[i].Invalidate;
            bo.BoldMembers[i].Deriver.MarkSubscriptionOutOfDate;
          end;
        end;
      end;
    end;
    Traverser.Next;
  end;
  Traverser.Free;

  if Assigned(fLocators) then
  begin
    Traverser := Locators.CreateTraverser;
    while not Traverser.EndOfList do
    begin
      if assigned(Traverser.Locator.BoldObject) then
        Traverser.Locator.UnloadBoldObject;
      Traverser.Next;
    end;
    Traverser.Free;
  end;

  FreeAndNil(fLocators);
  FreeAndNil(fDirtyObjects);
  FreeAndNil(fDelayedDestructionList);
  FreeAndNil(fTransactionList);
  FreeAndNil(fOldValueHandler);
  FreeAndNil(fOptimisticLockHandler);
  FreeAndNil(fSystemPersistenceHandler);
  FreeAndNil(fUndoHandler);
  inherited Destroy;
end;

class function TBoldSystem.DefaultSystem: TBoldSystem;
begin
  Result := G_DefaultBoldSystem
end;

function TBoldSystem.GetClassByIndex(index: Integer): TBoldObjectList;
begin
  if Assigned(fClasses) then
  begin
    Assert(fClasses[index] is TBoldObjectList);
    Result := TBoldObjectList(fClasses[index]);
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
    raise EBold.CreateFmt(sNoSuchClass, [ClassName, 'GetClassByExpressionName', ExpressionName]); // FIXME // do not localize
end;

procedure TBoldSystem.MarkObjectDirty(BoldObject: TBoldObject); {called by TBoldObject}
begin
  if BoldObject.BoldPersistent then
  begin
    if Assigned(NewDirtyList) then
      NewDirtyList.Add(BoldObject);
    if Assigned(NewModifiedList) then
      NewModifiedList.Add(BoldObject);

    if not BoldObject.InDirtyList then
    begin
      fDirtyObjects.Add(BoldObject);
      BoldObject.InDirtyList := True;
    end;
  end;
end;

procedure TBoldSystem.MarkObjectClean(BoldObject: TBoldObject); {called by TBoldObject}
begin
  if BoldObject.InDirtyList and BoldObject.BoldPersistent then
  begin
    fDirtyObjects.Remove(BoldObject);
    BoldObject.InDirtyList := False;

    if (Assigned(NewDirtyList) and NewDirtyList.Includes(BoldObject)) then
      NewDirtyList.Remove(BoldObject);

    if not fDirtyObjectsInvalid then
      SendEvent(beDirtyListInvalidOrItemDeleted);
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

procedure TBoldSystem.FetchLinksWithObjects(ObjectList: TBoldObjectList; const LinkName: string; FetchedObjects: TBoldObjectList = nil);
begin
  SystemPersistenceHandler.FetchLinksWithObjects(ObjectList, LinkName, FetchedObjects);
end;

procedure TBoldSystem.DirtyAsObjectList(ObjectList: TBoldObjectList);
var
  i: Integer;
begin
  for i := 0 to DirtyObjects.Count - 1 do
    ObjectList.Add(TBoldObject(DirtyObjects[i]));
end;

procedure TBoldSystem.UpdateDatabase;
var
  g: IBoldGuard;
  aList: TBoldObjectList;
begin
  g := TBoldGuard.Create(aList);
  aList := TBoldObjectList.CreateWithTypeInfo(BoldSystemTypeInfo.ListTypeInfoByElement[BoldSystemTypeInfo.RootClassTypeInfo]);
  DirtyAsObjectList(alist);
  UpdateDatabaseWithList(aList);
end;

procedure TBoldSystem.UpdateDatabaseWithList(ObjectList: TBoldObjectList);
begin
  if Assigned(fUndoHandler) then
    fUndoHandler.PrepareUpdate(ObjectList);
  SystemPersistenceHandler.UpdateDatabaseWithList(ObjectList);
  fOldValueHandler.PurgeEqualValues;
  // when all objects are saved, there should be nothing in the oldvalues-handler
{
  if (DirtyObjects.Count = 0) and  not (fOldValueHandler.IsEmpty) then
    BoldLog.Log('OldValueHandler is not empty after a complete save');
}
end;

procedure TBoldSystem.DiscardPersistent;
var
  LocalDirtyObjects: TList;
  i: integer;
begin
  DelayObjectDestruction;
  try
    // The while construction is to ensure that objects that get dirty due to
    // discarding an object in the list are also discarded
    // (Most likely to happen with links that are saved within another object or
    // by themselves)
    while DirtyObjects.Count > 0 do
    begin
      LocalDirtyObjects := DirtyObjects; // this is just to clean the list from non-dirty objects
      for i := LocalDirtyObjects.Count - 1 downto 0 do
        TBoldObject(LocalDirtyObjects[i]).BoldObjectLocator.DiscardBoldObject;
    end;
  finally
    AllowObjectDestruction;
  end;
end;

procedure TBoldSystem.DiscardTransient;
var
  g: IBoldGuard;
  Traverser: TBoldLocatorListTraverser;
begin
  g := TBoldGuard.Create(Traverser);
  Traverser := Locators.CreateTraverser;
  while not Traverser.EndOfList do
  begin
    if assigned(Traverser.Locator.BoldObject) and not Traverser.Locator.BoldObject.BoldPersistent then
      Traverser.Locator.DiscardBoldObject;
    Traverser.Next;
  end;
end;

procedure TBoldSystem.Discard;
begin
  DiscardPersistent;
  DiscardTransient;
  if assigned(PessimisticLockHandler) then
    PessimisticLockHandler.ReleaseUnneededRegions;
end;

procedure TBoldSystem.DefaultSubscribe(Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent = breReEvaluate);
begin
  // FIXME what here?
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
  if fDirtyObjectsInvalid then
  begin
    i := 0;
    while i < fDirtyObjects.Count do
      if assigned(fDirtyObjects[i])
      and not TBoldObject(fDirtyObjects[i]).BoldDirty then
        MarkObjectClean(TBoldObject(fDirtyObjects[i]))
      else
        inc(i);
    fDirtyObjects.Pack;
    fDirtyObjectsInvalid := False;
  end;
  result := fDirtyObjects;
end;

procedure TBoldSystem.MarkObjectPossiblyCleaner(BoldObject: TBoldObject);
begin
  if not fDirtyObjectsInvalid then
    SendEvent(beDirtyListInvalidOrItemDeleted);
  fDirtyObjectsInvalid := True;
end;

function TBoldSystem.GetBoldDirty: Boolean;
begin
  result := DirtyObjects.Count <> 0;
end;

function TBoldSystem.GetStringRepresentation(
  Representation: TBoldRepresentation): string;
begin
  result := BoldSystemTypeInfo.ExpressionName;
end;

procedure TBoldSystem.AllowObjectDestruction;
var
  i: Integer;
  anObject: TBoldObject;
begin
  if fDelayedDestructionCount <= 0 then
    raise EBold.CreateFmt(sDestructionNestingMismatch, [classname]);
  dec(fDelayedDestructionCount);
  if fDelayedDestructionCount = 0 then
  begin
    for i := 0 to fDelayedDestructionList.Count - 1 do
    begin
      Assert(TObject(fDelayedDestructionList[i]) is TBoldObject);
      anObject := TBoldObject(fDelayedDestructionList[i]);
      if (anObject.BoldPersistenceState <> bvpsModified) and
         (anObject.BoldExistenceState <> besExisting) then
        DestroyObject(anObject);
    end;
    fDelayedDestructionList.Clear;
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
  if (BoldObject.BoldPersistenceState = bvpsModified) or
     (BoldObject.BoldExistenceState = besExisting) then
    raise EBold.CreateFmt(sObjectNotDestroyable, [classname]);
  if fDelayedDestructionCount > 0 then
    fDelayedDestructionList.Add(BoldObject)
  else
  begin
    aLocator := BoldObject.BoldObjectLocator;
    SendExtendedEvent(beLocatorDestroying, [BoldObject]);
    aLocator.UnloadBoldObject;
    Locators.Remove(aLocator);
  end;
end;

procedure TBoldSystem.ReceiveEventFromOwned(originator: TObject;
  originalEvent: TBoldEvent);
var
  ClassList: TBoldObjectList;
begin
  if (OriginalEvent in [beObjectCreated, beObjectDeleted, beObjectFetched])
     and (originator is TBoldObject)
    then
  begin
    ClassList := Classes[TBoldObject(Originator).BoldClassTypeInfo.TopSortedIndex];
    Assert(ClassList.ObjectListController is TBoldClassListController);
    TBoldClassListController(ClassList.ObjectListController).ReceiveClassEvent(TBoldObject(Originator), OriginalEvent);
    SendExtendedEvent(originalEvent, [originator]);
  end;
end;

function TBoldSystem.CanCreateObject(ClassTypeInfo: TBoldClassTypeInfo): boolean;
begin
  result := SendQuery(bqMayCreateObject, [ClassTypeInfo], nil);
end;

class function TBoldAbstractOldValueHandler.NewValueInValueSpace(BoldMember: TBoldMember; ValueSpace: IBoldValueSpace): IBoldValue;
var
  ObjectContents: IBoldObjectContents;
  MemberId: TBoldmemberId;
  G: IBoldGuard;
begin
  G := TBoldGuard.Create(MemberId);
  ObjectContents := ValueSpace.EnsuredObjectContentsByObjectId[BoldMember.OwningObject.BoldObjectLocator.BoldObjectId];
  MemberId := TBoldMemberId.Create(BoldMember.BoldMemberRTInfo.Index);
  if assigned(ObjectContents.ValueByMemberId[MemberId]) then
    result := nil
  else
  begin
    ObjectContents.EnsureMember(MemberId, BoldMember.GetStreamName);
    result := ObjectContents.ValueByMemberId[MemberId];
  end;
end;

function TBoldSystem.CanDeleteObject(anObject: TBoldObject): boolean;
begin
  result := SendQuery(bqMayDeleteObject, [AnObject], nil);
end;

function TBoldSystem.GetBoldType: TBoldElementTypeInfo;
begin
  result := BoldSystemTypeInfo;
end;

function TBoldSystem.GetDisplayName: String;
begin
  result := boldType.ExpressionName;
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

procedure TBoldSystem.SetIsdefault(Value: Boolean);
begin
 if Value then
   G_DefaultBoldSystem := Self
 else if IsDefault then
   G_DefaultBoldSystem := nil;
end;

procedure TBoldSystem.StartTransaction(MinimalMode: TBoldSystemTransactionMode = stmNormal);
begin
  if MinimalMode <= TransactionMode then
  begin
    if not InTransaction then
    begin
      Assert(not RollbackAreaAssigned);
      fRollbackArea := TBoldFreeStandingValueSpace.create;
      fValidValueArea := TBoldFreeStandingValueSpace.create;
      DelayObjectDestruction;
    end;
    inc(fTransactionNesting);
  end;
end;

procedure TBoldSystem.CommitTransaction(MinimalMode: TBoldSystemTransactionMode = stmNormal);

  function OrdernoDiffers(Value: IBoldValue; member: TBoldMember):Boolean;
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
    { TODO : Lots of finishing }
    G := TBoldGuard.Create(ObjectIds);
    ObjectIds := TBoldObjectIdList.Create;
    FRollBackArea.AllObjectIds(ObjectIDs, false);
    for O := 0 to ObjectIds.Count - 1 do
    begin
      ObjectContents := FRollBackArea.GetFSObjectContentsByObjectId(ObjectIds[O]);
      BoldObject := Locators.GetObjectByID(ObjectIds[O]);
      assert(assigned(BoldObject)); // DelayDestruction should ensure that
      RegardAsExisting := (((BoldObject.BoldPersistenceState = bvpsCurrent) and
        (BoldObject.BoldExistenceState = besExisting)) or
         ((BoldObject.BoldPersistenceState = bvpsModified) and
        (BoldObject.BoldExistenceState = besDeleted)))
         and
        (ObjectContents.BoldExistenceState = besNotCreated);  // was transacted at fetch
      Assert(not RegardAsExisting);  // Remove when tested enough
      UndoHandler.HandleObject(ObjectContents, RegardAsExisting);
      for M := 0 to ObjectContents.MemberCount - 1 do
      begin
        Value :=  ObjectContents.ValueByIndex[m];
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
      if (BoldObject.BoldExistenceState = besDeleted)  then // deleted object, save all remaining values
      begin
        for M := 0 to BoldObject.BoldMemberCount - 1 do
          if BoldObject.BoldMemberAssigned[M] and BoldObject.BoldMembers[M].StoreInUndo  then
          begin
            Value := BoldObject.BoldMembers[M].AsIBoldValue[bdepContents];  // Store as contents, UnDO only used in other direction
            if BoldObject.BoldMembers[M].BoldPersistenceState <> bvpsInvalid then
              UndoHandler.HandleMember(ObjectContents, M, Value);
            Value := nil;
          end;
      end;
    end;
  end;

begin
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
        if TBoldUndoHandler(UndoHandler).UndoState=busNormal then
          HandleOldValues;
        FreeAndNil(fRollbackArea);
        FreeAndNil(fValidValueArea);
        dec(fTransactionNesting);
        AllowObjectDestruction;
      end
      else
        BoldRaiseLastFailure(self, 'CommitTransaction',  sCommitNotAllowed); // do not localize
    end
    else
      dec(fTransactionNesting);
  end;
end;

procedure TBoldSystem.RollbackTransaction(MinimalMode: TBoldSystemTransactionMode = stmNormal);
var
  aRollbackArea: TBoldFreestandingValueSpace;
  aValidValueArea: TBoldFreestandingValueSpace;
begin
  if MinimalMode <= TransactionMode then
  begin
    if not InTransaction then
      raise EBold.CreateFmt(sUnmatchedRollback, [classname]);

    if fTransactionNesting = 1 then
    begin
      aRollbackArea := fRollbackArea;
      fRollbackArea := nil; // so rollback doesn't write to rollback area itself
      aValidValueArea := fValidValueArea;
      fValidValueArea := nil;
      AsIBoldvalueSpace[bdepContents].ApplyValueSpace(aRollbackArea, false);
      aRollbackArea.Free;
      aValidValueArea.Free;
      AllowObjectDestruction;
      fTransactionList.Clear;
      fTransactionRollbackOnly := false;
      SendEvent(beRolledBack);
    end
    else
      fTransactionRollbackOnly := true;
    dec(fTransactionNesting);
  end;
end;

function TBoldSystem.CanCommit: Boolean;
var
  g: IBoldGuard;
  Traverser: TBoldDomainElementCollectionTraverser;
begin
  g := TBoldGuard.Create(Traverser);
  try
    result := SendQuery(bqMayCommit, [], nil);
    Traverser := fTransactionList.CreateTraverser;
    while result and not Traverser.EndOfList do
    begin
      result := Traverser.item.CanCommit;
      Traverser.Next;
    end;
  except
    result := false;
  end;
end;

class procedure TBoldAbstractOldValueHandler.CopyMemberToValueSpace(BoldMember: TBoldMember; ValueSpace: IBoldValueSpace);
var
  Value: IBoldValue;
  BoldObject: TBoldObject;
begin
  Assert(assigned(valuespace));

  BoldObject := BoldMember.OwningObject;
  Assert(Assigned(BoldObject));
  CopyObjectToValueSpace(BoldObject, ValueSpace);
  Value := NewValueInValueSpace(BoldMember, ValueSpace);
  if Assigned(Value) then
    Value.AssignContent(BoldMember.AsIBoldValue[bdepContents]);
end;

class procedure TBoldAbstractOldValueHandler.CopyObjectToValueSpace(BoldObject: TBoldObject; ValueSpace: IBoldValueSpace);
var
  anObjectContents: IBoldObjectContents;
  anObjectId: TBoldObjectId;
begin
  assert(assigned(ValueSpace));

  anObjectId := BoldObject.BoldObjectLocator.BoldObjectID;
  anObjectContents := ValueSpace.ObjectContentsByObjectId[anObjectID];
  if not assigned(anObjectContents) then
  begin
    ValueSpace.EnsureObjectContents(anObjectID);
    anObjectContents := ValueSpace.ObjectContentsByObjectId[anObjectId];
    anObjectContents.BoldExistenceState := BoldObject.BoldExistenceState;
    anObjectContents.BoldPersistenceState := BoldObject.BoldPersistenceState;
  end;
end;

procedure TBoldSystem.CopyMemberToRollBackBuffer(BoldMember: TBoldMember);
var
  Value: IBoldValue;
  FSObjectContents: TBoldFreeStandingObjectContents;
  MemberIndex: integer;
begin
  if RollBackAreaAssigned then
  begin
    MemberIndex := BoldMember.BoldMemberRTInfo.index;
    FSObjectContents := EnsureObjectInFsValueSpace(BoldMember.OwningObject, fRollbackArea);
    Value := FSObjectContents.ValueByIndex[BoldMember.BoldMemberRTInfo.Index];
    if Assigned(Value) then
    begin
      if (Value.BoldPersistenceState = bvpsInvalid) then
      begin
        FSObjectContents := EnsureObjectInFsValueSpace(BoldMember.OwningObject, fValidValueArea);
        if not Assigned(FSObjectContents.ValueByIndex[MemberIndex]) then
        begin
          FSObjectContents.EnsureMemberByIndex(MemberIndex, BoldMember.GetStreamName);
          FSObjectContents.ValueByIndex[MemberIndex].AssignContent(BoldMember.AsIBoldValue[bdepContents]);
        end;
      end;
    end
    else
    begin
      Assert (not(
              (BoldMember is TBOldObjectReference) and
              (BoldMember.BoldPersistenceState = bvpsInvalid) and
              TBOldObjectReference(BoldMember).HasOldValues and
              ((TBOldObjectReference(BoldMember).fObjectReferenceController as TBoldDirectSingleLinkController).GetLocator = nil)
              ));
      FSObjectContents.EnsureMemberByIndex(MemberIndex, BoldMember.GetStreamName);
      FSObjectContents.ValueByIndex[MemberIndex].AssignContent(BoldMember.AsIBoldValue[bdepContents]);
    end;
  end;
end;

procedure TBoldSystem.CopyObjectToRollBackBuffer(BoldObject: TBoldObject);
begin
  if RollBackAreaAssigned then
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
    result := TBoldSystem_Proxy.create(self, Mode).GetInterface(IID, obj);
    if not result then
      raise EBoldInternal.CreateFmt('ProxyClass for %s did not implement IBoldValueSpace', [ClassName]);
  end
  else
    result := inherited ProxyInterface(IID, Mode, Obj);
end;

function TBoldSystem.InTransaction: boolean;
begin
  Result := fTransactionNesting <> 0;
end;

function TBoldSystem.RollBackAreaAssigned: boolean;
begin
  Result := Assigned(fRollBackArea);
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

function TBoldSystem.AssertLinkIntegrity: Boolean;
var
  Traverser: TBoldLocatorListTraverser;
  i: integer;
  bo: TBoldObject;
  G: IBoldGuard;
begin
  G := TBoldGuard.Create(Traverser);
  Traverser := Locators.CreateTraverser;
  while not Traverser.EndOfList do
  begin
    if assigned(Traverser.Locator.BoldObject) then
    begin
      bo := Traverser.Locator.BoldObject;
      for i := 0 to bo.BoldMemberCount - 1 do
        if bo.BoldMemberAssigned[i] and (bo.BoldMembers[i].BoldPersistenceState <> bvpsInvalid)
          and (bo.BoldMembers[i].GetController <> nil) then
          bo.BoldMembers[i].GetController.AssertIntegrity;
    end;
    Traverser.Next;
  end;
  Result := True;
end;

{ TBoldObject }
function TBoldObject.GetDeriveMethodForMember(Member: TBoldMember): TBoldDeriveAndResubscribe;
begin
  result := nil;
  if Member.BoldmemberRTInfo.IsDerived and
    (Member.BoldMemberRTInfo.DeriveExpression <> '') then
       result := Member.CalculateDerivedMemberWithExpression;
end;

function TBoldObject.GetReverseDeriveMethodForMember(Member: TBoldMember): TBoldReverseDerive;
begin
  result := nil;
end;

function TBoldObject.GetBoldMembers(index: Integer): TBoldMember;
begin
  if (index < 0) or (index >= BoldMemberCount) then
    raise EBold.CreateFmt(sIndexOutOfRange, [ClassName, 'GetBoldMembers', Index, BoldMemberCount]); // do not localize
  Result := FDynamicData^[index];
  if not assigned(result) then
  begin
    if (BoldObjectLocator.BoldObjectID.TimeStamp <> BOLDMAXTIMESTAMP) and
      BoldClassTypeInfo.AllMembers[index].IsNonVersionedInVersionedClass then
      result := AtTime(BOLDMAXTIMESTAMP).BoldMembers[index]
    else
    begin
      result := CreateMemberByIndex(index);
      fDynamicData^[index] := result;
      InitializeMember(result, BoldClassTypeInfo.AllMembers[index] {MemberRTInfo}, GetElementFlag(befObjectWasCreatedNew));
    end;
  end;
end;

function TBoldObject.GetBoldMemberAssigned(Index: integer): Boolean;
begin
  if (index < 0) or (index >= BoldMemberCount) then
    raise EBold.CreateFmt(sIndexOutOfRange, [ClassName, 'GetBoldMemberAssigned', Index, BoldMemberCount]); // do not localize
  result := assigned(fDynamicData^[Index]);
end;

function TBoldObject.GetBoldObjectIsDeleted: Boolean;
begin
  result := BoldExistenceState = besDeleted;
end;

function TBoldObject.GetBoldObjectIsNew: Boolean;
begin
  // newly created and yet not stored obejcts
  result := (BoldExistenceState = besExisting) and (BoldPersistenceState = bvpsModified);
  // transient objects
  result := result or (BoldPersistenceState = bvpsTransient);
end;

function TBoldObject.GetBoldMemberCount: Integer;
begin
  Result := BoldClassTypeInfo.AllMembersCount;
end;

function TBoldObject.GetStringRepresentation(Representation: TBoldRepresentation): string;
begin
  if BoldClassTypeInfo.defaultstringrepresentation <> '' then
    result := EvaluateExpressionAsString(BoldClassTypeInfo.defaultstringrepresentation, brDefault)
  else
  begin
    if (Representation < BoldMemberCount) and
       (BoldMembers[Representation] is TBoldAttribute) then
        Result := BoldMembers[Representation].AsString
    else
      Result := Format('%s:%s', [BoldObjectLocator.AsString, BoldClassTypeInfo.ModelName]); // do not localize
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
  Assert(OwningElement is TBoldSystem);
  Result := TBoldSystem(OwningElement);
end;

function TBoldObject.GetAsIBoldObjectContents(Mode: TBoldDomainElementProxyMode): IBoldObjectContents;
begin
  ProxyInterface(IBoldObjectContents, Mode, result);
end;


function TBoldObject.CreateMemberByIndex(Index: integer): TBoldMember;
var
  MemberRTInfo: TBoldMemberRTInfo;
begin
  MemberRTInfo := BoldClassTypeInfo.AllMembers[Index];
  result := TBoldMemberClass(MemberRTInfo.MemberClass).InternalCreate(Self, MemberRTInfo,
    GetElementFlag(befObjectWasCreatedNew) and not MemberRTInfo.IsDerived);
end;

procedure TBoldObject.InitializeObject(System: TBoldSystem; ClassTypeInfo: TBoldClassTypeInfo; Locator: TBoldObjectLocator; Persistent: Boolean);
var
  DynamicDataSize: integer;
begin
  if ClassTypeInfo.IsAbstract then
    raise EBold.CreateFmt(sCannotInstansiateAbstractClass, [ClassTypeInfo.ExpressionName]);
  if ClassTypeInfo.IsImported then
    raise EBold.CreateFmt(sCannotInstansiateImportedClass, [ClassTypeInfo.ExpressionName]);
  fBoldClassTypeInfo := ClassTypeInfo;
  SetElementFlag(befObjectWasCreatedNew, not Assigned(Locator));
  SetElementFlag(befPersistent, System.BoldPersistent and ClassTypeInfo.Persistent and Persistent);
  SetElementFlag(befStoresTimeStamp, BoldClassTypeInfo.OptimisticLocking = bolmTimeStamp);

  DynamicDataSize := GetDynamicDataSize;
  if DynamicDataSize > 0 then
  begin
    fDynamicData := BoldMemoryManager_.AllocateMemory(DynamicDataSize);
    FillChar(fDynamicData^, DynamicDataSize, 0);
  end else
    fDynamicData := nil;

  if BoldStoresTimeStamp then
    fDynamicData^[BoldMemberCount] := Pointer(-1);        // marco added ^ x3


  if Assigned(Locator) then
  begin
    FBoldObjectLocator := Locator;
    BoldObjectLocator.FBoldObject := Self;
    BoldObjectLocator.EmbeddedSingleLinksToObject;
  end
  else
  begin
    FBoldObjectLocator := TBoldObjectLocator.CreateWithClassID(System, ClassTypeInfo.TopSortedIndex, True);
    BoldObjectLocator.FBoldObject := Self;
  end;
  if ClassTypeInfo.ToBeRemoved then
    ToBeRemovedClassAccessed;
end;

procedure TBoldObject.InitializeMember(Member: TBoldMember; MemberRTInfo: TBoldMemberRTInfo; IsNewObject: Boolean);
begin
  if IsNewobject then
  begin {New object}
    if Member.BoldPersistent then
      Member.InitializeStateToModified
    else if Member.Derived then
      Member.InitializeStateToInvalid
    else
      Member.InitializeStateToTransient;
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
      Member.InitializeStateToTransient;
  end;

  if IsNewObject and not memberRTInfo.IsDerived then
    Member.DoSetInitialValue;

  if memberRTInfo.IsDerived and (MemberRTInfo.DeriveExpression <> '') then
    Member.Deriver.OnDeriveAndSubscribe := Member.CalculateDerivedMemberWithExpression;

  if MemberRTInfo.ToBeRemoved and not IsNewObject then
    ToBeRemovedMemberAccessed(MemberRTInfo);
end;

constructor TBoldObject.InternalCreateWithClassAndLocator(ClassTypeInfo: TBoldClassTypeInfo; Locator: TBoldObjectLocator);
begin
  SetInternalState(BoldExistenceStateMask, BoldESShift, Integer(besExisting));
  SetInternalState(BoldPersistenceStateMask, BoldPSShift, Integer(bvpsInvalid));
  Assert(Assigned(Locator.BoldSystem));
  inherited Create(Locator.BoldSystem);
  InitializeObject(Locator.BoldSystem, ClassTypeInfo, Locator, True);
  EndReCreate;
end;

constructor TBoldObject.Create(AOwningElement: TBoldDomainElement; Persistent: Boolean = True);
var
  System: TBoldSystem;
  aClass: TBoldClassTypeInfo;
begin
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
      raise EBold.CreateFmt(sGeneratedCodeNotUsed + BOLDCRLF +
                            sGeneratedCode_HowToFix,
                            [ClassName])
    else
      raise EBold.CreateFmt(sNoClassInformation, [ClassName, classname])
  end;
  if aClass.IsLinkClass then
    raise EBold.CreateFmt(sCannotCreateAssociationClass, [classname]);
  InternalCreateNewWithClassAndSystem(aClass, System, Persistent);
end;

procedure TBoldObject.ReceiveEventFromOwned(originator: TObject; originalEvent: TBoldEvent);
begin
  if (BoldExistenceState = besExisting) and
     (originalEvent in beValueEvents) then
    SendEvent(beMemberChanged);

  if (originalEvent in beValueEvents) and
    assigned(BoldSystem.NewModifiedList) and
    (not (Originator is TBoldMember) or (not TBoldMember(Originator).Derived)) then
    BoldSystem.NewModifiedList.Add(self);
end;

destructor TBoldObject.Destroy;
var
  I: Integer;
  aLocator: TBoldObjectLocator;
begin
  if BoldExistenceState = besNotCreated then
  begin
    // OwningElement might not be set if constructor failed
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

  if Assigned(fDynamicData) then
  begin
    for i := 0 to BoldMemberCount - 1 do
      if GetBoldMemberAssigned(i) then
        BoldMembers[i].Free;
    BoldMemorymanager_.DeAllocateMemory(fDynamicData, GetDynamicDataSize);
  end;

  inherited Destroy;
end;

procedure TBoldObject.DefaultSubscribe(Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent = breReEvaluate);
begin
end;

procedure TBoldObject.UnLinkAll;
var
  M: Integer;
  MemberRTInfo: TBoldMemberRTInfo;
begin
  for M := 0 to BoldClassTypeInfo.AllMembers.Count - 1 do
  begin
    MemberRTInfo := BoldClassTypeInfo.AllMembers[M];
    if MemberRTInfo.IsRole and MemberRTInfo.IsDerived then
    begin
      if GetBoldMemberAssigned(m) then
        BoldMembers[m].Invalidate;
    end
    else if MemberRTInfo.isMultiRole then
    begin
      if ((MemberRTInfo as TBoldRoleRTInfo).RoleType = rtRole) and
        ((BoldMembers[M] as TBoldObjectList).Count <> 0) then
        (BoldMembers[M] as TBoldObjectList).Clear;
    end
    else if MemberRTInfo.isSingleRole then
    begin
      if ((MemberRTInfo as TBoldRoleRTInfo).RoleType = rtRole) and
        assigned((BoldMembers[M] as TBoldObjectReference).Locator) then
        (BoldMembers[M] as TBoldObjectReference).Clear;
    end;
  end;
end;

procedure TBoldObject.MarkObjectDirty;
var
  i: integer;
begin
  if BoldDirty then
    exit;
  for i := 0 to BoldMemberCount - 1 do
    if GetBoldMemberAssigned(i) and
      BoldMembers[i].BoldMemberRTInfo.IsStoredInObject and
      (BoldMembers[i].BoldPersistenceState = bvpsCurrent) then
    begin
      BoldMembers[i].MarkMemberDirty;
      exit;
    end;
  // if we could not find a loaded member, then take the first that is persistent.
  for i := 0 to BoldMemberCount - 1 do
    if BoldMembers[i].BoldMemberRTInfo.IsStoredInObject and
      (BoldMembers[i].BoldPersistenceState = bvpsCurrent) then
    begin
      BoldMembers[i].MarkMemberDirty;
      exit;
    end;
  raise EBold.CreateFmt(sNoPersistentMembers, [classname]);
end;

procedure TBoldObject.ReRead;
begin
  Invalidate;
  BoldSystem.SystemPersistenceHandler.FetchObjectById(BoldObjectLocator.BoldObjectID);
end;

{check than no links exist except for member "index"}
function TBoldObject.CheckLinks(index: Integer): Boolean;
var
  I: Integer;
  MemberRTInfo: TBoldmemberRTInfo;
begin
  Result := True;
  for I := 0 to BoldClassTypeInfo.AllMembers.Count - 1 do
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
      // in a transient system links that do not exist must be empty
      if BoldSystem.BoldPersistent or GetBoldMemberAssigned(i) then
      begin
        if not RoleRTInfo.IsDerived and (RoleRTInfo.RoleType = rtRole) then
        begin
          case RoleRTInfo.DeleteAction of
            daCascade: result := not cascade or (result and CascadeCanDelete(BoldMembers[i]));
            daAllow: ; // do nothing
            daProhibit: result := result and CheckEmpty(BoldMembers[i]);
          end;
        end
        else
        begin
          // linkobjects will always be deleted when the original object is deleted, so we must cascade the question
          if RoleRTInfo.RoleType = rtLinkRole then
             result := result and CascadeCanDelete(BoldMembers[i]) ;
        end;
        if not result then
          break;
      end;
    end;
  end;

  if result then
    result := SendQuery(bqMayDelete, [], nil) and BoldSystem.CanDeleteObject(self)
  else
    SetBoldLastFailureReason(TBoldFailureReason.Create(sObjectHasRelations, self));
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
      TBoldObjectReference(member).BoldObject := nil;
  end;

  procedure DoDelete;
  var
    MemberRTInfo: TBoldMemberRTInfo;
    RoleRTInfo: TBoldRoleRTInfo;
    i: integer;
  begin
    for i := 0 to BoldMemberCount - 1 do
    begin
      MemberRTInfo := BoldClassTypeInfo.AllMembers[i];

      if MemberRTInfo.IsRole then
      begin
        Assert(MemberRTInfo is TBoldRoleRTInfo);
        RoleRTInfo := TBoldRoleRTInfo(MemberRTInfo);
        if not RoleRTInfo.IsDerived and
           (RoleRTInfo.RoleType in [rtRole, rtInnerLinkRole]) then
        begin
          case RoleRTInfo.DeleteAction of
            daCascade: CascadeDelete(BoldMembers[i]);
            daAllow: ClearRelation(BoldMembers[i]);
            daProhibit: ;// do nothing, should always be empty by now
          end;
        end;
      end;
    end;
  end;

begin
  BoldClearLastFailure;
  if not StartDelete then
    BoldRaiseLastFailure(self, 'Delete', sPreconditionNotMet); // do not localize
  try
    BoldSystem.StartTransaction(stmNormal);
    DoDelete;
    EndDelete;
    BoldSystem.CommitTransaction(stmNormal);
  except
    FailDelete;
    BoldSystem.RollbackTransaction(stmNormal);
    raise;
  end;
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
    raise EBold.CreateFmt(sUnknownCompareType, [ClassName, 'IsEqualAs']); // do not localize
  end;
end;

procedure TBoldObject.GetAsList(ResultList: TBoldIndirectElement);
var
  NewList: TBoldList;
begin
  Assert(BoldType.SystemTypeInfo is TBoldSystemTypeInfo);
  NewList := TBoldObjectList.CreateWithTypeInfo(TBoldSystemTypeInfo(BoldType.SystemTypeInfo).ListTypeInfoByElement[BoldType]);
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
    CompleteCreate;
    SendEvent(beCompleteModify);
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
  sendevent(beObjectFetched);
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
    StateError('StartDelete'); // do not localize

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
    bvpsTransient: ; // do nothing
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

{
function TBoldObject.GetEffectiveInvalid: Boolean;
var
  i: Integer;
begin
  if GetElementFlag(befObjectWasCreatedNew) then
  begin
    result := false;
    exit;
  end;

  for i := 0 to BoldMemberCount - 1 do
  begin
    if (not BoldClassTypeInfo.AllMembers[i].DelayedFetch) and
       (BoldClassTypeInfo.AllMembers[i].Persistent) and
       (not GetBoldMemberAssigned(i) or (BoldMembers[i].BoldPersistenceState = bvpsInvalid)) then
    begin
      result := True;
      exit;
    end;
  end;
  result := False;
end;
}

procedure TBoldObject.MemberBecomingModified(BoldMember: TBoldMember);
begin
  MemberModified := true;
  MemberModifiedKnown := true;
  BoldSystem.MarkObjectDirty(self);
end;

procedure TBoldObject.MemberBecomingClean(BoldMember: TBoldMember);
begin
  if MemberModified then
  begin
    MemberModifiedKnown := False;
    BoldSystem.MarkObjectPossiblyCleaner(self);
  end;
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
      if GetBoldMemberAssigned(i) then
        MemberModified := BoldMembers[i].BoldDirty
      else
        MemberModified := GetElementFlag(befObjectWasCreatedNew) and
                          BoldClassTypeInfo.AllMembers[i].IsStoredInObject and
                          Assigned(BoldSystem.PersistenceController);
      if MemberModified then
        Exit;
    end;
end;

procedure TBoldObject.CompleteCreate;
begin
  // intentionally left blank
end;

procedure TBoldObject.CompleteUpdate;
begin
  // intentionally left blank
end;


function TBoldObject.MayUpdate: Boolean;
begin
  result := True;
end;

procedure TBoldObject.PrepareUpdate;
begin
  // intentionally left blank
end;

function TBoldObject.MayDelete: Boolean;
begin
  result := True;
end;

procedure TBoldObject.PrepareDelete;
begin
  // intentionally left blank
end;

procedure TBoldObject.DoStartDelete;
begin
  PrepareDelete;
  SendEvent(bePrepareDelete);
end;

procedure TBoldObject.DoStartUpdate;
begin
  PrepareUpdate;
  PrepareUpdateMembers;
end;

function TBoldObject.GetIsModified: Boolean;
begin
  result := BoldPersistenceState = bvpsModified;
end;

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
    // we should add a guard here that checks BoldMemberAssigned.
    // a nonassigned member can reasonably not prevent an update
    // do not add this until enough testing can be performed prior to release
    if MemberProhibitsUpdate(BoldMembers[i]) then
    begin
      result := False;
      exit;
    end;
  end;
  result := True;
end;

procedure TBoldObject.EndUpdateMembers(Translationlist: TBoldIdTranslationlist);
var
  i: Integer;
  Member: TBoldMember;
begin
  for i := 0 to BoldMemberCount - 1 do
    if GetBoldMemberAssigned(i) then
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

  procedure IfNecessaryEndFetchMember(Member: TBoldMember);
  begin
    if assigned(Member) and (Member.BoldPersistenceState = bvpsInvalid) then
      Member.BoldPersistenceState := bvpsCurrent;
  end;

var
  i: Integer;
  aMember: TBoldMember;
begin
  if assigned(MemberIdList) then
    for i := 0 to MemberIdList.Count - 1 do
      IfNecessaryEndFetchMember(BoldMembers[MemberIdList[i].MemberIndex])
  else
    for i := 0 to BoldMemberCount - 1 do
    begin
      if not BoldClassTypeInfo.AllMembers[i].DelayedFetch then
      begin
        aMember := BoldMembers[i];
        if aMember.BoldPersistent then
          IfNecessaryEndFetchMember(aMember);
      end;
    end;
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
  if BoldStoresTimeStamp then
    result := TBoldTimeStampType(fDynamicData^[BoldMemberCount])
  else
    result := -1;
end;

procedure TBoldObject.SetTimeStamp(NewValue: TBoldTimeStampType);
begin
  if BoldStoresTimeStamp then
    fDynamicData^[BoldMemberCount] := Pointer(NewValue);
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
  inherited StateError(format('%s (ExistenceState: %s PersistenceState: %s)', // do not localize
    [s, GetEnumName(TypeInfo(TBoldExistenceState), Ord(BoldExistenceState)),
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
  if not aSystem.CanCreateObject(ClassTypeInfo) then
    BoldRaiseLastFailure(aSystem, 'InternalCreateNewWithClassAndSystem', // do not localize
      format(sCannotCreateOfType, [ClassTypeInfo.ExpressionName]));
  aSystem.StartTransaction(stmNormal);
  try
    SetInternalState(BoldExistenceStateMask, BoldESShift, Integer(besNotCreated));
    SetInternalState(BoldPersistenceStateMask, BoldPSShift, Integer(bvpsCurrent));
    inherited Create(aSystem);
    InitializeObject(aSystem, ClassTypeInfo, nil, Persistent);
    EndCreate;
    aSystem.CommitTransaction(stmNormal);
  except
    aSystem.RollbackTransaction(stmNormal);
    raise;
  end;
end;

function TBoldObject.GetBoldType: TBoldElementTypeInfo;
begin
  result := fBoldClassTypeInfo;
end;

function TBoldObject.CanUpdate: Boolean;
begin
  result := MayUpdate and SendQuery(bqMayUpdate, [], nil);
end;

function TBoldObject.GetIsReadOnly: Boolean;
begin
  result := GetElementFlag(befObjectReadOnly);
end;

function TBoldObject.GetDisplayName: String;
begin
  result := ClassName;
end;

function TBoldObject.GetEvaluator: TBoldEvaluator;
begin
  result := BoldSystem.Evaluator;
end;

function TBoldObject.CanUnload: Boolean;
begin
  result := not BoldDirty;
  if not result then
    SetBoldLastFailureReason(TBoldFailureReason.create(sObjectIsDirty, self));
end;

procedure TBoldObject.Invalidate;
var
  i: Integer;
begin
  if BoldDirty then
    raise EBold.CreateFmt(sCannotInvalidateDirtyObject, [classname]);
  for i := 0 to BoldMemberCount - 1 do
    if BoldMembers[i].BoldPersistent then
      BoldMembers[i].Invalidate;
end;

procedure TBoldObject.Discard;
var
  i: Integer;
begin
  if BoldSystem.fTransactionNesting <> 0 then
    raise EBold.Create(sCannotDiscardInTransaction);

  if not BoldObjectLocator.BoldObjectID.IsStorable then
    UnlinkAll;

  for i := 0 to BoldMemberCount - 1 do
    if BoldMemberAssigned[i] then
      BoldMembers[i].InternalDiscard;

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

function TBoldObject.GetBoldExistenceState: TBoldExistenceState;
begin
  result := TBoldExistenceState(GetInternalState(BoldExistenceStateMask, BoldESShift));
end;

function TBoldObject.GetBoldPersistenceState: TBoldValuePersistenceState;
begin
  result := TBoldValuePersistenceState(GetInternalState(BoldPersistenceStateMask, BoldPSShift));
end;

function TBoldObject.ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean;
begin
  if IsEqualGuid(IID, IBoldObjectContents) then
  begin
    result := TBoldObject_Proxy.create(self, Mode).GetInterface(IID, obj);
    if not result then
      raise EBoldInternal.CreateFmt(sInterfaceNotImplemented, [ClassName]);
  end
  else
    result := inherited ProxyInterface(IID, Mode, Obj);
end;

function TBoldObject.AtTime(Time: TBoldTimestampType): TBoldObject;
begin
  result := BoldObjectLocator.AtTime(time).EnsuredBoldObject;
end;

function TBoldObject.GetBoldTime: TBoldTimestampType;
begin
  result := BoldObjectLocator.BoldObjectID.TimeStamp;
end;

procedure TBoldObject.PrepareUpdateMembers;
var
  i: integer;
begin
  for i := 0 to BoldMemberCount - 1 do
    if GetBoldMemberAssigned(i) and BoldMembers[i].BoldDirty then
      BoldMembers[i].PrepareUpdate;
end;

function TBoldObject.GetBoldObjectExists: Boolean;
begin
  result := BoldExistenceState = besExisting;
end;

function TBoldObject.ValidateMember(const ObjectDelphiName, MemberDelphiName: String; GeneratedMemberIndex: integer; MemberClass: TBoldMemberClass): Boolean;
var
  Member: TBoldMember;
  MemberRTInfo: TBoldMemberRTInfo;
begin
  if not assigned(self) then
    raise EBold.CreateFmt(sValidate_MemberNotAssigned,
                          [ObjectDelphiName, MemberDelphiName]);

  MemberRTInfo := BoldClassTypeInfo.AllMembers.ItemsByDelphiName[MemberDelphiName] as TBoldMemberRTInfo;

  if not assigned(MemberRTInfo) then
    raise EBold.CreateFmt(sValidate_NoSuchMember,
                          [ObjectDelphiName, MemberDelphiName]);

  if (GeneratedMemberIndex <> -1) and (MemberRTInfo.index <> GeneratedMemberIndex) then
    raise EBold.CreateFmt(sValidate_MemberIndexOutOfSynch, [
      ObjectDelphiName, MemberDelphiName, GeneratedMemberIndex, MemberRTInfo.index]);

  Member := BoldMembers[MemberRTInfo.index];

  if not assigned(Member) then
    raise EBoldInternal.CreateFmt('Internal Error: Member %s.%s is not assigned',
                                  [ObjectDelphiName, MemberDelphiName]);

  if not (Member is MemberClass) then
    raise EBold.CreateFmt(sValidate_InvalidMemberType,
                          [ObjectDelphiName, MemberDelphiName, MemberClass.ClassName, Member.ClassName]);

  result := true;
end;

function TBoldObject.GetDynamicDataSize: Integer;
begin
  Result := BoldMemberCount * SizeOf(Pointer);

  if BoldStoresTimeStamp then
    Result := Result + SizeOf(TBoldTimeStampType);
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

constructor TBoldMember.InternalCreate(OwningObject: TBoldObject; MemberRTInfo: TBoldMemberRTInfo; SetInitialValue: Boolean);
begin
  fBoldMetaType := MemberRTInfo;
  OwnedByObject := True;

  if assigned(MemberRtInfo) then
  begin
    InitializeMember(OwningObject, MemberRTInfo.BoldType);
  end
  else
    InitializeMember(OwningObject, nil);
end;

destructor TBoldMember.Destroy;
begin
  PrepareToDestroy;
  if HasDeriver then
    G_ExternalDerivers.ReferencedObjects[self] := nil;
  inherited Destroy;
end;

function TBoldMember.GetOwningObject: TBoldObject;
begin
  if OwnedByObject then
  begin
    Assert(OwningElement is TBoldObject);
    Result := TBoldObject(OwningElement)
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
  else if not assigned(OwningElement) then
    result := nil
  else
  begin
    Assert(OwningElement is TBoldSystem);
    result := TBoldSystem(OwningElement)
  end;
end;

procedure TBoldMember.CalculateDerivedMemberWithExpression(DerivedObject: TObject; Subscriber: TBoldSubscriber);
var
  ie: TBoldIndirectElement;
  g: IBoldGuard;
  s: string;
begin
  g := TBoldGuard.Create(ie);
  ie := TBoldIndirectElement.Create;
  try
    OwningObject.evaluateAndSubscribeToExpression(BoldMemberRTInfo.DeriveExpression, subscriber, ie, false);
    if Assigned(ie.Value) and (ie.value is TBoldmember) then
      TBoldMember(ie.value).EnsureContentsCurrent;
    AssignContentValueFromElement(ie.Value);
  except
    on e: Exception do
    begin
      s := format(sFailedToDerive + BOLDCRLF +
                  sOCLExpressionError, [DisplayName, BoldType.AsString, BoldMemberRTInfo.DeriveExpression, e.Message]);
      if assigned(ie.value) and not ie.value.BoldType.conformsto(boldtype) then
        s := format(s + BOLDCRLF+
                    sPossiblyBadConformance, [ie.value.BoldType.AsString, boldtype.AsString]);
      raise EBold.Create(s);
    end;
  end;
end;

function TBoldMember.ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean;
begin
  if IsEqualGuid(IID, IBoldValue) or IsEqualGuid(IID, IBoldStreamable) then
    Result := RetrieveProxyInterface(IID, Mode, obj, 'IBoldValue/IBoldStreamable') // do not localize
  else
    result := inherited ProxyInterface(IID, Mode, Obj);
end;

function TBoldMember.ObserverMayModify(Observer: TObject): Boolean;
begin
  Result := CanModify and
            ((ModifiedValueHolder = nil) or
             (ModifiedValueHolder = Observer));
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
    StateError('StartModify'); // do not localize
  result := CanModify;
  if result and assigned(BoldSystem) and assigned(OwningObject) and
     assigned(BoldSystem.PessimisticLockHandler) and
     not BoldMemberRTInfo.IsDerived then
    result := BoldSystem.PessimisticLockHandler.LockElement(self);
  if result then
  begin
    if assigned(OwningObject) and not BoldSystem.InTransaction and StoreInUndo then  // Object always has system
      BoldSystem.UndoHandler.HandleMember(OwningObject.AsIBoldObjectContents[bdepContents], BoldMemberRTInfo.Index, AsIBoldValue[bdepContents]);
    DoStartModify;
  end;
end;

procedure TBoldMember.EndModify;
begin
  if (BoldPersistenceState in [bvpsCurrent, bvpsInvalid]) and
     assigned(BoldMemberRTInfo) and
     (BoldMemberRTInfo.IsStoredInObject or
     // this test should be moved to a DataMappingModel, and not the persistenceController
     // it is needed for FileMapper since it has to store multilinks
      (BoldMemberRTInfo.IsMultiRole and
       (assigned(BoldSystem.PersistenceController) and
        BoldSystem.PersistenceController.MultilinksAreStoredInObject))) then
  begin
    BoldPersistenceState := bvpsModified;
  end;
  if Derived and not (DeriverState in bdsIsDeriving) then
    Deriver.ReverseDerive;
  CompleteModify;
  SendEvent(beCompleteModify);
  if assigned(OwningObject) then
  begin
    BoldSystem.AddToTransaction(OwningObject);
    BoldSystem.AddToTransaction(self);
  end;
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
  // intentionally left blank
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
  // intentionally left blank
end;

procedure TBoldMember.CompleteModify;
begin
  // intentionally left blank
end;

procedure TBoldMember.CompleteUpdate;
begin
  // intentionally left blank
end;

procedure TBoldMember.PrepareUpdate;
begin
  // intentionally left blank
end;

procedure TBoldMember.DoStartModify;
begin
  if assigned(OwningObject) then
    OwningObject.BoldSystem.fOldValueHandler.MemberValuePreChange(self);

  PrepareModify;
  SendEvent(bePrepareModify);
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

  result := not IsReadOnly;
  if not result then
  begin
    SetBoldLastFailureReason(TBoldFailureReason.Create(sMemberIsreadOnly, self));
    exit;
  end;

  if Derived then
     Result := (DeriverState in bdsIsDeriving) or Deriver.CanReverseDerive;

  if not result then
  begin
    SetBoldLastFailureReason(TBoldFailureReason.Create(sMemberIsreadOnlyDerived, self));
    exit;
  end;

  result := not assigned(OwningObject) or (OwningObject.BoldTime = BOLDMAXTIMESTAMP);
  if not result then
  begin
    SetBoldLastFailureReason(TBoldFailureReason.Create(sMemberIsHistory, self));
    exit;
  end;

  result := MayModify and SendQuery(bqMayModify, [], nil);
end;

function TBoldMember.CanUpdate: Boolean;
begin
  result := InternalMayUpdate and MayUpdate and SendQuery(bqMayUpdate, [], nil);
end;

procedure TBoldMember.EnsureContentsCurrent;
begin
 inherited;
 if (BoldPersistenceState = bvpsInvalid) and
    not GetElementFlag(befEnsuringCurrent) then
 begin
   SetElementFlag(befEnsuringCurrent, true);
   try
     if Derived then
     begin
       if DeriverState <> bdsCurrent then
         Deriver.EnsureCurrent;
       if not (DeriverState in bdsIsDeriving) then
         BoldPersistenceState := bvpsTransient;
     end
     else
       MakeDbCurrent;
   finally
     SetElementFlag(befEnsuringCurrent, false);
   end;
 end;
 if not GetElementFlag(befTouched) then
 begin
   SetElementFlag(befTouched, true);
   if assigned(owningObject) then
     OwningObject.SetElementFlag(befTouched, true);
 end;
end;

procedure TBoldMember.Invalidate;
begin
  if not (BoldPersistenceState in [bvpsCurrent, bvpsInvalid, bvpsTransient]) then
    StateError('Invalidate'); // do not localize

  if assigned(BoldMemberRTInfo) and
    not BoldMemberRTInfo.IsDerived and
   (BoldPersistenceState = bvpsTransient) then
    raise EBold.CreateFmt(sCannotInvalidateTransient, [classname, displayname]);

  if BoldPersistenceState in [bvpsCurrent, bvpsTransient] then
  begin
    BoldPersistenceState := bvpsInvalid;
    if HasDeriver then
      Deriver.MarkOutOfdate;
    FreeContent;
    SendEvent(beValueInvalid);
  end;
end;

function TBoldMember.CanRead(Subscriber: TBoldSubscriber): Boolean;
begin
  result := SendQuery(bqMayRead, [], Subscriber);
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
  result := TBoldValuePersistenceState(GetInternalState(BoldPersistenceStateMask, BoldPSShift));
end;

procedure TBoldMember.SetBoldPersistenceState(
  Value: TBoldValuePersistenceState);
var
  OldPState: TBoldValuePersistenceState;
begin
  OldPState := BoldPersistenceState;
  if Value <> OldPState then
  begin
    if Assigned(OwningObject) then
      OwningObject.BoldSystem.fOldValueHandler.MemberPersistenceStatePreChange(self, Value);
    SetInternalState(BoldPersistenceStateMask, BoldPSShift, integer(value));
    if OwnedByObject and BoldMemberRTInfo.IsSingleRole and (not Derived) and (not TBoldObjectReference(self).BoldRoleRTInfo.IsIndirect)and  (Value = bvpsInvalid) then
      TBoldObjectReference(self).HasOldValues := ((TBoldObjectReference(self).fObjectReferenceController as TBoldDirectSingleLinkController).GetLocator <> nil);
    if not derived and assigned(OwningObject) then
    begin
      if Value = bvpsModified then
        OwningObject.MemberBecomingModified(self)
      else if OldPState = bvpsModified then
        OwningObject.MemberBecomingClean(self);
    end;
  end;
end;

procedure TBoldMember.MakeDbCurrent;
begin
end;

function TBoldMember.GetDeriver: TBoldEventPluggedDeriver;
begin
  Assert((G_ExternalDerivers.ReferencedObjects[self] = nil)
    or (G_ExternalDerivers.ReferencedObjects[self] is TBoldMemberDeriver));
  result := TBoldMemberDeriver(G_ExternalDerivers.ReferencedObjects[self]);
  if not Assigned(Result) then
  begin
    Result := TBoldMemberDeriver.Create(Self);
    if assigned(OwningObject) then
    begin
      result.OnDeriveAndSubscribe := OwningObject.GetDeriveMethodForMember(self);
      result.OnReverseDerive := OwningObject.GetReverseDeriveMethodForMember(self);
    end;
    Result.OnNotifyOutOfdate := _NotifyOutOfDate;
    G_ExternalDerivers.ReferencedObjects[Self] := Result;
    SetElementFlag(befHasDeriver, True);
  end;
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
  inherited StateError(format('%s %s(PersistenceState: %s)', // do not localize
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

function TBoldMember.Clone: TBoldMember;
begin
  if assigned(BoldType) then
    Result := TBoldMemberFactory.CreateMemberFromBoldType(BoldType)
  else
    result := TBoldMemberClass(classtype).Create;
  try
    Result.BoldPersistenceState := bvpsTransient;
    Result.Assign(Self);
  except
    Result.Free;
    raise;
  end;
end;

constructor TBoldMember.Create;
begin
  InitializeMember(nil, nil);
end;

function TBoldMember.GetIsPartOfSystem: Boolean;
begin
  result := OwnedByObject or (OwningElement is TBoldSystem);
end;

function TBoldMember.GetIsReadOnly(Flag: TBoldElementFlag): Boolean;
begin
  result := GetElementFlag(Flag) or
    (assigned(BoldMemberRTInfo) and BoldMemberRTInfo.IsStoredInObject and OwningObject.IsReadOnly);
end;

procedure TBoldMember.InitializeMember(AOwningElement: TBoldDomainElement;
  ElementTypeInfo: TBoldElementTypeInfo);
begin
  inherited Create(AOwningElement);

  Assert(OwnedByObject = ( assigned(fBoldMetaType) and (fBoldMetaType is TBoldMemberRtInfo)));
  if OwnedByObject then
  begin
    SetElementFlag(befDerived, BoldMemberRTInfo.IsDerived);
    SetElementFlag(befPersistent, (AOwningElement is TBoldDomainElement) and TBoldDomainElement(AOwningElement).BoldPersistent and BoldMemberRTInfo.Persistent);
    if Derived then
      DeriverState := bdsSubscriptionOutOfDate;
  end
  else
  begin
    SetElementFlag(befPersistent, false);
    if assigned(ElementTypeInfo) then
      fBoldMetaType := ElementTypeInfo
    else
      fBoldMetaType := GetElementTypeInfoForType;
  end;
end;

procedure TBoldMember.InternalDiscard;
begin
  if BoldPersistenceState in [bvpsModified, bvpsTransient] then
  begin
    PreDiscard;
    case BoldPersistenceState of
      bvpsModified: begin
        // if the object is not stored yet, then we can not set the member
        // to invalid, since it can not be fetched...
        if not OwningObject.BoldObjectLocator.BoldObjectID.IsStorable then
          BoldPersistenceState := bvpsCurrent
        else
          BoldPersistenceState := bvpsInvalid;

        FreeContent;
        SendEvent(beValueInvalid);
      end;
      bvpsTransient: if not Derived then
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

procedure TBoldMember.InitializeStateToTransient;
begin
  SetInternalState(BoldPersistenceStateMask, BoldPSShift, Integer(bvpsTransient));
end;

procedure TBoldMember.PreChange;
begin
  if assigned(OwningObject) and not BoldMemberRTInfo.IsDerived then
    OwningObject.BoldSystem.CopyMemberToRollbackBuffer(self);
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
  result := ProxyClass.create(self, Mode).GetInterface(IID, obj);
  if not result then
    raise EBoldInternal.CreateFmt(sProxyClassDidNotImplementInterface, [ProxyClass.ClassName, ClassName, InterfaceName]);
end;

procedure TBoldMember.AssignContentValueFromElement(source: TBoldElement);
begin
  if not assigned(source) then
//    AssignContentValue(nil)
  else if source is TBoldMember then
    AsIBoldValue[bdepContents].AssignContent(TBoldMember(source).AsIBoldValue[bdepContents])
  else
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname, 'AssignContentValuefromElement']); // do not localize
end;

function TBoldMember.GetDeriverState: TBoldDeriverState;
begin
  result := TBoldDeriverState(GetInternalState(BoldDerivationStateMask, BoldDSShift));
end;

procedure TBoldMember.SetDeriverState(value: TBoldDeriverState);
begin
  SetInternalState(BoldDerivationStateMask, BoldDSShift, integer(Value));
end;

function TBoldMember.GetOldValue: IBoldValue;
var
  ObjectContents: IBoldObjectContents;
begin
  result := nil;
  if assigned(OwningObject) and assigned(BoldSystem) then
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
  Result := Assigned(BoldMemberRTinfo) and (not BoldMemberRTInfo.IsDerived) and
    (BoldMemberRTInfo.IsAttribute or
     (TBoldRoleRTInfo(BoldMemberRTInfo).RoleType = rtRole) and(not (TBoldRoleRTInfo(BoldMemberRTInfo).IsMultiRole or TBoldRoleRTInfo(BoldMemberRTInfo).IsIndirect)) or
     (TBoldRoleRTInfo(BoldMemberRTInfo).RoleType = rtInnerLinkRole)
    );
end;

function TBoldMember.IsEqualToValue(Value: IBoldValue): Boolean;
begin{ TODO : Make part of IBoldVAlue }
  raise Ebold.Create(sIsEqualToValueNotImplemented);
end;

function TBoldAttribute.IsEqualToValue(Value: IBoldValue): Boolean;
var
  MemberOfSameType: TBoldMember;
  G: IBoldGuard;
begin{ TODO : Make part of IBoldVAlue }
  // Messy
  G := TBoldGuard.Create(MemberOfSameType);
  if assigned(BoldType) then
    MemberOfSameType := TBoldMemberFactory.CreateMemberFromBoldType(BoldType)
  else
    MemberOfSameType := TBoldMemberClass(classtype).Create;
  MemberOfSameType.AsIBoldValue[bdepContents ].AssignContent(Value);
  MemberOfSameType.BoldPersistenceState := bvpsTransient;
  Result := IsEqual(MemberOfSameType);
end;

function TBoldObjectReference.IsEqualToValue(Value: IBoldValue): Boolean;
var
  IdRef: IBoldObjectIdRef;
  IdRefPair: IBoldObjectIdRefPair;
  ValueId : TBoldObjectId;
begin{ TODO : Make part of IBoldVAlue }
  if Supports(Value, IBoldObjectIdRef,IdRef) then
    ValueId := Idref.id
  else if Supports(Value, IBoldObjectIdRefPair,IdRefPair) then
    ValueId := IdRefPair.Id1
  else
    raise EBoldInternal.Create('Internal error');
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

procedure TBoldAttribute.DefaultSubscribe(Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent = breReEvaluate);
begin
  if mutable then
    AddSmallSubscription(Subscriber, [beValueChanged, beValueInvalid], RequestedEvent);
end;

function TBoldAttribute.GetBoldAttributeRTInfo: TBoldAttributeRTInfo;
begin
  Assert((not Assigned(BoldMemberRTInfo)) or (BoldMemberRTInfo is TBoldAttributeRTInfo));
  Result := TBoldAttributeRTInfo(BoldMemberRTInfo);
end;

function TBoldAttribute.GetIsNull: Boolean;
begin
  EnsureContentsCurrent;
  BoldClearLastFailure;
  if not CanRead(nil) then
    BoldRaiseLastFailure(self, 'GetIsNull', ''); // do not localize
  Result := ContentIsNull;
end;

procedure TBoldAttribute.SetToNull;
begin
  BoldClearLastFailure;
  if not CanSetToNull(nil) then
    BoldRaiseLastFailure(self, 'SetToNull', ''); // do not localize

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

function TBoldAttribute.CanSetToNull(Subscriber: TBoldSubscriber): Boolean;
begin
  result := ((not Assigned(BoldAttributeRTInfo)) or BoldAttributeRTInfo.AllowNull) and
            SendQuery(bqMaySetToNull, [], Subscriber);
  if not result then
    nullfailure;
end;

function TBoldAttribute.ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean;
begin
  if IsEqualGuid(IID, IBoldNullableValue) then
    Result := RetrieveProxyInterface(IID, Mode, obj, 'IBoldNullableValue') // do not localize
  else
    result := inherited ProxyInterface(IID, Mode, Obj);
end;

procedure TBoldAttribute.SubscribeToStringRepresentation(Representation: TBoldRepresentation; Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent = breReEvaluate);
begin
  DefaultSubscribe(Subscriber, RequestedEvent);
end;

procedure TBoldAttribute.SetToNonNull;
begin
  SetElementFlag(befIsNull, False);
end;

procedure TBoldAttribute.EnsureNotNull;
begin
  if IsNull then
    raise EBoldAccessNullValue.CreateFmt(sNullValueAccess, [DisplayName]);
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
  if not IsInvalid then
    SendExtendedEvent(Event, Args);
end;

function TBoldMember.IsInvalid: Boolean;
begin
  result := BoldPersistenceState = bvpsInvalid;
end;

function TBoldMember.GetDisplayName: String;
begin
  if assigned(BoldMemberRTInfo) then
    result := BoldMemberRTInfo.ClassTypeInfo.ExpressionName + '.' + BoldMemberRTInfo.ExpressionName
  else
    result := ClassName;
end;

procedure TBoldAttribute.NullFailure;
var
  CurrentFailureReason: TBoldFailureReason;
begin
  CurrentFailureReason := GetBoldLastFailureReason;
  if not assigned(CurrentFailureReason) then
    SetBoldLastFailureReason(TBoldFailureReason.Create(sNullValueNotAllowed, self));
end;

procedure TBoldAttribute.FormatFailure(const value, ExpectedDataType: String);
begin
  SetBoldLastFailureReason(TBoldFailureReason.CreateFmt(sFailure_Invalid, [value, expectedDataType], self));
end;

function TBoldAttribute.GetContentIsNull: Boolean;
begin
  Result := GetElementFlag(befIsNull);
end;

procedure TBoldAttribute.SetContentToNull;
begin
  if not ContentIsNull then
  begin
    SetElementFlag(befIsNull, True);
    Changed(beValueChanged, []);
  end;
end;

procedure TBoldAttribute.DoSetInitialValue;
var
  InitialValue: string;
begin
  if Assigned(BoldAttributeRTInfo) and BoldAttributeRtInfo.HasInitialValue then
  begin
    InitialValue := BoldAttributeRTInfo.InitialValue;
    if CompareText(InitialValue, '<NULL>') = 0 then // do not localize
      SetToNull
    else
      try
        AsString := InitialValue;
      except
        on e: Exception do
        begin
          Raise EBold.CreateFmt(sIllegalInitialValue,
                                [InitialValue, BoldAttributeRTInfo.AsString, BOLDCRLF, e.message]);
        end;
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
  raise Ebold.CreateFmt(sMethodNotImplemented, [classname, 'SetEmptyValue']); // do not localize
end;

{ TBoldObjectReference }

destructor TBoldObjectReference.Destroy;
begin
  PrepareToDestroy;
  FreeAndNil(fObjectReferenceController);
  inherited Destroy;
end;

procedure TBoldObjectReference.DefaultSubscribe(Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent = breReEvaluate);
begin
  if mutable then
    AddSmallSubscription(Subscriber, [beValueChanged, beValueInvalid], RequestedEvent);
end;

function TBoldObjectReference.GetLocator: TBoldObjectLocator;
begin
  EnsureContentsCurrent;
  BoldClearLastFailure;
  if not CanRead(nil) then
    BoldRaiseLastFailure(self, 'GetLocator', ''); // do not localize
  Result := ReferenceController.GetLocator;
end;

function TBoldObjectReference.GetBoldObject: TBoldObject;
var
  aLocator: TBoldObjectLocator;
begin
  aLocator := Locator; {CanRead, EnsureContentsCurrent called by GetLocator}
  if not Assigned(aLocator) then
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
      BoldRaiseLastFailure(self, 'SetLocator', ''); // do not localize
    ReferenceController.SetLocator(NewLocator);
  end;
end;

procedure TBoldObjectReference.SetBoldObject(NewObject: TBoldObject);
begin
  if Assigned(NewObject) then
    Locator := NewObject.BoldObjectLocator
  else
    Clear; // this will cause "CanClear" to be checked before the "CanSetLocator"
end;

procedure TBoldObjectReference.Clear;
begin
  BoldClearLastFailure;
  if not canClear(nil) then
    BoldRaiseLastFailure(self, 'Clear', ''); // do not localize
  Locator := nil;
end;

function TBoldObjectReference.GetStringRepresentation(Representation: TBoldRepresentation): string;
begin
  if Assigned(BoldObject) then
    Result := BoldObject.GetStringRepresentation(Representation)
  else
    Result := '';
end;

procedure TBoldObjectReference.SetStringRepresentation(Representation: TBoldRepresentation; Value: string);
begin
  if Assigned(BoldObject) then
    BoldObject.SetStringRepresentation(Representation, Value);
  //FixMe: is it an error trying to set the stringrep of a singlelink without value?
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
      raise EBold.CreateFmt(sUnknownCompareType, [ClassName, 'IsEqualAs']); // do not localize
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
  NewList := TBoldObjectList.CreateWithTypeInfo(TBoldSystemTypeInfo(BoldType.SystemTypeInfo).ListTypeInfoByElement[BoldType]);
  if Assigned(BoldObject) then
    NewList.Add(BoldObject);
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
  if assigned(newLocator) and assigned(OwningObject) and (OwningObject.BoldSystem <> NewLocator.BoldSystem) then
    result := false
  else
    result := VerifyClass(NewLocator) and SendQuery(bqMaySetValue, [NewLocator], Subscriber);
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
    KnownClass := TBoldSystemTypeInfo(BoldType.SystemTypeInfo).TopSortedClasses[aLocator.BoldObjectId.TopSortedIndex];
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
  aSystem := TBoldSystem.DefaultSystem;
  if assigned(aSystem) then
    InitializeMember(nil, aSystem.BoldSystemTypeInfo.TopSortedClasses.ItemsByObjectClass[ObjectClass])
  else
    InitializeMember(nil, nil);
  HasOldValues := false;
end;

procedure TBoldObjectReference.InitializeMember(
  AOwningElement: TBoldDomainElement; ElementTypeInfo: TBoldElementTypeInfo);
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
      else
        fObjectReferenceController := TBoldDirectSingleLinkController.Create(Self);
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
  result := SendQuery(bqMayClear, [], Subscriber);
end;

function TBoldObjectReference.InternalMayUpdate: Boolean;
begin
  result := ReferenceController.MayUpdate;
end;

function TBoldObjectReference.ProxyClass: TBoldMember_ProxyClass;
begin
  result := ReferenceController.ProxyClass;
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

  procedure GetOldValueAsIdLists(IdList1, IdList2: TBoldObjectIdList; Oldvalue: IBoldValue);
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

  procedure RemoveIdFromLink(LinkId: TBoldObjectId; OldLink: IBoldValue);
  var
    IdRef: IBoldObjectIdRef;
    IdRefPair: IBoldObjectIdRefPair;
    FreestandingList: IBoldFreeStandingIdList;
    FreestandingListPair: IBoldFreeStandingIdListPair;
  begin
    if OldLink.QueryInterface(IBoldObjectIdref, IdRef) = S_OK then
      IdRef.SetFromId(nil)
    else if OldLink.QueryInterface(IBoldObjectIdrefPair, IdRefPair) = S_OK then
      IdRefPair.SetFromIds(nil, nil)
    else if OldLink.QueryInterface(IBoldFreeStandingIdList, FreestandingList) = S_OK then
      FreeStandingList.RemoveId(LinkId)
    else if OldLink.QueryInterface(IBoldFreeStandingIdListPair, FreestandingListPair) = S_OK then
      FreeStandingListPair.RemoveId(LinkId)
    else
      raise EBoldInternal.CreateFmt('%s.AdjustOldValues (RemoveIdFromLink): Unknown type of link', [Classname]);
  end;

  procedure AddIdToLink(Id1, Id2: TBoldObjectId; OldLink: IBoldValue);
  var
    IdRef: IBoldObjectIdRef;
    IdRefPair: IBoldObjectIdRefPair;
    FreestandingList: IBoldFreeStandingIdList;
    FreestandingListPair: IBoldFreeStandingIdListPair;
  begin
    if OldLink.QueryInterface(IBoldObjectIdref, IdRef) = S_OK then
      IdRef.SetFromId(TranslationList.TranslateToNewId[Id1])
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
  // this happens for direct singlelinks and for innerlinks.
  // Either way, the Role that should be adjusted is always the one pointed out by BoldRoleRTInfo.IndexOfOtherEnd

  Oldvalues := OwningObject.BoldSystem.fOldValueHandler.Oldvalues;
  if BoldRoleRTInfo.IsStoredInObject then
  begin
    // fix to remove deleted objects from Oldvalues
    OldRef := OldValue as IBoldObjectIdRef;
    if assigned(OldRef) and Assigned(OldRef.Id) then
    begin
      OldObjectContents := OldValues.ObjectContentsByObjectId[Oldref.Id];
      if assigned(OldObjectContents) then
      begin
        OldLink := OldObjectContents.ValueByIndex[BoldRoleRTInfo.IndexOfOtherEnd];
        if assigned(OldLink) then
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
  if BoldRoleRTInfo.RoleType = rtRole then // only regular roles have an inital value.
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

{ TBoldList }

destructor TBoldList.Destroy;
begin
  PrepareToDestroy;
  FreeAndNil(fListController);
  inherited Destroy;
  FreeData;
end;

procedure TBoldList.DefaultSubscribe(Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent = breReEvaluate);
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
  result := ListController.GetStringRepresentation
end;

procedure TBoldList.Clear;
begin
  BoldClearLastFailure;
  PrepareClear;
  if mutable and not CanClear(nil) then
    BoldRaiseLastFailure(self, 'Clear', ''); // do not localize

  // Speed optimizing
//  if Derived and Deriver.IsDeriving then
//  begin
//    for i := count - 1 downto 0 do
//      ListController.RemoveByIndex(i);
//  end
//  else
//  begin
  if assigned(BoldSYstem) then
    BoldSystem.StartTransaction;
  try
    InternalClear;
    if assigned(BoldSYstem) then
      BoldSystem.CommitTransaction;
  except
    if assigned(BoldSYstem) then
      BoldSystem.RollbackTransaction;
    raise;
  end;
//  end;
end;

function TBoldList.Includes(Item: TBoldElement): Boolean;
begin
  Result := IncludesElement(Item);
end;

procedure TBoldList.SetDuplicateMode(NewMode: TBoldListDupMode);
var
  i, j: integer;
  OldMode: TBoldListDupMode;
begin
  OldMode := GetDuplicateMode;
  if (NewMode <> OldMode) and (NewMode = bldmMerge) then
  begin // warning: Timecomplexity is O(n2)
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

procedure TBoldList.Remove(Item: TBoldElement);
var
  I: Integer;
begin
  EnsureContentsCurrent;
  I := IndexOf(Item);
  if I <> -1 then
    RemoveByIndex(I)
  else
    raise EBold.CreateFmt(sItemNotInList, [ClassName]);
end;

procedure TBoldList.Add(Element: TBoldElement);
begin
  if not mutable then
    MutableError('AddToList'); // do not localize
  AddElement(Element);
end;

procedure TBoldList.AddList(List: TBoldList);
var
  I: Integer;
begin
  if not mutable then MutableError('AddListToList'); // do not localize
  for I := 0 to List.Count - 1 do
    AddElement(List[I]);
end;

function TBoldList.IndexOf(Item: TBoldElement): Integer;
begin
  result := IndexOfElement(Item);
end;

procedure TBoldList.Insert(index: Integer; Element: TBoldElement);
begin
  if not mutable then MutableError('InsertElementToList'); // do not localize
  InsertElement(index, Element);
end;

function TBoldList.GetCanCreateNew: Boolean;
begin
  result := CanModify and assigned(ListController) and ListController.CanCreateNew;
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

procedure TBoldList.GetAsList(ResultList: TBoldIndirectElement);
begin
  ResultList.SetReferenceValue(Self);
end;

procedure TBoldList.EnsureRange(FromIndex, ToIndex: integer);
begin
  // do nothing
end;

function TBoldList.canInsert(index: Integer; Element: TBoldElement; Subscriber: TBoldSubscriber): Boolean;
begin
  result := SendQuery(bqMayInsert, [index, Element], Subscriber);
end;

function TBoldList.CanRemove(index: Integer; Subscriber: TBoldSubscriber): Boolean;
begin
  result := SendQuery(bqMayRemove, [index], Subscriber);
end;

function TBoldList.CanMove(CurIndex, NewIndex: Integer; Subscriber: TBoldSubscriber = nil): Boolean;
begin
  result := (NewIndex >= 0) and (NewIndex < Count) and
            SendQuery(bqMayMove, [CurIndex, NewIndex], Subscriber);
end;

function TBoldList.CanSet(index: Integer; Item: TBoldElement; Subscriber: TBoldSubscriber): Boolean;
begin
  result := SendQuery(bqMayReplace, [index, Item], Subscriber);
end;

constructor TBoldMember.CreateWithTypeInfo(ElementTypeInfo: TBoldElementTypeInfo);
begin
  InitializeMember(nil, ElementTypeInfo);
end;

procedure TBoldList.Sort(CompareFunc: TBoldElementCompare);

  procedure QuickSort(L, R: Integer;
    SCompare: TBoldElementCompare);
  var
    I, J: Integer;
    P: TBoldElement;
  begin
    repeat
      I := L;
      J := R;
      P := Elements[(L + R) shr 1];
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
      if L < J then
        QuickSort(L, J, SCompare);
      L := I;
    until I >= R;
  end;

begin
  if (self <> nil) and (Count > 0) then
    QuickSort(0, Count - 1, CompareFunc);
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

procedure TBoldList.InitializeMember(AOwningElement: TBoldDomainElement;
  ElementTypeInfo: TBoldElementTypeInfo);
begin
  inherited;
  AllocateData;
end;

function TBoldList.CanClear(Subscriber: TBoldSubscriber): Boolean;
begin
  result := SendQuery(bqMayClear, [], Subscriber);
end;

function TBoldList.GetDuplicateMode: TBoldListDupMode;
begin
  result := TBoldListDupMode(GetInternalState(BoldDuplicateModeMask, BoldDMShift));
end;

procedure TBoldList.PrepareClear;
begin
  // do nothing
end;

procedure TBoldList.MakeContentsImmutable;
var
  I :integer;
begin
  for I := 0 to Count - 1 do
    Elements[I].MakeImmutable;
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
    Add(SourceList[I]); // Addoperator will clone each element before adding to list
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
begin
  EnsureContentsCurrent;
  BoldClearLastFailure;
  if not CanInsert(index, Element, nil) then
    BoldRaiseLastFailure(self, 'InsertElement', ''); // do not localize
  if Element is TBoldMember then
    Insert(index, TBoldMember(Element))
  else
    raise EBold.CreateFmt(sElementNotBoldMember, [ClassName, 'InsertElement']); // do not localize
end;

function TBoldMemberList.GetElement(index: Integer): TBoldElement;
begin
  EnsureContentsCurrent;
  BoldClearLastFailure;
  if not CanRead(nil) then
    BoldRaiseLastFailure(self, 'GetElement', ''); // do not localize
  Result := TBoldElement(List[index]);
end;

procedure TBoldMemberList.SetElement(index: Integer; Value: TBoldElement);
begin
  EnsureContentsCurrent;
  if CheckReplace(index, Value as TBoldMember) then
  begin
    BoldClearLastFailure;
    if not CanSet(index, Value, nil) then
      BoldRaiseLastFailure(self, 'SetElement', ''); // do not localize
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
    BoldRaiseLastFailure(self, 'GetCount', ''); // do not localize
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
  Assert(Elements[index] is TBoldMember);
  Result := TBoldMember(Elements[index]);
end;

procedure TBoldMemberList.SetBoldMember(index: Integer; Value: TBoldMember);
begin
  Elements[index] := Value;
end;

procedure TBoldMemberList.Add(Item: TBoldMember);
begin
  if CloneMembers then
    List.Add(TBoldMember(Item.Clone))
  else
    if CheckAdd(Item) then
      List.Add(Item)
end;

procedure TBoldMemberList.Move(CurIndex, NewIndex: Integer);
begin
  if not mutable then MutableError('MoveElementInList'); // do not localize
  EnsureContentsCurrent;
  List.Move(CurIndex, NewIndex);
  Changed(beOrderChanged, []);
end;

procedure TBoldMemberList.InternalAddWithoutCloning(Item: TBoldMember); // FIXME remove when TBoldClass no longer member
begin
  List.Add(Item);
end;

procedure TBoldMemberList.Insert(index: Integer; Item: TBoldMember);
begin
  if assigned(Item) then
  begin
    if CloneMembers then
      List.Insert(index, TBoldMember(Item.Clone))
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
  raise EBoldFeatureNotImplementedYet.CreateFmt(sMethodNotImplemented, [ClassName, 'GetStreamName']); // do not localize
end;

function TBoldMemberList.GetStringRepresentation(Representation: TBoldRepresentation): string;
begin
  result := IntToStr(Count);
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
  else if not Includes(NewMember) then
    Result := True
  else
    Result := DuplicateControl;
end;

function TBoldMemberList.CheckInsert(index: Integer; NewMember: TBoldMember): Boolean;
begin
  assert(assigned(NewMember), 'nil not allowed, should have been filtered out before');
  if Includes(NewMember) then
    Result := DuplicateControl
  else
    result := True;
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

procedure TBoldMemberList.InitializeMember(
  AOwningElement: TBoldDomainElement; ElementTypeInfo: TBoldElementTypeInfo);
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
  Elem := CreateNew;
  Assert(Elem is TBoldMember);
  try
    Insert(index, TBoldMember(Elem));
    if CloneMembers then
      FreeAndNil(Elem);
  except
    FreeAndNil(Elem);
    raise
  end;
end;
function TBoldMemberList.ProxyClass: TBoldMember_ProxyClass;
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

{ TBoldObjectList }

constructor TBoldObjectList.CreateTypedList(ObjectClass: TBoldObjectClass);
var
  aSystem: TBoldSystem;
  ClassTypeInfo: TBoldClassTypeInfo;
begin
  aSystem := TBoldSystem.DefaultSystem;
  if not assigned(aSystem) then
    raise EBold.CreateFmt(sCannotFindSystem, [classname]);
  ClassTypeInfo := aSystem.BoldSystemTypeInfo.TopSortedClasses.ItemsByObjectClass[ObjectClass];
  if not assigned(ClassTypeInfo) then
    raise EBold.CreateFmt(sClassIsNotBusinessClass, [classname, ObjectClass.ClassName]);

  InitializeMember(nil, aSystem.BoldSystemTypeInfo.ListTypeInfoByElement[ClassTypeInfo]);
end;

procedure TBoldObjectList.AllocateData;
begin
end;

function TBoldObjectList.InternalAddNew: TBoldElement;
var
  Obj: TBoldObject;
begin
  Result := CreateNew;
  Obj := result as TBoldObject;
  Obj.BoldSystem.StartTransaction;
  try
    Add(Obj);
    Obj.BoldSystem.CommitTransaction;
  except
    Obj.BoldSystem.RollBackTransaction;
    raise;
  end;
end;

procedure TBoldObjectList.InsertNew(index: Integer);
var
  Obj: TBoldObject;
begin
  Obj := CreateNew as TBoldObject;
  Obj.BoldSystem.StartTransaction;
  try
    Insert(index, Obj);
    Obj.BoldSystem.CommitTransaction;
  except
    Obj.BoldSystem.RollBackTransaction;
    raise;
  end;
end;

procedure TBoldObjectList.FreeData;
begin
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
      raise EBold.CreateFmt(sElementNotBoldObject, [ClassName, 'AddElement']); // do not localize
  end;
end;

procedure TBoldObjectList.AddList(List: TBoldList);
var
  I: Integer;
  ObjectList: TBoldObjectList;
begin
  if not mutable then
    MutableError('AddListToList'); // do not localize
  if not (List is TBoldObjectList) then
    raise EBold.CreateFmt(sListNotObjectList, [ClassName]);
  ObjectList := List as TBoldObjectList;
  for I := 0 to ObjectList.Count - 1 do
    AddLocator(ObjectList.Locators[I]);
end;

procedure TBoldObjectList.InsertElement(index: Integer; Element: TBoldElement);
begin
  if Assigned(Element) then
  begin
    if Element is TBoldObject then
      InsertLocator(index, TBoldObject(Element).BoldObjectLocator)
    else
      raise EBold.CreateFmt(sElementNotBoldObject, [ClassName, 'InsertElement']); // do not localize
  end;
end;

function TBoldObjectList.GetElement(index: Integer): TBoldElement;
var
  aLocator: TBoldObjectLocator;
begin
  aLocator := GetLocator(index);
  if Assigned(aLocator) then
  begin
    aLocator.EnsureBoldObject;
    Result := aLocator.BoldObject;
  end
  else
    Result := nil;
end;

procedure TBoldObjectList.SetElement(index: Integer; Value: TBoldElement);
begin
  if Value is TBoldObject then
    SetLocator(index, TBoldObject(Value).BoldObjectLocator)
  else if not assigned(Value) then
    raise EBold.CreateFmt(sElementIsNil, [classname])
  else
    raise EBold.CreateFmt(sElementNotBoldObject, [classname, 'SetElement']); // do not localize
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
    BoldRaiseLastFailure(self, 'GetCount', ''); // do not localize
  result := ListController.GetCount;
end;

function TBoldObjectList.GetLocator(index: Integer): TBoldObjectLocator;
begin
  EnsureContentsCurrent;
  BoldClearLastFailure;
  if not CanRead(nil) then
    BoldRaiseLastFailure(self, 'GetLocator', ''); // do not localize
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
  Clear;
  for i := SourceList.Count - 1 downto 0 do    // WARNING: TimeComplexity O2? How to ensure correct order when looping down? /Joho
    InsertLocator(0, SourceList.Locators[I]);
end;

procedure TBoldObjectList.SetLocator(index: Integer; NewLocator: TBoldObjectLocator);
begin
  EnsureContentsCurrent;
  if CheckReplace(index, NewLocator) then
  begin
  BoldClearLastFailure;
    if not CanSetLocator(index, NewLocator, nil) then
      BoldRaiseLastFailure(self, 'SetLocator', ''); // do not localize
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

procedure TBoldObjectList.AddLocator(NewLocator: TBoldObjectLocator);
begin
  EnsureContentsCurrent;
  if CheckAdd(NewLocator) then
  begin
    BoldClearLastFailure;
    if not CanInsertLocator(-1, NewLocator, nil) then
      BoldRaiseLastFailure(self, 'AddLocator', ''); // do not localize
    ObjectListController.AddLocator(NewLocator);
  end;
end;

procedure TBoldObjectList.Add(BoldObject: TBoldObject);
begin
  AddElement(BoldObject);
end;

procedure TBoldObjectList.Move(CurIndex, NewIndex: Integer);
begin
  if not mutable then MutableError('MoveElementInList'); // do not localize
  BoldClearLastFailure;
  if not CanMove(CurIndex, NewIndex, nil) then
    BoldRaiseLastFailure(self, 'Move', ''); // do not localize
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
        BoldRaiseLastFailure(self, 'InsertLocator', ''); // do not localize
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
    BoldRaiseLastFailure(self, 'RemoveByIndex', ''); // do not localize
  InternalRemoveByIndex(index);
end;

function TBoldObjectList.IndexOfLocator(Locator: TBoldObjectLocator): Integer;
begin
  BoldClearLastFailure;
  if not CanRead(nil) then
    BoldRaiseLastFailure(self, 'IndexOfLocator', ''); // do not localize
  EnsureContentsCurrent;
  result := ObjectListController.IndexOfLocator(Locator);
end;

function TBoldObjectList.Includes(BoldObject: TBoldObject): Boolean;
begin
  result := IncludesElement(BoldObject);
end;

function TBoldObjectList.LocatorInList(NewLocator: TBoldObjectLocator): Boolean;
begin
  BoldClearLastFailure;
  if not CanRead(nil) then
    BoldRaiseLastFailure(self, 'LocatorInList', ''); // do not localize
  EnsureContentsCurrent;
  result := ObjectListController.IncludesLocator(NewLocator);
end;

function TBoldObjectList.CreateObjectIdList: TBoldObjectIdList;
var
  I: Integer;
begin
  Result := TBoldObjectIdList.Create;
  for I := 0 to Count - 1 do
    Result.Add(Locators[I].BoldObjectID);
end;

procedure TBoldObjectList.FillFromIDList(ObjectIdList: TBoldObjectIdList; BoldSystem: TBoldSystem);
var
  I: Integer;
begin
  for I := 0 to ObjectIdList.Count - 1 do
    AddLocator(BoldSystem.EnsuredLocatorByID[ObjectIdList[I]]);
end;

function TBoldObjectList.GetByIndex(MemberList: TBoldMemberList): TBoldObject;
begin
  result := GetByIndexAndSubscribe(MemberList, nil);
end;

function TBoldObjectList.GetByIndexAndSubscribe(MemberList: TBoldMemberList; Subscriber: TBoldSubscriber): TBoldObject;
var
  Locator: TBoldObjectLocator;
begin
  BoldClearLastFailure;
  if not CanRead(nil) then
    BoldRaiseLastFailure(self, 'GetByIndex', ''); // do not localize
  Locator := ObjectListController.GetLocatorByQualifiersAndSubscribe(MemberList, Subscriber);
  if Assigned(Locator) then
    result := Locator.EnsuredBoldObject
  else
    Result := nil;
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
    KnownClass := TBoldSystemTypeInfo(BoldType.SystemTypeInfo).TopSortedClasses[aLocator.BoldObjectId.TopSortedIndex];
    if KnownClass.ConformsTo(AllowedClass) then
      Result := true
    else if aLocator.BoldObjectId.TopSortedIndexExact then
      Result := false
    else // if it looks wrong, but the classID is inexact, then we will allow it if it _could_ be right...
      Result := AllowedClass.ConformsTo(knownclass);
    if not result then
      SetBoldLastFailureReason(TBoldFailureReason.CreateFmt(sItemNotAllowedInList, [KnownClass.ExpressionName, AllowedClass.ExpressionName], self));
  end
  else
    result := true;
end;

function TBoldObjectList.GetObjectListController: TBoldAbstractObjectListController;
begin
  Assert(ListController is TBoldAbstractObjectListController);
  Result := TBoldAbstractObjectListController(ListController)
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

procedure TBoldObjectList.EnsureObjects;
begin
  EnsureRange(0, Count - 1);
end;

procedure TBoldObjectList.EnsureRange(FromIndex, ToIndex: integer);
var
  FetchList: TBoldObjectList;
  i: integer;
  aSystem: TBoldSystem;
  Locator: TBoldObjectLocator;

  function RestrictRange(value, min, max: integer): integer;
  begin
    if value < min then
      result := min
    else if value > max then
      result := max
    else
      result := value;
  end;
begin
  if Count > 0 then
  begin
    aSystem := nil;
    FetchList := TBoldObjectList.Create;
    try
      for i := RestrictRange(FromIndex, 0, count - 1) to RestrictRange(ToIndex, 0, count - 1) do
      begin
        Locator := Locators[i];
        if not assigned(Locator.BoldObject) (*or
          Locator.BoldObject.EffectiveInvalid*) then
        begin
          if not assigned(aSystem) then
            aSystem := Locator.BoldSystem;
          assert(Locator.BoldSystem = aSystem);
          FetchList.AddLocator(Locator);
        end;
      end;
      if assigned(aSystem) then
        aSystem.SystemPersistenceHandler.FetchList(FetchList);
    finally
      FetchList.Free;
    end;
  end;
end;

function TBoldObjectList.CanInsertLocator(index: Integer; Locator: TBoldObjectLocator; Subscriber: TBoldSubscriber): Boolean;
begin
  if not assigned(locator) then
    result := false
  else if ownedByObject and (locator.BoldSystem <> OwningObject.BoldSystem) then
    result := false
  else
    result := VerifyClass(Locator) and SendQuery(bqMayInsert, [index, Locator], Subscriber);
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
  if not assigned(locator) then
    result := false
  else if ownedByObject and (locator.BoldSystem <> OwningObject.BoldSystem) then
    result := false
  else
    result := VerifyClass(Locator) and SendQuery(bqMayReplace, [index, Locator], Subscriber);
end;

constructor TBoldObjectList.InternalCreateClassList(System: TBoldSystem; ListTypeInfo: TBoldListTypeInfo);
begin
  InitializeMember(System, ListTypeInfo);
  DuplicateMode := bldmMerge;
  InitializeStateToInvalid;
end;

function TBoldObjectList.CheckAdd(NewLocator: TBoldObjectLocator): Boolean;
begin
  if not Assigned(NewLocator) then
    Result := False // Adding nil does nothing
  else if assigned(OwningObject) and (OwningObject.BoldSystem <> NewLocator.BoldSystem) then
    result := false
  else if not ObjectListController.IncludesLocator(NewLocator) then // LocatorInList ensures current, and we don't want that
    Result := True
  else
    Result := DuplicateControl;
end;

function TBoldObjectList.CheckReplace(index: Integer; NewLocator: TBoldObjectLocator): Boolean;
begin
  if not assigned(NewLocator) then
    result := false
  else if assigned(OwningObject) and (OwningObject.BoldSystem <> NewLocator.BoldSystem) then
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
  if assigned(OwningObject) and (OwningObject.BoldSystem <> NewLocator.BoldSystem) then
    result := false
  else if ObjectListController.IncludesLocator(NewLocator) then
    result := DuplicateControl
  else
    result := True;
end;

procedure TBoldObjectList.InitializeMember(
  AOwningElement: TBoldDomainElement; ElementTypeInfo: TBoldElementTypeInfo);
begin
  inherited InitializeMember(AOwningElement, ElementTypeInfo);
  DuplicateMode := bldmMerge;
  if not assigned(BoldMemberRTInfo) then
  begin
    if assigned(AOwningElement) and (AOwningElement is TBoldSystem) then
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
  Assert((BoldMemberRTInfo = nil) or (BoldMemberRTInfo is TBoldRoleRTInfo));
  Result := TBoldRoleRTInfo(BoldMemberRTInfo)
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
      // the other subscription flag might be set...
      (ListController as TBoldObjectListController).DropSubscriptions;   // FIXME add to abstract controller
      (ListController as TBoldObjectListController).Resubscribe;        // FIXME add to abstract controller
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
      // the other subscription flag might be set...
      (ListController as TBoldObjectListController).DropSubscriptions;
      (ListController as TBoldObjectListController).Resubscribe;
    end;
  end
  else
    raise EBold.CreateFmt(sCanOnlyChangeForStandAloneLists, [ClassName, 'SetSubscribeToLocatorsInList']); // do not localize
end;

function TBoldObjectList.ProxyClass: TBoldMember_ProxyClass;
begin
  result := ObjectListController.ProxyClass;
end;

function TBoldObjectList.AtTime(Time: TBoldTimestampType): TBoldMember;
begin
  if ObjectListController.HandlesAtTime then
    result := ObjectListController.AtTime(Time)
  else
    result := inherited AtTime(Time);
end;

procedure TBoldObjectList.FreeContent;
begin
  inherited;
  ObjectListController.FreeContent;
end;

procedure TBoldObjectList.AssignContentValueFromElement(source: TBoldElement);
var
  i: integer;
  SourceList: TBoldObjectList;
  Controller: TBoldObjectListController;
begin
  if (source is TBoldObjectList) and (ListController is TBoldObjectListController) then
  begin
    Controller := ListController as TBoldObjectListController;
    SourceList := source as TBoldObjectList;
    Controller.LocatorList.Clear;
    for i := 0 to SourceList.Count - 1 do
      Controller.Locatorlist.Add(SourceList.Locators[i]);
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

{ TBoldListController }

constructor TBoldListController.Create(OwningList: TBoldList);
begin
  fOwningList := OwningList;
end;

function TBoldListController.CreateNew: TBoldElement;
begin
  raise EBold.Create(sCannotCreateNewElement);
end;

function TBoldListController.GetBoldSystem: TBoldSystem;
begin
  result := OwningList.BoldSystem;
end;

function TBoldListController.GetCanCreateNew: Boolean;
begin
  result := false;
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
  Assert(inherited OwningList is TBoldObjectList);
  result := TBoldObjectList(inherited OwningList);
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
  // do nothing
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

function TBoldObjectReferenceController.ProxyClass: TBoldMember_ProxyClass;
begin
  result := TBoldObjectReference_Proxy;
end;

{ TBoldAbstractController }

function TBoldAbstractController.AssertIntegrity: Boolean;
begin
  Result := True;
   // DO nothing, make abstract virtual later
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

procedure TBoldAbstractController.linkto(NewLocator: TBoldObjectLocator; updateOrderNo: Boolean; Mode: TBoldLinkUnlinkMode);
begin
  // do nothing
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
  // do nothing
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

procedure TBoldObjectReferenceController.AssignContentValue(Source: IBoldValue);
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
    BoldRaiseLastFailure(OwningReference, 'SetLocator', ''); // do not localize

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
  Assert(Item is TBoldObjectLocator);
  Result := TBoldObjectLocator(Item);
end;

function TBoldLocatorHashIndex.Match(const Key; Item: TObject): Boolean;
begin
  Result := TBoldObjectLocator(Key) = (ItemAsLocator(Item));
end;

function TBoldObjectList.GetElementTypeInfoForType: TBoldElementTypeInfo;
var
  aSystem: TBoldSystem;
begin
  aSystem := FindASystem;

  if assigned(aSystem) then
    result := aSystem.BoldSystemTypeInfo.ListTypes.ItemsByDelphiName[self.classname]
  else
    result := nil;
end;

function TBoldAttribute.GetElementTypeInfoForType: TBoldElementTypeInfo;
var
  aSystem: TBoldSystem;
begin
  aSystem := FindASystem;

  if assigned(aSystem) then
    result := aSystem.BoldSystemTypeInfo.AttributeTypeInfoByDelphiName[Self.ClassName]
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

constructor TBoldFailureReason.create(reason: String;
  Originator: TBoldDomainElement);
begin
  fReason := reason;
  fOriginator := Originator;
  if assigned(fOriginator) then
  begin
    fSubscriber := TBoldPassthroughSubscriber.create(ReceiveOriginatorDestroy);
    Originator.AddSmallSubscription(fSubscriber, [bedestroying], bedestroying);
  end;
end;

constructor TBoldFailureReason.CreateFmt(Reason: string;
  const args: array of const; Originator: TBoldDomainElement);
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
var
  MemberClass: TBoldMemberClass;
begin
  if BoldType is TBoldListTypeInfo then
    MemberClass := TBoldMemberClass(TBoldListTypeInfo(BoldType).ListClass)
  else if BoldType is TBoldAttributeTypeInfo then
    MemberClass := TBoldMemberClass(TBoldAttributeTypeInfo(BoldType).AttributeClass)
  else if BoldType is TBoldClassTypeInfo then
    MemberClass := TBoldObjectReference
  else
  begin
    if assigned(BoldType) then
      raise EBold.CreateFmt(sInvalidBoldType, [ClassName, BoldType.ClassName])
    else
      raise EBold.CreateFmt(sInvalidBoldType_Nil, [ClassName]);
  end;
  if assigned(MemberClass) then
    result := MemberClass.CreateWithTypeInfo(BoldType)
  else
    raise EBold.CreateFmt(sDelphiClassNotInstalled, [ClassName, BoldType.ExpressionName, BoldType.DelphiName])
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

procedure TBoldMember_Proxy.AssignContent(Source: IBoldValue);
begin
  AssignContentValue(Source);
  Assert ((Mode <> bdepPMIn) or (Source.BoldPersistenceState = bvpsCurrent));
  ProxedMember.SetBoldPersistenceState(Source.BoldPersistenceState);
end;

function TBoldMember_Proxy.GetBoldPersistenceState: TBoldValuePersistenceState;
begin
  result := ProxedMember.BoldPersistenceState;
end;

function TBoldMember_Proxy.GetContentName: String;
begin
  result := ProxedMember.GetStreamName;
end;

function TBoldMember_Proxy.GetStreamName: String;
begin
  result := ProxedMember.GetStreamName;
end;

function TBoldMember_Proxy.GetProxedController: TBoldAbstractController;
begin
  Result := ProxedMember.GetController;
end;

function TBoldMember_Proxy.GetProxedMember: TBoldMember;
begin
  Assert(ProxedElement is TBoldMember);
  result := TBoldMember(ProxedElement);
end;

procedure TBoldMember_Proxy.SetBoldPersistenceState(Value: TBoldValuePersistenceState);
begin
  ProxedMember.SetBoldPersistenceState(Value);
end;

{ TBoldAttribute_Proxy }

procedure TBoldAttribute_Proxy.AssignContentValue(Source: IBoldValue);
begin
  ProxedAttribute.AssignContentValue(Source);
end;

procedure TBoldAttribute_Proxy.SetContentToNull;
begin
  ProxedAttribute.SetContentToNull;
end;

function TBoldAttribute_Proxy.GetContentIsNull;
begin
  Result := ProxedAttribute.GetContentIsNull;
end;

function TBoldAttribute_Proxy.GetProxedAttribute: TBoldAttribute;
begin
  Assert(ProxedElement is TBoldAttribute);
  result := TBoldAttribute(ProxedElement);
end;

{ TBoldSystem_Proxy }

procedure TBoldSystem_Proxy.AllObjectIds(resultList: TBoldObjectIdList;
  OnlyLoaded: Boolean);
var
  Traverser: TBoldLocatorListTraverser;
begin
  Traverser := ProxedSystem.Locators.CreateTraverser;
  while not Traverser.EndOfList do
  begin
    if not OnlyLoaded or assigned(Traverser.Locator.BoldObject) then
      ResultList.Add(Traverser.Locator.BoldObjectID);
    Traverser.Next;
  end;
  Traverser.Free;
end;

procedure TBoldSystem_Proxy.ApplytranslationList(
  IdTranslationList: TBoldIdTranslationList);
var
  I: Integer;
  Locator: TBoldObjectLocator;
begin
  for I := 0 to IdTranslationList.Count - 1 do
  begin
    if Assigned(IdTranslationList.OldIDs[I]) then
    begin
      Locator := ProxedSystem.Locators.LocatorByID[IdTranslationList.OldIDs[I]];
      if Assigned(IdTranslationList.NewIDs[I]) then
        // Id changed value
        ProxedSystem.Locators.UpdateId(Locator, IdTranslationList.NewIds[i])
    end
    else
    begin
      // Loaded an ID that might or might not be in-core already
      ProxedSystem.EnsuredLocatorByID[IdTranslationList.NewIDs[I]];
    end;
  end;
  ProxedSystem.UndoHandler.ApplytranslationList(IdTranslationList);
  ProxedSystem.fOldValueHandler.OldValues.ApplytranslationList(IdTranslationList);
end;

procedure TBoldSystem_Proxy.ApplyValueSpace(ValueSpace: IBoldValueSpace;
  IgnorePersistenceState: Boolean);

  procedure ApplyObjectContents(BoldObject: TBoldObject; ObjectContents: IBoldObjectContents);
  var
    i: Integer;
    aValue: IBoldValue;
  begin
    BoldObject.SetBoldExistenceState(ObjectContents.BoldExistenceState);
    BoldObject.SetBoldPersistenceState(ObjectContents.BoldPersistenceState);
    for i := 0 to BoldObject.BoldMemberCount - 1 do
    begin
      aValue := ObjectContents.valueByIndex[i];
      if assigned(aValue) then
        BoldObject.BoldMembers[i].AsIBoldValue[bdepContents].AssignContent(aValue);
    end;
  end;

var
  i: Integer;
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
  finally
    anIdList.Free;
  end;
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
  if not ObjectLocator.BoldObjectID.TopSortedIndexExact and  { TODO : Try to promote }
    ObjectId.TopSortedIndexExact then
    ProxedSystem.Locators.UpdateID(ObjectLocator, ObjectId);
end;

procedure TBoldSystem_Proxy.ExactifyIDs(
  TranslationList: TBoldIdTranslationList);
var
  i: Integer;
begin
  for i := 0 to TranslationList.Count - 1 do
    ProxedSystem.Locators.UpdateId(ProxedSystem.Locators.LocatorByID[TranslationList.OldIds[i]], TranslationList.NewIds[i]);
end;

function TBoldSystem_Proxy.GetEnsuredObjectContentsByObjectId(
  ObjectId: TBoldObjectId): IBoldObjectContents;
begin
  result := GetObjectContentsByObjectId(ObjectId);
  if not assigned(result) then
    result := ProxedSystem.CreateExistingObjectByID(ObjectID).AsIBoldObjectContents[Mode];
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

function TBoldSystem_Proxy.GetProxedSystem: TBoldSystem;
begin
  Assert(ProxedElement is TBoldSystem);
  result := TBoldSystem(ProxedElement);
end;


{ TBoldObject_Proxy }

procedure TBoldObject_Proxy.EnsureMember(MemberId: TBoldMemberId;
  const ContentName: string);
var
  Member: TBoldMember;
begin
  if MemberId.MemberIndex >= ProxedObject.BoldMembercount then
    raise eBold.CreateFmt(sNotEnoughMembers, [classname, MemberId.MemberIndex]);
  Member := ProxedObject.BoldMembers[MemberId.MemberIndex];
  if CompareText(Member.GetStreamName, ContentName) <> 0 then
    raise EBold.CreateFmt(sUnexpectedStreamType,
      [classname, Member.DisplayName, ContentName, Member.GetStreamName]);

// FIXME: Make sure below works for BoldDirectSingleLink vs BoldObjectReference  FIXME FIXME FIXME
//  if AnsiCompareText(BoldMembers[MemberId.MemberIndex].TypeExpressionName, TypeExpressionName) <> 0 then
//    raise EBold.CreateFmt('TBoldObject.EnsureMember: Member was of wrong type (was %s, should have been: %s',
//     [BoldMembers[MemberId.MemberIndex].TypeExpressionName, TypeExpressionName]);
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

function TBoldObject_Proxy.GetProxedObject: TBoldObject;
begin
  Assert(ProxedElement is TBoldObject);
  result := TBoldObject(ProxedElement);
end;

procedure TBoldSystemLocatorList.UpdateID(Locator: TBoldObjectLocator; NewObjectID: TBoldObjectId; AllowInternal: Boolean);
begin
  if NewObjectID.IsStorable or AllowInternal then
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
    Result := (Member.BoldPersistenceState <> bvpsInvalid) and
              (not Member.Derived) and
              ((Member.BoldMemberRTInfo is TBoldAttributeRTInfo) or
              (TBoldRoleRTInfo(Member.BoldMemberRTInfo).RoleType in [rtRole, rtInnerLinkRole]));
  end;
begin
  if ProxedObject.BoldObjectIsNew or ProxedObject.BoldmemberAssigned[i] then
     Member := ProxedObject.BoldMembers[i]
   else
     Member := nil;

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
  Assert(item is TBoldObjectLocator);
  result := TBoldObjectLocator(item);
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

function TBoldMemberDeriver.GetInternalDeriverState: TBoldDeriverState;
begin
  Assert(DerivedObject is TBoldMember);
  result := TBoldMember(DerivedObject).GetDeriverState;
end;

procedure TBoldMemberDeriver.SetInternalDeriverState(const Value: TBoldDeriverState);
begin
  Assert(DerivedObject is TBoldMember);
  TBoldMember(DerivedObject).DeriverState := Value;
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

(*
procedure TBoldObjectLocator.TypeAtLeast(TopSortedIndex: integer; Exact: Boolean);
var
  OldId: TBoldObjectId;
begin
  if (TopSortedIndex > BoldObjectId.TopSortedIndex) {Topsorted so more exact is always bigger}
    or (Exact and not BoldObjectId.TopSortedIndexExact) then
  begin
    OldId := BoldObjectId;
    fBoldObjectId := OldId.CloneWithClassId(TopSortedIndex, Exact);
    OldId.Free;
  end
end;
*)

function TBoldObjectLocator.GetEmbeddedSingleLinks(EmbeddedIndex: integer): TBoldObjectLocator;
begin
 if Assigned(fEmbeddedSingleLinks) and
   (EmbeddedIndex < fEmbeddedSingleLinks.Count) then
   Result := TBoldObjectLocator(fEmbeddedSingleLinks[EmbeddedIndex])
 else
   Result := nil;
end;

procedure TBoldObjectLocator.SetEmbeddedSingleLinks(EmbeddedIndex: integer; const Value: TBoldObjectLocator);
begin
  if not Assigned(fEmbeddedSingleLinks) then
    fEmbeddedSingleLinks := TBoldObjectArray.Create(EmbeddedIndex + 1, []);
  with fEmbeddedSingleLinks do
  begin
    if EmbeddedIndex >= Count then
      Count := EmbeddedIndex + 1;
    Items[EmbeddedIndex] := Value;
    if Value = nil then {May be able to strip end}
    begin
      while (Count > 0) and (Items[Count - 1] = nil) do
        Count := Count - 1;
      if Count = 0 then
        FreeAndNil(fEmbeddedSingleLinks)
      else if Capacity > Count then
        Capacity := Count;
    end;
  end;
end;

procedure TBoldObjectLocator.EmbeddedSingleLinksFromObject;
var
  m: integer;
  EmbeddedIndex: integer;
  ObjectReferece: TBoldObjectReference;
  RoleRtInfo: TBoldRoleRTInfo;
begin
  for m := 0 to BoldObject.BoldClassTypeInfo.AllMembers.Count - 1 do
    begin
       EmbeddedIndex :=  BoldObject.BoldClassTypeInfo.AllMembers[m].EmbeddedLinkIndex;
       if (EmbeddedIndex <> -1) and BoldObject.BoldMemberAssigned[m] then
       begin
         Assert(BoldObject.BoldClassTypeInfo.AllMembers[m] is TBoldRoleRTInfo);
         Assert(BoldObject.BoldMembers[m] is TBoldObjectReference);
         RoleRtInfo := TBoldRoleRTInfo(BoldObject.BoldClassTypeInfo.AllMembers[m]);
         ObjectReferece := TBoldObjectReference(BoldObject.BoldMembers[m]);
         if (ObjectReferece.BoldPersistenceState = bvpsCurrent) and
           (RoleRtInfo.indexOfOtherEnd <> -1) and  // if other end exists as is loaded, we must keep reference.
           (ObjectReferece.Locator <> nil) and
           (ObjectReferece.Locator.BoldObject <> nil) and
           ObjectReferece.Locator.BoldObject.BoldMemberAssigned[RoleRtInfo.indexOfOtherEnd] then
           EmbeddedSingleLinks[EmbeddedIndex] := ObjectReferece.Locator;
       end;
    end;
end;

procedure TBoldObjectLocator.EmbeddedSingleLinksToObject;
var
  m: integer;
  EmbeddedIndex: integer;
  ObjRef: TBoldObjectReference;
begin
  if Assigned(fEmbeddedSingleLinks) then
  begin
    for m := 0 to BoldObject.BoldClassTypeInfo.AllMembers.Count - 1 do
    begin
      EmbeddedIndex :=  BoldObject.BoldClassTypeInfo.AllMembers[m].EmbeddedLinkIndex;
      if (EmbeddedIndex <> -1) and (EmbeddedSingleLinks[EmbeddedIndex] <> nil) then
      begin
        Assert(BoldObject.BoldMembers[m] is TBoldObjectReference);
        ObjRef := TBoldObjectReference(BoldObject.BoldMembers[m]);
        (ObjRef.AsIBoldValue[bdepInternalInitialize] as IBoldObjectIdRef).SetFromId(EmbeddedSingleLinks[EmbeddedIndex].BoldObjectID);
        assert((ObjRef.AsIBoldValue[bdepContents] as IBoldObjectIdRef).BoldPersistenceState = bvpsInvalid);
//        (ObjRef.AsIBoldValue[bdepInternalInitialize] as IBoldObjectIdRef).BoldPersistenceState := bvpsInvalid;
        ObjRef.HasOldValues := True;
      end;
    end;
    FreeAndNil(fEmbeddedSingleLinks);
  end;
end;

procedure TBoldObjectLocator.FreeEmbeddedSingleLinksOfOtherEnd;
var
  m: integer;
  i:  integer;
  SingleLink: TBoldObjectReference;
  MultiLink: TBoldObjectList;
begin
  for m := 0 to BoldObject.BoldClassTypeInfo.AllMembers.Count - 1 do
  begin
     if BoldObject.BoldClassTypeInfo.AllMembers[m].IsRole and
        (TBoldRoleRTInfo(BoldObject.BoldClassTypeInfo.AllMembers[m]).RoleType = rtLinkRole) and
        BoldObject.BoldMemberAssigned[m] and
        (BoldObject.BoldMembers[m].BoldPersistenceState = bvpsCurrent) then
      begin
        if BoldObject.BoldMembers[m] is TBoldObjectReference then
        begin
          SingleLink := TBoldObjectReference(BoldObject.BoldMembers[m]);
          if Assigned(SingleLink.Locator) and Assigned(SingleLink.Locator.fEmbeddedSingleLinks) then
            SingleLink.Locator.EmbeddedSingleLinks[SingleLink.BoldRoleRTInfo.RoleRTInfoOfOtherEnd.EmbeddedLinkIndex] := nil;
        end
        else
        begin
          multiLink := BoldObject.BoldMembers[m] as TBoldObjectList;
          for i := 0 to Multilink.Count - 1 do
            if Assigned(Multilink.Locators[i].fEmbeddedSingleLinks) then  // FIXME which locator for indirects?
              Multilink.Locators[i].EmbeddedSingleLinks[MultiLink.BoldRoleRTInfo.RoleRTInfoOfOtherEnd.EmbeddedLinkIndex] := nil;
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

procedure TBoldObjectReference_Proxy.AssignContentValue(Source: IBoldValue);
begin
  Assert(ProxedController is TBoldObjectReferenceController);
  if Mode = bdepContents then
    TBoldObjectReferenceController(ProxedController).AssignContentValue(Source)
  else
    UnsupportedMode(Mode, 'AssignContentValue'); // do not localize
end;

function TBoldLocatorIdHashIndex.Hash(const Key): Cardinal;
begin
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
    anObject := ObjectList[i];
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

begin
  result := MayUpdateForAll;
  if result then
    DoStartUpdateForAll;
end;

initialization
  G_ExternalDerivers := TBoldExternalizedReferenceList.Create;
  G_ExternalDerivers.ManageReferencedObject := true;
finalization
  G_ExternalDerivers.Free;
end.



