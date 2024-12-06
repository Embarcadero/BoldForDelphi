{ Global compiler directives }
{$include bold.inc}
unit BoldPMappersDefault;

interface

uses
  Db,
  Classes,
  BoldDefs,
  BoldMeta,
  BoldTypeNameDictionary,
  BoldCondition,
  BoldUpdatePrecondition,
  BoldSQLDatabaseConfig,
  BoldId,
  BoldSQLQuery,
  BoldDBInterfaces,
  BoldValueInterfaces,
  BoldValueSpaceInterfaces,
  BoldTaggedValueSupport,
  BoldPSParams,
  BoldPSDescriptions,
  BoldPSDescriptionsSQL,
  BoldIndexCollection,
  BoldPSDescriptionsDefault,
  BoldSQLMappingInfo,
  BoldPMappers,
  BoldPMappersSQL,
  BoldElements;


const
  MEMBERIDCOLUMN_NAME = 'MEMBER_ID';
  BOLDTIMESTAMPFIELDTYPE = ftInteger;

type
  { forward declarations }
  TBoldSystemDefaultMapper = class;
  TBoldObjectDefaultMapper = class;
  TBoldMemberDefaultMapper = class;
  TBoldModelVersionMember = class;
  EBoldCantGetID = class(EBold);

  TGetNewTimeStampEvent = procedure(out aCurrentTimeStamp: integer; out aLastClockTimestamp: integer; out aLastClock: TDateTime; out aTheNowValue: TDateTime; aClockLogGranularity: TDateTime) of object;
  TIDIncrementEvent = function(aNumberOfIdsToReserve: integer): integer of object;

 { TBoldSystemDefaultMapper }
  TBoldSystemDefaultMapper = class(TBoldSystemSQLMapper)
  private
    fNextDBID: Longint;
    fLastReservedDBID: Longint;
    fReservedCount: Longint;
    fCustomIndexes: TBoldIndexCollection;
    fXFilesTimeStampColumn: TBoldSQLColumnDescription;
    fXFilesGlobalIdColumn: TBoldSQLColumnDescription;
    fTimeStampTableTimeStampColumn: TBoldSQLColumnDescription;
    fLastClockTableLastTimeStampColumn: TBoldSQLColumnDescription;
    fLastClockTableLastClockColumn: TBoldSQLColumnDescription;
    fClockLogTableLastTimeStampColumn: TBoldSQLColumnDescription;
    fClockLogTableThisTimeStampColumn: TBoldSQLColumnDescription;
    fClockLogTableThisClockColumn: TBoldSQLColumnDescription;
    fClockLogTableLastClockColumn: TBoldSQLColumnDescription;
    fNewTimeStampEvent: TGetNewTimeStampEvent;
    fIDIncrementEvent: TIDIncrementEvent;
    fCompatibilityMode: boolean;
    function GetPSSystemDescription: TBoldDefaultSystemDescription;
    function GetRootClassObjectPersistenceMapper: TBoldObjectDefaultMapper;
    procedure GetChangePointsQuery(Query: IBoldQuery; IdList: TBoldObjectIdList; StartTime: TBoldTimestampType; EndTime: TBoldTimestampType; NameSpace: TBoldSqlnameSpace);
  protected
    function CreatePSParams: TBoldPSParams; override;
    function CreatePSSystemDescription: TBoldPSSystemDescription; override;
    procedure ReserveID; override;
    property NextDBID: Longint read fNextDBID write fNextDBID;
    property LastReservedDBID: Longint read fLastReservedDBID write fLastReservedDBID;
    property ReservedCount: Longint read fReservedCount write fReservedCount;
    function NewGlobalIdFromQuery(aQuery: IBoldQuery; BoldDbTypeColumn: Integer): TBoldObjectId;
    procedure GetNewTimeStamp; override;
    procedure FetchDeletedObjects(ObjectIdList: TBoldObjectIdList; const ValueSpace: IBoldValueSpace); override;
    procedure InitializeBoldDbType; override;
    function CreateMappingInfo: TBoldSQLMappingInfo; override;
    procedure InitializePSDescriptions; override;
    function EnsurePrecondition(Precondition: TBoldUpdatePrecondition; TranslationList: TBoldIdTranslationList): Boolean; override;
    function EnsureOptimisticLocking(Precondition: TBoldOptimisticLockingPrecondition; TranslationList: TBoldIdTranslationList): Boolean;
  public
    constructor CreateFromMold(moldModel: TMoldModel; TypeNameDictionary: TBoldTypeNameDictionary;
      CustomIndexes: TBoldIndexCollection; SQlDatabaseConfig: TBoldSQLDatabaseConfig; GetDatabaseFunc: TBoldGetDatabaseEvent);
    procedure PMFetchClassWithCondition(ObjectIDList: TBoldObjectIdList;
                                        const ValueSpace: IBoldValueSpace;
                                        BoldCondition: TBoldCondition;
                                        FetchMode: Integer;
                                        TranslationList: TBoldIdTranslationList); override;
    function GetListUsingQuery(ObjectIDList: TBoldObjectIdList; const ValueSpace: IBoldValueSpace; const aQuery: IBoldQuery; ClassId, BoldDbTypeColumn, ObjectIdColumn: integer; FetchMode: Integer; TranslationList: TBoldIdTranslationList; TimeStamp: TBoldTimeStampType; MaxAnswers: integer = -1; Offset: integer = -1): integer;
    function EnsureTable(const TableName: string; TableVersioned: Boolean): TBoldSQLTableDescription; override;
    function EnsureColumn(const TableName, ColumnName, SQLType, SQLAllowNull: string; const BDEType: TFieldType; Length: Integer; const AllowNull, InVersionedTable: Boolean; const DefaultDBValue: String): TBoldSQLColumnDescription;
    procedure EnsureIndex(const TableName, Fields: string; const PrimaryIndex,
        Unique, NonClustered, InVersionedTable: Boolean);
    procedure PMTranslateToGlobalIds(ObjectIdList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList); override;
    procedure PMTranslateToLocalIds(GlobalIdList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList); override;
    procedure PMSetReadonlyness(ReadOnlyList, WriteableList: TBoldObjectIdList); override;
    procedure PMTimestampForTime(ClockTime: TDateTime; var Timestamp: TBoldTimestampType); override;
    procedure PMTimeForTimestamp(Timestamp: TBoldTimestampType; var ClockTime: TDateTime); override;
    function NewIdFromQuery(const aQuery: IBoldQuery; ClassId, BoldDbTypeColumn, ObjectIdColumn: integer; Timestamp: TBoldTimeStampType): TBoldObjectId;
    function CanEvaluateInPS(sOCL: string; aSystem: TBoldElement;  aContext: TBoldElementTypeInfo = nil; const aVariableList: TBoldExternalVariableList = nil): Boolean; override;
    property PSSystemDescription: TBoldDefaultSystemDescription read GetPSSystemDescription;
    property RootClassObjectPersistenceMapper: TBoldObjectDefaultMapper read GetRootClassObjectPersistenceMapper;
    property XFilesTimeStampColumn: TBoldSQLColumnDescription read fXFilesTimeStampColumn;
    property XFilesGlobalIdColumn: TBoldSQLColumnDescription read fXFilesGlobalIdColumn;
    property LastClockTableLastTimeStampColumn: TBoldSQLColumnDescription read fLastClockTableLastTimeStampColumn;
    property LastClockTableLastClockColumn: TBoldSQLColumnDescription read fLastClockTableLastClockColumn;
    property ClockLogTableLastTimeStampColumn: TBoldSQLColumnDescription read fClockLogTableLastTimeStampColumn;
    property ClockLogTableThisTimeStampColumn: TBoldSQLColumnDescription read fClockLogTableThisTimeStampColumn;
    property ClockLogTableLastClockColumn: TBoldSQLColumnDescription read fClockLogTableLastClockColumn;
    property ClockLogTableThisClockColumn: TBoldSQLColumnDescription read fClockLogTableThisClockColumn;
    property TimeStampTableTimeStampColumn: TBoldSQLColumnDescription read fTimeStampTableTimeStampColumn;
    property NewTimeStampEvent: TGetNewTimeStampEvent read fNewTimeStampEvent write fNewTimeStampEvent;
    property IDIncrementEvent: TIDIncrementEvent read fIDIncrementEvent write fIDIncrementEvent;
    property CompatibilityMode: boolean read fCompatibilityMode write fCompatibilityMode;
  end;

  TQueryCacheEntry = record
    MemberList: TBoldMemberIdList;
    SqlStrings: TStringList;
    FetchMode: Integer;
    MemberPMList: TBoldMemberPersistenceMapperList;
    CustomMembers: TBoldMemberPersistenceMapperList;
  end;

  TPMCreateCacheEntry = record
    SqlStrings: TStringList;
    MemberPMList: TBoldMemberPersistenceMapperList;
  end;

  TQueryCache = array of TQueryCacheEntry;
  TPMCreateCache = array of TPMCreateCacheEntry;

  { TBoldObjectDefaultMapper }
  TBoldObjectDefaultMapper = class(TBoldObjectSQLMapper)
  private
    fSubClassesID: string;
    fModelVersionMember: TBoldModelVersionMember;
    fOptimisticLockingMode: TBoldOptimisticLockingMode;
    fQueryCache: TQueryCache;
    fPMCreateCache: TPMCreateCache;
    fSingleLinkList: TBoldMemberIdList;
    function GetSystemPersistenceMapper: TBoldSystemDefaultMapper;
    procedure PMUpdateStopTime(ObjectIDList: TBoldObjectIdList);
    procedure GetChangePoints(ObjectIDList: TBoldObjectIdList; Condition: TBoldChangePointCondition; NameSpace: TBoldSqlnameSpace);
    procedure PMMultiPurposeRetrieveExactIdList(ObjectsToFetch: TBoldObjectIdList; const ValueSpace: IBoldValueSpace; MemberIdList: TBoldMemberIdList; FetchMode: Integer; TranslationList: TBoldIdTranslationList; MissingList: TBoldObjectIdList; FailureList: TBoldObjectIdList; TimeStamp: TBoldTimeStampType);
    function FindInCache(MemberIdList: TBoldMemberIdList; FetchMode: integer; var MemberPMList, CustomMembers: TBoldMemberPersistenceMapperList; var ASql: TStringList): boolean;
    procedure HandleCompareData(FetchedId: TBoldObjectId; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; const Query: IBoldQuery; MemberPMList: TBoldMemberPersistenceMapperList; FetchMode: integer; FailureList: TBoldObjectIdList);
    function CompareFieldsToMembers(ObjectID: TBoldObjectId; const ValueSpace: IBoldValueSpace; const DataSet: IBoldDataSet; memberList: TBoldMemberPersistenceMapperList; TranslationList: TBoldIdTranslationList): Boolean;
    procedure DetectLinkClassDuplicates(ObjectIdList: TBoldObjectidList; const ValueSpace: IBoldvalueSpace; TranslationList: TBoldIdTranslationList; DuplicateList: TBoldObjectIdList);
    procedure GenerateMappingInfo(ExpressionName: String; MoldClass: TMoldClass);
    procedure PMTemporalUpdate(ObjectIdList: TBoldObjectIdList; const ValueSpace: IBoldvalueSpace; TranslationList: TBoldIdTranslationList);
    procedure FetchPreviousSingleLinkValues(ObjectIdList: TBoldObjectIdLIst; const Old_Values: IBoldvalueSpace);
    procedure MakeIDsExactUsingTable(ObjectIDList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList; Table: TBoldSQLTableDescription; EnsureAll: Boolean; HandleNonExisting: Boolean);
    function InternalIdListSegmentToWhereFragment(IdList: TBoldObjectIdList; Start, Stop: Integer; AllowParms: Boolean; const Parameterized: IBoldParameterized): String;
    procedure InternalMakeIDsExact(ObjectIDList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList; EnsureAll: Boolean; HandleNonExisting: Boolean);
    procedure FetchRawSqlCondition(ObjectIDList: TBoldObjectIdList; const ValueSpace: IBoldValueSpace; RawCondition: TBoldRawSqlCondition; FetchMode: Integer; TranslationList: TBoldIdTranslationList);
  protected
    procedure JoinSQLTableByKey(SQL: TStringList; MainTable, JoinTable: TBoldSQLTableDescription); override;
    procedure SQLForID(Table: TBoldSQLTableDescription; SQL: TStrings; UseAlias: Boolean); override;
    procedure SQLForDistributed(SQL: TStrings; const SQLStyle: TBoldSQLStyle); override;
    procedure PMFetchWithCondition(ObjectIDList: TBoldObjectIdList;
      const ValueSpace: IBoldValueSpace; BoldCondition: TBoldCondition; FetchMode: Integer; TranslationList: TBoldIdTranslationList); override;
    function NextExternalObjectId(const ValueSpace: IBoldValueSpace; ObjectId: TBoldObjectId): TBoldObjectId; override;
    function DistributableTable: TBoldSQLTableDescription; override;
    procedure PMCompareExactIDList(ObjectIDList: TBoldObjectIdList; const ValueSpace: IBoldValueSpace; MemberIdList: TBoldMemberIdList; TranslationList: TBoldIdTranslationList; FailureList: TBoldObjectIdList);
    procedure InitializePSDescriptions; override;
    procedure FillInMembers(MyMoldClass, CurrentMoldClass: TMoldClass; TypeNameDictionary: TBoldTypeNameDictionary); override;
  public
    constructor CreateFromMold(moldClass: TMoldClass; Owner: TBoldSystemPersistenceMapper; TypeNameDictionary: TBoldTypeNameDictionary); override;
    destructor Destroy; override;
    procedure SQLForKey(Table: TBoldSQLTableDescription; SQL: TStrings; const SQLStyle: TBoldSQLStyle; useAlias: Boolean); override;
    function UpdatesMembersInTable(aTable: TBoldSQLTableDescription): Boolean; override;
    function IdListSegmentToWhereFragment(IdList: TBoldObjectIdList; Start, Stop: Integer; AllowParms: Boolean; const Query: IBoldExecQuery): String; overload;
    function IdListSegmentToWhereFragment(IdList: TBoldObjectIdList; Start, Stop: Integer; AllowParms: Boolean; const Query: IBoldQuery): String; overload;
    procedure MakeIDsExact(ObjectIDList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList; HandleNonExisting: Boolean); override;
    procedure PMFetchExactIDList(ObjectIDList: TBoldObjectIdList; const ValueSpace: IBoldValueSpace; MemberIdList: TBoldMemberIdList; FetchMode: Integer; TranslationList: TBoldIdTranslationList; MissingList: TBoldObjectIdList); override;
    procedure PMDelete(ObjectIDList: TBoldObjectIdList; const ValueSpace: IBoldValueSpace; const Old_Values: IBoldValueSpace; TranslationList: TBoldIdTranslationList); override;
    procedure PMCreate(ObjectIDList: TBoldObjectIdList; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList); override;
    procedure PMUpdate(ObjectIDList: TBoldObjectIdList; const ValueSpace: IBoldValueSpace; const Old_Values: IBoldValueSpace; TranslationList: TBoldIdTranslationList); override;
    procedure DistributableInfoFromQuery(ObjectID: TBoldObjectId; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; const DataSet: IBoldDataSet);
    procedure HandleFetchData(FetchedId: TBoldObjectId; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; const Query: IBoldQuery; MemberPMList: TBoldMemberPersistenceMapperList; FetchMode: integer; FailureList: TBoldObjectIdList);
    procedure PortObject(ObjectId: TBoldObjectId; const Query: IBoldQuery);
    function IsOldVersion(const Query: IBoldQuery): Boolean;
    property SubClassesID: string read fSubClassesID write fSubClassesId;
    property SystemPersistenceMapper: TBoldSystemDefaultMapper read GetSystemPersistenceMapper;
    property ModelVersionMember: TBoldModelVersionMember read fModelVersionMember;
  end;

  { TBoldMemberDefaultMapper }
  TBoldMemberDefaultMapper = class(TBoldMemberSQLMapper)
  private
    procedure GenerateMappingInfo(MoldClass: TMoldClass; MoldMember: TMoldMember);
    function GetSystemPersistenceMapper: TBoldSystemDefaultMapper;
    function GetObjectPersistenceMapper: TBoldObjectDefaultMapper;
  protected
    FColumnIndex: Boolean;
    function CheckEitherNull(const field: IBoldField; const Value: IBoldValue; var Equal: Boolean): Boolean;
    function GetAllowNullAsSQL: string; override;
    procedure GetChangePoints(ObjectIDList: TBoldObjectIdList; Condition: TBoldChangePointCondition; NameSpace: TBoldSqlnameSpace); virtual;
    function CompareFields(const ObjectContent: IBoldObjectContents; const DataSet: IBoldDataSet; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList): Boolean;
    function CompareField(const ObjectContent: IBoldObjectContents; const Field: IBoldField; ColumnIndex: integer; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList): Boolean; virtual; abstract;
    procedure InitializePSDescriptions; override;
    function RequiresMemberMapping: Boolean; virtual;
    function FindDefiningTable(LocalMoldClass: TMoldClass; MoldMember: TMoldMember): string; virtual;
    function SupportsComparingWithoutValue: Boolean; virtual;
  public
    constructor CreateFromMold(moldMember: TMoldMember; moldClass: TMoldClass; Owner: TBoldObjectPersistenceMapper; const MemberIndex: Integer; TypeNameDictionary: TBoldTypeNameDictionary); override;
    procedure PMFetch(ObjectIdList: TBoldObjectIdList;
                   const ValueSpace: IBoldValueSpace;
                   FetchMode: Integer;
                   TranslationList: TBoldIdTranslationList; FailureList: TBoldObjectIdList); override;
    property ObjectPersistenceMapper: TBoldObjectDefaultMapper read GetObjectPersistenceMapper;
    property SystemPersistenceMapper: TBoldSystemDefaultMapper read GetSystemPersistenceMapper;
  end;

  {TBoldSingleColumnMember}
  TBoldSingleColumnMember = class(TBoldMemberDefaultMapper)
  protected
    function GetColumnCount: Integer; override;
    function GetColumnSize(ColumnIndex: Integer): Integer; override;
  public
    class procedure EnsureFirstColumn(ColumnIndex: Integer);
  end;

  {TBoldModelVersionMember}
  TBoldModelVersionMember = class(TBoldSingleColumnMember)
  private
    fVersionNumber: Integer;
  protected
    function GetColumnTypeAsSQL(ColumnIndex: Integer): string; override;
    function GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType; override;
    function CompareField(const ObjectContent: IBoldObjectContents; const Field: IBoldField; ColumnIndex: integer;const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList): Boolean; override;
    function FindDefiningTable(LocalMoldClass: TMoldClass; MoldMember: TMoldMember): string; override;
  public
    constructor CreateFromMold(moldMember: TMoldMember; moldClass: TMoldClass; Owner: TBoldObjectPersistenceMapper; const MemberIndex: Integer; TypeNameDictionary: TBoldTypeNameDictionary); override;
    function IsDirty(const ObjectContents: IBoldObjectContents): Boolean; override;
    function ShouldFetch(const ObjectContents: IBoldObjectContents): Boolean; override;
    procedure ValueToParam(const ObjectContent: IBoldObjectContents; const Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList); override;
    procedure ValueFromField(OwningObjectId: TBoldObjectId; const ObjectContent: IBoldObjectContents; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; const Field: IBoldField; ColumnIndex: Integer); override;
    function ValueAsVariant(const ObjectContent: IBoldObjectContents; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList): variant; override;
    function VersionFromQuery(const Query: IBoldQuery): Integer;
    property VersionNumber: Integer read fVersionNumber write fVersionNumber;
  end;

  { TBoldReadOnlynessMember }
  TBoldReadOnlynessMember = class(TBoldSingleColumnMember)
  protected
    function GetColumnTypeAsSQL(ColumnIndex: Integer): string; override;
    function GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType; override;
    function CompareField(const ObjectContent: IBoldObjectContents; const Field: IBoldField; ColumnIndex: integer;const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList): Boolean; override;
    function FindDefiningTable(LocalMoldClass: TMoldClass; MoldMember: TMoldMember): string; override;
  public
    constructor CreateFromMold(moldMember: TMoldMember; moldClass: TMoldClass; Owner: TBoldObjectPersistenceMapper; const MemberIndex: Integer; TypeNameDictionary: TBoldTypeNameDictionary); override;
    function IsDirty(const ObjectContents: IBoldObjectContents): Boolean; override;
    function ShouldFetch(const ObjectContents: IBoldObjectContents): Boolean; override;
    procedure ValueToParam(const ObjectContent: IBoldObjectContents; const Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList); override;
    procedure ValueFromField(OwningObjectId: TBoldObjectId; const ObjectContent: IBoldObjectContents; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; const Field: IBoldField; ColumnIndex: Integer); override;
    function ValueAsVariant(const ObjectContent: IBoldObjectContents; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList): variant; override;
  end;

  { TBoldNonXFileTimeStampMember }
  TBoldNonXFileTimeStampMember = class(TBoldSingleColumnMember)
  protected
    function GetColumnTypeAsSQL(ColumnIndex: Integer): string; override;
    function GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType; override;
    function CompareField(const ObjectContent: IBoldObjectContents; const Field: IBoldField; ColumnIndex: integer;const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList): Boolean; override;
    function FindDefiningTable(LocalMoldClass: TMoldClass; MoldMember: TMoldMember): string; override;
    function SupportsComparingWithoutValue: Boolean; override;
  public
    constructor CreateFromMold(moldMember: TMoldMember; moldClass: TMoldClass; Owner: TBoldObjectPersistenceMapper; const MemberIndex: Integer; TypeNameDictionary: TBoldTypeNameDictionary); override;
    function IsDirty(const ObjectContents: IBoldObjectContents): Boolean; override;
    function ShouldFetch(const ObjectContents: IBoldObjectContents): Boolean; override;
    procedure ValueToParam(const ObjectContent: IBoldObjectContents; const Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList); override;
    procedure ValueFromField(OwningObjectId: TBoldObjectId; const ObjectContent: IBoldObjectContents; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; const Field: IBoldField; ColumnIndex: Integer); override;
    function ValueAsVariant(const ObjectContent: IBoldObjectContents; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList): variant; override;
  end;

  TBoldXFilesMembers = class(TBoldSingleColumnMember)
  protected
    procedure InitializePSDescriptions; override;
    function RequiresMemberMapping: Boolean; override;
  end;

  { TBoldTimeStampMember }
  TBoldTimeStampMember = class(TBoldXFilesMembers)
  protected
    function GetColumnTypeAsSQL(ColumnIndex: Integer): string; override;
    function GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType; override;
    function CompareField(const ObjectContent: IBoldObjectContents; const Field: IBoldField; ColumnIndex: integer;const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList): Boolean; override;
    function FindDefiningTable(LocalMoldClass: TMoldClass; MoldMember: TMoldMember): string; override;
    function SupportsComparingWithoutValue: Boolean; override;
  public
    constructor CreateFromMold(moldMember: TMoldMember; moldClass: TMoldClass; Owner: TBoldObjectPersistenceMapper; const MemberIndex: Integer; TypeNameDictionary: TBoldTypeNameDictionary); override;
    function IsDirty(const ObjectContents: IBoldObjectContents): Boolean; override;
    function ShouldFetch(const ObjectContents: IBoldObjectContents): Boolean; override;
    procedure ValueToParam(const ObjectContent: IBoldObjectContents; const Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList); override;
    procedure ValueFromField(OwningObjectId: TBoldObjectId; const ObjectContent: IBoldObjectContents; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; const Field: IBoldField; ColumnIndex: Integer); override;
    function ValueAsVariant(const ObjectContent: IBoldObjectContents; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList): variant; override;
  end;

  { TBoldTimeStampMember }
  TBoldGlobalIdMember = class(TBoldXFilesMembers)
  protected
    function GetColumnTypeAsSQL(ColumnIndex: Integer): string; override;
    function GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType; override;
    function CompareField(const ObjectContent: IBoldObjectContents; const Field: IBoldField; ColumnIndex: integer;const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList): Boolean; override;
    function FindDefiningTable(LocalMoldClass: TMoldClass; MoldMember: TMoldMember): string; override;
  public
    constructor CreateFromMold(moldMember: TMoldMember; moldClass: TMoldClass; Owner: TBoldObjectPersistenceMapper; const MemberIndex: Integer; TypeNameDictionary: TBoldTypeNameDictionary); override;
    function IsDirty(const ObjectContents: IBoldObjectContents): Boolean; override;
    function ShouldFetch(const ObjectContents: IBoldObjectContents): Boolean; override;
    procedure ValueToParam(const ObjectContent: IBoldObjectContents; const Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList); override;
    procedure ValueFromField(OwningObjectId: TBoldObjectId; const ObjectContent: IBoldObjectContents; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; const Field: IBoldField; ColumnIndex: Integer); override;
    function ValueAsVariant(const ObjectContent: IBoldObjectContents; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList): variant; override;
  end;


  TExternalIdGenerator = function: string;

  EBoldOptimisticLockingFailed = class(EBoldOperationFailedForIdList)
  public
    constructor Create(msg: string; args: array of const; FailedObjects: TBoldObjectIdList);
  end;

var
  ExternalIDGenerator: TExternalIdGenerator;


implementation

uses
  SysUtils,

  BoldCoreConsts,
  BoldUtils,
  BoldSubscription,
  BoldNameExpander,
  BoldLogHandler,
  BoldDefaultId,
  BoldPMapperLists,
  BoldPSParamsDefault,
  BoldMappingInfo,
  BoldMath,
  BoldGlobalId,
  BoldPMappersLinkDefault,
  BoldOclLightWeightNodes,
  BoldSqlNodeMaker,
  BoldSqlNodes,
  BoldSqlNodesResolver,
  BoldSqlQueryGenerator,
  BoldGUIDUtils,
  BoldGuard,
  {$IFDEF RIL}
  {$IFNDEF BOLD_UNICODE}
  StringBuilder,
  {$ENDIF}
  {$ENDIF}
  BoldDefaultStreamNames,
  BoldOCL,
  BoldOclLightWeightNodeMaker,
  BoldOCLClasses,
  BoldSystem,
  BoldSystemRT,
  BoldContainers,
  BoldIndex,
  BoldIndexableList;

const
  TIMESTAMPMEMBERINDEX = -2;

{--Supporting functions/procedures---}

function TBoldSystemDefaultMapper.GetPSSystemDescription: TBoldDefaultSystemDescription;
begin
  result := (inherited PSSystemDescription) as TBoldDefaultSystemDescription;
end;

function TBoldObjectDefaultMapper.GetSystemPersistenceMapper: TBoldSystemDefaultMapper;
begin
  result := (inherited SystemPersistenceMapper) as TBoldSystemDefaultMapper;
end;

function TBoldSystemDefaultMapper.GetRootClassObjectPersistenceMapper: TBoldObjectDefaultMapper;
begin
  result := (inherited RootClassObjectPersistenceMapper) as TBoldObjectDefaultMapper;
end;

function TBoldMemberDefaultMapper.GetSystemPersistenceMapper: TBoldSystemDefaultMapper;
begin
  result := inherited SystemPersistenceMapper as TBoldSystemDefaultMapper;
end;

function TBoldMemberDefaultMapper.GetObjectPersistenceMapper: TBoldObjectDefaultMapper;
begin
  result := (inherited ObjectPersistenceMapper) as TBoldObjectDefaultMapper;
end;

{ TBoldSystemDefaultMapper }

function TBoldSystemDefaultMapper.CanEvaluateInPS(sOCL: string;
  aSystem: TBoldElement; aContext: TBoldElementTypeInfo;
  const aVariableList: TBoldExternalVariableList): Boolean;
var
  i: Integer;
  aResultEntry: TBoldOclEntry;
  aEnv: TBoldOclEnvironment;
  aOLWNodeMaker: TBoldOLWNodeMaker;
  aOCLCondition: TBoldOCLCondition;
  aSQLNodeResolver: TBoldSqlNodeResolver;
  aSQLNodeMaker: TBoldSQLNodeMaker;
  aBoldSystem: TBoldSystem;
  aBoldOCL: TBoldOCL;
  aResultType: TBoldElementTypeInfo;
  aClassTypeInfo: TBoldClassTypeInfo;
  aGlobalNameSpace: TBoldSqlNameSpace;
  aSQlGenerator: TBoldSQLQueryGenerator;
  aVariableIDLists: TBoldObjectArray;
  aSystemTypeInfo: TBoldSystemTypeInfo;

  procedure FixQueriesForEnv(VarBinding: TBoldSQLVariableBinding; Context: TBoldObjectIdList; NameSpace: TBoldSqlNameSpace);
  var
    MainTableRef: TBoldSqlTableReference;
    BoldID: TBoldDefaultID;
  begin
    if CompareText(VarBinding.VariableName, 'SELF') = 0 then // do not localize
    begin
      VarBinding.NewQuery(NameSpace);

      MainTableRef := VarBinding.TableReferenceForTable(VarBinding.ObjectMapper.MainTable, VarBinding.Query, true);
      VarBinding.Context := aOclCondition.Context;
      VarBinding.Query.AddWCF(TBoldSQLWCFBinaryInfix.CreateWCFForIdList(MainTableRef.GetColumnReference(IDCOLUMN_NAME), aOclCondition.Context));
    end else if VarBinding.IsExternal and (VarBinding.TopSortedIndex > -1) then
    begin
      VarBinding.NewQuery(NameSpace);

      MainTableRef := VarBinding.TableReferenceForTable(VarBinding.ObjectMapper.MainTable, VarBinding.Query, true);
      VarBinding.Context := TBoldObjectIdList.Create;
      aVariableIDLists.Add(VarBinding.Context);
      BoldID := TBoldDefaultID.CreateWithClassID(VarBinding.ObjectMapper.TopSortedIndex, True);
      BoldID.AsInteger := VarBinding.ExternalVarvalue;
      VarBinding.Context.Add(BoldID);
      VarBinding.Query.AddWCF(TBoldSQLWCFBinaryInfix.CreateWCFForIdList(MainTableRef.GetColumnReference(IDCOLUMN_NAME), VarBinding.Context));
    end;
  end;

begin
  Result := False;
  // Let all objects point to nil, so there are no problems on free
  aBoldSystem := nil;
  aBoldOCL := nil;
  aResultEntry := nil;
  aEnv := nil;
  aOLWNodeMaker := nil;
  aOCLCondition := nil;
  aSQlNodeResolver := nil;
  aSQLNodeMaker := nil;
  aSQLGenerator := nil;
  aGlobalNameSpace := nil;

  // On empty OCL a PS evaluation is unnecessary (objects are loaded already).
  // System parameter must be type of TBoldSystem (see TBoldPersistenceController).
  // Also OCL evaluator must be type of TBoldOCL, otherwise validation is not possible.
  if (sOCL = '') {or not ((aSystem is TBoldSystem) and
     (TBoldSystem(aSystem).Evaluator is TBoldOCL))} then
  begin
    Exit;
  end;
  if not Assigned(aSystem) and not Assigned(aContext) then
    raise EBold.Create('CanEvaluateInPS requires system or context.');
  // Validation does not work with collection as context, though evaluation
  // would be possible. Therefore always use ListElementTypeInfo.
  if Assigned(aContext) then
  begin
    if (aContext is TBoldListTypeInfo) then
      aContext := TBoldListTypeInfo(aContext).ListElementTypeInfo
    else
      aBoldOCL := TBoldOcl(aContext.Evaluator);
    aSystemTypeInfo := aContext.SystemTypeInfo as TBoldSystemTypeInfo;
  end
  else
  begin
    aBoldOCL := TBoldOcl(aSystem.Evaluator);
    aSystemTypeInfo := aSystem.BoldType as TBoldSystemTypeInfo;
  end;
  aVariableIDLists := TBoldObjectArray.Create(0, [bcoDataOwner]);
  Result := True;
  try
    try
      aEnv := TBoldOclEnvironment.Create(aBoldOCL.GlobalEnv);
      // OCL semantic check
      aResultEntry := aBoldOCL.SemanticCheck(sOCL, aContext, aEnv, aVariableList);
      aOLWNodeMaker := TBoldOLWNodeMaker.Create(aResultEntry.Ocl, aSystemTypeInfo as TBoldSystemTypeInfo, aBoldSystem, aEnv);
      aResultEntry.Ocl.AcceptVisitor(aOLWNodeMaker);
      // Can OCL be evaluated in PS in general?
      if not aOLWNodeMaker.Failed then begin
        aOCLCondition := TBoldOclCondition.Create;
        aOCLCondition.OclExpr := sOCL;

        for i := 0 to aOLWNodeMaker.ExternalVarBindings.Count - 1 do
          aOCLCondition.Env.Add(TBoldOLWVariableBinding(aOLWNodeMaker.ExternalVarBindings[i]));
        aOLWNodeMaker.ExternalVarBindings.Clear;

        aOCLCondition.RootNode := aOLWNodeMaker.RootNode;

        aResultType := aResultEntry.Ocl.BoldType;
        if aResultType is TBoldListTypeInfo then begin
          aClassTypeInfo := TBoldListTypeInfo(aResultType).ListElementTypeInfo as TBoldClassTypeInfo;
        end else begin
          aClassTypeInfo := aResultType as TBoldClassTypeInfo;
        end;
        aOCLCondition.TopSortedIndex := aClassTypeInfo.TopSortedIndex;

        // Can all parts of OCLs be translated to SQL symbols?
        aSQLNodeMaker := TBoldSQLNodeMaker.Create(aOCLCondition);
        aSQLNodeMaker.Execute;

        aSQLNodeResolver := TBoldSqlNodeResolver.Create(Self, aSQLNodeMaker.RootNode, aSQLNodeMaker.SQLVarBindings);
        aSQLNodeResolver.Execute;
        // Finally the real check, which checks every symbol (AcceptVisitor).
        aGlobalNameSpace := TBoldSqlnameSpace.Create;

        aSQlGenerator := TBoldSqlQueryGenerator.Create(aGlobalNameSpace);
        for i := 0 to aSQLNodeMaker.SQLVarBindings.Count - 1 do begin
          aSQLNodeMaker.SQLVarBindings[i].AcceptVisitor(aSQlGenerator);
          FixQueriesForEnv(aSQLNodeMaker.SQLVarBindings[i] as TBoldSqlVariableBinding, aOclCondition.Context, aGlobalNameSpace);
        end;

        aSQLNodeMaker.RootNode.AcceptVisitor(aSQlGenerator);
      end else begin
        Result := False;
        SetBoldLastFailureReason(TBoldFailureReason.CreateFmt('%s at position: %d', [aOLWNodeMaker.FailureReason, aOLWNodeMaker.FailurePosition], nil));
      end;
    except
      on E:Exception do
      begin
        SetBoldLastFailureReason(TBoldFailureReason.Create(E.Message, nil));
        Result := False;
        // do not raise exception, instead create failure reason and return false
      end;
    end;
  finally
    if Assigned(aResultEntry) then begin
      if aResultEntry.OwnedByDictionary then begin
        aResultEntry.UsedByOtherEvaluation := false
      end else begin
        aResultEntry.Free;
      end;
    end;
    aEnv.Free;
    aOLWNodeMaker.Free;
    aOCLCondition.Free;
    aSQLGenerator.Free;
    aSQlNodeResolver.Free;
    aGlobalNameSpace.Free;
    aSQLNodeMaker.Free;
    aVariableIDLists.Free;
  end;
end;

constructor TBoldSystemDefaultMapper.CreateFromMold(moldModel: TMoldModel; TypeNameDictionary: TBoldTypeNameDictionary;
  CustomIndexes: TBoldIndexCollection; SQlDatabaseConfig: TBoldSQLDatabaseConfig; GetDatabaseFunc: TBoldGetDatabaseEvent);
begin
  inherited;
  NextDBID := -1;
  LastReservedDBID := -2;
  ReservedCount := 0;
  fCustomIndexes := CustomIndexes;
end;


procedure TBoldSystemDefaultMapper.ReserveID;
begin
  Inc(fReservedCount);
end;

function TBoldSystemDefaultMapper.CreatePSSystemDescription: TBoldPSSystemDescription;
begin
  Result := TBoldDefaultSystemDescription.Create(nil, SQLDataBaseConfig, NationalCharConversion);
end;

{procedure TBoldSystemDefaultMapper.FillPSParams(PSParams: TBoldPSParams);
var
  I: Integer;
  om: TBoldObjectPersistenceMapper;
begin
  inherited FillPSParams(PSParams);
  with PSParams as TBoldPSDefaultParams do
    for I := 0 to ObjectPersistenceMappers.Count - 1 do
    begin
      om := ObjectPersistenceMappers[I];
      if Assigned(om) then
      begin
        AddTypeMapping(i, ObjectPersistenceMappers[I].ExpressionName);
        // in case anyone uses the PMapper after a CreateDB (like OLLE), better initialize the real property as well
        (ObjectPersistenceMappers[I] as TBoldObjectDefaultMapper).BoldDbType := i;
      end;
    end;
end;
}

function TBoldSystemDefaultMapper.CreatePSParams: TBoldPSParams;
begin
  Result := TBoldPSDefaultParams.Create;
end;

function TBoldSystemDefaultMapper.EnsureTable(const TableName: string; TableVersioned: Boolean): TBoldSQLTableDescription;
begin
  Result := PSSystemDescription.SQLTablesList.ItemsBySQLName[TableName];
  if not assigned(Result) then
  begin
    Result := TBoldSQLTableDescription.Create(PSSystemDescription, TableVersioned);
    with Result as TBoldSQLTableDescription do
    begin
      SQLName := TableName;
      AddColumn(IDCOLUMN_NAME, SQLDataBaseConfig.ColumnTypeForInteger, SQLDataBaseConfig.EffectiveSQLforNotNull, IDCOLUMN_TYPE, 0, False, '');
      AddColumn(TYPECOLUMN_NAME, SQLDataBaseConfig.ColumnTypeForSmallInt, SQLDataBaseConfig.EffectiveSQLforNotNull, TYPECOLUMN_TYPE, 0, False, '');
      if TableVersioned then
      begin
        AddColumn(TIMESTAMPSTARTCOLUMNNAME, SQLDataBaseConfig.ColumnTypeForInteger, SQLDataBaseConfig.EffectiveSQLforNotNull, BOLDTIMESTAMPFIELDTYPE, 0, false, SQLDataBaseConfig.CorrectlyQuotedDefaultValue('0'));
        EnsureIndex(IDCOLUMN_NAME + ';' + TIMESTAMPSTARTCOLUMNNAME, True, True, False);
        // the following two indices improves performance alot in Interbase, and seems to have no negative impact in SQLServer.
        EnsureIndex(TIMESTAMPSTARTCOLUMNNAME, false, false, false);
        EnsureIndex(IDCOLUMN_NAME, false, false, false);
      end
      else
        EnsureIndex(IDCOLUMN_NAME, True, True, false);

      EnsureIndex(TYPECOLUMN_NAME, False, False, false);
    end;
    Result := Result;
  end;
end;

function TBoldSystemDefaultMapper.EnsureColumn(const TableName, ColumnName, SQLType, SQLAllowNull: string;
    const BDEType: TFieldType; Length: Integer;
    const AllowNull, InVersionedTable: Boolean;
    const DefaultDBValue: String): TBoldSQLColumnDescription;
var
  Table: TBoldSQLTableDescription;
begin
  EnsureTable(TableName, InversionedTable);
  Table := PSSystemDescription.SQLTablesList.ItemsBySQLName[TableName];
  Result := Table.ColumnsList.ItemsBySQLName[ColumnName] as TBoldSQLColumnDescription;
  if not assigned(Result) then
    Result := Table.AddColumn(ColumnName, SQLType,  SQLAllowNull, BDEType, Length, AllowNull, DefaultDbValue)
  else
  begin
    Result.Size         := Length;
    Result.Mandatory    := not AllowNull;
    Result.SQLType      := SQLType;
    Result.FieldType    := BDEType;
    Result.SQLAllowNull := SQLAllowNull;
    result.DefaultDBValue := DefaultDBValue;
  end;
end;

procedure TBoldSystemDefaultMapper.EnsureIndex(const TableName, Fields: string;
    const PrimaryIndex, Unique, NonClustered, InVersionedTable: Boolean);
var
  Table: TBoldSQLTableDescription;
begin
  EnsureTable(TableName, InVersionedTable);
  Table := PSSystemDescription.SQLTablesList.ItemsBySQLName[TableName];
  Table.EnsureIndex(Fields, PrimaryIndex, Unique, NonClustered);
end;

function TBoldSystemDefaultMapper.NewIdFromQuery(const aQuery: IBoldQuery; ClassId, BoldDbTypeColumn, ObjectIdColumn: integer; Timestamp: TBoldTimeStampType): TBoldObjectId;
var
  ObjectId: TBoldDefaultId;
  TopSortedIndex: integer;
begin
  if BoldDbTypeColumn = -1 then
    TopSortedIndex := ClassId
  else
    TopSortedIndex := topSortedIndexForBoldDbType(aQuery.Fields[BoldDbTypeColumn].AsInteger);

  if TimeStamp <> BoldMaxTimeStamp then
    ObjectId := TBoldTimestampedDefaultId.createWithTimeAndClassId(TimeStamp, TopSortedIndex, BoldDbTypeColumn <> -1)
  else
    ObjectId := TBoldDefaultId.CreateWithClassId(TopSortedIndex, BoldDbTypeColumn <> -1);

  ObjectId.AsInteger := aQuery.Fields[ObjectIdColumn].AsInteger;
  result := ObjectId;
end;

function TBoldSystemDefaultMapper.GetListUsingQuery(ObjectIDList: TBoldObjectIdList; const ValueSpace: IBoldValueSpace; const aQuery: IBoldQuery; ClassId, BoldDbTypeColumn, ObjectIdColumn: integer; FetchMode: Integer; TranslationList: TBoldIdTranslationList; TimeStamp: TBoldTimeStampType; MaxAnswers: integer = -1; Offset: integer = -1): integer;
var
  ObjectId: TBoldObjectId;
  Counter: integer;
  RecordsToProcess: integer;
begin
  Result := 0;
  aQuery.Open;
  if offset <> -1 then
  begin
    RecordsToProcess := aQuery.RecordCount - Offset {- aQuery.RecNo};
    if Offset <> 0 then
      aQuery.MoveBy(Offset);
  end
  else
    RecordsToProcess := aQuery.RecordCount {- aQuery.RecNo};
// when MaxAnswer = -1 the while-test will never occur and we will get all the answers
  Counter := MaxAnswers;

  if (MaxAnswers <> -1) and (MaxAnswers < RecordsToProcess) then
    RecordsToProcess := MaxAnswers;
  ObjectIDList.Capacity := ObjectIDList.Count + RecordsToProcess;
  TranslationList.Capacity := TranslationList.Count + RecordsToProcess;

  while not aQuery.EOF and (Counter <> 0) do
  begin
    if TimeStamp = BOLDINVALIDTIMESTAMP then
      ObjectId := NewIdFromQuery(aQuery, ClassId, BoldDbTypeColumn, ObjectIdColumn, aQuery.FieldByUpperCaseName(TIMESTAMPSTARTCOLUMNNAMEUPPER).AsInteger)
    else
      ObjectId := NewIdFromQuery(aQuery, ClassId, BoldDbTypeColumn, ObjectIdColumn, TimeStamp);
    ValueSpace.EnsureObjectId(TranslationList.TranslateToNewId[ObjectId]); // is Translation here really needed ?
    ObjectIDList.Add(ObjectId);
    INC(Result);
    TranslationList.AddTranslationAdoptNew(nil, ObjectId);
    SendExtendedEvent(bpeFetchId, [ObjectId]);
    aQuery.Next;
    dec(Counter);
  end;
//  SendExtendedEvent(bpeFetchId, [ObjectIDList]);
{  if (MaxAnswers < 0)  or (Counter > 0) then
    result := MaxAnswers - Counter
  else
    result := aQuery.RecordCount;}
end;

function TBoldSystemDefaultMapper.EnsurePrecondition(Precondition: TBoldUpdatePrecondition; TranslationList: TBoldIdTranslationList): Boolean;
begin
  if Precondition is TBoldOptimisticLockingPrecondition then
  begin
    result := EnsureOptimisticLocking(PreCondition as TBoldOptimisticLockingPrecondition, translationList);
    // this is an optimization in the case of remote pmappers so that we do not have to stream the valuespace back again.
    // another solutiuon would be to spearate the in and out-parameters better...
    (PreCondition as TBoldOptimisticLockingPrecondition).ClearValueSpace;
  end
  else
    result := true;
end;

function TBoldSystemDefaultMapper.EnsureOptimisticLocking(Precondition: TBoldOptimisticLockingPrecondition; TranslationList: TBoldIdTranslationList): Boolean;

procedure MergeFailures(MasterList, NewList: TBoldObjectIdList);
var
  i: integer;
begin
  for i := 0 to NewLIst.Count - 1 do
    Precondition.AddFailedObject(Newlist[i]);
end;

function ContainsMemberId(MemberIdList: TBoldMemberIdList; MemberIndex: integer): boolean;
var
  i: integer;
begin
  result := false;
  for I := 0 to MemberIdList.Count - 1 do
  begin
    if MemberIdList[i].MemberIndex = MemberIndex then
    begin
      result := true;
      exit;
    end;
  end;
end;

var
  OldMemberIdList: TBoldMemberIdList;
  SingleClassList: TBoldObjectIdList;
  XFileTimeStampedObjects: TBoldObjectIdList;
  ObjectContents: IBoldObjectContents;
  Value: IBoldValue;
  ObjectIdList: TBoldObjectIdList;
  MemberIx, i: integer;
  ObjectMapper: TBoldObjectDefaultmapper;
  FailureList: TBoldObjectIdList;
  BoldGuard: IBoldGuard;
  TopSortedIndex: integer;
  MemberCount: integer;
  FetchBlockSize: integer;
begin
  BoldGuard := TBoldGuard.Create(XFileTimeStampedObjects, ObjectIdList, FailureList,
                                 SingleClassList, OldMemberIdList);
  OldMemberIdList := TBoldMemberIdList.Create;
  SingleClassList := TBoldObjectIdList.Create;
  XFileTimeStampedObjects := TBoldObjectIdList.Create;
  ObjectIdList := TBoldObjectIdList.Create;
  FailureList := TBoldObjectIdList.Create;
  FetchBlockSize := SQLDataBaseConfig.FetchBlockSize;

  Precondition.valueSpace.AllObjectIds(ObjectidList, true);

  for i := ObjectIdList.Count - 1 downto 0 do
  begin
    ObjectContents := Precondition.ValueSpace.ObjectContentsByObjectId[ObjectIdList[i]];
    // remove objects with no ObjectContents
    if not assigned(ObjectContents) then
      ObjectIdList.RemoveByIndex(i);
  end;

  while ObjectIdList.count > 0 do
  begin
    TopSortedIndex := ObjectIdList[ObjectIdList.count-1].TopSortedIndex;
    SingleClassList.Clear;
    SingleClassList.Add(ObjectIdList[ObjectIdList.count-1]);
    ObjectIdList.RemoveByIndex(ObjectIdList.count-1);
    for I := ObjectIdList.Count - 1 downto 0 do
    begin
      // collect objects of same class in SingleClassList
      if TopSortedIndex = ObjectIdList[i].TopSortedIndex then
      begin
        SingleClassList.Add(ObjectIdList[i]);
        ObjectIdList.RemoveByIndex(i);
        if SingleClassList.count = FetchBlockSize then
          break;
      end;
    end;
    OldMemberIdList.Clear;

    // process one object
    ObjectContents := Precondition.ValueSpace.ObjectContentsByObjectId[SingleClassList[SingleClassList.count-1]];
    if UseTimeStamp and (ObjectContents.TimeStamp <> -1) then
      if UseXFiles then
        XFileTimeStampedObjects.AddIfNotInList(SingleClassList[SingleClassList.count-1])
      else
        OldMemberIdList.Add(TBoldMemberId.Create(TIMESTAMPMEMBERINDEX));

    for MemberIx := 0 to ObjectContents.MemberCount - 1 do
    begin
      Value := ObjectContents.ValueByIndex[MemberIx];
      if assigned(Value) then
      begin
        OldMemberIdList.Add(TBoldMemberId.Create(MemberIx));
      end;
    end;
    // loop and compare other objects, if they need exact same members as first object
    // then keep them in SingleClassList and fetch them together
    // otherwise return the object to ObjectIdList to be processed in the next pass
    for i := SingleClassList.count - 2 downto 0 do // -2 is on purpose to skip the object we processed above
    begin
      ObjectContents := Precondition.ValueSpace.ObjectContentsByObjectId[SingleClassList[i]];
      MemberCount := 0;
      begin
        if UseTimeStamp and (ObjectContents.TimeStamp <> -1) then
        begin
          if UseXFiles then
            XFileTimeStampedObjects.AddIfNotInList(SingleClassList[i])
          else
          begin
            if not ContainsMemberId(OldMemberIdList, TIMESTAMPMEMBERINDEX) then
            begin // put the object back in the ObjectIdList
              ObjectIdList.Add(SingleClassList[i]);
              SingleClassList.RemoveByIndex(i);
              continue;
            end
            else
              Inc(MemberCount);
          end;
        end;
        for MemberIx := 0 to ObjectContents.MemberCount - 1 do
        begin
          Value := ObjectContents.ValueByIndex[MemberIx];
          if assigned(Value) then
          begin
            if not ContainsMemberId(OldMemberIdList, MemberIx) then
            begin  // set MemberCount to number we're sure won't match so it will be removed
              MemberCount := MaxInt;
              break;
            end
            else
              Inc(MemberCount);
          end;
        end;
        // now also make sure the OldMemberIdList doesn't contain more members
        if MemberCount <> OldMemberIdList.Count  then
        begin  // put the object back in the ObjectIdList
          ObjectIdList.Add(SingleClassList[i]);
          SingleClassList.RemoveByIndex(i);
        end
      end
    end;

    // we must compare the object even if it has no dirty members
    // since it might be a delete and the object in the db might
    // be deleted already. if it is timestamped however, we will
    // detect it more cheap that way
    if (OldMemberIdList.Count > 0) or (ObjectContents.TimeStamp = -1) then
    begin
      failureList.Clear;
      ObjectMapper := ObjectPersistenceMappers[TopSortedIndex] as TBoldObjectDefaultMapper;
      Objectmapper.PMCompareExactIDList(SingleClassList, Precondition.ValueSpace, OldMemberIdList, translationList, FailureList);
      MergeFailures(precondition.FailureList, FailureList);
    end;
  end;
  if XFileTimeStampedObjects.Count > 0 then
  begin
    OldMemberIdList.clear;
    FailureList.Clear;
    OldMemberIdList.Add(TBoldMemberId.Create(TIMESTAMPMEMBERINDEX));
    RootClassObjectPersistenceMapper.PMCompareExactIDList(XFileTimeStampedObjects, Precondition.ValueSpace, OldMemberIdList, translationlist, FailureList);
    if FailureList.Count > 0 then
    begin
      BoldLog.Log(sOptimisticLockingFailedOnTimeStamp);
      for i := 0 to FailureList.Count - 1 do
        BoldLog.LogFmt(sOptimisticLockFailedLog,
          [
            ObjectPersistenceMappers[FailureList[i].TopSortedIndex].ExpressionName,
            FailureList[i].AsString
          ]);
      MergeFailures(precondition.FailureList, FailureList);
    end;
  end;

  result := Precondition.FailureList.Count = 0;
end;

{ TBoldObjectDefaultMapper }

procedure TBoldObjectDefaultMapper.GenerateMappingInfo(ExpressionName: String; MoldClass: TMoldClass);

  procedure AddAllInstancesMappingRecursion(RecMoldClass: TMoldClass);
  var
    i: integer;
    RecTableName: String;
  begin
    if RecMoldClass.TableMapping in [tmOwn] then
    begin
      RecTableName := BoldExpandPrefix(RecMoldClass.TableName, RecMoldClass.Name, SystemPersistenceMapper.SQLDataBaseConfig.SystemTablePrefix, SystemPersistenceMapper.SQLDataBaseConfig.MaxDbIdentifierLength, MoldClass.Model.NationalCharConversion);
      SystemPersistenceMapper.MappingInfo.AddAllInstancesMapping(ExpressionName, RecTableName, false)
    end
    else if RecMoldClass.TableMapping in [tmChildren] then
      for i := 0 to RecMoldClass.SubClasses.Count - 1 do
        if RecMoldClass.SubClasses[i].EffectivePersistent then
          AddAllInstancesMappingRecursion(RecMoldClass.SubClasses[i]);
  end;
var
  TempSuperClass: TMoldClass;
  TableName: String;
  ParentMappings: TBoldAllInstancesMappingArray;
begin
  SetLength(ParentMappings, 0);
  // Db types mapping

  SystemPersistenceMapper.MappingInfo.AddTypeIdMapping(ExpressionName, MoldClass.TopSortedIndex);

  // write info in AllInstancesmappingTable

  if MoldClass.TableMapping = tmOwn then
  begin
    TableName := BoldExpandPrefix(MoldClass.TableName, MoldClass.Name, SystemPersistenceMapper.SQLDataBaseConfig.SystemTablePrefix, SystemPersistenceMapper.SQLDataBaseConfig.MaxDbIdentifierLength, MoldClass.Model.NationalCharConversion);
    SystemPersistenceMapper.MappingInfo.AddAllInstancesMapping(ExpressionName, TableName, false)
  end
  else if MoldClass.TableMapping = tmParent then
  begin
    ParentMappings := SystemPersistenceMapper.MappingInfo.GetAllInstancesMapping(moldClass.SuperClass.ExpandedExpressionName);
    SystemPersistenceMapper.MappingInfo.AddAllInstancesMapping(ExpressionName,
      ParentMappings[0].TableName, true);
  end
  else if MoldClass.TableMapping = tmChildren then
    AddAllInstancesMappingRecursion(MoldClass);

  // write info in ObjectStorageMappingTable

  if not MoldClass.IsAbstract then
  begin
    tempSuperClass := MoldClass.SuperClass;
    while assigned(TempSuperClass) do
    begin
      if TempSuperClass.TableMapping = tmOwn then
      begin
        ParentMappings := SystemPersistenceMapper.MappingInfo.GetAllInstancesMapping(TempSuperClass.ExpandedExpressionName);
        SystemPersistenceMapper.MappingInfo.AddObjectStorageMapping(ExpressionName, ParentMappings[0].TableName);
      end;
      TempSuperClass := TempSuperClass.SuperClass;
    end;
  end;
end;

constructor TBoldObjectDefaultMapper.CreateFromMold(moldClass: TMoldClass; Owner: TBoldSystemPersistenceMapper; TypeNameDictionary: TBoldTypeNameDictionary);
begin
  inherited;
  fOptimisticLockingMode := MoldClass.EffectiveOptimisticLocking;
end;

type
  TLittleClass = class
    Id: integer;
    dbType: TBoldDbType;
  end;

procedure TBoldObjectDefaultMapper.InternalMakeIDsExact(ObjectIDList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList; EnsureAll: Boolean; HandleNonExisting: Boolean);
var
  i: integer;
  ObjectMapper: TBoldObjectDefaultMapper;
begin
  if assigned(MainTable) then
    MakeIDsExactUsingTable(ObjectIDList, TranslationList, MainTable, not SystemPersistenceMapper.UseXFiles, HandleNonExisting)
  else
  begin
    for i := 0 to SystemPersistenceMapper.ObjectPersistenceMappers.Count-1 do
    begin
      ObjectMapper := SystemPersistenceMapper.ObjectPersistenceMappers[i] as TBoldObjectDefaultMapper;
      if assigned(ObjectMapper) and (ObjectMapper.SuperClass = self) then
        ObjectMapper.InternalMakeIDsExact(objectidlist, TranslationList, not SystemPersistenceMapper.UseXFiles, HandleNonExisting);
    end;
  end;
end;

procedure TBoldObjectDefaultMapper.MakeIDsExact(ObjectIDList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList; HandleNonExisting: Boolean);
var
  i: Integer;
  MissingIds: TBoldObjectIdList;
begin
  if ObjectIDList.Count = 0 then
    exit;
  MissingIds := TBoldObjectIdList.Create;
  try
    MakeIDsExactUsingTable(ObjectIDList, TranslationList, MainTable, not SystemPersistenceMapper.UseXFiles, HandleNonExisting);
    if SystemPersistenceMapper.UseXFiles then
    begin
      for i := 0 to ObjectIDList.Count-1 do
        if TranslationList.TranslateToNewId[ObjectIdList[i]] = ObjectIdList[i] then
          MissingIds.Add(ObjectIdList[i]);
      if MissingIds.Count > 0 then
        MakeIDsExactUsingTable(MissingIds, TranslationList, SystemPersistenceMapper.PSSystemDescription.XFilestable, true, HandleNonExisting);
    end;
  finally
    MissingIds.Free;
  end;
end;

procedure TBoldObjectDefaultMapper.SQLForID(Table: TBoldSQLTableDescription; SQL: TStrings; useAlias: Boolean);
begin
  SQL.Append(Format('%s.%s', [Tablealias(Table, useAlias), IDCOLUMN_NAME]))
end;

procedure TBoldObjectDefaultMapper.SQLForKey(Table: TBoldSQLTableDescription; SQL: TStrings; const SQLStyle: TBoldSQLStyle; useAlias: Boolean);
var
  tableQualifier,
  ColumnString: string;
begin
  tableQualifier := '';
  if (Table = MainTable) and useAlias then
    tableQualifier := TableAlias(Table, useALias) + '.';

  case SQLStyle of
    ssColumns   : ColumnString := tableQualifier;
    ssParameters: ColumnString := ':' + tableQualifier;
  end;

  SQL.Append(ColumnString+Table.ColumnsList[0].SQLName);
  SQL.Append(ColumnString+Table.ColumnsList[1].SQLName);
end;

procedure TBoldObjectDefaultMapper.JoinSQLTableByKey(SQL: TStringList; MainTable, JoinTable: TBoldSQLTableDescription);
{$IFDEF RIL}
var
  SB: TStringBuilder;
begin
    SB := TStringBuilder.Create();
  //SQL.Append(Format('%s.%s = %s.%s', [TableAlias(JoinTable, True), IDCOLUMN_NAME, TableAlias(MainTable, True), IDCOLUMN_NAME]));
    SB.Append(TableAlias(JoinTable, True));
    SB.Append('.');
    SB.Append(IDCOLUMN_NAME);
    SB.Append(' = ');
    SB.Append(TableAlias(MainTable, True));
    SB.Append('.');
    SB.Append(IDCOLUMN_NAME);
  SQL.Append(SB.Tostring);
  if Versioned then
  begin
    // SQL.Append(Format('%s.%s = %s.%s', [TableAlias(JoinTable, True), TIMESTAMPSTARTCOLUMNNAME, TableAlias(MainTable, True), TIMESTAMPSTARTCOLUMNNAME]));
      SB.Clear;
      SB.Append(TableAlias(JoinTable, True));
      SB.Append('.');
      SB.Append(TIMESTAMPSTARTCOLUMNNAME);
      SB.Append(' = ');
      SB.Append(TableAlias(MainTable, True));
      SB.Append('.');
      SB.Append(TIMESTAMPSTARTCOLUMNNAME);
    SQL.Append(SB.Tostring);
  end;
  FreeAndNil(SB);
end;
{$ELSE}
begin
  SQL.Append(Format('%s.%s = %s.%s', [TableAlias(JoinTable, True), IDCOLUMN_NAME, TableAlias(MainTable, True), IDCOLUMN_NAME]));
  if Versioned then
    SQL.Append(Format('%s.%s = %s.%s', [TableAlias(JoinTable, True), TIMESTAMPSTARTCOLUMNNAME, TableAlias(MainTable, True), TIMESTAMPSTARTCOLUMNNAME]));
end;
{$ENDIF}

procedure TBoldObjectDefaultMapper.PMUpdateStopTime(ObjectIDList: TBoldObjectIdList);
var
  i: integer;
  UpdateQuery: IBoldExecQuery;
  IdStringList: TStringlist;
begin
  IdStringlist := TStringList.Create;
  UpdateQuery := SystemPersistenceMapper.GetExecQuery;
  try
    for i := 0 to ObjectIdList.Count - 1 do
      IdStringList.Add(ObjectIdList[i].AsString);

    UpdateQuery.AssignSQLText(format('UPDATE %s SET %s = %d WHERE %s in (%s) AND (%s = %d)',
      [SystemPersistenceMapper.RootClassObjectPersistenceMapper.MainTable.SQLName,
       TIMESTAMPSTOPCOLUMNNAME,
       SystemPersistenceMapper.CurrentTimeStamp - 1,
       IDCOLUMN_NAME,
       BoldSeparateStringList(IdStringList, ',', '', ''),
       TIMESTAMPSTOPCOLUMNNAME,
       BOLDMAXTIMESTAMP]));

    UpdateQuery.ExecSQL;

  finally
    SystemPersistenceMapper.ReleaseExecQuery(UpdateQuery);
    IdStringlist.Free;
  end;
end;

procedure TBoldObjectDefaultMapper.PMCreate(ObjectIDList: TBoldObjectIdList; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList);

var
  aQuery: IBoldExecQuery;
  Row: Integer;
  SQL: TStringList;

  procedure ExecuteQuery;
  begin
    aQuery.Params.EndUpdate;
    aQuery.SQLStrings.EndUpdate;
//    aQuery.ParamCheck := true;
    aQuery.ExecSQL;
    aQuery.Params.BeginUpdate;
    aQuery.SQLStrings.BeginUpdate;
//    aQuery.ParamCheck := false;
    aQuery.Params.Clear;
    aQuery.AssignSQL(SQL);
    Row := 0;
  end;
var
{$IFDEF RIL}
  SB: TStringBuilder;
{$ENDIF}
  I, T, A: Integer;
  TickCounter: integer;
  TempList: TStringList;
  MemberPMList: TBoldMemberPersistenceMapperList;
  MemberPMapper: TBoldMemberDefaultMapper;
  NewID: TBoldObjectId;
  DuplicateList: TBoldObjectIdList;
  IdColumnParam: IBoldParameter;
  TypeColumnParam: IBoldParameter;
  BoldGuard: IBoldGuard;
  FoundInCache: boolean;
  StoreInCache: boolean;
  UseParams: boolean;
  Limit: integer;
begin
  BoldGuard := TBoldGuard.Create({MemberPMList}{$IFDEF RIL}SB{$ENDIF},TempList);
  {$IFDEF RIL}
  SB := TStringBuilder.Create;
  {$ENDIF}
  Tickcounter := 0;
  if IsLinkClass and (not Versioned) then
  begin
    DuplicateList := TBoldObjectidList.Create;
    try
      DetectLinkClassDuplicates(ObjectIdList, ValueSpace, TranslationList, DuplicateList);
      if DuplicateList.Count <> 0 then
      begin
        PMUpdate(DuplicateList, ValueSpace, nil, translationList);
        for i := 0 to DuplicateList.Count - 1 do
          ObjectIdList.RemoveByIndex(ObjectIdList.IndexByID[DuplicateList[i]]);
      end;
    finally
      DuplicateList.Free;
    end;
    if ObjectIdList.Count = 0 then
      exit;
  end;
  Limit := SystemPersistenceMapper.SQLDataBaseConfig.MultiRowInsertLimit;
  UseParams := SystemPersistenceMapper.SQLDataBaseConfig.UseParamsForInteger;
  aQuery := SystemPersistenceMapper.GetExecQuery;
  aQuery.ParamCheck := true;
  TempList := TStringList.Create;
  aQuery.SQLStrings.BeginUpdate;
  aQuery.Params.BeginUpdate;
  aQuery.Params.clear;
  FoundInCache := useParams and (Limit = 1) and (Length(fPMCreateCache) > 0);
  try
    for T := 0 to AllTables.Count - 1 do
    begin
      StoreInCache := false;
      if not FoundInCache then
      begin
        MemberPMList := TBoldMemberPersistenceMapperList.Create;
        MemberPMList.OwnsEntries := False;
        aQuery.ClearParams;
        for A := 0 to MemberPersistenceMappers.Count - 1 do
        begin
          MemberPMapper := MemberPersistenceMappers[A] as TBoldMemberDefaultMapper;
          if assigned(MemberPMapper) and
            MemberPMapper.IsStoredInObject and not MemberPMapper.CustomCreateUpDate and
            ((MemberPMapper.ColumnDescriptions[0] as TBoldSQLColumnDescription).TableDescription = AllTables[T]) then
            MemberPMList.Add(MemberPMapper);
        end;
        SQL := TStringList.Create;
        TempList.clear;
        SQLForMembers(AllTables[T], TempList, MemberPMList, ssColumns, True, False, False);

        if Alltables[T].Versioned then
        begin
          TempList.Add(TIMESTAMPSTARTCOLUMNNAME);
          if allTables[T].ContainsStopTimeStamp then
            TempList.Add(TIMESTAMPSTOPCOLUMNNAME);
        end;

        {$IFDEF RIL}
        //BoldAppendToStrings(SQL, Format('INSERT INTO %s (%s) ', [AllTables[T].SQLName, BoldSeparateStringList(TempLIst, ', ', '', '')]), True);
          SB.Clear;
          SB.Append('INSERT INTO ');
          SB.Append(AllTables[T].SQLName);
          SB.Append(' (');
          SB.Append(BoldSeparateStringList(TempLIst, ', ', '', ''));
          SB.Append(') ');
        BoldAppendToStrings(SQL, SB.ToString, True);
        {$ELSE}
        BoldAppendToStrings(SQL, Format('INSERT INTO %s (%s) ', [AllTables[T].SQLName,
                                                                 BoldSeparateStringList(TempList, ', ', '', '')]), True);
        {$ENDIF}

        if UseParams then
        begin
          TempList.Clear;
          SQLForMembers(AllTables[T], TempList, MemberPMList, ssParameters, True, False, False);
        end;

        if Alltables[T].Versioned then
        begin
          TempList.Add(':' + TIMESTAMPSTARTCOLUMNNAME);
          if allTables[T].ContainsStopTimeStamp then
            TempList.Add(':' + TIMESTAMPSTOPCOLUMNNAME);
        end;

        {$IFDEF RIL}
        SB.Clear;
        SB.Append('VALUES ');
        if UseParams then
        begin
          SB.Append('(');
          SB.Append(BoldSeparateStringList(TempLIst, ', ', '', ''));
          SB.Append(') ');
        end;
        BoldAppendToStrings(SQL, SB.ToString, True);

        {$ELSE}
        BoldAppendToStrings(SQL, Format('VALUES (%s) ', [BoldSeparateStringList(TempLIst, ', ', '', '')]), True);
        {$ENDIF}
        // store in cache
        StoreInCache := UseParams and (Limit = 1);
        if StoreInCache then
        begin
          i := Length(fPMCreateCache);
          SetLength(fPMCreateCache, i+1);
          fPMCreateCache[i].SqlStrings := Sql;
          fPMCreateCache[i].MemberPMList := MemberPMList;
        end;
      end
      else
      begin
        SQL := fPMCreateCache[T].SqlStrings;
        MemberPMList := fPMCreateCache[T].MemberPMList;
      end;
      aQuery.ClearParams;
      aQuery.AssignSQL(SQL);
      Row := 0;
      SB.Clear;
      for I := 0 to ObjectIDList.Count-1 do
      begin
        NewID := ObjectIDList[I];
        if TranslationList.count > 0 then
        begin
          if (i<TranslationList.count) and TranslationList.OldIds[i].IsEqual[ObjectIDList[I]] then
            NewID := TranslationList.NewIds[i]
          else
            NewID := TranslationList.TranslateToNewID[ObjectIDList[I]];
        end;
        if UseParams then
        begin
          IdColumnParam := aQuery.CreateParam(ftInteger, IDCOLUMN_NAME);
          TypeColumnParam := aQuery.CreateParam(ftSmallInt, TYPECOLUMN_NAME);
          IdColumnParam.AsInteger := (NewId as TBoldDefaultId).AsInteger;
          TypeColumnParam.AsSmallInt := SystemPersistenceMapper.BoldDbTypeForTopSortedIndex(NewId.topSortedIndex)
        end
        else
        begin
          SB.Append( Format('(%s,%d', [NewId.AsString, SystemPersistenceMapper.BoldDbTypeForTopSortedIndex(NewId.topSortedIndex)]) );
        end;
        TempList.Clear;
        ValuesToQueryByMemberList(ObjectIDList[I], ValueSpace, aQuery, TempList, MemberPMList, TranslationList, dsmCreate);
        SB.Append(TempList.text);
        SB.Replace(#13#10, '');
        if Alltables[T].Versioned then
        begin
          if versioned then
          begin
            aQuery.EnsureParamByName(TIMESTAMPSTARTCOLUMNNAME).AsInteger := SystemPersistenceMapper.CurrentTimeStamp;
            SB.Append(',:'+TIMESTAMPSTARTCOLUMNNAME);
          end
          else
          begin
            aQuery.EnsureParamByName(TIMESTAMPSTARTCOLUMNNAME).AsInteger := 0;
            SB.Append(',:'+TIMESTAMPSTARTCOLUMNNAME);
          end;

          if allTables[T].ContainsStopTimeStamp then
          begin
            aQuery.EnsureParamByName(TIMESTAMPSTOPCOLUMNNAME).AsInteger := BOLDMAXTIMESTAMP;
            SB.Append(',:'+TIMESTAMPSTOPCOLUMNNAME);
          end;
        end;
        Inc(TickCounter);
        if (TickCounter MOD AllTables.Count) = 0 then
        begin
          SystemPersistenceMapper.SendExtendedEvent(bpeCreateObject, [ObjectIdList[i], ValueSpace]);
          TickCounter := 0;
        end;
        inc(Row);
        if UseParams or (i = ObjectIDList.Count-1) or (Row = Limit) or (aQuery.Params.Count + aQuery.BatchQueryParamCount >= SystemPersistenceMapper.SQLDataBaseConfig.MaxBatchQueryParams) then
        begin
          if not UseParams then
            SB.Append(')');
          aQuery.SQLStrings.Add(SB.ToString);
          SB.Clear;
          ExecuteQuery;
        end
        else
        if not UseParams then
        begin
          SB.Append('),');
        end;
      end;
      if not StoreInCache then
      begin
        MemberPMList.free;
        SQl.free;
      end;
    end;
  finally
    aQuery.SQLStrings.Clear;
    aQuery.SQLStrings.EndUpdate;
    aQuery.Params.Clear;
    aQuery.Params.EndUpdate;
    SystemPersistenceMapper.ReleaseExecQuery(aQuery);
  end;
  for A := 0 to MemberPersistenceMappers.Count - 1 do
  begin
    with MemberPersistenceMappers[A] do
      if assigned(MemberPersistenceMappers[A]) and
        IsStoredInObject and
        CustomCreateUpDate then
        PMCreate(ObjectIDList, ValueSpace, TranslationList);
  end;
end;

procedure TBoldObjectDefaultMapper.PMTemporalUpdate(ObjectIdList: TBoldObjectIdList; const ValueSpace: IBoldvalueSpace; TranslationList: TBoldIdTranslationList);
var
  aQuery: IBoldExecQuery;
  OldDataQuery: IBoldQuery;
  SQL: TStringList;
  TickCounter, TypeColumnIndex, IdColumnIndex, i, T, A: integer;
  NewId: TBoldObjectId;
  TempList: TStringLIst;
  MemberPMapper: TBoldMemberDefaultMapper;
  MemberPMList: TBoldMemberPersistenceMapperList;
begin
  TickCounter := 0;
  PMUpdateStopTime(ObjectIdList);

  MemberPMList := TBoldMemberPersistenceMapperList.Create;
  MemberPMList.OwnsEntries := false;
  aQuery := SystemPersistenceMapper.GetExecQuery;
  OldDataQuery := SystemPersistenceMapper.GetQuery;
  TempList := TStringList.Create;
  SQL := TStringList.Create;
  try
    for T := 0 to AllTables.Count - 1 do
    begin
      if Alltables[T] <> SystemPersistenceMapper.PSSystemDescription.XFilestable then
      begin
        MemberPMList.Clear;

        for A := 0 to MemberPersistenceMappers.Count - 1 do
        begin
          MemberPMapper := MemberPersistenceMappers[A] as TBoldMemberDefaultMapper;
          if assigned(MemberPMapper) and
            MemberPMapper.IsStoredInObject and not MemberPMapper.CustomCreateUpDate and
            ((MemberPMapper.ColumnDescriptions[0] as TBoldSQLColumnDescription).TableDescription = AllTables[T]) then
            MemberPMList.Add(MemberPMapper);
        end;

        SQL.Clear;
        TempList.Clear;

        for i := 0 to ObjectidList.Count - 1 do
          TempList.Add(ObjectIdLIst[i].AsString);

        if AllTables[T].ContainsStopTimeStamp then
          OldDataQuery.AssignSQLText(format('SELECT * FROM %s WHERE (%s in %s) AND (%s = %d)', [
            Alltables[T].SQlname, IDCOLUMN_NAME,
            BoldSeparateStringlist(TempList, ', ', '(', ')'),
            TIMESTAMPSTOPCOLUMNNAME,
            SystemPersistenceMapper.CurrentTimeStamp - 1]))
        else
          OldDataQuery.AssignSQLText(format(
             'SELECT DataTable.* FROM %0:s DataTable, %1:s RootTable '+
             'WHERE (DataTable.%2:s in %3:s) AND '+
               '(DataTable.%2:s = RootTable.%2:s) AND '+
               '(DataTable.%4:s = RootTable.%4:s) AND '+
               '(RootTable.%5:s = %6:d)', [
            Alltables[T].SQlname,
            SystemPersistenceMapper.RootClassObjectPersistenceMapper.MainTable.SQLName,
            IDCOLUMN_NAME,
            BoldSeparateStringlist(TempList, ', ', '(', ')'),
            TIMESTAMPSTARTCOLUMNNAME,
            TIMESTAMPSTOPCOLUMNNAME,
            SystemPersistenceMapper.CurrentTimeStamp - 1]));

        OldDataQuery.Open;
        TempList.Clear;

        for i := 0 to OldDataQuery.FieldCount - 1 do
          TempList.Add(OldDataQuery.Fields[i].FieldName);

        BoldAppendToStrings(SQL, Format('INSERT INTO %s (%s) ', [AllTables[T].SQLName,
                                                                    BoldSeparateStringList(TempLIst, ', ', '', '')]), True);

        TempList.Clear;
        IdColumnIndex := -1;
        TypeColumnIndex := -1;
        for i := 0 to OldDataQuery.FieldCount - 1 do
        begin
          TempList.Add(':' + OldDataQuery.Fields[i].FieldName);
          if SameText(OldDataQuery.Fields[i].FieldName, IDCOLUMN_NAME) then
            IdColumnIndex := i;
          if SameText(OldDataQuery.Fields[i].FieldName, TYPECOLUMN_NAME) then
            TypeColumnIndex := i;
        end;

        BoldAppendToStrings(SQL, Format('VALUES (%s) ', [BoldSeparateStringList(TempLIst, ', ', '', '')]), True);

        if (IdColumnIndex = -1) or (TypeColumnIndex = -1) then
          raise EBoldInternal.CreateFmt(sTypeAndIDColumnMissing, [classname, aQuery.SQLText]);


        aQuery.AssignSQL(SQL);
        aQuery.ParamCheck := true;
        while not OldDataQuery.Eof do
        begin
          NewId := SystemPersistenceMapper.NewIdFromQuery(OldDataQuery, NO_CLASS, TypeColumnIndex, IdColumnIndex, BOLDMAXTIMESTAMP);
          try
            for i := 0 to OldDataQuery.FieldCount - 1 do
              aQuery.ParamByName(OldDataQuery.Fields[i].FieldName).AssignFieldValue(OldDataQuery.Fields[i]);
            ValuesToParamsByMemberList(NewId, ValueSpace, aQuery, MemberPMList, TranslationList, dsmUpdate);
            aQuery.ParamByName(TIMESTAMPSTARTCOLUMNNAME).AsInteger := SystemPersistenceMapper.CurrentTimeStamp;
            if allTables[T].ContainsStopTimeStamp then
              aQuery.ParamByName(TIMESTAMPSTOPCOLUMNNAME).AsInteger := BOLDMAXTIMESTAMP;

            Inc(TickCounter);
            if (TickCounter mod AllTables.Count) = 0 then
            begin
              SystemPersistenceMapper.SendExtendedEvent(bpeCreateObject, [NewId, ValueSpace]);
              TickCounter := 0;
            end;
            aQuery.ExecSQL;
            OldDataQuery.Next;
          finally
            NewId.Free;
          end;
        end;
        OldDataQuery.Close;
      end;
    end;
    for A := 0 to MemberPersistenceMappers.Count - 1 do
    begin
      with MemberPersistenceMappers[A] do
        if assigned(MemberPersistenceMappers[A]) and
          IsStoredInObject and
          CustomCreateUpDate then
          PMCreate(ObjectIDList, ValueSpace, TranslationList);
    end;
  finally
    TempLIst.Free;
    MemberPMList.Free;
    OldDataQuery.Close;
    SystemPersistenceMapper.ReleaseExecQuery(aQuery);
    SystemPersistenceMapper.ReleaseQuery(OldDataQuery);
    SQL.Free;
  end;
end;

procedure TBoldObjectDefaultMapper.PMUpdate(ObjectIDList: TBoldObjectIdList; const ValueSpace: IBoldValueSpace; const Old_Values: IBoldValueSpace; TranslationList: TBoldIdTranslationList);
var
  aQuery: IBoldExecQuery;
  I, T, A: Integer;
  TempLIst: TStringList;
  MemberPMList: TBoldMemberPersistenceMapperList;
  MemberPMapper: TBoldMemberDefaultMapper;
  ObjectContents: IBoldObjectContents;
  sql: TStringList;
  UseParams: boolean;
  BoldGuard: IBoldGuard;
begin
  BoldGuard := TBoldGuard.Create(MemberPMList, SQL, TempList);
  if Versioned then
  begin
    PMTemporalUpdate(ObjectIdList, ValueSpace, TranslationList);
    exit;
  end;
  MemberPMList := TBoldMemberPersistenceMapperList.Create;
  MemberPMList.OwnsEntries := False;

  if Assigned(Old_Values) then
    FetchPreviousSingleLinkValues(ObjectIdList, Old_Values);

  UseParams := SystemPersistenceMapper.SQLDataBaseConfig.UseParamsForInteger;
  aQuery := SystemPersistenceMapper.GetExecQuery;
  aQuery.ParamCheck := false;
  aQuery.SQLStrings.BeginUpdate;
  sql := TStringList.Create;
  TempList := TStringList.Create;
  try
    for T := 0 to AllTables.Count - 1 do
    begin
      if UpdatesMembersInTable(AllTables[T]) then
      begin
        for I := 0 to ObjectIDList.Count - 1 do
        begin
          ObjectContents := ValueSpace.ObjectContentsByObjectId[ObjectIDList[I]];
          MemberPMList.Clear;
          for A := 0 to MemberPersistenceMappers.Count - 1 do
          begin
            MemberPMapper := MemberPersistenceMappers[A] as TBoldMemberDefaultMapper;
            if assigned(MemberPMapper) and
              MemberPMapper.IsStoredInObject and not MemberPMapper.CustomCreateUpDate and
              MemberPMapper.IsDirty(ObjectContents) and
              ((MemberPMapper.ColumnDescriptions[0] as TBoldSQLColumnDescription).TableDescription = AllTables[T]) then
              MemberPMList.Add(MemberPMapper);
          end;

          if MemberPMLIst.Count > 0 then
          begin
            SQL.Clear;
            BoldAppendToStrings(SQL, Format('UPDATE %s SET ', [AllTables[T].SQLName]), True);
            if UseParams then
            begin
              TempList.Clear;
              SQLForMembers(AllTables[T], TempList, MemberPMList, ssValues, False, False, False);
              BoldAppendToStrings(SQL, BoldSeparateStringList(TempLIst, ', ', '', ''), True);
            end;
            aQuery.ClearParams;
            aQuery.AssignSQL(sql);

            TempList.Clear;
            ValuesToQueryByMemberList(ObjectIDList[I], ValueSpace, aQuery, TempList, MemberPMList, TranslationList, dsmUpdate);
            aQuery.SQLStrings.Add(TempList.Text);
            if UseParams then
            begin
              aQuery.CreateParam(ftInteger, IDCOLUMN_NAME).AsInteger := (TranslationList.TranslateToNewID[ObjectIDList[I]] as TBoldDefaultId).asInteger;
              BoldAppendToStrings(aQuery.SQLStrings, Format(' WHERE %s = :%0:s', [IDCOLUMN_NAME]), True);
            end
            else
              BoldAppendToStrings(aQuery.SQLStrings, Format(' WHERE %s = %s', [IDCOLUMN_NAME, ObjectIDList[I].AsString]), True);

            SystemPersistenceMapper.SendExtendedEvent(bpeUpdateObject, [ObjectIDList[I], ValueSpace, aQuery]);
            aQuery.SQLStrings.EndUpdate;
            aQuery.ParamCheck := true;
            aQuery.ExecSQL;
            aQuery.ParamCheck := false;
            aQuery.SQLStrings.BeginUpdate;
          end;
        end;
      end;
    end;
  finally
    aQuery.SQLStrings.Clear;
    aQuery.ClearParams;
    if aQuery.SQLStrings.Updating then
      aQuery.SQLStrings.EndUpdate;
    SystemPersistenceMapper.ReleaseExecQuery(aQuery);
  end;

  for A := 0 to MemberPersistenceMappers.Count - 1 do
  begin
    with MemberPersistenceMappers[A] do
      if assigned(MemberPersistenceMappers[A]) and
        IsStoredInObject and
        CustomCreateUpDate then
        PMUpdate(ObjectIDList, ValueSpace, TranslationList);
  end;
end;

procedure TBoldObjectDefaultMapper.PMDelete(ObjectIDList: TBoldObjectIdList; const ValueSpace: IBoldValueSpace;
              const Old_Values: IBoldValueSpace; TranslationList: TBoldIdTranslationList);
var
  I, T: Integer;
  aQuery: IBoldExecQuery;
  start, stop: integer;
  lst: TStringList;
  Block: Integer;
  ObjectCount: Integer;
  BoldGuard: IBoldGuard;
  FetchBlockSize: integer;
  IdListString: string;
begin
  BoldGuard := TBoldGuard.Create(lst);
  if Assigned(Old_Values) then
    FetchPreviousSingleLinkValues(ObjectIdList, Old_Values);

  if Versioned then
  begin
    PMUpdateStopTime(ObjectIdList);
    exit;
  end;

  for I := 0 to MemberPersistenceMappers.Count - 1 do
  begin
    with MemberPersistenceMappers[I] do
      if assigned(MemberPersistenceMappers[I]) and
        CustomCreateUpDate then
        PMDelete(ObjectIDList, ValueSpace, TranslationList);
  end;

  lst := TStringList.Create;
  FetchBlockSize := SystemPersistenceMapper.SQLDataBaseConfig.FetchBlockSize;
  aQuery := SystemPersistenceMapper.GetExecQuery;
  aQuery.ParamCheck := false;
  aQuery.ClearParams;
  try
    ObjectCount := ObjectIDList.Count - 1;
    for Block := 0 to (ObjectCount div FetchBlockSize) do
    begin
      lst.clear;
      Start := Block * FetchBlockSize;
      Stop := MinIntValue([Pred(Succ(Block) * FetchBlockSize), ObjectCount]);

      for i := start to stop do
        SystemPersistenceMapper.SendExtendedEvent(bpeDeleteObject, [ObjectIDList[I], ValueSpace]);
      BoldAppendToStrings(lst, 'DELETE FROM %s ', false);
      IdListString := IdListSegmentToWhereFragment(ObjectIdList, start, stop, false, aQuery);

      BoldAppendToStrings(lst, Format(' WHERE %%s %s', [IdListString]), false);
      for T := 0 to AllTables.Count - 1 do
      begin
        if AllTables[t] <> SystemPersistenceMapper.PSSystemDescription.XFilestable then
        begin
          aQuery.AssignSQLText(Format(lst.Text, [AllTables[T].SQLName, IDCOLUMN_NAME]));
          aQuery.ExecSQL;
        end else
        begin
          if AllTables[t].ColumnsList.IndexOf(SystemPersistenceMapper.XFilesTimeStampColumn)<> -1 then
          begin
            aQuery.AssignSQLText(Format('UPDATE %s SET %s = %d WHERE %s %s',
              [AllTables[T].SQLName, SystemPersistenceMapper.XFilesTimeStampColumn.SQLName,
               SystemPersistenceMapper.CurrentTimeStamp, IDCOLUMN_NAME, IdListString]));
            aQuery.ExecSQL;
          end;
        end;
      end;
    end;

    for I := 0 to ObjectIDList.Count - 1 do
    begin
      TranslationList.AddTranslation(ObjectIDList[I], nil);
    end;

  finally
    SystemPersistenceMapper.ReleaseExecQuery(aQuery);
  end;
end;

procedure LogLateFetch(SQLHits: integer; ExpressionName: String);
begin
  BoldPMLogFmt(sLogFetchedXobjectsOfY, [SQLHits, ExpressionName]);
  (*
  if SQLHits = 1 then
  try
    raise Exception.CreateFmt('Fetched %d objects of class %s', [SQLHits, ExpressionName]);
  except
    on E: Exception do
      TATErrorManager.LogLastException(E,'Fetch trace', 20);
  end;
  *)
end;

function TBoldObjectDefaultMapper.FindInCache(MemberIdList: TBoldMemberIdList;
  FetchMode: integer; var MemberPMList, CustomMembers: TBoldMemberPersistenceMapperList;
  var ASql: TStringList): boolean;
var
  i: integer;
begin
  result := false;
  for I := Length(fQueryCache) - 1 downto 0 do
    if (fQueryCache[i].fetchMode = FetchMode) and
       ((not Assigned(MemberIdList) and not Assigned(fQueryCache[i].MemberList))
       or (Assigned(MemberIdList) and (MemberIdList.IsEqual(fQueryCache[i].MemberList)))) then
    begin
      result := true;
      MemberPMList := fQueryCache[i].MemberPMList;
      CustomMembers := fQueryCache[i].CustomMembers;
      ASQL := fQueryCache[i].SqlStrings;
      exit;
    end;
end;

procedure TBoldObjectDefaultMapper.PMMultiPurposeRetrieveExactIdList(ObjectsToFetch: TBoldObjectIdList; const ValueSpace: IBoldValueSpace; MemberIdList: TBoldMemberIdList; FetchMode: Integer; TranslationList: TBoldIdTranslationList; MissingList: TBoldObjectIdList; FailureList: TBoldObjectIdList; TimeStamp: TBoldTimeStampType);
var
  Start, Stop: integer;
  CustomMembers,
  MemberPMList: TBoldMemberPersistenceMapperList;
  aQuery: IBoldQuery;
  TempList: TStringList;
  Block,
  ObjectCount: Longint;
  i: Integer;
  TempId, NewID: TBoldObjectId;
  SQLHits: integer;
  ObjectContents: IBoldObjectContents;
  sql: TStringList;
  FetchBlockSize: integer;
  RefetchIdList: TBoldObjectIdList;
  Guard: IBoldGuard;
  FoundInCache: boolean;
begin
  Guard := TBoldGuard.Create(TempList, RefetchIdList);
  FoundInCache := FindInCache(MemberIdList, FetchMode, MemberPMList, CustomMembers, sql);
  if not FoundInCache then
  begin
    sql := TStringList.Create;
    MemberPMList := TBoldMemberPersistenceMapperList.Create;
    MemberPMList.OwnsEntries := False;
    CustomMembers := TBoldMemberPersistenceMapperList.Create;
    CustomMembers.OwnsEntries := False;
    BuildMemberFetchLists(MemberIdList, MemberPMLIst, CustomMembers, FetchMode);
  end;
  TempList := TStringList.Create;
  RefetchIdList := TBoldObjectIdList.Create;
  FetchBlockSize := SystemPersistenceMapper.SQLDataBaseConfig.FetchBlockSize;
  aQuery := SystemPersistenceMapper.GetQuery;
  try
    try
      if SystemPersistenceMapper.SupportsObjectUpgrading then
        SystemPersistenceMapper.ObjectUpgrader.StartTransaction;


      if (MemberPMList.Count > 0) or not assigned(MemberIdList) or (MemberIdList.Count = 0) then
      begin
{
        if assigned(MissingList) then
          MissingList.AddList(ObjectsToFetch);
}
        ObjectCount := ObjectsToFetch.Count - 1;
        if assigned(MissingList) then
        begin
          MissingList.Capacity := ObjectCount+1;
          for I := ObjectsToFetch.Count - 1 downto 0 do
            MissingList.Add(ObjectsToFetch[i]);
        end;

        if not FoundInCache then
          RetrieveSelectStatement(SQL, MemberPMList, FetchMode, Versioned);
        for Block := 0 to (ObjectCount div FetchBlockSize) do
        begin
          Start := Block * FetchBlockSize;
          Stop := MinIntValue([Pred(Succ(Block) * FetchBlockSize), ObjectCount]);
          if Stop >= Start then
          begin
            aQuery.ClearParams;
            TempList.Assign(SQL);
            BoldAppendToStrings(TempList, IdListSegmentToWhereFragment(ObjectsToFetch, Start, Stop, true, aQuery), False);

            if versioned then
              RetrieveTimeStampCondition(TempList, TimeStamp, true, 'AND', false);

            aQuery.SQLText := TempList.Text;
            SQLHits := 0;
            aQuery.Open;
            while not aQuery.EOF do
            begin
              Inc(SQLHits);

              tempId := (SystemPersistenceMapper as TBoldSystemDefaultMapper).NewIdFromQuery(aQuery, NO_CLASS, 1, 0, timeStamp);
              NewId := ObjectsToFetch.IDByID[TempId];
              TempId.Free;

              if not assigned(NewId) then
                raise EBoldInternal.CreateFmt(sUninvitedObjectReturnedFromDB, [classname, aQuery.Fields[0].AsInteger]);

              if assigned(MissingList) then
              begin
                if MissingList.Last.IsEqual[NewId] then
                  MissingList.RemoveByIndex(MissingList.Count-1)
                else
                if MissingList.First.IsEqual[NewId] then
                  MissingList.RemoveByIndex(0)
                else
                begin
                  i := MissingList.IndexByID[NewId];
                  if i <> -1 then
                    MissingList.RemoveByIndex(i);
                end;
              end;

              if not NewId.TopSortedIndexExact then
                NewId := TranslationList.TranslateToNewID[NewId];

              if not NewId.TopSortedIndexExact then
                raise EBoldInternal.CreateFmt(sIDExactnessFailure, [classname]);

              if (TimeStamp = BOLDMAXTIMESTAMP) and not assigned(MemberIdList) and SystemPersistenceMapper.SupportsObjectUpgrading and IsOldVersion(aQuery) then
              begin
                RefetchIdList.Add(NewId);
                PortObject(NewId, aQuery);
              end
              else
              begin
                if FetchMode = fmCompare then
                  HandleCompareData(NewId, ValueSpace, TranslationList, aQuery, MemberPMList, FetchMode, FailureList)
                else
                  HandleFetchData(NewId, ValueSpace, TranslationList, aQuery, MemberPMList, FetchMode, FailureList);
                SystemPersistenceMapper.SendExtendedEvent(bpeFetchObject, [NewId, ValueSpace]);
              end;

              if versioned and (TimeStamp <> BOLDMAXTIMESTAMP) then
              begin
                ObjectContents := ValueSpace.EnsuredObjectContentsByObjectId[NewId];
                ObjectContents.IsReadOnly := true;
              end;

              aQuery.Next;
            end;
            aQuery.ClearParams;
            aQuery.Close;
//            SystemPersistenceMapper.SendExtendedEvent(bpeFetchObject, [SQLHits, ExpressionName]);
            if BoldPMLogHandler<>nil then
		          LogLateFetch(SQLHits, ExpressionName);
          end;
        end;

        if assigned(MissingList) and (FetchMode <> fmDistributable) and (MissingList.Count > 0) then
        begin
          if BoldPMLogHandler<>nil then
            BoldPMLogFmt(sLogXObjectsOfYMissing, [MissingList.Count, ExpressionName]);

//          if not SystemPersistenceMapper.SQLDataBaseConfig.IgnoreMissingObjects then
            for i := 0 to MissingList.Count-1 do
            begin
              if SystemPersistenceMapper.SQLDataBaseConfig.IgnoreMissingObjects then
                TranslationList.AddTranslationAdoptNew(MissingList[i], TBoldNonExistingObjectId.CreateWithClassID(MissingList[i].TopSortedIndex, MissingList[i].TopSortedIndexExact))
              else
              begin
                ObjectContents := ValueSpace.EnsuredObjectContentsByObjectId[MissingList[i]];
                ObjectContents.BoldExistenceState := besDeleted;
                ObjectContents.IsReadOnly := true;
              end;
            end;
        end;

      end;
      if SystemPersistenceMapper.SupportsObjectUpgrading then
        SystemPersistenceMapper.ObjectUpgrader.EndTransaction;
    except
      if SystemPersistenceMapper.SupportsObjectUpgrading then
        SystemPersistenceMapper.ObjectUpgrader.FailTransaction;
      raise;
    end;

    if RefetchIdList.Count > 0 then
      PMMultiPurposeRetrieveExactIdList(RefetchIdList, ValueSpace, MemberIdList, FetchMode, TranslationList,
                                        MissingList, FailureList, TimeStamp);

    for i := 0 to CustomMembers.Count - 1 do
      CustomMembers[i].PMFetch(ObjectsToFetch, ValueSpace, FetchMode, TranslationList, failureList);
  finally
    SystemPersistenceMapper.ReleaseQuery(aQuery);
    if not FoundInCache then
    begin
      // store in cache
      i := Length(fQueryCache);
      SetLength(fQueryCache, i+1);
      if Assigned(MemberIdList) then
        fQueryCache[i].MemberList := MemberIdList.Clone;
      fQueryCache[i].SqlStrings := Sql;
      fQueryCache[i].FetchMode := FetchMode;
      fQueryCache[i].MemberPMList := MemberPMList;
      fQueryCache[i].CustomMembers := CustomMembers;
    end;
  end;
end;

function TBoldObjectDefaultMapper.CompareFieldsToMembers(ObjectID: TBoldObjectId; const ValueSpace: IBoldValueSpace; const DataSet: IBoldDataSet; memberList: TBoldMemberPersistenceMapperList; TranslationList: TBoldIdTranslationList): Boolean;
var
  i: integer;
  ObjectContents: IBoldObjectContents;
begin
  result := true;
  ObjectContents := ValueSpace.ObjectContentsByObjectId[ObjectID];
  if assigned(ObjectContents) then
    for i := 0 to memberlist.count - 1 do
      result := result and TBoldMemberDefaultMapper(Memberlist[i]).CompareFields(ObjectContents, DataSet, ValueSpace, Translationlist);
end;

procedure TBoldObjectDefaultMapper.HandleCompareData(FetchedId: TBoldObjectId; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; const Query: IBoldQuery; MemberPMList: TBoldMemberPersistenceMapperList; FetchMode: integer; FailureList: TBoldObjectIdList);
begin
  if not CompareFieldsToMembers(FetchedId, ValueSpace, Query, MemberPMList, TranslationList) and
    not FailureList.IdInList[FetchedId] then
    FailureList.Add(FetchedId);
end;

procedure TBoldObjectDefaultMapper.PMCompareExactIDList(ObjectIDList: TBoldObjectIdList; const ValueSpace: IBoldValueSpace; MemberIdList: TBoldMemberIdList; TranslationList: TBoldIdTranslationList; FailureList: TBoldObjectIdList);
var
  MissingList: TBoldObjectIdList;
  i: integer;
  BoldGuard: IBoldGuard;
begin
  BoldGuard := TBoldGuard.Create(MissingList);
  MissingList := TBoldObjectIdList.Create;

  PMMultiPurposeRetrieveExactIdList(ObjectIdList, ValueSpace, MemberIdList, fmCompare, translationList, MissingList, FailureList, BoldMaxTimeStamp);
  if MissingList.Count > 0 then
  begin
    BoldLog.LogFmt(sOptimisticLockingFailedForNonExisting, [expressionName]);
      for i := 0 to MissingList.Count - 1 do
        BoldLog.LogFmt(sLogIdAsString, [MissingList[i].AsString]);
    FailureList.AddList(MissingList);
  end;
end;

procedure TBoldObjectDefaultMapper.HandleFetchData(FetchedId: TBoldObjectId; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; const Query: IBoldQuery; MemberPMList: TBoldMemberPersistenceMapperList; FetchMode: integer; FailureList: TBoldObjectIdList);
begin
  ValuesFromFieldsByMemberList(FetchedId, ValueSpace, TranslationList, Query, MemberPMList);
  if FetchMode = fmDistributable then
    DistributableInfoFromQuery(FetchedId, ValueSpace, TranslationList, Query);
end;

procedure TBoldObjectDefaultMapper.PMFetchExactIDList(ObjectIDList: TBoldObjectIdList; const ValueSpace: IBoldValueSpace; MemberIdList: TBoldMemberIdList; FetchMode: Integer; TranslationList: TBoldIdTranslationList; MissingList: TBoldObjectIdList);
var
  i: integer;
  TimeStamp: TBoldTimeStampType;
  ListOfOtherTimeStamps, ObjectsToFetch: TBoldObjectIdList;
begin
  if objectIdList.Count = 0 then
    exit;

  TimeStamp := ObjectIdList[0].TimeStamp;

  if Versioned and (ObjectIdList.Count>1) then
  begin
    ListOfOtherTimeStamps := TBoldObjectIdList.Create;
    ObjectsTofetch := TBoldObjectIdList.Create;
    for i := ObjectIdList.Count - 1 downto 0 do
    begin
      if ObjectIdList[i].TimeStamp = TimeStamp then
        ObjectsToFetch.Add(ObjectIdList[i])
      else
        ListOfOtherTimeStamps.Add(ObjectIdList[i]);
    end;
  end
  else
  begin
    ObjectsToFetch := ObjectidList;
    ListOfOtherTimeStamps := nil;
  end;

  try
    if assigned(ListOfOtherTimestamps) and (ListOfOtherTimeStamps.Count > 0) then
      PMfetchExactIdList(ListOfOtherTimeStamps, valuespace, memberidlist, fetchmode, translationlist, missinglist);

    PMMultiPurposeRetrieveExactIdList(ObjectsToFetch, ValueSpace, MemberIdList, FetchMode, TranslationList, missingList, nil, TimeStamp);

  finally
    if assigned(ListOfOtherTimeStamps) then
    begin
      ListOfOtherTimeStamps.Free;
      ObjectsToFetch.Free;
    end;
  end;
end;

procedure TBoldObjectDefaultMapper.FetchRawSqlCondition(ObjectIDList: TBoldObjectIdList; const ValueSpace: IBoldValueSpace;  RawCondition: TBoldRawSqlCondition; FetchMode: Integer; TranslationList: TBoldIdTranslationList);
var
  aQuery: IBoldQuery;
begin
  aQuery := SystemPersistenceMapper.GetQuery;
  try
    aQuery.AssignSQLText(RawCondition.SQL);
    aQuery.AssignParams(RawCondition.Params);
    RawCondition.AvailableAnswers := (SystemPersistenceMapper as TBoldSystemDefaultMapper).GetListUsingQuery(ObjectIDList, ValueSpace, aQuery, RawCondition.TopSortedIndex, -1, 0, FetchMode, TranslationList, RawCondition.Time, RawCondition.MaxAnswers, RawCondition.Offset);
  finally
    SystemPersistenceMapper.ReleaseQuery(aQuery);
  end;
end;

procedure TBoldObjectDefaultMapper.FillInMembers(MyMoldClass,
  CurrentMoldClass: TMoldClass; TypeNameDictionary: TBoldTypeNameDictionary);
var
  i: integer;
begin
  fOptimisticLockingMode := MyMoldClass.EffectiveOptimisticLocking;
  if SystemPersistenceMapper.UseModelVersion then
  begin
    fModelVersionMember := TBoldModelVersionMember.CreateFromMold(nil, MyMoldClass, self, -1, TypeNameDictionary);
    MemberPersistenceMappers.Add(fModelVersionMember);
  end;
  if SystemPersistenceMapper.UseReadOnly then
    MemberPersistenceMappers.Add(TBoldReadOnlynessMember.CreateFromMold(nil, MyMoldClass, self, -1, TypeNameDictionary));


  if SystemPersistenceMapper.UseXFiles then
  begin
    if SystemPersistenceMapper.UseTimestamp then
      MemberPersistenceMappers.Add(TBoldTimeStampMember.CreateFromMold(nil, MyMoldClass, self, -1, TypeNameDictionary));
    if SystemPersistenceMapper.UseGlobalId then
      MemberPersistenceMappers.Add(TBoldGlobalIdMember.CreateFromMold(nil, MyMoldClass, self, -1, TypeNameDictionary));
  end
  else
  begin
    if SystemPersistenceMapper.UseTimestamp and (not MyMoldClass.IsRootClass )then
      MemberPersistenceMappers.Add(TBoldNonXFileTimeStampMember.CreateFromMold(nil, MyMoldClass, self, -1, TypeNameDictionary));
  end;

  fObjectIdClass := BOLDDEFAULTIDNAME;
  for i := 0 to MyMoldClass.AllPossibleNames.Count - 1 do
    GenerateMappingInfo(MyMoldClass.AllPossibleNames[i], MyMoldClass);
  inherited;
end;

procedure TBoldObjectDefaultMapper.PMFetchWithCondition(ObjectIDList: TBoldObjectIdList; const ValueSpace: IBoldValueSpace; BoldCondition: TBoldCondition; FetchMode: Integer; TranslationList: TBoldIdTranslationList);
var
  aQuery: IBoldQuery;
  WhereToken,
  fromLine, JoinLine,
  RootTableName, JoinCondition,
  sCurrentMappingInfo: string;
  i, j: Integer;
  timeStamp: TBoldTimeStampType;
  MappingInfo: TBoldAllInstancesMappingArray;
  SQL: TStringList;
  SQLCondition: TBoldSQLCondition;
{$IFDEF RIL}
  SB: TStringBuilder;
{$ENDIF}
begin
  if not assigned(BoldCondition) or (BoldCondition is TBoldConditionWithClass) then
  begin
    if BoldCondition is TBoldRawSqlCondition then
    begin
      FetchRawSqlCondition(ObjectIDList, ValueSpace, TBoldRawSqlCondition(BoldCondition), FetchMode, TranslationList);
      Exit;
    end
    else if BoldCondition is TBoldSQLCondition then
      SQLCondition := BoldCondition as TBoldSQLCondition
    else
      SQLCondition := nil;

    MappingInfo := SystemPersistenceMapper.MappingInfo.GetAllInstancesMapping(ExpressionName);
    aQuery := SystemPersistenceMapper.GetQuery;
    sql := TStringList.Create;
    {$IFDEF RIL}
    SB := TStringBuilder.Create;
    {$ENDIF}
    try
      for i := 0 to length(MappingInfo) - 1 do
      begin
        sCurrentMappingInfo := MappingInfo[i].TableName;

        sql.Clear;
        WhereToken := 'WHERE';

        if assigned(BoldCondition) then
          timeStamp := (BoldCondition as TBoldConditionWithClass).Time
        else
          TimeStamp := BOLDMAXTIMESTAMP;

        if (BoldCondition is TBoldTimestampCondition) then
        begin
          SQL.Add(format('SELECT %s, %s', [IDCOLUMN_NAME, TYPECOLUMN_NAME]));
          SQL.Add(format('FROM %s', [(SystemPersistenceMapper.PSSystemDescription as TBoldDefaultSystemDescription).XFilestable.SQLName]));
          SQL.Add(format('WHERE %s > %d', [SystemPersistenceMapper.XFilesTimeStampColumn.SQLname, (BoldCondition as TBoldTimestampCondition).Timestamp]));
          aQuery.AssignSQL(SQL);
        end
        else
        begin
          {$IFDEF RIL}
          SB.Clear;
            SB.Append('SELECT ');
            SB.Append(sCurrentMappingInfo);
            SB.Append('.');
            SB.Append(IDCOLUMN_NAME);
            SB.Append(', ');
            SB.Append(sCurrentMappingInfo);
            SB.Append('.');
            SB.Append(TYPECOLUMN_NAME);
          SQL.Append(SB.ToString);

          FromLine := 'FROM '+sCurrentMappingInfo;
          {$ELSE}
          SQL.Append(Format('SELECT %s.%s, %s.%s', [MappingInfo[i].TableName, IDCOLUMN_NAME, MappingInfo[i].TableName, TYPECOLUMN_NAME]));
          FromLine := Format('FROM %s', [MappingInfo[i].TableName]);
          {$ENDIF}

          JoinLine := '';
          RootTableName := SystemPersistenceMapper.RootClassObjectPersistenceMapper.MainTable.SQLName;
          if assigned(SQLCondition) and
             SQLCondition.JoinInheritedTables and
             ((SQLCondition.whereFragment <> '') or
              (SQLCondition.OrderBy <> '')) then
          begin

            for j := 0 to AllTables.count - 1 do
              if not SameText(Alltables[j].SQLName, sCurrentMappingInfo) and
                (AllTables[j] <> SystemPersistenceMapper.PSSystemDescription.XFilestable) then
              begin
                JoinCondition := format('(%s.%s = %s.%s)',
                  [sCurrentMappingInfo, IDCOLUMN_NAME, AllTables[j].SQLName, IDCOLUMN_NAME]);

                if SystemPersistenceMapper.SQLDataBaseConfig.UseSQL92Joins then
                  fromLine := fromLine + format(' left join %s on %s ', [AllTables[j].SQLName, JoinCondition])
                else
                begin
                  fromLine := fromLine + format(', %s', [AllTables[j].SQLName]);
                  JoinLine := JoinLine + format('%s %s ', [WhereToken, joinCondition]);
                  WhereToken := 'AND';
                end
              end;
          end
          else if Versioned and
                  not SameText(sCurrentMappingInfo, RootTableName) then
          begin
            joinCondition := format('(%s.%s = %s.%s) ',
                  [sCurrentMappingInfo, IDCOLUMN_NAME, RootTableName, IDCOLUMN_NAME]);

            if SystemPersistenceMapper.SQLDataBaseConfig.UseSQL92Joins then
              fromLine := fromLine + format(' left join %s on %s ', [RootTableName, JoinCondition])
            else
            begin
              fromLine := FromLine + ', ' + RootTableName;
              JoinLine := JoinLine + format(' %s %s ', [WhereToken, joincondition]);
              WhereToken := 'AND';
            end;
          end;
          SQL.Add(fromLine);
          if JoinLine <> '' then
            SQL.Add(JoinLine);

          if Versioned then
          begin
            RetrieveTimeStampCondition(sql, TimeStamp, false, WhereToken, false);
            WhereToken := 'AND';
          end;

          if MappingInfo[i].ClassIdRequired then
          begin
            SQL.Add(Format('%s %s.%s in (%s)', [WhereToken, sCurrentMappingInfo, TYPECOLUMN_NAME, SubClassesID]));
            WhereToken := 'AND';
          end;

          if assigned(SQLCondition) then
          begin
            if SQLCondition.whereFragment <> '' then
              SQL.Append(Format('%s (%s)', [WhereToken, SQLCondition.whereFragment]));
            if SQLCondition.orderBy <> '' then
              SQL.Append(Format('ORDER BY %s', [SQLCondition.orderBy]));
          end;

          aQuery.AssignSQL(SQL);

          if assigned(SQLCondition) then
            aQuery.AssignParams(SQLCondition.Params);

          if assigned(SQLCondition) and Assigned(SystemPersistenceMapper.OnPsEvaluate) then
            SystemPersistenceMapper.OnPsEvaluate(aQuery);
         end;
        BoldCondition.AvailableAnswers := (SystemPersistenceMapper as TBoldSystemDefaultMapper).GetListUsingQuery(ObjectIDList, ValueSpace, aQuery, NO_CLASS ,1, 0, FetchMode, TranslationList, TimeStamp, BoldCondition.MaxAnswers, BoldCondition.Offset);
        {$IFDEF RIL}
        //BoldPMLogFmt('Fetched %d IDs in class %s from table %s', [ObjectIdList.Count, ExpressionName, MappingInfo[i].tableName]);
        if BoldPMLogHandler<>nil then { skip formating if the string is not used anyway... //ril }
        begin
          SB.Clear;
            SB.Append('Fetched ');
            SB.Append(IntToStr(ObjectIdList.Count));
            SB.Append(' IDs in class ');
            SB.Append(ExpressionName);
            SB.Append(' from table ');
            SB.Append(sCurrentMappingInfo);
          BoldPMLogFmt(SB.ToSTring, []);
        end;
        {$ELSE}
        BoldPMLogFmt(sLogFetchedXObjectsOfYFromTableZ, [ObjectIdList.Count, ExpressionName, MappingInfo[i].tableName]);
        {$ENDIF}
      end;
    finally
      SystemPersistenceMapper.ReleaseQuery(aQuery);
      sql.Free;
      {$IFDEF RIL}
      FreeAndNil(SB);
      {$ENDIF}
    end;
  end
  else
    raise EBold.CreateFmt(sUnknownConditionType, [BoldCondition.ClassName]);
end;

function TBoldObjectDefaultMapper.NextExternalObjectId(const ValueSpace: IBoldValueSpace; ObjectId: TBoldObjectId): TBoldObjectId;
var
  aQuery: IBoldQuery;
  aExecQuery: IBoldExecQuery;
  NewID: Longint;
  SystemDefaultMapper: TBoldSystemDefaultMapper;
begin
  SystemDefaultMapper := SystemPersistenceMapper as TBoldSystemDefaultMapper;
  if SystemDefaultMapper.NextDBID > SystemDefaultMapper.LastReservedDBID then
  begin
    NewID := -1;
    // plugin ID increment here
    if Assigned(SystemDefaultMapper.IDIncrementEvent) then
      NewID := SystemDefaultMapper.IDIncrementEvent(SystemDefaultMapper.ReservedCount);
    if (NewId <> -1) and SystemDefaultMapper.fCompatibilityMode then
      begin
        aExecQuery := SystemDefaultMapper.GetExecQuery;
        try
            aExecQuery.AssignSQLText(Format('UPDATE %s SET %s = %s + %d', [// do not localize
              SystemDefaultMapper.PSSystemDescription.IdTable.SQLName,
                IDCOLUMN_NAME,
                IDCOLUMN_NAME,
                SystemDefaultMapper.ReservedCount]));
            aExecQuery.ExecSQL;
        finally
          SystemDefaultMapper.ReleaseExecQuery(aExecQuery);
        end;
      end;
    if NewId = -1 then
    begin
      SystemDefaultMapper.StartTransaction(ValueSpace);
      aQuery := SystemDefaultMapper.GetQuery;
      aExecQuery := SystemDefaultMapper.GetExecQuery;
      try
        try
          aExecQuery.AssignSQLText(Format('UPDATE %s SET %s = %s + %d', [
                                    SystemDefaultMapper.PSSystemDescription.IdTable.SQLName,
                                    IDCOLUMN_NAME,
                                    IDCOLUMN_NAME,
                                    SystemDefaultMapper.ReservedCount]));
          aExecQuery.ExecSQL;
          aQuery.AssignSQLText(Format('SELECT %s FROM %s', [
                                 IDCOLUMN_NAME,
                                 SystemDefaultMapper.PSSystemDescription.IdTable.SQLName]));
          aQuery.Open;
          NewID := aQuery.Fields[0].AsInteger;
          aQuery.Close;
          SystemDefaultMapper.Commit(ValueSpace);
        except
          on e: EDatabaseError do
          begin
            SystemDefaultMapper.RollBack(ValueSpace);
            raise EBoldCantGetID.CreateFmt(sCannotGetID, [e.ClassName, e.message]);
          end;
        end;
      finally
        SystemDefaultMapper.ReleaseQuery(aQuery);
        SystemDefaultMapper.ReleaseExecQuery(aExecQuery);
      end;
    end;
    SystemDefaultMapper.NextDBID := NewID - SystemDefaultMapper.ReservedCount;
    SystemDefaultMapper.LastReservedDBID := SystemDefaultMapper.NextDBID + SystemDefaultMapper.ReservedCount - 1;
  end;
  SystemDefaultMapper.ReservedCount := 0;

  Result := TBoldDefaultId.CreateWithClassID(ObjectId.TopSortedIndex, true);
  (result as TBoldDefaultId).AsInteger := SystemDefaultMapper.NextDBID;

  SystemDefaultMapper.NextDBID := SystemDefaultMapper.NextDBID + 1;
end;

procedure TBoldObjectDefaultMapper.FetchPreviousSingleLinkValues(ObjectIdList: TBoldObjectIdLIst; const Old_Values: IBoldvalueSpace);
var
  i: integer;
begin
  if not Assigned(fSingleLinkList) then
  begin
    fSingleLinkList := TBoldMemberIdList.Create;
    for i := 0 to MemberPersistenceMappers.Count - 1 do
      if MemberPersistenceMappers[i] is TBoldEmbeddedSingleLinkDefaultMapper then
        fSingleLinkList.Add(TBoldmemberId.Create(MemberPersistenceMappers[i].MemberIndex));
  end;
  if fSingleLinkList.Count > 0 then
    PMFetchExactIDList(ObjectIdList, Old_Values, fSingleLInkList, fmNormal, nil, nil);
end;

procedure TBoldObjectDefaultMapper.MakeIDsExactUsingTable(
  ObjectIDList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList;
  Table: TBoldSQLTableDescription; EnsureAll: Boolean;
  HandleNonExisting: Boolean);
{$IFDEF RIL}
var
  Block,
  ObjectCount: Longint;
  topSortedIndex, I, j: Integer;
  tempId: TBoldObjectId;
  aQuery: IBoldQuery;
  lst: TStringList;
  Start, Stop: integer;
  WhereFragment: string;
  LittleObject: TLittleClass;
  Ids: TList;
  FetchBlockSize: integer;
  Found: boolean;
  SB: TStringBuilder;
begin
  if ObjectIDList.Count = 0 then
    exit;
  lst := TStringList.Create;
  SB := TStringBuilder.Create;
  Ids := TList.Create;
  aQuery := SystemPersistenceMapper.GetQuery;
  FetchBlockSize := SystemPersistenceMapper.SQLDataBaseConfig.FetchBlockSize;
  try
    ObjectCount := ObjectIDList.Count - 1;
    for Block := 0 to (ObjectCount div FetchBlockSize) do
    begin
      lst.Clear;
      aQuery.ClearParams;
      Table.RetrieveSelectIdAndTypeStatement(lst);
      Start := Block * FetchBlockSize;
      Stop := MinIntValue([Pred(Succ(Block) * FetchBlockSize), ObjectCount]);

      WhereFragment := IdListSegmentToWhereFragment(ObjectIDList, start, stop, true, aquery);
      {<*>}
      //BoldAppendToStrings(lst, Format(' WHERE %s %s', [IDCOLUMN_NAME, WhereFragment]), True);
      {
      sTestCompare := Format(' WHERE %s %s', [IDCOLUMN_NAME, WhereFragment]);
      }
		SB.Clear;
        SB.Append(' WHERE ');
        SB.Append(IDCOLUMN_NAME);
        SB.Append(' ');
        SB.Append(WhereFragment);
      {
      if S<>sTestCompare then
        raise Exception.CreateFmt('Optimize error in TBoldObjectDefaultMapper.MakeIDsExactUsingTable %s<>%s', [S, sTestCompare]);
      }
      BoldAppendToStrings(lst, SB.ToString, True);
      {</*>}

      aQuery.AssignSQL(lst);
      aQuery.Open;
      Ids.Count := 0;
      // commented out as Query.RecordCount can be slow
//      if aQuery.RecordCount > Ids.Capacity then
//        Ids.Capacity := aQuery.RecordCount;
      TranslationList.Capacity := aQuery.RecordCount;
      try
        while not aQuery.EOF do
        begin
          LittleObject := TLittleClass.Create;
          LittleObject.Id := aQuery.Fields[0].AsInteger;
          LittleObject.DbType := aQuery.Fields[1].AsInteger;
          Ids.Add(LittleObject);
          aQuery.next;
        end;
        for i := Start to Stop do
        begin
          Found := false;
          for j := 0 to Ids.Count - 1 do
          begin
            if TBoldDefaultId(ObjectIdList[i]).AsInteger = TLittleClass(Ids[j]).Id then
            begin
              Found := true;
              TopSortedIndex := SystemPersistenceMapper.topSortedIndexForBoldDbType(TLittleClass(Ids[j]).dbType);
              tempId := ObjectIdList[i].CloneWithClassId(TopSortedIndex, true);
              TranslationList.AddTranslationAdoptNew(ObjectIdList[i], tempId);
              Break;
            end;
          end;
          if not Found and not SystemPersistenceMapper.SQLDataBaseConfig.IgnoreMissingObjects then
          begin
            if HandleNonExisting then
            begin
              tempId := TBoldNonExistingObjectId.Create;
              TranslationList.AddTranslationAdoptNew(ObjectIdList[i], tempId);
            end
            else if EnsureAll then
              Raise EBoldObjectNotInPs.CreateFmt('Id %d not found in table %s', [TBoldDefaultId(ObjectIdList[i]).AsInteger, Table.SQLName]);
          end;
        end;
      finally
        aQuery.close;
        for j := 0 to Ids.Count - 1 do
          tObject(Ids[j]).Free;
      end;
    end;
  finally
    SystemPersistenceMapper.ReleaseQuery(aQuery);
    lst.Free;
    FreeAndNil(SB);
    Ids.free;
  end;
{$ELSE}
var
  Block,
  ObjectCount: Longint;
  topSortedIndex, I, j: Integer;
  tempId: TBoldObjectId;
  aQuery: IBoldQuery;
  lst: TStringList;
  Start, Stop: integer;
  WhereFragment: string;
  LittleObject: TLittleClass;
  Ids: TList;
  FetchBlockSize: integer;
begin
  if ObjectIDList.Count = 0 then
    exit;
  aQuery := SystemPersistenceMapper.GetQuery;
  FetchBlockSize := SystemPersistenceMapper.SQLDataBaseConfig.FetchBlockSize;
  Ids := TList.Create;
  lst := TStringList.Create;
  try
    ObjectCount := ObjectIDList.Count - 1;
    for Block := 0 to (ObjectCount div FetchBlockSize) do
    begin
      lst.Clear;
      Table.RetrieveSelectIdAndTypeStatement(lst);
      Start := Block * FetchBlockSize;
      Stop := MinIntValue([Pred(Succ(Block) * FetchBlockSize), ObjectCount]);

      WhereFragment := IdListSegmentToWhereFragment(ObjectIDList, start, stop, true, aquery);
      BoldAppendToStrings(lst, Format(' WHERE %s %s', [IDCOLUMN_NAME, WhereFragment]), True);
      aQuery.AssignSQL(lst);
      aQuery.Open;
      Ids.Count := 0;
      if Stop - Start >= Ids.Capacity then
        Ids.Capacity := Stop - Start +1;
      try
        while not aQuery.EOF do
        begin
          LittleObject := TLittleClass.Create;
          LittleObject.Id := aQuery.Fields[0].AsInteger;
          LittleObject.DbType := aQuery.Fields[1].AsInteger;
          Ids.Add(LittleObject);
          aQuery.next;
        end;
        for i := 0 to ObjectIdList.Count - 1 do
        begin
          for j := 0 to Ids.Count - 1 do
          begin
            if TBoldDefaultId(ObjectIdList[i]).AsInteger = TLittleClass(Ids[j]).Id then
            begin
              TopSortedIndex := SystemPersistenceMapper.topSortedIndexForBoldDbType(TLittleClass(Ids[j]).dbType);
              tempId := ObjectIdList[i].CloneWithClassId(TopSortedIndex, true);
              TranslationList.AddTranslation(ObjectIdList[i], tempId);
              TempId.Free;
            end;
          end;
        end;
      finally
        aQuery.close;
        for j := 0 to Ids.Count - 1 do
          tObject(Ids[j]).Free;
      end;
    end;
  finally
    SystemPersistenceMapper.ReleaseQuery(aQuery);
    lst.Free;
    Ids.Free;
  end;
{$ENDIF}
end;

function TBoldObjectDefaultMapper.InternalIdListSegmentToWhereFragment(
  IdList: TBoldObjectIdList; Start, Stop: Integer; AllowParms: Boolean;
  const Parameterized: IBoldParameterized): String;

  function GetParamStr(IdIndex, ParamIndex: integer; UseParams: Boolean): String;
  begin
    if UseParams then
    begin
      result := ':ID'+IntToStr(ParamIndex);
      Assert(IdList[IdIndex] is TBoldDefaultId, IdList[IdIndex].ClassName);
      Parameterized.CreateParam(ftInteger, 'ID'+IntToStr(ParamIndex)).AsInteger := (IdList[IdIndex] as TBoldDefaultId).AsInteger;
    end
    else
      result := IdList[Idindex].asString
  end;
{$IFDEF RIL}
var
  i: integer;
  ParamCount: integer;
  UseParams: Boolean;
  SB: TStringBuilder;
begin

  ParamCount := stop-start + 1;
  UseParams := AllowParms and (ParamCount <= SystemPersistenceMapper.SQLDataBaseConfig.MaxParamsInIdList);
  if ParamCount = 1 then
  begin
    Result := ' = ' + GetParamStr(start, 1, UseParams);
  end
  else
    try
      if UseParams then
        i := ParamCount * 12
      else
        i := ParamCount * 8;
      SB := TStringBuilder.Create(i+5);
      SB.Append('in (');
      for i := start to stop do
      begin
        if i > start then
          SB.Append(', ');
        SB.Append(GetParamStr(i, i-start+1, Useparams));
      end;
      SB.Append(')');
      {</*>}
      Result := SB.ToString;
    finally
      FreeAndNil(SB);
    end;
{$ELSE}
var
  TempList: TStringList;
  i: integer;
  Guard: IBoldGuard;
  ParamCount: integer;
  UseParams: Boolean;
begin
  Guard := TBoldGuard.Create(TempList);
  ParamCount := stop-start + 1;
  UseParams := ParamCount <= SystemPersistenceMapper.SQLDataBaseConfig.MaxParamsInIdList;
  if ParamCount = 1 then
    result := ' = '+GetParamStr(start, 1, UseParams)
  else
  begin
    TempList := TStringList.Create;
    for i := start to stop do
      TempList.Add(GetParamStr(i, i-start+1, Useparams));
    result := BoldSeparateStringList(TempLIst, ', ', 'in (', ')');
  end;
{$ENDIF}
end;

function TBoldObjectDefaultMapper.IdListSegmentToWhereFragment(
  IdList: TBoldObjectIdList; Start, Stop: Integer; AllowParms: Boolean;
  const Query: IBoldExecQuery): String;
begin
  result := InternalIdListSegmentToWhereFragment(IdList, Start, Stop, AllowParms, Query as IBoldParameterized);
end;

function TBoldObjectDefaultMapper.IdListSegmentToWhereFragment(
  IdList: TBoldObjectIdList; Start, Stop: Integer; AllowParms: Boolean;
  const Query: IBoldQuery): String;
begin
  result := InternalIdListSegmentToWhereFragment(IdList, Start, Stop, AllowParms, Query as IBoldParameterized);
end;

{ TBoldMemberDefaultMapper }

function TBoldMemberDefaultMapper.GetAllowNullAsSQL: string;
begin
  if AllowNull then
    Result := ''
  else
    Result := SystemPersistenceMapper.SQLDataBaseConfig.EffectiveSQLforNotNull;
end;

procedure TBoldMemberDefaultMapper.GenerateMappingInfo(MoldClass: TMoldClass; MoldMember: TMoldMember);
{$IFDEF RIL}
  procedure GenerateLocal(const ClassExpressionName, MemberExpressionName: String; var ColumnNames: String; const LocalMoldClass: TMoldClass);
  var
    i: integer;
    SubClass: TMoldClass;
  begin
    if LocalMoldClass.TableMapping in [tmOwn, tmParent] then
    begin
      TBoldSystemDefaultMapper(SystemPersistenceMapper).MappingInfo.AddMemberMapping(
          ClassExpressionName, MemberExpressionName,
          FindDefiningTable(LocalMoldClass, MoldMember),
          ColumnNames, ClassName, FColumnIndex);
    end;
    if LocalMoldClass.TableMapping = tmChildren then
    begin
      for i := 0 to LocalMoldClass.SubClasses.Count-1 do
      begin
        SubClass := LocalMoldClass.SubClasses[i];
        if SubClass.EffectivePersistent then
          GenerateLocal(ClassExpressionName, MemberExpressionName, ColumnNames, SubClass);
      end;
    end;
  end;

  procedure GenerateForClassName(const ClassExpressionName: String; var ColumnNames: String);
  var
    i: integer;
    s: TStringList;
    BoldGuard: IBoldGuard;
  begin
    GenerateLocal(ClassExpressionName, ExpressionName, ColumnNames, moldclass);
    if assigned(MoldMember) then
    begin
      BoldGuard := TBoldGuard.Create(s);
      s := TStringlist.Create;
      s.CommaText := MoldMember.FormerNames;
      for i := 0 to s.count - 1 do
        GenerateLocal(ClassExpressionName, s[i], ColumnNames, moldclass);
    end;
  end;

var
  i: integer;
  ColumnNames: String;
  SB: TStringBuilder;
begin
  if requiresMemberMapping then
  begin
    SB := TStringBuilder.Create;
    ColumnNames := '';
    for i := 0 to ColumnCount - 1 do
    begin
      if SB.Length > 0 then
        SB.Append(', ');
      SB.Append(BoldExpandName(InitialColumnName[i], '', xtSQL, SystemPersistenceMapper.SQLDataBaseConfig.MaxDBIdentifierLength, SystemPersistenceMapper.NationalCharConversion));
    end;
    ColumnNames := SB.ToString;
    for i := 0 to MoldClass.AllPossibleNames.count - 1 do
      GenerateForClassName(MoldClass.AllPossibleNames[i], ColumnNames);
    FreeAndNil(SB);
  end;
{$ELSE}
var
  ColumnNames: String;

  procedure GenerateLocal(ClassExpressionName, MemberExpressionName: String; LocalMoldClass: TMoldClass);
  var
    i: integer;
  begin
    if LocalMoldClass.TableMapping in [tmOwn, tmParent] then
      (SystemPersistenceMapper as TBoldSystemDefaultMapper).MappingInfo.AddMemberMapping(
        ClassExpressionName,
        MemberExpressionName,
        FindDefiningTable(LocalMoldClass, MoldMember),
        ColumnNames,
        ClassName,
        FColumnIndex);
    if LocalMoldClass.TableMapping = tmChildren then
      for i := 0 to LocalMoldClass.SubClasses.Count - 1 do
        if LocalMoldClass.SubClasses[i].EffectivePersistent then
          GenerateLocal(ClassExpressionName, MemberExpressionName, LocalMoldClass.SubClasses[i]);
  end;

  procedure GenerateForClassName(ClassExpressionName: String);
  var
    i: integer;
    s: TStringList;
    BoldGuard: IBoldGuard;
  begin
    GenerateLocal(ClassExpressionName, ExpressionName, moldclass);
    if assigned(MoldMember) then
    begin
      BoldGuard := TBoldGuard.Create(s);
      s := TStringlist.Create;
      s.CommaText := MoldMember.FormerNames;
      for i := 0 to s.count - 1 do
        GenerateLocal(ClassExpressionName, s[i], moldclass);
    end;
  end;

var
  i: integer;
begin
  if requiresMemberMapping then
  begin
    ColumnNames := '';
    for i := 0 to ColumnCount - 1 do
    begin
      if ColumnNames <> '' then
        ColumnNames := ColumnNames + ', ';
      ColumnNames := ColumnNames + BoldExpandName(InitialColumnName[i], '', xtSQL, SystemPersistenceMapper.SQLDataBaseConfig.MaxDBIdentifierLength, SystemPersistenceMapper.NationalCharConversion);
    end;
    for i := 0 to MoldClass.AllPossibleNames.count - 1 do
      GenerateForClassName(MoldClass.AllPossibleNames[i]);
  end;
{$ENDIF}
end;

constructor TBoldMemberDefaultMapper.CreateFromMold(moldMember: TMoldMember; moldClass: TMoldClass; Owner: TBoldObjectPersistenceMapper; const MemberIndex: Integer; TypeNameDictionary: TBoldTypeNameDictionary);
begin
  inherited;
  if (not assigned(MoldMember) or MoldMember.EffectivePersistent) and (ColumnCount > 0) then
    GenerateMappingInfo(MoldClass, MoldMember);
end;

procedure TBoldSystemDefaultMapper.GetNewTimeStamp;
var
  aQuery: IBoldQuery;
  aExecQuery: IBoldExecQuery;
  LastClockTimestamp: TBoldTimestampType;
  LastClock: TDateTime;
begin
  if Assigned(NewTimeStampEvent) then
  begin
    NewTimeStampEvent(FCurrentTimeStamp, LastClockTimestamp, LastClock, fTimeOfTimeStamp, ClockLogGranularity);
    if CompatibilityMode or UseClockLog then
    begin
      aExecQuery := GetExecQuery;
      try
        if CompatibilityMode then
        begin
          aExecQuery.AssignSQLText(
            Format('UPDATE %s SET %s = %d', [// do not localize
            PSSystemDescription.TimeStampTable.SQLName,
              TimeStampTableTimeStampColumn.SQLName,
              FCurrentTimeStamp]));
          aExecQuery.ExecSQL;
        end;
        if UseClockLog then
        begin
          if LastClockTimeStamp <> -1 then
          begin
            aExecQuery.ParamCheck := true;
            if CompatibilityMode then
            begin
              aExecQuery.AssignSQLText(
                Format('UPDATE %s SET %s = :newTimestamp, %s = :newClock', [// do not localize
                PSSystemDescription.LastClockTable.SQLName,
                  LastClockTableLastTimeStampColumn.SQLName,
                  LastClockTableLastClockColumn.SQLName]));
              aExecQuery.ParamByName('newTimestamp').AsInteger := FCurrentTimeStamp; // do not localize
              aExecQuery.ParamByName('newClock').AsDateTime := fTimeOfTimeStamp; // do not localize
              aExecQuery.ExecSQL;
            end;

            aExecQuery.AssignSQLText(Format('INSERT INTO %s (%s, %s, %s, %s) VALUES (:lastTimeStamp, :thisTimeStamp, :lastClock, :thisClock)', // do not localize
              [PSSystemDescription.ClockLogTable.SQLName,
              ClockLogTableLastTimeStampColumn.SQLName,
                ClockLogTableThisTimeStampColumn.SQLName,
                ClockLogTableLastClockColumn.SQLName,
                ClockLogTableThisClockColumn.SQLName]));
            aExecQuery.ParamByName('lastTimeStamp').AsInteger := LastClockTimestamp; // do not localize
            aExecQuery.ParamByName('thisTimeStamp').AsInteger := FCurrentTimeStamp; // do not localize
            aExecQuery.ParamByName('lastClock').AsDateTime := LastClock; // do not localize
            aExecQuery.ParamByName('thisClock').AsDateTime := fTimeOfTimeStamp; // do not localize
            aExecQuery.ExecSQL;
          end;
        end;
      finally
        ReleaseExecQuery(aExecQuery);
      end;
    end;
  end
  else
  begin
    aQuery := GetQuery;
    aExecQuery := GetExecQuery;
    aExecQuery.ParamCheck := false;
    try
      aExecQuery.AssignSQLText(
        Format('UPDATE %s SET %s = %s + 1', [
          PSSystemDescription.TimeStampTable.SQLName,
          TimeStampTableTimeStampColumn.SQLname,
          TimeStampTableTimeStampColumn.SQLname]));
      aExecQuery.ExecSQL;

      aQuery.AssignSQLText(
        Format('SELECT %s FROM %s', [
          TimeStampTableTimeStampColumn.SQLname,
          PSSystemDescription.TimeStampTable.SQLName]));
      aQuery.Open;
      FCurrentTimeStamp := aQuery.Fields[0].AsInteger;
      aQuery.Close;

      if UseClockLog then
      begin
        aQuery.AssignSQLText(
          Format('SELECT %s, %s FROM %s', [
            LastClockTableLastTimeStampColumn.SqlName,
            LastClockTableLastClockColumn.SQLName,
            PSSystemDescription.LastClockTable.SQLName]));
        aQuery.Open;
        LastClockTimestamp := aQuery.Fields[0].AsInteger;
        LastClock := aQuery.Fields[1].AsDateTime;
        aQuery.Close;

//        fTimeOfTimeStamp := GetCorrectTime; // TimeOfTimeStamp is already set in NewTimeStampEvent
        if (fTimeOfTimeStamp - LastClock) > ClockLogGranularity then
        begin
          aExecQuery.ParamCheck := true;
          aExecQuery.AssignSQLText(
            Format('UPDATE %s SET %s = %d, %s = :newClock', [
              PSSystemDescription.LastClockTable.SQLName,
              LastClockTableLastTimeStampColumn.SQLName,
              Integer(FCurrentTimeStamp),
              LastClockTableLastClockColumn.SQLName]));
          aExecQuery.ParamByName('newClock').AsDateTime := fTimeOfTimeStamp; // do not localize
          aExecQuery.ExecSQL;

          aExecQuery.AssignSQLText(Format('INSERT INTO %s (%s, %s, %s, %s) VALUES (%d, %d, :lastClock, :thisClock)',
                         [PSSystemDescription.ClockLogTable.SQLName,
                         ClockLogTableLastTimeStampColumn.SQLName,
                         ClockLogTableThisTimeStampColumn.SQLName,
                         ClockLogTableLastClockColumn.SQLName,
                         ClockLogTableThisClockColumn.SQLName,
                         Integer(LastClockTimestamp),
                         Integer(FCurrentTimeStamp)]));
          aExecQuery.ParamByName('lastClock').AsDateTime := LastClock; // do not localize
          aExecQuery.ParamByName('thisClock').AsDateTime := fTimeOfTimeStamp; // do not localize
          aExecQuery.ExecSQL;
        end;
      end;
    finally
      ReleaseQuery(aQuery);
      ReleaseExecQuery(aExecQuery);
    end;
  end;
end;


function TBoldObjectDefaultMapper.UpdatesMembersInTable(
  aTable: TBoldSQLTableDescription): Boolean;
var
  limit: Integer;
begin
  limit := 2;
  if atable.Versioned then
  begin
    Inc(limit);
    if aTable.ContainsStopTimeStamp then
      Inc(limit);
  end;
  result := (aTable.ColumnsList.Count > limit);
end;

function TBoldObjectDefaultMapper.DistributableTable: TBoldSQLTableDescription;
begin
  result := (SystemPersistenceMapper as TBoldSystemDefaultMapper).PSSystemDescription.XFilestable;
end;

procedure TBoldObjectDefaultMapper.SQLForDistributed(SQL: TStrings;
  const SQLStyle: TBoldSQLStyle);
begin
  case SQLStyle of
  ssColumns:
    begin
      if SystemPersistenceMapper.UseTimestamp then
        SQL.Append(SystemPersistenceMapper.XFilesTimeStampColumn.SQLname);
      if SystemPersistenceMapper.UseGlobalId then
        SQL.Append(SystemPersistenceMapper.XFilesGlobalIdColumn.SQLName);
    end;
  ssParameters:
    begin
      if SystemPersistenceMapper.UseTimestamp then
        SQL.Append(':' + SystemPersistenceMapper.XFilesTimeStampColumn.SQLname);
      if SystemPersistenceMapper.UseGlobalId then
        SQL.Append(':' + SystemPersistenceMapper.XFilesGlobalIdColumn.SQLName);
    end;
  end;
end;

procedure TBoldObjectDefaultMapper.DistributableInfoFromQuery(
  ObjectID: TBoldObjectId; const ValueSpace: IBoldValueSpace;
  TranslationList: TBoldIdTranslationList; const DataSet: IBoldDataSet);
begin
  if SystemPersistenceMapper.UseTimestamp then
    ValueSpace.EnsuredObjectContentsByObjectId[ObjectID].TimeStamp := DataSet.FieldByUpperCaseName(SystemPersistenceMapper.XFilesTimeStampColumn.SQLNameUpper).AsInteger;
  if SystemPersistenceMapper.UseGlobalId then
    ValueSpace.EnsuredObjectContentsByObjectId[ObjectID].GlobalId := DataSet.FieldByUpperCaseName(SystemPersistenceMapper.XFilesGlobalIdColumn.SQLNameUpper).AsString;
end;

procedure TBoldSystemDefaultMapper.PMTranslateToGlobalIds(
  ObjectIdList: TBoldObjectIdList;
  TranslationList: TBoldIdTranslationList);
var
  aQuery: IBoldQuery;
  IdList: TStringList;
  anObjectId: TBoldObjectId;
  aGlobalId: TBoldObjectId;
  Block, ObjectCount, I: Integer;
  FetchBlockSize: integer;
begin
  if ObjectIdList.Count = 0 then
    exit;

  aQuery := GetQuery;
  IdList := TStringList.Create;
  FetchBlockSize := SQLDataBaseConfig.FetchBlockSize;

  try
    ObjectCount := ObjectIdList.Count - 1;
    for Block := 0 to (ObjectCount div FetchBlockSize) do
    begin
      IdList.Clear;
      for I := Block * FetchBlockSize to MinIntValue([Pred(Succ(Block) * FetchBlockSize), ObjectCount]) do
        IdList.Add(ObjectIdList[I].AsString);

      aQuery.AssignSQLText(
        Format('SELECT %s, %s, %s FROM %s WHERE %0:s IN (%4:s)', [
          IDCOLUMN_NAME,
          TYPECOLUMN_NAME,
          XFilesGlobalIdColumn.SQLName,
          PSSystemDescription.XFilestable.SQLName,
          BoldSeparateStringList(IDList, ', ', '', '')]));

      aQuery.Open;

      TranslationList.Capacity := TranslationList.Count + aQuery.RecordCount;
      while not aQuery.EOF do
      begin
        anObjectId := NewIdFromQuery(aQuery, NO_CLASS, 1, 0, BOLDMAXTIMESTAMP);
        aGlobalId := NewGlobalIdFromQuery(aQuery, 1);

        TranslationList.AddTranslationAdoptBoth(anObjectId, aGlobalId);

        aQuery.Next;
      end;
      aQuery.Close;
    end;
  finally
    ReleaseQuery(aQuery);
    IdList.Free;
  end;
end;

procedure TBoldSystemDefaultMapper.PMTranslateToLocalIds(
  GlobalIdList: TBoldObjectIdList;
  TranslationList: TBoldIdTranslationList);
var
  aQuery: IBoldQuery;
  IdList: TStringList;
  anObjectId: TBoldObjectId;
  aGlobalId: TBoldObjectId;
  Block, ObjectCount, I: Integer;
  FetchBlockSize: integer;
begin
  if GlobalIdList.Count = 0 then
    exit;

  aQuery := GetQuery;
  IdList := TStringList.Create;
  FetchBlockSize := SQLDataBaseConfig.FetchBlockSize;

  try
    ObjectCount := GlobalIdList.Count - 1;
    for Block := 0 to (ObjectCount div FetchBlockSize) do
    begin
      IdList.Clear;

      for I := Block * FetchBlockSize to MinIntValue([Pred(Succ(Block) * FetchBlockSize), ObjectCount]) do
        IdList.Add(GlobalIdList[I].AsString);

      aQuery.AssignSQLText(
        Format('SELECT %s, %s, %s FROM %s WHERE %0:s IN (%4:s)', [
          XFilesGlobalIdColumn.SQLName,
          IDCOLUMN_NAME,
          TYPECOLUMN_NAME,
          PSSystemDescription.XFilestable.SQLName,
          BoldSeparateStringList(IdList, ''', ''', '''', '''')]));

      aQuery.Open;

      TranslationList.Capacity := TranslationList.Count + aQuery.RecordCount;
      while not aQuery.EOF do
      begin
        aGlobalId := NewGlobalIdFromQuery(aQuery, 2);
        anObjectId := NewIdFromQuery(aQuery,NO_CLASS,  2, 1, BoldMaxTimeStamp);

        TranslationList.AddTranslationAdoptBoth(aGlobalId, anObjectId);

        aQuery.Next;
      end;

      aQuery.Close;
    end;

  finally
    ReleaseQuery(aQuery);
    IdList.Free;
  end;
end;

procedure TBoldSystemDefaultMapper.FetchDeletedObjects(
  ObjectIdList: TBoldObjectIdList; const ValueSpace: IBoldValueSpace);
var
  aQuery: IBoldQuery;
  IdList: TStringList;
  anObjectId: TBoldDefaultId;
  Block, ObjectCount, I: Integer;
  aTimeStamp: TBoldTimeStampType;
  anObjectContents: IBoldObjectContents;
  FetchBlockSize: integer;

begin
  aQuery := GetQuery;
  IdList := TStringList.Create;
  anObjectId := TBoldDefaultId.Create;
  FetchBlockSize := SQLDataBaseConfig.FetchBlockSize;

  try
    ObjectCount := ObjectIdList.Count - 1;
    for Block := 0 to (ObjectCount div FetchBlockSize) do
    begin
      IdList.Clear;

      for I := Block * FetchBlockSize to MinIntValue([Pred(Succ(Block) * FetchBlockSize), ObjectCount]) do
        IdList.Add(ObjectIdList[I].AsString);

      aQuery.AssignSQLText(
        Format('SELECT %s, %s, %s FROM %s WHERE %0:s IN (%4:s)', [
          IDCOLUMN_NAME,
          TYPECOLUMN_NAME,
          XFilesTimeStampColumn.SQLname,
          PSSystemDescription.XFilestable.SQLName,
          BoldSeparateStringList(IdList, ', ', '', '')]));

      aQuery.Open;
      while not aQuery.EOF do
      begin
        anObjectId.AsInteger := aQuery.Fields[0].AsInteger;
        aTimeStamp := aQuery.Fields[2].AsInteger;

        anObjectContents := ValueSpace.EnsuredObjectContentsByObjectId[anObjectId];
        anObjectContents.TimeStamp := aTimeStamp;
        anObjectContents.BoldExistenceState := besDeleted;
        anObjectContents.BoldPersistenceState := bvpsCurrent;

        aQuery.Next;
      end;

      aQuery.Close;
    end;
  finally
    ReleaseQuery(aQuery);
    IdList.Free;
    anObjectId.Free;
  end;
end;

procedure TBoldSystemDefaultMapper.InitializeBoldDbType;
var
  i: integer;
  BoldDbType: integer;
  ObjectPersistenceMapper: TBoldObjectDefaultMapper;
  MissingIds: TStringList;
  BoldGuard: IBoldGuard;
begin
  BoldGuard := TBoldGuard.Create(MissingIDs);
  MissingIds := TStringList.Create;

  for i := 0 to ObjectPersistenceMappers.count - 1 do
  begin
    ObjectPersistenceMapper := ObjectPersistenceMappers[i] as TBoldObjectDefaultMapper;
    if assigned(ObjectPersistenceMapper) then
    begin
      BoldDbType := MappingInfo.GetDbTypeMapping(ObjectPersistenceMapper.ExpressionName);
      if BoldDbType = NO_CLASS then
        MissingIds.Add(format(sUnableToFindDBIDForX, [ObjectPersistenceMapper.ExpressionName]))
      else
        ObjectPersistenceMapper.BoldDbType := BoldDbType;

      ObjectPersistenceMapper.SubClassesID := IntToStr(ObjectPersistenceMapper.BoldDbType);
      while Assigned(ObjectPersistenceMapper.SuperClass) do
      begin
        ObjectPersistenceMapper := ObjectPersistenceMapper.SuperClass as TBoldObjectDefaultMapper;
        ObjectPersistenceMapper.SubClassesID := ObjectPersistenceMapper.SubClassesID + ', ' + IntToStr(BoldDbType);
      end;
    end;
  end;
  if MissingIds.Count > 0 then
    raise EBoldMissingID.Create(MissingIds.Text);
end;

procedure TBoldSystemDefaultMapper.PMSetReadonlyness(ReadOnlyList, WriteableList: TBoldObjectIdList);

  procedure SetReadOnlyTo(ObjectIdList: TBoldObjectIdList; ReadOnly: Boolean);
  var
    Block,
    ObjectCount: Longint;
    I: Integer;
    aQuery: IBoldExecQuery;
    IdList: TStringList;
    ReadOnlyValue: Integer;
    FetchBlockSize: integer;
  begin
    if not assigned(ObjectIdList) or (ObjectIdList.Count = 0) then
      exit;
    IDList := TStringList.Create;
    aQuery := GetExecQuery;
    ReadOnlyValue := 0;
    FetchBlockSize := SQLDataBaseConfig.FetchBlockSize;

    if ReadOnly then
      ReadOnlyValue := 1;
    try
      ObjectCount := ObjectIdList.Count - 1;
      for Block := 0 to (ObjectCount div FetchBlockSize) do
      begin
        IdList.Clear;

        for I := Block * FetchBlockSize to MinIntValue([Pred(Succ(Block) * FetchBlockSize), ObjectCount]) do
          IdList.Add(ObjectIDList[I].AsString);

        aQuery.AssignSQLText(
          Format('UPDATE %s SET %s = %d WHERE %s IN (%s)', [
            RootClassObjectPersistenceMapper.MainTable.SQLName,
            READONLYCOLUMN_NAME,
            ReadOnlyValue,
            IDCOLUMN_NAME,
            BoldSeparateStringList(IDList, ', ', '', '')]));

        aQuery.ExecSQL;
      end;
    finally
      ReleaseExecQuery(aQuery);
      IDList.Free;
    end;
  end;

begin
  SetReadOnlyTo(ReadOnlyList, True);
  SetReadOnlyTo(WriteableList, False);
end;

function TBoldSystemDefaultMapper.NewGlobalIdFromQuery(aQuery: IBoldQuery; BoldDbTypeColumn: Integer): TBoldObjectId;
var
  TopSortedIndex: Integer;
begin
  TopSortedIndex := topSortedIndexForBoldDbType(aQuery.Fields[BoldDbTypeColumn].AsInteger);
  Result := TBoldGlobalId.CreateWithInfo(
    aQuery.FieldByUpperCaseName(XFilesGlobalIdColumn.SQLNameUpper).AsString,
    TopSortedIndex,
    true,
    ObjectPersistenceMappers[TopSortedIndex].ExpressionName);
end;

procedure TBoldMemberDefaultMapper.PMFetch(ObjectIdList: TBoldObjectIdList;
  const ValueSpace: IBoldValueSpace; FetchMode: Integer;
  TranslationList: TBoldIdTranslationList; FailureList: TBoldObjectIdList);
var
  Block,
  ObjectCount: Longint;
  BlockSize: Integer;
  TempId, NewID: TBoldObjectId;
  aQuery: IBoldQuery;
  sql, TempList: TStringList;
  start, stop: integer;
  MemberPMList: TBoldMemberPersistenceMapperList;
  Table: TBoldSQLTableDescription;
begin


  if (ObjectIdList.Count > 1) and RequiresLiveQuery then
    BlockSize := 1
  else
    BlockSize := SystemPersistenceMapper.SQLDataBaseConfig.FetchBlockSize;;

  sql := TStringList.Create;
  TempList := TStringList.Create;
  MemberPMList := TBoldMemberPersistenceMapperList.Create;
  MemberPMList.OwnsEntries := False;
  MemberPmList.Add(self);

  aQuery := SystemPersistenceMapper.GetQuery;
  try
    if RequiresLiveQuery then
      aQuery.RequestLiveQuery := true;

    ObjectCount := ObjectIDList.Count - 1;
    for Block := 0 to (ObjectCount div BlockSize) do
    begin
      sql.clear;
      Table := (ColumnDescriptions[0] as TBoldSQLColumnDescription).TableDescription;
      TempLIst.Clear;
      TBoldObjectDefaultMapper(ObjectPersistenceMapper).SQLForMembers(Table, TempList, MemberPMList, ssColumns, true, true, False);
      SQL.Text := format('SELECT %s FROM %s WHERE %s ', [BoldSeparateStringList(TempLIst, ', ', '', ''), Table.SQLName, IDCOLUMN_NAME]);

      Start := Block * BlockSize;
      Stop := MinIntValue([Pred(Succ(Block) * BlockSize), ObjectCount]);
      if Start <= stop then
      begin
        BoldAppendToStrings(SQL, ObjectPersistenceMapper.IdListSegmentToWhereFragment(ObjectIdList, start, stop, true, aQuery), False);

        aQuery.AssignSQL(sql);
        aQuery.Open;
        while not aQuery.EOF do
        begin
          tempId := SystemPersistenceMapper.NewIdFromQuery(aQuery, NO_CLASS, 1, 0, ObjectIDList[0].TimeStamp);
          NewId := ObjectIDList.IDByID[TempId];
          TempId.Free;

          if not assigned(NewId) then
            raise EBoldInternal.CreateFmt('%s.PMFetch: Database returned object we didn''t ask for (ID: %d)', [ClassName, aQuery.Fields[0].AsInteger]);

          if not NewId.TopSortedIndexExact then
            NewId := TranslationList.TranslateToNewID[NewId];

          if not NewId.TopSortedIndexExact then
            raise EBoldInternal.CreateFmt('%s.PMFetch: Got an Id with no or only approx class!', [Classname]);

          ObjectPersistenceMapper.ValuesFromFieldsByMemberList(NewId, ValueSpace, TranslationList, aQuery, MemberPMList);

          aQuery.Next;
        end;
        aQuery.Close;

      end;
    end;
  finally
    SystemPersistenceMapper.ReleaseQuery(aQuery);
    sql.Free;
    MemberPMList.Free;
    TempList.Free;
  end;
end;

procedure TBoldSystemDefaultMapper.PMFetchClassWithCondition(
  ObjectIDList: TBoldObjectIdList; const ValueSpace: IBoldValueSpace;
  BoldCondition: TBoldCondition; FetchMode: Integer;
  TranslationList: TBoldIdTranslationList);
var
  i: integer;
  Q2: IBoldQuery;
  sql: TStringList;
  OclCondition: TBoldOclCondition;
  SQlNodeResolver: TBoldSqlNodeResolver;
  SQLNodeMaker: TBoldSQLNodeMaker;
  SQlGenerator: TBoldSQLQueryGenerator;
  aCPCond: TBoldChangePointCondition;
  GlobalNameSpace: TBoldSqlNameSpace;
  aVariableIDLists: TBoldObjectArray;
  vBoldParameterized: IBoldParameterized;

  procedure FixQueriesForEnv(VarBinding: TBoldSQLVariableBinding; Context: TBoldObjectIdList; NameSpace: TBoldSqlNameSpace);
  var
    MainTableRef: TBoldSqlTableReference;
    BoldID: TBoldDefaultID;
  begin
    if CompareText(VarBinding.VariableName, 'SELF') = 0 then // do not localize
    begin
      VarBinding.NewQuery(NameSpace);

      MainTableRef := VarBinding.TableReferenceForTable(VarBinding.ObjectMapper.MainTable, VarBinding.Query, true);
      VarBinding.Context := OclCondition.Context;
      VarBinding.Query.AddWCF(TBoldSQLWCFBinaryInfix.CreateWCFForIdList(MainTableRef.GetColumnReference(IDCOLUMN_NAME), OclCondition.Context));
    end else if VarBinding.IsExternal and (VarBinding.TopSortedIndex > -1) then
    begin
      VarBinding.NewQuery(NameSpace);

      MainTableRef := VarBinding.TableReferenceForTable(VarBinding.ObjectMapper.MainTable, VarBinding.Query, true);
      VarBinding.Context := TBoldObjectIdList.Create;
      aVariableIDLists.Add(VarBinding.Context);
      BoldID := TBoldDefaultID.CreateWithClassID(VarBinding.ObjectMapper.TopSortedIndex, True);
      BoldID.AsInteger := VarBinding.ExternalVarvalue;
      VarBinding.Context.AddAndAdopt(BoldID);
      VarBinding.Query.AddWCF(TBoldSQLWCFBinaryInfix.CreateWCFForIdList(MainTableRef.GetColumnReference(IDCOLUMN_NAME), VarBinding.Context));
    end;
  end;

begin
  if BoldCondition is TBoldOclCondition then
  begin
    SQLGenerator := nil;
    SQlNodeResolver := nil;
    GlobalNameSpace := nil;
    SQLNodeMaker := nil;
    q2 := GetQuery;
    if q2.QueryInterface(IBoldParameterized, vBoldParameterized) = S_OK then
    begin
      vBoldParameterized.ParamCheck := false;
      vBoldParameterized := nil;
    end;
    sql := TStringList.Create;
    OclCondition := BoldCondition as TBoldOclCondition;
    aVariableIDLists := TBoldObjectArray.Create(0, [bcoDataOwner]);
    try
      SQLNodeMaker := TBoldSQlNodeMaker.Create(OclCondition);
      SQLNodeMaker.Execute;

      SQlNodeResolver := TBoldSqlNodeResolver.Create(self, SQlNodeMaker.RootNode, SQLNodeMaker.SQLVarBindings);
      SQlNodeResolver.Execute;

      GlobalNameSpace := TBoldSqlnameSpace.Create;

      SQlGenerator := TBoldSqlQueryGenerator.Create(GlobalNameSpace);
      for i := 0 to SQLNodeMaker.SQLVarBindings.Count - 1 do
      begin
        SQLNodeMaker.SQLVarBindings[i].AcceptVisitor(SQLGenerator);
        FixQueriesForEnv(SQLNodeMaker.SQLVarBindings[i] as TBoldSqlVariableBinding, OclCondition.Context, GlobalNameSpace);
      end;

      SQLNodeMaker.RootNode.AcceptVisitor(SQLGenerator);
      SQLNodeMaker.RootNode.Query.GenerateSQL(sql);

      Q2.AssignSQL(sql);
      Q2.AssignParams(SQLNodeMaker.RootNode.Query.Params);

      if Assigned(OnPsEvaluate) then
        OnPsEvaluate(Q2);

      BoldCondition.AvailableAnswers := GetListUsingQuery(ObjectIdList, ValueSpace, Q2, NO_CLASS, 1, 0, FetchMode, TranslationList, BOLDMAXTIMESTAMP, BoldCondition.MaxAnswers, BoldCondition.Offset);
    finally
      ReleaseQuery(q2);
      sql.Free;
      SQLGenerator.Free;
      SQlNodeResolver.free;
      GlobalNameSpace.Free;
      SQLNodeMaker.Free;
      aVariableIDLists.Free;
    end;
  end
  else if BoldCondition is TBoldChangePointCondition then
  begin
    GlobalNameSpace := TBoldSqlnameSpace.Create;
    aCPCond := BoldCondition as TBoldChangePointCondition;

    for i := aCPCond.IdList.Count - 1 downto 0 do
      if not aCPCond.IdList[i].IsStorable then
        aCPCond.IdList.RemoveByIndex(i);

    if aCPCond.IdList.Count > 0 then
    begin
      q2 := GetQuery;
      try
        GetChangePointsQuery(q2, aCPCond.IdList, aCPCond.StartTime, aCPCond.EndTime, GlobalNameSpace);
        BoldCondition.AvailableAnswers := GetListUsingQuery(ObjectIdList, ValueSpace, Q2, NO_CLASS, 1, 0, FetchMode, TranslationList, BOLDINVALIDTIMESTAMP, BoldCondition.MaxAnswers, BoldCondition.Offset);
        (CommonSuperClassObjectMapper(aCPCond.IdList) as TBoldObjectDefaultMapper).GetChangePoints(ObjectIdList, aCPCond, GlobalNameSpace);
      finally
        ReleaseQuery(q2);
        GlobalNameSpace.Free;
      end;
    end;
  end
  else
    inherited;
end;

procedure TBoldSystemDefaultMapper.GetChangePointsQuery(Query: IBoldQuery;
  IdList: TBoldObjectIdList; StartTime, EndTime: TBoldTimestampType; NameSpace: TBoldSqlnameSpace);
var
  anSQLQuery: TBoldSQLQuery;
  aTableRef: TBoldSQLTableReference;
  aTableDesc: TBoldSQLTableDescription;
  sql: TStringList;
  StartColumnRef: TBoldSQLColumnReference;
  BoldGuard: IBoldGuard;
begin
  BoldGuard := TBoldGuard.Create(anSQLQuery, sql);
  anSQLQuery := TBoldSQLQuery.Create(qmSelect, PSSystemDescription, SQLDataBaseConfig, NameSpace);
  anSQLQuery.IgnoreHistoricObjects := false;
  sql := TStringList.Create;

  aTableDesc := RootClassObjectPersistenceMapper.MainTable;
  aTableRef := anSQLQuery.AddTableReference(aTableDesc.SQLName);
  StartColumnRef := aTableRef.GetColumnReference(TIMESTAMPSTARTCOLUMNNAME);

  anSQLQuery.AddColumnToRetrieve(aTableRef.GetColumnReference(IDCOLUMN_NAME));
  anSQLQuery.AddColumnToRetrieve(aTableRef.GetColumnReference(TYPECOLUMN_NAME));
  anSQLQuery.AddColumnToRetrieve(StartColumnRef);
  anSQLQuery.AddWCF(TBoldSQLWCFBinaryInfix.CreateWCFForIdList(aTableRef.GetColumnReference(IDCOLUMN_NAME), IdList));
  anSQLQuery.AddWCF(TBoldSQLWCFBinaryInfix.Create(TBoldSQLWCFColumnRef.Create(StartColumnRef),
                                                  TBoldSQLWCFInteger.Create(StartTime),
                                                  '>='));
  anSQLQuery.AddWCF(TBoldSQLWCFBinaryInfix.Create(TBoldSQLWCFColumnRef.Create(StartColumnRef),
                                                  TBoldSQLWCFInteger.Create(EndTime),
                                                  '<='));

  anSQLQuery.GenerateSQL(SQL);
  Query.AssignSQL(SQL);
end;


procedure TBoldObjectDefaultMapper.GetChangePoints(
  ObjectIDList: TBoldObjectIdList; Condition: TBoldChangePointCondition; NameSpace: TBoldSqlnameSpace);
var
  i: Integer;
begin
  if Assigned(Condition.MemberIdList) then
    for i := 0 to Condition.MemberIdList.Count - 1 do
      (MemberPersistenceMappers[MemberMapperIndexByMemberIndex[Condition.MemberIdList[i].MemberIndex]] as TBoldMemberDefaultMapper).GetChangePoints(ObjectIdList, Condition, NameSpace);
end;

function TBoldMemberDefaultMapper.CompareFields(const ObjectContent: IBoldObjectContents; const DataSet: IBoldDataSet; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList): Boolean;
var
  aField    : IBoldField;
  ColumnIndex: Integer;
begin
  result := true;
  if SupportsComparingWithoutValue or assigned(GetValue(ObjectContent)) then
  begin
    for ColumnIndex := 0 to ColumnCount - 1 do
    begin
      aField := DataSet.FieldByUpperCaseName(ColumnDescriptions[ColumnIndex].SQLNameUpper);
      if Assigned(aField) then
      begin
        if not CompareField(ObjectContent, aField, ColumnIndex, ValueSpace, TranslationList) then
        begin
          BoldLog.LogFmt(sOptimisticLockingFailed,
            [ObjectPersistenceMapper.ExpressionName,
            ExpressionName,
            ObjectContent.ObjectId.AsString,
            ColumnIndex,
            ColumnDescriptions[ColumnIndex].SQLName,
            aField.AsString,
            ObjectContent.Timestamp]
            );
          result := false;
        end;
      end
      else
        raise EBoldInternal.CreateFmt(sSomeColumnsNotInTable, [classname, 'CompareFields', ColumnIndex, ColumnDescriptions[ColumnIndex].SQLName]); // do not localize
    end;
  end;
end;

procedure TBoldMemberDefaultMapper.GetChangePoints(
  ObjectIDList: TBoldObjectIdList; Condition: TBoldChangePointCondition; NameSpace: TBoldSqlnameSpace);
begin
  raise EBold.CreateFmt(sNotSupportedOnMember, [classname]);
end;

procedure TBoldSystemDefaultMapper.PMTimeForTimestamp(
  Timestamp: TBoldTimestampType; var ClockTime: TDateTime);
var
  aQuery: IBoldQuery;
begin
  aQuery := GetQuery;
  try
    aQuery.AssignSQLText(
      format('SELECT %s FROM %s WHERE :time1 >= %s and :time2 < %s', [
        ClockLogTableLastClockColumn.SQLname,
        PSSystemDescription.ClockLogTable.SQLName,
        ClockLogTableLastTimeStampColumn.SQLName,
        ClockLogTableThisTimeStampColumn.SQLName]));
    aQuery.ParamByName('time1').AsInteger := Timestamp;
    aQuery.ParamByName('time2').AsInteger := Timestamp;
    aQuery.Open;
    if not aQuery.EOF then
      ClockTime := aQuery.Fields[0].AsDateTime
    else
      ClockTime := GetCorrectTime;
    aQuery.Close;
  finally
    ReleaseQuery(aQuery);
  end;
end;

procedure TBoldSystemDefaultMapper.PMTimestampForTime(ClockTime: TDateTime;
  var Timestamp: TBoldTimestampType);
var
  aQuery: IBoldQuery;
begin
  aQuery := GetQuery;
  try
    aQuery.AssignSQLText(
      Format('SELECT %s FROM %s WHERE :time1 > %s and :time2 <= %s', [
        ClockLogTableThisTimeStampColumn.SQLName,
        PSSystemDescription.ClockLogTable.SQLName,
        ClockLogTableLastClockColumn.SQLname,
        ClockLogTableThisClockColumn.SQLname]));
    aQuery.ParamByName('time1').AsDateTime := ClockTime;
    aQuery.ParamByName('time2').AsDateTime := ClockTime;
    aQuery.Open;
    if not aQuery.EOF then
      Timestamp := aQuery.Fields[0].AsInteger
    else
      Timestamp := BoldMaxTimestamp;
    aQuery.Close;
  finally
    ReleaseQuery(aQuery);
  end;
end;

function TBoldMemberDefaultMapper.SupportsComparingWithoutValue: Boolean;
begin
  result := false;
end;

{ EBoldOptimisticLockingFailed }

constructor EBoldOptimisticLockingFailed.Create(msg: string; args: array of const; FailedObjects: TBoldObjectIdList);
begin
  inherited Create(Msg, Args, FailedObjects);
end;

destructor TBoldObjectDefaultMapper.Destroy;
var
  i: integer;
begin
  for I := 0 to Length(fQueryCache) - 1 do
  begin
    fQueryCache[i].SqlStrings.Free;
    fQueryCache[i].MemberList.Free;
    fQueryCache[i].MemberPMList.Free;
    fQueryCache[i].CustomMembers.Free;
  end;
  for I := 0 to Length(fPMCreateCache) - 1 do
  begin
    fPMCreateCache[i].SqlStrings.Free;
    fPMCreateCache[i].MemberPMList.Free;
  end;
  fSingleLinkList.free;
  inherited;
end;

procedure TBoldObjectDefaultMapper.DetectLinkClassDuplicates(
  ObjectIdList: TBoldObjectidList; const ValueSpace: IBoldvalueSpace;
  TranslationList: TBoldIdTranslationList;
  DuplicateList: TBoldObjectIdList);
var
  Query: IBoldQuery;
  QueryText: string;
  LinkMapper1, LinkMapper2: TBoldEmbeddedSingleLinkDefaultMapper;
  LInkObject: IBoldObjectcontents;
  Id1, Id2: TBoldObjectId;
  i: integer;
  OldLinkObjectId: TBoldObjectId;

  function ObjectIsNew(Id: TBoldObjectId): Boolean;
  var
    Obj: IBoldObjectContents;
  begin
    Obj := ValueSpace.ObjectContentsByObjectId[Id];
    result := assigned(Obj) and
      (Obj.BoldExistenceState = besExisting) and
      (Obj.BoldPersistenceState = bvpsModified);
  end;

begin
  LinkMapper1 := (LinkClassRole1 as TBoldEmbeddedSingleLinkDefaultMapper);
  LinkMapper2 := (LinkClassRole2 as TBoldEmbeddedSingleLinkDefaultMapper);


  QueryText := format('SELECT %s, %s FROM %s WHERE (%s = %%s) AND (%s = %%s)', [
    IDCOLUMN_NAME, TYPECOLUMN_NAME,
    MainTable.SQLName,
    LinkMapper1.MainColumnName, LinkMapper2.MainColumnName]);
  Query := SystemPersistenceMapper.GetQuery;
  try
    for i := 0 to ObjectIdList.Count - 1 do
    begin
      LinkObject := ValueSpace.EnsuredObjectContentsByObjectId[ObjectIdList[i]];
      Id1 := (LinkMapper1.GetValue(LinkObject) as IBoldObjectIdRef).Id;
      Id2 := (LinkMapper2.GetValue(LinkObject) as IBoldObjectIdRef).Id;
      if (assigned(Id1) and assigned(Id2)) and not (ObjectisNew(Id1) or ObjectIsNew(Id2)) then
      begin
        if assigned(Id1) then
          Id1 := TranslationList.TranslateToNewId[Id1];
        if assigned(Id2) then
          Id2 := TranslationList.TranslateToNewId[Id2];


        if assigned(Id1) and assigned(Id2) then
        begin
          Query.AssignSQLText(format(QueryText, [Id1.AsString, Id2.AsString]));
          Query.open;
          if not query.Eof then
          begin
            OldLinkObjectId := SystemPersistenceMapper.NewIdFromQuery(Query, NO_CLASS, 1, 0, BOLDMAXTIMESTAMP);
            TranslationList.addTranslationAdoptNew(TranslationList.TranslateToNewId[ObjectIdList[i]], OldLinkObjectId);
            DuplicateList.Add(ObjectIdList[i]);
          end;
          Query.Close;
        end;
      end;
    end;

  finally
    SystemPersistenceMapper.ReleaseQuery(Query);
  end;
end;

function TBoldMemberDefaultMapper.CheckEitherNull(const field: IBoldField;
  const Value: IBoldValue; var Equal: Boolean): Boolean;
begin
  Equal := false;
  result := false;
  if Field.IsNull then
  begin
    Equal := (value as IBoldNullableValue).isNull;
    result := true;
  end
  else if (value as IBoldNullableValue).IsNull then
  begin
    equal := false;
    result := true;
  end;
end;

function TBoldSystemDefaultMapper.CreateMappingInfo: TBoldSQLMappingInfo;
begin
  result := TBoldDefaultMappingInfo.Create(SQLDataBaseConfig.SystemTablePrefix, SQLDataBaseConfig.MaxDBIdentifierLength, NationalCharConversion);
end;

procedure TBoldMemberDefaultMapper.InitializePSDescriptions;
var
  MemberMappings: TBoldMemberMappingArray;
  Columns: TStringList;
  i: Integer;
  BoldGuard: IBoldGuard;
begin
  BoldGuard := TBoldGuard.Create(Columns);
  MemberMappings := SystemPersistenceMapper.MappingInfo.GetMemberMappings(ObjectPersistenceMapper.ExpressionName, ExpressionName);
  if Length(MemberMappings) = 1 then
  begin
    Columns := TStringList.Create;
    Columns.CommaText := MemberMappings[0].Columns;
    if Columns.Count <> ColumnCount then
      raise EBold.CreateFmt(sUnsupportedMappingChange, [ObjectPersistenceMapper.ExpressionName, ExpressionName]);

    ColumnDescriptions.Capacity := ObjectPersistenceMapper.MemberPersistenceMappers.Count;
    for i := 0 to Columns.Count - 1 do
      ColumnDescriptions.Add(
        SystemPersistenceMapper.EnsureColumn(MemberMappings[0].TableName,
          Columns[i],
          ColumnTypeAsSQL[i],
          AllowNullAsSQL,
          ColumnBDEFieldType[i],
          ColumnSize[i],
          AllowNull,
          ObjectPersistenceMapper.Versioned,
          DefaultDbValue));
{$IFDEF IndexColumn}
    if MemberMappings[0].ColumnIndex then begin
      for i := 0 to Columns.Count - 1 do
        SystemPersistenceMapper.EnsureIndex(MemberMappings[0].TableName,
            Columns[i], False, False, True, False);
    end;
{$ENDIF}
  end
  else if (length(memberMappings) = 0) and RequiresMemberMapping then
    raise EBold.CreateFmt(sUnableToFindMappingForX, [ObjectPersistenceMapper.ExpressionName, ExpressionName]);
end;

procedure TBoldObjectDefaultMapper.InitializePSDescriptions;
begin

  if SystemPersistenceMapper.UseXFiles then
    AllTables.Add(SystemPersistenceMapper.PSSystemDescription.XFilestable);

  inherited;

  if not assigned(SuperClass) then
    SystemPersistenceMapper.PSSystemDescription.RootTable := MainTable;

  if Versioned and (self = SystemPersistenceMapper.RootClassObjectPersistenceMapper) then
  begin
    SystemPersistenceMapper.EnsureColumn(MainTable.SQLName, TIMESTAMPSTOPCOLUMNNAME, SystemPersistenceMapper.SQLDataBaseConfig.ColumnTypeForInteger, SystemPersistenceMapper.SQLDataBaseConfig.EffectiveSQLforNotNull, BOLDTIMESTAMPFIELDTYPE, 0, true, true, SystemPersistenceMapper.SQLDataBaseConfig.CorrectlyQuotedDefaultValue(intTostr(BOLDMAXTIMESTAMP)));
    MainTable.EnsureIndex(TIMESTAMPSTOPCOLUMNNAME, false, false, false);
    MainTable.ContainsStopTimeStamp := true;
  end;
end;

procedure TBoldSystemDefaultMapper.InitializePSDescriptions;
const
  MappingStringLength = 60;
var
  DefaultStringLength: integer;
begin
  DefaultStringLength := SQLDataBaseConfig.DefaultStringLength;
  PSSystemDescription.SQLTablesList.Capacity := ObjectPersistenceMappers.Count + 10;
  PSSystemDescription.IdTable := TBoldSQLTableDescription.Create(PSSystemDescription, false);
  PSSystemDescription.IdTable.SQLName := IDTABLE_NAME;
  PSSystemDescription.IdTable.AddColumn(IDCOLUMN_NAME, SQLDataBaseConfig.ColumnTypeForInteger, SQLDataBaseConfig.EffectiveSQLforNotNull, IDCOLUMN_TYPE, 0, False, '');
  PSSystemDescription.TypeTable := TBoldSQLTableDescription.Create(PSSystemDescription, false);
  PSSystemDescription.TypeTable.SQLName := TYPETABLE_NAME;
  PSSystemDescription.TypeTable.AddColumn(TYPECOLUMN_NAME, SQLDataBaseConfig.ColumnTypeForSmallInt, SQLDataBaseConfig.EffectiveSQLforNotNull, TYPECOLUMN_TYPE, 0, False, '');
  PSSystemDescription.TypeTable.AddColumn(CLASSNAMECOLUMN_NAME, format(SQLDataBaseConfig.ColumnTypeForString, [DefaultStringLength]), SQLDataBaseConfig.EffectiveSQLforNotNull, ftString, DefaultStringLength, False, '');
  if UseXFiles then
  begin
    PSSystemDescription.XFilesTable := TBoldSQLTableDescription.Create(PSSystemDescription, false);
    PSSystemDescription.XFilesTable.SQLName := TABLEPREFIXTAG + '_XFILES';
    PSSystemDescription.XFilesTable.AddColumn(IDCOLUMN_NAME, SQLDataBaseConfig.ColumnTypeForInteger, SQLDataBaseConfig.EffectiveSQLforNotNull, IDCOLUMN_TYPE, 0, False, '');
    PSSystemDescription.XFilesTable.AddColumn(TYPECOLUMN_NAME, SQLDataBaseConfig.ColumnTypeForSmallInt, SQLDataBaseConfig.EffectiveSQLforNotNull, TYPECOLUMN_TYPE, 0, False, '');
    PSSystemDescription.XFilesTable.EnsureIndex(IDCOLUMN_NAME, true, true, false);
    if UseGlobalId then
      fXFilesGlobalIdColumn := PSSystemDescription.XFilesTable.AddColumn(GLOBALIDCOLUMN_NAME, format(SQLDataBaseConfig.ColumnTypeForString, [DefaultStringLength]), SQLDataBaseConfig.EffectiveSQLforNotNull, ftString, DefaultStringLength, False, '');
    if UseTimestamp then
      fXFilesTimeStampColumn := PSSystemDescription.XFilesTable.AddColumn(TIMESTAMPCOLUMN_NAME, SQLDataBaseConfig.ColumnTypeForInteger, SQLDataBaseConfig.EffectiveSQLforNotNull, BOLDTIMESTAMPFIELDTYPE, 0, False, '');
  end;

  PSSystemDescription.TableTable := TBoldSQLTableDescription.Create(PSSystemDescription, false);
  PSSystemDescription.TableTable.SQLName := TABLETABLE_NAME;
  PSSystemDescription.TableTable.AddColumn(TABLENAMECOLUMN_NAME, format(SQLDataBaseConfig.ColumnTypeForString, [DefaultStringLength]), SQLDataBaseConfig.EffectiveSQLforNotNull, ftString, DefaultStringLength, False, '');

  if UseTimestamp then
  begin
    PSSystemDescription.TimeStampTable := TBoldSQLTableDescription.Create(PSSystemDescription, false);
    PSSystemDescription.TimeStampTable.SQLName := TIMESTAMPTABLE_NAME;
    fTimeStampTableTimeStampColumn := PSSystemDescription.TimeStampTable.AddColumn(TIMESTAMPCOLUMN_NAME, SQLDataBaseConfig.ColumnTypeForInteger, SQLDataBaseConfig.EffectiveSQLforNotNull, ftInteger, 0, False, '');
  end;

  if UseClockLog then
  begin
    PSSystemDescription.LastClockTable := TBoldSQLTableDescription.Create(PSSystemDescription, false);
    PSSystemDescription.LastClockTable.SQLName := LASTCLOCKTABLE_NAME;
    fLastClockTableLastTimeStampColumn := PSSystemDescription.LastClockTable.AddColumn(LASTTIMESTAMPCOLUMN_NAME, SQLDataBaseConfig.ColumnTypeForInteger, SQLDataBaseConfig.EffectiveSQLforNotNull, ftInteger, 0, False, '');
    fLastClockTableLastClockColumn := PSSystemDescription.LastClockTable.AddColumn(LASTCLOCKCOLUMN_NAME, SQLDataBaseConfig.ColumnTypeForDateTime, SQLDataBaseConfig.EffectiveSQLforNotNull, ftDateTime, 0, False, '');

    PSSystemDescription.ClockLogTable := TBoldSQLTableDescription.Create(PSSystemDescription, false);
    PSSystemDescription.ClockLogTable.SQLName := CLOCKLOGTABLE_NAME;
    fClockLogTableLastTimeStampColumn := PSSystemDescription.ClockLogTable.AddColumn(LASTTIMESTAMPCOLUMN_NAME, SQLDataBaseConfig.ColumnTypeForInteger, SQLDataBaseConfig.EffectiveSQLforNotNull, ftInteger, 0, False, '');
    fClockLogTableThisTimeStampColumn := PSSystemDescription.ClockLogTable.AddColumn(THISTIMESTAMPCOLUMN_NAME, SQLDataBaseConfig.ColumnTypeForInteger, SQLDataBaseConfig.EffectiveSQLforNotNull, ftInteger, 0, False, '');
    fClockLogTableLastClockColumn := PSSystemDescription.ClockLogTable.AddColumn(LASTCLOCKCOLUMN_NAME, SQLDataBaseConfig.ColumnTypeForDateTime, SQLDataBaseConfig.EffectiveSQLforNotNull, ftDateTime, 0, False, '');
    fClockLogTableThisClockColumn := PSSystemDescription.ClockLogTable.AddColumn(THISCLOCKCOLUMN_NAME, SQLDataBaseConfig.ColumnTypeForDateTime, SQLDataBaseConfig.EffectiveSQLforNotNull, ftDateTime, 0, False, '');
  end;

  PSSystemDescription.MemberMappingTable := TBoldSQLTableDescription.Create(PSSystemDescription, false);
  PSSystemDescription.MemberMappingTable.SQLName := MemberMappingTable_NAME;
  PSSystemDescription.MemberMappingTable.AddColumn(MMT_CLASSNAME_COLUMN, format(SQLDataBaseConfig.ColumnTypeForString, [MappingStringLength]), SQLDataBaseConfig.EffectiveSQLforNotNull, ftString, MappingStringLength, False, '');
  PSSystemDescription.MemberMappingTable.AddColumn(MMT_MEMBERNAME_COLUMN, format(SQLDataBaseConfig.ColumnTypeForString, [MappingStringLength]), SQLDataBaseConfig.EffectiveSQLforNotNull, ftString, MappingStringLength, False, '');
  PSSystemDescription.MemberMappingTable.AddColumn(MMT_TABLENAME_COLUMN, format(SQLDataBaseConfig.ColumnTypeForString, [MappingStringLength]), SQLDataBaseConfig.EffectiveSQLforNotNull, ftString, MappingStringLength, False, '');
  PSSystemDescription.MemberMappingTable.AddColumn(MMT_COLUMNS_COLUMN, format(SQLDataBaseConfig.ColumnTypeForString, [MappingStringLength]), SQLDataBaseConfig.EffectiveSQLforNotNull, ftString, MappingStringLength, False, '');
  PSSystemDescription.MemberMappingTable.AddColumn(MMT_MAPPERNAME_COLUMN, format(SQLDataBaseConfig.ColumnTypeForString, [MappingStringLength]), SQLDataBaseConfig.EffectiveSQLforNotNull, ftString, MappingStringLength, False, '');
{$IFDEF IndexColumn}
  PSSystemDescription.MemberMappingTable.AddColumn(MMT_INDEX_COLUMN, SQLDataBaseConfig.ColumnTypeForInteger, SQLDataBaseConfig.EffectiveSQLforNotNull, ftBoolean, 0, False, '');
{$ENDIF}
  PSSystemDescription.AllInstancesMappingTable := TBoldSQLTableDescription.Create(PSSystemDescription, false);
  PSSystemDescription.AllInstancesMappingTable.SQLName := AllInstancesMappingTable_NAME;
  PSSystemDescription.AllInstancesMappingTable.AddColumn(AID_CLASSNAME_COLUMN, format(SQLDataBaseConfig.ColumnTypeForString, [MappingStringLength]), SQLDataBaseConfig.EffectiveSQLforNotNull, ftString, MappingStringLength, False, '');
  PSSystemDescription.AllInstancesMappingTable.AddColumn(AID_TABLENAME_COLUMN, format(SQLDataBaseConfig.ColumnTypeForString, [MappingStringLength]), SQLDataBaseConfig.EffectiveSQLforNotNull, ftString, MappingStringLength, False, '');
  PSSystemDescription.AllInstancesMappingTable.AddColumn(AID_CLASSIDREQUIRED_COLUMN, SQLDataBaseConfig.ColumnTypeForInteger, SQLDataBaseConfig.EffectiveSQLforNotNull, ftInteger, 0, False, '');

  PSSystemDescription.ObjectStorageMappingTable := TBoldSQLTableDescription.Create(PSSystemDescription, false);
  PSSystemDescription.ObjectStorageMappingTable.SQLName := ObjectStorageMappingTable_NAME;
  PSSystemDescription.ObjectStorageMappingTable.AddColumn(ST_CLASSNAME_COLUMN, format(SQLDataBaseConfig.ColumnTypeForString, [MappingStringLength]), SQLDataBaseConfig.EffectiveSQLforNotNull, ftString, MappingStringLength, False, '');
  PSSystemDescription.ObjectStorageMappingTable.AddColumn(ST_TABLENAME_COLUMN, format(SQLDataBaseConfig.ColumnTypeForString, [MappingStringLength]), SQLDataBaseConfig.EffectiveSQLforNotNull, ftString, MappingStringLength, False, '');
  inherited;
end;

function TBoldMemberDefaultMapper.RequiresMemberMapping: Boolean;
begin
  result := ColumnCount > 0;
end;

function TBoldMemberDefaultMapper.FindDefiningTable(LocalMoldClass: TMoldClass; MoldMember: TMoldMember): string;
var
  StoringClass: TMoldClass;
begin
  StoringClass := LocalMoldClass.FindStoringClass(MoldMember.MoldClass, false, MoldMember);
  if assigned(StoringClass) then
    result := BoldExpandPrefix(StoringClass.TableName, StoringClass.Name, SystemPersistenceMapper.SQLDatabaseConfig.SystemTablePrefix, SystemPersistenceMapper.SQLDataBaseConfig.MaxDbIdentifierLength, StoringClass.Model.NationalCharConversion)
  else
    result := '';
end;

{TBoldSingleColumnMember}

function TBoldSingleColumnMember.GetColumnCount: Integer;
begin
  Result := 1;
end;

function TBoldSingleColumnMember.GetColumnSize(ColumnIndex: Integer): Integer;
begin
  Result := 0;
end;

class procedure TBoldSingleColumnMember.EnsureFirstColumn(ColumnIndex: Integer);
begin
  if ColumnIndex <> 0 then
    raise EBoldBadColumnIndex.CreateFmt(sIllegalColumnIndex, [ClassName, 'EnsureFirstColumn', ColumnIndex]); // do not localize
end;

{ TBoldModelVersionMember }

function TBoldModelVersionMember.CompareField(
  const ObjectContent: IBoldObjectContents; const Field: IBoldField; ColumnIndex: integer;
  const ValueSpace: IBoldValueSpace;
  TranslationList: TBoldIdTranslationList): Boolean;
begin
  result := true;
end;

constructor TBoldModelVersionMember.CreateFromMold(moldMember: TMoldMember;
  moldClass: TMoldClass; Owner: TBoldObjectPersistenceMapper;
  const MemberIndex: Integer;
  TypeNameDictionary: TBoldTypeNameDictionary);
begin
  fExpressionname := '_' + MODELVERSIONCOLUMN_NAME;
  fDefaultDbValue := '';
  fAllowNull := false;
  fDelayedFetch := false;
  fContentName := '';
  fIsStoredInObject := true;
  fInitialColumnRootName := MODELVERSIONCOLUMN_NAME;
  fVersionNumber := moldClass.Model.ModelVersion;
  inherited;
end;

function TBoldModelVersionMember.FindDefiningTable(LocalMoldClass: TMoldClass; MoldMember: TMoldMember): string;
var
  RootClass: TMoldClass;
begin
  RootClass := LocalMoldClass.Model.RootClass;
  result := BoldExpandPrefix(RootClass.TableName, RootClass.Name, SystemPersistenceMapper.SQLDatabaseConfig.SystemTablePrefix, SystemPersistenceMapper.SQLDataBaseConfig.MaxDbIdentifierLength, RootClass.Model.NationalCharConversion);
end;

function TBoldModelVersionMember.GetColumnBDEFieldType(
  ColumnIndex: Integer): TFieldType;
begin
  result := ftInteger;
end;

function TBoldModelVersionMember.GetColumnTypeAsSQL(ColumnIndex: Integer): string;
begin
  result := SystemPersistenceMapper.SQLDataBaseConfig.ColumnTypeForInteger;
end;

function TBoldModelVersionMember.IsDirty(
  const ObjectContents: IBoldObjectContents): Boolean;
begin
  result := true;
end;

function TBoldModelVersionMember.ShouldFetch(
  const ObjectContents: IBoldObjectContents): Boolean;
begin

  result := true;
end;

function TBoldModelVersionMember.ValueAsVariant(
  const ObjectContent: IBoldObjectContents; ColumnIndex: Integer;
  TranslationList: TBoldIdTranslationList): variant;
begin
  result := VersionNumber;
end;

procedure TBoldModelVersionMember.ValueFromField(
  OwningObjectId: TBoldObjectId; const ObjectContent: IBoldObjectContents;
  const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList;
  const Field: IBoldField; ColumnIndex: Integer);
begin
end;

procedure TBoldModelVersionMember.ValueToParam(
  const ObjectContent: IBoldObjectContents; const Param: IBoldParameter;
  ColumnIndex: Integer; TranslationList: TBoldIdTranslationList);
begin
  Param.AsInteger := VersionNumber;
end;

function TBoldModelVersionMember.VersionFromQuery(const Query: IBoldQuery): Integer;
var
  aField: IBoldField;
begin
  aField := Query.FieldByUpperCaseName(ColumnDescriptions[0].SQLNameUpper);
  if assigned(aField) then
    result := aField.AsInteger
  else
    raise EBoldInternal.CreateFmt(sColumnNotFoundInTable, [classname, ColumnDescriptions[0].SQLName]);
end;

{ TBoldReadOnlynessMember }

function TBoldReadOnlynessMember.CompareField(
  const ObjectContent: IBoldObjectContents; const Field: IBoldField; ColumnIndex: integer;
  const ValueSpace: IBoldValueSpace;
  TranslationList: TBoldIdTranslationList): Boolean;
begin
  result := true;
end;

constructor TBoldReadOnlynessMember.CreateFromMold(moldMember: TMoldMember;
  moldClass: TMoldClass; Owner: TBoldObjectPersistenceMapper;
  const MemberIndex: Integer;
  TypeNameDictionary: TBoldTypeNameDictionary);
begin
  fExpressionname := '_' + READONLYCOLUMN_NAME;
  fDefaultDbValue := '';
  fAllowNull := false;
  fDelayedFetch := false;
  fContentName := '';
  fIsStoredInObject := true;
  fInitialColumnRootName := READONLYCOLUMN_NAME;
  inherited;
end;

function TBoldReadOnlynessMember.FindDefiningTable(LocalMoldClass: TMoldClass; MoldMember: TMoldMember): string;
var
  RootClass: TMoldClass;
begin
  RootClass := LocalMoldClass.Model.RootClass;
  result := BoldExpandPrefix(RootClass.TableName, RootClass.Name, SystemPersistenceMapper.SQLDatabaseConfig.SystemTablePrefix, SystemPersistenceMapper.SQLDataBaseConfig.MaxDbIdentifierLength, RootClass.Model.NationalCharConversion);
end;

function TBoldReadOnlynessMember.GetColumnBDEFieldType(
  ColumnIndex: Integer): TFieldType;
begin
  result := ftInteger;
end;

function TBoldReadOnlynessMember.GetColumnTypeAsSQL(
  ColumnIndex: Integer): string;
begin
  result := SystemPersistenceMapper.SQLDataBaseConfig.ColumnTypeForInteger;
end;

function TBoldReadOnlynessMember.IsDirty(
  const ObjectContents: IBoldObjectContents): Boolean;
begin
  result := false;
end;

function TBoldReadOnlynessMember.ShouldFetch(
  const ObjectContents: IBoldObjectContents): Boolean;
begin
  result := true;
end;

function TBoldReadOnlynessMember.ValueAsVariant(
  const ObjectContent: IBoldObjectContents; ColumnIndex: Integer;
  TranslationList: TBoldIdTranslationList): variant;
begin
  result := 0;
end;

procedure TBoldReadOnlynessMember.ValueFromField(
  OwningObjectId: TBoldObjectId; const ObjectContent: IBoldObjectContents;
  const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList;
  const Field: IBoldField; ColumnIndex: Integer);
begin
  ObjectContent.SetIsReadOnly(field.AsInteger = 1);
end;

procedure TBoldReadOnlynessMember.ValueToParam(
  const ObjectContent: IBoldObjectContents; const Param: IBoldParameter;
  ColumnIndex: Integer; TranslationList: TBoldIdTranslationList);
begin
  Param.AsInteger := 0;
end;

function TBoldObjectDefaultMapper.IsOldVersion(const Query: IBoldQuery): Boolean;
var
  CurrentObjectVersion: integer;
begin
  CurrentObjectVersion := fModelVersionMember.VersionFromQuery(Query);
  result := (CurrentObjectVersion < SystemPersistenceMapper.ModelVersion) and assigned(SystemPersistenceMapper.ObjectUpgrader) and
    SystemPersistenceMapper.ObjectUpgrader.NeedsManualUpdate(ExpressionName, CurrentObjectVersion);
end;

procedure TBoldObjectDefaultMapper.PortObject(ObjectId: TBoldObjectId; const Query: IBoldQuery);
begin
  if assigned(SystemPersistenceMapper.ObjectUpgrader) then
    SystemPersistenceMapper.ObjectUpgrader.UpgradeObjectById(ObjectId, Query);
end;


{ TBoldTimeStampMember }

function TBoldTimeStampMember.CompareField(
  const ObjectContent: IBoldObjectContents; const Field: IBoldField;
  ColumnIndex: integer; const ValueSpace: IBoldValueSpace;
  TranslationList: TBoldIdTranslationList): Boolean;
begin
  if ColumnIndex = 0 then
    result := Field.AsInteger = ObjectContent.TimeStamp
  else
    raise EBold.CreateFmt(sIllegalColumnIndex, [classname, 'CompareField', ColumnIndex]); // do not localize
end;

constructor TBoldTimeStampMember.CreateFromMold(moldMember: TMoldMember;
  moldClass: TMoldClass; Owner: TBoldObjectPersistenceMapper;
  const MemberIndex: Integer; TypeNameDictionary: TBoldTypeNameDictionary);
begin
  fExpressionname := '_' + TIMESTAMPCOLUMN_NAME;
  fDefaultDbValue := '';
  fAllowNull := false;
  fDelayedFetch := MoldClass.EffectiveOptimisticLocking <> bolmTimeStamp;
  fContentName := '';
  fIsStoredInObject := true;
  fInitialColumnRootName := TIMESTAMPCOLUMN_NAME;
  inherited CreateFromMold(MoldMember, MoldClass, Owner, TIMESTAMPMEMBERINDEX, TypeNameDictionary);
end;

function TBoldTimeStampMember.FindDefiningTable(LocalMoldClass: TMoldClass; MoldMember: TMoldMember): string;
begin


  result := BoldExpandPrefix(TABLEPREFIXTAG + '_XFILES', '', SystemPersistenceMapper.SQLDatabaseConfig.SystemTablePrefix, SystemPersistenceMapper.SQLDataBaseConfig.MaxDBIdentifierLength, SystemPersistenceMapper.NationalCharConversion);
end;

function TBoldTimeStampMember.GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType;
begin
  result := BOLDTIMESTAMPFIELDTYPE;
end;

function TBoldTimeStampMember.GetColumnTypeAsSQL(ColumnIndex: Integer): string;
begin
  result := SystemPersistenceMapper.SQLDataBaseConfig.ColumnTypeForInteger;
end;


function TBoldTimeStampMember.IsDirty(const ObjectContents: IBoldObjectContents): Boolean;
begin
  result := true;
end;

function TBoldTimeStampMember.ShouldFetch(const ObjectContents: IBoldObjectContents): Boolean;
begin
  result := ObjectPersistenceMapper.fOptimisticLockingMode = bolmTimeStamp;
end;

function TBoldTimeStampMember.SupportsComparingWithoutValue: Boolean;
begin
  result := true;
end;

function TBoldTimeStampMember.ValueAsVariant(const ObjectContent: IBoldObjectContents;
  ColumnIndex: Integer; TranslationList: TBoldIdTranslationList): variant;
begin
  result := SystemPersistenceMapper.CurrentTimeStamp;;
end;

procedure TBoldTimeStampMember.ValueFromField(
  OwningObjectId: TBoldObjectId; const ObjectContent: IBoldObjectContents;
  const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList;
  const Field: IBoldField; ColumnIndex: Integer);
begin
  ObjectContent.TimeStamp := Field.AsInteger;
end;

procedure TBoldTimeStampMember.ValueToParam(
  const ObjectContent: IBoldObjectContents; const Param: IBoldParameter;
  ColumnIndex: Integer; TranslationList: TBoldIdTranslationList);
begin
  Param.AsInteger := SystemPersistenceMapper.CurrentTimeStamp;
end;

{ TBoldGlobalIdMember }

function TBoldGlobalIdMember.CompareField(
  const ObjectContent: IBoldObjectContents; const Field: IBoldField;
  ColumnIndex: integer; const ValueSpace: IBoldValueSpace;
  TranslationList: TBoldIdTranslationList): Boolean;
begin
  result := true;
end;


constructor TBoldGlobalIdMember.CreateFromMold(moldMember: TMoldMember;
  moldClass: TMoldClass; Owner: TBoldObjectPersistenceMapper;
  const MemberIndex: Integer; TypeNameDictionary: TBoldTypeNameDictionary);
begin
  fExpressionname := '_' + GLOBALIDCOLUMN_NAME;
  fDefaultDbValue := '';
  fAllowNull := false;
  fDelayedFetch := true;
  fContentName := 'String';
  fIsStoredInObject := true;
  fInitialColumnRootName := GLOBALIDCOLUMN_NAME;
  inherited;
end;

function TBoldGlobalIdMember.FindDefiningTable(LocalMoldClass: TMoldClass; MoldMember: TMoldMember): string;
begin


  result := BoldExpandPrefix(TABLEPREFIXTAG + '_XFILES', '', SystemPersistenceMapper.SQLDatabaseConfig.SystemTablePrefix, SystemPersistenceMapper.SQLDataBaseConfig.MaxDBIdentifierLength, SystemPersistenceMapper.NationalCharConversion);
end;

function TBoldGlobalIdMember.GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType;
begin
  result := ftString;
end;

function TBoldGlobalIdMember.GetColumnTypeAsSQL(ColumnIndex: Integer): string;
begin
  result := format(SystemPersistenceMapper.SQLDataBaseConfig.ColumnTypeForString, [39]);
end;


function TBoldGlobalIdMember.IsDirty(
  const ObjectContents: IBoldObjectContents): Boolean;
begin
  result := false;
end;

function TBoldGlobalIdMember.ShouldFetch(const ObjectContents: IBoldObjectContents): Boolean;
begin
  result := false;
end;

function TBoldGlobalIdMember.ValueAsVariant(const ObjectContent: IBoldObjectContents;
  ColumnIndex: Integer; TranslationList: TBoldIdTranslationList): variant;
begin
  result := ObjectContent.GlobalId;
  if result = '' then
    result := ExternalIdGenerator;
end;

procedure TBoldGlobalIdMember.ValueFromField(OwningObjectId: TBoldObjectId;
  const ObjectContent: IBoldObjectContents; const ValueSpace: IBoldValueSpace;
  TranslationList: TBoldIdTranslationList; const Field: IBoldField;
  ColumnIndex: Integer);
begin
  ObjectContent.GlobalId := Field.AsString;
end;

procedure TBoldGlobalIdMember.ValueToParam(
  const ObjectContent: IBoldObjectContents; const Param: IBoldParameter;
  ColumnIndex: Integer; TranslationList: TBoldIdTranslationList);
var
  GlobalId: String;
begin
  GlobalId := ObjectContent.GlobalId;
  if GlobalId = '' then
    GlobalId := ExternalIdGenerator;

  Param.AsString := GlobalId;
end;

{ TBoldXFilesMembers }

procedure TBoldXFilesMembers.InitializePSDescriptions;
var
  i: integer;
begin
  inherited;
  if ColumnDescriptions.Count = 0 then
  begin
    for i := 0 to ColumnCount - 1 do
    begin
      ColumnDescriptions.Add(
      SystemPersistenceMapper.PSSystemDescription.XFilestable.ColumnsList.ItemsBySQLName[GetInitialColumnName(i)]
      );
    end;
  end;
end;

function TBoldXFilesMembers.RequiresMemberMapping: Boolean;
begin
  result := false;
end;

{ TBoldNonXFileTimeStampMember }

function TBoldNonXFileTimeStampMember.CompareField(
  const ObjectContent: IBoldObjectContents; const Field: IBoldField; ColumnIndex: integer;
  const ValueSpace: IBoldValueSpace;
  TranslationList: TBoldIdTranslationList): Boolean;
begin
  if ColumnIndex = 0 then
    result := Field.AsInteger = ObjectContent.TimeStamp
  else
    raise EBold.CreateFmt('%s.CompareField: invalid columnIndex (%d)', [classname, ColumnIndex]);

end;

constructor TBoldNonXFileTimeStampMember.CreateFromMold(moldMember: TMoldMember;
  moldClass: TMoldClass; Owner: TBoldObjectPersistenceMapper;
  const MemberIndex: Integer; TypeNameDictionary: TBoldTypeNameDictionary);
begin
  fExpressionname := '_' + TIMESTAMPCOLUMN_NAME;
  fDefaultDbValue := '';
  fAllowNull := false;
  fDelayedFetch := MoldClass.EffectiveOptimisticLocking <> bolmTimeStamp;
  fContentName := '';
  fIsStoredInObject := true;
  fInitialColumnRootName := TIMESTAMPCOLUMN_NAME;
  inherited CreateFromMold(MoldMember, MoldClass, Owner, TIMESTAMPMEMBERINDEX, TypeNameDictionary);
end;

function TBoldNonXFileTimeStampMember.FindDefiningTable(
  LocalMoldClass: TMoldClass; MoldMember: TMoldMember): string;
var
  TopClassWithOwnTable, RootClass, C: TMoldClass;
begin
  TopClassWithOwnTable := nil;
  RootClass := LocalMoldClass.Model.RootClass;
  C := LocalMoldClass;
  while C <> RootClass do
  begin
    if C.TableMapping = tmOwn then
      TopClassWithOwnTable := C;
    Assert(Assigned(C.SuperClass), C.name);
    C := C.SuperClass;
  end;
  if not Assigned(TopClassWithOwnTable) then
    raise Exception.Create('No table found for timestamp for class: ' + LocalMoldClass.name);
  result := BoldExpandPrefix(TopClassWithOwnTable.TableName, TopClassWithOwnTable.Name, SystemPersistenceMapper.SQLDatabaseConfig.SystemTablePrefix, SystemPersistenceMapper.SQLDataBaseConfig.MaxDbIdentifierLength, RootClass.Model.NationalCharConversion);
end;

function TBoldNonXFileTimeStampMember.GetColumnBDEFieldType(
  ColumnIndex: Integer): TFieldType;
begin
  result := BOLDTIMESTAMPFIELDTYPE;
end;

function TBoldNonXFileTimeStampMember.GetColumnTypeAsSQL(
  ColumnIndex: Integer): string;
begin
  result := SystemPersistenceMapper.SQLDataBaseConfig.ColumnTypeForInteger;
end;

function TBoldNonXFileTimeStampMember.IsDirty(
  const ObjectContents: IBoldObjectContents): Boolean;
begin
  result := true;
end;

function TBoldNonXFileTimeStampMember.ShouldFetch(
  const ObjectContents: IBoldObjectContents): Boolean;
begin
  result := ObjectPersistenceMapper.fOptimisticLockingMode = bolmTimeStamp;
end;

function TBoldNonXFileTimeStampMember.SupportsComparingWithoutValue: Boolean;
begin
  result := true;
end;

function TBoldNonXFileTimeStampMember.ValueAsVariant(
  const ObjectContent: IBoldObjectContents; ColumnIndex: Integer;
  TranslationList: TBoldIdTranslationList): variant;
begin
  result := SystemPersistenceMapper.CurrentTimeStamp;
end;

procedure TBoldNonXFileTimeStampMember.ValueFromField(
  OwningObjectId: TBoldObjectId; const ObjectContent: IBoldObjectContents;
  const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList;
  const Field: IBoldField; ColumnIndex: Integer);
begin
  ObjectContent.TimeStamp := Field.AsInteger;
end;

procedure TBoldNonXFileTimeStampMember.ValueToParam(
  const ObjectContent: IBoldObjectContents; const Param: IBoldParameter;
  ColumnIndex: Integer; TranslationList: TBoldIdTranslationList);
begin
  Param.AsInteger := SystemPersistenceMapper.CurrentTimeStamp;
end;

initialization
  ExternalIDGenerator := BoldCreateGUIDWithBracketsAsString;

  BoldMemberPersistenceMappers.AddDescriptor(TBoldMemberDefaultMapper, alAbstract);
  BoldSystemPersistenceMappers.Add(TBoldSystemPersistenceMapperDescriptor.Create(DEFAULTNAME, TBoldSystemDefaultMapper));
  BoldObjectPersistenceMappers.Add(TBoldObjectPersistenceMapperDescriptor.Create(DEFAULTNAME, TBoldObjectDefaultMapper));

finalization
  if BoldMemberPersistenceMappersAssigned then
      BoldMemberPersistenceMappers.RemoveDescriptorByClass(TBoldMemberDefaultMapper);

  if BoldSystemPersistenceMappersAssigned then
    BoldSystemPersistenceMappers.RemoveDescriptorByName(DEFAULTNAME);
  if BoldObjectPersistenceMappersAssigned then
    BoldObjectPersistenceMappers.RemoveDescriptorByName(DEFAULTNAME);

end.
