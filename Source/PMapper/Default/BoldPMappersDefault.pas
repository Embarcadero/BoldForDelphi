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
  BoldPSDescriptionsDefault,
  BoldSQLMappingInfo,
  BoldPMappers,
  BoldPMappersSQL;

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


 { TBoldSystemDefaultMapper }
  TBoldSystemDefaultMapper = class(TBoldSystemSQLMapper)
  private
    fNextDBID: Longint;
    fLastReservedDBID: Longint;
    fReservedCount: Longint;
    fXFilesTimeStampColumn: TBoldSQLColumnDescription;
    fXFilesGlobalIdColumn: TBoldSQLColumnDescription;
    fTimeStampTableTimeStampColumn: TBoldSQLColumnDescription;
    fLastClockTableLastTimeStampColumn: TBoldSQLColumnDescription;
    fLastClockTableLastClockColumn: TBoldSQLColumnDescription;
    fClockLogTableLastTimeStampColumn: TBoldSQLColumnDescription;
    fClockLogTableThisTimeStampColumn: TBoldSQLColumnDescription;
    fClockLogTableThisClockColumn: TBoldSQLColumnDescription;
    fClockLogTableLastClockColumn: TBoldSQLColumnDescription;
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
    procedure FetchDeletedObjects(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace); override;
    procedure InitializeBoldDbType; override;
    function CreateMappingInfo: TBoldSQLMappingInfo; override;
    procedure InitializePSDescriptions; override;
    function EnsurePrecondition(Precondition: TBoldUpdatePrecondition; TranslationList: TBoldIdTranslationList): Boolean; override;
    function EnsureOptimisticLocking(Precondition: TBoldOptimisticLockingPrecondition; TranslationList: TBoldIdTranslationList): Boolean;
  public
    constructor CreateFromMold(moldModel: TMoldModel; TypeNameDictionary: TBoldTypeNameDictionary; SQlDatabaseConfig: TBoldSQLDatabaseConfig; GetDatabaseFunc: TBoldGetDatabaseEvent);
    procedure PMFetchClassWithCondition(ObjectIDList: TBoldObjectIdList;
                                        ValueSpace: IBoldValueSpace;
                                        BoldCondition: TBoldCondition;
                                        FetchMode: Integer;
                                        TranslationList: TBoldIdTranslationList); override;
    function GetListUsingQuery(ObjectIDList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; aQuery: IBoldQuery; FetchMode: Integer; TranslationList: TBoldIdTranslationList; TimeStamp: TBoldTimeStampType; MaxAnswers: integer = -1; Offset: integer = -1): integer;
    function EnsureTable(const TableName: string; TableVersioned: Boolean): TBoldSQLTableDescription; override;
    function EnsureColumn(const TableName, ColumnName, SQLType, SQLAllowNull: string; const BDEType: TFieldType; Length: Integer; const AllowNull, InVersionedTable: Boolean; const DefaultDBValue: String): TBoldSQLColumnDescription;
    procedure EnsureIndex(const TableName, Fields: string; const PrimaryIndex, Unique, InVersionedTable: Boolean);
    procedure PMTranslateToGlobalIds(ObjectIdList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList); override;
    procedure PMTranslateToLocalIds(GlobalIdList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList); override;
    procedure PMSetReadonlyness(ReadOnlyList, WriteableList: TBoldObjectIdList); override;
    procedure PMTimestampForTime(ClockTime: TDateTime; var Timestamp: TBoldTimestampType); override;
    procedure PMTimeForTimestamp(Timestamp: TBoldTimestampType; var ClockTime: TDateTime); override;
    function NewIdFromQuery(aQuery: IBoldQuery; BoldDbTypeColumn, ObjectIdColumn: integer; Timestamp: TBoldTimeStampType): TBoldObjectId;
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
  end;

  { TBoldObjectDefaultMapper }
  TBoldObjectDefaultMapper = class(TBoldObjectSQLMapper)
  private
    fSubClassesID: string;
    fModelVersionMember: TBoldModelVersionMember;
    fOptimisticLockingMode: TBoldOptimisticLockingMode;
    function GetSystemPersistenceMapper: TBoldSystemDefaultMapper;
    procedure PMUpdateStopTime(ObjectIDList: TBoldObjectIdList);
    procedure GetChangePoints(ObjectIDList: TBoldObjectIdList; Condition: TBoldChangePointCondition; NameSpace: TBoldSqlnameSpace);
    procedure PMMultiPurposeRetrieveExactIdList(ObjectsToFetch: TBoldObjectIdList; ValueSpace: IBoldValueSpace; MemberIdList: TBoldMemberIdList; FetchMode: Integer; TranslationList: TBoldIdTranslationList; MissingList: TBoldObjectIdList; FailureList: TBoldObjectIdList; TimeStamp: TBoldTimeStampType);

    procedure HandleCompareData(FetchedId: TBoldObjectId; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; Query: IBoldQuery; MemberPMList: TBoldMemberPersistenceMapperList; FetchMode: integer; FailureList: TBoldObjectIdList);
    function CompareFieldsToMembers(ObjectID: TBoldObjectId; ValueSpace: IBoldValueSpace; DataSet: IBoldDataSet; memberList: TBoldMemberPersistenceMapperList; TranslationList: TBoldIdTranslationList): Boolean;
    procedure DetectLinkClassDuplicates(ObjectIdList: TBoldObjectidList; ValueSpace: IBoldvalueSpace; TranslationList: TBoldIdTranslationList; DuplicateList: TBoldObjectIdList);
    procedure GenerateMappingInfo(ExpressionName: String; MoldClass: TMoldClass);
    procedure PMTemporalUpdate(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldvalueSpace; TranslationList: TBoldIdTranslationList);
    procedure FetchPreviousSingleLinkValues(ObjectIdList: TBoldObjectIdLIst; Old_Values: IBoldvalueSpace);
    procedure MakeIDsExactUsingTable(ObjectIDList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList; Table: TBoldSQLTableDescription);
    function InternalIdListSegmentToWhereFragment(IdList: TBoldObjectIdList; Start, Stop: integer; Parameterized: IBoldParameterized): String;
    procedure InternalMakeIDsExact(ObjectIDList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList);
  protected
    procedure JoinSQLTableByKey(SQL: TStringList; MainTable, JoinTable: TBoldSQLTableDescription); override;
    procedure SQLForID(Table: TBoldSQLTableDescription; SQL: TStrings; UseAlias: Boolean); override;
    procedure SQLForDistributed(SQL: TStrings; const SQLStyle: TBoldSQLStyle); override;
    procedure PMFetchWithCondition(ObjectIDList: TBoldObjectIdList;
      ValueSpace: IBoldValueSpace; BoldCondition: TBoldCondition; FetchMode: Integer; TranslationList: TBoldIdTranslationList); override;
    function NextExternalObjectId(ValueSpace: IBoldValueSpace; ObjectId: TBoldObjectId): TBoldObjectId; override;
    function DistributableTable: TBoldSQLTableDescription; override;
    procedure PMCompareExactIDList(ObjectIDList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; MemberIdList: TBoldMemberIdList; TranslationList: TBoldIdTranslationList; FailureList: TBoldObjectIdList);
    procedure InitializePSDescriptions; override;
  public
    constructor CreateFromMold(moldClass: TMoldClass; Owner: TBoldSystemPersistenceMapper; TypeNameDictionary: TBoldTypeNameDictionary); override;
    procedure SQLForKey(Table: TBoldSQLTableDescription; SQL: TStrings; const SQLStyle: TBoldSQLStyle; useAlias: Boolean); override;
    function UpdatesMembersInTable(aTable: TBoldSQLTableDescription): Boolean; override;
    function IdListSegmentToWhereFragment(IdList: TBoldObjectIdList; Start, Stop: integer; Query: IBoldExecQuery): String; overload;
    function IdListSegmentToWhereFragment(IdList: TBoldObjectIdList; Start, Stop: integer; Query: IBoldQuery): String; overload;
    procedure MakeIDsExact(ObjectIDList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList); override;
    procedure PMFetchExactIDList(ObjectIDList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; MemberIdList: TBoldMemberIdList; FetchMode: Integer; TranslationList: TBoldIdTranslationList; MissingList: TBoldObjectIdList); override;
    procedure PMDelete(ObjectIDList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; Old_Values: IBoldValueSpace; TranslationList: TBoldIdTranslationList); override;
    procedure PMCreate(ObjectIDList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList); override;
    procedure PMUpdate(ObjectIDList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; Old_Values: IBoldValueSpace; TranslationList: TBoldIdTranslationList); override;
    procedure DistributableInfoFromQuery(ObjectID: TBoldObjectId; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; DataSet: IBoldDataSet);
    procedure HandleFetchData(FetchedId: TBoldObjectId; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; Query: IBoldQuery; MemberPMList: TBoldMemberPersistenceMapperList; FetchMode: integer; FailureList: TBoldObjectIdList);
    procedure PortObject(ObjectId: TBoldObjectId; Query: IBoldQuery);
    function IsOldVersion(Query: IBoldQuery): Boolean;
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
    function CheckEitherNull(field: IBoldField; Value: IBoldValue; var Equal: Boolean): Boolean;
    function GetAllowNullAsSQL: string; override;
    procedure GetChangePoints(ObjectIDList: TBoldObjectIdList; Condition: TBoldChangePointCondition; NameSpace: TBoldSqlnameSpace); virtual;
    function CompareFields(ObjectContent: IBoldObjectContents; DataSet: IBoldDataSet; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList): Boolean;
    function CompareField(ObjectContent: IBoldObjectContents; Field: IBoldField; ColumnIndex: integer; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList): Boolean; virtual; abstract;
    procedure InitializePSDescriptions; override;
    function RequiresMemberMapping: Boolean; virtual;
    function FindDefiningTable(LocalMoldClass: TMoldClass; MoldMember: TMoldMember): string; virtual;
    function SupportsComparingWithoutValue: Boolean; virtual;
  public
    constructor CreateFromMold(moldMember: TMoldMember; moldClass: TMoldClass; Owner: TBoldObjectPersistenceMapper; const MemberIndex: Integer; TypeNameDictionary: TBoldTypeNameDictionary); override;
    procedure PMFetch(ObjectIdList: TBoldObjectIdList;
                   ValueSpace: IBoldValueSpace;
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
    function CompareField(ObjectContent: IBoldObjectContents; Field: IBoldField; ColumnIndex: integer; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList): Boolean; override;
    function FindDefiningTable(LocalMoldClass: TMoldClass; MoldMember: TMoldMember): string; override;
  public
    constructor CreateFromMold(moldMember: TMoldMember; moldClass: TMoldClass; Owner: TBoldObjectPersistenceMapper; const MemberIndex: Integer; TypeNameDictionary: TBoldTypeNameDictionary); override;
    function IsDirty(ObjectContents: IBoldObjectContents): Boolean; override;
    function ShouldFetch(ObjectContents: IBoldObjectContents): Boolean; override;
    procedure ValueToParam(ObjectContent: IBoldObjectContents; Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList); override;
    procedure ValueFromField(OwningObjectId: TBoldObjectId; ObjectContent: IBoldObjectContents; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; Field: IBoldField; ColumnIndex: Integer); override;
    function VersionFromQuery(Query: IBoldQuery): Integer;
    property VersionNumber: Integer read fVersionNumber write fVersionNumber;
  end;

  { TBoldReadOnlynessMember }
  TBoldReadOnlynessMember = class(TBoldSingleColumnMember)
  protected
    function GetColumnTypeAsSQL(ColumnIndex: Integer): string; override;
    function GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType; override;
    function CompareField(ObjectContent: IBoldObjectContents; Field: IBoldField; ColumnIndex: integer; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList): Boolean; override;
    function FindDefiningTable(LocalMoldClass: TMoldClass; MoldMember: TMoldMember): string; override;
  public
    constructor CreateFromMold(moldMember: TMoldMember; moldClass: TMoldClass; Owner: TBoldObjectPersistenceMapper; const MemberIndex: Integer; TypeNameDictionary: TBoldTypeNameDictionary); override;
    function IsDirty(ObjectContents: IBoldObjectContents): Boolean; override;
    function ShouldFetch(ObjectContents: IBoldObjectContents): Boolean; override;
    procedure ValueToParam(ObjectContent: IBoldObjectContents; Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList); override;
    procedure ValueFromField(OwningObjectId: TBoldObjectId; ObjectContent: IBoldObjectContents; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; Field: IBoldField; ColumnIndex: Integer); override;
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
    function CompareField(ObjectContent: IBoldObjectContents; Field: IBoldField; ColumnIndex: integer; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList): Boolean; override;
    function FindDefiningTable(LocalMoldClass: TMoldClass; MoldMember: TMoldMember): string; override;
    function SupportsComparingWithoutValue: Boolean; override;
  public
    constructor CreateFromMold(moldMember: TMoldMember; moldClass: TMoldClass; Owner: TBoldObjectPersistenceMapper; const MemberIndex: Integer; TypeNameDictionary: TBoldTypeNameDictionary); override;
    function IsDirty(ObjectContents: IBoldObjectContents): Boolean; override;
    function ShouldFetch(ObjectContents: IBoldObjectContents): Boolean; override;
    procedure ValueToParam(ObjectContent: IBoldObjectContents; Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList); override;
    procedure ValueFromField(OwningObjectId: TBoldObjectId; ObjectContent: IBoldObjectContents; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; Field: IBoldField; ColumnIndex: Integer); override;
  end;

  { TBoldTimeStampMember }
  TBoldGlobalIdMember = class(TBoldXFilesMembers)
  protected
    function GetColumnTypeAsSQL(ColumnIndex: Integer): string; override;
    function GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType; override;
    function CompareField(ObjectContent: IBoldObjectContents; Field: IBoldField; ColumnIndex: integer; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList): Boolean; override;
    function FindDefiningTable(LocalMoldClass: TMoldClass; MoldMember: TMoldMember): string; override;
  public
    constructor CreateFromMold(moldMember: TMoldMember; moldClass: TMoldClass; Owner: TBoldObjectPersistenceMapper; const MemberIndex: Integer; TypeNameDictionary: TBoldTypeNameDictionary); override;
    function IsDirty(ObjectContents: IBoldObjectContents): Boolean; override;
    function ShouldFetch(ObjectContents: IBoldObjectContents): Boolean; override;
    procedure ValueToParam(ObjectContent: IBoldObjectContents; Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList); override;
    procedure ValueFromField(OwningObjectId: TBoldObjectId; ObjectContent: IBoldObjectContents; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; Field: IBoldField; ColumnIndex: Integer); override;
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
  BoldDefaultStreamNames,
  BoldPMConsts;

const
  TIMESTAMPMEMBERINDEX = -2;

{--Supporting functions/procedures---}

{ TBoldSystemDefaultMapper }
constructor TBoldSystemDefaultMapper.CreateFromMold(moldModel: TMoldModel; TypeNameDictionary: TBoldTypeNameDictionary; SQlDatabaseConfig: TBoldSQLDatabaseConfig; GetDatabaseFunc: TBoldGetDatabaseEvent);
begin
  inherited;
  NextDBID := -1;
  LastReservedDBID := -2;
  ReservedCount := 0;
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
        EnsureIndex(IDCOLUMN_NAME + ';' + TIMESTAMPSTARTCOLUMNNAME, True, True);
        // the following two indices improves performance alot in Interbase, and seems to have no negative impact in SQLServer.
        EnsureIndex(TIMESTAMPSTARTCOLUMNNAME, false, false);
        EnsureIndex(IDCOLUMN_NAME, false, false);
      end
      else
        EnsureIndex(IDCOLUMN_NAME, True, True);

      EnsureIndex(TYPECOLUMN_NAME, False, False);
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

procedure TBoldSystemDefaultMapper.EnsureIndex(const TableName, Fields: string; const PrimaryIndex, Unique, InVersionedTable: Boolean);
var
  Table: TBoldSQLTableDescription;
begin
  EnsureTable(TableName, InVersionedTable);
  Table := PSSystemDescription.SQLTablesList.ItemsBySQLName[TableName];
  Table.EnsureIndex(Fields, PrimaryIndex, Unique);
end;

function TBoldSystemDefaultMapper.NewIdFromQuery(aQuery: IBoldQuery; BoldDbTypeColumn, ObjectIdColumn: integer; Timestamp: TBoldTimeStampType): TBoldObjectId;
var
  ObjectId: TBoldDefaultId;
  TopSortedIndex: integer;
begin
  if BoldDbTypeColumn = -1 then
    TopSortedIndex := NO_CLASS
  else
    TopSortedIndex := topSortedIndexForBoldDbType(aQuery.Fields[BoldDbTypeColumn].AsInteger);

  if TimeStamp <> BoldMaxTimeStamp then
    ObjectId := TBoldTimestampedDefaultId.createWithTimeAndClassId(TimeStamp, TopSortedIndex, true)
  else
    ObjectId := TBoldDefaultId.CreateWithClassId(TopSortedIndex, true);

  ObjectId.AsInteger := aQuery.Fields[ObjectIdColumn].AsInteger;
  result := ObjectId;
end;

function TBoldSystemDefaultMapper.GetListUsingQuery(ObjectIDList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; aQuery: IBoldQuery; FetchMode: Integer; TranslationList: TBoldIdTranslationList; TimeStamp: TBoldTimeStampType; MaxAnswers: integer = -1; Offset: integer = -1): integer;
var
  ObjectId: TBoldObjectId;
  Counter: integer;
begin
  aQuery.Open;
  if offset <> -1 then
    aQuery.MoveBy(Offset);
  // when MaxAnswer = -1 the while-test will never occur and we will get all the answers
  Counter := MaxAnswers;
  while not aQuery.EOF and (Counter <> 0) do
  begin
    if TimeStamp = BOLDINVALIDTIMESTAMP then
      ObjectId := NewIdFromQuery(aQuery, 1, 0, aQuery.FieldByName(TIMESTAMPSTARTCOLUMNNAME).AsInteger)
    else
      ObjectId := NewIdFromQuery(aQuery, 1, 0, TimeStamp);
    ValueSpace.EnsureObjectId(TranslationList.TranslateToNewId[ObjectId]);
    ObjectIDList.Add(ObjectId);
    TranslationList.AddTranslation(nil, ObjectId);
    SendExtendedEvent(bpeFetchId, [ObjectId]);
    ObjectId.Free;
    aQuery.Next;
    dec(Counter);
  end;
  if (MaxAnswers < 0)  or (Counter > 0) then
    result := MaxAnswers - Counter
  else
    result := aQuery.RecordCount;
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

var
  OldMemberIdList: TBoldMemberIdList;
  SingleObjectList: TBoldObjectIdList;
  TimeStampedObjects: TBoldObjectIdList;
  ObjectContents: IBoldObjectContents;
  Value: IBoldValue;
  ObjectIdList: TBoldObjectIdList;
  MemberIx, i: integer;
  ObjectMapper: TBoldObjectDefaultmapper;
  FailureList: TBoldObjectIdList;
  BoldGuard: IBoldGuard;
begin
  BoldGuard := TBoldGuard.Create(TimeStampedObjects, ObjectIdList, FailureList,
                                 SingleObjectlist, OldMemberIdList);
  OldMemberIdList := TBoldMemberIdList.Create;
  SingleObjectList := TBoldObjectIdList.Create;
  TimeStampedObjects := TBoldObjectIdList.Create;
  ObjectIdList := TBoldObjectIdList.Create;
  FailureList := TBoldObjectIdList.Create;

  Precondition.valueSpace.AllObjectIds(ObjectidList, true);
  for i := 0 to ObjectIdList.Count - 1 do
  begin
    ObjectContents := Precondition.ValueSpace.ObjectContentsByObjectId[ObjectIdList[i]];
    if assigned(ObjectContents) then
    begin
      OldMemberIdList.Clear;

      if ObjectContents.TimeStamp <> -1 then
        TimeStampedObjects.Add(ObjectIdList[i]);

      for MemberIx := 0 to ObjectContents.MemberCount - 1 do
      begin
        Value := ObjectContents.ValueByIndex[MemberIx];
        if assigned(Value) then
          OldMemberIdList.Add(TBoldmemberId.Create(MemberIx));
      end;

      // we must compare the object even if it has no dirty members
      // since it might be a delete and the object in the db might
      // be deleted already. if it is timestamped however, we will
      // detect it more cheap that way
      if (OldMemberIdList.Count > 0) or (ObjectContents.TimeStamp = -1) then
      begin
        SingleObjectList.Clear;
        SingleObjectList.Add(ObjectIdList[i]);
        failureList.Clear;
        ObjectMapper := ObjectPersistenceMappers[ObjectIdList[i].TopSortedIndex] as TBoldObjectDefaultMapper;
        Objectmapper.PMCompareExactIDList(SingleObjectList, Precondition.ValueSpace, OldMemberIdList, translationList, FailureList);
        MergeFailures(precondition.FailureList, FailureList);
      end;
    end;
  end;
  if TimeStampedObjects.Count > 0 then
  begin
    OldMemberIdList.clear;
    FailureList.Clear;
    OldMemberIdList.Add(TBoldMemberId.Create(TIMESTAMPMEMBERINDEX));
    RootClassObjectPersistenceMapper.PMCompareExactIDList(TimeStampedObjects, Precondition.ValueSpace, OldMemberIdList, translationlist, FailureList);
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
var
  i: integer;
begin
  inherited;
  fOptimisticLockingMode := MoldClass.EffectiveOptimisticLocking;
  if SystemPersistenceMapper.UseModelVersion then
  begin
    fModelVersionMember := TBoldModelVersionMember.CreateFromMold(nil, MoldClass, self, -1, TypeNameDictionary);
    MemberPersistenceMappers.Add(fModelVersionMember);
  end;
  if SystemPersistenceMapper.UseReadOnly then
    MemberPersistenceMappers.Add(TBoldReadOnlynessMember.CreateFromMold(nil, MoldClass, self, -1, TypeNameDictionary));


  if SystemPersistenceMapper.UseXFiles then
  begin
    if SystemPersistenceMapper.UseTimestamp then
      MemberPersistenceMappers.Add(TBoldTimeStampMember.CreateFromMold(nil, MoldClass, self, -1, TypeNameDictionary));
    if SystemPersistenceMapper.UseGlobalId then
      MemberPersistenceMappers.Add(TBoldGlobalIdMember.CreateFromMold(nil, MoldClass, self, -1, TypeNameDictionary));
  end;

  fObjectIdClass := BOLDDEFAULTIDNAME;
  for i := 0 to MoldClass.AllPossibleNames.Count - 1 do
    GenerateMappingInfo(MoldClass.AllPossibleNames[i], MoldClass);
end;

type
  TLittleClass = class
    Id: integer;
    dbType: TBoldDbType;
  end;



procedure TBoldObjectDefaultMapper.InternalMakeIDsExact(ObjectIDList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList);
var
  i: integer;
  ObjectMapper: TBoldObjectDefaultMapper;
begin
  if assigned(MainTable) then
    MakeIDsExactUsingTable(ObjectIDList, TranslationList, MainTable)
  else
  begin
    for i := 0 to SystemPersistenceMapper.ObjectPersistenceMappers.Count-1 do
    begin
      ObjectMapper := SystemPersistenceMapper.ObjectPersistenceMappers[i] as TBoldObjectDefaultMapper;
      if assigned(ObjectMapper) and (ObjectMapper.SuperClass = self) then
        ObjectMapper.InternalMakeIDsExact(objectidlist, TranslationList);
    end;
  end;
end;


procedure TBoldObjectDefaultMapper.MakeIDsExact(ObjectIDList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList);
var
  i: Integer;
  MissingIds: TBoldObjectIdList;
begin
  if ObjectIDList.Count = 0 then
    exit;
  InternalMakeIDsExact(ObjectIDList, TranslationList);
  MissingIds := TBoldObjectIdList.Create;
  try
    if SystemPersistenceMapper.UseXFiles then
    begin
      for i := 0 to ObjectIDList.Count-1 do
        if TranslationList.TranslateToNewId[ObjectIdList[i]] = ObjectIdList[i] then
          MissingIds.Add(ObjectIdList[i]);
      if MissingIds.Count > 0 then
        MakeIDsExactUsingTable(MissingIds, TranslationList, SystemPersistenceMapper.PSSystemDescription.XFilestable);
    end;
  finally
    MissingIds.Free;
  end;
end;

procedure TBoldObjectDefaultMapper.SQLForID(Table: TBoldSQLTableDescription; SQL: TStrings; useAlias: Boolean);
begin
  SQL.Append(Format('%s.%s', [Tablealias(Table, useAlias), IDCOLUMN_NAME])); // do not localize
end;

procedure TBoldObjectDefaultMapper.SQLForKey(Table: TBoldSQLTableDescription; SQL: TStrings; const SQLStyle: TBoldSQLStyle; useAlias: Boolean);
var
  tableQualifier, ColumnString: string;
begin
  tableQualifier := '';
  if (Table = MainTable) and useAlias then
    tableQualifier := TableAlias(Table, useALias) + '.';

  case SQLStyle of
    ssColumns: ColumnString := tableQualifier + '%s';
    ssParameters: ColumnString := ':' + tableQualifier + '%s';
  end;

  SQL.Append(Format(ColumnString, [Table.ColumnsList[0].SQLName]));
  SQL.Append(Format(ColumnString, [Table.ColumnsList[1].SQLName]));
end;

procedure TBoldObjectDefaultMapper.JoinSQLTableByKey(SQL: TStringList; MainTable, JoinTable: TBoldSQLTableDescription);
const
  SQLEQUALITY = '%s.%s = %s.%s';
begin
  SQL.Append(Format(SQLEQUALITY, [TableAlias(JoinTable, True), IDCOLUMN_NAME, TableAlias(MainTable, True), IDCOLUMN_NAME]));
  if Versioned then
    SQL.Append(Format(SQLEQUALITY, [TableAlias(JoinTable, True), TIMESTAMPSTARTCOLUMNNAME, TableAlias(MainTable, True), TIMESTAMPSTARTCOLUMNNAME]));
end;

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

    UpdateQuery.AssignSQLText(format('UPDATE %s SET %s = %d WHERE %s in (%s) AND (%s = %d)', // do not localize
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

procedure TBoldObjectDefaultMapper.PMCreate(ObjectIDList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList);
var
  I, T, A: Integer;
  TickCounter: integer;
  aQuery: IBoldExecQuery;
  sql, TempList: TStringList;

  MemberPMList: TBoldMemberPersistenceMapperList;
  MemberPMapper: TBoldMemberDefaultMapper;
  NewID: TBoldObjectId;
  DuplicateList: TBoldObjectIdList;
  BoldGuard: IBoldGuard;
begin
  BoldGuard := TBoldGuard.Create(MemberPMList);
  MemberPMList := TBoldMemberPersistenceMapperList.Create;
  MemberPMList.OwnsEntries := False;
  Tickcounter := 0;
  if IsLinkClass and (not Versioned) then
  begin
    // this will update link-objects rather than create new if they are already in the database
    // this happens only if two applications link the same two objects at the same time
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

  aQuery := SystemPersistenceMapper.GetExecQuery;
  sql := TStringList.Create;
  try
    for T := 0 to AllTables.Count - 1 do
    begin
      // Clear the memberlist, as we reuse it.
      while MemberPMList.Count > 0 do
        MemberPMList.RemoveByIndex(0);
      // Fill it with members to use.
      // FIXME: This will probably not work for members with several tables... /JoHo
      for A := 0 to MemberPersistenceMappers.Count - 1 do
      begin
        MemberPMapper := MemberPersistenceMappers[A] as TBoldMemberDefaultMapper;
        if assigned(MemberPMapper) and
          MemberPMapper.IsStoredInObject and not MemberPMapper.CustomCreateUpDate and
          ((MemberPMapper.ColumnDescriptions[0] as TBoldSQLColumnDescription).TableDescription = AllTables[T]) then
          MemberPMList.Add(MemberPMapper);
      end;

      //	Create insert query.
      sql.Clear;
      aQuery.ClearParams;
      TempList := TStringList.Create;
      SQLForMembers(AllTables[T], TempLIst, MemberPMList, ssColumns, True, False, False);

      if Alltables[T].Versioned then
      begin
        TempList.Add(TIMESTAMPSTARTCOLUMNNAME);
        if allTables[T].ContainsStopTimeStamp then
          TempList.Add(TIMESTAMPSTOPCOLUMNNAME);
      end;

      BoldAppendToStrings(SQL, Format('INSERT INTO %s (%s) ', [AllTables[T].SQLName, // do not localize
                                                                  BoldSeparateStringList(TempLIst, ', ', '', '')]), True);

      TempList.Clear;
      SQLForMembers(AllTables[T], TempList, MemberPMList, ssParameters, True, False, False);

      if Alltables[T].Versioned then
      begin
        TempList.Add(':' + TIMESTAMPSTARTCOLUMNNAME);
        if allTables[T].ContainsStopTimeStamp then
          TempList.Add(':' + TIMESTAMPSTOPCOLUMNNAME);
      end;

      BoldAppendToStrings(SQL, Format('VALUES (%s) ', [BoldSeparateStringList(TempLIst, ', ', '', '')]), True); // do not localize
      TempLIst.Free;

      aQuery.AssignSQL(SQL);
      aQuery.StartSQLBatch;
      try
        for I := 0 to ObjectIDList.Count - 1 do
        begin
          // All tables have id and type as column 0 and 1 respectively
          NewID := TranslationList.TranslateToNewID[ObjectIDList[I]];
          aQuery.ParamByName(IDCOLUMN_NAME).AsInteger := (NewId as TBoldDefaultId).AsInteger;
          aQuery.ParamByName(TYPECOLUMN_NAME).AsSmallInt := SystemPersistenceMapper.BoldDbTypeForTopSortedIndex(NewId.topSortedIndex);

          ValuesToParamsByMemberList(ObjectIDList[I], ValueSpace, aQuery, MemberPMList, TranslationList, dsmCreate);

          if Alltables[T].Versioned then
          begin
            if versioned then
              aQuery.ParamByName(TIMESTAMPSTARTCOLUMNNAME).AsInteger := SystemPersistenceMapper.CurrentTimeStamp
            else
              aQuery.ParamByName(TIMESTAMPSTARTCOLUMNNAME).AsInteger := 0;

            if allTables[T].ContainsStopTimeStamp then
              aQuery.ParamByName(TIMESTAMPSTOPCOLUMNNAME).AsInteger := BOLDMAXTIMESTAMP;
          end;

          Inc(TickCounter);
          if (TickCounter MOD AllTables.Count) = 0 then
          begin
            SystemPersistenceMapper.SendExtendedEvent(bpeCreateObject, [ObjectIdList[i], ValueSpace]);
            TickCounter := 0;
          end;
          aQuery.ExecSQL;
        end;
        aQuery.EndSQLBatch;
      except
        aQuery.FailSQLBatch;
        raise;
      end;
    end;
  finally
    SystemPersistenceMapper.ReleaseExecQuery(aQuery);
    sql.Free;
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

procedure TBoldObjectDefaultMapper.PMTemporalUpdate(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldvalueSpace; TranslationList: TBoldIdTranslationList);
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
        // Clear the memberlist, as we reuse it.
        MemberPMList.Clear;
        // Fill it with members to use.
        // FIXME: This will probably not work for members with several tables... /JoHo
        for A := 0 to MemberPersistenceMappers.Count - 1 do
        begin
          MemberPMapper := MemberPersistenceMappers[A] as TBoldMemberDefaultMapper;
          if assigned(MemberPMapper) and
            MemberPMapper.IsStoredInObject and not MemberPMapper.CustomCreateUpDate and
            ((MemberPMapper.ColumnDescriptions[0] as TBoldSQLColumnDescription).TableDescription = AllTables[T]) then
            MemberPMList.Add(MemberPMapper);
        end;

        //	Create insert query.

        SQL.Clear;
        TempList.Clear;

        for i := 0 to ObjectidList.Count - 1 do
          TempList.Add(ObjectIdLIst[i].AsString);

        if AllTables[T].ContainsStopTimeStamp then
          OldDataQuery.AssignSQLText(format('SELECT * FROM %s WHERE (%s in %s) AND (%s = %d)', [ // do not localize
            Alltables[T].SQlname, IDCOLUMN_NAME,
            BoldSeparateStringlist(TempList, ', ', '(', ')'),
            TIMESTAMPSTOPCOLUMNNAME,
            SystemPersistenceMapper.CurrentTimeStamp - 1]))
        else
          OldDataQuery.AssignSQLText(format(
             'SELECT DataTable.* FROM %0:s DataTable, %1:s RootTable ' + // do not localize
             'WHERE (DataTable.%2:s in %3:s) AND ' + // do not localize
               '(DataTable.%2:s = RootTable.%2:s) AND ' + // do not localize
               '(DataTable.%4:s = RootTable.%4:s) AND ' + // do not localize
               '(RootTable.%5:s = %6:d)', [ // do not localize
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

        BoldAppendToStrings(SQL, Format('INSERT INTO %s (%s) ', [AllTables[T].SQLName, // do not localize
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

        BoldAppendToStrings(SQL, Format('VALUES (%s) ', [BoldSeparateStringList(TempLIst, ', ', '', '')]), True); // do not localize

        if (IdColumnIndex = -1) or (TypeColumnIndex = -1) then
          raise EBoldInternal.CreateFmt(sTypeAndIDColumnMissing, [classname, aQuery.SQLText]);


        aQuery.AssignSQL(SQL);
        while not OldDataQuery.Eof do
        begin
          NewId := SystemPersistenceMapper.NewIdFromQuery(OldDataQuery, TypeColumnIndex, IdColumnIndex, BOLDMAXTIMESTAMP);
          try
            // copy all old data
            for i := 0 to OldDataQuery.FieldCount - 1 do
              aQuery.ParamByName(OldDataQuery.Fields[i].FieldName).AssignFieldValue(OldDataQuery.Fields[i]);

            // fill with known new data.
            ValuesToParamsByMemberList(NewId, ValueSpace, aQuery, MemberPMList, TranslationList, dsmUpdate);

            // set the timestamps
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

procedure TBoldObjectDefaultMapper.PMUpdate(ObjectIDList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; Old_Values: IBoldValueSpace; TranslationList: TBoldIdTranslationList);
var
  aQuery: IBoldExecQuery;
  I, T, A: Integer;
  TempLIst: TStringList;
  MemberPMList: TBoldMemberPersistenceMapperList;
  MemberPMapper: TBoldMemberDefaultMapper;
  ObjectContents: IBoldObjectContents;
  sql: TStringList;
  BoldGuard: IBoldGuard;
begin
  BoldGuard := TBoldGuard.Create(MemberPMList, SQL);
  if Versioned then
  begin
    PMTemporalUpdate(ObjectIdList, ValueSpace, TranslationList);
    exit;
  end;
  MemberPMList := TBoldMemberPersistenceMapperList.Create;
  MemberPMList.OwnsEntries := False;

  if Assigned(Old_Values) then
    FetchPreviousSingleLinkValues(ObjectIdList, Old_Values);

  aQuery := SystemPersistenceMapper.GetExecQuery;
  sql := TStringList.Create;
  try
    aQuery.StartSQLBatch;
    try
      for T := 0 to AllTables.Count - 1 do
      begin
        if UpdatesMembersInTable(AllTables[T]) then
        begin
          for I := 0 to ObjectIDList.Count - 1 do
          begin
            ObjectContents := ValueSpace.ObjectContentsByObjectId[ObjectIDList[I]];

            // Clear the memberlist, as we reuse it.
            while MemberPMList.Count > 0 do
              MemberPMList.RemoveByIndex(0);

            // Fill it with members to use.
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
              TempList := TStringList.Create;
              SQLForMembers(AllTables[T], TempList, MemberPMList, ssValues, False, False, False);

              SQL.Clear;
              BoldAppendToStrings(SQL, Format('UPDATE %s', [AllTables[T].SQLName]), True); // do not localize
              BoldAppendToStrings(SQL, Format('SET %s', [BoldSeparateStringList(TempLIst, ', ', '', '')]), True); // do not localize
              BoldAppendToStrings(SQL, Format('WHERE %s = :%0:s', [IDCOLUMN_NAME]), True); // do not localize

              TempList.Free;

              aQuery.ClearParams;
              aQuery.AssignSQL(sql);

              aQuery.ParamByName(IDCOLUMN_NAME).AsInteger := (TranslationList.TranslateToNewID[ObjectIDList[I]] as TBoldDefaultId).asInteger;
              ValuesToParamsByMemberList(ObjectIDList[I], ValueSpace, aQuery, MemberPMList, TranslationList, dsmUpdate);
              SystemPersistenceMapper.SendExtendedEvent(bpeUpdateObject, [ObjectIDList[I], ValueSpace, aQuery]);
              aQuery.ExecSQL;
            end;
          end;
        end;
      end;
      aQuery.EndSQLBatch;
    except
      aQuery.FailSQLBatch;
      raise;
    end;
  finally
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

procedure TBoldObjectDefaultMapper.PMDelete(ObjectIDList: TBoldObjectIdList; ValueSpace: IBoldValueSpace;
              Old_Values: IBoldValueSpace; TranslationList: TBoldIdTranslationList);
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
  try
    ObjectCount := ObjectIDList.Count - 1;
    for Block := 0 to (ObjectCount div FetchBlockSize) do
    begin
      lst.clear;
      Start := Block * FetchBlockSize;
      Stop := MinIntValue([Pred(Succ(Block) * FetchBlockSize), ObjectCount]);

      for i := start to stop do
        SystemPersistenceMapper.SendExtendedEvent(bpeDeleteObject, [ObjectIDList[I], ValueSpace]);

      // Construct the delete statement with a placeholder for table-name
      BoldAppendToStrings(lst, 'DELETE FROM %s', True); // do not localize
      IdListString := IdListSegmentToWhereFragment(ObjectIdList, start, stop, aQuery);

      BoldAppendToStrings(lst, Format('WHERE %%s %s', [IdListString]), True); // do not localize

      //Execute SQLStatement on all tables except the xfiles table
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
            aQuery.AssignSQLText(Format('UPDATE %s SET %s = %d WHERE %s %s', // do not localize
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

procedure TBoldObjectDefaultMapper.PMMultiPurposeRetrieveExactIdList(ObjectsToFetch: TBoldObjectIdList; ValueSpace: IBoldValueSpace; MemberIdList: TBoldMemberIdList; FetchMode: Integer; TranslationList: TBoldIdTranslationList; MissingList: TBoldObjectIdList; FailureList: TBoldObjectIdList; TimeStamp: TBoldTimeStampType);
var
  Start, Stop: integer;
  CustomMembers,
  MemberPMList: TBoldMemberPersistenceMapperList;
  aQuery: IBoldQuery;
  TempLIst: TStringList;
  Block,
  ObjectCount: Longint;
  i: Integer;
  TempId, NewID: TBoldObjectId;
  SQLHits: integer;
  ObjectContents: IBoldObjectContents;
  sql: TStringList;
  FetchBlockSize: integer;
  RefetchIdList: TBoldObjectIdList;
begin
  MemberPMList := TBoldMemberPersistenceMapperList.Create;
  MemberPMList.OwnsEntries := False;
  CustomMembers := TBoldMemberPersistenceMapperList.Create;
  CustomMembers.OwnsEntries := False;
  aQuery := SystemPersistenceMapper.GetQuery;
  TempLIst := TStringList.Create;
  sql := TStringList.Create;
  RefetchIdList := TBoldObjectIdList.Create;
  FetchBlockSize := SystemPersistenceMapper.SQLDataBaseConfig.FetchBlockSize;

  try
    BuildMemberFetchLists(MemberIdList, MemberPMLIst, CustomMembers, FetchMode);

    try
      if SystemPersistenceMapper.SupportsObjectUpgrading then
        SystemPersistenceMapper.ObjectUpgrader.StartTransaction;

      // if the memberidlist is empty, then we must fetch the objects to make sure they still exist in the database
      // this ensures optimistic locking on deleted objects.
      // The below test will skip fetching the object if we are fetching only "custom members"
      if (MemberPMList.Count > 0) or not assigned(MemberIdList) or (MemberIdList.Count = 0) then
      begin
        if assigned(MissingList) then
          MissingList.AddList(ObjectsToFetch);

        //  We do not want to retrieve more than FetchBlockSize objects at a time

        ObjectCount := ObjectsToFetch.Count - 1;
        for Block := 0 to (ObjectCount div FetchBlockSize) do
        begin
          sql.clear;
          RetrieveSelectStatement(SQL, MemberPMList, FetchMode, Versioned);

          Start := Block * FetchBlockSize;
          Stop := MinIntValue([Pred(Succ(Block) * FetchBlockSize), ObjectCount]);
          if Stop >= Start then
          begin
            aQuery.ClearParams;
            BoldAppendToStrings(SQL, IdListSegmentToWhereFragment(ObjectsToFetch, Start, Stop, aQuery), False);

            if versioned then
              RetrieveTimeStampCondition(SQL, TimeStamp, true, 'AND', false); // do not localize

            SQLHits := 0;
            aQuery.AssignSQL(SQL);
            aQuery.Open;
            while not aQuery.EOF do
            begin
              Inc(SQLHits);

              tempId := (SystemPersistenceMapper as TBoldSystemDefaultMapper).NewIdFromQuery(aQuery, 1, 0, timeStamp);
              NewId := ObjectsToFetch.IDByID[TempId];
              TempId.Free;

              if not assigned(NewId) then
                raise EBoldInternal.CreateFmt(sUninvitedObjectReturnedFromDB, [classname, aQuery.Fields[0].AsInteger]);

              if assigned(MissingList) then
              begin
                i := MissingList.IndexByID[NewId];
                if i <> -1 then
                  MissingList.RemoveByIndex(i)
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
            aQuery.Close;
            BoldPMLogFmt(sLogFetchedXobjectsOfY, [SQLHits, ExpressionName]);
          end;
        end;

        if assigned(MissingList) and (FetchMode <> fmDistributable) and (MissingList.Count > 0) then
        begin
          BoldPMLogFmt(sLogXObjectsOfYMissing, [MissingList.Count, ExpressionName]);

          for i := 0 to MissingList.Count - 1 do
          begin
            ObjectContents := ValueSpace.EnsuredObjectContentsByObjectId[MissingList[i]];
            ObjectContents.BoldExistenceState := besDeleted;
            ObjectContents.IsReadOnly := true;
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
    sql.Free;
    MemberPMList.Free;
    CustomMembers.Free;
    SystemPersistenceMapper.ReleaseQuery(aQuery);
    TempLIst.Free;
    RefetchIdList.Free;
  end;
end;

function TBoldObjectDefaultMapper.CompareFieldsToMembers(ObjectID: TBoldObjectId; ValueSpace: IBoldValueSpace; DataSet: IBoldDataSet; memberList: TBoldMemberPersistenceMapperList; TranslationList: TBoldIdTranslationList): Boolean;
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

procedure TBoldObjectDefaultMapper.HandleCompareData(FetchedId: TBoldObjectId; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; Query: IBoldQuery; MemberPMList: TBoldMemberPersistenceMapperList; FetchMode: integer; FailureList: TBoldObjectIdList);
begin
  if not CompareFieldsToMembers(FetchedId, ValueSpace, Query, MemberPMList, TranslationList) and
    not FailureList.IdInList[FetchedId] then
    FailureList.Add(FetchedId);
end;

procedure TBoldObjectDefaultMapper.PMCompareExactIDList(ObjectIDList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; MemberIdList: TBoldMemberIdList; TranslationList: TBoldIdTranslationList; FailureList: TBoldObjectIdList);
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

procedure TBoldObjectDefaultMapper.HandleFetchData(FetchedId: TBoldObjectId; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; Query: IBoldQuery; MemberPMList: TBoldMemberPersistenceMapperList; FetchMode: integer; FailureList: TBoldObjectIdList);
begin
  ValuesFromFieldsByMemberList(FetchedId, ValueSpace, TranslationList, Query, MemberPMList);
  if FetchMode = fmDistributable then
    DistributableInfoFromQuery(FetchedId, ValueSpace, TranslationList, Query);
end;

procedure TBoldObjectDefaultMapper.PMFetchExactIDList(ObjectIDList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; MemberIdList: TBoldMemberIdList; FetchMode: Integer; TranslationList: TBoldIdTranslationList; MissingList: TBoldObjectIdList);
var
  i: integer;
  TimeStamp: TBoldTimeStampType;
  ListOfOtherTimeStamps, ObjectsToFetch: TBoldObjectIdList;
begin
  // Note, when the list gets here it contains only object of one class, from one
  // Bold, that are not previously present in the Bold (or possibly a forced fetch)
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

procedure TBoldObjectDefaultMapper.PMFetchWithCondition(ObjectIDList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; BoldCondition: TBoldCondition; FetchMode: Integer; TranslationList: TBoldIdTranslationList);
var
  aQuery: IBoldQuery;
  WhereToken: string;
  fromLine: String;
  JoinLine: String;
  RootTableName: string;
  JoinCondition: string;
  i, j: Integer;
  timeStamp: TBoldTimeStampType;
  MappingInfo: TBoldAllInstancesMappingArray;
  SQL: TStringList;
  SQLCondition: TBoldSQLCondition;
begin
  if not assigned(BoldCondition) or (BoldCondition is TBoldConditionWithClass) then
  begin
    if BoldCondition is TBoldSQLCondition then
      SQLCondition := BoldCondition as TBoldSQLCondition
    else
      SQLCondition := nil;

    MappingInfo := SystemPersistenceMapper.MappingInfo.GetAllInstancesMapping(ExpressionName);
    aQuery := SystemPersistenceMapper.GetQuery;
    sql := TStringList.Create;
    try
      for i := 0 to length(MappingInfo) - 1 do
      begin
        sql.Clear;
        WhereToken := 'WHERE'; // Do not localize

        if assigned(BoldCondition) then
          timeStamp := (BoldCondition as TBoldConditionWithClass).Time
        else
          TimeStamp := BOLDMAXTIMESTAMP;

        if (BoldCondition is TBoldTimestampCondition) then
        begin
          SQL.Add(format('SELECT %s, %s', [IDCOLUMN_NAME, TYPECOLUMN_NAME])); // Do not localize
          SQL.Add(format('FROM %s', [(SystemPersistenceMapper.PSSystemDescription as TBoldDefaultSystemDescription).XFilestable.SQLName])); // Do not localize
          SQL.Add(format('WHERE %s > %d', [SystemPersistenceMapper.XFilesTimeStampColumn.SQLname, (BoldCondition as TBoldTimestampCondition).Timestamp])); // Do not localize
          aQuery.AssignSQL(SQL);
        end
        else
        begin
          SQL.Append(Format('SELECT %s.%s, %s.%s', [MappingInfo[i].TableName, IDCOLUMN_NAME, MappingInfo[i].TableName, TYPECOLUMN_NAME])); // do not localize
          FromLine := Format('FROM %s', [MappingInfo[i].TableName]); // do not localize
          JoinLine := '';
          RootTableName := SystemPersistenceMapper.RootClassObjectPersistenceMapper.MainTable.SQLName;

          // add the rest of the tables
          if assigned(SQLCondition) and
             SQLCondition.JoinInheritedTables and
             ((SQLCondition.whereFragment <> '') or
              (SQLCondition.OrderBy <> '')) then
          begin

            for j := 0 to AllTables.count - 1 do
              if not SameText(Alltables[j].SQLName, MappingInfo[i].TableName) and
                (AllTables[j] <> SystemPersistenceMapper.PSSystemDescription.XFilestable) then
              begin
                JoinCondition := format('(%s.%s = %s.%s)', // do not localize
                  [MappingInfo[i].TableName, IDCOLUMN_NAME, AllTables[j].SQLName, IDCOLUMN_NAME]);

                if SystemPersistenceMapper.SQLDataBaseConfig.UseSQL92Joins then
                  fromLine := fromLine + format(' left join %s on %s ', [AllTables[j].SQLName, JoinCondition]) // do not localize
                else
                begin
                  fromLine := fromLine + format(', %s', [AllTables[j].SQLName]); // do not localize
                  JoinLine := JoinLine + format('%s %s ', [WhereToken, joinCondition]); // do not localize
                  WhereToken := 'AND'; // do not localize
                end
              end;
          end
          else if Versioned and
                  not SameText(MappingInfo[i].TableName, RootTableName) then
          begin
            joinCondition := format('(%s.%s = %s.%s) ', // do not localize
                  [MappingInfo[i].TableName, IDCOLUMN_NAME, RootTableName, IDCOLUMN_NAME]);

            if SystemPersistenceMapper.SQLDataBaseConfig.UseSQL92Joins then
              fromLine := fromLine + format(' left join %s on %s ', [RootTableName, JoinCondition]) // do not localize
            else
            begin
              fromLine := FromLine + ', ' + RootTableName; // do not localize
              JoinLine := JoinLine + format(' %s %s ', [WhereToken, joincondition]); // do not localize
              WhereToken := 'AND'; // do not localize
            end;
          end;
          SQL.Add(fromLine);
          if JoinLine <> '' then
            SQL.Add(JoinLine);

          if Versioned then
          begin
            RetrieveTimeStampCondition(sql, TimeStamp, false, WhereToken, false);
            WhereToken := 'AND'; // do not localize
          end;

          if MappingInfo[i].ClassIdRequired then
          begin
            SQL.Add(Format('%s %s.%s in (%s)', [WhereToken, MappingInfo[i].TableName, TYPECOLUMN_NAME, SubClassesID])); // do not localize
            WhereToken := 'AND'; // do not localize
          end;

          if assigned(SQLCondition) then
          begin
            if SQLCondition.whereFragment <> '' then // do not localize
              SQL.Append(Format('%s (%s)', [WhereToken, SQLCondition.whereFragment])); // do not localize
            if SQLCondition.orderBy <> '' then // do not localize
              SQL.Append(Format('ORDER BY %s', [SQLCondition.orderBy])); // do not localize
          end;

          aQuery.AssignSQL(SQL);

          if assigned(SQLCondition) then
            aQuery.AssignParams(SQLCondition.Params);

         end;
        BoldCondition.AvailableAnswers := (SystemPersistenceMapper as TBoldSystemDefaultMapper).GetListUsingQuery(ObjectIDList, ValueSpace, aQuery, FetchMode, TranslationList, TimeStamp, BoldCondition.MaxAnswers, BoldCondition.Offset);
        BoldPMLogFmt(sLogFetchedXObjectsOfYFromTableZ, [ObjectIdList.Count, ExpressionName, MappingInfo[i].tableName]);
      end;
    finally
      SystemPersistenceMapper.ReleaseQuery(aQuery);
      sql.Free;
    end;
  end
  else
    raise EBold.CreateFmt(sUnknownConditionType, [BoldCondition.ClassName]);
end;

function TBoldObjectDefaultMapper.NextExternalObjectId(ValueSpace: IBoldValueSpace; ObjectId: TBoldObjectId): TBoldObjectId;
var
  aQuery: IBoldQuery;
  aExecQuery: IBoldExecQuery;
  NewID: Longint;
  SystemDefaultMapper: TBoldSystemDefaultMapper;
begin
  SystemDefaultMapper := SystemPersistenceMapper as TBoldSystemDefaultMapper;
  if SystemDefaultMapper.NextDBID > SystemDefaultMapper.LastReservedDBID then
  begin
    SystemDefaultMapper.StartTransaction(ValueSpace);
    aQuery := SystemDefaultMapper.GetQuery;
    aExecQuery := SystemDefaultMapper.GetExecQuery;
    try
      try
        aExecQuery.AssignSQLText(Format('UPDATE %s SET %s = %s + %d', [ // do not localize
                                  SystemDefaultMapper.PSSystemDescription.IdTable.SQLName,
                                  IDCOLUMN_NAME,
                                  IDCOLUMN_NAME,
                                  SystemDefaultMapper.ReservedCount]));
        aExecQuery.ExecSQL;
        aQuery.AssignSQLText(Format('SELECT %s FROM %s', [ // do not localize
                               IDCOLUMN_NAME,
                               SystemDefaultMapper.PSSystemDescription.IdTable.SQLName]));
        aQuery.Open;
        NewID := aQuery.Fields[0].AsInteger;
        aQuery.Close;
        SystemDefaultMapper.Commit(ValueSpace);
        SystemDefaultMapper.NextDBID := NewID - SystemDefaultMapper.ReservedCount;
        SystemDefaultMapper.LastReservedDBID := SystemDefaultMapper.NextDBID + SystemDefaultMapper.ReservedCount - 1;
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
  SystemDefaultMapper.ReservedCount := 0;

  Result := TBoldDefaultId.CreateWithClassID(ObjectId.TopSortedIndex, true);
  (result as TBoldDefaultId).AsInteger := SystemDefaultMapper.NextDBID;

  SystemDefaultMapper.NextDBID := SystemDefaultMapper.NextDBID + 1;
end;

procedure TBoldObjectDefaultMapper.FetchPreviousSingleLinkValues(ObjectIdList: TBoldObjectIdLIst; Old_Values: IBoldvalueSpace);
var
  i: integer;
  SingleLinkList: TBoldMemberIdList;
  BoldGuard: IBoldGuard;
begin
  BoldGuard := TBoldGuard.Create(SingleLinkList);
  SingleLinkList := TBoldMemberIdList.Create;

  for i := 0 to MemberPersistenceMappers.Count - 1 do
    if MemberPersistenceMappers[i] is TBoldEmbeddedSingleLinkDefaultMapper then
      SingleLinkList.Add(TBoldmemberId.Create(MemberPersistenceMappers[i].MemberIndex));
  if SingleLinkList.Count > 0 then
    PMFetchExactIDList(ObjectIdList, Old_Values, SingleLInkList, fmNormal, nil, nil);
end;

procedure TBoldObjectDefaultMapper.MakeIDsExactUsingTable(
  ObjectIDList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList;
  Table: TBoldSQLTableDescription);
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
  lst := TStringList.Create;
  aQuery := SystemPersistenceMapper.GetQuery;
  FetchBlockSize := SystemPersistenceMapper.SQLDataBaseConfig.FetchBlockSize;
  try
    //  We do not want to retrieve more than BLOCKSIZE objects at a time
    ObjectCount := ObjectIDList.Count - 1;
    for Block := 0 to (ObjectCount div FetchBlockSize) do
    begin
      lst.Clear;
      Table.RetrieveSelectIdAndTypeStatement(lst);
      Start := Block * FetchBlockSize;
      Stop := MinIntValue([Pred(Succ(Block) * FetchBlockSize), ObjectCount]);

      WhereFragment := IdListSegmentToWhereFragment(ObjectIDList, start, stop, aquery);
      BoldAppendToStrings(lst, Format(' WHERE %s %s', [IDCOLUMN_NAME, WhereFragment]), True); // do not localize
      aQuery.AssignSQL(lst);
      aQuery.Open;
      Ids := TList.Create;
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
          // fixme: Square
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
        Ids.Free;
      end;
    end;
  finally
    SystemPersistenceMapper.ReleaseQuery(aQuery);
    lst.Free;
  end;
end;

function TBoldObjectDefaultMapper.InternalIdListSegmentToWhereFragment(
  IdList: TBoldObjectIdList; Start, Stop: integer;
  Parameterized: IBoldParameterized): String;

  function GetParamStr(IdIndex, ParamIndex: integer; UseParams: Boolean): String;
  begin
    if UseParams then
    begin
      result := ':ID' + IntToStr(ParamIndex); // do not localize
      Parameterized.CreateParam(ftInteger, 'ID' + IntToStr(ParamIndex), ptInput, SizeOf(Integer)).AsInteger := (IdList[IdIndex] as TBoldDefaultId).AsInteger; // do not localize
    end
    else
      result := IdList[Idindex].asString
  end;
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
    result := BoldSeparateStringList(TempLIst, ', ', 'in (', ')'); // do not localize
  end;
end;

function TBoldObjectDefaultMapper.IdListSegmentToWhereFragment(
  IdList: TBoldObjectIdList; Start, Stop: integer;
  Query: IBoldExecQuery): String;
begin
  result := InternalIdListSegmentToWhereFragment(IdList, Start, Stop, Query as IBoldParameterized);
end;

function TBoldObjectDefaultMapper.IdListSegmentToWhereFragment(
  IdList: TBoldObjectIdList; Start, Stop: integer;
  Query: IBoldQuery): String;
begin
  result := InternalIdListSegmentToWhereFragment(IdList, Start, Stop, Query as IBoldParameterized);
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
        ClassName);
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
      // expansion below is to truncate the names to the right length again...
      ColumnNames := ColumnNames + BoldExpandName(InitialColumnName[i], '', xtSQL, SystemPersistenceMapper.SQLDataBaseConfig.MaxDBIdentifierLength, SystemPersistenceMapper.NationalCharConversion);
    end;

    // this will loop over the name, all former names, and the names of the association
    for i := 0 to MoldClass.AllPossibleNames.count - 1 do
      GenerateForClassName(MoldClass.AllPossibleNames[i]);
  end;
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
  TheNowValue: TDateTime;
begin
  aQuery := GetQuery;
  aExecQuery := GetExecQuery;
  try
    aExecQuery.AssignSQLText(
      Format('UPDATE %s SET %s = %s + 1', [ // do not localize
        PSSystemDescription.TimeStampTable.SQLName,
        TimeStampTableTimeStampColumn.SQLname,
        TimeStampTableTimeStampColumn.SQLname]));
    aExecQuery.ExecSQL;

    aQuery.AssignSQLText(
      Format('SELECT %s FROM %s', [ // do not localize
        TimeStampTableTimeStampColumn.SQLname,
        PSSystemDescription.TimeStampTable.SQLName]));
    aQuery.Open;
    FCurrentTimeStamp := aQuery.Fields[0].AsInteger;
    aQuery.Close;

    if UseClockLog then
    begin
      aQuery.AssignSQLText(
        Format('SELECT %s, %s FROM %s', [ // do not localize
          LastClockTableLastTimeStampColumn.SqlName,
          LastClockTableLastClockColumn.SQLName,
          PSSystemDescription.LastClockTable.SQLName]));
      aQuery.Open;
      LastClockTimestamp := aQuery.Fields[0].AsInteger;
      LastClock := aQuery.Fields[1].AsDateTime;
      aQuery.Close;

      TheNowValue := GetCorrectTime;
      if (TheNowValue - LastClock) > ClockLogGranularity then
      begin
        aExecQuery.AssignSQLText(
          Format('UPDATE %s SET %s = :newTimestamp, %s = :newClock', [ // do not localize
            PSSystemDescription.LastClockTable.SQLName,
            LastClockTableLastTimeStampColumn.SQLName,
            LastClockTableLastClockColumn.SQLName]));
        aExecQuery.ParamByName('newTimestamp').AsInteger := FCurrentTimeStamp; // do not localize
        aExecQuery.ParamByName('newClock').AsDateTime := TheNowValue; // do not localize
        aExecQuery.ExecSQL;

        aExecQuery.AssignSQLText(Format('INSERT INTO %s (%s, %s, %s, %s) VALUES (:lastTimeStamp, :thisTimeStamp, :lastClock, :thisClock)', // do not localize
                       [PSSystemDescription.ClockLogTable.SQLName,
                       ClockLogTableLastTimeStampColumn.SQLName,
                       ClockLogTableThisTimeStampColumn.SQLName,
                       ClockLogTableLastClockColumn.SQLName,
                       ClockLogTableThisClockColumn.SQLName]));
        aExecQuery.ParamByName('lastTimeStamp').AsInteger := LastClockTimestamp; // do not localize
        aExecQuery.ParamByName('thisTimeStamp').AsInteger := FCurrentTimeStamp; // do not localize
        aExecQuery.ParamByName('lastClock').AsDateTime := LastClock; // do not localize
        aExecQuery.ParamByName('thisClock').AsDateTime := TheNowValue; // do not localize
        aExecQuery.ExecSQL;
      end;
    end;
  finally
    ReleaseQuery(aQuery);
    ReleaseExecQuery(aExecQuery);
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
    if atable.ContainsStopTimeStamp then
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
  ObjectID: TBoldObjectId; ValueSpace: IBoldValueSpace;
  TranslationList: TBoldIdTranslationList; DataSet: IBoldDataSet);
begin
  if SystemPersistenceMapper.UseTimestamp then
    ValueSpace.EnsuredObjectContentsByObjectId[ObjectID].TimeStamp := DataSet.FieldByName(SystemPersistenceMapper.XFilesTimeStampColumn.SQLname).AsInteger;
  if SystemPersistenceMapper.UseGlobalId then
    ValueSpace.EnsuredObjectContentsByObjectId[ObjectID].GlobalId := DataSet.FieldByName(SystemPersistenceMapper.XFilesGlobalIdColumn.SQLName).AsString;
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
        Format('SELECT %s, %s, %s FROM %s WHERE %0:s IN (%4:s)', [ // do not localize
          IDCOLUMN_NAME,
          TYPECOLUMN_NAME,
          XFilesGlobalIdColumn.SQLName,
          PSSystemDescription.XFilestable.SQLName,
          BoldSeparateStringList(IDList, ', ', '', '')]));

      aQuery.Open;

      while not aQuery.EOF do
      begin
        anObjectId := NewIdFromQuery(aQuery, 1, 0, BOLDMAXTIMESTAMP);
        aGlobalId := NewGlobalIdFromQuery(aQuery, 1);

        TranslationList.AddTranslation(anObjectId, aGlobalId);

        anObjectId.Free;

        aGlobalId.Free;

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
        Format('SELECT %s, %s, %s FROM %s WHERE %0:s IN (%4:s)', [ // do not localize
          XFilesGlobalIdColumn.SQLName,
          IDCOLUMN_NAME,
          TYPECOLUMN_NAME,
          PSSystemDescription.XFilestable.SQLName,
          BoldSeparateStringList(IdList, ''', ''', '''', '''')]));

      aQuery.Open;

      while not aQuery.EOF do
      begin
        aGlobalId := NewGlobalIdFromQuery(aQuery, 2);
        anObjectId := NewIdFromQuery(aQuery, 2, 1, BoldMaxTimeStamp);

        TranslationList.AddTranslation(aGlobalId, anObjectId);

        freeandnil(anObjectId);
        freeAndNil(aGlobalId);

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
  ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace);
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
        Format('SELECT %s, %s, %s FROM %s WHERE %0:s IN (%4:s)', [ // do not localize
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
  if MissingIds.COunt > 0 then
    raise EBold.Create(MissingIds.Text);
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
          Format('UPDATE %s SET %s = %d WHERE %s IN (%s)', [ // do not localize
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
    aQuery.FieldByName(XFilesGlobalIdColumn.SQLName).AsString,
    TopSortedIndex,
    true,
    ObjectPersistenceMappers[TopSortedIndex].ExpressionName);
end;

procedure TBoldMemberDefaultMapper.PMFetch(ObjectIdList: TBoldObjectIdList;
  ValueSpace: IBoldValueSpace; FetchMode: Integer;
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
  // Note, when the list gets here it contains only object of one class, from one
  // Bold, that are not previously present in the Bold (or possibly a forced fetch)

  // all objectids are the same timestamp, this has been taken care of by PMFetchExactId
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
      SQL.Text := format('SELECT %s FROM %s WHERE %s ', [BoldSeparateStringList(TempLIst, ', ', '', ''), Table.SQLName, IDCOLUMN_NAME]); // do not localize

      Start := Block * BlockSize;
      Stop := MinIntValue([Pred(Succ(Block) * BlockSize), ObjectCount]);
      if Start <= stop then
      begin
        BoldAppendToStrings(SQL, ObjectPersistenceMapper.IdListSegmentToWhereFragment(ObjectIdList, start, stop, aQuery), False);

        aQuery.AssignSQL(sql);
        aQuery.Open;
        while not aQuery.EOF do
        begin
          tempId := SystemPersistenceMapper.NewIdFromQuery(aQuery, 1, 0, ObjectIDList[0].TimeStamp);
          NewId := ObjectIDList.IDByID[TempId];
          TempId.Free;

          if not assigned(NewId) then
            raise EBoldInternal.CreateFmt('%s.PMFetch: Database returned object we didn''t ask for (ID: %d)', [ClassName, aQuery.Fields[0].AsInteger]); // do not localize

          if not NewId.TopSortedIndexExact then
            NewId := TranslationList.TranslateToNewID[NewId];

          if not NewId.TopSortedIndexExact then
            raise EBoldInternal.CreateFmt('%s.PMFetch: Got an Id with no or only approx class!', [Classname]); // do not localize

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

procedure TBoldSystemDefaultMapper.PMFetchClassWithCondition(
  ObjectIDList: TBoldObjectIdList; ValueSpace: IBoldValueSpace;
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

  procedure FixQueriesForEnv(VarBinding: TBoldSQLVariableBinding; Context: TBoldObjectIdList; NameSpace: TBoldSqlNameSpace);
  var
    MainTableRef: TBoldSqlTableReference;
  begin
    if VarBinding.VariableName = 'SELF' then // do not localize
    begin
      VarBinding.NewQuery(NameSpace);

      MainTableRef := VarBinding.TableReferenceForTable(VarBinding.ObjectMapper.MainTable, VarBinding.Query, true);
      VarBinding.Context := OclCondition.Context;
      VarBinding.Query.AddWCF(TBoldSQLWCFBinaryInfix.CreateWCFForIdList(MainTableRef.GetColumnReference(IDCOLUMN_NAME), OclCondition.Context));
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
    sql := TStringList.Create;
    OclCondition := BoldCondition as TBoldOclCondition;
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

      BoldCondition.AvailableAnswers := GetListUsingQuery(ObjectIdList, ValueSpace, Q2, FetchMode, TranslationList, BOLDMAXTIMESTAMP, BoldCondition.MaxAnswers, BoldCondition.Offset);
    finally
      ReleaseQuery(q2);
      sql.Free;
      SQLGenerator.Free;
      SQlNodeResolver.free;
      GlobalNameSpace.Free;
      SQLNodeMaker.Free;
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
        BoldCondition.AvailableAnswers := GetListUsingQuery(ObjectIdList, ValueSpace, Q2, FetchMode, TranslationList, BOLDINVALIDTIMESTAMP, BoldCondition.MaxAnswers, BoldCondition.Offset);
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

function TBoldMemberDefaultMapper.CompareFields(ObjectContent: IBoldObjectContents; DataSet: IBoldDataSet; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList): Boolean;
var
  aField    : IBoldField;
  ColumnIndex: Integer;
begin
  result := true;
  if SupportsComparingWithoutValue or assigned(GetValue(ObjectContent)) then
  begin
    for ColumnIndex := 0 to ColumnCount - 1 do
    begin
      aField := DataSet.FieldByName(ColumnDescriptions[ColumnIndex].SQLName);
      if Assigned(aField) then
      begin
        if not CompareField(ObjectContent, aField, ColumnIndex, ValueSpace, TranslationList) then
        begin
          BoldLog.LogFmt(sOptimisticLockingFailed,
            [ObjectPersistenceMapper.ExpressionName,
            ExpressionName,
            ObjectContent.ObjectId.AsString,
            ColumnIndex,
            ColumnDescriptions[ColumnIndex].SQLName]);
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
      format('SELECT %s FROM %s WHERE :time1 >= %s and :time2 < %s', [ // do not localize
        ClockLogTableLastClockColumn.SQLname,
        PSSystemDescription.ClockLogTable.SQLName,
        ClockLogTableLastTimeStampColumn.SQLName,
        ClockLogTableThisTimeStampColumn.SQLName]));
    aQuery.ParamByName('time1').AsInteger := Timestamp; // do not localize
    aQuery.ParamByName('time2').AsInteger := Timestamp; // do not localize
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
      Format('SELECT %s FROM %s WHERE :time1 > %s and :time2 <= %s', [ // do not localize
        ClockLogTableThisTimeStampColumn.SQLName,
        PSSystemDescription.ClockLogTable.SQLName,
        ClockLogTableLastClockColumn.SQLname,
        ClockLogTableThisClockColumn.SQLname]));
    aQuery.ParamByName('time1').AsDateTime := ClockTime; // do not localize
    aQuery.ParamByName('time2').AsDateTime := ClockTime; // do not localize
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

function TBoldMemberDefaultMapper.GetObjectPersistenceMapper: TBoldObjectDefaultMapper;
begin
  result := (inherited ObjectPersistenceMapper) as TBoldObjectDefaultMapper;
end;

{ EBoldOptimisticLockingFailed }

constructor EBoldOptimisticLockingFailed.Create(msg: string; args: array of const; FailedObjects: TBoldObjectIdList);
begin
  inherited Create(Msg, Args, FailedObjects);
end;

procedure TBoldObjectDefaultMapper.DetectLinkClassDuplicates(
  ObjectIdList: TBoldObjectidList; ValueSpace: IBoldvalueSpace;
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

  // if we find a linkobject that links the same two objects as one of the new,
  // translate the new object to the old object.

  QueryText := format('SELECT %s, %s FROM %s WHERE (%s = %%s) AND (%s = %%s)', [ // do not localize
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

      // if either object is new, then this can not be a dupe
      if not (ObjectisNew(Id1) or ObjectIsNew(Id2)) then
      begin
        if assigned(Id1) then
          Id1 := TranslationList.TranslateToNewId[Id1];
        if assigned(Id2) then
          Id2 := TranslationList.TranslateToNewId[Id2];

        // if the linkobject is broken (doesn't have 2 IDs) or is pointing to objects that are deleted,
        // then we will get an inconsistent database later, but lets not get an AV here...

        if assigned(Id1) and assigned(Id2) then
        begin
          Query.AssignSQLText(format(QueryText, [Id1.AsString, Id2.AsString]));
          Query.open;
          if not query.Eof then
          begin
            OldLinkObjectId := SystemPersistenceMapper.NewIdFromQuery(Query, 1, 0, BOLDMAXTIMESTAMP);
            try
              // the new linkobject has already received a translation to its persistent ID
              // we need to add a translation from that ID to the existing link objectid
              TranslationList.addTranslation(TranslationList.TranslateToNewId[ObjectIdList[i]], OldLinkObjectId);
              DuplicateList.Add(ObjectIdList[i]);
            finally
              OldLinkObjectId.Free;
            end;
          end;
          Query.Close;
        end;
      end;
    end;

  finally
    SystemPersistenceMapper.ReleaseQuery(Query);
  end;
end;

function TBoldMemberDefaultMapper.CheckEitherNull(field: IBoldField;
  Value: IBoldValue; var Equal: Boolean): Boolean;
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
    for i := 0 to Columns.Count - 1 do
      ColumnDescriptions.Add(
        SystemPersistenceMapper.EnsureColumn(MemberMappings[0].TableName,
          Columns[i],
          ColumnTypeAsSQL[i],
          AllowNullAsSQL,
          ColumnBDEFieldType[i],
          ColumnSize[i],
          AllowNull,
          ObjectPersistenceMapper.Versioned,  // ??? is this really right? is it not supposed to be the mapper of the class that defines the attribute? - Yes, in theory. In practice it makes no difference.
          DefaultDbValue));
  end
  else if (length(memberMappings) = 0) and RequiresMemberMapping then
    raise EBold.CreateFmt(sUnableToFindMappingForX, [ObjectPersistenceMapper.ExpressionName, ExpressionName]);
end;

procedure TBoldObjectDefaultMapper.InitializePSDescriptions;
begin
  // it is important that the x-files table come before the other tables
  // (and in general that the tables for a superclass comes before the tables of a subclass)
  if SystemPersistenceMapper.UseXFiles then
    AllTables.Add(SystemPersistenceMapper.PSSystemDescription.XFilestable);

  inherited;

  if not assigned(SuperClass) then
    SystemPersistenceMapper.PSSystemDescription.RootTable := MainTable;

  // the starttimestamp column is added in EnsureTable
  // root class needs to have a stopcolumn as well...
  if Versioned and (self = SystemPersistenceMapper.RootClassObjectPersistenceMapper) then
  begin
    SystemPersistenceMapper.EnsureColumn(MainTable.SQLName, TIMESTAMPSTOPCOLUMNNAME, SystemPersistenceMapper.SQLDataBaseConfig.ColumnTypeForInteger, SystemPersistenceMapper.SQLDataBaseConfig.EffectiveSQLforNotNull, BOLDTIMESTAMPFIELDTYPE, 0, true, true, SystemPersistenceMapper.SQLDataBaseConfig.CorrectlyQuotedDefaultValue(intTostr(BOLDMAXTIMESTAMP)));
    // the following index improves performance alot in Interbase, and seems to have no negative impact in SQLServer.
    MainTable.EnsureIndex(TIMESTAMPSTOPCOLUMNNAME, false, false);
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
  // inherited is called at the end to ensure that systemtables are available to other parts of initialization

  // Create BOLD_ID
  PSSystemDescription.IdTable := TBoldSQLTableDescription.Create(PSSystemDescription, false);
  PSSystemDescription.IdTable.SQLName := IDTABLE_NAME;
  PSSystemDescription.IdTable.AddColumn(IDCOLUMN_NAME, SQLDataBaseConfig.ColumnTypeForInteger, SQLDataBaseConfig.EffectiveSQLforNotNull, IDCOLUMN_TYPE, 0, False, '');

  // Create BOLD_TYPES
  PSSystemDescription.TypeTable := TBoldSQLTableDescription.Create(PSSystemDescription, false);
  PSSystemDescription.TypeTable.SQLName := TYPETABLE_NAME;
  PSSystemDescription.TypeTable.AddColumn(TYPECOLUMN_NAME, SQLDataBaseConfig.ColumnTypeForSmallInt, SQLDataBaseConfig.EffectiveSQLforNotNull, TYPECOLUMN_TYPE, 0, False, '');
  PSSystemDescription.TypeTable.AddColumn(CLASSNAMECOLUMN_NAME, format(SQLDataBaseConfig.ColumnTypeForString, [DefaultStringLength]), SQLDataBaseConfig.EffectiveSQLforNotNull, ftString, DefaultStringLength, False, '');

  if UseXFiles then
  begin
    PSSystemDescription.XFilesTable := TBoldSQLTableDescription.Create(PSSystemDescription, false);
    PSSystemDescription.XFilesTable.SQLName := TABLEPREFIXTAG + '_XFILES'; // do not localize
    PSSystemDescription.XFilesTable.AddColumn(IDCOLUMN_NAME, SQLDataBaseConfig.ColumnTypeForInteger, SQLDataBaseConfig.EffectiveSQLforNotNull, IDCOLUMN_TYPE, 0, False, '');
    PSSystemDescription.XFilesTable.AddColumn(TYPECOLUMN_NAME, SQLDataBaseConfig.ColumnTypeForSmallInt, SQLDataBaseConfig.EffectiveSQLforNotNull, TYPECOLUMN_TYPE, 0, False, '');
    PSSystemDescription.XFilesTable.EnsureIndex(IDCOLUMN_NAME, true, true);
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

function TBoldMemberDefaultMapper.GetSystemPersistenceMapper: TBoldSystemDefaultMapper;
begin
  result := inherited SystemPersistenceMapper as TBoldSystemDefaultMapper;
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
  ObjectContent: IBoldObjectContents; Field: IBoldField; ColumnIndex: integer;
  ValueSpace: IBoldValueSpace;
  TranslationList: TBoldIdTranslationList): Boolean;
begin
// actually this method is irrelevant
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
  ObjectContents: IBoldObjectContents): Boolean;
begin
  result := true;
end;

function TBoldModelVersionMember.ShouldFetch(
  ObjectContents: IBoldObjectContents): Boolean;
begin
  // in an environment that uses ModelVersion,
  // it must always be fetched so that an automatic update can be performed
  result := true;
end;

procedure TBoldModelVersionMember.ValueFromField(
  OwningObjectId: TBoldObjectId; ObjectContent: IBoldObjectContents;
  ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList;
  Field: IBoldField; ColumnIndex: Integer);
begin
// do nothing
end;

procedure TBoldModelVersionMember.ValueToParam(
  ObjectContent: IBoldObjectContents; Param: IBoldParameter;
  ColumnIndex: Integer; TranslationList: TBoldIdTranslationList);
begin
  Param.AsInteger := VersionNumber;
end;

function TBoldModelVersionMember.VersionFromQuery(Query: IBoldQuery): Integer;
var
  aField: IBoldField;
begin
  aField := Query.FieldByName(ColumnDescriptions[0].SQLName);
  if assigned(aField) then
    result := aField.AsInteger
  else
    raise EBoldInternal.CreateFmt(sColumnNotFoundInTable, [classname, ColumnDescriptions[0].SQLName]);
end;

{ TBoldReadOnlynessMember }

function TBoldReadOnlynessMember.CompareField(
  ObjectContent: IBoldObjectContents; Field: IBoldField; ColumnIndex: integer;
  ValueSpace: IBoldValueSpace;
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
  ObjectContents: IBoldObjectContents): Boolean;
begin
  result := false;
end;

function TBoldReadOnlynessMember.ShouldFetch(
  ObjectContents: IBoldObjectContents): Boolean;
begin
  result := true;
end;

procedure TBoldReadOnlynessMember.ValueFromField(
  OwningObjectId: TBoldObjectId; ObjectContent: IBoldObjectContents;
  ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList;
  Field: IBoldField; ColumnIndex: Integer);
begin
  ObjectContent.SetIsReadOnly(field.AsInteger = 1);
end;

procedure TBoldReadOnlynessMember.ValueToParam(
  ObjectContent: IBoldObjectContents; Param: IBoldParameter;
  ColumnIndex: Integer; TranslationList: TBoldIdTranslationList);
begin
  // since IsDirty is false, this will only happen on Create or
  // when using UpdateWholeObjects (which is a bad thing...)
  Param.AsInteger := 0;
end;

function TBoldObjectDefaultMapper.IsOldVersion(Query: IBoldQuery): Boolean;
var
  CurrentObjectVersion: integer;
begin
  CurrentObjectVersion := fModelVersionMember.VersionFromQuery(Query);
  result := (CurrentObjectVersion < SystemPersistenceMapper.ModelVersion) and assigned(SystemPersistenceMapper.ObjectUpgrader) and
    SystemPersistenceMapper.ObjectUpgrader.NeedsManualUpdate(ExpressionName, CurrentObjectVersion);
end;

procedure TBoldObjectDefaultMapper.PortObject(ObjectId: TBoldObjectId; Query: IBoldQuery);
begin
  if assigned(SystemPersistenceMapper.ObjectUpgrader) then
    SystemPersistenceMapper.ObjectUpgrader.UpgradeObjectById(ObjectId, Query);
end;


{ TBoldTimeStampMember }

function TBoldTimeStampMember.CompareField(
  ObjectContent: IBoldObjectContents; Field: IBoldField;
  ColumnIndex: integer; ValueSpace: IBoldValueSpace;
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
  // unfortunately, it is too early to call the
  // SystemPersistenceMapper.XFilesTable.SQLName since it would create the
  // PSDescriptions before all the Mappers are in place.
  result := BoldExpandPrefix(TABLEPREFIXTAG + '_XFILES', '', SystemPersistenceMapper.SQLDatabaseConfig.SystemTablePrefix, SystemPersistenceMapper.SQLDataBaseConfig.MaxDBIdentifierLength, SystemPersistenceMapper.NationalCharConversion); // do not localize
end;

function TBoldTimeStampMember.GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType;
begin
  result := BOLDTIMESTAMPFIELDTYPE;
end;

function TBoldTimeStampMember.GetColumnTypeAsSQL(ColumnIndex: Integer): string;
begin
  result := SystemPersistenceMapper.SQLDataBaseConfig.ColumnTypeForInteger;
end;


function TBoldTimeStampMember.IsDirty(ObjectContents: IBoldObjectContents): Boolean;
begin
  result := true;
end;

function TBoldTimeStampMember.ShouldFetch(ObjectContents: IBoldObjectContents): Boolean;
begin
  // the timestamp should only be loaded if the object uses timestamp-mode for optimistic locking
  result := ObjectPersistenceMapper.fOptimisticLockingMode = bolmTimeStamp;
end;

function TBoldTimeStampMember.SupportsComparingWithoutValue: Boolean;
begin
  result := true;
end;

procedure TBoldTimeStampMember.ValueFromField(
  OwningObjectId: TBoldObjectId; ObjectContent: IBoldObjectContents;
  ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList;
  Field: IBoldField; ColumnIndex: Integer);
begin
  ObjectContent.TimeStamp := Field.AsInteger;
end;

procedure TBoldTimeStampMember.ValueToParam(
  ObjectContent: IBoldObjectContents; Param: IBoldParameter;
  ColumnIndex: Integer; TranslationList: TBoldIdTranslationList);
begin
  Param.AsInteger := SystemPersistenceMapper.CurrentTimeStamp;
end;

{ TBoldGlobalIdMember }

function TBoldGlobalIdMember.CompareField(
  ObjectContent: IBoldObjectContents; Field: IBoldField;
  ColumnIndex: integer; ValueSpace: IBoldValueSpace;
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
  fContentName := 'String'; // do not localize
  fIsStoredInObject := true;
  fInitialColumnRootName := GLOBALIDCOLUMN_NAME;
  inherited;
end;

function TBoldGlobalIdMember.FindDefiningTable(LocalMoldClass: TMoldClass; MoldMember: TMoldMember): string;
begin
  // unfortunately, it is too early to call the
  // SystemPersistenceMapper.XFilesTable.SQLName since it would create the
  // PSDescriptions before all the Mappers are in place.
  result := BoldExpandPrefix(TABLEPREFIXTAG + '_XFILES', '', SystemPersistenceMapper.SQLDatabaseConfig.SystemTablePrefix, SystemPersistenceMapper.SQLDataBaseConfig.MaxDBIdentifierLength, SystemPersistenceMapper.NationalCharConversion); // do not localize
end;

function TBoldGlobalIdMember.GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType;
begin
  result := ftString;
end;

function TBoldGlobalIdMember.GetColumnTypeAsSQL(ColumnIndex: Integer): string;
begin
  // {F4252AB4-8FFA-460F-BDBD-1BB57D588D14}
  result := format(SystemPersistenceMapper.SQLDataBaseConfig.ColumnTypeForString, [39]);
end;


function TBoldGlobalIdMember.IsDirty(
  ObjectContents: IBoldObjectContents): Boolean;
begin
  result := false;
end;

function TBoldGlobalIdMember.ShouldFetch(ObjectContents: IBoldObjectContents): Boolean;
begin
  result := false;
end;

procedure TBoldGlobalIdMember.ValueFromField(OwningObjectId: TBoldObjectId;
  ObjectContent: IBoldObjectContents; ValueSpace: IBoldValueSpace;
  TranslationList: TBoldIdTranslationList; Field: IBoldField;
  ColumnIndex: Integer);
begin
  ObjectContent.GlobalId := Field.AsString;
end;

procedure TBoldGlobalIdMember.ValueToParam(
  ObjectContent: IBoldObjectContents; Param: IBoldParameter;
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
  // since these members do not require membermappings... they have to build their PSDesc manually
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


