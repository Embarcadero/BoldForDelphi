unit BoldPMappersSQL;

interface

uses
  classes,
  db,
  BoldMeta,
  BoldDefs,
  BoldPSParams,
  BoldPMappers,
  BoldDbInterfaces,
  BoldId,
  BoldSQLMappingInfo,
  BoldSQLDatabaseConfig,
  BoldValueSpaceInterfaces,
  BoldTypeNameDictionary,
  BoldPSDescriptionsSQL,
  BoldTaggedValueSupport;

type
  { forward declarations }
  TBoldSystemSQLMapper = class;
  TBoldObjectSQLMapper = class;
  TBoldMemberSQLMapper = class;
  TBoldPreInitializeBoldDbType = procedure(SystemSQLMapper: TBoldSystemSQLMapper) of Object;

  EBoldDbTypeMissing = class(EBold);

  {---Enumerations---}
  TBoldDBStorageMode = (dsmUpdate, dsmCreate);

  {---TBoldSystemSQLMapper---}
  TBoldSystemSQLMapper = class(TBoldSystemPersistenceMapper)
  private
    fOnPreInitializeBoldDbType: TBoldPreInitializeBoldDbType;
    fTransactionStartedByMe: Boolean;
    fMappingInfo: TBoldSQLMappingInfo;
    fNationalCharConversion: TBoldNationalCharConversion;
    fSQLDataBaseConfig: TBoldSQLDataBaseConfig;
    fOnGetDatabase: TBoldGetDatabaseEvent;
    function GetIBoldDataBase: IBoldDataBase;
    function GetAllTables: TBoldSQLTableDescriptionList;
    function GetPSSystemDescription: TBoldSQLSystemDescription;
    function GetRootClassObjectPersistenceMapper: TBoldObjectSQLMapper;
    function GetMappingInfo: TBoldSQLMappingInfo;
  protected
    procedure InitializeBoldDbType; virtual; abstract;
    procedure FillPSParams(PSParams: TBoldPSParams); override;
    function CreatePSParams: TBoldPSParams; override;
    procedure StartTransaction(ValueSpace: IBoldValueSpace); override;
    procedure Commit(ValueSpace: IBoldValueSpace); override;
    procedure RollBack(ValueSpace: IBoldValueSpace); override;
    function CreateMappingInfo: TBoldSQLMappingInfo; virtual; abstract;
  public
    constructor CreateFromMold(MoldModel: TMoldModel; TypeNameDictionary: TBoldTypeNameDictionary; SQLDatabaseConfig: TBoldSQLDatabaseConfig; GetDatabaseFunc: TBoldGetDatabaseEvent);
    destructor Destroy; override;
    procedure PMFetch(ObjectIDList: TBoldObjectIdList;
                      ValueSpace: IBoldValueSpace;
                      MemberIdList: TBoldMemberIdList;
                      FetchMode: Integer;
                      TranslationList: TBoldIdTranslationList); override;
    function BoldDbTypeForTopSortedIndex(TopSortedIndex: Integer): TBoldDbType;
    procedure CloseDataBase;
    procedure CreatePersistentStorage; override;
    procedure GenerateDatabaseScript(Script: TStrings; Separator: string); virtual;
    function EnsureTable(const TableName: string; TableVersioned: Boolean): TBoldSQLTableDescription; virtual; abstract;
    function GetQuery: IBoldQuery;
    function GetExecQuery: IBoldExecQuery;
    procedure InvokeMemberMappersInitializeSystem; virtual;
    procedure OpenDatabase(ReadDbTypeFromDB: Boolean; ReadMappingFromDB: Boolean);
    procedure ReleaseQuery(var aQuery: IBoldQuery);
    procedure ReleaseExecQuery(var aQuery: IBoldExecQuery);
    function TopSortedIndexForBoldDbType(BoldDbType: TBoldDbType): Integer;
    property AllTables: TBoldSQLTableDescriptionList read GetAllTables;
//  property Connected: Boolean read GetConnected;
    property Database: IBoldDataBase read GetIBoldDataBase;
    property OnPreInitializeBoldDbType: TBoldPreInitializeBoldDbType read fOnPreInitializeBoldDbType write fOnPreInitializeBoldDbType;
    property PSSystemDescription: TBoldSQLSystemDescription read GetPSSystemDescription;
    property RootClassObjectPersistenceMapper: TBoldObjectSQLMapper read GetRootClassObjectPersistenceMapper;
    property MappingInfo: TBoldSQLMappingInfo read GetMappingInfo;
    property NationalCharConversion: TBoldNationalCharConversion read fNationalCharConversion;
    property SQLDataBaseConfig: TBoldSQLDataBaseConfig read fSQLDataBaseConfig;
  end;

  {---TBoldObjectSQLMapper---}
  TBoldObjectSQLMapper = class(TBoldObjectPersistenceMapper)
  private
    fBoldDbType: TBoldDbType;  // FIXME move down
    fAllTables: TBoldSQLTableDescriptionList;
    fMainTable: TBoldSQLTableDescription;
    function GetSystemPersistenceMapper: TBoldSystemSQLMapper;
    function GetAllTables: TBoldSQLtableDescriptionList;
  protected
    procedure SQLForMembers(Table: TBoldSQLTableDescription; SQL: TStrings; const MemberList: TBoldMemberPersistenceMapperList; const SQLStyle: TBoldSQLStyle; const IncludeKey: Boolean; const StoredInObjectOnly, useAlias: boolean);
    procedure SQLForKey(Table: TBoldSQLTableDescription; SQL: TStrings; const SQLStyle: TBoldSQLStyle; useAlias: Boolean); virtual; abstract;
    procedure SQLForDistributed(SQL: TStrings; const SQLStyle: TBoldSQLStyle); virtual; abstract;

    procedure JoinSQLTableByKey(SQL: TStringList; MainTable, JoinTable: TBoldSQLTableDescription); virtual; abstract;
    procedure SQLForID(Table: TBoldSQLTableDescription; SQL: TStrings; useAlias: Boolean); virtual; abstract;
    function TableAlias(Table: TBoldSQLTableDescription; useAlias: Boolean): String;
    function DistributableTable: TBoldSQLTableDescription; virtual; abstract;
    procedure InitializePSDescriptions; override;
  public
    constructor CreateFromMold(MoldClass: TMoldClass; Owner: TBoldSystemPersistenceMapper; TypeNameDictionary: TBoldTypeNameDictionary); override;
    destructor Destroy; override;
    procedure GenerateDatabaseScript(Script: TStrings; Separator: string); virtual;
    function UpdatesMembersInTable(aTable: TBoldSQLTableDescription): Boolean; virtual; abstract;
    property AllTables: TBoldSQLtableDescriptionList read GetAllTables;
    property MainTable: TBoldSQLTableDescription read fmainTable;
    procedure RetrieveSelectStatement(s: TStrings; MemberMapperList: TBoldMemberPersistenceMapperList; FetchMode: Integer; ForceRootTable: Boolean); virtual;
    procedure RetrieveTimeStampCondition(SQL: TStrings; TimeStamp: TBoldTimeStampType; UseAlias: Boolean; WhereToken: string; UseOwnTableForStartTime: Boolean; SuggestedStartTimeAlias: string = ''; SuggestedEndTimeAlias: string = '');
    property BoldDbType: TBoldDbType read fBoldDbType write fBoldDbType;
    procedure ValuesFromFieldsByMemberList(ObjectId: TBoldObjectId; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; DataSet: IBoldDataSet; MemberList: TBoldMemberPersistenceMapperList);
    procedure ValuesToParamsByMemberList(ObjectId: TBoldObjectId; ValueSpace: IBoldValueSpace; Query: IBoldExecQuery; memberList: TBoldMemberPersistenceMapperList; TranslationList: TBoldIdTranslationList; DBStorageMode: TBoldDBStorageMode);
    property SystemPersistenceMapper: TBoldSystemSQLMapper read GetSystemPersistenceMapper;
  end;

  {---TBoldMemberSQLMapper---}
  TBoldMemberSQLMapper = class(TBoldMemberPersistenceMapper)
  private
    fColumnDescriptions: TBoldSQLDescriptionList;
    function GetSystemPersistenceMapper: TBoldSystemSQLMapper;
    function GetObjectPersistenceMapper: TBoldObjectSQLMapper;
    function GetColumnDescriptions: TBoldSQLDescriptionList;
  protected
    fAllowNull: Boolean;
    fInitialColumnRootName: string;
    fDefaultDbValue: String;
    function GetColumnTypeAsSQL(ColumnIndex: Integer): string; virtual; abstract;
    function GetAllowNullAsSQL: string; virtual; abstract;
    function GetColumnCount: Integer; virtual; abstract;
    function GetColumnSize(ColumnIndex: Integer): Integer; virtual;
    function GetInitialColumnName(ColumnIndex: Integer): string; virtual;
    function DefaultDefaultDbValue: String; virtual;
    property InitialColumnRootName: string read fInitialColumnRootName;
    function GetRequiresLiveQuery: Boolean; virtual;
    procedure SetParamToNullWithDataType(aParam: IBoldParameter; FieldType: TFieldType);
    function GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType; virtual;
    property InitialColumnName[Columnindex: integer]: string read GetInitialColumnName;
  public
    constructor CreateFromMold(Moldmember: TMoldMember; MoldClass: TMoldClass; Owner: TBoldObjectPersistenceMapper; const MemberIndex: Integer; TypeNameDictionary: TBoldTypeNameDictionary); override;
    destructor Destroy; override;
    procedure GenerateDatabaseScript(Script: TStrings; Separator: string); virtual;
    property ColumnDescriptions: TBoldSQLDescriptionList read GetColumnDescriptions;
    property AllowNullAsSQL: string read GetAllowNullAsSQL;
    property ColumnCount: integer read GetColumnCount;
    property ColumnTypeAsSQL[Columnindex: integer]: string read GetColumnTypeAsSQL;
    property ColumnSize[Columnindex: integer]: integer read GetColumnSize;
    property AllowNull: Boolean read fAllowNull;
//    procedure ValueToParam(Value: IBoldValue; Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList); virtual;
    procedure ValueToParam(ObjectContent: IBoldObjectContents; Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList); virtual;
    procedure ValueFromField(OwningObjectId: TBoldObjectId; ObjectContent: IBoldObjectContents; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; Field: IBoldField; ColumnIndex: Integer); virtual;
    procedure ValueToQuery(ObjectContent: IBoldObjectContents; ValueSpace: IBoldValueSpace; Query: IBoldExecQuery; TranslationList: TBoldIdtranslationlist; DBStorageMode: TBoldDBStorageMode);
    procedure ValueFromQuery(OwningObjectId: TBoldObjectId; ObjectContent: IBoldObjectContents; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; DataSet: IBoldDataSet); virtual;
    procedure InitializeSystem(theDatabase: IBoldDataBase); virtual;
    property ColumnBDEFieldType[Columnindex: integer]: TFieldType read GetColumnBDEFieldType;
    property RequiresLiveQuery: Boolean read GetRequiresLiveQuery;
    property SystemPersistenceMapper: TBoldSystemSQLMapper read GetSystemPersistenceMapper;
    property ObjectPersistenceMapper: TBoldObjectSQLMapper read GetObjectPersistenceMapper;
    property DefaultDbValue: String read fDefaultDbValue write fDefaultDbValue;
  end;

implementation

uses
  BoldUtils,
  BoldPSParamsSQL,
  BoldNameExpander,
  SysUtils,
  BoldPMConsts;

{---TBoldSystemSQLMapper---}
function TBoldSystemSQLMapper.GetAllTables: TBoldSQLTableDescriptionList;
begin
  Result := PSSystemDescription.SQLTablesList;
end;

function TBoldSystemSQLMapper.GetPSSystemDescription: TBoldSQLSystemDescription;
begin
  result := (inherited PSSystemDescription) as TBoldSQLSystemDescription;
end;

constructor TBoldSystemSQLMapper.CreateFromMold(MoldModel: TMoldModel; TypeNameDictionary: TBoldTypeNameDictionary; SQlDatabaseConfig: TBoldSQLDatabaseConfig; GetDatabaseFunc: TBoldGetDatabaseEvent);
begin
  fNationalCharConversion := MoldModel.NationalCharConversion;
  fSQLDataBaseConfig := TBoldSQLDataBaseConfig.Create;
  fSQLDataBaseConfig.AssignConfig(SQLDatabaseConfig);
  fOnGetDatabase := GetDatabaseFunc;
  inherited createFromMold(MoldModel, TypeNameDictionary);
end;

procedure TBoldSystemSQLMapper.FillPSParams(PSParams: TBoldPSParams);
begin
  (PSParams as TBoldPSSQLParams).Database := Database;
end;

function TBoldSystemSQLMapper.CreatePSParams: TBoldPSParams;
begin
  Result := TBoldPSSQLParams.Create;
  FillPSParams(Result);
end;

function TBoldSystemSQLMapper.GetQuery: IBoldQuery;
begin
  result := Database.GetQuery;
end;

procedure TBoldSystemSQLMapper.ReleaseQuery(var aQuery: IBoldQuery);
begin
  Database.ReleaseQuery(aQuery);
end;

function TBoldSystemSQLMapper.GetIBoldDataBase: IBoldDataBase;
begin
  if not assigned(fOnGetDatabase) then
    raise EBoldInternal.CreateFmt('%s: No event that provides an IBoldDatabase', [classname]);
  result := fOnGetDatabase;
  if not assigned(result) then
    raise EBold.CreateFmt(sNoDatabase, [classname]);
end;

procedure TBoldSystemSQLMapper.StartTransaction(ValueSpace: IBoldValueSpace);
begin
  with Database do
    if IsSqlBased and (not InTransaction) then
    begin
      StartTransaction;
      fTransactionStartedByMe := true;
    end
    else
      fTransactionStartedByMe := false;
end;

procedure TBoldSystemSQLMapper.Commit(ValueSpace: IBoldValueSpace);
begin
  if fTransactionStartedByMe then
    with Database do
      if IsSqlBased and InTransaction then
        Commit;
  fTransactionStartedByMe := false;
end;

procedure TBoldSystemSQLMapper.RollBack(ValueSpace: IBoldValueSpace);
begin
  if fTransactionStartedByMe then
    with Database do
      if IsSqlBased and InTransaction then
        RollBack;
  fTransactionStartedByMe := false;
end;

procedure TBoldSystemSQLMapper.OpenDatabase(ReadDbTypeFromDB: Boolean; ReadMappingFromDB: Boolean);
begin
  try
    Database.OPEN;
    MappingInfo.ReadDataFromDB(DataBase, ReadDbTypeFromDB, ReadMappingFromDB);

    if assigned(OnPreInitializeBoldDbType) then
      OnPreInitializeBoldDbType(self);    // used for DBStructureValidator

    InitializeBoldDbType;
    EnsurePSDescription;
  except
    DataBase.Close;
    raise;
  end;
end;

procedure TBoldSystemSQLMapper.InvokeMemberMappersInitializeSystem;
var
  I, J: Integer;
  MemberMapperObjects: TList;
  MemberMapperTypes: TList;
  Obj: TObject;
  om: TBoldObjectPersistenceMapper;
begin
  MemberMapperObjects := TList.Create;
  MemberMapperTypes := TList.Create;
  try
    // create unique list of all member mappers
    for I := 0 to ObjectPersistenceMappers.Count - 1 do
    begin
      om := ObjectPersistenceMappers[I];
      if assigned(om) then
      begin
        for J := 0 to ObjectPersistenceMappers[I].MemberPersistenceMappers.Count - 1 do
        begin
          Obj := ObjectPersistenceMappers[I].MemberPersistenceMappers[J];
          if (Obj is TBoldMemberSQLMapper) and (MemberMapperTypes.IndexOf(Obj.ClassType) = -1) then
          begin
            MemberMapperObjects.Add(Obj);
            MemberMapperTypes.Add(Obj.ClassType);
          end;
        end;
      end;
    end;
    // finally initialize member mappers
    for I := 0 to MemberMapperObjects.Count - 1 do
      TBoldMemberSQLMapper(MemberMapperObjects[I]).InitializeSystem(Database);
  finally
    MemberMapperObjects.Free;
    MemberMapperTypes.Free;
  end;
end;

procedure TBoldSystemSQLMapper.CreatePersistentStorage;
begin
  inherited;
  InvokeMemberMappersInitializeSystem;
  MappingInfo.WriteDataToDB(Database);
end;

{---TBoldObjectSQLMapper---}
constructor TBoldObjectSQLMapper.CreateFromMold(MoldClass: TMoldClass; Owner: TBoldSystemPersistenceMapper; TypeNameDictionary: TBoldTypeNameDictionary);
begin
  inherited;
  assert(SystemPersistenceMapper is TBoldSystemSQLMapper);
  fBoldDbType := -1;
end;

destructor TBoldObjectSQLMapper.destroy;
begin
  FreeAndNil(fAllTables);
  inherited;
end;

function TBoldObjectSQLMapper.TableAlias(Table: TBoldSQLTableDescription; useAlias: Boolean): String;
var
  i: integer;
begin
  result := '';
  if useAlias then
  begin
    if Table = DistributableTable then
      result := 'X_FILES' // do not localize
    else
    begin
      i := AllTables.Indexof(Table) + 1;
      while i > 0 do
      begin
        Result := Result + chr(65 + i mod 25);
        i := i div 25;
      end;
      while assigned(alltables.ItemsBySQLName[Result]) do
        result := result + '_';
    end;
  end;
end;

procedure TBoldObjectSQLMapper.SQLForMembers(Table: TBoldSQLTableDescription; SQL: TStrings; const MemberList: TBoldMemberPersistenceMapperList; const SQLStyle: TBoldSQLStyle; const IncludeKey: Boolean; const StoredInObjectOnly, UseAlias: Boolean);
var
  c,
  m: integer;
  Column: TBoldSQLColumnDescription;
begin
  if (SQLStyle in [ssColumns, ssParameters]) and IncludeKey  then
  begin
    if assigned(Table) then
      SQLForKey(Table, SQL, SQLStyle, UseAlias)
    else
      SQLForKey(MainTable, SQL, SQLStyle, UseAlias)
  end;

  for m := 0 to MemberList.Count - 1 do
    if Assigned(MemberList[m]) and
       (not StoredInObjectOnly or MemberList[m].IsStoredInObject) then
      for c := 0 to (MemberList[m] as TBoldMemberSQLMapper).ColumnDescriptions.Count - 1 do
      begin
        Column := (MemberList[m] as TBoldMemberSQLMapper).ColumnDescriptions[c] as TBoldSQLColumnDescription;
        if not assigned(Table) or (Column.TableDescription = table) then
        begin
          if assigned(Table) then
            case SQLStyle of
              ssColumns: SQL.Append(Column.SQLName);
              ssParameters: SQL.Append(Format(':%s', [Column.SQLName])); // do not localize
              ssValues: SQL.Append(Format('%s = :%0:s', [Column.SQLName])); // do not localize
              else
                raise EBold.Create(sUnimplemented);
            end
          else
            case SQLStyle of
              ssColumns: SQL.Append(Format('%s.%s', [TableAlias(Column.TableDescription, useAlias), Column.SQLName])) // do not localize
              else
                raise EBold.Create(sUnimplemented);
            end;
        end;
      end;
end;

procedure TBoldObjectSQLMapper.RetrieveSelectStatement(s: TStrings; MemberMapperList: TBoldMemberPersistenceMapperList; FetchMode: Integer; ForceRootTable: Boolean);
var
  T: Integer;
  SelectList: TStringList;
  FromList: TStringList;
  WhereList: TStringList;
  JoinList: TStringList;
  Join: String;
  MapperIx, ColumnIx: integer;
  TableList: TBoldSQLtableDescriptionList;
  Table: TBoldSQLtableDescription;
  Mapper: TBoldMemberSQLMapper;
begin
  SelectList := TStringList.Create;
  FromList := TStringList.Create;
  WhereList := TStringList.Create;
  JoinLIst :=  TStringList.Create;
  TableList := TBoldSQLtableDescriptionList.Create(SystemPersistenceMapper.PSSystemDescription);
  tableList.OwnsEntries := false;

  try
    if ForceRootTable then
      TableList.Add(SystemPersistenceMapper.RootClassObjectPersistenceMapper.MainTable);

    for MapperIx := 0 to MemberMapperList.count - 1 do
    begin
      Mapper := MemberMapperList[MapperIx] as TBoldMemberSQLMapper;
      for ColumnIx := 0 to Mapper.ColumnDescriptions.Count - 1 do
      begin
        Table := (Mapper.ColumnDescriptions[ColumnIx] as TBoldSQLColumnDescription).TableDescription;
        if TableList.IndexOf(Table) = -1 then
          TableList.Add(Table);
      end;
    end;

    SQLForMembers(nil, SelectList,  MemberMapperList,  ssColumns, true, false, true);

    FromList.Append(Format('%s %s', [MainTable.SQLName, TableAlias(MainTable, true)])); // do not localize
    if SystemPersistenceMapper.SQLDataBaseConfig.UseSQL92Joins and (FetchMode = fmNormal) then
    begin
      // left join rateable x on Song.Bold_ID=rateable.Bold_id
      for T := 0 to TableList.COunt - 1 do
        if (TableList[T].ColumnsList.Count > 2) and (TableList[T] <> MainTable) then
        begin
          Join := format('left join %s %s on ', [TableList[T].SQLName, TableAlias(TableList[T], true)]); // do not localize
          JoinList.Clear;
          JoinSQLTableByKey(JoinList, MainTable, TableList[T]);
          Join := Join + BoldSeparateStringList(JoinList, ' and ', '(', ')'); // do not localize
          FromList.Add(Join);
        end;
    end
    else
    begin
      for T := 0 to TableList.Count - 1 do
        if (TableList[T].ColumnsList.Count > 2) and (TableList[T] <> MainTable) then
          FromList.Append(Format('%s %s',[TableList[T].SQLName, TableAlias(TableList[t], true)])); // do not localize

      for T := 0 to TableList.Count - 1 do
      begin
        if (TableList[T].ColumnsList.Count > 2) and (TableList[T] <> MainTable) then
          JoinSQLTableByKey(WhereList, MainTable, TableList[T]);
      end;

{      if FetchMode = fmDistributable then
      begin
        SQLForDistributed(SelectList, ssColumns);
        FromList.Append(Format('%s %s', [DistributableTable.SQLName,
                                          TableAlias(DistributableTable, true)]));
        JoinSQLTableByKey(WhereList, MainTable, DistributableTable);
      end;}
    end;

    SQLForID(Maintable, WhereList, True);

    BoldAppendToStrings(S, Format('SELECT %s ', [BoldSeparateStringList(SelectList, ', ', '', '')]), true); // do not localize

    if SystemPersistenceMapper.SQLDataBaseConfig.UseSQL92Joins and (FetchMode = fmNormal) then
      BoldAppendToStrings(S, Format('FROM %s ', [BoldSeparateStringList(FromList, ' ', '', '')]), true) // do not localize
    else
      BoldAppendToStrings(S, Format('FROM %s ', [BoldSeparateStringList(FromList, ', ', '', '')]), true); // do not localize

    BoldAppendToStrings(S, Format('WHERE %s ', [BoldSeparateStringList(WhereList, ' AND ', '', '')]), true); // do not localize
  finally
    SelectList.Free;
    FromList.Free;
    WhereList.Free;
    JoinList.Free;
    TableList.Free;
  end;
end;

procedure TBoldObjectSQLMapper.ValuesFromFieldsByMemberList(ObjectID: TBoldObjectId; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; DataSet: IBoldDataSet; memberList: TBoldMemberPersistenceMapperList);
var
  i: integer;
  ObjectContents: IBoldObjectContents;
begin
  ObjectContents := ValueSpace.EnsuredObjectContentsByObjectId[ObjectID];
  for i:= 0 to memberlist.count - 1 do
    TBoldMemberSQLMapper(Memberlist[i]).ValueFromQuery(ObjectId, ObjectContents, ValueSpace, TranslationList, DataSet);
end;

procedure TBoldObjectSQLMapper.ValuesToParamsByMemberList(ObjectID: TBoldObjectId; ValueSpace: IBoldValueSpace; Query: IBoldExecQuery; memberList: TBoldMemberPersistenceMapperList; TranslationList: TBoldIdTranslationList; DBStorageMode: TBoldDBStorageMode);
var
  i: integer;
begin
  for i := 0 to memberlist.count - 1 do
    TBoldMemberSQLMapper(Memberlist[i]).ValueToQuery(ValueSpace.ObjectContentsByObjectId[ObjectID], ValueSpace, Query, translationList, DBStorageMode);
end;

{---TBoldMemberSQLMapper---}
constructor TBoldMemberSQLMapper.CreateFromMold(Moldmember: TMoldMember; MoldClass: TMoldClass; Owner: TBoldObjectPersistenceMapper; const MemberIndex: Integer; TypeNameDictionary: TBoldTypeNameDictionary);
begin
  inherited;
  if not assigned(MoldMember) then
    exit;

  fInitialColumnRootName := BoldExpandName(MoldMember.ColumnName, MoldMember.name, xtSQL, SystemPersistenceMapper.SQLDataBaseConfig.MaxDBIdentifierLength, MoldClass.Model.NationalCharConversion);

  if MoldMember is TMoldAttribute then
    fDefaultDbValue := (MoldMember as TMoldAttribute).DefaultDBValue;

  if fDefaultDbValue = '' then
    fDefaultDbValue := DefaultDefaultDbValue;

  if MoldMember is TMoldAttribute then
    fAllowNull := (MoldMember as TMoldAttribute).AllowNull
  else
    fAllowNull := true;
end;

destructor TBoldMemberSQLMapper.destroy;
begin
  FreeAndNil(fColumnDescriptions);
  fInitialColumnRootName := '';
  inherited;
end;

function TBoldMemberSQLMapper.GetColumnSize(ColumnIndex: Integer): Integer;
begin
  raise EBold.CreateFmt(sIllegalColumnIndex, [ClassName, 'GetColumnSize', ColumnIndex]); // do not localize
end;

function TBoldMemberSQLMapper.GetInitialColumnName(ColumnIndex: Integer): string;
begin
  Result := InitialColumnRootName;
  if ColumnIndex > 0 then
    Result := Format('%s_%d', [Result, ColumnIndex]); // do not localize
end;

function TBoldSystemSQLMapper.BoldDbTypeForTopSortedIndex(
  TopSortedIndex: Integer): TBoldDbType;
begin
  result := (ObjectPersistenceMappers[TopSortedIndex] as TBoldObjectSQLMapper).BoldDbType;
end;

function TBoldSystemSQLMapper.TopSortedIndexForBoldDbType(
  BoldDbType: TBoldDbType): Integer;
var
  i: integer;
begin
  result := -1;
  for i := 0 to ObjectPersistenceMappers.Count - 1 do
    if (ObjectPersistenceMappers[i] is TBoldObjectSQLMapper) and // tests for nil
       ((ObjectPersistenceMappers[i] as TBoldObjectSQLMapper).BoldDbType = BoldDbType) then
    begin
      result := i;
      break;
    end;
end;

procedure TBoldMemberSQLMapper.SetParamToNullWithDataType(aParam: IBoldParameter; FieldType: TFieldType);
begin
  with aParam do
  begin
    Clear;
    if DataType <> FieldType then
      DataType := FieldType;
  end;
end;

procedure TBoldMemberSQLMapper.ValueToParam(ObjectContent: IBoldObjectContents; Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList);
begin
  raise EBold.CreateFmt(sIllegalCall, [classname, 'ValueToParam']); // do not localize
end;

procedure TBoldMemberSQLMapper.ValueFromField(OwningObjectId: TBoldObjectId; ObjectContent: IBoldObjectContents; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; Field: IBoldField; ColumnIndex: Integer);
begin
  raise EBold.CreateFmt(sIllegalCall, [classname, 'ValueFromField']); // do not localize
end;

procedure TBoldMemberSQLMapper.ValueToQuery(ObjectContent: IBoldObjectContents; ValueSpace: IBoldValueSpace; Query: IBoldExecQuery; TranslationList: TBoldIdTranslationList; DBStorageMode: TBoldDBStorageMode);
var
  aParam    : IBoldParameter;
  ColumnIndex: Integer;
begin
  if (DBStorageMode = dsmCreate) or IsDirty(ObjectContent) then
  begin
    for ColumnIndex := 0 to ColumnCount - 1 do
    begin
      aParam := Query.ParamByName(ColumnDescriptions[ColumnIndex].SQLName);
      if Assigned(aParam) then
        ValueToParam(ObjectContent, aParam, ColumnIndex, translationList)
      else
        raise EBoldInternal.CreateFmt(sSomeColumnsNotInTable, [classname, 'ValueToQuery', ColumnIndex, ColumnDescriptions[ColumnIndex].SQLName]); // do not localize
    end;
  end;
end;

procedure TBoldMemberSQLMapper.ValueFromQuery(OwningObjectId: TBoldObjectId; ObjectContent: IBoldObjectContents; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; DataSet: IBoldDataSet);
var
  aField    : IBoldField;
  ColumnIndex: Integer;
begin
  if ShouldFetch(ObjectContent) then
  begin
    for ColumnIndex := 0 to ColumnCount - 1 do
    begin
      aField := DataSet.FieldByName(ColumnDescriptions[ColumnIndex].SQLName);
      if Assigned(aField) then
        ValueFromField(OwningObjectId, ObjectContent, ValueSpace, TranslationList, aField, ColumnIndex)
      else
        raise EBoldInternal.CreateFmt(sSomeColumnsNotInTable, [classname, 'ValueFromQuery', ColumnIndex, ColumnDescriptions[ColumnIndex].SQLName]); // do not localize
    end;
  end;
end;

procedure TBoldMemberSQLMapper.InitializeSystem(theDatabase: IBoldDataBase);
begin
end;

function TBoldMemberSQLMapper.GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType;
begin
  raise EBold.CreateFmt(sIllegalColumnIndex, [ClassName, 'GetColumnBDEFieldType', ColumnIndex]); // do not localize
end;

procedure TBoldSystemSQLMapper.CloseDataBase;
begin
  Database.Close;
end;

{function TBoldSystemSQLMapper.GetConnected: Boolean;
begin
  result := assigned(DataBase) and DataBase.Connected;
end;
}

function TBoldMemberSQLMapper.GetRequiresLiveQuery: Boolean;
begin
  result := false;
end;

function TBoldObjectSQLMapper.GetSystemPersistenceMapper: TBoldSystemSQLMapper;
begin
  result := (inherited SystemPersistenceMapper) as TBoldSystemSQLMapper;
end;

function TBoldMemberSQLMapper.GetSystemPersistenceMapper: TBoldSystemSQLMapper;
begin
  result := ObjectPersistenceMapper.systemPersistenceMapper;
end;

function TBoldMemberSQLMapper.GetObjectPersistenceMapper: TBoldObjectSQLMapper;
begin
  result := (inherited ObjectPersistenceMapper) as TBoldObjectSQLMapper;
end;

procedure TBoldObjectSQLMapper.RetrieveTimeStampCondition(SQL: TStrings; TimeStamp: TBoldTimeStampType; UseAlias: Boolean; WhereToken: string; UseOwnTableForStartTime: Boolean; SuggestedStartTimeAlias: string = ''; SuggestedEndTimeAlias: string = '');
var
  StartTimeTable: TBoldSQLTableDescription;
  EndTimeTable: TBoldSQLTableDescription;
  StartTimeTableAlias: String;
  EndTimeTableAlias: String;
begin
  EndTimeTable := SystemPersistenceMapper.RootClassObjectPersistenceMapper.MainTable;
  if UseOwnTableForStartTime then
    StartTimeTable := MainTable
  else
    StartTimeTable := EndTimeTable;

  if useAlias then
  begin
    StartTimeTableAlias := tableAlias(StartTimeTable, true);
    EndTimeTableAlias := tableAlias(EndTimeTable, true);
  end else
  begin
    if SuggestedStartTimeAlias <> '' then
      StartTimeTableAlias := SuggestedStartTimeAlias
    else
      StartTimeTableAlias := StartTimeTable.SQLName;
    if SuggestedEndTimeAlias <> '' then
      EndTimeTableAlias := SuggestedEndTimeAlias
    else
      EndTimeTableAlias := EndTimeTable.SQLName;
  end;

  if TimeStamp = BOLDMAXTIMESTAMP then
    SQL.Append(Format('%s (%s.%s = %d)', [WhereToken, // do not localize
      EndTimeTableAlias, TIMESTAMPSTOPCOLUMNNAME, TimeStamp]))
  else
    SQL.Append(Format('%s ((%s.%s <= %d) and (%s.%s >= %d))', [WhereToken, // do not localize
      StartTimeTableAlias, TIMESTAMPSTARTCOLUMNNAME, TimeStamp,
      EndTimeTableAlias, TIMESTAMPSTOPCOLUMNNAME, TimeStamp]));
end;

function TBoldSystemSQLMapper.GetRootClassObjectPersistenceMapper: TBoldObjectSQLMapper;
begin
  result := (inherited RootClassObjectPersistenceMapper) as TBoldObjectSQLMapper;
end;

function TBoldSystemSQLMapper.GetMappingInfo: TBoldSQLMappingInfo;
begin
  if not assigned(fMappingInfo) then
    fMappingInfo := CreateMappingInfo;
  result := fMappingInfo;
end;

destructor TBoldSystemSQLMapper.Destroy;
begin
  FreeAndNil(fMappingInfo);
  FreeAndNil(fSQLDataBaseConfig);
  inherited;
end;

procedure TBoldObjectSQLMapper.InitializePSDescriptions;
var
  i: Integer;
  OSArr: TBoldObjectStorageMappingArray;
  AIArr: TBoldAllInstancesMappingArray;
  procedure RecurseSuperMappers(Mapper: TBoldObjectSQLMapper);
  begin
    // we want the superclass tables to come before the subclass tables, so the recursion is done first.
    if assigned(Mapper.SuperClass) then
      RecurseSuperMappers(Mapper.SuperClass as TBoldObjectSQLMapper);
    if assigned(Mapper.MainTable) and (Alltables.IndexOf(Mapper.MainTable) = -1) then
      AllTables.Add(Mapper.MainTable)
  end;
begin
  inherited;
  OSArr := SystemPersistenceMapper.MappingInfo.GetObjectStorageMapping(ExpressionName);
  if Length(OSArr) = 0 then
  begin
    if assigned(SuperClass) then
      RecurseSuperMappers(SuperClass as TBoldObjectSQLMapper);
  end
  else
  begin
    for i := 0 to Length(OSArr) - 1 do
      AllTables.Add(SystemPersistenceMapper.EnsureTable(OSArr[i].TableName, Versioned));
  end;

  AIArr := SystemPersistenceMapper.MappingInfo.GetAllInstancesMapping(ExpressionName);
  if Length(AIArr) = 1 then
    fMainTable := SystemPersistenceMapper.EnsureTable(AIArr[0].TableName, Versioned);

  if assigned(MainTable) and (AllTables.IndexOf(Maintable) = -1) then
    AllTables.Add(Maintable);
end;

function TBoldMemberSQLMapper.DefaultDefaultDbValue: String;
begin
  result := '';
end;

procedure TBoldSystemSQLMapper.GenerateDatabaseScript(Script: TStrings; Separator: string);
var
  i: integer;
begin
  PSSystemDescription.GenerateDatabaseScript(Script, Separator);
  for i := 0 to ObjectPersistenceMappers.Count - 1 do
    if assigned(ObjectPersistenceMappers[i]) then
      (ObjectPersistenceMappers[i] as TBoldObjectSQLMapper).GenerateDatabaseScript(script, separator);
  MappingInfo.ScriptForWriteData(Script, Separator, false);
end;

procedure TBoldObjectSQLMapper.GenerateDatabaseScript(Script: TStrings; Separator: string);
var
  i: integer;
begin
  for i := 0 to MemberPersistenceMappers.Count - 1 do
    if assigned(MemberPersistenceMappers[i]) then
      (MemberPersistenceMappers[i] as TBoldMemberSQLMapper).GenerateDatabaseScript(script, separator);
end;

procedure TBoldMemberSQLMapper.GenerateDatabaseScript(Script: TStrings; Separator: string);
begin
  // intentionally left blank
end;

function TBoldObjectSQLMapper.GetAllTables: TBoldSQLtableDescriptionList;
begin
  if not assigned(fAllTables) then
  begin
    fAllTables := TBoldSQLTableDescriptionList.Create(TBoldSystemSQLMapper(SystemPersistenceMapper).PSSystemDescription);
    fAllTables.OwnsEntries := False;
  end;
  result := fAllTables;
end;

function TBoldMemberSQLMapper.GetColumnDescriptions: TBoldSQLDescriptionList;
begin
  if not assigned(fColumnDescriptions) then
  begin
    fColumnDescriptions := TBoldSQLDescriptionList.Create(SystemPersistenceMapper.PSSystemDescription);
    fColumnDescriptions.OwnsEntries := false;
  end;
  result := fColumnDescriptions;
end;

function TBoldSystemSQLMapper.GetExecQuery: IBoldExecQuery;
begin
  result := Database.GetExecQuery;
end;

procedure TBoldSystemSQLMapper.ReleaseExecQuery(var aQuery: IBoldExecQuery);
begin
  Database.ReleaseExecQuery(aQuery);
end;

procedure TBoldSystemSQLMapper.PMFetch(ObjectIDList: TBoldObjectIdList;
  ValueSpace: IBoldValueSpace; MemberIdList: TBoldMemberIdList;
  FetchMode: Integer; TranslationList: TBoldIdTranslationList);
var
  WasInTransaction: Boolean;
begin
  WasInTransaction := Database.InTransaction;
  try
    inherited;
  finally
    if not WasInTransaction and Database.InTransaction then
      Database.Commit;
  end;
end;

end.
