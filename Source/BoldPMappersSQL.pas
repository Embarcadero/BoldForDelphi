{ Global compiler directives }
{$include bold.inc}
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
  BoldIndexCollection,
  BoldValueSpaceInterfaces,
  BoldTypeNameDictionary,
  BoldPSDescriptionsSQL,
  BoldTaggedValueSupport,
  BoldElements;

type
  { forward declarations }
  TBoldSystemSQLMapper = class;
  TBoldObjectSQLMapper = class;
  TBoldMemberSQLMapper = class;
  TBoldPreInitializeBoldDbType = procedure(SystemSQLMapper: TBoldSystemSQLMapper) of Object;
  TBoldOnPsEvaluate = procedure(const ABoldQuery: IBoldQuery) of Object;

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
    fCustomIndexes: TBoldIndexCollection;
    fOnGetDatabase: TBoldGetDatabaseEvent;
    fMaxDbType: integer;
    fExecQuery: IBoldExecQuery;
    fOnPsEvaluate: TBoldOnPsEvaluate;
    fTopSortedIndexForBoldDbType: array of integer; // JNo, optimization
    function GetIBoldDataBase: IBoldDataBase;
    function GetAllTables: TBoldSQLTableDescriptionList;
    function GetPSSystemDescription: TBoldSQLSystemDescription;
    function GetRootClassObjectPersistenceMapper: TBoldObjectSQLMapper;
    function GetMappingInfo: TBoldSQLMappingInfo;
    procedure InitializeTopSortedIndexForBoldDbType;
  protected
    procedure InitializeBoldDbType; virtual; abstract;
    procedure FillPSParams(PSParams: TBoldPSParams); override;
    function CreatePSParams: TBoldPSParams; override;
    procedure StartTransaction(const ValueSpace: IBoldValueSpace); override;
    procedure Commit(const ValueSpace: IBoldValueSpace); override;
    procedure RollBack(const ValueSpace: IBoldValueSpace); override;
    function CreateMappingInfo: TBoldSQLMappingInfo; virtual; abstract;
    procedure AddCustomIndexes;
  public
    constructor CreateFromMold(MoldModel: TMoldModel; TypeNameDictionary: TBoldTypeNameDictionary;
      CustomIndexes: TBoldIndexCollection; SQLDatabaseConfig: TBoldSQLDatabaseConfig; GetDatabaseFunc: TBoldGetDatabaseEvent);
    destructor Destroy; override;
    procedure PMFetch(ObjectIDList: TBoldObjectIdList;
                      const ValueSpace: IBoldValueSpace;
                      MemberIdList: TBoldMemberIdList;
                      FetchMode: Integer;
                      TranslationList: TBoldIdTranslationList); override;
    function BoldDbTypeForTopSortedIndex(TopSortedIndex: Integer): TBoldDbType;
    procedure CloseDataBase;
    procedure CreatePersistentStorage; override;
    procedure GenerateDatabaseScript(Script: TStrings); virtual;
    function EnsureTable(const TableName: string; TableVersioned: Boolean): TBoldSQLTableDescription; virtual; abstract;
    function EnsureColumn(const TableName, ColumnName, SQLType,
      SQLAllowNull: string; const BDEType: TFieldType; Length: Integer;
      const AllowNull, InVersionedTable: Boolean;
      const DefaultDBValue: String): TBoldSQLColumnDescription;
    procedure EnsureIndex(const TableName, Fields: string; const PrimaryIndex,
      Unique, InVersionedTable: Boolean);
    function GetQuery: IBoldQuery;
    function GetExecQuery: IBoldExecQuery;
    procedure StartSQLBatch; override;
    procedure EndSQLBatch; override;
    procedure FailSQLBatch; override;
    procedure InvokeMemberMappersInitializeSystem; virtual;
    procedure OpenDatabase(ReadDbTypeFromDB: Boolean; ReadMappingFromDB: Boolean);
    procedure ReleaseQuery(var aQuery: IBoldQuery);
    procedure ReleaseExecQuery(var aQuery: IBoldExecQuery);
    function TopSortedIndexForBoldDbType(BoldDbType: TBoldDbType): Integer;
    function CanEvaluateInPS(sOCL: string; aSystem: TBoldElement;  aContext: TBoldElementTypeInfo = nil; const aVariableList: TBoldExternalVariableList = nil): Boolean; virtual; abstract;
    property AllTables: TBoldSQLTableDescriptionList read GetAllTables;
    property Database: IBoldDataBase read GetIBoldDataBase;
    property OnPreInitializeBoldDbType: TBoldPreInitializeBoldDbType read fOnPreInitializeBoldDbType write fOnPreInitializeBoldDbType;
    property PSSystemDescription: TBoldSQLSystemDescription read GetPSSystemDescription;
    property RootClassObjectPersistenceMapper: TBoldObjectSQLMapper read GetRootClassObjectPersistenceMapper;
    property MappingInfo: TBoldSQLMappingInfo read GetMappingInfo;
    property NationalCharConversion: TBoldNationalCharConversion read fNationalCharConversion;
    property SQLDataBaseConfig: TBoldSQLDataBaseConfig read fSQLDataBaseConfig;
    property OnPsEvaluate: TBoldOnPsEvaluate read fOnPsEvaluate write fOnPsEvaluate;
  end;

  {---TBoldObjectSQLMapper---}
  TBoldObjectSQLMapper = class(TBoldObjectPersistenceMapper)
  private
    fBoldDbType: TBoldDbType;
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
    procedure GenerateDatabaseScript(Script: TStrings); virtual;
    function UpdatesMembersInTable(aTable: TBoldSQLTableDescription): Boolean; virtual; abstract;
    property AllTables: TBoldSQLtableDescriptionList read GetAllTables;
    property MainTable: TBoldSQLTableDescription read fmainTable;
    procedure RetrieveSelectStatement(s: TStrings; MemberMapperList: TBoldMemberPersistenceMapperList; FetchMode: Integer; ForceRootTable: Boolean); virtual;
    procedure RetrieveTimeStampCondition(SQL: TStrings; TimeStamp: TBoldTimeStampType; UseAlias: Boolean; WhereToken: string; UseOwnTableForStartTime: Boolean; SuggestedStartTimeAlias: string = ''; SuggestedEndTimeAlias: string = '');
    property BoldDbType: TBoldDbType read fBoldDbType write fBoldDbType;
    procedure ValuesFromFieldsByMemberList(ObjectId: TBoldObjectId; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; const DataSet: IBoldDataSet; MemberList: TBoldMemberPersistenceMapperList);
    procedure ValuesToParamsByMemberList(ObjectId: TBoldObjectId; const ValueSpace: IBoldValueSpace; const Query: IBoldExecQuery; memberList: TBoldMemberPersistenceMapperList; TranslationList: TBoldIdTranslationList; DBStorageMode: TBoldDBStorageMode);
    procedure ValuesToQueryByMemberList(ObjectId: TBoldObjectId; const ValueSpace: IBoldValueSpace; const Query: IBoldExecQuery; SQL: TStrings; memberList: TBoldMemberPersistenceMapperList; TranslationList: TBoldIdTranslationList; DBStorageMode: TBoldDBStorageMode);
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
    procedure SetParamToNullWithDataType(const aParam: IBoldParameter; FieldType: TFieldType);
    function GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType; virtual;
    property InitialColumnName[Columnindex: integer]: string read GetInitialColumnName;
  public
    constructor CreateFromMold(Moldmember: TMoldMember; MoldClass: TMoldClass; Owner: TBoldObjectPersistenceMapper; const MemberIndex: Integer; TypeNameDictionary: TBoldTypeNameDictionary); override;
    destructor Destroy; override;
    procedure GenerateDatabaseScript(Script: TStrings); virtual;
    property ColumnDescriptions: TBoldSQLDescriptionList read GetColumnDescriptions;
    property AllowNullAsSQL: string read GetAllowNullAsSQL;
    property ColumnCount: integer read GetColumnCount;
    property ColumnTypeAsSQL[Columnindex: integer]: string read GetColumnTypeAsSQL;
    property ColumnSize[Columnindex: integer]: integer read GetColumnSize;
    property AllowNull: Boolean read fAllowNull;
    function ValueAsVariant(const ObjectContent: IBoldObjectContents; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList): variant; virtual;
    procedure ValueToParam(const ObjectContent: IBoldObjectContents; const Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList); virtual;
    procedure ValueFromField(OwningObjectId: TBoldObjectId; const ObjectContent: IBoldObjectContents; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; const Field: IBoldField; ColumnIndex: Integer); virtual;
    procedure ValueToQuery(const ObjectContent: IBoldObjectContents; const ValueSpace: IBoldValueSpace; const Query: IBoldExecQuery; TranslationList: TBoldIdtranslationlist; DBStorageMode: TBoldDBStorageMode);
    procedure ValueFromQuery(OwningObjectId: TBoldObjectId; const ObjectContent: IBoldObjectContents; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; const DataSet: IBoldDataSet);
    procedure InitializeSystem(const theDatabase: IBoldDataBase); virtual;
    property ColumnBDEFieldType[Columnindex: integer]: TFieldType read GetColumnBDEFieldType;
    property RequiresLiveQuery: Boolean read GetRequiresLiveQuery;
    property SystemPersistenceMapper: TBoldSystemSQLMapper read GetSystemPersistenceMapper;
    property ObjectPersistenceMapper: TBoldObjectSQLMapper read GetObjectPersistenceMapper;
    property DefaultDbValue: String read fDefaultDbValue write fDefaultDbValue;
  end;

implementation

uses
  SysUtils,
  Variants,

  BoldCoreConsts,
  BoldUtils,
  BoldPSParamsSQL,
  BoldNameExpander,
  BoldValueInterfaces,
  BoldDefaultStreamNames
  {$IFDEF RIL}
  {$IFNDEF BOLD_UNICODE}
  ,StringBuilder
  {$ENDIF}
  {$ENDIF}
  ;
  
{---TBoldSystemSQLMapper---}
function TBoldSystemSQLMapper.GetAllTables: TBoldSQLTableDescriptionList;
begin
  Result := PSSystemDescription.SQLTablesList;
end;

function TBoldSystemSQLMapper.GetPSSystemDescription: TBoldSQLSystemDescription;
begin
  result := (inherited PSSystemDescription) as TBoldSQLSystemDescription;
end;

constructor TBoldSystemSQLMapper.CreateFromMold(MoldModel: TMoldModel; TypeNameDictionary: TBoldTypeNameDictionary;
  CustomIndexes: TBoldIndexCollection; SQlDatabaseConfig: TBoldSQLDatabaseConfig; GetDatabaseFunc: TBoldGetDatabaseEvent);
begin
  fNationalCharConversion := MoldModel.NationalCharConversion;
  fSQLDataBaseConfig := SQlDatabaseConfig;
  fCustomIndexes := TBoldIndexCollection.Create(nil);
  if Assigned(CustomIndexes) then
    fCustomIndexes.Assign(CustomIndexes);
  fOnGetDatabase := GetDatabaseFunc;
  fMaxDbType := -1;
  inherited CreateFromMold(MoldModel, TypeNameDictionary, SQlDatabaseConfig.DefaultObjectMapper);
  AddCustomIndexes;
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

function TBoldSystemSQLMapper.GetExecQuery: IBoldExecQuery;
begin
  if not Assigned(fExecQuery) then
  begin
    fExecQuery := Database.GetExecQuery;
  end;
  result :=  fExecQuery;
end;

procedure TBoldSystemSQLMapper.ReleaseExecQuery(var aQuery: IBoldExecQuery);
begin
  if fExecQuery <> aQuery then
    Database.ReleaseExecQuery(aQuery);
end;

function TBoldSystemSQLMapper.GetIBoldDataBase: IBoldDataBase;
begin
  if not assigned(fOnGetDatabase) then
    raise EBoldInternal.CreateFmt('%s: No event that provides an IBoldDatabase', [classname]);
  result := fOnGetDatabase;
  if not assigned(result) then
    raise EBold.CreateFmt(sNoDatabase, [classname]);
end;

procedure TBoldSystemSQLMapper.StartSQLBatch;
begin
  GetExecQuery.StartSQLBatch;
end;

procedure TBoldSystemSQLMapper.EndSQLBatch;
begin
  GetExecQuery.EndSQLBatch;
end;

procedure TBoldSystemSQLMapper.FailSQLBatch;
begin
  GetExecQuery.FailSQLBatch;
end;

procedure TBoldSystemSQLMapper.StartTransaction(const ValueSpace: IBoldValueSpace);
begin
{$IFDEF BOLD_LITE}
  if Database.InTransaction then
    raise EBold.Create('Transactions not supported in Bold Lite');
{$ELSE}
  with Database do
    if IsSqlBased and (not InTransaction) then
    begin
      StartTransaction;
      fTransactionStartedByMe := true;
    end
    else
      fTransactionStartedByMe := false;
{$ENDIF}
end;

procedure TBoldSystemSQLMapper.Commit(const ValueSpace: IBoldValueSpace);
begin
{$IFDEF BOLD_LITE}
  if Database.InTransaction then
    raise EBold.Create('Transactions not supported in Bold Lite');
{$ELSE}
  if fTransactionStartedByMe then
    with Database do
      if IsSqlBased and InTransaction then
        Commit;
  fTransactionStartedByMe := false;
{$ENDIF}
end;

procedure TBoldSystemSQLMapper.RollBack(const ValueSpace: IBoldValueSpace);
begin
{$IFDEF BOLD_LITE}
  if Database.InTransaction then
    raise EBold.Create('Transactions not supported in Bold Lite');
{$ELSE}
  if fTransactionStartedByMe then
    with Database do
      if IsSqlBased and InTransaction then
        RollBack;
  fTransactionStartedByMe := false;
{$ENDIF}
end;

function TBoldSystemSQLMapper.EnsureColumn(const TableName, ColumnName, SQLType, SQLAllowNull: string;
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

procedure TBoldSystemSQLMapper.EnsureIndex(const TableName, Fields: string; const PrimaryIndex, Unique, InVersionedTable: Boolean);
var
  Table: TBoldSQLTableDescription;
begin
  EnsureTable(TableName, InVersionedTable);
  Table := PSSystemDescription.SQLTablesList.ItemsBySQLName[TableName];
  Table.EnsureIndex(Fields, PrimaryIndex, Unique, false);
end;

procedure TBoldSystemSQLMapper.OpenDatabase(ReadDbTypeFromDB: Boolean; ReadMappingFromDB: Boolean);
begin
  try
    Database.OPEN;
    if not Database.Connected then
      raise EBold.CreateFmt('%s.OpenDatabase: Failed to connect to database', [classname]);

    MappingInfo.ReadDataFromDB(DataBase, ReadDbTypeFromDB, ReadMappingFromDB);

    if assigned(OnPreInitializeBoldDbType) then
      OnPreInitializeBoldDbType(self);    // used for DBStructureValidator

    InitializeBoldDbType;
    InitializeTopSortedIndexForBoldDbType;
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
      result := 'X_FILES'
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
{$IFDEF RIL}
var
  SB: TStringBuilder;
  c, m: integer;
  Column: TBoldSQLColumnDescription;
  MemberMapper: TBoldMemberSQLMapper;
begin
  if IncludeKey and (SQLStyle in [ssColumns, ssParameters])  then
  begin
    if assigned(Table) then
      SQLForKey(Table, SQL, SQLStyle, UseAlias)
    else
      SQLForKey(MainTable, SQL, SQLStyle, UseAlias)
  end;

{
  if ScanRF(sMembersSQL,#13#10,0)= SLen-1 then //has ctrlf at end of str
    SLen := SLen-2; //=removes ctrlf
}
  if MemberList.Count = 0 then
    exit;
  SB := TStringBuilder.Create(SQL.Text);
  try
    for m := 0 to MemberList.Count - 1 do
    begin
      MemberMapper := MemberList[m] as TBoldMemberSQLMapper;
      if Assigned(MemberMapper) and
         (not StoredInObjectOnly or MemberMapper.IsStoredInObject) then
      begin
        for c := 0 to MemberMapper.ColumnDescriptions.Count - 1 do
        begin
          Column := MemberMapper.ColumnDescriptions[c] as TBoldSQLColumnDescription;
          if not Assigned(Table) or (Column.TableDescription = table) then
          begin
            if Assigned(Table) then
            begin
              if SB.Length>0 then
                SB.Append(#13#10); {= "append"}
              case SQLStyle of
                ssColumns   : begin
                                SB.Append(Column.SQLName);
                              end;
                ssParameters: begin
                                SB.Append(':');
                                SB.Append(Column.SQLName);
                              end;
                ssValues    : begin
                                SB.Append(Format('%s = :%0:s', [Column.SQLName]));
                              end;
                else
                  raise EBold.Create('unimplememnted');
              end
            end
            else
            begin
              if SB.Length>0 then
                SB.Append(#13#10); {= "append"}
              case SQLStyle of               
                ssColumns: begin
                             // SQL.Append(Format('%s.%s', [TableAlias(Column.TableDescription, useAlias), Column.SQLName]))
                             SB.Append(TableAlias(Column.TableDescription, useAlias));
                             SB.Append('.');
                             SB.Append(Column.SQLName);
                           end;
                else
                  raise EBold.Create('unimplememnted');
              end;
            end;
          end;
        end;
      end;
    end;
  finally
    SB.Replace(#13#10#13#10, #13#10);
    SQL.Text := SB.ToString;
    FreeAndNil(SB);
  end;
end;
{$ELSE}
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
{$ENDIF}

procedure TBoldObjectSQLMapper.RetrieveSelectStatement(s: TStrings; MemberMapperList: TBoldMemberPersistenceMapperList; FetchMode: Integer; ForceRootTable: Boolean);
{$IFDEF RIL}
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
  SB: TStringBuilder;
begin
  SelectList := TStringList.Create;
  FromList := TStringList.Create;
  WhereList := TStringList.Create;
  JoinList :=  TStringList.Create;
  TableList := TBoldSQLtableDescriptionList.Create(SystemPersistenceMapper.PSSystemDescription);
  tableList.OwnsEntries := false;
  SB := TStringBuilder.Create;
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
    //FromList.Append(Format('%s %s', [MainTable.SQLName, TableAlias(MainTable, true)]));
      SB.Clear;
      SB.Append(MainTable.SQLName);
      SB.Append(' ');
      SB.Append(TableAlias(MainTable, true));
    FromList.Append(SB.ToString);

    if (FetchMode = fmNormal) and SystemPersistenceMapper.SQLDataBaseConfig.UseSQL92Joins  then
    begin
      { cheapest check first }
      for T := 0 to TableList.COunt-1 do
        if (TableList[T]<>MainTable) and (TableList[T].ColumnsList.Count>2)  then
        begin
          //Join := format('left join %s %s on ', [TableList[T].SQLName, TableAlias(TableList[T], true)]);
          SB.Clear;
            SB.Append('left join ');
            SB.Append(TableList[T].SQLName);
            SB.Append(' ');
            SB.Append(TableAlias(TableList[T], true));
            SB.Append(' on ');

          JoinList.Clear;
          JoinSQLTableByKey(JoinList, MainTable, TableList[T]);

          //Join := Join + BoldSeparateStringList(JoinList, ' and ', '(', ')');
            SB.Append(BoldSeparateStringList(JoinList, ' and ', '(', ')'));
          Join := SB.ToString;
          FromList.Append(Join);
        end;
    end
    else
    begin
      for T := 0 to TableList.Count - 1 do
        if (TableList[T] <> MainTable) and (TableList[T].ColumnsList.Count > 2) then
        begin
          //FromList.Append(Format('%s %s',[TableList[T].SQLName, TableAlias(TableList[t], true)]));
          SB.Clear;
            SB.Append(TableList[T].SQLName);
            SB.Append(' ');
            SB.Append(TableAlias(TableList[t], true));
          FromList.Append(SB.ToString);
        end;

      for T := 0 to TableList.Count-1 do
      begin
        {ril} // cheapest check first
        if (TableList[T]<>MainTable) and (TableList[T].ColumnsList.Count > 2)  then
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

    {ril} {'SELECT %s ':}
    BoldAppendToStrings(S, 'SELECT '+BoldSeparateStringList(SelectList, ', ', '', '')+' ', true);

    if SystemPersistenceMapper.SQLDataBaseConfig.UseSQL92Joins and (FetchMode = fmNormal) then
      {ril} {'FROM %s ':}
      BoldAppendToStrings(S, 'FROM '+BoldSeparateStringList(FromList, ' ', '', '')+' ', true)
    else
      {ril} {'FROM %s ':}
      BoldAppendToStrings(S, 'FROM '+BoldSeparateStringList(FromList, ', ', '', '')+' ', true);

    {ril} {'WHERE %s ':}
    BoldAppendToStrings(S, 'WHERE '+BoldSeparateStringList(WhereList, ' AND ', '', '')+' ', true);
  finally
    SelectList.Free;
    FromList.Free;
    WhereList.Free;
    JoinList.Free;
    TableList.Free;
    FreeAndNil(SB);
  end;
end;
{$ELSE}
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

    FromList.Append(Format('%s %s', [MainTable.SQLName, TableAlias(MainTable, true)]));
    if SystemPersistenceMapper.SQLDataBaseConfig.UseSQL92Joins and (FetchMode = fmNormal) then
    begin
      for T := 0 to TableList.COunt - 1 do
        if (TableList[T].ColumnsList.Count > 2) and (TableList[T] <> MainTable) then
        begin
          Join := format('left join %s %s on ', [TableList[T].SQLName, TableAlias(TableList[T], true)]);
          JoinList.Clear;
          JoinSQLTableByKey(JoinList, MainTable, TableList[T]);
          Join := Join + BoldSeparateStringList(JoinList, ' and ', '(', ')');
          FromList.Add(Join);
        end;
    end
    else
    begin
      for T := 0 to TableList.Count - 1 do
        if (TableList[T].ColumnsList.Count > 2) and (TableList[T] <> MainTable) then
          FromList.Append(Format('%s %s',[TableList[T].SQLName, TableAlias(TableList[t], true)]));

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

    BoldAppendToStrings(S, Format('SELECT %s ', [BoldSeparateStringList(SelectList, ', ', '', '')]), true);

    if SystemPersistenceMapper.SQLDataBaseConfig.UseSQL92Joins and (FetchMode = fmNormal) then
      BoldAppendToStrings(S, Format('FROM %s ', [BoldSeparateStringList(FromList, ' ', '', '')]), true)
    else
      BoldAppendToStrings(S, Format('FROM %s ', [BoldSeparateStringList(FromList, ', ', '', '')]), true);

    BoldAppendToStrings(S, Format('WHERE %s ', [BoldSeparateStringList(WhereList, ' AND ', '', '')]), true);
  finally
    SelectList.Free;
    FromList.Free;
    WhereList.Free;
    JoinList.Free;
    TableList.Free;
  end;
end;
{$ENDIF}


procedure TBoldObjectSQLMapper.ValuesFromFieldsByMemberList(ObjectID: TBoldObjectId; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; const DataSet: IBoldDataSet; memberList: TBoldMemberPersistenceMapperList);
var
  i,FieldIndex: integer;
  aField    : IBoldField;  
  ObjectContents: IBoldObjectContents;
  MemberMapper: TBoldMemberSQLMapper;
  ColumnIndex: Integer;
begin
  ObjectContents := ValueSpace.EnsuredObjectContentsByObjectId[ObjectID];
  FieldIndex := 2; // skip id and type
  for i:= 0 to memberlist.count - 1 do
  begin
    MemberMapper := TBoldMemberSQLMapper(Memberlist[i]);
    if MemberMapper.ShouldFetch(ObjectContents) then
    for ColumnIndex := 0 to MemberMapper.ColumnCount - 1 do
    begin
      aField := nil;
      if FieldIndex < DataSet.FieldCount then
      begin
        aField := DataSet.Fields[FieldIndex];
        if not SameText(aField.FieldName, MemberMapper.ColumnDescriptions[ColumnIndex].SQLName) then
          aField := nil;
      end;
      if not Assigned(aField) then
      begin
        aField := DataSet.FieldByUpperCaseName(MemberMapper.ColumnDescriptions[ColumnIndex].SQLNameUpper);
        FieldIndex := aField.Field.Index;
      end;
      MemberMapper.ValueFromField(ObjectId, ObjectContents, ValueSpace, TranslationList, aField, ColumnIndex);
      inc(FieldIndex);
    end;
  end;
end;

procedure TBoldObjectSQLMapper.ValuesToParamsByMemberList(ObjectId: TBoldObjectId; const ValueSpace: IBoldValueSpace; const Query: IBoldExecQuery; memberList: TBoldMemberPersistenceMapperList; TranslationList: TBoldIdTranslationList; DBStorageMode: TBoldDBStorageMode);
var
  i: integer;
  ObjectContents: IBoldObjectContents;
begin
  ObjectContents := ValueSpace.EnsuredObjectContentsByObjectId[ObjectID];
  for i := 0 to memberlist.count - 1 do
    TBoldMemberSQLMapper(Memberlist[i]).ValueToQuery(ObjectContents, ValueSpace, Query, translationList, DBStorageMode);
end;

procedure TBoldObjectSQLMapper.ValuesToQueryByMemberList(
  ObjectId: TBoldObjectId; const ValueSpace: IBoldValueSpace; const Query: IBoldExecQuery;
  SQL: TStrings; memberList: TBoldMemberPersistenceMapperList;
  TranslationList: TBoldIdTranslationList; DBStorageMode: TBoldDBStorageMode);
var
  i: integer;
  ColumnIndex: Integer;
  MemberMapper: TBoldMemberSQLMapper;
  ObjectContents: IBoldObjectContents;
  Members: TStringList;
  Value: Variant;
  Param: IBoldParameter;
  ParamName: string;
begin
  if SystemPersistenceMapper.SQLDataBaseConfig.UseParamsForInteger then
    ValuesToParamsByMemberList(ObjectID,ValueSpace,Query,MemberList,TranslationList,DbStorageMode)
  else
  begin
    ObjectContents := ValueSpace.EnsuredObjectContentsByObjectId[ObjectID];
    Members := TStringList.Create;
    for i := 0 to MemberList.count - 1 do
    begin
      MemberMapper := Memberlist[i] as TBoldMemberSQLMapper;
      if (DBStorageMode = dsmCreate) or MemberMapper.IsDirty(ObjectContents) then
      begin
        for ColumnIndex := 0 to MemberMapper.ColumnCount - 1 do
        begin
          Value := MemberMapper.ValueAsVariant(ObjectContents, ColumnIndex, TranslationList);
          if not SystemPersistenceMapper.SQLDataBaseConfig.UseParamsForEmptyString and SameText(MemberMapper.ContentName, BoldContentName_String) and (Value = '') then
            Members.Add(QuotedStr(''))  // handle blank strings
          else // handle nulls
          if VarIsNull(Value) then
            Members.Add(SystemPersistenceMapper.SQLDataBaseConfig.SQLforNull)
          else // handle integers
          if (MemberMapper.ColumnTypeAsSQL[ColumnIndex] = SystemPersistenceMapper.SQLDataBaseConfig.ColumnTypeForInteger) then
            Members.Add(Value)
          else
          begin
            ParamName := 'p'+IntToStr(Query.ParamCount);
            Param := Query.CreateParam(ftUnknown, ParamName);
            MemberMapper.ValueToParam(ObjectContents, Param, ColumnIndex, translationList);
            Members.Add(':'+ParamName);
          end;
          if DBStorageMode = dsmUpdate then
            Members[Members.Count-1] := Format('%s=%s', [MemberMapper.ColumnDescriptions[ColumnIndex].SQLName, Members[Members.Count-1]]);
        end;
      end;
    end;
    if (Members.Count > 0) then
    begin
      if (DBStorageMode = dsmCreate) then
        SQL.Add(','+Members.CommaText)
      else
        SQL.Add(Members.CommaText);
    end;
    Members.free;
  end;
end;

{---TBoldMemberSQLMapper---}
constructor TBoldMemberSQLMapper.CreateFromMold(Moldmember: TMoldMember; MoldClass: TMoldClass; Owner: TBoldObjectPersistenceMapper; const MemberIndex: Integer; TypeNameDictionary: TBoldTypeNameDictionary);
begin
  inherited;
  if not assigned(MoldMember) then
    exit;

  fInitialColumnRootName := BoldExpandName(MoldMember.ColumnName, MoldMember.name, xtSQL, SystemPersistenceMapper.SQLDataBaseConfig.MaxDBIdentifierLength, MoldClass.Model.NationalCharConversion);

  if MoldMember is TMoldAttribute then
    fAllowNull := (MoldMember as TMoldAttribute).AllowNull
  else
    fAllowNull := true;
      
  if MoldMember is TMoldAttribute then
    fDefaultDbValue := (MoldMember as TMoldAttribute).DefaultDBValue;
        
  if fDefaultDbValue = '' then
    fDefaultDbValue := DefaultDefaultDbValue;
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
    Result := Format('%s_%d', [Result, ColumnIndex]);
end;


procedure TBoldSystemSQLMapper.AddCustomIndexes;
var
  I: Integer;
  Table: TBoldSQLTableDescription;
  CustomIndex: TBoldIndexDefintion;
  AnIndex: TBoldSQLIndexDescription;
begin
  for I := 0 to fCustomIndexes.Count - 1 do
  begin
    CustomIndex := fCustomIndexes.IndexDefinition[I];
    Table := PSSystemDescription.SQLTablesList.ItemsBySQLName[CustomIndex.TableName];
    if CustomIndex.Remove then
    begin
      AnIndex := nil;
      if Assigned(Table) then
        AnIndex := Table.IndexList.ItemsByIndexFields[CustomIndex.Columns];
      if Assigned(AnIndex) then
      begin
        Table.IndexList.Remove(AnIndex);
      end
      else 
        raise EBold.Create('Can''t Remove nonexistent index: ' + CustomIndex.TableName + ': [' + CustomIndex.Columns+ ']');
    end
    else
    begin
      if (Table = nil) then
        raise EBold.Create('Can''t create index on nonexistant table ' + CustomIndex.TableName);
      Table.EnsureIndex(CustomIndex.Columns, False, CustomIndex.Unique, false);
    end;
  end;
end;

function TBoldSystemSQLMapper.BoldDbTypeForTopSortedIndex(
  TopSortedIndex: Integer): TBoldDbType;
begin
  result := (ObjectPersistenceMappers[TopSortedIndex] as TBoldObjectSQLMapper).BoldDbType;
end;

function TBoldSystemSQLMapper.TopSortedIndexForBoldDbType(BoldDbType: TBoldDbType): Integer;
begin
  if fMaxDbType = -1 then
    raise Exception.Create('fTopSortedIndexForBoldDbType not initialized');
  if BoldDbType > fMaxDbType then
    Result := -1
  else
    Result := fTopSortedIndexForBoldDbType[BoldDbType];
end;

procedure TBoldMemberSQLMapper.SetParamToNullWithDataType(const aParam: IBoldParameter; FieldType: TFieldType);
begin
  with aParam do
  begin
    Clear;
    if DataType <> FieldType then
      DataType := FieldType;
  end;
end;

procedure TBoldMemberSQLMapper.ValueToParam(const ObjectContent: IBoldObjectContents; const Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList);
begin
  raise EBold.CreateFmt(sIllegalCall, [classname, 'ValueToParam']); // do not localize
end;

function TBoldMemberSQLMapper.ValueAsVariant(const ObjectContent: IBoldObjectContents;
  ColumnIndex: Integer; TranslationList: TBoldIdTranslationList): variant;
begin
  raise EBold.CreateFmt(sIllegalCall, [classname, 'ValueAsVariant']); // do not localize
end;

procedure TBoldMemberSQLMapper.ValueFromField(OwningObjectId: TBoldObjectId; const ObjectContent: IBoldObjectContents; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; const Field: IBoldField; ColumnIndex: Integer);
begin
  raise EBold.CreateFmt(sIllegalCall, [classname, 'ValueFromField']); // do not localize
end;

procedure TBoldMemberSQLMapper.ValueToQuery(const ObjectContent: IBoldObjectContents; const ValueSpace: IBoldValueSpace; const Query: IBoldExecQuery; TranslationList: TBoldIdTranslationList; DBStorageMode: TBoldDBStorageMode);
var
  aParam    : IBoldParameter;
  ColumnIndex: Integer;
begin
  if (DBStorageMode = dsmCreate) or IsDirty(ObjectContent) then
  begin
    for ColumnIndex := 0 to ColumnCount - 1 do
    begin
      if Query.ParamCheck then
        aParam := Query.EnsureParamByName(ColumnDescriptions[ColumnIndex].SQLName)
      else
        aParam := Query.CreateParam(ftUnknown, 'p'+IntToStr(Query.ParamCount));
      if Assigned(aParam) then
        ValueToParam(ObjectContent, aParam, ColumnIndex, translationList)
      else
        raise EBoldInternal.CreateFmt(sSomeColumnsNotInTable, [classname, 'ValueToQuery', ColumnIndex, ColumnDescriptions[ColumnIndex].SQLName]); // do not localize
    end;
  end;
end;

procedure TBoldMemberSQLMapper.ValueFromQuery(OwningObjectId: TBoldObjectId; const ObjectContent: IBoldObjectContents; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; const DataSet: IBoldDataSet);
var
  aField    : IBoldField;
  ColumnIndex: Integer;
begin
  if ShouldFetch(ObjectContent) then
  begin
    for ColumnIndex := 0 to ColumnCount - 1 do
    begin
      aField := DataSet.FieldByUpperCaseName(ColumnDescriptions[ColumnIndex].SQLNameUpper);
      if not Assigned(aField) and (Length(ColumnDescriptions[ColumnIndex].SQLName) = 31) then // Could be DBX problem
        aField := DataSet.FieldByUpperCaseName(Copy(ColumnDescriptions[ColumnIndex].SQLNameUpper,1,30));
      if Assigned(aField) then
        ValueFromField(OwningObjectId, ObjectContent, ValueSpace, TranslationList, aField, ColumnIndex)
      else
        raise EBoldInternal.CreateFmt(sSomeColumnsNotInTable, [classname, 'ValueFromQuery', ColumnIndex, ColumnDescriptions[ColumnIndex].SQLName]); // do not localize
    end;
  end;
end;

procedure TBoldMemberSQLMapper.InitializeSystem(const theDatabase: IBoldDataBase);
begin
end;

function TBoldMemberSQLMapper.GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType;
begin
  raise EBold.CreateFmt(sIllegalColumnIndex, [ClassName, 'GetColumnBDEFieldType', ColumnIndex]); // do not localize
end;

procedure TBoldSystemSQLMapper.CloseDataBase;
begin
  if assigned(fExecQuery) then
    Database.ReleaseExecQuery(fExecQuery);
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
    SQL.Append(Format('%s (%s.%s = %d)', [WhereToken,
      EndTimeTableAlias, TIMESTAMPSTOPCOLUMNNAME, TimeStamp]))
  else
    SQL.Append(Format('%s ((%s.%s <= %d) and (%s.%s >= %d))', [WhereToken,
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
  FreeAndNil(fCustomIndexes);
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

procedure TBoldSystemSQLMapper.GenerateDatabaseScript(Script: TStrings);
var
  i: integer;
begin
  PSSystemDescription.GenerateDatabaseScript(Script);
  for i := 0 to ObjectPersistenceMappers.Count - 1 do
    if assigned(ObjectPersistenceMappers[i]) then
      (ObjectPersistenceMappers[i] as TBoldObjectSQLMapper).GenerateDatabaseScript(script);
  MappingInfo.ScriptForWriteData(Database, Script, False, SQLDatabaseConfig.SqlScriptSeparator, SQLDatabaseConfig.SqlScriptTerminator);
end;

procedure TBoldObjectSQLMapper.GenerateDatabaseScript(Script: TStrings);
var
  i: integer;
begin
  for i := 0 to MemberPersistenceMappers.Count - 1 do
    if assigned(MemberPersistenceMappers[i]) then
      (MemberPersistenceMappers[i] as TBoldMemberSQLMapper).GenerateDatabaseScript(script);
end;

procedure TBoldMemberSQLMapper.GenerateDatabaseScript(Script: TStrings);
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

procedure TBoldSystemSQLMapper.PMFetch(ObjectIDList: TBoldObjectIdList;
  const ValueSpace: IBoldValueSpace; MemberIdList: TBoldMemberIdList;
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

procedure TBoldSystemSQLMapper.InitializeTopSortedIndexForBoldDbType;
var
  i: integer;
  DbType: integer;
begin
  fMaxDbType := -1;
  for i := 0 to ObjectPersistenceMappers.Count - 1 do
    if ObjectPersistenceMappers[i] is TBoldObjectSQLMapper then
    begin
      DbType := TBoldObjectSQLMapper(ObjectPersistenceMappers[i]).BoldDbType;
      if DbType > fMaxDbType then
        fMaxDbType := DbType;
    end;
  if fMaxDbType > 50000 then
    raise Exception.Create(Format('DbType: %d, too big', [fMaxDbType]));
  SetLength(fTopSortedIndexForBoldDbType, fMaxDbType+1);
  for i := 0 to fMaxDbType do
    fTopSortedIndexForBoldDbType[i] := -1;
  for i := 0 to ObjectPersistenceMappers.Count - 1 do
    if (ObjectPersistenceMappers[i] is TBoldObjectSQLMapper) then
    begin
      DbType := TBoldObjectSQLMapper(ObjectPersistenceMappers[i]).BoldDbType;
      if fTopSortedIndexForBoldDbType[DbType] <> -1 then
        raise Exception.Create(Format('Duplicate DbType: %d', [i]));
      fTopSortedIndexForBoldDbType[DbType] := i;
    end;
end;

end.
