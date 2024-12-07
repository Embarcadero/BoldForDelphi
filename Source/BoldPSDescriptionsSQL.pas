
{ Global compiler directives }
{$include bold.inc}
unit BoldPSDescriptionsSQL;

interface

uses
  Db,
  Classes,
  Bolddefs,
  BoldDbInterfaces,
  BoldIndexableList,
  BoldPSParams,
  BoldPSParamsSQL,
  BoldSQLDatabaseConfig,
  BoldPSDescriptions,
  BoldTaggedValueSupport,
  BoldHashIndexes
  ;
const
  IDCOLUMN_TYPE = ftInteger;
  TYPECOLUMN_TYPE = ftSmallint;

type
  //Copied from Data.DB
  TIndexOptionExt = (ixPrimary, ixUnique, ixDescending, ixCaseInsensitive,
    ixExpression, ixNonMaintained, ixNonClustered {ext});
  TIndexOptionsExt = set of TIndexOptionExt;

  TBoldSQLSystemDescription = class;
  TBoldSQLDescriptionElement = class;
  TBoldSQLTableDescription = class;
  TBoldSQLColumnDescription = class;
  TBoldSQLIndexDescription = class;

  TBoldSQLDescriptionList = class;
  TBoldSQLTableDescriptionList = class;
  TBoldSQLIndexDescriptionList = class;

  {---TBoldSQLSystemDescription---}
  TBoldSQLSystemDescription = class(TBoldPSSystemDescription)
  private
    fExecQuery: IBoldExecQuery;
    fTables: TBoldSQLTableDescriptionList;
    fSQLDatabaseConfig: TBoldSQLDatabaseConfig;
    fNationalCharConversion: TBoldNationalCharConversion;
    function ContinueClearPS(TableNameList: TStrings): boolean;
  protected
    procedure InitializeKnownSystemtables(KnownTables: TStrings; PSParams: TBoldPSSQLParams); virtual;
    function EffectiveGenerationMode(PSParams: TBoldPSSQlParams): TBoldDataBaseGenerationMode;
    function EffectiveUseTransactions(PSParams: TBoldPSSQlParams): Boolean;
    procedure StartMetaDataTransaction(PSParams: TBoldPSSQlParams);
    procedure RollBackMetaDataTransaction(PSParams: TBoldPSSQlParams);
    procedure CommitMetaDataTransaction(PSParams: TBoldPSSQlParams);
  public
    constructor Create(aOwner: TBoldPSDescriptionElement; SQLDatabaseConfig: TBoldSQLDataBaseConfig; NationalCharConversion: TBoldNationalCharConversion); virtual;
    destructor Destroy; override;
    procedure CleanPersistentStorage(PSParams: TBoldPSSQLParams);
    procedure CreatePersistentStorage(PSParams: TBoldPSParams); override;
    procedure GenerateDatabaseScript(Script: TStrings); virtual;
    property SQLDatabaseConfig: TBoldSQLDatabaseConfig read fSQLDatabaseConfig;
    property SQLTablesList: TBoldSQLTableDescriptionList read fTables;
    property NationalCharConversion: TBoldNationalCharConversion read fNationalCharConversion;
  end;

  {---TBoldSQLDescriptionElement---}
  TBoldSQLDescriptionElement = class(TBoldPSDescriptionElement)
  private
    fSQLName: string;
    fSQLNameUpper: string;
  protected
    function GetDebugInfo: string; override;
    procedure SetSQLName(const v: string); virtual;
    function OwningSystem: TBoldSQLSystemdescription;
  public
    property SQLName: string read fSQLName write SetSQLName;
    property SQLNameUpper: string read fSQLNameUpper;
    function MappedSQLName(const value: String): String;
  end;

  {---TBoldSQLTableDescription---}
  TBoldSQLTableDescription = class(TBoldSQLDescriptionElement)
  private
    fIndexes: TBoldSQLIndexDescriptionList;
    fColumns: TBoldSQLDescriptionList;
    fPrimaryIndex: TBoldSQLIndexDescription;
    fVersioned: Boolean;
    fContainsStopTimeStamp: Boolean;
    function GetSystemDescription: TBoldSQLSystemDescription;
  protected
    procedure SetSQLName(const v: string); override;
    function TableExists(PSParams: TBoldPSSQLParams): boolean;
    procedure CreateTableBDE(PSParams: TBoldPSSQLParams);
    procedure CreateTableSQL(PSParams: TBoldPSSQLParams);
  public
    constructor Create(aOwner: TBoldPSDescriptionElement; Versioned: Boolean);
    destructor Destroy; override;
    function AddColumn(const ColName: string; SQLColType, AllowNullAsSQL: String; ColType: TFieldType; ColSize: Integer; AllowNull: Boolean; const DefaultDBValue: string): TBoldSQLColumnDescription;
    procedure EnsureIndex(const Fields: string; Primary, Unique, NonClustered: boolean);
    procedure CreateTable(PSParams: TBoldPSSQLParams);
    procedure DeleteTable(PSParams: TBoldPSSQLParams);
    procedure RetrieveSelectIdAndTypeStatement(S: TStrings);
    procedure GenerateDatabaseScript(Script: TStrings);
    function SQLForCreateTable(const DataBase: IBoldDatabase): string;
    function SQLForDropTable: string;
    property ColumnsList: TBoldSQLDescriptionList read fColumns;
    property IndexList: TBoldSQLIndexDescriptionList read fIndexes;
    property PrimaryIndex: TBoldSQLIndexDescription read fPrimaryINdex write fPrimaryIndex;
    property SystemDescription: TBoldSQLSystemDescription read GetSystemDescription;
    property Versioned: Boolean read fVersioned;
    property ContainsStopTimeStamp: Boolean read fContainsStopTimeStamp write fContainsStopTimeStamp;
  end;

  {---TBoldSQLColumnDescription---}
  TBoldSQLColumnDescription = class(TBoldSQLDescriptionElement)
  private
    fMandatory: boolean;
    fSize: integer;
    fSQLtype: string;
    fSQLAllowNull: string;
    fFieldType: TFieldType;
    fDefaultDBValue: String;
    function GetTableDescription: TBoldSQLTableDescription;
  protected
    function GetDebugInfo: string; override;
    procedure SetSQLName(const v: string); override;
  public
    procedure CreateBDEColumn(FieldDefs: TFieldDefs);
    constructor Create(aOwner: TBoldPSDescriptionElement);
    function GetSQLForColumn(const DataBase: IBoldDatabase): string;
    property TableDescription: TBoldSQLTableDescription read GetTableDescription;
    property Size: integer read fSize write fSize;
    property Mandatory: Boolean read fMandatory write fMandatory;
    property SQLType: string read fSQLtype write fSQLType;
    property SQLAllowNull: string read fSQLAllowNull write fSQLAllowNull;
    property FieldType: TFieldType read fFieldType write fFieldType;
    property DefaultDBValue: string read fDefaultDbValue write fDefaultDBValue;
  end;

  {---TBoldSQLIndexDescription---}
  TBoldSQLIndexDescription = class(TBoldPSDescriptionElement)
  private
    fIndexOptions: TIndexOptionsExt;
    fIndexedFields: string;
    function GetTableDescription: TBoldSQLTableDescription;
    function GetIndexedFieldsForSQL: String;
  public
    property TableDescription: TBoldSQLTableDescription read GetTableDescription;
    constructor Create(aOwner: TBoldPSDescriptionElement; const Fields: String);
    class function NormalizeFields(const IndexedFields: string): string;
    function SQLForPrimaryKey: string;
    function SQLForSecondaryKey: string;
    function GeneratedName: String;
    property IndexedFields: string read fIndexedFields write fIndexedFields;
    property IndexedFieldsForSQL: String read GetIndexedFieldsForSQL;
    procedure CreateBDEIndex(PSParams: TBoldPSSQLParams; IndexDefs: TIndexDefs);
    property IndexOptions: TIndexOptionsExt read fIndexOptions write fIndexOptions;
  end;

  TBoldSQLDescriptionListTraverser = class(TBoldIndexableListTraverser)
  private
    function GetItem: TBoldSQLDescriptionElement;
  public
    property Item: TBoldSQLDescriptionElement read GetItem;
    property Current: TBoldSQLDescriptionElement read GetItem;
  end;

  {---TBoldSQLDescriptionList---}
  TBoldSQLDescriptionList = class(TBoldIndexableList)
  private
    fSystemDescription: TBoldSQLSystemDescription;
    function GetItem(index: Integer): TBoldSQLDescriptionElement;
    function GetItemBySQLName(const SQLName: string): TBoldSQLDescriptionElement;
  protected
    function TraverserClass: TBoldIndexableListTraverserClass; override;
  public
    constructor Create(SystemDescription: TBoldSQLSystemDescription);
    procedure ToStrings(S: TStrings);
    function ToString: string; override;
    function CreateTraverser: TBoldSQLDescriptionListTraverser;
    function GetEnumerator: TBoldSQLDescriptionListTraverser;
    property Items[index: Integer]: TBoldSQLDescriptionElement read GetItem; default;
    property ItemsBySQLName[const SQLName: string]: TBoldSQLDescriptionElement read GetItemBySQLName;
  end;

  TBoldSQLTableDescriptionListTraverser = class(TBoldSQLDescriptionListTraverser)
  private
    function GetItem: TBoldSQLTableDescription;
  public
    property Item: TBoldSQLTableDescription read GetItem;
    property Current: TBoldSQLTableDescription read GetItem;
  end;

  {---TBoldSQLTableDescriptionList---}
  TBoldSQLTableDescriptionList = class(TBoldSQLDescriptionList)
  private
    function GetItem(index: Integer): TBoldSQLTableDescription;
    function GetItemBySQLName(const SQLName: string): TBoldSQLTableDescription;
  protected
    function TraverserClass: TBoldIndexableListTraverserClass; override;
  public
    function CreateTraverser: TBoldSQLTableDescriptionListTraverser;
    function GetEnumerator: TBoldSQLTableDescriptionListTraverser;
    property Items[index: Integer]: TBoldSQLTableDescription read GetItem; default;
    property ItemsBySQLName[const SQLName: string]: TBoldSQLtableDescription read GetItemBySQLName;
  end;

  TBoldSQLIndexDescriptionList = class(TBoldIndexableList)
  private
    class var IX_SQLIndexFields: integer;
    function GetItem(index: integer): TBoldSQLIndexDescription;
    function GetItemsByIndexFields(const IndexFields: string): TBoldSQLIndexDescription;
  public
    constructor Create;
    property items[index: integer]: TBoldSQLIndexDescription read GetItem; default;
    property ItemsByIndexFields[const IndexFields: string]: TBoldSQLIndexDescription read GetItemsByIndexFields;
  end;


var
  BoldCleanDatabaseForced: Boolean = false;

implementation

uses
  Controls,
  Dialogs,
  SysUtils,
  TypInfo,
  UITypes,

  BoldCoreConsts,
  BoldGuard,
  BoldNameExpander,
  BoldLogHandler,
  BoldQueryUserDlg,
  BoldIndex;

var
  IX_SQLDescriptionSQLName: integer = -1;

type
  {---TSQLDescriptorSQLNameIndex---}
  TSQLDescriptorSQLNameIndex = class(TBoldStringHashIndex)
  protected
    function ItemAsKeyString(Item: TObject): string; override;
  end;

  TSQLIndexFieldsIndex = class(TBoldStringHashIndex)
  protected
    function ItemAsKeyString(Item: TObject): string; override;
  end;

{---TSQLDescriptorSQLNameIndex---}
function TSQLDescriptorSQLNameIndex.ItemAsKeyString(Item: TObject): string;
begin
  Result := TBoldSQLDescriptionElement(Item).SQLName;
end;

function TBoldSQLIndexDescriptionList.GetItem(index: integer): TBoldSQLIndexDescription;
begin
  result := (inherited items[index]) as TBoldSQLIndexDescription;
end;

function TBoldSQLIndexDescriptionList.GetItemsByIndexFields(const IndexFields: string): TBoldSQLIndexDescription;
begin
  Result := TBoldSQLIndexDescription(TBoldStringHashIndex(Indexes[IX_SQLIndexFields]).FindByString(TBoldSQLIndexDescription.NormalizeFields(IndexFields)));
end;

{---TBoldSQLSystemDescription---}
constructor TBoldSQLSystemDescription.Create(aOwner: TBoldPSDescriptionElement; SQLDatabaseConfig: TBoldSQLDataBaseConfig; NationalCharConversion: TBoldNationalCharConversion);
begin
  inherited Create(aOwner);
  fTables := TBoldSQLTableDescriptionList.Create(self);
  fSQLDatabaseConfig := SQLDatabaseConfig;
  fNationalCharConversion := NationalCharConversion;
end;

destructor TBoldSQLSystemDescription.Destroy;
begin
  FreeAndNil(fTables);
  inherited;
end;

procedure TBoldSQLSystemDescription.InitializeKnownSystemtables(
  KnownTables: TStrings; PSParams: TBoldPSSQLParams);
begin
end;


procedure TBoldSQLSystemDescription.CreatePersistentStorage(PSParams: TBoldPSParams);
var
  PsParamsSQL: TBoldPSSQLParams;

  procedure CreateTables;
  var
    i: integer;
  begin
    fExecQuery := PSParamsSQL.Database.GetExecQuery;
    fExecQuery.ParamCheck := false;
    fExecQuery.StartSQLBatch;
    try
      if BoldLog.ProcessInterruption then
        exit;
      BoldLog.LogHeader := sCreatingTables;
      BoldLog.ProgressMax := SQLTablesList.Count-1;
      for i := 0 to SQLTablesList.Count - 1 do
      begin
        BoldLog.Progress := i;
        SQLTablesList[i].CreateTable(PsParamsSQL);
        BoldLog.Sync;
        if BoldLog.ProcessInterruption then
          exit;
      end;
      fExecQuery.EndSQLBatch;
      CommitMetaDataTransaction(PSParamsSQL);
    except
      RollBackMetaDataTransaction(PSParamsSQL);
      fExecQuery.FailSQLBatch;
      raise;
    end;
  end;

begin
  if not (PSParams is TBoldPSSQLParams) then
    raise EBold.CreateFmt(sUnknownPSParamsType, [classname, PSParams.Classname]);
  PSParamsSQL := PSParams as TBoldPSSQLParams;
  CleanPersistentStorage(PSParamsSQL);
  StartMetaDataTransaction(PSParamsSQL);
  try
    CreateTables;
  finally
    PSParamsSQL.Database.ReleaseExecQuery(fExecQuery);
  end;
end;

function TBoldSQLSystemDescription.ContinueClearPS(TableNameList: TStrings): boolean;
begin
  Result := True;
  if TableNameList.IndexOf(BoldExpandPrefix(IDTABLE_NAME, '', SQLDatabaseConfig.SystemTablePrefix, SQLDatabaseConfig.MaxDBIdentifierLength, NationalCharConversion)) > -1 then
    Result :=
      BoldCleanDatabaseForced or
      (MessageDlg(sContinueDeleteBoldTables, mtWarning, [mbYes, mbNo], 0) = mrYes);
end;

procedure TBoldSQLSystemDescription.CleanPersistentStorage(PSParams: TBoldPSSQLParams);
var
  i: Integer;
  Query: TBoldQueryResult;
  Knowntables: TStringList;
  TableNameList: TStringList;
  Guard: IBoldGuard;

  function DeleteTableBDE: boolean;
  var
  Table: IBoldtable;
  begin
  try
    Table := PSParams.DataBase.GetTable;
    try
      Table.TableName := TableNameList[i];
      Table.Exclusive := True;
      Table.FieldDefs.Update;
      Table.DeleteTable;
      Result := True;
    finally
      PSParams.Database.ReleaseTable(table)
    end;
  except
    Result := False;
  end;
  end;

  function DeleteTableSQL: boolean;
  var
  PsParamsSQL: TBoldPSSQLParams;
  Query: IBoldExecQuery;
  begin
  result := true;
  PSParamsSQL := PSParams as TBoldPSSQLParams;
  Query := PSParamsSQL.Database.GetExecQuery;
  try
    Query.AssignSQLText('DROP TABLE '+Tablenamelist[i]);
    Query.ExecSQL;
  finally
    PSParamsSQL.Database.ReleaseExecQuery(Query);
  end;
  end;

  function DeleteTable: Boolean;
  var
  MayDelete,
  IsBoldTable: boolean;

  begin
  Result:=False;
  IsBoldTable := Knowntables.IndexOf(TableNameList[i]) <> -1 ;

  if not IsBoldTable and (not PSParams.IgnoreUnknownTables) and
    not (Query in [qrYesAll, qrNoAll]) then
      Query := QueryUser(sDeleteTable, Format(sDeleteNonBoldTable, [TableNameList[i]]));

  MayDelete := (Query in [qrYes, qrYesAll]) and (not PSParams.IgnoreUnknownTables);

  if IsBoldTable or MayDelete then
    case EffectiveGenerationMode(PSParams) of
      dbgTable: result := DeleteTableBDE;
      dbgQuery: Result := DeleteTableSQL;
      else result := false;
    end;
  end;

begin
  Guard := TBoldGuard.Create(Knowntables, TableNameList);
  BoldLog.LogHeader := sCleaningPS;
  Query := qrYes;

  TableNameList := TStringList.Create;
  Knowntables := TStringList.Create;


  InitializeKnownSystemtables(KnownTables, PSParams);
  PSParams.Database.ReleaseCachedObjects;
  StartMetaDataTransaction(PSParams);
  try
    PSParams.Database.AllTableNames('*', False, TableNameList);

    if not ContinueClearPS(TableNameList) then
      raise EBold.Create(sCleaningPSAborted);

    BoldLog.ProgressMax := TableNameList.Count-1;
    for i := 0 to TableNameList.Count - 1 do
    begin
      BoldLog.Progress := i;
      try
        if DeleteTable then
        begin
          BoldLog.LogFmt(sDeletingTableX, [TableNameList[i]]);
        end
        else
          BoldLog.LogFmt(sKeepingTableX, [TableNameList[i]]);
      except
        on e:Exception do
          BoldLog.LogFmt(sErrorDeletingTable, [TableNameList[i], E.Message], ltError);
      end;
      BoldLog.Sync;
      if BoldLog.ProcessInterruption then
        exit;
    end;
    CommitMetaDataTransaction(PSParams);
  except
    RollBackMetaDataTransaction(PSParams);
    raise;
  end;
  BoldLog.Separator;
end;

{---TBoldSQLDescriptionElement---}
function TBoldSQLDescriptionElement.GetDebugInfo: string;
begin
  result := ClassName + ':' + SqlName;
end;

function TBoldSQLDescriptionElement.MappedSQLName(const value: String): String;
var
  System: TBoldSQLSystemDescription;
begin
  System := OwningSystem;
  if assigned(System) then
    result := BoldExpandPrefix(value, '', System.SQLDatabaseConfig.SystemTablePrefix, System.SQLDatabaseConfig.MaxDBIdentifierLength, System.NationalCharConversion)
  else
    result := value;
end;

function TBoldSQLDescriptionElement.OwningSystem: TBoldSQLSystemdescription;
begin
  if owner is TBoldSQLSystemDescription then
    result := Owner as TBoldSQLSystemDescription
  else if owner is TBoldSQLDescriptionElement then
    result := (Owner as TBoldSQLDescriptionElement).OwningSystem
  else
    result := nil;
end;

procedure TBoldSQLDescriptionElement.SetSQLName(const v: string);
begin
  fSQLName := MappedSQlName(v);
  fSQLNameUpper := AnsiUpperCase(v);
end;

{---TBoldSQLTableDescription---}
constructor TBoldSQLTableDescription.Create(aOwner: TBoldPSDescriptionElement; Versioned: Boolean);
begin
  inherited Create(AOwner);
  SystemDescription.SQLTablesList.Add(Self);
  fIndexes := TBoldSQLIndexDescriptionList.Create;
  fColumns := TBoldSQLDescriptionList.Create(SystemDescription);
  fVersioned := Versioned;
end;

destructor TBoldSQLTableDescription.Destroy;
begin
  FreeAndNil(fIndexes);
  FreeAndNil(fColumns);
  inherited;
end;

function TBoldSQLTableDescription.SQLForCreateTable(const DataBase: IBoldDatabase): string;
var
  i: integer;
  s: string;
begin
  Result := Format('CREATE TABLE %s (', [SQLName]);
  for i := 0 to ColumnsList.Count - 1 do
  begin
    if i > 0 then
      Result := Result + ', ' + BOLDCRLF;
    s := (ColumnsList[i] as TBoldSQLColumnDescription).GetSQLForColumn(DataBase);
    Result := Result + '  ' + s;
    BoldLog.Log(Format(sAddingColumn, [s]), ltDetail);
  end;
  if SystemDescription.SQLDatabaseConfig.SupportsConstraintsInCreateTable and Assigned(PrimaryIndex) then
  begin
    Result := Result + ', ' + BOLDCRLF + '  ' + PrimaryIndex.SQLForPrimaryKey;
    BoldLog.Log(Format(sAddingPrimaryIndex, [PrimaryIndex.SQLForPrimaryKey]), ltDetail);
  end;
  Result := Result + ')';
end;

function TBoldSQLTableDescription.SQLForDropTable: string;
begin
  result := format('DROP TABLE %s', [SQLName]);
end;


procedure TBoldSQLTableDescription.SetSQLName(const v: string);
begin
  if SQLName <> MappedSQLName(v) then
  begin
    inherited SetSQLName(v);
    SystemDescription.SQLTablesList.ItemChanged(self);
  end;
end;

function TBoldSQLTableDescription.GetSystemDescription: TBoldSQLSystemDescription;
begin
  Result := Owner as TBoldSQLSystemDescription;
end;


procedure TBoldSQLTableDescription.CreateTableBDE(PSParams: TBoldPSSQLParams);
const
  MAXIMUMINDEXCOUNT = 64;
var
  MaxIndex,
  i: integer;
  Table: IBoldtable;
begin
  Table := PSParams.Database.GetTable;
  with table do
  try
    TableName := SQLName;
    Exclusive := True;
    FieldDefs.Clear;
    for i := 0 to ColumnsList.Count - 1 do
      (ColumnsList[i] as TBoldSQLColumnDescription).CreateBDEColumn(FieldDefs);
    if IndexList.Count > MAXIMUMINDEXCOUNT then
      MaxIndex := MAXIMUMINDEXCOUNT
    else
      MaxIndex := IndexList.Count-1;

    IndexDefs.Clear;
    for i := 0 to MaxIndex do
      (IndexList[i] as TBoldSQLIndexDescription).CreateBDEIndex(PSParams, IndexDefs);
    CreateTable;
  finally
    PSParams.Database.ReleaseTable(Table);
  end;
end;

procedure TBoldSQLTableDescription.CreateTableSQL(PSParams: TBoldPSSQLParams);
var
  i: integer;
  Query: IBoldExecQuery;
begin
  Query := SystemDescription.fExecQuery;
  Query.AssignSQLText(SQLForCreateTable(PSParams.Database));
  Query.ExecSQL;
  for i := 0 to IndexList.count-1 do
    if (not SystemDescription.SQLDatabaseConfig.SupportsConstraintsInCreateTable) or
      (IndexList[i] <> PrimaryIndex) then
    begin
      Query.AssignSQLText((IndexList[i] as TBoldSQLIndexDescription).SQLForSecondaryKey);
      Query.ExecSQL;
    end;
end;


procedure TBoldSQLTableDescription.CreateTable(PSParams: TBoldPSSQLParams);
begin
  BoldLog.LogFmtIndent(sCreatingTableX, [SQLName]);
  case SystemDescription.EffectiveGenerationMode(PSParams) of
    dbgTable: CreateTableBDE(PSParams);
    dbgQuery: CreateTableSQL(PSParams);
  end;
  BoldLog.Dedent;
end;

function TBoldSQLTableDescription.TableExists(PSParams: TBoldPSSQLParams): boolean;
var
  List: TStringList;
begin
  List := TStringList.Create;
  try
    PSParams.Database.AllTableNames(SQLName, False, List);
    Result := List.Count = 1;
  finally
    List.Free;
  end;
end;

procedure TBoldSQLTableDescription.DeleteTable(PSParams: TBoldPSSQLParams);
var
  Table: IBoldTable;
begin
  BoldLog.LogFmt(sLocatingTableX, [SQLName]);
  if TableExists(PSParams) then
  begin
    BoldLog.LogFmt(sDeletingTableX, [SQLName]);
    Table := PSParams.Database.GetTable;
    with table do
    try
      TableName := SQLName;
      Exclusive := True;

      DeleteTable;
    finally
      PSParams.Database.ReleaseTable(table);
    end;
  end
  else
    BoldLog.LogFmt(sTableXNotPresent, [SQLName]);
end;

function TBoldSQLTableDescription.AddColumn(const ColName: string; SQLColType, AllowNullAsSQL: String; ColType: TFieldType; ColSize: Integer; AllowNull: Boolean; const DefaultDBValue: String): TBoldSQLColumnDescription;
begin
  Result := TBoldSQLColumnDescription.Create(Self);
  Result.SQLName := ColName;
  Result.SQLType := SQLColType;
  Result.FieldType := ColType;
  Result.Size := ColSize;
  Result.Mandatory := not AllowNull;
  Result.SQLAllowNull := AllowNullAsSQL;
  Result.DefaultDBValue := DefaultDBValue;
end;

procedure TBoldSQLTableDescription.EnsureIndex(const Fields: string; Primary, Unique, NonClustered: boolean);
var
  BoldSQLIndexDescription: TBoldSQLIndexDescription;
begin
  BoldSQLIndexDescription := IndexList.ItemsByIndexFields[Fields];
  if not assigned(BoldSQLIndexDescription) then
    BoldSQLIndexDescription := TBoldSQLIndexDescription.Create(Self, Fields);

  if Primary then
  begin
    BoldSQLIndexDescription.IndexOptions := BoldSQLIndexDescription.IndexOptions + [ixPrimary];
    PrimaryIndex := BoldSQLIndexDescription;
  end;
  if Unique then
    BoldSQLIndexDescription.IndexOptions := BoldSQLIndexDescription.IndexOptions + [ixUnique];
  if NonClustered then
    BoldSQLIndexDescription.IndexOptions := BoldSQLIndexDescription.IndexOptions + [ixNonClustered];
end;

procedure TBoldSQLTableDescription.RetrieveSelectIdAndTypeStatement(S: TStrings);
begin
  with S do
  begin
    Append(Format('SELECT %s.%s, %s.%s',[SQLName, IDCOLUMN_NAME, SQLName, TYPECOLUMN_NAME]));
    Append(Format('FROM %s',[SQLName]));
  end;
end;

{---TBoldSQLColumnDescription---}

constructor TBoldSQLColumnDescription.Create(aOwner: TBoldPSDescriptionElement);
begin
  inherited;
  TableDescription.ColumnsList.Add(Self);
end;

procedure TBoldSQLColumnDescription.CreateBDEColumn(FieldDefs: TFieldDefs);
var
  FieldDef: TFieldDef;
begin
  BoldLog.LogFmt(sAddingColumnInfo,
                 [SQLName,
                  GetEnumName(TypeInfo(TFieldType), Ord(FieldType)), Size], ltDetail);
  FieldDef := FieldDefs.AddFieldDef;
  FieldDef.Required := Mandatory;
  fieldDef.Name := SQLName;
  FieldDef.DataType := FieldType;
  FieldDef.Size := Size;
  if DefaultDBValue <> '' then
  begin
    BoldLog.Separator;
    BoldLog.LogFmt(sColumnHasDefaultDBValue, [SQLName, DefaultDBValue], ltWarning);
    BoldLog.Log(sUnsupportedInTableCreationMode);
    BoldLog.Separator;
  end;
end;

function TBoldSQLColumnDescription.GetTableDescription: TBoldSQLTableDescription;
begin
  Result := Owner as TBoldSQLTableDescription;
end;

procedure TBoldSQLColumnDescription.SetSQLName(const v: string);
begin
  if SQLName <> v then
  begin
    inherited SetSQLName(v);
    TableDescription.ColumnsList.ItemChanged(self);
  end;
end;

function TBoldSQLColumnDescription.GetDebugInfo: string;
begin
  result := TableDescription.SqlName + '.' + SqlName;
end;

function TBoldSQLColumnDescription.GetSQLForColumn(const DataBase: IBoldDatabase): string;
var
  DefaultValue: String;
begin
  if (not assigned(DataBase) or Database.SupportsDefaultColumnValues) and
    (DefaultDbValue <> '') then
    DefaultValue := 'DEFAULT '+DefaultDbValue
  else
    DefaultValue := '';
  Result := Format('%s %s %s %s', [SQLName, SQLType, DefaultValue, SQLAllowNull]);
end;

{---TBoldSQLIndexDescription---}

constructor TBoldSQLIndexDescription.Create(aOwner: TBoldPSDescriptionElement; const Fields: String);
begin
  inherited Create(aOwner);
  fIndexedFields := Fields;
  TableDescription.IndexList.Add(Self);
  IndexOptions := [];
end;

function TBoldSQLIndexDescription.SQLForPrimaryKey: string;
begin
  Result := Format('CONSTRAINT %s PRIMARY KEY (%s)', [generatedName, IndexedFieldsForSQL]);
end;

function TBoldSQLIndexDescription.SQLForSecondaryKey: string;
var
  sType: string;
begin
  if ixUnique in IndexOptions then
    sType := 'UNIQUE' // do not localize
  else if ixNonClustered in IndexOptions then
    sType := 'NONCLUSTERED' // do not localize
  else
    sType := '';
  Result := Format('CREATE %s INDEX %s ON %s (%s)', [sType, GeneratedName, TableDescription.SQLName, IndexedFieldsForSQL]); // do not localize
end;


function TBoldSQLIndexDescription.GetTableDescription: TBoldSQLTableDescription;
begin
  Result := Owner as TBoldSQLTableDescription;
end;

class function TBoldSQLIndexDescription.NormalizeFields(
  const IndexedFields: string): string;
var
  sl: TStringList;
begin
  result := StringReplace(IndexedFields, ';', ',', [rfReplaceAll]);
  result := StringReplace(result, ' ', '', [rfReplaceAll]);
  if pos(',', result) > 1 then
  begin
    sl := TStringList.Create;
    try
      sl.CommaText := result;
      sl.Sorted := true;
      Result := sl.CommaText;
    finally
      sl.free;
    end;
  end;
  result := StringReplace(Result, ',', ';', [rfReplaceAll]);
end;

procedure TBoldSQLIndexDescription.CreateBDEIndex(PSParams: TBoldPSSQLParams; IndexDefs: TIndexDefs);
var
  ActualOptions: TIndexOptions;
  ActualName: string;
  SQLName: string;
begin
  SQLName := GeneratedName;
  BoldLog.LogFmt(sAddingIndex, [SQLName, IndexedFields], ltDetail);
  //Conversion of TIndexOptionsEx to TIndexOptions
  if ixPrimary in IndexOptions then begin
    ActualOptions := ActualOptions + [db.ixPrimary];
  end;
  if ixUnique in IndexOptions then begin
    ActualOptions := ActualOptions + [db.ixUnique];
  end;
  if ixDescending in IndexOptions then begin
    ActualOptions := ActualOptions + [db.ixDescending];
  end;
  if ixCaseInsensitive in IndexOptions then begin
    ActualOptions := ActualOptions + [db.ixCaseInsensitive];
  end;
  if ixExpression in IndexOptions then begin
    ActualOptions := ActualOptions + [db.ixExpression];
  end;
  if ixNonMaintained in IndexOptions then begin
    ActualOptions := ActualOptions + [db.ixNonMaintained];
  end;
  ActualName := SQLName;
  if not PSParams.Database.IsSQLBased then
  begin
    if ActualOptions = [] then
      ActualOptions := [db.ixCaseInsensitive];

    if db.ixPrimary in ActualOptions then
      ActualName := '';
  end;
  IndexDefs.Add(ActualName, IndexedFields, ActualOptions);
end;


{---TBoldSQLDescriptionList---}
constructor TBoldSQLDescriptionList.Create(SystemDescription: TBoldSQLSystemDescription);
begin
  inherited Create;
  SetIndexCapacity(1);
  SetIndexVariable(IX_SQLDescriptionSQLName, AddIndex(TSQLDescriptorSQLNameIndex.Create));
  fSystemDescription := SystemDescription;
end;

function TBoldSQLDescriptionList.CreateTraverser: TBoldSQLDescriptionListTraverser;
begin
  result := inherited CreateTraverser as TBoldSQLDescriptionListTraverser;
end;

function TBoldSQLDescriptionList.GetEnumerator: TBoldSQLDescriptionListTraverser;
begin
  result := CreateTraverser;
end;

function TBoldSQLDescriptionList.GetItem(index: Integer): TBoldSQLDescriptionElement;
begin
  Result := TBoldSQLDescriptionElement(inherited Items[index]);
end;

function TBoldSQLDescriptionList.GetItemBySQLName(const SQLName: string): TBoldSQLDescriptionElement;
begin
  Result := TBoldSQLDescriptionElement(TSQLDescriptorSQLNameIndex(Indexes[IX_SQLDescriptionSQLName]).FindByString(
    BoldExpandName(SQLName, '', xtSQL,
      fSystemDescription.SQlDataBaseConfig.MaxDBIdentifierLength,
      fSystemDescription.NationalCharConversion)));
end;

function TBoldSQLDescriptionList.ToString: string;
var
  I: Integer;
  S: TStrings;
begin
  result := '';
  S := TStringList.Create;
  try
    for I := 0 to Count - 1 do
      S.Add(Items[I].SQLName);
    result := s.CommaText;
  finally
    s.Free;
  end;
end;

procedure TBoldSQLDescriptionList.ToStrings(S: TStrings);
var
  I: Integer;
begin
  S.BeginUpdate;
  S.Clear;
  for I := 0 to Count - 1 do
    S.AddObject(Items[I].SQLName, Items[I]);
  S.EndUpdate;
end;

function TBoldSQLDescriptionList.TraverserClass: TBoldIndexableListTraverserClass;
begin
  result := TBoldSQLDescriptionListTraverser;
end;

function TBoldSQLTableDescriptionList.CreateTraverser: TBoldSQLTableDescriptionListTraverser;
begin
  result := inherited CreateTraverser as TBoldSQLTableDescriptionListTraverser;
end;

function TBoldSQLTableDescriptionList.GetEnumerator: TBoldSQLTableDescriptionListTraverser;
begin
  result := CreateTraverser;
end;

function TBoldSQLTableDescriptionList.GetItem(index: Integer): TBoldSQLTableDescription;
begin
  Result := TBoldSQLTableDescription(inherited Items[index]);
end;

function TBoldSQLTableDescriptionList.GetItemBySQLName(const SQLName: string): TBoldSQLTableDescription;
begin
  Result := TBoldSQLTableDescription(TSQLDescriptorSQLNameIndex(Indexes[IX_SQLDescriptionSQLName]).FindByString(
    BoldExpandName(SQLName, '', xtSQL,
      fSystemDescription.SQLDatabaseConfig.MaxDBIdentifierLength,
      fSystemDescription.NationalCharConversion)));
end;

function TBoldSQLTableDescriptionList.TraverserClass: TBoldIndexableListTraverserClass;
begin
  result := TBoldSQLTableDescriptionListTraverser;
end;

function TBoldSQLIndexDescription.GetIndexedFieldsForSQL: String;
begin
  result := StringReplace(IndexedFields, ';', ',', [rfReplaceAll]);
end;

function TBoldSQLIndexDescription.GeneratedName: String;
var
  IndexNameLength: integer;
begin
  IndexNameLength := TableDescription.SystemDescription.SQLDatabaseConfig.MaxDBIdentifierLength;
  if IndexNameLength = -1 then
    IndexNameLength := TableDescription.SystemDescription.SQLDatabaseConfig.MaxIndexNameLength;
  result := BoldExpandName('IX_'+TableDescription.SQLName+'_'+StringReplace(IndexedFields, ';', '_', [rfReplaceAll]),
    '',
    xtSQL,
    IndexNameLength,
    TableDescription.SystemDescription.NationalCharConversion);
end;

{ TBoldSQLIndexDescriptionList }

constructor TBoldSQLIndexDescriptionList.Create;
begin
  inherited Create;
  SetIndexCapacity(1);
  IX_SQLIndexFields := -1;
  SetIndexVariable(IX_SQLIndexFields, AddIndex(TSQLIndexFieldsIndex.Create));
end;

{ TSQLIndexFieldsIndex }

function TSQLIndexFieldsIndex.ItemAsKeyString(Item: TObject): string;
begin
  Result := TBoldSQLIndexDescription.NormalizeFields(TBoldSQLIndexDescription(Item).IndexedFields);
end;

procedure TBoldSQLSystemDescription.GenerateDatabaseScript(Script: TStrings);
var
  i: integer;
begin
  for i := 0 to SQLTablesList.Count - 1 do
    SQLTablesList[i].GenerateDatabaseScript(Script);
end;

procedure TBoldSQLTableDescription.GenerateDatabaseScript(Script: TStrings);
var
  SQL: TStringList;
  Guard: IBoldGuard;
  i: integer;
begin
  Guard := TBoldGuard.create(SQL);
  SQL := TStringList.Create;
  sql.Text := SQLForCreateTable(nil) + SystemDescription.SQLDatabaseConfig.SqlScriptTerminator;
  Script.AddStrings(Sql);
  if SystemDescription.SQLDatabaseConfig.SqlScriptSeparator<>'' then
    Script.Add(SystemDescription.SQLDatabaseConfig.SqlScriptSeparator);

  for i := 0 to IndexList.count-1 do
    if IndexList[i] <> PrimaryIndex then
    begin
      sql.Text := (IndexList[i] as TBoldSQLIndexDescription).SQLForSecondaryKey + SystemDescription.SQLDatabaseConfig.SqlScriptTerminator;
      Script.AddStrings(Sql);
      if SystemDescription.SQLDatabaseConfig.SqlScriptSeparator<>'' then
        Script.Add(SystemDescription.SQLDatabaseConfig.SqlScriptSeparator);
    end;
end;

function TBoldSQLSystemDescription.EffectiveGenerationMode(PSParams: TBoldPSSQlParams): TBoldDataBaseGenerationMode;
begin
  result := SQLDatabaseConfig.DBGenerationMode;
  if not PSParams.DataBase.SupportsTableCreation then
    result := dbgQuery;
end;

function TBoldSQLSystemDescription.EffectiveUseTransactions(PSParams: TBoldPSSQlParams): Boolean;
begin
  result := SQLDatabaseConfig.AllowMetadataChangesInTransaction;
end;

procedure TBoldSQLSystemDescription.CommitMetaDataTransaction(PSParams: TBoldPSSQlParams);
begin
  if PsParams.Database.InTransaction then
  begin
    BoldLog.Log(sCommittingMetaData);
    PsParams.Database.Commit;
  end;
end;

procedure TBoldSQLSystemDescription.RollBackMetaDataTransaction(PSParams: TBoldPSSQlParams);
begin
  if PsParams.Database.InTransaction then
  begin
    BoldLog.Log(sRollBackMetaData);
    PsParams.Database.RollBack;
  end;
end;

procedure TBoldSQLSystemDescription.StartMetaDataTransaction(PSParams: TBoldPSSQlParams);
begin
  if EffectiveUseTransactions(PSParams) then
    PsParams.Database.StartTransaction;
end;

{ TBoldSQLDescriptionListTraverser }

function TBoldSQLDescriptionListTraverser.GetItem: TBoldSQLDescriptionElement;
begin
  result := TBoldSQLDescriptionElement(inherited item);
  Assert(result is TBoldSQLDescriptionElement);
end;

{ TBoldSQLTableDescriptionListTraverser }

function TBoldSQLTableDescriptionListTraverser.GetItem: TBoldSQLTableDescription;
begin
  result := TBoldSQLTableDescription(inherited item);
  Assert(result is TBoldSQLTableDescription);
end;

end.
