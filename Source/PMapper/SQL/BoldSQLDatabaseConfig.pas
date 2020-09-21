unit BoldSQLDatabaseConfig;

interface

uses
  BoldDefs,
  db,
  classes;

type
  TBoldDataBaseEngine = (
    dbeUnknown,
    dbeInterbaseSQLDialect1, dbeInterbaseSQLDialect3,
    dbeGenericANSISQL92,
    dbeSQLServer,
    dbeDBISAM,
    dbeOracle,
    dbeAdvantage,
    dbeParadox,
    dbeInformix);

  TBoldSQLDataBaseConfig = class;

  TBoldSQLDataBaseConfig = class(TPersistent)
  private
    FColumnTypeForBlob: string;
    FColumnTypeForDateTime: string;
    FColumnTypeForDate: string;
    FColumnTypeForTime: string;
    FColumnTypeForFloat: string;
    fOnChange: TNotifyEvent;
    fUseSQL92Joins: boolean;
    fFetchBlockSize: integer;
    fDefaultStringLength: integer;
    FColumnTypeForString: string;
    fDropColumnTemplate: string;
    fDropIndexTemplate: string;
    fDropTableTemplate: string;
    fSQLforNotNull: string;
    FColumnTypeForInteger: string;
    fColumnTypeForSmallInt: string;
    fSupportsConstraintsInCreateTable: Boolean;
    fQuoteNonStringDefaultValues: Boolean;
    fSupportsStringDefaultValues: Boolean;
    fReservedWords: TStringList;
    FColumnTypeForCurrency: string;
    fEngine: TBoldDatabaseEngine;
    fMaxParamsInIdList: integer;
    fMaxIndexNameLength: integer;
    fMaxDbIdentifierLength: integer;
    fDBGenerationMode: TBoldDatabaseGenerationMode;
    fAllowMetadataChangesInTransaction: Boolean;
    fFieldTypeForBlob: TFieldType;
    fEmptyStringMarker: String;
    fStoreEmptyStringsAsNULL: Boolean;
    fSystemTablePrefix: String;
    FSqlScriptCommentStart: string;
    FSqlScriptStartTransaction: string;
    FSqlScriptTerminator: string;
    FSqlScriptCommentStop: string;
    FSqlScriptSeparator: string;
    FSqlScriptRollBackTransaction: string;
    FSqlScriptCommitTransaction: string;
    procedure SetColumnTypeForBlob(const Value: string);
    procedure SetColumnTypeForDate(const Value: string);
    procedure SetColumnTypeForDateTime(const Value: string);
    procedure SetColumnTypeForTime(const Value: string);
    procedure Change;
    procedure SetUseSQL92Joins(const Value: boolean);
    procedure SetColumnTypeForFloat(const Value: string);
    procedure SetDefaultStringLength(const Value: integer);
    procedure SetFetchBlockSize(const Value: integer);
    procedure SetColumnTypeForString(const Value: string);
    procedure SetDropColumnTemplate(const Value: string);
    procedure SetDropIndexTemplate(const Value: string);
    procedure SetDropTableTemplate(const Value: string);
    procedure SetInitialValues;
    procedure SetSQLforNotNull(const Value: string);
    procedure SetColumnTypeForInteger(const Value: string);
    function GetEffectiveSQLForNotNull: string;
    procedure SetColumnTypeForSmallInt(const Value: string);
    procedure SetSupportsConstraintsInCreateTable(const Value: Boolean);
    procedure SetQuoteNonStringDefaultValues(const Value: Boolean);
    procedure SetSupportsStringDefaultValues(const Value: Boolean);
    procedure SetReservedWords(const Value: TStringList);
    procedure SetColumnTypeForCurrency(const Value: string);
    procedure SetMaxParamsInIdList(const Value: integer);
    procedure SetMaxIndexNameLenght(const Value: integer);
    procedure SetMaxDbIdentifierLength(const Value: integer);
    procedure SetDBGenerationMode(const Value: TBoldDatabaseGenerationMode);
    procedure setAllowMetadataChangesInTransaction(const Value: Boolean);
    procedure ReadUseTransactionsDuringDBCreate(Reader: TReader);
    procedure SetFieldTypeForBlob(const Value: TFieldType);
    procedure SetEmptyStringMarker(const Value: String);
    procedure SetStoreEmptyStringsAsNULL(const Value: Boolean);
    procedure SetSystemTablePrefix(const Value: String);
    procedure SetSqlScriptCommentStart(const Value: string);
    procedure SetSqlScriptCommentStop(const Value: string);
    procedure SetSqlScriptSeparator(const Value: string);
    procedure SetSqlScriptStartTransaction(const Value: string);
    procedure SetSqlScriptTerminator(const Value: string);
    procedure SetSqlScriptCommitTransaction(const Value: string);
    procedure SetSqlScriptRollBackTransaction(const Value: string);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignConfig(Source: TBoldSQLDataBaseConfig);
    function GetDropColumnQuery(const TableName, columnName: String): String;
    function GetDropIndexQuery(const TableName, IndexName: String): String;
    function GetDropTableQuery(const TableName: String): String;
    procedure InitializeDbEngineSettings(Engine: TBoldDatabaseEngine);
    function CorrectlyQuotedDefaultValue(value: string): String;
    property EffectiveSQLForNotNull: string read GetEffectiveSQLForNotNull;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
    property Engine: TBoldDatabaseEngine read fEngine write fEngine;
  published
    property ColumnTypeForDate: string read FColumnTypeForDate write SetColumnTypeForDate;
    property ColumnTypeForTime: string read FColumnTypeForTime write SetColumnTypeForTime;
    property ColumnTypeForDateTime: string read FColumnTypeForDateTime write SetColumnTypeForDateTime;
    property ColumnTypeForBlob: string read FColumnTypeForBlob write SetColumnTypeForBlob;
    property ColumnTypeForFloat: string read FColumnTypeForFloat write SetColumnTypeForFloat;
    property ColumnTypeForCurrency: string read FColumnTypeForCurrency write SetColumnTypeForCurrency;
    property ColumnTypeForString: string read FColumnTypeForString write SetColumnTypeForString;
    property ColumnTypeForInteger: string read FColumnTypeForInteger write SetColumnTypeForInteger;
    property ColumnTypeForSmallInt: string read fColumnTypeForSmallInt write SetColumnTypeForSmallInt;
    property FieldTypeForBlob: TFieldType read fFieldTypeForBlob write SetFieldTypeForBlob default ftBlob;
    property FetchBlockSize: integer read fFetchBlockSize write SetFetchBlockSize default 250;
    property MaxParamsInIdList: integer read fMaxParamsInIdList write SetMaxParamsInIdList default 20;
    property DefaultStringLength: integer read fDefaultStringLength write SetDefaultStringLength default 255;
    property UseSQL92Joins: boolean read fUseSQL92Joins write SetUseSQL92Joins default false;
    property DropColumnTemplate: string read fDropColumnTemplate write SetDropColumnTemplate;
    property DropTableTemplate: string read fDropTableTemplate write SetDropTableTemplate;
    property DropIndexTemplate: string read fDropIndexTemplate write SetDropIndexTemplate;
    property MaxDbIdentifierLength: integer read fMaxDbIdentifierLength write SetMaxDbIdentifierLength default -1;
    property MaxIndexNameLength: integer read fMaxIndexNameLength write SetMaxIndexNameLenght default 18;
    property SQLforNotNull: string read fSQLforNotNull write SetSQLforNotNull;
    property QuoteNonStringDefaultValues: Boolean read fQuoteNonStringDefaultValues write SetQuoteNonStringDefaultValues;
    property SupportsConstraintsInCreateTable: Boolean read fSupportsConstraintsInCreateTable write SetSupportsConstraintsInCreateTable;
    property SupportsStringDefaultValues: Boolean read fSupportsStringDefaultValues write SetSupportsStringDefaultValues;
    property DBGenerationMode: TBoldDatabaseGenerationMode read fDBGenerationMode write SetDBGenerationMode;
    property AllowMetadataChangesInTransaction: Boolean read fAllowMetadataChangesInTransaction write setAllowMetadataChangesInTransaction default True;
    property ReservedWords: TStringList read fReservedWords write SetReservedWords;
    property EmptyStringMarker: String read fEmptyStringMarker write SetEmptyStringMarker;
    property StoreEmptyStringsAsNULL: Boolean read fStoreEmptyStringsAsNULL write SetStoreEmptyStringsAsNULL;
    property SystemTablePrefix: String read fSystemTablePrefix write SetSystemTablePrefix;
    property SqlScriptSeparator: string read FSqlScriptSeparator write SetSqlScriptSeparator;
    property SqlScriptTerminator: string read FSqlScriptTerminator write SetSqlScriptTerminator;
    property SqlScriptCommentStart: string read FSqlScriptCommentStart write SetSqlScriptCommentStart;
    property SqlScriptCommentStop: string read FSqlScriptCommentStop write SetSqlScriptCommentStop;
    property SqlScriptStartTransaction: string read FSqlScriptStartTransaction write SetSqlScriptStartTransaction;
    property SqlScriptCommitTransaction: string read FSqlScriptCommitTransaction write SetSqlScriptCommitTransaction;
    property SqlScriptRollBackTransaction: string read FSqlScriptRollBackTransaction write SetSqlScriptRollBackTransaction;
  end;

implementation

uses
  SysUtils;

const
  EmptyMarker = '<Empty>';
  TableNameMarker = '<TableName>';
  ColumnNameMarker = '<ColumnName>';
  IndexNameMarker = '<IndexName>';

{ TBoldSQLDataBaseConfig }

procedure TBoldSQLDataBaseConfig.Assign(Source: TPersistent);
begin
  if Source is TBoldSQLDataBaseConfig then
    AssignConfig(Source as TBoldSQLDataBaseConfig)
  else
    inherited;
end;

procedure TBoldSQLDataBaseConfig.AssignConfig(Source: TBoldSQLDataBaseConfig);
begin
  fColumnTypeForBlob := Source.ColumnTypeForBlob;
  fColumnTypeForDateTime := Source.ColumnTypeForDateTime;
  fColumnTypeForDate := Source.ColumnTypeForDate;
  fColumnTypeForTime := Source.ColumnTypeForTime;
  fColumnTypeForFloat := Source.ColumnTypeForFloat;
  FColumnTypeForCurrency := Source.ColumnTypeForCurrency;
  fColumnTypeForString := Source.ColumnTypeForString;
  FColumnTypeForInteger := Source.ColumnTypeForInteger;
  fColumnTypeForSmallInt := Source.ColumnTypeForSmallInt;
  fUseSQL92Joins := Source.UseSQL92Joins;
  fFetchBlockSize := Source.FetchBlockSize;
  fMaxParamsInIdList := Source.MaxParamsInIdList;
  fMaxIndexNameLength := Source.MaxIndexNameLength;
  fMaxDbIdentifierLength := Source.MaxDbIdentifierLength;
  fFieldTypeForBlob := Source.FieldTypeForBlob;
  fStoreEmptyStringsAsNULL := Source.StoreEmptyStringsAsNULL;
  fSystemTablePrefix := Source.SystemTablePrefix;
  fEmptyStringMarker := Source.EmptyStringMarker;
  fAllowMetadataChangesInTransaction := Source.AllowMetadataChangesInTransaction;
  fDbGenerationMode := Source.DBGenerationMode;
  fDefaultStringLength := Source.DefaultStringLength;
  fDropColumnTemplate := Source.DropColumnTemplate;
  fDropTableTemplate := Source.DropTableTemplate;
  fDropIndexTemplate := Source.DropIndexTemplate;
  fSQLforNotNull := Source.SQLforNotNull;
  fSupportsConstraintsInCreateTable := Source.SupportsConstraintsInCreateTable;
  fQuoteNonStringDefaultValues := Source.QuoteNonStringDefaultValues;
  fSupportsStringDefaultValues := Source.SupportsStringDefaultValues;
  fEngine := Source.Engine;
  FSqlScriptCommentStart := Source.SqlScriptCommentStart;
  FSqlScriptTerminator := Source.SqlScriptTerminator;
  FSqlScriptCommentStop := Source.SqlScriptCommentStop;
  FSqlScriptSeparator := Source.SqlScriptSeparator;
  FSqlScriptStartTransaction := Source.SqlScriptStartTransaction;
  FSqlScriptCommitTransaction := Source.SqlScriptCommitTransaction;
  FSqlScriptRollBackTransaction := Source.SqlScriptRollBackTransaction;
  Change;
end;

procedure TBoldSQLDataBaseConfig.Change;
begin
  if assigned(fOnChange) then
    OnChange(self);
end;

constructor TBoldSQLDataBaseConfig.create;
begin
  inherited;
  fReservedWords := TStringList.Create;
  // Since SetInitialValues is called when the persistencehandle sets the "dbengine" property
  // and the fetchblocksize has not been tested with all databases, it should not be restored when
  // setting the initial values, instead it is initialized once in the constructor
  fFetchBlockSize := 250;
  SetInitialValues;
end;

procedure TBoldSQLDataBaseConfig.SetColumnTypeForBlob(const Value: string);
begin
  if fColumnTypeForBlob <> Value then
  begin
    FColumnTypeForBlob := Value;
    Change;
  end;
end;

procedure TBoldSQLDataBaseConfig.SetColumnTypeForDate(const Value: string);
begin
  if fColumnTypeForDate <> Value then
  begin
    FColumnTypeForDate := Value;
    Change;
  end;
end;

procedure TBoldSQLDataBaseConfig.SetColumnTypeForDateTime(
  const Value: string);
begin
  if fColumnTypeForDateTime <> Value then
  begin
    FColumnTypeForDateTime := Value;
    Change;
  end;
end;

procedure TBoldSQLDataBaseConfig.SetColumnTypeForFloat(const Value: string);
begin
  if FColumnTypeForFloat <> Value then
  begin
    FColumnTypeForFloat := Value;
    Change;
  end;
end;

procedure TBoldSQLDataBaseConfig.SetColumnTypeForString(const Value: string);
begin
  if FColumnTypeForString <> Value then
  begin
    FColumnTypeForString := Value;
    Change;
  end;
end;

procedure TBoldSQLDataBaseConfig.SetColumnTypeForTime(const Value: string);
begin
  if fColumnTypeForTime <> Value then
  begin
    FColumnTypeForTime := Value;
    Change;
  end;
end;

procedure TBoldSQLDataBaseConfig.SetDefaultStringLength(const Value: integer);
begin
  if fDefaultStringLength <> Value then
  begin
    fDefaultStringLength := Value;
    Change;
  end;
end;

procedure TBoldSQLDataBaseConfig.SetDropColumnTemplate(const Value: string);
begin
  if fDropColumnTemplate <> Value then
  begin
    fDropColumnTemplate := Value;
    Change;
  end;
end;

procedure TBoldSQLDataBaseConfig.SetDropIndexTemplate(const Value: string);
begin
  if fDropIndexTemplate <> Value then
  begin
    fDropIndexTemplate := Value;
    Change;
  end;
end;

procedure TBoldSQLDataBaseConfig.SetDropTableTemplate(const Value: string);
begin
  if fDropTableTemplate <> Value then
  begin
    fDropTableTemplate := Value;
    Change;
  end;
end;

procedure TBoldSQLDataBaseConfig.SetFetchBlockSize(const Value: integer);
begin
  if fFetchBlockSize <> Value then
  begin
    fFetchBlockSize := Value;
    Change;
  end;
end;

function TBoldSQLDataBaseConfig.GetDropColumnQuery(const TableName: string; const columnName: String): String;
begin
  result := DropColumnTemplate;
  result := StringReplace(result, TableNameMarker, TableName, [rfIgnoreCase, rfReplaceAll]);
  result := StringReplace(result, ColumnNameMarker, ColumnName, [rfIgnoreCase, rfReplaceAll]);
end;

function TBoldSQLDataBaseConfig.GetDropIndexQuery(const TableName: string; const IndexName: String): String;
begin
  result := DropIndexTemplate;
  result := StringReplace(result, TableNameMarker, TableName, [rfIgnoreCase, rfReplaceAll]);
  result := StringReplace(result, IndexNameMarker, IndexName, [rfIgnoreCase, rfReplaceAll]);
end;

function TBoldSQLDataBaseConfig.GetDropTableQuery(const TableName: String): String;
begin
  result := DropTableTemplate;
  result := StringReplace(result, TableNameMarker, TableName, [rfIgnoreCase, rfReplaceAll]);
end;


procedure TBoldSQLDataBaseConfig.SetInitialValues;
begin
  FColumnTypeForBlob := 'BLOB'; // do not localize
  FColumnTypeForDateTime := 'DATE'; // do not localize
  FColumnTypeForDate := 'DATE'; // do not localize
  FColumnTypeForTime := 'DATE'; // do not localize
  FColumnTypeForFloat := 'DOUBLE PRECISION'; // do not localize
  FColumnTypeForCurrency := 'DOUBLE PRECISION'; // do not localize
  fColumnTypeForString := 'VARCHAR(%d)'; // do not localize
  FColumnTypeForInteger := 'INTEGER'; // do not localize
  fColumnTypeForSmallInt := 'SMALLINT'; // do not localize
  fDefaultStringLength := 255;
  fMaxParamsInIdList := 20;
  fMaxIndexNameLength := 18;
  fMaxDbIdentifierLength := -1;
  fFieldTypeForBlob := ftBlob;
  fStoreEmptyStringsAsNULL := false;
  fSystemTablePrefix := 'BOLD'; // do not localize
  fEmptyStringMarker := '';
  fAllowMetadataChangesInTransaction := true;
  fDBGenerationMode := dbgQuery;
  fDropColumnTemplate := 'ALTER TABLE <TableName> DROP <ColumnName>'; // do not localize
  fDropTableTemplate := 'DROP TABLE <TableName>'; // do not localize
  fDropIndexTemplate := 'DROP INDEX <IndexName>'; // do not localize
  fSQLforNotNull := 'NOT NULL'; // do not localize
  fSupportsConstraintsInCreateTable := true;
  FQuoteNonStringDefaultValues := false;
  fSupportsStringDefaultValues := true;
  FSqlScriptCommentStart := '/* ';
  FSqlScriptStartTransaction := 'START TRANSACTION';
  FSqlScriptTerminator := ';';
  FSqlScriptCommentStop := ' */';
  FSqlScriptSeparator := '---';
  FSqlScriptRollBackTransaction := 'ROLLBACK';
  FSqlScriptCommitTransaction := 'COMMIT';
  fReservedWords.Text := 'ACTIVE, ADD, ALL, AFTER, ALTER'#10'AND, ANY, AS, ASC, ASCENDING,'#10 + // do not localize
                         'AT, AUTO, AUTOINC, AVG, BASE_NAME'#10'BEFORE, BEGIN, BETWEEN, BLOB, BOOLEAN,'#10 + // do not localize
                         'BOTH, BY, BYTES, CACHE, CAST, CHAR'#10'CHARACTER, CHECK, CHECK_POINT_LENGTH, COLLATE,'#10 + // do not localize
                         'COLUMN, COMMIT, COMMITTED, COMPUTED'#10'CONDITIONAL, CONSTRAINT, CONTAINING, COUNT, CREATE, CSTRING,'#10 + // do not localize
                         'CURRENT, CURSOR, DATABASE, DATE, DAY'#10'DEBUG, DEC, DECIMAL, DECLARE, DEFAULT,'#10 + // do not localize
                         'DELETE, DESC, DESCENDING, DISTINCT, DO'#10'DOMAIN, DOUBLE, DROP, ELSE, END,'#10 + // do not localize
                         'ENTRY_POINT, ESCAPE, EXCEPTION, EXECUTE'#10'EXISTS, EXIT, EXTERNAL, EXTRACT, FILE, FILTER,'#10 + // do not localize
                         'FLOAT, FOR, FOREIGN, FROM, FULL, FUNCTION'#10'GDSCODE, GENERATOR, GEN_ID, GRANT,'#10 + // do not localize
                         'GROUP, GROUP_COMMIT_WAIT_TIME, HAVING'#10'HOUR, IF, IN, INT, INACTIVE, INDEX, INNER,'#10 + // do not localize
                         'INPUT_TYPE, INSERT, INTEGER, INTO'#10'IS, ISOLATION, JOIN, KEY, LONG, LENGTH,'#10 + // do not localize
                         'LOGFILE, LOWER, LEADING, LEFT, LEVEL'#10'LIKE, LOG_BUFFER_SIZE, MANUAL, MAX, MAXIMUM_SEGMENT,'#10 + // do not localize
                         'MERGE, MESSAGE, MIN, MINUTE, MODULE_NAME'#10'MONEY, MONTH, NAMES, NATIONAL, NATURAL,'#10 + // do not localize
                         'NCHAR, NO, NOT, NULL, NUM_LOG_BUFFERS'#10'NUMERIC, OF, ON, ONLY, OPTION,'#10 + // do not localize
                         'OR, ORDER, OUTER, OUTPUT_TYPE, OVERFLOW'#10'PAGE_SIZE, PAGE, PAGES, PARAMETER, PASSWORD,'#10 + // do not localize
                         'PLAN, POSITION, POST_EVENT, PRECISION'#10'PROCEDURE, PROTECTED, PRIMARY, PRIVILEGES, RAW_PARTITIONS, RDB$DB_KEY,'#10 + // do not localize
                         'READ, REAL, RECORD_VERSION, REFERENCES'#10'RESERV, RESERVING, RETAIN, RETURNING_VALUES, RETURNS, REVOKE,'#10 + // do not localize
                         'RIGHT, ROLE, ROLLBACK, SECOND, SEGMENT'#10'SELECT, SET, SHARED, SHADOW, SCHEMA, SINGULAR,'#10 + // do not localize
                         'SIZE, SMALLINT, SNAPSHOT, SOME, SORT'#10'SQLCODE, STABILITY, STARTING, STARTS, STATISTICS,'#10 + // do not localize
                         'SUB_TYPE, SUBSTRING, SUM, SUSPEND, TABLE'#10'THEN, TIME, TIMESTAMP, TIMEZONE_HOUR, TIMEZONE_MINUTE,'#10 + // do not localize
                         'TO, TRAILING, TRANSACTION, TRIGGER, TRIM'#10'UNCOMMITTED, UNION, UNIQUE, UPDATE, UPPER,'#10 + // do not localize
                         'USER, VALUE, VALUES, VARCHAR, VARIABLE'#10'VARYING, VIEW, WAIT, WHEN, WHERE,'#10 + // do not localize
                         'WHILE, WITH, WORK, WRITE, YEAR'; // do not localize

  Change;
end;

procedure TBoldSQLDataBaseConfig.SetUseSQL92Joins(const Value: boolean);
begin
  if fUseSQL92Joins <> Value then
  begin
    fUseSQL92Joins := Value;
    Change;
  end;
end;

procedure TBoldSQLDataBaseConfig.InitializeDbEngineSettings(Engine: TBoldDatabaseEngine);
  procedure AddReservedWord(name: string);
  begin
    if pos(UpperCase(Name), UpperCase(ReservedWords.Text)) = 0 then
      ReservedWords.Add(name)
  end;
begin
  // if the engine is unknown, we do not alter any settings.
  if Engine <> dbeUnknown then
    SetInitialValues;
  case Engine of
    dbeSQLServer:
    begin
      fColumnTypeForDate := 'DATETIME'; // do not localize
      fColumnTypeForTime := 'DATETIME'; // do not localize
      fColumnTypeForDateTime := 'DATETIME'; // do not localize
      fColumnTypeForFloat := 'DECIMAL (28,10)'; // do not localize
      fColumnTypeForCurrency := 'DECIMAL (28,10)'; // do not localize
      fDropColumnTemplate := 'ALTER TABLE <TableName> DROP COLUMN <ColumnName>'; // do not localize
      fDropIndexTemplate := 'DROP INDEX <TableName>.<IndexName>'; // do not localize
    end;
    dbeGenericANSISQL92:
    begin
      fColumnTypeForDate := 'DATE'; // do not localize
      fColumnTypeForTime := 'TIME'; // do not localize
      fColumnTypeForDateTime := 'TIMESTAMP'; // do not localize
    end;
    dbeInterbaseSQLDialect3:
    begin
      fColumnTypeForDate := 'TIMESTAMP'; // do not localize
      fColumnTypeForTime := 'TIMESTAMP'; // do not localize
      fColumnTypeForDateTime := 'TIMESTAMP'; // do not localize
      fMaxIndexNameLength := 31;
      fMaxDbIdentifierLength := 31;
      fAllowMetadataChangesInTransaction := true;
    end;
    dbeInterbaseSQLDialect1:
    begin
      fMaxIndexNameLength := 31;
      fMaxDbIdentifierLength := 31;
      fAllowMetadataChangesInTransaction := true;
    end;
    dbeDBISAM:
    begin
      fColumnTypeForDate := 'DATE'; // do not localize
      fColumnTypeForTime := 'TIME'; // do not localize
      fColumnTypeForDateTime := 'TIMESTAMP'; // do not localize
      fColumnTypeForFloat := 'FLOAT'; // do not localize
      fColumnTypeForCurrency := 'FLOAT'; // do not localize
      fDefaultStringLength := 250;
      AddReservedWord('Description'); // do not localize
    end;
    dbeAdvantage:
    begin
      fSQLforNotNull := 'CONSTRAINT NOT NULL'; // do not localize
      fColumnTypeForSmallInt := 'SHORT'; // do not localize
      fColumnTypeForFloat := 'NUMERIC'; // do not localize
      fColumnTypeForCurrency := 'NUMERIC'; // do not localize
      fSupportsConstraintsInCreateTable := false;
      fQuoteNonStringDefaultValues := true;
      fSupportsStringDefaultValues := false;
      fColumnTypeForDate := 'DATE'; // do not localize
      fColumnTypeForTime := 'TIME'; // do not localize
      fColumnTypeForDateTime := 'TIMESTAMP'; // do not localize
      fColumnTypeForString := 'CHAR(%d)'; // do not localize
      fStoreEmptyStringsAsNULL := True;
    end;
    dbeOracle:
    begin
      FColumnTypeForString := 'VARCHAR2(%d)'; // do not localize
      FColumnTypeForFloat := 'NUMBER'; // do not localize
      FColumnTypeForCurrency := 'NUMBER'; // do not localize
      FColumnTypeForInteger := 'NUMBER(10,0)'; // do not localize
      fColumnTypeForSmallInt := 'NUMBER(5,0)'; // do not localize
      fDropColumnTemplate := 'ALTER TABLE <TableName> DROP COLUMN <ColumnName>'; // do not localize
      fMaxIndexNameLength := 30;
      fMaxDbIdentifierLength := 30;
    end;
    dbeParadox:
    begin
      fDBGenerationMode := dbgTable;
    end;
    dbeInformix:
    begin
      fMaxIndexNameLength := 18;
      fMaxDbIdentifierLength := 18;
      fColumnTypeForCurrency := 'MONEY'; // do not localize
      fColumnTypeForFloat := 'NUMERIC'; // do not localize
      fColumnTypeForDate := 'DATETIME YEAR TO DAY'; // do not localize
      fColumnTypeForTime := 'DATETIME HOUR TO FRACTION'; // do not localize
      fColumnTypeForDateTime := 'DATETIME YEAR TO FRACTION'; // do not localize
    end;
  end;
  Change;
end;

procedure TBoldSQLDataBaseConfig.SetSQLforNotNull(const Value: string);
var
  Temp: string;
begin
  if Value = '' then
    temp := EmptyMarker
  else
    temp := Value;
  if fSQLforNotNull <> temp then
  begin
    fSQLforNotNull := temp;
    Change;
  end;
end;

procedure TBoldSQLDataBaseConfig.SetColumnTypeForInteger(const Value: string);
begin
  if FColumnTypeForInteger <> Value then
  begin
    FColumnTypeForInteger := Value;
    Change;
  end;
end;

function TBoldSQLDataBaseConfig.GetEffectiveSQLForNotNull: string;
begin
  if SQLforNotNull = EmptyMarker then
    result := ''
  else
    result := SQLforNotNull;
end;

procedure TBoldSQLDataBaseConfig.SetColumnTypeForSmallInt(const Value: string);
begin
  if FColumnTypeForSmallInt <> Value then
  begin
    fColumnTypeForSmallInt := Value;
    Change;
  end;
end;

procedure TBoldSQLDataBaseConfig.SetSupportsConstraintsInCreateTable(const Value: Boolean);
begin
  if fSupportsConstraintsInCreateTable <> Value then
  begin
    fSupportsConstraintsInCreateTable := Value;
    Change;
  end;
end;

procedure TBoldSQLDataBaseConfig.SetQuoteNonStringDefaultValues(const Value: Boolean);
begin
  if fQuoteNonStringDefaultValues <> Value then
  begin
    fQuoteNonStringDefaultValues := Value;
    Change;
  end;
end;

function TBoldSQLDataBaseConfig.CorrectlyQuotedDefaultValue(
  value: string): String;
begin
  result := Value;
  if QuoteNonStringDefaultValues then
    result := '''' + result + '''';
end;

procedure TBoldSQLDataBaseConfig.SetSupportsStringDefaultValues(const Value: Boolean);
begin
  if fSupportsStringDefaultValues <> Value then
  begin
    fSupportsStringDefaultValues := Value;
    change;
  end;
end;

procedure TBoldSQLDataBaseConfig.SetReservedWords(const Value: TStringList);
begin
  fReservedWords.Assign(Value);
end;

destructor TBoldSQLDataBaseConfig.Destroy;
begin
  FreeAndNil(fReservedWords);
  inherited;
end;

procedure TBoldSQLDataBaseConfig.SetColumnTypeForCurrency(const Value: string);
begin
  if FColumnTypeForCurrency <> Value then
  begin
    FColumnTypeForCurrency := Value;
    Change;
  end;
end;

procedure TBoldSQLDataBaseConfig.SetMaxParamsInIdList(const Value: integer);
begin
  if fMaxParamsInIdList <> Value then
  begin
    fMaxParamsInIdList := Value;
    Change;
  end;
end;

procedure TBoldSQLDataBaseConfig.SetMaxIndexNameLenght(const Value: integer);
begin
  if fMaxIndexNameLength <> Value then
  begin
    fMaxIndexNameLength := Value;
    Change;
  end;
end;

procedure TBoldSQLDataBaseConfig.SetMaxDbIdentifierLength(const Value: integer);
begin
  if fMaxDbIdentifierLength <> Value then
  begin
    fMaxDbIdentifierLength := Value;
    Change;
  end;
end;

procedure TBoldSQLDataBaseConfig.SetDBGenerationMode(const Value: TBoldDatabaseGenerationMode);
begin
  if fDbGenerationMode <> Value then
  begin
    fDBGenerationMode := Value;
    Change;
  end;
end;

procedure TBoldSQLDataBaseConfig.setAllowMetadataChangesInTransaction(const Value: Boolean);
begin
  if fAllowMetadataChangesInTransaction <> Value then
  begin
    fAllowMetadataChangesInTransaction := Value;
    Change;
  end;
end;

procedure TBoldSQLDataBaseConfig.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('UseTransactionsDuringDBCreate', ReadUseTransactionsDuringDBCreate, nil, True); // do not localize
end;

procedure TBoldSQLDataBaseConfig.ReadUseTransactionsDuringDBCreate(Reader: TReader);
begin
  AllowMetadataChangesInTransaction := Reader.ReadBoolean;
end;

procedure TBoldSQLDataBaseConfig.SetFieldTypeForBlob(const Value: TFieldType);
begin
  if fFieldTypeForBlob <> Value then
  begin
    fFieldTypeForBlob := Value;
    Change;
  end;
end;

procedure TBoldSQLDataBaseConfig.SetEmptyStringMarker(const Value: String);
begin
  if fEmptyStringMarker <> Value then
  begin
    fEmptyStringMarker := Value;
    Change;
  end;
end;

procedure TBoldSQLDataBaseConfig.SetStoreEmptyStringsAsNULL(const Value: Boolean);
begin
  if fStoreEmptyStringsAsNULL <> Value then
  begin
    fStoreEmptyStringsAsNULL := Value;
    Change;
  end;
end;

procedure TBoldSQLDataBaseConfig.SetSystemTablePrefix(const Value: String);
var
  Temp: String;
begin
  if Value = '' then
    temp := 'BOLD' // do not localize
  else
    temp := Value;
  if fSystemTablePrefix <> temp then
  begin
    fSystemTablePrefix := temp;
    Change;
  end;
end;

procedure TBoldSQLDataBaseConfig.SetSqlScriptCommentStart(
  const Value: string);
begin
  FSqlScriptCommentStart := Value;
  Change;
end;

procedure TBoldSQLDataBaseConfig.SetSqlScriptCommentStop(
  const Value: string);
begin
  FSqlScriptCommentStop := Value;
  Change;
end;

procedure TBoldSQLDataBaseConfig.SetSqlScriptSeparator(
  const Value: string);
begin
  FSqlScriptSeparator := Value;
  Change;
end;

procedure TBoldSQLDataBaseConfig.SetSqlScriptStartTransaction(
  const Value: string);
begin
  FSqlScriptStartTransaction := Value;
  Change;
end;

procedure TBoldSQLDataBaseConfig.SetSqlScriptTerminator(
  const Value: string);
begin
  FSqlScriptTerminator := Value;
  Change;
end;

procedure TBoldSQLDataBaseConfig.SetSqlScriptCommitTransaction(
  const Value: string);
begin
  FSqlScriptCommitTransaction := Value;
  Change;
end;

procedure TBoldSQLDataBaseConfig.SetSqlScriptRollBackTransaction(
  const Value: string);
begin
  FSqlScriptRollBackTransaction := Value;
  Change;
end;

end.
