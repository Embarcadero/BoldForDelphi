
{ Global compiler directives }
{$include bold.inc}
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
    dbePostgres,
    dbeDBISAM,
    dbeOracle,
    dbeAdvantage,
    dbeParadox,
    dbeInformix);

const
  cMaxBatchQueryLength = 65536 * 64;
  cMaxBatchQueryParams = 1000;

type
  TBoldSQLDataBaseConfig = class;

  TBoldSQLDataBaseConfig = class(TPersistent)
  private
    fIfTemplate: string;
    fColumnExistsTemplate: string;
    fTableExistsTemplate: string;
    fIndexExistsTemplate: string;
    fIndexColumnExistsTemplate: string;
    fColumnTypeForBlob: string;
    fColumnTypeForDateTime: string;
    fColumnTypeForDate: string;
    fColumnTypeForTime: string;
    fColumnTypeForFloat: string;
    fColumnTypeForInt64: string;
    fDefaultValueForDateTime: string;
    fDefaultSystemMapper: string;
    fDefaultObjectMapper: string;
    fOnChange: TNotifyEvent;
    fUseSQL92Joins: boolean;
    fSingleIndexOrderedLinks: Boolean;
    fFetchBlockSize: integer;
    fMultiRowInsertLimit: integer;
    fUseParamsForInteger: boolean;
    fUseParamsForEmptyString: boolean;
    fDefaultStringLength: integer;
    fColumnTypeForString: string;
    fColumnTypeForUnicodeString: string;
    fColumnTypeForAnsiString: string;
    fColumnTypeForText: string;
    fColumnTypeForUnicodeText: string;
    fColumnTypeForAnsiText: string;
    fLongStringLimit: integer;
    fDropColumnTemplate: string;
    fDropIndexTemplate: string;
    fDropTableTemplate: string;
    fIndexInfoTemplate: string;
    fSQLforNotNull: string;
    fColumnTypeForInteger: string;
    fColumnTypeForSmallInt: string;
    fColumnTypeForGUID: string;
    fSupportsConstraintsInCreateTable: Boolean;
    fQuoteNonStringDefaultValues: Boolean;
    fSupportsStringDefaultValues: Boolean;
    fReservedWords: TStringList;
    fColumnTypeForCurrency: string;
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
    fSqlScriptCommentStart: string;
    fSqlScriptStartTransaction: string;
    fSqlScriptTerminator: string;
    fSqlScriptCommentStop: string;
    fSqlScriptSeparator: string;
    fSqlScriptRollBackTransaction: string;
    fSqlScriptCommitTransaction: string;
    fDatabaseCaseSensitiveTemplate: string;
    fQuoteLeftBracketInLike: Boolean;
    fIgnoreMissingObjects: boolean;
    fMaxBatchQueryLength: integer;
    fMaxBatchQueryParams: integer;
    fBatchQueryBegin: string;
    fBatchQueryEnd: string;
    fBatchQuerySeparator: string;
    fUseBatchQueries: boolean;
    fSQLforNull: string;
    fEvolveDropsUnknownIndexes: boolean;
    fCreateDatabaseTemplate: string;
    fDropDatabaseTemplate: string;
    fDatabaseExistsTemplate: string;
    fUnicodeStringPrefix: string;
    fTreatStringFieldAsUnicode: boolean;
    procedure SetIfTemplate(const Value: string);
    procedure SetColumnExistsTemplate(const Value: string);
    procedure SetTableExistsTemplate(const Value: string);
    procedure SetIndexExistsTemplate(const Value: string);
    procedure SetIndexColumnExistsTemplate(const Value: string);
    procedure SetColumnTypeForBlob(const Value: string);
    procedure SetColumnTypeForDate(const Value: string);
    procedure SetColumnTypeForDateTime(const Value: string);
    procedure SetColumnTypeForTime(const Value: string);
    procedure SetDefaultValueForDateTime(const Value: string);
    procedure SetDefaultObjectMapper(const Value: string);
    procedure SetDefaultSystemMapper(const Value: string);
    procedure Change;
    procedure SetUseSQL92Joins(const Value: boolean);
    procedure SetSingleIndexOrderedLinks(const Value: boolean);
    procedure SetColumnTypeForFloat(const Value: string);
    procedure SetDefaultStringLength(const Value: integer);
    procedure SetFetchBlockSize(const Value: integer);
    procedure SetColumnTypeForString(const Value: string);
    procedure SetColumnTypeForUnicodeString(const Value: string);
    procedure SetColumnTypeForText(const Value: string);
    procedure SetColumnTypeForUnicodeText(const Value: string);
    procedure SetLongStringLimit(Value: integer);
    procedure SetDropColumnTemplate(const Value: string);
    procedure SetDropIndexTemplate(const Value: string);
    procedure SetDropTableTemplate(const Value: string);
    procedure SetIndexInfoTemplate(const Value: string);
    procedure SetInitialValues;
    procedure SetSQLforNotNull(const Value: string);
    procedure SetColumnTypeForInteger(const Value: string);
    function GetEffectiveSQLForNotNull: string;
    procedure SetColumnTypeForSmallInt(const Value: string);
    procedure SetColumnTypeForInt64(const Value: string);
    procedure SetColumnTypeForGUID(const Value: string);
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
    procedure SetDatabaseCaseSensitiveTemplate(const Value: string);
    procedure SetFieldTypeForBlob(const Value: TFieldType);
    procedure SetEmptyStringMarker(const Value: String);
    procedure SetStoreEmptyStringsAsNULL(const Value: Boolean);
    procedure SetSystemTablePrefix(const Value: String);
    procedure SetQuoteLeftBracketInLike(const Value: Boolean);
    procedure SetSqlScriptCommentStart(const Value: string);
    procedure SetSqlScriptCommentStop(const Value: string);
    procedure SetSqlScriptSeparator(const Value: string);
    procedure SetSqlScriptStartTransaction(const Value: string);
    procedure SetSqlScriptTerminator(const Value: string);
    procedure SetSqlScriptCommitTransaction(const Value: string);
    procedure SetSqlScriptRollBackTransaction(const Value: string);
    procedure SetIgnoreMissingObjects(const Value: boolean);
    procedure SetMaxBatchQueryLength(const Value: integer);
    procedure SetMaxBatchQueryParams(const Value: integer);
    procedure SetBatchQueryBegin(const Value: string);
    procedure SetBatchQueryEnd(const Value: string);
    procedure SetBatchQuerySeparator(const Value: string);
    procedure SetUseBatchQueries(const Value: boolean);
    procedure SetMultiRowInsertLimit(const Value: integer);
    procedure SetUseParamsForInteger(const Value: boolean);
    procedure SetUseParamsForEmptyString(const Value: boolean);
    procedure SetSQLforNull(const Value: string);
    procedure SetEvolveDropsUnknownIndexes(const Value: boolean);
    procedure SetCreateDatabaseTemplate(const Value: string);
    procedure SetDropDatabaseTemplate(const Value: string);
    procedure SetDatabaseExistsTemplate(const Value: string);
    procedure SetUnicodeStringPrefix(const Value: string);
    procedure SetColumnTypeForAnsiString(const Value: string);
    procedure SetColumnTypeForAnsiText(const Value: string);
    procedure SetTreatStringFieldAsUnicode(const Value: boolean);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignConfig(Source: TBoldSQLDataBaseConfig);
    function GetCreateDatabaseQuery(const DatabaseName: String): String;
    function GetDropDatabaseQuery(const DatabaseName: String): String;
    function GetDatabaseExistsQuery(const DatabaseName: String): String;
    function GetDropColumnQuery(const TableName, columnName: String): String;
    function GetDropIndexQuery(const TableName, IndexName: String): String;
    function GetDropTableQuery(const TableName: String): String;
    function GetIndexInfoQuery(const TableName: String): String;
    function GetColumnTypeForString(Size: Integer): string;
    function GetColumnTypeForUnicodeString(Size: Integer): string;
    function GetColumnTypeForAnsiString(Size: Integer): string;
    procedure InitializeDbEngineSettings(Engine: TBoldDatabaseEngine);
    function CorrectlyQuotedDefaultValue(value: string): String;
    function GetColumnExistsQuery(const TableName, ColumnName: string): string;
    function GetIfColumnNotExistsQuery(const TableName, ColumnName, SQLStatement:
        string): string;
    function GetIndexColumnExistsQuery(const TableName, IndexColumnName: string):
        string;
    function GetIndexExistsQuery(const TableName, IndexName: string): string;
    function GetTableExistsQuery(const TableName: string): string;

    function IsSQLServerEngine: Boolean;

    property EffectiveSQLForNotNull: string read GetEffectiveSQLForNotNull;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
    property Engine: TBoldDatabaseEngine read fEngine write fEngine;
  published
    property DefaultSystemMapper: string read FDefaultSystemMapper write SetDefaultSystemMapper;
    property DefaultObjectMapper: string read FDefaultObjectMapper write SetDefaultObjectMapper;
    property IfTemplate: string read FIfTemplate write SetIfTemplate;
    property ColumnExistsTemplate: string read FColumnExistsTemplate write SetColumnExistsTemplate;
    property TableExistsTemplate: string read FTableExistsTemplate write SetTableExistsTemplate;
    property IndexExistsTemplate: string read FIndexExistsTemplate write SetIndexExistsTemplate;
    property IndexColumnExistsTemplate: string read FIndexColumnExistsTemplate write SetIndexColumnExistsTemplate;
    property ColumnTypeForDate: string read FColumnTypeForDate write SetColumnTypeForDate;
    property ColumnTypeForTime: string read FColumnTypeForTime write SetColumnTypeForTime;
    property ColumnTypeForDateTime: string read FColumnTypeForDateTime write SetColumnTypeForDateTime;
    property DefaultValueForDateTime: string read FDefaultValueForDateTime write SetDefaultValueForDateTime;
    property ColumnTypeForBlob: string read FColumnTypeForBlob write SetColumnTypeForBlob;
    property ColumnTypeForFloat: string read FColumnTypeForFloat write SetColumnTypeForFloat;
    property ColumnTypeForCurrency: string read FColumnTypeForCurrency write SetColumnTypeForCurrency;
    property ColumnTypeForString: string read FColumnTypeForString write SetColumnTypeForString;
    property ColumnTypeForUnicodeString: string read FColumnTypeForUnicodeString write SetColumnTypeForUnicodeString;
    property ColumnTypeForAnsiString: string read FColumnTypeForAnsiString write SetColumnTypeForAnsiString;
    property ColumnTypeForText: string read FColumnTypeForText write SetColumnTypeForText;
    property ColumnTypeForUnicodeText: string read FColumnTypeForUnicodeText write SetColumnTypeForUnicodeText;
    property ColumnTypeForAnsiText: string read FColumnTypeForAnsiText write SetColumnTypeForAnsiText;
    property LongStringLimit: integer read FLongStringLimit write SetLongStringLimit default -1;
    property ColumnTypeForInteger: string read FColumnTypeForInteger write SetColumnTypeForInteger;
    property ColumnTypeForSmallInt: string read fColumnTypeForSmallInt write SetColumnTypeForSmallInt;
    property ColumnTypeForInt64: string read fColumnTypeForInt64 write SetColumnTypeForInt64;
    property ColumnTypeForGUID: string read fColumnTypeForGUID write SetColumnTypeForGUID;
    property FieldTypeForBlob: TFieldType read fFieldTypeForBlob write SetFieldTypeForBlob default ftBlob;
    property FetchBlockSize: integer read fFetchBlockSize write SetFetchBlockSize default 250;
    property MaxParamsInIdList: integer read fMaxParamsInIdList write SetMaxParamsInIdList default 20;
    property DefaultStringLength: integer read fDefaultStringLength write SetDefaultStringLength default 255;
    property UseSQL92Joins: boolean read fUseSQL92Joins write SetUseSQL92Joins default false;
    property SingleIndexOrderedLinks: boolean read fSingleIndexOrderedLinks write SetSingleIndexOrderedLinks default false;
    property IgnoreMissingObjects: boolean read fIgnoreMissingObjects write SetIgnoreMissingObjects default false;
    property CreateDatabaseTemplate: string read fCreateDatabaseTemplate write SetCreateDatabaseTemplate;
    property DropDatabaseTemplate: string read fDropDatabaseTemplate write SetDropDatabaseTemplate;
    property DatabaseExistsTemplate: string read fDatabaseExistsTemplate write SetDatabaseExistsTemplate;
    property DropColumnTemplate: string read fDropColumnTemplate write SetDropColumnTemplate;
    property DropTableTemplate: string read fDropTableTemplate write SetDropTableTemplate;
    property IndexInfoTemplate: string read fIndexInfoTemplate write SetIndexInfoTemplate;
    property DropIndexTemplate: string read fDropIndexTemplate write SetDropIndexTemplate;
    property EvolveDropsUnknownIndexes: boolean read fEvolveDropsUnknownIndexes write SetEvolveDropsUnknownIndexes;
    property MaxDbIdentifierLength: integer read fMaxDbIdentifierLength write SetMaxDbIdentifierLength default -1;
    property MaxIndexNameLength: integer read fMaxIndexNameLength write SetMaxIndexNameLenght default 18;
    property MaxBatchQueryLength: integer read fMaxBatchQueryLength write SetMaxBatchQueryLength default cMaxBatchQueryLength;
    property MaxBatchQueryParams: integer read fMaxBatchQueryParams write SetMaxBatchQueryParams default cMaxBatchQueryParams;
    property BatchQueryBegin: string read fBatchQueryBegin write SetBatchQueryBegin;
    property BatchQueryEnd: string read fBatchQueryEnd write SetBatchQueryEnd;
    property BatchQuerySeparator: string read fBatchQuerySeparator write SetBatchQuerySeparator;
    property UseBatchQueries: boolean read fUseBatchQueries write SetUseBatchQueries default false;
    property UseParamsForInteger: boolean read fUseParamsForInteger write SetUseParamsForInteger default false;
    property UseParamsForEmptyString: boolean read fUseParamsForEmptyString write SetUseParamsForEmptyString default false;
    property MultiRowInsertLimit: integer read fMultiRowInsertLimit write SetMultiRowInsertLimit default 1;
    property SQLforNull: string read fSQLforNull write SetSQLforNull;
    property SQLforNotNull: string read fSQLforNotNull write SetSQLforNotNull;
    property QuoteNonStringDefaultValues: Boolean read fQuoteNonStringDefaultValues write SetQuoteNonStringDefaultValues;
    property SupportsConstraintsInCreateTable: Boolean read fSupportsConstraintsInCreateTable write SetSupportsConstraintsInCreateTable;
    property SupportsStringDefaultValues: Boolean read fSupportsStringDefaultValues write SetSupportsStringDefaultValues;
    property DBGenerationMode: TBoldDatabaseGenerationMode read fDBGenerationMode write SetDBGenerationMode;
    property AllowMetadataChangesInTransaction: Boolean read fAllowMetadataChangesInTransaction write setAllowMetadataChangesInTransaction default True;
    property DatabaseCaseSensitiveTemplate: string read FDatabaseCaseSensitiveTemplate write SetDatabaseCaseSensitiveTemplate;
    property ReservedWords: TStringList read fReservedWords write SetReservedWords;
    property EmptyStringMarker: String read fEmptyStringMarker write SetEmptyStringMarker;
    property StoreEmptyStringsAsNULL: Boolean read fStoreEmptyStringsAsNULL write SetStoreEmptyStringsAsNULL;
    property UnicodeStringPrefix: string read fUnicodeStringPrefix write SetUnicodeStringPrefix;
    property SystemTablePrefix: String read fSystemTablePrefix write SetSystemTablePrefix;
    property QuoteLeftBracketInLike: Boolean read fQuoteLeftBracketInLike write SetQuoteLeftBracketInLike;
    property SqlScriptSeparator: string read FSqlScriptSeparator write SetSqlScriptSeparator;
    property SqlScriptTerminator: string read FSqlScriptTerminator write SetSqlScriptTerminator;
    property SqlScriptCommentStart: string read FSqlScriptCommentStart write SetSqlScriptCommentStart;
    property SqlScriptCommentStop: string read FSqlScriptCommentStop write SetSqlScriptCommentStop;
    property SqlScriptStartTransaction: string read FSqlScriptStartTransaction write SetSqlScriptStartTransaction;
    property SqlScriptCommitTransaction: string read FSqlScriptCommitTransaction write SetSqlScriptCommitTransaction;
    property SqlScriptRollBackTransaction: string read FSqlScriptRollBackTransaction write SetSqlScriptRollBackTransaction;
    property TreatStringFieldAsUnicode: boolean read fTreatStringFieldAsUnicode write SetTreatStringFieldAsUnicode;

  end;

implementation

uses
  SysUtils;

const
  EmptyMarker = '<Empty>';
  DatabaseNameMarker = '<DatabaseName>';
  TableNameMarker = '<TableName>';
  ColumnNameMarker = '<ColumnName>';
  IndexNameMarker = '<IndexName>';
  ConditionMarker = '<Condition>';
  SQLStatementMarker = '<SQLStatement>';
  IndexColumnNameMarker = '<IndexColumnName>';

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
  fDefaultValueForDateTime := Source.DefaultValueForDateTime;
  fColumnTypeForDate := Source.ColumnTypeForDate;
  fColumnTypeForTime := Source.ColumnTypeForTime;
  fColumnTypeForFloat := Source.ColumnTypeForFloat;
  FColumnTypeForCurrency := Source.ColumnTypeForCurrency;
  fColumnTypeForString := Source.ColumnTypeForString;
  fColumnTypeForUnicodeString := Source.ColumnTypeForUnicodeString;
  fColumnTypeForAnsiString := Source.ColumnTypeForAnsiString;
  fColumnTypeForText := Source.ColumnTypeForText;
  fColumnTypeForUnicodeText := Source.ColumnTypeForUnicodeText;
  fColumnTypeForAnsiText := Source.ColumnTypeForAnsiText;
  FLongStringLimit := Source.LongStringLimit;
  FColumnTypeForInteger := Source.ColumnTypeForInteger;
  fColumnTypeForSmallInt := Source.ColumnTypeForSmallInt;
  fColumnTypeForInt64 := Source.ColumnTypeForInt64;
  fColumnTypeForGUID := Source.ColumnTypeForGUID;
  FDefaultSystemMapper := Source.FDefaultSystemMapper;
  FDefaultObjectMapper := Source.FDefaultObjectMapper;
  fUseSQL92Joins := Source.UseSQL92Joins;
  fMultiRowInsertLimit := Source.MultiRowInsertLimit;
  fUseParamsForInteger := Source.UseParamsForInteger;
  fUseParamsForEmptyString := Source.UseParamsForEmptyString;
  fSingleIndexOrderedLinks := Source.SingleIndexOrderedLinks;
  fFetchBlockSize := Source.FetchBlockSize;
  fIndexInfoTemplate := Source.IndexInfoTemplate;
  fMaxParamsInIdList := Source.MaxParamsInIdList;
  fMaxIndexNameLength := Source.MaxIndexNameLength;
  fMaxDbIdentifierLength := Source.MaxDbIdentifierLength;
  fMaxIndexNameLength := Source.MaxIndexNameLength;
  fMaxBatchQueryLength := Source.MaxBatchQueryLength;
  fMaxBatchQueryParams := Source.MaxBatchQueryParams;
  fBatchQueryBegin := Source.BatchQueryBegin;
  fBatchQueryEnd := Source.BatchQueryEnd;
  fBatchQuerySeparator := Source.BatchQuerySeparator;
  fUseBatchQueries := Source.UseBatchQueries;
  fFieldTypeForBlob := Source.FieldTypeForBlob;
  fStoreEmptyStringsAsNULL := Source.StoreEmptyStringsAsNULL;
  fSystemTablePrefix := Source.SystemTablePrefix;
  fEmptyStringMarker := Source.EmptyStringMarker;
  fUnicodeStringPrefix := Source.UnicodeStringPrefix;
  fTreatStringFieldAsUnicode := Source.TreatStringFieldAsUnicode;
  fAllowMetadataChangesInTransaction := Source.AllowMetadataChangesInTransaction;
  fDbGenerationMode := Source.DBGenerationMode;
  fDefaultStringLength := Source.DefaultStringLength;
  fCreateDatabaseTemplate := Source.CreateDatabaseTemplate;
  fDropDatabaseTemplate := Source.DropDatabaseTemplate;
  fDatabaseExistsTemplate := Source.DatabaseExistsTemplate;
  fDropColumnTemplate := Source.DropColumnTemplate;
  fDropTableTemplate := Source.DropTableTemplate;
  fDropIndexTemplate := Source.DropIndexTemplate;
  fEvolveDropsUnknownIndexes := Source.EvolveDropsUnknownIndexes;
  fSQLforNull := Source.SQLforNull;
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
  FIfTemplate := Source.IfTemplate;
  FColumnExistsTemplate := Source.ColumnExistsTemplate;
  FTableExistsTemplate := Source.TableExistsTemplate;
  FIndexExistsTemplate := Source.IndexExistsTemplate;
  FIndexColumnExistsTemplate := Source.IndexColumnExistsTemplate;
  FDatabaseCaseSensitiveTemplate := Source.DatabaseCaseSensitiveTemplate;
  FIgnoreMissingObjects := Source.IgnoreMissingObjects;
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
  FDefaultSystemMapper := DEFAULTNAME;
  FDefaultObjectMapper := DEFAULTNAME;
  // Since SetInitialValues is called when the persistencehandle sets the "dbengine" property
  // and the fetchblocksize has not been tested with all databases, it should not be restored when
  // setting the initial values, instead it is initialized once in the constructor
  fFetchBlockSize := 250;
  SetInitialValues;
end;

procedure TBoldSQLDataBaseConfig.SetBatchQueryBegin(const Value: string);
begin
  if fBatchQueryBegin <> Value then
  begin
    fBatchQueryBegin := Value;
    Change;
  end;
end;

procedure TBoldSQLDataBaseConfig.SetBatchQueryEnd(const Value: string);
begin
  if fBatchQueryEnd <> Value then
  begin
    fBatchQueryEnd := Value;
    Change;
  end;
end;

procedure TBoldSQLDataBaseConfig.SetBatchQuerySeparator(const Value: string);
begin
  if fBatchQuerySeparator <> Value then
  begin
    fBatchQuerySeparator := Value;
    Change;
  end;
end;

procedure TBoldSQLDataBaseConfig.SetColumnExistsTemplate(const Value: string);
begin
  if FColumnExistsTemplate <> Value then
  begin
    FColumnExistsTemplate := Value;
    Change;
  end;
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

procedure TBoldSQLDataBaseConfig.SetColumnTypeForGUID(const Value: string);
begin
  if FColumnTypeForGUID <> Value then
  begin
    FColumnTypeForGUID := Value;
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

procedure TBoldSQLDataBaseConfig.SetColumnTypeForUnicodeString(const Value: string);
begin
  if FColumnTypeForUnicodeString <> Value then
  begin
    FColumnTypeForUnicodeString := Value;
    Change;
  end;
end;

procedure TBoldSQLDataBaseConfig.SetColumnTypeForAnsiString(
  const Value: string);
begin
  if FColumnTypeForAnsiString <> Value then
  begin
    FColumnTypeForAnsiString := Value;
    Change;
  end;
end;

procedure TBoldSQLDataBaseConfig.SetColumnTypeForText(const Value: string);
begin
  if FColumnTypeForText <> Value then
  begin
    FColumnTypeForText := Value;
    Change;
  end;
end;

procedure TBoldSQLDataBaseConfig.SetColumnTypeForUnicodeText(const Value: string);
begin
  if FColumnTypeForUnicodeText <> Value then
  begin
    FColumnTypeForUnicodeText := Value;
    Change;
  end;
end;

procedure TBoldSQLDataBaseConfig.SetColumnTypeForAnsiText(const Value: string);
begin
  if FColumnTypeForAnsiText <> Value then
  begin
    FColumnTypeForAnsiText := Value;
    Change;
  end;
end;

procedure TBoldSQLDataBaseConfig.SetCreateDatabaseTemplate(const Value: string);
begin
  if fCreateDatabaseTemplate <> Value then
  begin
    fCreateDatabaseTemplate := Value;
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

procedure TBoldSQLDataBaseConfig.SetDefaultObjectMapper(const Value: string);
begin
  if fDefaultObjectMapper <> Value then
  begin
    fDefaultObjectMapper := Value;
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

procedure TBoldSQLDataBaseConfig.SetDefaultSystemMapper(const Value: string);
begin
  if fDefaultSystemMapper <> Value then
  begin
    fDefaultSystemMapper := Value;
    Change;
  end;
end;

procedure TBoldSQLDataBaseConfig.SetDefaultValueForDateTime(
  const Value: string);
begin
  if fDefaultValueForDateTime <> Value then
  begin
    fDefaultValueForDateTime := Value;
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

procedure TBoldSQLDataBaseConfig.SetDropDatabaseTemplate(const Value: string);
begin
  if fDropDatabaseTemplate <> Value then
  begin
    fDropDatabaseTemplate := Value;
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

function TBoldSQLDataBaseConfig.GetColumnTypeForString(Size: Integer): string;
begin
  if ( LongStringLimit = -1) or (Size <= LongStringLimit) then
    Result := Format(ColumnTypeForString, [Size])
  else
    Result := Format(ColumnTypeForText, [Size]);
end;

function TBoldSQLDataBaseConfig.GetColumnTypeForUnicodeString(
  Size: Integer): string;
begin
  if ( LongStringLimit = -1) or (Size <= LongStringLimit) then
    Result := Format(ColumnTypeForUnicodeString, [Size])
  else
    Result := Format(ColumnTypeForUnicodeText, [Size]);
end;

function TBoldSQLDataBaseConfig.GetColumnTypeForAnsiString(
  Size: Integer): string;
begin
  if ( LongStringLimit = -1) or (Size <= LongStringLimit) then
    Result := Format(ColumnTypeForAnsiString, [Size])
  else
    Result := Format(ColumnTypeForAnsiText, [Size]);
end;

function TBoldSQLDataBaseConfig.GetCreateDatabaseQuery(
  const DatabaseName: String): String;
begin
  Result := '';
  if CreateDatabaseTemplate <> '' then begin
    Result := CreateDatabaseTemplate;
    Result := StringReplace(result, DatabaseNameMarker, DatabaseName, [rfIgnoreCase, rfReplaceAll]);
  end else begin
    raise EBold.Create('Please set the template in the SQLDatabaseConfig ' +
        'for CreateDatabaseTemplate.');
  end;
end;

function TBoldSQLDataBaseConfig.GetDatabaseExistsQuery(
  const DatabaseName: String): String;
begin
  Result := '';
  if DatabaseExistsTemplate <> '' then begin
    Result := DatabaseExistsTemplate;
    Result := StringReplace(result, DatabaseNameMarker, DatabaseName, [rfIgnoreCase, rfReplaceAll]);
  end else begin
    raise EBold.Create('Please set the template in the SQLDatabaseConfig ' +
        'for DatabaseExistsTemplate.');
  end;
end;

function TBoldSQLDataBaseConfig.GetDropDatabaseQuery(
  const DatabaseName: String): String;
begin
  Result := '';
  if DropDatabaseTemplate <> '' then begin
    Result := DropDatabaseTemplate;
    Result := StringReplace(result, DatabaseNameMarker, DatabaseName, [rfIgnoreCase, rfReplaceAll]);
  end else begin
    raise EBold.Create('Please set the template in the SQLDatabaseConfig ' +
        'for DropDatabaseTemplate.');
  end;
end;

function TBoldSQLDataBaseConfig.GetDropColumnQuery(const TableName: string; const columnName: String): String;
begin
  Result := '';
  if DropColumnTemplate <> '' then begin
    Result := DropColumnTemplate;
    Result := StringReplace(result, TableNameMarker, TableName, [rfIgnoreCase,
        rfReplaceAll]);
    Result := StringReplace(result, ColumnNameMarker, ColumnName, [rfIgnoreCase,
        rfReplaceAll]);
  end else begin
    raise EBold.Create('Please set the template in the SQLDatabaseConfig ' +
        'for DropColumnTemplate.');
  end;
end;

function TBoldSQLDataBaseConfig.GetDropIndexQuery(const TableName: string; const IndexName: String): String;
begin
  Result := '';
  if DropIndexTemplate <> '' then begin
    Result := DropIndexTemplate;
    Result := StringReplace(result, TableNameMarker, TableName, [rfIgnoreCase,
        rfReplaceAll]);
    Result := StringReplace(result, IndexNameMarker, IndexName, [rfIgnoreCase,
        rfReplaceAll]);
  end else begin
    raise EBold.Create('Please set the template in the SQLDatabaseConfig ' +
        'for DropIndexTemplate.');
  end;
end;

function TBoldSQLDataBaseConfig.GetDropTableQuery(const TableName: String): String;
begin
  Result := '';
  if DropTableTemplate <> '' then begin
    Result := DropTableTemplate;
    Result := StringReplace(result, TableNameMarker, TableName, [rfIgnoreCase,
        rfReplaceAll]);
  end else begin
    raise EBold.Create('Please set the template in the SQLDatabaseConfig ' +
        'for DropTableTemplate.');
  end;
end;


procedure TBoldSQLDataBaseConfig.SetIndexInfoTemplate(const Value: string);
begin
  if fIndexInfoTemplate <> Value then
  begin
    fIndexInfoTemplate := Value;
    Change;
  end;
end;

procedure TBoldSQLDataBaseConfig.SetIfTemplate(const Value: string);
begin
  if FIfTemplate <> Value then
  begin
    FIfTemplate := Value;
    Change;
  end;
end;

procedure TBoldSQLDataBaseConfig.SetIgnoreMissingObjects(const Value: boolean);
begin
  if fIgnoreMissingObjects <> Value then
  begin
    fIgnoreMissingObjects := Value;
    Change;
  end;
end;

procedure TBoldSQLDataBaseConfig.SetIndexColumnExistsTemplate(
  const Value: string);
begin
  if FIndexColumnExistsTemplate <> Value then
  begin
    FIndexColumnExistsTemplate := Value;
    Change;
  end;
end;

procedure TBoldSQLDataBaseConfig.SetIndexExistsTemplate(const Value: string);
begin
  if FIndexExistsTemplate <> Value then
  begin
    FIndexExistsTemplate := Value;
    Change;
  end;
end;

procedure TBoldSQLDataBaseConfig.SetInitialValues;
begin
  fDatabaseCaseSensitiveTemplate := ''; // is database specific
  fIfTemplate := ''; // is database specific
  fColumnExistsTemplate := ''; // is database specific
  fTableExistsTemplate := ''; // is database specific
  fIndexExistsTemplate := ''; // is database specific
  fIndexColumnExistsTemplate := ''; // is database specific
  fColumnTypeForBlob := 'BLOB';
  fColumnTypeForDateTime := 'DATE';
  fColumnTypeForDate := 'DATE';
  fColumnTypeForTime := 'DATE';
  fDefaultValueForDateTime := '';
  fColumnTypeForFloat := 'DOUBLE PRECISION';
  fColumnTypeForCurrency := 'DOUBLE PRECISION';
  fColumnTypeForString := 'VARCHAR(%d)';
  fColumnTypeForUnicodeString := 'NVARCHAR(%d)'; // do not localize
  fColumnTypeForAnsiString := 'VARCHAR(%d)'; // do not localize
  fColumnTypeForText := 'VARCHAR(MAX)'; // do not localize
  fColumnTypeForUnicodeText := 'NVARCHAR(MAX)'; // do not localize
  fColumnTypeForAnsiText := 'VARCHAR(MAX)'; // do not localize
  fLongStringLimit := -1;
  fColumnTypeForInteger := 'INTEGER';
  fColumnTypeForSmallInt := 'SMALLINT';
  fColumnTypeForInt64 := 'BIGINT'; // do not localize
  fDefaultStringLength := 255;
  fMaxParamsInIdList := 20;
  fMaxIndexNameLength := 18;
  fMaxDbIdentifierLength := -1;
  fMaxBatchQueryLength := cMaxBatchQueryLength;
  fMaxBatchQueryParams := cMaxBatchQueryParams;
  fBatchQueryBegin := '';
  fBatchQueryEnd := '';
  fBatchQuerySeparator := ';';
  fUseBatchQueries := false;
  fFieldTypeForBlob := ftBlob;
  fStoreEmptyStringsAsNULL := false;
  fSystemTablePrefix := 'BOLD';
  fEmptyStringMarker := '';
  fUnicodeStringPrefix := '';
  fTreatStringFieldAsUnicode := true;
  fMultiRowInsertLimit := 1;
  fUseParamsForInteger := false;
  fUseParamsForEmptyString := false;
  fIgnoreMissingObjects := false;
  fAllowMetadataChangesInTransaction := true;
  fDBGenerationMode := dbgQuery;
  fCreateDatabaseTemplate := 'CREATE DATABASE <DatabaseName>';
  fDropDatabaseTemplate := 'DROP DATABASE <DatabaseName>';
  fDatabaseExistsTemplate := '';
  fDropColumnTemplate := 'ALTER TABLE <TableName> DROP <ColumnName>';
  fDropTableTemplate := 'DROP TABLE <TableName>';
  fDropIndexTemplate := 'DROP INDEX <IndexName>';
  fEvolveDropsUnknownIndexes := true;
  fSQLforNull := 'NULL';
  fSQLforNotNull := 'NOT NULL';
  fSupportsConstraintsInCreateTable := true;
  fQuoteNonStringDefaultValues := false;
  fSupportsStringDefaultValues := true;
  fSqlScriptCommentStart := '/* ';
  fSqlScriptStartTransaction := 'START TRANSACTION';
  fSqlScriptTerminator := ';';
  fSqlScriptCommentStop := ' */';
  fSqlScriptSeparator := '';
  fSqlScriptRollBackTransaction := 'ROLLBACK';
  fSqlScriptCommitTransaction := 'COMMIT';
  fReservedWords.Text := 'ACTIVE, ADD, ALL, AFTER, ALTER'#10'AND, ANY, AS, ASC, ASCENDING,'#10 +
                         'AT, AUTO, AUTOINC, AVG, BASE_NAME'#10'BEFORE, BEGIN, BETWEEN, BLOB, BOOLEAN,'#10 +
                         'BOTH, BY, BYTES, CACHE, CAST, CHAR'#10'CHARACTER, CHECK, CHECK_POINT_LENGTH, COLLATE,'#10 +
                         'COLUMN, COMMIT, COMMITTED, COMPUTED'#10'CONDITIONAL, CONSTRAINT, CONTAINING, COUNT, CREATE, CSTRING,'#10 +
                         'CURRENT, CURSOR, DATABASE, DATE, DAY'#10'DEBUG, DEC, DECIMAL, DECLARE, DEFAULT,'#10 +
                         'DELETE, DESC, DESCENDING, DISTINCT, DO'#10'DOMAIN, DOUBLE, DROP, ELSE, END,'#10 +
                         'ENTRY_POINT, ESCAPE, EXCEPTION, EXECUTE'#10'EXISTS, EXIT, EXTERNAL, EXTRACT, FILE, FILTER,'#10 +
                         'FLOAT, FOR, FOREIGN, FROM, FULL, FUNCTION'#10'GDSCODE, GENERATOR, GEN_ID, GRANT,'#10 +
                         'GROUP, GROUP_COMMIT_WAIT_TIME, HAVING'#10'HOUR, IF, IN, INT, INACTIVE, INDEX, INNER,'#10 +
                         'INPUT_TYPE, INSERT, INTEGER, INTO'#10'IS, ISOLATION, JOIN, KEY, LONG, LENGTH,'#10 +
                         'LOGFILE, LOWER, LEADING, LEFT, LEVEL'#10'LIKE, LOG_BUFFER_SIZE, MANUAL, MAX, MAXIMUM_SEGMENT,'#10 +
                         'MERGE, MESSAGE, MIN, MINUTE, MODULE_NAME'#10'MONEY, MONTH, NAMES, NATIONAL, NATURAL,'#10 +
                         'NCHAR, NO, NOT, NULL, NUM_LOG_BUFFERS'#10'NUMERIC, OF, ON, ONLY, OPTION,'#10 +
                         'OR, ORDER, OUTER, OUTPUT_TYPE, OVERFLOW'#10'PAGE_SIZE, PAGE, PAGES, PARAMETER, PASSWORD,'#10 +
                         'PLAN, POSITION, POST_EVENT, PRECISION'#10'PROCEDURE, PROTECTED, PRIMARY, PRIVILEGES, RAW_PARTITIONS, RDB$DB_KEY,'#10 +
                         'READ, REAL, RECORD_VERSION, REFERENCES'#10'RESERV, RESERVING, RETAIN, RETURNING_VALUES, RETURNS, REVOKE,'#10 +
                         'RIGHT, ROLE, ROLLBACK, SECOND, SEGMENT'#10'SELECT, SET, SHARED, SHADOW, SCHEMA, SINGULAR,'#10 +
                         'SIZE, SMALLINT, SNAPSHOT, SOME, SORT'#10'SQLCODE, STABILITY, STARTING, STARTS, STATISTICS,'#10 +
                         'SUB_TYPE, SUBSTRING, SUM, SUSPEND, TABLE'#10'THEN, TIME, TIMESTAMP, TIMEZONE_HOUR, TIMEZONE_MINUTE,'#10 +
                         'TO, TRAILING, TRANSACTION, TRIGGER, TRIM'#10'UNCOMMITTED, UNION, UNIQUE, UPDATE, UPPER,'#10 +
                         'USER, VALUE, VALUES, VARCHAR, VARIABLE'#10'VARYING, VIEW, WAIT, WHEN, WHERE,'#10 +
                         'WHILE, WITH, WORK, WRITE, YEAR';

  Change;
end;

procedure TBoldSQLDataBaseConfig.SetLongStringLimit(Value: integer);
begin
  if FLongStringLimit <> Value then
  begin
    FLongStringLimit := Value;
    Change;
  end;
end;

procedure TBoldSQLDataBaseConfig.SetSingleIndexOrderedLinks(const Value: boolean);
begin
  if fSingleIndexOrderedLinks <> Value then
  begin
    fSingleIndexOrderedLinks := Value;
    Change;
  end;
end;

procedure TBoldSQLDataBaseConfig.SetUnicodeStringPrefix(const Value: string);
begin
  if fUnicodeStringPrefix <> Value then
  begin
    fUnicodeStringPrefix := Value;
    Change;
  end;
end;

procedure TBoldSQLDataBaseConfig.SetUseBatchQueries(const Value: boolean);
begin
  if fUseBatchQueries <> Value then
  begin
    fUseBatchQueries := Value;
    Change;
  end;
end;

procedure TBoldSQLDataBaseConfig.SetUseParamsForEmptyString(
  const Value: boolean);
begin
  if fUseParamsForEmptyString <> Value then
  begin
    fUseParamsForEmptyString := Value;
    Change;
  end;
end;

procedure TBoldSQLDataBaseConfig.SetUseParamsForInteger(const Value: boolean);
begin
  if fUseParamsForInteger <> Value then
  begin
    fUseParamsForInteger := Value;
    Change;
  end;
end;

procedure TBoldSQLDataBaseConfig.SetMultiRowInsertLimit(const Value: integer);
begin
  if fMultiRowInsertLimit <> Value then
  begin
    fMultiRowInsertLimit := Value;
    Change;
  end;
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
  if Engine <> dbeUnknown then
    SetInitialValues;
  case Engine of
    dbePostgres:
    begin
      fColumnTypeForBlob := 'BYTEA'; // do not localize
      fColumnTypeForCurrency := 'NUMERIC'; // do not localize
      fColumnTypeForDateTime := 'TIMESTAMP'; // do not localize
      fColumnTypeForFloat := 'NUMERIC'; // do not localize
      FColumnTypeForTime := 'TIME'; // do not localize
      fColumnTypeForGuid := 'UUID';
      fMaxIndexNameLength := 63;
      fMaxDbIdentifierLength := 63;
      fMultiRowInsertLimit := 1000;
      fIndexColumnExistsTemplate := 'select indexname name from pg_indexes where upper(tablename) = upper(''<TableName>'')';
      FColumnExistsTemplate := 'SELECT column_name FROM information_schema.columns WHERE upper(table_name)=upper(''<TableName>'') and upper(column_name)=upper(''<ColumnName>'')'; // do not localize
      fDatabaseExistsTemplate := 'select exists(SELECT datname FROM pg_catalog.pg_database WHERE lower(datname) = lower(''<DatabaseName>''));';
//      IndexInfoTemplate fields: IndexName, IsPrimary, IsUnique, ColumnName
      IndexInfoTemplate :=  'SELECT ix.relname IndexName, indisunique isUnique, indisprimary isPrimary, '+
            '  regexp_replace(pg_get_indexdef(indexrelid), '#39'.*\((.*)\)'#39', '#39'\1'#39
+            ') columnName '
+          'FROM pg_index i '
+          'JOIN pg_class t ON t.oid = i.indrelid '
+          'JOIN pg_class ix ON ix.oid = i.indexrelid '
+          'WHERE t.relname = (''<TableName>'')';


      fReservedWords.Text := 'ALL, ANALYSE, AND, ANY, ARRAY, AS, ASC, ASYMMETRIC, AUTHORIZATION,'#10 + // do not localize
                             'BETWEEN, BINARY, BOOLEAN, BOTH, CASE, CAST, CHAR, CHARACTER, CHECK,'#10 + // do not localize
                             'CMIN, COALESCE, COLLATE, COLUMN, CONSTRAINT, CONVERT, CREATE, CROSS,'#10 + // do not localize
                             'CURRENT_DATE, CURRENT_ROLE, CURRENT_TIME, CURRENT_TIMESTAMP,'#10 + // do not localize
                             'CURRENT_USER, DEC, DECIMAL, DEFAULT, DEFERRABLE, SEC, ELSE, END,'#10 + // do not localize
                             'EXCEPT, EXISTS, EXTRACT, FALSE, FLOAT, FOR, FOREIGN, FREEZE, FROM,'#10 + // do not localize
                             'FULL, GRANT, GREATEST, GROUP, HAVING, ILIKE, IN, INITIALLY, INNER,'#10 + // do not localize
                             'INOUT, INT, INTEGER, INTERSECT, INTERVAL, INTO, IS, ISNULL, JOIN,'#10 + // do not localize
                             'LEADING, LEAST, LEFT, LIKE, LIMIT, LOCALTIME, LOCALTIMESTAMP,'#10 + // do not localize
                             'NATIONAL, NATURAL, NCHAR, NEW, NONE, NOT, NOTNULL, NULL, NULLIF,'#10 + // do not localize
                             'NUMERIC, OFF, OFFSET, OLD, ON, ONLY, OR, ORDER, OUT, OUTER, OVERLAPS,'#10 + // do not localize
                             'OVERLAY, PLI, POSITION, PRECISION, PRIMARY, REAL, REFERENCES,'#10 + // do not localize
                             'RETURNING, RIGHT, ROW, SELECT, SESSION_USER, SETOF, SIMILAR,'#10 + // do not localize
                             'SMALLINT, SOME, SUBSTRING, SYMMETRIC, TABLE, THEN, TIME, TIMESTAMP,'#10 + // do not localize
                             'TOP_LEVEL_COUNT, TRAILING, TREAT, TRIM, TRUE, UNION, UNIQUE, USER,'#10 + // do not localize
                             'USING, VALUES, VARCHAR, VERBOSE, WHEN, WHERE'; // do not localize


    end;
    dbeSQLServer:
    begin
      FDatabaseCaseSensitiveTemplate := 'EXECUTE sp_helpsort'; // do not localize
      FIfTemplate := 'IF <Condition> BEGIN <SQLStatement> END'; // do not localize
      FColumnExistsTemplate := 'SELECT * FROM SYS.COLUMNS WHERE UPPER(NAME) = UPPER(N''<ColumnName>'') AND OBJECT_ID = OBJECT_ID(UPPER(N''<TableName>''))'; // do not localize
      FTableExistsTemplate := 'SELECT * FROM SYS.TABLES WHERE UPPER(NAME)=UPPER(''<TableName>'')'; // do not localize
      FIndexExistsTemplate := 'SELECT NAME FROM SYS.INDEXES WHERE UPPER(NAME)=UPPER(''<IndexName>'') AND OBJECT_ID = OBJECT_ID(UPPER(N''<TableName>''))'; // do not localize
      FIndexColumnExistsTemplate :=
         'SELECT IND.NAME FROM SYS.INDEXES IND INNER'  // do not localize
        +' JOIN SYS.INDEX_COLUMNS IC ON  IND.OBJECT_ID = IC.OBJECT_ID AND'  // do not localize
        +' IND.INDEX_ID = IC.INDEX_ID INNER JOIN SYS.COLUMNS COL ON'  // do not localize
        +' IC.OBJECT_ID = COL.OBJECT_ID AND IC.COLUMN_ID = COL.COLUMN_ID'  // do not localize
        +' WHERE IND.OBJECT_ID = OBJECT_ID(UPPER(N''<TableName>'')) AND UPPER(COL.NAME) = UPPER(''<IndexColumnName>'')';  // do not localize
      fColumnTypeForDate := 'DATETIME';  // do not localize
      fColumnTypeForTime := 'DATETIME';  // do not localize
      fColumnTypeForDateTime := 'DATETIME';  // do not localize
      fCreateDatabaseTemplate := 'USE MASTER;GO;CREATE DATABASE <DatabaseName>';
      fDatabaseExistsTemplate := 'SELECT name FROM master.sys.databases WHERE name = N''<DatabaseName>''';
      fColumnTypeForFloat := 'DECIMAL (28,10)';  // do not localize
      fColumnTypeForCurrency := 'DECIMAL (28,10)';  // do not localize
      fColumnTypeForText := 'VARCHAR(MAX)'; // do not localize
      fColumnTypeForUnicodeText := 'NVARCHAR(MAX)'; // do not localize
      FColumnTypeForBlob := 'VARBINARY(MAX)'; // do not localize
      fColumnTypeForInt64 := 'BIGINT'; // do not localize
      fColumnTypeForGuid := 'UNIQUEIDENTIFIER';  // do not localize
      MaxDbIdentifierLength := 128;
      fMaxIndexNameLength := 128;
      fLongStringLimit := 4000;
      fMaxBatchQueryLength := 65536 * 1024; // Length of a string containing SQL statements (batch size) 65,536 * Network packet size Default packet size is 4096 bytes
      fMaxBatchQueryParams := 2000;
      fMultiRowInsertLimit := 1000;
      fSqlScriptStartTransaction := 'BEGIN TRANSACTION';
      fDropColumnTemplate :=
          'DECLARE @CONSTRAINTNAME NVARCHAR(200)'  // do not localize
         +' SELECT @CONSTRAINTNAME=OD.NAME'  // do not localize
         +'   FROM   SYSOBJECTS OT, SYSCOLUMNS C, SYSOBJECTS OD'  // do not localize
         +'   WHERE  UPPER(OT.NAME) = UPPER(''<TableName>'')'  // do not localize
         +'   AND  OT.ID          = C.ID'  // do not localize
         +'   AND  UPPER(C.NAME)  = UPPER(''<ColumnName>'')'  // do not localize
         +'   AND  C.CDEFAULT     = OD.ID'  // do not localize
         +' IF @CONSTRAINTNAME IS NOT NULL'  // do not localize
         +'   EXEC(''ALTER TABLE <TableName> DROP CONSTRAINT '' + @CONSTRAINTNAME)'  // do not localize
         +' IF EXISTS (SELECT * FROM SYSCOLUMNS WHERE ID=OBJECT_ID(''<TableName>'') AND UPPER(NAME)=UPPER(''<ColumnName>''))'  // do not localize
         +' EXEC(''ALTER TABLE <TableName> DROP COLUMN <ColumnName>'')';  // do not localize
      fDropIndexTemplate := 'DROP INDEX <TableName>.<IndexName>';  // do not localize
      fIndexInfoTemplate:=
        'SELECT IND.NAME INDEXNAME, IND.IS_PRIMARY_KEY ISPRIMARY, IND.IS_UNIQUE ISUNIQUE, COL.NAME COLUMNNAME FROM'      // do not localize
        +' SYS.INDEXES IND INNER JOIN SYS.INDEX_COLUMNS IC ON IND.OBJECT_ID = IC.OBJECT_ID AND IND.INDEX_ID = IC.INDEX_ID'  // do not localize
        +' INNER JOIN SYS.COLUMNS COL ON IC.OBJECT_ID = COL.OBJECT_ID AND IC.COLUMN_ID = COL.COLUMN_ID'      // do not localize
        +' WHERE UPPER(OBJECT_NAME(IND.OBJECT_ID))=UPPER(''<TableName>'')'  // do not localize
        +' ORDER BY INDEXNAME, INDEX_COLUMN_ID';  // do not localize
    end;
    dbeGenericANSISQL92:
    begin
      fColumnTypeForDate := 'DATE';
      fColumnTypeForTime := 'TIME';
      fColumnTypeForDateTime := 'TIMESTAMP';
    end;
    dbeInterbaseSQLDialect3:
    begin
      fColumnTypeForDate := 'TIMESTAMP';  // do not localize
      fColumnTypeForTime := 'TIMESTAMP';  // do not localize
      fColumnTypeForDateTime := 'TIMESTAMP';  // do not localize
      fMaxIndexNameLength := 31;
      fMaxDbIdentifierLength := 31;
      fAllowMetadataChangesInTransaction := true;
      fColumnTypeForInt64:='INT64';  // do not localize
      fColumnTypeForText:='VARCHAR(32765)';  // do not localize
      fColumnTypeForUnicodeString:='VARCHAR(%d) CHARACTER SET UNICODE';  // do not localize
      fColumnTypeForUnicodeText:='VARCHAR(4000) CHARACTER SET UNICODE';  // do not localize
      fColumnTypeForAnsiText:='VARCHAR(32765)';  // do not localize
      fIfTemplate:='EXECUTE BLOCK AS BEGIN IF (<Condition>) THEN EXECUTE STATEMENT ''<SQLStatement>''; END';  // do not localize
      fIndexColumnExistsTemplate:=
          'SELECT IX.RDB$INDEX_NAME AS Name FROM RDB$INDICES IX, RDB$INDEX_SEGMENTS SG WHERE IX.RDB$INDEX_NAME = SG.RDB$INDEX_NAME AND '  // do not localize
          +' UPPER(SG.RDB$FIELD_NAME)=UPPER(''<IndexColumnName>'') AND UPPER(IX.RDB$RELATION_NAME)=UPPER(''<TableName>'')';  // do not localize
      fIndexExistsTemplate:=
          'SELECT * FROM RDB$INDICES WHERE UPPER(RDB$INDEX_NAME) = UPPER(''<IndexName>'')';  // do not localize
      fTableExistsTemplate:=
          'SELECT * FROM RDB$RELATIONS WHERE UPPER(RDB$RELATION_NAME) = UPPER(''<TableName>'')';  // do not localize
      fColumnExistsTemplate:=
          'SELECT * FROM RDB$RELATION_FIELDS WHERE UPPER(RDB$RELATION_NAME)='  // do not localize
          +'UPPER(''<TableName>'') AND UPPER(RDB$FIELD_NAME)=UPPER(''<ColumnName>'')';        // do not localize
      fIndexInfoTemplate := 'select ix.rdb$index_name INDEXNAME, sg.rdb$field_name COLUMNNAME,' + // do not localize
                           'case (ix.rdb$unique_flag) when 1 then ''T'' else ''F'' end isunique,' +
                           'case(rc.rdb$constraint_type) when ''PRIMARY KEY'' then ''T'' else ''F'' end isprimary ' +
                           'from rdb$indices ix ' +
                           'left join rdb$relation_constraints rc on rc.rdb$index_name = ix.rdb$index_name ' +
                           'left join rdb$index_segments sg on ix.rdb$index_name = sg.rdb$index_name where Upper(ix.rdb$relation_name)=Upper(''<TableName>'') '+
                           'order by ix.rdb$index_name, sg.rdb$field_position';
                           ;

    end;
    dbeInterbaseSQLDialect1:
    begin
      fMaxIndexNameLength := 31;
      fMaxDbIdentifierLength := 31;
      fAllowMetadataChangesInTransaction := true;
      fColumnTypeForInt64:='INT64';  // do not localize
      fColumnTypeForText:='VARCHAR(32765)';  // do not localize
      fColumnTypeForUnicodeString:='VARCHAR(%d) CHARACTER SET UNICODE';  // do not localize
      fColumnTypeForUnicodeText:='VARCHAR(4000) CHARACTER SET UNICODE';  // do not localize
      fColumnTypeForAnsiText:='VARCHAR(32765)';  // do not localize
      fIfTemplate:='EXECUTE BLOCK AS BEGIN IF (<Condition>) THEN EXECUTE STATEMENT ''<SQLStatement>''; END';  // do not localize
      fIndexColumnExistsTemplate:=
          'SELECT IX.RDB$INDEX_NAME AS Name FROM RDB$INDICES IX, RDB$INDEX_SEGMENTS SG WHERE IX.RDB$INDEX_NAME = SG.RDB$INDEX_NAME AND '  // do not localize
          +' UPPER(SG.RDB$FIELD_NAME)=UPPER(''<IndexColumnName>'') AND UPPER(IX.RDB$RELATION_NAME)=UPPER(''<TableName>'')';  // do not localize
      fIndexExistsTemplate:=
          'SELECT * FROM RDB$INDICES WHERE UPPER(RDB$INDEX_NAME) = UPPER(''<IndexName>'')';  // do not localize
      fTableExistsTemplate:=
          'SELECT * FROM RDB$RELATIONS WHERE UPPER(RDB$RELATION_NAME) = UPPER(''<TableName>'')';  // do not localize
      fColumnExistsTemplate:=
          'SELECT * FROM RDB$RELATION_FIELDS WHERE UPPER(RDB$RELATION_NAME)='  // do not localize
          +'UPPER(''<TableName>'') AND UPPER(RDB$FIELD_NAME)=UPPER(''<ColumnName>'')';        // do not localize
      fIndexInfoTemplate := 'select ix.rdb$index_name INDEXNAME, sg.rdb$field_name COLUMNNAME,' + // do not localize
                           'case (ix.rdb$unique_flag) when 1 then ''T'' else ''F'' end isunique,' +
                           'case(rc.rdb$constraint_type) when ''PRIMARY KEY'' then ''T'' else ''F'' end isprimary ' +
                           'from rdb$indices ix ' +
                           'left join rdb$relation_constraints rc on rc.rdb$index_name = ix.rdb$index_name ' +
                           'left join rdb$index_segments sg on ix.rdb$index_name = sg.rdb$index_name where Upper(ix.rdb$relation_name)=Upper(''<TableName>'') '+
                           'order by ix.rdb$index_name, sg.rdb$field_position';
    end;
    dbeDBISAM:
    begin
      fColumnTypeForDate := 'DATE';
      fColumnTypeForTime := 'TIME';
      fColumnTypeForDateTime := 'TIMESTAMP';
      fColumnTypeForFloat := 'FLOAT';
      fColumnTypeForCurrency := 'FLOAT';
      fDefaultStringLength := 250;
      AddReservedWord('Description');
    end;
    dbeAdvantage:
    begin
      fSQLforNotNull := 'CONSTRAINT NOT NULL';
      fColumnTypeForSmallInt := 'SHORT';
      fColumnTypeForFloat := 'NUMERIC';
      fColumnTypeForCurrency := 'NUMERIC';
      fSupportsConstraintsInCreateTable := false;
      fQuoteNonStringDefaultValues := true;
      fSupportsStringDefaultValues := false;
      fColumnTypeForDate := 'DATE';
      fColumnTypeForTime := 'TIME';
      fColumnTypeForDateTime := 'TIMESTAMP';
      fColumnTypeForString := 'CHAR(%d)';
      fStoreEmptyStringsAsNULL := True;
    end;
    dbeOracle:
    begin
      FColumnTypeForString := 'VARCHAR2(%d)';  // do not localize
      FColumnTypeForFloat := 'NUMBER';  // do not localize
      FColumnTypeForCurrency := 'NUMBER(10,2)';  // do not localize
      fColumnTypeForText:='CLOB';  // do not localize
      fColumnTypeForUnicodeString:='NVARCHAR2(%d)';  // do not localize
      fColumnTypeForUnicodeText:='CLOB';  // do not localize
      fColumnTypeForAnsiText:='CLOB';  // do not localize
      fMaxIndexNameLength := 30;
      fMaxDbIdentifierLength := 30;
      fSupportsStringDefaultValues:=False;
      fIfTemplate:=
          'DECLARE V_COUNT INTEGER; BEGIN SELECT CASE WHEN (<Condition>) THEN 1 ELSE 0 END CASE1 INTO V_COUNT FROM DUAL;'  // do not localize
          +' IF (V_COUNT=1) THEN EXECUTE IMMEDIATE ''<SQLStatement>''; END IF;END;';  // do not localize
      fIndexColumnExistsTemplate:=
          'SELECT INDEX_NAME AS NAME FROM USER_IND_COLUMNS WHERE UPPER(COLUMN_NAME)=UPPER(''<IndexColumnName>'')'  // do not localize
          +' AND UPPER(TABLE_NAME)=UPPER(''<TableName>'')';  // do not localize
      fIndexExistsTemplate:=
          'SELECT * FROM USER_INDEXES WHERE UPPER(INDEX_NAME) = UPPER(''<IndexName>'') AND GENERATED = ''N''';  // do not localize
      fTableExistsTemplate:=
          'SELECT * FROM USER_TABLES WHERE UPPER(TABLE_NAME) = UPPER(''<TableName>'')';  // do not localize
      fColumnExistsTemplate:=
          'SELECT * FROM USER_TAB_COLUMNS WHERE UPPER(TABLE_NAME) = UPPER(''<TableName>'')'  // do not localize
          +' AND UPPER(COLUMN_NAME) = UPPER(''<ColumnName>'')';  // do not localize
      fIndexInfoTemplate:=
          'SELECT AIC.INDEX_NAME AS IndexName,'  // do not localize
          +'   CASE ALC.CONSTRAINT_TYPE'  // do not localize
          +'      WHEN ''P'' THEN ''T'''  // do not localize
          +'      ELSE ''F'''  // do not localize
          +'   END AS IsPrimary,'  // do not localize
          +'   CASE ALC.CONSTRAINT_TYPE'  // do not localize
          +'      WHEN ''U'' THEN ''T'''  // do not localize
          +'      WHEN ''P'' THEN ''T'''  // do not localize
          +'      ELSE ''F'''  // do not localize
          +'   END AS IsUnique,'  // do not localize
          +'   AIC.COLUMN_NAME AS ColumnName'  // do not localize
          +' FROM USER_IND_COLUMNS AIC'  // do not localize
          +' LEFT JOIN USER_CONSTRAINTS ALC ON AIC.INDEX_NAME = ALC.CONSTRAINT_NAME'  // do not localize
          +'  AND AIC.TABLE_NAME = ALC.TABLE_NAME'  // do not localize
          +' WHERE UPPER(AIC.TABLE_NAME) = UPPER(''<TableName>'')'  // do not localize
          +' ORDER BY IndexName, Column_Position';  // do not localize
      fDropColumnTemplate := 'ALTER TABLE <TableName> DROP COLUMN <ColumnName>';  // do not localize
      fBatchQueryBegin := 'BEGIN';
      fBatchQueryEnd := 'END;';
    end;
    dbeParadox:
    begin
      fDBGenerationMode := dbgTable;
    end;
    dbeInformix:
    begin
      fMaxIndexNameLength := 18;
      fMaxDbIdentifierLength := 18;
      fColumnTypeForCurrency := 'MONEY';
      fColumnTypeForFloat := 'NUMERIC';
      fColumnTypeForDate := 'DATETIME YEAR TO DAY';
      fColumnTypeForTime := 'DATETIME HOUR TO FRACTION';
      fColumnTypeForDateTime := 'DATETIME YEAR TO FRACTION';
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

procedure TBoldSQLDataBaseConfig.SetSQLforNull(const Value: string);
begin
  if FColumnTypeForInteger <> Value then
  begin
    fSQLforNull := Value;
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


function TBoldSQLDataBaseConfig.GetIndexInfoQuery(
  const TableName: String): String;
begin
  result := IndexInfoTemplate;
  result := StringReplace(result, '<TableName>', TableName, [rfIgnoreCase, rfReplaceAll]);
end;

procedure TBoldSQLDataBaseConfig.SetColumnTypeForSmallInt(const Value: string);
begin
  if FColumnTypeForSmallInt <> Value then
  begin
    fColumnTypeForSmallInt := Value;
    Change;
  end;
end;

procedure TBoldSQLDataBaseConfig.SetColumnTypeForInt64(const Value: string);
begin
  if fColumnTypeForInt64 <> Value then begin
    fColumnTypeForInt64 := Value;
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

procedure TBoldSQLDataBaseConfig.SetQuoteLeftBracketInLike(
  const Value: Boolean);
begin
  if fQuoteLeftBracketInLike <> Value then
  begin
    fQuoteLeftBracketInLike := Value;
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

function TBoldSQLDataBaseConfig.IsSQLServerEngine: Boolean;
begin
  Result := Engine = dbeSQLServer;
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

procedure TBoldSQLDataBaseConfig.SetMaxBatchQueryLength(const Value: integer);
begin
  if fMaxBatchQueryLength <> Value then
  begin
    fMaxBatchQueryLength := Value;
    Change;
  end;
end;

procedure TBoldSQLDataBaseConfig.SetMaxBatchQueryParams(const Value: integer);
begin
  if fMaxBatchQueryParams <> Value then
  begin
    fMaxBatchQueryParams := Value;
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
  Filer.DefineProperty('UseTransactionsDuringDBCreate', ReadUseTransactionsDuringDBCreate, nil, True);
end;

function TBoldSQLDataBaseConfig.GetColumnExistsQuery(const TableName,
    ColumnName: string): string;
begin
  Result := '';
  if ColumnExistsTemplate <> '' then begin
    Result := ColumnExistsTemplate;
    Result := StringReplace(Result, TableNameMarker, TableName, [rfIgnoreCase,
        rfReplaceAll]);
    Result := StringReplace(Result, ColumnNameMarker, ColumnName, [rfIgnoreCase,
        rfReplaceAll]);
  end else begin
    raise EBold.Create('Please set the template in the SQLDatabaseConfig ' +
        'for ColumnExistsTemplate.');
  end;
end;

function TBoldSQLDataBaseConfig.GetIfColumnNotExistsQuery(const TableName,
    ColumnName, SQLStatement: string): string;
var
  sCondition: string;
begin
  Result := '';
  if (ColumnExistsTemplate <> '') and (IfTemplate <> '') then begin
    sCondition := GetColumnExistsQuery(TableName, ColumnName);
    sCondition := Format('NOT EXISTS(%s)', [sCondition]);
    Result := IfTemplate;
    Result := StringReplace(Result, ConditionMarker, sCondition, [rfReplaceAll,
        rfIgnoreCase]);
    Result := StringReplace(Result, SQLStatementMarker, SQLStatement,
        [rfReplaceAll, rfIgnoreCase]);
  end else begin
    raise EBold.Create('Please set the templates in the SQLDatabaseConfig ' +
        'for IfTemplate and ColumnExistsTemplate.');
  end;
end;

function TBoldSQLDataBaseConfig.GetIndexColumnExistsQuery(const TableName,
    IndexColumnName: string): string;
begin
  Result := '';
  if IndexColumnExistsTemplate <> '' then begin
    Result := IndexColumnExistsTemplate;
    Result := StringReplace(Result, TableNameMarker, TableName, [rfIgnoreCase,
        rfReplaceAll]);
    Result := StringReplace(Result, IndexColumnNameMarker, IndexColumnName,
        [rfIgnoreCase, rfReplaceAll]);
  end else begin
    raise EBold.Create('Please set the template in the SQLDatabaseConfig ' +
        'for IndexColumnExistsTemplate.');
  end;
end;

function TBoldSQLDataBaseConfig.GetIndexExistsQuery(const TableName, IndexName:
    string): string;
begin
  Result := '';
  if IndexExistsTemplate <> '' then begin
    Result := IndexExistsTemplate;
    Result := StringReplace(Result, TableNameMarker, TableName, [rfIgnoreCase,
        rfReplaceAll]);
    Result := StringReplace(Result, IndexNameMarker, IndexName, [rfIgnoreCase,
        rfReplaceAll]);
  end else begin
    raise EBold.Create('Please set the template in the SQLDatabaseConfig ' +
        'for IndexExistsTemplate.');
  end;
end;

function TBoldSQLDataBaseConfig.GetTableExistsQuery(const TableName: string):
    string;
begin
  Result := '';
  if TableExistsTemplate <> '' then begin
    Result := TableExistsTemplate;
    Result := StringReplace(Result, TableNameMarker, TableName, [rfIgnoreCase,
        rfReplaceAll]);
  end else begin
    raise EBold.Create('Please set the template in the SQLDatabaseConfig ' +
        'for TableExistsTemplate.');
  end;
end;

procedure TBoldSQLDataBaseConfig.ReadUseTransactionsDuringDBCreate(Reader: TReader);
begin
  AllowMetadataChangesInTransaction := Reader.ReadBoolean;
end;

procedure TBoldSQLDataBaseConfig.SetDatabaseCaseSensitiveTemplate(const Value:
    string);
begin
  if FDatabaseCaseSensitiveTemplate <> Value then begin
    FDatabaseCaseSensitiveTemplate := Value;
    Change;
  end;
end;

procedure TBoldSQLDataBaseConfig.SetDatabaseExistsTemplate(const Value: string);
begin
  if fDatabaseExistsTemplate <> Value then
  begin
    fDatabaseExistsTemplate := Value;
    Change;
  end;
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

procedure TBoldSQLDataBaseConfig.SetEvolveDropsUnknownIndexes(const Value: boolean);
begin
  if fEvolveDropsUnknownIndexes <> Value then
  begin
    fEvolveDropsUnknownIndexes := Value;
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
    temp := 'BOLD'
  else
    temp := Value;
  if fSystemTablePrefix <> temp then
  begin
    fSystemTablePrefix := temp;
    Change;
  end;
end;

procedure TBoldSQLDataBaseConfig.SetTableExistsTemplate(const Value: string);
begin
  if FTableExistsTemplate <> Value then
  begin
    FTableExistsTemplate := Value;
    Change;
  end;
end;

procedure TBoldSQLDataBaseConfig.SetTreatStringFieldAsUnicode(
  const Value: boolean);
begin
  if fTreatStringFieldAsUnicode <> Value then
  begin
    fTreatStringFieldAsUnicode := Value;
    Change;
  end;
end;

procedure TBoldSQLDataBaseConfig.SetSqlScriptCommentStart(
  const Value: string);
begin
  if FSqlScriptCommentStart <> Value then
  begin
    FSqlScriptCommentStart := Value;
    Change;
  end;
end;

procedure TBoldSQLDataBaseConfig.SetSqlScriptCommentStop(
  const Value: string);
begin
  if FSqlScriptCommentStop <> Value then
  begin
    FSqlScriptCommentStop := Value;
    Change;
  end;
end;

procedure TBoldSQLDataBaseConfig.SetSqlScriptSeparator(
  const Value: string);
begin
  if FSqlScriptSeparator <> Value then
  begin
    FSqlScriptSeparator := Value;
    Change;
  end;
end;

procedure TBoldSQLDataBaseConfig.SetSqlScriptStartTransaction(
  const Value: string);
begin
  if FSqlScriptStartTransaction <> Value then
  begin
    FSqlScriptStartTransaction := Value;
    Change;
  end;
end;

procedure TBoldSQLDataBaseConfig.SetSqlScriptTerminator(
  const Value: string);
begin
  if FSqlScriptTerminator <> Value then
  begin
    FSqlScriptTerminator := Value;
    Change;
  end;
end;

procedure TBoldSQLDataBaseConfig.SetSqlScriptCommitTransaction(
  const Value: string);
begin
  if FSqlScriptCommitTransaction <>  Value then
  begin
    FSqlScriptCommitTransaction := Value;
    Change;
  end;
end;

procedure TBoldSQLDataBaseConfig.SetSqlScriptRollBackTransaction(
  const Value: string);
begin
  if FSqlScriptRollBackTransaction <> Value then
  begin
    FSqlScriptRollBackTransaction := Value;
    Change;
  end;
end;

end.
