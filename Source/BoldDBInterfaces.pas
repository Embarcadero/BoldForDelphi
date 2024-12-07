{ Global compiler directives }
{$include bold.inc}
unit BoldDBInterfaces;

interface

uses
  {$IFNDEF BOLD_UNICODE}
  StringBuilder,
  {$ENDIF}
  Classes,
  Db,
  SysUtils,
  BoldBase,
  BoldSQLDatabaseConfig,
  WideStrings,
  BoldLogHandler,
  BoldDefs,
  Variants;

const
  cInitialBatchBufferSize = 1024*64; // 64 kb
type
  IBoldQuery = interface;
  IBoldDataBase = interface;
  IBoldTable = interface;
  IBoldField = interface;
  IBoldParameter = interface;
  IBoldParameterized = interface;
  TBoldDataSetWrapper = class;
  TBoldDatabaseWrapper = class;
  TBoldFieldWrapper = class;
  TBoldAbstractQueryWrapper = class;

  TBoldFieldWrapperClass = class of TBoldFieldWrapper;

  TBoldGetDatabaseEvent = function: IBoldDatabase of object;

  {$IFDEF BOLD_UNICODE}
  TBoldBlobData = AnsiString;
  {$ELSE}
  TBoldBlobData = TBlobData;
  {$ENDIF}

  TBoldIndexDescription = record
    IndexName: String;
    IndexedColumns: String; // separated by ;
    IsPrimary: Boolean;
    IsUnique: Boolean;
  end;

  TBoldIndexDescriptionArray = array of TBoldIndexDescription;

  IBoldField = interface
  ['{F4126F4C-F1B2-472B-B53F-2ECEC8EE9253}']
    function GetField: TField;
    function GetFieldName: String;
    function GetAsVariant: Variant;
    procedure SetAsVariant(const Value: Variant);
    function GetAsString: String;
    procedure SetAsString(const Value: string);
    function GetAsAnsiString: TBoldAnsiString;
    procedure SetAsAnsiString(const Value: TBoldAnsiString);
    function GetAsWideString: TBoldUnicodeString;
    procedure SetAsWideString(const Value: TBoldUnicodeString);
    function GetAsInteger: Integer;
    procedure SetAsInteger(const Value: Integer);
    function GetAsBoolean: Boolean;
    function GetAsCurrency: Currency;
    function GetAsDateTime: TDateTime;
    function GetAsFloat: Double;
    function GetIsNull: Boolean;
    procedure SetAsBoolean(const Value: Boolean);
    procedure SetAsCurrency(const Value: Currency);
    procedure SetAsDateTime(const Value: TDateTime);
    procedure SetAsFloat(const Value: Double);
    function GetAsDate: TDateTime;
    function GetAsTime: TDateTime;
    procedure SetAsTime(const Value: TDateTime);
    procedure SetAsDate(const Value: TDateTime);
    procedure SetAsBlob(const Value: TBoldAnsiString);
    function GetAsBlob: TBoldAnsiString;
    function GetAsInt64: Int64;
    procedure SetAsInt64(const Value: Int64);

    property Field: TField read GetField;
    property AsVariant: Variant read GetAsVariant write SetAsVariant;
    property Value: Variant read GetAsVariant write SetAsVariant;
    property AsString: String read GetAsString write SetAsString;
    property AsAnsiString: TBoldAnsiString read GetAsAnsiString write SetAsAnsiString;
    property AsWideString: TBoldUnicodeString read GetAsWideString write SetAsWideString;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsCurrency: Currency read GetAsCurrency write SetAsCurrency;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsDate: TDateTime read GetAsDate write SetAsDate;
    property AsTime: TDateTime read GetAsTime write SetAsTime;
    property AsBlob: TBoldAnsiString read GetAsBlob write SetAsBlob;
    property AsInt64: Int64 read GetAsInt64 write SetAsInt64;
    property IsNull: Boolean read GetIsNull;
    property FieldName: String read GetFieldName;
  end;

  IBoldDBParam = interface
    ['{FB3D383D-2F7E-49DC-9834-40ABDCAA3445}']
    // some libraries use non TParam descendant implementations, for those that use TParam, this interface can be implemented
    function GetParameter: TParam;
    property Parameter: TParam read GetParameter;
  end;

  IBoldParameter = interface
  ['{FFAD2670-423A-11D3-89F6-006008F62CFF}']
    function GetAsVariant: Variant;
    procedure SetAsVariant(const NewValue: Variant);
    function GetName: String;
    procedure Clear;
    function GetDataType: TFieldType;
    procedure SetDataType(Value: TFieldType);
    function GetAsBCD: Currency;
    function GetAsBlob: TBoldBlobData;
    function GetAsBoolean: Boolean;
    function GetAsDateTime: TDateTime;
    function GetAsCurrency: Currency;
    function GetAsFloat: Double;
    function GetAsInteger: Longint;
    function GetAsMemo: string;
    function GetAsString: string;
    function GetAsAnsiString: TBoldAnsiString;
    function GetAsInt64: Int64;
    function GetAsWideString: WideString;
    function GetIsNull: Boolean;
    procedure SetAsBCD(const Value: Currency);
    procedure SetAsBlob(const Value: TBoldBlobData);
    procedure SetAsBoolean(Value: Boolean);
    procedure SetAsCurrency(const Value: Currency);
    procedure SetAsDate(const Value: TDateTime);
    procedure SetAsDateTime(const Value: TDateTime);
    procedure SetAsFloat(const Value: Double);
    procedure SetAsInteger(Value: Longint);
    procedure SetAsMemo(const Value: string);
    procedure SetAsString(const Value: string);
    procedure SetAsAnsiString(const Value: TBoldAnsiString);
    procedure SetAsWideString(const Value: Widestring);
    procedure SetAsSmallInt(Value: LongInt);
    procedure SetAsTime(const Value: TDateTime);
    procedure SetAsWord(Value: LongInt);
    procedure SetAsInt64(const Value: Int64);
    procedure SetText(const Value: string);
    procedure AssignFieldValue(const source: IBoldField);
    procedure Assign(const source: IBoldParameter);
    property AsVariant: Variant read GetAsVariant write SetAsVariant;
    property Name: String read GetName;
    property DataType: TFieldType read GetDataType write SetDataType;
    property AsBCD: Currency read GetAsBCD write SetAsBCD;
    property AsBlob: TBoldBlobData read GetAsBlob write SetAsBlob;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsCurrency: Currency read GetAsCurrency write SetAsCurrency;
    property AsDate: TDateTime read GetAsDateTime write SetAsDate;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsInteger: LongInt read GetAsInteger write SetAsInteger;
    property AsInt64: Int64 read GetAsInt64 write SetAsInt64;
    property AsSmallInt: LongInt read GetAsInteger write SetAsSmallInt;
    property AsMemo: string read GetAsMemo write SetAsMemo;
    property AsString: string read GetAsString write SetAsString;
    property AsAnsiString: TBoldAnsiString read GetAsAnsiString write SetAsAnsiString;
    property AsTime: TDateTime read GetAsDateTime write SetAsTime;
    property AsWord: LongInt read GetAsInteger write SetAsWord;
    property AsWideString: WideString read GetAsWideString write SetAsWideString;
    property IsNull: Boolean read GetIsNull;
    property Text: string read GetAsString write SetText;
  end;

  IBoldDataSet = interface
  ['{D00CA0A0-41CE-11D3-89F5-006008F62CFF}']
    procedure Append;
    procedure Close;
    function FieldByUpperCaseName(const FieldName: string): IBoldField;
    function FieldByName(const FieldName: string): IBoldField;
    function FindField(const FieldName: string): IBoldField;
    procedure First;
    function GetDataSet: TDataSet;
    function GetEof: Boolean;
    function GetFieldDefs: TFieldDefs;
    function GetFields(Index: integer): IBoldField;
    function GetFieldCount: integer;
    function GetFieldValue(const FieldName: string): Variant;
    procedure Next;
    procedure Open;
    procedure Edit;
    procedure Delete;
    function MoveBy(Distance: integer): integer;
    function Locate(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions): Boolean;
    function GetImplementor: TObject;
    function GetState: TDataSetState;

    procedure Post;
    procedure SetFieldValue(const FieldName: string; const Value: Variant);
    property AsDataSet: TDataSet read GetDataSet;
    property Eof: Boolean read GetEof;
    property FieldDefs: TFieldDefs read GetFieldDefs;
    property Fields[Index: integer]: IBoldField read GetFields;
    property FieldCount: integer read GetFieldCount;
    property FieldValues[const FieldName: string]: Variant read GetFieldValue write SetFieldValue; default;
    property Implementor: TObject read GetImplementor;
    property State: TDataSetState read GetState;
  end;

  IBoldParameterized = interface
  ['{B9020CF8-0300-4476-9453-4A3760E13225}']
    function GetParamCheck: Boolean;
    procedure SetParamCheck(value: Boolean);
    procedure ClearParams;
    procedure AssignParams(Params: TParams);
    function ParamByName(const Value: string): IBoldParameter;
    function FindParam(const Value: string): IBoldParameter;
    function EnsureParamByName(const Value: string): IBoldParameter;
    function GetParamCount: integer;
    function GetParam(i:integer): IBoldParameter;
    function CreateParam(FldType: TFieldType; const ParamName: string): IBoldParameter; overload;
    function Createparam(FldType: TFieldType; const ParamName: string; ParamType: TParamType; Size: integer): IBoldParameter; overload;
    function GetParams: TParams;
    property Param[i: integer]: IBoldParameter read GetParam;
    property ParamCount: integer read GetParamCount;
    property ParamCheck: Boolean read GetParamCheck write SetParamCheck;
    property Params: TParams read GetParams;
  end;

  IBoldExecQuery = interface(IBoldParameterized)
  ['{219D1FF1-F509-42A3-96E9-D7F62C28C1EA}']
    function GetSQLText: String;
    procedure StartSQLBatch;
    procedure EndSQLBatch;
    procedure FailSQLBatch;
    procedure ExecSQL;
    procedure AssignSQL(SQL: TStrings);
    procedure AssignSQLText(const SQL: String);
    function GetRowsAffected: integer;
    function GetImplementor: TObject;
    function GetUseReadTransactions: boolean;
    procedure SetUseReadTransactions(value: boolean);
    function GetSQLStrings: TStrings;
    function GetBatchQueryParamCount: integer;
    procedure Prepare;
    property RowsAffected: integer read GetRowsAffected;
    property Implementor: TObject read GetImplementor;
    property SQLText: String read GetSQLText write AssignSQLText;
    property SQLStrings: TStrings read GetSQLStrings;
    property UseReadTransactions: boolean read GetUseReadTransactions write SetUseReadTransactions;
    property BatchQueryParamCount: integer read GetBatchQueryParamCount;
  end;

  IBoldQuery = interface(IBoldDataSet)
  ['{D00CA0A1-41CE-11D3-89F5-006008F62CFF}']
    function GetSQLText: String;
    function GetRequestLiveQuery: Boolean;
    procedure SetRequestLiveQuery(NewValue: Boolean);
    function GetRecordCount: integer;
    procedure AssignSQLText(const SQL: String);
    procedure AssignSQL(SQL: TStrings);
    procedure ClearParams;
    procedure AssignParams(Params: TParams);
    function ParamByName(const Value: string): IBoldParameter;
    function FindParam(const Value: string): IBoldParameter;
    function GetUseReadTransactions: boolean;
    procedure SetUseReadTransactions(value: boolean);
    function GetRecNo: integer;
    procedure Prepare;
    property RequestLiveQuery: Boolean read GetRequestLiveQuery write SetRequestLiveQuery;
    property RecordCount: integer read GetRecordCount;
    property SQLText: String read GetSQLText write AssignSQLText;
    property UseReadTransactions: boolean read GetUseReadTransactions write SetUseReadTransactions;
    property RecNo: integer read GetRecNo;
  end;

  IBoldTable = interface(IBoldDataSet)
  ['{D6698E80-41CE-11D3-89F5-006008F62CFF}']
    procedure AddIndex(const Name, Fields: string; Options: TIndexOptions; const DescFields: string = '');
    function GetIndexDefs: TIndexDefs;
    procedure SetTableName(const NewName: String);
    function GetTableName: String;
    procedure SetExclusive(NewValue: Boolean);
    function GetExclusive: Boolean;
    procedure CreateTable;
    procedure DeleteTable;
    function GetExists: Boolean;

    property IndexDefs: TIndexDefs read GetIndexDefs;
    property Tablename: String read GetTableName write SetTableName;
    property Exclusive: Boolean read GetExclusive write SetExclusive;
    property Exists: Boolean read GetExists;
  end;

  IBoldDataBase = interface
  ['{DF60BA20-41CE-11D3-89F5-006008F62CFF}']
    procedure StartTransaction;
    procedure Commit;
    procedure RollBack;
    procedure Open;
    procedure Close;
    procedure Reconnect;
    function GetInTransaction: Boolean;
    function GetIsSQLBased: Boolean;
    procedure AllTableNames(Pattern: String; SystemTables: Boolean; TableNameList: TStrings);
    procedure SetlogInPrompt(NewValue: Boolean);
    function GetLogInPrompt: Boolean;
    procedure SetKeepConnection(NewValue: Boolean);
    function GetKeepConnection: Boolean;
    function GetConnected: Boolean;
    function SupportsDefaultColumnValues: Boolean;
    property InTransaction: Boolean read GetInTransaction;
    property IsSqlBased: Boolean read GetIsSQLBased;
    property LogInPrompt: Boolean read GetLogInPrompt write SetLogInPrompt;
    property KeepConnection: Boolean read GetKeepConnection write SetKeepConnection;
    property Connected: Boolean read GetConnected;
    function GetQuery: IBoldQuery;
    procedure ReleaseQuery(var Query: IBoldQuery);
    function GetExecQuery: IBoldExecQuery;
    procedure ReleaseExecQuery(var Query: IBoldExecQuery);
    function GetTable: IBoldTable;
    procedure ReleaseTable(var Table: IBoldTable);
    function SupportsTableCreation: Boolean;
    procedure ReleaseCachedObjects;
    function TableExists(const TableName: String): Boolean;
    function GetIndexDescriptions(const TableName: String): TBoldIndexDescriptionArray;
    function GetSQLDatabaseConfig: TBoldSQLDatabaseConfig;
    property SQLDatabaseConfig: TBoldSQLDatabaseConfig read GetSQLDatabaseConfig;
    function GetIsExecutingQuery: Boolean;
    property IsExecutingQuery: Boolean read GetIsExecutingQuery;
    procedure CreateDatabase(DropExisting: boolean = true);
    procedure DropDatabase;
    function DatabaseExists: boolean;
    function CreateAnotherDatabaseConnection: IBoldDatabase;
    function GetImplementor: TObject;
    property Implementor: TObject read GetImplementor;
  end;

  TBoldParameterWrapper = class(TBoldRefCountedObject)
  private
    fDatasetWrapper: TBoldAbstractQueryWrapper;
  protected
    property DatasetWrapper: TBoldAbstractQueryWrapper read fDatasetWrapper;
  public
    constructor Create(DatasetWrapper: TBoldAbstractQueryWrapper);
  end;

  TBoldDbParameter = class(TBoldParameterWrapper, IBoldParameter)
  private
    FParameter: TParam;
    function GetAsVariant: Variant;
    procedure SetAsVariant(const NewValue: Variant);
    function GetName: String;
    procedure Clear;
    function GetDataType: TFieldType;
    procedure SetDataType(Value: TFieldType);
    function GetAsBCD: Currency;
    function GetAsBlob: TBoldBlobData;
    function GetAsBoolean: Boolean;
    function GetAsCurrency: Currency;
    function GetAsFloat: Double;
    function GetAsInteger: Longint;
    function GetAsMemo: string;
    function GetAsString: string;
    function GetAsAnsiString: TBoldAnsiString;
    function GetAsWideString: WideString;
    function GetAsInt64: Int64;
    function GetIsNull: Boolean;
    procedure SetAsBCD(const Value: Currency);
    procedure SetAsBlob(const Value: TBoldBlobData);
    procedure SetAsBoolean(Value: Boolean);
    procedure SetAsCurrency(const Value: Currency);
    procedure SetAsDate(const Value: TDateTime);
    procedure SetAsFloat(const Value: Double);
    procedure SetAsInteger(Value: Longint);
    procedure SetAsMemo(const Value: string);
    procedure SetAsString(const Value: string);
    procedure SetAsAnsiString(const Value: TBoldAnsiString);
    procedure SetAsWideString(const Value: Widestring);
    procedure SetAsSmallInt(Value: LongInt);
    procedure SetAsTime(const Value: TDateTime);
    procedure SetAsInt64(const Value: Int64);
    procedure SetAsWord(Value: LongInt);
    procedure SetText(const Value: string);
    procedure AssignFieldValue(const source: IBoldField);
    procedure Assign(const source: IBoldParameter);
    function GetParameter: TParam;
  protected
    property Parameter: TParam read GetParameter;
    procedure SetAsDateTime(const Value: TDateTime); virtual;
    function GetAsDateTime: TDateTime; virtual;
  public
    constructor Create(DbParameter: TParam; DatasetWrapper: TBoldDatasetWrapper);
    destructor Destroy; override;
  end;

  TBoldAbstractQueryWrapper = class(TBoldNonRefCountedObject)
  private
    fDatabaseWrapper: TBoldDatabaseWrapper;
  protected
    function GetImplementor: TObject; virtual;
  public
    constructor Create(DatabaseWrapper: TBoldDatabaseWrapper); virtual;
    procedure Clear; virtual;
    property DatabaseWrapper: TBoldDatabaseWrapper read fDatabaseWrapper;
  end;

  TBoldDataSetWrapper = class(TBoldAbstractQueryWrapper)
  private
    fLastUsedFieldIndex: integer;
    // Names in uppercase. May differ from what is
    // in the dataset.
    fFieldNames: array of string;
    fNamesInited: Boolean;
    fWrapper1: TBoldFieldWrapper;
    fWrapper1AsInterface: IBoldField;
    fWrapper2: TBoldFieldWrapper;
    fWrapper2AsInterface: IBoldField;
    private function GetWrappedField(Field: TField): IBoldField;
  protected
    function GetFieldWrapperClass: TBoldFieldWrapperClass; virtual;
    function GetDataSet: TDataSet; virtual; abstract;
    function GetEof: Boolean;
    function GetFields(Index: integer): IBoldField;
    function GetFieldCount: integer;
    function GetFieldValue(const FieldName: string): Variant;
    procedure SetFieldValue(const FieldName: string; const Value: Variant);
    function GetFieldDefs: TFieldDefs;
    procedure Append;
    function ParamByName(const Value: string): IBoldParameter; virtual; abstract;
    function FindParam(const Value: string): IBoldParameter; virtual; abstract;
    function CreateParam(FldType: TFieldType; const ParamName: string): IBoldParameter; overload; virtual;
    function CreateParam(FldType: TFieldType; const ParamName: string; ParamType: TParamType; Size: integer): IBoldParameter; overload; virtual;
    function EnsureParamByName(const Value: string): IBoldParameter; virtual;
    procedure Close; virtual;
    function FieldByName(const FieldName: string): IBoldField; virtual;
    function FieldByUpperCaseName(const FieldNameUpper: string): IBoldField;
    function FindField(const FieldName: string): IBoldField;
    procedure First;
    procedure Delete;
    procedure Next;
    procedure Open; virtual;
    procedure Edit;
    function MoveBy(Distance: integer): integer;
    function Locate(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions): Boolean;
    procedure Post;
    function GetState: TDataSetState;
    function GetRecNo: integer; virtual;
    procedure Reconnect;
  public
    property DataSet: TDataSet read GetDataSet;
  end;

  TBoldBatchDataSetWrapper = class(TBoldDataSetWrapper)
  private
    fBatchQuery: IBoldExecQuery;
    FInBatch: Boolean;
    fAccumulatedSQLLength: integer;
    fParamsInBeginUpdate: Boolean;
    SB: TStringBuilder;
    function GetAccumulatedSQL: TStrings;
    function GetHasCachedStatements: boolean;
    procedure ReplaceParamMarkers(sql: TStrings; const Source, Dest: IBoldExecQuery);
    procedure SetInBatch(const Value: Boolean);
  protected
    procedure StartSQLBatch; virtual;
    procedure EndSQLBatch; virtual;
    procedure FailSQLBatch; virtual;
    procedure ExecSQL; virtual; abstract;
    function GetParams: TParams; virtual; abstract;
    function GetSqlText: string; virtual; abstract;
    procedure AssignSQLText(const SQL: string); virtual; abstract;
    function GetSQLStrings: TStrings; virtual; abstract;
    function GetSQLLength: integer;
    function ParamsContainBlob: Boolean;
    function GetBatchQueryParamCount: integer;
    procedure BatchExecSQL;
    procedure ExecuteBatch;
    property AccumulatedSQL: TStrings read GetAccumulatedSQL;
    property InBatch: Boolean read FInBatch write SetInBatch;
    property HasCachedStatements: boolean read GetHasCachedStatements;
    property BatchQuery: IBoldExecQuery read fBatchQuery;
  public
    constructor Create(DatabaseWrapper: TBoldDatabaseWrapper); override;
    destructor Destroy; override;
    property SQLText: string read GetSQLText write AssignSQLText;
    property SQLStrings: TStrings read GetSQLStrings;
    property Params: TParams read GetParams;
  end;

  TBoldDatabaseWrapper = class(TBoldNonRefCountedObject)
  strict private
    fSQLDataBaseConfig: TBoldSQLDatabaseConfig;
    fAllTableNames: TStringList;
    function GetWindowsLoginName: string;
    function GetIndexDescriptionsViaTable( const TableName: String): TBoldIndexDescriptionArray;
    function GetIndexDescriptionsViaQuery( const TableName: String): TBoldIndexDescriptionArray;
  protected
    function SupportsDefaultColumnValues: Boolean; virtual;
    procedure AllTableNames(Pattern: String; SystemTables: Boolean; TableNameList: TStrings); virtual; abstract;
    function GetIndexDescriptions(const TableName: String): TBoldIndexDescriptionArray; virtual;
    function GetTable: IBoldTable; virtual; abstract;
    procedure ReleaseTable(var Table: IBoldTable); virtual; abstract;
    function TableExists(const TableName: String): Boolean;
    function GetQuery: IBoldQuery; virtual; abstract;
    procedure ReleaseQuery(var Query: IBoldQuery); virtual; abstract;
    function GetExecQuery: IBoldExecQuery; virtual;
    procedure ReleaseExecQuery(var Query: IBoldExecQuery); virtual;
    function GetSQLDatabaseConfig: TBoldSQLDatabaseConfig;
    function InternalGetDatabaseError(const aErrorType: TBoldDatabaseErrorType;
        const E: Exception; sSQL, sServer, sDatabase, sUserName: string;
        bUseWindowsAuth: Boolean): EBoldDatabaseError;
  public
    constructor Create(SQLDataBaseConfig: TBoldSQLDatabaseConfig);
    destructor Destroy; override;
    procedure CreateDatabase(DropExisting: boolean = true); virtual;
    procedure DropDatabase; virtual;
    function DatabaseExists: boolean; virtual;
    property SQLDatabaseConfig: TBoldSQLDatabaseConfig read GetSQLDatabaseConfig;
  end;

  TBoldFieldWrapper = class(TBoldRefcountedObject, IBoldField)
  private
    fSavedValue: Variant;
    fField: TField;
    fDatasetWrapper: TBoldDataSetWrapper;
    function GetField: TField;
    function GetAsVariant: Variant;
    procedure SetAsVariant(const Value: Variant);
    procedure SetAsString(const Value: String);
    function GetAsAnsiString: TBoldAnsiString;
    procedure SetAsAnsiString(const Value: TBoldAnsiString);
    function GetAsWideString: TBoldUnicodeString;
    procedure SetAsWideString(const Value: TBoldUnicodeString);
    function GetAsInteger: Integer;
    procedure SetAsInteger(const Value: Integer);
    function GetAsBoolean: Boolean;
    function GetAsCurrency: Currency;
    function GetAsDateTime: TDateTime;
    function GetAsFloat: Double;
    function GetIsNull: Boolean;
    procedure SetAsBoolean(const Value: Boolean);
    procedure SetAsCurrency(const Value: Currency);
    procedure SetAsDateTime(const Value: TDateTime);
    procedure SetAsFloat(const Value: Double);
    function GetFieldName: String;
    function GetAsDate: TDateTime;
    function GetAsTime: TDateTime;
    procedure SetAsTime(const Value: TDateTime);
    procedure SetAsDate(const Value: TDateTime);
    procedure SetAsBlob(const Value: TBoldAnsiString);
    function GetAsBlob: TBoldAnsiString;
    function GetAsInt64: Int64;
    procedure SetAsInt64(const Value: Int64);
  protected
    function GetAsString: String; virtual;
    property DataSetWrapper: TBoldDatasetWrapper read fDatasetWrapper;
  public
    constructor Create(Field: TField; DatasetWrapper: TBoldDatasetWrapper);
    procedure ReTarget(Field: TField);
    property Field: TField read GetField;
    property FieldName: String read GetFieldName;
  end;

procedure BoldLogSQL(const sql: TStrings);
procedure BoldLogSQLWide(const sql: TWideStrings; const Params: IBoldParameterized);
procedure BoldLogSQLWithParams(const sql: TStrings; const Params: IBoldParameterized);
function BoldQueryAsString(const sql: TStrings; const Params: IBoldParameterized): string;
procedure kiCLogSQL(const s: String);
procedure kiCLogSQLException(const s: String);

var
  BoldSQLLogHandler: TBoldLogHandler = nil;
  BoldSQLLogCount: int64 = 0;
  BoldSQLMessage: string = '';
  kiCSQLLogHandler: TBoldLogHandler = nil;
  kiCSQLExceptionLogHandler: TBoldLogHandler = nil;

implementation

uses
  Windows,

  BoldCoreConsts,
  BoldSharedStrings,
  BoldUtils,
  BoldIsoDateTime;

procedure BoldLogSQL(const sql: TStrings);
var
  i: integer;
begin
  Inc(BoldSQLLogCount);
  if assigned(BoldSQLLogHandler) then
  begin
    if BoldSQLMessage <> '' then
        BoldSQLLogHandler.Log(BoldSQLMessage);
    BoldSQLLogHandler.Log(
      AsIsoDateTimeMs(now) +':'+
      format('SQL %3d- %s', [BoldSQLLogCount, trim(SQL[0])]));
    for i := 1 to SQL.Count - 1 do
    begin
      if trim(SQL[i]) <> '' then
        BoldSQLLogHandler.Log('                                                    '  +
                              Trim(SQL[i]));
    end;
  end;
end;

procedure BoldLogSQLWide(const sql: TWideStrings; const Params: IBoldParameterized);
var
  i: integer;
begin
  Inc(BoldSQLLogCount);
  if assigned(BoldSQLLogHandler) then
  begin
    if BoldSQLMessage <> '' then
      BoldSQLLogHandler.Log(BoldSQLMessage);
    BoldSQLLogHandler.Log(
      AsIsoDateTimeMs(now) +':'+
      format('SQL %3d- %s', [BoldSQLLogCount, trim(SQL[0])]));
    for i := 1 to SQL.Count - 1 do
    begin
      if trim(SQL[i]) <> '' then
        BoldSQLLogHandler.Log('                                                    '  +
                              Trim(SQL[i]));
    end;
    if Params <> nil then
      for I := 0 to Params.ParamCount - 1 do
      begin
      BoldSQLLogHandler.Log(
        '                                                    ' +
        Format('  [%s]:%s', [Params.Param[i].Name, Params.Param[i].AsString])
      );
      end;
  end;
end;

function BoldQueryAsString(const sql: TStrings; const Params: IBoldParameterized): string;
var
  i: integer;
begin
  result := '';
  for i := 0 to SQL.Count - 1 do
  begin
    if trim(SQL[i]) <> '' then
      result := result + Trim(SQL[i]);
  end;
  if Params <> nil then
    for I := 0 to Params.ParamCount - 1 do
    begin
      result := result + Format('  [%s]:%s', [Params.Param[i].Name, Params.Param[i].AsString]);
    end;
end;

procedure BoldLogSQLWithParams(const sql: TStrings; const Params: IBoldParameterized);
var
  i: integer;
begin
  Inc(BoldSQLLogCount);
  if assigned(BoldSQLLogHandler) then
  begin
    if BoldSQLMessage <> '' then
      BoldSQLLogHandler.Log(BoldSQLMessage);
    BoldSQLLogHandler.Log(
      AsIsoDateTimeMs(now) +':'+
      format('SQL %3d- %s', [BoldSQLLogCount, trim(SQL[0])]));
    for i := 1 to SQL.Count - 1 do
    begin
      if trim(SQL[i]) <> '' then
        BoldSQLLogHandler.Log('                                                    '  +
                              Trim(SQL[i]));
    end;
    if Params <> nil then
      for I := 0 to Params.ParamCount - 1 do
      begin
      BoldSQLLogHandler.Log(
        '                                                    ' +
        Format('  [%s]:%s', [Params.Param[i].Name, Params.Param[i].AsString])
      );
      end;
  end;
end;

procedure kiCLogSQL(const s: String);
begin
  if assigned(kiCSQLLogHandler) then begin
    kiCSQLLogHandler.Log(s);
  end;
end;

procedure kiCLogSQLException(const s: String);
begin
  if assigned(kiCSQLExceptionLogHandler) then
  begin
    kiCSQLExceptionLogHandler.Log(s);
  end;
end;

{ TBoldDbParameter }

procedure TBoldDbParameter.Assign(const source: IBoldParameter);
begin
  Parameter.Value := Source.AsVariant;
end;

procedure TBoldDbParameter.AssignFieldValue(const source: IBoldField);
begin
  Parameter.AssignFieldValue(Source.Field, Source.AsVariant);
end;

procedure TBoldDbParameter.Clear;
begin
  Parameter.Clear;
end;

constructor TBoldDbParameter.Create(DbParameter: TParam; DatasetWrapper: TBoldDatasetWrapper);
begin
  inherited Create(DatasetWrapper);
  fParameter := DbParameter;
end;

destructor TBoldDbParameter.Destroy;
begin
  inherited;
end;

function TBoldDbParameter.GetAsBCD: Currency;
begin
  result := Parameter.AsBCD;
end;

function TBoldDbParameter.GetAsBlob: TBoldBlobData;
begin
  result := TBoldBlobData(Parameter.AsBlob);
end;

function TBoldDbParameter.GetAsBoolean: Boolean;
begin
  result := Parameter.AsBoolean
end;

function TBoldDbParameter.GetAsCurrency: Currency;
begin
  result := Parameter.AsCurrency;
end;

function TBoldDbParameter.GetAsDateTime: TDateTime;
begin
  result := Parameter.AsDateTime
end;

function TBoldDbParameter.GetAsFloat: Double;
begin
  result := Parameter.AsFloat;
end;

function TBoldDbParameter.GetAsInteger: Longint;
begin
  result := Parameter.AsInteger
end;

function TBoldDBParameter.GetAsInt64: Int64;
begin
{$IFDEF BOLD_DELPHI15_OR_LATER}
  result := parameter.AsLargeInt;
{$ELSE}
  result := parameter.AsInteger;
{$ENDIF}
end;

function TBoldDbParameter.GetAsMemo: string;
begin
  result := Parameter.AsMemo;
end;

function TBoldDbParameter.GetAsString: string;
begin
  result := Parameter.AsString;
  if result = DatasetWrapper.DatabaseWrapper.SQLDatabaseConfig.EmptyStringMarker then
    result := '';
end;

function TBoldDbParameter.GetAsAnsiString: TBoldAnsiString;
begin
  {$IFDEF BOLD_UNICODE}
  result := Parameter.AsAnsiString;
  {$ELSE}
  result := Parameter.AsString;
  {$ENDIF}
  if string(result) = DatasetWrapper.DatabaseWrapper.SQLDatabaseConfig.EmptyStringMarker then
    result := '';
end;

function TBoldDbParameter.GetAsWideString: WideString;
begin
  result := Parameter.AsWideString;
  if string(result) = DatasetWrapper.DatabaseWrapper.SQLDatabaseConfig.EmptyStringMarker then
    result := '';
end;

function TBoldDbParameter.GetAsVariant: Variant;
begin
  result := Parameter.Value;
end;

function TBoldDbParameter.GetDataType: TFieldType;
begin
  Result := Parameter.DataType;
end;

function TBoldDbParameter.GetIsNull: Boolean;
begin
  result := Parameter.IsNull;
end;

function TBoldDbParameter.GetName: String;
begin
  result := Parameter.Name;
end;

function TBoldDbParameter.GetParameter: TParam;
begin
  result := fParameter;
end;

procedure TBoldDbParameter.SetAsBCD(const Value: Currency);
begin
  Parameter.AsBCD := Value;
end;

procedure TBoldDbParameter.SetAsBlob(const Value: TBoldBlobData);
begin
  {$IFDEF BOLD_UNICODE}
  Parameter.AsBlob := BytesOf(Value);
  {$ELSE}
  Parameter.AsBlob := Value;
  {$ENDIF}
end;

procedure TBoldDbParameter.SetAsBoolean(Value: Boolean);
begin
  Parameter.AsBoolean := Value;
end;

procedure TBoldDbParameter.SetAsCurrency(const Value: Currency);
begin
  Parameter.AsCurrency := Value;
end;

procedure TBoldDbParameter.SetAsDate(const Value: TDateTime);
begin
  Parameter.AsDate := Value;
end;

procedure TBoldDbParameter.SetAsDateTime(const Value: TDateTime);
begin
  Parameter.AsDateTime := Value;
end;

procedure TBoldDbParameter.SetAsFloat(const Value: Double);
begin
  Parameter.AsFloat := Value;
end;

procedure TBoldDbParameter.SetAsInteger(Value: Integer);
begin
  Parameter.AsInteger := Value;
end;

procedure TBoldDBParameter.SetAsInt64(const Value: Int64);
begin
{$IFDEF BOLD_DELPHI15_OR_LATER}
  Parameter.AsLargeInt := Value;
{$ELSE}
  Parameter.AsInteger:= Value;
{$ENDIF}
end;


procedure TBoldDbParameter.SetAsMemo(const Value: string);
begin
  Parameter.AsMemo := Value;
end;

procedure TBoldDbParameter.SetAsSmallInt(Value: Integer);
begin
  Parameter.AsSmallInt := Value;
end;

procedure TBoldDbParameter.SetAsString(const Value: string);
begin
  if value = '' then
    Parameter.Value := DatasetWrapper.DatabaseWrapper.SQLDatabaseConfig.EmptyStringMarker
  else
    Parameter.Value := Value
end;

procedure TBoldDbParameter.SetAsAnsiString(const Value: TBoldAnsiString);
begin
  if value = '' then
    Parameter.Value := DatasetWrapper.DatabaseWrapper.SQLDatabaseConfig.EmptyStringMarker
  else
  {$IFDEF BOLD_UNICODE}
    Parameter.AsAnsiString := Value
  {$ELSE}
    Parameter.AsString := Value
  {$ENDIF}
end;

procedure TBoldDbParameter.SetAsWideString(const Value: Widestring);
begin
  if value = '' then
    Parameter.Value := DatasetWrapper.DatabaseWrapper.SQLDatabaseConfig.EmptyStringMarker
  else
    Parameter.AsWideString := Value
end;

procedure TBoldDbParameter.SetAsTime(const Value: TDateTime);
begin
  Parameter.AsTime := Value;
end;

procedure TBoldDbParameter.SetAsVariant(const NewValue: Variant);
begin
  Parameter.Value := NewValue;
end;

procedure TBoldDbParameter.SetAsWord(Value: Integer);
begin
  Parameter.AsWord := Value;
end;

procedure TBoldDbParameter.SetDataType(Value: TFieldType);
begin
  Parameter.DataType := Value;
end;

procedure TBoldDbParameter.SetText(const Value: string);
begin
  Parameter.AsString := Value;
end;

{ TBoldDataSetWrapper }

procedure TBoldDataSetWrapper.Append;
begin
  DataSet.Append;
end;

procedure TBoldDataSetWrapper.Close;
begin
  if Assigned(DataSet) then  // some subclasses perform tricks here
     DataSet.Close;
end;

function TBoldDataSetWrapper.CreateParam(FldType: TFieldType;
  const ParamName: string; ParamType: TParamType;
  Size: integer): IBoldParameter;
begin
  raise EBold.CreateFmt(sCreateParamNotImplemented, [classname]);
end;

function TBoldDataSetWrapper.CreateParam(FldType: TFieldType; const ParamName: string): IBoldParameter;
begin
  result := Createparam(FldType, Paramname, ptUnknown, 0);
end;

procedure TBoldDataSetWrapper.Delete;
begin
  Dataset.Delete;
end;

procedure TBoldDataSetWrapper.Edit;
begin
  DataSet.Edit;
end;

function TBoldDataSetWrapper.EnsureParamByName(
  const Value: string): IBoldParameter;
begin
  result := FindParam(Value);
  if not Assigned(result) then
    result := CreateParam(db.ftUnknown, Value);
end;

function TBoldDataSetWrapper.FieldByName(const FieldName: string): IBoldField;
begin
   Result := FieldByUpperCaseName(AnsiUpperCase(FieldName));
end;

function TBoldDataSetWrapper.FieldByUpperCaseName(const FieldNameUpper: string): IBoldField;
var
  DataSetFieldList :TFieldList;

  procedure FoundIndex(Index: Integer);
  begin
      fLastUsedFieldIndex := Index;
      fFieldNames[Index] := FieldNameUpper;
  end;

  function TryIndex(Index: Integer): Boolean;
  begin
    Result :=
      (Index >= 0) and
      (Index < Length(fFieldNames)) and
      (FieldNameUpper = fFieldNames[Index]);
    if Result then
      FoundIndex(Index);
  end;

  function SearchIndex: Boolean;
  var
    I: Integer;
  begin
    result := false;
    for I := 0 to DataSet.FieldCount - 1 do
      if FieldNameUpper = fFieldNames[I] then
        begin
          FoundIndex(I);
          Result := true;
          break;
        end;
  end;

  procedure InitNames;
  var
    F: integer;
  begin
    DataSetFieldList.Update;
    SetLength(fFieldNames, DataSet.FieldCount);
    for F := 0 to DataSet.FieldCount - 1 do
      fFieldNames[F] := AnsiUpperCase(DataSetFieldList.Fields[F].FieldName);
    fNamesInited  := true;
  end;

begin
  DataSetFieldList := DataSet.FieldList;
  if not fNamesInited  then
    InitNames;
  if not TryIndex(fLastUsedFieldIndex+1) then
    if not TryIndex(fLastUsedFieldIndex-1) then
      if not TryIndex(fLastUsedFieldIndex) then
        if not SearchIndex then
          fLastUsedFieldIndex := -1;
  if fLastUsedFieldIndex = -1 then
    Result := nil
  else
    Result := GetWrappedField(DataSetFieldList.Fields[fLastUsedFieldIndex]);
end;

function TBoldDataSetWrapper.FindField(const FieldName: string): IBoldField;
var
  Field: TField;
begin
  Field := DataSet.FindField(FieldName);
  if assigned(field) then
    Result := GetWrappedField(Field)
  else
    result := nil;
end;

procedure TBoldDataSetWrapper.First;
begin
  DataSet.First;
end;

function TBoldDataSetWrapper.GetEof: Boolean;
begin
  result := DataSet.eof;
end;

function TBoldDataSetWrapper.GetFieldCount: integer;
begin
  result := DataSet.FieldCount;
end;

function TBoldDataSetWrapper.GetFieldDefs: TFieldDefs;
begin
  result := DataSet.FieldDefs;
end;

function TBoldDataSetWrapper.GetFields(Index: integer): IBoldField;
begin
  Result := GetWrappedField(DataSet.Fields[Index]);
end;

function TBoldDataSetWrapper.GetFieldValue(const FieldName: string): Variant;
begin
  result := DataSet.FieldValues[FieldName];
end;

function TBoldDataSetWrapper.GetFieldWrapperClass: TBoldFieldWrapperClass;
begin
  result := TBoldFieldWrapper;
end;

function TBoldDataSetWrapper.GetRecNo: integer;
begin
  result := Dataset.RecNo;
end;

function TBoldDataSetWrapper.GetState: TDataSetState;
begin
  result := DataSet.State;
end;

function TBoldDataSetWrapper.GetWrappedField(Field: TField): IBoldField;
begin
  // Reuse wrapper if we hold only reference
  if Assigned(fWrapper1) and (fWrapper1.RefCount =1) then
  begin
    fWrapper1.Retarget(Field);
    Result := fWrapper1AsInterface;
  end
  else if Assigned(fWrapper2) and (fWrapper2.RefCount =1) then
  begin
    fWrapper2.Retarget(Field);
    Result := fWrapper2AsInterface;
  end
  else if not Assigned(fWrapper1) then
  begin
    fWrapper1 := GetFieldWrapperClass.Create(Field, self);
    fWrapper1.GetInterface(IBoldField, fWrapper1AsInterface);
     Result := fWrapper1AsInterface;
  end
  else
  begin
    fWrapper2 := GetFieldWrapperClass.Create(Field, self);
    fWrapper2.GetInterface(IBoldField, fWrapper2AsInterface);
    Result := fWrapper2AsInterface;
  end;
end;

function TBoldDataSetWrapper.Locate(const KeyFields: string;
  const KeyValues: Variant; Options: TLocateOptions): Boolean;
begin
  result := Dataset.Locate(KeyFields, KeyValues, Options);
end;

function TBoldDataSetWrapper.MoveBy(Distance: integer): integer;
begin
  result := DataSet.MoveBy(Distance);
end;

procedure TBoldDataSetWrapper.Next;
begin
  DataSet.Next;
end;

procedure TBoldDataSetWrapper.Open;
begin
  Dataset.Open;
  fLastUsedFieldIndex := -1;
  fNamesInited := false;
end;

procedure TBoldDataSetWrapper.Post;
begin
  DataSet.Post;
end;

procedure TBoldDataSetWrapper.Reconnect;
begin
  if DatabaseWrapper.SupportsInterface(IBoldDataBase) then begin
    (DatabaseWrapper as IBoldDataBase).Reconnect;
  end;
end;

procedure TBoldDataSetWrapper.SetFieldValue(const FieldName: string;
  const Value: Variant);
begin
  DataSet.FieldValues[FieldName] := value;
end;

{ TBoldFieldWrapper }

procedure TBoldFieldWrapper.SetAsBlob(const Value: TBoldAnsiString);
begin
  Field.AsVariant := Value;
end;

constructor TBoldFieldWrapper.Create(Field: TField; DatasetWrapper: TBoldDatasetWrapper);
begin
  inherited create;
  fField := Field;
  fSavedValue := Field.Value;
  fDatasetWrapper := DatasetWrapper;
end;

function TBoldFieldWrapper.GetAsBlob: TBoldAnsiString;
begin
  {$IFDEF BOLD_UNICODE}
  Result := Field.AsAnsiString;
  {$ELSE}
  Result := Field.AsString;
  {$ENDIF}
end;

function TBoldFieldWrapper.GetAsBoolean: Boolean;
begin
  result := Field.AsBoolean;
end;

function TBoldFieldWrapper.GetAsCurrency: Currency;
begin
  result := Field.AsCurrency;
end;

function TBoldFieldWrapper.GetAsDate: TDateTime;
begin
  result := trunc(Field.AsDateTime);
end;

function TBoldFieldWrapper.GetAsDateTime: TDateTime;
begin
  result := Field.AsDateTime;
end;

function TBoldFieldWrapper.GetAsFloat: Double;
begin
  result := Field.AsFloat;
end;

function TBoldFieldWrapper.GetAsInteger: Integer;
begin
  result := Field.AsInteger;
end;

function TBoldFieldWrapper.GetAsString: String;
begin
  result := BoldSharedStringManager.GetSharedString(Field.AsString);
  if result = DatasetWrapper.DatabaseWrapper.SQLDatabaseConfig.EmptyStringMarker then
    result := '';
end;

function TBoldFieldWrapper.GetAsAnsiString: TBoldAnsiString;
begin
  {$IFDEF BOLD_UNICODE}
  result := BoldSharedStringManager.GetSharedAnsiString(Field.AsAnsiString);
  if String(result) = DatasetWrapper.DatabaseWrapper.SQLDatabaseConfig.EmptyStringMarker then
    result := '';
  {$ELSE}
  Result := GetAsString;
  {$ENDIF}
end;

function TBoldFieldWrapper.GetAsWideString: TBoldUnicodeString;
begin
  {$IFDEF BOLD_UNICODE}
  result := GetAsString;
  {$ELSE}
  result := Field.AsWideString;
  if result = DatasetWrapper.DatabaseWrapper.SQLDatabaseConfig.EmptyStringMarker then
    result := '';
  {$ENDIF}
end;

function TBoldFieldWrapper.GetAsTime: TDateTime;
begin
  result := frac(Field.AsDateTime);
end;

function TBoldFieldWrapper.GetAsInt64;
begin
{$IFDEF BOLD_DELPHI15_OR_LATER}
  Result := fField.AsLargeInt;
{$ELSE}
  Result := fField.AsInteger;
{$ENDIF}
end;

function TBoldFieldWrapper.GetAsVariant: Variant;
begin
  result := fSavedValue;
end;

function TBoldFieldWrapper.GetField: TField;
begin
  result := fField;
end;

function TBoldFieldWrapper.GetFieldName: String;
begin
  result := Field.Fieldname;
end;

function TBoldFieldWrapper.GetIsNull: Boolean;
begin
  result := VarType(fSavedValue) = varNull;
end;

procedure TBoldFieldWrapper.ReTarget(Field: TField);
begin
  fField := Field;
  fSavedValue := Field.Value;
end;

procedure TBoldFieldWrapper.SetAsInt64(const Value: Int64);
begin
{$IFDEF BOLD_DELPHI15_OR_LATER}
  Field.AsLargeInt := Value;
{$ELSE}
  Field.AsInteger:= Value;
{$ENDIF}
end;

procedure TBoldFieldWrapper.SetAsBoolean(const Value: Boolean);
begin
  Field.AsBoolean := Value;
end;

procedure TBoldFieldWrapper.SetAsCurrency(const Value: Currency);
begin
  Field.AsCurrency := Value;
end;

procedure TBoldFieldWrapper.SetAsDateTime(const Value: TDateTime);
begin
  Field.AsDateTime := Value;
end;

procedure TBoldFieldWrapper.SetAsFloat(const Value: Double);
begin
  Field.AsFloat := Value;
end;

procedure TBoldFieldWrapper.SetAsInteger(const Value: Integer);
begin
  Field.AsInteger := value;
end;

procedure TBoldFieldWrapper.SetAsString(const Value: String);
begin
  if value = '' then
    Field.AsString := DataSetWrapper.DatabaseWrapper.SQLDatabaseConfig.EmptyStringMarker
  else
    Field.AsString := Value;
end;

procedure TBoldFieldWrapper.SetAsAnsiString(const Value: TBoldAnsiString);
begin
  {$IFDEF BOLD_UNICODE}
  if value = '' then
    Field.AsAnsiString := TBoldAnsiString(
        DataSetWrapper.DatabaseWrapper.SQLDatabaseConfig.EmptyStringMarker)
  else
    Field.AsAnsiString := Value;
  {$ELSE}
  SetAsString(Value);
  {$ENDIF}
end;

procedure TBoldFieldWrapper.SetAsWideString(const Value: TBoldUnicodeString);
begin
  {$IFDEF BOLD_UNICODE}
  SetAsString(Value);
  {$ELSE}
  if value = '' then
    Field.AsWideString := TBoldUnicodeString(DataSetWrapper.DatabaseWrapper.SQLDatabaseConfig.EmptyStringMarker)
  else
    Field.AsWideString := Value;
  {$ENDIF}
end;

procedure TBoldFieldWrapper.SetAsVariant(const Value: Variant);
begin
  Field.AsVariant := value;
  fSavedValue := Value;
end;

procedure TBoldFieldWrapper.SetAsDate(const Value: TDateTime);
begin
  Field.AsDateTime := Value;
end;

procedure TBoldFieldWrapper.SetAsTime(const Value: TDateTime);
begin
  Field.AsDateTime := Value;
end;

{ TBoldDatabaseWrapper }

constructor TBoldDatabaseWrapper.Create(SQLDatabaseConfig: TBoldSQLDatabaseConfig);
begin
  inherited Create;
  fSQLDatabaseConfig := SQLDatabaseConfig;
end;

procedure TBoldDatabaseWrapper.CreateDatabase(DropExisting: boolean);
begin
// override in subclass
end;

function TBoldDatabaseWrapper.DatabaseExists: boolean;
begin
  result := false;
// override in subclass
end;

procedure TBoldDatabaseWrapper.DropDatabase;
begin
// override in subclass
end;

destructor TBoldDatabaseWrapper.destroy;
begin
  FreeAndNil(fAllTableNames);
  inherited;
end;

function TBoldDatabaseWrapper.GetExecQuery: IBoldExecQuery;
var
  aQuery: IBoldQuery;
  QueryClassName: String;
begin
  aQuery := GetQuery;
  if aQuery.QueryInterface(IBoldExecQuery, result) <> S_OK then
  begin
    QueryClassName := aQuery.Implementor.ClassName;
    ReleaseQuery(aQuery);
    raise EBoldInternal.CreateFmt('%s.GetExecQuery: %s does not implement IBoldExecQuery', [QueryClassName, classname]);
  end;
end;

function TBoldDatabaseWrapper.GetIndexDescriptions(
  const TableName: String): TBoldIndexDescriptionArray;
begin
  if SQLDatabaseConfig.IndexInfoTemplate = '' then
    Result := GetIndexDescriptionsViaTable(TableName)
  else
    Result := GetIndexDescriptionsViaQuery(TableName);
end;

function TBoldDatabaseWrapper.GetIndexDescriptionsViaQuery(
  const TableName: String): TBoldIndexDescriptionArray;
var
  DbQuery: IBoldQuery;
  CurrentIndex: integer;
  LastIndexName: string;
  SQL: string;
  vIndexField: IBoldField;
  vColumnField: IBoldField;
  vIsPrimaryField: IBoldField;
  vIsUniqueField: IBoldField;
const
  cIndexName = 'indexName';
  cColumnName = 'columnName';
  cIsPrimary = 'isPrimary';
  cisUnique = 'isUnique';
  cFieldNotFoundMessage = 'Field %s not found in IndexInfoQuery result set.';
begin
  SetLength(result, 0);
  DbQuery := GetQuery;
  lastIndexName := '';
  try
    SQL := SQLDatabaseConfig.GetIndexInfoQuery(TableName);
    DbQuery.AssignSQLText(SQL);
    DbQuery.Open;
    Assert(Assigned(DbQuery.FindField(cIndexName)), Format(cFieldNotFoundMessage, [cIndexName]));
    Assert(Assigned(DbQuery.FindField(cColumnName)), Format(cFieldNotFoundMessage, [cColumnName]));
    Assert(Assigned(DbQuery.FindField(cIsPrimary)), Format(cFieldNotFoundMessage, [cIsPrimary]));
    Assert(Assigned(DbQuery.FindField(cIsUnique)), Format(cFieldNotFoundMessage, [cIsUnique]));
    vIndexField := DbQuery.FieldByName(cIndexName);
    vColumnField := DbQuery.FieldByName(cColumnName);
    vIsPrimaryField := DbQuery.FieldByName(cIsPrimary);
    vIsUniqueField := DbQuery.FieldByName(cIsUnique);
    CurrentIndex := -1;
    // IndexName, IsPrimary, IsUnique, ColumnName
    while not DbQuery.Eof do
    begin
      if vIndexField.AsString <> LastIndexName then  // new Index
      begin
        INC(CurrentIndex);
        SetLength(Result, CurrentIndex+1);
        LastIndexName := vIndexField.AsString;
        Result[CurrentIndex].IndexName := LastIndexName;
        Result[CurrentIndex].IsPrimary := vIsPrimaryField.AsBoolean;
        Result[CurrentIndex].IsUnique := vIsUniqueField.AsBoolean;
      end;
      if Result[CurrentIndex].IndexedColumns = '' then
        Result[CurrentIndex].IndexedColumns := vColumnField.AsString
      else
        Result[CurrentIndex].IndexedColumns := Result[CurrentIndex].IndexedColumns + ';' + vColumnField.AsString;
      DbQuery.Next;
    end;
  finally
    DbQuery.Close;
    releaseQuery(DbQuery);
  end;
end;

function TBoldDatabaseWrapper.GetIndexDescriptionsViaTable(
  const TableName: String): TBoldIndexDescriptionArray;
var
  DbTable: IBoldTable;
  I: integer;
  IndexDef: TIndexDef;
begin
  SetLength(result, 0);
  DbTable := GetTable;
  try
    DbTable.Tablename := TableName;
    DbTable.Open;
    SetLength(Result, DbTable.IndexDefs.Count);
    for I := 0 to DbTable.IndexDefs.Count - 1 do
    begin
      IndexDef := DbTable.IndexDefs[i];
      result[i].IndexName := IndexDef.Name;
      Result[i].IndexedColumns := IndexDef.Fields;
      result[i].IsPrimary := ixPrimary in IndexDef.Options;
      result[i].IsUnique := ixUnique in IndexDef.Options;
    end;
  finally
    DbTable.Close;
    releaseTable(DbTable);
  end;

end;

function TBoldDatabaseWrapper.GetSQLDatabaseConfig: TBoldSQLDatabaseConfig;
begin
  result := fSQLDataBaseConfig;
end;

function TBoldDatabaseWrapper.GetWindowsLoginName: string;
var
  User: PChar;
  Size: DWord;
begin
  Result := '';
  Size := 256;
  User := StrAlloc(Size);
  try
    try
      if WNetGetUser(PChar(0), User, Size) = 0 then begin
        Result := string(User);
      end else begin
        RaiseLastOSError;
      end;
    except
      if GetUserName(User, Size) then begin
        Result := string(User);
      end else begin
        RaiseLastOSError;
      end;
    end;
  finally
    StrDispose(User);
  end;
end;


function TBoldDatabaseWrapper.InternalGetDatabaseError(const aErrorType:
    TBoldDatabaseErrorType; const E: Exception; sSQL, sServer, sDatabase,
    sUserName: string; bUseWindowsAuth: Boolean): EBoldDatabaseError;
var
  sMsg,
  sWindowsAuth: string;
begin
  case aErrorType of
    bdetConnection: begin
      sMsg := Format(BOLD_DATABASE_ERROR_CONNECTION, [sServer]);
      Result := EBoldDatabaseConnectionError.Create(sMsg);
    end;
    bdetSQL: begin
      sMsg := Format(BOLD_DATABASE_ERROR_SQL, [sSQL, E.Message]);
      Result := EBoldDatabaseSQLError.Create(sMsg);
    end;
    bdetUpdate: begin
      Result := EBoldDatabaseSQLError.Create(BOLD_DATABASE_ERROR_UPDATE);
    end;
    bdetDeadlock: begin
      sMsg := Format(BOLD_DATABASE_ERROR_DEADLOCK, [E.Message]);
      Result := EBoldDatabaseDeadlockError.Create(sMsg);
    end;
    bdetLogin: begin
      if bUseWindowsAuth then begin
        sWindowsAuth := BOLD_DATABASE_ERROR_LOGIN_WINDOWS_AUTH;
        sUserName := GetWindowsLoginName;
      end else begin
        sWindowsAuth := '';
      end;
      sMsg := Format(BOLD_DATABASE_ERROR_LOGIN, [sUserName, sDatabase, sServer]);
      Result := EBoldDatabaseLoginError.Create(sMsg);
    end;
//    bdetError: begin
    else begin
      sMsg := Format(BOLD_DATABASE_ERROR_UNKNOWN, [E.Message]);
      Result := EBoldDatabaseError.Create(sMsg);
    end;
  end;
  result.OriginalExceptionClass := E.ClassName;
  result.OriginalExceptionMessage := E.Message;
end;

procedure TBoldDatabaseWrapper.ReleaseExecQuery(var Query: IBoldExecQuery);
var
  Query2: IBoldQuery;
begin
  if Query.QueryInterface(IBoldQuery, Query2) <> S_OK then
    raise EBoldInternal.CreateFmt('%s.ReleaseExecQuery: %s queries does not implement IBoldExecQuery', [Query.Implementor.ClassName, classname]);

  Query2 := Query as IBoldQuery;
  Query := nil;
  ReleaseQuery(Query2);
end;

function TBoldDatabaseWrapper.SupportsDefaultColumnValues: Boolean;
begin
  result := true;
end;

function TBoldDatabaseWrapper.TableExists(const TableName: String): Boolean;
begin
  if not Assigned(fAllTableNames) then
  begin
    fAllTableNames := TStringList.Create;
    fAllTableNames.Sorted := true;
    AllTableNames('*', true, fAllTableNames);
  end;
  result := fAllTableNames.IndexOf(TableName) <> -1;
end;

{ TBoldParameterWrapper }

constructor TBoldParameterWrapper.Create(DatasetWrapper: TBoldAbstractQueryWrapper);
begin
  inherited create;
  fDatasetWrapper := DatasetWrapper;
end;

{ TBoldAbstractQueryWrapper }

procedure TBoldAbstractQueryWrapper.Clear;
begin
// to be override
end;

constructor TBoldAbstractQueryWrapper.Create(
  DatabaseWrapper: TBoldDatabaseWrapper);
begin
  inherited Create;
  fDatabaseWrapper := DatabaseWrapper;
end;

function TBoldAbstractQueryWrapper.GetImplementor: TObject;
begin
  result := self;
end;

{ TBoldBatchDataSetWrapper }

function TBoldBatchDataSetWrapper.ParamsContainBlob: Boolean;
var
  i: integer;
const
  BlobFieldTypes = [{$IFDEF BOLD_DELPHI15_OR_LATER}db.ftStream,{$ENDIF} db.ftBlob, db.ftGraphic..db.ftTypedBinary, db.ftOraBlob, db.ftOraClob];
begin
  result := false;
  for i := 0 to self.Params.Count-1 do
    if (Params[i].DataType in BlobFieldTypes) then
    begin
      result := true;
      exit;
    end;
end;

type TCollectionAccess = class(TCollection);

procedure TBoldBatchDataSetWrapper.ReplaceParamMarkers(sql: TStrings;
  const Source, Dest: IBoldExecQuery);
const
  Literals = ['''', '"', '`'];
var
  SourceParams, DestParams: TParams;
  Name: String;
  Prefix: String;
  NewParamName: String;
  CurPos, StartPos: integer;
  PrevPos: integer;
  i: integer;
  ParamIndex, FirstParam: integer;
  Line: String;
  oldParam, NewParam: TParam;
  Literal: Boolean;

  function NameDelimiter: Boolean;
  begin
    Result := CharInSet(line[CurPos], [' ', ',', ';', ')', #13, #10]);
  end;

  function IsLiteral: Boolean;
  begin
    Result := CharInSet(line[CurPos], Literals);
  end;

  function CurrentChar: Char;
  begin
    result := line[CurPos];
  end;

  procedure AddLine(const s: string);
  begin
    sb.Append(StringReplace(s, BOLDCRLF, ' ', [rfReplaceAll]));
  end;

begin
  DestParams := Dest.Params;
    if Source.ParamCount = 0 then
    begin
      for i := 0 to Sql.Count - 1 do
      begin
        Line := sql[i];
        AddLine(Line);
      end;
    end
    else
    begin
      SourceParams := Source.Params;
      if Dest.ParamCount = 0 then
      begin
        DestParams.Assign(SourceParams);
        for i := 0 to Sql.Count - 1 do
        begin
          Line := sql[i];
          AddLine(Line);
        end;
      end
      else
      begin
        FirstParam := Dest.ParamCount;
        ParamIndex := Dest.ParamCount;
        Dest.ParamCheck := false;
        for i := 0 to Sql.Count - 1 do
        begin
          Line := Sql[i];
          if line = '' then
            continue;
          CurPos := 1;
          PrevPos := CurPos;
          Literal := False;
          repeat
          while CharInSet(CurrentChar, LeadBytes) do Inc(CurPos, 2);
            if (CurrentChar = ':') and not Literal and (Line[CurPos+1] <> ':') then
            begin
              StartPos := CurPos;
              while (CurrentChar <> #0) and (Literal or not NameDelimiter) do
              begin
                Inc(CurPos);
                if Curpos > Length(Line) then
                  break;
              while CharInSet(CurrentChar, LeadBytes) do Inc(CurPos, 2);
                if IsLiteral then
                begin
                  Literal := not Literal;
                end;
              end;
              Name := copy(Line, StartPos+1, CurPos-(StartPos+1));
              OldParam := SourceParams[ParamIndex-FirstParam];
              Assert(OldParam.Name = Name);
              Prefix := 'P'+IntToStr(ParamIndex);
              NewParamName := prefix {+ Name};
              NewParam := DestParams.CreateParam(OldParam.DataType, NewParamName, ptUnknown);
              NewParam.Assign(OldParam);
              NewParam.Name := NewParamName;
              AddLine(Copy(Line, PrevPos, StartPos-PrevPos+1));
              PrevPos := CurPos;
              SB.Append(NewParamName);
              inc(ParamIndex);
            end
            else
              if IsLiteral then
                Literal := not Literal;
            Inc(CurPos);
          until CurPos > Length(Line);
          if PrevPos <= Length(Line) then
            AddLine(Copy(Line, PrevPos, MaxInt));
        end;
      end;
    end;
    SB.Append(DatabaseWrapper.SQLDatabaseConfig.BatchQuerySeparator);
    Dest.SQLStrings.Add(sb.ToString);
    Inc(fAccumulatedSQLLength, SB.Length + Length(Dest.SQLStrings.LineBreak));
  SB.Clear;
end;

function TBoldBatchDataSetWrapper.GetHasCachedStatements: boolean;
begin
  result := fAccumulatedSQLLength > 0;
end;

function TBoldBatchDataSetWrapper.GetSqlLength: integer;
var
  i: Integer;
  LB: string;
begin
  result := 0;
  LB := SQLStrings.LineBreak;
  for I := 0 to SQLStrings.Count - 1 do
    Inc(result, Length(SQLStrings[I]) + Length(LB));
end;

function TBoldBatchDataSetWrapper.GetAccumulatedSQL: TStrings;
begin
  result := BatchQuery.SQLStrings;
end;

function TBoldBatchDataSetWrapper.GetBatchQueryParamCount: integer;
begin
  result := 0;
  if Assigned(BatchQuery) then
    result := BatchQuery.ParamCount;
end;

procedure TBoldBatchDataSetWrapper.SetInBatch(const Value: Boolean);
begin
  if not DatabaseWrapper.SQLDatabaseConfig.UseBatchQueries then
    exit;
  if FInBatch <> Value then
  begin
    FInBatch := Value;
    if Value then
    begin
      fBatchQuery := DatabaseWrapper.GetExecQuery;
      fBatchQuery.SQLStrings.BeginUpdate;
      fBatchQuery.Params.BeginUpdate;
      fParamsInBeginUpdate := true;
      if Supports(fBatchQuery, IBoldParameterized) then
        (fBatchQuery as IBoldParameterized).ParamCheck := false;
    end
    else
    begin
      if fParamsInBeginUpdate then
      begin
        fBatchQuery.SQLStrings.EndUpdate;
        fBatchQuery.Params.EndUpdate;
      end;
      DatabaseWrapper.ReleaseExecQuery(fBatchQuery);
    end;
  end;
end;

procedure TBoldBatchDataSetWrapper.StartSQLBatch;
begin
  InBatch := DatabaseWrapper.SQLDatabaseConfig.UseBatchQueries;
end;

procedure TBoldBatchDataSetWrapper.BatchExecSQL;
var
  SQLDatabaseConfig: TBoldSQLDatabaseConfig;
begin
  if inBatch then
  begin
    SQLDatabaseConfig := DatabaseWrapper.SQLDatabaseConfig;
    if ParamsContainBlob
      or (fAccumulatedSQLLength + GetSqlLength +
         Length(SQLDatabaseConfig.BatchQueryBegin) + Length(SQLDatabaseConfig.BatchQueryEnd)
         >= SQLDatabaseConfig.MaxBatchQueryLength)
      or (BatchQuery.ParamCount + ParamCount >= SQLDatabaseConfig.MaxBatchQueryParams) then
      ExecuteBatch;
    ReplaceParamMarkers(SQLStrings, self as IBoldExecQuery, BatchQuery);
  end;
end;

constructor TBoldBatchDataSetWrapper.Create(
  DatabaseWrapper: TBoldDatabaseWrapper);
begin
  inherited Create(DatabaseWrapper);
  SB := TStringBuilder.Create(cInitialBatchBufferSize);
end;

destructor TBoldBatchDataSetWrapper.Destroy;
begin
  if Assigned(DatabaseWrapper) and Assigned(fBatchQuery) then
    DatabaseWrapper.ReleaseExecQuery(fBatchQuery);
  FreeAndNil(SB);
  inherited;
end;

procedure TBoldBatchDataSetWrapper.EndSQLBatch;
begin
  if InBatch and HasCachedStatements then
    ExecuteBatch;
  InBatch := false;
end;

procedure TBoldBatchDataSetWrapper.FailSQLBatch;
begin
  if InBatch then
  begin
    AccumulatedSQL.Clear;
    fAccumulatedSQLLength := 0;
  end;
end;

procedure TBoldBatchDataSetWrapper.ExecuteBatch;
begin
  if HasCachedStatements then
  begin
    fInBatch := false;
    try
      if fParamsInBeginUpdate then
      begin
        if DatabaseWrapper.SQLDatabaseConfig.BatchQueryBegin <> '' then
          fBatchQuery.SQLStrings.Insert(0, DatabaseWrapper.SQLDatabaseConfig.BatchQueryBegin);
        if DatabaseWrapper.SQLDatabaseConfig.BatchQueryEnd <> '' then
          fBatchQuery.SQLStrings.Append(DatabaseWrapper.SQLDatabaseConfig.BatchQueryEnd);
        fBatchQuery.SQLStrings.EndUpdate;
        fBatchQuery.Params.EndUpdate;
        fParamsInBeginUpdate:=false;
        fBatchQuery.ParamCheck := true;
      end;
      fBatchQuery.ExecSql;
    finally
      fInBatch := true;
      AccumulatedSQL.Clear;
      fAccumulatedSQLLength := 0;
      fBatchQuery.SQLStrings.BeginUpdate;
      fBatchQuery.Params.BeginUpdate;
      fBatchQuery.Params.Clear;
      fParamsInBeginUpdate:=true;
      fBatchQuery.ParamCheck := false;
    end;
  end;
end;

end.
