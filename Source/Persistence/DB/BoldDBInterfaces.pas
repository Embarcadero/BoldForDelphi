unit BoldDBInterfaces;

interface

uses
  Classes,
  Db,
  BoldBase,
  BoldSQLDatabaseConfig,
  BoldLogHandler;

type
  IBoldQuery = interface;
  IBoldDataBase = interface;
  IBoldTable = interface;
  IBoldField = interface;
  IBoldParameter = interface;
  TBoldDataSetWrapper = class;
  TBoldDatabaseWrapper = class;
  TBoldFieldWrapper = class;

  TBoldFieldWrapperClass = class of TBoldFieldWrapper;

  TBoldGetDatabaseEvent = function: IBoldDatabase of object;


  IBoldField = interface
  ['{F4126F4C-F1B2-472B-B53F-2ECEC8EE9253}']
    function GetField: TField;
    function GetFieldName: String;
    function GetAsVariant: Variant;
    procedure SetAsVariant(const Value: Variant);
    function GetAsString: String;
    procedure SetAsString(const Value: String);
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
    procedure SetAsBlob(const Value: string);
    function GetAsBlob: string;

    property Field: TField read GetField;
    property AsVariant: Variant read GetAsVariant write SetAsVariant;
    property Value: Variant read GetAsVariant write SetAsVariant;
    property AsString: String read GetAsString write SetAsString;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsCurrency: Currency read GetAsCurrency write SetAsCurrency;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsDate: TDateTime read GetAsDate write SetAsDate;
    property AsTime: TDateTime read GetAsTime write SetAsTime;
    property AsBlob: String read GetAsBlob write SetAsBlob;
    property IsNull: Boolean read GetIsNull;
    property FieldName: String read GetFieldName;
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
    function GetAsBoolean: Boolean;
    function GetAsDateTime: TDateTime;
    function GetAsCurrency: Currency;
    function GetAsFloat: Double;
    function GetAsInteger: Longint;
    function GetAsMemo: string;
    function GetAsString: string;
//    function GetAsWideString: WideString;
    function GetIsNull: Boolean;
    procedure SetAsBCD(const Value: Currency);
    procedure SetAsBlob(const Value: TBlobData);
    procedure SetAsBoolean(Value: Boolean);
    procedure SetAsCurrency(const Value: Currency);
    procedure SetAsDate(const Value: TDateTime);
    procedure SetAsDateTime(const Value: TDateTime);
    procedure SetAsFloat(const Value: Double);
    procedure SetAsInteger(Value: Longint);
    procedure SetAsMemo(const Value: string);
    procedure SetAsString(const Value: string);
//    procedure SetAsWideString(const Value: Widestring);
    procedure SetAsSmallInt(Value: LongInt);
    procedure SetAsTime(const Value: TDateTime);
    procedure SetAsWord(Value: LongInt);
    procedure SetText(const Value: string);
    procedure AssignFieldValue(source: IBoldField);
    property asVariant: Variant read GetAsVariant write SetAsVariant;
    property Name: String read GetName;
    property DataType: TFieldType read GetDataType write SetDataType;
    property AsBCD: Currency read GetAsBCD write SetAsBCD;
//marco    property AsBlob: TBlobData read GetAsString write SetAsBlob;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsCurrency: Currency read GetAsCurrency write SetAsCurrency;
    property AsDate: TDateTime read GetAsDateTime write SetAsDate;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsInteger: LongInt read GetAsInteger write SetAsInteger;
    property AsSmallInt: LongInt read GetAsInteger write SetAsSmallInt;
    property AsMemo: string read GetAsMemo write SetAsMemo;
    property AsString: string read GetAsString write SetAsString;
    property AsTime: TDateTime read GetAsDateTime write SetAsTime;
    property AsWord: LongInt read GetAsInteger write SetAsWord;
//    property AsWideString: WideString read GetAsWideString write SetAsWideString;
    property IsNull: Boolean read GetIsNull;
    property Text: string read GetAsString write SetText;
  end;

  IBoldDataSet = interface
  ['{D00CA0A0-41CE-11D3-89F5-006008F62CFF}']
    procedure Append;
    procedure Close;
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

  IBoldExecQuery = interface
  ['{219D1FF1-F509-42A3-96E9-D7F62C28C1EA}']
    function GetSQLText: String;
    procedure StartSQLBatch;
    procedure EndSQLBatch;
    procedure FailSQLBatch;
    procedure ExecSQL;
    procedure AssignSQL(SQL: TStrings);
    procedure AssignSQLText(SQL: String);
    procedure ClearParams;
    function ParamByName(const Value: string): IBoldParameter;
    function GetRowsAffected: integer;
    function GetImplementor: TObject;
    property RowsAffected: integer read GetRowsAffected;
    property Implementor: TObject read GetImplementor;
    property SQLText: String read GetSQLText;
  end;

  IBoldParameterized = interface
  ['{B9020CF8-0300-4476-9453-4A3760E13225}']
    procedure ClearParams;
    procedure AssignParams(Params: TParams);
    function ParamByName(const Value: string): IBoldParameter;
    function GetParamCount: integer;
    function GetParams(i:integer): IBoldParameter;
    function Createparam(FldType: TFieldType; const ParamName: string; ParamType: TParamType; Size: integer): IBoldParameter;
    property Params[i: integer]: IBoldParameter read GetParams;
    property ParamCount: integer read GetParamCount;
  end;

  IBoldQuery = interface(IBoldDataSet)
  ['{D00CA0A1-41CE-11D3-89F5-006008F62CFF}']
    function GetSQLText: String;
    function GetRequestLiveQuery: Boolean;
    procedure SetRequestLiveQuery(NewValue: Boolean);
    function GetRecordCount: integer;
    procedure AssignSQLText(SQL: String);
    procedure AssignSQL(SQL: TStrings);
    procedure ClearParams;
    procedure AssignParams(Params: TParams);
    function ParamByName(const Value: string): IBoldParameter;
    property RequestLiveQuery: Boolean read GetRequestLiveQuery write SetRequestLiveQuery;
    property RecordCount: integer read GetRecordCount;
    property SQLText: String read GetSQLText;
  end;

  IBoldTable = interface(IBoldDataSet)
  ['{D6698E80-41CE-11D3-89F5-006008F62CFF}']
    procedure AddIndex(const Name, Fields: string; Options: TIndexOptions; const DescFields: string = '');
    function GetIndexDefs: TIndexDefs;
    procedure SetTableName(NewName: String);
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
    function GetSQLDatabaseConfig: TBoldSQLDatabaseConfig;
    property SQLDatabaseConfig: TBoldSQLDatabaseConfig read GetSQLDatabaseConfig;
  end;

  TBoldParameterWrapper = class(TBoldRefCountedObject)
  private
    fDatasetWrapper: TBolddatasetWrapper;
  protected
    property DatasetWrapper: TBoldDatasetWrapper read fDatasetWrapper;
  public
    constructor create(DatasetWrapper: TBoldDatasetWrapper);
  end;

  TBoldDbParameter = class(TBoldParameterWrapper, IBoldParameter)
  private
    fParameter: TParam;
    function GetAsVariant: Variant;
    procedure SetAsVariant(const NewValue: Variant);
    function GetName: String;
    procedure Clear;
    function GetDataType: TFieldType;
    procedure SetDataType(Value: TFieldType);
    function GetAsBCD: Currency;
    function GetAsBoolean: Boolean;
    function GetAsCurrency: Currency;
    function GetAsFloat: Double;
    function GetAsInteger: Longint;
    function GetAsMemo: string;
    function GetAsString: string;
    function GetIsNull: Boolean;
    procedure SetAsBCD(const Value: Currency);
    procedure SetAsBlob(const Value: TBlobData);
    procedure SetAsBoolean(Value: Boolean);
    procedure SetAsCurrency(const Value: Currency);
    procedure SetAsDate(const Value: TDateTime);
    procedure SetAsFloat(const Value: Double);
    procedure SetAsInteger(Value: Longint);
    procedure SetAsMemo(const Value: string);
    procedure SetAsString(const Value: string);
    procedure SetAsSmallInt(Value: LongInt);
    procedure SetAsTime(const Value: TDateTime);
//    procedure SetAsWideString(const Value: Widestring);
    procedure SetAsWord(Value: LongInt);
    procedure SetText(const Value: string);
    procedure AssignFieldValue(source: IBoldField);
    function GetParameter: TParam;
  protected
    property Parameter: TParam read GetParameter;
    procedure SetAsDateTime(const Value: TDateTime); virtual;
    function GetAsDateTime: TDateTime; virtual;
  public
    constructor Create(DbParameter: TParam; DatasetWrapper: TBoldDatasetWrapper);
    destructor Destroy; override;
  end;

  TBoldDataSetWrapper = class(TBoldNonRefCountedObject)
  private
    fLastKnowFieldIndex: integer;
    fDatabaseWrapper: TBoldDatabaseWrapper;
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
    function Createparam(FldType: TFieldType; const ParamName: string; ParamType: TParamType; Size: integer): IBoldParameter;
    procedure Close; virtual;
    function FieldByName(const FieldName: string): IBoldField;
    function FindField(const FieldName: string): IBoldField;
    procedure First;
    procedure Delete;
    procedure Next;
    procedure Open; virtual;
    procedure Edit;
    function MoveBy(Distance: integer): integer;
    function Locate(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions): Boolean;
    procedure Post;
    function GetImplementor: TObject;
    function GetState: TDataSetState;
  public
    constructor Create(DatabaseWrapper: TBoldDatabaseWrapper);
    property DatabaseWrapper: TBoldDatabaseWrapper read fDatabaseWrapper;
    property DataSet: TDataSet read GetDataSet;
  end;

  TBoldDatabaseWrapper = class(TBoldNonRefCountedObject)
  private
    fSQLDataBaseConfig: TBoldSQLDatabaseConfig;
  protected
    function SupportsDefaultColumnValues: Boolean; virtual;
    procedure AllTableNames(Pattern: String; SystemTables: Boolean; TableNameList: TStrings); virtual; abstract;
    function TableExists(const TableName: String): Boolean;
    function GetQuery: IBoldQuery; virtual; abstract;
    procedure ReleaseQuery(var Query: IBoldQuery); virtual; abstract;
    function GetExecQuery: IBoldExecQuery; virtual;
    procedure ReleaseExecQuery(var Query: IBoldExecQuery); virtual;
    function GetSQLDatabaseConfig: TBoldSQLDatabaseConfig;
  public
    constructor Create(SQLDataBaseConfig: TBoldSQLDatabaseConfig);
    destructor Destroy; override;
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
    procedure SetAsBlob(const Value: string);
    function GetAsBlob: string;
  protected
    function GetAsString: String; virtual;
    property DataSetWrapper: TBoldDatasetWrapper read fDatasetWrapper;
  public
    constructor create(Field: TField; DatasetWrapper: TBoldDatasetWrapper);
    property Field: TField read GetField;
    property FieldName: String read GetFieldName;
  end;

procedure BoldLogSQL(const sql: TStrings);

var
  BoldSQLLogHandler: TBoldLogHandler = nil;
  BoldSQLLogCount: integer = 0;

implementation

uses
  Bolddefs,
  BoldSharedStrings,
  Variants,
  SysUtils,
  BoldUtils,
  BoldPMConsts;

procedure BoldLogSQL(const sql: TStrings);
var
  i: integer;
begin
  Inc(BoldSQLLogCount);
  if assigned(BoldSQLLogHandler) then
  begin
    BoldSQLLogHandler.Log(
      formatDateTime('c: ', now) + // do not localize
      format('SQL %3d- %s', [BoldSQLLogCount, trim(SQL[0])])); // do not localize
    for i := 1 to SQL.Count - 1 do
    begin
      if trim(SQL[i]) <> '' then
        BoldSQLLogHandler.Log('                                                    '  +
                              Trim(SQL[i]));
    end;
  end;
end;

{ TBoldDbParameter }

procedure TBoldDbParameter.AssignFieldValue(source: IBoldField);
begin
  Parameter.AssignFieldValue(Source.Field, Source.AsVariant);
end;

procedure TBoldDbParameter.Clear;
begin
  Parameter.Clear;
end;

constructor TBoldDbParameter.create(DbParameter: TParam; DatasetWrapper: TBoldDatasetWrapper);
begin
  inherited Create(DatasetWrapper);
  fParameter := DbParameter;
end;

destructor TBoldDbParameter.destroy;
begin
  inherited;
end;

function TBoldDbParameter.GetAsBCD: Currency;
begin
  result := Parameter.AsBCD;
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

procedure TBoldDbParameter.SetAsBlob(const Value: TBlobData);
begin
  Parameter.AsBlob := Value;
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

procedure TBoldDbParameter.SetAsTime(const Value: TDateTime);
begin
  Parameter.AsTime := Value;
end;

procedure TBoldDbParameter.SetAsVariant(const NewValue: Variant);
begin
  Parameter.Value := NewValue;
end;

//procedure TBoldDbParameter.SetAsWideString(const Value: Widestring);
//begin
//  Parameter.AsWi TParam
//end;

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
  DataSet.Close;
end;

constructor TBoldDataSetWrapper.Create(DatabaseWrapper: TBoldDatabaseWrapper);
begin
  inherited Create;
  fDatabaseWrapper := DatabaseWrapper;
end;

function TBoldDataSetWrapper.Createparam(FldType: TFieldType; const ParamName: string; ParamType: TParamType; Size: integer): IBoldParameter;
begin
  raise EBold.CreateFmt(sCreateParamNotImplemented, [classname]);
end;

procedure TBoldDataSetWrapper.Delete;
begin
  Dataset.Delete;
end;

procedure TBoldDataSetWrapper.Edit;
begin
  DataSet.Edit;
end;

function TBoldDataSetWrapper.FieldByName(const FieldName: string): IBoldField;
var
  Wrapper: TBoldFieldWrapper;
begin
  Dataset.FieldList.Update;
  if (fLastKnowFieldIndex + 1 >= 0) and
    (fLastKnowFieldIndex + 1 <= DataSet.FieldCount - 1) and
    (DataSet.FieldList.strings[fLastKnowFieldIndex + 1] = FieldName) then
  begin
    inc(fLastKnowFieldIndex);
  end
  else
    if (fLastKnowFieldIndex < 0) or
      (fLastKnowFieldIndex > DataSet.FieldCount - 1) or
      (DataSet.FieldList.Strings[fLastKnowFieldIndex] <> FieldName) then
    begin
      fLastKnowFieldIndex := DataSet.FieldList.FieldByName(FieldName).FieldNo - 1;
    end;

  Wrapper := GetFieldWrapperClass.Create(DataSet.FieldList.Fields[fLastKnowFieldIndex], self);
  Wrapper.GetInterface(IBoldField, result);
end;

function TBoldDataSetWrapper.FindField(const FieldName: string): IBoldField;
var
  Wrapper: TBoldFieldWrapper;
  Field: TField;
begin
  Field := DataSet.FindField(FieldName);
  if assigned(field) then
  begin
    Wrapper := GetFieldWrapperClass.Create(Field, self);
    Wrapper.GetInterface(IBoldField, result);
  end
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
var
  Wrapper: TBoldFieldWrapper;
begin
  Wrapper := GetFieldWrapperClass.Create(DataSet.Fields[Index], self);
  Wrapper.GetInterface(IBoldField, Result);
end;

function TBoldDataSetWrapper.GetFieldValue(const FieldName: string): Variant;
begin
  result := DataSet.FieldValues[FieldName];
end;

function TBoldDataSetWrapper.GetFieldWrapperClass: TBoldFieldWrapperClass;
begin
  result := TBoldFieldWrapper;
end;

function TBoldDataSetWrapper.GetImplementor: TObject;
begin
  result := self;
end;

function TBoldDataSetWrapper.GetState: TDataSetState;
begin
  result := DataSet.State;
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
  fLastKnowFieldIndex := -1;
end;

procedure TBoldDataSetWrapper.Post;
begin
  DataSet.Post;
end;

procedure TBoldDataSetWrapper.SetFieldValue(const FieldName: string;
  const Value: Variant);
begin
  DataSet.FieldValues[FieldName] := value;
end;

{ TBoldFieldWrapper }

procedure TBoldFieldWrapper.SetAsBlob(const Value: string);
begin
  Field.AsVariant := Value;
end;

constructor TBoldFieldWrapper.create(Field: TField; DatasetWrapper: TBoldDatasetWrapper);
begin
  inherited create;
  fField := Field;
  fSavedValue := Field.Value;
  fDatasetWrapper := DatasetWrapper;
end;

function TBoldFieldWrapper.GetAsBlob: string;
begin
  result := Field.AsVariant;
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

function TBoldFieldWrapper.GetAsTime: TDateTime;
begin
  result := frac(Field.AsDateTime);
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

constructor TBoldDatabaseWrapper.create(SQLDatabaseConfig: TBoldSQLDatabaseConfig);
begin
  inherited Create;
  fSQLDatabaseConfig := TBoldSQLDataBaseConfig.Create;
  fSQlDatabaseConfig.AssignConfig(SQLDatabaseConfig);
end;

destructor TBoldDatabaseWrapper.destroy;
begin
  FreeAndNil(fSQLDatabaseConfig);
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

function TBoldDatabaseWrapper.GetSQLDatabaseConfig: TBoldSQLDatabaseConfig;
begin
  result := fSQLDataBaseConfig;
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

function TBoldDatabaseWrapper.TableExists(
  const TableName: String): Boolean;
var
  TableNames: TStringList;
  i: integer;
begin
  TableNames := TStringList.Create;
  result := false;
  AllTableNames('*', true, TableNames);
  for i := 0 to Tablenames.Count-1 do
  begin
    if SameText(TableNames[i], TableName) then
      result := true;
  end;
  TableNames.Free;
end;

{ TBoldParameterWrapper }

constructor TBoldParameterWrapper.create(DatasetWrapper: TBoldDatasetWrapper);
begin
  inherited create;
  fDatasetWrapper := DatasetWrapper;
end;

end.
