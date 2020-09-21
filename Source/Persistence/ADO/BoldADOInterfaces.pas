unit BoldADOInterfaces;

interface

uses
  Classes,
  Db,
  ADODB,
  BoldSQLDatabaseConfig,
  BoldDBInterfaces;

type
  { forward declarations }
  TBoldADOParameter = class;
  TBoldADOQuery = class;
  TBoldADOTable = class;  TBoldADOConnection = class;
  TBoldADOQueryClass = class of TBoldADOQuery;

  { TBoldADOParameter }
  TBoldADOParameter = class(TBoldParameterWrapper, IBoldParameter)
  private
    fParameter: TParameter;
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
    procedure SetAsSmallInt(Value: LongInt);
    procedure SetAsTime(const Value: TDateTime);
    procedure SetAsWord(Value: LongInt);
    procedure SetText(const Value: string);
    function GetParameter: TParameter;
    procedure AssignFieldValue(source: IBoldField);
    property Parameter: TParameter read GetParameter;
  public
    constructor create(AdoParameter: TParameter; DatasetWrapper: TBoldDatasetWrapper);
  end;

  { TBoldADOQuery }
  TBoldADOQuery = class(TBoldDataSetWrapper, IBoldQuery, IBoldExecQuery, IBoldParameterized)
  private
    fQuery: TADOQuery;
    function GetQuery: TADOQuery;
    procedure AssignParams(Sourceparams: TParams);
    function GetParamCount: integer;
    function GetParams(i: integer): IBoldParameter;
    function GetRequestLiveQuery: Boolean;
    function ParamByName(const Value: string): IBoldParameter;
    function Createparam(FldType: TFieldType; const ParamName: string; ParamType: TParamType; Size: integer): IBoldParameter;
    procedure SetRequestLiveQuery(NewValue: Boolean);
    function GetSQLText: String;
    procedure AssignSQL(SQL: TStrings); virtual;
    procedure AssignSQLText(SQL: String);
    function GetRowsAffected: integer;
    function GetRecordCount: integer;
  protected
    procedure StartSQLBatch; virtual;
    procedure EndSQLBatch; virtual;
    procedure FailSQLBatch; virtual;
    function GetDataSet: TDataSet; override;
    procedure ClearParams;
    procedure ExecSQL; virtual;
    procedure Open; override;
    property Query: TADOQuery read GetQuery;
  public
    constructor Create(Query: TADOQuery; DatabaseWrapper: TBoldDatabaseWrapper);
  end;

  { TBoldADOTable }
  TBoldADOTable = class(TBoldDataSetWrapper, IBoldTable)
  private
    fTable: TADOTable;
    function GetTable: TADOTable;
    property Table: TADOTable read GetTable;
    procedure AddIndex(const Name, Fields: string; Options: TIndexOptions; const DescFields: string = '');
    procedure CreateTable;
    procedure DeleteTable;
    function GetIndexDefs: TIndexDefs;
    procedure SetTableName(NewName: String);
    function GetTableName: String;
    procedure SetExclusive(NewValue: Boolean);
    function GetExclusive: Boolean;
    function GetExists: Boolean;
  protected
    function GetDataSet: TDataSet; override;
  public
    constructor Create(Table: TADOTable; DatabaseWrapper: TBoldDatabaseWrapper);
  end;

  { TBoldADOConnection }
  TBoldADOConnection = class(TBoldDatabaseWrapper, IBoldDataBase)
    fDataBase: TADOConnection;
    fCachedTable: TADOTable;
    fCachedQuery: TADOQuery;
    function GetDataBase: TADOConnection;
    property DataBase: TADOConnection read GetDataBase;
    function GetConnected: Boolean;
    function GetInTransaction: Boolean;
    function GetIsSQLBased: Boolean;
    procedure SetlogInPrompt(NewValue: Boolean);
    function GetLogInPrompt: Boolean;
    procedure SetKeepConnection(NewValue: Boolean);
    function GetKeepConnection: Boolean;
    procedure StartTransaction;
    procedure Commit;
    procedure RollBack;
    procedure Open;
    procedure Close;

    function GetTable: IBoldTable;
    procedure ReleaseTable(var Table: IBoldTable);
    function SupportsTableCreation: Boolean;
    procedure ReleaseCachedObjects;
  protected
    procedure AllTableNames(Pattern: String; ShowSystemTables: Boolean; TableNameList: TStrings); override;
    function GetQuery: IBoldQuery; override;
    procedure ReleaseQuery(var Query: IBoldQuery); override;
  public
    constructor Create(DataBase: TADOConnection; SQLDataBaseConfig: TBoldSQLDatabaseConfig);
    destructor Destroy; override;
  end;

var
  BoldADOQueryClass: TBoldADOQueryClass = TBoldADOQuery;

implementation

uses
  BoldDefs,
  BoldUtils,
  SysUtils,
  Variants,
  Masks,
  BoldGuard,
  BoldCoreConsts;

{ TBoldADOQuery }

procedure TBoldADOQuery.AssignParams(SourceParams: TParams);
var
  i: integer;
begin
  Query.Parameters.Clear;
  if assigned(SourceParams) and (SourceParams.Count > 0) then
  begin
    for i := 0 to Sourceparams.Count - 1 do
      Query.Parameters.CreateParameter(Sourceparams[i].Name, Sourceparams[i].DataType, pdInput, 0, SourceParams[i].Value);
  end;
end;

procedure TBoldADOQuery.AssignSQL(SQL: TStrings);
var
  OldConnection: TADOConnection;
  BackupQuery: TADOQuery;
  Guard: IBoldGuard;
begin
  Guard := TBoldGuard.Create(BackupQuery);
  // ADO sometimes tries to bind the params at this point, unless we disconnect the connection first
  OldConnection := Query.Connection;
  if Query.Parameters.count > 0 then
  begin
    BackupQuery := TADOQuery.Create(nil);
    BackupQuery.Parameters.Assign(Query.Parameters);
  end;

  Query.Close; // safe operation even if the query is closed. Open queries will cause "invalid operation" on next line
  Query.Connection := nil;
  Query.SQL.BeginUpdate;
  Query.SQL.Assign(SQL);
  Query.SQL.EndUpdate;
  Query.Parameters.ParseSQL(SQL.Text, true);
  if assigned(BackupQuery) then
    Query.Parameters.AssignValues(BackupQuery.Parameters);
  Query.Connection := OldConnection;
end;

procedure TBoldADOQuery.AssignSQLText(SQL: String);
var
  StringList: TStringList;
  Guard: IBoldguard;
begin
  Guard := tBoldGuard.Create(StringList);
  StringList := TStringList.create;
  BoldAppendToStrings(StringList, SQL, true);
  AssignSQL(StringList);
end;

procedure TBoldADOQuery.ClearParams;
begin
  Query.Parameters.Clear;
end;

constructor TBoldADOQuery.Create(Query: TADOQuery; DatabaseWrapper: TBoldDatabaseWrapper);
begin
  inherited create(DatabaseWrapper);
  fQuery := Query;
end;

function TBoldADOQuery.Createparam(FldType: TFieldType; const ParamName: string; ParamType: TParamType; Size: integer): IBoldParameter;
const
  ParamDir: Array[TparamType] of TParameterDirection = (pdUnknown, pdInput, pdOutput, pdInputOutput, pdReturnValue);
begin
  result := TBoldADOParameter.Create(Query.Parameters.CreateParameter(ParamName, fldType, ParamDir[ParamType], Size, null), self);
end;

procedure TBoldADOQuery.EndSQLBatch;
begin
  // intentionally left blank
end;

procedure TBoldADOQuery.ExecSQL;
begin
  BoldLogSQL(Query.SQL);
  try
    Query.ExecSQL;
  except
    on e: Exception do
    begin
      e.Message := e.Message + BOLDCRLF + 'SQL: ' + Query.SQL.text; // do not localize
      raise;
    end;
  end;
end;

procedure TBoldADOQuery.FailSQLBatch;
begin
  // intentionally left blank
end;

function TBoldADOQuery.GetDataSet: TDataSet;
begin
  result := Query;
end;

function TBoldADOQuery.GetParamCount: integer;
begin
  result := Query.Parameters.count;
end;

function TBoldADOQuery.GetParams(i: integer): IBoldParameter;
begin
  result := TBoldADOParameter.Create(Query.parameters[i], self);
end;

function TBoldADOQuery.GetQuery: TADOQuery;
begin
  result := fQuery;
end;

function TBoldADOQuery.GetRecordCount: integer;
begin
  result := Query.RecordCount;
end;

function TBoldADOQuery.GetRequestLiveQuery: Boolean;
begin
  result := false;
end;

function TBoldADOQuery.GetRowsAffected: integer;
begin
  result := Query.RowsAffected;
end;

function TBoldADOQuery.GetSQLText: String;
begin
  result := Query.SQL.Text;
end;

procedure TBoldADOQuery.Open;
begin
  BoldLogSQL(Query.SQL);
  try
    Query.CacheSize := 10000;
    Query.CursorType := ctOpenForwardOnly;
    Query.LockType := ltReadOnly;
    inherited;
  except
    on e: Exception do
    begin
      e.Message := e.Message + BOLDCRLF + 'SQL: ' + Query.SQL.text; // do not localize
      raise;
    end;
  end;
end;

function TBoldADOQuery.ParamByName(const Value: string): IBoldParameter;
var
  Param: TParameter;
begin
  Param := Query.Parameters.ParamByName(Value);
  if assigned(Param) then
    result := TBoldADOParameter.Create(Param, self)
  else
    result := nil;
end;

procedure TBoldADOQuery.SetRequestLiveQuery(NewValue: Boolean);
begin
  // ignore
end;

procedure TBoldADOQuery.StartSQLBatch;
begin
  // intentionally left blank
end;

{ TBoldADOTable }

procedure TBoldADOTable.AddIndex(const Name, Fields: string;
  Options: TIndexOptions; const DescFields: string);
begin
  raise EBold.CreateFmt(sMethodNotImplemented, [ClassName, 'AddIndex']); // do not localize
end;

constructor TBoldADOTable.Create(Table: TADOTable; DatabaseWrapper: TBoldDatabaseWrapper);
begin
  inherited create(DatabaseWrapper);
  fTable := Table;
end;

procedure TBoldADOTable.CreateTable;
begin
  raise EBold.CreateFmt(sMethodNotImplemented, [ClassName, 'CreateTable']); // do not localize
end;

procedure TBoldADOTable.DeleteTable;
begin
  raise EBold.CreateFmt(sMethodNotImplemented, [ClassName, 'DeleteTable']); // do not localize
end;

function TBoldADOTable.GetDataSet: TDataSet;
begin
  result := fTable;
end;

function TBoldADOTable.GetExclusive: Boolean;
begin
  result := false;
end;

function TBoldADOTable.GetExists: Boolean;
var
  AllTables: TStringList;
  Guard: IBoldGuard;
begin
  Guard := TBoldGuard.Create(AllTables);
  Result := False;

  // First we make sure we have a table component and that it is connected to a database
  if Assigned(Table) and Assigned(Table.Connection) then
  begin
    // We now create a list that will hold all the table names in the database
    Alltables := TStringList.Create;
    Table.Connection.GetTableNames(AllTables);
    Result := AllTables.IndexOf(GetTableName) <> -1;
  end;
end;

function TBoldADOTable.GetIndexDefs: TIndexDefs;
begin
  result := Table.IndexDefs;
end;

function TBoldADOTable.GetTable: TADOTable;
begin
  result := fTable;
end;

function TBoldADOTable.GetTableName: String;
begin
  result := Table.TableName;
end;

procedure TBoldADOTable.SetExclusive(NewValue: Boolean);
begin
  if NewValue then
    Table.LockType := ltPessimistic
  else
    Table.LockType := ltOptimistic;
end;

procedure TBoldADOTable.SetTableName(NewName: String);
begin
  Table.TableName := NewName;
end;

{ TBoldADOConnection }

// Populate the "TableNameList" with tablenames from the database that maches "pattern"
procedure TBoldADOConnection.AllTableNames(Pattern: String; ShowSystemTables: Boolean; TableNameList: TStrings);
var
  TempList: TStringList;
  i: integer;
  TempPattern: String;
  Guard: IBoldGuard;
begin
  Guard := TBoldGuard.Create(TempList);
  TempList := TStringList.Create;
  if Pattern = '' then
    TempPattern := '*'
  else
    TempPattern := Pattern;

  // Retrieve the list of table names
  // Note: This does not include views or procedures, there is a specific
  //       method in TADOConnection for that
  GetDataBase.GetTableNames(TempList, ShowSystemTables);

  // MatchesMask is used to compare filenames with wildcards, suits us here
  // but there should be some care taken, when using tablenames with period
  // signes, as that might be interpreted as filename extensions
  for i := 0 to TempList.Count-1 do
    if MatchesMask(TempList[i], tempPattern) then
      TableNameList.Add(TempList[i]);
end;

procedure TBoldADOConnection.Commit;
begin
  DataBase.CommitTrans;
end;

function TBoldADOConnection.GetInTransaction: Boolean;
begin
  result := DataBase.InTransaction;
end;

function TBoldADOConnection.GetIsSQLBased: Boolean;
begin
  result := true;
end;

function TBoldADOConnection.GetKeepConnection: Boolean;
begin
  //CheckMe;
  result := true;
end;

function TBoldADOConnection.GetLogInPrompt: Boolean;
begin
  result := DataBase.LoginPrompt;
end;

procedure TBoldADOConnection.RollBack;
begin
  DataBase.RollBackTrans;
end;

procedure TBoldADOConnection.SetKeepConnection(NewValue: Boolean);
begin
  //CheckMe;
end;

procedure TBoldADOConnection.SetlogInPrompt(NewValue: Boolean);
begin
  DataBase.LoginPrompt := NewValue;
end;

procedure TBoldADOConnection.StartTransaction;
begin
  DataBase.BeginTrans;
end;

destructor TBoldADOConnection.Destroy;
begin
//  FreeAndNil(fParameters);
  FreeAndNil(fCachedQuery);
  FreeAndNil(fCachedTable);
  inherited;
end;

constructor TBoldADOConnection.create(DataBase: TADOConnection; SQLDataBaseConfig: TBoldSQLDatabaseConfig);
begin
  inherited create(SQLDataBaseConfig);
  fDatabase := DataBase;
end;

procedure TBoldADOConnection.Close;
begin
  DataBase.Close;
end;

function TBoldADOConnection.GetConnected: Boolean;
begin
  result := DataBase.Connected;
end;

function TBoldADOConnection.GetDataBase: TADOConnection;
begin
  result := fDataBase;
end;

function TBoldADOConnection.GetQuery: IBoldQuery;
var
  Query: TADOQuery;
begin
  if assigned(fCachedQuery) then
  begin
    Query := fCachedQuery;
    fCachedQuery := nil;
  end
  else
  begin
    Query := TADOQuery.Create(nil);
    Query.Connection := DataBase;
  end;
  result := BoldADOQueryClass.Create(Query, self);
end;

function TBoldADOConnection.GetTable: IBoldTable;
var
  Table: TADOTable;
begin
  if assigned(fCachedTable) then
  begin
    Table := fCachedTable;
    fCachedTable := nil;
  end
  else
  begin
    Table := TADOTable.Create(nil);
    Table.Connection := DataBase;
  end;
  result := TBoldADOTable.Create(Table, self);
end;

procedure TBoldADOConnection.Open;
begin
  DataBase.Open;
end;

procedure TBoldADOConnection.ReleaseQuery(var Query: IBoldQuery);
var
  ADOQuery: TBoldADOQuery;
begin
  if Query.Implementor is TBoldADOQuery then
  begin
    ADOQuery := Query.Implementor as TBoldADOQuery;
    Query := nil;
    if not assigned(fCachedQuery) then
    begin
      fCachedQuery := ADOQuery.fQuery;
      if fCachedQuery.Active then
        fCachedQuery.Close;
      fCachedQuery.SQL.Clear;
    end
    else
      ADOQuery.fQuery.free;
    ADOQuery.Free;
  end;
end;

procedure TBoldADOConnection.ReleaseTable(var Table: IBoldTable);
var
  ADOTable: TBoldADOTable;
begin
  if Table.Implementor is TBoldADOTable then
  begin
    ADOTable := Table.Implementor as TBoldADOTable;
    Table := nil;
    if not assigned(fCachedTable) then
    begin
      fCachedTable := ADOTable.fTable;
    end
    else
      ADOTable.fTable.free;
    ADOTable.Free;
  end;
end;

function TBoldADOConnection.SupportsTableCreation: Boolean;
begin
  result := false;
end;

{ TBoldADOParameter }

procedure TBoldADOParameter.Clear;
begin
  // FIXME
end;

constructor TBoldADOParameter.create(AdoParameter: TParameter; DatasetWrapper: TBoldDatasetWrapper);
begin
  inherited Create(DatasetWrapper);
  fParameter := AdoParameter;
end;

function TBoldADOParameter.GetAsBCD: Currency;
begin
  result := parameter.Value;
end;

function TBoldADOParameter.GetAsBoolean: Boolean;
begin
  result := parameter.Value;
end;

function TBoldADOParameter.GetAsCurrency: Currency;
begin
  result := parameter.Value;
end;

function TBoldADOParameter.GetAsDateTime: TDateTime;
begin
  result := parameter.Value;
end;

function TBoldADOParameter.GetAsFloat: Double;
begin
  result := parameter.Value;
end;

function TBoldADOParameter.GetAsInteger: Longint;
begin
  result := parameter.Value;
end;

function TBoldADOParameter.GetAsMemo: string;
begin
  result := parameter.Value;
end;

function TBoldADOParameter.GetAsString: string;
begin
  result := parameter.Value;
  if result = DatasetWrapper.DatabaseWrapper.SQLDatabaseConfig.EmptyStringMarker then
    result := '';
end;

function TBoldADOParameter.GetAsVariant: Variant;
begin
  result := parameter.Value;
end;

function TBoldADOParameter.GetDataType: TFieldType;
begin
  result := parameter.DataType;
end;

function TBoldADOParameter.GetIsNull: Boolean;
begin
  result := VarIsNull(Parameter.value)
end;

function TBoldADOParameter.GetName: String;
begin
  result := Parameter.Name;
end;

function TBoldADOParameter.GetParameter: TParameter;
begin
  result := fParameter;
end;

procedure TBoldADOParameter.SetAsBCD(const Value: Currency);
begin
  Parameter.Value := Value;
end;

procedure TBoldADOParameter.SetAsBlob(const Value: TBlobData);
begin
  if Parameter.DataType = ftUnknown then
    Parameter.DataType := ftBlob;
  if value = '' then
    Parameter.Value := DatasetWrapper.DatabaseWrapper.SQLDatabaseConfig.EmptyStringMarker
  else
    Parameter.Value := Value;
end;

procedure TBoldADOParameter.SetAsBoolean(Value: Boolean);
begin
  Parameter.Value := Value;
end;

procedure TBoldADOParameter.SetAsCurrency(const Value: Currency);
begin
  Parameter.Value := Value;
end;

procedure TBoldADOParameter.SetAsDate(const Value: TDateTime);
begin
  Parameter.Value := Value;
end;

procedure TBoldADOParameter.SetAsDateTime(const Value: TDateTime);
begin
  Parameter.Value := Value;
end;

procedure TBoldADOParameter.SetAsFloat(const Value: Double);
begin
  Parameter.Value := Value;
end;

procedure TBoldADOParameter.SetAsInteger(Value: Integer);
begin
  Parameter.Value := Value;
end;

procedure TBoldADOParameter.SetAsMemo(const Value: string);
begin
  Parameter.Value := Value;
end;

procedure TBoldADOParameter.SetAsSmallInt(Value: Integer);
begin
  Parameter.Value := Value;
end;

procedure TBoldADOParameter.SetAsString(const Value: string);
begin
  if value = '' then
    Parameter.Value := DatasetWrapper.DatabaseWrapper.SQLDatabaseConfig.EmptyStringMarker
  else
    Parameter.Value := Value
end;

procedure TBoldADOParameter.SetAsTime(const Value: TDateTime);
begin
  Parameter.Value := Value;
end;

procedure TBoldADOParameter.SetAsVariant(const NewValue: Variant);
begin
  Parameter.Value := NewValue;
end;

procedure TBoldADOParameter.SetAsWord(Value: Integer);
begin
  Parameter.Value := Value;
end;

procedure TBoldADOParameter.SetDataType(Value: TFieldType);
begin
  Parameter.DataType := Value;
end;

procedure TBoldADOParameter.SetText(const Value: string);
begin
  Parameter.Value := Value;
end;

procedure TBoldADOParameter.AssignFieldValue(source: IBoldField);
begin
  Parameter.Assign(Source.Field);
end;

procedure TBoldADOConnection.ReleaseCachedObjects;
begin
  FreeAndNil(fCachedTable);
  FreeAndNil(fCachedQuery);
end;

end.




