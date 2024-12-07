{ Global compiler directives }
{$include bold.inc}
unit BoldUniDACInterfaces;

interface

uses
  Classes,
  Db,
  SysUtils,  
  Uni,
  UniProvider,
  MemDS,
  BoldSQLDatabaseConfig,
  BoldDBInterfaces,
  BoldDefs;

type
  { forward declarations }
  TBoldUniDACParameter = class;
  TBoldUniDACQuery = class;
  TBoldUniDACTable = class;
  TBoldUniDACConnection = class;

  TBoldUniDACQueryClass = class of TBoldUniDACQuery;
  TBoldUniDACExecQueryClass = class of TBoldUniDACExecQuery;

  { TBoldUniDACParameter }
  TBoldUniDACParameter = class(TBoldParameterWrapper, IBoldParameter)
  private
    fUniParam: TUniParam;
    function GetAsVariant: Variant;
    procedure SetAsVariant(const NewValue: Variant);
    function GetName: string;
    procedure Clear;
    function GetDataType: TFieldType;
    procedure SetDataType(Value: TFieldType);
    function GetAsBCD: Currency;
    function GetAsblob: TBoldBlobData;    
    function GetAsBoolean: Boolean;
    function GetAsDateTime: TDateTime;
    function GetAsCurrency: Currency;
    function GetAsFloat: Double;
    function GetAsInteger: Longint;
    function GetAsInt64: Int64;
    function GetAsMemo: string;
    function GetAsString: string;
    function GetIsNull: Boolean;
    function GetAsWideString: WideString;
    procedure SetAsBCD(const Value: Currency);
    procedure SetAsBlob(const Value: TBoldBlobData);
    procedure SetAsBoolean(Value: Boolean);
    procedure SetAsCurrency(const Value: Currency);
    procedure SetAsDate(const Value: TDateTime);
    procedure SetAsDateTime(const Value: TDateTime);
    procedure SetAsFloat(const Value: Double);
    procedure SetAsInteger(Value: Longint);
    procedure SetAsInt64(const Value: Int64);    
    procedure SetAsMemo(const Value: string);
    procedure SetAsString(const Value: string);
    procedure SetAsSmallInt(Value: Longint);
    procedure SetAsTime(const Value: TDateTime);
    procedure SetAsWord(Value: Longint);
    procedure SetText(const Value: string);
    procedure SetAsWideString(const Value: Widestring);
    function GetAsAnsiString: TBoldAnsiString;
    procedure SetAsAnsiString(const Value: TBoldAnsiString);
    function GetUniParam: TUniParam;
    procedure AssignFieldValue(const source: IBoldField);
    procedure Assign(const source: IBoldParameter);
    property UniParam: TUniParam read GetUniParam;
  public
    constructor Create(UniDACParameter: TUniParam; DatasetWrapper: TBoldAbstractQueryWrapper);
  end;

  { TBoldUniDACQuery }
  TBoldUniDACQuery = class(TBoldBatchDataSetWrapper, IBoldQuery, IBoldExecQuery, IBoldParameterized)
  private
    fQuery: TUniQuery;
    fReadTransactionStarted: Boolean;
    fUseReadTransactions: boolean;
    function GetQuery: TUniQuery; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    procedure AssignParams(Sourceparams: TParams);
    function GetParamCount: Integer;
    function GetParam(i: Integer): IBoldParameter;  {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    function GetParamCheck: Boolean;
    procedure SetParamCheck(value: Boolean);
    function GetRequestLiveQuery: Boolean;
    procedure SetRequestLiveQuery(NewValue: Boolean);
    procedure AssignSQL(SQL: TStrings); virtual;
    function GetRecordCount: Integer;
    function GetUseReadTransactions: boolean;
    procedure SetUseReadTransactions(value: boolean);
    procedure BeginExecuteQuery;
    procedure EndExecuteQuery;
  protected
    function ParamByName(const Value: string): IBoldParameter; override;
    function FindParam(const Value: string): IBoldParameter; override;
    function CreateParam(FldType: TFieldType; const ParamName: string; ParamType: TParamType; Size: integer): IBoldParameter; override;
    function GetParams: TParams; override;
    function GetSQLStrings: TStrings; override;
    function GetSqlText: string; override;
    procedure AssignSQLText(const SQL: string); override;
    function GetRowsAffected: Integer;
    function GetDataSet: TDataSet; override;
    procedure ClearParams;
    procedure Open; override;
    procedure Close; override;
    procedure ExecSQL; override;
    function GetRecNo: integer; override;
    procedure Prepare;
    property Query: TUniQuery read GetQuery;
  public
    constructor Create(BoldUniDACConnection: TBoldUniDACConnection); reintroduce;
    destructor Destroy; override;
    procedure Clear; override;
  end;

  { TBoldUniDACQuery }
  TBoldUniDACExecQuery = class(TBoldBatchDataSetWrapper{TBoldAbstractQueryWrapper}, IBoldExecQuery, IBoldParameterized)
  private
    fExecQuery: TUniSQL;
    fReadTransactionStarted: Boolean;
    fUseReadTransactions: boolean;
  protected
    function GetExecQuery: TUniSQL;
    function GetParams: TParams;  override;
    procedure AssignParams(Sourceparams: TParams);
    function GetParamCount: Integer;
    function GetParam(i: Integer): IBoldParameter;
    function GetParamCheck: Boolean;
    procedure SetParamCheck(value: Boolean);
    function ParamByName(const Value: string): IBoldParameter; override;
    function FindParam(const Value: string): IBoldParameter; override;
    function CreateParam(FldType: TFieldType; const ParamName: string): IBoldParameter; overload; override;
    function CreateParam(FldType: TFieldType; const ParamName: string; ParamType: TParamType; Size: integer): IBoldParameter; overload; override;
    function EnsureParamByName(const Value: string): IBoldParameter; override;
    function GetSqlText: string; override;
    function GetSQLStrings: TStrings; override;
    procedure AssignSQL(SQL: TStrings); virtual;
    procedure AssignSQLText(const SQL: string);  override;
    function GetRowsAffected: Integer;
    function GetUseReadTransactions: boolean;
    procedure SetUseReadTransactions(value: boolean);
    procedure BeginExecuteQuery;
    procedure EndExecuteQuery;
    function GetBatchQueryParamCount: integer;
    procedure Prepare;
    function GetDataSet: TDataSet; override;
    procedure ClearParams;
    procedure ExecSQL; override;
    property ExecQuery: TUniSQL read GetExecQuery;
  public
    constructor Create(BoldUniDACConnection: TBoldUniDACConnection); reintroduce;
    destructor Destroy; override;
    procedure Clear; override;
  end;

  { TBoldUniDACTable }
  TBoldUniDACTable = class(TBoldDatasetWrapper, IBoldTable)
  private
    fUniTable: TUniTable;
    function GetUniTable: TUniTable;
    property UniTable: TUniTable read GetUniTable;
    procedure AddIndex(const Name, Fields: string; Options: TIndexOptions; const DescFields: string = '');
    procedure CreateTable;
    procedure DeleteTable;
    function GetIndexDefs: TIndexDefs;
    procedure SetTableName(const NewName: string);
    function GetTableName: string;
    procedure SetExclusive(NewValue: Boolean);
    function GetExclusive: Boolean;
    function GetExists: Boolean;
//    function GetCommaListOfIndexesForColumn(const aColumnName: string): string;
//    function GetPrimaryIndex: string;
  protected
    function GetDefaultConstraintNameForColumn(const aColumnName: string): string; {override;}
    function GetDataSet: TDataSet; override;
    function ParamByName(const Value: string): IBoldParameter; override;
    function FindParam(const Value: string): IBoldParameter; override;
  public
    constructor Create(aUniTable: TUniTable; BoldUniDACConnection: TBoldUniDACConnection); reintroduce;
  end;

  { TBoldUniDACConnection }
  TBoldUniDACConnection = class(TBoldDatabaseWrapper, IBoldDataBase)
  private
    fUniConnection: TUniConnection;
    fCachedTable: IBoldTable;
    fCachedQuery1: IBoldQuery;
    fCachedQuery2: IBoldQuery;
    fCachedExecQuery1: IBoldExecQuery;
    fExecuteQueryCount: integer;
    function GetUniConnection: TUniConnection;
    property UniConnection: TUniConnection read GetUniConnection;
    function GetConnected: Boolean;
    function GetInTransaction: Boolean;
    function GetIsSQLBased: Boolean;
    procedure SetlogInPrompt(NewValue: Boolean);
    function GetLogInPrompt: Boolean;
    procedure SetKeepConnection(NewValue: Boolean);
    function GetKeepConnection: Boolean;
    procedure StartTransaction;
    procedure StartReadTransaction;
    procedure Commit;
    procedure RollBack;
    procedure Open;
    procedure Close;
    procedure Reconnect;
    function SupportsTableCreation: Boolean;
    procedure ReleaseCachedObjects;
    function GetIsExecutingQuery: Boolean;
    procedure BeginExecuteQuery;
    procedure EndExecuteQuery;
    function CreateAnotherDatabaseConnection: IBoldDatabase;
    function GetImplementor: TObject;
  protected
    procedure AllTableNames(Pattern: string; ShowSystemTables: Boolean; TableNameList: TStrings); override;
    function GetTable: IBoldTable; override;
    function GetQuery: IBoldQuery; override;
    function GetExecQuery: IBoldExecQuery; override;
    procedure ReleaseTable(var Table: IBoldTable); override;
    procedure ReleaseQuery(var Query: IBoldQuery); override;
    procedure ReleaseExecQuery(var Query: IBoldExecQuery); override;
  public
    constructor Create(aUniConnection: TUniConnection; SQLDataBaseConfig: TBoldSQLDatabaseConfig);
    destructor Destroy; override;
    procedure CreateDatabase(DropExisting: boolean = true); override;
    procedure DropDatabase; override;
    function DatabaseExists: boolean; override;
    function GetDatabaseError(const E: Exception; const sSQL: string = ''):
        EBoldDatabaseError;
    property ExecuteQueryCount: integer read fExecuteQueryCount;            
  end;

var
  BoldUniDACQueryClass: TBoldUniDACQueryClass = TBoldUniDACQuery;
  BoldUniDACExecQueryClass: TBoldUniDACExecQueryClass = TBoldUniDACExecQuery;

implementation

uses
  Variants,
  Masks,

  BoldUtils,
  BoldGuard,
{$IFDEF LAZYFETCHDEBUG}
  BoldSystem,
  AttracsSpanFetchManager,
  BoldControlpack,
{$ENDIF}
  {$IFDEF ATTRACS}
  AttracsDefs,
  AttracsPerformance,
  AttracsTraceLog,
  {$IFDEF BOLD_PERFORMANCE_COUNTERS}
  BoldSystemPerf,
  {$ENDIF}
  {$ENDIF}
  BoldCoreConsts,
  CRAccess,
  UniScript;

{ TBoldUniDACQuery }

function TBoldUniDACQuery.GetQuery: TUniQuery;
begin
  if not Assigned(fQuery) then
  begin
    fQuery := TUniQuery.Create(nil);
    fQuery.Connection := (DatabaseWrapper as TBoldUniDACConnection).UniConnection;
  end;
  Result := fQuery;
end;

function TBoldUniDACQuery.GetDataSet: TDataSet;
begin
  Result := Query;
end;

function TBoldUniDACQuery.GetParamCheck: Boolean;
begin
  result := Query.ParamCheck;
end;

function TBoldUniDACQuery.GetParamCount: Integer;
begin
  Result := Query.Params.Count;
end;

function TBoldUniDACQuery.GetParams: TParams;
begin
  result := Query.Params;
end;

function TBoldUniDACQuery.GetParam(i: Integer): IBoldParameter;
begin
  Result := TBoldUniDACParameter.Create(Query.Params[i], Self);
end;

function TBoldUniDACQuery.GetRecNo: integer;
begin
  result := Query.RecNo - 1;
end;

function TBoldUniDACQuery.GetRecordCount: Integer;
begin
  Result := Query.RecordCount;
end;

function TBoldUniDACQuery.GetRequestLiveQuery: Boolean;
begin
  Result := False;
end;

function TBoldUniDACQuery.GetRowsAffected: Integer;
begin
  result := Query.RowsAffected;
end;

function TBoldUniDACQuery.GetSQLStrings: TStrings;
begin
  result := Query.SQL;
end;

function TBoldUniDACQuery.GetSQLText: string;
begin
  Result := Query.SQL.Text;
end;

function TBoldUniDACQuery.GetUseReadTransactions: boolean;
begin
  result := fUseReadTransactions;
end;

procedure TBoldUniDACQuery.AssignParams(Sourceparams: TParams);
var
  lIndexSourceParams: Integer;
  lUniParam: TUniParam;
begin
  Query.Params.Clear;
  if Assigned(Sourceparams) and (Sourceparams.Count > 0) then
  begin
    for lIndexSourceParams := 0 to Sourceparams.Count - 1 do
    begin
      lUniParam := Query.Params.CreateParam(Sourceparams[lIndexSourceParams].DataType, Sourceparams[lIndexSourceParams].Name, Sourceparams[lIndexSourceParams].ParamType) as TUniParam;
      lUniParam.Value := Sourceparams[lIndexSourceParams].Value;
    end;
  end;
end;

procedure TBoldUniDACQuery.AssignSQL(SQL: TStrings);
begin
  Query.SQL.Assign(SQL);
  //function ParseSQL(SQL: WideString; DoCreate: Boolean): WideString;
  //DoCreate indicates whether to clear all existing parameter definitions before parsing the SQL statement.
end;

procedure TBoldUniDACQuery.AssignSQLText(const SQL: string);
begin
  Query.SQL.Text := Sql;
{  if SQL = '' then
    Query.Params.clear
  else
    Query.Params.ParseSQL(SQL, False);}
end;

procedure TBoldUniDACQuery.BeginExecuteQuery;
begin
  (DatabaseWrapper as TBoldUniDACConnection).BeginExecuteQuery;
end;

procedure TBoldUniDACQuery.Clear;
begin
  AssignSQLText('');
  ClearParams;
end;

procedure TBoldUniDACQuery.ClearParams;
begin
  Query.Params.Clear;
end;

procedure TBoldUniDACQuery.Close;
begin
  inherited;
  if (fReadTransactionStarted) and (DatabaseWrapper as TBoldUniDACConnection).GetInTransaction then
    (DatabaseWrapper as TBoldUniDACConnection).Commit;
  fReadTransactionStarted := false;
end;


constructor TBoldUniDACQuery.Create(BoldUniDACConnection: TBoldUniDACConnection);
begin
  inherited Create(BoldUniDACConnection);
  fUseReadTransactions := true;
  fQuery := TUniQuery.Create(nil);
  fQuery.Connection := (DatabaseWrapper as TBoldUniDACConnection).UniConnection;
end;

function TBoldUniDACQuery.CreateParam(FldType: TFieldType; const ParamName: string; ParamType: TParamType; Size: integer): IBoldParameter;
var
  lUniParam: TUniParam;
begin
  lUniParam := Query.Params.CreateParam(FldType, ParamName, ptUnknown) as TUniParam;
//  lUniParam.Size := Size;
  lUniParam.Value := NULL;
  Result := TBoldUniDACParameter.Create(lUniParam, Self);
end;

destructor TBoldUniDACQuery.Destroy;
begin
  if (fReadTransactionStarted) then
    Close;
  FreeAndNil(fQuery);
  inherited;
end;

procedure TBoldUniDACQuery.EndExecuteQuery;
begin
  (DatabaseWrapper as TBoldUniDACConnection).EndExecuteQuery;
end;

type TStringsAccess = class(TStrings);

procedure TBoldUniDACQuery.ExecSQL;
var
  Retries: Integer;
  Done: Boolean;
{$IFDEF ATTRACS}
  PerformanceMeasurement : TPerformanceMeasurement;
begin
  //PATCH for logging long running SQL
  PerformanceMeasurement := TPerformanceMeasurement.ReStart;
{$ELSE}
begin
{$ENDIF}
  if InBatch then
  begin
    BatchExecSQL;
    exit;
  end;
  BeginExecuteQuery;
  try
    BoldLogSQLWithParams(Query.SQL, self);
    Retries := 0;
    Done := false;
    while not Done do
    begin
      try
        if (DatabaseWrapper as TBoldUniDACConnection).GetInTransaction then
          fReadTransactionStarted := false
        else
        begin
          if fUseReadTransactions then
          (DatabaseWrapper as TBoldUniDACConnection).StartReadTransaction;
          fReadTransactionStarted := fUseReadTransactions;
        end;
        Query.Execute;
        if fReadTransactionStarted and (DatabaseWrapper as TBoldUniDACConnection).GetInTransaction then
        begin
         (DatabaseWrapper as TBoldUniDACConnection).Commit;
         fReadTransactionStarted := false;
        end;
        Done := true;
      except
        on e: Exception do
        begin
          if (DatabaseWrapper as TBoldUniDACConnection).GetInTransaction then
            (DatabaseWrapper as TBoldUniDACConnection).Rollback;
          if (not fReadTransactionStarted) or (Retries > 4) then
            raise TBoldUniDACConnection(DatabaseWrapper).GetDatabaseError(E, Query.SQL.Text);
          fReadTransactionStarted := false;
          INC(Retries);
          sleep(Retries*200);
        end;
      end;
    end;
  {$IFDEF ATTRACS}
    {$IFDEF BOLD_PERFORMANCE_COUNTERS}
    PerformanceMeasurement.EndMeasurement;
    BoldSystemPerfObject.BoldDBXQuery_ExecSQL(PerformanceMeasurement.TimeTaken);
    {$ENDIF}
    if not PerformanceMeasurement.AcceptableTimeForUserResponseTime then
    begin
      PerformanceMeasurement.WhatMeasured := 'TBoldUniDACQuery.ExecSQL';
      PerformanceMeasurement.WhatMeasuredParameter := Query.SQL.Text;
      if TraceLogAssigned then
        PerformanceMeasurement.Trace;
    end;
  {$ENDIF}
  finally
    EndExecuteQuery;
  end;
end;

function TBoldUniDACQuery.FindParam(const Value: string): IBoldParameter;
var
  Param: TUniParam;
begin
  result := nil;
  Param := Query.FindParam(Value);
  if Assigned(Param) then
    Result := TBoldUniDACParameter.Create(Param, Self);
end;

procedure TBoldUniDACQuery.Open;
var
  Retries: Integer;
  Done: Boolean;
  EDatabase: EBoldDatabaseError;
{$IFDEF ATTRACS}
  PerformanceMeasurement : TPerformanceMeasurement;
begin
  //PATCH for logging long running SQL
  PerformanceMeasurement := TPerformanceMeasurement.ReStart;
{$ELSE}
begin
{$ENDIF}
  BeginExecuteQuery;
  try
  BoldLogSQLWithParams(Query.SQL, self);
  Retries := 0;
  Done := false;
  while not Done do
  begin
    try
      if (DatabaseWrapper as TBoldUniDACConnection).GetInTransaction then
        fReadTransactionStarted := false
      else
      begin
        if fUseReadTransactions then
          (DatabaseWrapper as TBoldUniDACConnection).StartReadTransaction;
        fReadTransactionStarted := fUseReadTransactions;
      end;
      Query.ReadOnly := True;      
      inherited;
      Done := true;
{$IFDEF LAZYFETCHDEBUG}
      if (Query.RecordCount = 1) and (TBoldSystem.DefaultSystem <> nil) and not AttracsSpanFetchManager.InSpanFetch and not TBoldSystem.DefaultSystem.IsUpdatingDatabase {and not FollowerGettingValue} then
        Assert(Assigned(query)); // Fake assert for placing breakpoint when debugging inefficient loading
{$ENDIF}
    except
      on e: Exception do
      begin
        EDatabase := TBoldUniDACConnection(DatabaseWrapper).
            GetDatabaseError(E, Query.SQL.Text);
        if (EDatabase is EBoldDatabaseConnectionError) {and
           (not Assigned(ReconnectAppExists) or ReconnectAppExists)} then
        begin
          EDatabase.free;
//          ReconnectDatabase(Query.SQL.Text);
          Reconnect;
        end else
        begin
          if (DatabaseWrapper as TBoldUniDACConnection).GetInTransaction then
            (DatabaseWrapper as TBoldUniDACConnection).Rollback;
          if (not fReadTransactionStarted) or (Retries > 4) then
            raise EDatabase
          else
            EDatabase.free;
          fReadTransactionStarted := false;
          INC(Retries);
          sleep(Retries*200);
        end;
      end;
    end;
  end;
{$IFDEF ATTRACS}
  {$IFDEF BOLD_PERFORMANCE_COUNTERS}
  PerformanceMeasurement.EndMeasurement;
  BoldSystemPerfObject.BoldDBXQuery_Open(PerformanceMeasurement.TimeTaken);
  {$ENDIF}

  if not PerformanceMeasurement.AcceptableTimeForUserResponseTime then
  begin
    PerformanceMeasurement.WhatMeasured := 'TBoldUniDACQuery.Open';
    PerformanceMeasurement.WhatMeasuredParameter := Query.SQL.Text;
    if TraceLogAssigned then
      PerformanceMeasurement.Trace;
  end;
{$ENDIF}
  finally
    EndExecuteQuery;
  end;
end;

function TBoldUniDACQuery.ParamByName(const Value: string): IBoldParameter;
var
  lUniParam: TUniParam;
begin
  lUniParam := Query.Params.ParamByName(Value);
  Result := TBoldUniDACParameter.Create(lUniParam, Self)
end;

procedure TBoldUniDACQuery.Prepare;
begin
  Query.Prepare;
end;

procedure TBoldUniDACQuery.SetParamCheck(value: Boolean);
begin
  if Query.ParamCheck <> Value then
    Query.ParamCheck := Value;
end;

procedure TBoldUniDACQuery.SetRequestLiveQuery(NewValue: Boolean);
begin
  // ignore
end;

procedure TBoldUniDACQuery.SetUseReadTransactions(value: boolean);
begin
  fUseReadTransactions := value;
end;

{ TBoldUniDACTable }

constructor TBoldUniDACTable.Create(aUniTable: TUniTable; BoldUniDACConnection: TBoldUniDACConnection);
begin
  inherited Create(BoldUniDACConnection);
  fUniTable := aUniTable;
end;

procedure TBoldUniDACTable.CreateTable;
begin
  raise EBold.CreateFmt('MethodNotImplemented', [ClassName, 'CreateTable']); // do not localize
end;

procedure TBoldUniDACTable.DeleteTable;
begin
  raise EBold.CreateFmt('MethodNotImplemented', [ClassName, 'DeleteTable']); // do not localize
end;

function TBoldUniDACTable.FindParam(const Value: string): IBoldParameter;
var
  Param: TUniParam;
begin
  result := nil;
  Param := UniTable.FindParam(Value);
  if Assigned(Param) then
    Result := TBoldUniDACParameter.Create(Param, Self);
end;

procedure TBoldUniDACTable.AddIndex(const Name, Fields: string;
  Options: TIndexOptions; const DescFields: string);
begin
  raise EBold.CreateFmt(sMethodNotImplemented, [ClassName, 'AddIndex']); // do not localize
end;
(*
function TBoldUniDACTable.GetCommaListOfIndexesForColumn(
  const aColumnName: string): string;
var
  lUniMetaData: TUniMetaData;
  lIndexList: TStringList;
  lIndexedColumn: string;
  lIndexName: string;
  lBoldGuard: IBoldGuard;
const
  cTableName = 'Table_Name';
  cIndexName = 'Index_Name';
  cColumnName = 'Column_Name';
begin
// TODO possibly slow comment
// to improve performance move metadata to the Connection and store it there

  lBoldGuard := TBoldGuard.Create(lUniMetaData, lIndexList);
  lUniMetaData := TUniMetaData.Create(nil);
  lIndexList := TStringList.Create;

  Assert(Assigned(UniTable));
  Assert(Assigned(UniTable.Connection));
  lUniMetaData.Connection := UniTable.Connection;
//  lUniMetaData.DatabaseName := UniTable.Connection.Database;
  lUniMetaData.MetaDataKind := 'Indexes';
{  lUniMetaData.TableName := GetTableName;
  lUniMetaData.Open;
  lUniMetaData.First;
  while not lUniMetaData.Eof do
  begin
    lIndexedColumn := lUniMetaData.FieldByName(cColumnName).AsString;
    if aColumnName = lIndexedColumn then
    begin
      lIndexName := lUniMetaData.FieldByName(cIndexName).AsString;
      lIndexList.Add(lIndexName);
    end;
    lUniMetaData.Next;
  end;
  Result := lIndexList.CommaText;
  lUniMetaData.Close;
}
end;

function TBoldUniDACTable.GetPrimaryIndex: string;
var
  lUniMetaData: TUniMetaData;
  lIndexName: string;
const
  cTableName = 'Table_Name';
  cIndexName = 'Index_Name';
  cColumnName = 'Column_Name';
  cPrimaryKey = 'Primary_Key';
//  COLUMN_NAME
begin
// TODO possibly slow comment
// to improve performance move metadata to the Connection and store it there

  lUniMetaData := TUniMetaData.Create(nil);
  try
    Assert(Assigned(UniTable));
    Assert(Assigned(UniTable.Connection));
    lUniMetaData.Connection := UniTable.Connection;
//    lUniMetaData.DatabaseName := UniTable.Connection.Database;
{    lUniMetaData.MetaDataKind := otPrimaryKeys;
    lUniMetaData.Open;
    lUniMetaData.Filter := Format('(%s = ''%s'')', [cTableName, GetTableName]);
    lUniMetaData.Filtered := True;
    if lUniMetaData.RecordCount = 1 then
    begin
      lIndexName := lUniMetaData.FieldByName(cColumnName).AsString;
      Result := lIndexName;
    end
    else
    begin
      Result := '';
    end;
    lUniMetaData.Close;
}
  finally
    lUniMetaData.free;
  end;
end;
*)
function TBoldUniDACTable.GetDataSet: TDataSet;
begin
  Result := fUniTable;
end;

function TBoldUniDACTable.GetDefaultConstraintNameForColumn(
  const aColumnName: string): string;
var
  lUniMetaData: TUniMetaData;
  lDefaultConstraintName: string;
  lBoldGuard: IBoldGuard;
const
  cConstraintName = 'CONSTRAINT_NAME';
begin
  Assert(Assigned(UniTable));
  Assert(Assigned(UniTable.Connection));

  lBoldGuard := TBoldGuard.Create(lUniMetaData);
  lUniMetaData := TUniMetaData.Create(nil);
  lUniMetaData.Connection := UniTable.Connection;
{  lUniMetaData.DatabaseName := UniTable.Connection.Database;
  lUniMetaData.TableName := GetTableName;
  lUniMetaData.ColumnName := aColumnName;
  lUniMetaData.ObjectType := otConstraintColumnUsage;
  lUniMetaData.Open;
  lUniMetaData.First;
  if not lUniMetaData.Eof then
  begin
    lDefaultConstraintName := lUniMetaData.FieldByName(cConstraintName).AsString;
  end;
  lUniMetaData.Close;
}
  Result := lDefaultConstraintName;
end;

function TBoldUniDACTable.GetExclusive: Boolean;
begin
  Result := False;
end;

function TBoldUniDACTable.GetExists: Boolean;
var
  lAllTables: TStringList;
  lGuard: IBoldGuard;
begin
  lGuard := TBoldGuard.Create(lAllTables);
  Result := False;

  // First we make sure we have a table component and that it is connected to a database
  if Assigned(UniTable) and Assigned(UniTable.Connection) then
  begin
    // We now create a list that will hold all the table names in the database
    lAllTables := TStringList.Create;
    UniTable.Connection.GetTableNames(lAllTables);
    Result := lAllTables.IndexOf(GetTableName) <> -1;
  end;
end;

function TBoldUniDACTable.GetIndexDefs: TIndexDefs;
begin
  raise EBold.CreateFmt(sMethodNotImplemented, [ClassName, 'GetIndexDefs']); // do not localize
//  Result := UniTable.IndexFieldNames
end;

function TBoldUniDACTable.GetUniTable: TUniTable;
begin
  Result := fUniTable;
end;

function TBoldUniDACTable.ParamByName(const Value: string): IBoldParameter;
var
  lUniParam: TUniParam;
begin
  lUniParam := UniTable.Params.ParamByName(Value);
  Result := TBoldUniDACParameter.Create(lUniParam, Self)
end;

function TBoldUniDACTable.GetTableName: string;
begin
  Result := UniTable.TableName;
end;

procedure TBoldUniDACTable.SetExclusive(NewValue: Boolean);
begin
end;

procedure TBoldUniDACTable.SetTableName(const NewName: string);
begin
  UniTable.TableName := NewName;
end;

{ TBoldUniDACConnection }

// Populate the "TableNameList" with tablenames from the database that maches "pattern"

procedure TBoldUniDACConnection.AllTableNames(Pattern: string; ShowSystemTables: Boolean; TableNameList: TStrings);
var
  lTempList: TStringList;
  lIndexTempList: Integer;
  lGuard: IBoldGuard;
  i: integer;
begin
  lGuard := TBoldGuard.Create(lTempList);
  lTempList := TStringList.Create;

  // Retrieve the list of table names
  // Note: This does not include views or procedures, there is a specific
  //       method in TUniConnection for that
  UniConnection.GetTableNames(lTempList, ShowSystemTables);

  // convert from fully qualified names in format: database.catalogue.table to just table name
  for i := 0 to lTempList.Count - 1 do
  begin
    while pos('.', lTempList[i]) > 0 do
    begin
      lTempList[i] := Copy(lTempList[i], pos('.', lTempList[i])+1, maxInt);
    end;
    // In case of Google BigQuery tablenames were surrounded with quotation marks.
    // To return only tablenames, these quotation marks must be removed    
    lTempList[i] := StringReplace(lTempList[i], '"', '', [rfReplaceAll]);
  end;

  if Pattern = '' then
    TableNameList.Assign(lTempList)
  else
  // MatchesMask is used to compare filenames with wildcards, suits us here
  // but there should be some care taken, when using tablenames with period
  // signes, as that might be interpreted as filename extensions
  for lIndexTempList := 0 to lTempList.Count - 1 do
  begin
    if MatchesMask(lTempList[lIndexTempList], Pattern) then
    begin
      TableNameList.Add(lTempList[lIndexTempList]);
    end;
  end;
end;

procedure TBoldUniDACConnection.Commit;
begin
  UniConnection.Commit;
end;

function TBoldUniDACConnection.GetImplementor: TObject;
begin
  result := UniConnection;
end;

function TBoldUniDACConnection.GetInTransaction: Boolean;
begin
  Result := UniConnection.InTransaction;
end;

function TBoldUniDACConnection.GetIsExecutingQuery: Boolean;
begin
  Result := fExecuteQueryCount > 0;
end;

function TBoldUniDACConnection.GetIsSQLBased: Boolean;
begin
  Result := True;
end;

function TBoldUniDACConnection.GetKeepConnection: Boolean;
begin
  //CheckMe;
  Result := True;
end;

function TBoldUniDACConnection.GetLogInPrompt: Boolean;
begin
  Result := UniConnection.LoginPrompt;
end;

procedure TBoldUniDACConnection.RollBack;
begin
  UniConnection.RollBack;
end;

procedure TBoldUniDACConnection.SetKeepConnection(NewValue: Boolean);
begin
  //CheckMe;
end;

procedure TBoldUniDACConnection.SetlogInPrompt(NewValue: Boolean);
begin
  UniConnection.LoginPrompt := NewValue;
end;

procedure TBoldUniDACConnection.StartReadTransaction;
begin
  UniConnection.DefaultTransaction.IsolationLevel := ilReadCommitted;
  UniConnection.StartTransaction;
end;

procedure TBoldUniDACConnection.StartTransaction;
begin
  UniConnection.DefaultTransaction.IsolationLevel := ilRepeatableRead;
  UniConnection.StartTransaction;
end;

function TBoldUniDACConnection.DatabaseExists: boolean;
var
  vQuery: IBoldQuery;
  vDatabaseName: string;
begin
  vDatabaseName := LowerCase(UniConnection.Database);
  UniConnection.Connected := false;
  UniConnection.Database := ''; // need to clear this to connect succesfully
  vQuery := GetQuery;
  try
    (vQuery.Implementor as TBoldUniDACQuery).Query.Connection := UniConnection;
    vQuery.SQLText := SQLDataBaseConfig.GetDatabaseExistsQuery(vDatabaseName);
    vQuery.UseReadTransactions := false;
    vQuery.Open;
    result := vQuery.RecordCount > 0;
    UniConnection.Disconnect;
  finally
    vQuery.UseReadTransactions := true;
    ReleaseQuery(vQuery);
    UniConnection.Database := vDatabaseName;
  end;
end;

destructor TBoldUniDACConnection.Destroy;
begin
  ReleaseCachedObjects;
  inherited;
end;

procedure TBoldUniDACConnection.DropDatabase;
var
//  vQuery: IBoldExecQuery;
  vDatabaseName: string;
  vUniScript: TUniScript;
//  vIsInterbase: boolean;
  vIsMSSQL: boolean;
  vIsPostgres: boolean;
const
  cInterbase = 'InterBase';
  cMSSQL = 'SQL Server';
  cPostgres = 'PostgreSQL';
  cDropDatabaseSQL = 'Drop Database %s';
  cGenerateDatabaseSQL = 'Create Database %s';
  cGenerateDatabaseInterbaseSQL = 'Create Database ''%s'' user ''%s'' password ''%s''';
  cGenerateDatabaseSQLServer = 'USE master;' + BOLDCRLF + 'GO' + BOLDCRLF + ' Create Database %s';
begin
  vDatabaseName := LowerCase(UniConnection.Database);
  vIsMSSQL := false;
//  UniConnection.Database := ''; // need to clear this to connect succesfully
  vUniScript := TUniScript.Create(nil);
  try
    vUniScript.Connection := UniConnection;
//    vIsInterbase := UniConnection.ProviderName = cInterBase;
    vIsMSSQL := UniConnection.ProviderName = cMSSQL;
    vIsPostgres := UniConnection.ProviderName = cPostgres;
    vUniScript.SQL.Text := Format(cDropDatabaseSQL, [vDatabaseName]);
    if vIsMSSQL then
      UniConnection.Database := 'master'
    else
    if vIsPostgres then
      UniConnection.Database := '';
    try
      vUniScript.Execute;
    except
      // ignore
    end;
    UniConnection.Close;
  finally
    vUniScript.free;
    if vIsMSSQL then
      UniConnection.Database := vDatabaseName;
  end;
end;

procedure TBoldUniDACConnection.EndExecuteQuery;
begin
  dec(fExecuteQueryCount);
end;

constructor TBoldUniDACConnection.Create(aUniConnection: TUniConnection; SQLDataBaseConfig: TBoldSQLDatabaseConfig);
begin
  inherited Create(SQLDataBaseConfig);
  fUniConnection := aUniConnection;
end;

function TBoldUniDACConnection.CreateAnotherDatabaseConnection: IBoldDatabase;
begin
  var Connection := TUniConnection.Create(nil); // owner ?
  Connection.Assign(self.fUniConnection);
  result := TBoldUniDACConnection.Create(Connection, SQLDatabaseConfig);
end;

procedure TBoldUniDACConnection.BeginExecuteQuery;
begin
  inc(fExecuteQueryCount);
end;

procedure TBoldUniDACConnection.Close;
begin
  UniConnection.Close;
end;

procedure TBoldUniDACConnection.CreateDatabase(DropExisting: boolean);
var
  vDatabaseName: string;
  vUniScript: TUniScript;
begin
  vDatabaseName := LowerCase(UniConnection.Database);
  if DropExisting and DatabaseExists then
    DropDatabase;
  UniConnection.Database := ''; // need to clear this to connect succesfully
  vUniScript := TUniScript.Create(nil);
  try
    vUniScript.Connection := UniConnection;
    vUniScript.SQL.Text := SQLDataBaseConfig.GetCreateDatabaseQuery(vDatabaseName);
    vUniScript.Execute;
    UniConnection.Close;
  finally
    UniConnection.Close;
    vUniScript.free;
    UniConnection.Database := vDatabaseName;
  end;
end;

function TBoldUniDACConnection.GetConnected: Boolean;
begin
  Result := UniConnection.Connected;
end;

function TBoldUniDACConnection.GetDatabaseError(const E: Exception;
  const sSQL: string): EBoldDatabaseError;
const
  SQLERRORCODE = 'SQL Error Code: ';
var
//  iErrorCode: Integer;
//  sMsg: string;
//  iPos: Integer;
  aErrorType: TBoldDatabaseErrorType;
  sServer,
  sDatabase,
  sUsername: string;
  bUseWindowsAuth: Boolean;
const
  // Provider names copied here to avoid dependancy
  cMSSQLProvider = 'SQL Server'; // TSQLServerUniProvider.GetProviderName
  cPostgreSQLProvider = 'PostgreSQL'; // TPostgreSQLUniProvider.GetProviderName
  cOracleSQLProvider = 'Oracle'; // TOracleUniProvider.GetProviderName
  cInterBaseProvider = 'InterBase';
  cMSSQLDeadLock = 1205;
begin
  aErrorType := bdetError;
  sServer := UniConnection.Server;
  sDatabase := UniConnection.Database;
  sUsername := UniConnection.Username;
  bUseWindowsAuth := Pos('Authentication=Windows', UniConnection.ConnectString) > 0;
  if (E is EUniError) then
  begin
    if UniConnection.ProviderName = cMSSQLProvider then
      case EUniError(E).ErrorCode of
        -2147467259, 2, 233: aErrorType := bdetConnection; // only set bdetConnection for cases where retry might work.
        208, 4145: aErrorType := bdetSQL;
        4060: aErrorType := bdetLogin; // SQLServer Error: 4060, Cannot open database "SessionStateService" requested by the login. The login failed. [SQLSTATE 42000]
        18456: aErrorType := bdetLogin; // SQLServer Error: 18456, Login failed for user 'domain\user'. [SQLSTATE 28000]
        cMSSQLDeadLock: aErrorType := bdetDeadlock;
        //Deadlock und weitere ErrorCodes?
      end
    else
    if UniConnection.ProviderName = cInterBaseProvider then
      case EUniError(E).ErrorCode of
        -803: aErrorType := bdetUpdate; // attempt to store duplicate value (visible to active transactions) in unique index
      end
    else
    if UniConnection.ProviderName = cPostgreSQLProvider then
      case EUniError(E).ErrorCode of
        0: aErrorType := bdetLogin;
      end
    else
      raise Exception.Create('Error codes not implemented for ' + UniConnection.ProviderName);
  end;
  Result := InternalGetDatabaseError(aErrorType, E, sSQL, sServer, sDatabase,
      sUsername, bUseWindowsAuth);
end;

function TBoldUniDACConnection.GetExecQuery: IBoldExecQuery;
begin
  if Assigned(fCachedExecQuery1) then
  begin
    result := fCachedExecQuery1;
    fCachedExecQuery1 := nil;
  end else
  begin
    Result := TBoldUniDACExecQueryClass.Create(Self);
  end;
end;

function TBoldUniDACConnection.GetUniConnection: TUniConnection;
begin
  Result := fUniConnection;
end;

function TBoldUniDACConnection.GetQuery: IBoldQuery;
begin
  if Assigned(fCachedQuery1) then
  begin
    result := fCachedQuery1;
    fCachedQuery1 := nil;
  end else
  if Assigned(fCachedQuery2) then
  begin
    result := fCachedQuery2;
    fCachedQuery2 := nil;
  end else
  begin
    Result := BoldUniDACQueryClass.Create(Self);
  end;
end;

function TBoldUniDACConnection.GetTable: IBoldTable;
var
  lUniTable: TUniTable;
begin
  if Assigned(fCachedTable) then
  begin
    result := fCachedTable;
    fCachedTable := nil;
  end
  else
  begin
    lUniTable := TUniTable.Create(nil);
    lUniTable.Connection := UniConnection;
    Result := TBoldUniDACTable.Create(lUniTable, Self);
  end;
end;

procedure TBoldUniDACConnection.Open;
begin
  try
    UniConnection.Open;
  except
    on E: Exception do begin
      raise GetDatabaseError(E);
    end;
  end;
end;

procedure TBoldUniDACConnection.Reconnect;
begin
  if Assigned(fUniConnection) then begin
    fUniConnection.Connected := False;
    fUniConnection.Connected := True;
  end;
end;

type TCollectionAccess = class(TCollection);

procedure TBoldUniDACConnection.ReleaseQuery(var Query: IBoldQuery);
var
  lBoldUniDACQuery: TBoldUniDACQuery;
begin
  Assert(Query.Implementor is TBoldUniDACQuery);
  lBoldUniDACQuery := Query.Implementor as TBoldUniDACQuery;
  lBoldUniDACQuery.clear;
  while lBoldUniDACQuery.SQLStrings.Updating do
    lBoldUniDACQuery.SQLStrings.EndUpdate;
  while TCollectionAccess(lBoldUniDACQuery.Params).UpdateCount > 0 do
    lBoldUniDACQuery.Params.EndUpdate;
  Query := nil;
  if not Assigned(fCachedQuery1) and not (csDestroying in UniConnection.ComponentState) then
    fCachedQuery1 := lBoldUniDACQuery
  else
  if not Assigned(fCachedQuery2) and not (csDestroying in UniConnection.ComponentState) then
    fCachedQuery2 := lBoldUniDACQuery
  else
    lBoldUniDACQuery.free;
end;

procedure TBoldUniDACConnection.ReleaseExecQuery(var Query: IBoldExecQuery);
var
  lBoldUniDACExecQuery: TBoldUniDACExecQuery;
begin
  Assert(Query.Implementor is TBoldUniDACExecQuery);
  lBoldUniDACExecQuery := Query.Implementor as TBoldUniDACExecQuery;
  if lBoldUniDACExecQuery.SQLStrings.Count <> 0 then
  begin
    lBoldUniDACExecQuery.SQLStrings.BeginUpdate;
    lBoldUniDACExecQuery.clear;
  end;
  while lBoldUniDACExecQuery.SQLStrings.Updating do
    lBoldUniDACExecQuery.SQLStrings.EndUpdate;
  while TCollectionAccess(lBoldUniDACExecQuery.Params).UpdateCount > 0 do
    lBoldUniDACExecQuery.Params.EndUpdate;
  Query := nil;
  if not Assigned(fCachedExecQuery1) and not (csDestroying in UniConnection.ComponentState) then
    fCachedExecQuery1 := lBoldUniDACExecQuery
  else
    lBoldUniDACExecQuery.free;
end;

procedure TBoldUniDACConnection.ReleaseTable(var Table: IBoldTable);
var
  lBoldUniDACTable: TBoldUniDACTable;
begin
  if Table.Implementor is TBoldUniDACTable then
  begin
    lBoldUniDACTable := Table.Implementor as TBoldUniDACTable;
    Table := nil;
    if not Assigned(fCachedTable) and not (csDestroying in UniConnection.ComponentState) then
      fCachedTable := lBoldUniDACTable
    else
      lBoldUniDACTable.free;
  end;
end;

function TBoldUniDACConnection.SupportsTableCreation: Boolean;
begin
  Result := False;
end;

{ TBoldUniDACParameter }

procedure TBoldUniDACParameter.Clear;
begin
  UniParam.Clear;
end;

constructor TBoldUniDACParameter.Create(UniDACParameter: TUniParam; DatasetWrapper: TBoldAbstractQueryWrapper);
begin
  inherited Create(DatasetWrapper);
  fUniParam := UniDACParameter;
end;

function TBoldUniDACParameter.GetAsAnsiString: TBoldAnsiString;
begin
  Result := UniParam.AsAnsiString;
end;

function TBoldUniDACParameter.GetAsBCD: Currency;
begin
  Result := UniParam.AsBCD;
end;

function TBoldUniDACParameter.GetAsblob: TBoldBlobData;
begin
  Result := AnsiString(UniParam.Value);
end;

function TBoldUniDACParameter.GetAsBoolean: Boolean;
begin
  Result := UniParam.AsBoolean;
end;

function TBoldUniDACParameter.GetAsCurrency: Currency;
begin
  Result := UniParam.AsCurrency;
end;

function TBoldUniDACParameter.GetAsDateTime: TDateTime;
begin
  Result := UniParam.AsDateTime;
end;

function TBoldUniDACParameter.GetAsFloat: Double;
begin
  Result := UniParam.AsFloat;
end;

function TBoldUniDACParameter.GetAsInt64: Int64;
begin
  result := UniParam.AsLargeInt;
end;

function TBoldUniDACParameter.GetAsInteger: Longint;
begin
  Result := UniParam.AsInteger;
end;

function TBoldUniDACParameter.GetAsMemo: string;
begin
  Result := UniParam.AsMemo;
end;

function TBoldUniDACParameter.GetAsString: string;
begin
  Result := UniParam.AsString;
  if Result = DatasetWrapper.DatabaseWrapper.SQLDataBaseConfig.EmptyStringMarker then
  begin
    Result := '';
  end;
end;

function TBoldUniDACParameter.GetAsVariant: Variant;
begin
  Result := UniParam.Value;
end;

function TBoldUniDACParameter.GetAsWideString: WideString;
begin
  Result := UniParam.AsWideString;
  if Result = DatasetWrapper.DatabaseWrapper.SQLDataBaseConfig.EmptyStringMarker then
  begin
    Result := '';
  end;
end;

function TBoldUniDACParameter.GetDataType: TFieldType;
begin
  Result := UniParam.DataType;
end;

function TBoldUniDACParameter.GetIsNull: Boolean;
begin
  Result := VarIsNull(UniParam.Value)
end;

function TBoldUniDACParameter.GetName: string;
begin
  Result := UniParam.Name;
end;

function TBoldUniDACParameter.GetUniParam: TUniParam;
begin
  Result := fUniParam;
end;

procedure TBoldUniDACParameter.SetAsAnsiString(const Value: TBoldAnsiString);
begin
  UniParam.AsAnsiString := Value;
end;

procedure TBoldUniDACParameter.SetAsBCD(const Value: Currency);
begin
  UniParam.Value := Value;
end;

procedure TBoldUniDACParameter.SetAsBlob(const Value: TBoldBlobData);
begin
  if UniParam.DataType = ftUnknown then
  begin
    UniParam.DataType := ftBlob;
  end;
  if Value = '' then
  begin
//    UniParam.Value := DatasetWrapper.DatabaseWrapper.SQLDataBaseConfig.EmptyStringMarker;
    UniParam.Value := Null;
  end else
  begin
    UniParam.Value := TBoldBlobData(AnsiString(Value));
  end;
end;

procedure TBoldUniDACParameter.SetAsBoolean(Value: Boolean);
begin
  UniParam.AsBoolean := Value;
end;

procedure TBoldUniDACParameter.SetAsCurrency(const Value: Currency);
begin
  UniParam.AsCurrency := Value;
end;

procedure TBoldUniDACParameter.SetAsDate(const Value: TDateTime);
begin
  UniParam.AsDate := Value;
end;

procedure TBoldUniDACParameter.SetAsDateTime(const Value: TDateTime);
begin
  UniParam.AsDateTime := Value;
end;

procedure TBoldUniDACParameter.SetAsFloat(const Value: Double);
begin
  UniParam.AsFloat := Value;
end;

procedure TBoldUniDACParameter.SetAsInt64(const Value: Int64);
begin
  UniParam.AsLargeInt := Value;
end;

procedure TBoldUniDACParameter.SetAsInteger(Value: Integer);
begin
  UniParam.AsInteger := Value;
end;

procedure TBoldUniDACParameter.SetAsMemo(const Value: string);
begin
  UniParam.AsMemo := Value;
end;

procedure TBoldUniDACParameter.SetAsSmallInt(Value: Integer);
begin
  UniParam.AsSmallInt := Value;
end;

procedure TBoldUniDACParameter.SetAsString(const Value: string);
begin
  if Value = '' then
  begin
    UniParam.AsString := DatasetWrapper.DatabaseWrapper.SQLDataBaseConfig.EmptyStringMarker;
  end else
  begin
    UniParam.AsString := Value;
  end;
end;

procedure TBoldUniDACParameter.SetAsTime(const Value: TDateTime);
begin
  UniParam.AsTime := Value;
end;

procedure TBoldUniDACParameter.SetAsVariant(const NewValue: Variant);
begin
  UniParam.Value := NewValue;
end;

procedure TBoldUniDACParameter.SetAsWideString(const Value: Widestring);
begin
  if Value = '' then
  begin
    UniParam.AsString := DatasetWrapper.DatabaseWrapper.SQLDataBaseConfig.EmptyStringMarker;
  end else
  begin
    UniParam.AsWideString := Value;
  end;
end;

procedure TBoldUniDACParameter.SetAsWord(Value: Integer);
begin
  UniParam.AsWord := Value;
end;

procedure TBoldUniDACParameter.SetDataType(Value: TFieldType);
begin
  UniParam.DataType := Value;
end;

procedure TBoldUniDACParameter.SetText(const Value: string);
begin
  UniParam.Value := Value;
end;

procedure TBoldUniDACParameter.Assign(const source: IBoldParameter);
begin
  UniParam.Value := Source.AsVariant;
end;

procedure TBoldUniDACParameter.AssignFieldValue(const source: IBoldField);
begin
  UniParam.Assign(source.Field);
end;

procedure TBoldUniDACConnection.ReleaseCachedObjects;
begin
  if Assigned(fCachedTable) then
    ReleaseTable(fCachedTable);

  if Assigned(fCachedQuery1) then
    ReleaseQuery(fCachedQuery1);

  if Assigned(fCachedQuery2) then
    ReleaseQuery(fCachedQuery2);

  if Assigned(fCachedExecQuery1) then
    ReleaseExecQuery(fCachedExecQuery1);
end;

{ TBoldUniDACExecQuery }

procedure TBoldUniDACExecQuery.AssignParams(Sourceparams: TParams);
var
  lIndexSourceParams: Integer;
  lUniParam: TUniParam;
begin
  ExecQuery.Params.Clear;
  if Assigned(Sourceparams) and (Sourceparams.Count > 0) then
  begin
    for lIndexSourceParams := 0 to Sourceparams.Count - 1 do
    begin
      lUniParam := ExecQuery.Params.CreateParam(Sourceparams[lIndexSourceParams].DataType, Sourceparams[lIndexSourceParams].Name, Sourceparams[lIndexSourceParams].ParamType) as TUniParam;
      lUniParam.Value := Sourceparams[lIndexSourceParams].Value;
    end;
  end;
end;

procedure TBoldUniDACExecQuery.AssignSQL(SQL: TStrings);
begin
  ExecQuery.SQL.BeginUpdate;
  ExecQuery.SQL.Assign(SQL);
  ExecQuery.SQL.EndUpdate;
  ExecQuery.Params.ParseSQL(SQL.Text, False);
end;

procedure TBoldUniDACExecQuery.AssignSQLText(const SQL: string);
var
  lStringList: TStringList;
  lGuard: IBoldGuard;
begin
  lGuard := TBoldGuard.Create(lStringList);
  lStringList := TStringList.Create;
  lStringList.Add(SQL);
  AssignSQL(lStringList);
end;

procedure TBoldUniDACExecQuery.BeginExecuteQuery;
begin
  (DatabaseWrapper as TBoldUniDACConnection).BeginExecuteQuery;
end;

procedure TBoldUniDACExecQuery.Clear;
begin
  inherited;
  AssignSQLText('');
  ClearParams;
end;

procedure TBoldUniDACExecQuery.ClearParams;
begin
  ExecQuery.Params.Clear;
end;

var
  _x: integer;

constructor TBoldUniDACExecQuery.Create(BoldUniDACConnection: TBoldUniDACConnection);
begin
  inherited Create(BoldUniDACConnection);
  fUseReadTransactions := true;
  inc(_x);
end;

function TBoldUniDACExecQuery.Createparam(FldType: TFieldType;
  const ParamName: string): IBoldParameter;
begin
  result := CreateParam(FldType, ParamName, ptUnknown, 0);
end;

function TBoldUniDACExecQuery.CreateParam(FldType: TFieldType; const ParamName: string; ParamType: TParamType; Size: integer): IBoldParameter;
var
  lUniParam: TUniParam;
begin
  lUniParam := ExecQuery.Params.CreateParam(FldType, ParamName, ptUnknown) as TUniParam;
//  lUniParam.Size := Size;
  lUniParam.Value := NULL;
  Result := TBoldUniDACParameter.Create(lUniParam, Self);
end;

destructor TBoldUniDACExecQuery.Destroy;
begin
  FreeAndNil(fExecQuery);
  dec(_x);
  inherited;
end;

procedure TBoldUniDACExecQuery.EndExecuteQuery;
begin
  (DatabaseWrapper as TBoldUniDACConnection).EndExecuteQuery;
end;

function TBoldUniDACExecQuery.EnsureParamByName(
  const Value: string): IBoldParameter;
var
  lUniParam: TUniParam;
begin
  lUniParam := ExecQuery.Params.FindParam(Value);
  if not Assigned(lUniParam) then
    lUniParam := ExecQuery.Params.CreateParam(ftUnknown, Value, ptUnknown) as TUniParam;
  Result := TBoldUniDACParameter.Create(lUniParam, Self)
end;

procedure TBoldUniDACExecQuery.ExecSQL;
var
  Retries: Integer;
  Done: Boolean;
{$IFDEF ATTRACS}
  PerformanceMeasurement : TPerformanceMeasurement;
begin
  //PATCH for logging long running SQL
  PerformanceMeasurement := TPerformanceMeasurement.ReStart;
{$ELSE}
begin
{$ENDIF}
  BeginExecuteQuery;
  try
  BoldLogSQLWithParams(ExecQuery.SQL, self);
  Retries := 0;
  Done := false;
  while not Done do
  begin
    try
      if (DatabaseWrapper as TBoldUniDACConnection).GetInTransaction then
        fReadTransactionStarted := false
      else
      begin
        if fUseReadTransactions then
        (DatabaseWrapper as TBoldUniDACConnection).StartReadTransaction;
        fReadTransactionStarted := fUseReadTransactions;
      end;
      ExecQuery.Execute;
      if fReadTransactionStarted and  (DatabaseWrapper as TBoldUniDACConnection).GetInTransaction then
      begin
       (DatabaseWrapper as TBoldUniDACConnection).Commit;
       fReadTransactionStarted := false;
      end;
      Done := true;
    except
      on e: Exception do
      begin
        if (not fReadTransactionStarted) or (Retries > 4) then
          raise TBoldUniDACConnection(DatabaseWrapper).GetDatabaseError(E, ExecQuery.SQL.Text);
        if (DatabaseWrapper as TBoldUniDACConnection).GetInTransaction then
          (DatabaseWrapper as TBoldUniDACConnection).Rollback;
        fReadTransactionStarted := false;
        INC(Retries);
        sleep(Retries*200);
      end;
    end;
  end;
{$IFDEF ATTRACS}
  {$IFDEF BOLD_PERFORMANCE_COUNTERS}
  PerformanceMeasurement.EndMeasurement;
  BoldSystemPerfObject.BoldDBXQuery_ExecSQL(PerformanceMeasurement.TimeTaken);
  {$ENDIF}
  if not PerformanceMeasurement.AcceptableTimeForUserResponseTime then
  begin
    PerformanceMeasurement.WhatMeasured := 'TBoldUniDACExecQuery.ExecSQL';
    PerformanceMeasurement.WhatMeasuredParameter := ExecQuery.SQL.Text;
    if TraceLogAssigned then
      PerformanceMeasurement.Trace;
  end;
{$ENDIF}
  finally
    EndExecuteQuery;
  end;
end;

function TBoldUniDACExecQuery.FindParam(const Value: string): IBoldParameter;
var
  Param: TParam;
begin
  Param := ExecQuery.FindParam(Value);
  if not Assigned(Param) then
    result := CreateParam(ftUnknown, Value);
end;

function TBoldUniDACExecQuery.GetBatchQueryParamCount: integer;
begin
  result := 0; // update when batch support is implemented
end;

function TBoldUniDACExecQuery.GetDataSet: TDataSet;
begin
  raise EBold.CreateFmt('MethodNotImplemented', [ClassName, 'GetDataSet']); // do not localize
end;

function TBoldUniDACExecQuery.GetExecQuery: TUniSQL;
begin
  if not Assigned(fExecQuery) then
  begin
    fExecQuery := TUniSQL.Create(nil);
    fExecQuery.Connection := (DatabaseWrapper as TBoldUniDACConnection).UniConnection;
  end;
  Result := fExecQuery;
end;

function TBoldUniDACExecQuery.GetParamCheck: Boolean;
begin
  result := ExecQuery.ParamCheck;
end;

function TBoldUniDACExecQuery.GetParamCount: Integer;
begin
  result := ExecQuery.Params.Count;
end;

function TBoldUniDACExecQuery.GetParams: TParams;
begin
  result := ExecQuery.Params;
end;

function TBoldUniDACExecQuery.GetParam(i: Integer): IBoldParameter;
begin
  Result := TBoldUniDACParameter.Create(ExecQuery.Params[i], Self);
end;

function TBoldUniDACExecQuery.GetRowsAffected: Integer;
begin
  Result := ExecQuery.RowsAffected;
end;

function TBoldUniDACExecQuery.GetSQLStrings: TStrings;
begin
  result := ExecQuery.SQL;
end;

function TBoldUniDACExecQuery.GetSqlText: string;
begin
  Result := ExecQuery.SQL.Text;
end;

function TBoldUniDACExecQuery.GetUseReadTransactions: boolean;
begin
  result := fUseReadTransactions;
end;

function TBoldUniDACExecQuery.ParamByName(const Value: string): IBoldParameter;
var
  lUniParam: TUniParam;
begin
  lUniParam := ExecQuery.Params.ParamByName(Value);
  if Assigned(lUniParam) then
  begin
    Result := TBoldUniDACParameter.Create(lUniParam, Self)
  end else
  begin
    Result := nil;
  end;
end;

procedure TBoldUniDACExecQuery.Prepare;
begin
  ExecQuery.Prepare;
end;

procedure TBoldUniDACExecQuery.SetParamCheck(value: Boolean);
begin
  if ExecQuery.ParamCheck <> Value then
    ExecQuery.ParamCheck := Value;
end;

procedure TBoldUniDACExecQuery.SetUseReadTransactions(value: boolean);
begin
  fUseReadTransactions := value;
end;

end.
