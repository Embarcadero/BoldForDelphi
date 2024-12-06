{ Global compiler directives }
{$include bold.inc}
unit BoldSQLDirectInterfaces;

interface
uses
  Windows,
  Classes,
  Db,
  SDEngine,
  SysUtils,
  BoldSQLDatabaseConfig,
  BoldDefs,
  BoldDBInterfaces;

type
  { forward declarations }
  TBoldSQLDirectDatabase = class;
  TBoldSQLDirectQuery = class;
  TBoldSQLDirectTable = class;

  TBoldSQLDirectQueryClass = class of TBoldSQLDirectQuery;

  TBoldSqlDirectDbParameter = class(TBoldDbParameter, IBoldDBParam)
  private
    function GetParameter: TParam;
  end;

  { TBoldSDQuery }
  TBoldSQLDirectQuery = class(TBoldBatchDataSetWrapper, IBoldQuery, IBoldExecQuery, IBoldParameterized)
  private
    FQuery: TSDQuery;
    fUseReadTransactions: boolean;
    function GetParamCount: integer;
    function GetParam(i: integer): IBoldParameter;
    function GetQuery: TSDQuery;
    function GetRecordCount: integer;
    function GetRequestLiveQuery: Boolean;
    procedure ClearParams;
    function GetRowsAffected: integer;
    function GetParamCheck: Boolean;
    procedure SetParamCheck(value: Boolean);
    procedure AssignParams(Sourceparams: TParams);
    procedure AssignSQL(SQL: TStrings);
    procedure SetRequestLiveQuery(NewValue: Boolean);
    function GetUseReadTransactions: boolean;
    procedure SetUseReadTransactions(value: boolean);
  protected
    function Createparam(FldType: TFieldType; const ParamName: string; ParamType: TParamType; Size: integer): IBoldParameter; override;
    function GetSqlText: String; override;
    procedure ExecSQL; override;
    procedure AssignSQLText(const SQL: String); override;
    function GetSQLStrings: TStrings; override;
    function ParamByName(const Value: string): IBoldParameter; override;
    function FindParam(const Value: string): IBoldParameter; override;
    function GetParams: TParams; override;
    function GetDataSet: TDataSet; override;
    procedure Open; override;
    property Query: TSDQuery read GetQuery;
  public
    constructor Create(Query: TSDQuery; DatabaseWrapper: TBoldDatabaseWrapper); reintroduce;
  end;

  { TBoldSQLDirectTable }
  TBoldSQLDirectTable = class(TBoldDataSetWrapper, IBoldTable)
  private
    fTable: TSDTable;
    procedure AddIndex(const Name, Fields: string; Options: TIndexOptions; const DescFields: string = '');
    procedure CreateTable;
    procedure DeleteTable;
    function GetTable: TSDTable;
    function GetIndexDefs: TIndexDefs;
    procedure SetTableName(const NewName: String);
    function GetTableName: String;
    procedure SetExclusive(NewValue: Boolean);
    function GetExclusive: Boolean;
    function GetExists: Boolean;
    property Table: TSDTable read GetTable;
  protected
    function GetDataSet: TDataSet; override;
    function ParamByName(const Value: string): IBoldParameter; override;
    function FindParam(const Value: string): IBoldParameter; override;
  public
    constructor Create(Table: TSDTable; DatabaseWrapper: TBoldDatabaseWrapper); reintroduce;
  end;

  { TBoldSDDataBase }
  TBoldSQLDirectDatabase = class(TBolddatabaseWrapper, IBoldDataBase)
  private
    FDatabase: TSDDataBase;
    fCachedTable: TSDTable;
    FCachedQuery: TSDQuery;
    fExecuteQueryCount: integer;
    function GetConnected: Boolean;
    function GetDataBase: TSDDataBase;
    function GetInTransaction: Boolean;
    function GetIsSQLBased: Boolean;
    function GetKeepConnection: Boolean;
    function GetLogInPrompt: Boolean;
    function SupportsTableCreation: boolean;
    procedure Close;
    procedure Commit;
    procedure Open;
    procedure Rollback;
    procedure SetKeepConnection(NewValue: Boolean);
    procedure SetlogInPrompt(NewValue: Boolean);
    procedure StartTransaction;
    property Database: TSDDatabase read GetDatabase;
    procedure ReleaseCachedObjects;
    function GetIsExecutingQuery: Boolean;
  protected
    procedure AllTableNames(Pattern: String; ShowSystemTables: Boolean; TableNameList: TStrings); override;
    function GetQuery: IBoldQuery; override;
    procedure ReleaseQuery(var Query: IBoldQuery); override;
    function GetTable: IBoldTable; override;
    procedure ReleaseTable(var Table: IBoldTable); override;
  public
    constructor Create(Database: TSDDataBase; SQLDataBaseConfig: TBoldSQLDatabaseConfig);
    destructor Destroy; override;
    procedure Reconnect;
    function GetDatabaseError(const E: Exception; const sSQL: string = ''):
        EBoldDatabaseError;
  end;

var
  BoldSQLDirectQueryClass: TBoldSQLDirectQueryClass = TBoldSQLDirectQuery;

implementation

uses
  BoldUtils,
  Controls,
  Masks,
  DateUtils,
  StrUtils,
  SDCommon;

function BuildLogStr(Query: TSDQuery): String;
var
  i: Integer;
begin
  Result:=DateTimeToStr(Now)+#13#10;
  Result:=Result+Query.SQL.Text+#13#10;
  for i:=0 to Query.Params.Count-1 do begin
    if Query.Params[i].DataType in [ftString, ftSmallint, ftInteger,
        ftWord, ftDate, ftTime, ftDateTime, ftMemo, ftFixedChar, ftWideString,
        ftLargeint, ftTimeStamp, ftFloat, ftCurrency, ftBoolean, ftFmtMemo] then begin
      Result:=Result+Query.Params[i].Name+'='+Query.Params[i].AsString+#13#10;
    end;
  end;
  Result:=Result+#13#10;
end;

{ TBoldSDQuery }

procedure TBoldSQLDirectQuery.AssignParams(Sourceparams: tparams);
begin
  if assigned(Sourceparams) then
    Query.Params.Assign(SourceParams)
  else
    Query.Params.Clear;
end;

procedure TBoldSQLDirectQuery.AssignSQL(SQL: TStrings);
begin
  Query.SQL.BeginUpdate;
  Query.SQL.Assign(SQL);
  Query.SQL.EndUpdate;
end;

procedure TBoldSQLDirectQuery.AssignSQLText(const SQL: String);
begin
  Query.SQL.BeginUpdate;
  Query.SQL.Clear;
  BoldAppendToStrings(Query.SQL, SQL, true);
  Query.SQL.EndUpdate;
end;

procedure TBoldSQLDirectQuery.ClearParams;
begin
  Query.Params.Clear;
end;

constructor TBoldSQLDirectQuery.Create(Query: TSDQuery; DatabaseWrapper: TBoldDatabaseWrapper);
begin
  inherited create(DatabaseWrapper);
  FQuery := Query;
  SetParamCheck(true);
end;

function TBoldSQLDirectQuery.Createparam(FldType: TFieldType; const ParamName: string; ParamType: TParamType; Size: integer): IBoldParameter;
begin
  result := TBoldSqlDirectDbParameter.Create(Query.params.CreateParam(fldType, ParamName, ParamType), self);
end;

procedure TBoldSQLDirectQuery.ExecSQL;
var
  tstart, tend: TTime;
begin
  try
    if InBatch then begin
      BatchExecSQL;
    end
    else begin
      BoldLogSQL(Query.SQL);
      kiCLogSQL(BuildLogStr(Query));
      tstart:=Now;
      Query.ExecSQL;
      tend:=Now;
      kiCLogSQL('Dauer: '+IntToStr(SecondsBetween(tstart, tend))+' Sekunden'+#13#10+#13#10);
    end;
  except
    on E: Exception do begin
      var mes:=e.Message + BOLDCRLF + 'SQL: '+ Query.SQL.Text;
      kiCLogSQLException(mes);
      raise TBoldSQLDirectDatabase(DatabaseWrapper).GetDatabaseError(E, Query.SQL.Text);
    end;
  end;
end;

function TBoldSQLDirectQuery.FindParam(const Value: string): IBoldParameter;
var
  Param: TParam;
begin
  result := nil;
  Param := Query.Params.FindParam(Value);
  if Assigned(Param) then
    result := TBoldSqlDirectDbParameter.Create(Param, self)
end;

function TBoldSQLDirectQuery.GetDataSet: TDataSet;
begin
  result := Query;
end;

function TBoldSQLDirectQuery.GetParamCheck: Boolean;
begin
  Result := Query.ParamCheck;
end;

function TBoldSQLDirectQuery.GetParamCount: integer;
begin
  result := Query.Params.count;
end;

function TBoldSQLDirectQuery.GetParams: TParams;
begin
  result := Query.Params;
end;

function TBoldSQLDirectQuery.GetParam(i: integer): IBoldParameter;
begin
  result := TBoldSqlDirectDbParameter.Create(Query.Params[i], self);
end;

function TBoldSQLDirectQuery.GetQuery: TSDQuery;
begin
  if not assigned(fQuery) then
    fQuery := TSDQuery.Create(nil);
  result := fQuery;
end;

function TBoldSQLDirectQuery.GetRecordCount: integer;
begin
  Result := Query.RecordCount;
end;

function TBoldSQLDirectQuery.GetRequestLiveQuery: Boolean;
begin
  result := Query.RequestLive;
end;

function TBoldSQLDirectQuery.GetRowsAffected: integer;
begin
  result := Query.RowsAffected;
end;

function TBoldSQLDirectQuery.GetSQLStrings: TStrings;
begin
  result := Query.SQL;
end;

function TBoldSQLDirectQuery.GetSQLText: String;
begin
  result := Query.SQL.Text;
end;

function TBoldSQLDirectQuery.GetUseReadTransactions: boolean;
begin
  result := fUseReadTransactions;
end;

procedure TBoldSQLDirectQuery.Open;
var
  tstart, tend: TTime;
  mes: String;
begin
  BoldLogSQL(Query.SQL);
  kiCLogSQL(BuildLogStr(Query));
  try
    tstart:=Now;
    inherited;
    tend:=Now;
    kiCLogSQL('Dauer: '+IntToStr(SecondsBetween(tstart, tend))+' Sekunden'+#13#10+#13#10);
  except
    on e: Exception do
    begin
      mes:=e.Message + BOLDCRLF + 'SQL: '+ Query.SQL.Text;
      kiCLogSQLException(mes);
      raise TBoldSQLDirectDatabase(DatabaseWrapper).GetDatabaseError(E, Query.SQL.Text);
    end;
  end
end;

function TBoldSQLDirectQuery.ParamByName(const Value: string): IBoldParameter;
var
  Param: TParam;
begin
  Param := Query.ParamByName(Value);
  result := TBoldSqlDirectDbParameter.Create(Param, self);
end;


procedure TBoldSQLDirectQuery.SetParamCheck(value: Boolean);
begin
  Query.ParamCheck := Value;
end;

procedure TBoldSQLDirectQuery.SetRequestLiveQuery(NewValue: Boolean);
begin
end;

procedure TBoldSQLDirectQuery.SetUseReadTransactions(value: boolean);
begin
  fUseReadTransactions := value;
end;


{ TBoldSQLDirectTable }

procedure TBoldSQLDirectTable.AddIndex(const Name, Fields: string;
  Options: TIndexOptions; const DescFields: string);
begin
  Assert(False, 'TBoldSQLDirectTable.AddIndex: Not Implemented');
  // Table.AddIndex(Name, Fields, Options, DescFields);
end;

constructor TBoldSQLDirectTable.Create(Table: TSDTable; DatabaseWrapper: TBoldDatabaseWrapper);
begin
  inherited Create(DatabaseWrapper);
  fTable := Table;
end;

procedure TBoldSQLDirectTable.CreateTable;
begin
  Table.CreateTable;
end;

procedure TBoldSQLDirectTable.DeleteTable;
begin
  Table.DeleteTable;
end;

function TBoldSQLDirectTable.FindParam(const Value: string): IBoldParameter;
begin
  Assert(False, 'Param not available on table. 6FEF4465-001D-4628-BDE5-FF37F5D6C493');
end;

function TBoldSQLDirectTable.GetDataSet: TDataSet;
begin
  result := Table;
end;

function TBoldSQLDirectTable.GetExclusive: Boolean;
begin
  result:=false;
  Assert(False, 'TBoldSQLDirectTable.GetExclusive: Not Implemented');
  //  result := Table.Exclusive;
end;

function TBoldSQLDirectTable.GetExists: Boolean;
begin
  result := Table.Exists;
end;

function TBoldSQLDirectTable.GetIndexDefs: TIndexDefs;
begin
  result := Table.IndexDefs;
end;

function TBoldSQLDirectTable.GetTable: TSDTable;
begin
  if not assigned(fTable) then
    fTable := TSDTable.Create(nil);
  result := fTable
end;

function TBoldSQLDirectTable.GetTableName: String;
begin
  result := Table.TableName;
end;

function TBoldSQLDirectTable.ParamByName(const Value: string): IBoldParameter;
begin
  Assert(False, 'Param not available on table. 42BA6296-0B9C-4EFD-8FB4-77EA0F96BE77');
end;

procedure TBoldSQLDirectTable.SetExclusive(NewValue: Boolean);
begin
  Assert(False, 'TBoldSQLDirectTable.DetExclusive: Not Implemented');
  // Table.Exclusive := NewValue;
end;

procedure TBoldSQLDirectTable.SetTableName(const NewName: String);
begin
  Table.TableName := NewName;
end;

{ TBoldSDDataBase }

procedure TBoldSQLDirectDatabase.AllTableNames(Pattern: String; ShowSystemTables: Boolean; TableNameList: TStrings);
var
  i, dotpos: Integer;
  TableOwner: String;
begin
  // when ShowSystemTables = true SQLDirect returns only System tables, no user tables, which we never want
  // in other DB implementations ShowSystemTables means User tables + System tables.
  ShowSystemTables := false;
  fDatabase.Session.GetTableNames(fDatabase.DatabaseName, Pattern, ShowSystemTables, TableNameList);

  TableOwner:=fDatabase.Params.Values['USER NAME']+'.';
  i:=0;
  while i<=TableNameList.Count-1 do begin
    if fDatabase.ServerType=stOracle then begin
      if not AnsiSameText(LeftStr(TableNameList[i], Length(TableOwner)), TableOwner) then begin
        TableNameList.Delete(i);
        Continue;
      end;
    end;
    dotPos := pos('.', TableNameList[i]);
    if dotPos > 0 then begin
      TableNameList[i] := Copy(TableNameList[i], dotPos+1, maxInt);
    end;
    Inc(i);
  end;

  if Pattern <> '' then begin
    for i := TableNameList.Count - 1 downto 0 do begin
      if not MatchesMask(TableNameList[i], Pattern) then begin
        TableNameList.Delete(i);
      end;
    end;
  end;
end;

procedure TBoldSQLDirectDatabase.Close;
begin
  Database.Close;
end;

procedure TBoldSQLDirectDatabase.Commit;
begin
  Database.Commit;
end;

constructor TBoldSQLDirectDatabase.create(DataBase: TSDDataBase; SQLDataBaseConfig: TBoldSQLDatabaseConfig);
begin
  inherited Create(SQLDataBaseConfig);
  FDataBase := DataBase;
end;

destructor TBoldSQLDirectDatabase.Destroy;
begin
  inherited;
  FDatabase := nil;
  FreeAndNil(fCachedTable);
  FreeAndNil(fCachedQuery);
end;

function TBoldSQLDirectDatabase.GetConnected: Boolean;
begin
  result := Database.Connected;
end;

function TBoldSQLDirectDatabase.GetDataBase: TSDDataBase;
begin
  result := FDataBase;
end;

function TBoldSQLDirectDatabase.GetDatabaseError(const E: Exception;
  const sSQL: string): EBoldDatabaseError;
var
  aErrorType: TBoldDatabaseErrorType;
  sServer,
  sDatabase,
  sUsername: string;
const
  cMSSQLProvider = 'SQL Server';
  cOracleProvider = 'Oracle';
  cFirebirdProvider = 'Firebird';
begin
  sServer:='';
  if FDatabase.ServerType=stOracle then
    sServer:=cOracleProvider
  else
  if FDatabase.ServerType=stFirebird then
    sServer:=cFirebirdProvider
  else
  if FDatabase.ServerType=stOLEDB then
    sServer:=cMSSQLProvider
  else
  if FDatabase.ServerType=stODBC then begin
    if ContainsStr(FDatabase.RemoteDatabase, cMSSQLProvider) then
      sServer:=cMSSQLProvider
    else
    if ContainsStr(FDatabase.RemoteDatabase, cOracleProvider) then
      sServer:=cOracleProvider
  end;
  if sServer='' then begin
    raise Exception.Create(
        'TBoldSQLDirectDatabase.GetDatabaseError: Error codes not implemented for '
        + FDatabase.RemoteDatabase+' ('+IntToStr(Integer(FDatabase.ServerType))+')');
  end;
  sDatabase := FDatabase.RemoteDatabase;
  sUsername := FDatabase.Params.Values['USER NAME'];
  aErrorType := bdetError;

  if not FDatabase.Connected then begin
    aErrorType := bdetConnection;
  end
  else begin
    if (E is ESDEngineError) then begin
      if sServer=cMSSQLProvider then begin
        case ESDEngineError(E).ErrorCode of
          -2147217900, -2139062144, -2147467259, -1, 2, 53, 233, 6005: aErrorType := bdetConnection;
          4060, 18456: aErrorType := bdetLogin;

        end;
      end
      else
      if sServer=cOracleProvider then begin
        case ESDEngineError(E).ErrorCode of
          1089, 1033, 1034, 3113, 12154, 12203, 12500, 12518, 12545, 12560: aErrorType:=bdetConnection;
          1017: aErrorType := bdetLogin;
        end;
      end
      else
      if sServer=cFirebirdProvider then begin
        case ESDEngineError(E).ErrorCode of
          335544721, 335544722, 335544741, 335544856, 335544421, 335544648: aErrorType:=bdetConnection;
        end;
      end;
    end;
  end;
  Result := InternalGetDatabaseError(aErrorType, E, sSQL, sServer, sDatabase,
      sUsername, False);
end;

function TBoldSQLDirectDatabase.GetInTransaction: Boolean;
begin
  result := Database.InTransaction;
end;

function TBoldSQLDirectDatabase.GetIsExecutingQuery: Boolean;
begin
  Result := fExecuteQueryCount > 0;
end;

function TBoldSQLDirectDatabase.GetIsSQLBased: Boolean;
begin
  result := DataBase.IsSQLBased;
end;

function TBoldSQLDirectDatabase.GetKeepConnection: Boolean;
begin
  result := DataBase.KeepConnection;
end;

function TBoldSQLDirectDatabase.GetLogInPrompt: Boolean;
begin
  result := DataBase.LoginPrompt;
end;

function TBoldSQLDirectDatabase.GetQuery: IBoldQuery;
var
  Query: TSDQuery;
begin
  if assigned(fCachedQuery) then
  begin
    Query := fCachedQuery;
    fCachedQuery := nil;
  end
  else
  begin
    Query := TSDQuery.Create(nil);
    Query.SessionName := Database.SessionName;
    Query.DatabaseName := Database.DatabaseName;
  end;
  result := BoldSQLDirectQueryClass.Create(Query, self);
end;

function TBoldSQLDirectDatabase.GetTable: IBoldTable;
var
  Table: TSDTable;
begin
  if assigned(fCachedTable) then
  begin
    Table := fCachedTable;
    fCachedTable := nil;
  end
  else
  begin
    Table := TSDTable.Create(nil);
    Table.SessionName := Database.SessionName;
    Table.DatabaseName := DataBase.DataBaseName;
  end;
  result := TBoldSQLDirectTable.Create(Table, self);
end;

procedure TBoldSQLDirectDatabase.Open;
begin
  try
    Database.Open;
  except
    on E: Exception do begin
      raise GetDatabaseError(E);
    end;
  end;
end;

procedure TBoldSQLDirectDatabase.Reconnect;
begin
  Assert(False, 'TBoldSQLDirectDatabase.Reconnect: Not Implemented');
end;

procedure TBoldSQLDirectDatabase.ReleaseCachedObjects;
begin
  FreeAndNil(fCachedTable);
  FreeAndNil(fCachedQuery);
end;

type TCollectionAccess = class(TCollection);

procedure TBoldSQLDirectDatabase.ReleaseQuery(var Query: IBoldQuery);
var
  SDQuery: TBoldSQLDirectQuery;
begin
  if Query.Implementor is TBoldSQLDirectQuery then begin
    SDQuery := Query.Implementor as TBoldSQLDirectQuery;
    Query := nil;
    if not assigned(FCachedQuery) then begin
      FCachedQuery := SDQuery.FQuery;
      if FCachedQuery.Active then
        FCachedQuery.Close;
      FCachedQuery.SQL.Clear;
      while FCachedQuery.SQL.Updating do
        FCachedQuery.SQL.EndUpdate;
      FCachedQuery.Params.Clear;
      while TCollectionAccess(FCachedQuery.Params).UpdateCount > 0 do
        FCachedQuery.Params.EndUpdate;
    end
    else
      SDQuery.FQuery.free;
    SDQuery.Free;
  end;
end;

procedure TBoldSQLDirectDatabase.ReleaseTable(var Table: IBoldTable);
var
  SQLDirectTable: TBoldSQLDirectTable;
begin
  if Table.Implementor is TBoldSQLDirectTable then
begin
    SQLDirectTable := Table.Implementor as TBoldSQLDirectTable;
    Table := nil;
    if not assigned(fCachedTable) then
      fCachedTable := SQLDirectTable.fTable
    else
      SQLDirectTable.fTable.free;
    SQLDIrectTable.Free;
  end;
end;

procedure TBoldSQLDirectDatabase.Rollback;
begin
  Database.Rollback;
end;

procedure TBoldSQLDirectDatabase.SetKeepConnection(NewValue: Boolean);
begin
  Database.KeepConnection := NewValue;
end;

procedure TBoldSQLDirectDatabase.SetlogInPrompt(NewValue: Boolean);
begin
  Database.LoginPrompt := NewValue;
end;

procedure TBoldSQLDirectDatabase.StartTransaction;
begin
  Database.StartTransaction;
end;

function TBoldSQLDirectDatabase.SupportsTableCreation: boolean;
begin
  result := true;
end;

{ TBoldSqlDirectDbParameter }

function TBoldSqlDirectDbParameter.GetParameter: TParam;
begin
  result := self.Parameter;
end;

end.
