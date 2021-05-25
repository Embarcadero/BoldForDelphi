
{ Global compiler directives }
{$include bold.inc}
unit BoldBDEInterfaces;

interface

uses
  Classes,
  Db,
  DbTables,
  BoldSQLDatabaseConfig,
  BoldDBInterfaces;

type
  { forward declarations }
  TBoldBDEDataBase = class;
  TBoldBDEQuery = class;
  TBoldBDETable = class;

  TBoldBDEQueryClass = class of TBoldBDEQuery;

  { TBoldBDEQuery }
  TBoldBDEQuery = class(TBoldDataSetWrapper, IBoldQuery, IBoldExecQuery, IBoldParameterized)
  private
    fQuery: TQuery;
    function GetQuery: TQuery;
    procedure AssignParams(SourceParams: TParams);
    function GetParamCount: integer;
    function GetParams: TParams;    
    function GetParam(i: integer): IBoldParameter;
    function GetRequestLiveQuery: Boolean;
    function ParamByName(const Value: string): IBoldParameter;
    procedure SetRequestLiveQuery(NewValue: Boolean);
    function GetSQLText: String;
    function GetSQLStrings: TStrings;    
    procedure AssignSQL(SQL: TStrings);
    procedure AssignSQLText(const SQL: String);
    function GetParamCheck: Boolean;
    procedure SetParamCheck(value: Boolean);    
    function GetRowsAffected: integer;
    function GetRecordCount: integer;
    function Createparam(FldType: TFieldType; const ParamName: string; ParamType: TParamType; Size: integer): IBoldParameter; override;
    function GetUseReadTransactions: boolean;
    procedure SetUseReadTransactions(value: boolean);
    procedure BeginExecuteQuery;
    procedure EndExecuteQuery;
    function GetBatchQueryParamCount: integer;    
  protected
    function GetDataSet: TDataSet; override;
    procedure StartSQLBatch; virtual;
    procedure EndSQLBatch; virtual;
    procedure FailSQLBatch; virtual;
    procedure ClearParams;
    procedure ExecSQL; virtual;
    property Query: TQuery read GetQuery;
    procedure Open; override;
  public
    constructor Create(Query: TQuery; DatabaseWrapper: TBoldDatabaseWrapper); virtual;
  end;

  { TBoldBDETable }
  TBoldBDETable = class(TBoldDataSetWrapper, IBoldTable)
  private
    fTable: TTable;
    procedure AddIndex(const Name, Fields: string; Options: TIndexOptions; const DescFields: string = '');
    procedure CreateTable;
    procedure DeleteTable;
    function GetTable: TTable;
    function GetIndexDefs: TIndexDefs;
    procedure SetTableName(const NewName: String);
    function GetTableName: String;
    procedure SetExclusive(NewValue: Boolean);
    function GetExclusive: Boolean;
    function GetExists: Boolean;
    property Table: TTable read GetTable;
  protected
    function GetDataSet: TDataSet; override;
  public
    constructor Create(Table: TTable; DatabaseWrapper: TBoldDatabaseWrapper);
  end;

  { TBoldBDEDataBase }
  TBoldBDEDataBase = class(TBoldDatabaseWrapper, IBoldDataBase)
  private
    fDataBase: TDataBase;
    fCachedTable: TTable;
    fCachedQuery: TQuery;
    fExecuteQueryCount: integer;
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
    procedure Reconnect;
    function GetTable: IBoldTable;
    procedure ReleaseTable(var Table: IBoldTable);
    function SupportsTableCreation: Boolean;
    procedure ReleaseCachedObjects;
    function GetIsExecutingQuery: Boolean;
    procedure BeginExecuteQuery;
    procedure EndExecuteQuery;
  protected
    procedure AllTableNames(Pattern: String; ShowSystemTables: Boolean; TableNameList: TStrings); override;
    function GetQuery: IBoldQuery; override;
    procedure ReleaseQuery(var Query: IBoldQuery); override;
  public
    constructor create(DataBase: TDataBase; SQLDataBaseConfig: TBoldSQLDatabaseConfig);
    destructor destroy; override;
    procedure CreateDatabase;    
  end;

var
  BoldBDEQueryClass: TBoldBDEQueryClass = TBoldBDEQuery;

implementation

uses
  SysUtils,
  BoldDefs,
  BoldUtils;

{ TBoldBDEQuery }

procedure TBoldBDEQuery.AssignParams(Sourceparams: tparams);
begin
  if assigned(Sourceparams) then
    Query.Params.Assign(SourceParams)
  else
    Query.Params.Clear;
end;

function TBoldBDEQuery.GetQuery: TQuery;
begin
  if not assigned(fQuery) then
    fQuery := TQuery.Create(nil);
  result := fQuery;
end;

function TBoldBDEQuery.GetParamCheck: Boolean;
begin
  Result := Query.ParamCheck;
end;

function TBoldBDEQuery.GetParamCount: integer;
begin
  result := Query.params.count;
end;

function TBoldBDEQuery.GetParams: TParams;
begin
  result := Query.Params;
end;

function TBoldBDEQuery.GetParam(I: integer): IBoldParameter;
begin
  result := TBoldDbParameter.Create(Query.Params[i], self);
end;

function TBoldBDEQuery.GetREquestLiveQuery: Boolean;
begin
  result := Query.RequestLive;
end;

function TBoldBDEQuery.ParamByName(const Value: string): IBoldParameter;
var
  Param: TParam;
begin
  Param := Query.Params.FindParam(Value);
  if assigned(Param) then
    result := TBoldDbParameter.Create(Param, self)
  else
    result := nil;
end;

procedure TBoldBDEQuery.SetParamCheck(value: Boolean);
begin
  Query.ParamCheck := Value;
end;

procedure TBoldBDEQuery.SetRequestLiveQuery(NewValue: Boolean);
begin
  Query.RequestLive := NewValue;
end;

procedure TBoldBDEQuery.SetUseReadTransactions(value: boolean);
begin

end;

function TBoldBDEQuery.GetBatchQueryParamCount: integer;
begin
  result := 0
end;

function TBoldBDEQuery.GetDataSet: TDataSet;
begin
  result := Query;
end;

procedure TBoldBDEQuery.ExecSQL;
begin
  BeginExecuteQuery;
  try
  BoldLogSQL(Query.SQL);
  try
    Query.ExecSQL;
  except
    on e: Exception do
    begin
      e.Message := e.Message + BOLDCRLF + 'SQL: ' + Query.SQL.text;
      raise;
    end;
  end
  finally
    EndExecuteQuery;
  end;
end;

constructor TBoldBDEQuery.Create(Query: TQuery; DatabaseWrapper: TBoldDatabaseWrapper);
begin
  inherited Create(DatabaseWrapper);
  fQuery := Query;
  SetParamCheck(true);  
end;

procedure TBoldBDEQuery.EndExecuteQuery;
begin
  (DatabaseWrapper as TBoldBDEDataBase).EndExecuteQuery;
end;

procedure TBoldBDEQuery.EndSQLBatch;
begin
end;

procedure TBoldBDEQuery.StartSQLBatch;
begin
end;

procedure TBoldBDEQuery.FailSQLBatch;
begin
end;

procedure TBoldBDEQuery.Open;
begin
  BeginExecuteQuery;
  try
  BoldLogSQL(Query.SQL);
  try
    inherited;
  except
    on e: Exception do
    begin
      e.Message := e.Message + BOLDCRLF + 'SQL: ' + Query.SQL.text;
      raise;
    end;
  end
  finally
    EndExecuteQuery;
  end;
end;

procedure TBoldBDEQuery.AssignSQL(SQL: TStrings);
begin
  Query.SQL.Assign(SQL);
end;

procedure TBoldBDEQuery.AssignSQLText(const SQL: String);
begin
  Query.SQL.BeginUpdate;
  Query.SQL.Clear;
  BoldAppendToStrings(Query.SQL, SQL, true);
  Query.SQL.EndUpdate;
end;

procedure TBoldBDEQuery.BeginExecuteQuery;
begin
  (DatabaseWrapper as TBoldBDEDataBase).EndExecuteQuery;
end;

function TBoldBDEQuery.GetSQLStrings: TStrings;
begin
  result := Query.SQL;
end;

function TBoldBDEQuery.GetSQLText: String;
begin
  result := Query.SQL.text;
end;

function TBoldBDEQuery.GetUseReadTransactions: boolean;
begin
  result := false;
end;

function TBoldBDEQuery.GetRowsAffected: integer;
begin
  result := Query.RowsAffected;
end;

function TBoldBDEQuery.GetRecordCount: integer;
begin
  result := Query.RecordCount;
end;

function TBoldBDEQuery.Createparam(FldType: TFieldType; const ParamName: string; ParamType: TParamType; Size: integer): IBoldParameter;
begin
  result := TBoldDbParameter.Create(Query.params.CreateParam(fldType, ParamName, ParamType), self);
end;

procedure TBoldBDEQuery.ClearParams;
begin
  query.Params.Clear;
end;

{ TBoldBDETable }

procedure TBoldBDETable.AddIndex(const Name, Fields: string;
  Options: TIndexOptions; const DescFields: string);
begin
  Table.AddIndex(Name, Fields, Options, DescFields);
end;

constructor TBoldBDETable.Create(Table: TTable; DatabaseWrapper: TBoldDatabaseWrapper);
begin
  inherited Create(DatabaseWrapper);
  fTable := Table;
end;

procedure TBoldBDETable.CreateTable;
begin
  Table.CreateTable;
end;

procedure TBoldBDETable.DeleteTable;
begin
  Table.DeleteTable;
end;

function TBoldBDETable.GetDataSet: TDataSet;
begin
  result := Table;
end;

function TBoldBDETable.GetExclusive: Boolean;
begin
  result := Table.Exclusive;
end;

function TBoldBDETable.GetExists: Boolean;
begin
  result := Table.Exists;
end;

function TBoldBDETable.GetIndexDefs: TIndexDefs;
begin
  result := Table.IndexDefs;
end;

function TBoldBDETable.GetTable: TTable;
begin
  if not assigned(fTable) then
    fTable := TTable.Create(nil);
  result := fTable
end;

function TBoldBDETable.GetTableName: String;
begin
  result := Table.TableName;
end;

procedure TBoldBDETable.SetExclusive(NewValue: Boolean);
begin
  Table.Exclusive := NewValue;
end;

procedure TBoldBDETable.SetTableName(const NewName: String);
begin
  Table.TableName := NewName;
end;

{ TBoldBDEDataBase }

procedure TBoldBDEDataBase.AllTableNames(Pattern: String; ShowSystemTables: Boolean; TableNameList: TStrings);
begin
  if Pattern = '' then
    Pattern := '*';
  Session.GetTableNames(fDataBase.DatabaseName, Pattern, False, ShowSystemTables, TableNameList);
end;

function TBoldBDEDataBase.GetInTransaction: Boolean;
begin
  result := fDataBase.InTransaction;
end;

function TBoldBDEDataBase.GetIsExecutingQuery: Boolean;
begin
  Result := fExecuteQueryCount > 0;
end;

function TBoldBDEDataBase.GetIsSQLBased: Boolean;
begin
  result := fDataBase.IsSQLBased;
end;

function TBoldBDEDataBase.GetKeepConnection: Boolean;
begin
  result := fDataBase.KeepConnection;
end;

function TBoldBDEDataBase.GetLogInPrompt: Boolean;
begin
  result := fDataBase.LoginPrompt;
end;

procedure TBoldBDEDataBase.SetKeepConnection(NewValue: Boolean);
begin
  fDataBase.KeepConnection := NewValue;
end;

procedure TBoldBDEDataBase.SetlogInPrompt(NewValue: Boolean);
begin
  fDataBase.LoginPrompt := NewValue;
end;

constructor TBoldBDEDataBase.create(DataBase: TDataBase; SQLDataBaseConfig: TBoldSQLDatabaseConfig);
begin
  inherited Create(SQLDataBaseConfig);
  fDataBase := DataBase;
end;

procedure TBoldBDEDataBase.CreateDatabase;
begin
  Assert(false, 'Not implemented.');
end;

function TBoldBDEDataBase.GetConnected: Boolean;
begin
  result := fDataBase.Connected;
end;

procedure TBoldBDEDataBase.StartTransaction;
begin
  fDataBase.StartTransaction;
end;

procedure TBoldBDEDataBase.Commit;
begin
  fDatabase.Commit;
end;

procedure TBoldBDEDataBase.RollBack;
begin
  fDataBase.Rollback;
end;

procedure TBoldBDEDataBase.Open;
begin
  fDataBase.Open;
end;

procedure TBoldBDEDataBase.BeginExecuteQuery;
begin
  inc(fExecuteQueryCount);
end;

procedure TBoldBDEDataBase.EndExecuteQuery;
begin
  dec(fExecuteQueryCount);
end;

procedure TBoldBDEDataBase.Close;
begin
  fDataBase.Close;
end;

destructor TBoldBDEDataBase.destroy;
begin
  inherited;
  fDatabase := nil;
  FreeAndNil(fCachedTable);
  FreeAndNil(fCachedQuery);
end;

function TBoldBDEDataBase.GetQuery: IBoldQuery;
var
  Query: TQuery;
begin
  if assigned(fCachedQuery) then
  begin
    Query := fCachedQuery;
    fCachedQuery := nil;
  end
  else
  begin
    Query := TQuery.Create(nil);
    Query.SessionName := fDatabase.SessionName;
    Query.DatabaseName := fDataBase.DataBaseName;
  end;
  result := BoldBDEQueryClass.Create(Query, self);
end;

function TBoldBDEDataBase.GetTable: IBoldTable;
var
  Table: TTable;
begin
  if assigned(fCachedTable) then
  begin
    Table := fCachedTable;
    fCachedTable := nil;
  end
  else
  begin
    Table := TTable.Create(nil);
    Table.SessionName := fDatabase.SessionName;
    Table.DatabaseName := fDataBase.DataBaseName;
  end;
  result := TBoldBDETable.Create(Table, self);
end;

procedure TBoldBDEDataBase.ReleaseQuery(var Query: IBoldQuery);
var
  BDEQuery: TBoldBDEQuery;
begin
  if Query.Implementor is TBoldBDEQuery then
  begin
    BDEQuery := Query.Implementor as TBoldBDEQuery;
    Query := nil;
    if not assigned(fCachedQuery) then
    begin
      fCachedQuery := BDEQuery.fQuery;
      if fCachedQuery.Active then
        fCachedQuery.Close;
      fCachedQuery.SQL.Clear;
    end
    else
      BDEQuery.fQuery.free;
    BDEQuery.Free;
  end;
end;

procedure TBoldBDEDataBase.ReleaseTable(var Table: IBoldTable);
var
  BDETable: TBoldBDETable;
begin
  if Table.Implementor is TBoldBDETable then
  begin
    BDETable := Table.Implementor as TBoldBDETable;
    Table := nil;
    if not assigned(fCachedTable) then
      fCachedTable := BDETable.fTable
    else
      BDETable.fTable.free;
    BDETable.Free;
  end;
end;

function TBoldBDEDataBase.SupportsTableCreation: Boolean;
begin
  result := true;
end;

procedure TBoldBDEDataBase.Reconnect;
begin
  if Assigned(FDataBase) then begin
    FDataBase.Connected := False;
    FDataBase.Connected := True;
  end;
end;

procedure TBoldBDEDataBase.ReleaseCachedObjects;
begin
  FreeAndNil(fCachedTable);
  FreeAndNil(fCachedQuery);
end;

end.
