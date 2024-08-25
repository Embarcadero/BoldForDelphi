
{ Global compiler directives }
{$include bold.inc}
unit BoldDBISAMInterfaces;

interface

uses
  Classes,
  Db,
  dbisamtb,
  BoldSQLDatabaseConfig,
  BoldDBInterfaces;

type
  { forward declarations }
  TBoldDBISAMDataBase = class;
  TBoldDBISAMQuery = class;
  TBoldDBISAMTable = class;

  TBoldDBISAMQueryClass = class of TBoldDBISAMQuery;

  { TBoldDBISAMQuery }
  TBoldDBISAMQuery = class(TBoldDataSetWrapper, IBoldQuery, IBoldExecQuery, IBoldParameterized)
  private
    fQuery: TDBISAMQuery;
    function GetQuery: TDBISAMQuery;
    procedure AssignParams(SourceParams: TParams);
    procedure ClearParams; 
    function GetParamCount: integer;
    function GetParams(i: integer): IBoldParameter;
    function GetRequestLiveQuery: Boolean;
    function ParamByName(const Value: string): IBoldParameter;
    procedure SetRequestLiveQuery(NewValue: Boolean);
    function GetSQLText: String;
    procedure AssignSQL(SQL: TStrings);
    procedure AssignSQLText(SQL: String);
    function GetParamCheck: Boolean;
    procedure SetParamCheck(value: Boolean);    
    function GetRowsAffected: integer;
    function GetRecordCount: integer;
    function Createparam(FldType: TFieldType; const ParamName: string; ParamType: TParamType; Size: integer): IBoldParameter;
  protected
    function GetDataSet: TDataSet; override;
    procedure StartSQLBatch; virtual;
    procedure EndSQLBatch; virtual;
    procedure FailSQLBatch; virtual;
    procedure ExecSQL; virtual;
    property Query: TDBISAMQuery read GetQuery;
    procedure Open; override;
  public
    constructor Create(Query: TDBISAMQuery; DatabaseWrapper: TBoldDatabaseWrapper); virtual;
  end;

  { TBoldDBISAMTable }
  TBoldDBISAMTable = class(TBoldDataSetWrapper, IBoldTable)
  private
    fTable: TDBISAMTable;
    procedure AddIndex(const Name, Fields: string; Options: TIndexOptions; const DescFields: string = '');
    procedure CreateTable;
    procedure DeleteTable;
    function GetTable: TDBISAMTable;
    function GetIndexDefs: TIndexDefs;
    procedure SetTableName(NewName: String);
    function GetTableName: String;
    procedure SetExclusive(NewValue: Boolean);
    function GetExclusive: Boolean;
    function GetExists: Boolean;
    property Table: TDBISAMTable read GetTable;
  protected
    function GetDataSet: TDataSet; override;
  public
    constructor Create(Table: TDBISAMTable; DatabaseWrapper: TBoldDatabaseWrapper);
  end;

  { TBoldDBISAMDataBase }
  TBoldDBISAMDataBase = class(TBoldDatabaseWrapper, IBoldDataBase)
  private
    fDataBase: TDBISAMDataBase;
    fCachedTable: TDBISAMTable;
    fCachedQuery: TDBISAMQuery;
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
    function SupportsTableCreation: Boolean;
    procedure ReleaseCachedObjects;
  protected
    function SupportsDefaultColumnValues: Boolean; override;
    procedure AllTableNames(Pattern: String; ShowSystemTables: Boolean; TableNameList: TStrings); override;
    function GetQuery: IBoldQuery; override;
    procedure ReleaseQuery(var Query: IBoldQuery); override;
    function GetTable: IBoldTable; override;
    procedure ReleaseTable(var Table: IBoldTable); override;
  public
    constructor create(DataBase: TDBISAMDataBase; SQLDataBaseConfig: TBoldSQLDatabaseConfig);
    destructor destroy; override;
  end;

var
  BoldDBISAMQueryClass: TBoldDBISAMQueryClass = TBoldDBISAMQuery;

implementation

uses
  SysUtils,
  BoldDefs;

{ TBoldDBISAMQuery }

procedure TBoldDBISAMQuery.AssignParams(Sourceparams: tparams);
begin
  if assigned(Sourceparams) then
    Query.Params.Assign(SourceParams)
  else
    Query.Params.Clear;
end;

function TBoldDBISAMQuery.GetQuery: TDBISAMQuery;
begin
  if not assigned(fQuery) then
    fQuery := TDBISAMQuery.Create(nil);
  result := fQuery;
end;

function TBoldDBISAMQuery.GetParamCheck: Boolean;
begin
  Result := Query.ParamCheck;
end;

function TBoldDBISAMQuery.GetParamCount: integer;
begin
  result := Query.params.count;
end;

function TBoldDBISAMQuery.GetParams(I: integer): IBoldParameter;
begin
  result := TBoldDbParameter.Create(Query.Params[i], self);
end;

function TBoldDBISAMQuery.GetRequestLiveQuery: Boolean;
begin
  result := Query.RequestLive;
end;

function TBoldDBISAMQuery.ParamByName(const Value: string): IBoldParameter;
var
  Param: TParam;
begin
  Param := Query.Params.FindParam(Value);
  if assigned(Param) then
    result := TBoldDbParameter.Create(Param, self)
  else
    result := nil;
end;

procedure TBoldDBISAMQuery.SetParamCheck(value: Boolean);
begin
  Query.ParamCheck := Value;
end;

procedure TBoldDBISAMQuery.SetRequestLiveQuery(NewValue: Boolean);
begin
  Query.RequestLive := NewValue;
end;

function TBoldDBISAMQuery.GetDataSet: TDataSet;
begin
  result := Query;
end;

procedure TBoldDBISAMQuery.ExecSQL;
begin
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
end;

constructor TBoldDBISAMQuery.Create(Query: TDBISAMQuery; DatabaseWrapper: TBoldDatabaseWrapper);
begin
  inherited Create(DatabaseWrapper);
  fQuery := Query;
  SetParamCheck(true);  
end;

procedure TBoldDBISAMQuery.EndSQLBatch;
begin
end;

procedure TBoldDBISAMQuery.StartSQLBatch;
begin
end;

procedure TBoldDBISAMQuery.FailSQLBatch;
begin
end;

procedure TBoldDBISAMQuery.Open;
begin
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
end;

procedure TBoldDBISAMQuery.AssignSQL(SQL: TStrings);
begin
  AssignSQLText(SQL.Text);
end;

procedure TBoldDBISAMQuery.AssignSQLText(SQL: String);

begin
  SQL := StringReplace(SQL, BOLDCRLF, ' ', [rfReplaceAll]);
  Query.SQL.BeginUpdate;
  Query.SQL.Clear;
  Query.SQL.Add(SQL);
  Query.SQL.EndUpdate;
end;

function TBoldDBISAMQuery.GetSQLText: String;
begin
  result := Query.SQL.text;
end;

function TBoldDBISAMQuery.GetRowsAffected: integer;
begin
  result := Query.RowsAffected;
end;

function TBoldDBISAMQuery.GetRecordCount: integer;
begin
  result := Query.RecordCount;
end;

function TBoldDBISAMQuery.Createparam(FldType: TFieldType; const ParamName: string; ParamType: TParamType; Size: integer): IBoldParameter;
begin
  result := TBoldDbParameter.Create(Query.params.CreateParam(fldType, ParamName, ParamType), self);
end;

procedure TBoldDBISAMQuery.ClearParams;
begin
  Query.Params.clear;
end;

{ TBoldDBISAMTable }

procedure TBoldDBISAMTable.AddIndex(const Name, Fields: string;
  Options: TIndexOptions; const DescFields: string);
begin
  Table.AddIndex(Name, Fields, Options);
end;

constructor TBoldDBISAMTable.Create(Table: TDBISAMTable; DatabaseWrapper: TBoldDatabaseWrapper);
begin
  inherited Create(DatabaseWrapper);
  fTable := Table;
end;

procedure TBoldDBISAMTable.CreateTable;
begin
  Table.CreateTable;
end;

procedure TBoldDBISAMTable.DeleteTable;
begin
  Table.DeleteTable;
end;

function TBoldDBISAMTable.GetDataSet: TDataSet;
begin
  result := Table;
end;

function TBoldDBISAMTable.GetExclusive: Boolean;
begin
  result := Table.Exclusive;
end;

function TBoldDBISAMTable.GetExists: Boolean;
begin
  result := Table.Exists;
end;

function TBoldDBISAMTable.GetIndexDefs: TIndexDefs;
begin
  result := Table.IndexDefs;
end;

function TBoldDBISAMTable.GetTable: TDBISAMTable;
begin
  if not assigned(fTable) then
    fTable := TDBISAMTable.Create(nil);
  result := fTable
end;

function TBoldDBISAMTable.GetTableName: String;
begin
  result := Table.TableName;
end;

procedure TBoldDBISAMTable.SetExclusive(NewValue: Boolean);
begin
  Table.Exclusive := NewValue;
end;

procedure TBoldDBISAMTable.SetTableName(NewName: String);
begin
  Table.TableName := NewName;
end;

{ TBoldDBISAMDataBase }

procedure TBoldDBISAMDataBase.AllTableNames(Pattern: String; ShowSystemTables: Boolean; TableNameList: TStrings);
begin
  if Pattern = '' then
    Pattern := '*';
  Session.GetTableNames(fDataBase.DatabaseName, TableNameList);
end;

function TBoldDBISAMDataBase.GetInTransaction: Boolean;
begin
  result := fDataBase.InTransaction;
end;

function TBoldDBISAMDataBase.GetIsSQLBased: Boolean;
begin
  result := true;
end;

function TBoldDBISAMDataBase.GetKeepConnection: Boolean;
begin
  result := fDataBase.KeepConnection;
end;

function TBoldDBISAMDataBase.GetLogInPrompt: Boolean;
begin
  result := false;
end;

procedure TBoldDBISAMDataBase.SetKeepConnection(NewValue: Boolean);
begin
  fDataBase.KeepConnection := NewValue;
end;

procedure TBoldDBISAMDataBase.SetlogInPrompt(NewValue: Boolean);
begin
end;

constructor TBoldDBISAMDataBase.create(DataBase: TDBISAMDataBase; SQLDataBaseConfig: TBoldSQLDatabaseConfig);
begin
  inherited Create(SQLDataBaseConfig);
  fDataBase := DataBase;
end;

function TBoldDBISAMDataBase.GetConnected: Boolean;
begin
  result := fDataBase.Connected;
end;

procedure TBoldDBISAMDataBase.StartTransaction;
begin
  fDataBase.StartTransaction;
end;

procedure TBoldDBISAMDataBase.Commit;
begin
  fDatabase.Commit;
end;

procedure TBoldDBISAMDataBase.RollBack;
begin
  fDataBase.Rollback;
end;

procedure TBoldDBISAMDataBase.Open;
begin
  fDataBase.Open;
end;

procedure TBoldDBISAMDataBase.Close;
begin
  fDataBase.Close;
end;

destructor TBoldDBISAMDataBase.destroy;
begin
  inherited;
  fDatabase := nil;
  FreeAndNil(fCachedTable);
  FreeAndNil(fCachedQuery);
end;

function TBoldDBISAMDataBase.GetQuery: IBoldQuery;
var
  Query: TDBISAMQuery;
begin
  if assigned(fCachedQuery) then
  begin
    Query := fCachedQuery;
    fCachedQuery := nil;
  end
  else
  begin
    Query := TDBISAMQuery.Create(nil);
    Query.SessionName := fDatabase.SessionName;
    Query.DatabaseName := fDataBase.DataBaseName;
  end;
  result := BoldDBISAMQueryClass.Create(Query, self);
end;

function TBoldDBISAMDataBase.GetTable: IBoldTable;
var
  Table: TDBISAMTable;
begin
  if assigned(fCachedTable) then
  begin
    Table := fCachedTable;
    fCachedTable := nil;
  end
  else
  begin
    Table := TDBISAMTable.Create(nil);
    Table.SessionName := fDatabase.SessionName;
    Table.DatabaseName := fDataBase.DataBaseName;
  end;
  result := TBoldDBISAMTable.Create(Table, self);
end;

procedure TBoldDBISAMDataBase.ReleaseQuery(var Query: IBoldQuery);
var
  DBISAMQuery: TBoldDBISAMQuery;
begin
  if Query.Implementor is TBoldDBISAMQuery then
  begin
    DBISAMQuery := Query.Implementor as TBoldDBISAMQuery;
    Query := nil;
    if not assigned(fCachedQuery) then
    begin
      fCachedQuery := DBISAMQuery.fQuery;
      if fCachedQuery.Active then
        fCachedQuery.Close;
      fCachedQuery.SQL.Clear;
    end
    else
      DBISAMQuery.fQuery.free;
    DBISAMQuery.Free;
  end;
end;

procedure TBoldDBISAMDataBase.ReleaseTable(var Table: IBoldTable);
var
  DBISAMTable: TBoldDBISAMTable;
begin
  if Table.Implementor is TBoldDBISAMTable then
  begin
    DBISAMTable := Table.Implementor as TBoldDBISAMTable;
    Table := nil;
    if not assigned(fCachedTable) then
      fCachedTable := DBISAMTable.fTable
    else
      DBISAMTable.fTable.free;
    DBISAMTable.Free;
  end;
end;

function TBoldDBISAMDataBase.SupportsTableCreation: Boolean;
begin
  result := true;
end;

procedure TBoldDBISAMDataBase.ReleaseCachedObjects;
begin
  FreeAndNil(fCachedTable);
  FreeAndNil(fCachedQuery);
end;

function TBoldDBISAMDataBase.SupportsDefaultColumnValues: Boolean;
begin
  result := false;
end;

end.
