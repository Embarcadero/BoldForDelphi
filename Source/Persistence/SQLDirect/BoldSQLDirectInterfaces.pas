unit BoldSQLDirectInterfaces;

interface
uses
  Classes,
  Db,
  SDEngine,
  BoldSQLDatabaseConfig,
  BoldDBInterfaces;

type
  { forward declarations }
  TBoldSQLDirectDatabase = class;
  TBoldSQLDirectQuery = class;
  TBoldSQLDirectQueryClass = class of TBoldSQLDirectQuery;

  { TBoldSDQuery }
  TBoldSQLDirectQuery = class(TBoldDataSetWrapper, IBoldQuery, IBoldExecQuery, IBoldParameterized)
  private
    FQuery: TSDQuery;
    function GetParamCount: integer;
    function GetParams(i: integer): IBoldParameter;
    function GetQuery: TSDQuery;
    function GetRecordCount: integer;
    function GetRequestLiveQuery: Boolean;
    procedure ClearParams;
    function GetRowsAffected: integer;
    function GetSQLText: String;
    function ParamByName(const Value: string): IBoldParameter;
    procedure AssignParams(Sourceparams: TParams);
    procedure AssignSQL(SQL: TStrings);
    procedure AssignSQLText(SQL: String);
    procedure SetRequestLiveQuery(NewValue: Boolean);
    function Createparam(FldType: TFieldType; const ParamName: string; ParamType: TParamType; Size: integer): IBoldParameter;
  protected
    function GetDataSet: TDataSet; override;
    procedure EndSQLBatch; virtual;
    procedure ExecSQL;
    procedure FailSQLBatch; virtual;
    procedure Open; override;
    procedure StartSQLBatch; virtual;
    property Query: TSDQuery read GetQuery;
  public
    constructor Create(Query: TSDQuery; DatabaseWrapper: TBoldDatabaseWrapper);
  end;

  { TBoldSDDataBase }
  TBoldSQLDirectDatabase = class(TBolddatabaseWrapper, IBoldDataBase)
  private
    FDatabase: TSDDataBase;
    FCachedQuery: TSDQuery;
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
    function GetTable: IBoldTable;
    procedure ReleaseTable(var Table: IBoldTable);
    procedure Rollback;
    procedure SetKeepConnection(NewValue: Boolean);
    procedure SetlogInPrompt(NewValue: Boolean);
    procedure StartTransaction;
    property Database: TSDDatabase read GetDatabase;
    procedure ReleaseCachedObjects;
  protected
    procedure AllTableNames(Pattern: String; ShowSystemTables: Boolean; TableNameList: TStrings); override;
    function GetQuery: IBoldQuery; override;
    procedure ReleaseQuery(var Query: IBoldQuery); override;
  public
    constructor Create(Database: TSDDataBase; SQLDataBaseConfig: TBoldSQLDatabaseConfig);
    destructor Destroy; override;
  end;

var
  BoldSQLDirectQueryClass: TBoldSQLDirectQueryClass = TBoldSQLDirectQuery;

implementation

uses
  SysUtils,
  BoldUtils,
  Dialogs;

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

procedure TBoldSQLDirectQuery.AssignSQLText(SQL: String);
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
end;

function TBoldSQLDirectQuery.Createparam(FldType: TFieldType; const ParamName: string; ParamType: TParamType; Size: integer): IBoldParameter;
begin
  result := TBoldDbParameter.Create(Query.params.CreateParam(fldType, ParamName, ParamType), self);
end;

procedure TBoldSQLDirectQuery.EndSQLBatch;
begin
end;

procedure TBoldSQLDirectQuery.ExecSQL;
begin
  try
    BoldLogSQL(Query.SQL);
    Query.ExecSQL;
  except
    on E: Exception do MessageDlg(E.Message + #13#10 +Query.SQL.Text, mtError, [mbOk], 0);
    end;
  end;

procedure TBoldSQLDirectQuery.FailSQLBatch;
begin
end;

function TBoldSQLDirectQuery.GetDataSet: TDataSet;
begin
  result := Query;
end;

function TBoldSQLDirectQuery.GetParamCount: integer;
begin
  result := Query.Params.count;
end;

function TBoldSQLDirectQuery.GetParams(i: integer): IBoldParameter;
begin
  result := TBoldDBParameter.Create(Query.Params[i], self);
end;

function TBoldSQLDirectQuery.GetQuery: TSDQuery;
begin
  result := FQuery;
end;

function TBoldSQLDirectQuery.GetRecordCount: integer;
begin
  Result := Query.RecordCount;
end;

function TBoldSQLDirectQuery.GetRequestLiveQuery: Boolean;
begin
  result := false;
end;

function TBoldSQLDirectQuery.GetRowsAffected: integer;
begin
  result := Query.RowsAffected;
end;

function TBoldSQLDirectQuery.GetSQLText: String;
begin
  result := Query.SQL.Text;
end;

procedure TBoldSQLDirectQuery.Open;
begin
  BoldLogSQL(Query.SQL);
  inherited;
end;

function TBoldSQLDirectQuery.ParamByName(const Value: string): IBoldParameter;
var
  Param: TParam;
begin
  Param := Query.ParamByName(Value);
  if assigned(Param) then
    result := TBoldDbParameter.Create(Param, self)
  else
    result := nil;
end;


procedure TBoldSQLDirectQuery.SetRequestLiveQuery(NewValue: Boolean);
begin
end;

procedure TBoldSQLDirectQuery.StartSQLBatch;
begin
end;

{ TBoldSDDataBase }

procedure TBoldSQLDirectDatabase.AllTableNames(Pattern: String; ShowSystemTables: Boolean; TableNameList: TStrings);
begin
  if (Pattern <> '') and (Pattern <> '*') then
    raise Exception.CreateFmt('%s.AlltableNames: This call does not allow patterns ("%s")', [ClassName, Pattern]);
  Database.Session.GetTableNames(Database.DatabaseName, Pattern, ShowSystemTables, TableNameList);
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
  inherited create(SQLDataBaseConfig);
  FDataBase := DataBase;
end;

destructor TBoldSQLDirectDatabase.destroy;
begin
  inherited;
  FDatabase := nil;
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

function TBoldSQLDirectDatabase.GetInTransaction: Boolean;
begin
  result := Database.InTransaction;
end;

function TBoldSQLDirectDatabase.GetIsSQLBased: Boolean;
begin
  result := true;
end;

function TBoldSQLDirectDatabase.GetKeepConnection: Boolean;
begin
  result := true;
end;

function TBoldSQLDirectDatabase.GetLogInPrompt: Boolean;
begin
  result := dataBase.LoginPrompt;
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
    Query.DatabaseName := Database.DatabaseName;
    Query.SessionName := Database.SessionName;
  end;
  result := BoldSQLDirectQueryClass.Create(Query, self);
end;

function TBoldSQLDirectDatabase.GetTable: IBoldTable;
begin
  result := nil;
end;

procedure TBoldSQLDirectDatabase.Open;
begin
  Database.Open;
end;

procedure TBoldSQLDirectDatabase.ReleaseCachedObjects;
begin
  //FreeAndNil(fCachedTable);
  FreeAndNil(fCachedQuery);
  end;

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
    end
    else
      SDQuery.FQuery.free;
    SDQuery.Free;
  end;
end;

procedure TBoldSQLDirectDatabase.ReleaseTable(var Table: IBoldTable);
begin
  raise Exception.Create('TBoldSQLDirectDatabase.ReleaseTable: Operation not supported');
end;

procedure TBoldSQLDirectDatabase.Rollback;
begin
  Database.Rollback;
end;

procedure TBoldSQLDirectDatabase.SetKeepConnection(NewValue: Boolean);
begin
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
  result := false;
end;

end.
