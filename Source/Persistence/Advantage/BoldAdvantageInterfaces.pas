
{ Global compiler directives }
{$include bold.inc}
unit BoldAdvantageInterfaces;

interface
uses
  Classes,
  Db,
  adscnnct,
  adsdata,
  adsfunc,
  adstable,
  BoldSQLDatabaseConfig,
  BoldDBInterfaces;
  
type
  { forward declarations }
  TBoldAdvantageDatabase = class;
  TBoldAdvantageQuery = class;
  TBoldAdvantageQueryClass = class of TBoldAdvantageQuery;
  TBoldAdvantageTable = class;

  { TBoldSDQuery }
  TBoldAdvantageQuery = class(TBoldDataSetWrapper, IBoldQuery, IBoldExecQuery, IBoldParameterized)
  private
    FQuery: TADSQuery;
    function GetParamCount: integer;
    function GetParams(i: integer): IBoldParameter;
    function GetQuery: TADSQuery;
    function GetRecordCount: integer;
    function GetRequestLiveQuery: Boolean;
    function GetRowsAffected: integer;
    function GetSQLText: String;
    function ParamByName(const Value: string): IBoldParameter;
    procedure AssignParams(Sourceparams: TParams);
    procedure ClearParams;
    procedure AssignSQL(SQL: TStrings);
    procedure AssignSQLText(const SQL: String);
    function GetParamCheck: Boolean;
    procedure SetParamCheck(value: Boolean);    
    procedure SetRequestLiveQuery(NewValue: Boolean);
    function Createparam(FldType: TFieldType; const ParamName: string; ParamType: TParamType; Size: integer): IBoldParameter;
  protected
    function GetDataSet: TDataSet; override;
    procedure EndSQLBatch; virtual;
    procedure ExecSQL;
    procedure FailSQLBatch; virtual;
    procedure Open; override;
    procedure StartSQLBatch; virtual;
    property Query: TADSQuery read GetQuery;
  public
    constructor Create(Query: TADSQuery; DatabaseWrapper: TBoldDatabaseWrapper);
  end;

  { TBoldAdvantageTable }
  TBoldAdvantageTable = class(TBoldDataSetWrapper, IBoldTable)
  private
    fTable: TADSTable;
    procedure AddIndex(const Name, Fields: string; Options: TIndexOptions; const DescFields: string = '');
    procedure CreateTable;
    procedure DeleteTable;
    function GetTable: TADSTable;
    function GetIndexDefs: TIndexDefs;
    procedure SetTableName(const NewName: String);
    function GetTableName: String;
    procedure SetExclusive(NewValue: Boolean);
    function GetExclusive: Boolean;
    function GetExists: Boolean;
    property Table: TADSTable read GetTable;
  protected
    function GetDataSet: TDataSet; override;
  public
    constructor Create(Table: TADSTable; DatabaseWrapper: TBoldDatabaseWrapper);
  end;


  { TBoldSDDataBase }
  TBoldAdvantageDatabase = class(TBolddatabaseWrapper, IBoldDataBase)
  private
    FDatabase: TADSConnection;
    FCachedQuery: TADSQuery;
    fCachedTable: TADSTable;
    function GetConnected: Boolean;
    function GetDataBase: TADSConnection;
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
    property Database: TADSConnection read GetDatabase;
    procedure ReleaseCachedObjects;
  protected
    procedure AllTableNames(Pattern: String; ShowSystemTables: Boolean; TableNameList: TStrings); override;
    function GetQuery: IBoldQuery; override;
    procedure ReleaseQuery(var Query: IBoldQuery); override;
    function GetTable: IBoldTable; override;
    procedure ReleaseTable(var Table: IBoldTable); override;    
  public
    constructor create(Database: TADSConnection; SQLDataBaseConfig: TBoldSQLDatabaseConfig);
    destructor destroy; override;
  end;

var
  BoldAdvantageQueryClass: TBoldAdvantageQueryClass = TBoldAdvantageQuery;

implementation

uses
  Dialogs,
  SysUtils,
  BoldUtils;

{ TBoldSDQuery }

{
procedure TBoldAdvantageQuery.AdaptSQL(SQL: TStringList);
var
  s: string;
begin
  s := StringReplace(sql.text, 'NOT NULL', '', [rfReplaceAll, rfIgnoreCase]);
  s := StringReplace(s, 'SMALLINT', 'INTEGER', rfReplaceAll, rfIgnoreCase]);
  sql.Text := s;
end;
}

procedure TBoldAdvantageQuery.AssignParams(Sourceparams: tparams);
begin
  if assigned(Sourceparams) then
    Query.Params.Assign(SourceParams)
  else
    Query.Params.Clear;
end;

procedure TBoldAdvantageQuery.AssignSQL(SQL: TStrings);
begin
  Query.SQL.BeginUpdate;
  Query.SQL.Assign(SQL);
  Query.SQL.EndUpdate;
end;

procedure TBoldAdvantageQuery.AssignSQLText(const SQL: String);
begin
  Query.SQL.BeginUpdate;
  Query.SQL.Clear;
  BoldAppendToStrings(Query.SQL, SQL, true);
  Query.SQL.EndUpdate;
end;

procedure TBoldAdvantageQuery.ClearParams;
begin
  Query.Params.Clear;
end;

constructor TBoldAdvantageQuery.Create(Query: TADSQuery; DatabaseWrapper: TBoldDatabaseWrapper);
begin
  inherited create(DatabaseWrapper);
  FQuery := Query;
  SetParamCheck(true);  
end;

function TBoldAdvantageQuery.Createparam(FldType: TFieldType;
  const ParamName: string; ParamType: TParamType;
  Size: integer): IBoldParameter;
begin
  result := TBoldDbParameter.Create(Query.params.CreateParam(fldType, ParamName, ParamType), self);
end;

procedure TBoldAdvantageQuery.EndSQLBatch;
begin
end;

procedure TBoldAdvantageQuery.ExecSQL;
begin
  try
    BoldLogSQL(Query.SQL);
    Query.ExecSQL;
  except
    on E: Exception do
      MessageDlg(E.Message + #13#10 +Query.SQL.Text, mtError, [mbOk], 0);
    end;
  end;

procedure TBoldAdvantageQuery.FailSQLBatch;
begin
end;

function TBoldAdvantageQuery.GetDataSet: TDataSet;
begin
  result := Query;
end;

function TBoldAdvantageQuery.GetParamCheck: Boolean;
begin
  Result := Query.ParamCheck;
end;

function TBoldAdvantageQuery.GetParamCount: integer;
begin
  result := Query.Params.count;
end;

function TBoldAdvantageQuery.GetParams(i: integer): IBoldParameter;
begin
  result := TBoldDBParameter.Create(Query.Params[i], self);
end;

function TBoldAdvantageQuery.GetQuery: TADSQuery;
begin
  result := FQuery;
end;

function TBoldAdvantageQuery.GetRecordCount: integer;
begin
  Result := Query.RecordCount;
end;

function TBoldAdvantageQuery.GetRequestLiveQuery: Boolean;
begin
  result := false;
end;

function TBoldAdvantageQuery.GetRowsAffected: integer;
begin
  result := Query.RowsAffected;
end;

function TBoldAdvantageQuery.GetSQLText: String;
begin
  result := Query.SQL.Text;
end;

procedure TBoldAdvantageQuery.Open;
begin
  BoldLogSQL(Query.SQL);
  inherited;
end;

function TBoldAdvantageQuery.ParamByName(const Value: string): IBoldParameter;
var
  Param: TParam;
begin
  Param := Query.ParamByName(Value);
  if assigned(Param) then
    result := TBoldDbParameter.Create(Param, self)
  else
    result := nil;
end;


procedure TBoldAdvantageQuery.SetParamCheck(value: Boolean);
begin
  Query.ParamCheck := Value;
end;

procedure TBoldAdvantageQuery.SetRequestLiveQuery(NewValue: Boolean);
begin
end;

procedure TBoldAdvantageQuery.StartSQLBatch;
begin
end;

{ TBoldAdvantageTable }

procedure TBoldAdvantageTable.AddIndex(const Name, Fields: string;
  Options: TIndexOptions; const DescFields: string);
begin
  Table.AddIndex(Name, Fields, Options);
end;

constructor TBoldAdvantageTable.Create(Table: TADSTable; DatabaseWrapper: TBoldDatabaseWrapper);
begin
  inherited Create(DatabaseWrapper);
  fTable := Table;
end;

procedure TBoldAdvantageTable.CreateTable;
begin
  Table.CreateTable;
end;

procedure TBoldAdvantageTable.DeleteTable;
begin
  Table.DeleteTable;
end;

function TBoldAdvantageTable.GetDataSet: TDataSet;
begin
  result := Table;
end;

function TBoldAdvantageTable.GetExclusive: Boolean;
begin
  result := Table.Exclusive;
end;

function TBoldAdvantageTable.GetExists: Boolean;
begin
  result := Table.Exists;
end;

function TBoldAdvantageTable.GetIndexDefs: TIndexDefs;
begin
  result := Table.IndexDefs;
end;

function TBoldAdvantageTable.GetTable: TADSTable;
begin
  if not assigned(fTable) then
    fTable := TADSTable.Create(nil);
  result := fTable
end;

function TBoldAdvantageTable.GetTableName: String;
begin
  result := Table.TableName;
end;

procedure TBoldAdvantageTable.SetExclusive(NewValue: Boolean);
begin
  Table.Exclusive := NewValue;
end;

procedure TBoldAdvantageTable.SetTableName(const NewName: String);
begin
  Table.TableName := NewName;
end;

{ TBoldSDDataBase }

procedure TBoldAdvantageDatabase.AllTableNames(Pattern: String; ShowSystemTables: Boolean; TableNameList: TStrings);
begin

  Database.GetTableNames(TableNameList, Pattern {ShowSystemTables});
end;

procedure TBoldAdvantageDatabase.Close;
begin
  Database.Disconnect;
end;

procedure TBoldAdvantageDatabase.Commit;
begin
  Database.Commit;
end;

constructor TBoldAdvantageDatabase.create(DataBase: TADSConnection; SQLDataBaseConfig: TBoldSQLDatabaseConfig);
begin
  inherited create(SQLDataBaseConfig);
  FDataBase := DataBase;
end;

destructor TBoldAdvantageDatabase.destroy;
begin
  inherited;
  FDatabase := nil;
  FreeAndNil(fCachedQuery);
  FreeAndNil(fCachedTable);
end;

function TBoldAdvantageDatabase.GetConnected: Boolean;
begin
  result := Database.IsConnected;
end;

function TBoldAdvantageDatabase.GetDataBase: TADSConnection;
begin
  result := FDataBase;
end;

function TBoldAdvantageDatabase.GetInTransaction: Boolean;
begin
  result := Database.TransactionActive;
end;

function TBoldAdvantageDatabase.GetIsSQLBased: Boolean;
begin
  result := true;
end;

function TBoldAdvantageDatabase.GetKeepConnection: Boolean;
begin
  result := true;
end;

function TBoldAdvantageDatabase.GetLogInPrompt: Boolean;
begin
  result := dataBase.LoginPrompt;
end;

function TBoldAdvantageDatabase.GetQuery: IBoldQuery;
var
  Query: TADSQuery;
begin
  if assigned(fCachedQuery) then
  begin
    Query := fCachedQuery;
    fCachedQuery := nil;
  end
  else
  begin
    Query := TADSQuery.Create(nil);
    Query.DatabaseName := fDatabase.Name;
  end;
  result := BoldAdvantageQueryClass.Create(Query, self);
end;

function TBoldAdvantageDatabase.GetTable: IBoldTable;
var
  Table: TAdsTable;
begin
  if assigned(fCachedTable) then
  begin
    Table := fCachedTable;
    fCachedTable := nil;
  end
  else
  begin
    Table := TADSTable.Create(nil);
    Table.AdsConnection := fDataBase;
  end;
  result := TBoldAdvantageTable.Create(Table, self);
end;

procedure TBoldAdvantageDatabase.Open;
begin
  Database.Connect;
end;

procedure TBoldAdvantageDatabase.ReleaseCachedObjects;
begin
  FreeAndNil(fCachedTable);
  FreeAndNil(fCachedQuery);
end;

procedure TBoldAdvantageDatabase.ReleaseQuery(var Query: IBoldQuery);
var
  SDQuery: TBoldAdvantageQuery;
begin
  if Query.Implementor is TBoldAdvantageQuery then begin
    SDQuery := Query.Implementor as TBoldAdvantageQuery;
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

procedure TBoldAdvantageDatabase.ReleaseTable(var Table: IBoldTable);
var
  AdvantageTable: TBoldAdvantageTable;
begin
  if Table.Implementor is TBoldAdvantageTable then
  begin
    AdvantageTable := Table.Implementor as TBoldAdvantageTable;
    Table := nil;
    if not assigned(fCachedTable) then
      fCachedTable := AdvantageTable.fTable
    else
      AdvantageTable.fTable.free;
    AdvantageTable.Free;
  end;
end;

procedure TBoldAdvantageDatabase.Rollback;
begin
  Database.Rollback;
end;

procedure TBoldAdvantageDatabase.SetKeepConnection(NewValue: Boolean);
begin
end;

procedure TBoldAdvantageDatabase.SetlogInPrompt(NewValue: Boolean);
begin
  Database.LoginPrompt := NewValue;
end;

procedure TBoldAdvantageDatabase.StartTransaction;
begin
  Database.BeginTransaction;
end;

function TBoldAdvantageDatabase.SupportsTableCreation: boolean;
begin
  result := true;
end;

end.
