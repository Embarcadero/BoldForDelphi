unit BoldDBXInterfaces;

interface

uses
  Classes,
  Db,
  SQlExpr,
  DBXpress,
  BoldSQLDataBaseConfig,
  BoldDBInterfaces;

type
  { forward declarations }
  TBoldDBXDataBase = class;
  TBoldDBXQuery = class;
  TBoldDBXTable = class;
  TBoldDBXQueryClass = class of TBoldDBXQuery;


  { TBoldDBXParameter }
  TBoldDBXParameter = class(TBoldDbParameter)
  protected
    procedure SetAsDateTime(const Value: TDateTime); override;
    function GetAsDateTime: TDateTime; override;
  end;

  { TBoldDBXQuery }
  TBoldDBXQuery = class(TBoldDataSetWrapper, IBoldQuery, IBoldExecQuery, IBoldParameterized)
  private
    fQuery: TSQLQuery;
    procedure AssignParams(SourceParams: TParams);
    function GetParamCount: integer;
    function GetParams(i: integer): IBoldParameter;
    function GetRequestLiveQuery: Boolean;
    function ParamByName(const Value: string): IBoldParameter;
    procedure SetRequestLiveQuery(NewValue: Boolean);
    function GetSQLText: String;
    procedure ClearParams;
    procedure AssignSQL(SQL: TStrings);
    procedure AssignSQLText(SQL: String);
    function GetRowsAffected: integer;
    function GetRecordCount: integer;
    function Createparam(FldType: TFieldType; const ParamName: string; ParamType: TParamType; Size: integer): IBoldParameter;
  protected
    function GetDataSet: TDataSet; override;
    procedure StartSQLBatch; virtual;
    procedure EndSQLBatch; virtual;
    procedure FailSQLBatch; virtual;
    procedure ExecSQL; virtual;
    property Query: TSQLQuery read fQuery;
    procedure Open; override;
  public
    constructor Create(Query: TSQLQuery; DatabaseWrapper: TBoldDatabaseWrapper); virtual;
  end;

  { TBoldDBXTable }
  TBoldDBXTable = class(TBoldDataSetWrapper, IBoldTable)
  private
    fTable: TSQLTable;
    procedure AddIndex(const Name, Fields: string; Options: TIndexOptions; const DescFields: string = '');
    procedure CreateTable;
    procedure DeleteTable;
    function GetTable: TSQLTable;
    function GetIndexDefs: TIndexDefs;
    procedure SetTableName(NewName: String);
    function GetTableName: String;
    procedure SetExclusive(NewValue: Boolean);
    function GetExclusive: Boolean;
    function GetExists: Boolean;
    property Table: TSQLTable read GetTable;
  protected
    function GetDataSet: TDataSet; override;
  public
    constructor Create(Table: TSQLTable; DatabaseWrapper: TBoldDatabaseWrapper);
  end;

  { TBoldDBXDataBase }
  TBoldDBXDataBase = class(TBoldDatabaseWrapper, IBoldDataBase)
  private
    fDataBase: TSQLConnection;
    fCachedTable: TSQLTable;
    fCachedQuery: TSQLQuery;
    fTransactionDesc: TTransactionDesc;
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
    constructor create(DataBase: TSQLConnection; SQLDataBaseConfig: TBoldSQLDataBaseConfig);
    destructor Destroy; override;
  end;

var
  BoldDBXQueryClass: TBoldDBXQueryClass = TBoldDBXQuery;

const
  BOLD_DEFAULT_DBX_TRANSACTION_ID = 1000;

implementation

uses
  SqlTimSt,
  BoldDefs,
  SysUtils,
  BoldUtils,
  BoldCoreConsts;

{ TBoldDBXQuery }

procedure TBoldDBXQuery.AssignParams(Sourceparams: tparams);
begin
  if assigned(Sourceparams) then
    Query.Params.Assign(SourceParams)
  else
    Query.Params.Clear;
end;

function TBoldDBXQuery.GetParamCount: integer;
begin
  result := Query.params.count;
end;

function TBoldDBXQuery.GetParams(I: integer): IBoldParameter;
begin
  result := TBoldDBXParameter.Create(Query.Params[i], self);
end;

function TBoldDBXQuery.GetREquestLiveQuery: Boolean;
begin
  // FIXME result := Query.RequestLive;
  result := false;
end;

function TBoldDBXQuery.ParamByName(const Value: string): IBoldParameter;
var
  Param: TParam;
begin
  Param := Query.Params.FindParam(Value);
  if assigned(Param) then
    result := TBoldDBXParameter.Create(Param, self)
  else
    result := nil;
end;

procedure TBoldDBXQuery.SetRequestLiveQuery(NewValue: Boolean);
begin
  // FIXME
//  Query.RequestLive := NewValue;
end;

function TBoldDBXQuery.GetDataSet: TDataSet;
begin
  result := Query;
end;

procedure TBoldDBXQuery.ExecSQL;
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
  end
end;

constructor TBoldDBXQuery.Create(Query: TSQLQuery; DatabaseWrapper: TBoldDatabaseWrapper);
begin
  inherited Create(DatabaseWrapper);
  fQuery := Query;
end;

procedure TBoldDBXQuery.EndSQLBatch;
begin
  // intentionally left blank
end;

procedure TBoldDBXQuery.StartSQLBatch;
begin
  // intentionally left blank
end;

procedure TBoldDBXQuery.FailSQLBatch;
begin
  // intentionally left blank
end;

procedure TBoldDBXQuery.Open;
begin
  BoldLogSQL(Query.SQL);
  try
    inherited;
  except
    on e: Exception do
    begin
      e.Message := e.Message + BOLDCRLF + 'SQL: ' + Query.SQL.text; // do not localize
      raise;
    end;
  end
end;

procedure TBoldDBXQuery.AssignSQL(SQL: TStrings);
begin
  Query.SQL.BeginUpdate;
  Query.SQL.Assign(SQL);
  Query.SQL.EndUpdate;
end;

procedure TBoldDBXQuery.AssignSQLText(SQL: String);
begin
  Query.SQL.BeginUpdate;
  Query.SQL.Clear;
  BoldAppendToStrings(Query.SQL, SQL, true);
  Query.SQL.EndUpdate;
end;

function TBoldDBXQuery.GetSQLText: String;
begin
  result := Query.SQL.text;
end;

function TBoldDBXQuery.GetRowsAffected: integer;
begin
  result := Query.RowsAffected;
end;

function TBoldDBXQuery.GetRecordCount: integer;
begin
  result := Query.RecordCount;
end;

procedure TBoldDBXQuery.ClearParams;
begin
  query.params.Clear;
end;

function TBoldDBXQuery.Createparam(FldType: TFieldType;
  const ParamName: string; ParamType: TParamType;
  Size: integer): IBoldParameter;
begin
  result := TBoldDbParameter.Create(Query.params.CreateParam(fldType, ParamName, ParamType), self);
end;

{ TBoldDBXTable }

procedure TBoldDBXTable.AddIndex(const Name, Fields: string;
  Options: TIndexOptions; const DescFields: string);
begin
  // FIXME
//  Table.AddIndex(Name, Fields, Options, DescFields);
end;

constructor TBoldDBXTable.Create(Table: TSQLTable; DatabaseWrapper: TBoldDatabaseWrapper);
begin
  inherited Create(DatabaseWrapper);
  fTable := Table;
end;

procedure TBoldDBXTable.CreateTable;
begin
  raise EBold.CreateFmt(sMethodNotImplemented, [classname, 'CreateTable']); // do not localize
end;

procedure TBoldDBXTable.DeleteTable;
begin
// FIXME  Table.DeleteTable;
end;

function TBoldDBXTable.GetDataSet: TDataSet;
begin
  result := Table;
end;

function TBoldDBXTable.GetExclusive: Boolean;
begin
// FIXME  result := Table.Exclusive;
  result := false;
end;

function TBoldDBXTable.GetExists: Boolean;
begin
// FIXME  result := Table.Exists;
  result := false;
end;

function TBoldDBXTable.GetIndexDefs: TIndexDefs;
begin
  result := Table.IndexDefs;
end;

function TBoldDBXTable.GetTable: TSQLTable;
begin
  if not assigned(fTable) then
    fTable := TSQLTable.Create(nil);
  result := fTable
end;

function TBoldDBXTable.GetTableName: String;
begin
  result := Table.TableName;
end;

procedure TBoldDBXTable.SetExclusive(NewValue: Boolean);
begin
// FIXME  Table.Exclusive := NewValue;
end;

procedure TBoldDBXTable.SetTableName(NewName: String);
begin
  Table.TableName := NewName;
end;

{ TBoldDBXDataBase }

procedure TBoldDBXDataBase.AllTableNames(Pattern: String; ShowSystemTables: Boolean; TableNameList: TStrings);
begin
  fDataBase.GetTableNames(TableNameList, ShowSystemTables);
end;

function TBoldDBXDataBase.GetInTransaction: Boolean;
begin
  result := fDataBase.InTransaction;
end;

function TBoldDBXDataBase.GetIsSQLBased: Boolean;
begin
  result := true;
end;

function TBoldDBXDataBase.GetKeepConnection: Boolean;
begin
  result := fDataBase.KeepConnection;
end;

function TBoldDBXDataBase.GetLogInPrompt: Boolean;
begin
  result := fDataBase.LoginPrompt;
end;

procedure TBoldDBXDataBase.SetKeepConnection(NewValue: Boolean);
begin
  fDataBase.KeepConnection := NewValue;
end;

procedure TBoldDBXDataBase.SetlogInPrompt(NewValue: Boolean);
begin
  fDataBase.LoginPrompt := NewValue;
end;

constructor TBoldDBXDataBase.create(DataBase: TSQLConnection; SQLDataBaseConfig: TBoldSQLDataBaseConfig);
begin
  inherited Create(SQLDataBaseConfig);
  fDataBase := DataBase;
end;

function TBoldDBXDataBase.GetConnected: Boolean;
begin
  result := fDataBase.Connected;
end;

procedure TBoldDBXDataBase.StartTransaction;
begin
//  fTransactionDesc.IsolationLevel := xilREADCOMMITTED;
  fTransactionDesc.TransactionID := BOLD_DEFAULT_DBX_TRANSACTION_ID;
  fDataBase.StartTransaction(fTransactionDesc);
end;

procedure TBoldDBXDataBase.Commit;
begin
  fDatabase.Commit(fTransactionDesc);
end;

procedure TBoldDBXDataBase.RollBack;
begin
  fDataBase.Rollback(fTransactionDesc);
end;

procedure TBoldDBXDataBase.Open;
begin
  fDataBase.Open;
end;

procedure TBoldDBXDataBase.Close;
begin
  fDataBase.Close;
end;

destructor TBoldDBXDataBase.destroy;
begin
  inherited;
  fDatabase := nil;
  FreeAndNil(fCachedTable);
  FreeAndNil(fCachedQuery);
end;

function TBoldDBXDataBase.GetQuery: IBoldQuery;
var
  Query: TSQLQuery;
begin
  if assigned(fCachedQuery) then
  begin
    Query := fCachedQuery;
    fCachedQuery := nil;
  end
  else
  begin
    Query := TSQLQuery.Create(nil);
    Query.SQLConnection := fDataBase;
  end;
  result := BoldDBXQueryClass.Create(Query, self);
end;

function TBoldDBXDataBase.GetTable: IBoldTable;
var
  Table: TSQLTable;
begin
  if assigned(fCachedTable) then
  begin
    Table := fCachedTable;
    fCachedTable := nil;
  end
  else
  begin
    Table := TSQLTable.Create(nil);
    Table.SQLConnection := fDataBase;
  end;
  result := TBoldDBXTable.Create(Table, self);
end;

procedure TBoldDBXDataBase.ReleaseQuery(var Query: IBoldQuery);
var
  DBXQuery: TBoldDBXQuery;
begin
  if Query.Implementor is TBoldDBXQuery then
  begin
    DBXQuery := Query.Implementor as TBoldDBXQuery;
    Query := nil;
    if not assigned(fCachedQuery) then
    begin
      fCachedQuery := DBXQuery.fQuery;
      if fCachedQuery.Active then
        fCachedQuery.Close;
      fCachedQuery.SQL.Clear;
    end
    else
      DBXQuery.fQuery.free;
    DBXQuery.Free;
  end;
end;

procedure TBoldDBXDataBase.ReleaseTable(var Table: IBoldTable);
var
  DBXTable: TBoldDBXTable;
begin
  if Table.Implementor is TBoldDBXTable then
  begin
    DBXTable := Table.Implementor as TBoldDBXTable;
    Table := nil;
    if not assigned(fCachedTable) then
      fCachedTable := DBXTable.fTable
    else
      DBXTable.fTable.free;
    DBXTable.Free;
  end;
end;

function TBoldDBXDataBase.SupportsTableCreation: Boolean;
begin
  result := false;
end;

procedure TBoldDBXDataBase.ReleaseCachedObjects;
begin
  FreeAndNil(fCachedTable);
  FreeAndNil(fCachedQuery);
end;

{ TBoldDBXParameter }

function TBoldDBXParameter.GetAsDateTime: TDateTime;
begin
  // dbexpress does not handle AsDateTime, only AsSQLTimeStamp
  result := SQLTimeStampToDateTime(Parameter.AsSQLTimeStamp);
end;

procedure TBoldDBXParameter.SetAsDateTime(const Value: TDateTime);
begin
  // dbexpress does not handle AsDateTime, only AsSQLTimeStamp
//  Parameter.AsDateTime := value;
  Parameter.AsSQLTimeStamp := DateTimetoSQLTimeStamp(value);
end;

end.

