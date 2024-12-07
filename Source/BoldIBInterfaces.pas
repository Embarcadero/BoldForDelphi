
/////////////////////////////////////////////////////////
//                                                     //
//              Bold for Delphi                        //
//    Copyright (c) 2002 BoldSoft AB, Sweden           //
//                                                     //
/////////////////////////////////////////////////////////

{ Global compiler directives }
{$include bold.inc}
unit BoldIBInterfaces;

interface
uses
  classes,
  Dialogs,
  Db,
  IBX.IB,
  IBX.IBQuery,
  IBX.IBTable,
  IBX.IBDataBase,
  BoldSQLDataBaseConfig,
  BoldBase,
  BoldDefs,
  BoldDBInterfaces,
  BoldContainers;

type
  { forward declarations }
  TBoldIBDataBase = class;
  TBoldIBQuery = class;
  TBoldIBTable = class;

  TBoldIBTransactionMode = (tmUnknown, tmStarted, tmNotStarted); 

  { TBoldIBQuery }
  TBoldIBQuery = class(TBoldBatchDataSetWrapper, IBoldQuery, IBoldExecQuery, IBoldParameterized)
  private
    fQuery: TIBQuery;
    fOpeningTransactionMode: TBoldIBTransactionMode;
    function GetQuery: TIBQuery;
    procedure EnsureTransaction;
    procedure AssignParams(Sourceparams: TParams);
    function GetParamCount: integer;
    function GetParamCheck: Boolean;
    procedure SetParamCheck(value: Boolean);
    function GetParam(i: integer): IBoldParameter;
    function GetParams: TParams;    
    function GetRequestLiveQuery: Boolean;
    function ParamByName(const Value: string): IBoldParameter; override;
    function FindParam(const Value: string): IBoldParameter; override;
    procedure SetRequestLiveQuery(NewValue: Boolean);
    function GetSQLText: String;
    function GetSQLStrings: TStrings;    
    procedure AssignSQL(SQL: TStrings);
    procedure AssignSQLText(const SQL: String);
    function Createparam(FldType: TFieldType; const ParamName: string; ParamType: TParamType; Size: integer): IBoldParameter; override;
    function GetRowsAffected: integer;
    function GetRecordCount: integer;
    function GetUseReadTransactions: boolean;
    procedure SetUseReadTransactions(value: boolean);
    procedure BeginExecuteQuery;
    procedure EndExecuteQuery;
    procedure Prepare;
  protected
    function GetDataSet: TDataSet; override;
    procedure ClearParams;
    procedure Open; override;
    procedure Close; override;
    procedure ExecSQL;
    property Query: TIBQuery read GetQuery;
  public
    constructor Create(Query: TIBQuery; DatabaseWrapper: TBoldDatabaseWrapper);
  end;

  { TBoldIBTable }
  TBoldIBTable = class(TBoldDataSetWrapper, IBoldTable)
  private
    fTable: TIBTable;
    function GetTable: TIBTable;
    property Table: TIBTable read GetTable;
    procedure EnsureTransaction;

    procedure AddIndex(const Name, Fields: string; Options: TIndexOptions; const DescFields: string = '');
    procedure CreateTable;
    procedure DeleteTable;
    function GetIndexDefs: TIndexDefs;
    procedure SetTableName(const NewName: String);
    function GetTableName: String;
    procedure SetExclusive(NewValue: Boolean);
    function GetExclusive: Boolean;
    function GetExists: Boolean;

  protected
    function GetDataSet: TDataSet; override;
    procedure Open; override;
  public
    constructor Create(Table: TIBTable; DatabaseWrapper: TBoldDatabaseWrapper);
  end;

  { TBoldIBDataBase }
  TBoldIBDataBase = class(TBolddatabaseWrapper, IBoldDataBase)
  private
    fDataBase: TIBDataBase;
    fCachedTable: TIBTable;
    fCachedQuery: TIBQuery;
    fExecuteQueryCount: integer;    
    function GetDataBase: TIBDataBase;
    property DataBase: TIBDataBase read GetDataBase;
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
    function SupportsTableCreation: Boolean;
    procedure ReleaseCachedObjects;
    function GetIsExecutingQuery: Boolean;
    procedure BeginExecuteQuery;
    procedure EndExecuteQuery;
  protected
    procedure AllTableNames(Pattern: String; ShowSystemTables: Boolean; TableNameList: TStrings); override;
    function GetQuery: IBoldQuery; override;
    procedure ReleaseQuery(var Query: IBoldQuery); override;
    function GetTable: IBoldTable; override;
    procedure ReleaseTable(var Table: IBoldTable); override;
  public
    constructor Create(DataBase: TIBDataBase; SQLDataBaseConfig: TBoldSQLDatabaseConfig);
    destructor Destroy; override;
    procedure CreateDatabase(DropExisting: boolean = true);
    function CreateAnotherDatabaseConnection: IBoldDatabase;
  end;

implementation

uses
  SysUtils,
  {$IFDEF ATTRACS}
  AttracsDefs,
  AttracsPerformance,
  AttracsErrorMgr,
  {$ENDIF}
  BoldUtils;


{ TBoldIBQuery }

procedure TBoldIBQuery.AssignParams(Sourceparams: tparams);
begin
  if assigned(Sourceparams) then
    Query.Params.Assign(SourceParams)
  else
    Query.Params.Clear;
end;

procedure TBoldIBQuery.AssignSQL(SQL: TStrings);
begin
  Query.SQL.BeginUpdate;
  Query.SQL.Assign(SQL);
  Query.SQL.EndUpdate;
end;

procedure TBoldIBQuery.AssignSQLText(const SQL: String);
begin
  Query.SQL.BeginUpdate;
  Query.SQL.Clear;
  BoldAppendToStrings(Query.SQL, SQL, true);
  Query.SQL.EndUpdate;
end;


procedure TBoldIBQuery.BeginExecuteQuery;
begin
  (DatabaseWrapper as TBoldIBDataBase).EndExecuteQuery;
end;

procedure TBoldIBQuery.EndExecuteQuery;
begin
  (DatabaseWrapper as TBoldIBDataBase).EndExecuteQuery;
end;

constructor TBoldIBQuery.Create(Query: TIBQuery; DatabaseWrapper: TBoldDatabaseWrapper);
begin
  inherited Create(DatabaseWrapper);
  fQuery := Query;
  SetParamCheck(true);
end;

procedure TBoldIBQuery.EnsureTransaction;
var
  NewTransaction: TIBtransaction;
begin
  if not assigned(Query.Transaction) then
  begin
    Newtransaction := Query.Database.FindDefaultTransaction;

    if not assigned(NewTransaction) then
    begin
      NewTransaction := TIBTransaction.Create(Query.DataBase);
      Query.DataBase.AddTransaction(NewTransaction);
      Query.Database.DefaultTransaction := NewTransaction;
      NewTransaction.AddDatabase(Query.DataBase);
    end;
    Query.Transaction := NewTransaction;
  end;
end;

procedure TBoldIBQuery.ExecSQL;
{$IFDEF ATTRACS}
var
  PerformanceMeasurement : TPerformanceMeasurement;
begin
  //PATCH for logging long running SQL
  PerformanceMeasurement := TPerformanceMeasurement.ReStart;
{$ELSE}
begin
{$ENDIF}
  BeginExecuteQuery;
  try
  BoldLogSQL(Query.SQL);
  try
    if Query.Transaction.InTransaction then
      fOpeningTransactionMode := tmStarted
    else
      fOpeningTransactionMode := tmNotStarted;
    Query.ExecSQL;
  except
    on e: Exception do
    begin
      e.Message := e.Message + BOLDCRLF + 'SQL: '+Query.SQL.text;
      raise;
    end;
  end;
{$IFDEF ATTRACS}
  if not PerformanceMeasurement.AcceptableTimeForUserResponseTime then
  begin
    PerformanceMeasurement.WhatMeasured := 'TBoldIBQuery.ExecSQL';
    PerformanceMeasurement.WhatMeasuredParameter := Query.SQL.Text;
    PerformanceMeasurement.Trace;
  end;
{$ENDIF}
  finally
    EndExecuteQuery;
  end;
end;

function TBoldIBQuery.FindParam(const Value: string): IBoldParameter;
var
  Param: TParam;
begin
  result := nil;
  Param := Query.Params.FindParam(Value);
  if Assigned(Param) then
    result := TBoldDbParameter.Create(Param, self)
end;

function TBoldIBQuery.GetDataSet: TDataSet;
begin
  result := Query;
end;

function TBoldIBQuery.GetParam(i: integer): IBoldParameter;
begin
  result := TBoldDBParameter.Create(Query.Params[i], self);
end;

function TBoldIBQuery.GetParamCheck: Boolean;
begin
  Result := Query.ParamCheck;
end;

function TBoldIBQuery.GetParamCount: integer;
begin
  result := Query.Params.count;
end;

type TTIBQueryAccess = class(TIBQuery);

function TBoldIBQuery.GetParams: TParams;
begin
  result := TTIBQueryAccess(Query).PSGetParams
end;

function TBoldIBQuery.GetQuery: TIBQuery;
begin
  result := fQuery;
end;

function TBoldIBQuery.GetRecordCount: integer;
begin
  Result := Query.RecordCount;
end;

function TBoldIBQuery.GetRequestLiveQuery: Boolean;
begin
  result := false;
end;

function TBoldIBQuery.GetRowsAffected: integer;
begin
  result := Query.RowsAffected;
end;

function TBoldIBQuery.GetSQLStrings: TStrings;
begin
  result := Query.SQL;
end;

function TBoldIBQuery.GetSQLText: String;
begin
  result := Query.SQL.Text;
end;

function TBoldIBQuery.GetUseReadTransactions: boolean;
begin
  result := false;
end;

procedure TBoldIBQuery.Open;
begin
  BeginExecuteQuery;
  try
  EnsureTransaction;
  BoldLogSQL(Query.SQL);
  try
    if Query.Transaction.InTransaction then
      fOpeningTransactionMode := tmStarted
    else
      fOpeningTransactionMode := tmNotStarted;
    inherited;
  except
    on e: Exception do
    begin
      e.Message := e.Message + BOLDCRLF + 'SQL: '+Query.SQL.text;
      raise;
    end;
  end
  finally
    EndExecuteQuery;
  end;
end;

procedure TBoldIBQuery.Close;
begin
  inherited;
  if (fOpeningTransactionMode = tmNotStarted) and Query.Transaction.InTransaction then
    Query.Transaction.Commit;
  fOpeningTransactionMode := tmUnknown;
end;


function TBoldIBQuery.ParamByName(const Value: string): IBoldParameter;
var
  Param: TParam;
begin
  Param := Query.ParamByName(Value);
  if assigned(Param) then
    result := TBoldDbParameter.Create(Param, self)
  else
    result := nil;
end;


procedure TBoldIBQuery.Prepare;
begin
  Query.prepare;
end;

{function TBoldIBQuery.Params: TParams;
begin
  result := Query.Params;
end;
}

procedure TBoldIBQuery.SetParamCheck(value: Boolean);
begin
  Query.ParamCheck := Value;
end;

procedure TBoldIBQuery.SetRequestLiveQuery(NewValue: Boolean);
begin
end;


procedure TBoldIBQuery.SetUseReadTransactions(value: boolean);
begin

end;

function TBoldIBQuery.Createparam(FldType: TFieldType; const ParamName: string; ParamType: TParamType; Size: integer): IBoldParameter;
begin
  result := TBoldDbParameter.Create(Query.params.CreateParam(fldType, ParamName, ParamType), self);
end;

procedure TBoldIBQuery.ClearParams;
begin
  Query.Params.Clear;
end;

{ TBoldIBTable }

procedure TBoldIBTable.AddIndex(const Name, Fields: string;
  Options: TIndexOptions; const DescFields: string);
begin
  Table.AddIndex(Name, fields, Options, descFields);
end;

constructor TBoldIBTable.Create(Table: TIBTable; DatabaseWrapper: TBoldDatabaseWrapper);
begin
  inherited Create(DatabaseWrapper);
  fTable := Table;
end;

procedure TBoldIBTable.CreateTable;
var
  OurTransaction: Boolean;
begin
  EnsureTransaction;
  OurTransaction := false;
  if not Table.Transaction.InTransaction then
  begin
    Table.Transaction.StartTransaction;
    OurTransaction := true;
  end;
  Table.CreateTable;
  if OurTransaction then
    Table.Transaction.Commit;
end;

procedure TBoldIBTable.DeleteTable;
var
  Ourtransaction: Boolean;
begin
  EnsureTransaction;
  OurTransaction := false;
  if not Table.Transaction.InTransaction then
  begin
    Table.Transaction.StartTransaction;
    OurTransaction := true;
  end;
  table.DeleteTable;
  if OurTransaction then
    Table.Transaction.Commit;
end;

procedure TBoldIBTable.EnsureTransaction;
var
  NewTransaction: TIBtransaction;
begin
  if not assigned(Table.Transaction) then
  begin
    Newtransaction := Table.Database.FindDefaultTransaction;

    if not assigned(NewTransaction) then
    begin
      NewTransaction := TIBTransaction.Create(table.DataBase);
      Table.DataBase.AddTransaction(NewTransaction);
      Table.Database.DefaultTransaction := NewTransaction;
      NewTransaction.AddDatabase(Table.DataBase);
    end;
    Table.Transaction := NewTransaction;
  end;
end;

function TBoldIBTable.GetDataSet: TDataSet;
begin
  result := Table;
end;


function TBoldIBTable.GetExclusive: Boolean;
begin
  result := false;
end;

function TBoldIBTable.GetExists: Boolean;
begin
  result := Table.Exists;
end;

function TBoldIBTable.GetIndexDefs: TIndexDefs;
begin
  result := Table.IndexDefs;
end;


function TBoldIBTable.GetTable: TIBTable;
begin
  result := fTable;
end;

function TBoldIBTable.GetTableName: String;
begin
  result := Table.TableName;
end;

procedure TBoldIBTable.Open;
begin
  EnsureTransaction;
  inherited;
end;

procedure TBoldIBTable.SetExclusive(NewValue: Boolean);
begin
end;


procedure TBoldIBTable.SetTableName(const NewName: String);
begin
  Table.TableName := NewName;
end;

{ TBoldIBDataBase }

procedure TBoldIBDataBase.AllTableNames(Pattern: String; ShowSystemTables: Boolean; TableNameList: TStrings);
begin
  if (Pattern <> '') and (Pattern <> '*') then
    raise Exception.CreateFmt('%s.AlltableNames: This call does not allow patterns ("%s")', [ClassName, Pattern]);
  DataBase.GetTableNames(TableNameList, ShowSystemTables);
end;

procedure TBoldIBDataBase.BeginExecuteQuery;
begin
  inc(fExecuteQueryCount);
end;

procedure TBoldIBDataBase.Close;
begin
  DataBase.Close;
end;

procedure TBoldIBDataBase.Commit;
var
  transaction: TIBTransaction;
begin
  transaction := DataBase.FindDefaultTransaction;
  if assigned(Transaction) then
    transaction.Commit
  else
    raise Exception.CreateFmt('%s.Commit: No default transaction', [ClassName]);
end;

constructor TBoldIBDataBase.create(DataBase: TIBDataBase; SQLDataBaseConfig: TBoldSQLDatabaseConfig);
begin
  inherited create(SQLDataBaseConfig);
  fDataBase := DataBase;
end;

function TBoldIBDataBase.CreateAnotherDatabaseConnection: IBoldDatabase;
begin
  raise Exception.CreateFmt('%s.CreateAnotherDatabaseConnection: Not implemented', [ClassName]);
end;

destructor TBoldIBDataBase.destroy;
begin
  fDatabase := nil;
  FreeAndNil(fCachedTable);
  FreeAndNil(fCachedQuery);
  inherited;
end;

procedure TBoldIBDataBase.EndExecuteQuery;
begin
  dec(fExecuteQueryCount);
end;

procedure TBoldIBDataBase.CreateDatabase(DropExisting: boolean = true);
var
  db: TIBDatabase;
  username: String;
  pwd: String;
const
  cPageSize = 4096;
begin
  if not assigned(Database) then
    raise EBold.CreateFmt('%s.CreateInterbaseDatbase: Unable to complete operation without an IBDatabase', [classname]);

  username := Database.Params.Values['user_name'];
  pwd := Database.Params.Values['password'];

  if (username = '') or (pwd = '') then
    raise EBold.CreateFmt('%s.CreateInterbaseDatabase: username or password missing', [classname]);
  if FileExists(Database.DatabaseName) then
    if not DeleteFile(Database.DatabaseName) then
      raise EBold.CreateFmt('%s.CreateInterbaseDatbase: Unable to remove old database file (%s)', [classname, DataBase.DatabaseName]);
  db := TIBDatabase.Create(nil);
  try
    db.DatabaseName := Database.DatabaseName;
    db.Params.add(format('USER "%s" PASSWORD "%s" PAGE_SIZE %d', [
       username, pwd, cPageSize]));
    db.SQLDialect := Database.SQLDialect;
    db.CreateDatabase;
  finally
    db.free;
  end;
end;

function TBoldIBDataBase.GetConnected: Boolean;
begin
  result := DataBase.Connected;
end;

function TBoldIBDataBase.GetDataBase: TIBDataBase;
begin
  result := fDataBase;
end;

function TBoldIBDataBase.GetInTransaction: Boolean;
var
  Transaction: TIBTransaction;
begin
  Transaction := DataBase.FindDefaultTransaction;
  result := assigned(Transaction) and Transaction.InTransaction;
end;

function TBoldIBDataBase.GetIsExecutingQuery: Boolean;
begin
  Result := fExecuteQueryCount > 0;
end;

function TBoldIBDataBase.GetIsSQLBased: Boolean;
begin
  result := true;
end;

function TBoldIBDataBase.GetKeepConnection: Boolean;
begin
  result := true
end;

function TBoldIBDataBase.GetLogInPrompt: Boolean;
begin
  result := dataBase.LoginPrompt;
end;

function TBoldIBDataBase.GetQuery: IBoldQuery;
var
  Query: TIBQuery;
begin
  if assigned(fCachedQuery) then
  begin
    Query := fCachedQuery;
    fCachedQuery := nil;
  end
  else
  begin
    Query := TIBQuery.Create(nil);
    Query.UniDirectional := True;
    Query.DataBase := DataBase;
  end;
  if not assigned(Query.Transaction) then
    Query.Transaction := fDataBase.DefaultTransaction;
  result := TBoldIBQuery.Create(Query, self);
end;

function TBoldIBDataBase.GetTable: IBoldTable;
var
  Table: TIBTable;
begin
  if assigned(fCachedTable) then
  begin
    Table := fCachedTable;
    fCachedTable := nil;
  end
  else
  begin
    Table := TIBTable.Create(nil);
    Table.DataBase := DataBase;
  end;
  result := TBoldIBTable.Create(Table, self);
end;

procedure TBoldIBDataBase.Open;
var
  NewTransaction: TIBTransaction;
begin
  if not assigned(DataBAse.DefaultTransaction) then
  begin
    NewTransaction := TIBTransaction.Create(DataBase);
    Database.DefaultTransaction := NewTransaction;
  end;
  DataBase.Open;
end;

procedure TBoldIBDataBase.Reconnect;
begin
  if Assigned(fDataBase) then begin
    fDataBase.Connected := False;
    fDataBase.Connected := True;
  end;
end;

procedure TBoldIBDataBase.ReleaseCachedObjects;
begin
  FreeAndNil(fCachedQuery);
  FreeAndNil(fCachedTable);
end;

procedure TBoldIBDataBase.ReleaseQuery(var Query: IBoldQuery);
var
  IBQuery: TBoldIBQuery;
begin
  if Query.Implementor is TBoldIBQuery then
  begin
    IBQuery := Query.Implementor as TBoldIBQuery;
    Query := nil;
    if not assigned(fCachedQuery) then
    begin
      fCachedQuery := IBQuery.fQuery;
      if fCachedQuery.Active then
        fCachedQuery.Close;
      fCachedQuery.SQL.Clear;
      fCachedQuery.Params.Clear;
    end
    else
      IBQuery.fQuery.free;
    IBQuery.Free;
  end;
end;

procedure TBoldIBDataBase.ReleaseTable(var Table: IBoldTable);
var
  IBTable: TBoldIBTable;
begin
  if Table.Implementor is TBoldIBTable then
  begin
    IBTable := Table.Implementor as TBoldIBTable;
    Table := nil;
    if not assigned(fCachedTable) then
    begin
      fCachedTable := IBTable.fTable;
    end
    else
      IBTable.fTable.free;
    IBTable.Free;
  end;
end;

procedure TBoldIBDataBase.RollBack;
var
  transaction: TIBTransaction;
begin
  transaction := DataBase.FindDefaultTransaction;
  if assigned(Transaction) then
    transaction.Rollback
  else
    raise Exception.CreateFmt('%s.Rollback: No default transaction', [ClassName]);
end;

procedure TBoldIBDataBase.SetKeepConnection(NewValue: Boolean);
begin
end;

procedure TBoldIBDataBase.SetlogInPrompt(NewValue: Boolean);
begin
  DataBase.LoginPrompt := NewValue;
end;

procedure TBoldIBDataBase.StartTransaction;
var
  transaction: TIBTransaction;
begin
  transaction := DataBase.FindDefaultTransaction;
  if assigned(Transaction) then
    transaction.StartTransaction
  else
    raise Exception.CreateFmt('%s.StartTransaction: No Default Transaction', [ClassName]);
end;

function TBoldIBDataBase.SupportsTableCreation: Boolean;
begin
  result := true;
end;

end.
