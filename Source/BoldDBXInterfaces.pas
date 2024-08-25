////////////////////////////////////////////////////////
//                                                     //
//              Bold for Delphi                        //
//    Copyright (c) 2002 BoldSoft AB, Sweden           //
//                                                     //
/////////////////////////////////////////////////////////

{ Global compiler directives }
{$include bold.inc}
unit BoldDBXInterfaces;

interface

uses
  Classes,
  Db,
  SQlExpr,
  DBXCommon,
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
    fSQLStrings: TStringList;
    fReadTransactionStarted: Boolean;
    fUseReadTransactions: boolean;
    procedure AssignParams(SourceParams: TParams);
    function GetParams: TParams;    
    function GetParamCount: integer;
    function GetParam(i: integer): IBoldParameter;
    function GetRequestLiveQuery: Boolean;
    function ParamByName(const Value: string): IBoldParameter;
    procedure SetRequestLiveQuery(NewValue: Boolean);
    function GetSQLText: String;
    procedure ClearParams;
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
    function GetSQLStrings: TStrings;
    procedure StartSQLBatch; virtual;
    procedure EndSQLBatch; virtual;
    procedure FailSQLBatch; virtual;
    procedure ExecSQL; virtual;
    property Query: TSQLQuery read fQuery;
    procedure Open; override;
    procedure Close; override;
  public
    constructor Create(Query: TSQLQuery; DatabaseWrapper: TBoldDatabaseWrapper); virtual;
    destructor Destroy; override;
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
    procedure SetTableName(const NewName: String);
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
    fTransaction: TDBXTransaction;
    fCachedTable: TSQLTable;
    fCachedQuery: TSQLQuery;
    fExecuteQueryCount: integer;
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
  protected
    procedure AllTableNames(Pattern: String; ShowSystemTables: Boolean; TableNameList: TStrings); override;
    function GetQuery: IBoldQuery; override;
    procedure ReleaseQuery(var Query: IBoldQuery); override;
    function GetTable: IBoldTable; override;
    procedure ReleaseTable(var Table: IBoldTable); override;    
  public
    constructor create(DataBase: TSQLConnection; SQLDataBaseConfig: TBoldSQLDataBaseConfig);
    destructor destroy; override;
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
  {$IFDEF ATTRACS}
  AttracsDefs,
  AttracsPerformance,
  AttracsTraceLog,
  {$IFDEF BOLD_PERFORMANCE_COUNTERS}
  BoldSystemPerf,
  {$ENDIF}
  {$ENDIF}
  BoldUtils;

{ TBoldDBXQuery }

procedure TBoldDBXQuery.AssignParams(Sourceparams: tparams);
var
  i: integer;
begin
  if assigned(Sourceparams) then
    Query.Params.Assign(SourceParams)
  else
    Query.Params.Clear;
    for i := 0 to Query.Params.Count - 1 do
      if Query.Params[i].DataType = ftDateTime then
        Query.Params[i].DataType := ftDate; // Patch since DBX does not support datetime in this version.
      
      
end;

function TBoldDBXQuery.GetParamCheck: Boolean;
begin
  Result := Query.ParamCheck;
end;

function TBoldDBXQuery.GetParamCount: integer;
begin
  result := Query.params.count;
end;

function TBoldDBXQuery.GetParams: TParams;
begin
  result := Query.Params;
end;

function TBoldDBXQuery.GetParam(I: integer): IBoldParameter;
begin
  result := TBoldDBXParameter.Create(Query.Params[i], self);
end;

function TBoldDBXQuery.GetREquestLiveQuery: Boolean;
begin
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

procedure TBoldDBXQuery.SetParamCheck(value: Boolean);
begin
  Query.ParamCheck := Value;
end;

procedure TBoldDBXQuery.SetRequestLiveQuery(NewValue: Boolean);
begin

end;

procedure TBoldDBXQuery.SetUseReadTransactions(value: boolean);
begin
  fUseReadTransactions := value;
end;

function TBoldDBXQuery.GetBatchQueryParamCount: integer;
begin
  result := 0;
end;

function TBoldDBXQuery.GetDataSet: TDataSet;
begin
  result := Query;
end;

procedure TBoldDBXQuery.ExecSQL;
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

{$IFDEF BOLD_DELPHI10_OR_LATER}
  {$IFDEF BOLD_UNICODE}
  BoldLogSQL(Query.SQL);
  {$ELSE}
  BoldLogSQLWide(Query.SQL, self);
  {$ENDIF}
{$ELSE}
  BoldLogSQL(Query.SQL);
{$ENDIF}

  Retries := 0;
  Done := false;
  while not Done do
  begin
    try
      if Query.SQLConnection.InTransaction then
        fReadTransactionStarted := false
      else
      begin
        (DatabaseWrapper as TBoldDBXDataBase).StartReadTransaction;
        fReadTransactionStarted := true;
      end;

      Query.ExecSQL;
      if fReadTransactionStarted and  (DatabaseWrapper as TBoldDBXDataBase).GetInTransaction then
      begin
       (DatabaseWrapper as TBoldDBXDataBase).Commit;
       fReadTransactionStarted := false;
      end;
      Done := true;
    except
      on e: Exception do
      begin
        e.Message := e.Message + BOLDCRLF + 'SQL: ' + Query.SQL.text;
        if (not fReadTransactionStarted) or (Retries > 4) then
          raise;
        if (DatabaseWrapper as TBoldDBXDataBase).GetInTransaction then
          (DatabaseWrapper as TBoldDBXDataBase).Rollback;
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
    PerformanceMeasurement.WhatMeasured := 'TBoldDBXQuery.ExecSQL';
    PerformanceMeasurement.WhatMeasuredParameter := Query.SQL.Text;
    PerformanceMeasurement.Trace;
  end;
{$ENDIF}
  finally
    EndExecuteQuery;
  end;
end;

constructor TBoldDBXQuery.Create(Query: TSQLQuery; DatabaseWrapper: TBoldDatabaseWrapper);
begin
  inherited Create(DatabaseWrapper);
  fQuery := Query;
  SetParamCheck(true);
  fUseReadTransactions := true;
  fSQLStrings := TStringList.Create;
end;

procedure TBoldDBXQuery.BeginExecuteQuery;
begin
  (DatabaseWrapper as TBoldDBXDataBase).BeginExecuteQuery;
end;

procedure TBoldDBXQuery.EndExecuteQuery;
begin
  (DatabaseWrapper as TBoldDBXDataBase).EndExecuteQuery;
end;

procedure TBoldDBXQuery.EndSQLBatch;
begin
end;

procedure TBoldDBXQuery.StartSQLBatch;
begin
end;

procedure TBoldDBXQuery.FailSQLBatch;
begin
end;

procedure TBoldDBXQuery.Open;
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
{$IFDEF BOLD_DELPHI10_OR_LATER}
  {$IFDEF BOLD_UNICODE}
  BoldLogSQL(Query.SQL);
  {$ELSE}
  BoldLogSQLWide(Query.SQL, self);
  {$ENDIF}
{$ELSE}
  BoldLogSQL(Query.SQL);
{$ENDIF}
  Retries := 0;
  Done := false;
  while not Done do
  begin
    try
      if (DatabaseWrapper as TBoldDBXDataBase).GetInTransaction then
        fReadTransactionStarted := false
      else
      begin
        (DatabaseWrapper as TBoldDBXDataBase).StartReadTransaction;
        fReadTransactionStarted := true;
      end;
      inherited;
      Done := true;
    except
      on e: Exception do
      begin
        e.Message := e.Message + BOLDCRLF + 'SQL: ' + Query.SQL.text;
        if (not fReadTransactionStarted) or (Retries > 4) then
          raise;
        if (DatabaseWrapper as TBoldDBXDataBase).GetInTransaction then
          (DatabaseWrapper as TBoldDBXDataBase).Rollback;
        fReadTransactionStarted := false;
        INC(Retries);
        sleep(Retries*200);
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
    PerformanceMeasurement.WhatMeasured := 'TBoldDBXQuery.Open';
    PerformanceMeasurement.WhatMeasuredParameter := Query.SQL.Text;
    PerformanceMeasurement.Trace;
  end;
{$ENDIF}
end;

procedure TBoldDBXQuery.AssignSQL(SQL: TStrings);
begin
  Query.SQL.BeginUpdate;
  Query.SQL.Assign(SQL);
  Query.SQL.EndUpdate;
end;

procedure TBoldDBXQuery.AssignSQLText(const SQL: String);
begin
  Query.SQL.BeginUpdate;
  Query.SQL.Clear;
{$IFDEF BOLD_DELPHI10_OR_LATER}
  Query.SQL.Append(SQL);  // FIXME, this gives one long line.
{$ELSE}
  BoldAppendToStrings(Query.SQL, SQL, true);
{$ENDIF}

  Query.SQL.EndUpdate;
end;

function TBoldDBXQuery.GetSQLStrings: TStrings;
begin
  result := fSQLStrings;
  result.clear;
  result.Add(Query.SQL.Text);
end;

function TBoldDBXQuery.GetSQLText: String;
begin
  result := Query.SQL.text;
end;

function TBoldDBXQuery.GetUseReadTransactions: boolean;
begin
  result := fUseReadTransactions;
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

procedure TBoldDBXQuery.Close;
begin
  inherited;
  if (fReadTransactionStarted) and (DatabaseWrapper as TBoldDBXDataBase).GetInTransaction then
    (DatabaseWrapper as TBoldDBXDataBase).Commit;
  fReadTransactionStarted := false;
end;

function TBoldDBXQuery.Createparam(FldType: TFieldType;
  const ParamName: string; ParamType: TParamType;
  Size: integer): IBoldParameter;
begin
  result := TBoldDbParameter.Create(Query.params.CreateParam(fldType, ParamName, ParamType), self);
end;

destructor TBoldDBXQuery.Destroy;
begin
  if (fReadTransactionStarted) then
    Close;
  FreeAndNil(fSQLStrings);
  inherited;
end;

{ TBoldDBXTable }

procedure TBoldDBXTable.AddIndex(const Name, Fields: string;
  Options: TIndexOptions; const DescFields: string);
begin

end;

constructor TBoldDBXTable.Create(Table: TSQLTable; DatabaseWrapper: TBoldDatabaseWrapper);
begin
  inherited Create(DatabaseWrapper);
  fTable := Table;
end;

procedure TBoldDBXTable.CreateTable;
begin
  raise EBold.CreateFmt('%s.CreateTable: Not supported', [classname]);
end;

procedure TBoldDBXTable.DeleteTable;
begin
end;

function TBoldDBXTable.GetDataSet: TDataSet;
begin
  result := Table;
end;

function TBoldDBXTable.GetExclusive: Boolean;
begin
  result := false;
end;

function TBoldDBXTable.GetExists: Boolean;
var
  DB: IBoldDataBase;
  NameList: TStringList;
begin
  NameList := TStringList.Create;
  NameList.CaseSensitive := false;
  try
    DB:= DatabaseWrapper as IBoldDataBase;
    DB.AllTableNames('', false, NameList);
    Result := NameList.IndexOf(fTable.TableName) > -1;
  finally
    FreeANdNil(NameList);
  end;
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
end;

procedure TBoldDBXTable.SetTableName(const NewName: String);
var
  DB: IBoldDataBase;
  NameList: TStringList;
  NameIndex: Integer;
  s: string;
begin
  s := NewName;
  NameList := TStringList.Create;
  NameList.CaseSensitive := false;
  try
    DB:= DatabaseWrapper as IBoldDataBase;
    DB.AllTableNames('', false, NameList);
    NameIndex := NameList.IndexOf(s);
    if NameIndex > -1 then
      s := NameList[NameIndex];
  finally
    FreeANdNil(NameList);
  end;
  Table.TableName := s;
end;

{ TBoldDBXDataBase }

procedure TBoldDBXDataBase.AllTableNames(Pattern: String; ShowSystemTables: Boolean; TableNameList: TStrings);
var
  SystemTableNames: TStringList;
  TableName: string;
begin
  if (Pattern <> '') and (Pattern <> '*') then
    raise Exception.CreateFmt('%s.AlltableNames: This call does not allow patterns ("%s")', [ClassName, Pattern]);
  fDataBase.GetTableNames(TableNameList, false);
  if ShowSystemTables then
  begin
    SystemTableNames := TStringList.Create;
    try
      fDataBase.GetTableNames(SystemTableNames, true);
      for TableName in SystemTableNames do
        if TableNameList.IndexOf(TableName) = -1 then
          TableNameList.Add(TableName);
    finally
      FreeAndNil(SystemTableNames);
    end;
  end;

end;

function TBoldDBXDataBase.GetInTransaction: Boolean;
begin
  result := fDataBase.InTransaction;
end;

function TBoldDBXDataBase.GetIsExecutingQuery: Boolean;
begin
  Result := fExecuteQueryCount > 0;
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

procedure TBoldDBXDataBase.StartReadTransaction;
begin
  fTransaction :=  fDataBase.BeginTransaction(TDBXIsolations.ReadCommitted);
end;

procedure TBoldDBXDataBase.StartTransaction;
begin
  fTransaction :=  fDataBase.BeginTransaction(TDBXIsolations.RepeatableRead);
end;

procedure TBoldDBXDataBase.Commit;
begin
  fDatabase.CommitFreeAndNil(fTransaction);
end;

procedure TBoldDBXDataBase.RollBack;
begin
  fDataBase.RollbackFreeAndNil(fTransaction);
end;

procedure TBoldDBXDataBase.Open;
begin
  fDataBase.Open;
end;

procedure TBoldDBXDataBase.BeginExecuteQuery;
begin
  inc(fExecuteQueryCount);
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

procedure TBoldDBXDataBase.EndExecuteQuery;
begin
  dec(fExecuteQueryCount);
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
      fCachedQuery.Params.Clear;
    end
    else
      DBXQuery.fQuery.free;
    DBXQuery.fQuery := nil;  
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

procedure TBoldDBXDataBase.Reconnect;
begin
  if Assigned(fDataBase) then begin
    fDataBase.Connected := False;
    fDataBase.Connected := True;
  end;
end;

procedure TBoldDBXDataBase.ReleaseCachedObjects;
begin
  FreeAndNil(fCachedTable);
  FreeAndNil(fCachedQuery);
end;

{ TBoldDBXParameter }

function TBoldDBXParameter.GetAsDateTime: TDateTime;
begin
  result := SQLTimeStampToDateTime(Parameter.AsSQLTimeStamp);
end;

procedure TBoldDBXParameter.SetAsDateTime(const Value: TDateTime);
begin

  Parameter.AsSQLTimeStamp := DateTimetoSQLTimeStamp(value);
end;

end.
