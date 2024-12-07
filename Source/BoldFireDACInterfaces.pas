
{ Global compiler directives }
{$include bold.inc}
unit BoldFireDACInterfaces;

interface

uses
  Classes,
  Db,
  SysUtils,

  FireDAC.Comp.Client,
  FireDAC.Stan.Param,

  BoldSQLDatabaseConfig,
  BoldDBInterfaces,
  BoldDefs;

type
  { forward declarations }
  TBoldFireDACParameter = class;
  TBoldFireDACQuery = class;
  TBoldFireDACTable = class;
  TBoldFireDACConnection = class;

  TFireDacParam = TFDParam;

  TBoldFireDACQueryClass = class of TBoldFireDACQuery;
  TBoldFireDACExecQueryClass = class of TBoldFireDACExecQuery;

  { TBoldFireDACParameter }
  TBoldFireDACParameter = class(TBoldParameterWrapper, IBoldParameter)
  private
    fFDParam: TFireDacParam;
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
    function GetFDParam: TFireDacParam;
    procedure AssignFieldValue(const source: IBoldField);
    procedure Assign(const source: IBoldParameter);
    property FDParam: TFireDacParam read GetFDParam;
  public
    constructor Create(FireDACParameter: TFireDacParam; DatasetWrapper: TBoldAbstractQueryWrapper);
  end;

  { TBoldFireDACQuery }
  TBoldFireDACQuery = class(TBoldBatchDataSetWrapper, IBoldQuery, IBoldExecQuery, IBoldParameterized)
  private
    fQuery: TFDQuery;
    fReadTransactionStarted: Boolean;
    fUseReadTransactions: boolean;
    function GetQuery: TFDQuery;
    procedure AssignParams(Sourceparams: TParams);
    function GetParamCount: Integer;
    function GetParam(i: Integer): IBoldParameter;
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
    function Createparam(FldType: TFieldType; const ParamName: string; ParamType: TParamType; Size: integer): IBoldParameter; override;
    function GetParams: TParams; override;
    function GetSqlText: string; override;
    function GetSQLStrings: TStrings; override;
    procedure AssignSQLText(const SQL: string); override;
    function GetRowsAffected: Integer;
    function GetDataSet: TDataSet; override;
    procedure Prepare;
    procedure ClearParams;
    procedure Open; override;
    procedure Close; override;
    procedure ExecSQL; override;
    function GetRecNo: integer; override;
    property Query: TFDQuery read GetQuery;
  public
    constructor Create(BoldFireDACConnection: TBoldFireDACConnection); reintroduce;
    destructor Destroy; override;
    procedure Clear; override;
  end;

  { TBoldFireDACQuery }
  TBoldFireDACExecQuery = class(TBoldAbstractQueryWrapper, IBoldExecQuery, IBoldParameterized)
  private
    fExecQuery: TFDQuery;
    fReadTransactionStarted: Boolean;
    fUseReadTransactions: boolean;
    function GetExecQuery: TFDQuery;
    function GetParams: TParams;
    procedure AssignParams(Sourceparams: TParams);
    function GetParamCount: Integer;
    function GetParam(i: Integer): IBoldParameter;
    function GetParamCheck: Boolean;
    procedure SetParamCheck(value: Boolean);
    function ParamByName(const Value: string): IBoldParameter;
    function FindParam(const Value: string): IBoldParameter;
    function Createparam(FldType: TFieldType; const ParamName: string): IBoldParameter; overload;
    function CreateParam(FldType: TFieldType; const ParamName: string; ParamType: TParamType; Size: integer): IBoldParameter; overload;
    function EnsureParamByName(const Value: string): IBoldParameter;
    function GetSQLText: string;
    function GetSQLStrings: TStrings;
    procedure AssignSQL(SQL: TStrings); virtual;
    procedure AssignSQLText(const SQL: string);
    function GetRowsAffected: Integer;
    function GetUseReadTransactions: boolean;
    procedure SetUseReadTransactions(value: boolean);
    procedure BeginExecuteQuery;
    procedure EndExecuteQuery;
    function GetBatchQueryParamCount: integer;
    procedure Prepare;
  protected
    procedure StartSQLBatch; virtual;
    procedure EndSQLBatch; virtual;
    procedure FailSQLBatch; virtual;
    procedure ClearParams;
    procedure ExecSQL; virtual;
    property ExecQuery: TFDQuery read GetExecQuery;
  public
    constructor Create(BoldFireDACConnection: TBoldFireDACConnection); reintroduce;
    destructor Destroy; override;
    procedure Clear; override;
  end;

  { TBoldFireDACTable }
  TBoldFireDACTable = class(TBoldDatasetWrapper, IBoldTable)
  private
    fFDTable: TFDTable;
    function GetFDTable: TFDTable;
    property FDTable: TFDTable read GetFDTable;
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
    constructor Create(aFDTable: TFDTable; BoldFireDACConnection: TBoldFireDACConnection); reintroduce;
  end;

  { TBoldFireDACConnection }
  TBoldFireDACConnection = class(TBoldDatabaseWrapper, IBoldDataBase)
    fFDConnection: TFDConnection;
    fCachedTable: TBoldFireDACTable;
    fCachedQuery1: TBoldFireDACQuery;
    fCachedQuery2: TBoldFireDACQuery;
    fCachedExecQuery1: TBoldFireDACQuery;
    fExecuteQueryCount: integer;
    function GetFDConnection: TFDConnection;
    property FDConnection: TFDConnection read GetFDConnection;
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
  private
    function GetTransaction: TFDTransaction;
    function GetUpdateTransaction: TFDTransaction;
    procedure SetTransaction(const Value: TFDTransaction);
    procedure SetUpdateTransaction(const Value: TFDTransaction);
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
    property Transaction: TFDTransaction read GetTransaction write SetTransaction;
    property UpdateTransaction: TFDTransaction read GetUpdateTransaction write SetUpdateTransaction;
  public
    constructor Create(aFDConnection: TFDConnection; SQLDataBaseConfig: TBoldSQLDatabaseConfig);
    destructor Destroy; override;
    procedure CreateDatabase(DropExisting: boolean = true); override;
    procedure DropDatabase; override;
    function DatabaseExists: boolean; override;
    function GetDatabaseError(const E: Exception; const sSQL: string = ''):
        EBoldDatabaseError;
    property ExecuteQueryCount: integer read fExecuteQueryCount;
  end;

var
  BoldFireDACQueryClass: TBoldFireDACQueryClass = TBoldFireDACQuery;
  BoldFireDACExecQueryClass: TBoldFireDACExecQueryClass = TBoldFireDACExecQuery;

implementation

uses
  Variants,
  Masks,

  FireDAC.Stan.Option,
  FireDAC.Comp.Script,
  FireDAC.Comp.ScriptCommands,
  FireDAC.Phys.Intf,
  FireDAC.Phys.PGWrapper,
  FireDAC.Stan.Intf,

  BoldUtils,
  BoldGuard,
  BoldCoreConsts;

{ TBoldFireDACQuery }

function TBoldFireDACQuery.GetQuery: TFDQuery;
begin
  if not Assigned(fQuery) then
  begin
    fQuery := TFDQuery.Create(nil);
    fQuery.Connection := (DatabaseWrapper as TBoldFireDACConnection).FDConnection;
  end;
  Result := fQuery;
end;

function TBoldFireDACQuery.GetDataSet: TDataSet;
begin
  Result := Query;
end;

function TBoldFireDACQuery.GetParamCheck: Boolean;
begin
  result := Query.ResourceOptions.ParamCreate;
end;

function TBoldFireDACQuery.GetParamCount: Integer;
begin
  Result := Query.Params.Count;
end;

type TFDAdaptedDataSetAccess = class(TFDAdaptedDataSet);

function TBoldFireDACQuery.GetParams: TParams;
begin
  result := TFDAdaptedDataSetAccess(Query).PSGetParams;
end;

function TBoldFireDACQuery.GetParam(i: Integer): IBoldParameter;
begin
  Result := TBoldFireDACParameter.Create(Query.Params[i], Self);
end;

function TBoldFireDACQuery.GetRecNo: integer;
begin
  result := Query.RecNo - 1;
end;

function TBoldFireDACQuery.GetRecordCount: Integer;
begin
  Result := Query.RecordCount;
end;

function TBoldFireDACQuery.GetRequestLiveQuery: Boolean;
begin
  Result := False;
end;

function TBoldFireDACQuery.GetRowsAffected: Integer;
begin
  result := Query.RowsAffected;
end;

function TBoldFireDACQuery.GetSQLStrings: TStrings;
begin
  result := Query.SQL;
end;

function TBoldFireDACQuery.GetSQLText: string;
begin
  Result := Query.SQL.Text;
end;

function TBoldFireDACQuery.GetUseReadTransactions: boolean;
begin
  result := fUseReadTransactions;
end;

procedure TBoldFireDACQuery.AssignParams(Sourceparams: TParams);
var
  lIndexSourceParams: Integer;
  lFDParam: TFireDacParam;
begin
  Query.Params.Clear;
  if Assigned(Sourceparams) and (Sourceparams.Count > 0) then
  begin
    for lIndexSourceParams := 0 to Sourceparams.Count - 1 do
    begin
      lFDParam := Query.Params.CreateParam(Sourceparams[lIndexSourceParams].DataType, Sourceparams[lIndexSourceParams].Name, Sourceparams[lIndexSourceParams].ParamType) as TFireDacParam;
      lFDParam.Value := Sourceparams[lIndexSourceParams].Value;
    end;
  end;
end;

procedure TBoldFireDACQuery.AssignSQL(SQL: TStrings);
begin
  Query.SQL.Assign(SQL);
  //function ParseSQL(SQL: WideString; DoCreate: Boolean): WideString;
  //DoCreate indicates whether to clear all existing parameter definitions before parsing the SQL statement.
end;

procedure TBoldFireDACQuery.AssignSQLText(const SQL: string);
begin
  Query.SQL.Text := Sql;
{  if SQL = '' then
    Query.Params.clear
  else
    Query.Params.ParseSQL(SQL, False);}
end;

procedure TBoldFireDACQuery.BeginExecuteQuery;
begin
  (DatabaseWrapper as TBoldFireDACConnection).BeginExecuteQuery;
end;

procedure TBoldFireDACQuery.Clear;
begin
  AssignSQLText('');
  ClearParams;
end;

procedure TBoldFireDACQuery.ClearParams;
begin
  Query.Params.Clear;
end;

procedure TBoldFireDACQuery.Close;
begin
  inherited;
  if (fReadTransactionStarted) and (DatabaseWrapper as TBoldFireDACConnection).GetInTransaction then
    (DatabaseWrapper as TBoldFireDACConnection).Commit;
  fReadTransactionStarted := false;
end;

constructor TBoldFireDACQuery.Create(BoldFireDACConnection: TBoldFireDACConnection);
begin
  inherited Create(BoldFireDACConnection);
  fUseReadTransactions := true;
  fQuery := TFDQuery.Create(nil);
  fQuery.Connection := (DatabaseWrapper as TBoldFireDACConnection).FDConnection;
end;

function TBoldFireDACQuery.CreateParam(FldType: TFieldType; const ParamName: string; ParamType: TParamType; Size: integer): IBoldParameter;
var
  lFDParam: TFireDacParam;
begin
  lFDParam := Query.Params.CreateParam(FldType, ParamName, ptUnknown) as TFireDacParam;
//  lFDParam.Size := Size;
  lFDParam.Value := NULL;
  Result := TBoldFireDACParameter.Create(lFDParam, Self);
end;

destructor TBoldFireDACQuery.Destroy;
begin
  if (fReadTransactionStarted) then
    Close;
  FreeAndNil(fQuery);
  inherited;
end;

procedure TBoldFireDACQuery.EndExecuteQuery;
begin
  (DatabaseWrapper as TBoldFireDACConnection).EndExecuteQuery;
end;

type TStringsAccess = class(TStrings);

procedure TBoldFireDACQuery.ExecSQL;
var
  Retries: Integer;
  Done: Boolean;
begin
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
        if (DatabaseWrapper as TBoldFireDACConnection).GetInTransaction then
          fReadTransactionStarted := false
        else
        begin
          if fUseReadTransactions then
          (DatabaseWrapper as TBoldFireDACConnection).StartReadTransaction;
          fReadTransactionStarted := fUseReadTransactions;
        end;
        Query.Execute;
        if fReadTransactionStarted and (DatabaseWrapper as TBoldFireDACConnection).GetInTransaction then
        begin
         (DatabaseWrapper as TBoldFireDACConnection).Commit;
         fReadTransactionStarted := false;
        end;
        Done := true;
      except
        on e: Exception do
        begin
          if (DatabaseWrapper as TBoldFireDACConnection).GetInTransaction then
            (DatabaseWrapper as TBoldFireDACConnection).Rollback;
          if (not fReadTransactionStarted) or (Retries > 4) then
            raise TBoldFireDACConnection(DatabaseWrapper).GetDatabaseError(E, Query.SQL.Text);
          fReadTransactionStarted := false;
          INC(Retries);
          sleep(Retries*200);
        end;
      end;
    end;
  finally
    EndExecuteQuery;
  end;
end;

function TBoldFireDACQuery.FindParam(const Value: string): IBoldParameter;
var
  Param: TFireDacParam;
begin
  result := nil;
  Param := Query.FindParam(Value);
  if Assigned(Param) then
    Result := TBoldFireDACParameter.Create(Param, Self);
end;

procedure TBoldFireDACQuery.Open;
var
  Retries: Integer;
  Done: Boolean;
  EDatabase: EBoldDatabaseError;
begin
  BeginExecuteQuery;
  try
  BoldLogSQLWithParams(Query.SQL, self);
  Retries := 0;
  Done := false;
  while not Done do
  begin
    try
      if (DatabaseWrapper as TBoldFireDACConnection).GetInTransaction then
        fReadTransactionStarted := false
      else
      begin
        if fUseReadTransactions then
          (DatabaseWrapper as TBoldFireDACConnection).StartReadTransaction;
        fReadTransactionStarted := fUseReadTransactions;
      end;
      Query.UpdateOptions.ReadOnly := true;
      inherited;
      Done := true;
    except
      on e: Exception do
      begin
        EDatabase := TBoldFireDACConnection(DatabaseWrapper).
            GetDatabaseError(E, Query.SQL.Text);
        if (EDatabase is EBoldDatabaseConnectionError) {and
           (not Assigned(ReconnectAppExists) or ReconnectAppExists)} then
        begin
          EDatabase.free;
//          ReconnectDatabase(Query.SQL.Text);
          Reconnect;
        end else
        begin
          if (DatabaseWrapper as TBoldFireDACConnection).GetInTransaction then
            (DatabaseWrapper as TBoldFireDACConnection).Rollback;
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
  finally
    EndExecuteQuery;
  end;
end;

function TBoldFireDACQuery.ParamByName(const Value: string): IBoldParameter;
var
  lFDParam: TFireDacParam;
begin
  lFDParam := Query.Params.ParamByName(Value);
  Result := TBoldFireDACParameter.Create(lFDParam, Self)
end;

procedure TBoldFireDACQuery.Prepare;
begin
  Query.Prepare;
end;

procedure TBoldFireDACQuery.SetParamCheck(value: Boolean);
begin
  Query.ResourceOptions.ParamCreate := Value;
end;

procedure TBoldFireDACQuery.SetRequestLiveQuery(NewValue: Boolean);
begin
  // ignore
end;

procedure TBoldFireDACQuery.SetUseReadTransactions(value: boolean);
begin
  fUseReadTransactions := value;
end;

{ TBoldFireDACTable }

constructor TBoldFireDACTable.Create(aFDTable: TFDTable; BoldFireDACConnection: TBoldFireDACConnection);
begin
  inherited Create(BoldFireDACConnection);
  fFDTable := aFDTable;
end;

procedure TBoldFireDACTable.CreateTable;
begin
  FDTable.CreateTable(true);
end;

procedure TBoldFireDACTable.DeleteTable;
begin
  FDTable.ExecSQL('Drop Table ' + FDTable.TableName);
end;

function TBoldFireDACTable.FindParam(const Value: string): IBoldParameter;
var
  Param: TFireDacParam;
begin
  result := nil;
  Param := FDTable.FindParam(Value);
  if Assigned(Param) then
    Result := TBoldFireDACParameter.Create(Param, Self);
end;

procedure TBoldFireDACTable.AddIndex(const Name, Fields: string;
  Options: TIndexOptions; const DescFields: string);
var
  SortOption: TFDSortOptions;
begin
// ixPrimary, ixUnique, ixDescending, ixCaseInsensitive, ixExpression, ixNonMaintained);
// soNoCase, soNullFirst, soDescNullLast, soDescending, soUnique, soPrimary, soNoSymbols);
  SortOption := [];
  if ixCaseInsensitive in Options  then
    Include(SortOption, soNoCase);
  if ixUnique in Options  then
    Include(SortOption, soUnique);
  if ixPrimary in Options  then
    Include(SortOption, soPrimary);
  if ixDescending in Options  then
    Include(SortOption, soDescending);
  FDTable.AddIndex(Name, Fields, '', SortOption, DescFields);
end;
(*
function TBoldFireDACTable.GetCommaListOfIndexesForColumn(
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

function TBoldFireDACTable.GetPrimaryIndex: string;
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
function TBoldFireDACTable.GetDataSet: TDataSet;
begin
  Result := fFDTable;
end;

function TBoldFireDACTable.GetDefaultConstraintNameForColumn(
  const aColumnName: string): string;
var
  lFDMetaData: TFDMetaInfoQuery;
  lDefaultConstraintName: string;
  lBoldGuard: IBoldGuard;
const
  cConstraintName = 'CONSTRAINT_NAME';
begin
  Assert(Assigned(FDTable));
  Assert(Assigned(FDTable.Connection));

  lBoldGuard := TBoldGuard.Create(lFDMetaData);
  lFDMetaData := TFDMetaInfoQuery.Create(nil);
  lFDMetaData.Connection := FDTable.Connection;
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

function TBoldFireDACTable.GetExclusive: Boolean;
begin
  Result := False;
end;

function TBoldFireDACTable.GetExists: Boolean;
var
  lAllTables: TStringList;
  lGuard: IBoldGuard;
begin
  Result := False;
  if Assigned(FDTable) and Assigned(FDTable.Connection) then
  begin
    lGuard := TBoldGuard.Create(lAllTables);
    lAllTables := TStringList.Create;
    FDTable.Connection.GetTableNames('', '', '', lAllTables, [osMy], [tkTable], false);
    Result := lAllTables.IndexOf(GetTableName) <> -1;
  end;
end;

function TBoldFireDACTable.GetIndexDefs: TIndexDefs;
begin
  Result := FDTable.IndexDefs;
end;

function TBoldFireDACTable.GetFDTable: TFDTable;
begin
  Result := fFDTable;
end;

type TFDTableAccess = class(TFDTable);

function TBoldFireDACTable.ParamByName(const Value: string): IBoldParameter;
var
  lFDParam: TFireDacParam;
begin
  lFDParam := TFDTableAccess(FDTable).Params.ParamByName(Value);
  Result := TBoldFireDACParameter.Create(lFDParam, Self);
end;

function TBoldFireDACTable.GetTableName: string;
begin
  Result := FDTable.TableName;
end;

procedure TBoldFireDACTable.SetExclusive(NewValue: Boolean);
begin
end;

procedure TBoldFireDACTable.SetTableName(const NewName: string);
begin
  FDTable.TableName := NewName;
end;

{ TBoldFireDACConnection }

// Populate the "TableNameList" with tablenames from the database that maches "pattern"

procedure TBoldFireDACConnection.AllTableNames(Pattern: string; ShowSystemTables: Boolean; TableNameList: TStrings);
var
  lTempList: TStringList;
  lIndexTempList: Integer;
  lGuard: IBoldGuard;
  i: integer;
begin
  lGuard := TBoldGuard.Create(lTempList);
  lTempList := TStringList.Create;
  if ShowSystemTables then
    FDConnection.GetTableNames(FDConnection.Params.Database,'','',lTempList, [osMy, osSystem, osOther], [tkTable])
  else
    FDConnection.GetTableNames(FDConnection.Params.Database,'','',lTempList, [osMy], [tkTable]);

  // convert from fully qualified names in format: database.catalogue.table to just table name
  for i := 0 to lTempList.Count - 1 do
    while pos('.', lTempList[i]) > 0 do
      lTempList[i] := Copy(lTempList[i], pos('.', lTempList[i])+1, maxInt);

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

procedure TBoldFireDACConnection.Commit;
begin
  FDConnection.Commit;
end;

function TBoldFireDACConnection.GetImplementor: TObject;
begin
  result := FDConnection;
end;

function TBoldFireDACConnection.GetInTransaction: Boolean;
begin
  Result := FDConnection.InTransaction;
end;

function TBoldFireDACConnection.GetIsExecutingQuery: Boolean;
begin
  Result := fExecuteQueryCount > 0;
end;

function TBoldFireDACConnection.GetIsSQLBased: Boolean;
begin
  Result := True;
end;

function TBoldFireDACConnection.GetKeepConnection: Boolean;
begin
  //CheckMe;
  Result := True;
end;

function TBoldFireDACConnection.GetLogInPrompt: Boolean;
begin
  Result := FDConnection.LoginPrompt;
end;

procedure TBoldFireDACConnection.RollBack;
begin
  FDConnection.RollBack;
end;

procedure TBoldFireDACConnection.SetKeepConnection(NewValue: Boolean);
begin
  //CheckMe;
end;

procedure TBoldFireDACConnection.SetlogInPrompt(NewValue: Boolean);
begin
  FDConnection.LoginPrompt := NewValue;
end;

procedure TBoldFireDACConnection.SetTransaction(const Value: TFDTransaction);
begin
  FDConnection.Transaction := Value;
end;

procedure TBoldFireDACConnection.SetUpdateTransaction(
  const Value: TFDTransaction);
begin
  FDConnection.UpdateTransaction := value;
end;

procedure TBoldFireDACConnection.StartReadTransaction;
begin
  Transaction.Options.Isolation  := xiReadCommitted;
  FDConnection.StartTransaction;
end;

procedure TBoldFireDACConnection.StartTransaction;
begin
  Transaction.Options.Isolation := xiRepeatableRead;
  FDConnection.StartTransaction;
end;

function TBoldFireDACConnection.DatabaseExists: boolean;
var
  vQuery: IBoldQuery;
  vDatabaseName: string;
begin
  vDatabaseName := LowerCase(FDConnection.Params.Database);
  FDConnection.Params.Database := ''; // need to clear this to connect succesfully
  vQuery := GetQuery;
  try
    vQuery.SQLText := SQLDataBaseConfig.GetDatabaseExistsQuery(vDatabaseName);
    vQuery.Open;
    result := vQuery.Fields[0].AsBoolean;
  finally
    ReleaseQuery(vQuery);
    FDConnection.Params.Database := vDatabaseName;
  end;
end;

destructor TBoldFireDACConnection.Destroy;
begin
  ReleaseCachedObjects;
  inherited;
end;

procedure TBoldFireDACConnection.DropDatabase;
var
  vDatabaseName: string;
  vScript: TFDScript;
  sl: TStringList;
begin
  vDatabaseName := LowerCase(FDConnection.Params.Database);
  FDConnection.Params.Database := ''; // need to clear this to connect succesfully
  vScript := TFDScript.Create(nil);
  sl := TStringList.Create;
  try
    sl.Text := SQLDataBaseConfig.GetDropDatabaseQuery(vDatabaseName);
    vScript.Connection := FDConnection;
    vScript.ExecuteScript(sl);
    FDConnection.Close;
  finally
    FDConnection.Params.Database := vDatabaseName;
    vScript.free;
    sl.free;
  end;
end;

procedure TBoldFireDACConnection.EndExecuteQuery;
begin
  dec(fExecuteQueryCount);
end;

constructor TBoldFireDACConnection.Create(aFDConnection: TFDConnection; SQLDataBaseConfig: TBoldSQLDatabaseConfig);
begin
  inherited Create(SQLDataBaseConfig);
  fFDConnection := aFDConnection;
end;

function TBoldFireDACConnection.CreateAnotherDatabaseConnection: IBoldDatabase;
begin
  var Connection := TFDConnection.Create(nil); // owner ?
  Connection.Assign(self.fFDConnection);
  result := TBoldFireDACConnection.Create(Connection, SQLDatabaseConfig);
end;

procedure TBoldFireDACConnection.BeginExecuteQuery;
begin
  inc(fExecuteQueryCount);
end;

procedure TBoldFireDACConnection.Close;
begin
  FDConnection.Close;
end;

procedure TBoldFireDACConnection.CreateDatabase(DropExisting: boolean = true);
var
  vDatabaseName: string;
  vScript: TFDScript;
  sl: TStringList;
begin
  vDatabaseName := LowerCase(FDConnection.Params.Database);
  if DropExisting and DatabaseExists then
    DropDatabase;
  FDConnection.Params.Database := ''; // need to clear this to connect succesfully
  vScript := TFDScript.Create(nil);
  sl := TStringList.Create;
  try
    sl.Text := SQLDataBaseConfig.GetCreateDatabaseQuery(vDatabaseName);
    vScript.Connection := FDConnection;
    vScript.ExecuteScript(sl);
    FDConnection.Close;
  finally
    FDConnection.Params.Database := vDatabaseName;
    vScript.free;
    sl.free;
  end;
end;

function TBoldFireDACConnection.GetConnected: Boolean;
begin
  Result := FDConnection.Connected;
end;

function TBoldFireDACConnection.GetDatabaseError(const E: Exception;
  const sSQL: string): EBoldDatabaseError;
const
  SQLERRORCODE = 'SQL Error Code: ';
var
{  iErrorCode: Integer;
  iPos: Integer;
  sServer,
  sDatabase,
  sUsername: string;
  bUseWindowsAuth: Boolean;}
  vConnectionString: string;
  sMsg: string;
  aErrorType: TBoldDatabaseErrorType;
const
  // Provider names copied here to avoid dependancy
  cMSSQLProvider = 'SQL Server'; // TSQLServerFDProvider.GetProviderName
  cPostgreSQLProvider = 'PostgreSQL'; // TPostgreSQLFDProvider.GetProviderName
  cOracleSQLProvider = 'Oracle'; // TOracleFDProvider.GetProviderName
  cInterBaseProvider = 'InterBase';
  cMSSQLDeadLock = 1205;
begin
  sMsg := E.Message;
  aErrorType := bdetError;
  vConnectionString := FDConnection.ConnectionString;
  Result := InternalGetDatabaseError(aErrorType, E, vConnectionString, '', '', '', false);

{
  bUseWindowsAuth := Pos('Authentication=Windows', FDConnection.ConnectString) > 0;
  if (E is EFDError) then
  begin
    if FDConnection.ProviderName = cMSSQLProvider then
      case EFDError(E).ErrorCode of
        -2147467259, 2, 233: aErrorType := bdetConnection; // only set bdetConnection for cases where retry might work.
        208, 4145: aErrorType := bdetSQL;
        4060: aErrorType := bdetLogin; // SQLServer Error: 4060, Cannot open database "SessionStateService" requested by the login. The login failed. [SQLSTATE 42000]
        18456: aErrorType := bdetLogin; // SQLServer Error: 18456, Login failed for user 'domain\user'. [SQLSTATE 28000]
        cMSSQLDeadLock: aErrorType := bdetDeadlock;
        //Deadlock und weitere ErrorCodes?
      end
    else
    if FDConnection.ProviderName = cInterBaseProvider then
      case EFDError(E).ErrorCode of
        -803: aErrorType := bdetUpdate; // attempt to store duplicate value (visible to active transactions) in FDque index
      end
    else
    if FDConnection.ProviderName = cPostgreSQLProvider then
      case EFDError(E).ErrorCode of
        0: aErrorType := bdetLogin;
      end
    else
      raise Exception.Create('Error codes not implemented for ' + FDConnection.ProviderName);
  end;
  Result := InternalGetDatabaseError(aErrorType, E, sSQL, sServer, sDatabase,
      sUsername, bUseWindowsAuth);
}
end;

function TBoldFireDACConnection.GetExecQuery: IBoldExecQuery;
begin
  if Assigned(fCachedExecQuery1) then
  begin
    result := fCachedExecQuery1;
    fCachedExecQuery1 := nil;
  end else
  begin
    Result := BoldFireDACQueryClass.Create(Self);
  end;
end;

function TBoldFireDACConnection.GetFDConnection: TFDConnection;
begin
  Result := fFDConnection;
end;

function TBoldFireDACConnection.GetQuery: IBoldQuery;
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
    Result := BoldFireDACQueryClass.Create(Self);
  end;
end;

function TBoldFireDACConnection.GetTable: IBoldTable;
var
  lFDTable: TFDTable;
begin
  if Assigned(fCachedTable) then
  begin
    result := fCachedTable;
    fCachedTable := nil;
  end
  else
  begin
    lFDTable := TFDTable.Create(nil);
    lFDTable.Connection := FDConnection;
    Result := TBoldFireDACTable.Create(lFDTable, Self);
  end;
end;

function TBoldFireDACConnection.GetTransaction: TFDTransaction;
begin
  if not Assigned(FDConnection.Transaction) then
    FDConnection.Transaction := TFDTransaction.Create(FDConnection);
  result := FDConnection.Transaction as TFDTransaction;
end;

function TBoldFireDACConnection.GetUpdateTransaction: TFDTransaction;
begin
  if not Assigned(FDConnection.UpdateTransaction) then
    FDConnection.UpdateTransaction := TFDTransaction.Create(FDConnection);
  result := FDConnection.UpdateTransaction as TFDTransaction;
end;

procedure TBoldFireDACConnection.Open;
begin
  try
    FDConnection.Params.Database := LowerCase(FDConnection.Params.Database);
    FDConnection.Open;
  except
    on E: Exception do begin
      raise GetDatabaseError(E);
    end;
  end;
end;

procedure TBoldFireDACConnection.Reconnect;
begin
  if Assigned(fFDConnection) then begin
    fFDConnection.Connected := False;
    fFDConnection.Connected := True;
  end;
end;

type TCollectionAccess = class(TCollection);

procedure TBoldFireDACConnection.ReleaseQuery(var Query: IBoldQuery);
var
  lBoldFireDACQuery: TBoldFireDACQuery;
begin
  if (Query.Implementor is TBoldFireDACQuery) then
  begin
    lBoldFireDACQuery := Query.Implementor as TBoldFireDACQuery;
    lBoldFireDACQuery.clear;
    while lBoldFireDACQuery.SQLStrings.Updating do
      lBoldFireDACQuery.SQLStrings.EndUpdate;
    while TCollectionAccess(lBoldFireDACQuery.Params).UpdateCount > 0 do
      lBoldFireDACQuery.Params.EndUpdate;
    Query := nil;
    if not Assigned(fCachedQuery1) then
      fCachedQuery1 := lBoldFireDACQuery
    else
    if not Assigned(fCachedQuery2) then
      fCachedQuery2 := lBoldFireDACQuery
    else
      lBoldFireDACQuery.free;
  end
end;

procedure TBoldFireDACConnection.ReleaseExecQuery(var Query: IBoldExecQuery);
var
  lBoldFireDACQuery: TBoldFireDACQuery;
//  lBoldFireDACExecQuery: TBoldFireDACExecQuery;
begin
  if (Query.Implementor is TBoldFireDACQuery) then
  begin
    lBoldFireDACQuery := Query.Implementor as TBoldFireDACQuery;
    if lBoldFireDACQuery.SQLStrings.Count <> 0 then
    begin
      lBoldFireDACQuery.SQLStrings.BeginUpdate;
      lBoldFireDACQuery.clear;
    end;
    while lBoldFireDACQuery.SQLStrings.Updating do
      lBoldFireDACQuery.SQLStrings.EndUpdate;
    while TCollectionAccess(lBoldFireDACQuery.Params).UpdateCount > 0 do
      lBoldFireDACQuery.Params.EndUpdate;
    Query := nil;
    if not Assigned(fCachedExecQuery1) then
      fCachedExecQuery1 := lBoldFireDACQuery
    else
      lBoldFireDACQuery.free;
  end
{  else
  if (Query.Implementor is TBoldFireDACExecQuery) then
  begin
    lBoldFireDACExecQuery := Query.Implementor as TBoldFireDACExecQuery;
    lBoldFireDACExecQuery.clear;
    Query := nil;
    if not Assigned(fCachedExecQuery1) then
      fCachedExecQuery1 := lBoldFireDACExecQuery
    else
      lBoldFireDACExecQuery.free;
  end
}
end;

procedure TBoldFireDACConnection.ReleaseTable(var Table: IBoldTable);
var
  lBoldFireDACTable: TBoldFireDACTable;
begin
  if Table.Implementor is TBoldFireDACTable then
  begin
    lBoldFireDACTable := Table.Implementor as TBoldFireDACTable;
    Table := nil;
    if not Assigned(fCachedTable) then
      fCachedTable := lBoldFireDACTable
    else
      lBoldFireDACTable.free;
  end;
end;

function TBoldFireDACConnection.SupportsTableCreation: Boolean;
begin
  Result := False;
end;

{ TBoldFireDACParameter }

procedure TBoldFireDACParameter.Clear;
begin
  FDParam.Clear;
end;

constructor TBoldFireDACParameter.Create(FireDACParameter: TFireDacParam; DatasetWrapper: TBoldAbstractQueryWrapper);
begin
  inherited Create(DatasetWrapper);
  fFDParam := FireDACParameter;
end;

function TBoldFireDACParameter.GetAsAnsiString: TBoldAnsiString;
begin
  Result := FDParam.AsAnsiString;
end;

function TBoldFireDACParameter.GetAsBCD: Currency;
begin
  Result := FDParam.AsBCD;
end;

function TBoldFireDACParameter.GetAsblob: TBoldBlobData;
begin
  Result := AnsiString(FDParam.Value);
end;

function TBoldFireDACParameter.GetAsBoolean: Boolean;
begin
  Result := FDParam.AsBoolean;
end;

function TBoldFireDACParameter.GetAsCurrency: Currency;
begin
  Result := FDParam.AsCurrency;
end;

function TBoldFireDACParameter.GetAsDateTime: TDateTime;
begin
  Result := FDParam.AsDateTime;
end;

function TBoldFireDACParameter.GetAsFloat: Double;
begin
  Result := FDParam.AsFloat;
end;

function TBoldFireDACParameter.GetAsInt64: Int64;
begin
  result := FDParam.AsLargeInt;
end;

function TBoldFireDACParameter.GetAsInteger: Longint;
begin
  Result := FDParam.AsInteger;
end;

function TBoldFireDACParameter.GetAsMemo: string;
begin
  Result := String(FDParam.AsMemo);
end;

function TBoldFireDACParameter.GetAsString: string;
begin
  Result := FDParam.AsString;
  if Result = DatasetWrapper.DatabaseWrapper.SQLDataBaseConfig.EmptyStringMarker then
  begin
    Result := '';
  end;
end;

function TBoldFireDACParameter.GetAsVariant: Variant;
begin
  Result := FDParam.Value;
end;

function TBoldFireDACParameter.GetAsWideString: WideString;
begin
  Result := FDParam.AsWideString;
  if Result = DatasetWrapper.DatabaseWrapper.SQLDataBaseConfig.EmptyStringMarker then
  begin
    Result := '';
  end;
end;

function TBoldFireDACParameter.GetDataType: TFieldType;
begin
  Result := FDParam.DataType;
end;

function TBoldFireDACParameter.GetIsNull: Boolean;
begin
  Result := VarIsNull(FDParam.Value)
end;

function TBoldFireDACParameter.GetName: string;
begin
  Result := FDParam.Name;
end;

function TBoldFireDACParameter.GetFDParam: TFireDacParam;
begin
  Result := fFDParam;
end;

procedure TBoldFireDACParameter.SetAsAnsiString(const Value: TBoldAnsiString);
begin
  FDParam.AsAnsiString := Value;
end;

procedure TBoldFireDACParameter.SetAsBCD(const Value: Currency);
begin
  FDParam.Value := Value;
end;

procedure TBoldFireDACParameter.SetAsBlob(const Value: TBoldBlobData);
begin
  if FDParam.DataType = ftUnknown then
  begin
    FDParam.DataType := ftBlob;
  end;
  if Value = '' then
  begin
//    FDParam.Value := DatasetWrapper.DatabaseWrapper.SQLDataBaseConfig.EmptyStringMarker;
    FDParam.Value := Null;
  end else
  begin
    FDParam.Value := TBoldBlobData(AnsiString(Value));
  end;
end;

procedure TBoldFireDACParameter.SetAsBoolean(Value: Boolean);
begin
  FDParam.AsBoolean := Value;
end;

procedure TBoldFireDACParameter.SetAsCurrency(const Value: Currency);
begin
  FDParam.AsCurrency := Value;
end;

procedure TBoldFireDACParameter.SetAsDate(const Value: TDateTime);
begin
  FDParam.AsDate := Value;
end;

procedure TBoldFireDACParameter.SetAsDateTime(const Value: TDateTime);
begin
  FDParam.AsDateTime := Value;
end;

procedure TBoldFireDACParameter.SetAsFloat(const Value: Double);
begin
  FDParam.AsFloat := Value;
end;

procedure TBoldFireDACParameter.SetAsInt64(const Value: Int64);
begin
  FDParam.AsLargeInt := Value;
end;

procedure TBoldFireDACParameter.SetAsInteger(Value: Integer);
begin
  FDParam.AsInteger := Value;
end;

procedure TBoldFireDACParameter.SetAsMemo(const Value: string);
begin
  FDParam.AsMemo := AnsiString(Value);
end;

procedure TBoldFireDACParameter.SetAsSmallInt(Value: Integer);
begin
  FDParam.AsSmallInt := Value;
end;

procedure TBoldFireDACParameter.SetAsString(const Value: string);
begin
  if Value = '' then
  begin
    FDParam.AsString := DatasetWrapper.DatabaseWrapper.SQLDataBaseConfig.EmptyStringMarker;
  end else
  begin
    FDParam.AsString := Value;
  end;
end;

procedure TBoldFireDACParameter.SetAsTime(const Value: TDateTime);
begin
  FDParam.AsTime := Value;
end;

procedure TBoldFireDACParameter.SetAsVariant(const NewValue: Variant);
begin
  FDParam.Value := NewValue;
end;

procedure TBoldFireDACParameter.SetAsWideString(const Value: Widestring);
begin
  if Value = '' then
  begin
    FDParam.AsString := DatasetWrapper.DatabaseWrapper.SQLDataBaseConfig.EmptyStringMarker;
  end else
  begin
    FDParam.AsWideString := Value;
  end;
end;

procedure TBoldFireDACParameter.SetAsWord(Value: Integer);
begin
  FDParam.AsWord := Value;
end;

procedure TBoldFireDACParameter.SetDataType(Value: TFieldType);
begin
  FDParam.DataType := Value;
end;

procedure TBoldFireDACParameter.SetText(const Value: string);
begin
  FDParam.Value := Value;
end;

procedure TBoldFireDACParameter.Assign(const source: IBoldParameter);
begin
  FDParam.Value := Source.AsVariant;
end;

procedure TBoldFireDACParameter.AssignFieldValue(const source: IBoldField);
begin
  FDParam.Assign(source.Field);
end;

procedure TBoldFireDACConnection.ReleaseCachedObjects;
begin
  FreeAndNil(fCachedTable);
  FreeAndNil(fCachedQuery1);
  FreeAndNil(fCachedQuery2);
  FreeAndNil(fCachedExecQuery1);
end;

{ TBoldFireDACExecQuery }

procedure TBoldFireDACExecQuery.AssignParams(Sourceparams: TParams);
var
  lIndexSourceParams: Integer;
  lFDParam: TFireDacParam;
begin
  ExecQuery.Params.Clear;
  if Assigned(Sourceparams) and (Sourceparams.Count > 0) then
  begin
    for lIndexSourceParams := 0 to Sourceparams.Count - 1 do
    begin
      lFDParam := ExecQuery.Params.CreateParam(Sourceparams[lIndexSourceParams].DataType, Sourceparams[lIndexSourceParams].Name, Sourceparams[lIndexSourceParams].ParamType) as TFireDacParam;
      lFDParam.Value := Sourceparams[lIndexSourceParams].Value;
    end;
  end;
end;

procedure TBoldFireDACExecQuery.AssignSQL(SQL: TStrings);
begin
  ExecQuery.SQL.BeginUpdate;
  ExecQuery.SQL.Assign(SQL);
  ExecQuery.SQL.EndUpdate;
end;

procedure TBoldFireDACExecQuery.AssignSQLText(const SQL: string);
var
  lStringList: TStringList;
  lGuard: IBoldGuard;
begin
  lGuard := TBoldGuard.Create(lStringList);
  lStringList := TStringList.Create;
  lStringList.Add(SQL);
  AssignSQL(lStringList);
end;

procedure TBoldFireDACExecQuery.BeginExecuteQuery;
begin
  (DatabaseWrapper as TBoldFireDACConnection).BeginExecuteQuery;
end;

procedure TBoldFireDACExecQuery.Clear;
begin
  inherited;
  AssignSQLText('');
  ClearParams;
end;

procedure TBoldFireDACExecQuery.ClearParams;
begin
  ExecQuery.Params.Clear;
end;

constructor TBoldFireDACExecQuery.Create(BoldFireDACConnection: TBoldFireDACConnection);
begin
  inherited Create(BoldFireDACConnection);
  fUseReadTransactions := true;
end;

function TBoldFireDACExecQuery.Createparam(FldType: TFieldType;
  const ParamName: string): IBoldParameter;
begin
  result := CreateParam(FldType, ParamName, ptUnknown, 0);
end;

function TBoldFireDACExecQuery.CreateParam(FldType: TFieldType; const ParamName: string; ParamType: TParamType; Size: integer): IBoldParameter;
var
  lFDParam: TFireDacParam;
begin
  lFDParam := ExecQuery.Params.CreateParam(FldType, ParamName, ptUnknown) as TFireDacParam;
  lFDParam.Size := Size;
  lFDParam.Value := NULL;
  Result := TBoldFireDACParameter.Create(lFDParam, Self);
end;

destructor TBoldFireDACExecQuery.Destroy;
begin
  FreeAndNil(fExecQuery);
  inherited;
end;

procedure TBoldFireDACExecQuery.EndExecuteQuery;
begin
  (DatabaseWrapper as TBoldFireDACConnection).EndExecuteQuery;
end;

function TBoldFireDACExecQuery.EnsureParamByName(
  const Value: string): IBoldParameter;
var
  lFDParam: TFireDacParam;
begin
  lFDParam := ExecQuery.Params.FindParam(Value);
  if not Assigned(lFDParam) then
    lFDParam := ExecQuery.Params.CreateParam(ftUnknown, Value, ptUnknown) as TFireDacParam;
  Result := TBoldFireDACParameter.Create(lFDParam, Self)
end;

procedure TBoldFireDACExecQuery.ExecSQL;
var
  Retries: Integer;
  Done: Boolean;
begin
  BeginExecuteQuery;
  try
  BoldLogSQLWithParams(ExecQuery.SQL, self);
  Retries := 0;
  Done := false;
  while not Done do
  begin
    try
      if (DatabaseWrapper as TBoldFireDACConnection).GetInTransaction then
        fReadTransactionStarted := false
      else
      begin
        if fUseReadTransactions then
        (DatabaseWrapper as TBoldFireDACConnection).StartReadTransaction;
        fReadTransactionStarted := fUseReadTransactions;
      end;
      ExecQuery.Execute;
      if fReadTransactionStarted and  (DatabaseWrapper as TBoldFireDACConnection).GetInTransaction then
      begin
       (DatabaseWrapper as TBoldFireDACConnection).Commit;
       fReadTransactionStarted := false;
      end;
      Done := true;
    except
      on e: Exception do
      begin
        if (not fReadTransactionStarted) or (Retries > 4) then
          raise TBoldFireDACConnection(DatabaseWrapper).GetDatabaseError(E, ExecQuery.SQL.Text);
        if (DatabaseWrapper as TBoldFireDACConnection).GetInTransaction then
          (DatabaseWrapper as TBoldFireDACConnection).Rollback;
        fReadTransactionStarted := false;
        INC(Retries);
        sleep(Retries*200);
      end;
    end;
  end;
  finally
    EndExecuteQuery;
  end;
end;

procedure TBoldFireDACExecQuery.StartSQLBatch;
begin
  raise EBold.CreateFmt('MethodNotImplemented', [ClassName, 'StartSQLBatch']); // do not localize
end;

procedure TBoldFireDACExecQuery.EndSQLBatch;
begin
  raise EBold.CreateFmt('MethodNotImplemented', [ClassName, 'EndSQLBatch']); // do not localize
end;

procedure TBoldFireDACExecQuery.FailSQLBatch;
begin
  raise EBold.CreateFmt('MethodNotImplemented', [ClassName, 'FailSQLBatch']); // do not localize
end;

function TBoldFireDACExecQuery.FindParam(const Value: string): IBoldParameter;
var
  Param: TFDParam;
begin
  Param := ExecQuery.FindParam(Value);
  if not Assigned(Param) then
    result := CreateParam(ftUnknown, Value);
end;

function TBoldFireDACExecQuery.GetBatchQueryParamCount: integer;
begin
  result := 0; // update when batch support is implemented
end;

function TBoldFireDACExecQuery.GetExecQuery: TFDQuery;
begin
  if not Assigned(fExecQuery) then
  begin
    fExecQuery := TFDQuery.Create(nil);
    fExecQuery.Connection := (DatabaseWrapper as TBoldFireDACConnection).FDConnection;
  end;
  Result := fExecQuery;
end;

function TBoldFireDACExecQuery.GetParamCheck: Boolean;
begin
  result := ExecQuery.ResourceOptions.ParamCreate;
end;

function TBoldFireDACExecQuery.GetParamCount: Integer;
begin
  result := ExecQuery.Params.Count;
end;

function TBoldFireDACExecQuery.GetParams: TParams;
begin
  result := TFDAdaptedDataSetAccess(ExecQuery).fVclParams;
end;

function TBoldFireDACExecQuery.GetParam(i: Integer): IBoldParameter;
begin
  Result := TBoldFireDACParameter.Create(ExecQuery.Params[i], Self);
end;

function TBoldFireDACExecQuery.GetRowsAffected: Integer;
begin
  Result := ExecQuery.RowsAffected;
end;

function TBoldFireDACExecQuery.GetSQLStrings: TStrings;
begin
  result := ExecQuery.SQL;
end;

function TBoldFireDACExecQuery.GetSQLText: string;
begin
  Result := ExecQuery.SQL.Text;
end;

function TBoldFireDACExecQuery.GetUseReadTransactions: boolean;
begin
  result := fUseReadTransactions;
end;

function TBoldFireDACExecQuery.ParamByName(const Value: string): IBoldParameter;
var
  lFDParam: TFireDacParam;
begin
  lFDParam := ExecQuery.Params.ParamByName(Value);
  if Assigned(lFDParam) then
  begin
    Result := TBoldFireDACParameter.Create(lFDParam, Self)
  end else
  begin
    Result := nil;
  end;
end;

procedure TBoldFireDACExecQuery.Prepare;
begin
  ExecQuery.Prepare;
end;

procedure TBoldFireDACExecQuery.SetParamCheck(value: Boolean);
begin
  ExecQuery.ResourceOptions.ParamCreate := Value;
end;

procedure TBoldFireDACExecQuery.SetUseReadTransactions(value: boolean);
begin
  fUseReadTransactions := value;
end;

end.
