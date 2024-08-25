
{ Global compiler directives }
{$include bold.inc}
unit BoldADOInterfaces;

interface

uses
  Classes,
  Db,
  ADODB,
  BoldDefs,
  BoldSQLDatabaseConfig,
  BoldDBInterfaces;

type
  { forward declarations }
  TBoldADOParameter = class;
  TBoldADOQuery = class;
  TBoldADOTable = class;
  TBoldADOConnection = class;
  TBoldADOQueryClass = class of TBoldADOQuery;

  { TBoldADOParameter }
  TBoldADOParameter = class(TBoldParameterWrapper, IBoldParameter)
  private
    fParameter: TParameter;
    function GetAsVariant: Variant;
    procedure SetAsVariant(const NewValue: Variant);
    function GetName: String;
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
    function GetAsMemo: string;
    function GetAsString: string;
    function GetAsInt64: Int64;
    function GetIsNull: Boolean;
    procedure SetAsBCD(const Value: Currency);
    procedure SetAsBlob(const Value: TBoldBlobData);
    procedure SetAsBoolean(Value: Boolean);
    procedure SetAsCurrency(const Value: Currency);
    procedure SetAsDate(const Value: TDateTime);
    procedure SetAsDateTime(const Value: TDateTime);
    procedure SetAsFloat(const Value: Double);
    procedure SetAsInteger(Value: Longint);
    procedure SetAsMemo(const Value: string);
    procedure SetAsString(const Value: string);
    procedure SetAsSmallInt(Value: LongInt);
    procedure SetAsTime(const Value: TDateTime);
    procedure SetAsWord(Value: LongInt);
    procedure SetAsInt64(const Value: Int64);
    procedure SetText(const Value: string);
    function GetParameter: TParameter;
    procedure AssignFieldValue(source: IBoldField);
    function GetAsAnsiString: TBoldAnsiString;
    function GetAsWideString: WideString;
    procedure SetAsAnsiString(const Value: TBoldAnsiString);
    procedure SetAsWideString(const Value: Widestring);
    property Parameter: TParameter read GetParameter;
  public
    constructor create(AdoParameter: TParameter; DatasetWrapper: TBoldDatasetWrapper);
  end;

  { TBoldADOQuery }
  TBoldADOQuery = class(TBoldDataSetWrapper, IBoldQuery, IBoldExecQuery, IBoldParameterized)
  private
    fQuery: TADOQuery;
    function GetQuery: TADOQuery;
    procedure AssignParams(Sourceparams: TParams);
    function GetParamCount: integer;
    function GetParams(i: integer): IBoldParameter;
    function GetRequestLiveQuery: Boolean;
    function ParamByName(const Value: string): IBoldParameter;
    function Createparam(FldType: TFieldType; const ParamName: string; ParamType: TParamType; Size: integer): IBoldParameter;
    procedure SetRequestLiveQuery(NewValue: Boolean);
    function GetSQLText: String;
    procedure AssignSQL(SQL: TStrings); virtual;
    procedure AssignSQLText(const SQL: String);
    function GetRowsAffected: integer;
    function GetRecordCount: integer;
    function GetParamCheck: Boolean;
    procedure SetParamCheck(value: Boolean);
    function GetUseReadTransactions: boolean;
    procedure SetUseReadTransactions(value: boolean);
  protected
    procedure StartSQLBatch; virtual;
    procedure EndSQLBatch; virtual;
    procedure FailSQLBatch; virtual;
    function GetDataSet: TDataSet; override;
    procedure ClearParams;
    procedure ExecSQL; virtual;
    procedure Open; override;
    property Query: TADOQuery read GetQuery;
  public
    constructor Create(Query: TADOQuery; DatabaseWrapper: TBoldDatabaseWrapper);
  end;

  { TBoldADOTable }
  TBoldADOTable = class(TBoldDataSetWrapper, IBoldTable)
  private
    fTable: TADOTable;
    function GetTable: TADOTable;
    property Table: TADOTable read GetTable;
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
  public
    constructor Create(Table: TADOTable; DatabaseWrapper: TBoldDatabaseWrapper);
  end;

  { TBoldADOConnection }
  TBoldADOConnection = class(TBoldDatabaseWrapper, IBoldDataBase)
    fDataBase: TADOConnection;
    fCachedTable: TADOTable;
    fCachedQuery: TADOQuery;
    function GetDataBase: TADOConnection;
    property DataBase: TADOConnection read GetDataBase;
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

    function GetIsExecutingQuery: Boolean;
    function SupportsTableCreation: Boolean;
    procedure ReleaseCachedObjects;
  protected
    procedure AllTableNames(Pattern: String; ShowSystemTables: Boolean; TableNameList: TStrings); override;
    function GetQuery: IBoldQuery; override;
    procedure ReleaseQuery(var Query: IBoldQuery); override;
    function GetTable: IBoldTable; override;
    procedure ReleaseTable(var Table: IBoldTable); override;
  public
    constructor create(DataBase: TADOConnection; SQLDataBaseConfig: TBoldSQLDatabaseConfig);
    destructor destroy; override;
  end;

var
  BoldADOQueryClass: TBoldADOQueryClass = TBoldADOQuery;

implementation

uses
  BoldUtils,
  SysUtils,
  Variants,
  Masks,
  BoldGuard;

{ TBoldADOQuery }

procedure TBoldADOQuery.AssignParams(SourceParams: TParams);
var
  i: integer;
begin
  Query.Parameters.Clear;
  if assigned(SourceParams) and (SourceParams.Count > 0) then
  begin
    for i := 0 to Sourceparams.Count - 1 do
      Query.Parameters.CreateParameter(Sourceparams[i].Name, Sourceparams[i].DataType, pdInput, 0, SourceParams[i].Value);
  end;
end;

procedure TBoldADOQuery.AssignSQL(SQL: TStrings);
var
  OldConnection: TADOConnection;
  BackupQuery: TADOQuery;
  Guard: IBoldGuard;
begin
  Guard := TBoldGuard.Create(BackupQuery);
  OldConnection := Query.Connection;
  if Query.Parameters.count > 0 then
  begin
    BackupQuery := TADOQuery.Create(nil);
    BackupQuery.Parameters.Assign(Query.Parameters);
  end;

  Query.Connection := nil;
  Query.SQL.BeginUpdate;
  Query.SQL.Assign(SQL);
  Query.SQL.EndUpdate;
  Query.Parameters.ParseSQL(SQL.Text, true);
  if assigned(BackupQuery) then
    Query.Parameters.AssignValues(BackupQuery.Parameters);
  Query.Connection := OldConnection;
end;

procedure TBoldADOQuery.AssignSQLText(const SQL: String);
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

procedure TBoldADOQuery.ClearParams;
begin
  Query.Parameters.Clear;
end;

constructor TBoldADOQuery.Create(Query: TADOQuery; DatabaseWrapper: TBoldDatabaseWrapper);
begin
  inherited create(DatabaseWrapper);
  fQuery := Query;
  SetParamCheck(true);
end;

function TBoldADOQuery.Createparam(FldType: TFieldType; const ParamName: string; ParamType: TParamType; Size: integer): IBoldParameter;
const
  ParamDir: Array[TparamType] of TParameterDirection = (pdUnknown, pdInput, pdOutput, pdInputOutput, pdReturnValue);
begin
  result := TBoldADOParameter.Create(Query.Parameters.CreateParameter(ParamName, fldType, ParamDir[ParamType], Size, null), self);
end;

procedure TBoldADOQuery.EndSQLBatch;
begin
end;

procedure TBoldADOQuery.ExecSQL;
begin
{$IFDEF BOLD_DELPHI10_OR_LATER}
  BoldLogSQLWide(Query.SQL, self);
{$ELSE}
  BoldLogSQL(Query.SQL);
{$ENDIF}
  try
    Query.ExecSQL;
  except
    on e: Exception do
    begin
      e.Message := e.Message + BOLDCRLF + 'SQL: ' + Query.SQL.text;
      raise;
    end;
  end;
end;

procedure TBoldADOQuery.FailSQLBatch;
begin
end;

function TBoldADOQuery.GetDataSet: TDataSet;
begin
  result := Query;
end;

function TBoldADOQuery.GetParamCheck: Boolean;
begin
  Result := Query.ParamCheck;
end;

function TBoldADOQuery.GetParamCount: integer;
begin
  result := Query.Parameters.count;
end;

function TBoldADOQuery.GetParams(i: integer): IBoldParameter;
begin
  result := TBoldADOParameter.Create(Query.parameters[i], self);
end;

function TBoldADOQuery.GetQuery: TADOQuery;
begin
  result := fQuery;
end;

function TBoldADOQuery.GetRecordCount: integer;
begin
  result := Query.RecordCount;
end;

function TBoldADOQuery.GetRequestLiveQuery: Boolean;
begin
  result := false;
end;

function TBoldADOQuery.GetRowsAffected: integer;
begin
  result := Query.RowsAffected;
end;

function TBoldADOQuery.GetSQLText: String;
begin
  result := Query.SQL.Text;
end;

function TBoldADOQuery.GetUseReadTransactions: boolean;
begin
  result := false;
end;

procedure TBoldADOQuery.Open;
begin
{$IFDEF BOLD_DELPHI10_OR_LATER}
  BoldLogSQLWide(Query.SQL, self);
{$ELSE}
  BoldLogSQL(Query.SQL);
{$ENDIF}
  try
    inherited;
  except
    on e: Exception do
    begin
      e.Message := e.Message + BOLDCRLF + 'SQL: ' + Query.SQL.text;
      raise;
    end;
  end;
end;

function TBoldADOQuery.ParamByName(const Value: string): IBoldParameter;
var
  Param: TParameter;
begin
  Param := Query.Parameters.ParamByName(Value);
  if assigned(Param) then
    result := TBoldADOParameter.Create(Param, self)
  else
    result := nil;
end;

procedure TBoldADOQuery.SetParamCheck(value: Boolean);
begin
  Query.ParamCheck := Value;
end;

procedure TBoldADOQuery.SetRequestLiveQuery(NewValue: Boolean);
begin
end;

procedure TBoldADOQuery.SetUseReadTransactions(value: boolean);
begin

end;

procedure TBoldADOQuery.StartSQLBatch;
begin
end;

{ TBoldADOTable }

procedure TBoldADOTable.AddIndex(const Name, Fields: string;
  Options: TIndexOptions; const DescFields: string);
begin
  raise EBold.CreateFmt('%s.AddIndex: not implemented', [ClassName]);
end;

constructor TBoldADOTable.Create(Table: TADOTable; DatabaseWrapper: TBoldDatabaseWrapper);
begin
  inherited create(DatabaseWrapper);
  fTable := Table;
end;

procedure TBoldADOTable.CreateTable;
begin
  raise EBold.CreateFmt('%s.CreateTable: not implemented', [ClassName]);
end;

procedure TBoldADOTable.DeleteTable;
begin
  raise EBold.CreateFmt('%s.DeleteTable: not implemented', [ClassName]);
end;

function TBoldADOTable.GetDataSet: TDataSet;
begin
  result := fTable;
end;

function TBoldADOTable.GetExclusive: Boolean;
begin
  result := false;
end;

function TBoldADOTable.GetExists: Boolean;
var
  AllTables: TStringList;
  Guard: IBoldGuard;
begin
  Guard := TBoldGuard.Create(AllTables);
  Result := False;
  if Assigned(Table) and Assigned(Table.Connection) then
  begin
    Alltables := TStringList.Create;
    Table.Connection.GetTableNames(AllTables);
    Result := AllTables.IndexOf(GetTableName) <> -1;
  end;
end;

function TBoldADOTable.GetIndexDefs: TIndexDefs;
begin
  result := Table.IndexDefs;
end;

function TBoldADOTable.GetTable: TADOTable;
begin
  result := fTable;
end;

function TBoldADOTable.GetTableName: String;
begin
  result := Table.TableName;
end;

procedure TBoldADOTable.SetExclusive(NewValue: Boolean);
begin
  if NewValue then
    Table.LockType := ltPessimistic
  else
    Table.LockType := ltOptimistic;
end;

procedure TBoldADOTable.SetTableName(const NewName: String);
begin
  Table.TableName := NewName;
end;

{ TBoldADOConnection }
procedure TBoldADOConnection.AllTableNames(Pattern: String; ShowSystemTables: Boolean; TableNameList: TStrings);
var
  TempList: TStringList;
  i: integer;
  TempPattern: String;
  Guard: IBoldGuard;
begin
  Guard := TBoldGuard.Create(TempList);
  TempList := TStringList.Create;
  if Pattern = '' then
    TempPattern := '*'
  else
    TempPattern := Pattern;


  GetDataBase.GetTableNames(TempList, ShowSystemTables);


  for i := 0 to TempList.Count-1 do
    if MatchesMask(TempList[i], tempPattern) then
      TableNameList.Add(TempList[i]);
end;



procedure TBoldADOConnection.Commit;
begin
  DataBase.CommitTrans;
end;

function TBoldADOConnection.GetInTransaction: Boolean;
begin
  result := DataBase.InTransaction;
end;

function TBoldADOConnection.GetIsExecutingQuery: Boolean;
begin
  result := false; // TODO: implement
end;

function TBoldADOConnection.GetIsSQLBased: Boolean;
begin
  result := true;
end;

function TBoldADOConnection.GetKeepConnection: Boolean;
begin
  result := true;
end;

function TBoldADOConnection.GetLogInPrompt: Boolean;
begin
  result := DataBase.LoginPrompt;
end;

procedure TBoldADOConnection.RollBack;
begin
  DataBase.RollBackTrans;
end;

procedure TBoldADOConnection.SetKeepConnection(NewValue: Boolean);
begin
end;

procedure TBoldADOConnection.SetlogInPrompt(NewValue: Boolean);
begin
  DataBase.LoginPrompt := NewValue;
end;

procedure TBoldADOConnection.StartTransaction;
begin
  DataBase.BeginTrans;
end;

destructor TBoldADOConnection.destroy;
begin
  FreeAndNil(fCachedQuery);
  FreeAndNil(fCachedTable);
  inherited;
end;

constructor TBoldADOConnection.create(DataBase: TADOConnection; SQLDataBaseConfig: TBoldSQLDatabaseConfig);
begin
  inherited create(SQLDataBaseConfig);
  fDatabase := DataBase;
end;  

procedure TBoldADOConnection.Close;
begin
  DataBase.Close;
end;

function TBoldADOConnection.GetConnected: Boolean;
begin
  result := DataBase.Connected;
end;

function TBoldADOConnection.GetDataBase: TADOConnection;
begin
  result := fDataBase;
end;

function TBoldADOConnection.GetQuery: IBoldQuery;
var
  Query: TADOQuery;
begin
  if assigned(fCachedQuery) then
  begin
    Query := fCachedQuery;
    fCachedQuery := nil;
  end
  else
  begin
    Query := TADOQuery.Create(nil);
    Query.Connection := DataBase;
  end;
  result := BoldADOQueryClass.Create(Query, self);
end;

function TBoldADOConnection.GetTable: IBoldTable;
var
  Table: TADOTable;
begin
  if assigned(fCachedTable) then
  begin
    Table := fCachedTable;
    fCachedTable := nil;
  end
  else
  begin
    Table := TADOTable.Create(nil);
    Table.Connection := DataBase;
  end;
  result := TBoldADOTable.Create(Table, self);
end;

procedure TBoldADOConnection.Open;
begin
  DataBase.Open;
end;

procedure TBoldADOConnection.ReleaseQuery(var Query: IBoldQuery);
var
  ADOQuery: TBoldADOQuery;
begin
  if Query.Implementor is TBoldADOQuery then
  begin
    ADOQuery := Query.Implementor as TBoldADOQuery;
    Query := nil;
    if not assigned(fCachedQuery) then
    begin
      fCachedQuery := ADOQuery.fQuery;
      if fCachedQuery.Active then
        fCachedQuery.Close;
      fCachedQuery.SQL.Clear;
    end
    else
      ADOQuery.fQuery.free;
    ADOQuery.Free;
  end;
end;

procedure TBoldADOConnection.ReleaseTable(var Table: IBoldTable);
var
  ADOTable: TBoldADOTable;
begin
  if Table.Implementor is TBoldADOTable then
  begin
    ADOTable := Table.Implementor as TBoldADOTable;
    Table := nil;
    if not assigned(fCachedTable) then
    begin
      fCachedTable := ADOTable.fTable;
    end
    else
      ADOTable.fTable.free;
    ADOTable.Free;
  end;
end;

function TBoldADOConnection.SupportsTableCreation: Boolean;
begin
  result := false;
end;

{ TBoldADOParameter }

procedure TBoldADOParameter.Clear;
begin
end;

constructor TBoldADOParameter.create(AdoParameter: TParameter; DatasetWrapper: TBoldDatasetWrapper);
begin
  inherited Create(DatasetWrapper);
  fParameter := AdoParameter;
end;

function TBoldADOParameter.GetAsAnsiString: TBoldAnsiString;
begin
  result := TBoldAnsiString(Parameter.Value);
  if string(result) = DatasetWrapper.DatabaseWrapper.SQLDatabaseConfig.EmptyStringMarker then
    result := '';
end;

function TBoldADOParameter.GetAsBCD: Currency;
begin
  result := parameter.Value;
end;

function TBoldADOParameter.GetAsblob: TBoldBlobData;
begin
  result := TBoldAnsiString(parameter.Value);
end;

function TBoldADOParameter.GetAsBoolean: Boolean;
begin
  result := parameter.Value;
end;

function TBoldADOParameter.GetAsCurrency: Currency;
begin
  result := parameter.Value;
end;

function TBoldADOParameter.GetAsDateTime: TDateTime;
begin
  result := parameter.Value;
end;

function TBoldADOParameter.GetAsFloat: Double;
begin
  result := parameter.Value;
end;

function TBoldADOParameter.GetAsInt64: Int64;
begin
  result := parameter.Value;
end;

function TBoldADOParameter.GetAsInteger: Longint;
begin
  result := parameter.Value;
end;

function TBoldADOParameter.GetAsMemo: string;
begin
  result := parameter.Value;
end;

function TBoldADOParameter.GetAsString: string;
begin
  result := parameter.Value;
  if result = DatasetWrapper.DatabaseWrapper.SQLDatabaseConfig.EmptyStringMarker then
    result := ''; 
end;

function TBoldADOParameter.GetAsVariant: Variant;
begin
  result := parameter.Value;
end;

function TBoldADOParameter.GetAsWideString: WideString;
begin
  result := Parameter.Value;
  if string(result) = DatasetWrapper.DatabaseWrapper.SQLDatabaseConfig.EmptyStringMarker then
    result := '';
end;

function TBoldADOParameter.GetDataType: TFieldType;
begin
  result := parameter.DataType;
end;

function TBoldADOParameter.GetIsNull: Boolean;
begin
  result := VarIsNull(Parameter.value)
end;

function TBoldADOParameter.GetName: String;
begin
  result := Parameter.Name;
end;

function TBoldADOParameter.GetParameter: TParameter;
begin
  result := fParameter;
end;

procedure TBoldADOParameter.SetAsAnsiString(const Value: TBoldAnsiString);
begin
  if value = '' then
    Parameter.Value := DatasetWrapper.DatabaseWrapper.SQLDatabaseConfig.EmptyStringMarker
  else
    Parameter.Value := Value
end;

procedure TBoldADOParameter.SetAsBCD(const Value: Currency);
begin
  Parameter.Value := Value;
end;

procedure TBoldADOParameter.SetAsBlob(const Value: TBlobData);
begin
  if Parameter.DataType = ftUnknown then
    Parameter.DataType := ftBlob;
  if value = '' then
    Parameter.Value := DatasetWrapper.DatabaseWrapper.SQLDatabaseConfig.EmptyStringMarker
  else
    Parameter.Value := Value;
end;

procedure TBoldADOParameter.SetAsBoolean(Value: Boolean);
begin
  Parameter.Value := Value;
end;

procedure TBoldADOParameter.SetAsCurrency(const Value: Currency);
begin
  Parameter.Value := Value;
end;

procedure TBoldADOParameter.SetAsDate(const Value: TDateTime);
begin
  Parameter.Value := Value;
end;

procedure TBoldADOParameter.SetAsDateTime(const Value: TDateTime);
begin
  Parameter.Value := Value;
end;

procedure TBoldADOParameter.SetAsFloat(const Value: Double);
begin
  Parameter.Value := Value;
end;

procedure TBoldADOParameter.SetAsInt64(const Value: Int64);
begin
  Parameter.Value := Value;
end;

procedure TBoldADOParameter.SetAsInteger(Value: Integer);
begin
  Parameter.Value := Value;
end;

procedure TBoldADOParameter.SetAsMemo(const Value: string);
begin
  Parameter.Value := Value;
end;

procedure TBoldADOParameter.SetAsSmallInt(Value: Integer);
begin
  Parameter.Value := Value;
end;

procedure TBoldADOParameter.SetAsString(const Value: string);
begin
  if value = '' then
    Parameter.Value := DatasetWrapper.DatabaseWrapper.SQLDatabaseConfig.EmptyStringMarker
  else
    Parameter.Value := Value
end;

procedure TBoldADOParameter.SetAsTime(const Value: TDateTime);
begin
  Parameter.Value := Value;
end;

procedure TBoldADOParameter.SetAsVariant(const NewValue: Variant);
begin
  Parameter.Value := NewValue;
end;

procedure TBoldADOParameter.SetAsWideString(const Value: Widestring);
begin
  if value = '' then
    Parameter.Value := DatasetWrapper.DatabaseWrapper.SQLDatabaseConfig.EmptyStringMarker
  else
    Parameter.Value := Value
end;

procedure TBoldADOParameter.SetAsWord(Value: Integer);
begin
  Parameter.Value := Value;
end;

procedure TBoldADOParameter.SetDataType(Value: TFieldType);
begin
  Parameter.DataType := Value;
end;

procedure TBoldADOParameter.SetText(const Value: string);
begin
  Parameter.Value := Value;
end;

procedure TBoldADOParameter.AssignFieldValue(source: IBoldField);
begin
  Parameter.Assign(Source.Field);
end;

procedure TBoldADOConnection.Reconnect;
begin
  if Assigned(fDataBase) then begin
    fDataBase.Connected := False;
    fDataBase.Connected := True;
  end;
end;

procedure TBoldADOConnection.ReleaseCachedObjects;
begin
  FreeAndNil(fCachedTable);
  FreeAndNil(fCachedQuery);
end;

end.
