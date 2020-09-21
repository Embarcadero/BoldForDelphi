unit BoldDOAInterfaces;

interface

uses
  Classes,
  Db,
  Contnrs,
  Oracle,
  OracleTypes,
  OracleData,
  Boldbase,
  BoldSQLDatabaseConfig,
  BoldDBInterfaces;

type
  { forward declarations }
  TBoldDOADataBase = class;
  TBoldDOAQuery = class;
  TLobValues = class
  private
     FLOB:TLoblocator;
     FValue:TBlobData;
  public
    constructor Create(aLOB:TLOBLocator;const aValue:TBlobData);
    destructor Destroy; override;
  end;
//  TBoldDOATable = class;


  { TBoldDOAQuery }
  TBoldDOAQuery = class(TBoldDataSetWrapper, IBoldQuery, IBoldParameterized)
  private
    fQuery: TOracleDataSet;
    fLOBList:TObjectList;
    function GetQuery: TOracleDataSet;
    procedure AssignParams(SourceParams: TParams);
    function GetParamCount: integer;
    function GetParams(i: integer): IBoldParameter;
    function GetRequestLiveQuery: Boolean;
    function ParamByName(const Value: string): IBoldParameter;
    procedure SetRequestLiveQuery(NewValue: Boolean);
    function GetSQLText: String;
    procedure AssignSQL(SQL: TStrings);
    procedure AssignSQLText(SQL: String);
    function GetRecordCount: integer;
    function Createparam(FldType: TFieldType; const ParamName: string; ParamType: TParamType; Size: integer): IBoldParameter;
  protected
    function GetDataSet: TDataSet; override;
    procedure ClearParams;
    property Query: TOracleDataSet read GetQuery;
    procedure Open; override;
    procedure BuildVariables;
    procedure Clear;
  public
    constructor Create(Query: TOracleDataSet; DatabaseWrapper: TBoldDatabaseWrapper); virtual;
    destructor Destroy; override;
  end;

  TBoldDOAExecQuery = class(TBoldNonRefCountedObject, IBoldExecQuery, IBoldParameterized)
  private
    fQuery: TOracleQuery;
    fDatabase: TBoldDOADatabase;
    fLOBList:TObjectList;
    procedure ClearParams;
    procedure AssignParams(SourceParams: TParams);
    function ParamByName(const Value: string): IBoldParameter;
    function GetParamCount: integer;
    function GetSQLText: String;
    function GetParams(i:integer): IBoldParameter;
    procedure AssignSQL(SQL: TStrings);
    procedure AssignSQLText(SQL: String);
    procedure StartSQLBatch;
    procedure EndSQLBatch;
    procedure FailSQLBatch;
    procedure ExecSQL;
    function Createparam(FldType: TFieldType; const ParamName: string; ParamType: TParamType; Size: integer): IBoldParameter;
    function GetRowsAffected: integer;
    function GetImplementor: TObject;
    property Query: TOracleQuery read fQuery;
  protected
    procedure BuildVariables;
    procedure Clear;
  public
    constructor Create(Query: TOracleQuery; Database: TBoldDOADatabase);
    destructor Destroy; override;
  end;

  { TBoldADOParameter }
  TBoldDOAParameter = class(TBoldRefCountedObject, IBoldParameter)
  private
    fQuery: TBoldDOAQuery;
    fExecQuery: TBoldDOAExecQuery;
    fParamIndex: integer;
    fParamName: String;
    procedure EnsureParameter(fieldType: TFieldType);
    function GetAsVariant: Variant;
    procedure SetAsVariant(const NewValue: Variant);
    function GetName: String;
    procedure Clear;
    function GetDataType: TFieldType;
    procedure SetDataType(Value: TFieldType);
    function GetAsBCD: Currency;
    function GetAsBoolean: Boolean;
    function GetAsDateTime: TDateTime;
    function GetAsCurrency: Currency;
    function GetAsFloat: Double;
    function GetAsInteger: Longint;
    function GetAsMemo: string;
    function GetAsString: string;
    function GetIsNull: Boolean;
    procedure SetAsBCD(const Value: Currency);
    procedure SetAsBlob(const Value: TBlobData);
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
    procedure SetText(const Value: string);
    procedure AssignFieldValue(source: IBoldField);
    function GetDatabaseWrapper: TBoldDatabaseWrapper;
  public
    constructor create(Query: TBoldDOAQuery; ExecQuery: TBoldDOAExecQuery; ParamIndex: integer; ParamName: String);
  end;

  { TBoldDOADataBase }
  TBoldDOADataBase = class(TBoldDatabaseWrapper, IBoldDataBase)
  private
    fDataBase: TOracleSession;
    fCachedQuery: TOracleDataSet;
    fCachedExecQuery: TOracleQuery;
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
    function GetExecQuery: IBoldExecQuery; override;
    procedure ReleaseExecQuery(var Query: IBoldExecQuery); override;
  public
    constructor Create(DataBase: TOracleSession; SQLDataBaseConfig: TBoldSQLDatabaseConfig);
    destructor Destroy; override;
  end;

implementation

uses
  Variants,
  BoldDefs,
  SysUtils,
  OracleCI,
  BoldUtils,
  DOAConsts;

function FieldTypeToOracleType(FieldType: TFieldType): integer;
begin
  case FieldType of
    ftInteger: Result := otInteger;
    ftString:  Result := otString;
    ftSmallint:  Result := otInteger;
    ftWord:  Result := otInteger;
    ftBoolean: Result := otBoolean;
    ftFloat: Result := otFloat;
    ftCurrency: Result := otFloat;
    ftDate: Result := otDate;
    ftTime: Result := otDate;
    ftDateTime: Result := otDate;
    ftBlob: Result := otBLOB;
    ftMemo: Result := otBLOB;
    ftGraphic: Result := otBLOB;
    ftFmtMemo: Result := otBLOB;
    ftOraBlob: Result := otBLOB;
    ftOraClob: Result := otCLOB;
{
    ftTypedBinary: Result := ot
    ftCursor: Result := ot
    ftFixedChar: Result := ot
    ftWideString: Result := ot
    ftBCD: Result := ot

    ftLargeint: Result := ot
    ftADT: Result := ot
    ftArray: Result := ot
    ftReference: Result := ot
    ftDataSet: Result := ot
    ftVariant: Result := ot
    ftInterface: Result := ot
    ftIDispatch: Result := ot
    ftGuid: Result := ot
    ftBytes: Result := ot
    ftVarBytes: Result := ot
    ftAutoInc: Result := ot
    ftParadoxOle: Result :=
    ftDBaseOle: Result := ot
}
    else
      Result := -1;
  end;
end;

function OracleTypeToFieldType(OracleType: Integer): TFieldType;
begin
  case OracleType of
    otInteger: Result := ftInteger;
    otString: Result := ftString;
    otBoolean: Result := ftBoolean;
    otFloat: Result := ftFloat;
    otNumber: Result := ftCurrency;
    otDate: Result := ftDateTime;
    otBLOB: Result := ftBlob;
    otCLOB: Result := ftOraClob;
    else
      Result := ftUnknown;
  end;
end;

{ TBoldDOAQuery }

procedure TBoldDOAQuery.AssignParams(Sourceparams: tparams);
begin
  if assigned(Sourceparams) then
    Query.Variables.Assign(SourceParams)
  else
    Query.Variables.list.Clear;
end;

function TBoldDOAQuery.GetQuery: TOracleDataSet;
begin
  if not assigned(fQuery) then
    fQuery := TOracleDataSet.Create(nil);
  result := fQuery;
end;

function TBoldDOAQuery.GetParamCount: integer;
begin
  result := Query.Variables.count;
end;

function TBoldDOAQuery.GetParams(I: integer): IBoldParameter;
begin
  result := TBoldDOAParameter.Create(self, nil, i, '');
end;

function TBoldDOAQuery.GetREquestLiveQuery: Boolean;
begin
  // FIXME: Query.RequestLive;
  result := true;
end;

function TBoldDOAQuery.ParamByName(const Value: string): IBoldParameter;
var
  i: integer;
begin
  i := Query.VariableIndex(value);
  if i = -1 then
  begin
    BuildVariables;
    i := Query.VariableIndex(value);
  end;
  result := TBoldDOAParameter.Create(self, nil, i, value)
end;

procedure TBoldDOAQuery.SetRequestLiveQuery(NewValue: Boolean);
begin
  //FIXME:  Query.RequestLive := NewValue;
  ;
end;

function TBoldDOAQuery.GetDataSet: TDataSet;
begin
  result := Query;
end;

constructor TBoldDOAQuery.Create(Query: TOracleDataSet; DatabaseWrapper: TBoldDatabaseWrapper);
begin
  inherited Create(DatabaseWrapper);
  fQuery := Query;
end;

procedure TBoldDOAQuery.Open;
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

procedure TBoldDOAQuery.AssignSQL(SQL: TStrings);
begin
  Query.SQL.BeginUpdate;
  Query.SQL.Assign(SQL);
  Query.SQL.EndUpdate;
end;

procedure TBoldDOAQuery.AssignSQLText(SQL: String);
begin
  Query.SQL.BeginUpdate;
  Query.SQL.Clear;
  BoldAppendToStrings(Query.SQL, SQL, true);
  Query.SQL.EndUpdate;
end;

function TBoldDOAQuery.GetSQLText: String;
begin
  result := Query.SQL.text;
end;

function TBoldDOAQuery.GetRecordCount: integer;
begin
  result := Query.RecordCount;
end;

function TBoldDOAQuery.Createparam(FldType: TFieldType; const ParamName: string; ParamType: TParamType; Size: integer): IBoldParameter;
begin
  Query.DeclareVariable(ParamName, FieldTypeToORacleType(FldType));
  result := ParamByName(ParamName);
end;

procedure TBoldDOAQuery.ClearParams;
begin
  query.DeleteVariables;
end;

procedure TBoldDOAQuery.BuildVariables;
var
  VarList: TStringList;
  i : integer;
begin
  VarList := Oracle.FindVariables(Query.SQL.Text, False);
  for i := 0 to VarList.Count - 1 do Query.DeclareVariable(VarList[i], otString);
end;

procedure TBoldDOAQuery.Clear;
begin
  Query.DeleteVariables;
  FLoblist.Clear;
end;
destructor TBoldDOAQuery.Destroy;
begin
  FLobList.Free;
  inherited;
end;
{ TBoldDOADataBase }

procedure TBoldDOADataBase.AllTableNames(Pattern: String; ShowSystemTables: Boolean; TableNameList: TStrings);
var
  aQuery: TOracleDataset;
  SQL: String;
begin
  aQuery := TOracleDataset.Create(nil);
  aQuery.Session := fDataBase;
  SQL :=
    'select owner, table_name, tablespace_name '+ // do not localize
    'from all_tables '; // do not localize

  if not ShowSystemTables then
    SQL := SQL +
      'where owner <> ''SYSTEM'' and owner <> ''DBSNMP'' and owner <> ''ORDSYS'' and '+ // do not localize
            'owner <> ''OUTLN'' and owner <> ''SYS'' and owner <> ''MDSYS'' and owner <> ''MTSSYS'' '; // do not localize
  SQL := SQL + 'order by owner, table_name, tablespace_name'; // do not localize
  aQuery.SQL.Text := SQL;
  aQuery.Open;
  while not aQuery.Eof do
  begin
    TableNameList.Add(aQuery.Fields[1].AsString);
    aQuery.Next;
  end;
  aQuery.Close;
  aQuery.Free;
end;

function TBoldDOADataBase.GetInTransaction: Boolean;
begin
  result := fDataBase.InTransaction;
end;

function TBoldDOADataBase.GetIsSQLBased: Boolean;
begin
  result := true;
end;

function TBoldDOADataBase.GetKeepConnection: Boolean;
begin
  // FIXME:   result := fDataBase.KeepConnection;
  result := true
end;

function TBoldDOADataBase.GetLogInPrompt: Boolean;
begin
  //FIXME:   result := fDataBase.LoginPrompt;
  result := true;
end;

procedure TBoldDOADataBase.SetKeepConnection(NewValue: Boolean);
begin
  // FIXME:   fDataBase.KeepConnection := NewValue;
end;

procedure TBoldDOADataBase.SetlogInPrompt(NewValue: Boolean);
begin
  // FIXME: fDataBase.LoginPrompt := NewValue;
end;

constructor TBoldDOADataBase.create(DataBase: TOracleSession; SQLDataBaseConfig: TBoldSQLDatabaseConfig);
begin
  inherited Create(SQLDataBaseConfig);
  fDataBase := DataBase;
end;

function TBoldDOADataBase.GetConnected: Boolean;
begin
  result := fDataBase.Connected;
end;

procedure TBoldDOADataBase.StartTransaction;
var
  TransactionMode: TTransactionMode;
begin
  case fDatabase.IsolationLevel of
    ilUnchanged: TransactionMode := tmReadCommitted;
    ilReadCommitted: TransactionMode := tmReadCommitted;
    ilSerializable: TransactionMode := tmSerializable;
    else
      TransactionMode := tmReadCommitted;
  end;

  fDataBase.SetTransaction(TransactionMode);
end;

procedure TBoldDOADataBase.Commit;
begin
  fDatabase.Commit;
end;

procedure TBoldDOADataBase.RollBack;
begin
  fDataBase.Rollback;
end;

procedure TBoldDOADataBase.Open;
begin
  fDataBase.Connected := true;
end;

procedure TBoldDOADataBase.Close;
begin
  fDataBase.Connected := false;
end;

destructor TBoldDOADataBase.destroy;
begin
  inherited;
  fDatabase := nil;
//  FreeAndNil(fCachedTable);
  FreeAndNil(fCachedQuery);
  FreeAndNil(fCachedExecQuery);
end;

function TBoldDOADataBase.GetQuery: IBoldQuery;
var
  Query: TOracleDataSet;
begin
  if assigned(fCachedQuery) then
  begin
    Query := fCachedQuery;
    fCachedQuery := nil;
  end
  else
  begin
    Query := TOracleDataSet.Create(nil);
    Query.Session := fDatabase;
  end;
  result := TBoldDOAQuery.Create(Query, self);
end;

function TBoldDOADataBase.GetTable: IBoldTable;
begin
  raise EBold.CreateFmt(sIBoldTablesNotSupported, [classname, 'GetTable']); // do not localize
end;
{
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
  result := TBoldDOATable.Create(Table, self);
end;
}
procedure TBoldDOADataBase.ReleaseQuery(var Query: IBoldQuery);
var
  DOAQuery: TBoldDOAQuery;
begin
  if Query.Implementor is TBoldDOAQuery then
  begin
    DOAQuery := Query.Implementor as TBoldDOAQuery;
    Query := nil;
    DOAQuery.ClearParams;
    if not assigned(fCachedQuery) then
    begin
      fCachedQuery := DOAQuery.fQuery;
      if fCachedQuery.Active then
        fCachedQuery.Close;
      fCachedQuery.SQL.Clear;
    end
    else
      DOAQuery.fQuery.free;
    DOAQuery.Free;
  end;
end;

procedure TBoldDOADataBase.ReleaseTable(var Table: IBoldTable);
begin
  raise EBold.CreateFmt(sIBoldTablesNotSupported, [classname, 'ReleaseTable']); // do not localize
end;
{
var
  DOATable: TBoldDOATable;
begin
  if Table.Implementor is TBoldDOATable then
  begin
    DOATable := Table.Implementor as TBoldDOATable;
    Table := nil;
    if not assigned(fCachedTable) then
      fCachedTable := DOATable.fTable
    else
      DOATable.fTable.free;
    DOATable.Free;
  end;
end;
}

function TBoldDOADataBase.SupportsTableCreation: Boolean;
begin
  result := False;
end;

procedure TBoldDOADataBase.ReleaseCachedObjects;
begin
//  FreeAndNil(fCachedTable);
  FreeAndNil(fCachedQuery);
  FreeAndNil(fCachedExecQuery);
end;

function TBoldDOADataBase.GetExecQuery: IBoldExecQuery;
var
  Query: TOracleQuery;
begin
  if assigned(fCachedExecQuery) then
  begin
    Query := fCachedExecQuery;
    fCachedExecQuery := nil;
  end
  else
  begin
    Query := TOracleQuery.Create(nil);
    Query.Session := fDatabase;
  end;
  result := TBoldDOAExecQuery.Create(Query, self);
end;

procedure TBoldDOADataBase.ReleaseExecQuery(var Query: IBoldExecQuery);
var
  DOAExecQuery: TBoldDOAExecQuery;
begin
  if Query.Implementor is TBoldDOAExecQuery then
  begin
    DOAExecQuery := Query.Implementor as TBoldDOAExecQuery;
    Query := nil;
    if not assigned(fCachedExecQuery) then
    begin
      fCachedExecQuery := DOAExecQuery.fQuery;
      fCachedExecQuery.SQL.Clear;
    end
    else
      DOAExecQuery.fQuery.free;
    DOAExecQuery.Free;
  end;
end;

{ TBoldDOAParameter }

procedure TBoldDOAParameter.AssignFieldValue(source: IBoldField);
begin
  EnsureParameter(Source.Field.DataType);
  SetAsVariant(Source.AsVariant);
end;

procedure TBoldDOAParameter.Clear;
begin
  SetAsVariant(NULL);
end;

constructor TBoldDOAParameter.create(Query: TBoldDOAQuery; ExecQuery: TBoldDOAExecQuery; ParamIndex: integer; ParamName: String);
begin
  inherited create;
  fQuery := Query;
  fExecQuery := ExecQuery;
  assert(assigned(fQuery) xor assigned(fExecQuery), 'Exactly one of Query and ExecQuery must be assigned');
  fParamIndex := ParamIndex;
  fParamName := ParamName;
end;

procedure TBoldDOAParameter.EnsureParameter(fieldType: TFieldType);
begin
  if fieldType <> ftUnknown then
  begin
    if assigned(fQuery) then
    begin
      FQuery.Query.DeleteVariable(fParamName);
      fQuery.Query.DeclareVariable(fParamName, FieldTypeToOracleType(FieldType));
      fParamIndex := fQuery.Query.VariableIndex(fParamName);
    end
    else
    begin
      fExecQuery.Query.DeleteVariable(fParamName);
      fExecQuery.Query.DeclareVariable(fParamName, FieldTypeToOracleType(FieldType));
      fParamIndex := fExecQuery.Query.VariableIndex(fParamName);
    end;
  end;
end;

function TBoldDOAParameter.GetAsBCD: Currency;
begin
  result := GetAsVariant;
end;

function TBoldDOAParameter.GetAsBoolean: Boolean;
begin
  result := GetAsVariant;
end;

function TBoldDOAParameter.GetAsCurrency: Currency;
begin
  result := GetAsVariant;
end;

function TBoldDOAParameter.GetAsDateTime: TDateTime;
begin
  result := GetAsVariant;
end;

function TBoldDOAParameter.GetAsFloat: Double;
begin
  result := GetAsVariant;
end;

function TBoldDOAParameter.GetAsInteger: Longint;
begin
  result := GetAsVariant;
end;

function TBoldDOAParameter.GetAsMemo: string;
begin
  result := GetAsVariant;
end;

function TBoldDOAParameter.GetAsString: string;
begin
  result := GetAsVariant;
  if result = GetDatabaseWrapper.SQLDatabaseConfig.EmptyStringMarker then
    result := '';
end;

function TBoldDOAParameter.GetDatabaseWrapper: TBoldDatabaseWrapper;
begin
  if assigned(fQuery) then
    result := fQuery.DatabaseWrapper
  else
    result := fExecQuery.fDatabase;
end;


function TBoldDOAParameter.GetAsVariant: Variant;
begin
  if assigned(fQuery) then
    result := fQuery.Query.GetVariable(fParamIndex)
  else
    result := fExecQuery.Query.GetVariable(fParamIndex);
end;

function TBoldDOAParameter.GetDataType: TFieldType;
var
  OracleType: integer;
begin
  if assigned(fQuery) then
    OracleType := fQuery.Query.VariableType(fParamIndex)
  else
    OracleType := fExecQuery.Query.VariableType(fParamIndex);
  result := OracleTypeToFieldType(OracleType);
end;

function TBoldDOAParameter.GetIsNull: Boolean;
begin
  result := VarIsNull(GetAsVariant.value);
end;

function TBoldDOAParameter.GetName: String;
begin
  if assigned(fQuery) then
    result := fQuery.Query.VariableName(fParamIndex)
  else
    result := fExecQuery.Query.VariableName(fParamIndex);
end;

procedure TBoldDOAParameter.SetAsBCD(const Value: Currency);
begin
  EnsureParameter(ftCurrency);
  SetAsVariant(Value);
end;

procedure TBoldDOAParameter.SetAsBlob(const Value: TBlobData);
var
  LOB:TLOBLocator;
begin
  EnsureParameter(ftBlob);
  if assigned(fQuery) then
  begin
    LOB := TLOBLocator.Create(fQuery.Query.Session,otBlob);
    FQuery.Query.SetComplexVariable(fQuery.Query.VariableName(fParamIndex),LOB);
  end
  else
  begin
    LOB := TLOBLocator.CreateTemporary(fExecQuery.Query.Session,otBlob,True);
    LOB.Write(Value[1],Length(Value));
    fExecQuery.Query.SetComplexVariable(fExecQuery.Query.VariableName(fParamIndex),LOB);
    FExecQuery.fLOBList.Add(TLobValues.Create(LOB,value))
  end;
end;

procedure TBoldDOAParameter.SetAsBoolean(Value: Boolean);
begin
  EnsureParameter(ftBoolean);
  SetAsVariant(Value);
end;

procedure TBoldDOAParameter.SetAsCurrency(const Value: Currency);
begin
  EnsureParameter(ftCurrency);
  SetAsVariant(Value);
end;

procedure TBoldDOAParameter.SetAsDate(const Value: TDateTime);
begin
  EnsureParameter(ftDate);
  SetAsDateTime(Value);
end;

procedure TBoldDOAParameter.SetAsDateTime(const Value: TDateTime);
begin
  EnsureParameter(ftDateTime);
  if assigned(fQuery) then
    fQuery.Query.SetVariable(fParamIndex, Value)
  else
    fExecQuery.Query.SetVariable(fParamIndex, Value)

//  SetAsVariant(Value);
end;

procedure TBoldDOAParameter.SetAsFloat(const Value: Double);
begin
  EnsureParameter(ftFloat);
  SetAsVariant(Value);
end;

procedure TBoldDOAParameter.SetAsInteger(Value: Integer);
begin
  EnsureParameter(ftInteger);
  SetAsVariant(Value);
end;

procedure TBoldDOAParameter.SetAsMemo(const Value: string);
begin
  EnsureParameter(ftMemo);
  SetAsVariant(Value);
end;

procedure TBoldDOAParameter.SetAsSmallInt(Value: Integer);
begin
  EnsureParameter(ftInteger);
  SetAsVariant(Value);
end;

procedure TBoldDOAParameter.SetAsString(const Value: string);
begin
  EnsureParameter(ftString);
  if value = '' then
    SetAsVariant(GetDatabaseWrapper.SQLDatabaseConfig.EmptyStringMarker)
  else
    SetAsVariant(Value);
end;

procedure TBoldDOAParameter.SetAsTime(const Value: TDateTime);
begin
  EnsureParameter(ftTime);
  SetAsDateTime(Value);
end;

procedure TBoldDOAParameter.SetAsVariant(const NewValue: Variant);
begin
  EnsureParameter(ftUnknown);
  if assigned(fQuery) then
    fQuery.Query.SetVariable(fParamIndex, NewValue)
  else
    fExecQuery.Query.SetVariable(fParamIndex, NewValue)
end;

procedure TBoldDOAParameter.SetAsWord(Value: Integer);
begin
  SetAsVariant(Value);
end;

procedure TBoldDOAParameter.SetDataType(Value: TFieldType);
var
  varData: TVariableData;
begin
  if assigned(fQuery) then
    VarData := fQuery.Query.Variables.Data(fParamIndex)
  else
    VarData := fExecQuery.Query.Variables.Data(fParamIndex);
  VarData.buftype := FieldTypeToOracleType(Value);
end;

procedure TBoldDOAParameter.SetText(const Value: string);
begin

end;

{ TBoldDOAExecQuery }

procedure TBoldDOAExecQuery.AssignSQL(SQL: TStrings);
begin
  Query.SQL.Assign(SQL);
end;

procedure TBoldDOAExecQuery.AssignSQLText(SQL: String);
begin
  Query.SQL.Text := SQL;
end;

procedure TBoldDOAExecQuery.ClearParams;
begin
  query.DeleteVariables;
  FLobList.Clear;
end;

constructor TBoldDOAExecQuery.Create(Query: TOracleQuery; Database: TBoldDOADatabase);
begin
  fQuery := Query;
  fDatabase := Database;
  FLobList := TObjectList.Create;
end;

function TBoldDOAExecQuery.Createparam(FldType: TFieldType;
  const ParamName: string; ParamType: TParamType;
  Size: integer): IBoldParameter;
begin
  Query.DeclareVariable(ParamName, FieldTypeToORacleType(FldType));
  result := ParamByName(ParamName);
end;

procedure TBoldDOAExecQuery.EndSQLBatch;
begin
  // Do nothing
end;

procedure TBoldDOAExecQuery.FailSQLBatch;
begin
  // Do nothing
end;

function TBoldDOAExecQuery.GetImplementor: TObject;
begin
  result := Query;
end;

function TBoldDOAExecQuery.GetParamCount: integer;
begin
  result := Query.VariableCount;
end;

function TBoldDOAExecQuery.GetParams(i: integer): IBoldParameter;
begin
  result := TBoldDOAParameter.Create(nil, self, i, '');
end;

function TBoldDOAExecQuery.GetRowsAffected: integer;
begin
  result := Query.RowsProcessed;
end;

function TBoldDOAExecQuery.GetSQLText: String;
begin
  result := Query.SQL.Text;
end;

function TBoldDOAExecQuery.ParamByName(
  const Value: string): IBoldParameter;
var
  i: integer;
begin
  i := Query.VariableIndex(value);
  if i = -1 then
  begin
    BuildVariables;
    i := Query.VariableIndex(value);
  end;
  result := TBoldDOAParameter.Create(nil, self, i, Value)
end;

procedure TBoldDOAExecQuery.ExecSQL;
begin
  BoldLogSQL(fQuery.SQL);
  try
    fQuery.Execute;
    fQuery.Close;
  except
    on e: Exception do
    begin
      e.Message := e.Message + BOLDCRLF + 'SQL: ' + fQuery.SQL.text; // do not localize
      raise;
    end;
  end
end;

procedure TBoldDOAExecQuery.StartSQLBatch;
begin
  // do nothing
end;

procedure TBoldDOAExecQuery.AssignParams(SourceParams: TParams);
begin
  if assigned(Sourceparams) then
    Query.Variables.Assign(SourceParams)
  else
    Query.Variables.list.Clear;
end;

procedure TBoldDOAExecQuery.BuildVariables;
var
  VarList: TStringList;
  i : integer;
begin
  VarList := Oracle.FindVariables(Query.SQL.Text, False);
  for i := 0 to VarList.Count - 1 do Query.DeclareVariable(VarList[i], otString);
end;

procedure TBoldDOAExecQuery.Clear;
begin
  Query.DeleteVariables;
  FLoblist.Clear;
end;

destructor TBoldDOAExecQuery.Destroy;
begin
  FLobList.Free;
  inherited;
end;

{ TLobValues }

constructor TLobValues.Create(aLOB: TLOBLocator;const aValue: TBlobData);
begin
  inherited Create;
  FLOB := aLOB;
  FValue := aValue;
end;

destructor TLobValues.Destroy;
begin
  FLOB.Free;
  inherited Destroy;
end;

initialization
end.
