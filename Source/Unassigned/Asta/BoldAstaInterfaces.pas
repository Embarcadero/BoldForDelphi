unit BoldAstaInterfaces;

interface
uses
  classes,
  Dialogs,
  Db,
  AstaStringLine,
  AstaClientDataset,
  AstaClientSocket,
  AstaParamList,
  BoldBase,
  BoldDefs,
  BoldDBInterfaces,
  BoldContainers;

type
  { forward declarations }
  TBoldAstaDataBase = class;
  TBoldAstaQuery = class;
  TBoldAstaTable = class;

  { TBoldAstaQuery }
  TBoldAstaQuery = class(TBoldDataSetWrapper, IBoldQuery, IBoldExecQuery, IBoldParameterized)
  private
    fQuery: TAstaClientDataSet;
    function GetQuery: TAstaClientDataSet;
    // methods that implement IBoldQuery
    procedure AssignParams(Sourceparams: TParams);
    function GetParamCount: integer;
    function GetParams(i: integer): IBoldParameter;
    function GetRequestLiveQuery: Boolean;
    function ParamByName(const Value: string): IBoldParameter;
    procedure SetRequestLiveQuery(NewValue: Boolean);
    function GetSQLText: String;
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
    procedure Open; override;
    procedure ExecSQL;
    property Query: TAstaClientDataSet read GetQuery;
  public
    constructor Create(Query: TAstaClientDataSet; DatabaseWrapper: TBoldDatabaseWrapper);
  end;

  TBoldAstaParameter = class(TBoldParameterWrapper, IBoldParameter)
  private
    fParameter: TAstaParamItem;
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
    function GetParameter: TAstaParamItem;
    procedure AssignFieldValue(source: IBoldField);
    property Parameter: TAstaParamItem read GetParameter;
  public
    constructor create(AstaParameter: TAstaParamItem; DatasetWrapper: TBoldDatasetWrapper);
  end;


  { TBoldAstaTable }
  TBoldAstaTable = class(TBoldDataSetWrapper, IBoldTable)
  private
    fTable: TAstaClientDataSet;
    function GetTable: TAstaClientDataSet;
    property Table: TAstaClientDataSet read GetTable;
    procedure AddIndex(const Name, Fields: string; Options: TIndexOptions; const DescFields: string = '');
    procedure CreateTable;
    procedure DeleteTable;
    function GetIndexDefs: TIndexDefs;
    procedure SetTableName(NewName: String);
    function GetTableName: String;
    procedure SetExclusive(NewValue: Boolean);
    function GetExclusive: Boolean;
    function GetExists: Boolean;
  protected
    function GetDataSet: TDataSet; override;
  public
    constructor Create(Table: TAstaClientDataSet; DatabaseWrapper: TBoldDatabaseWrapper);
  end;

  { TBoldAstaDataBase }
  TBoldAstaDataBase = class(TBolddatabaseWrapper, IBoldDataBase)
  private
    fDataBase: TAstaClientSocket;
    fCachedTable: TAstaClientDataSet;
    fCachedQuery: TAstaClientDataSet;
    function GetDataBase: TAstaClientSocket;
    property DataBase: TAstaClientSocket read GetDataBase;
    function GetConnected: Boolean;
    function GetInTransaction: Boolean;
    function GetIsSQLBased: Boolean;
    procedure AllTableNames(Pattern: String; ShowSystemTables: Boolean; TableNameList: TStrings);
    procedure SetlogInPrompt(NewValue: Boolean);
    function GetLogInPrompt: Boolean;
    procedure SetKeepConnection(NewValue: Boolean);
    function GetKeepConnection: Boolean;
    procedure StartTransaction;
    procedure Commit;
    procedure RollBack;
    procedure Open;
    procedure Close;
    function GetQuery: IBoldQuery;
    procedure ReleaseQuery(var Query: IBoldQuery);
    function GetTable: IBoldTable;
    procedure ReleaseTable(var Table: IBoldTable);
    function SupportsTableCreation: Boolean;
    procedure ReleaseCachedObjects;
  public
    constructor Create(DataBase: TAstaClientSocket; EmptyStringMarker: string);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  BoldUtils;

{ TBoldAstaQuery }

procedure TBoldAstaQuery.AssignParams(Sourceparams: tparams);
begin
  if assigned(Sourceparams) then
    Query.Params.Assign(SourceParams)
  else
    Query.Params.Clear;
end;

procedure TBoldAstaQuery.AssignSQL(SQL: TStrings);
begin
  Query.SQL.BeginUpdate;
  Query.SQL.Assign(SQL);
  Query.SQL.EndUpdate;
end;

procedure TBoldAstaQuery.AssignSQLText(SQL: String);
begin
  Query.SQL.BeginUpdate;
  Query.SQL.Clear;
  BoldAppendToStrings(Query.SQL, SQL, true);
  Query.SQL.EndUpdate;
end;


constructor TBoldAstaQuery.Create(Query: TAstaClientDataSet; DatabaseWrapper: TBoldDatabaseWrapper);
begin
  inherited Create(DatabaseWrapper);
  fQuery := Query;
end;

function TBoldAstaQuery.Createparam(FldType: TFieldType; const ParamName: string; ParamType: TParamType; Size: integer): IBoldParameter;
const
  AstaParamType: Array[TparamType] of TParameterDirection = (ptUnknown, ptInput, ptOutput, ptInputOutput, ptResult);
begin
  result := TBoldAstaParameter.Create(Query.params.CreateParam(fldType, ParamName, AstaParamType[ParamType]), self);
end;

procedure TBoldAstaQuery.EndSQLBatch;
begin
  // intentionally left blank
end;

procedure TBoldAstaQuery.ExecSQL;
begin
  BoldLogSQL(Query.SQL);
  try
    Query.ExecSQL;
  except
    on e: Exception do
    begin
      e.Message := e.Message + BOLDCRLF + 'SQL: '+Query.SQL.text;
      raise;
    end;
  end
end;

procedure TBoldAstaQuery.FailSQLBatch;
begin
  // intentionally left blank
end;

function TBoldAstaQuery.GetDataSet: TDataSet;
begin
  result := Query;
end;

function TBoldAstaQuery.GetParamCount: integer;
begin
  result := Query.Params.count;
end;

function TBoldAstaQuery.GetParams(i: integer): IBoldParameter;
begin
  result := TBoldAstaParameter.Create(Query.Params[i], self);
end;

function TBoldAstaQuery.GetQuery: TAstaClientDataSet;
begin
  result := fQuery;
end;

function TBoldAstaQuery.GetRecordCount: integer;
begin
  Result := Query.RecordCount;
end;

function TBoldAstaQuery.GetRequestLiveQuery: Boolean;
begin
  result := false;
end;

function TBoldAstaQuery.GetRowsAffected: integer;
begin
  result := Query.RowsAffected;
end;

function TBoldAstaQuery.GetSQLText: String;
begin
  result := Query.SQL.Text;
end;

procedure TBoldAstaQuery.Open;
begin
  BoldLogSQL(Query.SQL);
  try
    inherited;
  except
    on e: Exception do
    begin
      e.Message := e.Message + BOLDCRLF + 'SQL: '+Query.SQL.text;
      raise;
    end;
  end
end;


function TBoldAstaQuery.ParamByName(const Value: string): IBoldParameter;
var
  Param: TAstaParamItem;
begin
  Param := Query.ParamByName(Value);
  if assigned(Param) then
    result := TBoldAstaParameter.Create(Param, self)
  else
    result := nil;
end;


{function TBoldAstaQuery.Params: TParams;
begin
  result := Query.Params;
end;
}

procedure TBoldAstaQuery.SetRequestLiveQuery(NewValue: Boolean);
begin
  // ignore
end;


procedure TBoldAstaQuery.StartSQLBatch;
begin
  // intentionally left blank
end;

{ TBoldAstaTable }

procedure TBoldAstaTable.AddIndex(const Name, Fields: string;
  Options: TIndexOptions; const DescFields: string);
begin
  // FIXME
//  Table.AddIndex(Name, fields, Options, descFields);
end;

constructor TBoldAstaTable.Create(Table: TAstaClientDataSet; DatabaseWrapper: TBoldDatabaseWrapper);
begin
  inherited Create(DatabaseWrapper);
  fTable := Table;
end;

procedure TBoldAstaTable.CreateTable;
begin
  // FIXME
//  Table.CreateTable;
end;

procedure TBoldAstaTable.DeleteTable;
begin
  // FIXME
//  table.DeleteTable;
end;

function TBoldAstaTable.GetDataSet: TDataSet;
begin
  result := Table;
end;


function TBoldAstaTable.GetExclusive: Boolean;
begin
  result := false;
end;

function TBoldAstaTable.GetExists: Boolean;
begin
  // FIXME
//  result := Table.Exists;
end;

function TBoldAstaTable.GetIndexDefs: TIndexDefs;
begin
  // FIXME
//  result := Table.IndexDefs;
end;


function TBoldAstaTable.GetTable: TAstaClientDataSet;
begin
  result := fTable;
end;

function TBoldAstaTable.GetTableName: String;
begin
  result := Table.TableName;
end;


procedure TBoldAstaTable.SetExclusive(NewValue: Boolean);
begin
//  showmessage('cant set exclusive on IBTables');
end;


procedure TBoldAstaTable.SetTableName(NewName: String);
begin
  Table.TableName := NewName;
end;

{ TBoldAstaDataBase }

procedure TBoldAstaDataBase.AllTableNames(Pattern: String; ShowSystemTables: Boolean; TableNameList: TStrings);
begin
  // FIXME
//  DataBase.GetTableNames(TableNameList, ShowSystemTables);
end;

procedure TBoldAstaDataBase.Close;
begin
  DataBase.Close;
end;

procedure TBoldAstaDataBase.Commit;
begin
  // FIXME
//  Database.commit;
end;

constructor TBoldAstaDataBase.create(DataBase: TAstaClientSocket; EmptyStringMarker: string);
begin
  inherited create(EmptyStringMarker);
  fDataBase := DataBase;
end;

destructor TBoldAstaDataBase.destroy;
begin
  fDatabase := nil;
  FreeAndNil(fCachedTable);
  FreeAndNil(fCachedQuery);
  inherited;
end;

function TBoldAstaDataBase.GetConnected: Boolean;
begin
  result := DataBase.Active;
end;

function TBoldAstaDataBase.GetDataBase: TAstaClientSocket;
begin
  result := fDataBase;
end;

function TBoldAstaDataBase.GetInTransaction: Boolean;
begin
  // FIXME:
//  DataBase.InTransaction;
end;

function TBoldAstaDataBase.GetIsSQLBased: Boolean;
begin
  result := true;
end;

function TBoldAstaDataBase.GetKeepConnection: Boolean;
begin
  result := true
  // CHECkME
end;

function TBoldAstaDataBase.GetLogInPrompt: Boolean;
begin
  result := Database.AutoLoginDlg = ltLogInDlg;
end;

function TBoldAstaDataBase.GetQuery: IBoldQuery;
var
  Query: TAstaClientDataSet;
begin
  if assigned(fCachedQuery) then
  begin
    Query := fCachedQuery;
    fCachedQuery := nil;
  end
  else
  begin
    Query := TAstaClientDataSet.Create(nil);
    Query.AstaClientSocket := DataBase;
  end;
  result := TBoldAstaQuery.Create(Query, self);
end;

function TBoldAstaDataBase.GetTable: IBoldTable;
var
  Table: TAstaClientDataSet;
begin
  if assigned(fCachedTable) then
  begin
    Table := fCachedTable;
    fCachedTable := nil;
  end
  else
  begin
    Table := TAstaClientDataSet.Create(nil);
    Table.AstaClientSocket := DataBase;
  end;
  result := TBoldAstaTable.Create(Table, self);
end;

procedure TBoldAstaDataBase.Open;
begin
  DataBase.Open;
end;

procedure TBoldAstaDataBase.ReleaseCachedObjects;
begin
  FreeAndNil(fCachedQuery);
  FreeAndNil(fCachedTable);
end;

procedure TBoldAstaDataBase.ReleaseQuery(var Query: IBoldQuery);
var
  IBQuery: TBoldAstaQuery;
begin
  if Query.Implementor is TBoldAstaQuery then
  begin
    IBQuery := Query.Implementor as TBoldAstaQuery;
    Query := nil;
    if not assigned(fCachedQuery) then
    begin
      fCachedQuery := IBQuery.fQuery;
      if fCachedQuery.Active then
        fCachedQuery.Close;
      fCachedQuery.SQL.Clear;
    end
    else
      IBQuery.fQuery.free;
    IBQuery.Free;
  end;
end;

procedure TBoldAstaDataBase.ReleaseTable(var Table: IBoldTable);
var
  IBTable: TBoldAstaTable;
begin
  if Table.Implementor is TBoldAstaTable then
  begin
    IBTable := Table.Implementor as TBoldAstaTable;
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

procedure TBoldAstaDataBase.RollBack;
begin
  // FIXME
//    Database.Rollback
end;

procedure TBoldAstaDataBase.SetKeepConnection(NewValue: Boolean);
begin
  //CHECKME
end;

procedure TBoldAstaDataBase.SetlogInPrompt(NewValue: Boolean);
begin
  if NewValue then
    Database.AutoLoginDlg := ltLogInDlg
  else
    Database.AutoLoginDlg := ltNoChallenge
end;

procedure TBoldAstaDataBase.StartTransaction;
begin
  // FIXME
  //transaction.StartTransaction
end;

function TBoldAstaDataBase.SupportsTableCreation: Boolean;
begin
  result := true;
end;

{ TBoldAstaParameter }

procedure TBoldAstaParameter.Clear;
begin
  fParameter.Clear;
end;

constructor TBoldAstaParameter.create(AstaParameter: TAstaParamItem; DatasetWrapper: TBoldDatasetWrapper);
begin
  inherited Create(DatasetWrapper);
  fParameter := AstaParameter;
end;

function TBoldAstaParameter.GetAsBCD: Currency;
begin
  result := parameter.Value;
end;

function TBoldAstaParameter.GetAsBoolean: Boolean;
begin
  result := parameter.Value;
end;

function TBoldAstaParameter.GetAsCurrency: Currency;
begin
  result := parameter.Value;
end;

function TBoldAstaParameter.GetAsDateTime: TDateTime;
begin
  result := parameter.Value;
end;

function TBoldAstaParameter.GetAsFloat: Double;
begin
  result := parameter.Value;
end;

function TBoldAstaParameter.GetAsInteger: Longint;
begin
  result := parameter.Value;
end;

function TBoldAstaParameter.GetAsMemo: string;
begin
  result := parameter.Value;
end;

function TBoldAstaParameter.GetAsString: string;
begin
  result := parameter.Value;
  if result = DatasetWrapper.DatabaseWrapper.EmptyStringMarker then
    result := '';
end;

function TBoldAstaParameter.GetAsVariant: Variant;
begin
  result := parameter.Value;
end;

function TBoldAstaParameter.GetDataType: TFieldType;
begin
  result := parameter.DataType;
end;

function TBoldAstaParameter.GetIsNull: Boolean;
begin
  result := VarIsNull(Parameter.value)
end;

function TBoldAstaParameter.GetName: String;
begin
  result := Parameter.Name;
end;

function TBoldAstaParameter.GetParameter: TAstaParamItem;
begin
  result := fParameter;
end;

procedure TBoldAstaParameter.SetAsBCD(const Value: Currency);
begin
  Parameter.Value := Value;
end;

procedure TBoldAstaParameter.SetAsBlob(const Value: TBlobData);
begin
  if Parameter.DataType = ftUnknown then
    Parameter.DataType := ftBlob;
  if value = '' then
    Parameter.Value := DatasetWrapper.DatabaseWrapper.EmptyStringMarker
  else
    Parameter.Value := Value;
end;

procedure TBoldAstaParameter.SetAsBoolean(Value: Boolean);
begin
  Parameter.Value := Value;
end;

procedure TBoldAstaParameter.SetAsCurrency(const Value: Currency);
begin
  Parameter.Value := Value;
end;

procedure TBoldAstaParameter.SetAsDate(const Value: TDateTime);
begin
  Parameter.Value := Value;
end;

procedure TBoldAstaParameter.SetAsDateTime(const Value: TDateTime);
begin
  Parameter.Value := Value;
end;

procedure TBoldAstaParameter.SetAsFloat(const Value: Double);
begin
  Parameter.Value := Value;
end;

procedure TBoldAstaParameter.SetAsInteger(Value: Integer);
begin
  Parameter.Value := Value;
end;

procedure TBoldAstaParameter.SetAsMemo(const Value: string);
begin
  Parameter.Value := Value;
end;

procedure TBoldAstaParameter.SetAsSmallInt(Value: Integer);
begin
  Parameter.Value := Value;
end;

procedure TBoldAstaParameter.SetAsString(const Value: string);
begin
  if value = '' then
    Parameter.Value := DatasetWrapper.DatabaseWrapper.EmptyStringMarker
  else
    Parameter.Value := Value
end;

procedure TBoldAstaParameter.SetAsTime(const Value: TDateTime);
begin
  Parameter.Value := Value;
end;

procedure TBoldAstaParameter.SetAsVariant(const NewValue: Variant);
begin
  Parameter.Value := NewValue;
end;

procedure TBoldAstaParameter.SetAsWord(Value: Integer);
begin
  Parameter.Value := Value;
end;

procedure TBoldAstaParameter.SetDataType(Value: TFieldType);
begin
  Parameter.DataType := Value;
end;

procedure TBoldAstaParameter.SetText(const Value: string);
begin
  Parameter.Value := Value;
end;

procedure TBoldAstaParameter.AssignFieldValue(source: IBoldField);
begin
  Parameter.Assign(Source.Field);
end;

end.



