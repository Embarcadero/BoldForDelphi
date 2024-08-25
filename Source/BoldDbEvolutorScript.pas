{ Global compiler directives }
{$include bold.inc}
unit BoldDbEvolutorScript;

interface

uses
  db,
  Classes,
  BoldDefs,
  BoldBase,
  BoldLogHandler,
  BoldDBInterfaces,
  BoldPSDescriptionsSQL,
  BoldSQLDatabaseConfig,
  BoldContainers;

type
  TBoldTableOperation = class;
  TBoldAddTable = class;
  TBoldDropTable = class;
  TBoldDeleteInstances = class;
  TBoldColumnOperation = class;
  TBoldAddColumn = class;
  TBoldDropColumn = class;
  TBoldTwoTableOperation = class;
  TBoldCopyInstances = class;
  TBoldTwoColumnOperation = class;
  TBoldMoveData = class;
  TBoldDataBaseEvolutorScript = class;

  TBoldScriptOperation = class(TBoldMemoryManagedObject)
  private
    fScript: TBoldDataBaseEvolutorScript;
  public
    constructor Create(Script: TBoldDataBaseEvolutorScript);
    property Script: TBoldDataBaseEvolutorScript read fScript;
  end;

  TBoldTableOperation = class(TBoldScriptOperation)
  private
    fTableName: string;
  protected
    function GetDebugInfo: string; override;
  public
    constructor Create(Script: TBoldDataBaseEvolutorScript; const TableName: String);
    property TableName: string read fTableName;
  end;

  TBoldAddTable = class(TBoldScriptOperation)
  private
    fTableDescr: TBoldSQLTableDescription;
  protected
    function GetDebugInfo: string; override;
  public
    constructor Create(Script: TBoldDataBaseEvolutorScript; TableDescr: TBoldSQLTableDescription);
    procedure Execute;
    property TableDescr: TBoldSQLTableDescription read fTableDescr;
  end;

  TBoldDropTable = class(TBoldTableOperation)
  public
    procedure Execute;
  end;

  TBoldDeleteInstances = class(TBoldTableOperation)
  private
    fExpressionName: String;
    fDbType: TBoldDbType;
  protected
    function GetDebugInfo: string; override;
  public
    constructor Create(Script: TBoldDataBaseEvolutorScript; const ExpressionName, TableName: String; DbType: TBoldDbType);
    procedure Execute;
    property ExpressionName: String read fExpressionName;
    property DbType: TBoldDbType read fDbType;
  end;

  TBoldColumnOperation = class(TBoldTableOperation)
  private
    fColumnName: String;
  protected
    function GetDebugInfo: string; override;
  public
    constructor Create(Script: TBoldDataBaseEvolutorScript; const TableName, ColumnName: String);
    property ColumnName: String read fColumnName;
  end;

  TBoldAddColumn = class(TBoldScriptOperation)
  private
    fColumnDesc: TBoldSQLColumnDescription;
  protected
    function GetDebugInfo: string; override;
  public
    constructor Create(Script: TBoldDataBaseEvolutorScript; ColumnDesc: TBoldSQLColumnDescription);
    procedure Execute;
    property ColumnDesc: TBoldSQLColumnDescription read fColumnDesc;
  end;

  TBoldDropColumn = class(TBoldColumnOperation)
  public
    procedure Execute;
  end;

  TBoldDropIndex = class(TBoldScriptOperation)
  private
    fIndexName: String;
    fTableName: String;
  protected
    function GetDebugInfo: string; override;
  public
    constructor Create(Script: TBoldDataBaseEvolutorScript; const IndexName: String; const TableName: String);
    procedure Execute;
    property IndexName: String read fIndexName;
    property TableName: string read fTableName;
  end;

  TBoldAddIndex = class(TBoldScriptOperation)
  private
    fIndexDescription: TBoldSQLIndexDescription;
  protected
    function GetDebugInfo: string; override;
  public
    constructor Create(Script: TBoldDataBaseEvolutorScript; IndexDescription: TBoldSQLIndexDescription);
    procedure Execute;
    property IndexDescription: TBoldSQLIndexDescription read fIndexDescription;
  end;

  TBoldTwoTableOperation = class(TBoldScriptOperation)
  private
    fSourceTable: String;
    fTargetTable: String;
  protected
    function GetDebugInfo: string; override;
  public
    constructor Create(Script: TBoldDataBaseEvolutorScript; const SourceTable, TargetTable: String);
    property SourceTable: String read fSourceTable;
    property TargetTable: String read fTargetTable;
  end;

  TBoldCopyInstances = class(TBoldTwoTableOperation)
  private
    fExpressionName: String;
    fSourceDbType: TBoldDbType;
    fMoveData: TBoldMoveData;
    fIdColumns: string;
  public
    constructor Create(Script: TBoldDataBaseEvolutorScript; const ExpressionName, SourceTable, TargetTable, IdColumns: String; SourceDbType: TBoldDbType);
    destructor Destroy; override;
    procedure Execute;
    property ExpressionName: String read fExpressionName;
    property SourceDbType: TBoldDbType read fSourceDbType;
    property MoveData: TBoldMoveData read fMoveData write fMoveData;
    property IdColumns: string read fIdColumns;
  end;

  TBoldTwoColumnOperation = class(TBoldTwoTableOperation)
  private
    fSourceColumn: String;
    fTargetColumn: String;
  public
    constructor Create(Script: TBoldDataBaseEvolutorScript; const SourceTable, TargetTable, SourceColumn, TargetColumn: String);
    property SourceColumn: String read fSourceColumn;
    property TargetColumn: String read fTargetColumn;
  end;

  TBoldMoveData = class(TBoldTwoColumnOperation)
  private
    fIdColumns: string;
    fdbTypes: string;
    fAlsoMoveData: TBoldMoveData;
    function GetSignature: String;
  protected
//    function GetDebugInfo: string; override;
  public
    constructor Create(Script: TBoldDataBaseEvolutorScript; const SourceTable, TargetTable, SourceColumn, TargetColumn, IdColumns: String; dbType: TBoldDbType);
    destructor Destroy; override;
    procedure Execute;
    property IdColumns: string read fIdColumns;
    property DbTypes: string read fDbTypes;
    property Signature: String read GetSignature;
    property AlsoMoveData: TBoldMoveData read fAlsoMoveData write fAlsoMoveData;
  end;

  TBoldDataBaseEvolutorScript = class
  private
    fAddedTables: TBoldObjectArray;
    fAddedColumns: TBoldObjectArray;
    fCopiedInstances: TBoldObjectArray;
    fDeletedInstances: TBoldObjectArray;
    fDroppedColumns: TBoldObjectArray;
    fDroppedTables: TBoldObjectArray;
    fMovedData: TBoldObjectArray;
    fDroppedIndices: TBoldObjectArray;
    fAddedIndices: TBoldObjectArray;
    fScript: TStrings;
    fDataBase: IBoldDataBase;
    fSQLDataBaseConfig: TBoldSQLDatabaseConfig;
    fSQLStatements: TStringList;
    fInternalLog: TStringList;
    procedure AddCommandToScript(s: string);
    function GetAddedTables(index: integer): TBoldAddTable;
    procedure Comment(const msg: String; Args: array of const);
    procedure ExecuteSQL(const SQL: String; Args: array of const);
    function GetAddedColumns(index: integer): TBoldAddColumn;
    function GetDroppedColumns(index: integer): TBoldDropColumn;
    function GetDroppedTables(index: integer): TBoldDropTable;
    function GetCopiedInstances(index: integer): TBoldCopyInstances;
    function GetMovedData(index: integer): TBoldMoveData;
    function GetDeletedInstances(index: integer): TBoldDeleteInstances;
    function GetDroppedIndices(index: integer): TBoldDropIndex;
    function GetAddedIndices(index: integer): TBoldAddIndex;
    procedure ExtendSchema;
    procedure AdjustContents;
    procedure ReduceSchema;
    procedure ExecuteSQLStatements;
    procedure StartTransaction;
    procedure CommitTransaction;
    procedure RollBackTransaction;
    function CopyInstancesExists(const sExpressionName, sSourceTable,
     sTargetTable, sidColumns: string; aSourceDbType: TBoldDbType): Boolean;
  protected
    procedure Execute;
    property AddedTables[index: integer]: TBoldAddTable read GetAddedTables;
    property AddedColumns[index: integer]: TBoldAddColumn read GetAddedColumns;
    property DroppedTables[index: integer]: TBoldDropTable read GetDroppedTables;
    property DroppedColumns[index: integer]: TBoldDropColumn read GetDroppedColumns;
    property DroppedIndices[index: integer]: TBoldDropIndex read GetDroppedIndices;
    property AddedIndices[index: integer]: TBoldAddIndex read GetAddedIndices;
    property CopiedInstances[index: integer]: TBoldCopyInstances read GetCopiedInstances;
    property MovedData[index: integer]: TBoldMoveData read GetMovedData;
    property DeletedInstances[index: integer]: TBoldDeleteInstances read GetDeletedInstances;
  public
    constructor Create;
    destructor Destroy; override;
    function HasDropColumn(const TableName, ColumnName: String): boolean;
    function HasDropIndex(const IndexName: String; const TableName: string): boolean;
    function HasAddIndex(IndexDescription: TBoldSQLIndexDescription): boolean;
    function HasAddColumn(ColumnDesc: TBoldSQLColumnDescription): boolean;
    procedure AddTable(TableDescr: TBoldSQLTableDescription);
    procedure AddColumn(ColumnDesc: TBoldSQLColumnDescription);
    procedure DropIndex(const IndexName: String; const TableName: string);
    procedure AddIndex(IndexDescription: TBoldSQLIndexDescription);
    procedure DropColumn(const TableName, ColumnName: String);
    procedure DropTable(const TableName: String);
    procedure CopyInstances(const ExpressionName, SourceTable, TargetTable, IdColumns: String; SourceDbType: TBoldDbType);
    procedure DeleteInstances(Const ExpressionName, TableName: String; DbType: TBoldDbType);
    procedure MoveData(const SourceTable, TargetTable, SourceColumn, TargetColumn, IdColumns: String; dbType: TBoldDbType);
    procedure GenerateScript(Script: TStrings; SQLDataBaseConfig: TBoldSQLDatabaseConfig);
    procedure UpdateDatabase(DataBase: IBoldDataBase; SQLDataBaseConfig: TBoldSQLDatabaseConfig);
    procedure OptimizeScript;
    procedure AddSQLStatement(const sql: String);
    property InternalLog: TStringList read fInternalLog;
  end;

implementation

uses
  SysUtils,

  BoldCoreConsts,
  BoldGuard,
  BoldUtils;

{ TBoldAddTable }

constructor TBoldAddTable.Create(Script: TBoldDataBaseEvolutorScript; TableDescr: TBoldSQLTableDescription);
begin
  inherited Create(Script);
  fTableDescr:= TableDescr;
end;

procedure TBoldAddTable.Execute;
var
  i: integer;
  index: TBoldSQLIndexDescription;
begin
  Script.Comment(sAddTable, [TableDescr.SQLName]);
  Script.ExecuteSQL(TableDescr.SQLForCreateTable(Script.fDataBase), []);
  for i := 0 to TableDescr.IndexList.Count-1 do
  begin
    index := TableDescr.IndexList[i] as TBoldSQLIndexDescription;
    if not (ixPrimary in Index.IndexOptions) then
      Script.ExecuteSQL(Index.SQLForSecondaryKey, []);
  end;
end;

function TBoldAddTable.GetDebugInfo: string;
begin
  result := ClassName + ':' + TableDescr.SQLName;
end;

{ TBoldDataBaseEvolutorScript }

procedure TBoldDataBaseEvolutorScript.AddColumn(ColumnDesc: TBoldSQLColumnDescription);
begin
  if not HasAddColumn(ColumnDesc) then
    fAddedColumns.Add(TBoldAddColumn.Create(self, ColumnDesc));
end;

procedure TBoldDataBaseEvolutorScript.AddTable(TableDescr: TBoldSQLTableDescription);
var
  i: integer;
begin
  for i := 0 to fAddedTables.Count - 1 do
    if AddedTables[i].TableDescr = TableDescr then
      exit;
  fAddedTables.Add(TBoldAddTable.Create(self, TableDescr));
end;

function TBoldDataBaseEvolutorScript.CopyInstancesExists(const sExpressionName,
  sSourceTable, sTargetTable, sidColumns: string;
  aSourceDbType: TBoldDbType): Boolean;
var
  i: Integer;
  aCopyInstances: TBoldCopyInstances;
begin
  Result := False;
  for i := 0 to (fCopiedInstances.Count - 1) do begin
    aCopyInstances := GetCopiedInstances(i);
    Result := (aCopyInstances.fExpressionName = sExpressionName)
      and (aCopyInstances.fSourceTable = sSourceTable)
      and (aCopyInstances.fTargetTable = sTargetTable)
      and (aCopyInstances.fIdColumns = sidColumns)
      and (aCopyInstances.fSourceDbType = aSourceDbType);
    if Result then begin
      Break;
    end;
  end;
end;

procedure TBoldDataBaseEvolutorScript.CopyInstances(const ExpressionName, SourceTable, TargetTable, IdColumns: String; SourceDbType: TBoldDbType);
begin
  // prevent, that 2 identical instances of TBoldCopyInstance are added here:
  if not CopyInstancesExists(ExpressionName, SourceTable, TargetTable, idColumns, SourceDbType) then begin
    fCopiedInstances.Add(TBoldCopyInstances.Create(self, ExpressionName, SourceTable, TargetTable, IdColumns, SourceDbType));
  end;
end;

constructor TBoldDataBaseEvolutorScript.Create;
begin
  inherited;
  fAddedTables := TBoldObjectArray.Create(10, [bcoDataOwner]);
  fAddedColumns := TBoldObjectArray.Create(10, [bcoDataOwner]);
  fCopiedInstances := TBoldObjectArray.Create(10, [bcoDataOwner]);
  fDeletedInstances := TBoldObjectArray.Create(10, [bcoDataOwner]);
  fDroppedColumns := TBoldObjectArray.Create(10, [bcoDataOwner]);
  fDroppedTables := TBoldObjectArray.Create(10, [bcoDataOwner]);
  fMovedData := TBoldObjectArray.Create(10, []);
  fDroppedIndices := TBoldObjectArray.Create(10, [bcoDataOwner]);
  fAddedIndices := TBoldObjectArray.Create(10, [bcoDataOwner]);  
  fSQLStatements := TStringList.Create;
  fInternalLog := TStringList.Create;
end;

procedure TBoldDataBaseEvolutorScript.DeleteInstances(const ExpressionName, TableName: String; DbType: TBoldDbType);
begin
  fDeletedInstances.Add(TBoldDeleteInstances.Create(self, ExpressionName, TableName, DbType));
end;

destructor TBoldDataBaseEvolutorScript.Destroy;
var
  i: integer;
begin
  FreeAndNil(fAddedTables);
  FreeAndNil(fAddedColumns);
  FreeAndNil(fCopiedInstances);
  FreeAndNil(fDeletedInstances);
  FreeAndNil(fDroppedColumns);
  FreeAndNil(fDroppedTables);
  FreeAndNil(fDroppedIndices);
  FreeAndNil(fAddedIndices);  
  for i := 0 to fMovedData.Count-1 do
    fMovedData[i].Free;
  FreeAndNil(fMovedData);
  FreeAndNil(fSQLStatements);
  FreeAndNil(fInternalLog);
  inherited;
end;

procedure TBoldDataBaseEvolutorScript.DropColumn(const TableName, ColumnName: String);
begin
  if not HasDropColumn(TableName, ColumnName) then  
    fDroppedColumns.Add(TBoldDropColumn.Create(self, TableName, ColumnName));
end;

procedure TBoldDataBaseEvolutorScript.DropTable(const TableName: String);
var
  i: integer;
begin
  for i := 0 to fDroppedTables.Count - 1 do
    if DroppedTables[i].TableName = TableName then
      exit;
  fDroppedTables.Add(TBoldDropTable.Create(self, TableName));
end;

procedure TBoldDataBaseEvolutorScript.UpdateDatabase(DataBase: IBoldDataBase; SQLDataBaseConfig: TBoldSQLDatabaseConfig);
var
  WasOpen: Boolean;
begin
  fDataBase := DataBase;
  fSQLDataBaseConfig := SQLDatabaseConfig;
  WasOpen := DataBase.Connected;
  try
    if not DataBase.Connected then
      DataBase.Open;
    Execute;
  finally
    if not WasOpen then
      DataBase.Close;
    fDataBase := nil;
  end;
end;

procedure TBoldDataBaseEvolutorScript.GenerateScript(Script: TStrings; SQLDataBaseConfig: TBoldSQLDatabaseConfig);
begin
  fScript := Script;
  fSQLDataBaseConfig := SQLDatabaseConfig;
  try
    Execute;
  finally
    fScript := nil;
  end;
end;

function TBoldDataBaseEvolutorScript.GetAddedTables(index: integer): TBoldAddTable;
begin
  result := TBoldAddTable(fAddedTables[index]);
end;

procedure TBoldDataBaseEvolutorScript.MoveData(const SourceTable, TargetTable, SourceColumn, TargetColumn, IdColumns: String; dbType: TBoldDbType);
begin
  if not SameText(TargetTable+'.'+TargetColumn, SourceTable+'.'+SourceColumn) then
    fMovedData.Add(TBoldMoveData.Create(self, SourceTable, TargetTable, SourceColumn, TargetColumn, IdColumns, dbType));
end;

procedure TBoldDataBaseEvolutorScript.Execute;
begin
  fInternalLog.Clear;

  ExtendSchema;
  AdjustContents;

  ExecuteSQLStatements;

  ReduceSchema;
end;

procedure TBoldDataBaseEvolutorScript.Comment(const msg: String; Args: array of const);
begin
  if assigned(fScript) then
    fScript.Add(fSQLDataBaseConfig.SqlScriptCommentStart + format(msg, args)+ fSQLDataBaseConfig.SqlScriptCommentStop);

  BoldLog.LogFmt(msg, args);
end;

procedure TBoldDataBaseEvolutorScript.ExecuteSQL(const SQL: String; Args: array of const);
var
  q: IBoldExecQuery;
begin
  if assigned(fScript) then
    AddCommandToScript(format(SQL, args));

  if assigned(fDataBase) then
  begin
    q := fDataBase.GetExecQuery;
    try
      q.AssignSQLText(format(SQL, args));
      q.execSQL;
      fInternalLog.Add(format(sql, args));
    finally
      fDataBase.ReleaseExecQuery(q);
    end;
  end;
end;

function TBoldDataBaseEvolutorScript.GetAddedColumns(index: integer): TBoldAddColumn;
begin
  result := TBoldAddColumn(fAddedColumns[index]);
end;

function TBoldDataBaseEvolutorScript.GetAddedIndices(
  index: integer): TBoldAddIndex;
begin
  result := fAddedIndices[Index] as TBoldAddIndex;
end;

function TBoldDataBaseEvolutorScript.GetDroppedColumns(index: integer): TBoldDropColumn;
begin
  result := TBoldDropColumn(fDroppedColumns[index]);
end;

function TBoldDataBaseEvolutorScript.GetDroppedTables(index: integer): TBoldDropTable;
begin
  result := TBoldDropTable(fDroppedTables[index]);
end;

function TBoldDataBaseEvolutorScript.GetCopiedInstances(index: integer): TBoldCopyInstances;
begin
  result := TBoldCopyInstances(fCopiedInstances[index]);
end;

function TBoldDataBaseEvolutorScript.GetMovedData(
  index: integer): TBoldMoveData;
begin
  result := TBoldMoveData(fMovedData[index]);
end;

function TBoldDataBaseEvolutorScript.HasAddColumn(
  ColumnDesc: TBoldSQLColumnDescription): boolean;
  var
  i: integer;
begin
  result := false;
  for i := 0 to fAddedColumns.Count-1 do
    if AddedColumns[i].ColumnDesc=ColumnDesc then
    begin
      result := true;
      exit;
    end;
end;

function TBoldDataBaseEvolutorScript.HasAddIndex(
  IndexDescription: TBoldSQLIndexDescription): boolean;
var
  i: integer;
begin
  result := false;
   for i := 0 to fAddedIndices.Count - 1 do
     if AddedIndices[i].IndexDescription=IndexDescription then
     begin
       result := true;
       exit;
     end;
end;

function TBoldDataBaseEvolutorScript.HasDropColumn(const TableName,
  ColumnName: String): boolean;
var
  i: integer;
begin
  result := false;
  for i := 0 to fDroppedColumns.Count - 1 do
    if AnsiSameText(DroppedColumns[i].TableName, TableName) and
      AnsiSameText(DroppedColumns[i].ColumnName, ColumnName) then
      begin
        result := true;
        exit;
      end;
end;

function TBoldDataBaseEvolutorScript.HasDropIndex(const IndexName,
  TableName: string): boolean;
var
  i: integer;
begin
  result := false;
  for I := 0 to fDroppedIndices.Count - 1 do
    if AnsiSameText(DroppedIndices[i].IndexName, IndexName)
        and AnsiSameText(DroppedIndices[i].TableName, TableName) then
        begin
          result := true;
          exit;
        end;
end;

function TBoldDataBaseEvolutorScript.GetDeletedInstances(index: integer): TBoldDeleteInstances;
begin
  result := TBoldDeleteInstances(fDeletedInstances[index]);
end;

procedure TBoldDataBaseEvolutorScript.OptimizeScript;
var
  i, j: integer;
  MoveDataSignatures: TStringList;
  MoveData1, MoveData2: TBoldMoveData;
begin
  // this will find all Movedata-objects with the same targettable and dbtype,
  // and move them to the CopyInstances.
  // note that at this point, the movedatas are not chaines together yet,
  // and MoveData[j].AlsoMoveData is always nil
  for i := 0 to fCopiedInstances.Count - 1 do
    for j := fMovedData.Count - 1 downto 0 do
      if (MovedData[j].DbTypes = IntToStr(CopiedInstances[i].SourceDbType)) and
         SameText(MovedData[j].TargetTable, CopiedInstances[i].TargetTable) then
      begin
        assert(not assigned(MovedData[j].AlsoMoveData));
        MovedData[j].AlsoMoveData := CopiedInstances[i].MoveData;
        CopiedInstances[i].MoveData := MovedData[j];
        fMovedData.Delete(j);
      end;

  // this will chain up the rest of the movedatas that have the same
  // source and target table and dbtype
  i := 0;
  while i < fMovedData.Count do
  begin
    for j := fMovedData.Count-1 downto i+1 do
    begin
      if (MovedData[j].DbTypes = MovedData[i].DbTypes) and
         SameText(MovedData[j].SourceTable, MovedData[i].SourceTable) and
         SameText(MovedData[j].TargetTable, MovedData[i].TargetTable) then
      begin
        MovedData[j].AlsoMoveData := MovedData[i].AlsoMoveData;
        MovedData[i].AlsoMoveData := MovedData[j];
        fMovedData.Delete(j);
      end;
    end;
    inc(i);
  end;

  MoveDataSignatures := TStringList.Create;
  try
    for i := 0 to fMovedData.Count-1 do
      MoveDataSignatures.AddObject(MovedData[i].Signature, MovedData[i]);
    MoveDataSignatures.Sort;
    for i := 1 to MoveDataSignatures.Count-1 do
      if MoveDataSignatures[i-1] = MoveDataSignatures[i] then
      begin
        MoveData1 := TBoldMoveData(MoveDataSignatures.Objects[i-1]);
        MoveData2 := TBoldMoveData(MoveDataSignatures.Objects[i]);
        MoveData2.fdbTypes := MoveData2.dbTypes + ', '+ MoveData1.dbTypes;
        fMovedData.Remove(MoveData1);
        MoveData1.Free; 
      end;
  finally
    MoveDataSignatures.free;
  end;
end;

procedure TBoldDataBaseEvolutorScript.DropIndex(const IndexName: String; const TableNAme: String);
begin
  if not HasDropIndex(IndexName, TableName) then
    fDroppedIndices.Add(TBoldDropIndex.Create(self, IndexName, TableName));
end;

function TBoldDataBaseEvolutorScript.GetDroppedIndices(index: integer): TBoldDropIndex;
begin
  result := fDroppedIndices[Index] as TBoldDropIndex;
end;

procedure TBoldDataBaseEvolutorScript.AddIndex(IndexDescription: TBoldSQLIndexDescription);
begin
  if not HasAddIndex(IndexDescription) then
    fAddedIndices.Add(TBoldAddIndex.Create(self, IndexDescription ));
end;

procedure TBoldDataBaseEvolutorScript.AddSQLStatement(const sql: String);
begin
  fSQLStatements.Add(sql);
end;

procedure TBoldDataBaseEvolutorScript.AdjustContents;
var
  i: integer;
begin
  if fCopiedInstances.Count + fMovedData.Count + fDeletedInstances.Count = 0 then
    exit;
  // wrap all non MetaData changes in a transaction.
  // Many databases do not allow them in a transaction,
  // and Interbase does not make metadata changes visible inside the transaction
  StartTransaction;
  try
    for i := 0 to fCopiedInstances.Count-1 do
      CopiedInstances[i].Execute;

    for i := 0 to fMovedData.Count-1 do
      MovedData[i].Execute;

    for i := 0 to fDeletedInstances.Count-1 do
      DeletedInstances[i].Execute;

    CommitTransaction;
  except
    RollBackTransaction;
    raise;
  end;
end;

procedure TBoldDataBaseEvolutorScript.ExtendSchema;
var
  i: integer;
begin
  if fAddedTables.Count + fAddedColumns.Count + fAddedIndices.Count = 0 then
    Exit;
  // wrap all non MetaData changes in a transaction.
  // Many databases do not allow them in a transaction,
  // and Interbase does not make metadata changes visible inside the transaction
  StartTransaction;
  try
    for i := 0 to fAddedTables.Count-1 do
      AddedTables[i].Execute;

    for i := 0 to fAddedColumns.Count-1 do
      AddedColumns[i].Execute;

    for i := 0 to fAddedIndices.Count-1 do
      AddedIndices[i].Execute;

    CommitTransaction;
  except
    RollBackTransaction;
    raise;
  end;

end;

procedure TBoldDataBaseEvolutorScript.ReduceSchema;
var
  i: integer;
begin
  if fDroppedIndices.Count + fDroppedColumns.Count + fDroppedtables.Count = 0 then
    Exit;
  StartTransaction;
  try
    for i := 0 to fDroppedIndices.Count - 1 do
      DroppedIndices[i].Execute;

    for i := 0 to fDroppedColumns.Count - 1 do
      DroppedColumns[i].Execute;

    for i := 0 to fDroppedTables.Count - 1 do
      DroppedTables[i].Execute;

    CommitTransaction;
  except
    RollBackTransaction;
    raise;
  end;
end;

procedure TBoldDataBaseEvolutorScript.ExecuteSQLStatements;
var
  i: integer;
begin
  if fSqlStatements.Count = 0 then
    exit;
  StartTransaction;

  try
    for i := 0 to fSQLStatements.Count - 1 do
      ExecuteSQL(fSQLStatements[i], []);

    CommitTransaction;
  except
    RollBackTransaction;
    raise;
  end;
end;

procedure TBoldDataBaseEvolutorScript.AddCommandToScript(s: string);
begin
  fScript.Add(s+fSQLDataBaseConfig.SqlScriptTerminator);
  if fSQLDataBaseConfig.SqlScriptSeparator <> '' then
    fScript.Add(fSQLDataBaseConfig.SqlScriptSeparator);
end;

procedure TBoldDataBaseEvolutorScript.StartTransaction;
begin
  if not fSQLDataBaseConfig.AllowMetadataChangesInTransaction then
    exit;
  if assigned(fDataBase) then
    fDataBase.StartTransaction;
  if assigned(fScript) then
    AddCommandToScript(fSQLDataBaseConfig.SqlScriptStartTransaction);
end;

procedure TBoldDataBaseEvolutorScript.CommitTransaction;
const
  sCommittingToDB = 'Committing changes to database';
begin
  if not fSQLDataBaseConfig.AllowMetadataChangesInTransaction then
    exit;
  if assigned(fDataBase) and fDatabase.InTransaction then
  begin
    BoldLog.Log(sCommittingToDB);
    fDataBase.Commit;
  end;
  if assigned(fScript) then
    AddCommandToScript(fSQLDataBaseConfig.SqlScriptCommitTransaction);
end;

procedure TBoldDataBaseEvolutorScript.RollBackTransaction;
const
  sRollingBackDB = 'Rolling back database changes';
begin
  if not fSQLDataBaseConfig.AllowMetadataChangesInTransaction then
    exit;
  if assigned(fDataBase) and fDatabase.InTransaction then
  begin
    BoldLog.Log(sRollingBackDB);
    fDataBase.RollBack;
  end;
  if assigned(fScript) then
    AddCommandToScript(fSQLDataBaseConfig.SqlScriptRollBackTransaction);
end;

{ TBoldAddColumn }

constructor TBoldAddColumn.Create(Script: TBoldDataBaseEvolutorScript; ColumnDesc: TBoldSQLColumnDescription);
begin
  inherited Create(Script);
  fColumnDesc := ColumnDesc;
end;

procedure TBoldAddColumn.Execute;
begin
  Script.Comment(sAddColumn, [ColumnDesc.TableDescription.SQLName, ColumnDesc.SQLName]);
  Script.ExecuteSQL( 'ALTER TABLE %s ADD %s', [ColumnDesc.TableDescription.SQLName, ColumnDesc.GetSQLForColumn(Script.fDataBase)]);
  // There was code to also add index here, but we removed it as indexes are handled by separate AddIndex Operation
end;

function TBoldAddColumn.GetDebugInfo: string;
begin
  result := ClassName + ':' + ColumnDesc.TableDescription.SQLName + '.' + ColumnDesc.SQLName;
end;

{ TBoldCopyInstances }

constructor TBoldCopyInstances.Create(Script: TBoldDataBaseEvolutorScript; const ExpressionName, SourceTable, TargetTable, IdColumns: String; SourceDbType: TBoldDbType);
begin
  inherited Create(Script, SourceTable, TargetTable);
  fExpressionName := ExpressionName;
  fSourceDbType := SourceDbType;
  fIdColumns := IdColumns;
end;

destructor TBoldCopyInstances.destroy;
begin
  inherited;
  FreeAndNil(fMoveData);
end;

procedure TBoldCopyInstances.Execute;
var
  SourceColumns: TStringList;
  TargetColumns: TStringList;
  IdColumnList: TStringList;
  LocalMoveData: TBoldMoveData;
  sql: String;
  i, j: integer;
  SourceTables: TStringList;
  WhereConds: TStringList;
  SelectColumn: string;
begin
  Script.Comment(sAddInstanceOfClassToTable, [ExpressionName, TargetTable]);
  SourceColumns := TStringList.Create;
  TargetColumns := TStringList.Create;
  IdColumnList := TStringList.Create;
  SourceTables := TStringList.Create;
  WhereConds := TStringList.Create;
  try
    SourceTables.Add(SourceTable);
    IdColumnList.Commatext := IdColumns;
    for i := 0 to IdColumnLIst.Count-1 do
    begin
      SourceColumns.Add(SourceTable + '.' + IdColumnList[i]);
      TargetColumns.Add(IdColumnList[i]);
    end;
    LocalMoveData := MoveData;
    while assigned(LocalMoveData) do
    begin
      Script.Comment(sMoveDataFromXtoY, [LocalMoveData.SourceTable, LocalMoveData.SourceColumn, LocalMoveData.TargetTable, LocalMoveData.TargetColumn, LocalMoveData.DbTypes]);
      if SourceTables.IndexOf(LocalMoveData.SourceTable) = -1 then
        SourceTables.Add(LocalMoveData.SourceTable);
      SelectColumn := LocalMoveData.SourceTable + '.' + LocalMoveData.SourceColumn;
      if not SameText(LocalMoveData.SourceColumn, LocalMoveData.TargetColumn) then
        SelectColumn := SelectColumn + ' AS ' + LocalMoveData.TargetColumn;
      SourceColumns.Add(SelectColumn);
      TargetColumns.Add(LocalMoveData.TargetColumn);
      LocalMoveData := LocalMoveData.AlsoMoveData;
    end;

    sql := 'INSERT INTO %s (BOLD_TYPE, %s) SELECT %s.BOLD_TYPE, %s FROM %s%s';

    for i := 1 to SourceTables.Count-1 do
      for j := 0 to IdColumnList.Count-1 do
        WhereConds.Add(Format('%s.%s = %s.%s', [
          SourceTables[0], IdColumnList[j],
          SourceTables[i], IdColumnList[j]]));

    if SourceDbType <> NO_CLASS then
      WhereConds.Add(Format('BOLD_TYPE = %d', [SourceDbType]));

    Script.ExecuteSQL(sql, [
      TargetTable,
      BoldSeparateStringList(TargetColumns, ', ', '', ''),
      SourceTable,
      BoldSeparateStringList(SourceColumns, ', ', '', ''),
      BoldSeparateStringList(SourceTables, ', ', '', ''),
      BoldSeparateStringList(WhereConds, ' AND ', ' WHERE ', '')]);
  finally
    SourceColumns.Free;
    SourceTables.Free;
    TargetColumns.Free;
    IdColumnList.Free;
    WhereConds.Free;
  end;
end;

{ TBoldDeleteInstances }

constructor TBoldDeleteInstances.Create(Script: TBoldDataBaseEvolutorScript; const ExpressionName, TableName: String; DbType: TBoldDbType);
begin
  inherited Create(Script, TableName);
  fExpressionName := ExpressionName;
  fDbType := DbType;
end;

procedure TBoldDeleteInstances.Execute;
begin
  Script.Comment(sDeleteInstancesOfClassFromTable, [ExpressionName, TableName]);
  Script.ExecuteSQL('DELETE FROM %s WHERE BOLD_TYPE = %d', [TableName, DbType]); // do not localize
end;

function TBoldDeleteInstances.GetDebugInfo: string;
begin
  result := ClassName + ':' + ExpressionName;
end;

{ TBoldTableOperation }

constructor TBoldTableOperation.Create(Script: TBoldDataBaseEvolutorScript; const TableName: String);
begin
  inherited Create(Script);
  fTableName := TableName;
end;

function TBoldTableOperation.GetDebugInfo: string;
begin
  result := ClassName + ':' + TableName;
end;

{ TBoldColumnOperation }

constructor TBoldColumnOperation.Create(Script: TBoldDataBaseEvolutorScript; const TableName, ColumnName: String);
begin
  inherited Create(Script, TableName);
  fColumnName := ColumnName;
end;

function TBoldColumnOperation.GetDebugInfo: string;
begin
  result := ClassName + ':' + ColumnName;
end;

{ TBoldTwoTableOperation }

constructor TBoldTwoTableOperation.Create(Script: TBoldDataBaseEvolutorScript; const SourceTable, TargetTable: String);
begin
  inherited Create(Script);
  fSourceTable := SourceTable;
  fTargetTable := TargetTable;
end;

function TBoldTwoTableOperation.GetDebugInfo: string;
begin
  result := ClassName + ':' + SourceTable + '->' + TargetTable;
end;

{ TBoldTwoColumnOperation }

constructor TBoldTwoColumnOperation.Create(Script: TBoldDataBaseEvolutorScript; const SourceTable, TargetTable, SourceColumn, TargetColumn: String);
begin
  inherited Create(Script, SourceTable, TargetTable);
  fSourceColumn := SourceColumn;
  fTargetColumn := TargetColumn;
end;

{ TBoldMoveData }

constructor TBoldMoveData.Create(Script: TBoldDataBaseEvolutorScript; const SourceTable, TargetTable, SourceColumn, TargetColumn, IdColumns: String; dbType: TBoldDbType);
begin
  inherited Create(Script, SourceTable, TargetTable, SourceColumn, TargetColumn);
  fIdColumns := IdColumns;
  fdbTypes := IntToStr(dbtype);
end;

destructor TBoldMoveData.destroy;
begin
  inherited;
  FreeAndNil(fAlsoMoveData);
end;

procedure TBoldMoveData.Execute;
var
  SourceColumns: TStringList;
  TargetColumns: TStringList;
  LocalMoveData: TBoldMoveData;
  CopyStatements: TStringList;
  SelectColumns: TStringList;
  IdColumnList: TStringList;
  IdColumnJoins: TStringList;
  Guard: IBoldGuard;
  i: integer;
begin
  Guard := TBoldGuard.Create(SourceColumns, TargetColumns, CopyStatements, SelectColumns, IDColumnList, IDColumnJoins);
  SourceColumns := TStringList.Create;
  TargetColumns := TStringList.Create;
  CopyStatements := TStringList.Create;
  SelectColumns := TStringList.Create;
  IdColumnList := TStringList.Create;
  IdColumnJoins := TStringList.Create;
  
  LocalMoveData := self;
  while assigned(LocalMoveData) do
  begin
    Script.Comment(sMoveDataFromXtoY, [LocalMoveData.SourceTable, LocalMoveData.SourceColumn, LocalMoveData.TargetTable, LocalMoveData.TargetColumn, LocalMoveData.DbTypes]);
    SourceColumns.Add(LocalMoveData.SourceColumn);
    TargetColumns.Add(LocalMoveData.TargetColumn);
    LocalMoveData := LocalMoveData.AlsoMoveData;
  end;
  if SameText(SourceTable, TargetTable) then
  begin
    for i := 0 to SourceColumns.Count-1 do
      CopyStatements.Add(format('%s = %s', [TargetColumns[i], SourceColumns[i]]));
    Script.ExecuteSQL('UPDATE %s SET %s WHERE BOLD_TYPE IN (%s)', [TargetTable, BoldSeparateStringList(CopyStatements, ', ', '', ''), dbTypes]);
  end
  else
  begin
    IdColumnList.CommaText := IdColumns;
    for i := 0 to IdColumnList.Count-1 do
      IdColumnJoins.Add(format('Source.%s = Target.%s', [IdColumnList[i], IdColumnList[i]]));

    for i := 0 to SourceColumns.Count-1 do
      SelectColumns.Add(format('%s = (SELECT %s FROM %s Source WHERE %s)', [
        SourceColumns[i],
        TargetColumns[i],
        SourceTable,
        BoldSeparateStringList(IdColumnJoins, ' AND ', '', '')]));
    Script.ExecuteSQL('UPDATE %s Target SET %s WHERE BOLD_TYPE IN (%s)', [TargetTable, BoldSeparateStringList(SelectColumns, ', ', '', ''), DbTypes]);
  end;
end;

function TBoldMoveData.GetSignature: String;
var
  ColumnList: TStringList;
  tempMoveData: TBoldMoveData;
begin
  ColumnList := TStringList.Create;
  result := '';
  try
    TempMoveData := self;
    while assigned(tempMoveData) do
    begin
      ColumnList.Add(TempMoveData.SourceColumn+'->'+TempMoveData.TargetColumn);
      TempMoveData := TempMoveData.AlsoMoveData;
    end;
    ColumnList.Sort;
    result := SourceTable + '->' + TargetTable+':' + ColumnList.CommaText;
  finally
    ColumnList.Free;
  end;
end;

{ TBoldDropIndex }

constructor TBoldDropIndex.Create(Script: TBoldDataBaseEvolutorScript; const IndexName: String; const TableName: String);
begin
  inherited Create(Script);
  fIndexName := IndexName;
  fTableName := TableName;
end;

procedure TBoldDropIndex.Execute;
begin
  Script.Comment(sDropIndex, [IndexName]);
  Script.ExecuteSQL(Script.fSqlDatabaseConfig.GetDropIndexQuery(TableName, IndexName), []);
end;

function TBoldDropIndex.GetDebugInfo: string;
begin
  result := ClassName + ':' + TableName + '.' + IndexName;
end;

{ TBoldScriptOperation }

constructor TBoldScriptOperation.Create(Script: TBoldDataBaseEvolutorScript);
begin
  inherited Create;
  fScript := Script;
end;

{ TBoldDropColumn }

procedure TBoldDropColumn.Execute;
begin
  Script.Comment(sDropColumn, [TableName, ColumnName]);
  Script.ExecuteSQL(Script.fSqlDatabaseConfig.GetDropColumnQuery(TableName, ColumnName), []);
end;

{ TBoldDropTable }

procedure TBoldDropTable.Execute;
begin
  Script.Comment(sDropTable, [TableName]);
  Script.ExecuteSQL(Script.fSqlDatabaseConfig.GetDropTableQuery(TableName), []);
end;

{ TBoldAddIndex }

constructor TBoldAddIndex.Create(Script: TBoldDataBaseEvolutorScript; IndexDescription: TBoldSQLIndexDescription);
begin
  inherited Create(Script);
  fIndexDescription := IndexDescription;
end;

procedure TBoldAddIndex.Execute;
begin
  Script.Comment('Create index %s %s:[%s] ', [IndexDescription.GeneratedName, IndexDescription.TableDescription.SQLNAme, IndexDescription.IndexedFields]);
  Script.ExecuteSQL(IndexDescription.SQLForSecondaryKey, []);
end;

function TBoldAddIndex.GetDebugInfo: string;
begin
  result := ClassName + ':' + IndexDescription.IndexedFields;
end;

end.