unit BoldDbStructureValidator;

interface

uses
  BoldDbValidator,
  BoldPSDescriptionsSQL,
  BoldDbInterfaces;

type
  { forward declaration }
  TBoldDbStructureValidator = class;

  { TBoldDbStructureValidator }
  TBoldDbStructureValidator = class(TBoldDbValidator)
  private
    fCurrentTable: IBoldTable;
    function GetCurrentTable: IBoldTable;
  protected
    procedure ValidateNotNullForColumn(BoldSQLColumnDescription: TBoldSQLColumnDescription);
    procedure ValidateColumn(BoldSQLColumnDescription: TBoldSQLColumnDescription);
    procedure ValidateColumnsForTable(BoldSQLTableDescription: TBoldSQLTableDescription);
    procedure ValidateIndicesForTable(BoldSQLTableDescription: TBoldSQLTableDescription);
    procedure ValidateIndex(BoldSQLIndexDescription: TBoldSQLIndexDescription);
    procedure ValidateTable(BoldSQLTableDescription: TBoldSQLTableDescription);
    procedure DeActivate; override;
    property CurrentTable: IBoldTable read GetCurrentTable;
  public
    destructor Destroy; override;
    procedure Validate; override;
  end;

implementation

uses
  classes,
  db,
  BoldQueryUserDlg,
  BoldLogHandler,
  BoldDefs,
  BoldSQlMappingInfo,
  BoldPMappersDefault,
  BoldPMappersSQL,
  BoldPMappers,
  BoldPMConsts,
  SysUtils;

{ TBoldDbStructureValidator }
procedure TBoldDbStructureValidator.Validate;
var
  i: integer;
  QueryRes: TBoldQueryResult;
  MappingInfo: TBoldSQLMappingInfo;
  PMapper: TBoldSystemDefaultMapper;
  ObjectMapper: TBoldObjectPersistenceMapper;
  MappingsAdded: Boolean;
begin
  try
    PMapper := PersistenceHandle.PersistenceControllerDefault.PersistenceMapper;
    MappingInfo := PMapper.MappingInfo;
    PersistenceHandle.DataBaseInterface.Open;

    MappingInfo.ReadDataFromDB(PersistenceHandle.DataBaseInterface, true, true);
    MappingsAdded := false;
    QueryRes := qrNo;

    for i := 0 to PMapper.ObjectPersistenceMappers.Count - 1 do
    begin
      ObjectMapper := PMapper.ObjectPersistenceMappers[i];
      if MappingInfo.GetDbTypeMapping(ObjectMapper.ExpressionName) = NO_CLASS then
      begin
        if QueryRes <> qrYesAll then
          QueryRes := QueryUser(sMissingID, format(sAnIDWasMissing, [ObjectMapper.ExpressionName]));
        if QueryRes in [qrYesAll, qrYes] then
        begin
          MappingInfo.AddTypeIdMapping(ObjectMapper.ExpressionName, MappingInfo.HighestUsedDbType+1);
          MappingsAdded := true;
        end
        else
          exit;
      end;
    end;
    if MappingsAdded then
      MappingInfo.WriteDataToDB(PersistenceHandle.DataBaseInterface);
  finally
    PersistenceHandle.DataBaseInterface.Close;
  end;

  PersistenceHandle.Active := true;
  try
    BoldLog.ProgressMax := SystemSQLMapper.AllTables.count - 1;
    for i := 0 to SystemSQLMapper.AllTables.count - 1 do
    begin
      ValidateTable(SystemSQLMapper.AllTables[i]);
      BoldLog.LogHeader := Format(sCheckingTable, [SystemSQLMapper.AllTables[i].SQLName]);
      BoldLog.Progress := i;
    end;
  finally
    PersistenceHandle.Active := false;
  end;
end;

procedure TBoldDbStructureValidator.ValidateColumnsForTable(BoldSQLTableDescription: TBoldSQLTableDescription);
var
  i: integer;
begin
  for i := 0 to BoldSQLTableDescription.ColumnsList.count - 1 do
    ValidateColumn(BoldSQLTableDescription.ColumnsList[i] as TBoldSQlColumnDescription);
end;

procedure TBoldDbStructureValidator.ValidateColumn(
  BoldSQLColumnDescription: TBoldSQLColumnDescription);
var
  TableName: String;
  ColumnName: String;
begin
  TableName := BoldSQLColumnDescription.tableDescription.SQLName;
  ColumnName := BoldSQLColumnDescription.SQLName;
  if not assigned(Currenttable.FindField(ColumnName)) then
  begin
    BoldLog.LogFmt(sColumnMissing,
                   [Tablename, ColumnName, BoldSQLColumnDescription.SQLType]);
    Remedy.add(format('alter table %s add %s %s %s;', [TableName, ColumnName, BoldSQLColumnDescription.SQLType, BoldSQLColumnDescription.SQLAllowNull])); // do not localize
  end
  else
    if BoldSQLColumnDescription.Mandatory then
      ValidateNotNullForColumn(BoldSQLColumnDescription);
end;

procedure TBoldDbStructureValidator.ValidateIndex(
  BoldSQLIndexDescription: TBoldSQLIndexDescription);
var
  Index: TIndexDef;
  IndexFields: String;
begin
  IndexFields := BoldSQLIndexDescription.IndexedFields;
  Index := CurrentTable.IndexDefs.GetIndexForFields(IndexFields, false);
  if not assigned(Index) then
  begin
    BoldLog.LogFmt(sIndexMissing, [CurrentTable.TableName, IndexFields]);
    if ixPrimary in BoldSQLIndexDescription.IndexOptions then
      Remedy.Add(Format('alter table %s add %s', [CurrentTable.TableName, BoldSQLIndexDescription.SQLForPrimaryKey])) // do not localize
    else
      Remedy.Add(BoldSQLIndexDescription.SQLForSecondaryKey);
  end;
end;

procedure TBoldDbStructureValidator.ValidateIndicesForTable(
  BoldSQLTableDescription: TBoldSQLTableDescription);
var
  i: integer;
begin
  for i := 0 to BoldSQLTableDescription.IndexList.count - 1 do
    ValidateIndex(BoldSQLTableDescription.IndexList[i] as TBoldSQLIndexDescription);
end;

procedure TBoldDbStructureValidator.ValidateNotNullForColumn(
  BoldSQLColumnDescription: TBoldSQLColumnDescription);
var
  query: IBoldQuery;
  FieldNames: TStrings;
  TableName: String;
  ColumnName: String;
  NullCount: integer;
begin
  FieldNames := TStringList.Create;
  query := SystemSQLMapper.GetQuery;
  TableName := BoldSQLColumnDescription.tableDescription.SQLName;
  ColumnName := BoldSQLColumnDescription.SQLName;
  try
    Query.AssignSQLText(Format('SELECT count(*) FROM %s WHERE %s IS NULL', // do not localize
                               [TableName,
                                ColumnName]));
    Query.Open;
    nullcount := Query.Fields[0].AsInteger;
    Query.Close;
    if NullCount <> 0 then
    begin
      BoldLog.LogFmtIndent(sNullValuesFound,
        [NullCount, tableName, ColumnName]);
      Remedy.Add(Format('UPDATE %s SET %s = <initial value> WHERE %1:s IS NULL;', // do not localize
                        [TableName, ColumnName]));
    end;
  finally
    FieldNames.Free;
    SystemSQLMapper.ReleaseQuery(query);
  end;
end;

procedure TBoldDbStructureValidator.ValidateTable(
  BoldSQLTableDescription: TBoldSQLTableDescription);
begin
  CurrentTable.TableName := BoldSQLTableDescription.SQLName;
  if CurrentTable.Exists then
  begin
    currentTable.Open;
    ValidateColumnsForTable(BoldSQLTableDescription);
    ValidateIndicesForTable(BoldSQLTableDescription);
    CurrentTable.Close;
  end
  else
  begin
    BoldLog.LogFmt(sTableDoesNotExist, [BoldSQLTableDescription.SQLName]);
    remedy.Add(BoldSQLTableDescription.sqlforCreateTable(DataBase));
  end;
end;

destructor TBoldDbStructureValidator.destroy;
begin
  if assigned(fCurrentTable) then
    DataBase.ReleaseTable(fCurrentTable);
  inherited;
end;

function TBoldDbStructureValidator.GetCurrentTable: IBoldTable;
begin
  if not assigned(fCurrentTable) then
    fCurrentTable := DataBase.GetTable;
  result := fCurrentTable;
end;

procedure TBoldDbStructureValidator.DeActivate;
begin
  if assigned(fCurrentTable) then
    DataBase.ReleaseTable(fCurrentTable);
  inherited;
end;

end.



