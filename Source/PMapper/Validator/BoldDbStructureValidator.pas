
{ Global compiler directives }
{$include bold.inc}
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
    procedure ValidateColumn(BoldSQLColumnDescription: TBoldSQLColumnDescription);
    procedure ValidateColumnsForTable(BoldSQLTableDescription: TBoldSQLTableDescription);
    procedure ValidateIndicesForTable(BoldSQLTableDescription: TBoldSQLTableDescription);
    procedure FindExtraIndiciesForTable(BoldSQLTableDescription: TBoldSQLTableDescription);
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
  SysUtils,
  BoldRev;

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
      if Assigned(ObjectMapper) then
      begin
        if MappingInfo.GetDbTypeMapping(ObjectMapper.ExpressionName) = NO_CLASS then
        begin
          if QueryRes <> qrYesAll then
            QueryRes := QueryUser('Missing ID', format('A databaseId was missing for %s. Do you want to add an unused ID?', [ObjectMapper.ExpressionName]));
          if QueryRes in [qrYesAll, qrYes] then
          begin
            MappingInfo.AddTypeIdMapping(ObjectMapper.ExpressionName, MappingInfo.HighestUsedDbType+1);
            MappingsAdded := true;
          end
          else
            exit;
        end;
      end; // PMapper.ObjectPersistenceMappers[i] = nil ??? /FRHA
    end;
    if MappingsAdded then
      MappingInfo.WriteDataToDB(PersistenceHandle.DataBaseInterface);
  finally
    PersistenceHandle.DataBaseInterface.Close;
  end;
  PersistenceHandle.Active := false;
  PersistenceHandle.Active := true;
  try
    BoldLog.ProgressMax := SystemSQLMapper.AllTables.count - 1;
    for i := 0 to SystemSQLMapper.AllTables.count - 1 do
    begin
      ValidateTable(SystemSQLMapper.AllTables[i]);
      BoldLog.LogHeader := 'Checking table ' + SystemSQLMapper.AllTables[i].SQLName;
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
  aQuery: IBoldQuery;
  bColumnExists: Boolean;
  TableName: String;
  ColumnName: String;
  FieldDef: TFieldDef;
const
  sColumnMissing = 'Column missing: %s.%s (SQLType: %s)';
  sColumnSizeMismatch = 'Column %s in table %s has wrong size %d, should be %d';
  sColumnAllowsNull = 'Column %s in table %s allows null but the model does not';
  sColumnNotAllows = 'Column %s in table %s does not allow null but the model does';
  BlobFieldTypes = [{$IFDEF BOLD_DELPHI15_OR_LATER}ftStream,{$ENDIF} ftBlob..ftTypedBinary, ftOraBlob, ftOraClob];
begin
  TableName := BoldSQLColumnDescription.tableDescription.SQLName;
  ColumnName := BoldSQLColumnDescription.SQLName;
  aQuery := SystemSQLMapper.GetQuery;
  try
    aQuery.AssignSQLText(SystemSQLMapper.SQLDataBaseConfig.GetColumnExistsQuery(TableName, ColumnName));
    aQuery.Open;
    bColumnExists := aQuery.RecordCount = 1;
    aQuery.Close;
    if bColumnExists then
    begin
      FieldDef := CurrentTable.FieldDefs.Find(ColumnName);
      if not (FieldDef.DataType in BlobFieldTypes) then
      begin
        if FieldDef.Size <> BoldSQLColumnDescription.Size then
          BoldLog.LogFmt(sColumnSizeMismatch,
                         [ColumnName, Tablename, FieldDef.Size, BoldSQLColumnDescription.Size ], ltWarning);
      end;
      if FieldDef.Required <> BoldSQLColumnDescription.Mandatory then
      begin
        if FieldDef.Required then
          BoldLog.LogFmt(sColumnNotAllows,
                         [ColumnName, Tablename], ltWarning)
        else
          BoldLog.LogFmt(sColumnAllowsNull,
                         [ColumnName, Tablename], ltWarning)
      end;
    end;
  finally
    SystemSQLMapper.ReleaseQuery(aQuery);
  end;

  if not bColumnExists then begin
    BoldLog.LogFmt(sColumnMissing,
                   [Tablename, ColumnName, BoldSQLColumnDescription.SQLType], ltWarning);
    Remedy.add(format('alter table %s add %s %s %s;', [TableName, ColumnName,
        BoldSQLColumnDescription.SQLType, BoldSQLColumnDescription.SQLAllowNull])); // do not localize
  end else if BoldSQLColumnDescription.Mandatory then begin
    // Moved to BoldDbDataValidator
//    ValidateNotNullForColumn(BoldSQLColumnDescription);
  end;
end;

procedure TBoldDbStructureValidator.ValidateIndex(
  BoldSQLIndexDescription: TBoldSQLIndexDescription);
var
  i,j: Integer;
  sTableName: string;
  sIndexFields: string;
  aIndexFields: TStringList;
  aQuery: IBoldQuery;
  bIndexExists: Boolean;
  MatchList, TempList: TStringList;
  sIndexName: string;
  isMultiIndex: boolean;
  NameField: IBoldField;
const
  sIndexMissing = 'Index missing: %s.(%s)';
  cIndexNameColumn = 'name'; // mssql specific, should it be a setting in sqlconfig ?
begin
  MatchList := TStringList.Create;
  TempList := TStringList.Create;
  bIndexExists := False;
  sTableName := TBoldSQLTableDescription(BoldSQLIndexDescription.Owner).SQLName;
  sIndexFields := BoldSQLIndexDescription.IndexedFields;
  aQuery := DataBase.GetQuery;
  aIndexFields := TStringList.Create;
  try
    if pos(',', sIndexFields) > 1 then
      aIndexFields.Delimiter := ','
    else
      aIndexFields.Delimiter := ';';
    aIndexFields.DelimitedText := sIndexFields;
    //At this time only indices exists with one index-field
    //but maybe more in future
    isMultiIndex := aIndexFields.Count > 1;
    for i := 0 to aIndexFields.Count - 1 do
    begin
      aQuery.AssignSQLText(SystemSQLMapper.SQLDataBaseConfig
          .GetIndexColumnExistsQuery(sTableName, Trim(aIndexFields[i])));
      aQuery.Open;
      NameField := aQuery.FieldByName(cIndexNameColumn);
      bIndexExists := aQuery.RecordCount > 0;
      if isMultiIndex then
      begin  // for multi field indexes we have to determine if at least one index exists for all IndexFields
        TempList.Clear;
        while not aQuery.Eof do
        begin
          sIndexName := NameField.asString;
          if i = 0 then // for first column we add all index names to list
            MatchList.Add(sIndexName)
          else // for all other columns we check if index exists
            TempList.Add(sIndexName);
          aQuery.next;
        end;
        if i > 0 then
        begin
          for j := MatchList.Count - 1 downto 0 do
            if TempList.IndexOf(MatchList[j]) = -1 then
              MatchList.Delete(j);
          bIndexExists := MatchList.Count > 0;
        end;
      end;
      aQuery.Close;
      if not bIndexExists then begin
        Break;
      end;
    end;
  finally
    if isMultiIndex then
      bIndexExists := MatchList.count > 0;
    aIndexFields.Free;
    DataBase.ReleaseQuery(aQuery);
    TempList.free;
    MatchList.free;
  end;
  if not bIndexExists then
  begin
    BoldLog.LogFmt(sIndexMissing, [sTableName, sIndexFields], ltWarning);
    if BoldPSDescriptionsSQL.ixPrimary in BoldSQLIndexDescription.IndexOptions then
      Remedy.Add(Format('alter table %s add %s', [sTableName,
          BoldSQLIndexDescription.SQLForPrimaryKey])) // do not localize
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
  begin
    ValidateIndex(BoldSQLTableDescription.IndexList[i] as TBoldSQLIndexDescription);
  end;
end;

procedure TBoldDbStructureValidator.ValidateTable(
  BoldSQLTableDescription: TBoldSQLTableDescription);
var
  TableName: string;
begin
  TableName :=  BoldSQLTableDescription.MappedSQLName(BoldSQLTableDescription.SQLNameUpper);
  CurrentTable.TableName := TableName;
  if CurrentTable.Exists then
  begin
    CurrentTable.FieldDefs.Update;
    ValidateColumnsForTable(BoldSQLTableDescription);
    ValidateIndicesForTable(BoldSQLTableDescription);
    FindExtraIndiciesForTable(BoldSQLTableDescription);
  end
  else
  begin
    BoldLog.LogFmt('Table %s does not exist', [BoldSQLTableDescription.MappedSQLName(BoldSQLTableDescription.SQLNameUpper)], ltWarning);
    remedy.Add(BoldSQLTableDescription.sqlforCreateTable(DataBase));
  end;
end;

destructor TBoldDbStructureValidator.destroy;
begin
  if assigned(fCurrentTable) then
    DataBase.ReleaseTable(fCurrentTable);
  inherited;
end;

procedure TBoldDbStructureValidator.FindExtraIndiciesForTable(
  BoldSQLTableDescription: TBoldSQLTableDescription);

  function SameFields(const s1, s2: String): boolean;
  begin
    result := SameText(TBoldSQLIndexDescription.NormalizeFields(s1), TBoldSQLIndexDescription.NormalizeFields(s2));
  end;

  function DefinedInModel(IndexDef: TBoldIndexDescription): Boolean;
  var
    j: Integer;
  begin
    Result := false;
    for j := 0 to BoldSQLTableDescription.IndexList.count - 1 do
    if SameText(IndexDef.IndexName, BoldSQLTableDescription.IndexList[j].GeneratedName) and
      SameFields(IndexDef.IndexedColumns, BoldSQLTableDescription.IndexList[j].IndexedFields) then
      begin
        result := True;
        Exit;
      end;
    for j := 0 to PersistenceHandle.CustomIndexes.Count - 1 do
    begin
      if SameText(IndexDef.IndexName, PersistenceHandle.CustomIndexes.IndexDefinition[j].TableName) and
        SameFields(IndexDef.IndexedColumns, PersistenceHandle.CustomIndexes.IndexDefinition[j].Columns) and
          (IndexDef.IsUnique = PersistenceHandle.CustomIndexes.IndexDefinition[j].Unique) then
      begin
        result := True;
        Exit;
      end;
    end;
  end;

var
  i: Integer;
  IndexDefs: TBoldIndexDescriptionArray;
begin
  IndexDefs := DataBase.GetIndexDescriptions(BoldSQLTableDescription.SQlName);
  for I := 0 to length(IndexDefs) - 1 do
    if not DefinedInModel(IndexDefs[i]) then
      BoldLog.LogFmt('Extra Index: %s(%s)', [BoldSQLTableDescription.SQLName, IndexDefs[i].IndexedColumns], ltWarning);
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

initialization

end.
