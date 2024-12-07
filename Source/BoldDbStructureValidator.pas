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
  protected
    procedure DeActivate; override;
    function CreateValidatorThread: TBoldDbValidatorThread; override;
  public
    destructor Destroy; override;
    procedure Validate; override;
  end;

  { TBoldDbStructureValidatorThread }
  TBoldDbStructureValidatorThread = class(TBoldDbValidatorThread)
  private
    fCurrentTable: IBoldTable;
    function GetCurrentTable: IBoldTable;
    procedure DeActivate;
  protected
    procedure ValidateColumn(BoldSQLColumnDescription: TBoldSQLColumnDescription);
    procedure ValidateColumnsForTable(BoldSQLTableDescription: TBoldSQLTableDescription);
    procedure ValidateIndicesForTable(BoldSQLTableDescription: TBoldSQLTableDescription);
    procedure FindExtraIndiciesForTable(BoldSQLTableDescription: TBoldSQLTableDescription);
    procedure ValidateIndex(BoldSQLIndexDescription: TBoldSQLIndexDescription);
    procedure ValidateTable(BoldSQLTableDescription: TBoldSQLTableDescription);
    property CurrentTable: IBoldTable read GetCurrentTable;
  public
    constructor Create(AValidator: TBoldDbValidator); override;
    destructor Destroy; override;
    procedure Validate; override;
  end;

implementation

uses
  Classes,
  DB,
  SysUtils,

  BoldCoreConsts,
  BoldQueryUserDlg,
  BoldLogHandler,
  BoldDefs,
  BoldSQlMappingInfo,
  BoldPMappersDefault,
  BoldPMappersSQL,
  BoldPMappers,
  Winapi.ActiveX,
  System.TimeSpan;

{ TBoldDbStructureValidator }

procedure TBoldDbStructureValidatorThread.Validate;
var
  Table: TBoldSQLTableDescription;
begin
  fCurrentTable := Database.GetTable;
  try
    repeat
      var vObject := Validator.TableQueue.Dequeue;
      if not Assigned(vObject) then
        break;
      Assert(vObject is TBoldSQLTableDescription, vObject.Classname);
      Table := vObject as TBoldSQLTableDescription;
      if not Assigned(Table) then
        break;
      if DoCheckStop then exit;
      BoldLog.LogHeader := Format(sCheckingTable, [Table.SQLName]);
//      BoldLog.ProgressStep;
      ValidateTable(Table);
    until Validator.TableQueue.Empty;
  finally
    Database.ReleaseTable(fCurrentTable);
  end;
end;

procedure TBoldDbStructureValidatorThread.ValidateColumnsForTable(BoldSQLTableDescription: TBoldSQLTableDescription);
begin
  for var ColumnDescription in BoldSQLTableDescription.ColumnsList do
    ValidateColumn(ColumnDescription as TBoldSQlColumnDescription);
end;

procedure TBoldDbStructureValidatorThread.ValidateColumn(
  BoldSQLColumnDescription: TBoldSQLColumnDescription);
var
  aQuery: IBoldQuery;
  bColumnExists: Boolean;
  TableName: String;
  ColumnName: String;
  FieldDef: TFieldDef;
const
  BlobFieldTypes = [{$IFDEF BOLD_DELPHI15_OR_LATER}ftStream,{$ENDIF} ftBlob..ftTypedBinary, ftWideMemo, ftOraBlob, ftOraClob];
begin
  TableName := BoldSQLColumnDescription.tableDescription.SQLName;
  ColumnName := BoldSQLColumnDescription.SQLName;
  aQuery := Database.GetQuery;
  try
    aQuery.AssignSQLText(Database.SQLDataBaseConfig.GetColumnExistsQuery(TableName, ColumnName));
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
    Database.ReleaseQuery(aQuery);
  end;

  if not bColumnExists then begin
    BoldLog.LogFmt(sColumnMissing,
                   [Tablename, ColumnName, BoldSQLColumnDescription.SQLType], ltWarning);
    AddRemedy(format('alter table %s add %s %s %s;', [TableName, ColumnName,
        BoldSQLColumnDescription.SQLType, BoldSQLColumnDescription.SQLAllowNull])); // do not localize
  end else if BoldSQLColumnDescription.Mandatory then begin
    // Moved to BoldDbDataValidator
//    ValidateNotNullForColumn(BoldSQLColumnDescription);
  end;
end;

procedure TBoldDbStructureValidatorThread.ValidateIndex(
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
  isMultiIndex := false;
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
      aQuery.AssignSQLText(Database.SQLDataBaseConfig
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
      AddRemedy(Format('alter table %s add %s', [sTableName,
          BoldSQLIndexDescription.SQLForPrimaryKey])) // do not localize
    else
      AddRemedy(BoldSQLIndexDescription.SQLForSecondaryKey);
  end;
end;

procedure TBoldDbStructureValidatorThread.ValidateIndicesForTable(
  BoldSQLTableDescription: TBoldSQLTableDescription);
begin
  for var LIndexDescription in BoldSQLTableDescription.IndexList do
    ValidateIndex(LIndexDescription as TBoldSQLIndexDescription);
end;

procedure TBoldDbStructureValidatorThread.ValidateTable(
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
    BoldLog.LogFmt(sTableDoesNotExist, [BoldSQLTableDescription.MappedSQLName(BoldSQLTableDescription.SQLNameUpper)], ltWarning);
    AddRemedy(BoldSQLTableDescription.sqlforCreateTable(DataBase));
  end;
end;

destructor TBoldDbStructureValidatorThread.destroy;
begin
  if assigned(fCurrentTable) then
    DataBase.ReleaseTable(fCurrentTable);
  inherited;
end;

procedure TBoldDbStructureValidatorThread.FindExtraIndiciesForTable(
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

function TBoldDbStructureValidatorThread.GetCurrentTable: IBoldTable;
begin
  if not assigned(fCurrentTable) then
    fCurrentTable := DataBase.GetTable;
  result := fCurrentTable;
end;

constructor TBoldDbStructureValidatorThread.Create(AValidator: TBoldDbValidator);
begin
  inherited;

end;

procedure TBoldDbStructureValidatorThread.DeActivate;
begin
  if assigned(fCurrentTable) then
    DataBase.ReleaseTable(fCurrentTable);

  inherited;
end;

{ TBoldDbStructureValidator }

procedure TBoldDbStructureValidator.DeActivate;
begin

end;

destructor TBoldDbStructureValidator.Destroy;
begin

  inherited;
end;

procedure TBoldDbStructureValidator.Validate;
var
  i: integer;
  QueryRes: TBoldQueryResult;
  MappingInfo: TBoldSQLMappingInfo;
  PMapper: TBoldSystemDefaultMapper;
  ObjectMapper: TBoldObjectPersistenceMapper;
  MappingsAdded: Boolean;
  CurrentTable: IBoldTable;
begin
  BoldLog.LogHeader := 'Opening database';
  PMapper := PersistenceHandle.PersistenceControllerDefault.PersistenceMapper;
  MappingInfo := PMapper.MappingInfo;
  Database.Open;

  BoldLog.LogHeader := 'Reading mapping from database';
  MappingInfo.ReadDataFromDB(DataBase, true, true);
  MappingsAdded := false;
  QueryRes := qrNo;
  BoldLog.ProgressMax := PMapper.ObjectPersistenceMappers.Count - 1;

  for i := 0 to PMapper.ObjectPersistenceMappers.Count - 1 do
  begin
    if DoCheckStop then exit;
//    BoldLog.Progress := i;
    ObjectMapper := PMapper.ObjectPersistenceMappers[i];
    if Assigned(ObjectMapper) then
    begin
//      BoldLog.LogHeader := 'Reading mapping for: ' + ObjectMapper.ExpressionName;
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
    end; // PMapper.ObjectPersistenceMappers[i] = nil ??? /FRHA
  end;
  if MappingsAdded then
    MappingInfo.WriteDataToDB(Database);

  PersistenceHandle.Active := false;
  PersistenceHandle.Active := true;

  for i := 0 to SystemSQLMapper.AllTables.Count -1 do
    TableQueue.Enqueue(SystemSQLMapper.AllTables[i]);
  inherited;
end;

function TBoldDbStructureValidator.CreateValidatorThread: TBoldDbValidatorThread;
begin
  result := TBoldDbStructureValidatorThread.Create(self);
end;

end.