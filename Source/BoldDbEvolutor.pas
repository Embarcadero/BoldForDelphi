{ Global compiler directives }
{$include bold.inc}
unit BoldDbEvolutor;

interface

uses
  Classes,
  BoldSQLMappingInfo,
  BoldPSDescriptionsSQL,
  BoldDbEvolutorScript,
  BoldPMappersSQL,
  BoldAbstractPersistenceHandleDB;

type
  { forward declarations }
  TBoldDataBaseEvolutor = class;

  { function prototypes }
  TBoldMemberMappingPairAction = procedure (NewMemberMapping, OldMemberMapping: TBoldMemberMappingInfo; Param: TObject) of object;

  { TBoldDataBaseEvolutor }
  TBoldDataBaseEvolutor = class
  private
    fPersistenceHandle: TBoldAbstractPersistenceHandleDB;
    fPreScript: TBoldDatabaseEvolutorScript;
    fScript: TBoldDatabaseEvolutorScript;
    fOldMapping: TBoldSQLMappingInfo;
    fMergedMapping: TBoldSQLMappingInfo;
    fOldTables: TStringList;
    fNewColumns: TStringList;
    fOldColumns: TStringList;
    fNewTables: TStringList;
    fExistingInstances: TStringList;
    fGenericScript: Boolean;
    fUnhandledMemberMappings: TBoldMemberMappingList;
    function GetNewMapping: TBoldSQLMappingInfo;
    function GetNewPSDescription: TBoldSQLSystemDescription;
    function GetPMapper: TBoldSystemSQLMapper;
    function HasOldInstances(const OldExpressionName: String): Boolean;
    function CanHaveOldInstances(const OldExpressionName: String): Boolean;
    procedure LoadExistingInstances;
    function OldRootTableName: String;
    function HasStorageMapping(const ExpressionName, TableName: String; Mapping: TBoldSQLMappingInfo): Boolean;
    procedure MoveDataBetweenMappings(NewMemberMapping, OldMemberMapping: TBoldMemberMappingInfo);
    procedure GetAllTablesForClass(ExpressionName: String; Mapping: TBoldSQLMappingInfo; Tables: TStringList);
    function TranslateClassExpressionName(const ExpressionName: String; SourceMapping, DestMapping: TBoldSQLMappingInfo): string;
    function TranslateMemberName(const SourceExprName, SourceMemberName, DestExprName: String; SourceMapping, DestMapping: TBoldSQLMappingInfo): String;
    procedure MarkMemberMappingHandled(UnhandledMemberMappings: TBoldMemberMappingList; OldMemberMapping: TBoldMemberMappingInfo);
    function GetCommonPrimaryKeyColumns(const PrimaryKey1, PrimaryKey2: string): String;
    function GetPrimaryIndexForExistingTable(const TableName: String): String;
    function GetPrimaryIndexForNewTable(const TableName: String): String;
    procedure ForEachMemberMappingPair(Action: TBoldMemberMappingPairAction; IgnoreInstances: Boolean; Param: TObject = nil);
    procedure MoveDataAction(NewMemberMapping, OldMemberMapping: TBoldMemberMappingInfo; Param: TObject);
    procedure DetectTypeClashesAction(NewMemberMapping, OldMemberMapping: TBoldMemberMappingInfo; Param: TObject);
    procedure DetectMapperChange(NewMemberMapping, OldMemberMapping: TBoldMemberMappingInfo; Param: TObject);
    function DifferenceInColumns(const AColumns, BColumns: String): string;
  protected
    procedure InitializeTableData(TableList, ColumnList: TStringList; MappingInfo: TBoldSQLMappingInfo);
    procedure AddNewTables;
    procedure AddNewColumns;
    procedure AddNewInstances;
    procedure AddNewIndexes;
    procedure MoveData;
    procedure DeleteOldInstances;
    procedure DropOldColumns;
    procedure DropOldTables;
    procedure DropOldIndexes;
    procedure MergeOldDbTypes;
    procedure DetectTypeClashes;
    property OldMapping: TBoldSQLMappingInfo read fOldMapping;
    property NewMapping: TBoldSQLMappingInfo read GetNewMapping;
    property NewTables: TStringList read fNewTables;
    property OldTables: TStringList read fOldTables;
    property NewColumns: TStringList read fNewColumns;
    property OldColumns: TStringList read fOldColumns;
    property NewPSDescription: TBoldSQLSystemDescription read GetNewPSDescription;
    property PersistenceHandle: TBoldAbstractPersistenceHandleDB read fPersistenceHandle;
    property PMapper: TBoldSystemSQLMapper read GetPMapper;
    property Script: TBoldDatabaseEvolutorScript read fScript;
    property PreScript: TBoldDatabaseEvolutorScript read fPreScript;
  public
    constructor Create(PersistenceHandle: TBoldAbstractPersistenceHandleDB);
    destructor Destroy; override;
    procedure CalculateScript;
    procedure ExecuteScript;
    procedure GenerateScript(DbScript, MappingScript: TStrings);
    procedure GenerateWarnings(Info: TStrings);
    procedure GenerateExecutedStatements(Info: TStrings);
    property GenericScript: Boolean read fGenericScript write fGenericScript;
  end;

implementation

uses
  Db,
  SysUtils,

  BoldDefs,
  BoldMath,
  BoldnameExpander,
  BoldLogHandler,
  BoldDbInterfaces,
  BoldUtils,
  BoldCoreConsts,
  BoldGuard;

{ TBoldDataBaseEvolutor }

procedure TBoldDataBaseEvolutor.AddNewColumns;
var
  i, dotIndex: integer;
  TableName, ColumnName: String;
  NewTable: TBoldSQLTableDescription;
  NewColumn: TBoldSQLColumnDescription;
begin
  for i := 0 to NewColumns.Count - 1 do
  begin
    dotIndex := pos('.', NewColumns[i]);
    TableName := Copy(NewColumns[i], 1, dotIndex - 1);
    ColumnName := Copy(NewColumns[i], dotIndex + 1, maxint);
    if (OldColumns.IndexOf(NewColumns[i]) = -1) and (OldTables.IndexOf(TableName) <> -1) then
    begin
      NewTable := NewPSDescription.SQLTablesList.ItemsBySQLName[TableName];
      NewColumn := NewTable.ColumnsList.ItemsBySQLName[ColumnName] as TBoldSQLColumnDescription;
      if assigned(NewColumn) then
        Script.AddColumn(NewColumn);
    end;
  end;
end;

function ContainsIndex(IndexDefs: TBoldIndexDescriptionArray; Columns: WideString): Boolean;
var
  i: Integer;
begin
  for I := 0 to Length(IndexDefs) - 1 do
    if BoldNamesEqual(IndexDefs[i].IndexedColumns, Columns)  then
    begin
      Result := true;
      Exit;
    end;
  Result := false;
end;

procedure TBoldDataBaseEvolutor.AddNewIndexes;
var
  Def, i: integer;
  TableName: String;
  BoldTable: TBoldSQLTableDescription;
  AllTables: TStringList;
  IndexDefs: TBoldIndexDescriptionArray;
  g: IBoldGuard;
begin
  g := TBoldGuard.Create(AllTables);
  AllTables := TStringList.Create;
  PersistenceHandle.DataBaseInterface.AllTableNames('', true, AllTables);
  AllTables.CaseSensitive := false;
  for i := 0 to AllTables.Count - 1 do
  begin
    TableName := UpperCase(AllTables[i]);
    BoldTable := NewPSDescription.SQLTablesList.ItemsBySQLName[TableName];
    if not Assigned(BoldTable) then
      Continue;
    IndexDefs := PersistenceHandle.DataBaseInterface.GetIndexDescriptions(TableName);
    for Def := 0 to BoldTable.IndexList.Count - 1 do
    begin
      if not ContainsIndex(IndexDefs, BoldTable.IndexList[Def].IndexedFields) then
        Script.AddIndex(BoldTable.IndexList[Def]);
    end;
  end;
end;

procedure TBoldDataBaseEvolutor.AddNewInstances;
var
  i, t: integer;
  NewTables: TStringList;
  OldExprName, ExprName: String;
  OldAllInstances: TBoldAllInstancesMappingArray;
  g: IBoldGuard;
begin
  g := TBoldGuard.Create(NewTables);
  OldAllInstances := nil;
  NewTables := TStringList.Create;
  for i := 0 to PMapper.ObjectPersistenceMappers.Count - 1 do
  begin
    if assigned(PMapper.ObjectPersistenceMappers[i]) then
    begin
      ExprName := PMapper.ObjectPersistenceMappers[i].ExpressionName;
      OldExprName := TranslateClassExpressionName(ExprName, NewMapping, OldMapping);
      if (OldExprName <> '') and HasOldInstances(OldExprName) then
      begin
        GetAllTablesForClass(ExprName, NewMapping, NewTables);
        for t := 0 to NewTables.Count - 1 do
        begin
          if not HasStorageMapping(OldExprName, NewTables[t], OldMapping)  then
          begin
            OldAllInstances := OldMapping.GetAllInstancesMapping(OldExprName);
            if Length(OldAllInstances) = 1 then
              Script.CopyInstances(
                ExprName,
                OldAllInstances[0].TableName,
                NewTables[t],
                GetCommonPrimaryKeyColumns(
                  GetPrimaryIndexForExistingTable(OldAllInstances[0].TableName),
                  GetPrimaryIndexForNewTable(NewTables[t])),
                OldMapping.GetDbTypeMapping(OldExprName))
          end;
        end;
      end;
    end;
  end;
end;

procedure TBoldDataBaseEvolutor.AddNewTables;
var
  i: integer;
  NewTable: TBoldSQLTableDescription;
begin
  for i := 0 to NewTables.Count - 1 do
  begin
    if OldTables.IndexOf(NewTables[i]) = -1 then
    begin
      NewTable := NewPSDescription.SQLTablesList.ItemsBySQLName[NewTables[i]];
      Script.AddTable(NewTable);

      Script.AddSQLStatement(
        format('INSERT INTO %s (%s) VALUES (''%s'')', [
          BoldExpandPrefix(TABLETABLE_NAME, '', PersistenceHandle.SQLDataBaseConfig.SystemTablePrefix, NewPSDescription.SQLDatabaseConfig.MaxDBIdentifierLength, NewPSDescription.NationalCharConversion),
          TABLENAMECOLUMN_NAME,
          Newtable.SQLName]));
    end;
  end;
end;

type
  TExposedPMapper = class(TBoldSystemSQLMapper);

constructor TBoldDataBaseEvolutor.Create(PersistenceHandle: TBoldAbstractPersistenceHandleDB);
begin
  inherited Create;
  fPersistenceHandle := PersistenceHandle;
  fPersistenceHandle.ReleasePersistenceController;
  fOldMapping := TExposedPMapper(PMapper).CreateMappingInfo;
  fOldTables := TStringList.Create;
  fNewTables := TStringList.Create;
  fOldColumns := TStringList.Create;
  fNewColumns := TStringList.Create;
  fOldTables.Sorted := true;
  fNewTables.Sorted := true;
  fOldColumns.Sorted := true;
  fNewColumns.Sorted := true;
  fScript := TBoldDataBaseEvolutorScript.Create;
  fPreScript := TBoldDataBaseEvolutorScript.Create;
  fUnhandledMemberMappings := TBoldMemberMappingList.Create;
  fExistingInstances := TStringList.Create;
end;

procedure TBoldDataBaseEvolutor.DeleteOldInstances;
var
  i: integer;
  OldTableName, OldExprName, NewExprName: String;
begin
  for i := 0 to OldMapping.ObjectStorageMappings.Count - 1 do
  begin
    OldExprName := OldMapping.ObjectStorageMappings[i].ClassExpressionName;
    OldTableName := OldMapping.ObjectStorageMappings[i].TableName;
    NewExprName := TranslateClassExpressionName(OldExprName, OldMapping, NewMapping);
    if HasOldInstances(OldExprName) and
      (Newtables.IndexOf(OldtableName) <> -1) and
      ((NewExprName = '') or not HasStorageMapping(NewExprName, OldTableName, NewMapping))  then
    begin
      Script.DeleteInstances(OldExprName, OldTableName, OldMapping.GetDbTypeMapping(OldExprName));
    end;
  end;
end;

destructor TBoldDataBaseEvolutor.destroy;
begin
  inherited;
  FreeAndNil(fOldMapping);
  FreeAndNil(fOldTables);
  FreeAndNil(fNewTables);
  FreeAndNil(fOldColumns);
  FreeAndNil(fNewColumns);
  FreeAndNil(fExistingInstances);
  FreeAndNil(fScript);
  FreeAndNil(fPreScript);
  FreeAndNil(fMergedMapping);
  FreeAndNil(fUnHandledMemberMappings);
end;

procedure TBoldDataBaseEvolutor.DropOldColumns;
var
  Def, i, index, dotIndex: integer;
  s, TableName, ColumnName: String;
  IndexedColumns: TStringList;
  AllTables: TStringList;
  IndexDefs: TBoldIndexDescriptionArray;
  g: IBoldGuard;
begin
  g := TBoldGuard.Create(AllTables, IndexedColumns);
  IndexedColumns := TStringList.Create;
  IndexedColumns.Sorted := true;
  OldColumns.Sort;
  AllTables := TStringList.Create;
  PersistenceHandle.DataBaseInterface.AllTableNames('', true, AllTables);
  AllTables.CaseSensitive := false;
  for i := 0 to OldColumns.Count - 1 do
  begin
    dotIndex := pos('.', OldColumns[i]);
    s := Copy(OldColumns[i], 1, dotIndex - 1);
    if s <> TableName then
    begin
      TableName := s;
      index := AllTables.IndexOf(TableName);
      if index < 0 then
        continue;  // raise Exception.Create(TableName + ' not found at index ' + IntToStr(i));
      IndexDefs := PersistenceHandle.DataBaseInterface.GetIndexDescriptions(TableName);
    end;
    ColumnName := Copy(OldColumns[i], dotIndex + 1, maxint);
    if (NewColumns.IndexOf(OldColumns[i]) = -1) and (NewTables.IndexOf(TableName) <> -1) then
    begin
      for Def := 0 to Length(IndexDefs) - 1 do
      begin
        IndexedColumns.CommaText := UpperCase(StringReplace(IndexDefs[Def].IndexedColumns, ';', ',', [rfReplaceAll]));
        if IndexedColumns.IndexOf(UpperCase(ColumnName)) <> -1 then
        begin
          if not fPreScript.HasDropIndex(IndexDefs[Def].IndexName, Tablename) then
            Script.DropIndex(IndexDefs[Def].IndexName, Tablename);
        end;
      end;
      if not fPreScript.HasDropColumn(TableName, ColumnName) then
        Script.DropColumn(TableName, ColumnName);
    end;
  end;
end;

procedure TBoldDataBaseEvolutor.DropOldIndexes;
var
  Def, i: integer;
  TableName: String;
  BoldTable: TBoldSQLTableDescription;
  AllTables: TStringList;
  IndexDefs: TBoldIndexDescriptionArray;  
  g: IBoldGuard;
begin
  g := TBoldGuard.Create(AllTables);
  AllTables := TStringList.Create;
  PersistenceHandle.DataBaseInterface.AllTableNames('', true, AllTables);
  AllTables.CaseSensitive := false;
  for i := 0 to AllTables.Count - 1 do
  begin
    TableName := UpperCase(AllTables[i]);
    BoldTable := NewPSDescription.SQLTablesList.ItemsBySQLName[TableName];
    if not Assigned(BoldTable) then
      Continue;
    IndexDefs := PersistenceHandle.DataBaseInterface.GetIndexDescriptions(TableName);
    for Def := 0 to Length(IndexDefs) - 1 do
    begin
      if PersistenceHandle.SQLDataBaseConfig.EvolveDropsUnknownIndexes and
        not Assigned(BoldTable.IndexList.ItemsByIndexFields[IndexDefs[Def].IndexedColumns]) and
        not fPreScript.HasDropIndex(IndexDefs[Def].IndexName, Tablename) then
          Script.DropIndex(IndexDefs[Def].IndexName, Tablename);
    end;
  end;
end;

procedure TBoldDataBaseEvolutor.DropOldTables;
var
  i: integer;
begin
  for i := 0 to OldTables.Count - 1 do
    if NewTables.IndexOf(OldTables[i]) = -1 then
    begin
      Script.DropTable(OldTables[i]);
      Script.AddSQLStatement(
        Format('DELETE FROM %s WHERE UPPER(%s)=''%s''', [
          BoldExpandPrefix(TABLETABLE_NAME, '', PersistenceHandle.SQLDataBaseConfig.SystemTablePrefix, NewPSDescription.SQLDatabaseConfig.MaxDBIdentifierLength, NewPSDescription.NationalCharConversion),
          TABLENAMECOLUMN_NAME,
          uppercase(OldTables[i])]));
    end;
end;

procedure TBoldDataBaseEvolutor.CalculateScript;
begin
  if PersistenceHandle.Active then
    raise EBold.CreateFmt(sPersistenceHandleActive, [classname, PersistenceHandle.Name]);

  BoldLog.LogHeader := sInitializingScript;
  BoldLog.ProgressMax := 10;
  try
    PMapper.OpenDatabase(false, false);
    BoldLog.ProgressStep;
    fOldMapping.ReadDataFromDB(PersistenceHandle.DataBaseInterface, true, true);
    BoldLog.ProgressStep;
    fUnHandledMemberMappings.Clear;
    fUnhandledMemberMappings.FillFromList(OldMapping.MemberMappings);
    
    if not GenericScript then
      LoadExistingInstances;

    InitializeTableData(NewTables, NewColumns, NewMapping);
    InitializeTableData(OldTables, OldColumns, OldMapping);

    DetectTypeClashes;
    BoldLog.ProgressStep;

    AddNewTables;       BoldLog.ProgressStep;
    AddNewColumns;      BoldLog.ProgressStep;
    AddNewINstances;    BoldLog.ProgressStep;
    AddNewIndexes;      BoldLog.ProgressStep;
    MoveData;           BoldLog.ProgressStep;
    DropOldIndexes;     BoldLog.ProgressStep;
    DeleteOldInstances;
    BoldLog.ProgressStep;
    DropOldColumns;
    BoldLog.ProgressStep;
    DropOldTables;      BoldLog.ProgressStep;
    Script.OptimizeScript;
    MergeOldDbTypes;
  finally
    PMapper.CloseDataBase;
  end;
end;

procedure TBoldDataBaseEvolutor.GetAllTablesForClass(ExpressionName: String; Mapping: TBoldSQLMappingInfo; Tables: TStringList);
var
  StorageMappings: TBoldObjectStorageMappingArray;
  AllInstances: TBoldAllInstancesMappingArray;
  i: integer;
begin
  Tables.Clear;
  StorageMappings := Mapping.GetObjectStorageMapping(ExpressionName);
  for i := 0 to length(StorageMappings) - 1 do
    Tables.Add(StorageMappings[i].TableName);
  AllInstances := Mapping.GetAllInstancesMapping(ExpressionName);
  for i := 0 to length(AllInstances) - 1 do
    Tables.Add(AllInstances[i].TableName);
end;

function TBoldDataBaseEvolutor.GetNewMapping: TBoldSQLMappingInfo;
begin
  result := PMapper.MappingInfo;
end;

function TBoldDataBaseEvolutor.GetNewPSDescription: TBoldSQLSystemDescription;
begin
  result := PMapper.PSSystemDescription;
end;

function TBoldDataBaseEvolutor.GetPMapper: TBoldSystemSQLMapper;
begin
  result := PersistenceHandle.PersistenceControllerDefault.PersistenceMapper;
end;

function TBoldDataBaseEvolutor.HasOldInstances(const OldExpressionName: String): Boolean;
var
  dbTypeMapping: TBoldDbType;
begin
  result := CanHaveOldInstances(OldExpressionName);
  if not GenericScript and result then
  begin
    dbTypeMapping := OldMapping.GetDbTypeMapping(OldExpressionName);
    result := (dbTypeMapping <> NO_CLASS) and (fExistingInstances.IndexOf(IntToStr(dbTypeMapping)) <> -1);
  end;
end;

function TBoldDataBaseEvolutor.HasStorageMapping(const ExpressionName, TableName: String; Mapping: TBoldSQLMappingInfo): Boolean;
var
  Tables: TStringList;
  i: integer;
  g: IBoldGuard;
begin
  g := TBoldGuard.Create(Tables);
  result := false;
  Tables := TStringList.Create;
  GetAllTablesForClass(ExpressionName, Mapping, Tables);
  for i := 0 to Tables.Count - 1 do
  begin
    if SameText(Tables[i], TableName) then
    begin
      result := true;
      break;
    end;
  end;
end;

procedure TBoldDataBaseEvolutor.InitializeTableData(TableList, ColumnList: TStringList; MappingInfo: TBoldSQLMappingInfo);
  procedure AddName(Name: String; List: TStringList);
  begin
    if List.IndexOf(Name) = -1 then
      List.Add(Name)
  end;
var
  i, j: integer;
  Columns: TStringList;
  g: IBoldGuard;
begin
  g := TBoldGuard.Create(Columns);
  for i := 0 to MappingInfo.ObjectStorageMappings.Count - 1 do
    AddName(MappingInfo.ObjectStorageMappings[i].TableName, TableList);
  for i := 0 to MappingInfo.AllInstancesMappings.Count - 1 do
    AddName(MappingInfo.AllInstancesMappings[i].TableName, TableList);
  Columns := TStringList.Create;
  for i := 0 to MappingInfo.MemberMappings.Count - 1 do
  begin
    AddName(MappingInfo.MemberMappings[i].TableName, TableList);
    Columns.CommaText := MappingInfo.MemberMappings[i].Columns;
    for j := 0 to Columns.Count - 1 do
      AddName(MappingInfo.MemberMappings[i].TableName+'.' + Columns[j], ColumnList);
  end;
end;

procedure TBoldDataBaseEvolutor.LoadExistingInstances;
var
  Query: IBoldQuery;
begin
  query := PersistenceHandle.DataBaseInterface.GetQuery;
  try
    query.AssignSQLText(format('SELECT DISTINCT BOLD_TYPE FROM %s', [OldRootTableName]));
    Query.Open;
    while not QUery.Eof do
    begin
      fExistingInstances.Add(Query.Fields[0].AsString);
      Query.Next;
    end;
    Query.Close;
  finally
    PersistenceHandle.DataBaseInterface.ReleaseQuery(Query);
  end;
end;

procedure TBoldDataBaseEvolutor.ForEachMemberMappingPair(Action: TBoldMemberMappingPairAction; IgnoreInstances: Boolean; Param: TObject = nil);
var
  i, j: integer;
  NewMemberMappings: TBoldMemberMappingArray;
  OldMemberMappings: TBoldMemberMappingArray;
  NewMemberName, OldMemberName, NewExprName, OldExprName: String;
begin
  OldMemberMappings := nil;
  NewMemberMappings := nil;
  for i := 0 to PMapper.ObjectPersistenceMappers.Count - 1 do
  begin
    if assigned(PMapper.ObjectPersistenceMappers[i]) then
    begin
      NewExprName := PMapper.ObjectPersistenceMappers[i].ExpressionName;
      OldExprName := TranslateClassExpressionName(NewExprName, NewMapping, OldMapping);
      if (OldExprName <> '') and (IgnoreInstances or HasOldInstances(OldExprName)) then
      begin
        for j := 0 to PMapper.ObjectPersistenceMappers[i].MemberPersistenceMappers.Count - 1 do
        begin
          if assigned(PMapper.ObjectPersistenceMappers[i].MemberPersistenceMappers[j]) then
          begin
            NewMemberName := PMapper.ObjectPersistenceMappers[i].MemberPersistenceMappers[j].ExpressionName;
            NewMemberMappings := NewMapping.GetMemberMappings(NewExprName, NewMemberName);
            if length(NewMemberMappings) = 1 then
            begin
              OldMemberName := TranslateMemberName(NewExprName, NewMemberName, OldExprName, NewMapping, OldMapping);
              OldMemberMappings := OldMapping.GetMemberMappings(OldExprName, OldMemberName);
              if Length(OldMemberMappings) = 1 then
              begin
                Action(NewMemberMappings[0], OldMemberMappings[0], Param);
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TBoldDataBaseEvolutor.MoveDataAction(NewMemberMapping, OldMemberMapping: TBoldMemberMappingInfo; Param: TObject);
begin
  MarkMemberMappingHandled(fUnhandledMemberMappings, OldMemberMapping);
  if not SameText(
    NewMemberMapping.tableName + '.' + NewMemberMapping.Columns,
    OldMemberMapping.TableName + '.' + OldMemberMapping.Columns) and
    (NewMembermapping.MapperName = OldMemberMapping.MapperName) then
  begin
    MoveDataBetweenMappings(NewMemberMapping, OldMemberMapping);
  end;
end;

procedure TBoldDataBaseEvolutor.MoveData;
begin
  ForEachMemberMappingPair(MoveDataAction, false);
end;

procedure TBoldDataBaseEvolutor.MoveDataBetweenMappings(NewMemberMapping, OldMemberMapping: TBoldMemberMappingInfo);
var
  NewColumns, OldColumns: TStringList;
  i: integer;
  g: IBoldGuard;
begin
  g := TBoldGuard.Create(NewColumns, OldColumns);
  NewColumns := TStringList.Create;
  OldColumns := TStringList.Create;
  NewColumns.CommaText := NewMemberMapping.Columns;
  OldColumns.CommaText := OldMemberMapping.Columns;
  for i := 0 to MinIntValue([NewColumns.Count, OldColumns.Count]) - 1 do
  begin
    Script.MoveData(OldMemberMapping.TableName,
      NewMemberMapping.TableName,
      OldColumns[i],
      NewColumns[i],
      GetCommonPrimaryKeyColumns(
        GetPrimaryIndexForExistingTable(OldMemberMapping.TableName),
        GetPrimaryIndexForNewTable(NewMemberMapping.TableName)),
      OldMapping.GetDbTypeMapping(OldMemberMapping.ClassExpressionName));
  end;
end;

function TBoldDataBaseEvolutor.OldRootTableName: String;
var
  i, max: integer;
  tables: TStringList;
  TableName: String;
begin
  result := 'BOLD_OBJECT';
  Tables := TStringList.create;
  try
    if (OldMapping.ObjectStorageMappings.Count = 0) and
       (OldMapping.AllInstancesMappings.Count = 1) then
    begin
      TableName := OldMapping.AllInstancesMappings[0].TableName;
      Tables.Values[TableName] := IntToStr(StrToIntDef(Tables.Values[TableName], 0) + 1);
    end;
    for i := 0 to OldMapping.ObjectStorageMappings.Count - 1 do
    begin
      TableName := OldMapping.ObjectStorageMappings[i].TableName;
      Tables.Values[TableName] := IntToStr(StrToIntDef(Tables.Values[TableName], 0) + 1);
    end;
    max := 0;
    for i := 0 to Tables.Count - 1 do
      if StrToInt(Tables.Values[Tables.Names[i]]) > Max then
      begin
        Max := StrToInt(Tables.Values[Tables.Names[i]]);
        result := Tables.Names[i];
      end;
  finally
    tables.free;
  end;
end;

function TBoldDataBaseEvolutor.TranslateClassExpressionName(const ExpressionName: String; SourceMapping, DestMapping: TBoldSQLMappingInfo): String;
var
  DbType: TBoldDbType;
  i: integer;
  TestName: String;
begin
  result := '';
  if DestMapping.GetDbTypeMapping(ExpressionName) <> NO_CLASS then
    result := ExpressionName
  else
  begin
    dbType := SourceMapping.GetDbTypeMapping(ExpressionName);
    if dbType = NO_CLASS then
      raise EBold.CreateFmt(sUnableToFindDBID, [classname, expressionName]);
    for i := 0 to SourceMapping.DbTypeMappings.Count - 1 do
    begin
      TestName := SourceMapping.DbTypeMappings[i].ClassExpressionName;
      if (SourceMapping.DbTypeMappings[i].DbType = dbType) and
        (DestMapping.GetDbTypeMapping(TestName) <> NO_CLASS) then
      begin
        result := TestName;
        break;
      end;
    end;
  end;
end;

function TBoldDataBaseEvolutor.TranslateMemberName(
  const SourceExprName, SourceMemberName, DestExprName: String;
  SourceMapping, DestMapping: TBoldSQLMappingInfo): String;
var
  DestMemberMappings, SourceMemberMappings: TBoldMemberMappingArray;
  i: integer;
  Mapping: String;
begin
  result := '';
  SourceMemberMappings := nil;
  DestMemberMappings := nil;
  if length(DestMapping.GetMemberMappings(DestExprName, SourceMemberName)) = 1 then
    result := SourceMemberName
  else
  begin
    SourceMemberMappings := SourceMapping.GetMemberMappings(SourceExprName, SourceMemberName);
    if length(SourceMemberMappings) = 1 then
    begin
      Mapping := SourceMemberMappings[0].TableName + '.' + SourceMemberMappings[0].Columns;
      for i := 0 to SourceMapping.MemberMappings.Count - 1 do
      begin
        if SameText(SourceMapping.MemberMappings[i].ClassExpressionName, SourceExprName) and
           SameText(SourceMapping.MemberMappings[i].TableName + '.' + SourceMapping.MemberMappings[i].Columns, Mapping) then
        begin
          DestMemberMappings := DestMapping.GetMemberMappings(DestExprName, SourceMapping.MemberMappings[i].MemberName);
          if length(DestMemberMappings) = 1 then
          begin
            result := SourceMapping.MemberMappings[i].MemberName;
            break;
          end;
        end;
      end;
    end;
  end;
end;

procedure TBoldDataBaseEvolutor.ExecuteScript;
begin
  PersistenceHandle.DataBaseInterface.Open;
  try
    PreScript.InternalLog.Clear;
    Script.InternalLog.Clear;
    PreScript.UpdateDatabase(PersistenceHandle.DataBaseInterface, PersistenceHandle.SQLDataBaseConfig);
    Script.UpdateDatabase(PersistenceHandle.DataBaseInterface, PersistenceHandle.SQLDataBaseConfig);
    fMergedMapping.WriteDataToDB(PersistenceHandle.DataBaseInterface);
  finally
    PersistenceHandle.DataBaseInterface.Close;
  end;  
end;

procedure TBoldDataBaseEvolutor.MergeOldDbTypes;
var
  i, j: integer;
  FirstUnused: integer;
  DbType: TBoldDbType;
  NewExprName, OldExprName: String;
  x: TBoldClassMappingArray;
begin
  fMergedMapping := NewMapping.CloneWithoutDbType;
  x := nil;
  FirstUnused := OldMapping.HighestUsedDbType + 1;
  for i := 0 to NewMapping.DbTypeMappings.Count - 1 do
  begin
    NewExprname := NewMapping.DbTypeMappings[i].ClassExpressionName;
    OldExprName := TranslateClassExpressionName(NewExprName, NewMapping, OldMapping);
    DbType := OldMapping.GetDbTypeMapping(OldExprName);
    if DbType = NO_CLASS then
    begin
      for j := 0 to i - 1 do
      begin
        if NewMapping.DbTypeMappings[i].DbType = NewMapping.DbTypeMappings[j].dbtype then
        begin
          x := fMergedMapping.DbTypeMappings.MappingsForClassName[NewMapping.DbTypeMappings[j].ClassExpressionName];
          if length(x) = 1 then
          begin
            dbType := (x[0] as TBoldDbTypeMappingInfo).DbType;
            break;
          end;
        end;
      end;
      if dbType = NO_CLASS then
      begin
        DbType := FirstUnused;
        Inc(FirstUnused);
      end;
    end;
    fMergedMapping.AddTypeIdMapping(NewExprName, DbType);
  end;
end;

procedure TBoldDataBaseEvolutor.GenerateScript(DbScript, MappingScript: TStrings);
begin
  dbScript.BeginUpdate;
  PreScript.GenerateScript(dbScript, PersistenceHandle.SQLDataBaseConfig);
  Script.GenerateScript(dbScript, PersistenceHandle.SQLDataBaseConfig);
  dbScript.EndUpdate;

  MappingScript.BeginUpdate;
  fMergedMapping.ScriptForWriteData(
      PersistenceHandle.DatabaseInterface,
      MappingScript,
      True,
      PersistenceHandle.SQLDataBaseConfig.SqlScriptSeparator,
      PersistenceHandle.SQLDataBaseConfig.SqlScriptTerminator
      );
  MappingScript.EndUpdate;
end;

procedure TBoldDataBaseEvolutor.MarkMemberMappingHandled(UnhandledMemberMappings: TBoldMemberMappingList; OldMemberMapping: TBoldMemberMappingInfo);
var
  OldDbType: TBoldDbType;
  i: integer;
begin
  OldDbType := OldMapping.GetDbTypeMapping(OldMemberMapping.ClassExpressionName);
  if OldDbType <> NO_CLASS then
  begin
    for i := UnhandledMemberMappings.Count - 1 downto 0 do
      if (OldMapping.GetDbTypeMapping(UnhandledMemberMappings[i].ClassExpressionName) = OldDbType) and
        SameText(UnhandledMemberMappings[i].TableName, OldMemberMapping.TableName) and
        SameText(UnhandledMemberMappings[i].Columns, OldMemberMapping.Columns) then
          UnhandledMemberMappings.removeByindex(i);
  end else
    UnhandledMemberMappings.Remove(OldMemberMapping);
end;

procedure TBoldDataBaseEvolutor.DetectMapperChange(NewMemberMapping, OldMemberMapping: TBoldMemberMappingInfo; Param: TObject);
begin
  if (OldMemberMapping.MapperName <> '') and not NewMemberMapping.CompareMapping(OldMemberMapping) then
    (Param as TStrings).Add(
      format(sMemberChangedMapper, [
        NewMemberMapping.ClassExpressionName,
        NewMemberMapping.MemberName,
        OldMemberMapping.MapperName,
        NewMemberMapping.MapperName,
        OldMemberMapping.Columns,
        OldMemberMapping.TableName,
        NewMemberMapping.Columns,
        NewMemberMapping.TableName]));
  end;

procedure TBoldDataBaseEvolutor.GenerateWarnings(Info: TStrings);
var
  UnhandledMemberMappings: TBoldMemberMappingList;
  s: string;
  i: integer;
  NewExprname, OldExprname: String;
  NewClassExpressionName: String;
begin
  ForEachMemberMappingPair(DetectMapperChange, true, Info);

  for i := 0 to PMapper.ObjectPersistenceMappers.Count - 1 do
  begin
    if assigned(PMapper.ObjectPersistenceMappers[i]) and
      (length(NewMapping.GetObjectStorageMapping(PMapper.ObjectPersistenceMappers[i].ExpressionName)) = 0) then
    begin
      NewExprName := PMapper.ObjectPersistenceMappers[i].ExpressionName;
      oldExprName := TranslateClassExpressionName(NewExprName, NewMapping, OldMapping);
      if OldExprName <> '' then
      begin
        if length(OldMapping.GetObjectStorageMapping(OldExprName)) > 0 then
        begin
          if GenericScript then
            Info.Add(format(sClassBecameAbstract, [OldExprName, NewExprName]))
          else if HasOldInstances(OldExprName) then
            Info.Add(format(sUnableToHandleInstancesOfAbstract, [OldExprName, NewExprName]));
        end;
      end;
    end;
  end;

  UnhandledMemberMappings := TBoldMemberMappingList.create;
  try
    UnhandledMemberMappings.FillFromList(fUnhandledMemberMappings);
    while UnhandledMemberMappings.Count > 0 do
    begin
      if HasOldInstances(UnhandledMemberMappings[0].ClassExpressionName) then
      begin
        s := Format(sDataStoredInXForYWillBeLost, [
          UnhandledMemberMappings[0].TableName,
          UnhandledMemberMappings[0].columns,
          UnhandledMemberMappings[0].ClassExpressionName,
          UnhandledMemberMappings[0].MemberName]);
        NewClassExpressionName := TranslateClassExpressionName(UnhandledMemberMappings[0].ClassExpressionName, OldMapping, NewMapping);
        if newClassExpressionName <> UnhandledMemberMappings[0].ClassExpressionName then
        begin
          if NewClassExpressionname = '' then
            s := s + sClassNoLongerExists
          else
            s := s + format(sNewNameForClass, [NewClassExpressionName]);
        end;
        info.add(s);
      end;
      MarkMemberMappingHandled(UnhandledMemberMappings, UnhandledMemberMappings[0]);
    end;
  finally
    UnhandledMemberMappings.Free;
  end;
end;

function TBoldDataBaseEvolutor.CanHaveOldInstances(const OldExpressionName: String): Boolean;
var
  dbTypeMapping: TBoldDbType;
begin
  dbTypeMapping := OldMapping.GetDbTypeMapping(OldExpressionName);
  result :=  (dbTypeMapping <> NO_CLASS) and (length(OldMapping.GetObjectStorageMapping(OldExpressionName)) > 0);
end;

function TBoldDataBaseEvolutor.GetCommonPrimaryKeyColumns(const PrimaryKey1, PrimaryKey2: string): String;
var
  i: integer;
  Fields1, Fields2: TStringList;
  g: IBoldGuard;
begin
  g := TBoldGuard.Create(Fields1, Fields2);
  Fields1 := TStringList.Create;
  Fields2 := TStringList.Create;
  result := '';
  Fields1.CommaText := PrimaryKey1;
  Fields2.CommaText := UpperCase(PrimaryKey2);
  Fields2.Sorted := true;
  for i := Fields1.Count - 1 downto 0 do
    if Fields2.IndexOf(UpperCase(Fields1[i])) = -1 then
      Fields1.delete(i);
  result := Fields1.CommaText;
end;

function TBoldDataBaseEvolutor.GetPrimaryIndexForExistingTable(const TableName: String): String;
var
  IndexDefs: TBoldIndexDescriptionArray;
  i: integer;
begin
  result := '';
  IndexDefs := PersistenceHandle.DataBaseInterface.GetIndexDescriptions(TableName);
  for i := 0 to Length(IndexDefs)-1 do
  begin
    if IndexDefs[i].IsPrimary then
    begin
      result := StringReplace(IndexDefs[i].IndexedColumns, ';', ',', [rfReplaceAll]);
      exit;
    end;
  end;
  if result = '' then
    raise EBold.CreateFmt('%s.GetPrimaryIndexForExistingTable: Table "%s" has no primary key.', [ClassName, TableName]);
end;

function TBoldDataBaseEvolutor.GetPrimaryIndexForNewTable(const TableName: String): String;
var
  TableDesc: TBoldSQLTableDescription;
  i: integer;
begin
  result := IDCOLUMN_NAME; // Fallback: see GetPrimaryIndexForExistingTable
  TableDesc := PMapper.PSSystemDescription.SQLTablesList.ItemsBySQLName[tableName];
  if assigned(TableDesc) then
    for i := 0 to TableDesc.IndexList.Count - 1 do
      if BoldPSDescriptionsSQL.ixPrimary in TableDesc.IndexList[i].IndexOptions then
      begin
        result := TableDesc.IndexList[i].IndexedFieldsForSQL;
        exit;
      end;
end;

procedure TBoldDataBaseEvolutor.DetectTypeClashes;
begin
  ForEachMemberMappingPair(DetectTypeClashesAction, true)
end;

procedure TBoldDataBaseEvolutor.DetectTypeClashesAction(NewMemberMapping, OldMemberMapping: TBoldMemberMappingInfo; Param: TObject);
var
  i: integer;
  OldColumns: TStringList;
  NewColumns: TStringList;
  ColumnDesc: TBoldSQlColumnDescription;
  TableDesc: TBoldSQLTableDescription;
  SameTable: boolean;
  SameTableAndMapper: boolean;
  IsOrderedEvolve: boolean;
  g: IBoldGuard;
begin
  if (OldMemberMapping.MapperName <> '') and (not NewMemberMapping.CompareMapping(OldMemberMapping)) then
  begin
    g := TBoldGuard.Create(NewColumns, OldColumns);
    OldColumns := TStringList.create;
    OldColumns.CommaText := OldMemberMapping.Columns;
    NewColumns := TStringList.create;
    NewColumns.CommaText := NewMemberMapping.Columns;
    SameTable := (OldMemberMapping.TableName = NewMemberMapping.TableName);
    SameTableAndMapper := SameTable and (OldMemberMapping.MapperName = NewMemberMapping.MapperName);
    IsOrderedEvolve := SameTableAndMapper and (DifferenceInColumns(NewMemberMapping.Columns, OldMemberMapping.Columns) = NewColumns[0] + ORDERCOLUMN_SUFFIX);
    if not IsOrderedEvolve and SameTable then
      for i := 0 to OldColumns.Count - 1 do
        fScript.DropColumn(OldMemberMapping.TableName, OldColumns[i]);

    TableDesc := NewPSDescription.SQLTablesList.ItemsBySQLName[NewMemberMapping.TableName];
    if SameTable then
    for i := 0 to NewColumns.Count - 1 do
    begin
      if not IsOrderedEvolve or (i = ORDERCOLUMN_INDEX) then
      begin
        ColumnDesc := TableDesc.ColumnsList.ItemsBySQLName[NewColumns[i]] as TBoldSQLColumnDescription;
        fScript.AddColumn(ColumnDesc);
      end;
    end;
  end;
end;

function TBoldDataBaseEvolutor.DifferenceInColumns(const AColumns,
  BColumns: String): string;
var
  i,j : integer;
  a: TStringList;
  b: TStringList;
  c: TStringList;
begin
  a := TStringList.Create;
  b := TStringList.Create;
  c := TStringList.Create;
  try
    a.CommaText := AColumns;
    b.CommaText := BColumns;
    for i := 0 to a.Count - 1 do
    begin
      j := b.IndexOf(a[i]);
      if j = -1 then
        c.Add(a[i])
    end;
    for i := 0 to b.Count - 1 do
    begin
      j := a.IndexOf(b[i]);
      if j = -1 then
        c.Add(b[i])
    end;
    result := c.CommaText;
  finally
    a.free;
    b.free;
    c.free;
  end;
end;

procedure TBoldDataBaseEvolutor.GenerateExecutedStatements(Info: TStrings);
begin
  Info.Clear;
  Info.AddStrings(PreScript.InternalLog);
  Info.AddStrings(Script.InternalLog);
end;

end.