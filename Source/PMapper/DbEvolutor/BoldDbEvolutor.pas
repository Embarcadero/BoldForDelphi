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
    procedure AddCommandToScript(Script: TStrings; s: string);
  protected
    procedure InitializeTableData(TableList, ColumnList: TStringList; MappingInfo: TBoldSQLMappingInfo);
    procedure AddNewTables;
    procedure AddNewColumns;
    procedure AddNewInstances;
    procedure MoveData;
    procedure DeleteOldInstances;
    procedure DropOldColumns;
    procedure DropOldTables;
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
  BoldDefs,
  BoldMath,
  BoldnameExpander,
  BoldLogHandler,
  BoldDbInterfaces,
  SysUtils,
  BoldUtils,
  BoldPMConsts;

{ TBoldDataBaseEvolutor }

procedure TBoldDataBaseEvolutor.AddNewColumns;
var
  i: integer;
  TableName, ColumnName: String;
  NewTable: TBoldSQLTableDescription;
  NewColumn: TBoldSQLColumnDescription;
begin
  for i := 0 to NewColumns.Count - 1 do
  begin
    TableName := Copy(NewColumns[i], 1, pos('.', NewColumns[i]) - 1);
    ColumnName := Copy(NewColumns[i], pos('.', NewColumns[i]) + 1, maxint);
    if (OldColumns.IndexOf(NewColumns[i]) = -1) and (OldTables.IndexOf(TableName) <> -1) then
    begin
      NewTable := NewPSDescription.SQLTablesList.ItemsBySQLName[TableName];
      NewColumn := NewTable.ColumnsList.ItemsBySQLName[ColumnName] as TBoldSQLColumnDescription;
      if assigned(NewColumn) then
        Script.AddColumn(NewColumn);
    end;
  end;
end;

procedure TBoldDataBaseEvolutor.AddNewInstances;
var
  i, t: integer;
  NewTables: TStringList;
  OldExprName, ExprName: String;
  OldAllInstances: TBoldAllInstancesMappingArray;
begin
  // in order not to add data more than once, we must loop over the PMapper, and not the mappinginfo
  OldAllInstances := nil;
  NewTables := TStringList.Create;
  try
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
              // what if more than 1 or = 0
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
  finally
    NewTables.free;
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
        format('INSERT INTO %s (%s) VALUES (''%s'')', [ // do not localize
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
    // either the class does not exist anymore, or it is no longer stored in that table
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
  Def, i: integer;
  TableName, ColumnName: String;
  Table: IBoldTable;
  IndexedColumns: TStringList;
begin
  IndexedColumns := TStringList.Create;
  IndexedColumns.Sorted := true;
  OldColumns.Sort;
  Table := PersistenceHandle.DataBaseInterface.GetTable;
  try
    for i := 0 to OldColumns.Count - 1 do
    begin
      TableName := Copy(OldColumns[i], 1, pos('.', OldColumns[i]) - 1);
      ColumnName := Copy(OldColumns[i], pos('.', OldColumns[i]) + 1, maxint);
      if (NewColumns.IndexOf(OldColumns[i]) = -1) and (NewTables.IndexOf(TableName) <> -1) then
      begin
        // first time and every new table
        if (i = 0) or (TableName <> Table.TableName) then
        begin
          // retrieve fresh indexdefs
          Table.Tablename := TableName;
          Table.IndexDefs.Update;
        end;
        // see if any indices needs to be dropped
        for Def := 0 to Table.IndexDefs.Count - 1 do
        begin
          IndexedColumns.CommaText := UpperCase(StringReplace(Table.IndexDefs[Def].Fields, ';', ',', [rfReplaceAll]));
          if IndexedColumns.IndexOf(UpperCase(ColumnName)) <> -1 then
            Script.DropIndex(Table.IndexDefs[Def].Name, Table.Tablename);
        end;
        Script.DropColumn(TableName, ColumnName);
      end;
    end;
  finally
    IndexedColumns.Free;
    PersistenceHandle.DataBaseInterface.releaseTable(Table);
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
        Format('DELETE FROM %s WHERE UPPER(%s)=''%s''', [ // do not localize
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
    MoveData;           BoldLog.ProgressStep;
    DeleteOldInstances; BoldLog.ProgressStep;
    DropOldColumns;     BoldLog.ProgressStep;
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
  tables: TStringList;
  i: integer;
begin
  result := false;
  Tables := TStringList.Create;
  try
    GetAllTablesForClass(ExpressionName, Mapping, Tables);
    for i := 0 to Tables.Count - 1 do
    begin
      if SameText(Tables[i], TableName) then
      begin
        result := true;
        break;
      end;
    end;
  finally
    Tables.Free;
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
begin
  for i := 0 to MappingInfo.ObjectStorageMappings.Count - 1 do
    AddName(MappingInfo.ObjectStorageMappings[i].TableName, TableList);
  for i := 0 to MappingInfo.AllInstancesMappings.Count - 1 do
    AddName(MappingInfo.AllInstancesMappings[i].TableName, TableList);
  Columns := TStringList.Create;
  try
    for i := 0 to MappingInfo.MemberMappings.Count - 1 do
    begin
      AddName(MappingInfo.MemberMappings[i].TableName, TableList);
      Columns.CommaText := MappingInfo.MemberMappings[i].Columns;
      for j := 0 to Columns.Count - 1 do
        AddName(MappingInfo.MemberMappings[i].TableName + '.' + Columns[j], ColumnList);
    end;
  finally
    Columns.Free;
  end
end;

procedure TBoldDataBaseEvolutor.LoadExistingInstances;
var
  Query: IBoldQuery;
begin
  query := PersistenceHandle.DataBaseInterface.GetQuery;
  try
    query.AssignSQLText(format('SELECT DISTINCT BOLD_TYPE FROM %s', [OldRootTableName])); // do not localize
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
  // in order not to move data more than once, we must loop over the PMapper, and not the mappinginfo
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
            // do we need to worry about lengths = 0, lengths > 1?
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
begin
  NewColumns := TStringList.Create;
  OldColumns := TStringList.Create;
  try
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
  finally
    NewColumns.Free;
    OldColumns.Free;
  end;
end;

function TBoldDataBaseEvolutor.OldRootTableName: String;
var
  i, max: integer;
  tables: TStringList;
  TableName: String;
begin
  result := 'BOLD_OBJECT'; // do not localize
  Tables := TStringList.create;
  try
    if (OldMapping.ObjectStorageMappings.Count = 0) and
       (OldMapping.AllInstancesMappings.Count = 1) then
    begin
      // there is only the rootclass, it has no objectstorage, only AllInstances-mapping
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
        // oh, we found the testname in the destination mapping, then it is what we were looking for.
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
        // if they have the same class name, and the same mapping, it is the same member
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
      // try to locate a previously added type with the same dbtype in the new typemapping
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
  AddCommandToScript(MappingScript, PersistenceHandle.SQLDataBaseConfig.SqlScriptStartTransaction);
  fMergedMapping.ScriptForWriteData(MappingScript, PersistenceHandle.SQLDataBaseConfig.SqlScriptSeparator, true, PersistenceHandle.SQLDataBaseConfig.SqlScriptTerminator);
  AddCommandToScript(MappingScript, PersistenceHandle.SQLDataBaseConfig.SqlScriptCommitTransaction);
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
      // if the dbtype, table and columns match then it is the same attribute...
      if (OldMapping.GetDbTypeMapping(UnhandledMemberMappings[i].ClassExpressionName) = OldDbType) and
        SameText(UnhandledMemberMappings[i].TableName, OldMemberMapping.TableName) and
        SameText(UnhandledMemberMappings[i].Columns, OldMemberMapping.Columns) then
          UnhandledMemberMappings.removeByindex(i);
  end else
    UnhandledMemberMappings.Remove(OldMemberMapping);
end;

procedure TBoldDataBaseEvolutor.DetectMapperChange(NewMemberMapping, OldMemberMapping: TBoldMemberMappingInfo; Param: TObject);
begin
  if (NewMemberMapping.MapperName <> OldMemberMapping.MapperName) and (OldMemberMapping.MapperName <> '') then
  begin
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
      // New class is persistent and abstract
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
        s := format(sDataStoredInXForYWillBeLost, [
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
  // Abstract classes has no ObjectStorage mapping
  result :=  (dbTypeMapping <> NO_CLASS) and (length(OldMapping.GetObjectStorageMapping(OldExpressionName)) > 0);
end;

function TBoldDataBaseEvolutor.GetCommonPrimaryKeyColumns(const PrimaryKey1, PrimaryKey2: string): String;
var
  i: integer;
  Fields1, Fields2: TStringList;
begin
  Fields1 := TStringList.Create;
  Fields2 := TStringList.Create;
  result := ''; // perhaps should default to BOLD_ID...
  try
    Fields1.CommaText := PrimaryKey1;
    Fields2.CommaText := UpperCase(PrimaryKey2);
    Fields2.Sorted := true;
    for i := Fields1.Count - 1 downto 0 do
      if Fields2.IndexOf(UpperCase(Fields1[i])) = -1 then
        Fields1.delete(i);
    result := Fields1.CommaText;
  finally
    Fields1.Free;
    Fields2.Free;
  end;
end;

function TBoldDataBaseEvolutor.GetPrimaryIndexForExistingTable(const TableName: String): String;
var
  Table: IBoldTable;
  i: integer;
begin
  Table := PersistenceHandle.DataBaseInterface.GetTable;
  try
    Table.Tablename := tableName;
    result := '';
    Table.IndexDefs.Update;
    for i := 0 to Table.IndexDefs.Count - 1 do
    begin
      if ixPrimary in Table.IndexDefs[i].Options then
      begin
        result := StringReplace(Table.IndexDefs[i].Fields, ';', ',', [rfReplaceAll]);
        exit;
      end;
    end;
  finally
    PersistenceHandle.DataBaseInterface.ReleaseTable(Table);
  end;
end;

function TBoldDataBaseEvolutor.GetPrimaryIndexForNewTable(const TableName: String): String;
var
  TableDesc: TBoldSQLTableDescription;
  i: integer;
begin
  result := '';
  TableDesc := PMapper.PSSystemDescription.SQLTablesList.ItemsBySQLName[tableName];
  if assigned(TableDesc) then
    for i := 0 to TableDesc.IndexList.Count - 1 do
      if ixPrimary in TableDesc.IndexList[i].IndexOptions then
      begin
        result := TableDesc.IndexList[i].IndexedFieldsForSQL; // this has commas instead of semicolons as separator
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
  columns: TStringList;
  ColumnDesc: TBoldSQlColumnDescription;
  TableDesc: TBoldSQLTableDescription;
begin
  if (NewMemberMapping.MapperName <> OldMemberMapping.MapperName) and (OldMemberMapping.MapperName <> '') then
  begin
    Columns := TStringList.create;
    Columns.CommaText := OldMemberMapping.Columns;
    for i := 0 to Columns.Count - 1 do
      fPreScript.DropColumn(OldMemberMapping.TableName, Columns[i]);

    TableDesc := NewPSDescription.SQLTablesList.ItemsBySQLName[NewMemberMapping.TableName];
    Columns.CommaText := NewMemberMapping.Columns;
    for i := 0 to Columns.Count - 1 do
    begin
      ColumnDesc := TableDesc.ColumnsList.ItemsBySQLName[Columns[i]] as TBoldSQLColumnDescription;
      fScript.AddColumn(ColumnDesc);
    end;
  end;
end;

procedure TBoldDataBaseEvolutor.GenerateExecutedStatements(Info: TStrings);
begin
  Info.Clear;
  Info.AddStrings(PreScript.InternalLog);
  Info.AddStrings(Script.InternalLog);
end;

procedure TBoldDataBaseEvolutor.AddCommandToScript(Script: TStrings; s: string);
begin
  Script.Add(s+PersistenceHandle.SQLDataBaseConfig.SqlScriptTerminator);
  if PersistenceHandle.SQLDataBaseConfig.SqlScriptSeparator <> '' then
    Script.Add(PersistenceHandle.SQLDataBaseConfig.SqlScriptSeparator);
end;

end.
