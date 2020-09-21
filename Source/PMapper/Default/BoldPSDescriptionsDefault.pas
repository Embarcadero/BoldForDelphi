unit BoldPSDescriptionsDefault;

interface

uses
  Classes,
  BoldPSDescriptionsSQL,
  BoldPSParams,
  BoldPSParamsSQL,
  BoldPSParamsDefault;

type
  {---TBoldDefaultSystemDescription---}
  TBoldDefaultSystemDescription = class(TBoldSQLSystemDescription)
  private
    FIdTable: TBoldSQLTableDescription;
    FTimeStamptable: TBoldSQLTableDescription;
    FTypeTable: TBoldSQLTableDescription;
    FXFilestable: TBoldSQLTableDescription;
    FTabletable: TBoldSQLTableDescription;
    FLastClockTable: TBoldSQLTableDescription;
    FClockLogTable: TBoldSQLTableDescription;
    fRootTable: TBoldSQLTableDescription;
    fMemberMappingTable: TBoldSQLTableDescription;
    fAllInstancesMappingTable: TBoldSQLTableDescription;
    fObjectStorageMappingTable: TBoldSQLTableDescription;
    procedure SetIdTable(const Value: TBoldSQLTableDescription);
    procedure SetTimeStamptable(const Value: TBoldSQLTableDescription);
    procedure SetTypeTable(const Value: TBoldSQLTableDescription);
    procedure SetXFilestable(const Value: TBoldSQLTableDescription);
    procedure SetTabletable(const Value: TBoldSQLTableDescription);
    procedure SetRootTable(const Value: TBoldSQLTableDescription);

    procedure AddFirstID(PSParams: TBoldPSDefaultParams);
    procedure AddFirstTimeStamp(PSParams: TBoldPSDefaultParams);
    procedure AddFirstClock(PSParams: TBoldPSDefaultParams);
    procedure AddTableNames(PSParams: TBoldPSDefaultParams);

    procedure GenerateScriptForFirstID(Script: TStrings; Separator: String);
    procedure GenerateScriptForFirstTimeStamp(Script: TStrings; Separator: String);
    procedure GenerateScriptForFirstClock(Script: TStrings; Separator: String);
    procedure GenerateScriptForTableNames(Script: TStrings; Separator: String);
  protected

    procedure InitializeKnownSystemtables(KnownTables: TStrings; PSParams: TBoldPSSQLParams); override;
  public
    procedure CreatePersistentStorage(PSParams: TBoldPSParams); override;
    procedure GenerateDatabaseScript(Script: TStrings; Separator: string); override;
    property IdTable: TBoldSQLTableDescription read FIdTable write SetIdTable;
    property RootTable: TBoldSQLTableDescription read fRootTable write SetRootTable;
    property TypeTable: TBoldSQLTableDescription read FTypeTable write SetTypeTable;
    property TimeStampTable :TBoldSQLTableDescription read FTimeStamptable write SetTimeStamptable;
    property XFilestable: TBoldSQLTableDescription read FXFilestable write SetXFilestable;
    property Tabletable: TBoldSQLTableDescription read FTabletable write SetTabletable;
    property LastClockTable: TBoldSQLTableDescription read FLastClockTable write FLastClockTable;
    property ClockLogTable: TBoldSQLTableDescription read FClockLogTable write FClockLogTable;
    property MemberMappingTable: TBoldSQLTableDescription read fMemberMappingTable write fMemberMappingTable;
    property AllInstancesMappingTable: TBoldSQLTableDescription read fAllInstancesMappingTable write fAllInstancesMappingTable;
    property ObjectStorageMappingTable: TBoldSQLTableDescription read fObjectStorageMappingTable write fObjectStorageMappingTable;
  end;

implementation

uses
  SysUtils,
  BoldDBInterfaces,
  BoldDefs,
  BoldLogHandler,
  BoldNameExpander,
  BoldPMConsts;

{---TBoldDefaultSystemDescription---}

procedure TBoldDefaultSystemDescription.AddFirstClock(PSParams: TBoldPSDefaultParams);
  procedure AddFirstClockUsingTable;
  var
    table: IBoldTable;
  begin
    table := PSParams.Database.GetTable;
    with Table do
    try
      TableName := LastClockTable.SQLName;
      Open;
      Append;
      FieldValues[BoldExpandName(LASTTIMESTAMPCOLUMN_NAME, '', xtSQL, SQLDatabaseConfig.MaxDbIdentifierLength, NationalCharConversion)] := 0;
      FieldValues[BoldExpandName(LASTCLOCKCOLUMN_NAME, '', xtSQL, SQLDatabaseConfig.MaxDbIdentifierLength, NationalCharConversion)] := 0;
      Post;
      Close;
    finally
      PSParams.Database.releasetable(Table);
    end;
  end;

  procedure AddFirstClockUsingQuery;
  var
    Q: IBoldExecQuery;
  begin
    q := PSParams.DataBase.GetExecQuery;
    try
      q.AssignSQLText(format(
        'INSERT INTO %s (%s, %s) VALUES (0, :FirstClock)', // do not localize
          [LastClockTable.SQLName, LASTTIMESTAMPCOLUMN_NAME, LASTCLOCKCOLUMN_NAME] ));
      q.ParamByName('FirstClock').AsDateTime := 0; // do not localize
      q.ExecSQL;
    finally
      PSParams.DataBase.ReleaseExecQuery(q);
    end;
  end;

begin
  BoldLog.Log(sLogWritingFirstClock);

  case EffectiveGenerationMode(PSParams) of
    dbgTable: AddFirstClockUsingTable;
    dbgQuery: AddFirstClockUsingQuery;
    else raise EBold.CreateFmt(sUnknownGenerationMode, [ClassName, 'AddFirstClock']); // do not localize
  end;
end;


procedure TBoldDefaultSystemDescription.AddFirstID(PSParams: TBoldPSDefaultParams);
  procedure AddFirstIdUsingTable;
  var
    table: IBoldTable;
  begin
    table := PSParams.DataBase.GetTable;
    with Table do
    try
      TableName := IDTable.SQLName;
      Open;
      Append;
      FieldValues[IDCOLUMN_NAME] := 1;
      Post;
      Close;
    finally
      PSParams.DataBase.ReleaseTable(table);
    end;
  end;

  procedure AddFirstIdUsingQuery;
  var
    Q: IBoldExecQuery;
  begin
    q := PSParams.DataBase.GetExecQuery;
    try
      q.AssignSQLText(format(
        'INSERT INTO %s (%s) VALUES (1)', // do not localize
          [IDTable.SQLName, IDCOLUMN_NAME] ));
      q.ExecSQL;
    finally
      PSParams.DataBase.ReleaseExecQuery(q);
    end;
  end;

begin
  BoldLog.Log(sLogWritingFirstID);
  case EffectiveGenerationMode(PSParams) of
    dbgTable: AddFirstIDUsingTable;
    dbgQuery: AddFirstIDUsingQuery;
    else raise EBold.CreateFmt(sUnknownGenerationMode, [ClassName, 'AddFirstID']); // do not localize
  end;
end;

procedure TBoldDefaultSystemDescription.AddFirstTimeStamp(PSParams: TBoldPSDefaultParams);
  procedure AddFirstTimeStampUsingTable;
  var
    Table: IBoldTable;
  begin
    BoldLog.Log(sLogWritingFirstTimeStamp);
    Table := PSParams.DataBase.GetTable;
    with Table do
    try
      TableName := TimeStampTable.SQLName;
      Open;
      Append;
      FieldValues[BoldExpandName(TIMESTAMPCOLUMN_NAME, '', xtSQL, SQLDatabaseConfig.MaxDbIdentifierLength, NationalCharConversion)] := 0;
      Post;
      Close;
    finally
      PSParams.DataBase.ReleaseTable(table);
    end;
  end;
  procedure AddFirstTimeStampUsingQuery;
  var
    Q: IBoldExecQuery;
  begin
    q := PSParams.DataBase.GetExecQuery;
    try
      q.AssignSQLText(format(
        'INSERT INTO %s (%s) VALUES (0)', // do not localize
          [TimeStampTable.SQLName, BoldExpandName(TIMESTAMPCOLUMN_NAME, '', xtSQL, SQLDatabaseConfig.MaxDbIdentifierLength, NationalCharConversion)] ));
      q.ExecSQL;
    finally
      PSParams.DataBase.ReleaseExecQuery(q);
    end;
  end;

begin
  BoldLog.Log(sLogWritingFirstTimeStamp);
  case EffectiveGenerationMode(PSParams) of
    dbgTable: AddFirstTimeStampUsingTable;
    dbgQuery: AddFirstTimeStampUsingQuery;
    else raise EBold.CreateFmt(sUnknownGenerationMode, [ClassName, 'AddFirstTimeStamp']); // do not localize
  end;
end;



procedure TBoldDefaultSystemDescription.AddTableNames(PSParams: TBoldPSDefaultParams);
  procedure AddTableNamesUsingTable;
  var
    i: integer;
    Table: IBoldTable;
  begin
    table := PSParams.DataBase.GetTable;
    with Table do
    try
      TableName := TableTable.SQLName;
      Open;
      for i := 0 to SQLTablesList.Count - 1 do
        begin
          Append;
          FieldValues['TABLENAME'] := SQLTablesList[i].SQLName; // do not localize
          Post;
        end;
      Close;
    finally
      PSParams.DataBase.ReleaseTable(Table);
    end;
  end;

  procedure AddTableNamesUsingQuery;
  var
    Q: IBoldExecQuery;
    i: integer;
  begin
    q := PSParams.DataBase.GetExecQuery;
    try
      q.AssignSQLText(format(
        'INSERT INTO %s (TABLENAME) VALUES (:TABLENAME)', // do not localize
          [TableTable.SQLName] ));
      for i := 0 to SQLTablesList.Count - 1 do
      begin
        q.ParamByName('TABLENAME').AsString := SQLTablesList[i].SQLName; // do not localize
        q.ExecSQL;
      end;
    finally
      PSParams.DataBase.ReleaseExecQuery(q);
    end;
  end;

begin
  BoldLog.Log(sLogWritingTableNames);
  case EffectiveGenerationMode(PSParams) of
    dbgTable: AddTableNamesUsingTable;
    dbgQuery: AddTableNamesUsingQuery;
    else raise EBold.CreateFmt(sUnknownGenerationMode, [ClassName, 'AddTableNames']); // do not localize
  end;
end;

procedure TBoldDefaultSystemDescription.CreatePersistentStorage(PSParams: TBoldPSParams);
var
  PSParamsDefault: TBoldPSDefaultParams;
begin
  inherited;
  if BoldLog.ProcessInterruption then
    exit;
  BoldLog.LogHeader := sLogInitializingDefaultPS;
  PSParamsDefault := PSParams as TBoldPSDefaultParams;
  if EffectiveUseTransactions(PSParamsDefault) then
    PSParamsDefault.Database.StartTransaction;
  try
    AddFirstID(PSParamsDefault);
    if assigned(TimeStampTable) then
      AddFirstTimeStamp(PSParamsDefault);
    if assigned(LastClockTable) then
      AddFirstClock(PSParamsDefault);
    AddTableNames(PSParamsDefault);
    BoldLog.Separator;
  finally
    if EffectiveUseTransactions(PSParamsDefault) then
    begin
      BoldLog.Separator;
      BoldLog.Log(sCommittingInitialData);
      PSParamsDefault.Database.Commit;
    end;
  end;
end;

procedure TBoldDefaultSystemDescription.GenerateDatabaseScript(
  Script: TStrings; Separator: string);
begin
  inherited;
  GenerateScriptForFirstID(Script, Separator);
  if assigned(TimeStampTable) then
    GenerateScriptForFirstTimeStamp(Script, Separator);
  if assigned(LastClockTable) then
    GenerateScriptForFirstClock(Script, Separator);
  GenerateScriptForTableNames(Script, Separator);
end;

procedure TBoldDefaultSystemDescription.GenerateScriptForFirstClock(Script: TStrings; Separator: String);
begin
  Script.Add(Separator);
  Script.Add(format(
        'INSERT INTO %s (%s, %s) VALUES (0, %s)', // do not localize
          [LastClockTable.SQLName, LASTTIMESTAMPCOLUMN_NAME, LASTCLOCKCOLUMN_NAME, DateToStr(0)] ));
end;

procedure TBoldDefaultSystemDescription.GenerateScriptForFirstID(Script: TStrings; Separator: String);
begin
  Script.Add(Separator);
  Script.Add(format('INSERT INTO %s (%s) VALUES (1)', [IDTable.SQLName, IDCOLUMN_NAME] )); // do not localize
end;

procedure TBoldDefaultSystemDescription.GenerateScriptForFirstTimeStamp(Script: TStrings; Separator: String);
begin
  Script.Add(Separator);
  Script.Add(format(
        'INSERT INTO %s (%s) VALUES (0)', // do not localize
          [TimeStampTable.SQLName, BoldExpandName(TIMESTAMPCOLUMN_NAME, '', xtSQL, SQLDatabaseConfig.MaxDbIdentifierLength, NationalCharConversion)] ));
end;

procedure TBoldDefaultSystemDescription.GenerateScriptForTableNames(Script: TStrings; Separator: String);
var
  i: integer;
begin
  for i := 0 to SQLTablesList.Count - 1 do
  begin
    Script.Add(Separator);
    Script.Add(format(
      'INSERT INTO %s (TABLENAME) VALUES (''%s'')', // do not localize
        [TableTable.SQLName, SQLTablesList[i].SQLName]));
  end;
end;

procedure TBoldDefaultSystemDescription.InitializeKnownSystemtables(
  KnownTables: TStrings; PSParams: TBoldPSSQLParams);
var
  Query: IBoldQuery;
begin
  // Reset The Query-pointer to avoid AVs at the end of method
  Query := nil;
  // try to determine if the TablesTable exists...
  if PSParams.DataBase.TableExists(TableTable.SQLName) then
  begin
    // Load the data from the table
    Query := PSParams.DataBase.GetQuery;
    try
      Query.AssignSQLText(format('SELECT %s FROM %s', [TABLENAMECOLUMN_NAME, TableTable.SQLName])); // do not localize // do not localize
      try
        Query.Open;
        while not Query.Eof do
        begin
          KnownTables.Add(Query.Fields[0].AsString);
          Query.Next;
        end;
        Query.Close;
      except
        // silence any exceptions (this happens if the table does not exist in the database,
        // and the databaseinterfaces does not support IBoldTable
      end;
    finally
      PSParams.DataBase.ReleaseQuery(Query);
    end;
  end;
end;

procedure TBoldDefaultSystemDescription.SetIdTable(const Value: TBoldSQLTableDescription);
begin
  FIdTable := Value;
end;

procedure TBoldDefaultSystemDescription.SetRootTable(const Value: TBoldSQLTableDescription);
begin
  fRootTable := Value;
end;

procedure TBoldDefaultSystemDescription.SetTabletable(const Value: TBoldSQLTableDescription);
begin
  FTabletable := Value;
end;

procedure TBoldDefaultSystemDescription.SetTimeStamptable(const Value: TBoldSQLTableDescription);
begin
  FTimeStamptable := Value;
end;

procedure TBoldDefaultSystemDescription.SetTypeTable(const Value: TBoldSQLTableDescription);
begin
  FTypeTable := Value;
end;

procedure TBoldDefaultSystemDescription.SetXFilestable(const Value: TBoldSQLTableDescription);
begin
  FXFilestable := Value;
end;

end.
