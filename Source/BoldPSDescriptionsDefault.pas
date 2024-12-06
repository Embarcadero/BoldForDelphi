{ Global compiler directives }
{$include bold.inc}
unit BoldPSDescriptionsDefault;

interface

uses
  Classes,
  BoldPSDescriptionsSQL,
  BoldPSParams,
  BoldPSParamsSQL,
  BoldPSParamsDefault,
  BoldDBInterfaces;

type
  {---TBoldDefaultSystemDescription---}
  TBoldDefaultSystemDescription = class(TBoldSQLSystemDescription)
  private
    Query: IBoldExecQuery;
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

    procedure GenerateScriptForFirstID(Script: TStrings);
    procedure GenerateScriptForFirstTimeStamp(Script: TStrings);
    procedure GenerateScriptForFirstClock(Script: TStrings);
    procedure GenerateScriptForTableNames(Script: TStrings);
  protected
    procedure InitializeKnownSystemtables(KnownTables: TStrings; PSParams: TBoldPSSQLParams); override;
  public
    procedure CreatePersistentStorage(PSParams: TBoldPSParams); override;
    procedure GenerateDatabaseScript(Script: TStrings); override;
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
  DB,

  BoldCoreConsts,
  BoldDefs,
  BoldLogHandler,
  BoldNameExpander,
  BoldMath;

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
    vParam: IBoldParameter;
  begin
    Query.AssignSQLText(format(
      'INSERT INTO %s (%s, %s) VALUES (0, :FirstClock)',
        [LastClockTable.SQLName, LASTTIMESTAMPCOLUMN_NAME, LASTCLOCKCOLUMN_NAME] ));
    vParam := Query.FindParam('FirstClock');
    if not Assigned(vParam) then
      vParam := Query.CreateParam(ftDateTime, 'FirstClock');
    vParam.AsDateTime := 0;
    Query.ParamCheck := true;
    Query.ExecSQL;
    Query.ParamCheck := false;
  end;
  
begin
  BoldLog.Log(sLogWritingFirstClock);

  case EffectiveGenerationMode(PSParams) of
    dbgTable: AddFirstClockUsingTable;
    dbgQuery: AddFirstClockUsingQuery;
    else 
      raise EBold.CreateFmt(sUnknownGenerationMode, [ClassName, 'AddFirstClock']); // do not localize
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
  begin
    Query.AssignSQLText(format(
        'INSERT INTO %s (%s) VALUES (1)',
          [IDTable.SQLName, IDCOLUMN_NAME] ));
    Query.ExecSQL;
  end;

begin
  BoldLog.Log(sLogWritingFirstID);
  case EffectiveGenerationMode(PSParams) of
    dbgTable: AddFirstIDUsingTable;
    dbgQuery: AddFirstIDUsingQuery;
    else 
      raise EBold.CreateFmt(sUnknownGenerationMode, [ClassName, 'AddFirstID']); // do not localize
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
  begin
    Query.AssignSQLText(format(
      'INSERT INTO %s (%s) VALUES (0)',
        [TimeStampTable.SQLName, BoldExpandName(TIMESTAMPCOLUMN_NAME, '', xtSQL, SQLDatabaseConfig.MaxDbIdentifierLength, NationalCharConversion)] ));
    Query.ExecSQL;
  end;

begin
  BoldLog.Log(sLogWritingFirstTimeStamp);
  case EffectiveGenerationMode(PSParams) of
    dbgTable: AddFirstTimeStampUsingTable;
    dbgQuery: AddFirstTimeStampUsingQuery;
    else 
      raise EBold.CreateFmt(sUnknownGenerationMode, [ClassName, 'AddFirstTimeStamp']); // do not localize
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
          FieldValues['TABLENAME'] := SQLTablesList[i].SQLName;
          Post;
        end;
      Close;
    finally
      PSParams.DataBase.ReleaseTable(Table);
    end;
  end;

  procedure AddTableNamesUsingQuery;
  var
    i: integer;
    row, limit: integer;
    sb: TStringBuilder;
    sInsert: string;
  begin
    sb := TStringBuilder.Create;
    try
      sInsert := format('INSERT INTO %s (TABLENAME) VALUES ', [TableTable.SQLName]);
      limit := SQLDatabaseConfig.MultiRowInsertLimit;
      row := 0;
      for i := 0 to SQLTablesList.Count-1 do
      begin
        if row = 0 then
          sb.append(sInsert);
        sb.Append(Format('(''%s'')', [SQLTablesList[i].SQLName]));
        inc(row);
        if (row = limit) or (i = SQLTablesList.Count - 1) then
        begin
          Query.AssignSQLText(sb.ToString);
          Query.ExecSQL;
          row := 0;
          sb.clear;
        end
        else
          sb.Append(',');
      end;
    finally
      sb.free;
    end;
  end;

begin
  BoldLog.Log(sLogWritingTableNames);
  case EffectiveGenerationMode(PSParams) of
    dbgTable: AddTableNamesUsingTable;
    dbgQuery: AddTableNamesUsingQuery;
    else 
      raise EBold.CreateFmt(sUnknownGenerationMode, [ClassName, 'AddTableNames']); // do not localize
  end;
end;

procedure TBoldDefaultSystemDescription.CreatePersistentStorage(PSParams: TBoldPSParams);
var
  PSParamsDefault: TBoldPSDefaultParams;

  procedure InternalExecute;
  begin
    Query := PSParamsDefault.Database.GetExecQuery;
    Query.ParamCheck := false;
    Query.StartSQLBatch;
    if EffectiveUseTransactions(PSParamsDefault) then
      PSParamsDefault.Database.StartTransaction;
    AddFirstID(PSParamsDefault);
    if assigned(TimeStampTable) then
      AddFirstTimeStamp(PSParamsDefault);
    if assigned(LastClockTable) then
      AddFirstClock(PSParamsDefault);
    AddTableNames(PSParamsDefault);
    BoldLog.Separator;
    Query.EndSQLBatch;
  end;

begin
  inherited;
  if BoldLog.ProcessInterruption then
    exit;
  BoldLog.LogHeader := sLogInitializingDefaultPS;
  PSParamsDefault := PSParams as TBoldPSDefaultParams;
  try
    InternalExecute;
  finally
    PSParamsDefault.Database.ReleaseExecQuery(Query);
    if EffectiveUseTransactions(PSParamsDefault) then
    begin
      BoldLog.Separator;
      BoldLog.Log(sCommittingInitialData);
      PSParamsDefault.Database.Commit;
    end;
  end;
end;

procedure TBoldDefaultSystemDescription.GenerateDatabaseScript(Script: TStrings);
begin
  inherited;
  GenerateScriptForFirstID(Script);
  if assigned(TimeStampTable) then
    GenerateScriptForFirstTimeStamp(Script);
  if assigned(LastClockTable) then
    GenerateScriptForFirstClock(Script);
  GenerateScriptForTableNames(Script);
end;

procedure TBoldDefaultSystemDescription.GenerateScriptForFirstClock(Script: TStrings);
begin
  Script.Add(format(
        'INSERT INTO %s (%s, %s) VALUES (0, %s)%s',
          [LastClockTable.SQLName, LASTTIMESTAMPCOLUMN_NAME, LASTCLOCKCOLUMN_NAME, QuotedStr(DateToStr(0)),
           SQLDatabaseConfig.SqlScriptTerminator] ));
  if SQLDatabaseConfig.SqlScriptSeparator<>'' then
    Script.Add(SQLDatabaseConfig.SqlScriptSeparator);
end;

procedure TBoldDefaultSystemDescription.GenerateScriptForFirstID(Script: TStrings);
begin
  Script.Add(format('INSERT INTO %s (%s) VALUES (1)%s', [IDTable.SQLName, IDCOLUMN_NAME, SQLDatabaseConfig.SqlScriptTerminator] ));
  if SQLDatabaseConfig.SqlScriptSeparator<>'' then
    Script.Add(SQLDatabaseConfig.SqlScriptSeparator);
end;

procedure TBoldDefaultSystemDescription.GenerateScriptForFirstTimeStamp(Script: TStrings);
begin
  Script.Add(format(
        'INSERT INTO %s (%s) VALUES (0)%s',
          [TimeStampTable.SQLName,
           BoldExpandName(TIMESTAMPCOLUMN_NAME, '', xtSQL, SQLDatabaseConfig.MaxDbIdentifierLength, NationalCharConversion),
           SQLDatabaseConfig.SqlScriptTerminator] ));
  if SQLDatabaseConfig.SqlScriptSeparator<>'' then
    Script.Add(SQLDatabaseConfig.SqlScriptSeparator);
end;

procedure TBoldDefaultSystemDescription.GenerateScriptForTableNames(Script: TStrings);
var
  i: integer;
begin
  for i := 0 to SQLTablesList.Count - 1 do
  begin
    Script.Add(format(
      'INSERT INTO %s (TABLENAME) VALUES (''%s'')%s',
        [TableTable.SQLName, SQLTablesList[i].SQLName, SQLDatabaseConfig.SqlScriptTerminator]));
    if SQLDatabaseConfig.SqlScriptSeparator<>'' then
      Script.Add(SQLDatabaseConfig.SqlScriptSeparator);
  end;
end;

procedure TBoldDefaultSystemDescription.InitializeKnownSystemtables(
  KnownTables: TStrings; PSParams: TBoldPSSQLParams);
var
  Query: IBoldQuery;
begin
  Query := nil;
  if PSParams.DataBase.TableExists(TableTable.SQLName) then
  begin
    Query := PSParams.DataBase.GetQuery;
    try
      Query.AssignSQLText(format('SELECT %s FROM %s', [TABLENAMECOLUMN_NAME, TableTable.SQLName]));
      try
        Query.Open;
        while not Query.Eof do
        begin
          KnownTables.Add(Query.Fields[0].AsString);
          Query.Next;
        end;
        Query.Close;
      except

      end;
    finally
      PSParams.DataBase.ReleaseQuery(Query);
    end;
  end;
end;

procedure TBoldDefaultSystemDescription.SetIdTable(
  const Value: TBoldSQLTableDescription);
begin
  FIdTable := Value;
end;

procedure TBoldDefaultSystemDescription.SetRootTable(
  const Value: TBoldSQLTableDescription);
begin
  fRootTable := Value;
end;

procedure TBoldDefaultSystemDescription.SetTabletable(
  const Value: TBoldSQLTableDescription);
begin
  FTabletable := Value;
end;

procedure TBoldDefaultSystemDescription.SetTimeStamptable(
  const Value: TBoldSQLTableDescription);
begin
  FTimeStamptable := Value;
end;

procedure TBoldDefaultSystemDescription.SetTypeTable(
  const Value: TBoldSQLTableDescription);
begin
  FTypeTable := Value;
end;

procedure TBoldDefaultSystemDescription.SetXFilestable(
  const Value: TBoldSQLTableDescription);
begin
  FXFilestable := Value;
end;

end.