unit DBConverter;

interface

uses
  BoldLogHandler,
  BoldQueryUserDlg,
  classes,
  Forms,
  Db,
  DbTables;

type
  TBoldBatchMover = class;
  TBoldBatchMoverClass = class of TBoldBatchMover;
  TBoldDbCopyConverter = class;

  { TBoldBatchMover }
  TBoldBatchMover = class
  private
    fCopiedTablesWithType: TStrings;
    fFromTable: TTable;
    fToTable: TTable;
    fName: string;
    fNewDB: TDatabase;
    fOldDB: TDatabase;
    fLogLevel: integer;
  protected
    function BuildSQLForFromQuery(ToTable: TTable): string; virtual;
    function CompatibleFields(ToFieldDef: TFieldDef; FromField: TField): boolean; virtual;
    function ContainsBlobs: boolean;
    procedure CopyManually( OnlyLog : Boolean );
    procedure CopyUsingBatchMove( OnlyLog : Boolean );
    function DefaultForField(TableName: string; Field: TFieldDef; InQueryBuilding : Boolean): string; virtual;
    procedure OpenTables;
    procedure CloseTables;
    procedure ClearTargetTable;
    property NewDB: TDatabase read fNewDB;
    property OldDB: TDatabase read fOldDB;
    property LogLevel : integer read fLogLevel;
    property FromTable: TTable read fFromTable;
    property ToTable: TTable read fToTable;
  public
    constructor Create(OldDatabase: TDatabase; NewDatabase: TDatabase; LogLevel : integer); virtual;
    destructor Destroy; override;
    procedure CopyTable(const Name: string; OnlyLog : Boolean );
    property CopiedTablesWithType: TStrings read fCopiedTablesWithType;
  end;

  { TBoldDbCopyConverter }
  TBoldDbCopyConverter = class
  private
    fCopiedTablesWithType: TStrings;
    fNewDatabase: TDatabase;
    fOldDatabase: TDatabase;
    fMoverClass: TBoldBatchMoverClass;
    fOnlyLog: Boolean;
    fSourceSystemPrefix: String;
    fTargetSystemPRefix: String;
    fLogLevel: integer;
  protected
    procedure GetClassIds(Database: TDatabase; Mapping: TStrings; SystemTablePrefix : String);
    procedure AdjustClassIdsForBold1x;
    procedure AdjustClassIds;
    procedure CopyAllCommonTables;
    procedure CopyTable(const Name: string);  {Note, also maintains CopiedTablesWithtype}
    function GetMoverClass: TBoldBatchMoverClass; virtual;
    property OldDatabase: TDatabase read fOldDatabase;
    property NewDatabase: TDatabase read fNewDatabase;
    property OnlyLog : Boolean read fOnlyLog;
  public
    constructor Create(OldDatabase: TDatabase; NewDatabase: TDatabase);
    destructor Destroy; override;
    procedure PerformCopy( OnlyLog : Boolean; SourceSystemPrefix, TargetSystemPrefix : String; LogLevel : integer; TargetIsBold2 : Boolean );
    property MoverClass: TBoldBatchMoverClass read GetMoverClass write fMoverClass;
    property SourceSystemPrefix : String read fSourceSystemPrefix;
    property TargetSystemPrefix : String read fTargetSystemPRefix;
    property LogLevel : integer read fLogLevel;
  end;

const
  BlobFieldTypes: set of TFieldType = [ftBlob, ftGraphic, ftParadoxOle, ftDBaseOle, ftTypedBinary];

implementation

uses
  SysUtils,
  BoldUtils;

var
  LastClearTargetTableQueryResult: TBoldQueryResult;

function Max(const int1, int2: integer): integer;
begin
  if Int1 > Int2 then
    Result := Int1
  else
    Result := Int2;
end;

{ TBoldBatchMover }
constructor TBoldBatchMover.Create(OldDatabase: TDatabase; NewDatabase: TDatabase; LogLevel : integer);
begin
  inherited Create;
  fLoglevel := LogLevel;
  fNewDB := NewDatabase;
  fOldDB := OldDatabase;
  fFromTable := TTable.Create(nil);
  fToTable := TTable.Create(nil);
  FromTable.DatabaseName := OldDB.DatabaseName;
  ToTable.DatabaseName := NewDB.DatabaseName;

  fCopiedTablesWithType := TStringList.Create;
end;

destructor TBoldBatchMover.Destroy;
begin
  FreeAndNil(fFromTable);
  FreeAndNil(fToTable);
  FreeAndNil(fCopiedTablesWithType);
  inherited;
end;

function TBoldBatchMover.ContainsBlobs: boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to FromTable.FieldDefs.Count - 1 do
  begin
    Result := (FromTable.FieldDefs[i].DataType in BlobFieldTypes);
    if Result then Exit;
  end;

  for i := 0 to ToTable.FieldDefs.Count - 1 do
  begin
    Result := (ToTable.FieldDefs[i].DataType in BlobFieldTypes);
    if Result then Exit;
  end;
end;

procedure TBoldBatchMover.OpenTables;
begin
  FromTable.TableName := fName;
  FromTable.Open;
  ToTable.TableName := fName;
  ToTable.Open;
end;

procedure TBoldBatchMover.CloseTables;
begin
  FromTable.Close;
  ToTable.Close;
end;

//This method may be overridden to utilize a more proper compatibility-check algorithm
function TBoldBatchMover.CompatibleFields(ToFieldDef: TFieldDef; FromField: TField): boolean;
function FieldsAllowConversion(FromType, ToType: TFieldType): boolean;
begin
  Result := (FromField.DataType = FromType) and (ToFieldDef.DataType = ToType);
end;
begin
  Result := (ToFieldDef.DataType = FromField.DataType) or // Allow same type to same type
            FieldsAllowConversion(ftCurrency, ftFloat); // allow currency => float
end;

{This method can be modified to provide suitable defaults}
function TBoldBatchMover.DefaultForField(TableName: string; Field: TFieldDef; InQueryBuilding : Boolean ): string;
begin
  // The default is to set all new data to NULL
  if Field.Datatype = ftString then
    Result := ''''''
  else
    Result := 'NULL';

  // If NULL is not allowed, find the "emptiest" value
  if Field.Required then
    // It would be possible to extend this table to include more types
    case Field.DataType of
      ftUnknown:      ;
      ftString:       Result := '''''';
      ftSmallint,
      ftInteger,
      ftWord:         Result := '0';
      ftBoolean:      ;
      ftFloat,
      ftCurrency,
      ftBCD:          Result := '0';
      ftDate:         ;
      ftTime:         ;
      ftDateTime:     ;
      ftBytes:        ;
      ftVarBytes:     ;
      ftAutoInc:      ;
      ftBlob:         Result := '''''';
      ftMemo:         Result := '''''';
      ftGraphic:      ;
      ftFmtMemo:      ;
      ftParadoxOle:   ;
      ftDBaseOle:     ;
      ftTypedBinary:  ;
    end;
  if InQueryBuilding then
    BoldLog.LogFmt('New member. Default value: %s.%s --> %s', [TableName, Field.Name, Result]);
end;

function TBoldBatchMover.BuildSQLForFromQuery(ToTable: TTable): string;
var
  FieldName: string;
  i: integer;
  FromField: TField;
begin
  ToTable.FieldDefs.Update;

  Result := 'SELECT ';
  for i := 0 to ToTable.FieldDefs.Count - 1 do
  begin
    FieldName := ToTable.FieldDefs[i].Name;
    FromField := FromTable.FindField(FieldName);
    if i > 0 then
      Result := Result + ', ';

    if Assigned(FromField) and CompatibleFields(ToTable.FieldDefs[i], FromField) then
      Result := Result + FieldName
    else
      Result := result + DefaultForField(ToTable.TableName, ToTable.FieldDefs[i], true) + ' as ' + FieldName;
  end;
  Result := Result + Format(' FROM %s', [FromTable.TableName]);
end;

procedure TBoldBatchMover.CopyUsingBatchMove( OnlyLog : Boolean );
var
  FromQuery: TQuery;
  BatchMove: TBatchMove;
begin
  FromQuery := TQuery.Create(nil);
  BatchMove := TBatchMove.Create(nil);
  try
    FromQuery.DatabaseName := OldDB.DatabaseName;
    FromQuery.SQL.Text := BuildSQLForFromQuery(ToTable);
    BatchMove.Mode := batAppend;
    BatchMove.Source := FromQuery;
    BatchMove.Destination := ToTable;

    try
      fromQuery.Open;
      if Assigned(FromQuery.FindField('BOLD_TYPE')) then
        fCopiedTablesWithType.Append(ToTable.TableName);
      FromQuery.Close;
    except
    end;

    if fName = 'BOLD_ID' then
      BatchMove.Mode := batCopy;

    try
      if not OnlyLog then
        BatchMove.Execute;
      if LogLevel > 0 then
        BoldLog.LogFmtDedent('Completed %s: copied %d records', [fName, BatchMove.MovedCount]);
    except
      on e: Exception do
        BoldLog.LogFmtDedent('ERROR: %s: %s (table data not converted)', [fName, E.Message]);
    end;

  finally
    FromQuery.Free;
    BatchMove.Free;
  end;
end;

procedure TBoldBatchMover.CopyManually( OnlyLog : Boolean );
var
  FromField: TField;
  FromQuery: TQuery;
  i: integer;
  MovedCount : integer;
begin
  FromQuery := TQuery.Create(nil);
  try
    FromQuery.DatabaseName := OldDB.DatabaseName;
    FromQuery.SQL.Text := BuildSQLForFromQuery(Totable);
    FromQuery.Open;
    MovedCount := 0;
    if not OnlyLog then
    begin
      while not FromQuery.EOF do
      begin
        ToTable.Append;

        for i := 0 to ToTable.FieldCount - 1 do
        begin
          FromField := FromQuery.FindField(ToTable.Fields[i].FieldName);
          if Assigned(FromField) then
            ToTable.Fields[i].AsVariant := FromField.AsVariant
          else
            ToTable.Fields[i].AsVariant := DefaultForField(fName, ToTable.FieldDefs[i], false);
        end;
        toTable.Post;
        FromQuery.Next;
        Inc( MovedCount );
      end;
    end;
    if LogLevel > 0 then
      BoldLog.LogFmtDedent('Completed %s: copied %d records', [fName, MovedCount]);
    if Assigned(FromQuery.FindField('BOLD_TYPE')) then
      fCopiedTablesWithType.Append(ToTable.TableName);
    FromQuery.Close;
  finally
    FromQuery.Free;
  end;
end;

procedure TBoldBatchMover.CopyTable(const Name: string; OnlyLog : Boolean );
begin
  fName := Name;
  OpenTables;
  ClearTargetTable;
  if ContainsBlobs then
    CopyManually( OnlyLog )
  else
    CopyUsingBatchMove( OnlyLog );
  CloseTables;
end;

{ TBoldDbCopyConverter }
constructor TBoldDbCopyConverter.Create(OldDatabase, NewDatabase: TDatabase);
begin
  inherited Create;
  fOldDatabase := OldDatabase;
  fNewDatabase := NewDatabase;
  fCopiedTablesWithType:= TStringList.Create;
  BoldLog.Show;
  LastClearTargetTableQueryResult := qrNo;
end;

destructor TBoldDbCopyConverter.Destroy;
begin
  fCopiedTablesWithType.Free;
  fCopiedTablesWithType := nil;
  inherited;
end;

function TBoldDbCopyConverter.GetMoverClass: TBoldBatchMoverClass;
begin
  Result := fMoverClass;
  if not Assigned(Result) then
    Result := TBoldBatchMover;
end;

procedure TBoldDbCopyConverter.AdjustClassIdsForBold1x;
var
  OldMapping: TStrings;
  NewMapping: TStrings;
  TempMapping: TStrings;

  procedure FillTempMapping;
  var
    i: integer;
    MinFree: integer;
    Name: string;
  begin
    MinFree := 0;
    for i := 0 to NewMapping.Count-1 do
      MinFree := Max(MinFree, StrToInt(NewMapping.Values[NewMapping.Names[i]]) + 1);

    for i := 0 to OldMapping.Count-1 do
      MinFree := Max(MinFree, StrToInt(OldMapping.Values[OldMapping.Names[i]]) + 1);

    for i := 0 to NewMapping.Count-1 do
    begin
      Name := NewMapping.Names[i];
      if OldMapping.Values[Name] = NewMapping.Values[Name] then
        TempMapping.Append(NewMapping.Strings[i])
      else
      begin
        TempMapping.Append(Name + '=' + IntToStr(MinFree));
        Inc(MinFree);
      end;
    end;
  end; {proc}

  procedure RemapIds(FromMapping, ToMapping: TStrings);
  var
    t :integer;
    i: integer;
    TypeQuery : TQuery;
    Query: TQuery;
    TableName: string;
    ClassName: string;
    FromValue: string;
    ToValue: string;
    LogText : String;
    FoundTypes : TStringList;
    CurrentTableLogged : Boolean;
  begin
    Query := TQuery.Create(nil);
    TypeQuery := TQuery.Create(nil);
    FoundTypes := TStringList.Create;
    try
      Query.DatabaseName := NewDatabase.DatabaseName;
      TypeQuery.DatabaseName := NewDatabase.DatabaseName;
      BoldLog.ProgressMax := fCopiedTablesWithType.Count-1;

      for t := 0 to fCopiedTablesWithType.Count-1 do
      begin
        BoldLog.Progress := t;
        application.ProcessMessages;
        CurrentTableLogged := false;
        tableName := fCopiedTablesWithType[t];
        TypeQuery.SQL.Text := 'SELECT DISTINCT BOLD_TYPE FROM '+TableName;
        TypeQuery.Open;
        TypeQuery.First;
        FoundTypes.Clear;
        while not Typequery.eof do
        begin
          FoundTypes.Add( TypeQuery.FieldByName('BOLD_TYPE').AsString );
          TypeQuery.Next;
        end;
        TypeQuery.Close;
        for i := 0 to FromMapping.Count-1 do
        begin
          ClassName := FromMapping.Names[i];
          FromValue := FromMapping.values[ClassName];
          if FoundTypes.IndexOf( FromValue ) = -1 then
            continue;
          Query.SQL.Clear;
          if ToMapping.IndexOfName(ClassName) = -1 then
          begin
            LogText := Format( 'Deleting objects of class %s', [ClassName] );
            Query.SQL.Text := Format('DELETE FROM %s WHERE BOLD_TYPE=%s', [TableName, FromValue])
          end
          else
          begin
            ToValue := ToMapping.values[ClassName];
            if ToValue <> FromValue then
            begin
              LogText := format('Renumbering type of %s from %s to %s', [ClassName, fromvalue, tovalue] );
              Query.SQL.Text := Format('UPDATE %s SET BOLD_TYPE=%s WHERE BOLD_TYPE=%s', [TableName, ToValue, FromValue]);
            end;
          end;
          if Query.SQL.count <> 0 then
          begin
            if LogLevel > 0 then
            begin
              if not CurrentTableLogged then
                BoldLog.LogFmt( 'Processing Table %s', [TableName] );
              BoldLog.Log( LogText );
            end;
            CurrentTableLogged := true;
            Query.ExecSQL;
            if Loglevel > 0 then
              BoldLog.LogFmt( '%d rows affected', [Query.RowsAffected] );
          end;
        end;
      end;
    finally
      Query.Free;
      typeQuery.Free;
      FoundTypes.Free;
    end;
  end; {proc}

begin
  OldMapping := TStringList.Create;
  NewMapping := TStringList.Create;
  TempMapping := TStringList.Create;
  try
    GetClassIds(OldDatabase, OldMapping, SourceSystemPrefix);
    GetClassIds(NewDatabase, NewMapping, TargetSystemPrefix);
    FillTempMapping;
    BoldLog.LogHeader := 'Processing TypeChanges, pass 1 of 2';
    ReMapIds(OldMapping, TempMapping);
    BoldLog.LogHeader := 'Processing TypeChanges, pass 2 of 2';
    RemapIds(TempMapping, NewMapping);
  finally
    Oldmapping.Free;
    NewMapping.Free;
    TempMapping.Free;
  end;
end;

procedure TBoldDbCopyConverter.CopyAllCommonTables;
var
  OldTableNames: TStringList;
  NewTableNames: TStringList;
  CopyTableNames: TStringList;
  i: integer;
  Name: string;
begin
  OldTableNames := TStringList.Create;
  NewTableNames := TStringList.Create;
  NewTableNames.Sorted := true; //For faster lookups
  CopyTableNames := TStringList.Create;
  try
    OldDatabase.Session.GetTableNames(OldDatabase.DatabaseName, '', false, false, OldTableNames);
    NewDatabase.Session.GetTableNames(NewDatabase.DatabaseName, '', false, false, NewTableNames);

    if OldTableNames.IndexOf(SourceSystemPRefix + '_TYPE') = -1 then
      raise Exception.Create('Old database not a Bold database');

    if NewTableNames.IndexOf(TargetSystemPrefix + '_TYPE') = -1 then
      raise Exception.Create('New database not a Bold database');

    BoldLog.Log('Identifying tables...');
    BoldLog.LogHeader := 'Identifying tables...';
    for i := 0 to OldTableNames.Count -1 do
    begin
      Name := OldTableNames[i];
      if (NewTableNames.IndexOf(Name) <> -1) and
        (AnsiCompareText(Name, SourceSystemPrefix + '_TYPE') <> 0) and
        (AnsiCompareText(Name, SourceSystemPrefix + '_TABLES') <> 0) then
        CopyTableNames.Append(Name);
    end;

    BoldLog.Log( 'Moving Data...' );
    BoldLog.Progress := 0;
    BoldLog.ProgressMax := CopyTableNames.Count;
    BoldLog.LogHeader := 'Moving Data...';
    for i := 0 to CopytableNames.Count-1 do
    begin
      if Loglevel > 0 then
        BoldLog.LogFmtIndent('Processing: %s', [CopyTableNames[i]]);
      BoldLog.LogHeader := format( 'Processing: %s', [CopyTableNames[i]]);
      Application.ProcessMessages;
      CopyTable(CopyTableNames[i]);
      BoldLog.Progress := i;
    end;
   finally
     OldTableNames.Free;
     NewTableNames.Free;
     CopytableNames.Free;
   end;
end;

procedure TBoldDbCopyConverter.CopyTable(const Name: string);
var
  Mover : TBoldBatchMover;
begin
  Mover := MoverClass.Create(OldDatabase, NewDatabase, LogLevel);
  try
    Mover.CopyTable(Name, OnlyLog);
    fCopiedTablesWithType.AddStrings(Mover.CopiedTablesWithType);
  finally
    Mover.Free;
  end;
end;

procedure TBoldDbCopyConverter.PerformCopy( OnlyLog : Boolean; SourceSystemPrefix, TargetSystemPrefix : String; LogLevel : integer; TargetIsBold2 : Boolean );
begin
  fOnlyLog := OnlyLog;
  fLogLevel := LogLevel;
  fSourceSystemPrefix := SourceSystemPrefix;
  fTargetSystemPRefix := TargetSystemPrefix;
  CopyAllCommonTables;
  if not OnlyLog then begin
    if TargetIsBold2 then
      AdjustClassIds
    else
      AdjustClassIdsForBold1x;
  end;
  BoldLog.Separator;
  BoldLog.Log('Complete');
end;

procedure TBoldDbCopyConverter.GetClassIds(Database: TDatabase; Mapping: TStrings; SystemTablePrefix : String);
var
  Table: TTable;
begin
  Table := TTable.Create(nil);
  try
    Table.DatabaseName := Database.DatabaseName;
    Table.TableName := SystemTablePrefix + '_TYPE';
    Table.Open;
    Table.First;
    while not Table.EOF do
    begin
      Mapping.Append(Table.FieldByName('CLASSNAME').AsString + '=' + IntToStr(Table.FieldByName('BOLD_TYPE').AsInteger));
      Table.Next;
    end;
    Table.Close;
  finally
    Table.Free;
  end;
end;

procedure TBoldDbCopyConverter.AdjustClassIds;
var
  OldMapping: TStrings;
  NewMapping: TStrings;
  procedure MergeMappings( OldMapping, NewMapping : TStrings );
  var
    i : integer;
    Oldid : string;
    MaxId : Integer;
  begin
    MaxId := -MaxInt;
    for i := OldMapping.count-1 downto 0 do
    begin
      Oldid := OldMapping.Values[OldMapping.Names[i]];

      // log highest used ID (even deleted classes, since they will be transferred to the new db id there is a table)
      if StrToInt(OldId) > MaxId then
          MaxId := StrToInt( OldId );

      // If the classname is not in the new db, then remove the mapping.
      if NewMapping.Values[OldMapping.Names[i]] = '' then
        OldMapping.Delete(i)
    end;

    // add all new classes that are not in the old mapping
    for i := NewMapping.count-1 downto 0 do
    begin
      OldId := OldMapping.Values[NewMapping.Names[i]];
      if OldId = '' then
      begin
        Inc(MaxId);
        OldMapping.add( format( '%s=%d', [NewMapping.Names[i], MaxId ] ));
      end;
    end;
  end;

  procedure StoreMapping( DataBase : TDataBase; Mapping : TStrings; SystemTablePrefix : String );
  var
    i : integer;
    Query : TQuery;
  begin
    Query := TQuery.Create(nil);
    try
      Query.DatabaseName := DataBase.DatabaseName;
      Query.SQL.Text := Format( 'DELETE FROM %s_TYPE', [SystemTablePrefix] );
      Query.ExecSQL;
      Query.SQL.Text := Format( 'INSERT INTO %s_TYPE (BOLD_TYPE, CLASSNAME) VALUES (:BOLD_TYPE, :CLASSNAME)', [SystemTablePrefix] );
      for i := 0 to Mapping.count-1 do
      begin
        Query.ParamByName( 'BOLD_TYPE' ).AsString := Mapping.Values[Mapping.Names[i]];
        Query.ParamByName( 'CLASSNAME' ).AsString := Mapping.Names[i];
        Query.ExecSQL;
      end;
    finally
      Query.Free;
    end;
  end;

begin
  OldMapping := TStringList.Create;
  NewMapping := TStringList.Create;
  try
    GetClassIds(OldDatabase, OldMapping, SourceSystemPrefix);
    GetClassIds(NewDatabase, NewMapping, TargetSystemPrefix);
    MergeMappings( OldMapping, NewMapping );
    StoreMapping( NewDatabase, OldMapping, TargetSystemPrefix );
  finally
    Oldmapping.Free;
    NewMapping.Free;
  end;
end;

procedure TBoldBatchMover.ClearTargetTable;
var
  TestQuery: TQuery;
  count: integer;
begin
  if LastClearTargetTableQueryResult = qrNoAll then
    Exit;

  TestQuery := TQuery.Create(nil);
  try
    TestQuery.DataBaseName := NewDB.DatabaseName;
    TestQuery.SQL.text := format( 'SELECT COUNT(*) FROM %s', [ToTable.TableName] );
    TestQuery.Open;
    count := TestQuery.Fields[0].AsInteger;
    TestQuery.Close;
    if count <> 0 then begin
      if LastClearTargetTableQueryResult in [qrNo, qrYes] then
        LastClearTargetTableQueryResult := QueryUser(
          'DBConverter - Clean tables',
          Format( 'The table %s contains data, do you want to clear it before converting from old database?', [Totable.TableName] ));
      if LastClearTargetTableQueryResult in [qrYesAll, qrYes] then
      begin
        TestQuery.SQL.Text := 'DELETE FROM ' + ToTable.TableName;
        TestQuery.ExecSQL;
        TestQuery.Close;
      end;
    end;
  finally
    TestQuery.Free;
  end;
end;

end.




