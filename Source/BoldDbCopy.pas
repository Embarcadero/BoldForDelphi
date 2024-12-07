unit BoldDbCopy;

interface

uses
  Classes,
  System.SysUtils,
  BoldAbstractPersistenceHandleDB,
  BoldThreadSafeQueue,
  System.TimeSpan;

type
  TBoldDbCopy = class;

  TBoldDbCopyProgressEvent = procedure(Sender: TBoldDbCopy; AProgress: integer; AMax: integer; AEstimatedEndTime: TDateTime) of object;
  TBoldDbCopyLogEvent = procedure(Sender: TBoldDbCopy; const aStatus: string) of object;

  TBoldDbCopy = class(TComponent)
  private
    fDestinationPersistenceHandle: TBoldAbstractPersistenceHandleDB;
    fSourcePersistenceHandle: TBoldAbstractPersistenceHandleDB;
    fThreadCount: integer;
    fThreadList: TThreadList;
    fStartTime: TDateTime;
    fTableQueue: TBoldThreadSafeStringQueue;
    fTotalTables: integer;
    FOnComplete: TNotifyEvent;
    procedure SetDestinationPersistenceHandle(const Value: TBoldAbstractPersistenceHandleDB);
    procedure SetSourcePersistenceHandle(const Value: TBoldAbstractPersistenceHandleDB);
    procedure SetThreadCount(const Value: integer);
    procedure DoOnComplete;
    procedure ProcessTables;
    procedure SetOnComplete(const Value: TNotifyEvent);
  public
    procedure Run;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property TableQueue: TBoldThreadSafeStringQueue read fTableQueue;
    property SourcePersistenceHandle: TBoldAbstractPersistenceHandleDB read fSourcePersistenceHandle write SetSourcePersistenceHandle;
    property DestinationPersistenceHandle: TBoldAbstractPersistenceHandleDB read fDestinationPersistenceHandle write SetDestinationPersistenceHandle;
    property ThreadCount: integer read fThreadCount write SetThreadCount;
    property OnComplete: TNotifyEvent read FOnComplete write SetOnComplete;
    property TotalTables: integer read fTotalTables;
  end;

  EBoldDbCopy = class(Exception);

implementation

uses
  BoldPMappersDefault,
  BoldPSDescriptionsSQL,
  BoldDBInterfaces,
  BoldDefs,
  Data.DB,
  System.DateUtils,
  System.Character,
  Winapi.ActiveX, BoldLogHandler;

{ TBoldDbCopy }

procedure TBoldDbCopy.AfterConstruction;
begin
  inherited;
  fTableQueue := TBoldThreadSafeStringQueue.Create('TableQueue');
  ThreadCount := 4;
  fThreadList := TThreadList.Create;
end;

procedure TBoldDbCopy.BeforeDestruction;
begin
  fTableQueue.free;
  fThreadList.Free;
  inherited;
end;

procedure TBoldDbCopy.DoOnComplete;
begin
  if Assigned(FOnComplete) then
    FOnComplete(self);
end;

procedure TBoldDbCopy.ProcessTables;
begin
  var InsertSql: string;
  var Values: string;
  var Field: IBoldField;
  var SourceDatabaseInterface :=  SourcePersistenceHandle.DatabaseInterface.CreateAnotherDatabaseConnection;
  SourceDatabaseInterface.Open;
  var SourceQuery :=  SourcePersistenceHandle.DatabaseInterface.CreateAnotherDatabaseConnection.GetQuery;
  SourceQuery.UseReadTransactions := false;
  var DatabaseInterface :=  DestinationPersistenceHandle.DatabaseInterface.CreateAnotherDatabaseConnection;
  DatabaseInterface.Open;
  var DestinationQuery := DatabaseInterface.GetExecQuery;
  var sl := TStringList.Create;
  try
//    DoOnLog(Format('Thread %d started', [TThread.CurrentThread.ThreadID]));
    repeat
      var SourceTableName := fTableQueue.Dequeue;
      if SourceTableName = '' then
        exit;
      var DestinationPersistenceMapper := DestinationPersistenceHandle.PersistenceControllerDefault.PersistenceMapper;
      var DestinationTable := DestinationPersistenceMapper.AllTables.ItemsBySQLName[SourceTableName];
      var Columns := DestinationTable.ColumnsList.ToString;
      var SelectSql := Format('select %s from %s', [Columns, SourceTableName]);
      BoldLog.LogHeader := Format('Loading from %s', [SourceTableName]);
      SourceQuery.SQLText := SelectSql;
      SourceQuery.Open;
      var i,j: integer;
      sl.Clear;
      for i := 0 to DestinationTable.ColumnsList.Count-1 do
        sl.Add(':'+ DestinationTable.ColumnsList[i].SQLName);
      Values := sl.CommaText;
      j := 0;
      InsertSql := Format('insert into %s (%s) values (%s)', [DestinationTable.SQLName, Columns, Values]);
      DestinationQuery.SQLText := InsertSql;
      DestinationQuery.ParamCheck := true;
      i := DestinationQuery.ParamCount;
      j := SourceQuery.FieldCount;
      var s := DestinationQuery.Params.ToString;
      if i <> j then
      begin
        for var q := 0 to SourceQuery.FieldCount-1 do
        begin
          var ss := SourceQuery.Fields[q].FieldName;
          var ds := DestinationQuery.Param[q].Name;
          if DestinationQuery.Param[q].Name <> SourceQuery.Fields[q].FieldName then
            Assert(false, Format('Source field count %d does not match destination param count %d, expected:  %s found: %s', [j, i, ss, ds]));
        end;
      end;
      j := SourceQuery.RecordCount;
      if j = 0 then
        continue;
      BoldLog.Log(Format('%d records in table %s', [j, DestinationTable.SQLName]));
      BoldLog.ProgressMax := j;
      DatabaseInterface.StartTransaction;
      var vRecNo := 0;
      var ProcessedRecords := 0;
//      var s: string;
      var Bytes: TBytes;
      while not SourceQuery.Eof do
      begin
        for i := 0 to SourceQuery.FieldCount-1 do
        begin
          DestinationQuery.Param[i].AssignFieldValue(SourceQuery.Fields[i]);
          if DestinationQuery.Param[i].DataType = ftWideMemo then
          begin
            s := Trim(SourceQuery.Fields[i].AsString);
            for var x := Length(s)-1 downto 1 do
              if s[x].IsControl then
                Delete(s, x, 1);
            Bytes := TEncoding.UTF8.GetBytes(s);
            DestinationQuery.Param[i].AsString := TEncoding.UTF8.GetString(Bytes);
          end;
        end;
        try
          SourceQuery.Next;
          DestinationQuery.ExecSQL;
          inc(vRecNo);
          inc(ProcessedRecords);
          if vRecNo = 1000 then
          begin
            vRecNo := 0;
            DatabaseInterface.Commit;
            DatabaseInterface.StartTransaction;
            var Estimate := IncSecond(fStartTime, Round(SecondsBetween(fStartTime, now) * (j / ProcessedRecords)));
            BoldLog.Progress := ProcessedRecords;
            BoldLog.LogHeader := Format('%d/%d records processed in table %s', [ProcessedRecords,j, DestinationTable.SQLName]);
          end;
        except
          on e:Exception {EBoldDatabaseError} do
          begin
            if pos('invalid byte sequence for encoding', e.Message) > 0 then
              continue
            else
            begin
              DatabaseInterface.RollBack;
              raise;
            end;
          end;
        end;
      end;
      if DatabaseInterface.InTransaction then
        DatabaseInterface.Commit;
//      DoOnProgress(ProcessedRecords, j, now);
    until fTableQueue.Empty;
  finally
    sl.free;
    SourceDatabaseInterface.ReleaseQuery(SourceQuery);
    DatabaseInterface.ReleaseExecQuery(DestinationQuery);
    SourceDatabaseInterface.Close;
    DatabaseInterface.Close;
    BoldLog.Log(Format('Thread %d completed', [TThread.CurrentThread.ThreadID]));
  end;
end;

function SortByDescendingRowCount(List: TStringList; Index1,
  Index2: Integer): Integer;
var
  i, j: integer;
begin
  i := List.ValueFromIndex[Index1].ToInteger;
  j := List.ValueFromIndex[Index2].ToInteger;
  result := j - i;
end;

procedure TBoldDbCopy.Run;
var
  SourcePersistenceMapper: TBoldSystemDefaultMapper;
  DestinationPersistenceMapper: TBoldSystemDefaultMapper;
begin
  fStartTime := now;
  SourcePersistenceHandle.Active := true;
  DestinationPersistenceHandle.Active := true;
  SourcePersistenceMapper := SourcePersistenceHandle.PersistenceControllerDefault.PersistenceMapper;
  DestinationPersistenceMapper := DestinationPersistenceHandle.PersistenceControllerDefault.PersistenceMapper;
  var SourceTable: TBoldSQLTableDescription;
  var DestinationTable: TBoldSQLTableDescription;
  var SourceColumn: TBoldSQLDescriptionElement;
  var DestinationColumn: TBoldSQLDescriptionElement;
  for SourceTable in SourcePersistenceMapper.AllTables do
  begin
    DestinationTable := DestinationPersistenceMapper.AllTables.ItemsBySQLName[SourceTable.SQLName];
    if DestinationTable = nil then
      raise EBoldDbCopy.CreateFmt('Table %s not found in destination database.', [SourceTable.SQLName]);
    for SourceColumn in SourceTable.ColumnsList do
    begin
      DestinationColumn := DestinationTable.ColumnsList.ItemsBySQLName[SourceColumn.SQLName];
      if DestinationColumn = nil then
        raise EBoldDbCopy.CreateFmt('Column %s not found in table %s in destination database.', [SourceColumn.SQLName, DestinationTable.SQLName]);
    end;
  end;

  var SourceQuery: IBoldQuery;
  SourceQuery :=  SourcePersistenceHandle.DatabaseInterface.GetQuery;

  var vTables := TStringList.Create;
  for SourceTable in SourcePersistenceMapper.AllTables do
  begin
    var SelectSql := Format('select count(*) from %s', [SourceTable.SQLName]);
    SourceQuery.SQLText := SelectSql;
    SourceQuery.Open;
    if SourceQuery.Fields[0].AsInteger = 0 then
      continue;
    vTables.Values[SourceTable.SQLName] := SourceQuery.Fields[0].AsString;
    SourceQuery.Close;
  end;
  vTables.CustomSort(SortByDescendingRowCount);
  for var i := 0 to vTables.Count-1 do
    fTableQueue.Enqueue(vTables.Names[i]);
  fTotalTables := fTableQueue.count;
  for var I := 0 to ThreadCount-1 do
  begin
    var Thread := TThread.CreateAnonymousThread(procedure
     begin
       CoInitialize(nil);
       try
         ProcessTables;
       finally
         CoUninitialize;
         fThreadList.Remove(TThread.CurrentThread);
         if fThreadList.LockList.Count = 0 then
         begin
           var Duration := TTimeSpan.Subtract(now, fStartTime);
           BoldLog.Log(Format('Operation completed after %s', [Duration.ToString]));
           DoOnComplete;
         end;
         fThreadList.UnlockList;
       end;
     end);
    fThreadList.Add(Thread);
    Thread.Start;
  end;
end;

procedure TBoldDbCopy.SetDestinationPersistenceHandle(const Value: TBoldAbstractPersistenceHandleDB);
begin
  fDestinationPersistenceHandle := Value;
end;

procedure TBoldDbCopy.SetOnComplete(const Value: TNotifyEvent);
begin
  FOnComplete := Value;
end;

procedure TBoldDbCopy.SetSourcePersistenceHandle(const Value: TBoldAbstractPersistenceHandleDB);
begin
  fSourcePersistenceHandle := Value;
end;

procedure TBoldDbCopy.SetThreadCount(const Value: integer);
begin
  fThreadCount := Value;
end;

end.

