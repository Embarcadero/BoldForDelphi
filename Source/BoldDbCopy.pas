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

  TBoldDbCopyProgressEvent = procedure(Sender: TBoldDbCopy; AProgress: integer) of object;
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
    fAllTables: TStringList;
    fTotalRecords: integer;
    FOnComplete: TNotifyEvent;
    fProgressEvent: TBoldDbCopyProgressEvent;
    procedure SetDestinationPersistenceHandle(const Value: TBoldAbstractPersistenceHandleDB);
    procedure SetSourcePersistenceHandle(const Value: TBoldAbstractPersistenceHandleDB);
    procedure SetThreadCount(const Value: integer);
    procedure DoOnComplete;
    procedure ProcessTables;
    procedure SetOnComplete(const Value: TNotifyEvent);
  protected
    procedure DoOnProgress(AProcessedRecords: Integer);
  public
    procedure Run;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property TableQueue: TBoldThreadSafeStringQueue read fTableQueue;
    property SourcePersistenceHandle: TBoldAbstractPersistenceHandleDB read fSourcePersistenceHandle write SetSourcePersistenceHandle;
    property DestinationPersistenceHandle: TBoldAbstractPersistenceHandleDB read fDestinationPersistenceHandle write SetDestinationPersistenceHandle;
    property ThreadCount: integer read fThreadCount write SetThreadCount;
    property OnComplete: TNotifyEvent read FOnComplete write SetOnComplete;
    property OnProgress: TBoldDbCopyProgressEvent read fProgressEvent write fProgressEvent;
    property TotalTables: integer read fTotalTables;
    property TotalRecords: integer read fTotalRecords;
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
  Winapi.ActiveX, BoldLogHandler, System.Math, Uni;

{ TBoldDbCopy }

procedure TBoldDbCopy.AfterConstruction;
begin
  inherited;
  fTableQueue := TBoldThreadSafeStringQueue.Create('TableQueue');
  ThreadCount := 4;
  fThreadList := TThreadList.Create;
  fAllTables := TStringList.Create;
end;

procedure TBoldDbCopy.BeforeDestruction;
begin
  fTableQueue.free;
  fAllTables.free;
  fThreadList.Free;
  inherited;
end;

procedure TBoldDbCopy.DoOnComplete;
begin
  if Assigned(FOnComplete) then
    FOnComplete(self);
end;

procedure TBoldDbCopy.DoOnProgress(AProcessedRecords: Integer);
begin
  if Assigned(fProgressEvent) then
    fProgressEvent(self, AProcessedRecords);
end;

procedure TBoldDbCopy.ProcessTables;

var
  SourceQuery: IBoldQuery;
  MultiRowInsertLimit: Integer;
  MaxBatchQueryParams: Integer;
  DestinationTable: TBoldSQLTableDescription;
  DestinationQuery: IBoldExecQuery;
  Columns: string;
  RemainingRecords: integer;
  ParamIndex: integer;

  function CalcBatchSize: integer;
  begin
    var sl := TStringList.Create;
    var sl2 := TStringList.Create;
    var ProcessedRecords := 0;
    var vRecNo := 0;
    var ParamCount:= 0;
    Result := Min(RemainingRecords, MultiRowInsertLimit);
    for var _j := 0 to Result-1 do
    begin
      for var i := 0 to DestinationTable.ColumnsList.Count-1 do
      begin
        sl.Add(':p'+ _j.ToString + '_' + i.ToString);
        inc(ParamCount);
      end;
      sl2.Add(Trim('(' + Trim(sl.CommaText) + ')'));
      sl.Clear;
      if ParamCount >= MaxBatchQueryParams then
      begin
        Result := _j;
        break;
      end;
    end;
    (SourceQuery.AsDataSet as TUniQuery).FetchRows := result;
    sl2.QuoteChar := ' ';
    sl2.StrictDelimiter := false;
    var Values := sl2.DelimitedText;
    var InsertSql := Format('insert into %s (%s) values %s;', [DestinationTable.SQLName, Columns, Values]);
    DestinationQuery.ClearParams;
    DestinationQuery.ParamCheck := true;
    DestinationQuery.SQLText := InsertSql;
    DestinationQuery.Prepare;
    ParamIndex := 0;
    sl.free;
    sl2.free;
  end;

begin
  var InsertSql: string;
  var Values: string;
  var Field: IBoldField;
  var SourceDatabaseInterface := SourcePersistenceHandle.DatabaseInterface.CreateAnotherDatabaseConnection;
  (SourceDatabaseInterface.Implementor as TUniConnection).SpecificOptions.Values['ApplicationIntent'] := 'aiReadOnly';
  SourceDatabaseInterface.Open;
  SourceQuery := SourcePersistenceHandle.DatabaseInterface.CreateAnotherDatabaseConnection.GetQuery;
  SourceQuery.UseReadTransactions := false;
  (SourceQuery.AsDataSet as TUniQuery).SpecificOptions.Values['FetchAll'] := 'false';
  (SourceQuery.AsDataSet as TUniQuery).SpecificOptions.Values['SQL Server.FetchAll'] := 'false';
  (SourceQuery.AsDataSet as TUniQuery).UniDirectional := true;
  (SourceQuery.AsDataSet as TUniQuery).ReadOnly := true;
  var DatabaseInterface := DestinationPersistenceHandle.DatabaseInterface.CreateAnotherDatabaseConnection;
  DatabaseInterface.Open;
  DestinationQuery := DatabaseInterface.GetExecQuery;
  var TestQuery := DatabaseInterface.GetQuery;
  var sl := TStringList.Create;
  var sl2 := TStringList.Create;
  try
    repeat
      var SourceTableName := fTableQueue.Dequeue;
      if SourceTableName = '' then
        exit;
      var SourceRecordCount := fAllTables.Values[SourceTableName].ToInteger;
      var DestinationPersistenceMapper := DestinationPersistenceHandle.PersistenceControllerDefault.PersistenceMapper;
      MultiRowInsertLimit := DestinationPersistenceMapper.SQLDataBaseConfig.MultiRowInsertLimit;
      MaxBatchQueryParams := DestinationPersistenceMapper.SQLDataBaseConfig.MaxBatchQueryParams;
      DestinationTable := DestinationPersistenceMapper.AllTables.ItemsBySQLName[SourceTableName];
      Columns := DestinationTable.ColumnsList.ToString;
      var SelectSql := Format('select %s from %s', [Columns, SourceTableName]);
//      MultiRowInsertLimit := 20;
//      MaxBatchQueryParams := 10;
      BoldLog.LogHeader := Format('Loading from %s', [SourceTableName]);
      (SourceQuery.AsDataSet as TUniQuery).FetchRows := MultiRowInsertLimit;
      SourceQuery.SQLText := SelectSql;
      SourceQuery.Open;
      var i,j: integer;
      sl.Clear;
      sl2.Clear;
      j := SourceRecordCount;
      BoldLog.LogHeader := Format('%d records loaded from %s', [j, SourceTableName]);
      BoldLog.Log(Format('%d records in table %s', [j, DestinationTable.SQLName]));
      BoldLog.ProgressMax := j;

            TestQuery.SQLText := 'select count(*) from ' + SourceTableName;
            TestQuery.Open;
            if TestQuery.Fields[0].AsInteger > 0 then
            begin
              DestinationQuery.SQLText := 'delete from ' + SourceTableName;
              DestinationQuery.ExecSQL;
              TestQuery.Close;
              TestQuery.Open;
            end;
            TestQuery.Close;
            if DatabaseInterface.InTransaction then
              DatabaseInterface.Commit;

      DatabaseInterface.StartTransaction;
      RemainingRecords := SourceRecordCount;

      var ProcessedRecords := 0;
      var vRecNo := 0;
      var s: string;
      var Bytes: TBytes;
      var bc: Integer;
      bc := 0;
      var Batch := CalcBatchSize;
      repeat
        begin
          if RemainingRecords <= batch then
            Batch := CalcBatchSize;
          repeat
            if ParamIndex = DestinationQuery.ParamCount then
              break;
            for i := 0 to SourceQuery.FieldCount-1 do
            begin
              var Param := DestinationQuery.Param[ParamIndex];
              inc(ParamIndex);
              Param.AssignFieldValue(SourceQuery.Fields[i]);
              if Param.DataType = ftWideMemo then
              begin
                s := Trim(SourceQuery.Fields[i].AsString);
                for var x := Length(s)-1 downto 1 do
                  if s[x].IsControl then
                    Delete(s, x, 1);
                Bytes := TEncoding.UTF8.GetBytes(s);
                Param.AsString := TEncoding.UTF8.GetString(Bytes);
              end;
            end;
            inc(bc);
            SourceQuery.Next;
            inc(vRecNo);
            inc(ProcessedRecords);
            dec(RemainingRecords);
          until RemainingRecords = 0;
          Assert(ParamIndex = DestinationQuery.ParamCount);
          try
            DestinationQuery.ExecSQL;
            bc := 0;
            ParamIndex := 0;
            DatabaseInterface.Commit;
            DatabaseInterface.StartTransaction;
            DoOnProgress(vRecNo);
            vRecNo := 0;
            var Estimate := IncSecond(fStartTime, Round(SecondsBetween(fStartTime, now) * (j / ProcessedRecords)));
            BoldLog.Progress := ProcessedRecords;
            BoldLog.LogHeader := Format('%d/%d records processed in table %s', [ProcessedRecords,j, DestinationTable.SQLName]);
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
      until RemainingRecords = 0;
      if DatabaseInterface.InTransaction then
        DatabaseInterface.Commit;

      TestQuery.SQLText := 'select count(*) from ' + SourceTableName;
      TestQuery.Open;
      Assert(TestQuery.Fields[0].AsInteger = SourceRecordCount);
      if DatabaseInterface.InTransaction then
        DatabaseInterface.Commit;

    until fTableQueue.Empty;
  finally
    sl.free;
    sl2.Free;
    SourceDatabaseInterface.ReleaseQuery(SourceQuery);
    DatabaseInterface.ReleaseExecQuery(DestinationQuery);
    SourceDatabaseInterface.ReleaseQuery(TestQuery);
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

  for SourceTable in SourcePersistenceMapper.AllTables do
  begin
    var SelectSql := Format('select count(*) from %s', [SourceTable.SQLName]);
    SourceQuery.SQLText := SelectSql;
    SourceQuery.Open;
    if SourceQuery.Fields[0].AsInteger = 0 then
      continue;
    inc(fTotalRecords, SourceQuery.Fields[0].AsInteger);
    fAllTables.Values[SourceTable.SQLName] := SourceQuery.Fields[0].AsString;
    SourceQuery.Close;
  end;
  fAllTables.CustomSort(SortByDescendingRowCount);
//  var q := vTables.IndexOfName('Person');
//    vTables.Move(q, 0);
  for var i := 0 to fAllTables.Count-1 do
    fTableQueue.Enqueue(fAllTables.Names[i]);
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

