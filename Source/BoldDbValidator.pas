{ Global compiler directives }
{$include bold.inc}
unit BoldDbValidator;

interface

uses
  Classes,
  System.Threading,
  BoldDbInterfaces,
  BoldAbstractPersistenceHandleDB,
  BoldPMappersSQL,
  BoldSubscription,
  BoldThreadSafeQueue,
  System.Generics.Collections;

type
  { forward declarations }
  TBoldDbValidator = class;
  TBoldDbValidatorThread = class;

  TCheckStopEvent = function(): boolean of object;
  TBoldDbValidatorLogEvent = procedure(Sender: TBoldDbValidator; const aStatus: string) of object;

  { TBoldDbValidator }
  TBoldDbValidator = class(TComponent)
  private
    fSystemMapper: TBoldSystemSQLMapper;
    fRemedyList: TList<String>;
    fRemedyStrings: TStringList;
    fPersistenceHandle: TBoldAbstractPersistenceHandleDB;
    FEnabled: Boolean;
    fSubscriber: TBoldPassThroughSubscriber;
    FOnCheckStop: TCheckStopEvent;
    fTableQueue: TBoldThreadSafeObjectQueue;
    FThreadCount: integer;
    fThreadList: TThreadList;
    FOnComplete: TNotifyEvent;
    fOnLog: TBoldDbValidatorLogEvent;
    function GetSystemSQLMapper: TBoldSystemSQLMApper;
    procedure SetEnabled(const Value: Boolean);
    procedure CheckTypeTableConsistency(SystemSQLMapper: TBoldSystemSQLMapper);
    function GetDataBase: IBoldDataBase;
    procedure Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    procedure SetOnCheckStop(const Value: TCheckStopEvent);
    procedure SetThreadCount(const Value: integer);
    procedure SetOnComplete(const Value: TNotifyEvent);
    function GetRemedy: TStringList;
  protected
    fStartTime: TDateTime;
    function DoCheckStop: boolean;
    procedure DeActivate; virtual;
    procedure SetPersistenceHandle(const Value: TBoldAbstractPersistenceHandleDB); virtual;
    function TypeTableName: String;
    procedure DoOnComplete;
    procedure DoOnLog(const AStatus: string);
    function CreateValidatorThread: TBoldDbValidatorThread; virtual; abstract;
    property SystemSQLMapper: TBoldSystemSQLMapper read GetSystemSQLMapper;
    property DataBase: IBoldDataBase read GetDataBase;
    property ThreadList: TThreadList read fThreadList;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure Activate;
    procedure Validate; virtual;
    procedure Execute;
    property Remedy: TStringList read GetRemedy;
    property TableQueue: TBoldThreadSafeObjectQueue read fTableQueue;
  published
    property PersistenceHandle: TBoldAbstractPersistenceHandleDB read fPersistenceHandle write SetPersistenceHandle;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property OnCheckStop: TCheckStopEvent read FOnCheckStop write SetOnCheckStop;
    property ThreadCount: integer read FThreadCount write SetThreadCount;
    property OnComplete: TNotifyEvent read FOnComplete write SetOnComplete;
    property OnLog: TBoldDbValidatorLogEvent read fOnLog write fOnLog;
  end;

  TBoldDbValidatorThread = class(TThread)
  private
    fValidator: TBoldDbValidator;
    fBoldDatabase: IBoldDatabase;
    fSystemSQLMapper: TBoldSystemSQLMapper;
    fRemedyList: TList<String>;
    fPersistenceHandle: TBoldAbstractPersistenceHandleDB;
  protected
    procedure Validate; virtual; abstract;
    function DoCheckStop: boolean;
    procedure DoOnLog(const AStatus: string);
    procedure AddRemedy(const s: string);
    property Database: IBoldDatabase read fBoldDatabase;
    property Validator: TBoldDbValidator read fValidator;
    property SystemSQLMapper: TBoldSystemSQLMapper read fSystemSQLMapper;
    property PersistenceHandle: TBoldAbstractPersistenceHandleDB read fPersistenceHandle;
  public
    constructor Create(AValidator: TBoldDbValidator); virtual;
    destructor Destroy; override;
    procedure Execute; override;
  end;

implementation

uses
  Dialogs,
  Controls,
  SysUtils,

  BoldPMappers,
  BoldLogHandler,
  BoldNameExpander,
  BoldDefs,
  BoldCoreConsts,
  UITypes, Winapi.ActiveX;

{ TBoldDbValidator }

procedure TBoldDbValidator.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
end;

procedure TBoldDbValidator.SetOnCheckStop(const Value: TCheckStopEvent);
begin
  FOnCheckStop := Value;
end;

procedure TBoldDbValidator.SetOnComplete(const Value: TNotifyEvent);
begin
  FOnComplete := Value;
end;

function TBoldDbValidator.DoCheckStop: boolean;
begin
  result := Assigned(FOnCheckStop) and FOnCheckStop;
end;

procedure TBoldDbValidator.DoOnComplete;
begin
  if Remedy.Count <> 0 then
  begin
    BoldLog.Separator;
    BoldLog.Log(sFoundInconsistencies, ltWarning);
    for var i := 0 to Remedy.Count - 1 do
      BoldLog.Log(Remedy[i], ltDetail);
    BoldLog.Separator;
  end;
  BoldLog.Log(sDBValidationDone, ltInfo);
  DeActivate;
  if Assigned(FOnComplete) then
    FOnComplete(self);
end;

procedure TBoldDbValidator.DoOnLog(const AStatus: string);
begin
  if Assigned(fOnLog) then
    fOnLog(self, AStatus);
end;

function TBoldDbValidator.GetRemedy: TStringList;
begin
  result := fRemedyStrings;
  fRemedyStrings.BeginUpdate;
  try
    fRemedyStrings.Clear;
    for var i := 0 to fRemedyList.Count-1 do
      fRemedyStrings.Add(fRemedyList[i]);
  finally
    fRemedyStrings.EndUpdate;
  end;
end;

function TBoldDbValidator.GetSystemSQLMapper: TBoldSystemSQLMApper;
begin
  if not assigned(fSystemMapper) then
  begin
    fSystemMapper := PersistenceHandle.PersistenceControllerDefault.PersistenceMapper as TBoldSystemSQLMapper;
    fSystemMapper.AddSmallSubscription(fSubscriber, [beDestroying], beDestroying);
  end;
  result := fSystemMapper;
end;

procedure TBoldDbValidator.Receive(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  if RequestedEvent = beDestroying then
  begin
    fSystemMapper := nil;
    fSubscriber.CancelAllSubscriptions;
  end;
end;

constructor TBoldDbValidator.Create(Owner: TComponent);
begin
  inherited;
  fRemedyStrings := TStringList.Create;
  fRemedyList := TList<String>.Create;
  fSubscriber := TBoldPassThroughSubscriber.Create(Receive);
  fTableQueue := TBoldThreadSafeObjectQueue.Create('TableValidationQueue');
  fThreadList := TThreadList.Create;
  FThreadCount := 6;
end;

destructor TBoldDbValidator.destroy;
begin
  FreeAndNil(fRemedyList);
  FreeAndNil(fRemedyStrings);
  FreeAndNil(fSubscriber);
  FreeAndNil(fTableQueue);
  FreeAndNil(fThreadList);
  inherited;
end;

procedure TBoldDbValidator.SetPersistenceHandle(const Value: TBoldAbstractPersistenceHandleDB);
begin
  fPersistenceHandle := Value;
  fSystemMapper := nil;
end;

procedure TBoldDbValidator.SetThreadCount(const Value: integer);
begin
  FThreadCount := Value;
end;

procedure TBoldDbValidator.CheckTypeTableConsistency(SystemSQLMapper: TBoldSystemSQLMapper);
var
  query: IBoldQuery;
  ExecQuery: IBoldExecQuery;
  HighestBoldDbType,
  i, BoldDbType: integer;
  Name: String;
  Found: Boolean;
  MissingClasses: Boolean;
  ObjectPMapper: TBoldObjectSQLMapper;
  TypeParam, ClassParam: IBoldParameter;
  BoldDBTypeField: IBoldField;
  ClassNameField: IBoldField;
begin
  query := Database.GetQuery;
  ExecQuery := Database.GetExecQuery;
  try
    query.AssignSQLText(format('SELECT * FROM %s', [TypeTableName]));

    HighestBoldDbType := -1;

    Query.Open;
    BoldDBTypeField := Query.FieldByName('BOLD_TYPE');
    ClassNameField := Query.FieldByName('CLASSNAME');
    while not query.eof do
    begin
      BoldDbType := BoldDBTypeField.AsInteger;
      if BoldDbType > HighestBoldDbType then
        HighestBoldDbType := BoldDbType;

      Name := ClassNameField.AsString;

      Found := false;
      for i := 0 to SystemSQLMapper.ObjectPersistenceMappers.count - 1 do
      begin
        if assigned(SystemSQLMapper.ObjectPersistenceMappers[i]) and
           (CompareText(SystemSQLMapper.ObjectPersistenceMappers[i].ExpressionName, Name) = 0) then
        begin
          ObjectPMapper := SystemSQLMapper.ObjectPersistenceMappers[i] as TBoldObjectSQLMapper;
          ObjectPMapper.BoldDbType := BoldDbType;
          Found := true;
          Break;
        end;
      end;
      if not found then
        BoldLog.LogFmt(sClassInDBNotInModel, [Name, BoldDbType]);
      Query.Next;
    end;
    Query.Close;

    MissingClasses := false;
    for i := 0 to SystemSQLMapper.ObjectPersistenceMappers.count - 1 do
    begin
      ObjectPMapper := SystemSQLMapper.ObjectPersistenceMappers[i] as TBoldObjectSQLMapper;
      if assigned(ObjectPMapper) and (ObjectPMapper.BoldDbType = -1) then
      begin
        if not MissingClasses then
          BoldLog.Separator;
        BoldLog.LogFmt(sClassWithMissingID, [ObjectPMapper.ExpressionName]);
        MissingClasses := true;
      end;
    end;

    if MissingClasses and (MessageDlg(sCorrectClassWithNoID, mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
    begin
      BoldLog.Separator;
      Execquery.AssignSQLText(
        format('INSERT INTO %s (%s, %s) VALUES (:%s, :%s)', [
          TypeTablename,
          TYPECOLUMN_NAME,
          CLASSNAMECOLUMN_NAME,
          TYPECOLUMN_NAME,
          CLASSNAMECOLUMN_NAME]));

      ExecQuery.ParamCheck := true;
      TypeParam := ExecQuery.ParamByName(TYPECOLUMN_NAME);
      ClassParam := ExecQuery.ParamByName(CLASSNAMECOLUMN_NAME);
      for i := 0 to SystemSQLMapper.ObjectPersistenceMappers.count - 1 do
      begin
        ObjectPMapper := SystemSQLMapper.ObjectPersistenceMappers[i] as TBoldObjectSQLMapper;
        if assigned(ObjectPMapper) and (ObjectPMapper.BoldDbType = -1) then
        begin
          Inc(HighestBoldDbType);
          ObjectPMapper.BoldDbType := HighestBoldDbType;
          BoldLog.LogFmt(sAddBoldDBTType, [ObjectPMapper.BoldDbType, ObjectPMapper.expressionName]);
          TypeParam.AsInteger := HighestBoldDbType;
          ClassParam.AsString := ObjectPMapper.ExpressionName;
          ExecQuery.ExecSQL;
        end;
      end;
    end;
  finally
    Database.ReleaseQuery(Query);
    Database.ReleaseExecQuery(ExecQuery);
  end;
end;

procedure TBoldDbValidator.Activate;
begin
  SystemSQLMapper.OnPreInitializeBoldDbType := CheckTypeTableConsistency;
  PersistenceHandle.Active := True;
  BoldLog.ProgressMax := SystemSQLMapper.AllTables.Count;
end;

procedure TBoldDbValidator.DeActivate;
begin
  fSystemMapper := nil;
  if assigned(PersistenceHandle) then
  begin
    PersistenceHandle.Active := false;
    PersistenceHandle.ReleasePersistenceController;
  end;
end;

procedure TBoldDbValidator.Execute;
begin
  BoldLog.StartLog(sDBValidation);
  if assigned(PersistenceHandle) then
  begin
    Activate;
    Validate;
  end
  else
    BoldLog.Log(sMissingPSHandle, ltError);
end;

function TBoldDbValidator.GetDataBase: IBoldDataBase;
begin
  result := PersistenceHandle.DataBaseInterface;
end;

function TBoldDbValidator.TypeTableName: String;
begin
  result := BoldExpandPrefix(TYPETABLE_NAME, '', Database.SQLDataBaseConfig.SystemTablePrefix, Database.SQLDatabaseConfig.MaxDBIdentifierLength, SystemSQLMapper.NationalCharConversion);
end;

procedure TBoldDbValidator.Validate;
begin
  fStartTime := now;
  BoldLog.LogHeader := 'Validating...';
  BoldLog.ProgressMax := TableQueue.Count;
  for var i := 0 to ThreadCount-1 do
    ThreadList.Add(CreateValidatorThread);
end;

{ TBoldDbValidatorThread }

procedure TBoldDbValidatorThread.AddRemedy(const s: string);
begin
  fRemedyList.Add(s);
end;

constructor TBoldDbValidatorThread.Create(AValidator: TBoldDbValidator);
begin
  inherited Create(true);
  Assert(Assigned(AValidator));
  Assert(Assigned(AValidator.PersistenceHandle));
  Assert(Assigned(AValidator.PersistenceHandle.DatabaseInterface));
  fValidator := AValidator;
  fRemedyList := AValidator.fRemedyList;
  fPersistenceHandle := AValidator.PersistenceHandle;
  fSystemSQLMapper := AValidator.SystemSQLMapper;
  Suspended := False;
end;

destructor TBoldDbValidatorThread.Destroy;
begin
  inherited;
end;

function TBoldDbValidatorThread.DoCheckStop: boolean;
begin
  result := Validator.DoCheckStop;
end;

procedure TBoldDbValidatorThread.DoOnLog(const AStatus: string);
begin
  Validator.DoOnLog(AStatus);
end;

procedure TBoldDbValidatorThread.Execute;
begin
  NameThreadForDebugging(ClassName);
  CoInitialize(nil);
  try
    fBoldDatabase := Validator.PersistenceHandle.DatabaseInterface.CreateAnotherDatabaseConnection;
    fBoldDatabase.Open;
    try
      Validate;
    finally
      fBoldDatabase.Close;
      fBoldDatabase := nil;
    end;
  finally
    CoUninitialize;
    var List := Validator.ThreadList.LockList;
    var Count: integer;
    try
      Assert(List.IndexOf(self) >-1);
      List.Remove(self);
      Count := List.Count;
    finally
      Validator.ThreadList.UnlockList;
    end;
    if Count = 0 then
      Validator.DoOnComplete;
  end;
end;

end.
