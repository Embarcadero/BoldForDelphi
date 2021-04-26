
{ Global compiler directives }
{$include bold.inc}
unit BoldDbValidator;

interface

uses
  Classes,
  BoldDbInterfaces,
  BoldAbstractPersistenceHandleDB,
  BoldPMappersSQL,
  BoldSubscription;

type
  { forward declarations }
  TBoldDbValidator = class;

  { TBoldDbValidator }
  TBoldDbValidator = class(TComponent)
  private
    fSystemMapper: TBoldSystemSQLMapper;
    fRemedy: TStringList;
    fPersistenceHandle: TBoldAbstractPersistenceHandleDB;
    FEnabled: Boolean;
    fSubscriber: TBoldPassThroughSubscriber;
    function GetSystemSQLMapper: TBoldSystemSQLMApper;
    procedure SetEnabled(const Value: Boolean);
    procedure CheckTypeTableConsistency(SystemSQLMapper: TBoldSystemSQLMapper);
    function GetDataBase: IBoldDataBase;
    procedure Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
  protected
    procedure DeActivate; virtual;
    procedure SetPersistenceHandle(const Value: TBoldAbstractPersistenceHandleDB); virtual;
    function TypeTableName: String;
    property SystemSQLMapper: TBoldSystemSQLMapper read GetSystemSQLMapper;
    property DataBase: IBoldDataBase read GetDataBase;
  public
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;
    procedure Activate;
    procedure Validate; virtual; abstract;
    function Execute: Boolean;
    property Remedy: TStringList read fRemedy;
  published
    property PersistenceHandle: TBoldAbstractPersistenceHandleDB read fPersistenceHandle write SetPersistenceHandle;
    property Enabled: Boolean read FEnabled write SetEnabled;
  end;

implementation

uses
  BoldPMappers,
  BoldLogHandler,
  BoldNameExpander,
  BoldDefs,
  SysUtils,
  Dialogs,
  Controls;

{ TBoldDbValidator }

procedure TBoldDbValidator.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
end;

destructor TBoldDbValidator.destroy;
begin
  FreeAndNil(fRemedy);
  FreeAndNil(fSubscriber);
  inherited;
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

constructor TBoldDbValidator.Create(owner: TComponent);
begin
  inherited;
  fRemedy := TStringList.Create;
  fSubscriber := TBoldPassThroughSubscriber.Create(Receive);
end;

procedure TBoldDbValidator.SetPersistenceHandle(const Value: TBoldAbstractPersistenceHandleDB);
begin
  fPersistenceHandle := Value;
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
  query := SystemSQLMapper.GetQuery;
  ExecQuery := SystemSQLMapper.GetExecQuery;
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
      BoldLog.LogFmt('Database contains a class %s with BoldType %d that is not in the model...', [Name, BoldDbType]);
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
      BoldLog.LogFmt('Model contains a class %s that does not have a database id', [ObjectPMapper.ExpressionName]);
      MissingClasses := true;
    end;
  end;

  if MissingClasses and
    (MessageDlg('There are classes with no database ID. Do you want to correct this now?',
          mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
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
        BoldLog.LogFmt('Adding BoldDbType %d for %s', [ObjectPMapper.BoldDbType, ObjectPMapper.expressionName]);
        TypeParam.AsInteger := HighestBoldDbType;
        ClassParam.AsString := ObjectPMapper.ExpressionName;
        ExecQuery.ExecSQL;
      end;
    end;
  end;

  SystemSQLMapper.ReleaseQuery(Query);
  SystemSQLMapper.ReleaseExecQuery(ExecQuery);
end;

procedure TBoldDbValidator.Activate;
begin
  SystemSQLMapper.OnPreInitializeBoldDbType := CheckTypeTableConsistency;
  PersistenceHandle.Active := True;
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

function TBoldDbValidator.Execute: Boolean;
var
  i: integer;
begin
  BoldLog.StartLog('Database validation');
  result := false;
  if assigned(PersistenceHandle) then
  begin
    try
      try
        Activate;
        Validate;
        if remedy.Count <> 0 then
        begin
          BoldLog.Separator;
          BoldLog.Log('Inconsistencies found', ltWarning);
          for i := 0 to remedy.Count - 1 do
            BoldLog.Log(remedy[i], ltDetail);
          BoldLog.Separator;
        end;
        result := Remedy.Count = 0;
      finally
        BoldLog.Log('Database validation finished', ltInfo);
        DeActivate;
      end;
    except
      on e: Exception do
      begin
        BoldLog.LogFmt('Database validation failed: %s', [e.message], ltError);
      end;
    end;
  end
  else
    BoldLog.Log('Unable to perform validation, missing a PersistenceHandle', ltError);
  BoldLog.EndLog;
end;

function TBoldDbValidator.GetDataBase: IBoldDataBase;
begin
  result := PersistenceHandle.DataBaseInterface;
end;

function TBoldDbValidator.TypeTableName: String;
begin
  result := BoldExpandPrefix(TYPETABLE_NAME, '', PersistenceHandle.SQLDataBaseConfig.SystemTablePrefix, SystemSQLMapper.SQLDatabaseConfig.MaxDBIdentifierLength, SystemSQLMapper.NationalCharConversion);
end;

initialization
end.
