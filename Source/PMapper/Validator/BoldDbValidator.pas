unit BoldDbValidator;

interface

uses
  Classes,
  BoldDbInterfaces,
  BoldAbstractPersistenceHandleDB,
  BoldPMappersSQL;

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
    function GetSystemSQLMapper: TBoldSystemSQLMApper;
    procedure SetEnabled(const Value: Boolean);
    procedure CheckTypeTableConsistency(SystemSQLMapper: TBoldSystemSQLMapper);
    function GetDataBase: IBoldDataBase;
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
    property remedy: TStringList read fRemedy;
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
  BoldPMConsts,
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
  inherited;
end;

function TBoldDbValidator.GetSystemSQLMapper: TBoldSystemSQLMApper;
begin
  if not assigned(fSystemMapper) then
    fSystemMapper := PersistenceHandle.PersistenceControllerDefault.PersistenceMapper as TBoldSystemSQLMapper;
  result := fSystemMapper;
end;

constructor TBoldDbValidator.create(owner: TComponent);
begin
  inherited;
  fRemedy := TStringList.Create;
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
begin
  query := SystemSQLMapper.GetQuery;
  ExecQuery := SystemSQLMapper.GetExecQuery;
  query.AssignSQLText(format('SELECT * FROM %s', [TypeTableName])); // do not localize

  HighestBoldDbType := -1;

  Query.Open;
  while not query.eof do
  begin
    BoldDbType := Query.FieldByName('BOLD_TYPE').AsInteger; // do not localize
    if BoldDbType > HighestBoldDbType then
      HighestBoldDbType := BoldDbType;

    Name := Query.FieldByName('CLASSNAME').AsString; // do not localize

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
      BoldLog.LogFmt(sClassWithoutDBID, [ObjectPMapper.ExpressionName]);
      MissingClasses := true;
    end;
  end;

  if MissingClasses and
    (MessageDlg(sCorrectClassesWithNoID,
          mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
  begin
    BoldLog.Separator;
    Execquery.AssignSQLText(
      format('INSERT INTO %s (%s, %s) VALUES (:%s, :%s)', [ // do not localize
        TypeTablename,
        TYPECOLUMN_NAME,
        CLASSNAMECOLUMN_NAME,
        TYPECOLUMN_NAME,
        CLASSNAMECOLUMN_NAME]));

    for i := 0 to SystemSQLMapper.ObjectPersistenceMappers.count - 1 do
    begin
      ObjectPMapper := SystemSQLMapper.ObjectPersistenceMappers[i] as TBoldObjectSQLMapper;
      if assigned(ObjectPMapper) and (ObjectPMapper.BoldDbType = -1) then
      begin
        Inc(HighestBoldDbType);
        ObjectPMapper.BoldDbType := HighestBoldDbType;
        BoldLog.LogFmt(sAddingBoldDBType, [ObjectPMapper.BoldDbType, ObjectPMapper.expressionName]);
        ExecQuery.ParamByName(TYPECOLUMN_NAME).AsInteger := HighestBoldDbType;
        ExecQuery.ParamByName(CLASSNAMECOLUMN_NAME).AsString := ObjectPMapper.ExpressionName;
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
  BoldLog.StartLog(sDatabaseValidation);
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
          BoldLog.Log(sInconsistenciesFound, ltWarning);
          for i := 0 to remedy.Count - 1 do
            BoldLog.Log(remedy[i]);
          BoldLog.Separator;
        end;
        result := Remedy.Count = 0;
      finally
        DeActivate;
      end;
    except
      on e: Exception do
      begin
        BoldLog.LogFmt(sDBValidationFailed, [e.message], ltError);
      end;
    end;
  end
  else
    BoldLog.Log(sCannotValidateWithoutPHandle, ltError);
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

end.
