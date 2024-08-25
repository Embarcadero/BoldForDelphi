
{ Global compiler directives }
{$include bold.inc}
unit BoldIBDatabaseAction;

interface

uses
  Classes,
  IBX.IBDatabase,
  BoldActions,
  BoldSQLDatabaseConfig,
  BoldPersistenceHandle,
  BoldPersistenceHandlePAssThrough,
  BoldPersistenceHandleDB,
  BoldDatabaseAdapterIB,
  BoldAbstractDatabaseAdapter;

type
  { TBoldIBDatabaseAction }
  TBoldIBDatabaseAction = class(TBoldSystemHandleAction)
  private
    fOnSchemaGenerated: TNotifyEvent;
    fUserName: String;
    fPageSize: integer;
    fSQLDialect: integer;
    fPassword: String;
    procedure SchemaGenerated;
    function GetDatabaseAdapterIB: TBoldDatabaseAdapterIB;
    function GetPersistenceHandleDB: TBoldPersistenceHandleDB;
    function GetEffectiveDatabaseName: String;
    procedure SetPageSize(const Value: integer);
    procedure SetSQLDialect(const Value: integer);
    procedure SetPassWord(const Value: String);
    procedure SetUserName(const Value: String);
    function GetIBDatabase: TIBDatabase;
  protected
    procedure GenerateSchema;
    procedure CheckAllowEnable(var EnableAction: boolean); override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
    property IBDatabase: TIBDatabase read GetIBDatabase;
    property DatabaseAdapterIB: TBoldDatabaseAdapterIB read GetDatabaseAdapterIB;
    property PersistenceHandleDB: TBoldPersistenceHandleDB read GetPersistenceHandleDB;
    property EffectiveDatabaseName: String read GetEffectiveDatabaseName;
  published
    property OnSchemaGenerated: TNotifyEvent read fOnSchemaGenerated write fOnSchemaGenerated;
    property Username: String read fUserName write SetUserName;
    property Password: String read fPassword write SetPassWord;
    property PageSize: integer read fPageSize write SetPageSize default 4096;
    property SQLDialect: integer read fSQLDialect write SetSQLDialect default 3;
  end;

implementation
uses
  BoldDefs,
  Controls,
  Forms,
  BoldActionDefs,
  SysUtils;

{ TBoldIBDatabaseAction }

procedure TBoldIBDatabaseAction.CheckAllowEnable(var EnableAction: boolean);
begin
  EnableAction := Assigned(PersistenceHandleDB) and
                    not BoldSystemHandle.Active;
end;

constructor TBoldIBDatabaseAction.Create(AOwner: TComponent);
begin
  inherited;
  Caption := 'Create DB';
  fUserName := DEFAULTUSERNAME;
  fPassword := DEFAULTPASSWORD;
  fPageSize := 4096;
  fSQLDialect := 3;
end;

procedure TBoldIBDatabaseAction.ExecuteTarget(Target: TObject);
begin
  DatabaseAdapterIB.EnsureInterbaseDatabase(PageSize);
  GenerateSchema;
end;

procedure TBoldIBDatabaseAction.GenerateSchema;
begin
  if Assigned(PersistenceHandleDB) then
  begin
    Screen.Cursor := crHourGlass;
    try
      PersistenceHandleDB.CreateDataBaseSchema;
    finally
      Screen.Cursor := crDefault;
    end;
    SchemaGenerated;
  end;
end;

function TBoldIBDatabaseAction.GetDatabaseAdapterIB: TBoldDatabaseAdapterIB;
begin
  if assigned(PersistenceHandleDB) and
    (PersistenceHandleDB.DatabaseAdapter is TBoldDatabaseAdapterIB) then
    result := PersistenceHandleDB.DatabaseAdapter as TBoldDatabaseAdapterIB
  else
    result := nil;
end;

function TBoldIBDatabaseAction.GetEffectiveDatabaseName: String;
begin
  result := ChangeFileExt(ParamStr(0), '.gdb');
end;

function TBoldIBDatabaseAction.GetIBDatabase: TIBDatabase;
begin
  if assigned(DatabaseAdapterIB) then
    result := DatabaseAdapterIB.DataBase
  else
    result := nil;
end;

function TBoldIBDatabaseAction.GetPersistenceHandleDB: TBoldPersistenceHandleDB;
var
  PHandle: TBoldPersistenceHandle;
begin
  result := nil;
  if assigned(BoldSystemHandle) then
  begin
    PHandle := BoldSystemHandle.PersistenceHandle;
    while PHandle is TBoldPersistencehandlePassThrough do
      PHandle := TBoldPersistencehandlePassThrough(PHandle).NextPersistenceHandle;
    if PHandle is TBoldPersistenceHandleDB then
      result := TBoldPersistenceHandleDB(PHandle)
  end;
end;

procedure TBoldIBDatabaseAction.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) and assigned(PersistenceHandleDB) then
  begin
    if not assigned(DatabaseAdapterIB) then
      PersistenceHandleDB.DatabaseAdapter := TBoldDatabaseAdapterIB.create(owner);
    if not assigned(IBDatabase) then
    begin
      DatabaseAdapterIB.DataBase := TIBDatabase.Create(Owner);
      DatabaseAdapterIB.DataBase.SQLDialect := SQLDialect;
    end;

    if DatabaseAdapterIB.DatabaseEngine = dbeUnknown then
    begin
      if IBDatabase.SQLDialect = 1 then
        DatabaseAdapterIB.DatabaseEngine := dbeInterbaseSQLDialect1;
      if IBDatabase.SQLDialect = 3 then
        DatabaseAdapterIB.DatabaseEngine := dbeInterbaseSQLDialect3;
    end;
    if IBDatabase.Params.Values['user_name'] = '' then
      IBDatabase.Params.Values['user_name'] := Username;
    if IBDatabase.Params.Values['password'] = '' then
      IBDatabase.Params.Values['password'] := Password;
    if IBDatabase.Params.Values['password'] <> '' then
      IBDatabase.LoginPrompt := false;

    if IBDatabase.DatabaseName = '' then
      IBDatabase.DatabaseName := EffectiveDatabaseName;
  end;
end;

procedure TBoldIBDatabaseAction.SchemaGenerated;
begin
  if Assigned(fOnSchemaGenerated) then
    fOnSchemaGenerated(Self);
end;

procedure TBoldIBDatabaseAction.SetPageSize(const Value: integer);
  function IsPowerOf2(val: integer): Boolean;
  begin
    while (val mod 2) = 0 do
      val := val shr 1;
    result := val = 1;
  end;

begin
  if not IsPowerOf2(Value) then
    raise EBold.Create('PageSize must be a power of 2 (1024, 2048...)');
  if Value < 512 then
    raise EBold.Create('PageSize must be at least 512');
  fPageSize := Value
end;

procedure TBoldIBDatabaseAction.SetPassWord(const Value: String);
begin
  fPassword := trim(Value);
end;

procedure TBoldIBDatabaseAction.SetSQLDialect(const Value: integer);
begin
  if Value in [1, 3] then
    fSQLDialect := Value
  else
    raise EBold.Create('SQLDialect must be either 1 or 3');
end;

procedure TBoldIBDatabaseAction.SetUserName(const Value: String);
begin
  fUserName := trim(Value);
end;

end.
