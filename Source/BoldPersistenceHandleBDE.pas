
{ Global compiler directives }
{$include bold.inc}
unit BoldPersistenceHandleBDE;

interface

uses
  Classes,
  db,
  DbTables,
  Dialogs,
  BoldDBInterfaces,
  BoldBDEInterfaces,
  BoldPersistenceHandleDB,
  BoldPersistenceHandleDB_deprecated;

type
  { forward declarations }
  TBoldPersistenceHandleBDE = class;

  { TBoldPersistenceHandleBDE }
  TBoldPersistenceHandleBDE = class(TBoldDBPersistenceHandle)
  private
    fDataBaseName: String;
    FSessionName: String;
    fDataBase: TDataBase;
    fOwnDataBase: TDataBase;
    fExistingDataBase: TDataBase;
    fDataBaseAdapter: TBoldBDEDataBase;
    procedure SetSessionName(const Value: String);
    procedure SetDatabaseName(const Value: string);
    procedure SetDataBase(const Value: TDataBase);
    function getEffectiveDataBase: TDataBase;
    function GetDataBasename: string;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property EffectiveDataBase: TDataBase read getEffectiveDataBase;
    procedure SetPassword(const Value: string); override;
    procedure SetUserName(const Value: string); override;
    {$IFNDEF T2H}
    procedure InternalTransferproperties(const target: TBoldPersistenceHandleDB); override;
    {$ENDIF}
  public
    destructor destroy; override;
    function GetDataBaseInterface: IBoldDatabase; override;
  published
    property DatabaseName: string read GetDataBasename write SetDataBaseName;
    property SessionName: String read FSessionName write SetSessionName;
    property DataBase: TDataBase read fDataBase write SetDataBase;
  end deprecated;

implementation

uses
  SysUtils,
  BoldDatabaseAdapterBDE;

{ TBoldPersistenceHandleBDE }

destructor TBoldPersistenceHandleBDE.destroy;
begin
  Active := false;
  FreeAndNil(fOwnDataBase);
  FreeAndNil(fDataBaseAdapter);
  inherited;
end;

function TBoldPersistenceHandleBDE.GetDataBaseInterface: IBoldDatabase;
begin
  if not assigned(fDataBaseAdapter) then
    fDataBaseAdapter := TBoldBDEDataBase.create(EffectiveDataBase, SQLDataBaseConfig);
  result := fDataBaseAdapter;
end;

function TBoldPersistenceHandleBDE.GetDataBasename: string;
begin
  result := fDataBaseName;
end;

function TBoldPersistenceHandleBDE.getEffectiveDataBase: TDataBase;
var
  Session: TSession;
begin
  if assigned(fDataBase) then
  begin
    result := fDataBase;
    exit;
  end;

  if assigned(fExistingDataBase) then
  begin
    result := fExistingDataBase;
    exit;
  end;

  if assigned(fOwnDatabase) then
  begin
    result := fOwnDataBase;
    exit;
  end;

  Session := Sessions.FindSession(SessionName);
  if assigned(Session) then
  begin
    fExistingDataBase := Session.FindDatabase(DataBaseName);
    if assigned(fExistingDataBase) then
    begin
      if not fExistingDatabase.Connected then
      begin
        if UserName <> '' then
          fExistingDataBase.Params.Values['USER NAME'] := Username;
        if Password <> '' then
        begin
          fExistingDataBase.Params.Values['PASSWORD'] := Password;
          fExistingDataBase.LoginPrompt := false;
        end;
      end;
      fExistingDataBase.FreeNotification(self);
      result := fExistingDataBase;
      exit;
    end;
  end;

  fOwnDataBase := TDataBase.Create(nil);
  fOwnDataBase.name := name+'_DataBase';
  fOwnDataBase.DatabaseName := DatabaseName;
  fOwnDataBase.SessionName := SessionName;
  fOwnDataBase.Params.Values['USER NAME'] := Username;
  fOwnDataBase.Params.Values['PASSWORD'] := Password;
  if PassWord <> '' then
    fOwnDataBase.LoginPrompt := false;
  result := fOwnDataBase;
end;

procedure TBoldPersistenceHandleBDE.InternalTransferproperties(
  const target: TBoldPersistenceHandleDB);
var
  Adapter: TBoldDatabaseAdapterBDE;
  DesInfo: longint;
begin
  inherited;
  DesInfo := Target.DesignInfo;
  if not assigned(Target.DatabaseAdapter) then
  begin
    Target.DatabaseAdapter := TBoldDatabaseAdapterBDE.Create(Target.Owner);
    Target.DatabaseAdapter.Name := GetNewComponentName(Target.DatabaseAdapter, 'BoldDatabaseAdapterBDE');
    LongRec(DesInfo).Lo := LongRec(DesInfo).lo+16;
    LongRec(DesInfo).Hi := LongRec(DesInfo).hi+16;
    Target.DatabaseAdapter.DesignInfo          := DesInfo;
    showmessage('Created a new DatabaseAdapterBDE');
  end
  else if not (target.DatabaseAdapter is tBoldDatabaseAdapterBDE) then
    raise Exception.CreateFmt('The persistencehandle is connected to a %s, properties can only be transfered to a TBoldDatabaseAdapterBDE', [target.DatabaseAdapter.ClassName] );

  Adapter := target.DatabaseAdapter as tBoldDatabaseAdapterBDE;
  if assigned(fDatabase) then
    Adapter.DataBase := DataBase;

  if not assigned(Adapter.Database) then
  begin
    Adapter.DataBase := TDatabase.Create(Target.owner);
    Adapter.DataBase.Name := GetNewComponentName(Adapter.DataBase, 'Database');
    showmessage('Created a new Database');
    LongRec(DesInfo).Lo := LongRec(DesInfo).lo+16;
    LongRec(DesInfo).Hi := LongRec(DesInfo).hi+16;
    Adapter.DataBase.DesignInfo          := DesInfo;
  end;
  Adapter.Database.Params.Values['PASSWORD'] := Password;
  Adapter.Database.Params.Values['USER NAME'] := Username;
  if Adapter.Database.Params.Values['PASSWORD'] <> '' then
    Adapter.Database.LoginPrompt := false;
  if not assigned(Database) then
    Adapter.DataBase.DatabaseName := DatabaseName;
  if SessionName <> '' then
    Adapter.DataBase.SessionName := SessionName;
end;

procedure TBoldPersistenceHandleBDE.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

  if (aComponent = fDataBase) and (Operation = opRemove) then
  begin
    Active := false;
    FreeAndNil(fDatabaseAdapter);
    DataBase := nil;
  end;

  if (aComponent = fExistingDatabase) and (Operation = opRemove) then
  begin
    Active := false;
    FreeAndNil(fDatabaseAdapter);
    fExistingDataBase := nil;
  end;
end;

procedure TBoldPersistenceHandleBDE.SetDataBase(const Value: TDataBase);
begin
  if fDataBase <> Value then
  begin
    CheckInactive('SetDataBase');
    if assigned(fOwnDataBase) then
    begin
      FreeAndNil(FOwnDataBase);
      FreeAndNil(fdataBaseAdapter);
    end;
    if assigned(fExistingDataBase) then
    begin
      fExistingDataBase := nil;
      FreeAndNil(fdataBaseAdapter);
    end;
    fDataBase := Value;
    if assigned(fDataBase) then
      fDataBase.FreeNotification(self);
  end;
end;

procedure TBoldPersistenceHandleBDE.SetDatabaseName(const Value: string);
begin
  if FDatabaseName <> Value then
  begin
    CheckInactive('SetDataBaseName');
    if assigned(fOwnDataBase) then
      fOwnDataBase.DatabaseName := DatabaseName;
    FDatabaseName := Value;
    fExistingDataBase := nil;
  end;
end;

procedure TBoldPersistenceHandleBDE.SetPassword(const Value: string);
begin
  if Value <> PassWord then
  begin
    if assigned(fOwnDataBase) then
      fOwnDataBase.Params.Values['PASSWORD'] := value;
    if assigned(fExistingDataBase) then
      fExistingDataBase.Params.Values['PASSWORD'] := Value;
  end;
  inherited;    
end;

procedure TBoldPersistenceHandleBDE.SetSessionName(const Value: String);
begin
  if FSessionName <> Value then
  begin
    CheckInactive('SetSessionName');
    FSessionName := Value;
    if assigned(fOwnDataBase) then
      fOwnDataBase.SessionName := SessionName;
    fExistingDataBase := nil;
  end;
end;

procedure TBoldPersistenceHandleBDE.SetUserName(const Value: string);
begin
  if value <> Username then
  begin
    if assigned(fOwnDataBase) then
      fOwnDataBase.Params.Values['USER NAME'] := UserName;
    if assigned(fExistingDataBase) then
      fExistingDataBase.Params.Values['USER NAME'] := UserName;
  end;
  inherited;
end;

end.
