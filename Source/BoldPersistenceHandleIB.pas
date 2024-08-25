
{ Global compiler directives }
{$include bold.inc}
unit BoldPersistenceHandleIB;

interface

uses
  Classes,
  IBX.IBDatabase,
  BoldDBInterfaces,
  BoldIBInterfaces,
  BoldSQLDatabaseConfig,
  BoldPersistenceHandleDB,
  BoldPersistenceHandleDB_deprecated;

type
  { forward declarations }
  TBoldPersistenceHandleIB = class;

  { TBoldPersistenceHandleIB }
  TBoldPersistenceHandleIB = class(TBoldDBPersistenceHandle)
  private
    fDataBaseName: String;
    fIBDataBase: TIBDataBase;
    fOwnDataBase: TIBDataBase;
    fDataBaseAdapter: TBoldIBDataBase;
    function getEffectiveDataBase: TIBDataBase;
    procedure SetDatabaseName(const Value: String);
    procedure SetIBDatabase(const Value: TIBDataBase);
    procedure ReadDatabase(Reader: TReader);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetPassword(const Value: string); override;
    procedure SetUserName(const Value: string); override;
    property EffectiveDataBase: TIBDataBase read getEffectiveDataBase;
    procedure SetDataBaseEngine(const Value: TBoldDataBaseEngine); override;
    {$IFNDEF T2H}
    procedure InternalTransferproperties(const target: TBoldPersistenceHandleDB); override;
    {$ENDIF}
  public
    constructor create(owner: TComponent); override;
    destructor destroy; override;
    function GetDataBaseInterface: IBoldDatabase; override;
  published
    property DatabaseName: String  read fDataBaseName write SetDatabaseName;
    property IBDataBase: TIBDataBase read FIBDataBase write SetIBDataBase;
  end deprecated;

implementation

uses
  SysUtils,
  dialogs,
  BoldDefs,
  BoldDatabaseAdapterIB;

{ TBoldPersistenceHandleIB }

constructor TBoldPersistenceHandleIB.create(owner: TComponent);
begin
  inherited;
  UserName := 'SYSDBA';
  Password := 'masterkey';
end;

destructor TBoldPersistenceHandleIB.destroy;
begin
  Active := false;
  FreeAndNil(fOwnDataBase);
  FreeAndNil(fDataBaseAdapter);
  inherited;
end;

function TBoldPersistenceHandleIB.GetDataBaseInterface: IBoldDatabase;
begin
  if not assigned(fDataBaseAdapter) then
    fDataBaseAdapter := TBoldIBDataBase.create(EffectiveDataBase, SQLDataBaseconfig);
  result := fDataBaseAdapter;
end;

function TBoldPersistenceHandleIB.getEffectiveDataBase: TIBDataBase;
begin
  if assigned(fIBDataBase) then
    result := fIBDataBase
  else
  begin
    if not assigned(fOwnDataBase) then
    begin
      fOwnDataBase := TIBDataBase.Create(nil);
      fOwnDataBase.DatabaseName := DatabaseName;
      fOwnDataBase.Params.Values['USER_NAME'] := Username;
      fOwnDataBase.Params.Values['PASSWORD'] := Password;
      if PassWord <> '' then
        fOwnDataBase.LoginPrompt := false;
    end;
    result := fOwnDataBase;
  end;
end;

procedure TBoldPersistenceHandleIB.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (aComponent = fIBDataBase) and (Operation = opRemove) then
  begin
    if aComponent = EffectiveDataBase then
    begin
      Active := false;
      FreeAndNil(fDataBaseAdapter);
    end;
    fIBDataBase := nil;
  end;
end;

procedure TBoldPersistenceHandleIB.SetIBDatabase(const Value: TIBDataBase);
begin
  if fIBDataBase <> Value then
  begin
    CheckInactive('SetDataBase');
    if assigned(fOwnDataBase) then
    begin
      FreeAndNil(FOwnDataBase);
      FreeAndNil(fdataBaseAdapter);
    end;

    fIBDataBase := Value;

    if assigned(fIBDataBase) then
      fIBDataBase.FreeNotification(self);
  end;
end;

procedure TBoldPersistenceHandleIB.SetDatabaseName(const Value: String);
begin
  fDataBaseName := Value;
  if assigned(fOwnDataBase) then
    fOwnDataBase.DataBaseName := Value;
end;

procedure TBoldPersistenceHandleIB.SetPassword(const Value: string);
begin
  inherited;
  if assigned(fOwnDataBase) then
    fOwnDataBase.Params.Values['PASSWORD'] := Password;
end;

procedure TBoldPersistenceHandleIB.SetUserName(const Value: string);
begin
  inherited;
  if assigned(fOwnDataBase) then
    fOwnDataBase.Params.Values['USER_NAME'] := UserName;
end;

procedure TBoldPersistenceHandleIB.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Database', ReadDatabase, nil, True);
end;

procedure TBoldPersistenceHandleIB.ReadDatabase(Reader: TReader);
begin
  DatabaseName := Reader.ReadString;  
end;

procedure TBoldPersistenceHandleIB.SetDataBaseEngine(const Value: TBoldDataBaseEngine);
begin
  if value in [dbeUnknown, dbeInterbaseSQLDialect1, dbeInterbaseSQLDialect3] then
    inherited SetDatabaseEngine(value)
  else
    raise EBold.CreateFmt('%s.SetdatabaseEngine: Unsupported value. Must be interbase', [classname]);
end;

procedure TBoldPersistenceHandleIB.InternalTransferproperties(
  const target: TBoldPersistenceHandleDB);
var
  Adapter: tBoldDatabaseAdapterIB;
  DesInfo: longint;
begin
  inherited;
  DesInfo := Target.DesignInfo;
  if not assigned(Target.DatabaseAdapter) then
  begin
    Target.DatabaseAdapter := tBoldDatabaseAdapterIB.Create(Target.Owner);
    Target.DatabaseAdapter.Name := GetNewComponentName(Target.DatabaseAdapter, 'BoldDatabaseAdapterIB');
    LongRec(DesInfo).Lo := LongRec(DesInfo).lo+16;
    LongRec(DesInfo).Hi := LongRec(DesInfo).hi+16;
    Target.DatabaseAdapter.DesignInfo          := DesInfo;
    showmessage('Created a new DatabaseAdapterIB');
  end
  else if not (target.DatabaseAdapter is tBoldDatabaseAdapterIB) then
    raise Exception.CreateFmt('The persistencehandle is connected to a %s, properties can only be transfered to a TBoldDatabaseAdapterIB', [target.DatabaseAdapter.ClassName] );

  Adapter := target.DatabaseAdapter as tBoldDatabaseAdapterIB;
  if assigned(fIBDatabase) then
    Adapter.DataBase := IBDataBase;

  if not assigned(Adapter.Database) then
  begin
    Adapter.DataBase := TIBDatabase.Create(Target.owner);
    Adapter.DataBase.Name := GetNewComponentName(Adapter.DataBase, 'IBDatabase');
    showmessage('Created a new IBDatabase');
    LongRec(DesInfo).Lo := LongRec(DesInfo).lo+16;
    LongRec(DesInfo).Hi := LongRec(DesInfo).hi+16;
    Adapter.DataBase.DesignInfo          := DesInfo;
  end;
  Adapter.Database.Params.Values['PASSWORD'] := Password;
  Adapter.Database.Params.Values['USER_NAME'] := Username;
  if Adapter.Database.Params.Values['PASSWORD'] <> '' then
    Adapter.Database.LoginPrompt := false;

  if not assigned(IBDatabase) then
    Adapter.DataBase.DatabaseName := DatabaseName;
end;

end.
