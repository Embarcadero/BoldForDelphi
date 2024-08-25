
{ Global compiler directives }
{$include bold.inc}
unit BoldPersistenceHandleSQLDirect;

interface

uses
  Classes,
  SDEngine,
  BoldDBInterfaces,
  BoldDatabaseAdapterSQLDirect,
  BoldPersistenceHandleDB,
  BoldPersistenceHandleDB_deprecated,
  BoldSQLDirectInterfaces;

type
  { forward declarations }
  TBoldPersistenceHandleSQLDirect = class;

  { TBoldPersistenceHandleSQLDirect }
  TBoldPersistenceHandleSQLDirect = class(TBoldDBPersistenceHandle)
  private
    FDatabaseName: string;
    FSDDatabase: TSDDataBase;
    FDatabaseAdapter: TBoldSQLDirectDatabase;
    FEffectiveDataBase: TSDDataBase;
    function GetEffectiveDatabase: TSDDataBase;
    procedure SetDatabaseAdapter(const Value: TBoldSQLDirectDatabase);
    procedure SetDatabase(const Value: TSDDatabase);
    procedure SetDatabaseName(const Value: string);
    procedure SetEffectiveDatabase(const Value: TSDDatabase);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property DatabaseAdapter: TBoldSQLDirectDatabase read FDatabaseAdapter write SetDatabaseAdapter;
    property EffectiveDatabase: TSDDataBase read GetEffectiveDatabase write SetEffectiveDatabase;
    {$IFNDEF T2H}
    procedure InternalTransferproperties(const target: TBoldPersistenceHandleDB); override;
    {$ENDIF}
  public
    destructor Destroy; override;
    function GetDataBaseInterface: IBoldDatabase; override;
  published
    property DatabaseName: string read FDatabaseName write SetDatabaseName;
    property SDDatabase: TSDDataBase read FSDDatabase write SetDatabase;
  end deprecated;

implementation

uses
  Dialogs,
  SysUtils;

{ TBoldPersistenceHandleSQLDirect }

destructor TBoldPersistenceHandleSQLDirect.destroy;
begin
  Active := false;
  FreeAndNil(FEffectiveDatabase);
  FreeAndNil(FDatabaseAdapter);
  inherited;
end;

function TBoldPersistenceHandleSQLDirect.GetDataBaseInterface: IBoldDatabase;
begin
  if not assigned(FDatabaseAdapter) then
    FDatabaseAdapter := TBoldSQLDirectDatabase.create(EffectiveDatabase, SQLDataBaseConfig);
  result := FDataBaseAdapter;
end;

function TBoldPersistenceHandleSQLDirect.GetEffectiveDatabase: TSDDataBase;
begin
  if assigned(FSDDatabase) then
    result := FSDDatabase
  else
    raise Exception.CreateFmt('%s.GetEffectiveDatabase: SDDatabase property must be assigned', [ClassName]);
end;

procedure TBoldPersistenceHandleSQLDirect.InternalTransferproperties(
  const target: TBoldPersistenceHandleDB);
var
  Adapter: TBoldDatabaseAdapterSQLDirect;
  DesInfo: longint;
begin
  inherited;
  DesInfo := Target.DesignInfo;
  if not assigned(Target.DatabaseAdapter) then
  begin
    Target.DatabaseAdapter := TBoldDatabaseAdapterSQLDirect.Create(Target.Owner);
    Target.DatabaseAdapter.Name := GetNewComponentName(Target.DatabaseAdapter, 'BoldDatabaseAdapterSQLDirect');
    LongRec(DesInfo).Lo := LongRec(DesInfo).lo+16;
    LongRec(DesInfo).Hi := LongRec(DesInfo).hi+16;
    Target.DatabaseAdapter.DesignInfo          := DesInfo;
    showmessage('Created a new DatabaseAdapterSQLDirect');
  end
  else if not (target.DatabaseAdapter is tBoldDatabaseAdapterSQLDirect) then
    raise Exception.CreateFmt('The persistencehandle is connected to a %s, properties can only be transfered to a TBoldDatabaseAdapterSQLDirect', [target.DatabaseAdapter.ClassName] );

  Adapter := target.DatabaseAdapter as tBoldDatabaseAdapterSQLDirect;
  if assigned(SDDatabase) then
    Adapter.DataBase := SDDatabase;

  if not assigned(Adapter.Database) then
  begin
    Adapter.DataBase := TSDDatabase.Create(Target.owner);
    Adapter.DataBase.Name := GetNewComponentName(Adapter.DataBase, 'SDDatabase');
    showmessage('Created a new SDDatabase');
    LongRec(DesInfo).Lo := LongRec(DesInfo).lo+16;
    LongRec(DesInfo).Hi := LongRec(DesInfo).hi+16;
    Adapter.DataBase.DesignInfo          := DesInfo;
  end;
end;

procedure TBoldPersistenceHandleSQLDirect.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (aComponent = FSDDatabase) and (Operation = opRemove) then
  begin
    if aComponent = EffectiveDatabase then
    begin
      Active := false;
      FDatabaseAdapter := nil;
    end;
    FSDDatabase := nil;
  end;
end;

procedure TBoldPersistenceHandleSQLDirect.SetDataBase(const Value: TSDDataBase);
begin
  if FSDDatabase <> Value then
  begin
    if assigned(FEffectiveDatabase) then
    begin
      FreeAndNil(FEffectiveDatabase);
      FreeAndNil(FDatabaseAdapter);
    end;

    FSDDatabase := Value;

    if assigned(FSDDatabase) then
      FSDDatabase.FreeNotification(self);
  end;
end;

procedure TBoldPersistenceHandleSQLDirect.SetDatabaseAdapter(const Value: TBoldSQLDirectDatabase);
begin
  FDatabaseAdapter := Value;
end;

procedure TBoldPersistenceHandleSQLDirect.SetDatabaseName(const Value: string);
begin
  FDatabaseName := Value;
end;

procedure TBoldPersistenceHandleSQLDirect.SetEffectiveDataBase(const Value: TSDDataBase);
begin
  FEffectiveDatabase := Value;
end;

end.
