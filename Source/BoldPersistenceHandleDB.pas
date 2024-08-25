
{ Global compiler directives }
{$include bold.inc}
unit BoldPersistenceHandleDB;

interface
uses
  Classes,
  BoldSubscription,
  BoldDbInterfaces,
  BoldAbstractPersistenceHandleDB,
  BoldSQLDatabaseConfig,
  BoldIndexCollection,
  BoldAbstractDataBaseAdapter;

type
  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldPersistenceHandleDB = class(TBoldAbstractPersistenceHandleDB)
  private
    fDatabaseAdapter: TBoldAbstractDatabaseAdapter;
    fComponentSubscriber: TBoldPassthroughSubscriber;
    procedure _ReceiveComponentEvents(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    procedure SetDatabaseAdapter(const Value: TBoldAbstractDatabaseAdapter);
    procedure PlaceComponentSubscriptions;
  protected
    function GetSQLDatabaseConfig: TBoldSQLDatabaseConfig; override;
    function GetCustomIndexes: TBoldIndexCollection; override;
    function GetDataBaseInterface: IBoldDatabase; override;
    procedure SetActive(Value: Boolean); override;
    procedure AssertSQLDatabaseconfig(Context: String); override;
    {$IFNDEF T2H}
    function GetNewComponentName(Comp: Tcomponent; BaseName: string): String;
    {$ENDIF}
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DatabaseAdapter: TBoldAbstractDatabaseAdapter read fDatabaseAdapter write SetDatabaseAdapter;
  end;

implementation

uses
  SysUtils,

  BoldCoreConsts,
  BoldDefs;

const
  breDatabaseAdapterDestroying = 100;
  breDatabaseAdapterChanging = 101;

{ TBoldPersistenceHandleDB }

constructor TBoldPersistenceHandleDB.Create(aOwner: Tcomponent);
begin
  inherited;
  fComponentSubscriber := TBoldPassThroughSubscriber.Create(_ReceiveComponentEvents);
end;

destructor TBoldPersistenceHandleDB.Destroy;
begin
  FreeAndNil(fComponentSubscriber);
  inherited;                       
end;

procedure TBoldPersistenceHandleDB.AssertSQLDatabaseconfig(
  Context: String);
begin
  if not assigned(DatabaseAdapter) then
    raise EBold.CreateFmt(sNoDatabaseAdapterAvailable, [classname, Context]);
end;

function TBoldPersistenceHandleDB.GetCustomIndexes: TBoldIndexCollection;
begin
  if assigned(fDatabaseAdapter) then
    result := fDatabaseAdapter.CustomIndexes
  else
    result := nil;
end;

function TBoldPersistenceHandleDB.GetDataBaseInterface: IBoldDatabase;
begin
  if assigned(fDatabaseAdapter) then
    result := fDatabaseAdapter.DataBaseInterface
  else
    result := nil;
end;

function TBoldPersistenceHandleDB.GetNewComponentName(Comp: Tcomponent;
  BaseName: string): String;
var
  i: integer;
begin
  i := 1;
  while assigned(comp.Owner.FindComponent(BaseName+IntToStr(i))) do
    inc(i);
  result := BaseName + inttostr(i);
end;

function TBoldPersistenceHandleDB.GetSQLDatabaseConfig: TBoldSQLDatabaseConfig;
begin
  if assigned(fDatabaseAdapter) then
    result := fDatabaseAdapter.SQLDatabaseConfig
  else
    result := nil;
end;

procedure TBoldPersistenceHandleDB.PlaceComponentSubscriptions;
begin
  fComponentSubscriber.CancelAllSubscriptions;
  if assigned(DatabaseAdapter) then
  begin
    DatabaseAdapter.AddSmallSubscription(fComponentSubscriber, [beDestroying], breDatabaseAdapterDestroying);
    DatabaseAdapter.AddSubscription(fComponentSubscriber, beDatabaseAdapterChanged, breDatabaseAdapterChanging);
  end;
end;

procedure TBoldPersistenceHandleDB.SetActive(Value: Boolean);
begin
  if value and not assigned(DatabaseAdapter) then
    raise EBold.CreateFmt('%s.SetActive: Can not set persistence handle to active since it is not connected to a database adapter', [classname]);
  inherited;
end;

procedure TBoldPersistenceHandleDB.SetDatabaseAdapter(
  const Value: TBoldAbstractDatabaseAdapter);
begin
  if fDatabaseAdapter <> Value then
  begin
    fDatabaseAdapter := Value;
    PlaceComponentSubscriptions;
  end;
end;

procedure TBoldPersistenceHandleDB._ReceiveComponentEvents(
  Originator: TObject; OriginalEvent: TBoldEvent;
  RequestedEvent: TBoldRequestedEvent);
begin
  case requestedEvent of
    breDatabaseAdapterDestroying:
    begin
      if active and not (csDestroying in ComponentState) then
      begin
      end;
      fDatabaseAdapter := nil;
    end;
    breDatabaseAdapterChanging:
    begin
      if active and not (csDestroying in ComponentState) then
      begin
      end;
    end;
  end;
end;

end.
