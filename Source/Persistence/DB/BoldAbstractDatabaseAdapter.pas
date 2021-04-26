
{ Global compiler directives }
{$include bold.inc}
unit BoldAbstractDatabaseAdapter;

interface

uses
  Classes,
  BoldDBInterfaces,
  BoldSQLDatabaseConfig,
  BoldSubscription,
  BoldIndexCollection;

const
  beDatabaseAdapterChanged = 100;

type
  { forward declarations }
  TBoldAbstractDatabaseAdapter = class;

  { TBoldAbstractDatabaseAdapter }
  TBoldAbstractDatabaseAdapter = class(TBoldSubscribableComponent)
  private
    FSQLDatabaseConfig: TBoldSQLDatabaseConfig;
    fCustomIndexes: TBoldIndexCollection;
    fDatabaseEngine: TBoldDataBaseEngine;
    fInternalDatabase: TComponent;
    procedure SetSQLDatabaseConfig(const Value: TBoldSQLDatabaseConfig);
    procedure SetCustomIndexes(const Value: TBoldIndexCollection);
    procedure SetInternalDatabase(const Value: TComponent);
  protected
    procedure ReleaseBoldDatabase; virtual; abstract;
    procedure Changed;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetDataBaseEngine(const Value: TBoldDataBaseEngine); virtual;
    property DatabaseEngine: TBoldDataBaseEngine read fDatabaseEngine write SetDataBaseEngine;
    property InternalDatabase: TComponent read fInternalDatabase write SetInternalDatabase;
    function GetDataBaseInterface: IBoldDatabase; virtual; abstract;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    procedure CreateDatabase; virtual; abstract;
    property DatabaseInterface: IBoldDatabase read GetDatabaseInterface;
  published
    property SQLDatabaseConfig: TBoldSQLDatabaseConfig read FSQLDatabaseConfig write SetSQLDatabaseConfig;
    property CustomIndexes: TBoldIndexCollection read fCustomIndexes write SetCustomIndexes;
  end;

implementation

uses
  SysUtils,
  BoldRev;

{ TBoldAbstractDatabaseAdapter }

procedure TBoldAbstractDatabaseAdapter.Changed;
begin
  SendEvent(self, beDatabaseAdapterChanged);
end;

constructor TBoldAbstractDatabaseAdapter.create(aOwner: TComponent);
begin
  inherited;
  fSQLDatabaseConfig := TBoldSQLDatabaseConfig.Create;
  fCustomIndexes := TBoldIndexCollection.Create(Self);
end;

destructor TBoldAbstractDatabaseAdapter.destroy;
begin
  FreeAndNil(fSQLDatabaseConfig);
  FreeAndNil(fCustomIndexes);
  inherited;
end;

procedure TBoldAbstractDatabaseAdapter.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (aComponent = fInternalDataBase) and (Operation = opRemove) then
  begin
    Changed;
    ReleaseBoldDatabase;
    fInternalDataBase := nil;
  end;
end;

procedure TBoldAbstractDatabaseAdapter.SetInternalDatabase(const Value: TComponent);
begin
  if fInternalDataBase <> Value then
  begin
    Changed;
    ReleaseBoldDatabase;
    fInternalDataBase := Value;
    if assigned(fInternalDataBase) then
      fInternalDataBase.FreeNotification(self);
  end;
end;

procedure TBoldAbstractDatabaseAdapter.SetCustomIndexes(
  const Value: TBoldIndexCollection);
begin
  fCustomIndexes.Assign(value);
end;

procedure TBoldAbstractDatabaseAdapter.SetDataBaseEngine(const Value: TBoldDataBaseEngine);
begin
  if value <> fDatabaseEngine then
  begin
    if not (csLoading in ComponentState) then
      SQLDatabaseConfig.InitializeDbEngineSettings(Value);
    fDatabaseEngine := Value;
    SQLDataBaseConfig.Engine := DatabaseEngine;
  end;
end;

procedure TBoldAbstractDatabaseAdapter.SetSQLDatabaseConfig(const Value: TBoldSQLDatabaseConfig);
begin
  FSQLDatabaseConfig.AssignConfig(value);
end;

initialization
  
end.
