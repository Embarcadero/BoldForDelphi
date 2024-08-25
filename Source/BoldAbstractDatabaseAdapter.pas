
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
    class var fMasterAdapter: TBoldAbstractDatabaseAdapter;
  private
    FSQLDatabaseConfig: TBoldSQLDatabaseConfig;
    fCustomIndexes: TBoldIndexCollection;
    fDatabaseEngine: TBoldDataBaseEngine;
    fInternalDatabase: TComponent;
    fOnConnect: TNotifyEvent;
    fOnReconnect: TNotifyEvent;
    fOnDisconnect: TNotifyEvent;
    FIsMasterAdapter: Boolean;
    procedure SetSQLDatabaseConfig(const Value: TBoldSQLDatabaseConfig);
    procedure SetCustomIndexes(const Value: TBoldIndexCollection);
    procedure SetInternalDatabase(const Value: TComponent);
    function GetMasterAdapter: TBoldAbstractDatabaseAdapter;
    procedure SetIsMasterAdapter(const Value: Boolean);
  protected
    procedure ReleaseBoldDatabase; virtual; abstract;
    procedure Changed;
    procedure AssignTo(Dest: TPersistent); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetDataBaseEngine(const Value: TBoldDataBaseEngine); virtual;
    property InternalDatabase: TComponent read fInternalDatabase write SetInternalDatabase;
    function GetDataBaseInterface: IBoldDatabase; virtual; abstract;
    procedure DoOnConnect(Sender: TObject);
    procedure DoOnReconnect(Sender: TObject);
    procedure DoOnDisconnect(Sender: TObject);
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateDatabase(DropExisting: boolean = true); virtual;
    procedure DropDatabase; virtual;
    function DatabaseExists: boolean; virtual;
    property DatabaseInterface: IBoldDatabase read GetDatabaseInterface;
    property DatabaseEngine: TBoldDataBaseEngine read fDatabaseEngine write SetDataBaseEngine;
    property MasterAdapter: TBoldAbstractDatabaseAdapter read GetMasterAdapter;
  published
    property SQLDatabaseConfig: TBoldSQLDatabaseConfig read FSQLDatabaseConfig write SetSQLDatabaseConfig;
    property CustomIndexes: TBoldIndexCollection read fCustomIndexes write SetCustomIndexes;
    property OnConnect: TNotifyEvent read fOnConnect write fOnConnect;
    property OnReconnect: TNotifyEvent read fOnReconnect write fOnReconnect;
    property OnDisconnect: TNotifyEvent read fOnDisconnect write fOnDisconnect;
    property IsMasterAdapter: Boolean read FIsMasterAdapter write SetIsMasterAdapter default false;
    // masterAdapter has to be created first for subsequent adapters to use it's settings
    // setting masterAdapter has no effect on already created adapters
  end;

implementation

uses
  SysUtils;

{ TBoldAbstractDatabaseAdapter }

procedure TBoldAbstractDatabaseAdapter.AssignTo(Dest: TPersistent);
begin
  if Dest is TBoldAbstractDatabaseAdapter then
  begin
    TBoldAbstractDatabaseAdapter(Dest).DatabaseEngine := fDatabaseEngine;  
    TBoldAbstractDatabaseAdapter(Dest).SQLDatabaseConfig.Assign(SQLDatabaseConfig);
    TBoldAbstractDatabaseAdapter(Dest).CustomIndexes.Assign(CustomIndexes);
  end
  else
    inherited;
end;

procedure TBoldAbstractDatabaseAdapter.Changed;
begin
  SendEvent(self, beDatabaseAdapterChanged);
end;

constructor TBoldAbstractDatabaseAdapter.Create(aOwner: TComponent);
begin
  inherited;
  fSQLDatabaseConfig := TBoldSQLDatabaseConfig.Create;
  fCustomIndexes := TBoldIndexCollection.Create(Self);
  
  if not (csDesigning in ComponentState) and Assigned(MasterAdapter) then
    Assign(MasterAdapter);
end;

procedure TBoldAbstractDatabaseAdapter.CreateDatabase(DropExisting: boolean);
begin
// override in subclass
end;

function TBoldAbstractDatabaseAdapter.DatabaseExists: boolean;
begin
  result := false;
// override in subclass
end;

procedure TBoldAbstractDatabaseAdapter.DropDatabase;
begin
// override in subclass
end;

function TBoldAbstractDatabaseAdapter.GetMasterAdapter: TBoldAbstractDatabaseAdapter;
begin
  result := fMasterAdapter;
end;

destructor TBoldAbstractDatabaseAdapter.Destroy;
begin
  FreeAndNil(fSQLDatabaseConfig);
  FreeAndNil(fCustomIndexes);
  if IsMasterAdapter then
   fMasterAdapter := nil;
  inherited;
end;

procedure TBoldAbstractDatabaseAdapter.DoOnConnect(Sender: TObject);
begin
  if Assigned(fOnConnect) then
    fOnConnect(Sender);
end;

procedure TBoldAbstractDatabaseAdapter.DoOnDisconnect(Sender: TObject);
begin
  if Assigned(fOnDisconnect) then
    fOnDisconnect(Sender);
end;

procedure TBoldAbstractDatabaseAdapter.DoOnReconnect(Sender: TObject);
begin
  if Assigned(fOnReconnect) then
    fOnReconnect(Sender);
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

procedure TBoldAbstractDatabaseAdapter.SetIsMasterAdapter(const Value: Boolean);
begin
  if FIsMasterAdapter <> Value then
  begin
    FIsMasterAdapter := Value;
    if Value then
      fMasterAdapter := self;
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
  
end.