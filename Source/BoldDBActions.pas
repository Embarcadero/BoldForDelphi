{ Global compiler directives }
{$include bold.inc}
unit BoldDBActions;

interface

uses
  ActnList,
  Classes,

  BoldCoreConsts,
  BoldHandleAction,
  BoldPersistenceHandleDB,
  BoldSubscription,
  BoldDbValidator,
  BoldDbStructureValidator,
  BoldDbDataValidator;

const
  cDefaultPauseBetweenQueries = 1; //ms

type
  TBoldPersistenceHandleAction = class;
  TBoldGenerateSchemaAction = class;
  TBoldValidateDBStructureAction = class;
  TBoldValidateDBDataAction = class;
  TBoldEvolveDBAction = class;

  { TBoldSystemHandleAction }
  TBoldPersistenceHandleAction = class(TAction)
  private
    fHandleSubscriber: TBoldPassThroughSubscriber;
    fBoldPersistenceHandleDB: TBoldPersistenceHandleDB;
    procedure SetBoldPersistenceHandle(const Value: TBoldPersistenceHandleDB); virtual;
  protected
    procedure _HandleSubscriberReceive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent); virtual;
    procedure CheckAllowEnable(var EnableAction: boolean); virtual;
    property BoldPersistenceHandleDB: TBoldPersistenceHandleDB read fBoldPersistenceHandleDB write SetBoldPersistenceHandle;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateTarget(Target: TObject); override;
    function HandlesTarget(Target: TObject): Boolean; override;
  end;

  { TBoldGenerateSchemaAction }
  TBoldGenerateSchemaAction = class(TBoldPersistenceHandleAction)
  private
    fIgnoreUnknownTables: boolean;
  protected
    procedure CheckAllowEnable(var EnableAction: boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
  published
    property IgnoreUnknownTables: boolean read fIgnoreUnknownTables write fIgnoreUnknownTables;
    property BoldPersistenceHandleDB;
  end;

  { TBoldValidateDBStructureAction }
  TBoldValidateDBStructureAction = class(TBoldPersistenceHandleAction)
  private
    fValidator: TBoldDbStructureValidator;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ExecuteTarget(Target: TObject); override;
  published
    property BoldPersistenceHandleDB;
  end;

  { TBoldValidateDBDataAction }
  TBoldValidateDBDataAction = class(TBoldPersistenceHandleAction)
  private
    fValidator: TBoldDbDataValidator;
    fPauseBetweenQueries: integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ExecuteTarget(Target: TObject); override;
  published
    property PauseBetweenQueries: integer read fPauseBetweenQueries write fPauseBetweenQueries default cDefaultPauseBetweenQueries;
    property BoldPersistenceHandleDB;
  end;

  TBoldEvolveDBAction = class(TBoldPersistenceHandleAction)
  private
    fGenerateGenericScript: boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
  published
    property BoldPersistenceHandleDB;
    property GenerateGenericScript: boolean read fGenerateGenericScript write fGenerateGenericScript;
  end;

implementation

uses
  BoldDefs,
  SysUtils,
  BoldHandles,
  BoldSystemHandle,
  BoldActionDefs,
  BoldDbEvolutor,
  BoldDbEvolutorForm,
  BoldUtils;

const
  breFreeHandle = 44;
  breValueIdentityChanged = 45;

{ TBoldPersistenceHandleAction }

procedure TBoldPersistenceHandleAction._HandleSubscriberReceive(
  Originator: TObject; OriginalEvent: TBoldEvent;
  RequestedEvent: TBoldRequestedEvent);
begin
  Assert(Originator = fBoldPersistenceHandleDB);
  Assert(RequestedEvent in [breFreeHandle]);
  case RequestedEvent of
    breFreeHandle: fBoldPersistenceHandleDB := nil;
  end;
end;

constructor TBoldPersistenceHandleAction.Create(AOwner: TComponent);
begin
  inherited;
  fHandleSubscriber := TBoldPassthroughSubscriber.Create(_HandleSubscriberReceive);
  if (TBoldSystemHandle.DefaultBoldSystemHandle <> nil) and not (csLoading in ComponentState) and (csDesigning in ComponentState) then
    BoldPersistenceHandleDB := (TBoldSystemHandle.DefaultBoldSystemHandle as TBoldSystemHandle).PersistenceHandleDB as TBoldPersistenceHandleDB;
end;

destructor TBoldPersistenceHandleAction.Destroy;
begin
  inherited;
  FreeAndNil(fHandleSubscriber);
end;

function TBoldPersistenceHandleAction.HandlesTarget(Target: TObject): Boolean;
begin
  Result := True;
end;

procedure TBoldPersistenceHandleAction.SetBoldPersistenceHandle(
  const Value: TBoldPersistenceHandleDB);
begin
  if (fBoldPersistenceHandleDB <> Value) then
  begin
    fHandleSubscriber.CancelAllSubscriptions;
    fBoldPersistenceHandleDB := Value;
    if Assigned(fBoldPersistenceHandleDB) then
      fBoldPersistenceHandleDB.AddSmallSubscription(fHandleSubscriber, [beDestroying], breFreeHandle);
  end;
end;

procedure TBoldPersistenceHandleAction.UpdateTarget(Target: TObject);
var
  EnableAction: boolean;
begin
  inherited;
  EnableAction := True;
  CheckAllowEnable(EnableAction);
  Enabled := EnableAction;
end;

procedure TBoldPersistenceHandleAction.CheckAllowEnable(var EnableAction: boolean);
begin
  EnableAction := Assigned(fBoldPersistenceHandleDB);
end;

{ TBoldGenerateSchemaAction }

procedure TBoldGenerateSchemaAction.CheckAllowEnable(var EnableAction: boolean);
begin
  inherited;
  EnableAction := EnableAction and
                  Assigned(BoldPersistenceHandleDB.DatabaseAdapter)
                  and not BoldPersistenceHandleDB.Active;
end;

constructor TBoldGenerateSchemaAction.Create(AOwner: TComponent);
begin
  inherited;
  Caption := sGenerateSchema;
end;

procedure TBoldGenerateSchemaAction.ExecuteTarget(Target: TObject);
begin
  inherited;
  BoldPersistenceHandleDB.CreateDataBaseSchema(IgnoreUnknownTables);
end;


{ TBoldValidateDBStructureAction }

constructor TBoldValidateDBStructureAction.Create(AOwner: TComponent);
begin
  inherited;
  Caption := 'Validate DB Structure';
end;

destructor TBoldValidateDBStructureAction.Destroy;
begin
  FreeAndNil(fValidator);
  inherited;
end;

procedure TBoldValidateDBStructureAction.ExecuteTarget(Target: TObject);
begin
  inherited;
  if not assigned(fValidator) then
    fValidator := TBoldDbStructureValidator.Create(nil);
  fValidator.PersistenceHandle := BoldPersistenceHandleDB;
  fValidator.Execute;
end;

{ TBoldValidateDBDataAction }

constructor TBoldValidateDBDataAction.Create(AOwner: TComponent);
begin
  inherited;
  Caption := 'Validate DB Data';
  PauseBetweenQueries := 1;
end;

destructor TBoldValidateDBDataAction.Destroy;
begin
  FreeAndNil(fValidator);
  inherited;
end;

procedure TBoldValidateDBDataAction.ExecuteTarget(Target: TObject);
begin
  inherited;
  if not assigned(fValidator) then
    fValidator := TBoldDbDataValidator.Create(nil);
  fValidator.PersistenceHandle := BoldPersistenceHandleDB;
  fValidator.Execute;
end;

{ TBoldEvolveDBAction }

constructor TBoldEvolveDBAction.Create(AOwner: TComponent);
begin
  inherited;
  Caption := 'Evolve DB';
end;

procedure TBoldEvolveDBAction.ExecuteTarget(Target: TObject);
begin
  inherited;
  TfrmBoldDbEvolutor.EvolveDB(BoldPersistenceHandleDB, GenerateGenericScript);
end;

end.
