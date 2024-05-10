
{ Global compiler directives }
{$include bold.inc}
unit BoldExternalPersistenceHandleEventDriven;

interface

uses
  Classes,
  BoldAbstractpartiallyExternalPH,
  BoldPersistenceController,
  BoldExternalPersistenceControllerConfig,
  BoldExternalPersistenceControllerEventDriven;

type

  TBoldExternalPersistenceHandleEventDriven = class;


  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldExternalPersistenceHandleEventDriven = class(TBoldAbstractpartiallyExternalPH)
  private
    FConfig: TBoldExternalPersistenceConfigItems;
    fMaxFetchBlockSize: integer;
    procedure SetConfig(const Value: TBoldExternalPersistenceConfigItems);
    function GetPersistenceController: TBoldExternalPersistenceControllerEventDriven;
  protected
    function CreatePersistenceController: TBoldPersistenceController; override;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    property PersistenceController: TBoldExternalPersistenceControllerEventDriven read GetPersistenceController;
  published
    property Config: TBoldExternalPersistenceConfigItems read fConfig write SetConfig;
    property MaxFetchBlockSize: integer read fMaxFetchBlockSize write fMaxFetchBlockSize default 250;
    property UpdateBoldDatabaseFirst;
    {$IFNDEF T2H}
    property NextPersistenceHandle;
    property BoldModel;
    property OnStartUpdates;
    property OnEndUpdates;
    property OnFailUpdates;
    property OnActivate;
    property OnDeActivate;
    {$ENDIF}
  end;

implementation

uses
  SysUtils,
  BoldAbstractModel;

{ TBoldExternalPersistenceHandleEventDriven }

constructor TBoldExternalPersistenceHandleEventDriven.Create(Owner: TComponent);
begin
  inherited;
  FConfig := TBoldExternalPersistenceConfigItems.Create(self);
  fMaxFetchBlockSize := 250; 
end;

destructor TBoldExternalPersistenceHandleEventDriven.Destroy;
begin
  FreeAndNil(FConfig);
  inherited;
end;

function TBoldExternalPersistenceHandleEventDriven.CreatePersistenceController: TBoldPersistenceController;
var
  Controller: TBoldExternalPersistenceControllerEventDriven;
begin
  Controller := TBoldExternalPersistenceControllerEventDriven.Create(BoldModel.MoldModel, Config, BoldModel.TypeNameDictionary, OnStartUpdates, OnEndUpdates, OnFailUpdates, MaxFetchBlockSize, UpdateBoldDatabaseFirst);
  ChainPersistenceController(Controller);
  Result := Controller;
end;



procedure TBoldExternalPersistenceHandleEventDriven.SetConfig(const Value: TBoldExternalPersistenceConfigItems);
begin
  if Assigned(Value) then
    Config.Assign(Value);
end;

function TBoldExternalPersistenceHandleEventDriven.GetPersistenceController: TBoldExternalPersistenceControllerEventDriven;
begin
  result := inherited PersistenceController as TBoldExternalPersistenceControllerEventDriven;
end;


end.
