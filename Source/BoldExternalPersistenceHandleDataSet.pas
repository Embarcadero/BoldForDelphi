
{ Global compiler directives }
{$include bold.inc}
unit BoldExternalPersistenceHandleDataSet;

interface

uses
  Classes,
  BoldPersistenceController,
  BoldAbstractpartiallyExternalPH,
  BoldExternalPersistenceConfigItemDataSet,
  BoldExternalPersistenceControllerDataSet;

type
  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldExternalPersistenceHandleDataSet = class(TBoldAbstractPartiallyExternalPH)
  private
    FConfig: TBoldExternalPersistenceConfigDataSetItems;
    FMaxFetchBlockSize: integer;
    procedure SetConfig(const Value: TBoldExternalPersistenceConfigDataSetItems);
    function GetPersistenceController: TBoldExternalPersistenceControllerDataSet;
  protected
    function CreatePersistenceController: TBoldPersistenceController; override;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    property PersistenceController: TBoldExternalPersistenceControllerDataSet read GetPersistenceController;
  published
    property Config: TBoldExternalPersistenceConfigDataSetItems read FConfig write SetConfig;
    property MaxFetchBlockSize: Integer read FMaxFetchBlockSize write FMaxFetchBlockSize default 250;
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
  SysUtils;

{ TBoldExternalPersistenceHandleDataSet }

constructor TBoldExternalPersistenceHandleDataSet.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  FConfig := TBoldExternalPersistenceConfigDataSetItems.Create(self);
  FMaxFetchBlockSize := 250;
end;

destructor TBoldExternalPersistenceHandleDataSet.Destroy;
begin
  FreeAndNil(FConfig);
  inherited;
end;

function TBoldExternalPersistenceHandleDataSet.CreatePersistenceController: TBoldPersistenceController;
var
  Controller: TBoldExternalPersistenceControllerDataSet;
begin
  Controller := TBoldExternalPersistenceControllerDataSet.Create(BoldModel.MoldModel,
    Config, BoldModel.TypeNameDictionary, OnStartUpdates, OnEndUpdates, OnFailUpdates,
    MaxFetchBlockSize);
  ChainPersistenceController(Controller);
  Result := Controller;
end;

function TBoldExternalPersistenceHandleDataSet.GetPersistenceController: TBoldExternalPersistenceControllerDataSet;
begin
  result := inherited PersistenceController as TBoldExternalPersistenceControllerDataSet;
end;

procedure TBoldExternalPersistenceHandleDataSet.SetConfig(
  const Value: TBoldExternalPersistenceConfigDataSetItems);
begin
  if Assigned(Value) then
    Config.Assign(Value);
end;

end.
