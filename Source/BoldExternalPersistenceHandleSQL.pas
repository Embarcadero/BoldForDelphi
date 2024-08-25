
{ Global compiler directives }
{$include bold.inc}
unit BoldExternalPersistenceHandleSQL;

interface

uses
  SysUtils,
  Classes,
  BoldSubscription,
  BoldTypeNameDictionary,
  BoldAbstractModel,
  BoldDefs,
  BoldMeta,
  BoldAbstractpartiallyExternalPH,
  BoldExternalPersistenceControllerSQL,
  BoldAbstractDatabaseAdapter,
  BoldPersistenceController;

type
  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldExternalPersistenceHandleSQL = class(TBoldAbstractPartiallyExternalPH)
  private
    FDatabaseAdapter: TBoldAbstractDatabaseAdapter;
    FClassesToHandle: TStringList;
    function GetPersistenceController: TBoldExternalPersistenceControllerSQL;
    function GetClassesToHandle: TStrings;
    procedure SetClassesToHandle(const Value: TStrings);
  protected
    function CreatePersistenceController: TBoldPersistenceController; override;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    property PersistenceController: TBoldExternalPersistenceControllerSQL read GetPersistenceController;
  published
    property UpdateBoldDatabaseFirst;  
    property ClassesToHandle: TStrings read GetClassesToHandle write SetClassesToHandle;
    property DatabaseAdapter: TBoldAbstractDatabaseAdapter read FDatabaseAdapter write FDatabaseAdapter;
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


{ TBoldExternalPersistenceHandleSQL }

constructor TBoldExternalPersistenceHandleSQL.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  FClassesToHandle := TStringList.Create;
end;

destructor TBoldExternalPersistenceHandleSQL.Destroy;
begin
  FClassesToHandle.Free;
  inherited;
end;

function TBoldExternalPersistenceHandleSQL.CreatePersistenceController: TBoldPersistenceController;
var
  Controller: TBoldExternalPersistenceControllerSQL;
begin
  Controller := TBoldExternalPersistenceControllerSQL.Create(
    BoldModel.MoldModel, FDatabaseAdapter, BoldModel.TypeNameDictionary,
    OnStartUpdates, OnEndUpdates, OnFailUpdates, ClassesToHandle, UpdateBoldDatabaseFirst);
  ChainPersistenceController(Controller);
  Result := Controller;
end;

function TBoldExternalPersistenceHandleSQL.GetClassesToHandle: TStrings;
begin
  Result := FClassesToHandle;
end;

procedure TBoldExternalPersistenceHandleSQL.SetClassesToHandle(const Value: TStrings);
begin
  if Assigned(Value) then
    FClassesToHandle.Assign(Value);
end;

function TBoldExternalPersistenceHandleSQL.GetPersistenceController: TBoldExternalPersistenceControllerSQL;
begin
  result := inherited PersistenceController as TBoldExternalPersistenceControllerSQL;
end;

end.
