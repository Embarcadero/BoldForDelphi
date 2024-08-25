
{ Global compiler directives }
{$include bold.inc}
unit BoldHTTPClientPersistenceHandle;

interface

uses
  BoldPersistenceHandle,
  BoldPersistenceController,
  BoldHTTPPersistenceControllerClient,
  BoldAbstractModel,
  BoldWebConnection,
  classes
  ;

type
  { forward declarations }
  TBoldHTTPClientPersistenceHandle = class;

  { TBoldHTTPClientPersistenceHandle }
  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldHTTPClientPersistenceHandle = class(TBoldPersistenceHandle)
  private
    FModel: TBoldAbstractModel;
    FPersistenceController: TBoldPersistenceController;
    FWebConnection: TBoldWebConnection;
    procedure setWebConnection(Value: TBoldWebConnection);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function CreatePersistenceController: TBoldPersistenceController; override;
  published
    property BoldModel: TBoldAbstractModel read fModel write fModel;
    property WebConnection: TBoldWebConnection read FWebConnection write setWebConnection;
  end;

implementation

uses
  SysUtils,
  BoldUtils,
  BoldDefs
  ;

{ TBoldHTTPClientPersistenceHandle }

function TBoldHTTPClientPersistenceHandle.CreatePersistenceController: TBoldPersistenceController;
begin
  if not assigned(BoldModel) then
    raise EBold.createfmt('%s.CreatePersistenceController: Can not get a PersistenceController without a Model', [ClassName]);
  FPersistenceController := TBoldHTTPPersistenceControllerClient.Create(fModel.MoldModel);
  (FPersistenceController as TBoldHTTPPersistenceControllerClient).WebConnection := WebConnection;
  result := FPersistenceController;
end;

procedure TBoldHTTPClientPersistenceHandle.Notification(
  AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FModel then
      FModel := nil
    else if AComponent = FWebConnection then
      WebConnection := nil;
  end;
end;

procedure TBoldHTTPClientPersistenceHandle.setWebConnection(
  Value: TBoldWebConnection);
begin
  if (Value <> FWebConnection) then
  begin
    FWebConnection := Value;
    if Assigned(FPersistenceController) then
      (FPersistenceController as TBoldHTTPPersistenceControllerClient).WebConnection := Value;
  end;
end;

end.
