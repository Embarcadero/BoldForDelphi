unit BoldHTTPServerPersistenceHandlePassthrough;

interface

uses
  BoldPersistenceController,
  BoldPersistenceControllerSOAPAdapterCore,
  BoldPersistenceHandle,
  BoldAbstractModel,
  BoldSubscription,
  Classes;

type
  { forward declarations}
  TBoldHTTPServerPersistenceHandlePassthrough = class;

  { TBoldHTTPServerPersistenceHandlePassthrough }
  TBoldHTTPServerPersistenceHandlePassthrough = class(TBoldSubscribableComponent)
  private
    FPersistenceHandle: TBoldPersistenceHandle;
    FAdapterCore: TBoldPersistenceControllerSOAPAdapterCore;
    FModel: TBoldAbstractModel;
    function GetPersistenceController: TBoldPersistenceController;
    function GetAdapterCore: TBoldPersistenceControllerSOAPAdapterCore;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property PersistenceController: TBoldPersistenceController read GetPersistenceController;
    property AdapterCore: TBoldPersistenceControllerSOAPAdapterCore read getAdapterCore;
  public
    procedure Get(const request: WideString;  out reply: WideString);
  published
    property PersistenceHandle: TBoldPersistenceHandle read FPersistenceHandle write FPersistenceHandle;
    property BoldModel: TBoldAbstractModel read FModel write FModel;
  end;

implementation

uses
  SysUtils,
  BoldUtils,
  BoldDefs,
  BoldComConst;

{ TBoldHTTPServerPersistenceHandlePassthrough }

procedure TBoldHTTPServerPersistenceHandlePassthrough.Get(const request: WideString;
  out reply: WideString);
begin
  try
    if Assigned(PersistenceController) then
      AdapterCore.Get(request, reply, PersistenceController)
    else
      raise EBold.CreateFmt(sPersistenceHandleNotAssigned, [ClassName, 'Get']); // do not localize
  except on E: Exception do
    Reply := E.Message + ' ' + request;
  end;
end;

function TBoldHTTPServerPersistenceHandlePassthrough.GetPersistenceController: TBoldPersistenceController;
begin
  Result := nil;
  if Assigned(PersistenceHandle) then
    Result := PersistenceHandle.PersistenceController;
end;

function TBoldHTTPServerPersistenceHandlePassthrough.GetAdapterCore: TBoldPersistenceControllerSOAPAdapterCore;
begin
  if not Assigned(FAdapterCore) then
    FAdapterCore := TBoldPersistenceControllerSOAPAdapterCore.Create(BoldModel.MoldModel);
  Result := FAdapterCore;
end;

procedure TBoldHTTPServerPersistenceHandlePassthrough.Notification(
  AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FModel then
      FModel := nil;
    if AComponent = FPersistenceHandle then
      FPersistenceHandle := nil;
  end;
end;

end.
