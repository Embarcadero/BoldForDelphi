
{ Global compiler directives }
{$include bold.inc}
unit BoldSOAPServerPersistenceHandles;

interface

uses
  Classes,
  BoldPersistenceHandle,
  BoldSOAPPersistenceControllerStub,
  BoldAbstractModel,
  BoldSOAP_TLB,
  BoldComServerHandles;

type
  { forward declarations }
  TBoldSOAPServerPersistenceHandle = class;

  {-- TBoldComServerElementHandle --}
  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldSOAPServerPersistenceHandle = class(TBoldComExportHandle)
  private
    FBoldHandle: TBoldPersistenceHandle;
    fSOAPStub: TBoldSOAPPersistenceControllerAdapter;
    fSOAPIntf: IBoldSOAPService;
    fModel: TBoldAbstractModel;
    procedure SetBoldHandle(Value: TBoldPersistenceHandle);
  protected
    function GetComObject: IUnknown; override;
    function GetHandledObject: TObject; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  published
    property BoldHandle: TBoldPersistenceHandle read FBoldHandle write SetBoldHandle;
    property BoldModel: TBoldAbstractModel read fModel write fModel;
  end;

implementation


{-- TBoldComServerPersistenceHandle -------------------------------------------}

function TBoldSOAPServerPersistenceHandle.GetComObject: IUnknown;
begin
  if not assigned(fSOAPStub) then
    fSOAPStub := TBoldSOAPPersistenceControllerAdapter.Create(fModel.MoldModel, FBoldHandle.PersistenceController, False, nil, IUnknown);
  fSOAPIntf := fSOAPStub;
  result := fSOAPStub;
end;

function TBoldSOAPServerPersistenceHandle.GetHandledObject: TObject;
begin
  Result := nil;
end;

procedure TBoldSOAPServerPersistenceHandle.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FBoldHandle then
      BoldHandle := nil;
  end;
end;

procedure TBoldSOAPServerPersistenceHandle.SetBoldHandle(Value: TBoldPersistenceHandle);
begin
  if Value <> FBoldHandle then
  begin
    FBoldHandle := Value;
    if Assigned(FBoldHandle) then
      FBoldHandle.FreeNotification(Self);
  end;
end;

end.
