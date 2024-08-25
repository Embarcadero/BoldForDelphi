
{ Global compiler directives }
{$include bold.inc}
unit BoldComServerElementHandles;

interface

uses
  Classes,
  BoldHandles, { TBoldElementHandle }
  BoldComServerHandles;

type
  TBoldComServerElementExportMode = (emHandle, emValue);

  { forward declarations }
  TBoldComServerElementHandle = class;

  {-- TBoldComServerElementHandle --}
  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldComServerElementHandle = class(TBoldComExportHandle)
  private
    FBoldHandle: TBoldElementHandle;
    FExportMode: TBoldComServerElementExportMode;
    procedure SetBoldHandle(Value: TBoldElementHandle);
    procedure SetExportMode(Value: TBoldComServerElementExportMode);
  protected
    function GetComObject: IUnknown; override;
    function GetHandledObject: TObject; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  published
    property BoldHandle: TBoldElementHandle read FBoldHandle write SetBoldHandle;
    property ExportMode: TBoldComServerElementExportMode read FExportMode write SetExportMode;
  end;

implementation

uses
  BoldComAdapter,

  BoldComObjectSpaceAdapters,
  BoldComServerElementHandleFactory;

{-- TBoldComServerElementHandle -----------------------------------------------}

function TBoldComServerElementHandle.GetComObject: IUnknown;
begin
  if Assigned(BoldHandle) then
  begin
    if ExportMode = emHandle then
      Result := TBoldComAdapterFactory.Instance.CreateAdapterForObject(BoldHandle)
    else if (ExportMode = emValue) and Assigned(BoldHandle.Value) then
      Result := TBoldComAdapterFactory.Instance.CreateAdapterForObject(BoldHandle.Value)
    else
      Result := nil;
  end
  else
    Result := nil;
end;

function TBoldComServerElementHandle.GetHandledObject: TObject;
begin
  Result := nil;
end;

procedure TBoldComServerElementHandle.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FBoldHandle then
      BoldHandle := nil;
  end;
end;

procedure TBoldComServerElementHandle.SetBoldHandle(Value: TBoldElementHandle);
begin
  if Value <> FBoldHandle then
  begin
    FBoldHandle := Value;
    if Assigned(FBoldHandle) then
      FBoldHandle.FreeNotification(Self);
  end;
end;

procedure TBoldComServerElementHandle.SetExportMode(Value: TBoldComServerElementExportMode);
begin
  if Value <> FExportMode then
  begin
    FExportMode := Value;
  end;
end;

end.
