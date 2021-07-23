
{ Global compiler directives }
{$include bold.inc}
unit BoldEnqueuerExportHandle;

interface

uses
  BoldComServerHandles,
  Classes
  ;

type
  {forward declarations}
  TBoldEnqueuerExportHandle = class;

  TBoldEnqueuerExportHandle = class(TBoldComExportHandle)
  protected
    function GetComObject: IUnknown; override;
    function GetHandledObject: TObject; override;
  public
    constructor Create(Active: Boolean; ServerHandle: TBoldComServerHandle; ServerClass: string); reintroduce;
  end;

implementation

uses
  BoldEnqueuerCOM,
  BoldPropagatorConstants,
  BoldPropagatorGUIDs,
  BoldPropagatorServer,
  comobj
  ;

{ TBoldEnqueuerExportHandle }

constructor TBoldEnqueuerExportHandle.Create(Active: Boolean; ServerHandle: TBoldComServerHandle; ServerClass: string);
begin
  inherited Create(nil);
  ObjectName := ENQUEUER_OBJECT_NAME;
  self.Active := Active;
  self.ServerHandle := ServerHandle;
  self.ServerClass := ServerClass;
end;

function TBoldEnqueuerExportHandle.GetComObject: IUnknown;
begin
  Result := CreateComObject(TBoldPropagatorServer.Instance.EnqueuerCLSID);
end;

function TBoldEnqueuerExportHandle.GetHandledObject: TObject;
begin
  Result := nil;
end;

end.
