
{ Global compiler directives }
{$include bold.inc}
unit BoldClientHandlerExportHandle;

interface

uses
  BoldComServerHandles,
  Classes
  ;

type
  {forward declarations}
  TBoldClientHandlerExportHandle = class;

  TBoldClientHandlerExportHandle = class(TBoldComExportHandle)
  protected
    function GetComObject: IUnknown; override;
    function GetHandledObject: TObject; override;
  public
    constructor Create(Active: Boolean; ServerHandle: TBoldComServerHandle; ServerClass: string); reintroduce;
  end;

implementation

uses
  BoldClientHandlerCOM,
  BoldPropagatorConstants,
  BoldPropagatorGUIDs,
  BoldPropagatorServer,
  comobj
  ;

{ TBoldClientHandlerExportHandle }

constructor TBoldClientHandlerExportHandle.Create(Active: Boolean; ServerHandle: TBoldComServerHandle; ServerClass: string);
begin
  inherited Create(nil);
  ObjectName := CLIENT_HANDLER_OBJECT_NAME;
  self.Active := Active;
  self.ServerHandle := ServerHandle;
  self.ServerClass := ServerClass;
end;

function TBoldClientHandlerExportHandle.GetComObject: IUnknown;
begin
  Result := CreateComObject(TBoldPropagatorServer.Instance.ClientHandlerCLSID);
end;

function TBoldClientHandlerExportHandle.GetHandledObject: TObject;
begin
  Result := nil;
end;

end.
