
{ Global compiler directives }
{$include bold.inc}
unit BoldLockManagerExportHandle;

interface

uses
  BoldComServerHandles,
  Classes,
  comobj
  ;

type
  {forward declarations}
  TBoldLockManagerComExportHandle = class;

  TBoldLockManagerComExportHandle = class(TBoldComExportHandle)
  protected
    function GetComObject: IUnknown; override;
    function GetHandledObject: TObject; override;    
  public
    constructor Create(Active: Boolean; ServerHandle: TBoldComServerHandle; ServerClass: string); reintroduce; 
  end;

implementation

uses
  BoldLockManagerCOM,
  BoldLockingDefs,
  BoldPropagatorServer
  ;

{ TBoldLockManagerComExportHandle }

constructor TBoldLockManagerComExportHandle.Create(Active: Boolean; ServerHandle: TBoldComServerHandle; ServerClass: string);
begin
  inherited Create(nil);
  ObjectName := LOCK_MANAGER_OBJECT_NAME;
  self.Active := Active;
  self.ServerHandle := ServerHandle;
  self.ServerClass := ServerClass;
end;

function TBoldLockManagerComExportHandle.GetComObject: IUnknown;
begin
  Result := CreateComObject(TBoldPropagatorServer.Instance.LockManagerCLSID);
end;

function TBoldLockManagerComExportHandle.GetHandledObject: TObject;
begin
  Result := nil;
end;

end.
