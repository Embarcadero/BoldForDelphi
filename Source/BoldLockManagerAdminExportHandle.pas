
{ Global compiler directives }
{$include bold.inc}
unit BoldLockManagerAdminExportHandle;

interface

uses
  BoldComServerHandles,
  Classes
  ;

type
  {forward declarations}
  TBoldLockManagerAdminComExportHandle = class;

  TBoldLockManagerAdminComExportHandle = class(TBoldComExportHandle)
  protected
    function GetComObject: IUnknown; override;
    function GetHandledObject: TObject; override;    
  public
    constructor Create(Active: Boolean; ServerHandle: TBoldComServerHandle; ServerClass: string); reintroduce;
  end;

implementation

uses
  BoldLockManagerAdminCOM,
  BoldLockingDefs,
  BoldPropagatorServer,
  comobj
  ;

  {TBoldLockManagerAdminExportHandle}

constructor TBoldLockManagerAdminComExportHandle.Create(Active: Boolean; ServerHandle: TBoldComServerHandle; ServerClass: string);
begin
  inherited Create(nil);
  ObjectName := LOCK_MANAGER_ADMIN_OBJECT_NAME;
  self.Active := Active;
  self.ServerHandle := ServerHandle;
  self.ServerClass := ServerClass;
end;

function TBoldLockManagerAdminComExportHandle.GetComObject: IUnknown;
begin
  Result := CreateComObject(TBoldPropagatorServer.Instance.LockManagerAdminCLSID);
end;                   

function TBoldLockManagerAdminComExportHandle.GetHandledObject: TObject;
begin
  Result := nil;
end;

end.
