
{ Global compiler directives }
{$include bold.inc}
unit BoldServerHandlesDataMod;

interface
uses
  Messages,
  SysUtils,
  Classes,
  Controls,
  Forms,
  BoldComServerHandles,
  BoldSubscription,
  BoldHandle,
  BoldServerHandles,
  BoldClientHandlerExportHandle,
  BoldEnqueuerExportHandle,
  BoldLockManagerExportHandle,
  BoldLockManagerAdminExportHandle
  ;

type
  TdmServerHandles = class(TDataModule)
    BoldComServerHandle: TBoldComServerHandle;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    { Private declarations }
    fClientHandlerExportHandle: TBoldClientHandlerExportHandle;
    fEnqueuerExportHandle: TBoldEnqueuerExportHandle;
    fLockManagerExportHandle: TBoldLockManagerComExportHandle;
    fLockManagerAdminExportHandle: TBoldLockManagerAdminComExportHandle;
  public
    { Public declarations }
    procedure DisconnectAll;
  end;

var
  dmServerHandles: TdmServerHandles;

implementation

uses
  BoldLockManagerCOM,
  BoldPropagatorServer,
  BoldComServer,
  comobj,
  ActiveX;

{$R *.DFM}

procedure TdmServerHandles.DataModuleCreate(Sender: TObject);
begin
  BoldComServerHandle.Classes[0].Active := false;
  BoldComServerHandle.Classes[0].CLSID := GuidToString(TBoldPropagatorServer.Instance.PropagatorConnectionCLSID);
  fClientHandlerExportHandle := TBoldClientHandlerExportHandle.Create(True, BoldComServerHandle, BoldComServerHandle.Classes[0].Name);
  fEnqueuerExportHandle := TBoldEnqueuerExportHandle.Create(True, BoldComServerHandle, BoldComServerHandle.Classes[0].Name);
  fLockManagerExportHandle := TBoldLockManagerComExportHandle.Create(True, BoldComServerHandle, BoldComServerHandle.Classes[0].Name);
  fLockManagerAdminExportHandle := TBoldLockManagerAdminComExportHandle.Create(True, BoldComServerHandle, BoldComServerHandle.Classes[0].Name);
  BoldComServerHandle.Classes[0].Active := true;
  BoldComServerHandle.Active := true;
end;

procedure TdmServerHandles.DataModuleDestroy(Sender: TObject);
begin
  fClientHandlerExportHandle.Active := false;
  fEnqueuerExportHandle.Active := false;
  fLockManagerExportHandle.Active := false;
  fLockManagerAdminExportHandle.Active := false;
  FreeAndNil(fClientHandlerExportHandle);
  FreeAndNil(fEnqueuerExportHandle);
  FreeAndNil(fLockManagerExportHandle);
  FreeAndNil(fLockManagerAdminExportHandle);
end;

{This function uses brute force to close all connections to com objects in the propagator}
procedure TdmServerHandles.DisconnectAll;
begin
  fClientHandlerExportHandle.Active := false;
  fEnqueuerExportHandle.Active := false;
  fLockManagerExportHandle.Active := false;
  fLockManagerAdminExportHandle.Active := false;
  BoldComServerHandle.Active := false;
  while TBoldComServer.Instance.ConnectionCount > 0 do
    TBoldComServer.Instance.Connections[0].Disconnect;
  while TBoldPropagatorServer.Instance.ComObjectCount > 0 do
  begin
    TBoldPropagatorServer.Instance.ComObjects[0].Disconnect;
    TBoldPropagatorServer.Instance.ComObjects[0].Free;
  end;
end;

end.
