
{ Global compiler directives }
{$include bold.inc}
unit BoldServicePropagatorUnit;

interface

uses
  SysUtils,
  BoldUtils,
  BoldComServiceRegister,
  Windows,
  Classes,
  SvcMgr,
  comobj;

type
  TBoldPropagatorSvc = class(TService)
    procedure ServiceAfterInstall(Sender: TService);
    procedure ServiceAfterUninstall(Sender: TService);
    procedure ServiceStart(Sender: TService; var Started: Boolean);
  private
    FAppID: string;
    function getAppId: string;
  public
    function GetServiceController: TServiceController; override;
    property AppID: string read getAppID write FAppID;
  end;

var
  BoldPropagatorSvc: TBoldPropagatorSvc;

implementation

uses
  BoldAdvancedPropagator,
  BoldPropagatorServer,
  comserv,
  registry
  ;

{$R *.DFM}

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  BoldPropagatorSvc.Controller(CtrlCode);
end;

function TBoldPropagatorSvc.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TBoldPropagatorSvc.ServiceAfterInstall(Sender: TService);
begin
  RegisterServerAsService(True, AppId, Name);
end;

procedure TBoldPropagatorSvc.ServiceAfterUninstall(Sender: TService);
begin
  RegisterServerAsService(False, AppId, Name);
end;

procedure TBoldPropagatorSvc.ServiceStart(Sender: TService; var Started: Boolean);
begin
  Started := true;
end;

function TBoldPropagatorSvc.getAppId: string;
begin
  if (Trim(FAppID) = '') then
    FAppId := GuidToString(TBoldPropagatorServer.Instance.AppID);
  Result := FAppID;
end;

initialization
  TBoldPropagatorServer.Instance.Initialize;

finalization
  TBoldPropagatorServer.FreeSingleton;
end.
