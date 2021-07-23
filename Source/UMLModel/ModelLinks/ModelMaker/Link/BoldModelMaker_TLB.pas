
{ Global compiler directives }
{$include bold.inc}
unit BoldModelMaker_TLB;




























{$TYPEDADDRESS OFF}
interface

uses Windows, ActiveX, Classes, Graphics, OleServer, OleCtrls, StdVCL;






const
  ModelMakerMajorVersion = 1;
  ModelMakerMinorVersion = 0;

  LIBID_ModelMaker: TGUID = '{D077CEC0-83F0-11D5-A1D2-00C0DFE529B9}';

  IID_IApp: TGUID = '{D077CEC1-83F0-11D5-A1D2-00C0DFE529B9}';
  CLASS_App: TGUID = '{D077CEC3-83F0-11D5-A1D2-00C0DFE529B9}';
type


  IApp = interface;
  IAppDisp = dispinterface;



  App = IApp;




  IApp = interface(IDispatch)
    ['{D077CEC1-83F0-11D5-A1D2-00C0DFE529B9}']
    function  GetExpert(const ExpertID: WideString): IDispatch; safecall;
  end;




  IAppDisp = dispinterface
    ['{D077CEC1-83F0-11D5-A1D2-00C0DFE529B9}']
    function  GetExpert(const ExpertID: WideString): IDispatch; dispid 1;
  end;






  CoApp = class
    class function Create: IApp;
    class function CreateRemote(const MachineName: string): IApp;
  end;








{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TAppProperties= class;
{$ENDIF}
  TApp = class(TOleServer)
  private
    FIntf:        IApp;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps:       TAppProperties;
    function      GetServerProperties: TAppProperties;
{$ENDIF}
    function      GetDefaultInterface: IApp;
  protected
    procedure InitServerData; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IApp);
    procedure Disconnect; override;
    function  GetExpert(const ExpertID: WideString): IDispatch;
    property  DefaultInterface: IApp read GetDefaultInterface;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TAppProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}





 TAppProperties = class(TPersistent)
  private
    FServer:    TApp;
    function    GetDefaultInterface: IApp;
    constructor Create(AServer: TApp);
  protected
  public
    property DefaultInterface: IApp read GetDefaultInterface;
  published
  end;
{$ENDIF}

implementation

uses ComObj;

class function CoApp.Create: IApp;
begin
  Result := CreateComObject(CLASS_App) as IApp;
end;

class function CoApp.CreateRemote(const MachineName: string): IApp;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_App) as IApp;
end;

procedure TApp.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{D077CEC3-83F0-11D5-A1D2-00C0DFE529B9}';
    IntfIID:   '{D077CEC1-83F0-11D5-A1D2-00C0DFE529B9}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TApp.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IApp;
  end;
end;

procedure TApp.ConnectTo(svrIntf: IApp);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TApp.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TApp.GetDefaultInterface: IApp;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call ''Connect'' or ''ConnectTo'' before this operation');
  Result := FIntf;
end;

constructor TApp.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TAppProperties.Create(Self);
{$ENDIF}
end;

destructor TApp.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TApp.GetServerProperties: TAppProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function  TApp.GetExpert(const ExpertID: WideString): IDispatch;
begin
  Result := DefaultInterface.GetExpert(ExpertID);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TAppProperties.Create(AServer: TApp);
begin
  inherited Create;
  FServer := AServer;
end;

function TAppProperties.GetDefaultInterface: IApp;
begin
  Result := FServer.DefaultInterface;
end;

{$ENDIF}


end.
