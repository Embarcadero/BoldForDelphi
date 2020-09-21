unit BoldModelMaker_TLB;

// ************************************************************************ //
// WARNING                                                                    
// -------                                                                    
// The types declared in this file were generated from data read from a       
// Type Library. If this type library is explicitly or indirectly (via        
// another type library referring to this type library) re-imported, or the   
// 'Refresh' command of the Type Library Editor activated while editing the   
// Type Library, the contents of this file will be regenerated and all        
// manual modifications will be lost.                                         
// ************************************************************************ //

// PASTLWTR : 1.1
// File generated on 12/5/2001 9:07:15 AM from Type Library described below.

// *************************************************************************//
// NOTE:                                                                      
// Items guarded by $IFDEF_LIVE_SERVER_AT_DESIGN_TIME are used by properties  
// which return objects that may need to be explicitly created via a function 
// call prior to any access via the property. These items have been disabled  
// in order to prevent accidental use from within the object inspector. You   
// may enable them by defining LIVE_SERVER_AT_DESIGN_TIME or by selectively   
// removing them from the $IFDEF blocks. However, such items must still be    
// programmatically created via a method of the appropriate CoClass before    
// they can be used.                                                          
// ************************************************************************ //
// Type Lib: E:\MM32\AutoServer\ModelMaker.tlb (1)
// IID\LCID: {D077CEC0-83F0-11D5-A1D2-00C0DFE529B9}\0
// Helpfile: 
// DepndLst: 
//   (1) v2.0 stdole, (C:\WINNT\System32\stdole2.tlb)
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers.
interface

uses Windows, ActiveX, Classes, Graphics, OleServer, OleCtrls, StdVCL;

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:
//   Type Libraries     : LIBID_xxxx
//   CoClasses          : CLASS_xxxx
//   DISPInterfaces     : DIID_xxxx
//   Non-DISP interfaces: IID_xxxx
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  ModelMakerMajorVersion = 1;
  ModelMakerMinorVersion = 0;

  LIBID_ModelMaker: TGUID = '{D077CEC0-83F0-11D5-A1D2-00C0DFE529B9}';

  IID_IApp: TGUID = '{D077CEC1-83F0-11D5-A1D2-00C0DFE529B9}';
  CLASS_App: TGUID = '{D077CEC3-83F0-11D5-A1D2-00C0DFE529B9}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary
// *********************************************************************//
  IApp = interface;
  IAppDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library
// (NOTE: Here we map each CoClass to its Default Interface)
// *********************************************************************//
  App = IApp;


// *********************************************************************//
// Interface: IApp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D077CEC1-83F0-11D5-A1D2-00C0DFE529B9}
// *********************************************************************//
  IApp = interface(IDispatch)
    ['{D077CEC1-83F0-11D5-A1D2-00C0DFE529B9}']
    function  GetExpert(const ExpertID: WideString): IDispatch; safecall;
  end;

// *********************************************************************//
// DispIntf:  IAppDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D077CEC1-83F0-11D5-A1D2-00C0DFE529B9}
// *********************************************************************//
  IAppDisp = dispinterface
    ['{D077CEC1-83F0-11D5-A1D2-00C0DFE529B9}']
    function  GetExpert(const ExpertID: WideString): IDispatch; dispid 1;
  end;

// *********************************************************************//
// The Class CoApp provides a Create and CreateRemote method to
// create instances of the default interface IApp exposed by
// the CoClass App. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoApp = class
    class function Create: IApp;
    class function CreateRemote(const MachineName: string): IApp;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TApp
// Help String      : ModelMaker Object
// Default Interface: IApp
// Def. Intf. DISP? : No
// Event   Interface:
// TypeFlags        : (2) CanCreate
// *********************************************************************//
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
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TApp
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
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
