unit BoldComConnection;

interface

uses
  ActiveX;

const
  CATID_BoldServer: TGUID = '{E07B7DF1-77D2-11D2-B7E0-00600871B01B}';

const
  EVENT_DISCONNECT = 0;
  EVENT_SUBSCRIPTION = 1;

function BoldComConnectionTypeLibrary: ITypeLib;

// -----------------------------------------------------------------------------
// Type Library
// -----------------------------------------------------------------------------

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:
//   Type Libraries    : LIBID_xxxx
//   CoClasses         : CLASS_xxxx
//   DISPInterfaces    : DIID_xxxx
//   Non-DISP interfaces: IID_xxxx
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  BoldComConnectionMajorVersion = 1;
  BoldComConnectionMinorVersion = 0;

  LIBID_BoldComConnection: TGUID = '{E07B7DF2-77D2-11D2-B7E0-00600871B01B}';

  IID_IBoldClient: TGUID = '{E07B7DF3-77D2-11D2-B7E0-00600871B01B}';
  IID_IBoldServer: TGUID = '{E07B7DF4-77D2-11D2-B7E0-00600871B01B}';
  IID_IBoldProvider: TGUID = '{E07B7DF5-77D2-11D2-B7E0-00600871B01B}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary
// *********************************************************************//
  IBoldClient = interface;
  IBoldClientDisp = dispinterface;
  IBoldServer = interface;
  IBoldServerDisp = dispinterface;
  IBoldProvider = interface;
  IBoldProviderDisp = dispinterface;

// *********************************************************************//
// Interface: IBoldClient
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {E07B7DF3-77D2-11D2-B7E0-00600871B01B}
// *********************************************************************//
  IBoldClient = interface(IDispatch)
    ['{E07B7DF3-77D2-11D2-B7E0-00600871B01B}']
    function  OnServerEvent(Event: Integer; Data: OleVariant): OleVariant; safecall;
  end;

// *********************************************************************//
// DispIntf:  IBoldClientDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {E07B7DF3-77D2-11D2-B7E0-00600871B01B}
// *********************************************************************//
  IBoldClientDisp = dispinterface
    ['{E07B7DF3-77D2-11D2-B7E0-00600871B01B}']
    function  OnServerEvent(Event: Integer; Data: OleVariant): OleVariant; dispid 1;
  end;

// *********************************************************************//
// Interface: IBoldServer
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {E07B7DF4-77D2-11D2-B7E0-00600871B01B}
// *********************************************************************//
  IBoldServer = interface(IDispatch)
    ['{E07B7DF4-77D2-11D2-B7E0-00600871B01B}']
    function  Connect(const ClientId: WideString; Flags: Integer; const Client: IBoldClient): WordBool; safecall;
    function  Disconnect: WordBool; safecall;
    function  Execute(const Name: WideString; Params: OleVariant): OleVariant; safecall;
  end;

// *********************************************************************//
// DispIntf:  IBoldServerDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {E07B7DF4-77D2-11D2-B7E0-00600871B01B}
// *********************************************************************//
  IBoldServerDisp = dispinterface
    ['{E07B7DF4-77D2-11D2-B7E0-00600871B01B}']
    function  Connect(const ClientId: WideString; Flags: Integer; const Client: IBoldClient): WordBool; dispid 1;
    function  Disconnect: WordBool; dispid 2;
    function  Execute(const Name: WideString; Params: OleVariant): OleVariant; dispid 3;
  end;

// *********************************************************************//
// Interface: IBoldProvider
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {E07B7DF5-77D2-11D2-B7E0-00600871B01B}
// *********************************************************************//
  IBoldProvider = interface(IDispatch)
    ['{E07B7DF5-77D2-11D2-B7E0-00600871B01B}']
    function  CreateObject(const ClassName: WideString): IUnknown; safecall;
    function  GetObject(const ObjectName: WideString): IUnknown; safecall;
    function  Get_ObjectInfo: OleVariant; safecall;
    property ObjectInfo: OleVariant read Get_ObjectInfo;
  end;

// *********************************************************************//
// DispIntf:  IBoldProviderDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {E07B7DF5-77D2-11D2-B7E0-00600871B01B}
// *********************************************************************//
  IBoldProviderDisp = dispinterface
    ['{E07B7DF5-77D2-11D2-B7E0-00600871B01B}']
    function  CreateObject(const ClassName: WideString): IUnknown; dispid 1;
    function  GetObject(const ObjectName: WideString): IUnknown; dispid 2;
    property ObjectInfo: OleVariant readonly dispid 3;
  end;

// -----------------------------------------------------------------------------
// Type Library End
// -----------------------------------------------------------------------------

implementation

uses
  SysUtils,
  BoldComConst,
  BoldComUtils;

var
  G_TypeLibrary: ITypeLib = nil;

function BoldComConnectionTypeLibrary: ITypeLib;
begin
  if not Assigned(G_TypeLibrary) then
  begin
    if LoadRegTypeLib(LIBID_BoldComConnection,1,0,0,G_TypeLibrary) <> 0 then
      raise EBoldCom.Create(sUnableToLoadTypeLib);
  end;
  Result := G_TypeLibrary;
end;

end.
