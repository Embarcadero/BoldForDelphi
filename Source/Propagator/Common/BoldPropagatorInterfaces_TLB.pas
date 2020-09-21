unit BoldPropagatorInterfaces_TLB;

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
// File generated on 2002-05-22 15:40:37 from Type Library described below.

// ************************************************************************ //
// Type Lib: C:\Work\BFD\SOURCE\propagator\Common\BoldPropagatorInterfaces.tlb (1)
// IID\LCID: {DC6A300A-C8C2-4D26-9A8D-C8ECB164B54B}\0
// Helpfile: 
// DepndLst: 
//   (1) v2.0 stdole, (C:\WINNT\System32\stdole2.tlb)
//   (2) v4.0 StdVCL, (C:\WINNT\System32\STDVCL40.DLL)
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
  BoldPropagatorInterfacesMajorVersion = 1;
  BoldPropagatorInterfacesMinorVersion = 0;

  LIBID_BoldPropagatorInterfaces: TGUID = '{DC6A300A-C8C2-4D26-9A8D-C8ECB164B54B}';

  IID_IBoldEventPropagator: TGUID = '{BB7ABB77-BCE2-44C8-9510-F760F507A298}';
  IID_IBoldClientHandler: TGUID = '{A86E36B1-5EA3-4961-8448-45FD822A271E}';
  IID_IBoldListener: TGUID = '{0326BF5B-F5AF-4BED-B5E1-F84D2549415A}';
  CLASS_BoldPropagator: TGUID = '{97C5A9AD-B5F4-4350-A51D-028975782BDA}';
  IID_IBoldListenerAdmin: TGUID = '{7457EA48-E3B5-4E07-8496-87229ACA5E2D}';
  CLASS_BoldListener: TGUID = '{A1582C23-E7AB-451A-837F-2131B425E73B}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IBoldEventPropagator = interface;
  IBoldClientHandler = interface;
  IBoldListener = interface;
  IBoldListenerDisp = dispinterface;
  IBoldListenerAdmin = interface;
  IBoldListenerAdminDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  BoldPropagator = IBoldClientHandler;
  BoldListener = IBoldListener;


// *********************************************************************//
// Interface: IBoldEventPropagator
// Flags:     (256) OleAutomation
// GUID:      {BB7ABB77-BCE2-44C8-9510-F760F507A298}
// *********************************************************************//
  IBoldEventPropagator = interface(IUnknown)
    ['{BB7ABB77-BCE2-44C8-9510-F760F507A298}']
    function  SendEvents(BoldClientID: Integer; Events: OleVariant): HResult; stdcall;
    function  AddSubscriptions(BoldClientID: Integer; Subscriptions: OleVariant): HResult; stdcall;
    function  CancelSubscriptions(BoldClientID: Integer; Subscriptions: OleVariant): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IBoldClientHandler
// Flags:     (256) OleAutomation
// GUID:      {A86E36B1-5EA3-4961-8448-45FD822A271E}
// *********************************************************************//
  IBoldClientHandler = interface(IUnknown)
    ['{A86E36B1-5EA3-4961-8448-45FD822A271E}']
    function  RegisterClient(LeaseDuration: Integer; PollingInterval: Integer; 
                             const BoldClientListener: IBoldListener; 
                             const ClientIDString: WideString; out BoldClientID: Integer): HResult; stdcall;
    function  ExtendLease(BoldClientID: Integer; LeaseDuration: Integer; out ExtensionOK: WordBool): HResult; stdcall;
    function  UnRegisterClient(BoldClientID: Integer): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IBoldListener
// Flags:     (320) Dual OleAutomation
// GUID:      {0326BF5B-F5AF-4BED-B5E1-F84D2549415A}
// *********************************************************************//
  IBoldListener = interface(IUnknown)
    ['{0326BF5B-F5AF-4BED-B5E1-F84D2549415A}']
    function  ReceiveEvents(Events: OleVariant): Integer; safecall;
  end;

// *********************************************************************//
// DispIntf:  IBoldListenerDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {0326BF5B-F5AF-4BED-B5E1-F84D2549415A}
// *********************************************************************//
  IBoldListenerDisp = dispinterface
    ['{0326BF5B-F5AF-4BED-B5E1-F84D2549415A}']
    function  ReceiveEvents(Events: OleVariant): Integer; dispid 1;
  end;

// *********************************************************************//
// Interface: IBoldListenerAdmin
// Flags:     (320) Dual OleAutomation
// GUID:      {7457EA48-E3B5-4E07-8496-87229ACA5E2D}
// *********************************************************************//
  IBoldListenerAdmin = interface(IUnknown)
    ['{7457EA48-E3B5-4E07-8496-87229ACA5E2D}']
    function  Ping: Integer; safecall;
    procedure DisconnectClient(const aMessage: WideString; RemainDisconnected: Integer); safecall;
  end;

// *********************************************************************//
// DispIntf:  IBoldListenerAdminDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {7457EA48-E3B5-4E07-8496-87229ACA5E2D}
// *********************************************************************//
  IBoldListenerAdminDisp = dispinterface
    ['{7457EA48-E3B5-4E07-8496-87229ACA5E2D}']
    function  Ping: Integer; dispid 1;
    procedure DisconnectClient(const aMessage: WideString; RemainDisconnected: Integer); dispid 2;
  end;

// *********************************************************************//
// The Class CoBoldPropagator provides a Create and CreateRemote method to          
// create instances of the default interface IBoldClientHandler exposed by              
// the CoClass BoldPropagator. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoBoldPropagator = class
    class function Create: IBoldClientHandler;
    class function CreateRemote(const MachineName: string): IBoldClientHandler;
  end;

// *********************************************************************//
// The Class CoBoldListener provides a Create and CreateRemote method to          
// create instances of the default interface IBoldListener exposed by              
// the CoClass BoldListener. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoBoldListener = class
    class function Create: IBoldListener;
    class function CreateRemote(const MachineName: string): IBoldListener;
  end;

implementation

uses ComObj;

class function CoBoldPropagator.Create: IBoldClientHandler;
begin
  Result := CreateComObject(CLASS_BoldPropagator) as IBoldClientHandler;
end;

class function CoBoldPropagator.CreateRemote(const MachineName: string): IBoldClientHandler;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_BoldPropagator) as IBoldClientHandler;
end;

class function CoBoldListener.Create: IBoldListener;
begin
  Result := CreateComObject(CLASS_BoldListener) as IBoldListener;
end;

class function CoBoldListener.CreateRemote(const MachineName: string): IBoldListener;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_BoldListener) as IBoldListener;
end;

end.
