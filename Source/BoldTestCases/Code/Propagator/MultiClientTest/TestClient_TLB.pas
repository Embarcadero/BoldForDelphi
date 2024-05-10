unit TestClient_TLB;

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
// File generated on 2001-09-28 10:58:35 from Type Library described below.

// ************************************************************************ //
// Type Lib: C:\Work\BfD\TestCases\Propagator\MultiClientTest\TestClient.tlb (1)
// IID\LCID: {41888607-36A6-42A6-B748-7B6EA44D1D9F}\0
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
  TestClientMajorVersion = 1;
  TestClientMinorVersion = 0;

  LIBID_TestClient: TGUID = '{41888607-36A6-42A6-B748-7B6EA44D1D9F}';

  IID_IPropagatorTestClient: TGUID = '{DBFF90DB-A7C6-4569-8066-508A4EFBBEFF}';
  IID_IPropagatorTestManager: TGUID = '{FC876C56-02E0-41EA-BB94-D100C79EE7B1}';
  CLASS_PropagatorTestClient: TGUID = '{B7D69D85-936E-4ED0-94CF-00B61E4F264D}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IPropagatorTestClient = interface;
  IPropagatorTestManager = interface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  PropagatorTestClient = IPropagatorTestClient;


// *********************************************************************//
// Interface: IPropagatorTestClient
// Flags:     (256) OleAutomation
// GUID:      {DBFF90DB-A7C6-4569-8066-508A4EFBBEFF}
// *********************************************************************//
  IPropagatorTestClient = interface(IUnknown)
    ['{DBFF90DB-A7C6-4569-8066-508A4EFBBEFF}']
    function  CreateObject(const ClassName: WideString; out ObjectID: WideString): HResult; stdcall;
    function  UpdateNonEmbeddedState(const ClassName: WideString; const ObjectID: WideString; 
                                     const attributeName: WideString): HResult; stdcall;
    function  UpdateEmbeddedState(const ClassName: WideString; const ObjectID: WideString; 
                                  const attributeName: WideString): HResult; stdcall;
    function  DeleteObject(const ClassName: WideString; const ObjectID: WideString): HResult; stdcall;
    function  SetID(TestClientID: Integer; const TestManager: IPropagatorTestManager; 
                    const DBAlias: WideString): HResult; stdcall;
    function  RefreshObjectSpace: HResult; stdcall;
    function  GenerateEvents(Number: Integer): HResult; stdcall;
    function  UpdateDB: HResult; stdcall;
    function  StartAutoTest: HResult; stdcall;
    function  StopAutoTest: HResult; stdcall;
    function  CloseClient: HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IPropagatorTestManager
// Flags:     (256) OleAutomation
// GUID:      {FC876C56-02E0-41EA-BB94-D100C79EE7B1}
// *********************************************************************//
  IPropagatorTestManager = interface(IUnknown)
    ['{FC876C56-02E0-41EA-BB94-D100C79EE7B1}']
    function  EventReceived(const Event: WideString; TestClientID: Integer): HResult; stdcall;
  end;

// *********************************************************************//
// The Class CoPropagatorTestClient provides a Create and CreateRemote method to          
// create instances of the default interface IPropagatorTestClient exposed by              
// the CoClass PropagatorTestClient. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoPropagatorTestClient = class
    class function Create: IPropagatorTestClient;
    class function CreateRemote(const MachineName: string): IPropagatorTestClient;
  end;

implementation

uses ComObj;

class function CoPropagatorTestClient.Create: IPropagatorTestClient;
begin
  Result := CreateComObject(CLASS_PropagatorTestClient) as IPropagatorTestClient;
end;

class function CoPropagatorTestClient.CreateRemote(const MachineName: string): IPropagatorTestClient;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_PropagatorTestClient) as IPropagatorTestClient;
end;

end.
