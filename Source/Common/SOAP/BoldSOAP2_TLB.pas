unit BoldSOAP2_TLB;

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
// File generated on 4/20/2001 2:02:34 PM from Type Library described below.

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
// Type Lib: C:\Work\BfD\Source\Common\SOAP\BoldSOAP2.tlb (1)
// IID\LCID: {430A7A46-55F8-49B7-82A2-539FA3580ABE}\0
// Helpfile: 
// DepndLst: 
//   (1) v2.0 stdole, (C:\WINNT\System32\stdole2.tlb)
//   (2) v4.0 StdVCL, (C:\WINNT\System32\STDVCL40.DLL)
//   (3) v1.0 BoldSOAP, (C:\Work\BfD\Source\Common\SOAP\BoldSOAP.tlb)
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
interface

uses Windows, ActiveX, Classes, Graphics, OleServer, OleCtrls, StdVCL, 
  BoldSOAP_TLB;

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  BoldSOAP2MajorVersion = 1;
  BoldSOAP2MinorVersion = 0;

  LIBID_BoldSOAP2: TGUID = '{430A7A46-55F8-49B7-82A2-539FA3580ABE}';

  IID_IBoldSOAPService2: TGUID = '{D349D000-EDDB-4F8C-886F-CB00F640DB6A}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IBoldSOAPService2 = interface;
  IBoldSOAPService2Disp = dispinterface;

// *********************************************************************//
// Interface: IBoldSOAPService2
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D349D000-EDDB-4F8C-886F-CB00F640DB6A}
// *********************************************************************//
  IBoldSOAPService2 = interface(IBoldSOAPService)
    ['{D349D000-EDDB-4F8C-886F-CB00F640DB6A}']
    function  Get2(const request: WideString): WideString; safecall;
  end;

// *********************************************************************//
// DispIntf:  IBoldSOAPService2Disp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D349D000-EDDB-4F8C-886F-CB00F640DB6A}
// *********************************************************************//
  IBoldSOAPService2Disp = dispinterface
    ['{D349D000-EDDB-4F8C-886F-CB00F640DB6A}']
    function  Get2(const request: WideString): WideString; dispid 2;
    procedure Get(const request: WideString; out reply: WideString); dispid 1;
  end;

implementation

uses ComObj;

end.
