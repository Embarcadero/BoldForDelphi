unit BoldSOAP_TLB;

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
// Type Lib: C:\Work\BfD\Source\Common\SOAP\BoldSOAP.tlb (1)
// IID\LCID: {9BF07220-6C8A-11D4-BBAC-0010A4F9E114}\0
// Helpfile: 
// DepndLst: 
//   (1) v2.0 stdole, (C:\WINNT\System32\stdole2.tlb)
// Parent TypeLibrary:
//   (0) v1.0 BoldSOAP2, (C:\Work\BfD\Source\Common\SOAP\BoldSOAP2.tlb)
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
  BoldSOAPMajorVersion = 1;
  BoldSOAPMinorVersion = 0;

  LIBID_BoldSOAP: TGUID = '{9BF07220-6C8A-11D4-BBAC-0010A4F9E114}';

  IID_IBoldSOAPService: TGUID = '{9BF07221-6C8A-11D4-BBAC-0010A4F9E114}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IBoldSOAPService = interface;
  IBoldSOAPServiceDisp = dispinterface;

// *********************************************************************//
// Interface: IBoldSOAPService
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {9BF07221-6C8A-11D4-BBAC-0010A4F9E114}
// *********************************************************************//
  IBoldSOAPService = interface(IDispatch)
    ['{9BF07221-6C8A-11D4-BBAC-0010A4F9E114}']
    procedure Get(const request: WideString; out reply: WideString); safecall;
  end;

// *********************************************************************//
// DispIntf:  IBoldSOAPServiceDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {9BF07221-6C8A-11D4-BBAC-0010A4F9E114}
// *********************************************************************//
  IBoldSOAPServiceDisp = dispinterface
    ['{9BF07221-6C8A-11D4-BBAC-0010A4F9E114}']
    procedure Get(const request: WideString; out reply: WideString); dispid 1;
  end;

implementation

uses ComObj;

end.
