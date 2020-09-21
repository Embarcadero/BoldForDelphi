unit BoldLockingSupportInterfaces_TLB;

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
// File generated on 6/1/2001 9:37:35 AM from Type Library described below.

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
// Type Lib: C:\Work\BfD\Source\Propagator\Common\BoldLockingSupportInterfaces.tlb (1)
// IID\LCID: {0EE38CD0-5848-4A2F-96E6-BFE2007AC6BD}\0
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
  BoldLockingSupportInterfacesMajorVersion = 1;
  BoldLockingSupportInterfacesMinorVersion = 0;

  LIBID_BoldLockingSupportInterfaces: TGUID = '{0EE38CD0-5848-4A2F-96E6-BFE2007AC6BD}';

  IID_IBoldLockManager: TGUID = '{105D857F-DD36-48F8-9554-ABCC212053ED}';
  IID_IBoldLockManagerAdmin: TGUID = '{89074E8A-9A98-4D2A-A113-65F495611C6C}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IBoldLockManager = interface;
  IBoldLockManagerAdmin = interface;

// *********************************************************************//
// Interface: IBoldLockManager
// Flags:     (256) OleAutomation
// GUID:      {105D857F-DD36-48F8-9554-ABCC212053ED}
// *********************************************************************//
  IBoldLockManager = interface(IUnknown)
    ['{105D857F-DD36-48F8-9554-ABCC212053ED}']
    function  GetLocks(ClientId: Integer; TimeOut: Integer; RequestedExclusiveLocks: OleVariant; 
                       RequestedSharedLocks: OleVariant; out HeldLocks: OleVariant; 
                       out ClientsHoldingRequestedLocks: OleVariant): WordBool; safecall;
    function  ReleaseLocks(ClientId: Integer; Locks: OleVariant): HResult; safecall;
    function  EnsureLocks(ClientId: Integer; RequestedExclusiveLocks: OleVariant; 
                          RequestedSharedLocks: OleVariant): WordBool; safecall;
  end;

// *********************************************************************//
// Interface: IBoldLockManagerAdmin
// Flags:     (256) OleAutomation
// GUID:      {89074E8A-9A98-4D2A-A113-65F495611C6C}
// *********************************************************************//
  IBoldLockManagerAdmin = interface(IUnknown)
    ['{89074E8A-9A98-4D2A-A113-65F495611C6C}']
    function  ListAllClients(out Clients: OleVariant): HResult; safecall;
    function  RemoveLocksForClient(ClientId: Integer; Locks: OleVariant): HResult; safecall;
    function  KillClient(ClientId: Integer): HResult; safecall;
    function  LocksForClients(ClientIds: OleVariant; out Locks: OleVariant;
                              out LockDurations: OleVariant): HResult; safecall;
    function  ListLockingClients(out Clients: OleVariant): HResult; safecall;
    function  Get_LockManagerSuspended: WordBool; safecall;
    procedure Set_LockManagerSuspended(Value: WordBool); safecall;
    property LockManagerSuspended: WordBool read Get_LockManagerSuspended write Set_LockManagerSuspended;
  end;

implementation

uses ComObj;

end.
