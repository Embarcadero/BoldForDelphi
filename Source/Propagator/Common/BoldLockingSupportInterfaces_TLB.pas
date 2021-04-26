
{ Global compiler directives }
{$include bold.inc}
unit BoldLockingSupportInterfaces_TLB;





























{$TYPEDADDRESS OFF}
interface

uses Windows, ActiveX, Classes, Graphics, OleServer, OleCtrls, StdVCL;






const
  BoldLockingSupportInterfacesMajorVersion = 1;
  BoldLockingSupportInterfacesMinorVersion = 0;

  LIBID_BoldLockingSupportInterfaces: TGUID = '{0EE38CD0-5848-4A2F-96E6-BFE2007AC6BD}';

  IID_IBoldLockManager: TGUID = '{105D857F-DD36-48F8-9554-ABCC212053ED}';
  IID_IBoldLockManagerAdmin: TGUID = '{89074E8A-9A98-4D2A-A113-65F495611C6C}';
type


  IBoldLockManager = interface;
  IBoldLockManagerAdmin = interface;




  IBoldLockManager = interface(IUnknown)
    ['{105D857F-DD36-48F8-9554-ABCC212053ED}']
    function  GetLocks(ClientId: Integer; TimeOut: Integer; RequestedExclusiveLocks: OleVariant; 
                       RequestedSharedLocks: OleVariant; out HeldLocks: OleVariant; 
                       out ClientsHoldingRequestedLocks: OleVariant): WordBool; safecall;
    function  ReleaseLocks(ClientId: Integer; Locks: OleVariant): HResult; safecall;
    function  EnsureLocks(ClientId: Integer; RequestedExclusiveLocks: OleVariant; 
                          RequestedSharedLocks: OleVariant): WordBool; safecall;
  end;




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
