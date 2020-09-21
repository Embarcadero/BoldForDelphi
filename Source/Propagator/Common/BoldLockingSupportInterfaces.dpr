library BoldLockingSupportInterfaces;

uses
  comserv;

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.RES}
{$R *.TLB}

begin
  ComServer.LoadTypeLib;
end. 
