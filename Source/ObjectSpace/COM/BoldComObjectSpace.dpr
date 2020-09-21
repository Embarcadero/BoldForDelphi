library BoldComObjectSpace;

uses
  ComServ;

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

