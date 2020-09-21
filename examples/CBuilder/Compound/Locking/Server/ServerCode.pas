unit ServerCode;

interface

implementation

uses
  ComServ, BoldComServer;

const
  LockingDemoPSrv_CLSID: TGUID = '{8BC8A32E-260A-41EE-A05A-9DB9F62F4B2E}';

initialization
  TBoldComServerConnectionFactory.Create(ComServer,
    LockingDemoPSrv_CLSID, 'LockingDemoPSrv', 'Bold Locking Demo Persistence Server');

end.
 