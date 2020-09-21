unit ServerCode;

interface

implementation

uses
  ComServ, BoldComServer;

const
  OrgChart_CLSID: TGUID = '{AE605A6F-9C63-4CA4-90FC-B4D75614D4D7}';

initialization
  TBoldComServerConnectionFactory.Create(ComServer, 
    OrgChart_CLSID, 'OrgChart', 'OrgChart Server Application');

end.
