unit aniv_XMLtestserver;

interface

implementation

uses
  ComServ, BoldComServer;

const
  BoldXMLTestServer_CLSID: TGUID = '{EAD191C3-7A8C-11D4-BBAC-0010A4F9E114}';

initialization
  TBoldComServerConnectionFactory.Create(ComServer,
    BoldXMLTestServer_CLSID,'BoldXMLTestServer','Bold test of XML streaming');

end.
