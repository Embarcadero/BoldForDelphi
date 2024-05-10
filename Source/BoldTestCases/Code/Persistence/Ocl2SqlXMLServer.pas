unit Ocl2SqlXMLServer;

interface

implementation

uses
  ComServ, BoldComServer;

const
  Ocl2SqlXMLtestServer_CLSID: TGUID = '{753777E3-87CA-11D4-BBAC-0010A4F9E114}';

initialization
  TBoldComServerConnectionFactory.Create(ComServer,
    Ocl2SqlXMLtestServer_CLSID,'Ocl2SqlXMLtestServer','Bold Server Application');

end.
