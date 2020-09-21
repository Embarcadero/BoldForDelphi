unit ServerCode;

interface

implementation

uses
  ComServ, BoldComServer;

const
  BoldServer_CLSID: TGUID = '{5BC282A9-970A-4DE1-8644-2EB4465000FC}';

initialization
  TBoldComServerConnectionFactory.Create(ComServer,
    BoldServer_CLSID,'BoldServer','COM Account Server');

end.
