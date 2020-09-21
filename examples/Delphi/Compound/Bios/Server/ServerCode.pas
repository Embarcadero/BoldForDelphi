unit ServerCode;

interface

implementation

uses
  ComServ, BoldComServer;

const
  BoldServer_CLSID: TGUID = '{D7724C3F-9082-460F-9321-CB47C232CA29}';

initialization
  TBoldComServerConnectionFactory.Create(ComServer,
    BoldServer_CLSID,'BoldServer','Bold Server Application');

end.
