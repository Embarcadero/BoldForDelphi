unit joho_ClientGuiAndHandles_ServerCode;

interface

implementation

uses
  ComServ, BoldComServer;

const
  BoldServer_CLSID: TGUID = '{89891D39-7B94-4ED7-AC7C-8CD6D356E426}';

initialization
  TBoldComServerConnectionFactory.Create(ComServer,
    BoldServer_CLSID,'BoldServer','Bold Server Application');

end.
