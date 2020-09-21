unit ServerCode;

interface

implementation

uses
  ComServ, BoldComServer;

const
  XMLDispatcher_CLSID: TGUID = '{DCDA95FC-5719-4A65-B04E-89DE2DCC6DE5}';

initialization
  TBoldComServerConnectionFactory.Create(ComServer,
    XMLDispatcher_CLSID,'XMLDispatcher','XMLDispatcher Application');

end.
