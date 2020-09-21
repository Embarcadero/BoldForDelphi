library BoldHTTPServer;

uses
  WebBroker,
  ISAPIApp,
  MainUnit in 'HTTPServerDLL\MainUnit.pas' {WebModule1: TWebModule},
  dmCoreUnit in 'Common\dmCoreUnit.pas' {dmCore: TDataModule},
  dmPersistenceUnit in 'Common\dmPersistenceUnit.pas' {dmPersistence: TDataModule};

{$R *.RES}

exports
  GetExtensionVersion,
  HttpExtensionProc,
  TerminateExtension;

begin
  Application.Initialize;
  Application.CreateForm(TWebModule1, WebModule1);
  Application.Run;
end.
