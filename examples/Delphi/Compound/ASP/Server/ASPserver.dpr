library ASPserver;

uses
  WebBroker,
  ISAPIApp,
  MainWebModule in 'MainWebModule.pas' {wmASPServer: TWebModule},
  BuildingClasses in '..\Common\BuildingClasses.pas';

{$R *.RES}

exports
  GetExtensionVersion,
  HttpExtensionProc,
  TerminateExtension;

begin
  Application.Initialize;
  Application.CreateForm(TwmASPServer, wmASPServer);
  Application.Run;
end.
