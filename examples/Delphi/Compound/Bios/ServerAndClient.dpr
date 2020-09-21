program ServerAndClient;

uses
  Forms,
  Renderers in 'ClientGui\Renderers.pas' {DataModule2: TDataModule},
  DMCore in 'Core\DMCore.pas' {DMSystem: TDataModule},
  BuildingsAndOwners_Adapters in 'Core\BuildingsAndOwners_Adapters.pas',
  BuildingsAndOwners_TLB in 'Core\BuildingsAndOwners_TLB.pas',
  BuildingsAndOwners in 'Core\BuildingsAndOwners.pas',
  ServerCode in 'Server\ServerCode.pas',
  FServerConsole in 'Server\FServerConsole.pas' {frmServerConsole},
  DMServer in 'Server\DMServer.pas' {DataModule3: TDataModule},
  mainform in 'ClientGui\Mainform.pas' {allform},
  DMClient in 'Client\DMClient.pas' {DMClientSystem: TDataModule};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TDMSystem, DMSystem);
  Application.CreateForm(TDataModule2, DataModule2);
  Application.CreateForm(TDataModule3, DataModule3);
  Application.CreateForm(TfrmServerConsole, frmServerConsole);
  Application.CreateForm(Tallform, allform);
  Application.CreateForm(TDMClientSystem, DMClientSystem);
  Application.Run;
end.
