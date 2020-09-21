program Server;



{%File 'Core\ResidentialBuilding.inc'}
{%File 'Core\Person.inc'}
{%File 'Core\Building.inc'}
{%File 'Core\BuildingsAndOwners_Interface.inc'}

uses
  Forms,
  ServerCode in 'Server\ServerCode.pas',
  FServerConsole in 'Server\FServerConsole.pas' {frmServerConsole},
  DMServer in 'Server\DMServer.pas' {DataModule3: TDataModule},
  BuildingsAndOwners_Adapters in 'Core\BuildingsAndOwners_Adapters.pas',
  BuildingsAndOwners in 'Core\BuildingsAndOwners.pas',
  DMCore in 'Core\DMCore.pas' {DMSystem: TDataModule},
  BuildingsAndOwners_TLB in 'Core\BuildingsAndOwners_TLB.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TDMSystem, DMSystem);
  Application.CreateForm(TDataModule3, DataModule3);
  Application.CreateForm(TfrmServerConsole, frmServerConsole);
  Application.Run;
end.
