program Client;

uses
  Forms,
  Renderers in 'ClientGui\Renderers.pas' {DataModule2: TDataModule},
  BuildingsAndOwners_TLB in 'Core\BuildingsAndOwners_TLB.pas',
  Mainform in 'ClientGui\Mainform.pas' {allform},
  DMClient in 'Client\DMClient.pas' {DMClientSystem: TDataModule},
  BoldComponentValidatorCom in '..\..\..\Source\ObjectSpace\COM\BoldComponentValidatorCom.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TDMClientSystem, DMClientSystem);
  Application.CreateForm(TDataModule2, DataModule2);
  Application.CreateForm(Tallform, allform);
  Application.Run;
end.
