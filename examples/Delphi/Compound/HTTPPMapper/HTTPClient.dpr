program HTTPClient;

{%File 'Common\Person.inc'}
{%File 'Common\BuildingClasses_Interface.inc'}
{%File 'Common\Building.inc'}
{%File 'Common\ResidentialBuilding.inc'}

uses
  Forms,
  datamod in 'client\datamod.pas' {DataModule1: TDataModule},
  mainform in 'client\mainform.pas' {allform},
  dmCoreUnit in 'Common\dmCoreUnit.pas' {dmCore: TDataModule},
  BuildingClasses in 'Common\BuildingClasses.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TdmCore, dmCore);
  Application.CreateForm(TDataModule1, DataModule1);
  Application.CreateForm(Tallform, allform);
  Application.Run;
end.
