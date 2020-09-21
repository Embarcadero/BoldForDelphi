program Bldown;

{%File 'Building.inc'}
{%File 'Person.inc'}
{%File 'ResidentialBuilding.inc'}
{%File 'BuildingClasses_Interface.inc'}

uses
  Forms,
  datamod in 'Datamod.pas' {DataModule1: TDataModule},
  mainform in 'Mainform.pas' {allform},
  PersonAutoFormUnit in 'PersonAutoFormUnit.pas' {PersonAutoForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TDataModule1, DataModule1);
  Application.CreateForm(Tallform, allform);
  Application.Run;
end.
