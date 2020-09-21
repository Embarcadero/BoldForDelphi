program BldOwnSynchronized;

{%File 'ResidentialBuilding.inc'}
{%File 'Building.inc'}
{%File 'Person.inc'}
{%File 'BldOwnClasses_Interface.inc'}

uses
  Forms,
  datamod in 'datamod.pas' {DataModule1: TDataModule},
  mainform in 'mainform.pas' {allform},
  BldOwnClasses in 'BldOwnClasses.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TDataModule1, DataModule1);
  Application.CreateForm(Tallform, allform);
  Application.Run;
end.
