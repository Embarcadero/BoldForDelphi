program BE_CustomAttributes;

{%File 'CustAttrExampleClasses_Interface.inc'}
{%File 'CustAttrExampleClasses.inc'}

uses
  Forms,
  MainForm in 'MainForm.pas' {Form1},
  CustAttrExampleClasses in 'CustAttrExampleClasses.pas',
  BABudgetStatus in 'BABudgetStatus.pas',
  BACoordinate in 'BACoordinate.pas',
  BACoordinateInterface in 'BACoordinateInterface.pas',
  BACoordinatePMapper in 'BACoordinatePMapper.pas',
  BAName in 'BAName.pas',
  BAShortString in 'BAShortString.pas',
  BASwedishSocSec in 'BASwedishSocSec.pas',
  BoldPMString25 in 'BoldPMString25.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
