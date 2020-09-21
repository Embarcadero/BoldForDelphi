program conway;

{%File 'ConwayClasses.inc'}
{%File 'ConwayClasses_Interface.inc'}

uses
  Forms,
  fMain in 'fMain.pas' {frmMain},
  ConwayClasses in 'ConwayClasses.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
