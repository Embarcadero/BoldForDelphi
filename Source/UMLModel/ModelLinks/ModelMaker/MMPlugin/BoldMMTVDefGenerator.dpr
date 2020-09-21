program BoldMMTVDefGenerator;

uses
  Forms,
  BoldMMTVDefGenGUI in 'BoldMMTVDefGenGUI.pas' {Form1},
  BoldMMTVDefGen in 'BoldMMTVDefGen.pas',
  BoldMMTVDefs in 'BoldMMTVDefs.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
