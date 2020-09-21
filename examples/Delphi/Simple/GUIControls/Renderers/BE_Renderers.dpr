program BE_Renderers;

{%File 'RendererExampleClasses_Interface.inc'}
{%File 'RendererExampleClasses.inc'}

uses
  Forms,
  fMain in 'fMain.pas' {Form1},
  RendererExampleClasses in 'RendererExampleClasses.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
