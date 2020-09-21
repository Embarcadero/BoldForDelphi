program BE_ConstraintsDemo;

{%File 'ConstraintExampleClasses_Interface.inc'}
{%File 'ConstraintExampleClasses.inc'}

uses
  Forms,
  fConstraintsMain in 'fConstraintsMain.pas' {Form1},
  ConstraintExampleClasses in 'ConstraintExampleClasses.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
