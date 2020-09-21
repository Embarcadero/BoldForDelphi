program BE_ReverseDerivedAttribDemo;

{%File 'ReverseDeriveExampleClasses_Interface.inc'}
{%File 'ReverseDeriveExampleClasses.inc'}

uses
  Forms,
  fRDExamMain in 'fRDExamMain.pas' {Form1},
  ReverseDeriveExampleClasses in 'ReverseDeriveExampleClasses.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
