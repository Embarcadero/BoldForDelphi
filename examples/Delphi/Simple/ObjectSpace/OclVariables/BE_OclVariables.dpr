program BE_OclVariables;

{%File 'OclVariableClasses_Interface.inc'}

uses
  Forms,
  MainForm in 'MainForm.pas' {Form1},
  OclVariableClasses in 'OclVariableClasses.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
