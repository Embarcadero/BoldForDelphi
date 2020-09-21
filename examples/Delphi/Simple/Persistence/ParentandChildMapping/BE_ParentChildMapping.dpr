program BE_ParentChildMapping;

{%File 'ParentChildMappingClasses_Interface.inc'}
{%File 'ParentChildMappingClasses.inc'}

uses
  Forms,
  MainForm in 'MainForm.pas' {Form1},
  ParentChildMappingClasses in 'ParentChildMappingClasses.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
