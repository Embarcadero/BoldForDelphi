program BE_MultiLang;

{%File 'MultilangClasses_Interface.inc'}
{%File 'Building.inc'}
{%File 'Person.inc'}

uses
  Forms,
  MainForm in 'MainForm.pas' {Form1},
  MultilangClasses in 'MultilangClasses.pas',
  MLTestVSAttributes in 'MLTestVSAttributes.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
