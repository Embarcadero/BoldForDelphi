program BE_ComboBox;

{%File 'ComboBoxClasses_Interface.inc'}
{%File 'ComboBoxClasses.inc'}

uses
  Forms,
  fMain in 'fMain.pas' {Form1},
  ComboBoxClasses in 'ComboBoxClasses.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
