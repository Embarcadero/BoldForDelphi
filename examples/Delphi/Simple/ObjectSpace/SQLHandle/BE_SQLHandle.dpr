program BE_SQLHandle;

{%File 'SQLHandleClasses_Interface.inc'}

uses
  Forms,
  Mainform in 'Mainform.pas' {Form1},
  SQLHandleClasses in 'SQLHandleClasses.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
