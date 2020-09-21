program BE_OptimisticLocking;

{%File 'OptimisticLockingExampleClasses_Interface.inc'}

uses
  Forms,
  MainForm in 'MainForm.pas' {Form1},
  OptimisticLockingExampleClasses in 'OptimisticLockingExampleClasses.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
