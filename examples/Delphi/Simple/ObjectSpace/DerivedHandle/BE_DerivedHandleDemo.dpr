program BE_DerivedHandleDemo;

{%File 'DerivedHandleExampleClasses_Interface.inc'}

uses
  Forms,
  Mainform in 'Mainform.pas' {Form1},
  DerivedHandleExampleClasses in 'DerivedHandleExampleClasses.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
