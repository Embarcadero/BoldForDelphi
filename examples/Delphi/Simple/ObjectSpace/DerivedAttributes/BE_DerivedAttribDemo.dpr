program BE_DerivedAttribDemo;

{%File 'DerivedAttrExampleClasses_Interface.inc'}

uses
  Forms,
  fDerivedAttrMain in 'fDerivedAttrMain.pas' {Form1},
  DerivedAttrExampleClasses in 'DerivedAttrExampleClasses.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
