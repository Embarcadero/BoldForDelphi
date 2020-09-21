program BE_DerivedRelations;

{%File 'DerivedHandleExampleClasses_Interface.inc'}
{%File 'DerivedHandleExampleClasses.inc'}

uses
  Forms,
  fDerivedRelationsMain in 'fDerivedRelationsMain.pas' {Form1},
  DerivedHandleExampleClasses in 'DerivedHandleExampleClasses.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
