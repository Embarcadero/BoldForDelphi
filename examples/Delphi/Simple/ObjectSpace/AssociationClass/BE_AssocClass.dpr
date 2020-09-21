program BE_AssocClass;

{%File 'AssociationClassExampleClasses.inc'}
{%File 'AssociationClassExampleClasses_Interface.inc'}

uses
  Forms,
  MainForm in 'MainForm.pas' {frmMain},
  AssociationClassExampleClasses in 'AssociationClassExampleClasses.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
