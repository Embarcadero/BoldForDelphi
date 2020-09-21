program QueryDemoProject;

{%File 'QueryDemoClasses_Interface.inc'}
{%File 'QueryDemoClasses.inc'}

uses
  Forms,
  QueryDemoForm in 'QueryDemoForm.pas' {frmQueryDemo},
  QueryDemoClasses in 'QueryDemoClasses.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmQueryDemo, frmQueryDemo);
  Application.Run;
end.
