program BE_ExternalPersistence;

{%File 'BusinessClasses.inc'}
{%File 'BusinessClasses_Interface.inc'}

uses
  Forms,
  BusinessClasses in 'BusinessClasses.pas',
  BusinessClasses_PersistenceInterfaces in 'BusinessClasses_PersistenceInterfaces.pas',
  fMainForm in 'fMainForm.pas' {MainForm},
  dMain in 'dMain.pas' {dmMain: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TdmMain, dmMain);
  Application.Run;
end.
