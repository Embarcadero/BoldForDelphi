program DocVersions;

{%File 'DocumentClasses_Interface.inc'}
{%File 'DocumentClasses.inc'}

uses
  Forms,
  MainForm in 'MainForm.pas' {frmMain},
  MainDM in 'MainDM.pas' {dmMain: TDataModule},
  DocumentClasses in 'DocumentClasses.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TdmMain, dmMain);
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
