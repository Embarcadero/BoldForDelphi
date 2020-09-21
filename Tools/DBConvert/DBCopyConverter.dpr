program DBCopyConverter;

uses
  Forms,
  mainform in 'mainform.pas' {frmMain},
  DBConverter in 'DBConverter.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
