program LockManagerAdminClient;

uses
  Forms,
  MainForm in 'MainForm.pas' {FMain},
  dmLockManagerAdmin in 'dmLockManagerAdmin.pas' {dmMain: TDataModule};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFMain, FMain);
  Application.CreateForm(TdmMain, dmMain);
  Application.Run;
end.
