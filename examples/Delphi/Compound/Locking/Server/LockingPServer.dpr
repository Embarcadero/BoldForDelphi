program LockingPServer;

uses
  Forms,
  ServerForm in 'ServerForm.pas' {frmServer},
  ModelDM in '..\Common\ModelDM.pas' {dmModel: TDataModule},
  PServerDM in 'PServerDM.pas' {dmPServer: TDataModule},
  ServerCode in 'ServerCode.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmServer, frmServer);
  Application.CreateForm(TdmModel, dmModel);
  Application.CreateForm(TdmPServer, dmPServer);
  Application.Run;
end.
