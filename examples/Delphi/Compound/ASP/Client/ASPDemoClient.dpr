program ASPDemoClient;

uses
  Forms,
  ClientMainForm in 'ClientMainForm.pas' {Form1},
  ClientDM in 'ClientDM.pas' {dmClient: TDataModule},
  ModelDM in '..\Common\ModelDM.pas' {dmModel: TDataModule};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TdmModel, dmModel);
  Application.CreateForm(TdmClient, dmClient);
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
