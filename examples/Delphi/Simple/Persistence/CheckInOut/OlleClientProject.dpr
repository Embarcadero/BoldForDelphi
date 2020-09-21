program OlleClientProject;

uses
  Forms,
  ClientMainForm in 'ClientMainForm.pas' {Form2},
  MainDM in 'MainDM.pas' {DataModule1: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TDataModule1, DataModule1);
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
