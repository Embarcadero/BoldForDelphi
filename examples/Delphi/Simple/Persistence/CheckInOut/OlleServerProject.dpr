program OlleServerProject;

uses
  Forms,
  ServerMainForm in 'ServerMainForm.pas' {Form1},
  MainDM in 'MainDM.pas' {DataModule1: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TDataModule1, DataModule1);
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
