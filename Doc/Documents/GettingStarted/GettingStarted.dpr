program GettingStarted;

uses
  Forms,
  MainForm in 'MainForm.pas' {Form1},
  DataMod in 'DataMod.pas' {DataModule2: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TDataModule2, DataModule2);
  Application.Run;
end.
