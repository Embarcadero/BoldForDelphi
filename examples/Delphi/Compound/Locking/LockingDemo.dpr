program LockingDemo;

uses
  Forms,
  MainForm in 'MainForm.pas' {Form1},
  MainDM in 'MainDM.pas' {dmMain: TDataModule},
  ModelDM in 'Common\ModelDM.pas' {dmModel: TDataModule};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TdmModel, dmModel);
  Application.CreateForm(TdmMain, dmMain);
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
