program ModelEv_v1;

uses
  Forms,
  controls,
  Windows,
  dSystem in '..\Common\dSystem.pas' {dmSystem: TDataModule},
  fMain in 'fMain.pas' {frmMain},
  dSystemTypeInfo in '..\Common\dSystemTypeInfo.pas' {dmSystemTypeInfo: TDataModule},
  ModelEvClasses in 'ModelEvClasses.pas',
  dModel in 'dModel.pas' {dmModel: TDataModule},
  dPersistence in 'dPersistence.pas' {dmPersistence: TDataModule},
  fStart in 'fStart.pas' {frmStart};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TdmModel, dmModel);
  Application.CreateForm(TdmSystemTypeInfo, dmSystemTypeInfo);
  Application.CreateForm(TdmPersistence, dmPersistence);
  Application.CreateForm(TdmSystem, dmSystem);
  with TfrmStart.Create(application) do
  try
    if ShowModal = mrOK then
    begin
      Application.CreateForm(TfrmMain, frmMain);
      Application.Run;
    end;
  finally
    Free;
  end;
end.
