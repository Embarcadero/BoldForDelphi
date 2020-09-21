program ModelEv_v2;

uses
  Forms,
  controls,
  dSystem in '..\Common\dSystem.pas' {dmSystem: TDataModule},
  dSystemTypeInfo in '..\Common\dSystemTypeInfo.pas' {dmSystemTypeInfo: TDataModule},
  dPersistence in 'dPersistence.pas' {dmPersistence: TDataModule},
  dModel in 'dModel.pas' {dmModel: TDataModule},
  ModelEvClasses in 'ModelEvClasses.pas',
  fStart in 'fStart.pas' {frmStart},
  fMain in 'fMain.pas' {frmMain};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TdmSystem, dmSystem);
  Application.CreateForm(TdmSystemTypeInfo, dmSystemTypeInfo);
  Application.CreateForm(TdmPersistence, dmPersistence);
  Application.CreateForm(TdmModel, dmModel);
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
  Application.Run;
end.
