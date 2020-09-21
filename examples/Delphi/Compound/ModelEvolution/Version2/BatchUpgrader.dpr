program BatchUpgrader;

uses
  Forms,
  fBatchUpgraderGui in '..\Common\fBatchUpgraderGui.pas' {Form1},
  dPersistence in 'dPersistence.pas' {dmPersistence: TDataModule},
  dModel in 'dModel.pas' {dmModel: TDataModule},
  dSystemTypeInfo in '..\Common\dSystemTypeInfo.pas' {dmSystemTypeInfo: TDataModule},
  ModelEvClasses in 'ModelEvClasses.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TdmModel, dmModel);
  Application.CreateForm(TdmSystemTypeInfo, dmSystemTypeInfo);
  Application.CreateForm(TdmPersistence, dmPersistence);
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
