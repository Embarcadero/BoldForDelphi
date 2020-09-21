program DBGenerator;

uses
  Forms,
  DBGeneratorForm in 'DBGeneratorForm.pas' {frmDBGen},
  ModelDM in '..\Common\ModelDM.pas' {dmModel: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TdmModel, dmModel);
  Application.CreateForm(TfrmDBGen, frmDBGen);
  Application.Run;
end.
