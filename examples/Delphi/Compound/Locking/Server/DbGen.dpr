program DbGen;

uses
  Forms,
  DbGenForm in 'DbGenForm.pas' {frmDBGen},
  ModelDM in '..\Common\ModelDM.pas' {dmModel: TDataModule};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TdmModel, dmModel);
  Application.CreateForm(TfrmDBGen, frmDBGen);
  Application.Run;
end.
