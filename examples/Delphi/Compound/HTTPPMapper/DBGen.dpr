program DBGen;

uses
  Forms,
  dmCoreUnit in 'Common\dmCoreUnit.pas' {dmCore: TDataModule},
  dmPersistenceUnit in 'Common\dmPersistenceUnit.pas' {dmPersistence: TDataModule},
  dbGenForm in 'DBGenerator\dbGenForm.pas' {frmMainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TdmCore, dmCore);
  Application.CreateForm(TdmPersistence, dmPersistence);
  Application.CreateForm(TfrmMainForm, frmMainForm);
  Application.Run;
end.
