program ProdStruct;

{%File 'ProdStructClasses_Interface.inc'}
{%File 'ProdStructClasses.inc'}

uses
  Forms,
  dm1 in 'dm1.pas' {dmMain: TDataModule},
  main in 'main.pas' {frmMain},
  ProdStructClasses in 'ProdStructClasses.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TdmMain, dmMain);
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
