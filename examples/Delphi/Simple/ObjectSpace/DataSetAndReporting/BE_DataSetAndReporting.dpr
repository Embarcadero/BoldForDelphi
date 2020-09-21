program BE_DataSetAndReporting;

{%File 'DataSetExampleClasses_Interface.inc'}
{%File 'DataSetExampleClasses.inc'}

uses
  Forms,
  Windows,
  fStart in 'fStart.pas' {frmStart},
  dMain in 'dMain.pas' {dmMain: TDataModule},
  fMain in 'fMain.pas' {frmMain},
  FfrmReport in 'FfrmReport.pas' {frmReport},
  DataSetExampleClasses in 'DataSetExampleClasses.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TdmMain, dmMain);
  with TfrmStart.Create(application) do
  try
    if ShowModal = idOK then
    begin
      Application.CreateForm(TfrmMain, frmMain);
      Application.Run;
    end;
  finally
    Free;
  end;
end.
