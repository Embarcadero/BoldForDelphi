program BoldProject;

uses
  Forms,
  Windows,
  fStart in 'fStart.pas' {frmStart},
  dMain in 'dMain.pas' {dmMain: TDataModule},
  fMain in 'fMain.pas' {frmMain};

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
