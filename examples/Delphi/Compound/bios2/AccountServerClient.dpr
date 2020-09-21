program AccountServerClient;

uses
  Forms,
  dmSystem in 'client\dmSystem.pas' {DM: TDataModule},
  FMain in 'client\FMain.pas' {Form2},
  AccountClasses_TLB in 'Core\AccountClasses_TLB.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TDM, DM);
  Application.Run;
end.
