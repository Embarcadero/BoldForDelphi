program AccountServer;

{%File 'server\AccountClasses.inc'}
{%File 'server\AccountClasses_Interface.inc'}

uses
  Forms,
  dmProduct in 'server\dmProduct.pas' {DataModule2: TDataModule},
  FServerConsoleUnit in 'server\FServerConsoleUnit.pas' {Form1},
  ServerCode in 'server\ServerCode.pas',
  AccountClasses in 'server\AccountClasses.pas',
  AccountClasses_Adapters in 'server\AccountClasses_Adapters.pas',
  AccountClasses_TLB in 'Core\AccountClasses_TLB.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TDataModule2, DataModule2);
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
