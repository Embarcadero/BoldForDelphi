program XMLDispatcher;

{%File 'BudgetClasses_Interface.inc'}
{%File 'BudgetClasses.inc'}

uses
  Forms,
  BoldAppDataModUnit in 'BoldAppDataModUnit.pas' {dmMain: TDataModule},
  BoldAppMain in 'BoldAppMain.pas' {MainForm},
  dmXMLProducerForBudget in 'dmXMLProducerForBudget.pas' {dmXmlProducer: TDataModule},
  ServerCode in 'ServerCode.pas',
  BudgetClasses in 'BudgetClasses.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TdmMain, dmMain);
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TdmXmlProducer, dmXmlProducer);
  Application.Run;
end.
