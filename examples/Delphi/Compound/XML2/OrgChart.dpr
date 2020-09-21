program OrgChart;

{%File '..\..\..\..\SOURCE\Common\Include\Bold.inc'}
{%File 'OrgChartClasses.inc'}

uses
  Forms,
  MainForm in 'MainForm.pas' {frmMain},
  dmServer in 'dmServer.pas' {ServerDataModule: TDataModule},
  ServerCode in 'ServerCode.pas',
  OrgChartClasses in 'OrgChartClasses.pas',
  MainDataModule in 'MainDataModule.pas' {dmMain: TDataModule},
  BoldXMLExportInterfaces in 'XMLExporter\BoldXMLExportInterfaces.pas',
  BoldXMLExporter in 'XMLExporter\BoldXMLExporter.pas',
  BoldXMLExportAdapters in 'XMLExporter\BoldXMLExportAdapters.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TdmMain, dmMain);
  Application.CreateForm(TServerDataModule, ServerDataModule);
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
