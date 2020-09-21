program BE_BoldImage;

{%File 'ImageDemoClasses_Interface.inc'}

uses
  Forms,
  fMain in 'fMain.pas' {frmMain},
  fViewStretch in 'fViewStretch.pas' {frmStretch},
  fViewClipboardFmt in 'fViewClipboardFmt.pas' {frmViewClipboardFmt},
  fViewAutoSize in 'fViewAutoSize.pas' {frmImageViewer},
  ImageDemoClasses in 'ImageDemoClasses.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Bold Image Viewer Demo';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
