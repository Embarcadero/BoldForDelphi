program BoldUpgrader;

uses
  Forms,
  MainForm in 'MainForm.pas' {Form1},
  Bold1To2Upgrader in 'Bold1To2Upgrader.pas',
  DirectoryTraverser in 'DirectoryTraverser.pas',
  FileReplacer in 'FileReplacer.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
