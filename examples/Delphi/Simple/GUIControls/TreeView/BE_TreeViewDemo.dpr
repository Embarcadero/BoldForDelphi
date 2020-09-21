program BE_TreeViewDemo;

{%File 'TreeViewExampleClasses_Interface.inc'}

uses
  Forms,
  fTreeViewMain in 'fTreeViewMain.pas' {Form1},
  TreeViewExampleClasses in 'TreeViewExampleClasses.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
