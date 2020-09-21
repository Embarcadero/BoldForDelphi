program BoldClientifier;

uses
  Forms,
  MainForm in 'MainForm.pas' {Form1},
  FileReplacer in 'FileReplacer.pas',
  DirectoryTraverser in 'DirectoryTraverser.pas',
  Clientifier in 'Clientifier.pas',
  BoldUMLModelDataModule in '..\..\Source\UMLModel\Core\BoldUMLModelDataModule.pas' {dmModelEdit: TDataModule};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TdmModelEdit, dmModelEdit);
  Application.Run;
end.
