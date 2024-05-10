program jehoTestProj;

uses
  Forms,
  TestSuite in '..\..\Framework\UnitTest\TestSuite.pas',
  MainView in '..\..\Framework\UnitTest\MainView.pas' {MainViewForm},
  jehoTestCases in 'jehoTestCases.pas',
  jehoBCBoldTest in '..\..\Code\ObjectSpace\jehoBCBoldTest.pas',
  dmjehoBoldTest in '..\..\Code\ObjectSpace\dmjehoBoldTest.pas' {jehodmBoldTest: TDataModule},
  maan_UndoRedo in '..\..\Code\ObjectSpace\maan_UndoRedo.pas',
  UndoTestModelClasses in '..\..\Code\ObjectSpace\UndoTestModelClasses.pas',
  maan_UndoRedoTestCaseUtils in '..\..\Code\ObjectSpace\maan_UndoRedoTestCaseUtils.pas',
  TestModel1 in '..\..\Code\Main\TestModel1.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainViewForm, MainViewForm);
  Application.CreateForm(TjehodmBoldTest, jehodmBoldTest);
  Application.Run;
end.
