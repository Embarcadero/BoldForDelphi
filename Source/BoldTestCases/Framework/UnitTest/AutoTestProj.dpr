program AutoTestProj;
{-$IFDEF AUTO}
{-$APPTYPE CONSOLE}
{-$ENDIF}

uses                                
  Forms,
  Dialogs,
  SysUtils,
  Classes,
  {$INCLUDE AllTestCases.inc}
  TestSuite,  // Added to make things compile again
  MainView;  // Added to make things compile again

{$R *.RES}

type
  TAutoTestHandler = class
  private
    class procedure LogTest(TestCase: TBoldTestCase);
  end;

var
//  CommandLineParser: TCommandLineParser;
  Log: TStringList;

{ TAutoTestHandler }

class procedure TAutoTestHandler.LogTest(TestCase: TBoldTestCase);
var
  s: string;
begin
  with TestCase do
  begin
    s := TestGlobal.ErrorLine(ClassName,
                              Name,
                              ErrorMsg[TestResult],
                              testMessage,
                              IntToStr(ElapsedTestTime),
                              Comment);
    Log.Add(s);
    {$IFDEF AUTO}
    Writeln(Name + ' + ' + ErrorMsg[TestResult]);
    {$ENDIF}
  end;
end;

//var
//  i: integer;

begin
{
  CommandLineParser := TCommandLineParser.Create(0, 2);
  CommandLineParser.RegisterOption('FO', True);
  CommandLineParser.RegisterOption('AUTO', False);
  CommandLineParser.Parse;
}
{  if CommandLineParser.Options['AUTO'] then
  begin
    Log := TStringList.Create;
    for i := 1 to TestGlobal.TestCaseList.Count - 1 do
    begin
      Write(Format('Testing %d: %s...', [i, TestGlobal.TestCaseList[i]]));
      TestGlobal.RunTests(TestGlobal.TestCaseList[i], TAutoTestHandler.LogTest);
    end;
    if CommandLineParser.OptionValues['FO'] <> '' then
      Log.SaveToFile(CommandLineParser.OptionValues['FO']);
    Log.Free;
  end
  else   }
  begin
    Application.Initialize;
    Application.CreateForm(TMainViewForm, MainViewForm);
    MainViewForm.AutoActivate := CommandLineParser.Options['AUTO'];
    Application.Run;
  end;
  FreeAndNil(CommandLineParser);
end.
