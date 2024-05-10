unit TestSuite;

{ Usage notes:                                                                                                     
* All test cases must be subclasses to TBoldTestCase
* The new test cases need to override Suit to register their test methods (no need to call inherited)
* The new test case may override Setup and TearDown
* The parameter to Create is the name of the method to be tested
* Run executes the method passed as parameter to Create. The method must be published to be found!
* The reason to inherit from TPErsistent is to use the class registration
  framework offered by Delphi
* Any own constructor must call the inherited Create to set the method name to be used.
}

interface

uses
  classes,
  windows,
  BoldUtils,
  TestFramework,
  sysutils;

type
  { forward declarations }
  TBoldTestCase = class;
  TBoldTestSuite = class;
  TTestGlobal = class;
  TTestList = class;
  TTestAllTestCases = class;

  { Class types }
  TBoldTestCaseClass = class of TBoldTestCase;

  { method types }
  TObjectProcedure = procedure of object;
  TNotifyFault = procedure(TestCase: TBoldTestCase) of object;

  { enumerations }
  TResultCode = (etNotFound, etAllOK, etFailure, etError);

  IBoldTestCase = interface
    ['{7CD541BA-8FDB-4B1C-9142-500A235CEDE1}']
    function GetTestResult: TResultCode;

    function GetComment: string;
    procedure SetComment(Value: string);

    function GetTestMessage: string;
    procedure SetTestMessage(Value: string);

    function GetOwningSuite: TBoldTestSuite;

    procedure Run;
    property TestResult: TResultCode read GetTestResult;
    property Comment: String read GetComment write SetComment;
    property TestMessage: string read GetTestMessage write SetTestMessage;
    property OwningSuite: TBoldTestSuite read GetOwningSuite;
  end;

  { TBoldTestCase }                              
  TBoldTestCase = class(TTestCase, IBoldTestCase)
  private
    fOwningSuite: TBoldTestSuite;
    fTestMethod: TMethod;
    fTestResult: TResultCode;
    fTestMessage: string;
    FComment: String;
    function GetTestMethodAvailable: boolean;
  protected
    procedure RegisterTestMethodByName(const aName: string);
    function Equals(TestCase: TBoldTestCase): boolean; reintroduce;
    procedure InvokeTest;
    procedure TestNotFound;
    {-------- IBoldTestCase--------}
    function GetTestResult: TResultCode; virtual;

    function GetComment: string; virtual;
    procedure SetComment(Value: string); virtual;

    function GetTestMessage: string; virtual;
    procedure SetTestMessage(Value: string); virtual;

    function GetOwningSuite: TBoldTestSuite;  virtual;
    {-------- IBoldTestCase--------}

    property TestMethodAvailable: boolean read GetTestMethodAvailable;
   public
    constructor Create(Name: string); reintroduce; virtual;
    constructor CreateWithComment(const AName: string; const aComment: string = '');
    constructor CreateByFunction(FunctionPtr: Pointer);
    class procedure Suit(ASuite: TBoldTestSuite); virtual; // register test methods
    procedure Run;
    {IBoldTestCase}
    property TestResult: TResultCode read GetTestResult;
    property Comment: String read GetComment write SetComment;
    property TestMessage: string read GetTestMessage write SetTestMessage;
    property OwningSuite: TBoldTestSuite read GetOwningSuite;
  end;

  { TTestList }
  TTestList = class(TList)
  private
    function GetTItems(Index: Integer): TBoldTestCase;
    procedure SetTItems(Index: Integer; const Value: TBoldTestCase);
  protected
    function Includes(TestCase: TBoldTestCase): boolean;
  public
    procedure Clear; override;
    procedure AddNoDupe(TestCase: TBoldTestCase);
    property TItems[Index: Integer]: TBoldTestCase read GetTItems write SetTItems; default;
  end;

  { TBoldTestSuite }
  TBoldTestSuite = class(TObject)
  private
    FTests: TTestList;
    FRuns: integer;
    FErrors: integer;
    FFailures: integer;
    FNotFound: integer;
    FOnFault: TNotifyFault;
    function GetProblems: integer;
  public
    constructor Create; overload;
    constructor Create(classNames: array of String); overload;
    destructor Destroy; override;
    procedure Run;
    procedure AssignSuit(const TestCaseClassName: String);
    procedure AddTest(Test: TBoldTestCase);
    procedure AddAllTests(TestCaseClass: TBoldTestCaseClass);
    property Runs: integer read FRuns write FRuns;
    property Errors: integer read FErrors write FErrors;
    property Failures: integer read FFailures write FFailures;
    property ProblemCount: integer read GetProblems;
    property NotFound: integer read FNotFound write FNotFound;
    property OnFault: TNotifyFault read FOnFault write FOnFault;
  end;

  { TTestGlobal }
  TTestGlobal = class(TObject)
  private
    fTestCaseList: TStringList;
    function GetTestCaseClassByName(const Name: string): TBoldTestCaseClass;
  public
    constructor Create;
    destructor Destroy; override;
    procedure RegisterTestCase(TestCaseClass: TBoldTestCaseClass);
    procedure UnregisterTestCase(TestCaseClass: TBoldTestCaseClass);
    class procedure RunTests(const TestName: string; NotifyFault: TNotifyFault);
    class function ErrorLine(const aGroup, aName, ErrorMessage, aMsg, aTime, aComment: String): string;
    class function CommaLine(const aGroup, aName, ErrorMessage, aTime, aMsg, aComment: String): string;
    property TestCaseList: TStringList read fTestCaseList;
    property TestCaseClassByName[const name: string]: TBoldTestCaseClass read GetTestCaseClassByName;
  end;

  { TTestAllTestCases }
  TTestAllTestCases = class(TBoldTestCase)
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;  // register test methods
  end;

  {utility routines}
  function TestByName(List: IInterfaceList; const TestName: string): ITest;
  procedure SetComment(const aTest: ITest; const Comment: string); overload;
  procedure SetCommentForTest(const Suite: ITestSuite; const TestName, Comment: string); overload;

const
  ErrorMsg: array[etNotFound..etError] of string =
    ('Not Found', 'OK', 'Failure', 'Error');

procedure RegisterTestCase(TestCaseClass: TBoldTestCaseClass);
procedure UnRegisterTestCase(TestCaseClass: TBoldTestCaseClass);
procedure AssertFmt(Expr: boolean; const msg: string; const Args: array of const);

function TestGlobal: TTestGlobal;

implementation

uses
//  DBTables, // for Session object
  Dialogs,
  BoldMemoryManager;

var
  G_TestGlobal: TTestGlobal = nil;

function TestGlobal: TTestGlobal;
begin
  if not Assigned(G_TestGlobal) then
    G_TestGlobal := TTestGlobal.Create;
  Result := G_TestGlobal;
end;

procedure RegisterTestCase(TestCaseClass: TBoldTestCaseClass);
begin
  ShowMessage('Depracated. Use TestGlobal.RegisterTestCase');
  TestGlobal.RegisterTestCase(TestCaseClass);
end;

procedure UnRegisterTestCase(TestCaseClass: TBoldTestCaseClass);
begin
  ShowMessage('This procedure call has been made redundant');
  TestGlobal.UnregisterTestCase(TestCaseClass);
end;

procedure AssertFmt(Expr: boolean; const msg: string; const Args: array of const);
begin
  Assert(Expr, Format(Msg, args));
end;

{ TBoldTestCase }

constructor TBoldTestCase.CreateWithComment(const AName: String; const aComment: string = '');
begin
  inherited Create(AName);
  RegisterTestMethodByName(aName);
  fComment := aComment;
end;

//Note: It would possibly be simpler to create all tests using
// the function ptr. All tests would then exist (compiler check)
constructor TBoldTestCase.CreateByFunction(FunctionPtr: Pointer);
begin
  RegisterTestMethodByName(MethodName(FunctionPtr));
end;

class procedure TBoldTestCase.Suit(ASuite: TBoldTestSuite);
begin
end;

procedure TBoldTestCase.Run;
begin
  QueryPerformanceCounter(fStartTime);
  try
    if TestMethodAvailable then
    begin
      InvokeTest
    end
    else
      TestNotFound;
  except
    // Handle assertion exception = test failures
    on E:EAssertionFailed do
    begin
       fTestResult := etFailure;
       fTestMessage := E.Message;
    end;
    // Handle any other exception = test error
    on E:Exception do
    begin
       fTestResult := etError;
       fTestMessage := E.Message;
    end;
  end;
  QueryPerformanceCounter(fStopTime);
end;


procedure TBoldTestCase.RegisterTestMethodByName(const aName: string);
begin
//remove  FName := AName;

  fTestMethod.Code := MethodAddress(Name);
  fTestMethod.Data := Self;
end;

function TBoldTestCase.Equals(TestCase: TBoldTestCase): boolean;
begin
  Result := (ClassName = TestCase.ClassName) and
            (Name = TestCase.Name);
end;

function TBoldTestCase.GetTestMethodAvailable: boolean;
begin
//  Result := Assigned(fTestMethod.Code);
  Result := True;
  try
    CheckMethodIsNotEmpty(fTestMethod.Code);
  except
    Result := false;
  end;
end;

procedure TBoldTestCase.InvokeTest;
begin
 TObjectProcedure(fTestMethod);   // invoke method
 fTestResult   := etAllOK;
 fTestMessage  := '';
end;

procedure TBoldTestCase.TestNotFound;
begin
  fTestMessage := 'Method not found: ' + Name;
  fTestResult := etNotFound;
end;

function TBoldTestCase.GetComment: string;
begin
  Result := FComment;
end;

function TBoldTestCase.GetOwningSuite: TBoldTestSuite;
begin
  Result := fOwningSuite;
end;

function TBoldTestCase.GetTestMessage: string;
begin
  Result := fTestMessage;
end;

function TBoldTestCase.GetTestResult: TResultCode;
begin
  Result := fTestResult;
end;

procedure TBoldTestCase.SetComment(Value: string);
begin
  if (fComment <> Value) then
    FComment := Value;
end;

procedure TBoldTestCase.SetTestMessage(Value: string);
begin
  if (fTestMessage <> Value) then
    fTestMessage := Value;
end;

constructor TBoldTestCase.Create(Name: string);
begin
  inherited Create(Name);
end;

{ TBoldTestSuite }

constructor TBoldTestSuite.Create;
begin
  inherited Create;
  FTests := TTestList.Create;
end;

constructor TBoldTestSuite.Create(classNames: array of String);
var
  i: Integer;
begin
  Create;
  for i := 0 to High(classNames) do
    AssignSuit(classNames[i])
end;

procedure TBoldTestSuite.AssignSuit(const TestCaseClassName: String);
var
  TestCaseClass: TBoldTestCaseClass;
begin
  TestCaseClass := TestGlobal.TestCaseClassByName[TestCaseClassName];
  if Assigned(TestCaseClass) then
    TestCaseClass.Suit(Self)                           //Get the registered test methods
  else
    raise Exception.Create('Class not found ' +  TestCaseClassName);
end;

destructor TBoldTestSuite.Destroy;
begin
  FTests.Clear;
  FreeAndNil(FTests);
  inherited Destroy;
end;

// Run all tests
// Save the result
// Update counters
procedure TBoldTestSuite.Run;
var
  i: integer;
//  OldDatabaseCount: integer;
//  OldBoldInUse: integer;
begin
  FRuns     := 0;
  FErrors   := 0;
  FFailures := 0;
  FNotFound := 0;

  for i := 0 to FTests.Count-1 do // alla tester
  begin
    Inc(FRuns);
//    OldDatabaseCount := Session.DatabaseCount;
//    OldBoldInUse := BoldMemoryManager_.InUse;
    try
      fTests[i].fTestResult := etAllOK;
      FTests[i].SetUp;
      try
        FTests[i].Run;    // Run

        if Assigned(FOnFault) then
          FOnFault(fTests[i]);
      finally
        FTests[i].TearDown;
      end;
    except
      on E:Exception do
      begin
        if fTests[i].TestResult = etAllOK then
        begin
          fTests[i].fTestResult := etError;
          fTests[i].fTestMessage := 'SetUp/TearDown: ' + E.Message;
          if Assigned(FOnFault) then
            FOnFault(fTests[i]);
        end;
      end;
    end;
{
    if (fTests[i].TestResult = etAllOK ) and (Session.DatabaseCount <> OldDatabaseCount )then
    begin
      fTests[i].fTestResult := etError;
      fTests[i].fTestMessage := Format('%d UnFreed Database Components', [Session.DatabaseCount - OldDatabaseCount]);
      if Assigned(FOnFault) then
        FOnFault(fTests[i]);
    end;
}
    (*
    if (fTests[i].TestResult = etAllOK ) and (BoldMemoryManager_.InUse <> OldBoldInUse )then
    begin
      fTests[i].fTestResult := etError;
      fTests[i].fTestMessage := Format('%d (%d-%d) change in Bold Inuse memory', [BoldMemoryManager_.InUse - OldBoldInUse, BoldMemoryManager_.InUse, OldBoldInUse]);
      if Assigned(FOnFault) then
        FOnFault(fTests[i]);
    end;
    *)
    case fTests[i].TestResult of
      etNotFound:
      begin
        dec(FRuns);              // Not Found
        Inc(FNotFound);
      end;
      etAllOK: ;                 // allOK
      etFailure: Inc(FFailures); // Failure
      etError: Inc(FErrors);     // Error
    end;

  end;
end;

// Add test case to list
procedure TBoldTestSuite.AddTest(test: TBoldTestCase);
begin
  FTests.AddNoDupe(test);
  Test.fOwningSuite := Self;
end;

function TBoldTestSuite.GetProblems: integer;
begin
  Result := Errors + Failures + NotFound;
end;

procedure TBoldTestSuite.AddAllTests(TestCaseClass: TBoldTestCaseClass);
begin

end;

{ TTestList }

procedure TTestList.AddNoDupe(TestCase: TBoldTestCase);
begin
  if not Includes(TestCase) then
    Add(TestCase);
end;

procedure TTestList.Clear;
var
  t: TBoldTestCase;
begin
  while Count > 0 do
  begin
    t := TItems[0];
    t.Free;
    Delete(0);
  end;
  inherited Clear;
end;

function TTestList.GetTItems(Index: Integer): TBoldTestCase;
begin
  Result := TBoldTestCase(Items[Index])
end;

function TTestList.Includes(TestCase: TBoldTestCase): boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Count - 1 do
    Result := Result or TestCase.Equals(TItems[i]);
end;

procedure TTestList.SetTItems(Index: Integer; const Value: TBoldTestCase);
begin
  Items[Index] := Value
end;

{ TTestGlobal }

class function TTestGlobal.CommaLine(const aGroup, aName, ErrorMessage, aTime, aMsg, aComment: String): string;
var
  s: TStringlist;
begin
  s := TStringList.Create;
  s.Add(aGroup);
  s.Add(aName);
  s.Add(ErrorMessage);
  s.Add(aTime);
  s.Add(aMsg);
  s.Add(aComment);
  result := s.CommaText;
  s.Free;
end;

constructor TTestGlobal.Create;
begin
  inherited;
  fTestCaseList := TStringList.Create;
end;

destructor TTestGlobal.Destroy;
begin
  FreeAndNil(fTestCaseList);
  inherited;
end;

class function TTestGlobal.ErrorLine(const aGroup, aName, ErrorMessage,
  aMsg, aTime, aComment: String): string;
begin
//  Result := Format('%-15s %-20s %-10s %-7s %-30s / %s', [aGroup, aName, ErrorMessage, aTime, '[' + aMsg + ']', aComment]);
  Result := Format('%s%s%s%s%s%s%s%s[%s]%s%s', [aGroup, #9,
                                             aName, #9,
                                             ErrorMessage, #9,
                                             aTime, #9,
                                             aMsg, #9,
                                             aComment]);

end;

function TTestGlobal.GetTestCaseClassByName(
  const Name: string): TBoldTestCaseClass;
begin
  with TestCaseList do
    if IndexOf(Name) <> -1 then
      Result := TBoldTestCaseClass(Objects[IndexOf(Name)])
    else
      Result := nil;
end;

procedure TTestGlobal.RegisterTestCase(TestCaseClass: TBoldTestCaseClass);
begin
  TestCaseList.AddObject(TestCaseClass.ClassName, Pointer(TestCaseClass));
end;

class procedure TTestGlobal.RunTests(const TestName: string;
  NotifyFault: TNotifyFault);
var
  TestSuite: TBoldTestSuite;
begin
  TestSuite := TBoldTestSuite.Create([TestName]);
  try
    TestSuite.OnFault := NotifyFault;
    TestSuite.Run;
  finally
    TestSuite.Free;
  end;
end;

procedure TTestGlobal.UnregisterTestCase(TestCaseClass: TBoldTestCaseClass);
begin
  with TestCaseList do
    Delete(IndexOf(TestCaseClass.ClassName));
end;

{ TTestAllTestCases }

class procedure TTestAllTestCases.Suit(ASuite: TBoldTestSuite);
var
  i: integer;
begin
  with TestGlobal do
    for i := 0 to TestCaseList.Count - 1 do
      if TClass(TestCaseList.Objects[i]) <> Self then
        TBoldTestCaseClass(TestCaseList.Objects[i]).Suit(aSuite);
end;


  {utility routines}
function TestByName(List: IInterfaceList; const TestName: string): ITest;
var
  i: integer;
  CurrentTest: ITest;
  res: Boolean;
begin
  res := False;
  Result := nil;
  for i:= 0 to List.Count - 1 do
  begin
    CurrentTest := List[i] as ITest;
    res := (CompareStr(CurrentTest.Name, TestName) = 0);
    if res then Break;
  end;
  if res then
    Result := CurrentTest;
end;

procedure SetComment(const aTest: ITest; const Comment: string);
var
  aBoldTest: IBoldTestCase;
begin
  if Assigned(aTest) then
  begin
    aTest.QueryInterface(IBoldTestCase, aBoldTest);
    if Assigned(aBoldTest) then
      aBoldTest.Comment := Comment;
  end;
end;

procedure SetCommentForTest(const Suite: ITestSuite; const TestName, Comment: string);
var
  aTest: ITest;
begin
  if Assigned(Suite) then
  begin
    aTest := TestByName(Suite.Tests, TestName);
    SetComment(aTest, Comment);
  end;
end;

initialization
  TestGlobal.RegisterTestCase(TTestAllTestCases);

finalization
  FreeAndNil(G_TestGlobal);

end.
