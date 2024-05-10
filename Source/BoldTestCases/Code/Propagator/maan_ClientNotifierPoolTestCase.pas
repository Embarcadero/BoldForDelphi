unit maan_ClientNotifierPoolTestCase;

interface

uses
  BoldClientNotifierHandler,
  TestSuite,
  TestFramework;

type

  Tmaan_ClientNotifierPoolTestCase = class(TBoldTestCase)
  private
    fClientNotifierPool: TBoldClientNotifierPool;
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
    procedure SetUp; override;
    procedure TearDown; override; // cleanup after tests
  published
    procedure Test;
  end;

implementation

uses
  Sysutils,
  Boldutils;
{ Tmaan_ClientNotifierPoolTestCase }

procedure Tmaan_ClientNotifierPoolTestCase.SetUp;
begin
end;

class procedure Tmaan_ClientNotifierPoolTestCase.Suit(ASuite: TBoldTestSuite);
begin
  ASuite.AddTest(CreateWithComment('Test', 'Test TBoldClientNotifierPool'));
end;

class function Tmaan_ClientNotifierPoolTestCase.Suite: ITestSuite;
begin
  result := inherited Suite;
  SetCommentForTest(Result, 'Test', 'Test TBoldClientNotifierPool');
end;

procedure Tmaan_ClientNotifierPoolTestCase.TearDown;
begin
end;

procedure Tmaan_ClientNotifierPoolTestCase.Test;
begin
  fClientNotifierPool := TBoldClientNotifierPool.Create(nil, 5);
  FreeAndNil(FClientNotifierPool);
end;

Initialization
  TestGlobal.RegisterTestCase(Tmaan_ClientNotifierPoolTestCase);

finalization
  TestGlobal.UnRegisterTestCase(Tmaan_ClientNotifierPoolTestCase);

end.
