unit maan_BoldAdvancedPropagatorTestCase;

interface

uses
  BoldAdvancedPropagator,
  TestSuite,
  TestFramework;

type
  Tmaan_BoldAdvancedPropagatorTestCase = class(TBoldTestCase)
  private
    fAdvancedPropagator: TBoldAdvancedPropagator;
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
  SysUtils,
  BoldUtils,
  BoldPropagatorServer
  ;
{ Tmaan_BoldAdvancedPropagatorTestCase }

procedure Tmaan_BoldAdvancedPropagatorTestCase.SetUp;
begin
  fAdvancedPropagator := TBoldAdvancedPropagator.Create;
end;

class procedure Tmaan_BoldAdvancedPropagatorTestCase.Suit(
  ASuite: TBoldTestSuite);
begin
  ASuite.AddTest(CreateWithComment('Test', ' Testing class TBoldAdvancedPropagator'));
end;

class function Tmaan_BoldAdvancedPropagatorTestCase.Suite: ITestSuite;
begin
  result := inherited Suite;
  SetCommentForTest(result, 'Test', ' Testing class TBoldAdvancedPropagator');
end;

procedure Tmaan_BoldAdvancedPropagatorTestCase.TearDown;
begin
  FreeAndNil(fAdvancedPropagator);
  TBoldPropagatorServer.FreeSingleton;
end;

procedure Tmaan_BoldAdvancedPropagatorTestCase.Test;
begin
  fAdvancedPropagator.Initialize;
  Assert(True, '');
end;

initialization
  TestGlobal.RegisterTestCase(Tmaan_BoldAdvancedPropagatorTestCase);

finalization
  TestGlobal.UnRegisterTestCase(Tmaan_BoldAdvancedPropagatorTestCase);

end.
