unit jano_multiplicity;

interface

uses
  TestSuite,
  BoldUMLTypes,
  TestFramework;

type
  TJano_multiplicity = class(TBoldTestCase)
  private
    procedure AssertRange(Range: TBoldUMLRange; lower, upper: integer; UpperUnlimited: boolean);
    procedure AssertSetAsString(SetString: string; lower, upper: integer; upperUmlimited: boolean; GetString: string='');

  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;    
  published
    procedure UMLRange;
    procedure UMLMultiplicity;    
  end;


implementation
uses
  SysUtils;

procedure TJano_multiplicity.AssertRange(Range: TBoldUMLRange; lower,
  upper: integer; UpperUnlimited: boolean);
begin
  Assert(range.lower=lower);
  Assert(range.UpperUnlimited=UpperUnlimited);
  if not UpperUnlimited then
    Assert(upper=range.upper);
end;

procedure TJano_multiplicity.AssertSetAsString(SetString: string; lower,
  upper: integer; upperUmlimited: boolean; GetString: string);
var
  aRange: TBoldUMLRange;
begin
  aRange := TBoldUMLRange.Create;
  if GetString='' then
    GetString := SetString;
  try
    aRange.AsString := SetString;
    AssertRange(arange,lower,upper, upperUmlimited);
    Assert(aRange.AsString=GetString);
  finally
    arange.Free;
  end;
end;

class procedure TJano_multiplicity.Suit(ASuite: TBoldTestSuite);
begin
  inherited;
  ASuite.AddTest(CreateWithComment('UMLRange'));
  ASuite.AddTest(CreateWithComment('UMLMultiplicity'));
end;

class function TJano_multiplicity.Suite: ITestSuite;
begin
  Result := inherited Suite;
  SetCommentForTest(Result, 'UMLRange', 'Something');
  SetCommentForTest(Result, 'UMLMultiplicity', 'Something else');
end;

procedure TJano_multiplicity.UMLMultiplicity;
var
  aMultiplicity: TBoldUMLMultiplicity;
begin
  aMultiplicity := TBoldUMLMultiplicity.Create;
  try
    Assert(aMultiplicity.count = 0);
    aMultiplicity.AsString := '0';
    Assert(aMultiplicity.count=1);
    AssertRange(aMultiplicity.Range[0], 0, 0, false);
    assert (aMultiplicity.asstring = '0');
    aMultiplicity.AsString := '0..*';
    Assert(aMultiplicity.count=1);
    AssertRange(aMultiplicity.Range[0], 0, 17, true);
    assert (aMultiplicity.asstring = 'n');
   aMultiplicity.AsString := '0..4,5,7';
    Assert(aMultiplicity.count=3);
    AssertRange(aMultiplicity.Range[0], 0, 4, false);
    AssertRange(aMultiplicity.Range[1], 5, 5, false);
    AssertRange(aMultiplicity.Range[2], 7, 7, false);
    assert (aMultiplicity.asstring = '0..4,5,7')

  finally
    FreeAndNil(aMultiplicity);
  end;
end;

procedure TJano_multiplicity.UMLRange;
var
  aRange: TBoldUMLRange;
begin
  AssertSetAsString('0',     0, 0,  false);
  AssertSetAsString('1',     1, 1,  false);
  AssertSetAsString('9',     9, 9,  false);
  AssertSetAsString(' 9 ',     9, 9,  false, '9');
  AssertSetAsString('0..1',  0, 1,  false);
  AssertSetAsString(' 0..1 ',  0, 1,  false, '0..1');
  AssertSetAsString('0..n',  0, 17, true, 'n');
  AssertSetAsString('0..*',  0, 17, true, 'n');
  AssertSetAsString('0..-1', 0, 17, true, 'n');
  AssertSetAsString('1..n',  1, 17, true);
  AssertSetAsString('1..*',  1, 17, true, '1..n');
  AssertSetAsString('1..-1', 1, 17, true, '1..n');
  AssertSetAsString( '9..n', 9, 17, true);
  AssertSetAsString('9..*',  9, 17, true, '9..n');
  AssertSetAsString('9..-1', 9, 17, true, '9..n');

  aRange := TBoldUMLRange.Create;
  try
    AssertRange(arange, 0, 0, false);  // verify newly created range;

    // test formatting
    aRange.AsString := '1..n';
    assert(arange.FormatAsString('*') = '1..*');

    try
      aRange.AsString := 'n..1';
      Assert(False);
    except
      on E: Exception do assert(e is EConvertError);
    end;
  finally
    FreeAndNil(aRange);
  end;

end;

initialization
  TestGlobal.RegisterTestCase(TJano_multiplicity);

end.
