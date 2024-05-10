unit dmjehoBoldTest;

interface

uses
  Classes,
  Controls,
  Forms,
  TestSuite,
  jehoBCBoldTest,
  BoldSystem,
  BoldHandle,
  BoldUMLModelLink,
  BoldUMLRose98Link,
  BoldHandles,
  BoldSystemHandle,
  BoldSubscription,
  BoldModel,
  BoldTypeNameHandle,
  BoldDefs,
  TestFrameWork,
  BoldAttributes, BoldAbstractModel;

type
   TCompareTypeSet = set of TBoldCompareType; //(ctDefault, ctAsString, ctAsText, ctAsAnsiString, ctAsAnsiText, ctAsDate, ctAsTime);
//   CompareTypeSet = TComparetypeSet;

   TjehodmBoldTest = class(TDataModule)
    BoldModel1: TBoldModel;
    BoldSystemHandle1: TBoldSystemHandle;
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
    BoldUMLRose98Link1: TBoldUMLRose98Link;
    BoldTypeNameHandle1: TBoldTypeNameHandle;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TjehoBoldTest = class(TBoldTestCase)
  private
  protected
    procedure SetAttributeValues(ObjectClassA: TClassA);
    procedure CheckAttributes(ObjectClassA: TClassA);
    procedure NullAttributeValues(ObjectClassA: TClassA);
    procedure CheckNullAttributes(ObjectClassA: TClassA);
    procedure CheckAttributeIndexes(ObjectClassA: TClassA);
    procedure AccessMember(BoldMember: TBoldMember);
    procedure CompareMembers(comparemember, comparewith: TBoldMember);
    procedure CheckDerivedAttributes(ObjectSubA:TClassDerivedA);
    procedure CheckOclDerivedAttributes(ObjectOclSubA:TClassOclDerivedA);
    //CompareToAs Tester:
    procedure TestString(OperandA,OperandB: TBoldAttribute);
    procedure TestInteger(Operanda,OperandB: TBAInteger);
    procedure TestCurrency(OperandA,OperandB:TBACurrency);
    procedure TestDate(OperandA,OperandB:TBADate);
    procedure TestDateTime(OperandA,OperandB:TBADateTime);
    procedure TestTime(OperandA,OperandB:TBATime);
    procedure TestFloat(OperandA,OperandB:TBAFloat);
    procedure TestCompareToAs(Op1, Op2: TBoldAttribute; expected:integer);
    procedure checkcomparetypes(OperandA,OperandB:TBoldAttribute;ValidCompareTypes:TCompareTypeSet);
    procedure InternalCheckCompareTypes(OperandA,OperandB:TBoldAttribute;CompareType:TBoldCompareType; willwork:boolean);
    //Nytt:
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure AttributeValueTest;  //TC00001
    procedure AttributeNullTest;   //TC00002
    procedure AttributeIndexTest;  //TC00005
    procedure AttributeDerivationTest; //TC00006
    procedure AttributeOclDerivationTest;
    procedure CheckMultiDerivedAttributes;
    //CompareToAs Tester:
    procedure CompareToAsTestInit;
    //nytt:
    procedure CompareToAsNullTestInit;
    procedure TestCompareTypes;
  end;

var
  jehodmBoldTest: TjehodmBoldTest;

implementation
uses
  BoldUtils,
  SysUtils;

{$R *.DFM}
const
  FLOATCONST: Real        = 0.1;
  INTEGERCONST: Integer   = 1;
  STRINGCONST: String     = 'a';
  BOOLEANCONST: Boolean   = true;
  CURRENCYCONST: Currency = 0.1;
  NROFATTRIBUTES = 14;

{ TBoldTest }

procedure TjehoBoldTest.CheckAttributes(ObjectClassA: TClassA);
begin
  Assert(ObjectClassA.aInteger = INTEGERCONST, 'Integer');
  Assert(ObjectClassA.aFloat = FLOATCONST, 'Float');
  Assert(ObjectClassA.aSmallInt = INTEGERCONST, 'SmallInt');
  Assert(ObjectClassA.aCurrency = CURRENCYCONST,  'Currency');
  Assert(ObjectClassA.aTime = FLOATCONST, 'Time');
  Assert(ObjectClassA.aWord = INTEGERCONST, 'Word');
  Assert(ObjectClassA.aString = STRINGCONST, 'String');
  Assert(ObjectClassA.aDateTime = FLOATCONST, 'DateTime');
  Assert(ObjectClassA.aByte = INTEGERCONST, 'Byte');
  Assert(ObjectClassA.aDate = INTEGERCONST, 'Date');
  Assert(ObjectClassA.aBoolean = BOOLEANCONST, 'Boolean');
  Assert(ObjectClassA.aShortInt = INTEGERCONST, 'ShortInt');
  Assert(ObjectClassA.aBlob = STRINGCONST, 'Blob');
  Assert(ObjectClassA.aBlobContent = STRINGCONST, 'BlobContent');
end;


procedure TjehoBoldTest.CheckNullAttributes(ObjectClassA: TClassA);
begin
  with ObjectClassA do
  begin
    Assert(m_aInteger.IsNull, 'm_aInteger');
    Assert(m_aFloat.IsNull, 'm_aFloat');
    Assert(m_aSmallInt.IsNull, 'm_aSmallInt');
    Assert(m_aCurrency.IsNull, 'm_aCurrency');
    Assert(m_aTime.IsNull, 'm_aTime');
    Assert(m_aWord.IsNull, 'm_aWord');
    Assert(m_aString.IsNull, 'm_aString');
    Assert(m_aDateTime.IsNull, 'm_aDateTime');
    Assert(m_aByte.IsNull, 'm_aByte');
    Assert(m_aDate.IsNull, 'm_aDate');
    Assert(m_aBoolean.IsNull, 'm_aBoolean');
    Assert(m_aSmallInt.IsNull, 'm_aSmallInt');
    Assert(m_aBlob.IsNull, 'm_aBlob');
    Assert(m_aBlobContent.IsNull, 'm_aBlobContent');
  end;
end;

procedure TjehoBoldTest.NullAttributeValues(ObjectClassA: TClassA);
begin
  with ObjectClassA do
  begin
    m_aInteger.SetToNull;
    m_aString.SetToNull;
    m_aBoolean.SetToNull;
    m_aByte.SetToNull;
    m_aCurrency.SetToNull;
    m_aTime.SetToNull;
    m_aDate.SetToNull;
    m_aFloat.SetToNull;
    m_aDateTime.SetToNull;
    m_aWord.SetToNull;
    m_aShortInt.SetToNull;
    m_aSmallInt.SetToNull;
    m_aBlob.SetToNull;
    m_aBlobContent.SetToNull;
  end;
end;

procedure TjehoBoldTest.SetAttributeValues(ObjectClassA: TClassA);
begin
  with ObjectClassA do
  begin
    aInteger      := INTEGERCONST;
    aFloat        := FLOATCONST;
    aSmallInt     := INTEGERCONST;
    aCurrency     := CURRENCYCONST;
    aTime         := FLOATCONST;
    aWord         := INTEGERCONST;
    aString       := STRINGCONST;
    aDateTime     := FLOATCONST;
    aByte         := INTEGERCONST;
    aDate         := INTEGERCONST;
    aBoolean      := BOOLEANCONST;
    aShortInt     := INTEGERCONST;
    aBlob         := STRINGCONST;
    aBlobContent  := STRINGCONST;
  end;
end;

procedure TjehoBoldTest.SetUp;
begin
  jehodmBoldTest := TjehodmBoldTest.Create(nil);
end;

procedure TjehoBoldTest.AttributeNullTest;
var
  ObjectA:TClassA;
begin
  ObjectA := TClassA.Create(nil);
  NullAttributeValues(ObjectA);
  CheckNullAttributes(ObjectA);
  ObjectA.Delete;
end;

procedure TjehoBoldTest.AttributeValueTest;
var
  ObjectA:TClassA;
begin
  ObjectA := TClassA.Create(nil);
  SetAttributeValues(ObjectA);
  CheckAttributes(ObjectA);
  ObjectA.Delete;
end;

procedure TjehoBoldTest.TearDown;
begin
  FreeAndNil(jehodmBoldTest);
end;

class procedure TjehoBoldTest.Suit(ASuite: TBoldTestSuite);
begin
  inherited;
  ASuite.AddTest(CreateWithComment('AttributeValueTest', 'TC00001'));
  ASuite.AddTest(CreateWithComment('AttributeNullTest', 'TC00002'));
  ASuite.AddTest(CreateWithComment('AttributeDerivationTest', 'TC00005'));
  ASuite.AddTest(CreateWithComment('AttributeIndexTest', 'TC00006'));
  ASuite.AddTest(CreateWithComment('AttributeOclDerivationTest'));
  ASuite.AddTest(CreateWithComment('CheckMultiDerivedAttributes'));
  ASuite.AddTest(CreateWithComment('CompareToAsTestInit'));
  ASuite.AddTest(CreateWithComment('CompareToAsNullTestInit'));
  ASuite.AddTest(CreateWithComment('TestCompareTypes'));
end;

procedure TjehoBoldTest.AttributeIndexTest;
var
  ObjectA:TClassA;
begin
  ObjectA := TClassA.Create(nil);
  CheckAttributeIndexes(ObjectA);
  ObjectA.Delete;
end;

procedure TjehoBoldTest.CheckAttributeIndexes(ObjectClassA:TClassA);
begin
  Assert(ObjectClassA.BoldMemberCount = NROFATTRIBUTES, 'Wrong numer of attributes');

  with ObjectClassA do
  begin
    AccessMember(m_aString);
    AccessMember(m_aBoolean);
    AccessMember(m_aByte);
    AccessMember(m_aCurrency);
    AccessMember(m_aDate);
    AccessMember(m_aDateTime);
    AccessMember(m_aFloat);
    AccessMember(m_aInteger);
    AccessMember(m_aShortInt);
    AccessMember(m_aSmallInt);
    AccessMember(m_aTime);
    AccessMember(m_aWord);
    AccessMember(m_aBlob);
    AccessMember(m_aBlobContent);
  end;
end;

procedure TjehoBoldTest.AccessMember(BoldMember: TBoldMember);
begin
  // We check the correctness by accessing the member.
  // There are asserts in the generated code that will assert correct index
  AssertFmt(Assigned(BoldMember.BoldSystem), '%s has no system', [BoldMember.ClassName]);
end;


procedure TjehoBoldTest.AttributeDerivationTest;
var
  ObjectSubA: TClassDerivedA;
begin
  ObjectSubA := TClassDerivedA.Create(nil);
  SetAttributeValues(ObjectSubA);
  CheckDerivedAttributes(ObjectSubA);
  ObjectSubA.Delete;
end;

procedure TjehoBoldTest.CheckDerivedAttributes(ObjectSubA:TClassDerivedA);
begin
  with ObjectSubA do
  begin
    CompareMembers(m_aDerivedInteger, m_aInteger);
    CompareMembers(m_aDerivedFloat, m_aFloat);
    CompareMembers(m_aDerivedSmallInt, m_aSmallInt);
    CompareMembers(m_aDerivedCurrency, m_aCurrency);
    CompareMembers(m_aDerivedTime, m_aTime);
    CompareMembers(m_aDerivedWord, m_aWord);
    CompareMembers(m_aDerivedString, m_aString);
    CompareMembers(m_aDerivedDateTime, m_aDateTime);
    CompareMembers(m_aDerivedByte, m_aByte);
    CompareMembers(m_aDerivedDate, m_aDate);
    CompareMembers(m_aDerivedBoolean, m_aBoolean);
    CompareMembers(m_aDerivedSmallInt, m_aSmallInt);
    CompareMembers(m_aDerivedBlob, m_aBlob);
    CompareMembers(m_aDerivedBlobContent, m_aBlobContent);
  end;
end;

procedure TjehoBoldTest.CompareMembers(CompareMember,CompareWith: TBoldMember);
begin
  AssertFmt(CompareMember.IsEqual(CompareWith), 'Derived attribute %s did not work. sorry.', [CompareMember.ClassName]);
end;

procedure TjehoBoldTest.AttributeOclDerivationTest;
var
  ObjectOclSubA: TClassOclDerivedA;
begin
  ObjectOclsubA := TClassOclDerivedA.Create(nil);
  SetAttributeValues(ObjectOclSubA);
  CheckOclDerivedAttributes(ObjectOclSubA);
  ObjectOclSubA.Delete;
end;

procedure TjehoBoldTest.CheckOclDerivedAttributes;
begin
  with ObjectOclSubA do
  begin
    CompareMembers(m_aOclDerivedInteger, m_aInteger);
    CompareMembers(m_aOclDerivedFloat, m_aFloat);
    CompareMembers(m_aOclDerivedSmallInt, m_aSmallInt);
    CompareMembers(m_aOclDerivedCurrency, m_aCurrency);
    CompareMembers(m_aOclDerivedTime, m_aTime);
    CompareMembers(m_aOclDerivedWord, m_aWord);
    CompareMembers(m_aOclDerivedString, m_aString);
    CompareMembers(m_aOclDerivedDateTime, m_aDateTime);
    CompareMembers(m_aOclDerivedByte, m_aByte);
    CompareMembers(m_aOclDerivedDate, m_aDate);
    CompareMembers(m_aOclDerivedBoolean, m_aBoolean);
    CompareMembers(m_aOclDerivedSmallInt, m_aSmallInt);
    CompareMembers(m_aOclDerivedBlob, m_aBlob);
    CompareMembers(m_aOclDerivedBlobContent, m_aBlobContent);
  end;
end;

procedure TjehoBoldTest.CheckMultiDerivedAttributes;
var
  ObjectMultiOclsubA :TClassMultiDerivedA;
begin
  ObjectMultiOclsubA := TClassMultiDerivedA.Create(nil);
  SetAttributeValues(ObjectMultiOclSubA);
  with objectmultioclsuba do
  begin
    AssertFmt(m_aMultiDerivedBoolean.CompareToAs(ctDefault, m_aOclDerivedBoolean) = 0, 'Multi derived attribute %s did not work. sorry.', ['MultiDerivedBoolean']);
    AssertFmt(m_aMultiDerivedByte.CompareToAs(ctDefault, m_aOclDerivedByte) = 0, 'Multi derived attribute %s did not work. sorry.', ['MultiDerivedByte']);
    AssertFmt(m_aMultiDerivedCurrency.CompareToAs(ctDefault, m_aOclDerivedCurrency) = 0, 'Multi derived attribute %s did not work. sorry.', ['MultiDerivedCurrency']);
    AssertFmt(m_aMultiDerivedDate.CompareToAs(ctDefault, m_aOclDerivedDate) = 0, 'Multi derived attribute %s did not work. sorry.', ['MultiDerivedDate']);
    AssertFmt(m_aMultiDerivedDateTime.CompareToAs(ctDefault, m_aOclDerivedDateTime) = 0, 'Multi derived attribute %s did not work. sorry.', ['MultiDerivedDateTime']);
    AssertFmt(m_aMultiDerivedFloat.CompareToAs(ctDefault, m_aOclDerivedFloat) = 0, 'Multi derived attribute %s did not work. sorry.', ['MultiDerivedFloat']);
    AssertFmt(m_aMultiDerivedInteger.CompareToAs(ctDefault, m_aOclDerivedInteger) = 0, 'Multi derived attribute %s did not work. sorry.', ['MultiDerivedInteger']);
    AssertFmt(m_aMultiDerivedShortInt.CompareToAs(ctDefault, m_aOclDerivedShortInt) = 0, 'Multi derived attribute %s did not work. sorry.', ['MultiDerivedShortInt']);
    AssertFmt(m_aMultiDerivedSmallInt.CompareToAs(ctDefault, m_aOclDerivedSmallInt) = 0, 'Multi derived attribute %s did not work. sorry.', ['MultiDerivedSmallInt']);
    AssertFmt(m_aMultiDerivedString.CompareToAs(ctDefault, m_aOclDerivedString) = 0, 'Multi derived attribute %s did not work. sorry.', ['MultiDerivedString']);;
    AssertFmt(m_aMultiDerivedTime.CompareToAs(ctDefault, m_aOclDerivedTime) = 0, 'Multi derived attribute %s did not work. sorry.', ['MultiDerivedTime']);
    AssertFmt(m_aMultiDerivedWord.CompareToAs(ctDefault, m_aOclDerivedWord) = 0, 'Multi derived attribute %s did not work. sorry.', ['MultiDerivedWord']);
  end;
  ObjectMultiOclSubA.Delete;
end;

procedure TjehoBoldTest.CompareToAsTestInit;
var
  PrimaryTestClass: TClassB;
begin
  //Initiera testerna:
  PrimaryTestClass := TClassB.Create(nil);
  //TimeTest
  TestTime(PrimaryTestClass.m_aTime,PrimaryTestClass.m_bTime);
  //StringTest
  TestString(PrimaryTestClass.m_aString,PrimaryTestClass.m_bString);
  //IntegerTest
  TestInteger(PrimaryTestClass.m_aInteger,PrimaryTestClass.m_bInteger);
  TestInteger(PrimaryTestClass.m_aSmallInt,PrimaryTestClass.m_bSmallInt);
  TestInteger(PrimaryTestClass.m_aShortInt,PrimaryTestClass.m_bShortInt);
  TestInteger(PrimaryTestClass.m_aWord,PrimaryTestClass.m_bWord);
  TestInteger(PrimaryTestClass.m_aByte,PrimaryTestClass.m_bByte);
  //CurrencyTest
  TestCurrency(PrimaryTestClass.m_aCurrency,PrimaryTestClass.m_bCurrency);
  //DateTest
  TestDate(PrimaryTestClass.m_aDate,PrimaryTestClass.m_bDate);
  //DateTimeTest
  TestDateTime(PrimaryTestClass.m_aDateTime,PrimaryTestClass.m_bDateTime);
  //BlobTest
  TestString(PrimaryTestClass.m_aBlob,PrimaryTestClass.m_bBlob);
  TestString(PrimaryTestClass.M_aBlobContent,PrimaryTestClass.m_bBlobContent);
  PrimaryTestClass.Delete;
end;

procedure TjehoBoldTest.TestString(OperandA,OperandB: TBoldAttribute);
begin
  //StringTest
  //Sätt variablerna
  OperandA.AsString := 'AB';
  OperandB.AsString := 'AC';
  //Testa större/mindre
  TestCompareToAs(OperandA, OperandB, -1); //Testa mindre
  OperandB.AsString := 'AA'; //Sätt om en variabel
  TestCompareToAs(OperandA, OperandB, 1);  //Testa större
  //Stringtest klart
end;

procedure TjehoBoldTest.TestCompareToAs(Op1, Op2: TBoldAttribute; expected: integer);
begin
  AssertFmt(Op1.CompareToAs(ctdefault, Op2) = expected, 'Comparison %s failed', [op1.ClassName]);
end;

procedure TjehoBoldTest.TestInteger(Operanda, OperandB: TBAInteger);
begin
  //IntegerTest
  //Sätt variablerna
  OperandA.AsInteger := 2;
  OperandB.AsInteger := 3;
  //Testa större/mindre
  TestCompareToAs(OperandA, OperandB, -1); //Testa mindre
  OperandB.AsInteger := 1; //Sätt om en variabel
  TestCompareToAs(OperandA, OperandB, 1);  //Testa större
  //Integertest klart
end;

procedure TjehoBoldTest.TestCurrency(OperandA, OperandB: TBACurrency);
begin
  //CurrencyTest
  //Sätt variablerna
  OperandA.AsFloat := 2.2;
  OperandB.AsFloat := 3.3;
  //Testa större/mindre
  TestCompareToAs(OperandA, OperandB, -1); //Testa mindre
  OperandB.AsFloat := 1.1; //Sätt om en variabel
  TestCompareToAs(OperandA, OperandB, 1);  //Testa större
  //Currencytest klart
end;

procedure TjehoBoldTest.TestDate(OperandA, OperandB: TBADate);
begin
  //DateTest
  //Sätt variablerna
  OperandA.AsDate := 2;
  OperandB.AsDate := 3;
  //Testa större/mindre
  TestCompareToAs(OperandA, OperandB, -1); //Testa mindre
  OperandB.AsDate := 1; //Sätt om en variabel
  TestCompareToAs(OperandA, OperandB, 1);  //Testa större
  //Datetest klart
end;

procedure TjehoBoldTest.TestFloat(OperandA, OperandB: TBAFloat);
begin
  //FlyttalTest
  //Sätt variablerna
  OperandA.AsFloat := 2;
  OperandB.AsFloat := 3;
  //Testa större/mindre
  TestCompareToAs(OperandA, OperandB, -1); //Testa mindre
  OperandB.AsFloat := 1; //Sätt om en variabel
  TestCompareToAs(OperandA, OperandB, 1);  //Testa större
  //FlyttalTest klart
end;

procedure TjehoBoldTest.TestDateTime(OperandA, OperandB: TBADateTime);
begin
  //DateTimeTest
  //Sätt variablerna
  OperandA.AsDate := 2;
  OperandB.AsDate := 3;
  //Testa större/mindre
  TestCompareToAs(OperandA, OperandB, -1); //Testa mindre
  OperandB.AsDate := 1; //Sätt om en variabel
  TestCompareToAs(OperandA, OperandB, 1);  //Testa större
  //DateTimeTest klart
end;

procedure TjehoBoldTest.TestTime(OperandA, OperandB: TBATime);
begin
  //TimeTest
  //Sätt variablerna
  OperandA.AsTime := 2.4;
  OperandB.AsTime := 3.5;
  //Testa större/mindre
  TestCompareToAs(OperandA, OperandB, -1); //Testa mindre
  OperandB.AsTime := 1.3; //Sätt om en variabel
  TestCompareToAs(OperandA, OperandB, 1);  //Testa större
  //TimeTest klart
end;

procedure TjehoBoldTest.CompareToAsNullTestInit;
var
  ObjectSetToNullTest :TclassB;
  Object2SetToNullTest:TclassA;
begin
  ObjectSetToNullTest := TClassB.Create(nil);
  Object2SetToNullTest:= TClassA.Create(nil);
  with ObjectSetToNullTest do
  begin
    m_bInteger.SetToNull;
    m_bString.SetToNull;
    m_bBoolean.SetToNull;
    m_bByte.SetToNull;
    m_bCurrency.SetToNull;
    m_bTime.SetToNull;
    m_bDate.SetToNull;
    m_bFloat.SetToNull;
    m_bDateTime.SetToNull;
    m_bWord.SetToNull;
    m_bShortInt.SetToNull;
    m_bSmallInt.SetToNull;
    m_bBlob.SetToNull;
    m_bBlobContent.SetToNull;
  end;
  with Object2SetToNullTest do
  begin
    m_aInteger.AsInteger := 2;
    m_aString.AsString := 'AAA';
    m_aBoolean.AsBoolean := true;
    m_aByte.AsByte := 5;
    m_aCurrency.AsCurrency := 23.421;
    m_aTime.AsTime := 1.2;
    m_aDate.AsDate := 2;
    m_aFloat.AsFloat := 3.43;
    m_aDateTime.AsDateTime := 4.2;
    m_aWord.AsWord := 4;
    m_aShortInt.AsShortInt := 24;
    m_aSmallInt.AsSmallInt := 122;
    m_aBlob.AsString := 'yfiew';
    m_aBlobContent.AsString := 'graij';
  end;
  with ObjectSetToNullTest do
  begin
    TestCompareToAs(m_bInteger, Object2SetToNullTest.m_aInteger, -1);
    TestCompareToAs(m_bString, Object2SetToNullTest.m_aString, -1);
    TestCompareToAs(m_bBoolean, Object2SetToNullTest.m_aBoolean, -1);
    TestCompareToAs(m_bByte, Object2SetToNullTest.m_aByte, -1);
    TestCompareToAs(m_bCurrency, Object2SetToNullTest.m_aCurrency, -1);
    TestCompareToAs(m_bTime, Object2SetToNullTest.m_aTime, -1);
    TestCompareToAs(m_bDate, Object2SetToNullTest.m_aDate, -1);
    TestCompareToAs(m_bFloat, Object2SetToNullTest.m_aFloat, -1);
    TestCompareToAs(m_bDateTime, Object2SetToNullTest.m_aDateTime, -1);
    TestCompareToAs(m_bWord, Object2SetToNullTest.m_aWord, -1);
    TestCompareToAs(m_bShortInt, Object2SetToNullTest.m_aShortInt, -1);
    TestCompareToAs(m_bSmallInt, Object2SetToNullTest.m_aSmallInt, -1);
    TestCompareToAs(m_bBlob, Object2SetToNullTest.m_aBlob, -1);
    TestCompareToAs(m_bBlobContent, Object2SetToNullTest.m_aBlobContent, -1);
  end;
  ObjectSetToNullTest.delete;
  Object2SetToNullTest.delete;
end;

//CheckCT(m_aSTring, m_bString, [ctDefault, ctAsText, ctAnsi...]);
//CheckCT(m_aInteger, m_bInteger, [ctDefault]);
//
//CheckCT(op1, Op2: TBoldAttribute; ValidCompareTypes: CompareTypeSet);
//begin
//  InternalCheckCT(Op1, Op2, ctDefault, ctDefault in ValidCOmpareTypes);
//  InternalCheckCT(Op1, Op2, ctAsText, ctAsText in ValidCOmpareTypes);
//  InternalCheckCT(Op1, Op2, ctDefault, ctDefault in ValidCOmpareTypes);
//  InternalCheckCT(Op1, Op2, ctDefault, ctDefault in ValidCOmpareTypes);


procedure TjehoBoldTest.checkcomparetypes(OperandA, OperandB: TBoldAttribute; ValidCompareTypes: TCompareTypeSet);
begin
  InternalCheckCompareTypes(OperandA, OperandB, ctDefault, ctDefault in ValidCompareTypes);
  InternalCheckCompareTypes(OperandA, OperandB, ctAsString, ctAsString in ValidCompareTypes);
  InternalCheckCompareTypes(OperandA, OperandB, ctCaseSensitive, ctCaseSensitive in ValidCompareTypes);
  InternalCheckCompareTypes(OperandA, OperandB, ctCaseInsensitive, ctCaseInsensitive in ValidCompareTypes);
  InternalCheckCompareTypes(OperandA, OperandB, ctAsDate, ctAsDate in ValidCompareTypes);
  InternalCheckCompareTypes(OperandA, OperandB, ctAsTime, ctAsTime in ValidCompareTypes);
end;
procedure TjehoBoldTest.TestCompareTypes;
var
  TestClass:TClassA;
begin
  TestClass := TClassA.Create(nil);
  with TestClass do
  begin
    m_aInteger.AsInteger := 2;
    m_aString.AsString := 'AAA';
    m_aBoolean.AsBoolean := true;
    m_aByte.AsByte := 5;
    m_aCurrency.AsCurrency := 23.421;
    m_aTime.AsTime := 1.2;
    m_aDate.AsDate := 2;
    m_aFloat.AsFloat := 3.43;
    m_aDateTime.AsDateTime := 4.2;
    m_aWord.AsWord := 4;
    m_aShortInt.AsShortInt := 24;
    m_aSmallInt.AsSmallInt := 122;
    m_aBlob.AsString := 'yfiew';
    m_aBlobContent.AsString := 'graij';
  end;
  with testclass do
  begin
    CheckCompareTypes(m_aInteger,m_aInteger,[ctDefault]);
    CheckCompareTypes(m_aString,m_aString, [ctDefault, ctAsString, ctCaseSensitive, ctCaseInsensitive, ctAsDate, ctAsTime]);
    CheckCompareTypes(m_aBoolean,m_aBoolean,[ctDefault]);
    CheckCompareTypes(m_aByte,m_aByte,[ctDefault]);
    CheckCompareTypes(m_aCurrency,m_aCurrency,[ctDefault]);
    CheckCompareTypes(m_aTime,m_aTime,[ctDefault,ctAsTime]);
    CheckCompareTypes(m_aDate,m_aDate,[ctDefault,ctAsDate]);
    CheckCompareTypes(m_aFloat,m_aFloat,[ctDefault]);
    CheckCompareTypes(m_aDateTime,m_aDateTime,[ctDefault,ctAsDate,ctAsTime]);
    CheckCompareTypes(m_aWord,m_aWord,[ctDefault]);
    CheckCompareTypes(m_aShortInt,m_aShortInt,[ctDefault]);
    CheckCompareTypes(m_aSmallInt,m_aSmallInt,[ctDefault]);
    CheckCompareTypes(m_aBlob,m_aBlob,[ctDefault,ctAsString,ctCaseSensitive, ctCaseInsensitive]);
    CheckCompareTypes(m_aBlobContent,m_aBlobContent,[ctDefault,ctAsString,ctCaseSensitive, ctCaseInsensitive]);
  end;
TestClass.delete;
end;
//(ctDefault, ctAsString, ctAsText, ctAsAnsiString, ctAsAnsiText, ctAsDate, ctAsTime);
procedure TjehoBoldTest.InternalCheckCompareTypes(OperandA,OperandB: TBoldAttribute; CompareType: TBoldCompareType; willwork: boolean);
begin
  try
    if WillWork then
      OperandA.CompareToAs(CompareType, OperandB);
//    assert(willwork)
  except
    assert(not willwork);
  end;
end;

class function TjehoBoldTest.Suite: ITestSuite;
begin
  Result := inherited Suite;
  SetCommentForTest(Result, 'AttributeValueTest', 'TC00001');
  SetCommentForTest(Result, 'AttributeNullTest', 'TC00002');
  SetCommentForTest(Result, 'AttributeDerivationTest', 'TC00005');
  SetCommentForTest(Result, 'AttributeIndexTest', 'TC00006');
  SetCommentForTest(Result, 'AttributeOclDerivationTest', '');
  SetCommentForTest(Result, 'CheckMultiDerivedAttributes', '');
  SetCommentForTest(Result, 'CompareToAsTestInit', '');
  SetCommentForTest(Result, 'CompareToAsNullTestInit', '');
  SetCommentForTest(Result, 'TestCompareTypes', '');
end;

initialization
  TestGlobal.RegisterTestCase(TjehoBoldTest);

end.
