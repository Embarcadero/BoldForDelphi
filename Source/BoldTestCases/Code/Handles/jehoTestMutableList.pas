unit jehoTestMutableList;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  BoldElements,
  BoldSortedHandle,
  BoldSubscription,
  BoldHandles,
  BoldRootedHandles,
  BoldAbstractListHandle,
  BoldCursorHandle,
  BoldListHandle,
  TestSuite,
  ExtCtrls,
  BoldNavigator,
  BoldSystemHandle,
  BoldNavigatorDefs,
  TestFramework;

type
  TfrmjehoTestComparer = class(TForm)
    BoldListHandle1: TBoldListHandle;
    BoldComparer1: TBoldComparer;
    BoldSystemHandle1: TBoldSystemHandle;
    BoldNavigator1: TBoldNavigator;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TjehoTestMutableList = class(TBoldTestCase)
  public
    class procedure Suit(aSuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
  published
    procedure TestComparer;
  end;


implementation

uses dmModel1;

{$R *.DFM}

{ TYourTestCaseHere }

class procedure TjehoTestMutableList.Suit(aSuite: TBoldTestSuite);
begin
  inherited;
  //--Add tests here--
  ASuite.AddTest(CreateWithComment('TestComparer' , 'Accessing mutable list on listhandle'));
end;

class function TjehoTestMutableList.Suite: ITestSuite;
begin
  Result := inherited Suite;
  SetCommentForTest(Result, 'TestComparer', 'Accessing mutable list on listhandle');
end;

procedure TjehoTestMutableList.TestComparer;
var
  f: TfrmjehoTestComparer;
begin
  // The bug manifested itself when the mutablelist was accessed and the listhandle's
  // comparer's RootHandle had no value
  try
    f := TfrmjehoTestComparer.Create(nil);
  finally
    FreeAndNil(f);
  end;
end;

initialization
  TestGlobal.RegisterTestCase(TjehoTestMutableList);

end.
