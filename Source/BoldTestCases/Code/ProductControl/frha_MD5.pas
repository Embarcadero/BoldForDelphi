
unit frha_MD5;

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
  BoldMD5,
  TestSuite,
  TestFramework;

type

  TFrha_MD5 = class(TBoldTestCase)
  public
    class procedure Suit(aSuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
  published
    procedure MD5SelfTest;
  end;


implementation

{ TFrha_MD5 }

class procedure TFrha_MD5.Suit(aSuite: TBoldTestSuite);
begin
  inherited;
  ASuite.AddTest(CreateWithComment('MD5SelfTest' , ''));
end;

procedure TFrha_MD5.MD5SelfTest;
begin
  TBoldMD5.SelfTest;
end;




class function TFrha_MD5.Suite: ITestSuite;
begin
  result := inherited Suite;
  SetCommentForTest(result, 'MD5SelfTest' , '');
end;

initialization
  TestGlobal.RegisterTestCase(TFrha_MD5);


end.
