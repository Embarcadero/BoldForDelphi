
{ Global compiler directives }
{$include bold.inc}
unit BoldClientElementSupport;

interface
uses
  BoldSubscription,
  BoldComObjectSpace_TLB;

type


  TBoldElementCompareCom = function (Item1, Item2: IBoldElement): Integer of object;
  TBoldElementSubscribeCom = procedure (Element: IBoldElement; Subscriber: TBoldSubscriber) of object;
  TBoldElementFilterCom = function (Element: IBoldElement): Boolean of object;

  IBoldExternalVariableList = interface
  ['{14DA481F-16AC-480B-9950-242C79556F8F}']
  end;

  IBoldOCLComponentCom = interface
  ['{2F458CE1-DF22-11D3-A337-006008F62CFF}']
    function GetContextType: IBoldElementTypeInfo;
    procedure SetExpression(Expression: String);
    function GetExpression: String;
    function GetVariableList: IBoldExternalVariableList;
    property ContextType: IBoldElementTypeInfo read GetContextType;
    property Expression: String read GetExpression write SetExpression;
  end;

implementation

end.
