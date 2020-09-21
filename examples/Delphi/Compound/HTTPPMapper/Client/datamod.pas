unit datamod;

interface

uses
  SysUtils,
  Classes,
  Controls,
  Forms,
  DB,
  stdctrls,
  Graphics,
  BoldSystem,
  BoldSystemRT,
  BoldHandles,
  BoldModel,
  BoldControlPack,
  BoldCheckboxStateControlPack,
  BoldStringControlPack,
  BoldSubscription,
  BoldElements,
  BoldSystemHandle,
  BoldHandle,
  BoldPersistenceHandle,
  BoldTypeNameHandle,
  BoldBDEInterfaces,
  BoldPersistenceHandleDB,
  BoldSortedHandle,
  BoldFilteredHandle,
  BoldUMLModelLink,
  BoldUMLRose98Link,
  BoldActions,
  ActnList,
  BoldHandleAction, BoldAbstractModel, BoldHTTPClientPersistenceHandle,
  BoldWebConnection;

type
  TDataModule1 = class(TDataModule)
    FullNameRenderer: TBoldAsStringRenderer;
    IsRichRenderer: TBoldAsCheckBoxStateRenderer;
    IsRichFilter: TBoldFilter;
    NameComparer: TBoldComparer;
    NegativeRedRenderer: TBoldAsStringRenderer;
    BoldSystemHandle1: TBoldSystemHandle;
    BoldUMLRoseLink1: TBoldUMLRoseLink;
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
    BoldHTTPClientPersistenceHandle1: TBoldHTTPClientPersistenceHandle;
    BoldWebConnection1: TBoldWebConnection;
    function FullNameRendererGetAsString(Element: TBoldElement;
      representation: Integer; Expression: String): string;
    procedure FullNameRenderStubscribe(Element: TBoldElement;
      representation: Integer; Expression: String;
      subscriber: TBoldSubscriber);
    function IsRichRendererGetAsCheckBoxState(element: TBoldElement;
      representation: Integer; Expression: String): TCheckBoxState;
    function NameComparerCompare(item1,
      item2: TBoldElement): Integer;
    procedure NameComparerSubscribe(boldElement: TBoldElement;
      subscriber: TBoldSubscriber);
    function IsRichFilterFilter(element: TBoldElement): Boolean;
    procedure NegativeRedRendererHoldsChangedValue(
      element: TBoldElement; representation: Integer;
      Expression: String; Subscriber: TBoldSubscriber);
    procedure NegativeRedRendererSetFont(element: TBoldElement;
      aFont: TFont; representation: Integer; Expression: String);
    procedure IsRichFilterSubscribe(element: TBoldElement;
      subscriber: TBoldSubscriber);
    procedure IsRichRendererSubscribe(Element: TBoldElement;
      Representation: Integer; Expression: String;
      Subscriber: TBoldSubscriber);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DataModule1: TDataModule1;

implementation

uses BuildingClasses, dmCoreUnit;

{$R *.DFM}

function TDataModule1.FullNameRendererGetAsString(
  Element: TBoldElement; representation: Integer;
  Expression: String): string;
begin
  result := '';
  if assigned( element ) then
    with Element as TPerson do
      Result := Format('%s, %s', [LastName, FirstName])
end;

procedure TDataModule1.FullNameRenderStubscribe(Element: TBoldElement;
  representation: Integer; Expression: String; Subscriber: TBoldSubscriber);
begin
  Element.SubscribeToExpression('firstName', Subscriber, false);
  Element.SubscribeToExpression('lastName', Subscriber, false);
end;

function TDataModule1.IsRichRendererGetAsCheckBoxState(
  element: TBoldElement; representation: Integer;
  Expression: String): TCheckBoxState;
begin
  result := cbGrayed;
  if assigned(element) then
    with element as TPerson do
      if Assets > 10000 then
         Result := cbChecked
      else
         Result := cbUnChecked
end;

function TDataModule1.NameComparerCompare(item1,
  item2: TBoldElement): Integer;
begin
  Result := AnsiCompareText((Item1 as TPerson).LastName, (Item2 as TPerson).LastName);
  if Result = 0 then
    Result := AnsiCompareText((Item1 as TPerson).FirstName, (Item2 as TPerson).FirstName);
end;

procedure TDataModule1.NameComparerSubscribe(boldElement: TBoldElement;
  subscriber: TBoldSubscriber);
begin
  boldElement.SubscribeToExpression('firstName', Subscriber, false);
  boldElement.SubscribeToExpression('lastName', Subscriber, false);
end;

function TDataModule1.IsRichFilterFilter(
  element: TBoldElement): Boolean;
begin
  Result := False;
  if assigned( element ) then
    with element as TPerson do
      Result := Assets > 10000;
end;

procedure TDataModule1.NegativeRedRendererHoldsChangedValue(
  element: TBoldElement; representation: Integer;
  Expression: String; Subscriber: TBoldSubscriber);
begin
  if assigned(element) then
    with NegativeRedRenderer do
      DefaultHoldsChangedValue(element, representation, Expression, nil, subscriber);
end;

procedure TDataModule1.NegativeRedRendererSetFont(
  element: TBoldElement; aFont: TFont; representation: Integer;
  Expression: String);
begin
  if assigned(element) then
    with element as TPerson do
      if Assets < 0 then
        aFont.Color :=  clRed
      else
        aFont.Color := clBlue;
end;

procedure TDataModule1.IsRichFilterSubscribe(element: TBoldElement;
  subscriber: TBoldSubscriber);
begin
  Element.SubscribeToExpression('assets', Subscriber, False);
end;

procedure TDataModule1.IsRichRendererSubscribe(Element: TBoldElement;
  Representation: Integer; Expression: String;
  Subscriber: TBoldSubscriber);
begin
  Element.SubscribeToExpression('assets', Subscriber, False);
end;  

end.
