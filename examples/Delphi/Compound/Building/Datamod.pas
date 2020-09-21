unit datamod;

interface

uses
//  dbLogDlg,
  SysUtils,
  Classes,
  Controls,
  Forms,
  stdctrls,
  Graphics,
  BoldSystem,
  BoldSystemRT,
  BoldAbstractModel,
  BoldModel,
  BoldSubscription,
  BoldElements,
  BoldHandle,
  BoldHandles,
  BoldSystemHandle,
  BoldPersistenceHandle,
  BoldPersistenceHandleDB,
  BoldTypeNameHandle,
  BoldSortedHandle,
  BoldFilteredHandle,
  BoldUMLModelLink,
  BoldUMLRose98Link,
  BoldControlPack,
  BoldCheckboxStateControlPack,
  BoldStringControlPack,
  BoldActions,
  BoldreferenceHandle,
  ActnList,
  BoldHandleAction, BoldAFPPluggable, BoldAbstractPersistenceHandleDB,
  DB, IBDatabase,
  BoldAbstractDatabaseAdapter, BoldDatabaseAdapterIB;

type
  TDataModule1 = class(TDataModule)
    BoldModel1: TBoldModel;
    IsRichRenderer: TBoldAsCheckBoxStateRenderer;
    IsRichFilter: TBoldFilter;
    NameComparer: TBoldComparer;
    NegativeRedRenderer: TBoldAsStringRenderer;
    BoldSystemHandle1: TBoldSystemHandle;
    BoldUMLRoseLink1: TBoldUMLRoseLink;
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
    BoldPlaceableAFP1: TBoldPlaceableAFP;
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
    IBDatabase1: TIBDatabase;
    BoldDatabaseAdapterIB1: TBoldDatabaseAdapterIB;
    procedure IsRichRendererSubscribe(Element: TBoldElement; Representation: Integer; Expression: String; Subscriber: TBoldSubscriber);
    function IsRichRendererGetAsCheckBoxState(element: TBoldElement; representation: Integer; Expression: String): TCheckBoxState;
    function NameComparerCompare(item1, item2: TBoldElement): Integer;
    procedure NameComparerSubscribe(boldElement: TBoldElement; subscriber: TBoldSubscriber);
    procedure NegativeRedRendererHoldsChangedValue(element: TBoldElement; representation: Integer; Expression: String; Subscriber: TBoldSubscriber);
    procedure NegativeRedRendererSetFont(element: TBoldElement; aFont: TFont; representation: Integer; Expression: String);
    function IsRichFilterFilter(element: TBoldElement): Boolean;
    procedure IsRichFilterSubscribe(element: TBoldElement; subscriber: TBoldSubscriber);
    procedure BoldPlaceableAFP1GetFormClass(Element: TBoldElement; var Result: TFormClass);
    procedure BoldPlaceableAFP1RetrieveHandle(Form: TForm;
      var Result: TBoldReferenceHandle);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DataModule1: TDataModule1;

implementation

uses
  BuildingClasses, PersonAutoFormUnit;

{$R *.DFM}

function TDataModule1.IsRichRendererGetAsCheckBoxState(
  element: TBoldElement; representation: Integer;
  Expression: String): TCheckBoxState;
begin
  result := cbGrayed;
  if element is TPerson then
  begin
    if TPerson(element).Assets > 10000 then
       Result := cbChecked
    else
       Result := cbUnChecked
  end;
end;

function TDataModule1.NameComparerCompare(item1, item2: TBoldElement): Integer;
begin
  Result := AnsiCompareText((Item1 as TPerson).LastName, (Item2 as TPerson).LastName);
  if Result = 0 then
    Result := AnsiCompareText((Item1 as TPerson).FirstName, (Item2 as TPerson).FirstName);
end;

procedure TDataModule1.NameComparerSubscribe(boldElement: TBoldElement; subscriber: TBoldSubscriber);
begin
  boldElement.SubscribeToExpression('firstName', Subscriber, false);
  boldElement.SubscribeToExpression('lastName', Subscriber, false);
end;

function TDataModule1.IsRichFilterFilter(element: TBoldElement): Boolean;
begin
  Result := False;
  if Element is TPerson then
    Result := TPerson(element).Assets > 10000;
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
  if Element is TPerson then
  begin
    if TPerson(Element).Assets < 0 then
      aFont.Color :=  clRed
    else
      aFont.Color := clBlue;
  end;
end;

procedure TDataModule1.IsRichFilterSubscribe(element: TBoldElement; subscriber: TBoldSubscriber);
begin
  Element.SubscribeToExpression('assets', Subscriber, False);
end;

procedure TDataModule1.IsRichRendererSubscribe(Element: TBoldElement;
  Representation: Integer; Expression: String;
  Subscriber: TBoldSubscriber);
begin
  Element.SubscribeToExpression('assets', Subscriber, False);
end;

procedure TDataModule1.BoldPlaceableAFP1GetFormClass(Element: TBoldElement; var Result: TFormClass);
begin
  if Element is TPerson then
    result := TPersonAutoForm
  else
    result := nil;
end;

procedure TDataModule1.BoldPlaceableAFP1RetrieveHandle(Form: TForm; var Result: TBoldReferenceHandle);
begin
  if form is TPersonAutoForm then
    result := TPersonAutoForm(form).brhPerson
  else
    result := nil;
end;

end.
