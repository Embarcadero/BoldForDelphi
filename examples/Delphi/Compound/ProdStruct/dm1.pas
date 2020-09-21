unit dm1;

interface
uses
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  BoldModel,
  BoldControlPack,
  BoldStringControlPack,
  BoldSubscription,
  BoldElements,
  Boldhandles,
  BoldSystemHandle, BoldHandle, BoldPersistenceHandle,
  BoldPersistenceHandleDB, BoldActions, ActnList,
  BoldHandleAction, BoldAbstractModel, DB, IBDatabase,
  BoldAbstractDatabaseAdapter, BoldDatabaseAdapterIB,
  BoldAbstractPersistenceHandleDB;

type
  TdmMain = class(TDataModule)
    BoldModel: TBoldModel;
    BoldSystem: TBoldSystemHandle;
    BoldProfitAsStringRenderer: TBoldAsStringRenderer;
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
    BoldDatabaseAdapterIB1: TBoldDatabaseAdapterIB;
    IBDatabase1: TIBDatabase;
    function BoldProfitAsStringRendererGetAsString(element: TBoldElement;
      representation: Integer; Expression: string): string;
    procedure BoldProfitAsStringRendererSubscribe(element: TBoldElement;
      representation: Integer; Expression: string;
      Subscriber: TBoldSubscriber);
    procedure BoldProfitAsStringRendererSetAsString(
      Element: TBoldElement; Value: String; Representation: Integer;
      Expression: String);
    function BoldProfitAsStringRendererMayModify(
      Element: TBoldElement; Representation: Integer;
      Expression: String; Subscriber: TBoldSubscriber): Boolean;
  private
    { Private declarations }
  public
    { Public declarations }
  end;
  
var
  dmMain: TdmMain;
  
implementation

uses ProdStructClasses;

{$R *.DFM}

procedure TdmMain.BoldProfitAsStringRendererSubscribe(element: TBoldElement;
    representation: Integer; Expression: string;
    Subscriber: TBoldSubscriber);
begin
  {You need to know when price and/or totalCost changes to correctly show the profit.
  Element is assumed to be a Product.
  The method SubscribeToExpression places subscriptions for the subscriber.
  The subscriber comes from whoever uses the AsStringRenderer.
  Resubscribe is false in most cases.}
  if assigned(element) then
  begin
    element.SubscribeToExpression('price', subscriber, false);
    element.SubscribeToExpression('totalCost', subscriber, false);
  end;
end;

function TdmMain.BoldProfitAsStringRendererGetAsString(
    element: TBoldElement; representation: Integer;
    Expression: string): string;
begin
  {This method is called whenever any of the elements you subscribe to changes.
  Element is assumed to be a Product.
  Calculate a string to show.}
  Result := '';
  if Assigned(Element) then
    with element as TProduct do
      if TotalCost <> 0 then
        Result := Format('%d%%',[Round((Price / TotalCost - 1) * 100)])
      else
        Result := 'n/a';
end;

procedure TdmMain.BoldProfitAsStringRendererSetAsString(
  Element: TBoldElement; Value: String; Representation: Integer;
  Expression: String);

var
  CleanedValue: string;

  procedure CleanValue;
  var
    I: Integer;
  begin
    {Remove spaces and %}
    I := 1;
    while I<=Length(Value) do
    begin
      if Value[I] in ['0'..'9', DecimalSeparator ] then
        CleanedValue := CleanedValue+Value[I];
      Inc(I)
    end;
  end;

begin
  {This method is called when you change the value in a control that uses this Renderer.}
  CleanValue;
  with Element as TProduct do
    if (CleanedValue<>'') then
      Price := TotalCost * (StrToFloat(CleanedValue)/100+1);
end;

function TdmMain.BoldProfitAsStringRendererMayModify(
  Element: TBoldElement; Representation: Integer; Expression: String;
  Subscriber: TBoldSubscriber): Boolean;
begin
  {This method is called at the same time as the GetAsString event to render the ReadOnly value.
  Result sets the ReadOnly property on the control using this Renderer.}
  Result := False;
  if Assigned(Element) then
    with element as TProduct do
      Result := TotalCost <> 0;
end;

end.

