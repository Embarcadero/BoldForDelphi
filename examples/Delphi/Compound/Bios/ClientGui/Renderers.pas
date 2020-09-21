unit Renderers;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  stdCtrls,
  BoldCheckboxStateControlPackCom,
  BoldSubscription,
  BoldComObjectSpace_TLB, BoldClientElementSupport, BoldComClient,
  BoldControlPackCom,
  BoldStringControlPackCom,
  BuildingsAndOwners_TLB;

type
  TDataModule2 = class(TDataModule)
    NegativeRedRenderer: TBoldAsStringRendererCom;
    FullNameRenderer: TBoldAsStringRendererCom;
    IsRichRenderer: TBoldAsCheckBoxStateRendererCom;
    bsrAddress: TBoldAsStringRendererCom;
    bsrResidentsTotalAssets: TBoldAsStringRendererCom;
    bsrRentPerResident: TBoldAsStringRendererCom;
    function IsRichRendererGetAsCheckBoxState(Element: IBoldElement;
      Representation: Integer; Expression: String): TCheckBoxState;
    procedure IsRichRendererSubscribe(Element: IBoldElement;
      Representation: Integer; Expression: String;
      Subscriber: TBoldComClientSubscriber);
    function FullNameRendererGetAsString(Element: IBoldElement;
      Representation: Integer; Expression: String): String;
    procedure FullNameRendererSubscribe(Element: IBoldElement;
      Representation: Integer; Expression: String;
      Subscriber: TBoldComClientSubscriber);
    procedure NegativeRedRendererHoldsChangedValue(Element: IBoldElement;
      Representation: Integer; Expression: String;
      Subscriber: TBoldComClientSubscriber);
    procedure NegativeRedRendererSetFont(Element: IBoldElement;
      AFont: TFont; Representation: Integer; Expression: String);
    procedure bsrAddressSetColor(Element: IBoldElement; var AColor: TColor;
      Representation: Integer; Expression: String);
    procedure bsrAddressSetFont(Element: IBoldElement; AFont: TFont;
      Representation: Integer; Expression: String);
    function bsrResidentsTotalAssetsGetAsString(Element: IBoldElement;
      Representation: Integer; Expression: String): String;
    procedure bsrResidentsTotalAssetsSubscribe(Element: IBoldElement;
      Representation: Integer; Expression: String;
      Subscriber: TBoldComClientSubscriber);
    function bsrRentPerResidentGetAsString(Element: IBoldElement;
      Representation: Integer; Expression: String): String;
    procedure bsrRentPerResidentHoldsChangedValue(Element: IBoldElement;
      Representation: Integer; Expression: String;
      Subscriber: TBoldComClientSubscriber);
    function bsrRentPerResidentMayModify(Element: IBoldElement;
      Representation: Integer; Expression: String;
      Subscriber: TBoldComClientSubscriber): Boolean;
    procedure bsrRentPerResidentReleaseChangedValue(Element: IBoldElement;
      Representation: Integer; Expression: String;
      Subscriber: TBoldComClientSubscriber);
    procedure bsrRentPerResidentSetAsString(Element: IBoldElement;
      NewValue: String; Representation: Integer; Expression: String);
    procedure bsrRentPerResidentSubscribe(Element: IBoldElement;
      Representation: Integer; Expression: String;
      Subscriber: TBoldComClientSubscriber);
    function bsrRentPerResidentValidateCharacter(Element: IBoldElement;
      Value: String; Representation: Integer; Expression: String): Boolean;
    function bsrRentPerResidentValidateString(Element: IBoldElement;
      Value: String; Representation: Integer; Expression: String): Boolean;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DataModule2: TDataModule2;

implementation

{$R *.DFM}

function TDataModule2.IsRichRendererGetAsCheckBoxState(
  Element: IBoldElement; Representation: Integer;
  Expression: String): TCheckBoxState;
begin
  Result := cbGrayed;
  if Assigned(Element) then
  begin
    with element as IPerson do
      if Assets > 10000 then
         Result := cbChecked
      else
         Result := cbUnChecked
  end;
end;

procedure TDataModule2.IsRichRendererSubscribe(Element: IBoldElement;
  Representation: Integer; Expression: String;
  Subscriber: TBoldComClientSubscriber);
begin
  Element.SubscribeToExpression('assets', Subscriber.ClientId, Subscriber.SubscriberId, False, false);
end;

function TDataModule2.FullNameRendererGetAsString(Element: IBoldElement;
  Representation: Integer; Expression: String): String;
begin
  Result := '';
  if Assigned(Element) then
  begin
    with Element as IPerson do
      Result := Format('%s, %s', [LastName, FirstName])
  end;
end;

procedure TDataModule2.FullNameRendererSubscribe(Element: IBoldElement;
  Representation: Integer; Expression: String;
  Subscriber: TBoldComClientSubscriber);
begin
  Element.SubscribeToExpression('firstName', Subscriber.ClientId, Subscriber.SubscriberId, False, false);
  Element.SubscribeToExpression('lastName', Subscriber.ClientId, Subscriber.SubscriberId, False, false);
end;

procedure TDataModule2.NegativeRedRendererHoldsChangedValue(
  Element: IBoldElement; Representation: Integer; Expression: String;
  Subscriber: TBoldComClientSubscriber);
begin
  if assigned( element ) then
    with NegativeRedRenderer do
      DefaultHoldsChangedValue(element, representation, Expression, nil, subscriber);
end;

procedure TDataModule2.NegativeRedRendererSetFont(Element: IBoldElement;
  AFont: TFont; Representation: Integer; Expression: String);
begin
  if assigned( element ) then
  begin
    with element as IPerson do
      if Assets < 0 then
        aFont.Color :=  clRed
      else
        aFont.Color := clBlue;
  end;
end;

procedure TDataModule2.bsrAddressSetColor(Element: IBoldElement;
  var AColor: TColor; Representation: Integer; Expression: String);
begin
  if Assigned(element) then
    with element as IBuilding do
    begin
      if Pos('Bold', Address) > 0 then
        aColor := clAqua;
    end;
end;

procedure TDataModule2.bsrAddressSetFont(Element: IBoldElement;
  AFont: TFont; Representation: Integer; Expression: String);
begin
  if Assigned(element) then
    with element as IBuilding do
    begin
      if Pos( 'Bold', Address ) > 0 then
        aFont.Style := aFont.Style + [fsBold];
      if Pos( 'Rose', Address ) > 0 then
        aFont.Color := clRed;
      if Pos( 'Select', Address ) > 0 then
        aFont.Color := clGreen;
    end;           
end;

function TDataModule2.bsrResidentsTotalAssetsGetAsString(
  Element: IBoldElement; Representation: Integer;
  Expression: String): String;
var
  i: integer;
  sum: Currency;
begin
  Sum := 0;
  with Element as IResidential_Building do
    for i := 0 to Residents.Count-1 do
      Sum := Sum + (Residents.BoldObjects[i] as IPerson).Assets;
  Result := CurrToStr(Sum);
end;

procedure TDataModule2.bsrResidentsTotalAssetsSubscribe(
  Element: IBoldElement; Representation: Integer; Expression: String;
  Subscriber: TBoldComClientSubscriber);
var
  i: integer;
begin
  with Element as IResidential_Building do
  begin
    SubscribeToExpression('residents', Subscriber.ClientId, Subscriber.SubscriberId, true, false);
    for i := 0 to Residents.Count-1 do
      Residents.BoldObjects[i].SubscribeToExpression('assets', Subscriber.ClientId, Subscriber.SubscriberId, False, false);
  end;
end;

function TDataModule2.bsrRentPerResidentGetAsString(Element: IBoldElement;
  Representation: Integer; Expression: String): String;
begin
  result := '';
  with Element as IResidential_Building do
    if Residents.Count = 0 then
      Result := 'No Residents'
    else
      Result := CurrToStr(TotalRent/Residents.Count);
end;

procedure TDataModule2.bsrRentPerResidentHoldsChangedValue(
  Element: IBoldElement; Representation: Integer; Expression: String;
  Subscriber: TBoldComClientSubscriber);
begin
//  with Element as IResidential_Building do
//    M_TotalRent.RegisterModifiedValueHolder(subscriber);
end;

function TDataModule2.bsrRentPerResidentMayModify(Element: IBoldElement;
  Representation: Integer; Expression: String;
  Subscriber: TBoldComClientSubscriber): Boolean;
begin
  Result := False;
  with element as IResidential_Building do
    Result := Residents.Count > 0;
end;

procedure TDataModule2.bsrRentPerResidentReleaseChangedValue(
  Element: IBoldElement; Representation: Integer; Expression: String;
  Subscriber: TBoldComClientSubscriber);
begin
//  with Element as IResidential_Building do
//    M_TotalRent.UnRegisterModifiedValueHolder(Subscriber);
end;

procedure TDataModule2.bsrRentPerResidentSetAsString(Element: IBoldElement;
  NewValue: String; Representation: Integer; Expression: String);
begin
  with Element as IResidential_Building do
    TotalRent := StrToCurr(NewValue) * Residents.Count;
end;

procedure TDataModule2.bsrRentPerResidentSubscribe(Element: IBoldElement;
  Representation: Integer; Expression: String;
  Subscriber: TBoldComClientSubscriber);
begin
  Element.SubscribeToExpression('totalRent', Subscriber.ClientId, Subscriber.SubscriberId, False, false);
  Element.SubscribeToExpression('residents', Subscriber.ClientId, Subscriber.SubscriberId, False, false);
end;

function TDataModule2.bsrRentPerResidentValidateCharacter(
  Element: IBoldElement; Value: String; Representation: Integer;
  Expression: String): Boolean;
begin
  Result := value[1] in ['0'..'9', '-', '+', 'e', 'E', DecimalSeparator];
end;

function TDataModule2.bsrRentPerResidentValidateString(
  Element: IBoldElement; Value: String; Representation: Integer;
  Expression: String): Boolean;
begin
  try
    StrToCurr(value);
    Result := True;
  except
    Result := False;
  end;
end;

end.
