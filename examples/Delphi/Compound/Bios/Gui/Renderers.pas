unit Renderers;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  stdCtrls,
  BoldCheckboxStateControlPack,
  BoldSubscription,
  BoldElements,
  BoldControlPack,
  BoldStringControlPack,
  BuildingsAndOwners;

type
  TDataModule2 = class(TDataModule)
    NegativeRedRenderer: TBoldAsStringRenderer;
    FullNameRenderer: TBoldAsStringRenderer;
    IsRichRenderer: TBoldAsCheckBoxStateRenderer;
    bsrAddress: TBoldAsStringRenderer;
    bsrResidentsTotalAssets: TBoldAsStringRenderer;
    bsrRentPerResident: TBoldAsStringRenderer;
    function IsRichRendererGetAsCheckBoxState(Element: TBoldElement;
      Representation: Integer; Expression: String): TCheckBoxState;
    procedure IsRichRendererSubscribe(Element: TBoldElement;
      Representation: Integer; Expression: String;
      Subscriber: TBoldSubscriber);
    function FullNameRendererGetAsString(Element: TBoldElement;
      Representation: Integer; Expression: String): String;
    procedure FullNameRendererSubscribe(Element: TBoldElement;
      Representation: Integer; Expression: String;
      Subscriber: TBoldSubscriber);
    procedure NegativeRedRendererHoldsChangedValue(Element: TBoldElement;
      Representation: Integer; Expression: String;
      Subscriber: TBoldSubscriber);
    procedure NegativeRedRendererSetFont(Element: TBoldElement;
      AFont: TFont; Representation: Integer; Expression: String);
    procedure bsrAddressSetColor(Element: TBoldElement; var AColor: TColor;
      Representation: Integer; Expression: String);
    procedure bsrAddressSetFont(Element: TBoldElement; AFont: TFont;
      Representation: Integer; Expression: String);
    function bsrResidentsTotalAssetsGetAsString(Element: TBoldElement;
      Representation: Integer; Expression: String): String;
    procedure bsrResidentsTotalAssetsSubscribe(Element: TBoldElement;
      Representation: Integer; Expression: String;
      Subscriber: TBoldSubscriber);
    function bsrRentPerResidentGetAsString(Element: TBoldElement;
      Representation: Integer; Expression: String): String;
    procedure bsrRentPerResidentHoldsChangedValue(Element: TBoldElement;
      Representation: Integer; Expression: String;
      Subscriber: TBoldSubscriber);
    function bsrRentPerResidentMayModify(Element: TBoldElement;
      Representation: Integer; Expression: String;
      Subscriber: TBoldSubscriber): Boolean;
    procedure bsrRentPerResidentReleaseChangedValue(Element: TBoldElement;
      Representation: Integer; Expression: String;
      Subscriber: TBoldSubscriber);
    procedure bsrRentPerResidentSetAsString(Element: TBoldElement;
      NewValue: String; Representation: Integer; Expression: String);
    procedure bsrRentPerResidentSubscribe(Element: TBoldElement;
      Representation: Integer; Expression: String;
      Subscriber: TBoldSubscriber);
    function bsrRentPerResidentValidateCharacter(Element: TBoldElement;
      Value: String; Representation: Integer; Expression: String): Boolean;
    function bsrRentPerResidentValidateString(Element: TBoldElement;
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
  Element: TBoldElement; Representation: Integer;
  Expression: String): TCheckBoxState;
begin
  Result := cbGrayed;
  if Assigned(Element) then
  begin
    with element as TPerson do
      if Assets > 10000 then
         Result := cbChecked
      else
         Result := cbUnChecked
  end;
end;

procedure TDataModule2.IsRichRendererSubscribe(Element: TBoldElement;
  Representation: Integer; Expression: String;
  Subscriber: TBoldSubscriber);
begin
  Element.SubscribeToExpression('assets', Subscriber, False);
end;

function TDataModule2.FullNameRendererGetAsString(Element: TBoldElement;
  Representation: Integer; Expression: String): String;
begin
  Result := '';
  if Assigned(Element) then
  begin
    with Element as TPerson do
      Result := Format('%s, %s', [LastName, FirstName])
  end;
end;

procedure TDataModule2.FullNameRendererSubscribe(Element: TBoldElement;
  Representation: Integer; Expression: String;
  Subscriber: TBoldSubscriber);
begin
  Element.SubscribeToExpression('firstName', Subscriber, false);  Element.SubscribeToExpression('', Subscriber, False);
  Element.SubscribeToExpression('lastName', Subscriber, false);
end;

procedure TDataModule2.NegativeRedRendererHoldsChangedValue(
  Element: TBoldElement; Representation: Integer; Expression: String;
  Subscriber: TBoldSubscriber);
begin
  if assigned( element ) then
    with NegativeRedRenderer do
      DefaultHoldsChangedValue(element, representation, Expression, nil, subscriber);
end;

procedure TDataModule2.NegativeRedRendererSetFont(Element: TBoldElement;
  AFont: TFont; Representation: Integer; Expression: String);
begin
  if assigned( element ) then
  begin
    with element as TPerson do
      if Assets < 0 then
        aFont.Color :=  clRed
      else
        aFont.Color := clBlue;
  end;
end;

procedure TDataModule2.bsrAddressSetColor(Element: TBoldElement;
  var AColor: TColor; Representation: Integer; Expression: String);
begin
  if Assigned(element) then
    with element as TBuilding do
    begin
      if Pos('Bold', Address) > 0 then
        aColor := clAqua;
    end;

end;

procedure TDataModule2.bsrAddressSetFont(Element: TBoldElement;
  AFont: TFont; Representation: Integer; Expression: String);
begin
  if Assigned(element) then
    with element as TBuilding do
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
  Element: TBoldElement; Representation: Integer;
  Expression: String): String;
var
  i: integer;
  sum: Currency;
begin
  Sum := 0;
  if element is TResidential_Building then
    with Element as TResidential_Building do
      for i := 0 to Residents.Count-1 do
        Sum := Sum + Residents[i].Assets;
  Result := CurrToStr(Sum);
end;

procedure TDataModule2.bsrResidentsTotalAssetsSubscribe(
  Element: TBoldElement; Representation: Integer; Expression: String;
  Subscriber: TBoldSubscriber);
var
  i: integer;
begin
  if element is TResidential_Building then
    with Element as TResidential_Building do
    begin
      SubscribeToExpression('residents', subscriber, true);
      for i := 0 to Residents.Count-1 do
        Residents[i].SubscribeToExpression('assets', subscriber, false);
    end;
end;

function TDataModule2.bsrRentPerResidentGetAsString(Element: TBoldElement;
  Representation: Integer; Expression: String): String;
begin
  result := '';
  if element is TResidential_Building then
    with Element as TResidential_Building do
      if M_TotalRent.IsNull then
        Result := '***'
      else if Residents.Count = 0 then
        Result := 'No Residents'
      else
        Result := CurrToStr(TotalRent/Residents.Count);
end;

procedure TDataModule2.bsrRentPerResidentHoldsChangedValue(
  Element: TBoldElement; Representation: Integer; Expression: String;
  Subscriber: TBoldSubscriber);
begin
  if element is TResidential_Building then
    with Element as TResidential_Building do
      M_TotalRent.RegisterModifiedValueHolder(subscriber);
end;

function TDataModule2.bsrRentPerResidentMayModify(Element: TBoldElement;
  Representation: Integer; Expression: String;
  Subscriber: TBoldSubscriber): Boolean;
begin
  Result := False;
  if element is TResidential_Building then
    with element as TResidential_Building do
      Result := Residents.Count > 0;
end;

procedure TDataModule2.bsrRentPerResidentReleaseChangedValue(
  Element: TBoldElement; Representation: Integer; Expression: String;
  Subscriber: TBoldSubscriber);
begin
  if element is TResidential_Building then
    with Element as TResidential_Building do
      M_TotalRent.UnRegisterModifiedValueHolder(Subscriber);
end;

procedure TDataModule2.bsrRentPerResidentSetAsString(Element: TBoldElement;
  NewValue: String; Representation: Integer; Expression: String);
begin
  if element is TResidential_Building then
    with Element as TResidential_Building do
      TotalRent := StrToCurr(NewValue) * Residents.Count;
end;

procedure TDataModule2.bsrRentPerResidentSubscribe(Element: TBoldElement;
  Representation: Integer; Expression: String;
  Subscriber: TBoldSubscriber);
begin
  if element is TResidential_Building then
    with Element do
    begin
      SubscribeToExpression('totalRent', Subscriber, False);
      SubscribeToExpression('residents', Subscriber, False);
    end;
end;

function TDataModule2.bsrRentPerResidentValidateCharacter(
  Element: TBoldElement; Value: String; Representation: Integer;
  Expression: String): Boolean;
begin
  Result := value[1] in ['0'..'9', '-', '+', 'e', 'E', DecimalSeparator];
end;

function TDataModule2.bsrRentPerResidentValidateString(
  Element: TBoldElement; Value: String; Representation: Integer;
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
