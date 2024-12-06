
{ Global compiler directives }
{$include bold.inc}
unit BoldStringControlPackCom;

{$DEFINE BOLDCOMCLIENT} {Clientified 2002-08-05 13:13:02}

interface

uses
  // VCL
  Classes,
  Graphics,
  Windows,

  // Bold
  BoldClientElementSupport,
  BoldComClient,
  BoldComObjectSpace_TLB,
  BoldContainers,
  BoldControlPackCom,
  BoldDefs;

type
  {Forward declaration of classes}
  TBoldStringFollowerControllerCom = class;
  TBoldAsStringRendererCom = class;
  TBoldStringRendererDataCom = class;

  { TBoldAsStringRendererCom prototypes }
  TBoldGetAsStringCom = function (Element: IBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression): string of object;
  TBoldSetAsStringCom = procedure (Element: IBoldElement; NewValue: string; Representation: TBoldRepresentation; Expression: TBoldExpression) of object;
  TBoldValidateStringCom = function (Element: IBoldElement; Value: string; Representation: TBoldRepresentation; Expression: TBoldExpression): Boolean of object;
  TBoldSetFontCom = procedure (Element: IBoldElement; AFont: TFont; Representation: TBoldRepresentation; Expression: TBoldExpression) of object;
  TBoldSetColorCom = procedure (Element: IBoldElement; var AColor: TColor; Representation: TBoldRepresentation; Expression: TBoldExpression) of object;
  TBoldStringIsChangedCom = function (RendererData: TBoldStringRendererDataCom; NewValue: string; Representation: TBoldRepresentation; Expression: TBoldExpression): Boolean of object;

  { TBoldStringRendererDataCom }
  TBoldStringRendererDataCom = class(TBoldFollowerDataCom)
  private
    fOldStringValue: string;
    fCurrentStringValue: string;
    fMaxStringLength: integer;
  public
    constructor Create(OwningFollower: TBoldFollowerCom); override;
    property OldStringValue: string read fOldStringValue write fOldStringValue;
    property CurrentStringValue: string read fCurrentStringValue write fCurrentStringValue;
    property MaxStringLength: integer read fMaxStringLength write fMaxStringLength;
  end;

  { TBoldAsStringRendererCom }
  TBoldAsStringRendererCom = class(TBoldSingleRendererCom)
  private
    FOnGetAsString: TBoldGetAsStringCom;
    FOnSetAsString: TBoldSetAsStringCom;
    FOnValidateCharacter: TBoldValidateStringCom;
    FOnValidateString: TBoldValidateStringCom;
    fOnSetFont: TBoldSetFontCom;
    fOnSetColor: TBoldSetColorCom;
    fOnIsChanged: TBoldStringIsChangedCom;
  protected
    function GetSupportsMulti: Boolean; override;
    function GetRendererDataClass: TBoldRendererDataClassCom; override;
    function GetAsStringAndSubscribe(Element: IBoldElement; FollowerController: TBoldStringFollowerControllerCom; Subscriber: TBoldComClientSubscriber): string; virtual;
    procedure SetAsString(Element: IBoldElement; Value: string; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList); virtual;
    procedure DrawOnCanvas(Follower: TBoldFollowerCom; Canvas: TCanvas; Rect: TRect; Alignment: TAlignment; Margins: TPoint); override;
  public
    class function DefaultRenderer: TBoldAsStringRendererCom;
    class procedure DrawStringOnCanvas(Canvas: TCanvas; Rect: TRect; Alignment: TAlignment; Margins: TPoint; S: string);
    function DefaultGetAsStringAndSubscribe(Element: IBoldElement; FollowerController: TBoldStringFollowerControllerCom; Subscriber: TBoldComClientSubscriber): string; virtual;
    procedure DefaultSetAsString(Element: IBoldElement; Value: string; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList); virtual;
    function DefaultValidateCharacter(Element: IBoldElement; C: AnsiChar; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList): Boolean; virtual;
    function DefaultValidateString(Element: IBoldElement; Value: string; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList): Boolean; virtual;
    function DefaultIsChanged(RendererData: TBoldStringRendererDataCom; NewValue: string; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList): Boolean;
    function ValidateCharacter(Element: IBoldElement; C: AnsiChar; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList): Boolean; virtual;
    function ValidateString(Element: IBoldElement; Value: string; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList): Boolean; virtual;
    function IsChanged(RendererData: TBoldStringRendererDataCom; NewValue: string; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList): Boolean;
    procedure SetFont(Element: IBoldElement; EffectiveFont, Font: TFont; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList);
    procedure SetColor(Element: IBoldElement; var EffectiveColor: TColor; Color: TColor; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList);
    procedure MakeUptoDateAndSubscribe(Element: IBoldElement; RendererData: TBoldFollowerDataCom; FollowerController: TBoldFollowerControllerCom; Subscriber: TBoldComClientSubscriber); override;
    procedure MultiMakeUpToDateAndSubscribe(Elements: TBoldClientableListCom; Subscribers: TBoldObjectArray; RendererData: TBoldObjectArray; FollowerController: TBoldFollowerControllerCom);
    procedure DefaultMakeUptodateAndSetMayModifyAndSubscribe(Element: IBoldElement; RendererData: TBoldFollowerDataCom; FollowerController: TBoldStringFollowerControllerCom; Subscriber: TBoldComClientSubscriber); virtual;
  published
    property OnGetAsString: TBoldGetAsStringCom read FOnGetAsString write FOnGetAsString;
    property OnSetAsString: TBoldSetAsStringCom read FOnSetAsString write FOnSetAsString;
    property OnValidateCharacter: TBoldValidateStringCom read FOnValidateCharacter write FOnValidateCharacter;
    property OnValidateString: TBoldValidateStringCom read FOnValidateString write FOnValidateString;
    property OnSetFont: TBoldSetFontCom read fOnSetFont write fOnSetFont;
    property OnSetColor: TBoldSetColorCom read fOnSetColor write fOnSetColor;
    property OnIsChanged: TBoldStringIsChangedCom read fOnIsChanged write fOnIsChanged;
  end;

  { TBoldStringFollowerControllerCom }
  TBoldStringFollowerControllerCom = class(TBoldSingleFollowerControllerCom)
  private
    FNilStringRepresentation: string;
    function GetRenderer: TBoldAsStringRendererCom;
    procedure SetRenderer(Value: TBoldAsStringRendererCom);
    function GetEffectiveAsStringRenderer: TBoldAsStringRendererCom;
    procedure SetNilStringRepresentation(const Value: string);
  protected
    function GetSupportsMultiEnsure: Boolean; override;
    function GetEffectiveRenderer: TBoldRendererCom; override;
    property EffectiveAsStringRenderer: TBoldAsStringRendererCom read GetEffectiveAsStringRenderer;
    procedure DoMultiMakeUptodateAndSubscribe(Followers: TBoldObjectArray); override;
  public
    procedure MakeClean(Follower: TBoldFollowerCom); override;
    function GetCurrentAsString(Follower: TBoldFollowerCom): string;
    procedure SetAsString(Value: string; Follower: TBoldFollowerCom);
    function ValidateCharacter(C: AnsiChar; Follower: TBoldFollowerCom): Boolean;
    function ValidateString(const Value: string; Follower: TBoldFollowerCom): Boolean;
    procedure SetFont(EffectiveFont, Font: tFont; Follower: TBoldFollowerCom);
    procedure SetColor(var EffectiveColor: tColor; COLOR: tColor; Follower: TBoldFollowerCom);
    procedure MayHaveChanged(NewValue: string; Follower: TBoldFollowerCom);
    procedure DoMakeUptodateAndSubscribe(Follower: TBoldFollowerCom; Subscribe: Boolean); override;
    function GetAsString(Element: IBoldElement): string;
  published
    property Renderer: TBoldAsStringRendererCom read GetRenderer write SetRenderer;
    property NilStringRepresentation: string read FNilStringRepresentation write SetNilStringRepresentation;
  end;

implementation

uses
  SysUtils,
  BoldSubscription,
  BoldControlPackDefs,
  {$IFNDEF BOLDCOMCLIENT}
  BoldComObjectSpace_TLB,
  BoldDomainElement
  {$ELSE}
  Variants
  {$ENDIF};

var
  DefaultAsStringRenderer: TBoldAsStringRendererCom;

{---TBoldStringFollowerControllerCom---}

function TBoldStringFollowerControllerCom.GetRenderer: TBoldAsStringRendererCom;
begin
  Result := UntypedRenderer as TBoldAsStringRendererCom;
end;

procedure TBoldStringFollowerControllerCom.SetRenderer(Value: TBoldAsStringRendererCom);
begin
  UntypedRenderer := Value;
end;

function TBoldStringFollowerControllerCom.GetEffectiveRenderer: TBoldRendererCom;
begin
  Result := EffectiveAsStringRenderer;
end;

function TBoldStringFollowerControllerCom.GetEffectiveAsStringRenderer: TBoldAsStringRendererCom;
begin
  if Assigned(Renderer) then
    Result := Renderer
  else
    Result := TBoldAsStringRendererCom.DefaultRenderer;
end;

function TBoldStringFollowerControllerCom.GetCurrentAsString(Follower: TBoldFollowerCom): string;
begin
  Result := (Follower.RendererData as TBoldStringRendererDataCom).CurrentStringValue;
end;

procedure TBoldStringFollowerControllerCom.SetAsString(Value: string; Follower: TBoldFollowerCom);
begin
  EffectiveAsStringRenderer.SetAsString(Follower.Element, Value, Representation, Expression, VariableList);
end;

function TBoldStringFollowerControllerCom.ValidateCharacter(C: AnsiChar; Follower: TBoldFollowerCom): Boolean;
begin
  Result := EffectiveAsStringRenderer.ValidateCharacter(Follower.Element, C, Representation, Expression, VariableList);
end;

function TBoldStringFollowerControllerCom.ValidateString(const Value: string; Follower: TBoldFollowerCom): Boolean;
begin
  Result := EffectiveAsStringRenderer.ValidateString(Follower.Element, Value, Representation, Expression, VariableList);
end;

procedure TBoldStringFollowerControllerCom.SetFont(EffectiveFont, Font: tFont; Follower: TBoldFollowerCom);
begin
  EffectiveAsStringRenderer.SetFont(Follower.Element, EffectiveFont, Font, Representation, Expression, VariableList);
end;

procedure TBoldStringFollowerControllerCom.SetColor(var EffectiveColor: tColor; COLOR: tColor; Follower: TBoldFollowerCom);
begin
  EffectiveAsStringRenderer.SetColor(Follower.Element, EffectiveColor, COLOR, Representation, Expression, VariableList);
end;

procedure TBoldStringFollowerControllerCom.MayHaveChanged(NewValue: string; Follower: TBoldFollowerCom);
begin
  if Follower.State in bfsDisplayable then
  begin
    (Follower.RendererData as TBoldStringRendererDataCom).CurrentStringValue := NewValue;
    Follower.ControlledValueChanged(EffectiveAsStringRenderer.IsChanged(Follower.RendererData as TBoldStringRendererDataCom, NewValue, Representation, Expression, VariableList));
  end;
end;

procedure TBoldStringFollowerControllerCom.MakeClean(Follower: TBoldFollowerCom);
{$IFNDEF BOLDCOMCLIENT}
var
  el: TBoldDomainElement;
  FailureReason: TBoldFailureReason;
{$ENDIF}
begin
  if ValidateString(GetCurrentAsString(Follower), Follower) then
  begin
    ReleaseChangedValue(Follower);
    SetAsString(GetCurrentAsString(Follower), Follower);
  end
  else
  begin
    {$IFNDEF BOLDCOMCLIENT}
    if follower.Element is TBoldDomainElement then
      el := follower.Element as TBoldDomainElement
    else
      el := nil;
    FailureReason := GetBoldLastFailureReason;
    if assigned(FailureReason) then
      GetBoldLastFailureReason.MessageFormatStr := 'String validation failed for %s: %2:s';
    BoldRaiseLastFailure(el, '', 'Unknown reason');
    {$ELSE}
    raise EBold.Create('String validation failed');
    {$ENDIF}
  end;
end;

procedure TBoldStringFollowerControllerCom.DoMakeUptodateAndSubscribe(Follower: TBoldFollowerCom; Subscribe: Boolean);
var
  renderer: TBoldAsStringRendererCom;
  Subscriber: TBoldComClientSubscriber;
begin
  If Subscribe then
    Subscriber := Follower.Subscriber
  else
    Subscriber := nil;
  Renderer := EffectiveRenderer as TBoldAsStringRendererCom;
  if Assigned(Renderer.OnGetAsString) or Assigned(Renderer.OnSubscribe) or Assigned(Renderer.OnMayModify) then   begin
    Renderer.MakeUptodateAndSubscribe(Follower.Element, Follower.RendererData, Self, Subscriber);
    Follower.RendererData.MayModify := Renderer.MayModify(Follower.Element, Representation, Expression, GetVariableListAndSubscribe(follower.Subscriber), Follower.Subscriber);
  end else
    renderer.DefaultMakeUptodateAndSetMayModifyAndSubscribe(Follower.Element, Follower.RendererData, Self, Subscriber);
end;

procedure TBoldStringFollowerControllerCom.SetNilStringRepresentation(const Value: string);
begin
  if (FNilStringRepresentation<>Value) then
  begin
    FNilStringRepresentation := Value;
    Changed;
  end;
end;

{---TBoldAsStringRendererCom---}
  class procedure TBoldAsStringRendererCom.DrawStringOnCanvas(Canvas: TCanvas; Rect: TRect; Alignment: TAlignment; Margins: TPoint; S: string);
var
  Left: Integer;
begin
  case Alignment of
    taLeftJustify: Left := Margins.X + Rect.Left;
    taRightJustify: Left := (Rect.Right - Rect.Left) - Canvas.TextWidth(S) + Rect.Left - 1 - Margins.X;
  else
    Left := Rect.Left + ((Rect.Right - Rect.Left) - Canvas.TextWidth(S)) div 2;
  end;
  Canvas.TextRect(Rect, Left, Rect.Top + Margins.Y, S);
end;

procedure TBoldAsStringRendererCom.DrawOnCanvas(Follower: TBoldFollowerCom; Canvas: TCanvas; Rect: TRect; Alignment: TAlignment; Margins: TPoint);
begin
  DrawStringOnCanvas(Canvas, Rect, Alignment, Margins, TBoldStringRendererDataCom(Follower.RendererData).CurrentStringValue);
end;

procedure TBoldAsStringRendererCom.MakeUptoDateAndSubscribe(Element: IBoldElement; RendererData: TBoldFollowerDataCom; FollowerController: TBoldFollowerControllerCom; Subscriber: TBoldComClientSubscriber);
var
  S: string;
begin
  S := GetAsStringAndSubscribe(Element, FollowerController as TBoldStringFollowerControllerCom, Subscriber);
  (RendererData as TBoldStringRendererDataCom).OldStringValue := S;
  (RendererData as TBoldStringRendererDataCom).CurrentStringValue := S;
end;

procedure TBoldAsStringRendererCom.DefaultMakeUptodateAndSetMayModifyAndSubscribe(Element: IBoldElement; RendererData: TBoldFollowerDataCom; FollowerController: TBoldStringFollowerControllerCom; Subscriber: TBoldComClientSubscriber);
var
  {$IFDEF BOLDCOMCLIENT}
  e: IBoldElement;
  {$ELSE}
  E: TBoldIndirectElement;
  {$ENDIF}
  S: String;
begin
  S := '';
  if (csDesigning in ComponentState) and (Self<>DefaultRenderer) then
  begin
    if Name<>'' then
      S := '('+Name+')'
    else
      S := '('+ClassName+')';
    RendererData.MayModify := False;
  end
  else
  begin
    if Assigned(Element) then
    begin
      {$IFDEF BOLDCOMCLIENT}
      if assigned(Subscriber) then
        e := Element.EvaluateAndSubscribeToExpression(FollowerController.Expression, Subscriber.ClientId, Subscriber.SubscriberId, False, false)
      else
        e := Element.EvaluateExpression(FollowerController.Expression);
        
      if Assigned(E) then
      begin
        S := E.StringRepresentation[FollowerController.Representation];
        if Assigned(Subscriber) then
          E.SubscribeToStringRepresentation(FollowerController.Representation, Subscriber.ClientId, Subscriber.SubscriberId, breReEvaluate, false);
        RendererData.MayModify := true;
      end
      else
        S := FollowerController.NilStringRepresentation
      {$ELSE}
      E := TBoldIndirectElement.Create;
      try
        Element.EvaluateAndSubscribeToExpression(FollowerController.Expression, Subscriber, E, False, False, FollowerController.GetVariableListAndSubscribe(Subscriber));
        if (e.Value is IBoldObjectReference) and not assigned((e.Value as IBoldObjectReference).BoldObject) then
        begin
          s := FollowerController.NilStringRepresentation;
          if Assigned(Subscriber) then
            E.Value.SubscribeToStringRepresentation(FollowerController.Representation, Subscriber, breReEvaluate);
          RendererData.MayModify := E.Value.ObserverMayModifyAsString(FollowerController.Representation, Subscriber);
          (RendererData as TBoldStringRendererDataCom).MaxStringLength := -1;
        end
        else if Assigned(E.Value) then
        begin
          S := E.Value.StringRepresentation[FollowerController.Representation];
          if Assigned(Subscriber) then
            E.Value.SubscribeToStringRepresentation(FollowerController.Representation, Subscriber, breReEvaluate);
          RendererData.MayModify := E.Value.ObserverMayModifyAsString(FollowerController.Representation, Subscriber);
          if (E.Value is TBoldAttribute) and assigned((E.Value as TBoldAttribute).BoldAttributeRTInfo) then
            (RendererData as TBoldStringRendererDataCom).MaxStringLength := (E.Value as TBoldAttribute).BoldAttributeRTInfo.Length
          else
            (RendererData as TBoldStringRendererDataCom).MaxStringLength := -1;
        end
        else
          S := FollowerController.NilStringRepresentation
      finally
        E.Free;
      end;
      {$ENDIF}
    end
    else
      S := FollowerController.NilStringRepresentation
  end;
  (RendererData as TBoldStringRendererDataCom).OldStringValue := S;
  (RendererData as TBoldStringRendererDataCom).CurrentStringValue := S;
end;

function TBoldAsStringRendererCom.GetRendererDataClass: TBoldRendererDataClassCom;
begin
  Result := TBoldStringRendererDataCom;
end;

function TBoldAsStringRendererCom.DefaultGetAsStringAndSubscribe(Element: IBoldElement; FollowerController: TBoldStringFollowerControllerCom; Subscriber: TBoldComClientSubscriber): string;
var
  {$IFDEF BOLDCOMCLIENT}
  e: IBoldElement;
  {$ELSE}
  E: TBoldIndirectElement;
  {$ENDIF}
begin
  Result := '';
  if (csDesigning in ComponentState) and (Self<>DefaultRenderer) then
  begin
    if Name<>'' then
      Result := '('+Name+')'
    else
      Result := '('+ClassName+')';
  end
  else
  begin
    if Assigned(Element) then
    begin
      {$IFDEF BOLDCOMCLIENT}
      if assigned(Subscriber) then
        e := Element.EvaluateAndSubscribeToExpression(FollowerController.Expression, Subscriber.ClientId, Subscriber.SubscriberId, False, false)
      else
        e := Element.EvaluateExpression(FollowerController.Expression);
      if Assigned(E) then
      begin
        Result := E.StringRepresentation[FollowerController.Representation];
        if Assigned(Subscriber) then
          E.SubscribeToStringRepresentation(FollowerController.Representation, Subscriber.ClientId, Subscriber.SubscriberId, breReEvaluate, false);
      end
      else
        Result := FollowerController.NILStringRepresentation;
      {$ELSE}
      E := TBoldIndirectElement.Create;
      try
        Element.EvaluateAndSubscribeToExpression(FollowerController.Expression, Subscriber, E, False, False, FollowerController.GetVariableListAndSubscribe(Subscriber));
        if (e.Value is IBoldObjectReference) and not assigned((e.Value as IBoldObjectReference).BoldObject) then
        begin
          result := FollowerController.NilStringRepresentation;
          if Assigned(Subscriber) then
            E.Value.SubscribeToStringRepresentation(FollowerController.Representation, Subscriber, breReEvaluate);
        end
        else if Assigned(E.Value) then
        begin
          Result := E.Value.StringRepresentation[FollowerController.Representation];
          if Assigned(Subscriber) then
            E.Value.SubscribeToStringRepresentation(FollowerController.Representation, Subscriber, breReEvaluate);
        end
        else
          Result := FollowerController.NILStringRepresentation;
      finally
        E.Free;
      end;
      {$ENDIF}
    end
    else
      Result := FollowerController.NILStringRepresentation;
  end;
end;

procedure TBoldAsStringRendererCom.DefaultSetAsString(Element: IBoldElement; Value: string; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList);
var
  ValueElement: IBoldElement;
begin
  ValueElement := GetExpressionAsDirectElement(Element, Expression, VariableList);
  if Assigned(ValueElement) then
    ValueElement.StringRepresentation[Representation] := Value
  else
    raise EBold.Create('TBoldAsStringRendererCom.DefaultSetAsString: Can''t set string value');
end;

function TBoldAsStringRendererCom.DefaultValidateCharacter(Element: IBoldElement; C: AnsiChar; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList): Boolean;
var
  ValueElement: IBoldElement;
begin
  ValueElement := GetExpressionAsDirectElement(Element, Expression, VariableList);
  if Assigned(ValueElement) then
    Result := ValueElement.ValidateCharacter(C, Representation)
  else
    Result := False;
end;

function TBoldAsStringRendererCom.DefaultValidateString(Element: IBoldElement; Value: string; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList): Boolean;
var
  ValueElement: IBoldElement;
begin
  ValueElement := GetExpressionAsDirectElement(Element, Expression, VariableList);
  if Assigned(ValueElement) then
    Result := ValueElement.ValidateString(Value, Representation)
  else
    Result := False;
end;

function TBoldAsStringRendererCom.GetAsStringAndSubscribe(Element: IBoldElement; FollowerController: TBoldStringFollowerControllerCom; Subscriber: TBoldComClientSubscriber): string;
begin
  if Assigned(OnSubscribe) and Assigned(Subscriber) then
  begin
    if Assigned(Element) then
      OnSubscribe(Element, FollowerController.Representation, FollowerController.Expression, Subscriber);
    Subscriber := nil;
  end;
  if Assigned(OnGetAsString) then
    Result := OnGetAsString(Element, FollowerController.Representation, FollowerController.Expression)
  else
    Result := DefaultGetAsStringAndSubscribe(Element, FollowerController, Subscriber);
end;

procedure TBoldAsStringRendererCom.SetAsString(Element: IBoldElement; Value: string; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList);
begin
  if Assigned(FOnSetAsString) then
    OnSetAsString(Element, Value, Representation, Expression)
  else
    DefaultSetAsString(Element, Value, Representation, Expression, VariableList)
end;

function TBoldAsStringRendererCom.ValidateCharacter(Element: IBoldElement; C: AnsiChar; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList): Boolean;
begin
  if Assigned(FOnValidateCharacter) then
    Result := OnValidateCharacter(Element, C, Representation, Expression)
  else
    Result := DefaultValidateCharacter(Element, C, Representation, Expression, VariableList);
end;

function TBoldAsStringRendererCom.ValidateString(Element: IBoldElement; Value: string; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList): Boolean;
begin
  if Assigned(FOnValidateString) then
    Result := OnValidateString(Element, Value, Representation, Expression)
  else
    Result := DefaultValidateString(Element, Value, Representation, Expression, VariableList);
end;

procedure TBoldAsStringRendererCom.SetFont(Element: IBoldElement; EffectiveFont, Font: tFont; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList);
begin
  EffectiveFont.Assign(Font);
  if Assigned(fOnSetFont) then
    fOnSetFont(Element, EffectiveFont, Representation, Expression);
end;

procedure TBoldAsStringRendererCom.SetColor(Element: IBoldElement; var EffectiveColor: tColor; COLOR: tColor; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList);
begin
  EffectiveColor := COLOR;
  if Assigned(fOnSetColor) then
    fOnSetColor(Element, EffectiveColor, Representation, Expression);
end;

  class function TBoldAsStringRendererCom.DefaultRenderer: TBoldAsStringRendererCom;
begin
  Result := DefaultAsStringRenderer;
end;

function TBoldAsStringRendererCom.DefaultIsChanged(RendererData: TBoldStringRendererDataCom; NewValue: string; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList): Boolean;
begin
  Result := NewValue <> RendererData.OldStringValue;
end;

function TBoldAsStringRendererCom.IsChanged(RendererData: TBoldStringRendererDataCom; NewValue: string; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList): Boolean;
begin
  if Assigned(fOnIsChanged) then
    Result := fOnIsChanged(RendererData, NewValue, Representation, Expression)
  else
    Result := DefaultIsChanged(RendererData, NewValue, Representation, Expression, Variablelist);
end;

{ TBoldStringRendererDataCom }

constructor TBoldStringRendererDataCom.Create(OwningFollower: TBoldFollowerCom);
begin
  inherited;
  fMaxStringLength := -1;
end;

function TBoldAsStringRendererCom.GetSupportsMulti: Boolean;
begin
  {$IFDEF BOLDCOMCLIENT}
  Result := not  (Assigned(fOnGetAsString) or Assigned(OnSubscribe) or Assigned(OnMayModify));
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function TBoldStringFollowerControllerCom.GetSupportsMultiEnsure: Boolean;
begin
  {$IFDEF BOLDCOMCLIENT}
  Result := False;
  {$ELSE}
  Result := (EffectiveRenderer = DefaultAsStringRenderer) and
    (pos('+', expression) = 0);
  {$ENDIF}
end;

procedure TBoldStringFollowerControllerCom.DoMultiMakeUptodateAndSubscribe(
  Followers: TBoldObjectArray);
var
  Renderer: TBoldAsStringRendererCom;
  Elements: TBoldClientableListCom;
  TempFollower: TBoldFollowerCom;
  Subscribers: TBoldObjectArray;
  RendererData: TBoldObjectArray;
  I: integer;
  MaxIndex: integer;
begin
  Assert(SupportsMulti);
  MaxIndex := Followers.Count - 1;
  Renderer := EffectiveRenderer as TBoldAsStringRendererCom;
  Elements := TBoldClientableListCom.Create(MaxIndex,[]);
  Subscribers := TBoldObjectArray.Create(MaxIndex,[]);
  RendererData := TBoldObjectArray.Create(MaxIndex,[]);
  try
    for I := 0 to MaxIndex do
    begin
      TempFollower := TBoldFollowerCom(Followers[I]);
      Elements.Add(Tempfollower.Element);
      if TempFollower.State in bfdNeedResubscribe then
        Subscribers.Add(TempFollower.Subscriber)
      else
        Subscribers.Add(nil);
      RendererData.Add(TempFollower.RendererData);
    end;
    Renderer.MultiMakeUpToDateAndSubscribe(Elements, Subscribers, RendererData, Self);
  finally
    Elements.Free;
    Subscribers.Free;
    RendererData.Free;
  end;
end;

procedure TBoldAsStringRendererCom.MultiMakeUpToDateAndSubscribe(
  Elements: TBoldClientableListCom; Subscribers,
  RendererData: TBoldObjectArray;
  FollowerController: TBoldFollowerControllerCom);
  {$IFDEF BOLDCOMCLIENT}
var
  I: integer;
  ResultStrings: variant;
  SubscriberIds: variant;
  ElementsInt: variant;
  ClientId: string;
  TheElement: IBoldElement;
  unk: IUnknown;
begin
  SubscriberIds := VarArrayCreate([0, Elements.Count - 1], varInteger);
  ElementsInt := VarArrayCreate([0, Elements.Count - 1], varUnknown);
  ClientId := '';
  for I := 0 to  Elements.Count - 1 do
  begin
    ElementsInt[I] := Elements[I] as IUnknown;
    unk := Elements[0];
    TheElement := unk as IBoldElement;
    if Assigned(Subscribers[i]) then
    begin
      SubscriberIds[I] := TBoldComClientSubscriber(Subscribers[i]).SubscriberId;
      ClientId := TBoldComClientSubscriber(Subscribers[i]).ClientId;
    end
    else
      SubscriberIds[I] := 0;
  end;
  ResultStrings := TheElement.MultiEvaluateExpressionAsStringList(ElementsInt,
      TBoldSingleFollowerControllerCom(FollowerController).Expression, ClientId, SubscriberIds,
      TBoldSingleFollowerControllerCom(FollowerController).Representation);
  for I := 0 to  Elements.Count - 1 do
    with RendererData[i] as TBoldStringRendererDataCom do
    begin
       OldStringValue := ResultStrings[I];
       CurrentStringValue := ResultStrings[I];
    end;
  {$ELSE}
  begin
  {$ENDIF}
end;

function TBoldStringFollowerControllerCom.GetAsString(Element: IBoldElement): string;
begin
  result := EffectiveAsStringRenderer.GetAsStringAndSubscribe(element, self, nil);
end;

initialization
  DefaultAsStringRenderer := TBoldAsStringRendererCom.Create(nil);

finalization
  FreeAndNil(DefaultAsStringRenderer);

end.
