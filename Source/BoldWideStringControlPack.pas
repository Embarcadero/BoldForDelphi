
{ Global compiler directives }
{$include bold.inc}
unit BoldWideStringControlPack;

{$UNDEF BOLDCOMCLIENT}

interface

uses
  Graphics,
  Classes,
  Windows,
  BoldDefs,
  BoldControlPackDefs,
  BoldContainers,
  BoldElements,
  BoldAttributeWideString,
  BoldControlPack,
  BoldSubscription;

type
  {Forward declaration of classes}
  TBoldWideStringFollowerController = class;
  TBoldAsWideStringRenderer = class;
  TBoldWideStringRendererData = class;

  { TBoldAsWideStringRenderer prototypes }
  TBoldGetAsWideString = function (Element: TBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression): WideString of object;
  TBoldSetAsWideString = procedure (Element: TBoldElement; NewValue: WideString; Representation: TBoldRepresentation; Expression: TBoldExpression) of object;
  TBoldValidateWideString = function (Element: TBoldElement; Value: WideString; Representation: TBoldRepresentation; Expression: TBoldExpression): Boolean of object;
  TBoldSetFont = procedure (Element: TBoldElement; AFont: TFont; Representation: TBoldRepresentation; Expression: TBoldExpression) of object;
  TBoldSetColor = procedure (Element: TBoldElement; var AColor: TColor; Representation: TBoldRepresentation; Expression: TBoldExpression) of object;
  TBoldWideStringIsChanged = function (RendererData: TBoldWideStringRendererData; NewValue: WideString; Representation: TBoldRepresentation; Expression: TBoldExpression): Boolean of object;

  { TBoldWideStringRendererData }
  TBoldWideStringRendererData = class(TBoldRendererData)
  private
    fOldWideStringValue: WideString;
    fCurrentWideStringValue: WideString;
    fMaxWideStringLength: integer;
  public
    constructor Create(OwningFollower: TBoldFollower); override;
    property OldWideStringValue: WideString read fOldWideStringValue write fOldWideStringValue;
    property CurrentWideStringValue: WideString read fCurrentWideStringValue write fCurrentWideStringValue;
    property MaxWideStringLength: integer read fMaxWideStringLength write fMaxWideStringLength;
  end;

  { TBoldAsWideStringRenderer }
  TBoldAsWideStringRenderer = class(TBoldSingleRenderer)
  private
    FOnGetAsWideString: TBoldGetAsWideString;
    FOnSetAsWideString: TBoldSetAsWideString;
    FOnValidateCharacter: TBoldValidateWideString;
    FOnValidateWideString: TBoldValidateWideString;
    fOnSetFont: TBoldSetFont;
    fOnSetColor: TBoldSetColor;
    fOnIsChanged: TBoldWideStringIsChanged;
  protected
    function GetSupportsMulti: Boolean; override;
    function GetRendererDataClass: TBoldRendererDataClass; override;
    function GetAsWideStringAndSubscribe(Element: TBoldElement; FollowerController: TBoldWideStringFollowerController; Subscriber: TBoldSubscriber): WideString; virtual;
    procedure SetAsWideString(Element: TBoldElement; Value: WideString; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList); virtual;
    procedure DrawOnCanvas(Follower: TBoldFollower; Canvas: TCanvas; Rect: TRect; Alignment: TAlignment; Margins: TPoint); override;
    function HasSetValueEventOverrides: boolean; override;
  public
    class function DefaultRenderer: TBoldAsWideStringRenderer;
    class procedure DrawWideStringOnCanvas(Canvas: TCanvas; Rect: TRect; Alignment: TAlignment; Margins: TPoint; S: WideString);
    function DefaultGetAsWideStringAndSubscribe(Element: TBoldElement; FollowerController: TBoldWideStringFollowerController; Subscriber: TBoldSubscriber): WideString; virtual;
    procedure DefaultSetAsWideString(Element: TBoldElement; Value: WideString; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList); virtual;
    function DefaultValidateCharacter(Element: TBoldElement; C: AnsiChar; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList): Boolean; virtual;
    function DefaultValidateWideString(Element: TBoldElement; Value: WideString; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList): Boolean; virtual;
    function DefaultIsChanged(RendererData: TBoldWideStringRendererData; NewValue: WideString; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList): Boolean;
    function ValidateCharacter(Element: TBoldElement; C: AnsiChar; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList): Boolean; virtual;
    function ValidateWideString(Element: TBoldElement; Value: WideString; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList): Boolean; virtual;
    function IsChanged(RendererData: TBoldWideStringRendererData; NewValue: WideString; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList): Boolean;
    procedure SetFont(Element: TBoldElement; EffectiveFont, Font: TFont; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList);
    procedure SetColor(Element: TBoldElement; var EffectiveColor: TColor; Color: TColor; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList);
    procedure MakeUpToDateAndSubscribe(Element: TBoldElement; RendererData: TBoldRendererData; FollowerController: TBoldFollowerController; Subscriber: TBoldSubscriber); override;
    procedure MultiMakeUpToDateAndSubscribe(Elements: TBoldClientableList; Subscribers: TBoldObjectArray; RendererData: TBoldObjectArray; FollowerController: TBoldFollowerController);
    procedure DefaultMakeUptodateAndSetMayModifyAndSubscribe(Element: TBoldElement; RendererData: TBoldRendererData; FollowerController: TBoldWideStringFollowerController; Subscriber: TBoldSubscriber); virtual;
  published
    property OnGetAsWideString: TBoldGetAsWideString read FOnGetAsWideString write FOnGetAsWideString;
    property OnSetAsWideString: TBoldSetAsWideString read FOnSetAsWideString write FOnSetAsWideString;
    property OnValidateCharacter: TBoldValidateWideString read FOnValidateCharacter write FOnValidateCharacter;
    property OnValidateWideString: TBoldValidateWideString read FOnValidateWideString write FOnValidateWideString;
    property OnSetFont: TBoldSetFont read fOnSetFont write fOnSetFont;
    property OnSetColor: TBoldSetColor read fOnSetColor write fOnSetColor;
    property OnIsChanged: TBoldWideStringIsChanged read fOnIsChanged write fOnIsChanged;
  end;

  { TBoldWideStringFollowerController }
  TBoldWideStringFollowerController = class(TBoldSingleFollowerController)
  private
    FNilWideStringRepresentation: WideString;
    function GetRenderer: TBoldAsWideStringRenderer;
    procedure SetRenderer(Value: TBoldAsWideStringRenderer);
    function GetEffectiveAsWideStringRenderer: TBoldAsWideStringRenderer;
    procedure SetNilWideStringRepresentation(const Value: WideString);
  protected
    function GetSupportsMultiEnsure: Boolean; override;
    function GetEffectiveRenderer: TBoldRenderer; override;
    property EffectiveAsWideStringRenderer: TBoldAsWideStringRenderer read GetEffectiveAsWideStringRenderer;
    procedure DoMultiMakeUptodateAndSubscribe(Followers: TBoldFollowerArray); override;
  public
    procedure MakeClean(Follower: TBoldFollower); override;
    function GetCurrentAsWideString(Follower: TBoldFollower): WideString;
    procedure SetAsWideString(Value: WideString; Follower: TBoldFollower);
    function ValidateCharacter(C: AnsiChar; Follower: TBoldFollower): Boolean;
    function ValidateWideString(Value: WideString; Follower: TBoldFollower): Boolean;
    procedure SetFont(EffectiveFont, Font: tFont; Follower: TBoldFollower);
    procedure SetColor(var EffectiveColor: tColor; COLOR: tColor; Follower: TBoldFollower);
    procedure MayHaveChanged(NewValue: WideString; Follower: TBoldFollower);
    procedure DoMakeUptodateAndSubscribe(Follower: TBoldFollower; Subscribe: Boolean); override;
    function GetAsWideString(Element: TBoldElement): WideString;
  published
    property Renderer: TBoldAsWideStringRenderer read GetRenderer write SetRenderer;
    property NilWideStringRepresentation: WideString read FNilWideStringRepresentation write SetNilWideStringRepresentation;
  end;

implementation

uses
  SysUtils,
  {$IFNDEF BOLDCOMCLIENT}
  BoldSystem,
  {$ENDIF}
  Variants;

var
  DefaultAsWideStringRenderer: TBoldAsWideStringRenderer;

{---TBoldWideStringFollowerController---}

function TBoldWideStringFollowerController.GetRenderer: TBoldAsWideStringRenderer;
begin
  Result := UntypedRenderer as TBoldAsWideStringRenderer;
end;

procedure TBoldWideStringFollowerController.SetRenderer(Value: TBoldAsWideStringRenderer);
begin
  UntypedRenderer := Value;
end;

function TBoldWideStringFollowerController.GetEffectiveRenderer: TBoldRenderer;
begin
  Result := EffectiveAsWideStringRenderer;
end;

function TBoldWideStringFollowerController.GetEffectiveAsWideStringRenderer: TBoldAsWideStringRenderer;
begin
  if Assigned(Renderer) then
    Result := Renderer
  else
    Result := TBoldAsWideStringRenderer.DefaultRenderer;
end;

function TBoldWideStringFollowerController.GetCurrentAsWideString(Follower: TBoldFollower): WideString;
begin
  Result := (Follower.RendererData as TBoldWideStringRendererData).CurrentWideStringValue;
end;

procedure TBoldWideStringFollowerController.SetAsWideString(Value: WideString; Follower: TBoldFollower);
begin
  EffectiveAsWideStringRenderer.SetAsWideString(Follower.Element, Value, Representation, Expression, VariableList);
end;

function TBoldWideStringFollowerController.ValidateCharacter(C: AnsiChar; Follower: TBoldFollower): Boolean;
begin
  Result := EffectiveAsWideStringRenderer.ValidateCharacter(Follower.Element, C, Representation, Expression, VariableList);
end;

function TBoldWideStringFollowerController.ValidateWideString(Value: WideString; Follower: TBoldFollower): Boolean;
begin
  Result := EffectiveAsWideStringRenderer.ValidateWideString(Follower.Element, Value, Representation, Expression, VariableList);
end;

procedure TBoldWideStringFollowerController.SetFont(EffectiveFont, Font: tFont; Follower: TBoldFollower);
begin
  EffectiveAsWideStringRenderer.SetFont(Follower.Element, EffectiveFont, Font, Representation, Expression, VariableList);
end;

procedure TBoldWideStringFollowerController.SetColor(var EffectiveColor: tColor; COLOR: tColor; Follower: TBoldFollower);
begin
  EffectiveAsWideStringRenderer.SetColor(Follower.Element, EffectiveColor, COLOR, Representation, Expression, VariableList);
end;

procedure TBoldWideStringFollowerController.MayHaveChanged(NewValue: WideString; Follower: TBoldFollower);
begin
  if Follower.State in bfsDisplayable then
  begin
    (Follower.RendererData as TBoldWideStringRendererData).CurrentWideStringValue := NewValue;
    Follower.ControlledValueChanged(EffectiveAsWideStringRenderer.IsChanged(Follower.RendererData as TBoldWideStringRendererData, NewValue, Representation, Expression, VariableList));
  end;
end;

procedure TBoldWideStringFollowerController.MakeClean(Follower: TBoldFollower);
begin
  if ValidateWideString(GetCurrentAsWideString(Follower), Follower) then
  begin
    ReleaseChangedValue(Follower);
    SetAsWideString(GetCurrentAsWideString(Follower), Follower);
  end
  else
    raise EBold.Create('WideString validation failed')
end;

procedure TBoldWideStringFollowerController.DoMakeUptodateAndSubscribe(Follower: TBoldFollower; Subscribe: Boolean);
var
  renderer: TBoldAsWideStringRenderer;
  Subscriber: TBoldSubscriber;
begin
  If Subscribe then
    Subscriber := Follower.Subscriber
  else
    Subscriber := nil;
  Renderer := EffectiveRenderer as TBoldAsWideStringRenderer;
  if Assigned(Renderer.OnGetAsWideString) or Assigned(Renderer.OnSubscribe) or Assigned(Renderer.OnMayModify) then   begin
    Renderer.MakeUptodateAndSubscribe(Follower.Element, Follower.RendererData, Self, Subscriber);
  end else
    renderer.DefaultMakeUptodateAndSetMayModifyAndSubscribe(Follower.Element, Follower.RendererData, Self, Subscriber);
end;

procedure TBoldWideStringFollowerController.SetNilWideStringRepresentation(const Value: WideString);
begin
  if (FNilWideStringRepresentation<>Value) then
  begin
    FNilWideStringRepresentation := Value;
    Changed;
  end;
end;

{---TBoldAsWideStringRenderer---}
  class procedure TBoldAsWideStringRenderer.DrawWideStringOnCanvas(Canvas: TCanvas; Rect: TRect; Alignment: TAlignment; Margins: TPoint; S: WideString);
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

procedure TBoldAsWideStringRenderer.DrawOnCanvas(Follower: TBoldFollower; Canvas: TCanvas; Rect: TRect; Alignment: TAlignment; Margins: TPoint);
begin
  DrawWideStringOnCanvas(Canvas, Rect, Alignment, Margins, TBoldWideStringRendererData(Follower.RendererData).CurrentWideStringValue);
end;

procedure TBoldAsWideStringRenderer.MakeUpToDateAndSubscribe(Element: TBoldElement; RendererData: TBoldRendererData; FollowerController: TBoldFollowerController; Subscriber: TBoldSubscriber);
var
  S: WideString;
begin
  S := GetAsWideStringAndSubscribe(Element, FollowerController as TBoldWideStringFollowerController, Subscriber);
  (RendererData as TBoldWideStringRendererData).OldWideStringValue := S;
  (RendererData as TBoldWideStringRendererData).CurrentWideStringValue := S;
end;

procedure TBoldAsWideStringRenderer.DefaultMakeUptodateAndSetMayModifyAndSubscribe(Element: TBoldElement; RendererData: TBoldRendererData; FollowerController: TBoldWideStringFollowerController; Subscriber: TBoldSubscriber);
var
  {$IFDEF BOLDCOMCLIENT}
  e: IBoldElement;
  {$ELSE}
  E: TBoldIndirectElement;
  {$ENDIF}
  S: WideString;
begin
  S := '';
  if (csDesigning in ComponentState) and (Self<>DefaultRenderer) then
  begin
    if Name<>'' then
      S := '('+Name+')'
    else
      S := '('+ClassName+')';
  end
  else
  begin
    if Assigned(Element) then
    begin
      {$IFDEF BOLDCOMCLIENT}
      e := Element.EvaluateAndSubscribeToExpression(FollowerController.Expression, Subscriber.ClientId, Subscriber.SubscriberId, False, false);
      if Assigned(E) then
      begin
        S := E.WideStringRepresentation[FollowerController.Representation];
        if Assigned(Subscriber) then
          E.SubscribeToWideStringRepresentation(FollowerController.Representation, Subscriber.ClientId, Subscriber.SubscriberId, breReEvaluate, false);
      end
      else
        S := FollowerController.NilWideStringRepresentation
      {$ELSE}
      E := TBoldIndirectElement.Create;
      try
        Element.EvaluateAndSubscribeToExpression(FollowerController.Expression, Subscriber, E, False, False, FollowerController.GetVariableListAndSubscribe(Subscriber));
        if (e.Value is TBoldObjectReference) and not assigned((e.Value as TBoldObjectReference).BoldObject) then
        begin
          s := FollowerController.NilWideStringRepresentation;
          if Assigned(Subscriber) then
            E.Value.SubscribeToStringRepresentation(FollowerController.Representation, Subscriber, breReEvaluate);
          (RendererData as TBoldWideStringRendererData).MaxWideStringLength := -1;
        end
        else if Assigned(E.Value) then
        begin
          if e.value is TBAWideString then
            s := (e.value as TBAWideString).asWideString
          else
            S := E.Value.StringRepresentation[FollowerController.Representation];
          if Assigned(Subscriber) then
            E.Value.SubscribeToStringRepresentation(FollowerController.Representation, Subscriber, breReEvaluate);
          if (E.Value is TBoldAttribute) and assigned((E.Value as TBoldAttribute).BoldAttributeRTInfo) then
            (RendererData as TBoldWideStringRendererData).MaxWideStringLength := (E.Value as TBoldAttribute).BoldAttributeRTInfo.Length
          else
            (RendererData as TBoldWideStringRendererData).MaxWideStringLength := -1;
        end
        else
          S := FollowerController.NilWideStringRepresentation
      finally
        E.Free;
      end;
      {$ENDIF}
    end
    else
      S := FollowerController.NilWideStringRepresentation
  end;
  (RendererData as TBoldWideStringRendererData).OldWideStringValue := S;
  (RendererData as TBoldWideStringRendererData).CurrentWideStringValue := S;
end;

function TBoldAsWideStringRenderer.GetRendererDataClass: TBoldRendererDataClass;
begin
  Result := TBoldWideStringRendererData;
end;

function TBoldAsWideStringRenderer.DefaultGetAsWideStringAndSubscribe(Element: TBoldElement; FollowerController: TBoldWideStringFollowerController; Subscriber: TBoldSubscriber): WideString;
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
      e := Element.EvaluateAndSubscribeToExpression(FollowerController.Expression, Subscriber.ClientId, Subscriber.SubscriberId, False, false);
      if Assigned(E) then
      begin
        Result := E.WideStringRepresentation[FollowerController.Representation];
        if Assigned(Subscriber) then
          E.SubscribeToWideStringRepresentation(FollowerController.Representation, Subscriber.ClientId, Subscriber.SubscriberId, breReEvaluate, false);
      end
      else
        Result := FollowerController.NILWideStringRepresentation;
      {$ELSE}
      E := TBoldIndirectElement.Create;
      try
        Element.EvaluateAndSubscribeToExpression(FollowerController.Expression, Subscriber, E, False, False, FollowerController.GetVariableListAndSubscribe(Subscriber));
        if (e.Value is TBoldObjectReference) and not assigned((e.Value as TBoldObjectReference).BoldObject) then
        begin
          result := FollowerController.NilWideStringRepresentation;
          if Assigned(Subscriber) then
            E.Value.SubscribeToStringRepresentation(FollowerController.Representation, Subscriber, breReEvaluate);
        end
        else if Assigned(E.Value) then
        begin
          if e.value is TBAWideString then
            result := (e.value as TBAWideString).asWideString
          else
            result := E.Value.StringRepresentation[FollowerController.Representation];
          if Assigned(Subscriber) then
            E.Value.SubscribeToStringRepresentation(FollowerController.Representation, Subscriber, breReEvaluate);
        end
        else
          Result := FollowerController.NILWideStringRepresentation;
      finally
        E.Free;
      end;
      {$ENDIF}
    end
    else
      Result := FollowerController.NILWideStringRepresentation;
  end;
end;

procedure TBoldAsWideStringRenderer.DefaultSetAsWideString(Element: TBoldElement; Value: WideString; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList);
var
  ValueElement: TBoldElement;
begin
  ValueElement := GetExpressionAsDirectElement(Element, Expression, VariableList);
  if ValueElement is TBAWideString then
    (ValueElement as TBAWideString).asWideString := Value
  else
    raise EBold.Create('TBoldAsWideStringRenderer.DefaultSetAsWideString: Can''t set WideString value, target is not a TBAWideString');
end;

function TBoldAsWideStringRenderer.DefaultValidateCharacter(Element: TBoldElement; C: AnsiChar; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList): Boolean;
var
  ValueElement: TBoldElement;
begin
  ValueElement := GetExpressionAsDirectElement(Element, Expression, VariableList);
  if Assigned(ValueElement) then
    Result := ValueElement.ValidateCharacter(C, Representation)
  else
    Result := HasSetValueEventOverrides;
end;

function TBoldAsWideStringRenderer.DefaultValidateWideString(Element: TBoldElement; Value: WideString; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList): Boolean;
var
  ValueElement: TBoldElement;
begin
  ValueElement := GetExpressionAsDirectElement(Element, Expression, VariableList);
  if Assigned(ValueElement) then
  begin
    if ValueElement is TBAWideString then
      Result := (ValueElement as TBAWideString).ValidateWideString(Value, Representation)
    else
      Result := ValueElement.ValidateString(Value, Representation)
  end
  else
    Result := HasSetValueEventOverrides;
end;

function TBoldAsWideStringRenderer.GetAsWideStringAndSubscribe(Element: TBoldElement; FollowerController: TBoldWideStringFollowerController; Subscriber: TBoldSubscriber): WideString;
begin
  if Assigned(OnSubscribe) and Assigned(Subscriber) then
  begin
    if Assigned(Element) then
      OnSubscribe(Element, FollowerController.Representation, FollowerController.Expression, Subscriber);
    Subscriber := nil;
  end;
  if Assigned(OnGetAsWideString) then
    Result := OnGetAsWideString(Element, FollowerController.Representation, FollowerController.Expression)
  else
    Result := DefaultGetAsWideStringAndSubscribe(Element, FollowerController, Subscriber);
end;

procedure TBoldAsWideStringRenderer.SetAsWideString(Element: TBoldElement; Value: WideString; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList);
begin
  if Assigned(FOnSetAsWideString) then
    OnSetAsWideString(Element, Value, Representation, Expression)
  else
    DefaultSetAsWideString(Element, Value, Representation, Expression, VariableList)
end;

function TBoldAsWideStringRenderer.ValidateCharacter(Element: TBoldElement; C: AnsiChar; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList): Boolean;
begin
  if Assigned(FOnValidateCharacter) then
    Result := OnValidateCharacter(Element, C, Representation, Expression)
  else
    Result := DefaultValidateCharacter(Element, C, Representation, Expression, VariableList);
end;

function TBoldAsWideStringRenderer.ValidateWideString(Element: TBoldElement; Value: WideString; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList): Boolean;
begin
  if Assigned(FOnValidateWideString) then
    Result := OnValidateWideString(Element, Value, Representation, Expression)
  else
    Result := DefaultValidateWideString(Element, Value, Representation, Expression, VariableList);
end;

procedure TBoldAsWideStringRenderer.SetFont(Element: TBoldElement; EffectiveFont, Font: tFont; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList);
begin
  EffectiveFont.Assign(Font);
  if Assigned(fOnSetFont) then
    fOnSetFont(Element, EffectiveFont, Representation, Expression);
end;

procedure TBoldAsWideStringRenderer.SetColor(Element: TBoldElement; var EffectiveColor: tColor; COLOR: tColor; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList);
begin
  EffectiveColor := COLOR;
  if Assigned(fOnSetColor) then
    fOnSetColor(Element, EffectiveColor, Representation, Expression);
end;

  class function TBoldAsWideStringRenderer.DefaultRenderer: TBoldAsWideStringRenderer;
begin
  Result := DefaultAsWideStringRenderer;
end;

function TBoldAsWideStringRenderer.DefaultIsChanged(RendererData: TBoldWideStringRendererData; NewValue: WideString; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList): Boolean;
begin
  Result := NewValue <> RendererData.OldWideStringValue;
end;

function TBoldAsWideStringRenderer.IsChanged(RendererData: TBoldWideStringRendererData; NewValue: WideString; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList): Boolean;
begin
  if Assigned(fOnIsChanged) then
    Result := fOnIsChanged(RendererData, NewValue, Representation, Expression)
  else
    Result := DefaultIsChanged(RendererData, NewValue, Representation, Expression, Variablelist);
end;

{ TBoldWideStringRendererData }

constructor TBoldWideStringRendererData.Create(OwningFollower: TBoldFollower);
begin
  inherited;
  fMaxWideStringLength := -1;
end;

function TBoldAsWideStringRenderer.GetSupportsMulti: Boolean;
begin
  {$IFDEF BOLDCOMCLIENT}
  Result := not  (Assigned(fOnGetAsWideString) or Assigned(OnSubscribe) or Assigned(OnMayModify));
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function TBoldAsWideStringRenderer.HasSetValueEventOverrides: boolean;
begin
  result := Assigned(FOnSetAsWideString);
end;

function TBoldWideStringFollowerController.GetSupportsMultiEnsure: Boolean;
begin
  {$IFDEF BOLDCOMCLIENT}
  Result := False;
  {$ELSE}
  Result := (EffectiveRenderer = DefaultAsWideStringRenderer) and
    (pos('+', expression) = 0);
  {$ENDIF}
end;

procedure TBoldWideStringFollowerController.DoMultiMakeUptodateAndSubscribe(
  Followers: TBoldFollowerArray);
var
  Renderer: TBoldAsWideStringRenderer;
  Elements: TBoldClientableList;
  Subscribers: TBoldObjectArray;
  RendererData: TBoldObjectArray;
  F: TBoldFollower;
begin
  Assert(SupportsMulti);
  Renderer := EffectiveRenderer as TBoldAsWideStringRenderer;
  Elements := TBoldClientableList.Create(Length(Followers),[]);
  Subscribers := TBoldObjectArray.Create(Length(Followers),[]);
  RendererData := TBoldObjectArray.Create(Length(Followers),[]);
  try
    for F in Followers do
    begin
      Elements.Add(F.Element);
      if F.State in bfdNeedResubscribe then
        Subscribers.Add(F)
      else
        Subscribers.Add(nil);
      RendererData.Add(F.RendererData);
    end;
    Renderer.MultiMakeUpToDateAndSubscribe(Elements, Subscribers, RendererData, Self);
  finally
    Elements.Free;
    Subscribers.Free;
    RendererData.Free;
  end;
end;

procedure TBoldAsWideStringRenderer.MultiMakeUpToDateAndSubscribe(
  Elements: TBoldClientableList; Subscribers,
  RendererData: TBoldObjectArray;
  FollowerController: TBoldFollowerController);
  {$IFDEF BOLDCOMCLIENT}
var
  I: integer;
  ResultWideStrings: variant;
  SubscriberIds: variant;
  ElementsInt: variant;
  ClientId: String;
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
  ResultWideStrings := TheElement.MultiEvaluateExpressionAsWideStringList(ElementsInt,
      TBoldSingleFollowerControllerCom(FollowerController).Expression, ClientId, SubscriberIds,
      TBoldSingleFollowerControllerCom(FollowerController).Representation);
  for I := 0 to  Elements.Count - 1 do
    with RendererData[i] as TBoldWideStringRendererDataCom do
    begin
       OldWideStringValue := ResultWideStrings[I];
       CurrentWideStringValue := ResultWideStrings[I];
    end;
  {$ELSE}
  begin
  {$ENDIF}
end;

function TBoldWideStringFollowerController.GetAsWideString(Element: TBoldElement): WideString;
begin
  result := EffectiveAsWideStringRenderer.GetAsWideStringAndSubscribe(element, self, nil);
end;

initialization
  DefaultAsWideStringRenderer := TBoldAsWideStringRenderer.Create(nil);

finalization
  FreeAndNil(DefaultAsWideStringRenderer);

end.
