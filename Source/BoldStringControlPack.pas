{ Global compiler directives }
{$include bold.inc}
unit BoldStringControlPack;

{$UNDEF BOLDCOMCLIENT}

interface

uses
  Graphics,
  Classes,
  Windows,
  BoldDefs,
  BoldContainers,
  BoldElements,
  BoldControlPack,
  BoldSubscription;

type
  {Forward declaration of classes}
  TBoldStringFollowerController = class;
  TBoldAsStringRenderer = class;
  TBoldStringRendererData = class;

  { TBoldAsStringRenderer prototypes }
  TBoldGetAsString = function (aFollower: TBoldFollower): string of object;
  TBoldSetAsString = procedure (aFollower: TBoldFollower; const NewValue: string) of object;
  TBoldValidateString = function (aFollower: TBoldFollower; const Value: string): Boolean of object;
  TBoldSetFont = procedure (aFollower: TBoldFollower; AFont: TFont) of object;
  TBoldSetColor = procedure (aFollower: TBoldFollower; var AColor: TColor) of object;
  TBoldStringIsChanged = function (aFollower: TBoldFollower; const NewValue: string): Boolean of object;

  { TBoldStringRendererData }
  TBoldStringRendererData = class(TBoldRendererData)
  private
    fOldStringValue: string;
    fCurrentStringValue: string;
    fMaxStringLength: integer;
  public
    constructor Create(OwningFollower: TBoldFollower); override;
    property OldStringValue: string read fOldStringValue write fOldStringValue;
    property CurrentStringValue: string read fCurrentStringValue write fCurrentStringValue;
    property MaxStringLength: integer read fMaxStringLength write fMaxStringLength;
  end;

  { TBoldAsStringRenderer }
  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldAsStringRenderer = class(TBoldSingleRenderer)
  private
    FOnGetAsString: TBoldGetAsString;
    FOnSetAsString: TBoldSetAsString;
    FOnValidateCharacter: TBoldValidateString;
    FOnValidateString: TBoldValidateString;
    fOnSetFont: TBoldSetFont;
    fOnSetColor: TBoldSetColor;
    fOnIsChanged: TBoldStringIsChanged;
    function DefaultDisplayString: string;    
  protected
    function GetSupportsMulti: Boolean; override;
    function GetRendererDataClass: TBoldRendererDataClass; override;
    function GetAsStringAndSubscribe(aFollower: TBoldFollower; Subscriber: TBoldSubscriber): string; virtual;
    procedure SetAsString(aFollower: TBoldFollower; const Value: string); virtual;
    procedure DrawOnCanvas(Follower: TBoldFollower; Canvas: TCanvas; Rect: TRect; Alignment: TAlignment; Margins: TPoint); override;
  public
    class function DefaultRenderer: TBoldAsStringRenderer;
    class procedure DrawStringOnCanvas(Canvas: TCanvas; Rect: TRect; Alignment: TAlignment; Margins: TPoint; S: string);
    function DefaultGetAsStringAndSubscribe(aFollower: TBoldFollower; Subscriber: TBoldSubscriber): string; virtual;
    procedure DefaultSetAsString(aFollower: TBoldFollower; const Value: string); virtual;
    function DefaultValidateCharacter(aFollower: TBoldFollower; C: Char): Boolean; override;
    function DefaultValidateString(aFollower: TBoldFollower; const Value: string): Boolean; virtual;
    function DefaultIsChanged(aFollower: TBoldFollower; const NewValue: string): Boolean;
    function ValidateCharacter(aFollower: TBoldFollower; C: Char): Boolean; override;
    function ValidateString(aFollower: TBoldFollower; const Value: string): Boolean; virtual;
    function IsChanged(aFollower: TBoldFollower; const NewValue: string): Boolean;
    procedure SetFont(aFollower: TBoldFollower; EffectiveFont, Font: TFont);
    procedure SetColor(aFollower: TBoldFollower; var EffectiveColor: TColor; Color: TColor);
    procedure MakeUptodateAndSubscribe(aFollower: TBoldFollower; Subscriber: TBoldSubscriber); override;
    procedure MultiMakeUpToDateAndSubscribe(Elements: TBoldClientableList; Subscribers: TBoldObjectArray; RendererData: TBoldObjectArray; FollowerController: TBoldFollowerController);
    procedure DefaultMakeUptodateAndSetMayModifyAndSubscribe(aFollower: TBoldFollower; Subscriber: TBoldSubscriber); virtual;
  published
    property OnGetAsString: TBoldGetAsString read FOnGetAsString write FOnGetAsString;
    property OnSetAsString: TBoldSetAsString read FOnSetAsString write FOnSetAsString;
    property OnValidateCharacter: TBoldValidateString read FOnValidateCharacter write FOnValidateCharacter;
    property OnValidateString: TBoldValidateString read FOnValidateString write FOnValidateString;
    property OnSetFont: TBoldSetFont read fOnSetFont write fOnSetFont;
    property OnSetColor: TBoldSetColor read fOnSetColor write fOnSetColor;
    property OnIsChanged: TBoldStringIsChanged read fOnIsChanged write fOnIsChanged;
  end;

  { TBoldStringFollowerController }
  TBoldStringFollowerController = class(TBoldSingleFollowerController)
  private
    FNilStringRepresentation: string;
    function GetRenderer: TBoldAsStringRenderer;
    procedure SetRenderer(Value: TBoldAsStringRenderer);
    function GetEffectiveAsStringRenderer: TBoldAsStringRenderer;
    procedure SetNilStringRepresentation(const Value: string);
  protected
    function GetSupportsMultiEnsure: Boolean; override;
    function GetEffectiveRenderer: TBoldRenderer; override;
    property EffectiveAsStringRenderer: TBoldAsStringRenderer read GetEffectiveAsStringRenderer;
    procedure DoMultiMakeUptodateAndSubscribe(Followers: TBoldFollowerArray); override;
  public
    procedure MakeClean(Follower: TBoldFollower); override;
    function GetCurrentAsString(Follower: TBoldFollower): string;
    procedure SetAsString(Value: string; Follower: TBoldFollower);
    function ValidateCharacter(C: Char; Follower: TBoldFollower): Boolean;
    function ValidateString(Value: string; Follower: TBoldFollower): Boolean;
    procedure SetFont(EffectiveFont, Font: tFont; Follower: TBoldFollower);
    procedure SetColor(var EffectiveColor: tColor; COLOR: tColor; Follower: TBoldFollower);
    function MayHaveChanged(const NewValue: string; Follower: TBoldFollower): boolean;
    procedure DoMakeUptodateAndSubscribe(Follower: TBoldFollower; Subscribe: Boolean); override;
    function GetAsString(aFollower: TBoldFollower): string;
  published
    property Renderer: TBoldAsStringRenderer read GetRenderer write SetRenderer;
    property NilStringRepresentation: string read FNilStringRepresentation write SetNilStringRepresentation;
  end;

implementation

uses
  SysUtils,
  BoldControlPackDefs,
  {$IFNDEF BOLDCOMCLIENT}
  BoldSystem,
  BoldDomainElement,
  {$ELSE}
  Variants,
  {$ENDIF}
  BoldGuard;

var
  DefaultAsStringRenderer: TBoldAsStringRenderer;

{---TBoldStringFollowerController---}

function TBoldStringFollowerController.GetRenderer: TBoldAsStringRenderer;
begin
  Result := UntypedRenderer as TBoldAsStringRenderer;
end;

procedure TBoldStringFollowerController.SetRenderer(Value: TBoldAsStringRenderer);
begin
  UntypedRenderer := Value;
end;

function TBoldStringFollowerController.GetEffectiveRenderer: TBoldRenderer;
begin
  Result := EffectiveAsStringRenderer;
end;

function TBoldStringFollowerController.GetEffectiveAsStringRenderer: TBoldAsStringRenderer;
begin
  if Assigned(Renderer) then
    Result := Renderer
  else
    Result := TBoldAsStringRenderer.DefaultRenderer;
end;

function TBoldStringFollowerController.GetCurrentAsString(Follower: TBoldFollower): string;
begin
  Result := (Follower.RendererData as TBoldStringRendererData).CurrentStringValue;
end;

procedure TBoldStringFollowerController.SetAsString(Value: string; Follower: TBoldFollower);
begin
  EffectiveAsStringRenderer.SetAsString(Follower, Value);
end;

function TBoldStringFollowerController.ValidateCharacter(C: Char; Follower: TBoldFollower): Boolean;
begin
  Result := EffectiveAsStringRenderer.ValidateCharacter(Follower, C);
end;

function TBoldStringFollowerController.ValidateString(Value: string; Follower: TBoldFollower): Boolean;
begin
  Result := EffectiveAsStringRenderer.ValidateString(Follower, Value);
end;

procedure TBoldStringFollowerController.SetFont(EffectiveFont, Font: tFont; Follower: TBoldFollower);
begin
  EffectiveAsStringRenderer.SetFont(Follower, EffectiveFont, Font);
end;

procedure TBoldStringFollowerController.SetColor(var EffectiveColor: tColor; COLOR: tColor; Follower: TBoldFollower);
begin
  EffectiveAsStringRenderer.SetColor(Follower, EffectiveColor, Color);
end;

function TBoldStringFollowerController.MayHaveChanged(const NewValue: string; Follower: TBoldFollower): boolean;
var
  lBoldStringRendererData: TBoldStringRendererData;
begin
  if Follower.State in bfsDisplayable then
  begin
    lBoldStringRendererData := Follower.RendererData as TBoldStringRendererData;
    lBoldStringRendererData.CurrentStringValue := NewValue;
    result := EffectiveAsStringRenderer.IsChanged(Follower, NewValue);
    if result then
    begin
      Follower.ControlledValueChanged;
    end;
  end
  else
    result := false;
end;

procedure TBoldStringFollowerController.MakeClean(Follower: TBoldFollower);
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
    raise EBold.Create(sStringValidationFailed);
    {$ENDIF}
  end;
end;

procedure TBoldStringFollowerController.DoMakeUptodateAndSubscribe(Follower: TBoldFollower; Subscribe: Boolean);
var
  renderer: TBoldAsStringRenderer;
  Subscriber: TBoldSubscriber;
begin
  If Subscribe then
    Subscriber := Follower.Subscriber
  else
    Subscriber := nil;
  Renderer := EffectiveRenderer as TBoldAsStringRenderer;
  if Assigned(Renderer.OnGetAsString) or Assigned(Renderer.OnSubscribe) or Assigned(Renderer.OnMayModify) then
  begin
    Renderer.MakeUptodateAndSubscribe(Follower, Subscriber);
  end
  else
    renderer.DefaultMakeUptodateAndSetMayModifyAndSubscribe(Follower, Subscriber);
end;

procedure TBoldStringFollowerController.SetNilStringRepresentation(const Value: string);
begin
  if (FNilStringRepresentation<>Value) then
  begin
    FNilStringRepresentation := Value;
    Changed;
  end;
end;

{---TBoldAsStringRenderer---}
  class procedure TBoldAsStringRenderer.DrawStringOnCanvas(Canvas: TCanvas; Rect: TRect; Alignment: TAlignment; Margins: TPoint; S: string);
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

procedure TBoldAsStringRenderer.DrawOnCanvas(Follower: TBoldFollower; Canvas: TCanvas; Rect: TRect; Alignment: TAlignment; Margins: TPoint);
begin
  DrawStringOnCanvas(Canvas, Rect, Alignment, Margins, TBoldStringRendererData(Follower.RendererData).CurrentStringValue);
end;

procedure TBoldAsStringRenderer.MakeUpToDateAndSubscribe(aFollower: TBoldFollower; Subscriber: TBoldSubscriber);
var
  S: string;
  lRendererData: TBoldStringRendererData;
begin
  S := GetAsStringAndSubscribe(aFollower, Subscriber);
  lRendererData := (aFollower.RendererData as TBoldStringRendererData);
  lRendererData.OldStringValue := S;
  lRendererData.CurrentStringValue := S;
end;

procedure TBoldAsStringRenderer.DefaultMakeUptodateAndSetMayModifyAndSubscribe(aFollower: TBoldFollower; Subscriber: TBoldSubscriber);
var
  {$IFDEF BOLDCOMCLIENT} // defaultMakeUpToDate
  e: IBoldElement;
  {$ENDIF}
  S: String;
  lFollowerController: TBoldStringFollowerController;
  lRendererData: TBoldStringRendererData;
  lRepresentation: integer;
  lResultElement: TBoldElement;
begin
  S := '';
  lRendererData:= aFollower.RendererData as TBoldStringRendererData;
  if (csDesigning in ComponentState) and (Self <> DefaultRenderer) then
  begin
    s := DefaultDisplayString;
//    lRendererData.MayModify := False;
  end
  else
  begin
    lFollowerController := aFollower.AssertedController as TBoldStringFollowerController;
    lRepresentation := lFollowerController.Representation;
    if Assigned(aFollower.Element) then
    begin
      {$IFDEF BOLDCOMCLIENT} // defaultMakeUpToDate
      if assigned(Subscriber) then
        e := Element.EvaluateAndSubscribeToExpression(FollowerController.Expression, Subscriber.ClientId, Subscriber.SubscriberId, False, false)
      else
        e := Element.EvaluateExpression(FollowerController.Expression);

      if Assigned(E) then
      begin
        S := E.StringRepresentation[FollowerController.Representation];
        if Assigned(Subscriber) then
          E.SubscribeToStringRepresentation(FollowerController.Representation, Subscriber.ClientId, Subscriber.SubscriberId, breReEvaluate, false);
        lRendererData.MayModify := true;
      end
      else
        S := FollowerController.NilStringRepresentation
      {$ELSE}
      lResultElement := aFollower.Value;
      if (lResultElement is TBoldObjectReference) and not assigned(TBoldObjectReference(lResultElement).BoldObject) then
      begin
        s := lFollowerController.NilStringRepresentation;
        if Assigned(Subscriber) then
          lResultElement.SubscribeToStringRepresentation(lRepresentation, Subscriber, breReEvaluate);
//        lRendererData.MayModify := lResultElement.ObserverMayModify(Subscriber);
//        lRendererData.MayModify := lResultElement.ObserverMayModifyAsString(lRepresentation, Subscriber);
        lRendererData.MaxStringLength := -1;
      end
      else if Assigned(lResultElement) then
      begin
        S := lResultElement.StringRepresentation[lRepresentation];
        if Assigned(Subscriber) then
          lResultElement.SubscribeToStringRepresentation(lRepresentation, Subscriber, breReEvaluate);
//        lRendererData.MayModify := lResultElement.ObserverMayModify(Subscriber);
//        lRendererData.MayModify := lResultElement.ObserverMayModifyAsString(lRepresentation, Subscriber);
        if (lResultElement is TBoldAttribute) and assigned(TBoldAttribute(lResultElement).BoldAttributeRTInfo) then
          lRendererData.MaxStringLength := TBoldAttribute(lResultElement).BoldAttributeRTInfo.Length
        else
          lRendererData.MaxStringLength := -1;
{        if (lResultElement is TBoldDomainElement) and not TBoldDomainElement(lResultElement).BoldDirty then
        begin
          lRendererData.OldStringValue := S;
        end;
}
      end
      else
        S := lFollowerController.NilStringRepresentation;
      {$ENDIF}
    end
    else
    begin
      S := lFollowerController.NilStringRepresentation;
//      lRendererData.MayModify := false;
    end;
  end;
  lRendererData.OldStringValue := S;
  lRendererData.CurrentStringValue := S;
end;

function TBoldAsStringRenderer.GetRendererDataClass: TBoldRendererDataClass;
begin
  Result := TBoldStringRendererData;
end;

function TBoldAsStringRenderer.DefaultDisplayString: string;
begin
  if Name <> '' then
    Result := '(' + Name + ')'
  else
    Result := '(' + ClassName + ')';
end;

function TBoldAsStringRenderer.DefaultGetAsStringAndSubscribe(aFollower: TBoldFollower; Subscriber: TBoldSubscriber): string;
var
  {$IFDEF BOLDCOMCLIENT} // DefaultGet
  e: IBoldElement;
  {$ELSE}
  E: TBoldIndirectElement;
  {$ENDIF}
  lFollowerController: TBoldStringFollowerController;
  lResultElement: TBoldElement;
  lGuard: IBoldGuard;
begin
  Result := '';
  if (csDesigning in ComponentState) and (Self <> DefaultRenderer) then
  begin
    Result := DefaultDisplayString;
  end
  else
  begin
    lFollowerController := aFollower.AssertedController as TBoldStringFollowerController;
    if Assigned(aFollower.Element) then
    begin
      {$IFDEF BOLDCOMCLIENT} // defaultGet
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
      lResultElement := aFollower.Value;
      if not Assigned(lResultElement) then
      begin
        lGuard:= TBoldGuard.Create(E);
        E := TBoldIndirectElement.Create;
        aFollower.Element.EvaluateAndSubscribeToExpression(lFollowerController.Expression, Subscriber, E, False, False, lFollowerController.GetVariableListAndSubscribe(Subscriber));
        lResultElement := e.Value;
      end;
      if (lResultElement is TBoldObjectReference) and not assigned(TBoldObjectReference(lResultElement).BoldObject) then
      begin
        result := lFollowerController.NilStringRepresentation;
        if Assigned(Subscriber) then
          lResultElement.SubscribeToStringRepresentation(lFollowerController.Representation, Subscriber, breReEvaluate);
      end
      else if Assigned(lResultElement) then
      begin
        Result := lResultElement.StringRepresentation[lFollowerController.Representation];
        if Assigned(Subscriber) then
          lResultElement.SubscribeToStringRepresentation(lFollowerController.Representation, Subscriber, breReEvaluate);
      end
      else
        Result := lFollowerController.NILStringRepresentation;
      {$ENDIF}
    end
    else
      Result := lFollowerController.NILStringRepresentation;
  end;
end;

procedure TBoldAsStringRenderer.DefaultSetAsString(aFollower: TBoldFollower; const Value: string);
var
  ValueElement: TBoldElement;
begin
  ValueElement := aFollower.Value;
  if Assigned(ValueElement) then
    ValueElement.StringRepresentation[aFollower.AssertedController.Representation] := Value
  else
    raise EBold.Create('TBoldAsStringRenderer.DefaultSetAsString: Can''t set string value');
end;

function TBoldAsStringRenderer.DefaultValidateCharacter(aFollower: TBoldFollower; C: Char): Boolean;
var
  ValueElement: TBoldElement;
begin
  ValueElement := aFollower.Value;
  if Assigned(ValueElement) then
    Result := ValueElement.ValidateCharacter(C, aFollower.AssertedController.Representation )
  else
    Result := HasSetValueEventOverrides;
end;

function TBoldAsStringRenderer.DefaultValidateString(aFollower: TBoldFollower; const Value: string): Boolean;
var
  ValueElement: TBoldElement;
begin
  ValueElement := aFollower.Value;
  if Assigned(ValueElement) then
    Result := ValueElement.ValidateString(Value, aFollower.AssertedController.Representation)
  else
    Result := False;
end;

function TBoldAsStringRenderer.GetAsStringAndSubscribe(aFollower: TBoldFollower; Subscriber: TBoldSubscriber): string;
begin
  if Assigned(OnSubscribe) and Assigned(Subscriber) then
  begin
    if Assigned(aFollower.Element) then
      OnSubscribe(aFollower, Subscriber);
    Subscriber := nil;
  end;
  if Assigned(OnGetAsString) then
    Result := OnGetAsString(aFollower)
  else
    Result := DefaultGetAsStringAndSubscribe(aFollower, Subscriber);
end;

procedure TBoldAsStringRenderer.SetAsString(aFollower: TBoldFollower; const Value: string);
begin
  if Assigned(FOnSetAsString) then
    OnSetAsString(aFollower, Value)
  else
    DefaultSetAsString(aFollower, Value)
end;

function TBoldAsStringRenderer.ValidateCharacter(aFollower: TBoldFollower; C: Char): Boolean;
begin
  if Assigned(FOnValidateCharacter) then
    Result := OnValidateCharacter(aFollower, c)
  else
    Result := DefaultValidateCharacter(aFollower, c);
end;

function TBoldAsStringRenderer.ValidateString(aFollower: TBoldFollower; const Value: string): Boolean;
begin
  if Assigned(FOnValidateString) then
    Result := OnValidateString(aFollower, Value)
  else
    Result := DefaultValidateString(aFollower, Value);
end;

procedure TBoldAsStringRenderer.SetFont(aFollower: TBoldFollower; EffectiveFont, Font: TFont);
begin
  EffectiveFont.Assign(Font);
  if Assigned(fOnSetFont) then
    fOnSetFont(aFollower, EffectiveFont);
end;

procedure TBoldAsStringRenderer.SetColor(aFollower: TBoldFollower; var EffectiveColor: TColor; Color: TColor);
begin
  EffectiveColor := Color;
  if Assigned(fOnSetColor) then
    fOnSetColor(aFollower, EffectiveColor);
end;

class function TBoldAsStringRenderer.DefaultRenderer: TBoldAsStringRenderer;
begin
  Result := DefaultAsStringRenderer;
end;

function TBoldAsStringRenderer.DefaultIsChanged(aFollower: TBoldFollower; const NewValue: string): Boolean;
begin
  Result := NewValue <> TBoldStringRendererData(aFollower.RendererData).OldStringValue;
end;

function TBoldAsStringRenderer.IsChanged(aFollower: TBoldFollower; const NewValue: string): Boolean;
begin
  if Assigned(fOnIsChanged) then
    Result := fOnIsChanged(aFollower, NewValue)
  else
    Result := DefaultIsChanged(aFollower, NewValue);
end;

{ TBoldStringRendererData }

constructor TBoldStringRendererData.Create(OwningFollower: TBoldFollower);
begin
  inherited;
  fMaxStringLength := -1;
end;

function TBoldAsStringRenderer.GetSupportsMulti: Boolean;
begin
  {$IFDEF BOLDCOMCLIENT}
  Result := not  (Assigned(fOnGetAsString) or Assigned(OnSubscribe) or Assigned(OnMayModify));
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function TBoldStringFollowerController.GetSupportsMultiEnsure: Boolean;
begin
  {$IFDEF BOLDCOMCLIENT}
  Result := False;
  {$ELSE}
  Result := (EffectiveRenderer = DefaultAsStringRenderer) and
    (pos('+', expression) = 0);
  {$ENDIF}
end;

procedure TBoldStringFollowerController.DoMultiMakeUptodateAndSubscribe(
  Followers: TBoldFollowerArray);
var
  Renderer: TBoldAsStringRenderer;
  Elements: TBoldClientableList;
  F: TBoldFollower;
  Subscribers: TBoldObjectArray;
  RendererData: TBoldObjectArray;
begin
  Assert(SupportsMulti);
  Renderer := EffectiveRenderer as TBoldAsStringRenderer;
  Elements := TBoldClientableList.Create(Length(Followers),[]);
  Subscribers := TBoldObjectArray.Create(Length(Followers),[]);
  RendererData := TBoldObjectArray.Create(Length(Followers),[]);
  try
    for F in Followers do
    begin
      Elements.Add(F.Element);
      if F.State in bfdNeedResubscribe then
        Subscribers.Add(F.Subscriber)
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

procedure TBoldAsStringRenderer.MultiMakeUpToDateAndSubscribe(
  Elements: TBoldClientableList; Subscribers,
  RendererData: TBoldObjectArray;
  FollowerController: TBoldFollowerController);
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

function TBoldStringFollowerController.GetAsString(aFollower: TBoldFollower): string;
begin
  result := EffectiveAsStringRenderer.GetAsStringAndSubscribe(aFollower, nil);
end;

initialization
  DefaultAsStringRenderer := TBoldAsStringRenderer.Create(nil);

finalization
  FreeAndNil(DefaultAsStringRenderer);

end.
