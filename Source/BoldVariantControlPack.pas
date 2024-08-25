unit BoldVariantControlPack;

{ Global compiler directives }
{$include bold.inc}

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
  BoldSubscription,
  Variants;

type
  {Forward declaration of classes}
  TBoldVariantFollowerController = class;
  TBoldCustomAsVariantRenderer = class;  
  TBoldAsVariantRenderer = class;
  TBoldVariantRendererData = class;

  { TBoldAsVariantRenderer prototypes }
  TBoldGetAsVariant = function (AFollower: TBoldFollower): Variant of object;
  TBoldSetAsVariant = procedure (aFollower: TBoldFollower; const NewValue: Variant) of object;
  TBoldValidateVariant = function (aFollower: TBoldFollower; const Value: Variant): Boolean of object;
  TBoldSetFont = procedure (aFollower: TBoldFollower; AFont: TFont) of object;
  TBoldSetColor = procedure (aFollower: TBoldFollower; var AColor: TColor) of object;
  TBoldVariantIsChanged = function (aFollower: TBoldFollower; const NewValue: Variant): Boolean of object;
  TBoldValidateString = function (aFollower: TBoldFollower; const Value: string): Boolean of object;
  TBoldGetAsString = function (AFollower: TBoldFollower{; Subscriber: TBoldSubscriber}): string of object;

  { TBoldVariantRendererData }
  TBoldVariantRendererData = class(TBoldRendererData)
  private
    fOldVariantValue: Variant;
    fCurrentVariantValue: Variant;
    fMaxStringLength: integer;
  public
    constructor Create(OwningFollower: TBoldFollower); override;
    property OldVariantValue: Variant read fOldVariantValue write fOldVariantValue;
    property CurrentVariantValue: Variant read fCurrentVariantValue write fCurrentVariantValue;
    property MaxStringLength: integer read fMaxStringLength write fMaxStringLength;
  end;

  { TBoldCustomAsVariantRenderer }
  TBoldCustomAsVariantRenderer = class(TBoldSingleRenderer)
  private
    FOnGetAsVariant: TBoldGetAsVariant;
    FOnSetAsVariant: TBoldSetAsVariant;
    FOnValidateVariant: TBoldValidateVariant;
    fOnSetFont: TBoldSetFont;
    fOnSetColor: TBoldSetColor;
    fOnIsChanged: TBoldVariantIsChanged;
    FOnGetAsString: TBoldGetAsString;
  protected
    function DefaultDisplayString: string;
    function GetSupportsMulti: Boolean; override;
    function GetRendererDataClass: TBoldRendererDataClass; override;
    function GetAsVariantAndSubscribe(aFollower: TBoldFollower; Subscriber: TBoldSubscriber): Variant; virtual;
    procedure SetAsVariant(aFollower: TBoldFollower; const Value: Variant); virtual;
    procedure DrawOnCanvas(Follower: TBoldFollower; Canvas: TCanvas; Rect: TRect; Alignment: TAlignment; Margins: TPoint); override;
    function HasSetValueEventOverrides: boolean; override;
  public
    class function DefaultRenderer: TBoldCustomAsVariantRenderer; virtual;
    class procedure DrawValueOnCanvas(Canvas: TCanvas; Rect: TRect; Alignment: TAlignment; Margins: TPoint; const aValue: Variant);
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    function EffectiveRenderer: TBoldCustomAsVariantRenderer; virtual;
    function DefaultGetAsVariantAndSubscribe(aFollower: TBoldFollower; Subscriber: TBoldSubscriber): Variant; virtual;
    procedure DefaultSetAsVariant(aFollower: TBoldFollower; const Value: Variant); virtual;
    function DefaultValidateVariant(aFollower: TBoldFollower; const Value: Variant): Boolean; virtual;
    function DefaultIsChanged(aFollower: TBoldFollower; const NewValue: Variant): Boolean; virtual;
    function DefaultGetAsString(AFollower: TBoldFollower): string;
    function ValidateVariant(aFollower: TBoldFollower; const Value: Variant): Boolean; virtual;
    function IsChanged(aFollower: TBoldFollower; const NewValue: Variant): Boolean;
    function GetAsString(aFollower: TBoldFollower): string;
    procedure SetFont(aFollower: TBoldFollower; EffectiveFont, Font: TFont);
    procedure SetColor(aFollower: TBoldFollower; var EffectiveColor: TColor; Color: TColor);
    procedure MakeUptodateAndSubscribe(aFollower: TBoldFollower; Subscriber: TBoldSubscriber); override;
//    procedure MultiMakeUpToDateAndSubscribe(Elements: TBoldClientableList; Subscribers: TBoldObjectArray; RendererData: TBoldObjectArray; FollowerController: TBoldFollowerController);
    procedure DefaultMakeUptodateAndSetMayModifyAndSubscribe(aFollower: TBoldFollower; Subscriber: TBoldSubscriber); virtual;

    property OnGetAsVariant: TBoldGetAsVariant read FOnGetAsVariant write FOnGetAsVariant;
    property OnSetAsVariant: TBoldSetAsVariant read FOnSetAsVariant write FOnSetAsVariant;
    property OnValidateVariant: TBoldValidateVariant read FOnValidateVariant write FOnValidateVariant;
    property OnSetFont: TBoldSetFont read fOnSetFont write fOnSetFont;
    property OnSetColor: TBoldSetColor read fOnSetColor write fOnSetColor;
    property OnIsChanged: TBoldVariantIsChanged read fOnIsChanged write fOnIsChanged;
    property OnGetAsString: TBoldGetAsString read fOnGetAsString write fOnGetAsString;    
  end;

  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldAsVariantRenderer = class(TBoldCustomAsVariantRenderer)
  published
    property OnGetAsVariant;
    property OnSetAsVariant;
    property OnValidateVariant;
    property OnSetFont;
    property OnSetColor;
    property OnIsChanged;
    property OnGetAsString;
  end;

  { TBoldVariantFollowerController }
  TBoldVariantFollowerController = class(TBoldSingleFollowerController)
  private
    FNilRepresentation: Variant;
    function GetRenderer: TBoldCustomAsVariantRenderer;
    procedure SetRenderer(Value: TBoldCustomAsVariantRenderer);
    function GetEffectiveAsVariantRenderer: TBoldCustomAsVariantRenderer;
    procedure SetNilRepresentation(const Value: Variant);
    function IsNilRepresentationStored: boolean;
  protected
    function GetSupportsMultiEnsure: Boolean; override;
    function GetEffectiveRenderer: TBoldRenderer; override;
    property EffectiveAsVariantRenderer: TBoldCustomAsVariantRenderer read GetEffectiveAsVariantRenderer;
    procedure DoMultiMakeUptodateAndSubscribe(Followers: TBoldFollowerArray); override;
    procedure DoAssign(Source: TPersistent); override;
  public
    constructor Create(aOwningComponent: TComponent); reintroduce;
    procedure MakeClean(Follower: TBoldFollower); override;
    function GetCurrentAsVariant(Follower: TBoldFollower): Variant;
    procedure SetAsVariant(const Value: Variant; Follower: TBoldFollower);
    function ValidateVariant(const Value: Variant; Follower: TBoldFollower): Boolean;
    function ValidateCharacter(C: Char; aFollower: TBoldFollower): Boolean;
    procedure SetFont(EffectiveFont, Font: TFont; Follower: TBoldFollower);
    procedure SetColor(var EffectiveColor: TColor; Color: TColor; Follower: TBoldFollower);
    function MayHaveChanged(const NewValue: Variant; Follower: TBoldFollower): boolean;
    procedure DoMakeUptodateAndSubscribe(Follower: TBoldFollower; Subscribe: Boolean); override;
    function GetAsVariant(aFollower: TBoldFollower): Variant;
    function GetAsString(aFollower: TBoldFollower): String;
  published
    property Renderer: TBoldCustomAsVariantRenderer read GetRenderer write SetRenderer;
    property NilRepresentation: Variant read FNilRepresentation write SetNilRepresentation stored IsNilRepresentationStored;
  end;

implementation

uses
  SysUtils,
  BoldControlPackDefs,
  BoldSystem,
  BoldDomainElement,
  BoldGuiResourceStrings,
  BoldGuard,
  BoldValueInterfaces;

var
  DefaultAsVariantRenderer: TBoldCustomAsVariantRenderer;

const
// These should be moved to BoldGuiResourceStrings.pas and perhaps rephrased
  sVariantValidationFailedExtended = 'Validation failed for %s: %2:s';
  sVariantValidationFailed = 'Validation failed';
  sCannotSetVariantValue = '%s.DefaultSetAsVariant: Can''t set variant value';
  sUnknownReason = 'Unknown reason';

type
  TBoldFollowerControllerAccess = class(TBoldFollowerController);

{ TBoldVariantFollowerController }

constructor TBoldVariantFollowerController.Create(aOwningComponent: TComponent);
begin
  FNilRepresentation := Null;
  inherited;
end;

procedure TBoldVariantFollowerController.DoAssign(Source: TPersistent);
begin
  inherited;
  FNilRepresentation := TBoldVariantFollowerController(Source).NilRepresentation;
end;

procedure TBoldVariantFollowerController.DoMakeUptodateAndSubscribe(
  Follower: TBoldFollower; Subscribe: Boolean);
var
  renderer: TBoldCustomAsVariantRenderer;
  Subscriber: TBoldSubscriber;
begin
  If Subscribe then
    Subscriber := Follower.Subscriber
  else
    Subscriber := nil;
  Renderer := EffectiveRenderer as TBoldCustomAsVariantRenderer;
  if Assigned(Renderer.OnGetAsVariant) or Assigned(Renderer.OnSubscribe) or Assigned(Renderer.OnMayModify) then
    Renderer.MakeUptodateAndSubscribe(Follower, Subscriber)
  else
    renderer.DefaultMakeUptodateAndSetMayModifyAndSubscribe(Follower, Subscriber);
end;

procedure TBoldVariantFollowerController.DoMultiMakeUptodateAndSubscribe(
  Followers: TBoldFollowerArray);
begin
  Assert(SupportsMulti);
end;

function TBoldVariantFollowerController.GetAsString(
  aFollower: TBoldFollower): String;
begin
  result := EffectiveAsVariantRenderer.GetAsString(aFollower);
end;

function TBoldVariantFollowerController.GetAsVariant(
  aFollower: TBoldFollower): Variant;
begin
  result := EffectiveAsVariantRenderer.GetAsVariantAndSubscribe(aFollower, aFollower.Subscriber);
end;

function TBoldVariantFollowerController.GetCurrentAsVariant(
  Follower: TBoldFollower): Variant;
begin
  Result := (Follower.RendererData as TBoldVariantRendererData).CurrentVariantValue;
end;

function TBoldVariantFollowerController.GetRenderer: TBoldCustomAsVariantRenderer;
begin
  Result := UntypedRenderer as TBoldCustomAsVariantRenderer;
end;

function TBoldVariantFollowerController.GetEffectiveAsVariantRenderer: TBoldCustomAsVariantRenderer;
begin
  Result := Renderer;
  if not Assigned(Result) then
    Result := TBoldCustomAsVariantRenderer.DefaultRenderer;
end;

function TBoldVariantFollowerController.GetEffectiveRenderer: TBoldRenderer;
begin
  Result := EffectiveAsVariantRenderer;
end;

function TBoldVariantFollowerController.GetSupportsMultiEnsure: Boolean;
begin
  Result := (EffectiveRenderer = DefaultAsVariantRenderer) and (pos('+', expression) = 0);
end;

procedure TBoldVariantFollowerController.MakeClean(
  Follower: TBoldFollower);
var
  el: TBoldDomainElement;
  FailureReason: TBoldFailureReason;
  lValue: Variant;
begin
  lValue := GetCurrentAsVariant(Follower);
  if EffectiveAsVariantRenderer.IsChanged(Follower, lValue) then
  begin
    if ValidateVariant(lValue, Follower) then
    begin
      ReleaseChangedValue(Follower); // note, must do first, since set can change element
      SetAsVariant(lValue, Follower);
    end
    else
    begin
      if follower.Element is TBoldDomainElement then
        el := follower.Element as TBoldDomainElement
      else
        el := nil;
      FailureReason := GetBoldLastFailureReason;
      if assigned(FailureReason) then
        GetBoldLastFailureReason.MessageFormatStr := sVariantValidationFailedExtended;
      BoldRaiseLastFailure(el, '', sUnknownReason);
    end;
  end
  else
    ReleaseChangedValue(Follower);
end;

function TBoldVariantFollowerController.MayHaveChanged(
  const NewValue: Variant; Follower: TBoldFollower): boolean;
var
  lBoldVariantRendererData: TBoldVariantRendererData;
begin
  if Follower.State in bfsDisplayable then
  begin
    lBoldVariantRendererData := Follower.RendererData as TBoldVariantRendererData;
    lBoldVariantRendererData.CurrentVariantValue := NewValue;
    result := EffectiveAsVariantRenderer.IsChanged(Follower, NewValue);
    if result then
    begin
      Follower.ControlledValueChanged;
    end;
  end
  else
    result := false;
end;

procedure TBoldVariantFollowerController.SetAsVariant(const Value: Variant;
  Follower: TBoldFollower);
begin
  EffectiveAsVariantRenderer.SetAsVariant(Follower, Value);
end;

procedure TBoldVariantFollowerController.SetColor(
  var EffectiveColor: TColor; Color: TColor; Follower: TBoldFollower);
begin
  EffectiveAsVariantRenderer.SetColor(Follower, EffectiveColor, Color);
end;

procedure TBoldVariantFollowerController.SetFont(EffectiveFont,
  Font: TFont; Follower: TBoldFollower);
begin
  EffectiveAsVariantRenderer.SetFont(Follower, EffectiveFont, Font);
end;

function TBoldVariantFollowerController.IsNilRepresentationStored: boolean;
begin
  result := not VarIsNull(FNilRepresentation);
end;

procedure TBoldVariantFollowerController.SetNilRepresentation(
  const Value: Variant);
begin
  if (FNilRepresentation <> Value) then
  begin
    FNilRepresentation := Value;
    Changed;
  end;
end;

procedure TBoldVariantFollowerController.SetRenderer(
  Value: TBoldCustomAsVariantRenderer);
begin
  Assert(not Assigned(Value) or (Value is TBoldCustomAsVariantRenderer), Value.Classname + ' is not a ' + TBoldCustomAsVariantRenderer.ClassName);
  UntypedRenderer := Value;
end;

function TBoldVariantFollowerController.ValidateVariant(
  const Value: Variant; Follower: TBoldFollower): Boolean;
begin
  Result := EffectiveAsVariantRenderer.ValidateVariant(Follower, Value);
end;

function TBoldVariantFollowerController.ValidateCharacter(C: Char; aFollower: TBoldFollower): Boolean;
begin
  Result := EffectiveAsVariantRenderer.ValidateCharacter(aFollower, c);
end;

{ TBoldCustomAsVariantRenderer }

procedure TBoldCustomAsVariantRenderer.Assign(Source: TPersistent);
begin
  inherited;
  With Source as TBoldCustomAsVariantRenderer do
  begin
    self.OnGetAsVariant := OnGetAsVariant;
    self.OnSetAsVariant := OnSetAsVariant;
    self.OnValidateVariant := OnValidateVariant;
    self.OnSetFont := OnSetFont;
    self.OnSetColor := OnSetColor;
    self.OnIsChanged := OnIsChanged;
    self.OnValidateCharacter := OnValidateCharacter;
  end;
end;

procedure TBoldCustomAsVariantRenderer.AssignTo(Dest: TPersistent);
begin
  if Dest is TBoldCustomAsVariantRenderer then
    with Dest as TBoldCustomAsVariantRenderer do
    begin
      OnGetAsVariant := self.OnGetAsVariant;
      OnSetAsVariant := self.OnSetAsVariant;
      OnValidateVariant := self.OnValidateVariant;
      OnSetFont := self.OnSetFont;
      OnSetColor := self.OnSetColor;
      OnIsChanged := self.OnIsChanged;
      OnValidateCharacter := self.OnValidateCharacter;
    end
  else
    inherited;
end;

function TBoldCustomAsVariantRenderer.DefaultDisplayString: string;
begin
  if Name <> '' then
    Result := '(' + Name + ')'
  else
    Result := '(' + ClassName + ')';
end;

function TBoldCustomAsVariantRenderer.DefaultGetAsVariantAndSubscribe(
  aFollower: TBoldFollower; Subscriber: TBoldSubscriber
  ): Variant;
var
  lFollowerController: TBoldVariantFollowerController;
  lResultElement: TBoldElement;
begin
  Result := '';
  if (csDesigning in ComponentState) and (Self <> DefaultRenderer) then
  begin
    Result := DefaultDisplayString;
  end
  else
  begin
    lFollowerController := aFollower.AssertedController as TBoldVariantFollowerController;
    lResultElement := aFollower.Value;
    if Assigned(lResultElement) then
    begin
      if (lResultElement is TBoldObjectReference) and not assigned((lResultElement as TBoldObjectReference).BoldObject) then
      begin
        result := lFollowerController.NilRepresentation;
        if Assigned(Subscriber) then
          lResultElement.SubscribeToStringRepresentation(lFollowerController.Representation, Subscriber, breReEvaluate);
      end
      else if Assigned(lResultElement) then
      begin
        Result := lResultElement.AsVariant;
        if Assigned(Subscriber) then
          lResultElement.SubscribeToStringRepresentation(lFollowerController.Representation, Subscriber, breReEvaluate);
        if VarIsNull(Result) then
          Result := lFollowerController.NilRepresentation;
      end
      else
        Result := lFollowerController.NilRepresentation;
    end
    else
      Result := lFollowerController.NilRepresentation;
  end;
end;

function TBoldCustomAsVariantRenderer.DefaultIsChanged(aFollower: TBoldFollower;
  const NewValue: Variant): Boolean;
var
  lOldValue: variant;
begin
  lOldValue := TBoldVariantRendererData(aFollower.RendererData).OldVariantValue;
  Result := not (((VarIsNumeric(NewValue) and VarIsNumeric(lOldValue)) or (VarType(NewValue) = VarType(lOldValue))) and
    (NewValue = lOldValue));
end;

procedure TBoldCustomAsVariantRenderer.DefaultMakeUptodateAndSetMayModifyAndSubscribe(
  aFollower: TBoldFollower; Subscriber: TBoldSubscriber);
var
  lValue: Variant;
  lFollowerController: TBoldVariantFollowerController;
  lRendererData: TBoldVariantRendererData;
  lRepresentation: integer;
  lResultElement: TBoldElement;
begin
  lRendererData:= aFollower.RendererData as TBoldVariantRendererData;
  if (csDesigning in ComponentState) and (Self <> DefaultRenderer) then
  begin
    lValue := DefaultDisplayString;
  end
  else
  begin
    lFollowerController := aFollower.AssertedController as TBoldVariantFollowerController;
    lRepresentation := lFollowerController.Representation;
    lResultElement := aFollower.Value;
    if (lResultElement is TBoldObjectReference) and not assigned((lResultElement as TBoldObjectReference).BoldObject) then
    begin
      lValue := lFollowerController.NilRepresentation;
      if Assigned(Subscriber) then
        lResultElement.SubscribeToStringRepresentation(lRepresentation, Subscriber, breReEvaluate);
      lRendererData.MaxStringLength := -1;
    end
    else if Assigned(lResultElement) then
    begin
      lValue := lResultElement.AsVariant;
      if VarIsNull(lValue) then
        lValue := lFollowerController.NilRepresentation;
      if Assigned(Subscriber) then
        lResultElement.SubscribeToStringRepresentation(lRepresentation, Subscriber, breReEvaluate);
      if (lResultElement is TBoldAttribute) and assigned((lResultElement as TBoldAttribute).BoldAttributeRTInfo) then
        lRendererData.MaxStringLength := (lResultElement as TBoldAttribute).BoldAttributeRTInfo.Length
      else
        lRendererData.MaxStringLength := -1;
    end
    else
      lValue := lFollowerController.NilRepresentation
  end;
  lRendererData.OldVariantValue := lValue;
  lRendererData.CurrentVariantValue := lValue;
end;

class function TBoldCustomAsVariantRenderer.DefaultRenderer: TBoldCustomAsVariantRenderer;
begin
  Result := DefaultAsVariantRenderer;
end;

procedure TBoldCustomAsVariantRenderer.DefaultSetAsVariant(
  aFollower: TBoldFollower; const Value: Variant);
var
  ValueElement: TBoldElement;
begin
  ValueElement := aFollower.Value;
  if Assigned(ValueElement) then
    ValueElement.AsVariant := Value
  else
    raise EBold.CreateFmt(sCannotSetVariantValue, [ClassName]);
end;

function TBoldCustomAsVariantRenderer.DefaultValidateVariant(
  aFollower: TBoldFollower; const Value: Variant): Boolean;
var
  ValueElement: TBoldElement;
begin
  ValueElement := aFollower.Value;
  if Assigned(ValueElement) then
    result := ValueElement.ValidateVariant(Value, TBoldFollowerControllerAccess(aFollower.Controller).Representation)
  else
    Result := true;
end;

procedure TBoldCustomAsVariantRenderer.DrawOnCanvas(Follower: TBoldFollower;
  Canvas: TCanvas; Rect: TRect; Alignment: TAlignment; Margins: TPoint);
begin
  if (not Follower.Displayable) and (Follower.State <> bfsInactiveInvalidElement) then
    Follower.EnsureDisplayable;
  DrawValueOnCanvas(Canvas, Rect, Alignment, Margins, TBoldVariantRendererData(Follower.RendererData).CurrentVariantValue);
end;

class procedure TBoldCustomAsVariantRenderer.DrawValueOnCanvas(Canvas: TCanvas;
  Rect: TRect; Alignment: TAlignment; Margins: TPoint; const aValue: Variant);
var
  Left: Integer;
  s: string;
begin
  s := aValue;
  // Adjust for alignment
  case Alignment of
    taLeftJustify: Left := Margins.X + Rect.Left;
    taRightJustify: Left := (Rect.Right - Rect.Left) - Canvas.TextWidth(S) + Rect.Left - 1 - Margins.X;
  else
    Left := Rect.Left + ((Rect.Right - Rect.Left) - Canvas.TextWidth(S)) div 2;
  end;
  Canvas.TextRect(Rect, Left, Rect.Top + Margins.Y, S);
end;

function TBoldCustomAsVariantRenderer.EffectiveRenderer: TBoldCustomAsVariantRenderer;
begin
  result := self;
end;

function TBoldCustomAsVariantRenderer.GetAsVariantAndSubscribe(
  aFollower: TBoldFollower; Subscriber: TBoldSubscriber
  ): Variant;
begin
  if Assigned(OnSubscribe) and Assigned(Subscriber) then
  begin
    if Assigned(aFollower.Element) then
      OnSubscribe(aFollower, Subscriber);
    Subscriber := nil;
  end;
  if Assigned(OnGetAsVariant) then
    Result := OnGetAsVariant(aFollower)
  else
    Result := DefaultGetAsVariantAndSubscribe(aFollower, Subscriber);
end;

function TBoldCustomAsVariantRenderer.DefaultGetAsString(
  AFollower: TBoldFollower): string;
begin
  // what about Subscriber should it be a param ?
  try
    result := VarToStr(GetAsVariantAndSubscribe(AFollower, nil));
  except
    on EVariantError do
      Result := '';
  end;
end;

function TBoldCustomAsVariantRenderer.GetAsString(
  aFollower: TBoldFollower): string;
begin
  if Assigned(FOnGetAsString) then
    result := FOnGetAsString(AFollower)
  else
    result := DefaultGetAsString(AFollower);
end;

function TBoldCustomAsVariantRenderer.GetRendererDataClass: TBoldRendererDataClass;
begin
  Result := TBoldVariantRendererData;
end;

function TBoldCustomAsVariantRenderer.GetSupportsMulti: Boolean;
begin
  Result := False;
end;

function TBoldCustomAsVariantRenderer.HasSetValueEventOverrides: boolean;
begin
  result := Assigned(FOnSetAsVariant);
end;

function TBoldCustomAsVariantRenderer.IsChanged(aFollower: TBoldFollower;
  const NewValue: Variant): Boolean;
begin
  if Assigned(fOnIsChanged) then
    Result := fOnIsChanged(aFollower, NewValue)
  else
    Result := DefaultIsChanged(aFollower, NewValue);
end;


procedure TBoldCustomAsVariantRenderer.MakeUpToDateAndSubscribe
(aFollower: TBoldFollower; Subscriber: TBoldSubscriber);
var
  lValue: Variant;
  lRendererData: TBoldVariantRendererData;
begin
  lValue := GetAsVariantAndSubscribe(aFollower, Subscriber);
  lRendererData := (aFollower.RendererData as TBoldVariantRendererData);
  lRendererData.OldVariantValue := lValue;
  lRendererData.CurrentVariantValue := lValue;
end;

{procedure TBoldCustomAsVariantRenderer.MultiMakeUpToDateAndSubscribe(
  Elements: TBoldClientableList; Subscribers,
  RendererData: TBoldObjectArray;
  FollowerController: TBoldFollowerController);
begin
// do nothing
end;}

procedure TBoldCustomAsVariantRenderer.SetAsVariant(aFollower: TBoldFollower;
  const Value: Variant);
begin
  if Assigned(FOnSetAsVariant) then
    OnSetAsVariant(aFollower, Value)
  else
    DefaultSetAsVariant(aFollower, Value)
end;

procedure TBoldCustomAsVariantRenderer.SetColor(aFollower: TBoldFollower;
  var EffectiveColor: TColor; Color: TColor);
begin
  EffectiveColor := Color;
  if Assigned(fOnSetColor) then
    fOnSetColor(aFollower, EffectiveColor);
end;

procedure TBoldCustomAsVariantRenderer.SetFont(aFollower: TBoldFollower;
  EffectiveFont, Font: TFont);
begin
  EffectiveFont.Assign(Font);
  if Assigned(fOnSetFont) then
    fOnSetFont(aFollower, EffectiveFont);
end;

function TBoldCustomAsVariantRenderer.ValidateVariant(aFollower: TBoldFollower;
  const Value: Variant): Boolean;
begin
  if Assigned(FOnValidateVariant) then
    Result := OnValidateVariant(aFollower, Value)
  else
    Result := DefaultValidateVariant(aFollower, Value);
end;

{ TBoldVariantRendererData }

constructor TBoldVariantRendererData.Create(OwningFollower: TBoldFollower);
begin
  inherited;
  fMaxStringLength := -1;
  OldVariantValue := Null;
  CurrentVariantValue := Null;
end;

initialization
  DefaultAsVariantRenderer := TBoldAsVariantRenderer.Create(nil);

finalization
  FreeAndNil(DefaultAsVariantRenderer);

end.
