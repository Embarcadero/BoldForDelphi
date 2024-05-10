
{ Global compiler directives }
{$include bold.inc}
unit BoldFloatControlPack;

{$UNDEF BOLDCOMCLIENT}

interface

uses
  Classes,
  BoldDefs,
  BoldSubscription,
  BoldElements,
  BoldControlPack;

type
  { Forward declarations }
  TBoldFloatRendererData = class;
  TBoldAsFloatRenderer = class;
  TBoldFloatFollowerController = class;

  { TBoldAsFloatRenderer }
  TBoldGetAsFloatEvent = function (aFollower: TBoldFollower): double of object;
  TBoldSetAsFloatEvent = procedure (aFollower: TBoldFollower; const Value: double) of object;
  TBoldFloatIsChangedEvent = function (aFollower: TBoldFollower; const NewValue: double): Boolean of object;

  { TBoldFloatRendererData }
  TBoldFloatRendererData = class(TBoldRendererData)
  private
    FOldFloatValue: double;
    FCurrentFloatValue: double;
  public
    property CurrentFloatValue: double read FCurrentFloatValue write FCurrentFloatValue;
    property OldFloatValue: double read FOldFloatValue write FOldFloatValue;
  end;

  { TBoldAsFloatRenderer }
  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldAsFloatRenderer = class(TBoldSingleRenderer)
  private
    FOnGetAsFloat: TBoldGetAsFloatEvent;
    FOnSetAsFloat: TBoldSetAsFloatEvent;
    FOnIsChanged: TBoldFloatIsChangedEvent;
  protected
    class function DefaultRenderer: TBoldAsFloatRenderer;
    function GetRendererDataClass: TBoldRendererDataClass; override;
    function DefaultGetAsFloatAndSubscribe(aFollower: TBoldFollower; Subscriber: TBoldSubscriber): double; virtual;
    procedure DefaultSetAsFloat(aFollower: TBoldFollower; const Value: double); virtual;
    function GetAsFloatAndSubscribe(aFollower: TBoldFollower; Subscriber: TBoldSubscriber): double; virtual;
    procedure SetAsFloat(aFollower: TBoldFollower; const Value: double); virtual;
    function HasSetValueEventOverrides: boolean; override;
  public
    procedure MakeUptodateAndSubscribe(aFollower: TBoldFollower; Subscriber: TBoldSubscriber); override;
    function DefaultMayModify(aFollower: TBoldFollower): Boolean; override;
    function DefaultIsChanged(aFollower: TBoldFollower; const NewValue: double): Boolean;
    function IsChanged(aFollower: TBoldFollower; const NewValue: double): Boolean;
  published
    property OnGetAsFloat: TBoldGetAsFloatEvent read FOnGetAsFloat write FOnGetAsFloat;
    property OnSetAsFloat: TBoldSetAsFloatEvent read FOnSetAsFloat write FOnSetAsFloat;
    property OnIsChanged: TBoldFloatIsChangedEvent read FOnIsChanged write FOnIsChanged;
  end;

  { TBoldFloatFollowerController }
  TBoldFloatFollowerController = class(TBoldSingleFollowerController)
  private
    function GetRenderer: TBoldAsFloatRenderer;
    procedure SetRenderer(Value: TBoldAsFloatRenderer);
    function GetEffectiveAsFloatRenderer: TBoldAsFloatRenderer;
  protected
    property EffectiveAsFloatRenderer: TBoldAsFloatRenderer read GetEffectiveAsFloatRenderer;
    function GetEffectiveRenderer: TBoldRenderer; override;
  public
    function GetCurrentAsFloat(Follower: TBoldFollower): double;
    procedure  MakeClean(Follower: TBoldFollower); override;
    procedure MayHaveChanged(NewValue: double; Follower: TBoldFollower);
    procedure SetAsFloat(Value: double; Follower: TBoldFollower);
  published
    property Renderer: TBoldAsFloatRenderer read GetRenderer write SetRenderer;
  end;

implementation

uses
  SysUtils,
  BoldUtils,
  BoldAttributes,
  BoldControlPackDefs,  
  BoldGuard;

var
  G_DefaultAsFloatRenderer: TBoldAsFloatRenderer = nil;

{ TBoldAsFloatRenderer }
class function TBoldAsFloatRenderer.DefaultRenderer: TBoldAsFloatRenderer;
begin
  Result := G_DefaultAsFloatRenderer;
end;

function TBoldAsFloatRenderer.GetRendererDataClass: TBoldRendererDataClass;
begin
  Result := TBoldFloatRendererData;
end;

function TBoldAsFloatRenderer.HasSetValueEventOverrides: boolean;
begin
  result := Assigned(FOnSetAsFloat);
end;

function TBoldAsFloatRenderer.DefaultMayModify(aFollower: TBoldFollower): Boolean;
{$IFNDEF BOLDCOMCLIENT}
var
  ValueElement: TBoldElement;
{$ENDIF}
begin
  {$IFDEF BOLDCOMCLIENT}
  result := inherited DefaultMayModify(Element, Representation, Expression, VariableList, Subscriber);
  {$ELSE}
  ValueElement := aFollower.Value;
  result := ((ValueElement is TBAFloat) or (ValueElement is TBACurrency) {or (ValueElement is TBADateTime)}) and ValueElement.ObserverMayModify(aFollower.Subscriber)
  {$ENDIF}
end;

function TBoldAsFloatRenderer.DefaultGetAsFloatAndSubscribe(aFollower: TBoldFollower; Subscriber: TBoldSubscriber): double;
var
  {$IFDEF BOLDCOMCLIENT} // defaultGet
  el: IBoldElement;
  attr: IBoldAttribute;
  {$ELSE}
  IndirectElement: TBoldIndirectElement;
  lGuard: IBoldGuard;
  lResultElement: TBoldElement;
  {$ENDIF}
begin
  Result := 0;
  if Assigned(aFollower.Element) then
  begin
    {$IFDEF BOLDCOMCLIENT} // defaultGet
    if assigned(Subscriber) then
      el := Element.EvaluateAndSubscribeToExpression(Expression, Subscriber.ClientId, Subscriber.SubscriberId, false, false)
    else
      el := Element.EvaluateExpression(Expression);
    if el.QueryInterface(IBoldAttribute, Attr) = S_OK then
      result := Attr.AsVariant
    else
      raise EBold.CreateFmt(sCannotGetFloatValueFromElement, [Attr.AsString])
    {$ELSE}
    lResultElement := aFollower.Value;
    if not Assigned(lResultElement) then
    begin
      lGuard:= TBoldGuard.Create(IndirectElement);
      IndirectElement := TBoldIndirectElement.Create;
      aFollower.Element.EvaluateAndSubscribeToExpression(aFollower.AssertedController.Expression, Subscriber, IndirectElement, False, False, aFollower.Controller.GetVariableListAndSubscribe(Subscriber));
      lResultElement := IndirectElement.Value;
    end;
    if Assigned(lResultElement) then
    begin
      if (lResultElement is TBANumeric) then
        Result := TBANumeric(lResultElement).AsFloat
{      else if (lResultElement is TBADate) then
        Result := TBADate(lResultElement).AsDate
      else if (lResultElement is TBATime) then
        Result := TBATime(lResultElement).AsTime
      else if (lResultElement is TBADateTime) then
        Result := TBADateTime(lResultElement).AsDateTime
}      else
         raise EBold.CreateFmt('Can''t get float value from element (%s)', [lResultElement.ClassName])
    end;
    {$ENDIF}
  end;
end;

procedure TBoldAsFloatRenderer.DefaultSetAsFloat(aFollower: TBoldFollower; const Value: double);
var
  ValueElement: TBoldElement;
  {$IFDEF BOLDCOMCLIENT} // defaultSet
  attr: IBoldAttribute;
  {$ENDIF}
begin
  ValueElement := aFollower.Value;
  {$IFDEF BOLDCOMCLIENT} // defaultSet
  if assigned(ValueElement) and (ValueElement.QueryInterface(IBoldAttribute, attr) = S_OK) then
    Attr.AsVariant := Value
  else
    raise EBold.CreateFmt(sCannotSetFloatValueOnElement, [ValueElement.AsString]);
  {$ELSE}
  if ValueElement is TBAFloat then
    TBAFloat(ValueElement).AsFloat := Value
  else
  if ValueElement is TBACurrency then
    TBACurrency(ValueElement).AsCurrency := Value
{  else
  if ValueElement is TBADateTime then
    TBADateTime(ValueElement).AsDateTime := Value}
  else
    raise EBold.CreateFmt('Can''t set float value on element (%s)', [ValueElement.ClassName]);
  {$ENDIF}
end;

function TBoldAsFloatRenderer.GetAsFloatAndSubscribe(aFollower: TBoldFollower; Subscriber: TBoldSubscriber): double;
begin
  if Assigned(OnSubscribe) and Assigned(Subscriber) then
  begin
    if Assigned(aFollower.Element) then
      OnSubscribe(aFollower, Subscriber);
    Subscriber := nil;
  end;
  if Assigned(OnGetAsFloat) then
    Result := OnGetAsFloat(aFollower)
  else
    Result := DefaultGetAsFloatAndSubscribe(aFollower, Subscriber);
end;

procedure TBoldAsFloatRenderer.SetAsFloat(aFollower: TBoldFollower; const Value: double);
begin
  if Assigned(FOnSetAsFloat) then
    OnSetAsFloat(aFollower, Value)
  else
    DefaultSetAsFloat(aFollower, Value)
end;

procedure TBoldAsFloatRenderer.MakeUptodateAndSubscribe(aFollower: TBoldFollower; Subscriber: TBoldSubscriber);
var
  Value: double;
  lRendererData: TBoldFloatRendererData;
begin
  Value := GetAsFloatAndSubscribe(aFollower, Subscriber);
  lRendererData := (aFollower.RendererData as TBoldFloatRendererData);
  lRendererData.OldFloatValue := Value;
  lRendererData.CurrentFloatValue := Value;
end;

function TBoldAsFloatRenderer.DefaultIsChanged(aFollower: TBoldFollower; const NewValue: double): Boolean;
begin
  Result := NewValue <> TBoldFloatRendererData(aFollower.RendererData).OldFloatValue;
end;

function TBoldAsFloatRenderer.IsChanged(aFollower: TBoldFollower; const NewValue: double): Boolean;
begin
  if Assigned(FOnIsChanged) then
    Result := FOnIsChanged(aFollower, NewValue)
  else
    Result := DefaultIsChanged(aFollower, NewValue);
end;

{ TBoldFloatFollowerController }
function TBoldFloatFollowerController.GetRenderer: TBoldAsFloatRenderer;
begin
  Assert(not Assigned(UntypedRenderer) or (UntypedRenderer is TBoldAsFloatRenderer));
  Result := TBoldAsFloatRenderer(UntypedRenderer);
end;

procedure TBoldFloatFollowerController.SetRenderer(Value: TBoldAsFloatRenderer);
begin
  if Value <> Renderer then
  begin
    if Assigned(Value) then
      Value.FreeNotification(OwningComponent);
    UntypedRenderer := Value;
    Changed;
  end;
end;

function TBoldFloatFollowerController.GetEffectiveRenderer: TBoldRenderer;
begin
  Assert (not Assigned(UntypedRenderer) or (UntypedRenderer is TBoldAsFloatRenderer));
  if Assigned(UntypedRenderer) then
    Result := UntypedRenderer
  else
    Result := G_DefaultAsFloatRenderer;
end;

function TBoldFloatFollowerController.GetEffectiveAsFloatRenderer: TBoldAsFloatRenderer;
begin
  Assert (not Assigned(UntypedRenderer) or (UntypedRenderer is TBoldAsFloatRenderer));
  if Assigned(UntypedRenderer) then
    Result := TBoldAsFloatRenderer(UntypedRenderer)
  else
    Result := G_DefaultAsFloatRenderer;
end;

procedure TBoldFloatFollowerController.MakeClean(Follower: TBoldFollower);
begin
  ReleaseChangedValue(Follower);
  SetAsFloat(GetCurrentAsFloat(Follower), Follower);
end;

function TBoldFloatFollowerController.GetCurrentAsFloat(Follower: TBoldFollower): double;
begin
  Result := (Follower.RendererData as TBoldFloatRendererData).CurrentFloatValue;
end;

procedure TBoldFloatFollowerController.SetAsFloat(Value: double; Follower: TBoldFollower);
begin
  EffectiveAsFloatRenderer.SetAsFloat(Follower, Value);
end;

procedure TBoldFloatFollowerController.MayHaveChanged(NewValue: double; Follower: TBoldFollower);
begin
  if Follower.State in bfsDisplayable then
  begin
    (Follower.RendererData as TBoldFloatRendererData).CurrentFloatValue := NewValue;
    if EffectiveAsFloatRenderer.IsChanged(Follower, NewValue) then
      Follower.ControlledValueChanged;
  end;
end;

initialization
  G_DefaultAsFloatRenderer := TBoldAsFloatRenderer.Create(nil);

finalization
  FreeAndNil(G_DefaultAsFloatRenderer);

end.
