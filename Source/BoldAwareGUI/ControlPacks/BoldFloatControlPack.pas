unit BoldFloatControlPack;

{$UNDEF BOLDCOMCLIENT}

interface

uses
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
  TBoldGetAsFloatEvent = function (Element: TBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression): double of object;
  TBoldSetAsFloatEvent = procedure (Element: TBoldElement; const Value: double; Representation: TBoldRepresentation; Expression: TBoldExpression) of object;
  TBoldFloatIsChangedEvent = function (RenderData: TBoldFloatRendererData; const NewValue: double; Representation: TBoldRepresentation; Expression: TBoldExpression): Boolean of object;

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
  TBoldAsFloatRenderer = class(TBoldSingleRenderer)
  private
    FOnGetAsFloat: TBoldGetAsFloatEvent;
    FOnSetAsFloat: TBoldSetAsFloatEvent;
    FOnIsChanged: TBoldFloatIsChangedEvent;
  protected
    class function DefaultRenderer: TBoldAsFloatRenderer;
    function GetRendererDataClass: TBoldRendererDataClass; override;
    function DefaultGetAsFloatAndSubscribe(Element: TBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList; Subscriber: TBoldSubscriber): double; virtual;
    procedure DefaultSetAsFloat(Element: TBoldElement; const Value: double; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList); virtual;
    function GetAsFloatAndSubscribe(Element: TBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList; Subscriber: TBoldSubscriber): double; virtual;
    procedure SetAsFloat(Element: TBoldElement; const Value: double; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList); virtual;
  public
    procedure MakeUptodateAndSubscribe(Element: TBoldElement; RendererData: TBoldRendererData; FollowerController: TBoldFollowerController; Subscriber: TBoldSubscriber); override;
    function DefaultMayModify(Element: TBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList; Subscriber: TBoldSubscriber): Boolean; override;
    function DefaultIsChanged(RendererData: TBoldFloatRendererData; const NewValue: double; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList): Boolean;
    function IsChanged(RendererData: TBoldFloatRendererData; const NewValue: double; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList): Boolean;
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
//    procedure Assign(Source: TPersistent); override;
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
  BoldGuiResourceStrings,
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

function TBoldAsFloatRenderer.DefaultMayModify(Element: TBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList; Subscriber: TBoldSubscriber): Boolean;
{$IFNDEF BOLDCOMCLIENT} // defaultMayModify
var
  ValueElement: TBoldElement;
{$ENDIF}
begin
  {$IFDEF BOLDCOMCLIENT} // defaultMayModify
  result := inherited DefaultMayModify(Element, Representation, Expression, VariableList, Subscriber);
  {$ELSE}
  // Note! We don't call inherited DefaultMayModify to prevent evaluation of expression two times!
  ValueElement := GetExpressionAsDirectElement(Element, Expression, VariableList);
  result := ((ValueElement is TBAFloat) or (ValueElement is TBADateTime)) and ValueElement.ObserverMayModify(Subscriber)
  {$ENDIF}
end;

function TBoldAsFloatRenderer.DefaultGetAsFloatAndSubscribe(Element: TBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList; Subscriber: TBoldSubscriber): double;
var
  {$IFDEF BOLDCOMCLIENT} // defaultGet
  el: IBoldElement;
  attr: IBoldAttribute;
  {$ELSE}
  IndirectElement: TBoldIndirectElement;
  g: IBoldGuard;
  {$ENDIF}
begin
  Result := 0;
  if Assigned(Element) then
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
    g := TBoldGuard.Create(IndirectElement);
    IndirectElement := TBoldIndirectElement.Create;
    Element.EvaluateAndSubscribeToExpression(Expression, Subscriber, IndirectElement, False, False, VariableList);
    if Assigned(IndirectElement.Value) then
    begin
      if (IndirectElement.Value is TBANumeric) then
        Result := TBANumeric(IndirectElement.Value).AsFloat
      else if (IndirectElement.Value is TBADate) then
        Result := TBADate(IndirectElement.Value).AsDate
      else if (IndirectElement.Value is TBATime) then
        Result := TBATime(IndirectElement.Value).AsTime
      else if (IndirectElement.Value is TBADateTime) then
        Result := TBADateTime(IndirectElement.Value).AsDateTime
      else
        raise EBold.CreateFmt(sCannotGetFloatValueFromElement, [IndirectElement.Value.ClassName])
    end;
    {$ENDIF}
  end;
end;

procedure TBoldAsFloatRenderer.DefaultSetAsFloat(Element: TBoldElement; const Value: double; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList);
var
  ValueElement: TBoldElement;
  {$IFDEF BOLDCOMCLIENT} // defaultSet
  attr: IBoldAttribute;
  {$ENDIF}
begin
  ValueElement := GetExpressionAsDirectElement(Element, Expression , VariableList);
  {$IFDEF BOLDCOMCLIENT} // defaultSet
  if assigned(ValueElement) and (ValueElement.QueryInterface(IBoldAttribute, attr) = S_OK) then
    Attr.AsVariant := Value
  else
    raise EBold.CreateFmt(sCannotSetFloatValueOnElement, [ValueElement.AsString]);
  {$ELSE}
  if ValueElement is TBAFloat then
    TBAFloat(ValueElement).AsFloat := Value
  else if ValueElement is TBADateTime then
    TBADateTime(ValueElement).AsDateTime := Value
  else
    raise EBold.CreateFmt(sCannotSetFloatValueOnElement, [ValueElement.ClassName]);
  {$ENDIF}
end;

function TBoldAsFloatRenderer.GetAsFloatAndSubscribe(Element: TBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList; Subscriber: TBoldSubscriber): double;
begin
  if Assigned(OnSubscribe) and Assigned(Subscriber) then
  begin
    if Assigned(Element) then
      OnSubscribe(Element, Representation, Expression, Subscriber);
    Subscriber := nil;
  end;
  if Assigned(OnGetAsFloat) then
    Result := OnGetAsFloat(Element, Representation, Expression)
  else
    Result := DefaultGetAsFloatAndSubscribe(Element, Representation, Expression, VariableList, Subscriber);
end;

procedure TBoldAsFloatRenderer.SetAsFloat(Element: TBoldElement; const Value: double; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList);
begin
  if Assigned(FOnSetAsFloat) then
    OnSetAsFloat(Element, Value, Representation, Expression)
  else
    DefaultSetAsFloat(Element, Value, Representation, Expression, VariableList)
end;

procedure TBoldAsFloatRenderer.MakeUptodateAndSubscribe(Element: TBoldElement; RendererData: TBoldRendererData; FollowerController: TBoldFollowerController; Subscriber: TBoldSubscriber);
var
  Value: double;
begin
  Value := GetAsFloatAndSubscribe(Element,
    (FollowerController as TBoldFloatFollowerController).Representation,
    (FollowerController as TBoldFloatFollowerController).Expression,
    FollowerController.GetVariableListAndSubscribe(Subscriber),
    Subscriber);
  with (RendererData as TBoldFloatRendererData) do
  begin
    OldFloatValue := Value;
    CurrentFloatValue := Value;
  end;
end;

function TBoldAsFloatRenderer.DefaultIsChanged(RendererData: TBoldFloatRendererData; const NewValue: double; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList): Boolean;
begin
  Result := NewValue <> RendererData.OldFloatValue;
end;

function TBoldAsFloatRenderer.IsChanged(RendererData: TBoldFloatRendererData; const NewValue: double; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList): Boolean;
begin
  if Assigned(FOnIsChanged) then
    Result := FOnIsChanged(RendererData, NewValue, Representation, Expression)
  else
    Result := DefaultIsChanged(RendererData, NewValue, Representation, Expression, VariableList);
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
  EffectiveAsFloatRenderer.SetAsFloat(Follower.Element, Value, Representation, Expression, VariableList);
end;

procedure TBoldFloatFollowerController.MayHaveChanged(NewValue: double; Follower: TBoldFollower);
begin
  if Follower.State in bfsDisplayable then
  begin
    (Follower.RendererData as TBoldFloatRendererData).CurrentFloatValue := NewValue;
    Follower.ControlledValueChanged(EffectiveAsFloatRenderer.IsChanged(Follower.RendererData as TBoldFloatRendererData, NewValue, Representation, Expression, VariableList));
  end;
end;

initialization
  G_DefaultAsFloatRenderer := TBoldAsFloatRenderer.Create(nil);

finalization
  FreeAndNil(G_DefaultAsFloatRenderer);

end.
