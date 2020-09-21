unit BoldDateTimeControlPack;

{$UNDEF BOLDCOMCLIENT}

interface

uses
  BoldDefs,
  BoldSubscription,
  BoldElements,
  BoldControlPack;

type
  { Forward declarations }
  TBoldDateTimeRendererData = class;
  TBoldAsDateTimeRenderer = class;
  TBoldDateTimeFollowerController = class;

  {TBoldAsDateTimeRenderer}
  TBoldGetAsDateTimeEvent = function (Element: TBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression): TDateTime of object;
  TBoldSetAsDateTimeEvent = procedure (Element: TBoldElement; const Value: TDateTime; Representation: TBoldRepresentation; Expression: TBoldExpression) of object;
  TBoldDateTimeIsChangedEvent = function (RenderData: TBoldDateTimeRendererData; const NewValue: TDateTime; Representation: TBoldRepresentation; Expression: TBoldExpression): Boolean of object;

  { TBoldDateTimeRendererData }
  TBoldDateTimeRendererData = class(TBoldRendererData)
  private
    FOldDateTimeValue: TDateTime;
    FCurrentDateTimeValue: TDateTime;
  public
    property CurrentDateTimeValue: TDateTime read FCurrentDateTimeValue write FCurrentDateTimeValue;
    property OldDateTimeValue: TDateTime read FOldDateTimeValue write FOldDateTimeValue;
  end;

  { TBoldAsDateTimeRenderer }
  TBoldAsDateTimeRenderer = class(TBoldsingleRenderer)
  private
    FOnGetAsDateTime: TBoldGetAsDateTimeEvent;
    FOnSetAsDateTime: TBoldSetAsDateTimeEvent;
    FOnIsChanged: TBoldDateTimeIsChangedEvent;
  protected
    class function DefaultRenderer: TBoldAsDateTimeRenderer;
    function GetRendererDataClass: TBoldRendererDataClass; override;
    function DefaultGetAsDateTimeAndSubscribe(Element: TBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList; Subscriber: TBoldSubscriber): TDateTime; virtual;
    procedure DefaultSetAsDateTime(Element: TBoldElement; const Value: TDateTime; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList); virtual;
    function GetAsDateTimeAndSubscribe(Element: TBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList; Subscriber: TBoldSubscriber): TDateTime; virtual;
    procedure SetAsDateTime(Element: TBoldElement; const Value: TDateTime; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList); virtual;
  public
    procedure MakeUptodateAndSubscribe(Element: TBoldElement; RendererData: TBoldRendererData; FollowerController: TBoldFollowerController; Subscriber: TBoldSubscriber); override;
    function DefaultMayModify(Element: TBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList; Subscriber: TBoldSubscriber): Boolean; override;
    function DefaultIsChanged(RendererData: TBoldDateTimeRendererData; const NewValue: TDateTime; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList): Boolean;
    function IsChanged(RendererData: TBoldDateTimeRendererData; const NewValue: TDateTime; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList): Boolean;
  published
    property OnGetAsDateTime: TBoldGetAsDateTimeEvent read FOnGetAsDateTime write FOnGetAsDateTime;
    property OnSetAsDateTime: TBoldSetAsDateTimeEvent read FOnSetAsDateTime write FOnSetAsDateTime;
    property OnIsChanged: TBoldDateTimeIsChangedEvent read FOnIsChanged write FOnIsChanged;
  end;

  { TBoldDateTimeFollowerController }
  TBoldDateTimeFollowerController = class(TBoldSingleFollowerController)
  private
    function GetRenderer: TBoldAsDateTimeRenderer;
    procedure SetRenderer(Value: TBoldAsDateTimeRenderer);
    function GetEffectiveAsDateTimeRenderer: TBoldAsDateTimeRenderer;
  protected
    function GetEffectiveRenderer: TBoldRenderer; override;
    property EffectiveAsDateTimeRenderer: TBoldAsDateTimeRenderer read GetEffectiveAsDateTimeRenderer;
  public
//    procedure Assign(Source: TPersistent); override;
    function GetCurrentAsDateTime(Follower: TBoldFollower): TDateTime;
    procedure  MakeClean(Follower: TBoldFollower); override;
    procedure MayHaveChanged(NewValue: TDateTime; Follower: TBoldFollower);
    procedure SetAsDateTime(Value: TDateTime; Follower: TBoldFollower);
  published
    property Renderer: TBoldAsDateTimeRenderer read GetRenderer write SetRenderer;
  end;

implementation

uses
  SysUtils,
  BoldGuiResourceStrings,
  BoldControlPackDefs,
  BoldAttributes,
  BoldGuard;

var
  DefaultAsDateTimeRenderer: TBoldAsDateTimeRenderer = nil;

{ TBoldAsDateTimeRenderer }
class function TBoldAsDateTimeRenderer.DefaultRenderer: TBoldAsDateTimeRenderer;
begin
  Result := DefaultAsDateTimeRenderer;
end;

function TBoldAsDateTimeRenderer.GetRendererDataClass: TBoldRendererDataClass;
begin
  Result := TBoldDateTimeRendererData;
end;

function TBoldAsDateTimeRenderer.DefaultMayModify(Element: TBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList; Subscriber: TBoldSubscriber): Boolean;
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
  result := (ValueElement is TBAMoment) and ValueElement.ObserverMayModify(Subscriber);
  {$ENDIF}
end;

function TBoldAsDateTimeRenderer.DefaultGetAsDateTimeAndSubscribe(Element: TBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList; Subscriber: TBoldSubscriber): TDateTime;
var
  {$IFDEF BOLDCOMCLIENT} // DefaultGet
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
    {$IFDEF BOLDCOMCLIENT} //defaultGet
    if assigned(Subscriber) then
      el := Element.EvaluateAndSubscribeToExpression(Expression, Subscriber.ClientId, Subscriber.SubscriberId, false, false)
    else
      el := Element.EvaluateExpression(Expression);
    if Assigned(el) then
    begin
      if el.QueryInterface(IBoldAttribute, attr) = S_OK then
        Result := attr.AsVariant
      else
        raise EBold.CreateFmt(sCannotGetDateTimeValueFromElement, [Attr.Asstring])
    end;
    {$ELSE}
    g := TBoldGuard.Create(IndirectElement);
    IndirectElement := TBoldIndirectElement.Create;
    Element.EvaluateAndSubscribeToExpression(Expression, Subscriber, IndirectElement, False, false, VariableList);
    if Assigned(IndirectElement.Value) then
    begin
      if (IndirectElement.Value is TBADateTime) then
        Result := TBADateTime(IndirectElement.Value).AsDateTime
      else if (IndirectElement.Value is TBADate) then
        Result := TBADate(IndirectElement.Value).AsDate
      else if (IndirectElement.Value is TBATime) then
        Result := TBATime(IndirectElement.Value).AsTime
      else
        raise EBold.CreateFmt(sCannotGetDateTimeValueFromElement, [IndirectElement.Value.ClassName])
    end;
    {$ENDIF}
  end;
end;

procedure TBoldAsDateTimeRenderer.DefaultSetAsDateTime(Element: TBoldElement; const Value: TDateTime; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList);
var
  {$IFDEF BOLDCOMCLIENT} // defaultSet
  Attr: IBoldAttribute;
  {$ENDIF}
  ValueElement: TBoldElement;
begin
  ValueElement := GetExpressionAsDirectElement(Element, Expression, VariableList);
  {$IFDEF BOLDCOMCLIENT} // defaultSet
  if assigned(ValueElement) and (ValueElement.QueryInterface(IBoldAttribute, Attr) = S_OK) then
    Attr.AsVariant := Value
  else
    raise EBold.CreateFmt(sCannotSetDateTimeValueOnElement, [ValueElement.AsString]);
  {$ELSE}
  if Assigned(ValueElement) and (ValueElement is TBADateTime) then
    TBADateTime(ValueElement).AsDateTime := Value
  else if Assigned(ValueElement) and (ValueElement is TBADate) then
    TBADate(ValueElement).AsDate := Value
  else if Assigned(ValueElement) and (ValueElement is TBATime) then
    TBATime(ValueElement).AsTime := Value
  else
    raise EBold.CreateFmt(sCannotSetDateTimeValueOnElement, [ValueElement.ClassName]);
  {$ENDIF}
end;

function TBoldAsDateTimeRenderer.GetAsDateTimeAndSubscribe(Element: TBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList; Subscriber: TBoldSubscriber): TDateTime;
begin
  if Assigned(OnSubscribe) and Assigned(Subscriber) then
  begin
    if Assigned(Element) then
      OnSubscribe(Element, Representation, Expression, Subscriber);
    Subscriber := nil;
  end;
  if Assigned(OnGetAsDateTime) then
    Result := OnGetAsDateTime(Element, Representation, Expression)
  else
    Result := DefaultGetAsDateTimeAndSubscribe(Element, Representation, Expression, VariableList, Subscriber);
end;

procedure TBoldAsDateTimeRenderer.SetAsDateTime(Element: TBoldElement; const Value: TDateTime; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList);
begin
  if Assigned(FOnSetAsDateTime) then
    OnSetAsDateTime(Element, Value, Representation, Expression)
  else
    DefaultSetAsDateTime(Element, Value, Representation, Expression, VariableList)
end;

procedure TBoldAsDateTimeRenderer.MakeUptodateAndSubscribe(Element: TBoldElement; RendererData: TBoldRendererData; FollowerController: TBoldFollowerController; Subscriber: TBoldSubscriber);
var
  Value: TDateTime;
begin
  Assert(FollowerController is TBoldDateTimeFollowerController);
  Value := GetAsDateTimeAndSubscribe(Element,
    TBoldDateTimeFollowerController(FollowerController).Representation,
    TBoldDateTimeFollowerController(FollowerController).Expression, FollowerController.GetVariableListAndSubscribe(Subscriber),
    subscriber);
  with (RendererData as TBoldDateTimeRendererData) do
  begin
    OldDateTimeValue := Value;
    CurrentDateTimeValue := Value;
  end;
end;

function TBoldAsDateTimeRenderer.DefaultIsChanged(RendererData: TBoldDateTimeRendererData; const NewValue: TDateTime; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList): Boolean;
begin
  Result := NewValue <> RendererData.OldDateTimeValue;
end;

function TBoldAsDateTimeRenderer.IsChanged(RendererData: TBoldDateTimeRendererData; const NewValue: TDateTime; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList): Boolean;
begin
  if Assigned(FOnIsChanged) then
    Result := FOnIsChanged(RendererData, NewValue, Representation, Expression)
  else
    Result := DefaultIsChanged(RendererData, NewValue, Representation, Expression, VariableList);
end;

{ TBoldDateTimeFollowerController }

function TBoldDateTimeFollowerController.GetRenderer: TBoldAsDateTimeRenderer;
begin
  Result := UntypedRenderer as TBoldAsDateTimeRenderer;
end;

procedure TBoldDateTimeFollowerController.SetRenderer(Value: TBoldAsDateTimeRenderer);
begin
  if Value <> Renderer then
  begin
    if Assigned(Value) then
      Value.FreeNotification(OwningComponent);
    UntypedRenderer := Value;
    Changed;
  end;
end;

function TBoldDateTimeFollowerController.GetEffectiveRenderer: TBoldRenderer;
begin
  Result := EffectiveAsDateTimeRenderer;
end;

function TBoldDateTimeFollowerController.GetEffectiveAsDateTimeRenderer: TBoldAsDateTimeRenderer;
begin
  if Assigned(Renderer) then
    Result := Renderer
  else
    Result := DefaultAsDateTimeRenderer; //FIXME
end;

procedure TBoldDateTimeFollowerController.MakeClean(Follower: TBoldFollower);
begin
  ReleaseChangedValue(Follower);
  SetAsDateTime(GetCurrentAsDateTime(Follower), Follower);
end;

function TBoldDateTimeFollowerController.GetCurrentAsDateTime(Follower: TBoldFollower): TDateTime;
begin
  Result := (Follower.RendererData as TBoldDateTimeRendererData).CurrentDateTimeValue;
end;

procedure TBoldDateTimeFollowerController.SetAsDateTime(Value: TDateTime; Follower: TBoldFollower);
begin
  EffectiveAsDateTimeRenderer.SetAsDateTime(Follower.Element, Value, Representation, Expression, VariableList);
end;

procedure TBoldDateTimeFollowerController.MayHaveChanged(NewValue: TDateTime; Follower: TBoldFollower);
begin
  if Follower.State in bfsDisplayable then
  begin
    (Follower.RendererData as TBoldDateTimeRendererData).CurrentDateTimeValue := NewValue;
    Follower.ControlledValueChanged(EffectiveAsDateTimeRenderer.IsChanged(Follower.RendererData as TBoldDateTimeRendererData, NewValue, Representation, Expression, VariableList));
  end;
end;

initialization
  DefaultAsDateTimeRenderer := TBoldAsDateTimeRenderer.Create(nil);

finalization
  FreeAndNil(DefaultAsDateTimeRenderer);

end.
