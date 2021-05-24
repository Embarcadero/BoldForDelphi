
{ Global compiler directives }
{$include bold.inc}
unit BoldFloatControlPackCom;

{$DEFINE BOLDCOMCLIENT} {Clientified 2002-08-05 13:13:02}

interface

uses
  BoldDefs,
  BoldSubscription,
  BoldComObjectSpace_TLB, BoldClientElementSupport, BoldComClient,
  BoldControlPackCom;

type
  { Forward declarations }
  TBoldFloatRendererDataCom = class;
  TBoldAsFloatRendererCom = class;
  TBoldFloatFollowerControllerCom = class;

  { TBoldAsFloatRendererCom }
  TBoldGetAsFloatEventCom = function (Element: IBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression): double of object;
  TBoldSetAsFloatEventCom = procedure (Element: IBoldElement; const Value: double; Representation: TBoldRepresentation; Expression: TBoldExpression) of object;
  TBoldFloatIsChangedEventCom = function (RenderData: TBoldFloatRendererDataCom; const NewValue: double; Representation: TBoldRepresentation; Expression: TBoldExpression): Boolean of object;

  { TBoldFloatRendererDataCom }
  TBoldFloatRendererDataCom = class(TBoldFollowerDataCom)
  private
    FOldFloatValue: double;
    FCurrentFloatValue: double;
  public
    property CurrentFloatValue: double read FCurrentFloatValue write FCurrentFloatValue;
    property OldFloatValue: double read FOldFloatValue write FOldFloatValue;
  end;

  { TBoldAsFloatRendererCom }
  TBoldAsFloatRendererCom = class(TBoldSingleRendererCom)
  private
    FOnGetAsFloat: TBoldGetAsFloatEventCom;
    FOnSetAsFloat: TBoldSetAsFloatEventCom;
    FOnIsChanged: TBoldFloatIsChangedEventCom;
  protected
    class function DefaultRenderer: TBoldAsFloatRendererCom;
    function GetRendererDataClass: TBoldRendererDataClassCom; override;
    function DefaultGetAsFloatAndSubscribe(Element: IBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList; Subscriber: TBoldComClientSubscriber): double; virtual;
    procedure DefaultSetAsFloat(Element: IBoldElement; const Value: double; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList); virtual;
    function GetAsFloatAndSubscribe(Element: IBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList; Subscriber: TBoldComClientSubscriber): double; virtual;
    procedure SetAsFloat(Element: IBoldElement; const Value: double; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList); virtual;
  public
    procedure MakeUptodateAndSubscribe(Element: IBoldElement; RendererData: TBoldFollowerDataCom; FollowerController: TBoldFollowerControllerCom; Subscriber: TBoldComClientSubscriber); override;
    function DefaultMayModify(Element: IBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList; Subscriber: TBoldComClientSubscriber): Boolean; override;
    function DefaultIsChanged(RendererData: TBoldFloatRendererDataCom; const NewValue: double; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList): Boolean;
    function IsChanged(RendererData: TBoldFloatRendererDataCom; const NewValue: double; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList): Boolean;
  published
    property OnGetAsFloat: TBoldGetAsFloatEventCom read FOnGetAsFloat write FOnGetAsFloat;
    property OnSetAsFloat: TBoldSetAsFloatEventCom read FOnSetAsFloat write FOnSetAsFloat;
    property OnIsChanged: TBoldFloatIsChangedEventCom read FOnIsChanged write FOnIsChanged;
  end;

  { TBoldFloatFollowerControllerCom }
  TBoldFloatFollowerControllerCom = class(TBoldSingleFollowerControllerCom)
  private
    function GetRenderer: TBoldAsFloatRendererCom;
    procedure SetRenderer(Value: TBoldAsFloatRendererCom);
    function GetEffectiveAsFloatRenderer: TBoldAsFloatRendererCom;
  protected
    property EffectiveAsFloatRenderer: TBoldAsFloatRendererCom read GetEffectiveAsFloatRenderer;
    function GetEffectiveRenderer: TBoldRendererCom; override;
  public
    function GetCurrentAsFloat(Follower: TBoldFollowerCom): double;
    procedure  MakeClean(Follower: TBoldFollowerCom); override;
    procedure MayHaveChanged(NewValue: double; Follower: TBoldFollowerCom);
    procedure SetAsFloat(Value: double; Follower: TBoldFollowerCom);
  published
    property Renderer: TBoldAsFloatRendererCom read GetRenderer write SetRenderer;
  end;

implementation

uses
  SysUtils,
  BoldUtils,
  {!! DO NOT REMOVE !! BoldAttributes ,}
  BoldControlPackDefs,  
  BoldGuard;

var
  G_DefaultAsFloatRenderer: TBoldAsFloatRendererCom = nil;

{ TBoldAsFloatRendererCom }
class function TBoldAsFloatRendererCom.DefaultRenderer: TBoldAsFloatRendererCom;
begin
  Result := G_DefaultAsFloatRenderer;
end;

function TBoldAsFloatRendererCom.GetRendererDataClass: TBoldRendererDataClassCom;
begin
  Result := TBoldFloatRendererDataCom;
end;

function TBoldAsFloatRendererCom.DefaultMayModify(Element: IBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList; Subscriber: TBoldComClientSubscriber): Boolean;
{$IFNDEF BOLDCOMCLIENT}
var
  ValueElement: IBoldElement;
{$ENDIF}
begin
  {$IFDEF BOLDCOMCLIENT}
  result := inherited DefaultMayModify(Element, Representation, Expression, VariableList, Subscriber);
  {$ELSE}
  ValueElement := GetExpressionAsDirectElement(Element, Expression, VariableList);
  result := ((ValueElement is TBAFloat) or (ValueElement is TBADateTime)) and ValueElement.ObserverMayModify(Subscriber)
  {$ENDIF}
end;

function TBoldAsFloatRendererCom.DefaultGetAsFloatAndSubscribe(Element: IBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList; Subscriber: TBoldComClientSubscriber): double;
var
  {$IFDEF BOLDCOMCLIENT}
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
    {$IFDEF BOLDCOMCLIENT}
    if assigned(Subscriber) then
      el := Element.EvaluateAndSubscribeToExpression(Expression, Subscriber.ClientId, Subscriber.SubscriberId, false, false)
    else
      el := Element.EvaluateExpression(Expression);
    if el.QueryInterface(IBoldAttribute, Attr) = S_OK then
      result := Attr.AsVariant
    else
      raise EBold.CreateFmt('Can''t get float value from element (%s)', [Attr.AsString])
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
        raise EBold.CreateFmt('Can''t get float value from element (%s)', [IndirectElement.Value.ClassName])
    end;
    {$ENDIF}
  end;
end;

procedure TBoldAsFloatRendererCom.DefaultSetAsFloat(Element: IBoldElement; const Value: double; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList);
var
  ValueElement: IBoldElement;
  {$IFDEF BOLDCOMCLIENT}
  attr: IBoldAttribute;
  {$ENDIF}
begin
  ValueElement := GetExpressionAsDirectElement(Element, Expression , VariableList);
  {$IFDEF BOLDCOMCLIENT}
  if assigned(ValueElement) and (ValueElement.QueryInterface(IBoldAttribute, attr) = S_OK) then
    Attr.AsVariant := Value
  else
    raise EBold.CreateFmt('Can''t set float value on element (%s)', [ValueElement.AsString]);
  {$ELSE}
  if ValueElement is TBAFloat then
    TBAFloat(ValueElement).AsFloat := Value
  else if ValueElement is TBADateTime then
    TBADateTime(ValueElement).AsDateTime := Value
  else
    raise EBold.CreateFmt('Can''t set float value on element (%s)', [ValueElement.ClassName]);
  {$ENDIF}
end;

function TBoldAsFloatRendererCom.GetAsFloatAndSubscribe(Element: IBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList; Subscriber: TBoldComClientSubscriber): double;
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

procedure TBoldAsFloatRendererCom.SetAsFloat(Element: IBoldElement; const Value: double; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList);
begin
  if Assigned(FOnSetAsFloat) then
    OnSetAsFloat(Element, Value, Representation, Expression)
  else
    DefaultSetAsFloat(Element, Value, Representation, Expression, VariableList)
end;

procedure TBoldAsFloatRendererCom.MakeUptodateAndSubscribe(Element: IBoldElement; RendererData: TBoldFollowerDataCom; FollowerController: TBoldFollowerControllerCom; Subscriber: TBoldComClientSubscriber);
var
  Value: double;
begin
  Value := GetAsFloatAndSubscribe(Element,
    (FollowerController as TBoldFloatFollowerControllerCom).Representation,
    (FollowerController as TBoldFloatFollowerControllerCom).Expression,
    FollowerController.GetVariableListAndSubscribe(Subscriber),
    Subscriber);
  with (RendererData as TBoldFloatRendererDataCom) do
  begin
    OldFloatValue := Value;
    CurrentFloatValue := Value;
  end;
end;

function TBoldAsFloatRendererCom.DefaultIsChanged(RendererData: TBoldFloatRendererDataCom; const NewValue: double; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList): Boolean;
begin
  Result := NewValue <> RendererData.OldFloatValue;
end;

function TBoldAsFloatRendererCom.IsChanged(RendererData: TBoldFloatRendererDataCom; const NewValue: double; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList): Boolean;
begin
  if Assigned(FOnIsChanged) then
    Result := FOnIsChanged(RendererData, NewValue, Representation, Expression)
  else
    Result := DefaultIsChanged(RendererData, NewValue, Representation, Expression, VariableList);
end;

{ TBoldFloatFollowerControllerCom }
function TBoldFloatFollowerControllerCom.GetRenderer: TBoldAsFloatRendererCom;
begin
  Assert(not Assigned(UntypedRenderer) or (UntypedRenderer is TBoldAsFloatRendererCom));
  Result := TBoldAsFloatRendererCom(UntypedRenderer);
end;

procedure TBoldFloatFollowerControllerCom.SetRenderer(Value: TBoldAsFloatRendererCom);
begin
  if Value <> Renderer then
  begin
    if Assigned(Value) then
      Value.FreeNotification(OwningComponent);
    UntypedRenderer := Value;
    Changed;
  end;
end;

function TBoldFloatFollowerControllerCom.GetEffectiveRenderer: TBoldRendererCom;
begin
  Assert (not Assigned(UntypedRenderer) or (UntypedRenderer is TBoldAsFloatRendererCom));
  if Assigned(UntypedRenderer) then
    Result := UntypedRenderer
  else
    Result := G_DefaultAsFloatRenderer;
end;

function TBoldFloatFollowerControllerCom.GetEffectiveAsFloatRenderer: TBoldAsFloatRendererCom;
begin
  Assert (not Assigned(UntypedRenderer) or (UntypedRenderer is TBoldAsFloatRendererCom));
  if Assigned(UntypedRenderer) then
    Result := TBoldAsFloatRendererCom(UntypedRenderer)
  else
    Result := G_DefaultAsFloatRenderer;
end;

procedure TBoldFloatFollowerControllerCom.MakeClean(Follower: TBoldFollowerCom);
begin
  ReleaseChangedValue(Follower);
  SetAsFloat(GetCurrentAsFloat(Follower), Follower);
end;

function TBoldFloatFollowerControllerCom.GetCurrentAsFloat(Follower: TBoldFollowerCom): double;
begin
  Result := (Follower.RendererData as TBoldFloatRendererDataCom).CurrentFloatValue;
end;

procedure TBoldFloatFollowerControllerCom.SetAsFloat(Value: double; Follower: TBoldFollowerCom);
begin
  EffectiveAsFloatRenderer.SetAsFloat(Follower.Element, Value, Representation, Expression, VariableList);
end;

procedure TBoldFloatFollowerControllerCom.MayHaveChanged(NewValue: double; Follower: TBoldFollowerCom);
begin
  if Follower.State in bfsDisplayable then
  begin
    (Follower.RendererData as TBoldFloatRendererDataCom).CurrentFloatValue := NewValue;
    Follower.ControlledValueChanged(EffectiveAsFloatRenderer.IsChanged(Follower.RendererData as TBoldFloatRendererDataCom, NewValue, Representation, Expression, VariableList));
  end;
end;

initialization
  G_DefaultAsFloatRenderer := TBoldAsFloatRendererCom.Create(nil);

finalization
  FreeAndNil(G_DefaultAsFloatRenderer);

end.
