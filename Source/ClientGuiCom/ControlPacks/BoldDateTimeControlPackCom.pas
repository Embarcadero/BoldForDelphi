
{ Global compiler directives }
{$include bold.inc}
unit BoldDateTimeControlPackCom;

{$DEFINE BOLDCOMCLIENT} {Clientified 2002-08-05 13:13:02}

interface

uses
  BoldClientElementSupport,
  BoldComClient,
  BoldComObjectSpace_TLB,
  BoldControlPackCom,
  BoldDefs;

type
  { Forward declarations }
  TBoldDateTimeRendererDataCom = class;
  TBoldAsDateTimeRendererCom = class;
  TBoldDateTimeFollowerControllerCom = class;

  {TBoldAsDateTimeRendererCom}
  TBoldGetAsDateTimeEventCom = function (Element: IBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression): TDateTime of object;
  TBoldSetAsDateTimeEventCom = procedure (Element: IBoldElement; const Value: TDateTime; Representation: TBoldRepresentation; Expression: TBoldExpression) of object;
  TBoldDateTimeIsChangedEventCom = function (RenderData: TBoldDateTimeRendererDataCom; const NewValue: TDateTime; Representation: TBoldRepresentation; Expression: TBoldExpression): Boolean of object;

  { TBoldDateTimeRendererDataCom }
  TBoldDateTimeRendererDataCom = class(TBoldFollowerDataCom)
  private
    FOldDateTimeValue: TDateTime;
    FCurrentDateTimeValue: TDateTime;
  public
    property CurrentDateTimeValue: TDateTime read FCurrentDateTimeValue write FCurrentDateTimeValue;
    property OldDateTimeValue: TDateTime read FOldDateTimeValue write FOldDateTimeValue;
  end;

  { TBoldAsDateTimeRendererCom }
  TBoldAsDateTimeRendererCom = class(TBoldSingleRendererCom)
  private
    FOnGetAsDateTime: TBoldGetAsDateTimeEventCom;
    FOnSetAsDateTime: TBoldSetAsDateTimeEventCom;
    FOnIsChanged: TBoldDateTimeIsChangedEventCom;
  protected
    class function DefaultRenderer: TBoldAsDateTimeRendererCom;
    function GetRendererDataClass: TBoldRendererDataClassCom; override;
    function DefaultGetAsDateTimeAndSubscribe(Element: IBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList; Subscriber: TBoldComClientSubscriber): TDateTime; virtual;
    procedure DefaultSetAsDateTime(Element: IBoldElement; const Value: TDateTime; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList); virtual;
    function GetAsDateTimeAndSubscribe(Element: IBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList; Subscriber: TBoldComClientSubscriber): TDateTime; virtual;
    procedure SetAsDateTime(Element: IBoldElement; const Value: TDateTime; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList); virtual;
  public
    procedure MakeUptodateAndSubscribe(Element: IBoldElement; RendererData: TBoldFollowerDataCom; FollowerController: TBoldFollowerControllerCom; Subscriber: TBoldComClientSubscriber); override;
    function DefaultMayModify(Element: IBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList; Subscriber: TBoldComClientSubscriber): Boolean; override;
    function DefaultIsChanged(RendererData: TBoldDateTimeRendererDataCom; const NewValue: TDateTime; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList): Boolean;
    function IsChanged(RendererData: TBoldDateTimeRendererDataCom; const NewValue: TDateTime; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList): Boolean;
  published
    property OnGetAsDateTime: TBoldGetAsDateTimeEventCom read FOnGetAsDateTime write FOnGetAsDateTime;
    property OnSetAsDateTime: TBoldSetAsDateTimeEventCom read FOnSetAsDateTime write FOnSetAsDateTime;
    property OnIsChanged: TBoldDateTimeIsChangedEventCom read FOnIsChanged write FOnIsChanged;
  end;

  { TBoldDateTimeFollowerControllerCom }
  TBoldDateTimeFollowerControllerCom = class(TBoldSingleFollowerControllerCom)
  private
    function GetRenderer: TBoldAsDateTimeRendererCom;
    procedure SetRenderer(Value: TBoldAsDateTimeRendererCom);
    function GetEffectiveAsDateTimeRenderer: TBoldAsDateTimeRendererCom;
  protected
    function GetEffectiveRenderer: TBoldRendererCom; override;
    property EffectiveAsDateTimeRenderer: TBoldAsDateTimeRendererCom read GetEffectiveAsDateTimeRenderer;
  public
    function GetCurrentAsDateTime(Follower: TBoldFollowerCom): TDateTime;
    procedure  MakeClean(Follower: TBoldFollowerCom); override;
    procedure MayHaveChanged(NewValue: TDateTime; Follower: TBoldFollowerCom);
    procedure SetAsDateTime(Value: TDateTime; Follower: TBoldFollowerCom);
  published
    property Renderer: TBoldAsDateTimeRendererCom read GetRenderer write SetRenderer;
  end;

implementation

uses
  SysUtils,
  BoldControlPackDefs;

var
  DefaultAsDateTimeRenderer: TBoldAsDateTimeRendererCom = nil;

{ TBoldAsDateTimeRendererCom }
class function TBoldAsDateTimeRendererCom.DefaultRenderer: TBoldAsDateTimeRendererCom;
begin
  Result := DefaultAsDateTimeRenderer;
end;

function TBoldAsDateTimeRendererCom.GetRendererDataClass: TBoldRendererDataClassCom;
begin
  Result := TBoldDateTimeRendererDataCom;
end;

function TBoldAsDateTimeRendererCom.DefaultMayModify(Element: IBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList; Subscriber: TBoldComClientSubscriber): Boolean;
{$IFNDEF BOLDCOMCLIENT}
var
  ValueElement: IBoldElement;
{$ENDIF}  
begin
  {$IFDEF BOLDCOMCLIENT}
  result := inherited DefaultMayModify(Element, Representation, Expression, VariableList, Subscriber);
  {$ELSE}
  ValueElement := GetExpressionAsDirectElement(Element, Expression, VariableList);
  result := (ValueElement is TBAMoment) and ValueElement.ObserverMayModify(Subscriber);
  {$ENDIF}
end;

function TBoldAsDateTimeRendererCom.DefaultGetAsDateTimeAndSubscribe(Element: IBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList; Subscriber: TBoldComClientSubscriber): TDateTime;
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
    if Assigned(el) then
    begin
      if el.QueryInterface(IBoldAttribute, attr) = S_OK then
        Result := attr.AsVariant
      else
        raise EBold.CreateFmt('Can''t get datetime value from element (%s)', [Attr.Asstring])
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
        raise EBold.CreateFmt('Can''t get datetime value from element (%s)', [IndirectElement.Value.ClassName])
    end;
    {$ENDIF}
  end;
end;

procedure TBoldAsDateTimeRendererCom.DefaultSetAsDateTime(Element: IBoldElement; const Value: TDateTime; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList);
var
  {$IFDEF BOLDCOMCLIENT}
  Attr: IBoldAttribute;
  {$ENDIF}
  ValueElement: IBoldElement;
begin
  ValueElement := GetExpressionAsDirectElement(Element, Expression, VariableList);
  {$IFDEF BOLDCOMCLIENT}
  if assigned(ValueElement) and (ValueElement.QueryInterface(IBoldAttribute, Attr) = S_OK) then
    Attr.AsVariant := Value
  else
    raise EBold.CreateFmt('Can''t set datetime value on element (%s)', [ValueElement.AsString]);
  {$ELSE}
  if Assigned(ValueElement) and (ValueElement is TBADateTime) then
    TBADateTime(ValueElement).AsDateTime := Value
  else if Assigned(ValueElement) and (ValueElement is TBADate) then
    TBADate(ValueElement).AsDate := Value
  else if Assigned(ValueElement) and (ValueElement is TBATime) then
    TBATime(ValueElement).AsTime := Value
  else
    raise EBold.CreateFmt('Can''t set datetime value on element (%s)', [ValueElement.ClassName]);
  {$ENDIF}
end;

function TBoldAsDateTimeRendererCom.GetAsDateTimeAndSubscribe(Element: IBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList; Subscriber: TBoldComClientSubscriber): TDateTime;
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

procedure TBoldAsDateTimeRendererCom.SetAsDateTime(Element: IBoldElement; const Value: TDateTime; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList);
begin
  if Assigned(FOnSetAsDateTime) then
    OnSetAsDateTime(Element, Value, Representation, Expression)
  else
    DefaultSetAsDateTime(Element, Value, Representation, Expression, VariableList)
end;

procedure TBoldAsDateTimeRendererCom.MakeUptodateAndSubscribe(Element: IBoldElement; RendererData: TBoldFollowerDataCom; FollowerController: TBoldFollowerControllerCom; Subscriber: TBoldComClientSubscriber);
var
  Value: TDateTime;
begin
  Assert(FollowerController is TBoldDateTimeFollowerControllerCom);
  Value := GetAsDateTimeAndSubscribe(Element,
    TBoldDateTimeFollowerControllerCom(FollowerController).Representation,
    TBoldDateTimeFollowerControllerCom(FollowerController).Expression, FollowerController.GetVariableListAndSubscribe(Subscriber),
    subscriber);
  with (RendererData as TBoldDateTimeRendererDataCom) do
  begin
    OldDateTimeValue := Value;
    CurrentDateTimeValue := Value;
  end;
end;

function TBoldAsDateTimeRendererCom.DefaultIsChanged(RendererData: TBoldDateTimeRendererDataCom; const NewValue: TDateTime; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList): Boolean;
begin
  Result := NewValue <> RendererData.OldDateTimeValue;
end;

function TBoldAsDateTimeRendererCom.IsChanged(RendererData: TBoldDateTimeRendererDataCom; const NewValue: TDateTime; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList): Boolean;
begin
  if Assigned(FOnIsChanged) then
    Result := FOnIsChanged(RendererData, NewValue, Representation, Expression)
  else
    Result := DefaultIsChanged(RendererData, NewValue, Representation, Expression, VariableList);
end;

{ TBoldDateTimeFollowerControllerCom }

function TBoldDateTimeFollowerControllerCom.GetRenderer: TBoldAsDateTimeRendererCom;
begin
  Result := UntypedRenderer as TBoldAsDateTimeRendererCom;
end;

procedure TBoldDateTimeFollowerControllerCom.SetRenderer(Value: TBoldAsDateTimeRendererCom);
begin
  if Value <> Renderer then
  begin
    if Assigned(Value) then
      Value.FreeNotification(OwningComponent);
    UntypedRenderer := Value;
    Changed;
  end;
end;

function TBoldDateTimeFollowerControllerCom.GetEffectiveRenderer: TBoldRendererCom;
begin
  Result := EffectiveAsDateTimeRenderer;
end;

function TBoldDateTimeFollowerControllerCom.GetEffectiveAsDateTimeRenderer: TBoldAsDateTimeRendererCom;
begin
  if Assigned(Renderer) then
    Result := Renderer
  else
    Result := DefaultAsDateTimeRenderer;
end;

procedure TBoldDateTimeFollowerControllerCom.MakeClean(Follower: TBoldFollowerCom);
begin
  ReleaseChangedValue(Follower);
  SetAsDateTime(GetCurrentAsDateTime(Follower), Follower);
end;

function TBoldDateTimeFollowerControllerCom.GetCurrentAsDateTime(Follower: TBoldFollowerCom): TDateTime;
begin
  Result := (Follower.RendererData as TBoldDateTimeRendererDataCom).CurrentDateTimeValue;
end;

procedure TBoldDateTimeFollowerControllerCom.SetAsDateTime(Value: TDateTime; Follower: TBoldFollowerCom);
begin
  EffectiveAsDateTimeRenderer.SetAsDateTime(Follower.Element, Value, Representation, Expression, VariableList);
end;

procedure TBoldDateTimeFollowerControllerCom.MayHaveChanged(NewValue: TDateTime; Follower: TBoldFollowerCom);
begin
  if Follower.State in bfsDisplayable then
  begin
    (Follower.RendererData as TBoldDateTimeRendererDataCom).CurrentDateTimeValue := NewValue;
    Follower.ControlledValueChanged(EffectiveAsDateTimeRenderer.IsChanged(Follower.RendererData as TBoldDateTimeRendererDataCom, NewValue, Representation, Expression, VariableList));
  end;
end;   

initialization
  DefaultAsDateTimeRenderer := TBoldAsDateTimeRendererCom.Create(nil);

finalization
  FreeAndNil(DefaultAsDateTimeRenderer);

end.
