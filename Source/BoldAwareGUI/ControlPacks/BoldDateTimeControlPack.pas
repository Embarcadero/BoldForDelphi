
{ Global compiler directives }
{$include bold.inc}
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
  TBoldGetAsDateTimeEvent = function (aFollower: TBoldFollower): TDateTime of object;
  TBoldSetAsDateTimeEvent = procedure (aFollower: TBoldFollower; const Value: TDateTime) of object;
  TBoldDateTimeIsChangedEvent = function (aFollower: TBoldFollower; const NewValue: TDateTime): Boolean of object;

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
    function DefaultGetAsDateTimeAndSubscribe(aFollower: TBoldFollower; Subscriber: TBoldSubscriber): TDateTime; virtual;
    procedure DefaultSetAsDateTime(aFollower: TBoldFollower; const Value: TDateTime); virtual;
    function GetAsDateTimeAndSubscribe(aFollower: TBoldFollower; Subscriber: TBoldSubscriber): TDateTime; virtual;
    procedure SetAsDateTime(aFollower: TBoldFollower; const Value: TDateTime); virtual;
    function HasSetValueEventOverrides: boolean; override;
  public
    procedure MakeUptodateAndSubscribe(aFollower: TBoldFollower; Subscriber: TBoldSubscriber); override;
    function DefaultMayModify(aFollower: TBoldFollower): Boolean; override;
    function DefaultIsChanged(aFollower: TBoldFollower; const NewValue: TDateTime): Boolean;
    function IsChanged(aFollower: TBoldFollower; const NewValue: TDateTime): Boolean;
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

function TBoldAsDateTimeRenderer.HasSetValueEventOverrides: boolean;
begin
  result := Assigned(FOnSetAsDateTime);
end;

function TBoldAsDateTimeRenderer.DefaultMayModify(aFollower: TBoldFollower): Boolean;
{$IFNDEF BOLDCOMCLIENT}
var
  ValueElement: TBoldElement;
{$ENDIF}  
begin
  {$IFDEF BOLDCOMCLIENT}
  result := inherited DefaultMayModify(Element, Representation, Expression, VariableList, Subscriber);
  {$ELSE}
  ValueElement := aFollower.Value;
  result := (ValueElement is TBAMoment) and ValueElement.ObserverMayModify(aFollower.Subscriber);
  {$ENDIF}
end;

function TBoldAsDateTimeRenderer.DefaultGetAsDateTimeAndSubscribe(aFollower: TBoldFollower; Subscriber: TBoldSubscriber): TDateTime;
var
  {$IFDEF BOLDCOMCLIENT}
  el: IBoldElement;
  attr: IBoldAttribute;
  {$ELSE}
  lBoldIndirectElement: TBoldIndirectElement;
  lBoldGuard: IBoldGuard;
  lResultElement: TBoldElement;
  lBAMoment: TBAMoment;
  {$ENDIF}
begin
  Result := 0;
  if Assigned(aFollower.Element) then
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
    lResultElement := aFollower.Value;
    if not Assigned(lResultElement) then
    begin
      lBoldGuard:= TBoldGuard.Create(lBoldIndirectElement);
      lBoldIndirectElement := TBoldIndirectElement.Create;
      aFollower.Element.EvaluateAndSubscribeToExpression(aFollower.AssertedController.Expression, Subscriber, lBoldIndirectElement, False, False, aFollower.Controller.GetVariableListAndSubscribe(Subscriber));
      lResultElement := lBoldIndirectElement.Value;
    end;
    if Assigned(lResultElement) then
    begin
      if (lResultElement is TBAMoment) then
      begin
        lBAMoment := lResultElement as TBAMoment;
        if lBAMoment.IsNull then
        begin
          Result := 0; //TODO Null will display as 0 which isn't best
        end
        else if (lBAMoment is TBADateTime) then
        begin
          Result := TBADateTime(lBAMoment).AsDateTime
        end
        else if (lBAMoment is TBADate) then
        begin
          Result := TBADate(lBAMoment).AsDate
        end
        else if (lBAMoment is TBATime) then
        begin
          Result := TBATime(lBAMoment).AsTime
        end
      end
      else
        raise EBold.CreateFmt('Can''t get datetime value from element (%s)', [lResultElement.ClassName])
    end;
    {$ENDIF}
  end;
end;

procedure TBoldAsDateTimeRenderer.DefaultSetAsDateTime(aFollower: TBoldFollower; const Value: TDateTime);
var
  {$IFDEF BOLDCOMCLIENT}
  Attr: IBoldAttribute;
  {$ENDIF}
  ValueElement: TBoldElement;
begin
  ValueElement := aFollower.Value;
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

function TBoldAsDateTimeRenderer.GetAsDateTimeAndSubscribe(aFollower: TBoldFollower; Subscriber: TBoldSubscriber): TDateTime;
begin
  if Assigned(OnSubscribe) and Assigned(Subscriber) then
  begin
    if Assigned(aFollower.Element) then
      OnSubscribe(aFollower, Subscriber);
    Subscriber := nil;
  end;
  if Assigned(OnGetAsDateTime) then
    Result := OnGetAsDateTime(aFollower)
  else
    Result := DefaultGetAsDateTimeAndSubscribe(aFollower, Subscriber);
end;

procedure TBoldAsDateTimeRenderer.SetAsDateTime(aFollower: TBoldFollower; const Value: TDateTime);
begin
  if Assigned(FOnSetAsDateTime) then
    OnSetAsDateTime(aFollower, Value)
  else
    DefaultSetAsDateTime(aFollower, Value)
end;

procedure TBoldAsDateTimeRenderer.MakeUptodateAndSubscribe(aFollower: TBoldFollower; Subscriber: TBoldSubscriber);
var
  lDateTime: TDateTime;
  lRendererData: TBoldDateTimeRendererData;
begin
  lDateTime := GetAsDateTimeAndSubscribe(aFollower, subscriber);
  lRendererData := aFollower.RendererData as TBoldDateTimeRendererData;
  lRendererData.OldDateTimeValue := lDateTime;
  lRendererData.CurrentDateTimeValue := lDateTime;
end;

function TBoldAsDateTimeRenderer.DefaultIsChanged(aFollower: TBoldFollower; const NewValue: TDateTime): Boolean;
begin
  Result := NewValue <> TBoldDateTimeRendererData(aFollower.RendererData).OldDateTimeValue;
end;

function TBoldAsDateTimeRenderer.IsChanged(aFollower: TBoldFollower; const NewValue: TDateTime): Boolean;
begin
  if Assigned(FOnIsChanged) then
    Result := FOnIsChanged(aFollower, NewValue)
  else
    Result := DefaultIsChanged(aFollower, NewValue);
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
    Result := DefaultAsDateTimeRenderer;
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
  EffectiveAsDateTimeRenderer.SetAsDateTime(Follower, Value);
end;

procedure TBoldDateTimeFollowerController.MayHaveChanged(NewValue: TDateTime; Follower: TBoldFollower);
var
  lIsChanged: boolean;
  lRendererData: TBoldDateTimeRendererData;
begin
  if Follower.State in bfsDisplayable then
  begin
    lRendererData := (Follower.RendererData as TBoldDateTimeRendererData);
    lRendererData.CurrentDateTimeValue := NewValue;
    lIsChanged := EffectiveAsDateTimeRenderer.IsChanged(Follower, NewValue);
    if lIsChanged then
    begin
      Follower.ControlledValueChanged;
    end;
  end;
end;

initialization
  DefaultAsDateTimeRenderer := TBoldAsDateTimeRenderer.Create(nil);

finalization
  FreeAndNil(DefaultAsDateTimeRenderer);

end.
