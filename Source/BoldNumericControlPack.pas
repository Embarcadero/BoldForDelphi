/////////////////////////////////////////////////////////


unit BoldNumericControlPack;

{$UNDEF BOLDCOMCLIENT}

interface

uses
  Classes,
  BoldDefs,
  BoldSubscription,
  BoldElements,
  BoldControlPack;

//TODO: Implement AsFloatRenderer.
//TODO: Find default numeric representation for class

type
  {Forward declarations}
  TBoldIntegerRendererData = class;
  TBoldAsIntegerRenderer = class;
  TBoldIntegerFollowerController = class;

  {TBoldAsIntegerRenderer}
  TBoldGetAsIntegerEvent = function (aFollower: TBoldFollower): Integer of object;
  TBoldSetAsIntegerEvent = procedure (aFollower: TBoldFollower; const Value: Integer) of object;
  TBoldIntegerIsChangedEvent = function (aFollower: TBoldFollower; const NewValue: Integer): Boolean of object;

  { TBoldIntegerRendererData }
  TBoldIntegerRendererData = class(TBoldRendererData)
  private
    FOldIntegerValue: Integer;
    FCurrentIntegerValue: Integer;
  public
    property CurrentIntegerValue: Integer read FCurrentIntegerValue write FCurrentIntegerValue;
    property OldIntegerValue: Integer read FOldIntegerValue write FOldIntegerValue;
  end;

  { TBoldAsIntegerRenderer }
  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldAsIntegerRenderer = class(TBoldsingleRenderer)
  private
    FOnGetAsInteger: TBoldGetAsIntegerEvent;
    FOnSetAsInteger: TBoldSetAsIntegerEvent;
    FOnIsChanged: TBoldIntegerIsChangedEvent;
  protected
    function GetRendererDataClass: TBoldRendererDataClass; override;
    function DefaultGetAsIntegerAndSubscribe(aFollower: TBoldFollower; Subscriber: TBoldSubscriber): Integer; virtual;
    procedure DefaultSetAsInteger(aFollower: TBoldFollower; const Value: Integer); virtual;
    function GetAsIntegerAndSubscribe(aFollower: TBoldFollower; Subscriber: TBoldSubscriber): Integer; virtual;
    procedure SetAsInteger(aFollower: TBoldFollower; const Value: Integer); virtual;
  public
    class function DefaultRenderer: TBoldAsIntegerRenderer;
    function DefaultIsChanged(aFollower: TBoldFollower; const NewValue: Integer): Boolean;
    function DefaultMayModify(aFollower: TBoldFollower): Boolean; override;
    procedure MakeUptodateAndSubscribe(aFollower: TBoldFollower; Subscriber: TBoldSubscriber); override;
    function IsChanged(aFollower: TBoldFollower; const NewValue: Integer): Boolean;
  published
    property OnGetAsInteger: TBoldGetAsIntegerEvent read FOnGetAsInteger write FOnGetAsInteger;
    property OnSetAsInteger: TBoldSetAsIntegerEvent read FOnSetAsInteger write FOnSetAsInteger;
    property OnIsChanged: TBoldIntegerIsChangedEvent read FOnIsChanged write FOnIsChanged;
  end;

  { TBoldIntegerFollowerController }
  TBoldIntegerFollowerController = class(TBoldSingleFollowerController)
  private
    function GetRenderer: TBoldAsIntegerRenderer;
    procedure SetRenderer(Value: TBoldAsIntegerRenderer);
    function GetEffectiveAsIntegerRenderer: TBoldAsIntegerRenderer;
  protected
    function GetEffectiveRenderer: TBoldRenderer; override;
    property EffectiveAsIntegerRenderer: TBoldAsIntegerRenderer read GetEffectiveAsIntegerRenderer;
  public
    function GetCurrentAsInteger(Follower: TBoldFollower): Integer;
    procedure MakeClean(Follower: TBoldFollower); override;
    procedure MayHaveChanged(NewValue: Integer; Follower: TBoldFollower);
    procedure SetAsInteger(Value: Integer; Follower: TBoldFollower);
  published
    property Renderer: TBoldAsIntegerRenderer read GetRenderer write SetRenderer;
  end;

implementation

uses
  SysUtils,
  BoldUtils,
  BoldGuiResourceStrings,
  BoldAttributes,
  BoldControlPackDefs,
  BoldGuard;

var
  DefaultAsIntegerRenderer: TBoldAsIntegerRenderer = nil;

{ TBoldAsIntegerRenderer }
class function TBoldAsIntegerRenderer.DefaultRenderer: TBoldAsIntegerRenderer;
begin
  Result := DefaultAsIntegerRenderer;
end;

function TBoldAsIntegerRenderer.GetRendererDataClass: TBoldRendererDataClass;
begin
  Result := TBoldIntegerRendererData;
end;

function TBoldAsIntegerRenderer.DefaultMayModify(aFollower: TBoldFollower): Boolean;
{$IFNDEF BOLDCOMCLIENT} // defaultMayModify
var
  ValueElement: TBoldElement;
begin
  // Note! We don't call inherited DefaultMayModify to prevent evaluation of expression two times!
  ValueElement := aFollower.Value;
  result := (ValueElement is TBANumeric) and ValueElement.ObserverMayModify(aFollower.Subscriber)
end;
{$ELSE}
begin
  result := inherited DefaultMayModify(Element, Representation, Expression, VariableList, Subscriber);
end;
{$ENDIF}

function TBoldAsIntegerRenderer.DefaultGetAsIntegerAndSubscribe(aFollower: TBoldFollower; Subscriber: TBoldSubscriber): Integer;
var
  {$IFDEF BOLDCOMCLIENT} // defaultGet
  el: IBoldElement;
  attr: IBoldAttribute;
  {$ELSE}
  IndirectElement: TBoldIndirectElement;
  lResultElement: TBoldElement;
  lGuard: IBoldGuard;
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
    if assigned(el) and (el.QueryInterface(IBoldAttribute, attr) = S_OK) then
      result := round(attr.AsVariant)
    else
      raise EBold.CreateFmt(sCantGetIntegerValue, [el.AsString])
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
      if (lResultElement is TBAInteger) then
        Result := TBAInteger(lResultElement).AsInteger
      else if (lResultElement is TBANumeric) then
        Result := Round(TBANumeric(lResultElement).AsFloat)
      else
        raise EBold.CreateFmt(sCantGetIntegerValue, [lResultElement.ClassName])
    end;
    {$ENDIF}
  end;
end;

procedure TBoldAsIntegerRenderer.DefaultSetAsInteger(aFollower: TBoldFollower; const Value: Integer);
var
  ValueElement: TBoldElement;
  {$IFDEF BOLDCOMCLIENT} // defaultSet
  Attr: IBoldAttribute;
  {$ENDIF}
begin
  ValueElement := aFollower.Value;
  {$IFDEF BOLDCOMCLIENT} // defaultSet
  if assigned(ValueElement) and (ValueElement.QueryInterface(IBoldAttribute, attr) = S_OK) then
    attr.AsVariant := Value
  else
    raise EBold.CreateFmt(sCantSetIntegerValue, [ValueElement.AsString]);
  {$ELSE}
  if ValueElement is TBANumeric then
    TBANumeric(ValueElement).AsInteger := Value
  else
    raise EBold.CreateFmt(sCantSetIntegerValue, [ValueElement.ClassName]);
  {$ENDIF}
end;

function TBoldAsIntegerRenderer.GetAsIntegerAndSubscribe(aFollower: TBoldFollower; Subscriber: TBoldSubscriber): Integer;
begin
  if Assigned(OnSubscribe) and Assigned(Subscriber) then
  begin
    if Assigned(aFollower.Element) then
      OnSubscribe(aFollower, Subscriber);
    Subscriber := nil;
  end;
  if Assigned(OnGetAsInteger) then
    Result := OnGetAsInteger(aFollower)
  else
    Result := DefaultGetAsIntegerAndSubscribe(aFollower, Subscriber);
end;

procedure TBoldAsIntegerRenderer.SetAsInteger(aFollower: TBoldFollower; const Value: Integer);
begin
  if Assigned(FOnSetAsInteger) then
    OnSetAsInteger(aFollower, Value)
  else
    DefaultSetAsInteger(aFollower, Value)
end;

procedure TBoldAsIntegerRenderer.MakeUpToDateAndSubscribe(aFollower: TBoldFollower; Subscriber: TBoldSubscriber);
var
  Value: Integer;
begin
  Value := GetAsIntegerAndSubscribe(aFollower ,Subscriber);
  (aFollower.RendererData as TBoldIntegerRendererData).OldIntegerValue := Value;
  (aFollower.RendererData as TBoldIntegerRendererData).CurrentIntegerValue := Value;
end;

function TBoldAsIntegerRenderer.DefaultIsChanged(aFollower: TBoldFollower; const NewValue: Integer): Boolean;
begin
  Result := NewValue <> TBoldIntegerRendererData(aFollower.RendererData).OldIntegerValue;
end;

function TBoldAsIntegerRenderer.IsChanged(aFollower: TBoldFollower; const NewValue: Integer): Boolean;
begin
  if Assigned(FOnIsChanged) then
    Result := FOnIsChanged(aFollower, NewValue)
  else
    Result := DefaultIsChanged(aFollower, NewValue);
end;

{ TBoldIntegerFollowerController }

function TBoldIntegerFollowerController.GetRenderer: TBoldAsIntegerRenderer;
begin
  Result := UntypedRenderer as TBoldAsIntegerRenderer;
end;

procedure TBoldIntegerFollowerController.SetRenderer(Value: TBoldAsIntegerRenderer);
begin
  UntypedRenderer := Value;
end;

function TBoldIntegerFollowerController.GetEffectiveRenderer: TBoldRenderer;
begin
  Result := EffectiveAsIntegerRenderer;
end;

function TBoldIntegerFollowerController.GetEffectiveAsIntegerRenderer: TBoldAsIntegerRenderer;
begin
  if Assigned(Renderer) then
    Result := Renderer
  else
    Result := DefaultAsIntegerRenderer; //FIXME
end;

procedure TBoldIntegerFollowerController.MakeClean(Follower: TBoldFollower);
begin
//  if (ApplyPolicy <> bapChange) or EffectiveRenderer.ChangedValueEventsAssigned then
  begin
    ReleaseChangedValue(Follower); // note, must do first, since set can change element
  end;
  SetAsInteger(GetCurrentAsInteger(Follower), Follower);
end;

function TBoldIntegerFollowerController.GetCurrentAsInteger(Follower: TBoldFollower): Integer;
begin
  Result := (Follower.RendererData as TBoldIntegerRendererData).CurrentIntegerValue;
end;

procedure TBoldIntegerFollowerController.SetAsInteger(Value: Integer; Follower: TBoldFollower);
begin
  EffectiveAsIntegerRenderer.SetAsInteger(Follower, Value);
end;

procedure TBoldIntegerFollowerController.MayHaveChanged(NewValue: Integer; Follower: TBoldFollower);
var
  lIsChanged: boolean;
  lRendererData: TBoldIntegerRendererData;
begin
  if Follower.State in bfsDisplayable then
  begin
    lRendererData := (Follower.RendererData as TBoldIntegerRendererData);
    lRendererData.CurrentIntegerValue := NewValue;
    lIsChanged := EffectiveAsIntegerRenderer.IsChanged(Follower, NewValue);
    if lIsChanged then
    begin
      Follower.ControlledValueChanged;
    end;
  end;
end;

initialization
  DefaultAsIntegerRenderer := TBoldAsIntegerRenderer.Create(nil);

finalization
  FreeAndNil(DefaultAsIntegerRenderer);

end.


