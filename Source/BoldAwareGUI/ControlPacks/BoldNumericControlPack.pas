unit BoldNumericControlPack;

{$UNDEF BOLDCOMCLIENT}

interface

uses
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
  TBoldGetAsIntegerEvent = function (Element: TBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression): Integer of object;
  TBoldSetAsIntegerEvent = procedure (Element: TBoldElement; const Value: Integer; Representation: TBoldRepresentation; Expression: TBoldExpression) of object;
  TBoldIntegerIsChangedEvent = function (RenderData: TBoldIntegerRendererData; const NewValue: Integer; Representation: TBoldRepresentation; Expression: TBoldExpression): Boolean of object;

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
  TBoldAsIntegerRenderer = class(TBoldsingleRenderer)
  private
    FOnGetAsInteger: TBoldGetAsIntegerEvent;
    FOnSetAsInteger: TBoldSetAsIntegerEvent;
    FOnIsChanged: TBoldIntegerIsChangedEvent;
  protected
    function GetRendererDataClass: TBoldRendererDataClass; override;
    function DefaultGetAsIntegerAndSubscribe(Element: TBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList; Subscriber: TBoldSubscriber): Integer; virtual;
    procedure DefaultSetAsInteger(Element: TBoldElement; const Value: Integer; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList); virtual;
    function GetAsIntegerAndSubscribe(Element: TBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList; Subscriber: TBoldSubscriber): Integer; virtual;
    procedure SetAsInteger(Element: TBoldElement; const Value: Integer; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList); virtual;
  public
    class function DefaultRenderer: TBoldAsIntegerRenderer;
    function DefaultIsChanged(RendererData: TBoldIntegerRendererData; const NewValue: Integer; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList): Boolean;
    function DefaultMayModify(Element: TBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList; Subscriber: TBoldSubscriber): Boolean; override;
    procedure MakeUptodateAndSubscribe(Element: TBoldElement; RendererData: TBoldRendererData; FollowerController: TBoldFollowerController; Subscriber: TBoldSubscriber); override;
    function IsChanged(RendererData: TBoldIntegerRendererData; const NewValue: Integer; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList): Boolean;
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
  BoldControlPackDefs;

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

function TBoldAsIntegerRenderer.DefaultMayModify(Element: TBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList; Subscriber: TBoldSubscriber): Boolean;
{$IFNDEF BOLDCOMCLIENT} // defaultMayModify
var
  ValueElement: TBoldElement;
begin
  // Note! We don't call inherited DefaultMayModify to prevent evaluation of expression two times!
  ValueElement := GetExpressionAsDirectElement(Element, Expression, VariableList);
  result := (ValueElement is TBANumeric) and ValueElement.ObserverMayModify(Subscriber)
end;
{$ELSE}
begin
  result := inherited DefaultMayModify(Element, Representation, Expression, VariableList, Subscriber);
end;
{$ENDIF}

function TBoldAsIntegerRenderer.DefaultGetAsIntegerAndSubscribe(Element: TBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList; Subscriber: TBoldSubscriber): Integer;
var
  {$IFDEF BOLDCOMCLIENT} // defaultGet
  el: IBoldElement;
  attr: IBoldAttribute;
  {$ELSE}
  IndirectElement: TBoldIndirectElement;
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
    if assigned(el) and (el.QueryInterface(IBoldAttribute, attr) = S_OK) then
      result := round(attr.AsVariant)
    else
      raise EBold.CreateFmt(sCantGetIntegerValue, [el.AsString])
    {$ELSE}
    IndirectElement := TBoldIndirectElement.Create;
    try
      Element.EvaluateAndSubscribeToExpression(Expression, Subscriber, IndirectElement, False, False, VariableList);
      if Assigned(IndirectElement.Value) then
      begin
        if (IndirectElement.Value is TBAInteger) then
          Result := (IndirectElement.Value as TBAInteger).AsInteger
        else if (IndirectElement.Value is TBANumeric) then
          Result := Round((IndirectElement.Value as TBANumeric).AsFloat)
        else
          raise EBold.CreateFmt(sCantGetIntegerValue, [IndirectElement.Value.ClassName])
      end;
    finally
      IndirectElement.Free;
    end;
    {$ENDIF}
  end;
end;

procedure TBoldAsIntegerRenderer.DefaultSetAsInteger(Element: TBoldElement; const Value: Integer; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList);
var
  ValueElement: TBoldElement;
  {$IFDEF BOLDCOMCLIENT} // defaultSet
  Attr: IBoldAttribute;
  {$ENDIF}
begin
  ValueElement := GetExpressionAsDirectElement(Element, Expression, VariableList);
  {$IFDEF BOLDCOMCLIENT} // defaultSet
  if assigned(ValueElement) and (ValueElement.QueryInterface(IBoldAttribute, attr) = S_OK) then
    attr.AsVariant := Value
  else
    raise EBold.CreateFmt(sCantSetIntegerValue, [ValueElement.AsString]);
  {$ELSE}
  if ValueElement is TBANumeric then
    (ValueElement as TBANumeric).AsInteger := Value
  else
    raise EBold.CreateFmt(sCantSetIntegerValue, [ValueElement.ClassName]);
  {$ENDIF}
end;

function TBoldAsIntegerRenderer.GetAsIntegerAndSubscribe(Element: TBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList; Subscriber: TBoldSubscriber): Integer;
begin
  if Assigned(OnSubscribe) and Assigned(Subscriber) then
  begin
    if Assigned(Element) then
      OnSubscribe(Element, Representation, Expression, Subscriber);
    Subscriber := nil;
  end;
  if Assigned(OnGetAsInteger) then
    Result := OnGetAsInteger(Element, Representation, Expression)
  else
    Result := DefaultGetAsIntegerAndSubscribe(Element, Representation, Expression, VariableList, Subscriber);
end;

procedure TBoldAsIntegerRenderer.SetAsInteger(Element: TBoldElement; const Value: Integer; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList);
begin
  if Assigned(FOnSetAsInteger) then
    OnSetAsInteger(Element, Value, Representation, Expression)
  else
    DefaultSetAsInteger(Element, Value, Representation, Expression, VariableList)
end;

procedure TBoldAsIntegerRenderer.MakeUpToDateAndSubscribe(Element: TBoldElement; RendererData: TBoldRendererData; FollowerController: TBoldFollowerController; Subscriber: TBoldSubscriber);
var
  Value: Integer;
  Controller: TBoldIntegerFollowerController;
begin
  Controller := FollowerController as TBoldIntegerFollowerController;
  Value := GetAsIntegerAndSubscribe(Element, Controller.Representation, Controller.Expression, Controller.GetVariableListAndSubscribe(Subscriber) ,Subscriber);
  with (RendererData as TBoldIntegerRendererData) do
  begin
    OldIntegerValue := Value;
    CurrentIntegerValue := Value;
  end;
end;

function TBoldAsIntegerRenderer.DefaultIsChanged(RendererData: TBoldIntegerRendererData; const NewValue: Integer; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList): Boolean;
begin
  Result := NewValue <> RendererData.OldIntegerValue;
end;

function TBoldAsIntegerRenderer.IsChanged(RendererData: TBoldIntegerRendererData; const NewValue: Integer; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList): Boolean;
begin
  if Assigned(FOnIsChanged) then
    Result := FOnIsChanged(RendererData, NewValue, Representation, Expression)
  else
    Result := DefaultIsChanged(RendererData, NewValue, Representation, Expression, VariableList);
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
  ReleaseChangedValue(Follower);
  SetAsInteger(GetCurrentAsInteger(Follower), Follower);
end;

function TBoldIntegerFollowerController.GetCurrentAsInteger(Follower: TBoldFollower): Integer;
begin
  Result := (Follower.RendererData as TBoldIntegerRendererData).CurrentIntegerValue;
end;

procedure TBoldIntegerFollowerController.SetAsInteger(Value: Integer; Follower: TBoldFollower);
begin
  EffectiveAsIntegerRenderer.SetAsInteger(Follower.Element, Value, Representation, Expression, GetVariableListAndSubscribe(follower.Subscriber));
end;

procedure TBoldIntegerFollowerController.MayHaveChanged(NewValue: Integer; Follower: TBoldFollower);
begin
  if Follower.State in bfsDisplayable then
  begin
    (Follower.RendererData as TBoldIntegerRendererData).CurrentIntegerValue := NewValue;
    Follower.ControlledValueChanged(EffectiveAsIntegerRenderer.IsChanged(Follower.RendererData as TBoldIntegerRendererData, NewValue, Representation, Expression, GetVariableListAndSubscribe(follower.Subscriber)));
  end;
end;

initialization
  DefaultAsIntegerRenderer := TBoldAsIntegerRenderer.Create(nil);

finalization
  FreeAndNil(DefaultAsIntegerRenderer);

end.

