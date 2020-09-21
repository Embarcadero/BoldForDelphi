unit BoldExpressionHandle;

interface

uses
  Classes,
  BoldDefs,
  BoldSubscription,
  BoldElements,
  BoldRootedHandles,
  BoldOclVariables;

type
  { forward declaration of classes }
  TBoldExpressionHandle = class;

  {---TBoldExpressionHandle---}
  TBoldExpressionHandle = class(TBoldRootedHandle, IBoldOCLComponent)
  function IBoldOCLComponent.GetContextType = GetStaticRootType;
  private
    fExpression: string;
    FVariables: TBoldOclVariables;
    fVariablesSubscriber: TBoldPassThroughSubscriber;
    fEvaluateInPS: Boolean;
    procedure _VariablesReceive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    procedure SetExpression(Value: string);
    function GetExpression: String;
    procedure SetVariables(const Value: TBoldOclVariables);
    function GetVariableList: TBoldExternalVariableList;
    procedure SetEvaluateInPS(const Value: Boolean);
   protected
    function GetStaticBoldType: TBoldElementTypeInfo; override;
    procedure DeriveAndSubscribe(DerivedObject: TObject; Subscriber: TBoldSubscriber); override;
  public
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;
    function RefersToComponent(Component: TBoldSubscribableComponent): Boolean; override;
  published
    property Expression: TBoldExpression read fExpression write SetExpression;
    property Variables: TBoldOclVariables read FVariables write SetVariables;
    property EvaluateInPS: Boolean read fEvaluateInPS write SetEvaluateInPS default false;
    {$IFNDEF T2H}
    property Subscribe;
    {$ENDIF}
  end;

implementation

uses
  SysUtils,
  HandlesConst;

const
  breVariablesDestroyed = 200;

{---TBoldExpressionHandle---}

function TBoldExpressionHandle.GetStaticBoldType: TBoldElementTypeInfo;
begin
  if Assigned(StaticRootType) then
  begin
    if assigned(Variables) then
      Result := StaticRootType.Evaluator.ExpressionType(Expression, StaticRootType, False, Variables.VariableList)
    else
      Result := StaticRootType.Evaluator.ExpressionType(Expression, StaticRootType, False);
  end
  else
    Result := nil;
end;

procedure TBoldExpressionHandle.SetExpression(Value: string);
begin
  if Value <> fExpression then
  begin
    fExpression := Value;
    MarkSubscriptionOutOfdate;
  end;
end;

procedure TBoldExpressionHandle.DeriveAndSubscribe(DerivedObject: TObject;
  Subscriber: TBoldSubscriber);
var
  RootValue: TBoldElement;
  vars: TBoldExternalVariableList;
begin
  RootValue := EffectiveRootValue;
  if Assigned(RootValue) then
  begin
    if assigned(Variables) then
    begin
      Vars := variables.VariableList;
      variables.SubscribeToHandles(Subscriber);
    end
    else
      vars := nil;
    try
      RootValue.EvaluateAndSubscribeToExpression(Expression, Subscriber, ResultElement, False, EvaluateInPS, vars)
    except
      on e: Exception do
      begin
        e.message := format(sDeriveAndSubscribeFailed,
          [ClassName, Name, e.Message]);
        raise
      end;
    end;
  end else
    ResultElement.SetOwnedValue(nil);
  SubscribeToValue;
end;

function TBoldExpressionHandle.GetExpression: String;
begin
  result := fExpression;
end;

procedure TBoldExpressionHandle.SetVariables(const Value: TBoldOclVariables);
begin
  if Value <> Variables then
  begin
    if assigned(value) and value.LinksToHandle(self) then
      raise EBold.CreateFmt(sCircularReference, [classname, name, value.name]);
    FVariables := Value;
    StaticBoldTypeChanged;
    fVariablesSubscriber.CancelAllSubscriptions;
    if assigned(Value) then
      Value.AddSmallSubscription(fVariablesSubscriber, [beDestroying], breVariablesDestroyed);
  end;
end;

procedure TBoldExpressionHandle.SetEvaluateInPS(const Value: Boolean);
begin
  if Value <> fEvaluateInPS then
  begin
    fEvaluateInPS := Value;
    MarkSubscriptionOutOfDate;
  end;
end;

procedure TBoldExpressionHandle._VariablesReceive(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  if (Originator = FVariables) and (RequestedEvent = breVariablesDestroyed) then
    FVariables := nil;
end;

constructor TBoldExpressionHandle.create(owner: TComponent);
begin
  inherited;
  fVariablesSubscriber := TBoldPassthroughSubscriber.create(_VariablesReceive);
end;

destructor TBoldExpressionHandle.Destroy;
begin
  FreeAndNil(fVariablesSubscriber);
  inherited;
end;

function TBoldExpressionHandle.GetVariableList: TBoldExternalVariableList;
begin
  if assigned(fVariables) then
    result := fVariables.VariableList
  else
    result := nil;
end;

function TBoldExpressionHandle.RefersToComponent(Component: TBoldSubscribableComponent): Boolean;
begin
  result := inherited RefersToComponent(Component);
  if not result and assigned(Component) then
    result := Component = variables;
end;

end.
