
{ Global compiler directives }
{$include bold.inc}
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

  TBoldExpressionHandleClass = class of TBoldExpressionHandle;       

  {---TBoldExpressionHandle---}
  TBoldExpressionHandle = class(TBoldRootedHandle, IBoldOCLComponent)
  function IBoldOCLComponent.GetContextType = GetStaticRootType;
  private
    fExpression: string;
    FVariables: TBoldOclVariables;
    fVariablesSubscriber: TBoldPassThroughSubscriber;
    fEvaluateInPS: Boolean;
    fUsePrefetch: Boolean;
    procedure _VariablesReceive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    procedure SetExpression(const Value: TBoldExpression);
    function GetExpression: TBoldExpression;
    procedure SetVariables(const Value: TBoldOclVariables);
    procedure SetEvaluateInPS(const Value: Boolean);
   protected
    function GetStaticBoldType: TBoldElementTypeInfo; override;
    procedure DeriveAndSubscribe(DerivedObject: TObject; Subscriber: TBoldSubscriber); override;
    procedure DoAssign(Source: TPersistent); override;
    procedure DoExpressionChanged; virtual;
    function GetVariables: TBoldOclVariables; virtual;
    function GetVariableList: TBoldExternalVariableList; virtual;
    property VariableList: TBoldExternalVariableList read GetVariableList;
  public
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;
    function RefersToComponent(Component: TBoldSubscribableComponent): Boolean; override;
  published
    property Expression: TBoldExpression read GetExpression write SetExpression;
    property Variables: TBoldOclVariables read GetVariables write SetVariables;
    property EvaluateInPS: Boolean read fEvaluateInPS write SetEvaluateInPS default false;
    {$IFNDEF T2H}
    property Subscribe;
    {$ENDIF}
    property UsePrefetch: Boolean read fUsePrefetch write fUsePrefetch default true;
  end;

implementation

uses
  SysUtils,
  {$IFDEF SpanFetch}
  AttracsSpanFetchManager,
  {$ENDIF}  
  BoldRev;

const
  breVariablesDestroyed = 200;

{---TBoldExpressionHandle---}

function TBoldExpressionHandle.GetStaticBoldType: TBoldElementTypeInfo;
var
  vStaticRootType: TBoldElementTypeInfo;
begin
  vStaticRootType := StaticRootType;
  if Assigned(vStaticRootType) then
    Result := vStaticRootType.Evaluator.ExpressionType(Expression, vStaticRootType, True, VariableList)
  else
    Result := nil;
end;

procedure TBoldExpressionHandle.SetExpression(const Value: TBoldExpression);
begin
  if Value <> fExpression then
  begin
    fExpression := Value;
    MarkSubscriptionOutOfdate;
    DoExpressionChanged;
  end;
end;

procedure TBoldExpressionHandle.DeriveAndSubscribe(DerivedObject: TObject;
  Subscriber: TBoldSubscriber);

  function GetFullName(AComponent: TComponent): string;
  begin
    result := AComponent.GetNamePath;
    if result = '' then
      result := '('+AComponent.ClassName+')';
    if (AComponent is TComponent) and Assigned(TComponent(AComponent).Owner) then
      result := GetFullName(TComponent(AComponent).Owner) +  '.' + result;
  end;

var
  RootValue: TBoldElement;
  vars: TBoldExternalVariableList;
begin
  if csDestroying in ComponentState then
    raise EBold.CreateFmt('%s.DeriveAndSubscribe: %s Handle is in csDestroying state, can not DeriveAndSubscribe.', [classname, name]);
  RootValue := EffectiveRootValue;
  if Assigned(RootValue) then
  begin
    if assigned(Variables) then
      variables.SubscribeToHandles(Subscriber, Expression);
    Vars := VariableList;
    try
    begin
{$IFDEF SpanFetch}
    if UsePrefetch and not EvaluateInps then
      FetchOclSpan(RootValue, Expression, vars);
{$ENDIF}
      if Assigned(RootValue.BoldType) then // ValueSetValue has no BoldType
        RootValue.EvaluateAndSubscribeToExpression(Expression, Subscriber, ResultElement, False, EvaluateInPS, vars)
    end;
    except
      on e: Exception do
      begin
        e.message := format('%s.DeriveAndSubscribe (%s): Failed with message: %s',
          [ClassName, GetFullName(self), e.Message]);
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
      raise EBold.CreateFmt('%s.SetVariables: %s can not be linked to %s. Circular reference', [classname, name, value.name]);
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

procedure TBoldExpressionHandle.DoAssign(Source: TPersistent);
begin
  inherited;
  if Source is TBoldExpressionHandle then with TBoldExpressionHandle(Source) do
  begin
    self.Expression := Expression;
    self.Variables := Variables;
    self.EvaluateInPS := EvaluateInPS;
  end;
end;

procedure TBoldExpressionHandle.DoExpressionChanged;
begin
end;

constructor TBoldExpressionHandle.Create(owner: TComponent);
begin
  inherited;
  fVariablesSubscriber := TBoldPassthroughSubscriber.create(_VariablesReceive);
  fUsePrefetch := true;
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

function TBoldExpressionHandle.GetVariables: TBoldOclVariables;
begin
  result := fVariables;
end;

function TBoldExpressionHandle.RefersToComponent(Component: TBoldSubscribableComponent): Boolean;
begin
  result := inherited RefersToComponent(Component);
  if not result and assigned(Component) then
    result := Component = variables;
end;

initialization
end.

