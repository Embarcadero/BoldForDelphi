
{ Global compiler directives }
{$include bold.inc}
unit BoldExpressionHandleCom;

interface

uses
  BoldRootedHandlesCom,
  BoldVariableDefinitionCom;

type
  { forward declarations }
  TBoldExpressionHandleCom = class;

  {-- TBoldExpressionHandleCom --}
  TBoldExpressionHandleCom = class(TBoldRootedHandleCom)
  private
    FExpression: string;
    fEvaluateInPS: Boolean;
    FVariables: TBoldVariableDefinitionCom;
    function GetExpression: string;
    function GetVariables: TBoldVariableDefinitionCom;
    procedure SetExpression(const Value: string);
    procedure SetVariables(Value: TBoldVariableDefinitionCom);
    function GetEvaluateInPS: boolean;
    procedure SetEvaluateInPS(const Value: boolean);
  protected
    function ServerHandleClassName: string; override;
    procedure ClearAllValues; override;
    procedure ValuesFromServer; override;
    procedure ValuesToServer; override;
  public
    destructor Destroy; override;
  published
    property Expression: string read GetExpression write SetExpression;
    property Subscribe;
    property Variables: TBoldVariableDefinitionCom read GetVariables write SetVariables;
    property EvaluateInPS: boolean read GetEvaluateInPS write SetEvaluateInPS;
  end;

implementation

uses
  SysUtils,
  BoldComObjectSpace,
  BoldComObjectSpace_TLB,
  BoldComUtils,
  BoldDefs;

{-- TBoldExpressionHandleCom --------------------------------------------------}

destructor TBoldExpressionHandleCom.Destroy;
begin
  ConnectionClosing;
  inherited;
end;

procedure TBoldExpressionHandleCom.ClearAllValues;
begin
  FDynamicBoldType := nil;
  FStaticBoldType := nil;
  FStaticSystemTypeInfo := nil;
  FValue := nil;
  FHandleId := 0;

  FStaticRootType := nil;
end;


function TBoldExpressionHandleCom.GetExpression: string;
begin
  if not OwnsHandleOnServer then
    EnsureCurrent;
  Result := FExpression;
end;

function TBoldExpressionHandleCom.GetVariables: TBoldVariableDefinitionCom;
begin
  if not OwnsHandleOnServer then
    EnsureCurrent;
  Result := FVariables;
end;

procedure TBoldExpressionHandleCom.ValuesFromServer;
var
  NamedValues: OleVariant;
  DummySystem: IBoldSystem;
  DummyObject: IBoldObject;
  DummyList: IBoldList;
  DummyListType: IBoldElementTypeInfo;
begin
  ServerElementHandle.GetData(0,
    FValue,
    FDynamicBoldType,
    FStaticBoldType,
    FStaticSystemTypeInfo,
    DummySystem,
    FStaticRootType,
    DummyObject,
    DummyList,
    DummyListType,
    NamedValues);
  FHandleId := BoldGetNamedValue(NamedValues,'HandleId');
  if not OwnsHandleOnServer then
  begin
    FEnabled := BoldGetNamedValue(NamedValues,'Enabled');
    FRootTypeName := BoldGetNamedValue(NamedValues,'RootTypeName');
    FSubscribe := BoldGetNamedValue(NamedValues,'Subscribe');
    FExpression := BoldGetNamedValue(NamedValues,'Expression');
    fEvaluateInPS := BoldGetNamedValue(NamedValues, 'EvaluateInPS');
  end;
end;

procedure TBoldExpressionHandleCom.ValuesToServer;
var
  DataFlags: Integer;
  NamedValues: OleVariant;
  RootHandleId,StaticSystemHandleId: Integer;
begin
  DataFlags := DF_STATICSYSTEMHANDLE or DF_ENABLED or DF_ROOTHANDLE or
    DF_ROOTTYPENAME or DF_SUBSCRIBE or DF_EXPRESSION or DF_EVALUATEINPS;
  if Assigned(StaticSystemHandle) then
    StaticSystemHandleId := StaticSystemHandle.HandleId
  else
    StaticSystemHandleId := 0;
  if Assigned(RootHandle) then
    RootHandleId := RootHandle.HandleId
  else
    RootHandleId := 0;
  NamedValues := BoldCreateNamedValues(
    ['StaticSystemHandle',
    'Enabled',
    'RootHandle',
    'RootTypeName',
    'Subscribe',
    'Expression',
    'EvaluateInPS'],
    [StaticSystemHandleId,
    FEnabled,
    RootHandleId,
    FRootTypeName,
    FSubscribe,
    FExpression,
    fEvaluateInPS]);
  ServerElementHandle.SetData(DataFlags,nil,NamedValues);
end;


procedure TBoldExpressionHandleCom.SetExpression(const Value: string);
begin
  if Value <> FExpression then
  begin
    if not OwnsHandleOnServer then
      raise EBold.Create('Expression is read-only');
    FExpression := Value;
    LocalValueChanged;
  end;
end;

procedure TBoldExpressionHandleCom.SetVariables(Value: TBoldVariableDefinitionCom);
begin
  if Value <> Variables then
  begin
    if not OwnsHandleOnServer then
      raise EBold.Create('Variables is read-only');
    FVariables := Value;
    LocalValueChanged;
  end;
end;


function TBoldExpressionHandleCom.ServerHandleClassName: string;
begin
  result := 'TBoldExpressionHandle';
end;

function TBoldExpressionHandleCom.GetEvaluateInPS: boolean;
begin
  if not OwnsHandleOnServer then
    EnsureCurrent;
  Result := fEvaluateInPS;
end;

procedure TBoldExpressionHandleCom.SetEvaluateInPS(const Value: boolean);
begin
  if Value <> fEvaluateInPS then
  begin
    if not OwnsHandleOnServer then
      raise EBold.Create('EvaluateInPs is read-only');
    fEvaluateInPS := Value;
    LocalValueChanged;
  end;
end;

end.
