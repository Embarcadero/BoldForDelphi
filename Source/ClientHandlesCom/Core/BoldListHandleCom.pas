unit BoldListHandleCom;

interface

uses
  BoldComObjectSpace,
  BoldComObjectSpace_TLB,
  BoldCursorHandleCom;

type
  { forward declarations }
  TBoldListHandleCom = class;

  {-- TBoldListHandleCom --}
  TBoldListHandleCom = class(TBoldCursorHandleCom)
  private
    FExpression: string;
    fEvaluateInPS: Boolean;
    function GetExpression: string;
    procedure SetExpression(const Value: string);
    function GetEvaluateInPS: boolean;
    procedure SetEvaluateInPS(const Value: boolean);
  protected
    procedure ClearAllValues; override;
    function ServerHandleClassName: string; override;
    procedure ValuesFromServer; override;
    procedure ValuesToServer; override;
  public
    destructor Destroy; override;
  published
    property Expression: string read GetExpression write SetExpression;
    property EvaluateInPS: boolean read GetEvaluateInPS write SetEvaluateInPS;
  end;

implementation

uses
  SysUtils,
  ComHandlesConst,
  BoldComHandlesConst,
  BoldUtils,
  BoldDefs,
  BoldComUtils;

{-- TBoldListHandleCom --------------------------------------------------------}

destructor TBoldListHandleCom.Destroy;
begin
  ConnectionClosing;
  inherited;
end;

procedure TBoldListHandleCom.ClearAllValues;
begin
  // from TBoldElementHandleCom
  FDynamicBoldType := nil;
  FStaticBoldType := nil;
  FStaticSystemTypeInfo := nil;
  FValue := nil;
  FHandleId := 0;
  // from TBoldNonSystemHandleCom
  // from TBoldRootedHandleCom
  FStaticRootType := nil;
  // from TBoldAbstractListHandleCom
  FCount := 0;
  FCurrentBoldObject := nil;
  FCurrentIndex := -1;
  FList := nil;
  FListElementType := nil;
end;

function TBoldListHandleCom.GetExpression: string;
begin
  if not OwnsHandleOnServer then
    EnsureCurrent;
  Result := FExpression;
end;
{
procedure TBoldListHandleCom.SetComparerName(const Value: string);
begin
  if Value <> FComparerName then
  begin
    if not OwnsHandleOnServer then
      raise EBold.Create('BoldComparerName is read-only');
    FComparerName := Value;
    LocalValueChanged;
  end;
end;
}
procedure TBoldListHandleCom.SetExpression(const Value: string);
begin
  if Value <> FExpression then
  begin
    if not OwnsHandleOnServer then
      raise EBold.CreateFmt(sPropertyIsReadOnly, ['Expression']); // do not localize
    FExpression := Value;
    LocalValueChanged;
  end;
end;
{
procedure TBoldListHandleCom.SetFilterName(const Value: string);
begin
  if Value <> FFilterName then
  begin
    if not OwnsHandleOnServer then
      raise EBold.Create('BoldFilterName is read-only');
    FFilterName := Value;
    LocalValueChanged;
  end;
end;
}
procedure TBoldListHandleCom.ValuesFromServer;
var
  NamedValues: OleVariant;
  DummySystem: IBoldSystem;
begin
  ServerElementHandle.GetData(0,
    FValue,
    FDynamicBoldType,
    FStaticBoldType,
    FStaticSystemTypeInfo,
    DummySystem,
    FStaticRootType,
    FCurrentBoldObject,
    FList,
    FListElementType,
    NamedValues);
  FHandleId := BoldGetNamedValue(NamedValues, nv_HandleId);
  FCount := BoldGetNamedValue(NamedValues, nv_Count);
  if not OwnsHandleOnServer then
  begin
    FEnabled := BoldGetNamedValue(NamedValues, nv_Enabled);
    FRootTypeName := BoldGetNamedValue(NamedValues, nv_RootTypeName);
    FSubscribe := BoldGetNamedValue(NamedValues, nv_Subscribe);
    FCurrentIndex := BoldGetNamedValue(NamedValues, nv_CurrentIndex);
    FAutoFirst := BoldGetNamedValue(NamedValues, nv_AutoFirst);
    FExpression := BoldGetNamedValue(NamedValues, nv_Expression);
    fEvaluateInPS := BoldGetNamedValue(NamedValues, nv_EvaluateInPS);
  end
  else
    AdjustCurrentIndex;
end;

procedure TBoldListHandleCom.ValuesToServer;
var
  DataFlags: Integer;
  NamedValues: OleVariant;
  RootHandleId,StaticSystemHandleId: Integer;
begin
  DataFlags := DF_STATICSYSTEMHANDLE or DF_ENABLED or DF_ROOTHANDLE or
    DF_ROOTTYPENAME or DF_SUBSCRIBE or DF_CURRENTINDEX or DF_AUTOFIRST or
    DF_EXPRESSION or DF_EVALUATEINPS;
  if Assigned(StaticSystemHandle) then
    StaticSystemHandleId := StaticSystemHandle.HandleId
  else
    StaticSystemHandleId := 0;
  if Assigned(RootHandle) then
    RootHandleId := RootHandle.HandleId
  else
    RootHandleId := 0;
  NamedValues := BoldCreateNamedValues(
    [nv_StaticSystemHandle,
     nv_Enabled,
     nv_RootHandle,
     nv_RootTypeName,
     nv_Subscribe,
     nv_CurrentIndex,
     nv_AutoFirst,
     nv_Expression,
     nv_EvaluateInPS],
    [StaticSystemHandleId,
     FEnabled,
     RootHandleId,
     FRootTypeName,
     FSubscribe,
     FCurrentIndex,
     FAutoFirst,
     FExpression,
     fEvaluateInPS]);
  ServerElementHandle.SetData(DataFlags,nil,NamedValues);
end;

function TBoldListHandleCom.ServerHandleClassName: string;
begin
  result := ServerHandleClassName_ListHandle;
end;

function TBoldListHandleCom.GetEvaluateInPS: boolean;
begin
  if not OwnsHandleOnServer then
    EnsureCurrent;
  Result := fEvaluateInPS;
end;

procedure TBoldListHandleCom.SetEvaluateInPS(const Value: boolean);
begin
  if Value <> fEvaluateInPS then
  begin
    if not OwnsHandleOnServer then
      raise EBold.CreateFmt(sPropertyIsReadOnly, ['EvaluateInPS']); // do not localize
    fEvaluateInPS := Value;
    LocalValueChanged;
  end;
end;

end.
