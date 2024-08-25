
{ Global compiler directives }
{$include bold.inc}
unit BoldListHandleCom;

interface

uses
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
  BoldComObjectSpace,
  BoldComObjectSpace_TLB,
  BoldComUtils,
  BoldDefs;

{-- TBoldListHandleCom --------------------------------------------------------}

destructor TBoldListHandleCom.Destroy;
begin
  ConnectionClosing;
  inherited;
end;

procedure TBoldListHandleCom.ClearAllValues;
begin
  FDynamicBoldType := nil;
  FStaticBoldType := nil;
  FStaticSystemTypeInfo := nil;
  FValue := nil;
  FHandleId := 0;

  FStaticRootType := nil;
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
      raise EBold.Create('Expression is read-only');
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
  FHandleId := BoldGetNamedValue(NamedValues,'HandleId');
  FCount := BoldGetNamedValue(NamedValues,'Count');
  if not OwnsHandleOnServer then
  begin
    FEnabled := BoldGetNamedValue(NamedValues,'Enabled');
    FRootTypeName := BoldGetNamedValue(NamedValues,'RootTypeName');
    FSubscribe := BoldGetNamedValue(NamedValues,'Subscribe');
    FCurrentIndex := BoldGetNamedValue(NamedValues,'CurrentIndex');
    FAutoFirst := BoldGetNamedValue(NamedValues,'AutoFirst');
    FExpression := BoldGetNamedValue(NamedValues,'Expression');
    fEvaluateInPS := BoldGetNamedValue(NamedValues, 'EvaluateInPS');
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
    ['StaticSystemHandle',
    'Enabled',
    'RootHandle',
    'RootTypeName',
    'Subscribe',
    'CurrentIndex',
    'AutoFirst',
    'Expression',
    'EvaluateInPS'],
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
  result := 'TBoldListHandle';
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
      raise EBold.Create('EvaluateInPS is read-only');
    fEvaluateInPS := Value;
    LocalValueChanged;
  end;
end;

end.
