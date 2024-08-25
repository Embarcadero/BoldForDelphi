
{ Global compiler directives }
{$include bold.inc}
unit BoldVariableHandleCom;

interface

uses
  Classes,
  BoldComObjectSpace,
  BoldComObjectSpace_TLB,
  BoldHandlesCom;

type
  { forward declarations }
  TBoldVariableHandleCom = class;

  {-- TBoldVariableHandleCom --}
  TBoldVariableHandleCom = class(TBoldNonSystemHandleCom)
  private
    FValueTypeName: string;
    FInitialValues: TStrings;
    function GetValueTypeName: string;
    function GetInitialValues: TStrings;
    procedure InitialValuesChanged(Sender: TObject);
    procedure SetValueTypeName(const Value: string);
    procedure SetInitialValues(Value: TStrings);
    procedure ReadExpressionName(Reader: TReader);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure ClearAllValues; override;
    function ServerHandleClassName: string; override;
    procedure ValuesFromServer; override;
    procedure ValuesToServer; override;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
  published
    property ValueTypeName: string read GetValueTypeName write SetValueTypeName;
    property InitialValues: TStrings read GetInitialValues write SetInitialValues;
  end;

implementation

uses
  SysUtils,
  BoldUtils,
  BoldDefs,
  BoldComUtils;

{-- TBoldVariableHandleCom ----------------------------------------------------}

constructor TBoldVariableHandleCom.Create(Owner: TComponent);
begin
  inherited;
  FInitialValues := TStringList.Create;
  TStringList(FInitialValues).OnChange := InitialValuesChanged;
end;

destructor TBoldVariableHandleCom.Destroy;
begin
  ConnectionClosing;
  FreeAndNil(FInitialValues);
  inherited;
end;

procedure TBoldVariableHandleCom.ClearAllValues;
begin
  FDynamicBoldType := nil;
  FStaticBoldType := nil;
  FStaticSystemTypeInfo := nil;
  FValue := nil;
  FHandleId := 0;

end;

function TBoldVariableHandleCom.GetValueTypeName: string;
begin
  if not OwnsHandleOnServer then
    EnsureCurrent;
  Result := FValueTypeName;
end;

function TBoldVariableHandleCom.GetInitialValues: TStrings;
begin
  if not OwnsHandleOnServer then
    EnsureCurrent;
  Result := FInitialValues;
end;

procedure TBoldVariableHandleCom.InitialValuesChanged(Sender: TObject);
begin
  LocalValueChanged;
end;

procedure TBoldVariableHandleCom.SetValueTypeName(const Value: string);
begin
  if Value <> FValueTypeName then
  begin
    if not OwnsHandleOnServer then
      raise EBold.CreateFmt('%s.ValueTypeName is read-only', [ClassName]);
    FValueTypeName := Value;
    LocalValueChanged;
  end;
end;

procedure TBoldVariableHandleCom.SetInitialValues(Value: TStrings);
begin
  if Value <> FInitialValues then
  begin
    if not OwnsHandleOnServer then
      raise EBold.CreateFmt('%s.InitialValues is read-only', [ClassName]);
    FInitialValues.Assign(Value);
    LocalValueChanged;
  end;
end;

procedure TBoldVariableHandleCom.ValuesFromServer;
var
  NamedValues: OleVariant;
  DummySystem: IBoldSystem;
  DummyType: IBoldElementTypeInfo;
  DummyObject: IBoldObject;
  DummyList: IBoldList;
  DummyListType: IBoldElementTypeInfo;
  Temp: TStringList;
begin
  ServerElementHandle.GetData(0,
    FValue,
    FDynamicBoldType,
    FStaticBoldType,
    FStaticSystemTypeInfo,
    DummySystem,
    DummyType,
    DummyObject,
    DummyList,
    DummyListType,
    NamedValues);
  FHandleId := BoldGetNamedValue(NamedValues, 'HandleId');
  if not OwnsHandleOnServer then
  begin
    FValueTypeName := BoldGetNamedValue(NamedValues, 'ValueTypeName');
    Temp := TStringList.Create;
    try
      BoldVariantToStrings(BoldGetNamedValue(NamedValues, 'InitialValues'),Temp);
      FInitialValues.Assign(Temp);
    finally
      Temp.Free;
    end;
  end;
end;

procedure TBoldVariableHandleCom.ValuesToServer;
var
  DataFlags: Integer;
  NamedValues: OleVariant;
  StaticSystemHandleId: Integer;
begin
  DataFlags := DF_STATICSYSTEMHANDLE or DF_VALUETYPENAME or DF_INITIALVALUES;
  if Assigned(StaticSystemHandle) then
    StaticSystemHandleId := StaticSystemHandle.HandleId
  else
    StaticSystemHandleId := 0;
  NamedValues := BoldCreateNamedValues(
    ['StaticSystemHandle',
    'ValueTypeName',
    'InitialValues'],
    [StaticSystemHandleId,
    FValueTypeName,
    BoldStringsToVariant(FInitialValues)]);
  ServerElementHandle.SetData(DataFlags, nil, NamedValues);
end;

function TBoldVariableHandleCom.ServerHandleClassName: string;
begin
  result := 'TBoldVariableHandle';
end;

procedure TBoldVariableHandleCom.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('ExpressionName', ReadExpressionName, nil, True);
end;

procedure TBoldVariableHandleCom.ReadExpressionName(Reader: TReader);
begin
  ValueTypeName := Reader.ReadString;
end;

end.
