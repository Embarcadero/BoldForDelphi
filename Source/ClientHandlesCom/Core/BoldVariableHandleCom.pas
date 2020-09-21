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
  BoldComHandlesConst,
  ComHandlesConst,
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
  // from TBoldElementHandleCom
  FDynamicBoldType := nil;
  FStaticBoldType := nil;
  FStaticSystemTypeInfo := nil;
  FValue := nil;
  FHandleId := 0;
  // from TBoldNonSystemHandleCom
  // from TBoldVariableHandleCom
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
      raise EBold.CreateFmt(sPropertyIsReadOnly, [ValueTypeName]);
    FValueTypeName := Value;
    LocalValueChanged;
  end;
end;

procedure TBoldVariableHandleCom.SetInitialValues(Value: TStrings);
begin
  if Value <> FInitialValues then
  begin
    if not OwnsHandleOnServer then
      raise EBold.CreateFmt(sPropertyIsReadOnly, [InitialValues]);
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
  FHandleId := BoldGetNamedValue(NamedValues, nv_HandleId);
  if not OwnsHandleOnServer then
  begin
    FValueTypeName := BoldGetNamedValue(NamedValues, nv_ValueTypeName);
    Temp := TStringList.Create;
    try
      BoldVariantToStrings(BoldGetNamedValue(NamedValues, nv_InitialValues),Temp);
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
    [nv_StaticSystemHandle,
     nv_ValueTypeName,
     nv_InitialValues],
    [StaticSystemHandleId,
     FValueTypeName,
     BoldStringsToVariant(FInitialValues)]);
  ServerElementHandle.SetData(DataFlags, nil, NamedValues);
end;

function TBoldVariableHandleCom.ServerHandleClassName: string;
begin
  result := ServerHandleClassName_VariableHandle;
end;

procedure TBoldVariableHandleCom.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  // ExpressionName changed name to ValueTypeName after 2.5
  Filer.DefineProperty('ExpressionName', ReadExpressionName, nil, True); // do not localize
end;

procedure TBoldVariableHandleCom.ReadExpressionName(Reader: TReader);
begin
  ValueTypeName := Reader.ReadString;
end;

end.
