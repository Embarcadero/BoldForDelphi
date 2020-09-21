unit BoldCursorHandleCom;

interface

uses
  Classes,
  BoldAbstractListHandleCom;

type
  { forward declarations }
  TBoldCursorHandleCom = class;

  { TBoldCursorHandleCom }
  TBoldCursorHandleCom = class(TBoldAbstractListHandleCom)
  private
    function GetAutoFirst: Boolean;
    procedure SetAutoFirst(Value: Boolean);
  protected
    FAutoFirst: Boolean;
    procedure ClearAllValues; override;
    function ServerHandleClassName: string; override;
    procedure ValuesFromServer; override;
    procedure ValuesToServer; override;
    procedure AdjustCurrentIndex;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
  published
    property AutoFirst: Boolean read GetAutoFirst write SetAutoFirst default True;
  end;

implementation

uses
  SysUtils,
  BoldComObjectSpace,
  BoldComObjectSpace_TLB,
  BoldDefs,
  ComHandlesConst,
  BoldComHandlesConst,
  BoldComUtils;

{ TBoldCursorHandleCom }

destructor TBoldCursorHandleCom.Destroy;
begin
  ConnectionClosing;
  inherited;
end;

procedure TBoldCursorHandleCom.ClearAllValues;
begin
  // from TBoldElementHandleCom
  FDynamicBoldType      := nil;
  FStaticBoldType       := nil;
  FStaticSystemTypeInfo := nil;
  FValue                := nil;
  FHandleId             := 0;
  // from TBoldNonSystemHandleCom
  // from TBoldRootedHandleCom
  FStaticRootType := nil;
  // from TBoldAbstractListHandleCom
  FCount              := 0;
  FCurrentBoldObject  := nil;
  FCurrentIndex       := -1;
  FList               := nil;
  FListElementType    := nil;
end;

function TBoldCursorHandleCom.GetAutoFirst: Boolean;
begin
  if not OwnsHandleOnServer then
    EnsureCurrent;
  Result := FAutoFirst;
end;

procedure TBoldCursorHandleCom.SetAutoFirst(Value: Boolean);
begin
  if Value <> AutoFirst then
  begin
    if not OwnsHandleOnServer then
      raise EBold.Create(sAutoFirstIsReadOnly);
    FAutoFirst := Value;
    LocalValueChanged;
  end;
end;

procedure TBoldCursorHandleCom.ValuesFromServer;
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
  FCount    := BoldGetNamedValue(NamedValues, nv_Count);

  if not OwnsHandleOnServer then
  begin
    FEnabled      := BoldGetNamedValue(NamedValues, nv_Enabled);
    FRootTypeName := BoldGetNamedValue(NamedValues, nv_RootTypeName);
    FSubscribe    := BoldGetNamedValue(NamedValues, nv_Subscribe);
    FCurrentIndex := BoldGetNamedValue(NamedValues, nv_CurrentIndex);
    FAutoFirst    := BoldGetNamedValue(NamedValues, nv_AutoFirst);
  end
  else
    AdjustCurrentIndex;
end;

procedure TBoldCursorHandleCom.ValuesToServer;
var
  DataFlags: Integer;
  NamedValues: OleVariant;
  RootHandleId,
  StaticSystemHandleId: Integer;
begin
  DataFlags := DF_STATICSYSTEMHANDLE or
               DF_ENABLED or
               DF_ROOTHANDLE or
               DF_ROOTTYPENAME or
               DF_SUBSCRIBE or
               DF_CURRENTINDEX or
               DF_AUTOFIRST;

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
     nv_AutoFirst],
    [StaticSystemHandleId,
     FEnabled,
     RootHandleId,
     FRootTypeName,
     FSubscribe,
     FCurrentIndex,
     FAutoFirst]);
  ServerElementHandle.SetData(DataFlags, nil, NamedValues);
end;

function TBoldCursorHandleCom.ServerHandleClassName: string;
begin
  result := ServerHandleClassName_CursorHandle;
end;

constructor TBoldCursorHandleCom.Create(Owner: TComponent);
begin
  inherited;
  FAutoFirst := true;
end;

procedure TBoldCursorHandleCom.AdjustCurrentIndex;
begin
  if not assigned(fList) then
    fCurrentIndex := -1
  else if fAutoFirst and (fCurrentIndex = -1) and (fList.Count > 0) then
    fCurrentIndex := 0
  else if fCurrentIndex >= fList.Count then
    fCurrentIndex := fList.Count-1;
end;

end.
