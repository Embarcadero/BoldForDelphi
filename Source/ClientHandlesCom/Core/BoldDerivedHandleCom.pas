unit BoldDerivedHandleCom;

interface

uses
  BoldRootedHandlesCom;

type
  { forward declarations }
  TBoldDerivedHandleCom = class;

  {-- TBoldDerviedHandle --}
  TBoldDerivedHandleCom = class(TBoldRootedHandleCom)
  protected
    procedure ClearAllValues; override;
    function ServerHandleClassName: string; override;
    procedure ValuesFromServer; override;
    procedure ValuesToServer; override;
  public
    destructor Destroy; override;
  end;

implementation

uses
  BoldComObjectSpace,
  BoldComObjectSpace_TLB,
  ComHandlesConst,
  BoldComUtils;

{-- TBoldDerivedHandleCom --------------------------------------------------------}

destructor TBoldDerivedHandleCom.Destroy;
begin
  ConnectionClosing;
  inherited;
end;

procedure TBoldDerivedHandleCom.ClearAllValues;
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
  // from TBoldDerivedHandleCom
end;


procedure TBoldDerivedHandleCom.ValuesFromServer;
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
  FHandleId := BoldGetNamedValue(NamedValues, nv_HandleId);
  if not OwnsHandleOnServer then
  begin
    FEnabled := BoldGetNamedValue(NamedValues, nv_Enabled);
    FRootTypeName := BoldGetNamedValue(NamedValues, nv_RootTypeName);
    FSubscribe := BoldGetNamedValue(NamedValues, nv_Subscribe);
  end;
end;

procedure TBoldDerivedHandleCom.ValuesToServer;
var
  DataFlags: Integer;
  NamedValues: OleVariant;
  RootHandleId,StaticSystemHandleId: Integer;
begin
  DataFlags := DF_STATICSYSTEMHANDLE or DF_ENABLED or DF_ROOTHANDLE or
    DF_ROOTTYPENAME or DF_SUBSCRIBE;
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
    nv_Subscribe],
    [StaticSystemHandleId,
    FEnabled,
    RootHandleId,
    FRootTypeName,
    FSubscribe]);
  ServerElementHandle.SetData(DataFlags,nil,NamedValues);
end;

function TBoldDerivedHandleCom.ServerHandleClassName: string;
begin
  result := ServerHandleClassName_DerivedHandle;
end;

end.
