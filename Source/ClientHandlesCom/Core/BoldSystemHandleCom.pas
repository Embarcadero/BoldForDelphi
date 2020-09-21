unit BoldSystemHandleCom;

interface

uses
  BoldComObjectSpace,
  BoldComObjectSpace_TLB,
  BoldHandlesCom;

type
  { forward declarations }
  TBoldSystemHandleCom = class;

  {-- TBoldSystemHandleCom --}
  TBoldSystemHandleCom = class(TBoldAbstractSystemHandleCom)
  private
    FPersistent: Boolean;
    function GetPersistent: Boolean;
  protected
    function ServerHandleClassName: string; override;
    procedure ClearAllValues; override;
    procedure ValuesFromServer; override;
    procedure ValuesToServer; override;
  public
    destructor Destroy; override;
    procedure UpdateDatabase;
    property Persistent: Boolean read GetPersistent;
  end;

implementation

uses
  SysUtils,
  ComHandlesConst,
  BoldComHandlesConst,
  BoldUtils,
  BoldDefs,
  BoldComUtils;

{-- TBoldSystemHandleCom ------------------------------------------------------}

destructor TBoldSystemHandleCom.Destroy;
begin
  ConnectionClosing;
  inherited;
end;

procedure TBoldSystemHandleCom.ClearAllValues;
begin
  // from TBoldElementHandleCom
  FDynamicBoldType := nil;
  FStaticBoldType := nil;
  FStaticSystemTypeInfo := nil;
  FValue := nil;
  FHandleId := 0;
  // from TBoldAbstractSystemHandleCom
  FBoldSystem := nil;
  FSystemActive := False;
  // from TBoldSystemHandleCom
  FPersistent := False;
end;

function TBoldSystemHandleCom.GetPersistent: Boolean;
begin
  EnsureCurrent;
  Result := FPersistent;
end;

procedure TBoldSystemHandleCom.UpdateDatabase;
begin
  EnsureCurrent;
  if Assigned(System) then
    System.UpdateDatabase
  else
    raise EBold.Create(sNotConnected);
end;

procedure TBoldSystemHandleCom.ValuesFromServer;
var
  NamedValues: OleVariant;
  DummyType: IBoldElementTypeInfo;
  DummyObject: IBoldObject;
  DummyList: IBoldList;
  DummyListType: IBoldElementTypeInfo;
begin
  ServerElementHandle.GetData(0,
    FValue,
    FDynamicBoldType,
    FStaticBoldType,
    FStaticSystemTypeInfo,
    FBoldSystem,
    DummyType,
    DummyObject,
    DummyList,
    DummyListType,
    NamedValues);
  FHandleId := BoldGetNamedValue(NamedValues, nv_HandleId);
  FSystemActive := BoldGetNamedValue(NamedValues, nv_Active);
  FPersistent := BoldGetNamedValue(NamedValues, nv_Persistent);
end;

procedure TBoldSystemHandleCom.ValuesToServer;
begin
  // Nothing to set, the SystemHandle is always read-only.
end;

function TBoldSystemHandleCom.ServerHandleClassName: string;
begin
  result := ServerHandleClassName_SystemHandle;
end;

end.
