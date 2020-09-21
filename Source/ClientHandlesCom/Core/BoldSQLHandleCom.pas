unit BoldSQLHandleCom;

interface

uses
  Classes,
  BoldComObjectSpace,
  BoldComObjectSpace_TLB,
  BoldHandlesCom;

type
  { forward declarations }
  TBoldSQLHandleCom = class;

  {-- TBoldSQLHandleCom --}
  TBoldSQLHandleCom = class(TBoldNonSystemHandleCom)
  private
    FClassExpressionName: string;
    FClearBeforeExecute: Boolean;
    FSQLOrderByClause: string;
    FSQLWhereClause: string;
    function GetClassExpressionName: string;
    function GetClearBeforeExecute: Boolean;
    function GetSQLOrderByClause: string;
    function GetSQLWhereClause: string;
    procedure SetClassExpressionName(const Value: string);
    procedure SetClearBeforeExecute(Value: Boolean);
    procedure SetSQLOrderByClause(const Value: string);
    procedure SetSQLWhereClause(const Value: string);
  protected
    procedure ClearAllValues; override;
    function ServerHandleClassName: string; override;
    procedure ValuesFromServer; override;
    procedure ValuesToServer; override;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure ClearList;
    procedure ExecuteSQL;
  published
    property ClassExpressionName: string read GetClassExpressionName write SetClassExpressionName;
    property ClearBeforeExecute: Boolean read GetClearBeforeExecute write SetClearBeforeExecute default True;
    property SQLOrderByClause: string read GetSQLOrderByClause write SetSQLOrderByClause;
    property SQLWhereClause: string read GetSQLWhereClause write SetSQLWhereClause;
  end;

implementation

uses
  SysUtils,
  BoldComHandlesConst,
  ComHandlesConst,
  BoldUtils,
  BoldDefs,
  BoldComUtils;

constructor TBoldSQLHandleCom.Create(Owner: TComponent);
begin
  inherited;
  FClearBeforeExecute := True;
end;

destructor TBoldSQLHandleCom.Destroy;
begin
  ConnectionClosing;
  inherited;
end;

procedure TBoldSQLHandleCom.ClearAllValues;
begin
  // from TBoldElementHandleCom
  FDynamicBoldType := nil;
  FStaticBoldType := nil;
  FStaticSystemTypeInfo := nil;
  FValue := nil;
  FHandleId := 0;
  // from TBoldNonSystemHandleCom
  // from TBoldSQLHandleCom
end;

procedure TBoldSQLHandleCom.ClearList;
begin
  ServerElementHandle.SetData(-1, nil, BoldCreateNamedValues([nv_Action], [nv_ClearList]));
end;

procedure TBoldSQLHandleCom.ExecuteSQL;
begin
  ServerElementHandle.SetData(-1, nil, BoldCreateNamedValues([nv_Action], [nv_ExecuteSQL]));
end;

function TBoldSQLHandleCom.GetClassExpressionName: string;
begin
  if not OwnsHandleOnServer then
    EnsureCurrent;
  Result := FClassExpressionName;
end;

function TBoldSQLHandleCom.GetClearBeforeExecute: Boolean;
begin
  if not OwnsHandleOnServer then
    EnsureCurrent;
  Result := FClearBeforeExecute;
end;

function TBoldSQLHandleCom.GetSQLOrderByClause: string;
begin
  if not OwnsHandleOnServer then
    EnsureCurrent;
  Result := FSQLOrderByClause;
end;

function TBoldSQLHandleCom.GetSQLWhereClause: string;
begin
  if not OwnsHandleOnServer then
    EnsureCurrent;
  Result := FSQLWhereClause;
end;

function TBoldSQLHandleCom.ServerHandleClassName: string;
begin
  Result := ServerHandleClassName_SQLHandle;
end;

procedure TBoldSQLHandleCom.SetClassExpressionName(const Value: string);
begin
  if Value <> FClassExpressionName then
  begin
    if not OwnsHandleOnServer then
      raise EBold.CreateFmt(sPropertyIsReadOnly, ['ClassExpressionName']); // do not localize
    FClassExpressionName := Value;
    LocalValueChanged;
  end;
end;

procedure TBoldSQLHandleCom.SetClearBeforeExecute(Value: Boolean);
begin
  if Value <> FClearBeforeExecute then
  begin
    if not OwnsHandleOnServer then
      raise EBold.CreateFmt(sPropertyIsReadOnly, ['ClearBeforeExecute']); // do not localize
    FClearBeforeExecute := Value;
    LocalValueChanged;
  end;
end;

procedure TBoldSQLHandleCom.SetSQLOrderByClause(const Value: string);
begin
  if Value <> FSQLOrderByClause then
  begin
    if not OwnsHandleOnServer then
      raise EBold.CreateFmt(sPropertyIsReadOnly, ['SQLOrderByClause']); // do not localize
    FSQLOrderByClause := Value;
    LocalValueChanged;
  end;
end;

procedure TBoldSQLHandleCom.SetSQLWhereClause(const Value: string);
begin
  if Value <> FSQLWhereClause then
  begin
    if not OwnsHandleOnServer then
      raise EBold.CreateFmt(sPropertyIsReadOnly, ['SQLWhereClause']); // do not localize
    FSQLWhereClause := Value;
    LocalValueChanged;
  end;
end;

procedure TBoldSQLHandleCom.ValuesFromServer;
var
  NamedValues: OleVariant;
  DummySystem: IBoldSystem;
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
    DummySystem,
    DummyType,
    DummyObject,
    DummyList,
    DummyListType,
    NamedValues);
  FHandleId := BoldGetNamedValue(NamedValues, nv_HandleId);
  if not OwnsHandleOnServer then
  begin
    FClassExpressionName := BoldGetNamedValue(NamedValues, nv_ClassExpressionName);
    FClearBeforeExecute := BoldGetNamedValue(NamedValues, nv_ClearBeforeExecute);
    FSQLOrderByClause := BoldGetNamedValue(NamedValues, nv_SQLOrderByClause);
    FSQLWhereClause := BoldGetNamedValue(NamedValues, nv_SQLWhereClause);
  end;
end;

procedure TBoldSQLHandleCom.ValuesToServer;
var
  DataFlags: Integer;
  NamedValues: OleVariant;
  StaticSystemHandleId: Integer;
begin
  DataFlags := DF_STATICSYSTEMHANDLE or DF_CLASSEXPRESSIONNAME or
    DF_CLEARBEFOREEXECUTE or DF_SQLORDERBYCLAUSE or DF_SQLWHERECLAUSE;
  if Assigned(StaticSystemHandle) then
    StaticSystemHandleId := StaticSystemHandle.HandleId
  else
    StaticSystemHandleId := 0;
  NamedValues := BoldCreateNamedValues(
    [nv_StaticSystemHandle,
     nv_ClassExpressionName,
     nv_ClearBeforeExecute,
     nv_SQLOrderByClause,
     nv_SQLWhereClause],
    [StaticSystemHandleId,
     FClassExpressionName,
     FClearBeforeExecute,
     FSQLOrderByClause,
     FSQLWhereClause]);
  ServerElementHandle.SetData(DataFlags,nil,NamedValues);
end;

end.
