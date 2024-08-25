
{ Global compiler directives }
{$include bold.inc}
unit BoldReferenceHandleCom;

interface

uses
  BoldComObjectSpace_TLB,
  BoldHandlesCom;

type
  { forward declarations }
  TBoldReferenceHandleCom = class;

  {-- TBoldReferenceHandleCom --}
  TBoldReferenceHandleCom = class(TBoldNonSystemHandleCom)
  private
    FStaticValueTypeName: string;
    function GetStaticValueTypeName: string;
    function GetValue: IBoldElement;
    procedure SetStaticValueTypeName(const Value: string);
    procedure SetValue(NewValue: IBoldElement);
  protected
    procedure ClearAllValues; override;
    function ServerHandleClassName: string; override;
    procedure ValuesFromServer; override;
    procedure ValuesToServer; override;
  public
    destructor Destroy; override;
    property Value: IBoldElement read GetValue write SetValue;
  published
    property StaticValueTypeName: string read GetStaticValueTypeName write SetStaticValueTypeName;
  end;

implementation

uses
  SysUtils,
  BoldComObjectSpace,
  BoldComUtils,
  BoldDefs;

{-- TBoldReferenceHandleCom ---------------------------------------------------}

destructor TBoldReferenceHandleCom.Destroy;
begin
  ConnectionClosing;
  inherited;
end;

procedure TBoldReferenceHandleCom.ClearAllValues;
begin
  FDynamicBoldType := nil;
  FStaticBoldType := nil;
  FStaticSystemTypeInfo := nil;
  FValue := nil;
  FHandleId := 0;

end;


function TBoldReferenceHandleCom.GetStaticValueTypeName: string;
begin
  if not OwnsHandleOnServer then
    EnsureCurrent;
  Result := FStaticValueTypeName;
end;

function TBoldReferenceHandleCom.GetValue: IBoldElement;
begin
  if not OwnsHandleOnServer then
    EnsureCurrent;
  Result := FValue;
end;

procedure TBoldReferenceHandleCom.SetStaticValueTypeName(const Value: string);
begin
  if Value <> StaticValueTypeName then
  begin
    if not OwnsHandleOnServer then
      raise EBold.Create('StaticValueTypeName is read-only');
    FStaticValueTypeName := Value;
    LocalValueChanged;
  end;
end;

procedure TBoldReferenceHandleCom.SetValue(NewValue: IBoldElement);
begin
  if NewValue <> FValue then
  begin
    if not assigned(EffectiveConnectionHandle) then
      raise EBold.Createfmt('%s.Setvalue: Now allowed without a connectionHandle', [Classname]);
    if not EffectiveConnectionHandle.Connected then
      raise EBold.Createfmt('%s.Setvalue: Now allowed with an inactive ConnectionHandle', [ClassName]);


    if not OwnsHandleOnServer then
      raise EBold.Create('Value is read-only');
    FValue := NewValue;
    LocalValueChanged;
  end;
end;

procedure TBoldReferenceHandleCom.ValuesFromServer;
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
  FHandleId := BoldGetNamedValue(NamedValues,'HandleId');
  if not OwnsHandleOnServer then
    FStaticValueTypeName := BoldGetNamedValue(NamedValues,'StaticValueTypeName');
end;

procedure TBoldReferenceHandleCom.ValuesToServer;
var
  DataFlags: Integer;
  NamedValues: OleVariant;
  StaticSystemHandleId: Integer;
begin
  DataFlags := DF_VALUE or DF_STATICSYSTEMHANDLE or DF_STATICVALUETYPENAME;
  if Assigned(StaticSystemHandle) then
    StaticSystemHandleId := StaticSystemHandle.HandleId
  else
    StaticSystemHandleId := 0;
  NamedValues := BoldCreateNamedValues(
    ['StaticSystemHandle',
    'StaticValueTypeName'],
    [StaticSystemHandleId,
    FStaticValueTypeName]);
  ServerElementHandle.SetData(DataFlags,FValue,NamedValues);
end;

function TBoldReferenceHandleCom.ServerHandleClassName: string;
begin
  result := 'TBoldReferenceHandle';
end;

end.
