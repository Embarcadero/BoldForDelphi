
{ Global compiler directives }
{$include bold.inc}
unit BoldDerivedHandle;

interface

uses
  Classes,
  BoldElements,
  BoldSubscription,
  BoldRootedHandles;

type
   TBoldHandleDeriveAndSubscribe = procedure (Sender: TComponent; RootValue: TBoldElement; ResultElement: TBoldIndirectElement; Subscriber: TBoldSubscriber) of object;
   TBoldGetStaticBoldType = procedure (Sender: TComponent; RootType: TBoldElementTypeInfo; ResultType: TBoldElementTypeInfo) of object;

  { forward declaration of classes }
  TBoldDerivedHandle = class;

  {---TBoldDerviedHandle---}
  TBoldDerivedHandle = class(TBoldRootedHandle)
  private
    fOnDeriveAndSubscribe: TBoldHandleDeriveAndSubscribe;
    fValueTypeName: string;
    procedure SetOnDeriveAndSubscribe(Value: TBoldHandleDeriveAndSubscribe);
    procedure SetValueTypeName(Value: string);
  protected
    procedure DoAssign(Source: TPersistent); override;
    procedure DeriveAndSubscribe(DerivedObject: TObject; Subscriber: TBoldSubscriber); override;
    function GetStaticBoldType: TBoldElementTypeInfo; override;
  published
    property OnDeriveAndSubscribe: TBoldHandleDeriveAndSubscribe read fOnDeriveAndSubscribe write SetOnDeriveAndSubscribe;
    property ValueTypeName: string read fValueTypeName write SetValueTypeName;
  end;

implementation

uses
  SysUtils,
  BoldSystemRT,
  BoldDefs;


{ TBoldDerviedHandle }

procedure TBoldDerivedHandle.DoAssign(Source: TPersistent);
begin
  inherited;
  if Source is TBoldDerivedHandle then with TBoldDerivedHandle(Source) do
  begin
    self.ValueTypeName := ValueTypeName;
    self.OnDeriveAndSubscribe := OnDeriveAndSubscribe;
  end;
end;

procedure TBoldDerivedHandle.DeriveAndSubscribe(DerivedObject: TObject;Subscriber: TBoldSubscriber);
begin
  if csDestroying in ComponentState then
    raise EBold.CreateFmt('%s.DeriveAndSubscribe: %s Handle is in csDestroying state, can not DeriveAndSubscribe.', [classname, name]);
  Assert(DerivedObject is TBoldIndirectElement);
  if Assigned(fOnDeriveAndSubscribe) then
    fOnDeriveAndSubscribe(Self, EffectiveRootValue, TBoldIndirectElement(DerivedObject), Subscriber);
end;

function TBoldDerivedHandle.GetStaticBoldType: TBoldElementTypeInfo;
var
  SystemTypeInfo: TBoldSystemTypeInfo;
begin
  if assigned(StaticSystemHandle) then
    SystemTypeInfo := StaticSystemHandle.StaticSystemTypeInfo
  else if assigned(RootHandle) then
    SystemTypeInfo := RootHandle.StaticSystemTypeInfo
  else
    SystemTypeInfo := nil;

  if assigned(SystemTypeInfo) then
  begin
    result := SystemTypeInfo.ElementTypeInfoByExpressionName[ValueTypeName];
    if assigned(result) and not (result.BoldValueType in [bvtAttr, bvtList]) then
      raise EBold.CreateFmt('%s.GetStaticBoldType: Only lists and attributes are allowed as types (expr: %s)', [ClassName, ValueTypeName]);
  end
  else
    result := nil;
end;

procedure TBoldDerivedHandle.SetOnDeriveAndSubscribe(
  Value: TBoldHandleDeriveAndSubscribe);
begin
  if @fOnDeriveAndSubscribe <> @Value then
  begin
    fOnDeriveAndSubscribe := value;
    MarkSubscriptionOutOfdate;
  end;
end;

procedure TBoldDerivedHandle.SetValueTypeName(Value: string);
begin
  if fValueTypeName <> Value then
  begin
    fValueTypeName := Value;
    StaticBoldTypeChanged;
  end;
end;

end.
