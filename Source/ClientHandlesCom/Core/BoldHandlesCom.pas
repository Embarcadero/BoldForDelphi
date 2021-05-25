
{ Global compiler directives }
{$include bold.inc}
unit BoldHandlesCom;

interface

uses
  Classes,
  BoldSubscription,
  BoldComObjectSpace,
  BoldComObjectSpace_TLB,
  BoldComClient,
  BoldComClientHandles;

type
  { forward declaration of classes }
  TBoldElementHandleCom = class;
  TBoldAbstractSystemHandleCom = class;
  TBoldNonSystemHandleCom = class;

  {-- TBoldElementHandleCom --}
  TBoldElementHandleCom = class(TBoldComImportHandle)
  private
    FCurrent: Boolean;
    FServerElementHandle: IBoldElementHandle;
    FServerElementHandleSubscriber: TBoldComClientSubscriber;
    function GetBoldType: IBoldElementTypeInfo;
    function GetDynamicBoldType: IBoldElementTypeInfo;
    function GetHasHandleOnServer: Boolean;
    function GetServerElementHandleSubscriber: TBoldComClientSubscriber;
    function GetHandleId: Integer;
    function GetStaticBoldType: IBoldElementTypeInfo;
    function GetStaticSystemTypeInfo: IBoldSystemTypeInfo;
    function GetValue: IBoldElement;
    procedure ServerElementHandleSubscriber_Receive(Originator: TObject;
      OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    procedure SetServerElementHandle(const ElementHandle: IBoldElementHandle);
  protected
    FDynamicBoldType: IBoldElementTypeInfo;
    FHandleId: Integer;
    FStaticBoldType: IBoldElementTypeInfo;
    FStaticSystemTypeInfo: IBoldSystemTypeInfo;
    FValue: IBoldElement;
    procedure ConnectionOpened; override;
    procedure ConnectionClosing; override;
    function ServerHandleClassName: string; virtual;
    procedure ClearAllValues; virtual; abstract;
    procedure EnsureCurrent;
    function GetOwnsHandleOnServer: Boolean; virtual;
    procedure LocalValueChanged;
    procedure ValuesFromServer; virtual; abstract;
    procedure ValuesToServer; virtual; abstract;
    procedure Loaded; override;
    property HasHandleOnServer: Boolean read GetHasHandleOnServer;
    property OwnsHandleOnServer: Boolean read GetOwnsHandleOnServer;
    property ServerElementHandle: IBoldElementHandle read FServerElementHandle write SetServerElementHandle;
    property ServerElementHandleSubscriber: TBoldComClientSubscriber read GetServerElementHandleSubscriber;
  public
    destructor Destroy; override;
    property BoldType: IBoldElementTypeInfo read GetBoldType;
    property DynamicBoldType: IBoldElementTypeInfo read GetDynamicBoldType;
    property HandleId: Integer read GetHandleId;
    property StaticBoldType: IBoldElementTypeInfo read GetStaticBoldType;
    property StaticSystemTypeInfo: IBoldSystemTypeInfo read GetStaticSystemTypeInfo;
    property Value: IBoldElement read GetValue;
  end;

  {-- TBoldAbstractSystemHandleCom --}
  TBoldAbstractSystemHandleCom = class(TBoldElementHandleCom)
  private
    FIsDefault: Boolean;
    function GetIsDefault: Boolean;
    function GetSystem: IBoldSystem;
    function GetSystemActive: Boolean;
    procedure SetIsDefault(Value: Boolean);
  protected
    FBoldSystem: IBoldSystem;
    FSystemActive: Boolean;
    function GetOwnsHandleOnServer: Boolean; override;
  public
    property System: IBoldSystem read GetSystem;
    property SystemActive: Boolean read GetSystemActive;
    class function DefaultBoldSystemHandle: TBoldAbstractSystemHandleCom;
    class function DefaultBoldSystemTypeInfo: IBoldSystemTypeInfo;
  published
    property IsDefault: Boolean read GetIsDefault write SetIsDefault;
  end;

  {-- TBoldNonSystemHandleCom --}
  TBoldNonSystemHandleCom = class(TBoldElementHandleCom)
  private
    FStaticSystemHandle: TBoldAbstractSystemHandleCom;
    FStaticSystemHandleSubscriber: TBoldPassthroughSubscriber;
    function GetStaticSystemHandleSubscriber: TBoldPassthroughSubscriber;
    procedure SetStaticSystemHandle(Value: TBoldAbstractSystemHandleCom);
    procedure StaticSystemHandleSubscriber_Receive(Originator: TObject;
      OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    property StaticSystemHandleSubscriber: TBoldPassthroughSubscriber read GetStaticSystemHandleSubscriber;
  protected
  public
    destructor Destroy; override;
  published
    property StaticSystemHandle: TBoldAbstractSystemHandleCom read FStaticSystemHandle write SetStaticSystemHandle;
  end;


implementation

uses
  SysUtils,
  BoldComUtils;

const
  breFreeHandle = 44;
  breServerValueIdentityChanged = 45;

var
  G_DefaultBoldSystemHandle: TBoldAbstractSystemHandleCom = nil;


{-- TBoldElementHandleCom -----------------------------------------------------}

procedure TBoldElementHandleCom.ConnectionClosing;
begin
  ClearAllValues;
  ServerElementHandle := nil;
  fCurrent := false;
  SendEvent(Self,beValueIdentityChanged);
end;

procedure TBoldElementHandleCom.ConnectionOpened;
begin
  if OwnsHandleOnServer then
  begin
    ServerElementHandle :=
      EffectiveConnectionHandle.BoldProvider.CreateObject(
      ServerHandleClassName) as IBoldElementHandle;
    LocalValueChanged
  end
  else
    ServerElementHandle :=
      EffectiveConnectionHandle.BoldProvider.GetObject(
      ObjectName) as IBoldElementHandle;

  SendEvent(Self,beValueIdentityChanged);
end;

destructor TBoldElementHandleCom.Destroy;
begin
  FreeAndNil(FServerElementHandleSubscriber);
  inherited;
end;

procedure TBoldElementHandleCom.EnsureCurrent;
begin
  if {not FCurrent and} HasHandleOnServer then
  begin
    ValuesFromServer;
    FCurrent := True;
  end;
end;

function TBoldElementHandleCom.GetBoldType: IBoldElementTypeInfo;
begin
  EnsureCurrent;
  if Assigned(FDynamicBoldType) then
    Result := FDynamicBoldType
  else
    Result := FStaticBoldType;
end;

function TBoldElementHandleCom.GetDynamicBoldType: IBoldElementTypeInfo;
begin
  EnsureCurrent;
  Result := FDynamicBoldType;
end;

function TBoldElementHandleCom.GetHandleId: Integer;
var
  NamedValues: OleVariant;
  DummyValue: IBoldElement;
  DummySystem: IBoldSystem;
  DummyType: IBoldElementTypeInfo;
  DummySystemTypeInfo: IBoldSystemTypeInfo;
  DummyObject: IBoldObject;
  DummyList: IBoldList;
begin
  if (fHandleID = 0) and assigned(ServerElementHandle) then
  begin
    ServerElementHandle.GetData(DF_HANDLEID,
      DummyValue,
      DummyType,
      DummyType,
      DummySystemTypeInfo,
      DummySystem,
      DummyType,
      DummyObject,
      DummyList,
      DummyType,
      NamedValues);
    FHandleId := BoldGetNamedValue(NamedValues,'HandleId');
  end;
  Result := FHandleId;
end;

function TBoldElementHandleCom.GetHasHandleOnServer: Boolean;
begin
  Result := Assigned(FServerElementHandle);
end;

function TBoldElementHandleCom.GetOwnsHandleOnServer: Boolean;
begin
  Result := ObjectName = '';
end;

function TBoldElementHandleCom.GetServerElementHandleSubscriber: TBoldComClientSubscriber;
begin
  if not Assigned(FServerElementHandleSubscriber) then
    FServerElementHandleSubscriber := TBoldComClientPassthroughSubscriber.Create(ServerElementHandleSubscriber_Receive);
  Result := FServerElementHandleSubscriber;
end;

function TBoldElementHandleCom.GetStaticBoldType: IBoldElementTypeInfo;
begin
  EnsureCurrent;
  Result := FStaticBoldType;
end;

function TBoldElementHandleCom.GetStaticSystemTypeInfo: IBoldSystemTypeInfo;
begin
  EnsureCurrent;
  Result := FStaticSystemTypeInfo;
end;

function TBoldElementHandleCom.GetValue: IBoldElement;
begin
  EnsureCurrent;
  Result := FValue;
end;

procedure TBoldElementHandleCom.Loaded;
begin
  inherited;
  LocalValueChanged;
end;

procedure TBoldElementHandleCom.LocalValueChanged;
begin
  if not (csLoading in ComponentState) then
  begin
    if HasHandleOnServer and OwnsHandleOnServer then
      ValuesToServer;
    fCurrent := False;
  end;
end;

procedure TBoldElementHandleCom.ServerElementHandleSubscriber_Receive(
  Originator: TObject; OriginalEvent: TBoldEvent;
  RequestedEvent: TBoldRequestedEvent);
begin
  if RequestedEvent = breServerValueIdentityChanged then
  begin
    FCurrent := False;
    SendEvent(Self, beValueIdentityChanged);
  end;
end;

function TBoldElementHandleCom.ServerHandleClassName: string;
begin
  result := '';
end;

procedure TBoldElementHandleCom.SetServerElementHandle(const ElementHandle: IBoldElementHandle);
var
  EventSet: TBoldSmallEventSet;
begin
  FServerElementHandle := ElementHandle;
  if Assigned(FServerElementHandle) then
  begin
    EventSet := [beValueIdentityChanged];
    FServerElementHandle.AddSmallSubscription(ServerElementHandleSubscriber.ClientId,
      ServerElementHandleSubscriber.SubscriberId,Integer(EventSet),
      breServerValueIdentityChanged,True);
  end;
end;

{-- TBoldAbstractSystemHandle -------------------------------------------------}

class function TBoldAbstractSystemHandleCom.DefaultBoldSystemHandle: TBoldAbstractSystemHandleCom;
begin
  Result := G_DefaultBoldSystemHandle;
end;

class function TBoldAbstractSystemHandleCom.DefaultBoldSystemTypeInfo: IBoldSystemTypeInfo;
begin
  if Assigned(G_DefaultBoldSystemHandle) then
    Result := G_DefaultBoldSystemHandle.StaticSystemTypeInfo
  else
    Result := nil;
end;

function TBoldAbstractSystemHandleCom.GetIsDefault: Boolean;
begin
  Result := FIsDefault;
end;

function TBoldAbstractSystemHandleCom.GetOwnsHandleOnServer: Boolean;
begin
  Result := False;
end;

function TBoldAbstractSystemHandleCom.GetSystem: IBoldSystem;
begin
  EnsureCurrent;
  Result := FBoldSystem;
end;

function TBoldAbstractSystemHandleCom.GetSystemActive: Boolean;
begin
  EnsureCurrent;
  Result := FSystemActive;
end;

procedure TBoldAbstractSystemHandleCom.SetIsDefault(Value: Boolean);
begin
  if (Value <> IsDefault) then
  begin
    FIsDefault := Value;
    if IsDefault then
      G_DefaultBoldSystemHandle := Self
    else
      G_DefaultBoldSystemHandle := nil;
  end;
end;

{-- TBoldNonSystemHandle ------------------------------------------------------}

destructor TBoldNonSystemHandleCom.Destroy;
begin
  FreeAndNil(FStaticSystemHandleSubscriber);
  inherited;
end;

function TBoldNonSystemHandleCom.GetStaticSystemHandleSubscriber: TBoldPassthroughSubscriber;
begin
  if not Assigned(FStaticSystemHandleSubscriber) then
    FStaticSystemHandleSubscriber := TBoldPassthroughSubscriber.Create(
      StaticSystemHandleSubscriber_Receive);
  Result := FStaticSystemHandleSubscriber;
end;

procedure TBoldNonSystemHandleCom.SetStaticSystemHandle(Value: TBoldAbstractSystemHandleCom);
begin
  if (FStaticSystemHandle <> Value) then
  begin
    StaticSystemHandleSubscriber.CancelAllSubscriptions;
    FStaticSystemHandle := Value;
    if Assigned(StaticSystemHandle) then
    begin
      StaticSystemHandle.AddSmallSubscription(StaticSystemHandleSubscriber,
        [beDestroying], breFreeHandle);
    end;
    LocalValueChanged;
  end;
end;

procedure TBoldNonSystemHandleCom.StaticSystemHandleSubscriber_Receive(
  Originator: TObject; OriginalEvent: TBoldEvent;
  RequestedEvent: TBoldRequestedEvent);
begin
  Assert(Originator = StaticSystemHandle);
  Assert(RequestedEvent in [breFreeHandle]);
  case RequestedEvent of
    breFreeHandle: StaticSystemHandle := nil;
  end;
end;

end.
