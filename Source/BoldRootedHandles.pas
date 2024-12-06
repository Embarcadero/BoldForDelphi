
{ Global compiler directives }
{$include bold.inc}
unit BoldRootedHandles;

interface

uses
  Classes,
  BoldSubscription,
  BoldDeriver,
  BoldElements,
  BoldSystemRt,
  BoldSystem,
  BoldHandles;

type
  { forward declaration of classes }
  TBoldRootedHandle = class;

  {---TBoldRootedHandle---}
  TBoldRootedHandle = class(TBoldNonSystemHandle)
  private
    fInternalRootHandle: TBoldElementHandle;
    fEnabled: Boolean;
    fStreamedEnabled: Boolean;
    fInternalRootHandleSubscriber: TBoldPassthroughSubscriber;
    fValueSubscriber: TBoldPassthroughSubscriber;
    fDeriver: TBoldDeriver;
    FOnBeginValueIdentityChanged: TNotifyEvent;
    FOnEndValueIdentityChanged: TNotifyEvent;
    fResultElement: TBoldIndirectElement;
    fRootTypeName: String;
    procedure ReadDesignTimeContext(Reader: TReader);
    procedure ReadDesignTimeHandle(Reader: TReader);
    procedure _ReceiveFromRoot(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    procedure _ReceiveFromValue(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    procedure SetInternalRootHandle(Value: TBoldElementHandle);
    procedure ReadTrackBold(Reader: TReader);
    procedure _NotifyOutOfDate;
    function GetSubscribe: Boolean;
    function GetIsDeriving: Boolean;
    function IsRootTypeNameStored: Boolean;
    function GetRootTypeName: string;
    function GetIsCurrent: boolean;
  protected
    function GetBoldSystem: TBoldSystem; override;
    procedure SubscribeToValue;
    procedure EffectiveRootValueChanged; virtual;
    function EffectiveRootValue: TBoldElement;
    function GetStaticSystemHandle: TBoldAbstractSystemHandle; override;
    procedure SetStaticSystemHandle(Value: TBoldAbstractSystemHandle); override;
    function GetStaticSystemTypeInfo: TBoldSystemTypeInfo; override;
    procedure SetRootTypeName(Value: string); virtual;    
    procedure Loaded; override;
    procedure SetEnabled(Value: Boolean); virtual;
    procedure SetSubscribe(Value: Boolean); virtual;
    procedure DeriveAndSubscribe(DerivedObject: TObject; Subscriber: TBoldSubscriber); virtual; abstract;
    procedure MarkSubscriptionOutOfDate;
    procedure EnsureCurrent;
    procedure DefineProperties(Filer: TFiler); override;
    property ResultElement: TBoldIndirectElement read fResultElement;
    function GetValue: TBoldElement; override;
    function GetStaticRootType: TBoldElementTypeInfo;
    procedure ValueIdentityChanged;
    function GetRootHandle: TBoldElementHandle; virtual;
    procedure InternalValueIdentityChanged; virtual;
    procedure SetRootHandle(const Value: TBoldElementHandle); virtual;
    procedure DoAssign(Source: TPersistent); override;
    property InternalRootHandle: TBoldElementHandle read fInternalRootHandle write SetInternalRootHandle;
    property IsDeriving: Boolean read GetIsDeriving;
  public
    property Subscribe: Boolean read GetSubscribe write SetSubscribe default True;
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure MarkOutOfDate; virtual;
    function IsRootLinkedTo(Handle: TBoldElementHandle): Boolean;
    function RefersToComponent(Component: TBoldSubscribableComponent): Boolean; override;
    property StaticRootType: TBoldElementTypeInfo read GetStaticRootType;
    function IsStaticSystemHandleStored: boolean; override;
    property IsCurrent: boolean read GetIsCurrent;
 published
    property RootHandle: TBoldElementHandle read GetRootHandle write SetRootHandle;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property RootTypeName: string read GetRootTypeName write SetRootTypeName stored IsRootTypeNameStored;
    property OnBeginValueIdentityChanged: TNotifyEvent read
        FOnBeginValueIdentityChanged write FOnBeginValueIdentityChanged;
    property OnEndValueIdentityChanged: TNotifyEvent read
        FOnEndValueIdentityChanged write FOnEndValueIdentityChanged;
  end;

implementation

uses
  SysUtils,
  BoldDefs,
  BoldEnvironment,
  BoldCoreConsts;

const
  breFreeHandle = 42;
  breValueIdentityChanged = 43;

{---TBoldRootedHandle---}

constructor TBoldRootedHandle.Create(Owner: TComponent);
begin
  inherited;
  fInternalRootHandleSubscriber := TBoldPassthroughSubscriber.Create(_ReceivefromRoot);
  fValueSubscriber := TBoldPassthroughSubscriber.Create(_ReceiveFromValue);
  FStreamedEnabled := True;
  fEnabled := not (Assigned(Owner) and
              (csLoading in Owner.ComponentState));
  fResultElement := TBoldIndirectElement.Create;
  fDeriver := TBoldDeriver.Create(fResultElement);
  fDeriver.OnNotifyOutOfdate := _NotifyOutOfDate;
  fDeriver.OnDeriveAndSubscribe := DeriveAndSubscribe;
end;

procedure TBoldRootedHandle.DoAssign(Source: TPersistent);
begin
  inherited;
  if Source is TBoldRootedHandle then with TBoldRootedHandle(Source) do
  begin
    self.RootHandle := RootHandle;
    self.RootTypeName := RootTypeName;
    self.Enabled := Enabled;
  end;
end;

procedure TBoldRootedHandle.Loaded;
begin
  inherited Loaded;
  try
    if FStreamedEnabled then
      SetEnabled(True);
  except
    if csDesigning in ComponentState then
      BoldEffectiveEnvironment.HandleDesigntimeException(Self)
    else
      raise;
  end;
end;

function TBoldRootedHandle.GetStaticSystemHandle: TBoldAbstractSystemHandle;
begin
  if (RootHandle is TBoldNonSystemHandle) then
    Result := TBoldNonSystemHandle(RootHandle).StaticSystemHandle
  else
  if (RootHandle is TBoldAbstractSystemHandle) then
    Result := RootHandle as TBoldAbstractSystemHandle
  else
    Result := inherited GetStaticSystemHandle;
end;

procedure TBoldRootedHandle.SetStaticSystemHandle(Value: TBoldAbstractSystemHandle);
begin
  if Assigned(RootHandle) then
    Value := GetStaticSystemHandle;
  inherited SetStaticSystemHandle(Value);
end;

function TBoldRootedHandle.GetStaticSystemTypeInfo: TBoldSystemTypeInfo;
begin
  if Assigned(InternalRootHandle) then
    Result := InternalRoothandle.StaticSystemTypeInfo
  else
    Result := inherited GetStaticSystemTypeInfo;
end;

procedure TBoldRootedHandle.EffectiveRootValueChanged;
begin
   Assert(Assigned(fDeriver));
   MarkSubscriptionOutOfdate;
end;

procedure TBoldRootedHandle.SetInternalRootHandle(Value: TBoldElementHandle);
begin
  if (fInternalRootHandle <> Value) then
  begin
    if (Value = self) or
    ((Value is TBoldRootedHandle) and TBoldRootedHandle(Value).IsRootLinkedTo(Self)) then
      raise EBold.CreateFmt(sInternalRootHandle_CircRef, [ClassName]);
    fInternalRootHandleSubscriber.CancelAllSubscriptions;
    fInternalRootHandle := Value;
    if Assigned(InternalRootHandle) then
    begin
      InternalRootHandle.AddSmallSubscription(fInternalRootHandleSubscriber, [beDestroying], breFreeHandle);
      InternalRootHandle.AddSmallSubscription(fInternalRootHandleSubscriber, [beValueIdentityChanged], breValueIdentityChanged);
     end;
    EffectiveRootValueChanged;
  end;
end;

procedure TBoldRootedHandle.SetEnabled(Value: Boolean);
begin
  if csReading in ComponentState then
    FStreamedEnabled := Value
  else if Value <> FEnabled then
  begin
    FEnabled := Value;
    SendEvent(self, beValueIdentityChanged);
    if not Enabled then
      MarkSubscriptionOutOfDate;
  end;
end;

procedure TBoldRootedHandle.SetSubscribe(Value: Boolean);
begin
  fDeriver.Subscribe := Value;
end;

destructor TBoldRootedHandle.Destroy;
begin
  FreePublisher;
  FreeAndNil(fValueSubscriber);
  FreeAndNil(fInternalRootHandleSubscriber);
  FreeAndNil(fDeriver);
  FreeAndNil(FResultElement);
  inherited;
end;

function TBoldRootedHandle.EffectiveRootValue: TBoldElement;
begin
  Assert(Enabled);
  if Assigned(InternalRootHandle) then
    Result := InternalRootHandle.Value
  else
    Result := nil;
  Assert(not Assigned(Result) or (Result is TBoldElement));
end;

procedure TBoldRootedHandle._ReceiveFromRoot(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  if originator = InternalRootHandle then
  begin
    Assert(RequestedEvent in [breFreeHandle, breValueIdentityChanged]);
    case RequestedEvent of
      breFreeHandle:
        InternalRootHandle := nil;
      breValueIdentityChanged:
        EffectiveRootValueChanged;
    end;
  end;
end;

procedure TBoldRootedHandle._ReceiveFromValue(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  if originator = fResultElement.Value then
  begin
    case OriginalEvent of
      beObjectDeleted, beDestroying:
      begin
        fResultElement.SetReferenceValue(nil);
        if not IsDeriving then
        begin
          fValueSubscriber.CancelAllSubscriptions;
          MarkSubscriptionOutOfDate;
          ValueIdentityChanged;
        end;
      end;
    end;
  end;
end;

procedure TBoldRootedHandle.ReadTrackBold(Reader: TReader);
begin
  Subscribe := Reader.ReadBoolean;
end;

procedure TBoldRootedHandle._NotifyOutOfDate;
begin
  ValueIdentityChanged;
end;

function TBoldRootedHandle.GetSubscribe: Boolean;
begin
  Result := fDeriver.Subscribe;
end;

function TBoldRootedHandle.GetStaticRootType: TBoldElementTypeInfo;
begin
  Result := nil;
  if assigned(InternalRootHandle) then
    result := InternalRootHandle.StaticBoldType
  else
  if Assigned(StaticSystemTypeInfo) then
  begin
    if (RootTypeName <> '') then
      Result := StaticSystemTypeInfo.ElementTypeInfoByExpressionName[RootTypeName]
    else
      Result := TBoldAbstractSystemHandle.DefaultBoldSystemTypeInfo;    
  end;
end;

procedure TBoldRootedHandle.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('TrackBold', ReadTrackBold, nil, False);
  Filer.DefineProperty('DesignTimeContext', ReadDesignTimeContext, nil, False);
  Filer.DefineProperty('DesignTimeHandle', ReadDesignTimeHandle, nil, False);
end;

procedure TBoldRootedHandle.ReadDesignTimeContext(Reader: TReader);
begin
  RootTypeName := Reader.ReadString;
end;

procedure TBoldRootedHandle.ReadDesignTimeHandle(Reader: TReader);
begin
  Reader.ReadIdent;
end;

function TBoldRootedHandle.GetValue: TBoldElement;
begin
  if Enabled then
  begin
    fDeriver.EnsureCurrent;
    Result := fResultElement.Value;
  end else
    result := nil;
end;

function TBoldRootedHandle.IsRootLinkedTo(Handle: TBoldElementHandle): Boolean;
begin
  Assert(Assigned(Handle));
  if Handle = InternalRootHandle then
    Result := True
  else if InternalRootHandle is TBoldRootedHandle then
    Result := TBoldRootedHandle(InternalRootHandle).IsRootLinkedTo(Handle)
  else
    Result := false;
end;

function TBoldRootedHandle.IsRootTypeNameStored: Boolean;
begin
  result := (fRootTypeName <> '');
end;

function TBoldRootedHandle.IsStaticSystemHandleStored: boolean;
begin
  result := inherited IsStaticSystemHandleStored and not (RootHandle is TBoldNonSystemHandle);
end;

procedure TBoldRootedHandle.SetRootTypeName(Value: string);
begin
  if Value <> RootTypeName then
  begin
    fRootTypeName := Value;
    StaticBoldTypeChanged;
  end;
end;

procedure TBoldRootedHandle.SubscribeToValue;
begin
  fValueSubscriber.CancelAllSubscriptions;
  if not fResultElement.OwnsValue and assigned(fResultElement.Value) then
    fResultElement.Value.AddSmallSubscription(fValueSubscriber, [beObjectDeleted, beDestroying], beDestroying);
end;

procedure TBoldRootedHandle.MarkSubscriptionOutOfDate;
begin
  fDeriver.MarkSubscriptionOutOfdate;
end;

procedure TBoldRootedHandle.EnsureCurrent;
begin
  fDeriver.EnsureCurrent;
end;

procedure TBoldRootedHandle.MarkOutOfDate;
begin
  MarkSubscriptionOutOfDate;
end;

procedure TBoldRootedHandle.ValueIdentityChanged;
begin
  if Enabled then
    InternalValueIdentityChanged;
end;

function TBoldRootedHandle.GetRootHandle: TBoldElementHandle;
begin
  result := InternalRootHandle;
end;

function TBoldRootedHandle.GetRootTypeName: string;
begin
  result := fRootTypeName;
  if (result = '') then
  begin
    if Assigned(RootHandle) then
    begin
      if Assigned(RootHandle.BoldType) then
        result := RootHandle.BoldType.AsString
    end
    else
    if Assigned(StaticSystemTypeInfo) then
      result := StaticSystemTypeInfo.AsString
  end;
end;

procedure TBoldRootedHandle.SetRootHandle(const Value: TBoldElementHandle);
begin
  if InternalRootHandle <> value then
    InternalRootHandle := value;
end;

function TBoldRootedHandle.GetBoldSystem: TBoldSystem;
begin
  result := nil;
  if Assigned(RootHandle) then
    result := RootHandle.BoldSystem;
  if not Assigned(result) then
    result := inherited;
end;

function TBoldRootedHandle.GetIsCurrent: boolean;
begin
  Result := fDeriver.IsCurrent;
end;

function TBoldRootedHandle.GetIsDeriving: Boolean;
begin
  Result := fDeriver.IsDeriving;
end;

procedure TBoldRootedHandle.InternalValueIdentityChanged;
begin
  if Assigned(FOnBeginValueIdentityChanged) then begin
    FOnBeginValueIdentityChanged(Self);
  end;
  try
    SendEvent(self, beValueIdentityChanged);
  finally
    if Assigned(FOnEndValueIdentityChanged) then begin
      FOnEndValueIdentityChanged(Self);
    end;
  end;
end;

function TBoldRootedHandle.RefersToComponent(Component: TBoldSubscribableComponent): Boolean;
begin
  result := inherited RefersToComponent(Component);
  if not result and (Component is TBoldElementHandle) then
    result := IsRootLinkedTo(Component as TBoldElementHandle);
end;

end.
