unit BoldRootedHandles;

interface

uses
  Classes,
  BoldSubscription,
  BoldDeriver,
  BoldElements,
  BoldSystemRt,
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
    fResultElement: TBoldIndirectElement;
    fRootTypeName: String;
    procedure SetRootTypeName(Value: string);
    procedure ReadDesignTimeContext(Reader: TReader); // compatibility
    procedure ReadDesignTimeHandle(Reader: TReader); // compatibility
    procedure _ReceiveFromRoot(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    procedure _ReceiveFromValue(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    procedure SetInternalRootHandle(Value: TBoldElementHandle);
    procedure ReadTrackBold(Reader: TReader);
    procedure _NotifyOutOfDate;
    function GetSubscribe: Boolean;
    function GetIsDeriving: Boolean;
  protected
    procedure SubscribeToValue;
    procedure EffectiveRootValueChanged; virtual;
    function EffectiveRootValue: TBoldElement;
    function GetStaticSystemTypeInfo: TBoldSystemTypeInfo; override;
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
    procedure SetRootHandle(const Value: TBoldElementHandle); virtual;
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
 published
    property RootHandle: TBoldElementHandle read GetRootHandle write SetRootHandle;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property RootTypeName: string read fRootTypeName write SetRootTypeName;
  end;

implementation

uses
  SysUtils,
  BoldDefs,
  HandlesConst,
  BoldEnvironment;

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

function TBoldRootedHandle.GetStaticSystemTypeInfo: TBoldSystemTypeInfo;
begin
  if Assigned(InternalRootHandle) then
    Result := InternalRoothandle.StaticSystemTypeInfo
  else
    Result := inherited GetStaticSystemTypeInfo;
end;

procedure TBoldRootedHandle.EffectiveRootValueChanged;
begin
   Assert (Assigned(fDeriver));
   MarkSubscriptionOutOfdate;
  // ValueIdentityChanged; // fix for unknown bug.. JaNo will look at this.
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
    SendEvent(self, beValueIdentityChanged); // can't call ValueIdentityChanged. We should send even when changing to not enabled
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
    Result:= nil;
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
  if assigned(InternalRootHandle) then
    result := InternalRootHandle.StaticBoldType
  else if Assigned(StaticSystemTypeInfo) then
    Result := StaticSystemTypeInfo.ElementTypeInfoByExpressionName[RootTypeName]
  else
    Result := nil;
end;

procedure TBoldRootedHandle.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('TrackBold', ReadTrackBold, nil, False);                 // do not localize
  Filer.DefineProperty('DesignTimeContext', ReadDesignTimeContext, nil, False); // do not localize
  Filer.DefineProperty('DesignTimeHandle', ReadDesignTimeHandle, nil, False);   // do not localize
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
    SendEvent(self, beValueIdentityChanged);
end;

function TBoldRootedHandle.GetRootHandle: TBoldElementHandle;
begin
  result := InternalRootHandle;
end;

procedure TBoldRootedHandle.SetRootHandle(const Value: TBoldElementHandle);
begin
  InternalRootHandle := value;
end;

function TBoldRootedHandle.GetIsDeriving: Boolean;
begin
  Result := fDeriver.IsDeriving;
end;

function TBoldRootedHandle.RefersToComponent(Component: TBoldSubscribableComponent): Boolean;
begin
  result := inherited RefersToComponent(Component);
  if not result and (Component is TBoldElementHandle) then
    result := IsRootLinkedTo(Component as TBoldElementHandle);
end;

end.
