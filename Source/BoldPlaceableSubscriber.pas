{ Global compiler directives }
{$include bold.inc}
unit BoldPlaceableSubscriber;

interface

uses
  Classes,
  BoldSubscription,
  BoldElements,
  BoldHandles;

type
  {---forward declarations---}
  TBoldPlaceableSubscriber = class;

  {---method types---}
  TBoldSubscribeToElementEvent = procedure (element: TBoldElement; Subscriber: TBoldSubscriber) of object;
  TBoldPlaceableSubcriberReceive = procedure(sender: TBoldPlaceableSubscriber; Originator: TObject; OriginalEvent: TBoldEvent;
      RequestedEvent: TBoldRequestedEvent) of object;

  {---TBoldPlaceableSubscriber---}
  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldPlaceableSubscriber = class(TBoldSubscribableComponentViaBoldElem)
  private
    FBoldHandle: TBoldElementHandle;
    FOnReceive: TBoldPlaceableSubcriberReceive;
    FOnSubscribeToElement: TBoldSubscribeToElementEvent;
    FHandleSubscriber: TBoldPassthroughSubscriber;
    FValueSubscriber: TBoldExtendedPassthroughSubscriber;
    FDelayEventsUntilPostNotify: Boolean;
    procedure SetBoldHandle(Value: TBoldElementHandle);
    procedure HandleSubscriberReceive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    procedure HandleValueChanged;
    procedure ActOnHandleValueChanged(Sender: TObject);
    procedure ValueSubscriberReceive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const);
    procedure SetDelayEventsUntilPostNotify(const Value: Boolean);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SubscribeToElement(Element: TBoldElement; Subscriber: TBoldSubscriber); virtual;
    procedure Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const); virtual;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
  published
    property BoldHandle: TBoldElementHandle read FBoldHandle write SetBoldHandle;
    property OnReceive: TBoldPlaceableSubcriberReceive read FOnReceive write fOnReceive;
    property OnSubscribeToElement: TBoldSubscribeToElementEvent read FOnSubscribeToElement write FOnSubscribeToElement;
    property DelayEventsUntilPostNotify: Boolean read FDelayEventsUntilPostNotify write SetDelayEventsUntilPostNotify default false;
  end;

implementation

{---TBoldPlaceableSubscriber---}

constructor TBoldPlaceableSubscriber.Create(Owner: TComponent);
begin
  inherited;
  FHandleSubscriber := TBoldPassthroughSubscriber.Create(HandleSubscriberReceive);
  FValueSubscriber := TBoldExtendedPassthroughSubscriber.CreateWithExtendedReceive(ValueSubscriberReceive);
end;

destructor TBoldPlaceableSubscriber.Destroy;
begin
  FHandleSubscriber.Free;
  FValueSubscriber.Free;
  inherited;
end;

procedure TBoldPlaceableSubscriber.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = BoldHandle) then
    BoldHandle := nil;
end;

procedure TBoldPlaceableSubscriber.SetBoldHandle(Value: TBoldElementHandle);
begin
  if FBoldHandle <> Value then
  begin
    FHandleSubscriber.CancelAllSubscriptions;
    FBoldHandle := Value;
    if Assigned(Value) then
    begin
      Value.FreeNotification(Self);
      BoldHandle.AddSmallSubscription(fHandleSubscriber, [beDestroying, beValueIdentityChanged], breReEvaluate);
    end;
    HandleValueChanged;
  end;
end;

procedure TBoldPlaceableSubscriber.HandleSubscriberReceive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  if (csDestroying in ComponentState) or (Assigned(BoldHandle) and (csDestroying in BoldHandle.ComponentState)) then
    exit;
  Receive(Originator, OriginalEvent, RequestedEvent, []);
  HandleValueChanged;
end;

procedure TBoldPlaceableSubscriber.ValueSubscriberReceive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const);
begin
  Receive(Originator, OriginalEvent, RequestedEvent, args);
  if RequestedEvent = breReSubscribe then
  begin
    fValueSubscriber.CancelAllSubscriptions;
    if assigned(BoldHandle) and assigned(BoldHandle.Value) then
      SubscribeToElement(BoldHandle.Value, fValueSubscriber);
  end;
end;

procedure TBoldPlaceableSubscriber.HandleValueChanged;
begin
  if DelayEventsUntilPostNotify then
    BoldAddEventToPostNotifyQueue(ActOnHandleValueChanged, self, self)
  else
    ActOnHandleValueChanged(self);
end;

procedure TBoldPlaceableSubscriber.SubscribeToElement(Element: TBoldElement; Subscriber: TBoldSubscriber);
begin
  if Assigned(FOnSubscribeToElement) then
    FOnSubscribeToElement(Element, Subscriber)
  else if Assigned(Element) then
    Element.DefaultSubscribe(Subscriber);
end;

procedure TBoldPlaceableSubscriber.Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const);
begin
  if Assigned(FOnReceive) then
    fOnReceive(Self, Originator, OriginalEvent, RequestedEvent);
end;

procedure TBoldPlaceableSubscriber.ActOnHandleValueChanged(Sender: TObject);
begin
  fValueSubscriber.CancelAllSubscriptions;
  if Assigned(BoldHandle) and not (csDesigning in BoldHandle.ComponentState) then
    if not (csDestroying in ComponentState) and not (csDestroying in BoldHandle.ComponentState) and assigned(BoldHandle.Value) then
    SubscribeToElement(BoldHandle.Value, fValueSubscriber);
end;

procedure TBoldPlaceableSubscriber.SetDelayEventsUntilPostNotify(const Value: Boolean);
begin
  FDelayEventsUntilPostNotify := Value;
end;

end.
