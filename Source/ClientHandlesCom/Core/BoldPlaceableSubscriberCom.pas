unit BoldPlaceableSubscriberCom;

interface
(*

uses
  Classes,

  BoldSubscription,
  BoldComObjectSpace,
  BoldHandlesCom;
type
{ TODO: Not implemented at all yet }
{---forward declarations---}
  TBoldPlaceableSubscriberCom = class;

  {---method types---}
//  TBoldSubscribeToElementEvent = procedure (element: IBoldElement; Subscriber: TBoldComClientSubscriber) of object;
  TBoldPlaceableSubcriberReceive = procedure(sender: TBoldPlaceableSubscriberCom; Originator: TObject; OriginalEvent: TBoldEvent;
      RequestedEvent: TBoldRequestedEvent) of object;

  {---TBoldPlaceableSubscriberCom---}
  TBoldPlaceableSubscriberCom = class(TBoldSubscribableComponent)
  private
    FBoldHandle: TBoldElementHandleCom;
    FOnReceive: TBoldPlaceableSubcriberReceive;
    FOnSubscribeToElement: TBoldSubscribeToElementEvent;
    FHandleSubscriber: TBoldPassthroughSubscriber;
    FValueSubscriber: TBoldPassthroughSubscriber;
    procedure SetBoldHandle(Value: TBoldElementHandleCom);
    procedure HandleSubscriberReceive(Originator: TObject; OriginalEvent: TBoldEvent;
      RequestedEvent: TBoldRequestedEvent);
    procedure HandleValueChanged;
    procedure ValueSubscriberReceive(Originator: TObject; OriginalEvent: TBoldEvent;
      RequestedEvent: TBoldRequestedEvent);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SubscribeToElement(element: IBoldElement; Subscriber: TBoldComClientSubscriber); virtual;
    procedure Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent); virtual;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
  published
    property BoldHandle: TBoldElementHandleCom read FBoldHandle write SetBoldHandle;
    property OnReceive: TBoldPlaceableSubcriberReceive read FOnReceive write fOnReceive;
    property OnSubscribeToElement: TBoldSubscribeToElementEvent read FOnSubscribeToElement write FOnSubscribeToElement;
  end;
*)
implementation
(*
uses
  SysUtils,
  BoldUtils;

{---TBoldPlaceableSubscriberCom---}

constructor TBoldPlaceableSubscriberCom.Create(Owner: TComponent);
begin
  inherited;
  FHandleSubscriber := TBoldPassthroughSubscriber.Create(HandleSubscriberReceive);
  FValueSubscriber := TBoldPassthroughSubscriber.Create(ValueSubscriberReceive);
end;

destructor TBoldPlaceableSubscriberCom.Destroy;
begin
  FHandleSubscriber.Free;
  FValueSubscriber.Free;
  inherited;
end;

procedure TBoldPlaceableSubscriberCom.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = BoldHandle) then
    BoldHandle := nil;
end;

procedure TBoldPlaceableSubscriberCom.SetBoldHandle(Value: TBoldElementHandleCom);
begin
  if FBoldHandle <> Value then
  begin
    FHandleSubscriber.CancelAllSubscriptions;
    FBoldHandle := Value;
    if Value <> nil then
    begin
      Value.FreeNotification(Self);
      BoldHandle.AddSmallSubscription(fHandleSubscriber,[beDestroying, beValueIdentityChanged], breReEvaluate);  // CHECKME
    end;
    HandleValueChanged;
  end;
end;

procedure TBoldPlaceableSubscriberCom.HandleSubscriberReceive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  HandleValueChanged;
end;

procedure TBoldPlaceableSubscriberCom.ValueSubscriberReceive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  Receive(Originator, OriginalEvent, RequestedEvent);
end;

procedure TBoldPlaceableSubscriberCom.HandleValueChanged;
begin
  FValueSubscriber.CancelAllSubscriptions;
  if Assigned(BoldHandle) and Assigned(BoldHandle.Value) then
    SubscribeToElement(BoldHandle.Value, FValueSubscriber);
end;

procedure TBoldPlaceableSubscriberCom.SubscribeToElement(element: IBoldElement; Subscriber: TBoldComClientSubscriber);
begin
  if Assigned(FOnSubscribeToElement) then
    FOnSubscribeToElement(Element, Subscriber);
end;

procedure TBoldPlaceableSubscriberCom.Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  if Assigned(FOnReceive) then
    fOnReceive(Self, Originator, OriginalEvent, RequestedEvent);
end;
*)
end.
