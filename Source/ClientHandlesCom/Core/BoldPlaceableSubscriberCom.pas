
{ Global compiler directives }
{$include bold.inc}
unit BoldPlaceableSubscriberCom;

interface

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


initialization
*)
end.
