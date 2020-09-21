unit BoldElementHandleFollower;

{$UNDEF BOLDCOMCLIENT}

interface

uses
  {$IFDEF BOLDCOMCLIENT}
  BoldComClient,
  {$ENDIF}
  BoldQueue,
  BoldSubscription,
  BoldControlPack,
  BoldHandles;

type
  { forward declarations }
  TBoldElementHandleFollower = class;

  { TBoldElementHandleFollower }
  TBoldElementHandleFollower = class(TBoldQueueable)
  private
    fBoldHandle: TBoldElementHandle;
    fFollower: TBoldFollower;
    fFollowerValueCurrent: Boolean;
    fSubscriber: TBoldSubscriber;
    procedure SetFollowerValueCurrent(value: Boolean);
    procedure SetBoldHandle(value: TBoldElementHandle);
    property FollowerValueCurrent: Boolean read fFollowerValueCurrent write SetFollowerValueCurrent;
  protected
    procedure Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    procedure Display; override;
    property Subscriber: TBoldSubscriber read fSubscriber;
  public
    constructor Create(MatchObject: TObject; Controller: TBoldFollowerController);
    destructor Destroy; override;
    procedure Apply; override;
    procedure DiscardChange; override;
  published
    property BoldHandle: TBoldElementHandle read FBoldHandle write SetBoldHandle;
    property Follower: TBoldFollower read fFollower;
  end;

implementation

uses
  SysUtils,
  BoldUtils,
  BoldControlPackDefs,
  BoldDefs;

{ TBoldElementHandleFollower }

procedure TBoldElementHandleFollower.Receive(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  if RequestedEvent = breHandleNil then
    BoldHandle := nil
  else
    FollowerValueCurrent := false;
end;

constructor TBoldElementHandleFollower.Create(MatchObject: TObject;
  Controller: TBoldFollowerController);
begin
  inherited Create(nil);
  fSubscriber := TBoldPassthroughSubscriber.Create(Receive);
  fFollower := TBoldFollower.Create(MatchObject, Controller);
  fFollower.PrioritizedQueuable := Self;
  fFollowerValueCurrent := true;
end;

destructor TBoldElementHandleFollower.Destroy;
begin
  FreeAndNil(fFollower);
  FreeAndNil(fSubscriber);
  inherited;
end;

procedure TBoldElementHandleFollower.SetBoldHandle(value: TBoldElementHandle);
begin
  if (value <> BoldHandle) then
  begin
    fBoldHandle := Value;
    // will force subscription on Handle
    FollowerValueCurrent := false;
  end;
end;

procedure TBoldElementHandleFollower.Apply;
begin
  raise EBoldInternal.CreateFmt('%s.Apply called (abstract at this  level)', [ClassName]);
end;

procedure TBoldElementHandleFollower.DiscardChange;
begin
  raise EBoldInternal.CreateFmt('%s.DiscardChange called (abstract at this  level)', [ClassName]);
end;

procedure TBoldElementHandleFollower.Display;
begin
  FollowerValueCurrent := true;
end;

procedure TBoldElementHandleFollower.SetFollowerValueCurrent(value: Boolean);
  procedure PropagateValue;
  begin
    Assert(Assigned(Follower));
    if Assigned(BoldHandle) then
      Follower.Element := BoldHandle.Value
    else
      Follower.Element := nil;
  end;

  procedure Subscribe;
  begin
    if Assigned(BoldHandle) then
      BoldHandle.AddSmallSubscription(Subscriber, [beValueIdentityChanged], breValueIdentityChanged);
  end;

  procedure SubscribeToHandleReference;
  begin
    if assigned(BoldHandle) then
      BoldHandle.AddSmallSubscription(Subscriber, [beDestroying], breHandleNil);
  end;

begin
  if (value <> FollowerValueCurrent) then
  begin
    if Value then
    begin
      RemoveFromDisplayList;
      PropagateValue;
      Subscribe;
    end
    else
    begin
      Follower.element := nil;
      Subscriber.CancelAllSubscriptions;
      AddToDisplayList;
    end;
    fFollowerValueCurrent := value;
  end;
  SubscribeToHandleReference;
end;

end.

