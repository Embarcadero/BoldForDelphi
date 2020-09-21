unit BoldListHandleFollower;

{$UNDEF BOLDCOMCLIENT}

interface

uses
  {$IFDEF BOLDCOMCLIENT}
  BoldComClient,
  {$ENDIF}
  BoldQueue,
  BoldSubscription,
  BoldControlPack,
  BoldListListControlPack,
  BoldAbstractListHandle;

// Note, Currently subscibes to value-identity-change via element of handle, until
// subscribability has been added to the handle.

type
  { forward declarations }
  TBoldListHandleFollower = class;

  { TBoldListHandleFollower }
  TBoldListHandleFollower = class(TBoldQueueable)
  private
    fBoldHandle: TBoldAbstractListHandle;
    fFollower: TBoldFollower;
    fFollowerValueCurrent: Boolean;
    fHandleIndexLock: Boolean;
    fSubscriber: TBoldSubscriber;
    procedure SetFollowerValueCurrent(value: Boolean);
    procedure SetBoldHandle(value: TBoldAbstractListHandle);
    property FollowerValueCurrent: Boolean read fFollowerValueCurrent write SetFollowerValueCurrent;
  protected
    procedure Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    procedure Display; override;
    property Subscriber: TBoldSubscriber read fSubscriber;
  public
    procedure Apply; override;
    procedure DiscardChange; override;
    procedure SetFollowerIndex(index: integer);
    property BoldHandle: TBoldAbstractListHandle read FBoldHandle write SetBoldHandle;
    property HandleIndexLock: boolean read fHandleIndexLock write fHandleIndexLock default True;
    property Follower: TBoldFollower read fFollower;
    constructor Create(MatchObject: TObject; Controller: TBoldAbstractListAsFollowerListController);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  BoldControlPackDefs,
  BoldUtils,
  BoldDefs;

{ TBoldElementHandleFollower }

procedure TBoldListHandleFollower.Receive(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  case RequestedEvent of
  breHandleNil:
     BoldHandle := nil;
  breListIdentityChanged,
  breHandleIndexChanged:
     FollowerValueCurrent := False;
  end;
end;

constructor TBoldListHandleFollower.Create(MatchObject: TObject;
  Controller: TBoldAbstractListAsFollowerListController);
begin
  inherited Create(nil);
  fSubscriber := TBoldPassthroughSubscriber.Create(Receive);
  fFollower := TBoldFollower.Create(MatchObject, Controller);
  fFollower.PrioritizedQueuable := Self;
  fFollowerValueCurrent := true;
  fHandleIndexLock := true;
end;

destructor TBoldListHandleFollower.Destroy;
begin
  FreeAndNil(fFollower);
  FreeAndNil(fSubscriber);
  inherited;
end;

procedure TBoldListHandleFollower.SetBoldHandle(
  value: TBoldAbstractListHandle);
begin
  if (value <> BoldHandle) then
  begin
    fBoldHandle := Value;
    // will force subscription on Handle
    FollowerValueCurrent := false;
  end;
end;

procedure TBoldListHandleFollower.Apply;
begin
  raise EBoldInternal.CreateFmt('%s.Apply called', [ClassName]);
end;

procedure TBoldListHandleFollower.DiscardChange;
begin
  raise EBoldInternal.CreateFmt('%s.DiscardChange called', [ClassName]);
end;

procedure TBoldListHandleFollower.Display;
begin
  FollowerValueCurrent := true;
end;

procedure TBoldListHandleFollower.SetFollowerValueCurrent(value: Boolean);
  procedure PropagateValue;
  begin
    Assert(Assigned(Follower));
    if Assigned(BoldHandle) then
    begin
      fFollower.Element := BoldHandle.List;
      if (HandleIndexLock) then
        SetfollowerIndex((Follower.Controller as TBoldAbstractListAsFollowerListController).ListIndex(BoldHandle.CurrentIndex));
    end
    else
    begin
      fFollower.Element := nil;
    end;
  end;

  procedure Subscribe;
  begin
    if Assigned(BoldHandle) then
    begin
      BoldHandle.AddSmallSubscription(Subscriber, [beValueIdentityChanged], breListIdentityChanged); // FIXME
      BoldHandle.AddSmallSubscription(Subscriber, [beValueIdentityChanged], breHandleIndexChanged); // FIXME
    end;
  end;

  procedure SubscribeToHandleReference;
  begin
    if assigned(fBoldHandle) then
      BoldHandle.AddSmallSubscription(Subscriber, [beDestroying], breHandleNil);
  end;

begin
  if (value <> FollowerValueCurrent) then
  begin
    if Value then
    begin
      PropagateValue;
      RemoveFromDisplayList;
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

procedure TBoldListHandleFollower.SetFollowerIndex(index: integer);
var
  NewHandleIndex: integer;
begin
  Follower.CurrentIndex := Index;
  NewHandleIndex := TBoldAbstractListAsFollowerListController(Follower.Controller).GetListIndex(follower);
  if HandleIndexLock and
     assigned(BoldHandle) and
     assigned(BoldHandle.List) and
     (BoldHandle.List.Count > NewHandleIndex) then
    BoldHandle.CurrentIndex := NewHandleIndex;
end;

end.

