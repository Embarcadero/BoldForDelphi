
{ Global compiler directives }
{$include bold.inc}
unit BoldElementHandleFollowerCom;

{$DEFINE BOLDCOMCLIENT} {Clientified 2002-08-05 13:13:02}

interface

uses
  {$IFDEF BOLDCOMCLIENT}
  BoldComClient,
  {$ENDIF}
  BoldQueue,
  BoldSubscription,
  BoldControlPackCom,
  BoldHandlesCom;

type
  { forward declarations }
  TBoldElementHandleFollowerCom = class;

  { TBoldElementHandleFollowerCom }
  TBoldElementHandleFollowerCom = class(TBoldQueueable)
  private
    fBoldHandle: TBoldElementHandleCom;
    fFollower: TBoldFollowerCom;
    fFollowerValueCurrent: Boolean;
    fSubscriber: TBoldComClientSubscriber;
    procedure SetFollowerValueCurrent(value: Boolean);
    procedure SetBoldHandle(value: TBoldElementHandleCom);
    property FollowerValueCurrent: Boolean read fFollowerValueCurrent write SetFollowerValueCurrent;
  protected
    procedure Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    procedure Display; override;
    property Subscriber: TBoldComClientSubscriber read fSubscriber;
  public
    constructor Create(MatchObject: TObject; Controller: TBoldFollowerControllerCom);
    destructor Destroy; override;
    procedure Apply; override;
    procedure DiscardChange; override;
  published
    property BoldHandle: TBoldElementHandleCom read FBoldHandle write SetBoldHandle;
    property Follower: TBoldFollowerCom read fFollower;
  end;

implementation

uses
  BoldControlPackDefs,
  BoldDefs,
  SysUtils;

{ TBoldElementHandleFollowerCom }

procedure TBoldElementHandleFollowerCom.Receive(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  if RequestedEvent = breHandleNil then
    BoldHandle := nil
  else
    FollowerValueCurrent := false;
end;

constructor TBoldElementHandleFollowerCom.Create(MatchObject: TObject;
  Controller: TBoldFollowerControllerCom);
begin
  inherited Create(nil);
  fSubscriber := TBoldComClientPassthroughSubscriber.Create(Receive);
  fFollower := TBoldFollowerCom.Create(MatchObject, Controller);
  fFollower.PrioritizedQueuable := Self;
  fFollowerValueCurrent := true;
end;

destructor TBoldElementHandleFollowerCom.Destroy;
begin
  FreeAndNil(fFollower);
  FreeAndNil(fSubscriber);
  inherited;
end;

procedure TBoldElementHandleFollowerCom.SetBoldHandle(value: TBoldElementHandleCom);
begin
  if (value <> BoldHandle) then
  begin
    fBoldHandle := Value;
    FollowerValueCurrent := false;
  end;
end;

procedure TBoldElementHandleFollowerCom.Apply;
begin
  raise EBoldInternal.CreateFmt('%s.Apply called (abstract at this  level)', [ClassName]);
end;

procedure TBoldElementHandleFollowerCom.DiscardChange;
begin
  raise EBoldInternal.CreateFmt('%s.DiscardChange called (abstract at this  level)', [ClassName]);
end;

procedure TBoldElementHandleFollowerCom.Display;
begin
  FollowerValueCurrent := true;
end;

procedure TBoldElementHandleFollowerCom.SetFollowerValueCurrent(value: Boolean);
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
      RemoveFromDisplayList(false);
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
