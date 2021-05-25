
{ Global compiler directives }
{$include bold.inc}
unit BoldListHandleFollowerCom;

{$DEFINE BOLDCOMCLIENT} {Clientified 2002-08-05 13:13:02}

interface

uses
  {$IFDEF BOLDCOMCLIENT}
  BoldComClient,
  {$ENDIF}
  BoldQueue,
  BoldSubscription,
  BoldControlPackCom,
  BoldListListControlPackCom,
  BoldAbstractListHandleCom;


type
  { forward declarations }
  TBoldListHandleFollowerCom = class;

  { TBoldListHandleFollowerCom }
  TBoldListHandleFollowerCom = class(TBoldQueueable)
  private
    fBoldHandle: TBoldAbstractListHandleCom;
    fFollower: TBoldFollowerCom;
    fFollowerValueCurrent: Boolean;
    fHandleIndexLock: Boolean;
    fSubscriber: TBoldComClientSubscriber;
    procedure SetFollowerValueCurrent(value: Boolean);
    procedure SetBoldHandle(value: TBoldAbstractListHandleCom);
    property FollowerValueCurrent: Boolean read fFollowerValueCurrent write SetFollowerValueCurrent;
  protected
    procedure Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    procedure Display; override;
    property Subscriber: TBoldComClientSubscriber read fSubscriber;
  public
    procedure Apply; override;
    procedure DiscardChange; override;
    procedure SetFollowerIndex(index: integer);
    property BoldHandle: TBoldAbstractListHandleCom read FBoldHandle write SetBoldHandle;
    property HandleIndexLock: boolean read fHandleIndexLock write fHandleIndexLock default True;
    property Follower: TBoldFollowerCom read fFollower;
    constructor Create(MatchObject: TObject; Controller: TBoldAbstractListAsFollowerListControllerCom);
    destructor Destroy; override;
  end;                              

implementation

uses
  SysUtils,
  BoldControlPackDefs,
  BoldDefs;

{ TBoldElementHandleFollowerCom }

procedure TBoldListHandleFollowerCom.Receive(Originator: TObject;
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

constructor TBoldListHandleFollowerCom.Create(MatchObject: TObject;
  Controller: TBoldAbstractListAsFollowerListControllerCom);
begin
  inherited Create(nil);
  fSubscriber := TBoldComClientPassthroughSubscriber.Create(Receive);
  fFollower := TBoldFollowerCom.Create(MatchObject, Controller);
  fFollower.PrioritizedQueuable := Self;
  fFollowerValueCurrent := true;
  fHandleIndexLock := true;
end;

destructor TBoldListHandleFollowerCom.Destroy;
begin
  FreeAndNil(fFollower);
  FreeAndNil(fSubscriber);  
  inherited;
end;

procedure TBoldListHandleFollowerCom.SetBoldHandle(
  value: TBoldAbstractListHandleCom);
begin
  if (value <> BoldHandle) then
  begin
    fBoldHandle := Value;
    FollowerValueCurrent := false;
  end;
end;

procedure TBoldListHandleFollowerCom.Apply;
begin
  raise EBoldInternal.CreateFmt('%s.Apply called', [ClassName]);
end;

procedure TBoldListHandleFollowerCom.DiscardChange;
begin
  raise EBoldInternal.CreateFmt('%s.DiscardChange called', [ClassName]);
end;

procedure TBoldListHandleFollowerCom.Display;
begin
  FollowerValueCurrent := true;
end;

procedure TBoldListHandleFollowerCom.SetFollowerValueCurrent(value: Boolean);
  procedure PropagateValue;
  begin
    Assert(Assigned(Follower));
    if Assigned(BoldHandle) then
    begin
      fFollower.Element := BoldHandle.List;
      if (HandleIndexLock) then
        SetfollowerIndex((Follower.Controller as TBoldAbstractListAsFollowerListControllerCom).ListIndex(BoldHandle.CurrentIndex));
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
      BoldHandle.AddSmallSubscription(Subscriber, [beValueIdentityChanged], breListIdentityChanged);
      BoldHandle.AddSmallSubscription(Subscriber, [beValueIdentityChanged], breHandleIndexChanged);
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
      RemoveFromDisplayList(false);
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

procedure TBoldListHandleFollowerCom.SetFollowerIndex(index: integer);
var
  NewHandleIndex: integer;
begin
  Follower.CurrentIndex := Index;
  NewHandleIndex := TBoldAbstractListAsFollowerListControllerCom(Follower.Controller).GetListIndex(follower);
  if HandleIndexLock and
     assigned(BoldHandle) and
     assigned(BoldHandle.List) and
     (BoldHandle.List.Count > NewHandleIndex) then
    BoldHandle.CurrentIndex := NewHandleIndex;
end;


end.
