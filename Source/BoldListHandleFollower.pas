
{ Global compiler directives }
{$include bold.inc}
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
  BoldAbstractListHandle,
  BoldHandles;


type
  { forward declarations }
  TBoldListHandleFollower = class;

  { TBoldListHandleFollower }
  TBoldListHandleFollower = class(TBoldAbstractHandleFollower)
  private
    fBoldHandle: TBoldAbstractListHandle;
    fFollowerValueCurrent: Boolean;
    fHandleIndexLock: Boolean;
    procedure SetFollowerValueCurrent(value: Boolean);
    procedure SetBoldHandle(value: TBoldAbstractListHandle);
    property FollowerValueCurrent: Boolean read fFollowerValueCurrent write SetFollowerValueCurrent;
  protected
    function GetBoldHandle: TBoldElementHandle; override;
    procedure Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent); override;
    procedure Display; override;
  public
    procedure Apply; override;
    procedure DiscardChange; override;
    procedure SetFollowerIndex(index: integer);
    property BoldHandle: TBoldAbstractListHandle read FBoldHandle write SetBoldHandle;
    property HandleIndexLock: boolean read fHandleIndexLock write fHandleIndexLock default True;
    constructor Create(AMatchObject: TObject; Controller: TBoldAbstractListAsFollowerListController);
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
  case OriginalEvent of
    beValueIdentityChanged:
     FollowerValueCurrent := False;
    beDestroying:
    begin
      Assert(Originator = BoldHandle);
      BoldHandle := nil;
    end;
  end;
end;

constructor TBoldListHandleFollower.Create(AMatchObject: TObject;
  Controller: TBoldAbstractListAsFollowerListController);
begin
  inherited Create(AMatchObject, Controller);
  fFollowerValueCurrent := true;
  fHandleIndexLock := true;
end;

procedure TBoldListHandleFollower.SetBoldHandle(
  value: TBoldAbstractListHandle);
begin
  if (value <> BoldHandle) then
  begin
    fBoldHandle := Value;
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
      Follower.Element := BoldHandle.List;
      if (HandleIndexLock) then
        SetFollowerIndex((Follower.Controller as TBoldAbstractListAsFollowerListController).ListIndex(BoldHandle.CurrentIndex));
    end
    else
    begin
      Follower.Element := nil;
    end;
  end;

  procedure SubscribeToHandleReference;
  begin
    if assigned(fBoldHandle) then
      BoldHandle.AddSmallSubscription(Subscriber, [beDestroying]);
  end;

  procedure Subscribe;
  begin
    if assigned(fBoldHandle) then
      BoldHandle.AddSmallSubscription(Subscriber, [beDestroying, beValueIdentityChanged]);
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

function TBoldListHandleFollower.GetBoldHandle: TBoldElementHandle;
begin
  result := fBoldHandle;
end;


end.
