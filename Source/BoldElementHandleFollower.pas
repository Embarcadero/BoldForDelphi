
{ Global compiler directives }
{$include bold.inc}
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
  TBoldElementHandleFollower = class(TBoldAbstractHandleFollower)
  private
    fBoldHandle: TBoldElementHandle;
    fFollowerValueCurrent: Boolean;
    procedure SetFollowerValueCurrent(value: Boolean);
    procedure SetBoldHandle(value: TBoldElementHandle);
    property FollowerValueCurrent: Boolean read fFollowerValueCurrent write SetFollowerValueCurrent;
  protected
    function GetBoldHandle: TBoldElementHandle; override;
    procedure Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent); override;
    procedure Display; override;
  public
    constructor Create(AMatchObject: TObject; Controller: TBoldFollowerController);
    procedure Apply; override;
    procedure DiscardChange; override;
//  published
    property BoldHandle: TBoldElementHandle read FBoldHandle write SetBoldHandle;
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

constructor TBoldElementHandleFollower.Create(AMatchObject: TObject;
  Controller: TBoldFollowerController);
begin
  inherited Create(AMatchObject, Controller);
  fFollowerValueCurrent := true;
end;

procedure TBoldElementHandleFollower.SetBoldHandle(value: TBoldElementHandle);
begin
  if (value <> BoldHandle) then
  begin
    fBoldHandle := Value;
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
      if Follower.IsDirty then
        Follower.DiscardChange;
      Follower.element := nil;
      Subscriber.CancelAllSubscriptions;
      AddToDisplayList;
    end;
    fFollowerValueCurrent := value;
  end;
  SubscribeToHandleReference;
end;

function TBoldElementHandleFollower.GetBoldHandle: TBoldElementHandle;
begin
  result := fBoldHandle;
end;

end.
