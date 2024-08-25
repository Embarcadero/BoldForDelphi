{ Global compiler directives }
{$include bold.inc}
unit BoldHandleAction;

interface

uses
  Classes,
  Controls,
  Menus,
  ActnList,
  BoldHandles,
  BoldSubscription;

type
  { forward declarations }
  TBoldHandleAction = class;

  { TBoldHandleAction }
  TBoldHandleAction = class(TAction)
  private
    fHandleSubscriber: TBoldPassThroughSubscriber;
    fBoldElementHandle: TBoldElementHandle;
    procedure SetBoldElementHandle(const Value: TBoldElementHandle);
  protected
    procedure _HandleSubscriberReceive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent); virtual;
    property BoldElementHandle: TBoldElementHandle read fBoldElementHandle write SetBoldElementHandle;
    procedure CheckAllowEnable(var EnableAction: boolean); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateTarget(Target: TObject); override;
    function HandlesTarget(Target: TObject): Boolean; override;
  end;

implementation

uses
  SysUtils,
  BoldUtils;

const
  breFreeHandle = 44;
  breValueIdentityChanged = 45;

{ TBoldSystemHandleAction }

procedure TBoldHandleAction._HandleSubscriberReceive(
  Originator: TObject; OriginalEvent: TBoldEvent;
  RequestedEvent: TBoldRequestedEvent);
begin
  Assert(Originator = BoldElementHandle);
  Assert(RequestedEvent in [breFreeHandle]);
  case RequestedEvent of
    breFreeHandle: BoldElementHandle := nil;
  end;
end;

constructor TBoldHandleAction.Create(AOwner: TComponent);
begin
  inherited;
  fHandleSubscriber := TBoldPassthroughSubscriber.Create(_HandleSubscriberReceive);
end;

destructor TBoldHandleAction.Destroy;
begin
  inherited;
  FreeAndNil(fHandleSubscriber);
end;

function TBoldHandleAction.HandlesTarget(Target: TObject): Boolean;
begin
  Result := True;
end;

procedure TBoldHandleAction.SetBoldElementHandle(
  const Value: TBoldElementHandle);
begin
  if (fBoldElementHandle <> Value) then
  begin
    fHandleSubscriber.CancelAllSubscriptions;
    fBoldElementHandle := Value;
    if Assigned(BoldElementHandle) then
      BoldElementHandle.AddSmallSubscription(fHandleSubscriber, [beDestroying], breFreeHandle);
  end;
end;

procedure TBoldHandleAction.UpdateTarget(Target: TObject);
var
  EnableAction: boolean;
begin
  inherited;
  EnableAction := True;
  CheckAllowEnable(EnableAction);
  Enabled := EnableAction;
end;

procedure TBoldHandleAction.CheckAllowEnable(var EnableAction: boolean);
begin
  EnableAction := Assigned(BoldElementHandle);
end;

end.
