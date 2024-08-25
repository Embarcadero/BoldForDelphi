
{ Global compiler directives }
{$include bold.inc}
unit BoldPersistenceHandleSystem;

interface

uses
  BoldSystemhandle,
  BoldPersistenceController,
  BoldPersistenceControllerSystem,
  BoldPersistenceHandle,
  BoldSubscription,
  Classes;

type
  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldPersistenceHandleSystem = class(TBoldPersistenceHandle)
  private
    fSystemHandle: TBoldSystemHandle;
    fSystemHandleSubscriber: TBoldPassthroughSubscriber;
    function GetPersistenceControllerSystem: TBoldPersistenceControllerSystem;
    procedure SetSystemHandle(const Value: TBoldSystemHandle);
    procedure PlaceSubscriptions;
    function GetSubscriber: TBoldSubscriber;
    procedure ReceiveFromSystemHandle(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
  protected
    function CreatePersistenceController: TBoldPersistenceController; override;
    procedure SetActive(Value: Boolean); override;
    property Subscriber: TBoldSubscriber read GetSubscriber;
  public
    destructor Destroy; override;
    property PersistenceControllerSystem: TBoldPersistenceControllerSystem read GetPersistenceControllerSystem;
  published
    property SystemHandle: TBoldSystemHandle read fSystemHandle write SetSystemHandle;
  end;

implementation

uses
  SysUtils,
  BoldHandles;

{ TBoldPersistenceHandleSystem }

function TBoldPersistenceHandleSystem.CreatePersistenceController: TBoldPersistenceController;
begin
  result := TBoldPersistenceControllerSystem.Create;
end;

destructor TBoldPersistenceHandleSystem.Destroy;
begin
  FreeAndNil(fSystemHandleSubscriber);
  inherited;
end;

function TBoldPersistenceHandleSystem.GetPersistenceControllerSystem: TBoldPersistenceControllerSystem;
begin
  result := PersistenceController as TBoldPersistenceControllerSystem;
end;

function TBoldPersistenceHandleSystem.GetSubscriber: TBoldSubscriber;
begin
  if not Assigned(fSystemHandleSubscriber) then
    fSystemHandleSubscriber := TBoldPassthroughSubscriber.Create(ReceiveFromSystemHandle);
  result := fSystemHandleSubscriber;
end;

procedure TBoldPersistenceHandleSystem.PlaceSubscriptions;
begin
  Subscriber.CancelAllSubscriptions;
  if Assigned(SystemHandle) then
  begin
    SystemHandle.AddSmallSubscription(Subscriber, [beDestroying], beDestroying);
    SystemHandle.AddSmallSubscription(Subscriber, [beValueIdentityChanged], beValueIdentityChanged);
  end;
end;

procedure TBoldPersistenceHandleSystem.ReceiveFromSystemHandle(
  Originator: TObject; OriginalEvent: TBoldEvent;
  RequestedEvent: TBoldRequestedEvent);
begin
  case RequestedEvent of
    beDestroying: SystemHandle := nil;
    beValueIdentityChanged: PersistenceControllerSystem.BoldSystem := fSystemHandle.System;
  end;
end;

procedure TBoldPersistenceHandleSystem.SetActive(Value: Boolean);
begin
  if value <> Active then begin
    if value then
      PersistenceControllerSystem.BoldSystem := SystemHandle.System;
  end;
  inherited;
end;

procedure TBoldPersistenceHandleSystem.SetSystemHandle(
  const Value: TBoldSystemHandle);
begin
  if Value = fSystemHandle then
    exit;
  fSystemHandle := Value;
  if Assigned(Value) then
    PersistenceControllerSystem.BoldSystem := Value.System
  else
    PersistenceControllerSystem.BoldSystem := nil;
  PlaceSubscriptions;
end;

end.