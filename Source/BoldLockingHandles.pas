
{ Global compiler directives }
{$include bold.inc}
unit BoldLockingHandles;

interface

uses
  Classes,
  BoldDefs,
  BoldSubscription,
  BoldLockHandler,
  BoldLockHolder,
  BoldListenerHandle,
  BoldAbstractLockManagerHandle,
  BoldHandles;

type

  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldLockingHandle = class(TBoldSubscribableComponent)
  private
    fActive: Boolean;
    fSubscriber: TBoldPassthroughSubscriber;
    FSystemHandle: TBoldAbstractSystemHandle;
    fLockHandler: TBoldPessimisticLockHandler;
    fLockHolder: TBoldLockHolder;
    FListener: TBoldListenerHandle;
    FLockManager: TBoldAbstractLockManagerHandle;
    fOnProgress: TBoldLockManagerProgressEvent;
    fOnActivityStart: TNotifyEvent;
    fOnActivityEnd: TNotifyEvent;
    procedure SetSystemHandle(const Value: TBoldAbstractSystemHandle);
    procedure _Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    procedure Activate;
    procedure Deactivate;
    procedure SetListener(const Value: TBoldListenerHandle);
    procedure AdjustActive;
    procedure SetLockManager(const Value: TBoldAbstractLockManagerHandle);
    function GetLockHolder: TBoldLockHolder;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property LockHandler: TBoldPessimisticLockHandler read fLockHandler;
    property LockHolder: TBoldLockHolder read GetLockHolder;
    property Active: Boolean read fActive;
  published
    property SystemHandle: TBoldAbstractSystemHandle read FSystemHandle write SetSystemHandle;
    property Listener: TBoldListenerHandle read FListener write SetListener;
    property LockManager: TBoldAbstractLockManagerHandle read FLockManager write SetLockManager;
    property OnActivityStart: TNotifyEvent read fOnActivityStart write fOnActivityStart;
    property OnActivityEnd: TNotifyEvent read fOnActivityEnd write fOnActivityEnd;
    property OnProgress: TBoldLockManagerProgressEvent read fOnProgress write fOnProgress;
  end;

implementation

uses
  SysUtils,

  BoldCoreConsts,
  BoldUtils;

{ TBoldLockingHandle }

procedure TBoldLockingHandle.Activate;
begin
  if not assigned(SystemHandle) then
    raise EBoldInternal.CreateFmt(sCannotActivateWithoutSystemHandle, [classname]);
  if not assigned(SystemHandle.System) then
    raise EBoldInternal.CreateFmt(sCannotActivateWithInActiveSystem, [classname]);
  if not assigned(Listener) then
    raise EBold.CreateFmt(sCannotActivateWithoutListener, [classname]);
  if not assigned(LockManager) then
    raise EBold.CreateFmt(sCannotActivateWithoutLockManager, [classname]);

  if not Active then
  begin
    fLockHandler := TBoldPessimisticLockHandler.CreateWithLockHolder(FSystemHandle.System, LockHolder);
    fLockHandler.OnActivityStart := OnActivityStart;
    fLockHandler.OnActivityEnd := OnActivityEnd;
    fLockHandler.OnProgress := OnProgress;
    fActive := True;
  end;
end;

procedure TBoldLockingHandle.AdjustActive;
begin
  if assigned(SystemHandle) and SystemHandle.Active then
  begin
    Activate;
  end else
    Deactivate;

  if Active and assigned(LockManager) and (LockManager.LockManager <> LockHolder.LockManager) then
    LockHolder.LockManager := LockManager.LockManager;
end;

constructor TBoldLockingHandle.Create(AOwner: TComponent);
begin
  inherited;
  fSubscriber := TBoldPassthroughSubscriber.Create(_Receive);
end;

procedure TBoldLockingHandle.Deactivate;
begin
  if Active then
  begin
    FreeAndNil(fLockHolder);
    FreeAndNil(fLockHandler);
    fActive := false;
  end;
end;

destructor TBoldLockingHandle.Destroy;
begin
  FreeAndNil(fSubscriber);
  Deactivate;
  inherited;
end;

function TBoldLockingHandle.GetLockHolder: TBoldLockHolder;
begin
  if not assigned(fLockHolder) then
    fLockHolder := TBoldLockHolder.Create(FListener.ListenerThread, FListener.Dequeuer, LockManager.LockManager);
  result := fLockHolder;
end;

procedure TBoldLockingHandle.SetListener(const Value: TBoldListenerHandle);
begin
  FListener := Value;
  if assigned(Listener) then
    Listener.AddSubscription(fSubscriber, beDestroying, 0);
end;

procedure TBoldLockingHandle.SetLockManager(
  const Value: TBoldAbstractLockManagerHandle);
begin
  FLockManager := Value;
  if assigned(FLockManager) then
  begin
    LockManager.AddSubscription(fSubscriber, beConnected, beConnected);
    LockManager.AddSubscription(fSubscriber, beDestroying, beDestroying);
  end;
  AdjustActive;
end;

procedure TBoldLockingHandle.SetSystemHandle(
  const Value: TBoldAbstractSystemHandle);
begin
  FSystemHandle := Value;
  if assigned(FSystemHandle) then
  begin
    FSystemHandle.AddSubscription(fSubscriber, beValueIdentityChanged, beValueIdentityChanged);
    FSystemHandle.AddSubscription(fSubscriber, beDestroying, beDestroying);
  end;
  AdjustActive;
end;

procedure TBoldLockingHandle._Receive(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  if Originator = FSystemHandle then
  begin
    case OriginalEvent of
      beValueIdentityChanged:
        AdjustActive;
      beDestroying:
      begin
        Deactivate;
        SystemHandle := nil;
      end;
    end;
  end
  else if Originator = LockManager then
  begin
    case OriginalEvent of
      beConnected:
        if active then
          LockHolder.LockManager := LockManager.LockManager;
      beDisconnected:
        if active then
          LockHolder.LockManager := nil;
      beDestroying:
      begin
        if active then
          LockHolder.LockManager := nil;
        FLockManager := nil;
      end;
    end;
  end
  else if Originator = Listener then
  begin
    if OriginalEvent = beDestroying then
      Listener := nil;
  end;
end;

end.
