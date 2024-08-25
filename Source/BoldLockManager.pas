
{ Global compiler directives }
{$include bold.inc}
unit BoldLockManager;

interface

uses
  Variants,
  BoldLockList,
  BoldAdvancedPropagator,
  BoldDefs,
  SysUtils,
  BoldUtils,
  BoldSubscription,
  BoldLockingDefs,
  BoldPropagatorSubscriptions,
  Classes
  ;

type
  TBoldLockManager = class(TBoldExtendedPassthroughSubscriber)
  private
    fHandedLocks: TBoldLockList;
    fPropagator: TBoldAdvancedPropagator;
    fSuspended: Boolean;
  protected
    function CanAcquireLocks(const ClientId: TBoldClientId; const Locks: TStringList; const LockType: TBoldLockType;
            ClientsHoldingRequestedLocks: TBoldSortedIntegerList; HeldLocks: TStringList): Boolean;
    procedure AcquireLocks(const ClientId: TBoldClientID; const TimeOut: integer;
                           const Locks: TStringList; const LockType: TBoldLockType);
  public
    constructor Create(const Propagator: TBoldAdvancedPropagator);
    destructor Destroy; override;
    function GetLocks( const ClientID: TBoldClientID; const TimeOut: integer;
                       const RequestedExclusiveLocks, RequestedSharedLocks: TStringList;
                       out HeldLocks: OleVariant; out ClientsHoldingRequestedLocks: OleVariant): Boolean;
    procedure ReleaseLocks(const ClientID: TBoldClientID; const RequestedLocks: TStringList);
    function EnsureLocks(const ClientID: TBoldClientID; const RequestedExclusiveLocks, RequestedSharedLocks: TStringList): Boolean;
    procedure ReleaseAllLocksForClient(const ClientID: TBoldClientId);
    procedure _Receive(Originator: TObject; OriginalEvent: TBoldEvent;
            RequestedEvent: TBoldRequestedEvent; const Args: array of const);
    function HasLocks(const ClientId: TBoldClientId): Boolean;
    property HandedLocks: TBoldLockList read fHandedLocks write fHandedLocks;
    property Propagator: TBoldAdvancedPropagator read fPropagator write fPropagator;
    property Suspended: Boolean read fSuspended write fSuspended;
  end;

implementation

uses
  BoldPropagatorConstants,
  BoldObjectSpaceExternalEvents
  ;

{ TBoldLockManager }

procedure TBoldLockManager.AcquireLocks(const ClientID: TBoldClientID; const TimeOut: integer;
            const Locks: TStringList; const LockType: TBoldLockType);
var
  i: integer;
  CurrentTime: TTimeStamp;
begin
  CurrentTime := DateTimeToTimeStamp(Now);
  if Assigned(Locks) then
    for i:= 0 to Locks.Count - 1 do
      HandedLocks.AddLock(ClientId, TimeOut, CurrentTime, Locks[i], LockType);
end;

function TBoldLockManager.CanAcquireLocks( const ClientId: TBoldClientId; const Locks: TStringList;
    const LockType: TBoldLockType; ClientsHoldingRequestedLocks: TBoldSortedIntegerList; HeldLocks: TStringList): Boolean;
var
  i: integer;
  INode: TBoldLockNameIndexNode;
  CurrentNode, Temp: TBoldLockNode;
  LockLostEvent: String;
begin
  Result := True;
  if Assigned (Locks) then
    for i:= 0 to Locks.Count - 1 do
    begin
      INode := HandedLocks.Locks[Locks[i]];
      if Assigned(INode) then
        if (LockType = bltExclusive) or ((LockType = bltShared) and (INode.ExclusiveLock)) then
        begin
          CurrentNode := (INode.Next as TBoldLockNode);
          While Assigned(CurrentNode) do
          begin
            if (CurrentNode.ClientId <> ClientId) then
              if CurrentNode.HasTimedOut  then
              begin
                Temp := CurrentNode;
                CurrentNode := CurrentNode.Next[HandedLocks.LockNameIndexOrder] as TBoldLockNode;
                Temp.Remove;
                LockLostEvent := TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsLockLost, '', '',Locks[i] ,nil);
                Propagator.Enqueuer.SendLockEvent(Temp.ClientId, LockLostEvent, False);
                FreeAndNil(Temp);
              end
              else
              begin
                if Result then Result:= false;
                if (HeldLocks.IndexOf(Locks[i]) = -1) then
                  HeldLocks.Add(Locks[i]);
                ClientsHoldingRequestedLocks.Add(CurrentNode.ClientId);
                CurrentNode := CurrentNode.Next[HandedLocks.LockNameIndexOrder] as TBoldLockNode;
              end
            else
              CurrentNode := CurrentNode.Next[HandedLocks.LockNameIndexOrder] as TBoldLockNode;
          end;
        end
    end;
end;

constructor TBoldLockManager.Create(const Propagator: TBoldAdvancedPropagator);
begin
  inherited CreateWithExtendedReceive(_Receive);
  fHandedLocks := TBoldLockList.Create;
  fPropagator := Propagator;
  fPropagator.ClientHandler.AddSubscription(self, BOLD_PROPAGATOR_CLIENT_REMOVED, breReleaseClientLocks);
end;

destructor TBoldLockManager.Destroy;
begin
  FreeAndNil(fHandedLocks);
  inherited;
end;

function TBoldLockManager.GetLocks(const ClientID: TBoldClientID; const TimeOut: integer;
  const RequestedExclusiveLocks, RequestedSharedLocks: TStringList;
  out HeldLocks: OleVariant; out ClientsHoldingRequestedLocks: OleVariant): Boolean;
var
  ClientIds: TBoldSortedIntegerList;
  aHeldLocks: TStringList;
  i: integer;
  resCanAcquireExclusive, resCanAcquireShared: Boolean;
  ClientIdString: string;
  CurrentClientId: TBoldClientId;
  LeaseDuration, PollingInterval: integer;
  LeaseTimeout: TTimeStamp;
  Initialized: Boolean;
begin
  Result := false;
  ClientIds := TBoldSortedIntegerList.Create;
  aHeldLocks := TStringList.Create;
  resCanAcquireExclusive := CanAcquireLocks(ClientId, RequestedExclusiveLocks, bltExclusive, ClientIds, aHeldLocks);
  resCanAcquireShared := CanAcquireLocks(ClientId, RequestedSharedLocks, bltShared, ClientIds, aHeldLocks);
  try
    if not Suspended and Propagator.ClientHandler.IsRegistered(ClientId) and resCanAcquireExclusive and resCanAcquireShared then
    begin
      AcquireLocks(ClientId, TimeOut, RequestedExclusiveLocks, bltExclusive);
      AcquireLocks(ClientId, TimeOut, RequestedSharedLocks, bltShared);
      Propagator.Enqueuer.SendLockEvent(ClientId, TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsGotLocks, '', '', '', nil), True);
      Result := True;
    end;
  finally
    ClientsHoldingRequestedLocks := NULL;
    HeldLocks := NULL;
    if not Result then
    begin
      if (aHeldLocks.Count > 0) then
      begin
        HeldLocks := VarArrayCreate([0, aHeldLocks.Count - 1], varOleStr);
        for i:= 0 to aHeldLocks.Count - 1 do
          HeldLocks[i] := aHeldLocks[i];
      end;
      if (ClientIds.Count > 0) then
      begin
        ClientsHoldingRequestedLocks := VarArrayCreate([0, ClientIds.Count - 1], varOleStr);
        for i:= 0 to ClientIds.Count - 1 do
        begin
          CurrentClientId := TBoldClientId(ClientIds[i]);
          Propagator.ClientHandler.HasInfoForClient(CurrentClientId, ClientIdString, LeaseDuration, PollingInterval,
              LeaseTimeOut, Initialized);
          ClientsHoldingRequestedLocks[i] := Format('%d=%s', [CurrentClientId, ClientIdString]);
        end;
      end;
    end;
    FreeAndNil(ClientIds);
    FreeAndNil(aHeldLocks);
  end;
end;

procedure TBoldLockManager.ReleaseAllLocksForClient(
  const ClientID: TBoldClientId);
begin
  if not Suspended then
    fHandedLocks.RemoveClient(ClientId);
end;

procedure TBoldLockManager.ReleaseLocks(const ClientID: TBoldClientID; const RequestedLocks: TStringList);
var
  i: integer;
begin
  if not Suspended then
    for i:= 0 to RequestedLocks.Count - 1 do
      fHandedLocks.RemoveLockForClient(ClientID, RequestedLocks[i]);
end;

procedure TBoldLockManager._Receive(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent;
  const Args: array of const);
var
  ClientId: TBoldClientId;
begin
  if (RequestedEvent = breReleaseClientLocks) then
  begin
    ClientID := Args[0].VInteger;
    ReleaseAllLocksForClient(ClientId);
  end;
end;

function TBoldLockManager.HasLocks(const ClientId: TBoldClientId): Boolean;
begin
  Result := Assigned(HandedLocks.Clients[ClientId]) and
            Assigned(HandedLocks.Clients[ClientId].Next);
end;

function TBoldLockManager.EnsureLocks(const ClientID: TBoldClientID; const RequestedExclusiveLocks,
    RequestedSharedLocks: TStringList): Boolean;
var
  i: integer;
  CurrentLock: TBoldLockNode;
begin
  Result := not Suspended;
  i:= 0;
  if Assigned(RequestedExclusiveLocks) then
    while (i < RequestedExclusiveLocks.Count) and Result do
    begin
      CurrentLock := HandedLocks.Items[ClientID, RequestedExclusiveLocks[i]];
      Result := Assigned(CurrentLock) and (CurrentLock.LockType = bltExclusive);
      inc(i);
    end;
  i:= 0;
  if Assigned(RequestedSharedLocks) then
    while (i < RequestedSharedLocks.Count) and Result do
    begin
      CurrentLock := HandedLocks.Items[ClientID, RequestedSharedLocks[i]];
      Result := Assigned(CurrentLock) and (CurrentLock.LockType = bltShared);
      inc(i);
    end;
end;

end.
