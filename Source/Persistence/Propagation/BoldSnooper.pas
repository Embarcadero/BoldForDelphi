
{ Global compiler directives }
{$include bold.inc}
unit BoldSnooper;

interface

uses
  BoldAbstractSnooper,
  BoldPropagatorInterfaces_TLB,
  BoldDefs,
  BoldMeta,
  BoldLockingSupportInterfaces_TLB;

type
  {forward declarations}
  TBoldSnooper = class;


  { TBoldSnooper }
  TBoldSnooper = class(TBoldAbstractSnooper)
  private
    fOwner: TObject;
  protected
    procedure EnsureDataBaseLock(const ClientID: TBoldClientID); override;
    procedure ReleaseDataBaseLock(const ClientID: TBoldClientID); override;
    function GetLockManager: IBoldLockManager; virtual;
    function GetPropagator: IBoldEventPropagator; virtual;
    function GetCheckDatabaseLock: Boolean; virtual;
  public
    constructor Create(MoldModel: TMoldModel; aOwner: TObject);
    procedure TransmitEvents(const ClientID: TBoldClientID); override;
    property Propagator: IBoldEventPropagator read GetPropagator;
    property LockManager: IBoldLockManager read GetLockManager;
    property CheckDatabaseLock: Boolean read GetCheckDatabaseLock;
  end;

implementation

uses
  Sysutils,
  Variants,
  Classes,
  BoldPropagatorConstants,
  BoldLockingDefs,
  BoldUtils,
  BoldSnooperHandle,
  ComObj;

function DatabaseLock_AsOLEVariant: OleVariant;
begin
  result := VarArrayCreate([0, 0], varOleStr);
  result[0] := BOLD_DBLOCK_NAME;
end;

procedure TBoldSnooper.TransmitEvents(const ClientID: TBoldClientID);
  procedure CheckError(Res: HResult; Action: String);
  var
    s: string;
  begin
    if res <> S_OK then
    begin
      if Res = E_ENQUEUER_NOT_ENABLED then
        s := 'Call to %s failed. Propagator Enqueuer not enabled'
      else if res = E_CLIENT_NOT_REGISTERED then
        s := 'Call to %s failed. Client not registered with propagator'
      else if res = E_INVALID_PARAMETER then
        s := 'Call to %s failed. Invalid parameter'
      else if res = W_CLIENT_NOT_RECEIVING then
        s := 'Call to %s OK, but client is currently not receiving events'
      else
        s := 'Call to %s failed. Error: ' + SysErrorMessage(Res);
      DoPropagatorFailure(self, format(s, [Action]));
    end;
  end;
begin
  try
    if (ClientID <> NOTVALIDCLIENTID) and Assigned(Propagator) then
    begin
      try
        if (Events.Count <> 0) then
          CheckError(Propagator.SendEvents(ClientID, StringListToVarArray(Events)), 'SendEvents');
        if (Subscriptions.Count <> 0) then
          CheckError(Propagator.AddSubscriptions(ClientID, StringListToVarArray(Subscriptions)), 'AddSubscriptions');
        if (CancelledSubscriptions.Count <> 0) then
          CheckError(Propagator.CancelSubscriptions(ClientID, StringListToVarArray(CancelledSubscriptions)), 'CancelSubscriptions');
      except on E: EOleSysError do
        DoPropagatorFailure(self, E.Message);
      end;
    end;
  finally
    ClearEvents;
  end;
end;

procedure TBoldSnooper.EnsureDataBaseLock(const ClientId: TBoldClientID);
var
  Res: Boolean;
begin
  Res := not CheckDatabaseLock;
  if Res then
    Exit;
  res := LockManager.EnsureLocks(ClientID, DatabaseLock_AsOLEVariant, null);
  if not res then
    raise EBoldEnsureDatabaseLockError.CreateFmt('%s.EnsureDatabaseLock Cannot acquire Database Lock', [ClassName]);
end;

procedure TBoldSnooper.ReleaseDataBaseLock(const ClientID: TBoldClientID);
var
  res: Boolean;
begin
  res := not CheckDatabaseLock;
  if res then
    Exit;
  try
    LockManager.ReleaseLocks(ClientID, DatabaseLock_AsOLEVariant)
  except
    raise EBoldLockManagerError.CreateFmt('%s.ReleaseDatabaseLock Cannot acquire Database Lock', [ClassName]);
  end;  
end;

constructor TBoldSnooper.Create(MoldModel: TMoldModel; aOwner: TObject);
begin
  inherited Create(MoldModel);
  Assert(Assigned(aOwner));
  fOwner := aOwner;
end;

function TBoldSnooper.GetLockManager: IBoldLockManager;
begin
  if Assigned((fOwner as TBoldSnooperHandle).LockManagerHandle) then
    Result := (fOwner as TBoldSnooperHandle).LockManagerHandle.LockManager
  else
    raise EBoldLockManagerError.CreateFmt('%s.GetLockManager: LockManager not assigned', [ClassName]);
end;

function TBoldSnooper.GetPropagator: IBoldEventPropagator;
begin
  if Assigned((fOwner as TBoldSnooperHandle).PropagatorHandle) then
    Result := (fOwner as TBoldSnooperHandle).PropagatorHandle.EventPropagator;
  Assert(Assigned(Result));
end;

function TBoldSnooper.GetCheckDatabaseLock: Boolean;
begin
  Result := (fOwner as TBoldSnooperHandle).CheckDatabaseLock;
end;

initialization

end.
