
{ Global compiler directives }
{$include bold.inc}
unit BoldLockManagerAdmin;

interface

uses
  BoldLockManager,
  BoldDefs,
  Classes;

type
  TBoldLockManagerAdmin = class
  private
    fLockManager: TBoldLockManager;
    function GetLockManagerSuspended: Boolean;
    procedure SetLockManagerSuspended(Value: Boolean);
  public
    constructor Create(const LockManager: TBoldLockManager);
    procedure LocksForClients(const ClientIds: TStringList; const Locks: TStringList; const LockDurations: TStringList);
    procedure ListAllClients(const Clients: TStringList);
    procedure ListLockingClients(const Clients: TStringList);
    procedure RemoveLocksForClient(const ClientId: TBoldClientId; const Locks: TStringList);
    function KillClient(const ClientID: TBoldClientId): Boolean;
    property LockManager: TBoldLockManager read fLockManager;
    property LockManagerSuspended: Boolean read GetLockManagerSuspended write SetLockManagerSuspended;
  end;

implementation

uses
  Sysutils,
  Windows,

  BoldCoreConsts,
  BoldUtils,
  BoldLockList;

{ TBoldLockManagerAdmin }

constructor TBoldLockManagerAdmin.Create(
  const LockManager: TBoldLockManager);
begin
  inherited Create;
  fLockManager := LockManager;
end;

function TBoldLockManagerAdmin.GetLockManagerSuspended: Boolean;
begin
  Result := LockManager.Suspended;
end;

function TBoldLockManagerAdmin.KillClient(
  const ClientID: TBoldClientId): Boolean;
var
  RegistrationTime: TTimeStamp;
begin
  if LockManager.Propagator.ClientHandler.GetRegistrationTime(ClientID, RegistrationTime) then
    Result := (LockManager.Propagator.ClientHandler.UnRegisterClient(ClientId, RegistrationTime) = S_OK)
  else
    Result := false;
end;

procedure TBoldLockManagerAdmin.ListAllClients(const Clients: TStringList);
var
  RegisteredClients: TStringList;
begin
  RegisteredClients := TStringList.Create;
  try
    LockManager.Propagator.ClientHandler.GetRegisteredClientIDs(RegisteredClients);
    Clients.Assign(RegisteredClients);
  finally
    FreeAndNil(RegisteredClients);
  end;
end;

procedure TBoldLockManagerAdmin.ListLockingClients(
  const Clients: TStringList);
var
  i: integer;
  RegisteredClients: TStringList;
  ClientId: TBoldClientId;
begin
  RegisteredClients := TStringList.Create;
  try
    LockManager.Propagator.ClientHandler.GetRegisteredClientIDs(RegisteredClients);
    for i:= 0 to RegisteredClients.Count - 1 do
    begin
      ClientId := StrToInt(RegisteredClients.Names[i]);
      if LockManager.HasLocks(ClientId) then
        Clients.Add(RegisteredClients[i]);
    end;
  finally
    FreeAndNil(RegisteredClients);
  end;
end;

procedure TBoldLockManagerAdmin.LocksForClients( const ClientIds: TStringList; const Locks: TStringList; const LockDurations: TStringList);
var
  ClientLocks: TStringList;
  CurrentLockNode: TBoldLockNode;
  i, j: integer;
  CurrentClientId: TBoldClientId;
  CurrentTime: TTimeStamp;
  Hour, Min, Sec, MSec: Word;
  temp: Comp;
  LockDur: String;
  function GetPart(var source: comp; factor: integer): integer;
  var
    temp: comp;
  begin
    temp := trunc(Source / factor);
    result := trunc(source - temp*factor);
    source := temp;
  end;
  
begin
  if not Assigned(ClientIds) then
    raise EBold.CreateFmt(sClientIDsNotAssigned, [ClassName, 'LocksForClients']); // do not localize
  if not Assigned(Locks) then
    raise EBold.CreateFmt('%s.LocksForClients: Locks is not assigned', [ClassName]);
  if not Assigned(LockDurations) then
    raise EBold.CreateFmt('%s.LocksForClients: LockDurations is not assigned', [ClassName]);
  ClientLocks := TStringList.Create;
  try
    CurrentTime := DateTimeToTimeStamp(Now);
    for i:= 0 to ClientIds.Count - 1 do
    begin
      if ClientIds.Names[i] <> '' then
        CurrentClientId := StrToInt(trim(ClientIds.Names[i]))
      else
        CurrentClientId := StrToInt(trim(ClientIds[i]));
      LockManager.HandedLocks.GetLocksByClientId(CurrentClientId, ClientLocks);
      if ClientLocks.Count > 0 then
      begin
        for j:= 0 to ClientLocks.Count - 1 do
        begin
          CurrentLockNode := LockManager.HandedLocks.Items[CurrentClientId, ClientLocks[j]];
          temp := CurrentLockNode.GetLockDuration(CurrentTime);
          MSec := GetPart(temp, 1000);
          sec := Getpart(temp, 60);
          min := Getpart(temp, 60);
          hour := Getpart(temp, 24);
          LockDur := Format('%.2d:%.2d:%.2d:%.3d', [Hour, Min, Sec, MSec]);
          if temp > 0 then
            LockDur := format('%d day(s) ', [temp])+LockDur;
          LockDurations.Add(LockDur);
          Locks.Add(Format('%d=%s', [CurrentClientId, ClientLocks[j]]));
        end;
        ClientLocks.Clear;
      end;
    end;
  finally
    FreeAndNil(ClientLocks);
  end;
end;

procedure TBoldLockManagerAdmin.RemoveLocksForClient(
  const ClientId: TBoldClientId; const Locks: TStringList);
begin
  LockManager.ReleaseLocks(ClientId, Locks);
end;

procedure TBoldLockManagerAdmin.SetLockManagerSuspended(Value: Boolean);
begin
  LockManager.Suspended := Value;
end;

end.
