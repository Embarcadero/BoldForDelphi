
{ Global compiler directives }
{$include bold.inc}
unit BoldLockManagerAdminCOM;

interface

uses
  BoldLockingSupportInterfaces_TLB,
  BoldLockManagerAdmin,
  BoldThreadedComObjectFactory,
  comobj;

const
  CLSID_LOCKMANAGERADMIN : TGuid = '{D06C7BF6-EBC1-4D2E-954F-AEA567C262F7}';
type
  {forward declarations}
  TBoldLockManagerAdminCOM = class;
  TBoldLockManagerAdminComFactory = class;

  TBoldLockManagerAdminCOM = class(TTypedCOMObject, IBoldLockManagerAdmin)
  private
    function GetLockManagerAdmin: TBoldLockManagerAdmin;
  public
    function ListAllClients(out Clients: OleVariant): HResult; safecall;
    function ListLockingClients(out Clients: OleVariant): HResult; safecall;
    function RemoveLocksForClient(ClientId: Integer; Locks: OleVariant): HResult; safecall;
    function KillClient(ClientId: Integer): HResult; safecall;
    function LocksForClients(ClientIds: OleVariant;
      out Locks: OleVariant; out LockDurations: OleVariant): HResult; safecall;
    function  Get_LockManagerSuspended: WordBool; safecall;
    procedure Set_LockManagerSuspended(Value: WordBool); safecall;
    property LockManagerAdmin: TBoldLockManagerAdmin read getLockManagerAdmin;
  end;

  TBoldLockManagerAdminComFactory = class(TBoldThreadedComObjectFactory)
  public
    constructor Create(ComServer: TComServerObject;
      const ClassID: TGUID; const ClassName, Description: string);
  end;

var
  LockManagerAdminCOMFactory: TBoldLockManagerAdminCOMFactory;

implementation

uses
  SysUtils,
  BoldUtils,
  BoldComUtils,
  BoldPropagatorServer,
  BoldApartmentThread,
  Classes,
  windows
  ;

{ TBoldLockManagerAdminCOM }

function TBoldLockManagerAdminCOM.GetLockManagerAdmin: TBoldLockManagerAdmin;
begin
  Result := TBoldPropagatorServer.Instance.LockManagerAdmin;
end;

function TBoldLockManagerAdminCOM.Get_LockManagerSuspended: WordBool;
begin
  Result := LockManagerAdmin.LockManager.Suspended;
end;

function TBoldLockManagerAdminCOM.KillClient(ClientId: Integer): HResult;
begin
  LockManagerAdmin.KillClient(ClientId);
end;

function TBoldLockManagerAdminCOM.ListAllClients(out Clients: OleVariant): HResult;
var
  ClientIds: TStringList;
begin
  ClientIds := TStringList.Create;
  try
    LockManagerAdmin.ListAllClients(ClientIds);
    Clients := BoldStringsToVariant(ClientIds);
  finally
    FreeAndNil(ClientIds);
  end;
end;

function TBoldLockManagerAdminCOM.ListLockingClients(
  out Clients: OleVariant): HResult;
var
  ClientIds: TStringList;
begin
  ClientIds := TStringList.Create;
  try
    LockManagerAdmin.ListLockingClients(ClientIds);
    Clients := BoldStringsToVariant(ClientIds);
  finally
    FreeAndNil(ClientIds);
  end;
end;

function TBoldLockManagerAdminCOM.LocksForClients(ClientIds: OleVariant;
  out Locks: OleVariant; out LockDurations: OleVariant): HResult;
var
  lLocks, lLockDurations, lClientIds: TStringList;
begin
  Result := S_FALSE;
  lClientIds := TStringList.Create;
  lLocks := TStringList.Create;
  lLockDurations := TStringList.Create;
  try
    BoldVariantToStrings(ClientIds, lClientIds);
    LockManagerAdmin.LocksForClients(lClientIds, lLocks, lLockDurations);
    Locks := BoldStringsToVariant(lLocks);
    LockDurations := BoldStringsToVariant(lLockDurations);
    Result := S_OK;
  finally
    FreeAndNil(lClientIds);
    FreeAndNil(lLocks);
    FreeAndNil(lLockDurations);
  end;
end;

function TBoldLockManagerAdminCOM.RemoveLocksForClient(ClientId: Integer;
  Locks: OleVariant): HResult;
var
  LocksList: TStringList;
begin
  Result := S_FALSE;
  LocksList := TStringList.Create;
  try
    BoldVariantToStrings(Locks, LocksList);
    LockManagerAdmin.RemoveLocksForClient(ClientId, LocksList);
    Result := S_OK;
  finally
    FreeAndNil(LocksList);
  end;
end;

procedure TBoldLockManagerAdminCOM.Set_LockManagerSuspended(
  Value: WordBool);
begin
  LockManagerAdmin.LockManager.Suspended := Value;
end;

{ TBoldLockManagerAdminComFactory }

constructor TBoldLockManagerAdminComFactory.Create(
  ComServer: TComServerObject; const ClassID: TGUID;
  const ClassName, Description: string);
begin
  inherited Create(ComServer, TBoldLockManagerAdminCOM, ClassID, ClassName, Description, ciMultiInstance, batSTA);
end;

end.
