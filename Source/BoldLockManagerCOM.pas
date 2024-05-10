
{ Global compiler directives }
{$include bold.inc}
unit BoldLockManagerCOM;

interface

uses
  BoldLockManager,
  BoldLockingSupportInterfaces_TLB,
  BoldThreadedComObjectFactory,
  ComObj
  ;

const
  CLSID_LOCKMANAGER : TGUID = '{4D55535D-A3BC-426A-B58C-940A81E02D86}';

type
  {forward declarations}
  TBoldLockManagerCOM = class;
  TBoldLockManagerCOMFactory = class;

  TBoldLockManagerCOM = class(TTypedComObject, IBoldLockManager)
  private
    fLockManager: TBoldLockManager;
    function getLockManager: TBoldLockManager;
  public
    function  GetLocks(ClientId: Integer; TimeOut: Integer; RequestedExclusiveLocks: OleVariant;
                RequestedSharedLocks: OleVariant; out HeldLocks: OleVariant; out ClientsHoldingRequestedLocks: OleVariant): WordBool; safecall;
    function ReleaseLocks(ClientId: Integer; Locks: OleVariant): HResult; safecall;
    function  EnsureLocks(ClientId: Integer; RequestedExclusiveLocks: OleVariant;
                          RequestedSharedLocks: OleVariant): WordBool; safecall;
    property LockManager: TBoldLockManager read getLockManager;
  end;

  TBoldLockManagerCOMFactory = class(TBoldThreadedComObjectFactory)
  public
    constructor Create(ComServer: TComServerObject; 
      const ClassID: TGUID; const ClassName, Description: string);
  end;

var
  LockManagerCOMFactory: TBoldLockManagerCOMFactory;

implementation

uses
  SysUtils,
  BoldUtils,
  BoldComUtils,
  BoldAdvancedPropagator,
  BoldPropagatorServer,
  BoldApartmentThread,
  Classes,
  windows
  ;

{ TBoldLockManagerCOM }

function TBoldLockManagerCOM.EnsureLocks(ClientId: Integer;
  RequestedExclusiveLocks, RequestedSharedLocks: OleVariant): WordBool;
var
  SharedLocks, ExclusiveLocks: TStringList;
begin
  SharedLocks := TStringList.Create;
  ExclusiveLocks := TStringList.Create;
  try
    BoldVariantToStrings(RequestedExclusiveLocks, ExclusiveLocks);
    BoldVariantToStrings(RequestedSharedLocks, SharedLocks);
    Result := LockManager.EnsureLocks(ClientId, ExclusiveLocks, SharedLocks);
  finally
    FreeAndNil(SharedLocks);
    FreeAndNil(ExclusiveLocks);
  end;
end;

function TBoldLockManagerCOM.getLockManager: TBoldLockManager;
begin
  if not Assigned(fLockManager) then
    fLockManager := TBoldPropagatorServer.Instance.LockManager;
  Result := fLockManager;
end;

function TBoldLockManagerCOM.GetLocks(ClientId, TimeOut: Integer;
  RequestedExclusiveLocks, RequestedSharedLocks: OleVariant;
  out HeldLocks: OleVariant; out ClientsHoldingRequestedLocks: OleVariant): WordBool;
var
  SharedLocks, ExclusiveLocks: TStringList;
begin
  SharedLocks := TStringList.Create;
  ExclusiveLocks := TStringList.Create;
  try
    BoldVariantToStrings(RequestedExclusiveLocks, ExclusiveLocks);
    BoldVariantToStrings(RequestedSharedLocks, SharedLocks);
    Result := LockManager.GetLocks(ClientId, TimeOut, ExclusiveLocks, SharedLocks, HeldLocks, ClientsHoldingRequestedLocks);
  finally
    FreeAndNil(SharedLocks);
    FreeAndNil(ExclusiveLocks);
  end;
end;

function TBoldLockManagerCOM.ReleaseLocks(ClientId: Integer;
  Locks: OleVariant): HResult;
var
  LocksList: TStringList;
begin
  Result := S_FALSE;
  LocksList := TStringList.Create;
  try
    BoldVariantToStrings(Locks, LocksList);
    LockManager.ReleaseLocks(ClientId, LocksList);
  finally
    Result := S_OK;
    FreeAndNil(LocksList);
  end;
end;

{ TBoldLockManagerCOMFactory }

constructor TBoldLockManagerCOMFactory.Create(ComServer: TComServerObject;
  const ClassID: TGUID; const ClassName, Description: string );
begin
  inherited Create(ComServer, TBoldLockManagerCOM, ClassID, ClassName, Description, ciMultiInstance, batSTA);
end;

end.
