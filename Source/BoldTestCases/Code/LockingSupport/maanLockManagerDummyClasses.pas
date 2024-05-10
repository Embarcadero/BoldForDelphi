unit maanLockManagerDummyClasses;

interface

uses
  BoldAbstractLockManagerHandle,
  BoldLockingSupportInterfaces_TLB,
  Classes
  ;

type
  {forward declarations}
  TDummyLockManager = class;
  TDummyLockManagerHandle = class;

  TDummyLockManager = class(TInterfacedObject, IBoldLockManager)
  private
    fFunctionResult: Boolean;
    ffGetLocks: Boolean;
    ffReleaseLocks: Boolean;
    ffEnsureLocks: Boolean;
  public
    function  GetLocks(ClientId: Integer; TimeOut: Integer; RequestedExclusiveLocks: OleVariant;
                       RequestedSharedLocks: OleVariant; out HeldLocks: OleVariant;
                       out ClientsHoldingRequestedLocks: OleVariant): WordBool; safecall;
    function ReleaseLocks(ClientId: Integer; Locks: OleVariant): HResult; safecall;
    function  EnsureLocks(ClientId: Integer; RequestedExclusiveLocks: OleVariant;
                          RequestedSharedLocks: OleVariant): WordBool; safecall;
    procedure InitFlags;
    property FGetLocks: Boolean read ffGetLocks;
    property fReleaseLocks: Boolean read ffReleaseLocks;
    property fEnsureLocks: Boolean read ffEnsureLocks;
  end;

  TDummyLockManagerHandle = class(TBoldAbstractLockManagerHandle)
  private
    fLockManager: TDummyLockManager;
    FFunctionResult: Boolean;
    procedure SetFunctionResult(Value: Boolean);
  protected
    function GetLockManager: IBoldLockManager; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property functionResult: Boolean read fFunctionResult write SetFunctionResult;
    property LockManager:TDummyLockManager read fLockmanager write fLockmanager;
  end;

implementation

uses
  SysUtils,
  Boldutils,
  windows
  ;
  
{ TDummyLockManager }

function TDummyLockManager.EnsureLocks(ClientId: Integer;
  RequestedExclusiveLocks, RequestedSharedLocks: OleVariant): WordBool;
begin
  Result := fFunctionResult;
  ffEnsureLocks := true;
end;

function TDummyLockManager.GetLocks(ClientId, TimeOut: Integer;
  RequestedExclusiveLocks, RequestedSharedLocks: OleVariant; out HeldLocks: OleVariant;
  out ClientsHoldingRequestedLocks: OleVariant): WordBool;
begin
  Result := fFunctionResult;
  ffGetLocks := true;
end;

procedure TDummyLockManager.InitFlags;
begin
  ffGetLocks:= false;
  ffReleaseLocks:=  false;
  ffEnsureLocks:= false;
end;

function TDummyLockManager.ReleaseLocks(ClientId: Integer;
  Locks: OleVariant): HResult;
begin
  Result := S_OK;
  ffReleaseLocks := true;
end;

{ TDummyLockManagerHandle }

constructor TDummyLockManagerHandle.Create;
begin
  inherited Create(nil);
  fLockManager := TDummyLockManager.Create;
  fLockManager._AddRef;
  fFunctionResult := True;
end;

destructor TDummyLockManagerHandle.Destroy;
begin
  fLockManager._Release;
  fLockManager := nil;
  inherited;
end;

function TDummyLockManagerHandle.GetLockManager: IBoldLockManager;
begin
  Result := fLockmanager as IBoldLockManager;
end;

procedure TDummyLockManagerHandle.SetFunctionResult(Value: Boolean);
begin
  fFunctionResult := Value;
  fLockManager.fFunctionResult := Value;
end;

end.
