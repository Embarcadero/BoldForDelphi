
{ Global compiler directives }
{$include bold.inc}
unit BoldLockManagerAdminHandleCom;

interface

uses
  BoldAbstractLockManagerAdminHandle,
  BoldLockingSupportInterfaces_TLB,
  BoldComClientHandles,
  BoldSubscription,
  Classes;

type
  {forward declarations}
  TBoldLockManagerAdminHandleCom = class;

  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldLockManagerAdminHandleCom = class(TBoldAbstractLockManagerAdminHandle)
  private
    FComObject: IUnknown;
    fPassthroughSubscriber: TBoldPassThroughSubscriber;
    FConnectionHandle: TBoldComConnectionHandle;
    FActive : Boolean;
    function GetObjectName: string;
    function GetActive: Boolean;
    procedure SetActive(Value: Boolean);
    function GetComObject: IUnknown;
    function GetConnected: Boolean;
    procedure DoConnect;
    procedure DoDisconnect;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetLockManagerAdmin: IBoldLockManagerAdmin; override;
    procedure SetConnectionHandle(Value: TBoldComConnectionHandle);
    function GetConnectionHandle: TBoldComConnectionHandle;
    procedure _Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    property ComObject: IUnknown read GetComObject;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ObjectName: string read GetObjectName;
    property Connected: Boolean read GetConnected;
  published
    property ConnectionHandle: TBoldComConnectionHandle read GetConnectionHandle write SetConnectionHandle;
    property Active: Boolean read GetActive write SetActive;
  end;


implementation

uses
  Sysutils,
  BoldUtils,
  BoldLockingDefs,
  BoldDefs,
  BoldComClient;

{ TBoldLockManagerAdminHandleCom }

constructor TBoldLockManagerAdminHandleCom.Create(AOwner: TComponent);
begin
  inherited;
  fPassthroughSubscriber := TBoldPassthroughSubscriber.Create(_Receive);
  Active := true;
end;

destructor TBoldLockManagerAdminHandleCom.Destroy;
begin
  Active := false;
  fPassthroughSubscriber.CancelAllSubscriptions;
  FreeAndNil(fPassthroughSubscriber);
  inherited;
end;

procedure TBoldLockManagerAdminHandleCom.DoConnect;
begin
  SendEvent(self, beConnected);
end;

procedure TBoldLockManagerAdminHandleCom.DoDisconnect;
begin
  SendEvent(self, beDisconnected);
  FComObject := nil;
end;

function TBoldLockManagerAdminHandleCom.GetActive: Boolean;
begin
  Result := FActive;
end;

function TBoldLockManagerAdminHandleCom.GetComObject: IUnknown;
begin
  if not Assigned(FComObject) then
  begin
    if Connected then
      FComObject := ConnectionHandle.BoldProvider.GetObject(ObjectName);
  end;
  Result := FComObject;
end;

function TBoldLockManagerAdminHandleCom.GetConnected: Boolean;
begin
  Result := Active and Assigned(ConnectionHandle) and
    ConnectionHandle.Connected;
end;

function TBoldLockManagerAdminHandleCom.GetConnectionHandle: TBoldComConnectionHandle;
begin
  Result := FConnectionHandle;
end;

function TBoldLockManagerAdminHandleCom.GetLockManagerAdmin: IBoldLockManagerAdmin;
begin
  Result := nil;
  if Connected then
    Result := (ComObject as IBoldLockManagerAdmin);
end;

function TBoldLockManagerAdminHandleCom.GetObjectName: string;
begin
  Result := LOCK_MANAGER_ADMIN_OBJECT_NAME;
end;

procedure TBoldLockManagerAdminHandleCom.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent=FConnectionHandle) and (Operation=opRemove) then
    FConnectionHandle := nil;
end;

procedure TBoldLockManagerAdminHandleCom.SetActive(Value: Boolean);
begin
  if Value <> FActive then
  begin
    if Value then
    begin
      FActive := Value;
      if Connected then
        DoConnect;
    end
    else
    begin
      if Connected then
        DoDisconnect;
      FActive := Value;
    end;
  end;
end;

procedure TBoldLockManagerAdminHandleCom.SetConnectionHandle(
  Value: TBoldComConnectionHandle);
begin
  if (Value <> FConnectionHandle) then
  begin
    fPassThroughSubscriber.CancelAllSubscriptions;
    if Connected then
      DoDisconnect;
    FConnectionHandle := Value;
    if Assigned(FConnectionHandle) then
    begin
      FConnectionHandle.AddSubscription(fPassThroughSubscriber, bceDisconnected, bceDisconnected);
      FConnectionHandle.AddSubscription(fPassThroughSubscriber, bceConnected, bceConnected);
      if Connected then
        DoConnect;
    end;
  end;
end;

procedure TBoldLockManagerAdminHandleCom._Receive(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  case OriginalEvent of
    bceConnected:
      if Active then
      begin
        SendEvent(Self,bceConnected);
        DoConnect;
      end;
    bceDisconnected:
      if Active then
      begin
        SendEvent(Self,bceDisconnected);
        DoDisconnect;
      end;
  end;
end;

end.
