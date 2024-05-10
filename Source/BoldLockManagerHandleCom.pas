
{ Global compiler directives }
{$include bold.inc}
unit BoldLockManagerHandleCom;

interface

uses
  BoldAbstractLockManagerHandle,
  BoldLockingSupportInterfaces_TLB,
  BoldComClientHandles,
  BoldSubscription,
  Classes
  ;

type
  {forward declarations}
  TBoldLockManagerHandleCom = class;

  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldLockManagerHandleCom = class(TBoldAbstractLockManagerHandle)
  private
    FComObject: IUnknown;
    fPassthroughSubscriber: TBoldPassThroughSubscriber;
    FConnectionHandle: TBoldComConnectionHandle;
    FActive : Boolean;
    function GetObjectName: string;
    procedure SetActive(Value: Boolean);
    function GetComObject: IUnknown;
    function GetConnected: Boolean;
    procedure DoConnect;
    procedure DoDisconnect;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetLockManager: IBoldLockManager; override;
    procedure SetConnectionHandle(Value: TBoldComConnectionHandle);
    procedure _Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    property ComObject: IUnknown read GetComObject;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ObjectName: string read GetObjectName;
    property Connected: Boolean read GetConnected;
  published
    property ConnectionHandle: TBoldComConnectionHandle read FConnectionHandle write SetConnectionHandle;
    property Active: Boolean read FActive write SetActive;
  end;

implementation

uses
  SysUtils,
  BoldUtils,
  BoldLockingDefs,
  BoldComClient,
  BoldDefs
  ;

{ TBoldLockManagerHandleCom }

constructor TBoldLockManagerHandleCom.Create(AOwner: TComponent);
begin
  inherited;
  fPassthroughSubscriber := TBoldPassthroughSubscriber.Create(_Receive);
  Active := true;
end;

destructor TBoldLockManagerHandleCom.Destroy;
begin
  Active := false;
  fPassthroughSubscriber.CancelAllSubscriptions;
  FreeAndNil(fPassthroughSubscriber);
  inherited;
end;

procedure TBoldLockManagerHandleCom.DoConnect;
begin
  SendEvent(self, beConnected);
end;

procedure TBoldLockManagerHandleCom.DoDisconnect;
begin
  SendEvent(self, beDisconnected);
end;

function TBoldLockManagerHandleCom.GetComObject: IUnknown;
begin
  if not Assigned(FComObject) then
  begin
    if Connected then
      FComObject := ConnectionHandle.BoldProvider.GetObject(ObjectName);
  end;
  Result := FComObject;
end;

function TBoldLockManagerHandleCom.GetConnected: Boolean;
begin
  Result := Active and Assigned(ConnectionHandle) and
    ConnectionHandle.Connected;
end;

function TBoldLockManagerHandleCom.GetLockManager: IBoldLockManager;
begin
  Result := nil;
  if Connected then
    Result := (ComObject as IBoldLockManager);
end;

function TBoldLockManagerHandleCom.GetObjectName: string;
begin
  Result := LOCK_MANAGER_OBJECT_NAME;
end;

procedure TBoldLockManagerHandleCom.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent=FConnectionHandle) and (Operation=opRemove) then
    FConnectionHandle := nil;
end;

procedure TBoldLockManagerHandleCom.SetActive(Value: Boolean);
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

procedure TBoldLockManagerHandleCom.SetConnectionHandle(
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

procedure TBoldLockManagerHandleCom._Receive(Originator: TObject;
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