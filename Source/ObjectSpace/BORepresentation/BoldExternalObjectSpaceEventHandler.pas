unit BoldExternalObjectSpaceEventHandler;

interface

uses
  Classes,
  BoldSystemHandle,
  BoldSystem,
  BoldSubscription,
  BoldAbstractDequeuer,
  BoldDefaultID,
  BoldAbstractPropagatorHandle;

type
  TBoldClassChangedEvent = procedure (TheClass: TBoldObjectList) of object;
  TBoldEmbeddedStateChangedEvent = procedure (BoldObject: TBoldObject) of object;
  TBoldNonEmbeddedStateChangedEvent = procedure (BoldMember: TBoldMember) of object;
  TBoldConflictEvent = procedure (BoldObject: TBoldObject) of object;
  TBoldLockLostEvent = procedure (LockName: String) of object;
  TBoldDoDisconnectEvent = procedure(aMessage: String; RemainDisconnectedMSec: integer) of object;
  TBoldExternalObjectSpaceEventHandler = class;

  TBoldExternalObjectSpaceEventHandler = class(TBoldAbstractDequeuer)
  private
    fBoldSystemHandle: TBoldSystemHandle;
    fPropagatorHandle: TBoldAbstractPropagatorHandle;
    fPTSubscriber: TBoldPassthroughSubscriber;
    {*** User events ***}
    fOnClassChangedEvent: TBoldClassChangedEvent;
    fOnEmbeddedStateChanged: TBoldEmbeddedStateChangedEvent;
    fOnNonEmbeddedStateChanged: TBoldNonEmbeddedStateChangedEvent;
    fOnConflict: TBoldConflictEvent;
    fOnLockLost: TBoldLockLostEvent;
    fOnObjectDeleted: TBoldEmbeddedStateChangedEvent;
    fHandleNilObjects: Boolean;
    fDoDisconnect: TBoldDoDisconnectEvent;
    {*** End user events ***}
    procedure SetPropagatorHandle(Value: TBoldAbstractPropagatorHandle);
    procedure SetBoldSystemHandle(aSystemHandle: TBoldSystemHandle);
    procedure _Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    function GetObjectByID(ObjectID: TBoldDefaultID): TBoldObject;
    procedure Subscribe(const DoSubscribe: Boolean);
  protected
    procedure HandleMessage(aMsg: String); override;
    procedure ClassChanged(ClassName: String); virtual;
    procedure EmbeddedStateOfObjectChanged(ObjectID: TBoldDefaultID); virtual;
    procedure NonEmbeddedStateOfObjectChanged(MemberName: String; ObjectID: TBoldDefaultID); virtual;
    procedure ObjectDeleted(ObjectId: TBoldDefaultID); virtual;
    procedure LockLost(LockName: String); virtual;
    procedure Conflict(BoldObject: TBoldObject);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property HandleNilObjects: Boolean read fHandleNilObjects write fHandleNilObjects;
  published
    property BoldSystemHandle: TBoldSystemHandle read fBoldSystemHandle write SetBoldSystemHandle;
    property OnClassChanged: TBoldClassChangedEvent read fOnClassChangedEvent write fOnClassChangedEvent;
    property OnEmbeddedStateChanged: TBoldEmbeddedStateChangedEvent read fOnEmbeddedStateChanged write fOnEmbeddedStateChanged;
    property OnObjectDeleted: TBoldEmbeddedStateChangedEvent read fOnObjectDeleted write fOnObjectDeleted;
    property OnNonEmbeddedStateChanged: TBoldNonEmbeddedStateChangedEvent read fOnNonEmbeddedStateChanged write fOnNonEmbeddedStateChanged;
    property OnLockLost: TBoldLockLostEvent read fOnLockLost write fOnLockLost;
    property OnConflict: TBoldConflictEvent read fOnConflict write fOnConflict;
    property OnDoDisconnect: TBoldDoDisconnectEvent read fDoDisconnect write fDoDisconnect;
    property PropagatorHandle: TBoldAbstractPropagatorHandle read fPropagatorHandle write SetPropagatorHandle;
  end;

implementation

uses
  SysUtils,
  BoldObjectSpaceExternalEvents,
  BoldDomainElement,
  BoldValueSpaceInterfaces,
  BoldDefs,
  BoldCoreConsts;

{ TBoldDequeuer }

constructor TBoldExternalObjectSpaceEventHandler.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  fPTSubscriber := TBoldPassthroughSubscriber.Create(_Receive);
end;

destructor TBoldExternalObjectSpaceEventHandler.Destroy;
begin
  FreeAndNil(fPTSubscriber);
  inherited;
end;

procedure TBoldExternalObjectSpaceEventHandler.SetBoldSystemHandle(aSystemHandle: TBoldSystemHandle);
begin
  if aSystemHandle <> fBoldSystemHandle then
  begin
    Subscribe(False);
    fBoldSystemHandle := aSystemHandle;
    Subscribe(True);
  end;
end;

procedure TBoldExternalObjectSpaceEventHandler._Receive(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  if (Originator = fBoldSystemHandle) and (RequestedEvent = beDestroying) then
    BoldSystemHandle := nil;
end;

procedure TBoldExternalObjectSpaceEventHandler.HandleMessage(aMsg: String);
var
  ClassName, MemberName, LockName: String;
  SubsType: TBoldObjectSpaceSubscriptionType;
  ObjectID: TBoldDefaultID;
  temp: string;
begin
  if not assigned(fBoldSystemHandle) then
    raise EBold.CreateFmt(sEventHandlerNotConnected, [self.ClassName, name]);
  if not assigned(fBoldSystemHandle.System) then
    raise EBold.CreateFmt(sSystemHandleNotActive, [self.ClassName, fBoldSystemHandle.name]);
  if pos('DISCONNECT:', aMsg) = 1 then // do not localize
  begin
    if Assigned(fPropagatorHandle) then
      fPropagatorHandle.Connected := false;
    if assigned(OnDoDisconnect) then
    begin
      temp := copy(aMsg, pos(':', aMsg)+1, maxint);
      OnDoDisconnect(
        copy(temp, pos(':', temp)+1, maxint),
        StrToIntDef(copy(temp, 1, pos(':', temp)-1), -1)
      );
    end;
  end
  else
  begin
    ObjectID := TBoldDefaultID.Create;
    try
      SubsType := TBoldObjectSpaceExternalEvent.DecodeExternalEvent(aMsg,
                                                                    ClassName,
                                                                    MemberName,
                                                                    LockName,
                                                                    ObjectID);
      case SubsType of
        bsClassChanged: ClassChanged(ClassName);
        bsEmbeddedStateOfObjectChanged: EmbeddedStateOfObjectChanged(ObjectID);
        bsObjectDeleted: ObjectDeleted(ObjectId);
        bsNonEmbeddedStateOfObjectChanged: NonEmbeddedStateOfObjectChanged(MemberName, ObjectID);
        bsLockLost: LockLost(LockName);
      end;
    finally
      FreeAndNil(ObjectID);
    end;
  end;
end;

procedure TBoldExternalObjectSpaceEventHandler.ClassChanged(ClassName: String);
var
  theClass: TBoldObjectList;
begin
  theClass := fBoldSystemHandle.System.ClassByExpressionName[ClassName];
  if Assigned(theClass) then
  begin
    if Assigned(fOnClassChangedEvent) then
      fOnClassChangedEvent(theClass)
    else
      theClass.Invalidate;
  end
  else
    raise EBold.CreateFmt(sClassNotInSystem, [ClassName]);
end;

procedure TBoldExternalObjectSpaceEventHandler.EmbeddedStateOfObjectChanged(
  ObjectID: TBoldDefaultID);
var
  CurrObj: TBoldObject;
begin
  CurrObj := GetObjectByID(ObjectID);
  if Assigned(CurrObj) then
  begin
    if Assigned(fOnEmbeddedStateChanged) then
      fOnEmbeddedStateChanged(CurrObj)
    else
    begin
      if (CurrObj.BoldDirty) then
        Conflict(CurrObj)
      else
        CurrObj.Invalidate;
    end;
  end
  else if HandleNilObjects and Assigned(fOnEmbeddedStateChanged) then
    fOnEmbeddedStateChanged(nil);
end;

procedure TBoldExternalObjectSpaceEventHandler.NonEmbeddedStateOfObjectChanged(
  MemberName: String; ObjectID: TBoldDefaultID);
var
  CurrObj: TBoldObject;
  CurrMember: TBoldMember;
begin
  CurrMember := nil;
  CurrObj := GetObjectByID(ObjectID);
  if Assigned(CurrObj) then
    CurrMember := CurrObj.BoldMemberByExpressionName[MemberName];

  if Assigned(CurrMember) then
  begin
    if Assigned(fOnNonEmbeddedStateChanged) then
      fOnNonEmbeddedStateChanged(CurrMember)
    else
      CurrMember.Invalidate;
  end;
end;

procedure TBoldExternalObjectSpaceEventHandler.Conflict(
  BoldObject: TBoldObject);
begin
  if Assigned(fOnConflict) then
    fOnConflict(BoldObject);
end;

function TBoldExternalObjectSpaceEventHandler.GetObjectByID(ObjectID: TBoldDefaultID): TBoldObject;
var
  CurrLocator: TBoldObjectLocator;
begin
  Result := nil;
  CurrLocator := fBoldSystemHandle.System.Locators.LocatorByID[ObjectID];
  if Assigned(CurrLocator) then
    Result := CurrLocator.BoldObject;
end;

procedure TBoldExternalObjectSpaceEventHandler.LockLost(LockName: String);
begin
  if Assigned(fOnLockLost) then
    fOnLockLost(LockName);
end;

procedure TBoldExternalObjectSpaceEventHandler.ObjectDeleted(ObjectId: TBoldDefaultID);
var
  CurrObj: TBoldObject;
begin
  CurrObj := GetObjectByID(ObjectID);
  if Assigned(CurrObj) then
  begin
    if Assigned(fOnObjectDeleted) then
      fOnObjectDeleted(CurrObj)
    else
    begin
      if (CurrObj.BoldDirty) then
        Conflict(CurrObj)
      else
        CurrObj.AsIBoldObjectContents[bdepPMIn].BoldExistenceState := besDeleted;
    end;
  end;
end;

procedure TBoldExternalObjectSpaceEventHandler.Notification(
  AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) then
  begin
    if (AComponent = fBoldSystemHandle) then
      fBoldSystemHandle := nil
    else if (AComponent = fPropagatorHandle) then
      fPropagatorHandle := nil;
  end;
end;

procedure TBoldExternalObjectSpaceEventHandler.SetPropagatorHandle(
  Value: TBoldAbstractPropagatorHandle);
begin
  if (fPropagatorHandle <> Value) then
  begin
    Subscribe(False);
    fPropagatorHandle := Value;
    Subscribe(True);
  end;
end;

procedure TBoldExternalObjectSpaceEventHandler.Subscribe(
  const DoSubscribe: Boolean);
begin
  if DoSubscribe then
  begin
    if not (csDesigning in ComponentState) then
    begin
      if Assigned(fPropagatorHandle) then
        fPropagatorHandle.AddSmallSubscription(fPTSubscriber, [beDestroying], beDestroying);
      if Assigned(fBoldSystemHandle) then
        fBoldSystemHandle.AddSmallSubscription(fPTSubscriber, [beDestroying], beDestroying);
    end;
  end
  else
    fPTSubscriber.CancelAllSubscriptions;
end;

end.
