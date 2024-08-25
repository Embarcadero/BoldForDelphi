
{ Global compiler directives }
{$include bold.inc}
unit BoldAbstractModificationPropagator;

interface

uses
  { RTL/VCL }
  Classes,

  { Bold }
  BoldMeta,
  BoldSystem,
  BoldSystemHandle,
  BoldAbstractSnooper,
  BoldPersistenceControllerPassthrough,
  BoldPersistenceController,
  BoldPersistenceHandlePassthrough,
  BoldValueSpaceInterfaces,
  BoldUpdatePrecondition,
  BoldExternalObjectSpaceEventHandler,
  BoldId,
  BoldDefs,
  BoldThreadSafeQueue;

type
  TReceivePropagatorEvent = procedure(Sender: TObject; const Event: String) of object;

  { Forward declaration }
  TBoldAbstractNotificationPropagator = class;
  TBoldNotificationPropagatorPersistenceControllerPassthrough = class;

  { TBoldNotificationPropagatorPersistenceControllerPassthrough }
  TBoldNotificationPropagatorPersistenceControllerPassthrough = class(TBoldAbstractSnooper)
  private
    FBoldNotificationPropagator: TBoldAbstractNotificationPropagator;
    fClientId: TBoldClientid;
  public
    constructor Create(MoldModel: TMoldModel; ClientId: TBoldClientId); reintroduce;
    procedure PMUpdate(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; Old_Values: IBoldValueSpace; Precondition: TBoldUpdatePrecondition; TranslationList: TBoldIdTranslationList; var TimeStamp: TBoldTimeStampType; var TimeOfLatestUpdate: TDateTime; BoldClientID: TBoldClientID); override;
    procedure TransmitEvents(const ClientID: TBoldClientID); override;
    property BoldNotificationPropagator: TBoldAbstractNotificationPropagator read FBoldNotificationPropagator write FBoldNotificationPropagator;
    property ClientId: TBoldClientId read fClientId;
  end;

  { TBoldAbstractNotificationPropagator }
  TBoldAbstractNotificationPropagator = class(TBoldPersistenceHandlePassthrough)
  private
    FSystemHandle: TBoldSystemHandle;
    FDequeuer: TBoldExternalObjectSpaceEventHandler;
    FSendQueue: TBoldThreadSafeStringQueue;
    FReceiveQueue: TBoldThreadSafeStringQueue;
    FRefreshQueue: TBoldThreadSafeStringQueue;
    FOnReceiveEvent: TReceivePropagatorEvent;
    fClientId: TBoldClientId;
    function GetController: TBoldNotificationPropagatorPersistenceControllerPassthrough;
  protected
    { Puts one event in the out-queue }
    procedure EnqueEvent(const Event: String);
    procedure EnqueEventList(const aEventList: TStrings);    
    { Decodes and enques a received event from the in-queue }
    procedure ReceiveEvent(const Event: String); virtual;

    procedure OnReceiveQueueNotEmpty(Sender: TBoldThreadSafeQueue); virtual;
    { The following method must be overridden in the inheriting classes }
    procedure OnSendQueueNotEmpty(Sender: TBoldThreadSafeQueue); dynamic; abstract;

    property Dequeuer: TBoldExternalObjectSpaceEventHandler read FDequeuer write FDequeuer;
    property OnReceiveEvent: TReceivePropagatorEvent read FOnReceiveEvent write FOnReceiveEvent;
    property SendQueue: TBoldThreadSafeStringQueue read FSendQueue;
    property Controller: TBoldNotificationPropagatorPersistenceControllerPassthrough read GetController;
    { TBoldPersistenceHandlePassthrough }
    function CreatePersistenceController: TBoldPersistenceController; override;
    property SystemHandle: TBoldSystemHandle read FSystemHandle write FSystemHandle;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ClientId: TBoldClientId read fClientId;
  end;

const
  { Signature that is sent with every UDP propagation to identify ourselves }
  SPropagatorIdentification = '#BOLDEVENT#'#13#10;

implementation

uses
  SysUtils,
  BoldObjectSpaceExternalEvents;

{ TBoldAbstractNotificationPropagator }

constructor TBoldAbstractNotificationPropagator.Create(AOwner: TComponent);
begin
  inherited;

  FReceiveQueue := TBoldThreadSafeStringQueue.Create('NotificationPropagator/Receive');
  FReceiveQueue.OnQueueNotEmpty := OnReceiveQueueNotEmpty;

  FSendQueue := TBoldThreadSafeStringQueue.Create('NotificationPropagator/Send');
  FSendQueue.OnQueueNotEmpty := OnSendQueueNotEmpty;

  FRefreshQueue := TBoldThreadSafeStringQueue.Create('NotificationPropagator/Refresh');
  fClientId := 0;
end;

destructor TBoldAbstractNotificationPropagator.Destroy;
begin
  if Assigned(FDequeuer) then
    FDequeuer.Queue := nil;

  if Assigned(FSendQueue) then
    FreeAndNil(FSendQueue);
  if Assigned(FReceiveQueue) then
    FreeAndNil(FReceiveQueue);
  if Assigned(FRefreshQueue) then
    FreeAndNil(FRefreshQueue);
  inherited;
end;

{ Put a single event in the out-queue }
procedure TBoldAbstractNotificationPropagator.EnqueEvent(const Event: String);
begin
  FSendQueue.Enqueue(Event);
end;

procedure TBoldAbstractNotificationPropagator.EnqueEventList(
  const aEventList: TStrings);
begin
  FSendQueue.EnqueueList(aEventList);
end;

{ Decodes and deques a single event }
procedure TBoldAbstractNotificationPropagator.ReceiveEvent(const Event: String);
var
  AClassName, AMemberName, ALockName: String;
begin
  if Assigned(FDequeuer) then
  begin
    if FDequeuer.Queue <> FReceiveQueue then
      FDequeuer.Queue := FReceiveQueue;
    FReceiveQueue.Enqueue(Event);

    if TBoldObjectSpaceExternalEvent.DecodeExternalEvent(Event, AClassName,
        AMemberName, ALockName, nil) in [bsClassChanged] then
      FRefreshQueue.Enqueue(AClassName);
  end;

  if Assigned(FOnReceiveEvent) then
    FOnReceiveEvent(Self, Event);
end;

procedure TBoldAbstractNotificationPropagator.OnReceiveQueueNotEmpty(Sender: TBoldThreadSafeQueue);
begin
  while not FRefreshQueue.Empty do
    if Assigned(FDequeuer) and Assigned(FDequeuer.BoldSystemHandle) then
      FDequeuer.BoldSystemHandle.System.ClassByExpressionName[
        FRefreshQueue.Dequeue].Invalidate;

  if Assigned(FDequeuer) then
    FDequeuer.QueueNotEmpty;
end;

function TBoldAbstractNotificationPropagator.CreatePersistenceController: TBoldPersistenceController;
var
  TempController: TBoldNotificationPropagatorPersistenceControllerPassthrough;
begin
  TempController := TBoldNotificationPropagatorPersistenceControllerPassthrough.Create(SystemHandle.SystemTypeInfoHandle.BoldModel.MoldModel, ClientId);
  ChainPersistenceController(TempController);
  TempController.BoldNotificationPropagator := Self;
  Result := TempController;
end;


function TBoldAbstractNotificationPropagator.GetController: TBoldNotificationPropagatorPersistenceControllerPassthrough;
begin
  result := PersistenceController as TBoldNotificationPropagatorPersistenceControllerPassthrough;
end;

{ TBoldNotificationPropagatorPersistenceControllerPassthrough }

constructor TBoldNotificationPropagatorPersistenceControllerPassthrough.Create(MoldModel: TMoldModel; ClientId: TBoldClientId);
begin
  inherited Create(MoldModel);
  fClientId := ClientId;
end;

procedure TBoldNotificationPropagatorPersistenceControllerPassthrough.PMUpdate(
  ObjectIdList: TBoldObjectIdList; ValueSpace, Old_Values: IBoldValueSpace;
  Precondition: TBoldUpdatePrecondition;
  TranslationList: TBoldIdTranslationList;
  var TimeStamp: TBoldTimeStampType; var TimeOfLatestUpdate: TDateTime; BoldClientID: TBoldClientID);
begin
  inherited PMUpdate(ObjectIdList, valueSpace, Old_Values, PreCondition, TranslationList, TimeStamp, TimeOfLatestUpdate, ClientId);
end;

procedure TBoldNotificationPropagatorPersistenceControllerPassthrough.TransmitEvents(const ClientID: TBoldClientID);
begin
  try
    BoldNotificationPropagator.EnqueEventList(Events);
  finally
    ClearEvents;
  end;
end;

end.
