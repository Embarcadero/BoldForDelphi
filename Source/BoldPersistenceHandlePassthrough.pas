{ Global compiler directives }
{$include bold.inc}
unit BoldPersistenceHandlePassthrough;

interface

uses
  BoldPersistenceHandle,
  BoldPersistenceControllerPassthrough,
  BoldSubscription,
  Classes;

type
  TBoldPersistenceHandlePassthrough = class(TBoldPersistenceHandle)
  private
    fNextPersistenceHandle :TBoldPersistenceHandle;
    fNextPHandleSubscriber: TBoldPassthroughSubscriber;
    procedure SetNextPersistenceHandle(NextPHandle: TBoldPersistenceHandle);
    procedure NextPHandleDeleted(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    function GetLastPersistenceHandle: TBoldPersistenceHandle;
  protected
    procedure ChainPersistenceController(PersistenceController: TBoldPersistenceControllerPassThrough);
    procedure SetActive(Value: Boolean); override;
    procedure InitNextPHandle(NextPHandle: TBoldPersistenceHandle); virtual; 
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    property LastPersistenceHandle: TBoldPersistenceHandle read GetLastPersistenceHandle;
    property NextPersistenceHandle: TBoldPersistenceHandle read fNextPersistenceHandle write {fNextPersistenceHandle}SetNextPersistenceHandle;
  end;

implementation

uses
  SysUtils;

{ TBoldPersistenceControllerPassthroughHandle }

procedure TBoldPersistenceHandlePassthrough.SetActive(Value: Boolean);
begin
  inherited;
  if Assigned(NextPersistenceHandle) then
    NextPersistenceHandle.Active := Value;
end;

procedure TBoldPersistenceHandlePassthrough.SetNextPersistenceHandle(
  NextPHandle: TBoldPersistenceHandle);
begin
  if fNextPersistenceHandle <> NextPHandle then
  begin
    fNextPHandleSubscriber.CancelAllSubscriptions;
    fNextPersistenceHandle := NextPHandle;
    if Assigned(fNextPersistenceHandle) then
      fNextPersistenceHandle.AddSmallSubscription(fNextPHandleSubscriber, [beDestroying], beDestroying);
    InitNextPHandle(NextPHandle);      
  end;
end;

procedure TBoldPersistenceHandlePassthrough.NextPHandleDeleted(Originator: TObject; OriginalEvent: TBoldEvent;
 RequestedEvent: TBoldRequestedEvent);
begin
  if (Originator = fNextPersistenceHandle) and (RequestedEvent = beDestroying) then
    NextPersistenceHandle := nil;
end;

constructor TBoldPersistenceHandlePassthrough.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  fNextPHandleSubscriber := TBoldPassthroughSubscriber.Create(NextPHandleDeleted);
end;

destructor TBoldPersistenceHandlePassthrough.Destroy;
begin
  FreePublisher;
  FreeAndNil(fNextPHandleSubscriber);
  fNextPersistenceHandle := nil;
  inherited;  
end;

function TBoldPersistenceHandlePassthrough.GetLastPersistenceHandle: TBoldPersistenceHandle;
begin
  result := NextPersistenceHandle;
  while result is TBoldPersistenceHandlePassthrough do
    result := TBoldPersistenceHandlePassthrough(result).NextPersistenceHandle;
end;

procedure TBoldPersistenceHandlePassthrough.InitNextPHandle(
  NextPHandle: TBoldPersistenceHandle);
begin
end;

procedure TBoldPersistenceHandlePassthrough.ChainPersistenceController(PersistenceController: TBoldPersistenceControllerPassThrough);
begin
  if Assigned(NextPersistenceHandle) then
    PersistenceController.NextPersistenceController := NextPersistenceHandle.PersistenceController;
end;

end.