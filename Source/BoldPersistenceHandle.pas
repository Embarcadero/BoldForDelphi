
{ Global compiler directives }
{$include bold.inc}
unit BoldPersistenceHandle;

interface

uses
  Classes,
  BoldHandle,
  BoldSubscription,
  BoldPersistenceController;

type
  TBoldPersistenceHandle = class;
  TBoldPersistenceHandleClass = class of TBoldHandle;

  TBoldPersistenceHandle = class(TBoldHandle)
  private
    fActive: Boolean;
    fPersistenceController: TBoldPersistenceController;
    fPersistenceSubscriber: TBoldExtendedPassthroughSubscriber;
    function GetPersistenceController: TBoldPersistenceController;
    function GetHasPersistenceController: Boolean;
    function GetPersistenceSubscriber: TBoldExtendedPassthroughSubscriber;
  protected
    procedure CheckInactive(const Action: String);
    function CreatePersistenceController: TBoldPersistenceController; virtual; abstract;
    function GetHandledObject: TObject; override;
    procedure ReceiveExtendedEvent(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const);
    function GetActive: boolean; virtual;
    procedure SetActive(Value: Boolean); virtual;
    property HasPersistenceController: Boolean read GetHasPersistenceController;
    property PersistenceSubscriber: TBoldExtendedPassthroughSubscriber read GetPersistenceSubscriber;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure AddPersistenceSubscription(Subscriber: TBoldSubscriber; Events: TBoldSmallEventSet = []); virtual;
    procedure ReleasePersistenceController;
    property Active: Boolean read GetActive write SetActive;
    property PersistenceController: TBoldPersistenceController read GetPersistenceController;
  end;

implementation

uses
  SysUtils,

  BoldCoreConsts,
  BoldDefs;

function TBoldPersistenceHandle.GetHandledObject: TObject;
begin
  result := PersistenceController;
end;

function TBoldPersistenceHandle.GetActive: Boolean;
begin
  result := fActive;
end;

procedure TBoldPersistenceHandle.SetActive(Value: Boolean);
begin
  if fActive <> Value then
  begin
    fActive := Value;
    if not Value then
    begin
      SendEvent(self, beDeactivating);
      ReleasePersistenceController;
    end;
  end;
end;

procedure TBoldPersistenceHandle.CheckInactive(const Action: String);
begin
  if Active then
    raise EBold.CreateFmt(sNotAllowedOnActiveHandle, [Action]);
end;

constructor TBoldPersistenceHandle.create(Owner: TComponent);
begin
  inherited;
  fActive := False;
end;

function TBoldPersistenceHandle.GetPersistenceController: TBoldPersistenceController;
begin
  if not assigned(fPersistenceController) then
  begin
    fPersistenceController := CreatePersistenceController;
    fPersistenceController.AddSmallSubscription(PersistenceSubscriber, [beDestroying], beDestroying);
  end;
  result := fPersistenceController;
end;

function TBoldPersistenceHandle.GetPersistenceSubscriber: TBoldExtendedPassthroughSubscriber;
begin
  if not Assigned(fPersistenceSubscriber) then
    fPersistenceSubscriber := TBoldExtendedPassthroughSubscriber.CreateWithExtendedReceive(ReceiveExtendedEvent);
  result := fPersistenceSubscriber;
end;

destructor TBoldPersistenceHandle.Destroy;
begin
  FreePublisher;
  FreeAndNil(fPersistenceSubscriber);
  FreeAndNil(fPersistenceController);
  inherited;
end;

procedure TBoldPersistenceHandle.ReceiveExtendedEvent(
  Originator: TObject; OriginalEvent: TBoldEvent;
  RequestedEvent: TBoldRequestedEvent; const Args: array of const);
begin
  if (Originator = fPersistenceController) and (OriginalEvent = beDestroying) then
  begin
    fPersistenceController := nil;
  end
  else
    SendExtendedEvent(Originator, OriginalEvent, Args);
end;

procedure TBoldPersistenceHandle.AddPersistenceSubscription(Subscriber: TBoldSubscriber; Events: TBoldSmallEventSet = []);
begin
  PersistenceSubscriber.CancelAllSubscriptions;
  PersistenceController.AddSmallSubscription(fPersistenceSubscriber, [beDestroying]);
  PersistenceController.SubscribeToPersistenceEvents(fPersistenceSubscriber);
end;

procedure TBoldPersistenceHandle.ReleasePersistenceController;
begin
  Active := false;
  FreeAndNil(fPersistenceSubscriber);
  FreeAndNil(fPersistenceController);
end;

function TBoldPersistenceHandle.GetHasPersistenceController: Boolean;
begin
  result := assigned(fPersistenceController);
end;

end.
