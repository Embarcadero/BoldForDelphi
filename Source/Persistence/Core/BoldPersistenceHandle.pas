unit BoldPersistenceHandle;

interface

uses
  Classes,
  BoldDefs,
  BoldHandle,
  BoldSubscription,
  BoldPersistenceController;

type
  TBoldPersistenceHandle = class(TBoldHandle)
  private
    fActive: Boolean;
    fPersistenceController: TBoldPersistenceController;
    fPersistenceSubscriber: TBoldPassThroughSubscriber;
    function GetActive: Boolean;
    function GetPersistenceController: TBoldPersistenceController;
    function GetHasPersistenceController: Boolean;
  protected
    procedure CheckInactive(Action: String);
    function CreatePersistenceController: TBoldPersistenceController; virtual; abstract;
    function GetHandledObject: TObject; override;
    procedure ReceiveExtendedEvent(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const);
    procedure SetActive(Value: Boolean); virtual;
    property HasPersistenceController: Boolean read GetHasPersistenceController;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure AddPersistenceSubscription(Subscriber: TBoldSubscriber); virtual;
    procedure ReleasePersistenceController;
    property Active: Boolean read GetActive write SetActive;
    property PersistenceController: TBoldPersistenceController read GetPersistenceController;
  end;

implementation

uses
  SysUtils,
  PersistenceConsts;

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
  fActive := Value;
  if not Active then
    SendEvent(self, beDeactivating);
end;

procedure TBoldPersistenceHandle.CheckInactive(Action: String);
begin
  if active then
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
    fPersistenceController := CreatePersistenceController;
  result := fPersistenceController;
end;

destructor TBoldPersistenceHandle.Destroy;
begin
  FreePublisher;
  FreeAndNil(fPersistenceController);
  FreeAndNil(fPersistenceSubscriber);
  inherited;
end;

procedure TBoldPersistenceHandle.ReceiveExtendedEvent(
  Originator: TObject; OriginalEvent: TBoldEvent;
  RequestedEvent: TBoldRequestedEvent; const Args: array of const);
begin
  SendExtendedEvent(Originator, OriginalEvent, Args);
end;

procedure TBoldPersistenceHandle.AddPersistenceSubscription(Subscriber: TBoldSubscriber);
begin
  fPersistenceSubscriber := TBoldPassThroughSubscriber.CreateWithExtendedReceive(ReceiveExtendedEvent);
  PersistenceController.SubscribeToPeristenceEvents(fPersistenceSubscriber);
  AddSubscription(Subscriber, bpeEndFetch, bpeEndFetch);
  AddSubscription(Subscriber, bpeEndUpdate, bpeEndUpdate);
  AddSubscription(Subscriber, bpeEndFetchId, bpeEndUpdate);
  AddSubscription(Subscriber, bpeFetchObject, bpeFetchObject);
  AddSubscription(Subscriber, bpeFetchMember, bpeFetchMember);
  AddSubscription(Subscriber, bpeUpdateObject, bpeUpdateObject);
  AddSubscription(Subscriber, bpeCreateObject, bpeCreateObject);
  AddSubscription(Subscriber, bpeDeleteObject, bpeDeleteObject);
  AddSubscription(Subscriber, bpeFetchId, bpeFetchId);
  AddSubscription(Subscriber, bpeStartFetch, bpeStartFetch);
  AddSubscription(Subscriber, bpeStartUpdate, bpeStartUpdate);
  AddSubscription(Subscriber, bpeStartFetchId, bpeStartUpdate);
end;

procedure TBoldPersistenceHandle.ReleasePersistenceController;
begin
  if active then
    Active := false;
  FreeAndNil(fPersistenceController);
end;

function TBoldPersistenceHandle.GetHasPersistenceController: Boolean;
begin
  result := assigned(fPersistenceController);
end;

end.
