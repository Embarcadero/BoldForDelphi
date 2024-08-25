{ Global compiler directives }
{$include bold.inc}
unit BoldIDAdderHandle;

interface

uses
  BoldListenerHandle,
  BoldPersistenceHandlePassthrough,
  BoldPersistenceController,
  BoldPersistenceControllerPassthrough,
  BoldPropagatorConstants,
  BoldPersistenceHandle,
  BoldSubscription,
  comobj,
  classes;

type
  { forward declarations }
  TBoldIdAdderHandle = class;

  { TBoldIDAdderHandle }
  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldIDAdderHandle = class(TBoldPersistenceHandlePassthrough)
  private
    fBoldListenerHandle: TBoldListenerHandle;
    fPTSubscriber: TBoldPassThroughSubscriber;
    procedure _Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    procedure SetBoldListenerHandle(const Value: TBoldListenerHandle);
    procedure Subscribe(const DoSubscribe: Boolean);
  protected
    function CreatePersistenceController: TBoldPersistenceController; override;
    procedure SetActive(Value: Boolean); override;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
  published
    property NextPersistenceHandle;
    property BoldListener: TBoldListenerHandle read fBoldListenerHandle write SetBoldListenerHandle;
  end;

implementation

uses
  Dialogs,
  SysUtils,

  BoldCoreConsts,
  BoldUtils,
  BoldIDAdder;

{ TBoldIDAdderHandle }

constructor TBoldIDAdderHandle.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  fPTSubscriber := TBoldPassthroughSubscriber.Create(_Receive);
end;

function TBoldIDAdderHandle.CreatePersistenceController: TBoldPersistenceController;
var
  Temp: TBoldIDAdder;
begin
  temp := TBoldIDAdder.Create;
  ChainPersistenceController(Temp);
  if Assigned(fBoldListenerHandle) then
    Temp.Listener := fBoldListenerHandle.ListenerThread;
  result := Temp;
end;

destructor TBoldIDAdderHandle.Destroy;
begin
  FreeAndNil(fPTSubscriber);
  inherited;
end;

procedure TBoldIDAdderHandle.SetActive(Value: Boolean);
begin
  inherited;
  BoldListener.SetActive(Value);
end;

procedure TBoldIDAdderHandle._Receive(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  if (Originator = fBoldListenerHandle) and (RequestedEvent = beDestroying) then
    BoldListener := nil;
end;

procedure TBoldIDAdderHandle.SetBoldListenerHandle(
  const Value: TBoldListenerHandle);
begin
  if (fBoldListenerHandle <> Value) then
  begin
    Subscribe(False);
    fBoldListenerHandle := Value;
    Subscribe(True);
  end;
end;

procedure TBoldIDAdderHandle.Subscribe(const DoSubscribe: Boolean);
begin
  if DoSubscribe then
  begin
    if not (csDesigning in ComponentState) then
    begin
      if Assigned(fBoldListenerHandle) then
        fBoldListenerHandle.AddSmallSubscription(fPTSubscriber, [beDestroying], beDestroying);
    end;
  end
  else
    fPTSubscriber.CancelAllSubscriptions;
end;

end.
