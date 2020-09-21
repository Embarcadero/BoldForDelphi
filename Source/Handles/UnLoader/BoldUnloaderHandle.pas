unit BoldUnloaderHandle;

interface

uses
  BoldDefs,
  BoldBase,
  BoldUnloader,
  BoldSubscription,
  BoldSystem,
  BoldSystemHandle,
  Classes,
  ExtCtrls;

type
  { Forward declaration of classes }
  TBoldUnloaderHandle = class;

  TBoldUnloaderHandle = class(TComponent)
  private
    fActive: Boolean;
    fUnloader: TBoldUnloader;
    fTimer: TTimer;
    fBoldSystemHandle: TBoldSystemHandle;
    fSubscriber: TBoldPassthroughSubscriber;
    function GetUnloadDelayedFetch: boolean;
    function GetMinAgeForUnload: integer;
    function GetScanPerTick: integer;
    procedure ReceiveFromSystemHandle(Originator: TObject; OriginalEvent: TBoldEvent;
    RequestedEvent: TBoldRequestedEvent);
    procedure SetActive(const Value: boolean);
    procedure SetUnloadDelayedFetch(const Value: boolean);
    procedure SetMinAgeForUnload(const Value: integer);
    procedure SetScanPerTick(const Value: integer);
    property Unloader: TBoldUnloader read fUnloader;
    procedure SetBoldSystemHandle(const Value: TBoldSystemHandle);
    procedure Tick(Sender: Tobject);
    procedure PropagateToUnloder;
    function GetTickInterval: integer;
    procedure SetTickInterval(const Value: integer);
    function GetOnMayInvalidate: TBoldInvalidateMemberEvent;
    function GetOnMayUnload: TBoldUnloadObjectEvent;
    procedure SetOnMayInvalidate(const Value: TBoldInvalidateMemberEvent);
    procedure SetOnMayUnload(const Value: TBoldUnloadObjectEvent);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property TickIntervall: integer read GetTickInterval write SetTickInterval default 1000;
    property BoldSystemHandle: TBoldSystemHandle read fBoldSystemHandle write SetBoldSystemHandle;
    property ScanPerTick: integer read GetScanPerTick write SetScanPerTick default 1000;
    property MinAgeForUnload: integer read GetMinAgeForUnload write SetMinAgeForUnload default 300000;
    property UnloadDelayedFetch: boolean read GetUnloadDelayedFetch write SetUnloadDelayedFetch;
    property Active: boolean read fActive write SetActive default true;
    property OnMayInvalidate: TBoldInvalidateMemberEvent read GetOnMayInvalidate write SetOnMayInvalidate;
    property OnMayUnload: TBoldUnloadObjectEvent read GetOnMayUnload write SetOnMayUnload;
  end;


implementation

uses
  SysUtils,
  BoldUtils;
const
  breHandleDestroyed = 42;
  breSystemChanged = 43;

{ TBoldUnloaderHandle }

constructor TBoldUnloaderHandle.Create(AOwner: TComponent);
begin
  inherited;
  fUnLoader := TBoldUnloader.Create;
  fSubscriber := TBoldPassthroughSubscriber.Create(ReceiveFromSystemHandle);
  fTimer := TTimer.Create(Self);
  fTimer.Enabled := True;
  TickIntervall := 1000;
  ScanPertick := 1000;
  MinAgeForUnload := 300000;
  fTimer.OnTimer := Tick;
  Active := True;
end;

destructor TBoldUnloaderHandle.Destroy;
begin
  FreeAndNil(fSubscriber);
  FreeAndNil(fUnLoader);
  FreeAndNil(fTimer);
  inherited;
end;

function TBoldUnloaderHandle.GetUnloadDelayedFetch: boolean;
begin
  Result := Unloader.UnloadDelayedFetch;
end;

function TBoldUnloaderHandle.GetMinAgeForUnload: integer;
begin
  Result := Unloader.MinAgeForUnload;
end;

function TBoldUnloaderHandle.GetScanPerTick: integer;
begin
  Result := Unloader.ScanPerTick;
end;

function TBoldUnloaderHandle.GetTickInterval: integer;
begin
  Result := fTimer.Interval;
end;

procedure TBoldUnloaderHandle.PropagateToUnloder;
begin
  if Active then
  begin
    if Assigned(BoldSystemHandle) then
      fUnloader.BoldSystem := BoldSystemHandle.System
    else
     fUnloader.BoldSystem := nil;
    if Assigned(fUnloader.BoldSystem) then
      fUnloader.Active := True;
  end
  else
    fUnLoader.Active := False;
end;

procedure TBoldUnloaderHandle.ReceiveFromSystemHandle(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  if RequestedEvent = breHandleDestroyed then
    BoldSystemHandle := nil
  else
    PropagateToUnloder;
end;

procedure TBoldUnloaderHandle.SetActive(const Value: boolean);
begin
  if (fActive <> Value) then
  begin
    fActive := Value;
    PropagateToUnloder;
  end;
end;

procedure TBoldUnloaderHandle.SetBoldSystemHandle(
  const Value: TBoldSystemHandle);
begin
  if Value <> BoldSystemHandle then
  begin
    fSubscriber.CancelAllSubscriptions;
    fBoldSystemHandle := Value;
    if assigned(Value) then
    begin
      fBoldSystemHandle.AddSmallSubscription(fSubscriber, [beDestroying], breHandleDestroyed);
      fBoldSystemHandle.AddSmallSubscription(fSubscriber, [beValueIdentityChanged], breSystemChanged);
    end;
    PropagateToUnloder;
  end;
end;

procedure TBoldUnloaderHandle.SetUnloadDelayedFetch(
  const Value: boolean);
begin
  Unloader.UnloadDelayedFetch := Value;
end;

procedure TBoldUnloaderHandle.SetMinAgeForUnload(
  const Value: integer);
begin
  Unloader.MinAgeForUnload := Value;
end;

procedure TBoldUnloaderHandle.SetScanPerTick(const Value: integer);
begin
  Unloader.ScanPerTick := Value;
end;

procedure TBoldUnloaderHandle.SetTickInterval(const Value: integer);
begin
  fTimer.Interval := Value;
end;

procedure TBoldUnloaderHandle.Tick(Sender: TObject);
begin
  UnLoader.Tick;
end;

function TBoldUnloaderHandle.GetOnMayInvalidate: TBoldInvalidateMemberEvent;
begin
  Result := Unloader.OnMayInvalidate;
end;

function TBoldUnloaderHandle.GetOnMayUnload: TBoldUnloadObjectEvent;
begin
  Result := Unloader.OnMayUnload;
end;

procedure TBoldUnloaderHandle.SetOnMayInvalidate(
  const Value: TBoldInvalidateMemberEvent);
begin
  Unloader.OnMayInvalidate := Value;
end;

procedure TBoldUnloaderHandle.SetOnMayUnload(
  const Value: TBoldUnloadObjectEvent);
begin
  Unloader.OnMayUnload := Value;
end;

end.




