/////////////////////////////////////////////////////////


unit BoldUnloaderHandle;

interface

uses
  BoldDefs,
  BoldBase,
  BoldUnloader,
  BoldSubscription,
  BoldSystem,
  BoldSystemHandle,
  BoldHandles,
  Classes,
  ExtCtrls;

const
  cTickInterval = 60 * 1000; // 1 minute
  cScanPerTick = 100; // milliseconds, max time to spend scaning
  cMinAgeForUnload = 5; // unit is the TickInterval so (cTickInterval * cMinAgeForUnload) = 5 minutes

type
  { Forward declaration of classes }
  TBoldUnloaderHandle = class;

  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldUnloaderHandle = class(TBoldSystemExtensionComponent)
  private
    fActive: Boolean;
    fUnloader: TBoldUnloader;
    fTimer: TTimer;
    function GetUnloadDelayedFetch: boolean;
    function GetMinAgeForUnload: integer;
    function GetScanPerTick: integer;
    procedure SetActive(const Value: boolean);
    procedure SetUnloadDelayedFetch(const Value: boolean);
    procedure SetMinAgeForUnload(const Value: integer);
    procedure SetScanPerTick(const Value: integer);
    function GetOnMayStart: TBoldMayUnloadStartEvent;
    procedure SetOnMayStart(const Value: TBoldMayUnloadStartEvent);
    function GetOnReportUnload: TBoldReportUnloadEvent;
    procedure SetOnReportUnload(const Value: TBoldReportUnloadEvent);
    function GetUnloadFromCurrentClassList: boolean;
    procedure SetUnloadFromCurrentClassList(const Value: boolean);
    property Unloader: TBoldUnloader read fUnloader;
    procedure Tick(Sender: Tobject);
    procedure PropagateToUnloder;
    function GetTickInterval: integer;
    procedure SetTickInterval(const Value: integer);
    function GetOnMayInvalidate: TBoldInvalidateMemberEvent;
    function GetOnMayUnload: TBoldUnloadObjectEvent;
    procedure SetOnMayInvalidate(const Value: TBoldInvalidateMemberEvent);
    procedure SetOnMayUnload(const Value: TBoldUnloadObjectEvent);
  protected
    procedure StaticBoldTypeChanged; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property TickInterval: integer read GetTickInterval write SetTickInterval default cTickInterval;
    property ScanPerTick: integer read GetScanPerTick write SetScanPerTick default cScanPerTick;
    property MinAgeForUnload: integer read GetMinAgeForUnload write SetMinAgeForUnload default cMinAgeForUnload;
    property UnloadDelayedFetch: boolean read GetUnloadDelayedFetch write SetUnloadDelayedFetch default false;
    property UnloadFromCurrentClassList: boolean read GetUnloadFromCurrentClassList write SetUnloadFromCurrentClassList default false;
    property Active: boolean read fActive write SetActive default true;
    property OnMayInvalidate: TBoldInvalidateMemberEvent read GetOnMayInvalidate write SetOnMayInvalidate;
    property OnMayUnload: TBoldUnloadObjectEvent read GetOnMayUnload write SetOnMayUnload;
    property OnMayStart: TBoldMayUnloadStartEvent read GetOnMayStart write SetOnMayStart;
    property OnReportUnload: TBoldReportUnloadEvent read GetOnReportUnload write SetOnReportUnload;        
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
  fTimer := TTimer.Create(Self);
  fTimer.Enabled := True;
  TickInterval := cTickInterval;
  ScanPertick := cScanPertick;
  MinAgeForUnload := cMinAgeForUnload;
  fTimer.OnTimer := Tick;
  Active := True;
end;

destructor TBoldUnloaderHandle.Destroy;
begin
  FreeAndNil(fUnLoader);
  FreeAndNil(fTimer);
  inherited;
end;

function TBoldUnloaderHandle.GetUnloadDelayedFetch: boolean;
begin
  Result := Unloader.UnloadDelayedFetch;
end;

function TBoldUnloaderHandle.GetUnloadFromCurrentClassList: boolean;
begin
  result := Unloader.UnloadFromCurrentClassList;
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
    if Assigned(StaticSystemHandle) then
      fUnloader.BoldSystem := StaticSystemHandle.System
    else
     fUnloader.BoldSystem := nil;
    fUnloader.Active := Assigned(fUnloader.BoldSystem);
  end
  else
    fUnLoader.Active := False;
end;

procedure TBoldUnloaderHandle.SetActive(const Value: boolean);
begin
  if (fActive <> Value) then
  begin
    fActive := Value;
    PropagateToUnloder;
  end;
end;

procedure TBoldUnloaderHandle.SetUnloadDelayedFetch(
  const Value: boolean);
begin
  Unloader.UnloadDelayedFetch := Value;
end;

procedure TBoldUnloaderHandle.SetUnloadFromCurrentClassList(
  const Value: boolean);
begin
  Unloader.UnloadFromCurrentClassList := Value;
end;

procedure TBoldUnloaderHandle.StaticBoldTypeChanged;
begin
  inherited;
  PropagateToUnloder;
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

function TBoldUnloaderHandle.GetOnMayStart: TBoldMayUnloadStartEvent;
begin
  Result := Unloader.OnMayStart;
end;

function TBoldUnloaderHandle.GetOnMayUnload: TBoldUnloadObjectEvent;
begin
  Result := Unloader.OnMayUnload;
end;

function TBoldUnloaderHandle.GetOnReportUnload: TBoldReportUnloadEvent;
begin
  result := Unloader.OnReportUnload;
end;

procedure TBoldUnloaderHandle.SetOnMayInvalidate(
  const Value: TBoldInvalidateMemberEvent);
begin
  Unloader.OnMayInvalidate := Value;
end;

procedure TBoldUnloaderHandle.SetOnMayStart(const Value: TBoldMayUnloadStartEvent);
begin
  Unloader.OnMayStart := Value;
end;

procedure TBoldUnloaderHandle.SetOnMayUnload(
  const Value: TBoldUnloadObjectEvent);
begin
  Unloader.OnMayUnload := Value;
end;

procedure TBoldUnloaderHandle.SetOnReportUnload(
  const Value: TBoldReportUnloadEvent);
begin
  Unloader.OnReportUnload := Value;
end;

end.

