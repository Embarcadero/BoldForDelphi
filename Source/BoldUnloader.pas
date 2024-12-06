{ Global compiler directives }
{$include bold.inc}

unit BoldUnloader;

interface

uses
  BoldDefs,
  BoldBase,
  BoldValueSpaceInterfaces,
  BoldSystem,
  Classes;

type
  { Forward declaration of classes }
  TBoldInvalidateMemberEvent = procedure(Member: TBoldMember; var Invalidate: Boolean) of object;
  TBoldUnloadObjectEvent = procedure(BoldObject: TBoldObject; var Unload: Boolean) of object;
  TBoldReportUnloadEvent = procedure(const Scanned, UnloadedObjects, InvalidatedMembers: integer) of object;
  TBoldMayUnloadStartEvent = procedure(var aStart: boolean) of object;

  TBoldUnLoader = class;

  TBoldUnLoader = class(TBoldMemoryManagedObject)
  private
    fBoldSystem: TBoldSystem;
    fUnloadDelayedFetch: boolean;
    fMinAgeForUnload: integer;
    fScanPerTick: integer;
    fActive: boolean;
    fWaitCount: integer;
    fScanned: integer;
    fInvalidatedMemberCount: integer;
    fUnloadedObjectCount: integer;
    fScanTime: TDateTime;
    fTraverser: TBoldLocatorListTraverser;
    fOnMayInvalidate: TBoldInvalidateMemberEvent;
    fOnMayUnload: TBoldUnloadObjectEvent;
    fOnMayStart: TBoldMayUnloadStartEvent;
    fOnReportUnload: TBoldReportUnloadEvent;
    FUnloadFromCurrentClassList: boolean;
    function Scan: boolean;
    function ScanObject(BoldObject: TBoldObject): boolean;
    procedure SetActive(const Value: boolean);
    procedure SetBoldSystem(const Value: TBoldSystem);
    procedure StartScan;
    procedure StartWait;
  public
    destructor Destroy; override;
    procedure Tick;
    property InvalidatedMemberCount: integer read fInvalidatedMemberCount;
    property UnloadedObjectCount: integer read fUnloadedObjectCount;
    property Scanned: integer read fScanned;
    property ScanTime: TDateTime read fScanTime;
    property BoldSystem: TBoldSystem read fBoldSystem write SetBoldSystem;
    property ScanPerTick: integer read fScanPerTick write fScanPerTick; // milliseconds to spend in itterating on each tick
    property MinAgeForUnload: integer read fMinAgeForUnload write fMinAgeForUnload; // unit for MinAgeForUnload is TickInterval
    property UnloadDelayedFetch: boolean read fUnloadDelayedFetch write fUnloadDelayedFetch;
    property UnloadFromCurrentClassList: boolean read FUnloadFromCurrentClassList write FUnloadFromCurrentClassList;
    property Active: boolean read fActive write SetActive;
    property OnMayInvalidate: TBoldInvalidateMemberEvent read fOnMayInvalidate write fOnMayInvalidate;
    property OnMayUnload: TBoldUnloadObjectEvent read fOnMayUnload write fOnMayUnload;
    property OnMayStart: TBoldMayUnloadStartEvent read fOnMayStart write fOnMayStart;
    property OnReportUnload: TBoldReportUnloadEvent read fOnReportUnload write fOnReportUnload;
  end;

implementation

uses
  SysUtils,
  Windows,

  DateUtils,
  BoldUtils,
  BoldCoreConsts,
  BoldSystemRT,
  BoldIndex,
  BoldElements,
  BoldSubscription;

{ TBoldUnLoader }

procedure TBoldUnLoader.SetActive(const Value: boolean);
begin
  if Active <> Value then
  begin
    fActive := Value;
    if Value then
    begin
      if Assigned(BoldSystem) then
        Tick
      else
        raise EBold.CreateFmt(sNeedSystemToActivate, [ClassName]);
    end
    else
      FreeAndNil(fTraverser);
  end;
end;

procedure TBoldUnLoader.SetBoldSystem(const Value: TBoldSystem);
begin
  if fBoldSystem <> Value then
  begin
    Active := False;
    fBoldSystem := Value;
  end;
end;

procedure TBoldUnLoader.StartScan;
begin
  Assert(not Assigned(fTraverser));
  fTraverser := BoldSystem.Locators.CreateTraverser;
end;

procedure TBoldUnLoader.StartWait;
begin
  FreeAndNil(fTraverser);
  fWaitCount := 0;
end;

function TBoldUnLoader.Scan: boolean;
var
  Locator: TBoldObjectLocator;
  vScanned, vUnloadedObjectCount, vInvalidatedMemberCount: integer;
  lStartTime: Int64;
  lNow: Int64;
  lTimeOut: boolean;
begin
  lStartTime := GetTickCount;
  lNow := lStartTime;
  vScanned := Scanned;
  vUnloadedObjectCount := UnloadedObjectCount;
  vInvalidatedMemberCount := InvalidatedMemberCount;
  lTimeOut := false;
  while not lTimeOut and (fTraverser.MoveNext) do
  begin
    Locator := fTraverser.Locator;
    if assigned(Locator.BoldObject) and (Locator.BoldObject.BoldExistenceState = besExisting) and not Locator.BoldObject.BoldDirty then
      ScanObject(Locator.BoldObject);
    inc(fScanned);
    lNow := GetTickCount;
    lTimeOut := (lNow - lStartTime > scanPerTick) or (lNow < lStartTime);
  end;
  IncMilliSecond(ScanTime, lNow-lStartTime);
  if Assigned(fOnReportUnload) then
    fOnReportUnload(Scanned - vScanned, UnloadedObjectCount - vUnloadedObjectCount, InvalidatedMemberCount - vInvalidatedMemberCount);
  result := lTimeOut;
end;

function TBoldUnLoader.ScanObject(BoldObject: TBoldObject): boolean;

  procedure ScanMembers;
  var
    m: integer;
    Member: TBoldMember;
    lDoInvalidate: Boolean;
  begin
    for m := 0 to BoldObject.BoldMemberCount-1 do
    begin
      Member := BoldObject.BoldMemberIfAssigned[m];
      if Assigned(Member) then
      begin
        with Member do
        begin
          if (not Touched) then
          begin
          if (Derived and (BoldPersistenceState = bvpsTransient)) or
            ((BoldPersistenceState = bvpsCurrent) and (not BoldMemberRTInfo.DelayedFetch or UnloadDelayedFetch)) then
            begin
              lDoInvalidate := not MemberHasSubscribers;
              if Assigned(OnMayInvalidate) then
                OnMayInvalidate(Member, lDoInvalidate);
              if lDoInvalidate then
              begin
                inc(fInvalidatedMemberCount);
                Invalidate;
              end;
            end;
          end;
        end;
      end;
    end;
  end;

var
  vDoUnload: Boolean;
begin
  result := false;
  Scanmembers;
  with BoldObject do
  begin
    if Touched then
      ClearTouched
    else if BoldPersistent and not BoldDirty and not ObjectHasSubscribers then
    begin
      vDoUnload:= UnloadFromCurrentClassList or (BoldSystem.Classes[BoldClassTypeInfo.TopSortedIndex].BoldPersistenceState <> bvpsCurrent);
      if Assigned(OnMayUnload) then
        OnMayUnload(BoldObject, vDoUnload);
      if vDoUnload then
      begin
        BoldObjectLocator.UnloadBoldObject;
        inc(fUnloadedObjectCount);
        result := true;
      end;
    end;
  end;
end;

procedure TBoldUnLoader.Tick;
var
  lStart: boolean;
begin
  if Active then
  begin
    if BoldSystem.InTransaction or BoldSystem.IsProcessingTransactionOrUpdatingDatabase or BoldSystem.IsDerivingMembers  then
      exit;
    if Assigned(fOnMayStart) then
    begin
      lStart := true;
      fOnMayStart(lStart);
      if not lStart then
        exit;
    end;
    if Assigned(fTraverser) then // in a Scan
    begin
      if not Scan then
        StartWait;
    end
    else  // waiting
    begin
      INC(fWaitCount);
      if fWaitCount >= MinAgeForUnload then
        StartScan;
    end;
  end;
end;

destructor TBoldUnLoader.Destroy;
begin
  FreeAndNil(fTraverser);
  inherited;
end;

end.
