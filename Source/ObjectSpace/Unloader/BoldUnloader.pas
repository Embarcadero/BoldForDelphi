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

  TBoldUnLoader = class;

  TBoldUnLoader = class(TBoldMemoryManagedObject)
  private
    fBoldSystem: TBoldSystem;
    fUnloadDelayedFetch: boolean;
    fMinAgeForUnload: integer;
    fScanPerTick: integer;
    fActive: boolean;
    fWaitCount: integer;
    fTraverser: TBoldLocatorListTraverser;
    fOnMayInvalidate: TBoldInvalidateMemberEvent;
    fOnMayUnload: TBoldUnloadObjectEvent;
    procedure Scan;
    procedure ScanObject(BoldObject: TBoldObject);
    procedure SetActive(const Value: boolean);
    procedure SetBoldSystem(const Value: TBoldSystem);
    procedure StartScan;
    procedure StartWait;
  public
    destructor Destroy; override;
    procedure Tick;
    property BoldSystem: TBoldSystem read fBoldSystem write SetBoldSystem;
    property ScanPerTick: integer read fScanPerTick write fScanPerTick;
    property MinAgeForUnload: integer read fMinAgeForUnload write fMinAgeForUnload;
    property UnloadDelayedFetch: boolean read fUnloadDelayedFetch write fUnloadDelayedFetch;
    property Active: boolean read fActive write SetActive;
    property OnMayInvalidate: TBoldInvalidateMemberEvent read fOnMayInvalidate write fOnMayInvalidate;
    property OnMayUnload: TBoldUnloadObjectEvent read fOnMayUnload write fOnMayUnload;
  end;


implementation

uses
  SysUtils,
  BoldUtils,
  BoldCoreConsts;

{ TBoldUnLoader }

procedure TBoldUnLoader.SetActive(const Value: boolean);
begin
  if Active <> Value then
  begin
    fActive := Value;
    if Value then
    begin
      if Assigned(BoldSystem) then
        StartScan
      else
        raise EBold.CreateFmt(sNeedSystemToActivate, [ClassName]);
    end
    else
      FreeAndNil(fTraverser);
  end;
end;

procedure TBoldUnLoader.SetBoldSystem(const Value: TBoldSystem);
begin
  Active := False;
  fBoldSystem := Value;
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

procedure TBoldUnLoader.Scan;
var
  Locator: TBoldObjectLocator;
  Scanned: integer;
begin
 Scanned := 0;
 while (not fTraverser.EndOfList) and (Scanned < ScanPerTick) do
  begin
    Locator := fTraverser.Locator;
    if assigned(Locator.BoldObject) and (Locator.BoldObject.BoldExistenceState = besExisting) then
      ScanObject(Locator.BoldObject);
    fTraverser.Next;
    inc(Scanned);
  end;
end;

procedure TBoldUnLoader.ScanObject(BoldObject: TBoldObject);

  procedure ScanMembers;
  var
    m: integer;
    Member: TBoldMember;
    DoInvalidate: Boolean;
  begin
    for m := 0 to BoldObject.BoldMemberCount-1 do
    if BoldObject.BoldMemberAssigned[m] then
    begin
      Member := BoldObject.BoldMembers[m];
      with BoldObject.BoldMembers[m] do
      begin
        if (not Touched) then
        begin
          if (Derived and (BoldPersistenceState = bvpsTransient)) or
            (UnloadDelayedFetch and (BoldMemberRTInfo.DelayedFetch = True) and (BoldPersistenceState = bvpsCurrent)) then
          begin
            DoInvalidate := not MemberHasSubscribers;
            if Assigned(OnMayInvalidate) then
              OnMayInvalidate(Member, DoInvalidate);
            if DoInvalidate then
              Invalidate;
          end;
        end;
      end;
    end;
  end;

var
  DoUnload: Boolean;

begin
  Scanmembers;
  if BoldObject.Touched then
    BoldObject.ClearTouched
  else if BoldObject.BoldPersistent and not BoldObject.BoldDirty then
  begin
    DoUnload := not BoldObject.ObjectHasSubscribers;
    if Assigned(OnMayUnload) then
      OnMayUnload(BoldObject, DoUnload);
    if DoUnload then
      BoldObject.BoldObjectLocator.UnloadBoldObject
  end;
end;

procedure TBoldUnLoader.Tick;
begin
  if Active then
    if Assigned(fTraverser) then // in a Scan
    begin
      Scan;
      if fTraverser.EndOfList then
        StartWait;
    end
    else  // waiting
    begin
      INC(fWaitCount);
      if fWaitCount > MinAgeForUnload then
        StartScan;
    end;
end;

destructor TBoldUnLoader.Destroy;
begin
  FreeAndNil(fTraverser);
  inherited;
end;

end.




