
{ Global compiler directives }
{$include bold.inc}

unit BoldBase;

interface

uses
{$IFDEF DebugInstanceCounter}
  Classes,
  SysUtils,
{$ENDIF}
  BoldDefs;

type
  { forward declarations }
  TBoldInterfacedObject = class;
  TBoldRefCountedObject = class;
  TBoldNonRefCountedObject = class;

  {-- TBoldMemoryManagedObject --}
//  TBoldMemoryManagedObject = TObject; // Just an alias, to avoid 1 level deeper inheritance for no reason
  TBoldMemoryManagedObject = class(TObject)
  protected
    function GetDebugInfo: string; virtual;
    function ContextObject: TObject; virtual;
  public
    class function NewInstance: TObject; override;
    procedure FreeInstance; override;
    property DebugInfo: string read GetDebugInfo;
  end;

  {-- TBoldInterfacedObject --}
  TBoldInterfacedObject = class(TBoldMemoryManagedObject, IInterface)
  protected
    function QueryInterface(const IId: TGUID; out Obj): HResult; virtual; stdcall;
    function _AddRef: Integer; virtual; stdcall; abstract;
    function _Release: Integer; virtual; stdcall; abstract;
  public
    function SupportsInterface(const IID: TGUID): Boolean; virtual;
  end;

  {-- TBoldRefCountedObject --}
  TBoldRefCountedObject = class(TBoldInterfacedObject)
  protected
    FRefCount: Integer;
    function _AddRef: Integer; override;
    function _Release: Integer; override;
  public
    {$IFNDEF BOLD_BCB}
    procedure AfterConstruction; override;
    class function NewInstance: TObject; override;
    {$ENDIF}
    procedure BeforeDestruction; override;
    property RefCount: Integer read FRefCount;
  end;

  {-- TBoldNonRefCountedObject --}
  TBoldNonRefCountedObject = class(TBoldInterfacedObject)
  protected
    function _AddRef: Integer; override;
    function _Release: Integer; override;
  end;

  TBoldElementFlag = integer;

  // states are in high order bits.
  const
  BoldElementFlag0  = 1 shl 0;
  BoldElementFlag1  = 1 shl 1;
  BoldElementFlag2  = 1 shl 2;
  BoldElementFlag3  = 1 shl 3;
  BoldElementFlag4  = 1 shl 4;
  BoldElementFlag5  = 1 shl 5;
  BoldElementFlag6  = 1 shl 6;
  BoldElementFlag7  = 1 shl 7;
  BoldElementFlag8  = 1 shl 8;
  BoldElementFlag9  = 1 shl 9;
  BoldElementFlag10 = 1 shl 10;
  BoldElementFlag11 = 1 shl 11;
  BoldElementFlag12 = 1 shl 12;
  BoldElementFlag13 = 1 shl 13;
  BoldElementFlag14 = 1 shl 14;
  BoldElementFlag15 = 1 shl 15;
  BoldElementFlag16 = 1 shl 16;
  BoldElementFlag17 = 1 shl 17;
  BoldElementFlag18 = 1 shl 18;
  BoldElementFlag19 = 1 shl 19;
  BoldElementFlag20 = 1 shl 20;
  BoldElementFlag21 = 1 shl 21;
  BoldElementFlag22 = 1 shl 22;
  BoldElementFlag23 = 1 shl 23;

type

  {---TBoldFlaggedObject---}
  TBoldFlaggedObject = class(TBoldMemoryManagedObject)
  private
    fStateAndFlagBank: cardinal;
  protected
    procedure SetInternalState(Mask, shift, value: cardinal);
    function GetInternalState(Mask, shift: cardinal): cardinal;
    procedure SetElementFlag(Flag: TBoldElementFlag; Value: Boolean);
    function GetElementFlag(Flag: TBoldElementFlag): Boolean;
    property StateAndFlagBank: cardinal read fStateAndFlagBank;  // allow reading in subclasses
  end;

{$IFDEF DebugInstanceCounter}
const
  cDefaultInstanceLimit = 1000;
var
  DebugInstanceCounter: boolean = false;

procedure GetInstaceList(AStringList: TStringList; ASort: boolean = true; AInstanceLimit: integer = cDefaultInstanceLimit);
procedure ClearInstanceLog;
{$ENDIF}

implementation

uses
  BoldCoreConsts,
  {$IFDEF DebugInstanceCounter}
  BoldIndexableList,
  BoldHashIndexes,
  {$ENDIF}
  {$IFNDEF BOLD_DISABLEMEMORYMANAGER}
  BoldMemoryManager,
  {$ENDIF}
  Windows;

var
  Finalized: boolean;

{$IFDEF DebugInstanceCounter}
type
  TBoldClassStats = class(TObject)
  strict private
    fClass: TClass;
    fCreatedInstances: int64;
    fDestroyedInstances: int64;
  private
    fChanged: boolean;
  public
    function LiveInstances: int64;
    function MemoryUsage: int64;
    procedure InstanceCreated;
    procedure InstanceDestroyed;
    property BoldClass: TClass read fClass;
    property CreatedInstances: int64 read fCreatedInstances;
    property DestroyedInstances: int64 read fDestroyedInstances;
    constructor Create(AClass: TClass);
  end;

  TBoldClassStatsList = class(TBoldIndexableList)
  private
    function GetClassStats(const index: integer): TBoldClassStats;
    class var IX_Class: integer;
  public
    constructor Create;
    function StatsForClass(AClassType: TClass): TBoldClassStats;
    property ClassStats[const index: integer]: TBoldClassStats read GetClassStats; default;
    property Count;
  end;

  TClassIndex = class(TBoldClassHashIndex)
  protected
    function ItemAsKeyClass(Item: TObject): TClass; override;
  end;

var
  gClassStatsList: TBoldClassStatsList;
  gInternalUpdate: integer = 0;

{$ENDIF}

{-- TBoldInterfacedObject -----------------------------------------------------}

function TBoldInterfacedObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TBoldInterfacedObject.SupportsInterface(const IID: TGUID): Boolean;
var
  Obj: IInterface;
begin
  Result := GetInterface(IID,Obj);
end;

{-- TBoldRefCountedObject -----------------------------------------------------}
{$IFNDEF BOLD_BCB}
procedure TBoldRefCountedObject.AfterConstruction;
begin
  InterlockedDecrement(fRefCount);  // was set by NewInstace
end;
{$ENDIF}

procedure TBoldRefCountedObject.BeforeDestruction;
begin
  If RefCount <> 0 then
    raise EBold.CreateFmt(sRefCountNotNilOnDestruction, [ClassName]);
end;

{$IFNDEF BOLD_BCB}
class function TBoldRefCountedObject.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  TBoldRefCountedObject(Result).fRefCount := 1;
end;
{$ENDIF}

function TBoldRefCountedObject._AddRef: Integer;
begin
  Result := InterlockedIncrement(FRefCount);
end;

function TBoldRefCountedObject._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  if Result = 0 then
    Destroy;
end;

{-- TBoldNonRefCountedObject --------------------------------------------------}

function TBoldNonRefCountedObject._AddRef: Integer;
begin
  Result := -1;
end;

function TBoldNonRefCountedObject._Release: Integer;
begin
  Result := -1;
end;

{ TBoldFlaggedObject }

function TBoldFlaggedObject.GetElementFlag(Flag: TBoldElementFlag): Boolean;
begin
  result := (Flag and fStateAndFlagBank) <> 0;
end;

procedure TBoldFlaggedObject.SetElementFlag(Flag: TBoldElementFlag; Value: Boolean);
begin
  if Value then
    fStateAndFlagBank := fStateAndFlagBank or cardinal(flag)
  else
    fStateAndFlagBank := fStateAndFlagBank and not cardinal(flag);
end;

procedure TBoldFlaggedObject.SetInternalState(Mask, shift, value: cardinal);
begin
  fStateAndFlagBank :=
    (fStateAndFlagBank and not mask) or
   cardinal(Value shl Shift);
end;

// warning, this code is duplicated in TBoldMember since inlining does not work as it should
function TBoldFlaggedObject.GetInternalState(Mask, shift: cardinal): cardinal;
begin
  result := (fStateAndFlagBank and mask) shr shift;
end;

{$IFDEF DebugInstanceCounter}

{ TBoldClassStatsList }

constructor TBoldClassStatsList.Create;
begin
  inherited;
  SetIndexCapacity(1);
  IX_Class := -1;
  SetIndexVariable(IX_Class, AddIndex(TClassIndex.Create));
end;

function TBoldClassStatsList.GetClassStats(
  const index: integer): TBoldClassStats;
begin
  Result := TBoldClassStats(inherited Items[index]);
end;

function TBoldClassStatsList.StatsForClass(AClassType: TClass): TBoldClassStats;
begin
  Result := TBoldClassStats(TClassIndex(Indexes[IX_Class]).FindByClass(AClassType));
  if not Assigned(Result) then
  begin
    Result := TBoldClassStats.Create(AClassType);
    self.Add(result);
  end;
end;

{ TClassIndex }

function TClassIndex.ItemAsKeyClass(Item: TObject): TClass;
begin
  Result := TBoldClassStats(Item).BoldClass;
end;

{ TBoldClassStats }

constructor TBoldClassStats.Create(AClass: TClass);
begin
  fClass := AClass;
end;

procedure TBoldClassStats.InstanceCreated;
begin
  inc(fCreatedInstances);
  fChanged := true;
end;

procedure TBoldClassStats.InstanceDestroyed;
begin
  inc(fDestroyedInstances);
  fChanged := true;
end;

function TBoldClassStats.LiveInstances: int64;
begin
  result := fCreatedInstances - fDestroyedInstances;
end;

function TBoldClassStats.MemoryUsage: int64;
begin
  result := LiveInstances * BoldClass.InstanceSize;
end;

procedure GetInstaceList(AStringList: TStringList; ASort: boolean = true; AInstanceLimit: integer = cDefaultInstanceLimit);
var
  vSl: TStringList;
  i: integer;
  vBoldClassStats: TBoldClassStats;
  vTotalCreated, vTotalDestroyed, vTotalMemoryUsage: int64;
begin
  vTotalCreated := 0;
  vTotalDestroyed := 0;
  vTotalMemoryUsage := 0;
  vSl := TStringList.Create;
  vSl.sorted := ASort;
  try
    for I := 0 to gClassStatsList.Count - 1 do
    begin
      vBoldClassStats := gClassStatsList[i];
      vBoldClassStats.fChanged := false;
      Inc(vTotalCreated, vBoldClassStats.CreatedInstances);
      inc(vTotalDestroyed, vBoldClassStats.DestroyedInstances);
      Inc(vTotalMemoryUsage, vBoldClassStats.MemoryUsage);
      if vBoldClassStats.LiveInstances > AInstanceLimit then
        vSl.Add( Format('%10d - %10d = %10d: %s (%d bytes)', [vBoldClassStats.CreatedInstances, vBoldClassStats.DestroyedInstances, vBoldClassStats.LiveInstances, vBoldClassStats.BoldClass.ClassName, vBoldClassStats.MemoryUsage]));
    end;
    AStringList.Add(StringOfChar('-', 37));
    AStringList.Add('   Created - Destroyed  =      Live : ClassName (Minimal Memory usage in bytes)');
    AStringList.Add(StringOfChar('-', 37));
    AStringList.AddStrings(vSl);
    AStringList.Add(StringOfChar('-', 37));
    AStringList.Add( Format('%10d - %10d = %10d: (%d bytes)', [vTotalCreated, vTotalDestroyed, vTotalCreated - vTotalDestroyed, vTotalMemoryUsage]));
    AStringList.Add(StringOfChar('-', 37));
  finally
    vSl.free;
  end;
end;

procedure ClearInstanceLog;
begin
  gClassStatsList.Clear;
end;

procedure CheckForLiveInstances;
var
  i: integer;
  vBoldClassStats: TBoldClassStats;
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    for I := 0 to gClassStatsList.Count - 1 do
    begin
      vBoldClassStats := gClassStatsList[i];
      if vBoldClassStats.LiveInstances > 0 then
        sl.Add(Format('%d instances of %s', [vBoldClassStats.LiveInstances, vBoldClassStats.BoldClass.ClassName]));
    end;
    if sl.count > 0 then
      raise Exception.Create(sl.text);
  finally
    sl.free;
  end;
end;
{$ENDIF}


{ TBoldMemoryManagedObject }

function TBoldMemoryManagedObject.GetDebugInfo: string;
begin
  if ContextObject <> nil then
    result := ContextObject.ClassName
  else
    result := ClassName;
end;

function TBoldMemoryManagedObject.ContextObject: TObject;
begin
  result := self;
end;

class function TBoldMemoryManagedObject.NewInstance: TObject;
begin
{$IFNDEF BOLD_DISABLEMEMORYMANAGER}
  result := TObject(BoldMemoryManager_.AllocateMemory(InstanceSize));
  InitInstance(result);
{$ELSE}
  result := inherited NewInstance;
{$ENDIF}
{$IFDEF DebugInstanceCounter}
  if DebugInstanceCounter and (gInternalUpdate = 0) then
    gClassStatsList.StatsForClass(self).InstanceCreated;
{$ENDIF}
end;

procedure TBoldMemoryManagedObject.FreeInstance;

  procedure InternalRaise;
  begin
    raise EBold.Create(sObjectDestroyedAfterFinalization);
  end;

begin
  if Finalized then
    InternalRaise;
  {$IFDEF DebugInstanceCounter}
  if DebugInstanceCounter and (gInternalUpdate = 0) then
    gClassStatsList.StatsForClass(ClassType).InstanceDestroyed;
  {$ENDIF}
{$IFNDEF BOLD_DISABLEMEMORYMANAGER}
  CleanUpInstance;
  BoldMemoryManager_.DeAllocateMemory(Pointer(self), InstanceSize);
{$ELSE}
  inherited;
{$ENDIF}
end;

procedure InitDebugMethods;
begin
  exit;
{$IFDEF BOLD_DISABLEMEMORYMANAGER}
  with TBoldMemoryManagedObject(nil) do
  begin
    DebugInfo;
    GetDebugInfo;
  end;
{$ENDIF}
end;

initialization
{$IFDEF DebugInstanceCounter}
  inc(gInternalUpdate);
  gClassStatsList := TBoldClassStatsList.Create;
  dec(gInternalUpdate);
{$ENDIF}
InitDebugMethods;

finalization
{$IFDEF DebugInstanceCounter}
  CheckForLiveInstances;
  inc(gInternalUpdate);
  gClassStatsList.free;
  dec(gInternalUpdate);
{$ENDIF}
  Finalized := true;

end.
