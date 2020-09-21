unit BoldBase;

interface

uses
  BoldDefs;

type
  { forward declarations }
  TBoldInterfacedObject = class;
  TBoldRefCountedObject = class;
  TBoldNonRefCountedObject = class;

  {-- TBoldMemoryManagedObject --}
  TBoldMemoryManagedObject = class(TObject)
  public
    class function NewInstance: TObject; override;
    procedure FreeInstance; override;
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
    {$IFNDEF BOLD_BCB}  { TODO : Check if needed in BCB6 }
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
  end;

implementation

uses
  BoldCommonConst,
  {$IFNDEF BOLD_DISABLEMEMORYMANAGER}
  BoldMemoryManager,
  {$ENDIF}
  Windows;

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

function TBoldFlaggedObject.GetInternalState(Mask, shift: cardinal): cardinal;
begin
  result := (fStateAndFlagBank and mask) shr shift;
end;

{-- TBoldMemoryManagedObject -----------------------------------------------------}

procedure TBoldMemoryManagedObject.FreeInstance;
begin
  {$IFNDEF BOLD_DISABLEMEMORYMANAGER}
  CleanUpInstance;
  BoldMemoryManager_.DeAllocateMemory(Pointer(self), InstanceSize);
  {$ELSE}
  inherited;
  {$ENDIF}
end;

class function TBoldMemoryManagedObject.NewInstance: TObject;
begin
  {$IFNDEF BOLD_DISABLEMEMORYMANAGER}
  result := TObject(BoldMemoryManager_.AllocateMemory(InstanceSize));
  InitInstance(result);
  {$ELSE}
  result := inherited NewInstance;
  {$ENDIF}
end;

end.

