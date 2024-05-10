/////////////////////////////////////////////////////////

{ Global compiler directives }
{$include bold.inc}
unit BoldMemoryManager;

interface

(* -----------------------------------------------------------------

To use the allocation mechanisms in this unit,
subclass from TBoldMemoryManagedObject in BoldPase.pas or
override NewInstance and FreeInstance in your class:

uses
  BoldMemoryManager;

type
  class T<xxx> = class(yyy)
  protected
    procedure FreeInstance; override;
    class function NewInstance: TObject; override;
  end;

procedure T<xxx>.FreeInstance;
begin
  CleanUpInstance; // cleans up all strings...
  BoldMemoryManager_.DeAllocateMemory(Pointer(self), InstanceSize);
end;

class function T<xxx>.NewInstance: TObject;
begin
  result := TObject(BoldMemoryManager_.AllocateMemory(InstanceSize));
  InitInstance(result);
end;

---------------------------------------------------------------------*)

{.$DEFINE DEBUG_BOLDMEMORYMANAGER}
// Define this conditional, if your application maybe uses only one thread.
// Normaly every application is multithreaded and the check is waste.
{.$DEFINE CHECK_ISMULTITHREAD}

uses
  {$IFDEF DEBUG_BOLDMEMORYMANAGER}
//  Dialogs,
  {$ENDIF}
  BoldDefs,
  SyncObjs,
  Classes;

const
  BoldMemoryManagerPageSize = 2048;
  BoldMemoryManagerLSBZero = 2;
{//}  BoldMemoryManagerLSBFactor =  (1 shl BoldMemoryManagerLSBZero);
  BoldMemoryManagerMaxPage = 16; // memoryblocks with 4*MaxPage will be handled, must be power of 2
{//}  BoldMemoryManagerMask = ((1 shl BoldMemoryManagerLSBZero)-1) or (not Cardinal((BoldMemoryManagerMaxPage shl BoldMemoryManagerLSBZero)-1));  // mask for quick test of allowed size

type
  { forward declarations }
  TBoldMemoryManager = class;

  TBoldMemoryManagerPageCounters = array[BoldMemoryManagerLSBFactor..BoldMemoryManagerMaxPage*BoldMemoryManagerLSBFactor] of integer;  // only 1 of 4 used, speeds up indexing
  TBoldMemoryManagerPagePointers = array[BoldMemoryManagerLSBFactor..BoldMemoryManagerMaxPage*BoldMemoryManagerLSBFactor] of Pointer;  // due to stupid compiler

  { TBoldMemoryManager }
  TBoldMemoryManager = class
  private
    {$IFNDEF BOLD_DISABLEMEMORYMANAGER}
    FreePointers: TBoldMemoryManagerPagePointers;
    {$ENDIF}
    fPageSizes: TList;
    fPages: TList;
    CurrentInUse: TBoldMemoryManagerPageCounters;
    TotalAllocated: TBoldMemoryManagerPageCounters;
    fBytesInBigBlocks: integer;
    fBigBlockCount: integer;
    fCriticalSection: TCriticalSection;
    {$IFNDEF BOLD_DISABLEMEMORYMANAGER}
    procedure AllocateNewPage(Size: integer);
    procedure GarbageCollectPage(PageNum: integer; Page: Pointer; PageSize: integer);
    procedure LinkIn(var Chain: Pointer; Item: POinter);
    function LinkOut(var Chain: Pointer): Pointer;
    {$ENDIF}
    function GetAllocated: integer;
    function GetMemoryInfo: string;
    function GetOverhead: integer;
    function GetInUse: integer;
    function GetAllocatedPages: integer;
  protected
    property Pages: TList read fPages;
    property PageSizes: TList read fPageSizes;
  public
    constructor Create;
    destructor Destroy; override;
    procedure GarbageCollect;
    function AllocateMemory(Size: integer): Pointer;
    function AllocateMemoryZeroFill(Size: integer): Pointer;
    procedure DeAllocateMemory(Ptr: Pointer; Size: integer);
    function ReallocateMemoryZeroFill(Ptr: Pointer; OldSize: integer; NewSize: integer): Pointer;
    property Overhead: integer read GetOverhead;
    property Allocated: integer read GetAllocated;
    property InUse: integer read GetInUse;
    property AllocatedPages: integer read GetAllocatedPages;
    property MemoryInfo: String read GetMemoryInfo;
    property BytesInBigBlocks: integer read fBytesInBigBlocks write fBytesInBigBlocks;
    property BigBlockCount: integer read fBigBlockCount write fBigBlockCount;
  end;

function BoldMemoryManager_: TBoldMemoryManager;

implementation

uses
  SysUtils,
  Types,

  BoldCoreConsts;

{$IFDEF DEBUG_BOLDMEMORYMANAGER}
const
  LOGFILE: string = 'c:\temp\BoldMemoryManager.Log';

var
  strl: TStringList = nil;
{$ENDIF}


procedure Log(const Str: String);
begin
  {$IFDEF DEBUG_BOLDMEMORYMANAGER}
  if not assigned(Strl) then
  begin
    strl := TStringList.Create;
    if fileexists(LOGFILE) then
      Strl.LoadFromFile(LOGFILE);
  end;
  strl.Add(DateTimetoStr(now) + ': ' + Str);
  Strl.SavetoFile(LOGFILE);
  {$ENDIF}
end;

const
  PageSize = BoldMemoryManagerPageSize * BoldMemoryManagerMaxPage;

type
  TPage = array[0..PageSize] of Pointer;

var
  G_BoldMemoryManager: TBoldMemoryManager = nil;
  Finalized: Boolean = False;

procedure DestroyIfEmpty;
begin
  if Assigned(G_BoldMemoryManager) and (G_BoldMemoryManager.InUse = 0) then
    FreeAndNil(G_BoldMemoryManager);
end;

function BoldMemoryManager_: TBoldMemoryManager;
begin
  if G_BoldMemoryManager = nil then begin
    if Finalized then
      raise EBold.Create(sMemoryManagerCalledInFinalization);
    G_BoldMemoryManager :=  TBoldMemoryManager.Create;
  end;
  Result := G_BoldMemoryManager;
end;


{$IFNDEF BOLD_DISABLEMEMORYMANAGER}
procedure TBoldMemoryManager.LinkIn(var Chain: Pointer; Item: POinter);
begin
  // warning, duplicated in DeallocateMemory for effficency
  Pointer(Item^) := Chain;
  Chain := Item;
end;

function TBoldMemoryManager.LinkOut(var Chain: Pointer): Pointer;
begin
  // warning, duplicated in AllocateMemory for effficency
  result := Chain;
  Chain := Pointer(result^);
end;

procedure TBoldMemoryManager.AllocateNewPage(Size: integer);
var
  i: integer;
  Page: ^TPage;
  SizeInWords: integer;
begin
  Page := System.GetMemory(Size * BoldMemoryManagerPageSize);
  Pages.Add(Page);
  PageSizes.Add(Pointer(Size * BoldMemoryManagerPageSize));
  SizeInWords := Size shr BoldMemoryManagerLSBZero;
  {$IFDEF DEBUG_BOLDMEMORYMANAGER}
  FillChar(Page^, Size * BoldMemoryManagerPageSize, 0);
  {$ENDIF}
  for i := 0 to BoldMemoryManagerPageSize-2 do
  begin
    Page^[i*sizeInWords] := addr(Page^[(i + 1) * sizeInWords]);
  end;
  Page^[(BoldMemoryManagerPageSize-1) * sizeInWords] := nil;

  FreePointers[Size] := Page;

  Inc(TotalAllocated[Size], BoldMemoryManagerPageSize);
end;

procedure TBoldMemoryManager.GarbageCollectPage(PageNum: integer; Page: Pointer; PageSize: integer);
var
  OldFreeList, NewFreeList: Pointer;
  FreeInPage: integer;
  function PointerInPage(Ptr, Page: Pointer; PageSize: integer): Boolean;
  begin
    result := (integer(Ptr) >= Integer(Page)) and
      (integer(Ptr) < integer(Page) + PageSize);
  end;
begin
  FreeInPage := 0;
  NewFreeList := nil;
  OldFreeList := nil;
  while assigned(FreePointers[PageNum]) do
  begin
    if PointerInPage(FreePointers[PageNum], Page, PageSize) then
    begin
      LinkIn(NewFreeList, LinkOut(FreePointers[PageNum]));
      Inc(FreeInPage);
    end
    else
      LinkIn(OldFreeList, LinkOut(FreePointers[PageNum]));
  end;
  FreePointers[PageNum] := OldFreeList;

  if FreeInPage = BoldMemoryManagerPageSize then
  begin
    PageSizes.Delete(Pages.Remove(Page));
    system.FreeMemory(Page);
    Dec(TotalAllocated[PageNum], BoldMemoryManagerPageSize);
  end
  else
  begin
    while assigned(NewFreelist) do
      LinkIn(FreePointers[PageNum], LinkOut(NewFreeList));
  end;
end;

{$ENDIF}

function TBoldMemoryManager.GetOverhead: integer;
var
  i: integer;
begin
  {$IFDEF CHECK_ISMULTITHREAD}
  if (IsMultiThread) then
  {$ENDIF}
    fCriticalSection.Acquire;
  result := 0;
  for i := Low(TotalAllocated) to High(TotalAllocated) do
    result := result + (TotalAllocated[i] - CurrentInUse[i]) * i;
  {$IFDEF CHECK_ISMULTITHREAD}
  if (IsMultiThread) then
  {$ENDIF}
    fCriticalSection.Leave;
end;

function TBoldMemoryManager.GetInUse: integer;
var
  i: integer;
begin
  {$IFDEF CHECK_ISMULTITHREAD}
  if (IsMultiThread) then
  {$ENDIF}
    fCriticalSection.Acquire;
  result := 0;
  for i := Low(CurrentInUse) to High(CurrentInUse) do
    result := result + CurrentInUse[i] * i;
  {$IFDEF CHECK_ISMULTITHREAD}
  if (IsMultiThread) then
  {$ENDIF}
    fCriticalSection.Leave;
end;

type
  pInteger = ^Integer;


function TBoldMemoryManager.AllocateMemory(Size: integer): Pointer;
begin
  {$IFDEF BOLD_DISABLEMEMORYMANAGER}
   GetMem(result, Size);
   INC(fBigBlockCount);
  {$ELSE}
  // Note, ordering and code repetition intentional, to give best operaion pairing.
  {$IFDEF CHECK_ISMULTITHREAD}
  if (IsMultiThread) then
  {$ENDIF}
    fCriticalSection.Acquire;
  if ((Size and BoldMemoryManagerMask) = 0) then
  begin
    if size = 0 then
      result := nil
    else
    begin
      result := FreePointers[Size];
      if assigned(result) then
      begin
        Inc(CurrentInUse[Size]);
        FreePointers[Size] := Pointer(result^);
      end
      else
      begin
        AllocateNewPage(Size);
        result := FreePointers[Size];
        Inc(CurrentInUse[Size]);
        FreePointers[Size] :=  Pointer(result^);
      end;
    end;
  end
  else
  begin
    {$IFDEF DEBUG_BOLDMEMORYMANAGER}
    BytesInBigBlocks := BytesInBigBlocks + size;
    INC(fBigBlockCount);
    {$ENDIF}
    getMem(result, Size);
  end;
  {$IFDEF CHECK_ISMULTITHREAD}
  if (IsMultiThread) then
  {$ENDIF}
    fCriticalSection.Leave;
  {$ENDIF}
end;

procedure TBoldMemoryManager.DeAllocateMemory(Ptr: Pointer; Size: integer);
begin
  {$IFDEF BOLD_DISABLEMEMORYMANAGER}
  FreeMem(Ptr, Size);
  DEC(fBigBlockCount);
  if fBigBlockCount = 0 then
    FreeAndNil(G_BoldMemoryManager);
  {$ELSE}
  {$IFDEF CHECK_ISMULTITHREAD}
  if (IsMultiThread) then
  {$ENDIF}
    fCriticalSection.Acquire;
  if ((Size and BoldMemoryManagerMask) = 0) then
  begin
    if assigned(Ptr) then
    begin
      Pointer(Ptr^) := FreePointers[Size];
      FreePointers[Size] := Ptr;
      Dec(CurrentInUse[Size]);
      // we can't destroy in a multithreaded envirorment, it will cause an AV when leaving the critical section
      if (CurrentInUse[Size] = 0) and (Finalized) and not IsMultiThread then
        DestroyIfEmpty;
    end;
  end
  else
  begin
    {$IFDEF DEBUG_BOLDMEMORYMANAGER}
    BytesInBigBlocks := BytesInBigBlocks - size;
    DEC(fBigBlockCount);
    {$ENDIF}
    Freemem(Ptr, size);
  end;
  {$IFDEF CHECK_ISMULTITHREAD}
  if (IsMultiThread) then
  {$ENDIF}
    fCriticalSection.Leave
  {$ENDIF}
end;

procedure TBoldMemoryManager.GarbageCollect;
{$IFNDEF BOLD_DISABLEMEMORYMANAGER}
var
  PageNum, i: integer;
{$ENDIF}
begin
  {$IFNDEF BOLD_DISABLEMEMORYMANAGER}
  {$IFDEF CHECK_ISMULTITHREAD}
  if (IsMultiThread) then
  {$ENDIF}
    fCriticalSection.Acquire;
  for PageNum := Low(TotalAllocated) to High(TotalAllocated) do
  begin
    if (TotalAllocated[PageNum] - CurrentInUse[PageNum]) >= BoldMemoryManagerPageSize then
    begin
      for i := Pages.count-1 downto 0 do
      begin
        if Integer(PageSizes[i]) = PageNum*BoldMemoryManagerLSBFactor*BoldMemoryManagerPageSize then
        begin
          GarbageCollectPAge(PageNum, Pages[i], Integer(PageSizes[i]));
        end;
      end;
    end;
  end;
  {$IFDEF CHECK_ISMULTITHREAD}
  if (IsMultiThread) then
  {$ENDIF}
    fCriticalSection.Leave;
  {$ENDIF}
end;


constructor TBoldMemoryManager.Create;
begin
  inherited;
  fCriticalSection := TCriticalSection.Create;
  fPages := TList.Create;
  fPageSizes := TList.create;
end;

destructor TBoldMemoryManager.Destroy;
begin
  {$IFDEF CHECK_ISMULTITHREAD}
  if (IsMultiThread) then
  {$ENDIF}
    fCriticalSection.Acquire;
  while pages.Count > 0 do
  begin
    system.FreeMemory(Pages[0]);
    Pages.Delete(0);
  end;
  FreeAndNil(fPages);
  FreeAndNil(fPageSizes);
  {$IFDEF CHECK_ISMULTITHREAD}
  if (IsMultiThread) then
  {$ENDIF}
    fCriticalSection.Leave;
  FreeAndNil(fCriticalSection);
  Log(sMemoryManagerDestroyed);
  inherited;
end;

function TBoldMemoryManager.GetMemoryInfo: string;
{$IFNDEF BOLD_DISABLEMEMORYMANAGER}
var
  PageNum: Integer;
  AllocMemSize, AllocMemCount: Integer;
  {$IFDEF BOLD_DELPHI10_OR_LATER}
  MemMgrState: TMemoryManagerState;
  I: Integer;
  {$ENDIF}
{$ENDIF}
begin
  Result := '';
  {$IFDEF BOLD_DISABLEMEMORYMANAGER}
  Result := sMemMgrDisabled + BOLDCRLF +
            sMemMgrDisabledReason;
  {$ELSE}
  {$IFDEF CHECK_ISMULTITHREAD}
  if (IsMultiThread) then
  {$ENDIF}
    fCriticalSection.Acquire;
  try
    for PageNum := Low(TotalAllocated) to High(TotalAllocated) do begin
      if TotalAllocated[PageNum] <> 0 then begin
        Result := Result + Format(sMemMgrSize, [PageNum,   CurrentInUse[PageNum],
            (CurrentInUse[PageNum]*100.0*PageNum)/InUse,
            TotalAllocated[PageNum] - CurrentInUse[PageNum]]) + BOLDCRLF;
      end;
    end;
    Result := Result + Format('%-15s: %7dkb  (%10d bytes)', [sMemMgrAllocated, allocated DIV 1024, Allocated]) + BOLDCRLF; // do not localize
    Result := Result + Format('%-15s: %7dkb  (%10d bytes)', [sMemMgrInUse, InUse DIV 1024, InUse]) + BOLDCRLF; // do not localize
    Result := Result + Format('%-15s: %7dkb  (%10d bytes)', [sMemMgrOverHead, overhead DIV 1024, overhead]) + BOLDCRLF; // do not localize
    if BigBlockCount > 0 then begin
      {$IFDEF BOLD_DELPHI10_OR_LATER}
      GetMemoryManagerState(MemMgrState);
      AllocMemSize := MemMgrState.TotalAllocatedMediumBlockSize +
                      MemMgrState.TotalAllocatedLargeBlockSize;
      AllocMemCount := MemMgrState.AllocatedMediumBlockCount +
                       MemMgrState.AllocatedLargeBlockCount;
      for I := 0 to High(MemMgrState.SmallBlockTypeStates) do begin
        Inc(AllocMemSize, MemMgrState.SmallBlockTypeStates[I].InternalBlockSize +
                          MemMgrState.SmallBlockTypeStates[I].UseableBlockSize);
        Inc(AllocMemCount, MemMgrState.SmallBlockTypeStates[I].AllocatedBlockCount);
      end;
      {$ELSE}
      AllocMemSize := System.AllocMemSize;
      AllocMemCount := System.AllocMemCount;
      {$ENDIF}
      Result := Result + Format('%-15s: %7dkb  (%10d bytes)', [sMemMgrTotalBigBlocks, bytesinbigblocks DIV 1024, bytesinbigblocks]) + BOLDCRLF; // do not localize
      Result := Result + Format('%-15s: %7d    (%10d bytes)', [sMemMgrBigBlockCount, BigBlockCount, bytesinbigblocks DIV BigBlockCount]) + BOLDCRLF; // do not localize
      Result := Result + Format('%-15s: %7dkb  (%10d bytes)', [sMemMgrNonBold, (AllocMemSize - allocated-bytesinbigblocks)DIV 1024, (AllocMemSize - allocated-bytesinbigblocks)]) + BOLDCRLF; // do not localize
      Result := Result + Format('%-15s: %7d    (%10d bytes)', [sMemMgrNonBoldCount, AllocMemCount-BigBlockCount-AllocatedPages, (AllocMemSize - allocated - bytesinbigblocks) DIV (AllocMemCount-BigBlockCount-AllocatedPages)]) + BOLDCRLF; // do not localize
    end;
  finally
    {$IFDEF CHECK_ISMULTITHREAD}
    if (IsMultiThread) then
    {$ENDIF}
      fCriticalSection.Leave;
  end;
  {$ENDIF}
end;

function TBoldMemoryManager.GetAllocated: integer;
begin
  result := InUse + Overhead;
end;

function TBoldMemoryManager.GetAllocatedPages: integer;
begin
  {$IFDEF CHECK_ISMULTITHREAD}
  if (IsMultiThread) then
  {$ENDIF}
    fCriticalSection.Acquire;
  Result :=Pages.count;
  {$IFDEF CHECK_ISMULTITHREAD}
  if (IsMultiThread) then
  {$ENDIF}
    fCriticalSection.Leave;
end;

function TBoldMemoryManager.ReallocateMemoryZeroFill(Ptr: Pointer; OldSize: integer; NewSize: integer): Pointer;
begin
  if OldSize = NewSize then
  begin
    Result := Ptr;
    Exit;
  end;
  Result := AllocateMemory(NewSize);
  if NewSize > OldSize then
  begin
    Move(Ptr^, Result^, OldSize);
    FillChar((PAnsiChar(Result)+Oldsize)^, NewSize-OldSize, 0);
  end
  else // NewSize < Oldize
    Move(Ptr^ , Result^, NewSize);
  DeAllocateMemory(Ptr, OldSize);
end;

function TBoldMemoryManager.AllocateMemoryZeroFill(Size: integer): Pointer;
begin
  Result := AllocateMemory(Size);
  FillChar(Result^,Size, 0);
end;

initialization

finalization
  Finalized := True;
  DestroyIfEmpty;
{$IFDEF DEBUG_BOLDMEMORYMANAGER}
  FreeAndNil(strl);
{$ENDIF}
end.
