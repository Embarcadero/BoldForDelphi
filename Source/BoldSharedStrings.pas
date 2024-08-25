
{ Global compiler directives }
{$include bold.inc}
unit BoldSharedStrings;

interface

{$IFNDEF BOLD_DISABLESHAREDSTRINGS}
uses
{$IFDEF UseCriticalSection}
  SyncObjs,
{$ENDIF}
  BoldBase,
  BoldIndexableList;
{$ENDIF}

type
  {$IFNDEF BOLD_DISABLESHAREDSTRINGS}
  TBoldSharedStringHolder = class(TBoldMemoryManagedObject)
  private
    fValue: String;
    function GetExternalRefCount: integer;
  public
    constructor Create(const s: String);
    property ExternalRefCount: integer read GetExternalRefCount;
    property Value: String read FValue;
  end;

  TBoldSharedStringCache = class(TBoldUnOrderedIndexableList)
  private
    class var IX_StringHolderValue: integer;
    function GetHolderByValue(const s: String): TBoldSharedStringHolder;
  public
    constructor Create;
    property HolderByValue[const s: String]: TBoldSharedStringHolder read GetHolderByValue;
  end;

  {$IFDEF BOLD_UNICODE}
  TBoldSharedAnsiStringHolder = class(TBoldMemoryManagedObject)
  private
    fValue: AnsiString;
    function GetExternalRefCount: integer;
  public
    constructor Create(const s: AnsiString);
    property ExternalRefCount: integer read GetExternalRefCount;
    property Value: AnsiString read FValue;
  end;

  TBoldSharedAnsiStringCache = class(TBoldUnOrderedIndexableList)
  private
    function GetHolderByValue(const s: AnsiString): TBoldSharedAnsiStringHolder;
  public
    constructor Create;
    property HolderByValue[const s: AnsiString]: TBoldSharedAnsiStringHolder read GetHolderByValue;
  end;
  {$ENDIF}
  {$ENDIF}

  TBoldSharedStringManager = class
  private
    {$IFDEF UseCriticalSection}
    fCriticalSection: TCriticalSection;
    {$ENDIF}
    {$IFNDEF BOLD_DISABLESHAREDSTRINGS}
    fAddsRemainingToGarbageCollect: integer;
    fStringCache: TBoldSharedStringCache;
    {$IFDEF BOLD_UNICODE}
    fAnsiAddsRemainingToGarbageCollect: integer;
    fAnsiStringCache: TBoldSharedAnsiStringCache;
    {$ENDIF}
    fCachedHits: integer;
    procedure DoGarbageCollect(KeepStringsWithOneReference: Boolean); overload;
    {$IFDEF BOLD_UNICODE}
    procedure DoAnsiGarbageCollect(KeepStringsWithOneReference: Boolean); overload;
    {$ENDIF}
    {$ENDIF}
    function GetSavedMemory: integer;
    function GetInfoString: String;
  public
    constructor Create;
    destructor Destroy; override;
    procedure GarbageCollect;
    function GetSharedString(const s: String): String;
    {$IFDEF BOLD_UNICODE}
    function GetSharedAnsiString(const s: AnsiString): AnsiString;
    {$ENDIF}
    property SavedMemory: integer read GetSavedMemory;
    property InfoString: String read GetInfoString;
  end;

function BoldSharedStringManager: TBoldSharedStringManager;

{$IFNDEF BOLD_UNICODE}
function StringRefCount(const s: String): integer;
{$ENDIF}

implementation

uses
  SysUtils
  {$IFNDEF BOLD_DISABLESHAREDSTRINGS}
  ,
  BoldDefs,
  BoldIndex,
  BoldHashIndexes
  {$ENDIF};

var
  G_BoldSharedStringManager: TBoldSharedStringManager = nil;

{$IFNDEF BOLD_DISABLESHAREDSTRINGS}
const
  MINIMUM_ADDS = 100;

type
  TBoldStringHolderValueIndex = class(TBoldCaseSensitiveStringHashIndex)
  protected
    function ItemAsKeyString(Item: TObject): string; override;
  end;

  {$IFDEF BOLD_UNICODE}
  TBoldAnsiStringHolderValueIndex = class(TBoldCaseSensitiveStringHashIndex)
  protected
    function ItemAsKeyString(Item: TObject): string; override;
  end;
  {$ENDIF}
{$ENDIF}

function BoldSharedStringManager: TBoldSharedStringManager;
begin
  if not assigned(G_BoldSharedStringManager) then
    G_BoldSharedStringManager := TBoldSharedStringManager.Create;
  result := G_BoldSharedStringManager;
end;

{$IFNDEF BOLD_UNICODE}
function StringRefCount(const s: String): integer;
begin
  result := Integer(Pointer(integer(Addr(s)^)-8)^);
end;
{$ENDIF}

{$IFNDEF BOLD_DISABLESHAREDSTRINGS}
{ TBoldSharedStringCache }

constructor TBoldSharedStringCache.Create;
begin
  inherited;
  SetIndexVariable(IX_StringHolderValue, AddIndex(TBoldStringHolderValueIndex.Create));
end;

function TBoldSharedStringCache.GetHolderByValue(
  const s: String): TBoldSharedStringHolder;
begin
  Result := TBoldSharedStringHolder(TBoldCaseSensitiveStringHashIndex(Indexes[IX_StringHolderValue]).FindByString(s));
end;

{$IFDEF BOLD_UNICODE}
constructor TBoldSharedAnsiStringCache.Create;
begin
  inherited;
  SetIndexVariable(TBoldSharedStringCache.IX_StringHolderValue, AddIndex(TBoldAnsiStringHolderValueIndex.Create));
end;

function TBoldSharedAnsiStringCache.GetHolderByValue(
  const s: AnsiString): TBoldSharedAnsiStringHolder;
begin
  Result := TBoldSharedAnsiStringHolder(TBoldCaseSensitiveStringHashIndex(Indexes[TBoldSharedStringCache.IX_StringHolderValue]).FindByString(string(s)));
end;
{$ENDIF}

{ TBoldStringHolderValueIndex }

function TBoldStringHolderValueIndex.ItemAsKeyString(Item: TObject): string;
begin
  Result := TBoldSharedStringHolder(item).Value;
end;

{$IFDEF BOLD_UNICODE}
function TBoldAnsiStringHolderValueIndex.ItemAsKeyString(Item: TObject): string;
begin
  Result := string(TBoldSharedAnsiStringHolder(item).Value);
end;
{$ENDIF}

{ TBoldSharedStringHolder }

constructor TBoldSharedStringHolder.Create(const s: String);
begin
  inherited Create;
  fValue := s;
end;

function TBoldSharedStringHolder.GetExternalRefCount: integer;
begin
  result := StringRefCount(fValue)-1;
end;

{$IFDEF BOLD_UNICODE}
constructor TBoldSharedAnsiStringHolder.Create(const s: AnsiString);
begin
  inherited Create;
  fValue := s;
end;

function TBoldSharedAnsiStringHolder.GetExternalRefCount: integer;
begin
  result := StringRefCount(fValue)-1;
end;
{$ENDIF}
{$ENDIF}

  { TBoldSharedStringManager }

constructor TBoldSharedStringManager.Create;
begin
  inherited;
  {$IFDEF UseCriticalSection}
  fCriticalSection := TCriticalSection.Create;
  {$ENDIF}
  {$IFNDEF BOLD_DISABLESHAREDSTRINGS}
  fStringCache := TBoldSharedStringCache.Create;
  {$IFDEF BOLD_UNICODE}
  fAnsiStringCache := TBoldSharedAnsiStringCache.Create;
  {$ENDIF}
  fAddsRemainingToGarbageCollect := MINIMUM_ADDS;
  {$ENDIF}
end;

destructor TBoldSharedStringManager.Destroy;
begin
  {$IFNDEF BOLD_DISABLESHAREDSTRINGS}
  FreeAndNil(fStringCache);
  {$IFDEF BOLD_UNICODE}
  FreeAndNil(fAnsiStringCache);
  {$ENDIF}
  {$ENDIF}
  {$IFDEF UseCriticalSection}
  FreeAndNil(fCriticalSection);
  {$ENDIF}
  inherited;
end;

{$IFNDEF BOLD_DISABLESHAREDSTRINGS}
procedure TBoldSharedStringManager.DoGarbageCollect(
    KeepStringsWithOneReference: Boolean);
var
  Traverser: TBoldIndexableListTraverser;
  Holder: TBoldSharedStringHolder;
  MinimumReferencesToKeep: integer;
begin
  if KeepStringsWithOneReference then
    MinimumReferencesToKeep := 1
  else
    MinimumReferencesToKeep := 2;

  Traverser := fStringCache.CreateTraverser;
  while Traverser.MoveNext do
  begin
    Holder := TBoldSharedStringHolder(Traverser.Item);
    if Holder.ExternalRefCount < MinimumReferencesToKeep then
      fStringCache.remove(Holder);
  end;
  Traverser.Free;

  fAddsRemainingToGarbageCollect := fStringCache.Count;
  // Do not let fAddsRemainingToGarbageCollect get to small,
  // because then GarbageCollect would be called very often and
  // there is only a small chance, that Strings were added to the holder.
  if fAddsRemainingToGarbageCollect < MINIMUM_ADDS then begin
    fAddsRemainingToGarbageCollect := MINIMUM_ADDS;
  end;
end;

{$IFDEF BOLD_UNICODE}
procedure TBoldSharedStringManager.DoAnsiGarbageCollect(
    KeepStringsWithOneReference: Boolean);
var
  Traverser: TBoldIndexableListTraverser;
  Holder: TBoldSharedAnsiStringHolder;
  MinimumReferencesToKeep: integer;
begin
  if KeepStringsWithOneReference then
    MinimumReferencesToKeep := 1
  else
    MinimumReferencesToKeep := 2;

  Traverser := fAnsiStringCache.CreateTraverser;
  while Traverser.MoveNext do
  begin
    Holder := TBoldSharedAnsiStringHolder(Traverser.Item);
    if Holder.ExternalRefCount < MinimumReferencesToKeep then
      fAnsiStringCache.remove(Holder);
  end;
  Traverser.Free;

  fAnsiAddsRemainingToGarbageCollect := fAnsiStringCache.Count;
  // Do not let fAnsiAddsRemainingToGarbageCollect get to small,
  // because then GarbageCollect would be called very often and
  // there is only a small chance, that Strings were added to the holder.
  if fAnsiAddsRemainingToGarbageCollect < MINIMUM_ADDS then begin
    fAnsiAddsRemainingToGarbageCollect := MINIMUM_ADDS;
  end;
end;
{$ENDIF}
{$ENDIF}

procedure TBoldSharedStringManager.GarbageCollect;
begin
  {$IFNDEF BOLD_DISABLESHAREDSTRINGS}
  {$IFDEF UseCriticalSection}
  fCriticalSection.Enter;
  try
  {$ENDIF}
    DoGarbageCollect(False);
  {$IFDEF BOLD_UNICODE}
    DoAnsiGarbageCollect(False);
  {$ENDIF}
  {$IFDEF UseCriticalSection}
  finally
    fCriticalSection.Leave;
  end;
  {$ENDIF}
  {$ENDIF}
end;

function TBoldSharedStringManager.GetInfoString: String;
{$IFNDEF BOLD_DISABLESHAREDSTRINGS}
var
  iSharedStrings: Integer;
{$ENDIF}
begin
{$IFDEF BOLD_DISABLESHAREDSTRINGS}
  Result := 'SharedStringManager disabled'; {do not localize }
{$ELSE}
  {$IFDEF UseCriticalSection}
  fCriticalSection.Enter;
  try
  {$ENDIF}
    iSharedStrings := FStringCache.Count;
  {$IFDEF BOLD_UNICODE}
    Inc(iSharedStrings, fAnsiStringCache.Count);
  {$ENDIF}
    Result :=
      Format('Number of shared strings: %d', [iSharedStrings]) + BOLDCRLF + {do not localize }
      Format('Saved memory %d', [SavedMemory]); {do not localize }
{$IFDEF UseCriticalSection}
  finally
    fCriticalSection.Leave;
  end;
{$ENDIF}
{$ENDIF}
end;

function TBoldSharedStringManager.GetSavedMemory: integer;
{$IFNDEF BOLD_DISABLESHAREDSTRINGS}
var
  Traverser: TBoldIndexableListTraverser;
  Holder: TBoldSharedStringHolder;
  {$IFDEF BOLD_UNICODE}
  AnsiHolder: TBoldSharedAnsiStringHolder;
  {$ENDIF}
{$ENDIF}
begin
  Result := 0;
{$IFNDEF BOLD_DISABLESHAREDSTRINGS}
  Traverser := fStringCache.CreateTraverser;
  while Traverser.MoveNext do
  begin
    Holder := TBoldSharedStringHolder(Traverser.Item);
    Result := Result + (Holder.ExternalRefCount-1) * Length(Holder.Value) * SizeOf(Char);
  end;
  Traverser.Free;
  {$IFDEF BOLD_UNICODE}
  Traverser := fAnsiStringCache.CreateTraverser;
  while Traverser.MoveNext do
  begin
    AnsiHolder := TBoldSharedAnsiStringHolder(Traverser.Item);
    Result := Result + (AnsiHolder.ExternalRefCount-1) * Length(AnsiHolder.Value);
  end;
  Traverser.Free;
  {$ENDIF}
{$ENDIF}
end;

function TBoldSharedStringManager.GetSharedString(const s: String): String;
{$IFDEF BOLD_DISABLESHAREDSTRINGS}
begin
  Result := s;
{$ELSE}
var
  Holder: TBoldSharedStringHolder;
begin
  if s = '' then begin
    Result := s;
  end else begin
  {$IFDEF UseCriticalSection}
    fCriticalSection.Enter;
    try
  {$ENDIF}
    Holder := fStringCache.HolderByValue[s];
    if Assigned(Holder) then begin
      Inc(fCachedHits);
    end else begin
      Holder := TBoldSharedStringHolder.Create(s);
      fStringCache.Add(Holder);
    end;
    Result := Holder.Value;
  end;
  Dec(fAddsRemainingToGarbageCollect);
  if fAddsRemainingToGarbageCollect <= 0 then
    DoGarbageCollect(true);
{$IFDEF UseCriticalSection}
  finally
    fCriticalSection.Leave;
  end;
{$ENDIF}
{$ENDIF}
end;

{$IFDEF BOLD_UNICODE}
function TBoldSharedStringManager.GetSharedAnsiString(const s: AnsiString):
    AnsiString;
{$IFDEF BOLD_DISABLESHAREDSTRINGS}
begin
  Result := s;
{$ELSE}
var
  Holder: TBoldSharedAnsiStringHolder;
begin
  if s = '' then begin
    Result := s;
  end else begin
    Holder := fAnsiStringCache.HolderByValue[s];
    if Assigned(Holder) then begin
      Inc(fCachedHits);
    end else begin
      Holder := TBoldSharedAnsiStringHolder.Create(s);
      fAnsiStringCache.Add(Holder);
    end;
    Result := Holder.Value;
  end;
  Dec(fAnsiAddsRemainingToGarbageCollect);
  if fAnsiAddsRemainingToGarbageCollect <= 0 then
    DoAnsiGarbageCollect(true);
{$ENDIF}
end;
{$ENDIF}

initialization // empty
{$IFNDEF BOLD_DISABLESHAREDSTRINGS}
  TBoldSharedStringCache.IX_StringHolderValue := -1;
{$ENDIF}

finalization
  FreeAndNil(G_BoldSharedStringManager);
end.

