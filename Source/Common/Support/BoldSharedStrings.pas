unit BoldSharedStrings;

interface

uses
  BoldBase,
  BoldIndexableList;

type
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
    function GetHolderByValue(const s: String): TBoldSharedStringHolder;
  public
    constructor Create;
    property HolderByValue[const s: String]: TBoldSharedStringHolder read GetHolderByValue;
  end;

  TBoldSharedStringManager = class
  private
    fStringCache: TBoldSharedStringCache;
    {$IFNDEF BOLD_DISABLESHAREDSTRINGS}
    fCachedHits: integer;
    {$ENDIF}
    fAddsRemainingToGarbageCollect: integer;
    function GetSavedMemory: integer;
    function GetInfoString: String;
  public
    constructor Create;
    destructor Destroy; override;
    function GetSharedString(const s: String): String;
    procedure GarbageCollect(KeepStringsWithOneReference: Boolean);
    property SavedMemory: integer read GetSavedMemory;
    property InfoString: String read GetInfoString;
  end;

function BoldSharedStringManager: TBoldSharedStringManager;

function StringRefCount(const s: String): integer;

implementation

uses
  SysUtils,
  BoldDefs,
  BoldHashIndexes;

var
  IX_StringHolderValue: integer = -1;
  G_BoldSharedStringManager: TBoldSharedStringManager = nil;

type
  TBoldStringHolderValueIndex = class(TBoldCaseSensitiveStringHashIndex)
  protected
    function ItemAsKeyString(Item: TObject): string; override;
  end;

function BoldSharedStringManager: TBoldSharedStringManager;
begin
  if not assigned(G_BoldSharedStringManager) then
    G_BoldSharedStringManager := TBoldSharedStringManager.Create;
  result := G_BoldSharedStringManager;
end;

function StringRefCount(const s: String): integer;
begin
  result := Integer(Pointer(integer(Addr(s)^)-8)^);
end;

  { TBoldSharedStringManager }

constructor TBoldSharedStringManager.Create;
begin
  inherited;
  fStringCache := TBoldSharedStringCache.Create;
  fAddsRemainingToGarbageCollect := 100;
end;

destructor TBoldSharedStringManager.Destroy;
begin
  inherited;
  FreeAndNil(fStringCache);
end;

procedure TBoldSharedStringManager.GarbageCollect(KeepStringsWithOneReference: Boolean);
var
  traverser: TBoldIndexableListTraverser;
  Holder: TBoldSharedStringHolder;
  MinimumReferencesToKeep: integer;
begin
  if KeepStringsWithOneReference then
    MinimumReferencesToKeep := 1
  else
    MinimumReferencesToKeep := 2;
  Traverser := fStringCache.CreateTraverser;
  while not Traverser.EndOfList do
  begin
    Holder := TBoldSharedStringHolder(Traverser.Item);
    if Holder.ExternalRefCount < MinimumReferencesToKeep then
      fStringCache.remove(Holder);
    Traverser.Next;
  end;
  Traverser.Free;
  fAddsRemainingToGarbageCollect := fStringCache.Count;
end;

function TBoldSharedStringManager.GetInfoString: String;
begin
  result :=
    format('Number of shared strings: %d', [FStringCache.Count])+BOLDCRLF+ {do not localize }
    format('Saved memory %d', [SavedMemory]); {do not localize }
end;

function TBoldSharedStringManager.GetSavedMemory: integer;
var
  traverser: TBoldIndexableListTraverser;
  Holder: TBoldSharedStringHolder;
begin
  result := 0;
  Traverser := fStringCache.CreateTraverser;
  while not Traverser.EndOfList do
  begin
    Holder := TBoldSharedStringHolder(Traverser.Item);
    result := result + (Holder.ExternalRefCount-1)*length(Holder.Value);
    Traverser.Next;
  end;
  Traverser.Free;
end;

function TBoldSharedStringManager.GetSharedString(const s: String): String;
{$IFDEF BOLD_DISABLESHAREDSTRINGS}
begin
  result := s;
end;
{$ELSE}
var
  Holder: TBoldSharedStringHolder;
begin
  if s = '' then
    result := s
  else
  begin
    Holder := fStringCache.HolderByValue[s];
    if assigned(Holder) then
    begin
      result := Holder.Value;
      Inc(fCachedHits);
    end
    else
    begin
      Holder := TBoldSharedStringHolder.Create(s);
      fStringCache.Add(Holder);
      result := s;
    end;
  end;
  Dec(fAddsRemainingToGarbageCollect);
  if fAddsRemainingToGarbageCollect <= 0 then
    GarbageCollect(true);
end;
{$ENDIF}

{ TBoldSharedStringCache }

constructor TBoldSharedStringCache.Create;
begin
  inherited;
  SetIndexVariable(IX_StringHolderValue, AddIndex(TBoldStringHolderValueIndex.Create));
end;

function TBoldSharedStringCache.GetHolderByValue(
  const s: String): TBoldSharedStringHolder;
begin
  Result := TBoldSharedStringHolder(TBoldStringHolderValueIndex(Indexes[IX_StringHolderValue]).FindByString(s));
end;

{ TBoldStringHolderValueIndex }

function TBoldStringHolderValueIndex.ItemASKeyString(
  Item: TObject): string;
begin
  result := TBoldSharedStringHolder(item).value;
end;

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

initialization // empty
finalization
  FreeAndNil(G_BoldSharedStringManager);
end.
