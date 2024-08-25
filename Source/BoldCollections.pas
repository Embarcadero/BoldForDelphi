
{ Global compiler directives }
{$include bold.inc}
unit BoldCollections;

interface

uses
  Classes,
  BoldHashIndexes;

type
  {forward declarations of all classes}

  TBoldUniqueNameItemIndex = class;
  TBoldUniquelyNamedCollectionItem = class;
  TBoldCollectionWithUniquelyNamedItems = class;
  TBoldUniquelyNamedCollectionItemClass = class of TBoldUniquelyNamedCollectionItem;

  { TBoldUniqueNameItemIndex }

  TBoldUniqueNameItemIndex = class(TBoldStringHashIndex)
  protected
    function ItemAsKeyString(Item: TObject): string; override;
  end;

  { TBoldUniquelyNamedCollectionItem }

  TBoldUniquelyNamedCollectionItem = class(TCollectionItem)
  private
    procedure InternalSetUniqueName(const Value: string);
    procedure EnsureNameUnique(const Value: string);
    function GetCollection: TBoldCollectionWithUniquelyNamedItems;
  protected
    function GetDisplayName: string; override;
    function GetUniqueName: string; virtual; abstract;
    procedure SetUniqueName(const Value: String); virtual; abstract;
    property UniqueName: string read GetUniqueName write InternalSetUniqueName;
    property Collection: TBoldCollectionWithUniquelyNamedItems read GetCollection;
  public
    function GetNamePath: string; override;
  end;

  { TBoldUniquelyNamedCollectionItemWithNameStorage }

  TBoldUniquelyNamedCollectionItemWithNameStorage = class(TBoldUniquelyNamedCollectionItem)
  private
    fUniqueName: string;
  protected
    function GetUniqueName: string; override;
    procedure SetUniqueName(const Value: String); override;
  end;

  { TBoldCollectionWithUniquelyNamedItems }

  TBoldCollectionWithUniquelyNamedItems = class(TOwnedCollection)
  private
    fItemIndex: TBoldUniqueNameItemIndex;
    function GetItemByName(const Name: String): TBoldUniquelyNamedCollectionItem;
    function GetItemIndex: TBoldUniqueNameItemIndex;
  protected
    procedure Update(Item: TCollectionItem); override;
    property ItemIndex: TBoldUniqueNameItemIndex read GetItemIndex;
  public
    constructor Create(AOwner: TPersistent; ItemClass: TBoldUniquelyNamedCollectionItemClass);
    destructor Destroy; override;
    property ItemByName[const Name: String]: TBoldUniquelyNamedCollectionItem read GetItemByName;
  end;


implementation

uses
  SysUtils,

  BoldCoreConsts,
  BoldDefs,
  BoldIndex;

{ TBoldUniquelyNamedCollectionItem }

procedure TBoldUniquelyNamedCollectionItem.EnsureNameUnique(const Value: string);
begin
  if assigned(Collection.ItemByName[value]) then
    raise EBold.CreateFmt(sDuplicateName, [value]);
end;

function TBoldUniquelyNamedCollectionItem.GetCollection: TBoldCollectionWithUniquelyNamedItems;
begin
  result := inherited Collection as TBoldCollectionWithUniquelyNamedItems;
end;

function TBoldUniquelyNamedCollectionItem.GetDisplayName: string;
begin
  result := UniqueName;
end;

function TBoldUniquelyNamedCollectionItem.GetNamePath: string;
begin
  if assigned(Collection) then
    Result := Format('%s.%s',[Collection.GetNamePath, UniqueName])
  else
    Result := ClassName;
end;

procedure TBoldUniquelyNamedCollectionItem.InternalSetUniqueName(const Value: string);
begin
  if Value <> UniqueName then
  begin
    EnsureNameUnique(Value);
    Collection.ItemIndex.Remove(self);
    SetUniqueName(Value);
    Collection.ItemIndex.Add(self);
  end;
end;

{ TBoldUniqueNameItemIndex }

function TBoldUniqueNameItemIndex.ItemAsKeyString(Item: TObject): string;
begin
  assert(item is TBoldUniquelyNamedCollectionItem);
  result := TBoldUniquelyNamedCollectionItem(item).UniqueName;
end;

{ TBoldCollectionWithUniquelyNamedItems }

constructor TBoldCollectionWithUniquelyNamedItems.Create(AOwner: TPersistent; ItemClass: TBoldUniquelyNamedCollectionItemClass);
begin
  inherited Create(aOwner, ItemClass);
end;

destructor TBoldCollectionWithUniquelyNamedItems.Destroy;
begin
  FreeAndNil(fItemIndex);
  inherited;
end;

function TBoldCollectionWithUniquelyNamedItems.GetItemByName(const Name: String): TBoldUniquelyNamedCollectionItem;
begin
  result := ItemIndex.FindByString(name) as TBoldUniquelyNamedCollectionItem;
end;

function TBoldCollectionWithUniquelyNamedItems.GetItemIndex: TBoldUniqueNameItemIndex;
var
  i: integer;
begin
  if not assigned(fItemIndex) then
  begin
    fItemIndex := TBoldUniqueNameItemIndex.Create;
    for i := 0 to Count-1 do
      fItemIndex.Add(items[i]);
  end;
  result := fItemIndex;
end;

procedure TBoldCollectionWithUniquelyNamedItems.Update(Item: TCollectionItem);
begin
  FreeAndNil(fItemIndex);
end;

{ TBoldUniquelyNamedCollectionItemWithNameStorage }

function TBoldUniquelyNamedCollectionItemWithNameStorage.GetUniqueName: string;
begin
  result := fUniqueName;
end;

procedure TBoldUniquelyNamedCollectionItemWithNameStorage.SetUniqueName(const Value: String);
begin
  fUniqueName := Value;
end;

end.
