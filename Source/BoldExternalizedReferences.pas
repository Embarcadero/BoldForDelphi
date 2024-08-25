{ Global compiler directives }
{$include bold.inc}
unit BoldExternalizedReferences;

interface

uses
  BoldBase,
  BoldHashIndexes,
  BoldIndexableList;

type
  { forward declarations }
  TBoldExternalizedReferenceList = class;

  { TBoldExternalizedReferenceList }
  TBoldExternalizedReferenceList = class(TBoldMemoryManagedObject)
  private
    fList: TBoldUnorderedIndexableList;
    FManageReferencedObject: Boolean;
    procedure SetManageReferencedObject(const Value: Boolean);
    function GetCount: integer;
    function GetReferencedObject(Referee: TObject): TObject;
    procedure SetReferencedObject(Referee, Referenced: TObject);
  public
    constructor create;
    destructor Destroy; override;
    property ManageReferencedObject: Boolean read FManageReferencedObject write SetManageReferencedObject;
    property ReferencedObjects[Referee: TObject]: TObject read GetReferencedObject write SetReferencedObject;
    property Count: integer read GetCount;
  end;

implementation

uses
  SysUtils,
  BoldIndex;

type
  { TBoldExternalLink }
  TBoldExternalLink = class(TBoldMemoryManagedObject)
  private
    FReferee: TObject;
    FReferenced: TObject;
  public
    property Referee: TObject read FReferee write fReferee;
    property Referenced: TObject read FReferenced write fReferenced;
  end;

  { TBoldExternalizedIndexList }
  TBoldExternalizedIndexList = class(TBoldUnorderedIndexablelist)
  private
    class var IX_ExternalRef: integer;
  public
    constructor Create;
    function FindByReferee(Referee: TObject): TBoldExternalLink;
  end;

  { TBoldExternalizedReferenceHashIndex }
  TBoldExternalizedReferenceHashIndex = class(TBoldObjectHashIndex)
  protected
    function ItemAsKeyObject(Item: TObject): TObject; override;
  end;

{ TBoldExternalizedIndexList }

constructor TBoldExternalizedIndexList.Create;
begin
  inherited;
  SetIndexVariable(IX_ExternalRef, AddIndex(TBoldExternalizedReferenceHashIndex.Create));
  OwnsEntries := true;
end;

function TBoldExternalizedIndexList.FindByReferee(Referee: TObject): TBoldExternalLink;
begin
  result := TBoldExternalLink(TBoldExternalizedReferenceHashIndex(Indexes[IX_ExternalRef]).FindByObject(Referee));
end;

constructor TBoldExternalizedReferenceList.create;
begin
  inherited;
  flist := TBoldExternalizedIndexList.Create;
end;

destructor TBoldExternalizedReferenceList.Destroy;
begin
  FreeAndNil(fList);
  inherited;
end;

function TBoldExternalizedReferenceList.GetCount: integer;
begin
  result := fLIst.Count;
end;

function TBoldExternalizedReferenceList.GetReferencedObject(Referee: TObject): TObject;
var
  Link: TBoldExternalLink;
begin
  Assert(flist is TBoldExternalizedIndexList);
  Link := TBoldExternalizedIndexList(flist).FindByReferee(Referee);
  if assigned(Link) then
    result := Link.Referenced
  else
    result := nil;
end;

procedure TBoldExternalizedReferenceList.SetManageReferencedObject(const Value: Boolean);
begin
  FManageReferencedObject := Value;
end;

procedure TBoldExternalizedReferenceList.SetReferencedObject(Referee, Referenced: TObject);
var
  Link: TBoldExternalLink;
begin
  Assert(flist is TBoldExternalizedIndexList);
  Link := TBoldExternalizedIndexList(flist).FindByReferee(Referee);
  if assigned(Link) then
  begin
    if (Link.Referee = Referee) and (Link.Referenced = Referenced) then
      exit;
    if ManageReferencedObject then
      FreeAndNil(Link.fReferenced);
    fList.Remove(Link);
  end;
  if Assigned(referenced) then
  begin
    Link := TBoldExternalLink.Create;
    Link.Referenced := Referenced;
    Link.Referee := Referee;
    fList.Add(Link);
  end;
end;

{ TBoldExternalizedReferenceHashIndex }

function TBoldExternalizedReferenceHashIndex.ItemAsKeyObject(Item: TObject): TObject;
begin
  Assert(item is TBoldExternalLink);
  result := TBoldExternalLink(item).Referee;
end;

initialization
  TBoldExternalizedIndexList.IX_ExternalRef := -1;

end.