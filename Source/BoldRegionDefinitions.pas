{ Global compiler directives }
{$include bold.inc}
unit BoldRegionDefinitions;

interface

uses
  classes,
  BoldBase,
  BoldSystemRT,
  BoldSubscription;

type
  TBoldRegionDefinitions = class;
  TBoldRegionCoreDefinition = class;
  TBoldConcreteRegionDefinition = class;
  TBoldSubregionReference = class;
  TBoldRegionElementInclusion = class;
  TBoldSubregionReferenceList = class;
  TBoldRegionElementInclusionList = class;
  TBoldConcreteRegionDefinitionList = class;

  TBoldRegionDefinitions = class(TBoldSubscribableObject)
  private
    fClassList: TList;
    fConcreteRegions: TList;
    fCoreDefinitions: TList;
    procedure AddRegionElementInclusion(Item: TBoldRegionElementInclusion);
    procedure AddConcreteRegionDefinition(Item: TBoldConcreteRegionDefinition);
    procedure AddCoreDefinition(Item: TBoldRegionCoreDefinition);
    function GetRegionInclusionsByMember(Member: TBoldMemberRTInfo): TBoldRegionElementInclusionList;
    function GetConcreteRegionDefinitionsByRootClass(TheClass: TBoldClassTypeInfo): TBoldConcreteRegionDefinitionList;
    function GetCoreDefinition(Name: string): TBoldRegionCoreDefinition;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure ExpandDefinitions;
    property RegionInclusionsByMember[Member: TBoldMemberRTInfo]: TBoldRegionElementInclusionList read GetRegionInclusionsByMember;
    property ConcreteRegionDefinitionsByRootClass[TheClass: TBoldClassTypeInfo]: TBoldConcreteRegionDefinitionList read GetConcreteRegionDefinitionsByRootClass;
    function EnsuredCoreDefinition(Name: string): TBoldRegionCoreDefinition;
    property CoreDefinition[Name: string]: TBoldRegionCoreDefinition read GetCoreDefinition;
    property CoreDefinitions: TList read fCoreDefinitions;
  end;

  TBoldRegionCoreDefinition = class(TBoldMemoryManagedObject)
  private
    fName: string;
    fOwner: TBoldRegionDefinitions;
    fUsedBy: TBoldSubRegionReferenceList;
    fConcreteDefinitions: TBoldConcreteRegionDefinitionList;
  protected
    procedure ExpandDefinitions;
  public
    constructor Create(Owner: TBoldRegionDefinitions; Name: string);
    destructor Destroy; override;
    function EnsuredConcreteDefinition(Root: TBoldClasstypeInfo; var Existed: Boolean): TBoldConcreteRegionDefinition;
    property Name: string read fName;
    property Owner: TBoldRegionDefinitions read fOwner;
    property ConcreteDefinitions: TBoldConcreteRegionDefinitionList read fConcreteDefinitions;
    property UsedBy: TBoldSubRegionReferenceList read fUsedBy;
  end;

  TBoldConcreteRegionDefinition = class(TBoldMemoryManagedObject)
  private
    fElements: TBoldRegionElementInclusionList;
    fSubregions: TBoldSubregionReferenceList;
    fRootClass: TBoldClassTypeInfo;
    fCoreDefinition: TBoldRegionCoreDefinition;
    fParentRegions: TBoldSubregionReferenceList;
    function GetAsString: String;
  protected
    procedure CopyFromDefinition(SuperDef: TBoldConcreteRegionDefinition);
  public
    constructor Create(CoreDefinition: TBoldRegionCoreDefinition; Root: TBoldClassTypeInfo);
    destructor Destroy; override;
    procedure Clear;
    property CoreDefinition: TBoldRegionCoreDefinition read fCoreDefinition;
    property RootClass: TBoldClassTypeInfo read fRootClass;
    property Elements: TBoldRegionElementInclusionList read fElements;
    property Subregions: TBoldSubregionReferenceList read fSubregions;
    property ParentRegions: TBoldSubregionReferenceList read fParentRegions;
    property AsString: String read GetAsString;
  end;

  TBoldSubregionReference = class(TBoldMemoryManagedObject)
  private
    fSubregionRootNavigation: TBoldRoleRTInfo;
    fSubregionCoreDefinition: TBoldRegionCoreDefinition;
    fParentRegion: TBoldConcreteRegionDefinition;
    fIsDependent: Boolean;
  protected
    procedure Clone(NewOwner: TBoldConcreteRegionDefinition);
  public
    constructor Create(ParentRegion: TBoldConcreteRegionDefinition; SubregionCoreDefinition: TBoldRegionCoreDefinition; Navigation: TBoldRoleRTInfo; IsDependent: Boolean);
    property ParentRegion: TBoldConcreteRegionDefinition read fParentRegion;
    property SubregionRootNavigation: TBoldRoleRTInfo read fSubregionRootNavigation;
    property SubregionCoreDefinition: TBoldRegionCoreDefinition read FSubregionCoreDefinition;
    property IsDependent: Boolean read fIsDependent;
  end;

  TBoldRegionElementInclusion = class(TBoldMemoryManagedObject)
  private
    fMember: TBoldMemberRTInfo;
    fRegion: TBoldConcreteRegionDefinition;
  protected
    procedure Clone(NewOwner: TBoldConcreteRegionDefinition);
  public
    constructor Create(Region: TBoldConcreteRegionDefinition; Member: TBoldMemberRTInfo);
    property Region: TBoldConcreteRegionDefinition read fRegion;
    property Member: TBoldMemberRTInfo read FMember;
  end;

  TBoldConcreteRegionDefinitionList = class(TList)
  private
    function GetItem(i: Integer): TBoldConcreteRegionDefinition;
    function GetAsCommaText: string;
  public
    function FindByRootClass(RootClass: TBoldClassTypeInfo): TBoldConcreteRegionDefinition;
    property Items[i: Integer]: TBoldConcreteRegionDefinition read GetItem; default;
  public
    property AsCommaText: string read GetAsCommaText;
  end;

  TBoldSubregionReferenceList = class(TList)
  private
    function GetItem(i: Integer): TBoldSubregionReference;
    function GetAsCommaText: string;
  public
    property Items[i: Integer]: TBoldSubregionReference read GetItem; default;
    property AsCommaText: string read GetAsCommaText;
  end;

  TBoldRegionElementInclusionList = class(TList)
  private
    function GetItem(i: Integer): TBoldRegionElementInclusion;
    function GetAsCommaText: string;
  public
    property Items[i: Integer]: TBoldRegionElementInclusion read GetItem; default;
    property AsCommaText: string read GetAsCommaText;
  end;

const
  beRegionDefinitionClearing = 100;

implementation

uses
  SysUtils;

function GetEnsuredItem(List: TList; index: integer; theClass: TClass): TObject;
var
  Obj: TObject;
begin
  while list.count <= index do
    list.Add(nil);
  Obj := List[index];
  if not assigned(Obj) then
  begin
    Obj := theClass.Create;
    List[Index] := Obj;
  end;
  result := Obj;
end;

{ TBoldConcreteRegionDefinitionList }

function TBoldConcreteRegionDefinitionList.FindByRootClass(
  RootClass: TBoldClassTypeInfo): TBoldConcreteRegionDefinition;
var
  i: Integer;
begin
  result := nil;
  for i := 0 to Count-1 do
    if Items[i].RootClass = RootClass then
    begin
      result := Items[i];
      exit;
    end;
end;

function TBoldConcreteRegionDefinitionList.GetAsCommaText: string;
var
  sl: TStringList;
  i: integer;
begin
  sl := TStringList.Create;
  try
    for I := 0 to Count - 1 do
      sl.Add(self[i].AsString);
  finally
    result := sl.CommaText;
    sl.free;
  end;
end;

function TBoldConcreteRegionDefinitionList.GetItem(i: Integer): TBoldConcreteRegionDefinition;
begin
  result := TObject(inherited Items[i]) as TBoldConcreteRegionDefinition;
end;

{ TBoldRegionDefinitions }

procedure TBoldRegionDefinitions.AddConcreteRegionDefinition(Item: TBoldConcreteRegionDefinition);
begin
  ConcreteRegionDefinitionsByRootClass[Item.RootClass].Add(Item);
end;

procedure TBoldRegionDefinitions.AddCoreDefinition(
  Item: TBoldRegionCoreDefinition);
begin
  fCoreDefinitions.Add(Item);
end;

procedure TBoldRegionDefinitions.AddRegionElementInclusion(
  Item: TBoldRegionElementInclusion);
begin
  RegionInclusionsByMember[Item.Member].Add(Item);
end;

procedure TBoldRegionDefinitions.Clear;
  procedure ClearList(list: TList);
  var
    i: integer;
  begin
    for i := 0 to List.Count-1 do
      TObject(List[i]).Free;
    List.Clear;
  end;

var
  MemberList: TList;
  RegionElementInclusionList: TBoldRegionElementInclusionList;
  i, j: integer;
begin
  SendEvent(beRegionDefinitionClearing);
  for i := 0 to fClassList.Count-1 do
  begin
    MemberList := Tlist(fClassLIst[i]);
    if assigned(MemberList) then
    begin
      for j := 0 to MemberList.Count-1 do
      begin
        RegionElementInclusionList := TBoldRegionElementInclusionList(MemberList[j]);
        RegionElementInclusionList.Free;
      end;
      MemberList.Free;
    end;
  end;
  fClassList.Clear;

  ClearList(fConcreteRegions);
  ClearList(fCoreDefinitions);
end;

constructor TBoldRegionDefinitions.Create;
begin
  fClassList := TList.Create;
  fConcreteRegions := TList.Create;
  fCoreDefinitions := TList.Create;
end;

destructor TBoldRegionDefinitions.Destroy;
begin
  FreePublisher;
  Clear;
  FreeAndNil(fClassList);
  FreeAndNil(fConcreteRegions);
  FreeAndNil(fCoreDefinitions);
  inherited;
end;

function TBoldRegionDefinitions.EnsuredCoreDefinition(Name: string): TBoldRegionCoreDefinition;
begin
  result := CoreDefinition[Name];
  if not assigned(result) then
    result := TBoldRegionCoreDefinition.Create(self, Name);
end;

procedure TBoldRegionDefinitions.ExpandDefinitions;
var
  i: integer;
begin
  for i := 0 to fCoreDefinitions.Count-1 do
    TBoldRegionCoreDefinition(fCoreDefinitions[i]).ExpandDefinitions;
end;



function TBoldRegionDefinitions.GetConcreteRegionDefinitionsByRootClass(
  TheClass: TBoldClassTypeInfo): TBoldConcreteRegionDefinitionList;
begin
  result := GetEnsuredItem(fConcreteRegions, TheClass.TopSortedIndex, TBoldConcreteRegionDefinitionList) as TBoldConcreteRegionDefinitionList;
end;

function TBoldRegionDefinitions.GetCoreDefinition(
  Name: string): TBoldRegionCoreDefinition;
var
  i: Integer;
begin
  result := nil;
  for i := 0 to fCoreDefinitions.Count-1 do
    if TBoldRegionCoreDefinition(fCoreDefinitions[i]).Name = Name then
      result := TBoldRegionCoreDefinition(fCoreDefinitions[i]);
end;

function TBoldRegionDefinitions.GetRegionInclusionsByMember(
  Member: TBoldMemberRTInfo): TBoldRegionElementInclusionList;
var
  MemberList: TList;
begin
  MemberList := GetEnsuredItem(fClassList, Member.ClassTypeInfo.TopSortedIndex, TList) as TList;
  result := GetEnsuredItem(MemberList, Member.index, TBoldRegionElementInclusionList) as TBoldRegionElementInclusionList;
end;


{ TBoldRegionElementInclusionList }

function TBoldRegionElementInclusionList.GetAsCommaText: string;
var
  sl: TStringList;
  i: integer;
begin
  sl := TStringList.Create;
  try
    for I := 0 to Count - 1 do
      sl.Add(self[i].Member.DisplayName);
  finally
    result := sl.CommaText;
    sl.free;
  end;
end;

function TBoldRegionElementInclusionList.GetItem(
  i: Integer): TBoldRegionElementInclusion;
begin
  result := TObject(inherited Items[i]) as TBoldRegionElementInclusion;
end;

{ TBoldSubregionReferenceList }

function TBoldSubregionReferenceList.GetAsCommaText: string;
var
  sl: TStringList;
  i: integer;
begin
  sl := TStringList.Create;
  try
    for I := 0 to Count - 1 do
      sl.Add(self[i].ParentRegion.RootClass.DisplayName);
  finally
    result := sl.CommaText;
    sl.free;
  end;
end;

function TBoldSubregionReferenceList.GetItem(
  i: Integer): TBoldSubregionReference;
begin
  result := TObject(inherited Items[i]) as TBoldSubregionReference;
end;

{ TBoldRegionCoreDefinition }

constructor TBoldRegionCoreDefinition.Create(Owner: TBoldRegionDefinitions; Name: string);
begin
  inherited Create;
  fConcreteDefinitions := TBoldConcreteRegionDefinitionList.Create;
  fUsedBy := TBoldSubregionReferenceList.Create;
  fOwner := Owner;
  fName := Name;
  fOwner.AddCoreDefinition(self);
end;

destructor TBoldRegionCoreDefinition.Destroy;
var
  i: integer;
begin
  inherited;
  for i := 0 to fConcreteDefinitions.Count-1 do
    TObject(fConcreteDefinitions[i]).Free;
  FreeAndNil(fConcreteDefinitions);
  FreeAndNil(fUsedBy);
end;

function TBoldRegionCoreDefinition.EnsuredConcreteDefinition(Root: TBoldClasstypeInfo; var Existed: Boolean): TBoldConcreteRegionDefinition;
begin
  result := ConcreteDefinitions.FindByRootClass(Root);
  existed := assigned(result);
  if not existed then
    result := TBoldConcreteRegionDefinition.Create(self, Root);
end;

procedure TBoldRegionCoreDefinition.ExpandDefinitions;
var
  i: integer;
  existed: Boolean;
  SuperDef, ConcreteDef: TBoldConcreteRegionDefinition;
  SystemTypeInfo: TBoldSystemTypeInfo;
  ClassTypeInfo: TBoldClassTypeInfo;
begin
  if fConcreteDefinitions.Count > 0 then
  begin
    systemTypeInfo := fConcreteDefinitions[0].RootClass.SystemTypeInfo;
    for i := 1 to SystemTypeInfo.TopSortedClasses.Count-1 do
    begin
      ClassTypeInfo := SystemTypeInfo.TopSortedClasses[i];
      SuperDef := ConcreteDefinitions.FindByRootClass(ClassTypeInfo.SuperClassTypeInfo);
      if assigned(SuperDef) then
      begin
        ConcreteDef := EnsuredConcreteDefinition(ClassTypeInfo, existed);
        ConcreteDef.CopyFromDefinition(SuperDef);
      end;
    end;
  end;
end;

{ TBoldConcreteRegionDefinition }

procedure TBoldConcreteRegionDefinition.CopyFromDefinition(SuperDef: TBoldConcreteRegionDefinition);
var
  i: integer;
begin
  for i := 0 to SuperDef.Elements.Count-1 do
    SuperDef.Elements[i].Clone(self);
  for i := 0 to SuperDef.Subregions.Count-1 do
    SuperDef.Subregions[i].Clone(self);
end;

procedure TBoldConcreteRegionDefinition.Clear;
var
  i: integer;
begin
  for i := 0 to fElements.Count-1 do
    fElements[i].Free;
  fElements.Clear;
  for i := 0 to fSubregions.Count-1 do
    fSubregions[i].Free;
  fSubregions.Clear; 
end;

constructor TBoldConcreteRegionDefinition.Create(CoreDefinition: TBoldRegionCoreDefinition; Root: TBoldClassTypeInfo);
var
  i: integer;
begin
  fElements := TBoldRegionElementInclusionList.Create;
  fSubregions := TBoldSubregionReferenceList.Create;
  fCoreDefinition := CoreDefinition;
  fParentRegions := TBoldSubregionReferenceList.Create;
  fRootClass := Root;
  CoreDefinition.ConcreteDefinitions.Add(self);
  CoreDefinition.Owner.AddConcreteRegionDefinition(self);
  for i := 0 to CoreDefinition.UsedBy.Count-1 do
    if Root.ConformsTo(CoreDefinition.UsedBy[i].SubregionRootNavigation.ClassTypeInfoOfOtherEnd) then
      fParentRegions.Add(CoreDefinition.UsedBy[i]);
end;

destructor TBoldConcreteRegionDefinition.Destroy;
begin
  Clear;
  FreeAndNil(fElements);
  FreeAndNil(fSubregions);
  FreeAndNil(fParentRegions);
  inherited;
end;

function TBoldConcreteRegionDefinition.GetAsString: String;
begin
  result := RootClass.ExpressionName + ':' + CoreDefinition.Name;
end;

{ TBoldSubregionReference }

procedure TBoldSubregionReference.Clone(NewOwner: TBoldConcreteRegionDefinition);
begin
  TBoldSubregionReference.Create(NewOwner, SubregionCoreDefinition, SubregionRootNavigation, IsDependent);
end;

constructor TBoldSubregionReference.Create(ParentRegion: TBoldConcreteRegionDefinition; SubregionCoreDefinition: TBoldRegionCoreDefinition; Navigation: TBoldRoleRTInfo; IsDependent: Boolean);
var
  i: integer;
begin
  inherited Create;
  assert(assigned(Navigation), 'Tried to create a SubRegionReference without a RoleRTInfo');
  fParentRegion := ParentRegion;
  fSubregionCoreDefinition := SubregionCoreDefinition;
  fIsDependent := IsDependent;
  fSubregionRootNavigation := Navigation;

  ParentRegion.Subregions.Add(self);
  SubregionCoreDefinition.UsedBy.Add(self);
  for i := 0 to SubregionCoreDefinition.ConcreteDefinitions.Count-1 do
    if SubregionCoreDefinition.ConcreteDefinitions[i].RootClass.ConformsTo(Navigation.ClassTypeInfoOfOtherEnd) then
      SubregionCoreDefinition.ConcreteDefinitions[i].ParentRegions.Add(self);
end;

{ TBoldRegionElementInclusion }

procedure TBoldRegionElementInclusion.Clone(NewOwner: TBoldConcreteRegionDefinition);
begin
  TBoldRegionElementInclusion.Create(NewOwner, Member);
end;

constructor TBoldRegionElementInclusion.Create(Region: TBoldConcreteRegionDefinition; Member: TBoldMemberRTInfo);
begin
  inherited create;
  fRegion := Region;
  fMember := Member;

  Region.Elements.Add(self);
  Region.CoreDefinition.Owner.AddRegionElementInclusion(Self);

  if Member.IsMultiRole then
    (Member as TBoldRoleRTInfo).RoleRTInfoOfOtherEnd.SetForceOtherEnd;
end;

end.
