
{ Global compiler directives }
{$include bold.inc}
unit BoldPMapperLists;

interface

uses
  BoldTypeNameDictionary,
  BoldPMapper,
  BoldPMappers,
  BoldDefs,
  BoldHashIndexes,
  BoldIndexableList;

type
  TBoldPersistenceMapperDescriptor = class;
  TBoldMemberPersistenceMapperDescriptorList = class;
  TBoldMemberPersistenceMapperDescriptor = class;
  TBoldSystemPersistenceMapperDescriptorList = class;
  TBoldSystemPersistenceMapperDescriptor = class;
  TBoldObjectPersistenceMapperDescriptorList = class;
  TBoldObjectPersistenceMapperDescriptor = class;

  {---TBoldPersistenceMapperDescriptor---}
  TBoldPersistenceMapperDescriptor = class
  private
    fName: string;
    fPersistenceMapperClass: TBoldPersistenceMapperClass;
  public
    constructor Create(const name: string; PersistenceMapperClass: TBoldPersistenceMapperClass);
    property Name: string read fName;
    property PersistenceMapperClass: TBoldPersistenceMapperClass read fPersistenceMapperClass;
  end;

  {---TBoldMemberPersistenceMapperList---}
  TBoldMemberPersistenceMapperDescriptorList = class(TBoldIndexableList)
  private
    class var IX_MemberPMapperName: integer;
    class var IX_MemberPMapperClass: integer;
    function GetDescriptorBydelphiName(name: string): TBoldMemberPersistenceMapperDescriptor;
    function GetDescriptors(index: Integer): TBoldMemberPersistenceMapperDescriptor;
  public
    constructor Create;
    procedure AddDescriptor(MemberPersistenceMapperClass: TBoldMemberPersistenceMapperClass;
                            const AbstractionLevel: TBoldAbstractionLevel);
    procedure RemoveDescriptorByClass(aClass: TBoldMemberPersistenceMapperClass);
    function DescriptorForModelNameWithDefaultSupport(ModelName, DefaultMapperName: String; TypeNameDictionary: TBoldTypeNameDictionary): TBoldMemberPersistenceMapperDescriptor;
    property DescriptorByDelphiName[name: string]: TBoldMemberPersistenceMapperDescriptor read GetDescriptorByDelphiName;
    property Descriptors[index: integer]: TBoldMemberPersistenceMapperDescriptor read GetDescriptors;
  end;

  {---TBoldMemberPersistenceMapperDescriptor---}
  TBoldMemberPersistenceMapperDescriptor = class(TBoldPersistenceMapperDescriptor)
  private
    fAbstractionLevel: TBoldAbstractionLevel;
    function GetMemberPersistenceMapperClass: TBoldMemberPersistenceMapperClass;
  public
    constructor Create(MemberPersistenceMapperClass: TBoldMemberPersistenceMapperClass;
                       const AbstractionLevel: TBoldAbstractionLevel);
    property MemberPersistenceMapperClass: TBoldMemberPersistenceMapperClass read GetMemberPersistenceMapperClass;
    property AbstractionLevel: TBoldAbstractionLevel read fAbstractionLevel;
    function CanStore(const ContentName: string):Boolean;
  end;

  {---TBoldSystemPersistenceMapperList---}
  TBoldSystemPersistenceMapperDescriptorList = class(TBoldIndexableList)
  private
    class var IX_SystemPMapperName: integer;
    function GetDescriptorByName(name: string): TBoldSystemPersistenceMapperDescriptor;
    function GetDescriptors(index: Integer): TBoldSystemPersistenceMapperDescriptor;
  public
    constructor Create;
    procedure RemoveDescriptorByName(const Name: string);
    property DescriptorByName[name: string]: TBoldSystemPersistenceMapperDescriptor read GetDescriptorByName;
    property Descriptors[index: integer]: TBoldSystemPersistenceMapperDescriptor read GetDescriptors;
  end;

  {---TBoldSystemPersistenceMapperDescriptor---}
  TBoldSystemPersistenceMapperDescriptor = class(TBoldPersistenceMapperDescriptor)
  private
    function GetSystemPersistenceMapperClass: TBoldSystemPersistenceMapperClass;
  public
    constructor Create(const name: string; BoldSystemPersistenceMapperClass: TBoldSystemPersistenceMapperClass);
    property SystemPersistenceMapperClass: TBoldSystemPersistenceMapperClass read GetSystemPersistenceMapperClass;
  end;

  {---TBoldObjectPersistenceMapperList---}
  TBoldObjectPersistenceMapperDescriptorList = class(TBoldIndexableList)
  private
    class var IX_ObjectPMapperName: integer;
    function GetDescriptorByName(name: string): TBoldObjectPersistenceMapperDescriptor;
    function GetDescriptors(index: Integer): TBoldObjectPersistenceMapperDescriptor;
  public
    constructor Create;
    procedure RemoveDescriptorByName(const Name: string);
    property DescriptorByName[name: string]: TBoldObjectPersistenceMapperDescriptor read GetDescriptorByName;
    property Descriptors[index: integer]: TBoldObjectPersistenceMapperDescriptor read GetDescriptors;
  end;

  {---TBoldObjectPersistenceMapperDescriptor---}
  TBoldObjectPersistenceMapperDescriptor = class(TBoldPersistenceMapperDescriptor)
  private
    function GetObjectPersistenceMapperClass: TBoldObjectPersistenceMapperClass;
  public
    constructor Create(const name: string; BoldObjectPersistenceMapperClass: TBoldObjectPersistenceMapperClass);
    property ObjectPersistenceMapper: TBoldObjectPersistenceMapperClass read GetObjectPersistenceMapperClass;
  end;

function BoldMemberPersistenceMappers: TBoldMemberPersistenceMapperDescriptorList;
function BoldMemberPersistenceMappersAssigned: Boolean;
function BoldSystemPersistenceMappers: TBoldSystemPersistenceMapperDescriptorList;
function BoldSystemPersistenceMappersAssigned: Boolean;
function BoldObjectPersistenceMappers: TBoldObjectPersistenceMapperDescriptorList;
function BoldObjectPersistenceMappersAssigned: Boolean;

implementation

uses
  SysUtils,
  BoldUtils,
  BoldIndex
  ;

var
  G_BoldMemberPersistenceMappers: TBoldMemberPersistenceMapperDescriptorList = nil;
  G_BoldSystemPersistenceMappers: TBoldSystemPersistenceMapperDescriptorList = nil;
  G_BoldObjectPersistenceMappers: TBoldObjectPersistenceMapperDescriptorList = nil;

type
{---TBoldMemberPMapperNameIndex---}
  TBoldMemberPMapperNameIndex = class(TBoldStringHashIndex)
  protected
    function ItemAsKeyString(Item: TObject): string; override;
  end;

{---TBoldMemberPMapperClassIndex---}
  TBoldMemberPMapperClassIndex = class(TBoldClassHashIndex)
  protected
    function ItemAsKeyClass(Item: TObject): TClass; override;
  end;

{---TBoldSystemPMapperNameIndex---}
  TBoldSystemPMapperNameIndex = class(TBoldStringHashIndex)
  protected
    function ItemAsKeyString(Item: TObject): string; override;
  end;

{---TBoldObjectPMapperNameIndex---}
  TBoldObjectPMapperNameIndex = class(TBoldStringHashIndex)
  protected
    function ItemAsKeyString(Item: TObject): string; override;
  end;

{---TBoldMemberPMapperNameIndex---}
function TBoldMemberPMapperNameIndex.ItemAsKeyString(Item: TObject): string;
begin
  Result := TBoldMemberPersistenceMapperDescriptor(Item).MemberPersistenceMapperClass.ClassName;
end;

{---TBoldMemberPMapperClassIndex---}
function TBoldMemberPMapperClassIndex.ItemAsKeyClass(Item: TObject): TClass;
begin
  Result := TBoldMemberPersistenceMapperDescriptor(Item).MemberPersistenceMapperClass;
end;

{---TBoldSystemPMapperNameIndex---}
function TBoldSystemPMapperNameIndex.ItemAsKeyString(Item: TObject): string;
begin
  Result := TBoldSystemPersistenceMapperDescriptor(Item).Name;
end;

{---TBoldObjectPMapperNameIndex---}
function TBoldObjectPMapperNameIndex.ItemAsKeyString(Item: TObject): string;
begin
  Result := TBoldObjectPersistenceMapperDescriptor(Item).Name;
end;

{---TBoldPersistenceMapperDescriptor--}
constructor TBoldPersistenceMapperDescriptor.Create(const name: string; PersistenceMapperClass: TBoldPersistenceMapperClass);
begin
  inherited Create;
  fName := Name;
  fPersistenceMapperClass := PersistenceMapperClass;
end;

{---BoldMemberPersistenceMappers---}
function BoldMemberPersistenceMappers: TBoldMemberPersistenceMapperDescriptorList;
begin
  if not Assigned(G_BoldMemberPersistenceMappers) then
    G_BoldMemberPersistenceMappers := TBoldMemberPersistenceMapperDescriptorList.Create;
  Result := G_BoldMemberPersistenceMappers;
end;

function BoldMemberPersistenceMappersAssigned: Boolean;
begin
  Result := Assigned(G_BoldMemberPersistenceMappers);
end;

{---BoldSystemPersistenceMappers---}
function BoldSystemPersistenceMappers: TBoldSystemPersistenceMapperDescriptorList;
begin
  if not Assigned(G_BoldSystemPersistenceMappers) then
    G_BoldSystemPersistenceMappers := TBoldSystemPersistenceMapperDescriptorList.Create;
  Result := G_BoldSystemPersistenceMappers;
end;

function BoldSystemPersistenceMappersAssigned: Boolean;
begin
  Result := Assigned(G_BoldSystemPersistenceMappers);
end;

{---BoldObjectPersistenceMappers---}
function BoldObjectPersistenceMappers: TBoldObjectPersistenceMapperDescriptorList;
begin
  if not Assigned(G_BoldObjectPersistenceMappers) then
    G_BoldObjectPersistenceMappers := TBoldObjectPersistenceMapperDescriptorList.Create;
  Result := G_BoldObjectPersistenceMappers;
end;

function BoldObjectPersistenceMappersAssigned: Boolean;
begin
  Result := Assigned(G_BoldObjectPersistenceMappers);
end;

{---TBoldMemberPersistenceMapperDescriptorList---}
constructor TBoldMemberPersistenceMapperDescriptorList.Create;
begin
  inherited;
  SetIndexCapacity(2);
  SetIndexVariable(IX_MemberPMapperName, AddIndex(TBoldMemberPMapperNameIndex.Create));
  SetIndexVariable(IX_MemberPMapperClass, AddIndex(TBoldMemberPMapperClassIndex.Create));
end;

function TBoldMemberPersistenceMapperDescriptorList.GetDescriptorByDelphiName(name: string): TBoldMemberPersistenceMapperDescriptor;
begin
  Result := TBoldMemberPersistenceMapperDescriptor(TBoldStringHashIndex(Indexes[IX_MemberPMapperName]).FindByString(Name))
end;

function TBoldMemberPersistenceMapperDescriptorList.GetDescriptors(index: integer): TBoldMemberPersistenceMapperDescriptor;
begin
  Result := TBoldMemberPersistenceMapperDescriptor(Items[index]);
end;

procedure TBoldMemberPersistenceMapperDescriptorList.AddDescriptor(MemberPersistenceMapperClass: TBoldMemberPersistenceMapperClass;
                                                           const AbstractionLevel: TBoldAbstractionLevel);
begin
  Add(TBoldMemberPersistenceMapperDescriptor.Create(
                                            MemberPersistenceMapperClass,
                                            AbstractionLevel));
end;

procedure TBoldMemberPersistenceMapperDescriptorList.RemoveDescriptorByClass(aClass: TBoldMemberPersistenceMapperClass);
begin
  Assert(Assigned(DescriptorBydelphiName[aClass.ClassName]), 'Trying to remove TBoldMemberPersistenceMapperDescriptor ' + aClass.ClassName + ' not in list');
  Remove(DescriptorByDelphiName[aClass.ClassName]);
end;

{---TBoldMemberPersistenceMapperDescriptor---}
constructor TBoldMemberPersistenceMapperDescriptor.Create(MemberPersistenceMapperClass: TBoldMemberPersistenceMapperClass;
                                                  const AbstractionLevel: TBoldAbstractionLevel);
begin
  inherited Create(MemberPersistenceMapperClass.ClassName, MemberPersistenceMapperClass);
  fAbstractionLevel := AbstractionLevel;
end;

function TBoldMemberPersistenceMapperDescriptor.GetMemberPersistenceMapperClass: TBoldMemberPersistenceMapperClass;
begin
  Result := TBoldMemberPersistenceMapperClass(PersistenceMapperClass);
end;

function TBoldMemberPersistenceMapperDescriptor.CanStore(const ContentName: String): Boolean;
begin
  Result := MemberPersistenceMapperClass.CanStore(ContentName);
end;

{---TBoldSystemPersistenceMapperDescriptorList---}
constructor TBoldSystemPersistenceMapperDescriptorList.Create;
begin
  inherited;
  SetIndexCapacity(1);
  SetIndexVariable(IX_SystemPMapperName, AddIndex(TBoldSystemPMapperNameIndex.Create));
end;

function TBoldSystemPersistenceMapperDescriptorList.GetDescriptorByName(name: string): TBoldSystemPersistenceMapperDescriptor;
begin
  Result := TBoldSystemPersistenceMapperDescriptor(TBoldStringHashIndex(Indexes[IX_SystemPMapperName]).FindByString(Name))
end;

function TBoldSystemPersistenceMapperDescriptorList.GetDescriptors(index: Integer): TBoldSystemPersistenceMapperDescriptor;
begin
  Result := TBoldSystemPersistenceMapperDescriptor(Items[index]);
end;

procedure TBoldSystemPersistenceMapperDescriptorList.RemoveDescriptorByName(const Name: string);
begin
  Assert(Assigned(DescriptorByName[name]), 'Trying to remove TBoldSystemPersistenceMapperDescriptor descriptor ' + Name + ' not in list');
  Remove(DescriptorByName[name]);
end;

{---TBoldSystemPersistenceMapperDescriptor---}
constructor TBoldSystemPersistenceMapperDescriptor.Create(const name: string; BoldSystemPersistenceMapperClass: TBoldSystemPersistenceMapperClass);
begin
  inherited Create(Name, BoldSystemPersistenceMapperClass);
end;

function TBoldSystemPersistenceMapperDescriptor.GetSystemPersistenceMapperClass: TBoldSystemPersistenceMapperClass;
begin
  Result := TBoldSystemPersistenceMapperClass(PersistenceMapperClass);
end;

{---TBoldObjectPersistenceMapperDescriptorList---}
constructor TBoldObjectPersistenceMapperDescriptorList.Create;
begin
  inherited;
  SetIndexCapacity(1);
  SetIndexVariable(IX_ObjectPMapperName, AddIndex(TBoldObjectPMapperNameIndex.Create));
end;

function TBoldObjectPersistenceMapperDescriptorList.GetDescriptorByName(name: string): TBoldObjectPersistenceMapperDescriptor;
begin
  Result := TBoldObjectPersistenceMapperDescriptor(TBoldStringHashIndex(Indexes[IX_ObjectPMapperName]).FindByString(Name))
end;

function TBoldObjectPersistenceMapperDescriptorList.GetDescriptors(index: Integer): TBoldObjectPersistenceMapperDescriptor;
begin
  Result := TBoldObjectPersistenceMapperDescriptor(Items[index]);
end;

procedure TBoldObjectPersistenceMapperDescriptorList.RemoveDescriptorByName(const Name: string);
begin
  Assert(Assigned(DescriptorByName[name]), 'Trying to remove TBoldObjectPersistenceMapperDescriptor descriptor ' + Name + ' not in list');
  Remove(DescriptorByName[name]);
end;

{---TBoldObjectPersistenceMapperDescriptor---}
constructor TBoldObjectPersistenceMapperDescriptor.Create(const name: string; BoldObjectPersistenceMapperClass: TBoldObjectPersistenceMapperClass);
begin
  inherited Create(Name, BoldObjectPersistenceMapperClass);
end;

function TBoldObjectPersistenceMapperDescriptor.GetObjectPersistenceMapperClass: TBoldObjectPersistenceMapperClass;
begin
  Result := TBoldObjectPersistenceMapperClass(PersistenceMapperClass);
end;

function TBoldMemberPersistenceMapperDescriptorList.DescriptorForModelNameWithDefaultSupport(
  ModelName, DefaultMapperName: String;
  TypeNameDictionary: TBoldTypeNameDictionary): TBoldMemberPersistenceMapperDescriptor;
var
  ActualMapperName: String;
  Mapping: tBoldTypeNameMapping;
begin
  if SameText(DefaultMapperName, DEFAULTNAME) then
  begin
    Mapping := TypeNameDictionary.MappingForModelName[ModelName];
    if not assigned(Mapping) then
      Mapping := TypeNameDictionary.MappingForModelName[DEFAULTNAME];

    if not assigned(Mapping) then
      ActualMapperName := ''
    else
      ActualMapperName := Mapping.ExpandedMapperName;
  end
  else
    ActualmapperName := DefaultMapperName;

  result := DescriptorByDelphiName[ActualMapperName];
end;

initialization
  TBoldMemberPersistenceMapperDescriptorList.IX_MemberPMapperName := -1;
  TBoldMemberPersistenceMapperDescriptorList.IX_MemberPMapperClass := -1;
  TBoldSystemPersistenceMapperDescriptorList.IX_SystemPMapperName := -1;
  TBoldObjectPersistenceMapperDescriptorList.IX_ObjectPMapperName := -1;

finalization
  FreeAndNil(G_BoldMemberPersistenceMappers);
  FreeAndNil(G_BoldSystemPersistenceMappers);
  FreeAndNil(G_BoldObjectPersistenceMappers);
  {end - finalization}

end.