{ Global compiler directives }
{$include bold.inc}
unit BoldTaggedValueList;

interface

uses
  BoldDefs,
  BoldBase,
  BoldContainers,
  BoldHashIndexes,
  BoldNamedValueList;

type
  { forward declarations }
  TBoldTaggedValueList = class;
  TBoldTaggedValueDefinition = class;
  TBoldTaggedValuePerClassList = class;
  TBoldTaggedValueDefinitionIndex = class;

  { TBoldTaggedValueList }
  TBoldTaggedValueList = class(TBoldMemoryManagedObject)
  private
    fList: TBoldObjectArray;
    fDefinitionIndex: TBoldTaggedValueDefinitionIndex;
    function GetCount: integer;
    function GetDefinition(const i: integer): TBoldTaggedValueDefinition;
    function GetDefaultValueForTag(const Tag: string): string;
    function GetDefinitionForTag(const Tag: String): TBoldTaggedValueDefinition;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const TypeName, Tag, DefaultValue: string);
    property Definition[const i: integer]: TBoldTaggedValueDefinition read GetDefinition;
    property DefaultValueForTag[const Tag: string]: string read GetDefaultValueForTag;
    property DefinitionForTag[const Tag: String]: TBoldTaggedValueDefinition read GetDefinitionForTag;
    property Count: integer read GetCount;
  end;

  { TBoldTaggedValueDefinition }
  TBoldTaggedValueDefinition = class(TBoldMemoryManagedObject)
  private
    fTag: string;
    fDefaultValue: string;
    fTypeName: string;
  public
    constructor Create(const TypeName, Tag, DefaultValue: string);
    property Tag: string read fTag;
    property DefaultValue: string read fDefaultValue;
    property TypeName: string read fTypeName;
  end;

  { TBoldTaggedValuePerClassList }
  TBoldTaggedValuePerClassList = class(TBoldMemoryManagedObject)
  private
    fList: TBoldNamedValueList;
    function GetListForClassName(const ClassName: string): TBoldTaggedValueList;
    function GetDefaultForClassAndTag(const ClassName, Tag: string): string;
  public
    constructor Create;
    destructor Destroy; override;
    property ListForClassName[const ClassName: string]: TBoldTaggedValueList read GetListForClassName;
    property DefaultForClassAndTag[const ClassName, Tag: string]: string read GetDefaultForClassAndTag;
  end;

  TBoldTaggedValueDefinitionIndex = class(TBoldCaseSensitiveStringHashIndex)
  protected
    function ItemAsKeyString(Item: TObject): string; override;
  end;

implementation

uses
  SysUtils,
  BoldSharedStrings;

{ TBoldTaggedValueDefinition }

constructor TBoldTaggedValueDefinition.Create(const TypeName, Tag, DefaultValue: string);
begin
  inherited Create;
  fTag := BoldSharedStringManager.GetSharedString(Tag);
  fTypeName := BoldSharedStringManager.GetSharedString(TypeName);
  fDefaultValue := BoldSharedStringManager.GetSharedString(DefaultValue);
end;

{ TBoldTaggedValueList }

procedure TBoldTaggedValueList.Add(const TypeName, Tag, DefaultValue: string);
var
  def: TBoldTaggedValueDefinition;
begin
  def := TBoldTaggedValueDefinition.Create(TypeName, Tag, DefaultValue);
  fList.Add(def);
  fDefinitionIndex.Add(def);
end;

constructor TBoldTaggedValueList.Create;
begin
  inherited;
  fList := TBoldObjectArray.Create(1, [bcoDataOwner]);
  fDefinitionIndex := TBoldTaggedValueDefinitionIndex.Create;
end;

destructor TBoldTaggedValueList.Destroy;
begin
  FreeAndNil(fList);
  FreeAndNil(fDefinitionIndex);
  inherited;
end;

function TBoldTaggedValueList.GetCount: integer;
begin
  result := fList.Count;
end;

function TBoldTaggedValueList.GetDefaultValueForTag(const Tag: string): string;
var
  def: TBoldTaggedValueDefinition;
begin
  def := DefinitionForTag[Tag];
  if assigned(def) then
    result := Def.DefaultValue
  else
    result := '';
end;

function TBoldTaggedValueList.GetDefinition(const i: integer): TBoldTaggedValueDefinition;
begin
  Result := TBoldTaggedValueDefinition(fList[i]);
end;

function TBoldTaggedValueList.GetDefinitionForTag(const Tag: String): TBoldTaggedValueDefinition;
begin
  result := TBoldTaggedValueDefinition(fDefinitionIndex.FindByString(tag));
end;

{ TBoldTaggedValuePerClassList }

constructor TBoldTaggedValuePerClassList.Create;
begin
  inherited;
  fList := TBoldNamedValueList.Create;
end;

destructor TBoldTaggedValuePerClassList.Destroy;
var
  i: integer;
begin
  for i := 0 to fList.Count-1 do
    fList[i].aObject.Free;
  FreeAndNil(fList);
  inherited;
end;

function TBoldTaggedValuePerClassList.GetDefaultForClassAndTag(const ClassName, Tag: string): string;
begin
  Result := ListForClassName[ClassName].DefaultValueForTag[Tag]
end;

function TBoldTaggedValuePerClassList.GetListForClassName(const ClassName: string): TBoldTaggedValueList;

begin
  Result := TBoldTaggedValueList(fList.ObjectByname[ClassName]);
  if not Assigned(Result) then
  begin
    Result := TBoldTaggedValueList.Create;
    fList.AddEntry(ClassName, '', Result);
  end
end;

{ TBoldTaggedValueDefinitionIndex }

function TBoldTaggedValueDefinitionIndex.ItemAsKeyString(Item: TObject): string;
begin
  result := TBoldTaggedValueDefinition(item).Tag;
end;

end.
