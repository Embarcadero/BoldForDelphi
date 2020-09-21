unit BoldRose98ptyCreator;

interface

uses
  Classes,
  BoldTaggedValueList;

type
  { forward declaration of all classes }
  TBoldRose98ptyCreator = class;

  { TBoldRose98ptyCreator }
  TBoldRose98ptyCreator = class
  private
    fLeadingSpaces: integer;
    fList: TStrings;
    fRoseEnums: TBoldTaggedValuePerClassList; // Slightly unorthodox reuse, ClassName=EnumName
    procedure InitializeRoseEnums;
    procedure PutLine(const s: string); overload;
    procedure PutLine; overload;
    procedure Indent;
    procedure DeDent;
    procedure StartSection(const RoseName, UMLName: string);
    procedure EndSection;
    procedure PutSeparator(const s: string);
    procedure PutTaggedValuesForClass(const UMLName: string);
    procedure PutEnumDefinition(const EnumName: string);
    procedure CreateContents;
    procedure PutRoseAttribute(TaggedValue: TBoldTaggedValueDefinition);
    function LookupRoseEnumValue(const EnumName, Value: string): string;
   public
    constructor Create;
    destructor Destroy; override;
    property List: TStrings read fList;
  end;

implementation

uses
  BoldDefs,
  sysutils,
  BoldDefaultTaggedValues,
  BoldUMLTaggedValues,
  BoldUMLRose98Support,
  BoldRose98TaggedValues;

const
  INDENTSIZE = 2;

{ TBoldRose98ptyCreator }

procedure TBoldRose98ptyCreator.PutLine(const s: string);
begin
  if s = '' then
    fList.Add('')
  else
    fList.Add(StringOfChar(' ', fLeadingSpaces) + s);
end;

constructor TBoldRose98ptyCreator.Create;
begin
  fList := TStringList.Create;
  InitializeRoseEnums;
  CreateContents;
end;

procedure TBoldRose98ptyCreator.CreateContents;
begin
  PutLine('(object Petal version 40)'); // do not localize
  PutLine;
  PutLine('(list Attribute_Set'); // do not localize
  Indent;
  PutLine('(object Attribute tool "Bold" name "roseId"     value "753117540")'); // do not localize
  PutLine('(object Attribute tool "Bold" name "propertyId" value "809135966")'); // do not localize

  StartSection('Project', 'Model'); // do not localize
  PutLine('## Enum Declarations'); // do not localize
  PutLine;
  PutEnumDefinition('OptimisticLockingSet'); // do not localize
  PutEnumDefinition('NationalCharConversionEnum'); // do not localize

  PutLine('## Enum Declarations'); // do not localize
  PutLine;
  PutLine('## This enum has only one value and is used to ensure the correct PTY-version when importing to BfD'); // do not localize
  PutEnumDefinition('PTYVersionSet'); // do not localize

  PutTaggedValuesForClass('Model'); // do not localize
  PutLine('## removed tagged values'); // do not localize
  PutLine('## (object Attribute tool "Bold" name "DefaultMemberInfoClass" value "")'); // do not localize
  EndSection;

  PutSeparator('Class definitions'); // do not localize
  PutLine('(object Attribute tool "Bold" name "default__Class" value'); // do not localize
  Indent;
  PutLine('(list Attribute_Set'); // do not localize
  Indent;
  PutLine;
  PutLine('## Enum Declarations'); // do not localize
  PutLine;
  PutEnumDefinition('EvolutionStateEnum'); // do not localize
  PutEnumDefinition('TableMappingSet'); // do not localize
  PutEnumDefinition('OptimisticLockingSet'); // do not localize
  PutEnumDefinition(ENUM_TAG_CLASS_STORAGE);
  PutTaggedValuesForClass('Class'); // do not localize
  EndSection;

  StartSection('Attribute', 'Attribute'); // do not localize
  PutLine('## Enum Declarations'); // do not localize
  PutLine;
  PutEnumDefinition('AttributeKindSet'); // do not localize
  PutEnumDefinition('DelphiPropertySet'); // do not localize
  PutEnumDefinition('EvolutionStateEnum'); // do not localize
  PutEnumDefinition(ENUM_TAG_ATTRIBUTE_STORAGE);
  PutTaggedValuesForClass('Attribute'); // do not localize
  PutLine('## removed tagged values'); // do not localize
  PutLine('## (object Attribute tool "Bold" name "MemberInfoClass" value "<Default>")'); // do not localize
  EndSection;

  PutSeparator('Attribute definitions STDUML'); // do not localize
  PutLine(Format('(object Attribute tool "%s" name "roseId"     value "753117540")', [BOLDSTDUMLTOOLNAME])); // do not localize
  PutLine(Format('(object Attribute tool "%s" name "propertyId" value "809135966")', [BOLDSTDUMLTOOLNAME])); // do not localize
  PutLine;
  PutLine;
  PutLine(Format('(object Attribute tool "%s" name "default__Attribute" value', [BOLDSTDUMLTOOLNAME])); // do not localize
  Indent;
  PutLine('(list Attribute_Set'); // do not localize
  PutLine;
  Indent;
  Indent;
  PutLine('## Enum Declarations'); // do not localize
  PutLine;
  PutLine(Format('(object Attribute tool "%s" name	"PersistenceSet" value', [BOLDSTDUMLTOOLNAME])); // do not localize
  Indent;
  PutLine('(list Attribute_Set'); // do not localize
  PutLine(Format('  (object Attribute tool "%s" name "%s" value	0)', [BOLDSTDUMLTOOLNAME, TV_PERSISTENCE_PERSISTENT])); // do not localize
  PutLine(Format('  (object Attribute tool "%s" name "%s"  value	1)', [BOLDSTDUMLTOOLNAME, TV_PERSISTENCE_TRANSIENT])); // do not localize
  EndSection;
  PutLine;
  PutLine(Format('(object Attribute tool "%s" name "%s" value ("PersistenceSet" 0))', [BOLDSTDUMLTOOLNAME, TAG_PERSISTENCE])); // do not localize
  EndSection;

  StartSection('Association', 'Association'); // do not localize
  PutLine('## Enum Declarations'); // do not localize
  PutEnumDefinition('EvolutionStateEnum'); // do not localize
  PutEnumDefinition(ENUM_TAG_ASSOCIATION_STORAGE);
  PutTaggedValuesForClass('Association'); // do not localize
  PutLine('## removed tagged values:'); // do not localize
  PutLine('## (object Attribute tool "Bold" name "LinkClassId"    value -1)'); // do not localize
  EndSection;

  PutSeparator(' Association definitions STDUML'); // do not localize
  Indent;
  PutLine(Format('(object Attribute tool "%s" name "roseId"     value "753117540")', [BOLDSTDUMLTOOLNAME])); // do not localize
  PutLine(Format('(object Attribute tool "%s" name "propertyId" value "809135966")', [BOLDSTDUMLTOOLNAME])); // do not localize
  PutLine;
  PutLine(Format('(object Attribute tool "%s" name "default__Association" value', [BOLDSTDUMLTOOLNAME])); // do not localize
  Indent;
  PutLine('(list Attribute_Set'); // do not localize
  Indent;
  PutLine(Format('(object Attribute tool "%s" name	"PersistenceSet" value', [BOLDSTDUMLTOOLNAME])); // do not localize
  Indent;
  PutLine('(list Attribute_Set'); // do not localize
  PutLine(Format('  (object Attribute tool "%s" name "%s" value	0)', [BOLDSTDUMLTOOLNAME, TV_PERSISTENCE_PERSISTENT])); // do not localize
  PutLine(Format('  (object Attribute tool "%s" name "%s"  value	1)', [BOLDSTDUMLTOOLNAME, TV_PERSISTENCE_TRANSIENT])); // do not localize
  EndSection;
  PutLine;
  PutLine(Format('(object Attribute tool "%s" name "%s" value ("PersistenceSet" 0))', [BOLDSTDUMLTOOLNAME, TAG_PERSISTENCE])); // do not localize
  EndSection;

  StartSection('Role', 'AssociationEnd'); // do not localize
  PutEnumDefinition('DeleteActions'); // do not localize
  PutEnumDefinition('ChangeabilityKind'); // do not localize
  PutEnumDefinition('DefaultRegionModeAssociationEnum'); // do not localize
  PutTaggedValuesForClass('AssociationEnd'); // do not localize

  PutLine('## removed tagged values'); // do not localize
  PutLine('## (object Attribute tool	"Bold" name	"MemberInfoClass" value	"<Default>")'); // do not localize
  EndSection;

  StartSection('Operation', 'Operation'); // do not localize
  PutLine('## Enum Declarations'); // do not localize
  PutLine;
  PutEnumDefinition('BoldOperationKindSet'); // do not localize
  PutTaggedValuesForClass('Operation'); // do not localize
  EndSection;

  PutSeparator('Module definitions'); // do not localize
  PutLine('(object Attribute tool "Bold" name "default__Module-Spec" value'); // do not localize
  Indent;
  PutLine('(list Attribute_Set'); // do not localize
  Indent;
  PutLine('(object Attribute tool "Bold" name "CopyrightNotice" value "")'); // do not localize
  PutLine('(object Attribute tool "Bold" name "FileName"        value "<Name>.inc")'); // do not localize
  EndSection;
  Dedent;
  PutLine(')');
end;

procedure TBoldRose98ptyCreator.DeDent;
begin
  Dec(fLeadingSpaces, INDENTSIZE);
end;

destructor TBoldRose98ptyCreator.Destroy;
begin
  FreeAndNil(fList);
  FreeAndNil(fRoseEnums);
  inherited;
end;

procedure TBoldRose98ptyCreator.Indent;
begin
  Inc(fLeadingSpaces, INDENTSIZE);
end;

procedure TBoldRose98ptyCreator.PutSeparator(const s: string);
begin
  PutLine;
  PutLine(StringOfChar('#', 60));
  PutLine(StringOfChar('#', 04) + '  ' + s);
  PutLine(StringOfChar('#', 60));
  PutLine;
end;

procedure TBoldRose98ptyCreator.PutTaggedValuesForClass(const UMLName: string);
var
  i: integer;
begin
  PutLine(Format('## Tagged values for %s', [UMLName])); // do not localize
  PutLine;
  with BoldDefaultTaggedValueList.ListForClassName[UMLName] do
    for i := 0 to Count - 1 do
      PutRoseAttribute(Definition[i]);
  with Rose98TaggedValueList.ListForClassName[UMLName] do
    for i := 0 to Count - 1 do
      PutRoseAttribute(Definition[i]);
  PutLine;
end;

procedure TBoldRose98ptyCreator.InitializeRoseEnums;
begin
  fRoseEnums := TBoldTaggedValuePerClassList.Create;
  with fRoseEnums.ListForClassName['AttributeKindSet'] do // do not localize
  begin
    Add('Integer', TV_ATTRIBUTEKIND_BOLD,        '0'); // do not localize
    Add('Integer', TV_ATTRIBUTEKIND_DELPHI,      '1'); // do not localize
  end;
  with fRoseEnums.ListForClassName['BoldOperationKindSet'] do // do not localize
  begin
    Add('Integer', TV_DELPHIOPERATIONKIND_NORMAL,          '200'); // do not localize
    Add('Integer', TV_DELPHIOPERATIONKIND_VIRTUAL,         '201'); // do not localize
    Add('Integer', TV_DELPHIOPERATIONKIND_ABSTRACTVIRTUAL, '204'); // do not localize
    Add('Integer', TV_DELPHIOPERATIONKIND_DYNAMIC,         '203'); // do not localize
    Add('Integer', TV_DELPHIOPERATIONKIND_OVERRIDE,        '202'); // do not localize
  end;
  with fRoseEnums.ListForClassName['ChangeabilityKind'] do // do not localize
  begin
    Add('Integer', TV_CHANGEABILITY_ADDONLY,          '0'); // do not localize
    Add('Integer', TV_CHANGEABILITY_CHANGEABLE,       '1'); // do not localize
    Add('Integer', TV_CHANGEABILITY_FROZEN,           '2'); // do not localize
  end;
  with fRoseEnums.ListForClassName['DeleteActions'] do // do not localize
  begin
    Add('Integer', TV_DELETEACTION_DEFAULT,        '0'); // do not localize
    Add('Integer', TV_DELETEACTION_ALLOW,          '1'); // do not localize
    Add('Integer', TV_DELETEACTION_PROHIBIT,       '2'); // do not localize
    Add('Integer', TV_DELETEACTION_CASCADE,        '3'); // do not localize
  end;
  with fRoseEnums.ListForClassName['DelphiPropertySet'] do // do not localize
  begin
    Add('Integer', TV_DPNONE,                   '0'); // do not localize
    Add('Integer', TV_DPFIELD,                  '1'); // do not localize
    Add('Integer', TV_DPPRIVATEMETHOD,          '2'); // do not localize
    Add('Integer', TV_DPPROTECTEDVIRTUALMETHOD, '3'); // do not localize
  end;
  with fRoseEnums.ListForClassName['EvolutionStateEnum'] do // do not localize
  begin
    Add('Integer', TV_EVOLUTIONSTATE_NORMAL,       '0'); // do not localize
    Add('Integer', TV_EVOLUTIONSTATE_TOBEREMOVED,  '1'); // do not localize
    Add('Integer', TV_EVOLUTIONSTATE_REMOVED,      '2'); // do not localize
  end;
  with fRoseEnums.ListForClassName['NationalCharConversionEnum'] do // do not localize
  begin
    Add('Integer', TV_NATIONALCHARCONVERSION_DEFAULT,    '0'); // do not localize
    Add('Integer', TV_NATIONALCHARCONVERSION_TRUE,       '1'); // do not localize
    Add('Integer', TV_NATIONALCHARCONVERSION_FALSE,      '2'); // do not localize
  end;
  with fRoseEnums.ListForClassName['OptimisticLockingSet'] do // do not localize
  begin
    Add('Integer', TV_OPTIMISTICLOCKING_DEFAULT,         '0'); // do not localize
    Add('Integer', TV_OPTIMISTICLOCKING_OFF,             '1'); // do not localize
    Add('Integer', TV_OPTIMISTICLOCKING_MODIFIEDMEMBERS, '2'); // do not localize
    Add('Integer', TV_OPTIMISTICLOCKING_ALLMEMBERS,      '3'); // do not localize
    Add('Integer', TV_OPTIMISTICLOCKING_TIMESTAMP,       '4'); // do not localize
  end;
  with fRoseEnums.ListForClassName['TableMappingSet'] do // do not localize
  begin
    Add('Integer', TV_TABLEMAPPING_OWN,         '0'); // do not localize
    Add('Integer', TV_TABLEMAPPING_PARENT,      '1'); // do not localize
    Add('Integer', TV_TABLEMAPPING_CHILDREN,    '2'); // do not localize
    Add('Integer', TV_TABLEMAPPING_IMPORTED,    '3'); // do not localize
  end;

  with fRoseEnums.ListForClassName['DefaultRegionModeAssociationEnum'] do // do not localize
  begin
    Add('Integer', TV_DEFAULTREGIONMODE_ASSOCIATIONEND_DEFAULT,         '0'); // do not localize
    Add('Integer', TV_DEFAULTREGIONMODE_ASSOCIATIONEND_NONE,      '1'); // do not localize
    Add('Integer', TV_DEFAULTREGIONMODE_ASSOCIATIONEND_EXISTENCE,    '2'); // do not localize
    Add('Integer', TV_DEFAULTREGIONMODE_ASSOCIATIONEND_CASCADE,    '3'); // do not localize
    Add('Integer', TV_DEFAULTREGIONMODE_ASSOCIATIONEND_INDEPENDENTCASCADE,    '4'); // do not localize
  end;

  fRoseEnums.ListForClassName['PTYVersionSet'].Add('Integer', BOLDTVREV, '0'); // do not localize

  with fRoseEnums.ListForClassName[ENUM_TAG_CLASS_STORAGE] do
  begin
    Add('Integer', TV_STORAGE_INTERNAL,         '0'); // do not localize
    Add('Integer', TV_STORAGE_PARTIALLYEXTERNAL,      '1'); // do not localize
    Add('Integer', TV_STORAGE_EXTERNAL,    '2'); // do not localize
  end;

  with fRoseEnums.ListForClassName[ENUM_TAG_ASSOCIATION_STORAGE] do
  begin
    Add('Integer', TV_STORAGE_INTERNAL,         '0'); // do not localize
    Add('Integer', TV_STORAGE_EXTERNAL,    '1'); // do not localize
  end;

  with fRoseEnums.ListForClassName[ENUM_TAG_ATTRIBUTE_STORAGE] do
  begin
    Add('Integer', TV_STORAGE_INTERNAL,         '0'); // do not localize
    Add('Integer', TV_STORAGE_EXTERNAL,    '1'); // do not localize
    Add('Integer', TV_STORAGE_EXTERNALKEY,    '2'); // do not localize
  end;
end;

procedure TBoldRose98ptyCreator.PutRoseAttribute(TaggedValue: TBoldTaggedValueDefinition);
var
  ValueString: string;
begin
  With TaggedValue do
  begin
  if TypeName = 'String' then // do not localize
    ValueString := Format('"%s"', [DefaultValue])
  else if TypeName = 'Boolean' then // do not localize
    ValueString := UpperCase(DefaultValue)
  else if TypeName = 'Integer' then // do not localize
    ValueString := DefaultValue
  else if TypeName = 'Text' then // do not localize
    ValueString := Format('(value Text "%s")', [DefaultValue]) // do not localize
  else
    ValueString := Format('("%s" %s)', [TypeName, LookupRoseEnumValue(TypeName, DefaultValue)]); // do not localize

    PutLine(Format('(object Attribute tool "Bold" name %-33s value %s)', ['"' + Tag + '"', ValueString])); // do not localize
  end;
end;

function TBoldRose98ptyCreator.LookupRoseEnumValue(const EnumName, Value: string): string;
var
  i: integer;
begin
  with fRoseEnums.ListForClassName[EnumName] do
    for i := 0 to Count - 1 do
    begin
      if Definition[i].Tag = Value then
      begin
        Result := Definition[i].DefaultValue;
        Exit;
      end;
    end;
  raise EBoldInternal.CreateFmt('Unknown enum value %s.%s', [EnumName, Value]);
end;

procedure TBoldRose98ptyCreator.PutEnumDefinition(const EnumName: string);
var
  i: integer;
begin
  PutLine(Format('(object Attribute tool "Bold" name "%s" value', [EnumName])); // do not localize
  Indent;
  PutLine('(list Attribute_Set'); // do not localize
  Indent;
  with fRoseEnums.ListForClassName[EnumName] do
    for i := 0 to Count - 1 do
      PutRoseAttribute(Definition[i]);
  EndSection;
  PutLine;
end;

procedure TBoldRose98ptyCreator.EndSection;
begin
  DeDent;
  PutLine(')');
  Dedent;
  PutLine(')');
end;

procedure TBoldRose98ptyCreator.StartSection(const RoseName, UMLName: string);
begin
  putSeparator(Format('%s definitions', [UMLName])); // do not localize
  PutLine(Format('(object Attribute tool "Bold" name "default__%s" value', [RoseName])); // do not localize
  Indent;
  PutLine('(list Attribute_Set'); // do not localize
  Indent;
  PutLine;
end;

procedure TBoldRose98ptyCreator.PutLine;
begin
  PutLine('');
end;

end.
