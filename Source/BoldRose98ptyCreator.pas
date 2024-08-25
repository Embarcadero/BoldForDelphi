
{ Global compiler directives }
{$include bold.inc}
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
    fRoseEnums: TBoldTaggedValuePerClassList;
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
  PutLine('(object Petal version 40)');
  PutLine;
  PutLine('(list Attribute_Set');
  Indent;
  PutLine('(object Attribute tool "Bold" name "roseId"     value "753117540")');
  PutLine('(object Attribute tool "Bold" name "propertyId" value "809135966")');

  StartSection('Project', 'Model');
  PutLine('## Enum Declarations');
  PutLine;
  PutEnumDefinition('OptimisticLockingSet');
  PutEnumDefinition('NationalCharConversionEnum');

  PutLine('## Enum Declarations');
  PutLine;
  PutLine('## This enum has only one value and is used to ensure the correct PTY-version when importing to BfD');
  PutEnumDefinition('PTYVersionSet');

  PutTaggedValuesForClass('Model');
  PutLine('## removed tagged values');
  PutLine('## (object Attribute tool "Bold" name "DefaultMemberInfoClass" value "")');
  EndSection;

  PutSeparator('Class definitions');
  PutLine('(object Attribute tool "Bold" name "default__Class" value');
  Indent;
  PutLine('(list Attribute_Set');
  Indent;
  PutLine;
  PutLine('## Enum Declarations');
  PutLine;
  PutEnumDefinition('EvolutionStateEnum');
  PutEnumDefinition('TableMappingSet');
  PutEnumDefinition('OptimisticLockingSet');
  PutEnumDefinition(ENUM_TAG_CLASS_STORAGE);
  PutTaggedValuesForClass('Class');
  EndSection;

  StartSection('Attribute', 'Attribute');
  PutLine('## Enum Declarations');
  PutLine;
  PutEnumDefinition('AttributeKindSet');
  PutEnumDefinition('DelphiPropertySet');
  PutEnumDefinition('EvolutionStateEnum');
  PutEnumDefinition(ENUM_TAG_ATTRIBUTE_STORAGE);
  PutTaggedValuesForClass('Attribute');
  PutLine('## removed tagged values');
  PutLine('## (object Attribute tool "Bold" name "MemberInfoClass" value "<Default>")');
  EndSection;

  PutSeparator('Attribute definitions STDUML');
  PutLine(Format('(object Attribute tool "%s" name "roseId"     value "753117540")', [BOLDSTDUMLTOOLNAME]));
  PutLine(Format('(object Attribute tool "%s" name "propertyId" value "809135966")', [BOLDSTDUMLTOOLNAME]));
  PutLine;
  PutLine;
  PutLine(Format('(object Attribute tool "%s" name "default__Attribute" value', [BOLDSTDUMLTOOLNAME]));
  Indent;
  PutLine('(list Attribute_Set');
  PutLine;
  Indent;
  Indent;
  PutLine('## Enum Declarations');
  PutLine;
  PutLine(Format('(object Attribute tool "%s" name	"PersistenceSet" value', [BOLDSTDUMLTOOLNAME]));
  Indent;
  PutLine('(list Attribute_Set');
  PutLine(Format('  (object Attribute tool "%s" name "%s" value	0)', [BOLDSTDUMLTOOLNAME, TV_PERSISTENCE_PERSISTENT]));
  PutLine(Format('  (object Attribute tool "%s" name "%s"  value	1)', [BOLDSTDUMLTOOLNAME, TV_PERSISTENCE_TRANSIENT]));
  EndSection;
  PutLine;
  PutLine(Format('(object Attribute tool "%s" name "%s" value ("PersistenceSet" 0))', [BOLDSTDUMLTOOLNAME, TAG_PERSISTENCE]));
  EndSection;

  StartSection('Association', 'Association');
  PutLine('## Enum Declarations');
  PutEnumDefinition('EvolutionStateEnum');
  PutEnumDefinition(ENUM_TAG_ASSOCIATION_STORAGE);
  PutTaggedValuesForClass('Association');
  PutLine('## removed tagged values:');
  PutLine('## (object Attribute tool "Bold" name "LinkClassId"    value -1)');
  EndSection;

  PutSeparator(' Association definitions STDUML');
  Indent;
  PutLine(Format('(object Attribute tool "%s" name "roseId"     value "753117540")', [BOLDSTDUMLTOOLNAME]));
  PutLine(Format('(object Attribute tool "%s" name "propertyId" value "809135966")', [BOLDSTDUMLTOOLNAME]));
  PutLine;
  PutLine(Format('(object Attribute tool "%s" name "default__Association" value', [BOLDSTDUMLTOOLNAME]));
  Indent;
  PutLine('(list Attribute_Set');
  Indent;
 PutLine(Format('(object Attribute tool "%s" name	"PersistenceSet" value', [BOLDSTDUMLTOOLNAME]));
  Indent;
  PutLine('(list Attribute_Set');
  PutLine(Format('  (object Attribute tool "%s" name "%s" value	0)', [BOLDSTDUMLTOOLNAME, TV_PERSISTENCE_PERSISTENT]));
  PutLine(Format('  (object Attribute tool "%s" name "%s"  value	1)', [BOLDSTDUMLTOOLNAME, TV_PERSISTENCE_TRANSIENT]));
  EndSection;
  PutLine;
  PutLine(Format('(object Attribute tool "%s" name "%s" value ("PersistenceSet" 0))', [BOLDSTDUMLTOOLNAME, TAG_PERSISTENCE]));
  EndSection;

  StartSection('Role', 'AssociationEnd');
  PutEnumDefinition('DeleteActions');
  PutEnumDefinition('ChangeabilityKind');
  PutEnumDefinition('DefaultRegionModeAssociationEnum');
  PutTaggedValuesForClass('AssociationEnd');

  PutLine('## removed tagged values');
  PutLine('## (object Attribute tool	"Bold" name	"MemberInfoClass" value	"<Default>")');
  EndSection;

  StartSection('Operation', 'Operation');
  PutLine('## Enum Declarations');
  PutLine;
  PutEnumDefinition('BoldOperationKindSet');
  PutTaggedValuesForClass('Operation');
  EndSection;

  PutSeparator('Module definitions');
  PutLine('(object Attribute tool "Bold" name "default__Module-Spec" value');
  Indent;
  PutLine('(list Attribute_Set');
  Indent;
  PutLine('(object Attribute tool "Bold" name "CopyrightNotice" value "")');
  PutLine('(object Attribute tool "Bold" name "FileName"        value "<Name>.inc")');
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
  PutLine(Format('## Tagged values for %s', [UMLName]));
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
  with fRoseEnums.ListForClassName['AttributeKindSet'] do
  begin
    Add('Integer', TV_ATTRIBUTEKIND_BOLD,        '0');
    Add('Integer', TV_ATTRIBUTEKIND_DELPHI,      '1');
  end;
  with fRoseEnums.ListForClassName['BoldOperationKindSet'] do
  begin
    Add('Integer', TV_DELPHIOPERATIONKIND_NORMAL,          '200');
    Add('Integer', TV_DELPHIOPERATIONKIND_VIRTUAL,         '201');
    Add('Integer', TV_DELPHIOPERATIONKIND_ABSTRACTVIRTUAL, '204');
    Add('Integer', TV_DELPHIOPERATIONKIND_DYNAMIC,         '203');
    Add('Integer', TV_DELPHIOPERATIONKIND_OVERRIDE,        '202');
  end;
  with fRoseEnums.ListForClassName['ChangeabilityKind'] do
  begin
    Add('Integer', TV_CHANGEABILITY_ADDONLY,          '0');
    Add('Integer', TV_CHANGEABILITY_CHANGEABLE,       '1');
    Add('Integer', TV_CHANGEABILITY_FROZEN,           '2');
  end;
  with fRoseEnums.ListForClassName['DeleteActions'] do
  begin
    Add('Integer', TV_DELETEACTION_DEFAULT,        '0');
    Add('Integer', TV_DELETEACTION_ALLOW,          '1');
    Add('Integer', TV_DELETEACTION_PROHIBIT,       '2');
    Add('Integer', TV_DELETEACTION_CASCADE,        '3');
  end;
  with fRoseEnums.ListForClassName['DelphiPropertySet'] do
  begin
    Add('Integer', TV_DPNONE,                   '0');
    Add('Integer', TV_DPFIELD,                  '1');
    Add('Integer', TV_DPPRIVATEMETHOD,          '2');
    Add('Integer', TV_DPPROTECTEDVIRTUALMETHOD, '3');
  end;
  with fRoseEnums.ListForClassName['EvolutionStateEnum'] do
  begin
    Add('Integer', TV_EVOLUTIONSTATE_NORMAL,       '0');
    Add('Integer', TV_EVOLUTIONSTATE_TOBEREMOVED,  '1');
    Add('Integer', TV_EVOLUTIONSTATE_REMOVED,      '2');
  end;
  with fRoseEnums.ListForClassName['NationalCharConversionEnum'] do
  begin
    Add('Integer', TV_NATIONALCHARCONVERSION_DEFAULT,    '0');
    Add('Integer', TV_NATIONALCHARCONVERSION_TRUE,       '1');
    Add('Integer', TV_NATIONALCHARCONVERSION_FALSE,      '2');
  end;
  with fRoseEnums.ListForClassName['OptimisticLockingSet'] do
  begin
    Add('Integer', TV_OPTIMISTICLOCKING_DEFAULT,         '0');
    Add('Integer', TV_OPTIMISTICLOCKING_OFF,             '1');
    Add('Integer', TV_OPTIMISTICLOCKING_MODIFIEDMEMBERS, '2');
    Add('Integer', TV_OPTIMISTICLOCKING_ALLMEMBERS,      '3');
    Add('Integer', TV_OPTIMISTICLOCKING_TIMESTAMP,       '4');
  end;
  with fRoseEnums.ListForClassName['TableMappingSet'] do
  begin
    Add('Integer', TV_TABLEMAPPING_OWN,         '0');
    Add('Integer', TV_TABLEMAPPING_PARENT,      '1');
    Add('Integer', TV_TABLEMAPPING_CHILDREN,    '2');
    Add('Integer', TV_TABLEMAPPING_IMPORTED,    '3');
  end;

  with fRoseEnums.ListForClassName['DefaultRegionModeAssociationEnum'] do
  begin
    Add('Integer', TV_DEFAULTREGIONMODE_ASSOCIATIONEND_DEFAULT,         '0');
    Add('Integer', TV_DEFAULTREGIONMODE_ASSOCIATIONEND_NONE,      '1');
    Add('Integer', TV_DEFAULTREGIONMODE_ASSOCIATIONEND_EXISTENCE,    '2');
    Add('Integer', TV_DEFAULTREGIONMODE_ASSOCIATIONEND_CASCADE,    '3');
    Add('Integer', TV_DEFAULTREGIONMODE_ASSOCIATIONEND_INDEPENDENTCASCADE,    '4');
  end;

  fRoseEnums.ListForClassName['PTYVersionSet'].Add('Integer', BOLDTVREV, '0');

  with fRoseEnums.ListForClassName[ENUM_TAG_CLASS_STORAGE] do
  begin
    Add('Integer', TV_STORAGE_INTERNAL,         '0');
    Add('Integer', TV_STORAGE_PARTIALLYEXTERNAL,      '1');
    Add('Integer', TV_STORAGE_EXTERNAL,    '2');
  end;

  with fRoseEnums.ListForClassName[ENUM_TAG_ASSOCIATION_STORAGE] do
  begin
    Add('Integer', TV_STORAGE_INTERNAL,         '0');
    Add('Integer', TV_STORAGE_EXTERNAL,    '1');
  end;

  with fRoseEnums.ListForClassName[ENUM_TAG_ATTRIBUTE_STORAGE] do
  begin
    Add('Integer', TV_STORAGE_INTERNAL,         '0');
    Add('Integer', TV_STORAGE_EXTERNAL,    '1');
    Add('Integer', TV_STORAGE_EXTERNALKEY,    '2');
  end;
end;

procedure TBoldRose98ptyCreator.PutRoseAttribute(TaggedValue: TBoldTaggedValueDefinition);
var
  ValueString: string;
begin
  With TaggedValue do
  begin
  if TypeName = 'String' then
    ValueString := Format('"%s"', [DefaultValue])
  else if TypeName = 'Boolean' then
    ValueString := UpperCase(DefaultValue)
  else if TypeName = 'Integer' then
    ValueString := DefaultValue
  else if TypeName = 'Text' then
    ValueString := Format('(value Text "%s")', [DefaultValue])
  else
    ValueString := Format('("%s" %s)', [TypeName, LookupRoseEnumValue(TypeName, DefaultValue)]);

    PutLine(Format('(object Attribute tool "Bold" name %-33s value %s)', ['"' + Tag + '"', ValueString]));
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
  PutLine(Format('(object Attribute tool "Bold" name "%s" value', [EnumName]));
  Indent;
  PutLine('(list Attribute_Set');
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
  putSeparator(Format('%s definitions', [UMLName]));
  PutLine(Format('(object Attribute tool "Bold" name "default__%s" value', [RoseName]));
  Indent;
  PutLine('(list Attribute_Set');
  Indent;
  PutLine;
end;

procedure TBoldRose98ptyCreator.PutLine;
begin
  PutLine('');
end;

end.
