{ Global compiler directives }
{$include bold.inc}
unit BoldDefaultTaggedValues;

interface

uses
  BoldDefs,
  BoldTaggedValueList;

const
  BOLDTVREV_MAJOR = '6';
  BOLDTVREV_MINOR = '4';
  BOLDTVREV = BOLDTVREV_MAJOR + '.' + BOLDTVREV_MINOR;  


  {Genernal constants}
  TV_TRUE: string = 'True';
  TV_FALSE: string = 'False';
  TV_NAME: string = '<Name>';
  TV_NAME_UPPERCASE: string = '<NAME>';
  TV_NAME_Length = 6;

  TAG_ATTRIBUTEKIND: String = 'AttributeKind';
  TV_ATTRIBUTEKIND_BOLD: String = 'Bold';
  TV_ATTRIBUTEKIND_DELPHI: String = 'Delphi';
  TAG_DELETEACTION: String = 'DeleteAction';
  TV_DELETEACTION_DEFAULT: String = DEFAULTNAMELITERAL;
  TV_DELETEACTION_ALLOW: String = 'Allow';
  TV_DELETEACTION_PROHIBIT: String = 'Prohibit';
  TV_DELETEACTION_CASCADE: String = 'Cascade';
  TAG_DPREAD: String = 'DelphiPropertyRead';
  TAG_DPWRITE: String = 'DelphiPropertyWrite';
  TV_DPNONE: String = 'None';
  TV_DPFIELD: String = 'Field';
  TV_DPPRIVATEMETHOD: String = 'PrivateMethod';
  TV_DPPROTECTEDVIRTUALMETHOD: String = 'ProtectedVirtualMethod';
  TAG_EVOLUTIONSTATE: String = 'EvolutionState';
  TV_EVOLUTIONSTATE_NORMAL: String = 'Normal';
  TV_EVOLUTIONSTATE_TOBEREMOVED: String = 'ToBeRemoved';
  TV_EVOLUTIONSTATE_REMOVED: String = 'Removed';
  TAG_NATIONALCHARCONVERSION: String = 'NationalCharConversion';
  TV_NATIONALCHARCONVERSION_DEFAULT: String = DEFAULTNAMELITERAL;
  TV_NATIONALCHARCONVERSION_TRUE: String = 'True';
  TV_NATIONALCHARCONVERSION_FALSE: String = 'False';
  TAG_DELPHIOPERATIONKIND: String = 'OperationKind';
  TV_DELPHIOPERATIONKIND_NORMAL: String = 'Common';
  TV_DELPHIOPERATIONKIND_VIRTUAL: String = 'Virtual';
  TV_DELPHIOPERATIONKIND_OVERRIDE: String = 'Override';
  TV_DELPHIOPERATIONKIND_DYNAMIC: String = 'Dynamic';
  TV_DELPHIOPERATIONKIND_ABSTRACTVIRTUAL: String = 'Abstract';
  TAG_OPTIMISTICLOCKING: String = 'OptimisticLocking';
  TV_OPTIMISTICLOCKING_DEFAULT: String = DEFAULTNAMELITERAL;
  TV_OPTIMISTICLOCKING_OFF: String = 'Off';
  TV_OPTIMISTICLOCKING_MODIFIEDMEMBERS: String = 'ModifiedMembers';
  TV_OPTIMISTICLOCKING_ALLMEMBERS: String = 'AllMembers';
  TV_OPTIMISTICLOCKING_TIMESTAMP: String = 'TimeStamp';
  TV_OPTIMISTICLOCKING_MODIFIEDMEMBERS_OLDNAME: String = 'Member';
  TV_OPTIMISTICLOCKING_ALLMEMBERS_OLDNAME: String = 'Class';
  TAG_TABLEMAPPING: String = 'TableMapping';
  TV_TABLEMAPPING_OWN: String = 'Own';
  TV_TABLEMAPPING_PARENT: String = 'Parent';
  TV_TABLEMAPPING_CHILDREN: String = 'Children';
  TV_TABLEMAPPING_IMPORTED: String = 'Imported';
  DEFAULTTABLEMAPPINGSTRING = 'Own';

  TAG_STORAGE: String = 'Storage';
  ENUM_TAG_CLASS_STORAGE: string = 'ClassStorageEnum';
  ENUM_TAG_ATTRIBUTE_STORAGE: string = 'AttributeStorageEnum';
  ENUM_TAG_ASSOCIATION_STORAGE: string = 'AssociationStorageEnum';
  TV_STORAGE_INTERNAL: String = 'Internal';
  TV_STORAGE_PARTIALLYEXTERNAL: String = 'PartiallyExternal';
  TV_STORAGE_EXTERNAL: String = 'External';
  TV_STORAGE_EXTERNALKEY: String = 'ExternalKey';


  TV_PERSISTENT_OLD: String = 'Persistent';

  TAG_ALLOWNULL: String = 'AllowNULL';

  TAG_COLUMNNAME: String = 'ColumnName';
  TAG_COPYRIGHTNOTICE: String = 'CopyrightNotice';

  TAG_DEFAULTDBVALUE: String = 'DefaultDBValue';
  TAG_DEFAULTSUPERCLASS: String = 'DefaultSuperClass';
  TAG_DEFAULTLINKCLASSSUPERCLASS: String = 'DefaultLinkClassSuperClass';
  TAG_DEFAULTSTRINGREPRESENTATION: String = 'DefaultStringRepresentation';
  TAG_DELAYEDFETCH: String = 'DelayedFetch';
  TAG_DELPHIFIELD: String = 'DelphiField';
  TAG_DELPHINAME: String = 'DelphiName';
  TAG_CPPNAME: String = 'CPPName';
  TAG_DERIVATIONEXPRESSIONS: String = 'DerivationExpressions';
  TAG_DERIVATIONOCL: String = 'DerivationOCL';

  TAG_EMBED: String = 'Embed';
  TAG_EXPRESSIONNAME: String = 'ExpressionName';

  TAG_FORMERNAMES: String = 'FormerNames';

  TAG_GENERATEMULTIPLICITYCONSTRAINTS: String = 'GenerateMultiplicityConstraints';
  TAG_GUID: String = 'GUID';

  TAG_IMPORTED: String = 'Imported';
  TAG_IMPLEMENTATIONUSES: String = 'ImplementationUses';
  TAG_INCFILENAME: String = 'FileName';
  TAG_INTERFACENAME = 'InterfaceName';
  TAG_INTERFACEUSES: String = 'InterfaceUses';

  TAG_ISCONST: String = 'IsConst';

  TAG_LENGTH: String = 'Length';
  TAG_LINKCLASSNAME: String = 'LinkClassName';
  TAG_LISTGUID = 'LISTGUID';

  TAG_MODELVERSION: String = 'ModelVersion';

  TAG_ORDERED: String = 'Ordered';
  TAG_OVERRIDEINALLSUBCLASSES: String = 'OverrideInAllSubclasses';

  TAG_PMAPPERNAME: String = 'PMapper';

  TAG_REVERSEDERIVE: String = 'ReverseDerive';
  TAG_ROOTCLASS: String = 'RootClass';

  TAG_TABLENAME: String = 'TableName';
  TAG_EXTERNALTABLENAME: String = 'ExternalTableName';
  TAG_TYPELIBVERSION = 'TypeLibVersion';

  TAG_UNITNAME: String = 'UnitName';
  TAG_UPDATEWHOLEOBJECTS = 'UpdateWholeObjects';
  TAG_USECLOCKLOG: String = 'UseClockLog';
  TAG_USEGLOBALID: String = 'UseGlobalId';
  TAG_USEMODELVERSION: String = 'UseModelVersion';
  TAG_USEREADONLY: String = 'UseReadOnly';
  TAG_USETIMESTAMP: String = 'UseTimestamp';
  TAG_USEXFILES: String = 'UseXFiles';

  TAG_VERSIONED: string = 'Versioned';
  TAG_VIRTUALDERIVE: String = 'VirtualDerive';

  TAG_REGIONDEFINITIONS: string = 'RegionDefinitions';
  TAG_GENERATEDEFAULTREGIONS: string = 'GenerateDefaultRegions';

  TAG_GENERATEDEFAULTREGION_CLASS: string = 'GenerateDefaultRegion';

  TAG_DEFAULTREGIONMODE_ASSOCIATIONEND: string = 'DefaultRegionMode';
  TV_DEFAULTREGIONMODE_ASSOCIATIONEND_DEFAULT: string = 'Default';
  TV_DEFAULTREGIONMODE_ASSOCIATIONEND_NONE: string = 'None';
  TV_DEFAULTREGIONMODE_ASSOCIATIONEND_EXISTENCE: string = 'Existence';
  TV_DEFAULTREGIONMODE_ASSOCIATIONEND_INDEPENDENTCASCADE: string = 'IndependentCascade';
  TV_DEFAULTREGIONMODE_ASSOCIATIONEND_CASCADE: string = 'Cascade';

  BOLDINTERALTVPREFIX = '_BoldInternal.';
  TV_MODELERRORS = 'ModelErrors';
  BOLDBOLDIFYPREFIX = '_Boldify.';
  TAG_BOLDIFIED = 'boldified';
  TAG_AUTOCREATED = 'autoCreated';
  TAG_DEFAULTMULTIPLICITY = 'defaultMultiplicity';
  TAG_NONAME = 'noName';
  TAG_WASEMBEDED = 'wasEmbeded';
  TAG_WASPERSISTENT = 'wasPersistent';
  TAG_UNFLATTENEDNAMESPACE = 'unflattenedNamespace';


function BoldDefaultTaggedValueList: TBoldTaggedValuePerClassList;
function TVIsTrue(value: string): Boolean;
function TVIsFalse(value: string): Boolean;

implementation

uses
  SysUtils;

var
 G_BoldDefaultTaggedValues: TBoldTaggedValuePerClassList = nil;

procedure AddDefaultTaggedValues;
begin
  with G_BoldDefaultTaggedValues.ListForClassName['Model'] do
  begin
    Add('Boolean',  TAG_GENERATEMULTIPLICITYCONSTRAINTS, TV_TRUE);
    Add('Text',     TAG_INTERFACEUSES,                   '');
    Add('Text',     TAG_IMPLEMENTATIONUSES,              '');
    Add('Text',     TAG_COPYRIGHTNOTICE,                 '');
    Add('String',   TAG_DEFAULTSUPERCLASS,               '');
    Add('String',   TAG_DEFAULTLINKCLASSSUPERCLASS,      '');
    Add('String',   TAG_UNITNAME,                        'BusinessClasses');
    Add('String',   TAG_ROOTCLASS,                       '');
    Add('String',   TAG_PMAPPERNAME,                     DEFAULTNAMELITERAL);
    Add('Boolean',  TAG_USEXFILES,                       TV_TRUE);
    Add('Boolean',  TAG_USETIMESTAMP,                    TV_TRUE);
    Add('Boolean',  TAG_USEGLOBALID,                     TV_TRUE);
    Add('Boolean',  TAG_USEREADONLY,                     TV_TRUE);
    Add('Boolean',  TAG_USEMODELVERSION,                 TV_FALSE);
    Add('Integer',  TAG_MODELVERSION,                    '0');
    Add('Boolean',  TAG_USECLOCKLOG,                     TV_TRUE);
    Add('Boolean',  TAG_UPDATEWHOLEOBJECTS,              TV_FALSE);
    Add('OptimisticLockingSet',
                    TAG_OPTIMISTICLOCKING,               TV_OPTIMISTICLOCKING_OFF);
    Add('NationalCharConversionEnum',
                    TAG_NATIONALCHARCONVERSION,          TV_NATIONALCHARCONVERSION_DEFAULT);
    Add('String',   TAG_GUID,                            '');
    Add('String',   TAG_TYPELIBVERSION,                  '1.0');
    Add('Text',     TAG_REGIONDEFINITIONS,               '');
    Add('Boolean',  TAG_GENERATEDEFAULTREGIONS,          TV_FALSE);
  end;
  with G_BoldDefaultTaggedValues.ListForClassName['Class'] do
  begin
    Add('String',   TAG_INCFILENAME,                    '');
    Add('String',   TAG_UNITNAME,                       '');
    Add('Boolean',  TAG_IMPORTED,                       TV_FALSE);
    Add('TableMappingSet',
                    TAG_TABLEMAPPING,                   TV_TABLEMAPPING_OWN);
    Add('String',   TAG_DELPHINAME,                     'T<Name>');
    Add('String',   TAG_CPPNAME,                        TV_NAME);
    Add('String',   TAG_EXPRESSIONNAME,                 TV_NAME);
    Add('String',   TAG_TABLENAME,                      TV_NAME);
    Add('String',   TAG_EXTERNALTABLENAME,              TV_NAME);
    Add('EvolutionStateEnum',
                    TAG_EVOLUTIONSTATE,                 TV_EVOLUTIONSTATE_NORMAL);
    Add('String',   TAG_PMAPPERNAME,                    DEFAULTNAMELITERAL);
    Add('String',   TAG_DEFAULTSTRINGREPRESENTATION,    '');
    Add('Text',     TAG_DERIVATIONEXPRESSIONS,          '');
    Add('Boolean',  TAG_VERSIONED,                      TV_FALSE);
    Add('OptimisticLockingSet',
                    TAG_OPTIMISTICLOCKING,              TV_OPTIMISTICLOCKING_DEFAULT);
    Add('Text',   TAG_FORMERNAMES,                    '');
    Add('String',   TAG_INTERFACENAME,                  'I<Name>');
    Add('String',   TAG_GUID,                           '');
    Add('Boolean',
                    TAG_GENERATEDEFAULTREGION_CLASS,    TV_TRUE);
    Add(ENUM_TAG_CLASS_STORAGE,
                    TAG_STORAGE,                        TV_STORAGE_INTERNAL);
  end;
  with G_BoldDefaultTaggedValues.ListForClassName['Association'] do
  begin
    Add('String', TAG_LINKCLASSNAME,               TV_NAME);
    Add('Text', TAG_FORMERNAMES,                 '');
    Add('EvolutionStateEnum',
                  TAG_EVOLUTIONSTATE,              TV_EVOLUTIONSTATE_NORMAL);
    Add(ENUM_TAG_ASSOCIATION_STORAGE,
                    TAG_STORAGE,                        TV_STORAGE_INTERNAL);
  end;
  with G_BoldDefaultTaggedValues.ListForClassName['Attribute'] do
  begin
    Add('Integer', TAG_LENGTH,                     '255');
    Add('Boolean', TAG_ALLOWNULL,                  TV_FALSE);
    Add('Boolean', TAG_DELAYEDFETCH,               TV_FALSE);
    Add('String',  TAG_COLUMNNAME,                 TV_NAME);
    Add('String',  TAG_EXPRESSIONNAME,             TV_NAME);
    Add('String',  TAG_DELPHINAME,                 TV_NAME);
    Add('String',  TAG_CPPNAME,                    TV_NAME);
    Add('String',  TAG_PMAPPERNAME,                DEFAULTNAMELITERAL);

    Add('Text',    TAG_DERIVATIONOCL,              '');
    Add('Boolean', TAG_VIRTUALDERIVE,              TV_TRUE);
    Add('Boolean', TAG_REVERSEDERIVE,              TV_FALSE);
    Add('AttributeKindSet',
                   TAG_ATTRIBUTEKIND,              TV_ATTRIBUTEKIND_BOLD);
    Add('Boolean', TAG_DELPHIFIELD,                TV_FALSE);
    Add('DelphiPropertySet',
                   TAG_DPREAD,                     TV_DPNONE);
    Add('DelphiPropertySet',
                   TAG_DPWRITE,                    TV_DPNONE);

    Add('EvolutionStateEnum',
                   TAG_EVOLUTIONSTATE,            TV_EVOLUTIONSTATE_NORMAL);
    Add('Text',  TAG_FORMERNAMES,               '');
    Add('String',  TAG_DEFAULTDBVALUE,            '');
    Add(ENUM_TAG_ATTRIBUTE_STORAGE,
                    TAG_STORAGE,                        TV_STORAGE_INTERNAL);

  end;
  with G_BoldDefaultTaggedValues.ListForClassName['AssociationEnd'] do
  begin
    Add('Boolean', TAG_ORDERED,                    TV_FALSE);
    Add('String',  TAG_COLUMNNAME,                 TV_NAME);
    Add('String',  TAG_EXPRESSIONNAME,             TV_NAME);
    Add('String',  TAG_DELPHINAME,                 TV_NAME);
    Add('String',  TAG_CPPNAME,                    TV_NAME);
    Add('Boolean', TAG_EMBED,                      TV_TRUE);
    Add('Text',    TAG_DERIVATIONOCL,              '');
    Add('Boolean', TAG_VIRTUALDERIVE,              TV_TRUE);
    Add('DeleteActions',
                   TAG_DELETEACTION,               TV_DELETEACTION_DEFAULT);

    Add('Text',    TAG_FORMERNAMES,                '');
    Add('DefaultRegionModeAssociationEnum',
                  TAG_DEFAULTREGIONMODE_ASSOCIATIONEND, TV_DEFAULTREGIONMODE_ASSOCIATIONEND_DEFAULT);
 end;
  with G_BoldDefaultTaggedValues.ListForClassName['Operation'] do
  begin
    Add('String',  TAG_DELPHINAME,                     TV_NAME);
    Add('String',  TAG_CPPNAME,                        TV_NAME);
    Add('String',  TAG_EXPRESSIONNAME,                 TV_NAME);
    Add('BoldOperationKindSet',
                   TAG_DELPHIOPERATIONKIND,            TV_DELPHIOPERATIONKIND_NORMAL);
    Add('Boolean', TAG_OVERRIDEINALLSUBCLASSES,        TV_FALSE);
  end;

  with G_BoldDefaultTaggedValues.ListForClassName['Parameter'] do
  begin
    Add('Boolean', TAG_ISCONST,                        TV_FALSE);
  end;
end;

function BoldDefaultTaggedValueList: TBoldTaggedValuePerClassList;
begin
  if not Assigned(G_BoldDefaultTaggedValues) then
  begin
    G_BoldDefaultTaggedValues := TBoldTaggedValuePerClassList.Create;
    AddDefaultTaggedValues;
  end;
  Result := G_BoldDefaultTaggedValues
end;

function TVIsTrue(value: string): Boolean;
begin
  result := SameText(Value, TV_TRUE);
end;

function TVIsFalse(value: string): Boolean;
begin
  result := SameText(Value, TV_FALSE);
end;

initialization

finalization
  FreeAndNil(G_BoldDefaultTaggedValues);

end.