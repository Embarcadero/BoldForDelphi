unit BoldDefaultTaggedValues;

interface

uses
  BoldDefs,
  BoldTaggedValueList;

const
  BOLDTVREV_MAJOR = '6'; // Increase when changing the default of Tagged values
  BOLDTVREV_MINOR = '4'; // Increase when adding or removing Tagged values
  BOLDTVREV = BOLDTVREV_MAJOR + '.' + BOLDTVREV_MINOR;
       { TODO 1 : is changeability bold or stdvalue }
       { TODO 1 : Should the common UML-TV:s be broken out? }


  {Genernal constants}
  TV_TRUE: string = 'True';
  TV_FALSE: string = 'False';
  TV_NAME: string = '<Name>';
  TV_NAME_UPPERCASE: string = '<NAME>';
  TV_NAME_Length = 6;

const
  DEFAULTVALUE = 'default';

  // Tagged values of enum type

  // AttributeKind
  TAG_ATTRIBUTEKIND: String = 'AttributeKind';  // used to be stereotype
  TV_ATTRIBUTEKIND_BOLD: String = 'Bold';
  TV_ATTRIBUTEKIND_DELPHI: String = 'Delphi';


  // DeleteAction
  TAG_DELETEACTION: String = 'DeleteAction';
  TV_DELETEACTION_DEFAULT: String = DEFAULTNAMELITERAL;
  TV_DELETEACTION_ALLOW: String = 'Allow';
  TV_DELETEACTION_PROHIBIT: String = 'Prohibit';
  TV_DELETEACTION_CASCADE: String = 'Cascade';

  // DelphiPropertyRead/DelphiPropertyWrite
  TAG_DPREAD: String = 'DelphiPropertyRead';
  TAG_DPWRITE: String = 'DelphiPropertyWrite';
  TV_DPNONE: String = 'None'; // FIXME, add underscore
  TV_DPFIELD: String = 'Field';
  TV_DPPRIVATEMETHOD: String = 'PrivateMethod';
  TV_DPPROTECTEDVIRTUALMETHOD: String = 'ProtectedVirtualMethod';

  // EvolutionState
  TAG_EVOLUTIONSTATE: String = 'EvolutionState';
  TV_EVOLUTIONSTATE_NORMAL: String = 'Normal';
  TV_EVOLUTIONSTATE_TOBEREMOVED: String = 'ToBeRemoved';
  TV_EVOLUTIONSTATE_REMOVED: String = 'Removed';

  // NatinalCharConversion
  TAG_NATIONALCHARCONVERSION: String = 'NationalCharConversion';
  TV_NATIONALCHARCONVERSION_DEFAULT: String = DEFAULTNAMELITERAL;
  TV_NATIONALCHARCONVERSION_TRUE: String = 'True';
  TV_NATIONALCHARCONVERSION_FALSE: String = 'False';

  //  OperationKind
  TAG_DELPHIOPERATIONKIND: String = 'OperationKind';
  TV_DELPHIOPERATIONKIND_NORMAL: String = 'Common'; // not 'Normal' for backwards compatibility reasons
  TV_DELPHIOPERATIONKIND_VIRTUAL: String = 'Virtual';
  TV_DELPHIOPERATIONKIND_OVERRIDE: String = 'Override';
  TV_DELPHIOPERATIONKIND_DYNAMIC: String = 'Dynamic';
  TV_DELPHIOPERATIONKIND_ABSTRACTVIRTUAL: String = 'Abstract';

  // OptimisticLocking
  TAG_OPTIMISTICLOCKING: String = 'OptimisticLocking';
  TV_OPTIMISTICLOCKING_DEFAULT: String = DEFAULTNAMELITERAL;
  TV_OPTIMISTICLOCKING_OFF: String = 'Off';
  TV_OPTIMISTICLOCKING_MODIFIEDMEMBERS: String = 'ModifiedMembers';
  TV_OPTIMISTICLOCKING_ALLMEMBERS: String = 'AllMembers';
  TV_OPTIMISTICLOCKING_TIMESTAMP: String = 'TimeStamp';

  // deprecated names for Optimistic Locking tagged values
  TV_OPTIMISTICLOCKING_MODIFIEDMEMBERS_OLDNAME: String = 'Member';
  TV_OPTIMISTICLOCKING_ALLMEMBERS_OLDNAME: String = 'Class';


   // TableMapping
  TAG_TABLEMAPPING: String = 'TableMapping';
  TV_TABLEMAPPING_OWN: String = 'Own';
  TV_TABLEMAPPING_PARENT: String = 'Parent';
  TV_TABLEMAPPING_CHILDREN: String = 'Children';
  TV_TABLEMAPPING_IMPORTED: String = 'Imported';
  DEFAULTTABLEMAPPINGSTRING = 'Own'; //FIXME!!

  // Storage
  TAG_STORAGE: String = 'Storage';
  ENUM_TAG_CLASS_STORAGE: string = 'ClassStorageEnum';
  ENUM_TAG_ATTRIBUTE_STORAGE: string = 'AttributeStorageEnum';
  ENUM_TAG_ASSOCIATION_STORAGE: string = 'AssociationStorageEnum';
  TV_STORAGE_INTERNAL: String = 'Internal';
  TV_STORAGE_PARTIALLYEXTERNAL: String = 'PartiallyExternal';
  TV_STORAGE_EXTERNAL: String = 'External';
  TV_STORAGE_EXTERNALKEY: String = 'ExternalKey';


    TV_PERSISTENT_OLD: String = 'Persistent';

  // Tagged values of basic type

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

  // Tags for boldification
  BOLDBOLDIFYPREFIX = '_Boldify.';
  TAG_BOLDIFIED = 'boldified';
  TAG_AUTOCREATED = 'autoCreated';  // object
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
  SysUtils,
  BoldUtils;

var
 G_BoldDefaultTaggedValues: TBoldTaggedValuePerClassList = nil;

procedure AddDefaultTaggedValues;
begin
// Tagged values for Model
  with G_BoldDefaultTaggedValues.ListForClassName['Model'] do // do not localize
  begin
    Add('Boolean',  TAG_GENERATEMULTIPLICITYCONSTRAINTS, TV_TRUE); // do not localize
    Add('Text',     TAG_INTERFACEUSES,                   ''); // do not localize
    Add('Text',     TAG_IMPLEMENTATIONUSES,              ''); // do not localize
    Add('Text',     TAG_COPYRIGHTNOTICE,                 ''); // do not localize
    Add('String',   TAG_DEFAULTSUPERCLASS,               ''); // do not localize
    Add('String',   TAG_DEFAULTLINKCLASSSUPERCLASS,      ''); // do not localize
    Add('String',   TAG_UNITNAME,                        'BusinessClasses'); // do not localize
    Add('String',   TAG_ROOTCLASS,                       ''); // do not localize
    Add('String',   TAG_PMAPPERNAME,                     DEFAULTNAMELITERAL); // do not localize
    Add('Boolean',  TAG_USEXFILES,                       TV_TRUE); // do not localize
    Add('Boolean',  TAG_USETIMESTAMP,                    TV_TRUE); // do not localize
    Add('Boolean',  TAG_USEGLOBALID,                     TV_TRUE); // do not localize
    Add('Boolean',  TAG_USEREADONLY,                     TV_TRUE); // do not localize
    Add('Boolean',  TAG_USEMODELVERSION,                 TV_FALSE); // do not localize
    Add('Integer',  TAG_MODELVERSION,                    '0'); // do not localize
    Add('Boolean',  TAG_USECLOCKLOG,                     TV_TRUE); // do not localize
    Add('Boolean',  TAG_UPDATEWHOLEOBJECTS,              TV_FALSE); // do not localize
    Add('OptimisticLockingSet', // do not localize
                    TAG_OPTIMISTICLOCKING,               TV_OPTIMISTICLOCKING_OFF);
    Add('NationalCharConversionEnum', // do not localize
                    TAG_NATIONALCHARCONVERSION,          TV_NATIONALCHARCONVERSION_DEFAULT);
    Add('String',   TAG_GUID,                            ''); // do not localize
    Add('String',   TAG_TYPELIBVERSION,                  '1.0'); // do not localize
    Add('Text',     TAG_REGIONDEFINITIONS,               ''); // do not localize
    Add('Boolean',  TAG_GENERATEDEFAULTREGIONS,          TV_FALSE); // do not localize
  end;

// Tagged values for Class
  with G_BoldDefaultTaggedValues.ListForClassName['Class'] do // do not localize
  begin
    Add('String',   TAG_INCFILENAME,                    ''); // do not localize
    Add('String',   TAG_UNITNAME,                       ''); // do not localize
    Add('Boolean',  TAG_IMPORTED,                       TV_FALSE); // do not localize
    Add('TableMappingSet', // do not localize
                    TAG_TABLEMAPPING,                   TV_TABLEMAPPING_OWN);
    Add('String',   TAG_DELPHINAME,                     'T<Name>'); // do not localize
    Add('String',   TAG_CPPNAME,                        TV_NAME); // do not localize
    Add('String',   TAG_EXPRESSIONNAME,                 TV_NAME); // do not localize
    Add('String',   TAG_TABLENAME,                      TV_NAME); // do not localize
    Add('EvolutionStateEnum', // do not localize
                    TAG_EVOLUTIONSTATE,                 TV_EVOLUTIONSTATE_NORMAL);
    Add('String',   TAG_PMAPPERNAME,                    DEFAULTNAMELITERAL); // do not localize
    Add('String',   TAG_DEFAULTSTRINGREPRESENTATION,    ''); // do not localize
    Add('Text',     TAG_DERIVATIONEXPRESSIONS,          ''); // do not localize
    Add('Boolean',  TAG_VERSIONED,                      TV_FALSE); // do not localize
    Add('OptimisticLockingSet', // do not localize
                    TAG_OPTIMISTICLOCKING,              TV_OPTIMISTICLOCKING_DEFAULT);
    Add('Text',   TAG_FORMERNAMES,                    ''); // do not localize
    Add('String',   TAG_INTERFACENAME,                  'I<Name>'); // do not localize
    Add('String',   TAG_GUID,                           ''); // do not localize
    Add('Boolean', // do not localize
                    TAG_GENERATEDEFAULTREGION_CLASS,    TV_TRUE);
    Add(ENUM_TAG_CLASS_STORAGE,
                    TAG_STORAGE,                        TV_STORAGE_INTERNAL);
  end;

// Tagged values for Association
  with G_BoldDefaultTaggedValues.ListForClassName['Association'] do // do not localize
  begin
    Add('String', TAG_LINKCLASSNAME,               TV_NAME); // do not localize
    Add('Text', TAG_FORMERNAMES,                 ''); // do not localize
    Add('EvolutionStateEnum', // do not localize
                  TAG_EVOLUTIONSTATE,              TV_EVOLUTIONSTATE_NORMAL);
    Add(ENUM_TAG_ASSOCIATION_STORAGE,
                    TAG_STORAGE,                        TV_STORAGE_INTERNAL);
  end;

// Tagged values for Attribute
  with G_BoldDefaultTaggedValues.ListForClassName['Attribute'] do // do not localize
  begin
    Add('Integer', TAG_LENGTH,                     '255'); // do not localize
    Add('Boolean', TAG_ALLOWNULL,                  TV_FALSE); // do not localize
    Add('Boolean', TAG_DELAYEDFETCH,               TV_FALSE); // do not localize
    Add('String',  TAG_COLUMNNAME,                 TV_NAME); // do not localize
    Add('String',  TAG_EXPRESSIONNAME,             TV_NAME); // do not localize
    Add('String',  TAG_DELPHINAME,                 TV_NAME); // do not localize
    Add('String',  TAG_CPPNAME,                    TV_NAME); // do not localize
    Add('String',  TAG_PMAPPERNAME,                DEFAULTNAMELITERAL); // do not localize

    Add('Text',    TAG_DERIVATIONOCL,              ''); // do not localize
    Add('Boolean', TAG_VIRTUALDERIVE,              TV_TRUE); // do not localize
    Add('Boolean', TAG_REVERSEDERIVE,              TV_FALSE); // do not localize
    Add('AttributeKindSet', // do not localize
                   TAG_ATTRIBUTEKIND,              TV_ATTRIBUTEKIND_BOLD);
    Add('Boolean', TAG_DELPHIFIELD,                TV_FALSE); // do not localize
    Add('DelphiPropertySet', // do not localize
                   TAG_DPREAD,                     TV_DPNONE);
    Add('DelphiPropertySet', // do not localize
                   TAG_DPWRITE,                    TV_DPNONE);

    Add('EvolutionStateEnum', // do not localize
                   TAG_EVOLUTIONSTATE,            TV_EVOLUTIONSTATE_NORMAL);
    Add('Text',  TAG_FORMERNAMES,               ''); // do not localize
    Add('String',  TAG_DEFAULTDBVALUE,            ''); // do not localize
    Add(ENUM_TAG_ATTRIBUTE_STORAGE,
                    TAG_STORAGE,                        TV_STORAGE_INTERNAL);
  end;

// Tagged values for AssociationEnd
  with G_BoldDefaultTaggedValues.ListForClassName['AssociationEnd'] do // do not localize
  begin
    Add('Boolean', TAG_ORDERED,                    TV_FALSE); // do not localize
    Add('String',  TAG_COLUMNNAME,                 TV_NAME); // do not localize
    Add('String',  TAG_EXPRESSIONNAME,             TV_NAME); // do not localize
    Add('String',  TAG_DELPHINAME,                 TV_NAME); // do not localize
    Add('String',  TAG_CPPNAME,                    TV_NAME); // do not localize
    Add('Boolean', TAG_EMBED,                      TV_TRUE); // do not localize
    Add('Text',    TAG_DERIVATIONOCL,              ''); // do not localize
    Add('Boolean', TAG_VIRTUALDERIVE,              TV_TRUE); // do not localize
    Add('DeleteActions', // do not localize
                   TAG_DELETEACTION,               TV_DELETEACTION_DEFAULT);

    Add('Text',    TAG_FORMERNAMES,                ''); // do not localize
    Add('DefaultRegionModeAssociationEnum', // do not localize
                  TAG_DEFAULTREGIONMODE_ASSOCIATIONEND, TV_DEFAULTREGIONMODE_ASSOCIATIONEND_DEFAULT);
 end;

// Tagged values for Operation
  with G_BoldDefaultTaggedValues.ListForClassName['Operation'] do // do not localize
  begin
    Add('String',  TAG_DELPHINAME,                     TV_NAME); // do not localize
    Add('String',  TAG_CPPNAME,                        TV_NAME); // do not localize
    Add('String',  TAG_EXPRESSIONNAME,                 TV_NAME); // do not localize
    Add('BoldOperationKindSet', // do not localize
                   TAG_DELPHIOPERATIONKIND,            TV_DELPHIOPERATIONKIND_NORMAL);
    Add('Boolean', TAG_OVERRIDEINALLSUBCLASSES,        TV_FALSE); // do not localize
  end;

  with G_BoldDefaultTaggedValues.ListForClassName['Parameter'] do // do not localize
  begin
    Add('Boolean', TAG_ISCONST,                        TV_FALSE); // do not localize
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
