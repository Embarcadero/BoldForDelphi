unit BoldMMTVDefGen;

interface

uses
  BoldDefaultTaggedValues,
  BoldUMLTaggedValues,
  MSXML_TLB,
  BoldTaggedValueList;

type
  TBoldMMTVDefGen = class
  private
    fDoc: TDOMDocument;
    fEnumDefElement: IXMLDOMElement;
    function AddElement(Name: string; parent: IXMLDOMElement): IXMLDOMElement;
    procedure GenTV(Parent: IXMLDOMElement; TagName: string; TypeName: string; Default: string); 
    procedure GenTVList(parent: IXMLDOMElement; TVList: TBoldTaggedValueList; prefix: string);
    procedure AddEnumDef(EnumName: string; EnumValues: string);
  public
    procedure Generate(FileName: string);
  end;

implementation

uses
  BoldDefs,
  BoldMMTVDefs;

{ TBoldMMTVDefGen }

function TBoldMMTVDefGen.AddElement(Name: string; parent: IXMLDOMElement): IXMLDOMElement;
begin
  result := fDoc.createElement(Name);
  parent.appendChild(result);
end;

procedure TBoldMMTVDefGen.AddEnumDef(EnumName: string; EnumValues: string);
var
  Elem: IXMLDOMElement;
begin
  Elem := AddElement(NODENAME_ENUMDEFINITION, fEnumDefElement);
  Elem.setAttribute(ATTRNAME_ENUMNAME, EnumName);
  Elem.setAttribute(ATTRNAME_ENUMVALUES, EnumValues);
end;

procedure TBoldMMTVDefGen.Generate(FileName: string);
var
  RootElement: IXMLDOMElement;
  ClassDefElement: IXMLDOMElement;
  AttrDefElement: IXMLDOMElement;
  AssocDefElement: IXMLDOMElement;
  MethodDefElement: IXMLDOMElement;
  ModelDefElement: IXMLDOMElement;
begin
  fDoc := TDOMDocument.Create(nil);
  fDoc.documentElement := fDoc.createElement('TagDefinitions'); // do not localize
  RootElement := fDoc.documentElement;

  fEnumDefElement := AddElement(NODENAME_ENUMS, RootElement);
  ClassDefElement := AddElement(NODENAME_CLASSTAGS, RootElement);
  AttrDefElement := AddElement(NODENAME_ATTRTAGS, RootElement);
  AssocDefElement := AddElement(NODENAME_ASSOCTAGS, RootElement);
  MethodDefElement := AddElement(NODENAME_METHODTAGS, RootElement);
  ModelDefElement := AddElement(NODENAME_MODELTAGS, RootElement);

  AddEnumDef('Boolean', 'True, False'); // do not localize
  AddEnumDef('AttributeKindSet', TV_ATTRIBUTEKIND_BOLD + ', ' + TV_ATTRIBUTEKIND_DELPHI); // do not localize
  AddEnumDef('BoldOperationKindSet', TV_DELPHIOPERATIONKIND_NORMAL + ', ' + // do not localize
                                     TV_DELPHIOPERATIONKIND_VIRTUAL + ', ' +
                                     TV_DELPHIOPERATIONKIND_ABSTRACTVIRTUAL + ', ' +
                                     TV_DELPHIOPERATIONKIND_DYNAMIC + ', ' +
                                     TV_DELPHIOPERATIONKIND_OVERRIDE);
//  AddEnumDef('ChangeabilityKind', TV_CHANGEABILITY_ADDONLY + ', ' +
//                                     TV_CHANGEABILITY_CHANGEABLE + ', ' +
//                                     TV_CHANGEABILITY_FROZEN);
  AddEnumDef('DeleteActions', TV_DELETEACTION_DEFAULT + ', ' + // do not localize
                                     TV_DELETEACTION_ALLOW + ', ' +
                                     TV_DELETEACTION_PROHIBIT + ', ' +
                                     TV_DELETEACTION_CASCADE);
  AddEnumDef('DelphiPropertySet', TV_DPNONE + ', ' + // do not localize
                                     TV_DPFIELD + ', ' +
                                     TV_DPPRIVATEMETHOD + ', ' +
                                     TV_DPPROTECTEDVIRTUALMETHOD);
  AddEnumDef('EvolutionStateEnum', TV_EVOLUTIONSTATE_NORMAL + ', ' + // do not localize
                                     TV_EVOLUTIONSTATE_TOBEREMOVED + ', ' +
                                     TV_EVOLUTIONSTATE_REMOVED);
  AddEnumDef('NationalCharConversionEnum', TV_NATIONALCHARCONVERSION_DEFAULT + ', ' + // do not localize
                                     TV_NATIONALCHARCONVERSION_TRUE + ', ' +
                                     TV_NATIONALCHARCONVERSION_FALSE);
  AddEnumDef('OptimisticLockingSet', TV_OPTIMISTICLOCKING_DEFAULT + ', ' + // do not localize
                                     TV_OPTIMISTICLOCKING_OFF + ', ' +
                                     TV_OPTIMISTICLOCKING_MODIFIEDMEMBERS + ', ' +
                                     TV_OPTIMISTICLOCKING_ALLMEMBERS + ', ' +
                                     TV_OPTIMISTICLOCKING_TIMESTAMP);
  AddEnumDef('TableMappingSet', TV_TABLEMAPPING_OWN + ', ' + // do not localize
                                     TV_TABLEMAPPING_PARENT + ', ' +
                                     TV_TABLEMAPPING_CHILDREN + ', ' +
                                     TV_TABLEMAPPING_IMPORTED);
  AddEnumDef('DefaultRegionModeAssociationEnum', TV_DEFAULTREGIONMODE_ASSOCIATIONEND_DEFAULT + ', ' + // do not localize
                                     TV_DEFAULTREGIONMODE_ASSOCIATIONEND_NONE + ', ' +
                                     TV_DEFAULTREGIONMODE_ASSOCIATIONEND_EXISTENCE + ', ' +
                                     TV_DEFAULTREGIONMODE_ASSOCIATIONEND_CASCADE + ', ' +
                                     TV_DEFAULTREGIONMODE_ASSOCIATIONEND_INDEPENDENTCASCADE);
  AddEnumDef(ENUM_TAG_CLASS_STORAGE, TV_STORAGE_INTERNAL + ', ' +
                                     TV_STORAGE_PARTIALLYEXTERNAL + ', ' +
                                     TV_STORAGE_EXTERNAL);
  AddEnumDef(ENUM_TAG_ASSOCIATION_STORAGE, TV_STORAGE_INTERNAL + ', ' +
                                     TV_STORAGE_EXTERNAL);
  AddEnumDef(ENUM_TAG_ATTRIBUTE_STORAGE, TV_STORAGE_INTERNAL + ', ' +
                                     TV_STORAGE_EXTERNAL + ', ' +
                                     TV_STORAGE_EXTERNALKEY);
  AddEnumDef(ENUM_TAG_PERSISTENCE, TV_PERSISTENCE_PERSISTENT + ', ' +
                                     TV_PERSISTENCE_TRANSIENT);

  GenTVList(ClassDefElement, BoldDefaultTaggedValueList.ListForClassName['Class'], BOLDTVPREFIX); // do not localize
  GenTVList(AttrDefElement, BoldDefaultTaggedValueList.ListForClassName['Attribute'], BOLDTVPREFIX); // do not localize
  GenTVList(AssocDefElement, BoldDefaultTaggedValueList.ListForClassName['Association'], BOLDTVPREFIX); // do not localize
  GenTVList(AssocDefElement, BoldDefaultTaggedValueList.ListForClassName['AssociationEnd'], PREFIX_SOURCE_ASSOC_END + BOLDTVPREFIX); // do not localize
  GenTVList(AssocDefElement, BoldDefaultTaggedValueList.ListForClassName['AssociationEnd'], PREFIX_TARGET_ASSOC_END + BOLDTVPREFIX); // do not localize
  GenTVList(MethodDefElement, BoldDefaultTaggedValueList.ListForClassName['Operation'], BOLDTVPREFIX); // do not localize
  GenTVList(ModelDefElement, BoldDefaultTaggedValueList.ListForClassName['Model'], PREFIX_MODEL + BOLDTVPREFIX); // do not localize

  GenTVList(ClassDefElement, UMLTaggedValueList.ListForClassName['Class'], ''); // do not localize
  GenTVList(AttrDefElement, UMLTaggedValueList.ListForClassName['Attribute'], ''); // do not localize
  GenTVList(AssocDefElement, UMLTaggedValueList.ListForClassName['Association'], ''); // do not localize

  GenTV(ClassDefElement, BOLDTVPREFIX + TAG_CONSTRAINTS, 'Text', ''); // do not localize
  GenTV(AttrDefElement, BOLDTVPREFIX + TAG_CONSTRAINTS, 'Text', ''); // do not localize
  GenTV(MethodDefElement, BOLDTVPREFIX + TAG_CONSTRAINTS, 'Text', ''); // do not localize
  GenTV(AssocDefElement, PREFIX_SOURCE_ASSOC_END + BOLDTVPREFIX + TAG_CONSTRAINTS, 'Text', ''); // do not localize
  GenTV(AssocDefElement, PREFIX_TARGET_ASSOC_END + BOLDTVPREFIX + TAG_CONSTRAINTS, 'Text', ''); // do not localize

  fDoc.save(FileName);
end;

procedure TBoldMMTVDefGen.GenTV(Parent: IXMLDOMElement; TagName, TypeName,
  Default: string);
var
  Elem: IXMLDOMElement;
begin
  Elem := AddElement(NODENAME_TAGDEFINITION, Parent);
  Elem.setAttribute(ATTRNAME_TAGNAME, TagName);
  Elem.setAttribute(ATTRNAME_TYPENAME, TypeName);
  Elem.setAttribute(ATTRNAME_DEFAULT, Default);
end;

procedure TBoldMMTVDefGen.GenTVList(parent: IXMLDOMElement;
  TVList: TBoldTaggedValueList; prefix: string);
var
  i: integer;
begin
  for i := 0 to TVList.Count - 1 do
    GenTV(parent, prefix + TVList.Definition[i].Tag, TVList.Definition[i].TypeName,
          TVList.Definition[i].DefaultValue);
end;

end.
