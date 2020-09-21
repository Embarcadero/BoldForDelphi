unit BoldUMLModelSupport;

interface

uses
  BoldDefs,
  BoldLogHandler,
  Classes,
  BoldDefaultTaggedValues,
  BoldSubscription,
  BoldSystem,
  BoldUMLModel,
  BoldUMLTypes,
  BoldTaggedValueSupport;

const
  // tags for internal use
  TAG_TOOLID = 'toolId';

  TAG_FLATTENED = 'flattened';

type
  { forward declarations }
  TBoldUMLSupport = class;
  TBoldUMLBoldify = class;
  TBoldCopyAndClone = class;
  TBoldUMLOperationSupport = class;

  TBoldCopyMode = (bcmAttributes, bcmShallow, bcmDeep);

  TUMLModelElementManipulator = procedure(element: TUMLModelElement) of object;

  { TBoldCopyAndClone }
  TBoldCopyAndClone = class(TObject)
  public
    class procedure BoldCopy(DestinationObject, SourceObject: TBoldObject; Mode: TBoldCopyMode; StripToolId: Boolean);
    class function BoldClone(SourceObject: TBoldObject; Mode: TBoldCopyMode; StripToolId: Boolean = True): TBoldObject;
  end;

  { TBoldUMLSupport }
  TBoldUMLSupport = class(TObject)
  private
    class function NameInListExceptElement(Name: string; List: TUMLModelElementList; UMLElement: TUMLModelElement): Boolean;
    class procedure SubscribeToAllMembers(UMLElement: TUMLModelElement; Subscriber: TBoldSubscriber);
    class function GetEnsuredPackage(Package: TUMLPackage; QualifiedPackageName: String): TUMLPackage;
  public
    class function UMLModelNameToUMLName(const ModelName: string): string;
    class procedure EnsureBoldTaggedValues(Element: TUMLModelElement);  // Add missing tagged values
    class procedure EnsureBoldTaggedValuesInModel(Model: TUMLModel);
    class procedure ManipulateAllOwnedElements(Model: TUMLModel; manipulator: TUMLModelElementManipulator);
    class function EnsuredTaggedValue(Element: TUMLModelElement; const Tag: string): TUMLTaggedValue;
    class procedure RemoveTaggedValue(Element: TUMLModelElement; const Tag: string);
//    class procedure SetDefaultBoldTaggedValues(Element: TUMLModelElement); // Set all tagged values to default
    class procedure RelinkSpecializations(GeneralizableElement: TUMLGeneralizableElement);  // Give move all inheritance up one step.
    class function UniqueName(Element: TUMLModelElement; const SuggestedName: String): String;
    class procedure AddQualifier(AssociationEnd: TUMLAssociationEnd; Qualifier: TUMLAttribute; const SuggestedName: String);
    class procedure AddToNamespace(NameSpace: TUMLNamespace; Element: TUMLModelElement; const SuggestedName: String);
    class procedure AddToolId(Element: TUMLModelElement; ToolId: string);
    class function GetToolId(Element: TUMLModelElement): string;
    class procedure StripToolId(Model: TUMLModel);
    class function ElementForToolId(UMLModel: TUMLModel; ToolId: string): TUMLModelElement;
    class function IsFlattened(UMLModel: TUMLModel): Boolean;
    class procedure Flatten(UMLModel: TUMLModel);
    class procedure UnFlatten(UMLModel: TUMLModel);
    class procedure SubscribeToEntireModel(UMLModel: TUMLModel; Subscriber: TBoldSubscriber);
    class function AllModelParts(UMLModel: TUMLModel): TUMLModelElementList;
  end;

  { TBoldUMLBoldify }
  TBoldUMLBoldify = class(TPersistent)
  private
    fPluralSuffix: string;
    fEnforceDefaultUMLCase: Boolean;
    fDefaultNonNavigableMultiplicity: string;
    fDefaultNavigableMultiplicity: string;
    fUnembedMulti: boolean;
    fMakeDerivedTransient: boolean;
    class procedure SetBoldifyTaggedValue(element: TUMLModelElement; const Tag: string; const value: string );
    class function FullTag(const Tag: string): string;
   public
     constructor Create;
     procedure Boldify(UMLModel: TUMLModel);
     class procedure UnBoldify(Model: TUMLModel);
     procedure Assign(source: TPersistent); override;
     class function IsBoldified(Model: TUMLModel): Boolean;
     class function IsAutoCreated(Element: TUMLModelElement): Boolean;
     class procedure RemoveBoldifyTaggedValue(Element: TUMLModelElement; const Tag: string);
     class function GetBoldifyTaggedValue(Element: TUMLModelElement; const Tag: string): string;
     class function GetRootClass(Model: TUMLModel): TUMLClass;
     class procedure SetRootClassname(Model: TUMLModel; name: string);
     class function GetDefaultSuperClass(Model: TUMLModel): TUMLClass;
     class function GetDefaultLinkSuperClass(Model: TUMLModel): TUMLClass;
   published
     property PluralSuffix: string read fPluralSuffix write fPluralSuffix;
     property EnforceDefaultUMLCase: Boolean read fEnforceDefaultUMLCase write fEnforceDefaultUMLCase;
     property DefaultNavigableMultiplicity: string read fDefaultNavigableMultiplicity write fDefaultNavigableMultiplicity;
     property DefaultNonNavigableMultiplicity: string read fDefaultNonNavigableMultiplicity write fDefaultNonNavigableMultiplicity;
     property UnembedMulti: boolean read fUnembedMulti write fUnembedMulti default True;
     property MakeDerivedTransient: boolean read fMakeDerivedTransient write fMakeDerivedTransient default True;
   end;

   { TBoldUMLOperationSupport }
   TBoldUMLOperationSupport = class(TObject)   // FIXME move to model editor. TO specific for support
   public
     class procedure OverrideInClass(UMLClass: TUMLClassifier; UMLOperation: TUMLOperation);
     class procedure OverrideInAllSubclasses(UMLClass: TUMLClassifier;UMLOperation: TUMLOperation);
     class function ClassHasOperation(UMLClass: TUMLClassifier; MethodName: String; ParamTypes: TStringList): Boolean;
     class procedure ParameterTypesToList(UMLOperation: TUMLOperation; ParamTypes: TStringList);
   end;

implementation

uses
  SysUtils,
  BoldUtils,
  BoldSystemRT,
  BoldNameExpander,
  BoldUMLTaggedValues,
  BoldGuard,
  UMLConsts;

{ TBoldUMLSupport }

class procedure TBoldUMLSupport.AddQualifier(AssociationEnd: TUMLAssociationEnd; Qualifier: TUMLAttribute; const SuggestedName: String);
begin
  Qualifier.associationEnd := AssociationEnd;
  Qualifier.name := UniqueName(Qualifier, SuggestedName);
end;

class procedure TBoldUMLSupport.AddToNamespace(NameSpace: TUMLNamespace;
  Element: TUMLModelElement; const SuggestedName: String);
begin
  Element.namespace_ := NameSpace;
  Element.name := UniqueName(Element, SuggestedName);
end;

class procedure TBoldUMLSupport.AddToolId(Element: TUMLModelElement;
  ToolId: string);
begin
  EnsuredTaggedValue(Element, BOLDINTERALTVPREFIX + TAG_TOOLID).Value := ToolId;
end;

class function TBoldUMLSupport.AllModelParts(
  UMLModel: TUMLModel): TUMLModelElementList;
begin
  Result := UMLModel.EvaluateExpressionAsNewElement('UMLModelElement.allInstances->select(model=self)') as TUMLModelElementList // do not localize
end;

class function TBoldUMLSupport.ElementForToolId(UMLModel: TUMLModel;
  ToolId: string): TUMLModelElement;
begin
  Result :=  UMLModel.EvaluateExpressionAsDirectElement
      (
      Format('UMLTaggedValue.allInstances->select((tag=''%s'') and (value=''%s'')).modelElement->select(model=self)->first', // do not localize
        [BOLDINTERALTVPREFIX + TAG_TOOLID, ToolId])
      ) as TUMLModelElement;
end;

class procedure TBoldUMLSupport.EnsureBoldTaggedValues(
  Element: TUMLModelElement);
var
  i: integer;
  FullTag, Tag: String;
begin
  with BoldDefaultTaggedValueList.ListForClassName[UMLModelNameToUMLName(Element.BoldClassTypeInfo.ExpressionName)] do
    for i := 0 to Count - 1 do
    begin
      FullTag := BOLDTVPREFIX +  Definition[i].Tag;
      if Element.taggedValue[FullTag] = nil then
        EnsuredTaggedValue(Element, FullTag).value := Definition[i].DefaultValue;
    end;
  with UMLTaggedValueList.ListForClassName[UMLModelNameToUMLName(Element.BoldClassTypeInfo.ExpressionName)] do
    for i := 0 to Count - 1 do
    begin
      Tag := Definition[i].Tag;
      if Element.taggedValue[Tag] = nil then
        EnsuredTaggedValue(Element, Tag).value := Definition[i].DefaultValue;
    end;
end;

class procedure TBoldUMLSupport.EnsureBoldTaggedValuesInModel(
  Model: TUMLModel);
begin
  ManipulateAllOwnedElements(model,EnsureBoldTaggedValues);
end;

class function TBoldUMLSupport.EnsuredTaggedValue(Element: TUMLModelElement; const Tag: string): TUMLTaggedValue;
begin
  Result := Element.TaggedValue[Tag];
  if not Assigned(Result) then
  begin
    result := Element.M_taggedValue.AddNew;
    result.Tag := Tag
  end;
end;

class procedure TBoldUMLSupport.Flatten(UMLModel: TUMLModel);
var
  List: TUMLModelElementList;
  Element: TUMLModelElement;
  i: integer;
  BoldGuard: IBoldGuard;
begin
  BoldGuard := TBoldGuard.Create(List);
  BoldLog.StartLog(sFlatteningModel);
  try
    List :=  UMLModel.EvaluateExpressionAsNewElement
      (
      'allOwnedElement->select(oclIsKindOf(UMLClass) or oclIsKindOf(UMLAssociation))' // do not localize
      ) as TUMLModelElementList;
    BoldLog.ProgressMax := List.Count;
    for i := 0 to List.Count - 1 do
    begin
      Element := List[i];
      if (Element.namespace_ <> UMLModel) then
      begin
         EnsuredTaggedValue(Element, BOLDINTERALTVPREFIX + TAG_UNFLATTENEDNAMESPACE).Value := Element.namespace_.qualifiedName;
        Element.namespace_ := UMLModel;
      end;
      BoldLog.ProgressStep;
    end;
  finally
    BoldLog.EndLog;
  end;
  EnsuredTaggedValue(UMLModel, BOLDINTERALTVPREFIX + TAG_FLATTENED).Value := TV_TRUE;
end;

class function TBoldUMLSupport.GetEnsuredPackage(Package: TUMLPackage; QualifiedPackageName: String): TUMLPackage;
var
  PacakgeName: String;
  i: integer;
  temp: TUMLPackage;
begin
  if pos('.', QualifiedPackageName) <> 0 then
  begin
    PacakgeName := copy(QualifiedPackageName, 1, pos('.', QualifiedPackageName) - 1);
    System.Delete(QualifiedPackageName, 1, Pos('.', QualifiedPackageName));
  end
  else
  begin
    PacakgeName := QualifiedPackageName;
    QualifiedPackageName := '';
  end;
  temp := nil;
  for i := 0 to Package.ownedElement.Count - 1 do
  begin
    if (Package.ownedElement[i] is TUMLPackage) and
       SameText(PacakgeName, Package.ownedElement[i].Name) then
       temp := Package.ownedElement[i] as TUMLPackage
  end;
  if not assigned(Temp) then
  begin
    temp := TUMLPackage.create(Package.BoldSystem);
    temp.Name := PacakgeName;
    temp.namespace_ := Package;
  end;
  if QualifiedPackagename <> '' then
    result := GetEnsuredPackage(temp, QualifiedPackageName)
  else
    result := temp;
end;

class function TBoldUMLSupport.GetToolId(Element: TUMLModelElement): string;
var
  TaggedValue: TUMLTaggedValue;
begin
  TaggedValue := Element.TaggedValue[BOLDINTERALTVPREFIX + TAG_TOOLID];
  if assigned(TaggedValue) then
    Result := TaggedValue.value
  else
    Result := '';
end;

class function TBoldUMLSupport.IsFlattened(UMLModel: TUMLModel): Boolean;
begin
  Result := Assigned(UMLModel.taggedValue[BOLDINTERALTVPREFIX + TAG_FLATTENED]) and
      TVIsTrue(UMLModel.taggedValue[BOLDINTERALTVPREFIX + TAG_FLATTENED].Value);
end;

class procedure TBoldUMLSupport.ManipulateAllOwnedElements(
  Model: TUMLModel; manipulator: TUMLModelElementManipulator);
var
  i: integer;
  allOwnedElement: TUMLModelElementList;
  BoldGuard: IBoldGuard;
begin
  BoldGuard := TBoldGuard.Create(allOwnedElement);
  manipulator(model);
  allOwnedElement:= Model.EvaluateExpressionAsNewElement('UMLModelElement.allInstances->select(model=self)') as TUMLModelElementList; // do not localize
  for i := 0 to allOwnedElement.Count - 1 do
    manipulator(allOwnedElement[i]);
end;

class function TBoldUMLSupport.NameInListExceptElement(Name: string; List: TUMLModelElementList; UMLElement: TUMLModelElement): Boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to List.Count - 1 do
    if (List[i].name = Name) and (List[i] <> UMLElement) then
    begin
      Result := True;
      Break;
    end;
end;

class procedure TBoldUMLSupport.RelinkSpecializations(
  GeneralizableElement: TUMLGeneralizableElement );
var
  NewGeneralization: TUMLGeneralization;
  Child:  TUMLGeneralizableElement;
  s, g: integer;
begin
  // replace myself with all my superclasses in children
  for s := GeneralizableElement.specialization.Count - 1 downto 0 do
  begin
    Child := GeneralizableElement.specialization[s].child;
    GeneralizableElement.specialization[s].Delete;
    for g := 0 to GeneralizableElement.generalization.Count - 1 do
    begin
      NewGeneralization := TUMLGeneralization.Create(GeneralizableElement.BoldSystem);
      NewGeneralization.namespace_ := Child;
      NewGeneralization.parent := GeneralizableElement.generalization[g].parent;
      NewGeneralization.child := Child;
    end;
  end;
end;

class procedure TBoldUMLSupport.RemoveTaggedValue(Element: TUMLModelElement; const Tag: string);
var
  TaggedValue: TUMLTaggedValue;
begin
  TaggedValue := Element.taggedValue[Tag];
  if Assigned(TaggedValue) then
    TaggedValue.Delete;
end;
{
class procedure TBoldUMLSupport.SetDefaultBoldTaggedValues(
  Element: TUMLModelElement);
var
  i: integer;
begin
  with BoldDefaultTaggedValueList.ListForClassName[UMLModelNameToUMLName(Element.BoldClassTypeInfo.ExpressionName)] do
    for i := 0 to Count - 1 do
       EnsuredTaggedValue(Element, BOLDTVPREFIX + Definition[i].Tag).value := Definition[i].DefaultValue;
end;
}
class procedure TBoldUMLSupport.StripToolId(Model: TUMLModel);
var
  List: TUMLModelElementList;
  i: integer;
  BoldGuard: IBoldGuard;
begin
  BoldGuard := TBoldGuard.Create(List);
  List :=  Model.EvaluateExpressionAsNewElement
    (
    Format(
      'UMLTaggedValue.allInstances->select(model=self)->select(name=''%s'')', // do not localize
      [BOLDINTERALTVPREFIX + TAG_TOOLID])
    ) as TUMLModelElementList;
   for i := List.Count - 1 downto 0 do
     List[i].Delete;
end;

class procedure TBoldUMLSupport.SubscribeToAllMembers(UMLElement: TUMLModelElement; Subscriber: TBoldSubscriber);
{var
  m: integer;
  MemberRTInfo: TBoldMemberRTInfo;
  OtherEnd, RoleRTInfo: TBoldRoleRTInfo;
  IsTaggedValue, Subscribe: Boolean;
}
begin
  UMLElement.AddSmallSubscription(subscriber, [beMemberChanged], breReEvaluate);
{
  // is it not possible to use the event beMemberChanged that is sent by an object.
  IsTaggedValue := (UMLElement is TUMLTaggedValue);
  for m := 0 to UMLElement.BoldMemberCount - 1 do
  begin
    MemberRTInfo := UMLElement.BoldClassTypeInfo.AllMembers[m];
    // for tagged values, only subscribe to the value!
    if not MemberRTInfo.IsDerived and
      (not IsTaggedValue) or (MemberRTInfo.ExpressionName =  'value') then
    begin
      if MemberRTInfo.IsAttribute then
      begin
        Subscribe := true;
      end
      else
      begin
        Assert(MemberRTInfo is TBoldRoleRTInfo);
        RoleRTInfo := TBoldRoleRTInfo(MemberRTInfo);
        OtherEnd := RoleRTInfo.RoleRTInfoOfOtherEnd;
        Subscribe :=
          (RoleRTInfo.IsMultiRole or OtherEnd.IsSingleRole or not OtherEnd.IsNavigable) and
          RoleRTInfo.IsNavigable and (RoleRTInfo.RoleType = rtRole);
      end;
      if Subscribe then
        UMLElement.BoldMembers[m].DefaultSubscribe(Subscriber)
    end;
  end;
  }
end;

class procedure TBoldUMLSupport.SubscribeToEntireModel(UMLModel: TUMLModel; Subscriber: TBoldSubscriber);
var
  i: integer;
  Parts: TUMLModelElementList;
begin
  Parts := AllModelParts(UMLModel);

  try
    for i := 0 to Parts.Count - 1 do
      SubscribeToAllMembers(Parts[i], Subscriber);
  finally
    Parts.Free;
  end;
end;

class function TBoldUMLSupport.UMLModelNameToUMLName(const ModelName: string): string;
begin
  Result := Copy(ModelName, 4, MAXINT);  // Names in UMLModel prefixed by UML.
end;

class procedure TBoldUMLSupport.UnFlatten(UMLModel: TUMLModel);
var
  List: TUMLModelElementList;
  Element: TUMLModelElement;
  UnflattenedNamespaceName: string;
  i: integer;
  CurrentNameSpace: TUMLNamespace;
  BoldGuard: IBoldGuard;
begin
  BoldGuard := TBoldGuard.Create(List);
  CurrentNameSpace := UMLModel;
  List := TUMLModelElementList.Create;
  // ordering by name will give topologial sort
  List := UMLModel.EvaluateExpressionAsNewElement(Format('ownedElement->select(taggedValue->select(tag=''%s'')->notEmpty)->orderBy(taggedValue[''%s''].value)', // do not localize
  [BOLDINTERALTVPREFIX + TAG_UNFLATTENEDNAMESPACE, BOLDINTERALTVPREFIX + TAG_UNFLATTENEDNAMESPACE])) as TUMLModelElementList;
  ;
  for i := 0 to List.Count - 1 do
  begin
    Element := List[i];
    UnflattenedNamespaceName := Element.taggedValue[BOLDINTERALTVPREFIX + TAG_UNFLATTENEDNAMESPACE].Value;
    if UnflattenedNamespaceName <> CurrentNameSpace.qualifiedName then
    begin
      // remove the first part, it is the name of the model
      System.Delete(UnflattenedNameSpaceName, 1, pos('.', UnflattenedNamespaceName));
      CurrentNameSpace := GetEnsuredPackage(UMLModel, UnflattenedNamespaceName);
    end;
    // Set real namespace, and remove tagged value
    Element.namespace_ := CurrentNameSpace;
    RemoveTaggedValue(Element, BOLDINTERALTVPREFIX + TAG_UNFLATTENEDNAMESPACE);
  end;
  RemoveTaggedValue(UMLModel, BOLDINTERALTVPREFIX + TAG_FLATTENED);
end;

class function TBoldUMLSupport.UniqueName(Element: TUMLModelElement; const SuggestedName: String): String;
var
  ConflictFound: Boolean;
  i: integer;
begin
  i := 0;
  repeat
    if i = 0 then
      Result := SuggestedName
    else
      Result := Format('%s_%d', [SuggestedName, i]); // do not localize
    ConflictFound := False;
    if Assigned(Element.namespace_) then
      ConflictFound := NameInListExceptElement(Result, Element.namespace_.OwnedElement, Element);
    if (not ConflictFound) and (Element is TUMLFeature) and Assigned(TUMLFeature(Element).owner) then
      ConflictFound := NameInListExceptElement(Result, TUMLFeature(Element).owner.feature, Element);
    if (not ConflictFound) and (Element is TUMLAttribute) and assigned(TUMLAttribute(Element).associationEnd) then
      ConflictFound := NameInListExceptElement(Result, TUMLAttribute(Element).associationEnd.qualifier, Element);
    if (not ConflictFound) and (Element is TUMLParameter) and assigned(TUMLParameter(Element).behavioralFeature) then
      ConflictFound := NameInListExceptElement(Result, TUMLParameter(Element).behavioralFeature.parameter, Element);
    INC(i);
  until not ConflictFound;
end;

{ TBoldUMLBoldify }

procedure TBoldUMLBoldify.Assign(source: TPersistent);
begin
  if Source is TBoldUMLBoldify then
  begin
    PluralSuffix := TBoldUMLBoldify(Source).PluralSuffix;
    EnforceDefaultUMLCase :=  TBoldUMLBoldify(Source).EnforceDefaultUMLCase;
    DefaultNonNavigableMultiplicity := TBoldUMLBoldify(Source).DefaultNonNavigableMultiplicity;
    DefaultNavigableMultiplicity := TBoldUMLBoldify(Source).DefaultNavigableMultiplicity;
  end
  else
    raise EBold.Create('TBoldUMLBoldify.Assign'); // do not localize
end;

procedure TBoldUMLBoldify.Boldify(UMLModel: TUMLModel);
var
  RootClass: TUMLClass;
  RootClassName: string;
  LinkClassName: string;
  DefaultSuperClass: TUMLClass;
  DefaultLinkSuperClass: TUMLClass;
  AllClasses: TUMLClassList;
  AllAssociations: TUMLAssociationList;
  iClass: TUMLClass;
  Association: TUMLAssociation;
  Feature: TUMLFeature;
  AssociationIndex, ClassIndex, ConnectionIndex, FeatureIndex: integer;
  UnnamedAssociationName: string;
  AssocEnd: TUMLAssociationEnd;
  RootClassImplicitlyNamed: Boolean;
begin
  TBoldUMLSupport.EnsureBoldTaggedValuesInModel(UMLModel);
  AllClasses := nil;
  AllAssociations := nil;
  BoldLog.StartLog(sBoldifyingModel);
  try
    SetBoldifyTaggedValue(UMLModel, TAG_BOLDIFIED, TV_TRUE);
    // Create root class if it is missing
    RootClassName := UMLModel.GetBoldTV(TAG_ROOTCLASS);
    if RootClassName = '' then
    begin
      RootClassName := 'BusinessClassesRoot'; // do not localize
      RootClassImplicitlyNamed := true;
    end
    else
      RootClassImplicitlyNamed := false;

    Rootclass := UMLModel.EvaluateExpressionAsDirectElement(Format('UMLClass.allInstances->select((model=self) and (name=''%s''))->first', [RootClassName])) as TUMLClass; // do not localize
    if not Assigned(RootClass) then
    begin
      RootClass := TUMLClass.Create(UMLModel.BoldSystem);
      TBoldUMLSupport.EnsureBoldTaggedValues(RootClass);
      SetBoldifyTaggedValue(RootClass, TAG_AUTOCREATED, TV_TRUE);
      RootClass.name := RootClassName;
      RootClass.namespace_ := UMLModel;
      RootClass.taggedValue[BOLDTVPREFIX + TAG_INCFILENAME].value := '';
      RootClass.taggedValue[BOLDTVPREFIX + TAG_TABLENAME].value := OBJECTTABLE_NAME;
      RootClass.Persistent := True;

      if RootClassImplicitlyNamed then
        SetBoldifyTaggedValue(RootClass, TAG_NONAME, TV_TRUE);
    end;

    if Assigned(RootClass.SuperClass) then
      raise EBold.Create(sCannotBoldifyIfRootClassHasSuperClass);

    DefaultSuperClass := GetDefaultSuperClass(UMLModel);
    if not Assigned(DefaultSuperClass) then
      DefaultSuperClass := RootClass;

    DefaultLinkSuperClass := GetDefaultLinkSuperClass(UMLModel);
    if not Assigned(DefaultLinkSuperClass) then
      DefaultLinkSuperClass := RootClass;

    // Fixup Associations. Set default multiplicity, name unnamed associationends, create missing link-classes
    AllAssociations := UMLModel.EvaluateExpressionAsNewElement('UMLAssociation.allInstances->select(model=self)') as TUMLAssociationList; // do not localize

    BoldLog.ProgressMax := AllAssociations.Count;

    for AssociationIndex := 0 to AllAssociations.Count - 1 do
    begin
      Association := AllAssociations[AssociationIndex];
      if Association.persistent and Association.derived and MakeDerivedTransient then
      begin
        Association.persistent := false;
        SetBoldifyTaggedValue(Association, TAG_WASPERSISTENT, TV_TRUE)
      end;
      UnnamedAssociationName := '';
      for ConnectionIndex := 0 to Association.connection.count - 1 do
      begin
        AssocEnd := Association.connection[ConnectionIndex];
        if AssocEnd.multiplicity = '' then
        begin
          if AssocEnd.isNavigable then
            AssocEnd.multiplicity := DefaultNavigableMultiplicity
          else
            AssocEnd.multiplicity := DefaultNonNavigableMultiplicity;
          SetBoldifyTaggedValue(AssocEnd, TAG_DEFAULTMULTIPLICITY, TV_TRUE);
        end;
        if AssocEnd.multi and UnembedMulti and
          TVIsTrue(AssocEnd.GetBoldTV(TAG_EMBED)) then
        begin
          AssocEnd.SetBoldTV(TAG_EMBED, TV_FALSE);
          SetBoldifyTaggedValue(AssocEnd, TAG_WASEMBEDED, TV_TRUE);
        end;
        // Name unnamed associationends
        if AssocEnd.IsNavigable then
        begin
          if AssocEnd.name = '' then
          begin
            if assigned(AssocEnd.Type_) then
            begin
              if AssocEnd.multi then
                AssocEnd.name := AssocEnd.type_.name + PluralSuffix
              else
                AssocEnd.name := AssocEnd.type_.name;
              UnnamedAssociationName := UnnamedAssociationName + AssocEnd.type_.name;
            end
            else
              AssocEnd.name := Format('Role%d', [ConnectionIndex]); // do not localize
            SetBoldifyTaggedValue(AssocEnd, TAG_NONAME, TV_TRUE);
          end
          else
            UnnamedAssociationName := UnnamedAssociationName + AssocEnd.name;
        end;
      end;

      // fix implicit names for non named non navigable roles
      for ConnectionIndex := 0 to Association.Connection.Count - 1 do
      begin
        AssocEnd := Association.Connection[ConnectionIndex];
        if not AssocEnd.isNavigable then
        begin
          if AssocEnd.Name = '' then
          begin
            AssocEnd.Name := 'x_' + AssocEnd.otherEnd.Name + '_' + AssocEnd.type_.Name;
            SetBoldifyTaggedValue(AssocEnd, TAG_NONAME, TV_TRUE);
            UnnamedAssociationName := UnnamedAssociationName + '_'+AssocEnd.otherEnd.type_.Name;
          end
          else
            UnnamedAssociationName := UnnamedAssociationName + AssocEnd.Name;
        end;
      end;

      // make association transient if either end is transient
      if Association.persistent then
      begin
        for ConnectionIndex := 0 to Association.Connection.Count - 1 do
        begin
          AssocEnd := Association.Connection[ConnectionIndex];

          if assigned(AssocEnd.type_) and not AssocEnd.type_.persistent then
          begin
            Association.persistent := false;
            SetBoldifyTaggedValue(Association, TAG_WASPERSISTENT, TV_TRUE)
          end;
        end;
      end;

      // make association transient if association class is transient
      if Association.persistent and assigned(association.class_) and not association.class_.persistent then
      begin
        Association.persistent := false;
        SetBoldifyTaggedValue(Association, TAG_WASPERSISTENT, TV_TRUE)
      end;

      // Name association if unnamed

      if Association.name = '' then
      begin
        Association.name := UnnamedAssociationName;
        SetBoldifyTaggedValue(Association, TAG_NONAME, TV_TRUE);
      end;
      // Create link class if needed
      if not Association.Derived and
        (
        (Association.Connection[0].Multi and Association.Connection[1].Multi) or
          (
          (Association.connection.Count = 2) and
          TVIsFalse(Association.Connection[0].GetBoldTV(TAG_EMBED)) and
          TVIsFalse(Association.Connection[1].GetBoldTV(TAG_EMBED))
          )
        )
        and not assigned(Association.Class_) then
      begin
        // Note: allowing different names on association and linkclass is not strictly UML.
        LinkClassName := BoldExpandName(Association.getBoldTV(TAG_LINKCLASSNAME), Association.Name, xtDelphi, -1,
          TBoldTaggedValueSupport.StringToNationalCharConversion(UMLModel.GetBoldTV(TAG_NATIONALCHARCONVERSION)));
        if LinkClassName = '' then
          LinkClassName := Association.Name;
        Association.Class_ := TUMLClass.Create(Association.BoldSystem);
        Association.Class_.namespace_ := Association.namespace_;
        Association.Class_.Association := Association;
        Association.Class_.persistent := Association.persistent;
        TBoldUMLSupport.EnsureBoldTaggedValues(Association.Class_);
        if (Association.connection.Count = 2) and
           (TVIsTrue(Association.Connection[0].type_.getBoldTV(TAG_VERSIONED)) or
           TVIsTrue(Association.Connection[1].type_.getBoldTV(TAG_VERSIONED))) then
         Association.Class_.SetBoldTV(TAG_VERSIONED, TV_TRUE);
        SetBoldifyTaggedValue(Association.Class_, TAG_AUTOCREATED, TV_TRUE);
        if (LinkClassName = Association.name) then
          Association.Class_.name := Association.name  // Allow same name on class and association
        else
          Association.Class_.name := TBoldUMLSupport.UniqueName(Association.namespace_, LinkClassName); {will generate new name on collission}
      end;
      BoldLog.ProgressStep;
    end;

    AllClasses := UMLModel.EvaluateExpressionAsNewElement('classes') as TUMLClassList; // do not localize
    BoldLog.ProgressMax := AllClasses.count;

    for ClassIndex := 0 to AllClasses.Count - 1 do
    begin
      iClass := AllClasses[ClassIndex];
      for FeatureIndex := 0 to iClass.feature.Count - 1 do
      begin
        Feature := iClass.feature[FeatureIndex];
        if (Feature is TUMLAttribute) then
          with Feature as  TUMLAttribute do
            if persistent and derived and MakeDerivedTransient then
            begin
              persistent := false;
              SetBoldifyTaggedValue(Feature, TAG_WASPERSISTENT, TV_TRUE)
            end;
      end;

      // Fixup inheritance
      if not Assigned(iClass.superclass) and (iClass <> RootClass) then
      begin
        if iClass.isAssociationClass then
          iClass.SetFirstParent(DefaultLinkSuperClass)
        else if iClass <> DefaultSuperClass then
          iClass.SetFirstParent(DefaultSuperClass)
        else
          iClass.SetFirstParent(RootClass);
        SetBoldifyTaggedValue(iClass.generalization[0], TAG_AUTOCREATED, TV_TRUE);
      end;
      BoldLog.ProgressStep;
    end;
  finally
    FreeAndNil(AllClasses);
    FreeAndNil(AllAssociations);
    BoldLog.EndLog;
  end;
end;

constructor TBoldUMLBoldify.Create;
begin
  fPluralSuffix := '';
  fDefaultNonNavigableMultiplicity := '0..*'; // do not localize
  fDefaultNavigableMultiplicity  := '0..1'; // do not localize
  fUnembedMulti := True;
  fMakeDerivedTransient := True;
end;

class function TBoldUMLBoldify.FullTag(const Tag: string): string;
begin
  Result :=  BOLDBOLDIFYPREFIX + Tag;
end;

class function TBoldUMLBoldify.GetBoldifyTaggedValue(element: TUMLModelElement; const Tag: string): string;
var
  taggedValue: TUMLTaggedValue;
begin
  taggedValue := element.taggedValue[FullTag(Tag)];
  if Assigned(taggedValue) then
    Result := taggedValue.Value
  else
    Result := '';
end;

class function TBoldUMLBoldify.GetDefaultLinkSuperClass(
  Model: TUMLModel): TUMLClass;
begin
{ TODO : change name to fullname when package support done. }
  Result := model.EvaluateExpressionAsDirectElement(Format('UMLClass.allInstances->select((model=self) and (name=''%s''))->first', [model.GetBoldTV(TAG_DEFAULTLINKCLASSSUPERCLASS)])) as TUMLClass; // do not localize
  if not Assigned(Result) then
    Result := GetDefaultSuperClass(model);
end;

class function TBoldUMLBoldify.GetDefaultSuperClass(
  Model: TUMLModel): TUMLClass;
begin
{ TODO : change name to fullname when package support done. }
  Result := model.EvaluateExpressionAsDirectElement
    (
    Format('UMLClass.allInstances->select((model=self) and (name=''%s''))->first', [model.GetBoldTV(TAG_DEFAULTSUPERCLASS)]) // do not localize
    ) as TUMLClass;
end;

class function TBoldUMLBoldify.GetRootClass(Model: TUMLModel): TUMLClass;
begin
  if not IsBoldified(model) then
    raise EBold.Create(sCanOnlyBeCalledIfBoldified);
  Result := model.EvaluateExpressionAsDirectElement('UMLClass.allInstances->select((model=self) and (generalization->isEmpty))->first') as TUMLClass; // do not localize
end;

class function TBoldUMLBoldify.IsAutoCreated(
  Element: TUMLModelElement): Boolean;
begin
  Result := TVIsTrue(GetBoldifyTaggedValue(Element, TAG_AUTOCREATED));
end;

class function TBoldUMLBoldify.IsBoldified(model: TUMLModel): Boolean;
begin
  Result := TVIsTrue(GetBoldifyTaggedValue(model, TAG_BOLDIFIED));
end;

class procedure TBoldUMLBoldify.RemoveBoldifyTaggedValue(element: TUMLModelElement; const Tag: string);
var
  taggedValue: TUMLTaggedValue;
begin
  taggedValue := element.taggedValue[FullTag(Tag)];
  if Assigned(taggedValue) then
    taggedValue.Delete;
end;

class procedure TBoldUMLBoldify.SetBoldifyTaggedValue(element: TUMLModelElement; const Tag: string; const Value: string);
begin
  TBoldUMLSupport.EnsuredTaggedValue(Element, FullTag(Tag)).Value := Value;
end;

class procedure TBoldUMLBoldify.SetRootClassname(Model: TUMLModel; name: string);
begin
  model.SetBoldTV(TAG_ROOTCLASS, name);
end;

class procedure TBoldUMLBoldify.UnBoldify(model: TUMLModel);
var
  i: integer;
  TempList: TUMLModelElementList;
  ToRemove: TUMLModelElementList;
  ToUnname: TUMLModelElementList;
  AssociationsToMakePersistent: TUMLAssociationList;
  AttributesToMakePersistent: TUMLAttributeList;
  AllAssociationEnds: TUMLAssociationEndList;
  AssociationEnd: TUMLAssociationEnd;
  TempClass: TUMLClass;
  Guard: IBoldGuard;
  function GetNewList(ClassName: string; TaggedValue: string = ''; Value: String = ''): TUMLModelElementList;
  var
    expr: string;
  begin
    expr := format('%s.allInstances->select(model=self)', [classname]); // do not localize
    if TaggedValue <> '' then
      expr := expr + format('->select(taggedValue[''%s''].value=''%s'')', [TaggedValue, Value]); // do not localize
    result := model.EvaluateExpressionAsNewElement(expr) as TUMLModelElementList;
  end;
begin
  Guard := TBoldGuard.Create(
    TempList,
    ToRemove, ToUnname,
    AllAssociationEnds,
    AssociationsToMakePersistent,
    AttributesToMakePersistent);

  SetRootClassname(model, GetRootClass(Model).name);

  // Remove all autocreated elements
  TempList := GetNewList('UMLModelElement', BOLDBOLDIFYPREFIX + TAG_AUTOCREATED, TV_TRUE); // do not localize

  // the result of the OCL-expression is immutable. Copy it to a new list
  ToRemove := TUMLModelElementList.Create;
  ToRemove.AddList(TempList);

  // Autocreated classes that have been modified should not be removed.
  for i := ToRemove.Count-1 downto 0 do
  begin
    if ToRemove[i] is TUMLClass then
    begin
      TempClass := ToRemove[i] as TUMLClass;
      if (TempClass.feature.Count > 0) or (TempClass.associationEnd.Count > 0) then
      begin
        TempClass.SetTaggedValue(BOLDBOLDIFYPREFIX + TAG_AUTOCREATED, TV_FALSE);
        ToRemove.RemoveByIndex(i);
      end;
    end;
  end;

  while ToRemove.Count > 0 do
    ToRemove[ToRemove.Count - 1].Delete;

  ToUnname := GetNewList('UMLModelElement', BOLDBOLDIFYPREFIX + TAG_NONAME, TV_TRUE); // do not localize

  for i := 0 to ToUnname.Count - 1 do
  begin
    ToUnname[i].name := '';
    RemoveBoldifyTaggedValue(ToUnname[i], TAG_NONAME);
  end;

  AssociationsToMakePersistent := GetNewlist('UMLAssociation', BOLDBOLDIFYPREFIX + TAG_WASPERSISTENT, TV_TRUE) as TUMLAssociationList; // do not localize
  for i := 0 to AssociationsToMakePersistent.Count - 1 do
  begin
    AssociationsToMakePersistent[i].persistent := True;
    RemoveBoldifyTaggedValue(AssociationsToMakePersistent[i], TAG_WASPERSISTENT); //!! Skall vara
  end;

  AttributesToMakePersistent := GetNewList('UMLAttribute', BOLDBOLDIFYPREFIX + TAG_WASPERSISTENT, TV_TRUE) as TUMLAttributeList; // do not localize
  for i := 0 to AttributesToMakePersistent.Count - 1 do
  begin
    AttributesToMakePersistent[i].persistent := True;
    RemoveBoldifyTaggedValue(AttributesToMakePersistent[i], TAG_WASPERSISTENT);
  end;

  AllAssociationEnds := GetNewList('UMLAssociationEnd') as TUMLAssociationEndList; // do not localize
  for i := 0 to AllAssociationEnds.Count - 1 do
  begin
    AssociationEnd := AllAssociationEnds[i];
    if TVisTrue(GetBoldifyTaggedValue(AssociationEnd, TAG_DEFAULTMULTIPLICITY)) then
    begin
      AssociationEnd.multiplicity := '';
      RemoveBoldifyTaggedValue(AssociationEnd, TAG_DEFAULTMULTIPLICITY);
    end;
    if TVisTrue(GetBoldifyTaggedValue(AssociationEnd, TAG_WASEMBEDED)) then
    begin
      AssociationEnd.SetBoldTV(TAG_EMBED, TV_TRUE);
      RemoveBoldifyTaggedValue(AssociationEnd, TAG_WASEMBEDED);
    end;
  end;
  RemoveBoldifyTaggedValue(model, TAG_BOLDIFIED);
end;

{ TBoldCopyAndClone }

class function TBoldCopyAndClone.BoldClone(SourceObject: TBoldObject; Mode: TBoldCopyMode; StripToolId: Boolean): TBoldObject;
begin
  if not Assigned(SourceObject) or SourceObject.BoldClassTypeInfo.IsLinkClass then
    Result := nil
  else
  begin
    Result := TBoldObjectclass(SourceObject.ClassType).InternalCreateNewWithClassAndSystem(SourceObject.BoldClasstypeInfo, SourceObject.BoldSystem, SourceObject.BoldPersistent);
    BoldCopy(Result, SourceObject, Mode, StripToolId);
  end;
end;

class procedure TBoldCopyAndClone.BoldCopy(DestinationObject, SourceObject: TBoldObject; Mode: TBoldCopyMode; StripToolId: Boolean);

  // Note, parameter Mode used in subprocedures
  procedure CopyAttribute(DestinationAttr, SourceAttr: TBoldAttribute);
  begin
    DestinationAttr.Assign(SourceAttr);
  end;

  procedure CopySingleRole(DestinationRole, SourceRole: TBoldObjectReference);
  var
    DestinationRtInfo: TBoldRoleRTInfo;
    DestinationLinkRole, SourceLinkRole: TBoldObjectReference;
  begin
    DestinationRtInfo := DestinationRole.BoldRoleRTInfo;
    case mode of
      bcmAttributes:
        ; // no action
      bcmShallow:
        if (DestinationRtInfo.Aggregation <> akComposite) and DestinationRtInfo.RoleRTInfoOfOtherEnd.IsMultiRole then
          DestinationRole.BoldObject := SourceRole.BoldObject;
      bcmDeep:
        if DestinationRtInfo.Aggregation = akComposite then
          DestinationRole.BoldObject := BoldClone(SourceRole.BoldObject, bcmDeep, StripToolId)
        else if DestinationRtInfo.RoleRTInfoOfOtherEnd.IsMultiRole then
          DestinationRole.BoldObject := SourceRole.BoldObject;
    end;
    if DestinationRtInfo.IsIndirect and assigned(DestinationRole.BoldObject) then
    begin
      SourceLinkRole := SourceObject.BoldMembers[DestinationRtInfo.IndexOfLinkObjectRole] as TBoldObjectReference;
      DestinationLinkRole := DestinationObject.BoldMembers[DestinationRtInfo.IndexOfLinkObjectRole] as TBoldObjectReference;
        BoldCopy(DestinationLinkRole.BoldObject, SourceLinkRole.BoldObject, Mode, StripToolId);
    end;
  end;

  procedure CopyMultiRole(DestinationRole, SourceRole: TBoldObjectList);
  var
    DestinationRtInfo: TBoldRoleRTInfo;
    i: integer;
    DestinationLinkRole, SourceLinkRole: TBoldObjectList;
    SourceSubObject: TBoldObject;
  begin
    DestinationRtInfo := DestinationRole.BoldRoleRTInfo;
    case mode of
      bcmAttributes:
        ; // no action
      bcmShallow:
        if (DestinationRtInfo.Aggregation <> akComposite) and DestinationRtInfo.RoleRTInfoOfOtherEnd.IsMultiRole then
          DestinationRole.AddList(SourceRole);
      bcmDeep:
      begin
        if DestinationRtInfo.Aggregation = akComposite then
        begin
          for i := 0 to SourceRole.Count - 1 do
          begin
            SourceSubObject := SourceRole[i];
            if (SourceSubObject is TUMLTaggedValue) and  (TUMLTaggedValue(SourceSubObject).Tag = BOLDINTERALTVPREFIX + TAG_TOOLID) then
              // do nothing, i.e. don't include toolid tag.
            else
              DestinationRole.Add(BoldClone(SourceSubObject, bcmDeep, StripToolId));
          end;
        end
        else if DestinationRtInfo.RoleRTInfoOfOtherEnd.IsMultiRole then
          DestinationRole.AddList(SourceRole);
      end;
    end;
    if DestinationRtInfo.IsIndirect and (DestinationRole.Count > 0) then
    begin
      SourceLinkRole := SourceObject.BoldMembers[DestinationRtInfo.IndexOfLinkObjectRole] as TBoldObjectList;
      DestinationLinkRole := DestinationObject.BoldMembers[DestinationRtInfo.IndexOfLinkObjectRole] as TBoldObjectList;
      for i := 0 to SourceLinkRole.Count - 1 do
        BoldCopy(DestinationLinkRole[i], SourceLinkRole[i], mode, StripToolId);
    end;
  end;

var
  m: integer;
  DestinationMember: TBoldMember;
  SourceMember: TBoldMember;
  SourceMemberRtInfo: TBoldMemberRtInfo;
  SourceRoleRTInfo: TBoldRoleRTInfo;
begin
  for m := 0 to SourceObject.BoldMemberCount - 1 do
  begin
    SourceMemberRtInfo := SourceObject.BoldClassTypeInfo.allMembers[m];
    if not SourceMemberRTInfo.IsDerived then
    begin
      DestinationMember := DestinationObject.BoldMembers[m];
      SourceMember := SourceObject.BoldMembers[m];
      if SourceMemberRtInfo.IsAttribute then
        CopyAttribute(DestinationMember as TBoldAttribute, SourceMember as TBoldAttribute)
      else if SourceMemberRTInfo.IsRole then
      begin
        SourceRoleRTInfo := SourceMemberRTInfo as TBoldRoleRTInfo;
        if SourceRoleRTInfo.RoleType = rtRole then
        begin
          if SourceRoleRTInfo.IsSingleRole then
            CopySingleRole(DestinationMember as TBoldObjectReference, SourceMember as TBoldObjectReference)
          else if SourceMemberRtInfo.IsMultiRole then
            CopyMultiRole(DestinationMember as TBoldObjectList, SourceMember as TBoldObjectList)
        end;
      end
      else
        raise EBoldInternal.CreateFmt(sUnknownTypeOfMember, [classname, SourceMember.DisplayName, SourceMember.Boldtype.ExpressionName]);
    end;
  end;
end;

class function TBoldUMLOperationSupport.ClassHasOperation(
  UMLClass: TUMLClassifier; MethodName: String;
  ParamTypes: TStringList): Boolean;
var
  Index, Index2: Integer;
  Operation: TUMLOperation;
begin
  // kala 990709 returns true if the name and signature are the same.
  Result := False;
  for Index := 0 to UMLClass.Feature.Count - 1 do
  begin
    if UMLClass.Feature[Index] is TUMLOperation then
    begin
      Operation := TUMLOperation(UMLClass.Feature[Index]);
      if SameText(Operation.Name, MethodName) then
      begin
        if (ParamTypes.Count = Operation.Parameter.Count) or
           (ParamTypes.Count = Operation.Parameter.Count - 1) then  // -1 special case if there is a return-param, return-params does not affect overloading. kala 990708
        begin
        Result := True;
          for Index2 := 0 to ParamTypes.Count - 1 do
          begin
            if (UpperCase(ParamTypes[Index2]) <> UpperCase(Operation.Parameter[Index2].typeName)) and
               (UpperCase(Operation.Parameter[Index2].typeName) <> UpperCase('Return')) then    // CHECKME // do not localize
            begin
              Result := False;
              Exit;
            end;
          end;
        end;
      end;
    end;
  end;
end;

class procedure TBoldUMLOperationSupport.ParameterTypesToList(
  UMLOperation: TUMLOperation; ParamTypes: TStringList);
var
  i: integer;
begin
  for i := 0 to UMLOperation.Parameter.Count - 1 do
    ParamTypes.Add(UMLOperation.Parameter[i].typeName);
end;

class procedure TBoldUMLOperationSupport.OverrideInAllSubclasses(UMLClass: TUMLClassifier; UMLOperation: TUMLOperation);
var
  i: Integer;
begin
  for i := 0 to UMLClass.SubClasses.Count - 1 do
     OverrideInAllSubclasses(UMLClass.SubClasses[i], UMLOperation);
  OverRideInClass(UMLClass, UMLOperation);
end;

class procedure TBoldUMLOperationSupport.OverrideInClass(
  UMLClass: TUMLClassifier; UMLOperation: TUMLOperation);
var
  NewMethod: TUMLOperation;
  ParamTypes: TStringList;
  BoldGuard: IBoldGuard;
begin
  BoldGuard := TBoldGuard.Create(ParamTypes);
  ParamTypes := TStringList.Create;
  ParameterTypesToList(UMLOperation, ParamTypes);
  if not ClassHasOperation(UMLClass, UMLOperation.Name, ParamTypes) then
  begin
    NewMethod := TUMLOperation(TBoldCopyAndClone.BoldClone(UMLOperation, bcmDeep, true));
    UMLClass.Feature.Add(NewMethod);
    NewMethod.SetBoldTV(TAG_DELPHIOPERATIONKIND, TV_DELPHIOPERATIONKIND_OVERRIDE);
  end;
end;

end.
