unit BoldUMLModelValidator;

interface

uses
  Classes,
  BoldTypeNameDictionary,
  BoldNameExpander,
  BoldDefs,
  BoldUMLModelSupport,
  BoldSystem,
  BoldMemberTypeDictionary,
  BoldTaggedValueSupport,
  BoldUMLAbstractModelValidator,
  BoldUMLModel;

type
  { forward declarations }
  TBoldUMLModelValidator = class;

  { TBoldUMLModelValidator }
  TBoldUMLModelValidator = class(TBoldUMLAbstractModelValidator)
  private
    fTypeNameDictionary: TBoldTypeNameDictionary;
    fSQLReservedWordList: TStringList;
    function GetSQLReservedWordList: TStringlist;
    function CheckAnsiSQLStuff: Boolean;
    function CheckDBStuff: Boolean;
    function ExpandedDBName(Elem: TUMLModelElement): String;
    function ExpandedSourceName(Elem: TUMLModelElement): String;
    function ExpandedExpressionName(Elem: TUMLModelElement): String;
    function NationalCharConversion: TBoldNationalCharConversion;
    procedure ValidateAssociation(association: TUMLAssociation);
    procedure ValidateAssociationEnd(associationEnd: TUMLAssociationEnd);
    procedure ValidateAttribute(attribute: TUMLAttribute);
    procedure ValidateClass(aClass: TUMLClass);
    procedure ValidateDuplicates(Model: TUMLModel);
    procedure ValidateFeature(feature: TUMLFeature);
    procedure ValidateNames(Element: TUMLModelElement; ElementName: string);
    procedure ValidateOperation(operation: TUMLOperation);
    procedure ValidateParameter(parameter: TUMLParameter);
    function IsSQLReservedWord(const S: string): Boolean;
    property SQLReservedWordList: TStringlist read GetSQLReservedWordList;
  public
    destructor Destroy; override;
    procedure Validate(TypeNameDictionary: TBoldTypeNameDictionary);
    property TypeNameDictionary: TBoldTypeNameDictionary read fTypeNameDictionary;
  end;

const
  beModelValidated = 1024;

implementation

uses
  SysUtils,
  BoldUtils,
  BoldLogHandler,
  BoldPMapperLists,
  BoldPMappers,
  BoldDefaultTaggedValues,
  BoldDefaultStreamNames,
  BoldUMLTypes;

resourcestring
  // Validator errors
  sUMVModelNameEmpty = 'Model must have a name';
  sUMVModelUnknownMapper = 'Unknown system persistence mapper "%s" in model "%s"';
  sUMVClassNameEmpty = 'Class must have a name';
  sUMVClassNameExists = 'Duplicate class name "%s"';
  sUMVClassUnknownMapper = 'Unknown object persistence mapper "%s" in class "%s"';
  sUMVDelphiNameEmpty = '"%s" must have a Delphi name';
  sUMVDelphiNameInvalid = 'Invalid Delphi name "%s" in "%s"';
  sUMVDelphiNameReserved = 'Delphi name "%s" in "%s" is a reserved word';
  sUMVDelphiNameExists = 'Duplicate Delphi name "%s" in "%s"';
  sUMVDelphiNameExists2 = 'Duplicate Delphi name "%s" in "%s": "%s" and "%s"';
  sUMVCppNameEmpty = '"%s" must have a C++ name';
  sUMVCppNameInvalid = 'Invalid C++ name "%s" in "%s"';
  sUMVCppNameReserved = 'C++ name "%s" in "%s" is a reserved word';
  sUMVCppNameExists = 'Duplicate C++ name "%s" in "%s"';
  sUMVCppNameExists2 = 'Duplicate C++ name "%s" in "%s": "%s" and "%s"';

  sUMVUnitNameEmpty = '"%s" must have a unit name';
  sUMVExpressionNameEmpty = '"%s" must have an expression name';
  sUMVExpressionNameInvalid = 'Invalid expression name "%s" in "%s"';
  sUMVExpressionNameOCLReserved = 'Expression name "%s" in "%s" is a reserved word in OCL';
  sUMVExpressionNameExists = 'Duplicate expression name "%s" in "%s""';
  sUMVExpressionNameExists2 = 'Duplicate expression name "%s" in "%s": "%s" and "%s"';
  sUMVSQLTableNameEmpty = '"%s" must have a table name';
  sUMVSQLColumnNameEmpty = '"%s" must have a column name';
  sUMVTableNameInvalid = 'Invalid table name "%s" in "%s"';
  sUMVTableNameSQLReserved = 'Table name "%s" in "%s" is a reserved word in SQL';
  sUMVTableNameExists = 'Duplicate table name "%s" in "%s" and "%s"';
  sUMVColumnNameInvalid = 'Invalid column name "%s" in "%s"';
  sUMVColumnNameSQLReserved = 'Column name "%s" in "%s" is a reserved word in SQL';
  sUMVColumnNameExists = 'Duplicate column name "%s" in "%s": "%s" and "%s"';
  sUMVMemberNameEmpty = 'Member must have a name in "%s"';
  sUMVMemberNameExists = 'Duplicate member name "%s" in "%s": "%s" and "%s"';
  sUMVAttributeUnknownType = 'Attribute "%s" has unknown type (%s)';
  sUMVAttributeUnknownMapper = 'Attribute "%s" has unknown persistence mapper (%s)';
  sUMVAttributeCantStore = 'Attribute "%s" can''t be stored, incompatible persistence mapper';
  sUMVAssociationEndIsMultiWithOtherEndComposite = 'Association end "%s" is multi, but other end is composite';
  sUMVAssociationEndUnknownClass = 'Association end "%s" in association "%s" not associated with any class';
  sUMVAssociationEndUnknownMapper = 'Unknown association end persistence mapper (%s) in association "%s"';
  sUMVAssociationEndCantStore = 'Association end persistence mapper %s cannot store %s in association "%s"';
  sUMVInvalidAssociationEndIndirectAndEmbed = 'Association end "%s" in association "%s" is indirect and embedded';
  sUMVInvalidAssociationEndMultiAndEmbed = 'Association end "%s" in association "%s" is multi and embedded';
  sUMVInvalidAssociationEndOrderedandSingle = 'Association end "%s" in association "%s" is non-multi and ordered';
  sUMVSingleAssociationEndsEmbeddedInBothEnds = 'Association "%s"s both ends are embedded and single-single ends.';
  sUMVAssociationNeedsTwoRoles = 'Association "%s" must have two assocationEnds';
  sUMVAssociationEndNeedsType = 'AssociationEnd "%s" is not assocatied with any class';
  sUMVDerivedAssociationCanNotHaveClass = 'Derived association "%s" can not have an association class';
  sUMVAssociationM2MNeedsClass = 'Multi to multi association "%s" must have association class';
  sUMVAssociationRolesNeedClass = 'Non-embedded roles "%s" and "%s" need association class';
  sUMVAssociationClassCannotBePart = 'Association class for "%s" cannot be one of the classes in the association';
  sUMVPersistenceSubClass = 'Persistent class "%s" has non-persistent superclass';
  sUMVParentMappedClass = 'Parent mapped class "%s" must have superclass';
  sUMVChildrenMappedClass = 'Children mapped class "%s" must be abstract';
  sUMVImportedMappedClass = 'Imported mapped class "%s" must be imported';
  sUMVRecursiveMapping = 'Recursive mapping between "%s" and "%s"';
  sUMVParentMappedWithNotNullAttribute = 'Parent mapped class "%s" with NotNull attributes';
  sUMVAttributeDerivedAndPersistent = 'Attribute "%s" is both derived and persistent';
  sUMVAssociationDerivedAndPersistent = 'Association "%s" is both derived and persistent';
  sUMVAssociationWithTwoAggregatedEnds = 'Association "%s" has two ends that are aggregated';
  sUMVBoldTableReservedWord = 'Table name "%s" in "%s" is a "bold reserved" word';
  sUMVBoldColumnReservedWord = 'Column name "%s" in "%s" is a "bold reserved" word';
  sUMVDuplicateAssociationEndName = 'AssociationEnds in Association %s are both named %s';
  sUMVVersionedClassWithNonVersionedSuperClass = 'Class "%s" is versioned, but its superclass is not';
  sUMVAssociationEndIsSingleAndNotNAvigable = 'AssociationEnd %s is single and not navigable';
  sUMVAssociationEndIllegalQualifier = 'Qualifier %s in associationEnd %s does not have matching attribute in class of other end';
  sUMVAssociationEndQualifiedAndMulti = 'AssociationEnd %s is qualified and multi. Not supported';
  sUMVRootClassMissing = 'Root Class %s is missing';
  sUMVRootClassMustHaveName = 'Root class must have name';
  sUMVLinkClassWithSuperClassAsEnd = 'LinkClass (%s) must not inherit from any of the ends (%s)';
  sUMVAssociationAndClassNotEquallyPersistent = 'Association (%s) and its association class are not equally persistent';

function IsValidDelphiIdentifier(const Ident: string): Boolean;
const
  Alpha = ['A'..'Z', 'a'..'z', '_'];
  AlphaNumeric = Alpha + ['0'..'9'];
var
  I: Integer;
begin
  Result := False;
  if (Length(Ident) = 0) or not (Ident[1] in Alpha) then
    Exit;
  for I := 2 to Length(Ident) do if not (Ident[I] in AlphaNumeric) then
    Exit;
  Result := True;
end;

function IsValidCppIdentifier(const Ident: string): Boolean;
const
  Alpha = ['A'..'Z', 'a'..'z', '_'];
  AlphaNumeric = Alpha + ['0'..'9'];
var
  I: Integer;
begin
  Result := False;
  if (Length(Ident) = 0) or not (Ident[1] in Alpha) then
    Exit;
  for I := 2 to Length(Ident) do if not (Ident[I] in AlphaNumeric) then
    Exit;
  Result := True;
end;



function IsValidOCLIdentifier(const Ident: string): Boolean;
begin
  Result := (IsValidDelphiIdentifier(Ident)) and (Ident[1] <> '_');
end;

function IsValidSQLIdentifier(const Ident: string): Boolean;
const
  Alpha = ['A'..'Z', 'a'..'z', '$', '_'];
  AlphaNumeric = Alpha + ['0'..'9'];
var
  I: Integer;
begin
  Result := False;
  if (Length(Ident) = 0) or not (Ident[1] in Alpha) then
    Exit;
  for I := 2 to Length(Ident) do if not (Ident[I] in AlphaNumeric) then
    Exit;
  Result := True;
end;

function IsDelphiReservedWord(const S: string): Boolean;
const
  Delphi3ReservedCount = 66;
  Delphi3Reserved: array[1..Delphi3ReservedCount] of string = (
    'and', 'array', 'as', 'asm', 'begin', 'case', 'class', 'const', 'constructor', 'destructor',
    'dispinterface', 'div', 'do', 'downto', 'else', 'end', 'except', 'exports', 'file', 'finalization',
    'finally', 'for', 'function', 'goto', 'if', 'implementation', 'in', 'inherited', 'initialization', 'inline',
    'interface', 'is', 'label', 'library', 'mod', 'nil', 'not', 'object', 'of', 'or',
    'out', 'packed', 'procedure', 'program', 'property', 'raise', 'record', 'repeat', 'resourcestring', 'set',
    'shl', 'shr', 'string', 'stringresource', 'then', 'threadvar', 'to', 'try', 'type', 'unit',
    'until', 'uses', 'var', 'while', 'with', 'xor');
  Delphi3DirectiveCount = 32;
  Delphi3Directives: array[1..Delphi3DirectiveCount] of string = (
    'absolute', 'abstract', 'assembler', 'automated', 'cdecl', 'default', 'dispid', 'dynamic', 'export', 'external',
    'far', 'forward', 'index', 'message', {'name', }'near', 'nodefault', 'override', 'pascal', 'private',
    'protected', 'public', 'published', 'read', 'readonly', 'register', 'resident', 'safecall', 'stdcall', 'stored',
    'virtual', 'write', 'writeonly');
var
  I: Integer;
begin
  Result := False;
  if Length(S) = 0 then
    Exit;
  for I := 1 to Delphi3ReservedCount do
    if CompareText(S, Delphi3Reserved[I]) = 0 then
    begin
      Result := True;
      Exit;
    end;
  for I := 1 to Delphi3DirectiveCount do
    if CompareText(S, Delphi3Directives[I]) = 0 then
    begin
      Result := True;
      Exit;
    end;
end;

function IsCppReservedWord(const S: string): Boolean;
const
  CppReservedCount = 1;
  CppReserved: array[1..CppReservedCount] of string = ('if');
var
  I: Integer;
begin
  Result := False;
  if Length(S) = 0 then
    Exit;
  for I := 1 to CppReservedCount do
    if CompareText(S, CppReserved[I]) = 0 then
    begin
      Result := True;
      Exit;
    end;

end;


function IsBoldReservedWord(const S: String): Boolean;
const
  BoldReservedCount = 2;
  BoldReserved: array[1..BoldReservedCount] of String = (
    'BOLD_ID', 'BOLD_TYPE');
var
  I: Integer;
begin
  Result := False;
  for I := 1 to BoldReservedCount do
    if CompareText(S, BoldReserved[I]) = 0 then
    begin
      Result := True;
      Exit;
    end;
end;

function IsOCLReservedWord(const S: string): Boolean;
const
  OCLReservedCount = 7;
  OCLReserved: array[1..OCLReservedCount] of string = (
    'and', 'div', 'implies', 'mod', 'not', 'or', 'xor');
var
  I: Integer;
begin
  Result := False;
  for I := 1 to OCLReservedCount do
    if CompareText(S, OCLReserved[I]) = 0 then
    begin
      Result := True;
      Exit;
    end;
end;

function ContainsClass(Model: TUMLModel; name: String): Boolean;
var
  i: Integer;
begin
  result := False;
  for i := 0 to Model.Classes.Count - 1 do
    if CompareText(name, (Model.Classes[i] as TUMLClass).Name) = 0 then
    begin
      Result := True;
      exit;
    end;
end;

function TBoldUMLModelValidator.ExpandedSourceName(Elem: TUMLModelElement): String;
begin
  case Language of
    mvslDelphi: result := BoldExpandName(Elem.GetBoldTV(TAG_DELPHINAME), Elem.Name, xtDelphi, -1, NationalCharConversion);
    mvslCpp: result := BoldExpandName(Elem.GetBoldTV(TAG_CPPNAME), Elem.Name, xtDelphi, -1, NationalCharConversion);
    else result := '';
  end;
end;

function TBoldUMLModelValidator.ExpandedExpressionName(Elem: TUMLModelElement): String;
begin
  result := BoldExpandName(Elem.GetBoldTV(TAG_EXPRESSIONNAME), Elem.Name, xtExpression, -1, NationalCharConversion);
end;

function TBoldUMLModelValidator.ExpandedDBName(Elem: TUMLModelElement): String;
var
  Source: String;
begin
  if Elem is TUMLClass then
    Source := Elem.GetBoldTV(TAG_TABLENAME)
  else
    Source := elem.GetBoldTV(TAG_COLUMNNAME);

  result := BoldExpandPrefix(Source, Elem.Name, SQLDataBaseConfig.SystemTablePrefix,
      -1,
      NationalCharConversion);
end;

procedure TBoldUMLModelValidator.Validate(TypeNameDictionary: TBoldTypeNameDictionary);
var
  I: Integer;
  Names: TStringList;
  SourceCodeNames: TStringList;
  ExpressionNames: TStringList;
  TableNames: TStringList;
  Mapper: String;
  ErrorStr: String;
begin
  if not(TBoldUMLBoldify.IsBoldified(UMLModel) and TBoldUMLSupport.IsFlattened(UMLModel)) then
    raise EBoldInternal.Create('Model not Boldified and flattened');

  fTypeNameDictionary := TypeNameDictionary;

  ClearViolations;
  BoldLog.StartLog('Validating the model');

  if not assigned(TypeNameDictionary) then
    addError('No TypeNameDictionary available', [], UMLModel);

  if Assigned(UMLModel) then
  begin
    with UMLModel do
    begin
      if Name = '' then
        AddError(sUMVModelNameEmpty, [], UMLModel);

      ValidateNames(UMLModel, UMLModel.Name);

      Mapper := GetBoldTV(TAG_PMAPPERNAME);

      if not SameText(Mapper, DEFAULTNAME) and CheckDbStuff then
      begin
        if not Assigned(BoldSystemPersistenceMappers.DescriptorByName[Mapper]) then
          AddError(sUMVModelUnknownMapper, [Mapper, Name], UMLModel);
      end;

      Names := TStringList.Create;
      SourceCodeNames := TStringList.Create;
      ExpressionNames := TStringList.Create;
      TableNames := TStringList.Create;

      for I := 0 to Classes.Count - 1 do
      begin
          Names.AddObject(AnsiUpperCase(Classes[i].Name), Classes[i]);
          SourceCodeNames.AddObject(AnsiUpperCase(ExpandedSourceName(Classes[i])), Classes[i]);
          ExpressionNames.AddObject(AnsiUpperCase(Classes[i].ExpandedExpressionName), Classes[i]);
          if CheckDBStuff and Classes[i].Persistent then
            TableNames.AddObject(AnsiUpperCase(ExpandedDBName(Classes[i])), Classes[i]);
      end;
      Names.Sort;
      SourceCodeNames.Sort;
      ExpressionNames.Sort;
      TableNames.Sort;
      for i := 0 to Classes.Count-2 do
      begin
        if Names[i] = Names[i + 1] then
          AddError(sUMVClassNameExists, [(Names.Objects[i] as TUMLClass).Name], Names.Objects[i] as TUMLClass);

        if (Language <> mvslNone) and (SourceCodeNames[i] = SourceCodeNames[i + 1]) then
        begin
          case Language of
            mvslDelphi: ErrorStr := sUMVDelphiNameExists;
            mvslCpp: ErrorStr := sUMVCppNameExists
            else ErrorStr := 'Unknown source language in validator';
          end;
          AddError(ErrorStr, [ExpandedSourceName(SourceCodeNames.Objects[i] as TUMLClass),
                                          (SourceCodeNames.Objects[i] as TUMLClass).Name],
                                          SourceCodeNames.Objects[i] as TUMLClass)
        end;


        if ExpressionNames[i] = ExpressionNames[i + 1] then
          AddError(sUMVExpressionNameExists, [(ExpressionNames.Objects[i] as TUMLClass).ExpandedExpressionName,
                                              (ExpressionNames.Objects[i] as TUMLClass).Name],
                                              ExpressionNames.Objects[i] as TUMLClass);

        if (i < TableNames.Count - 1) and (TableNames[i] = TableNames[i + 1]) and CheckDbStuff then
          AddError(sUMVTableNameExists,
            [ExpandedDBName(TableNames.Objects[i] as TUMLClass),
             (TableNames.Objects[i] as TUMLClass).Name,
             (TableNames.Objects[i + 1] as TUMLClass).Name],
            TableNames.Objects[i] as TUMLClass);
      end;
      Names.Free;
      SourceCodeNames.Free;
      ExpressionNames.Free;
      TableNames.Free;

      BoldLog.ProgressMax := UMLModel.Classes.Count + UMLModel.Associations.Count;

      for I := 0 to UMLModel.Classes.Count - 1 do
      begin
        ValidateClass(UMLModel.Classes[I]);
        BoldLog.ProgressStep;
      end;
      for I := 0 to UMLModel.Associations.Count - 1 do
      begin
        ValidateAssociation(UMLModel.Associations[I]);
        BoldLog.ProgressStep;
      end;
      ValidateDuplicates(UMLModel);
    end;
  end;
  BoldLog.EndLog;
  UMLModel.SendEvent(beModelValidated);
end;

procedure TBoldUMLModelValidator.ValidateAttribute(attribute: TUMLAttribute);
var
  MemberClass: TClass;
  MemberPMapperDescriptor: TBoldMemberPersistenceMapperDescriptor;
  DelphiTypeName: String;
  Mappername: String;
  TypeDescriptor: TBoldMemberTypeDescriptor;
  Mapping: TBoldTypeNameMapping;
begin
  ValidateFeature(attribute);
  if attribute.Name = '' then
    Exit;

  with attribute do
  begin
    Mapping := TypeNameDictionary.MappingForModelName[typeName];
    if not assigned(Mapping) then
      AddError(sUMVAttributeUnknownType, [Owner.Name + '.' + Name, typeName], attribute)
    else
    begin
      if Mapping.ExpressionName = DEFAULTNAME then
        AddHint('%s uses default type mapping (type: %s)', [Owner.Name + '.' + Name, typeName], attribute);

      DelphiTypeName := Mapping.ExpandedDelphiName;
      TypeDescriptor := BoldMemberTypes.DescriptorByDelphiName[DelphiTypeName];

      if Assigned(TypeDescriptor) then
        MemberClass := TypeDescriptor.MemberClass
      else
        MemberClass := nil;
      if not assigned(memberClass) then
        AddHint('%s: No attribute class installed for %s', [Owner.Name + '.' + Name, delphiTypeName], attribute);

      MapperName := GetBoldTV(TAG_PMAPPERNAME);
      if not SameText(MapperName, DEFAULTNAME) and EffectivePersistent and CheckDBStuff then
      begin
        MemberPMapperDescriptor := BoldMemberPersistenceMappers.DescriptorForModelNameWithDefaultSupport(
            typeName,
            MapperName,
            TypeNameDictionary);
        if not Assigned(MemberPMapperDescriptor) then
          AddError(sUMVAttributeUnknownMapper, [Owner.Name + '.' + Name, MapperName], attribute);

        if Assigned(MemberPMapperDescriptor) and
          (not MemberPMapperDescriptor.CanStore(Mapping.ExpandedContentsName)) then
          AddError(sUMVAttributeCantStore, [Owner.Name + '.' + Name], attribute);
      end;

      if CheckDbStuff and Attribute.Derived and Attribute.EffectivePersistent then
        AddHint(sUMVAttributeDerivedAndPersistent, [Owner.Name + '.' + Name], Attribute);

    end;
  end;
end;

procedure TBoldUMLModelValidator.ValidateClass(aClass: TUMLClass);
var
  Mapper: String;
  temp: TUMLClassifier;
  I: Integer;

  function HasNotNullMembers(aClass: TUMLClass): Boolean;
  var
    I: Integer;
    Attr: TUMLAttribute;
  begin
    Result := False;
    for I := 0 to aClass.Feature.Count - 1 do
      if aClass.Feature[i] is TUMLAttribute then
      begin
      	attr := aClass.Feature[I] as TUMLAttribute;
      	if Attr.EffectivePersistent and TVIsFalse(Attr.GetBoldTV(TAG_ALLOWNULL)) then
      	  result := true;
     end;
  end;

begin
  // check model name
  if aClass.Name = '' then
  begin
    AddError(sUMVClassNameEmpty, [], aClass);
    Exit;
  end;
  ValidateNames(aClass, aClass.Name);

  if aClass.Persistent and CheckDBStuff then
  begin
    if (aClass.GetBoldTV(TAG_TABLEMAPPING) = TV_TABLEMAPPING_IMPORTED) and
       TVIsFalse(aClass.GetBoldTV(TAG_IMPORTED)) then
      AddError(sUMVImportedMappedClass, [aClass.Name], AClass);

    if (aClass.GetBoldTV(TAG_TABLEMAPPING) = TV_TABLEMAPPING_CHILDREN) and not aClass.isAbstract then
      AddError(sUMVChildrenMappedClass, [aClass.name], AClass);

    if not Assigned(aClass.SuperClass) and (aClass.GetBoldTV(TAG_TABLEMAPPING) = TV_TABLEMAPPING_PARENT) then
      AddError(sUMVParentMappedClass, [aClass.Name], AClass);

    if (aClass.GetBoldTV(TAG_TABLEMAPPING) = TV_TABLEMAPPING_PARENT) and HasNotNullMembers(aClass) then
      AddWarning(sUMVParentMappedWithNotNullAttribute, [aClass.name], AClass);

    if Assigned(aClass.SuperClass) and not aClass.SuperClass.Persistent then
      AddError(sUMVPersistenceSubClass, [aClass.Name], AClass);

    if Assigned(aClass.SuperClass) and aClass.SuperClass.Persistent and
       (aClass.GetBoldTV(TAG_TABLEMAPPING) = TV_TABLEMAPPING_PARENT) and
       (aClass.SuperClass.GetBoldTV(TAG_TABLEMAPPING) = TV_TABLEMAPPING_CHILDREN) then
      AddError(sUMVRecursiveMapping, [aClass.Name, aClass.SuperClass.Name], AClass);

    Mapper := aClass.GetBoldTV(TAG_PMAPPERNAME);
    if not SameText(Mapper, DEFAULTNAME) and aClass.Persistent then
    begin
      if not Assigned(BoldObjectPersistenceMappers.DescriptorByName[Mapper]) then
        AddError(sUMVClassUnknownMapper, [Mapper, aClass.Name], AClass);
    end;
  end;

  if assigned(aClass.SuperClass) and TVIsTrue(aClass.GetBoldTV(TAG_VERSIONED)) and
    not TVisTrue(aClass.SuperClass.GetBoldTV(TAG_VERSIONED)) then
      AddError(sUMVVersionedClassWithNonVersionedSuperClass, [aClass.Name], AClass);

  for I := 0 to aClass.Feature.Count - 1 do
  begin
    if aClass.Feature[i] is TUMLAttribute then
      ValidateAttribute(aClass.Feature[I] as TUMLAttribute)
    else
      ValidateOperation(aClass.Feature[I] as TUMLOperation);
  end;
  if assigned(aClass.Association) then
    for i := 0 to aClass.association.connection.Count-1 do
    begin
      temp := aClass;
      while assigned(temp) do
      begin
        if temp = aClass.association.connection[i].type_ then
          AddError(sUMVLinkClassWithSuperClassAsEnd, [aClass.Name, temp.Name], AClass);
        temp := temp.SuperClass;
      end;
    end;
end;

procedure TBoldUMLModelValidator.ValidateDuplicates(Model: TUMLModel);
var
  Names: TStringList;
  SourceNames: TStringList;
  ExpressionNames: TStringList;
  DatabaseNames: TStringList;

procedure CheckAndAddName(StringList: TStringList; OwningElement: string; Name: String; Element: TUMLModelElement; ErrorMsg: String; var Reported: Boolean; RememberReport: Boolean);
var
  index: integer;
  function GetParent(elem: TUMLModelElement): String;
  begin
    if (elem is TUMLFeature) then
      result := (elem as TUMLFeature).Owner.Name + '.' + name
    else if elem is TUMLAssociationEnd  and Assigned((elem as TUMLAssociationEnd).OtherEnd) then
      result := (elem as TUMLAssociationEnd).OtherEnd.Type_.Name + '.' + name
    else
      result := '';
  end;
begin
  Index := stringList.IndexOf(Name);
  if Index <> -1 then
    if not reported and not ((StringList.Objects[Index] is TUMLOperation) and (Element is TUMLOperation)) then
    begin
      AddError(ErrorMsg, [name, OwningElement, GetParent(element), GetParent(StringList.Objects[Index] as TUMLModelElement)], Element);
      AddError(ErrorMsg, [name, OwningElement, GetParent(StringList.Objects[Index] as TUMLModelElement), GetParent(element)], StringList.Objects[Index] as TUMLModelElement);
      if rememberReport then
        reported := true;
    end;
  StringList.AddObject(Name, element);
end;

procedure TraverseClass(aClass: TUMLClass);
var
  i: integer;
  reported: Boolean;
  nameCount, SourceCount, dbCount, ExprCount: integer;

procedure CheckAndAddAssociationEnd(AssoEnd: TUMLAssociationEnd);
begin
  if not AssoEnd.isNavigable and AssoEnd.Association.Derived then
    // do nothing, this asso-end will not exist in real life...
  else
  begin
    Reported := false;
    CheckAndAddName(Names, aClass.name, AssoEnd.Name, AssoEnd, sUMVMemberNameExists, reported, true);
    CheckAndAddName(ExpressionNames, aClass.name, AssoEnd.ExpandedExpressionName, AssoEnd, sUMVExpressionNameExists2, reported, false);
    case Language of
      mvslDelphi: CheckAndAddName(SourceNames, aClass.name, ExpandedSourceName(AssoEnd), AssoEnd, sUMVDelphiNameExists2, reported, false);
      mvslCpp: CheckAndAddName(SourceNames, aClass.name, ExpandedSourceName(AssoEnd), AssoEnd, sUMVCppNameExists2, reported, false);
    end;
    if AssoEnd.Association.EffectivePersistent and CheckDBStuff then
      CheckAndAddName(DatabaseNames, aClass.name, ExpandedDBName(AssoEnd), AssoEnd, sUMVColumnNameExists, reported, false);
  end;
end;

begin
  dbCount := DatabaseNames.Count;
  NameCount := Names.Count;
  SourceCount := SourceNames.Count;
  ExprCount := ExpressionNames.Count;

  for i := 0 to aClass.Feature.Count - 1 do
  begin
    Reported := false;
    CheckAndAddName(Names, aClass.Name, aClass.Feature[i].Name, aClass.Feature[i], sUMVMemberNameExists, reported, true);
    CheckAndAddName(ExpressionNames, aClass.Name, aClass.Feature[i].ExpandedExpressionName, aClass.Feature[i], sUMVExpressionNameExists2, reported, false);
    case language of
      mvslDelphi: CheckAndAddName(SourceNames, aClass.Name, ExpandedSourceName(aClass.Feature[i]), aClass.Feature[i], sUMVDelphiNameExists2, reported, false);
      mvslCpp: CheckAndAddName(SourceNames, aClass.Name, ExpandedSourceName(aClass.Feature[i]), aClass.Feature[i], sUMVCppNameExists2, reported, false);
    end;

    if (aClass.Feature[i] is TUMLAttribute) and
       (aClass.Feature[i] as TUMLAttribute).EffectivePersistent and
       CheckDbStuff and
       (aClass.Feature[i].GetBoldTV(TAG_STORAGE) <> TV_STORAGE_EXTERNAL) then
      CheckAndAddName(DatabaseNames, aClass.Name, ExpandedDBName(aClass.Feature[i]), aClass.Feature[i], sUMVColumnNameExists, reported, false);
  end;

  for I := 0 to aClass.AssociationEnd.Count - 1 do
    if (aClass.AssociationEnd[i].association.GetBoldTV(TAG_STORAGE) <> TV_STORAGE_EXTERNAL) then
      CheckAndAddAssociationEnd(aClass.AssociationEnd[i].otherEnd);

  // check the names of innerlinks
  if assigned(aClass.association) then
  begin
    CheckAndAddAssociationEnd(aClass.Association.connection[0]);
    CheckAndAddAssociationEnd(aClass.Association.connection[1]);
  end;

  for i := 0 to aClass.Subclasses.Count - 1 do
    TraverseClass(aClass.Subclasses[i] as TUMLClass);

  while DatabaseNames.Count > dbCount do
    DatabaseNames.Delete(dbCount);
  while Names.count > NameCount do
    Names.Delete(NameCount);
  while SourceNames.count > SourceCount do
    SourceNames.Delete(SourceCount);
  while ExpressionNames.count > ExprCount do
    ExpressionNames.Delete(ExprCount);
end;

begin
  Names := TStringList.Create;
  SourceNames := TStringList.Create;
  ExpressionNames := TStringList.Create;
  DatabaseNames := TStringList.Create;
  if not assigned(TBoldUMLBoldify.GetRootClass(Model)) then
    AddError(sUMVRootClassMissing, [model.GetBoldTV(TAG_ROOTCLASS)], Model)
  else
    traverseClass(TBoldUMLBoldify.GetRootClass(Model));
  Names.Free;
  SourceNames.Free;
  ExpressionNames.Free;
  DatabaseNames.Free;
end;

procedure TBoldUMLModelValidator.ValidateAssociation(association: TUMLAssociation);
var
  i: Integer;
  end0, end1: TUMLAssociationEnd;
begin
  with association do
  begin
    if Connection.Count <> 2 then
    begin
      AddError(sUMVAssociationNeedsTwoRoles, [Name], association);
      Exit;
    end;

    end0 := Connection[0] as TUMLAssociationEnd;
    end1 := Connection[1] as TUMLAssociationEnd;

    if not Derived and end0.Multi and end1.Multi and not Assigned(Class_) then
      AddError(sUMVAssociationM2MNeedsClass, [Name], association);

    if assigned(Class_) and (Class_.persistent <> Association.persistent) then
      AddError(sUMVAssociationAndClassNotEquallyPersistent, [Name], association);

    if Derived and Assigned(Class_) then
      AddError(sUMVDerivedAssociationCanNotHaveClass, [Name], association);

    if not derived and (TVIsFalse(end0.GetBoldTV(TAG_EMBED)) and TVIsFalse(end1.GetBoldTV(TAG_EMBED))) and
        not Assigned(Class_) then
      AddError(sUMVAssociationRolesNeedClass, [end0.Name, end1.Name], association);

    if (Assigned(Class_) and ((end0.Type_ = Class_) or (end1.Type_ = Class_))) then
      AddError(sUMVAssociationClassCannotBePart, [Name], association);

    if CheckDbStuff and association.Derived and association.EffectivePersistent then
      AddHint(sUMVAssociationDerivedAndPersistent, [association.Name], Association);

    if (end0.Aggregation <> akNone) and (end1.Aggregation <> akNone) then
      AddError(sUMVAssociationWithTwoAggregatedEnds, [Name], association);

    if end0.Name = end1.Name then
      AddError(sUMVDuplicateAssociationEndName, [Association.Name, end0.Name], Association);

    if CheckDbStuff and (not (end0.Multi or end1.Multi)) and
       TVIsTrue(end0.GetBoldTV('Embed')) and
       TVIsTrue(end1.GetBoldTV('Embed')) then
      AddHint(sUMVSingleAssociationEndsEmbeddedInBothEnds, [Association.name], Association);

    for I := 0 to Connection.Count - 1 do
      ValidateAssociationEnd(Connection[I] as TUMLAssociationEnd);
  end;
end;

procedure TBoldUMLModelValidator.ValidateOperation(operation: TUMLOperation);
var
  i: Integer;
begin
  ValidateFeature(operation);
  for i := 0 to operation.Parameter.Count - 1 do
    ValidateParameter(operation.Parameter[i] as TUMLParameter);
end;

procedure TBoldUMLModelValidator.ValidateParameter(parameter: TUMLParameter);
begin
end;

procedure TBoldUMLModelValidator.ValidateAssociationEnd(associationEnd: TUMLAssociationEnd);
var
  StreamName: String;
  PMapperDescriptor: TBoldMemberPersistenceMapperDescriptor;
  Mapper: String;
  i: integer;
begin
  ValidateNames(AssociationEnd, AssociationEnd.Association.Name + '.' + AssociationEnd.Name);

  if not Assigned(AssociationEnd.Type_) then
  begin
    AddError(sUMVAssociationEndNeedsType, [AssociationEnd.Name], AssociationEnd);
    Exit;
  end;

  if (AssociationEnd.Qualifier.Count > 0) and
    (GetUpperLimitForMultiplicity(AssociationEnd.multiplicity) > 1) then
    AddError(sUMVAssociationEndQualifiedAndMulti, [AssociationEnd.Name], AssociationEnd);

  for i := 0 to AssociationEnd.Qualifier.Count - 1 do
    if AssociationEnd.Type_.EvaluateExpressionAsDirectElement(
       Format('allFeature->filterOnType(UMLAttribute)->select((name=''%s'') and (typeName=''%s''))->first',
       [AssociationEnd.Qualifier[i].Name, AssociationEnd.Qualifier[i].typeName]
       )) = nil then
      AddError(sUMVAssociationEndIllegalQualifier, [AssociationEnd.Qualifier[i].Name, AssociationEnd.Name], AssociationEnd.Qualifier[i]);

  if associationEnd.Name = '' then Exit;

  if not Assigned(associationEnd.Type_) then
  begin
    AddError(sUMVAssociationEndUnknownClass, [associationEnd.Name, associationEnd.Association.Name], associationEnd);
    Exit;
  end;

  if associationEnd.IsNavigable and associationEnd.Multi and (associationEnd.OtherEnd.Aggregation = akComposite) then
    AddError(sUMVAssociationEndIsMultiWithOtherEndComposite, [associationEnd.Name], AssociationEnd);

  if not associationEnd.Multi and not associationEnd.IsNavigable then
    AddHint(sUMVAssociationEndIsSingleAndNotNavigable, [associationEnd.Name], AssociationEnd);

  if not associationEnd.Multi and associationEnd.isOrdered then
    addHint(sUMVInvalidAssociationEndOrderedandSingle, [associationEnd.Name,
                                                         associationEnd.Association.name],
                                                        associationEnd);

  if assigned(associationend.Association.Class_) and
    StringToBoolean(AssociationEnd.GetBoldTV(TAG_EMBED)) then
    addHint(sUMVInvalidAssociationEndIndirectAndEmbed, [associationEnd.Name,
                                                         associationEnd.Association.name], associationEnd);

  if not (AssociationEnd.association.Derived and not AssociationEnd.isNavigable)and
    associationEnd.multi and
    StringToBoolean(AssociationEnd.GetBoldTV(TAG_EMBED)) then
    addHint(sUMVInvalidAssociationEndMultiAndEmbed, [associationEnd.Name,
                                                      associationEnd.Association.name],
                                                     associationEnd);

  if assigned(associationend.Association.Class_) then
  begin
    if associationEnd.Multi then
      StreamName := BoldContentName_ObjectIdListRefPair
    else
      StreamName := BoldContentName_ObjectIdRefPair;
  end
  else
  begin
    if associationEnd.Multi then
      StreamName := BoldContentName_ObjectIdListRef
    else
      StreamName := BoldContentName_ObjectIdRef;
  end;
  Mapper := associationEnd.GetBoldTV(TAG_PMAPPERNAME);

  if (Mapper <> '') and
     not SameText(Mapper, DEFAULTNAME) and
     associationEnd.Association.EffectivePersistent and
     CheckDbStuff then
  begin
    PMapperDescriptor := BoldMemberPersistenceMappers.DescriptorByDelphiName[Mapper];

    if not Assigned(PMapperDescriptor) then
      AddError(sUMVAssociationEndUnknownMapper, [Mapper, associationEnd.association.Name], associationEnd);

    if Assigned(PMapperDescriptor) and (not PMapperDescriptor.CanStore(StreamName)) then
      AddError(sUMVAssociationEndCantStore, [mapper, associationEnd.Name,
                                             associationEnd.Association.Name],
                                            associationEnd);
  end;
end;


procedure TBoldUMLModelValidator.ValidateFeature(feature: TUMLFeature);
begin
  // check model name
  if feature.Name = '' then
    AddError(sUMVMemberNameEmpty, [feature.Owner.Name], feature)
  else
    ValidateNames(Feature, feature.Owner.Name + '.' + feature.Name);
end;

procedure TBoldUMLModelValidator.ValidateNames(Element: TUMLModelElement; ElementName: String);
var
  s: string;
  EmptyError: String;
  InvalidError: String;
  ReservedError: String;
  BoldReservedError: String;
  Stored: Boolean;
  AssocEnd: TUMLAssociationEnd;
begin
  // check Source name
  if element is TUMLModel then
  begin
    if Element.GetBoldTV(TAG_UNITNAME) = '' then
      AddError(sUMVUnitNameEmpty, [ElementName], Element);
    exit;
  end;

  if Language <> mvslNone then
  begin
    S := ExpandedSourceName(Element);
    case Language of
    mvslDelphi:
      begin
        if s = '' then
          AddError(sUMVDelphiNameEmpty, [ElementName], Element)
        else if not IsValidDelphiIdentifier(S) then
          AddError(sUMVDelphiNameInvalid, [S, ElementName], Element)
        else if IsDelphiReservedWord(S) then
          AddError(sUMVDelphiNameReserved, [S, ElementName], Element);
      end;
    mvslCpp:
      begin
        if s = '' then
          AddError(sUMVCppNameEmpty, [ElementName], Element)
        else if not IsValidCppIdentifier(S) then
          AddError(sUMVCppNameInvalid, [S, ElementName], Element)
        else if IsCppReservedWord(S) then
          AddError(sUMVCppNameReserved, [S, ElementName], Element);
      end;
    end;
  end;

  // check expression name
  S := ExpandedExpressionName(Element);
  if S = '' then
    AddError(sUMVExpressionNameEmpty, [ElementName], Element)
  else if not IsValidOCLIdentifier(S) then
    AddError(sUMVExpressionNameInvalid, [S,ElementName], Element)
  else if IsOCLReservedWord(S) then
    AddError(sUMVExpressionNameOCLReserved, [S,ElementName], Element);

  Stored := false;
  if (element is TUMLClass) then
    stored := (element as TUMLClass).Persistent
  else if element is TUMLAttribute then
    stored := (element as TUMLAttribute).EffectivePersistent
  else if element is TUMLAssociationEnd then
  begin
    AssocEnd := element as TUMLAssociationEnd;
    Stored := AssocEnd.Association.EffectivePersistent and
        not AssocEnd.Multi and
        TVIsTrue(AssocEnd.GetBoldTV(TAG_EMBED));
  end;

  stored := stored and (element.GetBoldTV(TAG_STORAGE) <> TV_STORAGE_EXTERNAL);

  if Stored and CheckDBStuff then
  begin
    if element is TUMLClass then
    begin
      EmptyError := sUMVSQLTableNameEmpty;
      InvalidError := sUMVTableNameInvalid;
      ReservedError := sUMVTableNameSQLReserved;
      BoldReservedError := sUMVBoldTableReservedWord;
    end
    else
    begin
      EmptyError := sUMVSQLColumnNameEmpty;
      InvalidError := sUMVColumnNameInvalid;
      ReservedError := sUMVColumnNameSQLReserved;
      BoldReservedError := sUMVBoldColumnReservedWord;
    end;
    S := ExpandedDBName(Element);

    if S = '' then
      AddError(EmptyError, [ElementName], Element)
    else if CheckAnsiSQLStuff and not IsValidSQLIdentifier(S) then
      AddError(InvalidError, [S, ElementName], Element)
    else if CheckAnsiSQLStuff and IsSQLReservedWord(S) then
      AddError(ReservedError, [S, ElementName], Element)
    else if IsBoldReservedWord(S) then
      AddError(BoldReservedError, [S, ElementName], Element);
  end;
end;

function TBoldUMLModelValidator.CheckDBStuff: Boolean;
begin
  Result := Assigned(SQLDatabaseConfig);
end;

function TBoldUMLModelValidator.CheckAnsiSQLStuff: Boolean;
begin
  result := CheckDBStuff; // FIXME add info to SQLDatabaseConfig for this?
end;

function TBoldUMLModelValidator.NationalCharConversion: TBoldNationalCharConversion;
begin
  result := TBoldTaggedValueSupport.StringToNationalCharConversion(UMLModel.GetBoldTV(TAG_NATIONALCHARCONVERSION));
end;

function TBoldUMLModelValidator.IsSQLReservedWord(const S: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  if Length(S) = 0 then Exit;

  for I := 0 to SQLReservedWordList.Count - 1 do
    if CompareText(S, SQLReservedWordList[I]) = 0 then
    begin
      Result := True;
      Exit;
    end;
end;

function TBoldUMLModelValidator.GetSQLReservedWordList: TStringlist;
var
  i: integer;
begin
  if not Assigned(fSQLReservedWordList) then
  begin
    fSQLReservedWordList := TStringList.Create;
    fSQLReservedWordList.Text := StringReplace(SQLDataBaseConfig.ReservedWords.Text, ',', #10, [rfReplaceAll]);
    for i := 0 to fSQLReservedWordList.Count - 1 do
      fSQLReservedWordList[i] := Trim(fSQLReservedWordList[i]);
  end;
  Result := fSQLReservedWordList;
end;

destructor TBoldUMLModelValidator.Destroy;
begin
  FreeAndNil(fSQLReservedWordList);
  inherited;
end;

end.
