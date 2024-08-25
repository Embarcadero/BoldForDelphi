{ Global compiler directives }
{$include bold.inc}
unit BoldGen;

interface

uses
  Classes,
  BoldMeta,
  BoldTypeNameDictionary,
  BoldFileHandler,
  BoldTemplateExpander,
  BoldDefs;

const
  INDENTSIZE = 2;
  DEBUGGERWORKAROUNDINTERVAL = 50;
  AddVisitorSupport = true;

type
  TBoldGenerator = class;
  TBoldGeneratorClass = class of TBoldGenerator;
  TBoldCodeGenInitializer = class;
  TBoldCodeGenInitializerClass = class of TBoldCodeGenInitializer;

  TBoldFunctionContent = (fcHeader, fcBody);
  TBoldEnsureFlag = (efEnsure, efAdd);
  TMoldClassFilterFunction = function (MoldClass: TMoldClass): Boolean of object;

  {TBoldGenerator}
  TBoldGenerator = class
  private
    fMoldModel: TMoldModel;
    fBoldFilehandler: TBoldFileHandler; {Keep one ot latest file, as optimization}
    fUseTypedLists: Boolean;
    fTypesWithoutNative: TStringList;
    fUnmappedTypes: tStringList;
    FMethodIndex: TStringList;
    fTypeNameDictionary: tBoldTypeNameDictionary;
    fCurrentClass: TMoldClass;
    fCurrentComponent: TMoldComponent;
    FGenerateBold1CompatibleCode: Boolean;
    fBaseFilePath: String;
    fGenerateMIDLCode: boolean;
    fGenerateIDLVariables: Boolean;
    procedure EnsureMethod(Strings: TStrings);
    procedure InitializeTemplateForComponent(Template: TBoldTemplateHolder; Model: TMoldModel; Component: TMoldComponent; InitializeClasses: Boolean); virtual;
    function MethodToDelphiHeader(OwningClass: TMoldClass; Method: TMoldMethod; TagValue: Integer; AddSignature: Boolean; AutoOverride: Boolean): String;
    function MethodToCOMHeader(OwningClass: TMoldClass; Method: TMoldMethod; InterfaceCode: Boolean; ParametersToCoerce, ParametersToInterfaceCoerce: TStringList): String;
    function MethodToIDLHeader(OwningClass: TMoldClass; Method: TMoldMethod): String;
    function MethodToCOMCall(OwningClass: TMoldClass; Method: TMoldMethod; ParametersToCoerce, ParametersToInterfaceCoerce: TStringList): String;
    function EnsureSafeIDLType(const ParamType: String; MoldClass: TMoldClass): String;
    procedure SetCurrentFileHandler(const path, Filename: string; ModuleType: TBoldModuleType; Show: Boolean; IsIncFile: Boolean);
    procedure AddIncFileHeader(StringList: TStringList);
    procedure SetMoldModel(const Value: TMoldModel);
    procedure SetGenerateBold1CompatibleCode(const Value: Boolean);
    function GetNativeDelphiTypeForModelNameNoDefaults(Attr: TMoldAttribute): String;
    procedure AddVarList(Template: TBoldTemplateHolder; VariableBaseName, CommaValues: string);
    procedure AddSuperClassName(Variables: TBoldTemplateVariables; MoldClass: tMoldClass);
    function MakePersistenceInterfaceName(MoldClass: TMoldClass): string;
    function ModuleTypeForFile(const FileName: string): TBoldModuleType;
  protected
    property MethodIndex: TStringList read fMethodIndex;
    function FindInCurrentFile(s: String): Boolean;
    procedure InitializeMethodIndex;
    property CurrentClass: TMoldClass read fCurrentClass write fCurrentClass;
    property GenerateIDLVariables: Boolean read fGenerateIDLVariables write fGenerateIDLVariables;
  public
    constructor Create(TypeNameDictionary: TBoldTypeNameDictionary); virtual;
    destructor Destroy; override;
    procedure ExpandTemplateList(TemplateList: TBoldTemplateList); virtual;
    procedure EnsureMethodImplementations; virtual;
    procedure GenerateBusinessObjectCode;
    procedure GeneratePersistenceInterfaces;
    procedure GenerateComInterfaces;
    property MoldModel: TMoldModel read fMoldModel write SetMoldModel;
    property UseTypedLists: Boolean read fUseTypedLists write fUseTypedLists;
    property TypeNameDictionary: tBoldTypeNameDictionary read fTypeNameDictionary;
    property UnMappedTypes: TStringList read fUnmappedTypes;
    property TypesWithoutNative: TStringList read fTypesWithoutNative;
    property GenerateBold1CompatibleCode: Boolean read FGenerateBold1CompatibleCode write SetGenerateBold1CompatibleCode;
    property BaseFilePath: String read fBaseFilePath write fBaseFilePath;
    property BoldFilehandler: TBoldFileHandler read fBoldFilehandler;
    property GenerateMIDLCode: boolean read fGenerateMIDLCode write fGenerateMIDLCode;
  end;

  TBoldCodeGenInitializer = class
  private
    fMoldModel: TMoldModel;
  protected
    function FilterLinkClass(MoldClass: TMoldClass): Boolean;
    function FindComponent(const ComponentName: string): TMoldComponent;
    function RenameComponent(const OldComponentName, NewComponentName: string): TMoldComponent;
    function EnsureComponent(const ComponentName: string): TMoldComponent;
    procedure EnsureDependency(const SubComponentName, SuperComponentname: String);
    procedure EnsureInheritanceDependencies;
    procedure MoveClassesToComponent(MoldClassFilterFunction: TMoldClassFilterFunction; const ComponentName: String);
    procedure MoveClassTreeToComponent(SuperClass: TMoldClass; const ComponentName: String; MoveSuperClass: Boolean);
    procedure MoveClassToComponent(const ClassName, ComponentName: String);
    procedure MoveClassToComponentByClass(MoldClass: TMoldClass; const ComponentName: String);
    procedure MoveImplicitLinkClassesToSuperClassComponent;
    procedure MoveImplicitLinkClassesToAssociationEndComponent;
    property MoldModel: TMoldModel read fMoldModel;
  public
    constructor create(MoldModel: TMoldModel); virtual;
    procedure Execute; virtual;
    function ValidateFileNames: Boolean;
  end;

var
  BoldGeneratorClass: TBoldGeneratorClass = TBoldGenerator;
  BoldCodeGenInitializerClass: TBoldCodeGenInitializerClass = TBoldCodeGenInitializer;

implementation

uses
  SysUtils,

  BoldCoreConsts,
  BoldUtils,
  BoldMetaSupport,
  BoldNameExpander,
  BoldGUIDUtils,
  BoldTaggedValueSupport,
  BoldDefaultTaggedValues,
  BoldGeneratorTemplates,
  BoldLogHandler,
  BoldUMLTypes;

const
  SEARCHPARAMETERLIST = False;

{---TBoldGenerator---}
constructor TBoldGenerator.Create(TypeNameDictionary: TBoldTypeNameDictionary);
begin
  inherited Create;
  fMethodIndex := TStringList.Create;
  fTypeNameDictionary := TypeNameDictionary;
  fUnmappedTypes:= TStringList.Create;
  ftypesWithoutNative := TStringList.Create;
  FGenerateBold1CompatibleCode := true;
end;

destructor TBoldGenerator.Destroy;
begin
  FreeAndNil(fMethodIndex);
  FreeAndNil(ftypesWithoutNative);
  FreeAndNil(fUnmappedTypes);
  BoldCloseAllFileHandlers;
  inherited;
end;

procedure TBoldGenerator.ExpandTemplatelist(TemplateList: TBoldTemplateList);
var
  TemplateIx, ComponentIx: integer;
begin
  MoldModel.EnsureTopSorted;
  if assigned(BoldCodeGenInitializerClass) then
    with BoldCodeGenInitializerClass.Create(MoldModel) do
    begin
      try
        Execute;
        ValidateFileNames;
      finally
        free;
      end;
    end;

  BoldLog.ProgressMax := MoldModel.Classes.Count * 3;

  BoldLog.LogFmt(sLogGeneratingInPath, [BaseFilePath]);
  for ComponentIx := 0 to MoldModel.Components.Count - 1 do
  begin
    for TemplateIx := 0 to TemplateList.Count - 1 do
    begin
      if BoldLog.ProcessInterruption then
        exit;

      with TemplateList[TemplateIx] do
      begin
        Variables.SetVariable('COMPONENTNAME', MoldModel.Components[ComponentIx].Name); // do not localize
        BoldLog.LogFmt(sLogGeneratingFile, [ExpandedFileName]);

        SetCurrentFileHandler(BaseFilePath, ExpandedFileName, ModuleTypeForFile(ExpandedFileName), true, false);
        BoldFilehandler.Clear;
        BoldLog.LogHeader := sLogInitializingVars;
        InitializeTemplateForComponent(TemplateList[TemplateIx],
                                       MoldModel,
                                       MoldModel.Components[ComponentIx],
                                       true);
        if BoldLog.ProcessInterruption then
          exit;

        BoldLog.LogHeader := sLogExpandingTemplate;
        BoldFilehandler.AddStrings(ExpandedTemplate);
      end;
    end;
  end;

  if MoldModel.MainComponent.Name = BoldDefaultTaggedValueList.DefaultForClassAndTag['Model', TAG_UNITNAME] then // do not localize
  begin
    BoldLog.Separator;
    BoldLog.Log(sLogConsiderNameChange1, ltWarning);
    BoldLog.Log(sLogConsiderNameChange2, ltWarning);
    BoldLog.Separator;
  end;
end;

const
  VisibilityToString: array[TVisibilityKind] of string = ('private', 'protected', 'public');

procedure TBoldGenerator.InitializeTemplateForComponent(Template: TBoldTemplateHolder; Model: TMoldModel; Component: TMoldComponent; InitializeClasses: Boolean);
var
  i: integer;
  temp: string;
  ClassCount: integer;
  FileName: String;
  Dependencies: TStringList;
  IncludeFiles: TStringList;

  procedure AddVariable(const VariableName, VariableValue: String);
  begin
    Template.Variables.ForceAdd(VariableName, VariableValue, []);
  end;

  procedure InitializeAttribute(MoldAttribute: TMoldAttribute; const PostFix: String);
  var
    typeName: String;
    ValueInterfacename: String;
    Mapping: TBoldTypeNameMapping;

  procedure internalInitialize;
  begin
    AddVariable('MEMBERHASNATIVE' + PostFix, '1'); // do not localize
    if not MoldAttribute.Derived or MoldAttribute.ReverseDerived then
      AddVariable('MEMBERISSETABLE' + PostFix, '1') // do not localize
    else
      AddVariable('MEMBERISSETABLE' + PostFix, '0'); // do not localize
    end;

  begin
    Mapping := TypeNameDictionary.MappingForModelName[MoldAttribute.BoldType];
    if not assigned(Mapping) then
    begin
      if UnmappedTypes.IndexOf(MoldAttribute.BoldType) = -1 then
      begin
        BoldLog.LogFmt(sLogNoDelphiMappingForType, [MoldAttribute.BoldType], ltWarning);
        UnmappedTypes.Add(MoldAttribute.BoldType);
      end;
      Typename := 'TBoldAttribute'; // do not localize
      ValueInterfacename := 'IBoldNullableValue'; // do not localize
    end else
    begin
      TypeName := Mapping.ExpandedDelphiName;
      ValueInterfacename := Mapping.ValueInterface;
      if GenerateIDLVariables then
      begin
        if (Mapping.ExpandedCOMType <> '') and
           (Mapping.IDLType <> '') then
        begin
          internalInitialize;
          AddVariable('MEMBERNATIVECOMTYPE' + PostFix, Mapping.ExpandedCOMType); // do not localize
          AddVariable('MEMBERNATIVEIDLTYPE' + PostFix, Mapping.IDLType); // do not localize
          AddVariable('DISPID' + PostFix, IntToHex(MoldAttribute.DispId, 8)); // do not localize
          if CompareText(Mapping.ExpandedCOMType, BoldWideStringTypeName) = 0 then
            AddVariable('SETMEMBERASCONST' + PostFix, 'const '); // do not localize
        end
        else
          if typesWithoutNative.IndexOf(MoldAttribute.BoldType) = -1 then
          begin
            BoldLog.LogFmt(sLogNoCOMMappingForType, [MoldAttribute.BoldType, MoldAttribute.MoldClass.Name, MoldAttribute.Name], ltWarning);
            typesWithoutNative.Add(MoldAttribute.BoldType);
          end;
      end
      else
      begin
        if (Mapping.ExpandedAccessor <> '') and
          (Mapping.ExpandedNativeType <> '') then
        begin
          internalInitialize;
          AddVariable('MEMBERNATIVEACCESSOR' + PostFix, Mapping.ExpandedAccessor);
          AddVariable('MEMBERNATIVETYPE' + PostFix, Mapping.ExpandedNativeType);
        end
        else begin
          if GenerateIDLVariables and (typesWithoutNative.IndexOf(MoldAttribute.BoldType) = -1) then
          begin
            BoldLog.LogFmt(sLogNoNativeMappingForType, [MoldAttribute.BoldType, MoldAttribute.Name, MoldAttribute.MoldClass.Name], ltWarning);
            typesWithoutNative.Add(MoldAttribute.BoldType);
          end;
        end;

        if MoldAttribute.EffectivePersistent and
          (Mapping.ValueInterfaceAccessor <> '') and
          (Mapping.ValueInterfaceNativeType <> '') then
        begin
          AddVariable('MEMBERHASVALUEINTERFACENATIVE' + PostFix, '1');
          AddVariable('MEMBERVALUEINTERFACENATIVEACCESSOR' + PostFix, Mapping.ValueInterfaceAccessor);
          AddVariable('MEMBERVALUEINTERFACENATIVETYPE' + PostFix, Mapping.ValueInterfaceNativeType);
        end
        else begin
          if GenerateIDLVariables and (typesWithoutNative.IndexOf(MoldAttribute.BoldType) = -1) then
          begin
            BoldLog.LogFmt(sNoValueTypeMappingForType, [MoldAttribute.BoldType, MoldAttribute.Name, MoldAttribute.MoldClass.Name], ltWarning);
            typesWithoutNative.Add(MoldAttribute.BoldType);
          end;
        end;

      end;
    end;
    AddVariable('MEMBERTYPE' + PostFix, TypeName);
    addVariable('MEMBERVALUEINTERFACE'+PostFix, ValueInterfaceName);
    AddVariable('MEMBERKIND' + PostFix, 'Attribute');
    AddVariable('MEMBERVISIBILITY' + PostFix, VisibilityTOString[MoldAttribute.Visibility]);
  end;

  procedure InitializeNativeAttribute(MoldAttribute: TMoldAttribute; const PostFix: String);
  var
    typeName: String;
  const
    PropertyAccessKindTOStr: array[TDelphiPropertyAccessKind] of string =
     ('None', 'Field', 'PrivateMethod', 'ProtectedVirtualMethod');
  begin
    TypeName := GetNativeDelphiTypeForModelNameNoDefaults(MoldAttribute);
    AddVariable('DelphiAttributeType' + PostFix, TypeName);
    AddVariable('DelphiAttributeKind' + PostFix, 'Attribute');
    AddVariable('DelphiAttributeHasField' + PostFix, BoldBooleanToTemplateVar[MoldAttribute.HasDelphiField]);

    AddVariable('DelphiAttributeVisibility' + PostFix, VisibilityToString[MoldAttribute.Visibility]);
    AddVariable('DelphiAttributePropertyRead' + PostFix, PropertyAccessKindToStr[MoldAttribute.DelphiPropertyRead]);
    AddVariable('DelphiAttributePropertyWrite' + PostFix, PropertyAccessKindToStr[MoldAttribute.DelphiPropertyWrite]);
  end;

  procedure InitializeQualifier(MoldRole: TMoldRole; const PostFix: String);
  var
    Mapping: TBoldTypeNameMapping;
    QualifierPropertyParams: String;
    QualifierFunctionParams: String;
    Member: TMoldMember;
    AttrOfOtherEnd: TMoldAttribute;
    i: integer;
    AllOK: Boolean;
  begin
    QualifierPropertyParams := '';
    QualifierFunctionParams := '';
    AllOK := true;
    for i := 0 to MoldRole.Qualifiers.Count - 1 do
    begin
      Member := MoldRole.OtherEnd.MoldClass.AllBoldMembers.ItemsByName[MoldRole.Qualifiers[i].Name];
      if Member is TMoldAttribute then
      begin
        AttrOfOtherEnd := Member as TMoldAttribute;
        Mapping := TypeNameDictionary.MappingForModelName[AttrOfOtherEnd.BoldType];
        if assigned(Mapping) then
        begin
          AddVariable('QUALIFIERNAME' + PostFix + '.' + IntToStr(i), AttrOfOtherEnd.ExpandedDelphiName);
          AddVariable('QUALIFIERBOLDTYPE' + PostFix + '.' + IntToStr(i),  Mapping.ExpandedDelphiName);
          AddVariable('QUALIFIERACCESSOR' + PostFix + '.' + IntToStr(i),  Mapping.ExpandedAccessor);
          BoldGeneratorTemplatesManager.AddQualifierPropertyParam(QualifierPropertyParams, AttrOfOtherEnd.ExpandedDelphiName, Mapping.ExpandedNativeType);
          BoldGeneratorTemplatesManager.AddQualifierFunctionParam(QualifierFunctionParams, AttrOfOtherEnd.ExpandedDelphiName, Mapping.ExpandedNativeType);
        end
        else
          AllOK := false;
      end
      else
        AllOK := false;
    end;
    AddVariable('QUALIFIERPROPERTYPARAMS' + PostFix, QualifierPropertyParams);
    AddVariable('QUALIFIERFUNCTIONPARAMS' + PostFix, QualifierFunctionParams);
    AddVariable('ROLEQUALIFIED' + PostFix, BoldBooleanToTemplateVar[AllOK and (MoldRole.Qualifiers.Count > 0)]);
    AddVariable('QUALIFIERCOUNT' + PostFix, IntToStr(MoldRole.Qualifiers.Count));
  end;

  procedure InitializeRole(MoldRole: TMoldRole; const PostFix: String);
  var
    OtherEnd: TMoldClass;
    IDLTypeOfOtherEnd: TMoldClass;
  begin
    OtherEnd := MoldRole.MoldClass.LowestVisibleAncestor(MoldRole.OtherEnd.MoldClass);
    if (not MoldRole.Multi or UseTypedLists) and assigned(OtherEnd) then
    begin
      AddVariable('MEMBERTYPE' + PostFix, OtherEnd.ExpandedDelphiName);
      AddVariable('MEMBERPERSISTENCEINTERFACE'+PostFix, MakePersistenceInterfaceName(OtherEnd));

      if GenerateIDLVariables then
      begin
        if GenerateMIDLCode then
          AddVariable('MEMBERCOMTYPE' + PostFix, OtherEnd.ExpandedInterfaceName)
        else
        begin
          IDLTypeOfOtherEnd := MoldRole.MoldClass.LowestCommonSuperClass(OtherEnd);
          if MoldRole.Multi and (MoldRole.MoldClass = IDLTypeOfOtherEnd) then
            IDLTypeOfOtherEnd := MoldRole.MoldClass.SuperClass;

          if assigned(IDLTypeOfOtherEnd) then
            AddVariable('MEMBERCOMTYPE' + PostFix, IDLTypeOfOtherEnd.ExpandedInterfaceName)
          else
            AddVariable('MEMBERCOMTYPE' + PostFix, 'IBoldObjectList')
        end;

        AddVariable('MEMBERREALCOMTYPE' + PostFix, OtherEnd.ExpandedInterfaceName);
      end;
    end
    else
    begin
      AddVariable('MEMBERTYPE' + PostFix, 'TBoldObject');
      if GenerateIDLVariables then
      begin
        AddVariable('MEMBERCOMTYPE' + PostFix, 'IBoldObject');
        AddVariable('MEMBERREALCOMTYPE' + PostFix, 'IBoldObject');
      end;
    end;

    AddVariable('DISPID' + PostFix, IntToHex(MoldRole.Dispid, 8));

    if MoldRole.Multi then
      AddVariable('MEMBERKIND' + PostFix, 'MultiRole')
    else
      AddVariable('MEMBERKIND' + PostFix, 'SingleRole');

    AddVariable('ISTRUEROLE' + PostFix, BoldBooleanToTemplateVar[MoldRole.RoleType = rtRole]);
    AddVariable('MEMBERVISIBILITY' + PostFix, VisibilityTOString[MoldRole.Visibility]);
    AddVariable('ROLENAVIGABLE' + PostFix, BoldBooleanToTemplateVar[MoldRole.Navigable]);

    InitializeQualifier(MoldRole, PostFix);
  end;

  function ParameterInfo(MoldMethod: TMoldMethod; const ParameterType, ParameterDelphiType: String; Kind: integer): String;
  var
    MoldClass: TMoldClass;
  begin
    MoldClass := MoldMethod.MoldClass.Model.Classes.ItemsByExpressionName[ParameterType];
    if not assigned(MoldClass) then
      MoldClass := MoldMethod.MoldClass.Model.Classes.ItemsByDelphiName[ParameterDelphiType];

    if assigned(MoldClass) then
      case kind of
        0: result := 'object';
        1: result := ParameterDelphiType;
        2: result := '';
      end
    else if SameText(ParameterType, 'STRING') then
      case kind of
        0: result := 'attribute';
        1: result := 'TBAString';
        2: result := '.AsString';
      end
    else if SameText(ParameterType, 'INTEGER') then
      case kind of
        0: result := 'attribute';
        1: result := 'TBAInteger';
        2: result := '.AsInteger';
      end
    else if SameText(ParameterType, 'DOUBLE') then
      case kind of
        0: result := 'attribute';
        1: result := 'TBAFloat';
        2: result := '.AsFloat';
      end
    else
      Template.Variables.SetVariable('CASEMETHODISOCLCOMPATIBLE', '0');
  end;
  procedure InitializeDispIdMethod(MoldMethod: TMoldMethod; const postfix: String; DispIdOffset: integer);
  var
    i: integer;
    ParametersToInterfaceCoerce,
    ParametersToCoerce: TStringList;
    paramName,
    ParamType,
    ParamInterfaceType,
    VarDeclString,
    ConvertString,
    ConvertBackString: String;
  begin
    ParametersToCoerce := TStringList.Create;
    ParametersToInterfaceCoerce := TStringList.create;
    try
      AddVariable('COMMETHODHEADERINTERFACE' + PostFix, MethodToCOMHeader(MoldMethod.MoldClass, MoldMethod, true, ParametersToCoerce, ParametersToInterfaceCoerce));
      if (ParametersToInterfaceCoerce.Count + ParametersTocoerce.Count) <> 0 then
      begin
        VarDeclString := 'var' + BOLDCRLF;
        ConvertString := '';
        ConvertBackString := '';

        for i := 0 to ParametersToCoerce.Count - 1 do
        begin
          VarDeclString := VarDeclString + '  ' + ParametersToCoerce.Names[i] + '_temp: '+ ParametersToCoerce.values[ParametersToCoerce.Names[i]] + ';' + BOLDCRLF;
          ConvertString := ConvertString + '  ' + ParametersToCoerce.Names[i] + '_temp := ' + ParametersToCoerce.Names[i] + ';' + BOLDCRLF;
          ConvertBackString := ConvertBackString + '  ' + ParametersToCoerce.names[i] +' := ' + ParametersToCoerce.Names[i] + '_temp;' + BOLDCRLF;
        end;
        for i := 0 to ParametersToInterfaceCoerce.Count - 1 do
        begin
          ParamName := ParametersToInterfaceCoerce.Names[i];
          ParamType := ParametersToInterfaceCoerce.Values[ParamName];
          ParamInterfaceType := Copy(ParamType, pos('=', ParamType) + 1, maxint);
          ParamType := Copy(ParamType, 1, pos('=', ParamType) - 1);

          varDeclString := VarDeclString +
            format('  %s_temp: %s;' + BOLDCRLF, [ParamName, ParamType]);
          ConvertString := ConvertString +
            format('  %s_temp := BoldComInterfaceToObject(%s) as %s;' + BOLDCRLF, [ParamName, ParamName, ParamType]);
          ConvertBackString := ConvertBackString +
            format('  BoldComCreateAdapter(%s_temp, False, %s, %s);' + BOLDCRLF, [ParamName, ParamInterfaceType, ParamName]);
        end;

        AddVariable('COMMETHOD_TEMPVARS' + PostFix, VarDeclString);
        AddVariable('COMMETHOD_TEMPVARSCONVERT' + PostFix, ConvertString);
        AddVariable('COMMETHOD_TEMPVARSCONVERTBACK' + PostFix, ConvertBackString);
      end;
      AddVariable('COMMETHODHEADERIMPLEMENTATION' + PostFix, MethodToCOMHeader(MoldMethod.MoldClass, MoldMethod, false, nil, nil));
      AddVariable('METHODWRAPPERCALL' + PostFix, MethodToComCall(MoldMethod.MoldClass, MoldMethod, parametersToCoerce, ParametersToInterfaceCoerce));
      AddVariable('IDLMETHODHEADER' + PostFix, MethodToIDLHeader(MoldMethod.MoldClass, MoldMethod));
      AddVariable('METHODDISPID' + PostFix, IntToHex(DispIdOffset, 8));
    finally
      ParametersToCoerce.Free;
    end;
  end;

  procedure InitializeMethod(MoldMethod: TMoldMethod; const postfix: String; AutoOverride: Boolean);
  var
    i: integer;
    MethodType: String;
  begin
    AddVariable('INTERFACEMETHODHEADER' + PostFix, MethodToDelphiHeader(MoldMethod.MoldClass, MoldMethod, Publictag, true, AutoOverride));
    AddVariable('METHODNAME' + PostFix, MoldMethod.ExpandedDelphiName);
    AddVariable('METHODVISIBILITY' + PostFix, visibilityToString[MoldMethod.Visibility]);
    if MoldMethod.IsClassMethod then
      MethodType := 'class '
    else
      MethodType := '';

    if MoldMethod.HasReturnValue then
      MethodType := methodType + 'function'
    else
      MethodType := methodType + 'procedure';

    AddVariable('METHODKIND' + PostFix, MethodType);

    AddVariable('METHODISOCLCOMPATIBLE' + PostFix, '1');

    if MoldMethod.HasReturnValue then
    begin
      AddVariable('METHODRESULTBOLDTYPE' + PostFix, ParameterInfo(MoldMethod, MoldMethod.ReturnType, MoldMethod.DelphiReturnType, 1));
      AddVariable('METHODRESULTACCESSOR' + PostFix, ParameterInfo(MoldMethod, MoldMethod.ReturnType, MoldMethod.DelphiReturnType, 2));
    end else begin
      AddVariable('METHODRESULTBOLDTYPE' + PostFix, 'TBAInteger');
      AddVariable('METHODRESULTACCESSOR' + PostFix, '.AsInteger');
    end;

    for i := 0 to MoldMethod.Parameters.count - 1 do
    begin
      with TMoldParameter(MoldMethod.Parameters[i]) do
      begin
        AddVariable('METHODPARAMETERKIND' + PostFix + '.' + IntToStr(i), ParameterInfo(MoldMethod, ParameterType, DelphiParameterType, 0));
        AddVariable('METHODPARAMETERBOLDTYPE' + PostFix + '.' + IntToStr(i), ParameterInfo(MoldMethod, ParameterType, DelphiParameterType, 1));
        AddVariable('METHODPARAMETERACCESSOR' + PostFix + '.' + IntToStr(i), ParameterInfo(MoldMethod, ParameterType, DelphiParameterType, 2));
      end;
    end;

    AddVariable('METHODPARAMETERCOUNT' + PostFix, IntToStr(MoldMethod.Parameters.count));
  end;

  procedure InitializeClass(MoldClass: TMoldClass; const PostFix: String);
  var
    i, Derivedcounter, MemberCounter: integer;
    Attribute: TMoldAttribute;
    Member: TMoldMember;
    MemberPostFix: String;
  begin
    AddVariable('CLASSNAME' + PostFix, MoldClass.ExpandedDelphiName);
    if MoldClass.GUID <> '' then
      AddVariable('CLASSGUID' + PostFix, MoldClass.GUID)
    else
      AddVariable('CLASSGUID' + PostFix, BoldCreateGUIDAsString(true));

    if MoldClass.ListGUID <> '' then
      AddVariable('LISTGUID' + PostFix, MoldClass.ListGUID)
    else
      AddVariable('LISTGUID' + PostFix, BoldCreateGUIDAsString(true));

    AddVariable('INTERFACENAME' + PostFix, MoldClass.ExpandedInterfaceName);
    AddVariable('PERSISTENCEINTERFACENAME' + PostFix, MakePersistenceInterfaceName(MoldClass));
    AddVariable('PERSISTENCEINTERFACEGUID' + PostFix, BoldCreateGUIDWithBracketsAsString);

    AddVariable('ISVERSIONED' + PostFix, BoldBooleanToTemplateVar[MoldClass.Versioned]);


    AddVariable('ISLINKCLASS' + PostFix, BoldBooleanToTemplateVar[Assigned(MoldClass.Association)]);
    AddVariable('CLASSEXPRESSIONNAME' + PostFix, MoldClass.ExpandedExpressionName);
    if Assigned(MoldClass.SuperClass) then
    begin
      AddVariable('SUPERCLASSNAME' + PostFix, MoldClass.SuperClass.ExpandedDelphiName);
      AddVariable('SUPERPERSISTENCEINTERFACENAME' + PostFix, MakePersistenceInterfaceName(MoldClass.SuperClass));
    end
    else
    begin
      AddVariable('SUPERCLASSNAME' + PostFix, 'TBoldObject');
      AddVariable('SUPERCLASSNAMESPACEPREFIX' + PostFix, 'Boldsystem::');
      AddVariable('CONSTRUCTORPARAMETER' + PostFix, ', true');
      AddVariable('SUPERPERSISTENCEINTERFACENAME' + PostFix, 'IPersistentBoldObject');
    end;

    if GenerateIDLVariables then
    begin
      if assigned(MoldClass.SuperClass) then
        AddVariable('SUPERINTERFACE' + PostFix, MoldClass.SuperClass.ExpandedInterfaceName)
      else
        AddVariable('SUPERINTERFACE' + PostFix, 'IBoldObject');

      if Assigned(MoldClass.SuperClass) then
        AddVariable('SUPERADAPTERNAME' + PostFix, MoldClass.SuperClass.ExpandedDelphiName)
      else
        AddVariable('SUPERADAPTERNAME' + PostFix, 'TBoldComObject');
    end;

    if MoldClass.IntroducesManuallyDerivedMembers then
      AddVariable('CLASSINTRODUCESMANUALLYDERIVEDMEMBERS' + PostFix, 'true');

    if MoldClass.IntroducesManuallyReverseDerivedMembers then
      AddVariable('CLASSINTRODUCESMANUALLYREVERSEDERIVEDMEMBERS' + PostFix, 'true');

    membercounter := 0;

    for i := MoldClass.FirstOwnBoldMemberIndex to MoldClass.AllBoldMembers.Count - 1 do
    begin
      Member := MoldClass.AllBoldMembers[i];
      MemberpostFix := PostFix + '.' + IntTostr(i - MoldClass.FirstOwnBoldMemberIndex);
      AddVariable('MEMBERNAME' + MemberPostFix, Member.ExpandedDelphiName);
      AddVariable('MEMBERINDEX' + MemberPostFix, IntToStr(i));

      AddVariable('MEMBERPERSISTENT' + MemberPostFix, BoldBooleanToTemplateVar[Member.EffectivePersistent]);

      if Member is TMoldAttribute then
        InitializeAttribute(Member as TMoldAttribute, MemberPostFix)
      else if Member is TMoldRole then
        InitializeRole(Member as TMoldRole, MemberPostFix);

      Inc(MemberCounter);
    end;
    AddVariable('MEMBERCOUNT' + Postfix, IntToStr(MemberCounter));
    membercounter := 0;

    for i := 0 to MoldClass.AllBoldMembers.Count - 1 do
    begin
      Member := MoldClass.AllBoldMembers[i];
      MemberpostFix := '-ALL' + PostFix + '.' + IntTostr(i);
      AddVariable('MEMBERNAME' + MemberPostFix, Member.ExpandedDelphiName);
      AddVariable('MEMBERINDEX' + MemberPostFix, IntToStr(i));

      AddVariable('MEMBERPERSISTENT' + MemberPostFix, BoldBooleanToTemplateVar[Member.EffectivePersistent]);

      if Member is TMoldAttribute then
        InitializeAttribute(Member as TMoldAttribute, MemberPostFix)
      else if Member is TMoldRole then
        InitializeRole(Member as TMoldRole, MemberPostFix);

      Inc(MemberCounter);
    end;
    AddVariable('MEMBERCOUNT-ALL' + Postfix , IntToStr(MemberCounter));
    membercounter := 0;

    for i := MoldClass.FirstOwnNativeAttributeIndex to MoldClass.AllNativeAttributes.Count - 1 do
    begin
      Attribute := MoldClass.AllNativeAttributes[i];

      if (Attribute.DelphiPropertyRead <> pkNone) or (Attribute.DelphiPropertyWrite <> pkNone) then
      begin
        MemberpostFix := PostFix + '.' + IntTostr(i - MoldClass.FirstOwnNativeAttributeIndex);
        AddVariable('DelphiAttributeName' + MemberPostFix, Attribute.ExpandedDelphiName);
        InitializeNativeAttribute(Attribute, MemberPostFix);
        Inc(MemberCounter);
      end;
    end;
    AddVariable('DelphiAttributeCount' + Postfix, IntToStr(MemberCounter));

    for i := 0 to MoldClass.Methods.Count - 1 do
      InitializeMethod(MoldClass.Methods[i], PostFix + '.' + IntToStr(i), false);

    for i := 0 to MoldClass.AllAutoOverrideMethods.Count - 1 do
      InitializeMethod(MoldClass.AllAutoOverrideMethods[i], PostFix + '.' + IntToStr(MoldClass.Methods.Count + i), true);

    AddVariable('METHODCOUNT' + PostFix, IntToStr(MoldClass.Methods.Count + MoldClass.AllAutoOverrideMethods.Count));

    if GenerateIDLVariables then
    begin
      MemberCounter := 0;
      for i := 0 to MoldClass.Methods.Count - 1 do
        if MoldClass.Methods[i].HasDispId then
        begin
          InitializeDispIDMethod(MoldClass.Methods[i], PostFix + '.' + IntToStr(MemberCounter), MoldClass.Methods[i].DispId);
          Inc(MemberCounter);
        end;
      AddVariable('DISPIDMETHODCOUNT' + PostFix, IntToStr(MemberCounter));
    end;

    Derivedcounter := 0;
    for i := 0 to MoldClass.AllBoldMembers.Count - 1 do
    begin
      if (MoldClass.AllBoldMembers[i].Derived and((MoldClass.AllBoldMembers[i].Derivationocl = '')  or MoldClass.AllBoldMembers[i].ReverseDerived)) and
        ((MoldClass.AllBoldMembers[i].MoldClass = MoldClass) or
        (TVIsTrue(MoldClass.AllBoldMembers[i].BoldTVByName[TAG_VIRTUALDERIVE]))) then
      begin

        AddVariable('DERIVEDMEMBERINTRODUCEDHERE' + Postfix + '.' + IntToStr(DerivedCounter), BoldBooleanToTemplateVar[MoldClass.AllBoldMembers[i].MoldClass = MoldClass]);
        AddVariable('DERIVEDMEMBERREVERSEDERIVED' + Postfix + '.' + IntToStr(DerivedCounter), BoldBooleanToTemplateVar[MoldClass.AllBoldMembers[i].ReverseDerived]);
        AddVariable('DERIVEDMEMBERFORWARDCODEDERVIED' + Postfix + '.' + IntToStr(DerivedCounter), BoldBooleanToTemplateVar[MoldClass.AllBoldMembers[i].ManuallyDerived]);

        AddVariable('DERIVEDMEMBERNAME' + Postfix + '.' + IntToStr(DerivedCounter), MoldClass.AllBoldMembers[i].ExpandedDelphiName);
        AddVariable('DERIVEDMEMBERINDEX' + Postfix + '.' + IntToStr(DerivedCounter), IntToStr(i));        
        Inc(DerivedCounter);
      end;
    end;
    AddVariable('DERIVEDMEMBERCOUNT' + Postfix, IntToStr(DerivedCounter));
  end;

  procedure BuildListOfAllUsedUnits(StringList: TStringList);
  var
    c: integer;
    a: integer;
    Mapping: TBoldTypeNameMapping;
  begin
    StringList.Clear;
    StringList.Sorted := True;
    StringList.Duplicates := dupIgnore;
    for c := 0 to Model.Classes.Count - 1 do
    begin
      for a := 0 to Model.Classes[c].Attributes.Count - 1 do
      begin
        Mapping := TypenameDictionary.MappingForModelName[Model.Classes[c].Attributes[a].BoldType];
        if Assigned(Mapping) then
          if (Mapping.UnitName = '') then
            StringList.Add('BoldAttributes')
          else
            StringList.Add(Mapping.UnitName);
       end;
    end;
  end;

begin
  Template.Variables.Clear;
  if GenerateMIDLCode then
    AddVariable('FORWARDDECLAREINTERFACES', '1');
  AddVariable('USETYPEDLISTS', BoldBooleanToTemplateVar[useTypedLIsts]);
  AddVariable('UNITNAME', Component.Name);

  if MoldModel.GUID <> '' then
    AddVariable('UNITGUID', MoldModel.GUID)
  else
    AddVariable('UNITGUID', BoldCreateGUIDAsString(true));

  AddVariable('CRC', MoldModel.CRC);

  AddVariable('COPYRIGHTNOTICE', Model.BoldTVByName['CopyrightNotice']);
  Dependencies := TStringList.Create;
  Component.GetInterfaceDependencies(Dependencies);
  for i := 0 to Dependencies.Count - 1 do
  begin
    AddVariable('INTERFACEADAPTERDEPENDENCY.' + IntToStr(i), Dependencies[i] + 'Adapters');
    AddVariable('INTERFACEDEPENDENCY.' + IntToStr(i), Dependencies[i]);
  end;
  AddVariable('INTERFACEDEPENDENCIESCOUNT', IntToStr(Dependencies.Count));

  Component.GetImplementationDependencies(Dependencies);
  for i := 0 to Dependencies.Count - 1 do
  begin
    AddVariable('IMPLEMENTATIONADAPTERDEPENDENCY.' + IntToStr(i), Dependencies[i] + 'Adapters');
    AddVariable('IMPLEMENTATIONDEPENDENCY.' + IntToStr(i), Dependencies[i]);
  end;
  AddVariable('IMPLEMENTATIONDEPENDENCIESCOUNT', IntToStr(Dependencies.Count));

  BuildListOfAllUsedUnits(Dependencies);
  for i := 0 to Dependencies.Count-1 do
    AddVariable('ATTRIBUTECLASSDEPENDENCY.'+IntToStr(i), Dependencies[i]);
  AddVariable('ATTRIBUTECLASSDEPENDENCIESCOUNT', IntToStr(Dependencies.Count));

  Dependencies.Free;

  for i := 0 to Model.Components.count - 1 do
  begin
    if temp <> '' then
      temp := temp + ', ';
    temp := temp + Model.Components[i].Name
  end;

  AddVariable('ALLCOMPONENTS', temp);

  ClassCount := 0;

  AddVariable('MODELNAME', MoldModel.ExpandedExpressionName);

  if MoldModel.Interfaceuses <> '' then
    AddVarList(Template, 'INTERFACEUSES', MoldModel.InterfaceUses + ',');

  if MoldModel.ImplementationUses <> '' then
    AddVarList(Template, 'IMPLEMENTATIONUSES', MoldModel.ImplementationUses + ',');

  IncludeFiles := TStringList.Create;

  if InitializeClasses then
  begin
    for i := 0 to MoldModel.Classes.Count - 1 do
    begin
      if (MoldModel.Classes[i].Component = Component) then
      begin
        if MoldModel.Classes[i].HasCodeStubs then
        begin
          FileName := MoldModel.Classes[i].EffectiveIncFileName(BoldGeneratorTemplatesManager.DefaultIncFileExtension);
          if (IncludeFiles.IndexOf(FileName) = -1) then
              IncludeFiles.Add(FileName);
        end;

        if (ClassCount + 1) mod DEBUGGERWORKAROUNDINTERVAL = 0 then
          AddVariable('DEBUGGERWORKAROUND.' + IntTostr(ClassCount), '1');

        InitializeClass(MoldModel.Classes[i], '.' + IntTostr(ClassCount));
        inc(ClassCount);
        BoldLog.ProgressStep;
        BoldLog.Sync;
        if BoldLog.ProcessInterruption then
          exit;
      end;
    end;
  end;

  AddVariable('CLASSCOUNT', IntToStr(ClassCount));
  for i := 0 to IncludeFiles.Count - 1 do
    AddVariable('INCLUDEFILE.' + IntToStr(i), IncludeFiles[i]);
  AddVariable('INCLUDEFILECOUNT', IntToStr(includeFiles.Count));

  if GenerateBold1CompatibleCode then
    AddVariable('DELPHIATTRIBUTEPOSTFIX', '_')
  else
    AddVariable('BOLDATTRIBUTEPOSTFIX', 'Attribute');

  IncludeFiles.Free;
end;

procedure TBoldGenerator.SetCurrentFileHandler(const path, Filename: string; ModuleType: TBoldModuleType; Show: boolean; IsIncFile: Boolean);
begin
  if IsIncFile then
    fBoldFileHandler := BoldFileHandlerForFile(path, FileName, ModuleType, show, AddIncFileHeader)
  else
    fBoldFileHandler := BoldFileHandlerForFile(path, FileName, ModuleType, show, nil);
end;

procedure TBoldGenerator.EnsureMethod(Strings: TStrings);
var
  SearchString: String;
begin
  SearchString := BoldGeneratorTemplatesManager.GetSearchStringfromMethodHeader(Strings[0], SEARCHPARAMETERLIST);
  if not FindInCurrentFile(SearchString) then
  begin
    BoldFileHandler.NewLine;
    BoldFilehandler.AddStrings(Strings);
  end;
end;

procedure TBoldGenerator.EnsureMethodImplementations;
var
  F, C, i: Integer;
  LastIncFileName, CurrentIncFileName: String;
  Attr: TMoldAttribute;
  Mapping: TBoldTypeNameMapping;
  SuperMethodIsAbstract: Boolean;
  MoldMethod: TMoldMethod;
  typeName: String;

procedure InitializeMethod(MoldClass: TMoldClass; MoldMethod: TMoldMethod; Variables: TBoldTemplateVariables);
var
  InheritedCall: String;
begin
  inheritedCall := BoldGeneratorTemplatesManager.GenerateInheritedCall(MoldClass, MoldMethod);

  Variables.SetVariable('INHERITEDCALL', InheritedCall);
  Variables.SetVariable('CLASSNAME', MoldClass.ExpandedDelphiName);
  AddSuperClassName(Variables, MoldClass);
end;

begin
  for F := 0 to MoldModel.Components.Count - 1 do
  begin
    fCurrentComponent := MoldModel.Components[f];
    CurrentIncFileName := '';
    for C := 0 to MoldModel.Classes.Count - 1 do
    begin
      if BoldLog.ProcessInterruption then
        exit;

      CurrentClass := MoldModel.Classes[c];
      if CurrentClass.Component = MoldModel.Components[F] then
      begin
        if CurrentClass.HasCodeStubs and
          (CurrentClass.Component = MoldModel.Components[F]) then
        begin
          LastIncFileName := CurrentIncFileName;
          CurrentIncFileName := CurrentClass.EffectiveIncFileName(BoldGeneratorTemplatesManager.DefaultIncFileExtension);

          if CompareText(LastIncFileName, CurrentIncFileName) <> 0 then
          begin
            SetCurrentFileHandler(BaseFilePath, CurrentIncFileName, mtIncFile, true, true);
            initializeMethodIndex;
          end;
          BoldLog.LogFmt(sProcessingClassXFileY, [CurrentClass.Name, CurrentIncFileName]);

          // userdefined methods

          for i := 0 to CurrentClass.Methods.Count - 1 do
            if CurrentClass.Methods[i].FuncType <> dfAbstractVirtual then
            begin
              with BoldGeneratorTemplatesManager.MethodTemplate do
              begin
                InitializeMethod(CurrentClass, CurrentClass.Methods[i], Variables);
                Variables.Setvariable('METHODHEADER', MethodToDelphiHeader(CurrentClass, CurrentClass.Methods[i], ImplementationTag, true, false));
                Variables.SetVariable('CALLINHERITED', BoldBooleanToTemplateVar[CurrentClass.Methods[i].CanCallInherited]);
                EnsureMethod(ExpandedTemplate);
              end;
            end;
          for i := 0 to CurrentClass.AllAutoOverrideMethods.Count - 1 do
            with BoldGeneratorTemplatesManager.MethodTemplate do
            begin
              MoldMethod := CurrentClass.AllAutoOverrideMethods[i];
              InitializeMethod(CurrentClass, MoldMethod, Variables);
              Variables.Setvariable('METHODHEADER', MethodToDelphiHeader(CurrentClass, CurrentClass.AllAutoOverrideMethods[i], ImplementationTag, true, true));
              SuperMethodIsAbstract := (MoldMethod.funcType = dfAbstractVirtual) and (CurrentClass.SuperClass = MoldMethod.Moldclass);
              Variables.SetVariable('CALLINHERITED', BoldBooleanToTemplateVar[not SuperMethodIsAbstract]);
              EnsureMethod(ExpandedTemplate);
            end;

          for i := CurrentClass.FirstOwnNativeAttributeIndex to CurrentClass.AllNativeAttributes.Count - 1 do
          begin
            Attr := CurrentClass.AllNativeAttributes[i];
            if Attr.DelphiPropertyRead in [pkPrivateMethod, pkProtectedVirtualMethod] then
            begin
              TypeName := GetNativeDelphiTypeForModelNameNoDefaults(Attr);
              with BoldGeneratorTemplatesManager.MethodTemplate do
              begin
                Variables.SetVariable('CLASSNAME', CurrentClass.ExpandedDelphiName);
                Variables.Setvariable('METHODHEADER', BoldGeneratorTemplatesManager.readMethodSignature(
                    CurrentClass.ExpandedDelphiName, Attr.ExpandedDelphiName, TypeName));
                Variables.SetVariable('CALLINHERITED', BoldBooleanToTemplateVar[False]);
                EnsureMethod(ExpandedTemplate);
              end;
            end;
          end;

          for i := CurrentClass.FirstOwnNativeAttributeIndex to CurrentClass.AllNativeAttributes.Count - 1 do
          begin
            Attr := CurrentClass.AllNativeAttributes[i];
            if Attr.DelphiPropertyWrite in [pkPrivateMethod, pkProtectedVirtualMethod] then
            begin
              TypeName := GetNativeDelphiTypeForModelNameNoDefaults(Attr);
              with BoldGeneratorTemplatesManager.MethodTemplate do
              begin
                Variables.SetVariable('CLASSNAME', CurrentClass.ExpandedDelphiName);
                Variables.Setvariable('METHODHEADER', BoldGeneratorTemplatesManager.WriteMethodSignature(
                    CurrentClass.ExpandedDelphiName, Attr.ExpandedDelphiName, TypeName));
                Variables.SetVariable('CALLINHERITED', BoldBooleanToTemplateVar[false]);
                EnsureMethod(ExpandedTemplate);
              end;
            end;
          end;

          for i := 0 to CurrentClass.AllBoldMembers.Count - 1 do
            if (CurrentClass.AllBoldMembers[i].Derived and((CurrentClass.AllBoldMembers[i].DerivationOcl = '')  or CurrentClass.AllBoldMembers[i].ReverseDerived))  and
              (
               (i >= CurrentClass.FirstOwnBoldMemberIndex) or
               (TVIsTrue(CurrentClass.AllBoldMembers[i].BoldTVByName[TAG_VIRTUALDERIVE]))
              ) then
            begin
              if CurrentClass.AllBoldMembers[i].ManuallyDerived then
                with BoldGeneratorTemplatesManager.DerivedMethodTemplate do
                begin
                  if (CurrentClass.AllBoldMembers[i] is TMoldAttribute) then
                    Mapping := TypeNameDictionary.MappingForModelName[(CurrentClass.AllBoldMembers[i] as TMoldAttribute).BoldType]
                  else
                    Mapping := nil;
                  if assigned(Mapping) and
                    (Mapping.ExpandedAccessor <> '') and
                    (Mapping.ExpandedNativeType <> '') then
                  begin
                    Variables.SetVariable('MEMBERHASNATIVE', '1');
                    Variables.SetVariable('MEMBERNATIVEACCESSOR', Mapping.ExpandedAccessor);
                    Variables.SetVariable('MEMBERNATIVETYPE', Mapping.ExpandedNativeType);
                  end else
                    Variables.SetVariable('MEMBERHASNATIVE', '0');

                  Variables.SetVariable('CLASSNAME', CurrentClass.ExpandedDelphiName);
                  Variables.SetVariable('INHERITEDCALL', 'inherited');
                  Variables.SetVariable('INTRODUCEDHERE', BoldBooleanToTemplateVar [i >= CurrentClass.FirstOwnBoldMemberIndex]);
                  Variables.SetVariable('MEMBERNAME', CurrentClass.AllBoldMembers[i].ExpandedDelphiName);
                  AddSuperClassName(variables, CurrentClass);
                  EnsureMethod(ExpandedTemplate);
                end;
              if CurrentClass.AllBoldMembers[i].Derived and CurrentClass.AllBoldMembers[i].ReverseDerived then
                with BoldGeneratorTemplatesManager.ReverseDeriveMethodTemplate do
                  begin
                    Variables.SetVariable('CLASSNAME', CurrentClass.ExpandedDelphiName);
                    Variables.SetVariable('INHERITEDCALL', 'inherited');
                    Variables.SetVariable('INTRODUCEDHERE', BoldBooleanToTemplateVar[i >= CurrentClass.FirstOwnBoldMemberIndex]);
                    Variables.SetVariable('MEMBERNAME', CurrentClass.AllBoldMembers[i].ExpandedDelphiName);
                    AddSuperClassName(variables, CurrentClass);
                    EnsureMethod(ExpandedTemplate);
                  end;
            end;
        end;
        BoldLog.progressStep;
        BoldLog.Sync;
      end;
    end;
  end;
end;

function TBoldGenerator.MethodToCOMHeader(OwningClass: TMoldClass; Method: TMoldMethod; InterfaceCode: Boolean; ParametersToCoerce, ParametersToInterfaceCoerce: TStringList): String;
begin
  result := BoldGeneratorTemplatesManager.MethodToCOMHeader(OwningClass, Method, InterfaceCode, ParametersToCoerce, ParametersToInterfaceCoerce, MoldModel, GenerateMIDLCode);
end;

function TBoldGenerator.MethodToIDLHeader(OwningClass: TMoldClass; Method: TMoldMethod): String;
var
  p: string;
  ParamType: String;
  params: string;
  i: integer;
  IsPtr: Boolean;
  Param: TMoldParameter;
begin
  params := '';
  for i := 0 to Method.Parameters.Count - 1 do
  begin
    if Params <> '' then
      params := params + ', ';
    Param := TMoldParameter(Method.Parameters[i]);
    case param.ParameterKind of
      pdIn: P := '[in]';
      pdOut: P := '[out]';
      pdInout: P := '[in, out]';
      pdReturn: P := '[out, retval]';
    end;
    paramType := TBoldMetaSupport.ParameterTypeToIDLType(Param.ParameterType, MoldModel, IsPtr);
    if not GenerateMIDLCode then
      ParamType := EnsureSafeIDLType(ParamType, Method.MoldClass);

    p := p + ' ' + ParamType;

    if IsPtr then
      p := p + '*';
    if Param.ParameterKind in [pdOut, pdInout, pdReturn] then
      p := p + '*';
    p := p + ' ' + Param.ParameterName;
    Params := params + p;
  end;

  if Method.HasReturnValue then
  begin
    if Params <> '' then
      Params := Params + ', ';
    p := TBoldMetaSupport.ParameterTypeToIDLType(Method.ReturnType, MoldModel, IsPtr);
    if not GenerateMIDLCode then
      P := EnsureSafeIDLType(P, Method.MoldClass);

    p := p + '*';
    if IsPtr then
      p := p + '*';

    Params := Params + '[out, retval] ' + p +  ' ReturnParam';
  end;

  if params = '' then
    params := 'void';

  result := Method.ExpandedDelphiName + '(' + Params + ')';
end;

function TBoldGenerator.MethodToCOMCall(OwningClass: TMoldClass; Method: TMoldMethod; ParametersToCoerce, ParametersToInterfaceCoerce: TStringList): String;
begin
  result := BoldGeneratorTemplatesManager.MethodToCOMCall(OwningClass, Method, ParametersToCoerce, ParametersToInterfaceCoerce, MoldModel);
end;

function TBoldGenerator.MethodToDelphiHeader(OwningClass: TMoldClass; Method: TMoldMethod; TagValue: Integer; AddSignature: Boolean; AutoOverride: Boolean): String;
begin
  result := BoldGeneratorTemplatesManager.MethodToCodeHeader(OwningClass, Method, TagValue, AddSignature, AutoOverride);
end;

procedure TBoldGenerator.AddIncFileHeader(StringList: TStringList);
begin
  InitializeTemplateForComponent(BoldGeneratorTemplatesManager.IncFileHeaderTemplate, MoldModel, fCurrentComponent, false);
  StringList.AddStrings(BoldGeneratorTemplatesManager.IncFileHeaderTemplate.ExpandedTemplate);
end;

function TBoldGenerator.FindInCurrentFile(s: String): Boolean;
var
  i: integer;
begin
  result := false;
  s := Uppercase(s);

  for i := 0 to MethodIndex.count - 1 do
  begin
    if pos(s, MethodIndex[i]) <> 0 then
    begin
      result := true;
      exit;
    end;
  end;

end;

procedure TBoldGenerator.InitializeMethodIndex;
var
  i: integer;
begin
  MethodIndex.Clear;
  for i := 0 to BoldFilehandler.Count - 1 do
  begin
    if BoldGeneratorTemplatesManager.StringContainsMethodHeader(BoldFileHandler.Content[i]) then
      MethodIndex.Add(uppercase(BoldFileHandler.Content[i]));
  end;
end;

procedure TBoldGenerator.SetMoldModel(const Value: TMoldModel);
begin
  if fMoldModel <> Value then
  begin
    fMoldModel := Value;
    MoldModel.EnsureTopSorted;
  end;
end;

procedure TBoldGenerator.SetGenerateBold1CompatibleCode(const Value: Boolean);
begin
  FGenerateBold1CompatibleCode := Value;
end;

{ TBoldCodeGenInitializer }

constructor TBoldCodeGenInitializer.create(MoldModel: TMoldModel);
begin
  fMoldModel := MoldModel;
end;

function TBoldCodeGenInitializer.FindComponent(const ComponentName: string): TMoldComponent;
begin
  result := MoldModel.FindComponent(ComponentName);
end;

function TBoldCodeGenInitializer.EnsureComponent(
  const ComponentName: string): TMoldComponent;
begin
  result := MoldModel.EnsureComponent(ComponentName);
end;

procedure TBoldCodeGenInitializer.Execute;
begin
  MoveImplicitLinkClassesToSuperClassComponent;
  EnsureInheritanceDependencies;
  MoveImplicitLinkClassesToAssociationEndComponent;
end;

function TBoldCodeGenInitializer.FilterLinkClass(MoldClass: TMoldClass): Boolean;
begin
  result := assigned(MoldClass.Association);
end;

procedure TBoldCodeGenInitializer.MoveClassesToComponent(MoldClassFilterFunction: TMoldClassFilterFunction;
                                                         const ComponentName: String);
var
   i: integer;
   TargetComponent: TMoldComponent;
begin
  TargetComponent := nil;
  for i := 0 to MoldModel.Classes.count - 1 do
  begin
    if MoldClassFilterFunction(MoldModel.Classes[i]) then
    begin
      if not Assigned(TargetComponent) then
        TargetComponent := EnsureComponent(ComponentName);
      MoldModel.Classes[i].Component := TargetComponent;
    end;
  end;
end;

function TBoldCodeGenInitializer.RenameComponent(const OldComponentName,
  NewComponentName: string): TMoldComponent;
begin
  result := MoldModel.RenameComponent(OldComponentName, NewComponentName);
end;

procedure TBoldCodeGenInitializer.MoveClassTreeToComponent(
  SuperClass: TMoldClass; const ComponentName: String; MoveSuperClass: Boolean);
var
   i: integer;
   TargetComponent: TMoldComponent;
begin
  TargetComponent := nil;
  if not assigned(SuperClass) then
    raise Exception.Create(sMoveToComponent_NoSuperClass);

  if not assigned(MoldModel) then
    raise Exception.Create(sMoveToComponent_NoMoldModel);

  for i := 0 to MoldModel.Classes.count - 1 do
  begin
    if MoldModel.Classes[i].ChildTo(SuperClass) and
      (MoveSuperClass or (MoldModel.Classes[i] <> SuperClass)) then
    begin
      if not assigned(TargetComponent) then
        TargetComponent := EnsureComponent(ComponentName);
      MoldModel.Classes[i].Component := TargetComponent;
    end;
  end;
end;


procedure TBoldCodeGenInitializer.EnsureDependency(const SubComponentName,
  SuperComponentname: String);
var
  SubComponent: TMoldComponent;
  SuperComponent: TMoldComponent;
begin
  if CompareText(SubComponentName, SuperComponentName) = 0 then
    exit;
  SubComponent := FindComponent(SubComponentName);
  SuperComponent := FindComponent(SuperComponentname);

  if assigned(SubComponent) and
     assigned(SuperComponent) then
  begin
    if SubComponent.Dependencies.IndexOf(SuperComponent) = -1 then
      SubComponent.Dependencies.add(SuperComponent);
  end;
end;

procedure TBoldCodeGenInitializer.EnsureInheritanceDependencies;
var
   i: integer;
begin
  for i := 0 to MoldModel.Classes.count - 1 do
    if assigned(MoldModel.Classes[i].SuperClass) then
      EnsureDependency(MoldModel.Classes[i].Component.Name, MoldModel.Classes[i].SuperClass.Component.name);
end;

function TBoldCodeGenInitializer.ValidateFileNames: Boolean;
var
   i, j: integer;
begin
  result := true;
  for i := 0 to MoldModel.Classes.Count - 1 do
  begin
    for j := 0 to MoldModel.Components.count - 1 do
    begin
      if (MoldModel.Classes[i].Component <> MoldModel.Components[j]) and
        (CompareText(MoldModel.Classes[i].EffectiveIncFileName(BoldGeneratorTemplatesManager.DefaultIncFileExtension), MoldModel.Components[j].Name + '.'+BoldGeneratorTemplatesManager.DefaultIncFileExtension) = 0) then
      begin
        BoldLog.LogFmt(sCollidingFileName, [MoldModel.Classes[i].Name, MoldModel.Components[j].Name]);
        result := false;
      end;
    end;
  end;
end;

procedure TBoldCodeGenInitializer.MoveClassToComponent(const ClassName,
  ComponentName: String);
begin
  MoveClassToComponentByClass(MoldModel.GetClassByName(ClassName), ComponentName);
end;

procedure TBoldCodeGenInitializer.MoveClassToComponentByClass(MoldClass: TMoldClass;
  const ComponentName: String);
begin
  MoldClass.Component := EnsureComponent(ComponentName);
end;

function TBoldGenerator.GetNativeDelphiTypeForModelNameNoDefaults(Attr: TMoldAttribute): String;
var
  Mapping: TBoldTypeNameMapping;
begin
  Mapping := TypeNameDictionary.ExactMappingForModelName[Attr.BoldType];
  if assigned(mapping) then
    result := Mapping.ExpandedNativeType
  else
    result := Attr.BoldType;
end;

function TBoldGenerator.EnsureSafeIDLType(const ParamType: String;
  MoldClass: TMoldClass): String;
var
  i: integer;
begin
  for i := 0 to MoldClass.Model.Classes.Count - 1 do
  begin
    if CompareText(ParamType, MoldClass.Model.Classes[i].ExpandedInterfaceName) = 0 then
    begin
      result := MoldClass.LowestCommonSuperClass(MoldClass.Model.Classes[i]).ExpandedInterfaceName;
      exit;
    end;
  end;
  result := ParamType;
end;

procedure TBoldGenerator.GenerateComInterfaces;
var
  j, ComponentIx: integer;
begin
  MoldModel.EnsureTopSorted;
  GenerateIDLVariables := true;
  if not GenerateMIDLCode then
    UseTypedLists := false;

  if assigned(BoldCodeGenInitializerClass) then
    with BoldCodeGenInitializerClass.Create(MoldModel) do
    try
      Execute;
      ValidateFileNames;
    finally
      free;
    end;

  BoldLog.ProgressMax := MoldModel.Classes.Count * 2;

  BoldLog.LogFmt(sLogGeneratingInPath, [BaseFilePath]);
  for ComponentIx := 0 to MoldModel.Components.Count - 1 do
  begin
    for j := 0 to BoldGeneratorTemplatesManager.ComFileTemplates.Count - 1 do
    begin
      if BoldLog.ProcessInterruption then
        exit;

      with BoldGeneratorTemplatesManager.COMFileTemplates[j] do
      begin
        Variables.SetVariable('COMPONENTNAME', MoldModel.Components[ComponentIx].Name); // do not localize
        BoldLog.LogFmt(sLogGeneratingFile, [ExpandedFileName]);

        SetCurrentFileHandler(BaseFilePath, ExpandedFileName, ModuleTypeForFile(ExpandedFileName), true, false);
        BoldFilehandler.Clear;
        BoldLog.LogHeader := sLogInitializingVars;
        InitializeTemplateForComponent(BoldGeneratorTemplatesManager.COMFileTemplates[j], MoldModel, MoldModel.Components[ComponentIx], true);
        if BoldLog.ProcessInterruption then
          exit;

        BoldLog.LogHeader := sLogExpandingTemplate;
        BoldFilehandler.AddStrings(ExpandedTemplate);
      end;
    end;
  end;
end;

procedure TBoldGenerator.AddVarList(Template: TBoldTemplateHolder; VariableBaseName, CommaValues: string);
var
  sList: TStringList;
  i: integer;
begin
  sList := TStringList.Create;
  sList.Commatext := CommaValues;
  while sList[sList.Count-1] = '' do
    sList.Delete(SList.Count-1);
  for i := 0 to sList.Count-1 do
    Template.Variables.ForceAdd(Variablebasename+'.'+IntToStr(i), SList[i], []);
  Template.Variables.ForceAdd(Variablebasename+'COUNT', IntToStr(sList.Count), []);
  sList.Free;
end;

procedure TBoldGenerator.AddSuperClassName(variables: TBoldTemplateVariables;
  MoldClass: tMoldClass);
begin
  if assigned(MoldClass.SuperClass) then
    variables.SetVariable('SUPERCLASSNAME', MoldClass.SuperClass.ExpandedDelphiName)
  else
    variables.SetVariable('SUPERCLASSNAME', 'TBoldObject')
end;

procedure TBoldCodeGenInitializer.MoveImplicitLinkClassesToAssociationEndComponent;
  function GetAssociationEndComponent(Association: TMoldAssociation; Index: integer): TMoldComponent;
  begin
    if assigned(Association.Roles[index]) and
       assigned(Association.Roles[index].MoldClass) and
       assigned(Association.Roles[index].MoldClass.component) then
       result := Association.Roles[index].MoldClass.Component
    else
      result := nil;
  end;
var
  i: integer;
  LinkClass: TMoldClass;
  Component1, Component2: TMoldComponent;
begin
  for i := 0 to MoldModel.Classes.Count-1 do
  begin
    if assigned(MoldModel.Classes[i].Association) then
    begin
      LinkClass := MoldModel.Classes[i];
      if LinkClass.TVByName[BOLDBOLDIFYPREFIX+TAG_AUTOCREATED] = TV_TRUE then
      begin
        Component1 := GetAssociationEndComponent(LinkClass.Association, 0);
        Component2 := GetAssociationEndComponent(LinkClass.Association, 1);


        if assigned(Component1) and (Component1 <> LinkClass.Component) and
          (Component1 = Component2) and
          Component1.DependentOf(LinkClass.Component) then
            LinkClass.Component := Component1;
      end;
    end;
  end;
end;

procedure TBoldCodeGenInitializer.MoveImplicitLinkClassesToSuperClassComponent;
var
  i: integer;
  LinkClass: TMoldClass;
begin
  for i := 0 to MoldModel.Classes.Count-1 do
  begin
    if assigned(MoldModel.Classes[i].Association) then
    begin
      LinkClass := MoldModel.Classes[i];
      if (LinkClass.Component = MoldModel.MainComponent) and
        (LinkClass.TVByName[BOLDBOLDIFYPREFIX+TAG_AUTOCREATED] = TV_TRUE) then
      begin
        if (LinkClass.SuperClass.Component <> MoldModel.MainComponent) then
          LinkClass.Component := LinkClass.SuperClass.Component;
      end;
    end;
  end;
end;

function TBoldGenerator.MakePersistenceInterfaceName(MoldClass: TMoldClass): string;
begin
  result := BoldExpandName('IPersistent<Name>', MoldClass.Name, xtDelphi, -1, nccDefault)
end;

procedure TBoldGenerator.GenerateBusinessObjectCode;
begin
  ExpandTemplateList(BoldGeneratorTemplatesManager.FileTemplates);
end;

procedure TBoldGenerator.GeneratePersistenceInterfaces;
var
  TemplateList: TBoldTemplateList;
begin
  if assigned(BoldGeneratorTemplatesManager.PersistenceInterfaceTemplate) then
  begin
    TemplateList := TBoldTemplateList.Create;
    TemplateList.Add(BoldGeneratorTemplatesManager.PersistenceInterfaceTemplate);
    ExpandTemplateList(TemplateList);
    TemplateList.free;
  end
  else
    raise EBold.create(sNoTemplateForPersistenceInterfaces);
end;

function TBoldGenerator.ModuleTypeForFile(const FileName: string): TBoldModuleType;
var
  Extension: string;
begin
  Extension := UpperCase(ExtractFileExt(FileName));
  if Extension = '.PAS' then
    Result := mtUnit
  else if Extension = '.INC' then
    Result := mtIncFile
  else
    Result := mtText;
end;

end.