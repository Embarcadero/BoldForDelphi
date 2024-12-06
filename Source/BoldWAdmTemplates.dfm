object attrdatamodule: Tattrdatamodule
  Height = 479
  Width = 741
  object ValueSetTemplate: TBoldTemplateHolder
    Template.Strings = (
      '{*******************************}'
      '{   This unit was created by    }'
      '{ the BoldSoft Attribute Wizard }'
      '{      $(DATETIME)      }'
      '{*******************************}'
      ''
      'unit $(UNITNAME);'
      ''
      'interface'
      ''
      'uses'
      '  BoldAttributes,'
      '  BoldMemberTypeDictionary,'
      '  BoldSystem,'
      '  BoldDefs;'
      ''
      
        '// To install this attribute type in your application, add the f' +
        'ollowing to your'
      '// TypeNameHandle:'
      '//'
      '// ModelName:      $(EXPRESSIONNAME)'
      '// ExpressionName: $(EXPRESSIONNAME)'
      '// DelphiName:     $(DELPHINAME)'
      '// ContentName:    integer'
      '// PMapper:        TBoldPMInteger'
      '// Accessor:       As<Name>'
      '// NativeType:     T<Name>Enum'
      
        '// UnitName:       $(UNITNAME)    // This unit'#39's name, modify if' +
        ' necessary'
      ''
      ''
      'type'
      '$(CASEVALUES)\'
      'true| T$(EXPRESSIONNAME)Enum = (\'
      'true|$(LOOPVALUECOUNT)\'
      'true|$(,VALUECOUNT:, )$(VALUEPREFIX)$(VALUENAME)\'
      'true|$(ENDLOOPVALUECOUNT));'
      '$(ENDCASEVALUES)\'
      ' $(DELPHINAME) = class(TBAValueSet)'
      ' private'
      '   function GetAs$(EXPRESSIONNAME): T$(EXPRESSIONNAME)Enum;'
      
        '   procedure SetAs$(EXPRESSIONNAME)(const Value: T$(EXPRESSIONNA' +
        'ME)Enum);'
      ' protected'
      '   function GetValues: TBAValueSetValueList; override;'
      ' public'
      
        '   property As$(EXPRESSIONNAME): T$(EXPRESSIONNAME)Enum read Get' +
        'As$(EXPRESSIONNAME) write SetAs$(EXPRESSIONNAME);'
      ' end;'
      ''
      'implementation'
      'uses'
      '  SysUtils,'
      '  BoldUtils;'
      'var'
      '  _$(EXPRESSIONNAME)Values: TBAValueSetValueList;'
      ''
      
        'function $(DELPHINAME).GetAs$(EXPRESSIONNAME): T$(EXPRESSIONNAME' +
        ')Enum;'
      'begin'
      '  result := T$(EXPRESSIONNAME)Enum(AsInteger);'
      'end;'
      ''
      'function $(DELPHINAME).GetValues: TBAValueSetValueList;'
      'begin'
      '  if not Assigned(_$(EXPRESSIONNAME)Values) then'
      '  begin'
      '    _$(EXPRESSIONNAME)Values := TBAValueSetValueList.Create;'
      '$(LOOPVALUECOUNT)\'
      
        '    _$(EXPRESSIONNAME)Values.Add(integer($(VALUEPREFIX)$(VALUENA' +
        'ME)), [\'
      '$(LOOPVALUEREPRESENTATIONCOUNT)\'
      '$(,VALUEREPRESENTATIONCOUNT:, )'#39'$(VALUEREPRESENTATION)'#39'\'
      '$(ENDLOOPVALUEREPRESENTATIONCOUNT)]);'
      '$(ENDLOOPVALUECOUNT)\'
      '  end;'
      '  Result := _$(EXPRESSIONNAME)Values;'
      'end;'
      ''
      
        'procedure $(DELPHINAME).SetAs$(EXPRESSIONNAME)(const Value: T$(E' +
        'XPRESSIONNAME)Enum);'
      'begin'
      '  AsInteger := Integer(Value);'
      'end;'
      ''
      'initialization'
      
        '  BoldmemberTypes.AddMemberTypeDescriptor($(DELPHINAME), alConcr' +
        'ete);'
      'finalization'
      '  FreeAndNil(_$(EXPRESSIONNAME)Values);'
      '  if BoldMemberTypesAssigned then'
      '    BoldMemberTypes.RemoveDescriptorByClass($(DELPHINAME));'
      'end.')
    Left = 80
    Top = 24
  end
  object SubClassedAttrTemplate: TBoldTemplateHolder
    Template.Strings = (
      '$(MACROSTARTProtectedMethods)\'
      '  protected'
      '    {protected declarations}'
      '$(CASEGetStringRepresentation)\'
      
        'true|   function GetStringRepresentation(Representation: TBoldRe' +
        'presentation): string; override;'
      '$(ENDCASEGetStringRepresentation)\'
      '$(CASESetStringRepresentation)\'
      
        'true|   procedure SetStringRepresentation(Representation: TBoldR' +
        'epresentation; Value: string); override;'
      '$(ENDCASESetStringRepresentation)\'
      '$(CASEGetStreamName)\'
      'true|   function GetStreamName: string; override;'
      '$(ENDCASEGetStreamName)\'
      '$(LOOPPROTECTEDMETHODCOUNT)\'
      
        '    $(PROTECTEDMETHODTYPE) $(PROTECTEDMETHODNAME) $(PROTECTEDMET' +
        'HODSIGNATURE) $(PROTECTEDMETHODDIRECTIVES)\'
      '$(ENDLOOPPROTECTEDMETHODCOUNT)\'
      '$(MACROEND ProtectedMethods)\'
      '\'
      '$(MACROSTARTPrivateMethods)\'
      '  private'
      '    {private declarations}'
      '$(LOOPFIELDCOUNT)\'
      '    f$(FIELDNAME): $(FIELDTYPE);'
      '$(ENDLOOPFIELDCOUNT)\'
      '$(LOOPFIELDCOUNT)\'
      '$(CASEFIELDREADABLE)\'
      '1|    function Get$(FIELDNAME): $(FIELDTYPE);'
      '$(ENDCASEFIELDREADABLE)\'
      '$(CASEFIELDWRITABLE)\'
      '1|    procedure InternalSet$(FIELDNAME)(NewValue: $(FIELDTYPE));'
      '1|    procedure Set$(FIELDNAME)(NewValue: $(FIELDTYPE));'
      '$(ENDCASEFIELDWRITABLE)\'
      '$(ENDLOOPFIELDCOUNT)\'
      '$(LOOPPRIVATEMETHODCOUNT)\'
      
        '    $(PRIVATEMETHODTYPE) $(PRIVATEMETHODNAME) $(PRIVATEMETHODSIG' +
        'NATURE) $(PRIVATEMETHODDIRECTIVES)\'
      '$(ENDLOOPPRIVATEMETHODCOUNT)\'
      '$(MACROEND PrivateMethods)\'
      '\'
      '$(MACROSTARTPublicMethods)\'
      '  public'
      '    {public declarations}'
      '$(CASESetAsVariant)\'
      'true|   procedure SetAsVariant(const Value: Variant); override;'
      '$(ENDCASESetAsVariant)\'
      '$(CASEGetAsVariant)\'
      'true|   function GetAsVariant: Variant; override;'
      '$(ENDCASEGetAsVariant)\'
      '$(CASEAssignValue)\'
      'true|   procedure AssignValue( Source: IBoldValue ); override;'
      '$(ENDCASEAssignValue)\'
      '$(CASEValidateString)\'
      
        'true|   function ValidateString(Value: string; Representation: T' +
        'BoldRepresentation): Boolean; override;'
      '$(ENDCASEValidateString)\'
      '$(CASEValidateCharacter)\'
      
        'true|  function ValidateCharacter(C: AnsiChar; Representation: T' +
        'BoldRepresentation): Boolean; override;'
      '$(ENDCASEValidateCharacter)\'
      '$(LOOPPUBLICMETHODCOUNT)\'
      
        '    $(PUBLICMETHODTYPE) $(PUBLICMETHODNAME) $(PUBLICMETHODSIGNAT' +
        'URE)  $(PUBLICMETHODDIRECTIVES)'
      '$(ENDLOOPPUBLICMETHODCOUNT)\'
      '$(MACROEND PublicMethods)\'
      '\'
      '$(MACROSTARTPublishedMethods)\'
      '  published'
      '    {published declarations}'
      '$(LOOPPUBLISHEDMETHODCOUNT)\'
      
        '    $(PUBLISHEDMETHODTYPE) $(PUBLISHEDMETHODNAME) $(PUBLISHEDMET' +
        'HODSIGNATURE) $(PUBLISHEDMETHODDIRECTIVES)\'
      '$(ENDLOOPPUBLISHEDMETHODCOUNT)\'
      '$(LOOPFIELDCOUNT)\'
      '    property $(FIELDNAME): $(FIELDTYPE) \'
      '$(CASEFIELDREADABLE)\'
      '1|read Get$(FIELDNAME) \'
      '$(ENDCASEFIELDREADABLE)\'
      '$(CASEFIELDWRITABLE)\'
      '1|write Set$(FIELDNAME)\'
      '$(ENDCASEFIELDWRITABLE)\'
      ';'
      '$(ENDLOOPFIELDCOUNT)\'
      '$(MACROEND PublishedMethods)\'
      '\'
      '{*******************************}'
      '{   This unit was created by    }'
      '{ the BoldSoft Attribute Wizard }'
      '{      $(DATETIME)      }'
      '{*******************************}'
      ''
      'unit $(UNITNAME);'
      ''
      'interface'
      ''
      'uses'
      '  BoldMemberTypeDictionary,'
      '  BoldElements,'
      '  BoldValueInterfaces,'
      '  BoldAttributes,'
      '  BoldSystem,'
      '  BoldDefs,'
      '  BoldSubscription,'
      '  BoldStreams;'
      ''
      
        '// To install this attribute type in your application, add the f' +
        'ollowing to your'
      '// TypeNameHandle:'
      '//'
      '// ModelName:      $(EXPRESSIONNAME)'
      '// ExpressionName: $(EXPRESSIONNAME)'
      '// DelphiName:     $(DELPHINAME)'
      '// ContentName:    <same as for $(SUPERCLASS)>'
      '// PMapper:        <same as for $(SUPERCLASS)>'
      '// Accessor:       As<Name>'
      '// NativeType:     <same as for $(SUPERCLASS)>'
      
        '// UnitName:       $(UNITNAME)    // This unit'#39's name, modify if' +
        ' necessary'
      ''
      ''
      'type'
      '  $(DELPHINAME) = class($(SUPERCLASS))'
      '$(CASEPROTECTED)\'
      'true|$(PROTECTEDMETHODS)'
      '$(ENDCASEPROTECTED)\'
      '\'
      '$(CASEPRIVATE)\'
      'true|$(PRIVATEMETHODS)'
      '$(ENDCASEPRIVATE)\'
      '\'
      '$(CASEPUBLIC)\'
      'true|$(PublicMethods)'
      '$(ENDCASEPUBLIC)\'
      '\'
      '$(CASEPUBLISHED)\'
      'true|$(PublishedMethods)'
      '$(ENDCASEPUBLISHED)\'
      '  end;'
      ''
      'implementation'
      ''
      '$(CASEGetStringRepresentation)\'
      
        'true|function $(DELPHINAME).GetStringRepresentation(Representati' +
        'on: TBoldRepresentation): string;'
      'true|begin'
      'true| // your code here'
      'true| case representation of'
      
        'true|   brDefault: result := inherited GetStringRepresentation( ' +
        'Representation );'
      
        'true|   2: result := inherited GetStringRepresentation( Represen' +
        'tation );'
      
        'true|   else result := inherited GetStringRepresentation( Repres' +
        'entation );'
      'true| end;'
      'true|end;'
      'true|'
      '$(ENDCASEGetStringRepresentation)\'
      '$(CASESetStringRepresentation)\'
      
        'true|procedure $(DELPHINAME).SetStringRepresentation(Representat' +
        'ion: TBoldRepresentation; Value: string);'
      'true|begin'
      'true| case representation of'
      
        'true|   brDefault: inherited SetStringRepresentation( Representa' +
        'tion, Value );'
      'true|   2: ;'
      'true|   else ;'
      'true| end;'
      'true|end;'
      'true|'
      '$(ENDCASESetStringRepresentation)\'
      '$(CASEValidateString)\'
      
        'true|function $(DELPHINAME).ValidateString(Value: string; Repres' +
        'entation: TBoldRepresentation): Boolean;'
      'true|begin'
      'true| case representation of'
      
        'true|   brDefault: result := inherited ValidateString( value, Re' +
        'presentation );'
      'true| end;'
      'true|end;'
      'true|'
      '$(ENDCASEValidateString)\'
      '$(CASEValidateCharacter)\'
      
        'true|function $(DELPHINAME).ValidateCharacter(C: AnsiChar; Repre' +
        'sentation: TBoldRepresentation): Boolean;'
      'true|begin'
      'true| case representation of'
      
        'true|   brDefault: result := inherited ValidateCharacter( c, Rep' +
        'resentation );'
      'true| end;'
      'true|end;'
      'true|'
      '$(ENDCASEValidateCharacter)\'
      '\'
      '$(CASESetAsVariant)\'
      'true|procedure $(DELPHINAME).SetAsVariant(const Value: Variant);'
      'true|begin'
      'true| // your code here'
      'true|end;'
      'true|'
      '$(ENDCASESetAsVariant)\'
      '$(CASEGetAsVariant)\'
      'true|function $(DELPHINAME).GetAsVariant: Variant;'
      'true|begin'
      'true| // your code here'
      'true|end;'
      'true|'
      '$(ENDCASEGetAsVariant)\'
      '$(CASEAssignValue)\'
      'true|procedure $(DELPHINAME).AssignValue( Source: IBoldValue );'
      'true|begin'
      'true| // your code here'
      'true|end;'
      'true|'
      '$(ENDCASEAssignValue)\'
      '$(CASEGetStreamName)\'
      'true|function $(DELPHINAME).GetStreamName: string;'
      'true|begin'
      'true| inherited;'
      'true| // your code here'
      'true|end;'
      'true|'
      '$(ENDCASEGetStreamName)\'
      '$(LOOPPROTECTEDMETHODCOUNT)\'
      
        '$(PROTECTEDMETHODTYPE) $(DELPHINAME).$(PROTECTEDMETHODNAME)$(PRO' +
        'TECTEDMETHODSIGNATURE)'
      'begin'
      '  // your code here'
      'end;'
      '$(ENDLOOPPROTECTEDMETHODCOUNT)\'
      '\'
      '$(LOOPPRIVATEMETHODCOUNT)\'
      
        '$(PRIVATEMETHODTYPE) $(DELPHINAME).$(PRIVATEMETHODNAME)$(PRIVATE' +
        'METHODSIGNATURE)'
      'begin'
      '  // your code here'
      'end;'
      ''
      '$(ENDLOOPPRIVATEMETHODCOUNT)\'
      '\'
      '$(LOOPPUBLICMETHODCOUNT)\'
      
        '$(PUBLICMETHODTYPE) $(DELPHINAME).$(PUBLICMETHODNAME)$(PUBLICMET' +
        'HODSIGNATURE)'
      'begin'
      '  // your code here'
      'end;'
      ''
      '$(ENDLOOPPUBLICMETHODCOUNT)\'
      ''
      'initialization'
      
        '  BoldmemberTypes.AddMemberTypeDescriptor( $(DELPHINAME), alConc' +
        'rete);'
      ''
      'finalization'
      '  if BoldMemberTypesAssigned then'
      '    BoldMemberTypes.RemoveDescriptorByClass($(DELPHINAME));'
      'end.')
    Left = 80
    Top = 112
  end
  object MapperTemplate: TBoldTemplateHolder
    Template.Strings = (
      '{*******************************}'
      '{   This unit was created by    }'
      '{ the BoldSoft Attribute Wizard }'
      '{      $(DATETIME)      }'
      '{*******************************}'
      ''
      'unit $(UNITNAME);'
      ''
      'interface'
      ''
      'uses'
      '  DB,'
      '  BoldMeta,'
      '  BoldDefs,'
      '  BoldMemberTypeDictionary,'
      '  BoldId,'
      '  BoldValueInterfaces,'
      '  BoldValueSpaceInterfaces,'
      '  BoldDbInterfaces,'
      '  BoldPMappers,'
      '  BoldTypeNameDictionary,'
      '  BoldPMapperLists,'
      '  BoldPMappersDefault,'
      '  BoldPMappersAttributeDefault,'
      '  $(INTERFACEUNITNAME);'
      ''
      '// ************* READ ME ****************'
      '//'
      
        '// In order to use the persistence mapper in this unit there are' +
        ' a few'
      '// things that must be done'
      
        '// * In a few places the text "<ColumnDataType>" occurs, these h' +
        'as to be edited'
      '//   to reflect the datatypes of the database you will use'
      
        '// * To use the persistence mapper to generate a database schema' +
        ' in design time'
      
        '//   (from the Bold UML Model Editor) you must include it in a d' +
        'elphi package'
      '//   that you compile and install'
      
        '// * To use the persistence mapper in a project you must refer t' +
        'o this unit'
      
        '//   somewhere in your project (either in the dpr-file, or for e' +
        'xample in a'
      
        '//   uses-clause in the datamodule that contains the persistence' +
        ' handle'
      '//'
      '// ************ END READ ME *************'
      ''
      'type'
      '  $(MAPPERNAME) = class($(SUPERMAPPERNAME))'
      '  private'
      '    {Private declarations}'
      '  protected'
      '    {Protected declarations}'
      
        '    function GetColumnTypeAsSQL(ColumnIndex: Integer): string; o' +
        'verride;'
      
        '    function GetColumnBDEFieldType(ColumnIndex: Integer): TField' +
        'Type; override;'
      '    function GetColumnCount: Integer; override;'
      
        '    function GetColumnSize(ColumnIndex: Integer): Integer; overr' +
        'ide;'
      
        '    function GetInitialColumnName(ColumnIndex: Integer): string;' +
        ' override;'
      '  public'
      '    {Public declarations}'
      
        '    constructor CreateFromMold(Moldmember: TMoldmember; MoldClas' +
        's: TMoldClass; Owner: TBoldObjectPersistenceMapper; const Member' +
        'Index: Integer; TypeNameDictionary: TBoldTypeNameDictionary); ov' +
        'erride;'
      
        '    class function CanStore(const StreamName: string): Boolean; ' +
        'override;'
      
        '    procedure ValueToParam(ObjectContent: IBoldObjectContents; P' +
        'aram: IBoldParameter; ColumnIndex: Integer; TranslationList: TBo' +
        'ldIdTranslationList); override;'
      
        '    procedure ValueFromField(OwningObjectId: TBoldObjectId; Obje' +
        'ctContent: IBoldObjectContents; ValueSpace: IBoldValueSpace; Tra' +
        'nslationList: TBoldIdTranslationList; Field: IBoldField; ColumnI' +
        'ndex: Integer); override;'
      '    procedure CreatePersistentStorage; override;'
      
        '    procedure InitializeSystem(theDatabase: IBoldDataBase); over' +
        'ride;'
      '  end;'
      ''
      'implementation'
      ''
      
        'constructor $(MAPPERNAME).CreateFromMold(Moldmember: TMoldmember' +
        '; MoldClass: TMoldClass; Owner: TBoldObjectPersistenceMapper; co' +
        'nst MemberIndex: Integer; TypeNameDictionary: TBoldTypeNameDicti' +
        'onary);'
      'begin'
      
        '  // if you need additional columns/tables to store your attribu' +
        'te,'
      
        '  // this is where you should create the necessary PSDescription' +
        's'
      '  inherited;'
      'end;'
      ''
      
        'class function $(MAPPERNAME).CanStore(const StreamName: string):' +
        ' Boolean;'
      'begin'
      '  Result := StreamName = $(STREAMCONSTANT);'
      'end;'
      ''
      
        'procedure $(MAPPERNAME).ValueFromField(OwningObjectId: TBoldObje' +
        'ctId; ObjectContent: IBoldObjectContents; ValueSpace: IBoldValue' +
        'Space; TranslationList: TBoldIdTranslationList; Field: IBoldFiel' +
        'd; ColumnIndex: Integer);'
      'var'
      '  aValue: $(INTERFACENAME);'
      'begin'
      '  aValue := GetEnsuredValue(ObjectContent) as $(INTERFACENAME);'
      '  case ColumnIndex of'
      '$(LOOPFIELDCOUNT)\'
      '    $(FIELDCOUNT): begin'
      '      if Field.IsNull then'
      '        aValue.SetContentToNull'
      '      else'
      
        '        aValue.$(FIELDNAME) := Field.As<ColumnDataType>; // edit' +
        ' "As<ColumnDataType>" to reflect the correct property of the IBo' +
        'ldField'
      '    end;'
      '$(ENDLOOPFIELDCOUNT)\'
      '    else inherited;'
      '  end;'
      'end;'
      ''
      
        'procedure $(MAPPERNAME).ValueToParam(ObjectContent: IBoldObjectC' +
        'ontents; Param: IBoldParameter; ColumnIndex: Integer; Translatio' +
        'nList: TBoldIdTranslationList);'
      'var'
      '  aValue: $(INTERFACENAME);'
      'begin'
      '  aValue := GetEnsuredValue(ObjectContent) as $(INTERFACENAME);'
      '  case ColumnIndex of'
      '$(LOOPFIELDCOUNT)\'
      '    $(FIELDCOUNT): begin'
      '      if aValue.IsNull then'
      
        '        SetParamToNullWithDataType(Param, GetColumnBDEFieldType(' +
        'ColumnIndex))'
      '      else'
      
        '        Param.As<ColumnDataType> := aValue.$(FIELDNAME); // edit' +
        ' "As<ColumnDataType>"  to reflect the correct property of the TP' +
        'aram'
      '    end;'
      '$(ENDLOOPFIELDCOUNT)\'
      '    else inherited;'
      '  end;'
      'end;'
      ''
      
        'function $(MAPPERNAME).GetColumnTypeAsSQL(ColumnIndex: Integer):' +
        ' string;'
      'begin'
      '  case ColumnIndex of'
      '$(LOOPFIELDCOUNT)\'
      '    $(FIELDCOUNT): Result := '#39'$(FIELDTYPE)'#39';'
      '$(ENDLOOPFIELDCOUNT)\'
      '    else Result := '#39'integer'#39';'
      '  end;'
      'end;'
      ''
      
        'function $(MAPPERNAME).GetColumnBDEFieldType(ColumnIndex: Intege' +
        'r): TFieldType;'
      'begin'
      '  case ColumnIndex of'
      '$(LOOPFIELDCOUNT)\'
      '    $(FIELDCOUNT): Result := $(FIELDBDETYPE);'
      '$(ENDLOOPFIELDCOUNT)\'
      '    else Result := ftInteger;'
      '  end;'
      'end;'
      ''
      'function $(MAPPERNAME).GetColumnCount: Integer;'
      'begin'
      '  Result := $(FIELDCOUNT);'
      'end;'
      ''
      
        'function $(MAPPERNAME).GetColumnSize(ColumnIndex: Integer): Inte' +
        'ger;'
      'begin'
      '  case ColumnIndex of'
      '$(LOOPFIELDCOUNT)\'
      
        '    $(FIELDCOUNT): Result := 0; //Change if this column stores a' +
        ' string!'
      '$(ENDLOOPFIELDCOUNT)\'
      '    else Result := inherited GetColumnSize(ColumnIndex);'
      '  end;'
      'end;'
      ''
      
        'function $(MAPPERNAME).GetInitialColumnName(ColumnIndex: Integer' +
        '): string;'
      'begin'
      '  // the column name should include the ColumnRootName'
      '  // in case there are several attributes using the same table'
      '  result := inherited GetInitialColumnName(ColumnIndex);'
      'end;'
      ''
      ''
      'procedure $(MAPPERNAME).CreatePersistentStorage;'
      'begin'
      '  // this method is called du'
      '  inherited;'
      'end;'
      ''
      
        'procedure $(MAPPERNAME).InitializeSystem(theDatabase: IBoldDataB' +
        'ase);'
      'begin'
      
        '  // this function is called when the entire database has been c' +
        'reated (all tables/columns are in place).'
      '  // Here you may initialize counters for example'
      '  inherited;'
      'end;'
      ''
      'initialization'
      
        '  BoldMemberPersistenceMappers.AddDescriptor($(MAPPERNAME), alCo' +
        'ncrete);'
      ''
      'finalization'
      '  if BoldMemberPersistenceMappersAssigned then'
      
        '    BoldMemberPersistenceMappers.RemoveDescriptorByClass($(MAPPE' +
        'RNAME));'
      'end.')
    Left = 236
    Top = 164
  end
  object AttributeTemplate: TBoldTemplateHolder
    Template.Strings = (
      '$(MACROSTARTProtectedMethods)\'
      '  protected'
      '    {protected declarations}'
      '$(LOOPFIELDCOUNT)\'
      '$(CASEFIELDREADABLE)\'
      '1|    function GetAs$(FIELDNAME): $(FIELDTYPE);'
      '$(ENDCASEFIELDREADABLE)\'
      '$(CASEFIELDWRITABLE)\'
      '1|    procedure SetAs$(FIELDNAME)(Value: $(FIELDTYPE));'
      
        '1|    function MaySet$(FIELDNAME)(NewValue: $(FIELDTYPE); Subscr' +
        'iber: TBoldSubscriber): Boolean; virtual;'
      '$(ENDCASEFIELDWRITABLE)\'
      '$(ENDLOOPFIELDCOUNT)\'
      '$(CASEGetStringRepresentation)\'
      
        'true|    function GetStringRepresentation(Representation: TBoldR' +
        'epresentation): string; override;'
      '$(ENDCASEGetStringRepresentation)\'
      '$(CASESetStringRepresentation)\'
      
        'true|    procedure SetStringRepresentation(Representation: TBold' +
        'Representation; Value: string); override;'
      '$(ENDCASESetStringRepresentation)\'
      '\'
      '$(CASEINTERFACEDCLASS)\'
      '$(ENDCASEINTERFACEDCLASS)\'
      '\'
      '$(LOOPPROTECTEDMETHODCOUNT)\'
      
        '    $(PROTECTEDMETHODTYPE) $(PROTECTEDMETHODNAME) $(PROTECTEDMET' +
        'HODSIGNATURE) $(PROTECTEDMETHODDIRECTIVES)\'
      '    '
      '$(ENDLOOPPROTECTEDMETHODCOUNT)\'
      '\'
      '$(CASEINTERFACEDCLASS)\'
      '1|    function ProxyClass: TBoldMember_ProxyClass; override;'
      '$(ENDCASEINTERFACEDCLASS)\'
      '\'
      '$(MACROEND ProtectedMethods)\'
      '\'
      '$(MACROSTARTPrivateMethods)\'
      '  private'
      '    {private declarations}'
      '$(LOOPFIELDCOUNT)\'
      '    f$(FIELDNAME): $(FIELDTYPE);'
      '$(ENDLOOPFIELDCOUNT)\'
      '$(LOOPFIELDCOUNT)\'
      '$(CASEFIELDREADABLE)\'
      '1|    function Get$(FIELDNAME)Content: $(FIELDTYPE);'
      '$(ENDCASEFIELDREADABLE)\'
      '$(CASEFIELDWRITABLE)\'
      '1|    procedure SetDataValue$(FIELDNAME)(Value: $(FIELDTYPE));'
      '1|    procedure Set$(FIELDNAME)Content(NewValue: $(FIELDTYPE));'
      '$(ENDCASEFIELDWRITABLE)\'
      '$(ENDLOOPFIELDCOUNT)\'
      '$(LOOPPRIVATEMETHODCOUNT)\'
      
        '    $(PRIVATEMETHODTYPE) $(PRIVATEMETHODNAME) $(PRIVATEMETHODSIG' +
        'NATURE) $(PRIVATEMETHODDIRECTIVES)\'
      '$(ENDLOOPPRIVATEMETHODCOUNT)\'
      '$(MACROEND PrivateMethods)\'
      '\'
      '$(MACROSTARTPublicMethods)\'
      '  public'
      '    {public declarations}'
      '$(CASEINTERFACEDCLASS)\'
      '1|$(CASESetAsVariant)\'
      
        '1|true|    procedure SetAsVariant(const Value: Variant); overrid' +
        'e;'
      '1|$(ENDCASESetAsVariant)\'
      '1|$(CASEGetAsVariant)\'
      '1|true|    function GetAsVariant: Variant; override;'
      '1|$(ENDCASEGetAsVariant)\'
      '1|$(CASEAssign)\'
      '1|true|    procedure Assign(Source: TBoldElement); override;'
      '1|$(ENDCASEAssign)\'
      '1|$(CASEAssignValue)\'
      '1|true|    procedure AssignValue(Source: IBoldValue); override;'
      '1|$(ENDCASEAssignValue)\'
      '1|$(CASEAssignValue)\'
      
        '1|true|    procedure AssignContentValue(Source: IBoldValue); ove' +
        'rride;'
      '1|$(ENDCASEAssignValue)\'
      '$(ENDCASEINTERFACEDCLASS)\'
      '$(CASEValidateString)\'
      
        'true|    function ValidateString(Value: string; Representation: ' +
        'TBoldRepresentation): Boolean; override;'
      '$(ENDCASEValidateString)\'
      '$(CASEValidateCharacter)\'
      
        'true|    function ValidateCharacter(C: AnsiChar; Representation:' +
        ' TBoldRepresentation): Boolean; override;'
      '$(ENDCASEValidateCharacter)\'
      '$(LOOPPUBLICMETHODCOUNT)\'
      
        '    $(PUBLICMETHODTYPE) $(PUBLICMETHODNAME) $(PUBLICMETHODSIGNAT' +
        'URE)  $(PUBLICMETHODDIRECTIVES)'
      '$(ENDLOOPPUBLICMETHODCOUNT)\'
      '$(LOOPFIELDCOUNT)\'
      '$(CASEFIELDWRITABLE)\'
      
        '1|    function CanSet$(FIELDNAME)(Value: $(FIELDTYPE); Subscribe' +
        'r: TBoldSubscriber): Boolean;'
      '$(ENDCASEFIELDWRITABLE)\'
      '    property As$(FIELDNAME): $(FIELDTYPE) \'
      '$(CASEFIELDREADABLE)\'
      '1|read GetAs$(FIELDNAME) \'
      '$(ENDCASEFIELDREADABLE)\'
      '$(CASEFIELDWRITABLE)\'
      '1|write SetAs$(FIELDNAME)\'
      '$(ENDCASEFIELDWRITABLE)\'
      ';'
      '$(ENDLOOPFIELDCOUNT)\'
      '$(CASEINTERFACEDCLASS)\'
      
        '1|    function ProxyInterface(const IId: TGUID; Mode: TBoldDomai' +
        'nElementProxyMode; out Obj): Boolean; override;'
      '$(ENDCASEINTERFACEDCLASS)\'
      '\'
      '$(MACROEND PublicMethods)\'
      '\'
      '$(MACROSTARTPublishedMethods)\'
      '  published'
      '    {published declarations}'
      '$(LOOPPUBLISHEDMETHODCOUNT)\'
      
        '    $(PUBLISHEDMETHODTYPE) $(PUBLISHEDMETHODNAME) $(PUBLISHEDMET' +
        'HODSIGNATURE) $(PUBLISHEDMETHODDIRECTIVES)\'
      '$(ENDLOOPPUBLISHEDMETHODCOUNT)\'
      '$(MACROEND PublishedMethods)\'
      '\'
      '$(MACROSTARTInterfacedClassMethods)\'
      '$(CASESetAsVariant)\'
      'true|procedure $(DELPHINAME).SetAsVariant(const Value: Variant);'
      'true|begin'
      'true|  AsString := Value; // your code here'
      'true|end;'
      'true|'
      '$(ENDCASESetAsVariant)\'
      '$(CASEGetAsVariant)\'
      'true|function $(DELPHINAME).GetAsVariant: Variant;'
      'true|begin'
      'true|  result := AsString; // your code here'
      'true|end;'
      'true|'
      '$(ENDCASEGetAsVariant)\'
      ''
      '$(CASEAssignValue)\'
      'true|procedure $(DELPHINAME).AssignValue(Source: IBoldValue);'
      'true|var'
      'true|  s: $(INTERFACENAME);'
      'true|begin'
      'true|  if source.QueryInterface($(INTERFACENAME), S) = S_OK then'
      'true|  begin'
      'true|$(LOOPFIELDCOUNT)\'
      'true|$(CASEFIELDWRITABLE)\'
      'true|1|    SetDataValue$(FIELDNAME)(s.$(FIELDNAME));'
      'true|$(ENDCASEFIELDWRITABLE)\'
      'true|$(ENDLOOPFIELDCOUNT)\'
      'true|  end'
      'true|  else'
      
        'true|    raise EBold.CreateFmt('#39'%s.AssignValue: unknown type of ' +
        'source'#39', [classname]);'
      'true|end;'
      'true|'
      '$(ENDCASEAssignValue)\'
      '$(CASEAssignContentValue)\'
      
        'true|procedure $(DELPHINAME).AssignContentValue(Source: IBoldVal' +
        'ue);'
      'true|var'
      'true|  s: $(INTERFACENAME);'
      'true|begin'
      'true|  if source.QueryInterface($(INTERFACENAME), S) = S_OK then'
      'true|  begin'
      'true|    if s.IsNull then'
      'true|      SetContentToNull'
      'true|    else'
      'true|    begin'
      'true|$(LOOPFIELDCOUNT)\'
      'true|$(CASEFIELDWRITABLE)\'
      'true|1|      Set$(FIELDNAME)Content(s.$(FIELDNAME));'
      'true|$(ENDCASEFIELDWRITABLE)\'
      'true|$(ENDLOOPFIELDCOUNT)\'
      'true|    end'
      'true|  end'
      'true|  else'
      
        'true|    raise EBold.CreateFmt('#39'%s.AssignValue: unknown type of ' +
        'source'#39', [classname]);'
      'true|end;'
      'true|'
      '$(ENDCASEAssignContentValue)\'
      '$(CASEAssign)\'
      'true|procedure $(DELPHINAME).Assign(Source: TBoldElement);'
      'true|begin'
      'true|  if Source is $(DELPHINAME) then'
      'true|  begin'
      'true|    if $(DELPHINAME)(Source).IsNull then'
      'true|      SetToNull'
      'true|    else'
      'true|    begin'
      'true|$(LOOPFIELDCOUNT)\'
      'true|$(CASEFIELDWRITABLE)\'
      
        'true|1|      As$(FIELDNAME) := $(DELPHINAME)(Source).As$(FIELDNA' +
        'ME);'
      'true|$(ENDCASEFIELDWRITABLE)\'
      'true|$(ENDLOOPFIELDCOUNT)\'
      'true|    end'
      'true|  end'
      'true|  else'
      'true|    AssignError(Source);'
      'true|end;'
      'true|'
      '$(ENDCASEAssign)\'
      '\'
      'function $(DELPHINAME).ProxyClass: TBoldMember_ProxyClass;'
      'begin'
      '  result := $(DELPHINAME)_Proxy;'
      'end;'
      ''
      
        'function $(DELPHINAME).ProxyInterface(const IId: TGUID; Mode: TB' +
        'oldDomainElementProxyMode; out Obj): Boolean;'
      'begin'
      '  if IsEqualGuid(IID, $(INTERFACENAME)) then'
      '  begin'
      
        '    result := ProxyClass.create(self, Mode).GetInterface(IID, ob' +
        'j);'
      '    if not result then'
      
        '      raise EBoldInternal.Create( '#39'ProxyClass for %s did not imp' +
        'lement $(INTERFACENAME)'#39');'
      '  end else'
      '    result := inherited ProxyInterface(IID, Mode, Obj);'
      'end;'
      ''
      '$(MACROEND InterfacedClassMethods)\'
      '\'
      '$(MACROSTARTProxyClass)\'
      
        '  $(DELPHINAME)_Proxy = class(TBoldAttribute_Proxy, $(INTERFACEN' +
        'AME))'
      '  private'
      '    function GetProxed$(EXPRESSIONNAME): $(DELPHINAME);'
      '  protected'
      
        '    property Proxed$(EXPRESSIONNAME): $(DELPHINAME) read GetProx' +
        'ed$(EXPRESSIONNAME) implements $(INTERFACENAME);'
      '  end;'
      '$(MACROEND ProxyClass)\'
      '{*******************************}'
      '{   This unit was created by    }'
      '{ the BoldSoft Attribute Wizard }'
      '{       $(DATETIME)             }'
      '{*******************************}'
      ''
      'unit $(UNITNAME);'
      ''
      'interface'
      ''
      'uses'
      '  BoldDefs,'
      '  ActiveX,'
      '  BoldMemberTypeDictionary,'
      '  BoldSubscription,'
      '  BoldSystem,'
      '  BoldStreams,'
      '  BoldAttributes,'
      '  BoldElements,'
      '  BoldDomainElement,'
      '$(CASEINTERFACEDCLASS)\'
      '1|  BoldValueInterfaces,'
      '1|  $(INTERFACEUNITNAME);'
      '$(ENDCASEINTERFACEDCLASS)\'
      '\'
      '$(CASEINTERFACEDCLASS)\'
      '0|  BoldValueInterfaces;'
      '$(ENDCASEINTERFACEDCLASS)\'
      ''
      
        '// To install this attribute type in your application, add the f' +
        'ollowing to your'
      '// TypeNameHandle:'
      '//'
      '// ModelName:      $(EXPRESSIONNAME)'
      '// ExpressionName: $(EXPRESSIONNAME)'
      '// DelphiName:     $(DELPHINAME)'
      '// ContentName:    $(EXPRESSIONNAME)'
      '// PMapper:        $(MAPPERNAME)'
      '// Accessor:'
      '// NativeType:'
      
        '// UnitName:       $(UNITNAME)    // This unit'#39's name, modify if' +
        ' necessary'
      ''
      'type'
      '  $(DELPHINAME) = class($(SUPERCLASS))'
      '$(CASEPRIVATE)\'
      'true|$(PRIVATEMETHODS)\'
      '$(ENDCASEPRIVATE)\'
      '\'
      '$(CASEPROTECTED)\'
      'true|$(PROTECTEDMETHODS)\'
      '$(ENDCASEPROTECTED)\'
      '\'
      '$(CASEPUBLIC)\'
      'true|$(PublicMethods)\'
      '$(ENDCASEPUBLIC)\'
      '\'
      '$(CASEPUBLISHED)\'
      'true|$(PublishedMethods)'
      '$(ENDCASEPUBLISHED)\'
      '  end;'
      '$(CASEINTERFACEDCLASS)\'
      '1|'
      '1|  {$(DELPHINAME)_Proxy}'
      '1|$(ProxyClass)\'
      '$(ENDCASEINTERFACEDCLASS)\'
      ''
      'implementation'
      ''
      'uses'
      '  windows;'
      ''
      '$(CASEGetStringRepresentation)\'
      
        'true|function $(DELPHINAME).GetStringRepresentation(Representati' +
        'on: TBoldRepresentation): string;'
      'true|begin'
      'true|  case representation of'
      'true|    brDefault: result := '#39#39'; // your code here'
      
        'true|    else result := inherited GetStringRepresentation( Repre' +
        'sentation );'
      'true|  end;'
      'true|end;'
      'true|'
      '$(ENDCASEGetStringRepresentation)\'
      '$(CASESetStringRepresentation)\'
      
        'true|procedure $(DELPHINAME).SetStringRepresentation(Representat' +
        'ion: TBoldRepresentation; Value: string);'
      'true|begin'
      'true|  case representation of'
      'true|    brDefault: ; // your code here'
      
        'true|    else inherited SetStringRepresentation( Representation,' +
        ' value );'
      'true|  end;'
      'true|end;'
      'true|'
      '$(ENDCASESetStringRepresentation)\'
      '$(CASEValidateString)\'
      
        'true|function $(DELPHINAME).ValidateString(Value: string; Repres' +
        'entation: TBoldRepresentation): Boolean;'
      'true|begin'
      'true|  case representation of'
      'true|    brDefault: result := true; // your code here'
      
        'true|    else result := inherited ValidateString( value, Represe' +
        'ntation );'
      'true|  end;'
      'true|end;'
      'true|'
      '$(ENDCASEValidateString)\'
      '$(CASEValidateCharacter)\'
      
        'true|function $(DELPHINAME).ValidateCharacter(C: AnsiChar; Repre' +
        'sentation: TBoldRepresentation): Boolean;'
      'true|begin'
      'true|  case representation of'
      'true|    brDefault: result := true; // your code here'
      
        'true|    else result := inherited ValidateCharacter( c, Represen' +
        'tation );'
      'true|  end;'
      'true|end;'
      'true|'
      '$(ENDCASEValidateCharacter)\'
      '\'
      '$(LOOPFIELDCOUNT)\'
      '$(CASEFIELDREADABLE)\'
      '1|function $(DELPHINAME).GetAs$(FIELDNAME): $(FIELDTYPE);'
      '1|begin'
      '1|  BoldClearLastFailure;'
      '1|  if not canRead(nil) then'
      '1|    BoldRaiseLastFailure( self, '#39'GetAs$(FIELDNAME)'#39', '#39#39');'
      '1|  EnsureNotNull; {ensures current}'
      '1|  Result := f$(FIELDNAME);'
      '1|end;'
      '1|'
      '1|function $(DELPHINAME).Get$(FIELDNAME)Content: $(FIELDTYPE);'
      '1|begin'
      '1|  Result := f$(FIELDNAME);'
      '1|end;'
      '1|'
      '$(ENDCASEFIELDREADABLE)\'
      '\'
      '$(CASEFIELDWRITABLE)\'
      
        '1|procedure $(DELPHINAME).SetAs$(FIELDNAME)(Value: $(FIELDTYPE))' +
        ';'
      '1|begin'
      '1|  SetDataValue$(FIELDNAME)(Value);'
      '1|end;'
      '1|'
      
        '1|function $(DELPHINAME).MaySet$(FIELDNAME)(NewValue: $(FIELDTYP' +
        'E); Subscriber: TBoldSubscriber): Boolean;'
      '1|begin'
      '1|  result := true;'
      '1|end;'
      '1|'
      
        '1|procedure $(DELPHINAME).SetDataValue$(FIELDNAME)(Value: $(FIEL' +
        'DTYPE));'
      '1|begin'
      '1|  BoldClearLastFailure;'
      '1|  if not CanSet$(FIELDNAME)(Value, nil) then'
      
        '1|    BoldRaiseLastFailure( self, '#39'SetDataValue$(FIELDNAME)'#39', '#39#39 +
        ');'
      '1|'
      '1|  if IsNull or (f$(FIELDNAME) <> Value) then'
      '1|  begin'
      '1|    if not StartModify then'
      
        '1|      BoldRaiseLastFailure(self, '#39'SetDataValue$(FIELDNAME)'#39', '#39 +
        #39');'
      '1|    try'
      '1|      Set$(FIELDNAME)Content(Value);'
      '1|      EndModify;'
      '1|    except'
      '1|      FailModify;'
      '1|      raise;'
      '1|    end;'
      '1|  end;'
      '1|end;'
      '1|'
      
        '1|procedure $(DELPHINAME).Set$(FIELDNAME)Content(NewValue: $(FIE' +
        'LDTYPE));'
      '1|begin'
      '1|  if (BoldPersistenceState = bvpsInvalid) or'
      '1|    ContentIsNull or (F$(FIELDNAME) <> NewValue) then'
      '1|  begin'
      '1|    PreChange;'
      '1|    F$(FIELDNAME) := NewValue;'
      '1|    SetToNonNull;'
      '1|    Changed(beValueChanged, [NewValue]);'
      '1|  end;'
      '1|end;'
      '1|'
      
        '1|function $(DELPHINAME).CanSet$(FIELDNAME)(Value: $(FIELDTYPE);' +
        ' Subscriber: TBoldSubscriber): Boolean;'
      '1|begin'
      '1|  result := MaySet$(FIELDNAME)(Value, Subscriber) and'
      '1|            SendQuery(bqMaySetValue, [Value], Subscriber);'
      '1|end;'
      '1|'
      '$(ENDCASEFIELDWRITABLE)\'
      '$(ENDLOOPFIELDCOUNT)\'
      '\'
      '$(CASEINTERFACEDCLASS)\'
      '1|$(InterfacedClassMethods)'
      '$(ENDCASEINTERFACEDCLASS)\'
      '$(LOOPPROTECTEDMETHODCOUNT)\'
      
        '$(PROTECTEDMETHODTYPE) $(DELPHINAME).$(PROTECTEDMETHODNAME)$(PRO' +
        'TECTEDMETHODSIGNATURE)'
      'begin'
      '  // your code here'
      'end;'
      ''
      '$(ENDLOOPPROTECTEDMETHODCOUNT)\'
      '\'
      '$(LOOPPRIVATEMETHODCOUNT)\'
      
        '$(PRIVATEMETHODTYPE) $(DELPHINAME).$(PRIVATEMETHODNAME)$(PRIVATE' +
        'METHODSIGNATURE)'
      'begin'
      '  // your code here'
      'end;'
      ''
      '$(ENDLOOPPRIVATEMETHODCOUNT)\'
      '\'
      '$(LOOPPUBLICMETHODCOUNT)\'
      
        '$(PUBLICMETHODTYPE) $(DELPHINAME).$(PUBLICMETHODNAME)$(PUBLICMET' +
        'HODSIGNATURE)'
      'begin'
      '  // your code here'
      'end;'
      ''
      '$(ENDLOOPPUBLICMETHODCOUNT)\'
      '$(CASEINTERFACEDCLASS)\'
      '  { $(DELPHINAME)_Proxy }'
      
        '1|function $(DELPHINAME)_Proxy.GetProxed$(EXPRESSIONNAME): $(DEL' +
        'PHINAME);'
      '1|begin'
      '1|  result := ProxedElement as $(DELPHINAME);'
      ''
      '1|end;'
      '$(ENDCASEINTERFACEDCLASS)\'
      ''
      'initialization'
      
        '  BoldmemberTypes.AddMemberTypeDescriptor($(DELPHINAME), alConcr' +
        'ete);'
      ''
      'finalization'
      '  if BoldMemberTypesAssigned then'
      '    BoldMemberTypes.RemoveDescriptorByClass($(DELPHINAME));'
      'end.')
    Left = 236
    Top = 100
  end
  object InterfaceTemplate: TBoldTemplateHolder
    Template.Strings = (
      '{*******************************}'
      '{   This unit was created by    }'
      '{ the BoldSoft Attribute Wizard }'
      '{      $(DATETIME)      }'
      '{*******************************}'
      ''
      'unit $(UNITNAME);'
      ''
      'interface'
      ''
      'uses'
      '  Windows, // Required for S_OK'
      '  BoldDefs, // Required for EBold exception'
      '  BoldStreams,'
      '  BoldFreeStandingValues,'
      '  BoldFreeStandingValueFactories,'
      '  BoldValueInterfaces;'
      ''
      'type'
      '  $(INTERFACENAME) = interface($(SUPERINTERFACENAME))'
      '  ['#39'$(INTERFACEGUID)'#39']'
      '$(LOOPFIELDCOUNT)\'
      '$(CASEFIELDREADABLE)\'
      '1|    function Get$(FIELDNAME)Content: $(FIELDTYPE);'
      '$(ENDCASEFIELDREADABLE)\'
      '$(CASEFIELDWRITABLE)\'
      '1|    procedure Set$(FIELDNAME)Content(Value: $(FIELDTYPE));'
      '$(ENDCASEFIELDWRITABLE)\'
      '$(ENDLOOPFIELDCOUNT)\'
      '$(LOOPFIELDCOUNT)\'
      '    property $(FIELDNAME): $(FIELDTYPE) \'
      '$(CASEFIELDREADABLE)\'
      '1|read Get$(FIELDNAME)Content \'
      '$(ENDCASEFIELDREADABLE)\'
      '$(CASEFIELDWRITABLE)\'
      '1|write Set$(FIELDNAME)Content\'
      '$(ENDCASEFIELDWRITABLE)\'
      ';'
      '\'
      '$(ENDLOOPFIELDCOUNT)\'
      '  end;'
      ''
      
        '  $(FREESTANDINGDELPHINAME) = class($(FREESTANDINGSUPERCLASS), $' +
        '(INTERFACENAME))'
      '  private'
      '$(LOOPFIELDCOUNT)\'
      '    f$(FIELDNAME) : $(FIELDTYPE);'
      '$(ENDLOOPFIELDCOUNT)\'
      '$(LOOPFIELDCOUNT)\'
      '$(CASEFIELDREADABLE)\'
      '1|    function Get$(FIELDNAME)Content: $(FIELDTYPE);'
      '$(ENDCASEFIELDREADABLE)\'
      '$(CASEFIELDWRITABLE)\'
      '1|    procedure Set$(FIELDNAME)Content(NewValue : $(FIELDTYPE));'
      '$(ENDCASEFIELDWRITABLE)\'
      '$(ENDLOOPFIELDCOUNT)\'
      '  protected'
      '    function GetStreamName: String; override;'
      '    procedure AssignContentValue(Source: IBoldValue); override;'
      '  public'
      '$(LOOPFIELDCOUNT)\'
      '    property $(FIELDNAME): $(FIELDTYPE) \'
      '$(CASEFIELDREADABLE)\'
      '1|read Get$(FIELDNAME)Content \'
      '$(ENDCASEFIELDREADABLE)\'
      '$(CASEFIELDWRITABLE)\'
      '1|write Set$(FIELDNAME)Content\'
      '$(ENDCASEFIELDWRITABLE)\'
      ';'
      '\'
      '$(ENDLOOPFIELDCOUNT)\'
      '  end;'
      ''
      'const'
      '  ContentName_$(EXPRESSIONNAME) = '#39'$(EXPRESSIONNAME)'#39';'
      ''
      'implementation'
      ''
      '{ $(FREESTANDINGDELPHINAME) }'
      ''
      'function $(FREESTANDINGDELPHINAME).GetStreamName: String;'
      'begin'
      '  result := ContentName_$(EXPRESSIONNAME);'
      'end;'
      ''
      
        'procedure $(FREESTANDINGDELPHINAME).AssignContentValue(Source: I' +
        'BoldValue);'
      'var'
      '  s: $(INTERFACENAME);'
      'begin'
      '  if source.QueryInterface($(INTERFACENAME), S) = S_OK then'
      '    if s.IsNull then'
      '      SetContentToNull'
      '    else'
      '    begin'
      '$(LOOPFIELDCOUNT)\'
      '$(CASEFIELDWRITABLE)\'
      '1|      $(FIELDNAME) := s.$(FIELDNAME);'
      '$(ENDCASEFIELDWRITABLE)\'
      '$(ENDLOOPFIELDCOUNT)\'
      '    end'
      '  else'
      
        '    raise EBold.CreateFmt('#39'%s.AssignContentValue: unknown type o' +
        'f source'#39', [classname]);'
      'end;'
      ''
      '$(LOOPFIELDCOUNT)\'
      '\'
      '$(CASEFIELDREADABLE)\'
      
        '1|function $(FREESTANDINGDELPHINAME).Get$(FIELDNAME)Content: $(F' +
        'IELDTYPE);'
      '1|begin'
      '1|  result := f$(FIELDNAME);'
      '1|end;'
      '1|'
      '$(ENDCASEFIELDREADABLE)\'
      '\'
      '$(CASEFIELDWRITABLE)\'
      
        '1|procedure $(FREESTANDINGDELPHINAME).Set$(FIELDNAME)Content(New' +
        'Value: $(FIELDTYPE));'
      '1|begin'
      '1|  f$(FIELDNAME) := NewValue;'
      '1|end;'
      '1|'
      '$(ENDCASEFIELDWRITABLE)\'
      '$(ENDLOOPFIELDCOUNT)\'
      'initialization'
      '  With FreeStandingValueFactory do'
      
        '    RegisterFreeStandingClass(ContentName_$(EXPRESSIONNAME), $(F' +
        'REESTANDINGDELPHINAME));'
      'end.')
    Left = 236
    Top = 36
  end
end
