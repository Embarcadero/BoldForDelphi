{ Global compiler directives }
{$include bold.inc}
unit BoldSystemRT;

interface

uses
  BoldDefs,
  BoldElements,
  BoldIndexableList,
  BoldMetaElementList,
  BoldGeneratedCodeDictionary,
  BoldMeta,
  BoldTypeNameDictionary,
  BoldUMLTypes,
  BoldTaggedValueSupport,
  Classes;

const
  LITE_VERSION_CLASS_LIMIT = 15;
  CLASS_TYPE_INFO_MEM_SIZE = 24;

type
  TBoldTypeInfoSearchOption = (soPartialMatch);
  TBoldTypeInfoSearchOptions = set of TBoldTypeInfoSearchOption;

  TBoldSearchType = (stClass, stAttribute, stRole, stMethod, stType);
  TBoldSearchTypes = set of TBoldSearchType;

const cDefaultSearchTypes = [stClass, stAttribute, stRole, stMethod, stType];
const cDefaultSearchTypeOptions = [soPartialMatch];

type
  {forward declarations, classes in actual model}
  TBoldClassTypeInfoList = class;
  TBoldAttributeTypeInfoList = class;
  TBoldMemberRTInfoList = class;
  TBoldRoleRTInfoList = class;  
  TBoldMethodRTInfoList = class;

  TBoldListTypeInfoList = class;

  TBoldTypeTypeInfo = class;

  TBoldSystemTypeInfo = class;

  TBoldClassTypeInfo = class;
  TBoldNilTypeInfo = class;

  TBoldMemberRTInfo = class;
  TBoldRoleRTInfo = class;
//  TBoldListTypeInfo = class; // moved to BoldElements
  TBoldAttributeRTInfo = class;
  TBoldAttributeTypeInfo = class;
  TBoldMethodRTInfo = class;
  TBoldElementTypeInfoWithConstraint = class;
  TBoldConstraintRTInfo = class;

  { TBoldRTEvaluator }
  TBoldRTEvaluator = class(TBoldEvaluator)
  public
    function RTInfo(const Ocl: string; Context: TBoldElementTypeInfo; ReRaise: Boolean; const VariableList: TBoldExternalVariableList = nil): TBoldMemberRTInfo; virtual; abstract;
  end;

  TBoldClassTypeInfoListTraverser = class(TBoldIndexableListTraverser)
  public
    function GetCurrent: TBoldClassTypeInfo;
    property Current: TBoldClassTypeInfo read GetCurrent;
  end;

  {---TBoldClassRTInfoList---}
  TBoldClassTypeInfoList = class(TBoldElementTypeInfoList)
  private
    class var IX_ObjectClass: integer;
    function GetItem(index: Integer): TBoldClassTypeInfo;
    function GetItemByExpressionName(const ExpressionName: string): TBoldClassTypeInfo;
    function GetItemByModelName(const ModelName: string): TBoldClassTypeInfo;
    function GetItemByObjectClass(ObjectClass: TClass): TBoldClassTypeInfo;
  protected
    function TraverserClass: TBoldIndexableListTraverserClass; override;
  public
    constructor Create;
    function GetEnumerator: TBoldClassTypeInfoListTraverser;
    property Items[index: Integer]: TBoldClassTypeInfo read GetItem; default;
    property ItemsByExpressionName[const ExpressionName: string]: TBoldClassTypeInfo read GetItemByExpressionName;
    property ItemsByModelName[const ModelName: string]: TBoldClassTypeInfo read GetItemByModelName;
    property ItemsByObjectClass[ObjectClass: TClass]: TBoldClassTypeInfo read GetItemByObjectClass;
  end;

  TBoldMemberRTInfoListTraverser = class(TBoldIndexableListTraverser)
  public
    function GetCurrent: TBoldMemberRTInfo;
    property Current: TBoldMemberRTInfo read GetCurrent;
  end;

  {---TBoldMemberRTInfoList---}
  TBoldMemberRTInfoList = class(TBoldMetaElementList)
  private
    function GetItem(index: Integer): TBoldMemberRTInfo;
    function GetItemByExpressionName(const ExpressionName: string): TBoldMemberRTInfo;
    function GetItemByModelName(const ModelName: string): TBoldMemberRTInfo;
  protected
    function TraverserClass: TBoldIndexableListTraverserClass; override;
  public
    function GetEnumerator: TBoldMemberRTInfoListTraverser;
    property Items[index: Integer]: TBoldMemberRTInfo read GetItem; default;
    property ItemsByExpressionName[const ExpressionName: string]: TBoldMemberRTInfo read GetItemByExpressionName;
    property ItemsByModelName[const ModelName: string]: TBoldMemberRTInfo read GetItemByModelName;
  end;

  TBoldAttributeTypeInfoListTraverser = class(TBoldIndexableListTraverser)
  public
    function GetCurrent: TBoldAttributeTypeInfo;
    property Current: TBoldAttributeTypeInfo read GetCurrent;
  end;

  TBoldAttributeTypeInfoList = class(TBoldMetaElementList)
  private
    function GetItem(index: Integer): TBoldAttributeTypeInfo;
    function GetItemByExpressionName(const ExpressionName: string): TBoldAttributeTypeInfo;
    function GetItemByModelName(const ModelName: string): TBoldAttributeTypeInfo;
  protected
    function TraverserClass: TBoldIndexableListTraverserClass; override;
  public
    function GetEnumerator: TBoldAttributeTypeInfoListTraverser;
    property Items[index: Integer]: TBoldAttributeTypeInfo read GetItem; default;
    property ItemsByExpressionName[const ExpressionName: string]: TBoldAttributeTypeInfo read GetItemByExpressionName;
    property ItemsByModelName[const ModelName: string]: TBoldAttributeTypeInfo read GetItemByModelName;
  end;

  TBoldRoleRTInfoListTraverser = class(TBoldIndexableListTraverser)
  public
    function GetCurrent: TBoldRoleRTInfo;
    property Current: TBoldRoleRTInfo read GetCurrent;
  end;

  {---TBoldRoleRTInfoList---}
  TBoldRoleRTInfoList = class(TBoldMetaElementList)
  private
    function GetItem(index: Integer): TBoldRoleRTInfo;
    function GetItemByExpressionName(const ExpressionName: string): TBoldRoleRTInfo;
    function GetItemByModelName(const ModelName: string): TBoldRoleRTInfo;
  protected
    function TraverserClass: TBoldIndexableListTraverserClass; override;
  public
    function GetEnumerator: TBoldRoleRTInfoListTraverser;
    property Items[index: Integer]: TBoldRoleRTInfo read GetItem; default;
    property ItemsByExpressionName[const ExpressionName: string]: TBoldRoleRTInfo read GetItemByExpressionName;
    property ItemsByModelName[const ModelName: string]: TBoldRoleRTInfo read GetItemByModelName;
  end;

  TBoldConstraintRTInfoListTraverser = class(TBoldIndexableListTraverser)
  public
    function GetCurrent: TBoldConstraintRTInfo;
    property Current: TBoldConstraintRTInfo read GetCurrent;
  end;

  TBoldConstraintRTInfoList = class(TBoldMetaElementList)
  private
    function GetItem(index: Integer): TBoldConstraintRTInfo;
    function GetItemByModelName(const ModelName: string): TBoldConstraintRTInfo;
  protected
    function TraverserClass: TBoldIndexableListTraverserClass; override;
  public
    function GetEnumerator: TBoldConstraintRTInfoListTraverser;
    property Items[index: Integer]: TBoldConstraintRTInfo read GetItem; default;
    property ItemsByModelName[const ModelName: string]: TBoldConstraintRTInfo read GetItemByModelName;
  end;

  TBoldMethodRTInfoListTraverser = class(TBoldIndexableListTraverser)
  public
    function GetCurrent: TBoldMethodRTInfo;
    property Current: TBoldMethodRTInfo read GetCurrent;
  end;

  {---TBoldMethodRTInfoList---}
  TBoldMethodRTInfoList = class(TBoldMetaElementList)
  private
    function GetItem(index: Integer): TBoldMethodRTInfo;
    function GetItemByModelName(const ModelName: string): TBoldMethodRTInfo;
    function GetItemByExpressionName(const ExpressionName: string): TBoldMethodRTInfo;
  protected
    function TraverserClass: TBoldIndexableListTraverserClass; override;
  public
    function GetEnumerator: TBoldMethodRTInfoListTraverser;
    property Items[index: Integer]: TBoldMethodRTInfo read GetItem; default;
    property ItemsByModelName[const ModelName: string]: TBoldMethodRTInfo read GetItemByModelName;
    property ItemsByExpressionName[const ExpressionName: string]: TBoldMethodRTInfo read GetItemByExpressionName;
  end;

  {---TBoldListTypeInfoList---}
  TBoldListTypeInfoList = class(TBoldElementTypeInfoList)
  private
    class var IX_Element: integer;
    class var IX_ListClass: integer;
    function GetItemByElement(Element: TBoldElementTypeInfo): TBoldListTypeInfo;
    function GetItemByListClass(ListClass: TClass): TBoldListTypeInfo;
  public
    constructor Create;
    property ItemByElement[Element: TBoldElementTypeInfo]: TBoldListTypeInfo read GetItemByElement;
    property ItemByListClass[ObjectClass: TClass]: TBoldListTypeInfo read GetItemByListClass;
  end;

  {---TBoldTypeTypeInfo---}
  TBoldTypeTypeInfo = class(TBoldElementTypeInfo)
  protected
    constructor Create(const ModelName: string; const ExpressionName: string; const Delphiname: string; ModelTypeInfo: TBoldElementTypeInfo);
    function GetBoldType: TBoldElementTypeInfo; override;
    function GetListTypeInfo: TBoldListTypeInfo; override;
  public
    function ConformsTo(CompareElement: TBoldElementTypeInfo): Boolean; override;
  end;

  TBoldElementTypeInfoWithConstraint = class(TBoldElementTypeInfo)
  private
    fConstraints: TBoldConstraintRTInfoList;
    fTaggedValues: TStrings;
    function GetTaggedValues(const Tag: string): string;
    function GetConstraints(const Name: String): TBoldConstraintRTInfo;
    function GetConstraintCount: integer;
    function GetTaggedvalueCount: integer;
    function GetConstraintByIndex(Index: integer): TBoldConstraintRTInfo;
    function GetTaggedValueByIndex(Index: integer): string;
  protected
    function GetListTypeInfo: TBoldListTypeInfo; override;
  public
    constructor Create(MoldElement: TMoldElement; SystemTypeInfo: TBoldSystemTypeInfo);
    destructor Destroy; override;
    procedure AddConstraint(Constraint: TBoldConstraintRTInfo);
    property Constraint[const Name: String]: TBoldConstraintRTInfo read GetConstraints;
    property ConstraintCount: integer read GetConstraintCount;
    property ConstraintByIndex[Index: integer]: TBoldConstraintRTInfo read GetConstraintByIndex;

    property TaggedValues[const Tag: string]: string read GetTaggedValues;
    property TaggedValueCount: integer read GetTaggedvalueCount;
    property TaggedValueByIndex[Index: integer]: string read GetTaggedValueByIndex;
  end;

  TBoldMetaElementWithConstraint = class(TBoldMetaElement)
  private
    fConstraints: TBoldConstraintRTInfoList;
    fTaggedValues: TStrings;
    function GetTaggedValues(const Tag: string): string;
    function GetConstraints(const Name: String): TBoldConstraintRTInfo;
    function GetConstraintByIndex(Index: integer): TBoldConstraintRTInfo;
    function GetConstraintCount: integer;
    function GetTaggedValueByIndex(Index: integer): string;
    function GetTaggedvalueCount: integer;
  public
    constructor Create(MoldElement: TMoldElement; const ModelName, ExpressionName, DelphiName: String; SystemTypeInfo: TBoldSystemTypeInfo);
    destructor Destroy; override;
    procedure AddConstraint(Constraint: TBoldConstraintRTinfo);
    property Constraint[const Name: String]: TBoldConstraintRTInfo read GetConstraints;
    property ConstraintCount: integer read GetConstraintCount;
    property ConstraintByIndex[Index: integer]: TBoldConstraintRTInfo read GetConstraintByIndex;
    property TaggedValues[const Tag: string]: string read GetTaggedValues;
    property TaggedValueCount: integer read GetTaggedvalueCount;
    property TaggedValueByIndex[Index: integer]: string read GetTaggedValueByIndex;
  end;

  {---TBoldSystemTypeInfo---}
  TBoldSystemTypeInfo = class(TBoldElementTypeInfoWithConstraint)
  private
{$IFDEF BOLD_LITE}
    fClassTypeInfoMem: array[0..LITE_VERSION_CLASS_LIMIT*CLASS_TYPE_INFO_MEM_SIZE] of Integer;
{$ENDIF}
    fInitializationLog: TStringList;
    fOptimisticLocking: TBoldOptimisticLockingMode;
    fAttributeTypes: TBoldAttributeTypeInfoList;
    fEvaluator: TBoldEvaluator;
    fListTypes: TBoldListTypeInfoList;
    fMethodsInstalled: Boolean;
    fNilTypeInfo: TBoldNilTypeInfo;
    fValueSetTypeInfo: TBoldElementTypeInfo;
    fTopSortedClasses: TBoldClassTypeInfoList;
    fValueSetTypeInfoList: TBoldElementTypeInfoList;
    fTypeTypeInfo: TBoldTypeTypeInfo;
    fUntypedListTypeInfo: TBoldListTypeInfo;
    fUseGeneratedCode: Boolean;
    fGenerateMultiplicityConstraints: Boolean;
    fValueTypeNameList: TBoldElementTypeInfoList;
    fStereotype: string;
    fUseClockLog: Boolean;
    fModelCRC: string;
    function GetInitializationLog: TStringList;
    function GetAttributeTypeInfoByDelphiName(const name: string): TBoldAttributeTypeInfo;
    function GetAttributeTypeInfoByExpressionName(const name: string): TBoldAttributeTypeInfo;
    function GetClassTypeInfoByExpressionName(const name: string): TBoldClassTypeInfo;
    function GetClassTypeInfoByModelName(const name: string): TBoldClassTypeInfo;
    function GetElementTypeInfoByDelphiName(const name: string): TBoldElementTypeInfo;
    function GetElementTypeInfoByExpressionName(const name: string): TBoldElementTypeInfo;
    function GetListTypeInfoByElement(Element: TBoldElementTypeInfo): TBoldListTypeInfo;
    function GetMemberTypeInfoByQualifiedName(const AClassName, AMemberName: string): TBoldMemberRtInfo;
    function GetRootClassTypeInfo: TBoldClassTypeInfo;
    function GetValueTypeNameList: TBoldElementTypeInfoList;
    function GetValueSetTypeInfoList: TBoldElementTypeInfoList;
    procedure InitializationError(const Message: String; args: array of const);
    procedure InstallAttributeType(TypeNameDictionary: TBoldTypeNameDictionary; pos: integer);
    function GetClassTypeInfoByClass(ObjectClass: TClass): TBoldClassTypeInfo;
  protected
    function GetEvaluator: TBoldEvaluator; override;
    function GetBoldType: TBoldElementTypeInfo; override;
  public
    constructor Create(AMoldModel: TMoldModel; UseGeneratedCode, CheckCodeCheckSum: Boolean; TypeNameDictionary: TBoldTypeNameDictionary);
    destructor Destroy; override;
    function ConformsTo(CompareElement: TBoldElementTypeInfo): Boolean; override;
    procedure GetValueTypeNames(S: TStrings; Classes, Types, System, metatype, lists: Boolean);
    procedure ReleaseEvaluator;
    function FindElement(const AText: string; ASearchOptions: TBoldTypeInfoSearchOptions = cDefaultSearchTypeOptions; ASearchTypes: TBoldSearchTypes = cDefaultSearchTypes): TBoldMetaElement;
    function FindValueSetAndTypeByName(const AName: string; out AElement: TBoldElement; out ATypeInfo: TBoldElementTypeInfo): boolean;
    function FindValueSetByName(const AName: string): TBoldElement;
    property AttributeTypeInfoByDelphiName[const name: string]: TBoldAttributeTypeInfo read GetAttributeTypeInfoByDelphiName;
    property AttributeTypeInfoByExpressionName[const name: string]: TBoldAttributeTypeInfo read GetAttributeTypeInfoByExpressionName;
    property AttributeTypes: TBoldAttributeTypeInfoList read fAttributeTypes;
    property ClassTypeInfoByExpressionName[const name: string]: TBoldClassTypeInfo read GetClassTypeInfoByExpressionName;
    property ClassTypeInfoByModelName[const name: string]: TBoldClassTypeInfo read GetClassTypeInfoByModelName;
    property ClassTypeInfoByClass[ObjectClass: TClass]: TBoldClassTypeInfo read GetClassTypeInfoByClass;
    property MemberTypeInfoByQualifiedName[const AClassName, AMemberName: string]: TBoldMemberRtInfo read GetMemberTypeInfoByQualifiedName;
    property ListTypeInfoByElement[Element: TBoldElementTypeInfo]: TBoldListTypeInfo read GetListTypeInfoByElement;
    property ListTypes: TBoldListTypeInfoList read fListTypes;
    property NilTypeInfo: TBoldNilTypeInfo read fNilTypeInfo;
    property ValueSetTypeInfo: TBoldElementTypeInfo read fValueSetTypeInfo;
    property ValueSetTypeInfoList: TBoldElementTypeInfoList read GetValueSetTypeInfoList;
    property ElementTypeInfoByDelphiName[const name: string]: TBoldElementTypeInfo read GetElementTypeInfoByDelphiName;
    property ElementTypeInfoByExpressionName[const name: string]: TBoldElementTypeInfo read GetElementTypeInfoByExpressionName;
    property ValueTypeNameList: TBoldElementTypeInfoList read GetValueTypeNameList;
    property RootClassTypeInfo: TBoldClassTypeInfo read GetRootClassTypeInfo;
    property UntypedListTypeInfo: TBoldListTypeInfo read fUntypedListTypeInfo;
    property TypeTypeInfo: TBoldTypeTypeInfo read fTypeTypeInfo;
    property MethodsInstalled: Boolean read fMethodsInstalled;
    property TopSortedClasses: TBoldClassTypeInfoList read FTopSortedClasses;
    property Persistent: Boolean index befSystemPersistent read GetElementFlag;
    property SystemIsRunnable: Boolean index befSystemIsRunnable read GetElementFlag;
    property UseGeneratedCode: Boolean read fUseGeneratedCode;
    property Stereotype: string read fStereotype;
    property GenerateMultiplicityConstraints: Boolean read fGenerateMultiplicityConstraints;
    property OptimisticLocking: TBoldOptimisticLockingMode read fOptimisticLocking;
    property UpdateWholeObjects: Boolean index befUpdateWholeObjects read GetElementFlag;
    property InitializationLog: TStringList read GetInitializationLog;
    property GenerateDefaultRegions: Boolean index befGenerateDefaultRegions read GetElementFlag;
    property UseClockLog: Boolean read fUseClockLog;
    property ModelCRC: string read fModelCRC;
  end;

  {---TBoldClassTypeInfo---}
  TBoldClassTypeInfo = class(TBoldElementTypeInfoWithConstraint)
  private
    fOptimisticLocking: TBoldOptimisticLockingMode;
    FAllMembers: TBoldMemberRTInfoList;
    fAllRoles: TBoldRoleRTInfoList;
    FFirstOwnMemberIndex: Integer;
    FMethods: TBoldMethodRTInfoList;
    FObjectClass: TClass;
    fSuperClassTypeInfo: TBoldClassTypeInfo;
    fSystemTypeInfo: TBoldSystemTypeInfo;
    fTopSortedIndex: Integer;
    FStereotype: string;
    fDefaultStringRepresentation: string;
    fEmbeddedSingleLinkCount: integer;
    fDerivedMemberCount: integer;
    fAllMembersCount: integer;
    fAllRolesCount: Integer;
    fPackagename: string;
    fSubClasssesBoldClassTypeInfoList: TBoldClassTypeInfoList;
    fListTypeInfo: TBoldListTypeInfo;
    function GetMemberIndexByExpressionName(const name: string): Integer;
    function GetMemberRTInfoByExpressionName(const Name: string): TBoldMemberRTInfo;
    function GetMemberRTInfoByModelName(const Name: string): TBoldMemberRTInfo;
    procedure InitializeMultiplicityConstraints;
    procedure Initialize(MoldClass: TMoldClass; TypeNameDictionary: TBoldTypeNameDictionary; BoldObjectClasses: TBoldGeneratedClassList; BoldObjectListClasses: TBoldGeneratedClassList; SkipMembers: Boolean); virtual;
    procedure SetObjectClass(BoldObjectClasses: TBoldGeneratedClassList);
    function GetQualifiedName: string;
    function GetMultilinkCount: integer;
    function GetSinglelinkCount: integer;
    function GetAttributeCount: integer;
  protected
    constructor Create(SystemTypeInfo: TBoldSystemTypeInfo; moldClass: TMoldClass; TypeNameDictionary: TBoldTypeNameDictionary;  BoldObjectClasses: TBoldGeneratedClassList; BoldObjectListClasses: TBoldGeneratedClassList; SkipMembers: Boolean = false);
    function GetBoldType: TBoldElementTypeInfo; override;
    function GetDisplayName: String; override;
    function GetListTypeInfo: TBoldListTypeInfo; override;
  public
{$IFDEF BOLD_LITE}
    class function NewInstance: TObject; override;
    procedure FreeInstance; override;
{$ENDIF}
    destructor Destroy; override;
    function BoldIsA(C2: TBoldElementTypeInfo): Boolean; virtual;
    function ConformsTo(CompareElement: TBoldElementTypeInfo): Boolean; override;
    function LeastCommonSuperClass(OtherClassTypeInfo: TBoldClassTypeInfo): TBoldClassTypeInfo;
    function ElementClass: TBoldElementClass; override;
    function CreateElement: TBoldElement; override;
    property AllMembers: TBoldMemberRTInfoList read fAllMembers;
    property AllRoles: TBoldRoleRTInfoList read fAllRoles;
    property AllRolesCount: Integer read fAllRolesCount;
    property DerivedMemberCount: integer read fDerivedMemberCount;
    property AttributeCount: integer read GetAttributeCount;
    property AllMembersCount: integer read fAllMembersCount;
    property SinglelinkCount: integer read GetSinglelinkCount;
    property MultilinkCount: integer read GetMultilinkCount;
    property FirstOwnMemberIndex: Integer read FFirstOwnMemberIndex;
    property HasSubclasses: Boolean index befHasSubclasses read GetElementFlag;
    property IsAbstract: Boolean index befIsAbstract read GetElementFlag;
    property IsImported: Boolean index befIsImported read GetElementFlag;
    property IsLinkClass: Boolean index befIsLinkClass read GetElementFlag;
    property MemberIndexByExpressionName[const name: string]: Integer read GetMemberIndexByExpressionName;
    property MemberRTInfoByExpressionName[const Name: string]: TBoldMemberRTInfo read GetMemberRTInfoByExpressionName;
    property MemberRTInfoByModelName[const Name: string]: TBoldMemberRTInfo read GetMemberRTInfoByModelName;
    property Methods: TBoldMethodRTInfoList read fMethods;
    property ObjectClass: TClass read FObjectClass;
    property Persistent: Boolean index befClassPersistent read GetElementFlag;
    property Stereotype: string read FStereotype;
    property SuperClassTypeInfo: TBoldClassTypeInfo read fSuperClassTypeInfo;
    property SystemTypeInfo: TBoldSystemTypeInfo read fSystemTypeInfo;
    property TopSortedIndex: Integer read fTopSortedIndex;
    property DefaultStringRepresentation: String read fDefaultStringRepresentation;
    property OptimisticLocking: TBoldOptimisticLockingMode read fOptimisticLocking;
    property ToBeRemoved: Boolean index befClassToBeRemoved read GetElementFlag;
    property Versioned: Boolean index befVersioned read GetElementFlag;
    property GenerateDefaultRegion: Boolean index befGenerateDefaultRegion read GetElementFlag;
    property EmbeddedSingleLinkCount: integer read fEmbeddedSingleLinkCount;
    property QualifiedName: string read GetQualifiedName;
    property SubClasssesBoldClassTypeInfoList: TBoldClassTypeInfoList read fSubClasssesBoldClassTypeInfoList;
  end;

  {---TBoldNilTypeInfo---}
  TBoldNilTypeInfo = class(TBoldClassTypeInfo)
  protected
    procedure Initialize(MoldClass: TMoldClass; TypeNameDictionary: TBoldTypeNameDictionary; BoldObjectClasses: TBoldGeneratedClassList; BoldObjectListClasses: TBoldGeneratedClassList; SkipMembers: Boolean); override;
  public
    function BoldIsA(C2: TBoldElementTypeInfo): Boolean; override;
    function ConformsTo(CompareElement: TBoldElementTypeInfo): Boolean; override;
    function GetStringRepresentation(Representation: TBoldRepresentation): string; override;
  end;

  {---TBoldMemberRTInfo---}
  TBoldMemberRTInfo = class(TBoldMetaElementWithConstraint)
  private
    fDeriveExpression: string;
    fDeriveMethod: Pointer;
    fReverseDeriveMethod: Pointer;
    fStereotype: string;
    fDispId: integer;
    fBoldType: TBoldElementTypeInfo;
    fClassTypeInfo: TBoldClassTypeInfo;
    fIndex: Integer;
    fDeriverIndex: Integer;
    fEmbeddedLinkIndex: integer;
    fVisibility: TVisibilityKind;
    fStreamName: string;
  protected
    constructor Create(ClassTypeInfo: TBoldClassTypeInfo; moldMember: TMoldMember; TypeNameDictionary: TBoldTypeNameDictionary);
    constructor CreateWithoutMoldMember(ClassTypeInfo: TBoldClassTypeInfo; const ModelName: string; const ExpressionName: string; const DelphiName: string; Persistent: Boolean ; TypeNameDictionary: TBoldTypeNameDictionary);
    function GetDisplayName: String; override;
    function GetBoldType: TBoldElementTypeInfo; override;
    function GetIsRole: Boolean; virtual;
    procedure SetBoldType(BoldType: TBoldElementTypeInfo);
    function GetMemberClass: TClass; virtual; abstract;
    function GetEncouragesOptimisticLockingOnDeletedOnly: Boolean; virtual;
    function GetCanHaveOldValue: Boolean; virtual;
    function GetStoreInUndo: boolean; virtual; abstract;
  public
    destructor Destroy; override;
    property IsDerived: Boolean index befIsDerived read GetElementFlag;
    property ClassTypeInfo: TBoldClassTypeInfo read fClassTypeInfo;
    property DelayedFetch: Boolean index befDelayedFetch read GetElementFlag;
    property index: Integer read FIndex;
    property EmbeddedLinkIndex: integer read fEmbeddedLinkIndex;
    property DeriverIndex: Integer read fDeriverIndex;
    property IsAttribute: Boolean index befIsAttribute read GetElementFlag;
    property IsMultiRole: Boolean index befIsMultiRole read GetElementFlag;
    property IsRole: Boolean read GetIsRole;
    property IsSingleRole: Boolean index befIsSingleRole read GetElementFlag;
    property IsStoredInObject: Boolean index befIsStoredInObject read GetElementFlag;
    property MemberClass: TClass read GetMemberClass;
    property Persistent: Boolean index befMemberPersistent read GetElementFlag;
    property Stereotype: string read fStereotype;
    property StreamName: string read fStreamName;
    property Visibility: TVisibilityKind read fVisibility;
    property DeriveExpression: string read fDeriveExpression;
    property IsReverseDerived: Boolean index befIsReverseDerived read GetElementFlag;
    property IsNonVersionedInVersionedClass: Boolean index befIsNonVersionedInVersionedClass read GetElementFlag;
    property EncouragesOptimisticLockingOnDeletedOnly: Boolean read GetEncouragesOptimisticLockingOnDeletedOnly;
    property CanHaveOldValue: Boolean read GetCanHaveOldValue;
    property ToBeRemoved: Boolean index befMemberToBeRemoved read GetElementFlag;
    property DispId: integer read fDispId;
    property DeriveMethod: Pointer read fDeriveMethod write fDeriveMethod;  // this is a bit unsafe since anyone can change it.
    property ReverseDeriveMethod: Pointer read fReverseDeriveMethod write fReverseDeriveMethod; // this is a bit unsafe since anyone can change it.
    property StoreInUndo: boolean read GetStoreInUndo;
  end;

  {---TBoldRoleRTInfo---}
  TBoldRoleRTInfo = class(TBoldMemberRTInfo)
  private
    fClassTypeInfoOfOtherEnd: TBoldClassTypeInfo;
    fIndexOfOtherEnd: Integer;
    fLinkClassTypeInfo: TBoldClassTypeInfo;
    fOtherIndexInLinkClass: Integer;
    fOwnIndexInLinkClass: Integer;
    fQualifiers: TBoldMemberRTInfoList;
    fRoleRTInfoOfOtherEnd: TBoldRoleRTInfo;
    fRoleType: TBoldRoleType;
    fMultiplicity: string;
    fChangeability: TChangeableKind;
    fAggregation: TAggregationKind;
    fDeleteAction: TDeleteAction;
    fAssociationStereotype: string;
    class procedure SetPass2InfoForAssociation(SystemTypeInfo: TBoldSystemTypeInfo; moldASsociation: TMoldAssociation);
    function GetIndexOfLinkObjectRole: Integer;
    function GetIndexOfMainRole: Integer;
    procedure InitQualifiers(Qualifiers: TMoldQualifierList);
    function GetDefaultRegionMode: TBoldAssociationEndDefaultRegionMode;
  protected
    constructor Create(ClassTypeInfo: TBoldClassTypeInfo; MoldRole: TMoldRole; TypeNameDictionary: TBoldTypeNameDictionary);
    constructor CreateInnerLinkRole(ClassTypeInfo: TBoldClassTypeInfo; MoldRole: TMoldRole; Dummy: Smallint; TypeNameDictionary: TBoldTypeNameDictionary);
    constructor CreateLinkObjectRole(ClassTypeInfo: TBoldClassTypeInfo; MainRole: TMoldRole; TypeNameDictionary: TBoldTypeNameDictionary ; Dummy: SmallInt);
    function GetIsQualified: Boolean;
    function GetMemberClass: TClass; override;
    function GetQualifiers: TBoldMemberRTInfoList;
    function GetStringRepresentation(Representation: TBoldRepresentation): string; override;
    function GetEncouragesOptimisticLockingOnDeletedOnly: Boolean; override;
    function GetCanHaveOldValue: Boolean; override;
    function GetIsRole: Boolean; override;
    function GetStoreInUndo: boolean; override;
  public
    destructor Destroy; override;
    procedure SetForceOtherEnd;
    property ClassTypeInfoOfOtherEnd: TBoldClassTypeInfo read FClassTypeInfoOfOtherEnd;
    property ForceOtherEnd: Boolean index befForceOtherEnd read GetElementFlag;
    property IndexOfLinkObjectRole: Integer read GetIndexOfLinkObjectRole;
    property IndexOfMainRole: Integer read GetIndexOfMainRole;
    property IndexOfOtherEnd: Integer read FIndexOfOtherEnd;
    property IsIndirect: Boolean index befIsIndirect read GetElementFlag;
    property IsNavigable: Boolean index befIsNavigable read GetElementFlag;
    property IsOrdered: Boolean index befIsOrdered read GetElementFlag;
    property IsQualified: Boolean read GetIsQualified;
    property IsQualifiedMulti: Boolean index befQualifiedMulti read GetElementFlag;
    property LinkClassTypeInfo: TBoldClassTypeInfo read FLinkClassTypeInfo;
    property Mandatory: Boolean index befMandatory read GetElementFlag;
    property Multiplicity: string read fMultiplicity;
    property OtherEndOrdered: Boolean index befOtherEndOrdered read GetElementFlag;
    property OtherIndexInLinkClass: Integer read FOtherIndexInLinkClass;
    property OwnIndexInLinkClass: Integer read FOwnIndexInLinkClass;
    property Qualifiers: TBoldMemberRTInfoList read GetQualifiers;
    property RoleRTInfoOfOtherEnd: TBoldRoleRTInfo read fRoleRTInfoOfOtherEnd;
    property RoleType: TBoldRoleType read fRoleType;
    property Changeability: TChangeableKind read fChangeability;
    property Aggregation: TAggregationKind read fAggregation;
    property DeleteAction: TDeleteAction read fDeleteAction;
    property DefaultRegionMode: TBoldAssociationEndDefaultRegionMode read GetDefaultRegionMode;
    property AssociationStereotype: string read fAssociationStereotype;
  end;

  {---TBoldAttributeRTInfo---}
  TBoldAttributeRTInfo = class(TBoldMemberRTInfo)
  private
    FLength: Integer;
    fInitialvalue: string;
  protected
    constructor Create(ClassTypeInfo: TBoldClassTypeInfo; MoldAttribute: TMoldAttribute; TypeNameDictionary: TBoldTypeNameDictionary);
    function GetMemberClass: TClass; override;
    function GetStringRepresentation(Representation: TBoldRepresentation): string; override;
    function GetIsRole: Boolean; override;
    function GetStoreInUndo: boolean; override;
  public
    property AllowNull: Boolean index befAllowNull read GetElementFlag;
    property Length: Integer read FLength;
    property InitialValue: string read fInitialvalue;
    property HasInitialValue: Boolean index befHasInitalvalue read GetElementFlag;
  end;

  {---TBoldAttributeTypeinfo---}
  TBoldAttributeTypeInfo = class(TBoldElementTypeInfo)
  private
    fAttributeClass: TClass;
    fIsAbstract: Boolean;
    fSuperAttributeTypeInfo: TBoldAttributeTypeInfo;
    fListTypeInfo: TBoldListTypeInfo;
  protected
    constructor Create(const ModelName, ExpressionName: string; AttributeClass: TClass; SuperType: TBoldAttributeTypeInfo;const  FallBackDelphiName: String; SystemTypeInfo: TBoldSystemTypeInfo; IsAbstract: Boolean);
    function GetBoldType: TBoldElementTypeInfo; override;
    function GetListTypeInfo: TBoldListTypeInfo; override;
  public
    function BoldIsA(aType: TBoldElementTypeInfo): Boolean;
    function ConformsTo(CompareElement: TBoldElementTypeInfo): Boolean; override;
    function ElementClass: TBoldElementClass; override;
    function CreateElement: TBoldElement; override;
    property AttributeClass: TClass read fAttributeClass;
    property IsAbstract: Boolean read fIsAbstract;
    property SuperAttributeTypeInfo: TBoldAttributeTypeInfo read fSuperAttributeTypeInfo;
    property ListTypeInfo: TBoldListTypeInfo read GetListTypeInfo;
  end;

  {---TBoldMethodRTInfo---}
  TBoldMethodRTInfo = class(TBoldMetaElementWithConstraint)
  private
    fClassTypeInfo: TBoldClassTypeInfo;
    fVisibility: TVisibilityKind;

    FFuncType: TDelphiFunctionType;
    fInheritedFrom: Integer;
    FIsClassMethod: Boolean;
    fParameterList: TStringList;
    FReturnType: string;
    fSignature: String;
    fDispId: integer;
    procedure EnsureParameterLIst(const Signature: string);
    function getIsInherited: Boolean;
    function getNumberOfParameters: Integer;
    function getParameters(Number: Integer): string;
    function getParameterTypes(Number: Integer): string;
    function GetSignature: string;
    function ParameterList: TStringLIst;
  protected
    function GetStringRepresentation(Representation: TBoldRepresentation): string; override;
    function GetBoldType: TBoldElementTypeInfo; override;
  public
    constructor Create(ClassTypeInfo: TBoldClassTypeInfo; moldMethod: TMoldMethod; Inheritedfrom: Integer; TypeNameDictionary: TBoldTypeNameDictionary);
    destructor Destroy; override;
    property FuncType: TDelphiFunctionType read FFuncType;
    property Inheritedfrom: Integer read fInheritedFrom;
    property IsClassmethod: Boolean read FIsClassMethod;
    property IsInherited: Boolean read getIsInherited;
    property NumberOfParameters: Integer read getNumberOfParameters;
    property Parameters[Number: Integer]: string read getParameters;
    property ParameterTypes[Number: Integer]: string read getParameterTypes;
    property ReturnType: string read FReturnType;
    property Signature: string read GetSignature;
    property ClassTypeInfo: TBoldClassTypeInfo read fClassTypeInfo;
    property DispId: integer read fDispId;
  end;

  TBoldConstraintRTInfo = class(TBoldMetaElementWithConstraint)
  private
    fBoldType: TBoldTypeTypeInfo;
    fExpression: String;
    fDescription: String;
  public
    constructor create(MoldElement: TMoldElement; const ModelName, ExpressionName, DelphiName: String; SystemTypeInfo: TBoldSystemTypeInfo; const Expression, Description: String);
    function GetBoldType: TBoldElementTypeInfo; override;

    property ConstraintExpression: String read fExpression;
    property ConstraintMessage: String read fDescription;
  end;

implementation

uses
  SysUtils,

  BoldCoreConsts,
  BoldUtils,
  BoldGuard,
  BoldNameExpander,
  BoldHashIndexes,
  BoldMemberTypeDictionary,
  BoldOcl,
  BoldTypeList,
  BoldDefaultTaggedValues,
  BoldSystem,
  BoldAttributes,
  BoldDefaultStreamNames;

{$IFDEF BOLD_LITE}
var
  G_TheSystemType: TBoldSystemTypeInfo;
{$ENDIF}

type
  {---TObjectClassIndex---}
  TObjectClassIndex = class(TBoldClassHashIndex)
  protected
    function ItemAsKeyClass(Item: TObject): TClass; override;
  end;

  { TElementIndex }
  TElementIndex = class(TBoldObjectHashIndex)
  protected
    function ItemAsKeyObject(Item: TObject): TObject; override;
  end;

  {---TObjectClassIndex---}
  TListClassIndex = class(TBoldClassHashIndex)
  protected
    function ItemAsKeyClass(Item: TObject): TClass; override;
  end;

{ TElementIndex }
function TElementIndex.ItemASKeyObject(Item: TObject): TObject;
begin
  Assert(Item is TBoldListTypeInfo);
  Result := TBoldListTypeInfo(Item).ListElementTypeInfo;
end;

  {---TObjectClassIndex---}
function TObjectClassIndex.ItemAsKeyClass(Item: TObject): TClass;
begin
  Result := TBoldClassTypeInfo(Item).ObjectClass;
end;

{---TBoldClassTypeInfoList---}
constructor TBoldClassTypeInfoList.Create;
begin
  inherited;
  SetIndexCapacity(4);
  SetIndexvariable(IX_ObjectClass, AddIndex(TObjectClassIndex.Create));
end;

function TBoldClassTypeInfoList.GetEnumerator: TBoldClassTypeInfoListTraverser;
begin
  result := CreateTraverser as TBoldClassTypeInfoListTraverser;
end;

function TBoldClassTypeInfoList.GetItem(index: Integer): TBoldClassTypeInfo;
begin
  Result := TBoldClassTypeInfo(inherited Items[index]);
end;

function TBoldClassTypeInfoList.GetItemByModelName(const ModelName: string): TBoldClassTypeInfo;
begin
  Result := TBoldClassTypeInfo(inherited ItemsByModelName[ModelName]);
end;

function TBoldClassTypeInfoList.GetItemByExpressionName(const ExpressionName: string): TBoldClassTypeInfo;
begin
  Result := TBoldClassTypeInfo(inherited ItemsByExpressionName[ExpressionName]);
end;

function TBoldClassTypeInfoList.GetItemByObjectClass(ObjectClass: TClass): TBoldClassTypeInfo;
begin
  Result := TBoldClassTypeInfo(TBoldClassHashIndex(Indexes[IX_ObjectClass]).FindByClass(ObjectClass));
end;

function TBoldClassTypeInfoList.TraverserClass: TBoldIndexableListTraverserClass;
begin
  result := TBoldClassTypeInfoListTraverser; 
end;

{---TBoldMemberRTInfoList---}
function TBoldMemberRTInfoList.GetEnumerator: TBoldMemberRTInfoListTraverser;
begin
  result := CreateTraverser as TBoldMemberRTInfoListTraverser;
end;

function TBoldMemberRTInfoList.GetItem(index: Integer): TBoldMemberRTInfo;
begin
  Result := TBoldMemberRTInfo(inherited Items[index]);
end;

function TBoldMemberRTInfoList.GetItemByModelName(const ModelName: string): TBoldMemberRTInfo;
begin
  Result := TBoldMemberRTInfo(inherited ItemsByModelName[ModelName]);
end;

function TBoldMemberRTInfoList.TraverserClass: TBoldIndexableListTraverserClass;
begin
  result := TBoldMemberRTInfoListTraverser;
end;

function TBoldMemberRTInfoList.GetItemByExpressionName(const ExpressionName: string): TBoldMemberRTInfo;
begin
  Result := TBoldMemberRTInfo(inherited ItemsByExpressionName[ExpressionName]);
end;

{---TBoldMethodRTInfoList---}
function TBoldMethodRTInfoList.GetEnumerator: TBoldMethodRTInfoListTraverser;
begin
  result := CreateTraverser as TBoldMethodRTInfoListTraverser;
end;

function TBoldMethodRTInfoList.GetItem(index: Integer): TBoldMethodRTInfo;
begin
  Result := TBoldMethodRTInfo(inherited Items[index]);
end;

function TBoldMethodRTInfoList.GetItemByModelName(const ModelName: string): TBoldMethodRTInfo;
begin
  Result := TBoldMethodRTInfo(inherited ItemsByModelName[ModelName]);
end;

function TBoldMethodRTInfoList.TraverserClass: TBoldIndexableListTraverserClass;
begin
  result := TBoldMethodRTInfoListTraverser;
end;

function TBoldMethodRTInfoList.GetItemByExpressionName(const ExpressionName: string): TBoldMethodRTInfo;
begin
  Result := TBoldMethodRTInfo(inherited ItemsByExpressionName[ExpressionName]);
end;

{---TBoldListTypeInfoList---}
function TBoldListTypeInfoList.GetItemByElement(Element: TBoldElementTypeInfo): TBoldListTypeInfo;
begin
  Result := TBoldListTypeInfo(TBoldObjectHashIndex(Indexes[IX_Element]).FindByObject(Element))
end;

function TBoldListTypeInfoList.GetItemByListClass(
  ListClass: TClass): TBoldListTypeInfo;
begin
  Result := TBoldListTypeInfo(TBoldClassHashIndex(Indexes[IX_ListClass]).FindByClass(ListClass));
end;

constructor TBoldListTypeInfoList.Create;
begin
  inherited;
  SetIndexCapacity(4);
  SetIndexVariable(IX_Element, AddIndex(TElementIndex.Create));
  SetIndexVariable(IX_ListClass, AddIndex(TListClassIndex.Create));
end;

{---TBoldTypeTypeInfo---}
function TBoldTypeTypeInfo.ConformsTo(CompareElement: TBoldElementTypeInfo): Boolean;
begin
  Result := CompareElement = self;
end;

constructor TBoldTypeTypeInfo.Create(const ModelName: string; const ExpressionName: string; const Delphiname: string; ModelTypeInfo: TBoldElementTypeInfo);
begin
  inherited Create(ModelName, ExpressionName, Delphiname, ModelTypeInfo);
  SetValueType(bvtType);
end;

function TBoldTypeTypeInfo.GetBoldType: TBoldElementTypeInfo;
begin
  result := self;
end;

function TBoldTypeTypeInfo.GetListTypeInfo: TBoldListTypeInfo;
begin
  Result := TBoldSystemTypeInfo(SystemTypeInfo).ListTypes.ItemByElement[self];
end;

{ TBoldAttributeTypeInfoList }

function TBoldAttributeTypeInfoList.GetEnumerator: TBoldAttributeTypeInfoListTraverser;
begin
  result := CreateTraverser as TBoldAttributeTypeInfoListTraverser;
end;

function TBoldAttributeTypeInfoList.GetItem(
  index: Integer): TBoldAttributeTypeInfo;
begin
  Result := TBoldAttributeTypeInfo(inherited Items[index]);
end;

function TBoldAttributeTypeInfoList.GetItemByExpressionName(
  const ExpressionName: string): TBoldAttributeTypeInfo;
begin
  Result := TBoldAttributeTypeInfo(inherited ItemsByExpressionName[ExpressionName]);
end;

function TBoldAttributeTypeInfoList.GetItemByModelName(
  const ModelName: string): TBoldAttributeTypeInfo;
begin
  Result := TBoldAttributeTypeInfo(inherited ItemsByModelName[ModelName]);
end;

function TBoldAttributeTypeInfoList.TraverserClass: TBoldIndexableListTraverserClass;
begin
  result := TBoldAttributeTypeInfoListTraverser;
end;

{---TBoldSystemTypeInfo---}

function TBoldSystemTypeInfo.GetListTypeInfoByElement(Element: TBoldElementTypeInfo): TBoldListTypeInfo;
begin
  if not Assigned(Element) then
    Result := UntypedListTypeInfo
  else
    result := Element.ListTypeInfo;
end;

function TBoldSystemTypeInfo.GetMemberTypeInfoByQualifiedName(const AClassName, AMemberName: string): TBoldMemberRtInfo;
var
  vClassTypeInfo: TBoldClassTypeInfo;
begin
  result := nil;
  vClassTypeInfo := ClassTypeInfoByExpressionName[AClassName];
  if Assigned(vClassTypeInfo) then
    result := vClassTypeInfo.MemberRTInfoByExpressionName[AMemberName];
end;

function TBoldSystemTypeInfo.GetClassTypeInfoByClass(
  ObjectClass: TClass): TBoldClassTypeInfo;
begin
  result := TopSortedClasses.ItemsByObjectClass[ObjectClass];
end;

function TBoldSystemTypeInfo.GetClassTypeInfoByExpressionName(const name: string): TBoldClassTypeInfo;
begin
  result := TopSortedClasses.ItemsByExpressionName[name];
end;

function TBoldSystemTypeInfo.GetAttributeTypeInfoByExpressionName(const name: string): TBoldAttributeTypeInfo;
begin
  Result := TBoldAttributeTypeInfo(fAttributeTypes.ItemsByExpressionName[name]);
  Assert(not Assigned(result) or (Result is TBoldAttributeTypeInfo));
end;

function TBoldSystemTypeInfo.GetAttributeTypeInfoByDelphiName(const name: string): TBoldAttributeTypeInfo;
begin
  Result := TBoldAttributeTypeInfo(fAttributeTypes.ItemsByDelphiName[name]);
  Assert(not Assigned(result) or (Result is TBoldAttributeTypeInfo));
end;


constructor TBoldSystemTypeInfo.Create(AMoldModel: TMoldModel; UseGeneratedCode, CheckCodeCheckSum: Boolean; TypeNameDictionary: TBoldTypeNameDictionary);
var
  i: integer;
  BoldObjectClasses: TBoldGeneratedClassList;
  BoldObjectListClasses: TBoldGeneratedClassList;
  Errors: TStringList;
  Guard: IBoldGuard;
  BoldGeneratedCodeDescriptor: TBoldGeneratedCodeDescriptor;
begin
  inherited Create(AMoldModel, self);
  Guard := TBoldGuard.Create(BoldObjectListClasses, BoldObjectClasses, Errors);

  fStereotype := AMoldModel.Stereotype;
  fUseClockLog := StringToBoolean(AMoldModel.BoldTVByName[TAG_USECLOCKLOG]);

  SetElementFlag(befSystemIsRunnable, true);
  fOptimisticLocking := AMoldModel.OptimisticLocking;
{$IFDEF BOLD_LITE}
  G_TheSystemType := self;
{$ENDIF}
  SetValueType(bvtSystem);
  fTypeTypeInfo := TBoldTypeTypeInfo.Create('MetaType', 'MetaType', 'MetaType', self); // do not localize

  fAttributeTypes := TBoldAttributeTypeInfoList.Create;
  AttributeTypes.Capacity := TypeNameDictionary.count;
  fListTypes := TBoldListTypeInfoList.Create;
  ListTypes.Capacity := TypeNameDictionary.count + AMoldModel.Classes.Count + 1;
  ListTypes.Add(TBoldListTypeInfo.Create(fTypeTypeinfo, self, TBoldTypeList));
  SetElementFlag(befSystemPersistent, true);
  SetElementFlag(befGenerateDefaultRegions, AMoldModel.GenerateDefaultRegions);
  FTopSortedClasses := TBoldClassTypeInfoList.Create;

  BoldObjectListClasses := TBoldGeneratedClassList.Create;
  BoldObjectClasses := TBoldGeneratedClassList.Create;
  fGenerateMultiplicityConstraints := TVIsTrue(AMoldModel.BoldTVByName[TAG_GENERATEMULTIPLICITYCONSTRAINTS]);

  fUseGeneratedCode := UseGeneratedCode;
  fModelCRC := AMoldModel.CRC;

  if UseGeneratedCode then
  begin
    BoldGeneratedCodeDescriptor := nil;
    for i := 0 to GeneratedCodes.Count - 1 do
    begin
      if (GeneratedCodes.ModelEntries[i].ExpressionName = AMoldModel.ExpandedExpressionName) then
      begin
        BoldGeneratedCodeDescriptor := GeneratedCodes.ModelEntries[i];
        if (not CheckCodeCheckSum or (BoldGeneratedCodeDescriptor.CRC = '') or (BoldGeneratedCodeDescriptor.CRC = AMoldModel.CRC)) then
        begin
          if assigned(BoldGeneratedCodeDescriptor.InstallBusinessClasses) then
          begin
            BoldObjectClasses.Capacity := AMoldModel.Classes.Count;
            BoldGeneratedCodeDescriptor.InstallBusinessClasses(BoldObjectClasses);
          end;
          if assigned(BoldGeneratedCodeDescriptor.InstallObjectListClasses) then
          begin
            BoldObjectListClasses.Capacity := AMoldModel.Classes.Count;
            BoldGeneratedCodeDescriptor.InstallObjectListClasses(BoldObjectListClasses);
          end;
          break;
        end;
      end;
    end;

    if CheckCodeCheckSum and (Assigned(BoldGeneratedCodeDescriptor) and (BoldGeneratedCodeDescriptor.CRC <> '') and 
      (BoldGeneratedCodeDescriptor.CRC <> AMoldModel.CRC) ) then
    begin
      InitializationError(sCRCDiffers, [AMoldModel.CRC, BoldGeneratedCodeDescriptor.CRC]);
    end;
  end;

  for I := 0 to TypeNameDictionary.count - 1 do
  begin
    InstallAttributeType(TypeNameDictionary, i);
  end;

  // The super-element-list that all other lists conform to (Used for OCL)
  // Note: The elements does not need to (and should not) conform!
  fUntypedListTypeInfo := TBoldListTypeInfo.Create(nil, self, TBoldObjectList);
  ListTypes.Add(fUntypedListTypeInfo);

  // Superclasses must be constructed first.
  // This also assures that FClasses will be topologicaly sorted
  AMoldModel.EnsureTopSorted;

  Errors := TStringList.Create;
  Errors.CommaText := AMoldModel.TVByName[BOLDINTERALTVPREFIX + TV_MODELERRORS];
  if Errors.Count > 0 then
  begin
    for i := 0 to Errors.Count - 1 do
      InitializationError(Errors[i], []);
    TBoldClassTypeInfo.Create(self, AMoldModel.Classes[0], TypeNameDictionary, BoldObjectClasses, BoldObjectListClasses, true);
    exit;
  end;

  TopSortedClasses.Capacity := AMoldModel.Classes.Count;
  for I := 0 to AMoldModel.Classes.Count - 1 do
    TBoldClassTypeInfo.Create(self, AMoldModel.Classes[I], TypeNameDictionary, BoldObjectClasses, BoldObjectListClasses);

  for I := 0 to AMoldModel.Associations.Count - 1 do
    TBoldRoleRTInfo.SetPass2InfoForAssociation(self, AMoldModel.Associations[I]);

  if GenerateMultiplicityConstraints then
    for i := 0 to TopSortedClasses.Count - 1 do
      TopSortedClasses[i].InitializeMultiplicityConstraints;

  if UseGeneratedCode then
  begin
    for i := 0 to TopSortedClasses.Count - 1 do
      if CompareText(TopSortedClasses[i].ObjectClass.ClassName, TopSortedClasses[i].DelphiName) <> 0 then
      begin
        InitializationError(sGeneratedCodeNotRegistered, [TopSortedClasses[i].ExpressionName]);
      end;
  end;

  fMethodsInstalled := False;
  fNilTypeInfo := TBoldNilTypeInfo.Create(self, nil, nil, nil, nil);
  fValueSetTypeInfo := AttributeTypeInfoByExpressionName['ValueSet'];
end;

function TBoldSystemTypeInfo.GetEvaluator: TBoldEvaluator;
begin
  if not assigned(fEvaluator) then
    fEvaluator := TBoldOCL.Create(self, nil);
  Result := fEvaluator;
end;

destructor TBoldSystemTypeInfo.Destroy;
begin
  FreeAndNil(fListTypes);
  FreeAndNil(fValueSetTypeInfoList);
  FreeAndNil(fAttributeTypes);
  FreeAndNil(fEvaluator);
  FreeAndNil(fTypeTypeInfo);
  FreeAndNil(fValueTypeNameList);
  FreeAndNil(FTopSortedClasses);
  FreeAndNil(fNilTypeInfo);
  FreeAndNil(fInitializationLog);
  inherited;
end;

function TBoldSystemTypeInfo.FindElement(const AText: string;
  ASearchOptions: TBoldTypeInfoSearchOptions;
  ASearchTypes: TBoldSearchTypes): TBoldMetaElement;

var
  s: string;

  function StringMatch(AElement: TBoldMetaElement): boolean;
  begin
    result :=((soPartialMatch in ASearchOptions) and (Pos(s, UpperCase(AElement.ExpressionName)) > 0))
    or SameText(UpperCase(AElement.ExpressionName), s);
  end;

var
  ClassTypeInfo: TBoldClassTypeInfo;
  MemberRTInfo: TBoldMemberRTInfo;
  MethodRTInfo: TBoldMethodRTInfo;
  AttributeTypeInfo: TBoldAttributeTypeInfo;
begin
  result := nil;
  s := UpperCase(AText);
// TBoldSearchType = stClass, stAttribute, stRole, stMethod, stType
  for ClassTypeInfo in TopSortedClasses do
  begin
    if (stClass in ASearchTypes) and StringMatch(ClassTypeInfo) then
    begin
      result := ClassTypeInfo;
      exit;
    end;
    if (stAttribute in ASearchTypes) or (stRole in ASearchTypes) then
      for MemberRTInfo in ClassTypeInfo.AllMembers do
      begin
        if (((stRole in ASearchTypes) and MemberRTInfo.IsRole)
        or ((stAttribute in ASearchTypes) and MemberRTInfo.IsAttribute)
        and StringMatch(MemberRTInfo)) then
        begin
          result := MemberRTInfo;
          exit;
        end
      end;
    if (stMethod in ASearchTypes) then
      for MethodRTInfo in ClassTypeInfo.Methods do
        if StringMatch(MethodRTInfo) then
        begin
          result := MethodRTInfo;
          exit;
        end
  end;
  if (stType in ASearchTypes) then
    for AttributeTypeInfo in AttributeTypes do
      if StringMatch(AttributeTypeInfo) then
      begin
        result := AttributeTypeInfo;
        exit;
      end
end;

function TBoldSystemTypeInfo.FindValueSetAndTypeByName(
  const AName: string; out AElement: TBoldElement; out ATypeInfo: TBoldElementTypeInfo): boolean;
var
  i: integer;
  vValueSetClass: TBAValueSetClass;
begin
  result := false;
  if AName = '' then
    exit;
  for I := 0 to ValueSetTypeInfoList.Count - 1 do
  begin
    vValueSetClass := TBAValueSetClass(ValueSetTypeInfoList[i].ElementClass);
    // TBABoolean descendants like TBAConstraint do not define new values, so they contain duplicated T/F therefore we skip them
    if {(vValueSetClass = TBABoolean) or} {(not vValueSetClass.InheritsFrom(TBABoolean) and} (vValueSetClass.GetValues <> nil) then
    begin
      AElement := vValueSetClass.GetValues.FindByString(brDefault, AName);
      if Assigned(AElement) then
      begin
        result := true;
        ATypeInfo := ValueSetTypeInfoList[i];
        exit;
      end;
    end;
  end;
end;

function TBoldSystemTypeInfo.FindValueSetByName(
  const AName: string): TBoldElement;
var
  vTypeInfo: TBoldElementTypeInfo;
begin
  FindValueSetAndTypeByName(AName, result, vTypeInfo);
end;

function TBoldSystemTypeInfo.ConformsTo(CompareElement: TBoldElementTypeInfo): Boolean;
begin
  Result := CompareElement = self;
end;

function TBoldSystemTypeInfo.GetValueTypeNameList: TBoldElementTypeInfoList;
var
  I: Integer;
begin
  if not assigned(fValueTypeNameList) then
  begin
    fValueTypeNameList := TBoldElementTypeInfoList.Create;
    fValueTypeNameList.OwnsEntries := false;
    fValueTypeNameList.Capacity := TopSortedClasses.Count + AttributeTypes.Count + ListTypes.Count + 2;
    fValueTypeNameList.Add(self);
    for I := 0 to TopSortedClasses.Count - 1 do
      fValueTypeNameList.Add(TopSortedClasses[I]);
    for I := 0 to AttributeTypes.Count - 1 do
      fValueTypeNameList.Add(AttributeTypes[I]);
    fValueTypeNameList.Add(BoldType);
    for I := 0 to ListTypes.Count - 1 do
      fValueTypeNameList.Add(ListTypes[I]);
  end;
  Result := fValueTypeNameList;
end;

function TBoldSystemTypeInfo.GetValueSetTypeInfoList: TBoldElementTypeInfoList;
var
  I: Integer;
begin
  if not assigned(fValueSetTypeInfoList) then
  begin
    fValueSetTypeInfoList := TBoldElementTypeInfoList.Create;
    fValueSetTypeInfoList.OwnsEntries := false;
    fValueSetTypeInfoList.Capacity := AttributeTypes.Count;
    for I := 0 to AttributeTypes.Count - 1 do
      if AttributeTypes[I].ConformsTo(fValueSetTypeInfo) and not TBoldAttributeTypeInfo(AttributeTypes[I]).IsAbstract then
        fValueSetTypeInfoList.Add(AttributeTypes[I]);
  end;
  result := fValueSetTypeInfoList;
end;

procedure TBoldSystemTypeInfo.GetValueTypeNames(S: TStrings; Classes, Types, System, metatype, lists: Boolean);
var
  I: Integer;
begin
  S.Clear;
  s.Capacity := ValueTypeNameList.Count;
  s.BeginUpdate;
  try
  for I := 0 to ValueTypeNameList.Count - 1 do
  begin
    case ValueTypeNameList[I].BoldValueType of
      bvtList: if lists then S.Add(ValueTypeNameList[I].ExpressionName);
      bvtClass: if Classes then S.Add(ValueTypeNameList[I].ExpressionName);
      bvtAttr: if Types then S.Add(ValueTypeNameList[I].ExpressionName);
      bvtSystem: if System then S.Add(ValueTypeNameList[I].ExpressionName);
      bvtType: if metatype then S.Add(ValueTypeNameList[I].ExpressionName);
    end;
  end;
  finally
    s.EndUpdate;
  end;
end;

function TBoldSystemTypeInfo.GetElementTypeInfoByExpressionName(const name: string): TBoldElementTypeInfo;
begin
  Result := ValueTypeNameList.ItemsByExpressionName[name];
end;

function TBoldSystemTypeInfo.GetElementTypeInfoByDelphiName(const name: string): TBoldElementTypeInfo;
begin
  Result := ValueTypeNameList.ItemsByDelphiName[name];
end;

{---TBoldClassTypeInfo---}

procedure TBoldClassTypeInfo.SetObjectClass(BoldObjectClasses: TBoldGeneratedClassList);
var
  Descriptor: TBoldGeneratedClassDescriptor;
begin
  fObjectClass := nil;

  Descriptor := BoldObjectClasses.EntryByExpressionName[ExpressionName];
  if assigned(Descriptor) then
    fObjectClass := Descriptor.TheClass;

  if not assigned(fObjectClass) then
  begin
    if assigned(SuperClassTypeInfo) then
      fObjectClass := SuperClassTypeInfo.ObjectClass
    else
      fObjectClass := TBoldObject;
  end;
end;

function TBoldClassTypeInfo.GetMemberRTInfoByExpressionName(const Name: string): TBoldMemberRTInfo;
begin
  Result := FAllMembers.ItemsByExpressionName[Name];
end;

function TBoldClassTypeInfo.GetMemberIndexByExpressionName(const name: string): Integer;
var
  TempMember: TBoldMemberRTInfo;
begin
  TempMember := MemberRTInfoByExpressionName[name];
  if assigned(TempMember) then
    Result := TempMember.index
  else
    Result := -1;
end;

constructor TBoldClassTypeInfo.Create(SystemTypeInfo: TBoldSystemTypeInfo; moldClass: TMoldClass; TypeNameDictionary: TBoldTypeNameDictionary;  BoldObjectClasses: TBoldGeneratedClassList; BoldObjectListClasses: TBoldGeneratedClassList; SkipMembers: Boolean = false);
begin
  if assigned(MoldClass) then
  begin
    inherited Create(MoldClass, SystemTypeInfo);
    fStereotype := MoldClass.Stereotype;
  end
  else
    inherited Create(nil, SystemTypeInfo);

  SetValueType(bvtClass);
  fSystemTypeInfo := SystemTypeInfo;
  fAllMembers := TBoldMemberRTInfoList.Create;
  FAllRoles := TBoldRoleRTInfoList.Create;
  FAllRoles.OwnsEntries := false;
  fMethods := TBoldMethodRTInfoList.Create;
  fSubClasssesBoldClassTypeInfoList := TBoldClassTypeInfoList.Create;
  fSubClasssesBoldClassTypeInfoList.OwnsEntries := false;
  fAllMembersCount := -1;
  Initialize(MoldClass, TypeNameDictionary, BoldObjectClasses, BoldObjectListClasses, SkipMembers);
end;

function TBoldClassTypeInfo.ElementClass: TBoldElementClass;
begin
  result := TBoldObjectReference
end;

function TBoldClassTypeInfo.CreateElement: TBoldElement;
begin
  result := TBoldObjectReference.CreateWithTypeInfo(self);
end;

function TBoldClassTypeInfo.BoldIsA(C2: TBoldElementTypeInfo): Boolean;
var
  lBoldClassTypeInfo: TBoldClassTypeInfo;
begin
  if Assigned(c2) and (c2.ClassType = TBoldClassTypeInfo) then
  begin
    if C2 = Self then
    begin
      Result := True;
    end
    else
    begin
      lBoldClassTypeInfo := TBoldClassTypeInfo(C2);
      // if TopSortedIndex of this class is smaller than of the other class then it can't possibly descend from it
      if TopSortedIndex < lBoldClassTypeInfo.TopSortedIndex then
        Result := false
      else
      begin
        if SystemTypeInfo.UseGeneratedCode then
          Result := ObjectClass.InheritsFrom(lBoldClassTypeInfo.ObjectClass)
        else
        begin
          Result := false;
          lBoldClassTypeInfo := self;
          while not result and Assigned(lBoldClassTypeInfo.SuperClassTypeInfo) do
          begin
            lBoldClassTypeInfo := lBoldClassTypeInfo.SuperClassTypeInfo;
            Result := lBoldClassTypeInfo = C2;
          end;
        end;
      end;
    end;
  end
  else
    Result := false;
end;

function TBoldClassTypeInfo.ConformsTo(CompareElement: TBoldElementTypeInfo): Boolean;
begin
  Result := BoldIsA(CompareElement);
  if (not Result) and (CompareElement is TBoldListTypeInfo) then
    Result := not assigned(TBoldListTypeInfo(CompareElement).ListElementTypeInfo) or
    BoldIsA(TBoldListTypeInfo(CompareElement).ListElementTypeInfo);
end;

{--- TBoldNilTypeInfo ---}

function TBoldNilTypeInfo.ConformsTo(CompareElement: TBoldElementTypeInfo): Boolean;
begin
  result := CompareElement is TBoldClassTypeInfo;
end;

function TBoldNilTypeInfo.BoldIsA(C2: TBoldElementTypeInfo): Boolean;
begin
  result := ConformsTo(c2);
end;

function TBoldNilTypeInfo.GetStringRepresentation(Representation: TBoldRepresentation): string;
begin
  result := 'nil';
end;

{---TBoldMemberRTInfo---}
constructor TBoldMemberRTInfo.Create(ClassTypeInfo: TBoldClassTypeInfo; moldMember: TMoldMember; TypeNameDictionary: TBoldTypeNameDictionary);
var
  MoldClass: TMoldClass;
  Expressions: TStringList;
  tempExpr: string;
  i: integer;
begin
  inherited Create(moldMember, MoldMember.Name, MoldMember.ExpandedExpressionName, MoldMember.ExpandedDelphiName, ClassTypeInfo.SystemTypeInfo);
  fStereotype := MoldMember.Stereotype;
  fVisibility := MoldMember.Visibility;
  fDispId := MoldMember.DispId;
  SetElementFlag(befIsDerived, MoldMember.Derived);
  SetElementFlag(befIsReverseDerived, MoldMember.ReverseDerived);
  SetElementFlag(befPersistent, moldMember.EffectivePersistent);
  fClassTypeInfo := ClassTypeInfo;
  SetElementFlag(befDelayedFetch, moldMember.EffectiveDelayedFetch);
  fIndex := ClassTypeInfo.AllMembers.Count;
  fEmbeddedLinkIndex := -1;
  fDeriverIndex := -1;
  ClassTypeInfo.AllMembers.Add(self);
  if IsRole then
    ClassTypeInfo.AllRoles.Add(Self);  
  if isDerived then
  begin
    fDeriverIndex := ClassTypeInfo.fDerivedMemberCount;
    inc(ClassTypeInfo.fDerivedMemberCount);  
    fDeriveExpression := MoldMember.DerivationOCL;

    if DeriveExpression <> '' then
    begin
      MoldClass := MoldMember.Model.Classes[ClassTypeInfo.topSortedIndex];
      Expressions := TStringList.Create;
      while assigned(MoldClass) and (MoldClass <> MoldMember.MoldClass) do
      begin
        Expressions.Text := MoldClass.BoldTVByName[TAG_DERIVATIONEXPRESSIONS];
        for i := 0 to Expressions.Count - 1 do
          Expressions[i] := trim(Expressions[i]);

        TempExpr := trim(Expressions.Values[ModelName]);
        if tempExpr <> '' then
        begin
          fDeriveExpression := TempExpr;
          break;
        end;
        MoldClass := MoldClass.SuperClass;
      end;
      Expressions.Free;
    end;
  end;
end;

constructor TBoldMemberRTInfo.CreateWithoutMoldMember(ClassTypeInfo: TBoldClassTypeInfo; const ModelName, ExpressionName, DelphiName: string; Persistent: Boolean; TypeNameDictionary: TBoldTypeNameDictionary);
  function EnsureLowerCaseLeadingCharacter(const ExpressionName: String): String;
  begin
    result := ExpressionName;
    if length(result) > 0 then
      result[1] := LowerCase(result[1])[1];
  end;
begin
  inherited Create(nil, ModelName, EnsureLowerCaseLeadingCharacter(ExpressionName), DelphiName, ClassTypeINfo.SystemTypeInfo);
  SetElementFlag(befPersistent, Persistent);
  fClassTypeInfo := ClassTypeInfo;
  FIndex := ClassTypeInfo.AllMembers.Count;
  fEmbeddedLinkIndex := -1;
  fDeriverIndex := -1;
  ClassTypeInfo.AllMembers.Add(self);
  if IsRole then
    ClassTypeInfo.AllRoles.Add(Self);  
end;

destructor TBoldMemberRTInfo.Destroy;
begin
  assert(not assigned(ClassTypeInfo.AllMembers));
  inherited;
end;

function TBoldMemberRTInfo.GetBoldType: TBoldElementTypeInfo;
begin
  result := fBoldType;
end;

{---TBoldMethodRTInfo---}
constructor TBoldMethodRTInfo.Create(ClassTypeInfo: TBoldClassTypeInfo; moldMethod: TMoldMethod; Inheritedfrom: Integer; TypeNameDictionary: TBoldTypeNameDictionary);
begin
  inherited Create(MoldMethod, moldMethod.name, moldMethod.ExpandedExpressionName, moldMethod.ExpandedDelphiName, ClassTypeInfo.SystemTypeInfo);

  fVisibility := MoldMethod.Visibility;
  if MoldMethod.HasDispId then
    fDispId := moldMethod.DispId
  else
    fDispId := -1;
  fClassTypeInfo := ClassTypeInfo;
  FIsClassMethod := moldMethod.IsClassmethod;
  FReturnType := moldMethod.ReturnType;
  FFuncType := moldMethod.FuncType;
  fInheritedFrom := Inheritedfrom;
  ClassTypeInfo.Methods.Add(self);
  fSignature := moldMethod.Signature;
end;

function TBoldMethodRTinfo.ParameterList: TStringLIst;
begin
  if not assigned(fParameterList) then
    EnsurePArameterList(fSignature);
  result := fParameterList;
end;

function TBoldMethodRTInfo.GetSignature;
begin
  Result := BoldSeparateStringList(ParameterList, '; ', '', '');
end;

destructor TBoldMethodRTInfo.Destroy;
begin
  freeAndNil(fParameterList);
  inherited;  
end;

function TBoldMethodRTInfo.GetStringRepresentation(Representation: TBoldRepresentation): string;
begin
  Result := ClassTypeInfo.AsString + '.' + ExpressionName + '(' + signature + ')';
end;

function TBoldMethodRTInfo.getParameters(Number: Integer): string;
begin
  if Number >= ParameterList.Count then
    raise EBoldInternal.Create('Invalid argument to GetParameters');
  Result := Trim(Copy(ParameterList[Number], 1, Pos(':', ParameterList[Number]) - 1));
end;

function TBoldMethodRTInfo.getParameterTypes(Number: Integer): string;
begin
  if Number >= ParameterList.Count then
    raise EBoldInternal.Create('Invalid argument to GetParameterTypes');
  Result := Trim(Copy(ParameterList[Number], Pos(':', ParameterList[Number]) + 1, 255));
end;

function TBoldMethodRTInfo.getNumberOfParameters: Integer;
begin
  Result := ParameterList.Count;
end;

function TBoldMethodRTInfo.getIsInherited: Boolean;
begin
  Result := fInheritedFrom <> -1;
end;

procedure TBoldMethodRTInfo.EnsureParameterLIst(const Signature: string);
var
  tempSign: string;
  Parameter, pName, pType: string;
  pClass: TBoldClassTypeInfo;
begin
  if not assigned(fParameterList) then
  begin
    fParameterList := TStringList.Create;
    tempSign := Trim(Signature);
    if tempSign <> '' then
    begin
      if tempSign[Length(tempSign)] <> ';' then
        tempSign := tempSign + ';';
      while tempSign <> '' do
      begin
        Parameter := Copy(tempSign, 1, Pos(';', tempSign) - 1);
        pType := Trim(Copy(Parameter, Pos(':', Parameter) + 1, 255));
        pClass := ClassTypeInfo.SystemTypeInfo.ClassTypeInfoByModelName[pType];
        if assigned(pClass) then
          pType := pClass.Delphiname;
        while Pos(',', Parameter) <> 0 do
        begin
          pname := trim(copy(Parameter, 0, Pos(',', Parameter) - 1));
          fParameterList.Add(pName + ': ' + pType);
          Parameter := Trim(copy(Parameter, Pos(',', Parameter) + 1, length(parameter)));
        end;
        pName := Trim(Copy(Parameter, 0, Pos(':', Parameter) - 1));
        fParameterList.Add(pName + ': ' + pType);
        Delete(tempSign, 1, Pos(';', tempSign));
      end;
    end;
  end;
end;

{---TBoldRoleRTInfo---}
class procedure TBoldRoleRTInfo.SetPass2InfoForAssociation(SystemTypeInfo: TBoldSystemTypeInfo; moldAssociation: TMoldAssociation);

  procedure PropagateOtherEnd(aRole: TBoldRoleRTInfo; ClassOfOtherEnd: TBoldClassTypeInfo; RoleofOtherEnd: TBoldRoleRTInfo);
  begin
    if assigned(aRole) then
    begin

      aRole.fClassTypeInfoOfOtherEnd := ClassOfOtherEnd;
      aRole.fRoleRTInfoOfOtherEnd := RoleofOtherEnd;
      if assigned(RoleOfOtherEnd) then
      begin
        aRole.SetElementFlag(befOtherEndOrdered, RoleofOtherEnd.IsOrdered);
        aRole.SetElementFlag(befForceOtherEnd, RoleofOtherEnd.IsSingleRole or RoleofOtherEnd.IsOrdered or RoleofOtherEnd.IsStoredInObject);
        aRole.FIndexOfOtherEnd := RoleofOtherEnd.index;
      end else
        aRole.FIndexOfOtherEnd := -1;
      if aRole.IsMultiRole then
        aRole.SetBoldType(ClassOfOtherEnd.ListTypeInfo)
      else
        aRole.SetBoldType(ClassOfOtherEnd);
    end;
  end;

  procedure CopyRoleProperties(SourceRoleRTInfo, DestRoleRTInfo: TBoldRoleRTInfo);
  var
    Index: Integer;
  begin
    DestRoleRTInfo.fRoleRTInfoOfOtherEnd := SourceRoleRTInfo.RoleRTInfoOfOtherEnd;
    DestRoleRTInfo.FClassTypeInfoOfOtherEnd := SourceRoleRTInfo.ClassTypeInfoOfOtherEnd;
    DestRoleRTInfo.SetElementFlag(befOtherEndOrdered, SourceRoleRTInfo.OtherEndOrdered);
    DestRoleRTInfo.SetElementFlag(beFForceOtherEnd, SourceRoleRTInfo.ForceOtherEnd);
    DestRoleRTInfo.SetInternalState(BoldDefaultRegionModeMask, BoldDefaultRegionModeShift, integer(SourceRoleRTInfo.DefaultRegionMode));

    DestRoleRTInfo.FIndexOfOtherEnd := SourceRoleRTInfo.IndexOfOtherEnd;
    DestRoleRTInfo.FOwnIndexInLinkClass := SourceRoleRTInfo.OwnIndexInLinkClass;
    DestRoleRTInfo.FOtherIndexInLinkClass := SourceRoleRTInfo.OtherIndexInLinkClass;
    DestRoleRTInfo.FLinkClassTypeInfo := SourceRoleRTInfo.LinkClassTypeInfo;
    if Assigned(SourceRoleRTInfo.fQualifiers) then
    begin
      for Index := 0 to SourceRoleRTInfo.fQualifiers.Count - 1 do
      begin
        if not Assigned(DestRoleRTInfo.fQualifiers) then
        begin
          DestRoleRTInfo.fQualifiers := TBoldMemberRTInfoList.Create;
          DestRoleRTInfo.fQualifiers.OwnsEntries := false;
        end;

        DestRoleRTInfo.fQualifiers.Add(SourceRoleRTInfo.fQualifiers[Index]);
      end;
    end;
    DestRoleRTInfo.SetBoldType(SourceRoleRTInfo.BoldType);
  end;

  procedure PropagateToSubClasses(aRole, aLinkObjectRole: TBoldRoleRTInfo; moldClass: TMoldClass);
  var
    I: Integer;
    SubClass: TMoldClass;
    SubClassTypeInfo: TBoldClassTypeInfo;
    SubRole,
    SubLinkClassRole: TBoldRoleRTInfo;
  begin
    if assigned(aRole) {and assigned(aRole.OtherEnd)} then
      for I := 0 to moldClass.Subclasses.Count - 1 do
      begin
        SubClass := moldClass.Subclasses[I];
        SubClassTypeInfo := SystemTypeInfo.TopSortedClasses.ItemsByModelName[SubClass.name];
        Subrole := SubClassTypeInfo.AllMembers.ItemsByModelName[aRole.ModelName] as TBoldRoleRTInfo;
        CopyRoleProperties(aRole, SubRole);
        if Assigned(aLinkObjectRole) then
        begin
          SubLinkClassRole := SystemTypeInfo.TopSortedClasses.ItemsByModelName[SubClass.name].AllMembers.ItemsByModelName[aLinkObjectRole.ModelName] as TBoldRoleRTInfo;
          CopyRoleProperties(aLinkObjectRole, SubLinkClassRole);
        end;

        PropagateToSubClasses(aRole, aLinkObjectRole, SubClass);
      end;
  end;

var
  Role1, Role2: TBoldRoleRTInfo;
  LinkClassRole1, LinkClassRole2: TBoldRoleRTInfo;
  LinkClass: TBoldClassTypeInfo;
  LinkObjectRole1, LinkObjectRole2: TBoldRoleRTInfo;
  Class1, Class2: TBoldClassTypeInfo;
  tempMember: TBoldMemberRTInfo;
begin
  LinkObjectRole1 := nil;
  LinkObjectRole2 := nil;
  if not assigned(moldAssociation.Roles[0].moldClass) then
  begin
    SystemTypeInfo.InitializationError(sInvalidAssociation, [MoldAssociation.Name , MoldAssociation.Roles[0].Name]);
    exit;
  end;
  if not assigned(moldAssociation.Roles[1].moldClass) then
  begin
    SystemTypeInfo.InitializationError(sInvalidAssociation, [MoldAssociation.Name , MoldAssociation.Roles[1].Name]);
    exit;
  end;
   
  Class1 := SystemTypeInfo.ClassTypeInfoByModelName[moldAssociation.Roles[0].moldClass.name];
  TempMember := Class1.MemberRTInfoByModelName[moldAssociation.Roles[0].name];
  Assert(not assigned(TempMember) or (TempMember is TBoldRoleRTInfo));
  Role1 := TBoldRoleRTInfo(TempMember);

  Class2 := SystemTypeInfo.ClassTypeInfoByModelName[moldAssociation.Roles[1].moldClass.name];
  TempMember := Class2.MemberRTInfoByModelName[moldAssociation.Roles[1].name];
  Assert(not assigned(TempMember) or (TempMember is TBoldRoleRTInfo));
  Role2 := TBoldRoleRTInfo(TempMember);

  PropagateOtherEnd(Role1, class2, Role2);
  PropagateOtherEnd(Role2, class1, Role1);
  if Assigned(moldAssociation.LinkClass) then
  begin
    LinkClass := SystemTypeInfo.ClassTypeInfoByModelName[moldAssociation.LinkClass.name];
    if assigned(Role1) then
      Role1.FLinkClassTypeInfo := LinkClass;
    if assigned(Role2) then
      Role2.FLinkClassTypeInfo := LinkClass;
    LinkClassRole1 := LinkClass.MemberRTInfoByModelName[moldAssociation.Roles[0].name] as TBoldRoleRTInfo;
    LinkClassRole2 := LinkClass.MemberRTInfoByModelName[moldAssociation.Roles[1].name] as TBoldRoleRTInfo;

    LinkClassRole1.fOtherIndexInLinkClass := LinkClassRole2.Index;
    LinkClassRole1.fOwnIndexInLinkClass := LinkClassRole1.Index;

    LinkClassRole2.fOtherIndexInLinkClass := LinkClassRole1.Index;
    LinkClassRole2.fOwnIndexInLinkClass := LinkClassRole2.Index;

    PropagateOtherEnd(LinkClassRole1, class2, Role2);
    PropagateOtherEnd(LinkClassRole2, class1, role1);
    if assigned(Role1) then
    begin
      Role1.FOwnIndexInLinkClass := LinkClassRole2.index;
      Role1.FOtherIndexInLinkClass := LinkClassRole1.index;
      Role1.fRoleRTInfoOfOtherEnd := LinkClassRole2;
      LinkObjectRole1 := Role1.ClassTypeInfo.AllMembers[Role1.index + 1] as TBoldRoleRTInfo;
      PropagateOtherEnd(LinkObjectRole1, LinkClass, LinkClassRole2);
    end;

    if assigned(Role2) then
    begin
      Role2.FOwnIndexInLinkClass := LinkClassRole1.index;
      Role2.FOtherIndexInLinkClass := LinkClassRole2.index;
      Role2.fRoleRTInfoOfOtherEnd := LinkClassRole1;
      LinkObjectRole2 := Role2.ClassTypeInfo.AllMembers[Role2.index + 1] as TBoldRoleRTInfo;
      PropagateOtherEnd(LinkObjectRole2, LinkClass, LinkClassRole1);
    end;
  end;
  if assigned(Role1) then
    Role1.InitQualifiers(moldAssociation.Roles[0].Qualifiers);
  if assigned(Role2) then
    Role2.InitQualifiers(moldAssociation.Roles[1].Qualifiers);
    
  PropagateToSubClasses(Role1, LinkObjectRole1, moldAssociation.Roles[0].moldClass);
  PropagateToSubClasses(Role2, LinkObjectRole2, moldAssociation.Roles[1].moldClass);
end;

procedure TBoldRoleRTInfo.InitQualifiers(Qualifiers: TMoldQualifierList);
var
  i: Integer;
begin
  if Qualifiers.Count > 0 then
  begin
    FQualifiers := TBoldMemberRTInfoList.Create;
    fQualifiers.OwnsEntries := false;
    for i := 0 to Qualifiers.Count - 1 do
      FQualifiers.Add(ClassTypeInfoOfOtherEnd.AllMembers.ItemsByModelName[Qualifiers[i].Name]);
  end;
end;

function TBoldRoleRTInfo.GetDefaultRegionMode: TBoldAssociationEndDefaultRegionMode;
begin
  result := TBoldAssociationEndDefaultRegionMode(GetInternalState(BoldDefaultRegionModeMask, BoldDefaultRegionModeShift))
end;


constructor TBoldRoleRTInfo.Create(ClassTypeInfo: TBoldClassTypeInfo; MoldRole: TMoldRole; TypeNameDictionary: TBoldTypeNameDictionary);
var
  EmbeddedOtherEnd: TMoldRole;
begin
  inherited Create(ClassTypeInfo, MoldRole, TypeNameDictionary);
  SetElementFlag(befIsMultiRole, MoldRole.Multi);
  SetElementFlag(befIsSingleRole, not MoldRole.Multi);
  SetElementFlag(befQualifiedMulti, MoldRole.QualifiedMulti);
  if IsMultiRole then
    SetElementFlag(befDelayedFetch, True);

  SetElementFlag(befMandatory, MoldRole.Mandatory);
  SetElementFlag(befIsStoredInObject, MoldRole.EffectiveEmbedded and (not MoldRole.Multi) and Persistent);
  SetElementFlag(befIsOrdered, MoldRole.EffectiveOrdered);
  SetElementFlag(befIsNavigable, MoldRole.Navigable);
  SetElementFlag(befIsIndirect, Assigned(MoldRole.Association.LinkClass));
  SetElementFlag(befMemberToBeRemoved, MoldRole.Association.EvolutionState = esToBeRemoved);
  SetInternalState(BoldDefaultRegionModeMask, BoldDefaultRegionModeShift, integer(MoldRole.EffectiveDefaultRegionMode));
  if (IsStoredInObject) then
  begin
    fEmbeddedLinkIndex := ClassTypeInfo.EmbeddedSingleLinkCount;
    inc(ClassTypeInfo.fEmbeddedSingleLinkCount);
  end;
  if MoldRole.HasLinkRole then
    EmbeddedOtherEnd := MoldRole.LinkRole.OtherEnd
  else
    EmbeddedOtherEnd := MoldRole.OtherEnd;
  SetElementFlag(befIsNonVersionedInVersionedClass, (not MoldRole.EffectiveEmbedded) and
    MoldRole.MoldClass.Versioned and not EmbeddedOtherEnd.MoldClass.Versioned);
  fMultiplicity := MoldRole.Multiplicity;
  fRoleType := rtRole;
  fAggregation := MoldRole.Aggregation;
  fDeleteAction := MoldRole.EffectiveDeleteAction;
  fChangeability := MoldRole.Changeability;
  fAssociationStereotype := MoldRole.Association.Stereotype;

  if IsSingleRole then
  begin
    if IsIndirect then
      fStreamName := BoldContentName_ObjectIdRefPair
    else
      fStreamName := BoldContentName_ObjectIdRef;
  end
  else
    if IsIndirect then
      fStreamName := BoldContentName_ObjectIdListRefPair
    else
      fStreamName := BoldContentName_ObjectIdListRef;
end;

constructor TBoldRoleRTInfo.CreateInnerLinkRole(ClassTypeInfo: TBoldClassTypeInfo; MoldRole: TMoldRole; Dummy: Smallint; TypeNameDictionary: TBoldTypeNameDictionary);
begin
  inherited Create(ClassTypeInfo, MoldRole, TypeNameDictionary);
  fRoleType := rtInnerLInkRole;
  SetElementFlag(befIsMultiRole, False);
  SetElementFlag(befIsSingleRole, true);
  SetElementFlag(befIsStoredInObject, Persistent);
  SetElementFlag(befIsOrdered, False);
  SetElementFlag(befIsIndirect, False);
  SetElementFlag(befIsNavigable, MoldRole.Navigable);
  SetInternalState(BoldDefaultRegionModeMask, BoldDefaultRegionModeShift, integer(MoldRole.EffectiveDefaultRegionMode));
  if Persistent then
  begin
    fEmbeddedLinkIndex := ClassTypeInfo.EmbeddedSingleLinkCount;
    inc(ClassTypeInfo.fEmbeddedSingleLinkCount);
  end;
  fAggregation := akNone;
  fDeleteAction := daAllow;
  fChangeability := ckFrozen;
  fStreamName := BoldContentName_ObjectIdRef;
end;

constructor TBoldRoleRTInfo.CreateLinkObjectRole(ClassTypeInfo: TBoldClassTypeInfo; MainRole: TMoldRole; TypeNameDictionary: TBoldTypeNameDictionary; Dummy: smallint);
var
  aLinkClass: TMoldClass;
begin
  aLinkClass := MainRole.Association.LinkClass;
  if MainRole.MoldClass.ChildTo(MainRole.OtherEnd.MoldClass) or
     MainRole.OtherEnd.MoldClass.ChildTo(MainRole.MoldClass) then
    inherited CreateWithoutMoldMember(ClassTypeInfo,
                                      MainRole.name + aLinkClass.name,
                                      MainRole.ExpandedExpressionName + aLinkClass.ExpandedExpressionName,
                                      BoldExpandName(MainRole.ExpressionName, MainRole.Name, xtDelphi, -1, MainRole.Model.NationalCharConversion) +
                                      BoldExpandName(ALinkClass.ExpressionName, ALinkClass.Name, xtDelphi, -1, MainRole.Model.NationalCharConversion),
                                      false, TypeNameDictionary)
  else
    inherited CreateWithoutMoldMember(ClassTypeInfo,
                                      aLinkClass.name,
                                      aLinkClass.ExpandedExpressionName,
                                      BoldExpandName(ALinkClass.ExpressionName, ALinkClass.Name, xtDelphi, -1, MainRole.Model.NationalCharConversion),
                                      false, TypeNameDictionary);
  SetElementFlag(befIsMultiRole, MainRole.Multi);
  SetElementFlag(befIsSingleRole, not MainRole.Multi);
  if IsMultiRole then
    SetElementFlag(befDelayedFetch, True);
  SetElementFlag(befMandatory, MainRole.Mandatory);
  SetElementFlag(befIsStoredInObject, false);
  SetElementFlag(befIsOrdered, MainRole.EffectiveOrdered);
  SetElementFlag(befIsNavigable, MainRole.Navigable);
  SetElementFlag(befIsIndirect, false);
  SetInternalState(BoldDefaultRegionModeMask, BoldDefaultRegionModeShift, integer(aedrmNone));
  fRoleType := rtLinkRole;
  fAggregation := akNone;
  fDeleteAction := daAllow;
  fChangeability := MainRole.Changeability;
  if IsMultiRole then
    fStreamName := BoldContentName_ObjectIdRef
  else
    fStreamName := BoldContentName_ObjectIdListRef;
end;

function TBoldRoleRTInfo.GetStoreInUndo: boolean;
begin
  result := not IsDerived and (((RoleType = rtRole) and (not (IsMultiRole or IsIndirect))) or (RoleType = rtInnerLinkRole));
end;

function TBoldRoleRTInfo.GetStringRepresentation(Representation: TBoldRepresentation): string;
begin
  Result := ClassTypeInfo.AsString + '.' + ExpressionName;
end;

function TBoldRoleRTInfo.GetCanHaveOldValue: Boolean;
begin
  result := Persistent and (RoleType in [rtRole, rtInnerLinkRole]);
end;

procedure TBoldRoleRTInfo.SetForceOtherEnd;
begin
  SetElementFlag(befForceOtherEnd, true);
end;

function TBoldRoleRTInfo.GetEncouragesOptimisticLockingOnDeletedOnly: Boolean;
begin

  result := not IsStoredInObject and Persistent and (RoleType = rtRole);
end;

function TBoldRoleRTInfo.GetIsQualified: Boolean;
begin
  result := assigned(Qualifiers) and (Qualifiers.Count > 0);
end;

function TBoldRoleRTInfo.GetIsRole: Boolean;
begin
  result := true;
end;

function TBoldRoleRTInfo.GetQualifiers: TBoldMemberRTInfoList;
begin
  result := FQualifiers;
end;

function TBoldRoleRTInfo.GetMemberClass: TClass;
begin
  if IsMultiRole then
  begin
    Assert(BoldType is TBoldListTypeInfo);
    result := TBoldListTypeInfo(BoldType).ListClass;
  end
  else
    result := TBoldObjectReference;
end;

{---TBoldAttributeRTInfo---}

constructor TBoldAttributeRTInfo.Create(ClassTypeInfo: TBoldClassTypeInfo; MoldAttribute: TMoldAttribute; TypeNameDictionary: TBoldTypeNameDictionary);
var
  Mapping: TBoldTypeNameMapping;
begin
  inherited Create(ClassTypeInfo, MoldAttribute, TypeNameDictionary);
  SetElementFlag(befIsAttribute, true);
  SetElementFlag(befIsStoredInObject, Persistent);
  SetElementFlag(befAllowNull, MoldAttribute.AllowNull);
  SetElementFlag(befHasInitalvalue, fInitialvalue <> '');
  SetElementFlag(befMemberToBeRemoved, MoldAttribute.EvolutionState = esToBeRemoved);
  fLength := MoldAttribute.Length;
  fInitialvalue := MoldAttribute.InitialValue;
  if fInitialValue <> '' then
    SetElementFlag(befHasInitalvalue, true);
  Mapping := TypeNameDictionary.MappingForModelName[MoldAttribute.BoldType];
  if not assigned(Mapping) then
    ClassTypeInfo.SystemTypeInfo.InitializationError(sCannotFindAttributeMapping,
                          [MoldAttribute.MoldClass.ExpandedExpressionname,
                           MoldAttribute.ExpandedExpressionName,
                           MoldAttribute.BoldType])
  else
  begin
    fBoldType := ClassTypeInfo.SystemTypeInfo.AttributeTypeInfoByExpressionName[Mapping.expressionName];
    fStreamName := Mapping.ExpandedContentsName;
    if not assigned(fBoldType) then
      ClassTypeInfo.SystemTypeInfo.InitializationError(sUnableToFindBoldTypeForAttribute,
                            [MoldAttribute.MoldClass.ExpandedExpressionname,
                             MoldAttribute.ExpandedExpressionName,
                             Mapping.ExpressionName]);

    if assigned(fBoldtype) and
      not assigned((fBoldType as TBoldAttributeTypeInfo).AttributeClass) then
      ClassTypeInfo.SystemTypeInfo.InitializationError(sAttributeHasNoDelphiType,
                            [MoldAttribute.MoldClass.ExpandedExpressionname,
                             MoldAttribute.ExpandedExpressionName,
                             MoldAttribute.BoldType]);
  end;

end;

function TBoldAttributeRTInfo.GetStoreInUndo: boolean;
begin
  result := not IsDerived;
end;

function TBoldAttributeRTInfo.GetStringRepresentation(Representation: TBoldRepresentation): string;
begin
  Result := ClassTypeInfo.AsString + '.' + ExpressionName;
end;

function TBoldAttributeRTInfo.GetIsRole: Boolean;
begin
  result := false;
end;

function TBoldAttributeRTInfo.GetMemberClass: TClass;
begin
  Assert(BoldType is TBoldAttributeTypeInfo);
  result := TBoldAttributeTypeInfo(BoldType).AttributeClass;
end;

{---TBoldAttributeTypeInfo---}
constructor TBoldAttributeTypeInfo.Create(const ModelName, ExpressionName: string; AttributeClass: TClass; SuperType: TBoldAttributeTypeInfo; const FallBackDelphiName: String; SystemTypeInfo: TBoldSystemTypeInfo; IsAbstract: Boolean);
begin
  if not assigned(AttributeClass) then
    inherited Create(Modelname, ExpressionName, FallBackDelphiName, SystemTypeInfo)
  else
    inherited Create(Modelname, ExpressionName, AttributeClass.ClassName, SystemTypeInfo);

  fIsAbstract := IsAbstract;
  fAttributeClass := AttributeClass;
  SetValueType(bvtAttr);
  fSuperAttributeTypeInfo := SuperType;
end;

function TBoldAttributeTypeInfo.ElementClass: TBoldElementClass;
begin
  result := TBoldMemberClass(AttributeClass);
end;

function TBoldAttributeTypeInfo.CreateElement: TBoldElement;
begin
  result := TBoldMemberClass(AttributeClass).CreateWithTypeInfo(self);
end;

function TBoldAttributeTypeInfo.ConformsTo(CompareElement: TBoldElementTypeInfo): Boolean;
begin
  Result := CompareElement = self;
  if not Result then
  begin
    if CompareElement is TBoldAttributeTypeInfo then
      Result := assigned(AttributeClass) and AttributeClass.InheritsFrom(TBoldAttributeTypeInfo(CompareElement).AttributeClass)
    else if CompareElement is TBoldListTypeInfo then
      Result := not assigned(TBoldListTypeInfo(CompareElement).ListElementTypeInfo) or
      ConformsTo(TBoldListTypeInfo(CompareElement).ListElementTypeInfo);
    if not result and ((SameText(ExpressionName, 'integer') and
      (SameText(compareElement.ExpressionName, 'float') or
       SameText(compareElement.ExpressionName, 'currency')))
{$IFDEF DateTimeConformsToDate}
       or
      (SameText(ExpressionName, 'date') and
       SameText(compareElement.ExpressionName, 'datetime'))
       or
      (SameText(ExpressionName, 'datetime') and
       SameText(compareElement.ExpressionName, 'date'))
{$ENDIF}
       ) then
      result := true
  end;
end;

function TBoldAttributeTypeInfo.BoldIsA(aType: TBoldElementTypeInfo): Boolean;
begin
  result := (aType = Self ) or (aType is TBoldAttributeTypeInfo) and Assigned(SuperAttributeTypeInfo) and SuperAttributeTypeInfo.BoldIsA(aType);
end;

function TBoldRoleRTInfo.GetIndexOfLinkObjectRole: Integer;
begin
  if IsIndirect then
    result := Index + 1
  else
    result := -1;
end;

function TBoldRoleRTInfo.GetIndexOfMainRole: Integer;
begin
  if roleType = rtLInkRole then
    result := Index - 1
  else
    result := -1;
end;

{$IFDEF BOLD_LITE}
class function TBoldClassTypeInfo.NewInstance: TObject;
begin
  if G_TheSystemType.TopSortedClasses.Count >= LITE_VERSION_CLASS_LIMIT then
    raise EBold.Create('Class limit exceeded');

  result := InitInstance(addr(G_TheSystemType.fClassTypeInfoMem[G_TheSystemType.TopSortedClasses.Count *
                                                       CLASS_TYPE_INFO_MEM_SIZE]));
end;

procedure TBoldClassTypeInfo.FreeInstance;
begin
  CleanUpInstance;
end;
{$ENDIF}

procedure TBoldSystemTypeInfo.ReleaseEvaluator;
begin
  FreeAndNil(fEvaluator);
end;

destructor TBoldClassTypeInfo.destroy;
begin
  assert(not assigned(SystemTypeInfo.TopSortedClasses));
  FreeAndNil(fAllMembers);
  FreeAndNil(FAllRoles);
  FreeAndNil(fMethods);
  FreeAndNil(fSubClasssesBoldClassTypeInfoList);  
  inherited;
end;

function TBoldSystemTypeInfo.GetRootClassTypeInfo: TBoldClassTypeInfo;
begin
  result := TopSortedClasses[0];
end;

function TBoldClassTypeInfo.LeastCommonSuperClass(OtherClassTypeInfo: TBoldClassTypeInfo): TBoldClassTypeInfo;
begin
  if OtherClassTypeInfo.BoldIsA(Self) then
    Result := Self
  else if Assigned(SuperClassTypeInfo) then
    Result := SuperClassTypeInfo.LeastCommonSuperClass(OtherClassTypeInfo)
  else
    Result := nil;
end;

function TBoldSystemTypeInfo.GetClassTypeInfoByModelName(
  const name: string): TBoldClassTypeInfo;
begin
  result := TopSortedClasses.ItemsByModelName[Name];
end;

function TBoldClassTypeInfo.GetMemberRTInfoByModelName(const Name: string): TBoldMemberRTInfo;
begin
  Result := FAllMembers.ItemsByModelName[Name];
end;

function TBoldClassTypeInfo.GetMultilinkCount: integer;
var
  i: integer;
begin
  result := 0;
  for i := 0 to AllRoles.Count -1 do
    if AllRoles[i].IsMultiRole then
      Inc(result);
end;

function TBoldClassTypeInfo.GetSinglelinkCount: integer;
var
  i: integer;
begin
  result := 0;
  for i := 0 to AllRoles.Count -1 do
    if AllRoles[i].IsSingleRole then
      Inc(result);
end;

function TBoldClassTypeInfo.GetAttributeCount: integer;
begin
  result := AllMembersCount - AllRolesCount;
end;

procedure TBoldClassTypeInfo.InitializeMultiplicityConstraints;
var
  upper, lower: integer;
  i: integer;
  RoleRT: TBoldRoleRTInfo;

  procedure AddMultiplicityConstraint(Role: TBoldRoleRTInfo; const ExprFragment: String; const moreless: String; limit: integer);
  var
    Constr: TBoldConstraintRTinfo;
  begin
    Constr := TBoldConstraintRTInfo.Create(nil,
      Role.ModelName + ' multiplicity constraint', '', '', // do not localize
      SystemTypeInfo,
      format('%s->size %s %d', [role.ExpressionName, ExprFragment, limit]), // do not localize
      format(sMultiplicityConstraintMessage,[role.ModelName, moreless, limit, role.ClassTypeInfoOfOtherEnd.ModelName]));
    AddConstraint(Constr);
  end;

begin
  for i := FirstOwnMemberIndex to AllMembers.Count - 1 do
  begin
    if AllMembers[i] is TBoldRoleRTInfo then
    begin
      RoleRT := TBoldRoleRtInfo(AllMembers[i]);
      if RoleRT.IsNavigable then
      begin
        upper := GetUpperLimitForMultiplicity(RoleRT.Multiplicity);
        lower := GetLowerLimitForMultiplicity(RoleRT.Multiplicity);

        if (upper > 1) and (upper < MaxInt) then
          AddMultiplicityConstraint(RoleRT, '<=', sAtMost, upper);
        if (lower > 0) then
          AddMultiplicityConstraint(RoleRT, '>=', sAtLeast, lower);
      end;
    end;
  end;
end;

procedure TBoldClassTypeInfo.Initialize(MoldClass: TMoldClass; TypeNameDictionary: TBoldTypeNameDictionary; BoldObjectClasses: TBoldGeneratedClassList; BoldObjectListClasses: TBoldGeneratedClassList; SkipMembers: Boolean);
var
  ListClass: TClass;
  ListClassDescriptor: TBoldGeneratedClassDescriptor;
  I: Integer;
  tempClass: TMoldClass;
  role: TMoldRole;
  RoleCount: integer;
  MethodCount: integer;
begin
  FTopSortedIndex := SystemTypeInfo.TopSortedClasses.Count;

  SetElementFlag(befIsAbstract, moldClass.IsAbstract);
  SetElementFlag(befIsLinkClass, Assigned(moldClass.Association));
  SetElementFlag(befIsImported, moldClass.Imported);
  SetElementFlag(befClassPersistent, moldClass.EffectivePersistent and
                                     SystemTypeInfo.Persistent);
  SetElementFlag(befClassToBeRemoved, MoldClass.EffectiveEvolutionState = esToBeRemoved);
  SetElementFlag(befVersioned, MoldClass.Versioned);
  SetElementFlag(befGenerateDefaultRegion, MoldClass.GenerateDefaultRegion);

  if assigned(moldClass.SuperClass) then
  begin
    fSuperClassTypeInfo := SystemTypeInfo.ClassTypeInfoByExpressionName[moldClass.SuperClass.ExpandedExpressionName];
    fSuperClassTypeInfo.SetElementFlag(befHasSubclasses, True);
    fSuperClassTypeInfo.SubClasssesBoldClassTypeInfoList.Add(Self);    
  end;
  SetObjectClass(BoldObjectClasses);

  fOptimisticLocking := MoldClass.EffectiveOptimisticLocking;

  SystemTypeInfo.TopSortedClasses.Add(self);

  ListClassDescriptor := BoldObjectListClasses.EntryByExpressionName[ExpressionName];

  if assigned(ListClassDescriptor) then
    ListClass := ListClassDescriptor.TheClass
  else
    ListClass := TBoldObjectList;

  fListTypeInfo := TBoldListTypeInfo.Create(Self, SystemTypeInfo, ListClass);
  SystemTypeInfo.ListTypes.Add(ListTypeInfo);

  FFirstOwnMemberIndex := MoldClass.FirstOwnBoldMemberIndex;

  if SkipMembers then
    exit;

  RoleCount := 0;
  for i := 0 to MoldClass.AllBoldMembers.Count - 1 do
  begin
    if not (MoldClass.AllBoldMembers[i] is TMoldAttribute) then
      Inc(RoleCount);
  end;
  fAllRoles.Capacity := RoleCount;
  FAllMembers.Capacity := MoldClass.AllBoldMembers.Count;
  
  for i := 0 to MoldClass.AllBoldMembers.Count - 1 do
  begin
    if MoldClass.AllBoldMembers[i] is TMoldAttribute then
      TBoldAttributeRTInfo.Create(self,
                                  TMoldAttribute(MoldClass.AllBoldMembers[i]),
                                  TypeNameDictionary)
    else
    begin
      Assert(MoldClass.AllBoldMembers[i] is TMoldRole);
      Role := TMoldRole(MoldClass.AllBoldMembers[i]);
      case Role.RoleType of
        rtRole: TBoldRoleRTInfo.Create(self, Role, TypeNameDictionary);
        rtLinkRole: TBoldRoleRTInfo.CreateLinkObjectRole(self, Role.MainRole, TypeNameDictionary, 0);
        rtInnerLinkRole: TBoldRoleRTInfo.CreateInnerLinkRole(self, Role, 0, TypeNameDictionary);
      end;
    end;
  end;
  fAllMembersCount := FAllMembers.Count;
  fAllRolesCount := FAllRoles.Count;

  fDefaultStringRepresentation := MoldClass.EffectiveDefaultStringRepresentation;


  MethodCount := 0;
  tempClass := moldClass;
  repeat
    MethodCount := MethodCount + tempClass.Methods.Count;
    tempClass := tempClass.SuperClass;
  until tempClass = nil;
  if MethodCount > 0 then  
    FMethods.Capacity := MethodCount;

  for I := 0 to moldClass.Methods.Count - 1 do
    TBoldMethodRTInfo.Create(self, moldClass.Methods[I], - 1, TypeNameDictionary);

  tempClass := moldClass.SuperClass;
  while assigned(tempClass) do
  begin
    for I := 0 to tempClass.Methods.Count - 1 do
    begin
      if not assigned(Methods.ItemsByExpressionName[tempClass.Methods[I].ExpandedExpressionName]) then
        TBoldMethodRTInfo.Create(self, tempClass.Methods[I], tempClass.TopSortedIndex, TypeNameDictionary);
    end;
    tempClass := tempClass.SuperClass;
  end;
  fPackagename := MoldClass.TVByName[BOLDINTERALTVPREFIX + TAG_UNFLATTENEDNAMESPACE];
end;

procedure TBoldNilTypeInfo.Initialize(MoldClass: TMoldClass; TypeNameDictionary: TBoldTypeNameDictionary; BoldObjectClasses: TBoldGeneratedClassList; BoldObjectListClasses: TBoldGeneratedClassList; SkipMembers: Boolean);
begin
  FTopSortedIndex := -1;
end;

function TBoldSystemTypeInfo.GetBoldType: TBoldElementTypeInfo;
begin
  result := TypeTypeInfo
end;

function TBoldClassTypeInfo.GetBoldType: TBoldElementTypeInfo;
begin
  result := SystemTypeInfo.TypeTypeInfo;
end;

function TBoldClassTypeInfo.GetDisplayName: String;
begin
  result := DelphiName;
end;

function TBoldClassTypeInfo.GetListTypeInfo: TBoldListTypeInfo;
begin
  result := fListTypeInfo;
end;

function TBoldMemberRTInfo.GetCanHaveOldValue: Boolean;
begin
  result := Persistent;
end;

function TBoldMemberRTInfo.GetDisplayName: String;
begin
  if Assigned(ClassTypeInfo) then
    result := ClassTypeInfo.Displayname + '.' + ExpressionName
  else
    result := inherited GetDisplayName;
end;

function TBoldMemberRTInfo.GetEncouragesOptimisticLockingOnDeletedOnly: Boolean;
begin
  result := false;
end;

function TBoldMemberRTInfo.GetIsRole: Boolean;
begin
  result := IsSingleRole or IsMultiRole;
end;

procedure TBoldMemberRTInfo.SetBoldType(BoldType: TBoldElementTypeInfo);
begin
  fBoldType := BoldType;
end;

function TBoldAttributeTypeInfo.GetBoldType: TBoldElementTypeInfo;
begin
  result := SystemTypeInfo.BoldType;
end;

function TBoldAttributeTypeInfo.GetListTypeInfo: TBoldListTypeInfo;
begin
  if not Assigned(fListTypeInfo) then
    fListTypeInfo := TBoldSystemTypeInfo(SystemTypeInfo).ListTypes.ItemByElement[self];
  result := fListTypeInfo;
end;

procedure TBoldSystemTypeInfo.InstallAttributeType(
  TypeNameDictionary: TBoldTypeNameDictionary; pos: integer);
var
  SuperClassName: String;
  j: integer;
  MemberClass: TClass;
  SuperDescriptor, AttributeDescriptor: TBoldMemberTypeDescriptor;
  TempAttributeType,
  SuperType: TBoldAttributeTypeInfo;
  SuperMapping, Mapping: TBoldTypeNameMapping;
  IsAbstract: Boolean;
begin
  Mapping := TypeNameDictionary.mapping[Pos];

  TempAttributeType := AttributeTypeInfoByExpressionName[Mapping.ExpressionName];

  if assigned(tempAttributeType) then
  begin
    if assigned(TempAttributeType.AttributeClass) and
      (AnsiCompareText(TempAttributeType.AttributeClass.ClassName, Mapping.ExpandedDelphiName) <> 0) then
      InitializationError(sErrorInstallingAttribute, [TypeNameDictionary.Mapping[pos].ExpressionName, TempAttributeType.AttributeClass.ClassName]);
    exit;
  end;

  Attributedescriptor := BoldMemberTypes.DescriptorByDelphiName[Mapping.ExpandedDelphiName];

  if assigned(AttributeDescriptor) then
  begin
    MemberClass := Attributedescriptor.MemberClass;
    if MemberClass.ClassParent <> TBoldAttribute then
    begin
      SuperClassName := MemberClass.ClassParent.ClassName;
      SuperType := AttributeTypeInfoByDelphiName[SuperClassName];
      if not assigned(SuperType) then
      begin
        for j := pos + 1 to TypeNameDictionary.Count - 1 do
          if AnsiCompareText(TypeNameDictionary.Mapping[j].ExpandedDelphiName, SuperClassName) = 0 then
            InstallAttributeType(TypeNameDictionary, j);
        SuperType := AttributeTypeInfoByDelphiName[SuperClassName];
        if not assigned(SuperType) then
        begin
          SuperDescriptor := BoldMemberTypes.DescriptorByDelphiName[SuperClassName];
          if not assigned(SuperDescriptor) then
            InitializationError(sErrorInstallingAttribute_MissingSuperType, [TypeNameDictionary.Mapping[pos].ExpressionName, SuperClassName])
          else
          begin
            SuperMapping := TypeNameDictionary.AddMapping;
            SuperMapping.DelphiName := SuperClassName;
            SuperMapping.ExpressionName := '__'+SuperClassName;
            SuperMapping.ModelName := '__'+SuperClassName;
            InstallAttributeType(TypeNameDictionary, TypeNameDictionary.Count-1);
            SuperType := AttributeTypeInfoByDelphiName[SuperClassName];
          end;
        end;
      end;
    end
    else
      SuperType := nil;
    isAbstract := Attributedescriptor.AbstractionLevel = alAbstract;
  end
  else
  begin
    MemberClass := nil;
    SuperType := nil;
    IsAbstract := True;
  end;
  TempAttributeType := TBoldAttributeTypeInfo.Create(
      Mapping.ModelName,
      Mapping.ExpressionName,
      MemberClass, SuperType, Mapping.ExpandedDelphiName, self, IsAbstract);
  AttributeTypes.Add(TempAttributeType);
  ListTypes.Add(TBoldListTypeInfo.Create(TempAttributeType, self, TBoldMemberList));
end;

function TBoldSystemTypeInfo.GetInitializationLog: TStringList;
begin
  if not Assigned(fInitializationLog) then
    fInitializationLog := TStringList.Create;
  Result := fInitializationLog;
end;

procedure TBoldSystemTypeInfo.InitializationError(const Message: String;
  args: array of const);
begin
  InitializationLog.Add(Format(Message, Args));
  SetElementFlag(befSystemIsRunnable, false);
end;

{ TBoldElementTypeInfoWithConstraint }

procedure TBoldElementTypeInfoWithConstraint.AddConstraint(
  Constraint: TBoldConstraintRTInfo);
begin
  if not assigned(fConstraints) then
  begin
    fConstraints := TBoldConstraintRTInfoList.Create;
    fConstraints.OwnsEntries := true;
  end;
  fConstraints.Add(Constraint);
end;

constructor TBoldElementTypeInfoWithConstraint.Create(MoldElement: TMoldElement; SystemTypeInfo: TBoldSystemTypeInfo);
var
  i: integer;
  Constraint: TBoldConstraintRTInfo;
  ConstraintName: String;
begin
  if assigned(MoldElement) then
    inherited Create(MoldElement.Name, MoldElement.ExpandedExpressionName, MoldElement.ExpandedDelphiname, SystemTYpeInfo)
  else
    inherited Create('', '', '', SystemTYpeInfo);

  if assigned(MoldElement) then
  begin
    fTaggedValues := MoldElement.GetAllNonBoldTaggedValues;
    if MoldElement.Constraints.Count > 0 then
    begin
      for i := 0 to MoldElement.Constraints.Count - 1 do
      begin
        constraintName := MoldElement.Constraints.Names[i];
        Constraint := TBoldConstraintRTInfo.Create(MoldElement,
          constraintName, '', '', SystemTypeInfo,
          MoldElement.Constraints.Values[constraintName], '');
        AddConstraint(Constraint);
      end;
    end;
  end;
end;

destructor TBoldElementTypeInfoWithConstraint.Destroy;
begin
  FreeAndNil(fTaggedValues);
  FreeAndNil(fConstraints);
  inherited;
end;

function TBoldElementTypeInfoWithConstraint.GetConstraintByIndex(Index: integer): TBoldConstraintRTInfo;
begin
  if assigned(fConstraints) then
    result := fConstraints[index]
  else
    result := nil;
end;

function TBoldElementTypeInfoWithConstraint.GetConstraintCount: integer;
begin
  if assigned(fConstraints) then
    result := fConstraints.Count
  else
    result := 0;
end;

function TBoldElementTypeInfoWithConstraint.GetConstraints(const Name: String): TBoldConstraintRTInfo;
begin
  if assigned(fConstraints) then
    result := fConstraints.ItemsByModelName[Name]
  else
    result := nil;
end;

function TBoldElementTypeInfoWithConstraint.GetListTypeInfo: TBoldListTypeInfo;
begin
  Result := TBoldSystemTypeInfo(SystemTypeInfo).ListTypes.ItemByElement[self];
end;

function TBoldElementTypeInfoWithConstraint.GetTaggedValueByIndex(Index: integer): string;
begin
  if assigned(fTaggedValues) then
    result := fTaggedValues[index]
  else
    result := '';
end;

function TBoldElementTypeInfoWithConstraint.GetTaggedvalueCount: integer;
begin
  if assigned(fTaggedValues) then
    result := fTaggedValues.Count
  else
    result := 0;
end;

function TBoldElementTypeInfoWithConstraint.GetTaggedValues(const Tag: string): string;
begin
  if Assigned(fTaggedValues) then
    Result := fTaggedValues.Values[Tag]
  else
    Result := '';
end;

{ TBoldMetaElementWithConstraint }

procedure TBoldMetaElementWithConstraint.AddConstraint(
  Constraint: TBoldConstraintRTinfo);
begin
  if not assigned(fConstraints) then
  begin
    fConstraints := TBoldConstraintRTInfoList.Create;
    fConstraints.OwnsEntries := true;
  end;
  fConstraints.Add(Constraint);
end;

constructor TBoldMetaElementWithConstraint.Create(MoldElement: TMoldElement; const ModelName, ExpressionName, DelphiName: String; SystemTypeInfo: TBoldSystemTypeInfo);
var
  i: integer;
  Constraint: TBoldConstraintRTInfo;
  ConstraintName: String;
begin
  inherited Create(ModelName, ExpressionName, DelphiName);
  if assigned(Moldelement) then
  begin
    fTaggedValues := MoldElement.GetAllNonBoldTaggedValues;
    if MoldElement.Constraints.Count > 0 then
    begin
      for i := 0 to MoldElement.Constraints.Count - 1 do
      begin
        ConstraintName := MoldElement.Constraints.Names[i];
        Constraint := TBoldConstraintRTInfo.Create(nil,
          ConstraintName, '', '', SystemTypeINfo,
          MoldElement.Constraints.Values[ConstraintName], '');
        AddConstraint(Constraint);
      end;
    end;
  end;
end;

destructor TBoldMetaElementWithConstraint.Destroy;
begin
  FreeAndNil(fTaggedValues);
  FreeAndNil(fConstraints);
  inherited;  
end;

function TBoldMetaElementWithConstraint.GetConstraintByIndex(Index: integer): TBoldConstraintRTInfo;
begin
  if assigned(fConstraints) then
    result := fConstraints[index]
  else
    result := nil;
end;

function TBoldMetaElementWithConstraint.GetConstraintCount: integer;
begin
  if assigned(fConstraints) then
    result := fConstraints.Count
  else
    result := 0;
end;

function TBoldMetaElementWithConstraint.GetConstraints(const Name: String): TBoldConstraintRTInfo;
begin
  if assigned(fConstraints) then
    result := fConstraints.ItemsByModelName[Name]
  else
    result := nil;
end;

function TBoldMetaElementWithConstraint.GetTaggedValueByIndex(Index: integer): string;
begin
  if Assigned(fTaggedValues) then
    Result := fTaggedValues[Index]
  else
    Result := '';
end;

function TBoldMetaElementWithConstraint.GetTaggedvalueCount: integer;
begin
  if Assigned(fTaggedValues) then
    Result := fTaggedValues.Count
  else
    Result := 0;
end;

function TBoldMetaElementWithConstraint.GetTaggedValues(const Tag: string): string;
begin
  if Assigned(fTaggedValues) then
    Result := fTaggedValues.Values[Tag]
  else
    Result := '';
end;

{ TBoldConstraintRTInfoList }

function TBoldConstraintRTInfoList.GetEnumerator: TBoldConstraintRTInfoListTraverser;
begin
  result := CreateTraverser as TBoldConstraintRTInfoListTraverser;
end;

function TBoldConstraintRTInfoList.GetItem(index: Integer): TBoldConstraintRTInfo;
begin
  result := TBoldConstraintRTInfo(inherited Items[index]);
end;

function TBoldConstraintRTInfoList.GetItemByModelName(const ModelName: string): TBoldConstraintRTInfo;
begin
  result := TBoldConstraintRTInfo(inherited ItemsByModelName[ModelName]);
end;

function TBoldConstraintRTInfoList.TraverserClass: TBoldIndexableListTraverserClass;
begin
  result := TBoldConstraintRTInfoListTraverser;
end;

{ TBoldConstraintRTInfo }

constructor TBoldConstraintRTInfo.create(MoldElement: TMoldElement; const ModelName, ExpressionName, DelphiName: String; SystemTypeInfo: TBoldSystemTypeInfo; const Expression, Description: String);
begin
  inherited create(MoldElement, ModelName, ExpressionName, DelphiName, SystemTypeInfo);
  fExpression := Expression;
  fDescription := Description;
  if Description = '' then
    fDescription := ModelName;
  fBoldType := SystemTYpeInfo.TypeTypeInfo;
end;

function TBoldConstraintRTInfo.GetBoldType: TBoldElementTypeInfo;
begin
  result := fBoldType;
end;

destructor TBoldRoleRTInfo.Destroy;
begin
  FreeAndNil(FQualifiers);
  inherited;
end;

function TBoldMethodRTInfo.GetBoldType: TBoldElementTypeInfo;
begin
  result := ClassTypeInfo.SystemTypeInfo.ElementTypeInfoByExpressionName[ReturnType];
end;         

function TBoldClassTypeInfo.GetQualifiedName: string;
begin
  if (fPackageName <> '') then
    result := fPackagename + '.' + ModelName
  else
    result := ModelName  ;
end;

{---TBoldRoleRTInfoList---}

function TBoldRoleRTInfoList.GetEnumerator: TBoldRoleRTInfoListTraverser;
begin
  result := CreateTraverser as TBoldRoleRTInfoListTraverser;
end;

function TBoldRoleRTInfoList.GetItem(index: Integer): TBoldRoleRTInfo;
begin
  Result := TBoldRoleRTInfo(inherited Items[index]);
end;

function TBoldRoleRTInfoList.GetItemByModelName(const ModelName: string): TBoldRoleRTInfo;
begin
  Result := TBoldRoleRTInfo(inherited ItemsByModelName[ModelName]);
end;

function TBoldRoleRTInfoList.TraverserClass: TBoldIndexableListTraverserClass;
begin
  result := TBoldRoleRTInfoListTraverser;
end;

function TBoldRoleRTInfoList.GetItemByExpressionName(const ExpressionName: string): TBoldRoleRTInfo;
begin
  Result := TBoldRoleRTInfo(inherited ItemsByExpressionName[ExpressionName]);
end;

{ TListClassIndex }

function TListClassIndex.ItemAsKeyClass(Item: TObject): TClass;
begin
  Result := TBoldListTypeInfo(Item).ListClass;
end;

{ TBoldMemberRTInfoListTraverser }

function TBoldMemberRTInfoListTraverser.GetCurrent: TBoldMemberRTInfo;
begin
  result := inherited GetItem as TBoldMemberRTInfo;
end;

{ TBoldClassTypeInfoListTraverser }

function TBoldClassTypeInfoListTraverser.GetCurrent: TBoldClassTypeInfo;
begin
  result := inherited GetItem as TBoldClassTypeInfo;
end;

{ TBoldAttributeTypeInfoListTraverser }

function TBoldAttributeTypeInfoListTraverser.GetCurrent: TBoldAttributeTypeInfo;
begin
  result := inherited GetItem as TBoldAttributeTypeInfo;
end;

{ TBoldRoleRTInfoListTraverser }

function TBoldRoleRTInfoListTraverser.GetCurrent: TBoldRoleRTInfo;
begin
  result := inherited GetItem as TBoldRoleRTInfo;
end;

{ TBoldConstraintRTInfoListTraverser }

function TBoldConstraintRTInfoListTraverser.GetCurrent: TBoldConstraintRTInfo;
begin
  result := inherited GetItem as TBoldConstraintRTInfo;
end;

{ TBoldMethodRTInfoListTraverser }

function TBoldMethodRTInfoListTraverser.GetCurrent: TBoldMethodRTInfo;
begin
  result := inherited GetItem as TBoldMethodRTInfo;
end;

initialization
{$IFDEF BOLD_LITE}
  if CLASS_TYPE_INFO_MEM_SIZE <> TBoldClassTypeInfo.InstanceSize then
    Raise EBold.Create('CLASS_TYPE_INFO_MEM_SIZE <> TBoldClassTypeInfo.InstanceSize');
{$ENDIF}
  TBoldClassTypeInfoList.IX_ObjectClass := -1;
  TBoldListTypeInfoList.IX_Element := -1;
  TBoldListTypeInfoList.IX_ListClass := -1;

end.
