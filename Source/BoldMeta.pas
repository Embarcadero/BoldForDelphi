
{ Global compiler directives }
{$include bold.inc}
unit BoldMeta;

interface

uses
  Classes,
  BoldDefs,
  BoldBase,
  BoldContainers,
  BoldNamedValueList,
  BoldUMLTypes,
  BoldTypeNameDictionary,
  BoldTaggedValueList,
  BoldTaggedValueSupport,
  BoldIndexableList,
  BoldHashIndexes,
  BoldDefaultTaggedValues;

const
  BOLD_FIRSTUSERDISPID=1024;
  BOLD_DISPID_ROUND=256;

type
  {forward declarations, classes in actual model}
  TMoldModel = class;
  TMoldClass = class;
  TMoldClassList = class;
  TMoldElement = class;
  TMoldElementList = class;
  TMoldMember = class;
  TMoldMemberList = class;
  TMoldAttribute = class;
  TMoldAttributeList = class;
  TMoldMethod = class;
  TMoldMethodList = class;
  TMoldRole = class;
  TMoldRoleList = class;
  TMoldAssociation = class;
  TMoldAssociationList = class;
  TMoldQualifier = class;
  TMoldQualifierList = class;
  TMoldComponent = class;
  TMoldComponentList = class;

  TMoldElementListTraverser = class;
  TMoldClassListTraverser = class;
  TMoldMemberListTraverser = class;
  TMoldRoleListTraverser = class;
  TMoldAttributeListTraverser = class;
  TMoldAssociationListTraverser = class;
  TMoldMethodListTraverser = class;
  TMoldQualifierListTraverser = class;

  TMoldElementClass = class of TMoldElement;

  TBoldDispIdAssigningState = (dasNotStarted, dasAssigning, dasDone);
  TBoldDefaultDeleteActionArray = array[TAggregationKind] of TDeleteAction;
  TModelOnChange = procedure of object;

  {---TMoldElement---}
  TMoldElement = class(TBoldMemoryManagedObject)
  private
    FName: string;
    fConstraints: TStrings;
    fStereotype: String;
    fTaggedValues: TBoldNamedValueList;
    fBoldTaggedValues: TBoldNamedValueList;
    fDispId: integer;
    FDefaultBoldTVList: TBoldTaggedValueList;
    function GetExpandedDelphiName: string;
    procedure SetBoldTVByName(const Tag, value: string);
    function GetBoldTVByName(const Tag: string): string;
    function GetStdTVByName(const Tag: string): string;
    function GetDelphiName: string;
    function GetExpressionName: string;
    function GetPMapperName: string;
    procedure SetStdTVByName(const Tag, Value: string);
    function GetDispId: integer;
    function GetDefaultBoldTVList: TBoldTaggedValueList;
    function GetTVByName(const Tag: string): string;
    procedure SetTVByName(const Tag, Value: string);
    function GetTaggedValues: TBoldNamedValueList;
    function GetBoldTaggedValues: TBoldNamedValueList;
    function GetNonDefaultTaggedValuesCommaText: string;
    procedure SetNonDefaultTaggedValuesCommaText(const Value: string);
  protected
    function GetHasDispId: boolean; virtual;
    function GetModel: TMoldModel; virtual; abstract;
    procedure SetName(const S: string); virtual;
    function GetExpandedExpressionName: string; virtual;
    function GetEvolutionState: TBoldEvolutionState;
    function GetFormerNames: string;
    function GetUMLClassName: string; virtual; abstract;
    property UMLClassName: string read GetUMLClassName;
    property DefaultBoldTVList: TBoldTaggedValueList read GetDefaultBoldTVList;
    property TaggedValues: TBoldNamedValueList read GetTaggedValues;
    property BoldTaggedValues: TBoldNamedValueList read GetBoldTaggedValues;
  public
    constructor Create(Parent: TMoldElement; const Name: string); virtual;
    destructor Destroy; override;
    procedure NameChanged; virtual;
    function GetAllNonBoldTaggedValues: TStringList;
    procedure AddAllTaggedValues(Tags, Values: TStrings);
    property Name: string read FName write SetName;
    property DelphiName: string read GetDelphiName;
    property ExpandedDelphiName: string read GetExpandedDelphiName;
    property ExpressionName: string read GetExpressionName;
    property ExpandedExpressionName: string read GetExpandedExpressionName;
    property Model: TMoldModel read GetModel;
    property PMapperName: string read GetPMapperName;
    property Constraints: TStrings read fConstraints;
    property Stereotype: String read fStereotype write fStereotype;
    property BoldTVByName[const Tag: string]: string read GetBoldTVByName write SetBoldTVByName;
    property StdTVByName[const Tag: string]: string read GetStdTVByName write SetStdTVByName;
    property TVByName[const Tag: string]: string read GetTVByName write SetTVByName;
    property NonDefaultTaggedValuesCommaText: string read GetNonDefaultTaggedValuesCommaText write SetNonDefaultTaggedValuesCommaText;
    property HasDispId: boolean read GetHasDispId;
    property DispId: integer read GetDispId;
  end;

  {---TMoldModel---}
  TMoldModel = class(TMoldElement)
  private
    fLinkRolesEnsured: Boolean;
    fTopSorted: Boolean;
    FClasses: TMoldClassList;
    FAssociations: TMoldAssociationList;
    FRootClass: TMoldClass;
    fComponents: TMoldComponentList;
    fTypeNameDictionary: TBoldTypeNameDictionary;
    fDispIdAssigningState: TBoldDispIdAssigningState;
    fDefaultDeleteAction: TBoldDefaultDeleteActionArray;
    fIsDestroying: Boolean;
    function CalculateCRC: Cardinal;
    procedure SetRootClass(NewRootClass: TMoldClass);
    function GetBoldUnitName: string;
    function GetComponents: TMoldComponentList;
    function GetMainComponent: TMoldComponent;
    function GetInterfaceUses: string;
    function GetUseGlobalId: Boolean;
    function GetUseReadOnly: Boolean;
    function GetUseModelVersion: Boolean;
    function GetModelVersion: Integer;
    function GetUseTimestamp: Boolean;
    function GetUseXFiles: Boolean;
    function GetUseClockLog: Boolean;
    function GetImplementationUses: string;
    function GetGUID: string;
    function GetTypeLibVersion: String;
    function GetOptimisticLocking: TBoldOptimisticLockingMode;
    function GetUpdateWholeObjects: Boolean;
    function GetExpandedUnitName: string;
    procedure RemoveAssoc(MoldAssoc: TMoldAssociation; ClassList: TMoldClassList; AssocList: TMoldAssociationList);
    procedure RemoveClass(MoldClass: TMoldClass; ClassList: TMoldClassList; AssocList: TMoldAssociationList);
    function GetNationalCharConversion: TBoldNationalCharConversion;
    function GetDispIdsAssigned: Boolean;
    function GetDefaultDeleteAction(AggregationKind: TAggregationKind): TDeleteAction;
    procedure SetDefaultDeleteAction(AggregationKind: TAggregationKind; const Value: TDeleteAction);
    function GetRegionDefinitions: String;
    function GetGenerateDefaultRegions: Boolean;
  protected
    function GetModel: TMoldModel; override;
    procedure AssignDispIds;
    function GetUMLClassName: string; override;
  public
    constructor Create(Parent: TMoldElement; const Name: string); override;
    destructor Destroy; override;
    procedure UpdateMemberIndexes;
    function GetClassByName(const name: string): TMoldClass;
    function FindRoleByClassNameAndName(const boldclassName, roleName: string): TMoldRole;
    procedure EnsureLinkRoles;
    procedure EnsureTopSorted;
    function TrimRemoved: boolean;
    function CRC: string;
    procedure EnsureCRC;
    property Classes: TMoldClassList read FClasses;
    property Associations: TMoldAssociationList read FAssociations;
    property InterfaceUses: string read GetInterfaceUses;
    property ImplementationUses: string read GetImplementationUses;
    property RootClass: TMoldClass read FRootClass write SetRootClass;
    property Components: TMoldComponentList read GetComponents;
    property MainComponent: TMoldComponent read GetMainComponent;
    property UseXFiles: Boolean read GetUseXFiles;
    property UseTimestamp: Boolean read GetUseTimestamp;
    property UseGlobalId: Boolean read GetUseGlobalId;
    property UseReadOnly: Boolean read GetUseReadOnly;
    property UseModelVersion: Boolean read GetUseModelVersion;
    property ModelVersion: Integer read GetModelVersion;
    property UseClockLog: Boolean read GetUseClockLog;
    function FindComponent(const ComponentName: string): TMoldComponent;
    function RenameComponent(const OldComponentName, NewComponentName: string): TMoldComponent;
    function EnsureComponent(const ComponentName: string): TMoldComponent;
    property BoldUnitName: string read GetBoldUnitName;
    property ExpandedUnitName: string read GetExpandedUnitName;
    property GUID: string read GetGUID;
    property TypeLibVersion: String read GetTypeLibVersion;
    property OptimisticLocking: TBoldOptimisticLockingMode read GetOptimisticLocking;
    property UpdateWholeObjects: Boolean read GetUpdateWholeObjects;
    property TypeNameDictionary: TBoldTypeNameDictionary read fTypeNameDictionary write fTypeNameDictionary;
    property TopSorted: Boolean read fTopSorted;
    property DispIdsAssigned: Boolean read GetDispIdsAssigned;
    property NationalCharConversion: TBoldNationalCharConversion read GetNationalCharConversion;
    property DefaultDeleteAction[AggregationKind: TAggregationKind]: TDeleteAction read GetDefaultDeleteAction write SetDefaultDeleteAction;
    property RegionDefinitions: String read GetRegionDefinitions;
    property GenerateDefaultRegions: Boolean read GetGenerateDefaultRegions;
  end;

  {---TMoldElementList---}
  TMoldElementList = class(TBoldIndexableList)
  private
    class var IX_Name: integer;
    function GetItem(index: Integer): TMoldElement;
    function GetItemByName(const name: string): TMoldElement;
  protected
    function TraverserClass: TBoldIndexableListTraverserClass; override;
  public
    function GetEnumerator: TMoldElementListTraverser;
    constructor Create;
    procedure RemoveEntryReference(Item: TMoldElement);
    function MakeUniqueName(const prefix: string): string;
    procedure Add(MoldElement: TMoldElement);
    property Items[index: Integer]: TMoldElement read GetItem; default;
    property ItemsByName[const name: string]: TMoldElement read GetItemByName;
  end;

  {---TMoldClassList---}
  TMoldClassList = class(TMoldElementList)
  private
    class var IX_ExpressionName: integer;
    class var IX_DelphiName: integer;
    function GetItem(index: Integer): TMoldClass;
    function GetItemByName(const name: string): TMoldClass;
    function GetItemByExpressionName(const ExpressionName: String): TMoldClass;
    function GetItemByDelphiName(const DelphiName: String): TMoldClass;
  protected
    function TraverserClass: TBoldIndexableListTraverserClass; override;
  public
    function GetEnumerator: TMoldClassListTraverser;
    constructor Create;
    property Items[index: Integer]: TMoldClass read GetItem; default;
    property ItemsByName[const name: string]: TMoldClass read GetItemByName;
    property ItemsByExpressionName[const ExpressionName: String]: TMoldClass read GetItemByExpressionName;
    property ItemsByDelphiName[const DelphiName: String]: TMoldClass read GetItemByDelphiName;
  end;

  { TMoldMemberList }
  TMoldMemberList = class(TMoldElementList)
  private
    function GetItem(index: Integer): TMoldMember;
    function GetItemByName(const name: string): TMoldMember;
  protected
    function TraverserClass: TBoldIndexableListTraverserClass; override;
  public
    function GetEnumerator: TMoldMemberListTraverser;
    property Items[index: Integer]: TMoldMember read GetItem; default;
    property ItemsByName[const name: string]: TMoldMember read GetItemByName;
  end;

  {---TMoldRoleList---}
  TMoldRoleList = class(TMoldMemberList)
  private
    function GetItem(index: Integer): TMoldRole;
    function GetItemByName(const name: string): TMoldRole;
  protected
    function TraverserClass: TBoldIndexableListTraverserClass; override;
  public
    function GetEnumerator: TMoldRoleListTraverser;
    property Items[index: Integer]: TMoldRole read GetItem; default;
    property ItemsByName[const name: string]: TMoldRole read GetItemByName;
  end;

  {---TMoldMethodList---}
  TMoldMethodList = class(TMoldElementList)
  private
    function GetItem(index: Integer): TMoldMethod;
    function GetItemByName(const name: string): TMoldMethod;
  protected
    function TraverserClass: TBoldIndexableListTraverserClass; override;
  public
    function GetEnumerator: TMoldMethodListTraverser;
    property Items[index: Integer]: TMoldMethod read GetItem; default;
    property ItemsByName[const name: string]: TMoldMethod read GetItemByName;
  end;

  {---TMoldAssociationList---}
  TMoldAssociationList = class(TMoldElementList)
  private
    function GetItem(index: Integer): TMoldAssociation;
    function GetItemByName(const name: string): TMoldAssociation;
  protected
    function TraverserClass: TBoldIndexableListTraverserClass; override;
  public
    function GetEnumerator: TMoldAssociationListTraverser;
    property Items[index: Integer]: TMoldAssociation read GetItem; default;
    property ItemsByName[const name: string]: TMoldAssociation read GetItemByName;
  end;

  {---TMoldAttributeList---}
  TMoldAttributeList = class(TMoldMemberList)
  private
    function GetItem(index: Integer): TMoldAttribute;
    function GetItemByName(const name: string): TMoldAttribute;
  protected
    function TraverserClass: TBoldIndexableListTraverserClass; override;
  public
    function GetEnumerator: TMoldAttributeListTraverser;
    property Items[index: Integer]: TMoldAttribute read GetItem; default;
    property ItemsByName[const name: string]: TMoldAttribute read GetItemByName;
  end;

  {---TMoldQualifierList---}
  TMoldQualifierList = class(TMoldElementList)
  private
    function GetItem(index: Integer): TMoldQualifier;
    function GetItemByName(const name: string): TMoldQualifier;
  protected
    function TraverserClass: TBoldIndexableListTraverserClass; override;
  public
    function GetEnumerator: TMoldQualifierListTraverser;
    property Items[index: Integer]: TMoldQualifier read GetItem; default;
    property ItemsByName[const name: string]: TMoldQualifier read GetItemByName;
  end;

  TMoldElementListTraverser = class(TBoldIndexableListTraverser)
  public
    function GetCurrent: TMoldElement;
    property Current: TMoldElement read GetCurrent;
  end;

  TMoldClassListTraverser = class(TBoldIndexableListTraverser)
  public
    function GetCurrent: TMoldClass;
    property Current: TMoldClass read GetCurrent;
  end;

  TMoldMemberListTraverser = class(TBoldIndexableListTraverser)
  public
    function GetCurrent: TMoldMember;
    property Current: TMoldMember read GetCurrent;
  end;

  TMoldRoleListTraverser = class(TBoldIndexableListTraverser)
  public
    function GetCurrent: TMoldRole;
    property Current: TMoldRole read GetCurrent;
  end;

  TMoldAttributeListTraverser = class(TBoldIndexableListTraverser)
  public
    function GetCurrent: TMoldAttribute;
    property Current: TMoldAttribute read GetCurrent;
  end;

  TMoldAssociationListTraverser = class(TBoldIndexableListTraverser)
  public
    function GetCurrent: TMoldAssociation;
    property Current: TMoldAssociation read GetCurrent;
  end;

  TMoldMethodListTraverser = class(TBoldIndexableListTraverser)
  public
    function GetCurrent: TMoldMethod;
    property Current: TMoldMethod read GetCurrent;
  end;

  TMoldQualifierListTraverser = class(TBoldIndexableListTraverser)
  public
    function GetCurrent: TMoldQualifier;
    property Current: TMoldQualifier read GetCurrent;
  end;


  {---TMoldClass---}
  TMoldClass = class(TMoldElement)
  private
    FModel: TMoldModel;
    FSuperClass: TMoldClass;
    FSubClasses: TMoldClassList;
    FAttributes: TMoldAttributeList;
    FMethods: TMoldMethodList;
    FAssociation: TMoldAssociation;
    FRoles: TMoldRoleList;
    FIsAbstract: Boolean;
    fAllBoldMembers: TMoldMemberList;
    fAllNativeAttributes: TMoldAttributeList;
    fFirstOwnBoldMemberIndex: integer;
    fFirstOwnNativeAttributeIndex: integer;
    fAllAutoOverrideMethods: TMoldMethodList;
    fPersistent: Boolean;
    fLastDispId: integer;
    fAllPossibleNames: TStringList;
    fTopSortedIndex: integer;
    procedure SetSuperClass(super: TMoldClass);
    function GetIncFileName: string;
    function GetIsRootClass: Boolean;
    procedure InitializeClass(Parent: TMoldModel);
    procedure SetComponent(const Value: TMoldComponent);
    function GetAllNativeAttributes: TMoldAttributeList;
    function GetAllBoldMembers: TMoldMemberList;
    function GetIntroducesManuallyDerivedMembers: Boolean;
    function GetFirstOwnBoldMemberIndex: integer;
    function GetFirstOwnNativeAttributeIndex: integer;
    function GetHasManuallyDerivedMembers: Boolean;
    function GetAllAutoOverrideMethods: TMoldMethodList;
    function GetComponent: TMoldComponent;
    function GetTableMapping: TTableMapping;
    function GetBoldUnitName: string;
    function GetImported: Boolean;
    function GetTableName: string;
    function GetExternalTableName: string;
    procedure SetBoldUnitName(const Value: string);
    procedure SetIncFileName(const Value: string);
    function GetDefaultStringRepresentation: String;
    function GetEffectiveDefaultStringRepresentation: String;
    function GetHasCodeStubs: Boolean;
    function GetVersioned: Boolean;
    function GetGuid: String;
    function GetListGuid: String;
    function GetOptimisticLocking: TBoldOptimisticLockingMode;
    function GetExpandedUnitName: string;
    procedure TrimRemoved;
    function GetEffectiveEvolutionState: TBoldEvolutionState;
    function GetFirstDispId: integer;
    function GetLastDispId: integer;
    function GetAllPossibleNames: TStringList;
    function GetHasDelphiAttributesWithAccessorFunctions: Boolean;
    function CalculateCRC: Cardinal;
    function GetEffectiveOptimisticLocking: TBoldOptimisticLockingMode;
    function GetGenerateDefaultRegion: Boolean;
    function GetStorage: TBoldStorage;
    function GetIntroducesManuallyReverseDerivedMembers: Boolean;
  protected
    procedure AssignMemberDispIds;
    function GetModel: TMoldModel; override;
    function GetExpandedExpressionName: string; override;
    function GetExpandedInterfaceName: string;
    function GetEffectivePersistent: Boolean;
    function GetHasNotNullMembers: Boolean;
    function GetUMLClassName: string; override;
  public
    constructor Create(Parent: TMoldElement; const Name: string); override;
    destructor Destroy; override;
    function ChildTo(MoldClass: TMoldClass): Boolean;
    procedure NameChanged; override;
    function FindStoringClass(DefiningClass: TMoldClass; AbovedefiningClass: Boolean; Member: TMoldMember): TMoldClass;
    function LowestCommonSuperClass(otherClass: TMoldClass): TMoldClass;
    function LowestVisibleAncestor(MoldClass:TMoldClass): TMoldClass;
    function EffectiveIncFileName(const DefaultExtension: String): String;
    property TableMapping: TTableMapping read GetTablemapping;
    property TableName: string read GetTableName;
    property ExternalTableName: string read GetExternalTableName;
    property IsRootClass: Boolean read GetIsRootClass;
    property SuperClass: TMoldClass read FSuperClass write SetSuperClass;
    property SubClasses: TMoldClassList read FSubClasses;
    property Attributes: TMoldAttributeList read FAttributes;
    property Methods: TMoldMethodList read FMethods;
    property Roles: TMoldRoleList read FRoles;
    property Association: TMoldAssociation read FAssociation write fAssociation;
    property IsAbstract: Boolean read FIsAbstract write fIsAbstract;
    property Imported: Boolean read GetImported;
    property BoldUnitName: string read GetBoldUnitName write SetBoldUnitName;
    property ExpandedUnitName: string read GetExpandedUnitName;
    property IncFileName: string read GetIncFileName write SetIncFileName;
    property HasNotNullMembers: Boolean read GetHasNotNullMembers;
    property TopSortedIndex:integer read fTopSortedIndex;
    property Component:TMoldComponent read GetComponent write SetComponent;
    property AllBoldMembers: TMoldMemberList read GetAllBoldMembers;
    property AllNativeAttributes: TMoldAttributeList read GetAllNativeAttributes;
    property AllAutoOverrideMethods: TMoldMethodList read GetAllAutoOverrideMethods;
    property FirstOwnBoldMemberIndex:integer read GetFirstOwnBoldMemberIndex;
    property FirstOwnNativeAttributeIndex:integer read GetFirstOwnNativeAttributeIndex;
    property IntroducesManuallyDerivedMembers: Boolean read GetIntroducesManuallyDerivedMembers;
    property IntroducesManuallyReverseDerivedMembers: Boolean read GetIntroducesManuallyReverseDerivedMembers;
    property HasManuallyDerivedMembers: Boolean read GetHasManuallyDerivedMembers;
    property HasDelphiAttributesWithAccessorFunctions: Boolean read GetHasDelphiAttributesWithAccessorFunctions;
    property EffectivePersistent: Boolean read GetEffectivePersistent;
    property Persistent: Boolean read fPersistent write fPersistent;
    property DefaultStringRepresentation: String read GetDefaultStringRepresentation;
    property EffectiveDefaultStringRepresentation: String read GetEffectiveDefaultStringRepresentation;
    property HasCodeStubs: Boolean read GetHasCodeStubs;
    property Versioned: Boolean read GetVersioned;
    property ExpandedInterfaceName: string read GetExpandedInterfaceName;
    property GUID: String read GetGuid;
    property ListGUID: String read GetListGuid;
    property LastDispId: integer read GetLastDispId;
    property FirstDispId: integer read GetFirstDispId;
    property OptimisticLocking: TBoldOptimisticLockingMode read GetOptimisticLocking;
    property EffectiveOptimisticLocking: TBoldOptimisticLockingMode read GetEffectiveOptimisticLocking;
    property EvolutionState: TBoldEvolutionState read GetEvolutionState;
    property FormerNames: String read GetFormerNames;
    property AllPossibleNames: TStringList read GetAllPossibleNames;
    property EffectiveEvolutionState: TBoldEvolutionState read GetEffectiveEvolutionState;
    property GenerateDefaultRegion: Boolean read GetGenerateDefaultRegion;
    property Storage: TBoldStorage read GetStorage;
  end;

  {---TMoldMember---}
  TMoldMember = class(TMoldElement)
  private
    FMoldClass: TMoldClass;
    fVisibility: TVisibilityKind;
    fIndex: integer;
    function GetDerived: Boolean; virtual; abstract;
    function GetReverseDerived: Boolean; virtual; abstract;
    function GetDerivationOCL: String;
    function GetMemberExists: boolean; virtual;
    function GetColumnName: string;
    function GetTypeStreamName: string; virtual; abstract;
  protected
    function GetEffectiveDelayedFetch: Boolean; virtual;
    function GetModel: TMoldModel; override;
    function GetExpandedExpressionName: string; override;
    function GetEffectivePersistent: Boolean; virtual; abstract;
    procedure SetMoldClass(NewMoldClass: TMoldClass); virtual;
    function GetMoldClass: TMoldClass;
    function GetManuallyDerived: Boolean;
    function GetStorage: TBoldStorage; virtual; abstract;
    function GetIsAttribute: boolean; virtual; abstract;
    function GetIsRole: boolean; virtual; abstract;
  public
    constructor Create(Parent: TMoldElement; const Name: string); override;
    property MoldClass: TMoldClass read GetMoldClass;
    property ColumnName: string read GetColumnName;
    property EffectiveDelayedFetch: Boolean read GetEffectiveDelayedFetch;
    property Visibility: TVisibilityKind read fVisibility write fVisibility;
    property Derived: Boolean read GetDerived;
    property ManuallyDerived: Boolean read GetManuallyDerived;
    property ReverseDerived: Boolean read GetReverseDerived;
    property DerivationOCL: String read GetDerivationOCL;
    property MemberExists: boolean read GetMemberExists;
    property EffectivePersistent: Boolean read GetEffectivePersistent;
    property TypeStreamName: string read GetTypeStreamName;
    property FormerNames: String read GetFormerNames;
    property Storage: TBoldStorage read GetStorage;
    property IsAttribute: boolean read GetIsAttribute;
    property IsRole: boolean read GetIsRole;
    property Index: integer read fIndex;
  end;

  {---TMoldAttribute---}
  TMoldAttribute = class(TMoldMember)
  private
    FDerived: Boolean;
    FBoldType: string;
    FInitialValue: String;
    function CalculateCRC: Cardinal;
    function GetDerived: Boolean; override;
    function GetReverseDerived: Boolean; override;
    function GetAllowNull: Boolean;
    function GetAttributeKind: TBoldAttributeKind;
    function GetDelphiPropertyRead: TDelphiPropertyAccessKind;
    function GetDelphiPropertyWrite: TDelphiPropertyAccessKind;
    function GetHasDelphiField: Boolean;
    function GetLength: Integer;
    function GetPersistent: Boolean;
    function GetTypeStreamName: string; override;
    function GetDefaultDBValue: string;
  protected
    function GetEffectivePersistent: Boolean; override;
    function GetEffectiveDelayedFetch: Boolean; override;
    function GetHasDispId: boolean; override;
    function GetUMLClassName: string; override;
    function GetStorage: TBoldStorage; override;
    function GetIsAttribute: boolean; override;
    function GetIsRole: boolean; override;
  public
    constructor Create(Parent: TMoldElement; const Name: string); override;
    destructor Destroy; override;
    procedure NameChanged; override;
    property AllowNull: Boolean read GetAllowNull;
    property Length: Integer read GetLength;
    property BoldType: string read FBoldType write fBoldType;
    property AttributeKind: TBoldAttributeKind read GetAttributeKind;
    property HasDelphiField: Boolean read GetHasDelphiField;
    property DelphiPropertyRead: TDelphiPropertyAccessKind read GetDelphiPropertyRead;
    property DelphiPropertyWrite: TDelphiPropertyAccessKind read GetDelphiPropertyWrite;
    property InitialValue: String read FInitialValue write FInitialValue;
    property Derived: Boolean read fDerived write fDerived;
    property Persistent: Boolean read GetPersistent;
    property ReverseDerived: Boolean read GetReverseDerived;
    property EvolutionState: TBoldEvolutionState read GetEvolutionState;
    property DefaultDBValue: string read GetDefaultDBValue;
  end;

  {---TMoldMethod---}
  TMoldMethod = class(TMoldElement)
  private
    FMoldClass: TMoldClass;
    FIsClassMethod: Boolean;
    FReturnType: string;
    fParameters: TBoldObjectArray;
    fVisibility: TVisibilityKind;
    procedure SetSignature(Value: string);
    function GetSignature: String;
    function GetParameters: TBoldObjectArray;
    function GetCallSignature: string;
    function GetFuncType: TDelphiFunctionType;
    function GetOverrideInAllSubclasses: Boolean;
    function GetDelphiReturnType: String;
    function GetHasReturnValue: Boolean;
    function GetCanCallInherited: Boolean;
  protected
    function GetModel: TMoldModel; override;
    function GetHasDispId: boolean; override;
    function GetUMLClassName: string; override;
  public
    constructor Create(Parent: TMoldElement; const Name: string); override;
    destructor Destroy; override;
    procedure NameChanged; override;
    property ReturnType: string read FReturnType write fReturnType;
    property DelphiReturnType: String read GetDelphiReturnType;
    property FuncType: TDelphiFunctionType read GetFuncType;
    property Signature: string read GetSignature write SetSignature;
    property CallSignature: string read GetCallSignature;
    property Parameters: TBoldObjectArray read GetParameters;
    property IsClassMethod: Boolean read FIsClassMethod write fIsClassMethod;
    property MoldClass: TMoldClass read FMoldClass;
    property Visibility: TVisibilityKind read fVisibility write fVisibility;
    property OverrideInAllSubclasses: Boolean read GetOverrideInAllSubclasses;
    property HasReturnValue: Boolean read GetHasReturnValue;
    property CanCallInherited: Boolean read GetCanCallInherited;
  end;

  { TMoldParameter }
  TMoldParameter= class(TBoldMemoryManagedObject)
  private
    FParameterType: String;
    FParameterName: String;
    FParameterKind: TBoldParameterDirectionKind;
    FIsConst: Boolean;
    fOwningMethod: TMoldMethod;
    function GetDelphiParameterType: String;
  public
    constructor Create(OwningMethod: TMoldMethod);
    property ParameterName: String read FParameterName write fParameterName;
    property ParameterType: String read FParameterType write fParameterType;
    property ParameterKind: TBoldParameterDirectionKind read FParameterKind write fParameterKind;
    property DelphiParameterType: String read GetDelphiParameterType;
    property IsConst: Boolean read FIsConst write FIsConst;
  end;

  {---TMoldRole---}
  TMoldRole = class(TMoldMember)
  private
    FAssociation: TMoldAssociation;
    FOrdered: Boolean;
    FNavigable: Boolean;
    FQualifiers: TMoldQualifierList;
    fRelatedRole: TMoldRole;
    fRoleType: TBoldRoleType;
    fAggregation: TAggregationKind;
    fChangeability: TChangeableKind;
    fMultiplicity: String;
    function CalculateCRC: Cardinal;
    function GetOtherEnd: TMoldRole;
    procedure SetAssociation(Association: TMoldAssociation);
    function GetHasLinkRole: Boolean;
    function GetLinkRole: TMoldRole;
    procedure EnsureLinkRole;
    function GetMainRole: TMoldRole;
    function GetMulti: Boolean;
    function GetDerived: Boolean; override;
    function GetReverseDerived: Boolean; override;
    function GetMandatory: Boolean;
    function GetEmbed: Boolean;
    function GetEffectiveEmbed: Boolean;
    function GetEffectiveOrdered: Boolean;
    function GetTypeStreamName: string; override;
    function GetQualifiedMulti: Boolean;
    function GetDefaultRegionMode: TBoldAssociationEndDefaultRegionMode;
    function GetEffectiveDefaultRegionMode: TBoldAssociationEndDefaultRegionMode;
  protected
    function GetEffectiveDelayedFetch: Boolean; override;
    function GetModel: TMoldModel; override;
    procedure SetMoldClass(NewMoldClass: TMoldClass); override;
    function GetEffectivePersistent: Boolean; override;
    function GetDeleteAction: TDeleteAction;
    function GetEffectiveDeleteAction: TDeleteAction;
    function GetMemberExists: boolean; override;
    function GetHasDispId: boolean; override;
    function GetUMLClassName: string; override;
    function GetStorage: TBoldStorage; override;
    function GetIsAttribute: boolean; override;
    function GetIsRole: boolean; override;
  public
    constructor Create(Parent: TMoldElement; const Name: string); override;
    destructor Destroy; override;
    procedure NameChanged; override;
    property Association: TMoldAssociation read FAssociation write setAssociation;
    property Multi: Boolean read GetMulti;
    property Navigable: Boolean read FNavigable write fNavigable;
    property OtherEnd: TMoldRole read GetOtherEnd;
    property Ordered: Boolean read FOrdered write fOrdered;
    property EffectiveOrdered: Boolean read GetEffectiveOrdered;
    property Mandatory: Boolean read GetMandatory;
    property Embed: Boolean read GetEmbed;
    property EffectiveEmbedded: Boolean read GetEffectiveEmbed;
    property MoldClass: TMoldClass read GetMoldClass write setMoldClass;
    property Qualifiers: TMoldQualifierList read FQualifiers;
    property QualifiedMulti: Boolean read GetQualifiedMulti;
    property HasLinkRole: Boolean read GetHasLinkRole;
    property LinkRole: TMoldRole read GetLinkRole;
    property MainRole: TMoldRole read GetMainRole;
    property RoleType: TBoldRoleType read fRoleType;
    property Aggregation: TAggregationKind read fAggregation write fAggregation;
    property Changeability: TChangeableKind read fChangeability write fChangeability;
    property Multiplicity: String read fMultiplicity write fMultiplicity;
    property DeleteAction: TDeleteAction read GetDeleteAction;
    property EffectiveDeleteAction: TDeleteAction read GetEffectiveDeleteAction;
    property DefaultRegionMode: TBoldAssociationEndDefaultRegionMode read GetDefaultRegionMode;
    property EffectiveDefaultRegionMode: TBoldAssociationEndDefaultRegionMode read GetEffectiveDefaultRegionMode;
  end;

  {---TMoldAssociation---}
  TMoldAssociation = class(TMoldElement)
  private
    fDerived: Boolean;
    FModel: TMoldModel;
    FRoles: TMoldRoleList;
    fLinkRoles: TMoldRolelist;
    fInnerLinkRoles: TMoldRoleList;
    FLinkClass: TMoldClass;
    fAllPossibleNames: TStringList;
    procedure SetLinkClass(Value: TMoldClass);
    function GetPersistent: Boolean;
    function GetAllPossibleNames: TStringList;
    function GetStorage: TBoldStorage;
  protected
    function GetModel: TMoldModel; override;
    function GetEffectivePersistent: Boolean;
    function GetUMLClassName: string; override;
  public
    constructor Create(Parent: TMoldElement; const Name: string); override;
    destructor Destroy; override;
    procedure NameChanged; override;
    property LinkClass: TMoldClass read FLinkClass write SetLinkClass;
    property Roles: TMoldRoleList read FRoles;
    property Derived: boolean read fDerived write fDerived;
    property Persistent: Boolean read GetPersistent;
    property EffectivePersistent: Boolean read GetEffectivePersistent;
    property EvolutionState: TBoldEvolutionState read GetEvolutionState;
    property FormerNames: String read GetFormerNames;
    property AllPossibleNames: TStringList read GetAllPossibleNames;
    property Storage: TBoldStorage read GetStorage;
  end;

  {---TMoldQualifier---}
  TMoldQualifier = class(TMoldElement)
  private
    FBoldType: string;
    FMoldRole: TMoldRole;
  protected
    function GetModel: TMoldModel; override;
    function GetUMLClassName: string; override;
  public
    constructor Create(Parent: TMoldElement; const Name: string); override;
    procedure NameChanged; override;
    property BoldType: string read FBoldType write fBoldType;
    property MoldRole: TMoldRole read FMoldRole;
  end;

  { TMoldComponent }
  TMoldComponent = class(TBoldMemoryManagedObject)
  private
    fDependencies: TMoldComponentList;
    fMoldModel: TMoldModel;
    fName: String;
  public
    constructor Create(MoldModel: TMoldModel);
    destructor Destroy; override;
    function DependentOf(Component: TMoldComponent): Boolean;
    procedure GetInterfaceDependencies(StringList: TStringList);
    procedure GetImplementationDependencies(StringList: TStringList);
    property Name: string read fName write fName;
    property Dependencies: TMoldComponentList read fDependencies;
  end;

  { TMoldComponentList }
  TMoldComponentList = Class(TList)
  private
    function GetItem(index: Integer): TMoldComponent;
    function GetItemByName(const name: string): TMoldComponent;
  public
    property Items[index: Integer]: TMoldComponent read GetItem; default;
    property ItemsByName[const name: string]: TMoldComponent read GetItemByName;
  end;

implementation

uses
  SysUtils,
  BoldCoreConsts,

  BoldDefaultStreamNames,
  BoldGuard,
  BoldMetaSupport,
  BoldNameExpander,
  BoldSharedStrings,
  BoldUMLTaggedValues,
  BoldUtils,
  BoldIndex;

type
  {---TNameIndex---}
  TNameIndex = class(TBoldCaseSensitiveStringHashIndex)
  protected
    function ItemAsKeyString(Item: TObject): string; override;
  end;

  TExpressionNameIndex = class(TBoldStringHashIndex)
  protected
    function ItemAsKeyString(Item: TObject): string; override;
  end;

  TDelphiNameIndex = class(TBoldStringHashIndex)
  protected
    function ItemAsKeyString(Item: TObject): string; override;
  end;


function CRCHash(const S: string): CARDINAL;
var
  i: integer;
begin
  Result := 0;
  for i := 1 to Length(S) do
  Result := ((Result shl 3) and 2147483647) or
            (Result shr (32-3)) xor ord(S[i]);
end;

function SumCRCs(CRC1, CRC2: Cardinal): Cardinal;
begin
  result := ((CRC1 shl 3) and 2147483647) or
             (CRC1 shr (32-3)) xor CRC2;
end;

{---TNameIndex---}
function TNameIndex.ItemAsKeyString(Item: TObject): string;
begin
  Result := TMoldElement(Item).name;
end;

{ TExpressionNameIndex }
function TExpressionNameIndex.ItemAsKeyString(Item: TObject): string;
begin
  Result := TMoldElement(Item).ExpandedExpressionName;
end;

{ TDelphiNameIndex }
function TDelphiNameIndex.ItemAsKeyString(Item: TObject): string;
begin
  Result := TMoldElement(Item).ExpandedDelphiName;
end;

{---TMoldElement---}
constructor TMoldElement.Create(Parent: TMoldElement; const Name: string);
begin
  inherited Create;
  fName := Name;
  fConstraints := TStringList.Create;
  fDispId := -1;
end;

function TMoldElement.GetExpandedDelphiName: string;
begin
  Result := BoldExpandName(DelphiName, name, xtDelphi, -1, Model.NationalCharConversion);
end;

function TMoldElement.GetExpandedExpressionName: string;
begin
  Result := BoldExpandName(ExpressionName, name, xtExpression, -1, Model.NationalCharConversion);
end;

procedure TMoldElement.SetName(const S: string);
begin
  if fName <> S then
  begin
    fName := S;
    NameChanged;
  end;
end;

{---TMoldModel---}
constructor TMoldModel.Create(Parent: TMoldElement; const Name: string);
begin
  DefaultDeleteAction[akNone] := daAllow;
  DefaultDeleteAction[akAggregate] := daProhibit;
  DefaultDeleteAction[akComposite] := daCascade;

  fLinkRolesEnsured := false;
  inherited Create(self, Name);
  FClasses := TMoldClassList.Create;
  FAssociations := TMoldAssociationList.Create;
  fDispIdAssigningState := dasNotStarted;

  BoldTVByName[TAG_DELPHINAME] := TV_NAME;

  FRootClass := TMoldClass.Create(self, Format('%sRoot',[name]));

end;

destructor TMoldModel.Destroy;
var
  i: integer;
begin
  fIsDestroying := true;
  FreeAndNil(fAssociations);
  FreeAndNil(fClasses);
  if assigned(fComponents) then
    for i := 0 to fComponents.Count-1 do
      fComponents[i].Free;
  FreeAndNil(fComponents);
  inherited;
end;

function TMoldModel.GetModel: TMoldModel;
begin
  Result := self;
end;

function TMoldClassList.GetItemByName(const name: string): TMoldClass;
begin
  Result := TMoldClass(TBoldCaseSensitiveStringHashIndex(Indexes[IX_Name]).FindByString(name));
end;

function TMoldClassList.TraverserClass: TBoldIndexableListTraverserClass;
begin
  result := TMoldClassListTraverser;
end;

function TMoldModel.FindRoleByClassNameAndName(const boldclassName, roleName: string): TMoldRole;
var
  MoldClass: TMoldClass;
begin
  Result := nil;
  MoldClass := Classes.ItemsByName[boldclassName];
  if Assigned(MoldClass) then
    Result := MoldClass.Roles.ItemsByName[roleName];
end;

function TMoldModel.GetClassByName(const name: string): TMoldClass;
begin
  Result := Classes.ItemsByName[name];
  if not Assigned(Result) then
    Result := TMoldClass.Create(self, name);
end;

function ClassesComparer(Item1, Item2: TObject): Integer;
begin
  if (Item1 is TMoldClass) and (Item1 is TMoldClass) then
    result := CompareStr(TMoldClass(Item1).name, TMoldClass(Item2).Name)
  else
    raise EBoldInternal.Create('ooops, class was not a class in TMoldModel.EnsureTopSorted');
end;

procedure TMoldModel.SetRootClass(NewRootClass: TMoldClass);
begin
  if Assigned(NewRootClass) then
    FRootClass := NewRootClass;
end;

function TMoldClass.GetIntroducesManuallyDerivedMembers: Boolean;
var
  i: integer;
begin
  result := false;
  for i := 0 to Attributes.Count-1 do
    result := result or
    (Attributes[i].Derived and (Attributes[i].DerivationOCL = ''));
  for i := 0 to Roles.Count-1 do
    result := result or (Roles[i].Derived and (Roles[i].DerivationOCL = '') and {Roles[i].Embed and} Roles[i].Navigable);
end;

function TMoldClass.GetIntroducesManuallyReverseDerivedMembers: Boolean;
var
  i: integer;
begin
  result := false;
  for i := 0 to Attributes.Count-1 do
    result := result or
    (Attributes[i].Derived and Attributes[i].ReverseDerived);
end;

function TMoldAssociationList.GetEnumerator: TMoldAssociationListTraverser;
begin
  result := CreateTraverser as TMoldAssociationListTraverser;
end;

function TMoldAssociationList.GetItem(index: Integer): TMoldAssociation;
begin
  Result := TMoldAssociation(inherited Items[index]);
end;

function TMoldRoleList.GetEnumerator: TMoldRoleListTraverser;
begin
  result := CreateTraverser as TMoldRoleListTraverser;
end;

function TMoldRoleList.GetItem(index: Integer): TMoldRole;
begin
  Result := TMoldRole(inherited Items[index]);
end;

procedure TMoldModel.EnsureLinkRoles;
var
  i:integer;
begin
  if not fLinkRolesEnsured then
    for i := 0 to Associations.count-1 do
    begin
      Associations[i].Roles[0].EnsureLinkRole;
      Associations[i].Roles[1].EnsureLinkRole;
    end;

  fLinkRolesEnsured := True;
end;

{ TMoldClass }

constructor TMoldClass.Create(Parent: TMoldElement; const Name: string);
begin
  inherited Create(Parent, Name);
  Assert(Parent is TMoldModel);
  InitializeClass(TMoldModel(Parent));
end;


procedure TMoldClass.InitializeClass(Parent: TMoldModel);
begin
  FModel              := Parent;

  Model.Classes.Add(self);
  FSubClasses         := TMoldClassList.Create;
  fSubClasses.OwnsEntries := false;
  FAttributes         := TMoldAttributeList.Create;
  FMethods            := TMoldMethodList.Create;
  FRoles              := TMoldRoleList.Create;
  fRoles.OwnsEntries  := false;
  FIsAbstract         := False;
  fPersistent         := true;
end;

destructor TMoldClass.Destroy;
var
  i: Integer;
  TempSuperClass: TMoldClass;
begin



  if not Model.fIsdestroying then
  begin
    TempSuperClass := SuperClass;
    SuperClass := nil;
    if Assigned(SubClasses) then
      for i := SubClasses.Count - 1 downto 0 do
        fSubClasses[i].SuperClass := TempSuperClass;
  end;

  freeAndNil(fSubClasses);

  if Assigned(Association) then
    Association.LinkClass := nil;

  FreeAndNil(fAttributes);
  freeAndNil(fMethods);

  for i := fRoles.Count-1 downto 0 do
    fRoles[i].MoldClass := nil;

  FreeAndNil(fRoles);
  freeAndNil(fAllBoldMembers);
  freeAndNil(fAllNativeAttributes);
  FreeAndNil(fAllPossibleNames);
  FreeAndNil(fAllAutoOverrideMethods);

  if Assigned(Model) and assigned(Model.Classes) then
    Model.Classes.RemoveEntryReference(self);
  inherited Destroy;
end;

function TMoldClass.GetModel: TMoldModel;
begin
  Result := FModel;
end;


procedure TMoldClass.NameChanged;
begin
  if Assigned(Model) and assigned(Model.Classes) then
    Model.Classes.ItemChanged(self);
  if Assigned(SuperClass) and assigned(SuperClass.SubClasses) then
    SuperClass.SubClasses.ItemChanged(self);
end;

function TMoldClass.GetIncFileName: string;
begin
  Result := BoldTVByName[TAG_INCFILENAME];
end;

procedure TMoldClass.SetSuperClass(super: TMoldClass);
begin
  if super <> FSuperClass then
  begin
    if Assigned(super) then
      if super.ChildTo(self) then
        raise EBold.Create(sRecursiveAssignment);
    if FSuperClass <> nil then
      FSuperClass.SubClasses.Remove(self);
    FSuperClass := super;
    if FSuperClass <> nil then
      FSuperClass.SubClasses.Add(self);
  end;
end;

function TMoldClass.ChildTo(MoldClass: TMoldClass): Boolean;
begin
  if self = MoldClass then
    Result := True
  else
    if Assigned(SuperClass) then
      Result := SuperClass.ChildTo(MoldClass)
    else
      Result := False;
end;

function TMoldClass.GetIsRootClass: Boolean;
begin
  Result := Model.RootClass = self;
end;

function TMoldClassList.GetEnumerator: TMoldClassListTraverser;
begin
  result := CreateTraverser as TMoldClassListTraverser;
end;

function TMoldClassList.GetItem(index: Integer): TMoldClass;
begin
  Result := TMoldClass(inherited Items[index]);
end;

function TMoldClass.GetEffectivePersistent: Boolean;
var
  AllSubClassesTransient: Boolean;
  I: Integer;
begin
  AllSubClassesTransient := True;
  for I := 0 to SubClasses.Count - 1 do
    AllSubClassesTransient := AllSubClassesTransient and
                              not SubClasses[I].EffectivePersistent;

  Result := Persistent and
            (not Assigned(Association) or Association.EffectivePersistent) and
            not (IsAbstract and AllSubClassesTransient);
end;

function TMoldClass.GetExpandedExpressionName: string;
begin
  Result := inherited GetExpandedExpressionName;
  if (length(result) > 0) and CharInSet(Result[1], ['a'..'z']) then
  begin
    Result[1] := UpCase(Result[1]);
    result := BoldSharedStringManager.GetSharedString(Result);
  end;
end;

{
procedure TMoldClass.MainTables(Strings: TStrings; const AllowChild, AllowParent: Boolean; TablePrefix: String);
var
  I: Integer;
begin
  case TableMapping of
    tmOwn: Strings.Add(ExpandedTableName[TablePrefix]);
    tmChildren:
      if AllowChild then
        for I := 0 to FSubClasses.Count - 1 do
          FSubClasses[I].MainTables(Strings, True, False, TablePrefix)
      else
        raise EBoldInternal.Create('TMoldClass.MainTables: Illegal Mapping');
    tmParent:
      if AllowParent and Assigned(FSuperClass) then
        FSuperClass.MainTables(Strings, False, True, TablePrefix)
      else
        raise EBoldInternal.Create('TMoldClass.MainTables: Illegal Mapping');
  end;
end;
}

function TMoldClass.FindStoringClass(DefiningClass: TMoldClass; AbovedefiningClass: Boolean; Member:TMoldMember): TMoldClass;
begin

  if Assigned(Association) and
    (member is TMoldRole) and
    (TMoldRole(Member).association = Association) then
    DefiningClass := self;

  result := nil;
  if AbovedefiningClass then
    case TableMapping of
      tmParent: Result := SuperClass.FindStoringClass(DefiningClass, True, member);
      tmOwn: Result := self;
      tmChildren: raise EBoldInternal.CreateFmt('%s.FindStoringClass: Illegal table mapping for member %s.%s', [ClassName, Member.MoldClass.Name, Member.Name])
    end
  else
    if self = DefiningClass then
    begin
      case TableMapping of
        tmParent: Result := SuperClass.FindStoringClass(DefiningClass, True, member);
        tmOwn: Result := self;
        tmChildren: Result := nil;
      end;
    end else
    begin
      Result := SuperClass.FindStoringClass(DefiningClass, False, member);
      if not assigned(Result) and (TableMapping = tmOwn) then
        Result := self;
    end
end;

function TMoldClass.GetHasNotNullMembers: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Attributes.Count - 1 do
    Result := Result or not Attributes[I].AllowNull;
end;

function TMoldClass.LowestVisibleAncestor(MoldClass: TMoldClass): TMoldClass;
begin
  if (MoldClass.Component = Component) or Component.dependentOf(MoldClass.Component) then
    result := MoldClass
  else if assigned(MoldClass.SuperClass) then
    result := LowestVisibleAncestor(MoldClass.SuperClass)
  else
    result := nil;
end;

function TMoldAttributeList.GetEnumerator: TMoldAttributeListTraverser;
begin
  result := CreateTraverser as TMoldAttributeListTraverser;
end;

function TMoldAttributeList.GetItem(index: Integer): TMoldAttribute;
begin
  Result := TMoldAttribute(inherited Items[index]);
end;

function TMoldClass.GetAllBoldMembers: TMoldMemberList;
var
  i: integer;

  procedure TryAdd(Member: TMoldMember);
  begin
    if Member.MemberExists then
      fAllBoldMembers.Add(Member);
  end;
begin
  if not assigned(fAllBoldMembers) then
  begin
    Model.EnsureLinkRoles;
    fAllBoldMembers := TMoldMemberList.Create;
    fAllBoldMembers.OwnsEntries := false;

    if Assigned(SuperClass) then
    begin
      AllBoldMembers.Capacity := SuperClass.AllBoldMembers.count + Attributes.Count + (Roles.Count*2);
      for i := 0 to SuperClass.AllBoldMembers.count-1 do
        AllBoldMembers.Add(SuperClass.AllBoldMembers[i]);
    end;

    fFirstOwnBoldMemberIndex := fAllBoldMembers.Count;

    for i := 0 to Attributes.Count-1 do
      if Attributes[i].AttributeKind = bastBold then
        TryAdd(Attributes[i]);

    for i := 0 to Roles.Count-1 do
    begin
      TryAdd(Roles[i]);
      if Roles[i].HasLinkRole then
        tryAdd(Roles[i].LinkRole);
    end;

    if assigned(Association) then
    begin
      TryAdd(Association.fInnerLinkRoles[0]);
      TryAdd(Association.fInnerLinkRoles[1]);
    end;
  end;
  result := fAllBoldMembers;
end;

function TMoldClass.GetAllNativeAttributes: TMoldAttributeList;
var
  i:integer;
begin
  if not assigned(fAllNativeAttributes) then
  begin
    fAllNativeAttributes := TMoldAttributeList.Create;
    fAllNativeAttributes.OwnsEntries := false;
    if Assigned(SuperClass) then
      for i := 0 to SuperClass.AllNativeAttributes.count-1 do
        fAllNativeAttributes.Add(SuperClass.AllNativeAttributes[i]);
    fFirstOwnNativeAttributeIndex := fAllNativeAttributes.Count;
    for i := 0 to Attributes.Count-1 do
      if Attributes[i].AttributeKind = bastDelphi then
        fAllNativeAttributes.Add(Attributes[i]);
  end;
  result := fAllNativeAttributes;
end;

function TMoldClass.GetFirstOwnBoldMemberIndex: integer;
begin
  if not assigned(fAllBoldMembers) then
    GetAllBoldMembers;
  result := fFirstOwnBoldMemberIndex;
end;

function TMoldClass.GetFirstOwnNativeAttributeIndex: integer;
begin
  if not assigned(fAllNativeAttributes) then
    GetAllNativeAttributes;
  result := fFirstOwnNativeAttributeIndex;
end;

{---TMoldElementList---}
constructor TMoldElementList.Create;
begin
  inherited;
  SetIndexCapacity(2);
  SetIndexVariable(IX_Name, AddIndex(TNameIndex.Create));
end;

function TMoldElementList.GetItemByName(const name: string): TMoldElement;
begin
  Result := TMoldElement(TBoldCaseSensitiveStringHashIndex(Indexes[IX_Name]).FindByString(name));
end;

function TMoldElementList.GetEnumerator: TMoldElementListTraverser;
begin
  result := CreateTraverser as TMoldElementListTraverser;
end;

function TMoldElementList.GetItem(index: Integer): TMoldElement;
begin
  Result := TMoldElement(inherited Items[index]);
end;

procedure TMoldElementList.Add(MoldElement: TMoldElement);
begin
  inherited Add(MoldElement);
end;

function TMoldElementList.MakeUniqueName(const prefix: string): string;
var
  i: Integer;
begin
  i := 0;
  repeat
    inc(i);
    Result := Format('%s%d', [prefix, i]);
  until ItemsByName[Result] = nil;
end;

procedure TMoldElementList.RemoveEntryReference(Item: TMoldElement);
var
  fOwnsEntries: Boolean;
begin
  fOwnsEntries := OwnsEntries;
  OwnsEntries := False;
  Remove(Item);
  OwnsEntries := fOwnsEntries;
end;

function TMoldElementList.TraverserClass: TBoldIndexableListTraverserClass;
begin
  result := TMoldElementListTraverser;
end;

{---TMoldClassList---}
constructor TMoldClassList.Create;
begin
  inherited;
  SetIndexVariable(IX_ExpressionName, AddIndex(TExpressionNameIndex.Create));
  SetIndexVariable(IX_DelphiName, AddIndex(TDelphiNameIndex.Create));
end;

function TMoldClassList.GetItemByDelphiName(const DelphiName: String): TMoldClass;
begin
  Result := TMoldClass(TBoldCaseSensitiveStringHashIndex(Indexes[IX_DelphiName]).FindByString(DelphiName));
end;

function TMoldClassList.GetItemByExpressionName(const ExpressionName: String): TMoldClass;
begin
  Result := TMoldClass(TBoldCaseSensitiveStringHashIndex(Indexes[IX_ExpressionName]).FindByString(Expressionname));
end;

{---TMoldRoleList---}

function TMoldRoleList.GetItemByName(const name: string): TMoldRole;
begin
  Result := TMoldRole(TBoldCaseSensitiveStringHashIndex(Indexes[IX_Name]).FindByString(name));
end;

function TMoldRoleList.TraverserClass: TBoldIndexableListTraverserClass;
begin
  result := TMoldRoleListTraverser;
end;

{---TMoldMethodList---}
function TMoldMethodList.GetEnumerator: TMoldMethodListTraverser;
begin
  result := CreateTraverser as TMoldMethodListTraverser;
end;

function TMoldMethodList.GetItem(index: Integer): TMoldMethod;
begin
  Result := TMoldMethod(inherited Items[index]);
end;

function TMoldMethodList.GetItemByName(const name: string): TMoldMethod;
begin
  Result := TMoldMethod(TBoldCaseSensitiveStringHashIndex(Indexes[IX_Name]).FindByString(name));
end;

function TMoldMethodList.TraverserClass: TBoldIndexableListTraverserClass;
begin
  result := TMoldMethodListTraverser;
end;

{---TMoldAssociationList---}

function TMoldAssociationList.GetItemByName(const name: string): TMoldAssociation;
begin
  Result := TMoldAssociation(TBoldCaseSensitiveStringHashIndex(Indexes[IX_Name]).FindByString(name));
end;

function TMoldAssociationList.TraverserClass: TBoldIndexableListTraverserClass;
begin
  result := TMoldAssociationListTraverser;
end;

{---TMoldAttributeList---}

function TMoldAttributeList.GetItemByName(const name: string): TMoldAttribute;
begin
  Result := TMoldAttribute(TBoldCaseSensitiveStringHashIndex(Indexes[IX_Name]).FindByString(name));
end;

function TMoldAttributeList.TraverserClass: TBoldIndexableListTraverserClass;
begin
  result := TMoldAttributeListTraverser;
end;

{---TMoldQualifierList---}
function TMoldQualifierList.GetEnumerator: TMoldQualifierListTraverser;
begin
  result := CreateTraverser as TMoldQualifierListTraverser;
end;

function TMoldQualifierList.GetItem(index: Integer): TMoldQualifier;
begin
  Result := TMoldQualifier(inherited Items[index]);
end;

function TMoldQualifierList.GetItemByName(const name: string): TMoldQualifier;
begin
  Result := TMoldQualifier(TBoldCaseSensitiveStringHashIndex(Indexes[IX_Name]).FindByString(name));
end;

function TMoldQualifierList.TraverserClass: TBoldIndexableListTraverserClass;
begin
  result := TMoldQualifierListTraverser;
end;

{---TMoldMember---}

function TMoldMember.GetModel: TMoldModel;
begin
  Result := MoldClass.Model;
end;

function TMoldMember.GetExpandedExpressionName: string;
begin
  Result := inherited GetExpandedExpressionName;
  if (length(result) > 0) and CharInSet(Result[1], ['A'..'Z']) then
  begin
    Result[1] := chr(ord(Result[1]) + 32);
    result := BoldSharedStringManager.GetSharedString(Result);
  end;
end;

procedure TMoldMember.SetMoldClass(NewMoldClass: TMoldClass);
begin
  fMoldClass := NewMoldClass;
end;

function TMoldMember.GetMoldClass: TMoldClass;
begin
  result := fMoldClass;
end;

{---TMoldAttribute---}
constructor TMoldAttribute.Create(Parent: TMoldElement; const Name: string);
begin
  inherited Create(Parent, Name);
  Assert(Parent is TMoldClass);
  SetMoldClass(TMoldClass(Parent));
  BoldType := 'String';
  MoldClass.Attributes.Add(self);
end;

destructor TMoldAttribute.Destroy;
begin
  if Assigned(MoldClass) and assigned(MoldClass.Attributes) then
    MoldClass.Attributes.RemoveEntryReference(self);
  inherited;
end;

procedure TMoldAttribute.NameChanged;
begin
  if Assigned(MoldClass) and Assigned(MoldClass.Attributes) then
    MoldClass.Attributes.ItemChanged(self);
end;

function TMoldAttribute.GetEffectivePersistent: Boolean;
begin
  result := Persistent and not Derived and moldclass.EffectivePersistent;
end;

{---TMoldMethod---}
constructor TMoldMethod.Create(Parent: TMoldElement; const Name: string);
begin
  inherited Create(Parent, Name);
  Assert(Parent is TMoldClass);
  FMoldClass    := TMoldClass(Parent);
  MoldClass.Methods.Add(self);
end;

destructor TMoldMethod.Destroy;
begin
  if Assigned(MoldClass) and assigned(MoldClass.Methods) then
    MoldClass.Methods.RemoveEntryReference(self);
  FreeAndNil(fParameters);
  inherited;
end;

function TMoldMethod.GetModel: TMoldModel;
begin
  Result := MoldClass.Model;
end;

procedure TMoldMethod.NameChanged;
begin
  if Assigned(MoldClass) and assigned(MoldClass.Methods) then
    MoldClass.Methods.ItemChanged(self);
end;

procedure TMoldMethod.setSignature(Value: string);
var
  i:integer;
  ParamType,
  next: string;
  param: TMoldParameter;
  IsConst, IsVar, IsOut: Boolean;
begin
  if Signature <> Value then
  begin
    for i := parameters.Count-1 downto 0 do
      parameters[i].free;
    Parameters.Clear;

    if value <> '' then
      value := value + ';';
    while value <> '' do
    begin
      IsConst := False;
      IsVar := False;
      IsOut := False;
      next := copy(value, 1, pos(';', value) - 1);
      value := copy(value, pos(';', value) + 1, maxint);

      if Pos(UpperCase('const '), UpperCase(Next)) > 0 then
      begin
        IsConst := True;
        Delete(Next, Pos(UpperCase('const '), UpperCase(Next)), Length('const '));
      end
      else if Pos(UpperCase('var '), UpperCase(Next)) > 0 then
      begin
        IsVar := True;
        Delete(Next, Pos(UpperCase('var '), UpperCase(Next)), Length('var '));
      end
      else if Pos(UpperCase('out '), UpperCase(Next)) > 0 then
      begin
        IsOut := True;
        Delete(Next, Pos(UpperCase('out '), UpperCase(Next)), Length('out '));
      end;

      if pos(':', next) <> 0 then
      begin
        paramtype := BoldTrim(copy(next, pos(':', next) + 1, maxint));
        next := copy(next, 1, pos(':', next) - 1);
      end;
      next := next + ',';
      while pos(',', next) <> 0 do
      begin
        param := TMoldParameter.create(self);
        param.ParameterName := BoldTrim(copy(next, 1, pos(',', next)-1));
        param.ParameterType := ParamType;
        param.FIsConst := IsConst;
        if IsVar then
          param.ParameterKind := pdInOut
        else if IsOut then
          param.ParameterKind := pdOut;
        Parameters.Add(Param);
        next := copy(next, pos(',', next)+1, maxint);
      end;
    end;
  end;
end;

function TMoldMethod.GetParameters: TBoldObjectArray;
begin
  if not assigned(fParameters) then
    fParameters := TBoldObjectArray.Create(0, [bcoDataOwner]);
  result := fParameters;
end;

function TMoldMethod.GetSignature: String;
var
  i: integer;
  Param: TMoldParameter;
begin
  result := '';
  for i := 0 to parameters.count-1 do
  begin
    if result <> '' then
      result := result + '; ';
    Assert(Parameters[i] is TMoldParameter);
    Param := TMoldParameter(Parameters[i]);
    if Param.IsConst then
      Result := Result + 'const '
    else if Param.ParameterKind = pdInOut then
      Result := Result + 'var '
    else if Param.ParameterKind = pdOut then
      Result := Result + 'out ';

    result := result + format('%s: %s', [
      TMoldParameter(parameters[i]).ParameterName,
      TMoldParameter(parameters[i]).ParameterType]);
  end;
end;

function TMoldMethod.GetUMLClassName: string;
begin
  result := 'Operation';
end;

{---TMoldRole---}
constructor TMoldRole.Create(Parent: TMoldElement; const Name: string);
begin
  inherited Create(Parent,Name);
  if (Parent is TMoldAssociation) then
  begin
    Association := TMoldAssociation(Parent);
  end;
  Multiplicity  := '0..1';
  Navigable     := True;
  fRoleType     := rtRole;
  FQualifiers   := TMoldQualifierList.Create;
end;

destructor TMoldRole.Destroy;
begin
  if Assigned(Association) and assigned(Association.Roles) then
    Association.Roles.RemoveEntryReference(self);
  if Assigned(MoldClass) and assigned(MoldClass.Roles) then
    MoldClass.Roles.RemoveEntryReference(self);
  FreeAndNil(fQualifiers);
  inherited;
end;

function TMoldRole.GetModel: TMoldModel;
begin
  Result := Association.Model;
end;

procedure TMoldRole.NameChanged;
begin
  if Assigned(MoldClass) and assigned(MoldClass.Roles) then
    MoldClass.Roles.ItemChanged(self);
  if Assigned(Association) and assigned(Association.Roles) then
    Association.Roles.ItemChanged(self);
end;

function TMoldRole.GetEffectivePersistent: Boolean;
begin
  Result := association.effectivePersistent and (not Derived) and (RoleType <> rtLinkRole);
end;

procedure TMoldRole.SetAssociation(Association: TMoldAssociation);
var
  oldAssociation: TMoldAssociation;
begin
  oldAssociation := FAssociation;
  FAssociation := Association;
  if oldAssociation <> Association then
  begin
    if Assigned(oldAssociation) then
      oldAssociation.Roles.Remove(self);
    if Assigned(Association) then
    begin
      case Self.RoleType of
        rtRole: Association.Roles.Add(self);
        rtLinkRole: Association.fLinkRoles.Add(Self);
        rtInnerLinkRole: Association.fInnerLinkRoles.Add(self);
      end;
    end;
  end;
end;

procedure TMoldRole.SetMoldClass(NewMoldClass: TMoldClass);
begin
  if RoleType = rtRole then
  begin
    if MoldClass <> NewMoldClass then
    begin
      if Assigned(MoldClass) then
        MoldClass.Roles.RemoveEntryReference(self);
      if Assigned(NewMoldClass) and
        not assigned(NewMoldClass.Roles.ItemsByName[self.Name]) then
        NewMoldClass.Roles.Add(self);
    end;
  end;
  Inherited;
end;

function TMoldRole.GetOtherEnd: TMoldRole;
begin
  result := nil;
  case RoleType of
    rtLinkRole:
      if Association.fLinkRoles[0] = self then
        result := Association.fInnerLinkRoles[0]
      else
        result := Association.fInnerLinkRoles[1];
    rtInnerLinkRole :
      if Association.fInnerLinkRoles[0] = self then
        result := Association.fLinkRoles[0]
      else
        result := Association.fLinkRoles[1];
    rtRole:
      if Association.Roles[0] = self then
        Result := Association.Roles[1]
      else
        Result := Association.Roles[0];
  end;
end;

function TMoldRole.GetHasLinkRole: Boolean;
begin
  result := (RoleType = rtRole) and Assigned(Association.LinkClass);
end;

function TMoldRole.GetIsAttribute: boolean;
begin
  result := false;
end;

function TMoldRole.GetIsRole: boolean;
begin
  result := true;
end;

function TMoldRole.GetLinkRole: TMoldRole;
begin
  MoldClass.Model.EnsureLinkRoles;
  if HasLinkRole then
    result := fRelatedRole
  else
    result := nil;
end;

procedure TMoldRole.EnsureLinkRole;
var
  InnerLinkRole:TMoldRole;
  RelatedName: string;

begin
  if HasLinkRole and not assigned(fRelatedRole) then
  begin
    if Association.Roles[1] = self then
      OtherEnd.EnsureLinkRole;
    if MoldClass.ChildTo(OtherEnd.MoldClass) or otherEnd.MoldClass.ChildTo(MoldClass) then
      RelatedName:= Name + Association.LinkClass.name
    else
      RelatedName := Association.LinkClass.name;

    fRelatedRole := TMoldRole.Create(self, RelatedName);
    fRelatedRole.fRelatedRole := self;
    fRelatedRole.Multiplicity := Multiplicity;
    fRelatedRole.fRoleType := rtLinkRole;
    fRelatedRole.MoldClass := MoldClass;
    fRelatedRole.FOrdered := Ordered;
    fRelatedRole.Association := Association;
    fRelatedRole.FNavigable := Navigable;
    fRelatedRole.BoldTVByName[TAG_EMBED] := TV_FALSE;
    fRelatedRole.fVisibility := Visibility;
    InnerLinkRole := TMoldRole.Create(self, OtherEnd.Name);
    InnerLinkRole.fRoleType := rtInnerLinkRole;
    InnerLinkRole.MoldClass := Association.LinkClass;
    InnerLinkRole.Association := Association;
    InnerLinkRole.Multiplicity := '1';
    InnerLinkRole.Ordered := false;
    InnerLinkRole.BoldTVByName[TAG_EMBED] := TV_TRUE;
    InnerLinkRole.Navigable := OtherEnd.Navigable;
    InnerLinkRole.fVisibility := OtherEnd.Visibility;
    InnerLinkRole.BoldTVByName[TAG_COLUMNNAME] := OtherEnd.ColumnName;
    InnerLinkRole.BoldTVByName[TAG_EXPRESSIONNAME] := OtherEnd.ExpressionName;
    InnerLInkRole.BoldTVByName[TAG_FORMERNAMES] := OtherEnd.FormerNames;
  end;
end;

function TMoldRole.GetMainRole: TMoldRole;
begin
  if RoleType = rtLinkRole then
    result := fRelatedRole
  else
    result := nil;
end;

function TMoldRole.GetUMLClassName: string;
begin
  result := 'AssociationEnd';
end;

{---TMoldAssociation---}
constructor TMoldAssociation.Create(Parent: TMoldElement; const Name: string);
begin
  inherited Create(Parent, Name);
  Assert(Parent is TMoldModel);
  FModel := TMoldModel(Parent);
  BoldTVByName[TAG_DELPHINAME] := TV_NAME;
  Model.Associations.Add(self);
  FRoles := TMoldRoleList.Create;
  fLinkRoles := TMoldRoleList.Create;
  fInnerLinkRoles := TMoldRolelist.Create;
end;

destructor TMoldAssociation.Destroy;
begin
  LinkClass := nil;
  if assigned(Model) and assigned(Model.Associations) then
    Model.Associations.RemoveEntryReference(self);
  FreeAndNil(fRoles);
  FreeAndNil(fLinkRoles);
  FreeAndNil(fInnerLInkRoles);
  FreeAndNil(fAllPossibleNames);
  inherited;
end;

procedure TMoldAssociation.NameChanged;
begin
  if Assigned(Model) and assigned(Model.Associations) then
    Model.Associations.ItemChanged(self);
end;

function TMoldAssociation.GetModel: TMoldModel;
begin
  Result := FModel;
end;

function TMoldAssociation.GetEffectivePersistent: Boolean;
begin
  Result := Persistent and not derived and


    assigned(Roles[0].MoldClass) and (Roles[0].MoldClass <> LinkClass) and Roles[0].MoldClass.EffectivePersistent and
    assigned(Roles[1].MoldClass) and (Roles[1].MoldClass <> LinkClass) and Roles[1].MoldClass.EffectivePersistent;
end;

procedure TMoldAssociation.SetLinkClass(Value: TMoldClass);
begin
  if Assigned(LinkClass) then
  begin
    LinkClass.Association := nil;
    FLinkClass := nil;
  end;
  if Assigned(Value) then
  begin
    if Assigned(Value.Association) and
      (Value.Association <> self) then
      raise EBold.Create(sClassIsRelation);
    FLinkClass := Value;
    LinkClass.Association := self;
  end;
end;

function TMoldAssociation.GetUMLClassName: string;
begin
  result := 'Association';
end;

{---TMoldQualifier---}

constructor TMoldQualifier.Create(Parent: TMoldElement; const Name: string);
begin
  inherited Create(Parent, Name);
  Assert(Parent is TMoldRole);
  FMoldRole := TMoldRole(Parent);
  MoldRole.Qualifiers.Add(self);
end;

procedure TMoldQualifier.NameChanged;
begin
  if Assigned(MoldRole) and assigned(MoldRole.Qualifiers) then
    MoldRole.Qualifiers.ItemChanged(self);
end;

function TMoldQualifier.GetModel: TMoldModel;
begin
  Result := MoldRole.Model;
end;

function TMoldQualifier.GetUMLClassName: string;
begin
  result := 'Attribute';
end;

function TMoldClass.GetHasManuallyDerivedMembers: Boolean;
var
  i: integer;
begin
  result := false;
  for i := 0 to AllBoldMembers.Count-1 do
    if AllBoldMembers[i].Derived and
      ((AllBoldMembers[i].DerivationOCL = '') or AllBoldMembers[i].ReverseDerived)  then
    begin
      result := true;
      exit;
    end;
end;

function TMoldClass.GetAllAutoOverrideMethods: TMoldMethodList;
var
  i: integer;
  TempClass: TMoldClass;
begin
  if not assigned(fAllAutoOverrideMethods) then
  begin
    tempClass := superclass;
    fAllAutoOverrideMethods := TMoldMethodList.create;
    fAllAutoOverrideMethods.OwnsEntries := false;
    while assigned(tempClass) do
    begin
      for i := 0 to TempClass.Methods.Count-1 do
        if TempClass.Methods[i].OverrideInAllSubclasses and
          not assigned(Methods.ItemsByName[TempClass.Methods[i].Name]) then
          fAllAutoOverrideMethods.Add(TempClass.Methods[i]);
      tempClass := TempClass.SuperClass;
    end;
  end;
  result := fAllAutoOverrideMethods;
end;

function TMoldClass.GetComponent: TMoldComponent;
var
  ComponentName: string;
begin
  ComponentName := ExpandedUnitName;
  if ComponentName = '' then
  begin
    if assigned(superClass) then
      result := superClass.Component
    else
      result := Model.MainComponent;
  end
  else
    result := Model.EnsureComponent(ComponentName);
end;

procedure TMoldClass.SetComponent(const Value: TMoldComponent);
begin
  if assigned(Value) then
    BoldUnitName := Value.Name
  else
    BoldUnitName := '';
end;

function TMoldClass.GetTableMapping: TTableMapping;
begin
  result := TBoldTaggedValueSupport.StringToTableMapping(BoldTVByName[TAG_TABLEMAPPING]);
end;

function TMoldClass.GetBoldUnitName: string;
begin
  result := BoldTVByName[TAG_UNITNAME];
end;

function TMoldClass.GetImported: Boolean;
begin
  result := TVIsTrue(BoldTVByName[TAG_IMPORTED]);
end;

function TMoldClass.GetTableName: string;
begin
  result := BoldTVByName[TAG_TABLENAME];
end;

function TMoldClass.GetExternalTableName: string;
begin
  result := BoldTVByName[TAG_EXTERNALTABLENAME];
end;

procedure TMoldClass.SetBoldUnitName(const Value: string);
begin
  BoldTVByName[TAG_UNITNAME] := Value;
end;

procedure TMoldClass.SetIncFileName(const Value: string);
begin
  BoldTVByName[TAG_INCFILENAME] := Value;
end;

function TMoldClass.GetDefaultStringRepresentation: String;
begin
  result := BoldTVByName[TAG_DEFAULTSTRINGREPRESENTATION];
end;

function TMoldClass.GetEffectiveDefaultStringRepresentation: String;
begin
  result := DefaultStringRepresentation;
  if (result = '') and assigned(SuperClass) then
    result := SuperClass.EffectiveDefaultStringRepresentation;
end;

function TMoldClass.GetHasCodeStubs: Boolean;
begin
  result := (Methods.count > 0) or
            (AllAutoOverrideMethods.Count > 0) or
            HasManuallyDerivedMembers or
            HasDelphiAttributesWithAccessorFunctions;
end;

function TMoldClass.EffectiveIncFileName(const DefaultExtension: String): String;
var
  Extension: string;
begin
  result := IncFileName;
  if pos('.', result) <> 0 then
  begin
    Extension := '.'+copy(Result, pos('.', Result)+1, maxint);
    result := copy(result, 1, pos('.', Result)-1);
  end
  else
    extension := DefaultExtension;

  Result := BoldExpandName(result, Name, xtDelphi, -1, Model.NationalCharConversion);

  if result <> '' then
    result := result + Extension
  else if assigned(SuperClass) and (SuperClass.Component = Component) then
    result := SuperClass.EffectiveIncFileName(DefaultExtension)
  else
    result := Component.Name + Extension;
end;

function TMoldClass.GetVersioned: Boolean;
var
  s: string;
begin
  s := BoldTVByName[TAG_VERSIONED];
  if SameText(s, DEFAULTNAME) then
    result := assigned(SuperClass) and SuperClass.Versioned
  else
    result := TVIsTrue(s);
end;

function TMoldClass.GetExpandedInterfaceName: string;
begin
  result := BoldExpandName(BoldTVByName[TAG_INTERFACENAME], name, xtDelphi, -1, Model.NationalCharConversion);
end;

function TMoldClass.GetGuid: String;
begin
  result := BoldTVByName[TAG_GUID];
end;

function TMoldClass.LowestCommonSuperClass(
  otherClass: TMoldClass): TMoldClass;
begin
  if OtherClass.ChildTo(self) then
    result := self
  else if ChildTo(OtherClass) then
    result := OtherClass
  else
    result := LowestCommonSuperClass(OtherClass.SuperClass);
end;

function TMoldClass.GetListGuid: String;
begin
  result := BoldTVByName[TAG_LISTGUID];
end;

function TMoldClass.GetOptimisticLocking: TBoldOptimisticLockingMode;
begin
  Result := TBoldTaggedValueSupport.StringToOptimisticLockingMode(BoldTVByName[TAG_OPTIMISTICLOCKING]);
end;

function TMoldClass.GetExpandedUnitName: string;
begin
  result := BoldExpandName(BoldUnitName, Name, xtDelphi, -1, Model.NationalCharConversion);
end;

procedure TMoldClass.TrimRemoved;
var
  i: integer;
  Attr: TMoldAttribute;
begin
  for i := Attributes.Count-1 downto 0 do
  begin
    attr := Attributes[i];
    if Attr.EvolutionState = esRemoved then
      Attr.free;
  end;
end;

function TMoldClass.GetEffectiveEvolutionState: TBoldEvolutionState;
begin
  result := EvolutionState;
  if assigned(Association) and (association.EvolutionState > result) then
    result := association.EvolutionState;
end;

procedure TMoldClass.AssignMemberDispIds;
var
  i, NextId: integer;
begin
  NextId := FirstDispId;
  for i := 0 to AllBoldMembers.Count-1 do
    if AllBoldMembers[i].HasDispId then
    begin
      AllBoldMembers[i].fDispId := NextId;
      Inc(NextId);
    end;
  for i := 0 to Methods.Count-1 do
    if Methods[i].HasDispId then
    begin
      Methods[i].fDispId := NextId;
      Inc(NextId);
    end;
  fLastDispId := NextId-1;
end;

function TMoldClass.GetFirstDispId: integer;
begin
  if Assigned(SuperClass) then
    Result := ((SuperClass.LastDispId + BOLD_DISPID_ROUND) div BOLD_DISPID_ROUND)* BOLD_DISPID_ROUND
  else
    Result := BOLD_FIRSTUSERDISPID;
end;

function TMoldClass.GetLastDispId: integer;
begin
  if fLastDispId = 0 then
    AssignMemberDispIds;
  result := fLastDispId;
end;

function TMoldClass.GetAllPossibleNames: TStringList;
var
  i: integer;
begin
  if not assigned(fAllPossibleNames) then
  begin
    fAllPossibleNames := TStringList.Create;
    fAllPossibleNames.Sorted := true;
    fAllPossibleNames.Duplicates := dupIgnore;
    fAllPossibleNames.CommaText := FormerNames;
    fAllPossibleNames.Add(ExpandedExpressionName);
    if assigned(association) then
    begin
      for i := 0 to Association.AllPossibleNames.Count-1 do
        fAllPossibleNames.Add(Association.AllPossibleNames[i]);
    end;
  end;
  result := fAllPossibleNames;
end;

function TMoldClass.GetHasDelphiAttributesWithAccessorFunctions: Boolean;
var
  i: integer;
begin
  Result := False;
  for i := FirstOwnNativeAttributeIndex to AllNativeAttributes.Count - 1 do
  begin
    Result := (AllNativeAttributes[i].DelphiPropertyRead in [pkPrivateMethod, pkProtectedVirtualMethod]) or
              (AllNativeAttributes[i].DelphiPropertyWrite in [pkPrivateMethod, pkProtectedVirtualMethod]);
    if Result then Break;
  end;
end;

function TMoldClass.GetUMLClassName: string;
begin
  result := 'Class';
end;

function TMoldClass.CalculateCRC: Cardinal;
var
  i: integer;
begin
  result := CRCHash(ExpandedDelphiName);
  for i := 0 to Attributes.Count-1 do
    result := SumCRCs(result, Attributes[i].CalculateCRC);
  for i := 0 to Roles.Count-1 do
    result := SumCRCs(result, Roles[i].CalculateCRC);
end;

function TMoldClass.GetEffectiveOptimisticLocking: TBoldOptimisticLockingMode;
begin
  result := OptimisticLocking;
  if result = bolmDefault then
  begin
    if assigned(SuperClass) then
      result := SuperClass.EffectiveOptimisticLocking
    else
      result := Model.OptimisticLocking;
  end;
end;

function TMoldClass.GetGenerateDefaultRegion: Boolean;
begin
  result := TVIsTrue(BoldTVByName[TAG_GENERATEDEFAULTREGION_CLASS]);
end;

function TMoldClass.GetStorage: TBoldStorage;
begin
  result := TBoldTaggedValueSupport.StringToStorage(BoldTVByName[TAG_STORAGE]);
end;

{ TMoldComponentList }

function TMoldComponentList.GetItem(index: Integer): TMoldComponent;
begin
  Result := TMoldComponent(inherited Items[index]);
end;

function TMoldComponentList.GetItemByName(const name: string): TMoldComponent;
var
  i: integer;
begin
  result := nil;
  for i := 0 to Count-1 do
    if SameText(Items[i].Name, Name) then
    begin
      result := Items[i];
      break;
    end;
end;

{ TMoldComponent }
procedure TMoldComponent.GetInterfaceDependencies(StringList: TStringList);
var
  i: integer;
begin
  for i := 0 to Dependencies.Count-1 do
    if StringList.IndexOf(dependencies[i].Name) = -1 then
    begin
      StringList.Add(dependencies[i].Name);
      Dependencies[i].GetInterfaceDependencies(StringList);
    end;
end;

procedure TMoldComponent.GetImplementationDependencies(StringList: TStringList);
var
  InterfaceDep: TStringlist;
  i: integer;
begin
  InterfaceDep := TstringList.create;
  StringList.Clear;
  try
    GetInterfaceDependencies(InterfaceDep);
    for i := 0 to fMoldModel.Components.count-1 do
    begin
      if (fMoldModel.Components[i] <> self) and
         (InterfaceDep.IndexOf(fMoldModel.Components[i].Name) = -1) then
        StringList.Add(fMoldModel.Components[i].Name);
    end;
  finally
    InterfaceDep.Free;
  end;
end;

constructor TMoldComponent.create(MoldModel: TMoldModel);
begin
  inherited create;
  fDependencies := TMoldComponentlist.Create;
  fMoldModel := MoldModel;
end;

function TMoldComponent.dependentOf(Component: TMoldComponent): Boolean;
var
  dependencyList:TStringList;
begin
  DependencyList := TStringList.Create;
  GetInterfaceDependencies(dependencyList);
  result := dependencyList.IndexOf(Component.Name) <> -1;
  dependencyList.Free;
end;

destructor TMoldComponent.destroy;
begin
  FreeAndNil(fDependencies);
  inherited;
end;

function TMoldRole.GetMulti: Boolean;
begin
  case RoleType of
    rtRole: Result := (GetUpperLimitForMultiplicity(Multiplicity) > 1) or (Qualifiers.Count > 0);
    rtLinkRole: Result := (GetUpperLimitForMultiplicity(Multiplicity) > 1) or (fRelatedRole.Qualifiers.Count > 0);
    rtInnerLinkRole: Result := false;
    else
      // there are only three rolekinds, this mainly to fool compiler hints;
      raise EBold.CreateFmt(sUnknownMoldRoleType, [ClassName, MoldClass.Name, Name]);
  end;
end;

destructor TMoldElement.Destroy;
begin
  FreeAndNil(fTaggedValues);
  FreeAndNil(fBoldTaggedValues);
  FreeAndNil(fConstraints);
  inherited;
end;

function TMoldElement.GetBoldTVByName(const Tag: string): string;
var
  NamedValueListEntry: TBoldNamedValueListEntry;
  Definition: TBoldTaggedValueDefinition;
  Found: Boolean;
begin
  Found := False;
  if Assigned(fBoldTaggedValues) then
  begin
    NamedValueListEntry := BoldTaggedValues.ItemByName[Tag];
    if Assigned(NamedValueListEntry) then
    begin
      Found := True;
      Result := NamedValueListEntry.Value;
    end;
  end;
  if not Found then
  begin
    Definition := DefaultBoldTVList.DefinitionForTag[Tag];
    if Assigned(Definition) then
      Result :=  Definition.DefaultValue
    else
      Result := '';
   end;
end;

function TMoldElement.GetAllNonBoldTaggedValues: TStringList;
begin
  if Assigned(fTaggedValues) then
  begin
    Result := TStringList.Create;
    TaggedValues.AddToStrings(Result);
  end
  else
    result := nil;
end;

function TMoldModel.EnsureComponent(const ComponentName: string): TMoldComponent;
begin
  result := FindComponent(ComponentName);
  if not assigned(result) then
  begin
    result := TMoldComponent.Create(self);
    result.Name := ComponentName;
    Components.Add(result);
  end;
end;

function TMoldModel.FindComponent(const ComponentName: string): TMoldComponent;
begin
  result := components.ItemsByName[ComponentName];
end;

function TMoldModel.RenameComponent(const OldComponentName,
  NewComponentName: string): TMoldComponent;
begin
  result := FindComponent(OldComponentName);
  if assigned(result) then
    result.Name := NewComponentName;
end;

function TMoldModel.GetBoldUnitName: string;
begin
  result := BoldTVByName[TAG_UNITNAME];
end;

procedure TMoldElement.SetBoldTVByName(const Tag, value: string);
var
 Definition: TBoldTaggedValueDefinition;
 TrimmedValue: string;
begin
  TrimmedValue := BoldTrim(Value);
  Definition := DefaultBoldTVList.DefinitionForTag[Tag];
  // only set tag if it differs from default
  if Assigned(Definition) and (Definition.DefaultValue = TrimmedValue) then
    if BoldTaggedValues.ValueByName[Tag] =  '' then
      exit;
  BoldTaggedValues.ValueByName[Tag] := TrimmedValue;
end;

function TMoldMember.GetManuallyDerived: Boolean;
begin
  result := Derived and (DerivationOCL = '');
end;

{ TMoldMemberList }

function TMoldMemberList.GetEnumerator: TMoldMemberListTraverser;
begin
  result := CreateTraverser as TMoldMemberListTraverser;
end;

function TMoldMemberList.GetItem(index: Integer): TMoldMember;
begin
  result := TMoldMember(Inherited GetItem(index));
  Assert(result is TMoldMember);
end;

function TMoldMemberList.GetItemByName(const name: string): TMoldMember;
begin
  result := TMoldMember(inherited GetItemByName(Name));
  Assert(result is TMoldMember);
end;

function TMoldMemberList.TraverserClass: TBoldIndexableListTraverserClass;
begin
  result := TMoldMemberListTraverser;
end;

function TMoldRole.GetDeleteAction: TDeleteAction;
begin
  Result := TBoldTaggedValueSupport.StringToDeleteAction(BoldTVByName[TAG_DELETEACTION]);
end;

function TMoldAttribute.GetDerived: Boolean;
begin
  result := fDerived;
end;

function TMoldAttribute.GetReverseDerived: Boolean;
begin
  result := TVIsTrue(BoldTVByName[TAG_REVERSEDERIVE]);
end;

function TMoldRole.GetDerived: Boolean;
begin
  result := Association.Derived;
end;

function TMoldRole.GetReverseDerived: Boolean;
begin
  result := TVIsTrue(Association.BoldTVByName[TAG_REVERSEDERIVE]);
end;

function TMoldMember.GetDerivationOCL: String;
begin
  result := BoldTVByName['DerivationOCL'];
end;

function TMoldMember.GetMemberExists: boolean;
begin
  result := true;
end;

function TMoldRole.GetMemberExists: boolean;
begin
  result := inherited GetMemberExists and not (derived and not navigable);
end;

function TMoldModel.GetComponents: TMoldComponentList;
var
  i:integer;
begin
  if not assigned(fComponents) then
  begin
    fComponents := TMoldComponentList.Create;
    for i := 0 to classes.Count-1 do
      if Classes[i].ExpandedUnitName <> '' then
        EnsureComponent(Classes[i].ExpandedUnitName);
    EnsureComponent(MainComponent.Name);
  end;
  result := fComponents;
end;

function TMoldMethod.GetCallSignature: string;
var
  i: integer;
begin
  result := '';
  for i := 0 to parameters.count-1 do
  begin
    if result <> '' then
      result := result + ', ';

    result := result + TMoldParameter(parameters[i]).ParameterName;
  end;
end;

function TMoldModel.GetMainComponent: TMoldComponent;
var
  BoldUnitName: String;
begin
  BoldUnitName := ExpandedUnitName;
  if BoldUnitName = '' then
    BoldUnitName := ExpandedDelphiName;

  Result := EnsureComponent(BoldUnitName);
end;

procedure TMoldElement.NameChanged;
begin
end;

function TMoldElement.GetDelphiName: string;
begin
  Result := BoldTVByName[TAG_DELPHINAME];
end;

function TMoldModel.GetInterfaceUses: string;
begin
  result := BoldTVByName[TAG_INTERFACEUSES];
end;

constructor TMoldMember.Create(Parent: TMoldElement; const Name: string);
begin
  inherited Create(Parent, Name);
  fIndex := -1;
end;

function TMoldMember.GetColumnName: string;
begin
  result := BoldTVByName[TAG_COLUMNNAME];
end;

function TMoldAttribute.GetAllowNull: Boolean;
begin
  result := TVIsTrue(BoldTVByName[TAG_ALLOWNULL]);
end;

function TMoldMethod.GetFuncType: TDelphiFunctionType;
begin
  result := TBOldTaggedValueSupport.StringToDelphiFunctionType(BoldTVByName[TAG_DELPHIOPERATIONKIND]);
end;

function TMoldRole.GetMandatory: Boolean;
begin
  result := GetLowerLimitForMultiplicity(Multiplicity) > 0;
end;

function TMoldElement.GetExpressionName: string;
begin
  result := BoldTVByName[TAG_EXPRESSIONNAME];
  if result = '' then
    result := TV_NAME;
end;

function TMoldElement.GetPMapperName: string;
begin
  result := BoldTVByName[TAG_PMAPPERNAME];
  if Result = '' then
    Result := DEFAULTNAMELITERAL;
end;

function TMoldRole.GetEmbed: Boolean;
begin
  result := TBoldTaggedValueSupport.StringToBoolean(BoldTVByName[TAG_EMBED]);
end;

function TMoldMember.GetEffectiveDelayedFetch: Boolean;
begin
  result := TVIsTrue(BoldTVByName[TAG_DELAYEDFETCH]);
end;

function TMoldAttribute.GetAttributeKind: TBoldAttributeKind;
begin
  result := TBoldTaggedValueSupport.StringToAttributeKind(BoldTVByName[TAG_ATTRIBUTEKIND]);
end;

function TMoldAttribute.GetDelphiPropertyRead: TDelphiPropertyAccessKind;
begin
  result := TBoldTaggedValueSupport.StringToDelphiPropertyAccessKind(BoldTVByName[TAG_DPREAD]);
end;

function TMoldAttribute.GetDelphiPropertyWrite: TDelphiPropertyAccessKind;
begin
  result := TBoldTaggedValueSupport.StringToDelphiPropertyAccessKind(BoldTVByName[TAG_DPWRITE]);
end;

function TMoldAttribute.GetHasDelphiField: Boolean;
begin
  result := TVIsTrue(BoldTVByName[TAG_DELPHIFIELD]);
end;

function TMoldAttribute.GetLength: Integer;
begin
  result := StrToIntDef(BoldTVByName[TAG_LENGTH], 255);
end;

function TMoldAttribute.GetDefaultDBValue: string;
begin
  result := BoldTVByName[TAG_DEFAULTDBVALUE];
end;

function TMoldAttribute.GetEffectiveDelayedFetch: Boolean;
begin
  result := (EvolutionState = esToBeRemoved) or (inherited GetEffectiveDelayedFetch);
end;

function TMoldAttribute.GetPersistent: Boolean;
begin
  result := SameText(StdTVByName[TAG_PERSISTENCE], TV_PERSISTENCE_PERSISTENT);
end;

function TMoldAttribute.GetUMLClassName: string;
begin
  result := 'Attribute';
end;

function TMoldAssociation.GetPersistent: Boolean;
begin
  result := SameText(StdTVByName[TAG_PERSISTENCE], TV_PERSISTENCE_PERSISTENT);
end;

function TMoldRole.GetEffectiveEmbed: Boolean;
begin
  result := (RoleType = rtInnerLinkRole) or
            (Embed and not multi and not assigned(Association.LinkClass));
end;

function TMoldRole.GetEffectiveOrdered: Boolean;
begin
  result := Ordered and Multi;
end;

function TMoldModel.GetUseGlobalId: Boolean;
begin
  result := TVIsTrue(BoldTVByName[TAG_USEGLOBALID]);
end;

function TMoldModel.GetUseReadOnly: Boolean;
begin
  result := TVIsTrue(BoldTVByName[TAG_USEREADONLY]);
end;

function TMoldModel.GetUseTimestamp: Boolean;
begin
  result := TVIsTrue(BoldTVByName[TAG_USETIMESTAMP]);
end;

function TMoldModel.GetUseXFiles: Boolean;
begin
  result := TVIsTrue(BoldTVByName[TAG_USEXFILES]);
end;


function TMoldModel.GetImplementationUses: string;
begin
  result := BoldTVByName[TAG_IMPLEMENTATIONUSES];
end;

function TMoldRole.GetEffectiveDeleteAction: TDeleteAction;
begin
  if DeleteAction = daDefault then
    result := Model.defaultDeleteAction[Aggregation]
  else
    result := DeleteAction;
end;

function TMoldModel.GetGUID: string;
begin
  result := BoldTVByName[TAG_GUID];
end;

function TMoldModel.GetTypeLibVersion: String;
begin
  result := BoldTVByName[TAG_TYPELIBVERSION];
end;

function TMoldMethod.GetOverrideInAllSubclasses: Boolean;
begin
  result := TVIsTrue(BoldTVByName[TAG_OVERRIDEINALLSUBCLASSES]);
end;

function TMoldMethod.GetHasDispId: Boolean;
var
  i: integer;
  dummy: Boolean;
begin
  Result := False;
  case Model.fDispIdAssigningState of
    dasNotStarted:
    begin
      Model.AssignDispIds;
      result := fDispId <> -1;
    end;
    dasAssigning:
    begin
      result := (FuncType in [dfNormal, dfVirtual, dfDynamic, dfAbstractVirtual]) and
             (Visibility = vkPublic);
      if Assigned(fParameters) then
        for i := 0 to fParameters.Count -1 do
          result := result and (TBoldMetaSupport.ParameterTypeToIDLType(TMoldParameter(fParameters[i]).ParameterType, MoldClass.Model, dummy) <> '');
      result := result and ((returnType = '') or (TBoldMetaSupport.ParameterTypeToIDLType(returnType, MoldClass.Model, dummy) <> ''));
    end;

    dasDone: result := fDispId <> -1;
  end;
end;

function TMoldElement.GetStdTVByName(const Tag: string): string;
begin
  if assigned(fTaggedValues) then
    Result := TaggedValues.ValueByName[Tag]
  else
    result := '';
end;

function TMoldModel.GetOptimisticLocking: TBoldOptimisticLockingMode;
begin
  Result := TBoldTaggedValueSupport.StringToOptimisticLockingMode(BoldTVByName[TAG_OPTIMISTICLOCKING]);
end;

function TMoldModel.GetUseClockLog: Boolean;
begin
  result := TVIsTrue(BoldTVByName[TAG_USECLOCKLOG]);
end;

function TMoldModel.GetUpdateWholeObjects: Boolean;
begin
  result := TVIsTrue(BoldTVByName[TAG_UPDATEWHOLEOBJECTS]);
end;

function TMoldAttribute.GetTypeStreamName: string;
begin
  result := Model.TypeNameDictionary.MappingForModelName[BoldType].ExpandedContentsName;
end;

function TMoldRole.GetTypeStreamName: string;
begin
  if Multi then
  begin
    if assigned(Association.LinkClass) then
      result := BoldContentName_ObjectIdListRefPair
    else
      result := BoldContentName_ObjectIdListRef;
  end else
  begin
    if assigned(Association.LinkClass) and (RoleType <> rtInnerLinkRole) then
      result := BoldContentName_ObjectIdRefPair
    else
      result := BoldContentName_ObjectIdRef;
  end;
end;

function TMoldModel.GetExpandedUnitName: string;
begin
  result := BoldExpandName(BoldUnitName, Name, xtDelphi, -1, Model.NationalCharConversion);
end;

procedure TMoldElement.SetStdTVByName(const Tag, Value: string);
begin
  TVByName[Tag] := Value;
end;

function TMoldElement.GetEvolutionState: TBoldEvolutionState;
begin
  result := TBoldTaggedValueSupport.StringToEvolutionState(BoldTVByName[TAG_EVOLUTIONSTATE]);
end;

function TMoldElement.GetFormerNames: string;
begin
  result := GetBoldTVByName(TAG_FORMERNAMES);
end;

procedure TMoldModel.RemoveClass(MoldClass: TMoldClass; ClassList: TMoldClassList; AssocList: TMoldAssociationList);
var
  i: integer;
begin
  if ClassList.IndexOf(MoldClass) = -1 then
  begin
    ClassList.Add(MoldClass);
    for i := 0 to MoldClass.Roles.Count-1 do
      RemoveAssoc(MoldClass.Roles[i].Association, ClassList, AssocList);
    if assigned(MoldClass.Association) then
      RemoveAssoc(MoldClass.Association, ClassList, AssocList);
  end;
end;

procedure TMoldModel.RemoveAssoc(MoldAssoc: TMoldAssociation; ClassList: TMoldClassList; AssocList: TMoldAssociationList);
begin
  if AssocList.IndexOf(MoldAssoc) = -1 then
  begin
    AssocList.Add(MoldAssoc);
    if assigned(MoldAssoc.LinkClass) then
      RemoveClass(MoldAssoc.LinkClass, ClassList, AssocList);
  end;
end;

function TMoldModel.TrimRemoved: boolean;
var
  ClassList: TMoldClassList;
  AssocList: TMoldAssociationList;
  procedure RemoveClass(MoldClass: TMoldClass; ClassList: TMoldClassList; AssocList: TMoldAssociationList);
  var
    i: integer;
  begin
    if not ClassList.Includes(MoldClass)then
    begin
      ClassList.Add(MoldClass);
      for i := 0 to MoldClass.Roles.Count-1 do
        RemoveAssoc(MoldClass.Roles[i].Association, ClassList, AssocList);
      if assigned(MoldClass.Association) then
        RemoveAssoc(MoldClass.Association, ClassList, AssocList);
    end;
  end;

  procedure RemoveAssoc(MoldAssoc: TMoldAssociation; ClassList: TMoldClassList; AssocList: TMoldAssociationList);
  begin
    if not AssocList.Includes(MoldAssoc) then
    begin
      AssocList.Add(MoldAssoc);
      if assigned(MoldAssoc.LinkClass) then
        RemoveClass(MoldAssoc.LinkClass, ClassList, AssocList);
    end;
  end;
var
  i: integer;
begin
  if fLinkRolesEnsured then
    raise EBold.Create(sCannotTrimAfterLinkRolesEnsured);

  ClassList := TMoldClassList.Create;
  AssocList := TMoldAssociationList.Create;

  try
    for i := classes.count-1 downto 0 do
      if Classes[i].EvolutionState = esRemoved then
        RemoveClass(Classes[i], ClassList, AssocList);

    for i := Associations.count-1 downto 0 do
      if Associations[i].EvolutionState = esRemoved then
        RemoveAssoc(Associations[i], ClassList, AssocList);
  finally
    result := (not ClassList.IsEmpty) or (not AssocList.IsEmpty);
    FreeAndNil(ClassList);
    freeAndNil(AssocList);
  end;

  for i := 0 to Classes.Count-1 do
    Classes[i].trimRemoved;
end;

procedure TMoldModel.UpdateMemberIndexes;
var
  i: integer;
  MoldClass: TMoldClass;
  j: Integer;
begin
  for i := 0 to Classes.Count-1 do
  begin
    MoldClass := Classes[i];
    for j := 0 to MoldClass.AllBoldMembers.Count -1 do
      MoldClass.AllBoldMembers[j].fIndex := j;
  end;
end;

function TMoldModel.GetUMLClassName: string;
begin
  result := 'Model';
end;

function TMoldRole.GetEffectiveDelayedFetch: Boolean;
begin
  result := (roleType = rtLinkRole) or
    Multi or
    (not EffectiveEmbedded) or
    (Association.EvolutionState = esToBeRemoved) or
    Inherited GetEffectiveDelayedFetch;
end;

function TMoldModel.GetUseModelVersion: Boolean;
begin
  result := TVIsTrue(BoldTVByName[TAG_USEMODELVERSION]);
end;

function TMoldModel.GetModelVersion: Integer;
begin
  result := StrToIntDef(BoldTVByName[TAG_MODELVERSION], -1);
end;

function TMoldElement.GetHasDispId: boolean;
begin
  Result := false;
end;

procedure TMoldModel.EnsureTopSorted;
var
  ClassIx, SubClassIx: Integer;
begin
  if Topsorted then
    Exit;
  Classes.OwnsEntries := false;
  Classes.Clear;
  Classes.OwnsEntries := true;
  Classes.Add(RootClass);
  ClassIx := 0;
  while ClassIx < Classes.Count do
  begin
    Classes[ClassIx].SubClasses.Sort(ClassesComparer);
    for SubClassIx := 0 to Classes[ClassIx].SubClasses.Count-1 do
      Classes.Add(Classes[ClassIx].SubClasses[SubClassIx]);
    Classes[ClassIx].fTopSortedIndex := ClassIx;
    inc(ClassIx);
  end;
  fTopSorted := True;
end;

procedure TMoldModel.AssignDispIds;
var
  i: integer;
begin
  fDispIdAssigningState := dasAssigning;
  EnsureTopSorted;
  for i := 0 to Classes.Count-1 do
    Classes[i].AssignMemberDispIds;
  fDispIdAssigningState := dasDone;
end;

function TMoldElement.GetDispId: integer;
begin
  if not HasDispId then
    raise EBold.Create(sMemberHasNoDispID);
  if not Model.DispIdsAssigned then
    Model.AssignDispIds;
  Result := fDispId;
end;

function TMoldAttribute.GetHasDispId: boolean;
begin
  Result := False;
  case Model.fDispIdAssigningState of
    dasNotStarted:
    begin
      Model.AssignDispIds;
      result := fDispId <> -1;
    end;
    dasAssigning: result := MemberExists and (AttributeKind = bastBold);
    dasDone: result := fDispId <> -1;
  end;
end;

function TMoldAttribute.GetIsAttribute: boolean;
begin
  result := true;
end;

function TMoldAttribute.GetIsRole: boolean;
begin
  result := false;
end;

function TMoldRole.GetHasDispId: boolean;
begin
  Result := False;
  case Model.fDispIdAssigningState of
    dasNotStarted:
    begin
      Model.AssignDispIds;
      result := fDispId <> -1;
    end;
    dasAssigning: result := MemberExists;
    dasDone: result := fDispId <> -1;
  end;
end;

function TMoldModel.GetNationalCharConversion: TBoldNationalCharConversion;
begin
  result := TBoldTaggedValueSupport.StringToNationalCharConversion(BoldTVByName[TAG_NATIONALCHARCONVERSION]);
end;

function TMoldModel.GetDispIdsAssigned: Boolean;
begin
  result := fDispIdAssigningState = dasDone;
end;

{ TMoldAssociation }

function TMoldAssociation.GetAllPossibleNames: TStringList;
var
  RoleNames1, RoleNames2: TStringList;
  Rn1Ix, Rn2Ix: integer;
  AssociationNameImplicit: Boolean;
  LinkClassImplicit: Boolean;
begin
  if not assigned(fAllPossibleNames) then
  begin
    fAllPossibleNames := TStringList.Create;
    fAllPossibleNames.Sorted := true;
    fAllPossibleNames.Duplicates := dupIgnore;
    fAllPossibleNames.CommaText := FormerNames;
    fAllPossibleNames.Add(ExpandedExpressionName);

    AssociationNameImplicit :=
      SameText(Name, Roles[0].Name + Roles[1].Name) or
      SameText(Name, Roles[1].Name + Roles[0].Name);

    LinkClassImplicit := not assigned(LinkClass) or SameText(LinkClass.name, Name);

    if AssociationNameImplicit and LinkClassImplicit and
    ((roles[0].FormerNames <> '') or (roles[1].formernames <> '')) then
    begin
      RoleNames1 := TStringList.Create;
      RoleNames2 := TStringList.Create;
      RoleNames1.CommaText := Roles[0].FormerNames;
      RoleNames1.Add(Roles[0].Name);
      RoleNames2.CommaText := Roles[1].FormerNames;
      RoleNames2.Add(Roles[1].Name);
      try
        for Rn1Ix := 0 to Rolenames1.Count-1 do
          for Rn2Ix := 0 to RoleNames2.Count-1 do
            if not ((Rn1Ix = Rolenames1.Count-1) and (Rn2Ix = Rolenames2.Count-1)) then
            begin
              fAllPossibleNames.Add(RoleNames1[Rn1Ix]+Rolenames2[Rn2Ix]);
              fAllPossibleNames.Add(RoleNames2[Rn2Ix]+Rolenames1[Rn1Ix]);
            end;
      finally
        RoleNames1.Free;
        RoleNames2.Free;
      end;
    end;
  end;
  result := fAllPossibleNames;
end;

function TMoldAssociation.GetStorage: TBoldStorage;
begin
  result := TBoldTaggedValueSupport.StringToStorage(BoldTVByName[TAG_STORAGE]);
end;

{ TMoldParameter }

constructor TMoldParameter.Create(OwningMethod: TMoldMethod);
begin
  inherited Create;
  fOwningMethod := OwningMethod;
end;

function TMoldParameter.GetDelphiParameterType: String;
var
  PossibleType: TMoldClass;
begin
  PossibleType := fOwningMethod.MoldClass.Model.Classes.ItemsByName[ParameterType];
  if assigned(PossibleType) then
    Result := PossibleType.ExpandedDelphiName
  else
    result := ParameterType;
end;

function TMoldMethod.GetDelphiReturnType: String;
var
  PossibleType: TMoldClass;
begin
  PossibleType := MoldClass.Model.Classes.ItemsByName[ReturnType];
  if assigned(PossibleType) then
    Result := PossibleType.ExpandedDelphiName
  else
    result := ReturnType;
end;

function TMoldMethod.GetHasReturnValue: Boolean;
begin
  Result := BoldTrim(ReturnType) <> '';
end;

function TMoldRole.GetQualifiedMulti: Boolean;
begin
  result := (Qualifiers.Count > 0) and (GetUpperLimitForMultiplicity(Multiplicity) > 1);
end;

function TMoldModel.CalculateCRC: Cardinal;
var
  i: integer;
begin
  EnsureTopSorted;
  result := 0;
  for i := 0 to Classes.Count-1 do
    result := SumCRCs(result, Classes[i].CalculateCRC);
end;

function TMoldAttribute.CalculateCRC: Cardinal;
begin
  result := SumCRCs(
    CRCHash(ExpandedDelphiName),
    CRCHash(BoldType));
end;

function TMoldRole.CalculateCRC: Cardinal;
begin
  result := SumCRCs(
    CRCHash(ExpandedDelphiName),
    CRCHash(Multiplicity));
  result := SumCRCs(result,
    CRCHash(OtherEnd.MoldClass.ExpandedDelphiName));
end;

function TMoldModel.CRC: string;
begin
  EnsureCRC;
  result := BoldTVByName['CRC'];
end;

procedure TMoldModel.EnsureCRC;
begin
  if BoldTVByName['CRC'] = '' then
    BoldTVByName['CRC'] := intToStr(CalculateCRC);
end;

function TMoldModel.GetDefaultDeleteAction(AggregationKind: TAggregationKind): TDeleteAction;
begin
  result := fDefaultDeleteAction[AggregationKind];
end;

procedure TMoldModel.SetDefaultDeleteAction(AggregationKind: TAggregationKind; const Value: TDeleteAction);
begin
  fDefaultDeleteAction[AggregationKind] := Value;
end;

function TMoldModel.GetRegionDefinitions: String;
begin
  result := BoldTVByName[TAG_REGIONDEFINITIONS];
end;

function TMoldModel.GetGenerateDefaultRegions: Boolean;
begin
  result := TVIsTrue(BoldTVByName[TAG_GENERATEDEFAULTREGIONS]);
end;

function TMoldRole.GetDefaultRegionMode: TBoldAssociationEndDefaultRegionMode;
begin
  result := TBoldTaggedValueSupport.StringToDefaultRegionMode(BoldTVByName[TAG_DEFAULTREGIONMODE_ASSOCIATIONEND]);
end;

function TMoldRole.GetEffectiveDefaultRegionMode: TBoldAssociationEndDefaultRegionMode;
begin
  result := DefaultRegionMode;
  if result = aedrmDefault then
  begin
    if Aggregation in [akAggregate, akComposite] then
      result := aedrmCascade
    else if not Multi then
      result := aedrmIndependentCascade
    else
      result := aedrmNone
  end;
end;

function TMoldMethod.GetCanCallInherited: Boolean;
var
  tempSuperClass: TMoldClass;
  SuperMethod: TMoldMethod;
begin
  result := false;
  if FuncType in [dfOverride, dfDynamic] then
  begin
    TempSuperClass := MoldClass.SuperClass;
    SuperMethod := nil;
    while assigned(TempSuperClass) and not assigned(SuperMethod) do
    begin
      Supermethod := TempSuperClass.Methods.ItemsByName[Name];
      TempSuperClass := TempSuperClass.SuperClass;
    end;
    result := assigned(Supermethod) and not (SuperMethod.FuncType = dfAbstractVirtual);
  end;
end;

function TMoldElement.GetDefaultBoldTVList: TBoldTaggedValueList;
  procedure DoGet;
  begin
    fDefaultBoldTVList := BoldDefaultTaggedValueList.ListForClassName[UMLClassName];
  end;
begin
  Result := fDefaultBoldTVList;
  if not Assigned(Result) then
  begin
    DoGet;
    Result :=  fDefaultBoldTVList;
  end;
  Assert(Assigned(Result), Format('Default TaggedVaulesList for %s not assigned', [UMLClassName]));
end;

function TMoldElement.GetTVByName(const Tag: string): string;
begin
   if BoldIsPrefix(Tag, BOLDTVPREFIX) then
     Result := BoldTVByName[Copy(Tag, Length(BOLDTVPREFIX) +1, MaxInt)]
   else if Assigned(fTaggedValues) and Assigned(TaggedValues.ItemByName[Tag]) then
     Result := TaggedValues.ValueByName[Tag]
   else
     Result := '';
end;

procedure TMoldElement.SetTVByName(const Tag, Value: string);
var
  TrimmedValue: string;
begin
  TrimmedValue := BoldTrim(Value);
  if BoldIsPrefix(Tag, BOLDTVPREFIX) then
    BoldTVByName[Copy(Tag, Length(BOLDTVPREFIX) +1, MaxInt)] := TrimmedValue
  else
    TaggedValues.ValueByName[Tag] := TrimmedValue;
end;

function TMoldElement.GetTaggedValues: TBoldNamedValueList;
begin
  if not Assigned(fTaggedValues) then
    fTaggedValues :=  TBoldNamedValueList.Create;
  Result := fTaggedValues;
end;

function TMoldElement.GetBoldTaggedValues: TBoldNamedValueList;
begin
  if not Assigned(fBoldTaggedValues) then
    fBoldTaggedValues :=  TBoldNamedValueList.Create;
  Result := fBoldTaggedValues;
end;

function TMoldElement.GetNonDefaultTaggedValuesCommaText: string;
var
  i: integer;
  StringList: TStringList;
  G: IBoldGuard;
begin
  G := TBoldGuard.Create(StringList);
  StringList := TStringList.Create;
  if Assigned(fTaggedValues) then
    TaggedValues.AddToStrings(StringList);
  if Assigned(fBoldTaggedValues) then
    for i := 0 to BoldTaggedValues.Count-1 do
      StringList.Add(BOLDTVPREFIX + BoldTaggedValues[i].Name + '=' + BoldTaggedValues[i].Value);
  Result := StringList.CommaText;
end;

procedure TMoldElement.SetNonDefaultTaggedValuesCommaText(
  const Value: string);
var
  i: integer;
  StringList: TStringList;
  Line: string;
  EqualPos: integer;
  Name: string;
  G: IBoldGuard;
begin
  G := TBoldGuard.Create(StringList);
  StringList := TStringList.Create;
  Stringlist.CommaText := Value;
  if StringList.Count = 0 then
    exit;
  TaggedValues.Capacity := StringList.Count;
  for i := 0 to StringList.Count-1 do
  begin
    Line := StringList[i];
    EqualPos := Pos('=', Line);
    Assert(EqualPos <> 0);
    Name := Copy(Line, 1, EqualPos-1);
    if Name = 'Persistence' then
      Name := TAG_PERSISTENCE;
    TVByName[Name] :=  Copy(Line, EqualPos + 1, MaxInt);
  end;
end;

procedure TMoldElement.AddAllTaggedValues(Tags, Values: TStrings);
var
  i: integer;
  Tag: string;
  NamedValueListEntry: TBoldNamedValueListEntry;
begin
  Values.Capacity := DefaultBoldTVList.Count;
  Tags.Capacity := DefaultBoldTVList.Count;
  for i := 0 to DefaultBoldTVList.Count-1 do
  begin
    Tag := DefaultBoldTVList.Definition[i].Tag;
    Tags.Add(BOLDTVPREFIX + DefaultBoldTVList.Definition[i].Tag);
    if Assigned(fBoldTaggedValues) then
      NamedValueListEntry := BoldTaggedValues.ItemByName[Tag]
    else
      NamedValueListEntry := nil;
    if Assigned(NamedValueListEntry) then
      Values.Add(NamedValueListEntry.Value)
    else
      Values.Add(DefaultBoldTVList.Definition[i].DefaultValue);
  end;
  if Assigned(fTaggedValues) then
    for i := 0 to TaggedValues.Count-1 do
    begin
      Tags.Add(TaggedValues.Items[i].Name);
      Values.Add(TaggedValues[i].Value);
    end;
end;

function TMoldAttribute.GetStorage: TBoldStorage;
begin
  result := TBoldTaggedValueSupport.StringToStorage(BoldTVByName[TAG_STORAGE]);
end;

function TMoldRole.GetStorage: TBoldStorage;
begin
  result := Association.Storage;
end;

{ TMoldElementListTraverser }

function TMoldElementListTraverser.GetCurrent: TMoldElement;
begin
  result := inherited GetItem as TMoldElement;
end;

{ TMoldClassListTraverser }

function TMoldClassListTraverser.GetCurrent: TMoldClass;
begin
  result := inherited GetItem as TMoldClass;
end;

{ TMoldMemberListTraverser }

function TMoldMemberListTraverser.GetCurrent: TMoldMember;
begin
  result := inherited GetItem as TMoldMember;
end;

{ TMoldRoleListTraverser }

function TMoldRoleListTraverser.GetCurrent: TMoldRole;
begin
  result := inherited GetItem as TMoldRole;
end;

{ TMoldAttributeListTraverser }

function TMoldAttributeListTraverser.GetCurrent: TMoldAttribute;
begin
  result := inherited GetItem as TMoldAttribute;
end;

{ TMoldMethodListTraverser }

function TMoldMethodListTraverser.GetCurrent: TMoldMethod;
begin
  result := inherited GetItem as TMoldMethod;
end;

{ TMoldQualifierListTraverser }

function TMoldQualifierListTraverser.GetCurrent: TMoldQualifier;
begin
  result := inherited GetItem as TMoldQualifier;
end;

{ TMoldAssociationListTraverser }

function TMoldAssociationListTraverser.GetCurrent: TMoldAssociation;
begin
  result := inherited GetItem as TMoldAssociation;
end;

initialization
  TMoldElementList.IX_Name := -1;
  TMoldClassList.IX_ExpressionName := -1;
  TMoldClassList.IX_DelphiName := -1;

end.
