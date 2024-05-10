
{ Global compiler directives }
{$include bold.inc}
unit BoldComObjectSpaceAdapters;

interface

uses
  ActiveX,
  BoldSubscription,
  BoldElements,
  BoldDomainElement,
  BoldSystemRT,
  BoldSystem,
  BoldAttributes,
  BoldComAdapter,
  BoldComObjectSpace_TLB,
  BoldHandles,
  BoldSystemHandle,
  BoldDerivedHandle,
  BoldExpressionHandle,
  BoldCursorHandle,
  BoldListHandle,
  BoldReferenceHandle,
  BoldSQLHandle,
  BoldVariableHandle;

type
  { forward declarations }
  TBoldComElementAdapter = class;
  TBoldComMetaElementAdapter = class;
  TBoldComElementTypeInfoAdapter = class;
  TBoldComTypeTypeInfoAdapter = class;
  TBoldComClassTypeInfoAdapter = class;
  TBoldComNilTypeInfoAdapter = class;
  TBoldComListTypeInfoAdapter = class;
  TBoldComAttributeTypeInfoAdapter = class;
  TBoldComSystemTypeInfoAdapter = class;
  TBoldComDomainElementAdapter = class;
  TBoldComObjectAdapter = class;
  TBoldComMemberAdapter = class;
  TBoldComAttributeAdapter = class;
  TBoldComObjectReferenceAdapter = class;
  TBoldComListAdapter = class;
  TBoldComObjectListAdapter = class;
  TBoldComMemberListAdapter = class;
  TBoldComSystemAdapter = class;
  TBoldComBlobAdapter = class;
  TBoldComElementHandleAdapter = class;
  TBoldComSystemHandleAdapter = class;
  TBoldComDerivedHandleAdapter = class;
  TBoldComExpressionHandleAdapter = class;
  TBoldComCursorHandleAdapter = class;
  TBoldComListHandleAdapter = class;
  TBoldComReferenceHandleAdapter = class;
  TBoldComSQLHandleAdapter = class;
  TBoldComVariableHandleAdapter = class;

  TBoldComElementAdapter = class(TBoldComAdapter, IBoldElement)
  private
    function GetAsElement: TBoldElement;
  protected
    { IBoldElement }
    procedure AddSmallSubscription(const ClientId: WideString; SubscriberId: Integer;
                                   SmallEvents: Integer; RequestedEvent: Integer;
                                   CancelOld: WordBool); safecall;
    procedure AddSubscription(const ClientId: WideString; SubscriberId: Integer; Event: Integer;
                              RequestedEvent: Integer; CancelOld: WordBool); safecall;
    procedure AssignElement(const Element: IBoldElement); safecall;
    function  CompareTo(const Element: IBoldElement): Integer; safecall;
    function  CompareToAs(CompareType: Integer; const Element: IBoldElement): Integer; safecall;
    procedure DefaultSubscribe(const ClientId: WideString; SubscriberId: Integer;
                               RequestedEvent: Integer; CancelOld: WordBool); safecall;
    function  EvaluateAndSubscribeToExpression(const Expression: WideString;
                                               const ClientId: WideString; SubscriberId: Integer;
                                               Resubscribe: WordBool; CancelOld: WordBool): IBoldElement; safecall;
    function  EvaluateExpression(const Expression: WideString): IBoldElement; safecall;
    function  EvaluateExpressionAsSessionIdList(const Expression: WideString): OleVariant; safecall;
    function  EvaluateExpressionAsSessionIdListAndStringList(const ListExpression: WideString;
                                                             const ItemExpression: WideString;
                                                             Representation: Integer): OleVariant; safecall;
    function  EvaluateExpressionAsString(const Expression: WideString; Representation: Integer): WideString; safecall;
    function  EvaluateExpressionAsStringList(const Expression: WideString; Representation: Integer): OleVariant; safecall;
    function  MultiEvaluateExpressionAsStringList(Elements: OleVariant;
                                                  const Expression: WideString;
                                                  const ClientId: WideString;
                                                  SubscriberIds: OleVariant; Representation: Integer): OleVariant; safecall;
    function  EvaluateExpressionsAsStringLists(Expressions: OleVariant; Representation: Integer): OleVariant; safecall;
    function  GetAsList: IBoldList; safecall;
    function  IsEqual(const Element: IBoldElement): WordBool; safecall;
    function  IsEqualAs(CompareType: Integer; const Element: IBoldElement): WordBool; safecall;
    procedure MakeImmutable; safecall;
    procedure SubscribeToExpression(const Expression: WideString; const ClientId: WideString;
                                    SubscriberId: Integer; Resubscribe: WordBool;
                                    CancelOld: WordBool); safecall;
    procedure SubscribeToStringRepresentation(Representation: Integer; const ClientId: WideString;
                                              SubscriberId: Integer; RequestedEvent: Integer;
                                              CancelOld: WordBool); safecall;
    function  ValidateCharacter(const Value: WideString; Representation: Integer): WordBool; safecall;
    function  ValidateString(const Value: WideString; Representation: Integer): WordBool; safecall;
    function  Get_AsString: WideString; safecall;
    procedure Set_AsString(const Value: WideString); safecall;
    function  Get_AsVariant: OleVariant; safecall;
    procedure Set_AsVariant(Value: OleVariant); safecall;
    function  Get_BoldType: IBoldElementTypeInfo; safecall;
    function  Get_BoldTypeName: WideString; safecall;
    function  Get_Mutable: WordBool; safecall;
    function  Get_StringRepresentation(Representation: Integer): WideString; safecall;
    procedure Set_StringRepresentation(Representation: Integer; const Value: WideString); safecall;
    function  Get__NewEnum: IUnknown; safecall;
    function  Get_HasAdaptee: WordBool; safecall;
  public
    constructor Create(AdaptableObject: TBoldAdaptableObject; Owner: Boolean;
      const TypeLib: ITypeLib; const DispIntf: TGUID); override;
    property AsElement: TBoldElement read GetAsElement;
  end;

  TBoldComMetaElementAdapter = class(TBoldComElementAdapter,IBoldMetaElement)
  private
    function GetAsMetaElement: TBoldMetaElement;
  protected
    { IBoldMetaElement }
    function  Get_DelphiName: WideString; safecall;
    function  Get_ExpressionName: WideString; safecall;
    function  Get_ModelName: WideString; safecall;
  public
    constructor Create(AdaptableObject: TBoldAdaptableObject; Owner: Boolean;
      const TypeLib: ITypeLib; const DispIntf: TGUID); override;
    property AsMetaElement: TBoldMetaElement read GetAsMetaElement;
  end;

  TBoldComElementTypeInfoAdapter = class(TBoldComMetaElementAdapter,IBoldElementTypeInfo)
  private
    function GetAsElementTypeInfo: TBoldElementTypeInfo;
  protected
    { IBoldElementTypeInfo }
    function  ConformsTo(const ElementTypeInfo: IBoldElementTypeInfo): WordBool; safecall;
    function  Get_BoldValueType: Integer; safecall;
    function  Get_SystemTypeInfo: IBoldSystemTypeInfo; safecall;
  public
    constructor Create(AdaptableObject: TBoldAdaptableObject; Owner: Boolean;
      const TypeLib: ITypeLib; const DispIntf: TGUID); override;
    property AsElementTypeInfo: TBoldElementTypeInfo read GetAsElementTypeInfo;
  end;

  TBoldComTypeTypeInfoAdapter = class(TBoldComElementTypeInfoAdapter,IBoldTypeTypeInfo)
  private
    function GetAsTypeTypeInfo: TBoldTypeTypeInfo;
  protected
    { IBoldTypeTypeInfo }
  public
    constructor Create(AdaptableObject: TBoldAdaptableObject; Owner: Boolean;
      const TypeLib: ITypeLib; const DispIntf: TGUID); override;
    property AsTypeTypeInfo: TBoldTypeTypeInfo read GetAsTypeTypeInfo;
  end;

  TBoldComClassTypeInfoAdapter = class(TBoldComElementTypeInfoAdapter,IBoldClassTypeInfo)
  private
    function GetAsClassTypeInfo: TBoldClassTypeInfo;
  protected
    { IBoldClassTypeInfo }
    function  LeastCommonSuperClass(const ClassTypeInfo: IBoldClassTypeInfo): IBoldClassTypeInfo; safecall;
    function  Get_Constraints: OleVariant; safecall;
    function  Get_FirstOwnMemberIndex: Integer; safecall;
    function  Get_HasSubClasses: WordBool; safecall;
    function  Get_IsAbstract: WordBool; safecall;
    function  Get_IsImported: WordBool; safecall;
    function  Get_IsLinkClass: WordBool; safecall;
    function  Get_IsPersistent: WordBool; safecall;
    function  Get_ListTypeInfo: IBoldListTypeInfo; safecall;
    function  Get_Stereotype: WideString; safecall;
    function  Get_SuperClassTypeInfo: IBoldClassTypeInfo; safecall;
    function  Get_TaggedValue(const Tag: WideString): WideString; safecall;
    function  Get_TopSortedIndex: Integer; safecall;
 public
    constructor Create(AdaptableObject: TBoldAdaptableObject; Owner: Boolean;
      const TypeLib: ITypeLib; const DispIntf: TGUID); override;
    property AsClassTypeInfo: TBoldClassTypeInfo read GetAsClassTypeInfo;
  end;

  TBoldComNilTypeInfoAdapter = class(TBoldComClassTypeInfoAdapter,IBoldNilTypeInfo)
  private
    function GetAsNilTypeInfo: TBoldNilTypeInfo;
  protected
    { IBoldNilTypeInfo }
  public
    constructor Create(AdaptableObject: TBoldAdaptableObject; Owner: Boolean;
      const TypeLib: ITypeLib; const DispIntf: TGUID); override;
    property AsNilTypeInfo: TBoldNilTypeInfo read GetAsNilTypeInfo;
  end;

  TBoldComListTypeInfoAdapter = class(TBoldComElementTypeInfoAdapter,IBoldListTypeInfo)
  private
    function GetAsListTypeInfo: TBoldListTypeInfo;
  protected
    { IBoldListTypeInfo }
    function  Get_ListElementTypeInfo: IBoldElementTypeInfo; safecall;
  public
    constructor Create(AdaptableObject: TBoldAdaptableObject; Owner: Boolean;
      const TypeLib: ITypeLib; const DispIntf: TGUID); override;
    property AsListTypeInfo: TBoldListTypeInfo read GetAsListTypeInfo;
  end;

  TBoldComAttributeTypeInfoAdapter = class(TBoldComElementTypeInfoAdapter,IBoldAttributeTypeInfo)
  private
    function GetAsAttributeTypeInfo: TBoldAttributeTypeInfo;
  protected
    { IBoldAttributeTypeInfo }
    function Get_SuperAttributeTypeInfo: IBoldAttributeTypeInfo; safecall;
  public
    constructor Create(AdaptableObject: TBoldAdaptableObject; Owner: Boolean;
      const TypeLib: ITypeLib; const DispIntf: TGUID); override;
    property AsAttributeTypeInfo: TBoldAttributeTypeInfo read GetAsAttributeTypeInfo;
  end;

  TBoldComSystemTypeInfoAdapter = class(TBoldComElementTypeInfoAdapter,IBoldSystemTypeInfo)
  private
    function GetAsSystemTypeInfo: TBoldSystemTypeInfo;
  protected
    { IBoldSystemTypeInfo }
    function  Get_AttributeTypeInfoByExpressionName(const Name: WideString): IBoldAttributeTypeInfo; safecall;
    function  Get_AttributeTypes: IUnknown; safecall;
    function  Get_ClassTypeInfoByExpressionName(const Name: WideString): IBoldClassTypeInfo; safecall;
    function  Get_ClassTypes: IUnknown; safecall;
    function  Get_Constraints: OleVariant; safecall;
    function  Get_ElementTypeInfoByExpressionName(const Name: WideString): IBoldElementTypeInfo; safecall;
    function  Get_IsPersistent: WordBool; safecall;
    function  Get_IsRunnable: WordBool; safecall;
    function  Get_ListTypeInfoByElement(const ElementTypeInfo: IBoldElementTypeInfo): IBoldListTypeInfo; safecall;
    function  Get_ListTypes: IUnknown; safecall;
    function  Get_MethodsInstalled: WordBool; safecall;
    function  Get_NilTypeInfo: IBoldNilTypeInfo; safecall;
    function  Get_RootClassTypeInfo: IBoldClassTypeInfo; safecall;
    function  Get_Stereotype: WideString; safecall;
    function  Get_TaggedValue(const Tag: WideString): WideString; safecall;
    function  Get_TopSortedClasses: IUnknown; safecall;
    function  Get_TypeTypeInfo: IBoldTypeTypeInfo; safecall;
    function  Get_UseGeneratedCode: WordBool; safecall;
    function  Get_ValueTypeNameList: IUnknown; safecall;
  public
    constructor Create(AdaptableObject: TBoldAdaptableObject; Owner: Boolean;
      const TypeLib: ITypeLib; const DispIntf: TGUID); override;
    property AsSystemTypeInfo: TBoldSystemTypeInfo read GetAsSystemTypeInfo;
  end;

  TBoldComDomainElementAdapter = class(TBoldComElementAdapter,IBoldDomainElement)
  private
    function GetAsDomainElement: TBoldDomainElement;
  protected
    { IBoldDomainElement }
    function  Get_BoldDirty: WordBool; safecall;
    function  Get_BoldPersistent: WordBool; safecall;
    function  Get_DisplayName: WideString; safecall;
    function  Get_OwningElement: IBoldDomainElement; safecall;
  public
    constructor Create(AdaptableObject: TBoldAdaptableObject; Owner: Boolean;
      const TypeLib: ITypeLib; const DispIntf: TGUID); override;
    property AsDomainElement: TBoldDomainElement read GetAsDomainElement;
  end;

  TBoldComObjectAdapter = class(TBoldComDomainElementAdapter,IBoldObject)
  private
    function GetAsObject: TBoldObject;
  protected
    { IBoldObject }
    procedure BoldMakePersistent; safecall;
    function  CheckLinks(Index: Integer): WordBool; safecall;
    procedure Delete; safecall;
    procedure DiscardChanges; safecall;
    procedure Invalidate; safecall;
    procedure LinkObject(const RoleName: WideString; const BoldObject: IBoldObject); safecall;
    procedure UnlinkAll; safecall;
    procedure UnlinkObject(const RoleName: WideString; const BoldObject: IBoldObject); safecall;
    function  Get_BoldClassTypeInfo: IBoldClassTypeInfo; safecall;
    function  Get_BoldExistenceState: Integer; safecall;
    function  Get_BoldMemberCount: Integer; safecall;
    function  Get_BoldMember(Index: OleVariant): IBoldMember; safecall;
    function  Get_BoldMemberValue(Index: OleVariant): OleVariant; safecall;
    procedure Set_BoldMemberValue(Index: OleVariant; Value: OleVariant); safecall;
    function  Get_BoldMemberValues: OleVariant; safecall;
    procedure Set_BoldMemberValues(Values: OleVariant); safecall;
    function  Get_BoldPersistenceState: Integer; safecall;
    function  Get_BoldSystem: IBoldSystem; safecall;
    function  Get_CanDelete: WordBool; safecall;
    function  Get_CanUpdate: WordBool; safecall;
    function  Get_SessionId: OleVariant; safecall;
  public
    constructor Create(AdaptableObject: TBoldAdaptableObject; Owner: Boolean;
      const TypeLib: ITypeLib; const DispIntf: TGUID); override;
    property AsObject: TBoldObject read GetAsObject;
  end;

  TBoldComMemberAdapter = class(TBoldComDomainElementAdapter,IBoldMember)
  private
    function GetAsMember: TBoldMember;
  protected
    procedure ReceiveEvent(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent); override;
    { IBoldMember }
    function  Clone: IBoldMember; safecall;
    procedure DiscardChanges; safecall;
    procedure EnsureContentsCurrent; safecall;
    procedure Invalidate; safecall;
    function  Get_BoldMemberRTInfo: IUnknown; safecall;
    function  Get_BoldSystem: IBoldSystem; safecall;
    function  Get_CanModify: WordBool; safecall;
    function  Get_CanRead: WordBool; safecall;
    function  Get_Derived: WordBool; safecall;
    function  Get_IsPartOfSystem: WordBool; safecall;
    function  Get_OwningObject: IBoldObject; safecall;

  public
    constructor Create(AdaptableObject: TBoldAdaptableObject; Owner: Boolean;
      const TypeLib: ITypeLib; const DispIntf: TGUID); override;
    property AsMember: TBoldMember read GetAsMember;
  end;

  TBoldComAttributeAdapter = class(TBoldComMemberAdapter,IBoldAttribute)
  private
    function GetAsAttribute: TBoldAttribute;
  protected
    { IBoldAttribute }
    procedure SetToNull; safecall;
    function  Get_BoldAttributeRTInfo: IUnknown; safecall;
    function  Get_CanSetToNull: WordBool; safecall;
    function  Get_IsNull: WordBool; safecall;
  public
    constructor Create(AdaptableObject: TBoldAdaptableObject; Owner: Boolean;
      const TypeLib: ITypeLib; const DispIntf: TGUID); override;
    property AsAttribute: TBoldAttribute read GetAsAttribute;
  end;

  TBoldComObjectReferenceAdapter = class(TBoldComMemberAdapter,IBoldObjectReference)
  private
    function GetAsObjectReference: TBoldObjectReference;
  protected
    { IBoldObjectReference }
    function  CanSet(const NewObject: IBoldObject): WordBool; safecall;
    function  Get_BoldObject: IBoldObject; safecall;
    procedure Set_BoldObject(const Value: IBoldObject); safecall;
    function  Get_BoldRoleRTInfo: IUnknown; safecall;
    function QueryInterface(const IId: TGUID; out Obj): HResult; override; stdcall;
  public
    constructor Create(AdaptableObject: TBoldAdaptableObject; Owner: Boolean;
      const TypeLib: ITypeLib; const DispIntf: TGUID); override;
    property AsObjectReference: TBoldObjectReference read GetAsObjectReference;
  end;

  TBoldComListAdapter = class(TBoldComMemberAdapter,IBoldListCore,IBoldList)
  private
    function GetAsBoldList: TBoldList;
  protected
    function GetVariantCount: Cardinal; override;
    function GetVariantItems(Index: Cardinal): OleVariant; override;
    { IBoldList }
    procedure Add(const Element: IBoldElement); safecall;
    procedure AddList(const List: IBoldListCore); safecall;
    function  AddNew: IBoldElement; safecall;
    function  CanClear: WordBool; safecall;
    function  CanInsert(Index: Integer; const Element: IBoldElement): WordBool; safecall;
    function  CanMove(CurrentIndex: Integer; NewIndex: Integer): WordBool; safecall;
    function  CanRemove(Index: Integer): WordBool; safecall;
    function  CanSet(Index: Integer; const Element: IBoldElement): WordBool; safecall;
    procedure Clear; safecall;
    function  GetRange(FromIndex: Integer; ToIndex: Integer): OleVariant; safecall;
    procedure EnsureRange(FromIndex: Integer; ToIndex: Integer); safecall;
    function  Includes(const Element: IBoldElement): WordBool; safecall;
    function  IndexOf(const Element: IBoldElement): Integer; safecall;
    procedure Insert(Index: Integer; const Element: IBoldElement); safecall;
    procedure InsertNew(Index: Integer); safecall;
    procedure Move(CurrentIndex: Integer; NewIndex: Integer); safecall;
    procedure Remove(const Element: IBoldElement); safecall;
    procedure RemoveByIndex(Index: Integer); safecall;
    function  ToStringList(Representation: Integer): OleVariant; safecall;
    function  ToStringListWithNil(Representation: Integer; const NilString: WideString): OleVariant; safecall;
    function  Get_CanCreateNew: WordBool; safecall;
    function  Get_Count: Integer; safecall;
    function  Get_DuplicateMode: Integer; safecall;
    procedure Set_DuplicateMode(Value: Integer); safecall;
    function  Get_Elements(Index: Integer): IBoldElement; safecall;
    procedure Set_Elements(Index: Integer; const Value: IBoldElement); safecall;
  public
    constructor Create(AdaptableObject: TBoldAdaptableObject; Owner: Boolean;
      const TypeLib: ITypeLib; const DispIntf: TGUID); override;
    property AsBoldList: TBoldList read GetAsBoldList;
  end;

  TBoldComObjectListAdapter = class(TBoldComListAdapter,IBoldObjectList)
  private
    function GetAsObjectList: TBoldObjectList;
  protected
    { IBoldObjectList }
    function  Get_BoldObjects(Index: Integer): IBoldObject; safecall;
    procedure Set_BoldObjects(Index: Integer; const Value: IBoldObject); safecall;
    function  Get_BoldRoleRTInfo: IUnknown; safecall;
    function  Get_SubscribeToObjectsInList: WordBool; safecall;
    procedure Set_SubscribeToObjectsInList(Value: WordBool); safecall;
  public
    constructor Create(AdaptableObject: TBoldAdaptableObject; Owner: Boolean;
      const TypeLib: ITypeLib; const DispIntf: TGUID); override;
    property AsObjectList: TBoldObjectList read GetAsObjectList;
  end;

  TBoldComMemberListAdapter = class(TBoldComListAdapter,IBoldMemberList)
  private
    function GetAsMemberList: TBoldMemberList;
  protected
    { IBoldMemberList }
    function  Get_BoldMembers(Index: Integer): IBoldMember; safecall;
    procedure Set_BoldMembers(Index: Integer; const Value: IBoldMember); safecall;
  public
    constructor Create(AdaptableObject: TBoldAdaptableObject; Owner: Boolean;
      const TypeLib: ITypeLib; const DispIntf: TGUID); override;
    property AsMemberList: TBoldMemberList read GetAsMemberList;
  end;

  TBoldComSystemAdapter = class(TBoldComDomainElementAdapter,IBoldSystem)
  private
    function GetAsSystem: TBoldSystem;
  protected
    { IBoldSystem }
    function  CreateNewMember(const ExpressionName: WideString): IBoldMember; safecall;
    function  CreateNewObject(const ExpressionName: WideString; Persistent: WordBool): IBoldObject; safecall;
    function  CreateNewObjectWithMemberValues(const ExpressionName: WideString;
                                              Persistent: WordBool; MemberValues: OleVariant): IBoldObject; safecall;
    procedure Discard; safecall;
    function  EnsureEnclosure(const ObjectList: IBoldObjectList; ValidateOnly: WordBool): WordBool; safecall;
    function  GetObjectBySessionId(SessionId: OleVariant): IBoldObject; safecall;
    procedure UpdateDatabase; safecall;
    procedure UpdateDatabaseWithList(const ObjectList: IBoldObjectList); safecall;
    function  Get_BoldSystemTypeInfo: IBoldSystemTypeInfo; safecall;
    function  Get_ClassByExpressionName(const ExpressionName: WideString): IBoldObjectList; safecall;
    function  Get_DirtyObjects: IBoldObjectList; safecall;
    function  Get_LoadedObjects: IBoldObjectList; safecall;
  public
    constructor Create(AdaptableObject: TBoldAdaptableObject; Owner: Boolean;
      const TypeLib: ITypeLib; const DispIntf: TGUID); override;
    property AsSystem: TBoldSystem read GetAsSystem;
  end;

  TBoldComBlobAdapter = class(TBoldComAttributeAdapter,IBoldBlob)
  private
    function GetAsBoldBlob: TBABlob;
  protected
    { IBoldBlob }
  public
    constructor Create(AdaptableObject: TBoldAdaptableObject; Owner: Boolean;
      const TypeLib: ITypeLib; const DispIntf: TGUID); override;
    property AsBoldBlob: TBABlob read GetAsBoldBlob;
  end;

  TBoldComElementHandleAdapter = class(TBoldComAdapter,IBoldElementHandle)
  private
    function GetAsElementHandle: TBoldElementHandle;
  protected
    { IBoldElementHandle }
    procedure AddSmallSubscription(const ClientId: WideString; SubscriberId: Integer;
                                   SmallEvents: Integer; RequestedEvent: Integer;
                                   CancelOld: WordBool); safecall;
    procedure AddSubscription(const ClientId: WideString; SubscriberId: Integer; Event: Integer;
                              RequestedEvent: Integer; CancelOld: WordBool); safecall;
    function  GetData(DataFlags: Integer; out Value: IBoldElement;
                      out DynamicBoldType: IBoldElementTypeInfo;
                      out StaticBoldType: IBoldElementTypeInfo;
                      out StaticSystemType: IBoldSystemTypeInfo; out BoldSystem: IBoldSystem;
                      out StaticRootType: IBoldElementTypeInfo;
                      out CurrentBoldObject: IBoldObject;
                      out BoldList: IBoldList; out ListElementType: IBoldElementTypeInfo;
                      out NamedValues: OleVariant): WordBool; virtual; safecall;
    function  SetData(DataFlags: Integer; const Value: IBoldElement; NamedValues: OleVariant): WordBool; virtual; safecall;
  public
    constructor Create(AdaptableObject: TBoldAdaptableObject; Owner: Boolean;
      const TypeLib: ITypeLib; const DispIntf: TGUID); override;
    property AsElementHandle: TBoldElementHandle read GetAsElementHandle;
  end;

  TBoldComSystemHandleAdapter = class(TBoldComElementHandleAdapter)
  private
    function GetAsSystemHandle: TBoldSystemHandle;
  protected
    function  GetData(DataFlags: Integer; out Value: IBoldElement;
                      out DynamicBoldType: IBoldElementTypeInfo;
                      out StaticBoldType: IBoldElementTypeInfo;
                      out StaticSystemType: IBoldSystemTypeInfo; out BoldSystem: IBoldSystem;
                      out StaticRootType: IBoldElementTypeInfo;
                      out CurrentBoldObject: IBoldObject;
                      out BoldList: IBoldList; out ListElementType: IBoldElementTypeInfo;
                      out NamedValues: OleVariant): WordBool; override;
    function  SetData(DataFlags: Integer; const Value: IBoldElement; NamedValues: OleVariant): WordBool; override;
  public
    constructor Create(AdaptableObject: TBoldAdaptableObject; Owner: Boolean;
      const TypeLib: ITypeLib; const DispIntf: TGUID); override;
    property AsSystemHandle: TBoldSystemHandle read GetAsSystemHandle;
  end;

  TBoldComDerivedHandleAdapter = class(TBoldComElementHandleAdapter)
  private
    function GetAsDerivedHandle: TBoldDerivedHandle;
  protected
    function  GetData(DataFlags: Integer; out Value: IBoldElement;
                      out DynamicBoldType: IBoldElementTypeInfo;
                      out StaticBoldType: IBoldElementTypeInfo;
                      out StaticSystemType: IBoldSystemTypeInfo; out BoldSystem: IBoldSystem;
                      out StaticRootType: IBoldElementTypeInfo;
                      out CurrentBoldObject: IBoldObject;
                      out BoldList: IBoldList; out ListElementType: IBoldElementTypeInfo;
                      out NamedValues: OleVariant): WordBool; override;
    function  SetData(DataFlags: Integer; const Value: IBoldElement; NamedValues: OleVariant): WordBool; override;
  public
    constructor Create(AdaptableObject: TBoldAdaptableObject; Owner: Boolean;
      const TypeLib: ITypeLib; const DispIntf: TGUID); override;
    property AsDerivedHandle: TBoldDerivedHandle read GetAsDerivedHandle;
  end;

  TBoldComExpressionHandleAdapter = class(TBoldComElementHandleAdapter)
  private
    function GetAsExpressionHandle: TBoldExpressionHandle;
  protected
    function  GetData(DataFlags: Integer; out Value: IBoldElement;
                      out DynamicBoldType: IBoldElementTypeInfo;
                      out StaticBoldType: IBoldElementTypeInfo;
                      out StaticSystemType: IBoldSystemTypeInfo; out BoldSystem: IBoldSystem;
                      out StaticRootType: IBoldElementTypeInfo;
                      out CurrentBoldObject: IBoldObject;
                      out BoldList: IBoldList; out ListElementType: IBoldElementTypeInfo;
                      out NamedValues: OleVariant): WordBool; override;
    function  SetData(DataFlags: Integer; const Value: IBoldElement; NamedValues: OleVariant): WordBool; override;
  public
    constructor Create(AdaptableObject: TBoldAdaptableObject; Owner: Boolean;
      const TypeLib: ITypeLib; const DispIntf: TGUID); override;
    property AsExpressionHandle: TBoldExpressionHandle read GetAsExpressionHandle;
  end;

  TBoldComCursorHandleAdapter = class(TBoldComElementHandleAdapter)
  private
    function GetAsCursorHandle: TBoldCursorHandle;
  protected
    function  GetData(DataFlags: Integer; out Value: IBoldElement;
                      out DynamicBoldType: IBoldElementTypeInfo;
                      out StaticBoldType: IBoldElementTypeInfo;
                      out StaticSystemType: IBoldSystemTypeInfo; out BoldSystem: IBoldSystem;
                      out StaticRootType: IBoldElementTypeInfo;
                      out CurrentBoldObject: IBoldObject;
                      out BoldList: IBoldList; out ListElementType: IBoldElementTypeInfo;
                      out NamedValues: OleVariant): WordBool; override;
    function  SetData(DataFlags: Integer; const Value: IBoldElement; NamedValues: OleVariant): WordBool; override;
  public
    constructor Create(AdaptableObject: TBoldAdaptableObject; Owner: Boolean;
      const TypeLib: ITypeLib; const DispIntf: TGUID); override;
    property AsCursorHandle: TBoldCursorHandle read GetAsCursorHandle;
  end;

  TBoldComListHandleAdapter = class(TBoldComElementHandleAdapter)
  private
    function GetAsListHandle: TBoldListHandle;
  protected
    function  GetData(DataFlags: Integer; out Value: IBoldElement;
                      out DynamicBoldType: IBoldElementTypeInfo;
                      out StaticBoldType: IBoldElementTypeInfo;
                      out StaticSystemType: IBoldSystemTypeInfo; out BoldSystem: IBoldSystem;
                      out StaticRootType: IBoldElementTypeInfo;
                      out CurrentBoldObject: IBoldObject;
                      out BoldList: IBoldList; out ListElementType: IBoldElementTypeInfo;
                      out NamedValues: OleVariant): WordBool; override;
    function  SetData(DataFlags: Integer; const Value: IBoldElement; NamedValues: OleVariant): WordBool; override;
  public
    constructor Create(AdaptableObject: TBoldAdaptableObject; Owner: Boolean;
      const TypeLib: ITypeLib; const DispIntf: TGUID); override;
    property AsListHandle: TBoldListHandle read GetAsListHandle;
  end;

  TBoldComReferenceHandleAdapter = class(TBoldComElementHandleAdapter)
  private
    function GetAsReferenceHandle: TBoldReferenceHandle;
  protected
    function  GetData(DataFlags: Integer; out Value: IBoldElement;
                      out DynamicBoldType: IBoldElementTypeInfo;
                      out StaticBoldType: IBoldElementTypeInfo;
                      out StaticSystemType: IBoldSystemTypeInfo; out BoldSystem: IBoldSystem;
                      out StaticRootType: IBoldElementTypeInfo;
                      out CurrentBoldObject: IBoldObject;
                      out BoldList: IBoldList; out ListElementType: IBoldElementTypeInfo;
                      out NamedValues: OleVariant): WordBool; override;
    function  SetData(DataFlags: Integer; const Value: IBoldElement; NamedValues: OleVariant): WordBool; override;
  public
    constructor Create(AdaptableObject: TBoldAdaptableObject; Owner: Boolean;
      const TypeLib: ITypeLib; const DispIntf: TGUID); override;
    property AsReferenceHandle: TBoldReferenceHandle read GetAsReferenceHandle;
  end;

  TBoldComSQLHandleAdapter = class(TBoldComElementHandleAdapter)
  private
    function GetAsSQLHandle: TBoldSQLHandle;
  protected
    function  GetData(DataFlags: Integer; out Value: IBoldElement;
                      out DynamicBoldType: IBoldElementTypeInfo;
                      out StaticBoldType: IBoldElementTypeInfo;
                      out StaticSystemType: IBoldSystemTypeInfo; out BoldSystem: IBoldSystem;
                      out StaticRootType: IBoldElementTypeInfo;
                      out CurrentBoldObject: IBoldObject;
                      out BoldList: IBoldList; out ListElementType: IBoldElementTypeInfo;
                      out NamedValues: OleVariant): WordBool; override;
    function  SetData(DataFlags: Integer; const Value: IBoldElement; NamedValues: OleVariant): WordBool; override;
  public
    constructor Create(AdaptableObject: TBoldAdaptableObject; Owner: Boolean;
      const TypeLib: ITypeLib; const DispIntf: TGUID); override;
    property AsSQLHandle: TBoldSQLHandle read GetAsSQLHandle;
  end;

  TBoldComVariableHandleAdapter = class(TBoldComElementHandleAdapter)
  private
    function GetAsVariableHandle: TBoldVariableHandle;
  protected
    function  GetData(DataFlags: Integer; out Value: IBoldElement;
                      out DynamicBoldType: IBoldElementTypeInfo;
                      out StaticBoldType: IBoldElementTypeInfo;
                      out StaticSystemType: IBoldSystemTypeInfo; out BoldSystem: IBoldSystem;
                      out StaticRootType: IBoldElementTypeInfo;
                      out CurrentBoldObject: IBoldObject;
                      out BoldList: IBoldList; out ListElementType: IBoldElementTypeInfo;
                      out NamedValues: OleVariant): WordBool; override;
    function  SetData(DataFlags: Integer; const Value: IBoldElement; NamedValues: OleVariant): WordBool; override;
  public
    constructor Create(AdaptableObject: TBoldAdaptableObject; Owner: Boolean;
      const TypeLib: ITypeLib; const DispIntf: TGUID); override;
    property AsVariableHandle: TBoldVariableHandle read GetAsVariableHandle;
  end;

implementation

uses
  SysUtils,
  Classes,
  Variants,
  BoldUtils,
  BoldDefs,
  BoldComUtils,
  BoldComObj,
  BoldComServer,
  BoldComObjectSpace;

procedure CreateIndirectAdapter(IndirectElement: TBoldIndirectElement; const IID: TGUID; out Obj);
var
  Adapter: TBoldComAdapter;
  UnknownAdapter: IUnknown;
  Adaptee: TBoldElement;
  Owner: Boolean;
begin
  Pointer(Obj) := nil;
  if IndirectElement.OwnsValue then
  begin
    Adaptee := IndirectElement.RelinquishValue;
    Owner := True;
  end
  else
  begin
    Adaptee := IndirectElement.Value;
    Owner := False;
  end;
  if Assigned(Adaptee) then
  begin
    Adapter := TBoldComAdapterFactory.Instance.CreateAdapterForObject(Adaptee, Owner);
    if not Assigned(Adapter) then
    begin
      if Owner then Adaptee.Free;
      raise EBoldCom.CreateFmt('No adapter registered for %s', [Adaptee.ClassName]);
    end;
    UnknownAdapter := Adapter;
    if UnknownAdapter.QueryInterface(IID,Obj) <> 0 then
    begin
      UnknownAdapter := nil;
      raise EBoldCom.CreateFmt('%s: Unsupported interface', [Adapter.ClassName]);
    end;
  end;
end;

function InterfaceToElement(const Element: IBoldElement): TBoldElement;
begin
  Result := BoldComInterfaceToObject(Element) as TBoldElement;
end;

{-- TBoldComElementAdapter ----------------------------------------------------}

constructor TBoldComElementAdapter.Create(AdaptableObject: TBoldAdaptableObject;
  Owner: Boolean; const TypeLib: ITypeLib; const DispIntf: TGUID);
begin
  if Assigned(TypeLib) then
    inherited Create(AdaptableObject, Owner, TypeLib, DispIntf)
  else
    inherited Create(AdaptableObject, Owner, BoldComObjectSpaceTypeLibrary, IBoldElement);
end;

function TBoldComElementAdapter.GetAsElement: TBoldElement;
begin
  Result := EnsuredAdaptee as TBoldElement;
end;

{ IBoldElement }

procedure TBoldComElementAdapter.AddSmallSubscription(const ClientId: WideString;
  SubscriberId: Integer; SmallEvents: Integer; RequestedEvent: Integer;
  CancelOld: WordBool); safecall;
var
  Sub: TBoldComServerSubscriber;
begin
  Sub := TBoldComServerSubscriber.GetSubscriber(ClientId, SubscriberId);
  if assigned(Sub) and CancelOld then
    Sub.CancelAllSubscriptions;
  AsElement.AddSmallSubscription(Sub, TBoldSmallEventSet(SmallEvents), RequestedEvent);
end;

procedure TBoldComElementAdapter.AddSubscription(const ClientId: WideString;
  SubscriberId: Integer; Event: Integer; RequestedEvent: Integer; CancelOld: WordBool);
var
  Sub: TBoldComServerSubscriber;
begin
  Sub := TBoldComServerSubscriber.GetSubscriber(ClientId, SubscriberId);
  if assigned(Sub) and CancelOld then
    Sub.CancelAllSubscriptions;
  AsElement.AddSubscription(Sub, Event, RequestedEvent);
end;

procedure TBoldComElementAdapter.AssignElement(const Element: IBoldElement);
begin
  AsElement.Assign(InterfaceToElement(Element));
end;

function TBoldComElementAdapter.CompareTo(const Element: IBoldElement): Integer;
begin
  Result := AsElement.CompareTo(InterfaceToElement(Element));
end;

function TBoldComElementAdapter.CompareToAs(CompareType: Integer;
  const Element: IBoldElement): Integer;
begin
  Result := AsElement.CompareToAs(TBoldCompareType(CompareType), InterfaceToElement(Element));
end;

procedure TBoldComElementAdapter.DefaultSubscribe(const ClientId: WideString;
  SubscriberId: Integer; RequestedEvent: Integer; CancelOld: WordBool);
var
  Sub: TBoldComServerSubscriber;
begin
  Sub := TBoldComServerSubscriber.GetSubscriber(ClientId, SubscriberId);
  if assigned(Sub) and CancelOld then
    Sub.CancelAllSubscriptions;
  AsElement.DefaultSubscribe(Sub, RequestedEvent);
end;

function TBoldComElementAdapter.EvaluateAndSubscribeToExpression(
  const Expression: WideString; const ClientId: WideString; SubscriberId: Integer;
  Resubscribe: WordBool; CancelOld: WordBool): IBoldElement;
var
  Sub: TBoldComServerSubscriber;
  IndirectElement: TBoldIndirectElement;
begin
  Result := nil;
  Sub := TBoldComServerSubscriber.GetSubscriber(ClientId, SubscriberId);
  if assigned(Sub) and CancelOld then
    Sub.CancelAllSubscriptions;
  IndirectElement := TBoldIndirectElement.Create;
  try
    AsElement.EvaluateAndSubscribeToExpression(Expression, Sub, IndirectElement, Resubscribe, false, nil);
    CreateIndirectAdapter(IndirectElement,IBoldElement,Result);
  finally
    IndirectElement.Free;
  end;
end;

function TBoldComElementAdapter.EvaluateExpression(const Expression: WideString): IBoldElement;
var
  IndirectElement: TBoldIndirectElement;
begin
  Result := nil;
  IndirectElement := TBoldIndirectElement.Create;
  try
    AsElement.EvaluateExpression(Expression, IndirectElement);
    CreateIndirectAdapter(IndirectElement, IBoldElement, Result);
  finally
    IndirectElement.Free;
  end;
end;

function TBoldComElementAdapter.EvaluateExpressionAsSessionIdList(
  const Expression: WideString): OleVariant;
var
  ResultElement: TBoldIndirectElement;
  DirectElement: TBoldDirectElement;
  SessionIdList: TStringList;
  ListIndex: Integer;
begin
  Result := Null;
  ResultElement := TBoldIndirectElement.Create;
  SessionIdList := TStringList.Create;
  try
    AsElement.EvaluateExpression(Expression,ResultElement);
    DirectElement := ResultElement.Value;
    if Assigned(DirectElement) then
    begin
      if DirectElement is TBoldObjectList then
      begin
        with DirectElement as TBoldObjectList do
        begin
          for ListIndex := 0 to Count - 1 do
            SessionIdList.Add(IntToStr(Integer(BoldObjects[ListIndex])));
        end;
      end
      else if (DirectElement is TBoldObject)then
        SessionIdList.Add(IntToStr(Integer(DirectElement)))
      else if (DirectElement is TBoldObjectReference) then
      begin
        with DirectElement as TBoldObjectReference do
          if Assigned(BoldObject) then
            SessionIdList.Add(IntToStr(Integer(BoldObject)))
      end;
      Result := BoldStringsToVariant(SessionIdList);
    end;
  finally
    SessionIdList.Free;
    ResultElement.Free;
  end;
end;

function TBoldComElementAdapter.EvaluateExpressionAsSessionIdListAndStringList(
  const ListExpression: WideString; const ItemExpression: WideString;
  Representation: Integer): OleVariant;
var
  ResultElement,ListResultElement: TBoldIndirectElement;
  DirectElement,ListDirectElement: TBoldDirectElement;
  SessionIdList: TStringList;
  ResultStrings: TStringList;
  StringDataList: OleVariant;
  ListItem: TBoldObject;
  I: Integer;
begin
  Result := Null;
  ListResultElement := TBoldIndirectElement.Create;
  SessionIdList := TStringList.Create;
  try
    AsElement.EvaluateExpression(ListExpression,ListResultElement);
    ListDirectElement := ListResultElement.Value;
    if Assigned(ListDirectElement) then
    begin
      if (ListDirectElement is TBoldObjectList) then
      begin
        with ListDirectElement as TBoldObjectList do
        begin
          StringDataList := VarArrayCreate([0, Count-1], varVariant);
          for I := 0 to Count - 1 do
          begin
            ListItem := BoldObjects[I];
            SessionIdList.Add(IntToStr(Integer(ListItem)));
            ResultElement := TBoldIndirectElement.Create;
            try
              ListItem.EvaluateExpression(ItemExpression, ResultElement);
              DirectElement := ResultElement.Value;
              if Assigned(DirectElement) then
              begin
                if DirectElement is TBoldList then
                begin
                  ResultStrings := TStringList.Create;
                  try
                    TBoldList(DirectElement).ToStrings(Representation, ResultStrings);
                    StringDataList[I] := BoldStringsToVariant(ResultStrings);
                  finally
                    ResultStrings.Free;
                  end;
                end
                else
                  StringDataList[I] := WideString(DirectElement.StringRepresentation[Representation]);
              end
              else
                StringDataList[I] := Null;
            finally
              ResultElement.Free;
            end;
          end;
        end;
      end
      else if (ListDirectElement is TBoldObject) or (ListDirectElement is TBoldObjectReference) then
      begin
        if (ListDirectElement is TBoldObjectReference) then
          ListDirectElement := (ListDirectElement as TBoldObjectReference).BoldObject;
        if Assigned(ListDirectELement) then
        begin
          SessionIdList.Add(IntToStr(Integer(ListDirectElement)));
          StringDataList := VarArrayCreate([0, 0], varVariant);
          ResultElement := TBoldIndirectElement.Create;
          try
            ListDirectElement.EvaluateExpression(ItemExpression,ResultElement);
            DirectElement := ResultElement.Value;
            if Assigned(DirectElement) then
            begin
              if DirectElement is TBoldList then
              begin
                ResultStrings := TStringList.Create;
                try
                  TBoldList(DirectElement).ToStrings(Representation, ResultStrings);
                  StringDataList[0] := BoldStringsToVariant(ResultStrings);
                finally
                  ResultStrings.Free;
                end;
              end
              else
                StringDataList[0] := WideString(DirectElement.StringRepresentation[Representation]);
            end
            else
              StringDataList[0] := Unassigned;
          finally
            ResultElement.Free;
          end;
        end;
      end;
      if SessionIdList.Count > 0 then
      begin
        Result := VarArrayCreate([0, 1], varVariant);
        Result[0] := BoldStringsToVariant(SessionIdList);
        Result[1] := StringDataList;
      end;
    end;
  finally
    ListResultElement.Free;
    SessionIdList.Free;
  end;
end;

function TBoldComElementAdapter.EvaluateExpressionAsString(
  const Expression: WideString; Representation: Integer): WideString;
begin
  Result := AsElement.EvaluateExpressionAsString(Expression, Representation);
end;

function TBoldComElementAdapter.EvaluateExpressionAsStringList(
  const Expression: WideString; Representation: Integer): OleVariant;
var
  IndirectElement: TBoldIndirectElement;
  DirectElement: TBoldDirectElement;
  Strings: TStringList;
begin
  Result := Null;
  IndirectElement := TBoldIndirectElement.Create;
  try
    AsElement.EvaluateExpression(Expression,IndirectElement);
    DirectElement := IndirectElement.Value;
    if Assigned(DirectElement) then
    begin
      Strings := TStringList.Create;
      try
        if DirectElement is TBoldList then
          TBoldList(DirectElement).ToStrings(Representation, Strings)
        else
          Strings.Add(DirectElement.StringRepresentation[Representation]);
        Result := BoldStringsToVariant(Strings);
      finally
        Strings.Free;
      end;
    end;
  finally
    IndirectElement.Free;
  end;
end;

function TBoldComElementAdapter.EvaluateExpressionsAsStringLists(
  Expressions: OleVariant; Representation: Integer): OleVariant;
var
  ThisElement: TBoldElement;
  ResultElement: TBoldIndirectElement;
  DirectElement: TBoldDirectElement;
  ExpressionList: TStringList;
  Strings: TStringList;
  ExpressionIndex: Integer;
begin
  Result := Null;
  ExpressionList := TStringList.Create;
  try
    BoldVariantToStrings(Expressions, ExpressionList);
    if ExpressionList.Count = 0 then Exit;
    ThisElement := AsElement;
    Result := VarArrayCreate([0, ExpressionList.Count - 1], varVariant);
    for ExpressionIndex := 0 to ExpressionList.Count - 1 do
    begin
      ResultElement := TBoldIndirectElement.Create;
      try
        ThisElement.EvaluateExpression(ExpressionList[ExpressionIndex], ResultElement);
        DirectElement := ResultElement.Value;
        if Assigned(DirectElement) then
        begin
          Strings := TStringList.Create;
          try
            if DirectElement is TBoldList then
              TBoldList(DirectElement).ToStrings(Representation, Strings)
            else
              Strings.Add(DirectElement.StringRepresentation[Representation]);
            Result[ExpressionIndex] := BoldStringsToVariant(Strings)
          finally
            Strings.Free;
          end;
        end
        else
          Result[ExpressionIndex] := Null;
      finally
        ResultElement.Free;
      end;
    end;
  finally
    ExpressionList.Free;
  end;
end;

function TBoldComElementAdapter.GetAsList: IBoldList;
var
  IndirectElement: TBoldIndirectElement;
begin
  Result := nil;
  IndirectElement := TBoldIndirectElement.Create;
  try
    AsElement.GetAsList(IndirectElement);
    CreateIndirectAdapter(IndirectElement, IBoldList, Result);
  finally
    IndirectElement.Free;
  end;
end;

function TBoldComElementAdapter.IsEqual(const Element: IBoldElement): WordBool;
begin
  Result := AsElement.IsEqual(InterfaceToElement(Element));
end;

function TBoldComElementAdapter.IsEqualAs(CompareType: Integer;
  const Element: IBoldElement): WordBool;
begin
  Result := AsElement.IsEqualAs(TBoldCompareType(CompareType), InterfaceToElement(Element));
end;

procedure TBoldComElementAdapter.MakeImmutable;
begin
  AsElement.MakeImmutable;
end;

procedure TBoldComElementAdapter.SubscribeToExpression(const Expression: WideString;
  const ClientId: WideString; SubscriberId: Integer; Resubscribe: WordBool; CancelOld: WordBool);
var
  Subscriber: TBoldComServerSubscriber;
begin
  Subscriber := TBoldComServerSubscriber.GetSubscriber(ClientId, SubscriberId);
  if CancelOld then
    Subscriber.CancelAllSubscriptions;
  AsElement.SubscribeToExpression(Expression, Subscriber, Resubscribe);
end;

procedure TBoldComElementAdapter.SubscribeToStringRepresentation(
  Representation: Integer; const ClientId: WideString; SubscriberId: Integer;
  RequestedEvent: Integer; CancelOld: WordBool);
var
  Subscriber: TBoldComServerSubscriber;
begin
  Subscriber := TBoldComServerSubscriber.GetSubscriber(ClientId, SubscriberId);
  if assigned(Subscriber) and CancelOld then
    Subscriber.CancelAllSubscriptions;
  AsElement.SubscribeToStringRepresentation(Representation, Subscriber, RequestedEvent);
end;

function TBoldComElementAdapter.ValidateCharacter(const Value: WideString;
  Representation: Integer): WordBool;
begin
  Result := AsElement.ValidateCharacter(String(Value)[1], Representation);
end;

function TBoldComElementAdapter.ValidateString(const Value: WideString;
  Representation: Integer): WordBool;
begin
  Result := AsElement.ValidateString(Value, Representation);
end;

function TBoldComElementAdapter.Get_AsString: WideString;
begin
  Result := AsElement.AsString;
end;

procedure TBoldComElementAdapter.Set_AsString(const Value: WideString);
begin
  AsElement.AsString := Value;
end;

function  TBoldComElementAdapter.Get_AsVariant: OleVariant;
begin
  Result := AsElement.AsVariant;
end;

procedure TBoldComElementAdapter.Set_AsVariant(Value: OleVariant);
begin
  AsElement.AsVariant := Value;
end;

function TBoldComElementAdapter.Get_BoldType: IBoldElementTypeInfo;
begin
  BoldComCreateAdapter(AsElement.BoldType, False, IBoldElementTypeInfo, Result);
end;

function TBoldComElementAdapter.Get_BoldTypeName: WideString;
begin
  Result := AsElement.BoldType.ExpressionName;
end;

function TBoldComElementAdapter.Get_Mutable: WordBool;
begin
  Result := AsElement.Mutable;
end;

function TBoldComElementAdapter.Get_StringRepresentation(Representation: Integer): WideString;
begin
  Result := AsElement.StringRepresentation[Representation];
end;

procedure TBoldComElementAdapter.Set_StringRepresentation(Representation: Integer;
  const Value: WideString);
begin
  AsElement.StringRepresentation[Representation] := Value;
end;

function TBoldComElementAdapter.Get__NewEnum: IUnknown;
begin
  Result := TBoldVariantEnumerator.Create(Self);
end;

function TBoldComElementAdapter.Get_HasAdaptee: WordBool;
begin
  result := assigned(Adaptee);
end;


{-- TBoldComMetaElementAdapter ------------------------------------------------}

constructor TBoldComMetaElementAdapter.Create(AdaptableObject: TBoldAdaptableObject;
  Owner: Boolean; const TypeLib: ITypeLib; const DispIntf: TGUID);
begin
  if Assigned(TypeLib) then
    inherited Create(AdaptableObject, Owner, TypeLib, DispIntf)
  else
    inherited Create(AdaptableObject, Owner, BoldComObjectSpaceTypeLibrary, IBoldMetaElement);
end;

function TBoldComMetaElementAdapter.GetAsMetaElement: TBoldMetaElement;
begin
  Result := EnsuredAdaptee as TBoldMetaElement;
end;

{ IBoldMetaElement }

function TBoldComMetaElementAdapter.Get_DelphiName: WideString;
begin
  Result := AsMetaElement.DelphiName;
end;

function TBoldComMetaElementAdapter.Get_ExpressionName: WideString;
begin
  Result := AsMetaElement.ExpressionName;
end;

function TBoldComMetaElementAdapter.Get_ModelName: WideString;
begin
  Result := AsMetaElement.ModelName;
end;

{-- TBoldComElementTypeInfoAdapter --------------------------------------------}

constructor TBoldComElementTypeInfoAdapter.Create(AdaptableObject: TBoldAdaptableObject;
  Owner: Boolean; const TypeLib: ITypeLib; const DispIntf: TGUID);
begin
  if Assigned(TypeLib) then
    inherited Create(AdaptableObject, Owner, TypeLib, DispIntf)
  else
    inherited Create(AdaptableObject, Owner, BoldComObjectSpaceTypeLibrary, IBoldElementTypeInfo);
end;

function TBoldComElementTypeInfoAdapter.GetAsElementTypeInfo: TBoldElementTypeInfo;
begin
  Result := EnsuredAdaptee as TBoldElementTypeInfo;
end;

{ IBoldElementTypeInfo }

function TBoldComElementTypeInfoAdapter.ConformsTo(
  const ElementTypeInfo: IBoldElementTypeInfo): WordBool;
begin
  Result := AsElementTypeInfo.ConformsTo(
    InterfaceToElement(ElementTypeInfo) as TBoldElementTypeInfo);
end;

function TBoldComElementTypeInfoAdapter.Get_BoldValueType: Integer;
begin
  Result := Integer(AsElementTypeInfo.BoldValueType);
end;

function TBoldComElementTypeInfoAdapter.Get_SystemTypeInfo: IBoldSystemTypeInfo;
begin
  BoldComCreateAdapter(AsElementTypeInfo.SystemTypeInfo, False, IBoldSystemTypeInfo, Result);
end;

{-- TBoldComTypeTypeInfoAdapter -----------------------------------------------}

constructor TBoldComTypeTypeInfoAdapter.Create(AdaptableObject: TBoldAdaptableObject;
  Owner: Boolean; const TypeLib: ITypeLib; const DispIntf: TGUID);
begin
  if Assigned(TypeLib) then
    inherited Create(AdaptableObject, Owner, TypeLib, DispIntf)
  else
    inherited Create(AdaptableObject, Owner, BoldComObjectSpaceTypeLibrary, IBoldTypeTypeInfo);
end;

function TBoldComTypeTypeInfoAdapter.GetAsTypeTypeInfo: TBoldTypeTypeInfo;
begin
  Result := EnsuredAdaptee as TBoldTypeTypeInfo;
end;

{ IBoldTypeTypeInfo }

{-- TBoldComClassTypeInfoAdapter ----------------------------------------------}

constructor TBoldComClassTypeInfoAdapter.Create(AdaptableObject: TBoldAdaptableObject;
  Owner: Boolean; const TypeLib: ITypeLib; const DispIntf: TGUID);
begin
  if Assigned(TypeLib) then
    inherited Create(AdaptableObject, Owner, TypeLib, DispIntf)
  else
    inherited Create(AdaptableObject, Owner, BoldComObjectSpaceTypeLibrary, IBoldClassTypeInfo);
end;

function TBoldComClassTypeInfoAdapter.GetAsClassTypeInfo: TBoldClassTypeInfo;
begin
  Result := EnsuredAdaptee as TBoldClassTypeInfo;
end;

{ IBoldClassTypeInfo }

function TBoldComClassTypeInfoAdapter.LeastCommonSuperClass(
  const ClassTypeInfo: IBoldClassTypeInfo): IBoldClassTypeInfo;
begin
  BoldComCreateAdapter(AsClassTypeInfo.LeastCommonSuperClass(
    InterfaceToElement(ClassTypeInfo) as TBoldClassTypeInfo),
    False, IBoldClassTypeInfo, Result);
end;

function TBoldComClassTypeInfoAdapter.Get_Constraints: OleVariant;
begin
  Result := Unassigned;
end;

function TBoldComClassTypeInfoAdapter.Get_FirstOwnMemberIndex: Integer;
begin
  Result := AsClassTypeInfo.FirstOwnMemberIndex;
end;

function TBoldComClassTypeInfoAdapter.Get_HasSubClasses: WordBool;
begin
  Result := AsClassTypeInfo.HasSubclasses;
end;

function TBoldComClassTypeInfoAdapter.Get_IsAbstract: WordBool;
begin
  Result := AsClassTypeInfo.IsAbstract;
end;

function TBoldComClassTypeInfoAdapter.Get_IsImported: WordBool;
begin
  Result := AsClassTypeInfo.IsImported;
end;

function TBoldComClassTypeInfoAdapter.Get_IsLinkClass: WordBool;
begin
  Result := AsClassTypeInfo.IsLinkClass;
end;

function TBoldComClassTypeInfoAdapter.Get_IsPersistent: WordBool;
begin
  Result := AsClassTypeInfo.Persistent;
end;

function TBoldComClassTypeInfoAdapter.Get_ListTypeInfo: IBoldListTypeInfo;
begin
  BoldComCreateAdapter(AsClassTypeInfo.ListTypeInfo, False, IBoldListTypeInfo, Result);
end;

function TBoldComClassTypeInfoAdapter.Get_Stereotype: WideString;
begin
  Result := AsClassTypeInfo.Stereotype;
end;

function TBoldComClassTypeInfoAdapter.Get_SuperClassTypeInfo: IBoldClassTypeInfo;
begin
  BoldComCreateAdapter(AsClassTypeInfo.SuperClassTypeInfo, False, IBoldClassTypeInfo, Result);
end;

function TBoldComClassTypeInfoAdapter.Get_TaggedValue(const Tag: WideString): WideString;
begin
  Result := AsClassTypeInfo.TaggedValues[Tag];
end;

function TBoldComClassTypeInfoAdapter.Get_TopSortedIndex: Integer;
begin
  Result := AsClassTypeInfo.TopSortedIndex;
end;

{-- TBoldComNilTypeInfoAdapter ------------------------------------------------}

constructor TBoldComNilTypeInfoAdapter.Create(AdaptableObject: TBoldAdaptableObject;
  Owner: Boolean; const TypeLib: ITypeLib; const DispIntf: TGUID);
begin
  if Assigned(TypeLib) then
    inherited Create(AdaptableObject, Owner, TypeLib, DispIntf)
  else
    inherited Create(AdaptableObject, Owner, BoldComObjectSpaceTypeLibrary, IBoldNilTypeInfo);
end;

function TBoldComNilTypeInfoAdapter.GetAsNilTypeInfo: TBoldNilTypeInfo;
begin
  Result := EnsuredAdaptee as TBoldNilTypeInfo;
end;

{ IBoldNilTypeInfo }

{-- TBoldComListTypeInfoAdapter -----------------------------------------------}

constructor TBoldComListTypeInfoAdapter.Create(AdaptableObject: TBoldAdaptableObject;
  Owner: Boolean; const TypeLib: ITypeLib; const DispIntf: TGUID);
begin
  if Assigned(TypeLib) then
    inherited Create(AdaptableObject, Owner, TypeLib, DispIntf)
  else
    inherited Create(AdaptableObject, Owner, BoldComObjectSpaceTypeLibrary, IBoldListTypeInfo);
end;

function TBoldComListTypeInfoAdapter.GetAsListTypeInfo: TBoldListTypeInfo;
begin
  Result := EnsuredAdaptee as TBoldListTypeInfo;
end;

{ IBoldListTypeInfo }

function TBoldComListTypeInfoAdapter.Get_ListElementTypeInfo: IBoldElementTypeInfo;
begin
  BoldComCreateAdapter(AsListTypeInfo.ListElementTypeInfo,
                       False,
                       IBoldElementTypeInfo,
                       Result);
end;

{-- TBoldComAttributeTypeInfoAdapter ------------------------------------------}

constructor TBoldComAttributeTypeInfoAdapter.Create(AdaptableObject: TBoldAdaptableObject;
  Owner: Boolean; const TypeLib: ITypeLib; const DispIntf: TGUID);
begin
  if Assigned(TypeLib) then
    inherited Create(AdaptableObject, Owner, TypeLib, DispIntf)
  else
    inherited Create(AdaptableObject, Owner, BoldComObjectSpaceTypeLibrary, IBoldAttributeTypeInfo);
end;

function TBoldComAttributeTypeInfoAdapter.GetAsAttributeTypeInfo: TBoldAttributeTypeInfo;
begin
  Result := EnsuredAdaptee as TBoldAttributeTypeInfo;
end;

{ IBoldAttributeTypeInfo }

function TBoldComAttributeTypeInfoAdapter.Get_SuperAttributeTypeInfo: IBoldAttributeTypeInfo;
begin
  BoldComCreateAdapter(AsAttributeTypeInfo.SuperAttributeTypeInfo,
                       False,
                       IBoldAttributeTypeInfo,
                       Result);
end;

{-- TBoldComSystemTypeInfoAdapter ---------------------------------------------}

constructor TBoldComSystemTypeInfoAdapter.Create(AdaptableObject: TBoldAdaptableObject;
  Owner: Boolean; const TypeLib: ITypeLib; const DispIntf: TGUID);
begin
  if Assigned(TypeLib) then
    inherited Create(AdaptableObject, Owner, TypeLib, DispIntf)
  else
    inherited Create(AdaptableObject, Owner, BoldComObjectSpaceTypeLibrary, IBoldSystemTypeInfo);
end;

function TBoldComSystemTypeInfoAdapter.GetAsSystemTypeInfo: TBoldSystemTypeInfo;
begin
  Result := EnsuredAdaptee as TBoldSystemTypeInfo;
end;

{ IBoldSystemTypeInfo }

function TBoldComSystemTypeInfoAdapter.Get_AttributeTypeInfoByExpressionName(
  const Name: WideString): IBoldAttributeTypeInfo;
begin
  BoldComCreateAdapter(AsSystemTypeInfo.AttributeTypeInfoByExpressionName[Name],
                       False,
                       IBoldAttributeTypeInfo,
                       Result);
end;

function TBoldComSystemTypeInfoAdapter.Get_AttributeTypes: IUnknown;
begin
  Result := nil;
end;

function TBoldComSystemTypeInfoAdapter.Get_ClassTypeInfoByExpressionName(
  const Name: WideString): IBoldClassTypeInfo;
begin
  BoldComCreateAdapter(AsSystemTypeInfo.ClassTypeInfoByExpressionName[Name],
                       False,
                       IBoldClassTypeInfo,
                       Result);
end;

function TBoldComSystemTypeInfoAdapter.Get_ClassTypes: IUnknown;
begin
  Result := nil;
end;

function TBoldComSystemTypeInfoAdapter.Get_Constraints: OleVariant;
begin
  Result := Unassigned;
end;

function TBoldComSystemTypeInfoAdapter.Get_ElementTypeInfoByExpressionName(
  const Name: WideString): IBoldElementTypeInfo;
begin
  BoldComCreateAdapter(AsSystemTypeInfo.ElementTypeInfoByExpressionName[Name],
                       False,
                       IBoldElementTypeInfo,
                       Result);
end;

function TBoldComSystemTypeInfoAdapter.Get_IsPersistent: WordBool;
begin
  Result := AsSystemTypeInfo.Persistent;
end;

function TBoldComSystemTypeInfoAdapter.Get_IsRunnable: WordBool;
begin
  Result := AsSystemTypeInfo.SystemIsRunnable;
end;

function TBoldComSystemTypeInfoAdapter.Get_ListTypeInfoByElement(
  const ElementTypeInfo: IBoldElementTypeInfo): IBoldListTypeInfo;
begin
  BoldComCreateAdapter(AsSystemTypeInfo.ListTypeInfoByElement[
    InterfaceToElement(ElementTypeInfo) as TBoldElementTypeInfo],
    False, IBoldListTypeInfo, Result);
end;

function TBoldComSystemTypeInfoAdapter.Get_ListTypes: IUnknown;
begin
  Result := nil;
end;

function TBoldComSystemTypeInfoAdapter.Get_MethodsInstalled: WordBool;
begin
  Result := AsSystemTypeInfo.MethodsInstalled;
end;

function TBoldComSystemTypeInfoAdapter.Get_NilTypeInfo: IBoldNilTypeInfo;
begin
  BoldComCreateAdapter(AsSystemTypeInfo.NilTypeInfo,
                       False,
                       IBoldNilTypeInfo,
                       Result);
end;

function TBoldComSystemTypeInfoAdapter.Get_RootClassTypeInfo: IBoldClassTypeInfo;
begin
  BoldComCreateAdapter(AsSystemTypeInfo.RootClassTypeInfo,
    False,IBoldClassTypeInfo, Result);
end;

function TBoldComSystemTypeInfoAdapter.Get_Stereotype: WideString;
begin
  Result := AsSystemTypeInfo.Stereotype;
end;

function TBoldComSystemTypeInfoAdapter.Get_TaggedValue(const Tag: WideString): WideString;
begin
  Result := AsSystemTypeInfo.TaggedValues[Tag];
end;

function TBoldComSystemTypeInfoAdapter.Get_TopSortedClasses: IUnknown;
begin
  Result := nil;
end;

function TBoldComSystemTypeInfoAdapter.Get_TypeTypeInfo: IBoldTypeTypeInfo;
begin
  BoldComCreateAdapter(AsSystemTypeInfo.TypeTypeInfo,
                       False,
                       IBoldTypeTypeInfo,
                       Result);
end;

function TBoldComSystemTypeInfoAdapter.Get_UseGeneratedCode: WordBool;
begin
  Result := AsSystemTypeInfo.UseGeneratedCode;
end;

function TBoldComSystemTypeInfoAdapter.Get_ValueTypeNameList: IUnknown;
begin
  Result := nil;
end;

{-- TBoldComDomainElementAdapter ----------------------------------------------}

constructor TBoldComDomainElementAdapter.Create(AdaptableObject: TBoldAdaptableObject;
  Owner: Boolean; const TypeLib: ITypeLib; const DispIntf: TGUID);
begin
  if Assigned(TypeLib) then
    inherited Create(AdaptableObject, Owner, TypeLib, DispIntf)
  else
    inherited Create(AdaptableObject, Owner, BoldComObjectSpaceTypeLibrary, IBoldDomainElement);
end;

function TBoldComDomainElementAdapter.GetAsDomainElement: TBoldDomainElement;
begin
  Result := EnsuredAdaptee as TBoldDomainElement;
end;

{ IBoldDomainElement }

function TBoldComDomainElementAdapter.Get_BoldDirty: WordBool;
begin
  Result := AsDomainElement.BoldDirty;
end;

function TBoldComDomainElementAdapter.Get_BoldPersistent: WordBool;
begin
  Result := AsDomainElement.BoldPersistent;
end;

function TBoldComDomainElementAdapter.Get_DisplayName: WideString;
begin
  Result := AsDomainElement.DisplayName;
end;

function TBoldComDomainElementAdapter.Get_OwningElement: IBoldDomainElement;
begin
  BoldComCreateAdapter(AsDomainElement.OwningElement, False, IBoldDomainElement, Result);
end;

{-- TBoldComObjectAdapter -----------------------------------------------------}

constructor TBoldComObjectAdapter.Create(AdaptableObject: TBoldAdaptableObject;
  Owner: Boolean; const TypeLib: ITypeLib; const DispIntf: TGUID);
begin
  if Assigned(TypeLib) then
    inherited Create(AdaptableObject, Owner, TypeLib, DispIntf)
  else
    inherited Create(AdaptableObject, Owner, BoldComObjectSpaceTypeLibrary, IBoldObject);
end;

function TBoldComObjectAdapter.GetAsObject: TBoldObject;
begin
  Result := EnsuredAdaptee as TBoldObject;
end;

{ IBoldObject }

procedure TBoldComObjectAdapter.BoldMakePersistent;
begin
  AsObject.BoldMakePersistent;
end;

function TBoldComObjectAdapter.CheckLinks(Index: Integer): WordBool;
begin
  Result := AsObject.CheckLinks(Index);
end;

procedure TBoldComObjectAdapter.Delete;
begin
  AsObject.Delete;
end;

procedure TBoldComObjectAdapter.DiscardChanges;
begin
  AsObject.Discard;
end;

procedure TBoldComObjectAdapter.Invalidate;
begin
  AsObject.Invalidate;
end;

procedure TBoldComObjectAdapter.LinkObject(const RoleName: WideString;
  const BoldObject: IBoldObject);
var
  BoldObj: TBoldObject;
  LinkObj: TBoldMember;
begin
  if Assigned(BoldObject) then
  begin
    BoldObj := InterfaceToElement(BoldObject) as TBoldObject;
    LinkObj := AsObject.BoldMemberByExpressionName[RoleName];
    if LinkObj is TBoldObjectReference then
      TBoldObjectReference(LinkObj).BoldObject := BoldObj
    else if LinkObj is TBoldObjectList then
      TBoldObjectList(LinkObj).Add(BoldObj)
    else
      raise EBoldCom.CreateFmt('%.LinkObject: Role %s does not exist', [ClassName, RoleName]);
  end;
end;

procedure TBoldComObjectAdapter.UnlinkAll;
begin
  AsObject.UnlinkAll;
end;

procedure TBoldComObjectAdapter.UnlinkObject(const RoleName: WideString;
  const BoldObject: IBoldObject);
var
  BoldObj: TBoldObject;
  LinkObj: TBoldMember;
begin
  if Assigned(BoldObject) then
    BoldObj := InterfaceToElement(BoldObject) as TBoldObject
  else
    BoldObj := nil;
  LinkObj := AsObject.BoldMemberByExpressionName[RoleName];
  if LinkObj is TBoldObjectReference then
  begin
    with LinkObj as TBoldObjectReference do
    begin
      if Assigned(BoldObj) then
      begin
        if BoldObject = BoldObj then BoldObject := nil;
      end
      else
        BoldObject := nil;
    end;
  end
  else if LinkObj is TBoldObjectList then
  begin
    with LinkObj as TBoldObjectList do
    begin
      if Assigned(BoldObj) then
      begin
        if IndexOf(BoldObj) >= 0 then Remove(BoldObj);
      end
      else
        Clear;
    end;
  end
  else
    raise EBoldCom.CreateFmt('%.UnlinkObject: Role %s does not exist', [ClassName, RoleName]);
end;

function TBoldComObjectAdapter.Get_BoldClassTypeInfo: IBoldClassTypeInfo;
begin
  BoldComCreateAdapter(AsObject.BoldClassTypeInfo, False, IBoldClassTypeInfo, Result);
end;

function TBoldComObjectAdapter.Get_BoldExistenceState: Integer;
begin
  Result := Integer(AsObject.BoldExistenceState);
end;

function TBoldComObjectAdapter.Get_BoldMemberCount: Integer;
begin
  Result := AsObject.BoldMemberCount;
end;

function TBoldComObjectAdapter.Get_BoldMember(Index: OleVariant): IBoldMember;
begin
  if BoldVariantIsType(Index, varOleStr) then
    BoldComCreateAdapter(AsObject.BoldMemberByExpressionName[Index], False, IBoldMember, Result)
  else
    BoldComCreateAdapter(AsObject.BoldMembers[Index], False, IBoldMember, Result);
end;

function TBoldComObjectAdapter.Get_BoldMemberValue(Index: OleVariant): OleVariant;
begin
  if BoldVariantIsType(Index,varOleStr) then
    Result := AsObject.BoldMemberByExpressionName[Index].AsVariant
  else
    Result := AsObject.BoldMembers[Index].AsVariant;
end;

procedure TBoldComObjectAdapter.Set_BoldMemberValue(Index: OleVariant; Value: OleVariant);
begin
  if BoldVariantIsType(Index,varOleStr) then
    AsObject.BoldMemberByExpressionName[Index].AsVariant := Value
  else
    AsObject.BoldMembers[Index].AsVariant := Value;
end;

function TBoldComObjectAdapter.Get_BoldMemberValues: OleVariant;
var
  ThisObject: TBoldObject;
  Member: TBoldMember;
  MemberIndex: Integer;
  NameArray: OleVariant;
  DataArray: OleVariant;
begin
  Result := Null;
  ThisObject := AsObject;
  if ThisObject.BoldMemberCount > 0 then
  begin
    NameArray := VarArrayCreate([0, ThisObject.BoldMemberCount-1], varOleStr);
    DataArray := VarArrayCreate([0, ThisObject.BoldMemberCount-1], varVariant);
    for MemberIndex := 0 to ThisObject.BoldMemberCount-1 do
    begin
      Member := ThisObject.BoldMembers[MemberIndex];
      NameArray[MemberIndex] := Member.BoldMemberRtInfo.ExpressionName;
      DataArray[MemberIndex] := Member.AsVariant;
    end;
    Result := VarArrayCreate([0, 1], varVariant);
    Result[0] := NameArray;
    Result[1] := DataArray;
  end;
end;

procedure TBoldComObjectAdapter.Set_BoldMemberValues(Values: OleVariant);
var
  I: Integer;
  NameArray: OleVariant;
  DataArray: OleVariant;
begin
  if BoldVariantIsNamedValues(Values) then
  begin
    NameArray := Values[0];
    DataArray := Values[1];
    for I := VarArrayLowBound(NameArray, 1) to VarArrayHighBound(NameArray, 1) do
    begin
      Set_BoldMemberValue(NameArray[I], DataArray[I]);
    end;
  end
  else
    raise EBoldCom.CreateFmt('%s.BoldMemberValues: Unknown data format', [ClassName]);
end;

function TBoldComObjectAdapter.Get_BoldPersistenceState: Integer;
begin
  Result := Integer(AsObject.BoldPersistenceState);
end;

function TBoldComObjectAdapter.Get_BoldSystem: IBoldSystem;
begin
  BoldComCreateAdapter(AsObject.BoldSystem, False, IBoldSystem, Result);
end;

function TBoldComObjectAdapter.Get_CanDelete: WordBool;
begin
  AsObject.CanDelete;
end;

function TBoldComObjectAdapter.Get_CanUpdate: WordBool;
begin
  AsObject.CanUpdate;
end;

function TBoldComObjectAdapter.Get_SessionId: OleVariant;
begin
  Result := IntToStr(Integer(AsObject));
end;

{-- TBoldComMemberAdapter -----------------------------------------------------}

constructor TBoldComMemberAdapter.Create(AdaptableObject: TBoldAdaptableObject;
  Owner: Boolean; const TypeLib: ITypeLib; const DispIntf: TGUID);
begin
  if Assigned(TypeLib) then
    inherited Create(AdaptableObject, Owner, TypeLib, DispIntf)
  else
    inherited Create(AdaptableObject, Owner, BoldComObjectSpaceTypeLibrary, IBoldMember);
end;

function TBoldComMemberAdapter.GetAsMember: TBoldMember;
begin
  Result := EnsuredAdaptee as TBoldMember;
end;

{ IBoldMember }

function TBoldComMemberAdapter.Clone: IBoldMember;
begin
  BoldComCreateAdapter(AsMember.Clone, True, IBoldMember, Result);
end;

procedure TBoldComMemberAdapter.DiscardChanges;
begin
  AsMember.Discard;
end;

procedure TBoldComMemberAdapter.EnsureContentsCurrent;
begin
  AsMember.EnsureContentsCurrent;
end;

procedure TBoldComMemberAdapter.Invalidate;
begin
  AsMember.Invalidate;
end;

function TBoldComMemberAdapter.Get_BoldMemberRTInfo: IUnknown;
begin
  BoldComCreateAdapter(AsMember.BoldMemberRTInfo, False, IBoldElement, Result);
end;

function TBoldComMemberAdapter.Get_BoldSystem: IBoldSystem;
begin
  BoldComCreateAdapter(AsMember.BoldSystem, False, IBoldSystem, Result);
end;

function TBoldComMemberAdapter.Get_CanModify: WordBool;
begin
  Result := AsMember.CanModify;
end;

function TBoldComMemberAdapter.Get_CanRead: WordBool;
begin
  Result := AsMember.CanRead(nil);
end;

function TBoldComMemberAdapter.Get_Derived: WordBool;
begin
  Result := AsMember.Derived;
end;

function TBoldComMemberAdapter.Get_IsPartOfSystem: WordBool;
begin
  Result := AsMember.IsPartOfSystem;
end;

function TBoldComMemberAdapter.Get_OwningObject: IBoldObject;
begin
  BoldComCreateAdapter(AsMember.OwningObject, False, IBoldObject, Result);
end;

procedure TBoldComMemberAdapter.ReceiveEvent(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  if (Originator = asMember) and (RequestedEvent = beDestroying) then
     SetAdaptee(AsMember.Clone, true)
   else
    inherited;
end;


{-- TBoldComAttributeAdapter -----------------------------------------------------}

constructor TBoldComAttributeAdapter.Create(AdaptableObject: TBoldAdaptableObject;
  Owner: Boolean; const TypeLib: ITypeLib; const DispIntf: TGUID);
begin
  if Assigned(TypeLib) then
    inherited Create(AdaptableObject, Owner, TypeLib, DispIntf)
  else
    inherited Create(AdaptableObject, Owner, BoldComObjectSpaceTypeLibrary, IBoldAttribute);
end;

function TBoldComAttributeAdapter.GetAsAttribute: TBoldAttribute;
begin
  Result := EnsuredAdaptee as TBoldAttribute;
end;

{ IBoldAttribute }

procedure TBoldComAttributeAdapter.SetToNull;
begin
  AsAttribute.SetToNull;
end;

function  TBoldComAttributeAdapter.Get_BoldAttributeRTInfo: IUnknown;
begin
  Result := nil;
end;

function  TBoldComAttributeAdapter.Get_CanSetToNull: WordBool;
begin
  Result := AsAttribute.CanSetToNull(nil);
end;

function  TBoldComAttributeAdapter.Get_IsNull: WordBool;
begin
  Result := AsAttribute.IsNull;
end;

{-- TBoldComObjectReferenceAdapter -----------------------------------------------------}

constructor TBoldComObjectReferenceAdapter.Create(AdaptableObject: TBoldAdaptableObject;
  Owner: Boolean; const TypeLib: ITypeLib; const DispIntf: TGUID);
begin
  if Assigned(TypeLib) then
    inherited Create(AdaptableObject, Owner, TypeLib, DispIntf)
  else
    inherited Create(AdaptableObject, Owner, BoldComObjectSpaceTypeLibrary, IBoldObjectReference);
end;

function TBoldComObjectReferenceAdapter.GetAsObjectReference: TBoldObjectReference;
begin
  Result := EnsuredAdaptee as TBoldObjectReference;
end;

{ IBoldObjectReference }

function  TBoldComObjectReferenceAdapter.CanSet(const NewObject: IBoldObject): WordBool;
begin
  Result := AsObjectReference.CanSet(InterfaceToElement(NewObject) as TBoldObject, nil);
end;

function  TBoldComObjectReferenceAdapter.Get_BoldObject: IBoldObject;
begin
  BoldComCreateAdapter(AsObjectReference.BoldObject, False, IBoldObject, Result);
end;

procedure TBoldComObjectReferenceAdapter.Set_BoldObject(const Value: IBoldObject);
begin
  AsObjectReference.BoldObject := InterfaceToElement(Value) as TBoldObject;
end;

function  TBoldComObjectReferenceAdapter.Get_BoldRoleRTInfo: IUnknown;
begin
  Result := nil;
end;

{-- TBoldComListAdapter -------------------------------------------------------}

constructor TBoldComListAdapter.Create(AdaptableObject: TBoldAdaptableObject;
  Owner: Boolean; const TypeLib: ITypeLib; const DispIntf: TGUID);
begin
  if Assigned(TypeLib) then
    inherited Create(AdaptableObject, Owner, TypeLib, DispIntf)
  else
    inherited Create(AdaptableObject, Owner, BoldComObjectSpaceTypeLibrary, IBoldList);
end;

function TBoldComListAdapter.GetAsBoldList: TBoldList;
begin
  Result := EnsuredAdaptee as TBoldList;
end;

function TBoldComListAdapter.GetVariantCount: Cardinal;
begin
  Result := AsBoldList.Count;
end;

function TBoldComListAdapter.GetVariantItems(Index: Cardinal): OleVariant;
var
  Intf: IBoldElement;
begin
  BoldComCreateAdapter(AsBoldList.Elements[Index], False, IBoldElement, Intf);
  Result := Intf;
end;

{ IBoldList }

procedure TBoldComListAdapter.Add(const Element: IBoldElement);
begin
  AsBoldList.Add(InterfaceToElement(Element));
end;

procedure TBoldComListAdapter.AddList(const List: IBoldListCore);
begin
  AsBoldList.AddList(InterfaceToElement(List) as TBoldList);
end;

function  TBoldComListAdapter.AddNew: IBoldElement;
begin
  BoldComCreateAdapter(AsBoldList.AddNew,False,IBoldElement,Result);
end;

function  TBoldComListAdapter.CanClear: WordBool;
begin
  Result := AsBoldList.CanClear(nil);
end;

function  TBoldComListAdapter.CanInsert(Index: Integer; const Element: IBoldElement): WordBool;
begin
  Result := AsBoldList.CanInsert(Index,InterfaceToElement(Element), nil);
end;

function  TBoldComListAdapter.CanMove(CurrentIndex: Integer; NewIndex: Integer): WordBool;
begin
  Result := AsBoldList.CanMove(CurrentIndex, NewIndex, nil);
end;

function  TBoldComListAdapter.CanRemove(Index: Integer): WordBool;
begin
  Result := AsBoldList.CanRemove(Index, nil);
end;

function  TBoldComListAdapter.CanSet(Index: Integer; const Element: IBoldElement): WordBool;
begin
  Result := AsBoldList.CanSet(Index,InterfaceToElement(Element), nil);
end;

procedure TBoldComListAdapter.Clear;
begin
  AsBoldList.Clear;
end;

procedure TBoldComListAdapter.EnsureRange(FromIndex: Integer; ToIndex: Integer);
begin
  AsBoldList.EnsureRange(FromIndex, ToIndex);
end;

function  TBoldComListAdapter.Includes(const Element: IBoldElement): WordBool;
begin
  Result := AsBoldList.Includes(InterfaceToElement(Element));
end;

function  TBoldComListAdapter.IndexOf(const Element: IBoldElement): Integer;
begin
  Result := AsBoldList.IndexOf(InterfaceToElement(Element));
end;

procedure TBoldComListAdapter.Insert(Index: Integer; const Element: IBoldElement);
begin
  AsBoldList.Insert(Index, InterfaceToElement(Element));
end;

procedure TBoldComListAdapter.InsertNew(Index: Integer);
begin
  AsBoldList.InsertNew(Index);
end;

procedure TBoldComListAdapter.Move(CurrentIndex: Integer; NewIndex: Integer);
begin
  AsBoldList.Move(CurrentIndex, NewIndex);
end;

procedure TBoldComListAdapter.Remove(const Element: IBoldElement);
begin
  AsBoldList.Remove(InterfaceToElement(Element));
end;

procedure TBoldComListAdapter.RemoveByIndex(Index: Integer);
begin
  AsBoldList.RemoveByIndex(Index);
end;

function TBoldComListAdapter.ToStringList(Representation: Integer): OleVariant;
begin
end;

function TBoldComListAdapter.ToStringListWithNil(Representation: Integer;
  const NilString: WideString): OleVariant;
begin
end;

function  TBoldComListAdapter.Get_CanCreateNew: WordBool;
begin
  Result := AsBoldList.CanCreateNew;
end;

function  TBoldComListAdapter.Get_Count: Integer;
begin
  Result := AsBoldList.Count;
end;

function  TBoldComListAdapter.Get_DuplicateMode: Integer;
begin
  Result := Integer(AsBoldList.DuplicateMode);
end;

procedure TBoldComListAdapter.Set_DuplicateMode(Value: Integer);
begin
  AsBoldList.DuplicateMode := TBoldListDupMode(Value);
end;

function  TBoldComListAdapter.Get_Elements(Index: Integer): IBoldElement;
begin
  BoldComCreateAdapter(AsBoldList.Elements[Index], False, IBoldElement, Result);
end;

procedure TBoldComListAdapter.Set_Elements(Index: Integer; const Value: IBoldElement);
begin
  AsBoldList.Elements[Index] := InterfaceToElement(Value);
end;

{-- TBoldComObjectListAdapter -------------------------------------------------}

constructor TBoldComObjectListAdapter.Create(AdaptableObject: TBoldAdaptableObject;
  Owner: Boolean; const TypeLib: ITypeLib; const DispIntf: TGUID);
begin
  if Assigned(TypeLib) then
    inherited Create(AdaptableObject, Owner, TypeLib, DispIntf)
  else
    inherited Create(AdaptableObject, Owner, BoldComObjectSpaceTypeLibrary, IBoldObjectList);
end;

function TBoldComObjectListAdapter.GetAsObjectList: TBoldObjectList;
begin
  Result := EnsuredAdaptee as TBoldObjectList;
end;

{ IBoldObjectList }

function  TBoldComObjectListAdapter.Get_BoldObjects(Index: Integer): IBoldObject;
begin
  BoldComCreateAdapter(AsObjectList.BoldObjects[Index], False, IBoldObject, Result);
end;

procedure TBoldComObjectListAdapter.Set_BoldObjects(Index: Integer; const Value: IBoldObject);
begin
  AsObjectList.BoldObjects[Index] := InterfaceToElement(Value) as TBoldObject;
end;

function  TBoldComObjectListAdapter.Get_BoldRoleRTInfo: IUnknown;
begin
  Result := nil;
end;

function  TBoldComObjectListAdapter.Get_SubscribeToObjectsInList: WordBool;
begin
  Result := AsObjectList.SubscribeToObjectsInList;
end;

procedure TBoldComObjectListAdapter.Set_SubscribeToObjectsInList(Value: WordBool);
begin
  AsObjectList.SubscribeToObjectsInList := Value;
end;

{-- TBoldComMemberListAdapter -------------------------------------------------}

constructor TBoldComMemberListAdapter.Create(AdaptableObject: TBoldAdaptableObject;
  Owner: Boolean; const TypeLib: ITypeLib; const DispIntf: TGUID);
begin
  if Assigned(TypeLib) then
    inherited Create(AdaptableObject, Owner, TypeLib, DispIntf)
  else
    inherited Create(AdaptableObject, Owner, BoldComObjectSpaceTypeLibrary, IBoldMemberList);
end;

function TBoldComMemberListAdapter.GetAsMemberList: TBoldMemberList;
begin
  Result := EnsuredAdaptee as TBoldMemberList;
end;

{ IBoldMemberList }

function  TBoldComMemberListAdapter.Get_BoldMembers(Index: Integer): IBoldMember;
begin
  BoldComCreateAdapter(AsMemberList.BoldMembers[Index], False, IBoldMember, Result);
end;

procedure TBoldComMemberListAdapter.Set_BoldMembers(Index: Integer; const Value: IBoldMember);
begin
  AsMemberList.BoldMembers[Index] := InterfaceToElement(Value) as TBoldMember;
end;

{-- TBoldComSystemAdapter -----------------------------------------------------}

constructor TBoldComSystemAdapter.Create(AdaptableObject: TBoldAdaptableObject;
  Owner: Boolean; const TypeLib: ITypeLib; const DispIntf: TGUID);
begin
  if Assigned(TypeLib) then
    inherited Create(AdaptableObject, Owner, TypeLib, DispIntf)
  else
    inherited Create(AdaptableObject, Owner, BoldComObjectSpaceTypeLibrary, IBoldSystem);
end;

function TBoldComSystemAdapter.GetAsSystem: TBoldSystem;
begin
  Result := EnsuredAdaptee as TBoldSystem;
end;

{ IBoldSystem }

function TBoldComSystemAdapter.CreateNewMember(
  const ExpressionName: WideString): IBoldMember;
begin
  Result := nil;
end;

function TBoldComSystemAdapter.CreateNewObject(
  const ExpressionName: WideString; Persistent: WordBool): IBoldObject;
begin
  BoldComCreateAdapter(AsSystem.CreateNewObjectByExpressionName(ExpressionName),
                       False,
                       IBoldObject,
                       Result);
end;

function TBoldComSystemAdapter.CreateNewObjectWithMemberValues(
  const ExpressionName: WideString; Persistent: WordBool;
  MemberValues: OleVariant): IBoldObject; safecall;
begin
  Result := nil;
end;

procedure TBoldComSystemAdapter.Discard;
begin
  AsSystem.Discard;
end;

function TBoldComSystemAdapter.EnsureEnclosure(const ObjectList: IBoldObjectList;
  ValidateOnly: WordBool): WordBool;
begin
  Result := AsSystem.EnsureEnclosure(InterfaceToElement(ObjectList) as TBOldObjectList, ValidateOnly);
end;

function TBoldComSystemAdapter.GetObjectBySessionId(SessionId: OleVariant): IBoldObject;
var
  Address: Integer;
  Obj: TBoldObject;
begin
  try
    Address := StrToInt(SessionId);
    Obj := TBoldObject(Address);
    Obj.BoldMemberCount;
    BoldComCreateAdapter(Obj, False, IBoldObject, Result);
  except
    on Exception do Result := nil;
  end;
end;

procedure TBoldComSystemAdapter.UpdateDatabase;
begin
  AsSystem.UpdateDatabase;
end;

procedure TBoldComSystemAdapter.UpdateDatabaseWithList(const ObjectList: IBoldObjectList);
begin
  AsSystem.UpdateDatabaseWithList(InterfaceToElement(ObjectList) as TBoldObjectList);
end;

function TBoldComSystemAdapter.Get_BoldSystemTypeInfo: IBoldSystemTypeInfo;
begin
  BoldComCreateAdapter(AsSystem.BoldSystemTypeInfo, False, IBoldSystemTypeInfo, Result);
end;

function TBoldComSystemAdapter.Get_ClassByExpressionName(
  const ExpressionName: WideString): IBoldObjectList;
begin
  BoldComCreateAdapter(AsSystem.ClassByExpressionName[ExpressionName],
                       False,
                       IBoldObjectList,
                       Result);
end;

function TBoldComSystemAdapter.Get_DirtyObjects: IBoldObjectList;
begin
  Result := nil;
end;

function TBoldComSystemAdapter.Get_LoadedObjects: IBoldObjectList;
begin
  Result := nil;
end;

{-- TBoldComBlobAdapter -----------------------------------------------------}

constructor TBoldComBlobAdapter.Create(AdaptableObject: TBoldAdaptableObject;
  Owner: Boolean; const TypeLib: ITypeLib; const DispIntf: TGUID);
begin
  if Assigned(TypeLib) then
    inherited Create(AdaptableObject, Owner, TypeLib, DispIntf)
  else
    inherited Create(AdaptableObject, Owner, BoldComObjectSpaceTypeLibrary, IBoldBlob);
end;

function TBoldComBlobAdapter.GetAsBoldBlob: TBABlob;
begin
  Result := EnsuredAdaptee as TBABlob;
end;

{ IBoldBlob }

{-- TBoldComElementHandleAdapter ----------------------------------------------}

constructor TBoldComElementHandleAdapter.Create(AdaptableObject: TBoldAdaptableObject;
  Owner: Boolean; const TypeLib: ITypeLib; const DispIntf: TGUID);
begin
  if Assigned(TypeLib) then
    inherited Create(AdaptableObject, Owner, TypeLib, DispIntf)
  else
    inherited Create(AdaptableObject, Owner, BoldComObjectSpaceTypeLibrary, IBoldElementHandle);
end;

function TBoldComElementHandleAdapter.GetAsElementHandle: TBoldElementHandle;
begin
  Result := EnsuredAdaptee as TBoldElementHandle;
end;

{ IBoldElement }

procedure TBoldComElementHandleAdapter.AddSmallSubscription(const ClientId: WideString;
  SubscriberId: Integer; SmallEvents: Integer; RequestedEvent: Integer;
  CancelOld: WordBool); safecall;
var
  Sub: TBoldComServerSubscriber;
begin
  Sub := TBoldComServerSubscriber.GetSubscriber(ClientId, SubscriberId);
  if assigned(sub) and CancelOld then
    Sub.CancelAllSubscriptions;
  AsElementHandle.AddSmallSubscription(Sub, TBoldSmallEventSet(SmallEvents), RequestedEvent);
end;

procedure TBoldComElementHandleAdapter.AddSubscription(const ClientId: WideString;
  SubscriberId: Integer; Event: Integer; RequestedEvent: Integer; CancelOld: WordBool);
var
  Sub: TBoldComServerSubscriber;
begin
  Sub := TBoldComServerSubscriber.GetSubscriber(ClientId, SubscriberId);
  if assigned(sub) and CancelOld then
    Sub.CancelAllSubscriptions;
  AsElementHandle.AddSubscription(Sub, Event, RequestedEvent);
end;

function TBoldComElementHandleAdapter.GetData(DataFlags: Integer;
  out Value: IBoldElement; out DynamicBoldType: IBoldElementTypeInfo;
  out StaticBoldType: IBoldElementTypeInfo; out StaticSystemType: IBoldSystemTypeInfo;
  out BoldSystem: IBoldSystem; out StaticRootType: IBoldElementTypeInfo;
  out CurrentBoldObject: IBoldObject;
  out BoldList: IBoldList; out ListElementType: IBoldElementTypeInfo;
  out NamedValues: OleVariant): WordBool;
begin
  Result := False;
end;

function TBoldComElementHandleAdapter.SetData(DataFlags: Integer;
  const Value: IBoldElement; NamedValues: OleVariant): WordBool;
begin
  Result := False;
end;

{-- TBoldComSystemHandleAdapter ----------------------------------------------}

constructor TBoldComSystemHandleAdapter.Create(AdaptableObject: TBoldAdaptableObject;
  Owner: Boolean; const TypeLib: ITypeLib; const DispIntf: TGUID);
begin
  if Assigned(TypeLib) then
    inherited Create(AdaptableObject, Owner, TypeLib, DispIntf)
  else
    inherited Create(AdaptableObject, Owner, BoldComObjectSpaceTypeLibrary, IBoldElementHandle);
end;

function TBoldComSystemHandleAdapter.GetAsSystemHandle: TBoldSystemHandle;
begin
  Result := EnsuredAdaptee as TBoldSystemHandle;
end;

function TBoldComSystemHandleAdapter.GetData(DataFlags: Integer;
  out Value: IBoldElement; out DynamicBoldType: IBoldElementTypeInfo;
  out StaticBoldType: IBoldElementTypeInfo; out StaticSystemType: IBoldSystemTypeInfo;
  out BoldSystem: IBoldSystem; out StaticRootType: IBoldElementTypeInfo;
  out CurrentBoldObject: IBoldObject;
  out BoldList: IBoldList; out ListElementType: IBoldElementTypeInfo;
  out NamedValues: OleVariant): WordBool;
var
  This: TBoldSystemHandle;
begin
  Result := True;
  This := AsSystemHandle;
  if dataflags = DF_HANDLEID then
    NamedValues := BoldCreateNamedValues(['HandleId'], [integer(this)])
  else
  begin
    BoldComCreateAdapter(This.Value, False, IBoldElement, Value);
    BoldComCreateAdapter(This.DynamicBoldType, False, IBoldElementTypeInfo, DynamicBoldType);
    BoldComCreateAdapter(This.StaticBoldType, False, IBoldElementTypeInfo, StaticBoldType);
    BoldComCreateAdapter(This.StaticSystemTypeInfo, False,IBoldSystemTypeInfo, StaticSystemType);
    if This.Active then
      BoldComCreateAdapter(This.System,False,IBoldSystem,BoldSystem)
    else
      BoldSystem := nil;
    StaticRootType := nil;
    CurrentBoldObject := nil;
    BoldList := nil;
    ListElementType := nil;
    NamedValues := BoldCreateNamedValues(
      ['HandleId',
       'Active',
       'Persistent'],
      [Integer(This),
       This.Active,
       This.Persistent]);
  end;
end;

function TBoldComSystemHandleAdapter.SetData(DataFlags: Integer;
  const Value: IBoldElement; NamedValues: OleVariant): WordBool;
begin
  Result := False;
end;

{-- TBoldComDerivedHandleAdapter ----------------------------------------------}

constructor TBoldComDerivedHandleAdapter.Create(AdaptableObject: TBoldAdaptableObject;
  Owner: Boolean; const TypeLib: ITypeLib; const DispIntf: TGUID);
begin
  if Assigned(TypeLib) then
    inherited Create(AdaptableObject, Owner, TypeLib, DispIntf)
  else
    inherited Create(AdaptableObject, Owner, BoldComObjectSpaceTypeLibrary, IBoldElementHandle);
end;

function TBoldComDerivedHandleAdapter.GetAsDerivedHandle: TBoldDerivedHandle;
begin
  Result := EnsuredAdaptee as TBoldDerivedHandle;
end;

function TBoldComDerivedHandleAdapter.GetData(DataFlags: Integer;
  out Value: IBoldElement; out DynamicBoldType: IBoldElementTypeInfo;
  out StaticBoldType: IBoldElementTypeInfo; out StaticSystemType: IBoldSystemTypeInfo;
  out BoldSystem: IBoldSystem; out StaticRootType: IBoldElementTypeInfo;
  out CurrentBoldObject: IBoldObject;
  out BoldList: IBoldList; out ListElementType: IBoldElementTypeInfo;
  out NamedValues: OleVariant): WordBool;
var
  This: TBoldDerivedHandle;
begin
  Result := True;
  This := AsDerivedHandle;
  if dataflags = DF_HANDLEID then
    NamedValues := BoldCreateNamedValues(['HandleId'], [integer(this)])
  else
  begin
    BoldComCreateAdapter(This.Value, False, IBoldElement, Value);
    BoldComCreateAdapter(This.DynamicBoldType, False, IBoldElementTypeInfo, DynamicBoldType);
    BoldComCreateAdapter(This.StaticBoldType, False, IBoldElementTypeInfo, StaticBoldType);
    BoldComCreateAdapter(This.StaticSystemTypeInfo, False, IBoldSystemTypeInfo, StaticSystemType);
    BoldSystem := nil;
    BoldComCreateAdapter(This.StaticRootType, False, IBoldElementTypeInfo, StaticRootType);
    CurrentBoldObject := nil;
    BoldList := nil;
    ListElementType := nil;
    NamedValues := BoldCreateNamedValues(
      ['HandleId',
       'StaticSystemHandle',
       'Enabled',
       'RootHandle',
       'RootTypeName',
       'Subscribe'],
      [Integer(This),
       Integer(This.StaticSystemHandle),
       This.Enabled,
       Integer(This.RootHandle),
       This.RootTypeName,
       This.Subscribe]);
  end;
end;

function TBoldComDerivedHandleAdapter.SetData(DataFlags: Integer;
  const Value: IBoldElement; NamedValues: OleVariant): WordBool;
var
  This: TBoldDerivedHandle;
  HandleId: Integer;
begin
  This := AsDerivedHandle;
  if (DataFlags and DF_STATICSYSTEMHANDLE) <> 0 then
  begin
    HandleId := BoldGetNamedValue(NamedValues,'StaticSystemHandle');
    if HandleId = 0 then
      This.StaticSystemHandle := nil
    else
      This.StaticSystemHandle := TBoldAbstractSystemHandle(HandleId);
  end;
  if (DataFlags and DF_ENABLED) <> 0 then
  begin
    This.Enabled := BoldGetNamedValue(NamedValues, 'Enabled');
  end;
  if (DataFlags and DF_ROOTHANDLE) <> 0 then
  begin
    HandleId := BoldGetNamedValue(NamedValues, 'RootHandle');
    if HandleId = 0 then
      This.RootHandle := nil
    else
      This.RootHandle := TBoldElementHandle(HandleId);
  end;
  if (DataFlags and DF_ROOTTYPENAME) <> 0 then
  begin
    This.RootTypeName := BoldGetNamedValue(NamedValues, 'RootTypeName');
  end;
  if (DataFlags and DF_SUBSCRIBE) <> 0 then
  begin
    This.Subscribe := BoldGetNamedValue(NamedValues, 'Subscribe');
  end;
end;

{-- TBoldComExpressionHandleAdapter ----------------------------------------------}

constructor TBoldComExpressionHandleAdapter.Create(AdaptableObject: TBoldAdaptableObject;
  Owner: Boolean; const TypeLib: ITypeLib; const DispIntf: TGUID);
begin
  if Assigned(TypeLib) then
    inherited Create(AdaptableObject, Owner, TypeLib, DispIntf)
  else
    inherited Create(AdaptableObject, Owner, BoldComObjectSpaceTypeLibrary, IBoldElementHandle);
end;

function TBoldComExpressionHandleAdapter.GetAsExpressionHandle: TBoldExpressionHandle;
begin
  Result := EnsuredAdaptee as TBoldExpressionHandle;
end;

function TBoldComExpressionHandleAdapter.GetData(DataFlags: Integer;
  out Value: IBoldElement; out DynamicBoldType: IBoldElementTypeInfo;
  out StaticBoldType: IBoldElementTypeInfo; out StaticSystemType: IBoldSystemTypeInfo;
  out BoldSystem: IBoldSystem; out StaticRootType: IBoldElementTypeInfo;
  out CurrentBoldObject: IBoldObject;
  out BoldList: IBoldList; out ListElementType: IBoldElementTypeInfo;
  out NamedValues: OleVariant): WordBool;
var
  This: TBoldExpressionHandle;
begin
  Result := True;
  This := AsExpressionHandle;
  if dataflags = DF_HANDLEID then
    NamedValues := BoldCreateNamedValues(['HandleId'], [integer(this)])
  else
  begin
    BoldComCreateAdapter(This.Value, False, IBoldElement, Value);
    BoldComCreateAdapter(This.DynamicBoldType, False, IBoldElementTypeInfo, DynamicBoldType);
    BoldComCreateAdapter(This.StaticBoldType, False, IBoldElementTypeInfo, StaticBoldType);
    BoldComCreateAdapter(This.StaticSystemTypeInfo, False, IBoldSystemTypeInfo, StaticSystemType);
    BoldSystem := nil;
    BoldComCreateAdapter(This.StaticRootType, False, IBoldElementTypeInfo, StaticRootType);
    CurrentBoldObject := nil;
    BoldList := nil;
    ListElementType := nil;
    NamedValues := BoldCreateNamedValues(
      ['HandleId',
      'StaticSystemHandle',
      'Enabled',
      'RootHandle',
      'RootTypeName',
      'Subscribe',
      'Expression',
      'EvaluateInPS'],
      [Integer(This),
      Integer(This.StaticSystemHandle),
      This.Enabled,
      Integer(This.RootHandle),
      This.RootTypeName,
      This.Subscribe,
      This.Expression,
      This.EvaluateInPS]);
  end;
end;

function TBoldComExpressionHandleAdapter.SetData(DataFlags: Integer;
  const Value: IBoldElement; NamedValues: OleVariant): WordBool;
var
  This: TBoldExpressionHandle;
  HandleId: Integer;
begin
  This := AsExpressionHandle;
  if (DataFlags and DF_STATICSYSTEMHANDLE) <> 0 then
  begin
    HandleId := BoldGetNamedValue(NamedValues, 'StaticSystemHandle');
    if HandleId = 0 then
      This.StaticSystemHandle := nil
    else
      This.StaticSystemHandle := TBoldAbstractSystemHandle(HandleId);
  end;
  if (DataFlags and DF_ENABLED) <> 0 then
  begin
    This.Enabled := BoldGetNamedValue(NamedValues, 'Enabled');
  end;
  if (DataFlags and DF_ROOTHANDLE) <> 0 then
  begin
    HandleId := BoldGetNamedValue(NamedValues, 'RootHandle');
    if HandleId = 0 then
      This.RootHandle := nil
    else
      This.RootHandle := TBoldElementHandle(HandleId);
  end;
  if (DataFlags and DF_ROOTTYPENAME) <> 0 then
  begin
    This.RootTypeName := BoldGetNamedValue(NamedValues, 'RootTypeName');
  end;
  if (DataFlags and DF_SUBSCRIBE) <> 0 then
  begin
    This.Subscribe := BoldGetNamedValue(NamedValues, 'Subscribe');
  end;
  if (DataFlags and DF_EXPRESSION) <> 0 then
  begin
    This.Expression := BoldGetNamedValue(NamedValues, 'Expression');
  end;
  if (DataFlags and DF_EVALUATEINPS) <> 0 then
  begin
    This.EvaluateInPs := BoldGetNamedValue(NamedValues,'EvaluateInPS');
  end;
end;

{-- TBoldComCursorHandleAdapter ----------------------------------------------}

constructor TBoldComCursorHandleAdapter.Create(AdaptableObject: TBoldAdaptableObject;
  Owner: Boolean; const TypeLib: ITypeLib; const DispIntf: TGUID);
begin
  if Assigned(TypeLib) then
    inherited Create(AdaptableObject, Owner, TypeLib, DispIntf)
  else
    inherited Create(AdaptableObject, Owner, BoldComObjectSpaceTypeLibrary, IBoldElementHandle);
end;

function TBoldComCursorHandleAdapter.GetAsCursorHandle: TBoldCursorHandle;
begin
  Result := EnsuredAdaptee as TBoldCursorHandle;
end;

function TBoldComCursorHandleAdapter.GetData(DataFlags: Integer;
  out Value: IBoldElement; out DynamicBoldType: IBoldElementTypeInfo;
  out StaticBoldType: IBoldElementTypeInfo; out StaticSystemType: IBoldSystemTypeInfo;
  out BoldSystem: IBoldSystem; out StaticRootType: IBoldElementTypeInfo;
  out CurrentBoldObject: IBoldObject;
  out BoldList: IBoldList; out ListElementType: IBoldElementTypeInfo;
  out NamedValues: OleVariant): WordBool;
var
  This: TBoldCursorHandle;
begin
  Result := True;
  This := AsCursorHandle;
  if dataflags = DF_HANDLEID then
    NamedValues := BoldCreateNamedValues(['HandleId'], [integer(this)])
  else
  begin
    BoldComCreateAdapter(This.Value, False, IBoldElement, Value);
    BoldComCreateAdapter(This.DynamicBoldType, False, IBoldElementTypeInfo, DynamicBoldType);
    BoldComCreateAdapter(This.StaticBoldType, False, IBoldElementTypeInfo, StaticBoldType);
    BoldComCreateAdapter(This.StaticSystemTypeInfo, False, IBoldSystemTypeInfo, StaticSystemType);
    BoldSystem := nil;
    BoldComCreateAdapter(This.StaticRootType, False, IBoldElementTypeInfo, StaticRootType);
    if this.value is TBoldObject then
      BoldComCreateAdapter(This.CurrentBoldObject, False, IBoldObject, CurrentBoldObject)
    else
      CurrentBoldObject := nil;
    BoldComCreateAdapter(This.List, False, IBoldList, BoldList);
    BoldComCreateAdapter(This.ListElementType, False, IBoldElementTypeInfo, ListElementType);
    NamedValues := BoldCreateNamedValues(
      ['HandleId',
      'StaticSystemHandle',
      'Enabled',
      'RootHandle',
      'RootTypeName',
      'Subscribe',
      'Count',
      'CurrentIndex',
      'AutoFirst'],
      [Integer(This),
      Integer(This.StaticSystemHandle),
      This.Enabled,
      Integer(This.RootHandle),
      This.RootTypeName,
      This.Subscribe,
      This.Count,
      This.CurrentIndex,
      This.AutoFirst]);
  end;
end;

function TBoldComCursorHandleAdapter.SetData(DataFlags: Integer;
  const Value: IBoldElement; NamedValues: OleVariant): WordBool;
var
  This: TBoldCursorHandle;
  HandleId: Integer;
  NewIndex: integer;
begin
  This := AsCursorHandle;
  if (DataFlags and DF_STATICSYSTEMHANDLE) <> 0 then
  begin
    HandleId := BoldGetNamedValue(NamedValues, 'StaticSystemHandle');
    if HandleId = 0 then
      This.StaticSystemHandle := nil
    else
      This.StaticSystemHandle := TBoldAbstractSystemHandle(HandleId);
  end;

  if (DataFlags and DF_ENABLED) <> 0 then
  begin
    This.Enabled := BoldGetNamedValue(NamedValues, 'Enabled');
  end;

  if (DataFlags and DF_ROOTHANDLE) <> 0 then
  begin
    HandleId := BoldGetNamedValue(NamedValues, 'RootHandle');
    if HandleId = 0 then
      This.RootHandle := nil
    else
      This.RootHandle := TBoldElementHandle(HandleId);
  end;

  if (DataFlags and DF_ROOTTYPENAME) <> 0 then
  begin
    This.RootTypeName := BoldGetNamedValue(NamedValues, 'RootTypeName');
  end;

  if (DataFlags and DF_SUBSCRIBE) <> 0 then
  begin
    This.Subscribe := BoldGetNamedValue(NamedValues, 'Subscribe');
  end;

  if (DataFlags and DF_CURRENTINDEX) <> 0 then
  begin
     NewIndex := BoldGetNamedValue(NamedValues,'CurrentIndex');
     if assigned(this.list) and (NewIndex < this.list.Count) and (newIndex >= -1) then
      This.CurrentIndex := NewIndex;
  end;

  if (DataFlags and DF_AUTOFIRST) <> 0 then
  begin
    This.AutoFirst := BoldGetNamedValue(NamedValues, 'AutoFirst');
  end;
end;

{-- TBoldComListHandleAdapter ----------------------------------------------}

constructor TBoldComListHandleAdapter.Create(AdaptableObject: TBoldAdaptableObject;
  Owner: Boolean; const TypeLib: ITypeLib; const DispIntf: TGUID);
begin
  if Assigned(TypeLib) then
    inherited Create(AdaptableObject, Owner, TypeLib, DispIntf)
  else
    inherited Create(AdaptableObject, Owner, BoldComObjectSpaceTypeLibrary, IBoldElementHandle);
end;

function TBoldComListHandleAdapter.GetAsListHandle: TBoldListHandle;
begin
  Result := EnsuredAdaptee as TBoldListHandle;
end;

function TBoldComListHandleAdapter.GetData(DataFlags: Integer;
  out Value: IBoldElement; out DynamicBoldType: IBoldElementTypeInfo;
  out StaticBoldType: IBoldElementTypeInfo; out StaticSystemType: IBoldSystemTypeInfo;
  out BoldSystem: IBoldSystem; out StaticRootType: IBoldElementTypeInfo;
  out CurrentBoldObject: IBoldObject;
  out BoldList: IBoldList; out ListElementType: IBoldElementTypeInfo;
  out NamedValues: OleVariant): WordBool;
var
  This: TBoldListHandle;
begin
  Result := True;
  This := AsListHandle;
  if dataflags = DF_HANDLEID then
    NamedValues := BoldCreateNamedValues(['HandleId'], [integer(this)])
  else
  begin
    BoldComCreateAdapter(This.Value, False, IBoldElement, Value);
    BoldComCreateAdapter(This.DynamicBoldType, False, IBoldElementTypeInfo, DynamicBoldType);
    BoldComCreateAdapter(This.StaticBoldType, False, IBoldElementTypeInfo, StaticBoldType);
    BoldComCreateAdapter(This.StaticSystemTypeInfo, False, IBoldSystemTypeInfo, StaticSystemType);
    BoldSystem := nil;
    BoldComCreateAdapter(This.StaticRootType, False, IBoldElementTypeInfo, StaticRootType);
    if this.value is TBoldObject then
      BoldComCreateAdapter(This.CurrentBoldObject, False, IBoldObject, CurrentBoldObject)
    else
      CurrentBoldObject := nil;
    BoldComCreateAdapter(This.List, False, IBoldList, BoldList);
    BoldComCreateAdapter(This.ListElementType, False, IBoldElementTypeInfo, ListElementType);
    NamedValues := BoldCreateNamedValues(
      ['HandleId',
      'StaticSystemHandle',
      'Enabled',
      'RootHandle',
      'RootTypeName',
      'Subscribe',
      'Count',
      'CurrentIndex',
      'AutoFirst',
      'Expression',
      'EvaluateInPS'],
      [Integer(This),
      Integer(This.StaticSystemHandle),
      This.Enabled,
      Integer(This.RootHandle),
      This.RootTypeName,
      This.Subscribe,
      This.Count,
      This.CurrentIndex,
      This.AutoFirst,
      This.Expression,
      this.EvaluateInPS]);
  end;
end;

function TBoldComListHandleAdapter.SetData(DataFlags: Integer;
  const Value: IBoldElement; NamedValues: OleVariant): WordBool;
var
  This: TBoldListHandle;
  HandleId: Integer;
  NewIndex: integer;
begin
  This := AsListHandle;
  if (DataFlags and DF_STATICSYSTEMHANDLE) <> 0 then
  begin
    HandleId := BoldGetNamedValue(NamedValues, 'StaticSystemHandle');
    if HandleId = 0 then
      This.StaticSystemHandle := nil
    else
      This.StaticSystemHandle := TBoldAbstractSystemHandle(HandleId);
  end;

  if (DataFlags and DF_ROOTHANDLE) <> 0 then
  begin
    HandleId := BoldGetNamedValue(NamedValues, 'RootHandle');
    if HandleId = 0 then
      This.RootHandle := nil
    else
      This.RootHandle := TBoldElementHandle(HandleId);
  end;

  if (DataFlags and DF_ROOTTYPENAME) <> 0 then
  begin
    This.RootTypeName := BoldGetNamedValue(NamedValues, 'RootTypeName');
  end;

  if (DataFlags and DF_SUBSCRIBE) <> 0 then
  begin
    This.Subscribe := BoldGetNamedValue(NamedValues, 'Subscribe');
  end;

  if (DataFlags and DF_EXPRESSION) <> 0 then
  begin
    This.Expression := BoldGetNamedValue(NamedValues, 'Expression');
  end;

  if (DataFlags and DF_ENABLED) <> 0 then
  begin
    This.Enabled := BoldGetNamedValue(NamedValues, 'Enabled');
  end;

  if (DataFlags and DF_CURRENTINDEX) <> 0 then
  begin
    NewIndex := BoldGetNamedValue(NamedValues,'CurrentIndex');
    if assigned(this.list) and (NewIndex < this.list.Count) and (newIndex >= -1) then
      This.CurrentIndex := NewIndex;
  end;

  if (DataFlags and DF_AUTOFIRST) <> 0 then
  begin
    This.AutoFirst := BoldGetNamedValue(NamedValues,'AutoFirst');
  end;

  if (DataFlags and DF_EVALUATEINPS) <> 0 then
  begin
    This.EvaluateInPs := BoldGetNamedValue(NamedValues,'EvaluateInPS');
  end;

end;

{-- TBoldComReferenceHandleAdapter --------------------------------------------}

constructor TBoldComReferenceHandleAdapter.Create(AdaptableObject: TBoldAdaptableObject;
  Owner: Boolean; const TypeLib: ITypeLib; const DispIntf: TGUID);
begin
  if Assigned(TypeLib) then
    inherited Create(AdaptableObject, Owner, TypeLib, DispIntf)
  else
    inherited Create(AdaptableObject, Owner, BoldComObjectSpaceTypeLibrary, IBoldElementHandle);
end;

function TBoldComReferenceHandleAdapter.GetAsReferenceHandle: TBoldReferenceHandle;
begin
  Result := EnsuredAdaptee as TBoldReferenceHandle;
end;

function TBoldComReferenceHandleAdapter.GetData(DataFlags: Integer;
  out Value: IBoldElement; out DynamicBoldType: IBoldElementTypeInfo;
  out StaticBoldType: IBoldElementTypeInfo; out StaticSystemType: IBoldSystemTypeInfo;
  out BoldSystem: IBoldSystem; out StaticRootType: IBoldElementTypeInfo;
  out CurrentBoldObject: IBoldObject;
  out BoldList: IBoldList; out ListElementType: IBoldElementTypeInfo;
  out NamedValues: OleVariant): WordBool;
var
  This: TBoldReferenceHandle;
begin
  Result := True;
  This := AsReferenceHandle;
  if dataflags = DF_HANDLEID then
    NamedValues := BoldCreateNamedValues(['HandleId'], [integer(this)])
  else
  begin
    BoldComCreateAdapter(This.Value, False, IBoldElement, Value);
    BoldComCreateAdapter(This.DynamicBoldType, False, IBoldElementTypeInfo, DynamicBoldType);
    BoldComCreateAdapter(This.StaticBoldType, False, IBoldElementTypeInfo, StaticBoldType);
    BoldComCreateAdapter(This.StaticSystemTypeInfo, False, IBoldSystemTypeInfo, StaticSystemType);
    BoldSystem := nil;
    StaticRootType := nil;
    CurrentBoldObject := nil;
    BoldList := nil;
    ListElementType := nil;
    NamedValues := BoldCreateNamedValues(
      ['HandleId',
      'StaticSystemHandle',
      'StaticValueTypeName'],
      [Integer(This),
      Integer(This.StaticSystemHandle),
      This.StaticValueTypeName]);
  end;
end;

function TBoldComReferenceHandleAdapter.SetData(DataFlags: Integer;
  const Value: IBoldElement; NamedValues: OleVariant): WordBool;
var
  This: TBoldReferenceHandle;
  HandleId: Integer;
begin
  This := AsReferenceHandle;
  if (DataFlags and DF_VALUE) <> 0 then
  begin
    This.Value := InterfaceToElement(Value);
  end;

  if (DataFlags and DF_STATICSYSTEMHANDLE) <> 0 then
  begin
    HandleId := BoldGetNamedValue(NamedValues, 'StaticSystemHandle');
    if HandleId = 0 then
      This.StaticSystemHandle := nil
    else
      This.StaticSystemHandle := TBoldAbstractSystemHandle(HandleId);
  end;

  if (DataFlags and DF_STATICVALUETYPENAME) <> 0 then
  begin
    This.StaticValueTypeName := BoldGetNamedValue(NamedValues, 'StaticValueTypeName');
  end;
end;

{-- TBoldComSQLHandleAdapter --------------------------------------------}

constructor TBoldComSQLHandleAdapter.Create(AdaptableObject: TBoldAdaptableObject;
  Owner: Boolean; const TypeLib: ITypeLib; const DispIntf: TGUID);
begin
  if Assigned(TypeLib) then
    inherited Create(AdaptableObject, Owner, TypeLib, DispIntf)
  else
    inherited Create(AdaptableObject, Owner, BoldComObjectSpaceTypeLibrary, IBoldElementHandle);
end;

function TBoldComSQLHandleAdapter.GetAsSQLHandle: TBoldSQLHandle;
begin
  Result := EnsuredAdaptee as TBoldSQLHandle;
end;

function TBoldComSQLHandleAdapter.GetData(DataFlags: Integer;
  out Value: IBoldElement; out DynamicBoldType: IBoldElementTypeInfo;
  out StaticBoldType: IBoldElementTypeInfo; out StaticSystemType: IBoldSystemTypeInfo;
  out BoldSystem: IBoldSystem; out StaticRootType: IBoldElementTypeInfo;
  out CurrentBoldObject: IBoldObject;
  out BoldList: IBoldList; out ListElementType: IBoldElementTypeInfo;
  out NamedValues: OleVariant): WordBool;
var
  This: TBoldSQLHandle;
begin
  Result := True;
  This := AsSQLHandle;
  if dataflags = DF_HANDLEID then
    NamedValues := BoldCreateNamedValues(['HandleId'], [integer(this)])
  else
  begin
    BoldComCreateAdapter(This.Value, False, IBoldElement, Value);
    BoldComCreateAdapter(This.DynamicBoldType, False, IBoldElementTypeInfo, DynamicBoldType);
    BoldComCreateAdapter(This.StaticBoldType, False, IBoldElementTypeInfo, StaticBoldType);
    BoldComCreateAdapter(This.StaticSystemTypeInfo, False,IBoldSystemTypeInfo, StaticSystemType);
    BoldSystem := nil;
    StaticRootType := nil;
    CurrentBoldObject := nil;
    BoldList := nil;
    ListElementType := nil;
    NamedValues := BoldCreateNamedValues(
      ['HandleId',
      'StaticSystemHandle',
      'ClassExpressionName',
      'ClearBeforeExecute',
      'SQLOrderByClause',
      'SQLWhereClause'],
      [Integer(This),
      Integer(This.StaticSystemHandle),
      This.ClassExpressionName,
      This.ClearBeforeExecute,
      This.SQLOrderByClause,
      This.SQLWhereClause]);
  end;
end;

function TBoldComSQLHandleAdapter.SetData(DataFlags: Integer;
  const Value: IBoldElement; NamedValues: OleVariant): WordBool;
var
  This: TBoldSQLHandle;
  HandleId: Integer;
  Action: String;
begin
  This := AsSQLHandle;
  if DataFlags = -1 then
  begin
    Action := BoldGetnamedValue(NamedValues, 'Action');
    if SameText(Action, 'ExecuteSQL') then
      this.ExecuteSQL
    else if SameText(Action, 'ClearList') then
      this.ClearList
  end
  else begin
    if (DataFlags and DF_STATICSYSTEMHANDLE) <> 0 then
    begin
      HandleId := BoldGetNamedValue(NamedValues, 'StaticSystemHandle');
      if HandleId = 0 then
        This.StaticSystemHandle := nil
      else
        This.StaticSystemHandle := TBoldAbstractSystemHandle(HandleId);
    end;
    if (DataFlags and DF_CLASSEXPRESSIONNAME) <> 0 then
    begin
      This.ClassExpressionName := BoldGetNamedValue(NamedValues, 'ClassExpressionName');
    end;
    if (DataFlags and DF_CLEARBEFOREEXECUTE) <> 0 then
    begin
      This.ClearBeforeExecute := BoldGetNamedValue(NamedValues, 'ClearBeforeExecute');
    end;
    if (DataFlags and DF_SQLORDERBYCLAUSE) <> 0 then
    begin
      This.SQLOrderByClause := BoldGetNamedValue(NamedValues, 'SQLOrderByClause');
    end;
    if (DataFlags and DF_SQLWHERECLAUSE) <> 0 then
    begin
      This.SQLWhereClause := BoldGetNamedValue(NamedValues, 'SQLWhereClause');
    end;
  end;
end;

{-- TBoldComVariableHandleAdapter --------------------------------------------}

constructor TBoldComVariableHandleAdapter.Create(AdaptableObject: TBoldAdaptableObject;
  Owner: Boolean; const TypeLib: ITypeLib; const DispIntf: TGUID);
begin
  if Assigned(TypeLib) then
    inherited Create(AdaptableObject, Owner, TypeLib, DispIntf)
  else
    inherited Create(AdaptableObject, Owner, BoldComObjectSpaceTypeLibrary, IBoldElementHandle);
end;

function TBoldComVariableHandleAdapter.GetAsVariableHandle: TBoldVariableHandle;
begin
  Result := EnsuredAdaptee as TBoldVariableHandle;
end;

function TBoldComVariableHandleAdapter.GetData(DataFlags: Integer;
  out Value: IBoldElement; out DynamicBoldType: IBoldElementTypeInfo;
  out StaticBoldType: IBoldElementTypeInfo; out StaticSystemType: IBoldSystemTypeInfo;
  out BoldSystem: IBoldSystem; out StaticRootType: IBoldElementTypeInfo;
  out CurrentBoldObject: IBoldObject;
  out BoldList: IBoldList; out ListElementType: IBoldElementTypeInfo;
  out NamedValues: OleVariant): WordBool;
var
  This: TBoldVariableHandle;
begin
  Result := True;
  This := AsVariableHandle;
  if dataflags = DF_HANDLEID then
    NamedValues := BoldCreateNamedValues(['HandleId'], [integer(this)])
  else
  begin
    BoldComCreateAdapter(This.Value, False, IBoldElement, Value);
    BoldComCreateAdapter(This.DynamicBoldType, False, IBoldElementTypeInfo, DynamicBoldType);
    BoldComCreateAdapter(This.StaticBoldType, False, IBoldElementTypeInfo, StaticBoldType);
    BoldComCreateAdapter(This.StaticSystemTypeInfo, False, IBoldSystemTypeInfo, StaticSystemType);
    BoldSystem := nil;
    StaticRootType := nil;
    CurrentBoldObject := nil;
    BoldList := nil;
    ListElementType := nil;
    NamedValues := BoldCreateNamedValues(
      ['HandleId',
      'StaticSystemHandle',
      'ValueTypeName',
      'InitialValues'],
      [Integer(This),
      Integer(This.StaticSystemHandle),
      This.ValueTypeName,
      BoldStringsToVariant(This.InitialValues)]);
    end;
end;

function TBoldComVariableHandleAdapter.SetData(DataFlags: Integer;
  const Value: IBoldElement; NamedValues: OleVariant): WordBool;
var
  This: TBoldVariableHandle;
  HandleId: Integer;
  Temp: TStringList;
begin
  This := AsVariableHandle;
  if (DataFlags and DF_STATICSYSTEMHANDLE) <> 0 then
  begin
    HandleId := BoldGetNamedValue(NamedValues, 'StaticSystemHandle');
    if HandleId = 0 then
      This.StaticSystemHandle := nil
    else
      This.StaticSystemHandle := TBoldAbstractSystemHandle(HandleId);
  end;
  if (DataFlags and DF_VALUETYPENAME) <> 0 then
  begin
    This.ValueTypeName := BoldGetNamedValue(NamedValues, 'ValueTypeName');
  end;
  if (DataFlags and DF_INITIALVALUES) <> 0 then
  begin
    Temp := TStringList.Create;
    try
      BoldVariantToStrings(BoldGetNamedValue(NamedValues, 'InitialValues'),Temp);
      This.InitialValues.Assign(Temp);
    finally
      Temp.Free;
    end;
  end;
end;

{------------------------------------------------------------------------------}

function TBoldComElementAdapter.MultiEvaluateExpressionAsStringList(Elements: OleVariant;
                                                  const Expression: WideString;
                                                  const ClientId: WideString;
                                                  SubscriberIds: OleVariant;
                                                  Representation: Integer): OleVariant;
var
  I: integer;
  Strings: TStringList;
  Element: TBoldElement;
  E: TBoldIndirectElement;
  S: string;
  unk: IUnknown;
  SubscriberId: integer;
  Subscriber: TBoldComServerSubscriber;
begin
  Result := Null;
  Strings := TStringList.Create;
  E := TBoldIndirectElement.Create;
  try
    if VarIsArray(Elements) and (VarArrayDimCount(Elements) = 1) then
    begin
      for I := VarArrayLowBound(Elements, 1) to VarArrayHighBound(Elements, 1) do
      begin
        unk := Elements[i];
        SubscriberId := SubscriberIds[i];
        Subscriber := TBoldComServerSubscriber.GetSubscriber(ClientId, SubscriberId);

        Element := BoldComInterfaceToObject(unk) as TBoldElement;
        Element.EvaluateAndSubscribeToExpression(Expression, Subscriber, E);
        if Assigned(E.Value) then
        begin
          S := E.Value.StringRepresentation[Representation];
          if assigned(Subscriber) then
            e.value.SubscribeToStringRepresentation(representation, subscriber);
        end
        else
          S := '';
        Strings.Add(S);
      end;
    end;
     Result := BoldStringsToVariant(Strings);
   finally
     Strings.Free;
     E.Free;
   end;
end;


function TBoldComListAdapter.GetRange(FromIndex, ToIndex: Integer): OleVariant;
var
  ElementInterface: IUnknown;
  RangeIndex: integer;
begin
  if FromIndex < 0 then
    FromIndex := 0;
  if ToIndex >= AsBoldList.Count then
    ToIndex := AsBoldList.Count-1;
  AsBoldList.EnsureRange(FromIndex, ToIndex);
  Result := VarArrayCreate([FromIndex, ToIndex], varUnknown);
  for RangeIndex := FromIndex to ToIndex do
  begin
    BoldComCreateAdapter(AsBoldList.Elements[RangeIndex], False, IBoldElement, ElementInterface);
    Result[RangeIndex] := ElementInterface;
  end;
end;

function TBoldComObjectReferenceAdapter.QueryInterface(const IId: TGUID;
  out Obj): HResult;
var
  BoldObject: IBoldObject;
begin
  result := inherited QueryInterface(IId, Obj);
  if not (result = S_OK) then
  begin
    BoldObject := Get_BoldObject;
    if Assigned(BoldObject) then
      result := BoldObject.QueryInterface(IId, obj);
  end;
end;


initialization
  BoldComRegisterAdapter(TBoldComElementAdapter,TBoldElement);
  BoldComRegisterAdapter(TBoldComMetaElementAdapter,TBoldMetaElement);
  BoldComRegisterAdapter(TBoldComElementTypeInfoAdapter,TBoldElementTypeInfo);
  BoldComRegisterAdapter(TBoldComTypeTypeInfoAdapter,TBoldTypeTypeInfo);
  BoldComRegisterAdapter(TBoldComClassTypeInfoAdapter,TBoldClassTypeInfo);
  BoldComRegisterAdapter(TBoldComNilTypeInfoAdapter,TBoldNilTypeInfo);
  BoldComRegisterAdapter(TBoldComListTypeInfoAdapter,TBoldListTypeInfo);
  BoldComRegisterAdapter(TBoldComAttributeTypeInfoAdapter,TBoldAttributeTypeInfo);
  BoldComRegisterAdapter(TBoldComSystemTypeInfoAdapter,TBoldSystemTypeInfo);
  BoldComRegisterAdapter(TBoldComDomainElementAdapter,TBoldDomainElement);
  BoldComRegisterAdapter(TBoldComObjectAdapter,TBoldObject);
  BoldComRegisterAdapter(TBoldComMemberAdapter,TBoldMember);
  BoldComRegisterAdapter(TBoldComAttributeAdapter,TBoldAttribute);
  BoldComRegisterAdapter(TBoldComObjectReferenceAdapter,TBoldObjectReference);
  BoldComRegisterAdapter(TBoldComListAdapter,TBoldList);
  BoldComRegisterAdapter(TBoldComObjectListAdapter,TBoldObjectList);
  BoldComRegisterAdapter(TBoldComMemberListAdapter,TBoldMemberList);
  BoldComRegisterAdapter(TBoldComSystemAdapter,TBoldSystem);
  BoldComRegisterAdapter(TBoldComBlobAdapter,TBABlob);
  BoldComRegisterAdapter(TBoldComElementHandleAdapter,TBoldElementHandle);
  BoldComRegisterAdapter(TBoldComSystemHandleAdapter,TBoldSystemHandle);
  BoldComRegisterAdapter(TBoldComDerivedHandleAdapter,TBoldDerivedHandle);
  BoldComRegisterAdapter(TBoldComExpressionHandleAdapter,TBoldExpressionHandle);
  BoldComRegisterAdapter(TBoldComCursorHandleAdapter,TBoldCursorHandle);
  BoldComRegisterAdapter(TBoldComListHandleAdapter,TBoldListHandle);
  BoldComRegisterAdapter(TBoldComReferenceHandleAdapter,TBoldReferenceHandle);
  BoldComRegisterAdapter(TBoldComSQLHandleAdapter,TBoldSQLHandle);
  BoldComRegisterAdapter(TBoldComVariableHandleAdapter,TBoldVariableHandle);

end.
