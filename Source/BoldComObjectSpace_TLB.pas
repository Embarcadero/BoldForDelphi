
{ Global compiler directives }
{$include bold.inc}
unit BoldComObjectSpace_TLB;




















{$TYPEDADDRESS OFF}

interface

uses Windows, ActiveX, Classes, Graphics, OleCtrls, StdVCL;






const
  BoldComObjectSpaceMajorVersion = 1;
  BoldComObjectSpaceMinorVersion = 0;

  LIBID_BoldComObjectSpace: TGUID = '{D28A8F60-C8DD-11D3-89A9-444553540000}';

  IID_IBoldElement: TGUID = '{E8E2859A-0BAA-4DD9-8B26-A0AE5DEC87DF}';
  IID_IBoldMetaElement: TGUID = '{78994402-CA6E-11D3-89A9-444553540000}';
  IID_IBoldElementTypeInfo: TGUID = '{78994404-CA6E-11D3-89A9-444553540000}';
  IID_IBoldTypeTypeInfo: TGUID = '{68A9FC6D-D646-11D3-89A9-444553540000}';
  IID_IBoldClassTypeInfo: TGUID = '{969E6924-D4A6-11D3-89A9-444553540000}';
  IID_IBoldNilTypeInfo: TGUID = '{969E6926-D4A6-11D3-89A9-444553540000}';
  IID_IBoldListTypeInfo: TGUID = '{969E6928-D4A6-11D3-89A9-444553540000}';
  IID_IBoldAttributeTypeInfo: TGUID = '{969E692A-D4A6-11D3-89A9-444553540000}';
  IID_IBoldSystemTypeInfo: TGUID = '{969E6922-D4A6-11D3-89A9-444553540000}';
  IID_IBoldDomainElement: TGUID = '{78994406-CA6E-11D3-89A9-444553540000}';
  IID_IBoldObject: TGUID = '{7C5C21FF-D247-11D3-89A9-444553540000}';
  IID_IBoldMember: TGUID = '{7C5C2201-D247-11D3-89A9-444553540000}';
  IID_IBoldAttribute: TGUID = '{7C5C25A5-D247-11D3-89A9-444553540000}';
  IID_IBoldObjectReference: TGUID = '{7C5C2608-D247-11D3-89A9-444553540000}';
  IID_IBoldListCore: TGUID = '{4153813B-4DE9-4A17-B747-7091A839BBFA}';
  IID_IBoldList: TGUID = '{798895F6-E991-432B-9F37-7998BA769199}';
  IID_IBoldObjectList: TGUID = '{75A31152-D30E-11D3-89A9-444553540000}';
  IID_IBoldMemberList: TGUID = '{75A31154-D30E-11D3-89A9-444553540000}';
  IID_IBoldSystem: TGUID = '{8A530C40-D017-11D3-89A9-444553540000}';
  IID_IBoldBlob: TGUID = '{68A9F8C2-D646-11D3-89A9-444553540000}';
  IID_IBoldElementHandle: TGUID = '{71446D80-01C9-4E3C-95A7-D74445C0776C}';
type


  IBoldElement = interface;
  IBoldElementDisp = dispinterface;
  IBoldMetaElement = interface;
  IBoldMetaElementDisp = dispinterface;
  IBoldElementTypeInfo = interface;
  IBoldElementTypeInfoDisp = dispinterface;
  IBoldTypeTypeInfo = interface;
  IBoldTypeTypeInfoDisp = dispinterface;
  IBoldClassTypeInfo = interface;
  IBoldClassTypeInfoDisp = dispinterface;
  IBoldNilTypeInfo = interface;
  IBoldNilTypeInfoDisp = dispinterface;
  IBoldListTypeInfo = interface;
  IBoldListTypeInfoDisp = dispinterface;
  IBoldAttributeTypeInfo = interface;
  IBoldAttributeTypeInfoDisp = dispinterface;
  IBoldSystemTypeInfo = interface;
  IBoldSystemTypeInfoDisp = dispinterface;
  IBoldDomainElement = interface;
  IBoldDomainElementDisp = dispinterface;
  IBoldObject = interface;
  IBoldObjectDisp = dispinterface;
  IBoldMember = interface;
  IBoldMemberDisp = dispinterface;
  IBoldAttribute = interface;
  IBoldAttributeDisp = dispinterface;
  IBoldObjectReference = interface;
  IBoldObjectReferenceDisp = dispinterface;
  IBoldListCore = interface;
  IBoldListCoreDisp = dispinterface;
  IBoldList = interface;
  IBoldListDisp = dispinterface;
  IBoldObjectList = interface;
  IBoldObjectListDisp = dispinterface;
  IBoldMemberList = interface;
  IBoldMemberListDisp = dispinterface;
  IBoldSystem = interface;
  IBoldSystemDisp = dispinterface;
  IBoldBlob = interface;
  IBoldBlobDisp = dispinterface;
  IBoldElementHandle = interface;
  IBoldElementHandleDisp = dispinterface;




  IBoldElement = interface(IDispatch)
    ['{E8E2859A-0BAA-4DD9-8B26-A0AE5DEC87DF}']
    procedure AddSmallSubscription(const ClientId: WideString; SubscriberId: Integer; 
                                   SmallEvents: Integer; RequestedEvent: Integer; 
                                   CancelOld: WordBool); safecall;
    procedure AddSubscription(const ClientId: WideString; SubscriberId: Integer; Event: Integer; 
                              RequestedEvent: Integer; CancelOld: WordBool); safecall;
    procedure AssignElement(const Element: IBoldElement); safecall;
    function CompareTo(const Element: IBoldElement): Integer; safecall;
    function CompareToAs(CompareType: Integer; const Element: IBoldElement): Integer; safecall;
    procedure DefaultSubscribe(const ClientId: WideString; SubscriberId: Integer; 
                               RequestedEvent: Integer; CancelOld: WordBool); safecall;
    function EvaluateAndSubscribeToExpression(const Expression: WideString; 
                                              const ClientId: WideString; SubscriberId: Integer; 
                                              Resubscribe: WordBool; CancelOld: WordBool): IBoldElement; safecall;
    function EvaluateExpression(const Expression: WideString): IBoldElement; safecall;
    function EvaluateExpressionAsSessionIdList(const Expression: WideString): OleVariant; safecall;
    function EvaluateExpressionAsSessionIdListAndStringList(const ListExpression: WideString; 
                                                            const ItemExpression: WideString; 
                                                            Representation: Integer): OleVariant; safecall;
    function EvaluateExpressionAsString(const Expression: WideString; Representation: Integer): WideString; safecall;
    function EvaluateExpressionAsStringList(const Expression: WideString; Representation: Integer): OleVariant; safecall;
    function EvaluateExpressionsAsStringLists(Expressions: OleVariant; Representation: Integer): OleVariant; safecall;
    function GetAsList: IBoldList; safecall;
    function IsEqual(const Element: IBoldElement): WordBool; safecall;
    function IsEqualAs(CompareType: Integer; const Element: IBoldElement): WordBool; safecall;
    procedure MakeImmutable; safecall;
    procedure SubscribeToExpression(const Expression: WideString; const ClientId: WideString; 
                                    SubscriberId: Integer; Resubscribe: WordBool; 
                                    CancelOld: WordBool); safecall;
    procedure SubscribeToStringRepresentation(Representation: Integer; const ClientId: WideString; 
                                              SubscriberId: Integer; RequestedEvent: Integer; 
                                              CancelOld: WordBool); safecall;
    function ValidateCharacter(const Value: WideString; Representation: Integer): WordBool; safecall;
    function ValidateString(const Value: WideString; Representation: Integer): WordBool; safecall;
    function Get_AsString: WideString; safecall;
    procedure Set_AsString(const Value: WideString); safecall;
    function Get_AsVariant: OleVariant; safecall;
    procedure Set_AsVariant(Value: OleVariant); safecall;
    function Get_BoldType: IBoldElementTypeInfo; safecall;
    function Get_BoldTypeName: WideString; safecall;
    function Get_Mutable: WordBool; safecall;
    function Get_StringRepresentation(Representation: Integer): WideString; safecall;
    procedure Set_StringRepresentation(Representation: Integer; const Value: WideString); safecall;
    function Get__NewEnum: IUnknown; safecall;
    function MultiEvaluateExpressionAsStringList(Elements: OleVariant; 
                                                 const Expression: WideString; 
                                                 const ClientId: WideString; 
                                                 SubscriberIds: OleVariant; Representation: Integer): OleVariant; safecall;
    function Get_HasAdaptee: WordBool; safecall;
    property AsString: WideString read Get_AsString write Set_AsString;
    property AsVariant: OleVariant read Get_AsVariant write Set_AsVariant;
    property BoldType: IBoldElementTypeInfo read Get_BoldType;
    property BoldTypeName: WideString read Get_BoldTypeName;
    property Mutable: WordBool read Get_Mutable;
    property StringRepresentation[Representation: Integer]: WideString read Get_StringRepresentation write Set_StringRepresentation;
    property _NewEnum: IUnknown read Get__NewEnum;
    property HasAdaptee: WordBool read Get_HasAdaptee;
  end;




  IBoldElementDisp = dispinterface
    ['{E8E2859A-0BAA-4DD9-8B26-A0AE5DEC87DF}']
    procedure AddSmallSubscription(const ClientId: WideString; SubscriberId: Integer; 
                                   SmallEvents: Integer; RequestedEvent: Integer; 
                                   CancelOld: WordBool); dispid 101;
    procedure AddSubscription(const ClientId: WideString; SubscriberId: Integer; Event: Integer; 
                              RequestedEvent: Integer; CancelOld: WordBool); dispid 102;
    procedure AssignElement(const Element: IBoldElement); dispid 103;
    function CompareTo(const Element: IBoldElement): Integer; dispid 105;
    function CompareToAs(CompareType: Integer; const Element: IBoldElement): Integer; dispid 106;
    procedure DefaultSubscribe(const ClientId: WideString; SubscriberId: Integer; 
                               RequestedEvent: Integer; CancelOld: WordBool); dispid 107;
    function EvaluateAndSubscribeToExpression(const Expression: WideString; 
                                              const ClientId: WideString; SubscriberId: Integer; 
                                              Resubscribe: WordBool; CancelOld: WordBool): IBoldElement; dispid 108;
    function EvaluateExpression(const Expression: WideString): IBoldElement; dispid 109;
    function EvaluateExpressionAsSessionIdList(const Expression: WideString): OleVariant; dispid 110;
    function EvaluateExpressionAsSessionIdListAndStringList(const ListExpression: WideString; 
                                                            const ItemExpression: WideString; 
                                                            Representation: Integer): OleVariant; dispid 111;
    function EvaluateExpressionAsString(const Expression: WideString; Representation: Integer): WideString; dispid 112;
    function EvaluateExpressionAsStringList(const Expression: WideString; Representation: Integer): OleVariant; dispid 113;
    function EvaluateExpressionsAsStringLists(Expressions: OleVariant; Representation: Integer): OleVariant; dispid 114;
    function GetAsList: IBoldList; dispid 115;
    function IsEqual(const Element: IBoldElement): WordBool; dispid 116;
    function IsEqualAs(CompareType: Integer; const Element: IBoldElement): WordBool; dispid 117;
    procedure MakeImmutable; dispid 118;
    procedure SubscribeToExpression(const Expression: WideString; const ClientId: WideString; 
                                    SubscriberId: Integer; Resubscribe: WordBool; 
                                    CancelOld: WordBool); dispid 119;
    procedure SubscribeToStringRepresentation(Representation: Integer; const ClientId: WideString; 
                                              SubscriberId: Integer; RequestedEvent: Integer; 
                                              CancelOld: WordBool); dispid 120;
    function ValidateCharacter(const Value: WideString; Representation: Integer): WordBool; dispid 121;
    function ValidateString(const Value: WideString; Representation: Integer): WordBool; dispid 122;
    property AsString: WideString dispid 0;
    property AsVariant: OleVariant dispid 123;
    property BoldType: IBoldElementTypeInfo readonly dispid 124;
    property BoldTypeName: WideString readonly dispid 125;
    property Mutable: WordBool readonly dispid 126;
    property StringRepresentation[Representation: Integer]: WideString dispid 127;
    property _NewEnum: IUnknown readonly dispid -4;
    function MultiEvaluateExpressionAsStringList(Elements: OleVariant; 
                                                 const Expression: WideString; 
                                                 const ClientId: WideString; 
                                                 SubscriberIds: OleVariant; Representation: Integer): OleVariant; dispid 128;
    property HasAdaptee: WordBool readonly dispid 129;
  end;




  IBoldMetaElement = interface(IBoldElement)
    ['{78994402-CA6E-11D3-89A9-444553540000}']
    function Get_DelphiName: WideString; safecall;
    function Get_ExpressionName: WideString; safecall;
    function Get_ModelName: WideString; safecall;
    property DelphiName: WideString read Get_DelphiName;
    property ExpressionName: WideString read Get_ExpressionName;
    property ModelName: WideString read Get_ModelName;
  end;




  IBoldMetaElementDisp = dispinterface
    ['{78994402-CA6E-11D3-89A9-444553540000}']
    property DelphiName: WideString readonly dispid 201;
    property ExpressionName: WideString readonly dispid 202;
    property ModelName: WideString readonly dispid 203;
    procedure AddSmallSubscription(const ClientId: WideString; SubscriberId: Integer; 
                                   SmallEvents: Integer; RequestedEvent: Integer; 
                                   CancelOld: WordBool); dispid 101;
    procedure AddSubscription(const ClientId: WideString; SubscriberId: Integer; Event: Integer; 
                              RequestedEvent: Integer; CancelOld: WordBool); dispid 102;
    procedure AssignElement(const Element: IBoldElement); dispid 103;
    function CompareTo(const Element: IBoldElement): Integer; dispid 105;
    function CompareToAs(CompareType: Integer; const Element: IBoldElement): Integer; dispid 106;
    procedure DefaultSubscribe(const ClientId: WideString; SubscriberId: Integer; 
                               RequestedEvent: Integer; CancelOld: WordBool); dispid 107;
    function EvaluateAndSubscribeToExpression(const Expression: WideString; 
                                              const ClientId: WideString; SubscriberId: Integer; 
                                              Resubscribe: WordBool; CancelOld: WordBool): IBoldElement; dispid 108;
    function EvaluateExpression(const Expression: WideString): IBoldElement; dispid 109;
    function EvaluateExpressionAsSessionIdList(const Expression: WideString): OleVariant; dispid 110;
    function EvaluateExpressionAsSessionIdListAndStringList(const ListExpression: WideString; 
                                                            const ItemExpression: WideString; 
                                                            Representation: Integer): OleVariant; dispid 111;
    function EvaluateExpressionAsString(const Expression: WideString; Representation: Integer): WideString; dispid 112;
    function EvaluateExpressionAsStringList(const Expression: WideString; Representation: Integer): OleVariant; dispid 113;
    function EvaluateExpressionsAsStringLists(Expressions: OleVariant; Representation: Integer): OleVariant; dispid 114;
    function GetAsList: IBoldList; dispid 115;
    function IsEqual(const Element: IBoldElement): WordBool; dispid 116;
    function IsEqualAs(CompareType: Integer; const Element: IBoldElement): WordBool; dispid 117;
    procedure MakeImmutable; dispid 118;
    procedure SubscribeToExpression(const Expression: WideString; const ClientId: WideString; 
                                    SubscriberId: Integer; Resubscribe: WordBool; 
                                    CancelOld: WordBool); dispid 119;
    procedure SubscribeToStringRepresentation(Representation: Integer; const ClientId: WideString; 
                                              SubscriberId: Integer; RequestedEvent: Integer; 
                                              CancelOld: WordBool); dispid 120;
    function ValidateCharacter(const Value: WideString; Representation: Integer): WordBool; dispid 121;
    function ValidateString(const Value: WideString; Representation: Integer): WordBool; dispid 122;
    property AsString: WideString dispid 0;
    property AsVariant: OleVariant dispid 123;
    property BoldType: IBoldElementTypeInfo readonly dispid 124;
    property BoldTypeName: WideString readonly dispid 125;
    property Mutable: WordBool readonly dispid 126;
    property StringRepresentation[Representation: Integer]: WideString dispid 127;
    property _NewEnum: IUnknown readonly dispid -4;
    function MultiEvaluateExpressionAsStringList(Elements: OleVariant; 
                                                 const Expression: WideString; 
                                                 const ClientId: WideString; 
                                                 SubscriberIds: OleVariant; Representation: Integer): OleVariant; dispid 128;
    property HasAdaptee: WordBool readonly dispid 129;
  end;




  IBoldElementTypeInfo = interface(IBoldMetaElement)
    ['{78994404-CA6E-11D3-89A9-444553540000}']
    function ConformsTo(const ElementTypeInfo: IBoldElementTypeInfo): WordBool; safecall;
    function Get_BoldValueType: Integer; safecall;
    function Get_SystemTypeInfo: IBoldSystemTypeInfo; safecall;
    property BoldValueType: Integer read Get_BoldValueType;
    property SystemTypeInfo: IBoldSystemTypeInfo read Get_SystemTypeInfo;
  end;




  IBoldElementTypeInfoDisp = dispinterface
    ['{78994404-CA6E-11D3-89A9-444553540000}']
    function ConformsTo(const ElementTypeInfo: IBoldElementTypeInfo): WordBool; dispid 301;
    property BoldValueType: Integer readonly dispid 302;
    property SystemTypeInfo: IBoldSystemTypeInfo readonly dispid 303;
    property DelphiName: WideString readonly dispid 201;
    property ExpressionName: WideString readonly dispid 202;
    property ModelName: WideString readonly dispid 203;
    procedure AddSmallSubscription(const ClientId: WideString; SubscriberId: Integer; 
                                   SmallEvents: Integer; RequestedEvent: Integer; 
                                   CancelOld: WordBool); dispid 101;
    procedure AddSubscription(const ClientId: WideString; SubscriberId: Integer; Event: Integer; 
                              RequestedEvent: Integer; CancelOld: WordBool); dispid 102;
    procedure AssignElement(const Element: IBoldElement); dispid 103;
    function CompareTo(const Element: IBoldElement): Integer; dispid 105;
    function CompareToAs(CompareType: Integer; const Element: IBoldElement): Integer; dispid 106;
    procedure DefaultSubscribe(const ClientId: WideString; SubscriberId: Integer; 
                               RequestedEvent: Integer; CancelOld: WordBool); dispid 107;
    function EvaluateAndSubscribeToExpression(const Expression: WideString; 
                                              const ClientId: WideString; SubscriberId: Integer; 
                                              Resubscribe: WordBool; CancelOld: WordBool): IBoldElement; dispid 108;
    function EvaluateExpression(const Expression: WideString): IBoldElement; dispid 109;
    function EvaluateExpressionAsSessionIdList(const Expression: WideString): OleVariant; dispid 110;
    function EvaluateExpressionAsSessionIdListAndStringList(const ListExpression: WideString; 
                                                            const ItemExpression: WideString; 
                                                            Representation: Integer): OleVariant; dispid 111;
    function EvaluateExpressionAsString(const Expression: WideString; Representation: Integer): WideString; dispid 112;
    function EvaluateExpressionAsStringList(const Expression: WideString; Representation: Integer): OleVariant; dispid 113;
    function EvaluateExpressionsAsStringLists(Expressions: OleVariant; Representation: Integer): OleVariant; dispid 114;
    function GetAsList: IBoldList; dispid 115;
    function IsEqual(const Element: IBoldElement): WordBool; dispid 116;
    function IsEqualAs(CompareType: Integer; const Element: IBoldElement): WordBool; dispid 117;
    procedure MakeImmutable; dispid 118;
    procedure SubscribeToExpression(const Expression: WideString; const ClientId: WideString; 
                                    SubscriberId: Integer; Resubscribe: WordBool; 
                                    CancelOld: WordBool); dispid 119;
    procedure SubscribeToStringRepresentation(Representation: Integer; const ClientId: WideString; 
                                              SubscriberId: Integer; RequestedEvent: Integer; 
                                              CancelOld: WordBool); dispid 120;
    function ValidateCharacter(const Value: WideString; Representation: Integer): WordBool; dispid 121;
    function ValidateString(const Value: WideString; Representation: Integer): WordBool; dispid 122;
    property AsString: WideString dispid 0;
    property AsVariant: OleVariant dispid 123;
    property BoldType: IBoldElementTypeInfo readonly dispid 124;
    property BoldTypeName: WideString readonly dispid 125;
    property Mutable: WordBool readonly dispid 126;
    property StringRepresentation[Representation: Integer]: WideString dispid 127;
    property _NewEnum: IUnknown readonly dispid -4;
    function MultiEvaluateExpressionAsStringList(Elements: OleVariant; 
                                                 const Expression: WideString; 
                                                 const ClientId: WideString; 
                                                 SubscriberIds: OleVariant; Representation: Integer): OleVariant; dispid 128;
    property HasAdaptee: WordBool readonly dispid 129;
  end;




  IBoldTypeTypeInfo = interface(IBoldElementTypeInfo)
    ['{68A9FC6D-D646-11D3-89A9-444553540000}']
  end;




  IBoldTypeTypeInfoDisp = dispinterface
    ['{68A9FC6D-D646-11D3-89A9-444553540000}']
    function ConformsTo(const ElementTypeInfo: IBoldElementTypeInfo): WordBool; dispid 301;
    property BoldValueType: Integer readonly dispid 302;
    property SystemTypeInfo: IBoldSystemTypeInfo readonly dispid 303;
    property DelphiName: WideString readonly dispid 201;
    property ExpressionName: WideString readonly dispid 202;
    property ModelName: WideString readonly dispid 203;
    procedure AddSmallSubscription(const ClientId: WideString; SubscriberId: Integer; 
                                   SmallEvents: Integer; RequestedEvent: Integer; 
                                   CancelOld: WordBool); dispid 101;
    procedure AddSubscription(const ClientId: WideString; SubscriberId: Integer; Event: Integer; 
                              RequestedEvent: Integer; CancelOld: WordBool); dispid 102;
    procedure AssignElement(const Element: IBoldElement); dispid 103;
    function CompareTo(const Element: IBoldElement): Integer; dispid 105;
    function CompareToAs(CompareType: Integer; const Element: IBoldElement): Integer; dispid 106;
    procedure DefaultSubscribe(const ClientId: WideString; SubscriberId: Integer; 
                               RequestedEvent: Integer; CancelOld: WordBool); dispid 107;
    function EvaluateAndSubscribeToExpression(const Expression: WideString; 
                                              const ClientId: WideString; SubscriberId: Integer; 
                                              Resubscribe: WordBool; CancelOld: WordBool): IBoldElement; dispid 108;
    function EvaluateExpression(const Expression: WideString): IBoldElement; dispid 109;
    function EvaluateExpressionAsSessionIdList(const Expression: WideString): OleVariant; dispid 110;
    function EvaluateExpressionAsSessionIdListAndStringList(const ListExpression: WideString; 
                                                            const ItemExpression: WideString; 
                                                            Representation: Integer): OleVariant; dispid 111;
    function EvaluateExpressionAsString(const Expression: WideString; Representation: Integer): WideString; dispid 112;
    function EvaluateExpressionAsStringList(const Expression: WideString; Representation: Integer): OleVariant; dispid 113;
    function EvaluateExpressionsAsStringLists(Expressions: OleVariant; Representation: Integer): OleVariant; dispid 114;
    function GetAsList: IBoldList; dispid 115;
    function IsEqual(const Element: IBoldElement): WordBool; dispid 116;
    function IsEqualAs(CompareType: Integer; const Element: IBoldElement): WordBool; dispid 117;
    procedure MakeImmutable; dispid 118;
    procedure SubscribeToExpression(const Expression: WideString; const ClientId: WideString; 
                                    SubscriberId: Integer; Resubscribe: WordBool; 
                                    CancelOld: WordBool); dispid 119;
    procedure SubscribeToStringRepresentation(Representation: Integer; const ClientId: WideString; 
                                              SubscriberId: Integer; RequestedEvent: Integer; 
                                              CancelOld: WordBool); dispid 120;
    function ValidateCharacter(const Value: WideString; Representation: Integer): WordBool; dispid 121;
    function ValidateString(const Value: WideString; Representation: Integer): WordBool; dispid 122;
    property AsString: WideString dispid 0;
    property AsVariant: OleVariant dispid 123;
    property BoldType: IBoldElementTypeInfo readonly dispid 124;
    property BoldTypeName: WideString readonly dispid 125;
    property Mutable: WordBool readonly dispid 126;
    property StringRepresentation[Representation: Integer]: WideString dispid 127;
    property _NewEnum: IUnknown readonly dispid -4;
    function MultiEvaluateExpressionAsStringList(Elements: OleVariant; 
                                                 const Expression: WideString; 
                                                 const ClientId: WideString; 
                                                 SubscriberIds: OleVariant; Representation: Integer): OleVariant; dispid 128;
    property HasAdaptee: WordBool readonly dispid 129;
  end;




  IBoldClassTypeInfo = interface(IBoldElementTypeInfo)
    ['{969E6924-D4A6-11D3-89A9-444553540000}']
    function LeastCommonSuperClass(const ClassTypeInfo: IBoldClassTypeInfo): IBoldClassTypeInfo; safecall;
    function Get_Constraints: OleVariant; safecall;
    function Get_FirstOwnMemberIndex: Integer; safecall;
    function Get_HasSubClasses: WordBool; safecall;
    function Get_IsAbstract: WordBool; safecall;
    function Get_IsImported: WordBool; safecall;
    function Get_IsLinkClass: WordBool; safecall;
    function Get_IsPersistent: WordBool; safecall;
    function Get_ListTypeInfo: IBoldListTypeInfo; safecall;
    function Get_Stereotype: WideString; safecall;
    function Get_SuperClassTypeInfo: IBoldClassTypeInfo; safecall;
    function Get_TaggedValue(const Tag: WideString): WideString; safecall;
    function Get_TopSortedIndex: Integer; safecall;
    property Constraints: OleVariant read Get_Constraints;
    property FirstOwnMemberIndex: Integer read Get_FirstOwnMemberIndex;
    property HasSubClasses: WordBool read Get_HasSubClasses;
    property IsAbstract: WordBool read Get_IsAbstract;
    property IsImported: WordBool read Get_IsImported;
    property IsLinkClass: WordBool read Get_IsLinkClass;
    property IsPersistent: WordBool read Get_IsPersistent;
    property ListTypeInfo: IBoldListTypeInfo read Get_ListTypeInfo;
    property Stereotype: WideString read Get_Stereotype;
    property SuperClassTypeInfo: IBoldClassTypeInfo read Get_SuperClassTypeInfo;
    property TaggedValue[const Tag: WideString]: WideString read Get_TaggedValue;
    property TopSortedIndex: Integer read Get_TopSortedIndex;
  end;




  IBoldClassTypeInfoDisp = dispinterface
    ['{969E6924-D4A6-11D3-89A9-444553540000}']
    function LeastCommonSuperClass(const ClassTypeInfo: IBoldClassTypeInfo): IBoldClassTypeInfo; dispid 401;
    property Constraints: OleVariant readonly dispid 402;
    property FirstOwnMemberIndex: Integer readonly dispid 403;
    property HasSubClasses: WordBool readonly dispid 404;
    property IsAbstract: WordBool readonly dispid 405;
    property IsImported: WordBool readonly dispid 406;
    property IsLinkClass: WordBool readonly dispid 407;
    property IsPersistent: WordBool readonly dispid 408;
    property ListTypeInfo: IBoldListTypeInfo readonly dispid 409;
    property Stereotype: WideString readonly dispid 410;
    property SuperClassTypeInfo: IBoldClassTypeInfo readonly dispid 411;
    property TaggedValue[const Tag: WideString]: WideString readonly dispid 412;
    property TopSortedIndex: Integer readonly dispid 413;
    function ConformsTo(const ElementTypeInfo: IBoldElementTypeInfo): WordBool; dispid 301;
    property BoldValueType: Integer readonly dispid 302;
    property SystemTypeInfo: IBoldSystemTypeInfo readonly dispid 303;
    property DelphiName: WideString readonly dispid 201;
    property ExpressionName: WideString readonly dispid 202;
    property ModelName: WideString readonly dispid 203;
    procedure AddSmallSubscription(const ClientId: WideString; SubscriberId: Integer; 
                                   SmallEvents: Integer; RequestedEvent: Integer; 
                                   CancelOld: WordBool); dispid 101;
    procedure AddSubscription(const ClientId: WideString; SubscriberId: Integer; Event: Integer; 
                              RequestedEvent: Integer; CancelOld: WordBool); dispid 102;
    procedure AssignElement(const Element: IBoldElement); dispid 103;
    function CompareTo(const Element: IBoldElement): Integer; dispid 105;
    function CompareToAs(CompareType: Integer; const Element: IBoldElement): Integer; dispid 106;
    procedure DefaultSubscribe(const ClientId: WideString; SubscriberId: Integer; 
                               RequestedEvent: Integer; CancelOld: WordBool); dispid 107;
    function EvaluateAndSubscribeToExpression(const Expression: WideString; 
                                              const ClientId: WideString; SubscriberId: Integer; 
                                              Resubscribe: WordBool; CancelOld: WordBool): IBoldElement; dispid 108;
    function EvaluateExpression(const Expression: WideString): IBoldElement; dispid 109;
    function EvaluateExpressionAsSessionIdList(const Expression: WideString): OleVariant; dispid 110;
    function EvaluateExpressionAsSessionIdListAndStringList(const ListExpression: WideString; 
                                                            const ItemExpression: WideString; 
                                                            Representation: Integer): OleVariant; dispid 111;
    function EvaluateExpressionAsString(const Expression: WideString; Representation: Integer): WideString; dispid 112;
    function EvaluateExpressionAsStringList(const Expression: WideString; Representation: Integer): OleVariant; dispid 113;
    function EvaluateExpressionsAsStringLists(Expressions: OleVariant; Representation: Integer): OleVariant; dispid 114;
    function GetAsList: IBoldList; dispid 115;
    function IsEqual(const Element: IBoldElement): WordBool; dispid 116;
    function IsEqualAs(CompareType: Integer; const Element: IBoldElement): WordBool; dispid 117;
    procedure MakeImmutable; dispid 118;
    procedure SubscribeToExpression(const Expression: WideString; const ClientId: WideString; 
                                    SubscriberId: Integer; Resubscribe: WordBool; 
                                    CancelOld: WordBool); dispid 119;
    procedure SubscribeToStringRepresentation(Representation: Integer; const ClientId: WideString; 
                                              SubscriberId: Integer; RequestedEvent: Integer; 
                                              CancelOld: WordBool); dispid 120;
    function ValidateCharacter(const Value: WideString; Representation: Integer): WordBool; dispid 121;
    function ValidateString(const Value: WideString; Representation: Integer): WordBool; dispid 122;
    property AsString: WideString dispid 0;
    property AsVariant: OleVariant dispid 123;
    property BoldType: IBoldElementTypeInfo readonly dispid 124;
    property BoldTypeName: WideString readonly dispid 125;
    property Mutable: WordBool readonly dispid 126;
    property StringRepresentation[Representation: Integer]: WideString dispid 127;
    property _NewEnum: IUnknown readonly dispid -4;
    function MultiEvaluateExpressionAsStringList(Elements: OleVariant; 
                                                 const Expression: WideString; 
                                                 const ClientId: WideString; 
                                                 SubscriberIds: OleVariant; Representation: Integer): OleVariant; dispid 128;
    property HasAdaptee: WordBool readonly dispid 129;
  end;




  IBoldNilTypeInfo = interface(IBoldClassTypeInfo)
    ['{969E6926-D4A6-11D3-89A9-444553540000}']
  end;




  IBoldNilTypeInfoDisp = dispinterface
    ['{969E6926-D4A6-11D3-89A9-444553540000}']
    function LeastCommonSuperClass(const ClassTypeInfo: IBoldClassTypeInfo): IBoldClassTypeInfo; dispid 401;
    property Constraints: OleVariant readonly dispid 402;
    property FirstOwnMemberIndex: Integer readonly dispid 403;
    property HasSubClasses: WordBool readonly dispid 404;
    property IsAbstract: WordBool readonly dispid 405;
    property IsImported: WordBool readonly dispid 406;
    property IsLinkClass: WordBool readonly dispid 407;
    property IsPersistent: WordBool readonly dispid 408;
    property ListTypeInfo: IBoldListTypeInfo readonly dispid 409;
    property Stereotype: WideString readonly dispid 410;
    property SuperClassTypeInfo: IBoldClassTypeInfo readonly dispid 411;
    property TaggedValue[const Tag: WideString]: WideString readonly dispid 412;
    property TopSortedIndex: Integer readonly dispid 413;
    function ConformsTo(const ElementTypeInfo: IBoldElementTypeInfo): WordBool; dispid 301;
    property BoldValueType: Integer readonly dispid 302;
    property SystemTypeInfo: IBoldSystemTypeInfo readonly dispid 303;
    property DelphiName: WideString readonly dispid 201;
    property ExpressionName: WideString readonly dispid 202;
    property ModelName: WideString readonly dispid 203;
    procedure AddSmallSubscription(const ClientId: WideString; SubscriberId: Integer; 
                                   SmallEvents: Integer; RequestedEvent: Integer; 
                                   CancelOld: WordBool); dispid 101;
    procedure AddSubscription(const ClientId: WideString; SubscriberId: Integer; Event: Integer; 
                              RequestedEvent: Integer; CancelOld: WordBool); dispid 102;
    procedure AssignElement(const Element: IBoldElement); dispid 103;
    function CompareTo(const Element: IBoldElement): Integer; dispid 105;
    function CompareToAs(CompareType: Integer; const Element: IBoldElement): Integer; dispid 106;
    procedure DefaultSubscribe(const ClientId: WideString; SubscriberId: Integer; 
                               RequestedEvent: Integer; CancelOld: WordBool); dispid 107;
    function EvaluateAndSubscribeToExpression(const Expression: WideString; 
                                              const ClientId: WideString; SubscriberId: Integer; 
                                              Resubscribe: WordBool; CancelOld: WordBool): IBoldElement; dispid 108;
    function EvaluateExpression(const Expression: WideString): IBoldElement; dispid 109;
    function EvaluateExpressionAsSessionIdList(const Expression: WideString): OleVariant; dispid 110;
    function EvaluateExpressionAsSessionIdListAndStringList(const ListExpression: WideString; 
                                                            const ItemExpression: WideString; 
                                                            Representation: Integer): OleVariant; dispid 111;
    function EvaluateExpressionAsString(const Expression: WideString; Representation: Integer): WideString; dispid 112;
    function EvaluateExpressionAsStringList(const Expression: WideString; Representation: Integer): OleVariant; dispid 113;
    function EvaluateExpressionsAsStringLists(Expressions: OleVariant; Representation: Integer): OleVariant; dispid 114;
    function GetAsList: IBoldList; dispid 115;
    function IsEqual(const Element: IBoldElement): WordBool; dispid 116;
    function IsEqualAs(CompareType: Integer; const Element: IBoldElement): WordBool; dispid 117;
    procedure MakeImmutable; dispid 118;
    procedure SubscribeToExpression(const Expression: WideString; const ClientId: WideString; 
                                    SubscriberId: Integer; Resubscribe: WordBool; 
                                    CancelOld: WordBool); dispid 119;
    procedure SubscribeToStringRepresentation(Representation: Integer; const ClientId: WideString; 
                                              SubscriberId: Integer; RequestedEvent: Integer; 
                                              CancelOld: WordBool); dispid 120;
    function ValidateCharacter(const Value: WideString; Representation: Integer): WordBool; dispid 121;
    function ValidateString(const Value: WideString; Representation: Integer): WordBool; dispid 122;
    property AsString: WideString dispid 0;
    property AsVariant: OleVariant dispid 123;
    property BoldType: IBoldElementTypeInfo readonly dispid 124;
    property BoldTypeName: WideString readonly dispid 125;
    property Mutable: WordBool readonly dispid 126;
    property StringRepresentation[Representation: Integer]: WideString dispid 127;
    property _NewEnum: IUnknown readonly dispid -4;
    function MultiEvaluateExpressionAsStringList(Elements: OleVariant; 
                                                 const Expression: WideString; 
                                                 const ClientId: WideString; 
                                                 SubscriberIds: OleVariant; Representation: Integer): OleVariant; dispid 128;
    property HasAdaptee: WordBool readonly dispid 129;
  end;




  IBoldListTypeInfo = interface(IBoldElementTypeInfo)
    ['{969E6928-D4A6-11D3-89A9-444553540000}']
    function Get_ListElementTypeInfo: IBoldElementTypeInfo; safecall;
    property ListElementTypeInfo: IBoldElementTypeInfo read Get_ListElementTypeInfo;
  end;




  IBoldListTypeInfoDisp = dispinterface
    ['{969E6928-D4A6-11D3-89A9-444553540000}']
    property ListElementTypeInfo: IBoldElementTypeInfo readonly dispid 401;
    function ConformsTo(const ElementTypeInfo: IBoldElementTypeInfo): WordBool; dispid 301;
    property BoldValueType: Integer readonly dispid 302;
    property SystemTypeInfo: IBoldSystemTypeInfo readonly dispid 303;
    property DelphiName: WideString readonly dispid 201;
    property ExpressionName: WideString readonly dispid 202;
    property ModelName: WideString readonly dispid 203;
    procedure AddSmallSubscription(const ClientId: WideString; SubscriberId: Integer; 
                                   SmallEvents: Integer; RequestedEvent: Integer; 
                                   CancelOld: WordBool); dispid 101;
    procedure AddSubscription(const ClientId: WideString; SubscriberId: Integer; Event: Integer; 
                              RequestedEvent: Integer; CancelOld: WordBool); dispid 102;
    procedure AssignElement(const Element: IBoldElement); dispid 103;
    function CompareTo(const Element: IBoldElement): Integer; dispid 105;
    function CompareToAs(CompareType: Integer; const Element: IBoldElement): Integer; dispid 106;
    procedure DefaultSubscribe(const ClientId: WideString; SubscriberId: Integer; 
                               RequestedEvent: Integer; CancelOld: WordBool); dispid 107;
    function EvaluateAndSubscribeToExpression(const Expression: WideString; 
                                              const ClientId: WideString; SubscriberId: Integer; 
                                              Resubscribe: WordBool; CancelOld: WordBool): IBoldElement; dispid 108;
    function EvaluateExpression(const Expression: WideString): IBoldElement; dispid 109;
    function EvaluateExpressionAsSessionIdList(const Expression: WideString): OleVariant; dispid 110;
    function EvaluateExpressionAsSessionIdListAndStringList(const ListExpression: WideString; 
                                                            const ItemExpression: WideString; 
                                                            Representation: Integer): OleVariant; dispid 111;
    function EvaluateExpressionAsString(const Expression: WideString; Representation: Integer): WideString; dispid 112;
    function EvaluateExpressionAsStringList(const Expression: WideString; Representation: Integer): OleVariant; dispid 113;
    function EvaluateExpressionsAsStringLists(Expressions: OleVariant; Representation: Integer): OleVariant; dispid 114;
    function GetAsList: IBoldList; dispid 115;
    function IsEqual(const Element: IBoldElement): WordBool; dispid 116;
    function IsEqualAs(CompareType: Integer; const Element: IBoldElement): WordBool; dispid 117;
    procedure MakeImmutable; dispid 118;
    procedure SubscribeToExpression(const Expression: WideString; const ClientId: WideString; 
                                    SubscriberId: Integer; Resubscribe: WordBool; 
                                    CancelOld: WordBool); dispid 119;
    procedure SubscribeToStringRepresentation(Representation: Integer; const ClientId: WideString; 
                                              SubscriberId: Integer; RequestedEvent: Integer; 
                                              CancelOld: WordBool); dispid 120;
    function ValidateCharacter(const Value: WideString; Representation: Integer): WordBool; dispid 121;
    function ValidateString(const Value: WideString; Representation: Integer): WordBool; dispid 122;
    property AsString: WideString dispid 0;
    property AsVariant: OleVariant dispid 123;
    property BoldType: IBoldElementTypeInfo readonly dispid 124;
    property BoldTypeName: WideString readonly dispid 125;
    property Mutable: WordBool readonly dispid 126;
    property StringRepresentation[Representation: Integer]: WideString dispid 127;
    property _NewEnum: IUnknown readonly dispid -4;
    function MultiEvaluateExpressionAsStringList(Elements: OleVariant; 
                                                 const Expression: WideString; 
                                                 const ClientId: WideString; 
                                                 SubscriberIds: OleVariant; Representation: Integer): OleVariant; dispid 128;
    property HasAdaptee: WordBool readonly dispid 129;
  end;




  IBoldAttributeTypeInfo = interface(IBoldElementTypeInfo)
    ['{969E692A-D4A6-11D3-89A9-444553540000}']
    function Get_SuperAttributeTypeInfo: IBoldAttributeTypeInfo; safecall;
    property SuperAttributeTypeInfo: IBoldAttributeTypeInfo read Get_SuperAttributeTypeInfo;
  end;




  IBoldAttributeTypeInfoDisp = dispinterface
    ['{969E692A-D4A6-11D3-89A9-444553540000}']
    property SuperAttributeTypeInfo: IBoldAttributeTypeInfo readonly dispid 401;
    function ConformsTo(const ElementTypeInfo: IBoldElementTypeInfo): WordBool; dispid 301;
    property BoldValueType: Integer readonly dispid 302;
    property SystemTypeInfo: IBoldSystemTypeInfo readonly dispid 303;
    property DelphiName: WideString readonly dispid 201;
    property ExpressionName: WideString readonly dispid 202;
    property ModelName: WideString readonly dispid 203;
    procedure AddSmallSubscription(const ClientId: WideString; SubscriberId: Integer; 
                                   SmallEvents: Integer; RequestedEvent: Integer; 
                                   CancelOld: WordBool); dispid 101;
    procedure AddSubscription(const ClientId: WideString; SubscriberId: Integer; Event: Integer; 
                              RequestedEvent: Integer; CancelOld: WordBool); dispid 102;
    procedure AssignElement(const Element: IBoldElement); dispid 103;
    function CompareTo(const Element: IBoldElement): Integer; dispid 105;
    function CompareToAs(CompareType: Integer; const Element: IBoldElement): Integer; dispid 106;
    procedure DefaultSubscribe(const ClientId: WideString; SubscriberId: Integer; 
                               RequestedEvent: Integer; CancelOld: WordBool); dispid 107;
    function EvaluateAndSubscribeToExpression(const Expression: WideString; 
                                              const ClientId: WideString; SubscriberId: Integer; 
                                              Resubscribe: WordBool; CancelOld: WordBool): IBoldElement; dispid 108;
    function EvaluateExpression(const Expression: WideString): IBoldElement; dispid 109;
    function EvaluateExpressionAsSessionIdList(const Expression: WideString): OleVariant; dispid 110;
    function EvaluateExpressionAsSessionIdListAndStringList(const ListExpression: WideString; 
                                                            const ItemExpression: WideString; 
                                                            Representation: Integer): OleVariant; dispid 111;
    function EvaluateExpressionAsString(const Expression: WideString; Representation: Integer): WideString; dispid 112;
    function EvaluateExpressionAsStringList(const Expression: WideString; Representation: Integer): OleVariant; dispid 113;
    function EvaluateExpressionsAsStringLists(Expressions: OleVariant; Representation: Integer): OleVariant; dispid 114;
    function GetAsList: IBoldList; dispid 115;
    function IsEqual(const Element: IBoldElement): WordBool; dispid 116;
    function IsEqualAs(CompareType: Integer; const Element: IBoldElement): WordBool; dispid 117;
    procedure MakeImmutable; dispid 118;
    procedure SubscribeToExpression(const Expression: WideString; const ClientId: WideString; 
                                    SubscriberId: Integer; Resubscribe: WordBool; 
                                    CancelOld: WordBool); dispid 119;
    procedure SubscribeToStringRepresentation(Representation: Integer; const ClientId: WideString; 
                                              SubscriberId: Integer; RequestedEvent: Integer; 
                                              CancelOld: WordBool); dispid 120;
    function ValidateCharacter(const Value: WideString; Representation: Integer): WordBool; dispid 121;
    function ValidateString(const Value: WideString; Representation: Integer): WordBool; dispid 122;
    property AsString: WideString dispid 0;
    property AsVariant: OleVariant dispid 123;
    property BoldType: IBoldElementTypeInfo readonly dispid 124;
    property BoldTypeName: WideString readonly dispid 125;
    property Mutable: WordBool readonly dispid 126;
    property StringRepresentation[Representation: Integer]: WideString dispid 127;
    property _NewEnum: IUnknown readonly dispid -4;
    function MultiEvaluateExpressionAsStringList(Elements: OleVariant; 
                                                 const Expression: WideString; 
                                                 const ClientId: WideString; 
                                                 SubscriberIds: OleVariant; Representation: Integer): OleVariant; dispid 128;
    property HasAdaptee: WordBool readonly dispid 129;
  end;




  IBoldSystemTypeInfo = interface(IBoldElementTypeInfo)
    ['{969E6922-D4A6-11D3-89A9-444553540000}']
    function Get_AttributeTypeInfoByExpressionName(const Name: WideString): IBoldAttributeTypeInfo; safecall;
    function Get_AttributeTypes: IUnknown; safecall;
    function Get_ClassTypeInfoByExpressionName(const Name: WideString): IBoldClassTypeInfo; safecall;
    function Get_ClassTypes: IUnknown; safecall;
    function Get_Constraints: OleVariant; safecall;
    function Get_ElementTypeInfoByExpressionName(const Name: WideString): IBoldElementTypeInfo; safecall;
    function Get_IsPersistent: WordBool; safecall;
    function Get_IsRunnable: WordBool; safecall;
    function Get_ListTypeInfoByElement(const ElementTypeInfo: IBoldElementTypeInfo): IBoldListTypeInfo; safecall;
    function Get_ListTypes: IUnknown; safecall;
    function Get_MethodsInstalled: WordBool; safecall;
    function Get_NilTypeInfo: IBoldNilTypeInfo; safecall;
    function Get_RootClassTypeInfo: IBoldClassTypeInfo; safecall;
    function Get_Stereotype: WideString; safecall;
    function Get_TaggedValue(const Tag: WideString): WideString; safecall;
    function Get_TopSortedClasses: IUnknown; safecall;
    function Get_TypeTypeInfo: IBoldTypeTypeInfo; safecall;
    function Get_UseGeneratedCode: WordBool; safecall;
    function Get_ValueTypeNameList: IUnknown; safecall;
    property AttributeTypeInfoByExpressionName[const Name: WideString]: IBoldAttributeTypeInfo read Get_AttributeTypeInfoByExpressionName;
    property AttributeTypes: IUnknown read Get_AttributeTypes;
    property ClassTypeInfoByExpressionName[const Name: WideString]: IBoldClassTypeInfo read Get_ClassTypeInfoByExpressionName;
    property ClassTypes: IUnknown read Get_ClassTypes;
    property Constraints: OleVariant read Get_Constraints;
    property ElementTypeInfoByExpressionName[const Name: WideString]: IBoldElementTypeInfo read Get_ElementTypeInfoByExpressionName;
    property IsPersistent: WordBool read Get_IsPersistent;
    property IsRunnable: WordBool read Get_IsRunnable;
    property ListTypeInfoByElement[const ElementTypeInfo: IBoldElementTypeInfo]: IBoldListTypeInfo read Get_ListTypeInfoByElement;
    property ListTypes: IUnknown read Get_ListTypes;
    property MethodsInstalled: WordBool read Get_MethodsInstalled;
    property NilTypeInfo: IBoldNilTypeInfo read Get_NilTypeInfo;
    property RootClassTypeInfo: IBoldClassTypeInfo read Get_RootClassTypeInfo;
    property Stereotype: WideString read Get_Stereotype;
    property TaggedValue[const Tag: WideString]: WideString read Get_TaggedValue;
    property TopSortedClasses: IUnknown read Get_TopSortedClasses;
    property TypeTypeInfo: IBoldTypeTypeInfo read Get_TypeTypeInfo;
    property UseGeneratedCode: WordBool read Get_UseGeneratedCode;
    property ValueTypeNameList: IUnknown read Get_ValueTypeNameList;
  end;




  IBoldSystemTypeInfoDisp = dispinterface
    ['{969E6922-D4A6-11D3-89A9-444553540000}']
    property AttributeTypeInfoByExpressionName[const Name: WideString]: IBoldAttributeTypeInfo readonly dispid 401;
    property AttributeTypes: IUnknown readonly dispid 402;
    property ClassTypeInfoByExpressionName[const Name: WideString]: IBoldClassTypeInfo readonly dispid 403;
    property ClassTypes: IUnknown readonly dispid 404;
    property Constraints: OleVariant readonly dispid 405;
    property ElementTypeInfoByExpressionName[const Name: WideString]: IBoldElementTypeInfo readonly dispid 406;
    property IsPersistent: WordBool readonly dispid 407;
    property IsRunnable: WordBool readonly dispid 408;
    property ListTypeInfoByElement[const ElementTypeInfo: IBoldElementTypeInfo]: IBoldListTypeInfo readonly dispid 409;
    property ListTypes: IUnknown readonly dispid 410;
    property MethodsInstalled: WordBool readonly dispid 411;
    property NilTypeInfo: IBoldNilTypeInfo readonly dispid 412;
    property RootClassTypeInfo: IBoldClassTypeInfo readonly dispid 413;
    property Stereotype: WideString readonly dispid 414;
    property TaggedValue[const Tag: WideString]: WideString readonly dispid 415;
    property TopSortedClasses: IUnknown readonly dispid 416;
    property TypeTypeInfo: IBoldTypeTypeInfo readonly dispid 417;
    property UseGeneratedCode: WordBool readonly dispid 418;
    property ValueTypeNameList: IUnknown readonly dispid 419;
    function ConformsTo(const ElementTypeInfo: IBoldElementTypeInfo): WordBool; dispid 301;
    property BoldValueType: Integer readonly dispid 302;
    property SystemTypeInfo: IBoldSystemTypeInfo readonly dispid 303;
    property DelphiName: WideString readonly dispid 201;
    property ExpressionName: WideString readonly dispid 202;
    property ModelName: WideString readonly dispid 203;
    procedure AddSmallSubscription(const ClientId: WideString; SubscriberId: Integer; 
                                   SmallEvents: Integer; RequestedEvent: Integer; 
                                   CancelOld: WordBool); dispid 101;
    procedure AddSubscription(const ClientId: WideString; SubscriberId: Integer; Event: Integer; 
                              RequestedEvent: Integer; CancelOld: WordBool); dispid 102;
    procedure AssignElement(const Element: IBoldElement); dispid 103;
    function CompareTo(const Element: IBoldElement): Integer; dispid 105;
    function CompareToAs(CompareType: Integer; const Element: IBoldElement): Integer; dispid 106;
    procedure DefaultSubscribe(const ClientId: WideString; SubscriberId: Integer; 
                               RequestedEvent: Integer; CancelOld: WordBool); dispid 107;
    function EvaluateAndSubscribeToExpression(const Expression: WideString; 
                                              const ClientId: WideString; SubscriberId: Integer; 
                                              Resubscribe: WordBool; CancelOld: WordBool): IBoldElement; dispid 108;
    function EvaluateExpression(const Expression: WideString): IBoldElement; dispid 109;
    function EvaluateExpressionAsSessionIdList(const Expression: WideString): OleVariant; dispid 110;
    function EvaluateExpressionAsSessionIdListAndStringList(const ListExpression: WideString; 
                                                            const ItemExpression: WideString; 
                                                            Representation: Integer): OleVariant; dispid 111;
    function EvaluateExpressionAsString(const Expression: WideString; Representation: Integer): WideString; dispid 112;
    function EvaluateExpressionAsStringList(const Expression: WideString; Representation: Integer): OleVariant; dispid 113;
    function EvaluateExpressionsAsStringLists(Expressions: OleVariant; Representation: Integer): OleVariant; dispid 114;
    function GetAsList: IBoldList; dispid 115;
    function IsEqual(const Element: IBoldElement): WordBool; dispid 116;
    function IsEqualAs(CompareType: Integer; const Element: IBoldElement): WordBool; dispid 117;
    procedure MakeImmutable; dispid 118;
    procedure SubscribeToExpression(const Expression: WideString; const ClientId: WideString; 
                                    SubscriberId: Integer; Resubscribe: WordBool; 
                                    CancelOld: WordBool); dispid 119;
    procedure SubscribeToStringRepresentation(Representation: Integer; const ClientId: WideString; 
                                              SubscriberId: Integer; RequestedEvent: Integer; 
                                              CancelOld: WordBool); dispid 120;
    function ValidateCharacter(const Value: WideString; Representation: Integer): WordBool; dispid 121;
    function ValidateString(const Value: WideString; Representation: Integer): WordBool; dispid 122;
    property AsString: WideString dispid 0;
    property AsVariant: OleVariant dispid 123;
    property BoldType: IBoldElementTypeInfo readonly dispid 124;
    property BoldTypeName: WideString readonly dispid 125;
    property Mutable: WordBool readonly dispid 126;
    property StringRepresentation[Representation: Integer]: WideString dispid 127;
    property _NewEnum: IUnknown readonly dispid -4;
    function MultiEvaluateExpressionAsStringList(Elements: OleVariant; 
                                                 const Expression: WideString; 
                                                 const ClientId: WideString; 
                                                 SubscriberIds: OleVariant; Representation: Integer): OleVariant; dispid 128;
    property HasAdaptee: WordBool readonly dispid 129;
  end;




  IBoldDomainElement = interface(IBoldElement)
    ['{78994406-CA6E-11D3-89A9-444553540000}']
    function Get_BoldDirty: WordBool; safecall;
    function Get_BoldPersistent: WordBool; safecall;
    function Get_DisplayName: WideString; safecall;
    function Get_OwningElement: IBoldDomainElement; safecall;
    property BoldDirty: WordBool read Get_BoldDirty;
    property BoldPersistent: WordBool read Get_BoldPersistent;
    property DisplayName: WideString read Get_DisplayName;
    property OwningElement: IBoldDomainElement read Get_OwningElement;
  end;




  IBoldDomainElementDisp = dispinterface
    ['{78994406-CA6E-11D3-89A9-444553540000}']
    property BoldDirty: WordBool readonly dispid 201;
    property BoldPersistent: WordBool readonly dispid 202;
    property DisplayName: WideString readonly dispid 203;
    property OwningElement: IBoldDomainElement readonly dispid 204;
    procedure AddSmallSubscription(const ClientId: WideString; SubscriberId: Integer; 
                                   SmallEvents: Integer; RequestedEvent: Integer; 
                                   CancelOld: WordBool); dispid 101;
    procedure AddSubscription(const ClientId: WideString; SubscriberId: Integer; Event: Integer; 
                              RequestedEvent: Integer; CancelOld: WordBool); dispid 102;
    procedure AssignElement(const Element: IBoldElement); dispid 103;
    function CompareTo(const Element: IBoldElement): Integer; dispid 105;
    function CompareToAs(CompareType: Integer; const Element: IBoldElement): Integer; dispid 106;
    procedure DefaultSubscribe(const ClientId: WideString; SubscriberId: Integer; 
                               RequestedEvent: Integer; CancelOld: WordBool); dispid 107;
    function EvaluateAndSubscribeToExpression(const Expression: WideString; 
                                              const ClientId: WideString; SubscriberId: Integer; 
                                              Resubscribe: WordBool; CancelOld: WordBool): IBoldElement; dispid 108;
    function EvaluateExpression(const Expression: WideString): IBoldElement; dispid 109;
    function EvaluateExpressionAsSessionIdList(const Expression: WideString): OleVariant; dispid 110;
    function EvaluateExpressionAsSessionIdListAndStringList(const ListExpression: WideString; 
                                                            const ItemExpression: WideString; 
                                                            Representation: Integer): OleVariant; dispid 111;
    function EvaluateExpressionAsString(const Expression: WideString; Representation: Integer): WideString; dispid 112;
    function EvaluateExpressionAsStringList(const Expression: WideString; Representation: Integer): OleVariant; dispid 113;
    function EvaluateExpressionsAsStringLists(Expressions: OleVariant; Representation: Integer): OleVariant; dispid 114;
    function GetAsList: IBoldList; dispid 115;
    function IsEqual(const Element: IBoldElement): WordBool; dispid 116;
    function IsEqualAs(CompareType: Integer; const Element: IBoldElement): WordBool; dispid 117;
    procedure MakeImmutable; dispid 118;
    procedure SubscribeToExpression(const Expression: WideString; const ClientId: WideString; 
                                    SubscriberId: Integer; Resubscribe: WordBool; 
                                    CancelOld: WordBool); dispid 119;
    procedure SubscribeToStringRepresentation(Representation: Integer; const ClientId: WideString; 
                                              SubscriberId: Integer; RequestedEvent: Integer; 
                                              CancelOld: WordBool); dispid 120;
    function ValidateCharacter(const Value: WideString; Representation: Integer): WordBool; dispid 121;
    function ValidateString(const Value: WideString; Representation: Integer): WordBool; dispid 122;
    property AsString: WideString dispid 0;
    property AsVariant: OleVariant dispid 123;
    property BoldType: IBoldElementTypeInfo readonly dispid 124;
    property BoldTypeName: WideString readonly dispid 125;
    property Mutable: WordBool readonly dispid 126;
    property StringRepresentation[Representation: Integer]: WideString dispid 127;
    property _NewEnum: IUnknown readonly dispid -4;
    function MultiEvaluateExpressionAsStringList(Elements: OleVariant; 
                                                 const Expression: WideString; 
                                                 const ClientId: WideString; 
                                                 SubscriberIds: OleVariant; Representation: Integer): OleVariant; dispid 128;
    property HasAdaptee: WordBool readonly dispid 129;
  end;




  IBoldObject = interface(IBoldDomainElement)
    ['{7C5C21FF-D247-11D3-89A9-444553540000}']
    procedure BoldMakePersistent; safecall;
    function CheckLinks(Index: Integer): WordBool; safecall;
    procedure Delete; safecall;
    procedure DiscardChanges; safecall;
    procedure Invalidate; safecall;
    procedure LinkObject(const RoleName: WideString; const BoldObject: IBoldObject); safecall;
    procedure UnlinkAll; safecall;
    procedure UnlinkObject(const RoleName: WideString; const BoldObject: IBoldObject); safecall;
    function Get_BoldClassTypeInfo: IBoldClassTypeInfo; safecall;
    function Get_BoldExistenceState: Integer; safecall;
    function Get_BoldMemberCount: Integer; safecall;
    function Get_BoldMember(Index: OleVariant): IBoldMember; safecall;
    function Get_BoldMemberValue(Index: OleVariant): OleVariant; safecall;
    procedure Set_BoldMemberValue(Index: OleVariant; Value: OleVariant); safecall;
    function Get_BoldMemberValues: OleVariant; safecall;
    procedure Set_BoldMemberValues(Value: OleVariant); safecall;
    function Get_BoldPersistenceState: Integer; safecall;
    function Get_BoldSystem: IBoldSystem; safecall;
    function Get_CanDelete: WordBool; safecall;
    function Get_CanUpdate: WordBool; safecall;
    function Get_SessionId: OleVariant; safecall;
    property BoldClassTypeInfo: IBoldClassTypeInfo read Get_BoldClassTypeInfo;
    property BoldExistenceState: Integer read Get_BoldExistenceState;
    property BoldMemberCount: Integer read Get_BoldMemberCount;
    property BoldMember[Index: OleVariant]: IBoldMember read Get_BoldMember;
    property BoldMemberValue[Index: OleVariant]: OleVariant read Get_BoldMemberValue write Set_BoldMemberValue;
    property BoldMemberValues: OleVariant read Get_BoldMemberValues write Set_BoldMemberValues;
    property BoldPersistenceState: Integer read Get_BoldPersistenceState;
    property BoldSystem: IBoldSystem read Get_BoldSystem;
    property CanDelete: WordBool read Get_CanDelete;
    property CanUpdate: WordBool read Get_CanUpdate;
    property SessionId: OleVariant read Get_SessionId;
  end;




  IBoldObjectDisp = dispinterface
    ['{7C5C21FF-D247-11D3-89A9-444553540000}']
    procedure BoldMakePersistent; dispid 301;
    function CheckLinks(Index: Integer): WordBool; dispid 302;
    procedure Delete; dispid 303;
    procedure DiscardChanges; dispid 304;
    procedure Invalidate; dispid 305;
    procedure LinkObject(const RoleName: WideString; const BoldObject: IBoldObject); dispid 306;
    procedure UnlinkAll; dispid 307;
    procedure UnlinkObject(const RoleName: WideString; const BoldObject: IBoldObject); dispid 308;
    property BoldClassTypeInfo: IBoldClassTypeInfo readonly dispid 309;
    property BoldExistenceState: Integer readonly dispid 310;
    property BoldMemberCount: Integer readonly dispid 311;
    property BoldMember[Index: OleVariant]: IBoldMember readonly dispid 312;
    property BoldMemberValue[Index: OleVariant]: OleVariant dispid 313;
    property BoldMemberValues: OleVariant dispid 314;
    property BoldPersistenceState: Integer readonly dispid 315;
    property BoldSystem: IBoldSystem readonly dispid 316;
    property CanDelete: WordBool readonly dispid 317;
    property CanUpdate: WordBool readonly dispid 318;
    property SessionId: OleVariant readonly dispid 319;
    property BoldDirty: WordBool readonly dispid 201;
    property BoldPersistent: WordBool readonly dispid 202;
    property DisplayName: WideString readonly dispid 203;
    property OwningElement: IBoldDomainElement readonly dispid 204;
    procedure AddSmallSubscription(const ClientId: WideString; SubscriberId: Integer; 
                                   SmallEvents: Integer; RequestedEvent: Integer; 
                                   CancelOld: WordBool); dispid 101;
    procedure AddSubscription(const ClientId: WideString; SubscriberId: Integer; Event: Integer; 
                              RequestedEvent: Integer; CancelOld: WordBool); dispid 102;
    procedure AssignElement(const Element: IBoldElement); dispid 103;
    function CompareTo(const Element: IBoldElement): Integer; dispid 105;
    function CompareToAs(CompareType: Integer; const Element: IBoldElement): Integer; dispid 106;
    procedure DefaultSubscribe(const ClientId: WideString; SubscriberId: Integer; 
                               RequestedEvent: Integer; CancelOld: WordBool); dispid 107;
    function EvaluateAndSubscribeToExpression(const Expression: WideString; 
                                              const ClientId: WideString; SubscriberId: Integer; 
                                              Resubscribe: WordBool; CancelOld: WordBool): IBoldElement; dispid 108;
    function EvaluateExpression(const Expression: WideString): IBoldElement; dispid 109;
    function EvaluateExpressionAsSessionIdList(const Expression: WideString): OleVariant; dispid 110;
    function EvaluateExpressionAsSessionIdListAndStringList(const ListExpression: WideString; 
                                                            const ItemExpression: WideString; 
                                                            Representation: Integer): OleVariant; dispid 111;
    function EvaluateExpressionAsString(const Expression: WideString; Representation: Integer): WideString; dispid 112;
    function EvaluateExpressionAsStringList(const Expression: WideString; Representation: Integer): OleVariant; dispid 113;
    function EvaluateExpressionsAsStringLists(Expressions: OleVariant; Representation: Integer): OleVariant; dispid 114;
    function GetAsList: IBoldList; dispid 115;
    function IsEqual(const Element: IBoldElement): WordBool; dispid 116;
    function IsEqualAs(CompareType: Integer; const Element: IBoldElement): WordBool; dispid 117;
    procedure MakeImmutable; dispid 118;
    procedure SubscribeToExpression(const Expression: WideString; const ClientId: WideString; 
                                    SubscriberId: Integer; Resubscribe: WordBool; 
                                    CancelOld: WordBool); dispid 119;
    procedure SubscribeToStringRepresentation(Representation: Integer; const ClientId: WideString; 
                                              SubscriberId: Integer; RequestedEvent: Integer; 
                                              CancelOld: WordBool); dispid 120;
    function ValidateCharacter(const Value: WideString; Representation: Integer): WordBool; dispid 121;
    function ValidateString(const Value: WideString; Representation: Integer): WordBool; dispid 122;
    property AsString: WideString dispid 0;
    property AsVariant: OleVariant dispid 123;
    property BoldType: IBoldElementTypeInfo readonly dispid 124;
    property BoldTypeName: WideString readonly dispid 125;
    property Mutable: WordBool readonly dispid 126;
    property StringRepresentation[Representation: Integer]: WideString dispid 127;
    property _NewEnum: IUnknown readonly dispid -4;
    function MultiEvaluateExpressionAsStringList(Elements: OleVariant; 
                                                 const Expression: WideString; 
                                                 const ClientId: WideString; 
                                                 SubscriberIds: OleVariant; Representation: Integer): OleVariant; dispid 128;
    property HasAdaptee: WordBool readonly dispid 129;
  end;




  IBoldMember = interface(IBoldDomainElement)
    ['{7C5C2201-D247-11D3-89A9-444553540000}']
    function Clone: IBoldMember; safecall;
    procedure DiscardChanges; safecall;
    procedure EnsureContentsCurrent; safecall;
    procedure Invalidate; safecall;
    function Get_BoldMemberRTInfo: IUnknown; safecall;
    function Get_BoldSystem: IBoldSystem; safecall;
    function Get_CanModify: WordBool; safecall;
    function Get_CanRead: WordBool; safecall;
    function Get_Derived: WordBool; safecall;
    function Get_IsPartOfSystem: WordBool; safecall;
    function Get_OwningObject: IBoldObject; safecall;
    property BoldMemberRTInfo: IUnknown read Get_BoldMemberRTInfo;
    property BoldSystem: IBoldSystem read Get_BoldSystem;
    property CanModify: WordBool read Get_CanModify;
    property CanRead: WordBool read Get_CanRead;
    property Derived: WordBool read Get_Derived;
    property IsPartOfSystem: WordBool read Get_IsPartOfSystem;
    property OwningObject: IBoldObject read Get_OwningObject;
  end;




  IBoldMemberDisp = dispinterface
    ['{7C5C2201-D247-11D3-89A9-444553540000}']
    function Clone: IBoldMember; dispid 301;
    procedure DiscardChanges; dispid 302;
    procedure EnsureContentsCurrent; dispid 303;
    procedure Invalidate; dispid 304;
    property BoldMemberRTInfo: IUnknown readonly dispid 305;
    property BoldSystem: IBoldSystem readonly dispid 306;
    property CanModify: WordBool readonly dispid 307;
    property CanRead: WordBool readonly dispid 308;
    property Derived: WordBool readonly dispid 309;
    property IsPartOfSystem: WordBool readonly dispid 310;
    property OwningObject: IBoldObject readonly dispid 311;
    property BoldDirty: WordBool readonly dispid 201;
    property BoldPersistent: WordBool readonly dispid 202;
    property DisplayName: WideString readonly dispid 203;
    property OwningElement: IBoldDomainElement readonly dispid 204;
    procedure AddSmallSubscription(const ClientId: WideString; SubscriberId: Integer; 
                                   SmallEvents: Integer; RequestedEvent: Integer; 
                                   CancelOld: WordBool); dispid 101;
    procedure AddSubscription(const ClientId: WideString; SubscriberId: Integer; Event: Integer; 
                              RequestedEvent: Integer; CancelOld: WordBool); dispid 102;
    procedure AssignElement(const Element: IBoldElement); dispid 103;
    function CompareTo(const Element: IBoldElement): Integer; dispid 105;
    function CompareToAs(CompareType: Integer; const Element: IBoldElement): Integer; dispid 106;
    procedure DefaultSubscribe(const ClientId: WideString; SubscriberId: Integer; 
                               RequestedEvent: Integer; CancelOld: WordBool); dispid 107;
    function EvaluateAndSubscribeToExpression(const Expression: WideString; 
                                              const ClientId: WideString; SubscriberId: Integer; 
                                              Resubscribe: WordBool; CancelOld: WordBool): IBoldElement; dispid 108;
    function EvaluateExpression(const Expression: WideString): IBoldElement; dispid 109;
    function EvaluateExpressionAsSessionIdList(const Expression: WideString): OleVariant; dispid 110;
    function EvaluateExpressionAsSessionIdListAndStringList(const ListExpression: WideString; 
                                                            const ItemExpression: WideString; 
                                                            Representation: Integer): OleVariant; dispid 111;
    function EvaluateExpressionAsString(const Expression: WideString; Representation: Integer): WideString; dispid 112;
    function EvaluateExpressionAsStringList(const Expression: WideString; Representation: Integer): OleVariant; dispid 113;
    function EvaluateExpressionsAsStringLists(Expressions: OleVariant; Representation: Integer): OleVariant; dispid 114;
    function GetAsList: IBoldList; dispid 115;
    function IsEqual(const Element: IBoldElement): WordBool; dispid 116;
    function IsEqualAs(CompareType: Integer; const Element: IBoldElement): WordBool; dispid 117;
    procedure MakeImmutable; dispid 118;
    procedure SubscribeToExpression(const Expression: WideString; const ClientId: WideString; 
                                    SubscriberId: Integer; Resubscribe: WordBool; 
                                    CancelOld: WordBool); dispid 119;
    procedure SubscribeToStringRepresentation(Representation: Integer; const ClientId: WideString; 
                                              SubscriberId: Integer; RequestedEvent: Integer; 
                                              CancelOld: WordBool); dispid 120;
    function ValidateCharacter(const Value: WideString; Representation: Integer): WordBool; dispid 121;
    function ValidateString(const Value: WideString; Representation: Integer): WordBool; dispid 122;
    property AsString: WideString dispid 0;
    property AsVariant: OleVariant dispid 123;
    property BoldType: IBoldElementTypeInfo readonly dispid 124;
    property BoldTypeName: WideString readonly dispid 125;
    property Mutable: WordBool readonly dispid 126;
    property StringRepresentation[Representation: Integer]: WideString dispid 127;
    property _NewEnum: IUnknown readonly dispid -4;
    function MultiEvaluateExpressionAsStringList(Elements: OleVariant; 
                                                 const Expression: WideString; 
                                                 const ClientId: WideString; 
                                                 SubscriberIds: OleVariant; Representation: Integer): OleVariant; dispid 128;
    property HasAdaptee: WordBool readonly dispid 129;
  end;




  IBoldAttribute = interface(IBoldMember)
    ['{7C5C25A5-D247-11D3-89A9-444553540000}']
    procedure SetToNull; safecall;
    function Get_BoldAttributeRTInfo: IUnknown; safecall;
    function Get_CanSetToNull: WordBool; safecall;
    function Get_IsNull: WordBool; safecall;
    property BoldAttributeRTInfo: IUnknown read Get_BoldAttributeRTInfo;
    property CanSetToNull: WordBool read Get_CanSetToNull;
    property IsNull: WordBool read Get_IsNull;
  end;




  IBoldAttributeDisp = dispinterface
    ['{7C5C25A5-D247-11D3-89A9-444553540000}']
    procedure SetToNull; dispid 401;
    property BoldAttributeRTInfo: IUnknown readonly dispid 402;
    property CanSetToNull: WordBool readonly dispid 403;
    property IsNull: WordBool readonly dispid 404;
    function Clone: IBoldMember; dispid 301;
    procedure DiscardChanges; dispid 302;
    procedure EnsureContentsCurrent; dispid 303;
    procedure Invalidate; dispid 304;
    property BoldMemberRTInfo: IUnknown readonly dispid 305;
    property BoldSystem: IBoldSystem readonly dispid 306;
    property CanModify: WordBool readonly dispid 307;
    property CanRead: WordBool readonly dispid 308;
    property Derived: WordBool readonly dispid 309;
    property IsPartOfSystem: WordBool readonly dispid 310;
    property OwningObject: IBoldObject readonly dispid 311;
    property BoldDirty: WordBool readonly dispid 201;
    property BoldPersistent: WordBool readonly dispid 202;
    property DisplayName: WideString readonly dispid 203;
    property OwningElement: IBoldDomainElement readonly dispid 204;
    procedure AddSmallSubscription(const ClientId: WideString; SubscriberId: Integer; 
                                   SmallEvents: Integer; RequestedEvent: Integer; 
                                   CancelOld: WordBool); dispid 101;
    procedure AddSubscription(const ClientId: WideString; SubscriberId: Integer; Event: Integer; 
                              RequestedEvent: Integer; CancelOld: WordBool); dispid 102;
    procedure AssignElement(const Element: IBoldElement); dispid 103;
    function CompareTo(const Element: IBoldElement): Integer; dispid 105;
    function CompareToAs(CompareType: Integer; const Element: IBoldElement): Integer; dispid 106;
    procedure DefaultSubscribe(const ClientId: WideString; SubscriberId: Integer; 
                               RequestedEvent: Integer; CancelOld: WordBool); dispid 107;
    function EvaluateAndSubscribeToExpression(const Expression: WideString; 
                                              const ClientId: WideString; SubscriberId: Integer; 
                                              Resubscribe: WordBool; CancelOld: WordBool): IBoldElement; dispid 108;
    function EvaluateExpression(const Expression: WideString): IBoldElement; dispid 109;
    function EvaluateExpressionAsSessionIdList(const Expression: WideString): OleVariant; dispid 110;
    function EvaluateExpressionAsSessionIdListAndStringList(const ListExpression: WideString; 
                                                            const ItemExpression: WideString; 
                                                            Representation: Integer): OleVariant; dispid 111;
    function EvaluateExpressionAsString(const Expression: WideString; Representation: Integer): WideString; dispid 112;
    function EvaluateExpressionAsStringList(const Expression: WideString; Representation: Integer): OleVariant; dispid 113;
    function EvaluateExpressionsAsStringLists(Expressions: OleVariant; Representation: Integer): OleVariant; dispid 114;
    function GetAsList: IBoldList; dispid 115;
    function IsEqual(const Element: IBoldElement): WordBool; dispid 116;
    function IsEqualAs(CompareType: Integer; const Element: IBoldElement): WordBool; dispid 117;
    procedure MakeImmutable; dispid 118;
    procedure SubscribeToExpression(const Expression: WideString; const ClientId: WideString; 
                                    SubscriberId: Integer; Resubscribe: WordBool; 
                                    CancelOld: WordBool); dispid 119;
    procedure SubscribeToStringRepresentation(Representation: Integer; const ClientId: WideString; 
                                              SubscriberId: Integer; RequestedEvent: Integer; 
                                              CancelOld: WordBool); dispid 120;
    function ValidateCharacter(const Value: WideString; Representation: Integer): WordBool; dispid 121;
    function ValidateString(const Value: WideString; Representation: Integer): WordBool; dispid 122;
    property AsString: WideString dispid 0;
    property AsVariant: OleVariant dispid 123;
    property BoldType: IBoldElementTypeInfo readonly dispid 124;
    property BoldTypeName: WideString readonly dispid 125;
    property Mutable: WordBool readonly dispid 126;
    property StringRepresentation[Representation: Integer]: WideString dispid 127;
    property _NewEnum: IUnknown readonly dispid -4;
    function MultiEvaluateExpressionAsStringList(Elements: OleVariant; 
                                                 const Expression: WideString; 
                                                 const ClientId: WideString; 
                                                 SubscriberIds: OleVariant; Representation: Integer): OleVariant; dispid 128;
    property HasAdaptee: WordBool readonly dispid 129;
  end;




  IBoldObjectReference = interface(IBoldMember)
    ['{7C5C2608-D247-11D3-89A9-444553540000}']
    function CanSet(const NewObject: IBoldObject): WordBool; safecall;
    function Get_BoldObject: IBoldObject; safecall;
    procedure Set_BoldObject(const Value: IBoldObject); safecall;
    function Get_BoldRoleRTInfo: IUnknown; safecall;
    property BoldObject: IBoldObject read Get_BoldObject write Set_BoldObject;
    property BoldRoleRTInfo: IUnknown read Get_BoldRoleRTInfo;
  end;




  IBoldObjectReferenceDisp = dispinterface
    ['{7C5C2608-D247-11D3-89A9-444553540000}']
    function CanSet(const NewObject: IBoldObject): WordBool; dispid 401;
    property BoldObject: IBoldObject dispid 402;
    property BoldRoleRTInfo: IUnknown readonly dispid 403;
    function Clone: IBoldMember; dispid 301;
    procedure DiscardChanges; dispid 302;
    procedure EnsureContentsCurrent; dispid 303;
    procedure Invalidate; dispid 304;
    property BoldMemberRTInfo: IUnknown readonly dispid 305;
    property BoldSystem: IBoldSystem readonly dispid 306;
    property CanModify: WordBool readonly dispid 307;
    property CanRead: WordBool readonly dispid 308;
    property Derived: WordBool readonly dispid 309;
    property IsPartOfSystem: WordBool readonly dispid 310;
    property OwningObject: IBoldObject readonly dispid 311;
    property BoldDirty: WordBool readonly dispid 201;
    property BoldPersistent: WordBool readonly dispid 202;
    property DisplayName: WideString readonly dispid 203;
    property OwningElement: IBoldDomainElement readonly dispid 204;
    procedure AddSmallSubscription(const ClientId: WideString; SubscriberId: Integer; 
                                   SmallEvents: Integer; RequestedEvent: Integer; 
                                   CancelOld: WordBool); dispid 101;
    procedure AddSubscription(const ClientId: WideString; SubscriberId: Integer; Event: Integer; 
                              RequestedEvent: Integer; CancelOld: WordBool); dispid 102;
    procedure AssignElement(const Element: IBoldElement); dispid 103;
    function CompareTo(const Element: IBoldElement): Integer; dispid 105;
    function CompareToAs(CompareType: Integer; const Element: IBoldElement): Integer; dispid 106;
    procedure DefaultSubscribe(const ClientId: WideString; SubscriberId: Integer; 
                               RequestedEvent: Integer; CancelOld: WordBool); dispid 107;
    function EvaluateAndSubscribeToExpression(const Expression: WideString; 
                                              const ClientId: WideString; SubscriberId: Integer; 
                                              Resubscribe: WordBool; CancelOld: WordBool): IBoldElement; dispid 108;
    function EvaluateExpression(const Expression: WideString): IBoldElement; dispid 109;
    function EvaluateExpressionAsSessionIdList(const Expression: WideString): OleVariant; dispid 110;
    function EvaluateExpressionAsSessionIdListAndStringList(const ListExpression: WideString; 
                                                            const ItemExpression: WideString; 
                                                            Representation: Integer): OleVariant; dispid 111;
    function EvaluateExpressionAsString(const Expression: WideString; Representation: Integer): WideString; dispid 112;
    function EvaluateExpressionAsStringList(const Expression: WideString; Representation: Integer): OleVariant; dispid 113;
    function EvaluateExpressionsAsStringLists(Expressions: OleVariant; Representation: Integer): OleVariant; dispid 114;
    function GetAsList: IBoldList; dispid 115;
    function IsEqual(const Element: IBoldElement): WordBool; dispid 116;
    function IsEqualAs(CompareType: Integer; const Element: IBoldElement): WordBool; dispid 117;
    procedure MakeImmutable; dispid 118;
    procedure SubscribeToExpression(const Expression: WideString; const ClientId: WideString; 
                                    SubscriberId: Integer; Resubscribe: WordBool; 
                                    CancelOld: WordBool); dispid 119;
    procedure SubscribeToStringRepresentation(Representation: Integer; const ClientId: WideString; 
                                              SubscriberId: Integer; RequestedEvent: Integer; 
                                              CancelOld: WordBool); dispid 120;
    function ValidateCharacter(const Value: WideString; Representation: Integer): WordBool; dispid 121;
    function ValidateString(const Value: WideString; Representation: Integer): WordBool; dispid 122;
    property AsString: WideString dispid 0;
    property AsVariant: OleVariant dispid 123;
    property BoldType: IBoldElementTypeInfo readonly dispid 124;
    property BoldTypeName: WideString readonly dispid 125;
    property Mutable: WordBool readonly dispid 126;
    property StringRepresentation[Representation: Integer]: WideString dispid 127;
    property _NewEnum: IUnknown readonly dispid -4;
    function MultiEvaluateExpressionAsStringList(Elements: OleVariant; 
                                                 const Expression: WideString; 
                                                 const ClientId: WideString; 
                                                 SubscriberIds: OleVariant; Representation: Integer): OleVariant; dispid 128;
    property HasAdaptee: WordBool readonly dispid 129;
  end;




  IBoldListCore = interface(IBoldMember)
    ['{4153813B-4DE9-4A17-B747-7091A839BBFA}']
    procedure AddList(const List: IBoldListCore); safecall;
    function CanClear: WordBool; safecall;
    function CanMove(CurrentIndex: Integer; NewIndex: Integer): WordBool; safecall;
    function CanRemove(Index: Integer): WordBool; safecall;
    procedure Clear; safecall;
    procedure EnsureRange(FromIndex: Integer; ToIndex: Integer); safecall;
    procedure InsertNew(Index: Integer); safecall;
    procedure Move(CurrentIndex: Integer; NewIndex: Integer); safecall;
    procedure RemoveByIndex(Index: Integer); safecall;
    function ToStringList(Representation: Integer): OleVariant; safecall;
    function ToStringListWithNil(Representation: Integer; const NilString: WideString): OleVariant; safecall;
    function Get_CanCreateNew: WordBool; safecall;
    function Get_Count: Integer; safecall;
    function Get_DuplicateMode: Integer; safecall;
    procedure Set_DuplicateMode(Value: Integer); safecall;
    property CanCreateNew: WordBool read Get_CanCreateNew;
    property Count: Integer read Get_Count;
    property DuplicateMode: Integer read Get_DuplicateMode write Set_DuplicateMode;
  end;




  IBoldListCoreDisp = dispinterface
    ['{4153813B-4DE9-4A17-B747-7091A839BBFA}']
    procedure AddList(const List: IBoldListCore); dispid 401;
    function CanClear: WordBool; dispid 402;
    function CanMove(CurrentIndex: Integer; NewIndex: Integer): WordBool; dispid 403;
    function CanRemove(Index: Integer): WordBool; dispid 404;
    procedure Clear; dispid 405;
    procedure EnsureRange(FromIndex: Integer; ToIndex: Integer); dispid 406;
    procedure InsertNew(Index: Integer); dispid 407;
    procedure Move(CurrentIndex: Integer; NewIndex: Integer); dispid 408;
    procedure RemoveByIndex(Index: Integer); dispid 409;
    function ToStringList(Representation: Integer): OleVariant; dispid 410;
    function ToStringListWithNil(Representation: Integer; const NilString: WideString): OleVariant; dispid 411;
    property CanCreateNew: WordBool readonly dispid 412;
    property Count: Integer readonly dispid 413;
    property DuplicateMode: Integer dispid 414;
    function Clone: IBoldMember; dispid 301;
    procedure DiscardChanges; dispid 302;
    procedure EnsureContentsCurrent; dispid 303;
    procedure Invalidate; dispid 304;
    property BoldMemberRTInfo: IUnknown readonly dispid 305;
    property BoldSystem: IBoldSystem readonly dispid 306;
    property CanModify: WordBool readonly dispid 307;
    property CanRead: WordBool readonly dispid 308;
    property Derived: WordBool readonly dispid 309;
    property IsPartOfSystem: WordBool readonly dispid 310;
    property OwningObject: IBoldObject readonly dispid 311;
    property BoldDirty: WordBool readonly dispid 201;
    property BoldPersistent: WordBool readonly dispid 202;
    property DisplayName: WideString readonly dispid 203;
    property OwningElement: IBoldDomainElement readonly dispid 204;
    procedure AddSmallSubscription(const ClientId: WideString; SubscriberId: Integer; 
                                   SmallEvents: Integer; RequestedEvent: Integer; 
                                   CancelOld: WordBool); dispid 101;
    procedure AddSubscription(const ClientId: WideString; SubscriberId: Integer; Event: Integer; 
                              RequestedEvent: Integer; CancelOld: WordBool); dispid 102;
    procedure AssignElement(const Element: IBoldElement); dispid 103;
    function CompareTo(const Element: IBoldElement): Integer; dispid 105;
    function CompareToAs(CompareType: Integer; const Element: IBoldElement): Integer; dispid 106;
    procedure DefaultSubscribe(const ClientId: WideString; SubscriberId: Integer; 
                               RequestedEvent: Integer; CancelOld: WordBool); dispid 107;
    function EvaluateAndSubscribeToExpression(const Expression: WideString; 
                                              const ClientId: WideString; SubscriberId: Integer; 
                                              Resubscribe: WordBool; CancelOld: WordBool): IBoldElement; dispid 108;
    function EvaluateExpression(const Expression: WideString): IBoldElement; dispid 109;
    function EvaluateExpressionAsSessionIdList(const Expression: WideString): OleVariant; dispid 110;
    function EvaluateExpressionAsSessionIdListAndStringList(const ListExpression: WideString; 
                                                            const ItemExpression: WideString; 
                                                            Representation: Integer): OleVariant; dispid 111;
    function EvaluateExpressionAsString(const Expression: WideString; Representation: Integer): WideString; dispid 112;
    function EvaluateExpressionAsStringList(const Expression: WideString; Representation: Integer): OleVariant; dispid 113;
    function EvaluateExpressionsAsStringLists(Expressions: OleVariant; Representation: Integer): OleVariant; dispid 114;
    function GetAsList: IBoldList; dispid 115;
    function IsEqual(const Element: IBoldElement): WordBool; dispid 116;
    function IsEqualAs(CompareType: Integer; const Element: IBoldElement): WordBool; dispid 117;
    procedure MakeImmutable; dispid 118;
    procedure SubscribeToExpression(const Expression: WideString; const ClientId: WideString; 
                                    SubscriberId: Integer; Resubscribe: WordBool; 
                                    CancelOld: WordBool); dispid 119;
    procedure SubscribeToStringRepresentation(Representation: Integer; const ClientId: WideString; 
                                              SubscriberId: Integer; RequestedEvent: Integer; 
                                              CancelOld: WordBool); dispid 120;
    function ValidateCharacter(const Value: WideString; Representation: Integer): WordBool; dispid 121;
    function ValidateString(const Value: WideString; Representation: Integer): WordBool; dispid 122;
    property AsString: WideString dispid 0;
    property AsVariant: OleVariant dispid 123;
    property BoldType: IBoldElementTypeInfo readonly dispid 124;
    property BoldTypeName: WideString readonly dispid 125;
    property Mutable: WordBool readonly dispid 126;
    property StringRepresentation[Representation: Integer]: WideString dispid 127;
    property _NewEnum: IUnknown readonly dispid -4;
    function MultiEvaluateExpressionAsStringList(Elements: OleVariant; 
                                                 const Expression: WideString; 
                                                 const ClientId: WideString; 
                                                 SubscriberIds: OleVariant; Representation: Integer): OleVariant; dispid 128;
    property HasAdaptee: WordBool readonly dispid 129;
  end;




  IBoldList = interface(IBoldListCore)
    ['{798895F6-E991-432B-9F37-7998BA769199}']
    procedure Add(const Element: IBoldElement); safecall;
    function AddNew: IBoldElement; safecall;
    function CanInsert(Index: Integer; const Element: IBoldElement): WordBool; safecall;
    function CanSet(Index: Integer; const Element: IBoldElement): WordBool; safecall;
    function Includes(const Element: IBoldElement): WordBool; safecall;
    function IndexOf(const Element: IBoldElement): Integer; safecall;
    procedure Insert(Index: Integer; const Element: IBoldElement); safecall;
    procedure Remove(const Element: IBoldElement); safecall;
    function Get_Elements(Index: Integer): IBoldElement; safecall;
    procedure Set_Elements(Index: Integer; const Value: IBoldElement); safecall;
    function GetRAnge(FromIndex: Integer; ToIndex: Integer): OleVariant; safecall;
    property Elements[Index: Integer]: IBoldElement read Get_Elements write Set_Elements;
  end;




  IBoldListDisp = dispinterface
    ['{798895F6-E991-432B-9F37-7998BA769199}']
    procedure Add(const Element: IBoldElement); dispid 501;
    function AddNew: IBoldElement; dispid 502;
    function CanInsert(Index: Integer; const Element: IBoldElement): WordBool; dispid 503;
    function CanSet(Index: Integer; const Element: IBoldElement): WordBool; dispid 504;
    function Includes(const Element: IBoldElement): WordBool; dispid 505;
    function IndexOf(const Element: IBoldElement): Integer; dispid 506;
    procedure Insert(Index: Integer; const Element: IBoldElement); dispid 507;
    procedure Remove(const Element: IBoldElement); dispid 508;
    property Elements[Index: Integer]: IBoldElement dispid 509;
    function GetRAnge(FromIndex: Integer; ToIndex: Integer): OleVariant; dispid 510;
    procedure AddList(const List: IBoldListCore); dispid 401;
    function CanClear: WordBool; dispid 402;
    function CanMove(CurrentIndex: Integer; NewIndex: Integer): WordBool; dispid 403;
    function CanRemove(Index: Integer): WordBool; dispid 404;
    procedure Clear; dispid 405;
    procedure EnsureRange(FromIndex: Integer; ToIndex: Integer); dispid 406;
    procedure InsertNew(Index: Integer); dispid 407;
    procedure Move(CurrentIndex: Integer; NewIndex: Integer); dispid 408;
    procedure RemoveByIndex(Index: Integer); dispid 409;
    function ToStringList(Representation: Integer): OleVariant; dispid 410;
    function ToStringListWithNil(Representation: Integer; const NilString: WideString): OleVariant; dispid 411;
    property CanCreateNew: WordBool readonly dispid 412;
    property Count: Integer readonly dispid 413;
    property DuplicateMode: Integer dispid 414;
    function Clone: IBoldMember; dispid 301;
    procedure DiscardChanges; dispid 302;
    procedure EnsureContentsCurrent; dispid 303;
    procedure Invalidate; dispid 304;
    property BoldMemberRTInfo: IUnknown readonly dispid 305;
    property BoldSystem: IBoldSystem readonly dispid 306;
    property CanModify: WordBool readonly dispid 307;
    property CanRead: WordBool readonly dispid 308;
    property Derived: WordBool readonly dispid 309;
    property IsPartOfSystem: WordBool readonly dispid 310;
    property OwningObject: IBoldObject readonly dispid 311;
    property BoldDirty: WordBool readonly dispid 201;
    property BoldPersistent: WordBool readonly dispid 202;
    property DisplayName: WideString readonly dispid 203;
    property OwningElement: IBoldDomainElement readonly dispid 204;
    procedure AddSmallSubscription(const ClientId: WideString; SubscriberId: Integer; 
                                   SmallEvents: Integer; RequestedEvent: Integer; 
                                   CancelOld: WordBool); dispid 101;
    procedure AddSubscription(const ClientId: WideString; SubscriberId: Integer; Event: Integer; 
                              RequestedEvent: Integer; CancelOld: WordBool); dispid 102;
    procedure AssignElement(const Element: IBoldElement); dispid 103;
    function CompareTo(const Element: IBoldElement): Integer; dispid 105;
    function CompareToAs(CompareType: Integer; const Element: IBoldElement): Integer; dispid 106;
    procedure DefaultSubscribe(const ClientId: WideString; SubscriberId: Integer; 
                               RequestedEvent: Integer; CancelOld: WordBool); dispid 107;
    function EvaluateAndSubscribeToExpression(const Expression: WideString; 
                                              const ClientId: WideString; SubscriberId: Integer; 
                                              Resubscribe: WordBool; CancelOld: WordBool): IBoldElement; dispid 108;
    function EvaluateExpression(const Expression: WideString): IBoldElement; dispid 109;
    function EvaluateExpressionAsSessionIdList(const Expression: WideString): OleVariant; dispid 110;
    function EvaluateExpressionAsSessionIdListAndStringList(const ListExpression: WideString; 
                                                            const ItemExpression: WideString; 
                                                            Representation: Integer): OleVariant; dispid 111;
    function EvaluateExpressionAsString(const Expression: WideString; Representation: Integer): WideString; dispid 112;
    function EvaluateExpressionAsStringList(const Expression: WideString; Representation: Integer): OleVariant; dispid 113;
    function EvaluateExpressionsAsStringLists(Expressions: OleVariant; Representation: Integer): OleVariant; dispid 114;
    function GetAsList: IBoldList; dispid 115;
    function IsEqual(const Element: IBoldElement): WordBool; dispid 116;
    function IsEqualAs(CompareType: Integer; const Element: IBoldElement): WordBool; dispid 117;
    procedure MakeImmutable; dispid 118;
    procedure SubscribeToExpression(const Expression: WideString; const ClientId: WideString; 
                                    SubscriberId: Integer; Resubscribe: WordBool; 
                                    CancelOld: WordBool); dispid 119;
    procedure SubscribeToStringRepresentation(Representation: Integer; const ClientId: WideString; 
                                              SubscriberId: Integer; RequestedEvent: Integer; 
                                              CancelOld: WordBool); dispid 120;
    function ValidateCharacter(const Value: WideString; Representation: Integer): WordBool; dispid 121;
    function ValidateString(const Value: WideString; Representation: Integer): WordBool; dispid 122;
    property AsString: WideString dispid 0;
    property AsVariant: OleVariant dispid 123;
    property BoldType: IBoldElementTypeInfo readonly dispid 124;
    property BoldTypeName: WideString readonly dispid 125;
    property Mutable: WordBool readonly dispid 126;
    property StringRepresentation[Representation: Integer]: WideString dispid 127;
    property _NewEnum: IUnknown readonly dispid -4;
    function MultiEvaluateExpressionAsStringList(Elements: OleVariant; 
                                                 const Expression: WideString; 
                                                 const ClientId: WideString; 
                                                 SubscriberIds: OleVariant; Representation: Integer): OleVariant; dispid 128;
    property HasAdaptee: WordBool readonly dispid 129;
  end;




  IBoldObjectList = interface(IBoldList)
    ['{75A31152-D30E-11D3-89A9-444553540000}']
    function Get_BoldObjects(Index: Integer): IBoldObject; safecall;
    procedure Set_BoldObjects(Index: Integer; const Value: IBoldObject); safecall;
    function Get_BoldRoleRTInfo: IUnknown; safecall;
    function Get_SubscribeToObjectsInList: WordBool; safecall;
    procedure Set_SubscribeToObjectsInList(Value: WordBool); safecall;
    property BoldObjects[Index: Integer]: IBoldObject read Get_BoldObjects write Set_BoldObjects;
    property BoldRoleRTInfo: IUnknown read Get_BoldRoleRTInfo;
    property SubscribeToObjectsInList: WordBool read Get_SubscribeToObjectsInList write Set_SubscribeToObjectsInList;
  end;




  IBoldObjectListDisp = dispinterface
    ['{75A31152-D30E-11D3-89A9-444553540000}']
    property BoldObjects[Index: Integer]: IBoldObject dispid 601;
    property BoldRoleRTInfo: IUnknown readonly dispid 602;
    property SubscribeToObjectsInList: WordBool dispid 603;
    procedure Add(const Element: IBoldElement); dispid 501;
    function AddNew: IBoldElement; dispid 502;
    function CanInsert(Index: Integer; const Element: IBoldElement): WordBool; dispid 503;
    function CanSet(Index: Integer; const Element: IBoldElement): WordBool; dispid 504;
    function Includes(const Element: IBoldElement): WordBool; dispid 505;
    function IndexOf(const Element: IBoldElement): Integer; dispid 506;
    procedure Insert(Index: Integer; const Element: IBoldElement); dispid 507;
    procedure Remove(const Element: IBoldElement); dispid 508;
    property Elements[Index: Integer]: IBoldElement dispid 509;
    function GetRAnge(FromIndex: Integer; ToIndex: Integer): OleVariant; dispid 510;
    procedure AddList(const List: IBoldListCore); dispid 401;
    function CanClear: WordBool; dispid 402;
    function CanMove(CurrentIndex: Integer; NewIndex: Integer): WordBool; dispid 403;
    function CanRemove(Index: Integer): WordBool; dispid 404;
    procedure Clear; dispid 405;
    procedure EnsureRange(FromIndex: Integer; ToIndex: Integer); dispid 406;
    procedure InsertNew(Index: Integer); dispid 407;
    procedure Move(CurrentIndex: Integer; NewIndex: Integer); dispid 408;
    procedure RemoveByIndex(Index: Integer); dispid 409;
    function ToStringList(Representation: Integer): OleVariant; dispid 410;
    function ToStringListWithNil(Representation: Integer; const NilString: WideString): OleVariant; dispid 411;
    property CanCreateNew: WordBool readonly dispid 412;
    property Count: Integer readonly dispid 413;
    property DuplicateMode: Integer dispid 414;
    function Clone: IBoldMember; dispid 301;
    procedure DiscardChanges; dispid 302;
    procedure EnsureContentsCurrent; dispid 303;
    procedure Invalidate; dispid 304;
    property BoldMemberRTInfo: IUnknown readonly dispid 305;
    property BoldSystem: IBoldSystem readonly dispid 306;
    property CanModify: WordBool readonly dispid 307;
    property CanRead: WordBool readonly dispid 308;
    property Derived: WordBool readonly dispid 309;
    property IsPartOfSystem: WordBool readonly dispid 310;
    property OwningObject: IBoldObject readonly dispid 311;
    property BoldDirty: WordBool readonly dispid 201;
    property BoldPersistent: WordBool readonly dispid 202;
    property DisplayName: WideString readonly dispid 203;
    property OwningElement: IBoldDomainElement readonly dispid 204;
    procedure AddSmallSubscription(const ClientId: WideString; SubscriberId: Integer; 
                                   SmallEvents: Integer; RequestedEvent: Integer; 
                                   CancelOld: WordBool); dispid 101;
    procedure AddSubscription(const ClientId: WideString; SubscriberId: Integer; Event: Integer; 
                              RequestedEvent: Integer; CancelOld: WordBool); dispid 102;
    procedure AssignElement(const Element: IBoldElement); dispid 103;
    function CompareTo(const Element: IBoldElement): Integer; dispid 105;
    function CompareToAs(CompareType: Integer; const Element: IBoldElement): Integer; dispid 106;
    procedure DefaultSubscribe(const ClientId: WideString; SubscriberId: Integer; 
                               RequestedEvent: Integer; CancelOld: WordBool); dispid 107;
    function EvaluateAndSubscribeToExpression(const Expression: WideString; 
                                              const ClientId: WideString; SubscriberId: Integer; 
                                              Resubscribe: WordBool; CancelOld: WordBool): IBoldElement; dispid 108;
    function EvaluateExpression(const Expression: WideString): IBoldElement; dispid 109;
    function EvaluateExpressionAsSessionIdList(const Expression: WideString): OleVariant; dispid 110;
    function EvaluateExpressionAsSessionIdListAndStringList(const ListExpression: WideString; 
                                                            const ItemExpression: WideString; 
                                                            Representation: Integer): OleVariant; dispid 111;
    function EvaluateExpressionAsString(const Expression: WideString; Representation: Integer): WideString; dispid 112;
    function EvaluateExpressionAsStringList(const Expression: WideString; Representation: Integer): OleVariant; dispid 113;
    function EvaluateExpressionsAsStringLists(Expressions: OleVariant; Representation: Integer): OleVariant; dispid 114;
    function GetAsList: IBoldList; dispid 115;
    function IsEqual(const Element: IBoldElement): WordBool; dispid 116;
    function IsEqualAs(CompareType: Integer; const Element: IBoldElement): WordBool; dispid 117;
    procedure MakeImmutable; dispid 118;
    procedure SubscribeToExpression(const Expression: WideString; const ClientId: WideString; 
                                    SubscriberId: Integer; Resubscribe: WordBool; 
                                    CancelOld: WordBool); dispid 119;
    procedure SubscribeToStringRepresentation(Representation: Integer; const ClientId: WideString; 
                                              SubscriberId: Integer; RequestedEvent: Integer; 
                                              CancelOld: WordBool); dispid 120;
    function ValidateCharacter(const Value: WideString; Representation: Integer): WordBool; dispid 121;
    function ValidateString(const Value: WideString; Representation: Integer): WordBool; dispid 122;
    property AsString: WideString dispid 0;
    property AsVariant: OleVariant dispid 123;
    property BoldType: IBoldElementTypeInfo readonly dispid 124;
    property BoldTypeName: WideString readonly dispid 125;
    property Mutable: WordBool readonly dispid 126;
    property StringRepresentation[Representation: Integer]: WideString dispid 127;
    property _NewEnum: IUnknown readonly dispid -4;
    function MultiEvaluateExpressionAsStringList(Elements: OleVariant; 
                                                 const Expression: WideString; 
                                                 const ClientId: WideString; 
                                                 SubscriberIds: OleVariant; Representation: Integer): OleVariant; dispid 128;
    property HasAdaptee: WordBool readonly dispid 129;
  end;




  IBoldMemberList = interface(IBoldList)
    ['{75A31154-D30E-11D3-89A9-444553540000}']
    function Get_BoldMembers(Index: Integer): IBoldMember; safecall;
    procedure Set_BoldMembers(Index: Integer; const Value: IBoldMember); safecall;
    property BoldMembers[Index: Integer]: IBoldMember read Get_BoldMembers write Set_BoldMembers;
  end;




  IBoldMemberListDisp = dispinterface
    ['{75A31154-D30E-11D3-89A9-444553540000}']
    property BoldMembers[Index: Integer]: IBoldMember dispid 601;
    procedure Add(const Element: IBoldElement); dispid 501;
    function AddNew: IBoldElement; dispid 502;
    function CanInsert(Index: Integer; const Element: IBoldElement): WordBool; dispid 503;
    function CanSet(Index: Integer; const Element: IBoldElement): WordBool; dispid 504;
    function Includes(const Element: IBoldElement): WordBool; dispid 505;
    function IndexOf(const Element: IBoldElement): Integer; dispid 506;
    procedure Insert(Index: Integer; const Element: IBoldElement); dispid 507;
    procedure Remove(const Element: IBoldElement); dispid 508;
    property Elements[Index: Integer]: IBoldElement dispid 509;
    function GetRAnge(FromIndex: Integer; ToIndex: Integer): OleVariant; dispid 510;
    procedure AddList(const List: IBoldListCore); dispid 401;
    function CanClear: WordBool; dispid 402;
    function CanMove(CurrentIndex: Integer; NewIndex: Integer): WordBool; dispid 403;
    function CanRemove(Index: Integer): WordBool; dispid 404;
    procedure Clear; dispid 405;
    procedure EnsureRange(FromIndex: Integer; ToIndex: Integer); dispid 406;
    procedure InsertNew(Index: Integer); dispid 407;
    procedure Move(CurrentIndex: Integer; NewIndex: Integer); dispid 408;
    procedure RemoveByIndex(Index: Integer); dispid 409;
    function ToStringList(Representation: Integer): OleVariant; dispid 410;
    function ToStringListWithNil(Representation: Integer; const NilString: WideString): OleVariant; dispid 411;
    property CanCreateNew: WordBool readonly dispid 412;
    property Count: Integer readonly dispid 413;
    property DuplicateMode: Integer dispid 414;
    function Clone: IBoldMember; dispid 301;
    procedure DiscardChanges; dispid 302;
    procedure EnsureContentsCurrent; dispid 303;
    procedure Invalidate; dispid 304;
    property BoldMemberRTInfo: IUnknown readonly dispid 305;
    property BoldSystem: IBoldSystem readonly dispid 306;
    property CanModify: WordBool readonly dispid 307;
    property CanRead: WordBool readonly dispid 308;
    property Derived: WordBool readonly dispid 309;
    property IsPartOfSystem: WordBool readonly dispid 310;
    property OwningObject: IBoldObject readonly dispid 311;
    property BoldDirty: WordBool readonly dispid 201;
    property BoldPersistent: WordBool readonly dispid 202;
    property DisplayName: WideString readonly dispid 203;
    property OwningElement: IBoldDomainElement readonly dispid 204;
    procedure AddSmallSubscription(const ClientId: WideString; SubscriberId: Integer; 
                                   SmallEvents: Integer; RequestedEvent: Integer; 
                                   CancelOld: WordBool); dispid 101;
    procedure AddSubscription(const ClientId: WideString; SubscriberId: Integer; Event: Integer; 
                              RequestedEvent: Integer; CancelOld: WordBool); dispid 102;
    procedure AssignElement(const Element: IBoldElement); dispid 103;
    function CompareTo(const Element: IBoldElement): Integer; dispid 105;
    function CompareToAs(CompareType: Integer; const Element: IBoldElement): Integer; dispid 106;
    procedure DefaultSubscribe(const ClientId: WideString; SubscriberId: Integer; 
                               RequestedEvent: Integer; CancelOld: WordBool); dispid 107;
    function EvaluateAndSubscribeToExpression(const Expression: WideString; 
                                              const ClientId: WideString; SubscriberId: Integer; 
                                              Resubscribe: WordBool; CancelOld: WordBool): IBoldElement; dispid 108;
    function EvaluateExpression(const Expression: WideString): IBoldElement; dispid 109;
    function EvaluateExpressionAsSessionIdList(const Expression: WideString): OleVariant; dispid 110;
    function EvaluateExpressionAsSessionIdListAndStringList(const ListExpression: WideString; 
                                                            const ItemExpression: WideString; 
                                                            Representation: Integer): OleVariant; dispid 111;
    function EvaluateExpressionAsString(const Expression: WideString; Representation: Integer): WideString; dispid 112;
    function EvaluateExpressionAsStringList(const Expression: WideString; Representation: Integer): OleVariant; dispid 113;
    function EvaluateExpressionsAsStringLists(Expressions: OleVariant; Representation: Integer): OleVariant; dispid 114;
    function GetAsList: IBoldList; dispid 115;
    function IsEqual(const Element: IBoldElement): WordBool; dispid 116;
    function IsEqualAs(CompareType: Integer; const Element: IBoldElement): WordBool; dispid 117;
    procedure MakeImmutable; dispid 118;
    procedure SubscribeToExpression(const Expression: WideString; const ClientId: WideString; 
                                    SubscriberId: Integer; Resubscribe: WordBool; 
                                    CancelOld: WordBool); dispid 119;
    procedure SubscribeToStringRepresentation(Representation: Integer; const ClientId: WideString; 
                                              SubscriberId: Integer; RequestedEvent: Integer; 
                                              CancelOld: WordBool); dispid 120;
    function ValidateCharacter(const Value: WideString; Representation: Integer): WordBool; dispid 121;
    function ValidateString(const Value: WideString; Representation: Integer): WordBool; dispid 122;
    property AsString: WideString dispid 0;
    property AsVariant: OleVariant dispid 123;
    property BoldType: IBoldElementTypeInfo readonly dispid 124;
    property BoldTypeName: WideString readonly dispid 125;
    property Mutable: WordBool readonly dispid 126;
    property StringRepresentation[Representation: Integer]: WideString dispid 127;
    property _NewEnum: IUnknown readonly dispid -4;
    function MultiEvaluateExpressionAsStringList(Elements: OleVariant; 
                                                 const Expression: WideString; 
                                                 const ClientId: WideString; 
                                                 SubscriberIds: OleVariant; Representation: Integer): OleVariant; dispid 128;
    property HasAdaptee: WordBool readonly dispid 129;
  end;




  IBoldSystem = interface(IBoldDomainElement)
    ['{8A530C40-D017-11D3-89A9-444553540000}']
    function CreateNewMember(const ExpressionName: WideString): IBoldMember; safecall;
    function CreateNewObject(const ExpressionName: WideString; Persistent: WordBool): IBoldObject; safecall;
    function CreateNewObjectWithMemberValues(const ExpressionName: WideString; 
                                             Persistent: WordBool; MemberValues: OleVariant): IBoldObject; safecall;
    procedure Discard; safecall;
    function EnsureEnclosure(const ObjectList: IBoldObjectList; ValidateOnly: WordBool): WordBool; safecall;
    function GetObjectBySessionId(SessionId: OleVariant): IBoldObject; safecall;
    procedure UpdateDatabase; safecall;
    procedure UpdateDatabaseWithList(const ObjectList: IBoldObjectList); safecall;
    function Get_BoldSystemTypeInfo: IBoldSystemTypeInfo; safecall;
    function Get_ClassByExpressionName(const ExpressionName: WideString): IBoldObjectList; safecall;
    function Get_DirtyObjects: IBoldObjectList; safecall;
    function Get_LoadedObjects: IBoldObjectList; safecall;
    property BoldSystemTypeInfo: IBoldSystemTypeInfo read Get_BoldSystemTypeInfo;
    property ClassByExpressionName[const ExpressionName: WideString]: IBoldObjectList read Get_ClassByExpressionName;
    property DirtyObjects: IBoldObjectList read Get_DirtyObjects;
    property LoadedObjects: IBoldObjectList read Get_LoadedObjects;
  end;




  IBoldSystemDisp = dispinterface
    ['{8A530C40-D017-11D3-89A9-444553540000}']
    function CreateNewMember(const ExpressionName: WideString): IBoldMember; dispid 301;
    function CreateNewObject(const ExpressionName: WideString; Persistent: WordBool): IBoldObject; dispid 302;
    function CreateNewObjectWithMemberValues(const ExpressionName: WideString; 
                                             Persistent: WordBool; MemberValues: OleVariant): IBoldObject; dispid 303;
    procedure Discard; dispid 304;
    function EnsureEnclosure(const ObjectList: IBoldObjectList; ValidateOnly: WordBool): WordBool; dispid 305;
    function GetObjectBySessionId(SessionId: OleVariant): IBoldObject; dispid 306;
    procedure UpdateDatabase; dispid 307;
    procedure UpdateDatabaseWithList(const ObjectList: IBoldObjectList); dispid 308;
    property BoldSystemTypeInfo: IBoldSystemTypeInfo readonly dispid 309;
    property ClassByExpressionName[const ExpressionName: WideString]: IBoldObjectList readonly dispid 310;
    property DirtyObjects: IBoldObjectList readonly dispid 311;
    property LoadedObjects: IBoldObjectList readonly dispid 312;
    property BoldDirty: WordBool readonly dispid 201;
    property BoldPersistent: WordBool readonly dispid 202;
    property DisplayName: WideString readonly dispid 203;
    property OwningElement: IBoldDomainElement readonly dispid 204;
    procedure AddSmallSubscription(const ClientId: WideString; SubscriberId: Integer; 
                                   SmallEvents: Integer; RequestedEvent: Integer; 
                                   CancelOld: WordBool); dispid 101;
    procedure AddSubscription(const ClientId: WideString; SubscriberId: Integer; Event: Integer; 
                              RequestedEvent: Integer; CancelOld: WordBool); dispid 102;
    procedure AssignElement(const Element: IBoldElement); dispid 103;
    function CompareTo(const Element: IBoldElement): Integer; dispid 105;
    function CompareToAs(CompareType: Integer; const Element: IBoldElement): Integer; dispid 106;
    procedure DefaultSubscribe(const ClientId: WideString; SubscriberId: Integer; 
                               RequestedEvent: Integer; CancelOld: WordBool); dispid 107;
    function EvaluateAndSubscribeToExpression(const Expression: WideString; 
                                              const ClientId: WideString; SubscriberId: Integer; 
                                              Resubscribe: WordBool; CancelOld: WordBool): IBoldElement; dispid 108;
    function EvaluateExpression(const Expression: WideString): IBoldElement; dispid 109;
    function EvaluateExpressionAsSessionIdList(const Expression: WideString): OleVariant; dispid 110;
    function EvaluateExpressionAsSessionIdListAndStringList(const ListExpression: WideString; 
                                                            const ItemExpression: WideString; 
                                                            Representation: Integer): OleVariant; dispid 111;
    function EvaluateExpressionAsString(const Expression: WideString; Representation: Integer): WideString; dispid 112;
    function EvaluateExpressionAsStringList(const Expression: WideString; Representation: Integer): OleVariant; dispid 113;
    function EvaluateExpressionsAsStringLists(Expressions: OleVariant; Representation: Integer): OleVariant; dispid 114;
    function GetAsList: IBoldList; dispid 115;
    function IsEqual(const Element: IBoldElement): WordBool; dispid 116;
    function IsEqualAs(CompareType: Integer; const Element: IBoldElement): WordBool; dispid 117;
    procedure MakeImmutable; dispid 118;
    procedure SubscribeToExpression(const Expression: WideString; const ClientId: WideString; 
                                    SubscriberId: Integer; Resubscribe: WordBool; 
                                    CancelOld: WordBool); dispid 119;
    procedure SubscribeToStringRepresentation(Representation: Integer; const ClientId: WideString; 
                                              SubscriberId: Integer; RequestedEvent: Integer; 
                                              CancelOld: WordBool); dispid 120;
    function ValidateCharacter(const Value: WideString; Representation: Integer): WordBool; dispid 121;
    function ValidateString(const Value: WideString; Representation: Integer): WordBool; dispid 122;
    property AsString: WideString dispid 0;
    property AsVariant: OleVariant dispid 123;
    property BoldType: IBoldElementTypeInfo readonly dispid 124;
    property BoldTypeName: WideString readonly dispid 125;
    property Mutable: WordBool readonly dispid 126;
    property StringRepresentation[Representation: Integer]: WideString dispid 127;
    property _NewEnum: IUnknown readonly dispid -4;
    function MultiEvaluateExpressionAsStringList(Elements: OleVariant; 
                                                 const Expression: WideString; 
                                                 const ClientId: WideString; 
                                                 SubscriberIds: OleVariant; Representation: Integer): OleVariant; dispid 128;
    property HasAdaptee: WordBool readonly dispid 129;
  end;




  IBoldBlob = interface(IBoldAttribute)
    ['{68A9F8C2-D646-11D3-89A9-444553540000}']
  end;




  IBoldBlobDisp = dispinterface
    ['{68A9F8C2-D646-11D3-89A9-444553540000}']
    procedure SetToNull; dispid 401;
    property BoldAttributeRTInfo: IUnknown readonly dispid 402;
    property CanSetToNull: WordBool readonly dispid 403;
    property IsNull: WordBool readonly dispid 404;
    function Clone: IBoldMember; dispid 301;
    procedure DiscardChanges; dispid 302;
    procedure EnsureContentsCurrent; dispid 303;
    procedure Invalidate; dispid 304;
    property BoldMemberRTInfo: IUnknown readonly dispid 305;
    property BoldSystem: IBoldSystem readonly dispid 306;
    property CanModify: WordBool readonly dispid 307;
    property CanRead: WordBool readonly dispid 308;
    property Derived: WordBool readonly dispid 309;
    property IsPartOfSystem: WordBool readonly dispid 310;
    property OwningObject: IBoldObject readonly dispid 311;
    property BoldDirty: WordBool readonly dispid 201;
    property BoldPersistent: WordBool readonly dispid 202;
    property DisplayName: WideString readonly dispid 203;
    property OwningElement: IBoldDomainElement readonly dispid 204;
    procedure AddSmallSubscription(const ClientId: WideString; SubscriberId: Integer; 
                                   SmallEvents: Integer; RequestedEvent: Integer; 
                                   CancelOld: WordBool); dispid 101;
    procedure AddSubscription(const ClientId: WideString; SubscriberId: Integer; Event: Integer; 
                              RequestedEvent: Integer; CancelOld: WordBool); dispid 102;
    procedure AssignElement(const Element: IBoldElement); dispid 103;
    function CompareTo(const Element: IBoldElement): Integer; dispid 105;
    function CompareToAs(CompareType: Integer; const Element: IBoldElement): Integer; dispid 106;
    procedure DefaultSubscribe(const ClientId: WideString; SubscriberId: Integer; 
                               RequestedEvent: Integer; CancelOld: WordBool); dispid 107;
    function EvaluateAndSubscribeToExpression(const Expression: WideString; 
                                              const ClientId: WideString; SubscriberId: Integer; 
                                              Resubscribe: WordBool; CancelOld: WordBool): IBoldElement; dispid 108;
    function EvaluateExpression(const Expression: WideString): IBoldElement; dispid 109;
    function EvaluateExpressionAsSessionIdList(const Expression: WideString): OleVariant; dispid 110;
    function EvaluateExpressionAsSessionIdListAndStringList(const ListExpression: WideString; 
                                                            const ItemExpression: WideString; 
                                                            Representation: Integer): OleVariant; dispid 111;
    function EvaluateExpressionAsString(const Expression: WideString; Representation: Integer): WideString; dispid 112;
    function EvaluateExpressionAsStringList(const Expression: WideString; Representation: Integer): OleVariant; dispid 113;
    function EvaluateExpressionsAsStringLists(Expressions: OleVariant; Representation: Integer): OleVariant; dispid 114;
    function GetAsList: IBoldList; dispid 115;
    function IsEqual(const Element: IBoldElement): WordBool; dispid 116;
    function IsEqualAs(CompareType: Integer; const Element: IBoldElement): WordBool; dispid 117;
    procedure MakeImmutable; dispid 118;
    procedure SubscribeToExpression(const Expression: WideString; const ClientId: WideString; 
                                    SubscriberId: Integer; Resubscribe: WordBool; 
                                    CancelOld: WordBool); dispid 119;
    procedure SubscribeToStringRepresentation(Representation: Integer; const ClientId: WideString; 
                                              SubscriberId: Integer; RequestedEvent: Integer; 
                                              CancelOld: WordBool); dispid 120;
    function ValidateCharacter(const Value: WideString; Representation: Integer): WordBool; dispid 121;
    function ValidateString(const Value: WideString; Representation: Integer): WordBool; dispid 122;
    property AsString: WideString dispid 0;
    property AsVariant: OleVariant dispid 123;
    property BoldType: IBoldElementTypeInfo readonly dispid 124;
    property BoldTypeName: WideString readonly dispid 125;
    property Mutable: WordBool readonly dispid 126;
    property StringRepresentation[Representation: Integer]: WideString dispid 127;
    property _NewEnum: IUnknown readonly dispid -4;
    function MultiEvaluateExpressionAsStringList(Elements: OleVariant; 
                                                 const Expression: WideString; 
                                                 const ClientId: WideString; 
                                                 SubscriberIds: OleVariant; Representation: Integer): OleVariant; dispid 128;
    property HasAdaptee: WordBool readonly dispid 129;
  end;




  IBoldElementHandle = interface(IDispatch)
    ['{71446D80-01C9-4E3C-95A7-D74445C0776C}']
    procedure AddSmallSubscription(const ClientId: WideString; SubscriberId: Integer; 
                                   SmallEvents: Integer; RequestedEvent: Integer; 
                                   CancelOld: WordBool); safecall;
    procedure AddSubscription(const ClientId: WideString; SubscriberId: Integer; Event: Integer; 
                              RequestedEvent: Integer; CancelOld: WordBool); safecall;
    function GetData(DataFlags: Integer; out Value: IBoldElement; 
                     out DynamicBoldType: IBoldElementTypeInfo; 
                     out StaticBoldType: IBoldElementTypeInfo; 
                     out StaticSystemType: IBoldSystemTypeInfo; out BoldSystem: IBoldSystem; 
                     out StaticRootType: IBoldElementTypeInfo; out CurrentBoldObject: IBoldObject; 
                     out BoldList: IBoldList; out ListElementType: IBoldElementTypeInfo; 
                     out NamedValues: OleVariant): WordBool; safecall;
    function SetData(DataFlags: Integer; const Value: IBoldElement; NamedValues: OleVariant): WordBool; safecall;
  end;




  IBoldElementHandleDisp = dispinterface
    ['{71446D80-01C9-4E3C-95A7-D74445C0776C}']
    procedure AddSmallSubscription(const ClientId: WideString; SubscriberId: Integer; 
                                   SmallEvents: Integer; RequestedEvent: Integer; 
                                   CancelOld: WordBool); dispid 101;
    procedure AddSubscription(const ClientId: WideString; SubscriberId: Integer; Event: Integer; 
                              RequestedEvent: Integer; CancelOld: WordBool); dispid 102;
    function GetData(DataFlags: Integer; out Value: IBoldElement; 
                     out DynamicBoldType: IBoldElementTypeInfo; 
                     out StaticBoldType: IBoldElementTypeInfo; 
                     out StaticSystemType: IBoldSystemTypeInfo; out BoldSystem: IBoldSystem; 
                     out StaticRootType: IBoldElementTypeInfo; out CurrentBoldObject: IBoldObject; 
                     out BoldList: IBoldList; out ListElementType: IBoldElementTypeInfo; 
                     out NamedValues: OleVariant): WordBool; dispid 103;
    function SetData(DataFlags: Integer; const Value: IBoldElement; NamedValues: OleVariant): WordBool; dispid 104;
  end;

implementation

uses ComObj;

end.
