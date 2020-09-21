unit BuildingsAndOwners_TLB;

// ************************************************************************ //
// WARNING                                                                    
// -------                                                                    
// The types declared in this file were generated from data read from a       
// Type Library. If this type library is explicitly or indirectly (via        
// another type library referring to this type library) re-imported, or the   
// 'Refresh' command of the Type Library Editor activated while editing the   
// Type Library, the contents of this file will be regenerated and all        
// manual modifications will be lost.                                         
// ************************************************************************ //

// PASTLWTR : 1.2
// File generated on 2003-02-11 17:55:03 from Type Library described below.

// ************************************************************************  //
// Type Lib: C:\dev\BfD\examples\Delphi\Compound\Bios\Core\BuildingsAndOwners.tlb (1)
// LIBID: {531EB87A-ED99-463D-9500-5EFCD99A4C1D}
// LCID: 0
// Helpfile: 
// HelpString: BuildingsAndOwners Library
// DepndLst: 
//   (1) v1.0 BoldComObjectSpace, (C:\dev\BfD\Stage\D7\Bin\BoldComObjectSpace.dll)
//   (2) v2.0 stdole, (C:\WINDOWS\System32\stdole2.tlb)
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
interface

uses Windows, ActiveX, BoldComObjectSpace_TLB, Classes, Graphics, StdVCL, Variants;
  

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  BuildingsAndOwnersMajorVersion = 1;
  BuildingsAndOwnersMinorVersion = 0;

  LIBID_BuildingsAndOwners: TGUID = '{531EB87A-ED99-463D-9500-5EFCD99A4C1D}';

  IID_IBusinessClassesRoot: TGUID = '{731E8B64-CF46-4EC0-9BD3-8AD4C3187758}';
  IID_IBuilding: TGUID = '{716EE41A-12DA-4B71-A36F-C533FCE8BBC3}';
  IID_IOwnership: TGUID = '{02663CB2-8873-4362-9DA6-3F7BC1D49122}';
  IID_IPerson: TGUID = '{859600BD-8B4F-48AD-BDB4-D419A40597DA}';
  IID_IResidential_Building: TGUID = '{5F4901A9-950D-4A6A-8CE1-13D715A895A7}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IBusinessClassesRoot = interface;
  IBusinessClassesRootDisp = dispinterface;
  IBuilding = interface;
  IBuildingDisp = dispinterface;
  IOwnership = interface;
  IOwnershipDisp = dispinterface;
  IPerson = interface;
  IPersonDisp = dispinterface;
  IResidential_Building = interface;
  IResidential_BuildingDisp = dispinterface;

// *********************************************************************//
// Interface: IBusinessClassesRoot
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {731E8B64-CF46-4EC0-9BD3-8AD4C3187758}
// *********************************************************************//
  IBusinessClassesRoot = interface(IBoldObject)
    ['{731E8B64-CF46-4EC0-9BD3-8AD4C3187758}']
  end;

// *********************************************************************//
// DispIntf:  IBusinessClassesRootDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {731E8B64-CF46-4EC0-9BD3-8AD4C3187758}
// *********************************************************************//
  IBusinessClassesRootDisp = dispinterface
    ['{731E8B64-CF46-4EC0-9BD3-8AD4C3187758}']
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

// *********************************************************************//
// Interface: IBuilding
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {716EE41A-12DA-4B71-A36F-C533FCE8BBC3}
// *********************************************************************//
  IBuilding = interface(IBusinessClassesRoot)
    ['{716EE41A-12DA-4B71-A36F-C533FCE8BBC3}']
    function Get_ZipCode: Integer; safecall;
    procedure Set_ZipCode(Value: Integer); safecall;
    function Get_Address: WideString; safecall;
    procedure Set_Address(const Value: WideString); safecall;
    function Get_Owners: IBoldObjectList; safecall;
    function Get_Ownership: IBoldObjectList; safecall;
    property ZipCode: Integer read Get_ZipCode write Set_ZipCode;
    property Address: WideString read Get_Address write Set_Address;
    property Owners: IBoldObjectList read Get_Owners;
    property Ownership: IBoldObjectList read Get_Ownership;
  end;

// *********************************************************************//
// DispIntf:  IBuildingDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {716EE41A-12DA-4B71-A36F-C533FCE8BBC3}
// *********************************************************************//
  IBuildingDisp = dispinterface
    ['{716EE41A-12DA-4B71-A36F-C533FCE8BBC3}']
    property ZipCode: Integer dispid 1024;
    property Address: WideString dispid 1025;
    property Owners: IBoldObjectList readonly dispid 1026;
    property Ownership: IBoldObjectList readonly dispid 1027;
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

// *********************************************************************//
// Interface: IOwnership
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {02663CB2-8873-4362-9DA6-3F7BC1D49122}
// *********************************************************************//
  IOwnership = interface(IBusinessClassesRoot)
    ['{02663CB2-8873-4362-9DA6-3F7BC1D49122}']
    function Get_Owners: IBusinessClassesRoot; safecall;
    function Get_OwnedBuildings: IBusinessClassesRoot; safecall;
    property Owners: IBusinessClassesRoot read Get_Owners;
    property OwnedBuildings: IBusinessClassesRoot read Get_OwnedBuildings;
  end;

// *********************************************************************//
// DispIntf:  IOwnershipDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {02663CB2-8873-4362-9DA6-3F7BC1D49122}
// *********************************************************************//
  IOwnershipDisp = dispinterface
    ['{02663CB2-8873-4362-9DA6-3F7BC1D49122}']
    property Owners: IBusinessClassesRoot readonly dispid 1024;
    property OwnedBuildings: IBusinessClassesRoot readonly dispid 1025;
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

// *********************************************************************//
// Interface: IPerson
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {859600BD-8B4F-48AD-BDB4-D419A40597DA}
// *********************************************************************//
  IPerson = interface(IBusinessClassesRoot)
    ['{859600BD-8B4F-48AD-BDB4-D419A40597DA}']
    function Get_FirstName: WideString; safecall;
    procedure Set_FirstName(const Value: WideString); safecall;
    function Get_LastName: WideString; safecall;
    procedure Set_LastName(const Value: WideString); safecall;
    function Get_Assets: Currency; safecall;
    procedure Set_Assets(Value: Currency); safecall;
    function Get_IsMarried: WordBool; safecall;
    procedure Set_IsMarried(Value: WordBool); safecall;
    function Get_Home: IBusinessClassesRoot; safecall;
    procedure Set_Home(const Value: IBusinessClassesRoot); safecall;
    function Get_OwnedBuildings: IBoldObjectList; safecall;
    function Get_Ownership: IBoldObjectList; safecall;
    procedure BorrowFrom(const Lender: IPerson; Amount: Integer); safecall;
    property FirstName: WideString read Get_FirstName write Set_FirstName;
    property LastName: WideString read Get_LastName write Set_LastName;
    property Assets: Currency read Get_Assets write Set_Assets;
    property IsMarried: WordBool read Get_IsMarried write Set_IsMarried;
    property Home: IBusinessClassesRoot read Get_Home write Set_Home;
    property OwnedBuildings: IBoldObjectList read Get_OwnedBuildings;
    property Ownership: IBoldObjectList read Get_Ownership;
  end;

// *********************************************************************//
// DispIntf:  IPersonDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {859600BD-8B4F-48AD-BDB4-D419A40597DA}
// *********************************************************************//
  IPersonDisp = dispinterface
    ['{859600BD-8B4F-48AD-BDB4-D419A40597DA}']
    property FirstName: WideString dispid 1024;
    property LastName: WideString dispid 1025;
    property Assets: Currency dispid 1026;
    property IsMarried: WordBool dispid 1027;
    property Home: IBusinessClassesRoot dispid 1028;
    property OwnedBuildings: IBoldObjectList readonly dispid 1029;
    property Ownership: IBoldObjectList readonly dispid 1030;
    procedure BorrowFrom(const Lender: IPerson; Amount: Integer); dispid 2048;
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

// *********************************************************************//
// Interface: IResidential_Building
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {5F4901A9-950D-4A6A-8CE1-13D715A895A7}
// *********************************************************************//
  IResidential_Building = interface(IBuilding)
    ['{5F4901A9-950D-4A6A-8CE1-13D715A895A7}']
    function Get_TotalRent: Currency; safecall;
    procedure Set_TotalRent(Value: Currency); safecall;
    function Get_Residents: IBoldObjectList; safecall;
    procedure ChargeRent; safecall;
    property TotalRent: Currency read Get_TotalRent write Set_TotalRent;
    property Residents: IBoldObjectList read Get_Residents;
  end;

// *********************************************************************//
// DispIntf:  IResidential_BuildingDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {5F4901A9-950D-4A6A-8CE1-13D715A895A7}
// *********************************************************************//
  IResidential_BuildingDisp = dispinterface
    ['{5F4901A9-950D-4A6A-8CE1-13D715A895A7}']
    property TotalRent: Currency dispid 1028;
    property Residents: IBoldObjectList readonly dispid 1029;
    procedure ChargeRent; dispid 2048;
    property ZipCode: Integer dispid 1024;
    property Address: WideString dispid 1025;
    property Owners: IBoldObjectList readonly dispid 1026;
    property Ownership: IBoldObjectList readonly dispid 1027;
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

implementation

uses ComObj;

end.
