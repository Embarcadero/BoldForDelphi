unit AccountClasses_TLB;

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
// File generated on 2003-02-11 17:57:10 from Type Library described below.

// ************************************************************************  //
// Type Lib: C:\dev\BfD\examples\Delphi\Compound\bios2\Core\AccountServer.tlb (1)
// LIBID: {6B076BBC-EAFF-4C11-B024-B542EF8CFCB3}
// LCID: 0
// Helpfile: 
// HelpString: AccountClasses Library
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
  AccountClassesMajorVersion = 1;
  AccountClassesMinorVersion = 0;

  LIBID_AccountClasses: TGUID = '{6B076BBC-EAFF-4C11-B024-B542EF8CFCB3}';

  IID_IAccountClassesRoot: TGUID = '{34342E85-B63D-4851-BCB8-001BF1EC75D4}';
  IID_IAccount: TGUID = '{F360E44E-4436-4721-98B3-3C001AA12073}';
  IID_IAccountValidator: TGUID = '{84433B48-0FCF-41CF-8C31-9F4ECFF4B5FE}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IAccountClassesRoot = interface;
  IAccountClassesRootDisp = dispinterface;
  IAccount = interface;
  IAccountDisp = dispinterface;
  IAccountValidator = interface;

// *********************************************************************//
// Interface: IAccountClassesRoot
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {34342E85-B63D-4851-BCB8-001BF1EC75D4}
// *********************************************************************//
  IAccountClassesRoot = interface(IBoldObject)
    ['{34342E85-B63D-4851-BCB8-001BF1EC75D4}']
  end;

// *********************************************************************//
// DispIntf:  IAccountClassesRootDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {34342E85-B63D-4851-BCB8-001BF1EC75D4}
// *********************************************************************//
  IAccountClassesRootDisp = dispinterface
    ['{34342E85-B63D-4851-BCB8-001BF1EC75D4}']
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
// Interface: IAccount
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F360E44E-4436-4721-98B3-3C001AA12073}
// *********************************************************************//
  IAccount = interface(IAccountClassesRoot)
    ['{F360E44E-4436-4721-98B3-3C001AA12073}']
    function Get_Total: Integer; safecall;
    procedure Set_Total(Value: Integer); safecall;
    function Get_Number: WideString; safecall;
    procedure Set_Number(const Value: WideString); safecall;
    function Get_Credit: Integer; safecall;
    procedure Set_Credit(Value: Integer); safecall;
    property Total: Integer read Get_Total write Set_Total;
    property Number: WideString read Get_Number write Set_Number;
    property Credit: Integer read Get_Credit write Set_Credit;
  end;

// *********************************************************************//
// DispIntf:  IAccountDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F360E44E-4436-4721-98B3-3C001AA12073}
// *********************************************************************//
  IAccountDisp = dispinterface
    ['{F360E44E-4436-4721-98B3-3C001AA12073}']
    property Total: Integer dispid 1024;
    property Number: WideString dispid 1025;
    property Credit: Integer dispid 1026;
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
// Interface: IAccountValidator
// Flags:     (256) OleAutomation
// GUID:      {84433B48-0FCF-41CF-8C31-9F4ECFF4B5FE}
// *********************************************************************//
  IAccountValidator = interface(IUnknown)
    ['{84433B48-0FCF-41CF-8C31-9F4ECFF4B5FE}']
    function Validate(const Number: WideString; out Value: WordBool): HResult; stdcall;
  end;

implementation

uses ComObj;

end.
