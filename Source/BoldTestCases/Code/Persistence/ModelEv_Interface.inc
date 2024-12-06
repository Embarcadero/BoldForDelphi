(*****************************************)
(*      This file is autogenerated       *)
(*   Any manual changes will be LOST!    *)
(*****************************************)
(* Generated 11-06-2019 15:41:18         *)
(*****************************************)
(* This file should be stored in the     *)
(* same directory as the form/datamodule *)
(* with the corresponding model          *)
(*****************************************)
(* Copyright notice:                     *)
(*                                       *)
(*****************************************)

{$IFNDEF ModelEv_Interface.inc}
{$DEFINE ModelEv_Interface.inc}

{$IFNDEF ModelEv_unitheader}
unit ModelEv;
{$ENDIF}

interface

uses
  // interface uses
  // interface dependencies
  // attribute classes
  BoldAttributes,
  // other
  Classes,
  Controls, // for TDate
  SysUtils,
  BoldDefs,
  BoldSubscription,
  BoldDeriver,
  BoldElements,
  BoldDomainElement,
  BoldSystemRT,
  BoldSystem;

type
  { forward declarations of all classes }

  TBusinessClassesRoot = class;
  TBusinessClassesRootList = class;
  TA = class;
  TAList = class;
  TB = class;
  TBList = class;
  TBaRole = class;
  TBaRoleList = class;
  TC = class;
  TCList = class;
  TSubB = class;
  TSubBList = class;
  TCSub = class;
  TCSubList = class;
  TCSubSub1 = class;
  TCSubSub1List = class;
  TCSubSub2 = class;
  TCSubSub2List = class;

  TBusinessClassesRoot = class(TBoldObject)
  private
  protected
  public
  end;

  TA = class(TBusinessClassesRoot)
  private
    function _Get_M_a1: TBAString;
    function _Geta1: String;
    procedure _Seta1(const NewValue: String);
    function _Get_M_a2: TBAInteger;
    function _Geta2: Integer;
    procedure _Seta2(const NewValue: Integer);
    function _GetB: TBList;
    function _GetBaRole: TBaRoleList;
  protected
  public
    property M_a1: TBAString read _Get_M_a1;
    property M_a2: TBAInteger read _Get_M_a2;
    property M_B: TBList read _GetB;
    property M_BaRole: TBaRoleList read _GetBaRole;
    property a1: String read _Geta1 write _Seta1;
    property a2: Integer read _Geta2 write _Seta2;
    property B: TBList read _GetB;
    property BaRole: TBaRoleList read _GetBaRole;
  end;

  TB = class(TBusinessClassesRoot)
  private
    function _Get_M_b1: TBAString;
    function _Getb1: String;
    procedure _Setb1(const NewValue: String);
    function _Get_M_b2: TBAInteger;
    function _Getb2: Integer;
    procedure _Setb2(const NewValue: Integer);
    function _GetaRole: TAList;
    function _GetBaRole: TBaRoleList;
    function _GetC: TCList;
  protected
  public
    property M_b1: TBAString read _Get_M_b1;
    property M_b2: TBAInteger read _Get_M_b2;
    property M_aRole: TAList read _GetaRole;
    property M_BaRole: TBaRoleList read _GetBaRole;
    property M_C: TCList read _GetC;
    property b1: String read _Getb1 write _Setb1;
    property b2: Integer read _Getb2 write _Setb2;
    property aRole: TAList read _GetaRole;
    property BaRole: TBaRoleList read _GetBaRole;
    property C: TCList read _GetC;
  end;

  TBaRole = class(TBusinessClassesRoot)
  private
    function _GetaRole: TA;
    function _Get_M_aRole: TBoldObjectReference;
    function _GetB: TB;
    function _Get_M_B: TBoldObjectReference;
  protected
  public
    property M_aRole: TBoldObjectReference read _Get_M_aRole;
    property M_B: TBoldObjectReference read _Get_M_B;
    property aRole: TA read _GetaRole;
    property B: TB read _GetB;
  end;

  TC = class(TBusinessClassesRoot)
  private
    function _Get_M_C1: TBAString;
    function _GetC1: String;
    procedure _SetC1(const NewValue: String);
    function _GetB: TB;
    function _Get_M_B: TBoldObjectReference;
    procedure _SetB(const value: TB);
  protected
  public
    property M_C1: TBAString read _Get_M_C1;
    property M_B: TBoldObjectReference read _Get_M_B;
    property C1: String read _GetC1 write _SetC1;
    property B: TB read _GetB write _SetB;
  end;

  TSubB = class(TB)
  private
    function _Get_M_SubB1: TBAString;
    function _GetSubB1: String;
    procedure _SetSubB1(const NewValue: String);
  protected
  public
    property M_SubB1: TBAString read _Get_M_SubB1;
    property SubB1: String read _GetSubB1 write _SetSubB1;
  end;

  TCSub = class(TC)
  private
    function _Get_M_CSub1: TBAString;
    function _GetCSub1: String;
    procedure _SetCSub1(const NewValue: String);
  protected
  public
    property M_CSub1: TBAString read _Get_M_CSub1;
    property CSub1: String read _GetCSub1 write _SetCSub1;
  end;

  TCSubSub1 = class(TCSub)
  private
  protected
  public
  end;

  TCSubSub2 = class(TCSub)
  private
    function _Get_M_CSubSub1: TBAString;
    function _GetCSubSub1: String;
    procedure _SetCSubSub1(const NewValue: String);
  protected
  public
    property M_CSubSub1: TBAString read _Get_M_CSubSub1;
    property CSubSub1: String read _GetCSubSub1 write _SetCSubSub1;
  end;

{$IFDEF UseBoldListEnumerator}
  TBusinessClassesRootListEnumerator = class(TBoldObjectListEnumerator)
  public
    function GetCurrent: TBusinessClassesRoot;
    property Current: TBusinessClassesRoot read GetCurrent;
  end;
{$ENDIF UseBoldListEnumerator}

  TBusinessClassesRootList = class(TBoldObjectList)
  protected
    function GetBoldObject(index: Integer): TBusinessClassesRoot;
    procedure SetBoldObject(index: Integer; NewObject: TBusinessClassesRoot);
  public
{$IFDEF UseBoldListEnumerator}
    function GetEnumerator: TBusinessClassesRootListEnumerator;
{$ENDIF UseBoldListEnumerator}
    function Includes(anObject: TBusinessClassesRoot): Boolean;
    function IndexOf(anObject: TBusinessClassesRoot): Integer;
    procedure Add(NewObject: TBusinessClassesRoot);
    function AddNew: TBusinessClassesRoot;
    procedure Insert(index: Integer; NewObject: TBusinessClassesRoot);
    property BoldObjects[index: Integer]: TBusinessClassesRoot read GetBoldObject write SetBoldObject; default;
  end;

{$IFDEF UseBoldListEnumerator}
  TAListEnumerator = class(TBoldObjectListEnumerator)
  public
    function GetCurrent: TA;
    property Current: TA read GetCurrent;
  end;
{$ENDIF UseBoldListEnumerator}

  TAList = class(TBusinessClassesRootList)
  protected
    function GetBoldObject(index: Integer): TA;
    procedure SetBoldObject(index: Integer; NewObject: TA);
  public
{$IFDEF UseBoldListEnumerator}
    function GetEnumerator: TAListEnumerator;
{$ENDIF UseBoldListEnumerator}
    function Includes(anObject: TA): Boolean;
    function IndexOf(anObject: TA): Integer;
    procedure Add(NewObject: TA);
    function AddNew: TA;
    procedure Insert(index: Integer; NewObject: TA);
    property BoldObjects[index: Integer]: TA read GetBoldObject write SetBoldObject; default;
  end;

{$IFDEF UseBoldListEnumerator}
  TBListEnumerator = class(TBoldObjectListEnumerator)
  public
    function GetCurrent: TB;
    property Current: TB read GetCurrent;
  end;
{$ENDIF UseBoldListEnumerator}

  TBList = class(TBusinessClassesRootList)
  protected
    function GetBoldObject(index: Integer): TB;
    procedure SetBoldObject(index: Integer; NewObject: TB);
  public
{$IFDEF UseBoldListEnumerator}
    function GetEnumerator: TBListEnumerator;
{$ENDIF UseBoldListEnumerator}
    function Includes(anObject: TB): Boolean;
    function IndexOf(anObject: TB): Integer;
    procedure Add(NewObject: TB);
    function AddNew: TB;
    procedure Insert(index: Integer; NewObject: TB);
    property BoldObjects[index: Integer]: TB read GetBoldObject write SetBoldObject; default;
  end;

{$IFDEF UseBoldListEnumerator}
  TBaRoleListEnumerator = class(TBoldObjectListEnumerator)
  public
    function GetCurrent: TBaRole;
    property Current: TBaRole read GetCurrent;
  end;
{$ENDIF UseBoldListEnumerator}

  TBaRoleList = class(TBusinessClassesRootList)
  protected
    function GetBoldObject(index: Integer): TBaRole;
    procedure SetBoldObject(index: Integer; NewObject: TBaRole);
  public
{$IFDEF UseBoldListEnumerator}
    function GetEnumerator: TBaRoleListEnumerator;
{$ENDIF UseBoldListEnumerator}
    function Includes(anObject: TBaRole): Boolean;
    function IndexOf(anObject: TBaRole): Integer;
    procedure Add(NewObject: TBaRole);
    function AddNew: TBaRole;
    procedure Insert(index: Integer; NewObject: TBaRole);
    property BoldObjects[index: Integer]: TBaRole read GetBoldObject write SetBoldObject; default;
  end;

{$IFDEF UseBoldListEnumerator}
  TCListEnumerator = class(TBoldObjectListEnumerator)
  public
    function GetCurrent: TC;
    property Current: TC read GetCurrent;
  end;
{$ENDIF UseBoldListEnumerator}

  TCList = class(TBusinessClassesRootList)
  protected
    function GetBoldObject(index: Integer): TC;
    procedure SetBoldObject(index: Integer; NewObject: TC);
  public
{$IFDEF UseBoldListEnumerator}
    function GetEnumerator: TCListEnumerator;
{$ENDIF UseBoldListEnumerator}
    function Includes(anObject: TC): Boolean;
    function IndexOf(anObject: TC): Integer;
    procedure Add(NewObject: TC);
    function AddNew: TC;
    procedure Insert(index: Integer; NewObject: TC);
    property BoldObjects[index: Integer]: TC read GetBoldObject write SetBoldObject; default;
  end;

{$IFDEF UseBoldListEnumerator}
  TSubBListEnumerator = class(TBoldObjectListEnumerator)
  public
    function GetCurrent: TSubB;
    property Current: TSubB read GetCurrent;
  end;
{$ENDIF UseBoldListEnumerator}

  TSubBList = class(TBList)
  protected
    function GetBoldObject(index: Integer): TSubB;
    procedure SetBoldObject(index: Integer; NewObject: TSubB);
  public
{$IFDEF UseBoldListEnumerator}
    function GetEnumerator: TSubBListEnumerator;
{$ENDIF UseBoldListEnumerator}
    function Includes(anObject: TSubB): Boolean;
    function IndexOf(anObject: TSubB): Integer;
    procedure Add(NewObject: TSubB);
    function AddNew: TSubB;
    procedure Insert(index: Integer; NewObject: TSubB);
    property BoldObjects[index: Integer]: TSubB read GetBoldObject write SetBoldObject; default;
  end;

{$IFDEF UseBoldListEnumerator}
  TCSubListEnumerator = class(TBoldObjectListEnumerator)
  public
    function GetCurrent: TCSub;
    property Current: TCSub read GetCurrent;
  end;
{$ENDIF UseBoldListEnumerator}

  TCSubList = class(TCList)
  protected
    function GetBoldObject(index: Integer): TCSub;
    procedure SetBoldObject(index: Integer; NewObject: TCSub);
  public
{$IFDEF UseBoldListEnumerator}
    function GetEnumerator: TCSubListEnumerator;
{$ENDIF UseBoldListEnumerator}
    function Includes(anObject: TCSub): Boolean;
    function IndexOf(anObject: TCSub): Integer;
    procedure Add(NewObject: TCSub);
    function AddNew: TCSub;
    procedure Insert(index: Integer; NewObject: TCSub);
    property BoldObjects[index: Integer]: TCSub read GetBoldObject write SetBoldObject; default;
  end;

{$IFDEF UseBoldListEnumerator}
  TCSubSub1ListEnumerator = class(TBoldObjectListEnumerator)
  public
    function GetCurrent: TCSubSub1;
    property Current: TCSubSub1 read GetCurrent;
  end;
{$ENDIF UseBoldListEnumerator}

  TCSubSub1List = class(TCSubList)
  protected
    function GetBoldObject(index: Integer): TCSubSub1;
    procedure SetBoldObject(index: Integer; NewObject: TCSubSub1);
  public
{$IFDEF UseBoldListEnumerator}
    function GetEnumerator: TCSubSub1ListEnumerator;
{$ENDIF UseBoldListEnumerator}
    function Includes(anObject: TCSubSub1): Boolean;
    function IndexOf(anObject: TCSubSub1): Integer;
    procedure Add(NewObject: TCSubSub1);
    function AddNew: TCSubSub1;
    procedure Insert(index: Integer; NewObject: TCSubSub1);
    property BoldObjects[index: Integer]: TCSubSub1 read GetBoldObject write SetBoldObject; default;
  end;

{$IFDEF UseBoldListEnumerator}
  TCSubSub2ListEnumerator = class(TBoldObjectListEnumerator)
  public
    function GetCurrent: TCSubSub2;
    property Current: TCSubSub2 read GetCurrent;
  end;
{$ENDIF UseBoldListEnumerator}

  TCSubSub2List = class(TCSubList)
  protected
    function GetBoldObject(index: Integer): TCSubSub2;
    procedure SetBoldObject(index: Integer; NewObject: TCSubSub2);
  public
{$IFDEF UseBoldListEnumerator}
    function GetEnumerator: TCSubSub2ListEnumerator;
{$ENDIF UseBoldListEnumerator}
    function Includes(anObject: TCSubSub2): Boolean;
    function IndexOf(anObject: TCSubSub2): Integer;
    procedure Add(NewObject: TCSubSub2);
    function AddNew: TCSubSub2;
    procedure Insert(index: Integer; NewObject: TCSubSub2);
    property BoldObjects[index: Integer]: TCSubSub2 read GetBoldObject write SetBoldObject; default;
  end;

function GeneratedCodeCRC: String;

implementation

uses
  // implementation uses
  // implementation dependencies
  // other
  BoldGeneratedCodeDictionary;

{$ENDIF}