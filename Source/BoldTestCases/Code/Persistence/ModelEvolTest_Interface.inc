(************************************)
(*    This file is autogenerated    *)
(* Any manual changes will be LOST! *)
(************************************)

(* This file should be stored in the same directory    *)
(* as the form/datamodule with the corresponding model *)

// 

{ File generated  }

{$IFNDEF ModelEvolTest_Interface.inc}
{$DEFINE ModelEvolTest_Interface.inc}

{$IFNDEF ModelEvolTest_unitheader}
unit ModelEvolTest;
{$ENDIF}

interface

uses
  {InterfaceUses}
  {InterfaceDependancies}
  BoldAttributes,{Attribute classes}
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
  TAssoc = class;
  TAssocList = class;
  TChildMapped = class;
  TChildMappedList = class;
  TParent = class;
  TParentList = class;
  TChildA = class;
  TChildAList = class;
  TChildB = class;
  TChildBList = class;
  TParentMapped = class;
  TParentMappedList = class;

  TBusinessClassesRoot = class(TBoldObject)
  private
  protected
  public
  end;

  TAssoc = class(TBusinessClassesRoot)
  private
    function _GetChildMapped: TChildMapped;
    function _Get_M_ChildMapped: TBoldObjectReference;
    function _GetParentMapped: TParentMapped;
    function _Get_M_ParentMapped: TBoldObjectReference;
  protected
  public
    property M_ChildMapped: TBoldObjectReference read _Get_M_ChildMapped;
    property M_ParentMapped: TBoldObjectReference read _Get_M_ParentMapped;
    property ChildMapped: TChildMapped read _GetChildMapped;
    property ParentMapped: TParentMapped read _GetParentMapped;
  end;

  TChildMapped = class(TBusinessClassesRoot)
  private
    function _Get_M_name: TBAString;
    function _Getname: String;
    procedure _Setname(NewValue: String);
    function _GetParentMapped: TParentMappedList;
    function _GetAssoc: TAssocList;
  protected
  public
    property M_name: TBAString read _Get_M_name;
    property M_ParentMapped: TParentMappedList read _GetParentMapped;
    property M_Assoc: TAssocList read _GetAssoc;
    property name: String read _Getname write _Setname;
    property ParentMapped: TParentMappedList read _GetParentMapped;
    property Assoc: TAssocList read _GetAssoc;
  end;

  TParent = class(TBusinessClassesRoot)
  private
  protected
  public
  end;

  TChildA = class(TChildMapped)
  private
  protected
  public
  end;

  TChildB = class(TChildMapped)
  private
  protected
  public
  end;

  TParentMapped = class(TParent)
  private
    function _Get_M_name: TBAString;
    function _Getname: String;
    procedure _Setname(NewValue: String);
    function _GetChildMapped: TChildMappedList;
    function _GetAssoc: TAssocList;
  protected
  public
    property M_name: TBAString read _Get_M_name;
    property M_ChildMapped: TChildMappedList read _GetChildMapped;
    property M_Assoc: TAssocList read _GetAssoc;
    property name: String read _Getname write _Setname;
    property ChildMapped: TChildMappedList read _GetChildMapped;
    property Assoc: TAssocList read _GetAssoc;
  end;


  TBusinessClassesRootList = class(TBoldObjectList)
  protected
    function GetBoldObject(index: Integer): TBusinessClassesRoot;
    procedure SetBoldObject(index: Integer; NewObject: TBusinessClassesRoot);
  public
    function Includes(anObject: TBusinessClassesRoot): Boolean;
    function IndexOf(anObject: TBusinessClassesRoot): Integer;
    procedure Add(NewObject: TBusinessClassesRoot);
    procedure Insert(index: Integer; NewObject: TBusinessClassesRoot);
    property BoldObjects[index: Integer]: TBusinessClassesRoot read GetBoldObject write SetBoldObject; default;
  end;

  TAssocList = class(TBusinessClassesRootList)
  protected
    function GetBoldObject(index: Integer): TAssoc;
    procedure SetBoldObject(index: Integer; NewObject: TAssoc);
  public
    function Includes(anObject: TAssoc): Boolean;
    function IndexOf(anObject: TAssoc): Integer;
    procedure Add(NewObject: TAssoc);
    procedure Insert(index: Integer; NewObject: TAssoc);
    property BoldObjects[index: Integer]: TAssoc read GetBoldObject write SetBoldObject; default;
  end;

  TChildMappedList = class(TBusinessClassesRootList)
  protected
    function GetBoldObject(index: Integer): TChildMapped;
    procedure SetBoldObject(index: Integer; NewObject: TChildMapped);
  public
    function Includes(anObject: TChildMapped): Boolean;
    function IndexOf(anObject: TChildMapped): Integer;
    procedure Add(NewObject: TChildMapped);
    procedure Insert(index: Integer; NewObject: TChildMapped);
    property BoldObjects[index: Integer]: TChildMapped read GetBoldObject write SetBoldObject; default;
  end;

  TParentList = class(TBusinessClassesRootList)
  protected
    function GetBoldObject(index: Integer): TParent;
    procedure SetBoldObject(index: Integer; NewObject: TParent);
  public
    function Includes(anObject: TParent): Boolean;
    function IndexOf(anObject: TParent): Integer;
    procedure Add(NewObject: TParent);
    procedure Insert(index: Integer; NewObject: TParent);
    property BoldObjects[index: Integer]: TParent read GetBoldObject write SetBoldObject; default;
  end;

  TChildAList = class(TChildMappedList)
  protected
    function GetBoldObject(index: Integer): TChildA;
    procedure SetBoldObject(index: Integer; NewObject: TChildA);
  public
    function Includes(anObject: TChildA): Boolean;
    function IndexOf(anObject: TChildA): Integer;
    procedure Add(NewObject: TChildA);
    procedure Insert(index: Integer; NewObject: TChildA);
    property BoldObjects[index: Integer]: TChildA read GetBoldObject write SetBoldObject; default;
  end;

  TChildBList = class(TChildMappedList)
  protected
    function GetBoldObject(index: Integer): TChildB;
    procedure SetBoldObject(index: Integer; NewObject: TChildB);
  public
    function Includes(anObject: TChildB): Boolean;
    function IndexOf(anObject: TChildB): Integer;
    procedure Add(NewObject: TChildB);
    procedure Insert(index: Integer; NewObject: TChildB);
    property BoldObjects[index: Integer]: TChildB read GetBoldObject write SetBoldObject; default;
  end;

  TParentMappedList = class(TParentList)
  protected
    function GetBoldObject(index: Integer): TParentMapped;
    procedure SetBoldObject(index: Integer; NewObject: TParentMapped);
  public
    function Includes(anObject: TParentMapped): Boolean;
    function IndexOf(anObject: TParentMapped): Integer;
    procedure Add(NewObject: TParentMapped);
    procedure Insert(index: Integer; NewObject: TParentMapped);
    property BoldObjects[index: Integer]: TParentMapped read GetBoldObject write SetBoldObject; default;
  end;

function GeneratedCodeCRC: integer;

implementation

{$ENDIF}

 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 

 