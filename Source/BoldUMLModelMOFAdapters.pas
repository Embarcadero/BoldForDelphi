
/////////////////////////////////////////////////////////
//                                                     //
//              Bold for Delphi                        //
//    Copyright (c) 2002 BoldSoft AB, Sweden           //
//                                                     //
/////////////////////////////////////////////////////////

{ Global compiler directives }
{$include bold.inc}
unit BoldUMLModelMOFAdapters;

interface

uses
  Classes,
  BoldMOFInterfaces,
  BoldBase,
  BoldSystemRT,
  BoldSystem,
  BoldAttributes,
  BoldUMLXMILink,
  {$IFDEF OXML}OXmlPDOM{$ELSE}Bold_MSXML_TLB{$ENDIF},
  BoldUMLModel;

type
  TBoldUMLMOFObjectAdapter = class(TBoldRefCountedObject, IBoldMOFObject)
  private
    fObj: TBoldObject;
    fOwningLink: TBoldUMLXMILink;
    function LocalId: string;
  protected
    function QualifiedClassName: string; virtual;  
    function Attributes: IBoldMOFAttributeList; virtual;
    function References: IBoldMOFReferenceList; virtual;
    property Obj: TBoldObject read fObj;
  public
    constructor Create(Obj: TBoldObject; OwningLink: TBoldUMLXMILink);
  end;

  TBoldUMLMOFAttributeListAdapter = class(TBoldRefCountedObject, IBoldMOFAttributeList)
  private
    fOwningLink: TBoldUMLXMILink;
    fAttrs: TList;
    function IsBoldAddedAttribute(Attr: TBoldMember): Boolean;
    {IBoldMOFAttributeList methods}
    function Count: Integer;
  protected
    function Attribute(index: Integer): IBoldMOFAttribute; virtual;
    property Attrs: TList read fAttrs;
  public
    constructor Create(Obj: TBoldObject; OwningLink: TBoldUMLXMILink);
    destructor Destroy; override;
  end;

  TBoldUMLMOFReferenceListAdapter = class(TBoldRefCountedObject, IBoldMOFReferenceList)
  private
    fOwningLink: TBoldUMLXMILink;
    fRefs: TList;
    procedure GetMIReferences(List: TList; Obj1: TBoldObject; Obj2: TBoldObject);
    procedure GetReferences(List: TList; Obj: TBoldObject);
    function IsBoldAddedRole(Role: TBoldMember): Boolean;
    {IBoldMOFReferenceList methods}
    function Count: Integer;
  protected
    function Reference(index: Integer): IBoldMOFReference; virtual;
    property Refs: TList read fRefs;
  public
    constructor Create(Obj: TBoldObject; OwningLink: TBoldUMLXMILink);
    destructor Destroy; override;
  end;

  TBoldUMLMOFAttributeAdapter = class(TBoldRefCountedObject, IBoldMOFAttribute)
  private
    fOwningLink: TBoldUMLXMILink;
    fAttr: TBoldAttribute;
    function IsBoolean: Boolean;
    function IsEnum: Boolean;
    function IsDerived: Boolean;
    function IsObject: Boolean;
    function AsObject: IBoldMOFObject;
    function AsBoolean: Boolean;
  protected
    function AsString: string; virtual;
    function QualifiedName: string; virtual;
    property Attr: TBoldAttribute read fAttr;
  public
    constructor Create(Attr: TBoldAttribute; OwningLink: TBoldUMLXMILink);
  end;

  TBoldUMLMOFReferenceAdapter = class(TBoldRefCountedObject, IBoldMOFReference)
  private
    fOwningLink: TBoldUMLXMILink;
    fRef: TBoldMember;
    function IsMulti: Boolean;
    function IsComposite: Boolean;
    function IsDerived: Boolean;
  protected
    function QualifiedName: string; virtual;
    function SingleObject: IBoldMOFObject; virtual;
    function Objects: IBoldMOFObjectList; virtual;
    property Ref: TBoldMember read fRef;
  public
    constructor Create(Ref: TBoldMember; OwningLink: TBoldUMLXMILink);
  end;

  TBoldGenericMOFObjectListAdapter = class(TBoldRefCountedObject, IBoldMOFObjectList)
  private
    fOwningLink: TBoldUMLXMILink;
    fObjList: TBoldObjectList;
    function Count: Integer;
  protected
    function GetObject(index: Integer): IBoldMOFObject; virtual;
    property ObjList: TBoldObjectList read fObjList;
  public
    destructor Destroy; override;
  end;

  TBoldUMLMOFObjectListAdapter = class(TBoldGenericMOFObjectListAdapter, IBoldMOFObjectList)
  public
    constructor Create(ObjList: TBoldObjectList; OwningLink: TBoldUMLXMILink);
  end;

  TBoldUMLMOFMultipleInheritenceTVListAdapter = class(TBoldGenericMOFObjectListAdapter, IBoldMOFObjectList)
  public
    constructor Create(TVList1, TVList2: TUMLTaggedValueList; OwningLink: TBoldUMLXMILink);
  end;

  TBoldUMLMOFMergedObjectListAdapter = class(TBoldGenericMOFObjectListAdapter, IBoldMOFObjectList)
  public
    constructor Create(ObjList1, ObjList2: TBoldObjectList; OwningLink: TBoldUMLXMILink);
  end;

  TBoldUMLMOFMultiplicityAdapter = class(TBoldRefCountedObject, IBoldMOFObject,
                                         IBoldMOFReferenceList, IBoldMOFReference,
                                         IBoldMOFObjectList, IBoldMOFAttributeList)
    function IBoldMOFObjectList.Count = ObjectCount;
    function IBoldMOFAttributeList.Count = AttributeCount;
  private
    fOwningLink: TBoldUMLXMILink;
    fMultAttr: TBoldAttribute;
    fRanges: TStringList;
    { IBoldMOFObject }
    function Attributes: IBoldMOFAttributeList;
    function References: IBoldMOFReferenceList;
    function QualifiedClassName: string;
    function LocalId: string;
    { IBoldMOFAttributeList}
    function AttributeCount: Integer;
    function Attribute(index: Integer): IBoldMOFAttribute;
    { IBoldMOFReferenceList }
    function Count: Integer;
    function Reference(index: Integer): IBoldMOFReference;
    { IBoldMOFReference }
    function IsMulti: Boolean;
    function IsComposite: Boolean;
    function IsDerived: Boolean;
    function SingleObject: IBoldMOFObject;
    function Objects: IBoldMOFObjectList;
    function QualifiedName: string;
    { IBoldMOFObjectList }
    function ObjectCount: Integer;
    function GetObject(index: Integer): IBoldMOFObject;
  public
    constructor Create(Mult: TBoldAttribute; OwningLink: TBoldUMLXMILink);
    destructor Destroy; override;
  end;

  TBoldUMLMOFMultiplicityRangeAdapter = class(TBoldRefCountedObject, IBoldMOFObject,
                                              IBoldMOFAttributeList, IBoldMOFReferenceList)
    function IBoldMOFReferenceList.Count = RefCount;
  private
    fOwningLink: TBoldUMLXMILink;
    fLower: Integer;
    fUpper: Integer;
    { IBoldMOFObject }
    function Attributes: IBoldMOFAttributeList;
    function References: IBoldMOFReferenceList;
    function QualifiedClassName: string;
    function LocalId: string;
    { IBoldMOFAttributeList }
    function Count: Integer;
    function Attribute(index: Integer): IBoldMOFAttribute;
    { IBoldMOFReferenceList }
    function RefCount: Integer;
    function Reference(index: Integer): IBoldMOFReference;
  public
    constructor Create(Range: string; OwningLink: TBoldUMLXMILink);
  end;

  TBoldUMLMOFMultiplicityRangeBoundAdapter = class(TBoldRefCountedObject, IBoldMOFAttribute)
  private
    fAttrName: string;
    fValue: Integer;
    { IBoldMOFAttribute }
    function IsBoolean: Boolean;
    function IsEnum: Boolean;
    function IsDerived: Boolean;
    function IsObject: Boolean;
    function AsObject: IBoldMOFObject;
    function AsString: string;
    function AsBoolean: Boolean;
    function QualifiedName: string;
  public
    constructor Create(AttrName: string; Value: Integer);
  end;


  TBoldUMLMOFExpressionAdapter = class(TBoldRefCountedObject, IBoldMOFObject,
                                       IBoldMOFReferenceList,
                                       IBoldMOFAttributeList, IBoldMOFAttribute)
    function IBoldMOFAttributeList.Count = AttributeCount;
  private
    fOwningLink: TBoldUMLXMILink;
    fExprAttr: TBoldAttribute;
    { IBoldMOFObject }
    function Attributes: IBoldMOFAttributeList;
    function References: IBoldMOFReferenceList;
    function QualifiedClassName: string;
    function LocalId: string;
    { IBoldMOFAttributeList}
    function AttributeCount: Integer;
    function Attribute(index: Integer): IBoldMOFAttribute;
    { IBoldMOFReferenceList }
    function Count: Integer;
    function Reference(index: Integer): IBoldMOFReference;
    { IBoldMOFAttribute }
    function IsBoolean: Boolean;
    function IsEnum: Boolean;
    function IsDerived: Boolean;
    function IsObject: Boolean;
    function AsObject: IBoldMOFObject;
    function AsString: string;
    function AsBoolean: Boolean;
    function QualifiedName: string;
  public
    constructor Create(Expr: TBoldAttribute; OwningLink: TBoldUMLXMILink);
  end;

  TBoldUMLMOFDummyAttrAdapter = class(TBoldRefCountedObject, IBoldMOFAttribute)
  private
    fQualifiedName: string;
    { IBoldMOFAttribute }
    function IsBoolean: Boolean;
    function IsEnum: Boolean;
    function IsDerived: Boolean;
    function IsObject: Boolean;
    function AsObject: IBoldMOFObject;
    function AsString: string;
    function AsBoolean: Boolean;
    function QualifiedName: string;
  public
    constructor Create(QualifiedName: string);
  end;

  function GetOtherMIObj(Obj: TBoldObject): TBoldObject;
  function IsDiamondInherited(Member: TBoldMember): Boolean;
  function IsOfBoldAddedClass(Obj: TBoldObject): Boolean;  
  
implementation

uses
  SysUtils,
  BoldDefs,
  BoldUMLXMICommon,
  BoldUMLTypes;

function IsMultiplicity(Attr: TBoldAttribute): Boolean;
begin
  result := Attr.BoldType.ModelName = 'Multiplicity';
end;

function IsExpression(Attr: TBoldAttribute): Boolean;
begin
  result := IsNameOfExpressionClass(Attr.BoldType.ModelName);
end;

function IsLinkObjectObject(Obj: TBoldObject): Boolean;
begin
  result := (Obj.ClassType = TUMLObject) and assigned(TUMLObject(Obj).link);
end;

function IsLinkObjectLink(Obj: TBoldObject): Boolean;
begin
  result := (Obj.classtype = TUMLLink) and assigned(TUMLLink(Obj).xobject);
end;

function IsAssociationClassClass(Obj: TBoldObject): Boolean;
begin
  result := (Obj.classtype = TUMLClass) and assigned(TUMLClass(Obj).association);
end;

function IsAssociationClassAssociation(Obj: TBoldObject): Boolean;
begin
  result := (Obj.classtype = TUMLAssociation) and assigned(TUMLAssociation(Obj).class_);
end;

function IsAssociationButNotAssociationClass(Obj: TBoldObject): Boolean;
begin
  result := (Obj.classtype = TUMLAssociation) and not assigned(TUMLAssociation(Obj).class_);
end;

function IsOfBoldAddedClass(Obj: TBoldObject): Boolean;
begin
  result := Obj.BoldClassTypeInfo.Stereotype = 'Bold';
end;

function GetOtherMIObj(Obj: TBoldObject): TBoldObject;
begin
  result := nil;
  if IsAssociationClassClass(Obj) then
    Result := TUMLClass(Obj).association
  else if IsAssociationClassAssociation(Obj) then
    Result := TUMLAssociation(Obj).class_
  else if IsLinkObjectObject(Obj) then
    result := TUMLObject(Obj).link
  else if IsLinkObjectLink(Obj) then
    result := TUMLLink(Obj).xobject;
end;

function IsDiamondInherited(Member: TBoldMember): Boolean;
var
  ThisObj: TBoldObject;
  OtherObj: TBoldObject;
begin
  ThisObj := Member.OwningObject;
  OtherObj := GetOtherMIObj(ThisObj);
  if assigned(OtherObj) then
    result := Member.BoldMemberRTInfo.index <
      ThisObj.BoldClassTypeInfo.LeastCommonSuperClass(OtherObj.BoldClassTypeInfo).AllMembersCount
  else
    result := false;
end;

function CreateObjectListAdapter(Ref: TBoldObjectList; OwningLink: TBoldUMLXMILink): IBoldMOFObjectList;
var
  Obj2: TBoldObject;
begin
  if IsDiamondInherited(Ref) then
  begin
    Obj2 := GetOtherMIObj(Ref.OwningObject);
    if Ref.BoldMemberRTInfo.ModelName = 'taggedValue' then
    begin
      result := TBoldUMLMOFMultipleInheritenceTVListAdapter.Create(
          Ref as TUMLTaggedValueList, TUMLModelElement(Obj2).M_taggedValue, OwningLink);
    end
    else
    begin
      result := TBoldUMLMOFMergedObjectListAdapter.Create(Ref, Obj2.BoldMembers[Ref.BoldMemberRTInfo.index] as TBoldObjectList, OwningLink);
    end;
  end
  else
    result := TBoldUMLMOFObjectListAdapter.Create(TBoldObjectList(Ref), OwningLink);
end;

function IsInheritedButShouldNotBe(Member: TBoldMember): Boolean;
begin
  if (IsAssociationButNotAssociationClass(Member.OwningObject) or
      (Member.OwningObject.ClassType = TUMLStereotype)) and
    (FindDefiningClass(Member.BoldMemberRTInfo).ModelName = 'Namespace') then
    result := true
  else
    result := false;
end;

{ TBoldUMLMOFObjectAdapter }

function TBoldUMLMOFObjectAdapter.Attributes: IBoldMOFAttributeList;
begin
  result := TBoldUMLMOFAttributeListAdapter.Create(fObj, fOwningLink);
end;

constructor TBoldUMLMOFObjectAdapter.Create(Obj: TBoldObject; OwningLink: TBoldUMLXMILink);
begin
  assert(assigned(Obj));
  assert(not IsOfBoldAddedClass(Obj));
  fOwningLink := OwningLink;
  if IsAssociationClassAssociation(Obj) then
    fObj := TUMLAssociation(Obj).class_
  else if IsLinkObjectLink(Obj) then
    fObj := TUMLLink(Obj).xobject
  else
    fObj := Obj;
end;

function TBoldUMLMOFObjectAdapter.LocalId: string;
begin
  result := 'x' + fObj.BoldObjectLocator.AsString;
end;

function TBoldUMLMOFObjectAdapter.QualifiedClassName: string;
begin
  if IsAssociationClassClass(fObj) then
    result := 'Foundation.Core.AssociationClass'
  else if IsLinkObjectObject(fObj) then
    result := 'Behavioral_Elements.Common_Behavior.LinkObject'
  else
    result := RemoveBoldPackageNames(fObj.BoldClassTypeInfo.QualifiedName);
end;

function TBoldUMLMOFObjectAdapter.References: IBoldMOFReferenceList;
begin
  result := TBoldUMLMOFReferenceListAdapter.Create(fObj, fOwningLink);
end;

{ TBoldUMLMOFAttributeList }

function TBoldUMLMOFAttributeListAdapter.Attribute(
  index: Integer): IBoldMOFAttribute;
var
  Attr: TBoldAttribute;
begin
  Attr := TBoldAttribute(fAttrs[index]);
  result := TBoldUMLMOFAttributeAdapter.Create(Attr, fOwningLink);
end;

function TBoldUMLMOFAttributeListAdapter.Count: Integer;
begin
  result := fAttrs.Count;
end;

constructor TBoldUMLMOFAttributeListAdapter.Create(Obj: TBoldObject; OwningLink: TBoldUMLXMILink);
var
  i: Integer;
begin
  fOwningLink := OwningLink;
  fAttrs := TList.Create;
  for i := 0 to Obj.BoldMemberCount-1 do
    if Obj.BoldMembers[i].BoldMemberRTInfo.IsAttribute and
      not IsBoldAddedAttribute(Obj.BoldMembers[i]) then
      fAttrs.Add(Obj.BoldMembers[i]);
end;


destructor TBoldUMLMOFAttributeListAdapter.Destroy;
begin
  FreeAndNil(fAttrs);
  inherited;
end;

function TBoldUMLMOFAttributeListAdapter.IsBoldAddedAttribute(
  Attr: TBoldMember): Boolean;
begin
  result := (Attr.BoldMemberRTInfo.Stereotype = 'Bold') or
            IsInheritedButShouldNotBe(Attr);
end;

{ TBoldUMLMOFAttributeAdapter }

function TBoldUMLMOFAttributeAdapter.AsBoolean: Boolean;
begin
  if not IsBoolean then
    raise EBoldInternal.CreateFmt('%s.AsBoolean: Attribute is not boolean', [classname]);

  result := TBABoolean(fAttr).AsBoolean;
end;

function TBoldUMLMOFAttributeAdapter.AsObject: IBoldMOFObject;
begin
  if IsMultiplicity(fAttr) then
    result := TBoldUMLMOFMultiplicityAdapter.Create(fAttr, fOwningLink)
  else if IsExpression(fAttr) then
    result := TBoldUMLMOFExpressionAdapter.Create(fAttr, fOwningLink)
  else
    raise EBoldInternal.CreateFmt('%s.AsObject: wrong type', [classname])
end;

function TBoldUMLMOFAttributeAdapter.AsString: string;
var
  dotpos: Integer;
begin
  if fOwningLink.TranslateRoseTaggedValues and
    (fAttr.DisplayName = 'UMLTaggedValue.tag') and
    (pos('.', fAttr.AsString) <> 0) then
  begin
    dotpos := pos('.', fAttr.AsString);
    result := 'RationalRose$' + Copy(fAttr.AsString, 1, dotpos-1) + ':' +
              Copy(fAttr.AsString, dotpos+1, MAXINT);
  end
  else
    result := fAttr.AsString;
end;

constructor TBoldUMLMOFAttributeAdapter.Create(Attr: TBoldAttribute; OwningLink: TBoldUMLXMILink);
begin
  inherited Create;
  fOwningLink := OwningLink;
  fAttr := Attr;
end;

function TBoldUMLMOFAttributeAdapter.IsBoolean: Boolean;
begin
  result := fAttr is TBABoolean;
end;

function TBoldUMLMOFAttributeAdapter.IsDerived: Boolean;
begin
  result := fAttr.BoldAttributeRTInfo.IsDerived;
end;

function TBoldUMLMOFAttributeAdapter.IsEnum: Boolean;
begin
  result := (fAttr is TBAValueSet) and
            not (fAttr is TBABoolean);
end;

function TBoldUMLMOFAttributeAdapter.IsObject: Boolean;
begin
  if IsMultiplicity(fAttr) or
    IsExpression(fAttr) then
    result := true
  else
    result := false;
end;

function TBoldUMLMOFAttributeAdapter.QualifiedName: string;
begin
  result := QualifiedMemberName(fAttr.BoldMemberRTInfo);
end;

{ TBoldUMLMOFReferenceListAdapter }

function TBoldUMLMOFReferenceListAdapter.Count: Integer;
begin
  result := fRefs.Count;
end;

constructor TBoldUMLMOFReferenceListAdapter.Create(Obj: TBoldObject; OwningLink: TBoldUMLXMILink);
begin
  inherited Create;
  fOwningLink := OwningLink;
  fRefs := TList.Create;
  if IsAssociationClassClass(Obj) then
    GetMIReferences(fRefs, TUMLClass(Obj).association, Obj)
  else if IsLinkObjectObject(Obj) then
    GetMIReferences(fRefs, TUMLObject(Obj).link, Obj)
  else
    GetReferences(fRefs, Obj);
end;


destructor TBoldUMLMOFReferenceListAdapter.Destroy;
begin
  FreeAndNil(fRefs);
  inherited;
end;


procedure TBoldUMLMOFReferenceListAdapter.GetMIReferences(List: TList;
  Obj1, Obj2: TBoldObject);
var
  List2: TList;
  i: Integer;
begin
  List2 := TList.Create;
  try
    GetReferences(List, Obj1);
    GetReferences(List2, Obj2);
    for i := 0 to List2.Count-1 do
    begin
      if TBoldMember(List[i]).BoldMemberRTInfo.ModelName =
        TBoldMember(List2[i]).BoldMemberRTInfo.ModelName then
      begin


      end else
        List.Add(List2[i]);
    end;
  finally
    List2.Free;
  end;
end;

procedure TBoldUMLMOFReferenceListAdapter.GetReferences(List: TList;
  Obj: TBoldObject);
var
  i: Integer;
begin
  for i := 0 to Obj.BoldMemberCount-1 do
    if Obj.BoldMembers[i].BoldMemberRTInfo.IsRole and
      (TBoldRoleRTInfo(Obj.BoldMembers[i].BoldMemberRTInfo).RoleType = rtRole) and
      (TBoldRoleRTInfo(Obj.BoldMembers[i].BoldMemberRTInfo).IsNavigable) and
      not IsBoldAddedRole(Obj.BoldMembers[i]) then
      List.Add(Obj.BoldMembers[i]);
end;

function TBoldUMLMOFReferenceListAdapter.IsBoldAddedRole(
  Role: TBoldMember): Boolean;
var
  RoleRt: TBoldRoleRtInfo;
begin
  assert(Role.BoldMemberRTInfo.IsRole);
  RoleRt := TBoldRoleRTInfo(Role.BoldMemberRTInfo);
  Result := (RoleRt.Stereotype = 'Bold') or
    (RoleRt.AssociationStereotype = 'Bold') or
    (RoleRt.ClassTypeInfoOfOtherEnd.Stereotype = 'Bold') or
    ((RoleRt.ModelName = 'associationEnd') and (RoleRt.ClassTypeInfo.ModelName = 'Classifier')) or
    IsInheritedButShouldNotBe(Role);
end;

function TBoldUMLMOFReferenceListAdapter.Reference(
  index: Integer): IBoldMOFReference;
begin
  result := TBoldUMLMOFReferenceAdapter.Create(TBoldMember(fRefs[index]), fOwningLink);
end;

{ TBoldUMLMOFReferenceAdapter }

constructor TBoldUMLMOFReferenceAdapter.Create(Ref: TBoldMember; OwningLink: TBoldUMLXMILink);
begin
  assert(Ref.BoldMemberRTInfo.IsRole);
  fOwningLink := OwningLink;
  fRef := Ref;
end;

function TBoldUMLMOFReferenceAdapter.IsComposite: Boolean;
begin
  result := TBoldRoleRTInfo(fRef.BoldMemberRTInfo).Aggregation = akComposite;
end;

function TBoldUMLMOFReferenceAdapter.IsDerived: Boolean;
begin
  result := fRef.BoldMemberRTInfo.IsDerived;
end;

function TBoldUMLMOFReferenceAdapter.IsMulti: Boolean;
begin
  result := fRef.BoldMemberRTInfo.IsMultiRole;
end;

function TBoldUMLMOFReferenceAdapter.Objects: IBoldMOFObjectList;
begin
  if not fRef.BoldMemberRTInfo.IsMultiRole then
    raise EBoldInternal.CreateFmt('%s.Objects: Reference is not a multilink', [classname]);

  result := CreateObjectListAdapter(TBoldObjectList(fRef), fOwningLink);
end;

function TBoldUMLMOFReferenceAdapter.QualifiedName: string;
begin
  result := QualifiedMemberName(fRef.BoldMemberRTInfo);
end;

function TBoldUMLMOFReferenceAdapter.SingleObject: IBoldMOFObject;
var
  anObj: TBoldObject;
begin
  if not fRef.BoldMemberRTInfo.IsSingleRole then
    raise EBoldInternal.CreateFmt('%.SingleObject: Reference is not a singlelink', [classname]);

  anObj := TBoldObjectReference(fRef).BoldObject;
  if assigned(anObj) and not IsOfBoldAddedClass(anObj) then
  begin
    result := TBoldUMLMOFObjectAdapter.Create(anObj, fOwningLink);
  end
  else
    result := nil;
end;

{ TBoldUMLMOFObjectListAdapter }

constructor TBoldUMLMOFObjectListAdapter.Create(ObjList: TBoldObjectList; OwningLink: TBoldUMLXMILink);
var
  i: Integer;
begin
  fOwningLink := OwningLink;
  fObjList := ObjList.Clone as TBoldObjectList;
  for i := fObjList.Count-1 downto 0 do
    if (IsAssociationClassAssociation(fObjList[i]) and
        fObjList.Includes(TUMLAssociation(fObjList[i]).class_)) or
       (IsLinkObjectLink(fObjList[i]) and
        fObjList.Includes(TUMLLink(fObjList[i]).xobject)) then
      fObjList.RemoveByIndex(i);
end;

{ TBoldUMLMOFMultipleInheritenceTVListAdapter }
constructor TBoldUMLMOFMultipleInheritenceTVListAdapter.Create(TVList1,
  TVList2: TUMLTaggedValueList; OwningLink: TBoldUMLXMILink);
var
  i: Integer;
  Element: TUMLModelElement;
begin
  fOwningLink := OwningLink;
  fObjList := TVList1.Clone as TBoldObjectList;
  Element := TUMLModelElement(TVList1.OwningObject);
  for i := 0 to TVList2.Count-1 do
    if not assigned(Element.taggedValue[TVList2[i].tag]) then
      fObjList.Add(TVList2[i]);
end;

{ TBoldUMLMOFMergedObjectListAdapter }

constructor TBoldUMLMOFMergedObjectListAdapter.Create(ObjList1,
  ObjList2: TBoldObjectList; OwningLink: TBoldUMLXMILink);
var
  i: Integer;
begin
  fOwningLink := OwningLink;
  fObjList := ObjList1.Clone as TBoldObjectList;
  for i := 0 to ObjList2.Count-1 do
    if not fObjList.Includes(ObjList2[i]) then
      fObjList.Add(ObjList2[i]);
end;

{ TBoldGenericMOFObjectListAdapter }

function TBoldGenericMOFObjectListAdapter.Count: Integer;
begin
  result := fObjList.Count;
end;

destructor TBoldGenericMOFObjectListAdapter.Destroy;
begin
  FreeAndNil(fObjList);
  inherited;
end;

function TBoldGenericMOFObjectListAdapter.GetObject(
  index: Integer): IBoldMOFObject;
begin
  result := TBoldUMLMOFObjectAdapter.Create(fObjList[index], fOwningLink);
end;


{ TBoldUMLMOFMultiplicityAdapter }

function TBoldUMLMOFMultiplicityAdapter.Attribute(
  index: Integer): IBoldMOFAttribute;
begin
  raise EBoldInternal.CreateFmt('%s.Attribute: Multiplicity has no attibutes', [classname]);
end;

function TBoldUMLMOFMultiplicityAdapter.AttributeCount: Integer;
begin
  result := 0;
end;

function TBoldUMLMOFMultiplicityAdapter.Attributes: IBoldMOFAttributeList;
begin
  result := self;
end;

function TBoldUMLMOFMultiplicityAdapter.Count: Integer;
begin
  result := 1;
end;

constructor TBoldUMLMOFMultiplicityAdapter.Create(Mult: TBoldAttribute; OwningLink: TBoldUMLXMILink);
begin
  fOwningLink := OwningLink;
  fMultAttr := Mult;
  fRanges := TStringList.Create;
  fRanges.CommaText := Mult.AsString;
end;

destructor TBoldUMLMOFMultiplicityAdapter.Destroy;
begin
  FreeAndNil(fRanges);
  inherited;
end;

function TBoldUMLMOFMultiplicityAdapter.GetObject(
  index: Integer): IBoldMOFObject;
begin
  result := TBoldUMLMOFMultiplicityRangeAdapter.Create(fRanges[index], fOwningLink);
end;

function TBoldUMLMOFMultiplicityAdapter.IsComposite: Boolean;
begin
  result := true;
end;

function TBoldUMLMOFMultiplicityAdapter.IsDerived: Boolean;
begin
  result := false; 
end;

function TBoldUMLMOFMultiplicityAdapter.IsMulti: Boolean;
begin
  result := true;
end;

function TBoldUMLMOFMultiplicityAdapter.LocalId: string;
begin
  raise EBoldInternal.CreateFmt('%s.LocalId: Multiplicity can only be embedded', [classname]);
end;

function TBoldUMLMOFMultiplicityAdapter.ObjectCount: Integer;
begin
  result := fRanges.Count;
end;

function TBoldUMLMOFMultiplicityAdapter.Objects: IBoldMOFObjectList;
begin
  result := self;
end;

function TBoldUMLMOFMultiplicityAdapter.QualifiedClassName: string;
begin
  result := 'Foundation.Data_Types.Multiplicity';
end;

function TBoldUMLMOFMultiplicityAdapter.QualifiedName: string;
begin
  result := 'Foundation.Data_Types.Multiplicity.range';
end;

function TBoldUMLMOFMultiplicityAdapter.Reference(
  index: Integer): IBoldMOFReference;
begin
  assert(Index = 0);
  result := self;
end;

function TBoldUMLMOFMultiplicityAdapter.References: IBoldMOFReferenceList;
begin
  result := self;
end;

function TBoldUMLMOFMultiplicityAdapter.SingleObject: IBoldMOFObject;
begin
  raise EBoldInternal.CreateFmt('%s.SingleObject: Multiplicity.ranges is multi', [classname]);
end;

{ TBoldUMLMOFMultiplicityRangeAdapter }

function TBoldUMLMOFMultiplicityRangeAdapter.Attribute(
  index: Integer): IBoldMOFAttribute;
begin
  if index = 0 then
    result := TBoldUMLMOFMultiplicityRangeBoundAdapter.Create('lower', fLower)
  else if index = 1 then
    result := TBoldUMLMOFMultiplicityRangeBoundAdapter.Create('upper', fUpper)
  else
    raise EBoldInternal.CreateFmt('%s.Attribute: index out of bounds %d', [classname, index]);
end;

function TBoldUMLMOFMultiplicityRangeAdapter.Attributes: IBoldMOFAttributeList;
begin
  result := self;
end;

function TBoldUMLMOFMultiplicityRangeAdapter.Count: Integer;
begin
  result := 2;
end;

constructor TBoldUMLMOFMultiplicityRangeAdapter.Create(Range: string; OwningLink: TBoldUMLXMILink);
var
  splitpos: Integer;
  Upper: string;
begin
  fOwningLink := OwningLink;
  splitpos := pos('..', Range);
  if splitpos = 0 then
  begin
    if Range = '*' then
    begin
      fLower := 0;
      fUpper := -1;
    end else
    begin
      fLower := StrToInt(Range);
      fUpper := fLower;
    end;  
  end
  else
  begin
    fLower := StrToInt(Copy(Range, 1, splitpos-1));
    Upper := Copy(Range, splitpos+2, maxint);
    if Upper = '*' then
      fUpper := -1
    else
      fUpper := StrToInt(Upper);
  end;
end;

function TBoldUMLMOFMultiplicityRangeAdapter.LocalId: string;
begin
  result := '';
end;

function TBoldUMLMOFMultiplicityRangeAdapter.QualifiedClassName: string;
begin
  result := 'Foundation.Data_Types.MultiplicityRange';
end;

function TBoldUMLMOFMultiplicityRangeAdapter.RefCount: Integer;
begin
  result := 0;
end;

function TBoldUMLMOFMultiplicityRangeAdapter.Reference(
  index: Integer): IBoldMOFReference;
begin
  raise EBoldInternal.CreateFmt('%s.Reference: MultiplicityRange has no references', [classname]);
end;

function TBoldUMLMOFMultiplicityRangeAdapter.References: IBoldMOFReferenceList;
begin
  result := self;
end;

{ TBoldUMLMOFMultiplicityRangeBoundAdapter }

function TBoldUMLMOFMultiplicityRangeBoundAdapter.AsBoolean: Boolean;
begin
  raise EBoldInternal.CreateFmt('%s.AsBoolean: Is not boolean', [classname]);
end;

function TBoldUMLMOFMultiplicityRangeBoundAdapter.AsObject: IBoldMOFObject;
begin
  raise EBoldInternal.CreateFmt('%s.AsObject: Not an object', [classname]);
end;

function TBoldUMLMOFMultiplicityRangeBoundAdapter.AsString: string;
begin
  result := IntToStr(fValue);
end;

constructor TBoldUMLMOFMultiplicityRangeBoundAdapter.Create(
  AttrName: string; Value: Integer);
begin
  fAttrName := AttrName;
  fValue := Value;
end;

function TBoldUMLMOFMultiplicityRangeBoundAdapter.IsBoolean: Boolean;
begin
  result := false;
end;

function TBoldUMLMOFMultiplicityRangeBoundAdapter.IsDerived: Boolean;
begin
  result := false;
end;

function TBoldUMLMOFMultiplicityRangeBoundAdapter.IsEnum: Boolean;
begin
  result := false;
end;

function TBoldUMLMOFMultiplicityRangeBoundAdapter.IsObject: Boolean;
begin
  result := false;
end;

function TBoldUMLMOFMultiplicityRangeBoundAdapter.QualifiedName: string;
begin
  result := 'Foundation.Data_Types.MultiplicityRange.' + fAttrName;
end;

{ TBoldUMLMOFExpressionAdapter }

function TBoldUMLMOFExpressionAdapter.AsBoolean: Boolean;
begin
  raise EBoldInternal.CreateFmt('%s.AsBoolean: Expression.body is not a boolean', [classname]);
end;

function TBoldUMLMOFExpressionAdapter.AsObject: IBoldMOFObject;
begin
  raise EBoldInternal.CreateFmt('%s.AsObject: Expression.body is not an object', [classname]);
end;

function TBoldUMLMOFExpressionAdapter.AsString: string;
begin
  result := fExprAttr.AsString;
end;

function TBoldUMLMOFExpressionAdapter.Attribute(
  index: Integer): IBoldMOFAttribute;
begin
  if index = 0 then
    result := TBoldUMLMOFDummyAttrAdapter.Create('Foundation.Data_Types.Expression.language')
  else if index = 1 then
    result := self
  else
    raise EBoldInternal.CreateFmt('%s.Attribute: index out of bounds', [classname]);
end;

function TBoldUMLMOFExpressionAdapter.AttributeCount: Integer;
begin
  result := 2;
end;

function TBoldUMLMOFExpressionAdapter.Attributes: IBoldMOFAttributeList;
begin
  result := self;
end;

function TBoldUMLMOFExpressionAdapter.Count: Integer;
begin
  result := 0;
end;

constructor TBoldUMLMOFExpressionAdapter.Create(Expr: TBoldAttribute; OwningLink: TBoldUMLXMILink);
begin
  fOwningLink := OwningLink;
  fExprAttr := Expr;
end;

function TBoldUMLMOFExpressionAdapter.IsBoolean: Boolean;
begin
  result := false;
end;

function TBoldUMLMOFExpressionAdapter.IsDerived: Boolean;
begin
  result := false;
end;

function TBoldUMLMOFExpressionAdapter.IsEnum: Boolean;
begin
  result := false;
end;

function TBoldUMLMOFExpressionAdapter.IsObject: Boolean;
begin
  result := false;
end;

function TBoldUMLMOFExpressionAdapter.LocalId: string;
begin
  raise EBoldInternal.CreateFmt('%s.LocalId: Expression can only be embedded', [classname]);
end;

function TBoldUMLMOFExpressionAdapter.QualifiedClassName: string;
begin
  result := 'Foundation.Data_Types.' + fExprAttr.BoldType.ModelName;
end;

function TBoldUMLMOFExpressionAdapter.QualifiedName: string;
begin
  result := 'Foundation.Data_Types.Expression.body';
end;

function TBoldUMLMOFExpressionAdapter.Reference(
  index: Integer): IBoldMOFReference;
begin
  raise EBoldInternal.CreateFmt('%s.Reference: index out of bounds', [classname]);
end;

function TBoldUMLMOFExpressionAdapter.References: IBoldMOFReferenceList;
begin
  result := self;
end;

{ TBoldUMLMOFDummyAttrAdapter }

function TBoldUMLMOFDummyAttrAdapter.AsBoolean: Boolean;
begin
  raise EBoldInternal.CreateFmt('%s.AsBoolean: % is not a boolean', [classname, fQualifiedName]);
end;

function TBoldUMLMOFDummyAttrAdapter.AsObject: IBoldMOFObject;
begin
  raise EBoldInternal.CreateFmt('%s.AsBoolean: % is not an object', [classname, fQualifiedName]);
end;

function TBoldUMLMOFDummyAttrAdapter.AsString: string;
begin
  result := '';
end;

constructor TBoldUMLMOFDummyAttrAdapter.Create(QualifiedName: string);
begin
  fQualifiedName := QualifiedName;
end;

function TBoldUMLMOFDummyAttrAdapter.IsBoolean: Boolean;
begin
  result := false;
end;

function TBoldUMLMOFDummyAttrAdapter.IsDerived: Boolean;
begin
  result := false;
end;

function TBoldUMLMOFDummyAttrAdapter.IsEnum: Boolean;
begin
  result := false;
end;

function TBoldUMLMOFDummyAttrAdapter.IsObject: Boolean;
begin
  result := false;
end;

function TBoldUMLMOFDummyAttrAdapter.QualifiedName: string;
begin
  result := fQualifiedName;
end;

end.
