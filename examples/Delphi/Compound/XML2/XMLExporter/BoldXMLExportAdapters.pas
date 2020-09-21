// /usr/local/cvsroot/tp/boldfordelphi/examples/Delphi/Compound/XML2/XMLExporter/BoldXMLExportAdapters.pas,v 1.1 2003/01/21 07:23:54 jhogstrom Exp

{$Include Bold.inc}

unit BoldXMLExportAdapters;

interface

uses
  Classes,
  BoldXMLExportInterfaces,
  BoldBase,
  BoldSystemRT,
  BoldSystem,
  BoldAttributes,
  MSXML_TLB,
  BoldUMLModel,
  BoldManipulators;

type

  TBoldXMLExportObjectAdapter = class(TBoldRefCountedObject, IBoldXMLObject)
  private
    fObj: TBoldObject;
    function QualifiedClassName: string;
    function LocalId: string;
    function GetObject: TBoldObject;
  protected
    property Obj: TBoldObject read GetObject;
    function Attributes: IBoldXMLAttributeList; virtual;
    function References: IBoldXMLReferenceList; virtual;
  public
    constructor Create(Obj: TBoldObject);
    function BoldId(BoldManipulator: TBoldManipulator; Mapping: string = ''): string;
  end;

  TBoldXMLExportAttributeListAdapter = class(TBoldRefCountedObject, IBoldXMLAttributeList)
  private
    fAttrs: TList;
    function IsBoldAddedAttribute(Attr: TBoldMember): Boolean;
    //IBoldXMLAttributeList methods
    function Count: Integer;
    function GetAttrs: TList;
  protected
    property Attrs: TList read GetAttrs;
    function Attribute(index: Integer): IBoldXMLAttribute; virtual;
  public
    constructor Create(Obj: TBoldObject);
    destructor Destroy; override;
  end;

  TBoldXMLExportReferenceListAdapter = class(TBoldRefCountedObject, IBoldXMLReferenceList)
  private
    fRefs: TList;
    procedure GetMIReferences(List: TList; Obj1: TBoldObject; Obj2: TBoldObject);
    procedure GetReferences(List: TList; Obj: TBoldObject);
    function IsBoldAddedRole(Role: TBoldMember): Boolean;
    //IBoldXMLReferenceList methods
    function Count: Integer;
    function GetRefs: TList;
  protected
    property Refs: TList read GetRefs;
    function Reference(index: Integer): IBoldXMLReference; virtual;
  public
    constructor Create(Obj: TBoldObject);
    destructor Destroy; override;
  end;

  TBoldXMLExportAttributeAdapter = class(TBoldRefCountedObject, IBoldXMLAttribute)
  private
    fAttr: TBoldAttribute;
    function IsBoolean: Boolean;
    function IsEnum: Boolean;
    function IsDerived: Boolean;
    function IsObject: Boolean;
    function AsObject: IBoldXMLObject;
    function AsString: string;
    function AsBoolean: Boolean;
    function QualifiedName: string;
    function GetAttr: TBoldAttribute;
  protected
    property Attr: TBoldAttribute read GetAttr;
  public
    constructor Create(Attr: TBoldAttribute);
    function BoldId(BoldManipulator: TBoldManipulator; Mapping: string = ''): string;
  end;

  TBoldXMLExportReferenceAdapter = class(TBoldRefCountedObject, IBoldXMLReference)
  private
    fRef: TBoldMember;
    function IsMulti: Boolean;
    function IsComposite: Boolean;
    function IsDerived: Boolean;
    function QualifiedName: string;
    function GetRef: TBoldMember;
  protected
    property Ref: TBoldMember read GetRef;
    function SingleObject: IBoldXMLObject; virtual;
    function Objects: IBoldXMLObjectList; virtual;
  public
    constructor Create(Ref: TBoldMember);
  end;

  TBoldGenericExportObjectListAdapter = class(TBoldRefCountedObject, IBoldXMLObjectList)
  private
    fObjList: TBoldObjectList;
    function Count: Integer;
    function GetObjList: TBoldObjectList;
  protected
    property ObjList: TBoldObjectList read GetObjList;
    function GetObject(index: Integer): IBoldXMLObject; virtual;
  public
    destructor Destroy; override;
  end;

  TBoldXMLExportObjectListAdapter = class(TBoldGenericExportObjectListAdapter, IBoldXMLObjectList)
  protected
    function GetObject(index: Integer): IBoldXMLObject; override;
  public
    constructor Create(ObjList: TBoldObjectList);
  end;

  TBoldXMLExportMultipleInheritenceTVListAdapter = class(TBoldGenericExportObjectListAdapter, IBoldXMLObjectList)
  public
    constructor Create(TVList1, TVList2: TUMLTaggedValueList);
  end;

  TBoldXMLExportMergedObjectListAdapter = class(TBoldGenericExportObjectListAdapter, IBoldXMLObjectList)
  protected
    function GetObject(index: Integer): IBoldXMLObject; override;
  public
    constructor Create(ObjList1, ObjList2: TBoldObjectList);
  end;

implementation

uses
  SysUtils,
  BoldRev,
  BoldUtils,
  BoldDefs,
  BoldUMLTypes;

function IsAssociationClassClass(Obj: TBoldObject): Boolean;
begin
  result := (Obj.classtype = TUMLClass) and assigned(TUMLClass(Obj).association);
end;

function IsAssociationClassAssociation(Obj: TBoldObject): Boolean;
begin
  result := (Obj.classtype = TUMLAssociation) and assigned(TUMLAssociation(Obj).class_);
end;

function IsOfBoldAddedClass(Obj: TBoldObject): Boolean;
begin
  result := False;
end;

function GetOtherMIObj(Obj: TBoldObject): TBoldObject;
begin
  result := nil;
  if IsAssociationClassClass(Obj) then
    Result := TUMLClass(Obj).association;
  if IsAssociationClassAssociation(Obj) then
    Result := TUMLAssociation(Obj).class_;
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

function CreateObjectListAdapter(Ref: TBoldObjectList): IBoldXMLObjectList;
var
  Obj2: TBoldObject;
begin
  if IsDiamondInherited(Ref) then
  begin
    Obj2 := GetOtherMIObj(Ref.OwningObject);
    if Ref.BoldMemberRTInfo.ModelName = 'taggedValue' then
    begin
      result := TBoldXMLExportMultipleInheritenceTVListAdapter.Create(
          Ref as TUMLTaggedValueList, TUMLModelElement(Obj2).M_taggedValue);
    end
    else
    begin
      result := TBoldXMLExportMergedObjectListAdapter.Create(Ref, Obj2.BoldMembers[Ref.BoldMemberRTInfo.index] as TBoldObjectList);
    end;
  end
  else
    result := TBoldXMLExportObjectListAdapter.Create(TBoldObjectList(Ref));
end;

{ TBoldXMLExportObjectAdapter }

function TBoldXMLExportObjectAdapter.Attributes: IBoldXMLAttributeList;
begin
  result := TBoldXMLExportAttributeListAdapter.Create(fObj);
end;

function TBoldXMLExportObjectAdapter.BoldId(
  BoldManipulator: TBoldManipulator; Mapping: string): string;
begin
  Result := BoldManipulator.IdStringForElement(Obj, Mapping);
end;

constructor TBoldXMLExportObjectAdapter.Create(Obj: TBoldObject);
begin
  assert(assigned(Obj));
  assert(not IsOfBoldAddedClass(Obj));
  if IsAssociationClassAssociation(Obj) then
    fObj := TUMLAssociation(Obj).class_
  else
    fObj := Obj;
end;

function TBoldXMLExportObjectAdapter.GetObject: TBoldObject;
begin
  Result := fObj;
end;

function TBoldXMLExportObjectAdapter.LocalId: string;
begin
  result := fObj.BoldObjectLocator.AsString;
end;

function TBoldXMLExportObjectAdapter.QualifiedClassName: string;
begin
  if IsAssociationClassClass(fObj) then
    result := 'AssociationClass' // FIXME: Qualify
  else
    result := fObj.BoldClassTypeInfo.QualifiedName; // FIXME: qualify with packages
end;

function TBoldXMLExportObjectAdapter.References: IBoldXMLReferenceList;
begin
  result := TBoldXMLExportReferenceListAdapter.Create(fObj);
end;

{ TBoldXMLExportAttributeList }

function TBoldXMLExportAttributeListAdapter.Attribute(
  index: Integer): IBoldXMLAttribute;
var
  Attr: TBoldAttribute;
begin
  Attr := TBoldAttribute(fAttrs[index]);
  result := TBoldXMLExportAttributeAdapter.Create(Attr);
end;

function TBoldXMLExportAttributeListAdapter.Count: Integer;
begin
  result := fAttrs.Count;
end;

constructor TBoldXMLExportAttributeListAdapter.Create(Obj: TBoldObject);
var
  i: Integer;
begin
  fAttrs := TList.Create;
  for i := 0 to Obj.BoldMemberCount-1 do
    if Obj.BoldMembers[i].BoldMemberRTInfo.IsAttribute and
      not IsBoldAddedAttribute(Obj.BoldMembers[i]) then
      fAttrs.Add(Obj.BoldMembers[i]);
end;


destructor TBoldXMLExportAttributeListAdapter.Destroy;
begin
  FreeAndNil(fAttrs);
  inherited;
end;

function TBoldXMLExportAttributeListAdapter.GetAttrs: TList;
begin
  Result := fAttrs;
end;

function TBoldXMLExportAttributeListAdapter.IsBoldAddedAttribute(
  Attr: TBoldMember): Boolean;
begin
  result := false; // FIXME!!!
end;

{ TBoldXMLExportAttributeAdapter }

function TBoldXMLExportAttributeAdapter.AsBoolean: Boolean;
begin
  if not IsBoolean then
    raise EBoldInternal.CreateFmt('%s.AsBoolean: Attribute is not boolean', [classname]);

  result := TBABoolean(fAttr).AsBoolean;
end;

function TBoldXMLExportAttributeAdapter.AsObject: IBoldXMLObject;
begin
    raise EBoldInternal.CreateFmt('%s.AsObject: wrong type', [classname])
end;

function TBoldXMLExportAttributeAdapter.AsString: string;
begin
  result := fAttr.AsString;
end;

function TBoldXMLExportAttributeAdapter.BoldId(
  BoldManipulator: TBoldManipulator; Mapping: string): string;
begin
  Result := BoldManipulator.IdStringForElement(Attr, Mapping);
end;

constructor TBoldXMLExportAttributeAdapter.Create(Attr: TBoldAttribute);
begin
  inherited Create;
  fAttr := Attr;
end;

function TBoldXMLExportAttributeAdapter.GetAttr: TBoldAttribute;
begin
  Result := fAttr;
end;

function TBoldXMLExportAttributeAdapter.IsBoolean: Boolean;
begin
  result := fAttr is TBABoolean;
end;

function TBoldXMLExportAttributeAdapter.IsDerived: Boolean;
begin
  result := fAttr.BoldAttributeRTInfo.IsDerived;
end;

function TBoldXMLExportAttributeAdapter.IsEnum: Boolean;
begin
  result := (fAttr is TBAValueSet) and
            not (fAttr is TBABoolean);
end;

function TBoldXMLExportAttributeAdapter.IsObject: Boolean;
begin
  if fAttr.BoldType.ModelName = 'Multiplicity' then
    result := true
  else
    result := false;
end;

function TBoldXMLExportAttributeAdapter.QualifiedName: string;
begin
  result := fAttr.BoldAttributeRTInfo.ModelName; // FIXME: qualify
end;

{ TBoldXMLExportReferenceListAdapter }

function TBoldXMLExportReferenceListAdapter.Count: Integer;
begin
  result := fRefs.Count;
end;

constructor TBoldXMLExportReferenceListAdapter.Create(Obj: TBoldObject);
begin
  inherited Create;
  fRefs := TList.Create;
  if IsAssociationClassClass(Obj) then
    GetMIReferences(fRefs, TUMLClass(Obj).association, Obj)
  else
    GetReferences(fRefs, Obj);
end;


destructor TBoldXMLExportReferenceListAdapter.Destroy;
begin
  FreeAndNil(fRefs);
  inherited;
end;


procedure TBoldXMLExportReferenceListAdapter.GetMIReferences(List: TList;
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
      // The two halves should have the common part (diamond inheritence) first.
      if TBoldMember(List[i]).BoldMemberRTInfo.ModelName =
        TBoldMember(List2[i]).BoldMemberRTInfo.ModelName then // FIXME: use qualified names
      begin
        // Just keep the first one.
        // FIXME: Merge?
        // Note: Tagged Values will be handled by ObjectListAdapter-factory
      end else
        List.Add(List2[i]);
    end;
  finally
    List2.Free;
  end;
end;

procedure TBoldXMLExportReferenceListAdapter.GetReferences(List: TList;
  Obj: TBoldObject);
var
  i: Integer;
begin
  for i := 0 to Obj.BoldMemberCount-1 do
    if Obj.BoldMembers[i].BoldMemberRTInfo.IsRole and
      ((TBoldRoleRTInfo(Obj.BoldMembers[i].BoldMemberRTInfo).RoleType = rtRole) or
        ((TBoldRoleRTInfo(Obj.BoldMembers[i].BoldMemberRTInfo).RoleType = rtLinkRole) and ((TBoldRoleRTInfo(Obj.BoldMembers[i].BoldMemberRTInfo).ClassTypeInfoOfOtherEnd.AllMembersCount > 2)))
      ) and
      not IsBoldAddedRole(Obj.BoldMembers[i]) then
      List.Add(Obj.BoldMembers[i]);

end;

function TBoldXMLExportReferenceListAdapter.GetRefs: TList;
begin
  Result := fRefs;
end;

function TBoldXMLExportReferenceListAdapter.IsBoldAddedRole(
  Role: TBoldMember): Boolean;
begin
  result := false; // FIXME!!!
end;

function TBoldXMLExportReferenceListAdapter.Reference(
  index: Integer): IBoldXMLReference;
begin
  result := TBoldXMLExportReferenceAdapter.Create(TBoldMember(fRefs[index]));
end;

{ TBoldXMLExportReferenceAdapter }

constructor TBoldXMLExportReferenceAdapter.Create(Ref: TBoldMember);
begin
//  assert(Ref.BoldMemberRTInfo.IsRole);
  fRef := Ref;
end;

function TBoldXMLExportReferenceAdapter.GetRef: TBoldMember;
begin
  result := fRef;
end;

function TBoldXMLExportReferenceAdapter.IsComposite: Boolean;
begin
  result := TBoldRoleRTInfo(fRef.BoldMemberRTInfo).Aggregation = akComposite;
end;

function TBoldXMLExportReferenceAdapter.IsDerived: Boolean;
begin
  result := fRef.BoldMemberRTInfo.IsDerived;
end;

function TBoldXMLExportReferenceAdapter.IsMulti: Boolean;
begin
  result := fRef.BoldMemberRTInfo.IsMultiRole;
end;

function TBoldXMLExportReferenceAdapter.Objects: IBoldXMLObjectList;
begin
  if not fRef.BoldMemberRTInfo.IsMultiRole then
    raise EBoldInternal.CreateFmt('%s.Objects: Reference is not a multilink', [classname]);

  result := CreateObjectListAdapter(TBoldObjectList(fRef));
end;

function TBoldXMLExportReferenceAdapter.QualifiedName: string;
begin
  result := fRef.BoldMemberRTInfo.ModelName; // FIXME: qualify
end;

function TBoldXMLExportReferenceAdapter.SingleObject: IBoldXMLObject;
var
  anObj: TBoldObject;
begin
  if not fRef.BoldMemberRTInfo.IsSingleRole then
    raise EBoldInternal.CreateFmt('%.SingleObject: Reference is not a singlelink', [classname]);

  anObj := TBoldObjectReference(fRef).BoldObject;
  if assigned(anObj) and not IsOfBoldAddedClass(anObj) then
  begin
    result := TBoldXMLExportObjectAdapter.Create(anObj);
  end
  else
    result := nil;
end;

{ TBoldXMLExportObjectListAdapter }

constructor TBoldXMLExportObjectListAdapter.Create(ObjList: TBoldObjectList);
var
  i: Integer;
begin
  fObjList := ObjList.Clone as TBoldObjectList;
  for i := fObjList.Count-1 downto 0 do
    if IsAssociationClassAssociation(fObjList[i]) and
       fObjList.Includes(TUMLAssociation(fObjList [i]).class_) then
      fObjList.RemoveByIndex(i);
end;

function TBoldXMLExportObjectListAdapter.GetObject(
  index: Integer): IBoldXMLObject;
begin
  Result := inherited GetObject(index);
end;

{ TBoldXMLExportMultipleInheritenceTVListAdapter }
constructor TBoldXMLExportMultipleInheritenceTVListAdapter.Create(TVList1,
  TVList2: TUMLTaggedValueList);
var
  i: Integer;
  Element: TUMLModelElement;
begin
  fObjList := TVList1.Clone as TBoldObjectList;
  Element := TUMLModelElement(TVList1.OwningObject);
  for i := 0 to TVList2.Count-1 do
    if not assigned(Element.taggedValue[TVList2[i].tag]) then
      fObjList.Add(TVList2[i]);
end;

{ TBoldXMLExportMergedObjectListAdapter }

constructor TBoldXMLExportMergedObjectListAdapter.Create(ObjList1,
  ObjList2: TBoldObjectList);
var
  i: Integer;
begin
  fObjList := ObjList1.Clone as TBoldObjectList;
  for i := 0 to ObjList2.Count-1 do
    if not fObjList.Includes(ObjList2[i]) then
      fObjList.Add(ObjList2[i]);
end;

function TBoldXMLExportMergedObjectListAdapter.GetObject(
  index: Integer): IBoldXMLObject;
begin
  Result := inherited GetObject(index);
end;

{ TBoldGenericExportObjectListAdapter }

function TBoldGenericExportObjectListAdapter.Count: Integer;
begin
  result := fObjList.Count;
end;


destructor TBoldGenericExportObjectListAdapter.Destroy;
begin
  FreeAndNil(fObjList);
  inherited;
end;

function TBoldGenericExportObjectListAdapter.GetObject(
  index: Integer): IBoldXMLObject;
begin
  result := TBoldXMLExportObjectAdapter.Create(fObjList[index]);
end;

function TBoldGenericExportObjectListAdapter.GetObjList: TBoldObjectList;
begin
  Result := fObjList;
end;

initialization
  BoldRegisterModuleVersion('$Workfile: BoldXMLExportAdapters.pas $ 1.1 2003/01/21 07:23:54');

end.
