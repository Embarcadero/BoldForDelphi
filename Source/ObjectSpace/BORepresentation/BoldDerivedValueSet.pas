
{ Global compiler directives }
{$include bold.inc}
unit BoldDerivedValueSet;

interface

uses
  BoldSystem,
  BoldAttributes;

type
  { forward declarations }
  TBADerivedValueSetValueList = class;

  { TBADerivedValueSetValueList }
  TBADerivedValueSetValueList = class(TBAValueSetValueList)
  protected
    procedure AddMembers(Int: Integer; Members: Array of TBoldMember); virtual;
  public
    constructor create(System: TBoldSystem; ClassToFollow, IntValueAttribute: String; StrValueAttributes: Array of String);
  end;

implementation

uses
  SysUtils,
  BoldDefs,
  BoldSystemRT;

procedure TBADerivedValueSetValueList.AddMembers(Int: Integer; Members: Array of TBoldMember);
var
  DynArr: Array of String;
  i: integer;
begin
  SetLength(DynArr, High(Members) + 1);
  for i := 0 to High(Members) do
      DynArr[i] := Members[i].AsString;
  add(Int, DynArr);
end;

constructor TBADerivedValueSetValueList.create(System: TBoldSystem; ClassToFollow, IntValueAttribute: String; StrValueAttributes: Array of String);
var
  ObjectList: TBoldObjectList;
  i, IntIndex: integer;
  ClassTypeInfo: TBoldClassTypeInfo;
  RTAttr: TBoldMemberRTInfo;
  j: integer;
  DynArr: Array of TBoldMember;
  IndexArr: Array of integer;
begin
  inherited create;

  ClassTypeInfo := System.BoldSystemTypeInfo.ClassTypeInfoByExpressionName[ClassToFollow];
  if not assigned(ClassTypeInfo) then
    raise Ebold.createFmt('%s.create: No class called %s', [ClassName, ClassToFollow]);

  RTAttr := ClassTypeInfo.MemberRTInfoByExpressionName[IntValueAttribute];
  if not assigned(RTAttr) then
    raise Ebold.createFmt('%s.create: No attribute called %s.%s', [ClassName, ClassToFollow, IntValueAttribute]);
  if not RTAttr.MemberClass.InheritsFrom(TBAInteger) then
    raise Ebold.createFmt('%s.create: %s.%s is not an integer', [ClassName, ClassToFollow, IntValueAttribute]);

  IntIndex := RTAttr.Index;

  SetLength(DynArr, High(StrValueAttributes) + 1);
  SetLength(IndexArr, High(StrValueAttributes) + 1);

  for i := 0 to High(StrValueAttributes) do
  begin
    RTAttr := ClassTypeInfo.MemberRTInfoByExpressionName[StrValueAttributes[i]];
    if not assigned(RTAttr) then
      raise Ebold.createFmt('%s.create: No attribute called %s.%s', [ClassName, ClassToFollow, StrValueAttributes[i]]);
    if not RTAttr.MemberClass.InheritsFrom(TBoldAttribute) then
      raise Ebold.createFmt('%s.create: %s.%s is not an Attribute', [ClassName, ClassToFollow, StrValueAttributes[i]]);

    IndexArr[i] := RTAttr.Index;
  end;

  ObjectList := System.Classes[ClassTypeInfo.TopSortedIndex];
  if objectlist.count = 0 then
    raise EBold.CreateFmt('%s.Create: No values in valuesetlist: %s', [Classname, ClassTypeInfo.ExpressionName]);
  for i := 0 to ObjectList.count - 1 do
  begin
    for j := 0 to High(StrValueAttributes) do
      DynArr[j] := ObjectList[i].BoldMembers[IndexArr[j]];
      AddMembers((ObjectList[i].BoldMembers[IntIndex] as TBAInteger).AsInteger, DynArr);
  end;
end;

initialization

end.
