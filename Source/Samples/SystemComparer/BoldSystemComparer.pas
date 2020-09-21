unit BoldSystemComparer;

interface

uses
  Classes,
  BoldSystem,
  BoldHandles;

type

  TBoldSystemComparer = class
  protected
    class function GetCorrespondingObject(LeftObject: TBoldObject; RightSystem: TBoldSystem): TBoldObject; virtual;
    class function CompareObjects(Left, Right: TBoldObject): string; virtual;
    class function CompareAttributes(Left, Right: TBoldAttribute): string; virtual;
    class function CompareMembers(Left, Right: TBoldMember): string; virtual;
    class function CompareObjectReferences(Left, Right: TBoldObjectReference): string; virtual;
    class function CompareObjectLists(Left, Right: TBoldObjectList): string; virtual;
    class function FullObjectName(BoldObject: TBoldObject): string; virtual;
    class function FullMemberName(BoldMember: TBoldMember): string; virtual;
    class function ObjectReferenceAsString(ObjectReference: TBoldObjectReference): string; virtual;
  public
    class function CompareSystems(Left, Right: TBoldSystem): string; virtual;
  end;


implementation

uses
  SysUtils,
  BoldUtils;


{ TBoldSystemComparer }

class function TBoldSystemComparer.CompareAttributes(Left,
  Right: TBoldAttribute): string;
begin
  if not Left.IsEqual(Right) then
    result := Format('Attributes differ: %s:''%s'' <> %s:''%s''', [
        FullMemberName(Left),
        Left.AsString,
        FullMemberName(right),
        Right.AsString]);
end;

class function TBoldSystemComparer.CompareMembers(Left,
  Right: TBoldMember): string;
begin
  if (Left is TBoldAttribute) and (Right is TBoldAttribute) then
    Result := CompareAttributes(TBoldAttribute(Left), TBoldAttribute(Right))
  else if (Left is TBoldObjectReference) and (Right is TBoldObjectReference) then
    Result := CompareObjectReferences(TBoldObjectReference(Left), TBoldObjectReference(Right))
  else if TBoldObjectList(Left).BoldRoleRTInfo.IsOrdered = TBoldObjectList(Left).BoldRoleRTInfo.IsOrdered then
    result := CompareObjectLists(TBoldObjectList(Left), TBoldObjectList(Right))
  else
    result := Format('members differ in type %s <> %s', [FullMemberName(Left), FullMemberName(Right)]);
end;

class function TBoldSystemComparer.CompareObjectLists(Left,
  Right: TBoldObjectList): string;
var
  i: integer;
  IndexOfCorresponding: integer;
  RightCopy: TBoldObjectList;
  Ordered: Boolean;
begin
 Ordered := Left.BoldRoleRTInfo.IsOrdered;
  if Left.Count <> Right.Count then
    Result := Format(
        'multilinks have different count : %s:%d <> %s:%d', [
        FullMemberName(Left),
        left.Count,
        FullMemberName(right),
        right.Count])
  else if Ordered then
  begin
    for i := 0 to Left.Count-1 do
      if GetCorrespondingObject(Left[i], Right.BoldSystem) <> Right[i] then
      begin
        Result :=  Format(
          'multilinks differ in position %d : %s:%s <> %s:%s', [
          i,
          FullMemberName(Left),
          FullObjectName(Left[i]),
          FullMemberName(Right),
          FullObjectName(Right[i])]);
        Exit;
      end;
   end
   else
   begin
    RightCopy := TBoldObjectList.Create;
    try
      RightCopy.AddList(Right);
      for i := 0 to Left.Count-1 do
      begin
        IndexOfCorresponding := RightCopy.IndexOf(GetCorrespondingObject(Left[i], Right.BoldSystem));
        if IndexOfCorresponding <> -1 then
          RightCopy.RemoveByIndex(IndexOfCorresponding)
        else
        begin
          Result :=  Format(
            'Position %d in left not found in right : %s[%d]:%s no in %s', [
            i, i,
            FullMemberName(Left),
            FullObjectName(Left[i]),
            FullMemberName(Right),
            FullObjectName(Right[i])]);
          Exit;
        end;
      end;
    finally
      RightCopy.Free;
    end;
  end;
end;

class function TBoldSystemComparer.CompareObjectReferences(Left,
  Right: TBoldObjectReference): string;
begin
  if GetCorrespondingObject(Left.BoldObject, right.BoldSystem) <> Right.BoldObject then
    Result := Format(
        'Single links differ: %s:''%s'' <> %s:''%s''', [
        FullMemberName(Left),
        ObjectReferenceAsString(Left),
        FullMemberName(right),
        ObjectReferenceAsString(Right)]);
end;

class function TBoldSystemComparer.CompareObjects(Left,
  Right: TBoldObject): string;
var
  i: integer;
begin
  if Left.BoldClassTypeInfo.ExpressionName <> right.BoldClassTypeInfo.ExpressionName then
    Result := Format(
        'Objects have different ClassName : %s:%d <> %s:%d', [
        FullObjectName(Left),
        Left.BoldClassTypeInfo.ExpressionName,
        FullObjectName(right),
        right.BoldClassTypeInfo.ExpressionName])
  else if Left.BoldMemberCount <> right.BoldMemberCount then
    Result := Format(
        'Objects have different membercount : %s:%d <> %s:%d', [
        FullObjectName(Left),
        left.BoldMemberCount,
        FullObjectName(right),
        right.BoldMemberCount])
   else
   begin
     for i := 0 to Left.BoldMemberCount-1 do
     begin
       result := CompareMembers(Left.BoldMembers[i], Right.BoldMembers[i]);
       if Result <> ' ' then
         Break;
     end;
   end;
end;

class function TBoldSystemComparer.CompareSystems(Left,
  Right: TBoldSystem): string;
var
  i: integer;
  IndexOfCorresponding: integer;
  RightCopy: TBoldObjectList;
begin
  if Left.Classes[0].Count <> Right.Classes[0].Count then
    Result := Format(
        'Systems have different number of objects : %d <> %d', [
        Left.Classes[0].Count,
        Right.Classes[0].Count]
        )
   else
   begin
    RightCopy := TBoldObjectList.Create;
    try
      RightCopy.AddList(Right.Classes[0]);
      for i := 0 to Left.Classes[0].Count-1 do
      begin
        IndexOfCorresponding := RightCopy.IndexOf(GetCorrespondingObject(Left.Classes[0][i], Right));
        if IndexOfCorresponding <> -1 then
        begin
          Result := Compareobjects(Left.Classes[0][i], Left.Classes[0][IndexOfCorresponding]);
          RightCopy.RemoveByIndex(IndexOfCorresponding)
        end
        else
          Result :=  Format(
            'Object %s in left not found in right' , [FullObjectName(Left.Classes[0][i])]);
       if result <> '' then
         Exit;
      end;
    finally
      RightCopy.Free;
    end;
  end;
end;

class function TBoldSystemComparer.FullMemberName(
  BoldMember: TBoldMember): string;
begin
  if not Assigned(BoldMember) then
    result := 'nil'
  else
    Result := Format('%s.%s', [FullObjectName(BoldMember.OwningObject), BoldMember.BoldMemberRTInfo.expressionname]);
end;

class function TBoldSystemComparer.FullObjectName(
  BoldObject: TBoldObject): string;
begin
  if not Assigned(BoldObject) then
    result := 'nil'
  else
    Result := Format('%s:%s', [BoldObject.BoldClassTypeInfo.expressionname, BoldObject.BoldObjectLocator.AsString]);
end;

class function TBoldSystemComparer.GetCorrespondingObject(
  LeftObject: TBoldObject; RightSystem: TBoldSystem): TBoldObject;
begin
  if not Assigned(LeftObject) then
    Result := nil
  else
    Result := RightSystem.Locators.ObjectByID[LeftObject.BoldObjectLocator.BoldObjectID];
end;

class function TBoldSystemComparer.ObjectReferenceAsString(ObjectReference:
  TBoldObjectReference): string;
begin
  if Assigned(ObjectReference) then
    Result := FullObjectName(ObjectReference.BoldObject)
  else
    result := 'Nil'; //do not localize
end;

end.
