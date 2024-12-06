{ Global compiler directives }
{$include bold.inc}
unit BoldSystemComparer;

interface

uses
  Classes,
  BoldSystem,
  BoldElements,
  BoldDomainElement,
  BoldHandles;

type
  TBoldSystemComparer = class;

  TOnDifferenceEvent = function(Sender: TBoldSystemComparer; ALeft, ARight: TBoldDomainElement; AMessage: string; AAditionalElement: TBoldDomainElement = nil): boolean of object;
  TGetCorrespondingObjectEvent = function(Sender: TBoldSystemComparer; LeftObject: TBoldObject; RightSystem: TBoldSystem): TBoldObject of object;
  TCompareOptions = set of (coDerived, // derived members
                            coTransient, // transient members
                            coTransientObjects, // transient objects
                            coCompareObjectTimestamp //  Object timestamp
                            {coIgnoreCurrentTime});

  TBoldSystemComparer = class
  private
    fOnDifference: TOnDifferenceEvent;
    fOptions: TCompareOptions;
    fGetCorrespondingObjectEvent: TGetCorrespondingObjectEvent;
    fMillisecondRounding: integer;
  protected
    function GetCorrespondingObject(LeftObject: TBoldObject; RightSystem: TBoldSystem): TBoldObject; virtual;
    function FullObjectName(BoldObject: TBoldObject): string; virtual;
    function FullMemberName(BoldMember: TBoldMember): string; virtual;
    function ObjectReferenceAsString(ObjectReference: TBoldObjectReference): string; virtual;
    function DoOnDifference(ALeft, ARight: TBoldDomainElement; AMessage: string; AAditionalElement: TBoldDomainElement = nil): boolean;
  public
    function CompareSystems(Left, Right: TBoldSystem): string; virtual;
    function CompareObjects(Left, Right: TBoldObject): string; virtual;
    function CompareAttributes(Left, Right: TBoldAttribute): string; virtual;
    function CompareMembers(Left, Right: TBoldMember): string; virtual;
    function CompareObjectReferences(Left, Right: TBoldObjectReference): string; virtual;
    function CompareObjectLists(Left, Right: TBoldObjectList): string; virtual;
    property OnDifference: TOnDifferenceEvent read fOnDifference write fOnDifference;
    property Options: TCompareOptions read fOptions write fOptions;
    property OnGetCorrespondingObject: TGetCorrespondingObjectEvent read fGetCorrespondingObjectEvent write fGetCorrespondingObjectEvent;
    property MillisecondRounding: integer read fMillisecondRounding write fMillisecondRounding default 0;
  end;

implementation

uses
  SysUtils,
  DateUtils,

  BoldUtils,
  BoldAttributes;

{ TBoldSystemComparer }

function TBoldSystemComparer.CompareAttributes(Left,
  Right: TBoldAttribute): string;
begin
  if not Left.IsEqual(Right) then
  begin
    if (Left is TBAMoment) and (Right is TBAMoment) and not Left.IsNull and not Right.IsNull and
      (MilliSecondsBetween(TBAMoment(Left).AsDateTime, TBAMoment(Right).AsDateTime) <= MillisecondRounding) then
      begin
        // Difference less than MillisecondRounding is ignored.
        result := '';
        exit;
      end;
    result := Format('Attributes differ: %s:''%s'' <> %s:''%s''', [
        FullMemberName(Left),
        Left.AsString,
        FullMemberName(right),
        Right.AsString]);
    if DoOnDifference(Left, Right, result) then
      result := '';
  end;
end;

function TBoldSystemComparer.CompareMembers(Left,
  Right: TBoldMember): string;
begin
  result := '';
  if not (coDerived in Options) and left.Derived then
    exit;
  if not (coTransient in Options) and not left.BoldPersistent then
    exit;
  if (Left is TBoldAttribute) and (Right is TBoldAttribute) then
    Result := CompareAttributes(TBoldAttribute(Left), TBoldAttribute(Right))
  else if (Left is TBoldObjectReference) and (Right is TBoldObjectReference) then
    Result := CompareObjectReferences(TBoldObjectReference(Left), TBoldObjectReference(Right))
  else if TBoldObjectList(Left).BoldRoleRTInfo.IsOrdered = TBoldObjectList(Left).BoldRoleRTInfo.IsOrdered then
    result := CompareObjectLists(TBoldObjectList(Left), TBoldObjectList(Right))
  else
  begin
    result := Format('members differ in type %s <> %s', [FullMemberName(Left), FullMemberName(Right)]);
    if DoOnDifference(Left, Right, result) then
      result := '';
  end;
end;

function TBoldSystemComparer.CompareObjectLists(Left,
  Right: TBoldObjectList): string;
var
  i: integer;
  IndexOfCorresponding: integer;
  RightCopy: TBoldObjectList;
  Ordered: Boolean;
begin
  Ordered := Left.BoldRoleRTInfo.IsOrdered;
  Left.EnsureObjects;
{  if Left.Count <> Right.Count then
    Result := Format(
        'multilinks have different count : %s:%d <> %s:%d', [
        FullMemberName(Left),
        left.Count,
        FullMemberName(right),
        right.Count])
  else} if Left.Empty then
    exit
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
        if not DoOnDifference(Left, Right, result, Left[i]) then
          break;
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
            i,
            FullMemberName(Left),
            i,
            FullObjectName(Left[i]),
            FullMemberName(Right)]);
            if not DoOnDifference(Left, Right, result, Left[i]) then
              break;
        end;
      end;
    finally
      RightCopy.Free;
    end;
  end;
end;

function TBoldSystemComparer.CompareObjectReferences(Left,
  Right: TBoldObjectReference): string;
var
  CorrespondingObject: TBoldObject;
begin
  CorrespondingObject := GetCorrespondingObject(Left.BoldObject, right.BoldSystem);
  if CorrespondingObject <> Right.BoldObject then
    Result := Format(
        'Single links differ: %s:''%s'' <> %s:''%s''', [
        FullMemberName(Left),
        ObjectReferenceAsString(Left),
        FullMemberName(right),
        ObjectReferenceAsString(Right)]);
  if (result <> '') and DoOnDifference(Left, Right, result) then
    result := '';
end;

function TBoldSystemComparer.CompareObjects(Left,
  Right: TBoldObject): string;
var
  i: integer;
begin
  Result := '';
  if not Left.BoldClassTypeInfo.BoldIsA(right.BoldClassTypeInfo) then
  begin
    Result := Format(
        'Objects have different ClassName : %s:%d <> %s:%d', [
        FullObjectName(Left),
        Left.BoldClassTypeInfo.ExpressionName,
        FullObjectName(right),
        right.BoldClassTypeInfo.ExpressionName]);
    if (result <> '') and DoOnDifference(Left, Right, result) then
      result := '';
  end
  else if Left.BoldMemberCount <> right.BoldMemberCount then
  begin
    Result := Format(
        'Objects have different membercount : %s:%d <> %s:%d', [
        FullObjectName(Left),
        left.BoldMemberCount,
        FullObjectName(right),
        right.BoldMemberCount]);
    if (result <> '') and DoOnDifference(Left, Right, result) then
      result := '';
  end
  else
  begin
    if (coCompareObjectTimestamp in Options) and
      (left.AsIBoldObjectContents[bdepContents].TimeStamp <> right.AsIBoldObjectContents[bdepContents].TimeStamp) then
      begin
        result := Format('Left %s Timestamp: %d <> Right %s Timestamp: %d', [Left.DebugInfo, left.AsIBoldObjectContents[bdepContents].TimeStamp, right.DebugInfo, right.AsIBoldObjectContents[bdepContents].TimeStamp]);
        if (result <> '') and DoOnDifference(Left, Right, result) then
          result := '';
      end
      else
      for i := 0 to Left.BoldMemberCount-1 do
      begin
        result := CompareMembers(Left.BoldMembers[i], Right.BoldMembers[i]);
      end;
  end;
end;

function TBoldSystemComparer.CompareSystems(Left,
  Right: TBoldSystem): string;
var
  i: integer;
  LeftObject, RightObject: TBoldObject;
begin
  if Left.Classes[0].Count <> Right.Classes[0].Count then
    Result := Format(
        'Systems have different number of objects : %d <> %d', [
        Left.Classes[0].Count,
        Right.Classes[0].Count]
        )
   else
   begin
    Left.Classes[0].EnsureObjects;
    Right.Classes[0].EnsureObjects;
{$IFDEF FetchFromClassList}
    for i := 0 to Left.BoldSystemTypeInfo.TopSortedClasses.Count -1 do
      Left.Classes[i].EnsureObjects;
    for i := 0 to Right.BoldSystemTypeInfo.TopSortedClasses.Count -1 do
      Right.Classes[i].EnsureObjects;
{$ENDIF}
    for i := 0 to Left.Classes[0].Count-1 do
    begin
      LeftObject := Left.Classes[0][i];
      if not (coTransientObjects in Options) and not LeftObject.BoldPersistent then
       continue; // skip transient objects
      RightObject := Right.Locators.ObjectByID[LeftObject.BoldObjectLocator.BoldObjectID];
      if Assigned(RightObject) then
        Result := Compareobjects(LeftObject, RightObject)
      else
        Result :=  Format(
          'Object %s in left not found in right' , [FullObjectName(Left.Classes[0][i])]);
     if result <> '' then
       break;
    end;
  end;
  if (result <> '') and DoOnDifference(Left, Right, result) then
    result := '';
end;

function TBoldSystemComparer.DoOnDifference(ALeft, ARight: TBoldDomainElement;
  AMessage: string; AAditionalElement: TBoldDomainElement = nil): boolean;
begin
  result := false;
  if Assigned(fOnDifference) then
    result := fOnDifference(self, ALeft, ARight, AMessage, AAditionalElement);
end;

function TBoldSystemComparer.FullMemberName(
  BoldMember: TBoldMember): string;
begin
  if not Assigned(BoldMember) then
    result := 'nil'
  else
    Result := Format('%s.%s', [FullObjectName(BoldMember.OwningObject), BoldMember.BoldMemberRTInfo.expressionname]);
end;

function TBoldSystemComparer.FullObjectName(
  BoldObject: TBoldObject): string;
begin
  if not Assigned(BoldObject) then
    result := 'nil'
  else
    Result := Format('%s:%s', [BoldObject.BoldClassTypeInfo.expressionname, BoldObject.BoldObjectLocator.AsString]);
end;

function TBoldSystemComparer.GetCorrespondingObject(
  LeftObject: TBoldObject; RightSystem: TBoldSystem): TBoldObject;
begin
  if not Assigned(LeftObject) then
    Result := nil
  else
  begin
    if Assigned(fGetCorrespondingObjectEvent) then
      Result := OnGetCorrespondingObject(self, LeftObject, RightSystem)
    else
      Result := RightSystem.Locators.ObjectByID[LeftObject.BoldObjectLocator.BoldObjectID];
  end;
end;

function TBoldSystemComparer.ObjectReferenceAsString(ObjectReference:
  TBoldObjectReference): string;
begin
  if Assigned(ObjectReference) then
    Result := FullObjectName(ObjectReference.BoldObject)
  else
    result := 'Nil';
end;

end.