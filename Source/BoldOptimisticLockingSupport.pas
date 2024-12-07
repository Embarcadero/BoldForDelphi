{ Global compiler directives }
{$include bold.inc}
unit BoldOptimisticLockingSupport;

interface

uses
  BoldSystemRT,
  BoldDomainElement,
  BoldSystem,
  BoldLockRegions,
  BoldValueInterfaces,
  BoldValueSpaceInterfaces,
  BoldUpdatePrecondition,
  BoldId;

type
  TBoldOptimisticLockHandler = class(TBoldAbstractOptimisticLockHandler)
  private
    function GetRemotedfromIdRefValue(Value: IBoldValue): TBoldObjectId;
    procedure GetRegionsForRemoteMember(OwningId: TBoldObjectId; Regions: TBoldRegionLookup; OwningRoleRTInfo: TBoldRoleRTInfo);

    procedure GetRegionsForDirtyOtherEnds(ObjectList: TBoldObjectList; Regions: TBoldRegionLookup);
    procedure CopyValue(const TargetVS: IBoldValueSpace; var TargetObjectContents: IBoldObjectContents; TargetObjectId: TBoldObjectId; MemberIndex: integer; const Value: IBoldValue; const StreamName: String);
    procedure RetrieveOptimisticLockingvalues(ObjectList: TBoldObjectlist; PreCondition: TBoldOptimisticLockingPrecondition);
    procedure GetRegionsForElement(Element: TBoldDomainElement; Regions: TBoldRegionLookup);
    procedure GetRegionsForDirtyMembersInList(ObjectList: TBoldObjectList; Regions: TBoldRegionLookup);
    procedure GetRegionsForDirtyMembers(Obj: TBoldObject; Regions: TBoldRegionLookup);
    procedure GetLockingValuesForRegions(Regions: TBoldRegionLookup; PreCondition: TBoldOptimisticLockingPrecondition);
    procedure GetLockingValuesForRegion(Region: TBoldRegion; PreCondition: TBoldOptimisticLockingPrecondition);
    procedure AddOptimisticRegionLocks(ObjectList: TBoldObjectlist; PreCondition: TBoldOptimisticLockingPrecondition);
    procedure AddRegionObjectsToEnclosure(Region: TBoldRegion; Enclosure: TBoldObjectList; ValidateOnly: Boolean; var ListIsEnclosure: Boolean);
    procedure AddRegionsObjectsToEnclosure(Regions: TBoldRegionLookup; Enclosure: TBoldObjectList; ValidateOnly: Boolean; var ListIsEnclosure: Boolean);
    function GetRegionFactory: TBoldRegionFactory;
  public
    procedure AddOptimisticLocks(ObjectList: TBoldObjectlist; PreCondition: TBoldOptimisticLockingPrecondition); override;
    procedure EnsureEnclosure(Obj: TBoldObject; Enclosure: TBoldObjectList; ValidateOnly: Boolean; var ListIsEnclosure: Boolean); override;
    property RegionFactory: TBoldRegionFactory read GetRegionFactory;
  end;

implementation

uses
  SysUtils,
  Classes,

  BoldCoreConsts,
  BoldDefs,
  BoldIndexableList,
  BoldElements,
  BoldTaggedValueSupport,
  BoldGuard;

{ TBoldOptimisticLockHandler }


procedure TBoldOptimisticLockHandler.AddOptimisticLocks(ObjectList: TBoldObjectlist; PreCondition: TBoldOptimisticLockingPrecondition);
begin
  RetrieveOptimisticLockingvalues(Objectlist, Precondition);
  if assigned(System.RegionFactory) then
    AddOptimisticRegionLocks(ObjectList, Precondition);
end;

procedure TBoldOptimisticLockHandler.AddOptimisticRegionLocks(ObjectList: TBoldObjectlist; PreCondition: TBoldOptimisticLockingPrecondition);
var
  Regions: TBoldRegionLookup;
  RegionExpander: TBoldRegionExpander;
  Guard: IBoldGuard;
begin
  Guard := TBoldGuard.create(Regions, RegionExpander);
  Regions := TBoldRegionLookup.Create;
  RegionExpander := TBoldRegionExpander.Create;

  GetRegionsForDirtyOtherEnds(ObjectList, Regions);
  GetRegionsForDirtyMembersInList(ObjectList, Regions);
  RegionExpander.ExpandRegionEnclosure(Regions);
  GetLockingValuesForRegions(Regions, PreCondition);
end;

procedure TBoldOptimisticLockHandler.CopyValue(const TargetVS: IBoldValueSpace;
  var TargetObjectContents: IBoldObjectContents;
  TargetObjectId: TBoldObjectId; MemberIndex: integer; const Value: IBoldValue;
  const StreamName: String);
var
  MemberId: TBoldMemberId;
  NewValue: IBoldValue;
begin
  if not assigned(TargetObjectContents) then
    TargetObjectContents := TargetVS.EnsuredObjectContentsByObjectId[TargetObjectId];
  MemberId := TBoldMemberId.Create(MemberIndex);
  try
    TargetObjectContents.EnsureMember(MemberId, StreamName);
    NewValue := TargetObjectContents.ValueByIndex[MemberIndex];
    NewValue.AssignContent(Value);
    NewValue.BoldPersistenceState := bvpsCurrent;
  finally
    MemberId.Free;
  end;
end;

procedure TBoldOptimisticLockHandler.GetRegionsForDirtyOtherEnds(ObjectList: TBoldObjectList; Regions: TBoldRegionLookup);
var
  ObjIx, i: integer;
  Obj: TBoldObject;
  ObjRef: TBoldObjectReference;
  OldRemoteId: TBoldObjectId;
begin
  for ObjIx := 0 to ObjectList.Count-1 do
  begin
    Obj := ObjectList[ObjIx];
    with Obj.BoldClassTypeInfo.AllRoles do
    for i := 0 to Count-1 do
    if Items[i].IsSingleRole then
    begin
      ObjRef := Obj.BoldMemberIfAssigned[Items[i].index] as TBoldObjectReference;
      if Assigned(ObjRef) and ObjRef.BoldDirty then
      begin
        OldRemoteId := GetRemotedfromIdRefValue(Objref.OldValue);
        if assigned(OldRemoteId) then
          GetRegionsForRemoteMember(OldRemoteId, Regions, ObjRef.BoldRoleRTInfo);
        if assigned(ObjRef.Locator) then
          GetRegionsForRemoteMember(ObjRef.Locator.BoldobjectId, Regions, ObjRef.BoldRoleRTInfo);
      end;
    end;
  end;
end;

procedure TBoldOptimisticLockHandler.GetLockingValuesForRegion(Region: TBoldRegion; PreCondition: TBoldOptimisticLockingPrecondition);
var
  ElementList: TList;
  elem: TBoldElement;
  Obj: TBoldObject;
  Member: TBoldMember;
  ObjectId: TBoldObjectId;
  ObjectContents: IBoldObjectContents;
  i: integer;
  g: IBoldGuard;
begin
  g := TBoldGuard.Create(ElementList);
  ElementList := TList.Create;
  Region.GetElements(Elementlist);
  for i := 0 to ElementLIst.Count-1 do
  begin
    Elem := TBoldElement(ElementLIst[i]);
    if elem is TBoldObject then
    begin
      Obj := elem as TBoldObject;
      if not obj.BoldObjectIsNew then
      begin
        PreCondition.ValueSpace.EnsureObjectContents(Obj.BoldObjectLocator.BoldObjectId);
      end;
    end
    else if elem is TBoldmember then
    begin
      Member := elem as TBoldMember;
      ObjectContents := nil;
      if (not Member.OwningObject.BoldObjectIsNew) and
         Member.BoldMemberRTInfo.CanHaveOldValue and
         (Member.BoldPersistenceState <> bvpsInvalid) then
      begin
        ObjectId := Member.OwningObject.BoldObjectLocator.BoldObjectId;
        if Member.BoldMemberRTInfo.IsStoredInObject and (Member.OwningObject.BoldClassTypeInfo.OptimisticLocking = bolmTimestamp) then
        begin
          Precondition.ValueSpace.EnsuredObjectContentsByObjectId[ObjectId].TimeStamp :=
            Member.OwningObject.AsIBoldObjectContents[bdepContents].TimeStamp;
        end
        else if assigned(Member.OldValue) then
          CopyValue(PreCondition.ValueSpace, ObjectContents, ObjectId, Member.BoldMemberRTInfo.Index, Member.OldValue, Member.AsIBoldValue[bdepContents].Contentname)
        else if Member.BoldPersistenceState = bvpsCurrent then
          CopyValue(PreCondition.ValueSpace, ObjectContents, ObjectId, Member.BoldMemberRTInfo.Index, Member.AsIBoldValue[bdepContents], Member.AsIBoldValue[bdepContents].Contentname)
      end;
    end
  end;
end;

procedure TBoldOptimisticLockHandler.GetLockingValuesForRegions(Regions: TBoldRegionLookup; PreCondition: TBoldOptimisticLockingPrecondition);
var
  Traverser: TBoldIndexableListTraverser;
begin
  if Regions.IsEmpty then
    exit;
  Traverser := Regions.CreateTraverser;
  while Traverser.MoveNext do
  begin
    GetLockingValuesForRegion(Traverser.Item as TBoldRegion, PreCondition);
  end;
  Traverser.Free;
end;

procedure TBoldOptimisticLockHandler.GetRegionsForDirtyMembersInList(ObjectList: TBoldObjectList; Regions: TBoldRegionLookup);
var
  ObjIx: integer;
begin
  for ObjIx := 0 to Objectlist.count-1 do
    GetRegionsForDirtyMembers(ObjectList[ObjIx], Regions);
end;

procedure TBoldOptimisticLockHandler.GetRegionsForElement(Element: TBoldDomainElement; Regions: TBoldRegionLookup);
var
  RegionList: TList;
  Region: TBoldRegion;
  i: integer;
begin
  RegionList := TList.Create;
  try
    System.RegionFactory.GetRegionsForElement(Element, RegionList);
    for i := 0 to RegionList.Count-1 do
    begin
      Region := TBoldRegion(RegionList[i]);
      if not assigned(Regions.FindByID(Region.AsString)) then
        Regions.Add(RegionList[i]);
    end;
  finally
    RegionList.Free;
  end;
end;

function TBoldOptimisticLockHandler.GetRemotedfromIdRefValue(Value: IBoldValue): TBoldObjectId;
var
  IdRef: IBoldObjectIdRef;
  IdRefPair: IBoldObjectIdRefPair;
begin
  result := nil;
  if assigned(Value) then
  begin
    if Value.QueryInterface(IBoldObjectIdref, IdRef) = S_OK then
      Result := IdRef.Id
    else if Value.QueryInterface(IBoldObjectIdrefPair, IdRefPair) = S_OK then
      result := IdRefPair.Id2;
  end;
end;

procedure TBoldOptimisticLockHandler.RetrieveOptimisticLockingvalues(ObjectList: TBoldObjectlist; PreCondition: TBoldOptimisticLockingPrecondition);
var
  ObjIx, MemberIx: integer;
  BoldObject: TBoldObject;
  RelatedObjectcontents,
  NewObjectContents,
  ObjectContents: IBoldObjectContents;
  value: IBoldValue;
  ObjectId: TBoldObjectId;
  Mode: TBoldOptimisticLockingMode;
  MemberRTInfo: TBoldMemberRTInfo;
  RoleRTInfo: TBoldRoleRTInfo;
  RelatedObject: TBoldObject;

begin
  for ObjIx := 0 to ObjectList.Count - 1 do
  begin
    BoldObject := ObjectList[ObjIx];
    Mode := BoldObject.BoldClassTypeInfo.OptimisticLocking;
    NewObjectContents := nil;
    if Mode in [bolmTimeStamp, bolmModifiedMembers, bolmAllMembers] then
    begin
      ObjectId := BoldObject.BoldObjectLocator.BoldObjectID;
      ObjectContents := OldValues.ObjectContentsByObjectId[ObjectId];
      for MemberIx := 0 to BoldObject.BoldMemberCount - 1 do
      begin
        MemberRTInfo := BoldObject.BoldClassTypeInfo.AllMembers[MemberIx];
        if MemberRTInfo.CanHaveOldValue then
        begin
          value := nil;
          if assigned(ObjectContents) then
          begin
            if (Mode in [bolmModifiedMembers, bolmAllMembers]) or MemberRTInfo.EncouragesOptimisticLockingOnDeletedOnly then
              Value := ObjectContents.ValueByIndex[MemberIx]
          end;



          if not assigned(Value) and
             BoldObject.BoldMemberAssigned[MemberIx] and
             (BoldObject.BoldMembers[MemberIx].BoldPersistenceState = bvpscurrent) then
          begin
            if ((Mode = bolmModifiedMembers) and BoldObject.BoldObjectIsDeleted) or
               (Mode = bolmAllMembers) or
               MemberRTInfo.EncouragesOptimisticLockingOnDeletedOnly then
            begin
              Value := BoldObject.BoldMembers[MemberIx].AsIBoldValue[bdepContents];
            end;
          end;
          if assigned(value) and
             MemberRTinfo.EncouragesOptimisticLockingOnDeletedOnly and
             not BoldObject.BoldObjectIsDeleted then
            value := nil;



          if assigned(value) and
             (MemberRTinfo.IsSingleRole) and ((MemberRTInfo as TBoldRoleRTInfo).RoleType = rtInnerLinkRole) then
          begin
             NewObjectContents := Precondition.ValueSpace.EnsuredObjectContentsByObjectId[ObjectId];
             value := nil;
          end;

          if (not BoldObject.BoldObjectIsNew) and  assigned(value) then
            CopyValue(Precondition.ValueSpace, NewObjectContents, ObjectId, MemberIx, Value, BoldObject.BoldMembers[MemberIx].AsIBoldValue[bdepContents].ContentName);

          if MemberRTInfo.IsSingleRole and MemberRTInfo.IsStoredInObject then
          begin
            RoleRTInfo := MemberRTInfo as TBoldRoleRTInfo;
            if (RoleRTInfo.RoleType = rtRole) and
               RoleRTInfo.RoleRTInfoOfOtherEnd.IsSingleRole and
               not RoleRTInfo.RoleRTInfoOfOtherEnd.IsStoredInObject and
               BoldObject.BoldMembers[MemberIx].BoldDirty then
            begin
              RelatedObject := (BoldObject.BoldMembers[MemberIx] as TBoldObjectReference).BoldObject;
              if assigned(relatedObject) and (not relatedObject.BoldObjectIsNew) then
              begin
                value := RelatedObject.BoldMembers[RoleRTInfo.IndexOfOtherEnd].OldValue;
                if assigned(Value) then
                begin
                  RelatedObjectcontents := Precondition.ValueSpace.EnsuredObjectContentsByObjectId[RelatedObject.BoldObjectLocator.BoldObjectId];
                  CopyValue(
                    Precondition.ValueSpace,
                    RelatedObjectcontents,
                    RelatedObject.BoldObjectLocator.BoldObjectId,
                    RoleRTInfo.IndexOfOtherEnd, Value,
                    RelatedObject.BoldMembers[RoleRTInfo.IndexOfOtherEnd].AsIBoldValue[bdepContents].ContentName);
                end;
              end;
            end;
          end;
        end;
      end;
      if (not BoldObject.BoldObjectIsNew) and (Mode = bolmTimestamp) then
      begin
        if not assigned(NewObjectContents) then
          NewObjectContents := Precondition.ValueSpace.EnsuredObjectContentsByObjectId[ObjectId];

        NewObjectContents.TimeStamp := BoldObject.AsIBoldObjectContents[bdepContents].TimeStamp;
      end;
    end;
  end;
  NewObjectContents := nil;
  ObjectContents := nil;
  RelatedObjectcontents := nil;
  value := nil;
end;

procedure TBoldOptimisticLockHandler.GetRegionsForRemoteMember(OwningId: TBoldObjectId; Regions: TBoldRegionLookup; OwningRoleRTInfo: TBoldRoleRTInfo);
var
  RemoteObj: TBoldObject;
  OtherEnd: TBoldMember;
begin
  if assigned(OwningId) then
  begin
    RemoteObj := System.EnsuredLocatorByID[OwningId].BoldObject;
    if assigned(RemoteObj) then
    begin
      OtherEnd := RemoteObj.Boldmembers[OwningRoleRTInfo.IndexOfOtherEnd];
      if OtherEnd.BoldPersistenceState = bvpsInvalid then
      begin
        if OwningRoleRTInfo.ForceOtherEnd then
          raise EBold.CreateFmt(sRelatedRoleNotLoaded, [classname, OwningRoleRTInfo.AsString]);
      end
      else
        GetRegionsForElement(OtherEnd, Regions);
    end
    else
      if OwningRoleRTInfo.ForceOtherEnd then
        raise EBold.CreateFmt(sRelatedObjectNotLoaded, [classname, OwningRoleRTInfo.AsString]);
  end;
end;

procedure TBoldOptimisticLockHandler.EnsureEnclosure(Obj: TBoldObject; Enclosure: TBoldObjectList; ValidateOnly: Boolean; var ListIsEnclosure: Boolean);
var
  Regions: TBoldRegionLookup;
  Expander: TBoldRegionExpander;
  EmptyLookup: TBoldRegionLookup;
begin
  if assigned(RegionFactory) then
  begin
    Regions := TBoldRegionLookup.Create;
    Expander := TBoldRegionExpander.Create;
    EmptyLookup := TBoldRegionLookup.Create;
    try
      GetRegionsForDirtyMembers(Obj, Regions);
      Expander.ExpandParentRegions(Regions, EmptyLookup, EmptyLookup);
      Expander.ExpandSubregions(Expander.NewParentRegions, EmptyLookup);
      AddRegionsObjectsToEnclosure(Expander.NewParentRegions, Enclosure, ValidateOnly, ListIsEnclosure);
      AddRegionsObjectsToEnclosure(Expander.NewSubRegions, Enclosure, ValidateOnly, ListIsEnclosure);
    finally
      Regions.Free;
      Expander.free;
      EmptyLookup.Free;
    end;
  end;
end;

procedure TBoldOptimisticLockHandler.GetRegionsForDirtyMembers(Obj: TBoldObject; Regions: TBoldRegionLookup);
var
  MemberIx: integer;
  Member: TBoldmember;
begin
  if Obj.BoldPersistenceState = bvpsModified then
    GetRegionsForElement(Obj, regions);

  for MemberIx := 0 to Obj.BoldMemberCount-1 do
  begin
    if Obj.BoldmemberAssigned[MemberIx] then
    begin
      Member := Obj.BoldMembers[MemberIx];
      if (Member.BoldPersistenceState = bvpsModified) then
      begin
        GetRegionsForElement(Obj, Regions);
      end;
    end;
  end;
end;

procedure TBoldOptimisticLockHandler.AddRegionObjectsToEnclosure(
  Region: TBoldRegion; Enclosure: TBoldObjectList; ValidateOnly: Boolean;
  var ListIsEnclosure: Boolean);

procedure TryAddObj(Obj: TBoldObject);
begin
  if Obj.BoldDirty and not Enclosure.Includes(Obj) then
  begin
    if not ValidateOnly then
      Enclosure.Add(Obj);
    ListIsEnclosure := false;
  end;
end;

var
  Elements: TList;
  Element: TBoldDomainElement;
  i: integer;
  G: IBoldGuard;
begin
  G := TBoldGuard.Create(Elements);
  Elements := TList.Create;
  Region.GetElements(elements);
  for i := 0 to Elements.Count-1 do
  begin
    Element := TBoldDomainElement(Elements[i]);
    if element is TBoldObject then
      TryAddObj(element as TBoldObject)
    else if element is TBoldmember then
      TryAddObj((element as TBoldMember).OwningObject);
  end;
end;

procedure TBoldOptimisticLockHandler.AddRegionsObjectsToEnclosure(
  Regions: TBoldRegionLookup; Enclosure: TBoldObjectList;
  ValidateOnly: Boolean; var ListIsEnclosure: Boolean);
var
  Traverser: TBoldIndexableListTraverser;
begin
  if Regions.IsEmpty then
    exit;
  Traverser := Regions.CreateTraverser;
  while Traverser.MoveNext do
  begin
    AddRegionObjectsToEnclosure(Traverser.Item as TBoldRegion, Enclosure, ValidateOnly, ListIsEnclosure);
  end;
  Traverser.Free;
end;

function TBoldOptimisticLockHandler.GetRegionFactory: TBoldRegionFactory;
begin
  result := System.RegionFactory as TBoldRegionFactory;
end;

end.
