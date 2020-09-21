unit BoldSystemPersistenceHandler;
// This unit contains implementations of functions in TBoldSystem related to persistence
// which could resonably have been separated, but are provided on TBoldSystem for user convenience

interface

uses
  Db,
  BoldBase,
  BoldDefs,
  BoldSystem,
  BoldId,
  BoldPersistenceController,
  BoldCondition;

type
  { forward declarations }
  TBoldSystemPersistenceHandler = class;
  TBoldTimeMappingCache = class;

  { TBoldSystemPersistenceHandler }
  TBoldSystemPersistenceHandler = class(TBoldAbstractSystemPersistenceHandler)
  private
    fTimeStampOfLatestUpdate: TBoldTimeStampType;
    fTimestampToTimeCache: TBoldTimeMappingCache;
    fTimeToTimestampCache: TBoldTimeMappingCache;
    procedure DoPreUpdate;
    function GetPersistenceController: TBoldPersistenceController;
    procedure PMFetch(ObjectList: TBoldObjectList; MemberIdList: TBoldMemberIdList);
  protected
    function GetTimeStampOfLatestUpdate: TBoldTimeStampType; override;
    property PersistenceController: TBoldPersistenceController read GetPersistenceController;
  public
    constructor Create(System: TBoldSystem); override;
    destructor Destroy; override;
    function EnsureEnclosure(ObjectList: TBoldObjectList; ValidateOnly: Boolean): Boolean; override;
    procedure FetchLinksWithObjects(ObjectList: TBoldObjectList; const LinkName: string; FetchedObjects: TBoldObjectList); override;
    procedure FetchList(FetchList: TBoldObjectList); override;
    procedure FetchObjectById(BoldObjectId: TBoldObjectId); override;
    procedure FetchMember(Member: TBoldMember); override;
    procedure FetchClass(ClassList: TBoldObjectList; Time: TBoldTimestampType); override;
    procedure GetAllWithCondition(aList: TBoldObjectList; Condition: TBoldCondition); override;
    procedure GetAllInClassWithSQL(aList: TBoldObjectList; AClass: TBoldObjectClass; WhereClause, OrderByClause: String; Params: TParams; JoinInheritedTables: Boolean; MaxAnswers: integer; Offset: integer); override;
    function GetTimeForTimestamp(Timestamp: TBoldTimestampType): TDateTime; override;
    function GetTimestampForTime(ClockTime: TDateTime): TBoldTimestampType; override;
    procedure UpdateDatabaseWithList(ObjectList: TBoldObjectList);  override;
  end;

  { TBoldTimeMappingCache }
  TBoldTimeMappingCache = class(TBoldMemoryManagedObject)
  private
    fTimestamps: array of TBoldTimestampType;
    fClocktimes: array of TDateTime;
    fNext: Integer;
    fCount: Integer;
  public
    constructor Create(MaxCount: Integer);
    function TimeForTimestamp(Timestamp: TBoldTimestampType; var ClockTime: TDateTime): Boolean;
    function TimestampForTime(ClockTime: TDateTime; var Timestamp: TBoldTimestampType): Boolean;
    procedure Add(Timestamp: TBoldTimestampType; ClockTime: TDateTime);
  end;

implementation

uses
  SysUtils,
  BoldvalueInterfaces,
  BoldValueSpaceInterfaces,
  BoldDomainElement,
  BoldSystemRT,
  BoldTaggedValueSupport,
  BoldUpdatePrecondition,
  BoldGuard,
  BoldMath,
  BoldCoreConsts;

{ TBoldSystemPersistenceHandler }

constructor TBoldSystemPersistenceHandler.Create(System: TBoldSystem);
begin
  Inherited Create(System);
  fTimeStampOfLatestUpdate := -1;
  fTimestampToTimeCache := TBoldTimeMappingCache.Create(100);
  fTimeToTimestampCache := TBoldTimeMappingCache.Create(20);
end;

procedure TBoldSystemPersistenceHandler.DoPreUpdate;
begin
  if Assigned(OnPreUpdate) then
    OnPreUpdate(self);
end;

function TBoldSystemPersistenceHandler.EnsureEnclosure(ObjectList: TBoldObjectList; ValidateOnly: Boolean): Boolean;
// TODO: The enclosure should include the expanded "regions" of all objects in the list
var
  I, M : Integer;
  ExamineBoldObject: TBoldObject;
  ListIsEnclosure: Boolean;
  MemberRTInfo: TBoldMemberRTInfo;
  RoleRTInfo: TBoldRoleRTInfo;
  OtherEnd: TBoldObjectReference;
  ObjectReference: TBoldObjectReference;

  procedure AddLocatorToEnclosure(Locator: TBoldObjectLocator);
  begin
    if assigned(Locator) and
      Locator.ObjectIsPersistent and
      not ObjectList.LocatorInList(Locator) then
    begin
      if not ValidateOnly then
      begin
        if (Locator.BoldObjectId is TBoldInternalObjectId) and not assigned(Locator.BoldObject) then
          assert(false, 'Internal Error, a deleted non-saved object got into the enclosure, try to figure out why...')
        else
          ObjectList.AddLocator(Locator);
      end;
      ListIsEnclosure := False;
    end;
  end;

  procedure AddUnsavedLocatorToEnclosure(Locator: TBoldObjectLocator);
  begin
    if assigned(Locator) and
      not Locator.BoldObjectID.IsStorable then
      AddLocatorToEnclosure(Locator);
  end;

  procedure AddIdToEnclosure(Id: TBoldObjectId);
  begin
    if assigned(Id) then
      AddLocatorToEnclosure(System.EnsuredLocatorByID[Id]);
  end;

  procedure AddOldValuesForRole(Role: TBoldMember; RoleRTInfo: TBoldRoleRTInfo);
  var
    Oldvalue: IBoldValue;
    i: integer;
  begin
    OldValue := Role.OldValue;
    if assigned(Oldvalue) then
    begin
      if RoleRTInfo.IsIndirect then
      begin
        if RoleRTInfo.IsMultiRole then
        begin
          // Indirect MultiLink
          for i := 0 to (OldValue as IBoldObjectIdListRefPair).Count-1 do
            AddIdToEnclosure((OldValue as IBoldObjectIdListRefPair).IdList1[i]);
        end
        else
        begin
          // Indirect SingleLink
          AddIdToEnclosure((OldValue as IBoldObjectIdRefPair).Id1);
        end;
      end
      else
      begin
        if RoleRTInfo.IsMultiRole then
        begin
          // Direct Multilink
          for i := 0 to (OldValue as IBoldObjectIdListRef).Count-1 do
            AddIdToEnclosure((OldValue as IBoldObjectIdListRef).IdList[i]);
        end
        else
        begin
          // Direct Singlelink
          if RoleRTInfo.RoleRTInfoOfOtherEnd.IsStoredInObject then
            AddIdToEnclosure((OldValue as IBoldObjectIdRef).Id);
        end;
      end;
    end;
  end;

begin
  ListIsEnclosure := true;
  i := 0;
  while i < ObjectList.Count do
   begin
    ExamineBoldObject := ObjectList[i];
    for M := 0 to ExamineBoldObject.BoldClassTypeInfo.AllMembers.Count - 1 do
    begin
      MemberRTInfo := ExamineBoldObject.BoldClassTypeInfo.AllMembers[M];
      if memberRTInfo.IsRole then
      begin
        RoleRTInfo := MemberRTInfo as TBoldRoleRTInfo;

        if RoleRTInfo.IsSingleRole and RoleRTInfo.Persistent and RoleRTInfo.IsStoredInObject and
           ExamineBoldObject.BoldMemberAssigned[M] and
           (ExamineBoldObject.BoldMembers[M].BoldPersistenceState = bvpsModified) then
        begin
          // singlelinks should include the other end if it is stored or belongs to a new object
          ObjectReference := ExamineBoldObject.BoldMembers[M] as TBoldObjectReference;
          if RoleRTInfo.RoleRTInfoOfOtherEnd.IsStoredInObject then
          begin
            AddLocatorToEnclosure(ObjectReference.Locator);
            AddOldValuesForRole(ObjectReference, RoleRTInfo);
          end
          else
          begin
            AddUnsavedLocatorToEnclosure(ObjectReference.Locator);
            if RoleRTInfo.RoleRTInfoOfOtherEnd.IsSingleRole then
            begin
              // the other end is single-nonembedded, so we must include the old value of that link in our enclosure
              // to make sure that the unlinking of that link is saved at the same time.
              if assigned(ObjectReference.Locator) and assigned(ObjectReference.Locator.BoldObject) then
              begin
                OtherEnd := ObjectReference.Locator.BoldObject.BoldMembers[RoleRTInfo.IndexOfOtherEnd] as TBoldObjectReference;
                AddIdToEnclosure(OtherEnd.OldEmbeddingOtherEndId);
              end;
            end;
          end;
        end;

        if ExamineBoldObject.BoldObjectIsDeleted then
        begin
          AddOldValuesForRole(ExamineBoldObject.BoldMembers[M], RoleRTInfo);
        end;
      end;
    end;
    System.OptimisticLockHandler.EnsureEnclosure(ExamineBoldObject, ObjectList, ValidateOnly, ListIsEnclosure);
    inc(i);
  end;
  result := ListIsEnclosure;
end;

procedure TBoldSystemPersistenceHandler.FetchLinksWithObjects(ObjectList: TBoldObjectList; const LinkName: string; FetchedObjects: TBoldObjectList);
var
  CommonClass: TBoldClassTypeInfo;
  I, J: Integer;
  MultiLink: TBoldObjectList;
  SingleLink: TBoldObjectReference;
  MemberRTInfo: TBoldMemberRTInfo;
  roleRTInfo: TBoldRoleRTInfo;
  FetchList: TBoldObjectList;
  MemberIdLIst: TBoldMemberIdLIst;
  Guard: IBoldGuard;

  procedure InternalAddLocator(Locator: TBoldObjectLocator);
  begin
    if Assigned(Locator) then
      if not Assigned(Locator.BoldObject) then
        FetchList.AddLocator(Locator);
  end;

begin
  Guard := tBoldGuard.Create(FetchList, MemberIdList);

  if not System.BoldPersistent or (ObjectList.Count = 0) then
    exit;

  FetchList := TBoldObjectList.create;
  MemberIdList := TBoldMemberIdList.Create;

  ObjectList.EnsureObjects;

  // Find common class
  CommonClass := ObjectList[0].BoldClassTypeInfo;
  for I := 1 to ObjectList.Count - 1 do
  begin
    CommonClass := CommonClass.LeastCommonSuperClass(ObjectList[I].BoldClassTypeInfo);
    if ObjectList[I].BoldSystem <> System then
      raise EBoldFeatureNotImplementedYet.Create(sCannotFetchWithLinksFromMultipleSystems);
  end;

  if not Assigned(CommonClass) then
    raise EBold.CreateFmt(sNoCommonSuperClass, [ClassName]);

  MemberRTInfo := CommonClass.MemberRTInfoByExpressionName[LinkName];
  if not (MemberRTInfo is TBoldRoleRTInfo) then
    raise EBold.CreateFmt(sNoRoleCalledX, [ClassName, CommonClass.ExpressionName, LinkName]);

  roleRTInfo := MemberRTInfo as TBoldRoleRTInfo;

  if not roleRTInfo.IsStoredInObject and RoleRTInfo.Persistent and (RoleRTInfo.RoleType = rtRole) then
  begin
    for I := 0 to ObjectList.Count - 1 do
      if ObjectList[I].BoldMembers[roleRTInfo.index].BoldPersistenceState = bvpsInvalid then
        FetchList.Add(ObjectList[I]);
    if FetchList.count > 0 then
    begin
      MemberIdList.Add(TBoldMemberId.Create(MemberRTInfo.index));
      PMFetch(FetchList, MemberIdList);
    end;
  end;

  FetchList.Clear;
  for I := 0 to ObjectList.Count - 1 do
  begin
    if roleRTInfo.IsMultiRole then
    begin
      MultiLink := ObjectList[I].BoldMembers[roleRTInfo.index] as TBoldObjectList;
      for J := 0 to MultiLink.Count - 1 do
        InternalAddLocator(MultiLink.Locators[J]);
    end
    else
    begin
      SingleLink := ObjectList[I].BoldMembers[roleRTInfo.index] as TBoldObjectReference;
      InternalAddLocator(SingleLink.Locator);
    end;
  end;
  if FetchList.Count > 0 then
    PMFetch(FetchList, nil);
  if Assigned(FetchedObjects) then
    FetchedObjects.Assign(FetchList);
end;

procedure TBoldSystemPersistenceHandler.FetchObjectById(BoldObjectId: TBoldObjectId);
var
  ObjectList: TBoldObjectList;
begin
  ObjectList := TBoldObjectList.Create;
  try
    ObjectList.AddLocator(System.EnsuredLocatorByID[BoldObjectID]);
    PMFetch(ObjectList, nil);
  finally
    ObjectList.Free;
  end;
end;

procedure TBoldSystemPersistenceHandler.GetAllWithCondition(aList: TBoldObjectList; Condition: TBoldCondition);
var
  I: Integer;
  Locator: TBoldObjectLocator;
  ObjectIdList: TBoldObjectIdList;
begin
  if not assigned(PersistenceController) then
    raise EBold.Create(sNoPersistenceController);
  ObjectIdList := TBoldObjectIdList.Create;
  try
    PersistenceController.PMFetchIDListWithCondition(ObjectIdList, System.AsIBoldvalueSpace[bdepPMIn], fmNormal, Condition, NOTVALIDCLIENTID);
    // remove objects that have been deleted in memory
    for I := ObjectIdList.Count - 1 downto 0 do
    begin
      Locator := System.Locators.LocatorByID[ObjectIdList[i]];
      if assigned(Locator) and assigned(Locator.BoldObject) and (Locator.BoldObject.BoldExistenceState = besDeleted) then
        ObjectIdList.RemoveByIndex(i);
    end;
    aList.FillFromIDList(ObjectIdList, system);
  finally
    ObjectIdList.Free;
  end;
end;

procedure TBoldSystemPersistenceHandler.GetAllInClassWithSQL(aList: TBoldObjectList; AClass: TBoldObjectClass; WhereClause, OrderByClause: String; Params: TParams; JoinInheritedTables: Boolean; MaxAnswers: integer; Offset: integer);
var
  Condition: TBoldSQLCondition;
  LocalParams: TParams;
  ClasstypeInfo: TBoldClasstypeInfo;
begin
  Condition := TBoldSQLCondition.Create;
  Condition.WhereFragment := WhereClause;
  Condition.OrderBy := OrderByClause;
  Condition.JoinInheritedTables := JoinInheritedTables;
  Condition.MaxAnswers := MaxAnswers;
  Condition.Offset := Offset;
  if assigned(aClass) then
  begin
    ClassTypeInfo := System.BoldSystemTypeInfo.TopSortedClasses.ItemsByObjectClass[AClass];
    if Assigned(ClassTypeInfo) then
      Condition.TopSortedIndex := ClassTypeInfo.TopSortedIndex
    else
      raise EBold.CreateFmt(sNoSuchClassInModel, [classname, aClass.ClassName]);
  end
  else
    raise EBold.CreateFmt(sClassParameterMissing, [classname]);

  if not assigned(Params) then
  begin
    LocalParams := TParams.Create(nil);
    Condition.Params := LocalParams;
  end
  else
  begin
    Condition.Params := PArams;
    LocalParams := nil;
  end;

  try
    GetAllWithCondition(aList, Condition);
  finally
    Condition.Free;
    LocalParams.Free;
  end;
end;

procedure TBoldSystemPersistenceHandler.UpdateDatabaseWithList(ObjectList: TBoldObjectList);
var
  i: integer;
  ObjectsToUpdate: TBoldObjectList;
  aTranslationList: TBoldIdTranslationList;
  ObjectIdList: TBoldObjectIdList;
  failureList: TBoldObjectList;
  Precondition: TBoldOptimisticLockingPrecondition;
begin
  if System.InTransaction then
    raise EBold.Create(sCannotUpdateWhileInTransaction);

  System.DelayObjectDestruction;
  ObjectsToUpdate := ObjectList.Clone as TBoldObjectList;
  try
    for i := ObjectsToUpdate.Count-1 downto 0 do
    begin
      if ObjectsToUpdate.Locators[i].BoldSystem <> System then
        raise EBold.CreateFmt(sForeignObjectInUpdate, [classname]);
      if not assigned(ObjectsToUpdate.Locators[i].BoldObject) or
         not ObjectsToUpdate[i].BoldPersistent then
        ObjectsToUpdate.RemoveByIndex(i);
    end;

    if ObjectsToUpdate.Count > 0 then
    begin
      DoPreUpdate;
      EnsureEnclosure(ObjectsToUpdate, false);
      ObjectIdList := ObjectsToUpdate.CreateObjectIdList;
      aTranslationList := TBoldIdTranslationList.Create;
      if System.BoldSystemTypeInfo.OptimisticLocking = bolmOff then
        Precondition := nil
      else
      begin
        Precondition := TBoldOptimisticLockingPrecondition.create;
        System.OptimisticLockHandler.AddOptimisticLocks(ObjectsToUpdate, PreCondition);
        if not Precondition.HasOptimisticLocks then
          FreeAndNil(PreCondition);
      end;

      //TODO: in the future, call another function to add optimistic locking data for optimistic region locking
      //TODO: If the model wants optimistic locking, but none of the classes of objects to be updated,
      //      the precondition should be freed.

      try

        if assigned(System.PessimisticLockHandler) and not System.PessimisticLockHandler.EnsureLocks then
          raise EBold.CreateFmt(sRequiredLocksNotHeld, [classname]);
        BoldClearLastfailure;

        try
          if not StartUpdateForAll(ObjectsToUpdate) then
            BoldRaiseLastFailure(System, 'UpdateDatabaseWithList', sStartUpdateFailed); // do not localize

          PersistenceController.PMUpdate(ObjectIdList, System.AsIBoldvalueSpace[bdepPMOut], nil, Precondition, aTranslationList, fTimeStampOfLatestUpdate, NOTVALIDCLIENTID);
          if assigned(Precondition) and Precondition.Failed then
          begin
            if assigned(system.OnOptimisticLockingFailed) then
            begin
              FailureList := TBoldObjectList.Create;
              try
                FailureList.FillFromIDList(Precondition.FailureList, System);
                system.OnOptimisticLockingFailed(ObjectsToUpdate, FailureList, Precondition.FailureReason);
              finally
                FailureList.free;
              end;
            end
            else
              raise EBoldOperationFailedForObjectList.Create(Precondition.FailureReason, [], Precondition.FailureList, System);
          end else
          begin
            for i := 0 to ObjectsToUpdate.Count - 1 do
              ObjectsToUpdate[i].AsIBoldObjectContents[bdepPMIn].TimeStamp := TimeStampOfLatestUpdate;
            EndUpdateForAll(ObjectsToUpdate, aTranslationList);
          end;
        except
          on e: Exception do
          begin
            if GetBoldLastFailureReason <> nil then
              BoldRaiseLastFailure(System, 'UpdateDatabaseWithlist', e.message) // do not localize
            else
              raise;
          end;
        end;
      finally
        aTranslationList.Free;
        ObjectIdList.Free;
        Precondition.Free;
      end;
    end;
  finally
    if assigned(System.PessimisticLockHandler) then
      System.PessimisticLockHandler.ReleaseUnneededRegions;
    ObjectsToUpdate.Free;
    System.AllowObjectDestruction;
  end;

end;

function TBoldSystemPersistenceHandler.GetTimeStampOfLatestUpdate: TBoldTimeStampType;
begin
  Result := fTimeStampOfLatestUpdate;
end;

function TBoldSystemPersistenceHandler.GetPersistenceController: TBoldPersistenceController;
begin
  Result := System.PersistenceController;
end;

procedure TBoldSystemPersistenceHandler.FetchMember(Member: TBoldMember);
var
  ObjectList: TBoldObjectList;
  MemberIdLIst: TBoldMemberIdLIst;
  MemberId: TBoldMemberId;
  Guard: IBoldguard;
begin
  Guard := TBoldGuard.Create(ObjectList, MemberIdList);
  ObjectList := TBoldObjectList.Create;
  ObjectList.Add(Member.OwningObject);
  if Member.BoldMemberRTInfo.DelayedFetch then
  begin
    MemberIdLIst := TBoldMemberIdLIst.Create;
    MemberId := TBoldMemberId.Create(Member.BoldMemberRTInfo.index);
    MemberIdList.Add(MemberId);
  end;
  PMFetch(ObjectList, MemberIdList);
end;

procedure TBoldSystemPersistenceHandler.FetchClass(ClassList: TBoldObjectList; Time: TBoldTimestampType);
var
  ObjectIdList: TBoldObjectIdList;
  Condition: TBoldConditionWithClass;
  ClassTypeInfo: TBoldClassTypeInfo;
  ListInterface: IBoldObjectIdListRef;
begin
  if not assigned(PersistenceController) then
    raise EBold.Create(sNoPersistenceController);
  ClassTypeInfo := (ClassList.BoldType as TBoldListTypeInfo).ListElementTypeInfo as TBoldClassTypeInfo;
  ObjectIdList := TBoldObjectIdList.Create;

  Condition := TBoldConditionWithClass.Create;
  Condition.TopSortedIndex := ClassTypeInfo.TopSortedIndex;
  Condition.Time := Time;

  try
    PersistenceController.PMFetchIDListWithCondition(ObjectIdList, System.AsIBoldvalueSpace[bdepPMIn], fmNormal, Condition, NOTVALIDCLIENTID);
    ClassList.ProxyInterface(IBoldObjectIdListRef, bdepPMIn, ListInterface);
    ListInterface.SetFromIdList(ObjectIdList);
  finally
    FreeAndNil(Condition);
    FreeAndNil(ObjectIdList);
  end;
end;

destructor TBoldSystemPersistenceHandler.Destroy;
begin
  FreeAndNil(fTimestampToTimeCache);
  FreeAndNil(fTimeToTimestampCache);
  inherited;
end;

function TBoldSystemPersistenceHandler.GetTimeForTimestamp(
  Timestamp: TBoldTimestampType): TDateTime;
begin
  if not fTimestampToTimeCache.TimeForTimestamp(Timestamp, result) then
  begin
    PersistenceController.PMTimeForTimestamp(Timestamp, result);
    fTimestampToTimeCache.Add(Timestamp, Result);
  end;
end;

function TBoldSystemPersistenceHandler.GetTimestampForTime(
  ClockTime: TDateTime): TBoldTimestampType;
begin
  PersistenceController.PMTimestampForTime(ClockTime, result);
end;


procedure TBoldSystemPersistenceHandler.FetchList(FetchList: TBoldObjectList);
begin
  PMFetch(FetchList, nil);
end;

procedure TBoldSystemPersistenceHandler.PMFetch(ObjectList: TBoldObjectList; MemberIdList: TBoldMemberIdList);
var
  ObjectIdList: TBoldObjectIdList;
  Guard: IBoldGuard;
  i: integer;
begin
  Guard := TBoldguard.Create(ObjectidList);
  if Objectlist.Count > 0 then
  begin
    ObjectIdList := TBoldObjectIdList.Create;
    for i := 0 to Objectlist.Count-1 do
      ObjectIdList.Add(ObjectList.Locators[i].BoldObjectID);

    PersistenceController.PMFetch(ObjectIdList, System.AsIBoldvalueSpace[bdepPMIn], MemberIdList, fmNormal, NOTVALIDCLIENTID);
    EndFetchForAll(ObjectList, MemberIdList);
  end;
end;

{ TBoldTimeMappingCache }

procedure TBoldTimeMappingCache.Add(Timestamp: TBoldTimestampType;
  ClockTime: TDateTime);
begin
  fTimestamps[fNext] := Timestamp;
  fClocktimes[fNext] := ClockTime;
  fNext := (fNext + 1) mod Length(fTimestamps);
  if fCount < Length(fTimestamps) then
    inc(fCount);
end;

constructor TBoldTimeMappingCache.Create(MaxCount: Integer);
begin
  SetLength(fTimestamps, MaxCount);
  SetLength(fClockTimes, MaxCount);
end;

function TBoldTimeMappingCache.TimeForTimestamp(
  Timestamp: TBoldTimestampType; var ClockTime: TDateTime): Boolean;
var
  i: Integer;
begin
  for i := 0 to MinIntValue([High(fTimestamps), fCount - 1]) do
    if fTimestamps[i] = Timestamp then
    begin
      ClockTime := fClocktimes[i];
      result := True;
      exit;
    end;
  result := false;
end;

function TBoldTimeMappingCache.TimestampForTime(ClockTime: TDateTime;
  var Timestamp: TBoldTimestampType): Boolean;
var
  i: Integer;
begin
  for i := 0 to MinIntValue([High(fClocktimes), fCount - 1]) do
    if fClocktimes[i] = ClockTime then
    begin
      Timestamp := fTimestamps[i];
      result := True;
      exit;
    end;
  result := false;
end;

end.
