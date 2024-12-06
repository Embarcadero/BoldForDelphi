{ Global compiler directives }
{$include bold.inc}
unit BoldSystemPersistenceHandler;

interface

uses
  Db,
  BoldBase,
  BoldDefs,
  BoldSystem,
  BoldId,
  BoldPersistenceController,
  BoldCondition,
  BoldElements;

type
  { forward declarations }
  TBoldSystemPersistenceHandler = class;
  TBoldTimeMappingCache = class;

  { TBoldSystemPersistenceHandler }
  TBoldSystemPersistenceHandler = class(TBoldAbstractSystemPersistenceHandler)
  private
    fTimeStampOfLatestUpdate: TBoldTimeStampType;
    fTimeOfLatestUpdate: TDateTime;
    fTimestampToTimeCache: TBoldTimeMappingCache;
    fTimeToTimestampCache: TBoldTimeMappingCache;
    procedure DoPreUpdate(ObjectList: TBoldObjectList);
    procedure DoPostUpdate(ObjectList: TBoldObjectList);
    function GetPersistenceController: TBoldPersistenceController;
    procedure PMFetch(ObjectList: TBoldObjectList; MemberIdList: TBoldMemberIdList);
  protected
    function GetTimeStampOfLatestUpdate: TBoldTimeStampType; override;
    function GetTimeOfLatestUpdate: TDateTime; override;
    property PersistenceController: TBoldPersistenceController read GetPersistenceController;
  public
    constructor Create(System: TBoldSystem); override;
    destructor Destroy; override;
    function EnsureEnclosure(ObjectList: TBoldObjectList; ValidateOnly: Boolean): Boolean; override;
    procedure FetchMembersWithObjects(aBoldObjectList: TBoldObjectList; aBoldMemberIdList: TBoldMemberIdList); override;
    procedure FetchMembersWithObjects(aBoldObjectList: TBoldObjectList; AMemberCommaList: string); override;
    procedure FetchLinksWithObjects(ObjectList: TBoldObjectList; const LinkName: string; FetchObjectsInLink: Boolean = True); override;
    procedure FetchList(FetchList: TBoldObjectList); override;
    procedure FetchObjectById(BoldObjectId: TBoldObjectId); override;
    procedure FetchMember(Member: TBoldMember); override;
    procedure FetchClass(ClassList: TBoldObjectList; Time: TBoldTimestampType); override;
    procedure GetAllWithCondition(aList: TBoldObjectList; Condition: TBoldCondition); override;
    procedure GetAllInClassWithSQL(aList: TBoldObjectList; AClass: TBoldObjectClass; WhereClause, OrderByClause: String; Params: TParams; JoinInheritedTables: Boolean; MaxAnswers: integer; Offset: integer); override;
    procedure GetAllInClassWithRawSQL(aList: TBoldObjectList; AClass: TBoldObjectClass; SQL: String; Params: TParams; MaxAnswers: integer; Offset: integer);override;
    function GetTimeForTimestamp(Timestamp: TBoldTimestampType): TDateTime; override;
    function GetTimestampForTime(ClockTime: TDateTime): TBoldTimestampType; override;
    procedure UpdateDatabaseWithList(ObjectList: TBoldObjectList); override;
    function CanEvaluateInPS(sOCL: string; aContext: TBoldElementTypeInfo = nil; const aVariableList: TBoldExternalVariableList = nil): Boolean; override;
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
  Classes,
  SysUtils,

  BoldCoreConsts,
  BoldSubscription,
  BoldValueInterfaces,
  BoldValueSpaceInterfaces,
  BoldDomainElement,
  BoldSystemRT,
  BoldTaggedValueSupport,
  BoldUpdatePrecondition,
  BoldGuard,
  BoldMath;

{ TBoldSystemPersistenceHandler }

function TBoldSystemPersistenceHandler.CanEvaluateInPS(sOCL: string;
  aContext: TBoldElementTypeInfo;
  const aVariableList: TBoldExternalVariableList): Boolean;
begin
  Result := PersistenceController.CanEvaluateInPS(sOCL, System, aContext, aVariableList);
end;

constructor TBoldSystemPersistenceHandler.Create(System: TBoldSystem);
begin
  Inherited Create(System);
  fTimeStampOfLatestUpdate := -1;
  fTimeOfLatestUpdate := 0;
  fTimestampToTimeCache := TBoldTimeMappingCache.Create(100);
  fTimeToTimestampCache := TBoldTimeMappingCache.Create(20);
end;

procedure TBoldSystemPersistenceHandler.DoPreUpdate(ObjectList: TBoldObjectList);
begin
  System.SendExtendedEvent(beBeginUpdate, [ObjectList]);
  if Assigned(OnPreUpdate) then
    OnPreUpdate(ObjectList);
end;

procedure TBoldSystemPersistenceHandler.DoPostUpdate(
  ObjectList: TBoldObjectList);
begin
  System.SendExtendedEvent(beEndUpdate, [ObjectList]);
  if Assigned(OnPostUpdate) then
    OnPostUpdate(ObjectList);
end;

function TBoldSystemPersistenceHandler.EnsureEnclosure(ObjectList: TBoldObjectList; ValidateOnly: Boolean): Boolean;
var
  ExamineBoldObject: TBoldObject;
  ListIsEnclosure: Boolean;
  RoleRTInfo: TBoldRoleRTInfo;

  procedure AddLocatorToEnclosure(Locator: TBoldObjectLocator);
  begin
    if assigned(Locator) and
      Locator.ObjectIsPersistent and
      not ObjectList.LocatorInList(Locator) then
    begin
      if not ValidateOnly then
      begin
        if (Locator.BoldObjectId is TBoldInternalObjectId) and not assigned(Locator.BoldObject) then
          assert(false, Format('Internal Error, a deleted non-saved object got into the enclosure, while processing Object ID:%s(%s), Role:%s. Locator.Id:%s.',
                        [ExamineBoldObject.Displayname, ExamineBoldObject.BoldObjectLocator.AsString, RoleRTInfo.asString, Locator.BoldObjectId.AsString]))
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
          for i := 0 to (OldValue as IBoldObjectIdListRefPair).Count-1 do
            AddIdToEnclosure((OldValue as IBoldObjectIdListRefPair).IdList1[i]);
        end
        else
        begin
          AddIdToEnclosure((OldValue as IBoldObjectIdRefPair).Id1);
        end;
      end
      else
      begin
        if RoleRTInfo.IsMultiRole then
        begin
          for i := 0 to (OldValue as IBoldObjectIdListRef).Count-1 do
            AddIdToEnclosure((OldValue as IBoldObjectIdListRef).IdList[i]);
        end
        else
        begin
          if RoleRTInfo.RoleRTInfoOfOtherEnd.IsStoredInObject then
            AddIdToEnclosure((OldValue as IBoldObjectIdRef).Id);
        end;
      end;
    end;
  end;

var
  OldCount: Integer;
  I,J,M : Integer;
  OtherEnd: TBoldObjectReference;
  ObjectReference: TBoldObjectReference;
begin
  ListIsEnclosure := true;
  i := 0;
  while i < ObjectList.Count do
  begin
    OldCount := ObjectList.Count;
    ExamineBoldObject := ObjectList[i];
    if OldCount<=ObjectList.Count then
      Inc(i);
    for J := 0 to ExamineBoldObject.BoldClassTypeInfo.AllRoles.Count - 1 do
    begin
      RoleRTInfo := ExamineBoldObject.BoldClassTypeInfo.AllRoles[J];
      M := RoleRTInfo.Index;
      if RoleRTInfo.IsSingleRole and RoleRTInfo.Persistent and RoleRTInfo.IsStoredInObject and
         ExamineBoldObject.BoldMemberAssigned[M] and (ExamineBoldObject.BoldMembers[M].BoldPersistenceState = bvpsModified) then
      begin
        ObjectReference := ExamineBoldObject.BoldMembers[M] as TBoldObjectReference;
        if Assigned(ObjectReference.Locator) and not ObjectReference.Locator.ObjectIsPersistent then
          raise EBold.CreateFmt('Can not update Object "%s", role "%s" points to object "%s" which is transient.', [ExamineBoldObject.DisplayName, ExamineBoldObject.BoldMembers[M].BoldMemberRTInfo.ExpressionName, ObjectReference.BoldObject.DisplayName]);
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
    System.OptimisticLockHandler.EnsureEnclosure(ExamineBoldObject, ObjectList, ValidateOnly, ListIsEnclosure);
  end;
  result := ListIsEnclosure;
end;

procedure TBoldSystemPersistenceHandler.FetchMembersWithObjects(
  aBoldObjectList: TBoldObjectList; aBoldMemberIdList: TBoldMemberIdList);
var
  ListToFetch: TBoldObjectList;
  vObject: TBoldObject;
  i,j: integer;
begin
  ListToFetch := TBoldObjectList.Create;
  try
    for i := 0 to aBoldObjectList.Count - 1 do
    begin
      vObject := aBoldObjectList.Locators[i].BoldObject;
      if not Assigned(vObject) then
        ListToFetch.AddLocator(aBoldObjectList.Locators[i])
      else
      if Assigned(aBoldMemberIdList) then
      begin
      for j := 0 to aBoldMemberIdList.Count - 1 do
        if not vObject.BoldMemberAssigned[aBoldMemberIdList[j].MemberIndex] then
        begin
          ListToFetch.Add(vObject);
          break;
        end;
      end
      else
        ListToFetch.Add(vObject);
    end;
    PMFetch(ListToFetch, aBoldMemberIdList);
  finally
    ListToFetch.free;
  end;
end;

procedure TBoldSystemPersistenceHandler.FetchMembersWithObjects(
  aBoldObjectList: TBoldObjectList; AMemberCommaList: string);
var
  sl: TStringList;
  i,j: integer;
  vMemberIdList: TBoldMemberIdList;
  vLeastCommonClassType: TBoldClassTypeInfo;
begin
  if aBoldObjectList.Empty then
    exit;
  sl := TStringList.Create;
  vMemberIdList := TBoldMemberIdList.Create;
  try
    sl.CommaText := AMemberCommaList;
    vLeastCommonClassType := aBoldObjectList.LeastCommonClassType;
    for i := 0 to sl.Count - 1 do
    begin
      j := vLeastCommonClassType.MemberIndexByExpressionName[sl[i]];
      if j = -1 then
        raise EBold.CreateFmt('Member %s not found in class %s.', [sl[i], vLeastCommonClassType.ExpressionName]);
      if vLeastCommonClassType.AllMembers[j].Persistent then
        vMemberIdList.Add(TBoldMemberId.create(j));
    end;
    FetchMembersWithObjects(aBoldObjectList, vMemberIdList);
  finally
    vMemberIdList.free;
    sl.free;
  end;
end;

procedure TBoldSystemPersistenceHandler.FetchLinksWithObjects(ObjectList: TBoldObjectList; const LinkName: string;FetchObjectsInLink: Boolean = True{; const FetchedObjectList: TBoldObjectList = nil});
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
  if not System.BoldPersistent then
    exit;
  ObjectList.EnsureObjects;
  if ObjectList.Count > 0 then
  begin
    Guard := TBoldGuard.Create(FetchList, MemberIdList);
    FetchList := TBoldObjectList.Create;
    FetchList.SubscribeToObjectsInList := False;
    MemberIdList := TBoldMemberIdList.Create;
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
      begin
        if ObjectList.Locators[i].BoldObjectID.NonExisting then
          continue;
        if ObjectList[I].BoldMembers[roleRTInfo.index].BoldPersistenceState = bvpsInvalid then
          FetchList.Add(ObjectList[I]);
      end;
      if FetchList.count > 0 then
      begin
        MemberIdList.Add(TBoldMemberId.Create(MemberRTInfo.index));
        PMFetch(FetchList, MemberIdList);
      end;
    end;
    FetchList.Clear;
    for I := 0 to ObjectList.Count - 1 do
    begin
      if ObjectList.Locators[i].BoldObjectID.NonExisting then
        continue;
      if roleRTInfo.IsMultiRole then
      begin
        Assert(ObjectList[I].BoldMembers[roleRTInfo.index] is TBoldObjectList);
        MultiLink := TBoldObjectList(ObjectList[I].BoldMembers[roleRTInfo.index]);
        for J := 0 to MultiLink.Count - 1 do
          InternalAddLocator(MultiLink.Locators[J]);
      end
      else
      begin
        Assert(ObjectList[I].BoldMembers[roleRTInfo.index] is TBoldObjectReference);
        SingleLink := TBoldObjectReference(ObjectList[I].BoldMembers[roleRTInfo.index]);
        InternalAddLocator(SingleLink.Locator);
      end;
    end;
    if (FetchList.Count>0) and FetchObjectsInLink then
      PMFetch(FetchList, nil);
  end;
end;

procedure TBoldSystemPersistenceHandler.FetchObjectById(BoldObjectId: TBoldObjectId);
var
  ObjectList: TBoldObjectList;
  Guard: IBoldguard;
begin
  Guard := TBoldGuard.Create(ObjectList);
  ObjectList := TBoldObjectList.Create;
  ObjectList.SubscribeToObjectsInList := false;
  ObjectList.duplicateMode := bldmAllow;
  ObjectList.AddLocator(System.EnsuredLocatorByID[BoldObjectID]);
  PMFetch(ObjectList, nil);
end;

procedure TBoldSystemPersistenceHandler.GetAllWithCondition(aList: TBoldObjectList; Condition: TBoldCondition);
var
  I: Integer;
  Locator: TBoldObjectLocator;
  ObjectIdList: TBoldObjectIdList;
  Guard: IBoldGuard;
begin
  if not assigned(PersistenceController) then
    raise EBold.CreateFmt(sNoPersistenceController, ['GetAllWithCondition']);

  Guard := TBoldguard.Create(ObjectidList);
  ObjectIdList := TBoldObjectIdList.Create;
  PersistenceController.PMFetchIDListWithCondition(ObjectIdList, System.AsIBoldvalueSpace[bdepPMIn], fmNormal, Condition, NOTVALIDCLIENTID);
  for I := ObjectIdList.Count - 1 downto 0 do
  begin
    Locator := System.Locators.LocatorByID[ObjectIdList[i]];
    if assigned(Locator) and assigned(Locator.BoldObject) and (Locator.BoldObject.BoldExistenceState = besDeleted) then
      ObjectIdList.RemoveByIndex(i);
  end;
  aList.FillFromIDList(ObjectIdList, system);
end;

procedure TBoldSystemPersistenceHandler.GetAllInClassWithRawSQL(
  aList: TBoldObjectList; AClass: TBoldObjectClass; SQL: String;
  Params: TParams; MaxAnswers, Offset: integer);
var
  Condition: TBoldRawSQLCondition;
  LocalParams: TParams;
  ClasstypeInfo: TBoldClasstypeInfo;
  Guard: IBoldGuard;
begin
  Guard := TBoldGuard.Create(Condition, LocalParams);
  Condition := TBoldRawSQLCondition.Create;
  Condition.SQL := SQL;
  Condition.MaxAnswers := MaxAnswers;
  Condition.Offset := Offset;
  if assigned(aClass) then
  begin
    ClassTypeInfo := System.BoldSystemTypeInfo.TopSortedClasses.ItemsByObjectClass[AClass];
    if Assigned(ClassTypeInfo) then
      Condition.TopSortedIndex := ClassTypeInfo.TopSortedIndex
    else
      raise EBold.CreateFmt('%s.GetAllInClassWithSQL: "%s" is not a class in the model', [classname, aClass.ClassName]);
  end
  else
    raise EBold.CreateFmt('%s.GetAllInClassWithSQL: Must not be called without a class-parameter', [classname]);

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
  GetAllWithCondition(aList, Condition);
end;

procedure TBoldSystemPersistenceHandler.GetAllInClassWithSQL(aList: TBoldObjectList; AClass: TBoldObjectClass; WhereClause, OrderByClause: String; Params: TParams; JoinInheritedTables: Boolean; MaxAnswers: integer; Offset: integer);
var
  Condition: TBoldSQLCondition;
  LocalParams: TParams;
  ClasstypeInfo: TBoldClasstypeInfo;
  Guard: IBoldGuard;
begin
  Guard := TBoldGuard.Create(Condition, LocalParams);
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
  GetAllWithCondition(aList, Condition);
end;

procedure TBoldSystemPersistenceHandler.UpdateDatabaseWithList(ObjectList: TBoldObjectList);
var
  i: integer;
  ObjectsToUpdate: TBoldObjectList;
  aTranslationList: TBoldIdTranslationList;
  ObjectIdList: TBoldObjectIdList;
  failureList: TBoldObjectList;
  Precondition: TBoldOptimisticLockingPrecondition;
  Guard: IBoldGuard;
begin
  if System.IsUpdatingDatabase then
    raise EBold.CreateFmt(sUpdateDbRentry, []);
  if System.InTransaction then
    raise EBold.Create(sCannotUpdateWhileInTransaction);
  if not assigned(PersistenceController) then
    raise EBold.CreateFmt(sNoPersistenceController, ['UpdateDatabaseWithList']);

  Guard := TBoldGuard.Create(ObjectsToUpdate, aTranslationList, ObjectIdList, Precondition, FailureList);


  ObjectsToUpdate := ObjectList.Clone as TBoldObjectList;
  for i := ObjectsToUpdate.Count-1 downto 0 do
    if not assigned(ObjectsToUpdate.Locators[i].BoldObject) or
       not ObjectsToUpdate[i].BoldPersistent then
      ObjectsToUpdate.RemoveByIndex(i);

  if ObjectsToUpdate.Count > 0 then
  begin
    EnsureEnclosure(ObjectsToUpdate, false);
    DoPreUpdate(ObjectsToUpdate);
    if ObjectsToUpdate.Count > 0 then
    begin
      System.DelayObjectDestruction;
      try
        System.IsUpdatingDatabase := True;
        ObjectIdList := ObjectsToUpdate.CreateObjectIdList(true);
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

        if assigned(System.PessimisticLockHandler) and not System.PessimisticLockHandler.EnsureLocks then
          raise EBold.CreateFmt(sRequiredLocksNotHeld, [classname]);
        BoldClearLastfailure;

        try
          if not StartUpdateForAll(ObjectsToUpdate) then
            BoldRaiseLastFailure(System, 'UpdateDatabaseWithList', sStartUpdateFailed); // do not localize

          PersistenceController.PMUpdate(ObjectIdList, System.AsIBoldvalueSpace[bdepPMOut], System.OptimisticLockHandler.OldValues, Precondition, aTranslationList, fTimeStampOfLatestUpdate, fTimeOfLatestUpdate, NOTVALIDCLIENTID);
          if assigned(Precondition) and Precondition.Failed then
          begin
            if assigned(system.OnOptimisticLockingFailed) then
            begin
              FailureList := TBoldObjectList.Create;
              FailureList.FillFromIDList(Precondition.FailureList, System);
              system.OnOptimisticLockingFailed(ObjectsToUpdate, FailureList, Precondition.FailureReason);
            end
            else
              raise EBoldOperationFailedForObjectList.Create(Precondition.FailureReason, [], Precondition.FailureList, System);
          end else
          begin
            for i := 0 to ObjectsToUpdate.Count - 1 do
              ObjectsToUpdate[i].AsIBoldObjectContents[bdepPMIn].TimeStamp := TimeStampOfLatestUpdate;
            EndUpdateForAll(ObjectsToUpdate, aTranslationList);
            if Assigned(System.UndoHandler) then
               System.UndoHandler.PrepareUpdate(ObjectList);
          end;
          DoPostUpdate(ObjectList);
        except
          on e: Exception do
          begin
            if GetBoldLastFailureReason <> nil then
              BoldRaiseLastFailure(System, 'UpdateDatabaseWithlist', e.message)
            else
              raise;
          end;
        end;
      finally
        if assigned(System.PessimisticLockHandler) then
          System.PessimisticLockHandler.ReleaseUnneededRegions;
        System.AllowObjectDestruction;
        System.IsUpdatingDatabase := false;
      end;
    end;
  end;
end;

function TBoldSystemPersistenceHandler.GetTimeStampOfLatestUpdate: TBoldTimeStampType;
begin
  Result := fTimeStampOfLatestUpdate;
end;

function TBoldSystemPersistenceHandler.GetTimeOfLatestUpdate: TDateTime;
begin
  Result := fTimeOfLatestUpdate;
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
  ObjectList.SubscribeToObjectsInList := false;
  ObjectList.DuplicateMode := bldmAllow;
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
  Guard: IBoldguard;
begin
  if not assigned(PersistenceController) then
    raise EBold.CreateFmt(sNoPersistenceController, ['FetchClass']);

  ClassTypeInfo := (ClassList.BoldType as TBoldListTypeInfo).ListElementTypeInfo as TBoldClassTypeInfo;
  Guard := TBoldGuard.Create(ObjectIdList, Condition);
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
begin
  if not assigned(PersistenceController) then
    raise EBold.Create('Unable to PMFetch. No PersistenceController...');
  if (Objectlist.Count > 0) then
  begin
    Guard := TBoldguard.Create(ObjectidList);
    ObjectIdList := Objectlist.CreateObjectIdList(true);
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
