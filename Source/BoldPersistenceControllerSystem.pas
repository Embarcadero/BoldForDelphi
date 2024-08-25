
{ Global compiler directives }
{$include bold.inc}
unit BoldPersistenceControllerSystem;

interface

uses
  BoldIndexableList,
  BoldPersistenceController,
  BoldUpdatePrecondition,
  BoldCondition,
  BoldId,
  BoldSystem,
  BoldValueSpaceInterfaces,
  BoldSubscription,
  BoldDefs,
  BoldElements;

type
  { forward declarations }
  TBoldPersistenceControllerSystem = class;

  {-- TBoldPersistenceController --}
  TBoldPersistenceControllerSystem = class(TBoldPersistenceController)
  private
    fBoldSystem: TBoldSystem;
    fLocatorSubscriber: TBoldExtendedPassthroughSubscriber;
    fMapping: TBoldIndexableList;
    procedure _LocatorDestroyedReceived(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const);
    procedure SetBoldSystem(System: TBoldSystem);
    function GetLocatorById(ObjectId: TBoldObjectId): TBoldObjectLocator;
    function GetIdByLocator(Locator: TBoldObjectLocator): TBoldObjectId;
    procedure FetchMember(const ObjectContents: IBoldObjectContents; MemberIndex: Integer; BoldMember: TBoldMember);
  public
    constructor Create;
    destructor Destroy; override;
    procedure PMExactifyIds(ObjectIdList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList; HandleNonExisting: Boolean); override;
    procedure PMFetch(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; MemberIdList: TBoldMemberIdList; FetchMode: Integer; BoldClientID: TBoldClientID); override;
    procedure PMFetchIDListWithCondition(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; FetchMode: Integer; Condition: TBoldCondition; BoldClientID: TBoldClientID); override;
    procedure PMUpdate(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; Old_Values: IBoldValueSpace; Precondition: TBoldUpdatePrecondition; TranslationList: TBoldIdTranslationList; var TimeStamp: TBoldTimeStampType; var TimeOfLatestUpdate: TDateTime; BoldClientID: TBoldClientID); override;
    procedure PMTranslateToGlobalIds(ObjectIdList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList); override;
    procedure PMTranslateToLocalIds(GlobalIdList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList); override;
    procedure PMSetReadOnlyness(ReadOnlyList, WriteableList: TBoldObjectIdList); override;
    procedure ReserveNewIds(ValueSpace: IBoldValueSpace; ObjectIdList: TBoldObjectIdList;
            TranslationList: TBoldIdTranslationList); override;
    function CanEvaluateInPS(sOCL: string; aSystem: TBoldElement;  aContext: TBoldElementTypeInfo = nil; const aVariableList: TBoldExternalVariableList = nil): Boolean; override;
    property BoldSystem: TBoldSystem read fBoldSystem write SetBoldSystem;
    property LocatorById[ObjectId: TBoldObjectId]: TBoldObjectLocator read GetLocatorById;
    property IdByLocator[Locator: TBoldObjectLocator]: TBoldObjectId read GetIdByLocator;
  end;

implementation

uses
  SysUtils,
  BoldValueInterfaces,
  BoldDomainElement,
  BoldDefaultId,
  BoldSystemRT,
  BoldIndex,
  BoldLogHandler,
  BoldGuard;

type
  { TBoldIdLocatorPair }
  TBoldIdLocatorPair = class
  private
    fLocator: TBoldObjectLocator;
    fId: TBoldObjectId;
  public
    constructor Create(Id: TBoldObjectId; Locator: TBoldObjectLocator);
    destructor Destroy; override;
  end;

  { TBoldMappingIdIndex }
  TBoldMappingIdIndex = class(TBoldHashIndex)
  protected
    function HashItem(Item: TObject): Cardinal; override;
    function Match(const Key; Item:TObject):Boolean; override;
    function Hash(const Key): Cardinal; override;
  end;

  { TBoldMappingLocatorIndex }
  TBoldMappingLocatorIndex = class(TBoldHashIndex)
  protected
    function HashItem(Item: TObject): Cardinal; override;
    function Match(const Key; Item:TObject):Boolean; override;
    function Hash(const Key): Cardinal; override;
  end;

  { TBoldIdLocatorMapping }
  TBoldIdLocatorMapping = class(TBoldIndexableList)
  private
    class var IX_MappingIdIndex: integer;
    class var IX_MappingLocatorIndex: integer;
    fNextId: Integer;
    function GetLocatorById(ID: TBoldObjectId): TBoldObjectLocator;
    function GetIdByLocator(Locator: TBoldObjectLocator): TBoldObjectId;
    function GetPairByLocator(Locator: TBoldObjectLocator): TBoldIdLocatorPair;
  public
    constructor Create;
    procedure AddPair(Id: TBoldObjectId; Locator: TBoldObjectLocator);
    procedure ExactifyClassForLocator(Locator: TBoldObjectLocator; ExactClassId: Integer);
    function EnsuredIdByLocator(Locator: TBoldObjectLocator): TBoldObjectId;
    procedure RemoveByLocator(Locator: TBoldObjectLocator);
    property LocatorById[ID: TBoldObjectId]: TBoldObjectLocator read GetLocatorById;
    property IdByLocator[Locator: TBoldObjectLocator]: TBoldObjectId read GetIdByLocator;
  end;

const
  OBJECTIDLOCATORNAME = 'ObjectIdLocator';

{ TBoldIdLocatorMapping }

procedure TBoldIdLocatorMapping.AddPair(Id: TBoldObjectId; Locator: TBoldObjectLocator);
begin
  Add(TBoldIdLocatorPair.Create(Id, Locator));
end;

constructor TBoldIdLocatorMapping.Create;
begin
  inherited;
  IX_MappingIdIndex := -1;
  IX_MappingLocatorIndex := -1;
  SetIndexVariable(IX_MappingIdIndex, AddIndex(TBoldMappingIdIndex.Create));
  SetIndexVariable(IX_MappingLocatorIndex, AddIndex(TBoldMappingLocatorIndex.Create));
  fNextId := 1;
end;

function TBoldIdLocatorMapping.GetPairByLocator(
  Locator: TBoldObjectLocator): TBoldIdLocatorPair;
begin
  result := TBoldIdLocatorPair(Indexes[IX_MappingLocatorIndex].Find(Locator));
end;

function TBoldIdLocatorMapping.GetIdByLocator(Locator: TBoldObjectLocator): TBoldObjectId;
var
  aPair: TBoldIdLocatorPair;
begin
  aPair := GetPairByLocator(Locator);
  if assigned(aPair) then
    result := aPair.fId
  else
    result := nil;
end;

function TBoldIdLocatorMapping.EnsuredIdByLocator(
  Locator: TBoldObjectLocator): TBoldObjectId;
var
  NewId: TBoldDefaultId;
begin
  if not assigned(Locator) then
    result := nil
  else
  begin
    result := IdByLocator[Locator];
    if not assigned(result) then
    begin
      NewId := TBoldDefaultId.CreateWithClassID(Locator.BoldObjectID.TopSortedIndex, Locator.BoldObjectID.TopSortedIndexExact);
      NewId.AsInteger := fNextId;
      inc(fNextId);
      AddPair(NewId, Locator);
      NewId.Free;
      result := IdByLocator[Locator];
    end;
  end;
end;

procedure TBoldIdLocatorMapping.ExactifyClassForLocator(
  Locator: TBoldObjectLocator; ExactClassId: Integer);
var
  NewId: TBoldObjectId;
  aPair: TBoldIdLocatorPair;
begin
  aPair := GetPairByLocator(Locator);
  NewId := aPair.fId.CloneWithClassId(ExactClassId, True);
  aPair.fId.Free;
  aPair.fId := NewId;
end;

function TBoldIdLocatorMapping.GetLocatorById(
  ID: TBoldObjectId): TBoldObjectLocator;
var
  aPair: TBoldIdLocatorPair;
begin
  aPair := TBoldIdLocatorPair(Indexes[IX_MappingIdIndex].Find(Id));
  if assigned(aPair) then
    result := aPair.fLocator
  else
    result := nil;
end;

procedure TBoldIdLocatorMapping.RemoveByLocator(
  Locator: TBoldObjectLocator);
var
  aPair: TBoldIdLocatorPair;
begin
  aPair := GetPairByLocator(Locator);
  if assigned(aPair) then
    Remove(aPair);
end;

{ TBoldMappingIdIndex }

function TBoldMappingIdIndex.Hash(const Key): Cardinal;
begin
   Assert(TObject(Key) is TBoldObjectId);
   Result := TBoldObjectId(Key).Hash;
end;

function TBoldMappingIdIndex.HashItem(Item: TObject): Cardinal;
begin
  result := (Item as TBoldIdLocatorPair).fId.Hash;
end;

function TBoldMappingIdIndex.Match(const Key; Item: TObject): Boolean;
begin
  Assert(TObject(Key) is TBoldObjectId);
  result := TBoldObjectId(Key).IsEqual[TBoldIdLocatorPair(Item).fId];
end;

{ TBoldMappingLocatorIndex }

function TBoldMappingLocatorIndex.Hash(const Key): Cardinal;
begin
  Assert(TObject(Key) is TBoldObjectLocator);
  Result := TBoldObjectLocator(Key).Hash;
end;

function TBoldMappingLocatorIndex.HashItem(Item: TObject): Cardinal;
begin
  result := (Item as TBoldIdLocatorPair).fLocator.Hash;
end;

function TBoldMappingLocatorIndex.Match(const Key; Item:TObject): Boolean;
begin
  Assert(TObject(Key) is TBoldObjectLocator);
  Assert(Item is TBoldIdLocatorPair);
  result := TBoldObjectLocator(Key) = TBoldIdLocatorPair(Item).fLocator;
end;

{ TBoldIdLocatorPair }

constructor TBoldIdLocatorPair.Create(Id: TBoldObjectId;
  Locator: TBoldObjectLocator);
begin
  fLocator := Locator;
  fId := Id.Clone;
end;

destructor TBoldIdLocatorPair.Destroy;
begin
  FreeAndNil(fId);
  inherited;
end;

{ TBoldPersistenceControllerSystem }

function TBoldPersistenceControllerSystem.CanEvaluateInPS(sOCL: string;
  aSystem: TBoldElement; aContext: TBoldElementTypeInfo;
  const aVariableList: TBoldExternalVariableList): Boolean;
begin
  result := BoldSystem.CanEvaluateInPs(sOCL, aContext, aVariableList);
end;

constructor TBoldPersistenceControllerSystem.Create;
begin
  fLocatorSubscriber := TBoldExtendedPassthroughSubscriber.CreateWithExtendedReceive(_LocatorDestroyedReceived);
  fMapping := TBoldIdLocatorMapping.Create;
end;

destructor TBoldPersistenceControllerSystem.Destroy;
begin
  FreeAndNil(fLocatorSubscriber);
  FreeAndNil(fMapping);
  inherited;
end;

procedure TBoldPersistenceControllerSystem.FetchMember(
  const ObjectContents: IBoldObjectContents; MemberIndex: Integer;
  BoldMember: TBoldMember);

  function ExtractNewIdList(ObjectList: TBoldObjectList): TBoldObjectIdList;
  var
    i: Integer;
  begin
    result := TBoldObjectIdList.Create;
    for i := 0 to ObjectList.Count - 1 do
      result.Add(TBoldIdLocatorMapping(fMapping).EnsuredIdByLocator(ObjectList.Locators[i]));
  end;

var
  aValue: IBoldValue;
  aRoleRT: TBoldRoleRTInfo;
  Ids1, Ids2: TBoldObjectIdList;
  Guard: IBoldGuard;
begin
  aValue := BoldMember.AsIBoldValue[bdepContents];
  Assert(Assigned(aValue));
  aValue := ObjectContents.EnsureMemberAndGetValueByIndex(MemberIndex, aValue.ContentName);
  Assert(Assigned(aValue));
  if (aValue.BoldPersistenceState = bvpsInvalid) then
  begin
    if BoldMember.BoldMemberRTInfo.Persistent then
    begin
      BoldMember.EnsureContentsCurrent;
      if not (BoldMember.BoldMemberRTInfo is TBoldRoleRTInfo) then
        aValue.AssignContent(BoldMember.AsIBoldValue[bdepContents])
      else
      begin
        aRoleRT := BoldMember.BoldMemberRTInfo as TBoldRoleRTInfo;
        if aRoleRT.IsMultiRole then
        begin
          Guard := TBoldGuard.Create(Ids1, Ids2);
          if aRoleRT.IsIndirect then
          begin
            Ids1 := ExtractNewIdList(BoldMember.OwningObject.BoldMembers[aRoleRT.IndexOfLinkObjectRole] as TBoldObjectList);
            Ids2 := ExtractNewIdList(BoldMember as TBoldObjectList);
            (aValue as IBoldObjectIdListRefPair).SetFromIdLists(Ids1, Ids2);
          end else
          begin
            Ids1 := ExtractNewIdList(BoldMember as TBoldObjectList);
            (aValue as IBoldObjectIdListRef).SetFromIdList(Ids1);
          end;
        end else
        begin
          if aRoleRT.IsIndirect then
            (aValue as IBoldObjectIdRefPair).SetFromIds(
                TBoldIdLocatorMapping(fMapping).EnsuredIdByLocator((BoldMember.OwningObject.BoldMembers[aRoleRT.IndexOfLinkObjectRole] as TBoldObjectReference).Locator),
                TBoldIdLocatorMapping(fMapping).EnsuredIdByLocator((BoldMember as TBoldObjectReference).Locator))
          else
            (aValue as IBoldObjectIdRef).SetFromId(
                TBoldIdLocatorMapping(fMapping).EnsuredIdByLocator((BoldMember as TBoldObjectReference).Locator)
                , false); //?
        end;
      end;
    end;
  end;
end;


function TBoldPersistenceControllerSystem.GetIdByLocator(
  Locator: TBoldObjectLocator): TBoldObjectId;
begin
  result := TBoldIdLocatorMapping(fMapping).IdByLocator[Locator];
end;

function TBoldPersistenceControllerSystem.GetLocatorById(
  ObjectId: TBoldObjectId): TBoldObjectLocator;
begin
  result := TBoldIdLocatorMapping(fMapping).LocatorById[ObjectId];
end;

procedure TBoldPersistenceControllerSystem.PMExactifyIds(
  ObjectIdList: TBoldObjectIdList;
  TranslationList: TBoldIdTranslationList; HandleNonExisting: Boolean);
var
  i: integer;
  anIdList: TBoldObjectIdList;
  aTranslationList: TBoldIdTranslationList;
  aLocator: TBoldObjectLocator;
  OldId: TBoldObjectId;
begin
  anIdList := TBoldObjectIdList.Create;
  aTranslationList := TBoldIDTranslationList.Create;
  try
    for i := 0 to ObjectIdList.Count - 1 do
      if not ObjectIdList[i].TopSortedIndexExact then
        anIdList.Add(TBoldIdLocatorMapping(fMapping).LocatorById[ObjectIdList[i]].BoldObjectID);

    if anIdList.Count > 0 then
    begin
      BoldSystem.PersistenceController.PMExactifyIds(anIdList, aTranslationList, HandleNonExisting);
      BoldSystem.AsIBoldvalueSpace[bdepContents].ApplytranslationList(aTranslationList);
      for i := 0 to anIdList.Count - 1 do
      begin
        aLocator := BoldSystem.Locators.LocatorByID[anIdList[i]];
        OldId := TBoldIdLocatorMapping(fMapping).IdByLocator[aLocator].Clone;
        try
          TBoldIdLocatorMapping(fMapping).ExactifyClassForLocator(aLocator,
                                                                  aTranslationList.TranslateToNewId[anIdList[i]].TopSortedIndex);
          TranslationList.AddTranslation(OldId, TBoldIdLocatorMapping(fMapping).IdByLocator[aLocator]);
        finally
          OldId.Free;
        end;
      end;
    end;
  finally
    anIdList.Free;
    aTranslationList.Free;
  end;
end;

procedure TBoldPersistenceControllerSystem.PMFetch(
  ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace;
  MemberIdList: TBoldMemberIdList; FetchMode: Integer;
  BoldClientID: TBoldClientID);

var
  i, j: Integer;
  aLocator: TBoldObjectLocator;
  anObject: TBoldObject;
  anObjectContent: IBoldObjectContents;
  aMember: TBoldMember;
  aMemberIndex: Integer;

  FetchIdList: TBoldObjectIdList;
  ExactifyTranslationList: TBoldIdTranslationList;
  lObjectList: TBoldObjectList;
  Locator: TBoldObjectLocator;
begin
  ExactifyTranslationList := TBoldIdTranslationList.Create;
  FetchIdList := ObjectIdList.Clone;
  try
    PMExactifyIDs(FetchIdList, ExactifyTranslationList, false);
    if ExactifyTranslationList.Count > 0 then
    begin
      ValueSpace.ExactifyIDs(ExactifyTranslationList);
      ObjectIdList.ExactifyIds(ExactifyTranslationList);
      FetchIdList.ExactifyIds(ExactifyTranslationList);
    end;

  lObjectList := TBoldObjectList.Create;
  try
    for I := 0 to FetchIdList.Count - 1 do
    begin
      Locator := TBoldIdLocatorMapping(fMapping).LocatorById[FetchIdList[i]];
      if Assigned(Locator) then
        lObjectList.AddLocator(Locator);
    end;
    if not lObjectList.Empty then
      BoldSystem.FetchMembersWithObjects(lObjectList, MemberIdList);
  finally
    lObjectList.free;
  end;

  for i := 0 to FetchIdList.Count - 1 do
  begin
    anObjectContent := ValueSpace.EnsuredObjectContentsByObjectId[FetchIdList[i]];
    aLocator := TBoldIdLocatorMapping(fMapping).LocatorById[FetchIdList[i]];
    if not assigned(aLocator) then
      anObjectContent.BoldExistenceState := besDeleted
    else
    begin
      anObject := aLocator.EnsuredBoldObject;
      anObjectContent.BoldExistenceState := anObject.BoldExistenceState;
      anObjectContent.TimeStamp := anObject.AsIBoldObjectContents[bdepContents].TimeStamp;
      if assigned(MemberIdList) then
      begin
        for j := 0 to MemberIdList.Count - 1 do
        begin
          aMemberIndex := MemberIdList[j].MemberIndex;
          FetchMember(anObjectContent, aMemberIndex, anObject.BoldMembers[aMemberIndex]);
        end;
      end else
      begin
        for j := 0 to anObject.BoldMemberCount - 1 do
        begin
          aMember := anObject.BoldMembers[j];
          if (not aMember.BoldMemberRTInfo.DelayedFetch) and aMember.BoldMemberRTInfo.Persistent then
            FetchMember(anObjectContent, j, aMember);
        end;
      end;
    end;
  end;

  finally
    ExactifyTranslationLIst.Free;
    FetchIdList.Free;
  end;
end;

procedure TBoldPersistenceControllerSystem.PMFetchIDListWithCondition(
  ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace;
  FetchMode: Integer; Condition: TBoldCondition; BoldClientID: TBoldClientID);
var
  anObjectList: TBoldObjectList;
  i: Integer;
  Locator: TBoldObjectLocator;
  FetchIdList: TBoldObjectIdList;
begin
  if Condition.classtype = TBoldConditionWithClass then
  begin
    anObjectList := BoldSystem.Classes[TBoldConditionWithClass(condition).TopSortedIndex];
    for i := 0 to anObjectList.Count - 1 do
      ObjectIdList.Add(TBoldIdLocatorMapping(fMapping).EnsuredIdByLocator(anObjectList.locators[i]));
  end
  else
  begin
    FetchIdList := TBoldObjectIdList.Create;
    try
      BoldSystem.PersistenceController.PMFetchIDListWithCondition(FetchIdList, BoldSystem.AsIBoldvalueSpace[bdepPMIn], FetchMode, Condition, BoldClientId);
      for i := 0 to FetchIdList.Count - 1 do
      begin
        Locator := BoldSystem.EnsuredLocatorByID[FetchIdList[i]];
        Assert(Assigned(Locator));
        ObjectIdList.Add(TBoldIdLocatorMapping(fMapping).EnsuredIdByLocator(Locator));
        ValueSpace.EnsuredObjectContentsByObjectId[ObjectIdList[i]];
      end;
      Assert(FetchIdList.Count = ObjectIdList.Count);
    finally
      FetchIdList.free;
    end;
  end;
end;

procedure TBoldPersistenceControllerSystem.PMSetReadOnlyness(ReadOnlyList,
  WriteableList: TBoldObjectIdList);
begin
  inherited;
end;

procedure TBoldPersistenceControllerSystem.PMTranslateToGlobalIds(
  ObjectIdList: TBoldObjectIdList;
  TranslationList: TBoldIdTranslationList);
begin
  inherited;
end;

procedure TBoldPersistenceControllerSystem.PMTranslateToLocalIds(
  GlobalIdList: TBoldObjectIdList;
  TranslationList: TBoldIdTranslationList);
begin
  inherited;
end;

procedure TBoldPersistenceControllerSystem.PMUpdate(
  ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; Old_Values: IBoldValueSpace;
  Precondition: TBoldUpdatePrecondition;
  TranslationList: TBoldIdTranslationList;
  var TimeStamp: TBoldTimeStampType; var TimeOfLatestUpdate: TDateTime; BoldClientID: TBoldClientID);

  procedure CopyValue(Value: IBoldValue; Member: TBoldMember);
  var
    anIdRef: IBoldObjectIdRef;
    anId: TBoldObjectId;
  begin
    if Member is TBoldObjectReference then
    begin
      if Value.QueryInterface(IBoldObjectIdRef, anIdRef) <> S_OK then
        raise EBoldInternal.CreateFmt('%s.PMUpdate: value does not support IBoldObjectIdRef.', [classname]);
      anId := anIdRef.GetId;
      if assigned(anId) then
        TBoldObjectReference(Member).Locator := TBoldIdLocatorMapping(fMapping).LocatorById[TranslationList.TranslateToNewId[anId]]
      else
        TBoldObjectReference(Member).Locator := nil;
    end
    else
      Member.AsIBoldValue[bdRemove].AssignContent(Value);
  end;

var
  i, j: Integer;
  anId: TBoldObjectId;
  aLocator: TBoldObjectLocator;
  anObject: TBoldObject;
  aClassName: string;
  NewId: TBoldObjectId;
  anObjectContents: IBoldObjectContents;
  anOldObjectContents: IBoldObjectContents;
  aMember: TBoldMember;
  aValue: IBoldValue;
  OwnsTransList: Boolean;
begin
  if BoldSystem.IsProcessingTransactionOrUpdatingDatabase then
    raise EBold.Create('Destination BoldSystem IsProcessingTransactionOrUpdatingDatabase.');
  if BoldSystem.InTransaction then
    raise EBold.Create('Destination BoldSystem InTransaction.');

  if assigned(Precondition) then
  begin // TODO: Implement
    if Precondition is TBoldOptimisticLockingPrecondition then
//    if not EnsurePrecondition(Precondition, translationList) then
//      exit;
  end;
  TimeOfLatestUpdate := now;
  if not assigned(TranslationList) then
  begin
    TranslationList := TBoldIDTranslationList.Create;
    OwnsTransList := True;
  end
  else
    OwnsTransList := False;

  BoldSystem.StartTransaction();
  try
    for i := 0 to ObjectIdList.Count - 1 do
    begin
      anId := ObjectIdList[i];
      if not anId.IsStorable then
      begin
        aClassName := BoldSystem.BoldSystemTypeInfo.TopSortedClasses[anId.TopSortedIndex].ExpressionName;
        anObject := BoldSystem.CreateNewObjectByExpressionName(aClassName);
        NewId := TBoldIdLocatorMapping(fMapping).EnsuredIdByLocator(anObject.BoldObjectLocator);
        TranslationList.AddTranslation(anId, NewId);
      end else
      begin
        aLocator := TBoldIdLocatorMapping(fMapping).LocatorById[anId];
        if assigned(aLocator) then
          anObject := aLocator.BoldObject
        else
          anObject := nil; // means object is deleted in destination system
      end;

      if assigned(anObject) then
      begin
        anOldObjectContents := Old_Values.ObjectContentsByObjectId[anId];
        if Assigned(anOldObjectContents) and (anObject.AsIBoldObjectContents[bdepPMIn].TimeStamp <> anOldObjectContents.TimeStamp) then
        begin
          BoldLog.LogFmt('Optimistic Locking TimeStamp failed for %.', [
            anObject.DisplayName]);
          raise EBold.CreateFmt('Optimistic Locking failed for object:%s.', [anObject.DisplayName]);
        end;
        anObjectContents := ValueSpace.EnsuredObjectContentsByObjectId[anId];
        if anObjectContents.BoldExistenceState = besDeleted then
          anObject.Delete
        else
        begin
          for j := 0 to anObject.BoldMemberCount - 1 do
          begin
            aMember := anObject.BoldMembers[j];
            aValue := anObjectContents.ValueByIndex[j];
            if Assigned(anOldObjectContents) and Assigned(anOldObjectContents.ValueByIndex[j]) and not aMember.IsEqualToValue(anOldObjectContents.ValueByIndex[j]) then
              raise EBold.CreateFmt('Optimistic Locking failed for member %s.', [aMember.DisplayName]);
            if aMember.BoldMemberRTInfo.Persistent and aMember.BoldMemberRTInfo.IsStoredInObject and
               assigned(aValue) and ((aValue.BoldPersistenceState = bvpsModified) or BoldSystem.BoldSystemTypeInfo.UpdateWholeObjects) then
              CopyValue(aValue, aMember);
          end;
        end;
      end;
    end;

    ValueSpace.ApplytranslationList(TranslationList);

    if OwnsTransList then
      TranslationList.Free;
    BoldSystem.CommitTransaction();
  except
    BoldSystem.RollbackTransaction();
    raise;
  end;
end;

procedure TBoldPersistenceControllerSystem.ReserveNewIds(ValueSpace: IBoldValueSpace; ObjectIdList: TBoldObjectIdList;
            TranslationList: TBoldIdTranslationList);
begin
end;

procedure TBoldPersistenceControllerSystem.SetBoldSystem(System: TBoldSystem);
begin
  if fBoldSystem <> System then
  begin
    fBoldSystem := System;
    fLocatorSubscriber.CancelAllSubscriptions;
    if assigned(fBoldSystem) then
      fBoldSystem.AddSmallSubscription(fLocatorSubscriber, [beLocatorDestroying], beLocatorDestroying);
  end;
end;

procedure TBoldPersistenceControllerSystem._LocatorDestroyedReceived(
  Originator: TObject; OriginalEvent: TBoldEvent;
  RequestedEvent: TBoldRequestedEvent;
  const Args: array of const);
begin
  assert(Length(Args) = 1, 'Error in Args');
  TBoldIdLocatorMapping(fMapping).RemoveByLocator((Args[0].VObject as TBoldObjectLocator));
end;

end.
