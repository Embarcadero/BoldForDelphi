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
  BoldDefs;

type
  { forward declarations }
  TBoldPersistenceControllerSystem = class;

  {-- TBoldPersistenceController --}
  TBoldPersistenceControllerSystem = class(TBoldPersistenceController)
  private
    fBoldSystem: TBoldSystem;
    fLocatorSubscriber: TBoldPassthroughSubscriber;
    fMapping: TBoldIndexableList; // TBoldIdLocatorMapping
    procedure _LocatorDestroyedReceived(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const);
    procedure SetBoldSystem(System: TBoldSystem);
    function GetLocatorById(ObjectId: TBoldObjectId): TBoldObjectLocator;
    function GetIdByLocator(Locator: TBoldObjectLocator): TBoldObjectId;
  public
    constructor Create;
    destructor Destroy; override;
    procedure PMExactifyIds(ObjectIdList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList); override;
    procedure PMFetch(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; MemberIdList: TBoldMemberIdList; FetchMode: Integer; BoldClientID: TBoldClientID); override;
    procedure PMFetchIDListWithCondition(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; FetchMode: Integer; Condition: TBoldCondition; BoldClientID: TBoldClientID); override;
    procedure PMUpdate(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; Old_Values: IBoldValueSpace; Precondition: TBoldUpdatePrecondition; TranslationList: TBoldIdTranslationList; var TimeStamp: TBoldTimeStampType; BoldClientID: TBoldClientID); override;
    procedure PMTranslateToGlobalIds(ObjectIdList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList); override;
    procedure PMTranslateToLocalIds(GlobalIdList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList); override;
    procedure PMSetReadOnlyness(ReadOnlyList, WriteableList: TBoldObjectIdList); override;
    // this info should be stored in separate Mapping model
    procedure ReserveNewIds(ValueSpace: IBoldValueSpace; ObjectIdList: TBoldObjectIdList;
            TranslationList: TBoldIdTranslationList); override;
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
  BoldIndex;

var
  IX_MappingIdIndex: integer = -1;
  IX_MappingLocatorIndex: integer = -1;

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

{ TBoldPersistenceControllerSystem }

constructor TBoldPersistenceControllerSystem.Create;
begin
  fLocatorSubscriber := TBoldPassthroughSubscriber.CreateWithExtendedReceive(_LocatorDestroyedReceived);
  fMapping := TBoldIdLocatorMapping.Create;
end;

destructor TBoldPersistenceControllerSystem.Destroy;
begin
  FreeAndNil(fLocatorSubscriber);
  FreeAndNil(fMapping);
  inherited;
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
  TranslationList: TBoldIdTranslationList);
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
      BoldSystem.PersistenceController.PMExactifyIds(anIdList, aTranslationList);
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

  procedure FetchMember(ObjectContents: IBoldObjectContents; MemberIndex: Integer; BoldMember: TBoldMember);

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
    aMemberId: TBoldMemberId;
  begin
    aMemberId := TBoldMemberID.create(MemberIndex);
    try
      ObjectContents.EnsureMember(aMemberId, BoldMember.AsIBoldValue[bdepContents].ContentName);
    finally
      aMemberId.Free;
    end;
    aValue := ObjectContents.ValueByIndex[MemberIndex];
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
            if aRoleRT.IsIndirect then
            begin
              Ids1 := ExtractNewIdList(BoldMember.OwningObject.BoldMembers[aRoleRT.IndexOfLinkObjectRole] as TBoldObjectList);
              Ids2 := ExtractNewIdList(BoldMember as TBoldObjectList);
              (aValue as IBoldObjectIdListRefPair).SetFromIdLists(Ids1, Ids2);
              Ids1.Free;
              Ids2.Free;
            end else
            begin
              Ids1 := ExtractNewIdList(BoldMember as TBoldObjectList);
              (aValue as IBoldObjectIdListRef).SetFromIdList(Ids1);
              Ids1.Free;
            end;
          end else
          begin
            if aRoleRT.IsIndirect then
              (aValue as IBoldObjectIdRefPair).SetFromIds(
                  TBoldIdLocatorMapping(fMapping).EnsuredIdByLocator((BoldMember.OwningObject.BoldMembers[aRoleRT.IndexOfLinkObjectRole] as TBoldObjectReference).Locator),
                  TBoldIdLocatorMapping(fMapping).EnsuredIdByLocator((BoldMember as TBoldObjectReference).Locator))
            else
              (aValue as IBoldObjectIdRef).SetFromId(
                  TBoldIdLocatorMapping(fMapping).EnsuredIdByLocator((BoldMember as TBoldObjectReference).Locator));
          end;
        end;
      end;
    end;
  end;

var
  i, j: Integer;
  aLocator: TBoldObjectLocator;
  anObject: TBoldObject;
  anObjectContent: IBoldObjectContents;
  aMember: TBoldMember;
  aMemberIndex: Integer;
begin
  for i := 0 to ObjectIdList.Count - 1 do
  begin
    anObjectContent := ValueSpace.EnsuredObjectContentsByObjectId[ObjectIdList[i]];
    aLocator := TBoldIdLocatorMapping(fMapping).LocatorById[ObjectIdList[i]];
    if not assigned(aLocator) then
      anObjectContent.BoldExistenceState := besDeleted
    else
    begin
      anObject := aLocator.EnsuredBoldObject;
      anObjectContent.BoldExistenceState := anObject.BoldExistenceState;
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
          if not aMember.BoldMemberRTInfo.DelayedFetch then
            FetchMember(anObjectContent, j, aMember);
        end;
      end;
    end;
  end;
end;

procedure TBoldPersistenceControllerSystem.PMFetchIDListWithCondition(
  ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace;
  FetchMode: Integer; Condition: TBoldCondition; BoldClientID: TBoldClientID);
var
  anObjectList: TBoldObjectList;
  i: Integer;
begin
  if Condition.classtype = TBoldConditionWithClass then
  begin
    anObjectList := BoldSystem.Classes[TBoldConditionWithClass(condition).TopSortedIndex];
    for i := 0 to anObjectList.Count - 1 do
      ObjectIdList.Add(TBoldIdLocatorMapping(fMapping).EnsuredIdByLocator(anObjectList.locators[i]));
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
  var TimeStamp: TBoldTimeStampType; BoldClientID: TBoldClientID);

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
      Member.AsIBoldValue[bdRemove].AssignContent(Value);  { TODO : Check }
  end;

var
  i, j: Integer;
  anId: TBoldObjectId;
  aLocator: TBoldObjectLocator;
  anObject: TBoldObject;
  aClassName: string;
  NewId: TBoldObjectId;
  anObjectContents: IBoldObjectContents;
  aMember: TBoldMember;
  aValue: IBoldValue;
  OwnsTransList: Boolean;
begin
  // TODO: Support Preconditions.

  if not assigned(TranslationList) then
  begin
    TranslationList := TBoldIDTranslationList.Create;
    OwnsTransList := True;
  end
  else
    OwnsTransList := False;

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
        anObject := nil;
    end;

    if assigned(anObject) then
    begin
      anObjectContents := ValueSpace.EnsuredObjectContentsByObjectId[anId];
      if anObjectContents.BoldExistenceState = besDeleted then
        anObject.Delete
      else
      begin
        for j := 0 to anObject.BoldMemberCount - 1 do
        begin
          aMember := anObject.BoldMembers[j];
          aValue := anObjectContents.ValueByIndex[j];
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
end;

procedure TBoldPersistenceControllerSystem.ReserveNewIds(ValueSpace: IBoldValueSpace; ObjectIdList: TBoldObjectIdList;
            TranslationList: TBoldIdTranslationList);
begin
  // do nothing, but implementation needed, as method is abstract in superclass
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
  TBoldIdLocatorMapping(fMapping).RemoveByLocator((Args[0].VObject as TBoldObject).BoldObjectLocator);
end;

{ TBoldIdLocatorMapping }

procedure TBoldIdLocatorMapping.AddPair(Id: TBoldObjectId; Locator: TBoldObjectLocator);
begin
  Add(TBoldIdLocatorPair.Create(Id, Locator));
end;

constructor TBoldIdLocatorMapping.Create;
begin
  inherited;
  SetIndexVariable(IX_MappingIdIndex, AddIndex(TBoldMappingIdIndex.Create));
  SetIndexVariable(IX_MappingLocatorIndex, AddIndex(TBoldMappingLocatorIndex.Create));
  fNextId := 1;
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

function TBoldIdLocatorMapping.GetPairByLocator(
  Locator: TBoldObjectLocator): TBoldIdLocatorPair;
begin
  result := TBoldIdLocatorPair(Indexes[IX_MappingLocatorIndex].Find(Locator));
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

end.
