
{ Global compiler directives }
{$include bold.inc}
unit BoldAbstractExternalPersistenceController;

interface
uses
  BoldValueSpaceInterfaces,
  BoldValueInterfaces,
  BoldId,
  Classes,
  BoldCondition,
  BoldTypenameDictionary,
  BoldUpdatePrecondition,
  BoldPersistenceControllerPassThrough,
  BoldDefs,
  BoldMeta;

type
  TBoldAbstractExternalPersistenceController = class(TBoldPersistenceControllerPassThrough)
  private
    fMoldModel: TMoldModel;
    fTypeNameDictionary: TBoldTypeNameDictionary;
    FOnStartUpdates: TNotifyEvent;
    FOnEndUpdates: TNotifyEvent;
    fOnFailUpdates: TNotifyEvent;
    fUpdateBoldDatabaseFirst: boolean;
    procedure SplitObjects(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; ObjectsToPassAlong, ObjectsToHandle: TBoldObjectIdList; var CommonClass: TMoldClass);
  protected
    procedure PrepareFetch(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; MoldClass: TMoldClass; MemberIdList: TBoldMemberIdList; var FetchContext: TObject); virtual;
    procedure PostFetch(FetchContext: TObject; MoldClass: TMoldClass); virtual;
    function HandlesClass(MoldClass: TMoldClass): Boolean; virtual; abstract;
    procedure EnsureObjectsforFetch(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; MemberIdList: TBoldMemberIdList);
    procedure FetchObject(ObjectContent: IBoldObjectContents; MemberIdList: TBoldMemberIdList; FetchContext: TObject; ValueSpace: IBoldValueSpace); virtual; abstract;
    procedure FetchObjects(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; MemberIdList: TBoldMemberIdList);
    procedure CreateObjects(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace); virtual; abstract;
    procedure DeleteObjects(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace); virtual; abstract;
    procedure UpdateObjects(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace); virtual; abstract;
    procedure EnsureMember(ObjectContents: IBoldObjectContents; MoldMember: TMoldMember; MemberIndex: integer);
    procedure HandleAllInstances(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; FetchMode: Integer; Condition: TBoldConditionWithClass; MoldClass: TMoldClass); virtual; abstract;
    procedure StartUpdates; virtual;
    procedure EndUpdates; virtual;
    procedure FailUpdates; virtual;
    function GetMaxFetchBlockSize: integer; virtual;

    property MoldModel: TMoldModel read fMoldModel;
    property TypeNameDictionary: TBoldTypeNameDictionary read fTypeNameDictionary;
    property MaxFetchBlockSize: integer read GetMaxFetchBlockSize;

  public
    constructor Create(MoldModel: TMoldModel; TypeNameDictionary: TBoldTypeNameDictionary; OnStartUpdates, OnEndUpdates, OnFailUpdates: TNotifyEvent; AUpdateBoldDatabaseFirst: boolean);
    procedure PMExactifyIds(ObjectIdList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList; HandleNonExisting: Boolean); override;
    procedure PMFetch(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; MemberIdList: TBoldMemberIdList; FetchMode: Integer; BoldClientID: TBoldClientID); override;
    procedure PMFetchIDListWithCondition(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; FetchMode: Integer; Condition: TBoldCondition; BoldClientID: TBoldClientID); override;
    procedure PMUpdate(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; Old_Values: IBoldValueSpace; Precondition: TBoldUpdatePrecondition; TranslationList: TBoldIdTranslationList; var TimeStamp: TBoldTimeStampType; var TimeOfLatestUpdate: TDateTime; BoldClientID: TBoldClientID); override;
    function KeyForObject(ObjectContents: IBoldObjectContents): IBoldValue;
    function ValueForObject(ObjectContents: IBoldObjectContents; MemberExpressionName: string): IBoldValue;
    property UpdateBoldDatabaseFirst: boolean read fUpdateBoldDatabaseFirst;
  end;


implementation

uses
  BoldUtils,
  SysUtils,
  BoldDefaultStreamNames,
  BoldGuard;

{ TBoldAbstractExternalPersistenceController }

constructor TBoldAbstractExternalPersistenceController.Create(MoldModel: TMoldModel; TypeNameDictionary: TBoldTypeNameDictionary; OnStartUpdates, OnEndUpdates, OnFailUpdates: TNotifyEvent; AUpdateBoldDatabaseFirst: boolean);
begin
  inherited Create;
  fMoldModel := MoldModel;
  fMoldModel.EnsureTopSorted;
  fTypeNameDictionary := TypeNameDictionary;
  FOnStartUpdates := OnStartUpdates;
  fOnFailUpdates := OnFailUpdates;
  FOnEndUpdates := OnEndUpdates;
  FUpdateBoldDatabaseFirst := AUpdateBoldDatabaseFirst;
end;

procedure TBoldAbstractExternalPersistenceController.PMExactifyIds(
  ObjectIdList: TBoldObjectIdList;
  TranslationList: TBoldIdTranslationList; HandleNonExisting: Boolean);
begin
  inherited;
end;

procedure TBoldAbstractExternalPersistenceController.PMFetch(
  ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace;
  MemberIdList: TBoldMemberIdList; FetchMode: Integer;
  BoldClientID: TBoldClientID);
var
  Member: TMoldMember;
  i: integer;
  ObjectsToHandle,
  ObjectsToPassAlong: TBoldObjectIdList;
  MembersToHandle, MembersToPassAlong: TBoldMemberIdList;
  Guard: IBoldguard;
  SuperClass: TMoldClass;
begin
  Guard := TBoldGuard.Create(ObjectsToHandle, ObjectsToPassAlong, MembersToHandle, MembersToPassAlong);
  ObjectsToHandle := TBoldObjectIdlist.Create;
  ObjectsToPassAlong := TBoldObjectIdlist.Create;
  SplitObjects(ObjectIdList, ValueSpace, ObjectsToPassAlong, ObjectsToHandle, SuperClass);

  if assigned(MemberIdList) then
  begin
    MembersToHandle := TBoldMemberIdList.Create;
    MembersToPassAlong := TBoldMemberIdList.create;
    for i := 0 to MemberIdList.Count-1 do
    begin
      Member := SuperClass.AllBoldMembers[MemberIdList[i].MemberIndex];
      if Member.Storage = bsExternal then
        MembersToHandle.Add(TBoldMemberId.Create(MemberIdList[i].MemberIndex))
      else
        MembersToPassAlong.Add(TBoldMemberId.Create(MemberIdList[i].MemberIndex));
    end;
    if MembersToHandle.Count = 0 then
      ObjectsToHandle.Clear;
  end;
  if (ObjectsToPassAlong.Count > 0) and (not assigned(MembersToPAssAlong) or (MembersToPassAlong.Count > 0)) then
    inherited PMFetch(ObjectsToPassAlong, Valuespace, MembersToPassAlong, FetchMode, BoldClientId);
  if ObjectsToHandle.count > 0 then
  begin
    EnsureObjectsforFetch(ObjectsToHandle, ValueSpace, MembersToHandle);
    FetchObjects(ObjectsToHandle, ValueSpace, MembersToHandle);
  end;
end;

procedure TBoldAbstractExternalPersistenceController.PMFetchIDListWithCondition(
  ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace;
  FetchMode: Integer; Condition: TBoldCondition;
  BoldClientID: TBoldClientID);
var
  MoldClass: TMoldClass;
begin
 if Condition.Classtype = TBoldConditionWithClass then
  begin
    MoldClass := MoldModel.Classes[TBoldConditionWithClass(Condition).TopSortedIndex];
    if (MoldClass.Storage <> bsInternal) and HandlesClass(MoldClass) then
      HandleAllInstances(ObjectIdList, ValueSpace, FetchMode, TBoldConditionWithClass(Condition), MoldClass)
    else
      inherited;
  end
  else
    inherited;
end;

procedure TBoldAbstractExternalPersistenceController.PMUpdate(
  ObjectIdList: TBoldObjectIdList; ValueSpace, Old_Values: IBoldValueSpace;
  Precondition: TBoldUpdatePrecondition;
  TranslationList: TBoldIdTranslationList;
  var TimeStamp: TBoldTimeStampType; var TimeOfLatestUpdate: TDateTime; BoldClientID: TBoldClientID);
var
  i: integer;
  ObjectsToHandle,
  ObjectsToPassAlong: TBoldObjectIdList;
  Guard: IBoldguard;
  SuperClass: TMoldClass;
  ObjectsToCreate, ObjectsToDelete, ObjectsToUpdate: TBoldObjectidList;
  ObjectContents: IBoldObjectContents;
  lNeedsExternalTransaction: boolean;
begin
  Guard := TBoldGuard.Create(
    ObjectsToHandle, ObjectsToPassAlong,
    ObjectsToCreate, ObjectsToDelete, ObjectsToUpdate);

  ObjectsToHandle := TBoldObjectIdlist.Create;
  ObjectsToPassAlong := TBoldObjectIdlist.Create;
  ObjectsToCreate := TBoldObjectIdlist.Create;
  ObjectsToDelete := TBoldObjectIdlist.Create;
  ObjectsToUpdate := TBoldObjectIdlist.Create;

  SplitObjects(ObjectIdList, ValueSpace, ObjectsToPassAlong, ObjectsToHandle, SuperClass);

  for i := 0 to ObjectsTohandle.Count-1 do
  begin
    ObjectContents := ValueSpace.ObjectContentsByObjectId[ObjectsTohandle[i]];
    if ObjectContents.BoldPersistenceState = bvpsModified then
    begin
      if ObjectContents.BoldExistenceState = besDeleted then
        ObjectsToDelete.Add(ObjectsTohandle[i])
      else
        ObjectsToCreate.Add(ObjectsTohandle[i]);
    end else
      ObjectsToUpdate.Add(ObjectsTohandle[i]);
  end;
  lNeedsExternalTransaction := ((ObjectsToCreate.Count + ObjectsToDelete.count + ObjectsToUpdate.count) > 0);
  if lNeedsExternalTransaction then
  begin
    StartUpdates;
    StartTransaction;
  end;
  try
    if UpdateBoldDatabaseFirst then
    begin
      inherited PMUpdate(ObjectsToPassAlong, ValueSpace, Old_Values, Precondition, TranslationList, TimeStamp, TimeOfLatestUpdate, BoldClientId);
      ObjectsToCreate.ApplyTranslationList(TranslationList);
    end;
    CreateObjects(ObjectsToCreate, ValueSpace);
    DeleteObjects(ObjectsToDelete, ValueSpace);
    UpdateObjects(ObjectsToUpdate, ValueSpace);
    if not UpdateBoldDatabaseFirst then
    begin
      inherited PMUpdate(ObjectsToPassAlong, ValueSpace, Old_Values, Precondition, TranslationList, TimeStamp, TimeOfLatestUpdate, BoldClientId);
    end;
    if lNeedsExternalTransaction then
    begin
      EndUpdates;
      CommitTransaction;
    end;
  except
    if lNeedsExternalTransaction then
    try
      FailUpdates;
    finally
      RollbackTransaction;
    end;
    raise;
  end;
end;

procedure TBoldAbstractExternalPersistenceController.EnsureObjectsForFetch(
  ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace;
  MemberIdList: TBoldMemberIdList);
var
  ObjectContents: IBoldObjectContents;
  MoldClass: TMoldClass;
  i, j: integer;


begin
  for i := 0 to ObjectIdList.Count-1 do
  begin
    ObjectContents := ValueSpace.EnsuredObjectContentsByObjectId[ObjectIdList[i]];
    MoldClass := MoldModel.Classes[ObjectIdList[i].TopSortedIndex];
    if assigned(MemberIdList) then
    begin
      for j := 0 to MemberIdList.Count-1 do
        EnsureMember(ObjectContents, MoldClass.AllBoldMembers[MemberIdList[j].MemberIndex], MemberIdList[j].MemberIndex);
    end
    else
    begin
      for j := 0 to MoldClass.AllBoldMembers.Count-1 do
        if MoldClass.AllBoldMembers[j].Storage in [bsExternal, bsPartiallyExternal] then
          EnsureMember(ObjectContents, MoldClass.AllBoldMembers[j], j);
    end;
  end;
end;


function TBoldAbstractExternalPersistenceController.KeyForObject(ObjectContents: IBoldObjectContents): IBoldValue;
var
  MoldClass: TMoldClass;
  i: integer;
begin
  result := nil;
  MoldClass := MoldModel.Classes[ObjectContents.ObjectId.TopSortedIndex];
  for i := 0 to MoldClass.AllBoldMembers.count-1 do
  begin
    if MoldClass.AllBoldMembers[i].Storage = bsExternalKey then
    begin
      result := ObjectContents.ValueByIndex[i];
      exit;
    end;
  end;
end;

function TBoldAbstractExternalPersistenceController.ValueForObject(
  ObjectContents: IBoldObjectContents;
  MemberExpressionName: string): IBoldValue;
var
  MoldClass: TMoldClass;
  i: integer;
begin
  result := nil;
  MoldClass := MoldModel.Classes[ObjectContents.ObjectId.TopSortedIndex];
  for i := 0 to MoldClass.AllBoldMembers.count-1 do
  begin
    if SameText(MoldClass.AllBoldMembers[i].ExpandedExpressionName, MemberExpressionName) then
    begin
      result := ObjectContents.ValueByIndex[i];
      exit;
    end;
  end;
end;

procedure TBoldAbstractExternalPersistenceController.EnsureMember(
  ObjectContents: IBoldObjectContents; MoldMember: TMoldMember;
  MemberIndex: integer);
var
  Contentname: String;
  MoldRole: TMoldRole;
  MemberId: tBoldMemberId;
begin
  Contentname := '';
  if MoldMember is TMoldRole then
  begin
    MoldRole := TMoldRole(MoldMember);
    if MoldRole.RoleType = rtInnerLinkRole then
      ContentName := BoldContentName_ObjectIdRef
    else
      if MoldRole.Multi then
        if assigned(MoldRole.Association.LinkClass) then
          ContentName := BoldContentName_ObjectIdListRefPair
        else
          ContentName := BoldContentName_ObjectIdListRef
    else {not multi}
      if assigned(MoldRole.Association.LinkClass) then
        ContentName := BoldContentName_ObjectIdRefPair
      else
        ContentName := BoldContentName_ObjectIdRef;
  end
  else if MoldMember is TMoldAttribute then
  begin
    Contentname := TypeNameDictionary.MappingForModelName[(MoldMember as TMoldAttribute).BoldType].ExpandedContentsName;
  end;
  MemberId := TBoldMemberId.Create(MemberIndex);
  ObjectContents.EnsureMember(MemberId, Contentname);
  MemberId.Free;
end;

procedure TBoldAbstractExternalPersistenceController.SplitObjects(
  ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace;
  ObjectsToPassAlong, ObjectsToHandle: TBoldObjectIdList; var CommonClass: TMoldClass);
var
  MoldClass: TMoldClass;
  i: integer;
  Id: TBoldObjectId;
begin
  CommonClass := nil;
  for i := 0 to ObjectIdList.count-1 do
  begin
    Id := ObjectIdList[i];
    MoldClass := MoldModel.Classes[Id.TopSortedIndex];
    if assigned(CommonClass) then
      CommonClass := CommonClass.LowestCommonSuperClass(MoldClass)
    else
      CommonClass := MoldClass;

    if MoldClass.Storage = bsInternal then
      ObjectsToPassAlong.Add(Id)
    else if not HandlesClass(MoldClass) then
      ObjectsToPassAlong.Add(Id)
    else if MoldClass.Storage = bsExternal then
      ObjectsToHandle.Add(Id)
    else if MoldClass.Storage = bsPartiallyExternal then
    begin
      ObjectsToPassAlong.Add(Id);
      ObjectsToHandle.Add(Id);
    end;
  end;
end;

procedure TBoldAbstractExternalPersistenceController.EndUpdates;
begin
  if assigned(fOnEndUpdates) then
    fOnEndUpdates(self);
end;

procedure TBoldAbstractExternalPersistenceController.StartUpdates;
begin
  if assigned(fOnStartUpdates) then
    fOnStartUpdates(self);
end;

procedure TBoldAbstractExternalPersistenceController.FetchObjects(
  ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace;
  MemberIdList: TBoldMemberIdList);
var
  i: integer;
  ObjectContents: IBoldObjectContents;
  TranslationList: TBoldIdTranslationList;
  Guard: IBoldguard;
  IdLIst: TBoldObjectIdList;
  ActionList: TBoldObjectIdList;
  FetchContext: TObject;
  MoldClass: TMoldClass;
  
  procedure AddToActionList(index: integer);
  begin
    ActionList.Add(IdList[index]);
    IdList.RemoveByIndex(Index);
  end;
begin
  Guard := TBoldGuard.Create(TranslationList, IdList, ActionList);
  ActionList := TBoldObjectIdList.Create;
  TranslationList := TBoldIdTranslationList.Create;

  PMExactifyIds(ObjectIdList, TranslationList, false);
  IdList := ObjectIdList.Clone;
  IdList.ApplyTranslationList(TranslationList);

  while IdList.Count > 0 do
  begin
    AddToActionList(0);
    MoldClass := MoldModel.Classes[ActionList[0].TopSortedIndex];
    for i := IdList.Count-1 downto 0 do
      if IdList[i].TopSortedIndex = ActionList[0].TopSortedIndex then
      begin
        AddToActionList(i);
        if ActionList.Count > MaxFetchBlockSize then
          break;
      end;
    PrepareFetch(ActionList, ValueSpace, MoldClass, MemberIdList, FetchContext);
    for i := 0 to ActionList.Count-1 do
    begin
      ObjectContents := ValueSpace.ObjectContentsByObjectId[ActionList[i]];
      FetchObject(ObjectContents, MemberIdList, FetchContext, ValueSpace);
    end;
    PostFetch(FetchContext, MoldClass);
    ActionLIst.Clear;
  end;
end;

procedure TBoldAbstractExternalPersistenceController.PostFetch(
  FetchContext: TObject; MoldClass: TMoldClass);
begin
end;

procedure TBoldAbstractExternalPersistenceController.PrepareFetch(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; MoldClass: TMoldClass; MemberIdList: TBoldMemberIdList; var FetchContext: TObject);
begin
end;

function TBoldAbstractExternalPersistenceController.GetMaxFetchBlockSize: integer;
begin
  result := 250;
end;

procedure TBoldAbstractExternalPersistenceController.FailUpdates;
begin
  if assigned(fOnFailUpdates) then
    fOnFailUpdates(self);
end;

end.
