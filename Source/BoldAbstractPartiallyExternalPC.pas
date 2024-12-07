
{ Global compiler directives }
{$include bold.inc}
unit BoldAbstractPartiallyExternalPC;

interface

uses
  Classes,
  BoldMeta,
  BoldCondition,
  BoldTypeNameDictionary,
  BoldValueSpaceInterfaces,
  BoldValueInterfaces,
  BoldAbstractExternalPersistenceController,
  BoldId;

type
  TBoldAbstractPartiallyExternalPC = class(TBoldAbstractExternalPersistenceController)
  private
    fDeletedExternalObjects: TBoldObjectIdList;
    fNewExternalObjects: TBoldObjectIdList;
  protected
    function FetchAllMembersWhenFetchingKey(MoldClass: TMoldClass): boolean; virtual;
    function ExternalKeyExistsInExternalStorage(MoldClass: TMoldClass; ExternalKey: TBoldObjectId): Boolean; virtual;
    procedure PrepareFetchExternal(ExternalKeys: TBoldObjectIdList; ValueSpace: IBoldValueSpace; MoldClass: TMoldClass; MemberIdList: TBoldMemberIdList; var FetchContext: TObject); virtual;
    procedure PrepareFetch(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; MoldClass: TMoldClass; MemberIdList: TBoldMemberIdList; var FetchContext: TObject); override;
    procedure HandleAllInstances(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; FetchMode: Integer; Condition: TBoldConditionWithClass; MoldClass: TMoldClass); override;
    procedure GetExternalKeys(MoldClass: TMoldClass; ExternalKeys: TBoldObjectIdList); virtual; abstract;
    procedure AssignKeyToObject(MoldClass: TMoldClass; ObjectContents: IBoldObjectContents; ExternalKey: TBoldObjectId; ValueSpace: IBoldValueSpace); virtual; abstract;
    function GetExternalKeyFromObject(ObjectContents: IBoldObjectContents; ValueSpace: IBoldValueSpace): TBoldObjectId; virtual; abstract;
    procedure MatchObjectsByKeys(MoldClass: TMoldClass; ValueSpace: IBoldValueSpace; InternalIds, ExternalKeys, FoundObjects: TBoldObjectIdList);
    procedure FetchExternalKeysForIDs(InternalObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; MoldClass: TMoldClass);
    procedure TranslateInternalIdsToExternal(InternalIds, ExternalKeys: TBoldObjectIdList; ValueSpace: IBoldValueSpace);
    function ExternalKeysToInternalSQL(MoldClass: TMoldClass; ExternalKeys: TBoldObjectIdList): String; virtual;
    procedure CreateInternalObject(MoldClass: TMoldClass; ExternalKey: TBoldObjectid; ValueSpace: IBoldValueSpace; Ids: TBoldObjectidList);
    procedure CreateInternalObjects(MoldClass: TMoldClass; ExternalIDlist: TBoldObjectIdList; const ValueSpace: IBoldValueSpace; Ids: TBoldObjectidList);
    procedure FindMoldRoleByName(ObjectContents: IBoldObjectContents; ExpressionName: String; var MoldRole: TMoldRole; var Index: integer);
  public
    constructor Create(MoldModel: TMoldModel; TypeNameDictionary: TBoldTypeNameDictionary; OnStartUpdates, OnEndUpdates, OnFailUpdates: TNotifyEvent; AUpdateBoldDatabaseFirst: boolean);
    destructor Destroy; override;
    procedure SetMultiLink(MultiLink: IBoldObjectIdListref; ExternalKeys: TBoldObjectIdList; MoldClassOfOtherEnd: TMoldClass);
    procedure SetSingleLink(SingleLink: IBoldObjectIdRef; ExternalKey: TBoldObjectId; MoldClassOfOtherEnd: TMoldClass);
    procedure TranslateExternalKeysToInternalIds(MoldClass: TMoldClass; ExternalKeys, InternalIds: TBoldObjectIdList);
    property DeletedExternalObjects: TBoldObjectIdList read fDeletedExternalObjects;
    property NewExternalObjects: TBoldObjectIdList read fNewExternalObjects;
  end;

implementation
uses
  SysUtils,

  BoldCoreConsts,
  BoldGuard,
  BoldNameExpander,
  BoldDefaultStreamNames,
  BoldDefs,
  BoldTaggedValueSupport,
  BoldFreeStandingValues,
  BoldUtils;
  
{ TBoldAbstractPartiallyExternalPC }

constructor TBoldAbstractPartiallyExternalPC.Create(MoldModel: TMoldModel; TypeNameDictionary: TBoldTypeNameDictionary; OnStartUpdates, OnEndUpdates, OnFailUpdates: TNotifyEvent; AUpdateBoldDatabaseFirst: boolean);
begin
  inherited;
  fDeletedExternalObjects := TBoldObjectIdList.Create;
  fNewExternalObjects := TBoldObjectIdList.Create;
end;


procedure TBoldAbstractPartiallyExternalPC.CreateInternalObject(
  MoldClass: TMoldClass; ExternalKey: TBoldObjectid; ValueSpace: IBoldValueSpace; Ids: TBoldObjectIdList);
var
  j: integer;
  TopSortedIndex: integer;
  NewId: TBoldObjectId;
  TranslationList: TBoldIDTranslationList;
  NewObject: IBoldObjectContents;
  Id: TBoldObjectId;
  Guard: IBoldGuard;
  UpdateIdList: TBoldObjectIdList;
  TimeStamp: Integer;
  TimeOfLatestUpdate: TDateTime;
begin
  Guard := TBoldguard.Create(TranslationList, NewId, UpdateIdlist);
  TranslationList := TBoldIDTranslationList.Create;
  UpdateIdList := TBoldObjectIdList.Create;

  TopSortedIndex := MoldClass.TopSortedIndex;
  NewId := TBoldInternalObjectId.CreateWithClassID(TopSortedIndex, MoldClass.SubClasses.Count = 0);

  NewObject := ValueSpace.GetEnsuredObjectContentsByObjectId(NewId);
  for j := 0 to MoldClass.AllBoldMembers.Count-1 do
  begin
    if MoldClass.AllBoldMembers[j].Storage in [bsInternal, bsExternalKey] then
    begin
      EnsureMember(NewObject, MoldClass.AllBoldMembers[j], j);
      NewObject.ValueByIndex[j].BoldPersistenceState := bvpsModified;
    end;
  end;
  NewObject.BoldPersistenceState := bvpsModified;
  NewObject.BoldExistenceState := besExisting;
  AssignKeyToObject(MoldClass, NewObject, ExternalKey, ValueSpace);
  NewObject := nil;

  UpdateIdList.Add(NewId);
  TimeOfLatestUpdate := now;
  NextPersistenceController.PMUpdate(UpdateIdList, ValueSpace, nil, nil, TranslationList, TimeStamp, TimeOfLatestUpdate, -1);
  Id := TranslationList.TranslateToNewId[NewId].Clone;
  Ids.Add(Id);
  NewExternalObjects.Add(Id);
end;

procedure TBoldAbstractPartiallyExternalPC.CreateInternalObjects(
  MoldClass: TMoldClass; ExternalIDlist: TBoldObjectIdList;
  const ValueSpace: IBoldValueSpace; Ids: TBoldObjectidList);
var
  i: integer;
  j: integer;
  TopSortedIndex: integer;
  NewId: TBoldObjectId;
  TranslationList: TBoldIDTranslationList;
  NewObject: IBoldObjectContents;
  Id: TBoldObjectId;
  Guard: IBoldGuard;
  UpdateIdList: TBoldObjectIdList;
  TimeStamp: Integer;
  TimeOfLatestUpdate: TDateTime;
begin
  Guard := TBoldguard.Create(TranslationList, NewId, UpdateIdlist);
  TranslationList := TBoldIDTranslationList.Create;
  UpdateIdList := TBoldObjectIdList.Create;

  TopSortedIndex := MoldClass.TopSortedIndex;
  for i := 0 to ExternalIDlist.Count - 1 do
  begin
    NewId := TBoldInternalObjectId.CreateWithClassID(TopSortedIndex, MoldClass.SubClasses.Count = 0);
    NewObject := ValueSpace.GetEnsuredObjectContentsByObjectId(NewId);
    for j := 0 to MoldClass.AllBoldMembers.Count-1 do
    begin
      if MoldClass.AllBoldMembers[j].Storage in [bsInternal, bsExternalKey] then
      begin
        EnsureMember(NewObject, MoldClass.AllBoldMembers[j], j);
        NewObject.ValueByIndex[j].BoldPersistenceState := bvpsModified;
      end;
    end;
    NewObject.BoldPersistenceState := bvpsModified;
    NewObject.BoldExistenceState := besExisting;
    AssignKeyToObject(MoldClass, NewObject, ExternalIDlist[i], ValueSpace);
    NewObject := nil;
    UpdateIdList.Add(NewId);
  end;

  TimeOfLatestUpdate := now;
  NextPersistenceController.PMUpdate(UpdateIdList, ValueSpace, nil, nil, TranslationList, TimeStamp, TimeOfLatestUpdate, -1);

  for i := 0 to ExternalIDlist.Count - 1 do
  begin
    Id := TranslationList.TranslateToNewId[NewId].Clone;
    Ids.Add(Id);
    NewExternalObjects.Add(Id);
  end;
end;

destructor TBoldAbstractPartiallyExternalPC.destroy;
begin
  FreeAndNil(fDeletedExternalObjects);
  FreeAndNil(fNewExternalObjects);
  inherited;
end;

function TBoldAbstractPartiallyExternalPC.ExternalKeysToInternalSQL(
  MoldClass: TMoldClass; ExternalKeys: TBoldObjectIdList): String;
var
  i: integer;
  ExternalKey: TMoldAttribute;
  Mapping: TBoldTypeNameMapping;
  KeyIsString: Boolean;
begin
  ExternalKey := nil;
  for i := 0 to MoldClass.AllBoldMembers.Count-1 do
    if (MoldClass.AllBoldMembers[i] is TMoldAttribute) and (MoldClass.AllBoldMembers[i].Storage = bsExternalKey) then
    begin
      if assigned(ExternalKey) then
        raise EBold.CreateFmt(sNotSupportedWithMultipleKeys, [classname, MoldClass.ExpandedExpressionName])
      else
        ExternalKey := MoldClass.AllBoldMembers[i] as TMoldAttribute;
    end;
  if not assigned(ExternalKey) then
    raise EBold.CreateFmt(sNotSupportedWithNoKeys, [classname, MoldClass.ExpandedExpressionName]);

  result := format('%s.%s in (', [
    BoldExpandName(ExternalKey.MoldClass.ExternalTableName, ExternalKey.MoldClass.Name, xtSQL, -1, nccFalse),
    BoldExpandName(ExternalKey.ColumnName, ExternalKey.name, xtSQL, -1, nccFalse)]);
  Mapping := TypeNameDictionary.MappingForModelName[ExternalKey.BoldType];
  KeyIsString := assigned(Mapping) and SameText(Mapping.ExpandedContentsName, BoldContentName_String);
  for i := 0 to ExternalKeys.Count-1 do
  begin
    if i <> 0 then
      result := result + ', ';
    if KeyIsString then
      result := result + '"' + ExternalKeys[i].AsString + '"'
    else
      result := result + ExternalKeys[i].AsString
  end;
  result := result + ')';
end;

function TBoldAbstractPartiallyExternalPC.FetchAllMembersWhenFetchingKey(MoldClass: TMoldClass): boolean;
begin
  result := false;
end;

procedure TBoldAbstractPartiallyExternalPC.FetchExternalKeysForIDs(
  InternalObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace;
  MoldClass: TMoldClass);
var
  i: integer;
  MemberIdList: TBoldMemberIdList;
  guard: IBoldGuard;
begin
  Guard := TBoldGuard.Create(MemberIdList);
  if FetchAllMembersWhenFetchingKey(MoldClass) then
  begin
    NextPersistenceController.PMFetch(InternalObjectIdList, ValueSpace, nil, fmNormal, -1);
  end
  else
  begin
    MemberIdList := TBoldMemberIdList.Create;
    for i := 0 to MoldClass.AllBoldMembers.count-1 do
      if MoldClass.AllBoldMembers[i].Storage = bsExternalKey then
        MemberIdList.Add(TBoldMemberId.Create(i));
    if MemberIdList.count > 0 then
      NextPersistenceController.PMFetch(InternalObjectIdList, ValueSpace, MemberIdList, fmNormal, -1);
  end;
end;

procedure TBoldAbstractPartiallyExternalPC.FindMoldRoleByName(ObjectContents: IBoldObjectContents;
  ExpressionName: String; var MoldRole: TMoldRole; var Index: integer);
var
  Moldclass: TMoldClass;
  i: integer;
begin
  MoldRole := nil;
  index := -1;
  MoldClass := MoldModel.Classes[ObjectContents.ObjectId.TopSortedIndex];
  for i := 0 to MoldClass.AllBoldMembers.Count-1 do
  begin
    if (MoldClass.AllBoldMembers[i] is TMoldRole) and SameText(MoldClass.AllBoldMembers[i].ExpandedExpressionName, ExpressionName) then
    begin
      MoldRole := MoldClass.AllBoldMembers[i] as TMoldRole;
      index := i;
    end;
  end;
  if index = -1 then
    raise EBold.CreateFmt(sNoSuchRole, [ClassNAme, ExpressionName, MoldClass.ExpandedExpressionName]);
end;

procedure TBoldAbstractPartiallyExternalPC.HandleAllInstances(
  ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace;
  FetchMode: Integer; Condition: TBoldConditionWithClass;
  MoldClass: TMoldClass);
var
  InternalObjectIdList: TBoldObjectIdList;
  ExternalKeys: TBoldObjectIdList;
  Guard1: IBoldGuard;
  TempValueSpace: TBoldFReeStandingValueSpace;
begin
  Guard1 := TBoldGuard.Create(TempValueSpace, InternalObjectIdList, ExternalKeys);

  TempvalueSpace := TBoldFreeStandingvalueSpace.Create;
  InternalObjectIdList := TBoldObjectidList.create;
  ExternalKeys := TBoldObjectIdList.create;
  NextPersistenceController.PMFetchIDListWithCondition(InternalObjectIdList, TempValueSpace, fmNormal, Condition, -1);
  GetExternalKeys(MoldClass, ExternalKeys);
  MatchObjectsByKeys(MoldClass, TempValueSpace, InternalObjectIdList, ExternalKeys, ObjectIdList);
end;

procedure TBoldAbstractPartiallyExternalPC.MatchObjectsByKeys(MoldClass: TMoldClass; ValueSpace: IBoldvalueSpace; InternalIds, ExternalKeys, FoundObjects: TBoldObjectIdList);
var
  i: integer;
  ExternalKey: TBoldObjectId;
  InternalID: TBoldObjectId;
  TranslationList: TBoldIdTranslationList;
  ObjectContents: IBoldObjectContents;
  Guard: IBoldGuard;
  lNotFoundObjects: TBoldObjectIdList;
begin
  Guard := TBoldguard.Create(TranslationList, lNotFoundObjects);
  lNotFoundObjects:= TBoldObjectIdList.Create;
  FetchExternalKeysForIDs(InternalIds, ValueSpace, MoldClass);

  TranslationList := TBoldIdTranslationList.Create;
  for i := 0 to InternalIds.Count-1 do
  begin
    ObjectContents := ValueSpace.EnsuredObjectContentsByObjectId[InternalIds[i]];
    ExternalKey := GetExternalKeyFromObject(ObjectContents, ValueSpace);
    TranslationList.AddTranslation(ExternalKey, InternalIds[i]);
    if not ExternalKeys.IdInList[ExternalKey] then
      DeletedExternalObjects.Add(InternalIds[i]);
    ExternalKey.Free;
  end;

  for i := 0 to ExternalKeys.Count-1 do
  begin
    InternalId := TranslationList.TranslateToNewId[ExternalKeys[i]];
    if assigned(InternalId) and (ExternalKeys[i] <> InternalId) then
    begin
      FoundObjects.Add(InternalId)
    end
    else
    begin
      lNotFoundObjects.Add(ExternalKeys[i]);
    end;
  end;
  if lNotFoundObjects.count > 0 then
    CreateInternalObjects(MoldClass, lNotFoundObjects, ValueSpace, FoundObjects)
end;

function TBoldAbstractPartiallyExternalPC.ExternalKeyExistsInExternalStorage(MoldClass: TMoldClass; ExternalKey: TBoldObjectId): Boolean;
begin
  result := true;
end;

procedure TBoldAbstractPartiallyExternalPC.PrepareFetch(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; MoldClass: TMoldClass; MemberIdList: TBoldMemberIdList; var FetchContext: TObject);
var
  i: integer;
  ObjectContents: IBoldObjectContents;
  ExternalKey: TBoldObjectId;
  ExternalKeys: TBoldObjectIdList;
  Guard: IBoldGuard;
begin
  Guard := TBoldGuard.Create(ExternalKeys);
  ExternalKeys := TBoldObjectidlist.Create;

  for i := 0 to ObjectIdList.count-1 do
  begin
    ObjectContents := ValueSpace.ObjectContentsByObjectId[ObjectIdList[i]];
    ExternalKey := GetExternalKeyFromObject(ObjectContents, ValueSpace);

    if not assigned(MemberIdList) or ExternalKeyExistsInExternalStorage(MoldClass, ExternalKey) then
      ExternalKeys.Add(ExternalKey)
    else
      DeletedExternalObjects.Add(ObjectIdList[i]);
    ExternalKey.free;
  end;
  PrepareFetchExternal(ExternalKeys, ValueSpace, MoldClass, MemberIdList, FetchContext);
end;

procedure TBoldAbstractPartiallyExternalPC.PrepareFetchExternal(
  ExternalKeys: TBoldObjectIdList; ValueSpace: IBoldValueSpace;
  MoldClass: TMoldClass; MemberIdList: TBoldMemberIdList;
  var FetchContext: TObject);
begin
end;

procedure TBoldAbstractPartiallyExternalPC.SetMultiLink(
  MultiLink: IBoldObjectIdListref; ExternalKeys: TBoldObjectIdList; MoldClassOfOtherEnd: TMoldClass);
var
  InternalIds: TBoldObjectIdList;
begin
  InternalIds := TBoldObjectidList.Create;
  TranslateExternalKeysToInternalIds(MoldClassOfOtherEnd, ExternalKeys, InternalIds);
  MultiLink.setFromIdList(InternalIds);
  InternalIds.Free;
end;

procedure TBoldAbstractPartiallyExternalPC.SetSingleLink(
  SingleLink: IBoldObjectidref; ExternalKey: TBoldObjectId; MoldClassOfOtherEnd: TMoldClass);
var
  ExternalKeys: TBoldObjectIdList;
  InternalIds: TBoldObjectIdList;
begin
  if assigned(ExternalKey) then
  begin
    ExternalKeys := TBoldObjectIdList.Create;
    ExternalKeys.Add(ExternalKey);
    InternalIds := TBoldObjectidList.Create;
    TranslateExternalKeysToInternalIds(MoldClassOfOtherEnd, ExternalKeys, InternalIds);
    if InternalIds.Count = 1 then
      SingleLink.SetFromId(InternalIds[0], false);
    InternalIds.Free;
    ExternalKeys.Free;
  end
  else
    SingleLink.SetFromId(nil, false);
end;

procedure TBoldAbstractPartiallyExternalPC.TranslateExternalKeysToInternalIds(
  MoldClass: TMoldClass; ExternalKeys, InternalIds: TBoldObjectIdList);
var
  Condition: TBoldSQLCondition;
  ValueSpace: TBoldFreeStandingValueSpace;
  ActionList: TBoldObjectIdList;
  TempInternalIds: TBoldObjectIdlist;
  i: integer;
  Guard: IBoldGuard;
  procedure ProcessActionlist;
  begin
    Condition.WhereFragment := ExternalKeysToInternalSQL(MoldClass, ActionList);
    NextPersistenceController.PMFetchIDListWithCondition(TempInternalIds, ValueSpace, fmNormal, Condition, -1);
    ActionList.Clear;
  end;

begin
  Guard := TBoldGuard.Create(ValueSpace, COndition, ActionList, TempInternalIds);
  ValueSpace := TBoldFreeStandingvalueSpace.Create;
  Condition := TBoldSQLCondition.Create;
  Condition.TopSortedIndex := MoldClass.TopSortedIndex;
  ActionList := TBoldObjectIdList.create;
  tempInternalIds := TBoldObjectIdList.Create;

  for i := 0 to ExternalKeys.count-1 do
  begin
    ActionList.Add(ExternalKeys[i]);
    if ActionList.Count = GetMaxFetchBlockSize then
      processActionList;
  end;
  if ActionList.Count > 0 then
    ProcessActionList;

  MatchObjectsByKeys(MoldClass, ValueSpace, TempInternalIds, ExternalKeys, InternalIds);
end;

procedure TBoldAbstractPartiallyExternalPC.TranslateInternalIdsToExternal(
  InternalIds, ExternalKeys: TBoldObjectIdList;
  ValueSpace: IBoldValueSpace);
var
  i: integer;
  id: TBoldObjectId;
  ObjectContents: IBoldObjectContents;
begin
  for i := 0 to InternalIds.Count-1 do
  begin
    ObjectContents := ValueSpace.ObjectContentsByObjectId[InternalIds[i]];
    Id := GetExternalKeyFromObject(ObjectContents, ValueSpace);
    ExternalKeys.Add(Id);
    Id.Free;
  end;
end;

end.