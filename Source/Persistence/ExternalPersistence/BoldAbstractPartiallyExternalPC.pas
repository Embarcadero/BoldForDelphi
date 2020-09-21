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
    procedure FindMoldRoleByName(ObjectContents: IBoldObjectContents; ExpressionName: String; var MoldRole: TMoldRole; var Index: integer);
  public
    constructor Create(MoldModel: TMoldModel; TypeNameDictionary: TBoldTypeNameDictionary; OnStartUpdates, OnEndUpdates, OnFailUpdates: TNotifyEvent);
    destructor Destroy; override;
    procedure SetMultiLink(MultiLink: IBoldObjectIdListref; ExternalKeys: TBoldObjectIdList; MoldClassOfOtherEnd: TMoldClass);
    procedure SetSingleLink(SingleLink: IBoldObjectIdRef; ExternalKey: TBoldObjectId; MoldClassOfOtherEnd: TMoldClass);
    procedure TranslateExternalKeysToInternalIds(MoldClass: TMoldClass; ExternalKeys, InternalIds: TBoldObjectIdList);
    property DeletedExternalObjects: TBoldObjectIdList read fDeletedExternalObjects;
    property NewExternalObjects: TBoldObjectIdList read fNewExternalObjects;
  end;

implementation

uses
  BoldGuard,
  SysUtils,
  BoldNameExpander,
  BoldDefaultStreamNames,
  BoldDefs,
  BoldTaggedValueSupport,
  BoldFreeStandingValues,
  BoldUtils,
  ExPeConsts;

{ TBoldAbstractPartiallyExternalPC }

constructor TBoldAbstractPartiallyExternalPC.Create(MoldModel: TMoldModel; TypeNameDictionary: TBoldTypeNameDictionary; OnStartUpdates, OnEndUpdates, OnFailUpdates: TNotifyEvent);
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
begin
  Guard := TBoldguard.Create(TranslationList, NewId, UpdateIdlist);
  TranslationList := TBoldIDTranslationList.Create;
  UpdateIdList := TBoldObjectIdList.Create;

  TopSortedIndex := MoldClass.TopSortedIndex;

  // FIXME: how can we be sure that the object is actually of the type MoldClass?
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

  // insert the external key to the new object so that it can be saved to the internal database
  AssignKeyToObject(MoldClass, NewObject, ExternalKey, ValueSpace);
  NewObject := nil;

  UpdateIdList.Add(NewId);
  NextPersistenceController.PMUpdate(UpdateIdList, ValueSpace, nil, nil, TranslationList, TimeStamp, -1);

  // return the translated ID!
  Id := TranslationList.TranslateToNewId[NewId].Clone;
  Ids.Add(Id);

  // indicate that this is a newly created object if anyone wants to initialize any internal-attributes.
  NewExternalObjects.Add(Id);
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

  result := format('%s.%s in (', [ // do not localize
    BoldExpandName(ExternalKey.MoldClass.Tablename, ExternalKey.MoldClass.Name, xtSQL, -1, nccFalse),
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

procedure TBoldAbstractPartiallyExternalPC.FetchExternalKeysForIDs(
  InternalObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace;
  MoldClass: TMoldClass);
var
  i: integer;
  MemberIdList: TBoldMemberIdList;
  guard: IBoldGuard;
begin
  Guard := TBoldGuard.Create(MemberIdList);
  MemberIdList := tBoldMemberIdList.Create;
  for i := 0 to MoldClass.AllBoldMembers.count-1 do
    if MoldClass.AllBoldMembers[i].Storage = bsExternalKey then
      MemberIdList.Add(TBoldMemberId.Create(i));
  NextPersistenceController.PMFetch(InternalObjectIdList, ValueSpace, MemberIdList, fmNormal, -1);
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

  // send the condition query to the internal database and fetch the objects to a local valuespace
  NextPersistenceController.PMFetchIDListWithCondition(InternalObjectIdList, TempValueSpace, fmNormal, Condition, -1);
  // Get the external keys
  GetExternalKeys(MoldClass, ExternalKeys);

  // match the external keys and the internal objects.
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
begin
  // The Internal ids and External keys should be expected to contain references to the same objects.

  Guard := TBoldguard.Create(TranslationList);
  FetchExternalKeysForIDs(InternalIds, ValueSpace, MoldClass);

  TranslationList := TBoldIdTranslationList.Create;
  for i := 0 to InternalIds.Count-1 do
  begin
    ObjectContents := ValueSpace.ObjectContentsByObjectId[InternalIds[i]];
    ExternalKey := GetExternalKeyFromObject(ObjectContents, ValueSpace);
    TranslationList.AddTranslation(ExternalKey, InternalIds[i]);
    // Objects that have been deleted in external database are just logged...
    if not ExternalKeys.IdInList[ExternalKey] then
      DeletedExternalObjects.Add(InternalIds[i]);
    ExternalKey.Free;
  end;

  for i := 0 to ExternalKeys.Count-1 do
  begin
    InternalId := TranslationList.TranslateToNewId[ExternalKeys[i]];
    if assigned(InternalId) and (ExternalKeys[i] <> InternalId) then
    begin
      // the objects that already exist in the internal database can be returned directly
      FoundObjects.Add(InternalId)
    end
    else
    begin
      // the missing objects has to be created in the internal database
      CreateInternalObject(MoldClass, ExternalKeys[i], ValueSpace, FoundObjects)
    end;
  end;
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
    // only perform the existencetest if this is the default-fetch of an object
    // when fetching custommembers we assume this test has already been performed
    if not assigned(MemberIdList) or ExternalKeyExistsInExternalStorage(MoldClass, ExternalKey) then
      ExternalKeys.Add(ExternalKey)
    else
      DeletedExternalObjects.Add(ObjectIdList[i]);
  end;
  PrepareFetchExternal(ExternalKeys, ValueSpace, MoldClass, MemberIdList, FetchContext);
end;

procedure TBoldAbstractPartiallyExternalPC.PrepareFetchExternal(
  ExternalKeys: TBoldObjectIdList; ValueSpace: IBoldValueSpace;
  MoldClass: TMoldClass; MemberIdList: TBoldMemberIdList;
  var FetchContext: TObject);
begin
  // intentionally left blank
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
  // set the member to point to the ID
  if assigned(ExternalKey) then
  begin
    ExternalKeys := TBoldObjectIdList.Create;
    ExternalKeys.Add(ExternalKey);
    InternalIds := TBoldObjectidList.Create;
    TranslateExternalKeysToInternalIds(MoldClassOfOtherEnd, ExternalKeys, InternalIds);
    if InternalIds.Count = 1 then
      SingleLink.SetFromId(InternalIds[0]);
    InternalIds.Free;
    ExternalKeys.Free;
  end
  else
    SingleLink.SetFromId(nil);
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

