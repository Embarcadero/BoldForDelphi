
{ Global compiler directives }
{$include bold.inc}
unit BoldExternalPersistenceControllerEventDriven;

interface

uses
  Classes,
  BoldSubscription,
  BoldId,
  BoldValueSpaceInterfaces,
  BoldTypeNameDictionary,
  BoldMeta,
  BoldAbstractPartiallyExternalPC,
  BoldExternalPersistenceSupport,
  BoldExternalPersistenceControllerConfig;

type
  TBoldExternalPersistenceControllerEventDriven = class(TBoldAbstractPartiallyExternalPC)
  private
    FConfig: TBoldExternalPersistenceConfigItems;
    fMaxFetchBlockSize: integer;
    function PersistentObjectFromObjectContents(const Obj: IBoldObjectContents; const ValueSpace: IBoldValueSpace; MoldClass: TMoldClass): IPersistentBoldObject;
  protected
    function FetchAllMembersWhenFetchingKey(MoldClass: TMoldClass): boolean; override;
    procedure PrepareFetchExternal(ExternalKeys: TBoldObjectIdList; ValueSpace: IBoldValueSpace; MoldClass: TMoldClass; MemberIdList: TBoldMemberIdList; var FetchContext: TObject); override;
    procedure PostFetch(FetchContext: TObject; MoldClass: TMoldClass); override;
    function ConfigItemByObjectContents(ObjectContents: IBoldObjectContents): TBoldExternalPersistenceConfigItem;
    procedure FetchObject(ObjectContents: IBoldObjectContents; MemberIdList: TBoldMemberIdList; FetchContext: TObject; ValueSpace: IBoldValueSpace); override;
    function HandlesClass(MoldClass: TMoldClass): Boolean; override;
    function ExternalKeyExistsInExternalStorage(MoldClass: TMoldClass; ExternalKey: TBoldObjectId): Boolean; override;
    procedure AssignKeyToObject(MoldClass: TMoldClass; ObjectContents: IBoldObjectContents; ExternalKey: TBoldObjectId; ValueSpace: IBoldValueSpace); override;
    function ExternalKeysToInternalSQL(MoldClass: TMoldClass; ExternalKeys: TBoldObjectIdList): String; override;
    procedure GetExternalKeys(MoldClass: TMoldClass; ExternalKeys: TBoldObjectIdList); override;
    function GetExternalKeyFromObject(ObjectContents: IBoldObjectContents; ValueSpace: IBoldValueSpace): TBoldObjectId; override;
    procedure CreateObjects(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace); override;
    procedure DeleteObjects(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace); override;
    procedure UpdateObjects(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace); override;
    function GetMaxFetchBlockSize: integer; override;
  public
    constructor Create(MoldModel: TMoldModel; Config: TBoldExternalPersistenceConfigItems; TypeNameDictionary: TBoldTypeNameDictionary; OnStartUpdates, OnEndUpdates, OnFailUpdates: TNotifyEvent; MaxFetchBlockSize: integer; UpdateBoldDatabaseFirst: boolean);
    procedure SubscribeToPersistenceEvents(Subscriber: TBoldSubscriber; Events: TBoldSmallEventSet = []); override;
    property Config: TBoldExternalPersistenceConfigItems read fConfig;
  end;


implementation

uses
  SysUtils,

  BoldCoreConsts,
  BoldDefs,
  BoldValueInterfaces,
  BoldStringId,
  BoldDefaultId;

{ TBoldExternalPersistenceControllerEventDriven }

procedure TBoldExternalPersistenceControllerEventDriven.AssignKeyToObject(MoldClass: TMoldClass; ObjectContents: IBoldObjectContents; ExternalKey: TBoldObjectId; ValueSpace: IBoldValueSpace);
var
  i: integer;
  KeyValue: IBoldValue;
  StrContent: IBoldStringContent;
  IntContent: IBoldIntegerContent;
  ConfigItem: TBoldExternalPersistenceConfigItem;
begin
  ConfigItem := Config.FindExpressionName(MoldClass.ExpandedExpressionName);
  if assigned(ConfigItem.OnAssignKeyToObject) then
    ConfigItem.OnAssignKeyToObject(PersistentObjectFromObjectContents(ObjectContents, ValueSpace, MoldClass), ExternalKey)
  else
  begin
    Keyvalue := nil;
    for i := 0 to MoldClass.AllBoldMembers.Count-1 do
    begin
      if MoldClass.AllBoldMembers[i].Storage = bsExternalKey then
      begin
        if assigned(Keyvalue) then
          raise EBold.createFmt(sAssignKeyValueRequiresOneKey, [MoldClass.Name]);

        KeyValue := ObjectContents.ValueByIndex[i];
        if KeyValue.QueryInterface(IBoldStringContent, StrContent) = S_OK then
          StrContent.AsString := ExternalKey.AsString
        else if KeyValue.QueryInterface(IBoldIntegerContent, IntContent) = S_OK then
          IntContent.AsInteger := StrToInt(ExternalKey.AsString)
        else
          raise EBold.createFmt('Keytype not handled automatically: %s.%s', [MoldClass.Name, MoldClass.AllBoldMembers[i].Name]);
      end;
    end;
  end;
end;

constructor TBoldExternalPersistenceControllerEventDriven.Create(MoldModel: TMoldModel; Config: TBoldExternalPersistenceConfigItems; TypeNameDictionary: TBoldTypeNameDictionary; OnStartUpdates, OnEndUpdates, OnFailUpdates: TNotifyEvent; MaxFetchBlockSize: integer; UpdateBoldDatabaseFirst: boolean);
begin
  inherited Create(MoldModel, TypeNameDictionary, OnStartUpdates, OnEndUpdates, OnFailUpdates, UpdateBoldDatabaseFirst);
  fConfig := Config;
  fMaxFetchBlockSize := MaxFetchBlockSize;
end;

procedure TBoldExternalPersistenceControllerEventDriven.CreateObjects(
  ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace);
var
  i: integer;
  ConfigItem: TBoldExternalPersistenceConfigItem;
  ObjectContents: IBoldObjectContents;
  ExternalKey: TBoldObjectId;
  MoldClass: TMoldClass;
begin
  for i := 0 to ObjectIdList.count-1 do
  begin
    ObjectContents := ValueSpace.ObjectContentsByObjectId[ObjectIdList[i]];
    MoldClass := MoldModel.Classes[ObjectidList[i].TopSortedIndex];
    ConfigItem := ConfigItemByObjectContents(ObjectContents);
    if assigned(ConfigItem) then
    begin
      if not assigned(ConfigItem.OnCreateObject) then
        raise EBold.CreateFmt(sCreateObjectsNotAllowed, [ConfigItem.ExpressionName]);
      ExternalKey := GetExternalKeyFromObject(ObjectContents, ValueSpace);
      ConfigItem.OnCreateObject(PersistentObjectFromObjectContents(ObjectContents, ValueSpace, MoldClass), ExternalKey, ValueSpace);
      ExternalKey.Free;
    end;
  end;

end;

procedure TBoldExternalPersistenceControllerEventDriven.DeleteObjects(
ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace);
var
  i: integer;
  ConfigItem: TBoldExternalPersistenceConfigItem;
  ObjectContents: IBoldObjectContents;
  ExternalKey: TBoldObjectId;
  MoldClass: TMoldClass;
begin
  for i := 0 to ObjectIdList.count-1 do
  begin
    ObjectContents := ValueSpace.ObjectContentsByObjectId[ObjectIdList[i]];
    MoldClass := MoldModel.Classes[ObjectidList[i].TopSortedIndex];
    ConfigItem := ConfigItemByObjectContents(ObjectContents);
    if assigned(ConfigItem) then
    begin
      if not assigned(ConfigItem.OnDeleteObject) then
        raise EBold.CreateFmt(sDeleteObjectsNotAllowed, [ConfigItem.ExpressionName]);
      ExternalKey := GetExternalKeyFromObject(ObjectContents, valueSpace);
      ConfigItem.OnDeleteObject(PersistentObjectFromObjectContents(ObjectContents, ValueSpace, MoldClass), ExternalKey);
      ExternalKey.Free;
    end;
  end;
end;

function TBoldExternalPersistenceControllerEventDriven.FetchAllMembersWhenFetchingKey(
  MoldClass: TMoldClass): boolean;
var
  lConfigItem: TBoldExternalPersistenceConfigItem;
begin
  lConfigItem := Config.FindExpressionName(MoldClass.ExpandedExpressionName);
  result := lConfigItem.FetchAllMembersWhenFetchingKey;
end;

procedure TBoldExternalPersistenceControllerEventDriven.FetchObject(
  ObjectContents: IBoldObjectContents;
  MemberIdList: TBoldMemberIdList; FetchContext: TObject; ValueSpace: IBoldValueSpace);
var
  ConfigItem: TBoldExternalPersistenceConfigItem;
  i: integer;
  MoldClass: TMoldClass;
  ExternalKey: TBoldObjectId;
  PersistentObject: IPersistentBoldObject;
begin
  ConfigItem := ConfigItemByObjectContents(ObjectContents);
  MoldClass := MoldModel.Classes[ObjectContents.ObjectId.TopSortedIndex];
  PersistentObject := PersistentObjectFromObjectContents(ObjectContents, valueSpace, MoldClass);
  ExternalKey := GetExternalKeyFromObject(ObjectContents, valueSpace);
  try
    if assigned(MemberidList) and assigned(ConfigItem.OnReadMember) then
    begin
      for i := 0 to MemberIdList.Count-1 do
        ConfigItem.OnReadMember(PersistentObject, ExternalKey, MoldClass.AllBoldMembers[MemberIdList[i].MemberIndex], FetchContext);
    end
    else
    begin
      if Assigned(ConfigItem.OnReadObject) then
        ConfigItem.OnReadObject(PersistentObject, ExternalKey, FetchContext)
      else
        raise EBold.CreateFmt(sReadObjectNotImplementedForClass, [MoldClass.Name]);
    end;
  finally
    ExternalKey.free;
  end;
  if not assigned(ConfigItem.OnUpdateObject) and not assigned(ConfigItem.OnDeleteObject) then
    ObjectContents.IsReadOnly := true; 
end;

procedure TBoldExternalPersistenceControllerEventDriven.GetExternalKeys(MoldClass: TMoldClass; ExternalKeys: TBoldObjectIdList);
var
  ConfigItem: TBoldExternalPersistenceConfigItem;
begin
  ConfigItem := Config.FindExpressionName(MoldClass.ExpandedExpressionName);
  if assigned(ConfigItem) and assigned(ConfigItem.OngetKeyList) then
    ConfigItem.OnGetKeyList(ExternalKeys)
  else
    raise EBold.CreateFmt(sGetExternalKeyNotImplementedForClass, [MoldClass.Name]);
end;

function TBoldExternalPersistenceControllerEventDriven.HandlesClass(MoldClass: TMoldClass): Boolean;
begin
  result := true;
end;

function TBoldExternalPersistenceControllerEventDriven.ExternalKeyExistsInExternalStorage(MoldClass: TMoldClass; ExternalKey: TBoldObjectId): Boolean;
var
  ConfigItem: TBoldExternalPersistenceConfigItem;
begin
  ConfigItem := Config.FindExpressionName(MoldClass.ExpandedExpressionName);
  if Assigned(ConfigItem.OnExists) then
    result := ConfigItem.OnExists(ExternalKey)
  else
    result := true;
end;

function TBoldExternalPersistenceControllerEventDriven.ConfigItemByObjectContents(
  ObjectContents: IBoldObjectContents): TBoldExternalPersistenceConfigItem;
var
  MoldClass: TMoldClass;
begin
  MoldClass := MoldModel.Classes[ObjectContents.ObjectId.TopSortedIndex];
  result := Config.FindExpressionName(MoldClass.ExpandedExpressionName);
end;

procedure TBoldExternalPersistenceControllerEventDriven.SubscribeToPersistenceEvents(Subscriber: TBoldSubscriber; Events: TBoldSmallEventSet);
begin
  inherited;
end;

procedure TBoldExternalPersistenceControllerEventDriven.UpdateObjects(
ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace);
var
  i: integer;
  ConfigItem: TBoldExternalPersistenceConfigItem;
  ObjectContents: IBoldObjectContents;
  ExternalKey: TBoldObjectId;
  MoldClass: TMoldClass;
begin
  for i := 0 to ObjectIdList.count-1 do
  begin
    ObjectContents := ValueSpace.ObjectContentsByObjectId[ObjectIdList[i]];
    MoldClass := MoldModel.Classes[ObjectidList[i].TopSortedIndex];
    ConfigItem := ConfigItemByObjectContents(ObjectContents);
    if assigned(ConfigItem) then
    begin
      if not assigned(ConfigItem.OnUpdateObject) then
        raise EBold.CreateFmt(sModifyObjectsNotAllowed, [ConfigItem.ExpressionName]);
      ExternalKey := GetExternalKeyFromObject(ObjectContents, ValueSpace);
      ConfigItem.OnUpdateObject(PersistentObjectFromObjectContents(ObjectContents, valueSpace, MoldClass), ExternalKey, ValueSpace);
      ExternalKey.Free;
    end;
  end;
end;

function TBoldExternalPersistenceControllerEventDriven.GetExternalKeyFromObject(
  ObjectContents: IBoldObjectContents; ValueSpace: IBoldValueSpace): TBoldObjectId;
var
  ConfigItem: TBoldExternalPersistenceConfigItem;
  i: integer;
  KeyValue: IBoldValue;
  StrContent: IBoldStringContent;
  IntContent: IBoldIntegerContent;
  DefId: TBoldDefaultId;
  MoldClass: TMoldClass;
  StrId: TBoldStringId;

begin
  ConfigItem := ConfigItemByObjectContents(ObjectContents);
  MoldClass := MoldModel.Classes[ObjectContents.ObjectId.TopSortedIndex];

  if assigned(ConfigItem.OnGetKeyFromObject) then
    result := ConfigItem.OnGetKeyFromObject(PersistentObjectFromObjectContents(ObjectContents, ValueSpace, MoldClass))
  else
  begin
    Result := nil;
    for i := 0 to MoldClass.AllBoldMembers.Count-1 do
    begin
      if MoldClass.AllBoldMembers[i].Storage = bsExternalKey then
      begin
        if assigned(Result) then
          raise EBold.createFmt('AssignKeyValue only supported automatically for classes with one external key: %s', [MoldClass.Name]);

        KeyValue := ObjectContents.ValueByIndex[i];
        if not assigned(KeyValue) then
          raise EBoldInternal.createFmt('Keyvalue not assigned: %s', [MoldClass.Name])
        else if KeyValue.QueryInterface(IBoldStringContent, StrContent) = S_OK then
        begin
          StrId := TBoldStringId.Create;
          StrId.AsString := StrContent.asString;
          result := StrId;
        end
        else if KeyValue.QueryInterface(IBoldIntegerContent, IntContent) = S_OK then
        begin
          DefId := TBoldDefaultId.Create;
          DefId.AsInteger := IntContent.AsInteger;
          result := DefId;
        end
        else
          raise EBold.createFmt('Keytype not handled automatically: %s.%s', [MoldClass.Name, MoldClass.AllBoldMembers[i].Name]);
      end;
    end;
  end;
end;


procedure TBoldExternalPersistenceControllerEventDriven.PostFetch(
  FetchContext: TObject; MoldClass: TMoldClass);
var
  ConfigItem: TBoldExternalPersistenceConfigItem;
begin
  inherited;
  ConfigItem := Config.FindExpressionName(MoldClass.ExpandedExpressionName);
  if assigned(ConfigItem) and Assigned(ConfigItem.OnPostFetch) then
    ConfigItem.OnPostFetch(FetchContext);
end;

procedure TBoldExternalPersistenceControllerEventDriven.PrepareFetchExternal(ExternalKeys: TBoldObjectIdList; ValueSpace: IBoldValueSpace; MoldClass: TMoldClass; MemberIdList: TBoldMemberIdList; var FetchContext: TObject);
var
  ConfigItem: TBoldExternalPersistenceConfigItem;
begin
  inherited;
  ConfigItem := Config.FindExpressionName(MoldClass.ExpandedExpressionName);
  if assigned(ConfigItem) and Assigned(ConfigItem.OnPrepareFetch) then
    ConfigItem.OnPrepareFetch(ExternalKeys, MemberIdList, FetchContext);
end;

function TBoldExternalPersistenceControllerEventDriven.GetMaxFetchBlockSize: integer;
begin
  result := fMaxFetchBlockSize;
end;

function TBoldExternalPersistenceControllerEventDriven.PersistentObjectFromObjectContents(
  const Obj: IBoldObjectContents; const ValueSpace: IBoldValueSpace; MoldClass: TMoldClass): IPersistentBoldObject;
var
  adapterClass: TBoldObjectPersistenceAdapterClass;
begin
  Adapterclass := TBoldObjectPersistenceAdapter.FindRegisteredPersistenceInterface(MoldModel.classes[Obj.ObjectId.TopSortedIndex].ExpandedExpressionName);
  if not assigned(AdapterClass) then
    AdapterClass := TBoldObjectPersistenceAdapter;
  result := AdapterClass.Create(Obj, ValueSpace, MoldClass);
end;

function TBoldExternalPersistenceControllerEventDriven.ExternalKeysToInternalSQL(MoldClass: TMoldClass; ExternalKeys: TBoldObjectIdList): String;
var
  ConfigItem: TBoldExternalPersistenceConfigItem;
begin
  ConfigItem := Config.FindExpressionName(MoldClass.ExpandedExpressionName);
  if assigned(ConfigItem) and assigned(ConfigItem.OnGetInternalSQLForKeys) then
    result := ConfigItem.OnGetInternalSQLForKeys(ExternalKeys)
  else
    result := inherited ExternalKeysToInternalSQL(MoldClass, ExternalKeys);
end;

end.
