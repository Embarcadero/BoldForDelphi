{ Global compiler directives }
{$include bold.inc}
unit BoldPMappers;

interface

uses
  BoldPSParams,
  BoldDefs,
  BoldMeta,
  BoldPMapper,
  BoldSubscription,
  BoldCondition,
  BoldUpdatePrecondition,
  BoldHashIndexes,
  BoldIndexableList,
  BoldPSDescriptions,
  BoldTypeNameDictionary,
  BoldId,
  BoldValueInterfaces,
  BoldAbstractObjectUpgrader,
  BoldLogHandler,
  BoldValueSpaceInterfaces;

type
  {forward declarations}    
  TBoldSystemPersistenceMapper = class;
  TBoldObjectPersistenceMapper = class;
  TBoldMemberPersistenceMapper = class;
  TBoldSystemPersistenceMapperClass = class of TBoldSystemPersistenceMapper;
  TBoldObjectPersistenceMapperClass = class of TBoldObjectPersistenceMapper;
  TBoldMemberPersistenceMapperClass = class of TBoldMemberPersistenceMapper;

  TBoldObjectPersistenceMapperList = class;
  TBoldMemberPersistenceMapperList = class;

  TBoldPreparePSParams = procedure(PSParams: TBoldPSParams) of object;


  { TBoldSystemPersistenceMapper }
  TBoldSystemPersistenceMapper = class(TBoldPersistenceMapper)
  private
    fRootClassObjectPMapper: TBoldObjectPersistenceMapper;
    fPSSystemDescription: TBoldPSSystemDescription;
    fObjectPersistenceMappers: TBoldObjectPersistenceMapperList;
    fPreparePSParams: TBoldPreparePSParams;
    fUseXFiles: Boolean;
    fUseTimeStamp: Boolean;
    fUseGlobalId: Boolean;
    fUseReadOnly: Boolean;
    fUseModelVersion: Boolean;
    fModelVersion: Integer;
    fUseClockLog: Boolean;
    fOnGetCurrentTime: TBoldGetTimeEvent;
    fClockLogGranularity: TDateTime;
    fUpdateWholeObjects: Boolean;
    fObjectUpgrader: TBoldAbstractObjectUpgrader;
    function GetPSSystemDescription: TBoldPSSystemDescription;
    procedure fPMCreate(ObjectIDList: TBoldObjectIdList;
                       const ValueSpace: IBoldValueSpace;
                       TranslationList: TBoldIdTranslationList);
    procedure fPMDelete(ObjectIDList: TBoldObjectIdList;
                       const ValueSpace: IBoldValueSpace;
                       const Old_Values: IBoldValueSpace;
                       TranslationList: TBoldIdTranslationList);
    procedure fPMUpdate(ObjectIDList: TBoldObjectIdList;
                        const ValueSpace: IBoldValueSpace;
                        const Old_Values: IBoldValueSpace;
                        TranslationLIst: TBoldIdTranslationList);
    function GetSupportsObjectUpgrading: Boolean;
  protected
    FCurrentTimeStamp: TBoldTimeStampType;
    fTimeOfTimeStamp: TDateTime;
    procedure InitializePSDescriptions; virtual;
    procedure FillPSParams(PSParams: TBoldPSParams); virtual; abstract;
    function CreatePSParams: TBoldPSParams; virtual; abstract;
    function CreatePSSystemDescription: TBoldPSSystemDescription; virtual; abstract;
    procedure ReserveID; virtual; abstract;
    function NextExternalObjectId(const ValueSpace: IBoldValueSpace;
                                  ObjectID: TBoldObjectId): TBoldObjectId;
    procedure StartTransaction(const ValueSpace: IBoldValueSpace); virtual; abstract;
    procedure Commit(const ValueSpace: IBoldValueSpace); virtual; abstract;
    procedure RollBack(const ValueSpace: IBoldValueSpace); virtual; abstract;

    procedure StartSQLBatch; virtual; abstract;
    procedure EndSQLBatch; virtual; abstract;
    procedure FailSQLBatch; virtual; abstract;

    procedure GetNewTimeStamp; virtual; abstract;
    function EnsurePrecondition(Precondition: TBoldUpdatePrecondition; TranslationList: TBoldIdTranslationList): Boolean; virtual; abstract;
    procedure FetchDeletedObjects(ObjectIdList: TBoldObjectIdList; const ValieSpace: IBoldValueSpace); virtual; abstract;
    procedure EnsurePSDescription;
  public
    constructor CreateFromMold(MoldModel: TMoldModel; TypeNameDictionary: TBoldTypeNameDictionary; DefaultObjectMapperName: string);
    destructor Destroy; override;
    procedure PMFetch(ObjectIDList: TBoldObjectIdList;
                      const ValueSpace: IBoldValueSpace;
                      MemberIdList: TBoldMemberIdList;
                      FetchMode: Integer;
                      TranslationList: TBoldIdTranslationList); virtual;
    procedure PMFetchClassWithCondition(ObjectIDList: TBoldObjectIdList;
                                        const ValueSpace: IBoldValueSpace;
                                        BoldCondition: TBoldCondition;
                                        FetchMode: Integer;
                                        TranslationList: TBoldIdTranslationList); virtual;
    procedure PMUpdate(ObjectIDList: TBoldObjectIdList;
                                       const ValueSpace: IBoldValueSpace;
                                       const Old_Values: IBoldValueSpace;
                                       Precondition: TBoldUpdatePrecondition;
                                       TranslationList: TBoldIdTranslationList;
                                       var TimeStamp: TBoldTimeStampType;
                                       var TimeOfLatestUpdate: TDateTime);
    function CommonSuperClassObjectMapper(ObjectIdList: TBoldObjectIdList): TBoldObjectPersistenceMapper;
    procedure CreatePersistentStorage; override;
    function GetCorrectTime: TDateTime;
    procedure SubscribeToPersistenceEvents(Subscriber: TBoldSubscriber; Events: TBoldSmallEventSet = []);
    procedure ReserveNewIds(const ValueSpace: IBoldValueSpace; ObjectIdList: TBoldObjectIdList;
                            TranslationList: TBoldIdTranslationList);

    property ObjectPersistenceMappers: TBoldObjectPersistenceMapperList
                                                           read fObjectPersistenceMappers;
    property PSSystemDescription: TBoldPSSystemDescription read GetPSSystemDescription;
    property RootClassObjectPersistenceMapper: TBoldObjectPersistenceMapper
                                                    read fRootClassObjectPMapper;
    property OnPreparePSParams: TBoldPreparePSParams read fPreparePSParams write fPreparePSParams;
    property CurrentTimeStamp: TBoldTimeStampType read FCurrentTimeStamp;
    property TimeOfTimeStamp: TDateTime read fTimeOfTimeStamp;
    procedure PMTranslateToGlobalIds(ObjectIdList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList); virtual; abstract;
    procedure PMTranslateToLocalIds(GlobalIdList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList); virtual; abstract;
    procedure PMSetReadonlyness(ReadOnlyList, WriteableList: TBoldObjectIdList); virtual; abstract;
    procedure PMTimestampForTime(ClockTime: TDateTime; var Timestamp: TBoldTimestampType); virtual; abstract;
    procedure PMTimeForTimestamp(Timestamp: TBoldTimestampType; var ClockTime: TDateTime); virtual; abstract;
    property UseXFiles: Boolean read fUseXFiles;
    property UseTimestamp: Boolean read fUseTimestamp;
    property UseGlobalId: Boolean read fUseGlobalId;
    property UseReadOnly: Boolean read fUseReadOnly;
    property UseModelVersion: Boolean read fUseModelVersion;
    property SupportsObjectUpgrading: Boolean read GetSupportsObjectUpgrading;
    property ModelVersion: Integer read fModelVersion;
    property UseClockLog: Boolean read fUseClockLog;
    property OnGetCurrentTime: TBoldGetTimeEvent read fOnGetCurrentTime write fOnGetCurrentTime;
    property ClockLogGranularity: TDateTime read fClockLogGranularity write fClockLogGranularity;
    property UpdateWholeObjects: Boolean read fUpdateWholeObjects;
    property ObjectUpgrader: TBoldAbstractObjectUpgrader read fObjectUpgrader write fObjectUpgrader;
  end;

  { TBoldObjectPersistenceMapper }
  TBoldObjectPersistenceMapper = class(TBoldPersistenceMapper)
  private
    fMemberPersistenceMappers: TBoldMemberPersistenceMapperList;
    fTopSortedIndex: integer;
    fSystemPersistenceMapper: TBoldSystemPersistenceMapper;
    fSuperClass: TBoldObjectPersistencemapper;
    fExpressionName: string;
    fIsLinkClass: Boolean;
    fHasSubclasses: Boolean;
    fVersioned: Boolean;
    fStorage: TBoldStorage;
    fLinkRoleMapperIndex1: integer;
    fLinkRoleMapperIndex2: integer;
    fMemberMapperIndexByMemberIndex: array of integer;
    function GetLinkClassRole1: TBoldMemberPersistenceMapper;
    function GetLinkClassRole2: TBoldMemberPersistenceMapper;
    function LeastCommonSuperClassMapper(ObjectMapper: TBoldObjectPersistenceMapper): TBoldObjectPersistenceMapper;
    function GetMemberMapperIndexByMemberIndex(const MemberIndex: Integer): integer;
  protected
    fObjectIdClass: String;
    procedure FillInMembers(MyMoldClass, CurrentMoldClass: TMoldClass; TypeNameDictionary: TBoldTypeNameDictionary); virtual;
    procedure PMFetchWithCondition(ObjectIDList: TBoldObjectIdList;
                                   const ValueSpace: IBoldValueSpace;
                                   BoldCondition: TBoldCondition;
                                   FetchMode: Integer;
                                   TranslationList: TBoldIdTranslationList); virtual; abstract;
    function NextExternalObjectId(const ValueSpace: IBoldValueSpace;
                                  ObjectID: TBoldObjectId): TBoldObjectId; virtual; abstract;
    procedure InitializePSDescriptions; virtual;
  public
    constructor CreateFromMold(MoldClass: TMoldClass; Owner: TBoldSystemPersistenceMapper; TypeNameDictionary: TBoldTypeNameDictionary); virtual;
    destructor Destroy; override;
    procedure MakeIDsExact(ObjectIDList: TBoldObjectIdList;
                           TranslationList: TBoldIdTranslationList; HandleNonExisting: Boolean); virtual; abstract;
    procedure EnsureIDsExact(ObjectIDList: TBoldObjectIdList;
                             TranslationList: TBoldIdTranslationList; HandleNonExisting: Boolean); virtual;
    procedure PMFetchExactIDList(ObjectIDList: TBoldObjectIdList;
                                 const ValueSpace: IBoldValueSpace;
                                 MemberIdList: TBoldMemberIdList;
                                 FetchMode: Integer;
                                 TranslationList:TBoldIdTranslationList;
                                 MissingList: TBoldObjectIdList); virtual; abstract;
    procedure PMFetchApproximateIDList(ObjectIDList: TBoldObjectIdList;
                                       const ValueSpace: IBoldValueSpace;
                                       MemberIdList: TBoldMemberIdList;
                                       FetchMode: Integer;
                                       TranslationList: TBoldIdTranslationList); virtual;
    procedure PMCreate(ObjectIDList: TBoldObjectIdList;
                       const ValueSpace: IBoldValueSpace;
                       TranslationList:TBoldIdTranslationList); virtual; abstract;
    procedure PMDelete(ObjectIDList: TBoldObjectIdList;
                       const ValueSpace: IBoldValueSpace;
                       const Old_Values: IBoldValueSpace;
                       TranslationList: TBoldIdTranslationList); virtual; abstract;
    procedure PMUpdate(ObjectIDList: TBoldObjectIdList;
                       const ValueSpace: IBoldValueSpace;
                       const Old_Values: IBoldValueSpace;
                       TranslationLIst: TBoldIdTranslationList); virtual; abstract;
    function BoldIsA(ObjectPMapper: TBoldObjectPersistenceMapper): Boolean;
    function LeastCommonSuperMapper(ObjectPMapper: TBoldObjectPersistenceMapper): TBoldObjectPersistenceMapper;
    procedure BuildMemberFetchLists(MemberIdList: TBoldMemberIdList; DefaultFetchMemberList, CustomFetchMemberList: TBoldMemberPersistenceMapperList;  FetchMode: integer);
    procedure CreatePersistentStorage; override;
    property SuperClass: TBoldObjectPersistenceMapper read fSuperClass;
    property TopSortedIndex: integer read fTopSortedIndex;
    property ExpressionName: string read fExpressionName;
    property MemberPersistenceMappers: TBoldMemberPersistenceMapperList read fMemberPersistenceMappers;
    property SystemPersistenceMapper: TBoldSystemPersistenceMapper read fSystemPersistenceMapper;
    property ObjectIdClass: String read fObjectIdClass;
    property HasSubClasses: Boolean read fHasSubClasses;
    property IsLinkClass: Boolean read fIsLinkClass;
    property LinkClassRole1: TBoldMemberPersistenceMapper read GetLinkClassRole1;
    property LinkClassRole2: TBoldMemberPersistenceMapper read GetLinkClassRole2;
    property Versioned: Boolean read fVersioned;
    property MemberMapperIndexByMemberIndex[const MemberIndex: Integer]: integer read GetMemberMapperIndexByMemberIndex;
  end;

  { TBoldMemberPersistenceMapper }
  TBoldMemberPersistenceMapper = class(TBoldPersistenceMapper)
  private
    fObjectPersistenceMapper: TBoldObjectPersistenceMapper;
    fMemberIndex: Integer;
  protected
    fDelayedFetch: Boolean;
    fContentName: string;
    fIsStoredInObject: Boolean;
    fCustomFetch: Boolean;
    fCustomCreateUpDate: Boolean;
    fExpressionname: string;
    procedure InitializePSDescriptions; virtual;
    function GetSupportsPolymorphicFetch: Boolean; virtual;
  public
    constructor CreateFromMold(MoldMember: TMoldMember;
                               MoldClass: TMoldClass;
                               Owner: TBoldObjectPersistenceMapper;
                               const MemberIndex: Integer;
                               TypeNameDictionary: TBoldTypeNameDictionary); virtual;
    class function CanStore(const ContentName: String): Boolean; virtual;
    class function IsCompatibleWith(AMemberPersistenceMapperClass: TBoldMemberPersistenceMapperClass): boolean; virtual;
    property DelayedFetch: Boolean read fDelayedFetch;
    property ObjectPersistenceMapper: TBoldObjectPersistenceMapper read fObjectPersistenceMapper;
    property IsStoredInObject: Boolean read fIsStoredInObject;
    property CustomFetch: Boolean read fCustomfetch;
    property CustomCreateUpDate: Boolean read fCustomCreateUpDate;
    property MemberIndex: Integer read fMemberIndex;
    property ContentName: string read fContentName;
    property ExpressionName: string read fExpressionName;
    function GetValue(const ObjectContents: IBoldObjectContents): IBoldValue;
    function GetEnsuredValue(const ObjectContents: IBoldObjectContents): IBoldValue;
    function IsDirty(const ObjectContents: IBoldObjectContents): Boolean; virtual;
    function ShouldFetch(const ObjectContents: IBoldObjectContents): Boolean; virtual;
    procedure PMCreate(ObjectIdList: TBoldObjectIdList; const ValueSpace: IBoldValueSpace;
                        TranslationList: TBoldIdTranslationList); virtual;
    procedure PMUpdate(ObjectIdList: TBoldObjectIdList; const ValueSpace: IBoldValueSpace;
                        TranslationList: TBoldIdTranslationList); virtual;
    procedure PMDelete(ObjectIdList: TBoldObjectIdList; const ValueSpace: IBoldValueSpace;
                        TranslationList: TBoldIdTranslationList); virtual;
    procedure PMFetch(ObjectIdList: TBoldObjectIdList;
                       const ValueSpace: IBoldValueSpace;
                       FetchMode: Integer;
                       TranslationList: TBoldIdTranslationList; FailureList: TBoldObjectIdList); virtual;
    property SupportsPolymorphicFetch: Boolean read GetSupportsPolymorphicFetch;
  end;

  { TBoldObjectPersistenceMapperList }
  TBoldObjectPersistenceMapperList = class(TBoldIndexableList)
  private
    function GetItem(index: Integer): TBoldObjectPersistenceMapper;
  public
    property Items[index: Integer]: TBoldObjectPersistenceMapper read GetItem; default;
  end;

  { TBoldMemberPersistenceMapperList }
  TBoldMemberPersistenceMapperList = class(TBoldIndexableList)
  private
    class var IX_MemberMapper: integer;
    function GetItem(index: Integer): TBoldmemberPersistenceMapper;
    function GetMemberMapperByExpressionName(ExpressionName: string): TBoldMemberPersistenceMapper;
  public
    constructor Create;
    property Items[index: Integer]: TBoldMemberPersistenceMapper read GetItem; default;
    property MemberMapperByExpressionName[ExpressionName: string]: TBoldMemberPersistenceMapper read GetMemberMapperByExpressionName;
  end;

  { TBoldMemberMapperIndex }
  TBoldMemberMapperIndex = class(TBoldStringHashIndex)
  protected
    function ItemAsKeyString(Item: TObject): string; override;
  end;

procedure BoldPMLog(const s: string);
procedure BoldPMLogFmt(const s: string; const Args: array of const);

var
  BoldPMLogHandler: TBoldLogHandler;

implementation

uses
  SysUtils,
  BoldUtils,
  BoldCoreConsts,
  BoldGuard,
  BoldDefaultStreamNames,
  BoldPMapperLists,
  BoldIsoDateTime;

procedure BoldPMLog(const s: string);
begin
  if assigned(BoldPMLogHandler) then
    BoldPMLogHandler.Log(AsISODateTimeMS(now)+':'+trim(s));
end;

procedure BoldPMLogFmt(const s: string; const Args: array of const);
begin
  if assigned(BoldPMLogHandler) then
    BoldPMLogHandler.LogFmt(AsISODateTimeMS(now)+':'+trim(s), Args);
end;

{ TBoldObjectPersistenceMapperList }
function TBoldObjectPersistenceMapperList.GetItem(index: Integer): TBoldObjectPersistenceMapper;
begin
  Result := TBoldObjectPersistenceMapper(inherited Items[index]);
end;

{ TBoldSystemPersistenceMapper }
constructor TBoldSystemPersistenceMapper.CreateFromMold(MoldModel: TMoldModel; TypeNameDictionary: TBoldTypeNameDictionary; DefaultObjectMapperName: string);
var
  i: integer;
  ObjectPmapper: TBoldObjectPersistenceMapper;
  ObjectMapperName: string;
  ObjectMapperDescriptor: TBoldObjectPersistenceMapperDescriptor;
  MoldClass: TMoldClass;
begin
  inherited;
  MoldModel.EnsureLinkRoles;
  MoldModel.EnsureTopSorted;
  fUseXFiles := MoldModel.UseXFiles;
  fUseTimeStamp := MoldModel.UseTimestamp;
  fUseGlobalId := MoldModel.UseGlobalId;
  fUseReadOnly := MoldModel.UseReadOnly;
  fUseModelVersion := MoldModel.UseModelVersion;
  fModelVersion := MoldModel.ModelVersion;
  fUseClockLog := MoldModel.UseClockLog;
  fUpdateWholeObjects := MoldModel.UpdateWholeObjects;
  fObjectPersistenceMappers := TBoldObjectPersistenceMapperList.Create;
  ObjectPersistenceMappers.Capacity := MoldModel.Classes.Count;
  for i := 0 to MoldModel.Classes.Count - 1 do
  begin
    if MoldModel.Classes[i].EffectivePersistent and (MoldModel.Classes[i].Storage in [bsInternal, bsPartiallyExternal]) then
    begin
      MoldClass := MoldModel.Classes[i];
      ObjectMapperName := MoldClass.PMapperName;
      if BoldNamesEqual(ObjectMapperName, DEFAULTNAMELITERAL) and (DefaultObjectMapperName <> '') then
        ObjectMapperName := DefaultObjectMapperName;      
      ObjectMapperDescriptor := BoldObjectPersistenceMappers.DescriptorByName[ObjectMapperName];
      if not assigned(ObjectMapperDEscriptor) then
        raise EBold.CreateFmt('Unable to find PersistenceMapper (%s) for class %s', [ObjectMapperName, MoldClass.Name]);
      ObjectPMapper := ObjectMapperDescriptor.ObjectPersistenceMapper.CreatefromMold(MoldClass, Self, TypeNameDictionary);
    end
    else
      ObjectPMapper := nil;
    ObjectPersistenceMappers.Add(ObjectPMapper);
  end;
  fRootClassObjectPMapper := ObjectPersistenceMappers[0];
end;

destructor TBoldSystemPersistenceMapper.Destroy;
begin
  FreeAndNil(fObjectPersistenceMappers);
  FreeAndNil(fPSSystemDescription);
  inherited;
end;

procedure TBoldSystemPersistenceMapper.PMFetch(ObjectIDList: TBoldObjectIdList;
                                               const ValueSpace: IBoldValueSpace;
                                               MemberIdList: TBoldMemberIdList;
                                               FetchMode: Integer;
                                               TranslationList: TBoldIdTranslationList);
begin
  RootClassObjectPersistenceMapper.PMFetchApproximateIDList(ObjectIDList, ValueSpace, MemberIdList, FetchMode, TranslationList);
end;

procedure TBoldSystemPersistenceMapper.PMFetchClassWithCondition(ObjectIDList: TBoldObjectIdList;
                                                                 const ValueSpace: IBoldValueSpace;
                                                                 BoldCondition: TBoldCondition;
                                                                 FetchMode: Integer;
                                                                 TranslationList: TBoldIdTranslationList);
begin
  if BoldCondition is TBoldConditionWithClass then
    with ObjectPersistenceMappers[(BoldCondition as TBoldConditionWithClass).TopSortedIndex] do
      PMFetchWithCondition(ObjectIdList, ValueSpace, BoldCondition, FetchMode, TranslationList)
  else
    raise EBold.CreateFmt('%s.PMFetchClassWithCondition: Condition must be a ConditionWithClass, not a %s', [ClassName, BoldCondition.Classname]);
end;

function tBoldSystemPersistenceMapper.NextExternalObjectId(const ValueSpace: IBoldValueSpace; ObjectID: TBoldObjectId): TBoldObjectId;
begin
  result := ObjectPersistenceMappers[ObjectId.TopSortedIndex].NextExternalObjectID(ValueSpace, ObjectId);
end;

procedure TBoldSystemPersistenceMapper.fPMCreate(ObjectIDList: TBoldObjectIdList;
                                                 const ValueSpace: IBoldValueSpace;
                                                 TranslationList:TBoldIdTranslationList);
var
  TempList,
  ActionList: TBoldObjectIdList;
  CurrentPMapper: TBoldObjectPersistenceMapper;
  i: integer;
  g: IBoldGuard;
begin
  g := TBoldGuard.Create(TempList, ActionList);
  tempList := Objectidlist.Clone;
  ActionList := TBoldObjectIdList.Create;
  while TempList.Count > 0 do
  begin
    i := TempList.Count -1;
    CurrentPMapper := ObjectPersistenceMappers[TempList[i].TopSortedIndex];
    ActionList.Clear;
    for i := i downto 0 do
      if ObjectPersistenceMappers[TempList[i].TopSortedIndex] = CurrentPMapper then
      begin
        ActionList.Add(TempList[i]);
        TempList.RemoveByIndex(i);
      end;
    CurrentPMapper.PMCreate(ActionList, ValueSpace, translationlist);
  end;
end;

procedure TBoldSystemPersistenceMapper.fPMDelete(ObjectIDList: TBoldObjectIdList;
                                                 const ValueSpace: IBoldValueSpace;
                                                 const Old_Values: IBoldValueSpace;
                                                 TranslationList: TBoldIdTranslationList);
var
  TempList,
  ActionList: TBoldObjectIdList;
  CurrentPMapper: TBoldObjectPersistenceMapper;
  i: integer;
  g: IBoldGuard;
begin
  g := TBoldGuard.Create(TempList, ActionList);
  tempList := ObjectidList.Clone;
  ActionList := TBoldObjectIdList.Create;
  while TempList.Count > 0 do
  begin
    i := TempList.Count -1;
    CurrentPMapper := ObjectPersistenceMappers[TempList[i].topSortedIndex];
    ActionList.clear;
    for i := i downto 0 do
      if ObjectPersistenceMappers[TempList[i].topSortedIndex] = CurrentPMapper then
      begin
        ActionList.Add(TempList[i]);
        TempList.RemoveByIndex(i);
      end;
    CurrentPMapper.PMDelete(ActionList, ValueSpace, Old_Values, TranslationList);
  end;
end;

procedure TBoldSystemPersistenceMapper.fPMUpdate(ObjectIDList: TBoldObjectIdList;
                                                 const ValueSpace: IBoldValueSpace;
                                                 const Old_Values: IBoldValueSpace;
                                                 TranslationLIst: TBoldIdTranslationList);
var
  TempList,
  ActionList: TBoldObjectIdList;
  CurrentPMapper: TBoldObjectPersistenceMapper;
  i: integer;
  g: IBoldGuard;
begin
  g := TBoldGuard.Create(TempList, ActionList);
  tempList := ObjectidList.Clone;
  ActionList := TBoldObjectIdList.Create;
  while TempList.Count > 0 do
  begin
    i := TempList.Count -1;
    CurrentPMapper := ObjectPersistenceMappers[TempList[i].topSortedIndex];
    ActionList.Clear;
    for i := i downto 0 do
      if ObjectPersistenceMappers[TempList[i].topSortedIndex] = CurrentPMapper then
      begin
        ActionList.Add(TempList[i]);
        TempList.RemoveByIndex(i);
      end;
    CurrentPMapper.PMUpdate(ActionList, ValueSpace, Old_Values, TranslationList);
  end;
end;

procedure TBoldSystemPersistenceMapper.CreatePersistentStorage;
var
  PSParams: TBoldPSParams;
  i: integer;
  g: IBoldGuard;
begin
  g := TBoldGuard.Create(PSParams);
  PSParams := CreatePSParams;
  FillPSParams(PSParams);
  if Assigned(OnPreparePSParams) then
    OnPreparePSParams(PSParams);
  PSSystemDescription.CreatePersistentStorage(PSParams);

  for i := 0 to ObjectPersistenceMappers.Count - 1 do
  begin
    if Assigned(ObjectPersistenceMappers[i]) then
      ObjectPersistenceMappers[i].CreatePersistentStorage;
  end;
end;

procedure TBoldSystemPersistenceMapper.PMUpdate(ObjectIDList: TBoldObjectIdList;
                                                const ValueSpace: IBoldValueSpace;
                                                const Old_Values: IBoldValueSpace;
                                                Precondition: TBoldUpdatePrecondition;
                                                TranslationList: TBoldIdTranslationList;
                                                var TimeStamp: TBoldTimeStampType;
                                                var TimeOfLatestUpdate: TDateTime);
var
  i: Integer;
  ModifiedObjectIDList,
  NewObjectIDList,
  DeletedObjectIDList: TBoldObjectIdList;
  ObjectContents: IBoldObjectContents;
  Guard: IBoldGuard;
begin
  if ObjectIDList.Count = 0 then
    Exit;
  Guard := TBoldGuard.Create(ModifiedObjectIDList, NewObjectIDList, DeletedObjectIDList);
  DeletedObjectIDList := TBoldObjectIdList.Create;
  DeletedObjectIDList.OwnsEntries := false;
  NewObjectIDList := TBoldObjectIdList.Create;
  NewObjectIDList.OwnsEntries := false;
  ModifiedObjectIDList := TBoldObjectIdList.Create;
  ModifiedObjectIDList.OwnsEntries := false;

  // downto order is important here - Daniel
  for i := ObjectIDList.Count - 1 downto 0 do
  begin
    ObjectContents := ValueSpace.ObjectContentsByObjectId[ObjectIdList[i]];
    if ObjectContents.BoldPersistenceState = bvpsModified then
    begin
      if ObjectContents.BoldExistenceState = besDeleted then
        DeletedObjectIDList.AddAndAdopt(ObjectIDList[i])
      else
        NewObjectIDList.AddAndAdopt(ObjectIDList[i]);
    end else
      ModifiedObjectIDList.AddAndAdopt(ObjectIDList[i]);
  end;

  ReserveNewIds(ValueSpace, ObjectIdList, TranslationList);
  if UseTimestamp then
    GetNewTimeStamp;

  StartTransaction(ValueSpace);
  try
    if assigned(Precondition) then
    begin
      if not EnsurePrecondition(Precondition, translationList) then
      begin
        Commit(valueSpace);
        exit;
      end;
    end;

    StartSQLBatch;
    if DeletedObjectIDList.Count > 0 then
      fPMDelete(DeletedObjectIDList, ValueSpace, Old_Values, TranslationList);

    if NewObjectIDList.Count > 0 then
      fPMCreate(NewObjectIDList, ValueSpace, translationList);

    if ModifiedObjectIDList.Count > 0 then
      fPMUpdate(ModifiedObjectIDList, ValueSpace, Old_Values, TranslationList);

    if UseTimestamp then
    begin
      TimeStamp := CurrentTimeStamp;
      TimeOfLatestUpdate := fTimeOfTimeStamp;
    end;
    EndSQLBatch;
    Commit(valueSpace);
  except
    RollBack(ValueSpace);
    FailSQLBatch;
    raise;
  end;
end;

procedure TBoldSystemPersistenceMapper.ReserveNewIds(const ValueSpace: IBoldValueSpace;
                                                     ObjectIdList: TBoldObjectIdList;
                                                     TranslationList: TBoldIdTranslationList);
var
  i: integer;
  newId: TBoldObjectID;
begin
  if ObjectIdList.Count = 0 then
    Exit;
  with ObjectIdList do
  begin
    for i := 0 to Count - 1 do
      if not ObjectIdList[i].IsStorable then
        ReserveId;
    for i := 0 to Count - 1 do
      if not ObjectIDList[i].IsStorable then
      begin
        NewId := NextExternalObjectId(valueSpace, ObjectIdList[i]);
        translationList.AddTranslation(ObjectIdList[i], NewID);
        NewId.Free;
      end;
  end;
end;

{ TBoldObjectPersistenceMapper }
constructor TBoldObjectPersistenceMapper.CreateFromMold(MoldClass: TMoldClass; Owner: TBoldSystemPersistenceMapper; TypeNameDictionary: TBoldTypeNameDictionary);
begin
  inherited;

  fIsLinkClass := Assigned(MoldClass.Association);

  fMemberPersistenceMappers := TBoldMemberPersistenceMapperList.Create;
  fSystemPersistenceMapper := Owner;
  fStorage := MoldClass.Storage;
  fExpressionName := MoldClass.ExpandedExpressionName;
  fTopSortedIndex := MoldClass.TopSortedIndex;
  fLinkRoleMapperIndex1 := -1;
  fLinkRoleMapperIndex2 := -1;
  if MoldClass.Versioned then
        fVersioned := True;

  if MoldClass.Versioned and
     assigned(MoldClass.SuperClass) and
     not MoldClass.SuperClass.Versioned then
    raise EBold.CreateFmt('Class %s is versioned, but not it''s superclass %s', [MoldClass.ExpandedExpressionName, MoldClass.SuperClass.ExpandedExpressionname]);

  if Assigned(MoldClass.SuperClass) then
  begin
    fSuperClass := SystemPersistenceMapper.ObjectPersistencemappers[MoldClass.SuperClass.TopSortedIndex];
    if not Assigned(fSuperClass) then
      raise EBold.CreateFmt('Class %s has a nonpersistent superclass %s', [MoldClass.ExpandedExpressionName, MoldClass.SuperClass.ExpandedExpressionname]);
    SuperClass.fHasSubClasses := true;
  end;

  FillInMembers(MoldClass, MoldClass, TypeNameDictionary);
end;

procedure TBoldObjectPersistenceMapper.CreatePersistentStorage;
var
  i: integer;
begin
  for i := 0 to MemberPersistenceMappers.Count - 1 do
    if assigned(MemberPersistenceMappers[i]) then
      MemberPersistenceMappers[i].CreatePersistentStorage;
end;

procedure TBoldObjectPersistenceMapper.PMFetchApproximateIDList(ObjectIDList: TBoldObjectIdList; const ValueSpace: IBoldValueSpace; MemberIdList: TBoldMemberIdList; FetchMode: Integer; TranslationList: TBoldIdTranslationList);
var
  ActionList,
  MissingList: TBoldObjectIdList;
  TempList, NewTempList: array of TBoldObjectId;
  NewTempCount: integer;
  MemberMapper: TBoldMemberPersistenceMapper;
  MemberMapperIndex: integer;
  TempMapper, CurrentPMapper: TBoldObjectPersistenceMapper;
  i: integer;

  function ObjectMapperForNewID(const OldID: TBoldObjectId; const TranslationList: TBoldIdTranslationList): TBoldObjectPersistenceMapper;
  var
    NewId: TBoldObjectId;
  begin
    if OldId.TopSortedIndexExact then
      NewId := OldID
    else
      NewId := TranslationList.TranslateToNewId[OldID];
    Result := SystemPersistenceMapper.ObjectPersistenceMappers[NewID.topSortedIndex];
  end;

  procedure EnsureActionList;
  begin
   if not Assigned(ActionList) then
     ActionList := TBoldObjectIdList.Create;
  end;

begin
  EnsureIDsExact(ObjectIDList, translationList, false);
  SetLength(TempList, ObjectIdList.Count);
  SetLength(NewTempList, ObjectIdList.Count);
  for i := 0 to ObjectidList.Count - 1 do
    TempList[i] := ObjectIdList[i];
  MissingList := TBoldObjectIdList.Create;
  NewTempCount := 0;
  ActionList := nil;
  try
    while Length(TempList) > 0 do
    begin
      EnsureActionList;
      try
        CurrentPMapper := ObjectMapperForNewID(TempList[0], TranslationList);
        if (Length(TempList) > 1) and Assigned(MemberIdList) and (MemberIdList.Count = 1) then
        begin
          for i := 1 to Length(TempList) - 1 do
          begin
            TempMapper := ObjectMapperForNewID(TempList[i], TranslationList);
            CurrentPMapper := CurrentPMapper.LeastCommonSuperMapper(TempMapper);
          end;
          MemberMapperIndex := CurrentPMapper.MemberMapperIndexByMemberIndex[MemberIdList[0].MemberIndex];
          if MemberMapperIndex <> -1 then
          begin
            MemberMapper := CurrentPMapper.MemberPersistenceMappers[MemberMapperIndex];
            if MemberMapper.SupportsPolymorphicFetch then
            begin
              for i := 0 to Length(TempList) - 1 do
                ActionList.Add(TempList[i]);
              SetLength(TempList, 0);
            end;
          end;
          if ActionList.Count = 0 then
            CurrentPMapper := ObjectMapperForNewID(TempList[0], TranslationList);
        end;

        for i := 0 to Length(TempList) - 1 do
          if ObjectMapperForNewID(TempList[i], TranslationList) = CurrentPMapper then
          begin
            ActionList.Add(TempList[i]);
          end
          else
          begin
            NewTempList[NewTempCount] := TempList[i];
            INC(NewTempCount);
          end;
        CurrentPMapper.PMFetchExactIDList(ActionList, ValueSpace, MemberIdList, FetchMode, TranslationList, MissingList);
      finally
        ActionList.Clear;
      end;
      SetLength(NewTempList, NewTempCount);
      TempList:= NewTempList;
      NewTempList := nil;
      SetLength(NewTempList, NewTempCount);
      NewTempCount := 0;
    end;
    if FetchMode = fmDistributable then
      if MissingList.Count > 0 then
        SystemPersistenceMapper.FetchDeletedObjects(MissingList, ValueSpace);
  finally
    ActionList.free;
    MissingList.Free;
  end;
end;

procedure TBoldObjectPersistenceMapper.EnsureIDsExact(ObjectIDList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList; HandleNonExisting: Boolean);
var
  InexactIDList: TBoldObjectIdList;
  i: integer;
  g: IBoldGuard;
begin
  g := TBoldGuard.Create(InexactIDList);
  InexactIDList := nil;
  for i := 0 to ObjectIDList.Count - 1 do
  begin
    if not ObjectIDList[i].TopSortedIndexExact then
    begin
      if not Assigned(InexactIDList) then
      begin
        InexactIDList := TBoldObjectIdList.Create;
        InexactIDList.Capacity := ObjectIDList.Count-i;
      end;
      InexactIDList.Add(ObjectIDList[i]);
    end;
  end;
  if Assigned(InexactIDList) and (InExactIDList.Count > 0) then
    MakeIDsExact(InexactIDList, TranslationList, HandleNonExisting);
end;

procedure TBoldObjectPersistenceMapper.FillInMembers(MyMoldClass, CurrentMoldClass: TMoldClass; TypeNameDictionary: TBoldTypeNameDictionary);
var
  i: integer;
  MoldMember: TMoldMember;
  MoldRole: TMoldRole;
  MoldAttribute: TMoldAttribute;
  MemberDescriptor: TBoldMemberPersistenceMapperDescriptor;
  IsIndirect: Boolean;
  MapperName: string;
  Mapping: TBoldTypeNameMapping;
begin
  SetLength(fMemberMapperIndexByMemberIndex, MyMoldClass.AllBoldMembers.count);
  for i := 0 to MyMoldClass.AllBoldMembers.count - 1 do
  begin
    MoldMember := MyMoldClass.AllBoldMembers[i] as TMoldMember;
    MemberDescriptor := nil;

    if (MoldMember.EffectivePersistent) and (MoldMember is TMoldAttribute) then
    begin
      MoldAttribute := MoldMember as TMoldAttribute;
      if (fStorage = bsInternal) or (MoldAttribute.Storage in [bsInternal, bsExternalKey]) then
      begin
        MemberDescriptor := BoldMemberPersistenceMappers.DescriptorForModelNameWithDefaultSupport(
          MoldAttribute.BoldType,
          MoldAttribute.PMapperName,
          TypeNameDictionary);

        if not assigned(MemberDescriptor) then
        begin
          Mappername := MoldAttribute.PMapperName;
          if SameText(Mappername, DEFAULTNAME) then
          begin
            Mapping := TypeNameDictionary.MappingForModelName[MoldAttribute.BoldType];
            if assigned(Mapping) then
              MapperName := Mapping.ExpandedMapperName;
          end;

          raise EBold.CreateFmt('Unable to find persistence mapper for %s.%s (type: %s, mapper: %s). possible reasons: '+BOLDCRLF +
            '* Typo'+BOLDCRLF+
            '* The mapper is not correctly installed in the initialization clause'+BOLDCRLF+
            '* The unit with the mapper is not included in the project', [
            MoldAttribute.MoldClass.ExpandedExpressionName,
            MoldAttribute.ExpandedExpressionName,
            MoldAttribute.BoldType,
            MapperName]);
        end;
      end;
    end
    else if MoldMember is TMoldRole then
    begin
      MoldRole := MoldMember as  TMoldRole;
      MemberDescriptor := nil;
      if ((fStorage = bsInternal) or (MoldRole.Association.Storage = bsInternal)) and not MoldRole.Derived then
      begin
        case MoldRole.RoleType of
          rtRole: if MoldRole.EffectivePersistent then
          begin
            if (MoldRole.PMapperName = '') or SameText(MoldRole.PMapperName, DEFAULTNAME) then
            begin
              IsIndirect := assigned(MoldRole.Association.LinkClass);
              if MoldRole.Multi then
                if IsIndirect then
                  MemberDescriptor := BoldMemberPersistenceMappers.DescriptorByDelphiName['TBoldIndirectMultiLinkDefaultMapper']
                else
                  MemberDescriptor := BoldMemberPersistenceMappers.DescriptorByDelphiName['TBoldDirectMultiLinkDefaultMapper']
              else if MoldRole.EffectiveEmbedded then
                MemberDescriptor := BoldMemberPersistenceMappers.DescriptorByDelphiName['TBoldEmbeddedSingleLinkDefaultMapper']
              else
                if IsIndirect then
                  MemberDescriptor := BoldMemberPersistenceMappers.DescriptorByDelphiName['TBoldIndirectSingleLinkDefaultMapper']
                else
                  MemberDescriptor := BoldMemberPersistenceMappers.DescriptorByDelphiName['TBoldDirectSingleLinkDefaultMapper'];
            end
            else
            begin
              MemberDescriptor := BoldMemberPersistenceMappers.DescriptorByDelphiName[MoldRole.PMapperName];
              if not assigned(MemberDescriptor) then
                raise EBold.CreateFmt('Unable to find PersistenceMapper for %s.%s (mapper: %s)', [MoldRole.MoldClass.ExpandedExpressionName, MoldRole.ExpandedExpressionName, MoldRole.PMappername]);
            end;
          end;
          rtLinkRole: if MoldRole.MainRole.EffectivePersistent then
          begin
            if MoldRole.MainRole.Multi then
              MemberDescriptor := BoldMemberPersistenceMappers.DescriptorByDelphiName['TBoldDirectMultiLinkDefaultMapper']
            else
              MemberDescriptor := BoldMemberPersistenceMappers.DescriptorByDelphiName['TBoldDirectSingleLinkDefaultMapper']
          end;
          rtInnerLinkRole: begin
            if MoldRole.EffectivePersistent and CurrentMoldClass.EffectivePersistent then
              MemberDescriptor := BoldMemberPersistenceMappers.DescriptorByDelphiName['TBoldEmbeddedSingleLinkDefaultMapper'];
            if fLinkRoleMapperIndex1 = -1 then
              fLinkRoleMapperIndex1 := MemberPersistenceMappers.Count
            else
              fLinkRoleMapperIndex2 := MemberPersistenceMappers.Count;
          end;
        end;
      end;
    end;
    if assigned(MemberDescriptor) then
    begin
      fMemberMapperIndexByMemberIndex[i] := MemberPersistenceMappers.Count;
      MemberPersistenceMappers.Add(MemberDescriptor.MemberPersistenceMapperClass.CreateFromMold(MoldMember, MyMoldClass, Self, i, TypeNameDictionary));
    end else
      fMemberMapperIndexByMemberIndex[i] := -1;
  end;
end;

destructor TBoldObjectPersistenceMapper.Destroy;
begin
  FreeAndNil(fMemberPersistenceMappers);
  inherited;
end;

function TBoldObjectPersistenceMapper.BoldIsA(ObjectPMapper: TBoldObjectPersistenceMapper): Boolean;
begin
  if self = ObjectPMapper then
    result := true
  else if not assigned(self.fSuperclass) then
    result := false
  else
    result := fSuperClass.BoldIsA(ObjectPMapper);
end;

function TBoldMemberPersistenceMapperList.GetMemberMapperByExpressionName(ExpressionName: string): TBoldMemberPersistenceMapper;
begin
  result := (Indexes[IX_MemberMapper] as TBoldMemberMapperIndex).FindByString(ExpressionName) as TBoldMemberPersistenceMapper;
end;

{ TBoldMemberPersistenceMapper }
class function TBoldMemberPersistenceMapper.CanStore(const ContentName: string): Boolean;
begin
  Result := False;
end;

class function TBoldMemberPersistenceMapper.IsCompatibleWith(AMemberPersistenceMapperClass: TBoldMemberPersistenceMapperClass): boolean;
begin
  // If new mapper inherits from old mapper we assume they are compatible
  Result := InheritsFrom(AMemberPersistenceMapperClass);
end;

constructor TBoldMemberPersistenceMapper.CreateFromMold(Moldmember: TMoldmember; MoldClass: TMoldClass; Owner: TBoldObjectPersistenceMapper; const MemberIndex: Integer; TypeNameDictionary: TBoldTypeNameDictionary);
begin
  inherited;
  fObjectPersistenceMapper := Owner;
  fCustomFetch := false;
  fCustomCreateUpDate := false;
  fMemberIndex := MemberIndex;
  if not assigned(MoldMember) then
    exit;

  fExpressionName := MoldMember.ExpandedExpressionName;
  if assigned(Owner.MemberPersistenceMappers.MemberMapperByExpressionName[fExpressionName]) then
    raise EBold.CreateFmt('Duplicate memberMappers called "%s" in class %s', [fExpressionName, Owner.ExpressionName]);

  if MoldMember is TMoldAttribute then
  begin
    fDelayedFetch := (Moldmember as TMoldAttribute).EffectiveDelayedFetch;
    fContentName := TypeNameDictionary.MappingForModelName[(MoldMember as TMoldAttribute).BoldType].ExpandedContentsName;
    fIsStoredInObject := MoldMember.EffectivePersistent;
  end
  else
  begin
    with MoldMember as TMoldRole do
    begin
      fIsStoredInObject := EffectivePersistent and
                           (EffectiveEmbedded or (RoleType = rtInnerLinkRole));
      fDelayedFetch := EffectiveDelayedFetch;
      if RoleType = rtInnerLinkRole then
        fContentName := BoldContentName_ObjectIdRef
      else
        if Multi then
          if assigned(Association.LinkClass) then
            fContentName := BoldContentName_ObjectIdListRefPair
          else
            fContentName := BoldContentName_ObjectIdListRef
      else {not multi}
        if assigned(Association.LinkClass) then
          fContentName := BoldContentName_ObjectIdRefPair
        else
          fContentName := BoldContentName_ObjectIdRef;
    end;
  end;
end;

procedure TBoldMemberPersistenceMapper.PMCreate(ObjectIdList: TBoldObjectIdList; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList);
begin
  raise EBold.CreateFmt(sCallToAbstractMethodOnCustomMapper, ['TBoldMemberPersistenceMapper', 'PMCreate', classname, 'PMCreate']);
end;

procedure TBoldMemberPersistenceMapper.PMUpdate(ObjectIdList: TBoldObjectIdList; const ValueSpace: IBoldValueSpace;TranslationList: TBoldIdTranslationList);
begin
  raise EBold.CreateFmt(sCallToAbstractMethodOnCustomMapper, ['TBoldMemberPersistenceMapper', 'PMUpDate', classname, 'PMUpDate']);
end;

procedure TBoldMemberPersistenceMapper.PMDelete(ObjectIdList: TBoldObjectIdList; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList);
begin
  raise EBold.CreateFmt(sCallToAbstractMethodOnCustomMapper, ['TBoldMemberPersistenceMapper', 'PMDelete', classname, 'PMDelete']);
end;

procedure TBoldMemberPersistenceMapper.PMFetch(ObjectIdList: TBoldObjectIdList; const ValueSpace: IBoldValueSpace; FetchMode: Integer; TranslationList: TBoldIdTranslationList; FailureList: TBoldObjectIdList);
begin
  raise EBold.CreateFmt(sCallToAbstractMethodOnCustomMapper, ['TBoldMemberPersistenceMapper', 'PMFetch', classname, 'PMFetch']);
end;

{ TBoldMemberPersistenceMapperList }

constructor TBoldMemberPersistenceMapperList.Create;
begin
  inherited;
  SetIndexCapacity(1);
  SetIndexVariable(IX_MemberMapper, AddIndex(TBoldMemberMapperIndex.Create));
end;

function TBoldMemberPersistenceMapperList.GetItem(index: Integer): TBoldMemberPersistenceMapper;
begin
  Result := TBoldMemberPersistenceMapper(inherited Items[index]);
end;

procedure TBoldObjectPersistenceMapper.BuildMemberFetchLists(
  MemberIdList: TBoldMemberIdList; DefaultFetchMemberList,
  CustomFetchMemberList: TBoldMemberPersistenceMapperList; FetchMode: integer);

  procedure ActuallyAdd(MemberMapper: TBoldMemberPersistenceMapper);
  begin
    if MemberMapper.CustomFetch and assigned(CustomFetchMemberList) then
      CustomFetchMemberList.Add(MemberMapper)
    else if not MemberMapper.CustomFetch and assigned(DefaultFetchMemberList) then
      DefaultFetchMemberList.Add(MemberMapper);
  end;

  procedure TestAdd(MemberMapper: TBoldMemberPersistenceMapper);
  begin
    if assigned(MemberMapper) then
    begin
      case FetchMode of
        fmDistributable :
          if MemberMapper.IsStoredInObject then
            actuallyAdd(MemberMapper);
        fmNormal, fmCompare:
          if assigned(MemberIdList) or not MemberMapper.DelayedFetch then
            actuallyAdd(MemberMapper);
      end;
    end;
  end;

var
  i, MapperIndex: integer;
begin
  assert(not assigned(DefaultFetchMemberList) or not DefaultFetchMemberList.OwnsEntries, 'Cannot put member mappers in a member mapper list that owns its entries.');
  assert(not assigned(CustomFetchMemberList) or not CustomFetchMemberList.OwnsEntries, 'Cannot put member mappers in a member mapper list that owns its entries.');
  if assigned(MemberIdList) then
    for i := 0 to MemberIdList.Count - 1 do
    begin
      MapperIndex := MemberMapperIndexByMemberIndex[MemberIdList[i].MemberIndex];
      if MapperIndex = -1 then
        raise EBold.CreateFmt('%s.BuildMemberFetchLists: MemberMapperIndexByMemberIndex not found for memberIndex %d', [classname, MemberIdList[i].MemberIndex]);
      TestAdd(MemberPersistenceMappers[MapperIndex]);
    end
  else
  begin
    if Assigned(DefaultFetchMemberList) then
      DefaultFetchMemberList.Capacity := MemberPersistenceMappers.Count;
    for i := 0 to MemberPersistenceMappers.Count - 1 do
      TestAdd(MemberPersistenceMappers[i]);
  end;
end;

function TBoldObjectPersistenceMapper.GetLinkClassRole1: TBoldMemberPersistenceMapper;
begin
  if IsLinkClass then
    Result := MemberPersistenceMappers[fLinkRoleMapperIndex1]
  else
    result := nil
end;

function TBoldObjectPersistenceMapper.GetLinkClassRole2: TBoldMemberPersistenceMapper;
begin
  if IsLinkClass then
    Result := MemberPersistenceMappers[fLinkRoleMapperIndex2]
  else
    result := nil
end;

procedure TBoldSystemPersistenceMapper.SubscribeToPersistenceEvents(
  Subscriber: TBoldSubscriber; Events: TBoldSmallEventSet);
begin
  if Events <> [] then
    AddSmallSubscription(Subscriber, Events)
  else
  begin
    AddSubscription(Subscriber, bpeStartFetch, bpeStartFetch);
    AddSubscription(Subscriber, bpeEndFetch, bpeEndFetch);
    AddSubscription(Subscriber, bpeFetchObject, bpeFetchObject);
    AddSubscription(Subscriber, bpeFetchMember, bpeFetchMember);
    AddSubscription(Subscriber, bpeUpdateObject, bpeUpdateObject);
    AddSubscription(Subscriber, bpeCreateObject, bpeCreateObject);
    AddSubscription(Subscriber, bpeDeleteObject, bpeDeleteObject);
    AddSubscription(Subscriber, bpeFetchId, bpeFetchId);
  end;
end;

{ TBoldMemberMapperIndex }

function TBoldMemberMapperIndex.ItemAsKeyString(Item: TObject): string;
begin
  result := TBoldMemberPersistenceMapper(Item).ExpressionName;
end;

function TBoldSystemPersistenceMapper.CommonSuperClassObjectMapper(
  ObjectIdList: TBoldObjectIdList): TBoldObjectPersistenceMapper;
var
  i: Integer;
begin
  if ObjectIdList.Count = 0 then
    raise EBold.CreateFmt('%s.CommonSuperClassObjectMapper: ObjectIdList is empty', [classname]);

  result := ObjectPersistenceMappers[ObjectIdList[0].TopSortedIndex];
  for i := 1 to ObjectIdList.Count - 1 do
    result := result.LeastCommonSuperClassMapper(ObjectPersistenceMappers[ObjectIdList[i].TopSortedIndex]);
end;

function TBoldObjectPersistenceMapper.LeastCommonSuperClassMapper(
  ObjectMapper: TBoldObjectPersistenceMapper): TBoldObjectPersistenceMapper;
begin
  result := ObjectMapper;
  while not BoldIsA(result) do
    result := result.SuperClass;
end;

function TBoldSystemPersistenceMapper.GetCorrectTime: TDateTime;
begin
  if assigned(OnGetCurrentTime) then
    result := OnGetCurrentTime
  else
    result := now;
end;

procedure TBoldSystemPersistenceMapper.EnsurePSDescription;
begin
  if not assigned(fPSSystemDescription) then
  begin
    fPSSystemDescription := CreatePSSystemDescription;
    try
      InitializePSDescriptions;
    except
      FreeAndNil(fPSSystemDescription);
      raise;
    end;
  end;
end;

function TBoldSystemPersistenceMapper.GetPSSystemDescription: TBoldPSSystemDescription;
begin
  EnsurePSDescription;
  result := fPSSystemDescription;
end;

procedure TBoldSystemPersistenceMapper.InitializePSDescriptions;
var
  i: Integer;
begin
  for i := 0 to ObjectPersistenceMappers.Count - 1 do
    if assigned(ObjectPersistenceMappers[i]) then
      ObjectPersistenceMappers[i].InitializePSDescriptions;
end;

procedure TBoldObjectPersistenceMapper.InitializePSDescriptions;
var
  i: Integer;
begin
  for i := 0 to MemberPersistenceMappers.Count - 1 do
    if assigned(MemberPersistenceMappers[i]) then
      MemberPersistenceMappers[i].InitializePSDescriptions;
end;

procedure TBoldMemberPersistenceMapper.InitializePSDescriptions;
begin
end;

function TBoldMemberPersistenceMapper.IsDirty(
  const ObjectContents: IBoldObjectContents): Boolean;
var
  aValue: IBoldValue;
begin
  if ObjectPersistenceMapper.SystemPersistenceMapper.UpdateWholeObjects then
    result := true
  else
  begin
    aValue := ObjectContents.ValueByIndex[MemberIndex];
    if assigned(aValue) then
      result := aValue.BoldPersistenceState = bvpsModified
    else
      result := false;
  end;
end;

function TBoldMemberPersistenceMapper.GetValue(const ObjectContents: IBoldObjectContents): IBoldValue;
begin
  if MemberIndex <> -1 then
    result := ObjectContents.ValueByIndex[MemberIndex]
  else
    result := nil;
end;

function TBoldMemberPersistenceMapper.GetEnsuredValue(const ObjectContents: IBoldObjectContents): IBoldValue;
begin
  if MemberIndex <> -1 then
    Result := ObjectContents.EnsureMemberAndGetValueByIndex(MemberIndex, ContentName)
  else
    Result := nil;
end;

function TBoldMemberPersistenceMapper.ShouldFetch(const ObjectContents: IBoldObjectContents): Boolean;
var
  aValue: IBoldValue;
begin
  aValue := ObjectContents.ValueByIndex[MemberIndex];
  if assigned(aValue) then
    result := aValue.BoldPersistenceState in [bvpsInvalid, bvpsCurrent]
  else
    result := true;
end;

function TBoldObjectPersistenceMapper.GetMemberMapperIndexByMemberIndex(const MemberIndex: Integer): integer;
var
  i: integer;
begin
  if (MemberIndex >= 0) and (MemberIndex < Length(fMemberMapperIndexByMemberIndex)) then
    result := fMemberMapperIndexByMemberIndex[MemberIndex]
  else if MemberIndex < 0 then
  begin
    result := -1;
    for i := 0 to MemberPersistenceMappers.count - 1 do
      if MemberPersistenceMappers[i].MemberIndex = MemberIndex then
      begin
        result := i;
        break;
      end;
  end
  else
    result := -1;
end;

function TBoldSystemPersistenceMapper.GetSupportsObjectUpgrading: Boolean;
begin
  result := UseModelVersion and assigned(ObjectUpgrader);
end;

function TBoldObjectPersistenceMapper.LeastCommonSuperMapper(
  ObjectPMapper: TBoldObjectPersistenceMapper): TBoldObjectPersistenceMapper;
begin
  if BoldIsA(ObjectPMapper) then
    result := ObjectPMapper
  else if ObjectPMapper.BoldIsA(self) then
    result := self
  else
    result := SuperClass.LeastCommonSuperClassMapper(ObjectPMapper);
end;

function TBoldMemberPersistenceMapper.GetSupportsPolymorphicFetch: Boolean;
begin
  result := false;
end;

initialization
  TBoldMemberPersistenceMapperList.IX_MemberMapper := -1;

end.

