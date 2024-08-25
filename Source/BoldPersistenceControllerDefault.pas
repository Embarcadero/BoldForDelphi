{ Global compiler directives }
{$include bold.inc}
unit BoldPersistenceControllerDefault;

interface

uses
  BoldMeta,
  BoldId,
  BoldPMappersDefault,
  BoldUpdatePrecondition,
  BoldCondition,
  BoldSubscription,
  BoldDBInterfaces,
  BoldValueSpaceInterfaces,
  BoldPersistenceController,
  BoldSQLDatabaseConfig,
  BoldTypeNameDictionary,
  BoldDefs,
  BoldIndexCollection,
  BoldElements;

type
  { forward declarations }
  TBoldPersistenceControllerDefault = class;

  {---TBoldPersistenceControllerDefault---}
  TBoldPersistenceControllerDefault = class(TBoldPersistenceController)
  private
    fPersistenceMapper: TBoldSystemDefaultMapper;
    fActive: Boolean;
    fMoldModel: TMoldModel;
    fTypeNameDictionary: TBoldTypeNameDictionary;
    fSQLDataBaseConfig: TBoldSQLDataBaseConfig;
    fCustomIndexes: TBoldIndexCollection;
    fOnGetDatabase: TBoldGetDatabaseEvent;
    procedure EnsureActive(Action: String);  
    function GetPersistenceMapper: TBoldSystemDefaultMapper;
  public
    constructor CreateFromMold(MoldModel: TMoldModel; TypeNameDictionary: TBoldTypeNameDictionary;
      CustomIndexes: TBoldIndexCollection; SQLDataBaseConfig: TBoldSQLDataBaseConfig; GetDatabaseFunc: TBoldGetDatabaseEvent);
    destructor Destroy; override;
    procedure PMExactifyIds(ObjectIdList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList; HandleNonExisting: Boolean); override;
    procedure PMFetch(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; MemberIdList: TBoldMemberIdList; FetchMode: integer; BoldClientID: TBoldClientID); override;
    procedure PMFetchIDListWithCondition(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; FetchMode: Integer; Condition: TBoldCondition; BoldClientID: TBoldClientID); override;
    procedure PMUpdate(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; Old_Values: IBoldValueSpace; Precondition: TBoldUpdatePrecondition; TranslationList: TBoldIdTranslationList; var TimeStamp: TBoldTimeStampType; var TimeOfLatestUpdate: TDateTime; BoldClientID: TBoldClientID); override;
    procedure OpenDatabase(ReadMappingFromDb: Boolean);
    procedure CloseDataBase;
    procedure PMTranslateToGlobalIds(ObjectIdList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList); override;
    procedure PMTranslateToLocalIds(GlobalIdList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList); override;
    procedure PMSetReadOnlyness(ReadOnlyList, WriteableList: TBoldObjectIdList); override;
    procedure SubscribeToPersistenceEvents(Subscriber: TBoldSubscriber; Events: TBoldSmallEventSet = []); override;
    procedure ReserveNewIds(ValueSpace: IBoldValueSpace; ObjectIdList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList); override;
    property PersistenceMapper: TBoldSystemDefaultMapper read GetPersistenceMapper;
    procedure PMTimestampForTime(ClockTime: TDateTime; var Timestamp: TBoldTimestampType); override;
    procedure PMTimeForTimestamp(Timestamp: TBoldTimestampType; var ClockTime: TDateTime); override;
    procedure StartTransaction; override;
    procedure CommitTransaction; override;
    procedure RollbackTransaction; override;
    function DatabaseInterface: IBoldDatabase; override;    
    function InTransaction: Boolean;
    function CanEvaluateInPS(sOCL: string; aSystem: TBoldElement; aContext: TBoldElementTypeInfo = nil; const aVariableList: TBoldExternalVariableList = nil): Boolean; override;
    property Active: Boolean read fActive;
    property SQLDataBaseConfig: TBoldSQLDataBaseConfig read fSQLDataBaseConfig;
  end;

implementation

uses
  SysUtils,
  BoldUtils,
  BoldPMappers,
  BoldGuard,
  BoldPMapperLists, //PATCH
  BoldPMappersSQL,
  BoldPMappersAttributeDefault,
  BoldPMappersLinkDefault;

{---TBoldPersistenceControllerDefault---}
constructor TBoldPersistenceControllerDefault.CreateFromMold(MoldModel: TMoldModel; TypeNameDictionary: TBoldTypeNameDictionary;
  CustomIndexes: TBoldIndexCollection; SQLDataBaseConfig: TBoldSQLDataBaseConfig; GetDatabaseFunc: TBoldGetDatabaseEvent);
begin
  inherited Create;
  fMoldModel := MoldModel;
  fTypeNameDictionary := TypeNameDictionary;
  fActive := false;
  fSQLDataBaseConfig := SQLDataBaseConfig;
  fCustomIndexes := TBoldIndexCollection.Create(nil);
  fCustomIndexes.Assign(CustomIndexes);
  fOnGetDatabase := GetDatabaseFunc;
end;

procedure TBoldPersistenceControllerDefault.PMExactifyIds(ObjectIdList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList; HandleNonExisting: Boolean);
begin
  EnsureActive('PMExactifyIDs');
  PersistenceMapper.RootClassObjectPersistenceMapper.EnsureIDsExact(ObjectIdlist, TranslationList, HandleNonExisting);
end;

procedure TBoldPersistenceControllerDefault.PMUpdate(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; Old_Values: IBoldValueSpace; Precondition: TBoldUpdatePrecondition; TranslationList: TBoldIdTranslationList; var TimeStamp: TBoldTimeStampType; var TimeOfLatestUpdate: TDateTime; BoldClientID: TBoldClientID);
var
  aTranslationList: TBoldIdTranslationList;
  NewTimeStamp: TBoldTimeStampType;
begin
  EnsureActive('PMUpdate');
  if assigned(TranslationList) then
    aTranslationList := TranslationList
  else
    aTranslationList := TBoldIdTranslationList.Create;
  SendExtendedEvent(bpeStartUpdate, [ObjectIdList, valueSpace]);
  try
    PersistenceMapper.PMUpdate(ObjectIdList, ValueSpace, Old_Values, Precondition, aTranslationList, NewTimeStamp, TimeOfLatestUpdate);
    if not assigned(Precondition) or not Precondition.failed then
    begin
      TimeStamp := NewTimeStamp;
      ValueSpace.ApplyTranslationList(aTranslationList);
    end;
  finally
    if not assigned(TranslationList) then
      aTranslationList.Free;
    SendEvent(bpeEndUpdate);
  end;
end;

procedure TBoldPersistenceControllerDefault.PMFetch(ObjectIDList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; MemberIdList: TBoldMemberIdList; FetchMode: Integer; BoldClientID: TBoldClientID);
var
  TranslationList: TBoldIdTranslationList;
  ExactifyTranslationList: TBoldIdTranslationList;
  DefaultMemberList: TBoldMemberPersistenceMapperList;
  FetchIdList: TBoldObjectIdList;
  WasInTransaction: Boolean;
  NeedsExacitfy: boolean;
  Guard: IBoldGuard;
begin
  Guard := TBoldguard.Create(TranslationLIst, ExactifyTranslationLIst, DefaultMemberList, FetchIdList);
  EnsureActive('PMFetch');
  SendExtendedEvent(bpeStartFetch, [ObjectIdList, MemberIdList]);
  try
    WasInTransaction := PersistenceMapper.Database.InTransaction;
    NeedsExacitfy := ObjectIdList.HasInexactIds;
    DefaultMemberList := TBoldMemberPersistenceMapperList.Create;
    DefaultMemberList.OwnsEntries := false;
    TranslationList := TBoldIdTranslationList.Create;
    if NeedsExacitfy then
    begin
      ExactifyTranslationList := TBoldIdTranslationList.Create;
      FetchIdList := ObjectIdList.Clone;
      PMExactifyIDs(FetchIdList, ExactifyTranslationList, false);
      ValueSpace.ExactifyIDs(ExactifyTranslationList);
      ObjectIdList.ExactifyIds(ExactifyTranslationList);
      FetchIdList.ExactifyIds(ExactifyTranslationList);
      if FetchIdList.HasNonExistingIds then
        FetchIdList.RemoveNonExistingIds;
      PersistenceMapper.PMFetch(FetchIDList, ValueSpace, MemberIdList, FetchMode, TranslationList)
    end
    else
    begin
      if ObjectIdList.HasNonExistingIds then
      begin
        FetchIdList := ObjectIdList.Clone;
        FetchIdList.RemoveNonExistingIds;
        PersistenceMapper.PMFetch(FetchIdList, ValueSpace, MemberIdList, FetchMode, TranslationList);
      end
      else
      PersistenceMapper.PMFetch(ObjectIdList, ValueSpace, MemberIdList, FetchMode, TranslationList);
    end;
    ValueSpace.ApplyTranslationList(TranslationList);
    if not WasInTransaction and PersistenceMapper.Database.InTransaction then
      PersistenceMapper.Database.Commit;
  finally
    SendExtendedEvent(bpeEndFetch, [ObjectIdList, MemberIdList]);
  end;
end;

procedure TBoldPersistenceControllerDefault.PMFetchIDListWithCondition(ObjectIDList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; FetchMode: Integer; Condition: TBoldCondition; BoldClientID: TBoldClientID);
var
  TranslationList: TBoldIdTranslationList;
  Guard: IBoldGuard;
  WasInTransaction: Boolean;
begin
  Guard := TBoldguard.Create(TranslationList);
  EnsureActive('PMFetchIdListWithCondition');
  WasInTransaction := PersistenceMapper.Database.InTransaction;
  SendExtendedEvent(bpeStartFetchId, [Condition]);
  try
    TranslationList := TBoldIdTranslationList.Create;
    PersistenceMapper.PMFetchClassWithCondition(ObjectIdList, ValueSpace, Condition, FetchMode, TranslationList);
    ValueSpace.ApplyTranslationList(TranslationList);
    if not WasInTransaction and PersistenceMapper.Database.InTransaction then
      PersistenceMapper.Database.Commit;
  finally
    SendExtendedEvent(bpeEndFetchId, [ObjectIdList]);
  end;
end;

procedure TBoldPersistenceControllerDefault.OpenDatabase(ReadMappingFromDb: Boolean);
begin
  try
    PersistenceMapper.OpenDatabase(true, ReadMappingFromDb);
  except
    FreeAndNil(fPersistenceMapper);
    raise;
  end;
  fActive := true;
end;

function TBoldPersistenceControllerDefault.CanEvaluateInPS(sOCL: string;
  aSystem: TBoldElement; aContext: TBoldElementTypeInfo;
  const aVariableList: TBoldExternalVariableList): Boolean;
begin
  Result := PersistenceMapper.CanEvaluateInPS(sOCL, aSystem, aContext, aVariableList);
end;

procedure TBoldPersistenceControllerDefault.CloseDataBase;
begin
  PersistenceMapper.CloseDatabase;
  fActive := false;
end;

destructor TBoldPersistenceControllerDefault.Destroy;
begin
  FreeAndNil(fPersistenceMapper);
  FreeAndNil(fCustomIndexes);
  inherited;
end;

procedure TBoldPersistenceControllerDefault.PMTranslateToGlobalIds(
  ObjectIdList: TBoldObjectIdList;
  TranslationList: TBoldIdTranslationList);
begin
  EnsureActive('PMTranslateToGlobalIds');
  PersistenceMapper.PMTranslateToGlobalIds(ObjectIdList, TranslationList);
end;

procedure TBoldPersistenceControllerDefault.PMTranslateToLocalIds(
  GlobalIdList: TBoldObjectIdList;
  TranslationList: TBoldIdTranslationList);
begin
  EnsureActive('PMTranslateToLocalIds');
  PersistenceMapper.PMTranslateToLocalIds(GlobalIdList, TranslationList);
end;

procedure TBoldPersistenceControllerDefault.PMSetReadOnlyness(ReadOnlyList,
  WriteableList: TBoldObjectIdList);
begin
  EnsureActive('PMSetReadonlyness');
  PersistenceMapper.PMSetReadonlyness(ReadOnlyList, WriteableList);
end;

procedure TBoldPersistenceControllerDefault.SubscribeToPersistenceEvents(
  Subscriber: TBoldSubscriber; Events: TBoldSmallEventSet);
begin
  inherited;
  PersistenceMapper.SubscribeToPersistenceEvents(Subscriber, Events);
end;

procedure TBoldPersistenceControllerDefault.ReserveNewIds(ValueSpace: IBoldValueSpace; ObjectIdList: TBoldObjectIdList;
            TranslationList: TBoldIdTranslationList);
begin
  EnsureActive('ReserveNewIds');
  PersistenceMapper.ReserveNewIds(ValueSpace, ObjectIdList, TranslationList);
end;

procedure TBoldPersistenceControllerDefault.PMTimeForTimestamp(
  Timestamp: TBoldTimestampType; var ClockTime: TDateTime);
begin
  EnsureActive('PMTimeForTimeStamp');
  PersistenceMapper.PMTimeForTimestamp(Timestamp, ClockTime);
end;

procedure TBoldPersistenceControllerDefault.PMTimestampForTime(
  ClockTime: TDateTime; var Timestamp: TBoldTimestampType);
begin
  EnsureActive('PMTimestampForTime');
  PersistenceMapper.PMTimestampForTime(ClockTime, Timestamp);
end;

procedure TBoldPersistenceControllerDefault.EnsureActive(Action: String);
begin
  if not fActive then
    raise EBold.CreateFmt('%s.%s: Not Active', [ClassName, Action]);
end;

function TBoldPersistenceControllerDefault.DatabaseInterface: IBoldDatabase;
begin
  if Assigned(PersistenceMapper) then  
    result := PersistenceMapper.Database
  else
    result := nil;
end;

function TBoldPersistenceControllerDefault.GetPersistenceMapper: TBoldSystemDefaultMapper;
type
  TTBoldSystemDefaultMapperClass = class of TBoldSystemDefaultMapper;
var
  SystemMapperDescriptor: TBoldSystemPersistenceMapperDescriptor;
  SystemPMapperName: string;
  SysPMapperClass: TTBoldSystemDefaultMapperClass;
begin
  if not assigned(fPersistenceMapper) then
  begin
    SystemPMapperName := fMoldModel.PMapperName;
    if BoldNamesEqual(SystemPMapperName, DEFAULTNAME) and  (SQLDataBaseConfig.DefaultSystemMapper <> '') then
       SystemPMapperName := SQLDataBaseConfig.DefaultSystemMapper;
    
    SystemMapperDescriptor := BoldSystemPersistenceMappers.DescriptorByName[SystemPMapperName];   //PATCH
    if not assigned(SystemMapperDescriptor) then                                                       //PATCH
      raise EBold.CreateFmt('Unable to find SystemPersistenceMapper (%s)', [SystemPMapperName]);  //PATCH

    SysPMapperClass := TTBoldSystemDefaultMapperClass(SystemMapperDescriptor.SystemPersistenceMapperClass); //PATCH - This ugly code is needed to call the correct static constructor!!!

    fPersistenceMapper := SysPMapperClass.CreateFromMold(fMoldModel, fTypeNameDictionary, fCustomIndexes,SQLDataBaseConfig, fOnGetDatabase); //PATCH
//    fPersistenceMapper := TBoldSystemDefaultMapper.CreateFromMold(fMoldModel, fTypeNameDictionary, SQLDataBaseConfig, fOnGetDatabase); //PATCH - Original is Hardcoded! 
  end;

  result := fPersistenceMapper;
end;

procedure TBoldPersistenceControllerDefault.CommitTransaction;
begin
  PersistenceMapper.Database.Commit;
end;

procedure TBoldPersistenceControllerDefault.RollbackTransaction;
begin
  PersistenceMapper.Database.RollBack;
end;

procedure TBoldPersistenceControllerDefault.StartTransaction;
begin
  PersistenceMapper.Database.StartTransaction;
end;

function TBoldPersistenceControllerDefault.InTransaction: Boolean;
begin
  Result := PersistenceMapper.Database.InTransaction;
end;

end.
