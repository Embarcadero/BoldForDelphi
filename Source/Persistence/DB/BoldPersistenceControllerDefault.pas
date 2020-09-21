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
  BoldDefs;

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
    fOnGetDatabase: TBoldGetDatabaseEvent;
    procedure EnsureActive(Action: String);
    function GetPersistenceMapper: TBoldSystemDefaultMapper;
    procedure SQLDatabaseConfigChanged(Sender: TObject);
  public
    constructor CreateFromMold(MoldModel: TMoldModel; TypeNameDictionary: TBoldTypeNameDictionary; SQLDataBaseConfig: TBoldSQLDataBaseConfig; GetDatabaseFunc: TBoldGetDatabaseEvent);
    destructor Destroy; override;
    procedure PMExactifyIds(ObjectIdList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList); override;
    procedure PMFetch(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; MemberIdList: TBoldMemberIdList; FetchMode: integer; BoldClientID: TBoldClientID); override;
    procedure PMFetchIDListWithCondition(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; FetchMode: Integer; Condition: TBoldCondition; BoldClientID: TBoldClientID); override;
    procedure PMUpdate(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; Old_Values: IBoldValueSpace; Precondition: TBoldUpdatePrecondition; TranslationList: TBoldIdTranslationList; var TimeStamp: TBoldTimeStampType; BoldClientID: TBoldClientID); override;
    procedure OpenDatabase(ReadMappingFromDb: Boolean);
    procedure CloseDataBase;
    procedure PMTranslateToGlobalIds(ObjectIdList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList); override;
    procedure PMTranslateToLocalIds(GlobalIdList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList); override;
    procedure PMSetReadOnlyness(ReadOnlyList, WriteableList: TBoldObjectIdList); override;
    procedure SubscribeToPeristenceEvents(Subscriber: TBoldSubscriber); override;
    procedure ReserveNewIds(ValueSpace: IBoldValueSpace; ObjectIdList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList); override;
    property PersistenceMapper: TBoldSystemDefaultMapper read GetPersistenceMapper;
    procedure PMTimestampForTime(ClockTime: TDateTime; var Timestamp: TBoldTimestampType); override;
    procedure PMTimeForTimestamp(Timestamp: TBoldTimestampType; var ClockTime: TDateTime); override;
    procedure StartTransaction;
    procedure CommitTransaction;
    procedure RollbackTransaction;
    function InTransaction: Boolean;
    property Active: Boolean read fActive;
    property SQLDataBaseConfig: TBoldSQLDataBaseConfig read fSQLDataBaseConfig;
  end;

implementation

uses
  SysUtils,
  BoldPMappers,
  BoldGuard,
  BoldPMappersSQL,
  PersistenceConsts,
  BoldPMappersAttributeDefault, // Not used, just to pull them in
  BoldPMappersLinkDefault;  // Not used, just to pull them in

{---TBoldPersistenceControllerDefault---}
constructor TBoldPersistenceControllerDefault.CreateFromMold(MoldModel: TMoldModel; TypeNameDictionary: TBoldTypeNameDictionary; SQLDataBaseConfig: TBoldSQLDataBaseConfig; GetDatabaseFunc: TBoldGetDatabaseEvent);
begin
  fMoldModel := MoldModel;
  fTypeNameDictionary := TypeNameDictionary;
  fActive := false;
  fSQLDataBaseConfig := TBoldSQLDataBaseConfig.Create;
  fSQLDataBaseConfig.AssignConfig(SQLDataBaseConfig);
  fSQLDataBaseConfig.OnChange := SQLDataBaseConfigChanged;
  fOnGetDatabase := GetDatabaseFunc;
end;

procedure TBoldPersistenceControllerDefault.PMExactifyIds(ObjectIdList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList);
begin
  EnsureActive('PMExactifyIDs'); // do not localize
  PersistenceMapper.RootClassObjectPersistenceMapper.EnsureIDsExact(ObjectIdlist, TranslationList);
end;

procedure TBoldPersistenceControllerDefault.PMUpdate(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; Old_Values: IBoldValueSpace; Precondition: TBoldUpdatePrecondition; TranslationList: TBoldIdTranslationList; var TimeStamp: TBoldTimeStampType; BoldClientID: TBoldClientID);
var
  aTranslationList: TBoldIdTranslationList;
  NewTimeStamp: TBoldTimeStampType;
begin
  EnsureActive('PMUpdate'); // do not localize
  SendExtendedEvent(bpeStartUpdate, [ObjectIdList, valueSpace]);
  if assigned(TranslationList) then
    aTranslationList := TranslationList
  else
    aTranslationList := TBoldIdTranslationList.Create;

  try
    PersistenceMapper.PMUpdate(ObjectIdList, ValueSpace, Old_Values, Precondition, aTranslationList, NewTimeStamp);
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

procedure TBoldPersistenceControllerDefault.PMFetch(ObjectIDList: TBoldObjectIdList;  ValueSpace: IBoldValueSpace; MemberIdList: TBoldMemberIdList; FetchMode: Integer; BoldClientID: TBoldClientID);
var
  TranslationList: TBoldIdTranslationList;
  ExactifyTranslationList: TBoldIdTranslationList;
  DefaultMemberList: TBoldMemberPersistenceMapperList;
  FetchIdList: TBoldObjectIdList;
  WasInTransaction: Boolean;
begin
  EnsureActive('PMFetch'); // do not localize
  SendExtendedEvent(bpeStartFetch, [ObjectIdList, MemberIdList]);

  TranslationList := TBoldIdTranslationList.Create;
  ExactifyTranslationList := TBoldIdTranslationList.Create;
  DefaultMemberList := TBoldMemberPersistenceMapperList.Create;
  DefaultMemberList.OwnsEntries := false;
  FetchIdList := ObjectIdList.Clone;

  try
    WasInTransaction := PersistenceMapper.Database.InTransaction;
    PMExactifyIDs(FetchIdList, ExactifyTranslationList);
    ValueSpace.ExactifyIDs(ExactifyTranslationList);
    ObjectIdList.ExactifyIds(ExactifyTranslationList);
    FetchIdList.ExactifyIds(ExactifyTranslationList);
    PersistenceMapper.PMFetch(FetchIDList, ValueSpace, MemberIdList, FetchMode, TranslationList);
    ValueSpace.ApplyTranslationList(TranslationList);
    if not WasInTransaction and PersistenceMapper.Database.InTransaction then
      PersistenceMapper.Database.Commit;
  finally
    TranslationLIst.Free;
    ExactifyTranslationLIst.Free;
    DefaultMemberList.Free;
    FetchIdList.Free;
    SendEvent(bpeEndFetch);
  end;
end;

procedure TBoldPersistenceControllerDefault.PMFetchIDListWithCondition(ObjectIDList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; FetchMode: Integer; Condition: TBoldCondition; BoldClientID: TBoldClientID);
var
  TranslationList: TBoldIdTranslationList;
  Guard: IBoldGuard;
  WasInTransaction: Boolean;
begin
  Guard := TBoldguard.Create(TranslationList);
  EnsureActive('PMFetchIdListWithCondition'); // do not localize
  SendExtendedEvent(bpeStartFetchId, [Condition]);
  TranslationList := TBoldIdTranslationList.Create;
  WasInTransaction := PersistenceMapper.Database.InTransaction;
  PersistenceMapper.PMFetchClassWithCondition(ObjectIdList, ValueSpace, Condition, FetchMode, TranslationList);
  ValueSpace.ApplyTranslationList(TranslationList);
  if not WasInTransaction and PersistenceMapper.Database.InTransaction then
    PersistenceMapper.Database.Commit;
  SendEvent(bpeEndFetchId);
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

procedure TBoldPersistenceControllerDefault.CloseDataBase;
begin
  PersistenceMapper.CloseDatabase;
  fActive := false;
end;

destructor TBoldPersistenceControllerDefault.Destroy;
begin
  FreeAndNil(fPersistenceMapper);
  FreeAndNil(fSQLDataBaseConfig);
  inherited;
end;

procedure TBoldPersistenceControllerDefault.PMTranslateToGlobalIds(
  ObjectIdList: TBoldObjectIdList;
  TranslationList: TBoldIdTranslationList);
begin
  EnsureActive('PMTranslateToGlobalIds'); // do not localize
  PersistenceMapper.PMTranslateToGlobalIds(ObjectIdList, TranslationList);
end;

procedure TBoldPersistenceControllerDefault.PMTranslateToLocalIds(
  GlobalIdList: TBoldObjectIdList;
  TranslationList: TBoldIdTranslationList);
begin
  EnsureActive('PMTranslateToLocalIds'); // do not localize
  PersistenceMapper.PMTranslateToLocalIds(GlobalIdList, TranslationList);
end;

procedure TBoldPersistenceControllerDefault.PMSetReadonlyness(ReadOnlyList,
  WriteableList: TBoldObjectIdList);
begin
  EnsureActive('PMSetReadonlyness'); // do not localize
  PersistenceMapper.PMSetReadonlyness(ReadOnlyList, WriteableList);
end;

procedure TBoldPersistenceControllerDefault.SubscribeToPeristenceEvents(
  Subscriber: TBoldSubscriber);
begin
  Inherited;
  PersistenceMapper.SubscribeToPersistenceEvents(Subscriber);
end;

procedure TBoldPersistenceControllerDefault.ReserveNewIds(ValueSpace: IBoldValueSpace; ObjectIdList: TBoldObjectIdList;
            TranslationList: TBoldIdTranslationList);
begin
  EnsureActive('ReserveNewIds'); // do not localize
  PersistenceMapper.ReserveNewIds(ValueSpace, ObjectIdList, TranslationList);
end;

procedure TBoldPersistenceControllerDefault.PMTimeForTimestamp(
  Timestamp: TBoldTimestampType; var ClockTime: TDateTime);
begin
  EnsureActive('PMTimeForTimeStamp'); // do not localize
  PersistenceMapper.PMTimeForTimestamp(Timestamp, ClockTime);
end;

procedure TBoldPersistenceControllerDefault.PMTimestampForTime(
  ClockTime: TDateTime; var Timestamp: TBoldTimestampType);
begin
  EnsureActive('PMTimestampForTime'); // do not localize
  PersistenceMapper.PMTimestampForTime(ClockTime, Timestamp);
end;

procedure TBoldPersistenceControllerDefault.EnsureActive(Action: String);
begin
  if not fActive then
    raise EBold.CreateFmt(sNotActive, [ClassName, Action]);
end;

function TBoldPersistenceControllerDefault.GetPersistenceMapper: TBoldSystemDefaultMapper;
begin
  if not assigned(fPersistenceMapper) then
    fPersistenceMapper := TBoldSystemDefaultMapper.CreateFromMold(fMoldModel, fTypeNameDictionary, SQLDataBaseConfig, fOnGetDatabase);

  result := fPersistenceMapper;
end;

procedure TBoldPersistenceControllerDefault.SQLDatabaseConfigChanged(Sender: TObject);
begin
  if assigned(fPersistencemapper) then
    PersistenceMapper.SQLDataBaseConfig.AssignConfig(SQLDataBaseConfig);
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
