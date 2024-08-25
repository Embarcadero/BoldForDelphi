{ Global compiler directives }
{$include bold.inc}
unit BoldBatchUpgrader;

interface

uses
  BoldPMappersDefault,
  BoldObjectUpgrader,
  BoldId;

type
  { forward declaration }
  TBoldBatchUpgrader = class;

  { TBoldBatchUpgrader }
  TBoldBatchUpgrader = class
  private
    fUpgrader: TBoldObjectUpgrader;
    fIsUpgrading: Boolean;
    fUpgradedObjects: integer;
    fStartTime: TDateTime;
    fSystemMapper: TBoldSystemDefaultMapper;
    fBatchSize: integer;
    fIntervalBetweenBatches: integer;
    fMaxExecuteTime: TDateTime;
    fAutoUpgradedObjects: Integer;
    procedure UpgradeObjectIdList(ObjectIdList: TBoldObjectIdList);
    procedure UpgradeObjectIdListInTransaction(ObjectIdList: TBoldObjectIdList);
    procedure UpgradeClass(TopSortedIndex: integer);
    procedure AutoUpgradeObjects;
    property SystemMapper: TBoldSystemDefaultMapper read fSystemMapper;
  public
    constructor Create(SystemMapper: TBoldSystemDefaultMapper; Upgrader: TBoldObjectUpgrader);
    procedure UpgradeObjects;
    property Upgrader: TBoldObjectUpgrader read fUpgrader;
    property BatchSize: integer read fBatchSize write fBatchSize;
    property MaxExecuteTime: TDateTime read fMaxExecuteTime write fMaxExecuteTime;
    property IntervalBetweenBatches: integer read fIntervalBetweenBatches write fIntervalBetweenBatches;
    property IsUpgrading: Boolean read fIsUpgrading;
    property UpgradedObjects: Integer read fUpgradedObjects;
    property AutoUpgradedObjects: Integer read fAutoUpgradedObjects;
  end;

implementation

uses
  SysUtils,
  Classes,

  BoldCoreConsts,
  BoldDefs,
  BoldCondition,
  BoldDbInterfaces,
  BoldPMappers,
  BoldUtils;

procedure TBoldBatchUpgrader.AutoUpgradeObjects;
var
  Script: TStringList;
  Query: IBoldExecQuery;
  i: integer;
begin
  Script := TStringList.create;
  Query := SystemMapper.GetExecQuery;
  try
    Upgrader.GenerateAutoUpgradeScript(Script);
    for i := 0 to Script.Count - 1 do
    begin
      Query.AssignSQLText(Script[i]);
      Query.ExecSQL;
      Inc(fAutoUpgradedObjects, Query.RowsAffected);
    end;
  finally
    SystemMapper.ReleaseExecQuery(Query);
    Script.Free;
  end;
end;

constructor TBoldBatchUpgrader.Create(SystemMapper: TBoldSystemDefaultMapper; Upgrader: TBoldObjectUpgrader);
begin
  inherited create;
  if not SystemMapper.SupportsObjectUpgrading then
    raise EBold.CreateFmt(sNoSupportForObjectUpgrade, [ClassName]);
  fSystemMapper := SystemMapper;
  fUpgrader := Upgrader;
  fBatchSize := 100;
  fMaxExecuteTime := EncodeTime(1, 0, 0, 0);
  fIsUpgrading := false;
  fIntervalBetweenBatches := 5000;
  fUpgradedObjects := 0;
end;

procedure TBoldBatchUpgrader.UpgradeClass(TopSortedIndex: integer);
var
  Condition: TBoldSQLCondition;
  ObjectIdList: TBoldObjectIdList;
  TranslationList: TBoldIdTranslationList;
begin
  Condition := TBoldSQLCondition.Create;
  ObjectIdList := TBoldObjectIdList.Create;
  TranslationList := TBoldIdTranslationList.Create;
  try
    Condition.TopSortedIndex := TopSortedIndex;
    Condition.WhereFragment := format( '%s = %d AND %s <> %d', [
      TYPECOLUMN_NAME,
      SystemMapper.BoldDbTypeForTopSortedIndex(TopSortedIndex),
      SystemMapper.RootClassObjectPersistenceMapper.ModelVersionMember.ColumnDescriptions[0].SQLName,
      SystemMapper.ModelVersion]);
    Condition.MaxAnswers := BatchSize;
    repeat
      ObjectIdList.Clear;
      SystemMapper.PMFetchClassWithCondition(ObjectIDList, Upgrader.ValueSpacePmIn, Condition, fmNormal, TranslationList);
      if ObjectIdList.Count <> 0 then
      begin
        UpgradeObjectIdListInTransaction(ObjectIdList);
        Sleep(IntervalBetweenBatches);
      end;
    until (ObjectIdList.Count = 0) or (now > fStartTime + MaxExecuteTime);
  finally
    Condition.Free;
    ObjectIdList.free;
    TranslationList.Free;
  end;
end;

procedure TBoldBatchUpgrader.UpgradeObjectIdList(ObjectIdList: TBoldObjectIdList);
var
  MemberPMList: TBoldMemberPersistenceMapperList;
  aQuery: IBoldQuery;
  sql: TStringList;
  TempLIst: TStringList;
  i: Integer;
  TempId, NewID: TBoldObjectId;
  ObjectMapper: TBoldObjectDefaultMapper;
begin
  MemberPMList := TBoldMemberPersistenceMapperList.Create;
  MemberPMList.OwnsEntries := False;
  aQuery := SystemMapper.GetQuery;
  TempList := TStringList.Create;
  sql := TStringList.Create;
  ObjectMapper := SystemMapper.ObjectPersistenceMappers[ObjectidList[0].TopSortedIndex] as TBoldObjectDefaultMapper;

  try
    Objectmapper.BuildMemberFetchLists(nil, MemberPMLIst, nil, fmNormal);

    ObjectMapper.RetrieveSelectStatement(SQL, MemberPMList, fmNormal, Objectmapper.versioned);

    for i := 0 to ObjectIdList.Count - 1 do
    begin
      if not ObjectIdList[i].TopSortedIndexExact then
        raise EBold.createFmt(sObjectListNotExact, [Classname]);
      if ObjectIdList[i].topSortedIndex <> ObjectMapper.TopSortedIndex then
        raise EBold.createFmt(sObjectListNotHomogenous, [Classname]);
      TempLIst.Append(ObjectIdList[i].asString);
    end;

    if (MemberPMList.Count > 0) and (TempList.Count > 0) then
    begin
      BoldAppendToStrings(SQL, Format('in (%s)',[BoldSeparateStringList(TempList, ', ', '', '')]), False);
      if Objectmapper.versioned then
        Objectmapper.RetrieveTimeStampCondition(SQL, BOLDMAXTIMESTAMP, true, 'AND', false);

      aQuery.AssignSQL(SQL);
      aQuery.Open;
      while not aQuery.EOF do
      begin
        // ClassId param -1 was added due to changes in signature of NewIdFromQuery,
        // check if we can replace -1 with known ClassId if possible in order to get ExactId right away - Daniel
        tempId := SystemMapper.NewIdFromQuery(aQuery, -1, 1, 0, BOLDMAXTIMESTAMP);
        NewId := ObjectIdList.IDByID[TempId];
        TempId.Free;

        Upgrader.UpgradeObjectById(NewId, aQuery);
        inc(fUpgradedObjects);
        aQuery.Next;
      end;
      aQuery.Close;
    end;
  finally
    sql.Free;
    MemberPMList.Free;
    SystemMapper.ReleaseQuery(aQuery);
    TempLIst.Free;
  end;
end;

procedure TBoldBatchUpgrader.UpgradeObjectIdListInTransaction(ObjectIdList: TBoldObjectIdList);
begin
  Upgrader.StartTransaction;
  try
    UpgradeObjectIdList(ObjectIdList);
    Upgrader.EndTransaction;
  except
    Upgrader.FailTransaction;
    raise;
  end;
end;

procedure TBoldBatchUpgrader.UpgradeObjects;
var
  i: integer;
begin
  if IsUpgrading then
    exit;
  if not assigned(SystemMapper.DataBase) then
    raise EBold.CreateFmt(sDBMustBeOpened, [classname]);
  fIsUpgrading := true;
  try
    fStartTime := now;
    AutoUpgradeObjects;
    for i := 0 to SystemMapper.ObjectPersistenceMappers.Count - 1 do
      if assigned(SystemMapper.ObjectPersistenceMappers[i]) then
        UpgradeClass(i);
  finally
    fIsUpgrading := false;
  end;
end;

end.
