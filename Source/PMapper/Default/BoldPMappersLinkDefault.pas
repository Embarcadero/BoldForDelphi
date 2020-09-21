unit BoldPMappersLinkDefault;

interface

uses
  Classes,
  BoldDBInterfaces,
  Db,
  BoldPMappers,
  BoldDefs,
  BoldMeta,
  BoldTypeNameDictionary,
  BoldId,
  BoldCondition,
  BoldValueInterfaces,
  BoldValueSpaceInterfaces,
  BoldSQLQuery,
  BoldPMappersDefault;

type
  { forward declarations }
  TBoldLinkDefaultMapper = class;
  TBoldEmbeddedSingleLinkDefaultMapper = class;
  TBoldNonEmbeddedLinkDefaultMapper = class;
  TBoldMultiLinkDefaultMapper = class;
  TBoldDirectSingleLinkDefaultmapper = class;
  TBoldIndirectSingleLinkDefaultmapper = class;
  TBoldDirectMultiLinkDefaultmapper = class;
  TBoldIndirectMultiLinkDefaultmapper = class;

  { TBoldLinkDefaultMapper }
  TBoldLinkDefaultMapper = class(TBoldMemberDefaultMapper)
  private
    function GetObjectPersistenceMappers(Index: Integer): TBoldObjectDefaultMapper;
  protected
    function GetOtherEndObjectMapper: TBoldObjectDefaultMapper; virtual; abstract;
  public
    property OtherEndObjectMapper: TBoldObjectDefaultMapper read GetOtherEndObjectMapper;
    property ObjectPersistenceMappers[Index: Integer]: TBoldObjectDefaultMapper read GetObjectPersistenceMappers;
  end;

  { TBoldEmbeddedSingleLinkDefaultMapper }
  TBoldEmbeddedSingleLinkDefaultMapper = class(TBoldLinkDefaultMapper)
  private
    fOtherEndExact: Boolean;
    fOtherEndObjectPMIndex: Integer;
    fOtherEndMemberIndex: Integer;
    fColumnCount: Integer;
    fIsInherited: Boolean;
    procedure GetNonEmbeddedChangePoints(ObjectIdList: TBoldObjectIdList; Condition: TBoldChangePointCondition; NameSpace: TBoldSqlnameSpace);
    function NewDefaultId(IdValue: integer; TimeStamp: TBoldTimestampType): TBoldObjectId;
    function GetMainColumnName: String;
    function GetOrderColumnName: String;
    function GetOtherEndMemberPMIndex: Integer;
  protected
    function GetColumnTypeAsSQL(ColumnIndex: Integer): string; override;
    function GetAllowNullAsSQL: string; override;
    function GetColumnCount: Integer; override;
    function GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType; override;
    function GetColumnSize(ColumnIndex: Integer): Integer; override;
    function GetInitialColumnName(ColumnIndex: Integer): string; override;
    function CompareField(ObjectContent: IBoldObjectContents; Field: IBoldField; ColumnIndex: integer; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList): Boolean; override;
    procedure InitializePSDescriptions; override;
    function GetOtherEndObjectMapper: TBoldObjectDefaultMapper; override;
    function DefaultDefaultDbValue: String; override;
  public
    class function CanStore(const ContentName: string): Boolean; override;
    procedure ValueToParam(ObjectContent: IBoldObjectContents; Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList); override;
    procedure ValueFromField(OwningObjectId: TBoldObjectId; ObjectContent: IBoldObjectContents; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; Field: IBoldField; ColumnIndex: Integer); override;
    constructor CreateFromMold(moldMember: TMoldMember; moldClass: TMoldClass; Owner: TBoldObjectPersistenceMapper; const MemberIndex: Integer; TypeNameDictionary: TBoldTypeNameDictionary); override;
    property OtherEndObjectPMIndex: Integer read fOtherEndObjectPMIndex;
    property OtherEndMemberPMIndex: Integer read GetOtherEndMemberPMIndex;
    property OtherEndExact: Boolean read fOtherEndExact;
    property IsInherited: Boolean read fIsInherited;
    property OrderColumnName: String read GetOrderColumnName;
    property MainColumnName: String read GetMainColumnName;
  end;

  { TBoldNonEmbeddedLinkDefaultMapper }
  TBoldNonEmbeddedLinkDefaultMapper = class (TBoldLinkDefaultMapper)
  private
    function GetClosestOtherEndMemberMapperIndex: Integer;
  private
    fClosestOtherEndObjectMapperIndex: Integer;
    fClosestOtherEndMemberIndex: Integer;
    fRemoteInnerLinkMemberIndex: integer;
    fIsIndirect: Boolean;
    fRemoteOtherEndObjectMapperIndex: Integer;
    procedure ProcessSQL(Query: IBoldQuery; WhereFragment: String; resultList: TList; TimeStamp: TBoldTimeStampType);
    procedure ProcessResult(ResultList: TList; ValueSpace: IBoldValueSpace; ObjectIdList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList; timeStamp: TBoldTimeStampType; FetchMode: integer; FailureList: TBoldObjectIdList);
    function GetLinkClassTableName: string;
    function GetLinkClassObjectMapper: TBoldObjectDefaultMapper; virtual; abstract;
    function GetClosestColumnName: string;
    property ClosestOtherEndObjectMapperIndex: Integer read fClosestOtherEndObjectMapperIndex;
    property ClosestOtherEndMemberMapperIndex: Integer read GetClosestOtherEndMemberMapperIndex;
    property RemoteOtherEndObjectMapperIndex: Integer read fRemoteOtherEndObjectMapperIndex;
    function GetRemoteInnerLinkMapper: TBoldEmbeddedSingleLinkDefaultMapper;
    function GetClosestOtherEndObjectMapper: TBoldObjectDefaultMapper;
  protected
    function GetIsOrdered: Boolean; virtual; abstract;
    procedure GetChangePoints(ObjectIDList: TBoldObjectIdList; Condition: TBoldChangePointCondition; NameSpace: TBoldSqlnameSpace); override;
    function GetColumnCount: Integer; override;
    procedure CompareValuesToLists(OwningId: TBoldObjectId; MemberInterface: IBoldValue; ListOfClosestEnd: TBoldObjectIdlist; ListOfRemoteEnd: TBoldObjectIdlist; FailureList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList); virtual; abstract;
    procedure StuffValuesFromLists(MemberInterface: IBoldValue; ListOfClosestEnd: TBoldObjectIdlist; ListOfRemoteEnd: TBoldObjectIdlist); virtual; abstract;
    function CompareField(ObjectContent: IBoldObjectContents; Field: IBoldField; ColumnIndex: integer; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList): Boolean; override;
    function EmbeddingMapper: TBoldEmbeddedSingleLinkDefaultMapper;
    function GetOtherEndObjectMapper: TBoldObjectDefaultMapper; override;
    function GetSupportsPolymorphicFetch: Boolean; override;
  public
    property ClosestColumnName: string read GetClosestColumnName;
    property RemoteInnerLinkMapper: TBoldEmbeddedSingleLinkDefaultMapper read GetRemoteInnerLinkMapper;
    property ClosestOtherEndObjectMapper: TBoldObjectDefaultMapper read GetClosestOtherEndObjectMapper;
    property IsIndirect: Boolean read fIsIndirect;
    property LinkClassTablename: string read GetLinkClassTableName;
    property LinkClassObjectMapper: TBoldObjectDefaultMapper read GetLinkClassObjectMapper;
    property Ordered: Boolean read GetIsOrdered;
    procedure PMFetch(ObjectIDList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; FetchMode: Integer; TranslationList: TBoldIdTranslationList; FailureList: TBoldObjectIdList); override;
    constructor CreateFromMold(moldMember: TMoldMember; moldClass: TMoldClass; Owner: TBoldObjectPersistenceMapper; const MemberIndex: Integer; TypeNameDictionary: TBoldTypeNameDictionary); override;
  end;

  { TBoldSingleLinkDefaultMapper }
  TBoldSingleLinkDefaultMapper = class(TBoldNonEmbeddedLinkDefaultMapper)
  protected
    function GetIsOrdered: Boolean; override;
    function FirstIdInList(List: TBoldObjectIdList): TBoldObjectId;
  end;

  { TBoldDirectSingleLinkDefaultmapper }
  TBoldDirectSingleLinkDefaultmapper = class(TBoldSingleLinkDefaultMapper)
  protected
    procedure StuffValuesFromLists(MemberInterface: IBoldValue; ListOfClosestEnd: TBoldObjectIdlist; ListOfRemoteEnd: TBoldObjectIdlist); override;
    procedure CompareValuesToLists(OwningId: TBoldObjectId; MemberInterface: IBoldValue; ListOfClosestEnd: TBoldObjectIdlist; ListOfRemoteEnd: TBoldObjectIdlist; FailureList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList); override;
    function GetLinkClassObjectMapper: TBoldObjectDefaultMapper; override;
   public
    constructor CreateFromMold(moldMember: TMoldMember; moldClass: TMoldClass; Owner: TBoldObjectPersistenceMapper; const MemberIndex: Integer; TypeNameDictionary: TBoldTypeNameDictionary); override;
    class function CanStore(const ContentName: string): Boolean; override;
  end;

  { TBoldIndirectSingleLinkDefaultmapper }
  TBoldIndirectSingleLinkDefaultmapper = class(TBoldSingleLinkDefaultMapper)
  protected
    procedure StuffValuesFromLists(MemberInterface: IBoldValue; ListOfClosestEnd: TBoldObjectIdlist; ListOfRemoteEnd: TBoldObjectIdlist); override;
    procedure CompareValuesToLists(OwningId: TBoldObjectId; MemberInterface: IBoldValue; ListOfClosestEnd: TBoldObjectIdlist; ListOfRemoteEnd: TBoldObjectIdlist; FailureList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList); override;
    function GetLinkClassObjectMapper: TBoldObjectDefaultMapper; override;
  public
    constructor CreateFromMold(moldMember: TMoldMember; moldClass: TMoldClass; Owner: TBoldObjectPersistenceMapper; const MemberIndex: Integer; TypeNameDictionary: TBoldTypeNameDictionary); override;
    class function CanStore(const ContentName: string): Boolean; override;
  end;

  { TBoldMultiLinkDefaultMapper }
  TBoldMultiLinkDefaultMapper = class(TBoldNonEmbeddedLinkDefaultMapper)
  private
    FOrdered: Boolean;
  protected
    function GetIsOrdered: Boolean; override;
  public
    constructor CreateFromMold(moldMember: TMoldMember; moldClass: TMoldClass; Owner: TBoldObjectPersistenceMapper; const MemberIndex: Integer; TypeNameDictionary: TBoldTypeNameDictionary); override;
  end;

  { TBoldDirectMultiLinkDefaultmapper }
  TBoldDirectMultiLinkDefaultmapper = class(TBoldMultiLinkDefaultMapper)
  protected
    procedure StuffValuesFromLists(MemberInterface: IBoldValue; ListOfClosestEnd: TBoldObjectIdlist; ListOfRemoteEnd: TBoldObjectIdlist); override;
    procedure CompareValuesToLists(OwningId: TBoldObjectId; MemberInterface: IBoldValue; ListOfClosestEnd: TBoldObjectIdlist; ListOfRemoteEnd: TBoldObjectIdlist; FailureList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList); override;
    function GetLinkClassObjectMapper: TBoldObjectDefaultMapper; override;
  public
    constructor CreateFromMold(moldMember: TMoldMember; moldClass: TMoldClass; Owner: TBoldObjectPersistenceMapper; const MemberIndex: Integer; TypeNameDictionary: TBoldTypeNameDictionary); override;
    class function CanStore(const ContentName: string): Boolean; override;
  end;

  { TBoldIndirectMultiLinkDefaultmapper }
  TBoldIndirectMultiLinkDefaultmapper = class(TBoldMultiLinkDefaultMapper)
  protected
    procedure StuffValuesFromLists(MemberInterface: IBoldValue; ListOfClosestEnd: TBoldObjectIdlist; ListOfRemoteEnd: TBoldObjectIdlist); override;
    procedure CompareValuesToLists(OwningId: TBoldObjectId; MemberInterface: IBoldValue; ListOfClosestEnd: TBoldObjectIdlist; ListOfRemoteEnd: TBoldObjectIdlist; FailureList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList); override;
    function GetLinkClassObjectMapper: TBoldObjectDefaultMapper; override;
  public
    constructor CreateFromMold(moldMember: TMoldMember; moldClass: TMoldClass; Owner: TBoldObjectPersistenceMapper; const MemberIndex: Integer; TypeNameDictionary: TBoldTypeNameDictionary); override;
    class function CanStore(const ContentName: string): Boolean; override;
  end;

implementation

uses
  SysUtils,
  BoldUtils,
  BoldLogHandler,
  BoldDefaultId,
  BoldPSDescriptionsSQL,
  BoldSQLMappingInfo,
  BoldPMappersSQL,
  BoldMath,
  BoldSubscription,
  BoldTaggedValueSupport,
  BoldPMapperLists,
  BoldGuard,
  BoldDefaultStreamNames,
  BoldPMConsts;

{ Supporting functions/procedures }

{Returns new ObjectId with owned ClassId, both must be freed}
function CreateAndEnsureId(ObjectId: Integer; ClassId: Integer; Exact: Boolean; TransLationlist: TBoldIdTranslationList; ValueSpace: IBoldValueSpace; TimeStamp: TBoldTimeStampType): TBoldDefaultId;
begin
  Assert (ClassID <> -1);
  if TimeStamp = BOLDMAXTIMESTAMP then
    Result := TBoldDefaultId.CreateWithClassId(ClassId, Exact)
  else
    result := TBoldTimestampedDefaultId.CreateWithTimeAndClassId(TimeStamp, ClassId, Exact);

  Result.AsInteger := ObjectId;
  if assigned(TranslationList) then
    TranslationList.AddTranslation(nil, Result); // needed?
  ValueSpace.EnsureObjectId(Result);
end;

{function TBoldLinkDefaultMapper.CreateIdForOtherEndFromQuery(aQuery: IBoldQuery; ObjectIDColumn, ClassIDColumn: integer): TBoldObjectId;
var
  ObjectPersistenceMapperOfOtherEnd: TBoldObjectPersistenceMapper;
begin
  Assert(not aQuery.Fields[ObjectIdColumn].IsNull and
        (aQuery.Fields[ObjectIdColumn].AsInteger <> INTERNALNULLKEY));
  If ClassIDColumn = -1 then
  begin
    result := CreateIdForOtherEndFromField(aQuery.Fields[ObjectIdColumn]);
  end
  else
  begin
    ObjectPersistenceMapperOfOtherEnd := SystemPersistenceMapper.ObjectPersistenceMappers[fObjectMapperIndexOfOtherEnd];
    Result := (SystemPersistenceMapper as TBoldSystemDefaultMapper).NewIdFromQuery(aQuery, ClassIdColumn, ObjectIdColumn)
  end;
end;     }

type
  TTempLinkValues = class(TObject)
  public
    ObjectId: Integer;
    Ordervalue: integer;
    ClosestId: integer;
    ClosestClassid: integer;
    RemoteId: integer;
  end;

function SortLinkValues(Item1, Item2: Pointer): integer;
begin
  result := TTempLinkValues(Item1).Objectid - TTempLinkValues(Item2).Objectid;
  if result = 0 then
    result := TTempLinkValues(Item1).OrderValue - TTempLinkValues(Item2).OrderValue;
  if result = 0 then
    result := TTempLinkValues(Item1).RemoteId - TTempLinkValues(Item2).RemoteId;
  if result = 0 then
    result := TTempLinkValues(Item1).ClosestId - TTempLinkValues(Item2).ClosestId;
end;

constructor TBoldNonEmbeddedLinkDefaultMapper.CreateFromMold(
  moldMember: TMoldMember; moldClass: TMoldClass;
  Owner: TBoldObjectPersistenceMapper; const MemberIndex: Integer;
  TypeNameDictionary: TBoldTypeNameDictionary);
begin
  inherited;
  fCustomFetch := True;
  fIsStoredInObject := IsStoredInObject and ((MoldMember as TMoldRole).RoleType in [rtRole, rtInnerLinkRole]);
end;

function TBoldNonEmbeddedLinkDefaultMapper.CompareField(ObjectContent: IBoldObjectContents; Field: IBoldField; ColumnIndex: integer; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList): Boolean;
begin
  raise EBold.CreateFmt(sCannotCallOnTransientClass, [classname]);
end;

function TBoldNonEmbeddedLinkDefaultMapper.GetColumnCount: Integer;
begin
  Result := 0;
end;

procedure TBoldNonEmbeddedLinkDefaultMapper.ProcessSQL(Query: IBoldQuery; WhereFragment: String; resultList: TList; TimeStamp: TBoldTimeStampType);
var
  i, j: integer;
  ClassIdRequired: Boolean;
  NextColumnIndex: integer;
  LinkData: TTempLinkValues;
  MappingInfo: TBoldMemberMappingArray;
  AIMappingInfo: TBoldAllInstancesMappingArray;
  EmbeddingColumnName, EmbeddingOrderColumnName: String;
  WhereClause, SelectClause: String;
  Selectlist: TStringList;
  sql: TStringList;
  OperatingOnRootTable: Boolean;
  RootTableJoin: string;
const
  LinkTableAlias: String = 'LinkTable_Alias';
  RootTableAlias: String = 'RootTable_Alias';
begin
  AIMappingInfo := nil;
  SelectList := TStringList.Create;
  sql := TStringList.Create;
  MappingInfo := SystemPersistenceMapper.MappingInfo.GetMemberMappings(ClosestOtherEndObjectMapper.ExpressionName, EmbeddingMapper.ExpressionName);
  try
    for i := 0 to length(MappingInfo) - 1 do
    begin
      EmbeddingColumnName := MappingInfo[i].ColumnByIndex[0];
      if Ordered then
        EmbeddingOrderColumnName := MappingInfo[i].ColumnByIndex[1]
      else
        EmbeddingOrderColumnName := '';

      SelectList.Clear;
      {ID, TYPE, ClosstId, [OderColumn], [RemoteColumn]}
      SelectList.Add(LinkTableAlias + '.' + IDCOLUMN_NAME);
      SelectList.Add(LinkTableAlias + '.' + TYPECOLUMN_NAME); // FIXME hardwired
      SelectList.Append(LinkTableAlias + '.' + EmbeddingColumnName);
      // there is no need to add an OrderBy clause since the list will be ordered in memory prior to processing
      if ordered then
        SelectList.Append(EmbeddingOrderColumnName);
      if IsIndirect then
        SelectList.Append(Format('%s', [RemoteInnerLinkMapper.MainColumnName]));
      SelectClause := Format('SELECT %s', [BoldSeparateStringList(SelectList, ', ', '', '')]); // do not localize

      WhereClause := Format('WHERE (%s.%s) %s', [LinkTableAlias, EmbeddingColumnName, WhereFragment]); // do not localize

      SQL.Clear;
      SQL.Add(SelectClause);
      SQL.Add('FROM '+ MappingInfo[i].TableName + ' ' + LinkTableAlias); // do not localize

      OperatingOnRootTable := SameText(MappingInfo[i].TableName, SystemPersistenceMapper.RootClassObjectPersistenceMapper.Maintable.SQLName);

      RootTableJoin := format('((%s.%s = %s.%s) and (%s.%s = %s.%s))', [ // do not localize
            LinkTableAlias, TIMESTAMPSTARTCOLUMNNAME,
            RootTableAlias, TIMESTAMPSTARTCOLUMNNAME,
            LinkTableAlias, IDCOLUMN_NAME,
            RootTableAlias, IDCOLUMN_NAME]);

      // Add the root table if it is needed and not already there

      if ClosestOtherEndObjectMapper.Versioned and not OperatingOnRootTable then
      begin
        if SystemPersistenceMapper.SQLDataBaseConfig.UseSQL92Joins then
          SQL.append(format(' left join %s %s on %s', [ // do not localize
            SystemPersistenceMapper.RootClassObjectPersistenceMapper.MainTable.SQLName,
            RootTableAlias,
            RootTableJoin] ))
        else
          SQL.Append(format(', %s %s', [ // do not localize
            SystemPersistenceMapper.RootClassObjectPersistenceMapper.MainTable.SQLName,
            RootTableAlias] ));
      end;

      SQL.Add(WhereClause);

      ClassIDRequired := true;
      AIMappingInfo := SystemPersistenceMapper.MappingInfo.GetAllInstancesMapping(ClosestOtherEndObjectMapper.ExpressionName);
      for j := 0 to length(AIMappingInfo) - 1 do
        if SameText(AIMappingInfo[j].TableName, MappingInfo[i].TableName) and
           not AIMappingInfo[j].ClassIdRequired then
          ClassIdRequired := false;

      if ClassIdRequired then
        SQL.Add(format('AND (%s in (%s))', [TYPECOLUMN_NAME, ClosestOtherEndObjectMapper.SubClassesID])); // do not localize

      // add timestamp conditions and nessesary joins
      if ClosestOtherEndObjectMapper.Versioned then
      begin
        if OperatingOnRootTable then
          ClosestOtherEndObjectMapper.RetrieveTimeStampCondition(SQL, TimeStamp, false, 'AND', True, LinkTableAlias, LinkTableAlias) // do not localize
        else
          ClosestOtherEndObjectMapper.RetrieveTimeStampCondition(SQL, TimeStamp, false, 'AND', True, LinkTableAlias, RootTableAlias); // do not localize
        if not OperatingOnRootTable and not SystemPersistenceMapper.SQLDataBaseConfig.UseSQL92Joins then
          SQL.Append(format('and %s', [RootTableJoin])); // do not localize
      end;

      Query.AssignSQL(SQL);
      Query.Open;
      while not Query.Eof do
      begin
        LinkData := TTempLinkValues.Create;
        LinkData.ClosestId := Query.Fields[0].AsInteger;
        LinkData.ClosestClassid := Query.Fields[1].AsInteger;
        LinkData.ObjectId := Query.Fields[2].AsInteger;
        NextColumnIndex := 3;
        if ordered then
        begin
          LinkData.Ordervalue := Query.Fields[NextColumnIndex].AsInteger;
          INC(NextColumnIndex);
        end
        else
          LinkData.OrderValue := 0;

        if isIndirect then
          LinkData.RemoteId := Query.Fields[NextColumnIndex].AsInteger;
        resultList.Add(LinkData);
        Query.Next;
      end;
    end;
  finally
    SelectList.Free;
    sql.free;
  end;
end;

procedure TBoldNonEmbeddedLinkDefaultMapper.ProcessResult(ResultList: TList; ValueSpace: IBoldValueSpace; ObjectIdList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList; TimeStamp: TBoldTimeStampType; FetchMode: integer; FailureList: TBoldObjectIdList);
var
  UnprocessedObjects: TBoldObjectidList;
  ListOfClosestEnd: TBoldObjectIdlist;
  ListOfRemoteEnd: TBoldObjectIdlist;
  MemberID: TBoldMemberID;

  procedure ProcessResultForOneObject(CurrentObjectId: TBoldObjectId);
  var
    MemberInterface: IBoldValue;
    ObjectContents: IBoldObjectContents;
    i, OldfailureCount: integer;
  begin
    ObjectContents := ValueSpace.EnsuredObjectContentsByObjectId[CurrentobjectId];
    ObjectContents.EnsureMember(MemberId, ContentName);
    MemberInterface := ObjectContents.ValueByMemberId[MemberID];
    if FetchMode = fmCompare then
    begin
      OldFailureCount := failureLIst.Count;
      CompareValuestoLists(CurrentObjectId, MemberInterface, ListOfClosestEnd, ListOfRemoteEnd, FailureList, TranslationList);
      if FailureList.Count > OldFailureCount then
      begin
        BoldLog.LogFmt(sOptimisticLockingFailedForTheFollowing, [
          ObjectPersistenceMapper.ExpressionName,
          ExpressionName]);
        for i := OldFailureCount to FailureList.Count - 1 do
          BoldLog.LogFmt(sLogIdAsString, [FailureList[i].AsString]);
      end;
    end
    else if MemberInterface.BoldPersistenceState = bvpsInvalid then
      StuffValuesFromLists(MemberInterface, ListOfClosestEnd, ListOfRemoteEnd);
    ListOfClosestEnd.Clear;
    ListOfRemoteEnd.Clear;
    UnprocessedObjects.RemoveByIndex(UnprocessedObjects.IndexByID[CurrentobjectId]);
  end;

  procedure EnsureAndAddToList(List: TBoldObjectIdList; ClassId: integer; Exact: Boolean; IdAsInteger: integer);
  var
    BoldId: TBoldDefaultId;
    BoldGuard: IBoldGuard;
  begin
    BoldGuard := TBoldGuard.Create(BoldID);
    BoldId := CreateAndEnsureId(IdAsInteger,
                                ClassId,
                                Exact,
                                TranslationList,
                                ValueSpace,
                                TimeStamp);
    List.Add(BoldId);
  end;

  function FindInListByIdAsInteger(List: TBoldObjectIdList; IdAsInteger: integer): TBoldObjectId;
  var
    BoldId: TBoldTimestampedDefaultId;
    BoldGuard: IBoldGuard;
  begin
    BoldGuard := TBoldGuard.Create(BoldID);
    BoldId := TBoldTimestampedDefaultId.Create;
    BoldId.AsInteger := IdAsInteger;
    BoldId.TimeStamp := timeStamp;
    Result := List.IDByID[BoldId];
  end;

var
  I,
  CurrentId: Integer;
  Currentresult: TTempLinkValues;
  RemoteOtherEndExact: Boolean;
  TranslatedClassId: integer;
  BoldGuard: IBoldGuard;
begin
  BoldGuard := TBoldGuard.Create(ListOfRemoteEnd, ListOfClosestEnd, UnprocessedObjects, MemberID);
  RemoteOtherEndExact := IsIndirect and
                         (not ObjectPersistenceMappers[RemoteOtherEndObjectMapperIndex].HasSubClasses);

  UnprocessedObjects := ObjectIDList.Clone;
  ListOfClosestEnd := TBoldObjectIdlist.Create;
  ListOfRemoteEnd := TBoldObjectIdlist.Create;
  MemberID := TBoldMemberId.Create(MemberIndex);

  if resultList.Count > 0 then
  begin
    CurrentId := TTempLinkValues(ResultList[0]).ObjectId;
    for i := 0 to ResultList.Count - 1 do
    begin
      CurrentResult := TTempLinkValues(ResultList[i]);
      if CurrentId <> CurrentResult.ObjectId then
      begin
         ProcessResultForOneObject(FindInListByIdAsInteger(UnprocessedObjects, CurrentId));
         CurrentId := CurrentResult.ObjectId;
      end;
      TranslatedClassId := SystemPersistenceMapper.TopSortedIndexForBoldDbType(CurrentResult.ClosestClassid);
      EnsureAndAddToList(ListOfClosestEnd, TranslatedClassId, True, CurrentResult.ClosestId);
      if IsIndirect then
        EnsureAndAddToList(ListOfRemoteEnd,
                           RemoteOtherEndObjectMapperIndex,
                           RemoteOtherEndExact,
                           CurrentResult.RemoteId);
    end;

    ProcessResultForOneObject(FindInListByIdAsInteger(UnprocessedObjects, CurrentId));
  end;

  // clear links for objects that had no related objects in the database

  while UnprocessedObjects.Count > 0 do
    ProcessResultForOneObject(UnprocessedObjects[0] as TBoldDefaultId); {Lists empty at this point}
end;

(*
function TBoldNonEmbeddedLinkDefaultMapper.CreateSelectClause: string;
var
  SelectList: TStringList;
begin
  SelectList := TStringList.Create;
  try
   { ID, TYPE, ClosstId, [OderColumn], [RemoteColumn]}
    SelectList.Add(IDCOLUMN_NAME);
    SelectList.Add(TYPECOLUMN_NAME); // FIXME hardwired
    SelectList.Append(EmbeddingMapper.MainColumnName);
    if ordered then
      SelectList.Append(EmbeddingMapper.OrderColumnName);
    if IsIndirect then
      SelectList.Append(Format('%s', [RemoteInnerLinkMapper.MainColumnName]));
    Result := Format('SELECT %s', [BoldSeparateStringList(SelectList, ', ', '', '')]);
  finally
    SelectList.Free;
  end;
end;
*)

procedure TBoldNonEmbeddedLinkDefaultMapper.PMFetch(ObjectIDList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; FetchMode: Integer; TranslationList: TBoldIdTranslationList; FailureList: TBoldObjectIdList);
var
  TimeStamp: TBoldTimeStampType;
  ResultList: TList;
  start, stop: integer;
  Block, ObjectCount, I: Integer;
  WhereFragment: String;
  TopSortedIndex: Integer;
  aQuery: IBoldQuery;
begin
  if ObjectIDList.Count = 0 then
    Exit;

  TopSortedIndex := ObjectPersistenceMapper.TopSortedIndex;

  // splitting on timestamps is performed by the objectpersistencemapper already

  TimeStamp := ObjectIdList[0].TimeStamp;

  for I := 0 to ObjectIDList.Count - 1 do
  begin
    if SupportsPolymorphicFetch then
    begin
      if not SystemPersistenceMapper.ObjectPersistenceMappers[ObjectIDList[I].TopSortedIndex].BoldIsA(ObjectPersistenceMapper) then
        raise EBoldInternal.CreateFmt('%s.PMFetch: Not homogenous Object-list', [classname]);
    end
    else
    begin
      if ObjectIDList[I].TopSortedIndex <> TopSortedIndex then
        raise EBoldInternal.CreateFmt('%s.PMFetch: Not homogenous Object-list', [classname]);
    end;
    if ObjectIDList[I].TimeStamp <> TimeStamp then
      raise EBoldInternal.CreateFmt('%s.PMFetch: Objectlist contains multiple timestamps', [classname]);
  end;

  ResultList := TList.Create;
  aQuery := SystemPersistenceMapper.GetQuery;

  try
    ObjectCount := ObjectIDList.Count - 1;
    // only do the fetching in blocks, the actual processing must be done in one go
    // since ProcessResult will clear all objects with empty links
    // besides, the point of blockfetching is mainly to reduce the size of the SQL-statement
    for Block := 0 to (ObjectCount div SystemPersistenceMapper.SQLDataBaseConfig.FetchBlockSize) do
    begin
      aQuery.ClearParams;
      Start := Block * SystemPersistenceMapper.SQLDataBaseConfig.FetchBlockSize;
      Stop := MinIntValue([Pred(Succ(Block) * SystemPersistenceMapper.SQLDataBaseConfig.FetchBlockSize), ObjectCount]);
      WhereFragment := ObjectPersistenceMapper.IdListSegmentToWhereFragment(ObjectIdList, Start, Stop, aQuery);
      ProcessSQL(aQuery, WhereFragment, resultList, TimeStamp);
    end;
    ResultList.Sort(SortLinkValues);
    BoldPMLogFmt(sLogFetchIDs, [ResultLIst.Count, ObjectIdList.Count, ObjectPersistenceMapper.ExpressionName, ExpressionName]);

    ProcessResult(ResultList, ValueSpace, ObjectIdList, TranslationList, timeStamp, FetchMode, FailureList);
  finally
    for i := 0 to ResultList.Count - 1 do
      TObject(ResultList[i]).Free;
    ResultList.Free;
    SystemPersistenceMapper.ReleaseQuery(aQuery)
  end;
end;

{ TBoldEmbeddedSingleLinkDefaultMapper }

function TBoldEmbeddedSingleLinkDefaultMapper.GetColumnCount: Integer;
begin
  Result := fColumnCount;
end;

function TBoldEmbeddedSingleLinkDefaultMapper.GetColumnTypeAsSQL(ColumnIndex: Integer): string;
begin
  case ColumnIndex of
    0, 1: Result := SystemPersistenceMapper.SQLDataBaseConfig.ColumnTypeForInteger;
  else
    raise EBoldBadColumnIndex.CreateFmt(sIllegalColumnIndex, [ClassName, 'GetColumnTypeAsSQL', ColumnIndex]); // do not localize
  end;
end;

function TBoldEmbeddedSingleLinkDefaultMapper.GetAllowNullAsSQL: string;
begin
  Result := '';
end;

function TBoldEmbeddedSingleLinkDefaultMapper.GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType;
begin
  case ColumnIndex of
    0, 1: Result := ftInteger;
  else
    raise EBoldBadColumnIndex.CreateFmt(sIllegalColumnIndex, [ClassName, 'GetColumnBDEFieldType', ColumnIndex]); // do not localize
  end;
end;

function TBoldEmbeddedSingleLinkDefaultMapper.GetColumnSize(ColumnIndex: Integer): Integer;
begin
  Result := 0;
end;

function TBoldEmbeddedSingleLinkDefaultMapper.CompareField(ObjectContent: IBoldObjectContents; Field: IBoldField; ColumnIndex: integer; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList): Boolean;
var
  anIdRef: IBoldObjectIdRef;
begin
  result := true;
  case ColumnIndex of
    0: begin
      anIdRef := GetValue(ObjectContent) as IBoldObjectIdRef;
      if Field.IsNull or (Field.AsInteger = INTERNALNULLKEY) then
        result := not assigned(anIdRef.Id)
      else if not assigned(anIdRef.Id) then
        result := false
      else
        result := IntToStr(Field.AsInteger) = anIdRef.Id.AsString;
    end;
    1: begin
      anIdRef := GetValue(ObjectContent) as IBoldObjectIdRef;
      if Field.IsNull or (Field.AsInteger = INTERNALNULLKEY) then
        result := true
      else if not assigned(anIdRef.Id) then
        result := true
      else
        result := Field.AsInteger = anIdRef.OrderNo;
    end;
  end;
end;

function TBoldEmbeddedSingleLinkDefaultMapper.GetInitialColumnName(ColumnIndex: Integer): string;
begin
  case ColumnIndex of
    0: Result := InitialColumnRootName;
    1: Result := InitialColumnRootName + '_O';
  else
    raise EBoldBadColumnIndex.CreateFmt(sIllegalColumnIndex, [classname, 'GetInitialColumnName', ColumnIndex]); // do not localize
  end;
end;

procedure TBoldEmbeddedSingleLinkDefaultMapper.ValueToParam(ObjectContent: IBoldObjectContents; Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList);
var
  SingleLink: IBoldObjectIdRef;
begin
  SingleLink := GetEnsuredValue(ObjectContent) as IBoldObjectIdRef;
  case ColumnIndex of
    0:
      if assigned(SingleLink.Id) then
        Param.AsInteger := (TranslationList.TranslateToNewID[SingleLink.Id] as TBoldDefaultId).asInteger
      else
        Param.AsInteger := INTERNALNULLKEY;
    1:
      Param.AsInteger := SingleLink.OrderNo;
  else
    raise EBoldBadColumnIndex.CreateFmt(sIllegalColumnIndex, [ClassName, 'ValueToParam', ColumnIndex]); // do not localize
  end;
end;

procedure TBoldEmbeddedSingleLinkDefaultMapper.ValueFromField(OwningObjectId: TBoldObjectId; ObjectContent: IBoldObjectContents; ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; Field: IBoldField; ColumnIndex: Integer);
var
  ObjectId: TBoldObjectId;
  anIdRef: IBoldObjectIdRef;
begin
  anIdRef := GetEnsuredValue(ObjectContent) as IBoldObjectIdRef;
  case ColumnIndex of
    0:
      if not Field.IsNull and (Field.AsInteger <> INTERNALNULLKEY) then
      begin
        ObjectId := CreateAndEnsureId(Field.AsInteger, OtherEndObjectPMIndex, OtherEndExact, TranslationList, ValueSpace, OwningObjectId.TimeStamp);
        anIdRef.SetFromId(ObjectId);
        ObjectID.Free;
      end
      else
       anIdRef.SetFromId(nil);
    1:
      anIdRef.Orderno := Field.AsInteger;
    else
      raise EBoldBadColumnIndex.CreateFmt(sIllegalColumnIndex, [ClassName, 'ValueFromField', ColumnIndex]); // do not localize
  end;
end;

class function TBoldEmbeddedSingleLinkDefaultMapper.CanStore(const ContentName: String): Boolean;
begin
  Result := AnsiCompareText(ContentName, BoldContentName_ObjectIdRef) = 0;
end;

class function TBoldIndirectSingleLinkDefaultMapper.CanStore(const ContentName: String): Boolean;
begin
  Result := AnsiCompareText(ContentName, BoldContentName_ObjectIdRefPair) = 0;
end;

function TBoldSingleLinkDefaultMapper.GetIsOrdered: Boolean;
begin
  Result := False;
end;

{ TBoldMultiLinkDefaultMapper }
constructor TBoldMultiLinkDefaultMapper.CreateFromMold(moldMember: TMoldMember; moldClass: TMoldClass; Owner: TBoldObjectPersistenceMapper; const MemberIndex: Integer; TypeNameDictionary: TBoldTypeNameDictionary);
begin
  inherited;
  FOrdered := (moldMember as TMoldRole).EffectiveOrdered;
end;

function TBoldMultiLinkDefaultMapper.GetIsOrdered: Boolean;
begin
  Result := FOrdered;
end;

class function TBoldDirectMultiLinkDefaultMapper.CanStore(const ContentName: String): Boolean;
begin
  Result := AnsiCompareText(ContentName, BoldContentName_ObjectIdListRef) = 0;
end;

class function TBoldIndirectMultiLinkDefaultMapper.CanStore(const ContentName: String): Boolean;
begin
  Result := AnsiCompareText(ContentName, BoldContentName_ObjectIdListRefPair) = 0;
end;

{ TBoldDirectSingleLinkDefaultmapper }

constructor TBoldEmbeddedSingleLinkDefaultMapper.CreateFromMold(
  moldMember: TMoldMember; moldClass: TMoldClass;
  Owner: TBoldObjectPersistenceMapper; const MemberIndex: Integer;
  TypeNameDictionary: TBoldTypeNameDictionary);
var
  Role: TMoldRole;
  ClassOfOtherEnd: TMoldClass;
begin
  Role := moldMember as TMoldRole;
  ClassOfOtherEnd := Role.OtherEnd.moldClass;
  if Role.OtherEnd.EffectiveOrdered then
    fColumnCount := 2
  else
    fColumnCount := 1;
  inherited;  { note, must be called after fColumnCount has been set}
  fIsStoredInObject := IsStoredInObject and (Role.RoleType in [rtRole, rtInnerLinkRole]);
  fOtherEndObjectPMIndex := ClassOfOtherEnd.TopSortedIndex;
  fOtherEndExact :=  ClassOfOtherEnd.SubClasses.Count = 0;
  fOtherEndMemberIndex := ClassOfOtherEnd.AllBoldMembers.IndexOf(role.OtherEnd);
  fIsInherited := MoldMember.MoldClass <> MoldClass;
end;

{ TBoldDirectSingleLinkDefaultmapper }

class function TBoldDirectSingleLinkDefaultmapper.CanStore(const ContentName: string): Boolean;
begin
  Result := AnsiCompareText(ContentName, BoldContentName_ObjectIdRef) = 0;
end;

constructor TBoldDirectSingleLinkDefaultmapper.CreateFromMold(
  moldMember: TMoldMember; moldClass: TMoldClass;
  Owner: TBoldObjectPersistenceMapper; const MemberIndex: Integer;
  TypeNameDictionary: TBoldTypeNameDictionary);
var
  Role: TMoldRole;
  ClassOfOtherEnd: TMoldClass;
begin
  inherited;

  Role := moldMember as TMoldRole;
  ClassOfOtherEnd := Role.OtherEnd.moldClass;
  fClosestOtherEndObjectMapperIndex := ClassOfOtherEnd.TopSortedIndex;
  fClosestOtherEndMemberIndex := ClassOfOtherEnd.AllBoldMembers.IndexOf(Role.OtherEnd);
  fRemoteOtherEndObjectMapperIndex := fClosestOtherEndObjectMapperIndex;
  fRemoteInnerLinkMemberIndex := -1;
end;

function TBoldDirectSingleLinkDefaultmapper.GetLinkClassObjectMapper: TBoldObjectDefaultMapper;
begin
  result := nil;
end;

procedure TBoldDirectSingleLinkDefaultmapper.CompareValuesToLists(OwningId: TBoldObjectId; MemberInterface: IBoldValue; ListOfClosestEnd: TBoldObjectIdlist; ListOfRemoteEnd: TBoldObjectIdlist; FailureList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList);
var
  IdRef: IBoldObjectIdRef;
begin
  IdRef := MemberInterface as IBoldObjectIdRef;
  if assigned(IdRef.Id) then
  begin
    if (ListOfClosestEnd.Count <> 1) or not ListOfClosestEnd[0].IsEqual[TranslationList.TranslateToNewId[IdRef.Id]] then
      FailureList.AddIfNotInList(OwningId);
  end
  else
  begin
    if ListOfClosestEnd.Count <> 0 then
      FailureList.AddIfNotInList(OwningId);
  end;
end;

procedure TBoldDirectSingleLinkDefaultmapper.StuffValuesFromLists(
  MemberInterface: IBoldValue; ListOfClosestEnd,
  ListOfRemoteEnd: TBoldObjectIdlist);
begin
   (MemberInterface as IBoldObjectIdRef).SetFromId(FirstIdInList(ListOfClosestEnd));
end;

{ TBoldIndirectSingleLinkDefaultmapper }

constructor TBoldIndirectSingleLinkDefaultmapper.CreateFromMold(
  moldMember: TMoldMember; moldClass: TMoldClass;
  Owner: TBoldObjectPersistenceMapper; const MemberIndex: Integer;
  TypeNameDictionary: TBoldTypeNameDictionary);
var
  Role: TMoldRole;
  LinkClass: TMoldClass;
begin
  inherited;
  Role := moldMember as TMoldRole;
  fIsIndirect := True;
  LinkClass := Role.Association.LinkClass;
  fClosestOtherEndObjectMapperIndex := LinkClass.TopSortedIndex;
  fClosestOtherEndMemberIndex := LinkClass.AllBoldMembers.IndexOf(Role.LinkRole.OtherEnd);
  fRemoteOtherEndObjectMapperIndex := Role.OtherEnd.MoldClass.TopSortedIndex;

  fRemoteInnerLinkMemberIndex := LinkClass.AllBoldMembers.IndexOf(Role.OtherEnd.LinkRole.OtherEnd);
  if (LinkClass.TableMapping = tmChildren) then
    raise EBoldFeatureNotImplementedYet.CreateFmt(sChildMappedLinkClassesNotSupported,
                                                  [MoldClass.name, Role.Name, LinkClass.Name]);
end;

function TBoldIndirectSingleLinkDefaultmapper.GetLinkClassObjectMapper: TBoldObjectDefaultMapper;
begin
  result := ObjectPersistenceMappers[ClosestOtherEndObjectMapperIndex];
end;

procedure TBoldIndirectSingleLinkDefaultmapper.CompareValuesToLists(OwningId: TBoldObjectId; MemberInterface: IBoldValue; ListOfClosestEnd: TBoldObjectIdlist; ListOfRemoteEnd: TBoldObjectIdlist; FailureList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList);
var
  IdRefPair: IBoldObjectIdRefPair;
begin
  IdRefPair := MemberInterface as IBoldObjectIdRefPair;
  if assigned(IdRefPair.Id1) then
  begin
    if (ListOfClosestEnd.Count <> 1) or not ListOfClosestEnd[0].IsEqual[TranslationList.TranslateToNewId[IdRefPair.Id1]] then
      FailureList.AddIfNotInList(OwningId)
    else if (ListOfRemoteEnd.Count <> 1) or not ListOfRemoteEnd[0].IsEqual[TranslationList.TranslateToNewId[IdRefPair.Id2]] then
      FailureList.AddIfNotInList(OwningId);
  end
  else
  begin
    if ListOfClosestEnd.Count <> 0 then
      FailureList.AddIfNotInList(OwningId);
  end;
end;

procedure TBoldIndirectSingleLinkDefaultmapper.StuffValuesFromLists(
  MemberInterface: IBoldValue; ListOfClosestEnd,
  ListOfRemoteEnd: TBoldObjectIdlist);
begin
  (MemberInterface as IBoldObjectIdRefPair).SetFromIds(FirstIdInList(ListOfClosestEnd), FirstIdInList(ListOfRemoteEnd));
end;

{ TBoldDirectMultiLinkDefaultmapper }

procedure TBoldDirectMultiLinkDefaultmapper.CompareValuesToLists(
  OwningId: TBoldObjectId; MemberInterface: IBoldValue; ListOfClosestEnd,
  ListOfRemoteEnd, FailureList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList);
var
  IdList: IBoldObjectIdListRef;
  Equal: Boolean;
  i: integer;
begin
  IdList := MemberInterface as IBoldObjectIdListRef;
  Equal := IdList.Count = ListOfClosestEnd.Count;
  if Equal then
  begin
    for i := 0 to IdList.Count - 1 do
      // note, even if the link is ordered, we compare the lists unordered since
      // the main purpose is to ensure consistency, and the order of the
      // OptimisticLocking-value can not be guaranteed
      equal := equal and ListOfClosestEnd.IdInList[TranslationList.TranslateToNewId[IdList.IdList[i]]];
  end;
  if not equal then
    FailureList.AddIfNotInList(OwningId);
end;

constructor TBoldDirectMultiLinkDefaultmapper.CreateFromMold(
  moldMember: TMoldMember; moldClass: TMoldClass;
  Owner: TBoldObjectPersistenceMapper; const MemberIndex: Integer;
  TypeNameDictionary: TBoldTypeNameDictionary);
var
  Role: TMoldRole;
  ClassOfOtherEnd: TMoldClass;
begin
  inherited;

  Role := moldMember as TMoldRole;
  ClassOfOtherEnd := Role.OtherEnd.moldClass;
  fClosestOtherEndObjectMapperIndex := ClassOfOtherEnd.TopSortedIndex;
  fClosestOtherEndMemberIndex := ClassOfOtherEnd.AllBoldMembers.IndexOf(Role.OtherEnd);
  fRemoteOtherEndObjectMapperIndex := fClosestOtherEndObjectMapperIndex;
  fRemoteInnerLinkMemberIndex := -1;
end;

function TBoldDirectMultiLinkDefaultmapper.GetLinkClassObjectMapper: TBoldObjectDefaultMapper;
begin
  result := nil;
end;

procedure TBoldDirectMultiLinkDefaultmapper.StuffValuesFromLists(
  MemberInterface: IBoldValue; ListOfClosestEnd,
  ListOfRemoteEnd: TBoldObjectIdlist);
begin
  (MemberInterface as IBoldObjectIdListRef).SetFromIdList(ListOfClosestEnd);
end;

{ TBoldIndirectMultiLinkDefaultmapper }

procedure TBoldIndirectMultiLinkDefaultmapper.CompareValuesToLists(
  OwningId: TBoldObjectId; MemberInterface: IBoldValue; ListOfClosestEnd,
  ListOfRemoteEnd, FailureList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList);
var
  IdLists: IBoldObjectIdListRefPair;
  Equal: Boolean;
  i: integer;
begin
  IdLists := MemberInterface as IBoldObjectIdListRefPair;
  Equal := IdLists.Count = ListOfClosestEnd.Count;
  if Equal then
  begin
    for i := 0 to IdLists.Count - 1 do
      if Ordered then
        equal := equal and
          ListOfClosestEnd[i].IsEqual[TranslationList.TranslateToNewId[IdLists.IdList1[i]]] and
          ListOfRemoteEnd[i].IsEqual[TranslationList.TranslateToNewId[IdLists.IdList2[i]]]
      else
        equal := equal and
          ListOfClosestEnd.IdInList[TranslationList.TranslateToNewId[IdLists.IdList1[i]]] and
          ListOfRemoteEnd.IdInList[TranslationList.TranslateToNewId[IdLists.IdList2[i]]];
  end;
  if not equal then
    FailureList.AddIfNotInList(OwningId);
end;

constructor TBoldIndirectMultiLinkDefaultmapper.CreateFromMold(
  moldMember: TMoldMember; moldClass: TMoldClass;
  Owner: TBoldObjectPersistenceMapper; const MemberIndex: Integer;
  TypeNameDictionary: TBoldTypeNameDictionary);
var
  Role: TMoldRole;
  LinkClass: TMoldClass;
begin
  inherited;
  Role := moldMember as TMoldRole;
  fIsIndirect := True;
  LinkClass := Role.Association.LinkClass;
  fClosestOtherEndObjectMapperIndex := LinkClass.TopSortedIndex;
  fClosestOtherEndMemberIndex := LinkClass.AllBoldMembers.IndexOf(Role.LinkRole.OtherEnd);
  fRemoteOtherEndObjectMapperIndex := Role.OtherEnd.MoldClass.TopSortedIndex;
  fRemoteInnerLinkMemberIndex := LinkClass.AllBoldMembers.IndexOf(Role.OtherEnd.LinkRole.OtherEnd);

  if (LinkClass.TableMapping = tmChildren) then
    raise EBoldFeatureNotImplementedYet.CreateFmt(sChildMappedLinkClassesNotSupported,
                                                  [MoldClass.name, Role.Name, LinkClass.Name]);
end;

function TBoldIndirectMultiLinkDefaultmapper.GetLinkClassObjectMapper: TBoldObjectDefaultMapper;
begin
  result := ObjectPersistenceMappers[ClosestOtherEndObjectMapperIndex];
end;

procedure TBoldIndirectMultiLinkDefaultmapper.StuffValuesFromLists(
  MemberInterface: IBoldValue; ListOfClosestEnd,
  ListOfRemoteEnd: TBoldObjectIdlist);
begin
  (MemberInterface as IBoldObjectIdListRefPair).SetFromIdLists(ListOfClosestEnd, ListOfRemoteEnd);
end;

{ TBoldSingleLinkDefaultMapper }

function TBoldSingleLinkDefaultMapper.FirstIdInList(List: TBoldObjectIdList): TBoldObjectId;
begin
  Assert(List.Count <= 1);
  if List.Count = 0 then
    Result := nil
  else
    Result := List[0];
end;

procedure TBoldNonEmbeddedLinkDefaultMapper.GetChangePoints(
  ObjectIDList: TBoldObjectIdList; Condition: TBoldChangePointCondition; NameSpace: TBoldSqlnameSpace);
begin
  (ObjectPersistenceMappers[fClosestOtherEndObjectMapperIndex].
    MemberPersistenceMappers[ClosestOtherEndMemberMapperIndex] as TBoldEmbeddedSingleLinkDefaultMapper).
    GetNonEmbeddedChangePoints(ObjectIdList, Condition, NameSpace);
end;

procedure TBoldEmbeddedSingleLinkDefaultMapper.GetNonEmbeddedChangePoints(
  ObjectIdList: TBoldObjectIdList; Condition: TBoldChangePointCondition; NameSpace: TBoldSqlnameSpace);

  procedure FetchIds(SQLQuery: TBoldSQLQuery; AddTime: integer);
  var
    aQuery: IBoldQuery;
    ObjectId: TBoldObjectId;
    sql: TStringList;
    Timestamp: TBoldTimestampType;
  begin
    aQuery := SystemPersistenceMapper.GetQuery;
    SQL := TStringList.Create;
    try
      SQLQuery.GenerateSQL(SQL);
      aQuery.AssignSQL(SQL);
      aQuery.Open;
      aQuery.First;
      while not aQuery.EOF do
      begin
        TimeStamp := aQuery.Fields[1].AsInteger;
        if TimeStamp <> BOLDMAXTIMESTAMP then
        begin
          ObjectId := NewDefaultId(aQuery.Fields[0].AsInteger, Timestamp + AddTime);
          ObjectIDList.Add(ObjectId);
          SendExtendedEvent(bpeFetchId, [ObjectId]);
          ObjectId.Free;
        end;
        aQuery.Next;
      end;
    finally
      SystemPersistenceMapper.ReleaseQuery(aQuery);
      SQL.Free;
    end;
  end;

  function MakeMainQuery(var NewTableRef: TBoldSQLTableReference; MemberMapping: TBoldMemberMappingInfo): TBoldSQLQuery;
  var
    pointerColumnRef: TBoldSQLColumnReference;
  begin
    result := TBoldSQLQuery.Create(qmSelect, SystemPersistenceMapper.PSSystemDescription, SystemPersistenceMapper.SQLDataBaseConfig, NameSpace);
    result.IgnoreHistoricObjects := false;
    NewTableRef := result.AddTableReference(MemberMapping.TableName);
    pointerColumnRef := NewTableRef.GetColumnReference(MemberMapping.ColumnByIndex[0]);
    result.AddColumnToRetrieve(pointerColumnRef);
    result.AddWCF(TBoldSQLWCFBinaryInfix.CreateWCFForIdList(pointerColumnRef,
                                                                                           Condition.IdList));
  end;

  function MakeSubQuery(Query: TBoldSQLQuery; JoinTableRef: TBoldSQLTableReference; var NewTableRef: TBoldSQLTableReference; MemberMapping: TBoldMemberMappingInfo): TBoldSQLQuery;
  begin
    result := TBoldSQLQuery.Create(qmSelect, SystemPersistenceMapper.PSSystemDescription, SystemPersistenceMapper.SQLDataBaseConfig, NameSpace);
    result.IgnoreHistoricObjects := false;
    NewTableRef := result.AddTableReference(MemberMapping.TableName);
    result.AddJoin(NewTableRef.GetColumnReference(MemberMapping.ColumnByIndex[0]), JoinTableRef.GetColumnReference(MemberMapping.ColumnByIndex[0]));
    result.AddJoin(NewTableRef.GetColumnReference(IDCOLUMN_NAME), JoinTableRef.GetColumnReference(IDCOLUMN_NAME));
    Query.AddWCF(TBoldSQLWCFUnaryPrefix.Create(TBoldSQLWCFExists.Create(result, NewTableRef),
                                               'not')); // do not localize
  end;

  function JoinRootTableInto(Query: TBoldSQLQuery; TableRef: TBoldSQLTableReference): TBoldSQLTableReference;
  begin
    result := Query.AddTableReference(SystemPersistenceMapper.RootClassObjectPersistenceMapper.MainTable.SQLName);
    Query.AddJoin(result.GetColumnReference(IDCOLUMN_NAME), TableRef.GetColumnReference(IDCOLUMN_NAME));
    Query.AddJoin(result.GetColumnReference(TIMESTAMPSTARTCOLUMNNAME), TableRef.GetColumnReference(TIMESTAMPSTARTCOLUMNNAME));
  end;

  procedure AddConsecutiveTimeJoin(Query: TBoldSQLQuery; firstTableRef, nextTableRef: TBoldSQLTableReference);
  var
    stopColumnRef: TBoldSQLColumnReference;
    startColumnRef: TBoldSQLColumnReference;
  begin
    startColumnRef := nextTableRef.GetColumnReference(TIMESTAMPSTARTCOLUMNNAME);
    stopColumnRef := firstTableRef.GetColumnReference(TIMESTAMPSTOPCOLUMNNAME);
    Query.AddWCF(TBoldSQLWCFBinaryInfix.Create(TBoldSQLWCFColumnRef.Create(startColumnRef),
                                               TBoldSQLWCFBinaryInfix.Create(TBoldSQLWCFColumnRef.Create(stopColumnRef),
                                                                             TBoldSQLWCFInteger.Create(1), '+'),
                                               '='));
  end;

var
  i: Integer;
  anSQLQuery: TBoldSQLQuery;
  subQuery: TBoldSQLQuery;
  rootTableRef: TBoldSQLTableReference;
  objTableRef: TBoldSQLTableReference;
  obj2TableRef: TBoldSQLTableReference;
  MappingInfos: TBoldMemberMappingArray;
begin
  MappingInfos := SystemPersistenceMapper.MappingInfo.GetMemberMappings(ObjectPersistenceMapper.ExpressionName, ExpressionName);
  for i := 0 to length(MappingInfos) - 1 do
  begin
    anSQLQuery := MakeMainQuery(objTableRef, MappingInfos[i]);
    try
      subQuery := MakeSubQuery(anSQLQuery, objTableRef, obj2TableRef, MappingInfos[i]);
      rootTableRef := JoinRootTableInto(anSQLQuery, objTableRef);
      AddConsecutiveTimeJoin(subQuery, rootTableRef, obj2TableRef);
      anSQLQuery.AddColumnToRetrieve(rootTableRef.GetColumnReference(TIMESTAMPSTOPCOLUMNNAME));
      FetchIds(anSQLQuery, 1);
    finally
      anSQLQuery.Free;
    end;

    anSQLQuery := MakeMainQuery(objTableRef, MappingInfos[i]);
    try
      subQuery := MakeSubQuery(anSQLQuery, objTableRef, obj2tableRef, MappingInfos[i]);
      rootTableRef := JoinRootTableInto(subQuery, obj2TableRef);
      AddConsecutiveTimeJoin(subQuery, rootTableRef, objTableRef);
      anSQLQuery.AddColumnToRetrieve(objTableRef.GetColumnReference(TIMESTAMPSTARTCOLUMNNAME));
      FetchIds(anSQLQuery, 0);
    finally
      anSQLQuery.Free;
    end;
  end;
end;

function TBoldEmbeddedSingleLinkDefaultMapper.NewDefaultId(IdValue: integer; TimeStamp: TBoldTimestampType): TBoldObjectId;
var
  ObjectId: TBoldDefaultId;
begin
  if TimeStamp <> BoldMaxTimeStamp then
    ObjectId := TBoldTimestampedDefaultId.createWithTimeAndClassId(TimeStamp, fOtherEndObjectPMIndex, false)
  else
    ObjectId := TBoldDefaultId.CreateWithClassId(fOtherEndObjectPMIndex, false);

  ObjectId.AsInteger := IdValue;
  result := ObjectId;
end;

procedure TBoldEmbeddedSingleLinkDefaultMapper.InitializePSDescriptions;
var
  i, j: Integer;
  MemberMappings: TBoldMemberMappingArray;
  Columns: TStringList;
  BoldGuard: IBoldGuard;
begin
  inherited;
  BoldGuard := TBoldGuard.Create(Columns);
  {Add index}
  SetLength(MemberMappings, 0);

  if not IsInherited then
  begin
    Columns := TStringList.create;
    MemberMappings := SystemPersistenceMapper.MappingInfo.GetMemberMappings(ObjectPersistenceMapper.ExpressionName, ExpressionName);
    for i := 0 to length(MemberMappings) - 1 do
    begin
      Columns.CommaText := MemberMappings[i].Columns;
      for j := 0 to Columns.Count - 1 do
        SystemPersistenceMapper.EnsureIndex(MemberMappings[i].TableName,
          Columns[j], False, False, ObjectPersistenceMapper.Versioned);
    end;
  end;
end;

function TBoldNonEmbeddedLinkDefaultMapper.EmbeddingMapper: TBoldEmbeddedSingleLinkDefaultMapper;
var
  OtherEndObjectMapper: TBoldObjectPersistenceMapper;
  OtherEndMemberMapper: TBoldMemberPersistenceMapper;
begin
  OtherEndObjectMapper := ObjectPersistenceMappers[ClosestOtherEndObjectMapperIndex];
  OtherEndMemberMapper := OtherEndObjectMapper.MemberPersistenceMappers[ClosestOtherEndMemberMapperIndex];
  if OtherEndMemberMapper is TBoldEmbeddedSingleLinkDefaultMapper then
    result := OtherEndMemberMapper as TBoldEmbeddedSingleLinkDefaultMapper
  else
    result := nil;
end;

function TBoldEmbeddedSingleLinkDefaultMapper.GetMainColumnName: String;
begin
  result := ColumnDescriptions[0].SQLName;
end;

function TBoldEmbeddedSingleLinkDefaultMapper.GetOrderColumnName: String;
begin
  result := ColumnDescriptions[1].SQLName;
end;

function TBoldNonEmbeddedLinkDefaultMapper.GetLinkClassTableName: string;
begin
  result := LinkClassObjectMapper.MainTable.SQLName;
end;

function TBoldNonEmbeddedLinkDefaultMapper.GetClosestColumnName: string;
begin
  result := EmbeddingMapper.MainColumnName;
end;

function TBoldEmbeddedSingleLinkDefaultMapper.GetOtherEndObjectMapper: TBoldObjectDefaultMapper;
begin
  result := ObjectPersistenceMappers[OtherEndObjectPMIndex];
end;

function TBoldNonEmbeddedLinkDefaultMapper.GetOtherEndObjectMapper: TBoldObjectDefaultMapper;
begin
  result := ObjectPersistenceMappers[RemoteOtherEndObjectMapperIndex];
end;

{ TBoldLinkDefaultMapper }

function TBoldLinkDefaultMapper.GetObjectPersistenceMappers(
  Index: Integer): TBoldObjectDefaultMapper;
begin
  result := SystemPersistenceMapper.ObjectPersistenceMappers[Index] as TBoldObjectDefaultMapper;
end;

function TBoldNonEmbeddedLinkDefaultMapper.GetRemoteInnerLinkMapper: TBoldEmbeddedSingleLinkDefaultMapper;
var
  ObjectMapper: TBoldObjectPersistenceMapper;
begin
  if IsIndirect then
  begin
    ObjectMapper := ObjectPersistenceMappers[ClosestOtherEndObjectMapperIndex];
    result := ObjectMapper.MemberPersistenceMappers[ObjectMapper.MemberMapperIndexByMemberIndex[fRemoteInnerLinkMemberIndex]] as TBoldEmbeddedSingleLinkDefaultMapper
  end
  else
    result := nil;
end;

function TBoldNonEmbeddedLinkDefaultMapper.GetClosestOtherEndObjectMapper: TBoldObjectDefaultMapper;
begin
  result := ObjectPersistenceMappers[ClosestOtherEndObjectMapperIndex];
end;

function TBoldNonEmbeddedLinkDefaultMapper.GetClosestOtherEndMemberMapperIndex: Integer;
begin
  result := ClosestOtherEndObjectMapper.MemberMapperIndexByMemberIndex[fClosestOtherEndMemberIndex];
end;

function TBoldEmbeddedSingleLinkDefaultMapper.GetOtherEndMemberPMIndex: Integer;
begin
  result := OtherEndObjectMapper.MemberMapperIndexByMemberIndex[fOtherEndMemberIndex];
end;

function TBoldEmbeddedSingleLinkDefaultMapper.DefaultDefaultDbValue: String;
begin
  result := SystemPersistenceMapper.SQLDataBaseConfig.CorrectlyQuotedDefaultValue('-1');
end;

function TBoldNonEmbeddedLinkDefaultMapper.GetSupportsPolymorphicFetch: Boolean;
begin
  result := true;
end;

initialization
  with BoldMemberPersistenceMappers do
  begin
    AddDescriptor(TBoldNonEmbeddedLinkDefaultMapper, alAbstract);
    AddDescriptor(TBoldMultiLinkDefaultMapper, alAbstract);
    AddDescriptor(TBoldEmbeddedSingleLinkDefaultMapper, alConcrete);
    AddDescriptor(TBoldDirectSingleLinkDefaultmapper, alConcrete);
    AddDescriptor(TBoldIndirectSingleLinkDefaultmapper, alConcrete);
    AddDescriptor(TBoldDirectMultiLinkDefaultmapper, alConcrete);
    AddDescriptor(TBoldIndirectMultiLinkDefaultmapper, alConcrete);
  end;

   {end - initialization}

finalization
  if BoldMemberPersistenceMappersAssigned then
    with BoldMemberPersistenceMappers do
    begin
      RemoveDescriptorByClass(TBoldNonEmbeddedLinkDefaultMapper);
      RemoveDescriptorByClass(TBoldMultiLinkDefaultMapper);
      RemoveDescriptorByClass(TBoldEmbeddedSingleLinkDefaultMapper);
      RemoveDescriptorByClass(TBoldIndirectSingleLinkDefaultmapper);
      RemoveDescriptorByClass(TBoldDirectMultiLinkDefaultmapper);
      RemoveDescriptorByClass(TBoldIndirectMultiLinkDefaultmapper);
    end;
  {end - finalization}

end.


