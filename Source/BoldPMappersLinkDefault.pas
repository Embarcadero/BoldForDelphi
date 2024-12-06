{ Global compiler directives }
{$include bold.inc}
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
    function CompareField(const ObjectContent: IBoldObjectContents; const Field: IBoldField; ColumnIndex: integer; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList): Boolean; override;
    procedure InitializePSDescriptions; override;
    function GetOtherEndObjectMapper: TBoldObjectDefaultMapper; override;
    function DefaultDefaultDbValue: String; override;
  public
    class function CanStore(const ContentName: string): Boolean; override;
    procedure ValueToParam(const ObjectContent: IBoldObjectContents; const Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList); override;
    procedure ValueFromField(OwningObjectId: TBoldObjectId; const ObjectContent: IBoldObjectContents; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; const Field: IBoldField; ColumnIndex: Integer); override;
    function ValueAsVariant(const ObjectContent: IBoldObjectContents; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList): variant; override;
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
    procedure ProcessSQL(const Query: IBoldQuery; WhereFragment: String; resultList: TList; TimeStamp: TBoldTimeStampType);
    procedure ProcessResult(ResultList: TList; const ValueSpace: IBoldValueSpace; ObjectIdList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList; timeStamp: TBoldTimeStampType; FetchMode: integer; FailureList: TBoldObjectIdList);
    function GetLinkClassTableName: string;
    function GetLinkClassObjectMapper: TBoldObjectDefaultMapper; virtual; abstract;
    function GetClosestColumnName: string;
    function GetRemoteInnerLinkMapper: TBoldEmbeddedSingleLinkDefaultMapper;
    function GetClosestOtherEndObjectMapper: TBoldObjectDefaultMapper;
  protected
    function GetIsOrdered: Boolean; virtual; abstract;
    procedure GetChangePoints(ObjectIDList: TBoldObjectIdList; Condition: TBoldChangePointCondition; NameSpace: TBoldSqlnameSpace); override;
    function GetColumnCount: Integer; override;
    procedure CompareValuesToLists(OwningId: TBoldObjectId; const MemberInterface: IBoldValue; ListOfClosestEnd: TBoldObjectIdlist; ListOfRemoteEnd: TBoldObjectIdlist; FailureList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList); virtual; abstract;
    procedure StuffValuesFromLists(const MemberInterface: IBoldValue; ListOfClosestEnd: TBoldObjectIdlist; ListOfRemoteEnd: TBoldObjectIdlist); virtual; abstract;
    function CompareField(const ObjectContent: IBoldObjectContents; const Field: IBoldField; ColumnIndex: integer; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList): Boolean; override;
    function EmbeddingMapper: TBoldEmbeddedSingleLinkDefaultMapper;
    function GetOtherEndObjectMapper: TBoldObjectDefaultMapper; override;
    function GetSupportsPolymorphicFetch: Boolean; override;
  public
    property ClosestColumnName: string read GetClosestColumnName;
    property RemoteInnerLinkMapper: TBoldEmbeddedSingleLinkDefaultMapper read GetRemoteInnerLinkMapper;
    property ClosestOtherEndObjectMapper: TBoldObjectDefaultMapper read GetClosestOtherEndObjectMapper;
    property IsIndirect: Boolean read fIsIndirect;
    property LinkClassTablename: string read GetLinkClassTableName;
    property ClosestOtherEndObjectMapperIndex: Integer read fClosestOtherEndObjectMapperIndex;
    property ClosestOtherEndMemberMapperIndex: Integer read GetClosestOtherEndMemberMapperIndex;
    property RemoteOtherEndObjectMapperIndex: Integer read fRemoteOtherEndObjectMapperIndex;
    property LinkClassObjectMapper: TBoldObjectDefaultMapper read GetLinkClassObjectMapper;
    property Ordered: Boolean read GetIsOrdered;
    procedure PMFetch(ObjectIDList: TBoldObjectIdList; const ValueSpace: IBoldValueSpace; FetchMode: Integer; TranslationList: TBoldIdTranslationList; FailureList: TBoldObjectIdList); override;
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
    procedure StuffValuesFromLists(const MemberInterface: IBoldValue; ListOfClosestEnd: TBoldObjectIdlist; ListOfRemoteEnd: TBoldObjectIdlist); override;
    procedure CompareValuesToLists(OwningId: TBoldObjectId; const MemberInterface: IBoldValue; ListOfClosestEnd: TBoldObjectIdlist; ListOfRemoteEnd: TBoldObjectIdlist; FailureList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList); override;
    function GetLinkClassObjectMapper: TBoldObjectDefaultMapper; override;
   public
    constructor CreateFromMold(moldMember: TMoldMember; moldClass: TMoldClass; Owner: TBoldObjectPersistenceMapper; const MemberIndex: Integer; TypeNameDictionary: TBoldTypeNameDictionary); override;
    class function CanStore(const ContentName: string): Boolean; override;
  end;

  { TBoldIndirectSingleLinkDefaultmapper }
  TBoldIndirectSingleLinkDefaultmapper = class(TBoldSingleLinkDefaultMapper)
  protected
    procedure StuffValuesFromLists(const MemberInterface: IBoldValue; ListOfClosestEnd: TBoldObjectIdlist; ListOfRemoteEnd: TBoldObjectIdlist); override;
    procedure CompareValuesToLists(OwningId: TBoldObjectId; const MemberInterface: IBoldValue; ListOfClosestEnd: TBoldObjectIdlist; ListOfRemoteEnd: TBoldObjectIdlist; FailureList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList); override;
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
    procedure StuffValuesFromLists(const MemberInterface: IBoldValue; ListOfClosestEnd: TBoldObjectIdlist; ListOfRemoteEnd: TBoldObjectIdlist); override;
    procedure CompareValuesToLists(OwningId: TBoldObjectId; const MemberInterface: IBoldValue; ListOfClosestEnd: TBoldObjectIdlist; ListOfRemoteEnd: TBoldObjectIdlist; FailureList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList); override;
    function GetLinkClassObjectMapper: TBoldObjectDefaultMapper; override;
  public
    constructor CreateFromMold(moldMember: TMoldMember; moldClass: TMoldClass; Owner: TBoldObjectPersistenceMapper; const MemberIndex: Integer; TypeNameDictionary: TBoldTypeNameDictionary); override;
    class function CanStore(const ContentName: string): Boolean; override;
  end;

  { TBoldIndirectMultiLinkDefaultmapper }
  TBoldIndirectMultiLinkDefaultmapper = class(TBoldMultiLinkDefaultMapper)
  protected
    procedure StuffValuesFromLists(const MemberInterface: IBoldValue; ListOfClosestEnd: TBoldObjectIdlist; ListOfRemoteEnd: TBoldObjectIdlist); override;
    procedure CompareValuesToLists(OwningId: TBoldObjectId; const MemberInterface: IBoldValue; ListOfClosestEnd: TBoldObjectIdlist; ListOfRemoteEnd: TBoldObjectIdlist; FailureList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList); override;
    function GetLinkClassObjectMapper: TBoldObjectDefaultMapper; override;
  public
    constructor CreateFromMold(moldMember: TMoldMember; moldClass: TMoldClass; Owner: TBoldObjectPersistenceMapper; const MemberIndex: Integer; TypeNameDictionary: TBoldTypeNameDictionary); override;
    class function CanStore(const ContentName: string): Boolean; override;
  end;

  const
    MaxUnion = 30;

implementation

uses
  SysUtils,

  BoldCoreConsts,
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
  {$IFDEF RIL}
  {$IFNDEF BOLD_UNICODE}
  StringBuilder,
  {$ENDIF}
  {$ENDIF}
  BoldDefaultStreamNames;

{ Supporting functions/procedures }

{Returns new ObjectId with owned ClassId, both must be freed}
function CreateAndEnsureId(ObjectId: Integer; ClassId: Integer; Exact: Boolean; TransLationlist: TBoldIdTranslationList; const ValueSpace: IBoldValueSpace; TimeStamp: TBoldTimeStampType): TBoldDefaultId;
begin
  Assert (ClassID <> -1);
  if TimeStamp = BOLDMAXTIMESTAMP then
    Result := TBoldDefaultId.CreateWithClassId(ClassId, Exact)
  else
    result := TBoldTimestampedDefaultId.CreateWithTimeAndClassId(TimeStamp, ClassId, Exact);

  Result.AsInteger := ObjectId;
  if assigned(TranslationList) then
    TranslationList.AddTranslation(nil, Result);
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
var
  Tl1, Tl2: TTempLinkValues;
begin
  Tl1 := TTempLinkValues(Item1);
  Tl2 := TTempLinkValues(Item2);
  result := Tl1.Objectid - Tl2.Objectid;
  if result = 0 then
    result := Tl1.OrderValue - Tl2.OrderValue;
  if result = 0 then
    result := Tl1.RemoteId - Tl2.RemoteId;
  if result = 0 then
    result := Tl1.ClosestId - Tl2.ClosestId;
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

function TBoldNonEmbeddedLinkDefaultMapper.CompareField(const ObjectContent: IBoldObjectContents; const Field: IBoldField; ColumnIndex: integer; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList): Boolean;
begin
  raise EBold.CreateFmt(sCannotCallOnTransientClass, [classname]);
end;

function TBoldNonEmbeddedLinkDefaultMapper.GetColumnCount: Integer;
begin
  Result := 0;
end;

procedure TBoldNonEmbeddedLinkDefaultMapper.ProcessSQL(const Query: IBoldQuery; WhereFragment: String; resultList: TList; TimeStamp: TBoldTimeStampType);
{$IFDEF RIL}
var
  Cnt: integer;

  i, j: integer;
  ClassIdRequired: Boolean;
  NextColumnIndex: integer;
  LinkData: TTempLinkValues;
  MappingInfo: TBoldMemberMappingArray;
  AIMappingInfo: TBoldAllInstancesMappingArray; // of TBoldAllInstancesMappingInfo
  EmbeddingColumnName,
  EmbeddingOrderColumnName: String;
  WhereClause,
  SelectClause: String;
  Selectlist: TStringList;
  SQL: TStringList;
  OperatingOnRootTable: Boolean;
  RootTableJoin,
  sMappingInfoTableName: string;
  First: Boolean;
const
  LinkTableAlias:     String = 'LinkTable_Alias';
  LinkTableAlias_Dot: String = 'LinkTable_Alias.';
  RootTableAlias:     String = 'RootTable_Alias';
  procedure ExecQuery;
  var
    tmpQuery: IBoldQuery;
  begin
    tmpQuery := Query;
    tmpQuery.AssignSQL(SQL);
    SQL.Clear;
    tmpQuery.Open;
    if tmpQuery.RecordCount > resultList.Capacity then
      resultList.Capacity := tmpQuery.RecordCount;
    while not tmpQuery.Eof do
    begin
      LinkData := TTempLinkValues.Create;
      LinkData.ClosestId :=      tmpQuery.Fields[0].AsInteger;
      LinkData.ClosestClassid := tmpQuery.Fields[1].AsInteger;
      LinkData.ObjectId :=       tmpQuery.Fields[2].AsInteger;
      NextColumnIndex := 3;
      if ordered then
      begin
        LinkData.Ordervalue :=   tmpQuery.Fields[NextColumnIndex].AsInteger;
        INC(NextColumnIndex);
      end
      else
        LinkData.OrderValue := 0;

      if isIndirect then
        LinkData.RemoteId :=     tmpQuery.Fields[NextColumnIndex].AsInteger;
      ResultList.Add(LinkData);
      tmpQuery.Next;
    end;
    tmpQuery.Close;
  end;
var
  SB: TStringBuilder;


begin
  AIMappingInfo := nil;
  SelectList := TStringList.Create;
  sql := TStringList.Create;
  SB := TStringBuilder.Create;
  first := true;
  MappingInfo := SystemPersistenceMapper.MappingInfo.GetMemberMappings(ClosestOtherEndObjectMapper.ExpressionName, EmbeddingMapper.ExpressionName);
  try
    if length(MappingInfo)>1 then
      BoldPMLogFmt('Fetching accross %4d tables for %s', [length(MappingInfo), EmbeddingMapper.ExpressionName]);
    for i := 0 to length(MappingInfo) - 1 do
    begin
      if not first then
        SQL.Append(' UNION ');
      sMappingInfoTableName := MappingInfo[i].TableName;

      EmbeddingColumnName := MappingInfo[i].ColumnByIndex[0];
      if Ordered then
      begin
        if ORDERCOLUMN_INDEX >= MappingInfo[i].ColumnCount then
          raise EBoldBadColumnIndex.CreateFmt('%s.: Order column not found for association %s', [ClassName, EmbeddingColumnName]);
        EmbeddingOrderColumnName := MappingInfo[i].ColumnByIndex[ORDERCOLUMN_INDEX];
      end
      else
        EmbeddingOrderColumnName := '';

      SelectList.Clear;
      {ID, TYPE, ClosstId, [OderColumn], [RemoteColumn]}

      //SelectList.Add(LinkTableAlias + '.' + IDCOLUMN_NAME);
      SB.Clear;
        SB.Append(LinkTableAlias_Dot);
        SB.Append(IDCOLUMN_NAME);
      SelectList.Append(SB.ToString);
      //SelectList.Add(LinkTableAlias + '.' + TYPECOLUMN_NAME);
      SB.Clear;
        SB.Append(LinkTableAlias_Dot);
        SB.Append(TYPECOLUMN_NAME);
      SelectList.Append(SB.ToString);
      //SelectList.Append(LinkTableAlias + '.' + EmbeddingColumnName);
      SB.Clear;
        SB.Append(LinkTableAlias_Dot);
        SB.Append(EmbeddingColumnName);
      SelectList.Append(SB.ToString);

      if ordered then
        SelectList.Append(EmbeddingOrderColumnName);
      if IsIndirect then
        SelectList.Append(RemoteInnerLinkMapper.MainColumnName);

      //SelectClause := Format('SELECT %s', [BoldSeparateStringList(SelectList, ', ', '', '')]);
      SelectClause := 'SELECT ' + BoldSeparateStringList(SelectList,', ','','');

      //WhereClause := Format('WHERE (%s.%s) %s', [LinkTableAlias, EmbeddingColumnName, WhereFragment]);
        SB.Clear;
        SB.Append('WHERE (');
        SB.Append(LinkTableAlias_Dot);
        SB.Append(EmbeddingColumnName);
        SB.Append(') ');
        SB.Append(WhereFragment);
        WhereClause := SB.ToString;

      SQL.Append(SelectClause);
        //SQL.Add('FROM '+ MappingInfo[i].TableName + ' ' + LinkTableAlias);
        SB.Clear;
        SB.Append('FROM ');
        SB.Append(sMappingInfoTableName);
        SB.Append(' ');
        SB.Append(LinkTableAlias);
      SQL.Append(SB.ToString);

      RootTableJoin := ''; { build this string conditionally, only if needed !! //ril }
      {
      RootTableJoin := format('((%s.%s = %s.%s) and (%s.%s = %s.%s))', [
            LinkTableAlias, TIMESTAMPSTARTCOLUMNNAME,
            RootTableAlias, TIMESTAMPSTARTCOLUMNNAME,
            LinkTableAlias, IDCOLUMN_NAME,
            RootTableAlias, IDCOLUMN_NAME]);
      }

      if ClosestOtherEndObjectMapper.Versioned and
         not SameText(sMappingInfoTableName, SystemPersistenceMapper.RootClassObjectPersistenceMapper.Maintable.SQLName) {=OperatingOnRootTable} then
      begin
        if SystemPersistenceMapper.SQLDataBaseConfig.UseSQL92Joins then
        begin
          SB.Clear;
            SB.Append('((');
            SB.Append(LinkTableAlias_Dot);
            SB.Append(TIMESTAMPSTARTCOLUMNNAME);
            SB.Append(' = ');
            SB.Append(RootTableAlias);
            SB.Append('.');
            SB.Append(TIMESTAMPSTARTCOLUMNNAME);
            SB.Append(') and (');
            SB.Append(LinkTableAlias_Dot);
            SB.Append(IDCOLUMN_NAME);
            SB.Append(' = ');
            SB.Append(RootTableAlias);
            SB.Append('.');
            SB.Append(IDCOLUMN_NAME);
            SB.Append('))');
          RootTableJoin := SB.ToString;
          {
          SQL.append(format(' left join %s %s on %s', [
             SystemPersistenceMapper.RootClassObjectPersistenceMapper.MainTable.SQLName, RootTableAlias, RootTableJoin] ))
          }

          SB.Clear;
            SB.Append(' left join ');
            SB.Append(SystemPersistenceMapper.RootClassObjectPersistenceMapper.MainTable.SQLName);
            SB.Append(' ');
            SB.Append(RootTableAlias);
            SB.Append(' on ');
            SB.Append(RootTableJoin);
          SQL.Append(SB.ToString);
        end
        else
        begin
          {SQL.Append(format(', %s %s', [SystemPersistenceMapper.RootClassObjectPersistenceMapper.MainTable.SQLName, RootTableAlias] )); }
           SQL.Append(', '+SystemPersistenceMapper.RootClassObjectPersistenceMapper.MainTable.SQLName+' '+RootTableAlias);
        end;
      end;

      SQL.Append(WhereClause);

      ClassIDRequired := true;
      AIMappingInfo := SystemPersistenceMapper.MappingInfo.GetAllInstancesMapping(ClosestOtherEndObjectMapper.ExpressionName);
      { 1. Cheapest check first.
        2. Break when false as this won't change anymore... }
      Cnt := Length(AIMappingInfo);
      for j := 0 to Cnt-1 do
      begin
        if not AIMappingInfo[j].ClassIdRequired and
           SameText(AIMappingInfo[j].TableName, sMappingInfoTableName) then
        begin
          ClassIdRequired := False;
          Break;
        end;
      end;

      if ClassIdRequired then
      begin
        {ril}//SQL.Add(format('AND (%s in (%s))', [TYPECOLUMN_NAME, ClosestOtherEndObjectMapper.SubClassesID]));
        //SQL.Append('AND ('+TYPECOLUMN_NAME+' in ('+ClosestOtherEndObjectMapper.SubClassesID+'))');
        SB.Clear;
          SB.Append('AND (');
          SB.Append(TYPECOLUMN_NAME);
          SB.Append(' in (');
          SB.Append(ClosestOtherEndObjectMapper.SubClassesID);
          SB.Append('))');
        SQL.Append(SB.ToString);
      end;
      
      if ClosestOtherEndObjectMapper.Versioned then
      begin
        OperatingOnRootTable := SameText(sMappingInfoTableName, SystemPersistenceMapper.RootClassObjectPersistenceMapper.Maintable.SQLName);
        if OperatingOnRootTable then
          ClosestOtherEndObjectMapper.RetrieveTimeStampCondition(SQL, TimeStamp, false, 'AND', True, LinkTableAlias, LinkTableAlias)
        else
          ClosestOtherEndObjectMapper.RetrieveTimeStampCondition(SQL, TimeStamp, false, 'AND', True, LinkTableAlias, RootTableAlias);
        if not OperatingOnRootTable and not SystemPersistenceMapper.SQLDataBaseConfig.UseSQL92Joins then
        begin
          if RootTableJoin='' then { not prepared yet }
          begin
            SB.Clear;
              SB.Append('((');
              SB.Append(LinkTableAlias_Dot);
              SB.Append(TIMESTAMPSTARTCOLUMNNAME);
              SB.Append(' = ');
              SB.Append(RootTableAlias);
              SB.Append('.');
              SB.Append(TIMESTAMPSTARTCOLUMNNAME);
              SB.Append(') and (');
              SB.Append(LinkTableAlias_Dot);
              SB.Append(IDCOLUMN_NAME);
              SB.Append(' = ');
              SB.Append(RootTableAlias);
              SB.Append('.');
              SB.Append(IDCOLUMN_NAME);
              SB.Append('))');
            RootTableJoin := SB.ToString;
          end;
          SQL.Append('and '+RootTableJoin);
        end;
      end;
      if ((i+1) mod MaxUnion) = 0 then
    begin
      ExecQuery;
      first := true;
    end
    else
      first := false;
     end;
    if Sql.Count > 0 then
      ExecQuery;
   finally
    SelectList.Free;
    SQL.free;
    SB.Free;
  end;
{$ELSE}
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
        EmbeddingOrderColumnName := MappingInfo[i].ColumnByIndex[ORDERCOLUMN_INDEX]
      else
        EmbeddingOrderColumnName := '';

      SelectList.Clear;
      {ID, TYPE, ClosstId, [OderColumn], [RemoteColumn]}
      SelectList.Add(LinkTableAlias + '.' + IDCOLUMN_NAME);
      SelectList.Add(LinkTableAlias + '.' + TYPECOLUMN_NAME);
      SelectList.Append(LinkTableAlias + '.' + EmbeddingColumnName);
      if ordered then
        SelectList.Append(EmbeddingOrderColumnName);
      if IsIndirect then
        SelectList.Append(Format('%s', [RemoteInnerLinkMapper.MainColumnName]));
      SelectClause := Format('SELECT %s', [BoldSeparateStringList(SelectList, ', ', '', '')]);

      WhereClause := Format('WHERE (%s.%s) %s', [LinkTableAlias, EmbeddingColumnName, WhereFragment]);

      SQL.Clear;
      SQL.Add(SelectClause);
      SQL.Add('FROM '+ MappingInfo[i].TableName + ' ' + LinkTableAlias);

      OperatingOnRootTable := SameText(MappingInfo[i].TableName, SystemPersistenceMapper.RootClassObjectPersistenceMapper.Maintable.SQLName);

      RootTableJoin := format('((%s.%s = %s.%s) and (%s.%s = %s.%s))', [
            LinkTableAlias, TIMESTAMPSTARTCOLUMNNAME,
            RootTableAlias, TIMESTAMPSTARTCOLUMNNAME,
            LinkTableAlias, IDCOLUMN_NAME,
            RootTableAlias, IDCOLUMN_NAME]);

      if ClosestOtherEndObjectMapper.Versioned and not OperatingOnRootTable then
      begin
        if SystemPersistenceMapper.SQLDataBaseConfig.UseSQL92Joins then
          SQL.append(format(' left join %s %s on %s', [
            SystemPersistenceMapper.RootClassObjectPersistenceMapper.MainTable.SQLName,
            RootTableAlias,
            RootTableJoin] ))
        else
          SQL.Append(format(', %s %s', [
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
        SQL.Add(format('AND (%s in (%s))', [TYPECOLUMN_NAME, ClosestOtherEndObjectMapper.SubClassesID]));
      if ClosestOtherEndObjectMapper.Versioned then
      begin
        if OperatingOnRootTable then
          ClosestOtherEndObjectMapper.RetrieveTimeStampCondition(SQL, TimeStamp, false, 'AND', True, LinkTableAlias, LinkTableAlias)
        else
          ClosestOtherEndObjectMapper.RetrieveTimeStampCondition(SQL, TimeStamp, false, 'AND', True, LinkTableAlias, RootTableAlias);
        if not OperatingOnRootTable and not SystemPersistenceMapper.SQLDataBaseConfig.UseSQL92Joins then
          SQL.Append(format('and %s', [RootTableJoin]));
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
{$ENDIF} 
end; { TBoldNonEmbeddedLinkDefaultMapper.ProcessSQL }


procedure TBoldNonEmbeddedLinkDefaultMapper.ProcessResult(ResultList: TList; const ValueSpace: IBoldValueSpace; ObjectIdList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList; TimeStamp: TBoldTimeStampType; FetchMode: integer; FailureList: TBoldObjectIdList);
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
    MemberInterface := ObjectContents.EnsureMemberAndGetValueByIndex(MemberId.MemberIndex, ContentName);
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
    if UnprocessedObjects.First.IsEqual[CurrentobjectId] then
      UnprocessedObjects.RemoveByIndex(0)
    else
    if UnprocessedObjects.Last.IsEqual[CurrentobjectId] then
      UnprocessedObjects.RemoveByIndex(UnprocessedObjects.Count-1)
    else
      UnprocessedObjects.RemoveByIndex(UnprocessedObjects.IndexByID[CurrentobjectId]);
  end;

  procedure EnsureAndAddToList(List: TBoldObjectIdList; ClassId: integer; Exact: Boolean; IdAsInteger: integer);
  var
    BoldId: TBoldDefaultId;
  begin
    BoldId := CreateAndEnsureId(IdAsInteger,
                                ClassId,
                                Exact,
                                TranslationList,
                                ValueSpace,
                                TimeStamp);
    List.AddAndAdopt(BoldId);
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
  BoldObjectId: TBoldObjectId;
  BoldGuard: IBoldGuard;
begin
  BoldGuard := TBoldGuard.Create(ListOfRemoteEnd, ListOfClosestEnd, UnprocessedObjects, MemberID);
  RemoteOtherEndExact := IsIndirect and
                         (not ObjectPersistenceMappers[RemoteOtherEndObjectMapperIndex].HasSubClasses);

  UnprocessedObjects := ObjectIDList.Clone;
  MemberID := TBoldMemberId.Create(MemberIndex);
  ListOfClosestEnd := TBoldObjectIdlist.Create;
  ListOfRemoteEnd := TBoldObjectIdlist.Create;
  CurrentId := -1;
  if resultList.Count > 0 then
  try
    ListOfClosestEnd.Capacity := resultList.Count;
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

    BoldObjectId := FindInListByIdAsInteger(UnprocessedObjects, CurrentId);
    ProcessResultForOneObject(BoldObjectId);
  except
    on e:EBoldDuplicateSingleLinkValueInDb do
    begin
      e.Message := 'BoldId: ' + IntToStr(CurrentId) + ' ' + e.Message;

      raise;
    end
    else
      raise;
  end;

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

procedure TBoldNonEmbeddedLinkDefaultMapper.PMFetch(ObjectIDList: TBoldObjectIdList; const ValueSpace: IBoldValueSpace; FetchMode: Integer; TranslationList: TBoldIdTranslationList; FailureList: TBoldObjectIdList);
var
  TimeStamp: TBoldTimeStampType;
  ResultList: TList;
  start, stop: integer;
  Block, ObjectCount, I, FetchBlockSize, unions: Integer;
  WhereFragment: String;
  TopSortedIndex: Integer;
  aQuery: IBoldQuery;
begin
  if ObjectIDList.Count = 0 then
    Exit;

  TopSortedIndex := ObjectPersistenceMapper.TopSortedIndex;

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
    Unions := MinIntValue([MaxUnion, length(SystemPersistenceMapper.MappingInfo.GetMemberMappings(ClosestOtherEndObjectMapper.ExpressionName, EmbeddingMapper.ExpressionName))]);
    FetchBlockSize := 1;
    if Unions > 0 then
      FetchBlockSize := SystemPersistenceMapper.SQLDataBaseConfig.FetchBlockSize div Unions;
    if FetchBlockSize = 0 then
      FetchBlockSize := 1;
    for Block := 0 to (ObjectCount div FetchBlockSize) do
    begin
      aQuery.ClearParams;
      Start := Block * FetchBlockSize;
      Stop := MinIntValue([Pred(Succ(Block) * FetchBlockSize), ObjectCount]);
      WhereFragment := ObjectPersistenceMapper.IdListSegmentToWhereFragment(ObjectIdList, Start, Stop, false, aQuery);
      ProcessSQL(aQuery, WhereFragment, resultList, TimeStamp);
    end;
    if ResultList.Count > 1 then
      ResultList.Sort(SortLinkValues);
    if BoldPMLogHandler<>nil then
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

function TBoldEmbeddedSingleLinkDefaultMapper.CompareField(const ObjectContent: IBoldObjectContents; const Field: IBoldField; ColumnIndex: integer; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList): Boolean;
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
    1: Result := InitialColumnRootName + ORDERCOLUMN_SUFFIX;
  else
    raise EBoldBadColumnIndex.CreateFmt(sIllegalColumnIndex, [classname, 'GetInitialColumnName', ColumnIndex]); // do not localize
  end;
end;

procedure TBoldEmbeddedSingleLinkDefaultMapper.ValueToParam(const ObjectContent: IBoldObjectContents; const Param: IBoldParameter; ColumnIndex: Integer; TranslationList: TBoldIdTranslationList);
var
  SingleLink: IBoldObjectIdRef;
  aBoldObjectID: TBoldObjectID;
begin
  SingleLink := GetEnsuredValue(ObjectContent) as IBoldObjectIdRef;
  case ColumnIndex of
    0:
      if assigned(SingleLink.Id) then
      begin
        aBoldObjectID := TranslationList.TranslateToNewID[SingleLink.Id];
        if aBoldObjectID is TBoldDefaultId then
          Param.AsInteger := TBoldDefaultId(aBoldObjectID).asInteger
        else
          Param.AsInteger := INTERNALNULLKEY;
      end
      else
        Param.AsInteger := INTERNALNULLKEY;
    1:
      Param.AsInteger := SingleLink.OrderNo;
  else
    raise EBoldBadColumnIndex.CreateFmt('%s.ValueToParam: Bad column index (%d)', [ClassName, ColumnIndex]);
  end;
end;

function TBoldEmbeddedSingleLinkDefaultMapper.ValueAsVariant(
  const ObjectContent: IBoldObjectContents; ColumnIndex: Integer;
  TranslationList: TBoldIdTranslationList): variant;
var
  SingleLink: IBoldObjectIdRef;
  aBoldObjectID: TBoldObjectID;
begin
  SingleLink := GetEnsuredValue(ObjectContent) as IBoldObjectIdRef;
  case ColumnIndex of
    0:
      if assigned(SingleLink.Id) then
      begin
        aBoldObjectID := TranslationList.TranslateToNewID[SingleLink.Id];
        if aBoldObjectID is TBoldDefaultId then
          result := TBoldDefaultId(aBoldObjectID).asInteger
        else
          result := INTERNALNULLKEY;
      end
      else
        result := INTERNALNULLKEY;
    1:
      result := SingleLink.OrderNo;
  else
    raise EBoldBadColumnIndex.CreateFmt(sIllegalColumnIndex, [ClassName, 'ValueAsVariant', ColumnIndex]); // do not localize
  end;
end;

procedure TBoldEmbeddedSingleLinkDefaultMapper.ValueFromField(OwningObjectId: TBoldObjectId; const ObjectContent: IBoldObjectContents; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList; const Field: IBoldField; ColumnIndex: Integer);
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
        anIdRef.SetFromId(ObjectId, true);
      end
      else
       anIdRef.SetFromId(nil, false);
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
  fOtherEndMemberIndex := role.OtherEnd.Index;
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
  fClosestOtherEndMemberIndex := Role.OtherEnd.Index;
  fRemoteOtherEndObjectMapperIndex := fClosestOtherEndObjectMapperIndex;
  fRemoteInnerLinkMemberIndex := -1;
end;

function TBoldDirectSingleLinkDefaultmapper.GetLinkClassObjectMapper: TBoldObjectDefaultMapper;
begin
  result := nil;
end;

procedure TBoldDirectSingleLinkDefaultmapper.CompareValuesToLists(OwningId: TBoldObjectId; const MemberInterface: IBoldValue; ListOfClosestEnd: TBoldObjectIdlist; ListOfRemoteEnd: TBoldObjectIdlist; FailureList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList);
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
  const MemberInterface: IBoldValue; ListOfClosestEnd,
  ListOfRemoteEnd: TBoldObjectIdlist);
begin
   (MemberInterface as IBoldObjectIdRef).SetFromId(FirstIdInList(ListOfClosestEnd), false);
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
  fClosestOtherEndMemberIndex := Role.LinkRole.OtherEnd.Index;
  fRemoteOtherEndObjectMapperIndex := Role.OtherEnd.MoldClass.TopSortedIndex;
  fRemoteInnerLinkMemberIndex := Role.OtherEnd.LinkRole.OtherEnd.Index;
  if (LinkClass.TableMapping = tmChildren) then
    raise EBoldFeatureNotImplementedYet.CreateFmt(sChildMappedLinkClassesNotSupported,
                                                  [MoldClass.name, Role.Name, LinkClass.Name]);
end;

function TBoldIndirectSingleLinkDefaultmapper.GetLinkClassObjectMapper: TBoldObjectDefaultMapper;
begin
  result := ObjectPersistenceMappers[ClosestOtherEndObjectMapperIndex];
end;

procedure TBoldIndirectSingleLinkDefaultmapper.CompareValuesToLists(OwningId: TBoldObjectId; const MemberInterface: IBoldValue; ListOfClosestEnd: TBoldObjectIdlist; ListOfRemoteEnd: TBoldObjectIdlist; FailureList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList);
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
  const MemberInterface: IBoldValue; ListOfClosestEnd,
  ListOfRemoteEnd: TBoldObjectIdlist);
begin
  (MemberInterface as IBoldObjectIdRefPair).SetFromIds(FirstIdInList(ListOfClosestEnd), FirstIdInList(ListOfRemoteEnd));
end;

{ TBoldDirectMultiLinkDefaultmapper }

procedure TBoldDirectMultiLinkDefaultmapper.CompareValuesToLists(
  OwningId: TBoldObjectId; const MemberInterface: IBoldValue; ListOfClosestEnd,
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
  fClosestOtherEndMemberIndex := Role.OtherEnd.Index;
  fRemoteOtherEndObjectMapperIndex := fClosestOtherEndObjectMapperIndex;
  fRemoteInnerLinkMemberIndex := -1;
end;

function TBoldDirectMultiLinkDefaultmapper.GetLinkClassObjectMapper: TBoldObjectDefaultMapper;
begin
  result := nil;
end;

procedure TBoldDirectMultiLinkDefaultmapper.StuffValuesFromLists(
  const MemberInterface: IBoldValue; ListOfClosestEnd,
  ListOfRemoteEnd: TBoldObjectIdlist);
begin
  (MemberInterface as IBoldObjectIdListRef).SetFromIdList(ListOfClosestEnd);
end;

{ TBoldIndirectMultiLinkDefaultmapper }

procedure TBoldIndirectMultiLinkDefaultmapper.CompareValuesToLists(
  OwningId: TBoldObjectId; const MemberInterface: IBoldValue; ListOfClosestEnd,
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
  fClosestOtherEndMemberIndex := Role.LinkRole.OtherEnd.Index;
  fRemoteOtherEndObjectMapperIndex := Role.OtherEnd.MoldClass.TopSortedIndex;
  fRemoteInnerLinkMemberIndex := Role.OtherEnd.LinkRole.OtherEnd.Index;
  if (LinkClass.TableMapping = tmChildren) then
    raise EBoldFeatureNotImplementedYet.CreateFmt(sChildMappedLinkClassesNotSupported,
                                                  [MoldClass.name, Role.Name, LinkClass.Name]);
end;

function TBoldIndirectMultiLinkDefaultmapper.GetLinkClassObjectMapper: TBoldObjectDefaultMapper;
begin
  result := ObjectPersistenceMappers[ClosestOtherEndObjectMapperIndex];
end;

procedure TBoldIndirectMultiLinkDefaultmapper.StuffValuesFromLists(
  const MemberInterface: IBoldValue; ListOfClosestEnd,
  ListOfRemoteEnd: TBoldObjectIdlist);
begin
  (MemberInterface as IBoldObjectIdListRefPair).SetFromIdLists(ListOfClosestEnd, ListOfRemoteEnd);
end;

{ TBoldSingleLinkDefaultMapper }

function TBoldSingleLinkDefaultMapper.FirstIdInList(List: TBoldObjectIdList): TBoldObjectId;
begin
  if List.Count > 1 then
    raise EBoldDuplicateSingleLinkValueInDb.Create(ObjectPersistenceMapper.ExpressionName + '.' + ExpressionName + ';List count is ' + IntToStr(List.Count) + ', expected <= 1, possible reason: duplicate values in DB for a single link');
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
                                               'not'));
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
  CatenatedColumns: string;
  BoldGuard: IBoldGuard;
begin
  inherited;
  BoldGuard := TBoldGuard.Create(Columns);
  {Add index}
  SetLength(MemberMappings, 0);

  if not IsInherited then
  begin
    MemberMappings := SystemPersistenceMapper.MappingInfo.GetMemberMappings(ObjectPersistenceMapper.ExpressionName, ExpressionName);
    Columns := TStringList.create;      
    if SystemPersistenceMapper.SQLDataBaseConfig.SingleIndexOrderedLinks then
    begin
      for i := 0 to length(MemberMappings) - 1 do
      begin
        Columns.CommaText := MemberMappings[i].Columns;
        CatenatedColumns := Columns[0];
        for j := 1 to Columns.Count - 1 do
          CatenatedColumns := CatenatedColumns + ';'  + Columns[j];
        SystemPersistenceMapper.EnsureIndex(MemberMappings[i].TableName,
          CatenatedColumns, False, False, False, ObjectPersistenceMapper.Versioned);
      end;
    end
    else
    begin
      for i := 0 to length(MemberMappings) - 1 do
      begin
        Columns.CommaText := MemberMappings[i].Columns;
        for j := 0 to Columns.Count - 1 do
          SystemPersistenceMapper.EnsureIndex(MemberMappings[i].TableName,
            Columns[j], False, False, False, ObjectPersistenceMapper.Versioned);
      end;
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
  result := ColumnDescriptions[ORDERCOLUMN_INDEX].SQLName;
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
