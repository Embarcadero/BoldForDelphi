
{ Global compiler directives }
{$include bold.inc}
unit BoldExternalPersistenceControllerSQL;

interface

uses
  DB,
  Classes,
  {$IFDEF BOLD_DELPHI6_OR_LATER}
  Variants,
  {$ENDIF}
  BoldSubscription,
  BoldPersistenceController,
  BoldNameExpander,
  BoldId,
  BoldValueSpaceInterfaces,
  BoldValueInterfaces,
  BoldGuard,
  BoldTypeNameDictionary,
  BoldMeta,
  BoldAbstractPartiallyExternalPC,
  BoldExternalPersistenceSupport,
  BoldAbstractDatabaseAdapter,
  BoldDBInterfaces;

type
  TBoldQueryMode = (bqmSelect, bqmUpdate, bqmInsert, bqmDelete);

  TBoldExternalPersistenceControllerSQL = class(TBoldAbstractPartiallyExternalPC)
  private
    FDatabaseAdapter: TBoldAbstractDatabaseAdapter;
    FClassesToHandle: TStringList;
  protected
     { This method locates a record in a BoldTable based on the provided }
     { object contents }
     function LocateInDB(MoldClass: TMoldClass; ObjectContents: IBoldObjectContents;
       BoldQuery: IBoldQuery): Boolean;

    { This method assigns values to the parameters for an IBoldQuery }
    procedure AssignParametersWithMemberIdList(MoldClass: TMoldClass; ObjectContents: IBoldObjectContents; Query: IBoldParameterized; MemberIdList: TBoldMemberIdList);

    procedure AssignParameters(MoldClass: TMoldClass; ObjectContents: IBoldObjectContents; Query: IBoldParameterized);

{    procedure AssignParameters(MoldClass: TMoldClass; ObjectContents: IBoldObjectContents; BoldExecQuery: IBoldExecQuery; MemberIdList: TBoldMemberIdList); overload;

    procedure AssignParameters(MoldClass: TMoldClass; ObjectContents: IBoldObjectContents; BoldExecQuery: IBoldExecQuery); overload;}

    { This method populates MemberIdList with members that are both external }
    { and dirty }
    procedure GetExternalDirtyMembers(MoldClass: TMoldClass; ObjectContents: IBoldObjectContents; MemberIdList: TBoldMemberIdList);

    { This method, creates one record in the external database }
    procedure InternalCreateObject(MoldClass: TMoldClass; ObjectContents: IBoldObjectContents); virtual;

    { This method generates an Insert SQL for the members in MemberIdList }
    function GenerateInsertSQL(MoldClass: TMoldClass; ObjectContents: IBoldObjectContents; MemberIdList: TBoldMemberIdList): String; virtual;

    { This method updaets one database record with ObjectContents }
    procedure InternalUpdateObject(MoldClass: TMoldClass; ObjectContents: IBoldObjectContents); virtual;

    function GenerateUpdateSQL(MoldClass: TMoldClass; ObjectContents: IBoldObjectContents): String; virtual;

    { This method deletes one record from the external database based on }
    { object contents.  }
    procedure InternalDeleteObject(MoldClass: TMoldClass; ObjectContents: IBoldObjectContents); virtual;

    { This method generates a Delete SQL query }
    function GenerateDeleteSQL(MoldClass: TMoldClass; ObjectContents: IBoldObjectContents): String; virtual;


    procedure PrepareFetchExternal(ExternalKeys: TBoldObjectIdList; ValueSpace: IBoldValueSpace; MoldClass: TMoldClass; MemberIdList: TBoldMemberIdList; var FetchContext: TObject); override;

    procedure PostFetch(FetchContext: TObject; MoldClass: TMoldClass); override;

    procedure FetchObject(ObjectContents: IBoldObjectContents; MemberIdList: TBoldMemberIdList; FetchContext: TObject; ValueSpace: IBoldValueSpace); override;

    function InternalFetchObjects(MoldClass: TMoldClass): IBoldDataSet; virtual;

    function HandlesClass(MoldClass: TMoldClass): Boolean; override;
    function ExternalKeyExistsInExternalStorage(MoldClass: TMoldClass; ExternalKey: TBoldObjectId): Boolean; override;
    procedure AssignKeyToObject(MoldClass: TMoldClass; ObjectContents: IBoldObjectContents; ExternalKey: TBoldObjectId; ValueSpace: IBoldValueSpace); override;
    function ExternalKeysToInternalSQL(MoldClass: TMoldClass; ExternalKeys: TBoldObjectIdList): String; override;
    function ObjectContentsToInternalSQL(MoldClass: TMoldClass; ObjectContents: IBoldObjectContents): String; virtual;
    procedure GetExternalKeys(MoldClass: TMoldClass; ExternalKeys: TBoldObjectIdList); override;
    function GetExternalKeyFromObject(ObjectContents: IBoldObjectContents; ValueSpace: IBoldValueSpace): TBoldObjectId; override;
    procedure CreateObjects(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace); override;
    procedure DeleteObjects(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace); override;
    procedure UpdateObjects(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace); override;
    function GetMaxFetchBlockSize: integer; override;
    property DatabaseAdapter: TBoldAbstractDatabaseAdapter read FDatabaseAdapter;
  public
    constructor Create(MoldModel: TMoldModel; ADatabaseAdapter: TBoldAbstractDatabaseAdapter;
      TypeNameDictionary: TBoldTypeNameDictionary; OnStartUpdates, OnEndUpdates, OnFailUpdates: TNotifyEvent;
      AClassesToHandle: TStrings; AUpdateBoldDatabaseFirst: boolean); reintroduce;
    destructor Destroy; override;
    procedure SubscribeToPersistenceEvents(Subscriber: TBoldSubscriber; Events: TBoldSmallEventSet = []); override;
  end;

{------------------------------------------------------------------------------}
  TFetchObjectList = class;

  TAbstractFetchObject = class(TCollectionItem)
  private
    FMember: TMoldMember;
    FMemberIndex: Integer;
    function GetFetchObjectList: TFetchObjectList;
    function GetPersistenceController: TBoldExternalPersistenceControllerSQL;
    function GetMoldClass: TMoldClass;
    function GetValueSpace: IBoldValueSpace;
  protected
    procedure Fetch(Source: IBoldQuery; ObjectContents: IBoldObjectContents); dynamic; abstract;
    property FetchObjectList: TFetchObjectList read GetFetchObjectList;
    property PersistenceController: TBoldExternalPersistenceControllerSQL
      read GetPersistenceController;
  public
    constructor Create(ACollection: TCollection; AMemberIndex: Integer); reintroduce;
  published
    property ValueSpace: IBoldValueSpace read GetValueSpace;
    property MoldClass: TMoldClass read GetMoldClass;
    property Member: TMoldMember read FMember;
    property MemberIndex: Integer read FMemberIndex;
  end;
  TFetchObjectClass = class of TAbstractFetchObject;

  TMemberFetchObject = class(TAbstractFetchObject)
  protected
    procedure Fetch(Source: IBoldQuery; ObjectContents: IBoldObjectContents); override;
  end;

  TAbstractRoleFetchObject = class(TAbstractFetchObject)
  private
    SQL: String;
    function GetMoldRole: TMoldRole;
  protected
    function PrepareSQL: String; virtual;
    function ExecSQL(Source: IBoldQuery;
      ObjectContents: IBoldObjectContents): IBoldQuery; virtual;
  public
    property Role: TMoldRole read GetMoldRole;
  end;

  TSingleRoleFetchObject = class(TAbstractRoleFetchObject)
  protected
    procedure Fetch(Source: IBoldQuery; ObjectContents: IBoldObjectContents); override;
  end;

  TMultiRoleFetchObject = class(TAbstractRoleFetchObject)
  protected
    procedure Fetch(Source: IBoldQuery; ObjectContents: IBoldObjectContents); override;
  end;

  TFetchObjectList = class(TCollection)
  private
    FMoldClass: TMoldClass;
    FExternalKeys: TBoldObjectIdList;
    FValueSpace: IBoldValueSpace;
    FPersistenceController: TBoldExternalPersistenceControllerSQL;
    function GetItem(Index: Integer): TAbstractFetchObject;
    procedure SetItem(Index: Integer; Value: TAbstractFetchObject);
  protected
    function Add(Member: TMoldMember): TAbstractFetchObject;
  public
    constructor Create(APersistenceController: TBoldExternalPersistenceControllerSQL;
      AMoldClass: TMoldClass; AMemberIdList: TBoldMemberIdList;
      AExternalKeys: TBoldObjectIdList;
      AValueSpace: IBoldValueSpace); reintroduce;
    function IndexOf(MemberId: TBoldMemberId): Integer;
    function Find(MemberId: TBoldMemberId): TAbstractFetchObject;

    property Items[Index: Integer]: TAbstractFetchObject read GetItem
      write SetItem; default;

    property PersistenceController: TBoldExternalPersistenceControllerSQL
      read FPersistenceController;
    property MoldClass: TMoldClass read FMoldClass;
    property ExternalKeys: TBoldObjectIdList read FExternalKeys;
    property ValueSpace: IBoldValueSpace read FValueSpace;
  end;

  TFetchContext = class
  private
    FFetchObjectList: TFetchObjectList;
    Source: IBoldQuery;
  public
    constructor Create(PersistenceController: TBoldExternalPersistenceControllerSQL;
      ExternalKeys: TBoldObjectIdList; ValueSpace: IBoldValueSpace;
      MoldClass: TMoldClass; MemberIdList: TBoldMemberIdList);
    destructor Destroy; override;
    procedure FetchObject(ObjectContents: IBoldObjectContents);
    procedure PostFetch;
    property FetchObjectList: TFetchObjectList read FFetchObjectList;
  end;

implementation

uses
  Math,
  SysUtils,

  BoldCoreConsts,
  BoldUtils,
  BoldDefs,
  BoldTaggedValueSupport,
  BoldStringId,
  BoldDefaultId;

function _GetTableName(MoldClass: TMoldClass): String;
begin
  Result := BoldExpandName(MoldClass.ExternalTableName, MoldClass.name, xtSQL, -1, nccFalse);
end;

function _GetColumnName(BoldMember: TMoldMember): String;
begin
  Result := BoldExpandName(BoldMember.ColumnName, BoldMember.name, xtSQL, -1, nccFalse);
end;

function MemberIndexByName(MoldClass: TMoldClass; MemberName: String): Integer;
begin
  for Result := 0 to MoldClass.AllBoldMembers.Count-1 do
    if SameText(MoldClass.AllBoldMembers[Result].ExpandedExpressionName, MemberName) then
      exit;
  Result := -1;
end;

function FindExternalKey(MoldClass: TMoldClass): Integer;
begin
  for Result := 0 to MoldClass.AllBoldMembers.Count-1 do
    if MoldClass.AllBoldMembers[Result].Storage = bsExternalKey then
      exit;
  Result := -1;
end;

function RemovePreAt(Member: TMoldMember): String;
begin
  result := BoldExpandName(Member.ColumnName, Member.name, xtSQL, -1, Member.MoldClass.Model.NationalCharConversion);
  if (Length(result) > 0) and {(result[1] = '@')} CharInSet(result[1], ['@', '_']) then {!!}
    Result := Copy(result, 2, Length(result))
end;

function FindExternalKeyColumns(MoldClass: TMoldClass): String;
var
  i: integer;
begin
  Result := '';
  for i := 0 to MoldClass.AllBoldMembers.Count-1 do
    if MoldClass.AllBoldMembers[i].Storage = bsExternalKey then
      Result := Result + RemovePreAt(MoldClass.AllBoldMembers[i]) + ';';

  if Length(Result) > 0 then
    SetLength(Result, Length(Result)-1);
end;

function GetCharCount(C: Char; S: String): Integer;
var
  i: integer;
begin
  result := 0;
  for i := 1 to Length(S) do
    if S[i] = C then
      Inc(Result);
end;

function GetNextWord(var S: String; Splitter: Char): String;
begin
  if Pos(Splitter, S) > 0 then
  begin
    Result := Copy(S, 1, Pos(Splitter, S)-1);
    Delete(S, 1, Pos(Splitter, S));
  end
  else
  begin
    Result := S;
    S := '';
  end;
end;


function GetDataValue(DataSet: TDataSet; FieldNames: String): Variant;
var
  FieldCount: Integer;
  i: integer;
  S: String;
begin
  FieldCount := GetCharCount(';', FieldNames) + 1;
  if FieldCount = 1 then
    Result := DataSet.FieldByName(FieldNames).Value
  else if FieldCount > 0 then
  begin
    S := FieldNames;
    Result := VarArrayCreate([0, FieldCount-1], varVariant);
    for i := 0 to FieldCount-1 do
      Result[i] := DataSet.FieldByName(GetNextWord(S, ';')).Value;
  end;
end;

function VarArrayToStr(V: Variant): String;
var
  i: integer;
begin
  Result := '';
  for i := VarArrayLowBound(V, 1) to VarArrayHighBound(V, 1) do
    Result := Result + VarToStr(V[i]) + ';';
  if Length(Result) > 0 then
    SetLength(Result, Length(Result)-1);
end;

function NormalizeVariant(V: Variant): Variant;
begin
  if VarType(V) and varArray <> 0 then
    Result := VarArrayToStr(V)
  else
    Result := V;
end;

function BoldValueToVariant(B: IBoldValue): Variant;
var
  Nullable: IBoldNullableValue;
  S: IBoldStringContent;
  I: IBoldIntegerContent;
  F: IBoldFloatContent;
  C: IBoldCurrencyContent;
  Bool: IBoldBooleanContent;
  D: IBoldDateContent;
  T: IBoldTimeContent;
  DT: IBoldDateTimeContent;
  BL: IBoldBlobContent;
begin
  if (B.QueryInterface(IBoldNullableValue, Nullable) = S_OK) and
     (Nullable.IsNull) then
    Result := Null
  else if B.QueryInterface(IBoldStringContent, S) = S_OK then
    Result := S.asString
  else if B.QueryInterface(IBoldIntegerContent, I) = S_OK then
    Result := I.asInteger
  else if B.QueryInterface(IBoldFloatContent, F) = S_OK then
    Result := F.asFloat
  else if B.QueryInterface(IBoldCurrencyContent, C) = S_OK then
    Result := C.asCurrency
  else if B.QueryInterface(IBoldBooleanContent, Bool) = S_OK then
    Result := Bool.asBoolean
  else if B.QueryInterface(IBoldDateContent, D) = S_OK then
    Result := D.asDate
  else if B.QueryInterface(IBoldTimeContent, T) = S_OK then
    Result := T.asTime
  else if B.QueryInterface(IBoldDateTimeContent, DT) = S_OK then
    Result := DT.asDateTime
  else if B.QueryInterface(IBoldBlobContent, BL) = S_OK then
    Result := BL.asBlob
  else raise Exception.Create(sUnknownDataType);
end;

procedure VariantToBoldValue(B: IBoldValue; Value: Variant);
var
  Nullable: IBoldNullableValue;
  S: IBoldStringContent;
  I: IBoldIntegerContent;
  F: IBoldFloatContent;
  C: IBoldCurrencyContent;
  Bool: IBoldBooleanContent;
  D: IBoldDateContent;
  T: IBoldTimeContent;
  DT: IBoldDateTimeContent;
  BL: IBoldBlobContent;
begin
  if (B.QueryInterface(IBoldNullableValue, Nullable) = S_OK) and
     (VarIsNull(Value)) then
    Nullable.SetContentToNull
  else if B.QueryInterface(IBoldStringContent, S) = S_OK then
    S.asString := Value
  else if B.QueryInterface(IBoldIntegerContent, I) = S_OK then
    I.asInteger := Value
  else if B.QueryInterface(IBoldFloatContent, F) = S_OK then
    F.asFloat := Value
  else if B.QueryInterface(IBoldCurrencyContent, C) = S_OK then
    C.asCurrency := Value
  else if B.QueryInterface(IBoldBooleanContent, Bool) = S_OK then
    Bool.asBoolean := Value
  else if B.QueryInterface(IBoldDateContent, D) = S_OK then
    D.asDate := Value
  else if B.QueryInterface(IBoldTimeContent, T) = S_OK then
    T.asTime := Value
  else if B.QueryInterface(IBoldDateTimeContent, DT) = S_OK then
    DT.asDateTime := Value
  else if B.QueryInterface(IBoldBlobContent, BL) = S_OK then
    BL.asBlob := AnsiString(Value)
  else raise Exception.Create(sUnknownDataType);
end;

procedure SetBoldValueToNull(B: IBoldValue);
var
  Nullable: IBoldNullableValue;
  S: IBoldStringContent;
  I: IBoldIntegerContent;
  F: IBoldFloatContent;
  C: IBoldCurrencyContent;
  Bool: IBoldBooleanContent;
  D: IBoldDateContent;
  T: IBoldTimeContent;
  DT: IBoldDateTimeContent;
  BL: IBoldBlobContent;
begin
  // all the below types support IBoldNullableValue
  if B.QueryInterface(IBoldNullableValue, Nullable) = S_OK then
    Nullable.SetContentToNull
  else if B.QueryInterface(IBoldStringContent, S) = S_OK then
    S.asString := ''
  else if B.QueryInterface(IBoldIntegerContent, I) = S_OK then
    I.asInteger := 0
  else if B.QueryInterface(IBoldFloatContent, F) = S_OK then
    F.asFloat := 0
  else if B.QueryInterface(IBoldCurrencyContent, C) = S_OK then
    C.asCurrency := 0
  else if B.QueryInterface(IBoldBooleanContent, Bool) = S_OK then
    Bool.asBoolean := False
  else if B.QueryInterface(IBoldDateContent, D) = S_OK then
    D.asDate := 0
  else if B.QueryInterface(IBoldTimeContent, T) = S_OK then
    T.asTime := 0
  else if B.QueryInterface(IBoldDateTimeContent, DT) = S_OK then
    DT.asDateTime := 0
  else if B.QueryInterface(IBoldBlobContent, BL) = S_OK then
    BL.asBlob := ''
  else raise Exception.Create(sUnknownDataType);
end;

function GetKeyCount(MoldClass: TMoldClass): Integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to MoldClass.AllBoldMembers.Count-1 do
    if MoldClass.AllBoldMembers[i].Storage = bsExternalKey then
      Inc(Result);
end;

function GetObjectKeys(MoldClass: TMoldClass; ObjectContents: IBoldObjectContents): Variant;
var
  i: integer;
  KeyCount: Integer;
  CurrKey: Integer;
begin
  Result := Unassigned;

  KeyCount := GetKeyCount(MoldClass);
  if KeyCount = 0 then
    exit
  else if KeyCount > 1 then
    Result := VarArrayCreate([0, KeyCount-1], varVariant);

  CurrKey := 0;

  for i := 0 to MoldClass.AllBoldMembers.Count-1 do
    if MoldClass.AllBoldMembers[i].Storage = bsExternalKey then
      if KeyCount = 1 then
      begin
        Result := BoldValueToVariant(ObjectContents.ValueByIndex[i]);
        exit;
      end
      else
      begin
        Result[CurrKey] := BoldValueToVariant(ObjectContents.ValueByIndex[i]);
        Inc(CurrKey);
      end;
end;

procedure GetExternalKeyMembers(MoldClass: TMoldClass;
  MemberIdList: TBoldMemberIdList);
var
  i: integer;
begin
  for i := 0 to MoldClass.AllBoldMembers.Count-1 do
    if MoldClass.AllBoldMembers[i].Storage = bsExternalKey then
      MemberIdList.Add(TBoldMemberId.Create(i));
end;

function QuoteStringIfNeeded(const S: String): String;
var
  V: Double;
  C: Integer;
begin
  Val(S, V, C);
  if C <> 0 then
    Result := '''' + S + ''''
  else
    Result := S;
end;

{ TBoldExternalPersistenceControllerEventDriven }

constructor TBoldExternalPersistenceControllerSQL.Create(
  MoldModel: TMoldModel; ADatabaseAdapter: TBoldAbstractDatabaseAdapter;
  TypeNameDictionary: TBoldTypeNameDictionary;
  OnStartUpdates, OnEndUpdates, OnFailUpdates: TNotifyEvent; AClassesToHandle: TStrings; AUpdateBoldDatabaseFirst: boolean);
begin
  inherited Create(MoldModel, TypeNameDictionary, OnStartUpdates, OnEndUpdates, OnFailUpdates, AUpdateBoldDatabaseFirst);
  FDatabaseAdapter := ADatabaseAdapter;
  FClassesToHandle := TStringList.Create;
  FClassesToHandle.Assign(AClassesToHandle);
end;

destructor TBoldExternalPersistenceControllerSQL.Destroy;
begin
  FreeAndNil(FClassesToHandle);
  inherited;
end;

procedure TBoldExternalPersistenceControllerSQL.CreateObjects(
  ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace);
var
  i: integer;
  ObjectContents: IBoldObjectContents;
  MoldClass: TMoldClass;
begin
  for i := 0 to ObjectIdList.count-1 do
  begin
    MoldClass := MoldModel.Classes[ObjectidList[i].TopSortedIndex];
    ObjectContents := ValueSpace.ObjectContentsByObjectId[ObjectIdList[i]];
    InternalCreateObject(MoldClass, ObjectContents);
  end;
end;

procedure TBoldExternalPersistenceControllerSQL.DeleteObjects(
  ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace);
var
  i: integer;
  ObjectContents: IBoldObjectContents;
  MoldClass: TMoldClass;
begin
  for i := 0 to ObjectIdList.count-1 do
  begin
    ObjectContents := ValueSpace.ObjectContentsByObjectId[ObjectIdList[i]];
    MoldClass := MoldModel.Classes[ObjectidList[i].TopSortedIndex];
    InternalDeleteObject(MoldClass, ObjectContents);
  end;
end;

procedure TBoldExternalPersistenceControllerSQL.PrepareFetchExternal(
  ExternalKeys: TBoldObjectIdList; ValueSpace: IBoldValueSpace;
  MoldClass: TMoldClass; MemberIdList: TBoldMemberIdList; var FetchContext: TObject);
var
  FetchMembers: TBoldMemberIdList;
  i: integer;
  lMember: TMoldMember;
begin
  if Assigned(MemberIdList) and (MemberIdList.Count > 0) then
    FetchMembers := MemberIdList
  else
  begin
    FetchMembers := TBoldMemberIdList.Create;
    for i := 0 to MoldClass.AllBoldMembers.Count-1 do
    begin
      lMember := MoldClass.AllBoldMembers[i];
      if (lMember.Storage in [bsExternal, bsExternalKey]) and
         not lMember.Derived and
         not ((lMember is TMoldRole) and
              (TMoldRole(lMember).Multi)) and
         not (SameText(lMember.TVByName['DelayedFetch'], 'True')) then
        FetchMembers.Add(TBoldMemberId.Create(i));
    end;
  end;

  FetchContext := TFetchContext.Create(Self, ExternalKeys, ValueSpace, MoldClass,
    FetchMembers);
    
  if FetchMembers <> MemberIdList then
    FetchMembers.Free;
end;

procedure TBoldExternalPersistenceControllerSQL.PostFetch(
  FetchContext: TObject; MoldClass: TMoldClass);
begin
  if Assigned(FetchContext) then
  begin
    TFetchContext(FetchContext).PostFetch;
    FetchContext.Free;
  end;  
  inherited;  
end;

procedure TBoldExternalPersistenceControllerSQL.FetchObject(
  ObjectContents: IBoldObjectContents; MemberIdList: TBoldMemberIdList;
  FetchContext: TObject; ValueSpace: IBoldValueSpace);
begin
  TFetchContext(FetchContext).FetchObject(ObjectContents);
end;

procedure TBoldExternalPersistenceControllerSQL.GetExternalKeys(MoldClass: TMoldClass; ExternalKeys: TBoldObjectIdList);
var
  DBFieldName: String;
  ExternalID: TBoldObjectId;
  DBValue: Variant;
  BoldQuery: IBoldQuery;
begin
  DBFieldName := FindExternalKeyColumns(MoldClass);

  BoldQuery := DatabaseAdapter.DatabaseInterface.GetQuery;
  BoldQuery.AssignSQLText(Format('SELECT %S FROM %S', [
      StringReplace(DBFieldName, ';', ', ', [rfReplaceAll]), _GetTableName(MoldClass)]));
  BoldQuery.Open;

  while not BoldQuery.Eof do
  begin
    DBValue := NormalizeVariant(GetDataValue(BoldQuery.AsDataSet, DBFieldName));
    if VarType(DBValue) in [varInteger, varSmallint, varSingle, varDouble] then
    begin
      ExternalId := TBoldDefaultId.Create;
      TBoldDefaultId(ExternalId).AsInteger := DBValue;
    end
    else if VarType(DBValue) = varString then
    begin
      ExternalId := TBoldStringId.Create;
      TBoldStringId(ExternalId).AsString := DBValue;
    end
    else
      raise Exception.CreateFmt(sUnknownVarTypeLoadingObject, [MoldClass.name]);
    ExternalKeys.Add(ExternalId);
    ExternalId.Free;
    BoldQuery.Next;
  end;
  BoldQuery.Close;
end;

function TBoldExternalPersistenceControllerSQL.HandlesClass(MoldClass: TMoldClass): Boolean;
begin
  Result := FClassesToHandle.IndexOf(MoldClass.ExpandedExpressionName) <> -1;
end;

function TBoldExternalPersistenceControllerSQL.ExternalKeyExistsInExternalStorage(
  MoldClass: TMoldClass; ExternalKey: TBoldObjectId): Boolean;
var
  BoldQuery: IBoldQuery;
  DBFieldName: String;
  ExternalKeys: TBoldObjectIdList;
begin
  BoldQuery := DatabaseAdapter.DatabaseInterface.GetQuery;
  with BoldQuery do
  begin
    DBFieldName := FindExternalKeyColumns(MoldClass);

    ExternalKeys := TBoldObjectIdList.Create;
    try
      ExternalKeys.Add(ExternalKey);
      AssignSQLText(Format('SELECT %S FROM %S WHERE %S', [
        StringReplace(DBFieldName, ';', ', ', [rfReplaceAll]),
        _GetTableName(MoldClass), ExternalKeysToInternalSQL(MoldClass, ExternalKeys)]));
    finally
      ExternalKeys.Free;
    end;
    Open;
    Result := not AsDataSet.IsEmpty;
  end;
end;

procedure TBoldExternalPersistenceControllerSQL.SubscribeToPersistenceEvents(Subscriber: TBoldSubscriber; Events: TBoldSmallEventSet);
begin
  inherited;
end;

procedure TBoldExternalPersistenceControllerSQL.UpdateObjects(
  ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace);
var
  i: integer;
  ObjectContents: IBoldObjectContents;
  MoldClass: TMoldClass;
begin
  for i := 0 to ObjectIdList.count-1 do
  begin
    ObjectContents := ValueSpace.ObjectContentsByObjectId[ObjectIdList[i]];
    MoldClass := MoldModel.Classes[ObjectidList[i].TopSortedIndex];
    InternalUpdateObject(MoldClass, ObjectContents);
  end;
end;

function TBoldExternalPersistenceControllerSQL.GetExternalKeyFromObject(
  ObjectContents: IBoldObjectContents; ValueSpace: IBoldValueSpace): TBoldObjectId;
var
  MoldClass: TMoldClass;
  DBValue: Variant;
begin
  MoldClass := MoldModel.Classes[ObjectContents.ObjectId.TopSortedIndex];

  DBValue := NormalizeVariant(GetObjectKeys(MoldClass, ObjectContents));

  if VarType(DBValue) in [varInteger, varSmallint, varSingle, varDouble] then
  begin
    Result := TBoldDefaultId.Create;
    TBoldDefaultId(Result).AsInteger := DBValue;
  end
  else if VarType(DBValue) = varString then
  begin
    Result := TBoldStringId.Create;
    TBoldStringId(Result).AsString := DBValue;
  end
  else
    raise Exception.CreateFmt(sUnknownVarTypeLoadingObject, [MoldClass.name]);
end;

function TBoldExternalPersistenceControllerSQL.GetMaxFetchBlockSize: integer;
begin
  result := DatabaseAdapter.SQLDatabaseConfig.FetchBlockSize ;
end;

function TBoldExternalPersistenceControllerSQL.ExternalKeysToInternalSQL(MoldClass: TMoldClass; ExternalKeys: TBoldObjectIdList): String;
var
  i, j: integer;
  KeyCount: Integer;
  SQL: String;
  ExternalColumns: String;
  ExternalKey: String;
  S: String;
  T: String;
  v, c: Integer;
begin
  ExternalColumns := FindExternalKeyColumns(MoldClass);
  KeyCount := GetCharCount(';', ExternalColumns) + 1;

  if KeyCount = 1 then
  begin
    Result := inherited ExternalKeysToInternalSQL(MoldClass, ExternalKeys);
    Exit;
  end;

  Result := '';
  for i := 0 to ExternalKeys.Count-1 do
  begin
    SQL := '(';
    ExternalKey := ExternalKeys[i].AsString;
    S := ExternalColumns;
    for j := 0 to GetCharCount(';', ExternalKey) do
    begin
      T := GetNextWord(ExternalKey, ';');
      Val(T, v, c);
      if c <> 0 then
        T := '''' + T + '''';
      SQL := SQL + '(' + GetNextWord(S, ';') + ' = ' + T + ') AND ';
    end;
    if Length(SQL) > 1 then
      SetLength(SQL, Length(SQL)-5);
    SQL := SQL + ')';

    Result := Result + SQL + ' OR ';
  end;

  if Length(Result) > 0 then
    SetLength(Result, Length(Result)-4);
end;

procedure TBoldExternalPersistenceControllerSQL.AssignKeyToObject(
  MoldClass: TMoldClass; ObjectContents: IBoldObjectContents;
  ExternalKey: TBoldObjectId; ValueSpace: IBoldValueSpace);
var
  i: integer;
  KeyValue: IBoldValue;
  StrContent: IBoldStringContent;
  IntContent: IBoldIntegerContent;
  S: String;
begin
  begin
    Keyvalue := nil;
    S := ExternalKey.AsString;
    for i := 0 to MoldClass.AllBoldMembers.Count-1 do
    begin
      if MoldClass.AllBoldMembers[i].Storage = bsExternalKey then
      begin
        KeyValue := ObjectContents.ValueByIndex[i];
        if KeyValue.QueryInterface(IBoldStringContent, StrContent) = S_OK then
          StrContent.AsString := GetNextWord(S, ';')
        else if KeyValue.QueryInterface(IBoldIntegerContent, IntContent) = S_OK then
          IntContent.AsInteger := StrToInt(GetNextWord(S, ';'))
        else
          raise EBold.createFmt('Keytype not handled automatically: %s.%s', [MoldClass.Name, MoldClass.AllBoldMembers[i].Name]);
      end;
    end;
  end;
end;

function TBoldExternalPersistenceControllerSQL.LocateInDB(
  MoldClass: TMoldClass; ObjectContents: IBoldObjectContents;
  BoldQuery: IBoldQuery): Boolean;
var
  ExternalKeys: String;
  ExternalKeyValues: Variant;
begin
  Result := False;
  ExternalKeys := FindExternalKeyColumns(MoldClass);
  if ExternalKeys <> '' then
  begin
    ExternalKeyValues := GetObjectKeys(MoldClass, ObjectContents);
    Result := BoldQuery.AsDataSet.Locate(ExternalKeys, ExternalKeyValues, []);
  end;
end;

procedure TBoldExternalPersistenceControllerSQL.AssignParametersWithMemberIdlist(
  MoldClass: TMoldClass; ObjectContents: IBoldObjectContents;
  Query: IBoldParameterized; MemberIdList: TBoldMemberIdList);
var
  i: integer;
  BoldValue: IBoldValue;
  ParamName: String;
begin
  for i := 0 to MemberIdList.Count-1 do
  begin
    BoldValue := ObjectContents.ValueByMemberId[MemberIdList[i]];
    ParamName := _GetColumnName(MoldClass.AllBoldMembers[MemberIdList[i].MemberIndex]);

    Query.ParamByName(ParamName).asVariant := BoldValueToVariant(BoldValue);
  end;
end;

procedure TBoldExternalPersistenceControllerSQL.InternalCreateObject(
  MoldClass: TMoldClass; ObjectContents: IBoldObjectContents);
var
  BoldExecQuery: IBoldExecQuery;
  MemberIdList: TBoldMemberIdList;
begin
  MemberIdList := TBoldMemberIdList.Create;
  try
    { Assign BoldExecQuery }
    BoldExecQuery := DatabaseAdapter.DatabaseInterface.GetExecQuery;

    { Acquire list of dirty and external members }
    MemberIdList.Clear;
    GetExternalDirtyMembers(MoldClass, ObjectContents, MemberIdList);

    { Generate the SQL query }
    BoldExecQuery.AssignSQLText(GenerateInsertSQL(MoldClass, ObjectContents, MemberIdList));

    { Assign the parameters }
    AssignParametersWithMemberIdList(MoldClass, ObjectContents, BoldExecQuery as IBoldParameterized, MemberIdList);

    { Execute the query }
    BoldExecQuery.ExecSQL;
  finally
    MemberIdList.Free;
  end;
end;

function TBoldExternalPersistenceControllerSQL.GenerateInsertSQL(
  MoldClass: TMoldClass; ObjectContents: IBoldObjectContents;
  MemberIdList: TBoldMemberIdList): String;
var
  i: integer;
  FieldNames: String;
  Params: String;
begin
  FieldNames := '';
  Params := '';

  for i := 0 to MemberIdList.Count-1 do
  begin
    FieldNames := FieldNames + _GetColumnName(MoldClass.AllBoldMembers[i]) + ', ';
    Params := Params + ':' + _GetColumnName(MoldClass.AllBoldMembers[i]) + ', ';
  end;

  { Remove trailing ', ' };
  if Length(FieldNames) > 0 then
  begin
    SetLength(FieldNames, Length(FieldNames)-2);
    SetLength(Params, Length(Params)-2);
    Result := Format('INSERT INTO %s (%s) VALUES (%s)', [_GetTableName(MoldClass), FieldNames,
      Params]);
  end
  else
    Result := Format('INSERT INTO %s', [_GetTableName(MoldClass)]);
end;


procedure TBoldExternalPersistenceControllerSQL.InternalDeleteObject(
  MoldClass: TMoldClass; ObjectContents: IBoldObjectContents);
var
  BoldExecQuery: IBoldExecQuery;
begin
  { Assign BoldQuery }
  BoldExecQuery := DatabaseAdapter.DatabaseInterface.GetExecQuery;

  { Generate the SQL query }
  BoldExecQuery.AssignSQLText(GenerateDeleteSQL(MoldClass, ObjectContents));

  { Execute the query }
  BoldExecQuery.ExecSQL;
end;

function TBoldExternalPersistenceControllerSQL.GenerateDeleteSQL(
  MoldClass: TMoldClass; ObjectContents: IBoldObjectContents): String;
begin
  Result := Format('DELETE FROM %S WHERE %S', [_GetTableName(MoldClass),
    ObjectContentsToInternalSQL(MoldClass, ObjectContents)]);
end;

function TBoldExternalPersistenceControllerSQL.InternalFetchObjects(
  MoldClass: TMoldClass): IBoldDataSet;
begin

end;

procedure TBoldExternalPersistenceControllerSQL.InternalUpdateObject(
  MoldClass: TMoldClass; ObjectContents: IBoldObjectContents);
var
  BoldExecQuery: IBoldExecQuery;
  MemberIdList: TBoldMemberIdList;
begin
  MemberIdList := TBoldMemberIdList.Create;
  try
    { Assign BoldExecQuery }
    BoldExecQuery := DatabaseAdapter.DatabaseInterface.GetExecQuery;

    { Acquire list of dirty and external members }
    GetExternalDirtyMembers(MoldClass, ObjectContents, MemberIdList);

    { Generate the SQL query }
    BoldExecQuery.AssignSQLText(GenerateUpdateSQL(MoldClass, ObjectContents));

    { Assign the parameters }
    AssignParametersWithMemberIdList(MoldClass, ObjectContents, BoldExecQuery as IBoldParameterized, MemberIdList);

    { Execute the query }
    BoldExecQuery.ExecSQL;
  finally
    MemberIdList.Free;
  end;
end;

function TBoldExternalPersistenceControllerSQL.GenerateUpdateSQL(
  MoldClass: TMoldClass; ObjectContents: IBoldObjectContents): String;
var
  i: integer;
  FieldNames: String;
  lMember: TMoldMember;
begin
  FieldNames := '';

  for i := 0 to MoldClass.AllBoldMembers.Count-1 do
    if (MoldClass.AllBoldMembers[i].Storage in [bsExternal, bsExternalKey]) and
      not MoldClass.AllBoldMembers[i].Derived then
    begin
      lMember := MoldClass.AllBoldMembers[i];    
      { Do not store multi links and non-embedded single links }
      if (lMember is TMoldRole) and
         (TMoldRole(lMember).Multi or
         not TMoldRole(lMember).Embed) then
        Continue;

      { Only update the record if the member has been modified }
      if ObjectContents.ValueByIndex[i].BoldPersistenceState = bvpsModified then
      begin
        FieldNames := FieldNames + _GetColumnName(lMember) + ' = :' +
          _GetColumnName(lMember) + ', ';
      end;
    end;

  { Remove trailing ', ' };
  if Length(FieldNames) > 0 then
  begin
    SetLength(FieldNames, Length(FieldNames)-2);
    Result := Format('UPDATE %S SET %S', [_GetTableName(MoldClass), FieldNames]);
    Result := Result + ' WHERE ' + ObjectContentsToInternalSQL(MoldClass, ObjectContents);
  end
  else
    Result := '';
end;

procedure TBoldExternalPersistenceControllerSQL.GetExternalDirtyMembers(
  MoldClass: TMoldClass; ObjectContents: IBoldObjectContents;
  MemberIdList: TBoldMemberIdList);
var
  i: integer;
  BoldValue: IBoldValue;
  lMember: TMoldMember;
begin
  for i := 0 to MoldClass.AllBoldMembers.Count-1 do
    if (MoldClass.AllBoldMembers[i].Storage in [bsExternal, bsExternalKey]) and
      not MoldClass.AllBoldMembers[i].Derived then
    begin
      lMember := MoldClass.AllBoldMembers[i];
      { Do not store multi links and non-embedded single links }
      if (lMember is TMoldRole) and
         (TMoldRole(lMember).Multi or
         not TMoldRole(lMember).Embed) then
        Continue;

      { Only add members that has been modified }
      BoldValue := ObjectContents.ValueByIndex[i];
      if BoldValue.BoldPersistenceState = bvpsModified then
        MemberIdList.Add(TBoldMemberId.Create(i));
    end;
end;

procedure TBoldExternalPersistenceControllerSQL.AssignParameters(
  MoldClass: TMoldClass; ObjectContents: IBoldObjectContents;
  Query: IBoldParameterized);
var
  MemberIdList: TBoldMemberIdList;
begin
  MemberIdList := TBoldMemberIdList.Create;
  try
    GetExternalKeyMembers(MoldClass, MemberIdList);
    AssignParametersWithmemberIdList(MoldClass, ObjectContents, Query, MemberIdList);
  finally
    MemberIdList.Free;
  end;
end;

{ TAbstractFetchObject }

constructor TAbstractFetchObject.Create(ACollection: TCollection;
  AMemberIndex: Integer);
begin
  inherited Create(ACollection);
  FMemberIndex := AMemberIndex;
  FMember := MoldClass.AllBoldMembers[MemberIndex];
end;

function TAbstractFetchObject.GetMoldClass: TMoldClass;
begin
  Result := FetchObjectList.MoldClass;
end;

function TAbstractFetchObject.GetFetchObjectList: TFetchObjectList;
begin
  Result := Collection as TFetchObjectList;
end;

function TAbstractFetchObject.GetPersistenceController: TBoldExternalPersistenceControllerSQL;
begin
  Result := FetchObjectList.PersistenceController;
end;

function TAbstractFetchObject.GetValueSpace: IBoldValueSpace;
begin
  Result := FetchObjectList.ValueSpace;
end;

{ TMemberFetchObject }

procedure TMemberFetchObject.Fetch(Source: IBoldQuery;
  ObjectContents: IBoldObjectContents);
var
  DBFieldName: String;
  Value: IBoldValue;
begin
  Value := ObjectContents.ValueByIndex[MemberIndex];
  DBFieldName := RemovePreAt(MoldClass.AllBoldMembers[MemberIndex]);

  if Source.FieldByName(DBFieldName).IsNull then
    SetBoldValueToNull(Value)
  else
    VariantToBoldValue(Value, Source.FieldByName(DBFieldName).Value);
end;

{ TAbstractRoleFetchObject }

function TAbstractRoleFetchObject.PrepareSQL: String;
var
  W1, W2, KeyNames: String;
  i: integer;
begin
  Result := '';
  if SQL = '' then
  begin
    { Find match }
    W1 := RemovePreAt(Role.OtherEnd); { WHERE %S }
    W2 := RemovePreAt(Role);          {          = :%S }
    KeyNames := W2;

{!!}
    { Sanity check }
    Assert((W1 <> '') and (W2 <> ''), Format(sRoleHasNoColumnNames, [Role.Association.name]));
    Assert(GetCharCount(';', W1) = GetCharCount(';', W2), Format(sRoleEndCountMismatch, [Role.Association.name]));
{!!}

    { Parse SQL }
    SQL := '';
    for i := 0 to GetCharCount(';', W1) do
      SQL := SQL + '(' + GetNextWord(W1, ';') + ' = :' + GetNextWord(W2, ';') + ') AND';

    { Remove last ' AND' }
    SetLength(SQL, Length(SQL)-4);

    SQL := Format('SELECT %S FROM %S WHERE (%S)', [
      StringReplace(FindExternalKeyColumns(Role.OtherEnd.MoldClass), ';', ', ', [rfReplaceAll]),
      _GetTableName(Role.OtherEnd.MoldClass),
      SQL]);
  end;
  Result := SQL;
end;

function TAbstractRoleFetchObject.ExecSQL(Source: IBoldQuery;
  ObjectContents: IBoldObjectContents): IBoldQuery;
var
  i: integer;
  KeyNames: String;
  KeyName: String;
begin
  Result := PersistenceController.DatabaseAdapter.DatabaseInterface.GetQuery;
  Result.AssignSQLText(PrepareSQL);

  KeyNames := RemovePreAt(Role);
  for i := 0 to GetCharCount(';', KeyNames) do
  begin
    KeyName := GetNextWord(KeyNames, ';');
    Result.ParamByName(KeyName).asVariant := Source[KeyName];
  end;
  Result.Open;
end;

function TAbstractRoleFetchObject.GetMoldRole: TMoldRole;
begin
  Result := Member as TMoldRole;
end;

{ TSingleRoleFetchObject }

procedure TSingleRoleFetchObject.Fetch(Source: IBoldQuery;
  ObjectContents: IBoldObjectContents);
var
  OtherEndQuery: IBoldQuery;
  Value: IBoldValue;
  IdRef: IBoldObjectIdRef;
  KeyNames: String;
  DBValue: Variant;
  ExternalId: TBoldObjectId;
begin
  OtherEndQuery := ExecSQL(Source, ObjectContents);
  Value := ObjectContents.ValueByIndex[MemberIndex];

  if OtherEndQuery.AsDataSet.IsEmpty then
    PersistenceController.SetSingleLink(IdRef, nil,
      Role.OtherEnd.MoldClass)
  else
  begin
    KeyNames := FindExternalKeyColumns(Role.OtherEnd.MoldClass);
    DBValue := GetDataValue(OtherEndQuery.AsDataSet, KeyNames);
    if VarType(DBValue) in [varInteger, varSmallint, varSingle, varDouble] then
    begin
      ExternalId := TBoldDefaultId.Create;
      TBoldDefaultId(ExternalId).AsInteger := DBValue;
    end
    else if VarType(dbValue) = varString then
    begin
      ExternalId := TBoldStringId.Create;
      TBoldStringId(ExternalId).AsString := DBValue;
    end
    else
      raise Exception.CreateFmt(sUnknownVarTypeLoadingSingleID, [MoldClass.name, Role.name]);

    if Value.QueryInterface(IBoldObjectIdRef, IdRef) = S_OK then
      PersistenceController.SetSingleLink(IdRef, ExternalId,
        Role.OtherEnd.MoldClass);
  end;
end;

{ TMultiRoleFetchObject }

procedure TMultiRoleFetchObject.Fetch(Source: IBoldQuery;
  ObjectContents: IBoldObjectContents);
var
  Value: IBoldValue;
  OtherEndQuery: IBoldQuery;
  IdRefList: IBoldObjectIdListRef; 
  KeyNames: String;
  DBValue: Variant;
  ExternalId: TBoldObjectId;
  ExternalIdList: TBoldObjectIdList;
begin
  OtherEndQuery := ExecSQL(Source, ObjectContents);
  Value := ObjectContents.ValueByIndex[MemberIndex];
  KeyNames := FindExternalKeyColumns(Role.OtherEnd.MoldClass);

  ExternalIdList := TBoldObjectIdList.Create;
  try
    while not OtherEndQuery.Eof do
    begin
      DBValue := NormalizeVariant(GetDataValue(OtherEndQuery.AsDataSet, KeyNames));
      if VarType(DBValue) in [varInteger, varSmallint, varSingle, varDouble] then
      begin
        ExternalId := TBoldDefaultId.Create;
        TBoldDefaultId(ExternalId).AsInteger := DBValue;
        ExternalIdList.Add(ExternalId);
        ExternalId.Free;
      end
      else if VarType(dbValue) = varString then
      begin
        ExternalId := TBoldStringId.Create;
        TBoldStringId(ExternalId).AsString := DBValue;
        ExternalIdList.Add(ExternalId);
        ExternalId.Free;
      end
      else
        raise Exception.CreateFmt(sUnknownVarTypeLoadingSingleID, [MoldClass.name, Role.name]);
      
      OtherEndQuery.Next;
    end;
    if Value.QueryInterface(IBoldObjectIdListRef, IdRefList) = S_OK then
      PersistenceController.SetMultiLink(IdRefList, ExternalIdList,
        Role.OtherEnd.MoldClass);
  finally
    ExternalIdList.Free;
  end;
end;

{ TFetchObjectList }
constructor TFetchObjectList.Create(
  APersistenceController: TBoldExternalPersistenceControllerSQL;
  AMoldClass: TMoldClass; AMemberIdList: TBoldMemberIdList;
  AExternalKeys: TBoldObjectIdList; AValueSpace: IBoldValueSpace);
var
  i: integer;
begin
  inherited Create(TAbstractFetchObject);
  FPersistenceController := APersistenceController;
  FMoldClass := AMoldClass;
  FExternalKeys := AExternalKeys;
  FValueSpace := AValueSpace;

  for i := 0 to AMemberIdList.Count-1 do
    Add(MoldClass.AllBoldMembers[AMemberIdList[i].MemberIndex]);
end;

function TFetchObjectList.Add(Member: TMoldMember): TAbstractFetchObject;
var
  FetchClass: TFetchObjectClass;
begin
  if Member is TMoldRole then
    if TMoldRole(Member).Multi then
      FetchClass := TMultiRoleFetchObject
    else
      FetchClass := TSingleRoleFetchObject
  else
    FetchClass := TMemberFetchObject;

  Result := FetchClass.Create(Self, MoldClass.AllBoldMembers.IndexOf(Member));

  { Added, that is called from Classes.TCollection.Add is deprecated and does }
  { nothing, so we don't call it from here }
end;

function TFetchObjectList.GetItem(Index: Integer): TAbstractFetchObject;
begin
  Result := inherited GetItem(Index) as TAbstractFetchObject;
end;

procedure TFetchObjectList.SetItem(Index: Integer;
  Value: TAbstractFetchObject);
begin
  inherited SetItem(Index, Value);
end;

function TFetchObjectList.IndexOf(MemberId: TBoldMemberId): Integer;
begin
  for Result := 0 to Count-1 do
    if Items[Result].MemberIndex = MemberId.MemberIndex then
      exit;
  Result := -1;
end;

function TFetchObjectList.Find(
  MemberId: TBoldMemberId): TAbstractFetchObject;
var
  Index: Integer;
begin
  Index := IndexOf(MemberId);
  if Index = -1 then
    Result := nil
  else
    Result := Items[Index];
end;

{ TFetchContext }

constructor TFetchContext.Create(
  PersistenceController: TBoldExternalPersistenceControllerSQL;
  ExternalKeys: TBoldObjectIdList;
  ValueSpace: IBoldValueSpace; MoldClass: TMoldClass;
  MemberIdList: TBoldMemberIdList);

var
  S: TStringList;
  SQL: String;
  i: integer;
begin
{!!}
  { Sanity check, should never happen }
  Assert(MemberIdList.Count > 0, 'MemberIdList.Count = 0!!');
  Assert(ExternalKeys.Count > 0, 'Nothing to fetch!!');
{!!}

  inherited Create;
  FFetchObjectList := TFetchObjectList.Create(PersistenceController,
    MoldClass, MemberIdList, ExternalKeys, ValueSpace);

  { Remove duplicates in memberlist, occours with roles }
  S := TStringList.Create;
  try
    S.Duplicates := dupIgnore;
    for i := 0 to MemberIdList.Count-1 do
      S.Add(StringReplace(
        RemovePreAt(MoldClass.AllBoldMembers[MemberIdList[i].MemberIndex]),
        ';', #13#10, [rfReplaceAll]));

    SQL := '';
    for i := 0 to S.Count-1 do
      SQL := SQL + S[i] + ', ';
    SetLength(SQL, Length(SQL)-2);

    SQL := Format('SELECT %S FROM %S WHERE %S', [
      SQL, _GetTableName(MoldClass), PersistenceController.ExternalKeysToInternalSQL(MoldClass, ExternalKeys)]);
  finally
    S.Free;
  end;
  Source := PersistenceController.DatabaseAdapter.DatabaseInterface.GetQuery;
  Source.AssignSQLText(SQL);
  Source.Open;
end;

destructor TFetchContext.Destroy;
begin
  FFetchObjectList.Free;
  inherited;
end;

procedure TFetchContext.FetchObject(ObjectContents: IBoldObjectContents);
var
  i: integer;
begin
  for i := 0 to FetchObjectList.Count-1 do
  begin
    if FetchObjectList.PersistenceController.LocateInDB(
         FetchObjectList.MoldClass, ObjectContents, Source) then
      FetchObjectList[i].Fetch(Source, ObjectContents)
    else
      raise Exception.Create(sObjectNoLongerInDB);
  end;
end;

procedure TFetchContext.PostFetch;
begin
  { Do nothing }
end;


function TBoldExternalPersistenceControllerSQL.ObjectContentsToInternalSQL(
  MoldClass: TMoldClass; ObjectContents: IBoldObjectContents): String;
var
  ObjectIdList: TBoldObjectIdList;
  ObjectId: TBoldObjectId;
begin
  ObjectId := GetExternalKeyFromObject(ObjectContents, nil);
  try
    ObjectIdList := TBoldObjectIdList.Create;
    try
      ObjectIdList.Add(ObjectId);
      Result := ExternalKeysToInternalSQL(MoldClass, ObjectIdList);
    finally
      ObjectIdList.Free;
    end;
  finally
    ObjectId.Free;
  end;
end;

end.