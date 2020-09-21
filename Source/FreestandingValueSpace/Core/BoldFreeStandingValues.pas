unit BoldFreeStandingValues;

interface

uses
  Classes,
  BoldDefs,
  BoldBase,
  BoldStreams,
  BoldId,
  BoldIndexableList,
  BoldValueSpaceInterfaces,
  BoldValueInterfaces;

type
  { forward declarations }
  TBoldFreeStandingElement = class;
  TBoldFreeStandingValueSpace = class;
  TBoldFreeStandingObjectContents = class;
  TBoldFSObjectContentList = class;

  TBoldFreeStandingValue = class;
  TBFSString = class;
  TBFSInteger = class;
  TBFSFloat = class;
  TBFSCurrency = class;
  TBFSBoolean = class;
  TBFSDate = class;
  TBFSTime = class;
  TBFSDateTime = class;
  TBFSBlob = class;
  TBFSTypedBlob = class;
  TBFSObjectIdRef = class;
  TBFSObjectIdRefPair = class;
  TBFSObjectIdListRef = class;
  TBFSObjectIdListRefPair = class;

  IBoldFreeStandingIdList = interface
  ['{DB2619EC-7C21-4528-982A-62DF96F501FC}']
    procedure RemoveId(Id: TBoldObjectId);
    procedure AddId(Id: TBoldObjectId);
  end;

  IBoldFreeStandingIdListPair = interface
  ['{0347FECD-F555-4931-96C6-2889F17C2CB6}']
    procedure RemoveId(Id: TBoldObjectId);
    procedure AddIds(Id1, Id2: TBoldObjectId);
  end;

  TBoldFreeStandingElementClass = class of TBoldFreeStandingElement;

  { TBoldFreeStandingElement }
  TBoldFreeStandingElement = class(TBoldNonRefCountedObject)
  public
    constructor Create; virtual;
  end;

  { TBoldFreeStandingValueSpace }
  TBoldFreeStandingValueSpace = class(TBoldFreeStandingElement, IBoldValueSpace)
  private
    fIdLIst: TBoldObjectIdlist;
    fObjectContentsList: TBoldFSObjectContentList;
    fTranslationList: TBoldIdTranslationList;
    function GetIdList: TBoldObjectIdList;
    function GetObjectContentsByObjectId(ObjectId: TBoldObjectId): IBoldObjectContents;
    procedure ApplyTranslationList(IdTranslationList: TBoldIdTranslationList);
    procedure EnsureObjectId(ObjectId: TBoldObjectId);
    procedure ExactifyIDs(TranslationList: TBoldIdTranslationList);
  protected
    property IdList: TBoldObjectIdList read GetIdList;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure RemoveDeletedObjects;
    procedure MarkAllObjectsAndMembersCurrent;
    function GetFSObjectContentsByObjectId(ObjectId: TBoldObjectId): TBoldFreeStandingObjectContents;
    procedure RemoveFSObjectContentsByObjectId(ObjectId: TBoldObjectId);
    procedure ApplyValueSpace(ValueSpace: IBoldValueSpace; IgnorePersistenceState: Boolean);
    procedure AllObjectIds(resultList: TBoldObjectIdList; OnlyLoaded: Boolean);
    procedure UpdateOwnValuesFrom(ValueSpace: IBoldValueSpace);
    procedure RemoveAllObjectContents;
    function GetHasContentsForId(ObjectId: TBoldObjectId): boolean;
    procedure EnsureObjectContents(ObjectId: TBoldObjectId);
    function GetEnsuredObjectContentsByObjectId(ObjectId: TBoldObjectId): IBoldObjectContents;
    function GetEnsuredFSObjectContentsByObjectId(ObjectId: TBoldObjectId): TBoldFreeStandingObjectContents;
    function GetValueForIdAndMemberIndex(Id: TBoldObjectId; MemberIndex: integer): IBoldValue;
  end;

  PMemberList = ^TMemberList;
  TMemberList = array[0..MaxListSize - 1] of TBoldFreeStandingValue;



  { TBoldFreeStandingObjectContents }
  TBoldFreeStandingObjectContents = class(TBoldFreeStandingElement, IBoldObjectContents, IBoldStreamable)
  private
    fBoldExistenceState: TBoldExistenceState;
    fBoldPersistenceState: TBoldValuePersistenceState;
    fGlobalId: string;
    fIsReadOnly: Boolean;
    fMemberList: PMemberList;
    fMemberCount: integer;
    fObjectId: TBoldObjectId;
    fTimeStamp: TBoldTimeStampType;
    function GetObjectId: TBoldObjectId;
    function GetStreamName: string;
    procedure ApplyTranslationList(TranslationList: TBoldIdTranslationList);
    function GetBoldExistenceState: TBoldExistenceState;
    function GetBoldMemberCount: Integer;
    function GetBoldPersistenceState: TBoldValuePersistenceState;
    function GetGlobalId: string;
    function GetIsModified: Boolean;
    function GetIsReadOnly: Boolean;
    function GetValueByIndex(I: Integer): IBoldValue;
    function GetFSValueByIndex(i: integer): TBoldFreeStandingValue;
    function GetValueByMemberId(MemberId: TBoldMemberID):IBoldValue;
    function GetTimeStamp: TBoldTimeStampType;
    procedure SetBoldExistenceState(Value: TBoldExistenceState);
    procedure SetBoldPersistenceState(Value: TBoldValuePersistenceState);
    procedure SetGlobalId(const NewValue: string);
    procedure SetIsReadOnly(NewValue: Boolean);
    procedure SetTimeStamp(NewValue: TBoldTimeStampType);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure MarkAllMembersCurrent;
    procedure RemoveMemberByIndex(Index: integer);
    procedure EnsureMember(MemberId: TBoldMemberId; const StreamName: string);
    procedure EnsureMemberByIndex(Index: integer; const StreamName: string);
    procedure ApplyObjectContents(ObjectContents: IBoldObjectContents; const ApplyValues: Boolean; const IgnorePersistenceState: Boolean);
    procedure UpdateObjectContentsFrom(ObjectContents: IBoldObjectContents);
    property ValueByIndex[I: integer]: IBoldValue read GetValueByIndex;
    property FSValueByIndex[I: integer]: TBoldFreeStandingValue read GetFSValueByIndex;
    property BoldExistenceState: TBoldExistenceState read GetBoldExistenceState write SetBoldExistenceState;
    property BoldPersistenceState: TBoldValuePersistenceState read GetBoldPersistenceState write SetBoldPersistenceState;
    property MemberCount: integer read fMemberCount;
  end;

  { TBoldFreeStandingValue }
  TBoldFreeStandingValue = class(TBoldFreeStandingElement, IBoldValue, IBoldStreamable)
  private
    fBoldPersistenceState: TBoldValuePersistenceState;
  protected
    function GetContentName: String;
    function GetBoldPersistenceState: TBoldValuePersistenceState;
    procedure SetBoldPersistenceState(Value: TBoldValuePersistenceState);
    function GetStreamName: string; virtual; abstract;
    procedure ApplyTranslationList(TranslationList: TBoldIdTranslationList); virtual;
    procedure AssignContentValue(Source: IBoldValue); virtual;
  public
    constructor Create; override;
    procedure AssignContent(Source: IBoldValue);
    property ContentName: string read GetContentName;
    property BoldPersistenceState: TBoldValuePersistenceState read fBoldPersistenceState write fBoldPersistenceState;
  end;

  { TBoldFreeStandingNullableValue }
  TBoldFreeStandingNullableValue = class(TBoldFreeStandingValue, IBoldNullableValue)
  private
    fIsNull: Boolean;
  protected
    procedure SetToNonNull;
    function GetContentIsNull: Boolean;
    procedure SetContentToNull;
  end;

  { TBFSInteger }
  TBFSInteger = class(TBoldFreeStandingNullableValue, IBoldIntegerContent)
  private
    fDataValue: Integer;
    function GetContentAsInteger: Integer;
    procedure SetContentAsInteger(NewValue: Integer);
  protected
    function GetStreamName: string; override;
    procedure AssignContentValue(Source: IBoldValue); override;
  public
    property AsInteger: Integer read GetContentAsInteger write SetContentAsInteger;
  end;

  { TBFSString }
  TBFSString = class(TBoldFreeStandingNullableValue, IBoldStringContent)
  private
    fDataValue: String;
    function GetContentAsString: String;
    procedure SetContentAsString(const NewValue: String);
  protected
    function GetStreamName: string; override;
    procedure AssignContentValue(Source: IBoldValue); override;
  public
    property AsString: String read GetContentAsString write SetContentAsString;
  end;

  { TBFSCurrency }
  TBFSCurrency = class(TBoldFreeStandingNullableValue, IBoldCurrencyContent)
  private
    fDataValue: Currency;
    function GetContentAsCurrency: Currency;
    procedure SetContentAsCurrency(NewValue: Currency);
  protected
    function GetStreamName: String; override;
    procedure AssignContentValue(Source: IBoldValue); override;
  public
    property AsCurrency: Currency read GetContentAsCurrency write SetContentAsCurrency;
  end;

  { TBFSFloat }
  TBFSFloat = class(TBoldFreeStandingNullableValue, IBoldFloatContent)
  private
    fDataValue: Double;
    function GetContentAsFloat: Double;
    procedure SetContentAsFloat(NewValue: Double);
  protected
    function GetStreamName: String; override;
    procedure AssignContentValue(Source: IBoldValue); override;
  public
    property AsFloat: Double read GetContentAsFloat write SetContentAsFloat;
  end;

  { TBFSBoolean }
  TBFSBoolean = class(TBoldFreeStandingNullableValue, IBoldBooleanContent)
  private
    fDataValue: Boolean;
    function GetContentAsBoolean: Boolean;
    procedure SetContentAsBoolean(NewValue: Boolean);
  protected
    function GetStreamName: String; override;
    procedure AssignContentValue(Source: IBoldValue); override;
  public
    property AsBoolean: Boolean read GetContentAsBoolean write SetContentAsBoolean;
  end;

  { TBFSDateTimeAbstract }
  TBFSDateTimeAbstract = class(TBoldFreeStandingNullableValue)
  private
    fDataValue: TDateTime;
  protected
    function GetContentAsDateTime: TDateTime;
    procedure SetContentAsDateTime(NewValue: TDateTime);
    function GetContentAsTime: TDateTime;
    procedure SetContentAsTime(NewValue: TDateTime);
    function GetContentAsDate: TDateTime;
    procedure SetContentAsDate(NewValue: TDateTime);
  end;

  { TBFSDateTime }
  TBFSDateTime = class(TBFSDateTimeAbstract, IBoldDateTimeContent)
  protected
    function GetStreamName: String; override;
    procedure AssignContentValue(Source: IBoldValue); override;
  public
    property AsDateTime: TDateTime read GetContentAsDateTime write SetContentAsDateTime;
  end;

  { TBFSDate }
  TBFSDate = class(TBFSDateTimeAbstract, IBoldDateContent)
  protected
    function GetStreamName: String; override;
    procedure AssignContentValue(Source: IBoldValue); override;
  public
    property AsDate: TDateTime read GetContentAsDate write SetContentAsDate;
  end;

  { TBFSTime }
  TBFSTime = class(TBFSDateTimeAbstract, IBoldTimeContent)
  protected
    function GetStreamName: String; override;
    procedure AssignContentValue(Source: IBoldValue); override;
  public
    property AsTime: TDateTime read GetContentAsTime write SetContentAsTime;
  end;

  { TBFSBlobAbstract }
  TBFSBlobAbstract = class(TBoldFreeStandingNullableValue)
  private
    fDataValue: String;
    function GetContentAsBlob: String;
    procedure SetContentAsBlob(const NewValue: String);
  protected
    function GetStreamName: String; override;
  public
    property AsBlob: String read GetContentAsBlob write SetContentAsBlob;
  end;

  { TBFSBlob }
  TBFSBlob = class(TBFSBlobAbstract, IBoldBlobContent)
  protected
    procedure AssignContentValue(Source: IBoldValue); override;
  end;

  { TBFSTypedBlob }
  TBFSTypedBlob = class(TBFSBlobAbstract, IBoldTypedBlob, IBoldBlobContent)
  private
    fContent: String;
    function GetContentTypeContent: String;
    procedure SetContentTypeContent(const NewValue: String);
  protected
      function GetStreamName: String; override;
    procedure AssignContentValue(Source: IBoldValue); override;
  public
    property ContentTypeContent: String read GetContentTypeContent write SetContentTypeContent;
  end;

{==============================}

  { TBFSObjectIDRefAbstract }
  TBFSObjectIDRefAbstract = class(TBoldFreeStandingValue)
  private
    fOrderNo: Integer;
    function GetOrderNo: integer;
    procedure SetOrderNo(NewOrder: Integer);
  public
    property OrderNo: integer read GetOrderNo write SetOrderNo;
  end;

  { TBFSObjectIDRef }
  TBFSObjectIDRef = class(TBFSObjectIDRefAbstract, IBoldObjectIdRef)
  private
    fObjectId: TBoldObjectId;
    function GetId: TBoldObjectID;
  protected
    procedure AssignContentValue(Source: IBoldValue); override;
    function GetStreamName: String; override;
    procedure ApplyTranslationList(TranslationList: TBoldIdTranslationList); override;
  public
    destructor Destroy; override;
    procedure SetFromId(Id: TBoldObjectId);
    property Id: TBoldObjectId read GetId;
  end;

  { TBFSObjectIDRefPair }
  TBFSObjectIDRefPair = class(TBFSObjectIDRefAbstract, IBoldObjectIdRefPair)
  private
    fObjectIds: TBoldObjectIdList;
    procedure EnsureIdList;
    function GetId1: TBoldObjectID;
    function GetId2: TBoldObjectID;
  protected
    procedure AssignContentValue(Source: IBoldValue); override;
    function GetStreamName: String; override;
    procedure ApplyTranslationList(TranslationList: TBoldIdTranslationList); override;
  public
    destructor Destroy; override;
    procedure SetFromIds(Id1, Id2: TBoldObjectId);
    property Id1: TBoldObjectId read GetId1;
    property Id2: TBoldObjectId read GetId2;
  end;

  { TBFSObjectIdListref }
  TBFSObjectIdListref = class(TBoldFreeStandingValue, IBoldObjectIdListRef, IBoldFreeStandingIdList)
  private
    fIdLIst: TBoldObjectidLIst;
    function GetIdList(Index: Integer): TBoldObjectID;
    procedure RemoveId(Id: TBoldObjectId);
    procedure AddId(Id: TBoldObjectId);
    procedure EnsureList;
  protected
    procedure AssignContentValue(Source: IBoldValue); override;
    function GetStreamName: String; override;
    procedure ApplyTranslationList(TranslationList: TBoldIdTranslationList); override;
    function GetCount: integer;
  public
    destructor Destroy; override;
    procedure SetFromIdList(IdList: TBoldObjectIdList);
    property IdList[Index: integer]: TBoldObjectID read GetIdList;
    property Count: integer read GetCount;
  end;

  { TBFSObjectIdListrefPair }
  TBFSObjectIdListrefPair = class(TBoldFreeStandingValue, IBoldObjectIdListRefPair, IBoldFreeStandingIdListPair)
  private
    fIdLIst1, fIdList2: TBoldObjectidLIst;
    function GetIdList1(Index: Integer): TBoldObjectID;
    function GetIdList2(Index: Integer): TBoldObjectID;
    procedure EnsureLists;
    procedure AddIds(Id1, Id2: TBoldObjectId);
    procedure RemoveId(Id: TBoldObjectId);
  protected
    procedure AssignContentValue(Source: IBoldValue); override;
    function GetStreamName: String; override;
    procedure ApplyTranslationList(TranslationList: TBoldIdTranslationList); override;
    function GetCount: integer;
  public
    destructor Destroy; override;
    procedure SetFromIdLists(IdList1, IdList2: TBoldObjectIdList);
    property IdList1[Index: integer]: TBoldObjectID read GetIdList1;
    property IdList2[Index: integer]: TBoldObjectID read GetIdList2;
    property Count: integer read GetCount;
  end;

  { TBoldFSObjectContentList }

  // this list should be an unordered indexable list for performace reasons, but FSValueSpace.ApplyTranslationList is a bit tricky to rewrite...
  TBoldFSObjectContentList = class(TBoldIndexableList)
  private
    function GetItems(index: integer): TBoldFreeStandingObjectContents;
    procedure SetItems(index: integer; const Value: TBoldFreeStandingObjectContents);
  public
    constructor Create;
    function FindObjectContentById(Id: TBoldObjectId): TBoldFreeStandingObjectContents;
    property Items[index: integer]: TBoldFreeStandingObjectContents read GetItems write SetItems; default;
  end;

implementation

uses
  SysUtils,
  FreeStandingValuesConst,
  BoldUtils,
  BoldIndex,
  BoldHashIndexes,
  BoldFreeStandingValueFactories,
  BoldDefaultStreamNames,
  BoldMemoryManager,
  BoldGuard;

type
  {---TBoldObjectIdHashIndex---}
  TBoldFSObjectContentHashIndex = class(TBoldHashIndex)
  protected
    function ItemAsBoldObjectId(Item: TObject): TBoldObjectId; virtual;
    function HashItem(Item: TObject): Cardinal; override;
    function Match(const Key; Item:TObject):Boolean; override;
    function Hash(const Key): Cardinal; override;
    function FindById(boldObjectId:TboldObjectId): TObject;
  end;

var
  IX_FSObjectContent: integer = -1;


{---TBoldFreeStandingElement---}

constructor TBoldFreeStandingElement.Create;
begin
  // this is empty, but allows other freestanding values to initialize values
  // in their overridden constructors
  inherited;
end;

{---TBoldFreeStandingvalueSpace---}

constructor TBoldFreeStandingvalueSpace.Create;
begin
  inherited;
  fObjectContentsList := TBoldFSObjectContentList.create;
  fIdLIst := tBoldObjectIdList.Create;
end;

destructor TBoldFreeStandingvalueSpace.Destroy;
{var
  i: integer;}
begin
{  The objects should now be maintained by the indexed list.
  for i := 0 to fObjectContentsList.Count - 1 do
    TObject(fObjectContentsList[i]).Free;
 }
  FreeAndNil(fObjectContentsList);
  FreeAndNil(fIdLIst);
  FreeAndNil(fTranslationList);
  inherited;
end;

function TBoldFreeStandingvalueSpace.GetIdList: TBoldObjectIdList;
begin
  if not assigned(fIdList) then
    fIdList := TBoldObjectidList.Create;
  result := fIdList;
end;

function TBoldFreeStandingvalueSpace.GetFSObjectContentsByObjectId(ObjectId: TBoldObjectId): TBoldFreeStandingObjectContents;
begin
  result := fObjectContentsList.FindObjectContentById(ObjectId);
end;

procedure TBoldFreeStandingvalueSpace.RemoveFSObjectContentsByObjectId(ObjectId: TBoldObjectId);
begin
  fObjectContentsList.Remove(GetFSObjectContentsByObjectId(ObjectId));
end;

function TBoldFreeStandingvalueSpace.GetObjectContentsByObjectId(ObjectId: TBoldObjectId): IBoldObjectContents;
var
  temp: TBoldFreeStandingObjectContents;
begin
  temp := GetFSObjectContentsByObjectId(ObjectId);
  if assigned(temp) then
    result := temp
  else
    result := nil;
end;

function TBoldFreeStandingvalueSpace.GetEnsuredObjectContentsByObjectId(ObjectId: TBoldObjectId): IBoldObjectContents;
begin
  Result := GetEnsuredFSObjectContentsByObjectId(ObjectId);
end;

function TBoldFreeStandingvalueSpace.GetHasContentsForId(ObjectId: TBoldObjectId): boolean;
begin
  result := assigned(GetObjectContentsByObjectId(Objectid));
end;

procedure TBoldFreeStandingvalueSpace.EnsureObjectContents(ObjectId: TBoldObjectId);
begin
  GetEnsuredFSObjectContentsByObjectId(ObjectId);
end;

procedure TBoldFreeStandingvalueSpace.EnsureObjectId(ObjectId: TBoldObjectId);
begin
  if not IdLIst.IdInList[ObjectId] then
    IdLIst.Add(ObjectId);
end;

procedure TBoldFreeStandingvalueSpace.ApplytranslationList(IdTranslationList: TBoldIdTranslationList);

  procedure DoOneObject(IndexInList: integer; anObject: TBoldFreeStandingObjectContents);
  var
    newId: TBoldObjectId;
  begin
    newId := IdTranslationList.TranslateToNewId[anObject.fObjectId];
    if assigned(newId) and (newId <> anObject.fObjectId) then
    begin
      fObjectContentsList.OwnsEntries := false;
      fObjectContentsList.items[indexInList] := nil;
      fObjectContentsList.OwnsEntries := true;
      IdList.ReplaceID(anObject.fObjectId, newId);
      anObject.fObjectId := IdList.IDByID[newId];
      fObjectContentsList.items[indexInList] := anObject;
    end;
    anObject.ApplyTranslationList(IdTranslationList);
  end;

var
  i: integer;
begin
  if not assigned(fTranslationList) then
    fTranslationList := TBoldIDTranslationList.Create;
  For i := 0 to IdTranslationList.Count - 1 do
      fTranslationList.AddTranslation(IdTranslationList.OldIds[i], IdTranslationList.NewIds[i]);
  for i := 0 to fObjectContentsList.Count - 1 do
    DoOneObject(i, TBoldFreeStandingObjectContents(fObjectContentsList[i]));
end;

procedure TBoldFreeStandingvalueSpace.ApplyValueSpace(ValueSpace: IBoldValueSpace; IgnorePersistenceState: Boolean);
var
  i: Integer;
  anIdList: TBoldObjectIdList;
  ObjectId: TBoldObjectId;
  ObjectContents: TBoldFreeStandingObjectContents;
  g: IBoldGuard;
begin
  g := TBoldGuard.Create(anIDList);
  anIdList := TBoldObjectIdList.Create;
  ValueSpace.AllObjectIds(anIdList, true);
  for i := 0 to anIdList.Count - 1 do
  begin
    ObjectId := anIdList[i];
    ObjectContents := GetFSObjectContentsByObjectId(ObjectId);
    if not assigned(ObjectContents) then
    begin
      EnsureObjectContents(ObjectId);
      ObjectContents := GetFSObjectContentsByObjectId(ObjectId);
    end;
    ObjectContents.ApplyObjectContents(ValueSpace.ObjectContentsByObjectId[ObjectId], True, IgnorePersistenceState);
  end;
end;

procedure TBoldFreeStandingValueSpace.ExactifyIDs(TranslationList: TBoldIdTranslationList);
begin
end;

{---TBoldFreeStandingObjectContents---}

constructor TBoldFreeStandingObjectContents.create;
begin
  inherited;
  fBoldExistenceState := besExisting;
  fTimeStamp := -1;
end;

destructor TBoldFreeStandingObjectContents.Destroy;
var
  i: integer;
begin
  if Assigned(fMemberList) then
  begin
    for i := 0 to fMemberCount - 1 do
      fMemberList^[i].Free;
    BoldMemoryManager_.DeAllocateMemory(fMemberList, fMemberCount*sizeof(Pointer));
  end;
  inherited;
end;


function TBoldFreeStandingObjectContents.GetValueByMemberId(MemberId: TBoldMemberID):IBoldValue;
begin
  result := GetValueByIndex(MemberId.MemberIndex);
end;

function TBoldFreeStandingObjectContents.GetStreamName: string;
begin
  result := BOLDOBJECTCONTENTSNAME;
end;

procedure TBoldFreeStandingObjectContents.EnsureMember(MemberId: TBoldMemberId; const StreamName: string);
begin
   EnsurememberByIndex( MemberId.MemberIndex, StreamName );
end;

function TBoldFreeStandingObjectContents.GetValueByIndex(I: Integer): IBoldValue;
begin
  if i < MemberCount then
    result := fMemberList^[i]
  else
    result := nil;
end;

function TBoldFreeStandingObjectContents.GetFSValueByIndex(i: integer): TBoldFreeStandingValue;
begin
  if i < MemberCount then
    result := fMemberList^[i]
  else
    result := nil;
end;


function TBoldFreeStandingObjectContents.GetIsModified: Boolean;
begin
  result := True;
end;

{---TBoldFreeStandingValue---}

function TBoldFreeStandingValue.GetBoldPersistenceState: TBoldValuePersistenceState;
begin
  result := fBoldPersistenceState;
end;

procedure TBoldFreeStandingValue.ApplyTranslationList(TranslationList: TBoldIdTranslationList);
begin
  // do nothing
end;

{---TBoldFreeStandingNullableValue---}

procedure TBoldFreeStandingNullableValue.SetContentToNull;
begin
  fIsNull := true;
end;

function TBoldFreeStandingNullableValue.GetContentIsNull: Boolean;
begin
  result := fIsNull;
end;

procedure TBoldFreeStandingNullableValue.SetToNonNull;
begin
  fIsNull := false;
end;

{---TBFSInteger---}

function TBFSInteger.GetStreamName: string;
begin
  result := BoldContentName_Integer;
end;

function TBFSInteger.GetContentAsInteger: Integer;
begin
  result := fDataValue;
end;

procedure TBFSInteger.SetContentAsInteger(NewValue: Integer);
begin
  fDataValue := NewValue;
  SetToNonNull;
end;

{---TBFSString---}

function TBFSString.GetStreamName: string;
begin
  result := BoldContentName_String;
end;

function TBFSString.GetContentAsString: String;
begin
  result := fDataValue;
end;

procedure TBFSString.SetContentAsString(const NewValue: String);
begin
  fDataValue := NewValue;
  SetToNonNull;
end;

{---TBFSCurrency---}

function TBFSCurrency.GetStreamName: String;
begin
  result := BoldContentName_Currency;
end;

function TBFSCurrency.GetContentAsCurrency: Currency;
begin
  result := fDataValue;
end;

procedure TBFSCurrency.SetContentAsCurrency(NewValue: Currency);
begin
  fDataValue := NewValue;
  SetToNonNull
end;

{---TBFSFloat---}

function TBFSFloat.GetStreamName: String;
begin
  result := BoldContentName_Float;
end;

function TBFSFloat.GetContentAsFloat: double;
begin
  result := fDataValue;
end;

procedure TBFSFloat.SetContentAsFloat(NewValue: Double);
begin
  fDataValue := NewValue;
  SetToNonNull;
end;

{---TBFSBoolean---}

function TBFSBoolean.GetStreamName: String;
begin
  Result := BoldContentName_Boolean;
end;

function TBFSBoolean.GetContentAsBoolean: Boolean;
begin
  Result := fDataValue;
end;

procedure TBFSBoolean.SetContentAsBoolean(NewValue: Boolean);
begin
  fDataValue := NewValue;
  SetToNonNull;
end;

{---TBFSDateTimeAbstract---}

function TBFSDateTimeAbstract.GetContentAsDateTime: TDateTime;
begin
  Result := fDataValue;
end;

procedure TBFSDateTimeAbstract.SetContentAsDateTime(NewValue: TDateTime);
begin
  fDataValue := NewValue;
  SetToNonNull;
end;

function TBFSDateTimeAbstract.GetContentAsDate: TDateTime;
begin
  result := fDataValue;
end;

procedure TBFSDateTimeAbstract.SetContentAsDate(NewValue: TDateTime);
begin
  fDataValue := NewValue;
  SetToNonNull;
end;

function TBFSDateTimeAbstract.GetContentAsTime: TDateTime;
begin
  result := fDataValue;
end;

procedure TBFSDateTimeAbstract.SetContentAsTime(NewValue: TDateTime);
begin
  fDataValue := NewValue;
  SetToNonNull;
end;

{---TBFSBlobAbstract---}

function TBFSBlobAbstract.GetStreamName: String;
begin
  Result := BoldContentName_Blob;
end;

function TBFSBlobAbstract.GetContentAsBlob: String;
begin
  Result := fDataValue;
end;

procedure TBFSBlobAbstract.SetContentAsBlob(const NewValue: String);
begin
  fDataValue := NewValue;
  SetToNonNull;
end;

{---TBFSTypedBlob---}

function TBFSTypedBlob.GetStreamName: String;
begin
  Result := BoldContentName_TypedBlob;
end;

function TBFSTypedBlob.GetContentTypeContent: String;
begin
  Result := fContent;
end;

procedure TBFSTypedBlob.SetContentTypeContent(const NewValue: String);
begin
  fContent := NewValue;
  SetToNonNull;
end;

{--- TBFSObjectIdRefAbstract ---}

function TBFSObjectIdRefAbstract.GetOrderNo: integer;
begin
  Result := fOrderNo;
end;

procedure TBFSObjectIdRefAbstract.SetOrderNo(NewOrder: Integer);
begin
  fOrderNo := NewOrder;
end;

{--- TBFSObjectIdRef ---}

function TBFSObjectIdRef.GetStreamName: String;
begin
  Result := BoldContentName_ObjectIdRef;
end;

procedure TBFSObjectIdRef.SetFromId(Id: TBoldObjectId);
begin
  if assigned(Id) then
  begin
    if (not Assigned(fObjectId)) or (not Id.IsEqual[fObjectId]) then
    begin
      FreeAndNil(fObjectId);
      fObjectId := Id.Clone;
    end
  end
  else
    FreeAndNil(fObjectId);
end;

function TBFSObjectIdRef.GetId: TBoldObjectID;
begin
  Result := fObjectId;
end;

{--- TBFSObjectIdRefPair ---}

function TBFSObjectIdRefPair.GetStreamName: String;
begin
  Result := BoldContentName_ObjectIdRefPair;
end;

procedure TBFSObjectIdRefPair.SetFromIds(Id1, Id2: TBoldObjectId);
begin
  EnsureIdList;
  while fObjectIds.Count > 0 do
    fObjectIds.RemoveByIndex(0);
  if assigned(Id1) then
  begin
    fObjectIds.Add(Id1);
    if assigned(Id2) then
      fObjectIds.Add(Id2);
  end;
end;

function TBFSObjectIdRefPair.GetId1: TBoldObjectID;
begin
  EnsureIdList;
  if fObjectIds.Count > 0 then
    result := fObjectIds[0]
  else
    result := nil;
end;

function TBFSObjectIdRefPair.GetId2: TBoldObjectID;
begin
  EnsureIdList;
  if fObjectIds.Count > 1 then
    result := fObjectIds[1]
  else
    result := nil;
end;

{--- TBFSObjectIdListref ---}
function TBFSObjectIdListref.GetStreamName: String;
begin
  Result := BoldContentName_ObjectIdListRef;
end;

procedure TBFSObjectIdListref.SetFromIdList(IdList: TBoldObjectIdList);
begin
  fIdLIst.Free;
  fIdList := IdList.Clone;
end;

function TBFSObjectIdListref.GetIdList(Index: Integer): TBoldObjectID;
begin
  result := fIdList[Index];
end;

{--- TBFSObjectIdListrefPair ---}
function TBFSObjectIdListrefPair.GetStreamName: String;
begin
  result := BoldContentName_ObjectIdListRefPair;
end;

procedure TBFSObjectIdListrefPair.SetFromIdLists(IdList1, IdList2: TBoldObjectIdList);
begin
  fIdLIst1.Free;
  fIdList1 := IdList1.Clone;
  fIdLIst2.Free;
  fIdList2 := IdList2.Clone;
end;

function TBFSObjectIdListrefPair.GetIdList1(Index: Integer): TBoldObjectID;
begin
  result := fIdList1[Index];
end;

function TBFSObjectIdListrefPair.GetIdList2(Index: Integer): TBoldObjectID;
begin
  result := fIdList2[Index];
end;

function TBoldFreeStandingObjectContents.GetIsReadOnly: Boolean;
begin
  result := fIsReadOnly;
end;

procedure TBoldFreeStandingObjectContents.SetIsReadOnly(NewValue: Boolean);
begin
  fIsReadOnly := NewValue;
end;

function TBoldFreeStandingObjectContents.GetTimeStamp: TBoldTimeStampType;
begin
  result := fTimeStamp;
end;

procedure TBoldFreeStandingObjectContents.SetTimeStamp(NewValue: TBoldTimeStampType);
begin
  fTimeStamp := NewValue;
end;

function TBoldFreeStandingObjectContents.GetGlobalId: string;
begin
  result := fGlobalId;
end;

procedure TBoldFreeStandingObjectContents.SetGlobalId(const NewValue: string);
begin
  fGlobalId := NewValue;
end;

procedure TBoldFreeStandingObjectContents.ApplyTranslationList(
  TranslationList: TBoldIdTranslationList);
var
  i: Integer;
begin
  for i := 0 to MemberCount - 1 do
    if Assigned(fMemberList^[i]) then
      fMemberList^[i].ApplyTranslationList(TranslationList);
end;

procedure TBFSObjectIdListref.ApplyTranslationList(
  TranslationList: TBoldIdTranslationList);
begin
  if assigned(fIdList) then
    fIdLIst.ApplyTranslationList(TranslationList);
end;

procedure TBFSObjectIdListrefPair.ApplyTranslationList(
  TranslationList: TBoldIdTranslationList);
begin
  if assigned(fIdlist1) then
  begin
    // the two lists are either assigned or not assigned, there can never be one assigned and one not assigned
    fIdLIst1.ApplyTranslationList(TranslationList);
    fIdList2.ApplyTranslationList(TranslationList);
  end;
end;

procedure TBFSObjectIDRef.ApplyTranslationList(
  TranslationList: TBoldIdTranslationList);
var
  newId: TBoldObjectId;
begin
  if Assigned(FObjectId) then
  begin
    newId := TranslationList.TranslateToNewId[fObjectId];
    if assigned(NewId) and not NewId.IsEqual[fObjectId] then
    begin
      FreeAndNil(fObjectId);
      fObjectId := NewId.Clone;
    end;
  end;
end;

procedure TBFSObjectIDRefPair.ApplyTranslationList(
  TranslationList: TBoldIdTranslationList);
begin
  if assigned(fObjectIds) then
    fObjectIds.ApplyTranslationList(TranslationList);
end;

destructor TBFSObjectIDRef.Destroy;
begin
  FreeAndNil(fObjectId);
  inherited;
end;

destructor TBFSObjectIDRefPair.Destroy;
begin
  FreeAndNil(fObjectIds);
  inherited;
end;

procedure TBFSObjectIDRefPair.EnsureIdList;
begin
  if not assigned(fObjectIds) then
    fObjectIds := TBoldObjectIdList.Create;
end;

procedure TBoldFreeStandingValueSpace.AllObjectIds(
  resultList: TBoldObjectIdList; OnlyLoaded: Boolean);
var
  i: integer;
begin
  for i := 0 to fIdList.Count - 1 do
    if OnlyLoaded then
    begin
      if Assigned(GetObjectContentsByObjectId(fIdList[i])) then
        resultList.Add(fIdLIst[i]);
    end
    else
      resultList.Add(fIdList[i])
end;

procedure TBoldFreeStandingValueSpace.RemoveDeletedObjects;
var
  ObjectContents: TBOldFreeStandingObjectContents;
  i: integer;
  id: TBoldObjectId;
begin
  for i := fObjectContentsList.Count - 1 downto 0 do
  begin
    ObjectContents := TBOldFreeStandingObjectContents(fObjectContentsList[i]);
    if ObjectContents.fBoldExistenceState = besDeleted then
    begin
      id := fIdLIst.IDByID[ObjectContents.fObjectId];
      fObjectContentsList.Remove(ObjectContents);
      fIdLIst.Remove(id);
    end;
  end;
end;

function TBFSObjectIdListref.GetCount: integer;
begin
  if assigned(fIdList) then
    result := fIdList.Count
  else
    result := 0;
end;

function TBFSObjectIdListrefPair.GetCount: integer;
begin
  if assigned(fIdList1) then
    result := fIdList1.Count
  else
    result := 0;
end;

procedure TBoldFreeStandingValue.SetBoldPersistenceState(
  Value: TBoldValuePersistenceState);
begin
  fBoldPersistenceState := Value;
end;

function TBoldFreeStandingObjectContents.GetBoldExistenceState: TBoldExistenceState;
begin
  result := fBoldExistenceState;
end;

function TBoldFreeStandingObjectContents.GetBoldPersistenceState: TBoldValuePersistenceState;
begin
  result := fBoldPersistenceState;
end;

procedure TBoldFreeStandingObjectContents.SetBoldExistenceState(
  Value: TBoldExistenceState);
begin
  fBoldExistenceState := Value;
end;

procedure TBoldFreeStandingObjectContents.SetBoldPersistenceState(
  Value: TBoldValuePersistenceState);
begin
  fBoldPersistenceState := Value;
end;

procedure TBoldFreeStandingValue.AssignContent(Source: IBoldValue);
begin
  AssignContentValue(Source);
  BoldPersistenceState := Source.BoldPersistenceState;
end;

procedure TBFSInteger.AssignContentValue(Source: IBoldValue);
var
  s: IBoldIntegerContent;
begin
  if source.QueryInterface(IBoldIntegerContent, S) = S_OK then
    if s.IsNull then
      SetContentToNull
    else
      SetContentAsInteger(s.AsInteger)
  else
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname]);
end;

procedure TBFSString.AssignContentValue(Source: IBoldValue);
var
  s: IBoldStringContent;
begin
  if source.QueryInterface(IBoldStringContent, S) = S_OK then
    if s.IsNull then
      SetContentToNull
    else
      SetContentAsString(s.AsString)
  else
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname]);
end;

procedure TBFSCurrency.AssignContentValue(Source: IBoldValue);
var
  s: IBoldCurrencyContent;
begin
  if source.QueryInterface(IBoldCurrencyContent, S) = S_OK then
    if s.IsNull then
      SetContentToNull
    else
      SetContentAsCurrency(s.AsCurrency)
  else
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname]);
end;

procedure TBFSFloat.AssignContentValue(Source: IBoldValue);
var
  s: IBoldFloatContent;
begin
  if source.QueryInterface(IBoldFloatContent, S) = S_OK then
    if s.IsNull then
      SetContentToNull
    else
      SetContentAsFloat(s.AsFloat)
  else
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname]);
end;

procedure TBFSBoolean.AssignContentValue(Source: IBoldValue);
var
  s: IBoldBooleanContent;
begin
  if source.QueryInterface(IBoldBooleanContent, S) = S_OK then
    if s.IsNull then
      SetContentToNull
    else
      SetContentAsBoolean(s.AsBoolean)
  else
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname]);
end;

{ TBFSDateTime }

procedure TBFSDateTime.AssignContentValue(Source: IBoldValue);
var
  s: IBoldDateTimeContent;
begin
  if source.QueryInterface(IBoldDateTimeContent, S) = S_OK then
    if s.IsNull then
      SetContentToNull
    else
      SetContentAsDateTime(s.AsDateTime)
  else
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname]);
end;

function TBFSDateTime.GetStreamName: String;
begin
  result := BoldContentName_DateTime;
end;

{ TBFSDate }

procedure TBFSDate.AssignContentValue(Source: IBoldValue);
var
  s: IBoldDateContent;
begin
  if source.QueryInterface(IBoldDateContent, S) = S_OK then
    if s.IsNull then
      SetContentToNull
    else
      SetContentAsDate(s.AsDate)
  else
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname]);
end;

function TBFSDate.GetStreamName: String;
begin
  result := BoldContentName_Date;
end;

{ TBFSTime }

procedure TBFSTime.AssignContentValue(Source: IBoldValue);
var
  s: IBoldTimeContent;
begin
  if source.QueryInterface(IBoldTimeContent, S)= S_OK then
    if s.IsNull then
      SetContentToNull
    else
      SetContentAsTime(s.AsTime)
  else
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname]);
end;

function TBFSTime.GetStreamName: String;
begin
  result := BoldContentName_Time;
end;

{ TBFSBlob }

procedure TBFSBlob.AssignContentValue(Source: IBoldValue);
var
  s: IBoldBlobContent;
begin
  if source.QueryInterface(IBoldBlobContent, S) = S_OK then
    if s.IsNull then
      SetContentToNull
    else
      SetContentAsBlob(s.AsBlob)
  else
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname]);
end;

procedure TBFSTypedBlob.AssignContentValue(Source: IBoldValue);
var
  s: IBoldBlobContent;
  t: IBoldTypedBlob;
begin
  if (source.QueryInterface(IBoldBlobContent, S) = S_OK) and
     (source.QueryInterface(IBoldTypedBlob, t) = S_OK) then
  begin
    if s.IsNull then
      SetContentToNull
    else
      SetContentAsBlob(s.AsBlob);
    SetContentTypeContent(t.ContentTypeContent);
  end
  else
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname]);
end;

procedure TBFSObjectIDRef.AssignContentValue(Source: IBoldValue);
var
  s: IBoldObjectIdRef;
begin
  if source.QueryInterface(IBoldObjectIDRef, S) = S_OK then
  begin
    SetFromId(s.Id);
    OrderNo := s.OrderNo;
  end
  else
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname]);
end;

procedure TBFSObjectIDRefPair.AssignContentValue(Source: IBoldValue);
var
  s: IBoldObjectIdRefPair;
begin
  if source.QueryInterface(IBoldObjectIDRefPair, S) = S_OK then
  begin
    SetFromIds(s.Id1, s.Id2);
    OrderNo := s.OrderNo;
  end
  else
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname]);
end;

procedure TBFSObjectIdListref.AssignContentValue(Source: IBoldValue);
var
  s: IBoldObjectIdListRef;
  i: Integer;
begin
  if source.QueryInterface(IBoldObjectIDListRef, S) = S_OK then
  begin
    EnsureList;
    fIdList.Clear;
    for i := 0 to s.Count - 1 do
      fIdList.Add(s.IdList[i]);
  end
  else
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname]);
end;

procedure TBFSObjectIdListrefPair.AssignContentValue(Source: IBoldValue);
var
  s: IBoldObjectIdListRefPair;
  i: Integer;
begin
  if source.QueryInterface(IBoldObjectIDListRefPair, S) = S_OK then
  begin
    EnsureLists;
    fIdList1.Clear;
    fIdList2.Clear;
    for i := 0 to s.Count - 1 do
    begin
      fIdList1.Add(s.IdList1[i]);
      fIdList2.Add(s.IdList2[i]);
    end;
  end
  else
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname]);
end;

destructor TBFSObjectIdListrefPair.Destroy;
begin
  FreeAndNil(fIdList1);
  FreeAndNil(fIdList2);
  inherited;
end;

destructor TBFSObjectIdListref.Destroy;
begin
  FreeAndNil(fIdList);
  inherited;
end;

procedure TBoldFreeStandingObjectContents.RemoveMemberByIndex(Index: integer);
begin
  if MemberCount > Index then
  begin
    fMemberList^[Index].Free;
    fMemberList^[Index] := nil;
  end;
end;

{ TBoldFSObjectContentList }

constructor TBoldFSObjectContentList.Create;
begin
  inherited Create;
  SetIndexCapacity(1);
  OwnsEntries := true;
  SetIndexVariable(IX_FSObjectContent, AddIndex(TBoldFSObjectContentHashIndex.Create));
end;

function TBoldFSObjectContentList.FindObjectContentById(Id: TBoldObjectId): TBoldFreeStandingObjectContents;
begin
  Result := TBoldFreeStandingObjectContents(TBoldFSObjectContentHashIndex(indexes[IX_FSObjectContent]).FindByID(ID));
end;

function TBoldFSObjectContentList.GetItems(index: integer): TBoldFreeStandingObjectContents;
begin
  result := TBoldFreeStandingObjectContents(inherited items[index]);
end;

procedure TBoldFSObjectContentList.SetItems(index: integer;
  const Value: TBoldFreeStandingObjectContents);
begin
  inherited items[index] := Value;
end;

{ TBoldFSObjectContentHashIndex }

function TBoldFSObjectContentHashIndex.FindById(BoldObjectId: TBoldObjectId): TObject;
begin
  Result := Find(BoldObjectId);
end;

function TBoldFSObjectContentHashIndex.Hash(const Key): Cardinal;
begin
  Result := TBoldObjectId(Key).Hash;
end;

function TBoldFSObjectContentHashIndex.HashItem(Item: TObject): Cardinal;
begin
  Result := ItemAsBoldObjectId(Item).Hash;
end;

function TBoldFSObjectContentHashIndex.ItemAsBoldObjectId(Item: TObject): TBoldObjectId;
begin
  Assert(Item is TBoldFreeStandingObjectContents);
  Result := TBoldFreeStandingObjectContents(Item).fObjectId;
end;

function TBoldFSObjectContentHashIndex.Match(const Key; Item: TObject): Boolean;
begin
  Result := TBoldObjectId(Key).IsEqual[ItemAsBoldObjectId(Item)];
end;

function TBoldFreeStandingValue.GetContentName: String;
begin
  result := GetStreamName;
end;

constructor TBoldFreeStandingValue.create;
begin
  inherited;
  BoldPersistenceState := bvpsInvalid;
end;

procedure TBoldFreeStandingValue.AssignContentValue(Source: IBoldValue);
begin
  raise EBold.CreateFmt(sAbstractError, [classname]);
end;

function TBoldFreeStandingObjectContents.GetObjectId: TBoldObjectId;
begin
  result := fObjectId;
end;

procedure TBoldFreeStandingObjectContents.ApplyObjectContents(
  ObjectContents: IBoldObjectContents; const ApplyValues: Boolean; const IgnorePersistenceState: Boolean);
var
  i: Integer;
  aValue: IBoldValue;
  MemberId: TBoldMemberId;
begin
  SetBoldExistenceState(ObjectContents.BoldExistenceState);
  SetBoldPersistenceState(ObjectContents.BoldPersistenceState);
  if ApplyValues then
    for i := 0 to ObjectContents.MemberCount - 1 do
    begin
      aValue := ObjectContents.valueByIndex[i];
      if assigned(aValue) then
      begin
        MemberId := TBoldMemberId.Create(i);
        try
          EnsureMember(MemberId, aValue.ContentName);
          if IgnorePersistenceState then
            fMemberList^[i].AssignContentValue(aValue)
          else
            fMemberList^[i].AssignContent(aValue);
        finally
          FreeAndNil(MemberId);
        end;
      end;
    end;
end;

procedure TBoldFreeStandingValueSpace.UpdateOwnValuesFrom(
  ValueSpace: IBoldValueSpace);
var
  i: Integer;
  anIdList: TBoldObjectIdList;
  ObjectId: TBoldObjectId;
  OwnObjectContents: TBoldFreeStandingObjectContents;
  ObjectContents: IBoldObjectContents;
begin
  anIdList := TBoldObjectIdList.Create;
  try
    AllObjectIds(anIdList, true);
    for i := 0 to anIdList.Count - 1 do
    begin
      ObjectId := anIdList[i];
      OwnObjectContents := GetFSObjectContentsByObjectId(ObjectId);
      ObjectContents := ValueSpace.ObjectContentsByObjectId[ObjectId];
      if assigned(ObjectContents) then
        OwnObjectContents.UpdateObjectContentsFrom(ObjectContents);
    end;
  finally
    anIdList.Free;
  end;
end;

procedure TBoldFreeStandingObjectContents.UpdateObjectContentsFrom(
  ObjectContents: IBoldObjectContents);
var
  i: Integer;
  OwnValue, aValue: IBoldValue;
begin
  SetBoldExistenceState(ObjectContents.BoldExistenceState);
  SetBoldPersistenceState(ObjectContents.BoldPersistenceState);
  for i := 0 to MemberCount - 1 do
  begin
    OwnValue := GetValueByIndex(i);
    aValue := ObjectContents.ValueByIndex[i];
    if assigned(OwnValue) and assigned(aValue) then
      OwnValue.AssignContent(aValue);
  end;
end;

procedure TBoldFreeStandingValueSpace.RemoveAllObjectContents;
var
 i: integer;
 ObjectIds: TBoldObjectIdlist;
begin
  ObjectIds := TBoldObjectIdList.Create;
  try
    AllObjectIds(ObjectIds, True);
    for i:= 0 to ObjectIds.Count - 1 do
      RemoveFSObjectContentsByObjectId(ObjectIds[i]);
  finally
    FreeAndNil(Objectids);
  end;
end;

procedure TBoldFreeStandingValueSpace.MarkAllObjectsAndMembersCurrent;
var
  ObjectContents: TBOldFreeStandingObjectContents;
  ObjectIx: integer;
begin
  for ObjectIx := fObjectContentsList.Count - 1 downto 0 do
  begin
    ObjectContents := TBOldFreeStandingObjectContents(fObjectContentsList[ObjectIx]);
    ObjectContents.fBoldPersistenceState := bvpsCurrent;
    ObjectContents.MarkAllMembersCurrent;
  end;
end;

procedure TBoldFreeStandingObjectContents.MarkAllMembersCurrent;
var
  M: integer;
begin
  for M := 0 to MemberCount - 1 do
    if Assigned(fMemberList^[M]) then
      GetValueByIndex(M).BoldPersistenceState := bvpsCurrent;
end;



procedure TBoldFreeStandingObjectContents.EnsureMemberByIndex(
  Index: integer; const StreamName: string);
begin
  if not Assigned(fMemberlist) then
  begin
    fMemberlist := BoldMemoryManager_.AllocateMemoryZeroFill((Index+1)*sizeof(Pointer));
    fMemberCount := Index+1;
  end;
  if (Index >= MemberCount) then
  begin
    fMemberlist := BoldMemoryManager_.ReallocateMemoryZeroFill(fMemberlist, MemberCount*sizeof(Pointer),
    (Index+1)*sizeof(Pointer));
    fMemberCount := Index+1;
  end;

  if not assigned(fMemberlist^[Index]) then
    fMemberlist^[Index] := TBoldFreeStandingValue(FreeStandingValueFactory.CreateInstance(StreamName));
end;


function TBoldFreeStandingValueSpace.GetValueForIdAndMemberIndex(
  Id: TBoldObjectId; MemberIndex: integer): IBoldValue;
var
  ObjectContents: TBoldFreeStandingObjectContents;
begin
  ObjectContents := GetFSObjectContentsByObjectId(Id);
  if Assigned(ObjectContents) then
     Result := ObjectContents.ValueByIndex[MemberIndex]
end;

function TBoldFreeStandingValueSpace.GetEnsuredFSObjectContentsByObjectId(
  ObjectId: TBoldObjectId): TBoldFreeStandingObjectContents;
var
  LocalId: TBoldObjectId;
begin
  Result := GetFSObjectContentsByObjectId(ObjectId);
  if not Assigned(Result) then
  begin
    LocalId := IdLIst.IdById[ObjectId];
    if not assigned(LocalId) then
    begin
      IdList.Add(ObjectId);
      LocalId :=IdLIst.IdById[ObjectId];
    end;
    Result := TBoldFreeStandingObjectContents.Create;
    Result.fObjectId := LocalId;
    fObjectContentsList.Add(Result);
  end;
end;

function TBoldFreeStandingObjectContents.GetBoldMemberCount: Integer;
begin
  Result := fMemberCount;
end;


procedure TBFSObjectIdListrefPair.RemoveId(Id: TBoldObjectId);
var
  p: integer;
begin
  p := fIdLIst1.IndexByID[id];
  if p <> -1 then
  begin
    fIdLIst1.RemoveByIndex(p);
    fIdList2.RemoveByIndex(p);
  end;
end;

procedure TBFSObjectIdListref.RemoveId(Id: TBoldObjectId);
var
  p: integer;
begin
  p := fIdList.IndexByID[id];
  if p <> -1 then
    fIdLIst.RemoveByIndex(p);
end;

procedure TBFSObjectIdListrefPair.AddIds(Id1, Id2: TBoldObjectId);
begin
  fIdList1.Add(Id1);
  fIdLIst2.Add(Id2);
end;

procedure TBFSObjectIdListref.AddId(Id: TBoldObjectId);
begin
  EnsureList;
  fIdList.Add(Id);
end;

procedure TBFSObjectIdListref.EnsureList;
begin
  if not assigned(fIdList) then
    fIdList := TBoldObjectIdList.Create;
end;

procedure TBFSObjectIdListrefPair.EnsureLists;
begin
  if not assigned(fIdList1) then
    fIdList1 := TBoldObjectIdList.Create;
  if not assigned(fIdList2) then
    fIdList2 := TBoldObjectIdList.Create;
end;

end.
