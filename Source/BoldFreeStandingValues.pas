{ Global compiler directives }
{$include bold.inc}
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
  TBFSAnsiString = class;
  TBFSUnicodeString = class;
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
  TBoldFreeStandingObjectContentsClass = class of TBoldFreeStandingObjectContents;

  { TBoldFreeStandingElement }
  TBoldFreeStandingElement = class(TBoldNonRefCountedObject)
  public
    class function ContentType: TBoldValueContentType; virtual; abstract;
    function GetContentType: TBoldValueContentType;
    constructor Create; virtual;
  end;

  { TBoldFreeStandingValueSpace }
  TBoldFreeStandingValueSpace = class(TBoldFreeStandingElement, IBoldValueSpace)
  strict private
    fIdList: TBoldObjectIdlist;
    fObjectContentsList: TBoldFSObjectContentList;
    fTranslationList: TBoldIdTranslationList;
  private
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
    class function ContentType: TBoldValueContentType; override;
    function AssertLinkIntegrity: Boolean;
    procedure RemoveDeletedObjects;
    procedure MarkAllObjectsAndMembersCurrent;
    function GetFSObjectContentsByObjectId(ObjectId: TBoldObjectId): TBoldFreeStandingObjectContents;
    procedure RemoveFSObjectContentsByObjectId(ObjectId: TBoldObjectId);
    procedure RemoveFSObjectContents(ObjectContents: TBoldFreeStandingObjectContents);
    procedure ApplyValueSpace(const ValueSpace: IBoldValueSpace; IgnorePersistenceState: Boolean);
    procedure AllObjectIds(resultList: TBoldObjectIdList; OnlyLoaded: Boolean);
    function GetAnyObjectId: TBoldObjectId;
    procedure ClearWhenObjectContentsEmpty;
    procedure UpdateOwnValuesFrom(const ValueSpace: IBoldValueSpace);
    procedure RemoveAllObjectContents;
    function GetHasContentsForId(ObjectId: TBoldObjectId): boolean;
    procedure EnsureObjectContents(ObjectId: TBoldObjectId);
    function GetEnsuredObjectContentsByObjectIdAndCheckIfCreated(ObjectId: TBoldObjectId; out aBoldObjectContents: IBoldObjectContents): boolean;
    function GetEnsuredObjectContentsByObjectId(ObjectId: TBoldObjectId): IBoldObjectContents;
    function GetEnsuredFSObjectContentsByObjectId(ObjectId: TBoldObjectId): TBoldFreeStandingObjectContents; overload;
    function GetEnsuredFSObjectContentsByObjectId(ObjectId: TBoldObjectId; out aCreated: boolean): TBoldFreeStandingObjectContents; overload;
    function GetValueForIdAndMemberIndex(Id: TBoldObjectId; MemberIndex: integer): IBoldValue;
    function IdCount: integer;
    function IsEmpty: boolean;
    procedure Clear;
  end;

  { TBoldFreeStandingObjectContents }
  TBoldFreeStandingObjectContents = class(TBoldFreeStandingElement, IBoldObjectContents, IBoldStreamable)
  private
    fBoldExistenceState: TBoldExistenceState;
    fBoldPersistenceState: TBoldValuePersistenceState;
    fGlobalId: string;
    fIsReadOnly: Boolean;
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
  protected
    fMemberList: array of TBoldFreeStandingValue;
    procedure EnsureMemberListLength(index: integer); //
  public
    constructor Create; override;
    destructor Destroy; override;
    class function ContentType: TBoldValueContentType; override;
    procedure MarkAllMembersCurrent;
    procedure RemoveMemberByIndex(Index: integer);
    procedure EnsureMember(MemberId: TBoldMemberId; const StreamName: string);
    function EnsureMemberAndGetValueByIndex(Index: integer; const StreamName: string): IBoldValue;
    procedure ApplyObjectContents(const ObjectContents: IBoldObjectContents; ApplyValues: Boolean; IgnorePersistenceState: Boolean);
    procedure UpdateObjectContentsFrom(const ObjectContents: IBoldObjectContents);
    function IsEmpty: boolean;
    property ValueByIndex[I: integer]: IBoldValue read GetValueByIndex;
    property FSValueByIndex[I: integer]: TBoldFreeStandingValue read GetFSValueByIndex;
    property BoldExistenceState: TBoldExistenceState read GetBoldExistenceState write SetBoldExistenceState;
    property BoldPersistenceState: TBoldValuePersistenceState read GetBoldPersistenceState write SetBoldPersistenceState;
    property TimeStamp: TBoldTimeStampType read GetTimeStamp write SetTimeStamp;
    property MemberCount: integer read GetBoldMemberCount;
    property ObjectId: TBoldObjectId read GetObjectId;
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
    procedure AssignContentValue(const Source: IBoldValue); virtual;
  public
    constructor Create; override;
    procedure AssignContent(const Source: IBoldValue);
    function IsEqualToValue(const Value: IBoldValue): Boolean; virtual;
    property ContentName: string read GetContentName;
    property BoldPersistenceState: TBoldValuePersistenceState read fBoldPersistenceState write fBoldPersistenceState;
  end;

  { TBoldFreeStandingNullableValue }
  TBoldFreeStandingNullableValue = class(TBoldFreeStandingValue, IBoldNullableValue, IBoldVariantReadable, IBoldStringRepresentable)
  private
    fIsNull: Boolean;
    function GetAsVariant: Variant;
  protected
    procedure SetToNonNull;
    function GetContentIsNull: Boolean;
    procedure SetContentToNull;
    function GetValueAsVariant: Variant; virtual; abstract;
    function GetStringRepresentation(representation:integer): String; virtual;
    function GetContentAsString: String; virtual;
  public
    function IsEqualToValue(const Value: IBoldValue): Boolean; override;
  end;

  { TBFSInteger }
  TBFSInteger = class(TBoldFreeStandingNullableValue, IBoldIntegerContent)
  private
    fDataValue: Integer;
    function GetContentAsInteger: Integer;
    procedure SetContentAsInteger(NewValue: Integer);
  protected
    function GetStreamName: string; override;
    procedure AssignContentValue(const Source: IBoldValue); override;
    function GetValueAsVariant: Variant; override;
  public
    class function ContentType: TBoldValueContentType; override;
    function IsEqualToValue(const Value: IBoldValue): Boolean; override;
    property AsInteger: Integer read GetContentAsInteger write SetContentAsInteger;
  end;

  { TBFSString }
  TBFSString = class(TBoldFreeStandingNullableValue, IBoldStringContent)
  private
    fDataValue: String;
    procedure SetContentAsString(const NewValue: String);
  protected
    function GetContentAsString: String; override;
    function GetStreamName: string; override;
    procedure AssignContentValue(const Source: IBoldValue); override;
    function GetValueAsVariant: Variant; override;
  public
    class function ContentType: TBoldValueContentType; override;
    function IsEqualToValue(const Value: IBoldValue): Boolean; override;
    property AsString: String read GetContentAsString write SetContentAsString;
  end;

  { TBFSAnsiString }
  TBFSAnsiString = class(TBoldFreeStandingNullableValue, IBoldAnsiStringContent)
  private
    fDataValue: AnsiString;
    procedure SetContentAsAnsiString(const NewValue: TBoldAnsiString);
    function GetContentAsAnsiString: TBoldAnsiString;
    procedure SetContentAsString(const NewValue: String);
  protected
    function GetContentAsString: String; override;
    function GetStreamName: string; override;
    procedure AssignContentValue(const Source: IBoldValue); override;
    function GetValueAsVariant: Variant; override;
  public
    class function ContentType: TBoldValueContentType; override;
    function IsEqualToValue(const Value: IBoldValue): Boolean; override;
    property AsAnsiString: AnsiString read GetContentAsAnsiString write SetContentAsAnsiString;
  end;

  { TBFSUnicodeString }
  TBFSUnicodeString = class(TBoldFreeStandingNullableValue, IBoldUnicodeStringContent)
  private
    fDataValue: UnicodeString;
    procedure SetContentAsUnicodeString(const NewValue: TBoldUnicodeString);
    function GetContentAsUnicodeString: TBoldUnicodeString;
    procedure SetContentAsString(const NewValue: String);
  protected
    function GetContentAsString: String; override;
    function GetStreamName: string; override;
    procedure AssignContentValue(const Source: IBoldValue); override;
    function GetValueAsVariant: Variant; override;
  public
    class function ContentType: TBoldValueContentType; override;
    function IsEqualToValue(const Value: IBoldValue): Boolean; override;
    property AsUnicodeString: UnicodeString read GetContentAsUnicodeString write SetContentAsUnicodeString;
  end;

  { TBFSCurrency }
  TBFSCurrency = class(TBoldFreeStandingNullableValue, IBoldCurrencyContent)
  private
    fDataValue: Currency;
    function GetContentAsCurrency: Currency;
    procedure SetContentAsCurrency(NewValue: Currency);
  protected
    function GetStreamName: String; override;
    procedure AssignContentValue(const Source: IBoldValue); override;
    function GetValueAsVariant: Variant; override;
  public
    class function ContentType: TBoldValueContentType; override;
    function IsEqualToValue(const Value: IBoldValue): Boolean; override;
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
    procedure AssignContentValue(const Source: IBoldValue); override;
    function GetValueAsVariant: Variant; override;
  public
    class function ContentType: TBoldValueContentType; override;
    function IsEqualToValue(const Value: IBoldValue): Boolean; override;
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
    procedure AssignContentValue(const Source: IBoldValue); override;
    function GetValueAsVariant: Variant; override;
  public
    class function ContentType: TBoldValueContentType; override;
    function IsEqualToValue(const Value: IBoldValue): Boolean; override;
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
    procedure AssignContentValue(const Source: IBoldValue); override;
    function GetValueAsVariant: Variant; override;
  public
    class function ContentType: TBoldValueContentType; override;
    function IsEqualToValue(const Value: IBoldValue): Boolean; override;
    property AsDateTime: TDateTime read GetContentAsDateTime write SetContentAsDateTime;
  end;

  { TBFSDate }
  TBFSDate = class(TBFSDateTimeAbstract, IBoldDateContent)
  protected
    function GetStreamName: String; override;
    procedure AssignContentValue(const Source: IBoldValue); override;
    function GetValueAsVariant: Variant; override;
  public
    class function ContentType: TBoldValueContentType; override;
    function IsEqualToValue(const Value: IBoldValue): Boolean; override;
    property AsDate: TDateTime read GetContentAsDate write SetContentAsDate;
  end;

  { TBFSTime }
  TBFSTime = class(TBFSDateTimeAbstract, IBoldTimeContent)
  protected
    function GetStreamName: String; override;
    procedure AssignContentValue(const Source: IBoldValue); override;
    function GetValueAsVariant: Variant; override;
  public
    class function ContentType: TBoldValueContentType; override;
    function IsEqualToValue(const Value: IBoldValue): Boolean; override;
    property AsTime: TDateTime read GetContentAsTime write SetContentAsTime;
  end;

  { TBFSBlobAbstract }
  TBFSBlobAbstract = class(TBoldFreeStandingNullableValue)
  private
    fDataValue: AnsiString;
    function GetContentAsBlob: TBoldAnsiString;
    procedure SetContentAsBlob(const NewValue: TBoldAnsiString);
  protected
    function GetStreamName: String; override;
    function GetValueAsVariant: Variant; override;
  public
    property AsBlob: TBoldAnsiString read GetContentAsBlob write SetContentAsBlob;
  end;

  { TBFSBlob }
  TBFSBlob = class(TBFSBlobAbstract, IBoldBlobContent)
  protected
    procedure AssignContentValue(const Source: IBoldValue); override;
  public
    class function ContentType: TBoldValueContentType; override;
    function IsEqualToValue(const Value: IBoldValue): Boolean; override;
  end;

  { TBFSTypedBlob }
  TBFSTypedBlob = class(TBFSBlobAbstract, IBoldTypedBlob, IBoldBlobContent)
  private
    fContent: String;
    function GetContentTypeContent: String;
    procedure SetContentTypeContent(const NewValue: String);
  protected
    function GetStreamName: String; override;
    procedure AssignContentValue(const Source: IBoldValue); override;
  public
    class function ContentType: TBoldValueContentType; override;
    function IsEqualToValue(const Value: IBoldValue): Boolean; override;
    property ContentTypeContent: String read GetContentTypeContent write SetContentTypeContent;
  end;

{==============================}

  { TBFSObjectIDRefAbstract }
  TBFSObjectIDRefAbstract = class(TBoldFreeStandingValue, IBoldStringRepresentable)
  private
    fOrderNo: Integer;
    function GetOrderNo: integer;
    procedure SetOrderNo(NewOrder: Integer);
  protected
    function GetStringRepresentation(representation:integer): String; virtual; abstract;
    function GetContentAsString: String; virtual; abstract;
  public
    property OrderNo: integer read GetOrderNo write SetOrderNo;
  end;

  { TBFSObjectIDRef }
  TBFSObjectIDRef = class(TBFSObjectIDRefAbstract, IBoldObjectIdRef, IBoldStringRepresentable)
  private
    fObjectId: TBoldObjectId;
    function GetId: TBoldObjectID;
  protected
    function GetStringRepresentation(representation:integer): String; override;
    function GetContentAsString: String; override;
    procedure AssignContentValue(const Source: IBoldValue); override;
    function GetStreamName: String; override;
    procedure ApplyTranslationList(TranslationList: TBoldIdTranslationList); override;
  public
    destructor Destroy; override;
    class function ContentType: TBoldValueContentType; override;
    function IsEqualToValue(const Value: IBoldValue): Boolean; override;
    procedure SetFromId(Id: TBoldObjectId; Adopt: Boolean);
    property Id: TBoldObjectId read GetId;
  end;

  { TBFSObjectIDRefPair }
  TBFSObjectIDRefPair = class(TBFSObjectIDRefAbstract, IBoldObjectIdRefPair, IBoldStringRepresentable)
  private
    fObjectIds: TBoldObjectIdList;
    procedure EnsureIdList;
    function GetId1: TBoldObjectID;
    function GetId2: TBoldObjectID;
  protected
    procedure AssignContentValue(const Source: IBoldValue); override;
    function GetStreamName: String; override;
    procedure ApplyTranslationList(TranslationList: TBoldIdTranslationList); override;
  public
    destructor Destroy; override;
    class function ContentType: TBoldValueContentType; override;
    procedure SetFromIds(Id1, Id2: TBoldObjectId);
    property Id1: TBoldObjectId read GetId1;
    property Id2: TBoldObjectId read GetId2;
  end;

  { TBFSObjectIdListref }
  TBFSObjectIdListref = class(TBoldFreeStandingValue, IBoldObjectIdListRef, IBoldFreeStandingIdList, IBoldStringRepresentable)
  private
    fIdLIst: TBoldObjectidLIst;
    function GetIdList(Index: Integer): TBoldObjectID;
    procedure RemoveId(Id: TBoldObjectId);
    procedure AddId(Id: TBoldObjectId);
    procedure EnsureList;
  protected
    function GetStringRepresentation(representation:integer): String;
    function GetContentAsString: String;
    procedure AssignContentValue(const Source: IBoldValue); override;
    function GetStreamName: String; override;
    procedure ApplyTranslationList(TranslationList: TBoldIdTranslationList); override;
    function GetCount: integer;
  public
    destructor Destroy; override;
    class function ContentType: TBoldValueContentType; override;
    procedure SetFromIdList(IdList: TBoldObjectIdList);
    procedure SetList(IdList: TBoldObjectIdList);
    property IdList[Index: integer]: TBoldObjectID read GetIdList;
    property Count: integer read GetCount;
  end;

  { TBFSObjectIdListrefPair }
  TBFSObjectIdListrefPair = class(TBoldFreeStandingValue, IBoldObjectIdListRefPair, IBoldFreeStandingIdListPair, IBoldStringRepresentable)
  private
    fIdLIst1, fIdList2: TBoldObjectidLIst;
    function GetIdList1(Index: Integer): TBoldObjectID;
    function GetIdList2(Index: Integer): TBoldObjectID;
    procedure EnsureLists;
    procedure AddIds(Id1, Id2: TBoldObjectId);
    procedure RemoveId(Id: TBoldObjectId);
  protected
    function GetStringRepresentation(representation:integer): String;
    function GetContentAsString: String;
    procedure AssignContentValue(const Source: IBoldValue); override;
    function GetStreamName: String; override;
    procedure ApplyTranslationList(TranslationList: TBoldIdTranslationList); override;
    function GetCount: integer;
  public
    destructor Destroy; override;
    class function ContentType: TBoldValueContentType; override;
    procedure SetFromIdLists(IdList1, IdList2: TBoldObjectIdList);
    property IdList1[Index: integer]: TBoldObjectID read GetIdList1;
    property IdList2[Index: integer]: TBoldObjectID read GetIdList2;
    property Count: integer read GetCount;
  end;

  { TBoldFSObjectContentList }
  TBoldFSObjectContentList = class(TBoldIndexableList)
  private
    class var IX_FSObjectContent: integer;
    function GetItems(index: integer): TBoldFreeStandingObjectContents;
    procedure SetItems(index: integer; const Value: TBoldFreeStandingObjectContents);
  public
    constructor Create;
    function FindObjectContentById(Id: TBoldObjectId): TBoldFreeStandingObjectContents;
    property Items[index: integer]: TBoldFreeStandingObjectContents read GetItems write SetItems; default;
  end;


var
  BoldFreeStandingObjectContentsClass: TBoldFreeStandingObjectContentsClass = TBoldFreeStandingObjectContents;

implementation

uses
  SysUtils,

  BoldCoreConsts,
  BoldUtils,
  BoldIndex,
  BoldHashIndexes,
  BoldFreeStandingValueFactories,
  BoldDefaultStreamNames,
  BoldGuard,
  Variants;

type
  {---TBoldObjectIdHashIndex---}
  TBoldFSObjectContentHashIndex = class(TBoldHashIndex)
  protected
    function ItemAsBoldObjectId(Item: TObject): TBoldObjectId; virtual;
    function HashItem(Item: TObject): Cardinal; override;
    function Match(const Key; Item:TObject):Boolean; override;
    function Hash(const Key): Cardinal; override;
    function FindById(BoldObjectId: TBoldObjectId): TObject;
  end;

const
  sAssignContentValue = 'AssignContentValue';

{---TBoldFreeStandingElement---}

constructor TBoldFreeStandingElement.Create;
begin

  inherited;
end;

function TBoldFreeStandingElement.GetContentType: TBoldValueContentType;
begin
  result := ContentType;
end;

{---TBoldFreeStandingvalueSpace---}

constructor TBoldFreeStandingvalueSpace.Create;
begin
  inherited;
  fObjectContentsList := TBoldFSObjectContentList.create;
  fIdLIst := TBoldObjectIdList.Create;
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

function TBoldFSObjectContentList.FindObjectContentById(Id: TBoldObjectId): TBoldFreeStandingObjectContents;
begin
  Result := TBoldFreeStandingObjectContents(TBoldHashIndex(indexes[IX_FSObjectContent]).Find(ID));
end;

function TBoldFreeStandingvalueSpace.GetFSObjectContentsByObjectId(ObjectId: TBoldObjectId): TBoldFreeStandingObjectContents;
begin
  result := fObjectContentsList.FindObjectContentById(ObjectId);
end;

procedure TBoldFreeStandingValueSpace.RemoveFSObjectContents(
  ObjectContents: TBoldFreeStandingObjectContents);
begin
  fObjectContentsList.Remove(ObjectContents);
end;

procedure TBoldFreeStandingvalueSpace.RemoveFSObjectContentsByObjectId(ObjectId: TBoldObjectId);
begin
  fObjectContentsList.Remove(GetFSObjectContentsByObjectId(ObjectId));
end;

function TBoldFreeStandingvalueSpace.GetObjectContentsByObjectId(ObjectId: TBoldObjectId): IBoldObjectContents;
begin
  result := GetFSObjectContentsByObjectId(ObjectId);
end;

function TBoldFreeStandingvalueSpace.GetEnsuredObjectContentsByObjectId(ObjectId: TBoldObjectId): IBoldObjectContents;
begin
  Result := GetEnsuredFSObjectContentsByObjectId(ObjectId);
end;

function TBoldFreeStandingValueSpace.GetEnsuredObjectContentsByObjectIdAndCheckIfCreated(
  ObjectId: TBoldObjectId;
  out aBoldObjectContents: IBoldObjectContents): boolean;
begin
  aBoldObjectContents := GetEnsuredFSObjectContentsByObjectId(ObjectId, result);
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
    if IdTranslationList.OldIds[i] <> nil then
      fTranslationList.AddTranslation(IdTranslationList.OldIds[i], IdTranslationList.NewIds[i]);
  for i := 0 to fObjectContentsList.Count - 1 do
    DoOneObject(i, TBoldFreeStandingObjectContents(fObjectContentsList[i]));
end;

procedure TBoldFreeStandingvalueSpace.ApplyValueSpace(const ValueSpace: IBoldValueSpace; IgnorePersistenceState: Boolean);
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

function TBoldFreeStandingValueSpace.AssertLinkIntegrity: Boolean;
var
  i: Integer;
  anIdList: TBoldObjectIdList;
  ObjectId: TBoldObjectId;
  ObjectContents: TBoldFreeStandingObjectContents;
  g: IBoldGuard;
begin
  g := TBoldGuard.Create(anIDList);
  anIdList := TBoldObjectIdList.Create;
  AllObjectIds(anIdList, true);
  for i := 0 to anIdList.Count - 1 do
  begin
    ObjectId := anIdList[i];
    ObjectContents := GetFSObjectContentsByObjectId(ObjectId);
    if assigned(ObjectContents) then
    begin
      Assert(ObjectContents.GetObjectId.AsString <> '');
    end;
  end;
  Result := true;
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
  for i := 0 to Length(fMemberList) - 1 do
    FreeAndNil(fMemberList[i]);
  inherited;
end;


function TBoldFreeStandingObjectContents.GetValueByMemberId(MemberId: TBoldMemberID):IBoldValue;
begin
  result := GetValueByIndex(MemberId.MemberIndex);
end;

function TBoldFreeStandingObjectContents.IsEmpty: boolean;
var
  i: integer;
begin
  result := true;
  for i := Length(fMemberList) - 1 downto 0 do
    if Assigned(fMemberList[i]) then
    begin
      result := false;
      exit;
    end;
end;

function TBoldFreeStandingObjectContents.GetStreamName: string;
begin
  result := BOLDOBJECTCONTENTSNAME;
end;

procedure TBoldFreeStandingObjectContents.EnsureMember(MemberId: TBoldMemberId; const StreamName: string);
begin
  EnsureMemberAndGetValueByIndex( MemberId.MemberIndex, StreamName );
end;

function TBoldFreeStandingObjectContents.GetValueByIndex(I: Integer): IBoldValue;
begin
  if i < Length(fMemberList) then
    result := fMemberList[i]
  else
    result := nil;
end;

function TBoldFreeStandingObjectContents.GetFSValueByIndex(i: integer): TBoldFreeStandingValue;
begin
  if i < MemberCount then
    result := fMemberList[i]
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
end;

{---TBoldFreeStandingNullableValue---}

procedure TBoldFreeStandingNullableValue.SetContentToNull;
begin
  fIsNull := true;
end;

function TBoldFreeStandingNullableValue.GetContentAsString: String;
begin
  result := GetStringRepresentation(brDefault);
end;

function TBoldFreeStandingNullableValue.GetContentIsNull: Boolean;
begin
  result := fIsNull;
end;

function TBoldFreeStandingNullableValue.IsEqualToValue(const Value: IBoldValue):
    Boolean;
var
  aNullableValue: IBoldNullableValue;
begin
  Result := Assigned(Value) and
      (Value.QueryInterface(IBoldNullableValue, aNullableValue) = S_OK) and
      (fIsNull = aNullableValue.IsNull);
end;

function TBoldFreeStandingNullableValue.GetStringRepresentation(
  representation: integer): String;
begin
  if GetContentIsNull then
    result := ''
  else
    result := VarToStr(GetValueAsVariant);
end;

procedure TBoldFreeStandingNullableValue.SetToNonNull;
begin
  fIsNull := false;
end;

function TBoldFreeStandingNullableValue.GetAsVariant;
begin
  if GetContentIsNull then
    result := Null
  else
    result := GetValueAsVariant;
end;

{---TBFSInteger---}

function TBFSInteger.GetStreamName: string;
begin
  result := BoldContentName_Integer;
end;

function TBFSInteger.GetValueAsVariant: Variant;
begin
  result := AsInteger;
end;

function TBFSInteger.GetContentAsInteger: Integer;
begin
  result := fDataValue;
end;

class function TBFSInteger.ContentType: TBoldValueContentType;
begin
  result := bctInteger;
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

function TBFSString.GetValueAsVariant: Variant;
begin
  result := AsString;
end;

function TBFSString.GetContentAsString: String;
begin
  result := fDataValue;
end;

class function TBFSString.ContentType: TBoldValueContentType;
begin
  result := bctString;
end;

procedure TBFSString.SetContentAsString(const NewValue: String);
begin
  fDataValue := NewValue; // Use BoldSharedStringManager ?
  SetToNonNull;
end;

procedure TBFSString.AssignContentValue(const Source: IBoldValue);
var
  s: IBoldStringContent;
begin
  if source.QueryInterface(IBoldStringContent, S) = S_OK then
    if s.IsNull then
      SetContentToNull
    else
      SetContentAsString(s.AsString)
  else
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname, sAssignContentValue]);
end;

function TBFSString.IsEqualToValue(const Value: IBoldValue): Boolean;
var
  aStringValue: IBoldStringContent;
begin
  Result := inherited IsEqualToValue(Value) and
      (Value.QueryInterface(IBoldStringContent, aStringValue) = S_OK) and
      (AsString = aStringValue.asString);
end;

{ TBFSAnsiString }

procedure TBFSAnsiString.AssignContentValue(const Source: IBoldValue);
var
  s: IBoldAnsiStringContent;
begin
  if source.QueryInterface(IBoldAnsiStringContent, S) = S_OK then
    if s.IsNull then
      SetContentToNull
    else
      SetContentAsAnsiString(s.asAnsiString)
  else
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname, sAssignContentValue]);
end;

class function TBFSAnsiString.ContentType: TBoldValueContentType;
begin
  result := bctAnsiString;
end;

function TBFSAnsiString.GetContentAsAnsiString: TBoldAnsiString;
begin
  result := fDataValue;
end;

function TBFSAnsiString.GetContentAsString: String;
begin
  result := String(fDataValue);
end;

function TBFSAnsiString.GetStreamName: string;
begin
  result := BoldContentName_AnsiString;
end;

function TBFSAnsiString.GetValueAsVariant: Variant;
begin
  result := AsAnsiString;
end;

function TBFSAnsiString.IsEqualToValue(const Value: IBoldValue): Boolean;
var
  aAnsiStringValue: IBoldAnsiStringContent;
begin
  Result := inherited IsEqualToValue(Value) and
      (Value.QueryInterface(IBoldAnsiStringContent, aAnsiStringValue) = S_OK) and
      (AsAnsiString = aAnsiStringValue.asAnsiString);
end;

procedure TBFSAnsiString.SetContentAsAnsiString(const NewValue: TBoldAnsiString);
begin
  fDataValue := NewValue; // Use BoldSharedStringManager ?
  SetToNonNull;
end;

procedure TBFSAnsiString.SetContentAsString(const NewValue: String);
begin
  fDataValue := AnsiString(NewValue); // Use BoldSharedStringManager ?
  SetToNonNull;
end;

{ TBFSUnicodeString }

procedure TBFSUnicodeString.AssignContentValue(const Source: IBoldValue);
var
  s: IBoldUnicodeStringContent;
begin
  if source.QueryInterface(IBoldUnicodeStringContent, S) = S_OK then
    if s.IsNull then
      SetContentToNull
    else
      SetContentAsUnicodeString(s.asUnicodeString)
  else
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname, sAssignContentValue]);
end;

class function TBFSUnicodeString.ContentType: TBoldValueContentType;
begin
  result := bctUnicodeString;
end;

function TBFSUnicodeString.GetContentAsString: String;
begin
  result := fDataValue;
end;

function TBFSUnicodeString.GetContentAsUnicodeString: TBoldUnicodeString;
begin
  result := fDataValue;
end;

function TBFSUnicodeString.GetStreamName: string;
begin
  result := BoldContentName_UnicodeString;
end;

function TBFSUnicodeString.GetValueAsVariant: Variant;
begin
  result := AsUnicodeString;
end;

function TBFSUnicodeString.IsEqualToValue(const Value: IBoldValue): Boolean;
var
  aUnicodeStringValue: IBoldUnicodeStringContent;
begin
  Result := inherited IsEqualToValue(Value) and
      (Value.QueryInterface(IBoldUnicodeStringContent, aUnicodeStringValue) = S_OK) and
      (AsUnicodeString = aUnicodeStringValue.asUnicodeString);
end;

procedure TBFSUnicodeString.SetContentAsString(const NewValue: String);
begin
  fDataValue := NewValue; // Use BoldSharedStringManager ?
  SetToNonNull;
end;

procedure TBFSUnicodeString.SetContentAsUnicodeString(
  const NewValue: TBoldUnicodeString);
begin
  fDataValue := NewValue; // Use BoldSharedStringManager ?
  SetToNonNull;
end;

{---TBFSCurrency---}

function TBFSCurrency.GetStreamName: String;
begin
  result := BoldContentName_Currency;
end;

function TBFSCurrency.GetValueAsVariant: Variant;
begin
  result := AsCurrency;
end;

function TBFSCurrency.GetContentAsCurrency: Currency;
begin
  result := fDataValue;
end;

class function TBFSCurrency.ContentType: TBoldValueContentType;
begin
  result := bctCurrency;
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

function TBFSFloat.GetValueAsVariant: Variant;
begin
  result := AsFloat;
end;

function TBFSFloat.GetContentAsFloat: double;
begin
  result := fDataValue;
end;

class function TBFSFloat.ContentType: TBoldValueContentType;
begin
  result := bctFloat;
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

function TBFSBoolean.GetValueAsVariant: Variant;
begin
  result := AsBoolean;
end;

function TBFSBoolean.GetContentAsBoolean: Boolean;
begin
  Result := fDataValue;
end;

class function TBFSBoolean.ContentType: TBoldValueContentType;
begin
  result := bctBoolean;
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

function TBFSBlobAbstract.GetValueAsVariant: Variant;
begin
  result := AsBlob;
end;

function TBFSBlobAbstract.GetContentAsBlob: TBoldAnsiString;
begin
  Result := fDataValue;
end;

procedure TBFSBlobAbstract.SetContentAsBlob(const NewValue: TBoldAnsiString);
begin
  fDataValue := NewValue;
  SetToNonNull;
end;

{---TBFSTypedBlob---}

function TBFSTypedBlob.GetStreamName: String;
begin
  Result := BoldContentName_TypedBlob;
end;

class function TBFSTypedBlob.ContentType: TBoldValueContentType;
begin
  result := bctTypedBlob;
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

procedure TBFSObjectIdRef.SetFromId(Id: TBoldObjectId; Adopt: Boolean);
var
  Assign: Boolean;
begin
  Assign := false;
  if assigned(Id) then
  begin
    if (not Assigned(fObjectId)) or (not Id.IsEqual[fObjectId]) then
    begin
      FreeAndNil(fObjectId);
      Assign := True;
    end
  end
  else
    FreeAndNil(fObjectId);
  if Adopt then
  begin
    if Assign then
      fObjectId := Id
    else
      FreeAndNil(Id);
  end
  else
    if Assign then
      fObjectId := Id.Clone;
end;

class function TBFSObjectIDRef.ContentType: TBoldValueContentType;
begin
  result := bctObjectIdRef;
end;

function TBFSObjectIDRef.GetContentAsString: String;
begin
  result := GetStringRepresentation(brDefault);
end;

function TBFSObjectIDRef.GetStringRepresentation(
  representation: integer): String;
begin
  if Assigned(fObjectId) then
    result := fObjectId.AsString
  else
    result := '<nil>';
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

class function TBFSObjectIDRefPair.ContentType: TBoldValueContentType;
begin
  result := bctObjectIdRefPair;
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

function TBFSObjectIdListref.GetStringRepresentation(
  representation: integer): String;
begin
  result := fIdLIst.CommaSeparatedIdList;
end;

procedure TBFSObjectIdListref.SetFromIdList(IdList: TBoldObjectIdList);
begin
  fIdLIst.Free;
  fIdList := IdList.Clone;
end;

procedure TBFSObjectIdListref.SetList(IdList: TBoldObjectIdList);
var
  i: integer;
begin
  IdList.Clear;
  IdList.Capacity := fIdList.Count;
  for i := 0 to fIdList.Count - 1 do
    IdList.Add(fIdList[i]);
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

function TBFSObjectIdListrefPair.GetStringRepresentation(
  representation: integer): String;
begin
  result := fIdList2.CommaSeparatedIdList;
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
  for i := 0 to Length(fMemberList)- 1 do
    if Assigned(fMemberList[i]) then
      fMemberList[i].ApplyTranslationList(TranslationList);
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
  i,j: integer;
begin
  j := fIdList.Count;
  if j = 0 then
    exit;
  resultList.Capacity := j;
  for i := 0 to j - 1 do
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

class function TBFSObjectIdListref.ContentType: TBoldValueContentType;
begin
  result := bctObjectIdListRef;
end;

function TBFSObjectIdListref.GetContentAsString: String;
begin
  result := GetStringRepresentation(brDefault);
end;

function TBFSObjectIdListref.GetCount: integer;
begin
  if assigned(fIdList) then
    result := fIdList.Count
  else
    result := 0;
end;

class function TBFSObjectIdListrefPair.ContentType: TBoldValueContentType;
begin
  result := bctObjectIdListRefPair;
end;

function TBFSObjectIdListrefPair.GetContentAsString: String;
begin
  result := GetStringRepresentation(brDefault);
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

class function TBoldFreeStandingObjectContents.ContentType: TBoldValueContentType;
begin
  result := bctObject;
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

procedure TBoldFreeStandingValue.AssignContent(const Source: IBoldValue);
begin
  AssignContentValue(Source);
  BoldPersistenceState := Source.BoldPersistenceState;
end;

procedure TBFSInteger.AssignContentValue(const Source: IBoldValue);
var
  s: IBoldIntegerContent;
begin
  if source.QueryInterface(IBoldIntegerContent, S) = S_OK then
    if s.IsNull then
      SetContentToNull
    else
      SetContentAsInteger(s.AsInteger)
  else
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname, sAssignContentValue]);
end;

function TBFSInteger.IsEqualToValue(const Value: IBoldValue): Boolean;
var
  aIntegerValue: IBoldIntegerContent;
begin
  Result := inherited IsEqualToValue(Value) and
      (Value.QueryInterface(IBoldIntegerContent, aIntegerValue) = S_OK) and
      (AsInteger = aIntegerValue.asInteger);
end;

procedure TBFSCurrency.AssignContentValue(const Source: IBoldValue);
var
  s: IBoldCurrencyContent;
begin
  if source.QueryInterface(IBoldCurrencyContent, S) = S_OK then
    if s.IsNull then
      SetContentToNull
    else
      SetContentAsCurrency(s.AsCurrency)
  else
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname, sAssignContentValue]);
end;

function TBFSCurrency.IsEqualToValue(const Value: IBoldValue): Boolean;
var
  aCurrencyValue: IBoldCurrencyContent;
begin
  Result := inherited IsEqualToValue(Value) and
      (Value.QueryInterface(IBoldCurrencyContent, aCurrencyValue) = S_OK) and
      (AsCurrency = aCurrencyValue.asCurrency);
end;

procedure TBFSFloat.AssignContentValue(const Source: IBoldValue);
var
  s: IBoldFloatContent;
begin
  if source.QueryInterface(IBoldFloatContent, S) = S_OK then
    if s.IsNull then
      SetContentToNull
    else
      SetContentAsFloat(s.AsFloat)
  else
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname, sAssignContentValue]);
end;

function TBFSFloat.IsEqualToValue(const Value: IBoldValue): Boolean;
var
  aFloatValue: IBoldFloatContent;
begin
  Result := inherited IsEqualToValue(Value) and
      (Value.QueryInterface(IBoldFloatContent, aFloatValue) = S_OK) and
      (AsFloat = aFloatValue.asFloat);
end;

procedure TBFSBoolean.AssignContentValue(const Source: IBoldValue);
var
  s: IBoldBooleanContent;
begin
  if source.QueryInterface(IBoldBooleanContent, S) = S_OK then
    if s.IsNull then
      SetContentToNull
    else
      SetContentAsBoolean(s.AsBoolean)
  else
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname, sAssignContentValue]);
end;

function TBFSBoolean.IsEqualToValue(const Value: IBoldValue): Boolean;
var
  aBooleanValue: IBoldBooleanContent;
begin
  Result := inherited IsEqualToValue(Value) and
      (Value.QueryInterface(IBoldBooleanContent, aBooleanValue) = S_OK) and
      (AsBoolean = aBooleanValue.asBoolean);
end;

{ TBFSDateTime }

procedure TBFSDateTime.AssignContentValue(const Source: IBoldValue);
var
  s: IBoldDateTimeContent;
begin
  if source.QueryInterface(IBoldDateTimeContent, S) = S_OK then
    if s.IsNull then
      SetContentToNull
    else
      SetContentAsDateTime(s.AsDateTime)
  else
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname, sAssignContentValue]);
end;

function TBFSDateTime.GetValueAsVariant: Variant;
begin
  result := AsDateTime;
end;

class function TBFSDateTime.ContentType: TBoldValueContentType;
begin
  result := bctDateTime;
end;

function TBFSDateTime.GetStreamName: String;
begin
  result := BoldContentName_DateTime;
end;

function TBFSDateTime.IsEqualToValue(const Value: IBoldValue): Boolean;
var
  aDateTimeValue: IBoldDateTimeContent;
begin
  Result := inherited IsEqualToValue(Value) and
      (Value.QueryInterface(IBoldDateTimeContent, aDateTimeValue) = S_OK) and
      (AsDateTime = aDateTimeValue.asDateTime);
end;

{ TBFSDate }

procedure TBFSDate.AssignContentValue(const Source: IBoldValue);
var
  s: IBoldDateContent;
begin
  if source.QueryInterface(IBoldDateContent, S) = S_OK then
    if s.IsNull then
      SetContentToNull
    else
      SetContentAsDate(s.AsDate)
  else
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname, sAssignContentValue]);
end;

function TBFSDate.GetValueAsVariant: Variant;
begin
  result := AsDate;
end;

class function TBFSDate.ContentType: TBoldValueContentType;
begin
  result := bctDate;
end;

function TBFSDate.GetStreamName: String;
begin
  result := BoldContentName_Date;
end;

function TBFSDate.IsEqualToValue(const Value: IBoldValue): Boolean;
var
  aDateValue: IBoldDateContent;
begin
  Result := inherited IsEqualToValue(Value) and
      (Value.QueryInterface(IBoldDateContent, aDateValue) = S_OK) and
      (AsDate = aDateValue.asDate);
end;

{ TBFSTime }

procedure TBFSTime.AssignContentValue(const Source: IBoldValue);
var
  s: IBoldTimeContent;
begin
  if source.QueryInterface(IBoldTimeContent, S)= S_OK then
    if s.IsNull then
      SetContentToNull
    else
      SetContentAsTime(s.AsTime)
  else
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname, sAssignContentValue]);
end;

function TBFSTime.GetValueAsVariant: Variant;
begin
  result := AsTime;
end;

class function TBFSTime.ContentType: TBoldValueContentType;
begin
  result := bctTime;
end;

function TBFSTime.GetStreamName: String;
begin
  result := BoldContentName_Time;
end;

function TBFSTime.IsEqualToValue(const Value: IBoldValue): Boolean;
var
  aTimeValue: IBoldTimeContent;
begin
  Result := inherited IsEqualToValue(Value) and
      (Value.QueryInterface(IBoldTimeContent, aTimeValue) = S_OK) and
      (AsTime = aTimeValue.asTime);
end;

{ TBFSBlob }

procedure TBFSBlob.AssignContentValue(const Source: IBoldValue);
var
  s: IBoldBlobContent;
begin
  if source.QueryInterface(IBoldBlobContent, S) = S_OK then
    if s.IsNull then
      SetContentToNull
    else
      SetContentAsBlob(s.AsBlob)
  else
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname, sAssignContentValue]);
end;

class function TBFSBlob.ContentType: TBoldValueContentType;
begin
  result := bctBlob;
end;

function TBFSBlob.IsEqualToValue(const Value: IBoldValue): Boolean;
var
  aBlobValue: IBoldBlobContent;
begin
  Result := inherited IsEqualToValue(Value) and
      (Value.QueryInterface(IBoldBlobContent, aBlobValue) = S_OK) and
      (AsBlob = aBlobValue.asBlob);
end;

procedure TBFSTypedBlob.AssignContentValue(const Source: IBoldValue);
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
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname, sAssignContentValue]);
end;

function TBFSTypedBlob.IsEqualToValue(const Value: IBoldValue): Boolean;
var
  aBlobValue: IBoldBlobContent;
  aTypedBlob: IBoldTypedBlob;
begin
  Result := inherited IsEqualToValue(Value) and
      (Value.QueryInterface(IBoldBlobContent, aBlobValue) = S_OK) and
      (Value.QueryInterface(IBoldTypedBlob, aTypedBlob) = S_OK) and
      (AsBlob = aBlobValue.asBlob) and
      (ContentTypeContent = aTypedBlob.ContentTypeContent);
end;

procedure TBFSObjectIDRef.AssignContentValue(const Source: IBoldValue);
var
  s: IBoldObjectIdRef;
begin
  if source.QueryInterface(IBoldObjectIDRef, S) = S_OK then
  begin
    SetFromId(s.Id, false);
    OrderNo := s.OrderNo;
  end
  else
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname, sAssignContentValue]);
end;

function TBFSObjectIDRef.IsEqualToValue(const Value: IBoldValue): Boolean;
var
  aObjectIDRef: IBoldObjectIdRef;
begin
  Result := Assigned(Value) and
      (Value.QueryInterface(IBoldObjectIdRef, aObjectIDRef) = S_OK) and
      ((Assigned(ID) and Assigned(aObjectIDRef.Id) and Id.IsEqual[aObjectIDRef.Id]) or
       ((ID = nil) and (aObjectIDRef.Id = nil)));
end;

procedure TBFSObjectIDRefPair.AssignContentValue(const Source: IBoldValue);
var
  s: IBoldObjectIdRefPair;
begin
  if source.QueryInterface(IBoldObjectIDRefPair, S) = S_OK then
  begin
    SetFromIds(s.Id1, s.Id2);
    OrderNo := s.OrderNo;
  end
  else
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname, sAssignContentValue]);
end;

procedure TBFSObjectIdListref.EnsureList;
begin
  if not assigned(fIdList) then
    fIdList := TBoldObjectIdList.Create
  else
    Assert(fIdList is TBoldObjectIdList);
end;

procedure TBFSObjectIdListrefPair.EnsureLists;
begin
  if not assigned(fIdList1) then
    fIdList1 := TBoldObjectIdList.Create;
  if not assigned(fIdList2) then
    fIdList2 := TBoldObjectIdList.Create;
end;

procedure TBFSObjectIdListref.AssignContentValue(const Source: IBoldValue);
var
  s: IBoldObjectIdListRef;
begin
  if source.QueryInterface(IBoldObjectIDListRef, S) = S_OK then
  begin
    EnsureList;
    s.SetList(fIdList);
  end
  else
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname, sAssignContentValue]);
end;

procedure TBFSObjectIdListrefPair.AssignContentValue(const Source: IBoldValue);
var
  s: IBoldObjectIdListRefPair;
  i: Integer;
begin
  if source.QueryInterface(IBoldObjectIDListRefPair, S) = S_OK then
  begin
    EnsureLists;
    fIdList1.Clear;
    fIdList2.Clear;
    fIdList1.Capacity := s.Count;
    fIdList2.Capacity := s.Count;
    for i := 0 to s.Count - 1 do
    begin
      fIdList1.Add(s.IdList1[i]);
      fIdList2.Add(s.IdList2[i]);
    end;
  end
  else
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname, sAssignContentValue]);
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
    fMemberList[Index].Free;
    fMemberList[Index] := nil;
  end;
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

{ TBoldFSObjectContentList }

constructor TBoldFSObjectContentList.Create;
begin
  inherited Create;
  SetIndexCapacity(1);
  OwnsEntries := true;
  IX_FSObjectContent := -1;
  SetIndexVariable(IX_FSObjectContent, AddIndex(TBoldFSObjectContentHashIndex.Create));
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

function TBoldFreeStandingValue.GetContentName: String;
begin
  result := GetStreamName;
end;

constructor TBoldFreeStandingValue.create;
begin
  inherited;
  BoldPersistenceState := bvpsInvalid;
end;

procedure TBoldFreeStandingValue.AssignContentValue(const Source: IBoldValue);
begin
  raise EBold.CreateFmt(sAbstractError, [classname, 'AssignContentValue']);
end;

function TBoldFreeStandingValue.IsEqualToValue(const Value: IBoldValue): Boolean;
begin
  raise EBold.CreateFmt(sAbstractError, [classname, 'IsEqualToValue']);
end;

function TBoldFreeStandingObjectContents.GetObjectId: TBoldObjectId;
begin
  result := fObjectId;
end;

procedure TBoldFreeStandingObjectContents.ApplyObjectContents(
  const ObjectContents: IBoldObjectContents; ApplyValues: Boolean; IgnorePersistenceState: Boolean);
var
  i: Integer;
  aValue: IBoldValue;
begin
  SetBoldExistenceState(ObjectContents.BoldExistenceState);
  SetBoldPersistenceState(ObjectContents.BoldPersistenceState);
  if ApplyValues then
    for i := ObjectContents.MemberCount - 1 downto 0 do
    begin
      aValue := ObjectContents.valueByIndex[i];
      if assigned(aValue) then
      begin
        EnsureMemberAndGetValueByIndex(i, aValue.ContentName);
        if IgnorePersistenceState then
          fMemberList[i].AssignContentValue(aValue)
        else
          fMemberList[i].AssignContent(aValue);
      end;
    end;
end;

procedure TBoldFreeStandingValueSpace.UpdateOwnValuesFrom(
  const ValueSpace: IBoldValueSpace);
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
  const ObjectContents: IBoldObjectContents);
var
  i: Integer;
  OwnValue, aValue: IBoldValue;
begin
  SetBoldExistenceState(ObjectContents.BoldExistenceState);
  SetBoldPersistenceState(ObjectContents.BoldPersistenceState);
  for i := MemberCount - 1 downto 0 do
  begin
    OwnValue := GetValueByIndex(i);
    if Assigned(OwnValue) then begin
      aValue := ObjectContents.ValueByIndex[i];
      if Assigned(aValue) then begin
        OwnValue.AssignContent(aValue);
      end;
    end;
  end;
end;

procedure TBoldFreeStandingValueSpace.RemoveAllObjectContents;
{var
 i: integer;
 ObjectIds: TBoldObjectIdlist;}
begin
  fObjectContentsList.Clear;
{
  ObjectIds := TBoldObjectIdList.Create;
  try
    AllObjectIds(ObjectIds, True);
    for i:= 0 to ObjectIds.Count - 1 do
      RemoveFSObjectContentsByObjectId(ObjectIds[i]);
  finally
    FreeAndNil(Objectids);
  end;
}
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
    if Assigned(fMemberList[M]) then
      fMemberList[M].BoldPersistenceState := bvpsCurrent;
end;

procedure TBoldFreeStandingObjectContents.EnsureMemberListLength(
  index: integer);
begin
  if (Index >= Length(fMemberList)) then
    SetLength(fMemberList, Index+1);
end;

procedure TBoldFreeStandingValueSpace.Clear;
begin
  fIdLIst.Clear;
  fObjectContentsList.Clear;
  FreeAndNil(fTranslationList);
end;

procedure TBoldFreeStandingValueSpace.ClearWhenObjectContentsEmpty;
begin
  if fObjectContentsList.Count = 0 then begin
    if Assigned(fIdLIst) then begin
      fIdLIst.Clear;
    end;
    if Assigned(fTranslationList) then begin
      FreeAndNil(fTranslationList);
    end;
  end;
end;

function TBoldFreeStandingObjectContents.EnsureMemberAndGetValueByIndex(
  Index: integer; const StreamName: string): IBoldValue;
begin
  EnsureMemberListLength(Index);
  if not assigned(fMemberlist[Index]) then
    fMemberlist[Index] := TBoldFreeStandingValue(FreeStandingValueFactory.CreateInstance(StreamName));
  Result :=  fMemberlist[Index];
end;

function TBoldFreeStandingValueSpace.GetValueForIdAndMemberIndex(
  Id: TBoldObjectId; MemberIndex: integer): IBoldValue;
var
  ObjectContents: TBoldFreeStandingObjectContents;
begin
  ObjectContents := GetFSObjectContentsByObjectId(Id);
  if Assigned(ObjectContents) then
     Result := ObjectContents.ValueByIndex[MemberIndex]
  else
    result := nil;
end;

class function TBoldFreeStandingValueSpace.ContentType: TBoldValueContentType;
begin
  result := bctValueSpace;
end;

function TBoldFreeStandingValueSpace.GetAnyObjectId: TBoldObjectId;
var
  i: integer;
begin
  result := nil;
  for I := 0 to fIdList.Count -1 do
    if GetObjectContentsByObjectId(fIdList[i]) <> nil then
    begin
      result := fIdList[i];
      exit;
    end;
end;

function TBoldFreeStandingValueSpace.GetEnsuredFSObjectContentsByObjectId(
  ObjectId: TBoldObjectId;
  out aCreated: boolean): TBoldFreeStandingObjectContents;
var
  LocalId: TBoldObjectId;
begin
  Result := GetFSObjectContentsByObjectId(ObjectId);
  aCreated := not Assigned(Result);
  if aCreated then
  begin
    LocalId := IdLIst.IdById[ObjectId];
    if not assigned(LocalId) then
    begin
      LocalId := IdList.AddAndGetID(ObjectId);
    end;
    Result := BoldFreeStandingObjectContentsClass.Create;
    Result.fObjectId := LocalId;
    fObjectContentsList.Add(Result);
  end;
end;

function TBoldFreeStandingValueSpace.GetEnsuredFSObjectContentsByObjectId(
  ObjectId: TBoldObjectId): TBoldFreeStandingObjectContents;
var
  lCreated: boolean;
begin
  result := GetEnsuredFSObjectContentsByObjectId(ObjectId, lCreated);
end;

function TBoldFreeStandingValueSpace.IdCount: integer;
begin
  result := fIdLIst.Count;
end;

function TBoldFreeStandingValueSpace.IsEmpty: boolean;
var
  i: integer;
begin
  for i := 0 to fIdList.Count - 1 do
    if Assigned(GetObjectContentsByObjectId(fIdList[i])) then
    begin
      result := false;
      exit;
    end;
  result := true;
end;

function TBoldFreeStandingObjectContents.GetBoldMemberCount: Integer;
begin
  Result := Length(fMemberList);
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
  p := fIdList.IndexByID[Id];
  if p <> -1 then
    fIdLIst.RemoveByIndex(p);
end;

procedure TBFSObjectIdListrefPair.AddIds(Id1, Id2: TBoldObjectId);
begin
  EnsureLists;
  fIdList1.Add(Id1);
  fIdLIst2.Add(Id2);
end;

procedure TBFSObjectIdListref.AddId(Id: TBoldObjectId);
begin
  EnsureList;
  fIdList.Add(Id);
end;

end.
