{ Global compiler directives }
{$include bold.inc}
unit BoldId;

interface

uses
  BoldDefs,
  BoldBase,
  BoldStreams,
  BoldIndex,
  BoldXMLStreaming,
  BoldIndexableList;

type
  {---forward declarations---}
  TBoldID = class;
  TBoldMemberID = class;
  TBoldObjectId = class;
  TBoldInternalObjectId = class;
  TBoldIdList = class;
  TBoldObjectIdList = class;
  TBoldMemberIdList = class;
  TBoldIDTranslationList = class;

  TBoldObjectIdClass = Class of TBoldObjectId;

  {---TBoldID---}
  TBoldID = class (TBoldNonRefCountedObject, IBoldStreamable)
  protected
    function GetStreamName: string; virtual; abstract;
    function GetAsString: string; virtual; abstract;
    function GetDebugInfo: string; override;    
  public
    property AsString: string read GetAsString;
  end;

  {---TBoldMemberID---}
  TBoldMemberID = class(TBoldID)
  private
    fMemberIndex: Integer;
  protected
    function GetStreamName: string; override;
    function GetAsString: string; override;
  public
    constructor Create(MemberIndex: Integer);
    function Clone: TBoldMemberID;
    property MemberIndex: integer read fMemberIndex;
  end;

  {---TBoldSubMemberID---}
  TBoldSubMemberID = class(TBoldmemberID)
  private
    fOwnsPartof: Boolean;
    fPartOf: TBoldMemberID;
  public
    constructor create(partOf: TBoldMemberID; OwnspartOf: Boolean; IndexInMemberList: integer);
    destructor Destroy; override;
  end;

  {---TBoldObjectId---}
  TBoldObjectId = class(TBoldID)
  private
    FClassId: integer;
    function GetTopSortedIndex: integer;
    function GetTopSortedIndexExact: Boolean;
    procedure SetClassIdData(TopSortedIndex: integer; Exact: boolean);
  protected
    function GetIsStorable: Boolean; virtual; abstract;
    function GetHash: Cardinal; virtual; abstract;
    Function GetIsEqual(MatchID: TBoldObjectID): Boolean; virtual; abstract;
    function GetTimeStamp: TBoldTimeStampType; virtual;
    function GetNonExisting: Boolean; virtual;
  public
    constructor CreateWithClassID(TopSortedIndex: integer; Exact: Boolean); virtual;
    function CloneWithClassId(TopSortedIndex: integer; Exact: Boolean): TBoldObjectid; virtual; abstract;
    function Clone: TBoldObjectId;
    property TopSortedIndex: integer read getTopSortedIndex;
    property TopSortedIndexExact: Boolean read GetTopSortedIndexExact;
    property IsStorable: Boolean read GetIsStorable;
    property IsEqual[MatchId: TBoldObjectId]: Boolean read GetIsEqual;
    property NonExisting: Boolean read GetNonExisting;
    property Hash: Cardinal read GetHash;
    property TimeStamp: TBoldTimeStampType read GetTimeStamp;
  end;

  {---TBoldInternalObjectId---}
  TBoldInternalObjectId = class(TBoldObjectId)
  private
    fInternalIdentifier: integer;
  protected
    function GetAsString: string; override;
    function GetHash: cardinal; override;
    function GetIsStorable: Boolean; override;
    function GetIsEqual(MatchID: TBoldObjectID): Boolean; override;
    function GetStreamName: string; override;
  public
    constructor CreateWithClassID(TopSortedIndex: integer; Exact: Boolean); override;
    constructor CreateWithClassIDandInternalId(InternalIdentifier: integer; TopSortedIndex: integer; Exact: Boolean);
    function CloneWithClassId(TopSortedIndex: integer; Exact: Boolean):TBoldObjectid; override;
  end;

  {---TBoldExternalObjectID---}
  TBoldExternalObjectID = class(TBoldObjectId)
  protected
    function GetIsStorable: Boolean; override;
  end;

  TBoldNonExistingObjectId = class(TBoldObjectId)
  protected
    function GetStreamName: string; override;
    function GetAsString: string; override;
    function GetNonExisting: Boolean; override;
    function GetIsStorable: Boolean; override;
    function GetHash: Cardinal; override;
    function GetIsEqual(MatchID: TBoldObjectID): Boolean; override;
  public
    function CloneWithClassId(TopSortedIndex: integer; Exact: Boolean): TBoldObjectid; override;
  end;

  {---TBoldIdList---}
  TBoldIdList = class(TBoldIndexableList)
  public
    function CommaSeparatedIdList: String;
    property AsString: string read CommaSeparatedIdList;
  end;

  {---TBoldObjectIdHashIndex---}
  TBoldObjectIdHashIndex = class(TBoldHashIndex)
  protected
    function ItemAsBoldObjectId(Item: TObject): TboldObjectId; virtual;
    function HashItem(Item: TObject): Cardinal; override;
    function Match(const Key; Item:TObject):Boolean; override;
    function Hash(const Key): Cardinal; override;
    function FindById(BoldObjectId: TBoldObjectId): TObject;
  end;

  {---TBoldObjectIdList---}
  TBoldObjectIdList = class(TBoldIdList, IBoldStreamable)
  private
    class var IX_ObjectID: integer;
    function GetObjectIDIndex: TBoldObjectIDHashIndex;
    function GetHasInexactIds: boolean;
    function GetHasNonExistingIds: boolean;
    function GetFirst: TBoldObjectId;
    function GetLast: TBoldObjectId;
  protected
    function GetCount: integer;
    function GetIDByID(ObjectID: TBoldObjectId): TBoldObjectId;
    function GetIndexByID(ObjectID: TBoldObjectId): Integer;
    function GetObjectId(index: Integer): TBoldObjectId;
    function GetIdInList(ObjectID: TBoldObjectId): Boolean;
    function GetStreamName: string;
    property ObjectIDIndex: TBoldObjectIDHashIndex read GetObjectIDIndex;
  public
    constructor Create;
    procedure Add(ObjectID: TBoldObjectId);
    procedure AddAndAdopt(ObjectID: TBoldObjectId);
    procedure AddIfNotInList(ObjectID: TBoldObjectId);
    procedure AddList(ObjectIdList: TBoldObjectIdList);
    function AddAndGetID(aBoldObjectId: TBoldObjectId): TBoldObjectId;
    procedure Insert(Index: integer; ObjectID: TBoldObjectId);
    function ContainsSameIDs(List: TBoldObjectIdList): Boolean;
    function Clone: TBoldObjectIdList;
    procedure Remove(Id: TBoldObjectId);
    procedure ReplaceID(OldId, NewId: TBoldObjectId);
    procedure ExactifyIds(TranslationList: TBoldIDTranslationList);
    procedure ApplyTranslationList(TranslationList: TBoldIdTranslationList);
    procedure RemoveNonExistingIds;
    property IDByID[ObjectID: TBoldObjectId]: TBoldObjectId read GetIdById;
    property IndexByID[ObjectID: TBoldObjectId]: Integer read GetIndexByID;
    property ObjectIds[index: Integer]: TBoldObjectId read GetObjectId; default;
    property IdInList[Objectid: TBoldObjectId]: Boolean read GetIdInList;
    property HasInexactIds: boolean read GetHasInexactIds;
    property HasNonExistingIds: boolean read GetHasNonExistingIds;
    property First: TBoldObjectId read GetFirst;
    property Last: TBoldObjectId read GetLast;
  end;

  {---TBoldMemberIdList---}
  TBoldMemberIdList = class(TBoldIdList, IBoldStreamable)
    function GetStreamName: string;
  protected
    function GetMemberIds(Index: Integer): TBoldMemberId;
  public
    function IsEqual(AList: TBoldMemberIdList): boolean;
    function HasId(AId: TBoldMemberId): boolean;
    function Clone: TBoldMemberIdList;
    property MemberIds[Index: integer]: TBoldMemberId read GetMemberIds; default;
  end;

  {---TBoldIDTranslationList---}
  TBoldIDTranslationList = class(TBoldNonRefCountedObject, IBoldStreamable)
  private
    fOldIds: TBoldObjectIdList;
    fNewIds: TBoldObjectIdList;
    function GetOldId(index: Integer): TBoldObjectId;
    function GetNewId(index: Integer): TBoldObjectId;
    function GetCount: Integer;
    function GetTranslateToOldId(NewID: TBoldObjectId): TBoldObjectId; overload;
    function GetTranslateToNewId(OldID: TBoldObjectId): TBoldObjectId; overload;
//    function GetTranslateToOldId(Index: integer): TBoldObjectId; overload;
//    function GetTranslateToNewId(Index: integer): TBoldObjectId; overload;
    function GetCapacity: Integer;
    procedure SetCapacity(const Value: Integer);
  protected
    function GetStreamName: string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddTranslation(OldId, NewId: TBoldObjectId);
    procedure AddTranslationAdoptNew(OldId, NewId: TBoldObjectId);
    procedure AddTranslationAdoptBoth(OldId, NewId: TBoldObjectId);
    property Count: Integer read GetCount;
    property TranslateToOldId[NewID: TBoldObjectId]: TBoldObjectId read GetTranslateToOldId;
    property TranslateToNewId[OldID: TBoldObjectId]: TBoldObjectId read GetTranslateToNewId;
    property OldIds[Index: integer] :TBoldObjectId read GetOldId;
    property NewIds[Index: integer] :TBoldObjectId read GetNewId;
    property Capacity: Integer read GetCapacity write SetCapacity;
  end;

  { EBoldOperationFailedForIdList }
  EBoldOperationFailedForIdList = class(EBold)
  private
    fIdList: TBoldObjectIdList;
  public
    constructor Create(msg: string; args: array of const; IdList: TBoldObjectIdList);
    destructor Destroy; override;
    property IdList: TBoldObjectIdList read fIdList;
  end;

  { TBoldXMLObjectIdStreamer }
  TBoldXMLObjectIdStreamer = class(TBoldXMLObjectStreamer)
  public
    procedure WriteObject(Obj: TBoldInterfacedObject; Node: TBoldXMLNode); override;
    procedure ReadObject(Obj: TObject; Node: TBoldXMLNode); override;
  end;

implementation

uses
  {$IFDEF OXML}OXmlPDOM{$ELSE}Bold_MSXML_TLB{$ENDIF},
  SysUtils,
  {$IFNDEF BOLD_UNICODE}
  StringBuilder,
  {$ENDIF}
  BoldDefaultXMLStreaming,
  BoldDefaultStreamNames,
  BoldMeta;

var
  InternalIdCounter: Integer = 0;

const
  TOPSORTEDINDEXTMASK = $0FFFFFFF;
  CLASSIDEXACTMASK = $10000000;


type
  { TBoldXMLInternalObjectIdStreamer }
  TBoldXMLInternalObjectIdStreamer = class(TBoldXMLObjectIdStreamer)
  protected
    function GetStreamName: string; override;
  public
    procedure WriteObject(Obj: TBoldInterfacedObject; Node: TBoldXMLNode); override;
    procedure ReadObject(Obj: TObject; Node: TBoldXMLNode); override;
    function CreateObject: TObject; override;
  end;

  { TBoldXMLObjectIdListStreamer }
  TBoldXMLObjectIdListStreamer = class(TBoldXMLObjectStreamer)
  protected
    function GetStreamName: string; override;
  public
    procedure WriteObject(Obj: TBoldInterfacedObject; Node: TBoldXMLNode); override;
    procedure ReadObject(Obj: TObject; Node: TBoldXMLNode); override;
    function CreateObject: TObject; override;
  end;

  { TBoldXMLTranslationListStreamer }
  TBoldXMLTranslationListStreamer = class(TBoldXMLObjectStreamer)
  protected
    function GetStreamName: string; override;
  public
    procedure WriteObject(Obj: TBoldInterfacedObject; Node: TBoldXMLNode); override;
    procedure ReadObject(Obj: TObject; Node: TBoldXMLNode); override;
    function CreateObject: TObject; override;
  end;

  { TBoldXMLMemberIdStreamer }
  TBoldXMLMemberIdStreamer = class(TBoldXMLObjectStreamer)
  protected
    function GetStreamName: string; override;
  public
    procedure WriteObject(Obj: TBoldInterfacedObject; Node: TBoldXMLNode); override;
    procedure ReadObject(Obj: TObject; Node: TBoldXMLNode); override;
    function CreateObject: TObject; override;
  end;

  { TBoldXMLMemberIdListStreamer }
  TBoldXMLMemberIdListStreamer = class(TBoldXMLObjectStreamer)
  protected
    function GetStreamName: string; override;
  public
    procedure WriteObject(Obj: TBoldInterfacedObject; Node: TBoldXMLNode); override;
    procedure ReadObject(Obj: TObject; Node: TBoldXMLNode); override;
    function CreateObject: TObject; override;
  end;

{---TBoldObjectId---}

function TBoldObjectId.getTopSortedIndex: integer;
const
  TOPSORTEDINDEXTMASK = TOPSORTEDINDEXTMASK; // copied here for inlining
begin
  result := fClassId and TOPSORTEDINDEXTMASK;
end;

function TBoldObjectId.GetTopSortedIndexExact: Boolean;
const
  CLASSIDEXACTMASK = CLASSIDEXACTMASK; // copied here for inlining
begin
  result := (fClassId and CLASSIDEXACTMASK) = CLASSIDEXACTMASK;
end;

procedure TBoldObjectId.SetClassIdData(TopSortedIndex: integer; Exact: boolean);
const
  CLASSIDEXACTMASK = CLASSIDEXACTMASK; // copied here for inlining
begin
  if exact then
    fClassId := TopSortedIndex or CLASSIDEXACTMASK
  else
    fClassId := TopSortedIndex;
end;

{---TBoldObjectIdHashIndex---}

function TBoldObjectIdHashIndex.ItemAsBoldObjectId(Item: TObject): TboldObjectId;
begin
  Assert(Item is TBoldObjectId);
  Result := TBoldObjectId(Item);
end;

function TBoldObjectIdHashIndex.HashItem(Item: TObject): Cardinal;
begin
  Result := ItemAsBoldObjectId(Item).Hash;
end;

function TBoldObjectIdHashIndex.Match(const Key; Item:TObject):Boolean;
begin
  Result := TBoldObjectId(Key).IsEqual[ItemAsBoldObjectId(Item)];
end;

function TBoldObjectIdHashIndex.FindById(BoldObjectId: TBoldObjectId): TObject;
begin
  Result := Find(BoldObjectId);
end;

{---TBoldMemberID---}

function TBoldMemberID.Clone: TBoldMemberID;
begin
  result := TBoldMemberID.Create(MemberIndex);
end;

constructor TBoldMemberId.Create(MemberIndex: Integer);
begin
  fMemberIndex := MemberIndex;
end;

function TBoldMemberID.GetAsString: string;
begin
  result := IntToStr(fMemberIndex);
end;

function TBoldMemberId.GetStreamName: string;
begin
  result := BOLDMEMBERIDNAME;
end;

{---TBoldSubMemberID---}

constructor TBoldSubMemberId.create(partOf: TBoldMemberID; OwnsPartOf: Boolean; IndexInMemberList: integer);
begin
  Inherited Create(IndexInMemberList);
  fPartOf := PartOf;
  fOwnsPartOf := OwnsPartOf;
end;

destructor TBoldSubMemberId.Destroy;
begin
  if fOwnsPartOf then
  begin
    fPartOf.Free;
    fPartOf := nil;
  end;
  inherited;
end;

{---TBoldObjectId---}

function TBoldObjectId.Clone: TBoldObjectId;
begin
  result := CloneWithClassId(TopSortedIndex, TopSortedIndexExact);
end;

constructor TBoldObjectId.CreateWithClassID(TopSortedIndex: integer; Exact: Boolean);
begin
  inherited Create;
  SetClassIdData(TopSortedIndex, Exact);
end;

{---TBoldInternalObjectId---}

constructor TBoldInternalObjectID.CreateWithClassIDandInternalId(InternalIdentifier: integer; TopSortedIndex: integer; Exact: Boolean);
begin
  Inherited CreateWithClassID(TopSortedIndex, Exact);
  fInternalIdentifier := InternalIdentifier;
end;

function TBoldInternalObjectID.CloneWithClassId(TopSortedIndex: integer; Exact: Boolean):TBoldObjectid;
begin
  result := TBoldInternalObjectId.CreateWithClassIDandInternalId(fInternalIdentifier, TopSortedIndex, Exact);
end;

constructor TBoldInternalObjectId.CreateWithClassID(TopSortedIndex: integer; Exact: Boolean);
begin
  inherited CreateWithClassID(TopSortedIndex, Exact);
  fInternalIdentifier := InternalIdCounter;
  Inc(InternalIdCounter);
  if fInternalIdentifier >= InternalIdCounter then
    InternalIdCounter := fInternalIdentifier + 1;
end;

function TBoldInternalObjectId.GetAsString: string;
begin
  Result := IntToStr(fInternalIdentifier);
end;

function TBoldInternalObjectId.GetIsStorable: Boolean;
begin
  Result := False;
end;

function TBoldInternalObjectID.GetIsEqual(MatchID: TBoldObjectID): Boolean;
begin
  result := Assigned(MatchId) and (MatchID.ClassType = ClassType ) and
            (fInternalIdentifier = TBoldInternalObjectId(MatchID).fInternalIdentifier);
end;

function TBoldInternalObjectID.GetHash: cardinal;
begin
  result := fInternalIdentifier;
end;

function TBoldInternalObjectID.GetStreamName: string;
begin
  result := BOLDINTERNALIDNAME;
end;

{--- TBoldExternalObjectId ---}
function TBoldExternalObjectID.GetIsStorable: Boolean;
begin
  Result := True;
end;

{---TBoldObjectIdList---}
constructor TBoldObjectIdList.Create;
begin
  inherited;
  SetIndexCapacity(1);
  OwnsEntries := true;
end;

function TBoldObjectIdList.GetIdInList(ObjectID: TBoldObjectId): Boolean;
begin
  Result := Assigned(ObjectId) and Assigned(GetIdByID(ObjectID));
end;

function TBoldObjectIdList.ContainsSameIDs(List: TBoldObjectIdList): Boolean;
var
  i: integer;
begin
  result := List.Count = Count;
  if result then
  begin
    for i := 0 to List.Count-1 do
      result := result and IdInList[LIst[i]];
  end;
end;

function TBoldObjectIdList.GetObjectIDIndex: TBoldObjectIDHashIndex;
begin
  if UnorderedIndexCount = 0 then
  begin
    IX_ObjectID := -1;
    SetIndexVariable(IX_ObjectID, AddIndex(TBoldObjectIDHashIndex.Create));
  end;
  result := TBoldObjectIDHashIndex(Indexes[IX_ObjectID]);
end;

function TBoldObjectIdList.GetHasInexactIds: boolean;
var
  i: integer;
begin
  result := false;
  for i := 0 to Count - 1 do
    if not TBoldObjectId(Items[i]).TopSortedIndexExact then
    begin
      result := true;
      exit;
    end;
end;

function TBoldObjectIdList.GetHasNonExistingIds: boolean;
var
  i: integer;
begin
  result := false;
  for i := 0 to Count - 1 do
    if TBoldObjectId(Items[i]).NonExisting then
    begin
      result := true;
      exit;
    end;
end;

function TBoldObjectIdList.GetIDByID(ObjectID: TBoldObjectId): TBoldObjectId;
var
  i: integer;
begin
  if Count < 10 then
  begin
    Result := nil;
    for i := 0 to Count - 1 do
      if ObjectId.IsEqual[TBoldObjectId(Items[i])] then
      begin
        Result := TBoldObjectId(Items[i]);
        Break;
      end;
  end
  else
    Result := TBoldObjectId(ObjectIDIndex.FindByID(ObjectID));
end;

function TBoldObjectIdList.GetIndexByID(ObjectID: TBoldObjectId): Integer;
var
  InternalId: TBoldObjectId;
  i: integer;
begin
  result := -1;
  if Count < 10 then
  begin
    for i := 0 to Count - 1 do
      if ObjectId.IsEqual[TBoldObjectId(Items[i])] then
      begin
        Result := i;
        exit;
      end;
  end
  else
  begin
    InternalId := TBoldObjectId(ObjectIDIndex.FindByID(ObjectID));;
    if assigned(InternalId) then
      Result := IndexOf(InternalId);
  end;
end;

function TBoldObjectIdList.GetObjectId(index: Integer): TBoldObjectId;
begin
  Result := TBoldObjectId(Items[index]);
end;

function TBoldObjectIdList.GetLast: TBoldObjectId;
begin
  if IsEmpty then
    result := nil
  else
    result := GetObjectID(Count-1);
end;

procedure TBoldObjectIdList.Add(ObjectID: TBoldObjectId);
begin
  if assigned(ObjectID) then
    inherited Add(ObjectId.Clone)
  else
    inherited Add(nil);
end;

procedure TBoldObjectIdList.AddAndAdopt(ObjectID: TBoldObjectId);
begin
  inherited Add(ObjectId);
end;

function TBoldObjectIdList.AddAndGetID(
  aBoldObjectId: TBoldObjectId): TBoldObjectId;
begin
  if assigned(aBoldObjectId) then
  begin
    result := aBoldObjectId.Clone
  end else
  begin
    result := nil;
  end;
  inherited Add(result);
end;

procedure TBoldObjectIdList.AddIfNotInList(ObjectID: TBoldObjectId);
begin
  if not IdInList[Objectid] then
    Add(ObjectId);
end;

procedure TBoldObjectIdList.Insert(Index: integer;
  ObjectID: TBoldObjectId);
begin
  if assigned(ObjectID) then
    inherited insert(index, ObjectId.Clone)
  else
    inherited insert(index, nil);
end;

procedure TBoldObjectIdList.ReplaceID(OldId, NewId: TBoldObjectId);
var
  OldIndex: Integer;
  tempId: TBoldObjectId;
begin
  if OldId = NewId then
    exit;
  TempID := IDByID[OldId];
  OldIndex := IndexOf(tempId);
  Items[OldIndex] := NewId.Clone;
end;

function TBoldObjectIdList.GetCount: integer;
begin
  result := count;
end;

function TBoldObjectIdList.GetFirst: TBoldObjectId;
begin
  if IsEmpty then
    result := nil
  else
    result := GetObjectID(0);
end;

function TBoldObjectIdList.Clone: TBoldObjectIdList;
var
  i: Integer;
begin
  Result := TBoldObjectIdList.Create;
  Result.Capacity := Count;
  for i := 0 to Count - 1 do
    Result.Add(ObjectIDs[i]);
end;

function TBoldObjectIdList.GetStreamName: string;
begin
  result := BOLDOBJECTIDLISTNAME;
end;

{---TBoldMemberIdList---}

function TBoldMemberIdList.GetStreamName: string;
begin
  result := BOLDMEMBERIDLISTNAME;
end;

function TBoldMemberIdList.GetMemberIds(Index: Integer): TBoldMemberId;
begin
  result := TBoldmemberId(Items[Index]);
  Assert(result is TBoldmemberId);
end;

function TBoldMemberIdList.HasId(AId: TBoldMemberId): boolean;
var
  i: integer;
begin
  for I := 0 to Count - 1 do
    if MemberIds[i].MemberIndex = AId.MemberIndex then
    begin
      result := true;
      exit;
    end;
  result := false;
end;

function TBoldMemberIdList.IsEqual(AList: TBoldMemberIdList): boolean;
var
  i: integer;
begin
  result := Assigned(AList) and (AList.Count = Count);
  if result then
  for I := 0 to Count - 1 do
  begin
    if not AList.HasId(MemberIds[i]) then
    begin
      result := false;
      exit;
    end;
  end;
end;

function TBoldMemberIdList.Clone: TBoldMemberIdList;
var
  i: integer;
begin
  result := TBoldMemberIdList.Create;
  for I := 0 to Count - 1 do
    Result.Add(MemberIds[i].clone);
end;

{---TBoldIDTranslationList---}

function TBoldIDTranslationList.GetCapacity: Integer;
begin
  result := fOldIds.Capacity;
end;

function TBoldIDTranslationList.GetCount: Integer;
begin
  Result := fOldIds.Count;
end;

procedure TBoldIDTranslationList.AddTranslationAdoptNew(OldId,
  NewId: TBoldObjectId);
begin
  fOldIDs.Add(OldId);
  fNewIds.AddAndAdopt(NewId)
end;

procedure TBoldIDTranslationList.AddTranslationAdoptBoth(OldId,
  NewId: TBoldObjectId);
begin
  fOldIDs.AddAndAdopt(OldId);
  fNewIds.AddAndAdopt(NewId)
end;

constructor TBoldIDTranslationList.Create;
begin
  fOldIds := TBoldObjectIdList.Create;
  fNewIds := TBoldObjectIdList.Create;
end;

destructor TBoldIDTranslationList.Destroy;
begin
  FreeAndNil(fOldIds);
  FreeAndNil(fNewIds);
  inherited;
end;

function TBoldIDTranslationList.GetOldId(index: Integer): TBoldObjectId;
begin
  Result := fOldIds[index];
end;

function TBoldIDTranslationList.GetNewId(index: Integer): TBoldObjectId;
begin
  Result := fNewIds[index];
end;

procedure TBoldIDTranslationList.AddTranslation(OldId, NewId: TBoldObjectId);
var
  iIndex: Integer;
begin
  // This routine no longer handles multiple translations!
{
  if Assigned(OldId) then begin
    iIndex := fOldIds.IndexByID[OldId];
    if (iIndex > -1) then begin
      if ((NewID = nil) and (fNewIds[iIndex] = nil)) or
         ((fNewIds[iIndex] <> nil) and fNewIds[iIndex].IsEqual[NewId]) then
      begin
        Exit;
      end;
    end;
  end;
  if Assigned(NewId) then begin
    iIndex := fNewIds.IndexByID[NewId];
    if (iIndex > -1) then begin
      if ((OldID = nil) and (fOldIds[iIndex] = nil)) or
         ((fOldIds[iIndex] <> nil) and fOldIds[iIndex].IsEqual[OldId]) then
      begin
        Exit;
      end;
    end;
  end;

  fOldIDs.Add(OldId);
  fNewIds.Add(NewId);
}
  // Translation only makes sense, if both IDs are set
  if Assigned(OldId) and Assigned(NewId) then begin
    iIndex := fOldIds.IndexByID[OldId];
    if (iIndex > -1) then begin
      if fNewIds[iIndex].IsEqual[NewId] then begin
        Exit;
      end;
    end;
    iIndex := fNewIds.IndexByID[NewId];
    if (iIndex > -1) then begin
      if fOldIds[iIndex].IsEqual[OldId] then begin
        Exit;
      end;
    end;

    fOldIDs.Add(OldId);
    fNewIds.Add(NewId);
  end;
end;

function TBoldIDTranslationList.GetTranslateToOldId(NewID: TBoldObjectId): TBoldObjectId;
var
  Pos: Integer;
begin
  result := NewId;
  Pos := fNewIds.IndexByID[Result];
  if pos <> -1 then
    result := GetOldId(Pos);
{  while Pos <> -1 do
  begin
    result := OriginalId[Pos];
    Pos := fFinalIds.IndexByID[Result];
  end;}
end;

procedure TBoldIDTranslationList.SetCapacity(const Value: Integer);
begin
  fOldIds.Capacity := Value;
  fNewIds.Capacity := Value;
end;

function TBoldIDTranslationList.GetTranslateToNewId(OldID: TBoldObjectId): TBoldObjectId;
var
  Pos: Integer;
begin
  result := OldId;
  Pos := fOldIds.IndexById[Result];
  if pos <> -1 then
    Result := GetNewId(Pos);
   
{  while Pos <> -1 do
  begin
    Result := FinalId[Pos];
    Pos := fOriginalIds.IndexById[Result];
  end;}
end;

{
function TBoldIDTranslationList.GetTranslateToOldId(Index: integer): TBoldObjectId;
begin
  result := fOldIds[Index];
end;

function TBoldIDTranslationList.GetTranslateToNewId(Index: integer): TBoldObjectId;
begin
  Result := GetNewId(Index);
end;
}

function TBoldIDTranslationList.GetStreamName: string;
begin
  result := BOLDIDTRANSLATIONLISTNAME;
end;

procedure TBoldObjectIdList.ExactifyIds(
  TranslationList: TBoldIDTranslationList);
var
  i: integer;
begin
  Assert(TranslationList.count <= Count);
  for i := 0 to TranslationList.count - 1 do
    if not ObjectIds[i].TopSortedIndexExact then
    begin
      if TranslationList.OldIds[i].IsEqual[ObjectIds[i]] then
        Items[i] := TranslationList.NewIds[i].Clone
      else
        Items[i] := TranslationList.TranslateToNewId[ObjectIds[i]].Clone;
    end;
end;

procedure TBoldObjectIdList.ApplyTranslationList(
  TranslationList: TBoldIdTranslationList);
var
  i: Integer;
  anId: TBoldObjectId;
  SameCount: boolean;
begin
  SameCount := TranslationList.Count = count;
  for i := Count - 1 downto 0 do
    begin
      Assert(Assigned(ObjectIds[i]));
      if SameCount and Assigned(TranslationList.OldIds[i]) then
      begin
        if TranslationList.OldIds[i].IsEqual[self.ObjectIds[i]] then
        begin // faster handling for special but most common case, where both lists contain same elements at same places
          self.Items[i] := TranslationList.NewIds[i].Clone;
          continue;
        end;
      end;
      anId := TranslationList.TranslateToNewId[ObjectIds[i]];
      if assigned(anId) then
        ReplaceId(ObjectIds[i], anId);
    end;
end;

function TBoldObjectId.GetNonExisting: Boolean;
begin
  Result := false;
end;

function TBoldObjectId.GetTimeStamp: TBoldTimeStampType;
begin
  result := BOLDMAXTIMESTAMP;
end;

{ EBoldUpdateFailedForIdList }

constructor EBoldOperationFailedForIdList.Create(msg: string; args: array of const; IdList: TBoldObjectIdList);
begin
  inherited createfmt(msg, args);
  fIdList := IdList.Clone;
end;

destructor EBoldOperationFailedForIdList.Destroy;
begin
  FreeAndNil(fIdList);
  inherited;
end;

{ TBoldXMLObjectIdStreamer }

procedure TBoldXMLObjectIdStreamer.ReadObject(Obj: TObject; Node: TBoldXMLNode);
var
  ClassIdNode,
  aSubNode: TBoldXMLNode;
  aModel: TMoldModel;
  ObjectId: TBoldObjectId;
  TopSortedIndex: integer;
  Exact: boolean;
begin
  inherited;
  aModel := (Node.Manager as TBoldDefaultXMLStreamManager).Model;

  aSubNode := Node.GetSubNode('ClassName');
  if assigned(aSubNode) then
  begin
    TopSortedIndex := aModel.Classes.ItemsByName[aSubNode.ReadString].TopSortedIndex;;
    aSubNode.Free;

    aSubNode := Node.GetSubNode('Exact');
    if assigned(aSubNode) then
      Exact := aSubNode.ReadBoolean
    else
      Exact := true;
    aSubNode.Free;
  end
  else
  begin
    ClassIdNode := Node.GetSubNode('classid');
    aSubNode := ClassIdNode.GetSubNode('name');
    TopSortedIndex := aModel.Classes.ItemsByName[aSubNode.ReadString].TopSortedIndex;
    aSubNode.Free;
    aSubNode := ClassIdNode.GetSubNode('exact');
    Exact := aSubNode.ReadBoolean;
    aSubNode.Free;
  end;

  ObjectId := Obj as TBoldObjectId;
  ObjectId.SetClassIdData(TopSortedIndex, Exact);
end;

procedure TBoldXMLObjectIdStreamer.WriteObject(Obj: TBoldInterfacedObject; Node: TBoldXMLNode);
var
  aSubNode: TBoldXMLNode;
  aModel: TMoldModel;
  ObjectId: TBoldObjectId; 
begin
  inherited;
  aModel := (Node.Manager as TBoldDefaultXMLStreamManager).Model;
  ObjectId := Obj as TBoldObjectId;

  aSubNode := Node.NewSubNode('ClassName');
  aSubNode.WriteString(aModel.Classes[ObjectId.TopSortedIndex].Name);
  aSubNode.Free;

  if not ObjectId.TopSortedIndexExact then
  begin
    aSubNode := Node.NewSubNode('Exact');
    aSubNode.WriteBoolean(ObjectId.TopSortedIndexExact);
    aSubNode.Free;
  end;
end;

{ TBoldXMLInternalObjectIdStreamer }

function TBoldXMLInternalObjectIdStreamer.CreateObject: TObject;
begin
  result := TBoldInternalObjectId.Create;
end;

function TBoldXMLInternalObjectIdStreamer.GetStreamName: string;
begin
  result := BOLDINTERNALIDNAME;
end;

procedure TBoldXMLInternalObjectIdStreamer.ReadObject(Obj: TObject;
  Node: TBoldXMLNode);
var
  aSubNode: TBoldXMLNode;
begin
  inherited;
  aSubNode := Node.GetSubNode('identifier');
  (Obj as TBoldInternalObjectId).fInternalIdentifier := aSubNode.ReadInteger;
  aSubNode.Free;
end;

procedure TBoldXMLInternalObjectIdStreamer.WriteObject(
  Obj: TBoldInterfacedObject; Node: TBoldXMLNode);
var
  aSubNode: TBoldXMLNode;
begin
  inherited;
  aSubNode := Node.NewSubNode('identifier');
  aSubNode.WriteInteger((Obj as TBoldInternalObjectId).fInternalIdentifier);
  aSubNode.Free;
end;

{ TBoldXMLObjectIdListStreamer }

function TBoldXMLObjectIdListStreamer.CreateObject: TObject;
begin
  result := TBoldObjectIdList.Create;
end;

function TBoldXMLObjectIdListStreamer.GetStreamName: string;
begin
  result := BOLDOBJECTIDLISTNAME;
end;

procedure TBoldXMLObjectIdListStreamer.ReadObject(Obj: TObject;
  Node: TBoldXMLNode);
var
  anIdList: TBoldObjectIdList;
  {$IFDEF OXML}
  aNodeEnumerator: TXMLResNodeListEnumerator;
  aNodeList: IXMLNodeList;
  aNode: PXMLNode;
  {$ELSE}
  aNodeList: IXMLDOMNodeList;
  aNode: IXMLDOMNode;
  {$ENDIF}
  aSubNode: TBoldXMLNode;
  ObjectId: TBoldObjectId;
begin
  inherited;
  anIdList := Obj as TBoldObjectIdList;
  {$IFDEF OXML}
  if Node.XMLDomElement.GetElementsByTagName('id', aNodeList) then begin
    aNodeEnumerator := aNodeList.GetEnumerator;
    try
      while aNodeEnumerator.MoveNext do begin
        aNode := aNodeEnumerator.Current;
        aSubNode := Node.MakeNodeForElement(aNode);
        ObjectId := aSubNode.ReadObject('') as TBoldObjectId;
        anIdList.Add(ObjectId);
        ObjectId.Free;
        aSubNode.Free;
      end;
    finally
      aNodeEnumerator.Free;
    end;
  end;
  {$ELSE}
  aNodeList := Node.XMLDomElement.getElementsByTagName('id');  // do not localize
  aNode := aNodeList.nextNode;
  while assigned(aNode) do
  begin
    aSubNode := Node.MakeNodeForElement(aNode as IXMLDOMElement);
    ObjectId := aSubNode.ReadObject('') as TBoldObjectId;
    anIdList.Add(ObjectId);
    ObjectId.Free;
    aSubNode.Free;
    aNode := aNodeList.nextNode;
  end;
  {$ENDIF}
end;

procedure TBoldXMLObjectIdListStreamer.WriteObject(Obj: TBoldInterfacedObject;
  Node: TBoldXMLNode);
var
  i: Integer;
  anIdList: TBoldObjectIdList;
  aSubNode: TBoldXMLNode;
begin
  inherited;
  anIdList := Obj as TBoldObjectIdList;
  for i := 0 to anIdList.Count - 1 do
  begin
    aSubNode := Node.NewSubNode('id');
    aSubNode.WriteObject('', anIdList[i]);
    aSubNode.Free;
  end;
end;

{ TBoldXMLTranslationListStreamer }

function TBoldXMLTranslationListStreamer.CreateObject: TObject;
begin
  result := TBoldIDTranslationList.Create;
end;

function TBoldXMLTranslationListStreamer.GetStreamName: string;
begin
  result := BOLDIDTRANSLATIONLISTNAME;
end;

procedure TBoldXMLTranslationListStreamer.ReadObject(Obj: TObject;
  Node: TBoldXMLNode);
var
  aTranslationList: TBoldIDTranslationList;
begin
  inherited;
  aTranslationList := Obj as TBoldIDTranslationList;
  aTranslationList.fOldIds.Free;
  aTranslationList.fNewIds.Free;
  aTranslationList.fOldIds := Node.ReadSubNodeObject('OldIds', BOLDOBJECTIDLISTNAME) as TBoldObjectIdList;
  aTranslationList.fNewIds := Node.ReadSubNodeObject('NewIds', BOLDOBJECTIDLISTNAME) as TBoldObjectIdList;
end;

procedure TBoldXMLTranslationListStreamer.WriteObject(
  Obj: TBoldInterfacedObject; Node: TBoldXMLNode);
var
  aTranslationList: TBoldIDTranslationList;
begin
  inherited;
  aTranslationList := Obj as TBoldIDTranslationList;
  Node.WriteSubNodeObject('OldIds', BOLDOBJECTIDLISTNAME, aTranslationList.fOldIds);
  Node.WriteSubNodeObject('NewIds', BOLDOBJECTIDLISTNAME, aTranslationList.fNewIds);
end;

{ TBoldXMLMemberIdStreamer }

function TBoldXMLMemberIdStreamer.CreateObject: TObject;
begin
  result := TBoldMemberID.create(0);
end;

function TBoldXMLMemberIdStreamer.GetStreamName: string;
begin
  result := BOLDMEMBERIDNAME;
end;

procedure TBoldXMLMemberIdStreamer.ReadObject(Obj: TObject;
  Node: TBoldXMLNode);
begin
  inherited;
  (Obj as TBoldMemberId).fMemberIndex := Node.ReadSubNodeInteger('MemberIndex'); 
end;

procedure TBoldXMLMemberIdStreamer.WriteObject(Obj: TBoldInterfacedObject;
  Node: TBoldXMLNode);
begin
  inherited;
  Node.WriteSubNodeInteger('MemberIndex', (Obj as TBoldMemberId).MemberIndex);
end;

{ TBoldXMLMemberIdListStreamer }

function TBoldXMLMemberIdListStreamer.CreateObject: TObject;
begin
  result := TBoldMemberIdList.Create;
end;

function TBoldXMLMemberIdListStreamer.GetStreamName: string;
begin
  result := BOLDMEMBERIDLISTNAME;
end;

procedure TBoldXMLMemberIdListStreamer.ReadObject(Obj: TObject;
  Node: TBoldXMLNode);
var
  aMemberIdList: TBoldMemberIdList;
  {$IFDEF OXML}
  aNodeEnumerator: TXMLResNodeListEnumerator;
  aNodeList: IXMLNodeList;
  aNode: PXMLNode;
  {$ELSE}
  aNodeList: IXMLDOMNodeList;
  aNode: IXMLDOMNode;
  {$ENDIF}
  aSubNode: TBoldXMLNode;
begin
  inherited;
  aMemberIdList := Obj as TBoldMemberIdList;
  {$IFDEF OXML}
  if Node.XMLDomElement.GetElementsByTagName('id', aNodeList) then begin
    aNodeEnumerator := aNodeList.GetEnumerator;
    try
      while aNodeEnumerator.MoveNext do begin
        aNode := aNodeEnumerator.Current;
        aSubNode := Node.MakeNodeForElement(aNode);
        aMemberIdList.Add(aSubNode.ReadObject('') as TBoldMemberId);
        aSubNode.Free;
      end;
    finally
      aNodeEnumerator.Free;
    end;
  end;
  {$ELSE}
  aNodeList := Node.XMLDomElement.getElementsByTagName('id');  // do not localize
  aNode := aNodeList.nextNode;
  while assigned(aNode) do
  begin
    aSubNode := Node.MakeNodeForElement(aNode as IXMLDOMElement);
    aMemberIdList.Add(aSubNode.ReadObject('') as TBoldMemberId);
    aSubNode.Free;
    aNode := aNodeList.nextNode;
  end;
  {$ENDIF}
end;

procedure TBoldXMLMemberIdListStreamer.WriteObject(
  Obj: TBoldInterfacedObject; Node: TBoldXMLNode);
var
  i: Integer;
  aMemberIdList: TBoldMemberIdList;
  aSubNode: TBoldXMLNode;
begin
  inherited;
  aMemberIdList := Obj as TBoldMemberIdList;
  for i := 0 to aMemberIdList.Count - 1 do
  begin
    aSubNode := Node.NewSubNode('id');
    aSubNode.WriteObject('', aMemberIdList[i]);
    aSubNode.Free;
  end;
end;

{ TBoldID }

procedure TBoldObjectIdList.AddList(ObjectIdList: TBoldObjectIdList);
var
  i: integer;
begin
  Capacity := Count + ObjectidList.Count;
  for i := 0 to ObjectidList.Count - 1 do
    Add(ObjectidList[i]);
end;

function TBoldObjectIdHashIndex.Hash(const Key): Cardinal;
begin
  Result := TBoldObjectId(Key).Hash;
end;

procedure TBoldObjectIdList.remove(Id: TBoldObjectId);
var
  p: integer;
begin
  p := IndexOf(Id);
  if p <> -1 then
    removebyIndex(p);
end;

procedure TBoldObjectIdList.RemoveNonExistingIds;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    if TBoldObjectId(Items[i]).NonExisting then
      RemoveByIndex(i);
end;

{ TBoldID }

function TBoldID.GetDebugInfo: string;
begin
  result := AsString;
end;

{ TBoldNonExistingObjectId }

function TBoldNonExistingObjectId.CloneWithClassId(TopSortedIndex: integer; Exact: Boolean): TBoldObjectid;
begin
//  raise EBold.CreateFmt('CloneWithClassId not available in %s',[ClassName]);
  result := TBoldNonExistingObjectId.CreateWithClassID(TopSortedIndex, Exact);
end;

function TBoldNonExistingObjectId.GetAsString: string;
begin
  result := '-1';
end;

function TBoldNonExistingObjectId.GetHash: Cardinal;
begin
  result := 0;
end;

function TBoldNonExistingObjectId.GetIsEqual(MatchID: TBoldObjectID): Boolean;
begin
  result := MatchId is TBoldNonExistingObjectId;
end;

function TBoldNonExistingObjectId.GetIsStorable: Boolean;
begin
  result := false;
end;

function TBoldNonExistingObjectId.GetNonExisting: Boolean;
begin
  Result := true;
end;

function TBoldNonExistingObjectId.GetStreamName: string;
begin
  result := '';
end;

{ TBoldIdList }

function TBoldIdList.CommaSeparatedIdList: String;
var
  i: integer;
  sb: TStringBuilder;
begin
  case count of
    0 :  result := '';
    1 :  result := TBoldId(Items[0]).AsString;
    2..MaxInt:
      begin
        sb := TStringBuilder.Create(TBoldId(Items[0]).AsString);
        try
          for i := 1 to Count - 1 do
          begin
            sb.Append(',');
            sb.Append(TBoldId(Items[i]).AsString);
          end;
          result := sb.ToString;
        finally
          sb.free;
        end;
      end;
  end;
end;

initialization
  TBoldXMLStreamerRegistry.MainStreamerRegistry.RegisterStreamer(TBoldXMLInternalObjectIdStreamer.Create);
  TBoldXMLStreamerRegistry.MainStreamerRegistry.RegisterStreamer(TBoldXMLObjectIdListStreamer.Create);
  TBoldXMLStreamerRegistry.MainStreamerRegistry.RegisterStreamer(TBoldXMLMemberIdStreamer.Create);
  TBoldXMLStreamerRegistry.MainStreamerRegistry.RegisterStreamer(TBoldXMLMemberIdListStreamer.Create);
  TBoldXMLStreamerRegistry.MainStreamerRegistry.RegisterStreamer(TBoldXMLTranslationListStreamer.Create);

end.
