unit BoldId;

interface

uses
  Classes,
  BoldDefs,
  BoldBase,
  BoldStreams,
  BoldIndex,
  BoldHashIndexes,
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
  end;

  {---TBoldMemberID---}
  TBoldMemberID = class(TBoldID)
  private
    fMemberIndex: Integer;
  protected
    function GetStreamName: string; override;
  public
    constructor create(MemberIndex: Integer);
    property MemberIndex: integer read fMemberIndex;
  end;

  {---TBoldSubMemberID---}
  TBoldSubMemberID = class(TBoldmemberID)
  private
    fOwnsPartof: Boolean;
    fPartOf: TBoldMemberID;
  public
    constructor Create(partOf: TBoldMemberID; OwnspartOf: Boolean; IndexInMemberList: integer);
    destructor Destroy; override;
  end;

  {---TBoldObjectId---}
  TBoldObjectId = class(TBoldID)
  private
    FClassId: integer;
    function getTopSortedIndex: integer;
    function GetTopSortedIndexExact: Boolean;
    procedure SetClassIdData(TopSortedIndex: integer; Exact: boolean);
  protected
    function GetAsString: string; virtual; abstract;
    function GetIsStorable: Boolean; virtual; abstract;
    function GetHash: Cardinal; virtual; abstract;
    Function GetIsEqual(MatchID: TBoldObjectID): Boolean; virtual; abstract;
    function GetTimeStamp: TBoldTimeStampType; virtual;
  public
    constructor CreateWithClassID(TopSortedIndex: integer; Exact: Boolean); virtual;
    function CloneWithClassId(TopSortedIndex: integer; Exact: Boolean): TBoldObjectid; virtual; abstract;
    function Clone: TBoldObjectId;
    property TopSortedIndex: integer read getTopSortedIndex;
    property TopSortedIndexExact: Boolean read GetTopSortedIndexExact;
    property AsString: string read GetAsString;
    property IsStorable: Boolean read GetIsStorable;
    property IsEqual[MatchId: TBoldObjectId]: Boolean read GetIsEqual;
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

  {---TBoldIdList---}
  TBoldIdList = class(TBoldIndexableList);

  {---TBoldObjectIdHashIndex---}
  TBoldObjectIdHashIndex = class(TBoldHashIndex)
  protected
    function ItemAsBoldObjectId(Item: TObject): TboldObjectId; virtual;
    function HashItem(Item: TObject): Cardinal; override;
    function Match(const Key; Item:TObject):Boolean; override;
    function Hash(const Key): Cardinal; override;
    function FindById(boldObjectId:TboldObjectId): TObject;
  end;

  {---TBoldObjectIdList---}
  TBoldObjectIdList = class(TBoldIdList, IBoldStreamable)
  private
    function GetObjectIDIndex: TBoldObjectIDHashIndex;
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
    procedure AddIfNotInList(ObjectID: TBoldObjectId);
    procedure AddList(ObjectIdList: TBoldObjectIdList);
    procedure Insert(Index: integer; ObjectID: TBoldObjectId);
    function ContainsSameIDs(List: TBoldObjectIdList): Boolean;
    function Clone: TBoldObjectIdList;
    function CommaSeparatedIdList: String;
    procedure remove(Id: TBoldObjectId);
    procedure ReplaceID(OldId, NewId: TBoldObjectId);
    procedure ExactifyIds(TranslationList: TBoldIDTranslationList);
    procedure ApplyTranslationList(TranslationList: TBoldIdTranslationList);
    property IDByID[ObjectID: TBoldObjectId]: TBoldObjectId read GetIdById;
    property IndexByID[ObjectID: TBoldObjectId]: Integer read GetIndexByID;
    property ObjectIds[index: Integer]: TBoldObjectId read GetObjectId; default;
    property IdInList[Objectid: TBoldObjectId]: Boolean read GetIdInList;
  end;

  {---TBoldMemberIdList---}
  TBoldMemberIdList = class(TBoldIdList, IBoldStreamable)
    function GetStreamName: string;
  protected
    function GetMemberIds(Index: Integer): TBoldMemberId;
  public
    Property MemberIds[Index: integer]: TBoldmemberId read GetMemberIds; default;
  end;

  {---TBoldIDTranslationList---}
  TBoldIDTranslationList = class(TBoldNonRefCountedObject, IBoldStreamable)
  // Possible Translations:
  // In Create-phase: ObjectID changed (Internal -> external), ClassID unchanged
  // In Update-Phase: ObjectID Changed (internal -> external)
  // In Delete-Phase: Nothing changed
  // In Fetch-phase: ObjectID unchanged, ClassID changed
  private
    fOldIds: TBoldObjectIdList;
    fNewIds: TBoldObjectIdList;
    function GetOldId(index: Integer): TBoldObjectId;
    function GetNewId(index: Integer): TBoldObjectId;
    function GetCount: Integer;
    function GetTranslateToOldId(NewID: TBoldObjectId): TBoldObjectId;
    function GetTranslateToNewId(OldID: TBoldObjectId): TBoldObjectId;
  protected
    function GetStreamName: string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddTranslation(OldId, NewId: TBoldObjectId);
    property Count: Integer read GetCount;
    property TranslateToOldId[NewID: TBoldObjectId]: TBoldObjectId read GetTranslateToOldId;
    property TranslateToNewId[OldID: TBoldObjectId]: TBoldObjectId read GetTranslateToNewId;
    property OldIds[Index: integer] :TBoldObjectId read GetOldId;
    property NewIds[Index: integer] :TBoldObjectId read GetNewId;
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
  SysUtils,
  BoldUtils,
  MSXML_TLB,
  BoldDefaultXMLStreaming,
  BoldDefaultStreamNames,
  BoldMeta;

var
  InternalIdCounter: Integer = 0;
  IX_ObjectID: integer = -1;

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

function TBoldObjectIdHashIndex.FindById(boldObjectId:TboldObjectId): TObject;
begin
  Result := Find(boldObjectId);
end;

{---TBoldMemberID---}
constructor TBoldMemberId.create(MemberIndex: Integer);
begin
  fMemberIndex := MemberIndex;
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

destructor TBoldSubMemberId.destroy;
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
  result := TBoldInternalObjectId.CreateWithClassId(TopSortedIndex, Exact);
  (result as TBoldInternalObjectId).fInternalIdentifier := fInternalIdentifier;
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
begin
  InternalId := GetIdByID(ObjectID);
  if assigned(InternalId) then
    Result := IndexOf(InternalId)
  else
    result := -1;
end;

function TBoldObjectIdList.GetIdInList(ObjectID: TBoldObjectId): Boolean;
begin
  Result := Assigned(ObjectId) and Assigned(GetIdByID(ObjectID));
end;

function TBoldObjectIdList.Clone: TBoldObjectIdList;
var
  i: Integer;
begin
  Result := TBoldObjectIdList.Create;
  for i := 0 to Count - 1 do
    Result.Add(ObjectIDs[i]);
end;

function TBoldObjectIdList.GetObjectId(index: Integer): TBoldObjectId;
begin
  Result := TBoldObjectId(Items[index]);
end;

procedure TBoldObjectIdList.Add(ObjectID: TBoldObjectId);
var
  newObjectID: TBoldObjectID;
begin
  if assigned(ObjectID) then
    NewObjectId := ObjectId.Clone
  else
    NewObjectId := nil;
  inherited Add(NewObjectID);
end;

procedure TBoldObjectIdList.AddIfNotInList(ObjectID: TBoldObjectId);
begin
  if not IdInList[Objectid] then
    Add(ObjectId);
end;

procedure TBoldObjectIdList.Insert(Index: integer;
  ObjectID: TBoldObjectId);
var
  newObjectID: TBoldObjectID;
begin
  if assigned(ObjectID) then
    NewObjectId := ObjectId.Clone
  else
    NewObjectId := nil;
  inherited insert(index, NewObjectID);
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
  RemoveByIndex(OldIndex);
  Insert(OldIndex, NewId);
end;

function TBoldObjectIdList.GetCount: integer;
begin
  result := count;
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
  Assert(Items[Index] is TBoldmemberId);
  result := Items[Index] as TBoldmemberId;
  assert(Assigned(Result));
end;

{---TBoldIDTranslationList---}
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

function TBoldIDTranslationList.GetCount: Integer;
begin
  Result := fOldIds.Count;
end;

procedure TBoldIDTranslationList.AddTranslation(OldId, NewId: TBoldObjectId);
begin
  fOldIDs.Add(OldId);
  fNewIds.Add(NewId)
end;

function TBoldIDTranslationList.GetTranslateToOldId(NewID: TBoldObjectId): TBoldObjectId;
var
  Pos: Integer;
begin
  result := NewId;
  Pos := fNewIds.IndexByID[Result];
  if pos <> -1 then
    result := GetOldId(Pos);
  // This routine no longer handles multiple translations!
{  while Pos <> -1 do
  begin
    result := OriginalId[Pos];
    Pos := fFinalIds.IndexByID[Result];
  end;}
end;

function TBoldIDTranslationList.GetTranslateToNewId(OldID: TBoldObjectId): TBoldObjectId;
var
  Pos: Integer;
begin
  result := OldId;
  Pos := fOldIds.IndexById[Result];
  if pos <> -1 then
    Result := GetNewId(Pos);
  // This routine no longer handles multiple translations!
{  while Pos <> -1 do
  begin
    Result := FinalId[Pos];
    Pos := fOriginalIds.IndexById[Result];
  end;}
end;

function TBoldIDTranslationList.GetStreamName: string;
begin
  result := BOLDIDTRANSLATIONLISTNAME;
end;

procedure TBoldObjectIdList.ExactifyIds(
  TranslationList: TBoldIDTranslationList);
var
  i: integer;
begin
  for i := 0 to count - 1 do
    if not ObjectIds[i].TopSortedIndexExact then
      ReplaceID(ObjectIds[i], TranslationList.TranslateToNewId[ObjectIds[i]]);
end;

procedure TBoldObjectIdList.ApplyTranslationList(
  TranslationList: TBoldIdTranslationList);
var
  i: Integer;
  anId: TBoldObjectId;
begin
  for i := Count - 1 downto 0 do
  begin
    anId := TranslationList.TranslateToNewId[ObjectIds[i]];
    if assigned(anId) then
      ReplaceId(ObjectIds[i], anId);
  end;
end;

{ TBoldClassIdWithExpressionName }
function TBoldObjectId.GetTimeStamp: TBoldTimeStampType;
begin
  result := BOLDMAXTIMESTAMP;
end;

{ EBoldUpdateFailedForIdList }

constructor EBoldOperationFailedForIdList.create(msg: string; args: array of const; IdList: TBoldObjectIdList);
begin
  inherited createfmt(msg, args);
  fIdList := IdList.Clone;
end;

destructor EBoldOperationFailedForIdList.destroy;
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

  aSubNode := Node.GetSubNode('ClassName'); // do not localize
  if assigned(aSubNode) then
  begin
    TopSortedIndex := aModel.Classes.ItemsByName[aSubNode.ReadString].TopSortedIndex;;
    aSubNode.Free;

    aSubNode := Node.GetSubNode('Exact');    // do not localize
    if assigned(aSubNode) then
      Exact := aSubNode.ReadBoolean
    else
      Exact := true;
    aSubNode.Free;
  end
  else
  begin
    // BackwardCompatibility
    ClassIdNode := Node.GetSubNode('classid');   // do not localize
    aSubNode := ClassIdNode.GetSubNode('name');  // do not localize
    TopSortedIndex := aModel.Classes.ItemsByName[aSubNode.ReadString].TopSortedIndex;
    aSubNode.Free;
    aSubNode := ClassIdNode.GetSubNode('exact');  // do not localize
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

  aSubNode := Node.NewSubNode('ClassName'); // do not localize
  aSubNode.WriteString(aModel.Classes[ObjectId.TopSortedIndex].Name);
  aSubNode.Free;

  if not ObjectId.TopSortedIndexExact then
  begin
    aSubNode := Node.NewSubNode('Exact'); // do not localize
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
  aSubNode := Node.GetSubNode('identifier'); // do not localize
  (Obj as TBoldInternalObjectId).fInternalIdentifier := aSubNode.ReadInteger;
  aSubNode.Free;
end;

procedure TBoldXMLInternalObjectIdStreamer.WriteObject(
  Obj: TBoldInterfacedObject; Node: TBoldXMLNode);
var
  aSubNode: TBoldXMLNode;
begin
  inherited;
  aSubNode := Node.NewSubNode('identifier'); // do not localize
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
  aNodeList: IXMLDOMNodeList;
  aNode: IXMLDOMNode;
  aSubNode: TBoldXMLNode;
  ObjectId: TBoldObjectId;
begin
  inherited;
  anIdList := Obj as TBoldObjectIdList;
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
    aSubNode := Node.NewSubNode('id');   // do not localize
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
  aTranslationList.fOldIds := Node.ReadSubNodeObject('OldIds', BOLDOBJECTIDLISTNAME) as TBoldObjectIdList;  // do not localize
  aTranslationList.fNewIds := Node.ReadSubNodeObject('NewIds', BOLDOBJECTIDLISTNAME) as TBoldObjectIdList;  // do not localize
end;

procedure TBoldXMLTranslationListStreamer.WriteObject(
  Obj: TBoldInterfacedObject; Node: TBoldXMLNode);
var
  aTranslationList: TBoldIDTranslationList;
begin
  inherited;
  aTranslationList := Obj as TBoldIDTranslationList;
  Node.WriteSubNodeObject('OldIds', BOLDOBJECTIDLISTNAME, aTranslationList.fOldIds); // do not localize
  Node.WriteSubNodeObject('NewIds', BOLDOBJECTIDLISTNAME, aTranslationList.fNewIds);  // do not localize
end;

{ TBoldXMLMemberIdStreamer }

function TBoldXMLMemberIdStreamer.CreateObject: TObject;
begin
  result := TBoldMemberID.create(0); // index param to constructor chosen arbitrarily, will be overwritten anyway
end;

function TBoldXMLMemberIdStreamer.GetStreamName: string;
begin
  result := BOLDMEMBERIDNAME;
end;

procedure TBoldXMLMemberIdStreamer.ReadObject(Obj: TObject;
  Node: TBoldXMLNode);
begin
  inherited;
  (Obj as TBoldMemberId).fMemberIndex := Node.ReadSubNodeInteger('MemberIndex'); // do not localize
end;

procedure TBoldXMLMemberIdStreamer.WriteObject(Obj: TBoldInterfacedObject;
  Node: TBoldXMLNode);
begin
  inherited;
  Node.WriteSubNodeInteger('MemberIndex', (Obj as TBoldMemberId).MemberIndex); // do not localize
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
  aNodeList: IXMLDOMNodeList;
  aNode: IXMLDOMNode;
  aSubNode: TBoldXMLNode;
begin
  inherited;
  aMemberIdList := Obj as TBoldMemberIdList;
  aNodeList := Node.XMLDomElement.getElementsByTagName('id');  // do not localize
  aNode := aNodeList.nextNode;
  while assigned(aNode) do
  begin
    aSubNode := Node.MakeNodeForElement(aNode as IXMLDOMElement);
    aMemberIdList.Add(aSubNode.ReadObject('') as TBoldMemberId);
    aSubNode.Free;
    aNode := aNodeList.nextNode;
  end;
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
    aSubNode := Node.NewSubNode('id');  // do not localize
    aSubNode.WriteObject('', aMemberIdList[i]);
    aSubNode.Free;
  end;
end;


{ TBoldID }

function TBoldObjectIdList.GetObjectIDIndex: TBoldObjectIDHashIndex;
begin
  if UnorderedIndexCount = 0 then
    SetIndexVariable(IX_ObjectID, AddIndex(TBoldObjectIDHashIndex.Create));
  result := TBoldObjectIDHashIndex(Indexes[IX_ObjectID]);
end;

procedure TBoldObjectIdList.AddList(ObjectIdList: TBoldObjectIdList);
var
  i: integer;
begin
  for i := 0 to ObjectidList.Count - 1 do
    Add(ObjectidList[i]);
end;

function TBoldObjectId.getTopSortedIndex: integer;
begin
  result := fClassId and TOPSORTEDINDEXTMASK;
end;

function TBoldObjectId.GetTopSortedIndexExact: Boolean;
begin
  result := (fClassId and CLASSIDEXACTMASK) = CLASSIDEXACTMASK;
end;

procedure TBoldObjectId.SetClassIdData(TopSortedIndex: integer; Exact: boolean);
begin
  if exact then
    fClassId := TopSortedIndex or CLASSIDEXACTMASK
  else
    fClassId := TopSortedIndex;
end;

function TBoldObjectIdHashIndex.Hash(const Key): Cardinal;
begin
  Result := TBoldObjectId(Key).Hash;
end;

function TBoldObjectIdList.CommaSeparatedIdList: String;
var
  i: integer;
begin
  result := '';
  for i := 0 to Count - 1 do
  begin
    if i <> 0 then
      result := result + ', ';
    result := result + ObjectIds[i].AsString;
  end;
end;

procedure TBoldObjectIdList.remove(Id: TBoldObjectId);
var
  p: integer;
begin
  p := IndexOf(Id);
  if p <> -1 then
    removebyIndex(p);
end;

initialization
  TBoldXMLStreamerRegistry.MainStreamerRegistry.RegisterStreamer(TBoldXMLInternalObjectIdStreamer.Create);
  TBoldXMLStreamerRegistry.MainStreamerRegistry.RegisterStreamer(TBoldXMLObjectIdListStreamer.Create);
  TBoldXMLStreamerRegistry.MainStreamerRegistry.RegisterStreamer(TBoldXMLMemberIdStreamer.Create);
  TBoldXMLStreamerRegistry.MainStreamerRegistry.RegisterStreamer(TBoldXMLMemberIdListStreamer.Create);
  TBoldXMLStreamerRegistry.MainStreamerRegistry.RegisterStreamer(TBoldXMLTranslationListStreamer.Create);

end.

