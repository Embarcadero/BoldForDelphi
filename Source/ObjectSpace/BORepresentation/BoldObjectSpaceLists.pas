unit BoldObjectSpaceLists;

interface

uses
  Classes,
  BoldSubscription,
  BoldSystem,
  BoldSystemRT,
  BoldIndex,
  BoldIndexableList;

type
  TBoldObjectAttributeIndexList = class;
  TBoldObjectLocatorList = class;
  TBoldMembersHashIndex = class;

{---TBoldObjectAttributeIndexList---}
  TBoldObjectAttributeIndexList = class(TBoldIndexableList)
  private
    function GetHasMembersIndex: Boolean;
    function GetMembersIndex: TBoldMembersHashIndex;
    function CreateMembersIndex(ObjectList: TBoldObjectList; MemberList: TBoldMemberRTInfoList): TBoldMembersHashIndex;
    procedure NotifyMemberIndexBad; virtual;
  protected
    procedure EnsureLazyCreateIndexes; virtual;
  public
    procedure InitMembersIndex(ObjectList: TBoldObjectList; MemberList: TBoldMemberRTInfoList);
    property HasMembersIndex: Boolean read GetHasMembersIndex;
    function GetLocatorByAttributesAndSubscribe(MemberList: TBoldMemberList; Subscriber: TBoldSubscriber): TBoldObjectLocator;
  end;

{---TBoldObjectLocatorList---}
  TBoldObjectLocatorList = class(TBoldObjectAttributeIndexList)
  private
    function GetLocatorIndex: TBoldLocatorHashIndex;
    function GetLocators(index: Integer): TBoldObjectLocator;
    procedure SetLocators(index: Integer; Value: TBoldObjectLocator);
    function GetLocatorInList(Locator: TBoldObjectLocator): Boolean;
    property LocatorIndex: TBoldLocatorHashIndex read GetLocatorIndex;
  protected
    procedure EnsureLazyCreateIndexes; override;
  public
    constructor Create;
    constructor CreateFromObjectList(BoldObjectList: TBoldObjectList);
    procedure Add(NewLocator: TBoldObjectLocator);
    function Clone: TBoldObjectLocatorList;
    procedure Ensure(NewLocator: TBoldObjectLocator);
    procedure FillObjectList(BoldObjectList: TBoldObjectList);
    procedure MergeObjectList(BoldObjectList: TBoldObjectList);
    property Locators[index: Integer]: TBoldObjectLocator read GetLocators write SetLocators; default;
    property LocatorInList[Locator: TBoldObjectLocator]: Boolean read GetLocatorInList;
  end;

 {---TBoldMembersHashIndex---}
  TBoldMembersHashIndex = class(TBoldHashIndex)
  private
    FMemberIndexList: TList;
    fMemberSubscriber: TBoldPassThroughSubscriber;
    fOwner: TBoldObjectAttributeIndexList;
    fObjectList: TBoldObjectList;
    procedure _receiveMemberChanged(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
  protected
    constructor Create(Owner: TBoldObjectAttributeIndexList; ObjectList: TBoldObjectList; Members: TBoldMemberRTInfoList);
    function HashItem(Item: TObject): Cardinal; override;
    function Match(const Key; Item:TObject):Boolean; override;
    function Hash(const Key): Cardinal; override;
    function LocatorFromItem(Item: TObject): TBoldObjectLocator;
    function ObjectFromItem(Item: TObject): TBoldObject;
  public
    destructor Destroy; override;
    procedure Clear(DestroyObjects: Boolean = false); override;
    procedure Add(Item: TObject); override;
    function GetLocatorByAttributesAndSubscribe(MemberList: TBoldMemberList; Subscriber: TBoldSubscriber): TBoldObjectLocator;
  end;

implementation

uses
  SysUtils,
  BoldId,
  BoldHashIndexes;

var
  IX_BoldObjectLocator: integer = -1;
  IX_BoldQualifiers: integer = -1;

type
  {---TBoldMembersKey---}
  TBoldMembersKey = class
  private
    FAttributeList: TBoldMemberList;
    FHash: Cardinal;
  public
    constructor Create(Attributes: TBoldMemberList);
    property AttributeList: TBoldMemberList read FAttributeList;
    function Hash: Cardinal;
  end;

procedure TBoldMembersHashIndex.Add(Item: TObject);
var
  i: integer;
  MemberId: TBoldmemberId;
begin
  inherited;
  if assigned(item) then
  begin
    assert(Item is TBoldObjectLocator);
    for i := 0 to FMemberIndexList.Count - 1 do
    begin
      MemberId := TBoldmemberId(FMemberIndexList[i]);
      ObjectfromItem(Item).BoldMembers[MemberId.MemberIndex].DefaultSubscribe(fMemberSubscriber);
    end;
  end;
end;

procedure TBoldMembersHashIndex._receiveMemberChanged(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  if assigned(fObjectList) then
    fObjectList.SendEvent(beQualifierChanged);
  fOwner.NotifyMemberIndexBad; // this will tell the owning locatorlist to destroy us.
end;

procedure TBoldMembersHashIndex.Clear(DestroyObjects: Boolean = false);
begin
  if assigned(fMemberSubscriber) then
    fMemberSubscriber.CancelAllSubscriptions;
  inherited;
end;

{---TBoldObjectAttributeIndexList---}

function TBoldObjectAttributeIndexList.GetLocatorByAttributesAndSubscribe(MemberList: TBoldMemberList; Subscriber: TBoldSubscriber): TBoldObjectLocator;
begin
  assert(indexes[IX_BoldQualifiers] is TBoldMembersHashIndex);
  result := TBoldMembersHashIndex(indexes[IX_BoldQualifiers]).GetLocatorByAttributesAndSubscribe(MemberList, Subscriber);
end;

procedure TBoldObjectAttributeIndexList.InitMembersIndex(ObjectList: TBoldObjectList; MemberList: TBoldMemberRTInfoList);
begin
  EnsureLazyCreateIndexes;
  if (IX_BoldQualifiers <> -1) and (IndexCount > IX_BoldQualifiers) then
    Indexes[IX_BoldQualifiers] := CreateMembersIndex(ObjectList, MemberList)
  else
    SetIndexVariable(IX_BoldQualifiers, AddIndex(CreateMembersIndex(ObjectList, MemberList)));
end;

function TBoldObjectAttributeIndexList.CreateMembersIndex(ObjectList: TBoldObjectList; MemberList: TBoldMemberRTInfoList): TBoldMembersHashIndex;
begin
  result := TBoldMembersHashIndex.Create(self, ObjectList, MemberList);
end;

function TBoldObjectAttributeIndexList.GetHasMembersIndex: Boolean;
begin
  result := (IX_BoldQualifiers <> -1) and (IndexCount > IX_BoldQualifiers) and
    (Indexes[IX_BoldQualifiers] is TBoldMembersHashIndex);
end;

procedure TBoldObjectAttributeIndexList.EnsureLazyCreateIndexes;
begin
  // nothing;
end;


procedure TBoldObjectAttributeIndexList.NotifyMemberIndexBad;
var
  TempIndex: TBoldIndex;
begin
  if HasMembersIndex then
  begin
    TempIndex := GetMembersIndex;
    RemoveAndFreeIndex(TempIndex, false);
  end;
end;

function TBoldObjectAttributeIndexList.GetMembersIndex: TBoldMembersHashIndex;
begin
  result := TBoldMembersHashIndex(indexes[IX_BoldQualifiers]);
end;



{---TBoldObjectLocatorList---}
constructor TBoldObjectLocatorList.Create;
begin
  inherited;
  SetIndexCapacity(1);
  OwnsEntries := False;
end;

constructor TBoldObjectLocatorList.CreateFromObjectList(BoldObjectList: TBoldObjectList);
var
  I: Integer;
begin
  Create;
  for I := 0 to BoldObjectList.Count - 1 do
    Add(BoldObjectList.Locators[I]);
end;

function TBoldObjectLocatorList.GetLocators(index: Integer): TBoldObjectLocator;
begin
  Result := TBoldObjectLocator(Items[index]);
end;

function TBoldObjectLocatorList.GetLocatorIndex: TBoldLocatorHashIndex;
begin
  if UnorderedIndexCount = 0 then
    SetIndexVariable(IX_BoldObjectLocator, AddIndex(TBoldLocatorHashIndex.Create));
  result := TBoldLocatorHashIndex(Indexes[IX_BoldObjectLocator]);
end;


function TBoldObjectLocatorList.Clone: TBoldObjectLocatorList;
var
  I: Integer;
begin
  Result := TBoldObjectLocatorList.Create;
  for I := 0 to Count - 1 do
    Result.Add(Locators[I]);
end;

function TBoldObjectLocatorList.GetLocatorInList(Locator: TBoldObjectLocator): Boolean;
begin
  result := assigned(Locator);
  if result then
  begin
    if Count > 10 then
      Result := Assigned(LocatorIndex.FindLocatorByLocator(Locator))
    else
      result := IndexOf(Locator) <> -1;
  end;
end;

procedure TBoldObjectLocatorList.MergeObjectList(BoldObjectList: TBoldObjectList);
var
  I: Integer;
begin
  for I := 0 to BoldObjectList.Count - 1 do
    if not LocatorInList[BoldObjectList.Locators[I]] then
      Add(BoldObjectList.Locators[I]);
end;

procedure TBoldObjectLocatorList.FillObjectList(BoldObjectList: TBoldObjectList);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if not BoldObjectList.LocatorInList(Locators[I]) then
      BoldObjectList.Add(Locators[I].BoldObject);
end;

procedure TBoldObjectLocatorList.Add(NewLocator: TBoldObjectLocator);
begin
  inherited Add(NewLocator);
end;

procedure TBoldObjectLocatorList.EnsureLazyCreateIndexes;
begin
  inherited;
  GetLocatorIndex;
end;

procedure TBoldObjectLocatorList.Ensure(NewLocator: TBoldObjectLocator);
begin
  if not LocatorInList[NewLocator] then
    Add(NewLocator);
end;



procedure TBoldObjectLocatorList.SetLocators(index: Integer; Value: TBoldObjectLocator);
begin
  Items[index] := Value;
end;

  {---TBoldMembersHashIndex---}
constructor TBoldMembersHashIndex.Create(Owner: TBoldObjectAttributeIndexList; ObjectList: TBoldObjectList; Members: TBoldMemberRTInfoList);
var
  i: Integer;
begin
  inherited Create;
  FMemberIndexList := TList.Create;
  FMemberIndexList.Capacity := Members.Count;
  for i := 0 to Members.Count - 1 do
    FMemberIndexList.Add(TBoldMemberId.create(Members[i].index));
  fMemberSubscriber := TBoldPassThroughSubscriber.Create(_receiveMemberChanged);
  fOwner := Owner;
  fObjectList := ObjectList;
end;

destructor TBoldMembersHashIndex.Destroy;
var
  i: Integer;
begin
  for i := 0 to FMemberIndexList.Count - 1 do
    TObject(FMemberIndexList[i]).Free;
  FreeAndNil(FMemberIndexList);
  FreeAndNil(fMemberSubscriber);
  inherited;
end;

function TBoldMembersHashIndex.HashItem(Item: TObject): Cardinal;
var
  i: Integer;
  concatval: String;
  index: Integer;
  member: TBoldMember;
  BoldObject: TBoldObject;
begin
  concatval := '';
  BoldObject := ObjectFromItem(Item);
  for i := 0 to FMemberIndexList.Count - 1 do
  begin
    Assert(TObject(FMemberIndexList[i]) is TBoldMemberId);
    index := TBoldMemberId(FMemberIndexList[i]).MemberIndex;
    member := BoldObject.BoldMembers[index];
    concatval := concatval + member.AsString;
  end;
  result := TBoldStringKey.HashString(concatval, bscCaseDependent);
end;

function TBoldMembersHashIndex.Match(const Key; Item:TObject):Boolean;
var
  MembersKey: TBoldMembersKey;
  BoldObject: TBoldObject;
  i: Integer;
begin
  MembersKey := TBoldMembersKey(Key);
  BoldObject := ObjectFromItem(Item);
  result := FMemberIndexList.Count = MembersKey.AttributeList.Count;
  i := 0;
  while result and (i < FMemberIndexList.Count) do
  begin
    Assert(TObject(FMemberIndexList[i]) is TBoldMemberId);
    result := MembersKey.AttributeList[i].IsEqual(BoldObject.BoldMembers[TBoldMemberId(FMemberIndexList[i]).MemberIndex]);
    inc(i);
  end;
end;

function TBoldMembersHashIndex.ObjectFromItem(Item: TObject): TBoldObject;
var
  temp: TBoldObjectLocator;
begin
  temp := LocatorFromItem(Item);
  if assigned(temp) then
    result := temp.EnsuredBoldObject
  else
    result := nil;
end;

function TBoldMembersHashIndex.LocatorFromItem(Item: TObject): TBoldObjectLocator;
begin
  Assert(not Assigned(Item) or (Item is TBoldObjectLocator));
  result := TBoldObjectLocator(Item);
end;

function TBoldMembersHashIndex.GetLocatorByAttributesAndSubscribe(MemberList: TBoldMemberList; Subscriber: TBoldSubscriber): TBoldObjectLocator;
var
  Key: TBoldMembersKey;
  i: integer;
begin
  Key := TBoldMembersKey.Create(MemberList);
  try
    result := LocatorFromItem(Find(Key));
  finally
    Key.Free;
  end;

  if assigned(subscriber) then
  begin
    if assigned(result) then
    begin
      for i := 0 to FMemberIndexList.Count - 1 do
        result.EnsuredBoldObject.BoldMembers[TBoldMemberId(FMemberIndexList[i]).MemberIndex].DefaultSubscribe(subscriber, breReSubscribe);
    end
    else if assigned(fObjectList) then
      fObjectList.AddSmallSubscription(Subscriber, [beQualifierChanged], breReSubscribe);
  end;
end;

  {---TBoldMembersKey---}

constructor TBoldMembersKey.Create(Attributes: TBoldMemberList);
var
  i: Integer;
  concatval: String;
begin
  FAttributeList := Attributes;
  for i := 0 to FAttributeList.Count - 1 do
    concatval := concatval + FAttributeList[i].AsString;
  FHash := TBoldStringKey.HashString(concatval, bscCaseDependent);
end;

function TBoldMembersKey.Hash: Cardinal;
begin
  result := FHash;
end;

function TBoldMembersHashIndex.Hash(const Key): Cardinal;
begin
  Result := TBoldMembersKey(Key).Hash;
end;

end.
