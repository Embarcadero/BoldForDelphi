unit BoldIndexableLoadedObjectsList;

{ Global compiler directives }
{$include bold.inc}

interface

uses
  BoldSubscription,
  BoldIndexableList,
  BoldSystem,
  BoldSystemRT,
  BoldObjectSpaceLists,
  BoldHashIndexes;

type
  // By descending from TBoldObjectLocatorList we reuse the mechanism for member hashing
  // but at the price of having integer index in addition member index
  // alternatively we could inherit from TBoldUnOrderedIndexableList, then we'd have to reimplement some of TBoldObjectLocatorList
  TBoldIndexableLoadedObjectsList = class(TBoldObjectLocatorList)
  private
    fBoldSystem: TBoldSystem;
    fSubscriber: TBoldExtendedPassthroughSubscriber;
    fClass: TBoldClassTypeInfo;
    fMembers: TBoldMemberRTInfoList;
    fStringCompareMode: TBoldStringCompareMode;
    procedure ObjectChangeReceive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const);
    procedure AddLoadedObjects;
  protected
    function CreateMembersIndex(ObjectList: TBoldObjectList; MemberList: TBoldMemberRTInfoList): TBoldMembersHashIndex; override;
  public
    constructor Create(const IndexedMembers: array of TBoldMemberRTInfo; AClass: TBoldClassTypeInfo = nil; ABoldSystem: TBoldSystem = nil; AStringCompareMode: TBoldStringCompareMode = bscCaseDependent); overload;
    constructor Create(const IndexedMembers: array of string; AClass: TBoldObjectClass = nil; ABoldSystem: TBoldSystem = nil; AStringCompareMode: TBoldStringCompareMode = bscCaseDependent); overload;
    destructor Destroy; override;
    procedure Add(BoldObject: TBoldObject);
    procedure Remove(BoldObject: TBoldObject);
    function GetObjectByMemberValues(const values: array of const): TBoldObject;
  end;

implementation

uses
  SysUtils,
  Classes,

  BoldAttributes,
  BoldIndex,
  BoldDefs,
  BoldBase,
  BoldDomainElement,
  BoldElements,
  BoldMetaElementList;

type
  TBoldReusableMembersHashIndex = class(TBoldMembersHashIndex)
  private
    fStringCompareMode: TBoldStringCompareMode;
  protected
    procedure _ReceiveEvent(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const); override;
  public
    property StringCompareMode: TBoldStringCompareMode read fStringCompareMode write fStringCompareMode;
  end;

procedure TBoldIndexableLoadedObjectsList.Add(BoldObject: TBoldObject);
begin
  if Assigned(BoldObject) and (not Assigned(fClass) or BoldObject.BoldClassTypeInfo.ConformsTo(fClass)) then
    inherited Add(BoldObject.BoldObjectLocator);
end;

procedure TBoldIndexableLoadedObjectsList.Remove(BoldObject: TBoldObject);
begin
  if Assigned(BoldObject) and (not Assigned(fClass) or BoldObject.BoldClassTypeInfo.ConformsTo(fClass)) then
    inherited Remove(BoldObject.BoldObjectLocator);
end;

procedure TBoldIndexableLoadedObjectsList.AddLoadedObjects;
var
  Traverser: TBoldIndexableListTraverser;
  ClassList: TBoldObjectList;
  i: integer;
begin
  if Assigned(fClass) and (fBoldSystem.Classes[fClass.TopSortedIndex].BoldPersistenceState = bvpsCurrent) then
  begin
    ClassList := fBoldSystem.Classes[fClass.TopSortedIndex];
    for I := 0 to ClassList.Count - 1 do
      inherited AddToAllIndexes(ClassList.Locators[i]); // inherited skips ConformsTo check
  end
  else
  begin
    Traverser := fBoldSystem.Locators.CreateTraverser;
    try
      while Traverser.MoveNext do
      begin
        with TBoldObjectLocator(Traverser.Item) do
          Add(BoldObject);
      end;
    finally
      Traverser.free;
    end;
  end;
end;

constructor TBoldIndexableLoadedObjectsList.Create(const IndexedMembers: array of TBoldMemberRTInfo; AClass: TBoldClassTypeInfo; ABoldSystem: TBoldSystem; AStringCompareMode: TBoldStringCompareMode);
var
  i: integer;
begin
  if not Assigned(ABoldSystem) then
    fBoldSystem := TBoldSystem.DefaultSystem
  else
    fBoldSystem := ABoldSystem;
  if length(IndexedMembers) = 0 then
    raise Exception.Create('No members specified.');
  if not Assigned(AClass) then
    AClass := fBoldSystem.BoldSystemTypeInfo.RootClassTypeInfo;
  fClass := AClass;
  for i := 0 to length(IndexedMembers) - 1 do
    if not AClass.ConformsTo(IndexedMembers[i].ClassTypeInfo) then
      raise Exception.CreateFmt('Member %s does not exist in class %s.', [IndexedMembers[i].DisplayName, AClass.DisplayName]);
  inherited Create;
  fStringCompareMode := AStringCompareMode;
  fMembers := TBoldMemberRTInfoList.Create;
  fMembers.OwnsEntries := false;
  for i := 0 to length(IndexedMembers) - 1 do
    fMembers.Add(IndexedMembers[i]);
  fSubscriber := TBoldExtendedPassthroughSubscriber.CreateWithExtendedReceive(ObjectChangeReceive);
  fBoldSystem.Classes[0].AddSmallSubscription(fSubscriber, [beCompleteModify, beObjectFetched, beObjectDeleted, beObjectUnloaded, beDestroying], beDestroying);
  InitMembersIndex(nil, fMembers);
  AddLoadedObjects;
end;

constructor TBoldIndexableLoadedObjectsList.Create(
  const IndexedMembers: array of string; AClass: TBoldObjectClass;
  ABoldSystem: TBoldSystem; AStringCompareMode: TBoldStringCompareMode);
var
  i: integer;
  ClassTypeInfo: TBoldClassTypeInfo;
  MemberRtInfo: TBoldMemberRTInfo;
  vIndexedMembers: array of TBoldMemberRTInfo;
begin
  if not Assigned(ABoldSystem) then
    fBoldSystem := TBoldSystem.DefaultSystem
  else
    fBoldSystem := ABoldSystem;
  ClassTypeInfo := fBoldSystem.BoldSystemTypeInfo.TopSortedClasses.ItemsByObjectClass[AClass];
  if not Assigned(ClassTypeInfo) then
    raise Exception.Create('Class type not found.');
  SetLength(vIndexedMembers, length(IndexedMembers));
  for i := 0 to length(IndexedMembers) - 1 do
  begin
    MemberRtInfo := ClassTypeInfo.MemberRTInfoByExpressionName[IndexedMembers[i]];
    if not Assigned(MemberRtInfo) then
      raise Exception.CreateFmt('Member %s does not exist in class %s.', [IndexedMembers[i], ClassTypeInfo.DisplayName]);
    vIndexedMembers[i] := MemberRtInfo;
  end;
  Create(vIndexedMembers, ClassTypeInfo, fBoldSystem, AStringCompareMode);
end;

function TBoldIndexableLoadedObjectsList.CreateMembersIndex(
  ObjectList: TBoldObjectList;
  MemberList: TBoldMemberRTInfoList): TBoldMembersHashIndex;
begin
  result := TBoldReusableMembersHashIndex.Create(self, ObjectList, MemberList, fStringCompareMode);
end;

destructor TBoldIndexableLoadedObjectsList.Destroy;
begin
  FreeAndNil(fSubscriber);
  FreeAndNil(fMembers);
  inherited;
end;

function TBoldIndexableLoadedObjectsList.GetObjectByMemberValues(
  const values: array of const): TBoldObject;
var
  MemberList: TBoldMemberList;
  Locator: TBoldObjectLocator;
  i: integer;
begin
  MemberList := VarArrayToBoldMemberList(Values);
  try
    Locator := GetLocatorByAttributesAndSubscribe(MemberList, nil);
    if Assigned(Locator) then
      result := Locator.BoldObject
    else
      result := nil;
  finally
    for I := 0 to MemberList.count - 1 do
      MemberList[i].free;
    MemberList.Free;
  end;
end;

procedure TBoldIndexableLoadedObjectsList.ObjectChangeReceive(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent;
  const Args: array of const);
begin
  // beObjectCreated is too early and attributes won't be set yet, so we use beCompleteModify
  case OriginalEvent of
    beObjectFetched: Add(Originator as TBoldObject);
    beCompleteModify:
      begin
        if (Originator is TBoldObject) then
          Add(Originator as TBoldObject); // only handle Objects, members should be handled by inherited code
      end;
    beObjectUnloaded, beObjectDeleted: Remove(Originator as TBoldObject);
    beDestroying:
      begin
        fSubscriber.CancelAllSubscriptions;
        fBoldSystem := nil;
        Clear;
      end;
  end;
end;

{ TBoldReusableMembersHashIndex }

procedure TBoldReusableMembersHashIndex._ReceiveEvent(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const);
var
  Locator: TBoldObjectLocator;
begin
  Assert(Originator is TBoldMember);
  Locator := TBoldMember(Originator).OwningObject.BoldObjectLocator;
  ItemChanged(Locator);
end;

end.

