
{ Global compiler directives }
{$include bold.inc}
unit BoldUndoHandler;

interface

uses
  BoldBase,
  BoldSystem,
  BoldFreeStandingValues,
  BoldId,
  BoldValueInterfaces,
  BoldValueSpaceInterfaces,
  BoldUndoInterfaces,
  BoldSystemRT,
  Classes;

type
  {forward declarations}
  TBoldUndoHandler = class;
  TBoldUndoBlock = class;
  TBoldUndoBlockList = class;

  TBoldUndoBlock = class(TBoldNonRefCountedObject, IBoldUndoBlock)
  private
    FName: string;
    FCaption: string;
    FValueSpace: TBoldFreeStandingValueSpace;
    FUndoBlockList: TBoldUndoBlockList;
    FContainsChanges: Boolean;
    FCreated: TDateTime;
//  Not used currently
//    procedure GetLinksToObject(const ObjectId: TBoldObjectId; const OwnIndexInLinkClass: integer;
//      const SingleLinkClassTypeInfo: TBoldClassTypeInfo; SingleLinkIds: TBoldObjectIdList);
//    procedure AllIdsInClass(const ClassTypeInfo: TBoldClassTypeInfo; IdList: TBoldObjectIdList);
    function GetFSValueSpace: TBoldFreeStandingValueSpace;
    procedure SetFSValueSpace(const Value: TBoldFreeStandingValueSpace);
    function GetName: string;
    function GetValueSpace: IBoldValueSpace;
    function GetContainsChanges: Boolean;
    function GetContent: String;
    function GetCaption: String;
    function GetCreated: TDateTime;
    function GetIndex: integer;
    function GetChangesForObject(ABoldObjectID: TBoldObjectID; AddHeader: boolean): string;
    function GetObjectCount: integer;
  protected
    function HasObjectContentsForAnyObjectInList(const ObjectList: TBoldObjectList): Boolean;
    procedure HandleMember(const ObjectContents: IBoldObjectContents; MemberIndex: integer; const MemberValue: IBoldValue);
    procedure HandleObject(const Obj: IBoldObjectContents; RegardAsExisting: Boolean);
    function IsDependantOn(Block: TBoldUndoBlock): Boolean;
  public
    constructor CreateNamedBlock(AUndoBlockList: TBoldUndoBlockList; const BlockName: string; const ACaption: string; const FSVAlueSpace: TBoldFreeStandingValueSpace = nil);
    destructor Destroy; override;
    procedure ApplytranslationList(IdTranslationList: TBoldIdTranslationList);
    procedure Merge(Block: TBoldUndoBlock; const Overwrite: Boolean);
    function ValueExists(const ObjectID: TBoldObjectId; const MemberIndex: integer): Boolean; overload;
    function ValueExists(const ObjectID: TBoldObjectID; const MemberIndex: integer; out Value: IBoldValue): Boolean; overload;
    procedure AddObjectsToList(const AList: TBoldList);
    property BlockName: string read FName;
    property FSValueSpace: TBoldFreeStandingValueSpace read GetFSValueSpace write SetFSVAlueSpace;
    property ValueSpace: IBoldValueSpace read GetValueSpace;
    property ContainsChanges: Boolean read GetContainsChanges;
    property ObjectCount: integer read GetObjectCount;
    property Created: TDateTime read GetCreated;
    property Content: String read GetContent;
    property Caption: String read GetCaption;
    property Index: Integer read GetIndex;
  end;

  TBoldUndoBlockList = class(TBoldNonRefCountedObject, IBoldUndoList)
  private
    fBoldUndoHandler: TBoldUndoHandler;
    FList: TStringList;
    function GetBlockByIndex(Index: integer): TBoldUndoBlock;
    function GetBlockByName(const BlockName: string): TBoldUndoBlock;
    function GetCount: integer;
    function GetCurrentBlock: TBoldUndoBlock;
    function GetAssertedBlockByName(const BlockName: string): TBoldUndoBlock;
    function GetAssertedBlockByIndex(Index: integer): TBoldUndoBlock;
    function GetItem(Index: integer): IBoldUndoBlock;
    function GetItemByName(const Name: string): IBoldUndoBlock;
    function GetTopBlock: IBoldUndoBlock;
    function GetContainsChanges: Boolean;
  protected
    procedure Clear;
    function AddBlock(var BlockName: string; const ACaption: string; const FSVAlueSpace: TBoldFreeStandingValueSpace = nil): TBoldUndoBlock;
    property AssertedBlockByName[const BlockName: string]: TBoldUndoBlock read GetAssertedBlockByName;
    property AssertedBlockByIndex[Index: integer]: TBoldUndoBlock read GetAssertedBlockByIndex;
    function AssertedIndexOf(const BlockName: string): integer;
    procedure InternalRemoveBlock(const BlockName: string); overload;
    procedure InternalRemoveBlock(Block: TBoldUndoBlock); overload;
    function GetIsEmpty: boolean;
  public
    procedure ApplytranslationList(IdTranslationList: TBoldIdTranslationList);
    constructor Create(ABoldUndoHandler: TBoldUndoHandler);
    destructor Destroy; override;
    procedure MoveBlock(CurIndex, NewIndex: integer);
    function IndexOf(const BlockName: string): integer; overload;
    function IndexOf(Block: TBoldUndoBlock): integer; overload;
    function RemoveBlock(const BlockName: string): Boolean;
    procedure RenameBlock(const OldName, NewName: string);
    procedure MoveToTop(const BlockName: string);
    procedure MergeBlocks(const DestinationBlockName, SourceBlockName: string);
    property BlockByIndex[Index: integer]: TBoldUndoBlock read GetBlockByIndex; default;
    property BlockByName[const BlockName: string]: TBoldUndoBlock read GetBlockByName;
    function CanMoveBlock(CurIndex, NewIndex: integer): Boolean;
    function CanMergeBlock(CurIndex, NewIndex: integer): Boolean;
    function CanMoveToTop(CurIndex: integer): Boolean;
    procedure GetDependantBlocks(const BlockName: string; DependantBlocks: TList);
    procedure MergeAll;
    function GetFirstNonEmptyBlock: TBoldUndoBlock;
    property Count: integer read GetCount;
    property CurrentBlock: TBoldUndoBlock read GetCurrentBlock;
    property ContainsChanges: Boolean read GetContainsChanges;
    property IsEmpty: boolean read GetIsEmpty;
  end;

  TBoldUndoHandler = class(TBoldAbstractUndoHandler, IBoldUndoHandler)
  private
    FUndoBlocks: TBoldUndoBlockList;
    FRedoBlocks: TBoldUndoBlockList;
    fUndoState: TBoldUndoState;
    fEnabled: Boolean;
    fReuseEmptyBlocks: boolean;
{    function GetFetchedValueOfIndirectMultiLink(const Member: TBoldMember; const OwningObjectId: TBoldObjectId;
       const RoleRTInfo: TBoldRoleRTInfo): TBoldFreeStandingValue;
    function GetFetchedValueOfDirectMultiLink (const Member: TBoldMember; const OwningObjectId: TBoldObjectId;
       const RoleRTInfo: TBoldRoleRTInfo): TBoldFreeStandingValue;
    }
    procedure DoUndo(UnDoValueSpace: TBoldFreeStandingValueSpace;
                      RedoValueSpace: TBoldFreeStandingValueSpace);
    procedure DoUndoInTransaction(BlockName: string; FromList, ToList: TBoldUndoBlockList);  // Don't make blockname const!
    function GetUndoList: IBoldUndoList;
    function GetRedoList: IBoldUndoList;
    function CanUndoBlock(const BlockName: string): Boolean;
    function CanRedoBlock(const BlockName: string):Boolean;
    function GetCurrentUndoBlock: TBoldUndoBlock;
    function GetIsEmpty: boolean;
    function GetCurrentUndoBlockHasChanges: boolean;
    function GetCurrentUndoBlockCaption: string;
    procedure ClearCurrentUndoBlock;
    function GetUniqueBlockName: string;
  protected
    function GetEnabled: Boolean; override;
    procedure SetEnabled(value: Boolean); override;
  public
    constructor Create(System: TBoldSystem); override;
    destructor Destroy; override;
    function SetCheckPoint(const ACaption: string = ''): string;
    procedure HandleMember(const ObjectContents: IBoldObjectContents; MemberIndex: integer; const MemberValue: IBoldValue); overload; override;
    procedure HandleObject(const Obj: IBoldObjectContents; RegardAsExisting: Boolean); override;
    procedure UndoBlock(const BlockName: string);
    procedure RedoBlock(const BlockName: string);
    procedure UnDoLatest;
    procedure RedoLatest;
    procedure ApplytranslationList(IdTranslationList: TBoldIdTranslationList); override;
    procedure PrepareUpdate(const ObjectList: TBoldObjectList); override;
    procedure ClearAllUndoBlocks;
    property UndoBlocks: TBoldUndoBlockList read fUndoBlocks;
    property RedoBlocks: TBoldUndoBlockList read fRedoBlocks;
    property UndoState: TBoldUndoState read fUndoState write fUndoState;
    property CurrentUndoBlock: TBoldUndoBlock read GetCurrentUndoBlock;
    property ReuseEmptyBlocks: boolean read fReuseEmptyBlocks write fReuseEmptyBlocks;
    property CurrentUndoBlockCaption: string read GetCurrentUndoBlockCaption;
  end;

implementation

uses
  SysUtils,

  BoldCoreConsts,
  BoldDefs,
  BoldGuard,
  BoldDomainElement,
  BoldSubscription;

const
  cUnNamedBlockName = 'Undo';

{ TBoldUndoBlock }

constructor TBoldUndoBlock.CreateNamedBlock(AUndoBlockList: TBoldUndoBlockList; const BlockName: string; const ACaption: string; const FSVAlueSpace: TBoldFreeStandingValueSpace);
begin
  inherited Create;
  FUndoBlockList := AUndoBlockList;
  FContainsChanges := false;
  FValueSpace := FSVAlueSpace;
  FCreated := now;
  FName := {FormatDateTime('hh:nn:ss', FCreated) + ' - ' + }BlockName;
  fCaption := ACaption;
end;

function TBoldUndoBlock.GetFSValueSpace: TBoldFreeStandingValueSpace;
begin
  if not Assigned(FValueSpace) then
    FValueSpace := TBoldFreeStandingValueSpace.Create;
  Result := FValueSpace;
end;

function TBoldUndoBlock.GetIndex: integer;
begin
  result := FUndoBlockList.IndexOf(self);
end;

function TBoldUndoBlock.IsDependantOn(
  Block: TBoldUndoBlock): Boolean;
var
  ObjectContents: TBoldFreeStandingObjectContents;
  i, j: integer;
  aValue: IBoldValue;
  ObjectIds: TBoldObjectIdList;
  G: IBoldGuard;
begin
  G := TBoldGuard.Create(ObjectIds);
  ObjectIds := TBoldObjectIdList.Create;
  Result := false;
  FSValueSpace.AllObjectIds(ObjectIds, True);
  for i:= 0 to ObjectIds.Count - 1 do
  begin
    ObjectContents := FSValueSpace.GetFSObjectContentsByObjectId(ObjectIds[i]);
    for j := 0 to ObjectContents.MemberCount - 1 do
    begin
      Result := Assigned(ObjectContents.ValueByIndex[j]) and Block.ValueExists(ObjectIds[i], j, aValue);
      if Result then
        Break;
    end;
    if Result then
      Break;
  end;
end;

procedure TBoldUndoBlock.Merge(Block: TBoldUndoBlock; const Overwrite: Boolean);
var
  OwnContents, ObjectContents: TBoldFreeStandingObjectContents;
  ObjectId: TBoldObjectId;
  i, j: integer;
  ObjectIds: TBoldObjectIdList;
  G: IBoldGuard;
begin
  G := TBoldGuard.Create(ObjectIds);
  ObjectIds := TBoldObjectIdList.Create;
  Block.FSValueSpace.AllObjectIds(ObjectIds, True);
  for i:= 0 to ObjectIds.Count - 1 do
  begin
    ObjectId := ObjectIds[i];
    ObjectContents := Block.FSValueSpace.GetFSObjectContentsByObjectId(ObjectId);
    if not FSValueSpace.GetHasContentsForId(ObjectId) then begin
      FSValueSpace.EnsureObjectContents(ObjectId);
      OwnContents := FSValueSpace.GetFSObjectContentsByObjectId(ObjectId);
      // When FSObjectContent is recreated for this block,
      // then Existence/PersistenceState must be adopted from source block.
      OwnContents.BoldExistenceState := ObjectContents.BoldExistenceState;
      OwnContents.BoldPersistenceState := ObjectContents.BoldPersistenceState;
    end else begin
      OwnContents := FSValueSpace.GetFSObjectContentsByObjectId(ObjectId);
    end;
    for j:= 0 to ObjectContents.MemberCount - 1 do
      if Assigned(ObjectContents.valueByIndex[j]) then
      begin
        if (OwnContents.MemberCount > j) and Assigned(OwnContents.ValueByIndex[j]) then
        begin
          if overwrite then
            OwnContents.ValueByIndex[j].AssignContent(ObjectContents.ValueByIndex[j]);
        end
        else
        begin
          OwnContents.EnsureMemberAndGetValueByIndex(J, ObjectContents.ValueByIndex[j].ContentName).AssignContent(ObjectContents.ValueByIndex[j]);
        end;
      end;
  end;
end;

function TBoldUndoBlock.ValueExists(const ObjectID: TBoldObjectID;
  const MemberIndex: integer; out Value: IBoldValue): Boolean;
var
  ObjectContents: TBoldFreeStandingObjectContents;
begin
  Value := nil;
  ObjectContents := FSValueSpace.GetFSObjectContentsByObjectId(ObjectID);
  if Assigned(ObjectContents) and (MemberIndex < ObjectContents.memberCount) then
    Value := ObjectContents.ValueByIndex[MemberIndex];
  Result := Assigned(Value);
end;

function TBoldUndoBlock.ValueExists(const ObjectID: TBoldObjectId;
  const MemberIndex: integer): Boolean;
var
  Value: IBoldValue;
begin
  result := ValueExists(ObjectId, MemberIndex, Value);
end;

procedure TBoldUndoBlock.HandleMember(const ObjectContents: IBoldObjectContents; MemberIndex: integer; const MemberValue: IBoldValue);
var
  ObjectId: TBoldObjectId;
  FSObjectContents: TBoldFreeStandingObjectContents;
begin
  ObjectId := ObjectContents.ObjectId;
  FSObjectContents := FSValueSpace.GetFSObjectContentsByObjectId(ObjectId);
  if not Assigned(FSObjectContents) then
  begin
    HandleObject(ObjectContents, true);
    FSObjectContents := FSValueSpace.GetFSObjectContentsByObjectId(ObjectId);
  end;
  if not Assigned(FSObjectContents) or (MemberIndex >= FSObjectContents.memberCount) or not Assigned(FSObjectContents.ValueByIndex[MemberIndex]) then
    begin
      FContainsChanges := true;
      if not Assigned(FSObjectContents) then
      begin
        FSObjectContents := FSValueSpace.GetEnsuredFSObjectContentsByObjectId(ObjectId);
        FSObjectContents.ApplyObjectContents(ObjectContents, False, false);
      end;
      FSObjectContents.EnsureMemberAndGetValueByIndex(MemberIndex, MemberValue.ContentName).AssignContent(MemberValue);
    end;
end;

function TBoldUndoBlock.HasObjectContentsForAnyObjectInList(
  const ObjectList: TBoldObjectList): Boolean;
var
  i: integer;
  ObjectId: TBoldObjectId;
begin
  Result := false;
  for i:= 0 to ObjectList.Count - 1 do
  begin
    ObjectId := ObjectList.BoldObjects[i].BoldObjectLocator.BoldObjectID;
    Result := Assigned(FSValueSpace.GetFSObjectContentsByObjectId(ObjectId));
    if Result then
      Break;
  end;
end;

// Not used currently
//procedure TBoldUndoBlock.GetLinksToObject(const ObjectId: TBoldObjectId; const OwnIndexInLinkClass: integer;
//  const SingleLinkClassTypeInfo: TBoldClassTypeInfo; SingleLinkIds: TBoldObjectIdList);
//var
//  ObjectIds: TBoldObjectIdList;
//  i: integer;
//  LinkValue: IBoldValue;
//begin
//  ObjectIds := TBoldObjectIdList.Create;
//  try
//    AllIdsInClass(SingleLinkClassTypeInfo, ObjectIds);
//    for i:= 0 to ObjectIds.Count - 1 do
//      if ValueExists(ObjectIds[i], OwnIndexInLinkClass, LinkValue) and
//        Assigned((LinkValue as IBoldObjectIdRef).Id) and
//        ((LinkValue as IBoldObjectIdRef).Id.IsEqual[ObjectId]) then
//           SingleLinkIds.Add(ObjectIds[i].Clone);
//  except
//    FreeAndNil(ObjectIds);
//  end;
//end;

procedure TBoldUndoBlock.AddObjectsToList(const AList: TBoldList);
var
  i: integer;
  ObjectIds: TBoldObjectIdList;
  Locators: TBoldSystemLocatorList;
  G: IBoldGuard;
begin
  G := TBoldGuard.Create(ObjectIds);
  ObjectIds := TBoldObjectIdList.Create;
  FSValueSpace.AllObjectIds(ObjectIds, false);
  Locators := FUndoBlockList.fBoldUndoHandler.System.Locators;
  for i:= 0 to ObjectIds.Count - 1 do
    AList.Add(Locators.ObjectByID[ObjectIds[i]]);
end;

// Not used currently
//procedure TBoldUndoBlock.AllIdsInClass(const ClassTypeInfo: TBoldClassTypeInfo; IdList: TBoldObjectIdList);
//var
//  i: integer;
//  ObjectIds: TBoldObjectIdList;
//  Locators: TBoldSystemLocatorList;
//  G: IBoldGuard;
//begin
//  G := TBoldGuard.Create(ObjectIds);
//  ObjectIds := TBoldObjectIdList.Create;
//  FSValueSpace.AllObjectIds(ObjectIds, True);
//  Locators := FUndoBlockList.fBoldUndoHandler.System.Locators;
//  for i:= 0 to ObjectIds.Count - 1 do
//  begin
//    if (Locators.ObjectByID[ObjectIds[i]].BoldClassTypeInfo = ClassTypeInfo) then
//      IdList.Add(ObjectIds[i]);
//  end;
//end;

procedure TBoldUndoBlock.HandleObject(const Obj: IBoldObjectContents; RegardAsExisting: Boolean);
var
  ObjectId: TBoldObjectId;
  ObjectContents: TBoldFreeStandingObjectContents;
begin
  Assert(assigned(obj));
  ObjectId := Obj.ObjectID;
  ObjectContents := FSValueSpace.GetFSObjectContentsByObjectId(ObjectId);
  if not Assigned(ObjectContents) then
  begin
    FContainsChanges := True;
    FSValueSpace.EnsureObjectContents(ObjectId);
    ObjectContents := FSValueSpace.GetFSObjectContentsByObjectId(ObjectId);
    ObjectContents.ApplyObjectContents(Obj, False, false);
    if RegardAsExisting then
    begin
      ObjectContents.BoldPersistenceState := bvpsCurrent;
      ObjectContents.BoldExistenceState := besExisting;
    end;
  end;
end;

destructor TBoldUndoBlock.Destroy;
begin
  FreeAndNil(FValueSpace);
  inherited;
end;

procedure TBoldUndoBlock.SetFSValueSpace(
  const Value: TBoldFreeStandingValueSpace);
begin
  if (FValueSpace <> Value) then
  begin
    FreeAndNil(FValueSpace);
    FValueSpace := Value;
    fContainsChanges := TRUE;
  end;
end;

function TBoldUndoBlock.GetName: string;
begin
  Result := fName;
end;

function TBoldUndoBlock.GetObjectCount: integer;
begin
  result := FSValueSpace.IdCount;
end;

function TBoldUndoBlock.GetValueSpace: IBoldValueSpace;
begin
  result := FSValueSpace as IBoldValueSpace;
end;

function TBoldUndoBlock.GetCaption: String;
begin
  result := fCaption;
  if (result = '') and ContainsChanges then
    result := GetChangesForObject(FValueSpace.GetAnyObjectId, false);
end;

function TBoldUndoBlock.GetContainsChanges: Boolean;
begin
  result := fContainsChanges;
end;

function TBoldUndoBlock.GetContent: String;
var
  sl: TStringList;
  ObjectIds: TBoldObjectIdList;
  G: IBoldGuard;
  I: Integer;
  s: string;
begin
  result := '';
  G := TBoldGuard.Create(ObjectIds, sl);
  ObjectIds := TBoldObjectIdList.Create;
  sl := TStringList.Create;
  if FContainsChanges then
  begin
    FSValueSpace.AllObjectIds(ObjectIds, false);
    for I := 0 to ObjectIDs.Count -1 do
    begin
      s := GetChangesForObject(ObjectIDs[i], true);
      if s <> '' then
        sl.Add(s);
    end;
    result := sl.Text;
  end;
end;

function TBoldUndoBlock.GetChangesForObject(ABoldObjectID: TBoldObjectId; AddHeader: boolean): string;
var
  vBoldObjectLocator: TBoldObjectLocator;
  vBoldObject: TBoldObject;
  vObjectContents: TBoldFreeStandingObjectContents;
  vClassType: TBoldClassTypeInfo;
  vBoldValue: IBoldValue;
  vMember: TBoldMember;
  i: integer;
  sl: TStringList;
  vBoldVariantReadable: IBoldVariantReadable;
  vBoldObjectIdRef: IBoldObjectIdRef;
  vBoldObjectIdRefPair: IBoldObjectIdRefPair;
  vBoldObjectIdListRef: IBoldObjectIdListRef;
  vBoldFreeStandingIdListPair: IBoldFreeStandingIdListPair;
  vObjectAsString : string;
  vValueAsString: string;
  vMemberAsString: string;

  function IdToString(AId: TBoldObjectId): string;
  begin
    if Assigned(AId) then
      result := aID.AsString
    else
     result := 'NIL';
  end;
begin
  result := '';
  sl := TStringList.Create;
  try
    vObjectContents := FValueSpace.GetFSObjectContentsByObjectId(ABoldObjectID);
    vBoldObjectLocator := FUndoBlockList.fBoldUndoHandler.System.Locators.LocatorByID[ABoldObjectID];
    vClassType := FUndoBlockList.fBoldUndoHandler.System.GetClassTypeForID(ABoldObjectId);
    if not Assigned(vBoldObjectLocator) then
    begin
      Sl.Add(Format('[%s]:%s deleted', [ABoldObjectID.AsString, vClassType.ExpressionName]));
    end
    else
    if not Assigned(vBoldObjectLocator.BoldObject) then
    begin
      Sl.Add(Format('[%s]:%s deleted', [ABoldObjectID.AsString, vClassType.ExpressionName]));
    end
    else
    begin
      vBoldObject := vBoldObjectLocator.BoldObject;
      vObjectAsString := vBoldObject.DisplayName;
      if (vObjectContents.BoldExistenceState = besNotCreated) then
        vObjectAsString := vObjectAsString + ' Created';
      for i := 0 to vObjectContents.MemberCount - 1 do
      begin
        vBoldValue := vObjectContents.ValueByIndex[i];
        if Assigned(vBoldValue) then
        begin
          vMember := vBoldObject.BoldMembers[i];
          if vMember.IsEqualToValue(vBoldValue) then
            continue;
          vMemberAsString := vMember.AsString;
          if Supports(vBoldValue, IBoldVariantReadable, vBoldVariantReadable) then
            vValueAsString := vBoldVariantReadable.asString
          else
          if Supports(vBoldValue, IBoldObjectIdRef, vBoldObjectIdRef) then
          begin
            vValueAsString := IdToString(vBoldObjectIdRef.Id);
            if Assigned((vMember as TBoldObjectReference).Locator) then
              vMemberAsString := (vMember as TBoldObjectReference).Locator.BoldObjectID.AsString
            else
              vMemberAsString := 'NIL';
          end
          else
          if Supports(vBoldValue, IBoldObjectIdRefPair, vBoldObjectIdRefPair) then
          begin
            vValueAsString := IdToString(vBoldObjectIdRefPair.Id1) + ',' + IdToString(vBoldObjectIdRefPair.Id2);
          end
          else
          if Supports(vBoldValue, IBoldObjectIdListRef, vBoldObjectIdListRef) then
          begin
            vValueAsString := IntToStr(vBoldObjectIdListRef.Count);
            vMemberAsString := (vMember as TBoldObjectList).AsString;
          end
          else
          if Supports(vBoldValue, IBoldFreeStandingIdListPair, vBoldFreeStandingIdListPair) then
          begin
            Assert(false, 'Support for IBoldFreeStandingIdListPair not implemented yet.');//vBoldFreeStandingIdListPair.
          end
          else
            Assert(false);
          if AddHeader then
            Sl.Add(Format('  %s: ''%s''->''%s''', [vMember.BoldMemberRTInfo.ExpressionName, vValueAsString, vMemberAsString]))
          else
            Sl.Add(Format('%s: ''%s''->''%s''', [vMember.BoldMemberRTInfo.DisplayName, vValueAsString, vMemberAsString]));
        end;
      end;
//      if AddHeader and (sl.Count>0) then
      Sl.Insert(0, vObjectAsString{+#13#10});
    end;
  finally
    result := Trim(sl.Text);
    sl.free;
  end;
end;

function TBoldUndoBlock.GetCreated: TDateTime;
begin
  result := fCreated;
end;

procedure TBoldUndoBlock.ApplytranslationList(
  IdTranslationList: TBoldIdTranslationList);
begin
  ValueSpace.ApplytranslationList(IdTranslationList);
end;

{ TBoldUndoHandler }

constructor TBoldUndoHandler.Create(System: TBoldSystem);
begin
  inherited;
  FUndoBlocks := TBoldUndoBlockList.Create(self);
  FRedoBlocks := TBoldUndoBlockList.Create(self);
  fUndoState := busNormal;
  ReuseEmptyBlocks := true; // default Bold behavior
  Enabled := false;
end;

destructor TBoldUndoHandler.Destroy;
begin
  FreeAndNil(FUndoBlocks);
  FreeAndNil(FRedoBlocks);
  inherited;
end;

procedure TBoldUndoHandler.HandleMember(const ObjectContents: IBoldObjectContents; MemberIndex: integer; const MemberValue: IBoldValue);
begin
  if Enabled and (UndoState = busNormal) then
  begin
    RedoBlocks.Clear;
    CurrentUndoBlock.HandleMember(ObjectContents, MemberIndex, MemberValue);
  end;
end;

function TBoldUndoBlockList.GetContainsChanges: Boolean;
var
  i: integer;
begin
  result := true;
  for I := Count - 1 downto 0 do
    if BlockByIndex[i].ContainsChanges then
      exit;
  result := false;
end;

function TBoldUndoBlockList.GetCount: integer;
begin
  Result := fList.Count;
end;

function TBoldUndoBlockList.GetBlockByIndex(
  Index: integer): TBoldUndoBlock;
begin
  if (Index >= 0) and (Index < FList.Count) then
    Result := FList.Objects[Index] as TBoldUndoBlock
  else
    Result := nil;
end;

function TBoldUndoBlockList.GetBlockByName(const BlockName: string): TBoldUndoBlock;
var
  Idx: integer;
begin
  Idx := FList.IndexOf(blockName);
  if Idx <> - 1 then
    Result := FList.Objects[Idx] as TBoldUndoBlock
  else
    Result := nil;
end;


procedure TBoldUndoHandler.PrepareUpdate(const ObjectList: TBoldObjectList);
var
  aBlock: TBoldUndoBlock;
  i: integer;
begin
  if not Enabled then
    exit;
  RedoBlocks.Clear;
  i := 0;
  while i < UnDoBlocks.Count do
  begin
    aBlock := UndoBlocks.BlockByIndex[i];
    if aBlock.HasObjectContentsForAnyObjectInList(ObjectList) then
    begin
      UnDoBlocks.InternalRemoveBlock(aBlock)
    end
    else
      inc(i);
  end;
  UndoBlocks.MergeAll; // this was not original behavior
  SendExtendedEvent(self, beUndoChanged, []);
end;

procedure TBoldUndoHandler.UndoBlock(const BlockName: string);
begin
  if System.InTransaction then
    raise EBold.CreateFmt(sCannotUndoInTransaction, [ClassName, 'UndoBlock']);
  UndoState := busUndoing;
  try
    DoUndoInTransaction(BlockName, UndoBlocks, RedoBlocks);
  finally
    UndoState := busNormal;
    SendExtendedEvent(self, beUndoBlock, [BlockName]);
  end;
end;


procedure TBoldUndoHandler.RedoBlock(const BlockName: string);
begin
  if System.InTransaction then
    raise EBold.CreateFmt(sCannotUndoInTransaction, [ClassName, 'RedoBlock']);
  UndoState := busRedoing;
  try
    DoUndoInTransaction(BlockName, RedoBlocks, UndoBlocks);
  finally
    UndoState := busNormal;
    SendExtendedEvent(self, beRedoBlock, [BlockName]);
  end;
end;

procedure TBoldUndoHandler.RedoLatest;
begin
  if (RedoBlocks.Count > 0) then
  begin
    if not CurrentUndoBlock.ContainsChanges then
      CurrentUndoBlock.FUndoBlockList.RemoveBlock(CurrentUndoBlock.FName);
    RedoBlock(RedoBlocks.CurrentBlock.BlockName);
  end;
end;

procedure TBoldUndoHandler.UndoLatest;
begin
  if (UndoBlocks.Count > 0) and (UndoBlocks.GetFirstNonEmptyBlock <> nil) then
  begin
    if not CurrentUndoBlock.ContainsChanges then
      CurrentUndoBlock.FUndoBlockList.RemoveBlock(CurrentUndoBlock.FName);
    UndoBlock(UndoBlocks.GetFirstNonEmptyBlock.FName {CurrentUndoBlock.BlockName});
  end;
end;

procedure TBoldUndoHandler.HandleObject(const Obj: IBoldObjectContents; RegardAsExisting: Boolean);
begin
  if Enabled and (UndoState = busNormal) then
  begin
    RedoBlocks.Clear;
    CurrentUndoBlock.HandleObject(Obj, RegardAsExisting);
  end;
end;

procedure TBoldUndoHandler.DoUndo(UnDoValueSpace: TBoldFreeStandingValueSpace;
                                   RedoValueSpace: TBoldFreeStandingValueSpace);
type
  TObjectAction=(oaDelete, oaExisingPersistent, oaNewPersistent, oaTransient, oaUse);
var
  ObjectIds: TBoldObjectIdList;
  ModifiedMembers: TBoldMemberList;

    procedure GetInnerLinkIndices(BoldObject: TBoldObject; var MemberIndex1, MemberIndex2: Integer);
  var
    RoleRTInfo: TBoldRoleRTInfo;
  begin
    MemberIndex1 := -1;
    MemberIndex2 := -1;
    for RoleRTInfo in BoldObject.BoldClassTypeInfo.AllRoles do
    begin
      if RoleRTInfo.RoleType = rtInnerLinkRole then
      begin
        if (MemberIndex1 = -1) then
          MemberIndex1 := RoleRTInfo.index
        else if (MemberIndex2 = -1) then
          MemberIndex2 := RoleRTInfo.index
        else
          Break;
      end;
    end;
  end;

  procedure UnDoInnerLinks(BoldObject: TBoldObject;  ObjectContents: IBoldObjectContents);
  var
    Member0, Member1: TBoldObjectReference;
    MemberIndex0, MemberIndex1: integer;
    Value0, Value1: IBoldObjectIdRef;
    NilIdValue: TBFSObjectIdRef;
    SavIdValue:  TBFSObjectIdRef;
    G: IBoldGuard;
  begin
    G := TBoldGuard.Create(NilIdValue, SavIdValue);
    GetInnerLinkIndices(BoldObject, MemberIndex0, MemberIndex1);
    Member0 := BoldObject.BoldMembers[MemberIndex0] as TBoldObjectReference;
    Member1 :=  BoldObject.BoldMembers[MemberIndex1] as TBoldObjectReference;
    Assert(Member0.BoldRoleRTInfo.RoleType = rtInnerLinkRole);
    Assert(Member1.BoldRoleRTInfo.RoleType = rtInnerLinkRole);

    if ObjectContents.BoldExistenceState = besExisting then
    begin
      Value0 := ObjectContents.valueByIndex[MemberIndex0] as IBoldObjectIdRef;
      Value1 := ObjectContents.valueByIndex[MemberIndex1] as IBoldObjectIdRef;
      if Assigned(Value0) and Assigned(Value1) then
      begin


        SavIdValue := TBFSObjectIdRef.Create;
        SavIdValue.AssignContent(Member1.AsIBoldValue[bdepContents]);
        Member1.AsIBoldValue[bdepContents].AssignContent(Value1);
        Member0.AsIBoldValue[bdepUnDo].AssignContent(Value0);
        Member1.AsIBoldValue[bdepContents].AssignContent(SavIdValue);
        Member1.AsIBoldValue[bdepUnDo].AssignContent(Value1);
        ModifiedMembers.Add(Member0);
        ModifiedMembers.Add(Member1);
      end
      else if Assigned(Value0) then
      begin
        Member0.AsIBoldValue[bdepUnDO].AssignContent(Value0);
        ModifiedMembers.Add(Member0);
      end
      else if Assigned(Value1) then
      begin
        Member1.AsIBoldValue[bdepUnDO].AssignContent(Value1);
        ModifiedMembers.Add(Member1);
      end;
    end
    else
    begin
      NilIdValue := TBFSObjectIdRef.Create;
      NilIdValue.BoldPersistenceState := bvpsCurrent;
      Member0.AsIBoldValue[bdepUnDO].AssignContent(NilIdValue);
      Member1.AsIBoldValue[bdepUnDO].AssignContent(NilIdValue);
      ModifiedMembers.Add(Member0);
      ModifiedMembers.Add(Member1);
    end;
    Value0 := nil;
    Value1 := nil;
  end;

  procedure UnDoObjectAndAttributes(BoldObject: TBoldObject; ObjectContents: IBoldObjectContents);
  var
    i: Integer;
    aValue: IBoldValue;
    BoldMember: TBoldMember;
  begin
    assert(ObjectContents.BoldExistenceState = besExisting);
    BoldObject.AsIBoldObjectContents[bdepContents].BoldPersistenceState := ObjectContents.BoldPersistenceState;
    BoldObject.AsIBoldObjectContents[bdepContents].BoldExistenceState := ObjectContents.BoldExistenceState;
    for i := 0 to BoldObject.BoldMemberCount - 1 do
    begin
      aValue := ObjectContents.valueByIndex[i];
      if assigned(aValue) then
      begin
        BoldMember := BoldObject.BoldMembers[i];
        if BoldMember is TBoldAttribute then
        begin
          BoldMember.AsIBoldValue[bdepUndo].AssignContent(aValue);
          ModifiedMembers.Add(BoldMember);
        end;
      end;
    end;
  end;

  procedure UnDoLinks(BoldObject: TBoldObject; ObjectContents: IBoldObjectContents);
  var
    aValue: IBoldValue;
    BoldMember: TBoldMember;
    RoleRTInfo: TBoldRoleRTInfo;
  begin
    for RoleRTInfo in BoldObject.BoldClassTypeInfo.AllRoles do
    begin
      if RoleRTInfo.IsSingleRole and (RoleRTInfo.RoleType = rtRole) then
      begin
        aValue := ObjectContents.valueByIndex[RoleRTInfo.Index];
        if assigned(aValue) then
        begin
          BoldMember := BoldObject.BoldMembers[RoleRTInfo.Index];
          BoldMember.AsIBoldValue[bDepUndo].AssignContent(aValue);
          ModifiedMembers.Add(BoldMember);
        end;
      end;
    end;
    if BoldObject.BoldClassTypeInfo.IsLinkClass then
      UnDoInnerLinks(BoldObject, ObjectContents);
  end;

  procedure SaveOldValues;
  var
    oid: TBoldObjectId;
    FSObjectContents: TBoldFreeStandingObjectContents;
    FSRedoObjectContents: TBoldFreeStandingObjectContents;
    i, M: integer;
    Value: IBoldValue;
    BoldObject: TBoldObject;
  begin
    for i:= 0 to ObjectIds.Count - 1 do
    begin
      oid := ObjectIds[i];
      RedoValueSpace.EnsureObjectContents(oid);
      BoldObject := System.Locators.ObjectByID[oid];
      FSObjectContents := UndoValueSpace.GetFSObjectContentsByObjectId(oid);
      if Assigned(BoldObject) then
      begin
        if FSObjectContents.BoldExistenceState = besExisting then
        begin
          RedoValueSpace.GetFSObjectContentsByObjectId(oid).ApplyObjectContents(
                FSObjectContents, true, false);
          RedoValueSpace.GetFSObjectContentsByObjectId(oid).UpdateObjectContentsFrom(BoldObject.AsIBoldObjectContents[bdepContents])
        end
        else
        begin
          RedoValueSpace.GetFSObjectContentsByObjectId(oid).ApplyObjectContents(BoldObject.AsIBoldObjectContents[bdepContents], False, false);
          for M := 0 to BoldObject.BoldMemberCount -1 do
            if BoldObject.BoldMemberAssigned[M] and BoldObject.BoldMembers[M].StoreInUndo then
            begin
              FSRedoObjectContents := RedoValueSpace.GetFSObjectContentsByObjectId(oid);
              if FSRedoObjectContents is TBoldSystemFreeStandingObjectContents then
                Value := TBoldSystemFreeStandingObjectContents(FSRedoObjectContents).EnsureMemberAndGetValueByIndex(BoldObject.BoldMembers[M])
              else
                Value := FSRedoObjectContents.EnsureMemberAndGetValueByIndex(M, BoldObject.BoldMembers[M].AsIBoldValue[bdepContents].ContentName);
              Value.AssignContent(BoldObject.BoldMembers[M].AsIBoldValue[bdepContents]);
            end;
        end;
      end
      else
      begin
        RedoValueSpace.GetFSObjectContentsByObjectId(oid).BoldExistenceState := besNotCreated;
        RedoValueSpace.GetFSObjectContentsByObjectId(oid).BoldPersistenceState := bvpsCurrent;
      end;
    end;
  end;

  function GetObjectAction(  FSObjectContents: TBoldFreeStandingObjectContents;   BoldObject: TBoldObject): TObjectAction;
  begin
    result := oaUse;
    if Assigned(BoldObject) then
    begin
     if FSObjectContents.BoldExistenceState = besExisting then
        Result := oaUse
      else
        Result := oaDelete;
    end
    else
    begin
      case FSObjectContents.BoldExistenceState of
      besExisting:
        case FSObjectContents.BoldPersistenceState of
        bvpsCurrent:
          Result := oaExisingPersistent;
        bvpsModified:
          Result := oaNewPersistent;
        bvpsTransient:
          Result := oaTransient;
        else
          raise EBold.create(sInternalError);
        end;
      besNotCreated, besDeleted:
        case FSObjectContents.BoldPersistenceState of
        bvpsCurrent, bvpsModified, bvpsTransient:
          Result := oaDelete;
        else
          raise EBold.create(sInternalError);
       end;
     end;
   end;
  end;

var
  oid: TBoldObjectId;
  FSObjectContents: TBoldFreeStandingObjectContents;
  i: integer;
  BoldObject: TBoldObject;
  ObjectAction: TObjectAction;
  ClassName: string;
  G: IBoldGuard;
begin
  G := TBoldGuard.Create(ObjectIds, ModifiedMembers);
  ObjectIds := TBoldObjectIdList.Create;
  UndoValueSpace.AllObjectIds(ObjectIds, false);
  SaveOldValues;
  ModifiedMembers := TBoldMemberList.Create;
  ModifiedMembers.CloneMembers := false;
  for i:= 0 to ObjectIds.Count - 1 do
  begin
    oid := ObjectIds[i];
    RedoValueSpace.EnsureObjectContents(oid);
    BoldObject := System.Locators.ObjectByID[oid];
    FSObjectContents := UndoValueSpace.GetFSObjectContentsByObjectId(oid);
    ObjectAction := GetObjectAction(FSObjectContents, BoldObject);
    if not Assigned(BoldObject) then
    begin
      ClassName := System.BoldSystemTypeInfo.TopSortedClasses[oid.TopSortedIndex].ExpressionName;
      if ObjectAction = oaExisingPersistent then
        BoldObject := System.CreateExistingObjectByID(oid)
      else
      begin
        if ObjectAction = oaNewPersistent then
          BoldObject := System.CreateNewObjectByExpressionName(ClassName, true)
        else if ObjectAction = oaTransient then
          BoldObject := System.CreateNewObjectByExpressionName(ClassName, false);
        if (ObjectAction <> oaDelete) then
          System.Locators.UpdateId(BoldObject.BoldObjectLocator, oid, True);
      end
    end;
    if ObjectAction <> oaDelete then
      UnDoObjectAndAttributes(BoldObject, UndoValueSpace.GetFSObjectContentsByObjectId(oid));
  end;
  for i:= 0 to ObjectIds.Count - 1 do
  begin
    oid := ObjectIds[i];
    RedoValueSpace.EnsureObjectContents(oid);
    BoldObject := System.Locators.ObjectByID[oid];
    if Assigned(BoldObject) then
      UnDoLinks(BoldObject, UndoValueSpace.GetFSObjectContentsByObjectId(oid));
  end;

  for i:= 0 to ObjectIds.Count - 1 do
  begin
    oid := ObjectIds[i];
    BoldObject := System.Locators.ObjectByID[oid];
    FSObjectContents := UndoValueSpace.GetFSObjectContentsByObjectId(oid);
    if Assigned(BoldObject) and (BoldObject.BoldExistenceState = besExisting) and (GetObjectAction(FSObjectContents, BoldObject) = oaDelete) then
    begin
      BoldObject.AsIBoldObjectContents[bdepContents].BoldExistenceState := besDeleted;
      BoldObject.AsIBoldObjectContents[bdepContents].BoldPersistenceState := FSObjectContents.BoldPersistenceState;
      DeleteObject(BoldObject);
    end;
  end;

  for i:= 0 to ModifiedMembers.Count - 1 do
    ModifiedMembers[i].SendExtendedEvent(beValueInvalid, []);
end;

function TBoldUndoBlockList.IndexOf(const BlockName: string): integer;
begin
  Result := fList.IndexOf(BlockName);
end;

function TBoldUndoBlockList.IndexOf(Block: TBoldUndoBlock): integer;
begin
  Result := fList.IndexOfObject(Block);
end;

procedure TBoldUndoHandler.DoUndoInTransaction(BlockName: string;
  FromList, ToList: TBoldUndoBlockList);
var
  aBlock: TBoldUndoBlock;
  RedoValueSpace: TBoldFreeStandingValueSpace;
  BlockCaption: string;
  G: IBoldGuard;
begin
  G := TBoldGuard.Create(RedoValueSpace);
  aBlock := FromList.BlockByName[BlockName];
  if Not Assigned(aBlock) then
    raise EBold.CreateFmt(sInvalidBlockName, [BlockName]);
  if not FromList.CanMoveToTop(FromList.IndexOf(BlockName)) then
    raise EBold.CreateFmt(sCannotMoveToTop, [BlockName]);

  BlockCaption := aBlock.Caption;
  RedoValueSpace := TBoldFreeStandingValueSpace.Create;
  System.StartTransaction;
  try
    DoUndo(aBlock.FSValueSpace, RedoValueSpace);
    FromList.InternalRemoveBlock(BlockName);
    if aBlock.ContainsChanges then  // do no transfer empty blocks
      ToList.AddBlock(BlockName, BlockCaption).FSValueSpace := RedoValueSpace;
    RedoValueSpace := nil;
    System.CommitTransaction;
  except
    System.RollbackTransaction;
  end;
end;

function TBoldUndoHandler.CanRedoBlock(const BlockName: string): Boolean;
begin
  Result := RedoBlocks.CanMoveToTop(RedoBlocks.AssertedIndexOf(BlockName));
end;

function TBoldUndoHandler.CanUndoBlock(const BlockName: string): Boolean;
begin
  Result := UnDoBlocks.CanMoveToTop(UndoBlocks.AssertedIndexOf(BlockName));
end;

function TBoldUndoHandler.GetCurrentUndoBlock: TBoldUndoBlock;
begin
  if UndoBlocks.IsEmpty then
    SetCheckPoint;
  result := UndoBlocks.CurrentBlock;
end;

function TBoldUndoHandler.GetCurrentUndoBlockCaption: string;
begin
  result := '';
  if UndoBlocks.GetFirstNonEmptyBlock <> nil then
    result := UndoBlocks.GetFirstNonEmptyBlock.Caption;
end;

function TBoldUndoHandler.GetCurrentUndoBlockHasChanges: boolean;
begin
  result := false;
  if not UndoBlocks.IsEmpty then
   result := GetCurrentUndoBlock.ContainsChanges;
end;

function TBoldUndoHandler.GetEnabled: Boolean;
begin
  result := fEnabled;
end;

function TBoldUndoHandler.GetIsEmpty: boolean;
begin
  result := UndoBlocks.Count = 0;
end;

function TBoldUndoHandler.GetRedoList: IBoldUndoList;
begin
  Result := FRedoBlocks;
end;

function TBoldUndoHandler.GetUndoList: IBoldUndoList;
begin
  Result := FUndoBlocks;
end;

function TBoldUndoHandler.GetUniqueBlockName: string;
var
  i: integer;
  s: string;
begin
   s := cUnNamedBlockName;
   i := 2;
   Result := s;
   while ((UndoBlocks.IndexOf(Result) <> -1) or (RedoBlocks.IndexOf(Result) <> -1)) do
   begin
     Result := Format('%s %d', [s, i]);
     inc(i);
   end;
end;

{function TBoldUndoHandler.GetNonUndoableBlock: IBoldUndoBlock;
begin
  Result := FNonUndoableBlock;
end;
}

procedure TBoldUndoHandler.ApplytranslationList(
  IdTranslationList: TBoldIdTranslationList);
begin
  UndoBlocks.ApplytranslationList(IdTranslationList);
  RedoBlocks.ApplytranslationList(IdTranslationList);
end;

function TBoldUndoHandler.SetCheckPoint(const ACaption: string): string;
begin
  if ReuseEmptyBlocks and (not UndoBlocks.IsEmpty and not UndoBlocks.CurrentBlock.ContainsChanges) then
  begin
    result := CurrentUndoBlock.BlockName;
    CurrentUndoBlock.fCaption := ACaption;
  end
  else
  begin
    result := GetUniqueBlockName;
    UndoBlocks.AddBlock(result, ACaption);
    SendExtendedEvent(self, beUndoSetCheckpoint, [result]);
  end;
end;

procedure TBoldUndoHandler.ClearAllUndoBlocks;
begin
  FUndoBlocks.Clear;
  FRedoBlocks.Clear;
  SendExtendedEvent(self, beUndoChanged, []);
end;

procedure TBoldUndoHandler.ClearCurrentUndoBlock;
begin
  FUndoBlocks.RemoveBlock(FUndoBlocks.CurrentBlock.BlockName);
  SendExtendedEvent(self, beUndoChanged, []);
end;

procedure TBoldUndoHandler.SetEnabled(value: Boolean);
begin
  if fEnabled <> value then
  begin
    fEnabled := value;
    SendExtendedEvent(self, beUndoChanged, [])
  end;
end;

{ TBoldUndoBlockList }

function TBoldUndoBlockList.AddBlock(var BlockName: string; const ACaption: string;
  const FSVAlueSpace: TBoldFreeStandingValueSpace): TBoldUndoBlock;
var
  Idx: integer;
begin
  if FList.IndexOf(BlockName) = -1 then
  begin
    Idx := FList.Add(BlockName);
    Result := TBoldUndoBlock.CreateNamedBlock(self, BlockName, ACaption, FSVAlueSpace);
    FList.Objects[Idx] := Result;
  end
  else
    raise EBold.CreateFmt(sBlockNameInUse, [ClassName, BlockName]);
end;

function TBoldUndoBlockList.CanMoveBlock(CurIndex,
  NewIndex: integer): Boolean;
var
  i: integer;
  CurBlock, NewBlock: TBoldUndoBlock;
begin
  CurBlock := AssertedBlockByIndex[CurIndex];
  NewBlock := AssertedBlockByIndex[NewIndex];
  Result := (CurIndex = NewIndex) or not CurBlock.IsDependantOn(NewBlock);
  if Result then
  begin
    if (CurIndex < NewIndex) then
      for i:= CurIndex + 1 to NewIndex do
      begin
        Result := not CurBlock.IsDependantOn(BlockByIndex[i]);
        if not Result then
          Break;
      end
    else
      for i:= CurIndex - 1 downto NewIndex do
      begin
        Result := not CurBlock.IsDependantOn(BlockByIndex[i]);
        if not Result then
          Break;
      end;
  end;
end;

procedure TBoldUndoBlockList.Clear;
var
  i: integer;
  aBlock: TBoldUndoBlock;
begin
  for i:= 0 to FList.Count - 1 do
  begin
    aBlock := FList.Objects[i] as TBoldUndoBlock;
    FreeAndNil(aBlock);
  end;
  FList.Clear;
end;

constructor TBoldUndoBlockList.Create(ABoldUndoHandler: TBoldUndoHandler);
begin
  fBoldUndoHandler := ABoldUndoHandler;
  FList := TStringList.Create;
end;

destructor TBoldUndoBlockList.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

procedure TBoldUndoBlockList.MoveBlock(CurIndex,
  NewIndex: integer);
begin
  if CurIndex = NewIndex then
    exit;
  if not CanMoveBlock(CurIndex, NewIndex) then
    raise EBold.Create(sCannotMoveBlock);
   FList.Move(CurIndex, NewIndex);
end;

procedure TBoldUndoBlockList.MoveToTop(const BlockName: string);
begin
  MoveBlock(IndexOf(BlockName), FList.Count - 1);
end;

procedure TBoldUndoBlockList.InternalRemoveBlock(Block: TBoldUndoBlock);
var
  idx: integer;
begin
  idx := IndexOf(Block);
  FList.Delete(idx);
  FreeAndNil(Block);
end;

procedure TBoldUndoBlockList.InternalRemoveBlock(const BlockName: string);
var
  idx: integer;
  Block: TBoldUndoBlock;
begin
  idx := AssertedIndexOf(BlockName);
  Block := FList.Objects[idx] as TBoldUndoBlock;
  InternalRemoveBlock(Block);
end;

procedure TBoldUndoBlockList.RenameBlock(const OldName, NewName: string);
var
  aBlock: TBoldUndoBlock;
begin
  if Assigned(GetBlockByName(NewName)) then
    raise EBold.CreateFmt(sCannotRenameBlock, [NewName]);
  aBlock := AssertedBlockByName[OldName];
  FList.Strings[IndexOf(OldName)] := NewName;
  aBlock.FName := NewName;
end;

procedure TBoldUndoBlockList.MergeBlocks(const DestinationBlockName, SourceBlockName: string);
var
  DestinationBlock, SourceBlock: TBoldUndoBlock;
begin
  DestinationBlock := AssertedBlockByName[DestinationBlockName];
  SourceBlock := AssertedBlockByName[SourceBlockName];
  if not CanMergeBlock(IndexOf(SourceBlockName), IndexOf(DestinationBlockName)) then
    raise EBold.Create(sCannotMergeBlocks);
  DestinationBlock.Merge(SourceBlock, IndexOf(DestinationBlockName) > IndexOf(SourceBlockName));
  InternalRemoveBlock(SourceBlock);
end;

function TBoldUndoBlockList.CanMergeBlock(CurIndex,
  NewIndex: integer): Boolean;
var
  i: integer;
  CurBlock, NewBlock: TBoldUndoBlock;
begin
  Result := true;
  CurBlock := AssertedBlockByIndex[CurIndex];
  NewBlock := AssertedBlockByIndex[CurIndex];
  if CurIndex < NewIndex then
  begin
    for i:= CurIndex + 1 to NewIndex - 1 do
    begin
      Result := not CurBlock.IsDependantOn(BlockByIndex[i]);
      if not Result then
        Break;
    end
  end
  else if CurIndex > NewIndex then
    for i:= CurIndex - 1 downto NewIndex + 1 do
    begin
      Result := not NewBlock.IsDependantOn(BlockByIndex[i]);
      if not Result then
        Break;
    end;
end;

function TBoldUndoBlockList.GetCurrentBlock: TBoldUndoBlock;
begin
  if Count > 0 then
    Result := FList.Objects[Count - 1] as TBoldUndoBlock
  else
  begin
    fBoldUndoHandler.SetCheckPoint();
    Result :=  fBoldUndoHandler.CurrentUndoBlock;
  end;
end;

function TBoldUndoBlockList.GetAssertedBlockByName(
  const BlockName: string): TBoldUndoBlock;
begin
  Result := BlockByName[BlockName];
  if not Assigned(Result) then
    raise EBold.CreateFmt(sNoSuchBlock, [BlockName]);
end;

function TBoldUndoBlockList.CanMoveToTop(CurIndex: integer): Boolean;
begin
  Result := CanMoveBlock(CurIndex, FList.Count-1);
end;

function TBoldUndoBlockList.AssertedIndexOf(
  const BlockName: string): integer;
begin
  Result := IndexOf(BlockName);
  if Result = -1 then
    raise EBold.CreateFmt(sNoSuchBlock, [BlockName]);
end;

function TBoldUndoBlockList.GetItemByName(const Name: string): IBoldUndoBlock;
begin
  result := BlockByName[Name] as IBoldUndoBlock;
end;

function TBoldUndoBlockList.GetIsEmpty: boolean;
begin
  result := Count = 0;
end;

function TBoldUndoBlockList.GetItem(Index: integer): IBoldUndoBlock;
begin
  Result := BlockByIndex[Index] as IBoldUndoBlock ;
end;

function TBoldUndoBlockList.GetTopBlock: IBoldUndoBlock;
begin
  result := CurrentBlock as IBoldUndoBlock;
end;

procedure TBoldUndoBlockList.ApplytranslationList(
  IdTranslationList: TBoldIdTranslationList);
var
  i: integer;
begin
  for i := 0 to count-1 do
    BlockByIndex[i].ApplytranslationList(IdTranslationList);
end;

procedure TBoldUndoBlockList.MergeAll;
var
  i: integer;
  aBlock: TBoldUndoBlock;
begin
  i:= Count - 1;
  while i > 0 do
  begin
    aBlock := BlockByIndex[i];
    BlockByIndex[i - 1].Merge(aBlock, True);
    InternalRemoveBlock(aBlock);
    dec(i);
  end;
end;

procedure TBoldUndoBlockList.GetDependantBlocks(const BlockName: string;
  DependantBlocks: TList);
var
  i, j, b: integer;
  aBlock, CurBlock: TBoldUndoBlock;
  ObjectContents: TBoldFreeStandingObjectContents;
  aValue: IBoldValue;
  ObjectIds: TBoldObjectIdList;
  G: IBOldGuard;
begin
  G := TBoldGuard.Create(ObjectIds);
  aBlock := GetAssertedBlockByName(BlockName);
  if Assigned(aBlock) then
  begin
    if not Assigned(DependantBlocks) then
      raise EBold.CreateFmt(sParameterNotDefined, [ClassName]);
    ObjectIds := TBoldObjectIdList.Create;
    aBlock.FSValueSpace.AllObjectIds(ObjectIds, true);
    DependantBlocks.Clear;
    for i:= 0 to ObjectIds.Count - 1 do
    begin
      ObjectContents := aBlock.FSValueSpace.GetFSObjectContentsByObjectId(ObjectIds.ObjectIds[0]);
      for j:= 0 to ObjectContents.MemberCount - 1 do
        for  b := AssertedIndexOf(aBlock.BlockName)-1 downto 0 do
        begin
          CurBlock := BlockByIndex[b];
          if Assigned(ObjectContents.ValueByIndex[j]) and CurBlock.ValueExists(ObjectIds.ObjectIds[i], j, aValue)
              and (DependantBlocks.IndexOf(CurBlock) = - 1) then
            DependantBlocks.Add(CurBlock);
        end;
    end;
  end;
end;

function TBoldUndoBlockList.GetFirstNonEmptyBlock: TBoldUndoBlock;
begin
  if CurrentBlock.ContainsChanges then
    result := CurrentBlock
  else
  if self.Count > 1 then
    result := self.BlockByIndex[self.Count-2] // 2nd from the end
  else
    result := nil;
end;

function TBoldUndoBlockList.GetAssertedBlockByIndex(
  Index: integer): TBoldUndoBlock;
begin
  Result := BlockByIndex[Index];
  if not Assigned(Result) then
    raise EBold.CreateFmt(sNoSuchBlockIndex, [Index]);
end;

function TBoldUndoBlockList.RemoveBlock(const BlockName: string): Boolean;
var
  idx: integer;
begin
  idx := AssertedIndexOf(BlockName);
  Result := CanMoveToTop(idx);
  if Result then
    InternalRemoveBlock(BlockName);
end;

end.

