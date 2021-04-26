
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
    FValueSpace: TBoldFreeStandingValueSpace;
    FContainsChanges: Boolean;
    procedure GetLinksToObject(const System: TBoldSystem; const ObjectId: TBoldObjectId; const OwnIndexInLinkClass: integer;
      const SingleLinkClassTypeInfo: TBoldClassTypeInfo; SingleLinkIds: TBoldObjectIdList);
    procedure AllIdsInClass(const System: TBoldSystem; const ClassTypeInfo: TBoldClassTypeInfo; IdList: TBoldObjectIdList);
    function GetFSValueSpace: TBoldFreeStandingValueSpace; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    procedure SetFSValueSpace(const Value: TBoldFreeStandingValueSpace); {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    function GetName: string; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    function GetValueSpace: IBoldValueSpace; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    function GetContainsChanges: Boolean; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
  protected
    function HasObjectContentsForAnyObjectInList(const ObjectList: TBoldObjectList): Boolean;
    procedure HandleMember(const ObjectContents: IBoldObjectContents; MemberIndex: integer; const MemberValue: IBoldValue);
    procedure HandleObject(const Obj: IBoldObjectContents; RegardAsExisting: Boolean);
    function IsDependantOn(Block: TBoldUndoBlock): Boolean;
  public
    constructor CreateNamedBlock(const BlockName: string; const FSVAlueSpace: TBoldFreeStandingValueSpace = nil);
    destructor Destroy; override;
    procedure ApplytranslationList(IdTranslationList: TBoldIdTranslationList); {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    procedure Merge(Block: TBoldUndoBlock; const Overwrite: Boolean);
    function ValueExists(const ObjectID: TBoldObjectId; const MemberIndex: integer): Boolean; overload;
    function ValueExists(const ObjectID: TBoldObjectID; const MemberIndex: integer; out Value: IBoldValue): Boolean; overload;
    procedure AddObjectsToList(const System: TBoldSystem; const AList: TBoldList);
    property BlockName: string read FName;
    property FSValueSpace: TBoldFreeStandingValueSpace read GetFSValueSpace write setFSVAlueSpace;
    property ValueSpace: IBoldValueSpace read GetValueSpace;
    property ContainsChanges: Boolean read GetContainsChanges;
  end;

  TBoldUndoBlockList = class(TBoldNonRefCountedObject, IBoldUndoList)
  private
    FList: TStringList;
    function GetBlockByIndex(Index: integer): TBoldUndoBlock;
    function GetBlockByName(const BlockName: string): TBoldUndoBlock;
    function GetCount: integer;
    function GetCurrentBlock: TBoldUndoBlock;
    function GetAssertedBlockByName(const BlockName: string): TBoldUndoBlock;
    function GetAssertedBlockByIndex(Index: integer): TBoldUndoBlock;
    function GetItem(Index: integer): IBoldUndoBlock; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    function GetItemByName(const Name: string): IBoldUndoBlock; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    function GetTopBlock: IBoldUndoBlock; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    function GetContainsChanges: Boolean;
  protected
    procedure Clear;
    function AddBlock(const BlockName: string; const FSVAlueSpace: TBoldFreeStandingValueSpace = nil): TBoldUndoBlock;
    property AssertedBlockByName[const BlockName: string]: TBoldUndoBlock read GetAssertedBlockByName;
    property AssertedBlockByIndex[Index: integer]: TBoldUndoBlock read GetAssertedBlockByIndex;
    function AssertedIndexOf(const BlockName: string): integer;
    procedure InternalRemoveBlock(const BlockName: string); overload;
    procedure InternalRemoveBlock(Block: TBoldUndoBlock); overload;
  public
    procedure ApplytranslationList(IdTranslationList: TBoldIdTranslationList);
    constructor Create;
    destructor Destroy; override;
    procedure MoveBlock(CurIndex, NewIndex: integer);
    function IndexOf(const BlockName: string): integer; overload; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    function IndexOf(Block: TBoldUndoBlock): integer; overload; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    function RemoveBlock(const BlockName: string): Boolean;
    procedure RenameBlock(const OldName, NewName: string);
    procedure MoveToTop(const BlockName: string);
    procedure MergeBlocks(const DestinationBlockName, SourceBlockName: string);
    property BlockByIndex[Index: integer]: TBoldUndoBlock read GetBlockByIndex; default;
    property BlockByName[const BlockName: string]: TBoldUndoBlock read GetBlockByName;
    function CanMoveBlock(CurIndex, NewIndex: integer): Boolean;
    function CanMergeBlock(CurIndex, NewIndex: integer): Boolean;
    function CanMoveToTop(CurIndex: integer): Boolean; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    procedure GetDependantBlocks(const BlockName: string; DependantBlocks: TList);
    procedure MergeAll;
    property Count: integer read GetCount;
    property CurrentBlock: TBoldUndoBlock read GetCurrentBlock;
    property ContainsChanges: Boolean read GetContainsChanges;
  end;

  TBoldUndoHandler = class(TBoldAbstractUndoHandler, IBoldUndoHandler)
  private
    FUndoBlocks: TBoldUndoBlockList;
    FRedoBlocks: TBoldUndoBlockList;
    fUndoState: TBoldUndoState;
    fEnabled: Boolean;
{    function GetFetchedValueOfIndirectMultiLink(const Member: TBoldMember; const OwningObjectId: TBoldObjectId;
       const RoleRTInfo: TBoldRoleRTInfo): TBoldFreeStandingValue;
    function GetFetchedValueOfDirectMultiLink (const Member: TBoldMember; const OwningObjectId: TBoldObjectId;
       const RoleRTInfo: TBoldRoleRTInfo): TBoldFreeStandingValue;
    }
    procedure DoUndo(UnDoValueSpace: TBoldFreeStandingValueSpace;
                      RedoValueSpace: TBoldFreeStandingValueSpace);
    procedure DoUndoInTransaction(BlockName: string; FromList, ToList: TBoldUndoBlockList);  // Don't make blockname const!
    function GetUndoList: IBoldUndoList; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    function GetRedoList: IBoldUndoList; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    function CanUndoBlock(const BlockName: string): Boolean;
    function CanRedoBlock(const BlockName: string):Boolean;
    function GetEnabled: Boolean; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    procedure SetEnabled(value: Boolean);    
  public
    constructor Create(System: TBoldSystem); override;
    destructor Destroy; override;
    function GetUniqueBlockName(const SuggestedName: string): string;
    procedure SetNamedCheckPoint(const CheckPointName: string);
    procedure SetCheckPoint;
    procedure HandleMember(const ObjectContents: IBoldObjectContents; MemberIndex: integer; const MemberValue: IBoldValue); overload; override;
    procedure HandleObject(const Obj: IBoldObjectContents; RegardAsExisting: Boolean); override;
    procedure UndoBlock(const BlockName: string);
    procedure RedoBlock(const BlockName: string);
    procedure UnDoLatest;
    procedure RedoLatest;
    procedure ApplytranslationList(IdTranslationList: TBoldIdTranslationList); override;
    procedure PrepareUpdate(const ObjectList: TBoldObjectList); override;
    procedure ClearAllUndoBlocks; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    property UndoBlocks: TBoldUndoBlockList read fUndoBlocks;
    property RedoBlocks: TBoldUndoBlockList read fRedoBlocks;
    property UndoState: TBoldUndoState read fUndoState write fUndoState;
    property Enabled: Boolean read GetEnabled write SetEnabled;
  end;

implementation

uses
  SysUtils,
  BoldDefs,
  BoldGuard,
  BoldDomainElement;

const
  cUnNamedBlockName = 'UnNamed';

{ TBoldUndoBlock }

constructor TBoldUndoBlock.CreateNamedBlock(const BlockName: string; const FSVAlueSpace: TBoldFreeStandingValueSpace);
begin
  inherited Create;
  FContainsChanges := false;
  FName := BlockName;
  FValueSpace := FSVAlueSpace;
end;

function TBoldUndoBlock.GetFSValueSpace: TBoldFreeStandingValueSpace;
begin
  if not Assigned(FValueSpace) then
    FValueSpace := TBoldFreeStandingValueSpace.Create;
  Result := FValueSpace;
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

procedure TBoldUndoBlock.GetLinksToObject(const System: TBoldSystem; const ObjectId: TBoldObjectId; const OwnIndexInLinkClass: integer;
  const SingleLinkClassTypeInfo: TBoldClassTypeInfo; SingleLinkIds: TBoldObjectIdList);
var
  ObjectIds: TBoldObjectIdList;
  i: integer;
  LinkValue: IBoldValue;
begin
  ObjectIds := TBoldObjectIdList.Create;
  try
    AllIdsInClass(System, SingleLinkClassTypeInfo, ObjectIds);
    for i:= 0 to ObjectIds.Count - 1 do
      if ValueExists(ObjectIds[i], OwnIndexInLinkClass, LinkValue) and
        Assigned((LinkValue as IBoldObjectIdRef).Id) and
        ((LinkValue as IBoldObjectIdRef).Id.IsEqual[ObjectId]) then
           SingleLinkIds.Add(ObjectIds[i].Clone);
  except
    FreeAndNil(ObjectIds);
  end;
end;

procedure TBoldUndoBlock.AddObjectsToList(const System: TBoldSystem;
  const AList: TBoldList);
var
  i: integer;
  ObjectIds: TBoldObjectIdList;
  G: IBoldGuard;
begin
  G := TBoldGuard.Create(ObjectIds);
  ObjectIds := TBoldObjectIdList.Create;
  FSValueSpace.AllObjectIds(ObjectIds, false);
  for i:= 0 to ObjectIds.Count - 1 do
    AList.Add(System.Locators.ObjectByID[ObjectIds[i]]);
end;

procedure TBoldUndoBlock.AllIdsInClass( const System: TBoldSystem;
  const ClassTypeInfo: TBoldClassTypeInfo; IdList: TBoldObjectIdList);
var
  i: integer;
  ObjectIds: TBoldObjectIdList;
  G: IBoldGuard;
begin
  G := TBoldGuard.Create(ObjectIds);
  ObjectIds := TBoldObjectIdList.Create;
  FSValueSpace.AllObjectIds(ObjectIds, True);
  for i:= 0 to ObjectIds.Count - 1 do
  begin
    if (System.Locators.ObjectByID[ObjectIds[i]].BoldClassTypeInfo = ClassTypeInfo) then
      IdList.Add(ObjectIds[i]);
  end;
end;

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

function TBoldUndoBlock.GetValueSpace: IBoldValueSpace;
begin
  result := FSValueSpace as IBoldValueSpace;
end;

function TBoldUndoBlock.GetContainsChanges: Boolean;
begin
  result := fContainsChanges;
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
  FUndoBlocks := TBoldUndoBlockList.Create;
  FRedoBlocks := TBoldUndoBlockList.Create;
  fUndoState := busNormal;
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
    UndoBlocks.CurrentBlock.HandleMember(ObjectContents, MemberIndex, MemberValue);
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
{$IFDEF MergeEmptyBlocks}
  UndoBlocks.MergeAll;
{$ENDIF}
end;

procedure TBoldUndoHandler.SetNamedCheckPoint(const CheckPointName: string);
begin
  if Assigned(UndoBlocks.GetBlockByName(CheckPointName)) or Assigned(RedoBlocks.GetBlockByName(CheckPointName)) then
    raise EBold.CreateFmt('%s.SetCheckPoint: An Undo/Redo block named %s is already defined', [Classname, CheckPointName])
{$IFDEF MergeEmptyBlocks}
  else if not (UndoBlocks.CurrentBlock.ContainsChanges) then
    UndoBlocks.RenameBlock(UndoBlocks.CurrentBlock.BlockName, CheckPointName)
{$ENDIF}
  else
    UndoBlocks.AddBlock(CheckPointName);
end;

procedure TBoldUndoHandler.UndoBlock(const BlockName: string);
begin
  if System.InTransaction then
    raise EBold.CreateFmt('%s.UndoBlock: the Undo-mechanism can only be invoked outside a transaction', [ClassName]);
  UndoState := busUndoing;
  try
    DoUndoInTransaction(BlockName, UndoBlocks, RedoBlocks);
  finally
    UndoState := busNormal;
  end;
end;


procedure TBoldUndoHandler.RedoBlock(const BlockName: string);
begin
  if System.InTransaction then
    raise EBold.CreateFmt('%s.RedoBlock: the Undo-mechanism can only be invoked outside a transaction', [ClassName]);
  UndoState := busRedoing;
  try
    DoUndoInTransaction(BlockName, RedoBlocks, UndoBlocks);
  finally
    UndoState := busNormal;
  end;
end;

procedure TBoldUndoHandler.RedoLatest;
begin
  if (RedoBlocks.Count > 0) then
    RedoBlock(RedoBlocks.CurrentBlock.BlockName);
end;

procedure TBoldUndoHandler.UndoLatest;
begin
  if (UndoBlocks.Count > 0) then
    UndoBlock(UndoBlocks.CurrentBlock.BlockName);
end;

procedure TBoldUndoHandler.HandleObject(const Obj: IBoldObjectContents; RegardAsExisting: Boolean);
begin
  if Enabled and (UndoState = busNormal) then
  begin
    RedoBlocks.Clear;
    UndoBlocks.CurrentBlock.HandleObject(Obj, RegardAsExisting);
  end;
end;
procedure TBoldUndoHandler.DoUndo(UnDoValueSpace: TBoldFreeStandingValueSpace;
                                   RedoValueSpace: TBoldFreeStandingValueSpace);
type
  TObjectAction=(oaDelete, oaExisingPersistent, oaNewPersistent, oaTransient, oaUse);
var
  ObjectIds: TBoldObjectIdList;

    procedure GetInnerLinkIndices(BoldObject: TBoldObject; var MemberIndex1, MemberIndex2: Integer);
  var
    i: integer;
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
      end
      else if Assigned(Value0) then
        Member0.AsIBoldValue[bdepUnDO].AssignContent(Value0)
      else if Assigned(Value1) then
        Member1.AsIBoldValue[bdepUnDO].AssignContent(Value1);
    end
    else
    begin
      NilIdValue := TBFSObjectIdRef.Create;
      NilIdValue.BoldPersistenceState := bvpsCurrent;
      Member0.AsIBoldValue[bdepUnDO].AssignContent(NilIdValue);
      Member1.AsIBoldValue[bdepUnDO].AssignContent(NilIdValue);
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
          BoldMember.AsIBoldValue[bdepUndo].AssignContent(aValue);
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
          raise EBold.create('Internal error?');
        end;
      besNotCreated, besDeleted:
        case FSObjectContents.BoldPersistenceState of
        bvpsCurrent, bvpsModified, bvpsTransient:
          Result := oaDelete;
        else
          raise EBold.create('Internal error?');
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
  G := TBoldGuard.Create(ObjectIds);
  ObjectIds := TBoldObjectIdList.Create;
  UndoValueSpace.AllObjectIds(ObjectIds, false);
  SaveOldValues;
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
  G: IBoldGuard;
begin
  G := TBoldGuard.Create(RedoValueSpace);
  aBlock := FromList.BlockByName[BlockName];
  if Not Assigned(aBlock) then
    raise EBold.CreateFmt('%s is not a valid blockname for this operation', [BlockName]);
  if not FromList.CanMoveToTop(FromList.IndexOf(BlockName)) then
    raise EBold.CreateFmt('%s Can''t be moved to top', [BlockName]);
  RedoValueSpace := TBoldFreeStandingValueSpace.Create;
  System.StartTransaction;
  try
    DoUndo(aBlock.FSValueSpace, RedoValueSpace);
    FromList.InternalRemoveBlock(BlockName);
    ToList.AddBlock(BlockName).FSValueSpace := RedoValueSpace;
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

function TBoldUndoHandler.GetEnabled: Boolean;
begin
  result := fEnabled;
end;

function TBoldUndoHandler.GetRedoList: IBoldUndoList;
begin
  Result := FRedoBlocks;
end;

function TBoldUndoHandler.GetUndoList: IBoldUndoList;
begin
  Result := FUndoBlocks;
end;

function TBoldUndoHandler.GetUniqueBlockName(const SuggestedName: string): string;
var
  i: integer;
begin
   i := 0;
   Result := SuggestedName;
   while ((UndoBlocks.IndexOf(Result) <> -1) or (RedoBlocks.IndexOf(Result) <> -1)) do
   begin
     Result := Format('%s %d', [SuggestedName, i]);
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

procedure TBoldUndoHandler.SetCheckPoint;
begin
  SetNamedCheckPoint(GetUniqueBlockName(cUnNamedBlockName));
end;

procedure TBoldUndoHandler.ClearAllUndoBlocks;
begin
  FUndoBlocks.Clear;
  FRedoBlocks.Clear;
end;

procedure TBoldUndoHandler.SetEnabled(value: Boolean);
begin
  if fEnabled <> value then
  begin
    fEnabled := value;
  end;
end;

{ TBoldUndoBlockList }

function TBoldUndoBlockList.AddBlock(const BlockName: string;
  const FSVAlueSpace: TBoldFreeStandingValueSpace): TBoldUndoBlock;
var
  Idx: integer;
begin
  if FList.IndexOf(BlockName) = -1 then
  begin
    Idx := FList.Add(BlockName);
    Result := TBoldUndoBlock.CreateNamedBlock(BlockName, FSVAlueSpace);
    FList.Objects[Idx] := Result;
  end
  else
    raise EBold.CreateFmt('%s.AddBlock: a block named %s already exists', [ClassName, BlockName]);
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

constructor TBoldUndoBlockList.Create;
begin
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
  if not CanMoveBlock(CurIndex, NewIndex) then
    raise EBold.Create('can''t move Block');
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
    raise EBold.Create('Can''t rename block');
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
    raise EBold.Create('Can''t merge blocks');
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
  Result := nil;
  if (Count = 0) then
    Result := AddBlock(cUnNamedBlockName)
  else if Count > 0 then
    Result := FList.Objects[Count - 1] as TBoldUndoBlock;
end;

function TBoldUndoBlockList.GetAssertedBlockByName(
  const BlockName: string): TBoldUndoBlock;
begin
  Result := BlockByName[BlockName];
  if not Assigned(Result) then
    raise EBold.CreateFmt('There is no block named %s', [BlockName]);
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
    raise EBold.CreateFmt('There is no block named %s', [BlockName]);
end;

function TBoldUndoBlockList.GetItemByName(const Name: string): IBoldUndoBlock;
begin
  result := BlockByName[Name] as IBoldUndoBlock;
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
      raise EBold.CreateFmt('%s.GetDependantBlocks: parameter DependantBlocks not assigned', [ClassName]);
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

function TBoldUndoBlockList.GetAssertedBlockByIndex(
  Index: integer): TBoldUndoBlock;
begin
  Result := BlockByIndex[Index];
  if not Assigned(Result) then
    raise EBold.CreateFmt('There is no block with index %d', [Index]);
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

initialization

end.

