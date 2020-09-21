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
//    procedure GetLinksToObject(const System: TBoldSystem; const ObjectId: TBoldObjectId; const OwnIndexInLinkClass: integer;
//      const SingleLinkClassTypeInfo: TBoldClassTypeInfo; SingleLinkIds: TBoldObjectIdList);
//    procedure AllIdsInClass(const System: TBoldSystem; const ClassTypeInfo: TBoldClassTypeInfo; IdList: TBoldObjectIdList);
    function GetFSValueSpace: TBoldFreeStandingValueSpace;
    procedure SetFSValueSpace(const Value: TBoldFreeStandingValueSpace);
    function GetName: string;
    function GetValueSpace: IBoldValueSpace;
    function GetContainsChanges: Boolean;
  protected
    function HasObjectContentsForAnyObjectInList(const ObjectList: TBoldObjectList): Boolean;
    function HandleMember(ObjectContents: IBoldObjectContents; MemberIndex: integer; MemberValue: IBoldValue): Boolean;
    procedure HandleObject(Obj: IBoldObjectContents; RegardAsExisting: Boolean);
    function IsDependantOn(Block: TBoldUndoBlock): Boolean;
  public
    constructor CreateNamedBlock(const BlockName: string; const FSVAlueSpace: TBoldFreeStandingValueSpace = nil);
    destructor Destroy; override;
    procedure ApplytranslationList(IdTranslationList: TBoldIdTranslationList);
    procedure Merge(Block: TBoldUndoBlock; const Overwrite: Boolean);
    function ValueExists(const ObjectID: TBoldObjectId; const MemberIndex: integer): Boolean; overload;
    function ValueExists(const ObjectID: TBoldObjectID; const MemberIndex: integer; out Value: IBoldValue): Boolean; overload;
    property BlockName: string read FName;
    property FSValueSpace: TBoldFreeStandingValueSpace read GetFSValueSpace write setFSVAlueSpace;
    property ValueSpace: IBoldValueSpace read GetValueSpace;
    property ContainsChanges: Boolean read GetContainsChanges;
  end;

  TBoldUndoBlockList = class(TBoldNonRefCountedObject, IBoldUndoList)
  private
    FList: TStringList;
    function GetBlocksByIndex(Index: integer): TBoldUndoBlock;
    function GetBlocksByName(BlockName: string): TBoldUndoBlock;
    function GetCount: integer;
    function GetCurrentBlock: TBoldUndoBlock;
    function GetAssertedBlocksByName(BlockName: string): TBoldUndoBlock;
    function GetAssertedBlocksByIndex(Index: integer): TBoldUndoBlock;
    function GetItems(Index: integer): IBoldUndoBlock;
    function GetItemsByName(Name: string): IBoldUndoBlock;
    function GetTopBlock: IBoldUndoBlock;
  protected
    procedure Clear;
    function AddBlock(BlockName: string; const FSVAlueSpace: TBoldFreeStandingValueSpace = nil): TBoldUndoBlock;
    property AssertedBlocksByName[BlockName: string]: TBoldUndoBlock read GetAssertedBlocksByName;
    property AssertedBlocksByIndex[Index: integer]: TBoldUndoBlock read GetAssertedBlocksByIndex;
    function AssertedIndexOf(const BlockName: string): integer;
    procedure InternalRemoveBlock(BlockName: string);  // do not make const
  public
    procedure ApplytranslationList(IdTranslationList: TBoldIdTranslationList);
    constructor Create;
    destructor Destroy; override;
    procedure MoveBlock(CurIndex, NewIndex: integer);
    function IndexOf(BlockName: string): integer;
    function RemoveBlock(BlockName: string): Boolean;  // do not make const
    procedure RenameBlock(OldName, NewName: string);  // do not make const
    procedure MoveToTop(BlockName: string);
    procedure MergeBlocks(DestinationBlockName, SourceBlockName: string);
    property BlocksbyIndex[Index: integer]: TBoldUndoBlock read GetBlocksByIndex;
    property BlocksByName[BlockName: string]: TBoldUndoBlock read GetBlocksByName;
    function CanMoveBlock(CurIndex, NewIndex: integer): Boolean;
    function CanMergeBlock(CurIndex, NewIndex: integer): Boolean;
    function CanMoveToTop(CurIndex: integer): Boolean;
    procedure GetDependantBlocks(const BlockName: string; DependantBlocks: TList);
    procedure MergeAll;
    property Count: integer read GetCount;
    property CurrentBlock: TBoldUndoBlock read GetCurrentBlock;
  end;

  TBoldUndoHandler = class(TBoldAbstractUndoHandler, IBoldUndoHandler)
  private
    FUndoBlocks: TBoldUndoBlockList;
    FRedoBlocks: TBoldUndoBlockList;
    fUndoState: TBoldUndoState;
    fEnabled: Boolean;
    procedure DoUndo(UnDoValueSpace: TBoldFreeStandingValueSpace;
                      RedoValueSpace: TBoldFreeStandingValueSpace);
    procedure DoUndoInTransaction(BlockName: string; FromList, ToList: TBoldUndoBlockList);  // Don't make blockname const!
    function GetUndoList: IBoldUndoList;
    function GetRedoList: IBoldUndoList;
    function CanUndoBlock(BlockName: string): Boolean;
    function CanRedoBlock(BlockName: string):Boolean;
    function GetEnabled: Boolean;
    procedure SetEnabled(value: Boolean);
  public
    constructor Create(System: TBoldSystem); override;
    destructor Destroy; override;
    function GetUniqueBlockName(SuggestedName: string): string;
    procedure SetNamedCheckPoint(CheckPointName: string);
    procedure SetCheckPoint;
    procedure HandleMember(ObjectContents: IBoldObjectContents; MemberIndex: integer; MemberValue: IBoldValue); override;
    procedure HandleObject(Obj: IBoldObjectContents; RegardAsExisting: Boolean); override;
    procedure UndoBlock(BlockName: string);
    procedure RedoBlock(BlockName: string);
    procedure UnDoLatest;
    procedure Redolatest;
    procedure ApplytranslationList(IdTranslationList: TBoldIdTranslationList); override;
    procedure PrepareUpdate(const ObjectList: TBoldObjectList); override;
    procedure ClearAllUndoBlocks;
    property UndoBlocks: TBoldUndoBlockList read fUndoBlocks;
    property RedoBlocks: TBoldUndoBlockList read fRedoBlocks;
    property UndoState: TBoldUndoState read fUndoState write fUndoState;
  end;

implementation

uses
  SysUtils,
  BoldUtils,
  BoldDefs,
  BoldGuard,
  BoldDomainElement,
  BoldCoreConsts,
  BoldSubscription;


{ TBoldUndoHandler }

constructor TBoldUndoHandler.Create(System: TBoldSystem);
begin
  inherited;
  FUndoBlocks := TBoldUndoBlockList.Create;
  FRedoBlocks := TBoldUndoBlockList.Create;
  fUndoState := busNormal;
end;

destructor TBoldUndoHandler.Destroy;
begin
  FreeAndNil(FUndoBlocks);
  FreeAndNil(FRedoBlocks);
  inherited;
end;

(*
function TBoldUndoHandler.GetFetchedValue(Member: TBoldMember): IBoldValue;
var
  i: integer;
  RoleRTInfo: TBoldRoleRTInfo;
  HasLinkObject: Boolean;
  ObjectID: TBoldObjectID;
begin
  //TODO: finish implementation
  Result := nil;
  if Member.BoldMemberRTInfo.IsDerived then
    Exit;
  if Member.BoldMemberRTInfo.IsMultiRole then //if multilink
  begin
    ObjectID := Member.OwningObject.BoldObjectLocator.BoldObjectID;
    RoleRTInfo := TBoldRoleRTInfo(Member.BoldMemberRTInfo);
    HasLinkObject := Assigned(RoleRTInfo.LinkClassTypeInfo);
    if HasLinkObject then
      Result := (GetFetchedValueOfIndirectMultiLink(Member, ObjectId, RoleRTInfo) as IBoldValue)
    else
      // no links
      Result := GetFetchedValueOfDirectMultiLink(Member, ObjectId, RoleRtInfo);
  end
  else
    for i:= 0 to UndoBlocks.Count - 1 do
      if UndoBlocks.BlocksbyIndex[i].ValueExists(Member.OwningObject.BoldObjectLocator.BoldObjectID,
                Member.BoldMemberRTInfo.index, Result) then
        Break;
end;
*)

procedure TBoldUndoHandler.HandleMember(ObjectContents: IBoldObjectContents; MemberIndex: integer; MemberValue: IBoldValue);
begin
  if fEnabled and (UndoState = busNormal) then
  begin
    RedoBlocks.Clear;
    UndoBlocks.CurrentBlock.HandleMember(ObjectContents, MemberIndex, MemberValue);
  end;
end;

procedure TBoldUndoHandler.PrepareUpdate(const ObjectList: TBoldObjectList);
var
  aBlock: TBoldUndoBlock;
  i: integer;
begin
  RedoBlocks.Clear;
  i := 0;
  while i < UnDoBlocks.Count do
  begin
    aBlock := UndoBlocks.BlocksbyIndex[i]; //UndoBlocks.CurrentBlock;
    if aBlock.HasObjectContentsForAnyObjectInList(ObjectList) then
    begin
      UnDoBlocks.InternalRemoveBlock(aBlock.BlockName)
    end
    else
      inc(i);
  end;
  UndoBlocks.MergeAll;
end;

procedure TBoldUndoHandler.SetNamedCheckPoint(CheckPointName: string);
begin
  if Assigned(UndoBlocks.GetBlocksByName(CheckPointName)) or Assigned(RedoBlocks.GetBlocksByName(CheckPointName)) then
    raise EBold.CreateFmt(sBlockNameInUse, [Classname, CheckPointName])
  else if not (UndoBlocks.CurrentBlock.ContainsChanges) then
    UndoBlocks.RenameBlock(UndoBlocks.CurrentBlock.BlockName, CheckPointName)
  else
    UndoBlocks.AddBlock(CheckPointName);
end;

procedure TBoldUndoHandler.UndoBlock(BlockName: string);
begin
  if System.InTransaction then
    raise EBold.CreateFmt(sCannotUndoInTransaction, [ClassName]);
  UndoState := busUndoing;
  try
    DoUndoInTransaction(BlockName, UndoBlocks, RedoBlocks);
  finally
    UndoState := busNormal;
  end;
end;

(*
function TBoldUndoHandler.GetFetchedValueOfDirectMultiLink(
  const Member: TBoldMember; const OwningObjectId: TBoldObjectId;
  const RoleRTInfo: TBoldRoleRTInfo): TBoldFreeStandingValue;
var
  CurrentValue: IBoldObjectIdListRef;
  FetchedValue: TBFSObjectIdListRef;
  ObjectIds, IdList: TBoldObjectIdList;
  i: integer;
  aValue: IBoldValue;
  G: IBoldGuard;
begin
  G := TBoldGuard.Create(ObjectIds, IdList);
  //TODO: implement FetchedValues for LinkObjects??
  FetchedValue := TBFSObjectIdListRef.Create;
  CurrentValue := Member.AsIBoldValue[bdepContents] as IBoldObjectIdListRef;
  if Assigned(CurrentValue) then
  begin
    FetchedValue.AssignContent(CurrentValue);
    ObjectIds := TBoldObjectIdList.Create;
    IdList := TBoldObjectIdList.Create;
    // non modified links
    for i:= 0 to CurrentValue.Count - 1 do
      if not NonUndoableBlock.ValueExists(CurrentValue.IdList[i], RoleRTInfo.IndexOfOtherEnd, aValue) then
        IdList.Add(CurrentValue.IdList[i].Clone);
    // deleted links
    NonUndoableBlock.AllIdsInClass(System, RoleRTInfo.ClassTypeInfoOfOtherEnd, ObjectIds);
    for i:= 0 to ObjectIds.Count - 1 do
    begin
      if NonUndoableBlock.ValueExists(Objectids[i], RoleRTInfo.IndexOfOtherEnd, aValue) and
        Assigned((aValue as IBoldObjectIdRef).Id) and (aValue as IBoldObjectIdRef).Id.IsEqual[OwningObjectId] then
          IdList.Add(ObjectIds[i]);
    end;
    FetchedValue.SetFromIdList(IdList);
  end;
  Result := FetchedValue;
end;

function TBoldUndoHandler.GetFetchedValueOfIndirectMultiLink(
  const Member: TBoldMember; const OwningObjectId: TBoldObjectId;
  const RoleRTInfo: TBoldRoleRTInfo): TBoldFreeStandingValue;
var
  CurrentValue: IBoldObjectIdListRefPair;
  FetchedValue: TBFSObjectIdListrefPair;
  i: integer;
  ObjectIds, IdList1, IdList2: TBoldObjectIdList; // Idlist1 is the list of ids of the linkobjects
  LinkObjectId : TBoldObjectId;
  LinkValue: IBoldValue;
begin
  //TODO: implement
  CurrentValue := Member.AsIBoldValue[bdepContents] as IBoldObjectIdListRefPair;
  FetchedValue := TBFSObjectIdListRefPair.Create;
  FetchedValue.AssignContent(CurrentValue);
  ObjectIds := TBoldObjectIdList.Create;
  IdList1 := TBoldObjectIdList.Create;
  IdList2 := TBoldObjectIdList.Create;
  try
    //non modified links
    for i:= 0 to CurrentValue.Count - 1 do
    begin
      LinkObjectId := CurrentValue.IdList1[i]; //id1 is link object
      if not Assigned(NonUndoableBlock.FSValueSpace.GetFSObjectContentsByObjectId(LinkObjectId)) then  // link not modified
      begin
        IdList1.Add(CurrentValue.IdList1[i].Clone);
        IdList2.Add(CurrentValue.IdList2[i].Clone);
      end;
    end;
    //deleted links
    NonUndoableBlock.GetLinksToObject(System, OwningObjectId, RoleRTInfo.OwnIndexInLinkClass, RoleRtInfo.LinkClassTypeInfo, ObjectIds);
    for i:= 0 to ObjectIds.Count - 1 do
    begin
      IdList1.Add(ObjectIds[i].Clone);
      NonUndoableBlock.ValueExists(ObjectIds[i],RoleRtInfo.OtherIndexInLinkClass, LinkValue);
      IdList2.Add((LinkValue as IBoldObjectIdRef).Id.Clone);
    end;
  finally
    FetchedValue.SetFromIdLists(IdList1, IdList2);
    Result := FetchedValue;
    FreeAndNil(ObjectIds);
    FreeAndNil(IdList1);
    FreeAndNil(IdList2);
  end;
end;
*)

procedure TBoldUndoHandler.RedoBlock(BlockName: string);
begin
  if System.InTransaction then
    raise EBold.CreateFmt(sCannotUndoInTransaction, [ClassName]);
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

procedure TBoldUndoHandler.HandleObject(Obj: IBoldObjectContents; RegardAsExisting: Boolean);
begin
  if fEnabled and (UndoState = busNormal) then
  begin
    RedoBlocks.Clear;
    UndoBlocks.CurrentBlock.HandleObject(Obj, RegardAsExisting);
  end;
end;

// Perform Undo, Fill in valuespace for Redoing
procedure TBoldUndoHandler.DoUndo(UnDoValueSpace: TBoldFreeStandingValueSpace;
                                   RedoValueSpace: TBoldFreeStandingValueSpace);
type
  TObjectAction=(oaDelete, oaExisingPersistent, oaNewPersistent, oaTransient, oaUse);
var
  ObjectIds: TBoldObjectIdList;

    procedure GetInnerLinkIndices(BoldObject: TBoldObject; var MemberIndex1, MemberIndex2: Integer);
  var
    i: integer;
    MemberRTInfo: TBoldMemberRTInfo;
  begin
    MemberIndex1 := -1;
    MemberIndex2 := -1;
    for i:= 0 to BoldObject.BoldMemberCount - 1 do
    begin
      MemberRTInfo := BoldObject.BoldClassTypeInfo.AllMembers[i];
      if MemberRTInfo.IsRole and ((MemberRTInfo as TBoldRoleRTInfo).RoleType = rtInnerLinkRole) then
      begin
        if (MemberIndex1 = -1) then
          MemberIndex1 := i
        else if (MemberIndex2 = -1) then
          MemberIndex2 := i
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
       // Very clumsy, but seems to work
       // Problem is that when first assigment is done, other linkobject-link is not set yet, so
       // otherend will be nil. This means that things will not be set upp properly.
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
    i: Integer;
    aValue: IBoldValue;
    BoldMember: TBoldMember;
  begin
    for i := 0 to BoldObject.BoldMemberCount - 1 do
    begin
      aValue := ObjectContents.valueByIndex[i];
      if assigned(aValue) then
      begin
        BoldMember := BoldObject.BoldMembers[i];
        if (BoldMember is TBoldObjectReference) and (TBoldObjectReference(BoldMember).BoldRoleRTInfo.RoleType = rtRole) then
          BoldMember.AsIBoldValue[bDepUndo].AssignContent(aValue);
      end;
    end;
    if BoldObject.BoldClassTypeInfo.IsLinkClass then
      UnDoInnerLinks(BoldObject, ObjectContents);
  end;

  procedure SaveOldValues;
  var
    oid: TBoldObjectId;
    FSObjectContents: TBoldFreeStandingObjectContents;
    i, M: integer;
    BoldObject: TBoldObject;
    MemberId: TBoldMemberId;
    G: IBoldGuard;
  begin
    G := TBoldGuard.Create(MemberId);
    for i:= 0 to ObjectIds.Count - 1 do
    begin
      oid := ObjectIds[i];
      RedoValueSpace.EnsureObjectContents(oid);
      BoldObject := System.Locators.ObjectByID[oid];
      FSObjectContents := UndoValueSpace.GetFSObjectContentsByObjectId(oid);
      if Assigned(BoldObject) then
      begin
        if FSObjectContents.BoldExistenceState = besExisting then  // just keep changed values
        begin
          RedoValueSpace.GetFSObjectContentsByObjectId(oid).ApplyObjectContents(
                FSObjectContents, true, false);
          RedoValueSpace.GetFSObjectContentsByObjectId(oid).UpdateObjectContentsFrom(BoldObject.AsIBoldObjectContents[bdepContents])
        end
        else  // Object going away, keep all
        begin
          RedoValueSpace.GetFSObjectContentsByObjectId(oid).ApplyObjectContents(BoldObject.AsIBoldObjectContents[bdepContents], False, false);
          for M := 0 to BoldObject.BoldMemberCount -1 do
            if BoldObject.BoldMemberAssigned[M] and BoldObject.BoldMembers[M].StoreInUndo then
            begin
              MemberId := TBoldMemberId.Create(M);
              RedoValueSpace.GetFSObjectContentsByObjectId(oid).EnsureMember(MemberId, BoldObject.BoldMembers[M].AsIBoldValue[bdepContents].ContentName);
              RedoValueSpace.GetFSObjectContentsByObjectId(oid).ValueByIndex[M].AssignContent(BoldObject.BoldMembers[M].AsIBoldValue[bdepContents]);
              FreeAndNil(MemberId);
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
    result := oaUse; // stupid compiler
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
  G := TBoldGuard.Create(ObjectIds);
  ObjectIds := TBoldObjectIdList.Create;
  UndoValueSpace.AllObjectIds(ObjectIds, false);
  SaveOldValues;
  // pass one, create all Objects, and set attributes
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
  // Pass 2, update links once all objects are in place
  for i:= 0 to ObjectIds.Count - 1 do
  begin
    oid := ObjectIds[i];
    RedoValueSpace.EnsureObjectContents(oid);
    BoldObject := System.Locators.ObjectByID[oid];
    if Assigned(BoldObject) then
      UnDoLinks(BoldObject, UndoValueSpace.GetFSObjectContentsByObjectId(oid));
  end;
  // Pass 3, delete objects

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

procedure TBoldUndoHandler.DoUndoInTransaction(BlockName: string;
  FromList, ToList: TBoldUndoBlockList);
var
  aBlock: TBoldUndoBlock;
  RedoValueSpace: TBoldFreeStandingValueSpace;
  G: IBoldGuard;
begin
  G := TBoldGuard.Create(RedoValueSpace);
  aBlock := FromList.BlocksByName[BlockName];
  if Not Assigned(aBlock) then
    raise EBold.CreateFmt(sInvalidBlockName, [BlockName]);
  if not FromList.CanMoveToTop(FromList.IndexOf(BlockName)) then
    raise EBold.CreateFmt(sCannotMoveToTop, [BlockName]);
  RedoValueSpace := TBoldFreeStandingValueSpace.Create;
  System.StartTransaction;
  try
    DoUndo(aBlock.FSValueSpace, RedoValueSpace);
    FromList.InternalRemoveBlock(BlockName);
    ToList.AddBlock(BlockName).FSValueSpace := RedoValueSpace;
    RedoValueSpace := nil; // now owned by Redo-block
    System.CommitTransaction;
  except
    System.RollbackTransaction;
  end;
end;

function TBoldUndoHandler.CanRedoBlock(BlockName: string): Boolean;
begin
  Result := RedoBlocks.CanMoveToTop(RedoBlocks.AssertedIndexOf(BlockName));
end;

function TBoldUndoHandler.CanUndoBlock(BlockName: string): Boolean;
begin
  Result := UnDoBlocks.CanMoveToTop(UndoBlocks.AssertedIndexOf(BlockName));
end;

function TBoldUndoHandler.GetRedoList: IBoldUndoList;
begin
  Result := FRedoBlocks;
end;

function TBoldUndoHandler.GetUndoList: IBoldUndoList;
begin
   Result := FUndoBlocks;
end;

function TBoldUndoHandler.GetUniqueBlockName(
  SuggestedName: string): string;
var
  i: integer;
begin
   i := 0;
   Result := SuggestedName;
   while ((UndoBlocks.IndexOf(Result) <> -1) or (RedoBlocks.IndexOf(Result) <> -1)) do
   begin
     Result := Format('%s %d', [SuggestedName, i]); // do not localize
     inc(i);
   end;
end;

procedure TBoldUndoHandler.ApplytranslationList(
  IdTranslationList: TBoldIdTranslationList);
begin
  UndoBlocks.ApplytranslationList(IdTranslationList);
  RedoBlocks.ApplytranslationList(IdTranslationList);
end;

procedure TBoldUndoHandler.SetCheckPoint;
begin
  SetNamedCheckPoint(GetUniqueBlockName(''));
end;

procedure TBoldUndoHandler.ClearAllUndoBlocks;
begin
  FUndoBlocks.Clear;
  FRedoBlocks.Clear;
end;

function TBoldUndoHandler.GetEnabled: Boolean;
begin
  result := fEnabled;
end;

procedure TBoldUndoHandler.SetEnabled(value: Boolean);
begin
  fEnabled := value;
end;

{ TBoldUndoBlockList }

function TBoldUndoBlockList.AddBlock(
  BlockName: string; const FSVAlueSpace: TBoldFreeStandingValueSpace): TBoldUndoBlock;
var
  Idx: integer;
begin
  if FList.IndexOfName(BlockName) = -1 then
  begin
    Idx := FList.Add(BlockName);
    Result := TBoldUndoBlock.CreateNamedBlock(BlockName, FSVAlueSpace);
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
  CurBlock := AssertedBlocksByIndex[CurIndex];
  NewBlock := AssertedBlocksByIndex[NewIndex];
  Result := (CurIndex = NewIndex) or not CurBlock.IsDependantOn(NewBlock);
  if Result then
  begin
    if (CurIndex < NewIndex) then //moving up
      for i:= CurIndex + 1 to NewIndex do
      begin
        Result := not CurBlock.IsDependantOn(BlocksByIndex[i]);
        if not Result then
          Break;
      end
    else //moving down
      for i:= CurIndex - 1 downto NewIndex do
      begin
        Result := not CurBlock.IsDependantOn(BlocksByIndex[i]);
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
    raise EBold.Create(sCannotMoveBlock);
   FList.Move(CurIndex, NewIndex);
end;

function TBoldUndoBlockList.GetBlocksByIndex(
  Index: integer): TBoldUndoBlock;
begin
  if (Index >= 0) and (Index < FList.Count) then
    Result := FList.Objects[Index] as TBoldUndoBlock
  else
    Result := nil;
end;

function TBoldUndoBlockList.GetBlocksByName(
  BlockName: string): TBoldUndoBlock;
var
  Idx: integer;
begin
  Idx := FList.IndexOf(blockName);
  if Idx <> - 1 then
    Result := FList.Objects[Idx] as TBoldUndoBlock
  else
    Result := nil;
end;

function TBoldUndoBlockList.GetCount: integer;
begin
  Result := fList.Count;
end;

function TBoldUndoBlockList.IndexOf(BlockName: string): integer;
begin
  Result := fList.IndexOf(BlockName);
end;

procedure TBoldUndoBlockList.MoveToTop(
  BlockName: string);
begin
  MoveBlock(IndexOf(BlockName), FList.Count - 1);
end;

procedure TBoldUndoBlockList.InternalRemoveBlock(BlockName: string);
var
  idx: integer;
  obj: TObject;
begin
  idx := AssertedIndexOf(BlockName);
  obj := FList.Objects[idx];
  FreeAndNil(obj);
  FList.Delete(idx);
end;

procedure TBoldUndoBlockList.RenameBlock(OldName,
  NewName: string);
var
  aBlock: TBoldUndoBlock;
begin
  if Assigned(GetBlocksByName(NewName)) then
    raise EBold.Create(sCannotRenameBlock);
  aBlock := AssertedBlocksByName[OldName];
  FList.Strings[IndexOf(OldName)] := NewName;
  aBlock.FName := NewName;
end;

 procedure TBoldUndoBlockList.MergeBlocks(DestinationBlockName,
  SourceBlockName: string);
var
  DestinationBlock, SourceBlock: TBoldUndoBlock;
begin
  DestinationBlock := AssertedBlocksbyName[DestinationBlockName];
  SourceBlock := AssertedBlocksByName[SourceBlockName];
  if not CanMergeBlock(IndexOf(SourceBlockName), IndexOf(DestinationBlockName)) then
    raise EBold.Create(sCannotMergeBlocks);
  DestinationBlock.Merge(SourceBlock, IndexOf(DestinationBlockName) > IndexOf(SourceBlockName));
  InternalRemoveBlock(SourceBlockName);
end;

function TBoldUndoBlockList.CanMergeBlock(CurIndex,
  NewIndex: integer): Boolean;
var
  i: integer;
  CurBlock, NewBlock: TBoldUndoBlock;
begin
  Result := true;
  CurBlock := AssertedBlocksByIndex[CurIndex];
  NewBlock := AssertedBlocksByIndex[CurIndex];
  if CurIndex < NewIndex then //moving up
  begin
    for i:= CurIndex + 1 to NewIndex - 1 do
    begin
      Result := not CurBlock.IsDependantOn(BlocksByIndex[i]);
      if not Result then
        Break;
    end
  end
  else if CurIndex > NewIndex then //moving down
    for i:= CurIndex - 1 downto NewIndex + 1 do
    begin
      Result := not NewBlock.IsDependantOn(BlocksByIndex[i]);
      if not Result then
        Break;
    end;
end;

function TBoldUndoBlockList.GetCurrentBlock: TBoldUndoBlock;
begin
  Result := nil;
  if (Count = 0) then
    Result := AddBlock('UnNamed') // do not localize
  else if Count > 0 then
    Result := FList.Objects[Count - 1] as TBoldUndoBlock;
end;

function TBoldUndoBlockList.GetAssertedBlocksByName(
  BlockName: string): TBoldUndoBlock;
begin
  Result := BlocksByName[BlockName];
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

function TBoldUndoBlockList.GetItemsByName(
   Name: string): IBoldUndoBlock;
begin
  result := BlocksByName[Name] as IBoldUndoBlock;
end;

function TBoldUndoBlockList.GetItems(Index: integer): IBoldUndoBlock;
begin
  Result := BlocksbyIndex[Index] as IBoldUndoBlock ;
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
    BlocksbyIndex[i].ApplytranslationList(IdTranslationList);
end;

procedure TBoldUndoBlockList.MergeAll;
var
  i: integer;
  aBlock: TBoldUndoBlock;
begin
  i:= Count - 1;
  while i > 0 do
  begin
    aBlock := BlocksByIndex[i];
    BlocksByIndex[i - 1].Merge(aBlock, True);
    InternalRemoveBlock(aBlock.BlockName);
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
  aBlock := GetAssertedBlocksByName(BlockName);
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
          CurBlock := BlocksbyIndex[b];
          if Assigned(ObjectContents.ValueByIndex[j]) and CurBlock.ValueExists(ObjectIds.ObjectIds[i], j, aValue)
              and (DependantBlocks.IndexOf(CurBlock) = - 1) then
            DependantBlocks.Add(CurBlock);
        end;//for b
    end;//for i
  end;
end;

function TBoldUndoBlockList.GetAssertedBlocksByIndex(
  Index: integer): TBoldUndoBlock;
begin
  Result := BlocksByIndex[Index];
  if not Assigned(Result) then
    raise EBold.CreateFmt(sNoSuchBlockIndex, [Index]);
end;

function TBoldUndoBlockList.RemoveBlock(BlockName: string): Boolean;
var
  idx: integer;
begin
  idx := AssertedIndexOf(BlockName);
  Result := CanMoveToTop(idx);
  if Result then
    InternalRemoveBlock(BlockName);
end;

{ TBoldUndoBlock }

constructor TBoldUndoBlock.CreateNamedBlock(const BlockName: string; const FSVAlueSpace: TBoldFreeStandingValueSpace);
begin
  inherited Create;
  FContainsChanges := false;
  FName := BlockName;
  FValueSpace := FSVAlueSpace;
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
  MemberId: TBoldMemberId;
  i, j: integer;
  ObjectIds: TBoldObjectIdList;
  G: IBoldGuard;
begin
  G := TBoldGuard.Create(MemberId, ObjectIds);
  // Todo: Same as ApplyValueSpace???
  ObjectIds := TBoldObjectIdList.Create;
  Block.FSValueSpace.AllObjectIds(ObjectIds, True);
  for i:= 0 to ObjectIds.Count - 1 do
  begin
    ObjectId := ObjectIds[i];
    ObjectContents := Block.FSValueSpace.GetFSObjectContentsByObjectId(ObjectId);
    if not FSValueSpace.GetHasContentsForId(ObjectId) then
      FSValueSpace.EnsureObjectContents(ObjectId);
    OwnContents :=FSValueSpace.GetFSObjectContentsByObjectId(ObjectId);
    for j:= 0 to ObjectContents.MemberCount - 1 do
      if Assigned(ObjectContents.valueByIndex[j]) then
      begin
        MemberId := TBoldMemberId.Create(j);
        if (OwnContents.MemberCount > j) and Assigned(OwnContents.ValueByIndex[j]) then // CHECKCME
        begin
          if overwrite then
            OwnContents.ValueByIndex[j].AssignContent(ObjectContents.ValueByIndex[j]);
        end
        else
        begin
          OwnContents.EnsureMember(MemberId, ObjectContents.ValueByIndex[j].ContentName);
          OwnContents.ValueByIndex[j].AssignContent(ObjectContents.ValueByIndex[j]);
        end;
        FreeAndNil(memberId);
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
  OC: TBoldFreeStandingObjectContents;
begin
  OC := FSValueSpace.GetFSobjectContentsByObjectId(ObjectID);
  Result := Assigned(OC) and (MemberIndex < OC.memberCount) and
    Assigned(OC.ValueByIndex[MemberIndex]);
end;

function TBoldUndoBlock.HandleMember(ObjectContents: IBoldObjectContents; MemberIndex: integer; MemberValue: IBoldValue): Boolean;
var
  MemberId: TBoldMemberId;
  ObjectId: TBoldObjectId;
  FSObjectContents: TBoldFreeStandingObjectContents;
  G: IBoldGuard;
begin
  G := TBoldGuard.Create(MemberId);
  ObjectId := ObjectContents.ObjectId;
  Result := not ValueExists(ObjectId, MemberIndex);
  if Result then
  begin
    FContainsChanges := true;
    MemberId := TBoldMemberId.Create(MemberIndex);
    FSObjectContents := FSValueSpace.GetFSObjectContentsByObjectId(ObjectId);
    if not Assigned(FSObjectContents) then
    begin
      FSValueSpace.EnsureObjectContents(ObjectId);
      FSObjectContents := FSValueSpace.GetFSObjectContentsByObjectId(ObjectId);
      FSObjectContents.ApplyObjectContents(ObjectContents, False, false);
    end;
    FSObjectContents.EnsureMember(MemberId, MemberValue.ContentName);
    FSObjectContents.ValueByIndex[MemberIndex].AssignContent(MemberValue);
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

(*
procedure TBoldUndoBlock.GetLinksToObject(const System: TBoldSystem; const ObjectId: TBoldObjectId; const OwnIndexInLinkClass: integer;
  const SingleLinkClassTypeInfo: TBoldClassTypeInfo; SingleLinkIds: TBoldObjectIdList);
var
  ObjectIds: TBoldObjectIdList;
  i: integer;
  LinkValue: IBoldValue;
begin
  // TODO: new implementation
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
*)

(*
procedure TBoldUndoBlock.AllIdsInClass( const System: TBoldSystem;
  const ClassTypeInfo: TBoldClassTypeInfo; IdList: TBoldObjectIdList);
var
  i: integer;
  ObjectIds: TBoldObjectIdList;
  G: IBoldGuard;
begin
{ TODO : Won't work with really deleted objects }
  G := TBoldGuard.Create(ObjectIds);
  ObjectIds := TBoldObjectIdList.Create;
  FSValueSpace.AllObjectIds(ObjectIds, True);
  for i:= 0 to ObjectIds.Count - 1 do
  begin
    if (System.Locators.ObjectByID[ObjectIds[i]].BoldClassTypeInfo = ClassTypeInfo) then
      IdList.Add(ObjectIds[i]);
  end;
end;
*)

procedure TBoldUndoBlock.HandleObject(Obj: IBoldObjectContents; RegardAsExisting: Boolean);
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

function TBoldUndoBlock.GetFSValueSpace: TBoldFreeStandingValueSpace;
begin
  if not Assigned(FValueSpace) then
    FValueSpace := TBoldFreeStandingValueSpace.Create;
  Result := FValueSpace;
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

end.

