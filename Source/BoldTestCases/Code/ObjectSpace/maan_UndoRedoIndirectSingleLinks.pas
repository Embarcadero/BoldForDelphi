unit maan_UndoRedoIndirectSingleLinks;

interface

uses
  maan_UndoRedo,
  SysUtils,
  BoldUtils,
  TestSuite,
  TestFrameWork;

type

  Tmaan_IndirectSingleFetchRefetchTestCase = class(Tmaan_UndoRedoAbstractTestCase)
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
  published
    procedure IndirectSingle_FetchSingleRoleInvalid;
    procedure IndirectSingle_FetchSingleRoleInvalidAdjust;
    procedure IndirectSingle_FetchSingleRoleCurrent;
    procedure IndirectSingle_FetchMultiRoleInvalid;
  end;

  Tmaan_IndirectSingleModifyTestCase = class(Tmaan_UndoRedoAbstractTestCase)
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
  published
    procedure IndirectSingle_ModifyMultiRoleInsertCurrent;
    procedure IndirectSingle_ModifyMultiRoleDeleteCurrent;
    procedure IndirectSingle_ModifySingleRoleCurrent;
  end;

  Tmaan_IndirectSingleUndoTestCase = class(Tmaan_UndoRedoAbstractTestCase)
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
  published
    procedure IndirectSingle_UndoSingleRoleModified;
  end;

implementation

uses
  BoldSystem,
  dialogs,
  BoldId,
  maan_UndoRedoTestCaseUtils,
  BoldDefs,
  BoldFreeStandingValues,
  BoldDomainElement,
  BoldSubscription,
  BoldValueInterfaces,
  BoldValueSpaceInterfaces,
  BoldUndoHandler,
  UndoTestModelClasses;

{ Tmaan_IndirectSingleFetchRefetchTestCase }

procedure Tmaan_IndirectSingleFetchRefetchTestCase.IndirectSingle_FetchSingleRoleCurrent;
var
  ObjA, ObjB: TClassWithLink;
  procedure prepare;
  begin
    RefreshSystem;
    ObjA := FClassWithLinkList[0];
    ObjB := FClassWithLinkList[1];
    ObjB.M_one.EnsureContentsCurrent;
    VerifyState(ObjB.M_one, bvpsCurrent);
  end;

  procedure ModifySingleRoleStateInDb;
  begin
    OpenSystem2;
    Assert(FClassWithLinkList2[1].BoldObjectLocator.BoldObjectId.AsString = FClassWithLinkList[1].BoldObjectLocator.BoldObjectId.AsString);
    Assert(FClassWithLinkList2[2].BoldObjectLocator.BoldObjectId.AsString = FClassWithLinkList[2].BoldObjectLocator.BoldObjectId.AsString);
    FClassWithLinkList2[1].one := FClassWithLinkList2[2];
    SaveAndCloseSystem2;
  end;

  procedure UndoModifySingleRoleStateInDb;
  begin
    OpenSystem2;
    FClassWithLinkList2[1].one := FClassWithLinkList2[0];
    SaveAndCloseSystem2;
  end;


begin
  SetConfigurationForIndirectSingle;
  {ObjB Invalid, and Old(ObjB = New(ObjB)}
  Prepare;
  VerifyState(ObjA.M_many, bvpsInvalid);
  ModifySingleRoleStateInDb;
  StoreValue(ObjB.M_one);
  FSubscriber.SubscribeToElement(ObjB.M_one);
  ObjB.M_one.Refetch;
  Assert(ObjB.one = FClassWithLinkList[2], 'Cannot refetch member');
  VerifyState(ObjB.M_one, bvpsCurrent);
  FSubscriber.VerifySendEvent(beValueChanged, ObjB.M_one);
  VerifyState(ObjA.M_many, bvpsInvalid);
  Exit;
end;

procedure Tmaan_IndirectSingleFetchRefetchTestCase.IndirectSingle_FetchSingleRoleInvalid;
var
  ObjA, ObjB, NewObjB: TClassWithLink;
  OneEmbeddedIndex, ManyEmbeddedIndex: integer;
  procedure prepare;
  begin
    RefreshSystem;
    ObjA := FClassWithLinkList[0];
    ObjB := FClassWithLinkList[1];
    NewObjB := FClassWithLinkList[2];
    try
      OneEmbeddedIndex := ObjA.M_one.BoldRoleRTInfo.LinkClassTypeInfo.AllMembers[ObjA.M_one.BoldRoleRTInfo.OwnIndexInLinkClass].EmbeddedLinkIndex;
      ManyEmbeddedIndex := ObjA.M_one.BoldRoleRTInfo.LinkClassTypeInfo.AllMembers[ObjA.M_many.BoldRoleRTInfo.OwnIndexInLinkClass].EmbeddedLinkIndex;
    except
      showmessage('implement SingleRoleSingleLinks for link classes');
    end;
  end;
begin
  {ObjB.one invalid}
  SetConfigurationForIndirectSingle;
  {ObjA.many Invalid}
  Prepare;
  VerifyState(ObjA.M_many, bvpsInvalid);
  ObjB.M_one.EnsureContentsCurrent; //fetch SingleRole role
  VerifyState(ObjB.M_one, bvpsCurrent);
  Assert(ObjB.one = ObjA);

  {ObjA.many Current, and NewObjB not in ObjA.many}
  Prepare;
  ObjA.many.EnsureContentsCurrent;
  VerifyState(ObjA.M_many, bvpsCurrent);
  VerifyState(NewObjB.M_one, bvpsInvalid);
  OpenSystem2;
  FClassWithLinkList2[0].many.Add(FClassWithLinkList2[2]);
  SaveAndCloseSystem2;

  StoreObject(ObjA);
  FSubscriber.SubscribeToElement(ObjA.M_many);
  NewObjB.M_one.EnsureContentsCurrent; //fetch SingleRole role
  VerifyState(NewObjB.M_one, bvpsCurrent); //should the values in EmbeddedSingleLinks be set? What about the state?
  Assert(NewObjB.one = ObjA);
// TODO: should the multi end be adjusted?  VerifyListAdjustedIn(ObjA.many, NewObjB, false);
//  FSubscriber.VerifySendEvent(beItemAdded, ObjA.many);

  {ObjA.many Current, and ObjB in ObjA.many}
  Prepare;
  ObjA.many.EnsureContentsCurrent;
  VerifyState(ObjA.M_many, bvpsCurrent);
  VerifyState(ObjB.M_one, bvpsInvalid);
  Assert(ObjB.M_oneLinkClass.Locator.EmbeddedSingleLinks[OneEmbeddedIndex] = ObjA.BoldObjectLocator);
//TODO: remove  Assert(ObjBLocator.EmbeddedSingleLinks[EmbeddedIndex].BoldObjectID.AsString = ObjA.BoldObjectLocator.BoldObjectID.AsString);

  OpenSystem2;
  FClassWithLinkList2[1].one := nil;  // remove MultiRoleRole from ObjA.many
  SaveAndCloseSystem2;

  StoreObject(ObjA);
  FSubscriber.SubscribeToElement(ObjA.M_many);
  ObjB.M_one.EnsureContentsCurrent; //fetch SingleRole role
  VerifyState(ObjB.M_one, bvpsCurrent);
  Assert(ObjB.one = nil);
  VerifyListAdjustedEx(ObjA.many, ObjB , true);
  FSubscriber.VerifySendEvent(beItemDeleted, ObjA.many);
end;

procedure Tmaan_IndirectSingleFetchRefetchTestCase.IndirectSingle_FetchSingleRoleInvalidAdjust;
var
  ObjA, ObjB: TClassWithLInk;
  NewObjALocator: TBoldObjectLocator;
  procedure prepare;
  begin
    RefreshSystem;
    ObjA := FClassWithLinkList[0];
    ObjB := FClassWithLinkList[1];
    NewObjALocator := FClassWithLinkList.Locators[2];
    ObjB.M_one.Invalidate;
    VerifyState(ObjB.M_one, bvpsInvalid);
    VerifyHasOldValues(ObjB.M_one, true);
  end;

  procedure FetchSingleRoleRole(obj: TClassWithLink);
  begin
    obj.BoldObjectLocator.EnsureBoldObject;
  end;

  procedure ModifySingleRoleStateInDb;
  begin
    OpenSystem2;
    FClassWithLinkList2[1].one := FClassWithLinkList2[2];
    SaveAndCloseSystem2;
  end;

  procedure UndoModifySingleRoleStateInDb;
  begin
    //Undo the changes
    OpenSystem2;
    FClassWithLinkList2[1].one := FClassWithLinkList2[0];
    SaveAndCloseSystem2;
  end;

begin
  SetConfigurationForIndirectSingle;
  {ObjA.many Invalid, and ObjB.one not modified after fetch}
  Prepare;
  VerifyState(ObjA.M_many, bvpsInvalid);
  FetchSingleRoleRole(ObjB);
  Assert(ObjB.one = ObjA);
  VerifyState(ObjB.M_one, bvpsCurrent);
  VerifyState(ObjA.M_many, bvpsInvalid);

  {ObjA.many Invalid, and ObjB.one modified after fetch}
  Prepare;
  VerifyState(ObjB.M_many, bvpsInvalid);
  VerifyState((NewObjALocator.EnsuredBoldObject as TClassWithLink).M_many, bvpsInvalid);

  ModifySingleRoleStateInDb;
  FetchSingleRoleRole(ObjB);
  Assert(ObjB.one = (NewObjALocator.BoldObject as TClassWithLink));
  VerifyState(ObjB.M_one, bvpsCurrent);
  VerifyState(ObjA.M_many, bvpsInvalid);
  VerifyState((NewObjALocator.BoldObject as TClassWithLink).M_many, bvpsInvalid);
  UndoModifySingleRoleStateInDb;

  {ObjA.many Invalid, NewObjA.many current, NewObjB current}
  Prepare;
  (NewObjALocator.EnsuredBoldObject as TClassWithLink).many.EnsureContentsCurrent;
  VerifyState(ObjB.M_many, bvpsInvalid);
  VerifyState((NewObjALocator.EnsuredBoldObject as TClassWithLink).M_many, bvpsCurrent);

  ModifySingleRoleStateInDb;
  FSubscriber.SubscribeToElement((NewObjALocator.BoldObject as TClassWithLink).M_many);
  FetchSingleRoleRole(ObjB);
  Assert(ObjB.one = (NewObjALocator.BoldObject as TClassWithLink));
  VerifyState(ObjB.M_one, bvpsCurrent);
  VerifyState(ObjA.M_many, bvpsInvalid);
  VerifyState((NewObjALocator.BoldObject as TClassWithLink).M_many, bvpsCurrent);
  VerifyListAdjustedIn((NewObjALocator.BoldObject as TClassWithLink).many, ObjB, true);
  FSubscriber.VerifySendEvent(beItemAdded, (NewObjALocator.BoldObject as TClassWithLink).M_many);
  UndoModifySingleRoleStateInDb;

  {ObjA.many Current, Old(ObjA = New(ObjB)}
  Prepare;
  ObjA.many.EnsureContentsCurrent;
  VerifyState(ObjA.M_many, bvpsCurrent);

  StoreValue(ObjB.M_one);
  StoreValue(ObjA.M_many);
  FetchSingleRoleRole(ObjB);
  VerifySetContents(ObjB.M_one.AsIBoldValue[bdepContents], GetStoredValueOfMember(ObjB.M_one));
  VerifyState(ObjB.M_one, bvpsCurrent);

  VerifyState(ObjA.M_many, bvpsCurrent);
  Assert(ObjA.ValuesAreEqual(ObjA.M_many.AsIBoldValue[bdepContents],
    GetStoredValueOfMember(ObjA.M_many), 'many'), 'TestFetchSingleRoleRoleInvalidAdjust failed');

  {ObjA.many Current, NewObjB current, and ObjA <> NewObjA}
  Prepare;
  (NewObjALocator.BoldObject as TClassWithLink).many.EnsureContentsCurrent;
  ObjA.many.EnsureContentsCurrent;
  VerifyState(ObjB.M_many, bvpsCurrent);
  VerifyState((NewObjALocator.BoldObject as TClassWithLink).M_many, bvpsCurrent);

  ModifySingleRoleStateInDb;
  StoreValue(ObjB.M_one);
  StoreValue((NewObjALocator.BoldObject as TClassWithLink).M_many);
  StoreValue(ObjA.M_many);
  FSubscriber.SubscribeToElement((NewObjALocator.BoldObject as TClassWithLink).M_many);
  FSubscriber.SubscribeToElement(ObjA.M_many);
  FetchSingleRoleRole(ObjB);
  VerifySetContents(ObjB.M_one.AsIBoldValue[bdepContents], GetStoredValueOfMember(ObjB.M_one));
  VerifyState(ObjB.M_one, bvpsCurrent);
  VerifyState(ObjA.M_many, bvpsCurrent);
  FSubscriber.VerifySendEvent(beItemDeleted, ObjA.M_many);
  VerifyListAdjustedEx(ObjA.many, ObjB, true);
  VerifyState((NewObjALocator.BoldObject as TClassWithLink).M_many, bvpsCurrent);
  FSubscriber.VerifySendEvent(beItemAdded, (NewObjALocator.BoldObject as TClassWithLink).M_many);
  VerifyListAdjustedIn((NewObjALocator.BoldObject as TClassWithLink).many, ObjB, true);
  UndoModifySingleRoleStateInDb;
end;


procedure Tmaan_IndirectSingleFetchRefetchTestCase.IndirectSingle_FetchMultiRoleInvalid;
var
  ObjA, ObjB, ObjA2, ObjB2: TClassWithLink;
  ObjBLocator: TBoldObjectLocator;
  EmbeddedIndex: integer;

  procedure prepare;
  begin
    if not dmUndoRedo.BoldSystemHandle1.Active then
      dmUndoRedo.BoldSystemHandle1.Active := true;
    FClassWithLinkList[0].many.Clear;
    FClassWithLinkList[2].many.Clear;
    dmUndoRedo.BoldSystemHandle1.UpdateDatabase;
    FClassWithLinkList[1].one := FClassWithLinkList[0];
    FClassWithLinkList[3].one := FClassWithLinkList[2];
    dmUndoRedo.BoldSystemHandle1.UpdateDatabase;
    RefreshSystem;
    ObjA := FClassWithLinkList[0];
    ObjBLocator := FClassWithLinkList.locators[1];
    ObjA2 := FClassWithLinkList[2];
    ObjB2 := FClassWithLinkList[3];
    VerifyState(ObjA.M_many, bvpsInvalid);
    EmbeddedIndex := ObjA.M_one.BoldMemberRTInfo.EmbeddedLinkIndex ;
  end;

  procedure ModifySingleRoleStateInDb;
  begin
    OpenSystem2;
    FClassWithLinkList2[3].one := FClassWithLinkList2[0];
    SaveAndCloseSystem2;
  end;

begin
  SetConfigurationForIndirectSingle;
  {ObjB.one invalid}
  Prepare;
  StoreValue(ObjA.M_many);
  ObjA.many.EnsureContentsCurrent;
  VerifySetContents(ObjA.M_many.AsIBoldValue[bdepContents], GetStoredValueOfMember(ObjA.M_many));
  VerifyState(ObjA.M_many, bvpsCurrent);
  Assert(ObjBLocator.EmbeddedSingleLinks[EmbeddedIndex].BoldObjectID.AsString = ObjA.BoldObjectLocator.BoldObjectID.AsString);

  {ObjB.one Invalid/Adjust}
  {after fetch ObjB not included in ObjA.many}
  Prepare;
  ObjB := (ObjBLocator.EnsuredBoldObject as TClassWithLink);
  ObjB.M_one.Invalidate;
  StoreValue(ObjA.M_many);
  VerifyState(ObjB.M_one, bvpsInvalid);
  VerifyAdjustFlag(ObjB.M_one);

  //change in DB
  OpenSystem2;
  FClassWithLinkList2.EnsureObjects;
  FClassWithLinkList2[1].one := nil;
  System2.UpdateDatabase;
  SaveAndCloseSystem2;

  ObjA.many.EnsureContentsCurrent;
  VerifySetContents(ObjA.M_many.AsIBoldValue[bdepContents], GetStoredValueOfMember(ObjA.M_many));
  VerifyState(ObjA.M_many, bvpsCurrent);
  VerifyState(ObjB.M_one, bvpsInvalid);


  {ObjB.one Invalid/Adjust}
  {after fetch ObjB.one not modified}
  Prepare;
  ObjB := (ObjBLocator.EnsuredBoldObject as TClassWithLink);
  ObjB.M_one.Invalidate;
  VerifyState(ObjB.M_one, bvpsInvalid);
  VerifyAdjustFlag(ObjB.M_one);
  StoreValue(ObjA.M_many);
  ObjA.many.EnsureContentsCurrent;
  VerifySetContents(ObjA.M_many.AsIBoldValue[bdepContents], GetStoredValueOfMember(ObjA.M_many));
  VerifyState(ObjA.M_many, bvpsCurrent);
  VerifyState(ObjB.M_one, bvpsCurrent); // if invalid/adjust set state to current

  {ObjB2.one Invalid/Adjust}
  {after fetch ObjB2 included in ObjA.many}
  Prepare;
  ObjA2.M_many.EnsureContentsCurrent;  //fetch
  VerifyState(ObjA2.M_many, bvpsCurrent);
  Assert(ObjB2.one = ObjA2);
  ObjB2.Invalidate;
  VerifyState(ObjB2.M_one, bvpsInvalid);
  VerifyAdjustFlag(ObjB2.M_one);
  VerifyState(ObjA.M_many, bvpsInvalid);
  FSubscriber.SubscribeToElement(ObjA2.M_many);

  //change in DB
  OpenSystem2;
  FClassWithLinkList2.EnsureObjects;
  FClassWithLinkList2[3].one := FClassWithLinkList2[0];
  System2.UpdateDatabase;
  SaveAndCloseSystem2;

  VerifyState(ObjA.M_many, bvpsInvalid);
  VerifyState(ObjB2.M_one, bvpsInvalid);
  VerifyAdjustFlag(ObjB2.M_one);
  Assert((ObjB2.M_one as TBoldObjectReference).Locator = ObjA2.BoldObjectLocator);
  ObjA.many.EnsureContentsCurrent;
  Assert(ObjA.many.count = 2);
  VerifyState(ObjA.M_many, bvpsCurrent);
  VerifyState(ObjB2.M_one, bvpsCurrent);
  Assert(ObjB2.one = ObjA);
  VerifyListAdjustedEx(ObjA2.M_many, ObjB2, true);
  FSubscriber.VerifySendEvent(beItemDeleted, ObjA2.M_many);

  {ObjB.one Current}
  {after fetch ObjB not included in ObjA.many}
  Prepare;
  ObjB := (ObjBLocator.EnsuredBoldObject as TClassWithLink);
  VerifyState(ObjB.M_one, bvpsCurrent);
  StoreValue(ObjA.M_many);
  //change in DB
  OpenSystem2;
  FClassWithLinkList2.EnsureObjects;
  FClassWithLinkList2[1].one := nil;
  System2.UpdateDatabase;
  SaveAndCloseSystem2;

  ObjA.many.EnsureContentsCurrent;
  VerifySetContents(ObjA.M_many.AsIBoldValue[bdepContents], GetStoredValueOfMember(ObjA.M_many));
  VerifyState(ObjA.M_many, bvpsCurrent);
  VerifyState(ObjB.M_one, bvpsInvalid);

  {ObjB.one Current}
  {after fetch ObjB not modified}
  Prepare;
  ObjB := (ObjBLocator.EnsuredBoldObject as TClassWithLink);
  VerifyState(ObjB.M_one, bvpsCurrent);
  StoreValue(ObjA.M_many);
  ObjA.many.EnsureContentsCurrent;
  VerifySetContents(ObjA.M_many.AsIBoldValue[bdepContents], GetStoredValueOfMember(ObjA.M_many));
  VerifyState(ObjA.M_many, bvpsCurrent);
  VerifyState(ObjB.M_one, bvpsCurrent);

  {ObjB.one Current}
  {after fetch ObjB2 included in ObjA.many}
  Prepare;
  ObjA2.M_many.EnsureContentsCurrent;
  VerifyState(ObjA2.M_many, bvpsCurrent);
  VerifyState(ObjB2.M_one, bvpsCurrent);
  StoreValue(ObjB2.M_one);
  StoreValue(ObjA.M_many);
  FSubscriber.SubscribeToElement(ObjA2.M_many);
  FSubscriber.SubscribeToElement(ObjB2.M_one);
  //change in DB
  OpenSystem2;
  FClassWithLinkList2.EnsureObjects;
  FClassWithLinkList2[3].one := FClassWithLinkList2[0];
  System2.UpdateDatabase;
  SaveAndCloseSystem2;

  ObjA.many.ensureContentsCurrent;
  VerifySetContents(ObjA.M_many.AsIBoldValue[bdepContents], GetStoredValueOfMember(ObjA.M_many));
  VerifyState(ObjA.M_many, bvpsCurrent);
  VerifySetContents(ObjB2.M_one.AsIBoldValue[bdepContents], GetStoredValueOfMember(ObjB2.M_one));
  VerifyState(ObjB2.M_one, bvpsCurrent);
  FSubscriber.VerifySendEvent(beValueChanged, ObjB2.M_one);
  VerifyListAdjustedEx(ObjA2.M_many, ObjB2, true);
  FSubscriber.VerifySendEvent(beItemDeleted, ObjA2.M_many);
end;

class procedure Tmaan_IndirectSingleFetchRefetchTestCase.Suit(
  ASuite: TBoldTestSuite);
begin
  ASuite.AddTest(CreateWithComment('IndirectSingle_FetchSingleRoleCurrent'));
  ASuite.AddTest(CreateWithComment('IndirectSingle_FetchSingleRoleInvalid'));
  ASuite.AddTest(CreateWithComment('IndirectSingle_FetchSingleRoleInvalidAdjust'));
  ASuite.AddTest(CreateWithComment('IndirectSingle_FetchMultiRoleInvalid'));
end;

class function Tmaan_IndirectSingleFetchRefetchTestCase.Suite: ITestSuite;
begin
  result := inherited suite;
  SetCommentForTest(Result, 'IndirectSingle_FetchSingleRoleCurrent', '');
  SetCommentForTest(Result, 'IndirectSingle_FetchSingleRoleInvalid', '');
  SetCommentForTest(Result, 'IndirectSingle_FetchSingleRoleInvalidAdjust', '');
  SetCommentForTest(Result, 'IndirectSingle_FetchMultiRoleInvalid', '');
end;

{ Tmaan_IndirectSingleUndoTestCase }

procedure Tmaan_IndirectSingleUndoTestCase.IndirectSingle_UndoSingleRoleModified;
var
  ObjA1, ObjA2, ObjB1, ObjB2: TClassWithLink;
  ObjA1Id, ObjA2Id, ObjB1Id, ObjB2Id: TBoldobjectId;
  aLinkClass, newLinkClass: TLinkClass;
  aLinkClassId, newLinkclassId: TBoldObjectId;
  FSValue: TBFSObjectIdRef;
  procedure Prepare;
  begin
    RefreshSystem;
    FClassWithLinkList[1].one := FClassWithLinkList[0];
    FClassWithLinkList[3].one := FClassWithLinkList[2];
    RefreshSystem;
    ObjA1 := FClassWithLinkList[0];
    ObjA1Id := ObjA1.BoldObjectLocator.BoldObjectId.Clone;
    ObjB1 := FClassWithLinkList[1];
    ObjB1Id := ObjB1.BoldObjectLocator.BoldObjectId.Clone;
    ObjA2 := FClassWithLinkList[2];
    ObjA2Id := objA2.BoldObjectLocator.BoldObjectId.Clone;
    ObjB2 := FClassWithLinkList[3];
    objB2Id := ObjB2.BoldObjectLocator.BoldObjectId.Clone;
  end;

  procedure SetObjectsFromIds;
  begin
    ObjA1 := System.Locators.ObjectById[ObjA1Id] as TClassWithLink;
    ObjB1 := System.Locators.ObjectById[ObjB1Id] as TClassWithLink;
    ObjA2 := System.Locators.ObjectById[ObjA2Id] as TClassWithLink;
    ObjB2 := System.Locators.ObjectById[ObjB2Id] as TClassWithLink;
  end;

  procedure VerifyIsInBlock(Block: TBoldUndoBlock; aId: TboldObjectId; OneId, ManyId: TBoldObjectId);
  var
    ValueOne, ValueMany: IBoldValue;
  begin
    Block.ValueExists(aId, ObjB1.M_one.BoldRoleRTInfo.OwnIndexInLinkClass, ValueOne);
    Block.ValueExists(aId, ObjA1.M_many.BoldRoleRTInfo.OwnIndexInLinkClass, ValueMany);
    Assert(Assigned(ValueOne) and
    ((ValueOne as IBoldObjectIdREf).Id.AsString = OneId.AsString));
    Assert(Assigned(ValueMany) and
     ((ValueMany as IBoldObjectIdRef).Id.AsString = ManyId.AsString));
  end;

begin
  SetConfigurationForIndirectSingle;
  Prepare;
  {ObjB1.one invalid, ObjA1 invalid, ObjA2 invalid}
  FSValue := TBFSObjectIdRef.create;
  try
    VerifyState(ObjB1.M_one, bvpsInvalid);
    Assert(ObjB1.one = ObjA1);
    aLinkClass := ObjB1.oneLinkClass;
    aLinkClassId := aLinkClass.BoldObjectLocator.BoldObjectID.clone;
    UndoHandler.SetCheckPoint;
    ObjB1.one := ObjA2; // modify
    VerifyState(ObjA1.M_many, bvpsInvalid);
    VerifyState(ObjA2.M_many, bvpsInvalid);
    Assert((not Assigned(System.Locators.ObjectByID[aLinkClassId])) or
           (System.Locators.ObjectByID[aLinkClassId].BoldExistenceState = besDeleted));
    VerifyObjectInBlock(UndoHandler.UndoBlocks.CurrentBlock, aLinkClassId, besExisting);
    newLinkClass := ObjB1.oneLinkclass;
    newLinkClassId := newLinkClass.BoldObjectLocator.BoldObjectId.clone;
    VerifyObjectInBlock(UndoHandler.UndoBlocks.CurrentBlock, newLinkclassId, besNotCreated);
    FSubscriber.SubscribeToElement(ObjB1.M_one);
    UndoHandler.UndoLatest; // Undo
    SetObjectsFromIds;
    Assert(Assigned(System.Locators.ObjectByID[aLinkClassId]));
    Assert(not Assigned(System.Locators.ObjectByID[newLinkClassId]));
    Assert(ObjB1.one = ObjA1);
    VerifyObjectInBlock(UndoHandler.RedoBlocks.CurrentBlock, newLinkclassId, besExisting);
    FSubscriber.VerifySendEvent(beValueChanged, ObjB1.M_one);
    VerifyState(ObjA2.M_many, bvpsinvalid);
    VerifyState(ObjA1.M_many, bvpsInvalid);
    UndoHandler.Redolatest; //Redo
    SetObjectsFromIds;
    Assert(ObjB1.one = ObjA2);
    VerifyState(ObjA1.M_many, bvpsInvalid);
    VerifyState(ObjA2.M_many, bvpsInvalid);
    Assert((not Assigned(System.Locators.ObjectByID[aLinkClassId])) or
           (System.Locators.ObjectByID[aLinkClassId].BoldExistenceState = besDeleted));
    VerifyObjectInBlock(UndoHandler.UndoBlocks.CurrentBlock, aLinkClassId, besExisting);
    Assert((Assigned(System.Locators.ObjectByID[newLinkClassId])) or
           (System.Locators.ObjectByID[newLinkClassId].BoldExistenceState = besExisting));
//    VerifyObjectInBlock(UndoHandler.RedoBlocks.CurrentBlock, newLinkclassId, besNotCreated);
  finally
    FreeAndNil(FSValue);
  end;

  {ObjB1.one current, ObjA1 invalid}
  Prepare;
  FSValue := TBFSObjectIdRef.create;
  try
    ObjB1.M_one.EnsureContentsCurrent;
    VerifyState(ObjB1.M_one, bvpsCurrent);
    aLinkClass := ObjB1.oneLinkClass;
    aLinkClassId := aLinkClass.BoldObjectLocator.BoldObjectId.Clone;
    Assert(ObjB1.one = ObjA1);
    ObjB1.one := ObjA2; // modify
    newLinkClass := ObjB1.oneLinkclass;
    newLinkClassId := newLinkClass.BoldObjectLocator.BoldObjectId.Clone;
    VerifyState(ObjA1.M_many, bvpsInvalid);
    ObjA2.many.EnsureContentsCurrent;
    VerifyState(ObjA2.M_many, bvpsCurrent);
    FSubscriber.SubscribeToElement(ObjB1.M_one);
    FSubscriber.SubscribeToElement(ObjA2.M_many);
    FSubscriber.SubscribeToElement(ObjA1.M_many);
    Assert((not Assigned(System.Locators.ObjectByID[aLinkClassId])) or
           (System.Locators.ObjectByID[aLinkClassId].BoldExistenceState = besDeleted));
    VerifyObjectInBlock(UndoHandler.UndoBlocks.CurrentBlock, aLinkClassId, besExisting);
    UndoHandler.UndoLatest; // Undo
    SetObjectsFromIds;
    Assert(Assigned(System.Locators.ObjectByID[aLinkClassId]));
    Assert(not Assigned(System.Locators.ObjectByID[newLinkClassId]));
    Assert(ObjB1.one = ObjA1);
    VerifyObjectInBlock(UndoHandler.RedoBlocks.CurrentBlock, newLinkclassId, besExisting);
    FSubscriber.VerifySendEvent(beValueChanged, ObjB1.M_one);
    VerifyState(ObjA1.M_many, bvpsInvalid);
    VerifyState(ObjA2.M_many, bvpsCurrent);
    Assert(ObjA2.many.IndexOf(ObjB1) = -1);
    FSubscriber.VerifySendEvent(beItemDeleted, ObjA2.M_many);
    UndoHandler.Redolatest; //Redo
    VerifyState(ObjA1.M_many, bvpsInvalid);
    VerifyState(ObjA2.M_many, bvpsCurrent);
    Assert((not Assigned(System.Locators.ObjectByID[aLinkClassId])) or
           (System.Locators.ObjectByID[aLinkClassId].BoldExistenceState = besDeleted));
    VerifyObjectInBlock(UndoHandler.UndoBlocks.CurrentBlock, aLinkClassId, besExisting);
    Assert(ObjA2.many.IndexOf(ObjB1) <> -1);
    FSubscriber.VerifySendEvent(beItemAdded, ObjA2.M_many);
  finally
    FreeAndNil(FSValue);
  end;

  {ObjB2.one current}
  Prepare;
  FSValue := TBFSObjectIdRef.create;
  try
    ObjB2.M_one.EnsureContentsCurrent;
    VerifyState(ObjB2.M_one, bvpsCurrent);
    Assert(objB2.one = ObjA2);
    aLinkClass := ObjB2.oneLinkClass;
    aLinkClassId := aLInkClass.BoldObjectLocator.BoldObjectId.Clone;
    UndoHandler.SetCheckPoint;
    ObjB2.one := ObjA1; // modify
    newLinkClass := ObjB2.oneLinkClass;
    newLinkClassId := newLinkClass.BoldObjectLocator.BoldObjectid.clone;
    Assert((not Assigned(System.Locators.ObjectByID[aLinkClassId])) or
           (System.Locators.ObjectByID[aLinkClassId].BoldExistenceState = besDeleted));
    VerifyObjectInBlock(UndoHandler.UndoBlocks.CurrentBlock, aLinkClassId, besExisting);
    ObjA1.many.EnsureContentsCurrent;
    ObjA2.many.EnsureContentsCurrent;
    VerifyState(ObjA1.M_many, bvpsCurrent);
    VerifyState(ObjA2.M_many, bvpsCurrent);
    FSubscriber.SubscribeToElement(ObjB2.m_one);
    FSubscriber.SubscribeToElement(ObjA1.M_many);
    FSubscriber.SubscribeToElement(ObjA2.M_many);
    UndoHandler.UndoLatest; //Undo
    SetObjectsFromIds;
    Assert(Assigned(System.Locators.ObjectByID[aLinkClassId]));
    Assert(not Assigned(System.Locators.ObjectByID[newLinkClassId]));
    Assert(ObjB1.one = ObjA1);
    VerifyObjectInBlock(UndoHandler.RedoBlocks.CurrentBlock, newLinkclassId, besExisting);
    FSubscriber.VerifySendEvent(beValueChanged, ObjB2.M_one);
    VerifyListAdjustedEx(ObjA1.many, ObjB2, false);
    VerifyListAdjustedIn(ObjA2.many, ObjB2, false);
    FSubscriber.VerifySendEvent(beItemDeleted, ObjA1.M_many);
    FSubscriber.VerifySendEvent(beItemAdded, ObjA2.M_many);
    UndoHandler.RedoLatest; //Redo
    Assert((not Assigned(System.Locators.ObjectByID[aLinkClassId])) or
           (System.Locators.ObjectByID[aLinkClassId].BoldExistenceState = besDeleted));
    VerifyObjectInBlock(UndoHandler.UndoBlocks.CurrentBlock, aLinkClassId, besExisting);
    FSubscriber.VerifySendEvent(beItemAdded, ObjA1.M_many);
    FSubscriber.VerifySendEvent(beItemDeleted, ObjA2.M_many);
  finally
    FreeAndNil(FSValue);
  end;
end;

class procedure Tmaan_IndirectSingleUndoTestCase.Suit(ASuite: TBoldTestSuite);
begin
  ASuite.AddTest(CreateWithComment('IndirectSingle_UndoSingleRoleModified', ' 2.7b- Undo block & 2.7c- Redo block'));
end;

class function Tmaan_IndirectSingleUndoTestCase.Suite: ITestSuite;
begin
  Result := inherited Suite;
  SetCommentForTest(result, 'IndirectSingle_UndoSingleRoleModified', ' 2.7b- Undo block & 2.7c- Redo block');
end;

{ Tmaan_IndirectSingleModifyTestCase }

procedure Tmaan_IndirectSingleModifyTestCase.IndirectSingle_ModifySingleRoleCurrent;
var
  ObjA, ObjB, ObjA2: TClassWithLink;
  aLinkObjectLocator, newLinkObjectLocator: TBoldObjectLocator;
  aLinkObjectId, newLinkObjectId: TBoldObjectId;

  procedure Prepare;
  begin
    RefreshSystem;
    ObjA := FClassWithLinkList[0];
    ObjB := FClassWithLinkList[1];
    ObjA2 := FClassWithLinkList[2];
    ObjB.M_one.EnsureContentsCurrent;
    VerifyState(ObjB.M_one, bvpsCurrent);
  end;
begin
  SetConfigurationForIndirectSingle;
  {ObjA.many Invalid}
  Prepare;
  ObjA.many.EnsureContentsCurrent;
  VerifyState(ObjA.M_many, bvpsCurrent);
  VerifyState(ObjA2.M_many, bvpsInvalid);
  FSubscriber.SubscribeToElement(ObjB.M_one);
  FSubscriber.SubscribeToElement(ObjA.M_many);
  Assert(ObjB.one = ObjA);
  aLinkObjectLocator := objB.oneLinkClass.BoldObjectLocator;
  aLinkObjectId := ALinkObjectLocator.BoldObjectId;
  ObjB.one := ObjA2; // modify
  Assert(ObjB.one = ObjA2);
  newLinkObjectLocator := objB.oneLinkClass.boldObjectLocator;
  newLinkObjectId := newLinkObjectLocator.BoldObjectId;
  VerifyObjectInBlock(UndoHandler.UndoBlocks.CurrentBlock, aLinkObjectId, besExisting);
  Assert( not Assigned(system.Locators.ObjectByID[aLinkObjectId]) or
          (system.Locators.ObjectByID[aLinkObjectId].BoldExistenceState = besDeleted));
  VerifyObjectInBlock(UndoHandler.UndoBlocks.CurrentBlock, newLinkObjectId, besNotCreated);
  FSubscriber.VerifySendEvent(beValueChanged, ObjB.M_one);
  VerifyState(ObjA.M_many, bvpsCurrent);
  VerifyState(ObjA2.M_many, bvpsInvalid);
  VerifyListAdjustedEx(ObjA.many, ObjB, false);
  FSubscriber.VerifySendEvent(beItemDeleted, ObjA.M_many);
  ObjB.one := ObjA;

  {ObjA.many Current}
  Prepare;
  ObjA.many.EnsureContentsCurrent;
  ObjA2.many.EnsureContentsCurrent;
  VerifyState(ObjA.M_many, bvpsCurrent);
  VerifyState(ObjA2.M_many, bvpsCurrent);
  FSubscriber.SubscribeToElement(ObjB.M_one);
  FSubscriber.SubscribeToElement(ObjA.M_many);
  FSubscriber.SubscribeToElement(ObjA2.M_many);
  Assert(ObjB.one = ObjA);
  aLinkObjectLocator := objB.oneLinkClass.BoldObjectLocator;
  aLinkObjectId := ALinkObjectLocator.BoldObjectId;
  ObjB.one := ObjA2; // modify
  Assert(objB.one = ObjA2);
  newLinkObjectLocator := objB.oneLinkClass.boldObjectLocator;
  newLinkObjectId := newLinkObjectLocator.BoldObjectId;
  VerifyObjectInBlock(UndoHandler.UndoBlocks.CurrentBlock, aLinkObjectId, besExisting);
  Assert( not Assigned(system.Locators.ObjectByID[aLinkObjectId]) or
          (system.Locators.ObjectByID[aLinkObjectId].BoldExistenceState = besDeleted));
  VerifyObjectInBlock(UndoHandler.UndoBlocks.CurrentBlock, newLinkObjectId, besNotCreated);
  FSubscriber.VerifySendEvent(beValueChanged, ObjB.M_one);
  VerifyState(ObjA.M_many, bvpsCurrent);
  VerifyListAdjustedEx(ObjA.many, ObjB, false);
  FSubscriber.VerifySendEvent(beItemDeleted, ObjA.M_many);
  VerifyState(ObjA2.M_many, bvpsCurrent);
  VerifyListAdjustedIn(ObjA2.many, ObjB, false);
  FSubscriber.VerifySendEvent(beItemAdded, ObjA2.M_many);
  ObjB.one := ObjA;

  CloseAll;
end;

procedure Tmaan_IndirectSingleModifyTestCase.IndirectSingle_ModifyMultiRoleDeleteCurrent;
var
  ObjA, ObjB: TClassWithLink;
  EmbeddedIndex: integer;
  aLinkObjectLocator: TBoldObjectLocator;
  aLinkObjectId: TBoldObjectId;
  procedure Prepare;
  begin
    FClassWithLinkList[1].one := FClassWithLinkList[0];
    FClassWithLinkList[3].one := FClassWithLinklist[2];
    EmbeddedIndex := FClassWithLinklist[1].M_one.BoldMemberRTInfo.EmbeddedLinkIndex;
    RefreshSystem;
    ObjA := FClassWithLinkList[0];
    ObjB := FClassWithLinkList[1];
    ObjA.many.EnsureContentsCurrent;
    VerifyState(ObjA.M_many, bvpsCurrent);
  end;

begin
  SetConfigurationForIndirectSingle;
  {ObjB.one invalid}
  Prepare;
  FSubscriber.SubscribeToElement(ObjA.M_many);
  VerifyState(ObjB.M_one, bvpsInvalid);
  aLinkobjectLocator := ObjA.manyLinkClass.Locators[ObjA.many.IndexofLocator(ObjB.BoldObjectLocator)];
  aLInkObjectId := aLinkObjectLocator.BoldObjectID.Clone;
  ObjA.many.RemoveByIndex(ObjA.many.IndexOfLocator(ObjB.BoldObjectLocator)); //modify-delete
  VerifyObjectInBlock(UndoHandler.UndoBlocks.CurrentBlock, aLinkObjectId, besExisting);
  Assert( not Assigned(System.Locators.ObjectById[aLinkObjectId]) or
          (System.Locators.ObjectByID[aLinkObjectId].BoldExistenceState = besDeleted) );
  Assert(ObjB.one = nil);
  VerifyState(ObjA.M_many, bvpsCurrent);
  Assert(ObjA.many.Count = 0, 'ListAdjustedEx failed');
  Assert(not ObjA.many.Includes(ObjB));
  FSubscriber.VerifySendEvent(beItemDeleted, ObjA.M_many);

  {ObjB.one invalid/adjust}
//TODO: indirect-single links don't have an invalid/adjust state
{  Prepare;
  ObjB.M_one.EnsureContentsCurrent;
  VerifyState(ObjB.M_one, bvpsCurrent);
  ObjB.M_one.Invalidate;
  VerifyState(ObjB.M_one, bvpsInvalid);
  VerifyHasOldValues(ObjB.M_one, true);
  FSubscriber.SubscribeToElement(ObjA.M_many);
  aLinkobjectLocator := ObjA.manyLinkClass.Locators[ObjA.many.IndexofLocator(ObjB.BoldObjectLocator)];
  aLInkObjectId := aLinkObjectLocator.BoldObjectID.Clone;
  ObjA.many.Remove(ObjB); //modify-delete
  VerifyObjectInBlock(UndoHandler.UndoBlocks.CurrentBlock, aLinkObjectId, besExisting);
  Assert( not Assigned(System.Locators.ObjectById[aLinkObjectId]) or
          (System.Locators.ObjectByID[aLinkObjectId].BoldExistenceState = besDeleted) );
  Assert(ObjB.one = nil);
  VerifyState(ObjA.M_many, bvpsCurrent);
  VerifyListAdjustedEx(ObjA.many, ObjB, false);
  FSubscriber.VerifySendEvent(beItemDeleted, ObjA.M_many);
}
  {ObjB.one current}
  Prepare;
  ObjB.M_one.EnsureContentsCurrent;
  VerifyState(ObjB.M_one, bvpsCurrent);
  FSubscriber.SubscribeToElement(ObjA.M_many);
  FSubscriber.SubscribeToElement(ObjB.M_one);
  aLinkobjectLocator := ObjA.manyLinkClass.Locators[ObjA.many.IndexofLocator(ObjB.BoldObjectLocator)];
  aLInkObjectId := aLinkObjectLocator.BoldObjectID.Clone;
  ObjA.many.Remove(ObjB); //modify-delete
  Assert(ObjB.one = nil);
  FSubscriber.VerifySendEvent(beValueChanged, ObjB.M_one);
  VerifyObjectInBlock(UndoHandler.UndoBlocks.CurrentBlock, aLinkObjectId, besExisting);
  Assert( not Assigned(System.Locators.ObjectById[aLinkObjectId]) or
          (System.Locators.ObjectByID[aLinkObjectId].BoldExistenceState = besDeleted) );
  VerifyState(ObjA.M_many, bvpsCurrent);
  VerifyListAdjustedEx(ObjA.many, ObjB, false);
  FSubscriber.VerifySendEvent(beItemDeleted, ObjA.M_many);

  CloseAll;
end;

procedure Tmaan_IndirectSingleModifyTestCase.IndirectSingle_ModifyMultiRoleInsertCurrent;
var
  ObjA, ObjB, ObjA2: TClassWithLink;
  aLinkObjectLocator: TBoldObjectLocator;
  aLinkObjectId: TBoldObjectId;
  EmbeddedIndex: integer;
  procedure Prepare;
  begin
    FClassWithLinkList[1].one := nil;
    FClassWithLinkList[3].one := FClassWithLinklist[2];
    RefreshSystem;
    ObjA := FClassWithLinkList[0];
    ObjB := FClassWithLinkList[1];
    ObjA2 := (FClassWithLinkList.Locators[2].EnsuredBoldObject as TClassWithLink);
    ObjA.many.EnsureContentsCurrent;
    EmbeddedIndex := ObjA.M_one.BoldMemberRTInfo.EmbeddedLinkIndex;
    VerifyState(ObjA.M_many, bvpsCurrent);
  end;

begin
  SetConfigurationForIndirectSingle;
  {ObjB.one invalid}
  Prepare;
  FSubscriber.SubscribeToElement(ObjA.M_many);
  VerifyState(ObjB.M_one, bvpsInvalid);
  ObjA.many.Add(ObjB); //modify-insert
  aLinkObjectLocator := ObjA.manyLinkClass.Locators[ObjA.many.IndexOfLocator(ObjB.BoldObjectLocator)];
  aLinkObjectId := aLinkObjectLocator.BoldObjectId.Clone;
  VerifyObjectInBlock(UndoHandler.UndoBlocks.CurrentBlock, aLinkObjectId, besNotCreated);
  Assert((Assigned(System.Locators.ObjectById[aLinkObjectId])) or
         (System.Locators.ObjectById[aLinkObjectId].BoldExistenceState = besExisting));
  VerifyState(ObjA.M_many, bvpsCurrent);
  Assert(ObjB.one = ObjA);
  Assert(ObjA.many.Includes(ObjB), 'VerifyListAdjustedIn failed');
  FSubscriber.VerifySendEvent(beItemAdded, ObjA.M_many);

  {ObjB.one invalid/adjust}
//TODO: review, indirect single links don't have an invalid/adjust state
{  Prepare;
  ObjB.M_one.EnsurecontentsCurrent;
  VerifyState(ObjB.M_one, bvpsCurrent);
  ObjB.M_one.Invalidate;
  VerifyState(ObjB.M_one, bvpsInvalid);
  VerifyHasOldValues(ObjB.M_one, true);
  FSubscriber.SubscribeToElement(ObjA.M_many);
  ObjA.M_many.Add(ObjB); //modify
  VerifyState(ObjA.M_many, bvpsCurrent);
  VerifyState(ObjB.M_one, bvpsCurrent);
  aLinkObjectLocator := ObjA.manyLinkClass.Locators[ObjA.many.IndexOfLocator(ObjB.BoldObjectLocator)];
  aLinkObjectId := aLinkObjectLocator.BoldObjectId.Clone;
  VerifyObjectInBlock(UndoHandler.UndoBlocks.CurrentBlock, aLinkObjectId, besNotCreated);
  Assert((Assigned(System.Locators.ObjectById[aLinkObjectId])) or
         (System.Locators.ObjectById[aLinkObjectId].BoldExistenceState = besExisting));
  VerifyListAdjustedIn(ObjA.many, ObjB, false);
  FSubscriber.VerifySendEvent(beItemAdded, ObjA.M_many);
}
  {ObjB.one current}
  Prepare;
  ObjB.M_one.EnsureContentsCurrent;
  VerifyState(ObjB.M_one, bvpsCurrent);
  FSubscriber.SubscribeToElement(ObjA.M_many);
  FSubscriber.SubscribeToElement(ObjB.M_one);
  StoreValue(ObjB.M_one);
  ObjA.many.Add(ObjB); //modify
  VerifyState(ObjB.M_one, bvpsCurrent);
  aLinkObjectLocator := ObjA.manyLinkClass.Locators[ObjA.many.IndexOfLocator(ObjB.BoldObjectLocator)];
  aLinkObjectId := aLinkObjectLocator.BoldObjectId.Clone;
  VerifyObjectInBlock(UndoHandler.UndoBlocks.CurrentBlock, aLinkObjectId, besNotCreated);
  Assert((Assigned(System.Locators.ObjectById[aLinkObjectId])) or
         (System.Locators.ObjectById[aLinkObjectId].BoldExistenceState = besExisting));
  FSubscriber.VerifySendEvent(beValueChanged, ObjB.M_one);
  VerifyState(ObjA.M_many, bvpsCurrent);
  VerifyListAdjustedIn(ObjA.many, ObjB, false);
  FSubscriber.VerifySendEvent(beItemAdded, ObjA.M_many);

  CloseAll;
end;

class procedure Tmaan_IndirectSingleModifyTestCase.Suit(ASuite: TBoldTestSuite);
begin
  ASuite.AddTest(CreateWithComment('IndirectSingle_ModifySingleRoleCurrent', '2.7b & 2.2a Undo block'));
  ASuite.AddTest(CreateWithComment('IndirectSingle_ModifyMultiRoleDeleteCurrent', '2.7b & 2.2a Undo block'));
  ASuite.AddTest(CreateWithComment('IndirectSingle_ModifyMultiRoleInsertCurrent', '2.7b & 2.2a Undo block'));
end;

class function Tmaan_IndirectSingleModifyTestCase.Suite: ITestSuite;
begin
  Result := inherited Suite;
  SetCommentForTest(Result, 'IndirectSingle_ModifySingleRoleCurrent', '2.7b & 2.2a Undo block');
  SetCommentForTest(Result, 'IndirectSingle_ModifyMultiRoleDeleteCurrent', '2.7b & 2.2a Undo block');
  SetCommentForTest(Result, 'IndirectSingle_ModifyMultiRoleInsertCurrent', '2.7b & 2.2a Undo block');
end;

initialization
//  TestGlobal.RegisterTestCase(Tmaan_IndirectSingleFetchRefetchTestCase);
  TestGlobal.RegisterTestCase(Tmaan_IndirectSingleModifyTestCase);
  TestGlobal.RegisterTestCase(Tmaan_IndirectSingleUndoTestCase);
finalization
//  TestGlobal.UnRegisterTestCase(Tmaan_IndirectSingleFetchRefetchTestCase);
  TestGlobal.UnRegisterTestCase(Tmaan_IndirectSingleModifyTestCase);
  TestGlobal.UnRegisterTestCase(Tmaan_IndirectSingleUndoTestCase);
end.
