unit maan_UndoRedoIndirectMultiLinks;

interface

uses
  maan_UndoRedo,
  SysUtils,
  BoldUtils,
  TestSuite,
  TestFrameWork;

type

  Tmaan_IndirectMultiLinksFetchRefetchTestCase = class(Tmaan_UndoRedoAbstractTestCase)
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
  published
    procedure IndirectMulti_FetchRefetch;
  end;

  Tmaan_IndirectMultiLinksModifyTestCase = class(Tmaan_UndoRedoAbstractTestCase)
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
  published
    procedure IndirectMulti_ModifyInsertCurrent;
    procedure IndirectMulti_ModifyDeleteCurrent;
  end;

  Tmaan_IndirectMultiLinksUndoTestCase = class(Tmaan_UndoRedoAbstractTestCase)
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
  published
    procedure IndirectMulti_Undo;
  end;

  
implementation

uses
  BoldSystem,
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


{ Tmaan_IndirectMultiLinksFetchRefetchTestCase }

procedure Tmaan_IndirectMultiLinksFetchRefetchTestCase.IndirectMulti_FetchRefetch;
var
  ObjA, ObjB, ObjA2, ObjB2: TSomeClass;

  procedure prepare;
  begin
    if not dmUndoRedo.BoldSystemHandle1.Active then
      dmUndoRedo.BoldSystemHandle1.Active := true;
    FSomeClassList[0].part.Clear;
    FsomeClassList[2].partof.Clear;
    dmUndoRedo.BoldSystemHandle1.UpdateDatabase;
    FSomeClassList[1].part.Add(FSomeClassList[0]);
    FSomeClassList[1].part.Add(FSomeClassList[2]);
    FSomeClassList[3].part.Add(FSomeClassList[0]);
    FSomeClassList[3].part.Add(FSomeClassList[2]);
    dmUndoRedo.BoldSystemHandle1.UpdateDatabase;
    RefreshSystem;
    ObjA := FSomeClassList[0];
    ObjB := FSomeClassList[1];
    ObjA2 := FSomeClassList[2];
    ObjB2 := FSomeClassList[3];
    VerifyState(ObjA.M_partof, bvpsInvalid);
  end;

  procedure ModifyEmbeddedStateInDb;
  begin
    OpenSystem2;
    FSomeClassList2[3].parent := FSomeClassList2[0];
    SaveAndCloseSystem2;
  end;

begin
  //Fetching ObjB.part

  SetSimpleConfiguration;
  {ObjA.partof invalid}
  Prepare;
  ObjB.part.EnsureContentsCurrent; //fetch
  Assert(ObjB.part.Includes(ObjA));
  Assert(ObjB.part.Includes(ObjA2));
  VerifyState(ObjB.M_part, bvpsCurrent);

  {ObjA.partof current}
  {after fetch ObjA not included in ObjB.part}
  Prepare;
  ObjA.partof.EnsureContentsCurrent;
  ObjA.Invalidate;
  VerifyState(ObjA.M_partof, bvpsInvalid);

  //change in DB
  OpenSystem2;
  FSomeClassList2.EnsureObjects;
  FSomeClassList2[1].part.RemoveByIndex(FSomeClassList2[1].part.IndexOf(FSomeClassList2[0]));
  System2.UpdateDatabase;
  SaveAndCloseSystem2;

  ObjA.partof.EnsureContentsCurrent;
  VerifyState(ObjA.M_partof, bvpsCurrent);
  Assert(not ObjA.part.Includes(ObjB));
  Assert( not ObjB.part.Includes(ObjA) or (ObjB.M_part.BoldPersistenceState = bvpsInvalid));

  CloseAll;
end;

class procedure Tmaan_IndirectMultiLinksFetchRefetchTestCase.Suit(
  ASuite: TBoldTestSuite);
begin
  ASuite.AddTest(CreateWithComment('IndirectMulti_FetchRefetch'));
end;


class function Tmaan_IndirectMultiLinksFetchRefetchTestCase.Suite: ITestSuite;
begin
  Result := inherited Suite;
  SetCommentForTest(Result, 'IndirectMulti_FetchRefetch', '');
end;

{ Tmaan_IndirectMultiLinksModifyTestCase }

procedure Tmaan_IndirectMultiLinksModifyTestCase.IndirectMulti_ModifyDeleteCurrent;
var
  ObjA, ObjB, ObjA2, ObjB2: TSomeClass;
  ALinkObjectLocator: TBoldObjectLocator;
  aLinkObjectId: TboldObjectId;
  procedure Prepare;
  begin
    if not dmUndoRedo.BoldSystemHandle1.Active then
      dmUndoRedo.BoldSystemHandle1.Active := true;
    FSomeClassList[0].part.Clear;
    FsomeClassList[2].partof.Clear;
    dmUndoRedo.BoldSystemHandle1.UpdateDatabase;
    FSomeClassList[1].part.Add(FSomeClassList[0]);
    FSomeClassList[1].part.Add(FSomeClassList[2]);
    FSomeClassList[3].part.Add(FSomeClassList[0]);
    FSomeClassList[3].part.Add(FSomeClassList[2]);
    dmUndoRedo.BoldSystemHandle1.UpdateDatabase;
    RefreshSystem;
    ObjA := FSomeClassList[0];
    ObjB := FSomeClassList[1];
    ObjA2 := FSomeClassList[2];
    ObjB2 := FSomeClassList[3];
    ObjB.M_part.EnsureContentsCurrent;
    VerifyState(ObjB.M_part, bvpsCurrent);
  end;

begin
  SetSimpleConfiguration;
  {ObjA2.partof invalid}
  Prepare;
  FSubscriber.SubscribeToElement(ObjB.M_part);
  Assert(ObjB.part.Includes(ObjA2));
  VerifyState(ObjA2.M_partof, bvpsInvalid);
  aLinkObjectLocator := ObjB.M_partpartpartof.Locators[ObjB.part.IndexOf(ObjA2)];
  aLinkObjectId := ALinkObjectLocator.BoldObjectId.Clone;
  ObjB.part.RemoveByIndex(ObjB.part.IndexOf(ObjA2)); // modify-delete
//Todo: review, should be current?  VerifyState(ObjA2.M_partof, bvpsInvalid);
  Assert(not objB.part.Includes(ObjA2));
  Assert((not Assigned(System.Locators.ObjectById[aLinkObjectId])) or
         (system.Locators.ObjectById[aLinkObjectId].BoldExistenceState = besDeleted));
  VerifyObjectInBlock(UndoHandler.UndoBlocks.CurrentBlock, alinkObjectId, besExisting);
  FSubscriber.VerifySendEvent(beItemDeleted, ObjB.M_part);

  {ObjA2.partof current}
  Prepare;
  FSubscriber.SubscribeToElement(ObjB.M_part);
  FSubscriber.SubscribeToElement(ObjA2.M_partof);
  Assert(ObjB.part.Includes(ObjA2));
  ObjA2.partof.EnsureContentsCurrent;
  VerifyState(ObjA2.M_partof, bvpsCurrent);
  aLinkObjectLocator := ObjB.M_partpartpartof.Locators[ObjB.part.IndexOf(ObjA2)];
  aLinkObjectId := ALinkObjectLocator.BoldObjectId.Clone;
  ObjB.part.RemoveByIndex(ObjB.part.IndexOf(ObjA2)); // modify-delete
  VerifyState(ObjA2.M_partof, bvpsCurrent);
  Assert(not ObjB.part.Includes(ObjA2));
  Assert(not ObjA2.partof.Includes(ObjB));
  Assert(not objB.part.Includes(ObjA2));
  Assert((not Assigned(System.Locators.ObjectById[aLinkObjectId])) or
         (system.Locators.ObjectById[aLinkObjectId].BoldExistenceState = besDeleted));
  VerifyObjectInBlock(UndoHandler.UndoBlocks.CurrentBlock, alinkObjectId, besExisting);
  FSubscriber.VerifySendEvent(beItemDeleted, ObjA2.M_partof);
  FSubscriber.VerifySendEvent(beItemDeleted, ObjB.M_part);

  CloseAll;
end;

procedure Tmaan_IndirectMultiLinksModifyTestCase.IndirectMulti_ModifyInsertCurrent;
var
  ObjA, ObjB, ObjA2, ObjB2: TSomeClass;
  aLinkObjectLocator: TboldObjectLocator;
  aLinkObjectId: TBoldObjectId;
  procedure Prepare;
  begin
    if not dmUndoRedo.BoldSystemHandle1.Active then
      dmUndoRedo.BoldSystemHandle1.Active := true;
    FSomeClassList[0].part.Clear;
    FsomeClassList[2].partof.Clear;
    dmUndoRedo.BoldSystemHandle1.UpdateDatabase;
    FSomeClassList[1].part.Add(FSomeClassList[0]);
    FSomeClassList[3].part.Add(FSomeClassList[0]);
    FSomeClassList[3].part.Add(FSomeClassList[2]);
    dmUndoRedo.BoldSystemHandle1.UpdateDatabase;
    RefreshSystem;
    ObjA := FSomeClassList[0];
    ObjB := FSomeClassList[1];
    ObjA2 := FSomeClassList[2];
    ObjB2 := FSomeClassList[3];
    ObjB.M_part.EnsureContentsCurrent;
    VerifyState(ObjB.M_part, bvpsCurrent);
  end;

begin
  SetSimpleConfiguration;
  {ObjA2.partof invalid}
  Prepare;
  FSubscriber.SubscribeToElement(ObjB.M_part);
  Assert( not ObjB.part.Includes(ObjA2));
  VerifyState(ObjA2.M_partof, bvpsInvalid);
  ObjB.part.Add(ObjA2); // modify-insert
  aLinkObjectLocator := ObjB.partpartpartof.Locators[ObjB.part.IndexOf(ObjA2)];
  aLinkObjectId := aLinkObjectLocator.BoldObjectId.Clone;
  FSubscriber.VerifySendEvent(beItemAdded, ObjB.M_part);
//TODO: review, should be current?  VerifyState(ObjA2.M_partof, bvpsInvalid);
  Assert(objB.part.Includes(ObjA2));
  VerifyObjectInBlock(UndoHandler.UndoBlocks.CurrentBlock, aLinkObjectId, besNotCreated);
  Assert((Assigned(System.Locators.ObjectById[aLinkObjectId])) and
         (System.Locators.ObjectById[aLinkObjectId].BoldExistenceState = besExisting));

  {ObjA2.partof current}
  Prepare;
  FSubscriber.SubscribeToElement(ObjB.M_part);
  FSubscriber.SubscribeToElement(ObjA2.M_partof);
  ObjA2.partof.EnsureContentsCurrent;
  Assert( not ObjB.part.Includes(ObjA2));
  VerifyState(ObjA2.M_partof, bvpsCurrent);
  ObjB.part.Add(ObjA2); // modify-insert
  aLinkObjectLocator := ObjB.partpartpartof.Locators[ObjB.part.IndexOf(ObjA2)];
  aLinkObjectId := aLinkObjectLocator.BoldObjectId.Clone;
  VerifyState(ObjA2.M_partof, bvpsCurrent);
  Assert(ObjA2.partof.Includes(ObjB));
  Assert(objB.part.Includes(ObjA2));
  VerifyObjectInBlock(UndoHandler.UndoBlocks.CurrentBlock, aLinkObjectId, besNotCreated);
  Assert((Assigned(System.Locators.ObjectById[aLinkObjectId])) or
         (System.Locators.ObjectById[aLinkObjectId].BoldExistenceState = besExisting));
  FSubscriber.VerifySendEvent(beItemAdded, ObjA2.M_partof);
  FSubscriber.VerifySendEvent(beItemAdded, ObjB.M_part);

  CloseAll;
end;

class procedure Tmaan_IndirectMultiLinksModifyTestCase.Suit(
  ASuite: TBoldTestSuite);
begin
  ASuite.AddTest(CreateWithComment('IndirectMulti_ModifyDeleteCurrent', '2.7b Undo block'));
  ASuite.AddTest(CreateWithComment('IndirectMulti_ModifyInsertCurrent', '2.7b Undo block'));
end;

class function Tmaan_IndirectMultiLinksModifyTestCase.Suite: ITestSuite;
begin
  Result := inherited Suite;
  SetCommentForTest(Result, 'IndirectMulti_ModifyDeleteCurrent', '2.7b Undo block');
  SetCommentForTest(Result, 'IndirectMulti_ModifyInsertCurrent', '2.7b Undo block');
end;

{ Tmaan_IndirectMultiLinksUndoTestCase }

procedure Tmaan_IndirectMultiLinksUndoTestCase.IndirectMulti_Undo;
var
  ObjA, ObjB, ObjA2, ObjB2: TSomeClass;
  ObjAId, ObjBId, ObjA2Id, ObjB2Id: TBoldObjectId;
  ALinkObject: Tpartpartof;
  ALinkObjectId: TBoldObjectId;
  procedure Prepare(Insert: Boolean);
  begin
    if not dmUndoRedo.BoldSystemHandle1.Active then
      dmUndoRedo.BoldSystemHandle1.Active := true;
    System.AssertLinkIntegrity;
    FSomeClassList[0].part.Clear;
    System.AssertLinkIntegrity;    
    FsomeClassList[2].partof.Clear;
    dmUndoRedo.BoldSystemHandle1.UpdateDatabase;
    FSomeClassList[1].part.Add(FSomeClassList[0]);
    if not Insert then
      FSomeClassList[1].part.Add(FSomeClassList[2]);
    System.AssertLinkIntegrity;
    FSomeClassList[3].part.Add(FSomeClassList[0]);
    System.AssertLinkIntegrity;
    FSomeClassList[3].part.Add(FSomeClassList[2]);
    System.AssertLinkIntegrity;     
    dmUndoRedo.BoldSystemHandle1.UpdateDatabase;
    RefreshSystem;
    ObjA := FSomeClassList[0];
    ObjAId := ObjA.BoldObjectLocator.BoldObjectId.Clone;
    ObjB := FSomeClassList[1];
    ObjBId := ObjB.BoldObjectLocator.BoldObjectId.Clone;
    ObjA2 := FSomeClassList[2];
    ObjA2Id := ObjA2.BoldObjectLocator.BoldObjectId.Clone;
    ObjB2 := FSomeClassList[3];
    ObjB2Id := ObjB2.BoldObjectLocator.BoldObjectId.Clone;
    ObjB.M_part.EnsureContentsCurrent;
    VerifyState(ObjB.M_part, bvpsCurrent);
  end;

  procedure SetObjectsFromIds;
  begin
    ObjA := System.Locators.ObjectById[ObjAId] as TSomeClass;
    ObjB := System.Locators.ObjectById[ObjBId] as TSomeClass;
    ObjA2 := System.Locators.ObjectById[ObjA2Id] as TSomeClass;
    ObjB2 := System.Locators.ObjectById[ObjB2Id] as TSomeClass;
  end;

  procedure VerifyIsInBlock(Block: TBoldUndoBlock; aId: TboldObjectId);
  var
    Valuepart, Valuepartof: IBoldValue;
  begin
    Block.ValueExists(aId, ObjB.part.BoldRoleRTInfo.OwnIndexInLinkClass, ValuePartOf);
    Block.ValueExists(aId, ObjA.partof.BoldRoleRTInfo.OwnIndexInLinkClass, ValuePart);
    Assert(Assigned(ValuePart) and
    ((ValuePart as IBoldObjectIdREf).Id.AsString = ObjA2.BoldObjectLocator.BoldObjectId.AsString));
    Assert(Assigned(Valuepartof) and
     ((ValuePartof as IBoldObjectIdRef).Id.AsString = ObjB.BoldObjectLocator.BoldObjectId.AsString));
  end;

begin
  {2.7b Undo block}
  {2.7c Redo block}
  SetSimpleConfiguration;
  //Modify insert, ObjB.part Current & ObjA2.partof Invalid
  Prepare(true);
  UndoHandler.SetCheckPoint('UndoIndirectMulti');
  Assert(not objB.part.Includes(ObjA2));
//  Assert(not ObjA2.partof.Includes(ObjB));
  ObjB.part.Add(ObjA2); // modify-insert
  Assert(objB.part.Includes(ObjA2));
  Assert(ObjA2.partof.Includes(ObjB));
  ALinkObject := ObjB.partpartpartof.BoldObjects[ObjB.part.IndexOf(ObjA2)];
  ALinkObjectId := ALinkObject.BoldObjectLocator.BoldObjectId.Clone;
  VerifyObjectInBlock(UndoHandler.UndoBlocks.CurrentBlock, ALinkObjectId, besNotCreated);
  System.AssertLinkIntegrity;
  UndoHandler.UnDoLatest;  //Undo
  System.AssertLinkIntegrity;  
  SetObjectsFromIds;
  VerifyState(ObjB.M_part, bvpsCurrent);
  VerifyState(ObjA2.M_partof, bvpsCurrent);
  Assert(not objB.part.Includes(ObjA2));
  Assert(not ObjA2.partof.Includes(ObjB));
  VerifyIsInBlock(UndoHandler.RedoBlocks.CurrentBlock, ALinkObjectId);
  UndoHandler.Redolatest;  //Redo
  SetObjectsFromIds;
  Assert(ObjA2.partof.Includes(ObjB));
  Assert(objB.part.Includes(ObjA2));
  VerifyObjectInBlock(UndoHandler.UndoBlocks.CurrentBlock, ALinkObjectId, besNotCreated);

  Prepare(true);
  //Modify insert, ObjB.part Current & ObjA2.partof Current
  objB.M_part.EnsureContentsCurrent;
  ObjB2.M_part.EnsureContentsCurrent;
  ObjA.M_partof.EnsureContentsCurrent;
  ObjA2.M_partof.EnsureContentsCurrent;
  UndoHandler.SetCheckPoint('UndoIndirectMulti');
  ObjB.part.Add(ObjA2); // modify-insert
  Assert(ObjA2.partof.Includes(ObjB));
  Assert(objB.part.Includes(ObjA2));
  ALinkObject := ObjB.partpartpartof.BoldObjects[ObjB.part.IndexOf(ObjA2)];
  ALinkObjectId := ALinkObject.BoldObjectLocator.BoldObjectId.Clone;
  VerifyObjectInBlock(UndoHandler.UndoBlocks.CurrentBlock, ALinkObjectId, besNotCreated);
  System.AssertLinkIntegrity;
  UndoHandler.UnDoLatest;  //Undo
  System.AssertLinkIntegrity;  
  SetObjectsFromIds;
  Assert(not ObjA2.partof.Includes(ObjB));
  Assert(not objB.part.Includes(ObjA2));
  VerifyIsInBlock(UndoHandler.RedoBlocks.CurrentBlock, ALinkObjectId);
  UndoHandler.Redolatest;  //Redo
  SetObjectsFromIds;
  Assert(ObjA2.partof.Includes(ObjB));
  Assert(objB.part.Includes(ObjA2));
  VerifyObjectInBlock(UndoHandler.UndoBlocks.CurrentBlock, ALinkObjectId, besNotCreated);
  System.AssertLinkIntegrity;


  //modify delete, ObjB.part Current & ObjA2.partof Invalid
  Prepare(false);
  UndoHandler.SetCheckPoint('UndoIndirectMulti2');
  ALinkObjectId := ObjB.M_partpartpartof.Locators[ObjB.part.IndexOf(ObjA2)].BoldObjectId.Clone;
  ObjB.part.RemoveByIndex(ObjB.part.IndexOf(ObjA2)); // modify-delete
  Assert(not ObjA2.partof.Includes(ObjB));
  Assert(not objB.part.Includes(ObjA2));
  VerifyIsInBlock(UndoHandler.UndoBlocks.CurrentBlock, ALinkObjectId);
  VerifyObjectInBlock(UndoHandler.UndoBlocks.CurrentBlock, ALinkObjectId, besExisting);
  UndoHandler.UnDoLatest; //Undo delete
  SetObjectsFromIds;
  ALinkObject := System.Locators.ObjectById[ALinkObjectId] as Tpartpartof;
  Assert(Assigned(ALinkObject)); // Link object should be recreated
  Assert(ObjA2.partof.Includes(ObjB));
  Assert(objB.part.Includes(ObjA2));
  VerifyObjectInBlock(UndoHandler.RedoBlocks.CurrentBlock, ALinkObjectId, besDeleted);
  System.AssertLinkIntegrity;
  UndoHandler.Redolatest; //Redo delete
  System.AssertLinkIntegrity;
  SetObjectsFromIds;
  Assert(not ObjA2.partof.Includes(ObjB));
  Assert(not objB.part.Includes(ObjA2));
  VerifyIsInBlock(UndoHandler.UndoBlocks.CurrentBlock, ALinkObjectId);
  VerifyObjectInBlock(UndoHandler.UndoBlocks.CurrentBlock, ALinkObjectId, besExisting);
  System.AssertLinkIntegrity;

  //modify delete, ObjB.part Current & ObjA2.partof Current

  Prepare(false);
  objB.M_part.EnsureContentsCurrent;
  ObjB2.M_part.EnsureContentsCurrent;
  ObjA.M_partof.EnsureContentsCurrent;
  ObjA2.M_partof.EnsureContentsCurrent;
  UndoHandler.SetCheckPoint('UndoIndirectMulti2');
  ALinkObjectId := ObjB.M_partpartpartof.Locators[ObjB.part.IndexOf(ObjA2)].BoldObjectId.Clone;
  ObjB.part.RemoveByIndex(ObjB.part.IndexOf(ObjA2)); // modify-delete
  Assert(not ObjA2.partof.Includes(ObjB));
  Assert(not objB.part.Includes(ObjA2));
  VerifyIsInBlock(UndoHandler.UndoBlocks.CurrentBlock, ALinkObjectId);
  VerifyObjectInBlock(UndoHandler.UndoBlocks.CurrentBlock, ALinkObjectId, besExisting);
  UndoHandler.UnDoLatest; //Undo delete
  SetObjectsFromIds;
  ALinkObject := System.Locators.ObjectById[ALinkObjectId] as Tpartpartof;
  Assert(Assigned(ALinkObject)); // Link object should be recreated
  Assert(ObjA2.partof.Includes(ObjB));
  Assert(objB.part.Includes(ObjA2));
  VerifyObjectInBlock(UndoHandler.RedoBlocks.CurrentBlock, ALinkObjectId, besDeleted);
  UndoHandler.Redolatest; //Redo delete
  SetObjectsFromIds;
  Assert(not ObjA2.partof.Includes(ObjB));
  Assert(not objB.part.Includes(ObjA2));
  VerifyIsInBlock(UndoHandler.UndoBlocks.CurrentBlock, ALinkObjectId);
  VerifyObjectInBlock(UndoHandler.UndoBlocks.CurrentBlock, ALinkObjectId, besExisting);
  CloseAll;

end;

class procedure Tmaan_IndirectMultiLinksUndoTestCase.Suit(
  ASuite: TBoldTestSuite);
begin
  ASuite.AddTest(CreateWithComment('IndirectMulti_Undo', '2.7b Undo block & 2.7c Redo block'));
end;

class function Tmaan_IndirectMultiLinksUndoTestCase.Suite: ITestSuite;
begin
  Result := inherited Suite;
  SetCommentForTest(Result, 'IndirectMulti_Undo', '2.7b Undo block & 2.7c Redo block');
end;

initialization
//  TestGlobal.RegisterTestCase(Tmaan_IndirectMultiLinksFetchRefetchTestCase);
  TestGlobal.RegisterTestCase(Tmaan_IndirectMultiLinksModifyTestCase);
  TestGlobal.RegisterTestCase(Tmaan_IndirectMultiLinksUndoTestCase);
finalization
//  TestGlobal.RegisterTestCase(Tmaan_IndirectMultiLinksFetchRefetchTestCase);
  TestGlobal.RegisterTestCase(Tmaan_IndirectMultiLinksModifyTestCase);
  TestGlobal.RegisterTestCase(Tmaan_IndirectMultiLinksUndoTestCase);

end.
