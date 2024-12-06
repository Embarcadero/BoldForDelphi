unit BoldUnitTestReg;

{$INCLUDE Bold.inc}

interface

procedure RegisterBoldAwareGUITestCases;
procedure RegisterClientHandlesComTestCases;
procedure RegisterCommonTestCases;
procedure RegisterFreeStandingValueSpaceTestCases;
procedure RegisterHandlesTestCases;
procedure RegisterLockingSupportTestCases;
procedure RegisterMoldModelTestCases;
procedure RegisterObjectSpaceTestCases;
procedure RegisterPersistenceTestCases;
procedure RegisterPersistenceControllerCoreTestCases;
procedure RegisterPersistenceControllersTestCases;
procedure RegisterProductControlTestCases;
procedure RegisterPropagatorTestCases;
procedure RegisterDatabaseTestCases;
procedure RegisterUMLModelTestCases;
procedure RegisterValueSpaceTestCases;
//procedure RegisterComGUITestCases;  // 14 Jan 2019. Do not register COM components to make TestAttracs compile in Delphi 10.1 Berlin

implementation
uses
  TestFrameWork,
  // all the files should go here
  jano_Multiplicity,
  jehoTestMutableList,
  joho_ModelEvolution {dmModelEvolution: TDataModule},
  joho_ocl2Sql {dm_ocl2sql: TDataModule},
  joho_persistence {dmjohoPersistence: TDataModule},
  ModelEv ,
  ModelEvolTest ,
  ModelEvTemporal ,
  ModEv_Upgrade ,
  ocl2SqlTest ,
  joho_dbevol {dmTestCase: TDataModule},
  maandmSnooper,
  kaladmListener,
//  joho_ComGUI,
  maan_LockManagerComTestCase,
  maan_LockManagerAdminTestCase,
  maan_LockManagerTestCase,
  maan_PropagatorLockingSupport,
  maanBoldSnooperHandle,
  aniv_1,
  aniv_FLS,
 {$IFNDEF OXML}
  aniv_xml,
 {$ENDIF}
  aniv_temporal,
  dmjehoBoldTest,
  dmNullableMomentAttributes,
  jehoMultiLinks,
  joho_OptimistsicLocking,
  maan_FLS,
  maan_Undo,
  maan_UndoRedo,
  maan_UndoRedoIndirectMultiLinks,
  maan_UndoRedoIndirectSingleLinks,
  frha_MD5,
  maanThreadSafeQueue,
  maan_ClientNotifierPoolTestCase,
  maanAdvancedPropagator,
  maanClientHandler,
  maanClientNotificationComps,
  maanClientNotifierHandler,
  maanEnqueuer,
  maanPropagatorCleanUp,
  maanSubscriptionHandler,
  maan_BoldAdvancedPropagatorTestCase,
  BoldComUtils;

procedure RegisterBoldAwareGUITestCases;
begin
end;

procedure RegisterClientHandlesComTestCases;
begin
  //nothing
end;

procedure RegisterCommonTestCases;
begin
  RegisterTests('Common Suite',
                     [ TJano_multiplicity.Suite]);
end;

procedure RegisterFreeStandingValueSpaceTestCases;
begin
  //nothing
end;

procedure RegisterHandlesTestCases;
begin
  RegisterTests('Handles Suite', [TjehoTestMutableList.Suite]);
end;

procedure RegisterLockingSupportTestCases;
begin
  RegisterTests('LockingSupport Suite',
                [Tmaan_LockManagerAdminTestCase.Suite,
                 Tmaan_LockManagerCOMTestCase.Suite,
                 Tmaan_LockManagerTestCase.Suite,
                 Tmaan_LockListTestCase.Suite,
                 Tmaan_LockNameListTestCase.Suite,
                 Tmaan_PropagatorLockingSupport.Suite,
                 TmaanBoldSnooperHandle.Suite]);
end;

procedure RegisterMoldModelTestCases;
begin
  //nothing
end;

procedure RegisterObjectSpaceTestCases;
begin
  RegisterTests('ObjectSpace Suite',
               [TTestSuite.Create('General testcases',
                            [TAniv_1_ListTraverser.Suite,
                             TAniv_Transaction1.Suite,
                             TAniv_AssocWithLinkClass.Suite,
                             TAniv_ParentChild.Suite,
                             TAniv_SingleSingleEmbeddedEmbeddedAssoc.Suite,
                             TAniv_FilePersistenceMapper.Suite,
                             TAniv_AutomaticRemovalOfObjectFromLists.Suite,
                             TAniv_ClassListTests.Suite,
                             TAniv_InvalidateMembers.Suite,
                             TAniv_Discard.Suite,
                             TAniv_Links.Suite,
                             TAniv_ObjectList.Suite,
                             TAniv_PrioQueue.Suite,
                             TAniv_Various.Suite,
                             TAniv_fmDistributable.Suite]),
               TTestSuite.Create('FullLockingSupport testcases',
                            [TAniv_RegionDefinitions.Suite,
                             TAniv_Regions.Suite,
                             TAniv_LockHandler.Suite,
                             TAniv_FLS_Various.Suite,
                             TAniv_LockHolder.Suite,
                             TAniv_LockingHandle.Suite]),
               TAniv_temporal1.Suite,
{$IFNDEF OXML} // TODO update TAniv_XMLStreaming to work with OXML
               TAniv_XMLStreaming.Suite,
{$ENDIF}
               TjehoBoldTest.Suite,
               TNullableMomentAttributes.Suite,
               TjehoBoldTest.Suite,
               Tjoho_OptimisticLocking.Suite,
               TTestSuite.Create('UndoRedo testcases',
                            [Tmaan_SystemPreUpdate.Suite,
                             Tmaan_UndoHandler.Suite,
                             Tmaan_FetchRefetchTestCase.Suite,
                             Tmaan_ModifyTestCase.Suite,
                             Tmaan_UndoTestCase.Suite,
                             Tmaan_IndirectMultiLinksFetchRefetchTestCase.Suite,
                             Tmaan_IndirectMultiLinksModifyTestCase.Suite,
                             Tmaan_IndirectMultiLinksUndoTestCase.Suite,
//                             Tmaan_IndirectSingleFetchRefetchTestCase.Suite,
                             Tmaan_IndirectSingleModifyTestCase.Suite,
                             Tmaan_IndirectSingleUndoTestCase.Suite
                             ])
               ]);
end;

procedure RegisterPersistenceTestCases;
begin
  RegisterTests('Persistence Suite',
      [TModelEvolutionTest.Suite,
       Tjoho_ocl2Sql.Suite,
       Tjoho_o2sNav.Suite,
       Tjoho_o2sList.Suite,
       Tjoho_o2sBool.Suite,
       Tjoho_o2sArith.Suite,
       Tjoho_o2sOther.Suite,
       Tjoho_o2sCombo.Suite,
       Tjoho_o2sBagSet.Suite,
       TTestOCL2SQL.Suite,
       Tjoho_o2sNewStuff.Suite,
       TJoho_EmptyObjects.Suite,
       Tjoho_dbEvol.Suite,
       Tjoho_EvolTest.Suite]);
end;

procedure RegisterPersistenceControllerCoreTestCases;
begin
  //nothing
end;

procedure RegisterPersistenceControllersTestCases;
begin
  RegisterTests('PersistenceControllers Suite',
                [Tmaan_BoldIDAdder.Suite,
                 TTestCaseSnooper_Object.Suite,
                 TTestCaseSnooper_EmbeddedState.Suite,
                 TTestCaseSnooper_NonEmbeddedState.Suite,
                 TTestCaseSnooper_FetchObject.Suite,
                 TTestCaseSnooper_FetchClass.Suite,
                 TKalaListenerTest.Suite]);
end;

procedure RegisterProductControlTestCases;
begin
  RegisterTests('ProductControl Suite', [TFrha_MD5.Suite]);
end;

procedure RegisterPropagatorTestCases;
begin
  RegisterTests('Propagator Suite',
                [TMaan_TestCaseThreadSafeQueue.Suite,
                 Tmaan_ClientNotifierPoolTestCase.Suite,
                 TTestCase_AdvancedPropagator.Suite,
                 TMaan_TestCaseClientHandler.Suite,
                 TTestCase_PriorityListEnlister.Suite,
                 TTestCase_ClientNotifier.Suite,
                 TTestCase_ClientNotifierHandler.Suite,
                 TTestCase_Enqueuer.Suite,
                 TTestCase_BoldSubscriptionList.Suite,
                 TTestCase_PropagatorCleanUp.Suite,
                 TTestCase_Dequeuer.Suite,
                 TTestCase_SubscriptionHandler.Suite,
                 Tmaan_BoldAdvancedPropagatorTestCase.Suite
                 ]);
end;

procedure RegisterDatabaseTestCases;
begin
  //nothing
end;

procedure RegisterUMLModelTestCases;
begin
  //nothing
end;

procedure RegisterValueSpaceTestCases;
begin
  //nothing
end;

//procedure RegisterComGUITestCases;
//begin
//  RegisterTests('ComGUI Suite',
//                [Tjoho_comGui.Suite]);
//end;

initialization

  RegisterBoldAwareGUITestCases;
  RegisterClientHandlesComTestCases;
  RegisterCommonTestCases;
  RegisterFreeStandingValueSpaceTestCases;
  RegisterHandlesTestCases;
  RegisterLockingSupportTestCases;
  RegisterMoldModelTestCases;
  RegisterObjectSpaceTestCases;
  RegisterPersistenceTestCases;
  RegisterPersistenceControllerCoreTestCases;
  RegisterPersistenceControllersTestCases;
  RegisterProductControlTestCases;
  RegisterPropagatorTestCases;
  RegisterDatabaseTestCases;
  RegisterUMLModelTestCases;
  RegisterValueSpaceTestCases;
//  RegisterComGUITestCases;
  BoldInitializeComSecurity(RPC_C_AUTHN_LEVEL_NONE, ilAnonymous);

end.
