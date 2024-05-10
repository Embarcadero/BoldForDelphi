unit aniv_FLS;

{$INCLUDE Bold.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, TestSuite, dmModel1,
  BoldSubscription, BoldHandles, BoldSystemHandle, BoldComServerHandles,
  BoldHandle, BoldServerHandles, BoldSOAPServerPersistenceHandles, BoldLockHolder,
  BoldListenerHandle, BoldLockHandler, BoldAbstractDequeuer,
  BoldExternalObjectSpaceEventHandler, BoldPersistenceHandle,
  BoldPersistenceHandleDB, ActnList,
  BoldHandleAction, BoldActions, BoldDBActions,
  BoldAbstractLockManagerHandle, BoldLockManagerHandleCom,
  BoldDefaultTaggedValues,
  BoldAbstractModel,
  BoldIndexableList,
  BoldClientHandles, BoldComClientHandles, BoldLockingHandles,
  TestFrameWork,
  BoldAbstractPropagatorHandle,
  BoldPropagatorHandleCOM,
  BoldListenerThread, DB, BoldAbstractDatabaseAdapter,
   BoldAbstractPersistenceHandleDB, DBAccess, Uni, BoldDatabaseAdapterUniDAC;

type
  TdmAnivFLS = class(TDataModule)
    BoldSystemHandle1: TBoldSystemHandle;
    BoldListenerHandle1: TBoldListenerHandle;
    BoldComConnectionHandle1: TBoldComConnectionHandle;
    BoldLockManagerHandleCom1: TBoldLockManagerHandleCom;
    BoldLockingHandle1: TBoldLockingHandle;
    BoldPropagatorHandleCOM1: TBoldPropagatorHandleCOM;
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
    BoldDatabaseAdapterUniDAC1: TBoldDatabaseAdapterUniDAC;
    UniConnection1: TUniConnection;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TLocalTestCase = class(TBoldTestCase)
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;


  TAniv_RegionDefinitions = class(TLocalTestCase)
  protected
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
  published
    procedure Initializing;
    procedure SetForceOtherEnd;
  end;

  TAniv_Regions = class(TLocalTestCase)
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
  published
    procedure Factory;
    procedure SubregionExpansion;
    procedure RegionsForElementExpansion;
    procedure Subscription;
    procedure ParentExpansion;
  end;

 {$IFNDEF BOLD_NO_QUERIES}
  TDummyLockHolder = class;
   
  TAniv_LockHandler = class(TLocalTestCase)
  private
    FLockHandler: TBoldPessimisticLockHandler;
    LockHolder: TDummyLockHolder;
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure RequiredRegions;
    procedure ReleaseUnNeededRegions;
    procedure DeletingObjects;
    procedure ParentRegions;
  end;

  TAniv_FLS_Various = class(TLocalTestCase)
  private
    LockHolder: TDummyLockHolder;
    LockHandler: TBoldPessimisticLockHandler;
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure EnsureLocks;
  end;

  TAniv_LockHolder = class(TLocalTestCase)
  private
      LockHolder: TBoldLockHolder;
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGetLocks;
  end;

  TAniv_LockingHandle = class(TLocalTestCase)
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
  published
    procedure Connection;
  end;

  TDummyLockHolder = class(TBoldLockHolder)
  private
    FInConflict: Boolean;
    FConflictEvents: TStringList;
    fEnsureLocksResult: Boolean;
    listenerThread: TBoldListenerThread;
  public
    constructor Create;
    destructor Destroy; override;
    function Lock(Shared: TBoldLockList; Exclusive: TBoldLockList; HeldLocks, ClientsHoldingRequestedLocks: TStringList): Boolean; override;
    procedure Release(Locks: TBoldLockList); override;
    procedure GetPropagationEvents(EventList: TStringList); override;
    function EnsureLocks: Boolean; override;
  end;
 {$ENDIF}

var
  dmAnivFLS: TdmAnivFLS;

implementation

{$R *.DFM}

uses
  BoldRegionDefinitions,
  BoldLockRegions,
  BoldDefs,
  BoldSystem,
  TestModel1,
  BoldSystemRT,
  BoldLockManagerCOM,
  BoldIndex,
  BoldGuard
  ;

function HasDependentSubregion(Reg, SubRegion: TBoldRegion): Boolean;
var
  i: integer;
  SubRegions: TBoldRegionList;
begin
  result := false;
  SubRegions := TBoldRegionList.create;
  Reg.FillDependentSubregions(SubRegions);
  for i := 0 to Subregions.Count-1 do
    result := result or (Subregions[i] = SubRegion);
  subregions.free;
end;


{ TAniv_RegionDefinitions }

procedure SetRegionDefinitions(S: String);
begin
  EnsureDM_Model;
  if assigned(dmAnivFLS) then
  begin
    dmAnivFLS.BoldSystemHandle1.Active := false;
  end;
  dm_Model1.BoldModel1.MoldModel.BoldTVByName[TAG_REGIONDEFINITIONS] := s;
  dm_Model1.BoldModel1.SendEvent(dm_Model1.BoldModel1, beModelChanged);
end;

procedure EnsureDM;
begin
  Ensuredm_Model;
  if not assigned(dmAnivFLS) then
  begin
    Application.Initialize;
    dmAnivFLS := TdmAnivFLS.Create(Application);
    dmAnivFLS.BoldDatabaseAdapterUniDac1.CreateDatabase;
    dmAnivFLS.BoldPersistenceHandleDB1.CreateDataBaseSchema;
    dmAnivFLS.BoldSystemHandle1.Active := True;
  end else
  begin
    if Assigned(dmAnivFLS.BoldSystemHandle1.System) then
      dmAnivFLS.BoldSystemHandle1.System.Discard;
    dmAnivFLS.BoldSystemHandle1.Active := False;
    // freeing the modelDataModule can cause the systemhandle to lose the connection to the STIHandle
    dmAnivFLS.BoldSystemHandle1.SystemTypeInfoHandle := dm_Model1.BoldSystemTypeInfoHandle1;
    dmAnivFLS.BoldPersistenceHandleDB1.BoldModel := dm_Model1.BoldModel1;
    dmAnivFLS.BoldSystemHandle1.Active := True;
  end;
end;


procedure FreeDM;
begin
  FreeAndNil(dmAnivFLS);
end;

procedure TAniv_RegionDefinitions.Initializing;
var
  Defs: TBoldRegionDefinitions;
  Core1: TBoldRegionCoreDefinition;
  Def1: TBoldConcreteRegionDefinition;
  Elem1: TBoldRegionElementInclusion;
  Def2: TBoldConcreteRegionDefinition;
  Elem2: TBoldRegionElementInclusion;
  Sub1: TBoldSubregionReference;
  aClassType1, aClassType2: TBoldClassTypeInfo;
  aMemberRT1, aMemberRT2: TBoldMemberRTInfo;
begin

  EnsureDM;
  Defs := dm_Model1.BoldSystemTypeInfoHandle1.RegionDefinitions;
  Defs.Clear;
  Core1 := TBoldRegionCoreDefinition.Create(Defs, 'r1');
  aClassType1 := dm_Model1.BoldSystemTypeInfoHandle1.StaticSystemTypeInfo.ClassTypeInfoByExpressionName['Song'];
  Def1 := TBoldConcreteRegionDefinition.Create(Core1, aClassType1);
  aMemberRT1 := aClassType1.MemberRTInfoByExpressionName['Title'];
  Elem1 := TBoldRegionElementInclusion.Create(def1, aMemberRT1);

  aClassType2 := dm_Model1.BoldSystemTypeInfoHandle1.StaticSystemTypeInfo.ClassTypeInfoByExpressionName['Hitlist'];
  Def2 := TBoldConcreteRegionDefinition.Create(Core1, aClassType2);
  aMemberRT2 := aClassType2.MemberRTInfoByExpressionName['Name'];
  Elem2 := TBoldRegionElementInclusion.Create(Def2, aMemberRT2);

  Sub1 := TBoldSubregionReference.Create(def1, Core1, aClassType1.MemberRTInfoByExpressionName['HitList'] as TBoldRoleRTInfo, true);

  assert(Defs.RegionInclusionsByMember[aMemberRT1].Count = 1, '');
  assert(Defs.RegionInclusionsByMember[aMemberRT1].Items[0] = Elem1, '');
  assert(Defs.RegionInclusionsByMember[aMemberRT2].Count = 1, '');
  assert(Defs.RegionInclusionsByMember[aMemberRT2].Items[0] = Elem2, '');

  assert(Core1.Owner = Defs, '');
  assert(Core1.ConcreteDefinitions.Count = 2, '');
  assert(Core1.ConcreteDefinitions.indexof(Def1) <> -1, '');
  assert(Core1.ConcreteDefinitions.indexof(Def2) <> -1, '');
  assert(Core1.Name = 'r1', '');
  assert(Core1.UsedBy.Count = 1, '');
  assert(Core1.UsedBy[0] = sub1, '');

  assert(Def1.CoreDefinition = Core1, '');
  assert(Def1.RootClass = aClassType1, '');
  assert(Def1.Elements.Count = 1, '');
  assert(Def1.Elements[0] = elem1, '');
  assert(Def1.Subregions.Count = 1, '');
  assert(Def1.Subregions[0] = Sub1, '');

  assert(Def2.CoreDefinition = Core1, '');
  assert(Def2.RootClass = aClassType2, '');
  assert(Def2.Elements.Count = 1, '');
  assert(Def2.Elements[0] = elem2, '');
  assert(Def2.Subregions.Count = 0, '');
  assert(Def2.ParentRegions.Count = 1, '');
  assert(Def2.ParentRegions[0] = Sub1, '');

  assert(Elem1.Region = Def1, '');
  assert(Elem1.Member = aMemberRT1, '');

  assert(Elem2.Region = Def2, '');
  assert(Elem2.Member = aMemberRT2, '');

  assert(Sub1.ParentRegion = Def1, '');
  assert(Sub1.SubregionCoreDefinition = Core1, '');
end;

procedure TAniv_RegionDefinitions.SetForceOtherEnd;
var
  aClassType: TBoldClassTypeInfo;
  ChildRole: TBoldRoleRTInfo;
  ParentRole: TBoldRoleRTInfo;
  PartRole: TBoldRoleRTInfo;

  Defs: TBoldRegionDefinitions;
  Core1: TBoldRegionCoreDefinition;
  Def1: TBoldConcreteRegionDefinition;
  G: IBoldGuard;
begin
  G := TBoldGuard.Create(Defs);
  FreeAndNil(dm_Model1);
  Ensuredm_Model;

  aClassType := dm_Model1.BoldSystemTypeInfoHandle1.StaticSystemTypeInfo.ClassTypeInfoByExpressionName['ClassA'];
  ChildRole := aClassType.MemberRTInfoByExpressionName['child'] as TBoldRoleRTInfo;
  ParentRole := aClassType.MemberRTInfoByExpressionName['parent'] as TBoldRoleRTInfo;
  PartRole := aClassType.MemberRTInfoByExpressionName['part'] as TBoldRoleRTInfo;

  assert(not ParentRole.ForceOtherEnd, 'precond');
  assert(PartRole.RoleRTInfoOfOtherEnd.RoleType = rtInnerLinkRole, 'precond');
  assert(not PartRole.RoleRTInfoOfOtherEnd.ForceOtherEnd, 'precond');

  Defs := TBoldRegionDefinitions.Create;
  Core1 := TBoldRegionCoreDefinition.Create(Defs, 'r1');
  Def1 := TBoldConcreteRegionDefinition.Create(Core1, aClassType);

  TBoldRegionElementInclusion.Create(Def1, ChildRole);
  assert(ParentRole.ForceOtherEnd, '');

  TBoldRegionElementInclusion.Create(Def1, PartRole);
  assert(PartRole.RoleRTInfoOfOtherEnd.ForceOtherEnd, '');

end;

procedure TLocalTestCase.SetUp;
begin
  inherited;
  Ensuredm_Model;
end;

procedure TLocalTestCase.TearDown;
begin
  if ASsigned(dmAnivFLS) and assigned(dmAnivFLS.BoldSystemHandle1.System) then
    dmAnivFLS.BoldSystemHandle1.System.Discard;
  FreeAndNil(dmAnivFLS);
end;

class procedure TAniv_RegionDefinitions.Suit(ASuite: TBoldTestSuite);
begin
  ASuite.AddTest(CreateWithComment('SetForceOtherEnd'));
  ASuite.AddTest(CreateWithComment('Initializing'));
end;

class function TAniv_RegionDefinitions.Suite: ITestSuite;
begin
  Result := inherited Suite;
  SetCommentForTest(REsult, 'SetForceOtherEnd', '');
  SetCommentForTest(REsult, 'Initializing', '');
end;

{ TAniv_Regions }

procedure TAniv_Regions.Factory;
var
  Defs: TBoldRegionDefinitions;
  Core1: TBoldRegionCoreDefinition;
  Def1: TBoldConcreteRegionDefinition;
  aClassType: TBoldClassTypeInfo;

  Obj1, Obj2: TBoldObject;

  reg1, reg2, reg3: TBoldRegion;

  aFactory: TBoldRegionFactory;
  G: IBoldGuard;
begin
//  SetRegionDefinitions('R1[Song]:');
  G := TBoldGuard.Create(aFactory);
  EnsureDM;

  Defs := dm_Model1.BoldSystemTypeInfoHandle1.RegionDefinitions;
  Defs.Clear;
  Core1 := TBoldRegionCoreDefinition.Create(Defs, 'r1');
  aClassType := dm_Model1.BoldSystemTypeInfoHandle1.StaticSystemTypeInfo.ClassTypeInfoByExpressionName['Song'];
  Def1 := TBoldConcreteRegionDefinition.Create(Core1, aClassType);

  Obj1 := dmAnivFLS.BoldSystemHandle1.System.CreateNewObjectByExpressionName('Song');
  Obj2 := dmAnivFLS.BoldSystemHandle1.System.CreateNewObjectByExpressionName('Song');

  aFactory := TBoldRegionFactory.Create(Defs); //dmAnivFLS.BoldSystemHandle1.RegionFactory;

  reg1 := aFactory.GetRegion(Core1, Obj1.BoldObjectLocator);
  assert(reg1.Definition.CoreDefinition = Core1, '');
  assert(reg1.Root = Obj1);
  reg2 := aFactory.GetRegion(Core1, Obj2.BoldObjectLocator);
  assert(reg1 <> reg2, '');
  reg3 := aFactory.GetRegion(Core1, Obj1.BoldObjectLocator);
  assert(reg1 = reg3, '');
end;

procedure TAniv_Regions.ParentExpansion;
var
  Defs: TBoldRegionDefinitions;
  Core1: TBoldRegionCoreDefinition;
  Def1: TBoldConcreteRegionDefinition;
  aClassType1, aClassType2: TBoldClassTypeInfo;
  Elem1: TBoldRegionElementInclusion;
  Def2: TBoldConcreteRegionDefinition;
  Elem2: TBoldRegionElementInclusion;
  Sub1: TBoldSubregionReference;
  aMemberRT1, aMemberRT2: TBoldMemberRTInfo;
  aRoleRT: TBoldRoleRTInfo;

  Comp1, comp2: TCompositeItem;
  Single1: TSingleItem;

  aFactory: TBoldRegionFactory;
  reg1: TBoldRegion;
  G: IBoldGuard;

  regions: TBoldRegionLookup;
  RegionExpander: TBoldRegionExpander;
  Traverser: TBoldIndexableListTraverser;
  Comp1Found, Comp2Found: Boolean;
  TempRegion: TBoldRegion;

begin
  G := TBoldGuard.Create(aFactory);

//  SetRegionDefinitions(
//    'R1[CompositeItem]: Name  | R1[item]' + BOLDCRLF +
//    'R1[SingleItem   ]: Name'                         );
  EnsureDM;

  Defs := dm_Model1.BoldSystemTypeInfoHandle1.RegionDefinitions;
  Defs.Clear;
  Core1 := TBoldRegionCoreDefinition.Create(Defs, 'r1');
  aClassType1 := dm_Model1.BoldSystemTypeInfoHandle1.StaticSystemTypeInfo.ClassTypeInfoByExpressionName['CompositeItem'];
  Def1 := TBoldConcreteRegionDefinition.Create(Core1, aClassType1);
  aMemberRT1 := aClassType1.MemberRTInfoByExpressionName['Name'];
  Elem1 := TBoldRegionElementInclusion.Create(Def1, aMemberRT1);


  aClassType2 := dm_Model1.BoldSystemTypeInfoHandle1.StaticSystemTypeInfo.ClassTypeInfoByExpressionName['SingleItem'];
  Def2 := TBoldConcreteRegionDefinition.Create(Core1, aClassType2);
  aMemberRT2 := aClassType2.MemberRTInfoByExpressionName['Name'];
  Elem2 := TBoldRegionElementInclusion.Create(Def2, aMemberRT2);
  aRoleRT := aClassType1.MemberRTInfoByExpressionName['Item'] as TBoldRoleRTInfo;
  Sub1 := TBoldSubregionReference.Create(Def1, Core1, aRoleRT, true);

  Comp1 := TCompositeItem.Create(dmAnivFLS.BoldSystemHandle1.System);
  Comp2 := TCompositeItem.Create(dmAnivFLS.BoldSystemHandle1.System);
  Single1 := TSingleItem.Create(dmAnivFLS.BoldSystemHandle1.System);

  Comp1.Item.Add(Comp2);
  Comp2.Item.Add(Single1);

  aFactory :=  TBoldRegionFactory.Create(Defs); //dmAnivFLS.BoldSystemHandle1.RegionFactory;
  Reg1 := aFactory.GetRegion(Core1, Single1.BoldObjectLocator);

  Regions := TBoldRegionLookup.Create;
  Regions.Add(Reg1);

  RegionExpander := TBoldRegionExpander.Create;
  RegionExpander.ExpandRegionEnclosure(Regions);

  Comp1Found := false;
  Comp2Found := false;
  Traverser := Regions.CreateTraverser;
  while Traverser.MoveNext do
  begin
    TempRegion := Traverser.Item as TBoldRegion;
    if TempRegion.Root = Comp1 then
      Comp1Found := true;
    if TempRegion.Root = Comp2 then
      Comp2Found := true;
  end;
  Traverser.Free;
  assert(Comp1Found, 'Comp1 is not included in the regionenclosure (should have been in a parent region to Reg1');
  assert(Comp2Found, 'Comp2 is not included in the regionenclosure (should have been in a parent region to Reg1');
  assert(regions.count = 3);

  RegionExpander.Free;
  Regions.Free;

  dmAnivFLS.BoldSystemHandle1.System.Discard;
end;

procedure TAniv_Regions.RegionsForElementExpansion;
var
  Defs: TBoldRegionDefinitions;
  Core1: TBoldRegionCoreDefinition;
  Def1: TBoldConcreteRegionDefinition;
  aClassType1: TBoldClassTypeInfo;
  Elem1: TBoldRegionElementInclusion;
  aMemberRT1: TBoldMemberRTInfo;
//  aRoleRT1, aRoleRT2: TBoldRoleRTInfo;

  aFactory: TBoldRegionFactory;
  aRegionList: TBoldRegionList;
  aRegion1, aRegion2, aRegion3: TBoldRegion;

  ClassA1, ClassA2, ClassA3, ClassA4: TClassA;
  G: IBoldGuard;
begin
  G := TBoldGuard.Create(aRegionList);
  EnsureDM;

  Defs := dm_Model1.BoldSystemTypeInfoHandle1.RegionDefinitions;
  Defs.Clear;
  aClassType1 := dm_Model1.BoldSystemTypeInfoHandle1.StaticSystemTypeInfo.ClassTypeInfoByExpressionName['ClassA'];
  aMemberRT1 := aClassType1.MemberRTInfoByExpressionName['aString'];
//  aRoleRT1 := aClassType1.MemberRTInfoByExpressionName['part'] as TBoldRoleRTInfo;
//  aRoleRT2 := aClassType1.MemberRTInfoByExpressionName['next'] as TBoldRoleRTInfo;

  Core1 := TBoldRegionCoreDefinition.Create(Defs, 'r1');
  Def1 := TBoldConcreteRegionDefinition.Create(Core1, aClassType1);
  Elem1 := TBoldRegionElementInclusion.Create(Def1, aMemberRT1);
//  Elem2 := TBoldRegionElementInclusion.Create(Def1, aMemberRT1);
//  Elem2.Navigation := aRoleRT1;

{ // two step navigation is not supported anymore
  Core2 := TBoldRegionCoreDefinition.Create(Defs, 'r2');
  Def2 := TBoldConcreteRegionDefinition.Create(Core2, aClassType1);
  Elem3 := TBoldRegionElementInclusion.Create(Def2, aMemberRT1);
  Elem3.Navigation.Add(aRoleRT2);
  Elem3.Navigation.Add(aRoleRT2);
}

  ClassA1 := TClassA.Create(dmAnivFLS.BoldSystemHandle1.System);
  ClassA2 := TClassA.Create(dmAnivFLS.BoldSystemHandle1.System);
  ClassA3 := TClassA.Create(dmAnivFLS.BoldSystemHandle1.System);
  ClassA4 := TClassA.Create(dmAnivFLS.BoldSystemHandle1.System);

{  ClassA1.partof.Add(ClassA2);
  ClassA1.partof.Add(ClassA3);
  ClassA1.previous := ClassA2;
  ClassA2.previous := ClassA4;
}
  aRegionList := TBoldRegionList.Create;
  aFactory := dmAnivFLS.BoldSystemHandle1.RegionFactory;
  aFactory.GetRegionsForElement(ClassA1.M_aString, aRegionList);

  aRegion1 := aFactory.GetRegion(Core1, ClassA1.BoldObjectLocator);
  aRegion2 := aFactory.GetRegion(Core1, ClassA2.BoldObjectLocator);
  aRegion3 := aFactory.GetRegion(Core1, ClassA3.BoldObjectLocator);

// aRegion4 := aFactory.GetRegion(Core2, ClassA4);

  assert(aRegionList.Count = 1, '');
  assert(aRegionList.IndexOf(aRegion1) <> -1, '');
  assert(aRegionList.IndexOf(aRegion2) = -1, '');
  assert(aRegionList.IndexOf(aRegion3) = -1, '');
//  assert(aRegionList.IndexOf(aRegion4) <> -1, '');

  assert(aRegion1 <> aRegion2, '');
  assert(aRegion1 <> aRegion3, '');
  assert(aRegion2 <> aRegion3, '');

//  assert(aRegion1 <> aRegion4, '');
//  assert(aRegion2 <> aRegion4, '');
//  assert(aRegion3 <> aRegion4, '');

  dmAnivFLS.BoldSystemHandle1.System.Discard;
end;

procedure TAniv_Regions.SubregionExpansion;
var
  Defs: TBoldRegionDefinitions;
  Core1: TBoldRegionCoreDefinition;
  Def1: TBoldConcreteRegionDefinition;
  aClassType1, aClassType2: TBoldClassTypeInfo;
  Elem1: TBoldRegionElementInclusion;
  Def2: TBoldConcreteRegionDefinition;
  Elem2: TBoldRegionElementInclusion;
  Sub1: TBoldSubregionReference;
  aMemberRT1, aMemberRT2: TBoldMemberRTInfo;
  aRoleRT: TBoldRoleRTInfo;

  Comp1, comp2: TCompositeItem;
  Single1: TSingleItem;

  aFactory: TBoldRegionFactory;
  reg1: TBoldRegion;
  SubRegions, SubSubRegions, subsubsubRegions: TBoldRegionList;
begin
  EnsureDM;

  Defs := dm_Model1.BoldSystemTypeInfoHandle1.RegionDefinitions;
  Defs.Clear;
  Core1 := TBoldRegionCoreDefinition.Create(Defs, 'r1');
  aClassType1 := dm_Model1.BoldSystemTypeInfoHandle1.StaticSystemTypeInfo.ClassTypeInfoByExpressionName['CompositeItem'];
  Def1 := TBoldConcreteRegionDefinition.Create(Core1, aClassType1);
  aMemberRT1 := aClassType1.MemberRTInfoByExpressionName['Name'];
  Elem1 := TBoldRegionElementInclusion.Create(Def1, aMemberRT1);
  aClassType2 := dm_Model1.BoldSystemTypeInfoHandle1.StaticSystemTypeInfo.ClassTypeInfoByExpressionName['SingleItem'];
  Def2 := TBoldConcreteRegionDefinition.Create(Core1, aClassType2);
  aMemberRT2 := aClassType2.MemberRTInfoByExpressionName['Name'];
  Elem2 := TBoldRegionElementInclusion.Create(Def2, aMemberRT2);
  aRoleRT := aClassType1.MemberRTInfoByExpressionName['Item'] as TBoldRoleRTInfo;
  Sub1 := TBoldSubregionReference.Create(Def1, Core1, aRoleRT, true);

  Comp1 := TCompositeItem.Create(dmAnivFLS.BoldSystemHandle1.System);
  Comp2 := TCompositeItem.Create(dmAnivFLS.BoldSystemHandle1.System);
  Single1 := TSingleItem.Create(dmAnivFLS.BoldSystemHandle1.System);

  Comp1.Item.Add(Comp2);
  Comp2.Item.Add(Single1);

  aFactory := dmAnivFLS.BoldSystemHandle1.RegionFactory;
  Reg1 := aFactory.GetRegion(Core1, Comp1.BoldObjectLocator);


  SubRegions := TBoldRegionList.Create;
  SubSubRegions := TBoldRegionList.Create;
  SubSubSubRegions := TBoldRegionList.Create;

  Reg1.FillDependentSubregions(SubRegions);
  assert(Subregions.Count = 1, '');
  assert(Subregions[0].Root = Comp2, '');

  Subregions[0].FillDependentSubregions(SubSubREgions);
  assert(SubSubregions.Count = 1, '');
  assert(SubSubregions[0].Root = Single1, '');

  SubSubRegions[0].FillDependentSubregions(SubSubSubREgions);
  assert(SubSubSubregions.Count = 0, '');
  SubRegions.Free;
  SubSubRegions.Free;
  SubSubSubRegions.Free;

  dmAnivFLS.BoldSystemHandle1.System.Discard;
end;

procedure TAniv_Regions.Subscription;
var
  Defs: TBoldRegionDefinitions;
  Core1: TBoldRegionCoreDefinition;
  Def1: TBoldConcreteRegionDefinition;
  aClassType1, aClassType2: TBoldClassTypeInfo;
  Elem1: TBoldRegionElementInclusion;
  Def2: TBoldConcreteRegionDefinition;
  Elem2: TBoldRegionElementInclusion;
  Sub1: TBoldSubregionReference;
  aMemberRT1, aMemberRT2: TBoldMemberRTInfo;
  aRoleRT: TBoldRoleRTInfo;

  Comp1, comp2, Comp3: TCompositeItem;
  Single1, Single2: TSingleItem;

  aFactory: TBoldRegionFactory;
  reg1, reg2, reg3, reg4, reg5: TBoldRegion;
  SubRegions: TBoldRegionList;
begin
  EnsureDM;

  Defs := dm_Model1.BoldSystemTypeInfoHandle1.RegionDefinitions;
  Defs.Clear;
  Core1 := TBoldRegionCoreDefinition.Create(Defs, 'r1');
  aClassType1 := dm_Model1.BoldSystemTypeInfoHandle1.StaticSystemTypeInfo.ClassTypeInfoByExpressionName['CompositeItem'];
  Def1 := TBoldConcreteRegionDefinition.Create(Core1, aClassType1);
  aMemberRT1 := aClassType1.MemberRTInfoByExpressionName['Name'];
  Elem1 := TBoldRegionElementInclusion.Create(Def1, aMemberRT1);
  aClassType2 := dm_Model1.BoldSystemTypeInfoHandle1.StaticSystemTypeInfo.ClassTypeInfoByExpressionName['SingleItem'];
  Def2 := TBoldConcreteRegionDefinition.Create(Core1, aClassType2);
  aMemberRT2 := aClassType2.MemberRTInfoByExpressionName['Name'];
  Elem2 := TBoldRegionElementInclusion.Create(Def2, aMemberRT2);
  aRoleRT := aClassType1.MemberRTInfoByExpressionName['Item'] as TBoldRoleRTInfo;
  Sub1 := TBoldSubregionReference.Create(Def1, Core1, aRoleRT, true);
  Comp1 := TCompositeItem.Create(dmAnivFLS.BoldSystemHandle1.System);
  Comp2 := TCompositeItem.Create(dmAnivFLS.BoldSystemHandle1.System);
  Comp3 := TCompositeItem.Create(dmAnivFLS.BoldSystemHandle1.System);
  Single1 := TSingleItem.Create(dmAnivFLS.BoldSystemHandle1.System);
  Single2 := TSingleItem.Create(dmAnivFLS.BoldSystemHandle1.System);

  Comp1.Item.Add(Comp2);
  Comp1.Item.Add(Single1);

  aFactory := dmAnivFLS.BoldSystemHandle1.RegionFactory;
  Reg1 := aFactory.GetRegion(Core1, Comp1.BoldObjectLocator);
  Reg2 := aFactory.GetRegion(Core1, Comp2.BoldObjectLocator);
  Reg3 := aFactory.GetRegion(Core1, Comp3.BoldObjectLocator);
  Reg4 := aFactory.GetRegion(Core1, Single1.BoldObjectLocator);
  Reg5 := aFactory.GetRegion(Core1, Single2.BoldObjectLocator);

  SubRegions := TBoldRegionList.Create;
  Reg1.FillDependentSubregions(SubRegions);

  assert(SubRegions.Count = 2, '');
  assert(HasDependentSubregion(reg1, reg2), '');
  assert(HasDependentSubregion(reg1, reg4), '');
  SubRegions.Clear;

  Comp1.Item.remove(Single1);
  Comp1.Item.Add(Comp3);
  Comp1.Item.Add(Single2);

  Reg1.FillDependentSubregions(SubRegions);
  assert(Subregions.Count = 3, '');
  assert(HasDependentSubregion(reg1, reg2), '');
  assert(HasDependentSubregion(reg1, reg3), '');
  assert(HasDependentSubregion(reg1, reg5), '');
  SubRegions.Free;

  dmAnivFLS.BoldSystemHandle1.System.Discard;
end;

class procedure TAniv_Regions.Suit(ASuite: TBoldTestSuite);
begin
  ASuite.AddTest(CreateWithComment('Factory'));
  ASuite.AddTest(CreateWithComment('SubregionExpansion'));
  ASuite.AddTest(CreateWithComment('RegionsForElementExpansion'));
  ASuite.AddTest(CreateWithComment('Subscription'));
  ASuite.AddTest(CreateWithComment('ParentExpansion'));
end;

class function TAniv_Regions.Suite: ITestSuite;
begin
  Result := inherited Suite;
  SetCommentForTest(Result, 'Factory', '');
  SetCommentForTest(Result, 'SubregionExpansion', '');
  SetCommentForTest(Result, 'RegionsForElementExpansion', '');
  SetCommentForTest(Result, 'Subscription', '');
  SetCommentForTest(Result, 'ParentExpansion', '');
end;

{$IFNDEF BOLD_NO_QUERIES}
{ TAniv_LockHandler }

procedure TAniv_LockHandler.DeletingObjects;
var
  Defs: TBoldRegionDefinitions;
  Core1: TBoldRegionCoreDefinition;
  Def1: TBoldConcreteRegionDefinition;
  aClassType1: TBoldClassTypeInfo;
  Elem1: TBoldRegionElementInclusion;
  aMemberRT1: TBoldMemberRTInfo;
  Factory: TBoldRegionFactory;

  Obj1: TClassA;
begin
  Defs := dm_Model1.BoldSystemTypeInfoHandle1.RegionDefinitions;
  Defs.Clear;
  Core1 := TBoldRegionCoreDefinition.Create(Defs, 'r1');
  aClassType1 := dm_Model1.BoldSystemTypeInfoHandle1.StaticSystemTypeInfo.ClassTypeInfoByExpressionName['ClassA'];
  Def1 := TBoldConcreteRegionDefinition.Create(Core1, aClassType1);
  aMemberRT1 := aClassType1.MemberRTInfoByExpressionName['aString'];
  Elem1 := TBoldRegionElementInclusion.Create(Def1, aMemberRT1);

  Factory := dmAnivFLS.BoldSystemHandle1.RegionFactory;

  Obj1 := TClassA.Create(dmAnivFLS.BoldSystemHandle1.System);
  dmAnivFLS.BoldSystemHandle1.UpdateDatabase;

  dmAnivFLS.BoldSystemHandle1.System.StartTransaction;
  try
    assert(FLockHandler.RequiredExclusive.Count = 0, 'precondition to testcase');
    Obj1.Delete;
    assert(FLockHandler.RequiredExclusive.Count = 1, '');
    assert(FLockHandler.RequiredExclusive[0].Root = Obj1);
  finally
    dmAnivFLS.BoldSystemHandle1.System.CommitTransaction;
    dmAnivFLS.BoldSystemHandle1.System.Discard;
  end;  
//  FLockHandler.ReleaseUnNeededRegions;
//  assert(FLockHandler.RequiredExclusive.Count = 0, 'precondition to testcase');
end;

procedure TAniv_LockHandler.ParentRegions;
var
  Factory: TBoldRegionFactory;
  Defs: TBoldRegionDefinitions;
  Core1: TBoldRegionCoreDefinition;
  aClassType1: TBoldClassTypeInfo;
  Def1: TBoldConcreteRegionDefinition;
  aMemberRT1: TBoldMemberRTInfo;
  Elem1: TBoldRegionElementInclusion;
  Sub1: TBoldSubregionReference;
  aRoleRT: TBoldRoleRTInfo;

  Obj1, Obj2, Obj3, Obj4, Obj5: TClassA;
begin
  Defs := dm_Model1.BoldSystemTypeInfoHandle1.RegionDefinitions;
  Defs.Clear;
  Core1 := TBoldRegionCoreDefinition.Create(Defs, 'r1');
  aClassType1 := dm_Model1.BoldSystemTypeInfoHandle1.StaticSystemTypeInfo.ClassTypeInfoByExpressionName['ClassA'];
  Def1 := TBoldConcreteRegionDefinition.Create(Core1, aClassType1);
  aMemberRT1 := aClassType1.MemberRTInfoByExpressionName['aString'];
  Elem1 := TBoldRegionElementInclusion.Create(Def1, aMemberRT1);
  aRoleRT := aClassType1.MemberRTInfoByExpressionName['part'] as TBoldRoleRTInfo;
  Sub1 := TBoldSubregionReference.Create(Def1, Core1, aRoleRT, true);
  Factory := dmAnivFLS.BoldSystemHandle1.RegionFactory;

  Obj1 := TClassA.Create(dmAnivFLS.BoldSystemHandle1.System);
  Obj2 := TClassA.Create(dmAnivFLS.BoldSystemHandle1.System);
  Obj3 := TClassA.Create(dmAnivFLS.BoldSystemHandle1.System);
  Obj4 := TClassA.Create(dmAnivFLS.BoldSystemHandle1.System);
  Obj5 := TClassA.Create(dmAnivFLS.BoldSystemHandle1.System);

  Obj1.part.Add(Obj2);
  Obj1.part.Add(Obj3);
  Obj2.part.Add(Obj4);
  Obj5.part.Add(Obj3);

  dmAnivFLS.BoldSystemHandle1.UpdateDatabase;

  dmAnivFLS.BoldSystemHandle1.System.StartTransaction;
  try
    assert(FLockHandler.RequiredExclusive.Count = 0, 'precond');
    assert(FLockHandler.RequiredShared.Count = 0, 'precond');
    Obj2.aString := 'modified value';
    assert(FLockHandler.RequiredExclusive.Count = 1, Format('%d, sould be %d', [FLockHandler.RequiredExclusive.Count, 1]));
    assert(FLockHandler.RequiredExclusive[0].Root = Obj2, '');
  //  assert(FLockHandler.RequiredShared.Count = 2, '');
    assert(FLockHandler.RequiredShared.IndexOf(Factory.GetRegion(Core1, Obj1.BoldObjectLocator)) <> -1, '');
    assert(FLockHandler.RequiredShared.IndexOf(Factory.GetRegion(Core1, Obj3.BoldObjectLocator)) <> -1, '');
    assert(FLockHandler.RequiredShared.IndexOf(Factory.GetRegion(Core1, Obj4.BoldObjectLocator)) <> -1, '');
    assert(FLockHandler.RequiredShared.IndexOf(Factory.GetRegion(Core1, Obj5.BoldObjectLocator)) = -1, '');
  finally
    dmAnivFLS.BoldSystemHandle1.System.CommitTransaction;
    dmAnivFLS.BoldSystemHandle1.System.Discard;
  end;
end;

procedure TAniv_LockHandler.ReleaseUnNeededRegions;
var
  Factory: TBoldRegionFactory;
  Defs: TBoldRegionDefinitions;
  Core1: TBoldRegionCoreDefinition;
  aClassType1: TBoldClassTypeInfo;
  Def1: TBoldConcreteRegionDefinition;
  aMemberRT1: TBoldMemberRTInfo;
  Elem1: TBoldRegionElementInclusion;
  Sub1: TBoldSubregionReference;
  aRoleRT: TBoldRoleRTInfo;
  i: integer;
  Classes: array[1..4] of TClassA;
  Regions: array[1..4] of TBoldRegion;
begin
  Defs := dm_Model1.BoldSystemTypeInfoHandle1.RegionDefinitions;
  Defs.Clear;
  Core1 := TBoldRegionCoreDefinition.Create(Defs, 'r1');
  aClassType1 := dm_Model1.BoldSystemTypeInfoHandle1.StaticSystemTypeInfo.ClassTypeInfoByExpressionName['ClassA'];
  Def1 := TBoldConcreteRegionDefinition.Create(Core1, aClassType1);
  aMemberRT1 := aClassType1.MemberRTInfoByExpressionName['aString'];
  Elem1 := TBoldRegionElementInclusion.Create(Def1, aMemberRT1);
  aRoleRT := aClassType1.MemberRTInfoByExpressionName['child'] as TBoldRoleRTInfo;
  Sub1 := TBoldSubregionReference.Create(Def1, Core1, aRoleRT, true);
  Factory := dmAnivFLS.BoldSystemHandle1.RegionFactory;
  for i:= 1 to 4 do
  begin
    Classes[i] := TClassA.Create(dmAnivFLS.BoldSystemHandle1.System);
    Classes[i].aString := Format('ClassA%d', [i]);
  end;
  Classes[1].child.Add(Classes[2]);
  Classes[1].child.Add(Classes[4]);
  Classes[2].child.Add(Classes[4]);
  dmAnivFLS.BoldSystemHandle1.UpdateDatabase;

  for i:= 1 to 4 do
    Regions[i] := factory.GetRegion(Core1, Classes[i].BoldObjectLocator);
  FLockHandler.ReleaseUnNeededRegions;
  Assert(LockHolder.HeldExclusive.Count = 0);
  Assert(LockHolder.HeldShared.Count = 0);

  Classes[1].aString := 'Hello World';
  FLockHandler.ReleaseUnNeededRegions;
  Assert(LockHolder.HeldExclusive.Count = 1);
  Assert(LockHolder.HeldExclusive.Includes('1.r1'));
  Assert(LockHolder.HeldShared.Count = 2, Format('%d, should be %d',[LockHolder.HeldShared.Count,2]) );
  Assert(LockHolder.HeldShared.Includes('2.r1'));
  Assert(LockHolder.HeldShared.Includes('4.r1'));

  dmAnivFLS.BoldSystemHandle1.UpdateDatabase;
  Classes[3].aString := 'Goodbye world';
  Classes[3].child.Add(Classes[4]);
  FLockHandler.ReleaseUnNeededRegions;
  Assert(LockHolder.HeldExclusive.Count = 1);
  Assert(LockHolder.HeldExclusive.Includes('3.r1'));
//  Assert(LockHolder.HeldShared.Count = 2);
//  Assert(LockHolder.HeldShared.Includes('2.r1'));
  Assert(LockHolder.HeldShared.Includes('4.r1'));

  dmAnivFLS.BoldSystemHandle1.UpdateDatabase;
end;

procedure TAniv_LockHandler.RequiredRegions;
var
  Factory: TBoldRegionFactory;
  Defs: TBoldRegionDefinitions;
  Core1: TBoldRegionCoreDefinition;
  aClassType1: TBoldClassTypeInfo;
  Def1: TBoldConcreteRegionDefinition;
  aMemberRT1: TBoldMemberRTInfo;
  Elem1: TBoldRegionElementInclusion;
  Sub1: TBoldSubregionReference;
  aRoleRT: TBoldRoleRTInfo;
  i: integer;
  Classes: array[1..4] of TClassA;
  Ex: EBoldGetLocksFailed;
begin
  Defs := dm_Model1.BoldSystemTypeInfoHandle1.RegionDefinitions;
  Defs.Clear;
  Core1 := TBoldRegionCoreDefinition.Create(Defs, 'r1');
  aClassType1 := dm_Model1.BoldSystemTypeInfoHandle1.StaticSystemTypeInfo.ClassTypeInfoByExpressionName['ClassA'];
  Def1 := TBoldConcreteRegionDefinition.Create(Core1, aClassType1);
  aMemberRT1 := aClassType1.MemberRTInfoByExpressionName['aString'];
  Elem1 := TBoldRegionElementInclusion.Create(Def1, aMemberRT1);
  aRoleRT := aClassType1.MemberRTInfoByExpressionName['child'] as TBoldRoleRTInfo;
  Sub1 := TBoldSubregionReference.Create(Def1, Core1, aRoleRT, true);
  Factory := dmAnivFLS.BoldSystemHandle1.RegionFactory;
  for i:= 1 to 4 do
  begin
    Classes[i] := TClassA.Create(dmAnivFLS.BoldSystemHandle1.System);
    Classes[i].aString := Format('ClassA%d', [i]);
  end;
  Classes[1].child.Add(Classes[2]);
  Classes[2].child.Add(Classes[4]);

  dmAnivFLS.BoldSystemHandle1.UpdateDatabase;
  dmAnivFLS.BoldSystemHandle1.System.StartTransaction;
  try
    Classes[1].aString := 'Hello World';
    Assert(FLockHandler.RequiredExclusive.Count = 1);
    Assert(FLockHandler.RequiredShared.Count = 2, Format('%d, should be %d', [FLockHandler.RequiredShared.Count, 2]));
    Assert(FLockHandler.RequiredShared.IndexOf(Factory.GetRegion(Core1, classes[2].BoldObjectLocator)) <> -1);
    Assert(FLockHandler.RequiredShared.IndexOf(Factory.GetRegion(Core1, classes[4].BoldObjectLocator)) <> -1);
    Assert(FLockHandler.RequiredExclusive[0].Root = Classes[1]);
    Classes[1].child.Add(Classes[3]);

    Assert(FLockHandler.RequiredExclusive.Count = 1);
    Assert(FLockHandler.RequiredShared.IndexOf(Factory.GetRegion(Core1, classes[2].BoldObjectLocator)) <> -1);
    Assert(FLockHandler.RequiredShared.IndexOf(Factory.GetRegion(Core1, classes[3].BoldObjectLocator)) <> -1);
    Assert(FLockHandler.RequiredShared.IndexOf(Factory.GetRegion(Core1, classes[4].BoldObjectLocator)) <> -1);
    Assert(FLockHandler.RequiredExclusive[0].Root = Classes[1]);
    dmAnivFLS.BoldSystemHandle1.System.CommitTransaction;
  except
    dmAnivFLS.BoldSystemHandle1.System.RollbackTransaction;
    raise;
  end;

  Assert(FLockHandler.RequiredShared.Count = 0);
  Assert(FLockHandler.RequiredExclusive.Count = 0);
  dmAnivFLS.BoldSystemHandle1.UpdateDatabase;

  // conflict detection
  dmAnivFLS.BoldSystemHandle1.System.StartTransaction;
  try
    Classes[1].aString := 'Goodbye world';
    Assert(FLockHandler.RequiredShared.Count = 3);
    Assert(FLockHandler.RequiredExclusive.Count = 1);
    Assert(FLockHandler.RequiredShared.IndexOf(Factory.GetRegion(Core1, classes[2].BoldObjectLocator)) <> -1);
    Assert(FLockHandler.RequiredShared.IndexOf(Factory.GetRegion(Core1, classes[3].BoldObjectLocator)) <> -1);
    Assert(FLockHandler.RequiredShared.IndexOf(Factory.GetRegion(Core1, classes[4].BoldObjectLocator)) <> -1);
    Assert(FLockHandler.RequiredExclusive[0].Root = Classes[1]);
  except
    dmAnivFLS.BoldSystemHandle1.System.RollbackTransaction;
    raise;
  end;
  LockHolder.FInConflict := True;
  LockHolder.FConflictEvents.Add(Format('E:%s', [Classes[2].BoldObjectLocator.BoldObjectID.AsString]));
  try
    dmAnivFLS.BoldSystemHandle1.System.CommitTransaction;
    Assert(false, 'conflict not found');
  except on E: Exception do
    begin
      dmAnivFls.BoldSystemHandle1.System.RollbackTransaction;
      Assert(E is EBoldGetLocksFailed, 'EBoldGetLocksFailed : exception not raised');
      Ex := E as EBoldGetLocksFailed;
      Assert((Ex.ClientIds[0] = '1'), 'GetLocks: ClientsHoldingRequestedLocks: incorrect list') ;
      Assert((Ex.ConflictingRegions[0].AsString = '1.r1'), 'GetLocks: HeldLocks: incorrect list');
    end;
  end;
end;

procedure TAniv_LockHandler.SetUp;
begin
  EnsureDM;
  LockHolder := TDummyLockHolder.Create;
  FLockHandler := TBoldPessimisticLockHandler.CreateWithLockHolder(dmAnivFLS.BoldSystemHandle1.System, LockHolder);
end;

class procedure TAniv_LockHandler.Suit(ASuite: TBoldTestSuite);
begin
  ASuite.AddTest(CreateWithComment('ReleaseUnNeededRegions'));
  ASuite.AddTest(CreateWithComment('DeletingObjects'));
  ASuite.AddTest(CreateWithComment('ParentRegions'));
  ASuite.AddTest(CreateWithComment('RequiredRegions'));
end;

class function TAniv_LockHandler.Suite: ITestSuite;
begin
  Result := inherited Suite;
  SetCommentForTest(Result, 'ReleaseUnNeededRegions', '');
  SetCommentForTest(Result, 'DeletingObjects', '');
  SetCommentForTest(Result, 'ParentRegions', '');
  SetCommentForTest(Result, 'RequiredRegions', '');
end;

procedure TAniv_LockHandler.TearDown;
begin
  if Assigned(dmAnivFLS) and assigned(dmAnivFLS.BoldSystemHandle1.System) then
    dmAnivFLS.BoldSystemHandle1.System.Discard;
  FreeAndNil(FLockHandler);
  FreeAndNil(LockHolder);
  FreeAndNil(dmAnivFLS);
end;

{ TAniv_LockHolder }

procedure TAniv_LockHolder.SetUp;
begin
  EnsureDM;
  dmAnivFLS.BoldListenerHandle1.SetActive(True);
  LockHolder := TBoldLockHolder.Create(dmAnivFLS.BoldListenerHandle1.ListenerThread,
                                       dmAnivFLS.BoldListenerHandle1.Dequeuer,
                                       dmAnivFLS.BoldLockManagerHandleCom1.LockManager);
end;

class procedure TAniv_LockHolder.Suit(ASuite: TBoldTestSuite);
begin
  ASuite.AddTest(CreateWithComment('TestGetLocks'));
end;

class function TAniv_LockHolder.Suite: ITestSuite;
begin
  Result := inherited Suite;
  SetCommentForTest(Result, 'TestGetLocks', '');
end;

procedure TAniv_LockHolder.TearDown;
begin
  if Assigned(dmAnivFLS) and assigned(dmAnivFLS.BoldSystemHandle1.System) then
    dmAnivFLS.BoldSystemHandle1.System.Discard;
  FreeAndNil(LockHolder);
  FreeAndNil(dmAnivFLS);
end;

procedure TAniv_LockHolder.TestGetLocks;
var
  Shared, Exclusive: TBoldLockList;
  res: Boolean;
  ClientIds, HeldLocks: TStringList;
  G: IBoldGuard;
begin
  G := TBoldGuard.Create(Shared, Exclusive, ClientIds, HeldLocks);
  Shared := TBoldLockList.Create;
  Exclusive := TBoldLockList.Create;
  Shared.AddLock('HelloWorld');
  Exclusive.AddLock('HelloMe');
  ClientIds := TStringList.Create;
  HeldLocks := TStringList.Create;
  res := LockHolder.Lock(Shared, Exclusive, HeldLocks, ClientIds);
  Assert(res, 'Passed test');
  dmAnivFLS.BoldListenerHandle1.SetActive(False);
end;

{ TDummyLockHolder }

function TDummyLockHolder.Lock(Shared, Exclusive: TBoldLockList; HeldLocks, ClientsHoldingRequestedLocks: TStringList): Boolean;
begin
  result := true; //do nothing
  Shared.RemoveList(HeldShared);
  Shared.RemoveList(HeldExclusive);
  Exclusive.RemoveList(HeldExclusive);
  if result then
  begin
    HeldShared.AddList(Shared);
    HeldExclusive.AddList(Exclusive);
  end;
  if FInConflict then
  begin
    HeldLocks.Add('1.r1');
    ClientsHoldingRequestedLocks.Add('1');
  end;
end;

procedure TDummyLockHolder.Release(Locks: TBoldLockList);
begin
  HeldExclusive.RemoveList(Locks);
  HeldShared.RemoveList(Locks);
end;

procedure TDummyLockHolder.GetPropagationEvents(EventList: TStringList);
var
  i: integer;
begin
  if FInConflict then
  begin
    for i:= 0 to FConflictEvents.Count - 1 do
      EventList.Add(FConflictEvents[i]);
  end;
end;

constructor TDummyLockHolder.Create;
begin
  listenerThread := TBoldListenerThread.Create(true);
  inherited Create(listenerThread, nil, nil);
  FInConflict := false;
  FConflictEvents := TStringList.Create;
  fEnsureLocksResult := true;
end;

function TDummyLockHolder.EnsureLocks: Boolean;
begin
  result := fEnsureLocksResult;
end;

destructor TDummyLockHolder.Destroy;
begin
  FConflictEvents.Clear;
  FreeAndNil(fConflictEvents);
  ListenerThread.Quit(True);
  ListenerThread.Free;
  inherited;
end;

{ TAniv_LockingHandle }

procedure TAniv_LockingHandle.Connection;
begin
  EnsureDM;
  dmAnivFLS.BoldSystemHandle1.Active := false;
  dmAnivFLS.BoldLockingHandle1.SystemHandle := nil;
  dmAnivFLS.BoldLockingHandle1.LockManager := nil;

  assert(not dmAnivFLS.BoldSystemHandle1.Active, 'precond');
  assert(not dmAnivFLS.BoldLockingHandle1.Active, '');

  dmAnivFLS.BoldLockingHandle1.LockManager := dmAnivFLS.BoldLockManagerHandleCom1;
  assert(not dmAnivFLS.BoldLockingHandle1.Active, '');

  dmAnivFLS.BoldLockingHandle1.SystemHandle := dmAnivFLS.BoldSystemHandle1;
  assert(not dmAnivFLS.BoldLockingHandle1.Active, '');

  dmAnivFLS.BoldSystemHandle1.Active := true;
  assert(dmAnivFLS.BoldLockingHandle1.Active, '');
  assert(assigned(dmAnivFLS.BoldLockingHandle1.LockHandler), '');

  dmAnivFLS.BoldSystemHandle1.Active := false;
  assert(not dmAnivFLS.BoldLockingHandle1.Active, '');

  dmAnivFLS.BoldLockingHandle1.SystemHandle := nil;
  dmAnivFLS.BoldLockingHandle1.LockManager := nil;
end;

class procedure TAniv_LockingHandle.Suit(ASuite: TBoldTestSuite);
begin
  ASuite.AddTest(CreateWithComment('Connection'));
end;

class function TAniv_LockingHandle.Suite: ITestSuite;
begin
  result := inherited Suite;
  SetCommentForTest(Result, 'Connection', '');
end;

{ TAniv_FLS_Various }

procedure TAniv_FLS_Various.EnsureLocks;
var
  Defs: TBoldRegionDefinitions;
  Core1: TBoldRegionCoreDefinition;
  Def1: TBoldConcreteRegionDefinition;
  aClassType1: TBoldClassTypeInfo;
  Elem1: TBoldRegionElementInclusion;
  aMemberRT1: TBoldMemberRTInfo;
  Factory: TBoldRegionFactory;

  Obj1: TClassA;
begin
  Defs := dm_Model1.BoldSystemTypeInfoHandle1.RegionDefinitions;
  Defs.Clear;
  Core1 := TBoldRegionCoreDefinition.Create(Defs, 'r1');
  aClassType1 := dm_Model1.BoldSystemTypeInfoHandle1.StaticSystemTypeInfo.ClassTypeInfoByExpressionName['ClassA'];
  Def1 := TBoldConcreteRegionDefinition.Create(Core1, aClassType1);
  aMemberRT1 := aClassType1.MemberRTInfoByExpressionName['aString'];
  Elem1 := TBoldRegionElementInclusion.Create(Def1, aMemberRT1);

  Factory := dmAnivFLS.BoldSystemHandle1.RegionFactory;

  Obj1 := TClassA.Create(dmAnivFLS.BoldSystemHandle1.System);
  dmAnivFLS.BoldSystemHandle1.UpdateDatabase;

  Obj1.aString := 'some value';
  try
    LockHolder.fEnsureLocksResult := false;
    dmAnivFLS.BoldSystemHandle1.UpdateDatabase;
    assert(false, 'Should have raised an exception');
  except
  end;

  dmAnivFLS.BoldSystemHandle1.System.Discard;
end;

procedure TAniv_FLS_Various.SetUp;
begin
  EnsureDM;
  LockHolder := TDummyLockHolder.Create;
  LockHandler := TBoldPessimisticLockHandler.CreateWithLockHolder(dmAnivFLS.BoldSystemHandle1.System, LockHolder);
end;

class procedure TAniv_FLS_Various.Suit(ASuite: TBoldTestSuite);
begin
  ASuite.AddTest(CreateWithComment('EnsureLocks'));
end;

class function TAniv_FLS_Various.Suite: ITestSuite;
begin
  Result := inherited Suite;
  SetCommentForTest(Result, 'EnsureLocks', '');
end;

procedure TAniv_FLS_Various.TearDown;
begin
  if Assigned(dmAnivFLS) and assigned(dmAnivFLS.BoldSystemHandle1.System) then
    dmAnivFLS.BoldSystemHandle1.System.Discard;
  FreeAndNil(LockHandler);
  FreeAndNil(LockHolder);
  FreeAndNil(dmAnivFLS);
end;
{$ENDIF}

initialization
  TestGlobal.RegisterTestCase(TAniv_RegionDefinitions);
  TestGlobal.RegisterTestCase(TAniv_Regions);
 {$IFNDEF BOLD_NO_QUERIES}
  TestGlobal.RegisterTestCase(TAniv_FLS_Various); 
  TestGlobal.RegisterTestCase(TAniv_LockHandler);
  TestGlobal.RegisterTestCase(TAniv_LockHolder);
  TestGlobal.RegisterTestCase(TAniv_LockingHandle);
 {$ENDIF}
end.

