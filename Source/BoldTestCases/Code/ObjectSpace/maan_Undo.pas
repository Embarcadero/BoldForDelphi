unit maan_Undo;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  dmModel1,
  BoldUndoHandler,
  TestModel1, ActnList, BoldHandleAction, BoldActions, BoldDBActions,
  BoldHandle, BoldPersistenceHandle, BoldPersistenceHandleDB,
  BoldSubscription, BoldHandles, BoldSystem,
  BoldSystemHandle, BoldValueInterfaces,
  TestFrameWork,
  TestSuite, DB, BoldAbstractDatabaseAdapter,
  BoldAbstractPersistenceHandleDB, DBAccess, Uni, BoldDatabaseAdapterUniDAC;

type
  TdmUndo = class(TDataModule)
    BoldSystemHandle1: TBoldSystemHandle;
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
    BoldDatabaseAdapterUniDAC1: TBoldDatabaseAdapterUniDAC;
    UniConnection1: TUniConnection;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  Tmaan_UndoHandler = class(TBoldTestCase)
  private
    Songs: array[1..4] of TSong;
    ClassAArray: array[1..4] of TClassA;
    UndoHandler: TBoldUndoHandler;
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure HandleMember;
//    procedure FetchedValues;
    procedure UpdateDatabase;
    procedure UpdateDatabaseWithList;
    procedure Undo;
    procedure UndoInTransaction;
    procedure Redo;
    procedure MergeBlocks;
    procedure GetDependantBlocks;
    procedure MoveBlock;
    procedure SetCheckPoint;
  end;

var  
  dmUndo: TdmUndo;

implementation

uses
  BoldGuard,
  BoldDomainElement,
  BoldFreeStandingValues,
  BoldSystemRT,
  BoldId,
  BoldDefs
  ;

{$R *.DFM}

const
  cUnNamedBlockName = 'Undo';

procedure EnsureDM;
begin
  Ensuredm_Model;
  if not assigned(dmUndo) then
  begin
    Application.Initialize;
    dmUndo := TdmUndo.Create(Application);
    dmUndo.BoldDatabaseAdapterUniDac1.CreateDatabase;
    dmUndo.BoldPersistenceHandleDB1.CreateDataBaseSchema;
    dmUndo.BoldSystemHandle1.Active := True;
  end else
  begin
    if Assigned(dmUndo.BoldSystemHandle1.System) then
      dmUndo.BoldSystemHandle1.System.Discard;
    dmUndo.BoldSystemHandle1.Active := False;
    // freeing the modelDataModule can cause the systemhandle to lose the connection to the STIHandle
    dmUndo.BoldSystemHandle1.SystemTypeInfoHandle := dm_Model1.BoldSystemTypeInfoHandle1;
    dmUndo.BoldPersistenceHandleDB1.BoldModel := dm_Model1.BoldModel1;
    dmUndo.BoldSystemHandle1.Active := True;
  end;
end;

{ Tmaan_UndoHandler }

procedure Tmaan_UndoHandler.MoveBlock;
var
  Check1, Check2, Check3: string;
begin
  {2.6a- It is possible to move blocks in the Undo chain as long as no conflicts arise}
  ClassAArray[1] := TClassA(dmUndo.BoldSystemHandle1.System.CreateNewObjectByExpressionName('ClassA'));
  ClassAArray[2] := TClassA(dmUndo.BoldSystemHandle1.System.CreateNewObjectByExpressionName('ClassA'));
  ClassAArray[3] := TClassA(dmUndo.BoldSystemHandle1.System.CreateNewObjectByExpressionName('ClassA'));
  ClassAArray[1].aString := 'ABC';
  ClassAArray[2].aString := 'DEF';
  ClassAArray[3].aString := 'HIJ';
  dmUndo.BoldSystemHandle1.System.UpdateDatabase;
  Check1 := UndoHandler.SetCheckPoint('Check1');
  ClassAArray[1].aString := 'BC';
  Check2 := UndoHandler.SetCheckPoint('Check2');
  ClassAArray[2].aString := 'EF';
  Check3 := UndoHandler.SetCheckPoint('Check3');
  ClassAArray[2].aString := 'F';
  try
    UndoHandler.UndoBlocks.MoveBlock(UndoHandler.UndoBlocks.IndexOf(Check2), UndoHandler.UndoBlocks.IndexOf(Check3));
    Assert(False, 'Should not be able to mve Undo-blocks when there are dependencies');
  except on E:EBold do
  end;
  try
    UndoHandler.UndoBlocks.MoveBlock(UndoHandler.UndoBlocks.IndexOf(Check3), UndoHandler.UndoBlocks.IndexOf(Check2));
    Assert(False, 'Should not be able to mve Undo-blocks when there are dependencies');
  except on E:EBold do
  end;
  UndoHandler.UndoBlocks.MoveBlock(UndoHandler.UndoBlocks.IndexOf(Check1), UndoHandler.UndoBlocks.IndexOf(Check2));
  Assert(UndoHandler.UndoBlocks.CurrentBlock.BlockName = Check3);
  UndoHandler.UnDoLatest;
  Assert(ClassAArray[1].aString = 'BC');
  Assert(ClassAArray[2].aString = 'EF');
  UndoHandler.UndoLatest;
  Assert(ClassAArray[1].aString = 'ABC');
  Assert(ClassAArray[2].aString = 'EF');
  UndoHandler.UndoLatest;
  Assert(ClassAArray[2].aString = 'DEF');
  Assert(ClassAArray[1].aString = 'ABC');
  Assert(UndoHandler.UndoBlocks.Count = 0);

  {2.7f Move Redo-blocks (similar to 2.6a)}
  Assert(UndoHandler.RedoBlocks.Count = 3);
  Assert(UndoHandler.RedoBlocks.BlockByIndex[2].BlockName = Check2);
  Assert(UndoHandler.RedoBlocks.BlockByIndex[1].BlockName = Check1);
  Assert(UndoHandler.RedoBlocks.BlockByIndex[0].BlockName = Check3);
  try
    UndoHandler.RedoBlocks.MoveBlock(UndoHandler.RedoBlocks.IndexOf(Check2), UndoHandler.RedoBlocks.IndexOf(Check3));
    Assert(False, 'Should not be able to mve Redo-blocks when there are dependencies');
  except on E:EBold do
  end;
  try
    UndoHandler.RedoBlocks.MoveBlock(UndoHandler.RedoBlocks.IndexOf(Check3), UndoHandler.RedoBlocks.IndexOf(Check2));
    Assert(False, 'Should not be able to move Redo-blocks when there are dependencies');
  except on E:EBold do
  end;
  UndoHandler.RedoBlocks.MoveBlock(UndoHandler.RedoBlocks.IndexOf(Check1), UndoHandler.RedoBlocks.IndexOf(Check3));
  UndoHandler.RedoLatest;
  Assert(ClassAArray[1].aString = 'ABC');
  Assert(ClassAArray[2].aString = 'EF');
  UndoHandler.RedoLatest;
  Assert(ClassAArray[1].aString = 'ABC');
  Assert(ClassAArray[2].aString = 'F');
  UndoHandler.RedoLatest;
  Assert(ClassAArray[1].aString = 'BC');
  Assert(ClassAArray[2].aString = 'F');

  dmUndo.BoldSystemHandle1.UpdateDatabase;
end;

{procedure Tmaan_UndoHandler.FetchedValues;
var
  idx: integer;
  Str: string;
  Value: IBoldValue;
  IdListRefPair: IBoldObjectIdListRefPair;
  Id1, Id2: TBoldObjectId;
  IdListRef: IBoldObjectIdListRef;
  procedure CheckFetchedValue(const Member: TBoldMember; const CheckedValue: string);
  var
    aValue: IBoldValue;
    str: string;
  begin
    aValue := UndoHandler.GetFetchedValue(Member);
    str := (aValue as IBoldStringContent).asString;
    Assert(str = CheckedValue);
  end;
  procedure GetValue(const Member: TBoldmember);
  begin
    Value := UndoHandler.GetFetchedValue(Member);
  end;
begin
  UndoHandler := TBoldUndoHandler(dmUndo.BoldSystemHandle1.System.UndoHandler);
  Songs[1] := TSong(dmUndo.BoldSystemHandle1.System.CreateNewObjectByExpressionName('Song'));
  Songs[1].Title := 'Song1';
  Songs[2] := TSong(dmUndo.BoldSystemHandle1.System.CreateNewObjectByExpressionName('Song'));
  Songs[2].Title := 'Song2';
  dmUndo.BoldSystemHandle1.UpdateDatabase;
  //check fetched value of attributes
  Str := Songs[1].Title;
  Songs[1].Title := 'Just changed';
  checkFetchedValue(Songs[1].M_Title, str);
  HitLists[1] := THitList(dmUndo.BoldSystemHandle1.System.CreateNewObjectByExpressionName('HitList'));
  HitLists[1].Name := 'Hit1';
  Songs[1].HitList.Add(HitLists[1]);
  Songs[2].Delete;
  GetValue(Songs[1].HitList);
  Assert((Value as IBoldObjectIdListRefPair).Count = 0);
  GetValue(Songs[1].hitListsong);
  Assert((Value as IBoldObjectIdListRef).Count = 0);
  dmUndo.BoldSystemHandle1.UpdateDatabase;
  idx := Songs[1].HitList.IndexOf(HitLists[1]);
  IdListRefPair := Songs[1].HitList.AsIBoldValue[bdepContents] as IBoldObjectIdListRefPair;
  Id1 := IdListRefPair.IdList1[0].Clone;
  Id2 := IdListRefPair.IdList2[0].Clone;
  songs[1].HitList.RemoveByIndex(idx);
  GetValue(Songs[1].HitList);
  IdListRefPair := Value as IBoldObjectIdListRefPair;
  Assert(IdListRefPair.Count = 1);
  Assert(IdListRefPair.IdList1[0].IsEqual[Id1]);
  Assert(IdListRefPair.IdList2[0].IsEqual[Id2]);
  GetValue(Songs[1].HitListSong);
//  Assert((Value as IBoldObjectIdListRef).IdList[0] = LinkObjectId);
  dmUndo.BoldSystemHandle1.UpdateDatabase;

  //direct multi links
  ClassAArray[1] := TClassA(dmUndo.BoldSystemHandle1.System.CreateNewObjectByExpressionName('ClassA'));
  ClassAArray[2] := TClassA(dmUndo.BoldSystemHandle1.System.CreateNewObjectByExpressionName('ClassA'));
  ClassAArray[1].aString := 'CA1';
  ClassAArray[2].aString := 'CA2';
  ClassAArray[1].child.Add(ClassAArray[2]);
  GetValue(ClassAArray[1].child);
  IdListRef := Value as IBoldObjectIdListRef;
  Assert(IdlistRef.Count  = 0);
  dmUndo.BoldSystemHandle1.UpdateDatabase;
  ClassAArray[1].child.RemoveByIndex(ClassAArray[1].child.IndexOf(ClassAArray[2]));
  GetValue(ClassAArray[1].child);
  IdListRef := Value as IBoldObjectIdListRef;
  Assert(IdListRef.Count = 1);
  Assert(IdListRef.IdList[0].IsEqual[ClassAArray[2].BoldObjectLocator.BoldObjectID]);
  dmUndo.BoldSystemHandle1.UpdateDatabase;
end;
}
procedure Tmaan_UndoHandler.HandleMember;
var
  OpenBlock, CurrentBlock: TBoldUndoBlock;
  ObjectIds: TBoldObjectIdList;
  G: IBoldGuard;
  Check1,Check2,Check3,Check4,Check5: string;
  function CheckValueExists: string;
  var
    aValue: IBoldValue;
  begin
    if UndoHandler.UndoBlocks.CurrentBlock.ValueExists(Songs[1].BoldObjectLocator.BoldObjectID, Songs[1].M_Title.BoldMemberRTInfo.index, aValue) then
    begin
      Result := (aValue as IBoldStringContent).asString;
    end;
    aValue := nil;
  end;
begin
  G := TBoldGuard.Create(ObjectIds);

  ObjectIds := TBoldObjectIdList.Create;
  {Test 1}
  Check1 := UndoHandler.SetCheckPoint('Check1');
  Songs[1] := TSong(dmUndo.BoldSystemHandle1.System.CreateNewObjectByExpressionName('Song'));
  Songs[1].Title := 'Dreams';
  Songs[2] := TSong(dmUndo.BoldSystemHandle1.System.CreateNewObjectByExpressionName('Song'));
  Songs[2].Title := 'Dreaming';
  Check2 := UndoHandler.SetCheckPoint('Check2');
  Songs[1].Title := 'Song1';
  Songs[2].Title := 'Song2';
  Songs[1].Title := 'Song11';
  Songs[2].Title := 'Song22';
  OpenBlock := UndoHandler.Undoblocks.CurrentBlock;
  ObjectIds.Clear;
  OpenBlock.FSValueSpace.AllObjectIds(ObjectIds, true);
  Assert(ObjectIds.Count = 2);
  //save all
  dmUndo.BoldSystemHandle1.UpdateDatabase;
  Assert(UndoHandler.Undoblocks.Count = 0);
  {Test 2}
  ObjectIds.Clear;
  Songs[1].Title := 'S11';
  Songs[2].Title := 'S22';
  ObjectIds.Clear;
  UndoHandler.Undoblocks.CurrentBlock.FSValueSpace.allObjectIds(ObjectIds, true);
  Assert(ObjectIds.Count = 2);
  Songs[1].Title := 'Song12';
  ObjectIds.Clear;
  UndoHandler.Undoblocks.CurrentBlock.FSValueSpace.AllObjectIds(ObjectIds, true);
  Assert(ObjectIds.Count = 2);
  Assert( CheckValueExists = 'Song11');
  Check4 := UndoHandler.SetCheckPoint('Check4');
  Check5 := UndoHandler.SetCheckPoint('Check5');
  Songs[1].Title := 'S1234';
  dmUndo.BoldSystemHandle1.UpdateDatabase;
end;

procedure Tmaan_UndoHandler.MergeBlocks;
var
  Check1,Check2,Check3,Check4,Check5,Check6: string;
begin
  {2.5a It is possible to merge two adjacent blocks}
  {2.5b it is possible to merge two non-adjacent blocks provided that none of the intervening blocks
        modify any member contained in the first(bottom) of the two}
  ClassAArray[1] := TClassA(dmUndo.BoldSystemHandle1.System.CreateNewObjectByExpressionName('ClassA'));
  ClassAArray[2] := TClassA(dmUndo.BoldSystemHandle1.System.CreateNewObjectByExpressionName('ClassA'));
  ClassAArray[3] := TClassA(dmUndo.BoldSystemHandle1.System.CreateNewObjectByExpressionName('ClassA'));
  ClassAArray[1].aString := 'ABC';
  ClassAArray[2].aString := 'DEF';
  ClassAArray[3].aString := 'JHI';
  dmUndo.BoldSystemHandle1.UpdateDatabase;
  Check1 := UndoHandler.SetCheckPoint('Check1');
  ClassAArray[1].aString := 'BC';
  Check2 := UndoHandler.SetCheckPoint('Check2');
  ClassAArray[2].aString := 'EF';
  ClassAArray[3].aString := 'HI';
  Check3 := UndoHandler.SetCheckPoint('Check3');
  ClassAArray[2].aString := 'EFG';
  ClassAArray[3].aString := 'I';
  Assert(UndoHandler.UndoBlocks.CanMergeBlock(UndoHandler.UndoBlocks.IndexOf(Check1), UndoHandler.UndoBlocks.IndexOf(Check3)), 'Should  be able to merge blocks check1 and check3');
  Assert(not UndoHandler.UndoBlocks.CanMergeBlock(UndoHandler.UndoBlocks.IndexOf(Check3), UndoHandler.UndoBlocks.IndexOf(Check1)), 'Should not be able to merge check3 into check1 check1 and check3');
  UndoHandler.UndoBlocks.MergeBlocks(Check1, Check2);
  Assert(Assigned(UndoHandler.UndoBlocks.BlockByName[Check1]));
  UndoHandler.UndoBlock(UndoHandler.Undoblocks.CurrentBlock.BlockName);
  UndoHandler.UndoBlock(UndoHandler.Undoblocks.CurrentBlock.BlockName);
  Assert(ClassAArray[1].aString = 'ABC');
  Assert(ClassAArray[2].aString = 'DEF');
  Assert(ClassAArray[3].aString = 'JHI');
  Assert(UndoHandler.UndoBlocks.Count = 0);
  Check4 := UndoHandler.SetCheckPoint('Check4');
  ClassAArray[1].aString := '111';
  ClassAArray[3].aString := 'xxx';
  Check5 := UndoHandler.SetCheckPoint('Check5');
  ClassAArray[2].aString := 'aaa';
  Check6 := UndoHandler.SetCheckPoint('Check6');
  ClassAArray[2].aString := 'bbb';
  ClassAArray[3].aString := 'yyy';
  Assert(UndoHandler.UndoBlocks.CanMergeBlock(UndoHandler.UndoBlocks.IndexOf(Check5), UndoHandler.UndoBlocks.IndexOf(Check6)));
  Assert(not UndoHandler.UndoBlocks.CanMergeBlock(UndoHandler.UndoBlocks.IndexOf(Check6), UndoHandler.UndoBlocks.IndexOf(Check4)));
  UndoHandler.UndoBlocks.MergeBlocks(Check6, Check4);
  UndoHandler.UnDoLatest;
  Assert(ClassAArray[2].aString = 'aaa');
  Assert(ClassAArray[1].aString = 'ABC');
  Assert(ClassAArray[3].aString = 'JHI');
  UndoHandler.UnDoLatest;
  Assert(ClassAArray[2].aString = 'DEF');
  Assert(ClassAArray[1].aString = 'ABC');
  Assert(ClassAArray[3].aString = 'JHI');
  UndoHandler.RedoLatest;
  Assert(ClassAArray[2].aString = 'aaa');
  Assert(ClassAArray[1].aString = 'ABC');
  Assert(ClassAArray[3].aString = 'JHI');
  UndoHandler.RedoLatest;
  Assert(ClassAArray[2].aString = 'bbb');
  Assert(ClassAArray[1].aString = '111');
  Assert(ClassAArray[3].aString = 'yyy');
  dmUndo.BoldSystemHandle1.System.UpdateDatabase;
end;

procedure Tmaan_UndoHandler.SetCheckPoint;
var
  Check1,Check2,Check3: string;
begin
  {2.7a Create Undo block}
  {2.7e Name and rename blocks}
  try
    Check1 := UndoHandler.SetCheckPoint('Check1');
    Check2 := UndoHandler.SetCheckPoint('Check1');
    Assert(False);
  except
    Assert(UndoHandler.UndoBlocks.Count = 2);
  end;
  Check2 := UndoHandler.SetCheckPoint('Check2');
  Assert(UndoHandler.UndoBlocks.Count = 2);

  dmUndo.BoldSystemHandle1.System.CreateNewObjectByExpressionName('ClassA', true);
  Check3 := UndoHandler.SetCheckPoint('Check3');
  Assert(UndoHandler.UndoBlocks.Count = 3);
  Assert(UndoHandler.Undoblocks.BlockByIndex[0].BlockName = Check1);
  try
    UndoHandler.UndoBlocks.RenameBlock(Check3, Check2);
    Assert(False);
  except
    Assert(True);
  end;
  UndoHandler.UndoBlocks.RenameBlock(Check2, 'RenamedBlock');
  Assert(Assigned(UndoHandler.UndoBlocks.BlockByName['RenamedBlock']));
  dmUndo.BoldSystemHandle1.UpdateDatabase;
end;

procedure Tmaan_UndoHandler.SetUp;
begin
  EnsureDM;
  UndoHandler := TBoldUndoHandler(dmUndo.BoldSystemHandle1.System.UndoHandler);
  UndoHandler.Enabled := true;
end;

class procedure Tmaan_UndoHandler.Suit(ASuite: TBoldTestSuite);
begin
  ASuite.AddTest(CreateWithComment('SetCheckPoint', '2.7a Create Undo block & 2.7e Name and rename blocks'));
  ASuite.AddTest(CreateWithComment('HandleMember'));
  ASuite.AddTest(CreateWithComment('Undo', '2.7b Undo block'));
  ASuite.AddTest(CreateWithComment('Redo', '2.2b The reverse of the changes will be placed as a Redo block ...' +
  '& 2.7c Redo block' +
  '& 2.3d Following one or more UnDos the application ...' + ' & 2.3c All ReDo information is lost as soon as ...'));
  ASuite.AddTest(CreateWithComment('UpdateDatabase', '2.1b All Undo information is lost when the database is updated'));
  ASuite.AddTest(CreateWithComment('UpdateDatabaseWithList', '2.1c If only part of the data is updated, the remaining changes will be merged into a single open Undo-block'));
  ASuite.AddTest(CreateWithComment('MergeBlocks', '2.5a Merge two adjacent blocks.' +
  '& 2.5b Merge two non-adjacent blocks provided that none of the intervening blocks modify any member contained in the first(bottom) of the two'));
  ASuite.AddTest(CreateWithComment('MoveBlock', '2.6a Move blocks in the Undo chain as long as no conflicts arise.'
        + ' & 2.7f Move Redo-blocks (similar to 2.6a)'));
  ASuite.AddTest(CreateWithComment('GetDependantBlocks', '2.7d Get dependant blocks'));
//TODO: review  ASuite.AddTest(Create('FetchedValues'));
  ASuite.AddTest(CreateWithComment('UndoInTransaction', '2.1a The Undo-mechanism can only be invoked outside a transaction'));
end;

procedure Tmaan_UndoHandler.TearDown;
begin
  if dmUndo.BoldSystemHandle1.Active then
  begin
    dmUndo.BoldSystemHandle1.System.Discard;
    dmUndo.BoldSystemHandle1.Active := false;
  end;
  FreeAndNil(dmUndo);
end;

procedure Tmaan_UndoHandler.Undo;
var
  Check1: string;
begin
  {2.7b Undo block}
  ClassAArray[1] := TClassA(dmUndo.BoldSystemHandle1.System.CreateNewObjectByExpressionName('ClassA', True));
  ClassAArray[2] := TClassA(dmUndo.BoldSystemHandle1.System.CreateNewObjectByExpressionName('ClassA', True));
  ClassAArray[1].aString := 'CA1';
  ClassAArray[2].aString := 'CA2';
  Check1 := UndoHandler.SetCheckPoint('Check1');
  ClassAArray[1].aString := 'Class1';
  ClassAArray[2].aString := 'Class2';
  UndoHandler.UndoBlock(Check1);
  Assert(ClassAArray[1].aString = 'CA1');
  Assert(ClassAArray[2].aString = 'CA2');
  UndoHandler.RedoBlock(Check1);
  Assert(ClassAArray[1].aString = 'Class1');
  Assert(ClassAArray[2].aString = 'Class2');
  dmUndo.BoldSystemHandle1.UpdateDatabase;
end;

procedure Tmaan_UndoHandler.UpdateDatabase;
var
  str: string;
  ObjectIds: TBoldObjectIdList;
  Check1: string;
{
  procedure CheckFetchedValue(const Member: TBoldMember; const CheckedValue: string);
  var
    aValue: IBoldValue;
    str: string;
  begin
    aValue := UndoHandler.GetFetchedValue(Member);
    str := (aValue as IBoldStringContent).asString;
    Assert(str = CheckedValue);
  end;
  procedure GetValue(const Member: TBoldmember);
  begin
    Value := UndoHandler.GetFetchedValue(Member);
  end;
  }
begin
  {2.1b- All Undo information is lost when the database is updated}
  ObjectIds := TBoldObjectIdList.Create;
  try
    UndoHandler := TBoldUndoHandler(dmUndo.BoldSystemHandle1.System.UndoHandler);
    Songs[1] := TSong(dmUndo.BoldSystemHandle1.System.CreateNewObjectByExpressionName('Song'));
    Songs[1].Title := 'Song1';
    Songs[2] := TSong(dmUndo.BoldSystemHandle1.System.CreateNewObjectByExpressionName('Song'));
    Songs[2].Title := 'Song2';
    dmUndo.BoldSystemHandle1.UpdateDatabase;
    Assert(UndoHandler.UndoBlocks.Count = 0);
//    Undohandler.NonUndoableBlock.FSValueSpace.AllObjectIds(ObjectIds, true);
//    Assert(ObjectIds.Count = 0);
    Check1 := UndoHandler.SetCheckPoint('Check1');
    Str := Songs[1].Title;
    Songs[1].Title := 'Lemon';
//    checkFetchedValue(Songs[1].M_Title, str);
    dmUndo.BoldSystemHandle1.UpdateDatabase;
    Assert(UndoHandler.UndoBlocks.Count = 0);

  finally
    FreeAndNil(ObjectIds);
  end;
end;

procedure Tmaan_UndoHandler.Redo;
var
  Check1, Check2: string;
begin
  {2.2b The reverse of the changes will be placed as a Redo block at the top of the Redo chain}
  {2.7c Redo block}
  {2.3d Following one or more UnDos the application can then ReDo the changes undone, in the reverse order}
  ClassAArray[1] := TClassA(dmUndo.BoldSystemHandle1.System.CreateNewObjectByExpressionName('ClassA', True));
  ClassAArray[2] := TClassA(dmUndo.BoldSystemHandle1.System.CreateNewObjectByExpressionName('ClassA', True));
  ClassAArray[1].aString := 'CA1';
  ClassAArray[2].aString := 'CA2';
  dmUndo.BoldSystemHandle1.UpdateDatabase;
  Check1 := UndoHandler.SetCheckPoint('Check1');
  ClassAArray[1].aString := 'Class1';
  ClassAArray[2].aString := 'Class2';
  ClassAArray[2].parent := ClassAArray[1];
  Assert(ClassAArray[1].child.Count = 1);

  Check2 := UndoHandler.SetCheckPoint('Check2');
  ClassAArray[1].aString := 'aClassObject1';
  ClassAArray[2].aString := 'aClassObject2';

  UndoHandler.UnDoLatest;
  Assert(ClassAArray[1].aString = 'Class1');
  Assert(ClassAArray[2].aString = 'Class2');
  Assert(ClassAArray[2].parent = ClassAArray[1]);
  Assert(ClassAArray[1].child.Count = 1);
  Assert(UndoHandler.RedoBlocks.Count = 1);
  Assert(Assigned(UndoHandler.RedoBlocks.BlockByName[Check2]));

  UndoHandler.UnDoLatest;
  Assert(ClassAArray[1].aString = 'CA1');
  Assert(ClassAArray[2].aString = 'CA2');
  Assert(ClassAArray[2].parent = nil);
  Assert(ClassAArray[1].child.Count = 0);
  Assert(UndoHandler.RedoBlocks.Count = 2);
  Assert(Assigned(UndoHandler.RedoBlocks.BlockByName[Check1]));


  UndoHandler.RedoLatest;
  Assert(ClassAArray[1].aString = 'Class1');
  Assert(ClassAArray[2].aString = 'Class2');
  Assert(ClassAArray[2].parent = ClassAArray[1]);
  Assert(ClassAArray[1].child.Count = 1);

  UndoHandler.RedoLatest;
  Assert(ClassAArray[1].aString = 'aClassObject1');
  Assert(ClassAArray[2].aString = 'aClassObject2');
  Assert(ClassAArray[2].parent = ClassAArray[1]);
  Assert(ClassAArray[1].child.Count = 1);

  {2.3c All ReDo information is lost as soon as any change is made by other means than Undo/Redo}
  UndoHandler.UndoLatest;
  Assert(UndoHandler.RedoBlocks.Count = 1);
  UndoHandler.UndoLatest;
  Assert(UndoHandler.RedoBlocks.Count = 2);
  Assert(ClassAArray[1].aString = 'CA1');
  Assert(ClassAArray[2].aString = 'CA2');
  Assert(ClassAArray[2].parent = nil);
  Assert(ClassAArray[1].child.Count = 0);

  ClassAArray[1].aString := 'ABC';
  Assert(UndoHandler.RedoBlocks.Count = 0);
  dmUndo.BoldSystemHandle1.UpdateDatabase;
end;

type TBoldSystemAccess = class(TBoldSystem);

procedure Tmaan_UndoHandler.UpdateDatabaseWithList;
var
  ObjectIds: TBoldObjectIdList;
  ObjectList: TBoldObjectList;
  Check1, Check2, Check3: string;
{
  procedure CheckFetchedValue(const Member: TBoldMember; const CheckedValue: string);
  var
    aValue: IBoldValue;
    str: string;
  begin
    aValue := UndoHandler.GetFetchedValue(Member);
    str := (aValue as IBoldStringContent).asString;
    Assert(str = CheckedValue);
  end;
  procedure GetValue(const Member: TBoldmember);
  begin
    Value := UndoHandler.GetFetchedValue(Member);
  end;
  }

begin
  {2.1c- If only part of the data is updated, the remaining changes will be merged into a single 'open'
   Undo-block}
  ObjectIds := TBoldObjectIdList.Create;
  try
    UndoHandler := TBoldUndoHandler(dmUndo.BoldSystemHandle1.System.UndoHandler);
    Songs[1] := TSong(dmUndo.BoldSystemHandle1.System.CreateNewObjectByExpressionName('Song'));
    Songs[1].Title := 'Song1';
    Songs[2] := TSong(dmUndo.BoldSystemHandle1.System.CreateNewObjectByExpressionName('Song'));
    Songs[2].Title := 'Song2';
    Songs[3] := TSong(dmUndo.BoldSystemHandle1.System.CreateNewObjectByExpressionName('Song'));
    Songs[3].Title := 'Song3';
    ObjectList := TBoldObjectList.Create;
    ObjectList.Add(Songs[1]);
    dmUndo.BoldSystemHandle1.UpdateDatabase;
    Assert(TBoldSystemAccess(dmUndo.BoldSystemHandle1.System).OldValueHandler.IsEmpty);
    Songs[1].Title := 'American Pie';
    Songs[2].Title := 'Music';
    dmUndo.BoldSystemHandle1.System.UpdateDatabaseWithList(ObjectList);
    Assert(UndoHandler.UndoBlocks.Count = 0);
    ObjectIds.Clear;
//    UndoHandler.NonUndoableBlock.FSValueSpace.AllObjectIds(ObjectIds, True);
//    Assert(ObjectIds.Count = 1);
//    Assert(ObjectIds[0].IsEqual[Songs[2].BoldObjectLocator.BoldObjectID]);
    dmUndo.BoldSystemHandle1.UpdateDatabase;
    Assert(TBoldSystemAccess(dmUndo.BoldSystemHandle1.System).OldValueHandler.IsEmpty);
    Assert(UndoHandler.UndoBlocks.Count = 0);
    ObjectIds.Clear;
//    UndoHandler.NonUndoableBlock.FSValueSpace.AllObjectIds(ObjectIds, True);
//    Assert(ObjectIds.Count = 0);

    Check1 := UndoHandler.SetCheckPoint('Check1');
    Songs[1].Title := 'Lemon';
    Check2 := UndoHandler.SetCheckPoint('Check2');
    Songs[2].Title := 'Discotheque';
    Check3 := UndoHandler.SetCheckPoint('Check3');
    Songs[3].Title := 'WindSurf';
    dmUndo.BoldSystemHandle1.System.UpdateDatabaseWithList(ObjectList);
    Assert(UndoHandler.UndoBlocks.Count = 1);
    ObjectIds.Clear;
//    UndoHandler.NonUndoableBlock.FSValueSpace.AllObjectIds(ObjectIds, True);
//    Assert(ObjectIds.Count = 0);
    dmUndo.BoldSystemHandle1.UpdateDatabase;
    Assert(TBoldSystemAccess(dmUndo.BoldSystemHandle1.System).OldValueHandler.IsEmpty);    
    Assert(UndoHandler.UndoBlocks.Count = 0);
    ObjectIds.Clear;
//    UndoHandler.NonUndoableBlock.FSValueSpace.AllObjectIds(ObjectIds, True);
//    Assert(ObjectIds.Count = 0);
  finally
    FreeAndNil(ObjectIds);
    FreeAndNil(ObjectList);
  end;
end;

procedure Tmaan_UndoHandler.GetDependantBlocks;
var
  aBlock: TBoldUndoBlock;
  DependantBlocks: TList;
  Check1, Check2, Check3: string;
begin
  {2.7d Get dependant blocks}
  DependantBlocks := TList.Create;
  try
    ClassAArray[1] := TClassA(dmUndo.BoldSystemHandle1.System.CreateNewObjectByExpressionName('ClassA', True));
    ClassAArray[2] := TClassA(dmUndo.BoldSystemHandle1.System.CreateNewObjectByExpressionName('ClassA', True));
    ClassAArray[3] := TClassA(dmUndo.BoldSystemHandle1.System.CreateNewObjectByExpressionName('ClassA', True));

    ClassAArray[1].aString := 'aClassObject1';
    ClassAArray[2].aString := 'aClassObject2';
    ClassAArray[3].aString := 'aClassObject3';
    dmUndo.BoldSystemHandle1.UpdateDatabase;

    Check1 := UndoHandler.SetCheckPoint('Check1');
    ClassAArray[1].aString := 'Die Hard 1';
    ClassAArray[2].aString := 'Mission Impossible 1';

    Check2 := UndoHandler.SetCheckPoint('Check2');
    ClassAArray[2].aString := 'Mission Impossible 2';
    ClassAArray[3].aString := 'Speed 2';

    Check3 := UndoHandler.SetCheckPoint('Check3');
    ClassAArray[1].aString := 'Die Hard 3';
    ClassAArray[2].aString := 'Mission Impossible 3';

    aBlock := UndoHandler.UndoBlocks.BlockByName[Check3];
    UndoHandler.UndoBlocks.GetDependantBlocks(Check3, DependantBlocks);
    Assert(DependantBlocks.Count = 2);

    dmUndo.BoldSystemHandle1.UpdateDatabase;
  finally
    FreeAndNil(DependantBlocks);
  end;
end;

procedure Tmaan_UndoHandler.UndoInTransaction;
var
  Check1: string;
begin
  {2.1a- The Undo-mechanism can only be invoked outside a transaction}
  Songs[1] := TSong(dmUndo.BoldSystemHandle1.System.CreateNewObjectByExpressionName('Song'));
  Songs[1].Title := 'Dreams';
  dmUndo.BoldSystemHandle1.UpdateDatabase;
  Check1 := UndoHandler.SetCheckPoint('SetCheckPoint1');
  Songs[1].Title := 'Dreaming';
  dmUndo.BoldSystemHandle1.System.StartTransaction;
  try
    UndoHandler.UnDoLatest;
    Assert(False, 'It should only be possible to invoke the Undo-mechanism outside a transaction');
  except on E: EBold do
    dmUndo.BoldSystemHandle1.System.RollbackTransaction;
  end;
  UndoHandler.UnDoLatest;
  dmUndo.BoldSystemHandle1.System.StartTransaction;
  try
    UndoHandler.Redolatest;
    Assert(False, 'It should only be possible to invoke the Undo-mechanism outside a transaction');
  except on E: EBold do
    dmUndo.BoldSystemHandle1.System.RollbackTransaction;
  end;
end;

class function Tmaan_UndoHandler.Suite: ITestSuite;
begin
  result := inherited Suite;
  SetCommentForTest(Result, 'SetCheckPoint', '2.7a Create Undo block & 2.7e Name and rename blocks');
  SetCommentForTest(Result, 'HandleMember', '');
  SetCommentForTest(Result, 'Undo', '2.7b Undo block');
  SetCommentForTest(Result, 'Redo', '2.2b The reverse of the changes will be placed as a Redo block ...' +
  '& 2.7c Redo block' +
  '& 2.3d Following one or more UnDos the application ...' + ' & 2.3c All ReDo information is lost as soon as ...');
  SetCommentForTest(Result, 'UpdateDatabase', '2.1b All Undo information is lost when the database is updated');
  SetCommentForTest(Result, 'UpdateDatabaseWithList', '2.1c If only part of the data is updated, the remaining changes will be merged into a single open Undo-block');
  SetCommentForTest(Result, 'MergeBlocks', '2.5a Merge two adjacent blocks.' +
  '& 2.5b Merge two non-adjacent blocks provided that none of the intervening blocks modify any member contained in the first(bottom) of the two');
  SetCommentForTest(Result, 'MoveBlock', '2.6a Move blocks in the Undo chain as long as no conflicts arise.'
        + ' & 2.7f Move Redo-blocks (similar to 2.6a)');
  SetCommentForTest(Result, 'GetDependantBlocks', '2.7d Get dependant blocks');
//TODO: review  ASuite.AddTest(Create('FetchedValues'));
  SetCommentForTest(Result, 'UndoInTransaction', '2.1a The Undo-mechanism can only be invoked outside a transaction');
end;

initialization
  TestGlobal.RegisterTestCase(Tmaan_UndoHandler);

finalization
  TestGlobal.UnRegisterTestCase(Tmaan_UndoHandler);
end.
