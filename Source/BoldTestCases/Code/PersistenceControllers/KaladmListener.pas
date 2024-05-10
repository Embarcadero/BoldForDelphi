unit KaladmListener;

{$INCLUDE Bold.inc}

interface

uses
  {$IFDEF BOLD_DELPHI6_OR_LATER}
  variants,
  {$ENDIF}
  SysUtils,
  Classes,
  Controls,
  Forms,
  Dialogs,
  TestSuite,
  BoldAbstractDequeuer,
  BoldExternalObjectSpaceEventHandler,
  BoldSubscription,
  BoldHandle,
  BoldHandles,
  BoldListenerHandle,
  BoldSystemHandle,
  BoldPropagatorInterfaces_TLB,
  BoldListenerCOM,
  ComServ,
  TestModel1,
  dmModel1,
  BoldSystem,
  BoldPersistenceHandle,
  BoldPersistenceHandleDB,
  ActnList,
  BoldHandleAction,
  BoldActions,
  BoldDBActions,
  TestFramework,
  BoldAbstractPersistenceHandleDB,
  DB,
  BoldAbstractDatabaseAdapter,
  DBAccess,
  Uni,
  BoldDatabaseAdapterUniDAC,
  System.Actions;

type
  TDataModule2 = class(TDataModule)
    BoldListenerHandle1: TBoldListenerHandle;
    BoldExternalObjectSpaceEventHandler1: TBoldExternalObjectSpaceEventHandler;
    BoldSystemHandle1: TBoldSystemHandle;
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
    ActionList: TActionList;
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
    BoldDatabaseAdapterUniDAC1: TBoldDatabaseAdapterUniDAC;
    UniConnection1: TUniConnection;
    procedure BoldExternalObjectSpaceEventHandler1ClassChanged(
      TheClass: TBoldObjectList);
    procedure BoldExternalObjectSpaceEventHandler1EmbeddedStateChanged(
      BoldObject: TBoldObject);
    procedure BoldExternalObjectSpaceEventHandler1NonEmbeddedStateChanged(
      BoldMember: TBoldMember);
    procedure BoldExternalObjectSpaceEventHandler1LockLost(
      LockName: String);
  private
    ClassChangedCalled, EmeddedStateChangedCalled, NonembeddedStateChangedCalled, LockLostEvent: Boolean;
    LockName: string;    
  public
  end;

  TKalaListenerTest = class(TBoldTestCase)
  private
    function CreateAndSendTestEvents(aListener: TBoldListenerCOM; var FakeEvents: Variant): IBoldListener;
  public
    procedure SetUp; override;
    procedure TearDown; override;
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
  published
    procedure TestQueueing;
    procedure TestDequeueingEvents;
  end;

var
  DataModule2: TDataModule2 = nil;

implementation

uses
  BoldQueue,
  BoldThreadSafeQueue,
  BoldGuard;

{$R *.DFM}

procedure EnsureDataModule;
begin
  Ensuredm_Model;
  if not Assigned(DataModule2) then
  begin
    Application.Initialize;
    DataModule2 := TDataModule2.Create(Application);
    datamodule2.BoldDatabaseAdapterUniDAC1.CreateDatabase;
    DataModule2.BoldPersistenceHandleDB1.CreateDataBaseSchema;
  end;
end;

procedure FreeDatamodule;
begin
  FreeANdNil(Datamodule2);
end;

{ TListenerTest }

class procedure TKalaListenerTest.Suit(ASuite: TBoldTestSuite);
begin
  inherited;
  ASuite.AddTest(CreateWithComment('TestQueueing'));
  ASuite.AddTest(CreateWithComment('TestDequeueingEvents'));
end;

procedure TKalaListenerTest.TestDequeueingEvents;
var
  aSong: TSong;
  aHitlist: THitlist;

  FakeEvents: Variant;
  COMListener: TBoldListenerCOM;
  anIBoldListener: IBoldListener;
  aQueue: TBoldThreadSafeStringQueue;
  G: IBoldGuard;
begin
  G := TBoldGuard.Create(aQueue);
  DataModule2.BoldSystemHandle1.Active := True;
  aSong := TSong.Create(DataModule2.BoldSystemHandle1.System);
  aHitlist := THitlist.Create(DataModule2.BoldSystemHandle1.System);

  aSong.Title := 'Enter sandman';
  aHitlist.Name := 'All times metal';
  aSong.hitList.Add(aHitlist);
  DataModule2.BoldSystemHandle1.UpdateDatabase;

  DataModule2.ClassChangedCalled := False;
  DataModule2.EmeddedStateChangedCalled := False;
  DataModule2.NonembeddedStateChangedCalled := False;

  FakeEvents := VarArrayCreate([1, 4], varOleStr);
  FakeEvents[1] := 'C:Song';
  FakeEvents[2] := 'E:' + aSong.BoldObjectLocator.BoldObjectID.AsString;
  FakeEvents[3] := 'I:hitList:' + aSong.BoldObjectLocator.BoldObjectID.AsString;
  FakeEvents[4] := 'L:1.r1';

  aQueue := TBoldThreadSafeStringQueue.Create('Testcase/?');
  COMListener := TBoldListenerCOM.Create(aQueue, 100, 2000, 80, false, 'Test Project');
  DataModule2.BoldExternalObjectSpaceEventHandler1.Queue := COMListener.Queue;
  anIBoldListener := CreateAndSendTestEvents(COMListener, FakeEvents);
  DataModule2.BoldExternalObjectSpaceEventHandler1.QueueNotEmpty;

  BoldInstalledQueue.PerformPreDisplayQueue;

  Assert(Datamodule2.ClassChangedCalled, 'Class change ');
  Assert(Datamodule2.EmeddedStateChangedCalled, 'Error in class change');
  Assert(Datamodule2.NonembeddedStateChangedCalled, 'Error in class change');
  Assert((Datamodule2.LockLostEvent) and (Datamodule2.LockName = '1.r1'), 'LockLost event failed');
  anIBoldListener := nil;
end;

function TKalaListenerTest.CreateAndSendTestEvents(
  aListener: TBoldListenerCOM; var FakeEvents: Variant): IBoldListener;
var
  anIBoldListener: IBoldListener;
begin
  if VarIsEmpty(FakeEvents) then
  begin
    FakeEvents := VarArrayCreate([1, 3], varOleStr);
    FakeEvents[1] := 'C:Song';
    FakeEvents[2] := 'E:1';
    FakeEvents[3] := 'I:hitList:23';
  end;

  // Create the COM Listener and send the event array.
  Result := (aListener as IBoldListener);
  Result.ReceiveEvents(FakeEvents);

  anIBoldListener := nil;
end;

procedure TKalaListenerTest.TestQueueing;
var
  FakeEvents: Variant;
  i: Integer;
  COMListener: TBoldListenerCOM;
  anIBoldListener: IBoldListener;
  aQueue: TBoldThreadSafeStringQueue;
  G: IBoldGuard;
begin
  G := TBoldGuard.Create(aQueue);
  aQueue := TBoldThreadSafeStringQueue.Create('Testcase/?');
  COMListener := TBoldListenerCOM.Create(aQueue, 100, 2000, 80, false, 'Test Project');
  anIBoldListener := CreateAndSendTestEvents(COMListener, FakeEvents);
  // Dequeue the events and make sure that they are the same.
  for i := 1 to VarArrayHighBound(FakeEvents, 1) do
    Assert(COMListener.Queue.Dequeue = FakeEvents[i], Format('Faild event %s', [IntToStr(i)]));

  anIBoldListener := nil;
end;

procedure TDataModule2.BoldExternalObjectSpaceEventHandler1ClassChanged(TheClass: TBoldObjectList);
begin
  ClassChangedCalled := True;
end;

procedure TDataModule2.BoldExternalObjectSpaceEventHandler1EmbeddedStateChanged(
  BoldObject: TBoldObject);
begin
  EmeddedStateChangedCalled := True;
end;

procedure TDataModule2.BoldExternalObjectSpaceEventHandler1NonEmbeddedStateChanged(
  BoldMember: TBoldMember);
begin
  NonembeddedStateChangedCalled := True;
end;

procedure TKalaListenerTest.SetUp;
begin
  inherited;
  EnsureDataModule;
  DataModule2.BoldSystemHandle1.Active := True;
end;

procedure TDataModule2.BoldExternalObjectSpaceEventHandler1LockLost(
  LockName: String);
begin
  LockLostEvent := True;
  self.LockName := LockName;
end;

procedure TKalaListenerTest.TearDown;
begin
  FreeDataModule;
end;

class function TKalaListenerTest.Suite: ITestSuite;
begin
  result := inherited suite;
  SetCommentForTest(result, 'TestQueueing', '');
  SetCommentForTest(result, 'TestDequeueingEvents', '');
end;

initialization
  TestGlobal.RegisterTestCase(TKalaListenerTest);
  TBoldListenerComFactory.Create(ComServer);

finalization
  TestGlobal.UnRegisterTestCase(TKalaListenerTest);

end.
