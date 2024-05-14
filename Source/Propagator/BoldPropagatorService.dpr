program BoldPropagatorService;

uses
  SvcMgr,
  Sysutils,
  BoldSubscriptionHandler in '..\BoldSubscriptionHandler.pas',
  BoldAdvancedPropagator in 'BoldAdvancedPropagator.pas',
  BoldClientHandler in '..\BoldClientHandler.pas',
  BoldClientNotifierHandler in '..\BoldClientNotifierHandler.pas',
  BoldClientQueue in '..\BoldClientQueue.pas',
  BoldEnqueuer in '..\BoldEnqueuer.pas',
  BoldIndexedList in '..\BoldIndexedList.pas',
  BoldListNodes in '..\BoldListNodes.pas',
  BoldOutputQueueHandler in '..\BoldOutputQueueHandler.pas',
  BoldPriorityListEnlister in '..\BoldPriorityListEnlister.pas',
  BoldPropagatorCleanup in 'BoldPropagatorCleanup.pas',
  BoldPropagatorMainForm in 'BoldPropagatorMainForm.pas' {PropagatorMainForm},
  BoldPropagatorSubscriptions in 'BoldPropagatorSubscriptions.pas',
  BoldPropagatorUIManager in 'BoldPropagatorUIManager.pas',
  BoldAbstractOutputQueueHandler in '..\BoldAbstractOutputQueueHandler.pas',
  BoldServicePropagatorUnit in '..\BoldServicePropagatorUnit.pas' {BoldPropagatorSvc: TService},
  BoldComServiceRegister in '..\BoldComServiceRegister.pas',
  BoldLockList in '..\BoldLockList.pas',
  BoldLockManager in '..\BoldLockManager.pas',
  BoldLockManagerAdmin in '..\BoldLockManagerAdmin.pas',
  BoldLockManagerAdminCOM in '..\BoldLockManagerAdminCOM.pas',
  BoldLockManagerCOM in '..\BoldLockManagerCOM.pas',
  BoldServerHandlesDataMod in '..\BoldServerHandlesDataMod.pas' {dmServerHandles: TDataModule},
  BoldPropagatorServer in 'BoldPropagatorServer.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TBoldPropagatorSvc, BoldPropagatorSvc);
  Application.Run;
end.
