program BoldPropagatorService;

uses
  SvcMgr,
  Sysutils,
  BoldSubscriptionHandler in 'Source\Propagator\Enterprise\BoldSubscriptionHandler.pas',
  BoldAdvancedPropagator in 'Source\Propagator\Enterprise\BoldAdvancedPropagator.pas',
  BoldClientHandler in 'Source\Propagator\Enterprise\BoldClientHandler.pas',
  BoldClientNotifierHandler in 'Source\Propagator\Enterprise\BoldClientNotifierHandler.pas',
  BoldClientQueue in 'Source\Propagator\Enterprise\BoldClientQueue.pas',
  BoldEnqueuer in 'Source\Propagator\Enterprise\BoldEnqueuer.pas',
  BoldIndexedList in 'Source\Propagator\Enterprise\BoldIndexedList.pas',
  BoldListNodes in 'Source\Propagator\Enterprise\BoldListNodes.pas',
  BoldOutputQueueHandler in 'Source\Propagator\Enterprise\BoldOutputQueueHandler.pas',
  BoldPriorityListEnlister in 'Source\Propagator\Enterprise\BoldPriorityListEnlister.pas',
  BoldPropagatorCleanup in 'Source\Propagator\Enterprise\BoldPropagatorCleanup.pas',
  BoldPropagatorMainForm in 'Source\Propagator\Enterprise\BoldPropagatorMainForm.pas' {PropagatorMainForm},
  BoldPropagatorSubscriptions in 'Source\Propagator\Enterprise\BoldPropagatorSubscriptions.pas',
  BoldPropagatorUIManager in 'Source\Propagator\Enterprise\BoldPropagatorUIManager.pas',
  BoldAbstractOutputQueueHandler in 'Source\Propagator\Enterprise\BoldAbstractOutputQueueHandler.pas',
  BoldServicePropagatorUnit in 'Source\Propagator\Enterprise\BoldServicePropagatorUnit.pas' {BoldPropagatorSvc: TService},
  BoldComServiceRegister in 'Source\Propagator\Enterprise\BoldComServiceRegister.pas',
  BoldLockList in 'Source\Propagator\Enterprise\BoldLockList.pas',
  BoldLockManager in 'Source\Propagator\Enterprise\BoldLockManager.pas',
  BoldLockManagerAdmin in 'Source\Propagator\Enterprise\BoldLockManagerAdmin.pas',
  BoldLockManagerAdminCOM in 'Source\Propagator\Enterprise\BoldLockManagerAdminCOM.pas',
  BoldLockManagerCOM in 'Source\Propagator\Enterprise\BoldLockManagerCOM.pas',
  BoldServerHandlesDataMod in 'Source\Propagator\Enterprise\BoldServerHandlesDataMod.pas' {dmServerHandles: TDataModule},
  BoldPropagatorServer in 'Source\Propagator\Enterprise\BoldPropagatorServer.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TBoldPropagatorSvc, BoldPropagatorSvc);
  Application.Run;
end.
