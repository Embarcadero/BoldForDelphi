library BoldPropagatorDll;

uses
  ComServ,
  BoldSubscriptionHandler in 'Source\Propagator\Enterprise\BoldSubscriptionHandler.pas',
  BoldAdvancedPropagator in 'Source\Propagator\Enterprise\BoldAdvancedPropagator.pas',
  BoldClientHandler in 'Source\Propagator\Enterprise\BoldClientHandler.pas',
  BoldClientNotifierHandler in 'Source\Propagator\Enterprise\BoldClientNotifierHandler.pas',
  BoldClientQueue in 'Source\Propagator\Enterprise\BoldClientQueue.pas',
  BoldEnqueuer in 'Source\Propagator\Enterprise\BoldEnqueuer.pas',
  BoldIndexedList in 'Source\Propagator\Enterprise\BoldIndexedList.pas',
  BoldIndexList in 'Source\Propagator\Enterprise\BoldIndexList.pas',
  BoldListNodes in 'Source\Propagator\Enterprise\BoldListNodes.pas',
  BoldOutputQueueHandler in 'Source\Propagator\Enterprise\BoldOutputQueueHandler.pas',
  BoldPriorityListEnlister in 'Source\Propagator\Enterprise\BoldPriorityListEnlister.pas',
  BoldPropagatorApplication in 'Source\Propagator\Enterprise\BoldPropagatorApplication.pas',
  BoldPropagatorServer in 'Source\Propagator\Enterprise\BoldPropagatorServer.pas',
  BoldPropagatorCleanup in 'Source\Propagator\Enterprise\BoldPropagatorCleanup.pas',
  BoldPropagatorMainForm in 'Source\Propagator\Enterprise\BoldPropagatorMainForm.pas' {PropagatorMainForm},
  BoldPropagatorSubscriptions in 'Source\Propagator\Enterprise\BoldPropagatorSubscriptions.pas',
  BoldPropagatorUIManager in 'Source\Propagator\Enterprise\BoldPropagatorUIManager.pas',
  BoldAbstractOutputQueueHandler in 'Source\Propagator\Enterprise\BoldAbstractOutputQueueHandler.pas',
  BoldLockingSupportInterfaces_TLB in '..\Common\BoldLockingSupportInterfaces_TLB.pas',
  BoldClientHandlerCOM in 'Source\Propagator\Enterprise\BoldClientHandlerCOM.pas',
  BoldEnqueuerCOM in 'Source\Propagator\Enterprise\BoldEnqueuerCOM.pas',
  BoldAdvancedPropagatorCOM in 'Source\Propagator\Enterprise\BoldAdvancedPropagatorCOM.pas',
  BoldLockManagerCOM in 'Source\Propagator\Enterprise\BoldLockManagerCOM.pas',
  BoldLockManager in 'Source\Propagator\Enterprise\BoldLockManager.pas',
  BoldLockManagerAdmin in 'Source\Propagator\Enterprise\BoldLockManagerAdmin.pas',
  BoldLockManagerAdminCOM in 'Source\Propagator\Enterprise\BoldLockManagerAdminCOM.pas',
  BoldLockList in 'Source\Propagator\Enterprise\BoldLockList.pas',
  BoldServerHandlesDataMod in 'Source\Propagator\Enterprise\BoldServerHandlesDataMod.pas' {dmServerHandles: TDataModule},
  BoldLockManagerExportHandle in 'Source\Propagator\Enterprise\BoldLockManagerExportHandle.pas',
  BoldLockManagerAdminExportHandle in 'Source\Propagator\Enterprise\BoldLockManagerAdminExportHandle.pas',
  BoldClientHandlerExportHandle in 'Source\Propagator\Enterprise\BoldClientHandlerExportHandle.pas',
  BoldEnqueuerExportHandle in 'Source\Propagator\Enterprise\BoldEnqueuerExportHandle.pas';

exports
  DllGetClassObject,
  _DllCanUnloadNow name 'DllCanUnloadNow',
  DllRegisterServer,
  DllUnregisterServer;

{$R *.RES}

begin
end.
