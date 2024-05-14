library BoldPropagatorDll;

uses
  ComServ,
  BoldSubscriptionHandler in '..\BoldSubscriptionHandler.pas',
  BoldAdvancedPropagator in 'BoldAdvancedPropagator.pas',
  BoldClientHandler in '..\BoldClientHandler.pas',
  BoldClientNotifierHandler in '..\BoldClientNotifierHandler.pas',
  BoldClientQueue in '..\BoldClientQueue.pas',
  BoldEnqueuer in '..\BoldEnqueuer.pas',
  BoldIndexedList in '..\BoldIndexedList.pas',
  BoldIndexList in '..\BoldIndexList.pas',
  BoldListNodes in '..\BoldListNodes.pas',
  BoldOutputQueueHandler in '..\BoldOutputQueueHandler.pas',
  BoldPriorityListEnlister in '..\BoldPriorityListEnlister.pas',
  BoldPropagatorApplication in 'BoldPropagatorApplication.pas',
  BoldPropagatorServer in 'BoldPropagatorServer.pas',
  BoldPropagatorCleanup in 'BoldPropagatorCleanup.pas',
  BoldPropagatorMainForm in 'BoldPropagatorMainForm.pas' {PropagatorMainForm},
  BoldPropagatorSubscriptions in 'BoldPropagatorSubscriptions.pas',
  BoldPropagatorUIManager in 'BoldPropagatorUIManager.pas',
  BoldAbstractOutputQueueHandler in '..\BoldAbstractOutputQueueHandler.pas',
  BoldLockingSupportInterfaces_TLB in '..\BoldLockingSupportInterfaces_TLB.pas',
  BoldClientHandlerCOM in 'BoldClientHandlerCOM.pas',
  BoldEnqueuerCOM in 'BoldEnqueuerCOM.pas',
  BoldAdvancedPropagatorCOM in '..\BoldAdvancedPropagatorCOM.pas',
  BoldLockManagerCOM in '..\BoldLockManagerCOM.pas',
  BoldLockManager in '..\BoldLockManager.pas',
  BoldLockManagerAdmin in '..\BoldLockManagerAdmin.pas',
  BoldLockManagerAdminCOM in '..\BoldLockManagerAdminCOM.pas',
  BoldLockList in '..\BoldLockList.pas',
  BoldServerHandlesDataMod in '..\BoldServerHandlesDataMod.pas' {dmServerHandles: TDataModule},
  BoldLockManagerExportHandle in '..\BoldLockManagerExportHandle.pas',
  BoldLockManagerAdminExportHandle in '..\BoldLockManagerAdminExportHandle.pas',
  BoldClientHandlerExportHandle in '..\BoldClientHandlerExportHandle.pas',
  BoldEnqueuerExportHandle in '..\BoldEnqueuerExportHandle.pas';

exports
  DllGetClassObject,
  _DllCanUnloadNow name 'DllCanUnloadNow',
  DllRegisterServer,
  DllUnregisterServer;

{$R *.RES}

begin
end.
