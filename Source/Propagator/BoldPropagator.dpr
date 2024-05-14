program BoldPropagator;

uses
  VCL.Forms,
  BoldEnqueuer in 'Enterprise\BoldEnqueuer.pas',
  BoldClientHandler in 'Enterprise\BoldClientHandler.pas',
  BoldPropagatorMainForm in 'Enterprise\BoldPropagatorMainForm.pas' {PropagatorMainForm},
  BoldListNodes in 'Enterprise\BoldListNodes.pas',
  BoldIndexedList in 'Enterprise\BoldIndexedList.pas',
  BoldPropagatorSubscriptions in 'Enterprise\BoldPropagatorSubscriptions.pas',
  BoldPropagatorGUIDs in 'Common\BoldPropagatorGUIDs.pas',
  BoldSubscriptionHandler in 'Enterprise\BoldSubscriptionHandler.pas',
  BoldAbstractOutputQueueHandler in 'Enterprise\BoldAbstractOutputQueueHandler.pas',
  BoldOutputQueueHandler in 'Enterprise\BoldOutputQueueHandler.pas',
  BoldPriorityListEnlister in 'Enterprise\BoldPriorityListEnlister.pas',
  BoldClientNotifierHandler in 'Enterprise\BoldClientNotifierHandler.pas',
  BoldClientQueue in 'Enterprise\BoldClientQueue.pas',
  BoldAdvancedPropagator in 'Enterprise\BoldAdvancedPropagator.pas',
  BoldPropagatorCleanup in 'Enterprise\BoldPropagatorCleanup.pas',
  BoldPropagatorInterfaces_TLB in 'Common\BoldPropagatorInterfaces_TLB.pas',
  BoldPropagatorUIManager in 'Enterprise\BoldPropagatorUIManager.pas',
  BoldPropagatorApplication in 'Enterprise\BoldPropagatorApplication.pas',
  BoldLockManager in 'Enterprise\BoldLockManager.pas',
  BoldLockManagerAdmin in 'Enterprise\BoldLockManagerAdmin.pas',
  BoldLockManagerCOM in 'Enterprise\BoldLockManagerCOM.pas',
  BoldThreadedComObjectFactory in '..\Common\COM\BoldThreadedComObjectFactory.pas',
  BoldLockManagerAdminCOM in 'Enterprise\BoldLockManagerAdminCOM.pas',
  BoldLockList in 'Enterprise\BoldLockList.pas',
  BoldServerHandlesDataMod in 'Enterprise\BoldServerHandlesDataMod.pas' {dmServerHandles: TDataModule},
  BoldLockManagerExportHandle in 'Enterprise\BoldLockManagerExportHandle.pas',
  BoldLockManagerAdminExportHandle in 'Enterprise\BoldLockManagerAdminExportHandle.pas',
  BoldAdvancedPropagatorCOM in 'Enterprise\BoldAdvancedPropagatorCOM.pas',
  BoldPropagatorServer in 'Enterprise\BoldPropagatorServer.pas',
  BoldClientHandlerExportHandle in 'Enterprise\BoldClientHandlerExportHandle.pas',
  BoldEnqueuerExportHandle in 'Enterprise\BoldEnqueuerExportHandle.pas',
  BoldClientHandlerCOM in 'Enterprise\BoldClientHandlerCOM.pas',
  BoldEnqueuerCOM in 'Enterprise\BoldEnqueuerCOM.pas',
  BoldApartmentThread in '..\Common\COM\BoldApartmentThread.pas',
  BoldObjectMarshaler in 'Common\BoldObjectMarshaler.pas',
  PropagatorConsts in 'PropagatorConsts.pas';

{$R BoldPropagatorInterfaces.tlb}

{$R *.RES}

begin
  Application.Initialize;
  Application.Run;
end.
