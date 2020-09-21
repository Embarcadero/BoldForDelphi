program BoldPropagator;

uses
  Forms,
  BoldEnqueuer in 'BoldEnqueuer.pas',
  BoldClientHandler in 'BoldClientHandler.pas',
  BoldPropagatorMainForm in 'BoldPropagatorMainForm.pas' {PropagatorMainForm},
  BoldListNodes in 'BoldListNodes.pas',
  BoldIndexedList in 'BoldIndexedList.pas',
  BoldPropagatorSubscriptions in 'BoldPropagatorSubscriptions.pas',
  BoldPropagatorGUIDs in '..\Common\BoldPropagatorGUIDs.pas',
  BoldSubscriptionHandler in 'BoldSubscriptionHandler.pas',
  BoldAbstractOutputQueueHandler in 'BoldAbstractOutputQueueHandler.pas',
  BoldOutputQueueHandler in 'BoldOutputQueueHandler.pas',
  BoldPriorityListEnlister in 'BoldPriorityListEnlister.pas',
  BoldClientNotifierHandler in 'BoldClientNotifierHandler.pas',
  BoldClientQueue in 'BoldClientQueue.pas',
  BoldAdvancedPropagator in 'BoldAdvancedPropagator.pas',
  BoldPropagatorCleanup in 'BoldPropagatorCleanup.pas',
  BoldPropagatorInterfaces_TLB in '..\Common\BoldPropagatorInterfaces_TLB.pas',
  BoldPropagatorUIManager in 'BoldPropagatorUIManager.pas',
  BoldPropagatorApplication in 'BoldPropagatorApplication.pas',
  BoldLockManager in 'BoldLockManager.pas',
  BoldLockManagerAdmin in 'BoldLockManagerAdmin.pas',
  BoldLockManagerCOM in 'BoldLockManagerCOM.pas',
  BoldThreadedComObjectFactory in '..\..\Common\COM\BoldThreadedComObjectFactory.pas',
  BoldLockManagerAdminCOM in 'BoldLockManagerAdminCOM.pas',
  BoldLockList in 'BoldLockList.pas',
  BoldServerHandlesDataMod in 'BoldServerHandlesDataMod.pas' {dmServerHandles: TDataModule},
  BoldLockManagerExportHandle in 'BoldLockManagerExportHandle.pas',
  BoldLockManagerAdminExportHandle in 'BoldLockManagerAdminExportHandle.pas',
  BoldAdvancedPropagatorCOM in 'BoldAdvancedPropagatorCOM.pas',
  BoldPropagatorServer in 'BoldPropagatorServer.pas',
  BoldClientHandlerExportHandle in 'BoldClientHandlerExportHandle.pas',
  BoldEnqueuerExportHandle in 'BoldEnqueuerExportHandle.pas',
  BoldClientHandlerCOM in 'BoldClientHandlerCOM.pas',
  BoldEnqueuerCOM in 'BoldEnqueuerCOM.pas',
  BoldApartmentThread in '..\..\Common\COM\BoldApartmentThread.pas',
  BoldObjectMarshaler in '..\Common\BoldObjectMarshaler.pas',
  PropagatorConsts in '..\Common\PropagatorConsts.pas';

{$R ..\common\BoldPropagatorInterfaces.tlb}

{$R *.RES}

begin
  Application.Initialize;
  Application.Run;
end.
