unit MainDM;

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
  BoldHandle,
  BoldPersistenceHandle,
  BoldAbstractModel,
  BoldModel,
  BoldHandles,
  BoldSubscription,
  BoldSystemHandle,
  BoldListenerHandle,
  BoldClientHandles,
  BoldComClientHandles,
  BoldAbstractLockManagerHandle,
  BoldLockManagerHandleCom,
  BoldLockingHandles,
  ModelDM,
  BoldAbstractDequeuer,
  BoldExternalObjectSpaceEventHandler,
  BoldSOAPClientPersistenceHandles,
  BoldSnooperHandle,
  BoldPersistenceHandlePassthrough,
  BoldIDAdderHandle,
  BoldRegionDefinitions,
  BoldLockRegions,
  BoldAbstractPropagatorHandle,
  BoldPropagatorHandleCOM,
  BoldAbstractComClientPersistenceHandles,
  BoldListenerCOM;

type
  TdmMain = class(TDataModule)
    bshLocking: TBoldSystemHandle;
    bstihLocking: TBoldSystemTypeInfoHandle;
    BoldLockingHandle1: TBoldLockingHandle;
    BoldLockManagerHandleCom1: TBoldLockManagerHandleCom;
    bcchPropagatorServer: TBoldComConnectionHandle;
    BoldListenerHandle1: TBoldListenerHandle;
    BoldExternalObjectSpaceEventHandler1: TBoldExternalObjectSpaceEventHandler;
    BoldIdAdderHandle1: TBoldIdAdderHandle;
    BoldSOAPClientPersistenceHandle1: TBoldSOAPClientPersistenceHandle;
    bcchPersistence: TBoldComConnectionHandle;
    BoldPropagatorHandleCOM1: TBoldPropagatorHandleCOM;
    procedure bshLockingPreUpdate(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dmMain: TdmMain;

implementation

uses
  BoldExceptionHandlers,
  BoldSystemRT,
  MainForm,
  comserv;

{$R *.DFM}

{
The region definitions in this example looks like this:

Region1[Item]: Name, Price, | Region1[Colour]
Region1[Colour]: Name
Region1[PurchaseOrder]: OrderNo |Region1[OrderLine]
Region1[OrderLine]: quantity, PurchaseOrder

They are defined in the UMLModel in the tagged value "Bold.RegionDefinitions".
This model does not use default region
}


procedure TdmMain.bshLockingPreUpdate(Sender: TObject);
begin
  if form1.cbPessimisticLocking.Checked then
  begin
    if not BoldLockingHandle1.LockHolder.LockDatabase then
      raise Exception.Create('Cannot get database lock');
    ShowMessage('Got exclusive database lock. Press OK to update.');
  end;
end;

procedure TdmMain.DataModuleCreate(Sender: TObject);
begin
  if assigned(bstihLocking.RegionDefinitions) then
    sender := sender;
end;


initialization
  TBoldListenerCOMFactory.Create(Comserver);
end.
