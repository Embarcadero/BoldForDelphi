//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "dmMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "BoldAbstractComClientPersistenceHandles"
#pragma link "BoldAbstractDequeuer"
#pragma link "BoldAbstractLockManagerHandle"
#pragma link "BoldAbstractPropagatorHandle"
#pragma link "BoldClientHandles"
#pragma link "BoldComClientHandles"
#pragma link "BoldExternalObjectSpaceEventHandler"
#pragma link "BoldHandle"
#pragma link "BoldHandles"
#pragma link "BoldIDAdderHandle"
#pragma link "BoldListenerHandle"
#pragma link "BoldLockingHandles"
#pragma link "BoldLockManagerHandleCom"
#pragma link "BoldPersistenceHandle"
#pragma link "BoldPersistenceHandlePassthrough"
#pragma link "BoldPropagatorHandleCOM"
#pragma link "BoldSOAPClientPersistenceHandles"
#pragma link "BoldSubscription"
#pragma link "BoldSystemHandle"
#pragma resource "*.dfm"
TdmMainLocking *dmMainLocking;
//---------------------------------------------------------------------------
__fastcall TdmMainLocking::TdmMainLocking(TComponent* Owner)
  : TDataModule(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TdmMainLocking::bshLockingPreUpdate(TObject *Sender)
{
  if (Form1->cbPessimisticLocking->Checked)
  {
    if (!BoldLockingHandle1->LockHolder->LockDatabase())
      throw new Exception("Cannot get database lock");
    ShowMessage("Got exclusive database lock. Press OK to update.");
  }
}
//---------------------------------------------------------------------------
void __fastcall TdmMainLocking::DataModuleCreate(TObject *Sender)
{
  if (bstihLocking->RegionDefinitions)
    Sender = Sender;
}
//---------------------------------------------------------------------------
