//---------------------------------------------------------------------------

#ifndef dmMainH
#define dmMainH

#include "dModel.h"
#include "MainForm.h"
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "BoldAbstractComClientPersistenceHandles.hpp"
#include "BoldAbstractDequeuer.hpp"
#include "BoldAbstractLockManagerHandle.hpp"
#include "BoldAbstractPropagatorHandle.hpp"
#include "BoldClientHandles.hpp"
#include "BoldComClientHandles.hpp"
#include "BoldExternalObjectSpaceEventHandler.hpp"
#include "BoldHandle.hpp"
#include "BoldHandles.hpp"
#include "BoldIDAdderHandle.hpp"
#include "BoldListenerHandle.hpp"
#include "BoldLockingHandles.hpp"
#include "BoldLockManagerHandleCom.hpp"
#include "BoldPersistenceHandle.hpp"
#include "BoldPersistenceHandlePassthrough.hpp"
#include "BoldPropagatorHandleCOM.hpp"
#include "BoldSOAPClientPersistenceHandles.hpp"
#include "BoldSubscription.hpp"
#include "BoldSystemHandle.hpp"
//---------------------------------------------------------------------------
class TdmMainLocking : public TDataModule
{
__published:	// IDE-managed Components
  TBoldSystemHandle *bshLocking;
  TBoldSystemTypeInfoHandle *bstihLocking;
  TBoldLockingHandle *BoldLockingHandle1;
  TBoldLockManagerHandleCom *BoldLockManagerHandleCom1;
  TBoldComConnectionHandle *bcchPropagatorServer;
  TBoldListenerHandle *BoldListenerHandle1;
  TBoldExternalObjectSpaceEventHandler *BoldExternalObjectSpaceEventHandler1;
  TBoldIdAdderHandle *BoldIdAdderHandle1;
  TBoldSOAPClientPersistenceHandle *BoldSOAPClientPersistenceHandle1;
  TBoldComConnectionHandle *bcchPersistence;
  TBoldPropagatorHandleCOM *BoldPropagatorHandleCOM1;
  void __fastcall bshLockingPreUpdate(TObject *Sender);
  void __fastcall DataModuleCreate(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TdmMainLocking(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TdmMainLocking *dmMainLocking;
//---------------------------------------------------------------------------
#endif
 