//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "PServerDM.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "BoldAbstractLockManagerHandle"
#pragma link "BoldAbstractPropagatorHandle"
#pragma link "BoldClientHandles"
#pragma link "BoldComClientHandles"
#pragma link "BoldComServerHandles"
#pragma link "BoldHandle"
#pragma link "BoldLockManagerHandleCom"
#pragma link "BoldPersistenceHandle"
#pragma link "BoldPersistenceHandleDB"
#pragma link "BoldPersistenceHandlePassthrough"
#pragma link "BoldPropagatorHandleCOM"
#pragma link "BoldServerHandles"
#pragma link "BoldSnooperHandle"
#pragma link "BoldSOAPServerPersistenceHandles"
#pragma link "BoldSubscription"
#pragma link "BoldAbstractDatabaseAdapter"
#pragma link "BoldAbstractPersistenceHandleDB"
#pragma link "BoldDatabaseAdapterIB"
#pragma link "BoldPersistenceHandlePTWithModel"
#pragma resource "*.dfm"
TdmPServer *dmPServer;
//---------------------------------------------------------------------------
__fastcall TdmPServer::TdmPServer(TComponent* Owner)
  : TDataModule(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TdmPServer::Loaded()
{
  TDataModule::Loaded();
  // Due to timing-problems, it is better to activate the persistencehandles here in Loaded instead of Datamodulecreate.
  // The first client to connect might try to retrieve the com-interface to the persistencehandle before DataModuleCreate.
  BoldPersistenceHandleBDE1->Active = true;
  BoldSOAPServerPersistenceHandle1->Active = true;
}
//---------------------------------------------------------------------------

