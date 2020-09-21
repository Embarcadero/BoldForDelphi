//---------------------------------------------------------------------------

#ifndef PServerDMH
#define PServerDMH

#include "dModel.h"
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "BoldAbstractLockManagerHandle.hpp"
#include "BoldAbstractPropagatorHandle.hpp"
#include "BoldClientHandles.hpp"
#include "BoldComClientHandles.hpp"
#include "BoldComServerHandles.hpp"
#include "BoldHandle.hpp"
#include "BoldLockManagerHandleCom.hpp"
#include "BoldPersistenceHandle.hpp"
#include "BoldPersistenceHandleDB.hpp"
#include "BoldPersistenceHandlePassthrough.hpp"
#include "BoldPropagatorHandleCOM.hpp"
#include "BoldServerHandles.hpp"
#include "BoldSnooperHandle.hpp"
#include "BoldSOAPServerPersistenceHandles.hpp"
#include "BoldSubscription.hpp"
#include "BoldAbstractDatabaseAdapter.hpp"
#include "BoldAbstractPersistenceHandleDB.hpp"
#include "BoldDatabaseAdapterIB.hpp"
#include "BoldPersistenceHandlePTWithModel.hpp"
#include <DB.hpp>
#include <IBDatabase.hpp>
//---------------------------------------------------------------------------
class TdmPServer : public TDataModule
{
__published:	// IDE-managed Components
  TBoldSOAPServerPersistenceHandle *BoldSOAPServerPersistenceHandle1;
  TBoldComServerHandle *BoldComServerHandle1;
  TBoldSnooperHandle *BoldSnooperHandle1;
  TBoldPropagatorHandleCOM *BoldPropagatorHandleCOM1;
  TBoldLockManagerHandleCom *BoldLockManagerHandleCom1;
  TBoldComConnectionHandle *BoldComConnectionHandle1;
  TBoldPersistenceHandleDB *BoldPersistenceHandleDB1;
  TBoldDatabaseAdapterIB *BoldDatabaseAdapterIB1;
  TIBDatabase *IBDatabase1;
private:	// User declarations
protected:
void __fastcall Loaded(void);
public:		// User declarations
  __fastcall TdmPServer(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TdmPServer *dmPServer;
//---------------------------------------------------------------------------
#endif
