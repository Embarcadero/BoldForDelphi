//---------------------------------------------------------------------------

#ifndef dMainH
#define dMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "BoldAbstractModel.hpp"
#include "BoldHandle.hpp"
#include "BoldHandles.hpp"
#include "BoldModel.hpp"
#include "BoldPersistenceHandle.hpp"
#include "BoldPersistenceHandleDB.hpp"
#include "BoldSubscription.hpp"
#include "BoldSystemHandle.hpp"
#include "BoldUMLModelLink.hpp"
#include "BoldUMLRose98Link.hpp"
#include "BoldAbstractDatabaseAdapter.hpp"
#include "BoldAbstractPersistenceHandleDB.hpp"
#include "BoldActions.hpp"
#include "BoldDatabaseAdapterIB.hpp"
#include "BoldHandleAction.hpp"
#include "BoldIBDatabaseAction.hpp"
#include <ActnList.hpp>
#include <DB.hpp>
#include <IBDatabase.hpp>
//---------------------------------------------------------------------------
class TdmMain : public TDataModule
{
__published:	// IDE-managed Components
  TBoldSystemHandle *bshMain;
  TBoldSystemTypeInfoHandle *stiMain;
  TBoldModel *bmoMain;
        TBoldPersistenceHandleDB *BoldPersistenceHandleDB1;
        TBoldDatabaseAdapterIB *BoldDatabaseAdapterIB1;
        TIBDatabase *IBDatabase1;
        TBoldUMLRoseLink *BoldUMLRoseLink1;
        TActionList *ActionList1;
        TBoldActivateSystemAction *BoldActivateSystemAction1;
        TBoldUpdateDBAction *BoldUpdateDBAction1;
        TBoldIBDatabaseAction *BoldIBDatabaseAction1;
private:	// User declarations
public:		// User declarations
  __fastcall TdmMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TdmMain *dmMain;
//---------------------------------------------------------------------------
#endif
