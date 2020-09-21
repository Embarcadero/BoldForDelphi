//---------------------------------------------------------------------------

#ifndef dMainH
#define dMainH

#include "dMain.h"
#include "FfrmReport.h"
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "BoldHandle.hpp"
#include "BoldHandles.hpp"
#include "BoldPersistenceHandle.hpp"
#include "BoldPersistenceHandleDB.hpp"
#include "BoldSubscription.hpp"
#include "BoldSystemHandle.hpp"
#include <ActnList.hpp>
#include "BoldAbstractModel.hpp"
#include "BoldActions.hpp"
#include "BoldDBActions.hpp"
#include "BoldHandleAction.hpp"
#include "BoldModel.hpp"
#include "BoldUMLModelLink.hpp"
#include "BoldAbstractDatabaseAdapter.hpp"
#include "BoldAbstractPersistenceHandleDB.hpp"
#include "BoldDatabaseAdapterIB.hpp"
#include <DB.hpp>
#include <IBDatabase.hpp>
#include "BoldIBDatabaseAction.hpp"
//---------------------------------------------------------------------------
class TdmMain : public TDataModule
{
__published:	// IDE-managed Components
  TBoldSystemHandle *bshMain;
  TBoldSystemTypeInfoHandle *stiMain;
  TBoldModel *bmoMain;
  TActionList *ActionList1;
  TBoldActivateSystemAction *BoldActivateSystemAction1;
  TBoldUpdateDBAction *BoldUpdateDBAction1;
  TBoldModel *BoldModel1;
        TBoldPersistenceHandleDB *bphBDMain;
        TBoldDatabaseAdapterIB *bdbaMain;
        TIBDatabase *ibdMain;
        TBoldIBDatabaseAction *BoldIBDatabaseAction1;
private:	// User declarations
public:		// User declarations
  __fastcall TdmMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TdmMain *dmMain;
//---------------------------------------------------------------------------
#endif
