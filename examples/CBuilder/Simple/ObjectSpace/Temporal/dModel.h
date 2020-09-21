//---------------------------------------------------------------------------

#ifndef dModelH
#define dModelH
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
#include "BoldPersistenceHandleBDE.hpp"
#include "BoldPersistenceHandleDB.hpp"
#include "BoldSubscription.hpp"
#include "BoldSystemHandle.hpp"
#include "BoldUMLModelLink.hpp"
#include "BoldUMLRose98Link.hpp"
#include "BoldAbstractDatabaseAdapter.hpp"
#include "BoldAbstractPersistenceHandleDB.hpp"
#include "BoldDatabaseAdapterIB.hpp"
#include <DB.hpp>
#include <IBDatabase.hpp>
//---------------------------------------------------------------------------
class TdmModel : public TDataModule
{
__published:	// IDE-managed Components
  TBoldModel *BoldModel1;
  TBoldSystemTypeInfoHandle *BoldSystemTypeInfoHandle1;
  TBoldSystemHandle *BoldSystemHandle1;
        TBoldPersistenceHandleDB *BoldPersistenceHandleDB1;
        TBoldDatabaseAdapterIB *BoldDatabaseAdapterIB1;
        TIBDatabase *IBDatabase1;
        TBoldUMLRoseLink *BoldUMLRoseLink1;
private:	// User declarations
public:		// User declarations
  __fastcall TdmModel(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TdmModel *dmModel;
//---------------------------------------------------------------------------
#endif
