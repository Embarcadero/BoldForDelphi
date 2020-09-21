//---------------------------------------------------------------------------

#ifndef dPersistenceH
#define dPersistenceH

#include "dModel.h"
#include "dSystemTypeInfo.h"

//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "BoldAbstractObjectUpgraderHandle.hpp"
#include "BoldHandle.hpp"
#include "BoldObjectUpgraderHandle.hpp"
#include "BoldPersistenceHandle.hpp"
#include "BoldPersistenceHandleBDE.hpp"
#include "BoldPersistenceHandleDB.hpp"
#include "BoldSubscription.hpp"
//---------------------------------------------------------------------------
class TdmPersistence : public TDataModule
{
__published:	// IDE-managed Components
  TBoldPersistenceHandleBDE *PersistenceHandle;
  TBoldObjectUpgraderHandle *ObjectUpgrader;
private:	// User declarations
public:		// User declarations
  __fastcall TdmPersistence(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TdmPersistence *dmPersistence;
//---------------------------------------------------------------------------
#endif
