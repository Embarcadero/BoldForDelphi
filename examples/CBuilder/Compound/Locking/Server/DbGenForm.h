//---------------------------------------------------------------------------

#ifndef DbGenFormH
#define DbGenFormH

#include "dModel.h"
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
#include "BoldAbstractDatabaseAdapter.hpp"
#include "BoldAbstractPersistenceHandleDB.hpp"
#include "BoldDatabaseAdapterIB.hpp"
#include <DB.hpp>
#include <IBDatabase.hpp>
//---------------------------------------------------------------------------
class TfrmDBGen : public TForm
{
__published:	// IDE-managed Components
  TBoldPersistenceHandleDB *BoldPersistenceHandleDB1;
  TBoldDatabaseAdapterIB *BoldDatabaseAdapterIB1;
  TIBDatabase *IBDatabase1;
  TButton *Button1;
  void __fastcall Button1Click(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TfrmDBGen(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmDBGen *frmDBGen;
//---------------------------------------------------------------------------
#endif
