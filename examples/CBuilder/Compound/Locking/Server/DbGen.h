//---------------------------------------------------------------------------

#ifndef DbGenH
#define DbGenH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "BoldHandle.hpp"
#include "BoldHandles.hpp"
#include "BoldIBAliasCreator.hpp"
#include "BoldPersistenceHandle.hpp"
#include "BoldPersistenceHandleBDE.hpp"
#include "BoldPersistenceHandleDB.hpp"
#include "BoldSubscription.hpp"
#include "BoldSystemHandle.hpp"
//---------------------------------------------------------------------------
class TfrmDBGen : public TForm
{
__published:	// IDE-managed Components
  TBoldIBAliasCreator *BoldIBAliasCreator1;
  TBoldSystemHandle *BoldSystemHandle1;
  TBoldPersistenceHandleBDE *BoldPersistenceHandleBDE1;
private:	// User declarations
public:		// User declarations
  __fastcall TfrmDBGen(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmDBGen *frmDBGen;
//---------------------------------------------------------------------------
#endif
