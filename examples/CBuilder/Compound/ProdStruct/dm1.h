//---------------------------------------------------------------------------

#ifndef dm1H
#define dm1H
#include "ProdStructClasses.hpp"
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "BoldAbstractModel.hpp"
#include "BoldControlPack.hpp"
#include "BoldElements.hpp"
#include "BoldHandle.hpp"
#include "BoldHandles.hpp"
#include "BoldModel.hpp"
#include "BoldPersistenceHandle.hpp"
#include "BoldPersistenceHandleBDE.hpp"
#include "BoldPersistenceHandleDB.hpp"
#include "BoldStringControlPack.hpp"
#include "BoldSubscription.hpp"
#include "BoldSystemHandle.hpp"
#include "BoldAbstractDatabaseAdapter.hpp"
#include "BoldAbstractPersistenceHandleDB.hpp"
#include "BoldDatabaseAdapterIB.hpp"
#include <DB.hpp>
#include <IBDatabase.hpp>
//---------------------------------------------------------------------------
class TdmMain : public TDataModule
{
__published:	// IDE-managed Components
  TBoldAsStringRenderer *BoldProfitAsStringRenderer;
  TBoldModel *BoldModel;
  TBoldSystemTypeInfoHandle *BoldSystemTypeInfoHandle1;
  TBoldSystemHandle *BoldSystem;
        TBoldPersistenceHandleDB *BoldPersistenceHandleDB1;
        TBoldDatabaseAdapterIB *BoldDatabaseAdapterIB1;
        TIBDatabase *IBDatabase1;
  AnsiString __fastcall BoldProfitAsStringRendererGetAsString(
          TBoldElement *Element, int Representation,
          AnsiString Expression);
  bool __fastcall BoldProfitAsStringRendererMayModify(
          TBoldElement *Element, int Representation, AnsiString Expression,
          TBoldSubscriber *Subscriber);
  void __fastcall BoldProfitAsStringRendererSetAsString(
          TBoldElement *Element, AnsiString NewValue, int Representation,
          AnsiString Expression);
  void __fastcall BoldProfitAsStringRendererSubscribe(
          TBoldElement *Element, int Representation, AnsiString Expression,
          TBoldSubscriber *Subscriber);
private:	// User declarations
  AnsiString __fastcall CleanValue(AnsiString NewValue);
public:		// User declarations
  __fastcall TdmMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TdmMain *dmMain;
//---------------------------------------------------------------------------
#endif
