//---------------------------------------------------------------------------

#ifndef datamodH
#define datamodH
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
#include "BoldCheckboxStateControlPack.hpp"
#include "BoldControlPack.hpp"
#include "BoldElements.hpp"
#include "BoldStringControlPack.hpp"
#include "BoldFilteredHandle.hpp"
#include "BoldSortedHandle.hpp"
#include "BoldAbstractDatabaseAdapter.hpp"
#include "BoldAbstractPersistenceHandleDB.hpp"
#include "BoldDatabaseAdapterIB.hpp"
#include <DB.hpp>
#include <IBDatabase.hpp>
//---------------------------------------------------------------------------
class TDataModule1 : public TDataModule
{
__published:	// IDE-managed Components
        TBoldModel *BoldModel1;
        TBoldSystemTypeInfoHandle *BoldSystemTypeInfoHandle1;
        TBoldSystemHandle *BoldSystemHandle1;
        TBoldAsCheckBoxStateRenderer *IsRichRenderer;
  TBoldAsStringRenderer *FullNameRenderer;
  TBoldAsStringRenderer *NegativeRedRenderer;
  TBoldFilter *IsRichFilter;
  TBoldComparer *NameComparer;
        TBoldPersistenceHandleDB *BoldPersistenceHandleDB1;
        TBoldDatabaseAdapterIB *BoldDatabaseAdapterIB1;
        TIBDatabase *IBDatabase1;
        TBoldUMLRoseLink *BoldUMLRoseLink1;
        TCheckBoxState __fastcall IsRichRendererGetAsCheckBoxState(
          TBoldElement *Element, int Representation,
          AnsiString Expression);
        void __fastcall IsRichRendererSubscribe(TBoldElement *Element,
          int Representation, AnsiString Expression,
          TBoldSubscriber *Subscriber);
  AnsiString __fastcall FullNameRendererGetAsString(TBoldElement *Element,
          int Representation, AnsiString Expression);
  void __fastcall FullNameRendererSubscribe(TBoldElement *Element,
          int Representation, AnsiString Expression,
          TBoldSubscriber *Subscriber);
  void __fastcall NegativeRedRendererHoldsChangedValue(
          TBoldElement *Element, int Representation, AnsiString Expression,
          TBoldSubscriber *Subscriber);
  void __fastcall NegativeRedRendererSetFont(TBoldElement *Element,
          TFont *AFont, int Representation, AnsiString Expression);
  void __fastcall IsRichFilterSubscribe(TBoldElement *Element,
          TBoldSubscriber *Subscriber);
  int __fastcall NameComparerCompare(TBoldElement *Item1,
          TBoldElement *Item2);
  void __fastcall NameComparerSubscribe(TBoldElement *Element,
          TBoldSubscriber *Subscriber);
  bool __fastcall IsRichFilterFilter(TBoldElement *Element);
private:	// User declarations
public:		// User declarations
        __fastcall TDataModule1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TDataModule1 *DataModule1;
//---------------------------------------------------------------------------
#endif
