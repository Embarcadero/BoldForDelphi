//---------------------------------------------------------------------------

#ifndef dModelH
#define dModelH
#include "BldOwnClasses.hpp"

//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "BoldAbstractDequeuer.hpp"
#include "BoldAbstractModel.hpp"
#include "BoldAbstractPropagatorHandle.hpp"
#include "BoldCheckboxStateControlPack.hpp"
#include "BoldClientHandles.hpp"
#include "BoldComClientHandles.hpp"
#include "BoldControlPack.hpp"
#include "BoldElements.hpp"
#include "BoldExternalObjectSpaceEventHandler.hpp"
#include "BoldFilteredHandle.hpp"
#include "BoldHandle.hpp"
#include "BoldHandles.hpp"
#include "BoldIDAdderHandle.hpp"
#include "BoldListenerHandle.hpp"
#include "BoldModel.hpp"
#include "BoldPersistenceHandle.hpp"
#include "BoldPersistenceHandleDB.hpp"
#include "BoldPersistenceHandlePassthrough.hpp"
#include "BoldPropagatorHandleCOM.hpp"
#include "BoldSnooperHandle.hpp"
#include "BoldSortedHandle.hpp"
#include "BoldStringControlPack.hpp"
#include "BoldSubscription.hpp"
#include "BoldSystemHandle.hpp"
#include "BoldAbstractDatabaseAdapter.hpp"
#include "BoldAbstractPersistenceHandleDB.hpp"
#include "BoldDatabaseAdapterIB.hpp"
#include "BoldPersistenceHandlePTWithModel.hpp"
#include <ExtCtrls.hpp>
#include <DB.hpp>
#include <IBDatabase.hpp>
//---------------------------------------------------------------------------
class TDataModule1 : public TDataModule
{
__published:	// IDE-managed Components
  TBoldModel *BoldModel1;
  TBoldSystemTypeInfoHandle *BoldSystemTypeInfoHandle1;
  TBoldSystemHandle *BoldSystemHandle1;
  TBoldSystemHandle *TempSystem;
  TBoldAsCheckBoxStateRenderer *IsRichRenderer;
  TBoldAsStringRenderer *FullNameRenderer;
  TBoldAsStringRenderer *NegativeRedRenderer;
  TBoldFilter *IsRichFilter;
  TBoldComparer *NameComparer;
  TTimer *Timer1;
  TBoldComConnectionHandle *bcchEnterprisePropagator;
  TBoldPropagatorHandleCOM *BoldPropagatorHandleCOM1;
  TBoldIdAdderHandle *BoldIDAdderHandle1;
  TBoldExternalObjectSpaceEventHandler *BoldExternalObjectSpaceEventHandler1;
  TBoldSnooperHandle *BoldSnooperHandle1;
  TBoldListenerHandle *BoldListenerHandle1;
        TBoldPersistenceHandleDB *BoldPersistenceHandleDB1;
        TBoldDatabaseAdapterIB *BoldDatabaseAdapterIB1;
        TIBDatabase *IBDatabase1;
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
  bool __fastcall IsRichFilterFilter(TBoldElement *Element);
  void __fastcall IsRichFilterSubscribe(TBoldElement *Element,
          TBoldSubscriber *Subscriber);
  int __fastcall NameComparerCompare(TBoldElement *Item1,
          TBoldElement *Item2);
  void __fastcall NameComparerSubscribe(TBoldElement *Element,
          TBoldSubscriber *Subscriber);
  void __fastcall Timer1Timer(TObject *Sender);
  //void __fastcall BoldListenerHandle1ExtendLeaseFailed(TObject *Sender);
  void __fastcall BoldListenerHandle1RegistrationFailed(TObject *Sender);
  void __fastcall BoldExternalObjectSpaceEventHandler1Conflict(
          TBoldObject *BoldObject);
  void __fastcall bcchEnterprisePropagatorBeforeConnect(TObject *Sender);
  void __fastcall bcchEnterprisePropagatorConnectFailed(TObject *Sender);
  bool __fastcall BoldListenerHandle1ThreadError(AnsiString aMessage);
        void __fastcall BoldListenerHandle1ExtendLeaseFailed(
          TBoldExtendLeaseResult res, const AnsiString Msg);
private:	// User declarations
public:		// User declarations
  __fastcall TDataModule1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TDataModule1 *DataModule1;
//---------------------------------------------------------------------------
#endif
