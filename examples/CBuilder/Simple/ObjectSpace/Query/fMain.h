//---------------------------------------------------------------------------

#ifndef fMainH
#define fMainH
#include "QueryDemoClasses.hpp"
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "BoldAbstractListHandle.hpp"
#include "BoldAbstractModel.hpp"
#include "BoldActions.hpp"
#include "BoldCursorHandle.hpp"
#include "BoldDBActions.hpp"
#include "BoldGrid.hpp"
#include "BoldHandle.hpp"
#include "BoldHandleAction.hpp"
#include "BoldHandles.hpp"
#include "BoldListBox.hpp"
#include "BoldListHandle.hpp"
#include "BoldModel.hpp"
#include "BoldNavigator.hpp"
#include "BoldNavigatorDefs.hpp"
#include "BoldPersistenceHandle.hpp"
#include "BoldPersistenceHandleBDE.hpp"
#include "BoldPersistenceHandleDB.hpp"
#include "BoldRootedHandles.hpp"
#include "BoldSubscription.hpp"
#include "BoldSystemHandle.hpp"
#include <ActnList.hpp>
#include <ExtCtrls.hpp>
#include <Grids.hpp>
#include "BoldAbstractDatabaseAdapter.hpp"
#include "BoldAbstractPersistenceHandleDB.hpp"
#include "BoldDatabaseAdapterIB.hpp"
#include "BoldIBDatabaseAction.hpp"
#include <DB.hpp>
#include <IBDatabase.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
  TLabel *Label2;
  TBoldListBox *BoldListBox1;
  TBoldListHandle *BoldListHandle1;
  TButton *btnLock;
  TButton *btnRelease;
  TBoldListBox *BoldListBox2;
  TBoldListHandle *BoldListHandle3;
  TLabel *Label1;
  TActionList *ActionList1;
  TBoldActivateSystemAction *BoldActivateSystemAction1;
  TBoldModel *BoldModel1;
  TBoldSystemTypeInfoHandle *BoldSystemTypeInfoHandle1;
  TBoldSystemHandle *BoldSystemHandle1;
  TButton *Button1;
  TButton *Button2;
  TBoldGrid *BoldGrid1;
  TBoldListHandle *BoldListHandle2;
  TLabel *Label3;
  TBoldNavigator *BoldNavigator1;
  TButton *btnSave;
        TBoldPersistenceHandleDB *BoldPersistenceHandleDB1;
        TBoldDatabaseAdapterIB *BoldDatabaseAdapterIB1;
        TIBDatabase *IBDatabase1;
        TBoldIBDatabaseAction *BoldIBDatabaseAction1;
  void __fastcall btnSaveClick(TObject *Sender);
  void __fastcall btnLockClick(TObject *Sender);
  void __fastcall btnReleaseClick(TObject *Sender);
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall FormDestroy(TObject *Sender);
private:	// User declarations
  TBoldPassthroughSubscriber *LockSubscriber;

 // Boolean __fastcall AnswerFalse();
  Boolean __fastcall AnswerFalse(TObject *Originator, TBoldEvent OriginalEvent,
                      TBoldRequestedEvent RequestedEvent, const System::TVarRec * Args,
                      const int Args_Size, Boldsubscription::TBoldSubscriber* Subscriber);

public:		// User declarations
  __fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
