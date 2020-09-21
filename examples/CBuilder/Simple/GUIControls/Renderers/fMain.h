//---------------------------------------------------------------------------

#ifndef fMainH
#define fMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "BoldAbstractListHandle.hpp"
#include "BoldAbstractModel.hpp"
#include "BoldActions.hpp"
#include "BoldControlPack.hpp"
#include "BoldCursorHandle.hpp"
#include "BoldDBActions.hpp"
#include "BoldEdit.hpp"
#include "BoldElements.hpp"
#include "BoldGrid.hpp"
#include "BoldHandle.hpp"
#include "BoldHandleAction.hpp"
#include "BoldHandles.hpp"
#include "BoldListHandle.hpp"
#include "BoldModel.hpp"
#include "BoldNavigator.hpp"
#include "BoldNavigatorDefs.hpp"
#include "BoldPersistenceHandle.hpp"
#include "BoldPersistenceHandleBDE.hpp"
#include "BoldPersistenceHandleDB.hpp"
#include "BoldRootedHandles.hpp"
#include "BoldStringControlPack.hpp"
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
  TBoldGrid *BoldGrid1;
  TBoldListHandle *blhPerson;
  TBoldAsStringRenderer *bsrFullName;
  TBoldAsStringRenderer *bsrSalaryLevel;
  TBoldListHandle *blhGlobals;
  TBoldEdit *btxtBreakPoint;
  TLabel *lblSalaryBreakPoint;
  TBoldNavigator *BoldNavigator1;
  TButton *Button1;
  TButton *Button2;
  TActionList *ActionList1;
  TBoldActivateSystemAction *BoldActivateSystemAction1;
  TBoldSystemTypeInfoHandle *BoldSystemTypeInfoHandle1;
  TBoldSystemHandle *BoldSystemHandle1;
  TBoldModel *BoldModel1;
        TBoldPersistenceHandleDB *BoldPersistenceHandleDB1;
        TBoldDatabaseAdapterIB *BoldDatabaseAdapterIB1;
        TIBDatabase *IBDatabase1;
        TBoldIBDatabaseAction *BoldIBDatabaseAction1;
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
  void __fastcall BoldActivateSystemAction1SystemOpened(TObject *Sender);
  void __fastcall bsrSalaryLevelSetColor(TBoldElement *Element,
          TColor &AColor, int Representation, AnsiString Expression);
  void __fastcall bsrSalaryLevelSubscribe(TBoldElement *Element,
          int Representation, AnsiString Expression,
          TBoldSubscriber *Subscriber);
  AnsiString __fastcall bsrFullNameGetAsString(TBoldElement *Element,
          int Representation, AnsiString Expression);
  bool __fastcall bsrFullNameMayModify(TBoldElement *Element,
          int Representation, AnsiString Expression,
          TBoldSubscriber *Subscriber);
  void __fastcall bsrFullNameSetAsString(TBoldElement *Element,
          AnsiString NewValue, int Representation, AnsiString Expression);
  void __fastcall bsrFullNameSubscribe(TBoldElement *Element,
          int Representation, AnsiString Expression,
          TBoldSubscriber *Subscriber);
private:	// User declarations
public:		// User declarations
  __fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
