//---------------------------------------------------------------------------

#ifndef fMainH
#define fMainH
#include "DerivedHandleExampleClasses.hpp"
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
#include "BoldDerivedHandle.hpp"
#include "BoldEdit.hpp"
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
#include "BoldSubscription.hpp"
#include "BoldSystemHandle.hpp"
#include "BoldTrackBar.hpp"
#include "BoldVariableHandle.hpp"
#include <ActnList.hpp>
#include <ComCtrls.hpp>
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
  TGroupBox *GroupBox1;
  TButton *btnUpdate;
  TButton *Button2;
  TButton *Button3;
  TBoldNavigator *bnAllPersons;
  TLabel *lblPersons;
  TBoldGrid *grdAllPersons;
  TBoldTrackBar *btbRichBreakpoint;
  TLabel *Label5;
  TLabel *Label4;
  TLabel *Label6;
  TBoldEdit *edRichBreakpoint;
  TLabel *Label2;
  TBoldEdit *edRichPersonCount;
  TUpDown *UpDown1;
  TLabel *Label3;
  TBoldGrid *grdRichPersons;
  TBoldCursorHandle *bchRichPersons;
  TBoldDerivedHandle *bdhRichPersons;
  TBoldVariableHandle *bvhRichCount;
  TBoldVariableHandle *bvhAssetBreakpoint;
  TBoldListHandle *blhAllPersons;
  TActionList *ActionList1;
  TBoldActivateSystemAction *BoldActivateSystemAction1;
  TBoldSystemTypeInfoHandle *BoldSystemTypeInfoHandle1;
  TBoldSystemHandle *BoldSystemHandle1;
  TBoldModel *BoldModel1;
        TBoldPersistenceHandleDB *BoldPersistenceHandleDB1;
        TBoldDatabaseAdapterIB *BoldDatabaseAdapterIB1;
        TIBDatabase *IBDatabase1;
        TBoldIBDatabaseAction *BoldIBDatabaseAction1;
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall BoldActivateSystemAction1SystemClosed(TObject *Sender);
  void __fastcall BoldActivateSystemAction1SystemOpened(TObject *Sender);
  void __fastcall btnUpdateClick(TObject *Sender);
  void __fastcall bdhRichPersonsDeriveAndSubscribe(TComponent *Sender,
          TBoldElement *RootValue, TBoldIndirectElement *ResultElement,
          TBoldSubscriber *Subscriber);
private:	// User declarations
  Integer __fastcall RichSorter(TBoldElement *Person1, TBoldElement *Person2);
public:		// User declarations
  __fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
