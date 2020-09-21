//---------------------------------------------------------------------------

#ifndef fMainH
#define fMainH

#include "DerivedAttrExampleClasses.hpp"
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
#include "BoldEdit.hpp"
#include "BoldGrid.hpp"
#include "BoldHandle.hpp"
#include "BoldHandleAction.hpp"
#include "BoldHandles.hpp"
#include "BoldListActions.hpp"
#include "BoldListHandle.hpp"
#include "BoldModel.hpp"
#include "BoldPersistenceHandle.hpp"
#include "BoldPersistenceHandleBDE.hpp"
#include "BoldPersistenceHandleDB.hpp"
#include "BoldRootedHandles.hpp"
#include "BoldSubscription.hpp"
#include "BoldSystemHandle.hpp"
#include <ActnList.hpp>
#include <Grids.hpp>
#include "BoldAbstractPersistenceHandleDB.hpp"
#include "BoldPersistenceHandleDB_deprecated.hpp"
#include "BoldAbstractDatabaseAdapter.hpp"
#include "BoldDatabaseAdapterIB.hpp"
#include "BoldIBDatabaseAction.hpp"
#include <DB.hpp>
#include <IBDatabase.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
  TBoldModel *BoldModel1;
  TBoldSystemTypeInfoHandle *BoldSystemTypeInfoHandle1;
  TBoldSystemHandle *BoldSystemHandle1;
  TActionList *ActionList1;
  TBoldActivateSystemAction *BoldActivateSystemAction1;
  TBoldUpdateDBAction *BoldUpdateDBAction1;
  TBoldListHandleDeleteAction *BoldListHandleDeleteAction1;
  TBoldListHandleAddNewAction *BoldListHandleAddNewAction1;
  TBoldListHandle *blhGlobalSettings;
  TBoldListHandle *blhProduct;
  TBoldGrid *BoldGrid1;
  TButton *btnAddProduct;
  TButton *btnDelProduct;
  TButton *btnSave;
  TGroupBox *gboGlobalSettings;
  TLabel *lblVAT;
  TLabel *lblPercent;
  TLabel *Label1;
  TBoldEdit *btxtVAT;
  TButton *Button1;
  TButton *Button2;
        TBoldPersistenceHandleDB *BoldPersistenceHandleDB1;
        TBoldDatabaseAdapterIB *BoldDatabaseAdapterIB1;
        TIBDatabase *IBDatabase1;
        TBoldIBDatabaseAction *BoldIBDatabaseAction1;
  void __fastcall BoldActivateSystemAction1SystemOpened(TObject *Sender);
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
  void __fastcall FormCreate(TObject *Sender);
private:	// User declarations
  void __fastcall SetGlobals(void);
public:		// User declarations
  __fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
