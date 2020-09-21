//---------------------------------------------------------------------------

#ifndef MainFormH
#define MainFormH
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
#include "BoldExpressionHandle.hpp"
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
#include "BoldOclVariables.hpp"
#include "OclVariablesClasses.hpp"
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
  TButton *btnSave;
  TGroupBox *gboGlobalSettings;
  TLabel *lblVAT;
  TLabel *lblPercent;
  TLabel *Label1;
  TBoldEdit *btxtVAT;
  TBoldNavigator *bnavProduct;
  TButton *Button1;
  TButton *Button2;
  TBoldListHandle *blhProduct;
  TBoldSystemHandle *BoldSystemHandle1;
  TBoldSystemTypeInfoHandle *BoldSystemTypeInfoHandle1;
  TBoldModel *BoldModel1;
  TBoldExpressionHandle *behVAT;
  TBoldOclVariables *BoldOclVariables1;
  TActionList *ActionList1;
  TBoldActivateSystemAction *BoldActivateSystemAction1;
        TBoldPersistenceHandleDB *BoldPersistenceHandleDB1;
        TBoldDatabaseAdapterIB *BoldDatabaseAdapterIB1;
        TIBDatabase *IBDatabase1;
        TBoldIBDatabaseAction *BoldIBDatabaseAction1;
  void __fastcall BoldActivateSystemAction1SystemOpened(TObject *Sender);
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
  void __fastcall btnSaveClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
