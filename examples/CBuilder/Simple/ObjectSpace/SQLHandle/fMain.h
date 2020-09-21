//---------------------------------------------------------------------------

#ifndef fMainH
#define fMainH
#include "SQLHandleClasses.hpp"

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
#include "BoldListHandle.hpp"
#include "BoldModel.hpp"
#include "BoldNavigator.hpp"
#include "BoldNavigatorDefs.hpp"
#include "BoldPersistenceHandle.hpp"
#include "BoldPersistenceHandleBDE.hpp"
#include "BoldPersistenceHandleDB.hpp"
#include "BoldRootedHandles.hpp"
#include "BoldSQLHandle.hpp"
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
  TBoldSystemTypeInfoHandle *BoldSystemTypeInfoHandle1;
  TBoldSystemHandle *BoldSystemHandle1;
  TBoldModel *BoldModel1;
  TActionList *ActionList1;
  TBoldActivateSystemAction *BoldActivateSystemAction1;
  TBoldSQLHandle *bsqlhPersons;
  TBoldListHandle *blhSelectedPersons;
  TGroupBox *GroupBox1;
  TButton *btnUpdateDB;
  TButton *Button2;
  TButton *Button3;
  TBoldNavigator *bnAllPersons;
  TBoldGrid *grdAllPersons;
  TBoldListHandle *blhAllPersons;
  TBoldGrid *grdSQLResult;
  TGroupBox *gbSelection;
  TLabel *Label2;
  TLabel *Label3;
  TLabel *Label4;
  TComboBox *cmbAssetsOperator;
  TComboBox *cmbFirstnameOperator;
  TComboBox *cmbLastnameOperator;
  TEdit *edAssetsExpr;
  TEdit *edFirstnameExpr;
  TEdit *edLastnameExpr;
  TGroupBox *gbSorting;
  TLabel *Label5;
  TComboBox *cmbOrderBy1;
  TComboBox *cmbOrderBy3;
  TComboBox *cmbOrderBy2;
  TButton *btnExecSQL;
  TLabel *Label7;
  TLabel *Label6;
  TLabel *Label1;
        TBoldPersistenceHandleDB *BoldPersistenceHandleDB1;
        TBoldDatabaseAdapterIB *BoldDatabaseAdapterIB1;
        TIBDatabase *IBDatabase1;
        TBoldIBDatabaseAction *BoldIBDatabaseAction1;
  void __fastcall BoldActivateSystemAction1SystemClosed(TObject *Sender);
  void __fastcall BoldActivateSystemAction1SystemOpened(TObject *Sender);
  void __fastcall btnUpdateDBClick(TObject *Sender);
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
  void __fastcall btnExecSQLClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
