//---------------------------------------------------------------------------

#ifndef fMainH
#define fMainH
#include "ConstraintExampleClasses.hpp"
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "BoldAbstractListHandle.hpp"
#include "BoldAbstractModel.hpp"
#include "BoldActions.hpp"
#include "BoldAFPPluggable.hpp"
#include "BoldComboBox.hpp"
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
  TLabel *Label1;
  TBoldGrid *BoldGrid1;
  TBoldListHandle *blhCustomers;
  TBoldNavigator *BoldNavigator1;
  TBoldNavigator *BoldNavigator5;
  TBoldGrid *BoldGrid4;
  TBoldListHandle *blhAllParts;
  TLabel *Label2;
  TBoldModel *BoldModel1;
  TBoldSystemTypeInfoHandle *BoldSystemTypeInfoHandle1;
  TBoldSystemHandle *BoldSystemHandle1;
  TActionList *ActionList1;
  TBoldActivateSystemAction *BoldActivateSystemAction1;
  TBoldPlaceableAFP *BoldPlaceableAFP1;
  TBoldGrid *BoldGrid3;
  TBoldListHandle *blhOrderItems;
  TBoldNavigator *BoldNavigator3;
  TBoldComboBox *BoldComboBox1;
  TLabel *Label5;
  TLabel *Label4;
  TLabel *Label3;
  TBoldGrid *BoldGrid2;
  TBoldListHandle *blhOrders;
  TBoldNavigator *BoldNavigator2;
  TLabel *Label7;
  TBoldGrid *BoldGrid5;
  TBoldListHandle *blhBrokenConstraints;
  TLabel *Label6;
  TButton *btnSave;
  TButton *Button2;
  TButton *Button1;
        TBoldPersistenceHandleDB *BoldPersistenceHandleDB1;
        TBoldDatabaseAdapterIB *BoldDatabaseAdapterIB1;
        TIBDatabase *IBDatabase1;
        TBoldIBDatabaseAction *BoldIBDatabaseAction1;
  void __fastcall BoldActivateSystemAction1SystemClosed(TObject *Sender);
  void __fastcall BoldActivateSystemAction1SystemOpened(TObject *Sender);
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall btnSaveClick(TObject *Sender);
  void __fastcall BoldGrid5DblClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
