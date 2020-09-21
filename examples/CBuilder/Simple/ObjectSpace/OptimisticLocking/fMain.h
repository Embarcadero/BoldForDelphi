//---------------------------------------------------------------------------

#ifndef fMainH
#define fMainH
#include "OptimisticLockingExampleClasses.hpp"
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "BoldAbstractListHandle.hpp"
#include "BoldAbstractModel.hpp"
#include "BoldActions.hpp"
#include "BoldAFPPluggable.hpp"
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
#include "BoldVariableHandle.hpp"
#include "BoldSystem.hpp"
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
  TBoldModel *BoldModel1;
  TBoldSystemTypeInfoHandle *BoldSystemTypeInfoHandle1;
  TActionList *ActionList1;
  TBoldActivateSystemAction *BoldActivateSystemAction1;
  TBoldActivateSystemAction *BoldActivateSystemAction2;
  TBoldPlaceableAFP *BoldPlaceableAFP1;
  TGroupBox *GroupBox1;
  TLabel *Label2;
  TButton *cmdOpen1;
  TBoldGrid *BoldGrid1;
  TBoldGrid *BoldGrid2;
  TBoldNavigator *BoldNavigator1;
  TBoldNavigator *BoldNavigator2;
  TButton *cmdUpdate1;
  TBoldListBox *BoldListBox1;
  TButton *cmdDiscardApp1;
  TBoldListHandle *blhAllPersons1;
  TBoldSystemHandle *BoldSystemHandle1;
  TBoldListHandle *blhAllCars1;
  TBoldVariableHandle *bvhFailedObjects1;
  TBoldCursorHandle *bchFailedObjects1;
  TGroupBox *GroupBox2;
  TLabel *Label1;
  TButton *cmdOpen2;
  TBoldGrid *BoldGrid3;
  TBoldGrid *BoldGrid4;
  TBoldNavigator *BoldNavigator3;
  TBoldNavigator *BoldNavigator4;
  TButton *cmdUpdate2;
  TBoldListBox *BoldListBox2;
  TButton *cmdDiscardApp2;
  TBoldListHandle *blhAllPersons2;
  TBoldSystemHandle *BoldSystemHandle2;
  TBoldListHandle *blhallCars2;
  TBoldVariableHandle *bvhFailedObjects2;
  TBoldCursorHandle *bchFailedObjects2;
  TButton *cmdCreateDB;
        TBoldPersistenceHandleDB *BoldPersistenceHandleDB1;
        TBoldDatabaseAdapterIB *BoldDatabaseAdapterIB1;
        TIBDatabase *IBDatabase1;
        TBoldPersistenceHandleDB *BoldPersistenceHandleDB2;
        TBoldDatabaseAdapterIB *BoldDatabaseAdapterIB2;
        TIBDatabase *IBDatabase2;
        TBoldIBDatabaseAction *BoldIBDatabaseAction1;
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall cmdDiscardApp1Click(TObject *Sender);
  void __fastcall cmdDiscardApp2Click(TObject *Sender);
  void __fastcall cmdUpdate1Click(TObject *Sender);
  void __fastcall cmdUpdate2Click(TObject *Sender);
private:	// User declarations
  void __fastcall DiscardObject(TBoldObject *BoldObject);
public:		// User declarations
  __fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
