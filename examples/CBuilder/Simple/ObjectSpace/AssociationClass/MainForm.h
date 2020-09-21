//---------------------------------------------------------------------------

#ifndef MainFormH
#define MainFormH

#include "AssociationClassExampleClasses.hpp"
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "BoldAbstractListHandle.hpp"
#include "BoldCursorHandle.hpp"
#include "BoldGrid.hpp"
#include "BoldHandles.hpp"
#include "BoldListBox.hpp"
#include "BoldListHandle.hpp"
#include "BoldNavigator.hpp"
#include "BoldNavigatorDefs.hpp"
#include "BoldRootedHandles.hpp"
#include "BoldSubscription.hpp"
#include <ExtCtrls.hpp>
#include <Grids.hpp>
#include "BoldAbstractModel.hpp"
#include "BoldActions.hpp"
#include "BoldDBActions.hpp"
#include "BoldHandle.hpp"
#include "BoldHandleAction.hpp"
#include "BoldModel.hpp"
#include "BoldPersistenceHandle.hpp"
#include "BoldPersistenceHandleDB.hpp"
#include "BoldSystemHandle.hpp"
#include "BoldUMLModelLink.hpp"
#include "BoldAFPDefault.hpp"
#include <ActnList.hpp>
#include "BoldAFPPluggable.hpp"
#include "BoldAbstractDatabaseAdapter.hpp"
#include "BoldAbstractPersistenceHandleDB.hpp"
#include "BoldDatabaseAdapterIB.hpp"
#include "BoldIBDatabaseAction.hpp"
#include <DB.hpp>
#include <IBDatabase.hpp>
#include "BoldUMLRose98Link.hpp"
//---------------------------------------------------------------------------
class TfrmMain : public TForm
{
__published:	// IDE-managed Components
  TLabel *Label3;
  TBoldGrid *AllPersonsGrid;
  TBoldListHandle *AllPersons;
  TBoldListBox *PersonEmployersListBox;
  TLabel *Label1;
  TBoldListHandle *PersonEmployers;
  TBoldNavigator *PersonNavigator;
  TBoldGrid *PersonJobGrid;
  TBoldListHandle *PersonJobs;
  TLabel *Label4;
  TLabel *Label5;
  TBoldGrid *JobGrid;
  TBoldListHandle *AllJobs;
  TBoldGrid *CompanyGrid;
  TBoldListHandle *AllCompanies;
  TLabel *Label6;
  TBoldListBox *CompanyEmployeesListBox;
  TBoldNavigator *CompanyNavigator;
  TBoldListHandle *CompanyEmployees;
  TLabel *Label2;
  TLabel *Label7;
  TBoldGrid *CompanyJobGrid;
  TBoldListHandle *CompanyJobs;
  TButton *Button1;
  TButton *Button2;
  TBoldModel *BoldModel1;
  TBoldSystemHandle *BoldSystemHandle1;
  TBoldSystemTypeInfoHandle *BoldSystemTypeInfoHandle1;
  TActionList *ActionList1;
  TBoldActivateSystemAction *BoldActivateSystemAction1;
        TBoldPersistenceHandleDB *BoldPersistenceHandleDB1;
        TBoldDatabaseAdapterIB *BoldDatabaseAdapterIB1;
        TIBDatabase *IBDatabase1;
        TBoldUMLRoseLink *BoldUMLRoseLink1;
        TBoldIBDatabaseAction *BoldIBDatabaseAction1;
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
private:	// User declarations
public:		// User declarations
  __fastcall TfrmMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif
