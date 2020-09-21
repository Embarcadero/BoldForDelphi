//---------------------------------------------------------------------------

#ifndef fMainH
#define fMainH

#include "ReverseDeriveExampleClasses.hpp"
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "BoldAbstractListHandle.hpp"
#include "BoldAbstractModel.hpp"
#include "BoldActions.hpp"
#include "BoldAFPPluggable.hpp"
#include "BoldCheckBox.hpp"
#include "BoldCursorHandle.hpp"
#include "BoldDBActions.hpp"
#include "BoldEdit.hpp"
#include "BoldGrid.hpp"
#include "BoldHandle.hpp"
#include "BoldHandleAction.hpp"
#include "BoldHandles.hpp"
#include "BoldLabel.hpp"
#include "BoldListHandle.hpp"
#include "BoldModel.hpp"
#include "BoldNavigator.hpp"
#include "BoldNavigatorDefs.hpp"
#include "BoldPersistenceHandle.hpp"
#include "BoldPersistenceHandleBDE.hpp"
#include "BoldPersistenceHandleDB.hpp"
#include "BoldRootedHandles.hpp"
#include "BoldSubscription.hpp"
#include <ActnList.hpp>
#include <ExtCtrls.hpp>
#include <Grids.hpp>
#include "BoldSystemHandle.hpp"
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
  TBoldListHandle *blhFonts;
  TBoldListHandle *blhPersons;
  TBoldListHandle *blhFamilies;
  TBoldPlaceableAFP *BoldPlaceableAFP1;
  TLabel *Label2;
  TLabel *Label3;
  TLabel *Label1;
  TBoldGrid *BoldGrid1;
  TBoldGrid *BoldGrid2;
  TBoldNavigator *BoldNavigator2;
  TBoldCheckBox *cbxnoValues1;
  TBoldCheckBox *cbxblink1;
  TBoldCheckBox *cbxbold1;
  TBoldCheckBox *cbxunderline1;
  TBoldEdit *BoldEdit1;
  TLabel *lblresultString1;
  TBoldLabel *BoldLabel1;
  TBoldNavigator *BoldNavigator1;
  TButton *Button2;
  TButton *Button1;
  TButton *btnSave;
  TBoldSystemHandle *BoldSystemHandle1;
        TBoldPersistenceHandleDB *BoldPersistenceHandleDB1;
        TBoldDatabaseAdapterIB *BoldDatabaseAdapterIB1;
        TIBDatabase *IBDatabase1;
        TBoldIBDatabaseAction *BoldIBDatabaseAction1;
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall btnSaveClick(TObject *Sender);
  void __fastcall BoldActivateSystemAction1SystemClosed(TObject *Sender);
  void __fastcall BoldActivateSystemAction1SystemOpened(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
