//---------------------------------------------------------------------------

#ifndef fMainH
#define fMainH
#include "ParentChildMappingClasses.hpp"

//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "BoldAbstractListHandle.hpp"
#include "BoldAbstractModel.hpp"
#include "BoldActions.hpp"
#include "BoldCheckBox.hpp"
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
#include "BoldSubscription.hpp"
#include "BoldSystemHandle.hpp"
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
  TLabel *Label1;
  TLabel *Label2;
  TPageControl *pbcParentOrChild;
  TTabSheet *tsParent;
  TLabel *lblJet;
  TLabel *lblProp;
  TBoldGrid *bgrdJet;
  TBoldGrid *bgrdProp;
  TBoldNavigator *bnavJet;
  TBoldNavigator *bnavProp;
  TBoldCheckBox *bchkThrustVector;
  TTabSheet *tsChild;
  TLabel *lblTruck;
  TLabel *lblBus;
  TBoldGrid *bgrdTruck;
  TBoldGrid *bgrdBus;
  TBoldNavigator *bnavBus;
  TBoldNavigator *bnavTruck;
  TPanel *Panel1;
  TButton *Button1;
  TButton *Button2;
  TBoldModel *BoldModel1;
  TBoldSystemHandle *BoldSystemHandle1;
  TBoldSystemTypeInfoHandle *BoldSystemTypeInfoHandle1;
  TBoldListHandle *blhJet;
  TBoldListHandle *blhProp;
  TBoldListHandle *blhTruck;
  TBoldListHandle *blhBus;
  TActionList *ActionList;
  TBoldActivateSystemAction *BoldActivateSystemAction;
        TBoldPersistenceHandleDB *BoldPersistenceHandleDB1;
        TBoldDatabaseAdapterIB *BoldDatabaseAdapterIB1;
        TIBDatabase *IBDatabase1;
        TBoldIBDatabaseAction *BoldIBDatabaseAction1;
private:	// User declarations
public:		// User declarations
  __fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
