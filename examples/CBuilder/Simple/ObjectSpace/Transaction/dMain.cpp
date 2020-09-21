//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "dMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "BoldAbstractModel"
#pragma link "BoldHandle"
#pragma link "BoldHandles"
#pragma link "BoldModel"
#pragma link "BoldPersistenceHandle"
#pragma link "BoldPersistenceHandleDB"
#pragma link "BoldSubscription"
#pragma link "BoldSystemHandle"
#pragma link "BoldUMLModelLink"
#pragma link "BoldUMLRose98Link"
#pragma link "BoldAbstractDatabaseAdapter"
#pragma link "BoldAbstractPersistenceHandleDB"
#pragma link "BoldDatabaseAdapterIB"
#pragma link "BoldActions"
#pragma link "BoldHandleAction"
#pragma link "BoldIBDatabaseAction"
#pragma resource "*.dfm"
TdmMain *dmMain;
//---------------------------------------------------------------------------
__fastcall TdmMain::TdmMain(TComponent* Owner)
  : TDataModule(Owner)
{
}
//---------------------------------------------------------------------------
