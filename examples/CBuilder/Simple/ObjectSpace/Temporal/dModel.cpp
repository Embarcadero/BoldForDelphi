//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "dModel.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "BoldAbstractModel"
#pragma link "BoldHandle"
#pragma link "BoldHandles"
#pragma link "BoldModel"
#pragma link "BoldPersistenceHandle"
#pragma link "BoldPersistenceHandleBDE"
#pragma link "BoldPersistenceHandleDB"
#pragma link "BoldSubscription"
#pragma link "BoldSystemHandle"
#pragma link "BoldUMLModelLink"
#pragma link "BoldUMLRose98Link"
#pragma link "BoldAbstractDatabaseAdapter"
#pragma link "BoldAbstractPersistenceHandleDB"
#pragma link "BoldDatabaseAdapterIB"
#pragma resource "*.dfm"
TdmModel *dmModel;
//---------------------------------------------------------------------------
__fastcall TdmModel::TdmModel(TComponent* Owner)
  : TDataModule(Owner)
{
}
//---------------------------------------------------------------------------
