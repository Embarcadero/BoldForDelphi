//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "dPersistence.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "BoldAbstractObjectUpgraderHandle"
#pragma link "BoldHandle"
#pragma link "BoldObjectUpgraderHandle"
#pragma link "BoldPersistenceHandle"
#pragma link "BoldPersistenceHandleBDE"
#pragma link "BoldPersistenceHandleDB"
#pragma link "BoldSubscription"
#pragma resource "*.dfm"
TdmPersistence *dmPersistence;
//---------------------------------------------------------------------------
__fastcall TdmPersistence::TdmPersistence(TComponent* Owner)
  : TDataModule(Owner)
{
}
//---------------------------------------------------------------------------
