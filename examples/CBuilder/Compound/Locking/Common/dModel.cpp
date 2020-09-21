//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "dModel.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "BoldAbstractModel"
#pragma link "BoldModel"
#pragma link "BoldSubscription"
#pragma resource "*.dfm"
TdmModel *dmModel;
//---------------------------------------------------------------------------
__fastcall TdmModel::TdmModel(TComponent* Owner)
  : TDataModule(Owner)
{
}
//---------------------------------------------------------------------------
