//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "dModel.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "BoldAbstractModel"
#pragma link "BoldHandle"
#pragma link "BoldModel"
#pragma link "BoldSubscription"
#pragma link "BoldUMLModelLink"
#pragma link "BoldUMLRose98Link"
#pragma resource "*.dfm"
TdmModel *dmModel;
//---------------------------------------------------------------------------
__fastcall TdmModel::TdmModel(TComponent* Owner)
  : TDataModule(Owner)
{
}
//---------------------------------------------------------------------------
