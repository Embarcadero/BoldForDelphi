//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "dSystemTypeInfo.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "BoldHandles"
#pragma link "BoldSubscription"
#pragma resource "*.dfm"
TdmSystemTypeInfo *dmSystemTypeInfo;
//---------------------------------------------------------------------------
__fastcall TdmSystemTypeInfo::TdmSystemTypeInfo(TComponent* Owner)
  : TDataModule(Owner)
{
}
//---------------------------------------------------------------------------
