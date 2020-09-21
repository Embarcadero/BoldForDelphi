//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "dSystem.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "BoldHandles"
#pragma link "BoldSubscription"
#pragma link "BoldSystemHandle"
#pragma link "BoldActions"
#pragma link "BoldDBActions"
#pragma link "BoldHandleAction"
#pragma resource "*.dfm"
TdmSystem *dmSystem;
//---------------------------------------------------------------------------
__fastcall TdmSystem::TdmSystem(TComponent* Owner)
  : TDataModule(Owner)
{
}
//---------------------------------------------------------------------------
