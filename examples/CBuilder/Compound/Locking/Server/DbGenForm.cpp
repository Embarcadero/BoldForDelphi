//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "DbGenForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "BoldHandle"
#pragma link "BoldHandles"
#pragma link "BoldPersistenceHandle"
#pragma link "BoldPersistenceHandleDB"
#pragma link "BoldSubscription"
#pragma link "BoldSystemHandle"
#pragma link "BoldAbstractDatabaseAdapter"
#pragma link "BoldAbstractPersistenceHandleDB"
#pragma link "BoldDatabaseAdapterIB"
#pragma resource "*.dfm"
TfrmDBGen *frmDBGen;
//---------------------------------------------------------------------------
__fastcall TfrmDBGen::TfrmDBGen(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmDBGen::Button1Click(TObject *Sender)
{
  BoldDatabaseAdapterIB1->CreateInterbaseDatabase();
  BoldPersistenceHandleDB1->CreateDataBaseSchema();
  ShowMessage("OK");
}
//---------------------------------------------------------------------------

