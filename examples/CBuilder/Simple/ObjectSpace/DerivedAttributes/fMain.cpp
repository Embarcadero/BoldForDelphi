//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "fMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "BoldAbstractListHandle"
#pragma link "BoldAbstractModel"
#pragma link "BoldActions"
#pragma link "BoldCursorHandle"
#pragma link "BoldDBActions"
#pragma link "BoldEdit"
#pragma link "BoldGrid"
#pragma link "BoldHandle"
#pragma link "BoldHandleAction"
#pragma link "BoldHandles"
#pragma link "BoldListActions"
#pragma link "BoldListHandle"
#pragma link "BoldModel"
#pragma link "BoldPersistenceHandle"
#pragma link "BoldPersistenceHandleBDE"
#pragma link "BoldPersistenceHandleDB"
#pragma link "BoldRootedHandles"
#pragma link "BoldSubscription"
#pragma link "BoldSystemHandle"
#pragma link "BoldAbstractPersistenceHandleDB"
#pragma link "BoldPersistenceHandleDB_deprecated"
#pragma link "BoldAbstractDatabaseAdapter"
#pragma link "BoldDatabaseAdapterIB"
#pragma link "BoldIBDatabaseAction"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::BoldActivateSystemAction1SystemOpened(TObject *Sender)
{
  if (BoldSystemHandle1->Active)
  {
    if (BoldSystemHandle1->System->ClassByExpressionName["GlobalSettings"]->Count < 1) // blhGlobalSettings
      BoldSystemHandle1->System->ClassByExpressionName["GlobalSettings"]->AddNew(); //blhGlobalSettings->List->AddNew();
    if (BoldSystemHandle1->System->BoldDirty)
      BoldSystemHandle1->UpdateDatabase();
    SetGlobals();
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::SetGlobals()
{
  TGlobalSettings* aGlobalSetting;

  aGlobalSetting = (TGlobalSettings*)BoldSystemHandle1->System->ClassByExpressionName["GlobalSettings"]->BoldObjects[0];
  if (aGlobalSetting->M_vat)
    BoldSystemHandle1->System->Evaluator->DefineVariable("global_VAT",
      aGlobalSetting->M_vat, aGlobalSetting->M_vat->BoldType, false);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormCloseQuery(TObject *Sender, bool &CanClose)
{
  CanClose = true;
  if ( (BoldSystemHandle1->Active) && (BoldSystemHandle1->System->BoldDirty) )
    switch (MessageDlg("There are dirty objects. Save them before exit?", mtConfirmation,
      TMsgDlgButtons() << mbYes << mbNo << mbCancel, 0) )
    {
      case mrYes: BoldSystemHandle1->System->UpdateDatabase(); break;
      case mrNo: BoldSystemHandle1->System->Discard(); break;
      case mrCancel: CanClose = false;
    }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender)
{
  Width  = 304;
  Height = 370;
}
//---------------------------------------------------------------------------
