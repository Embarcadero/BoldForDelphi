//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "fMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "BoldAbstractListHandle"
#pragma link "BoldAbstractModel"
#pragma link "BoldActions"
#pragma link "BoldAFPPluggable"
#pragma link "BoldComboBox"
#pragma link "BoldCursorHandle"
#pragma link "BoldDBActions"
#pragma link "BoldGrid"
#pragma link "BoldHandle"
#pragma link "BoldHandleAction"
#pragma link "BoldHandles"
#pragma link "BoldListHandle"
#pragma link "BoldModel"
#pragma link "BoldNavigator"
#pragma link "BoldNavigatorDefs"
#pragma link "BoldPersistenceHandle"
#pragma link "BoldPersistenceHandleDB"
#pragma link "BoldRootedHandles"
#pragma link "BoldSubscription"
#pragma link "BoldSystemHandle"
#pragma link "BoldAbstractDatabaseAdapter"
#pragma link "BoldAbstractPersistenceHandleDB"
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
void __fastcall TForm1::BoldActivateSystemAction1SystemClosed(TObject *Sender)
{
  btnSave->Enabled = BoldSystemHandle1->Active;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::BoldActivateSystemAction1SystemOpened(
      TObject *Sender)
{
  btnSave->Enabled = BoldSystemHandle1->Active;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender)
{
  Width = 603;
  Height = 582;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btnSaveClick(TObject *Sender)
{
  BoldSystemHandle1->UpdateDatabase();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::BoldGrid5DblClick(TObject *Sender)
{
  AutoFormProviderRegistry()->FormForElement(((TBAConstraint*)(blhBrokenConstraints->CurrentElement))->OwningElement)->Show();
}
//---------------------------------------------------------------------------
