//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "MainForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "BoldAbstractListHandle"
#pragma link "BoldAbstractModel"
#pragma link "BoldActions"
#pragma link "BoldCursorHandle"
#pragma link "BoldDBActions"
#pragma link "BoldEdit"
#pragma link "BoldExpressionHandle"
#pragma link "BoldGrid"
#pragma link "BoldHandle"
#pragma link "BoldHandleAction"
#pragma link "BoldHandles"
#pragma link "BoldListHandle"
#pragma link "BoldModel"
#pragma link "BoldNavigator"
#pragma link "BoldNavigatorDefs"
#pragma link "BoldPersistenceHandle"
#pragma link "BoldPersistenceHandleBDE"
#pragma link "BoldPersistenceHandleDB"
#pragma link "BoldRootedHandles"
#pragma link "BoldSubscription"
#pragma link "BoldSystemHandle"
#pragma link "BoldOclVariables"
#pragma link "OclVariablesClasses"
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
void __fastcall TForm1::BoldActivateSystemAction1SystemOpened(
      TObject *Sender)
{
  if (BoldSystemHandle1->System->ClassByExpressionName["GlobalSettings"]->Count == 0)
  {
    new TGlobalSettings(BoldSystemHandle1->System);
    BoldSystemHandle1->UpdateDatabase();
  }
}
//---------------------------------------------------------------------------

void __fastcall TForm1::FormCreate(TObject *Sender)
{
  Width  = 305;
  Height = 369;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::FormCloseQuery(TObject *Sender, bool &CanClose)
{
  CanClose = True;
  if (BoldSystemHandle1->Active)
  {
    if (BoldSystemHandle1->System->DirtyObjects->Count > 0)
    {
      switch (MessageDlg( "There are dirty objects. save them before exit?", mtConfirmation, TMsgDlgButtons() << mbYes << mbNo << mbCancel, 0))
      {
        case mrYes: BoldSystemHandle1->System->UpdateDatabase();
        case mrNo: BoldSystemHandle1->System->Discard();
        case mrCancel: CanClose = False;
      }
    }
  }
}
//---------------------------------------------------------------------------

void __fastcall TForm1::btnSaveClick(TObject *Sender)
{
  BoldSystemHandle1->System->UpdateDatabase();
}
//---------------------------------------------------------------------------

