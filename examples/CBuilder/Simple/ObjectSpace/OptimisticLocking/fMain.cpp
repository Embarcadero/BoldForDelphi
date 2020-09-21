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
#pragma link "BoldCursorHandle"
#pragma link "BoldDBActions"
#pragma link "BoldGrid"
#pragma link "BoldHandle"
#pragma link "BoldHandleAction"
#pragma link "BoldHandles"
#pragma link "BoldListBox"
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
#pragma link "BoldVariableHandle"
#pragma ling "BoldSystem"
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
void __fastcall TForm1::FormCloseQuery(TObject *Sender, bool &CanClose)
{
  CanClose = true;
  if ( (BoldSystemHandle1->Active &&
    BoldSystemHandle1->System->DirtyObjects->Count > 0) ||
    (BoldSystemHandle2->Active && BoldSystemHandle2->System->DirtyObjects->Count > 0) )
    switch (MessageDlg("There are dirty objects. Save them before exit?", mtConfirmation, TMsgDlgButtons() << mbYes << mbNo << mbCancel, 0) )
    {
      case mrYes: BoldSystemHandle1->System->UpdateDatabase();
                  BoldSystemHandle2->System->UpdateDatabase();
                  break;
      case mrNo:  BoldSystemHandle1->System->Discard();
                  BoldSystemHandle2->System->Discard();
                  break;
      case mrCancel: CanClose = false;
    }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender)
{
  Height = 465;
  Width  = 651;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::cmdDiscardApp1Click(TObject *Sender)
{
  DiscardObject(bchFailedObjects1->CurrentBoldObject);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::cmdDiscardApp2Click(TObject *Sender)
{
  DiscardObject(bchFailedObjects2->CurrentBoldObject);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::cmdUpdate1Click(TObject *Sender)
{
  bchFailedObjects1->List->Clear();
  try
  {
    BoldSystemHandle1->UpdateDatabase();
  }
  catch (Exception &ex)
  {
    bchFailedObjects1->List->AddList( ((EBoldOperationFailedForObjectList&)ex).ObjectList );
    throw (EBoldOperationFailedForObjectList&)ex;
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::cmdUpdate2Click(TObject *Sender)
{
  bchFailedObjects2->List->Clear();
  try
  {
    BoldSystemHandle2->UpdateDatabase();
  }
  catch (Exception &ex)
  {
    bchFailedObjects2->List->AddList( ((EBoldOperationFailedForObjectList&)ex).ObjectList );
    throw (EBoldOperationFailedForObjectList&)ex;
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::DiscardObject(TBoldObject *BoldObject)
{
  if (BoldObject)
    BoldObject->Discard();
  else
    throw new Exception("No object selected to discard");
}
//---------------------------------------------------------------------------

