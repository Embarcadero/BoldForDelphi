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
#pragma link "BoldGrid"
#pragma link "BoldHandle"
#pragma link "BoldHandleAction"
#pragma link "BoldHandles"
#pragma link "BoldListBox"
#pragma link "BoldListHandle"
#pragma link "BoldModel"
#pragma link "BoldPersistenceHandle"
#pragma link "BoldPersistenceHandleBDE"
#pragma link "BoldPersistenceHandleDB"
#pragma link "BoldRootedHandles"
#pragma link "BoldSubscription"
#pragma link "BoldSystemHandle"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btnSaveClick(TObject *Sender)
{
  BoldSystemHandle1->System->UpdateDatabase();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::BoldListBox1DragOver(TObject *Sender,
      TObject *Source, int X, int Y, TDragState State, bool &Accept)
{
  Accept = (BoldGUIHandler()->DraggedObjects->Count == 1);
  Accept = ((blhRooms->CurrentBoldObject) &&
            ((TRoom*)(blhRooms->CurrentBoldObject))->ValidateNewInhabitant((TStudent*)(BoldGUIHandler()->DraggedObjects->BoldObjects[0])));

}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormCloseQuery(TObject *Sender, bool &CanClose)
{
  CanClose = true;
  if (BoldSystemHandle1->Active)
    if (BoldSystemHandle1->System->DirtyObjects->Count > 0)
      switch (MessageDlg("There are dirty objects. Save them before exit?",  mtConfirmation, TMsgDlgButtons() << mbYes << mbNo << mbCancel, 0))
      {
        case mrYes: BoldSystemHandle1->System->UpdateDatabase(); break;
        case mrNo: BoldSystemHandle1->System->Discard(); break;
        case mrCancel: CanClose = False;
      }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender)
{
  Height = 450;
  Width  = 303;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btnAddMaleClick(TObject *Sender)
{
  if (BoldSystemHandle1->Active)
    blhMaleStudents->List->AddNew();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btnDelMaleClick(TObject *Sender)
{
  DeleteCurrentFromList(blhMaleStudents);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btnAddFemaleClick(TObject *Sender)
{
  if (BoldSystemHandle1->Active)
    blhFemaleStudents->List->AddNew();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btnDelFemaleClick(TObject *Sender)
{
  DeleteCurrentFromList(blhFemaleStudents);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::DeleteCurrentFromList(TBoldListHandle *AListHandle)
{
  if ((BoldSystemHandle1->Active) && (AListHandle->CurrentBoldObject))
  {
    (AListHandle->CurrentBoldObject)->UnLinkAll();
    (AListHandle->CurrentBoldObject)->Delete();
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btnDelRoomClick(TObject *Sender)
{
  DeleteCurrentFromList(blhRooms);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::btnAddRoomClick(TObject *Sender)
{
  if (BoldSystemHandle1->Active)
    new TRoom(BoldSystemHandle1->System);
}
//---------------------------------------------------------------------------

