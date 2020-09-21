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
#pragma link "BoldNavigator"
#pragma link "BoldNavigatorDefs"
#pragma link "BoldPersistenceHandle"
#pragma link "BoldPersistenceHandleBDE"
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
//Boolean __fastcall TForm1::AnswerFalse()
Boolean __fastcall TForm1::AnswerFalse(TObject *Originator, TBoldEvent OriginalEvent,
                      TBoldRequestedEvent RequestedEvent, const System::TVarRec * Args,
                      const int Args_Size, Boldsubscription::TBoldSubscriber* Subscriber)
{
  Boolean Result = false;
  SetBoldLastFailureReason(new TBoldFailureReason("Value is locked", NULL));
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btnSaveClick(TObject *Sender)
{
  try
  {
    BoldSystemHandle1->UpdateDatabase();
  }
  catch (Exception &exception)
  {
    BoldRaiseLastFailure(BoldSystemHandle1->System, "SaveToDatabase", "Update failed");
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btnLockClick(TObject *Sender)
{
  ((TThing*)(BoldListHandle1->CurrentBoldObject))->M_Name->AddSubscription(LockSubscriber, bqMaySetValue, bqMaySetValue);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btnReleaseClick(TObject *Sender)
{
  LockSubscriber->CancelAllSubscriptions();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormCloseQuery(TObject *Sender, bool &CanClose)
{
  CanClose = true;
  if (BoldSystemHandle1->Active)
    if (BoldSystemHandle1->System->DirtyObjects->Count > 0)
      switch (MessageDlg("There are dirty objects. Save them before exit?", mtConfirmation, TMsgDlgButtons() << mbYes << mbNo << mbCancel, 0))
      {
        case mrYes: BoldSystemHandle1->System->UpdateDatabase(); break;
        case mrNo: BoldSystemHandle1->System->Discard(); break;
        case mrCancel: CanClose = false;
      }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender)
{
  TBoldEventHandler ReceiveFunc = NULL;
  TBoldQueryHandler AnswerFunc = &AnswerFalse;
  LockSubscriber = new TBoldPassthroughSubscriber(ReceiveFunc, AnswerFunc);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormDestroy(TObject *Sender)
{
  LockSubscriber->Free();
}

