//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "fMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "BoldAbstractListHandle"
#pragma link "BoldAFPPluggable"
#pragma link "BoldComboBox"
#pragma link "BoldCursorHandle"
#pragma link "BoldEdit"
#pragma link "BoldExpressionHandle"
#pragma link "BoldHandles"
#pragma link "BoldListBox"
#pragma link "BoldListHandle"
#pragma link "BoldNavigator"
#pragma link "BoldNavigatorDefs"
#pragma link "BoldPageControl"
#pragma link "BoldRootedHandles"
#pragma link "BoldSubscription"
#pragma link "BoldSystem"
#pragma resource "*.dfm"
TfrmMain *frmMain;
//---------------------------------------------------------------------------
__fastcall TfrmMain::TfrmMain(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::FormCloseQuery(TObject *Sender, bool &CanClose)
{
  CanClose = true;
  if (dmMain->bshMain->Active && dmMain->bshMain->System->BoldDirty)
  {
    if (MessageDlg("You have dirty objects. Do you want to quit anyway?", mtConfirmation, TMsgDlgButtons() << mbYes << mbNo, 0) == mrYes)
      dmMain->bshMain->System->Discard();
    else
      CanClose = false;
  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnRunClick(TObject *Sender)
{
  dmMain->bshMain->System->StartTransaction(stmNormal);
  try
  {
    while (blhAllRequests->List->Count > 0)
    {
      ((TRequest*)(blhAllRequests->List->Elements[0]))->Perform();
      ((TRequest*)(blhAllRequests->List->Elements[0]))->Delete();
    }
    dmMain->bshMain->System->CommitTransaction(stmNormal);
  }
  catch (Exception &exception)
  {
    dmMain->bshMain->System->RollbackTransaction(stmNormal);
    ShowMessage(exception.Message);
  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnDeleteClick(TObject *Sender)
{
  blhAllRequests->CurrentBoldObject->Delete();
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnTransferClick(TObject *Sender)
{
  new TTransfer(dmMain->bshMain->System);
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnModifyCreditClick(TObject *Sender)
{
  new TModifyCredit(dmMain->bshMain->System);
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnCloseAccountClick(TObject *Sender)
{
  new TClose(dmMain->bshMain->System);
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::Exit1Click(TObject *Sender)
{
  int status;
  exit(status);
}
//---------------------------------------------------------------------------
