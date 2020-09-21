//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "MainForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "BoldAbstractLockManagerAdminHandle"
#pragma link "BoldClientHandles"
#pragma link "BoldComClientHandles"
#pragma link "BoldHandle"
#pragma link "BoldLockManagerAdminHandleCom"
#pragma link "BoldSubscription"
#pragma resource "*.dfm"
TfrmMain *frmMain;
//---------------------------------------------------------------------------
__fastcall TfrmMain::TfrmMain(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnConnectClick(TObject *Sender)
{
  /*
  if (!dmMain->BoldComConnectionHandle1->Connected)
  {
    dmMain->BoldComConnectionHandle1->Connected = true;
    dmMain->BoldLockManagerAdminHandleCom1->Active = true;
    btnConnect->Caption = "&Disconnect";
  }
  else
  {
    dmMain->BoldLockManagerAdminHandleCom1->Active = false;
    dmMain->BoldComConnectionHandle1->Connected = false;
    btnConnect->Caption = "&Connect";
  }
  */

}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::BitBtn1Click(TObject *Sender)
{
 // dmMain->GetClients();
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::rgListClick(TObject *Sender)
{
 // dmMain->ViewAll = (rgList->ItemIndex == 0);
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::lbClientsClick(TObject *Sender)
{
  /*
  TClientInfo* clientInfo;
  TLockInfo* lockInfo;
  TListItem* item;

  clientInfo = (TClientInfo*)(lbClients->Items->Objects[lbClients->ItemIndex]);
  lbClientName->Caption = lbClients->Items->Strings[lbClients->ItemIndex];
  lvLocks->Items->BeginUpdate();
  lvLocks->Items->Clear();
  lvLocks->ViewStyle = vsReport;
  if (clientInfo->Locks)
  {
    for (int i = 0; i < clientInfo->Locks->Count; i++)
    {
      item = lvLocks->Items->Add();
      lockInfo = (TLockInfo*)(clientInfo->Locks->Objects[i]);
      item->Caption = lockInfo->LockName;
      item->SubItems->Add(lockInfo->Duration);
    }
  }
  lvLocks->Items->EndUpdate();
  */
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::cbShowLocksClick(TObject *Sender)
{
/*
  lvLocks->Visible = cbShowLocks->Checked;
  lbClient->Visible = lvLocks->Visible;
  lbClientName->Visible = lvLocks->Visible;
  lvLocks->Items->BeginUpdate();
  lvLocks->Items->Clear();
  lvLocks->Items->EndUpdate();
  if (lvLocks->Visible)
    lbClientsClick(this);
    */
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::Kill1Click(TObject *Sender)
{
  Integer clientId;

  if (lbClients->ItemIndex != -1)
  {
    if (lbClients->Items->Names[lbClients->ItemIndex] != "")
      clientId = StrToInt(Trim(lbClients->Items->Names[lbClients->ItemIndex]));
    else
      clientId = StrToInt(Trim(lbClients->Items->Strings[lbClients->ItemIndex]));
   // dmMain->KillClient(clientId);
  }

}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::ShowLocks1Click(TObject *Sender)
{
  cbShowLocksClick(Sender);
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::tsLockManagerStatusShow(TObject *Sender)
{
  /*
  if (dmMain->LockManagerSuspended)
    rgLockManagerStatus->ItemIndex = 1;
  else
    rgLockManagerStatus->ItemIndex = 0;
  */
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::tsClientsShow(TObject *Sender)
{
 // dmMain->ViewAll = (rgList->ItemIndex == 0);
  lvLocks->ViewStyle = vsReport;
}
//---------------------------------------------------------------------------


