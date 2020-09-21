//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "fMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "BoldAbstractListHandle"
#pragma link "BoldCursorHandle"
#pragma link "BoldGrid"
#pragma link "BoldHandles"
#pragma link "BoldListHandle"
#pragma link "BoldNavigator"
#pragma link "BoldNavigatorDefs"
#pragma link "BoldRootedHandles"
#pragma link "BoldSubscription"
#pragma link "BoldSystemDebuggerForm"
#pragma resource "*.dfm"
TfrmMain *frmMain;
//---------------------------------------------------------------------------
__fastcall TfrmMain::TfrmMain(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::Button1Click(TObject *Sender)
{
  TOrderItem *OrderItem;

  if ( (blhOrders->CurrentBoldObject) && (blhProducts->CurrentBoldObject) )
  {
    OrderItem = new TOrderItem(dmSystem->SystemHandle->System);
    OrderItem->Order = (TOrder*)(blhOrders->CurrentBoldObject);
    OrderItem->Product = (TProduct*)(blhProducts->CurrentBoldObject);
  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::FormCloseQuery(TObject *Sender, bool &CanClose)
{
  CanClose = true;
  if ( (dmSystem->SystemHandle->Active) && (dmSystem->SystemHandle->System->BoldDirty) )
  {
    if (MessageDlg("You have dirty objects. Do you want  to quit anyway?", mtConfirmation, TMsgDlgButtons() << mbYes << mbNo, 0) == mrYes)
      dmSystem->SystemHandle->System->Discard();
    else
      CanClose = false;
  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::Button2Click(TObject *Sender)
{
  dmSystem->SystemHandle->UpdateDatabase();
  dmSystem->SystemHandle->Active = false;
  dmSystem->SystemHandle->Active = true;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::Exit1Click(TObject *Sender)
{
  int status;
  exit(status);
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::SystemDebugger1Click(TObject *Sender)
{
  TBoldSystemDebuggerFrm *debug = new TBoldSystemDebuggerFrm(this, dmSystem->SystemHandle->System);
  debug->Show();
  debug->Free();
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::About1Click(TObject *Sender)
{
  ShowMessage("Yet another project powered by Bold technology");
}
//---------------------------------------------------------------------------

