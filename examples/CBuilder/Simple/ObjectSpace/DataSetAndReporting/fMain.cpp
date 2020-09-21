//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "fMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "BoldAbstractListHandle"
#pragma link "BoldCursorHandle"
#pragma link "BoldEdit"
#pragma link "BoldExpressionHandle"
#pragma link "BoldGrid"
#pragma link "BoldHandles"
#pragma link "BoldListBox"
#pragma link "BoldListHandle"
#pragma link "BoldNavigator"
#pragma link "BoldNavigatorDefs"
#pragma link "BoldRootedHandles"
#pragma link "BoldSubscription"
#pragma ling "BoldSystemDebuggerForm"
#pragma resource "*.dfm"
TfrmMain *frmMain;
//---------------------------------------------------------------------------
__fastcall TfrmMain::TfrmMain(TComponent* Owner)
  : TForm(Owner)
{
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
  TBoldSystemDebuggerFrm *debug = new TBoldSystemDebuggerFrm(this, dmMain->bshMain->System);
  debug->Show();
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::FormCloseQuery(TObject *Sender, bool &CanClose)
{
  CanClose = true;
  if ((dmMain->bshMain->Active) && (dmMain->bshMain->System->BoldDirty))
  {
   if (MessageDlg("You have unsaved changes, quit anyway?",
                  mtConfirmation, TMsgDlgButtons() << mbYes << mbNo, 0) == mrYes)
      dmMain->bshMain->System->Discard();
   else
      CanClose = false;
  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::About1Click(TObject *Sender)
{
  ShowMessage(AnsiString("Yet another project powered by Bold technology"));
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::btnGenerateReportClick(TObject *Sender)
{
  TfrmReport *report;
  report = new TfrmReport(this);
  report->bdsBuilding->Active = true;
  report->bdsResident->Active = true;
  report->qrMain->Preview();

  delete report;
}
//---------------------------------------------------------------------------

