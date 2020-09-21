//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "fBatchUpgraderGui.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "BoldHandles"
#pragma link "BoldLabel"
#pragma link "BoldSubscription"
#pragma link "BoldSystemHandle"
#pragma link "BoldTrackBar"
#pragma link "BoldVariableHandle"
#pragma link "BoldBatchUpgrader"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button1Click(TObject *Sender)
{
  fActive = (!fActive);
  if (fActive)
    GoToSleep();
  else
  {
    Timer1->Enabled = false;
    Button1->Caption = "Start";
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Timer1Timer(TObject *Sender)
{
  Timer1->Enabled = false;
  lblStatus->Caption = "Upgrading";
  lblStatus->Refresh();
  CopyValuesFromGuiToComponents();
  dmPersistence->PersistenceHandle->Active = true;
  fBatchUpgrader->UpgradeObjects();

  TVarRec v[] = {fBatchUpgrader->UpgradedObjects, fBatchUpgrader->AutoUpgradedObjects};
  lblReport->Caption = Format("Upgraded: %d  AutoUpgraded: %d", v, ARRAYSIZE(v) - 1);
  dmPersistence->PersistenceHandle->Active = false;
  GoToSleep();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::FormCreate(TObject *Sender)
{
  fBatchUpgrader = new TBoldBatchUpgrader(
    dmPersistence->PersistenceHandle->PersistenceControllerDefault->PersistenceMapper,
    dmPersistence->ObjectUpgrader->ObjectUpgrader);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::CopyValuesFromGuiToComponents(void)
{
  double dtime, d;
  Timer1->Interval = (tbSleepTime->Position)*1000;
  fBatchUpgrader->BatchSize = tbBatchSize->Position;

  dtime = dtpExecuteTime->Time.operator double();
  fBatchUpgrader->MaxExecuteTime = modf(dtime, &d);
  fBatchUpgrader->IntervalBetweenBatches = (tbIntervalTime->Position)*1000;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GoToSleep(void)
{
  CopyValuesFromGuiToComponents();
  Timer1->Enabled = true;
  lblStatus->Caption = "Sleeping";
  Button1->Caption = "Stop";
}

