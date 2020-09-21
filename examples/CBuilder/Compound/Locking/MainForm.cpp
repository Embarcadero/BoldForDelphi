//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "MainForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "BoldAbstractListHandle"
#pragma link "BoldActions"
#pragma link "BoldAFPPluggable"
#pragma link "BoldCursorHandle"
#pragma link "BoldExceptionHandlers"
#pragma link "BoldGrid"
#pragma link "BoldHandleAction"
#pragma link "BoldHandles"
#pragma link "BoldListBox"
#pragma link "BoldListHandle"
#pragma link "BoldNavigator"
#pragma link "BoldNavigatorDefs"
#pragma link "BoldRootedHandles"
#pragma link "BoldSubscription"
#pragma link "BoldVariableHandle"
#pragma link "BoldLockHandler"
#pragma link "BoldDefs"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::BoldExceptionHandler1ApplyException(Exception *E,
      TComponent *Component, TBoldElement *Elem, bool &Discard)
{
  Integer i;
  AnsiString ConflictingObjects = "", Users = "";
  EBoldGetLocksFailed *LockException = (EBoldGetLocksFailed*)E;

  if (LockException->ClientIds)
    Users = LockException->ClientIds->CommaText;

  if (LockException->ConflictingRegions)
  {
    for (i = 0; i < LockException->ConflictingRegions->Count; i++)
      ConflictingObjects = ConflictingObjects +  LockException->ConflictingRegions->Items[i]->Root->AsString + " ";
  }

  if ( dynamic_cast<EBoldGetLocksFailed*>(E) != 0)
  {
    TVarRec v[] = {ConflictingObjects, Users, Component->Name, Elem->BoldType->ExpressionName};
    ShowMessage(Format("The change is not allowed because one or more objects are locked by other user(s). \n Objects: %s \n Users: %s \n Occured in component %s, when trying to modify the element '%s'.",
                       v, ARRAYSIZE(v) - 1));
  }
  else
    ShowMessage("Unknown exception: " + E->Message);
  Discard = true;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::cbPessimisticLockingClick(TObject *Sender)
{
  // pessimistic locking requires propagator
  if (cbPessimisticLocking->Checked)
    cbPropagator->Checked = true;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::cbPropagatorClick(TObject *Sender)
{
  // pessimistic locking requires propagator
  if (!cbPropagator->Checked)
    cbPessimisticLocking->Checked = false;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button4Click(TObject *Sender)
{
  if (!dmMainLocking->bshLocking->Active)
  {
    // Adjust remote persistence connection
    dmMainLocking->bcchPersistence->ServerHost = edtServerMachineName->Text;
    dmMainLocking->bcchPersistence->Connected = true;

    // Adjust Propagator
    dmMainLocking->bcchPropagatorServer->ServerHost = edtServerMachineName->Text;
    dmMainLocking->BoldListenerHandle1->ClientIdentifierString = edtUserName->Text;
    dmMainLocking->bcchPropagatorServer->Connected = cbPropagator->Checked;
    dmMainLocking->BoldPropagatorHandleCOM1->Active = cbPropagator->Checked;
    dmMainLocking->BoldListenerHandle1->SetActive(cbPropagator->Checked);

    // Adjust Pessimistic Locking
    dmMainLocking->bcchPropagatorServer->ServerHost = edtServerMachineName->Text;
    dmMainLocking->BoldLockManagerHandleCom1->Active = cbPessimisticLocking->Checked;
    if (cbPessimisticLocking->Checked)
      dmMainLocking->BoldLockingHandle1->SystemHandle = dmMainLocking->bshLocking;
    else
      dmMainLocking->BoldLockingHandle1->SystemHandle = NULL;

    // Adjust Optimistic Locking
    if (cbOptimisticLocking->Checked)
      dmModel->BoldModel1->MoldModel->BoldTVByName[TAG_OPTIMISTICLOCKING] = TV_OPTIMISTICLOCKING_TIMESTAMP;
    else
      dmModel->BoldModel1->MoldModel->BoldTVByName[TAG_OPTIMISTICLOCKING] = TV_OPTIMISTICLOCKING_OFF;
    dmModel->BoldModel1->SendEvent(dmModel->BoldModel1, beModelChanged);
  }
  else
  {
    dmMainLocking->bcchPersistence->Connected = false;

    dmMainLocking->bcchPropagatorServer->Connected = false;
    dmMainLocking->BoldLockManagerHandleCom1->Active = false;

    dmMainLocking->BoldListenerHandle1->SetActive(false);
    dmMainLocking->BoldPropagatorHandleCOM1->Active = false;
  }

  // Toggle the system
  dmMainLocking->bshLocking->Active = (!dmMainLocking->bshLocking->Active);

  // Adjust gui
  cbPessimisticLocking->Enabled = (!dmMainLocking->bshLocking->Active);
  cbPropagator->Enabled = (!dmMainLocking->bshLocking->Active);
  cbOptimisticLocking->Enabled = (!dmMainLocking->bshLocking->Active);
  if (dmMainLocking->bshLocking->Active)
    Button4->Caption = "Stop System";
  else
    Button4->Caption = "Start System";

}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button3Click(TObject *Sender)
{
  BoldVariableHandle1->ObjectList->Clear();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button2Click(TObject *Sender)
{
  if ( !dmMainLocking->bshLocking->System->EnsureEnclosure(BoldVariableHandle1->ObjectList, false) )
    ShowMessage("Additional objects have to be saved, they have been added to the list");
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button1Click(TObject *Sender)
{
  dmMainLocking->bshLocking->System->UpdateDatabaseWithList(BoldVariableHandle1->ObjectList);
}
//---------------------------------------------------------------------------
