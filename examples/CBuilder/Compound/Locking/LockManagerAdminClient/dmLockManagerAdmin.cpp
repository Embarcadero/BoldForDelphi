//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "dmLockManagerAdmin.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "BoldAbstractLockManagerAdminHandle"
#pragma link "BoldClientHandles"
#pragma link "BoldComClientHandles"
#pragma link "BoldHandle"
#pragma link "BoldLockManagerAdminHandleCom"
#pragma link "BoldSubscription"
#pragma link "BoldUtils"
#pragma resource "*.dfm"
TdmMain *dmMain;
//---------------------------------------------------------------------------
__fastcall TdmMain::TdmMain(TComponent* Owner)
  : TDataModule(Owner)
{
}
//---------------------------------------------------------------------------
void FreeObjects(const TStringList* List)
{
  Integer i;
  TObject* obj;

  if (!List)
    exit(i);

  for (i = 0; i < List->Count; i++)
  {
    obj = List->Objects[i];
    obj->Free(); // FreeAndNil(obj);
  }
}
//---------------------------------------------------------------------------
void AssignObjects(const TStringList* Source, TStringList* Destination )
{
  if ( (Source) && (Destination) )
    for (int i = 0; i < Source->Count; i++)
      Destination->Objects[i] = Source->Objects[i];
}
//---------------------------------------------------------------------------
/* TClientInfo */
__fastcall TClientInfo::TClientInfo(AnsiString IdString)
  : TObject()
{
  fIdString = IdString;
  fLocks = new TStringList();
}

__fastcall TClientInfo::~TClientInfo()
{
  FreeObjects(fLocks);
  // FreeAndNil(fLocks);
}
//---------------------------------------------------------------------------
/* TLockInfo */
__fastcall TLockInfo::TLockInfo(AnsiString Duration, AnsiString LockName)
  : TObject()
{
  fDuration = Duration;
  fLockName = LockName;
  fClients = new TStringList();
}

__fastcall TLockInfo::~TLockInfo()
{
  FreeObjects(fClients);
  // FreeAndNil(fClients);
}
//---------------------------------------------------------------------------
void __fastcall TdmMain::GetClients()
{
  TStringList *lClients, *lLocks, *lLockDurations;
  OleVariant vClients, vLocks, vLockDurations;
  TClientInfo* clientInfo;
  TLockInfo* lockInfo;
  Integer i, j, idx;
  long l;

  if (BoldComConnectionHandle1->Connected)
  {
    lClients = new TStringList();
    lLocks = new TStringList();
    lLockDurations = new TStringList();
    try
    {
      if (!fClients)
        fClients = new TStringList();
      if (!fLocks)
        fLocks = new TStringList();
      if (!BoldLockManagerAdminHandleCom1->Active)
        BoldLockManagerAdminHandleCom1->Active = true;

      // listallclients         (1)
      if (ViewAll)
        BoldLockManagerAdminHandleCom1->LockManagerAdmin->ListAllClients(vClients, l);
      else
        BoldLockManagerAdminHandleCom1->LockManagerAdmin->ListLockingClients(vClients, l);

      BoldVariantToStrings(vClients, lClients);

      // get locks for clients
      BoldLockManagerAdminHandleCom1->LockManagerAdmin->LocksForClients(vClients, vLocks, vLockDurations, l);
      BoldVariantToStrings(vLocks, lLocks);
      BoldVariantToStrings(vLockDurations, lLockDurations);

      if (lLocks)
        for (i = 0; i < lLocks->Count; i++)
        {
          lockInfo = new TLockInfo(lLockDurations->Strings[i], lLocks->Values[lLocks->Names[i]]);
          lLocks->Strings[i] = lLocks->Names[i];
          lLocks->Objects[i] = lockInfo;
        }

      if (lClients)
        for (i = 0; i < lClients->Count; i++)
        {
          clientInfo = new TClientInfo(lClients->Values[lClients->Names[i]]);
          lClients->Strings[i] = lClients->Names[i];
          lClients->Objects[i] = clientInfo;

          j = lLocks->IndexOf(lClients->Strings[i]);
          while (j != -1)
          {
            idx = clientInfo->Locks->Add(lLocks->Strings[j]);
            clientInfo->Locks->Strings[j] = ((TLockInfo*)(lLocks->Objects[j]))->LockName;
            clientInfo->Locks->Objects[idx] = lLocks->Objects[j];
            lLocks->Delete(j);
            j = lLocks->IndexOf(lClients->Strings[i]);
          }
        }

      Clients = lClients;
    }
    __finally
    {
      lClients->Free(); // FreeAndNil(lClients);
      lLocks->Free(); // FreeAndNil(lLocks);
      lLockDurations->Free(); // FreeAndNil(lLockDurations);
    }
  }
}
//---------------------------------------------------------------------------
Boolean __fastcall TdmMain::GetLockManagerSuspended()
{
  Boolean result = false;
  if (BoldComConnectionHandle1->Connected)
  {
    if (!BoldLockManagerAdminHandleCom1->Active)
      BoldLockManagerAdminHandleCom1->Active = true;
    result = BoldLockManagerAdminHandleCom1->LockManagerAdmin->LockManagerSuspended;
  }
  return result;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TdmMain::GetServerName()
{
  return Trim(frmMain->edServerName->Text);
}
//---------------------------------------------------------------------------
void __fastcall TdmMain::KillClient(Integer ClientId)
{
  try
  {
    if (BoldComConnectionHandle1->Connected)
    {
      if (!BoldLockManagerAdminHandleCom1->Active)
        BoldLockManagerAdminHandleCom1->Active = true;
      long l;
      BoldLockManagerAdminHandleCom1->LockManagerAdmin->KillClient(ClientId, l);
      dmMain->GetClients();
    }
  } catch (Exception &e)
  {
    throw e;
  }
}
//----------------------------------------------------------------------------
void __fastcall TdmMain::SetClients(const TStringList* Value)
{
  Integer i, idx;

  FreeObjects(fClients);
  fClients->Clear();
  fClients->Assign((TPersistent*)Value);
  AssignObjects(Value, fClients);
  if ( (frmMain) && (frmMain->PageControl1->ActivePage->Name == "tsClients") )
  {
    frmMain->lbClients->Clear();
    for (i = 0; i < fClients->Count; i++)
    {
      //TVarRec v[] = { fClients->Strings[i], ((TClientInfo*)(fClients->Objects[i]))->IdString };
      idx = frmMain->lbClients->Items->Add( fClients->Strings[i] + "=" + ((TClientInfo*)(fClients->Objects[i]))->IdString );// Format("%s=%s", v, ARRAYSIZE(v) - 1));
      frmMain->lbClients->Items->Objects[idx] = fClients->Objects[i];
    }
  }
}
//----------------------------------------------------------------------------
void __fastcall TdmMain::SetLockManagerSuspended(const Boolean Value)
{
  if (BoldComConnectionHandle1->Connected)
  {
    if (!BoldLockManagerAdminHandleCom1->Active)
      BoldLockManagerAdminHandleCom1->Active = true;
    BoldLockManagerAdminHandleCom1->LockManagerAdmin->LockManagerSuspended = Value;
  }
}
//----------------------------------------------------------------------------
void __fastcall TdmMain::SetLocks(const TStringList* Value)
{
  FreeObjects(fLocks);
  fLocks->Clear();
  fLocks->Assign((TPersistent*)Value);
  AssignObjects(Value, fLocks);
}
//----------------------------------------------------------------------------
void __fastcall TdmMain::SetViewAll(const Boolean Value)
{
  if (Value != fViewAll)
  {
    fViewAll = Value;
    GetClients();
  }
}


