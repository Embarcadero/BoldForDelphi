//---------------------------------------------------------------------------

#ifndef dmLockManagerAdminH
#define dmLockManagerAdminH

#include "MainForm.h"
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "BoldAbstractLockManagerAdminHandle.hpp"
#include "BoldClientHandles.hpp"
#include "BoldComClientHandles.hpp"
#include "BoldHandle.hpp"
#include "BoldLockManagerAdminHandleCom.hpp"
#include "BoldSubscription.hpp"
#include "BoldUtils.hpp"
//---------------------------------------------------------------------------
class TdmMain : public TDataModule
{
__published:	// IDE-managed Components
  TBoldComConnectionHandle *BoldComConnectionHandle1;
  TBoldLockManagerAdminHandleCom *BoldLockManagerAdminHandleCom1;
private:	// User declarations
  TStringList* fClients;
  TStringList* fLocks;
  Boolean fViewAll;
  void __fastcall SetLocks(const TStringList* Value);
  void __fastcall SetClients(const TStringList* Value);
  void __fastcall SetViewAll(const Boolean Value);
  Boolean __fastcall GetLockManagerSuspended(void);
  void __fastcall SetLockManagerSuspended(const Boolean Value);
  AnsiString __fastcall GetServerName(void);
public:		// User declarations
  __fastcall TdmMain(TComponent* Owner);
  void __fastcall GetClients(void);
  void __fastcall KillClient(Integer ClientId);
  __property Boolean LockManagerSuspended = {read=GetLockManagerSuspended, write=SetLockManagerSuspended};
  __property TStringList* Clients = {read=fClients, write=SetClients};
  __property TStringList* Locks = {read=fLocks, write=SetLocks};
  __property Boolean ViewAll = {read=fViewAll, write=SetViewAll};
  __property AnsiString ServerName = {read=GetServerName};
};

class TClientInfo : public TObject
{
private:
  AnsiString fIdString;
  TStringList* fLocks;
public:
  __fastcall TClientInfo(AnsiString IdString);
  __fastcall ~TClientInfo(void);
  __property AnsiString IdString = {read=fIdString, write=fIdString};
  __property TStringList* Locks = {read=fLocks, write=fLocks};
};

class TLockInfo : public TObject
{
private:
  AnsiString fDuration;
  AnsiString fLockName;
  TStringList* fClients;
public:
  __fastcall TLockInfo(AnsiString Duration, AnsiString LockName);
  __fastcall ~TLockInfo(void);
  __property AnsiString Duration = {read=fDuration, write=fDuration};
  __property AnsiString LockName = {read=fLockName, write=fLockName};
};

void FreeObjects(const TStringList* List);
void AssignObjects(const TStringList* Source, TStringList* Destination);

//---------------------------------------------------------------------------
extern PACKAGE TdmMain *dmMain;
//---------------------------------------------------------------------------
#endif
