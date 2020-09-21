//---------------------------------------------------------------------------

#ifndef dSystemH
#define dSystemH

#include "dSystemTypeInfo.h"
#include "dPersistence.h"

//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "BoldHandles.hpp"
#include "BoldSubscription.hpp"
#include "BoldSystemHandle.hpp"
#include <ActnList.hpp>
#include "BoldActions.hpp"
#include "BoldDBActions.hpp"
#include "BoldHandleAction.hpp"
//---------------------------------------------------------------------------
class TdmSystem : public TDataModule
{
__published:	// IDE-managed Components
  TBoldSystemHandle *SystemHandle;
  TActionList *ActionList1;
  TBoldUpdateDBAction *BoldUpdateDBAction1;
  TBoldActivateSystemAction *BoldActivateSystemAction1;
  TBoldIBAliasAction *BoldIBAliasAction1;
private:	// User declarations
public:		// User declarations
  __fastcall TdmSystem(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TdmSystem *dmSystem;
//---------------------------------------------------------------------------
#endif
