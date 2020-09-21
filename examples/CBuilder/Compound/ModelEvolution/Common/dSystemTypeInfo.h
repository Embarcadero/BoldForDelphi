//---------------------------------------------------------------------------

#ifndef dSystemTypeInfoH
#define dSystemTypeInfoH

#include "dModel.h"
#include "ModelEvClasses.hpp"
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "BoldHandles.hpp"
#include "BoldSubscription.hpp"
//---------------------------------------------------------------------------
class TdmSystemTypeInfo : public TDataModule
{
__published:	// IDE-managed Components
  TBoldSystemTypeInfoHandle *SystemTypeInfo;
private:	// User declarations
public:		// User declarations
  __fastcall TdmSystemTypeInfo(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TdmSystemTypeInfo *dmSystemTypeInfo;
//---------------------------------------------------------------------------
#endif
