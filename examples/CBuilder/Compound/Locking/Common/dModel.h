//---------------------------------------------------------------------------

#ifndef dModelH
#define dModelH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "BoldAbstractModel.hpp"
#include "BoldModel.hpp"
#include "BoldSubscription.hpp"
//---------------------------------------------------------------------------
class TdmModel : public TDataModule
{
__published:	// IDE-managed Components
  TBoldModel *BoldModel1;
private:	// User declarations
public:		// User declarations
  __fastcall TdmModel(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TdmModel *dmModel;
//---------------------------------------------------------------------------
#endif
