//---------------------------------------------------------------------------

#ifndef dModelH
#define dModelH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "BoldAbstractModel.hpp"
#include "BoldHandle.hpp"
#include "BoldModel.hpp"
#include "BoldSubscription.hpp"
#include "BoldUMLModelLink.hpp"
#include "BoldUMLRose98Link.hpp"
//---------------------------------------------------------------------------
class TdmModel : public TDataModule
{
__published:	// IDE-managed Components
  TBoldModel *Model;
  TBoldUMLRose98Link *BoldUMLRose98Link1;
private:	// User declarations
public:		// User declarations
  __fastcall TdmModel(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TdmModel *dmModel;
//---------------------------------------------------------------------------
#endif
