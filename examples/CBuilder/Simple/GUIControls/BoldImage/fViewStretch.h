//---------------------------------------------------------------------------

#ifndef fViewStretchH
#define fViewStretchH

#include "fMain.h"
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "BoldImage.hpp"
//---------------------------------------------------------------------------
class TfrmStretch : public TForm
{
__published:	// IDE-managed Components
  TBoldImage *BoldImage;
private:	// User declarations
public:		// User declarations
  __fastcall TfrmStretch(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmStretch *frmStretch;
//---------------------------------------------------------------------------
#endif
