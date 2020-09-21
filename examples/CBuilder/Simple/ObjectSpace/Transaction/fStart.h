//---------------------------------------------------------------------------

#ifndef fStartH
#define fStartH

#include "dMain.h"
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
//---------------------------------------------------------------------------
class TfrmStart : public TForm
{
__published:	// IDE-managed Components
  TButton *btnCancel;
        TButton *cmdCreate;
        TButton *cmdOpen;
private:	// User declarations
public:		// User declarations
  __fastcall TfrmStart(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmStart *frmStart;
//---------------------------------------------------------------------------
#endif
