//---------------------------------------------------------------------------

#ifndef ServerFormH
#define ServerFormH

#include "dModel.h"
#include "PServerDM.h"
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
//---------------------------------------------------------------------------
class TfrmServer : public TForm
{
__published:	// IDE-managed Components
private:	// User declarations
public:		// User declarations
  __fastcall TfrmServer(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmServer *frmServer;
//---------------------------------------------------------------------------
#endif
