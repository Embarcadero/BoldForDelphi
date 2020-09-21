//---------------------------------------------------------------------------

#ifndef fStartH
#define fStartH

#include "dSystem.h"
#include "dPersistence.h"
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "BoldDbEvolutorForm.hpp"
#include "BoldDbEvolutor.hpp"

//---------------------------------------------------------------------------
class TfrmStart : public TForm
{
__published:	// IDE-managed Components
  TButton *btnCreateSchema;
  TButton *btnOpenSystem;
  TButton *Button1;
  TButton *btnCancel;
  void __fastcall Button1Click(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TfrmStart(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmStart *frmStart;
//---------------------------------------------------------------------------
#endif
