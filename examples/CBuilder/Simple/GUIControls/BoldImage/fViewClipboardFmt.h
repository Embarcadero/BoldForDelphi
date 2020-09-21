//---------------------------------------------------------------------------

#ifndef fViewClipboardFmtH
#define fViewClipboardFmtH
//---------------------------------------------------------------------------
#include <Windows.hpp>
#include <SysUtils.hpp>
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <Dialogs.hpp>
#include <Clipbrd.hpp>
#include <StdCtrls.hpp>
//---------------------------------------------------------------------------
class TfrmViewClipboardFmt : public TForm
{
__published:	// IDE-managed Components
  TMemo *Memo1;
  void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
  void __fastcall FormShow(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TfrmViewClipboardFmt(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmViewClipboardFmt *frmViewClipboardFmt;
//---------------------------------------------------------------------------
#endif
