//---------------------------------------------------------------------------

#ifndef fViewAutoSizeH
#define fViewAutoSizeH
//---------------------------------------------------------------------------
#include <SysUtils.hpp>
#include <Classes.hpp>
#include <Controls.hpp>
#include <Forms.hpp>
#include <Dialogs.hpp>
#include <StdCtrls.hpp>
#include <ComCtrls.hpp>
#include <ToolWin.hpp>
#include "BoldHandles.hpp"
#include "BoldRootedHandles.hpp"
#include "BoldImage.hpp"
#include "BoldReferenceHandle.hpp"
#include "BoldExpressionHandle.hpp"
#include "BoldSubscription.hpp"
//---------------------------------------------------------------------------
class TfrmImageViewer : public TForm
{
__published:	// IDE-managed Components
  TToolBar *ToolBar1;
  TToolButton *btnScale100;
  TToolButton *ToolButton2;
  TComboBox *cboScale;
  TToolButton *ToolButton1;
  TToolButton *btnAuto;
  TToolButton *btnScale;
  TScrollBox *ScrollBox1;
  TBoldImage *BoldImage;
  TBoldReferenceHandle *behImage;
  void __fastcall btnScale100Click(TObject *Sender);
  void __fastcall cboScaleChange(TObject *Sender);
  void __fastcall btnAutoClick(TObject *Sender);
  void __fastcall btnScaleClick(TObject *Sender);
  void __fastcall BoldImageResize(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TfrmImageViewer(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmImageViewer *frmImageViewer;
//---------------------------------------------------------------------------
#endif
