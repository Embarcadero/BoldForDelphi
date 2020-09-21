//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "fViewAutoSize.h"
//---------------------------------------------------------------------------
#pragma link "BoldHandles"
#pragma link "BoldRootedHandles"
#pragma link "BoldImage"
#pragma link "BoldReferenceHandle"
#pragma link "BoldExpressionHandle"
#pragma link "BoldSubscription"

#pragma resource "*.dfm"
TfrmImageViewer *frmImageViewer;
//---------------------------------------------------------------------------
__fastcall TfrmImageViewer::TfrmImageViewer(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmImageViewer::btnScale100Click(TObject *Sender)
{
  btnScale->Click();
  btnScale->Down = true;
  BoldImage->Scale = 100;
}
//---------------------------------------------------------------------------
void __fastcall TfrmImageViewer::cboScaleChange(TObject *Sender)
{
  btnScale->Click();
  btnScale->Down = true;
  BoldImage->Scale = StrToInt(cboScale->Text);
}
//---------------------------------------------------------------------------
void __fastcall TfrmImageViewer::btnAutoClick(TObject *Sender)
{
  BoldImage->StretchMode = bsmStretchProportional;
  BoldImage->AutoSize = false;
  BoldImage->Align = alClient;
}
//---------------------------------------------------------------------------
void __fastcall TfrmImageViewer::btnScaleClick(TObject *Sender)
{
  BoldImage->StretchMode = bsmStretchToScale;
  BoldImage->AutoSize = true;
  BoldImage->Align = alNone;
}
//---------------------------------------------------------------------------
void __fastcall TfrmImageViewer::BoldImageResize(TObject *Sender)
{
  BoldImage->Repaint();
  cboScale->Text = IntToStr(BoldImage->Scale);
}
//---------------------------------------------------------------------------
