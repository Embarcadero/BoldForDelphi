//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "fMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "BoldAbstractListHandle"
#pragma link "BoldAbstractModel"
#pragma link "BoldActions"
#pragma link "BoldCursorHandle"
#pragma link "BoldDBActions"
#pragma link "BoldEdit"
#pragma link "BoldHandle"
#pragma link "BoldHandleAction"
#pragma link "BoldHandles"
#pragma link "BoldImage"
#pragma link "BoldListHandle"
#pragma link "BoldMemo"
#pragma link "BoldModel"
#pragma link "BoldNavigator"
#pragma link "BoldNavigatorDefs"
#pragma link "BoldPersistenceHandle"
#pragma link "BoldPersistenceHandleDB"
#pragma link "BoldRootedHandles"
#pragma link "BoldSubscription"
#pragma link "BoldSystemHandle"
#pragma link "BoldElements"
#pragma link "BoldSystem"
#pragma link "BoldImageBitmap"
#pragma link "BoldImageJPEG"
#pragma link "BoldPlaceableSubscriber"
#pragma link "BoldControlPack"
#pragma link "BoldStringControlPack"
#pragma link "BoldViewerControlPack"
#pragma link "BoldAbstractDatabaseAdapter"
#pragma link "BoldAbstractPersistenceHandleDB"
#pragma link "BoldDatabaseAdapterIB"
#pragma link "BoldIBDatabaseAction"
#pragma resource "*.dfm"
TfrmMain *frmMain;
//---------------------------------------------------------------------------
__fastcall TfrmMain::TfrmMain(TComponent* Owner)
  : TForm(Owner)
{
  viewer = new TfrmImageViewer(this);
}
//---------------------------------------------------------------------------
__fastcall TfrmMain::~TfrmMain(void)
{
  delete viewer;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::FormCloseQuery(TObject *Sender, bool &CanClose)
{
  CanClose = true;
  if (BoldSystemHandle1->Active)
    if (BoldSystemHandle1->System->DirtyObjects->Count > 0)
      switch (MessageDlg("There are dirty objects. save them before exit?",
      mtConfirmation, TMsgDlgButtons() << mbYes << mbNo << mbCancel, 0))
      {
        case mrYes: BoldSystemHandle1->System->UpdateDatabase(); break;
        case mrNo: BoldSystemHandle1->System->Discard(); break;
        case mrCancel: CanClose = false;
      }
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::FormCreate(TObject *Sender)
{
  cboDrawFocus->ItemIndex = 1;
  chkEnabled->Checked = true;
  chkReadOnly->Checked = false;
  chkTabStop->Checked = true;
  edtContentTypeOnPaste->Text = BoldImage->ContentTypeOnPaste;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnViewClick(TObject *Sender)
{
  viewer->behImage->Value = blhImages->CurrentBoldObject->BoldMemberByExpressionName["Image"];
  viewer->Show();
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnSaveClick(TObject *Sender)
{
  if (BoldImage->Viewer)
  {
    TMetaClass* EmptyHolder = new TMetaClass();
    SavePictureDialog1->DefaultExt = BoldImage->Viewer->DefaultExtension(EmptyHolder);
    if (SavePictureDialog1->Execute())
      BoldImage->Viewer->SaveToFile( SavePictureDialog1->FileName );
    delete EmptyHolder;
  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnLoadClick(TObject *Sender)
{
  TMetaClass* EmptyHolder = new TMetaClass();
  if (BoldImage->Viewer)
    OpenPictureDialog1->DefaultExt = BoldImage->Viewer->DefaultExtension(EmptyHolder);
  if (OpenPictureDialog1->Execute())
    BoldImage->LoadFromFile(OpenPictureDialog1->FileName);
  delete EmptyHolder;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnViewClipboardFmtClick(TObject *Sender)
{
  if (frmViewClipboardFmt)
  {
    frmViewClipboardFmt->Show();
    frmViewClipboardFmt->BringToFront();
  }
  else
    frmViewClipboardFmt = new TfrmViewClipboardFmt(NULL);
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnPasteClick(TObject *Sender)
{
  BoldImage->PasteFromClipboard();
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnCopyClick(TObject *Sender)
{
  BoldImage->CopyToClipboard();
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnCutClick(TObject *Sender)
{
  BoldImage->CutToClipboard();
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::chkTabStopClick(TObject *Sender)
{
  BoldImage->TabStop = ((TCheckBox*)Sender)->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::chkReadOnlyClick(TObject *Sender)
{
  BoldImage->ReadOnly = ((TCheckBox*)Sender)->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::chkEnabledClick(TObject *Sender)
{
  BoldImage->Enabled = ((TCheckBox*)Sender)->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::edtContentTypeOnPasteChange(TObject *Sender)
{
  BoldImage->ContentTypeOnPaste = edtContentTypeOnPaste->Text;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::cboDrawFocusChange(TObject *Sender)
{
  BoldImage->DrawFocus = ( ((TComboBox*)Sender)->ItemIndex == 1 );
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnUpdateDBClick(TObject *Sender)
{
  BoldSystemHandle1->UpdateDatabase();
}
//---------------------------------------------------------------------------
