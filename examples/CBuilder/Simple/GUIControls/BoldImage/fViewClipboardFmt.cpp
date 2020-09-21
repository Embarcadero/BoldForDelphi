//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "fViewClipboardFmt.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TfrmViewClipboardFmt *frmViewClipboardFmt;
//---------------------------------------------------------------------------
__fastcall TfrmViewClipboardFmt::TfrmViewClipboardFmt(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmViewClipboardFmt::FormClose(TObject *Sender,
      TCloseAction &Action)
{
  Action = caFree;
  frmViewClipboardFmt = NULL;
}
//---------------------------------------------------------------------------
void __fastcall TfrmViewClipboardFmt::FormShow(TObject *Sender)
{
  int i;
  char *Buf;

  for (i = 0; i < Clipboard()->FormatCount; i++)
  {
    switch (Clipboard()->Formats[i])
    {
      case 1: Buf = "CF_TEXT"; break;
      case 2: Buf = "CF_BITMAP"; break;
      case 3: Buf = "CF_METAFILEPICT"; break;
      case 4: Buf = "CF_SYLK"; break;
      case 5: Buf = "CF_DIF"; break;
      case 6: Buf = "CF_TIFF"; break;
      case 7: Buf = "CF_OEMTEXT"; break;
      case 8: Buf = "CF_DIB"; break;
      case 9: Buf = "CF_PALETTE"; break;
      case 10: Buf = "CF_PENDATA"; break;
      case 11: Buf = "CF_RIFF"; break;
      case 12: Buf = "CF_WAVE"; break;
      case 13: Buf = "CF_UNICODETEXT"; break;
      case 14: Buf = "CF_ENHMETAFILE"; break;
      case 15: Buf = "CF_HDROP"; break;
      case 16: Buf = "CF_LOCALE"; break;
      case 17: Buf = "CF_MAX"; break;
      case 128: Buf = "CF_OWNERDISPLAY"; break;
      case 129: Buf = "CF_DSPTEXT"; break;
      case 130: Buf = "CF_DSPBITMAP"; break;
      case 131: Buf = "CF_DSPMETAFILEPICT"; break;
      case 142: Buf = "CF_DSPENHMETAFILE"; break;
      default: Buf = "";
                    GetClipboardFormatName( Clipboard()->Formats[i], Buf, sizeof( Buf ) );
    }
    Memo1->Lines->Add( IntToStr( Clipboard()->Formats[i] )+" <"+Buf+">" );
  }
}
//---------------------------------------------------------------------------

