//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "FfrmReport.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "BoldAbstractListHandle"
#pragma link "BoldCursorHandle"
#pragma link "BoldDataSet"
#pragma link "BoldHandles"
#pragma link "BoldListHandle"
#pragma link "BoldRootedHandles"
#pragma link "BoldSubscription"
#pragma resource "*.dfm"
TfrmReport *frmReport;
//---------------------------------------------------------------------------
__fastcall TfrmReport::TfrmReport(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
