//---------------------------------------------------------------------------

#ifndef FfrmReportH
#define FfrmReportH

#include "dMain.h"
#include "fMain.h"
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "BoldAbstractListHandle.hpp"
#include "BoldCursorHandle.hpp"
#include "BoldDataSet.hpp"
#include "BoldHandles.hpp"
#include "BoldListHandle.hpp"
#include "BoldRootedHandles.hpp"
#include "BoldSubscription.hpp"
#include <Db.hpp>
#include <ExtCtrls.hpp>
#include <Qrctrls.hpp>
#include <QuickRpt.hpp>
//---------------------------------------------------------------------------
class TfrmReport : public TForm
{
__published:	// IDE-managed Components
  TBoldListHandle *blhBuildings;
  TBoldListHandle *blhResidents;
  TBoldDataSet *bdsResident;
  TQuickRep *qrMain;
  TBoldDataSet *bdsBuilding;
  TQRBand *bndTitle;
  TQRLabel *QRLabel1;
  TQRLabel *QRLabel2;
  TQRSubDetail *bndBuildings;
  TQRDBText *QRDBText1;
  TQRDBText *QRDBText2;
  TQRBand *bndResidentsHeader;
  TQRLabel *QRLabel3;
  TQRLabel *QRLabel4;
  TQRSubDetail *bndResidents;
  TQRDBText *QRDBText3;
  TQRDBText *QRDBText4;
private:	// User declarations
public:		// User declarations
  __fastcall TfrmReport(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmReport *frmReport;
//---------------------------------------------------------------------------
#endif
