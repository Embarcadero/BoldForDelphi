//---------------------------------------------------------------------------

#ifndef fMainH
#define fMainH
#include "ConwayClasses.hpp"
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "BoldAbstractModel.hpp"
#include "BoldCheckBox.hpp"
#include "BoldHandles.hpp"
#include "BoldLabel.hpp"
#include "BoldMemo.hpp"
#include "BoldModel.hpp"
#include "BoldPropertiesController.hpp"
#include "BoldReferenceHandle.hpp"
#include "BoldSubscription.hpp"
#include "BoldSystemHandle.hpp"
#include "BoldTrackBar.hpp"
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TfrmMain : public TForm
{
__published:	// IDE-managed Components
  TButton *btnClear;
  TBoldReferenceHandle *refGame;
  TButton *btnTick;
  TButton *btnStart;
  TBoldTrackBar *BoldTrackBar1;
  TBoldPropertiesController *bpcTimerInterval;
  TBoldCheckBox *bcbCollecting;
  TTimer *Timer1;
  TLabel *Label1;
  TBoldLabel *BoldLabel4;
  TBoldLabel *BoldLabel3;
  TBoldLabel *BoldLabel1;
  TLabel *Label2;
  TBoldTrackBar *btbFontSize;
  TBoldPropertiesController *bpcFontSize;
  TBoldLabel *BoldLabel2;
  TLabel *Label3;
  TBoldSystemHandle *BoldSystemHandle1;
  TBoldSystemTypeInfoHandle *BoldSystemTypeInfoHandle1;
  TBoldModel *BoldModel1;
  TBoldMemo *BoldMemo1;
  void __fastcall btnClearClick(TObject *Sender);
  void __fastcall btnTickClick(TObject *Sender);
  void __fastcall btnStartClick(TObject *Sender);
  void __fastcall FormCreate(TObject *Sender);
private:	// User declarations
  TGame* __fastcall GetGame();
  __property TGame* Game = {read=GetGame};
public:		// User declarations
  __fastcall TfrmMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif
