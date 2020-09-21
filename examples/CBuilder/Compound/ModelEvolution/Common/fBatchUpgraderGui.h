//---------------------------------------------------------------------------

#ifndef fBatchUpgraderGuiH
#define fBatchUpgraderGuiH

#include "dPersistence.h"
#include "dSystemTypeInfo.h"
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "BoldHandles.hpp"
#include "BoldLabel.hpp"
#include "BoldSubscription.hpp"
#include "BoldSystemHandle.hpp"
#include "BoldTrackBar.hpp"
#include "BoldVariableHandle.hpp"
#include "BoldBatchUpgrader.hpp"
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
#include <Math.h>
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
  TLabel *Label1;
  TLabel *lblStatus;
  TLabel *lblReport;
  TTimer *Timer1;
  TLabel *Label2;
  TBoldSystemHandle *BoldSystemHandle1;
  TButton *Button1;
  TDateTimePicker *dtpExecuteTime;
  TLabel *Label3;
  TLabel *Label4;
  TBoldLabel *BoldLabel1;
  TLabel *Label5;
  TBoldTrackBar *tbIntervalTime;
  TBoldVariableHandle *BoldVariableHandle1;
  TLabel *Label6;
  TLabel *Label7;
  TBoldLabel *BoldLabel2;
  TLabel *Label8;
  TBoldTrackBar *tbBatchSize;
  TBoldVariableHandle *BoldVariableHandle2;
  TLabel *Label9;
  TLabel *Label10;
  TBoldLabel *BoldLabel3;
  TLabel *Label11;
  TBoldTrackBar *tbSleepTime;
  TBoldVariableHandle *BoldVariableHandle3;
  void __fastcall Button1Click(TObject *Sender);
  void __fastcall Timer1Timer(TObject *Sender);
  void __fastcall FormCreate(TObject *Sender);
private:	// User declarations
  TBoldBatchUpgrader *fBatchUpgrader;
  Boolean fActive;
  void __fastcall CopyValuesFromGuiToComponents(void);
  void __fastcall GoToSleep(void);
public:		// User declarations
  __fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
