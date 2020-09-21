//---------------------------------------------------------------------------

#ifndef fMainH
#define fMainH

#include "dMain.h"
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "BoldAbstractListHandle.hpp"
#include "BoldCursorHandle.hpp"
#include "BoldEdit.hpp"
#include "BoldExpressionHandle.hpp"
#include "BoldGrid.hpp"
#include "BoldHandles.hpp"
#include "BoldListBox.hpp"
#include "BoldListHandle.hpp"
#include "BoldNavigator.hpp"
#include "BoldNavigatorDefs.hpp"
#include "BoldRootedHandles.hpp"
#include "BoldSubscription.hpp"
#include "BoldSystemDebuggerForm.hpp"
#include <ExtCtrls.hpp>
#include <Grids.hpp>
#include <Menus.hpp>
//---------------------------------------------------------------------------
class TfrmMain : public TForm
{
__published:	// IDE-managed Components
  TLabel *Label1;
  TBoldGrid *BoldGrid1;
  TBoldListHandle *blhPersons;
  TMainMenu *MainMenu1;
  TMenuItem *File1;
  TMenuItem *UpdateDB1;
  TMenuItem *N2;
  TMenuItem *Exit1;
  TMenuItem *Debug1;
  TMenuItem *SystemDebugger1;
  TMenuItem *N1;
  TMenuItem *About1;
  TBoldNavigator *BoldNavigator1;
  TBoldEdit *BoldEdit1;
  TBoldExpressionHandle *blhPersonHome;
  TLabel *Label3;
  TBoldListHandle *blhResidents;
  TBoldListBox *BoldListBox1;
  TLabel *Label4;
  TButton *btnGenerateReport;
  TBoldListHandle *blhBuildings;
  TLabel *Label2;
  TBoldGrid *BoldGrid2;
  TBoldNavigator *BoldNavigator2;
  void __fastcall Exit1Click(TObject *Sender);
  void __fastcall SystemDebugger1Click(TObject *Sender);
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
  void __fastcall About1Click(TObject *Sender);
  void __fastcall btnGenerateReportClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TfrmMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif
