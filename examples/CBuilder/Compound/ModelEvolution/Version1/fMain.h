//---------------------------------------------------------------------------

#ifndef fMainH
#define fMainH

#include "dSystem.h"
#include "ModelEvClasses.hpp"
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "BoldAbstractListHandle.hpp"
#include "BoldCursorHandle.hpp"
#include "BoldGrid.hpp"
#include "BoldHandles.hpp"
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
  TBoldGrid *BoldGrid1;
  TBoldListHandle *blhPersons;
  TBoldNavigator *BoldNavigator1;
  TBoldGrid *BoldGrid3;
  TBoldListHandle *blhOrders;
  TBoldNavigator *BoldNavigator3;
  TBoldGrid *BoldGrid4;
  TBoldListHandle *blhOrderItems;
  TBoldGrid *BoldGrid2;
  TBoldListHandle *blhProducts;
  TBoldNavigator *BoldNavigator2;
  TButton *Button1;
  TMainMenu *MainMenu1;
  TMenuItem *File1;
  TMenuItem *Updatedatabase1;
  TMenuItem *N2;
  TMenuItem *Exit1;
  TMenuItem *Debug1;
  TMenuItem *SystemDebugger1;
  TMenuItem *N1;
  TMenuItem *About1;
  TButton *Button2;
  void __fastcall Button1Click(TObject *Sender);
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
  void __fastcall Button2Click(TObject *Sender);
  void __fastcall Exit1Click(TObject *Sender);
  void __fastcall SystemDebugger1Click(TObject *Sender);
  void __fastcall About1Click(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TfrmMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif
 