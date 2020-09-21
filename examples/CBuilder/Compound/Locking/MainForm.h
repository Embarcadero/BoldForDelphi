//---------------------------------------------------------------------------

#ifndef MainFormH
#define MainFormH

#include "dmMain.h"
#include "dModel.h"
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "BoldAbstractListHandle.hpp"
#include "BoldActions.hpp"
#include "BoldAFPPluggable.hpp"
#include "BoldCursorHandle.hpp"
#include "BoldExceptionHandlers.hpp"
#include "BoldGrid.hpp"
#include "BoldHandleAction.hpp"
#include "BoldHandles.hpp"
#include "BoldListBox.hpp"
#include "BoldListHandle.hpp"
#include "BoldNavigator.hpp"
#include "BoldNavigatorDefs.hpp"
#include "BoldRootedHandles.hpp"
#include "BoldSubscription.hpp"
#include "BoldVariableHandle.hpp"
#include "BoldLockHandler.hpp"
#include "BoldDefs.hpp"
#include <ActnList.hpp>
#include <ExtCtrls.hpp>
#include <Grids.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
  TLabel *Label1;
  TLabel *Label2;
  TLabel *Label3;
  TLabel *Label4;
  TLabel *Label5;
  TBoldGrid *BoldGrid1;
  TBoldNavigator *BoldNavigator1;
  TBoldGrid *BoldGrid2;
  TBoldNavigator *BoldNavigator2;
  TBoldGrid *BoldGrid3;
  TBoldGrid *BoldGrid4;
  TBoldNavigator *BoldNavigator3;
  TBoldNavigator *BoldNavigator4;
  TCheckBox *cbPessimisticLocking;
  TCheckBox *cbOptimisticLocking;
  TCheckBox *cbPropagator;
  TButton *Button4;
  TGroupBox *GroupBox1;
  TBoldListBox *BoldListBox1;
  TButton *Button1;
  TButton *Button2;
  TButton *Button3;
  TEdit *edtServerMachineName;
  TEdit *edtUserName;
  TButton *cmdSave;
  TBoldListHandle *blhAllItems;
  TBoldListHandle *blhAllColours;
  TBoldListHandle *blhAllOrders;
  TBoldListHandle *blhOrderLines;
  TBoldPlaceableAFP *BoldPlaceableAFP1;
  TBoldVariableHandle *BoldVariableHandle1;
  TBoldCursorHandle *BoldCursorHandle1;
  TBoldExceptionHandler *BoldExceptionHandler1;
  TActionList *ActionList1;
  TBoldUpdateDBAction *BoldUpdateDBAction1;
  void __fastcall BoldExceptionHandler1ApplyException(Exception *E,
          TComponent *Component, TBoldElement *Elem, bool &Discard);
  void __fastcall cbPessimisticLockingClick(TObject *Sender);
  void __fastcall cbPropagatorClick(TObject *Sender);
  void __fastcall Button4Click(TObject *Sender);
  void __fastcall Button3Click(TObject *Sender);
  void __fastcall Button2Click(TObject *Sender);
  void __fastcall Button1Click(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
 