//---------------------------------------------------------------------------

#ifndef fMainH
#define fMainH

#include "BankingClasses.hpp"
#include "dMain.h"
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "BoldAbstractListHandle.hpp"
#include "BoldAFPPluggable.hpp"
#include "BoldComboBox.hpp"
#include "BoldCursorHandle.hpp"
#include "BoldEdit.hpp"
#include "BoldExpressionHandle.hpp"
#include "BoldHandles.hpp"
#include "BoldListBox.hpp"
#include "BoldListHandle.hpp"
#include "BoldNavigator.hpp"
#include "BoldNavigatorDefs.hpp"
#include "BoldPageControl.hpp"
#include "BoldRootedHandles.hpp"
#include "BoldSubscription.hpp"
#include "BoldSystem.hpp"
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
#include <Menus.hpp>
//---------------------------------------------------------------------------
class TfrmMain : public TForm
{
__published:	// IDE-managed Components
  TGroupBox *GroupBox1;
  TLabel *Label1;
  TLabel *Label2;
  TLabel *Label3;
  TBoldListBox *BoldListBox1;
  TBoldNavigator *BoldNavigator1;
  TBoldEdit *BoldEdit1;
  TBoldEdit *BoldEdit2;
  TBoldEdit *BoldEdit3;
  TGroupBox *GroupBox2;
  TLabel *Label4;
  TLabel *Label10;
  TBoldListBox *BoldListBox2;
  TButton *btnRun;
  TButton *btnTransfer;
  TButton *btnModifyCredit;
  TButton *btnCloseAccount;
  TBoldPageControl *BoldPageControl1;
  TTabSheet *tsTransfer;
  TLabel *Label5;
  TLabel *Label6;
  TLabel *Label7;
  TBoldComboBox *BoldComboBox1;
  TBoldComboBox *BoldComboBox2;
  TBoldEdit *BoldEdit4;
  TTabSheet *tsClose;
  TLabel *Label11;
  TBoldComboBox *BoldComboBox3;
  TTabSheet *tsModifyCredit;
  TLabel *Label8;
  TLabel *Label9;
  TBoldComboBox *BoldComboBox4;
  TBoldEdit *BoldEdit5;
  TButton *btnDelete;
  TMainMenu *MainMenu1;
  TMenuItem *File1;
  TMenuItem *Exit1;
  TBoldListHandle *blhAllAccounts;
  TBoldListHandle *blhAllRequests;
  TBoldPlaceableAFP *BoldPlaceableAFP1;
  TBoldExpressionHandle *behTransfer;
  TBoldExpressionHandle *behClose;
  TBoldExpressionHandle *behModifyCredit;
        TButton *Button1;
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
  void __fastcall btnRunClick(TObject *Sender);
  void __fastcall btnDeleteClick(TObject *Sender);
  void __fastcall btnTransferClick(TObject *Sender);
  void __fastcall btnModifyCreditClick(TObject *Sender);
  void __fastcall btnCloseAccountClick(TObject *Sender);
  void __fastcall Exit1Click(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TfrmMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif
