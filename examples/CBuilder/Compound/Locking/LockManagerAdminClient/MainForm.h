//---------------------------------------------------------------------------

#ifndef MainFormH
#define MainFormH

//#include "dmLockManagerAdmin.h"
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <Buttons.hpp>
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
#include <Menus.hpp>
#include "BoldAbstractLockManagerAdminHandle.hpp"
#include "BoldClientHandles.hpp"
#include "BoldComClientHandles.hpp"
#include "BoldHandle.hpp"
#include "BoldLockManagerAdminHandleCom.hpp"
#include "BoldSubscription.hpp"
#include <ComCtrls.hpp>
//---------------------------------------------------------------------------
class TfrmMain : public TForm
{
__published:	// IDE-managed Components
  TLabel *Label3;
  TPageControl *PageControl1;
  TTabSheet *tsLockManagerStatus;
  TRadioGroup *rgLockManagerStatus;
  TTabSheet *tsClients;
  TLabel *Label1;
  TLabel *lbClientName;
  TLabel *lbClient;
  TListBox *lbClients;
  TCheckBox *cbShowLocks;
  TListView *lvLocks;
  TRadioGroup *rgList;
  TTabSheet *tsLocks;
  TLabel *Label2;
  TListView *ListView2;
  TCheckBox *CheckBox2;
  TListBox *ListBox2;
  TBitBtn *BitBtn2;
  TEdit *edServerName;
  TButton *btnConnect;
  TPopupMenu *pmClient;
  TMenuItem *Kill1;
  TMenuItem *ShowLocks1;
  TPopupMenu *pmLock;
  TMenuItem *Free1;
  TMenuItem *Viewclientsholdinglock1;
  void __fastcall btnConnectClick(TObject *Sender);
  void __fastcall BitBtn1Click(TObject *Sender);
  void __fastcall rgListClick(TObject *Sender);
  void __fastcall lbClientsClick(TObject *Sender);
  void __fastcall cbShowLocksClick(TObject *Sender);
  void __fastcall Kill1Click(TObject *Sender);
  void __fastcall ShowLocks1Click(TObject *Sender);
  void __fastcall tsLockManagerStatusShow(TObject *Sender);
  void __fastcall tsClientsShow(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TfrmMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif
 