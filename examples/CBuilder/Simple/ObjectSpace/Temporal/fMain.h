//---------------------------------------------------------------------------

#ifndef fMainH
#define fMainH

#include "dModel.h"
#include "DocumentClasses.hpp"
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "BoldAbstractListHandle.hpp"
#include "BoldActions.hpp"
#include "BoldAFPPluggable.hpp"
#include "BoldCaptionController.hpp"
#include "BoldComboBox.hpp"
#include "BoldCursorHandle.hpp"
#include "BoldDBActions.hpp"
#include "BoldDerivedHandle.hpp"
#include "BoldExpressionHandle.hpp"
#include "BoldGrid.hpp"
#include "BoldHandleAction.hpp"
#include "BoldHandles.hpp"
#include "BoldListBox.hpp"
#include "BoldListHandle.hpp"
#include "BoldMemo.hpp"
#include "BoldNavigator.hpp"
#include "BoldNavigatorDefs.hpp"
#include "BoldRootedHandles.hpp"
#include "BoldSubscription.hpp"
#include "BoldVariableDefinition.hpp"
#include "BoldVariableHandle.hpp"
#include <ActnList.hpp>
#include <ExtCtrls.hpp>
#include <Grids.hpp>
#include "BoldIBDatabaseAction.hpp"
#include "BoldOclVariables.hpp"
//---------------------------------------------------------------------------
class TfrmMain : public TForm
{
__published:	// IDE-managed Components
  TGroupBox *GroupBox4;
  TBoldComboBox *BoldComboBox1;
  TBoldExpressionHandle *behCurrentUser;
  TGroupBox *GroupBox3;
  TLabel *Label8;
  TLabel *Label1;
  TButton *btnNewUser;
  TEdit *edtNewUserName;
  TBoldListHandle *blhAllUsers;
  TGroupBox *GroupBox1;
  TLabel *Label2;
  TLabel *Label3;
  TLabel *Label5;
  TBoldListBox *BoldListBox1;
  TBoldNavigator *BoldNavigator1;
  TBoldNavigator *BoldNavigator2;
  TButton *Button1;
  TBoldGrid *grdOldVersions;
  TBoldGrid *BoldGrid2;
  TButton *btnPublish;
  TBoldListHandle *blhAllProjects;
  TBoldCaptionController *BoldCaptionController1;
  TBoldListHandle *blhDocuments;
  TBoldListHandle *blhDocs;
  TBoldVariableHandle *bvhDispOld;
  TBoldPlaceableAFP *BoldPlaceableAFP1;
  TBoldDerivedHandle *bdhDocVersions;
  TBoldExpressionHandle *behAllHistoricalParts;
  TBoldDerivedHandle *bdhPartVersions;
  TBoldVariableDefinition *bvdPartVersions;
  TBoldListHandle *blhDocVersions;
  TBoldVariableDefinition *BoldVariableDefinition1;
  TBoldExpressionHandle *behDisplayDoc;
  TGroupBox *grbADocument;
  TLabel *Label4;
  TLabel *Label6;
  TBoldListBox *BoldListBox3;
  TBoldNavigator *BoldNavigator3;
  TBoldMemo *BoldMemo1;
  TBoldListHandle *blhDocParts;
  TGroupBox *GroupBox2;
  TButton *Button3;
  TButton *Button2;
  TButton *Button4;
  TActionList *ActionList1;
  TBoldActivateSystemAction *BoldActivateSystemAction1;
  TBoldUpdateDBAction *BoldUpdateDBAction1;
        TBoldIBDatabaseAction *BoldIBDatabaseAction1;
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
  void __fastcall BoldActivateSystemAction1SystemOpened(TObject *Sender);
  void __fastcall btnNewUserClick(TObject *Sender);
  void __fastcall bdhDocVersionsDeriveAndSubscribe(TComponent *Sender,
          TBoldElement *RootValue, TBoldIndirectElement *ResultElement,
          TBoldSubscriber *Subscriber);
  void __fastcall btnPublishClick(TObject *Sender);
  void __fastcall Button1Click(TObject *Sender);
  void __fastcall bdhPartVersionsDeriveAndSubscribe(TComponent *Sender,
          TBoldElement *RootValue, TBoldIndirectElement *ResultElement,
          TBoldSubscriber *Subscriber);
private:	// User declarations
public:		// User declarations
  __fastcall TfrmMain(TComponent* Owner);

  TPerson *aPerson;
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif
 