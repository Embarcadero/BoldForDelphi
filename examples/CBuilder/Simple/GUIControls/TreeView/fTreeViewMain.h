//---------------------------------------------------------------------------

#ifndef fTreeViewMainH
#define fTreeViewMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "BoldAbstractListHandle.hpp"
#include "BoldAbstractModel.hpp"
#include "BoldActions.hpp"
#include "BoldCursorHandle.hpp"
#include "BoldDBActions.hpp"
#include "BoldExpressionHandle.hpp"
#include "BoldGrid.hpp"
#include "BoldHandle.hpp"
#include "BoldHandleAction.hpp"
#include "BoldHandles.hpp"
#include "BoldListBox.hpp"
#include "BoldListHandle.hpp"
#include "BoldModel.hpp"
#include "BoldNavigator.hpp"
#include "BoldNavigatorDefs.hpp"
#include "BoldPersistenceHandle.hpp"
#include "BoldPersistenceHandleDB.hpp"
#include "BoldReferenceHandle.hpp"
#include "BoldRootedHandles.hpp"
#include "BoldSubscription.hpp"
#include "BoldSystem.hpp"
#include "BoldTreeView.hpp"
#include <ActnList.hpp>
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
#include <Graphics.hpp>
#include <Grids.hpp>
#include <ImgList.hpp>
#include "BoldEdit.hpp"
#include "TreeViewExampleClasses.hpp"
#include "BoldSystemHandle.hpp"
#include "BoldAbstractDatabaseAdapter.hpp"
#include "BoldAbstractPersistenceHandleDB.hpp"
#include "BoldDatabaseAdapterIB.hpp"
#include "BoldIBDatabaseAction.hpp"
#include <DB.hpp>
#include <IBDatabase.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
  TLabel *lblMountainbike;
  TLabel *lblFrame;
  TBoldGrid *BoldGrid1;
  TBoldListHandle *blhMTB;
  TBoldNavigator *BoldNavigator1;
  TBoldListBox *BoldListBox1;
  TBoldListHandle *blhComponents;
  TLabel *lblComponents;
  TBoldExpressionHandle *behFrame;
  TBoldReferenceHandle *brhTreeRoot;
  TImageList *imlTreeNodeIcons;
  TActionList *ActionList1;
  TBoldActivateSystemAction *BoldActivateSystemAction1;
  TBoldModel *BoldModel1;
  TBoldSystemHandle *BoldSystemHandle1;
  TBoldSystemTypeInfoHandle *BoldSystemTypeInfoHandle1;
  TGroupBox *grpFrames;
  TBoldGrid *BoldGrid2;
  TBoldNavigator *BoldNavigator2;
  TBoldListHandle *blhAllFrames;
  TGroupBox *grpGears;
  TBoldGrid *BoldGrid4;
  TBoldNavigator *BoldNavigator4;
  TBoldListHandle *blhAllGears;
  TGroupBox *grpBrakes;
  TBoldGrid *BoldGrid3;
  TBoldNavigator *BoldNavigator3;
  TBoldListHandle *blhAllBrakes;
  TGroupBox *GroupBox3;
  TBoldGrid *BoldGrid5;
  TBoldNavigator *BoldNavigator5;
  TBoldListHandle *blhAllWheels;
  TButton *btnCurrentAsRoot;
  TButton *Button1;
  TButton *Button2;
  TButton *btnShowAll;
  TPageControl *pgcTreeViews;
  TTabSheet *tsBasicTree;
  TBoldTreeView *btrvBasic;
  TTabSheet *tsEnhancedTree;
  TBoldTreeView *btrvEnhanced;
  TPanel *Panel1;
  TImage *imgUnlink;
  TBoldEdit *BoldEdit1;
        TBoldPersistenceHandleDB *BoldPersistenceHandleDB1;
        TBoldDatabaseAdapterIB *BoldDatabaseAdapterIB1;
        TIBDatabase *IBDatabase1;
        TBoldIBDatabaseAction *BoldIBDatabaseAction1;
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall btnCurrentAsRootClick(TObject *Sender);
  void __fastcall btnShowAllClick(TObject *Sender);
  void __fastcall BoldActivateSystemAction1SystemClosed(TObject *Sender);
  void __fastcall BoldActivateSystemAction1SystemOpened(TObject *Sender);
  void __fastcall OpenSystemClicked(TObject *Sender);
  void __fastcall imgUnlinkDragDrop(TObject *Sender, TObject *Source, int X, int Y);
  void __fastcall imgUnlinkDragOver(TObject *Sender, TObject *Source, int X, int Y, TDragState State, bool &Accept);
  void __fastcall btrvEnhancedDragDrop(TObject *Sender, TObject *Source, int X, int Y);
  void __fastcall btrvEnhancedDragOver(TObject *Sender, TObject *Source, int X, int Y, TDragState State, bool &Accept);
  void __fastcall pgcTreeViewsChange(TObject *Sender);
  void __fastcall btrvEnhancedStartDrag(TObject *Sender, TDragObject *&DragObject);
private:	// User declarations
  void __fastcall PopulateClass(AnsiString AClassname, AnsiString APropName, TBoldObjectList *AList);
public:		// User declarations
  __fastcall TForm1(TComponent* Owner);

  TBoldObject *DragContext;
  TMTB *TargetObject;
  TBoldTreeView *CurrentTreeView;
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
