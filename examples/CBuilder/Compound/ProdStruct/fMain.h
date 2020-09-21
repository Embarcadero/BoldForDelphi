//---------------------------------------------------------------------------

#ifndef fMainH
#define fMainH
#include "dm1.h"
#include "ProdStructClasses.hpp"
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "BoldAbstractListHandle.hpp"
#include "BoldActions.hpp"
#include "BoldCursorHandle.hpp"
#include "BoldDBActions.hpp"
#include "BoldGrid.hpp"
#include "BoldHandleAction.hpp"
#include "BoldHandles.hpp"
#include "BoldListBox.hpp"
#include "BoldListHandle.hpp"
#include "BoldNavigator.hpp"
#include "BoldNavigatorDefs.hpp"
#include "BoldReferenceHandle.hpp"
#include "BoldRootedHandles.hpp"
#include "BoldSubscription.hpp"
#include "BoldTreeView.hpp"
#include <ActnList.hpp>
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
#include <Grids.hpp>
#include <ImgList.hpp>
#include "BoldIBDatabaseAction.hpp"
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
  TPanel *Panel1;
  TBevel *Bevel1;
  TLabel *Products;
  TLabel *Label1;
  TLabel *Label2;
  TLabel *Label3;
  TLabel *Label4;
  TLabel *lblNavigator;
  TBoldGrid *grdProducts;
  TBoldGrid *grdSimpleProducts;
  TBoldGrid *grdAssemblies;
  TBoldListBox *lbAssemblyParts;
  TBoldListBox *lsProductPartOf;
  TButton *btnUpdateDatabase;
  TBoldNavigator *nvgShared;
  TButton *btnChangeRoot;
  TButton *btnShowAll;
  TButton *Button1;
  TButton *Button2;
  TBoldListHandle *hdlAllProducts;
  TBoldListHandle *hdlProductPartOf;
  TBoldReferenceHandle *hdlTreeRoot;
  TImageList *ImageList1;
  TActionList *ActionList1;
  TBoldActivateSystemAction *BoldActivateSystemAction1;
  TBoldUpdateDBAction *BoldUpdateDBAction1;
  TBoldListHandle *hdlAllSimpleProducts;
  TBoldListHandle *hdlAllAssemblies;
  TBoldListHandle *hdlAssemblyParts;
  TBoldTreeView *BoldTreeView1;
  TSplitter *Splitter1;
        TBoldIBDatabaseAction *BoldIBDatabaseAction1;
  void __fastcall BoldActivateSystemAction1SystemOpened(TObject *Sender);
  void __fastcall btnChangeRootClick(TObject *Sender);
  void __fastcall btnShowAllClick(TObject *Sender);
  void __fastcall lbAssemblyPartsKeyPress(TObject *Sender, char &Key);
  void __fastcall grdProductsEnter(TObject *Sender);
  void __fastcall grdSimpleProductsEnter(TObject *Sender);
  void __fastcall grdAssembliesDblClick(TObject *Sender);
  void __fastcall grdAssembliesEnter(TObject *Sender);
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
private:	// User declarations
  TSimple_Product *Blade, *Handle;
  TAssembly *Knife, *KnifeKit;
public:		// User declarations
  __fastcall TForm1(TComponent* Owner);
  __fastcall ~TForm1(void);

};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
