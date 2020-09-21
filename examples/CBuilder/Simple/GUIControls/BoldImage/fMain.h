//---------------------------------------------------------------------------

#ifndef fMainH
#define fMainH

#include "ImageDemoClasses.hpp"
#include "fViewClipboardFmt.h"
#include "fViewAutoSize.h"
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ActnList.hpp>
#include <Dialogs.hpp>
#include <ExtCtrls.hpp>
#include <ExtDlgs.hpp>
#include <SysUtils.hpp>
#include "BoldAbstractListHandle.hpp"
#include "BoldAbstractModel.hpp"
#include "BoldActions.hpp"
#include "BoldCursorHandle.hpp"
#include "BoldDBActions.hpp"
#include "BoldEdit.hpp"
#include "BoldHandle.hpp"
#include "BoldHandleAction.hpp"
#include "BoldHandles.hpp"
#include "BoldImage.hpp"
#include "BoldListHandle.hpp"
#include "BoldMemo.hpp"
#include "BoldModel.hpp"
#include "BoldNavigator.hpp"
#include "BoldNavigatorDefs.hpp"
#include "BoldPersistenceHandle.hpp"
#include "BoldPersistenceHandleDB.hpp"
#include "BoldRootedHandles.hpp"
#include "BoldSubscription.hpp"
#include "BoldSystemHandle.hpp"
#include "BoldElements.hpp"
#include "BoldSystem.hpp"
#include "BoldImageBitmap.hpp"
#include "BoldImageJPEG.hpp"
#include "BoldPlaceableSubscriber.hpp"
#include "BoldControlPack.hpp"
#include "BoldStringControlPack.hpp"
#include "BoldViewerControlPack.hpp"
#include "BoldAbstractDatabaseAdapter.hpp"
#include "BoldAbstractPersistenceHandleDB.hpp"
#include "BoldDatabaseAdapterIB.hpp"
#include <DB.hpp>
#include <IBDatabase.hpp>
#include "BoldIBDatabaseAction.hpp"
//---------------------------------------------------------------------------


class TfrmMain : public TForm
{
__published:	// IDE-managed Components
  TLabel *lblBoldImage;
  TBoldEdit *btxtImageAsString;
  TGroupBox *gbxProperties;
  TLabel *lblDrawFocus;
  TLabel *lblContentTypeOnPaste;
  TComboBox *cboDrawFocus;
  TEdit *edtContentTypeOnPaste;
  TCheckBox *chkEnabled;
  TCheckBox *chkReadOnly;
  TCheckBox *chkTabStop;
  TGroupBox *gbxClipBoard;
  TButton *btnPaste;
  TButton *btnCut;
  TButton *btnCopy;
  TButton *btnViewClipboardFmt;
  TGroupBox *gbxFiles;
  TButton *btnLoad;
  TButton *btnSave;
  TButton *btnView;
  TButton *Button1;
  TButton *Button2;
  TButton *btnUpdateDB;
  TBoldNavigator *BoldNavigator1;
  TBoldMemo *bmemDescription;
  TLabel *lblDescription;
  TSavePictureDialog *SavePictureDialog1;
  TOpenPictureDialog *OpenPictureDialog1;
  TActionList *ActionList1;
  TBoldActivateSystemAction *BoldActivateSystemAction1;
  TBoldSystemTypeInfoHandle *BoldSystemTypeInfoHandle1;
  TBoldSystemHandle *BoldSystemHandle1;
  TBoldModel *BoldModel1;
  TBoldListHandle *blhImages;
  TPanel *pnlImage;
  TBoldImage *BoldImage;
        TBoldPersistenceHandleDB *BoldPersistenceHandleDB1;
        TBoldDatabaseAdapterIB *BoldDatabaseAdapterIB1;
        TIBDatabase *IBDatabase1;
        TBoldIBDatabaseAction *BoldIBDatabaseAction1;
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall btnViewClick(TObject *Sender);
  void __fastcall btnSaveClick(TObject *Sender);
  void __fastcall btnLoadClick(TObject *Sender);
  void __fastcall btnViewClipboardFmtClick(TObject *Sender);
  void __fastcall btnPasteClick(TObject *Sender);
  void __fastcall btnCopyClick(TObject *Sender);
  void __fastcall btnCutClick(TObject *Sender);
  void __fastcall chkTabStopClick(TObject *Sender);
  void __fastcall chkReadOnlyClick(TObject *Sender);
  void __fastcall chkEnabledClick(TObject *Sender);
  void __fastcall edtContentTypeOnPasteChange(TObject *Sender);
  void __fastcall cboDrawFocusChange(TObject *Sender);
  void __fastcall btnUpdateDBClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TfrmMain(TComponent* Owner);
  __fastcall ~TfrmMain(void);
  TfrmImageViewer *viewer;
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif
