//---------------------------------------------------------------------------

#ifndef fMainH
#define fMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "BoldAbstractListHandle.hpp"
#include "BoldAbstractModel.hpp"
#include "BoldActions.hpp"
#include "BoldComboBox.hpp"
#include "BoldCursorHandle.hpp"
#include "BoldDBActions.hpp"
#include "BoldEdit.hpp"
#include "BoldHandle.hpp"
#include "BoldHandleAction.hpp"
#include "BoldHandles.hpp"
#include "BoldListBox.hpp"
#include "BoldListHandle.hpp"
#include "BoldModel.hpp"
#include "BoldPersistenceHandle.hpp"
#include "BoldPersistenceHandleDB.hpp"
#include "BoldRootedHandles.hpp"
#include "BoldSubscription.hpp"
#include "BoldSystemHandle.hpp"
#include "BoldVariableHandle.hpp"
#include <ActnList.hpp>
#include <ExtCtrls.hpp>
#include "BoldAbstractDatabaseAdapter.hpp"
#include "BoldAbstractPersistenceHandleDB.hpp"
#include "BoldDatabaseAdapterIB.hpp"
#include <DB.hpp>
#include <IBDatabase.hpp>
#include "BoldIBDatabaseAction.hpp"
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
  TLabel *lblPerson;
  TBoldEdit *btxtPerson;
  TBoldListBox *BoldListBox1;
  TBoldListHandle *blhPerson;
  TButton *btnAdd;
  TButton *btnRemove;
  TBoldComboBox *bcboFavouriteMusic;
  TBoldComboBox *bcboPreferredFood;
  TBoldComboBox *bcboMajorsIn;
  TBoldComboBox *bcboSchool;
  TLabel *lblSchool;
  TLabel *lblMajorsIn;
  TLabel *lblPreferredFood;
  TLabel *lblFavouriteMusic;
  TGroupBox *gboFood;
  TBoldListBox *blstFood;
  TBoldEdit *btxtFood;
  TButton *btnDelFood;
  TButton *btnAddFood;
  TGroupBox *gboSchool;
  TBoldListBox *blstSchool;
  TBoldEdit *btxtSchool;
  TButton *btnDelSchool;
  TButton *btnAddSchool;
  TButton *btnSave;
  TButton *Button2;
  TButton *Button1;
  TBoldSystemTypeInfoHandle *BoldSystemTypeInfoHandle1;
  TBoldSystemHandle *BoldSystemHandle1;
  TBoldModel *BoldModel1;
  TBoldCursorHandle *bchMusic;
  TBoldListHandle *blhFood;
  TBoldCursorHandle *bchMajorTopic;
  TBoldListHandle *blhSchool;
  TBoldVariableHandle *bvhMajorTopic;
  TBoldVariableHandle *bvhMusic;
  TActionList *ActionList1;
  TBoldActivateSystemAction *BoldActivateSystemAction1;
  TBevel *Bevel1;
        TBoldPersistenceHandleDB *BoldPersistenceHandleDB1;
        TBoldDatabaseAdapterIB *BoldDatabaseAdapterIB1;
        TIBDatabase *IBDatabase1;
        TBoldIBDatabaseAction *BoldIBDatabaseAction1;
  void __fastcall BoldActivateSystemAction1SystemOpened(TObject *Sender);
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall btnAddClick(TObject *Sender);
  void __fastcall btnRemoveClick(TObject *Sender);
  void __fastcall btnAddFoodClick(TObject *Sender);
  void __fastcall btnDelFoodClick(TObject *Sender);
  void __fastcall btnAddSchoolClick(TObject *Sender);
  void __fastcall btnDelSchoolClick(TObject *Sender);
  void __fastcall btnSaveClick(TObject *Sender);
private:	// User declarations
  void __fastcall PopulateClass(AnsiString AClassname, AnsiString APropName, TBoldListHandle *AListHandle);

public:		// User declarations
  __fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
