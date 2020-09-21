//---------------------------------------------------------------------------

#ifndef fMainH
#define fMainH
#include "DataValidationExampleClasses.hpp"
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
#include "BoldGrid.hpp"
#include "BoldHandle.hpp"
#include "BoldHandleAction.hpp"
#include "BoldHandles.hpp"
#include "BoldListBox.hpp"
#include "BoldListHandle.hpp"
#include "BoldModel.hpp"
#include "BoldPersistenceHandle.hpp"
#include "BoldPersistenceHandleBDE.hpp"
#include "BoldPersistenceHandleDB.hpp"
#include "BoldRootedHandles.hpp"
#include "BoldSubscription.hpp"
#include "BoldSystemHandle.hpp"
#include <ActnList.hpp>
#include <ExtCtrls.hpp>
#include <Grids.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
  TLabel *lblMaleStudents;
  TBevel *Bevel1;
  TButton *btnAddMale;
  TButton *btnDelMale;
  TBoldGrid *bgrdMaleStudents;
  TBoldListHandle *blhMaleStudents;
  TBoldGrid *bgrdFemaleStudents;
  TBoldListHandle *blhFemaleStudents;
  TLabel *lblFemaleStudents;
  TButton *btnAddFemale;
  TButton *btnDelFemale;
  TActionList *ActionList1;
  TBoldActivateSystemAction *BoldActivateSystemAction1;
  TBoldIBAliasAction *BoldIBAliasAction1;
  TBoldModel *BoldModel1;
  TBoldSystemHandle *BoldSystemHandle1;
  TBoldSystemTypeInfoHandle *BoldSystemTypeInfoHandle1;
  TBoldPersistenceHandleBDE *BoldPersistenceHandleBDE1;
  TBoldListBox *BoldListBox1;
  TBoldListHandle *blhInhabitants;
  TLabel *lblStudents;
  TBoldGrid *bgrdRooms;
  TLabel *lblRooms;
  TBoldListHandle *blhRooms;
  TButton *btnAddRoom;
  TButton *btnDelRoom;
  TButton *btnSave;
  TButton *Button2;
  TButton *Button1;
  TLabel *Label2;
  TLabel *Label1;
  void __fastcall btnSaveClick(TObject *Sender);
  void __fastcall BoldListBox1DragOver(TObject *Sender, TObject *Source,
          int X, int Y, TDragState State, bool &Accept);
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall btnAddMaleClick(TObject *Sender);
  void __fastcall btnDelMaleClick(TObject *Sender);
  void __fastcall btnAddFemaleClick(TObject *Sender);
  void __fastcall btnDelFemaleClick(TObject *Sender);
  void __fastcall btnDelRoomClick(TObject *Sender);
  void __fastcall btnAddRoomClick(TObject *Sender);
private:	// User declarations
  void __fastcall DeleteCurrentFromList(TBoldListHandle *AListHandle);
public:		// User declarations
  __fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
