//---------------------------------------------------------------------------

#ifndef fMainH
#define fMainH

#include "dModel.h"
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "BoldAbstractListHandle.hpp"
#include "BoldActions.hpp"
#include "BoldCheckBox.hpp"
#include "BoldControlPack.hpp"
#include "BoldCursorHandle.hpp"
#include "BoldDBActions.hpp"
#include "BoldEdit.hpp"
#include "BoldElements.hpp"
#include "BoldExpressionHandle.hpp"
#include "BoldGrid.hpp"
#include "BoldHandleAction.hpp"
#include "BoldHandles.hpp"
#include "BoldListBox.hpp"
#include "BoldListHandle.hpp"
#include "BoldPersistenceNotifier.hpp"
#include "BoldRootedHandles.hpp"
#include "BoldStringControlPack.hpp"
#include "BoldSubscription.hpp"
#include <ActnList.hpp>
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
#include <Grids.hpp>
#include <Menus.hpp>
//---------------------------------------------------------------------------
class TfrmMain : public TForm
{
__published:	// IDE-managed Components
  TLabel *Label1;
  TGroupBox *PersonGroup;
  TLabel *Label2;
  TLabel *Label3;
  TLabel *FirstName;
  TLabel *Label6;
  TLabel *Label7;
  TBoldEdit *bedFirstName;
  TBoldEdit *bedPersonHome;
  TBoldListBox *blbPersonOwnedBuildings;
  TBoldEdit *bedLastName;
  TBoldEdit *bedAssets;
  TBoldCheckBox *bcbRich;
  TButton *btnUpdateDB;
  TBoldEdit *PersonCount;
  TCheckBox *CheckBox1;
  TCheckBox *CheckBox2;
  TBoldGrid *bgrPerson;
  TProgressBar *pbdbNotification;
  TPageControl *PageControl1;
  TTabSheet *tabBuilding;
  TBoldGrid *bgrBuilding;
  TGroupBox *GroupBox1;
  TLabel *Label4;
  TLabel *Label9;
  TBoldEdit *bedAddress;
  TBoldListBox *blbBuildingOwners;
  TRadioGroup *rgNameRepresentation;
  TTabSheet *tabResidentialBuilding;
  TBoldGrid *bgrResidentialBuilding;
  TGroupBox *BuildingGroup;
  TLabel *Owners;
  TLabel *Resi;
  TLabel *Label5;
  TBoldEdit *bedAddress2;
  TBoldListBox *blbBuildingResidents;
  TBoldListBox *blbBuildingOwners2;
  TButton *btnChargeRent;
  TButton *btnCreateDB;
  TButton *cmdOpen;
  TPopupMenu *BuildingPopup;
  TMenuItem *newBuilding;
  TMenuItem *Delete1;
  TMenuItem *Showinownwindow2;
  TPopupMenu *PersonPopup;
  TMenuItem *NewPerson;
  TMenuItem *DeletePerson;
  TMenuItem *Showinownwindow1;
  TPopupMenu *SingleLinkPopup;
  TMenuItem *MenuItem5;
  TPopupMenu *MultiLinkPopup;
  TMenuItem *MenuItem1;
  TMenuItem *ShowInOwnWindow3;
  TBoldListHandle *blhAllPerson;
  TBoldListHandle *blhAllBuilding;
  TBoldListHandle *blhOwnedBuildingsHandle;
  TBoldListHandle *blhOwners;
  TBoldExpressionHandle *behHome;
  TBoldAsStringRenderer *bsrRentPerResident;
  TBoldAsStringRenderer *bsrResidentsTotalAssets;
  TBoldAsStringRenderer *bsrAddress;
  TBoldListHandle *blhResidents;
  TBoldListHandle *blhAllResidentialBuilding;
  TBoldListHandle *blhOwners2;
  TBoldPersistenceProgressNotifier *BoldPersistenceProgressNotifier1;
  TActionList *ActionList1;
  TBoldActivateSystemAction *BoldActivateSystemAction1;
  TBoldUpdateDBAction *BoldUpdateDBAction1;
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
  void __fastcall BoldActivateSystemAction1SystemOpened(TObject *Sender);
  void __fastcall PopupPopup(TObject *Sender);
  void __fastcall CheckBox1Click(TObject *Sender);
  void __fastcall CheckBox2Click(TObject *Sender);
  AnsiString __fastcall bsrRentPerResidentGetAsString(
          TBoldElement *Element, int Representation,
          AnsiString Expression);
  void __fastcall bsrRentPerResidentHoldsChangedValue(
          TBoldElement *Element, int Representation, AnsiString Expression,
          TBoldSubscriber *Subscriber);
  bool __fastcall bsrRentPerResidentMayModify(TBoldElement *Element,
          int Representation, AnsiString Expression,
          TBoldSubscriber *Subscriber);
  void __fastcall bsrRentPerResidentReleaseChangedValue(
          TBoldElement *Element, int Representation, AnsiString Expression,
          TBoldSubscriber *Subscriber);
  void __fastcall bsrRentPerResidentSetAsString(TBoldElement *Element,
          AnsiString NewValue, int Representation, AnsiString Expression);
  void __fastcall bsrRentPerResidentSubscribe(TBoldElement *Element,
          int Representation, AnsiString Expression,
          TBoldSubscriber *Subscriber);
  bool __fastcall bsrRentPerResidentValidateCharacter(
          TBoldElement *Element, AnsiString Value, int Representation,
          AnsiString Expression);
  bool __fastcall bsrRentPerResidentValidateString(TBoldElement *Element,
          AnsiString Value, int Representation, AnsiString Expression);
  void __fastcall bsrAddressSetColor(TBoldElement *Element, TColor &AColor,
          int Representation, AnsiString Expression);
  void __fastcall bsrAddressSetFont(TBoldElement *Element, TFont *AFont,
          int Representation, AnsiString Expression);
  AnsiString __fastcall bsrResidentsTotalAssetsGetAsString(
          TBoldElement *Element, int Representation,
          AnsiString Expression);
  void __fastcall bsrResidentsTotalAssetsSubscribe(TBoldElement *Element,
          int Representation, AnsiString Expression,
          TBoldSubscriber *Subscriber);
  void __fastcall rgNameRepresentationClick(TObject *Sender);
  void __fastcall btnChargeRentClick(TObject *Sender);
  void __fastcall NewPersonClick(TObject *Sender);
  void __fastcall DeletePersonClick(TObject *Sender);
  void __fastcall ShowInOwnWindowClick(TObject *Sender);
  void __fastcall newBuildingClick(TObject *Sender);
  void __fastcall DeleteCurrentObjectClick(TObject *Sender);
  void __fastcall SingleItemRemove(TObject *Sender);
  void __fastcall Remove(TObject *Sender);
private:	// User declarations
  void __fastcall EnsureObjects(void);
  TBoldListHandle* __fastcall CurrentBuildingHandle();
public:		// User declarations
  __fastcall TfrmMain(TComponent* Owner);
  TComponent *Currentpopupcomponent;
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif
