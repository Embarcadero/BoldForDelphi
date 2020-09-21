//---------------------------------------------------------------------------

#ifndef MainformH
#define MainformH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "BoldAbstractModel.hpp"
#include "BoldModel.hpp"
#include "BoldSubscription.hpp"
#include "BoldEdit.hpp"
#include "BoldAbstractListHandle.hpp"
#include "BoldAFPPluggable.hpp"
#include "BoldCursorHandle.hpp"
#include "BoldGrid.hpp"
#include "BoldHandles.hpp"
#include "BoldListHandle.hpp"
#include "BoldRootedHandles.hpp"
#include <Grids.hpp>
#include <Menus.hpp>
#include "BoldCheckBox.hpp"
#include "BoldExpressionHandle.hpp"
#include "BoldListBox.hpp"
#include "BoldControlPack.hpp"
#include "BoldElements.hpp"
#include "BoldStringControlPack.hpp"
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
#include "BoldActions.hpp"
#include "BoldDBActions.hpp"
#include "BoldHandleAction.hpp"
#include "BoldPersistenceNotifier.hpp"
#include <ActnList.hpp>
#include "BoldIBDatabaseAction.hpp"
#include "BoldUndoInterfaces.hpp"
//---------------------------------------------------------------------------
class Tallform : public TForm
{
__published:	// IDE-managed Components
        TBoldModel *BoldModel1;
        TLabel *Label1;
        TBoldEdit *PersonCount;
        TBoldGrid *bgrPerson;
        TBoldListHandle *blhAllPerson;
        TPopupMenu *PersonPopup;
        TBoldPlaceableAFP *BoldPlaceableAFP1;
        TMenuItem *NewPerson;
        TMenuItem *DeletePerson;
        TMenuItem *Showinownwindow1;
        TCheckBox *CheckBox1;
        TCheckBox *CheckBox2;
        TGroupBox *PersonGroup;
        TLabel *FirstName;
        TLabel *Label2;
        TBoldEdit *bedLastName;
        TBoldEdit *bedFirstName;
        TLabel *Assets;
        TBoldEdit *bedAssets;
        TBoldCheckBox *bcbRich;
        TLabel *HomeAddress;
        TBoldEdit *bedPersonHome;
        TLabel *OwnedBuildings;
        TBoldListBox *blbPersonOwnedBuildings;
        TBoldExpressionHandle *behHome;
        TBoldListHandle *blhOwnedBuildingsHandle;
        TPopupMenu *SingleLinkPopup;
        TPopupMenu *MultiLinkPopup;
        TMenuItem *Delete1;
        TMenuItem *Delete2;
        TMenuItem *Showinownwindow2;
  TPageControl *PageControl1;
  TTabSheet *tabBuilding;
  TTabSheet *tabResidentialBuilding;
  TBoldGrid *bgrBuilding;
  TGroupBox *GroupBox1;
  TLabel *Label3;
  TLabel *Label4;
  TBoldEdit *bedAddress;
  TBoldListBox *blbBuildingOwners;
  TRadioGroup *rgNameRepresentation;
  TBoldListHandle *blhAllResidentialBuilding;
  TBoldListHandle *blhAllBuilding;
  TPopupMenu *BuildingPopup;
  TBoldAsStringRenderer *HighRentRenderer;
  TBoldAsStringRenderer *bsrRentPerResident;
  TBoldAsStringRenderer *bsrResidentsTotalAssets;
  TBoldAsStringRenderer *bsrAddress;
  TMenuItem *newBuilding;
  TMenuItem *deleteBuilding;
  TMenuItem *ShowinownwindowBuilding;
  TBoldListHandle *blhOwners2;
  TBoldListHandle *blhOwners;
  TBoldListHandle *blhResidents;
  TBoldGrid *bgrResidentialBuilding;
  TGroupBox *BuildingGroup;
  TLabel *Label5;
  TLabel *Label6;
  TLabel *Label7;
  TBoldEdit *bedAddress2;
  TBoldListBox *blbBuildingOwners2;
  TBoldListBox *blbBuildingResidents;
  TButton *btnChargeRent;
  TButton *btnUpdateDB;
  TActionList *ActionList1;
  TBoldActivateSystemAction *BoldActivateSystemAction1;
  TBoldUpdateDBAction *BoldUpdateDBAction1;
  TButton *Button1;
  TButton *Button2;
  TProgressBar *pbdbNotification;
  TBoldPersistenceProgressNotifier *BoldPersistenceProgressNotifier1;
        TBoldIBDatabaseAction *BoldIBDatabaseAction1;
        TButton *btnCheckpoint;
        TButton *btnUnDo;
        TButton *btnRedo;
  void __fastcall PopupPopup(TObject *Sender);
  void __fastcall NewPersonClick(TObject *Sender);
  void __fastcall DeleteCurrentObjectClick(TObject *Sender);
  void __fastcall ShowInOwnWindow(TObject *Sender);
  void __fastcall CheckBox1Click(TObject *Sender);
  void __fastcall CheckBox2Click(TObject *Sender);
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
  void __fastcall SingleItemRemove(TObject *Sender);
  void __fastcall Remove(TObject *Sender);
  void __fastcall newBuildingClick(TObject *Sender);
  void __fastcall HighRentRendererSetColor(TBoldElement *Element,
          TColor &AColor, int Representation, AnsiString Expression);
  void __fastcall HighRentRendererSetFont(TBoldElement *Element,
          TFont *AFont, int Representation, AnsiString Expression);
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
  void __fastcall BoldActivateSystemAction1SystemOpened(TObject *Sender);
  void __fastcall btnChargeRentClick(TObject *Sender);
  void __fastcall rgNameRepresentationClick(TObject *Sender);
        void __fastcall BoldPlaceableAFP1GetFormClass(
          TBoldElement *Element, TFormClass &Result);
        void __fastcall BoldPlaceableAFP1RetrieveHandle(TForm *Form,
          TBoldReferenceHandle *&Result);
        void __fastcall btnCheckpointClick(TObject *Sender);
        void __fastcall btnUnDoClick(TObject *Sender);
        void __fastcall btnRedoClick(TObject *Sender);
private:	// User declarations
  TBoldListHandle* __fastcall CurrentBuildingHandle();
public:		// User declarations
        __fastcall Tallform(TComponent* Owner);
  TComponent *Currentpopupcomponent;

};
//---------------------------------------------------------------------------
extern PACKAGE Tallform *allform;
//---------------------------------------------------------------------------
#endif
