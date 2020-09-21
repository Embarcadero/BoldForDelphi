//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Mainform.h"
#include "datamod.h"
#include "BuildingClasses.hpp"
//#include "BoldGUI,
#include "PersonAutoFormUnit.h"
#include <stdlib.h>
#include <Dialogs.hpp>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "BoldAbstractModel"
#pragma link "BoldModel"
#pragma link "BoldSubscription"
#pragma link "BoldEdit"
#pragma link "BoldAbstractListHandle"
#pragma link "BoldAFPPluggable"
#pragma link "BoldCursorHandle"
#pragma link "BoldGrid"
#pragma link "BoldHandles"
#pragma link "BoldListHandle"
#pragma link "BoldRootedHandles"
#pragma link "BoldCheckBox"
#pragma link "BoldExpressionHandle"
#pragma link "BoldListBox"
#pragma link "BoldControlPack"
#pragma link "BoldElements"
#pragma link "BoldStringControlPack"
#pragma link "BoldActions"
#pragma link "BoldDBActions"
#pragma link "BoldHandleAction"
#pragma link "BoldPersistenceNotifier"
#pragma link "BoldIBDatabaseAction"
#pragma link "BoldUndoInterfaces"
#pragma resource "*.dfm"
Tallform *allform;
//---------------------------------------------------------------------------
__fastcall Tallform::Tallform(TComponent* Owner)
        : TForm(Owner)
{
 // Currentpopupcomponent = new TComponent(this);
}
//---------------------------------------------------------------------------
void __fastcall Tallform::PopupPopup(TObject *Sender)
{
  if (dynamic_cast <TMenuItem*> (Sender))
  {
   Currentpopupcomponent = ActiveControl;
  }
  else if(dynamic_cast <TPopupMenu*> (Sender))
  {
   Currentpopupcomponent = ((TPopupMenu*)Sender)->PopupComponent;
  }
  else
  {
   Currentpopupcomponent = NULL;
  }
}
//---------------------------------------------------------------------------
void __fastcall Tallform::NewPersonClick(TObject *Sender)
{
  TPerson *nyperson = new TPerson(DataModule1->BoldSystemHandle1->System);
}
//---------------------------------------------------------------------------
void __fastcall Tallform::DeleteCurrentObjectClick(TObject *Sender)
{
  TBoldObject *BoldObject;
  TBoldListHandle *ListHandle;
  TBoldListBox *ListBox;
  TBoldGrid *Grid;

  if ((ListBox = dynamic_cast <TBoldListBox*> (Currentpopupcomponent)) != 0)
  {
    ListHandle = (TBoldListHandle*)ListBox->BoldHandle;
    if (ListHandle)
      BoldObject = ListBox->BoldHandle->CurrentBoldObject;
  }
  else if ((Grid = dynamic_cast <TBoldGrid*> (Currentpopupcomponent)) != 0)
  {
    ListHandle = (TBoldListHandle*)Grid->BoldHandle;
    if (ListHandle)
      BoldObject = Grid->BoldHandle->CurrentBoldObject;
  }
  else
    BoldObject = NULL;

  if (BoldObject)
    BoldObject->Delete();
}
//---------------------------------------------------------------------------
void __fastcall Tallform::ShowInOwnWindow(TObject *Sender)
{
  TBoldObject *BoldObject;
  TBoldListHandle *ListHandle;
  TBoldListBox *ListBox;
  TBoldGrid *Grid;

  if ((ListBox = dynamic_cast <TBoldListBox*> (Currentpopupcomponent)) != 0)
  {
    ListHandle = (TBoldListHandle*)ListBox->BoldHandle;
    if (ListHandle)
      BoldObject = ListBox->BoldHandle->CurrentBoldObject;
  }
  else if ((Grid = dynamic_cast <TBoldGrid*> (Currentpopupcomponent)) != 0)
  {
    ListHandle = (TBoldListHandle*)Grid->BoldHandle;
    if (ListHandle)
      BoldObject = Grid->BoldHandle->CurrentBoldObject;
  }
  else
    BoldObject = NULL;

  if (BoldObject)
    AutoFormProviderRegistry()->FormForElement(BoldObject)->Show();
}
//---------------------------------------------------------------------------
void __fastcall Tallform::CheckBox1Click(TObject *Sender)
{
  if (CheckBox1->Checked)
    blhAllPerson->BoldFilter = DataModule1->IsRichFilter;
  else
    blhAllPerson->BoldFilter = NULL;
}
//---------------------------------------------------------------------------
void __fastcall Tallform::CheckBox2Click(TObject *Sender)
{
  if (CheckBox2->Checked)
    blhAllPerson->BoldComparer = DataModule1->NameComparer;
  else
    blhAllPerson->BoldComparer = NULL;
}
//---------------------------------------------------------------------------
void __fastcall Tallform::FormCloseQuery(TObject *Sender, bool &CanClose)
{
  int status;
  if (!DataModule1->BoldSystemHandle1->Active)
    exit(status);
  if (DataModule1->BoldSystemHandle1->System->BoldDirty)
  {
    if (MessageDlg("You have unsaved changes, quit anyway?",
                  mtConfirmation, TMsgDlgButtons() << mbYes << mbNo, 0) == mrYes)
      DataModule1->BoldSystemHandle1->System->Discard();
    else
      CanClose = false;
  }
}
//---------------------------------------------------------------------------
void __fastcall Tallform::SingleItemRemove(TObject *Sender)
{
  TBoldEdit *ec;
  if ((ec = dynamic_cast <TBoldEdit*> (Currentpopupcomponent)) != 0)
  {
    if (dynamic_cast <TBoldObjectReference*> (ec->BoldHandle->Value))
      ((TBoldObjectReference*)ec->BoldHandle->Value)->BoldObject = NULL;
  }
}
//---------------------------------------------------------------------------
void __fastcall Tallform::Remove(TObject *Sender)
{
  TBoldListBox *lb;
  if ((lb = dynamic_cast <TBoldListBox*> (Currentpopupcomponent)) != 0)
  {
    if (lb->BoldHandle)
      lb->BoldHandle->RemoveCurrentElement();
  }
}
//---------------------------------------------------------------------------
void __fastcall Tallform::newBuildingClick(TObject *Sender)
{
  CurrentBuildingHandle()->List->AddNew();
}
//---------------------------------------------------------------------------
TBoldListHandle* __fastcall Tallform::CurrentBuildingHandle()
{
  if (PageControl1->ActivePage == tabBuilding)
    return blhAllBuilding;
  else
    return blhAllResidentialBuilding;
}
//---------------------------------------------------------------------------

void __fastcall Tallform::HighRentRendererSetColor(TBoldElement *Element,
      TColor &AColor, int Representation, AnsiString Expression)
{
  TResidential_Building *building;
  if ((building = dynamic_cast <TResidential_Building*> (Element)) != 0)
  {
    if (building->TotalRent >= 1500)
      AColor = clSilver;
    else
      AColor = clWhite;
  }
}
//---------------------------------------------------------------------------

void __fastcall Tallform::HighRentRendererSetFont(TBoldElement *Element,
      TFont *AFont, int Representation, AnsiString Expression)
{
  TResidential_Building *building;
  if ((building = dynamic_cast <TResidential_Building*> (Element)) != 0)
  {
    if (building->TotalRent >= 1500)
      AFont->Color = clSilver;
    else
      AFont->Color = clWhite;
  }
}
//---------------------------------------------------------------------------

void __fastcall Tallform::bsrAddressSetColor(TBoldElement *Element,
      TColor &AColor, int Representation, AnsiString Expression)
{
  TBuilding *building;
  if (Element)
  {
    if ((building = dynamic_cast <TBuilding*> (Element)) != 0)
    {
      if (building->Address.Pos("Bold") > 0)
        AColor = clAqua;
    }
  }
}
//---------------------------------------------------------------------------

void __fastcall Tallform::bsrAddressSetFont(TBoldElement *Element,
      TFont *AFont, int Representation, AnsiString Expression)
{
  TBuilding *building;
  if (Element)
  {
    if ((building = dynamic_cast <TBuilding*> (Element)) != 0)
    {
      if (building->M_Address->AsString.Pos("Bold") > 0)
        AFont->Style = AFont->Style + TFontStyles()<< fsBold;//fsBold];
      if (building->M_Address->AsString.Pos("Rose") > 0)
        AFont->Color = clRed;
      if (building->M_Address->AsString.Pos("Select") > 0)
        AFont->Color = clGreen;
    }
  }
}
//---------------------------------------------------------------------------

AnsiString __fastcall Tallform::bsrResidentsTotalAssetsGetAsString(
      TBoldElement *Element, int Representation, AnsiString Expression)
{
  int i;
  Currency Sum = 0;
  TResidential_Building *building;

  if ((building = dynamic_cast <TResidential_Building*> (Element)) != 0)
  {
    for (i=0; i<building->Residents->Count; i++)
      Sum = Sum + building->Residents->BoldObjects[i]->Assets;
  }
  return CurrToStr(Sum);
}
//---------------------------------------------------------------------------

void __fastcall Tallform::bsrResidentsTotalAssetsSubscribe(
      TBoldElement *Element, int Representation, AnsiString Expression,
      TBoldSubscriber *Subscriber)
{
  int i;
  TResidential_Building *building;
  TBoldExternalVariableList* VariableList = NULL;

  if ((building = dynamic_cast <TResidential_Building*> (Element)) != 0)
  {
    building->SubscribeToExpression("residents", Subscriber, true, false, VariableList);
    for(i=0; i<building->Residents->Count; i++)
      building->Residents->BoldObjects[i]->SubscribeToExpression("assets", Subscriber, false, false, VariableList);
  }
}
//---------------------------------------------------------------------------

AnsiString __fastcall Tallform::bsrRentPerResidentGetAsString(
      TBoldElement *Element, int Representation, AnsiString Expression)
{
  AnsiString Result = "";

  TResidential_Building *building;
  if ((building = dynamic_cast <TResidential_Building*> (Element)) != 0)
  {
    if (building->M_TotalRent->IsNull)
      Result = "***";
    else if ((building->Residents == NULL) || (building->Residents->Count == 0))
      Result = "No Residents";
    else
      Result = CurrToStr(building->TotalRent/building->Residents->Count);
  }
  return Result;
}
//---------------------------------------------------------------------------

void __fastcall Tallform::bsrRentPerResidentHoldsChangedValue(
      TBoldElement *Element, int Representation, AnsiString Expression,
      TBoldSubscriber *Subscriber)
{
  TResidential_Building *building;
  if ((building = dynamic_cast <TResidential_Building*> (Element)) != 0)
    building->M_TotalRent->RegisterModifiedValueHolder(Subscriber);
}
//---------------------------------------------------------------------------

bool __fastcall Tallform::bsrRentPerResidentMayModify(
      TBoldElement *Element, int Representation, AnsiString Expression,
      TBoldSubscriber *Subscriber)
{
  bool Result = false;

  TResidential_Building *building;
  if ((building = dynamic_cast <TResidential_Building*> (Element)) != 0)
  {
    if (building->Residents)
      Result = building->Residents->Count > 0;
  }
  return Result;
}
//---------------------------------------------------------------------------

void __fastcall Tallform::bsrRentPerResidentReleaseChangedValue(
      TBoldElement *Element, int Representation, AnsiString Expression,
      TBoldSubscriber *Subscriber)
{
  TResidential_Building *building;
  if ((building = dynamic_cast <TResidential_Building*> (Element)) != 0)
    building->M_TotalRent->UnRegisterModifiedValueHolder(Subscriber);
}
//---------------------------------------------------------------------------

void __fastcall Tallform::bsrRentPerResidentSetAsString(
      TBoldElement *Element, AnsiString NewValue, int Representation,
      AnsiString Expression)
{
  AnsiString v;
  v = NewValue; // avoid name clash
  TResidential_Building *building;
  if ((building = dynamic_cast <TResidential_Building*> (Element)) != 0)
    building->TotalRent = StrToCurr(v) * building->Residents->Count;
}
//---------------------------------------------------------------------------

void __fastcall Tallform::bsrRentPerResidentSubscribe(
      TBoldElement *Element, int Representation, AnsiString Expression,
      TBoldSubscriber *Subscriber)
{
  TBoldExternalVariableList* VariableList = NULL;
  if (dynamic_cast <TResidential_Building*> (Element))
  {
    Element->SubscribeToExpression("totalRent", Subscriber, false, false, VariableList);
    Element->SubscribeToExpression("residents", Subscriber, false, false, VariableList);
  }
}
//---------------------------------------------------------------------------

bool __fastcall Tallform::bsrRentPerResidentValidateCharacter(
      TBoldElement *Element, AnsiString Value, int Representation,
      AnsiString Expression)
{
  if (Value[1] == '0' || Value[1] == '1' || Value[1] == '2' || Value[1] == '3' ||
      Value[1] == '4' || Value[1] == '5' || Value[1] == '6' || Value[1] == '7' ||
      Value[1] == '8' || Value[1] == '9' || Value[1] == '-' || Value[1] == '+' ||
      Value[1] == 'e' || Value[1] == 'E' || Value[1] == DecimalSeparator)
      return true;

  return false;
}
//---------------------------------------------------------------------------

bool __fastcall Tallform::bsrRentPerResidentValidateString(
      TBoldElement *Element, AnsiString Value, int Representation,
      AnsiString Expression)
{
  try
  {
    StrToCurr(Value);
    return true;
  } catch(Exception &e)
  {
    return false;
  }
}
//---------------------------------------------------------------------------

void __fastcall Tallform::BoldActivateSystemAction1SystemOpened(
      TObject *Sender)
{
    Randomize();
}
//---------------------------------------------------------------------------

void __fastcall Tallform::btnChargeRentClick(TObject *Sender)
{
  TResidential_Building *residens;
  if ((residens = dynamic_cast <TResidential_Building*>(blhAllResidentialBuilding->CurrentBoldObject)) != 0)
    residens->ChargeRent();
}
//---------------------------------------------------------------------------


void __fastcall Tallform::rgNameRepresentationClick(TObject *Sender)
{
  switch(rgNameRepresentation->ItemIndex)
  {
    case 0: blbBuildingOwners->BoldRowProperties->Expression = "firstName";
            break;
    case 1: blbBuildingOwners->BoldRowProperties->Expression = "lastName";
            break;
    case 2: blbBuildingOwners->BoldRowProperties->Expression = "firstName + ' ' + lastName";
            break;
  }
}
//---------------------------------------------------------------------------

void __fastcall Tallform::BoldPlaceableAFP1GetFormClass(
      TBoldElement *Element, TFormClass &Result)
{
  Result = __classid(TPersonAutoForm);
}
//---------------------------------------------------------------------------

void __fastcall Tallform::BoldPlaceableAFP1RetrieveHandle(TForm *Form,
      TBoldReferenceHandle *&Result)

{
  Result = NULL;
  TPersonAutoForm *PersonForm = dynamic_cast<TPersonAutoForm*>(Form);
  if (PersonForm != NULL)
  {
    Result = PersonForm->brhPerson;
  }
}
//---------------------------------------------------------------------------

void __fastcall Tallform::btnCheckpointClick(TObject *Sender)
{
  DataModule1->BoldSystemHandle1->System->UndoHandlerInterface->SetCheckPoint();
}
//---------------------------------------------------------------------------

void __fastcall Tallform::btnUnDoClick(TObject *Sender)
{
 DataModule1->BoldSystemHandle1->System->UndoHandlerInterface->UndoLatest();
}
//---------------------------------------------------------------------------

void __fastcall Tallform::btnRedoClick(TObject *Sender)
{
  DataModule1->BoldSystemHandle1->System->UndoHandlerInterface->RedoLatest();
}
//---------------------------------------------------------------------------

