//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "fMain.h"
#include "ComboBoxClasses.hpp"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "BoldAbstractListHandle"
#pragma link "BoldAbstractModel"
#pragma link "BoldActions"
#pragma link "BoldComboBox"
#pragma link "BoldCursorHandle"
#pragma link "BoldDBActions"
#pragma link "BoldEdit"
#pragma link "BoldHandle"
#pragma link "BoldHandleAction"
#pragma link "BoldHandles"
#pragma link "BoldListBox"
#pragma link "BoldListHandle"
#pragma link "BoldModel"
#pragma link "BoldPersistenceHandle"
#pragma link "BoldPersistenceHandleDB"
#pragma link "BoldRootedHandles"
#pragma link "BoldSubscription"
#pragma link "BoldSystemHandle"
#pragma link "BoldVariableHandle"
#pragma link "BoldAbstractDatabaseAdapter"
#pragma link "BoldAbstractPersistenceHandleDB"
#pragma link "BoldDatabaseAdapterIB"
#pragma link "BoldIBDatabaseAction"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::BoldActivateSystemAction1SystemOpened(TObject *Sender)
{
  if (blhFood->List->Count < 1)
    PopulateClass("Food", "Name", blhFood);
  if (blhSchool->List->Count < 1)
    PopulateClass("School", "Name", blhSchool);
  if (BoldSystemHandle1->System->DirtyObjects->Count > 0)
    BoldSystemHandle1->System->UpdateDatabase();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormCloseQuery(TObject *Sender, bool &CanClose)
{
  CanClose = true;
  if (BoldSystemHandle1->Active)
    if (BoldSystemHandle1->System->DirtyObjects->Count > 0)
      switch (MessageDlg("There are dirty objects. save them before exit?",
      mtConfirmation, TMsgDlgButtons() << mbYes << mbNo << mbCancel, 0))
      {
        case mrYes: BoldSystemHandle1->System->UpdateDatabase(); break;
        case mrNo: BoldSystemHandle1->System->Discard(); break;
        case mrCancel: CanClose = false;
      }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender)
{
  Width = 354;
  Height = 393;
  Randomize();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btnAddClick(TObject *Sender)
{
  TPerson *person = new TPerson(BoldSystemHandle1->System);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btnRemoveClick(TObject *Sender)
{
  TBoldObject *boldObject;

  if((boldObject = dynamic_cast<TBoldObject*>(blhPerson->CurrentBoldObject)) != 0)
  {
    boldObject->UnLinkAll();
    boldObject->Delete();
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btnAddFoodClick(TObject *Sender)
{
  blhFood->List->AddNew();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btnDelFoodClick(TObject *Sender)
{
  TBoldObject *boldObject;

  if ((boldObject = dynamic_cast<TBoldObject*>(blhFood->CurrentBoldObject)) != 0)
  {
    boldObject->UnLinkAll();
    boldObject->Delete();
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btnAddSchoolClick(TObject *Sender)
{
  blhSchool->List->AddNew();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btnDelSchoolClick(TObject *Sender)
{
  blhSchool->CurrentBoldObject->Delete();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btnSaveClick(TObject *Sender)
{
  BoldSystemHandle1->UpdateDatabase();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::PopulateClass(AnsiString AClassName, AnsiString APropName, TBoldListHandle *AListHandle)
{
  TStrings *NameStrings = new TStringList();
  int i;
  TBoldElement *AElement;

  NameStrings->LoadFromFile(AClassName + ".txt");
  for (i = 0; i < NameStrings->Count; i++)
  {
    AElement = AListHandle->List->AddNew();
    ((TBoldObject*)AElement)->BoldMemberByExpressionName[APropName]->AsString = NameStrings->Strings[i];
  }
}
//---------------------------------------------------------------------------

