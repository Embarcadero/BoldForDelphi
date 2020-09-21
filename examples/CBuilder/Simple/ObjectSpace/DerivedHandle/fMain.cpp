//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "fMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "BoldAbstractListHandle"
#pragma link "BoldAbstractModel"
#pragma link "BoldActions"
#pragma link "BoldCursorHandle"
#pragma link "BoldDBActions"
#pragma link "BoldDerivedHandle"
#pragma link "BoldEdit"
#pragma link "BoldGrid"
#pragma link "BoldHandle"
#pragma link "BoldHandleAction"
#pragma link "BoldHandles"
#pragma link "BoldListHandle"
#pragma link "BoldModel"
#pragma link "BoldNavigator"
#pragma link "BoldNavigatorDefs"
#pragma link "BoldPersistenceHandle"
#pragma link "BoldPersistenceHandleBDE"
#pragma link "BoldPersistenceHandleDB"
#pragma link "BoldRootedHandles"
#pragma link "BoldSubscription"
#pragma link "BoldSystemHandle"
#pragma link "BoldTrackBar"
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
void __fastcall TForm1::FormCloseQuery(TObject *Sender, bool &CanClose)
{
  CanClose = true;
  if ((BoldSystemHandle1->Active) &&
   (BoldSystemHandle1->System->DirtyObjects->Count > 0))
    switch (MessageDlg("There are dirty objects. Save them before exit?", mtConfirmation, TMsgDlgButtons() << mbYes << mbNo << mbCancel, 0))
    {
      case mrYes: BoldSystemHandle1->System->UpdateDatabase(); break;
      case mrNo: BoldSystemHandle1->System->Discard(); break;
      case mrCancel: CanClose = false;
    }
}
//---------------------------------------------------------------------------

void __fastcall TForm1::FormCreate(TObject *Sender)
{
  Height = 555;
  Width  = 339;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::BoldActivateSystemAction1SystemClosed(
      TObject *Sender)
{
  if (btnUpdate)
    btnUpdate->Enabled = false;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::BoldActivateSystemAction1SystemOpened(
      TObject *Sender)
{
  TPerson *aPerson;
  Integer i;
  double dAssets;

  AnsiString persons[2][10];
  persons[0][0] = "Adam";     persons[1][0] = "10000";
  persons[0][1] = "Bertil";   persons[1][1] = "20000";
  persons[0][2] = "Ceasar";   persons[1][2] = "30000";
  persons[0][3] = "David";    persons[1][3] = "40000";
  persons[0][4] = "Eric";     persons[1][4] = "50000";
  persons[0][5] = "Fredrik";  persons[1][5] = "60000";
  persons[0][6] = "Gustav";   persons[1][6] = "70000";
  persons[0][7] = "Henrik";   persons[1][7] = "80000";
  persons[0][8] = "Ivar";     persons[1][8] = "90000";
  persons[0][9] = "Jacob";    persons[1][9] = "100000";

  if (blhAllPersons->Count == 0)
  {
    for (i = 0; i <= 9; i++)
    {
      aPerson = new TPerson(NULL);

      dAssets = StrToFloat(persons[1][i]);
      aPerson->Assets = dAssets;
      aPerson->FirstName = persons[0][i];
      ((TPersonList*)(blhAllPersons->List))->Add(aPerson);
    }
  }
  btnUpdate->Enabled = true;

}
//---------------------------------------------------------------------------

void __fastcall TForm1::btnUpdateClick(TObject *Sender)
{
  BoldSystemHandle1->UpdateDatabase();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::bdhRichPersonsDeriveAndSubscribe(
      TComponent *Sender, TBoldElement *RootValue,
      TBoldIndirectElement *ResultElement, TBoldSubscriber *Subscriber)
{
  TPersonList *AllPersons, *RichPersons;
  Integer i;
  TBoldSystem *Root;

  if (RootValue)
  {
    Root = (TBoldSystem*)RootValue;
    AllPersons = (TPersonList*)Root->ClassByExpressionName["Person"];
    RichPersons = (TPersonList*)(AllPersons->Clone());

    RichPersons->Sort(RichSorter);
    for (i = RichPersons->Count - 1; i >= 0; i--)
    {
      RichPersons->BoldObjects[i]->M_Assets->DefaultSubscribe(Subscriber, breReSubscribe); // breReEvaluate?
      if (((RichPersons->BoldObjects[i]->Assets < ((TBACurrency*)(bvhAssetBreakpoint->Value))->AsCurrency))
        || (i >= ((TBAInteger*)(bvhRichCount->Value))->AsInteger))
        RichPersons->RemoveByIndex(i);
    }
    AllPersons->DefaultSubscribe(Subscriber, breReSubscribe);
    ResultElement->SetOwnedValue(RichPersons);


    bvhRichCount->Value->DefaultSubscribe(Subscriber, breReSubscribe);
    bvhAssetBreakpoint->Value->DefaultSubscribe(Subscriber, breReSubscribe);
  }

}
//---------------------------------------------------------------------------
Integer __fastcall TForm1::RichSorter(TBoldElement *Person1, TBoldElement *Person2)
{
  Integer Result = ( (((TPerson*)Person2)->Assets) - (((TPerson*)Person1)->Assets) );// round((Person2 as TPerson).assets - (Person1 as TPerson).assets);
  return Result;
}
