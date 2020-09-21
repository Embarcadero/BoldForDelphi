//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "fMain.h"
#include "RendererExampleClasses.hpp"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "BoldAbstractListHandle"
#pragma link "BoldAbstractModel"
#pragma link "BoldActions"
#pragma link "BoldControlPack"
#pragma link "BoldCursorHandle"
#pragma link "BoldDBActions"
#pragma link "BoldEdit"
#pragma link "BoldElements"
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
#pragma link "BoldStringControlPack"
#pragma link "BoldSubscription"
#pragma link "BoldSystemHandle"
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
void __fastcall TForm1::FormCreate(TObject *Sender)
{
  Height = 330;
  Width = 447;
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
void __fastcall TForm1::BoldActivateSystemAction1SystemOpened(
      TObject *Sender)
{
  if (blhGlobals->List->Count < 1)
    blhGlobals->List->AddNew();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::bsrSalaryLevelSetColor(TBoldElement *Element,
      TColor &AColor, int Representation, AnsiString Expression)
{
  if (Element)
    if (((TPerson*)Element)->Salary < ((TGlobals*)(blhGlobals->List->Elements[0]))->SalaryBreakPoint )
      AColor = clSilver;

 // if assigned(element) then
 //   if (element as TPerson).Salary < (blhGlobals.List[0] as TGlobals).SalaryBreakPoint then
 //    aColor := clSilver;

}
//---------------------------------------------------------------------------
void __fastcall TForm1::bsrSalaryLevelSubscribe(TBoldElement *Element,
      int Representation, AnsiString Expression,
      TBoldSubscriber *Subscriber)
{
  TBoldExternalVariableList *var = NULL;
  Element->SubscribeToExpression("Globals.allInstances->first.salaryBreakPoint", Subscriber, false, false, var);
}
//---------------------------------------------------------------------------
AnsiString __fastcall TForm1::bsrFullNameGetAsString(TBoldElement *Element, int Representation, AnsiString Expression)
{
  AnsiString result = "";
  TPerson *person;

  if (Element)
  {
    person = dynamic_cast<TPerson*>(Element);
    if (person->FirstName != "")
      result = person->FirstName;
    else
      result = "<FirstName>";
    if (person->LastName != "")
      result += " " + person->LastName;
    else
      result += " <LastName>";
  }
  return result;
}
//---------------------------------------------------------------------------
bool __fastcall TForm1::bsrFullNameMayModify(TBoldElement *Element,
      int Representation, AnsiString Expression,
      TBoldSubscriber *Subscriber)
{
  //int result = Element->EvaluateExpressionAsDirectElement("").ObserverMayModify(subscriber);
  bool result;
  TBoldExternalVariableList *var = NULL;
  TPerson *person;
  person = dynamic_cast<TPerson*>(Element);

  result = person->EvaluateExpressionAsDirectElement("firstName", var)->ObserverMayModify(Subscriber);
  result = (result && (person->EvaluateExpressionAsDirectElement("lastName", var)->ObserverMayModify(Subscriber)));

  return result;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::bsrFullNameSetAsString(TBoldElement *Element,
      AnsiString NewValue, int Representation, AnsiString Expression)
{
  TPerson *person = dynamic_cast<TPerson*>(Element);
  person->FirstName = NewValue.SubString(0, (NewValue.Pos(" ") - 1));
  person->LastName = NewValue.SubString(NewValue.Pos(" "), (NewValue.Length() - NewValue.Pos(" ") + 1));
}
//---------------------------------------------------------------------------
void __fastcall TForm1::bsrFullNameSubscribe(TBoldElement *Element,
      int Representation, AnsiString Expression,
      TBoldSubscriber *Subscriber)
{
  TPerson *person;
  TBoldExternalVariableList *var = NULL;

  person = dynamic_cast<TPerson*>(Element);
  person->SubscribeToExpression("firstName", Subscriber, false, false, var);
  person->SubscribeToExpression("lastName", Subscriber, false, false, var);
}
//---------------------------------------------------------------------------
