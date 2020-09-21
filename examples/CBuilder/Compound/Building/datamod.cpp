//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "datamod.h"
#include "Buildingclasses.hpp"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "BoldAbstractModel"
#pragma link "BoldHandle"
#pragma link "BoldHandles"
#pragma link "BoldModel"
#pragma link "BoldPersistenceHandle"
#pragma link "BoldPersistenceHandleBDE"
#pragma link "BoldPersistenceHandleDB"
#pragma link "BoldSubscription"
#pragma link "BoldSystemHandle"
#pragma link "BoldUMLModelLink"
#pragma link "BoldUMLRose98Link"
#pragma link "BoldCheckboxStateControlPack"
#pragma link "BoldControlPack"
#pragma link "BoldElements"
#pragma link "BoldStringControlPack"
#pragma link "BoldFilteredHandle"
#pragma link "BoldSortedHandle"
#pragma link "BoldAbstractDatabaseAdapter"
#pragma link "BoldAbstractPersistenceHandleDB"
#pragma link "BoldDatabaseAdapterIB"
#pragma resource "*.dfm"
TDataModule1 *DataModule1;
//---------------------------------------------------------------------------
__fastcall TDataModule1::TDataModule1(TComponent* Owner)
        : TDataModule(Owner)
{
}
//---------------------------------------------------------------------------
TCheckBoxState __fastcall TDataModule1::IsRichRendererGetAsCheckBoxState(
      TBoldElement *Element, int Representation, AnsiString Expression)
{
  TPerson *pd;
  if (Element)
  {
    if ((pd = dynamic_cast<TPerson *>(Element)) != 0)
    {
      if (pd->Assets > 10000)
         return cbChecked; // Result := cbChecked
      else
         return cbUnchecked; //Result := cbUnChecked
    }
  }
  return cbGrayed;
}
//---------------------------------------------------------------------------
void __fastcall TDataModule1::IsRichRendererSubscribe(
      TBoldElement *Element, int Representation, AnsiString Expression,
      TBoldSubscriber *Subscriber)
{
  TBoldExternalVariableList* VariableList = NULL;
  Element->SubscribeToExpression("assets", Subscriber, false, false, VariableList);
}
//---------------------------------------------------------------------------
AnsiString __fastcall TDataModule1::FullNameRendererGetAsString(
      TBoldElement *Element, int Representation, AnsiString Expression)
{
  AnsiString result = "";
  TPerson *pd;

  if (Element)
  {
    if ((pd = dynamic_cast<TPerson *>(Element)) != 0)
    {
      TVarRec v[] = {pd->LastName, pd->FirstName};
      result = Format("%s, %s", v, ARRAYSIZE(v) - 1);
    }
  }
  return result;
}
//---------------------------------------------------------------------------
void __fastcall TDataModule1::FullNameRendererSubscribe(
      TBoldElement *Element, int Representation, AnsiString Expression,
      TBoldSubscriber *Subscriber)
{
  TBoldExternalVariableList* VariableList = NULL;
  Element->SubscribeToExpression("", Subscriber, false, false, VariableList);
}
//---------------------------------------------------------------------------
void __fastcall TDataModule1::NegativeRedRendererHoldsChangedValue(
      TBoldElement *Element, int Representation, AnsiString Expression,
      TBoldSubscriber *Subscriber)
{
  TBoldExternalVariableList* VariableList = NULL;
  if (Element)
      NegativeRedRenderer->DefaultHoldsChangedValue(Element, Representation, Expression, VariableList, Subscriber);

  //Element.EvaluateExpressionAsDirectElement('').RegisterModifiedValueHolder(Subscriber);
}
//---------------------------------------------------------------------------
void __fastcall TDataModule1::NegativeRedRendererSetFont(
      TBoldElement *Element, TFont *AFont, int Representation,
      AnsiString Expression)
{
  TPerson *pd;
  if (Element)
  {
    if ((pd = dynamic_cast<TPerson *>(Element)) != 0)
    {
      if (pd->Assets < 0)
        AFont->Color =  clRed;
      else
        AFont->Color = clBlue;
    }
  }
}
//---------------------------------------------------------------------------


//---------------------------------------------------------------------------
void __fastcall TDataModule1::IsRichFilterSubscribe(TBoldElement *Element,
      TBoldSubscriber *Subscriber)
{
  TBoldExternalVariableList* VariableList = NULL;
  Element->SubscribeToExpression("assets", Subscriber, false, false, VariableList);
}
//---------------------------------------------------------------------------
int __fastcall TDataModule1::NameComparerCompare(TBoldElement *Item1,
      TBoldElement *Item2)
{
  int result = 0;
  TPerson *pd1, *pd2;
  if ((Item1) && (Item2))
  {
    pd1 = dynamic_cast<TPerson *>(Item1);
    pd2 = dynamic_cast<TPerson *>(Item2);
    result = AnsiCompareText(pd1->LastName, pd2->LastName);
    if (result == 0)
      result = AnsiCompareText(pd1->FirstName, pd2->FirstName);
  }
  return result;
}
//---------------------------------------------------------------------------
void __fastcall TDataModule1::NameComparerSubscribe(TBoldElement *Element,
      TBoldSubscriber *Subscriber)
{
  //Element.SubscribeToExpression('', Subscriber, False);
  TBoldExternalVariableList* VariableList = NULL;
  Element->SubscribeToExpression("firstName", Subscriber, false, false, VariableList);
  Element->SubscribeToExpression("lastName", Subscriber, false, false, VariableList);
}
//---------------------------------------------------------------------------
bool __fastcall TDataModule1::IsRichFilterFilter(TBoldElement *Element)
{
  bool result = false;
  TPerson *pd1;

  if (Element)
  {
    if ((pd1 = dynamic_cast<TPerson*>(Element)) != 0)
      result = (pd1->Assets > 10000);
  }
  return result;
}
//---------------------------------------------------------------------------

