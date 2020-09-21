//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "dm1.h"
#include <System.hpp>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "BoldAbstractModel"
#pragma link "BoldControlPack"
#pragma link "BoldElements"
#pragma link "BoldHandle"
#pragma link "BoldHandles"
#pragma link "BoldModel"
#pragma link "BoldPersistenceHandle"
#pragma link "BoldPersistenceHandleBDE"
#pragma link "BoldPersistenceHandleDB"
#pragma link "BoldStringControlPack"
#pragma link "BoldSubscription"
#pragma link "BoldSystemHandle"
#pragma link "BoldAbstractDatabaseAdapter"
#pragma link "BoldAbstractPersistenceHandleDB"
#pragma link "BoldDatabaseAdapterIB"
#pragma resource "*.dfm"
TdmMain *dmMain;
//---------------------------------------------------------------------------
__fastcall TdmMain::TdmMain(TComponent* Owner)
  :TDataModule(Owner)
{
}
//---------------------------------------------------------------------------
AnsiString __fastcall TdmMain::BoldProfitAsStringRendererGetAsString(
      TBoldElement *Element, int Representation, AnsiString Expression)
{
  //This method is called whenever any of the elements you subscribe to changes.
  //Element is assumed to be a Product. Calculate a string to show.

  AnsiString Result = "";
  if (Element)
  {
    TProduct *product = (TProduct*)Element;
    if (product->TotalCost != 0)
    {
      TVarRec v[] = {((product->Price / (product->TotalCost - 1)) * 100)};
      Result = Format("%d", v, ARRAYSIZE(v) - 1); //"%d,%%" //Round((product->Price / product->TotalCost - 1) * 100));
    }
    else
      Result = "n/a";
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TdmMain::BoldProfitAsStringRendererMayModify(
      TBoldElement *Element, int Representation, AnsiString Expression,
      TBoldSubscriber *Subscriber)
{
  //This method is called at the same time as the GetAsString event to render the ReadOnly value.
  //Result sets the ReadOnly property on the control using this Renderer.

  bool Result = false;
  if (Element)
  {
    TProduct *product = (TProduct*)Element;
    Result = (product->TotalCost != 0);
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TdmMain::BoldProfitAsStringRendererSetAsString(
      TBoldElement *Element, AnsiString NewValue, int Representation,
      AnsiString Expression)
{
  //This method is called when you change the value in a control that uses this Renderer.
  AnsiString CleanedValue = CleanValue(NewValue);

  TProduct *product = (TProduct*)Element;
  double cvalue = StrToFloat(CleanedValue);
  if (CleanedValue != "")
    product->Price = (product->TotalCost * ((cvalue/100)+1));
}

AnsiString __fastcall TdmMain::CleanValue(AnsiString NewValue)
{
  Integer i = 1;
  AnsiString CleanedValue = "";
  //Remove spaces and %
  while (i <= NewValue.Length())
  {
    if (NewValue[i] == 0 || NewValue[i] == 1 || NewValue[i] == 2 || NewValue[i] == 3 ||
      NewValue[i] == 4 || NewValue[i] == 5 || NewValue[i] == 6 || NewValue[i] == 7 ||
      NewValue[i] == 8 || NewValue[i] == 9 || NewValue[i] == DecimalSeparator)
      CleanedValue = CleanedValue+NewValue[i];
    i++;
  }
  return CleanedValue;
}

//---------------------------------------------------------------------------
void __fastcall TdmMain::BoldProfitAsStringRendererSubscribe(
      TBoldElement *Element, int Representation, AnsiString Expression,
      TBoldSubscriber *Subscriber)
{
  /*You need to know when price and/or totalCost changes to correctly show the profit.
  Element is assumed to be a Product.
  The method SubscribeToExpression places subscriptions for the subscriber.
  The subscriber comes from whoever uses the AsStringRenderer.
  Resubscribe is false in most cases. */
  if (Element)
  {
    TBoldExternalVariableList* VariableList = NULL;
    Element->SubscribeToExpression("price", Subscriber, false, false, VariableList);
    Element->SubscribeToExpression("totalCost", Subscriber, false, false, VariableList);
  }

}
//---------------------------------------------------------------------------
