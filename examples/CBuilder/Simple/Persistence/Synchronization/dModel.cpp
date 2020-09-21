//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "dModel.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "BoldAbstractDequeuer"
#pragma link "BoldAbstractModel"
#pragma link "BoldAbstractPropagatorHandle"
#pragma link "BoldCheckboxStateControlPack"
#pragma link "BoldClientHandles"
#pragma link "BoldComClientHandles"
#pragma link "BoldControlPack"
#pragma link "BoldElements"
#pragma link "BoldExternalObjectSpaceEventHandler"
#pragma link "BoldFilteredHandle"
#pragma link "BoldHandle"
#pragma link "BoldHandles"
#pragma link "BoldIDAdderHandle"
#pragma link "BoldListenerHandle"
#pragma link "BoldModel"
#pragma link "BoldPersistenceHandle"
#pragma link "BoldPersistenceHandleDB"
#pragma link "BoldPersistenceHandlePassthrough"
#pragma link "BoldPropagatorHandleCOM"
#pragma link "BoldSnooperHandle"
#pragma link "BoldSortedHandle"
#pragma link "BoldStringControlPack"
#pragma link "BoldSubscription"
#pragma link "BoldSystemHandle"
#pragma link "BoldAbstractDatabaseAdapter"
#pragma link "BoldAbstractPersistenceHandleDB"
#pragma link "BoldDatabaseAdapterIB"
#pragma link "BoldPersistenceHandlePTWithModel"
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
  TCheckBoxState result = cbGrayed;
  if (Element)
  {
    if ( ((TPerson*)(Element))->Assets > 10000)
       result = cbChecked;
    else
       result = cbUnchecked;
  }
  return result;
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
  if (Element)
  {
    TVarRec v[] = {((TPerson*)(Element))->LastName, ((TPerson*)(Element))->FirstName};
    result = Format("%s, %s", v, ARRAYSIZE(v) - 1);
  }
  return result;
}
//---------------------------------------------------------------------------
void __fastcall TDataModule1::FullNameRendererSubscribe(
      TBoldElement *Element, int Representation, AnsiString Expression,
      TBoldSubscriber *Subscriber)
{
  TBoldExternalVariableList* VariableList = NULL;
  Element->SubscribeToExpression("firstName", Subscriber, false, false, VariableList);
  Element->SubscribeToExpression("", Subscriber, false, false, VariableList);
  Element->SubscribeToExpression("lastName", Subscriber, false, false, VariableList);

}
//---------------------------------------------------------------------------
void __fastcall TDataModule1::NegativeRedRendererHoldsChangedValue(
      TBoldElement *Element, int Representation, AnsiString Expression,
      TBoldSubscriber *Subscriber)
{
  //Element->EvaluateExpressionAsDirectElement("").RegisterModifiedValueHolder(Subscriber);
  if (Element)
    NegativeRedRenderer->DefaultHoldsChangedValue(Element, Representation, Expression, NULL, Subscriber);
}
//---------------------------------------------------------------------------
void __fastcall TDataModule1::NegativeRedRendererSetFont(
      TBoldElement *Element, TFont *AFont, int Representation,
      AnsiString Expression)
{
  if (Element)
    if ( ((TPerson*)(Element))->Assets < 0)
      AFont->Color = clRed;
    else
      AFont->Color = clBlue;
}
//---------------------------------------------------------------------------
bool __fastcall TDataModule1::IsRichFilterFilter(TBoldElement *Element)
{
  bool result = false;
  if (Element)
    result = (((TPerson*)(Element))->Assets > 10000);
  return result;
}
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
  int result = AnsiCompareText(((TPerson*)Item1)->LastName, ((TPerson*)Item2)->LastName);
  if (result == 0)
    result = AnsiCompareText(((TPerson*)Item1)->FirstName, ((TPerson*)Item2)->FirstName);

  return result;
}
//---------------------------------------------------------------------------
void __fastcall TDataModule1::NameComparerSubscribe(TBoldElement *Element,
      TBoldSubscriber *Subscriber)
{
  TBoldExternalVariableList* VariableList = NULL;
  Element->SubscribeToExpression("firstName", Subscriber,  false, false, VariableList);
  Element->SubscribeToExpression("lastName", Subscriber,  false, false, VariableList);
}
//---------------------------------------------------------------------------
void __fastcall TDataModule1::Timer1Timer(TObject *Sender)
{
  BoldListenerHandle1->ListenerThread->ExtendLease();
}
//---------------------------------------------------------------------------
void __fastcall TDataModule1::BoldListenerHandle1RegistrationFailed(
      TObject *Sender)
{
  ShowMessage("Failed to register with propagator");
}
//---------------------------------------------------------------------------
void __fastcall TDataModule1::BoldExternalObjectSpaceEventHandler1Conflict(
      TBoldObject *BoldObject)
{
  ShowMessage(BoldObject->AsString + "Conflict with a modification in another database!!");
}
//---------------------------------------------------------------------------
void __fastcall TDataModule1::bcchEnterprisePropagatorBeforeConnect(
      TObject *Sender)
{
  bcchEnterprisePropagator->ServerHost = InputBox("Propagator Server", "Enter the Machine Name", "localhost");
}
//---------------------------------------------------------------------------
void __fastcall TDataModule1::bcchEnterprisePropagatorConnectFailed(
      TObject *Sender)
{
  ShowMessage("Failed to connect to the Propagator");
}
//---------------------------------------------------------------------------
bool __fastcall TDataModule1::BoldListenerHandle1ThreadError(
      AnsiString aMessage)
{
  bool result = false;
  ShowMessage("Thread error in the ListenerThread: " + aMessage);
  return result;
}
//---------------------------------------------------------------------------
void __fastcall TDataModule1::BoldListenerHandle1ExtendLeaseFailed(
      TBoldExtendLeaseResult res, const AnsiString Msg)
{
 ShowMessage("Could not extend the lease");
}
//---------------------------------------------------------------------------

