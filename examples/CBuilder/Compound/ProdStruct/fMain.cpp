//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "fMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "BoldAbstractListHandle"
#pragma link "BoldActions"
#pragma link "BoldCursorHandle"
#pragma link "BoldDBActions"
#pragma link "BoldGrid"
#pragma link "BoldHandleAction"
#pragma link "BoldHandles"
#pragma link "BoldListBox"
#pragma link "BoldListHandle"
#pragma link "BoldNavigator"
#pragma link "BoldNavigatorDefs"
#pragma link "BoldReferenceHandle"
#pragma link "BoldRootedHandles"
#pragma link "BoldSubscription"
#pragma link "BoldTreeView"
#pragma link "BoldIBDatabaseAction"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
__fastcall TForm1::~TForm1(void)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::BoldActivateSystemAction1SystemOpened(
      TObject *Sender)
{
  if (hdlAllProducts->Count == 0)
  {
    Blade = new TSimple_Product(dmMain->BoldSystem->System);
    Blade->Name = "BigBlade";
    Blade->ProductionCost = 5;

    Handle = new TSimple_Product(dmMain->BoldSystem->System);
    Handle->Name = "Handle";
    Handle->ProductionCost = 2;

    Knife = new TAssembly(dmMain->BoldSystem->System);
    Knife->Name = "BigKnife";
    Knife->AssemblyCost = 1;
    Knife->Parts->Add(Blade);
    Knife->Parts->Add(Handle);

    KnifeKit = new TAssembly(dmMain->BoldSystem->System);
    KnifeKit->Name = "KnifeKit";
    KnifeKit->AssemblyCost = 1;
    KnifeKit->Parts->Add(Knife);

    Blade = new TSimple_Product(dmMain->BoldSystem->System);
    Blade->Name = "SmallBlade";
    Blade->ProductionCost = 4;

    Knife = new TAssembly(dmMain->BoldSystem->System);
    Knife->Name = "SmallKnife";
    Knife->AssemblyCost = 1;
    Knife->Parts->Add(Blade);
    Knife->Parts->Add(Handle);

    KnifeKit->Parts->Add(Knife);
  }
  hdlTreeRoot->Value = dmMain->BoldSystem->System->ClassByExpressionName["Product"];
}
//---------------------------------------------------------------------------

void __fastcall TForm1::btnChangeRootClick(TObject *Sender)
{
  /* This is an example of how you change the root on a TreeView component.
  The Root and BoldHandle property are mutual exclusive.
  Root is set to the element on the active row in the Products Grid. */

  hdlTreeRoot->Value = hdlAllProducts->CurrentElement;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::btnShowAllClick(TObject *Sender)
{
  /* This is an example of how you change the root on a TreeView component.
  The Root and BoldHandle property are mutual exclusive.
  BoldHandle is set to the entire system and the expression evaluates to a list of all products.
  Assembly and Simple_Product are concreate subclasses of the abstract class Product. */

  hdlTreeRoot->Value = dmMain->BoldSystem->System->ClassByExpressionName["Product"];
}
//---------------------------------------------------------------------------

void __fastcall TForm1::lbAssemblyPartsKeyPress(TObject *Sender, char &Key)
{
  // This is an example of how you remove a relation when the user press backspace.
  if (Key == VK_BACK)
    ((TAssembly*)(grdAssemblies->CurrentBoldElement))->Parts->Remove(lbAssemblyParts->CurrentBoldObject);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::grdProductsEnter(TObject *Sender)
{
  // This is an example of how you change the behaviour of a component depending on focus.
  nvgShared->BoldHandle = hdlAllProducts;
  lblNavigator->Caption = "Products";
}
//---------------------------------------------------------------------------

void __fastcall TForm1::grdSimpleProductsEnter(TObject *Sender)
{
  // This is an example of how you change the behaviour of a component depending on focus.
  nvgShared->BoldHandle = hdlAllSimpleProducts;
  lblNavigator->Caption = "Simple Products";
}
//---------------------------------------------------------------------------

void __fastcall TForm1::grdAssembliesDblClick(TObject *Sender)
{
  // Show the Runtime Column Editor when the user double clicks
  grdAssemblies->EditColumns();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::grdAssembliesEnter(TObject *Sender)
{
  // This is an example of how you change the behaviour of a component depending on focus.
  nvgShared->BoldHandle = hdlAllAssemblies;
  lblNavigator->Caption = "Assemblies";
}
//---------------------------------------------------------------------------

void __fastcall TForm1::FormCloseQuery(TObject *Sender, bool &CanClose)
{
  CanClose = true;
  if (dmMain->BoldSystem->Active)
    if (dmMain->BoldSystem->System->DirtyObjects->Count > 0)
      switch (MessageDlg("There are dirty objects. Save them before exit?", mtConfirmation, TMsgDlgButtons() << mbYes << mbNo << mbCancel, 0))
      {
        case mrYes: dmMain->BoldSystem->UpdateDatabase(); break;
        case mrNo: dmMain->BoldSystem->System->Discard(); break;
        case mrCancel: CanClose = false;
      }
}
//---------------------------------------------------------------------------

