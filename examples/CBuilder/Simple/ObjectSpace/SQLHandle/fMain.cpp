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
#pragma link "BoldSQLHandle"
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
void __fastcall TForm1::BoldActivateSystemAction1SystemClosed(
      TObject *Sender)
{
  if (btnUpdateDB)
    btnUpdateDB->Enabled = BoldSystemHandle1->Active;

  if (btnExecSQL)
    btnExecSQL->Enabled = BoldSystemHandle1->Active;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::BoldActivateSystemAction1SystemOpened(
      TObject *Sender)
{
  btnUpdateDB->Enabled = BoldSystemHandle1->Active;
  btnExecSQL->Enabled = BoldSystemHandle1->Active;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btnUpdateDBClick(TObject *Sender)
{
  BoldSystemHandle1->UpdateDatabase();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormCloseQuery(TObject *Sender, bool &CanClose)
{
  CanClose = true;
  if (BoldSystemHandle1->Active)
    if (BoldSystemHandle1->System->DirtyObjects->Count > 0)
      switch (MessageDlg("There are dirty objects. Save them before exit?", mtConfirmation, TMsgDlgButtons() << mbYes << mbNo << mbCancel, 0 ))
      {
        case mrYes: BoldSystemHandle1->System->UpdateDatabase(); break;
        case mrNo: BoldSystemHandle1->System->Discard(); break;
        case mrCancel: CanClose = false;
      }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btnExecSQLClick(TObject *Sender)
{
  AnsiString WhereClause, OrderByClause, Conjunction;
  Conjunction = "";

  if(BoldSystemHandle1->System->BoldDirty)
    ShowMessage(AnsiString("There are changes that are not saved to the database. Results may not be as expected."));

  // just some code to set up the order by and where statements for this example
  if(cmbAssetsOperator->Text != "")
  {
    WhereClause = Format("ASSETS %s %s", ARRAYOFCONST((cmbAssetsOperator->Text, edAssetsExpr->Text)));
    Conjunction = " and";
  }
  if(cmbFirstnameOperator->Text != "")
  {
    WhereClause = WhereClause + Format("%s FIRSTNAME %s '%s'", ARRAYOFCONST((Conjunction, cmbFirstnameOperator->Text, edFirstnameExpr->Text)));
    Conjunction = " and";
  }
  if(cmbLastnameOperator->Text != "")
  {
    WhereClause = WhereClause + Format("%s LASTNAME %s '%s'", ARRAYOFCONST((Conjunction, cmbLastnameOperator->Text, edLastnameExpr->Text)));
  }

  OrderByClause = cmbOrderBy1->Text + ", " + cmbOrderBy2->Text + ", " + cmbOrderBy3->Text;

  bsqlhPersons->SQLOrderByClause = OrderByClause;
  bsqlhPersons->SQLWhereClause = WhereClause;

  ShowMessage(WhereClause);
  bsqlhPersons->ExecuteSQL();
}
//---------------------------------------------------------------------------



