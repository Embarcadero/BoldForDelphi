//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "fTreeViewMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "BoldAbstractListHandle"
#pragma link "BoldAbstractModel"
#pragma link "BoldActions"
#pragma link "BoldCursorHandle"
#pragma link "BoldDBActions"
#pragma link "BoldExpressionHandle"
#pragma link "BoldGrid"
#pragma link "BoldHandle"
#pragma link "BoldHandleAction"
#pragma link "BoldHandles"
#pragma link "BoldListBox"
#pragma link "BoldListHandle"
#pragma link "BoldModel"
#pragma link "BoldNavigator"
#pragma link "BoldNavigatorDefs"
#pragma link "BoldPersistenceHandle"
#pragma link "BoldPersistenceHandleDB"
#pragma link "BoldReferenceHandle"
#pragma link "BoldRootedHandles"
#pragma link "BoldSubscription"
#pragma link "BoldSystemHandle"
#pragma link "BoldTreeView"
#pragma link "BoldEdit"
#pragma link "BoldGUI"
#pragma link "BoldAbstractListHandle"
#pragma link "BoldAbstractModel"
#pragma link "BoldActions"
#pragma link "BoldCursorHandle"
#pragma link "BoldDBActions"
#pragma link "BoldEdit"
#pragma link "BoldExpressionHandle"
#pragma link "BoldGrid"
#pragma link "BoldHandle"
#pragma link "BoldHandleAction"
#pragma link "BoldHandles"
#pragma link "BoldListBox"
#pragma link "BoldListHandle"
#pragma link "BoldModel"
#pragma link "BoldNavigator"
#pragma link "BoldNavigatorDefs"
#pragma link "BoldPersistenceHandle"
#pragma link "BoldPersistenceHandleBDE"
#pragma link "BoldPersistenceHandleDB"
#pragma link "BoldReferenceHandle"
#pragma link "BoldRootedHandles"
#pragma link "BoldSubscription"
#pragma link "BoldSystem"
#pragma link "BoldTreeView"
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
  Height = 508;
  Width = 641;
  pgcTreeViews->ActivePage = tsBasicTree;
  CurrentTreeView = btrvBasic;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btnCurrentAsRootClick(TObject *Sender)
{
  brhTreeRoot->Value = CurrentTreeView->CurrentElement;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btnShowAllClick(TObject *Sender)
{
  brhTreeRoot->Value = BoldSystemHandle1->System->ClassByExpressionName["MTB"];
}
//---------------------------------------------------------------------------
void __fastcall TForm1::OpenSystemClicked(TObject *Sender)
{
  if (BoldSystemHandle1->Active == true)
    BoldActivateSystemAction1SystemClosed(Sender);
  else
    BoldActivateSystemAction1SystemOpened(Sender);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::BoldActivateSystemAction1SystemClosed(TObject *Sender)
{
    if (btnShowAll)
      btnShowAll->Enabled = BoldSystemHandle1->Active;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::BoldActivateSystemAction1SystemOpened(TObject *Sender)
{
  if (BoldSystemHandle1->System->ClassByExpressionName["BikeFrame"]->Count < 1)
    PopulateClass("BikeFrame", "Name", BoldSystemHandle1->System->ClassByExpressionName["BikeFrame"]);
  if (BoldSystemHandle1->System->ClassByExpressionName["Gear"]->Count < 1)
    PopulateClass("Gear", "Model", BoldSystemHandle1->System->ClassByExpressionName["Gear"]);
  if (BoldSystemHandle1->System->ClassByExpressionName["Brake"]->Count < 1)
    PopulateClass("Brake", "Model", BoldSystemHandle1->System->ClassByExpressionName["Brake"]);
  if (BoldSystemHandle1->System->ClassByExpressionName["Wheel"]->Count < 1)
    PopulateClass("Wheel", "Model", BoldSystemHandle1->System->ClassByExpressionName["Wheel"]);
  if (BoldSystemHandle1->System->ClassByExpressionName["MTB"]->Count < 1)
    PopulateClass("MTB", "Name", BoldSystemHandle1->System->ClassByExpressionName["MTB"]);
  if (BoldSystemHandle1->System->DirtyObjects->Count > 0)
    BoldSystemHandle1->System->UpdateDatabase();

  brhTreeRoot->Value = BoldSystemHandle1->System->ClassByExpressionName["MTB"];
  btnShowAll->Enabled = BoldSystemHandle1->Active;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::imgUnlinkDragDrop(TObject *Sender, TObject *Source, int X, int Y)
{
  TBoldObject *ObjectToUnlink;
  ObjectToUnlink = BoldGUIHandler()->DraggedObjects->BoldObjects[0];
  if ((dynamic_cast<TBikeFrame*>(ObjectToUnlink)) != 0)
    ((TMTB*)DragContext)->BuiltAround = NULL;
  else
    ((TMTB*)DragContext)->ConsistsOf->Remove(ObjectToUnlink);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::imgUnlinkDragOver(TObject *Sender, TObject *Source, int X, int Y, TDragState State, bool &Accept)
{
  Accept = false;
  if ((BoldGUIHandler()->DraggedObjects->Count == 1) && (DragContext))
    Accept = true;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btrvEnhancedDragDrop(TObject *Sender, TObject *Source, int X, int Y)
{
  TBoldObject *ObjectToLink;
  ObjectToLink = BoldGUIHandler()->DraggedObjects->BoldObjects[0];
  if ((dynamic_cast<TParts*>(ObjectToLink)) != 0)
    TargetObject->ConsistsOf->Add(dynamic_cast<TParts*>(ObjectToLink));
  else
    TargetObject->BuiltAround = dynamic_cast<TBikeFrame*>(ObjectToLink);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btrvEnhancedDragOver(TObject *Sender, TObject *Source, int X, int Y, TDragState State, bool &Accept)
{
  TBoldTreeView *TreeView;
  TBoldElement *TargetElement;
  Accept = false;

  TreeView = (TBoldTreeView*)Sender;
  TargetElement = TreeView->GetElementAt(X, Y);

  TBoldObjectList* objectlist;
  objectlist = BoldGUIHandler()->DraggedObjects;

  if (TargetElement)
  {
    if ((objectlist->Count == 1) && ((dynamic_cast<TMTB*>(TargetElement)) != 0)
      && ((dynamic_cast<TMTB*>(objectlist->BoldObjects[0])) == 0 ))
    {
      TargetObject = dynamic_cast<TMTB*>(TargetElement);
      Accept = ( ((dynamic_cast<TParts*>(objectlist->BoldObjects[0])) != 0)
                || ((dynamic_cast<TBikeFrame*>(objectlist->BoldObjects[0])) != 0) );
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::pgcTreeViewsChange(TObject *Sender)
{
  if (pgcTreeViews->ActivePage == tsBasicTree)
    CurrentTreeView = btrvBasic;
  else
    CurrentTreeView = btrvEnhanced;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::PopulateClass(AnsiString AClassName, AnsiString APropName, TBoldObjectList *AList)
{
  TStrings *NameStrings = new TStringList();
  int i;
  TBoldElement *AElement;

  NameStrings->LoadFromFile(AClassName + ".txt");
  for (i = 0; i < NameStrings->Count; i++)
  {
    AElement = AList->AddNew();
    ((TBoldObject*)AElement)->BoldMemberByExpressionName[APropName]->AsString = NameStrings->Strings[i];
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btrvEnhancedStartDrag(TObject *Sender, TDragObject *&DragObject)
{
  TBoldElement *ABoldElement;
  DragContext = NULL;

  if ( (CurrentTreeView->Selected) && (CurrentTreeView->Selected->Parent) )
  {
    ABoldElement = ((TBoldTreeNode*)(CurrentTreeView->Selected->Parent))->Follower->Element;
    if ( (dynamic_cast<TMTB*>(ABoldElement)) != 0)
      DragContext = (TBoldObject*)ABoldElement;
  }
}
//---------------------------------------------------------------------------

