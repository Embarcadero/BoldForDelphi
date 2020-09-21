//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "fMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "BoldAbstractListHandle"
#pragma link "BoldActions"
#pragma link "BoldAFPPluggable"
#pragma link "BoldCaptionController"
#pragma link "BoldComboBox"
#pragma link "BoldCursorHandle"
#pragma link "BoldDBActions"
#pragma link "BoldDerivedHandle"
#pragma link "BoldExpressionHandle"
#pragma link "BoldGrid"
#pragma link "BoldHandleAction"
#pragma link "BoldHandles"
#pragma link "BoldListBox"
#pragma link "BoldListHandle"
#pragma link "BoldMemo"
#pragma link "BoldNavigator"
#pragma link "BoldNavigatorDefs"
#pragma link "BoldRootedHandles"
#pragma link "BoldSubscription"
#pragma link "BoldVariableDefinition"
#pragma link "BoldVariableHandle"
#pragma link "BoldIBDatabaseAction"
#pragma link "BoldOclVariables"
#pragma resource "*.dfm"
TfrmMain *frmMain;
//---------------------------------------------------------------------------
__fastcall TfrmMain::TfrmMain(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::FormCloseQuery(TObject *Sender, bool &CanClose)
{
  CanClose = true;
  if ( (dmModel->BoldSystemHandle1->Active) && (dmModel->BoldSystemHandle1->System->BoldDirty) )
    switch (MessageDlg("There are dirty objects. Save them before exit?", mtConfirmation, TMsgDlgButtons() << mbYes << mbNo << mbCancel, 0) )
    {
      case mrYes: dmModel->BoldSystemHandle1->System->UpdateDatabase(); break;
      case mrNo: dmModel->BoldSystemHandle1->System->Discard(); break;
      case mrCancel: CanClose = false;
    }
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::BoldActivateSystemAction1SystemOpened(
      TObject *Sender)
{
  new TPersContext(dmModel->BoldSystemHandle1->System);
  if (dmModel->BoldSystemHandle1->Active)
    MessageDlg("You must select a Current User. Use the 'Create User' button to create a new user", mtWarning, TMsgDlgButtons() << mbOK, 0);
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnNewUserClick(TObject *Sender)
{
  aPerson = new TPerson(dmModel->BoldSystemHandle1->System);
  aPerson->Name = edtNewUserName->Text;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::bdhDocVersionsDeriveAndSubscribe(
      TComponent *Sender, TBoldElement *RootValue,
      TBoldIndirectElement *ResultElement, TBoldSubscriber *Subscriber)
{
  TBoldChangePointCondition *aCond;
  TBoldListTypeInfo *DocumentListType;
  int i;
  if ( (RootValue) && (dmModel->BoldSystemHandle1->Active) )
  {
    aCond = new TBoldChangePointCondition();
    aCond->IdList = new TBoldObjectIdList();
    aCond->IdList->Add(((TBoldObject*)(RootValue))->BoldObjectLocator->BoldObjectID);
    aCond->MemberIdList = new TBoldMemberIdList();
    aCond->MemberIdList->Add(new TBoldMemberID(((TDocument*)(RootValue))->DocumentPart->BoldMemberRTInfo->index));
    aCond->StartTime = 0;
    aCond->EndTime = BOLDMAXTIMESTAMP;

    DocumentListType = dmModel->BoldSystemTypeInfoHandle1->StaticSystemTypeInfo->ListTypeInfoByElement[RootValue->BoldType];
    TMetaClass *vmt = NULL;

    if (ResultElement->Value)
      ((TBoldObjectList*)(ResultElement->Value))->Clear();
    else
      ResultElement->SetOwnedValue(TBoldMemberFactory::CreateMemberFromBoldType(vmt, DocumentListType));

    dmModel->BoldSystemHandle1->System->GetAllWithCondition((TBoldObjectList*)(ResultElement->Value), (TBoldCondition*)aCond);

    delete aCond->IdList;
    aCond->MemberIdList->Free();
    aCond->Free();
  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnPublishClick(TObject *Sender)
{
  TDocument *Doc;
  Integer i, y;
  TVersion *NewVersion;
  Boolean DoEx;

  DoEx = true;
  Doc = (TDocument*)(blhDocVersions->CurrentBoldObject);
  for (i = 0; i < Doc->Version->Count; i++)
    if (Doc->Version->BoldObjects[i]->Time == Doc->BoldTime)
      DoEx = false; //exit(y);

  if (DoEx)
  {
    NewVersion = ((TVersion*)(Doc->Version->AddNew()));
    NewVersion->Time = Doc->BoldTime;
    NewVersion->IsPublished = true;
  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::Button1Click(TObject *Sender)
{
  TBABoolean *DisplayOld;

  DisplayOld = (TBABoolean*)(bvhDispOld->Value);
  DisplayOld->AsBoolean = (!DisplayOld->AsBoolean);
  blhDocVersions->Enabled = DisplayOld->AsBoolean;
  grdOldVersions->Enabled = DisplayOld->AsBoolean;
  btnPublish->Enabled = DisplayOld->AsBoolean;
  if (DisplayOld->AsBoolean)
    Button1->Caption = "View current";
  else
    Button1->Caption = "View history";
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::bdhPartVersionsDeriveAndSubscribe(
      TComponent *Sender, TBoldElement *RootValue,
      TBoldIndirectElement *ResultElement, TBoldSubscriber *Subscriber)
{
  TBoldChangePointCondition *aCond;

  if ( (RootValue) && (dmModel->BoldSystemHandle1->Active))
  {
    aCond = new TBoldChangePointCondition();
    aCond->IdList = ((TBoldObjectList*)RootValue)->CreateObjectIdList();
    aCond->StartTime = 0;
    aCond->EndTime = BOLDMAXTIMESTAMP;

    TMetaClass *vmt = NULL;
    if (ResultElement->Value)
      ((TBoldObjectList*)(ResultElement->Value))->Clear();
    else
      ResultElement->SetOwnedValue(TBoldMemberFactory::CreateMemberFromBoldType(vmt, RootValue->BoldType));

    dmModel->BoldSystemHandle1->System->GetAllWithCondition((TBoldObjectList*)(ResultElement->Value), aCond);

    RootValue->DefaultSubscribe(Subscriber, breReEvaluate);

    aCond->IdList->Free();
    aCond->Free();
  }
}
//---------------------------------------------------------------------------


