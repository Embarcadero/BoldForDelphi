//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "fMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "BoldAbstractModel"
#pragma link "BoldCheckBox"
#pragma link "BoldHandles"
#pragma link "BoldLabel"
#pragma link "BoldMemo"
#pragma link "BoldModel"
#pragma link "BoldPropertiesController"
#pragma link "BoldReferenceHandle"
#pragma link "BoldSubscription"
#pragma link "BoldSystemHandle"
#pragma link "BoldTrackBar"
#pragma resource "*.dfm"

TfrmMain *frmMain;
//---------------------------------------------------------------------------
__fastcall TfrmMain::TfrmMain(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnClearClick(TObject *Sender)
{
  Game->ClearCells();
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnTickClick(TObject *Sender)
{
  Screen->Cursor = crHourGlass;
  Game->Tick();
  Screen->Cursor = crDefault;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnStartClick(TObject *Sender)
{
  Timer1->Enabled = !(Timer1->Enabled);
  if (Timer1->Enabled)
    dynamic_cast <TButton*>(Sender)->Caption = "Stop ticking";
  else
    dynamic_cast <TButton*>(Sender)->Caption = "Start ticking";
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::FormCreate(TObject *Sender)
{
  TStringList* StringList = new TStringList();

  refGame->Value = new TGame(NULL);
  StringList->LoadFromFile("instructions.txt");
  ShowMessage(StringList->Text);
  StringList->Free();
}
//---------------------------------------------------------------------------
TGame* __fastcall TfrmMain::GetGame(void)
{
  //TGame *resultGame = new TGame(NULL);
  TGame *resultGame = dynamic_cast <TGame*>(refGame->Value);
  return resultGame;
}


