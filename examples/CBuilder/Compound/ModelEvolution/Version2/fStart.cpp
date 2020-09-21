//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "fStart.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "BoldDbEvolutorForm"
#pragma link "BoldDbEvolutor"
#pragma resource "*.dfm"
TfrmStart *frmStart;
//---------------------------------------------------------------------------
__fastcall TfrmStart::TfrmStart(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmStart::Button1Click(TObject *Sender)
{
  TBoldDataBaseEvolutor *Evolutor;
  TfrmBoldDbEvolutor *Form;

  Evolutor = new TBoldDataBaseEvolutor(dmPersistence->PersistenceHandle);
  Form = new TfrmBoldDbEvolutor(this);
  try
  {
    Evolutor->GenericScript = true;
    Evolutor->CalculateScript();
    Evolutor->GenerateScript(Form->SQLScript, Form->MappingInfoScript);
    Evolutor->GenerateWarnings(Form->Warnings);
    if (Form->ShowModal() == mrOk)
    {
      Evolutor->ExecuteScript();
      dmSystem->SystemHandle->Active = true;
      ModalResult = mrOk;
    }
  }
  __finally
  {
    Evolutor->Free();
    Form->Free();
  }
}
//---------------------------------------------------------------------------
