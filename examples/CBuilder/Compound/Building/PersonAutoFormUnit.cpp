//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "PersonAutoFormUnit.h"
#include "datamod.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "BoldAbstractListHandle"
#pragma link "BoldCheckBox"
#pragma link "BoldCursorHandle"
#pragma link "BoldEdit"
#pragma link "BoldGrid"
#pragma link "BoldHandles"
#pragma link "BoldListHandle"
#pragma link "BoldReferenceHandle"
#pragma link "BoldRootedHandles"
#pragma link "BoldSubscription"
#pragma resource "*.dfm"
TPersonAutoForm *PersonAutoForm;
//---------------------------------------------------------------------------
__fastcall TPersonAutoForm::TPersonAutoForm(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------
