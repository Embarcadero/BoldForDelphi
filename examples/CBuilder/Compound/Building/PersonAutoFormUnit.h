//---------------------------------------------------------------------------

#ifndef PersonAutoFormUnitH
#define PersonAutoFormUnitH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "BoldAbstractListHandle.hpp"
#include "BoldCheckBox.hpp"
#include "BoldCursorHandle.hpp"
#include "BoldEdit.hpp"
#include "BoldGrid.hpp"
#include "BoldHandles.hpp"
#include "BoldListHandle.hpp"
#include "BoldReferenceHandle.hpp"
#include "BoldRootedHandles.hpp"
#include "BoldSubscription.hpp"
#include <Grids.hpp>
//---------------------------------------------------------------------------
class TPersonAutoForm : public TForm
{
__published:	// IDE-managed Components
        TBoldEdit *beFirstName;
        TBoldEdit *BoldEdit2;
        TBoldEdit *BoldEdit3;
        TLabel *Label1;
        TLabel *Label2;
        TLabel *Label3;
        TBoldCheckBox *bcbIsMarried;
        TLabel *Label4;
        TBoldGrid *bgrOwnedBuildings;
        TBoldReferenceHandle *brhPerson;
        TBoldListHandle *blhOwnedBuildings;
private:	// User declarations
public:		// User declarations
        __fastcall TPersonAutoForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TPersonAutoForm *PersonAutoForm;
//---------------------------------------------------------------------------
#endif
