//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
#include "fStart.h"
USERES("BE_Transaction.res");
USEFORM("fStart.cpp", frmStart);
USEFORM("fMain.cpp", frmMain);
USEFORM("dMain.cpp", dmMain); /* TDataModule: File Type */
USEUNIT("BankingClasses.cpp");
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  TfrmStart *start;
  try
  {
     Application->Initialize();
     Application->CreateForm(__classid(TdmMain), &dmMain);

     start = new TfrmStart(Application);
     try
     {
        if (start->ShowModal() == mrOk)
        {
          Application->CreateForm(__classid(TfrmMain), &frmMain);
          Application->Run();
        }
     }
     __finally
     {
        delete start;
     }
  }
  catch (Exception &exception)
  {
     Application->ShowException(&exception);
  }
  return 0;
}
//---------------------------------------------------------------------------

