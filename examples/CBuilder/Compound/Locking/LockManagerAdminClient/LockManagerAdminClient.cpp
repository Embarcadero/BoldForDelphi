//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USEFORM("MainForm.cpp", frmMain);
USEFORM("dmLockManagerAdmin.cpp", dmMain); /* TDataModule: File Type */
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
     Application->Initialize();
     //Application->CreateForm(__classid(TdmMain), &dmMain);
     Application->CreateForm(__classid(TfrmMain), &frmMain);
     Application->CreateForm(__classid(TdmMain), &dmMain);
     Application->Run();
  }
  catch (Exception &exception)
  {
     Application->ShowException(&exception);
  }
  return 0;
}
//---------------------------------------------------------------------------




