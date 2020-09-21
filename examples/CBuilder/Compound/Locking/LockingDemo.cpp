//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("LockingDemo.res");
USEFORM("Common\dModel.cpp", dmModel); /* TDataModule: File Type */
USEFORM("dmMain.cpp", dmMainLocking); /* TDataModule: File Type */
USEFORM("MainForm.cpp", Form1);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
     Application->Initialize();
     Application->CreateForm(__classid(TdmModel), &dmModel);
     Application->CreateForm(__classid(TdmMainLocking), &dmMainLocking);
     Application->CreateForm(__classid(TForm1), &Form1);
     Application->Run();
  }
  catch (Exception &exception)
  {
     Application->ShowException(&exception);
  }
  return 0;
}
//---------------------------------------------------------------------------
