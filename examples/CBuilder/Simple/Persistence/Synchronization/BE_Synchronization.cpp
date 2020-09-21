//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("BE_Synchronization.res");
USEFORM("fMain.cpp", frmMain);
USEFORM("dModel.cpp", DataModule1); /* TDataModule: File Type */
USEUNIT("BldOwnClasses.cpp");
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
     Application->Initialize();
     Application->CreateForm(__classid(TfrmMain), &frmMain);
     Application->CreateForm(__classid(TDataModule1), &DataModule1);
     Application->Run();
  }
  catch (Exception &exception)
  {
     Application->ShowException(&exception);
  }
  return 0;
}
//---------------------------------------------------------------------------
