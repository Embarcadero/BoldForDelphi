//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("BE_ObjectVersioning.res");
USEFORM("dModel.cpp", dmModel); /* TDataModule: File Type */
USEFORM("fMain.cpp", frmMain);
USEUNIT("DocumentClasses.cpp");
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
     Application->Initialize();
     Application->CreateForm(__classid(TdmModel), &dmModel);
     Application->CreateForm(__classid(TfrmMain), &frmMain);
     Application->Run();
  }
  catch (Exception &exception)
  {
     Application->ShowException(&exception);
  }
  return 0;
}
//---------------------------------------------------------------------------
