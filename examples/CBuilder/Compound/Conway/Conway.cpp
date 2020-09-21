//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("Conway.res");
USEFORM("fMain.cpp", frmMain);
USEUNIT("ConwayClasses.cpp");
USEUNIT("ConwayClasses_impl.cpp");
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
     Application->Initialize();
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
