//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("BE_SQLHandle.res");
USEFORM("fMain.cpp", Form1);
USEUNIT("SQLHandleClasses.cpp");
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
     Application->Initialize();
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
