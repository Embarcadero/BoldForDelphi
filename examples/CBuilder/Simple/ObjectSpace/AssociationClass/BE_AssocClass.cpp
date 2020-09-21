//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("BE_AssocClass.res");
USEFORM("MainForm.cpp", frmMain);
USEUNIT("AssociationClassExampleClasses.cpp");
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
