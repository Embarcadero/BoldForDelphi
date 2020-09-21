//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("BE_ProdStruct.res");
USEFORM("fMain.cpp", Form1);
USEFORM("dm1.cpp", dmMain); /* TDataModule: File Type */
USEUNIT("ProdStructClasses.cpp");
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
     Application->Initialize();
     Application->CreateForm(__classid(TForm1), &Form1);
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
