//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("BatchUpgrader.res");
USEFORM("..\Common\dSystemTypeInfo.cpp", dmSystemTypeInfo); /* TDataModule: File Type */
USEFORM("dPersistence.cpp", dmPersistence); /* TDataModule: File Type */
USEFORM("dModel.cpp", dmModel); /* TDataModule: File Type */
USEFORM("..\Common\fBatchUpgraderGui.cpp", Form1);
USEUNIT("ModelEvClasses.cpp");
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
     Application->Initialize();
     Application->CreateForm(__classid(TdmModel), &dmModel);
     Application->CreateForm(__classid(TdmSystemTypeInfo), &dmSystemTypeInfo);
     Application->CreateForm(__classid(TdmPersistence), &dmPersistence);
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
