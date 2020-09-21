//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
#include "fStart.h"
USERES("BE_ModelEv_v2.res");
USEFORM("fStart.cpp", frmStart);
USEFORM("dModel.cpp", dmModel); /* TDataModule: File Type */
USEFORM("dPersistence.cpp", dmPersistence); /* TDataModule: File Type */
USEFORM("..\Common\dSystem.cpp", dmSystem); /* TDataModule: File Type */
USEFORM("..\Common\dSystemTypeInfo.cpp", dmSystemTypeInfo); /* TDataModule: File Type */
USEUNIT("ModelEvClasses.cpp");
USEFORM("fMain.cpp", frmMain);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
     Application->Initialize();
     Application->CreateForm(__classid(TdmModel), &dmModel);
     Application->CreateForm(__classid(TdmPersistence), &dmPersistence);
     Application->CreateForm(__classid(TdmSystem), &dmSystem);
     Application->CreateForm(__classid(TdmSystemTypeInfo), &dmSystemTypeInfo);

     TfrmStart *start;
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
