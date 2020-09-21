//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USEFORM("ServerForm.cpp", frmServer);
USEFORM("..\Common\dModel.cpp", dmModel); /* TDataModule: File Type */
USEFORM("PServerDM.cpp", dmPServer); /* TDataModule: File Type */
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
     Application->Initialize();
     Application->CreateForm(__classid(TfrmServer), &frmServer);
     Application->CreateForm(__classid(TdmModel), &dmModel);
     Application->CreateForm(__classid(TdmPServer), &dmPServer);
     Application->Run();
  }
  catch (Exception &exception)
  {
     Application->ShowException(&exception);
  }
  return 0;
}
//---------------------------------------------------------------------------
