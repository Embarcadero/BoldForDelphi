//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("DbGen.res");
USEFORM("DbGenForm.cpp", frmDBGen);
USEFORM("..\Common\dModel.cpp", dmModel); /* TDataModule: File Type */
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
     Application->Initialize();
     Application->CreateForm(__classid(TdmModel), &dmModel);
     Application->CreateForm(__classid(TfrmDBGen), &frmDBGen);
     Application->Run();
  }
  catch (Exception &exception)
  {
     Application->ShowException(&exception);
  }
  return 0;
}
//---------------------------------------------------------------------------
