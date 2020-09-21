//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("BE_BoldImage.res");
USEFORM("fMain.cpp", frmMain);
USEFORM("fViewAutoSize.cpp", frmImageViewer);
USEFORM("fViewClipboardFmt.cpp", frmViewClipboardFmt);
USEFORM("fViewStretch.cpp", frmStretch);
USEUNIT("ImageDemoClasses.cpp");
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
     Application->Initialize();
     Application->CreateForm(__classid(TfrmMain), &frmMain);
     Application->CreateForm(__classid(TfrmImageViewer), &frmImageViewer);
     Application->CreateForm(__classid(TfrmViewClipboardFmt), &frmViewClipboardFmt);
     Application->CreateForm(__classid(TfrmStretch), &frmStretch);
     Application->Run();
  }
  catch (Exception &exception)
  {
     Application->ShowException(&exception);
  }
  return 0;
}
//---------------------------------------------------------------------------
