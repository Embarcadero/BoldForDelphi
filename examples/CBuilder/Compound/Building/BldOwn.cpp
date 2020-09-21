//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("BldOwn.res");
USEFORM("Mainform.cpp", allform);
USEFORM("datamod.cpp", DataModule1); /* TDataModule: File Type */
USEFORM("PersonAutoFormUnit.cpp", PersonAutoForm);
USEUNIT("BuildingClasses.cpp");
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
                 Application->Initialize();
                 Application->CreateForm(__classid(TDataModule1), &DataModule1);
     Application->CreateForm(__classid(Tallform), &allform);
     Application->CreateForm(__classid(TPersonAutoForm), &PersonAutoForm);
     Application->Run();
        }
        catch (Exception &exception)
        {
                 Application->ShowException(&exception);
        }
        return 0;
}
//---------------------------------------------------------------------------
