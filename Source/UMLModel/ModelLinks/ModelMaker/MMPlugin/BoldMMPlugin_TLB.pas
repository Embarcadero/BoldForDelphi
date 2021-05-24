
{ Global compiler directives }
{$include bold.inc}
unit BoldMMPlugin_TLB;




















{$TYPEDADDRESS OFF}
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
interface

uses Windows, ActiveX, Classes, Graphics, StdVCL, Variants;






const
  BoldMMPluginMajorVersion = 1;
  BoldMMPluginMinorVersion = 0;

  LIBID_BoldMMPlugin: TGUID = '{08A3186D-598D-4595-8B1C-76D9FFDF952A}';

  IID_IBoldExpert: TGUID = '{4BBAE613-E7D6-4924-B2C3-43BB849EEC79}';
  CLASS_CBoldExpert: TGUID = '{72789CDE-7A9C-475D-B935-ABF9283D9400}';
type


  IBoldExpert = interface;
  IBoldExpertDisp = dispinterface;



  CBoldExpert = IBoldExpert;




  IBoldExpert = interface(IDispatch)
    ['{4BBAE613-E7D6-4924-B2C3-43BB849EEC79}']
    function GetModelAsString: WideString; safecall;
    procedure OpenProject(const FileName: WideString); safecall;
    function Get_ProjectFileName: WideString; safecall;
    function SetModelAsString(const ModelAsString: WideString): WideString; safecall;
    procedure NewProject(const FileName: WideString); safecall;
    procedure SaveProject(SaveAs: WordBool); safecall;
    property ProjectFileName: WideString read Get_ProjectFileName;
  end;




  IBoldExpertDisp = dispinterface
    ['{4BBAE613-E7D6-4924-B2C3-43BB849EEC79}']
    function GetModelAsString: WideString; dispid 1;
    procedure OpenProject(const FileName: WideString); dispid 2;
    property ProjectFileName: WideString readonly dispid 3;
    function SetModelAsString(const ModelAsString: WideString): WideString; dispid 4;
    procedure NewProject(const FileName: WideString); dispid 5;
    procedure SaveProject(SaveAs: WordBool); dispid 6;
  end;






  CoCBoldExpert = class
    class function Create: IBoldExpert;
    class function CreateRemote(const MachineName: string): IBoldExpert;
  end;

implementation

uses ComObj;

class function CoCBoldExpert.Create: IBoldExpert;
begin
  Result := CreateComObject(CLASS_CBoldExpert) as IBoldExpert;
end;

class function CoCBoldExpert.CreateRemote(const MachineName: string): IBoldExpert;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CBoldExpert) as IBoldExpert;
end;

end.
