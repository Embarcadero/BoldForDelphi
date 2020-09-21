unit BoldMMPlugin_TLB;

// ************************************************************************ //
// WARNING                                                                    
// -------                                                                    
// The types declared in this file were generated from data read from a       
// Type Library. If this type library is explicitly or indirectly (via        
// another type library referring to this type library) re-imported, or the   
// 'Refresh' command of the Type Library Editor activated while editing the   
// Type Library, the contents of this file will be regenerated and all        
// manual modifications will be lost.                                         
// ************************************************************************ //

// PASTLWTR : 1.2
// File generated on 8/12/2002 10:25:54 AM from Type Library described below.

// ************************************************************************  //
// Type Lib: C:\vss\Development\BfD\Source\UMLModel\ModelLinks\ModelMaker\MMPlugin\BoldMMPlugin.tlb (1)
// LIBID: {08A3186D-598D-4595-8B1C-76D9FFDF952A}
// LCID: 0
// Helpfile: 
// HelpString: TypeLib1 Library
// DepndLst: 
//   (1) v2.0 stdole, (C:\WINDOWS\System32\stdole2.tlb)
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
interface

uses Windows, ActiveX, Classes, Graphics, StdVCL, Variants;
  

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  BoldMMPluginMajorVersion = 1;
  BoldMMPluginMinorVersion = 0;

  LIBID_BoldMMPlugin: TGUID = '{08A3186D-598D-4595-8B1C-76D9FFDF952A}';

  IID_IBoldExpert: TGUID = '{4BBAE613-E7D6-4924-B2C3-43BB849EEC79}';
  CLASS_CBoldExpert: TGUID = '{72789CDE-7A9C-475D-B935-ABF9283D9400}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IBoldExpert = interface;
  IBoldExpertDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  CBoldExpert = IBoldExpert;


// *********************************************************************//
// Interface: IBoldExpert
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {4BBAE613-E7D6-4924-B2C3-43BB849EEC79}
// *********************************************************************//
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

// *********************************************************************//
// DispIntf:  IBoldExpertDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {4BBAE613-E7D6-4924-B2C3-43BB849EEC79}
// *********************************************************************//
  IBoldExpertDisp = dispinterface
    ['{4BBAE613-E7D6-4924-B2C3-43BB849EEC79}']
    function GetModelAsString: WideString; dispid 1;
    procedure OpenProject(const FileName: WideString); dispid 2;
    property ProjectFileName: WideString readonly dispid 3;
    function SetModelAsString(const ModelAsString: WideString): WideString; dispid 4;
    procedure NewProject(const FileName: WideString); dispid 5;
    procedure SaveProject(SaveAs: WordBool); dispid 6;
  end;

// *********************************************************************//
// The Class CoCBoldExpert provides a Create and CreateRemote method to          
// create instances of the default interface IBoldExpert exposed by              
// the CoClass CBoldExpert. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
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
