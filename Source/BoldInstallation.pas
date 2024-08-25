
/////////////////////////////////////////////////////////
//                                                     //
//              Bold for Delphi                        //
//    Copyright (c) 2002 BoldSoft AB, Sweden           //
//                                                     //
/////////////////////////////////////////////////////////

{ Global compiler directives }
{$include bold.inc}
unit BoldInstallation;

{$IFNDEF BOLD_DEV}
{$DEBUGINFO OFF}
{$LOCALSYMBOLS OFF}
{$OPTIMIZATION ON}
{$ENDIF}

interface

uses
  BoldRegistry,
  Classes;

function GetLibraryPath: String;

implementation

uses
  SysUtils,
  BoldUtils,
  Dialogs,
  Registry,
  Windows,
  {$IFNDEF NO_OTA}
  ToolsAPI,
  {$ENDIF}
  BoldAbout,
  BoldDefs,
  BoldDefsDT,
  BoldGuard;

function GetLibraryPath: String;
const
  {$IFDEF BOLD_DELPHI}
  ROOT_MACRO = '';
  {$ENDIF}
  {$IFDEF BOLD_BCB}
  ROOT_MACRO = '';
  {$ENDIF}
var
  Registry: TRegistry;
  Guard: IBoldGuard;
  RegIniFile: TRegIniFile;
  {$IFNDEF NO_OTA}
  OTAServices: IOTAServices;
  {$ENDIF}
begin
  Guard := TBoldGuard.Create(Registry, RegIniFile);

  result := '';
  {$IFNDEF NO_OTA}
  if Supports(BorlandIDEServices, IOTAServices, OTAServices) then
  begin
    RegIniFile := TRegIniFile.Create(OTAServices.GetBaseRegistryKey);
    Result := RegIniFile.ReadString(regLibrary, regSearchPath, '');
  end;
  {$ENDIF}

  Registry := TRegistry.Create;
  registry.RootKey := HKEY_LOCAL_MACHINE;
  if Registry.OpenKey(BOLD_HOST_IDE_REGISTRYPATH, false) and (Registry.ReadString('RootDir') <> '') then
    Result := StringReplace(Result, ROOT_MACRO, Registry.ReadString('RootDir'), [rfReplaceAll, rfIgnoreCase]);
end;

end.
