
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

{$IFNDEF BOLD_ARCHITECT}
procedure Register;
procedure AddLicenseText(Product: String; LicenseText: TStrings);

var
  DeploymentKeyInfo: TStringList;
  LicenseInfo: TStringList;
  Licenses: TStringList;
{$ENDIF}

implementation

{$IFDEF BOLD_DEV}
  {$DEFINE BOLD_LICENSE}
{$ENDIF}

uses
  SysUtils,
  BoldRev,
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
  {$IFDEF BOLD_LICENSE}
  BoldProductControl,
  {$ENDIF}
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

{$IFNDEF BOLD_ARCHITECT}

{$IFDEF BOLD_LICENSE}
const
  OSSProductInfoBlock: TProductInfoBlock = (
    Sig1: $407e7e21; Sig2: $646c6f42; Sig3: $3f79654b;
    EvaluationKey: (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
    ReleaseKey: (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
    ActivationKey: (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
    ProdName: 'OSS';
    Release: (0, 0, 0, 0);
    Mask: 0;
    Sig4: $407e7e21; Sig5: $646c6f42; Sig6: $3f79654b;
  );

  OVEProductInfoBlock: TProductInfoBlock = (
    Sig1: $407e7e21; Sig2: $646c6f42; Sig3: $3f79654b;
    EvaluationKey: (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
    ReleaseKey: (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
    ActivationKey: (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
    ProdName: 'OVE';
    Release: (0, 0, 0, 0);
    Mask: 0;
    Sig4: $407e7e21; Sig5: $646c6f42; Sig6: $3f79654b;
  );

  CMSProductInfoBlock: TProductInfoBlock = (
    Sig1: $407e7e21; Sig2: $646c6f42; Sig3: $3f79654b;
    EvaluationKey: (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
    ReleaseKey: (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
    ActivationKey: (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
    ProdName: 'CMS';
    Release: (0, 0, 0, 0);
    Mask: 0;
    Sig4: $407e7e21; Sig5: $646c6f42; Sig6: $3f79654b;
  );

type
  TBoldProductControlClass = class of TBoldProductControl;

  TExposedPC = class(TBoldProductControl)
  end;

  TBoldProductControlOSS = class(TBoldProductControl)
  protected
    class function GetProductInfoBlock: TProductInfoBlock; override;
  end;

  TBoldProductControlCMS = class(TBoldProductControl)
  protected
    class function GetProductInfoBlock: TProductInfoBlock; override;
  end;

  TBoldProductControlOVE = class(TBoldProductControl)
  protected
    class function GetProductInfoBlock: TProductInfoBlock; override;
  end;
{$ENDIF}

procedure AddLicenseText(Product: String; LicenseText: TStrings);
begin
  LicenseInfo.Values[Product + '_LICENSE'] := LicenseText.CommaText;
end;

function CheckInstallation(LibraryPath: String): String;
begin
(*{$IFDEF BOLD_DELPHI}
  if (FileSearch('BoldSystem.dcu', LibraryPath)='') then
    MessageDlg('Can''t find BoldSystem.dcu in the library path. Add Bold for Delphis Lib folder to the library path for proper function, please.', mtWarning, [mbOk], 0);
  {$ENDIF}
  {$IFDEF BOLD_BCB}
  if (FileSearch('BoldSystem.obj', LibraryPath)='') then
    MessageDlg('Can''t find BoldSystem.obj in the library path. Add Bold for C++ Builder Lib folder to the library path for proper function, please.', mtWarning, [mbOk], 0);
  {$ENDIF} *)
end;

{$IFDEF BOLD_LICENSE}

function CheckResourceFile(PC: TBoldProductControl; Filename: String): Boolean;
var
  DeploymentKey: String;
begin
  DeploymentKey := GetDeploymentKeyFromResFile(FileName);

  DeploymentKeyInfo.Add(PC.Product + '_DeploymentKeyInFile=' + DeploymentKey);
  DeploymentKeyInfo.Add(PC.Product + '_DeploymentKeyExpected=' + TExposedPC(PC).GetDeploymentKey);

  result := DeploymentKey = TExposedPC(PC).GetDeploymentKey;
end;


function CheckLicenseWarning(PCClass: TBoldProductControlClass; LibraryPath: String; ProductLongName: String; Info: String): Boolean;
var
  PC: TBoldProductControl;
  Guard: IBoldGuard;
  DeploymentKeyName: string;
  FileName: string;
  f: File of byte;
  FileOK: Boolean;
begin
  if ProductLongName = '' then
    ProductLongName := 'Unknown';
  if ProductLongName[1] = '$' then
    ProductLongName := 'Bold for Delphi (DEV)';

  Guard := TBoldGuard.Create(PC);
  PC := PCClass.Create;
  PC.UpdateStatusInRepository;
  Licenses.Add(PC.Product);
  LicenseInfo.Add(PC.Product + '=' + PC.StatusString);
  LicenseInfo.Add(PC.Product + '_RELEASE=' + PC.Release);
  LicenseInfo.Add(PC.Product + '_LONGNAME=' + ProductLongName);
  LicenseInfo.Add(PC.Product + '_INFO=' + Info);

  TExposedPC(PC).SaveRegistry;
  result := PC.IssueLicenseWarning;
  LicenseInfo.add(PC.Product + '_PARAMS=' + PC.LicenseManagerParameters);
  if PC.IsValidReleaseOrEvalLicensePresent then
  begin
    DeploymentKeyName := format('BoldSoftDeploymentKey-%s-%s.res', [pc.Product, pc.Release]);
    FileName := FileSearch(DeploymentKeyName, LibraryPath);
    FileOK := FileName <> '';
    if FileOK then
    begin
      DeploymentKeyInfo.Add(PC.Product + '_FileName=' + FileName);
      AssignFile(f, FileName);
      Reset(f);
      FileOK := FileSize(f) > 44;
      DeploymentKeyInfo.Add(PC.Product + '_FileSize=' + IntToStr(FileSize(f)));
      CloseFile(f);
    end;

    if not FileOK then
    begin
      if GetBoldResPath <> '' then
      begin
        DeploymentKeyInfo.Add(PC.Product + '_Saved=' + GetBoldResPath);
        SaveDeploymentKeyToResFile(TExposedPC(PC).GetDeploymentKey, GetBoldResPath, PC.Product, pc.Release);
        FileName := FileSearch(DeploymentKeyName, LibraryPath);
        FileOK := FileName <> '';
      end;
    end;

    if FileOK then
    begin
      FileOK := CheckResourceFile(PC, FileName);
      if FileOK then
        DeploymentKeyInfo.Add(PC.Product + '=OK')
      else
        DeploymentKeyInfo.Add(PC.Product + '=Invalid');
    end
    else
      DeploymentKeyInfo.Add(PC.Product + '=Missing');
    result := result or not FileOK;
  end;
end;

function CheckForLicenseManager: Boolean;
var
  Registry: TRegistry;
  Guard: IBoldGuard;
begin
  Guard := TBoldGuard.Create(Registry);

  Registry := TRegistry.Create;
  registry.RootKey := HKEY_LOCAL_MACHINE;
  result := true;
  if Registry.OpenKey(LicenseManagerRegistryRoot, false) then
  begin
    if not FileExists(Registry.ReadString(ValueApplicationName)) then
    begin
      Showmessage('Bold License Manager was not found, please start it from the Windows menu');
      result := false;
    end;
  end;
end;
{$ENDIF}


function IssueLicenseWarning: Boolean;
var
  LibraryPath: String;
begin
  LibraryPath := GetLibraryPath;
  result := false;
  CheckInstallation(LibraryPath);
  DeploymentKeyInfo.Clear;
  {$IFDEF BOLD_LICENSE}
  if CheckForLicenseManager then
  begin
    {$BOOLEVAL ON}
    result :=
      CheckLicenseWarning(TBoldProductControl, LibraryPath, 'Bold for Delphi', 'Required') or
      CheckLicenseWarning(TBoldProductControlOSS, LibraryPath, 'ObjectSpace Synchronization Server', 'Optional') or
      CheckLicenseWarning(TBoldProductControlOVE, LibraryPath, 'Object Versioning Extension', 'Optional') or
      CheckLicenseWarning(TBoldProductControlCMS, LibraryPath, 'Concurrency Management Server', 'Optional');
    {$BOOLEVAL OFF}
  end;
  {$ENDIF}
end;

procedure Register;
{$IFDEF BOLD_LICENSE}
{$IFNDEF BOLD_DEV}
var
  frm: TfrmAboutBold;
{$ENDIF}
{$ENDIF}
begin
  if IssueLicenseWarning then
  begin
  {$IFDEF BOLD_LICENSE}
  {$IFNDEF BOLD_DEV}
    frm := TfrmAboutBold.create(nil);
    frm.ShowLicensePage;
    frm.free;
  {$ENDIF}
  {$ENDIF}
  end;
end;

{$IFDEF BOLD_LICENSE}

{ TBoldProductControlOSS }

class function TBoldProductControlOSS.GetProductInfoBlock: TProductInfoBlock;
begin
  result := OSSProductInfoBlock;
end;

{ TBoldProductControlCMS }

class function TBoldProductControlCMS.GetProductInfoBlock: TProductInfoBlock;
begin
  result := CMSProductInfoBlock;
end;

{ TBoldProductControlOVE }

class function TBoldProductControlOVE.GetProductInfoBlock: TProductInfoBlock;
begin
  result := OVEProductInfoBlock;
end;
{$ENDIF}

{$ENDIF}

{$IFDEF BOLD_TRIAL}
function CheckDelphiTrial: Boolean;
var
  RegKey: TRegistry;
begin
  result := true;
  RegKey := TRegistry.Create;
  if Regkey.OpenKey(BOLD_HOST_IDE_REGISTRYPATH, false) then
  begin
    result := (RegKey.ReadString('LMKEY') = '') and (RegKey.ReadString('LMLIC') = '');
    result := result and not FileExists(RegKey.ReadString('RootDir')+'\bin\dcc32.exe');
  end;
  RegKey.Free
end;
{$ENDIF}

initialization
  BoldRegisterModuleVersion('$Workfile: BoldInstallation.pas $ $Revision: 37 $ $Date: 02-08-11 23:56 $');
  {$IFDEF BOLD_TRIAL}
  if not CheckDelphiTrial then
  begin
    {$IFDEF BOLD_DEV}
    showmessage(BOLD_TRIALMESSAGE3);
    {$ELSE}
    raise Exception.Create(BOLD_TRIALMESSAGE3);
    {$ENDIF}
  end;
  {$ENDIF}
  {$IFNDEF BOLD_ARCHITECT}
  LicenseInfo := TStringList.create;
  Licenses := TStringList.create;
  DeploymentKeyInfo := TStringList.create;

finalization
  FreeAndNil(DeploymentKeyInfo);
  FreeAndNil(LicenseInfo);
  FReeAndNil(Licenses);
  {$ENDIF}
end.
