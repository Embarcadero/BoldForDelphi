
{*******************************************************}
{                                                       }
{       Borland Delphi Runtime Library                  }
{       COM server support                              }
{                                                       }
{       Copyright (C) 1997,99 Inprise Corporation       }
{                                                       }
{*******************************************************}

unit ComServ;

{$DENYPACKAGEUNIT}

interface

uses Windows, Messages, ActiveX, SysUtils, ComObj;

type

{ Application start mode }

  TStartMode = (smStandalone, smAutomation, smRegServer, smUnregServer);

{ Class manager event types }

  TLastReleaseEvent = procedure(var Shutdown: Boolean) of object;

{ TComServer }

  TComServer = class(TComServerObject)
  private
    FObjectCount: Integer;
    FFactoryCount: Integer;
    FTypeLib: ITypeLib;
    FServerName: string;
    FHelpFileName: string;
    FIsInprocServer: Boolean;
    FStartMode: TStartMode;
    FStartSuspended: Boolean;
    FRegister: Boolean;
    FUIInteractive: Boolean;
    FOnLastRelease: TLastReleaseEvent;
    procedure FactoryFree(Factory: TComObjectFactory);
    procedure FactoryRegisterClassObject(Factory: TComObjectFactory);
    procedure FactoryUpdateRegistry(Factory: TComObjectFactory);
    procedure LastReleased;
  protected
    function CountObject(Created: Boolean): Integer; override;
    function CountFactory(Created: Boolean): Integer; override;
    function GetHelpFileName: string; override;
    function GetServerFileName: string; override;
    function GetServerKey: string; override;
    function GetServerName: string; override;
    function GetStartSuspended: Boolean; override;
    function GetTypeLib: ITypeLib; override;
    procedure SetHelpFileName(const Value: string); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Initialize;
    procedure LoadTypeLib;
    procedure SetServerName(const Name: string);
    procedure UpdateRegistry(Register: Boolean);
    property IsInprocServer: Boolean read FIsInprocServer write FIsInprocServer;
    property ObjectCount: Integer read FObjectCount;
    property StartMode: TStartMode read FStartMode;
    property UIInteractive: Boolean read FUIInteractive write FUIInteractive;
    property OnLastRelease: TLastReleaseEvent read FOnLastRelease write FOnLastRelease;
  end;

var
  ComServer: TComServer;

function DllGetClassObject(const CLSID, IID: TGUID; var Obj): HResult; stdcall;
function DllCanUnloadNow: HResult; stdcall;
function DllRegisterServer: HResult; stdcall;
function DllUnregisterServer: HResult; stdcall;

implementation

uses ComConst;

function GetModuleFileName: string;
var
  Buffer: array[0..261] of Char;
begin
  SetString(Result, Buffer, Windows.GetModuleFileName(HInstance,
    Buffer, SizeOf(Buffer)));
end;

function GetModuleName: string;
begin
  Result := ChangeFileExt(ExtractFileName(GetModuleFileName), '');
end;

function LoadTypeLibrary(const ModuleName: string): ITypeLib;
begin
  OleCheck(LoadTypeLib(PWideChar(WideString(ModuleName)), Result));
end;

procedure RegisterTypeLibrary(TypeLib: ITypeLib; const ModuleName: string);
var
  Name: WideString;
  HelpPath: WideString;
begin
  Name := ModuleName;
  HelpPath := ExtractFilePath(ModuleName);
  OleCheck(RegisterTypeLib(TypeLib, PWideChar(Name), PWideChar(HelpPath)));
end;

procedure UnregisterTypeLibrary(TypeLib: ITypeLib);
type
  TUnregisterProc = function(const GUID: TGUID; VerMajor, VerMinor: Word;
    LCID: TLCID; SysKind: TSysKind): HResult stdcall;
var
  Handle: THandle;
  UnregisterProc: TUnregisterProc;
  LibAttr: PTLibAttr;
begin
  Handle := GetModuleHandle('OLEAUT32.DLL');
  if Handle <> 0 then
  begin
    @UnregisterProc := GetProcAddress(Handle, 'UnRegisterTypeLib');
    if @UnregisterProc <> nil then
    begin
      OleCheck(ComServer.TypeLib.GetLibAttr(LibAttr));
      with LibAttr^ do
        UnregisterProc(guid, wMajorVerNum, wMinorVerNum, lcid, syskind);
      ComServer.TypeLib.ReleaseTLibAttr(LibAttr);
    end;
  end;
end;

function GetTypeLibName(TypeLib: ITypeLib): string;
var
  Name: WideString;
begin
  OleCheck(TypeLib.GetDocumentation(-1, @Name, nil, nil, nil));
  Result := Name;
end;

function DllGetClassObject(const CLSID, IID: TGUID; var Obj): HResult;
var
  Factory: TComObjectFactory;
begin
  Factory := ComClassManager.GetFactoryFromClassID(CLSID);
  if Factory <> nil then
    if Factory.GetInterface(IID, Obj) then
      Result := S_OK
    else
      Result := E_NOINTERFACE
  else
  begin
    Pointer(Obj) := nil;
    Result := CLASS_E_CLASSNOTAVAILABLE;
  end;
end;

function DllCanUnloadNow: HResult;
begin
  if (ComServer = nil) or
    ((ComServer.FObjectCount = 0) and (ComServer.FFactoryCount = 0)) then
    Result := S_OK
  else
    Result := S_FALSE;
end;

function DllRegisterServer: HResult;
begin
  Result := S_OK;
  try
    ComServer.UpdateRegistry(True);
  except
    Result := E_FAIL;
  end;
end;

function DllUnregisterServer: HResult;
begin
  Result := S_OK;
  try
    ComServer.UpdateRegistry(False);
  except
    Result := E_FAIL;
  end;
end;

{ Automation TerminateProc }

function AutomationTerminateProc: Boolean;
begin
  Result := True;
  if (ComServer <> nil) and (ComServer.ObjectCount > 0) and ComServer.UIInteractive then
  begin
    Result := MessageBox(0, PChar(SNoCloseActiveServer1 + SNoCloseActiveServer2),
      PChar(SAutomationWarning), MB_YESNO or MB_TASKMODAL or
      MB_ICONWARNING or MB_DEFBUTTON2) = IDYES;
  end;
end;

{ TComServer }

constructor TComServer.Create;

  function FindSwitch(const Switch: string): Boolean;
  begin
    Result := FindCmdLineSwitch(Switch, ['-', '/'], True);
  end;

begin
  FTypeLib := nil;
  FIsInprocServer := ModuleIsLib;
  if FindSwitch('AUTOMATION') or FindSwitch('EMBEDDING') then
    FStartMode := smAutomation
  else if FindSwitch('REGSERVER') then
    FStartMode := smRegServer
  else if FindSwitch('UNREGSERVER') then
    FStartMode := smUnregServer;
  FUIInteractive := True;
end;

destructor TComServer.Destroy;
begin
  ComClassManager.ForEachFactory(Self, FactoryFree);
end;

function TComServer.CountObject(Created: Boolean): Integer;
begin
  if Created then
  begin
    Result := InterlockedIncrement(FObjectCount);
    if (not IsInProcServer) and (StartMode = smAutomation)
      and Assigned(ComObj.CoAddRefServerProcess) then
      ComObj.CoAddRefServerProcess;
  end
  else
  begin
    Result := InterlockedDecrement(FObjectCount);
    if (not IsInProcServer) and (StartMode = smAutomation)
      and Assigned(ComObj.CoReleaseServerProcess) then
    begin
      if ComObj.CoReleaseServerProcess = 0 then
        LastReleased;
    end
    else if Result = 0 then
      LastReleased;
  end;
end;

function TComServer.CountFactory(Created: Boolean): Integer;
begin
  if Created then
    Result := InterlockedIncrement(FFactoryCount)
  else
    Result := InterlockedDecrement(FFactoryCount);
end;

procedure TComServer.FactoryFree(Factory: TComObjectFactory);
begin
  Factory.Free;
end;

procedure TComServer.FactoryRegisterClassObject(Factory: TComObjectFactory);
begin
  Factory.RegisterClassObject;
end;

procedure TComServer.FactoryUpdateRegistry(Factory: TComObjectFactory);
begin
  if Factory.Instancing <> ciInternal then
    Factory.UpdateRegistry(FRegister);
end;

function TComServer.GetHelpFileName: string;
begin
  Result := FHelpFileName;
end;

function TComServer.GetServerFileName: string;
begin
  Result := GetModuleFileName;
end;

function TComServer.GetServerKey: string;
begin
  if FIsInprocServer then
    Result := 'InprocServer32' else
    Result := 'LocalServer32';
end;

function TComServer.GetServerName: string;
begin
  if FServerName <> '' then
    Result := FServerName
  else
    if FTypeLib <> nil then
      Result := GetTypeLibName(FTypeLib)
    else
      Result := GetModuleName;
end;

procedure TComServer.SetServerName(const Name: string);
begin
  if FTypeLib = nil then
    FServerName := Name;
end;

function TComServer.GetTypeLib: ITypeLib;
begin
  LoadTypeLib;
  Result := FTypeLib;
end;

procedure TComServer.Initialize;
begin
  try
    UpdateRegistry(FStartMode <> smUnregServer);
  except
    on E: EOleRegistrationError do
      // User may not have write access to the registry.
      // Squelch the exception unless we were explicitly told to register.
      if FStartMode = smRegServer then raise;
  end;
  if FStartMode in [smRegServer, smUnregServer] then Halt;
  ComClassManager.ForEachFactory(Self, FactoryRegisterClassObject);
end;

procedure TComServer.LastReleased;
var
  Shutdown: Boolean;
begin
  if not FIsInprocServer then
  begin
    Shutdown := FStartMode = smAutomation;
    try
      if Assigned(FOnLastRelease) then FOnLastRelease(Shutdown);
    finally
      if Shutdown then PostThreadMessage(MainThreadID, WM_QUIT, 0, 0);
    end;
  end;
end;

procedure TComServer.LoadTypeLib;
var
  Temp: ITypeLib;
begin
  if FTypeLib = nil then
  begin
  // this may load typelib more than once, but avoids need for critical section
  // and releases the interface correctly
    Temp := LoadTypeLibrary(GetModuleFileName);
    Integer(Temp) := InterlockedExchange(Integer(FTypeLib), Integer(Temp));
  end;
end;

procedure TComServer.UpdateRegistry(Register: Boolean);
begin
  if FTypeLib <> nil then
    if Register then
      RegisterTypeLibrary(FTypeLib, GetModuleFileName) else
      UnregisterTypeLibrary(FTypeLib);
  FRegister := Register;
  ComClassManager.ForEachFactory(Self, FactoryUpdateRegistry);
end;

var
  SaveInitProc: Pointer = nil;
  OleAutHandle: Integer;

procedure InitComServer;
begin
  if SaveInitProc <> nil then TProcedure(SaveInitProc);
  ComServer.FStartSuspended := (CoInitFlags <> -1) and
    Assigned(ComObj.CoInitializeEx) and Assigned(ComObj.CoResumeClassObjects);
  ComServer.Initialize;
  if ComServer.FStartSuspended then
    ComObj.CoResumeClassObjects;
end;

function TComServer.GetStartSuspended: Boolean;
begin
  Result := FStartSuspended;
end;

procedure TComServer.SetHelpFileName(const Value: string);
begin
  FHelpFileName := Value;
end;

initialization
begin
  OleAutHandle := SafeLoadLibrary('OLEAUT32.DLL');
  ComServer := TComServer.Create;
  if not ModuleIsLib then
  begin
    SaveInitProc := InitProc;
    InitProc := @InitComServer;
    AddTerminateProc(@AutomationTerminateProc);
  end;
end;

finalization
begin
  ComServer.Free;
  ComServer := nil;
  FreeLibrary(OleAutHandle);
end;

end.
