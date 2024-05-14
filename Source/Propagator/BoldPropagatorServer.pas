
{ Global compiler directives }
{$include bold.inc}
unit BoldPropagatorServer;

interface

uses
  comobj,
  BoldAdvancedPropagator,
  BoldAdvancedPropagatorCOM,
  BoldClientHandlerCOM,
  BoldEnqueuerCOM,
  BoldLockManagerCOM,
  BoldLockManager,
  BoldLockManagerAdmin,
  BoldContainers,
  BoldThreadedComObjectFactory
  ;
type
  TBoldPropagatorServer = class;
  TBoldPropagatorServerClass = class of TBoldPropagatorServer;

  {TBoldPropagatorServer}
  TBoldPropagatorServer = class
  private
    FModuleName: string;
    FClientNotifierPoolSize: integer;
    FEnableLogging: Boolean;
    FLogFileName,
    FErrorLogFileName,
    fThreadLogFileName: string;
    FMaxLogFileSize: integer;
    FServerName : string;
    FClientHandlerCLSID,
    FEnqueuerCLSID,
    FLockManagerCLSID,
    FLockManagerAdminCLSID,
    FPropagatorCLSID : TGuid;
    FAdvancedPropagator: TBoldAdvancedPropagator;
    fLockManager: TBoldLockManager;
    fLockManagerAdmin: TBoldLockManagerAdmin;
    fDisconnectClientsOnSendFailure: Boolean;
    FInitialized: Boolean;
    FComObjects: TBoldObjectArray;
    function getLockManager: TBoldLockManager;
    function getModuleName: string;
    function getLockManagerAdmin: TBoldLockManagerAdmin;
    procedure CreateClassFactories;
    function GetAppID: TGuid;
    function GetComObjectCount: integer;
    function GetObjectByIndex(Index: integer): TBoldComObject;
  protected
    procedure RegServer(const AppId: TGuid);
    function getAdvancedPropagator: TBoldAdvancedPropagator; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadConfiguration;
    procedure SetConfiguration(const ServerName: string);
    procedure Initialize;
    class function Instance: TBoldPropagatorServer;
    class procedure FreeSingleton;
    procedure AddComObject(Obj: TBoldComObject);
    procedure RemoveComObject(Obj: TBoldComObject);
    property AdvancedPropagator: TBoldAdvancedPropagator read getAdvancedPropagator;
    property LockManager: TBoldLockManager read getLockManager;
    property LockManagerAdmin: TBoldLockManagerAdmin read getLockManagerAdmin;
    property ModuleName: string read getModuleName;
    property Initialized: Boolean read FInitialized write FInitialized;
    property ServerName: string read fServerName;
    property ClientHandlerCLSID: TGuid read fClientHandlerCLSID;
    property EnqueuerCLSID: TGuid read fEnqueuerCLSID;
    property LockManagerCLSID: TGuid read fLockManagerCLSID;
    property LockManagerAdminCLSID: TGuid read fLockManagerAdminCLSID;
    property PropagatorConnectionCLSID: TGuid read fPropagatorCLSID;
    property AppID: TGuid read GetAppID;
    property ComObjects[Index: integer]: TBoldComObject read GetObjectByIndex;
    property ComObjectCount: integer read GetComObjectCount;
  end;

  function _DllCanUnloadNow: HResult;

implementation

uses
  Sysutils,
  BoldUtils,
  BoldPropagatorGUIDs,
  BoldApartmentThread,
  BoldPropagatorInterfaces_TLB,
  BoldLockingSupportInterfaces_TLB,
  BoldPropagatorConstants,
  BoldComServer,
  BoldServerHandlesDataMod,
  BoldLockManagerAdminCOM,
  BoldComUtils,
  BoldGUIDUtils,
  BoldThreadSafeLog,
  inifiles,
  Forms,
  dialogs,
  windows,
  comserv,
  registry,
  BoldDefs
  ;

var
  G_PropagatorServer: TBoldPropagatorServer = nil;

function _DllCanUnloadNow: HResult;
begin
  Result := DllCanUnloadNow;
  if Result = S_OK then
    TBoldPropagatorServer.FreeSingleton;
end;

class function TBoldPropagatorServer.Instance: TBoldPropagatorServer;
begin
  if not Assigned(G_PropagatorServer) then
    G_PropagatorServer := self.Create;
  Result := G_PropagatorServer;
end;

class procedure TBoldPropagatorServer.FreeSingleton;
begin
  FreeAndNil(G_PropagatorServer);
end;

{ TBoldPropagatorServer }
constructor TBoldPropagatorServer.Create;
begin
  inherited;
  FInitialized := false;
  FComObjects := TBoldObjectArray.Create(100,[]);
end;

destructor TBoldPropagatorServer.Destroy;
begin
  FreeAndNil(dmServerHandles);
  FreeAndNil(FAdvancedPropagator);
  FreeAndNil(fLockManagerAdmin);
  FreeAndNil(fLockManager);
  FreeAndNil(FComObjects);
  FInitialized := false;
  inherited;
end;

function TBoldPropagatorServer.getAdvancedPropagator: TBoldAdvancedPropagator;
begin
  if Assigned(FAdvancedPropagator) and not (FAdvancedPropagator.Initialized) then
  begin
    FAdvancedPropagator.SetConfigurationParams(FClientNotifierPoolSize, FEnableLogging, FLogFileName,
      FErrorLogFileName, fTHreadLogFileName, FMaxLogFileSize, fDisconnectClientsOnSendFailure);
    FAdvancedPropagator.Initialize;
  end;
  Result := FAdvancedPropagator;
end;

function TBoldPropagatorServer.getModuleName: string;
begin
  if (FModuleName = '') then
    FModuleName := GetModuleFileNameAsString(true);
  Result := FModuleName;
end;

procedure TBoldPropagatorServer.Initialize;
begin
  LoadConfiguration;
  Application.Title := ServerName;
  FAdvancedPropagator := TBoldAdvancedPropagator.Create;
  dmServerHandles := TDmServerHandles.Create(nil);
  if FindCmdLineSwitch('ServerName', ['-', '/'], True) then
  begin
    SetConfiguration(ParamStr(2));
    Halt;
  end
  else
  begin
    CreateClassFactories;
    ClientHandlerCOMFactory.ClientHandler := AdvancedPropagator.ClientHandler;
    EnqueuerCOMFactory.Enqueuer := AdvancedPropagator.Enqueuer;
    FInitialized := True;
  end;
end;

procedure TBoldPropagatorServer.CreateClassFactories;
begin
 {$IFDEF BOLD_USE_CO_ADVANCED_PROPAGATOR}
  TBoldPropagatorFactory.Create(ComServer, CLASS_BoldPropagator, Format('%s.AdvancedPropagator', [Instance.ServerName]),
      Instance.ServerName);
 {$ELSE}
  TBoldComServerConnectionFactory.Create(ComServer, Instance.PropagatorConnectionCLSID,
     Format('%s.PropagatorConnection', [Instance.ServerName]), Instance.ServerName);

  {$ENDIF}
  if not Assigned(ClientHandlerComFactory) then
    ClientHandlerCOMFactory := TBoldClientHandlerThreadedCOMFactory.Create(ComServer, Instance.ClientHandlerCLSID, Format('%s.ClientHandler', [Instance.ServerName]),
      Instance.ServerName);
  if not Assigned(EnqueuerCOMFactory) then
    EnqueuerCOMFactory := TBoldEnqueuerThreadedComFactory.Create(ComServer, TBoldEnqueuerCOM, Instance.EnqueuerCLSID, Format('%s.EventPropagator', [Instance.ServerName]), 'EventPropagator',
                            ciMultiInstance, batMTA);
  if not Assigned(LockManagerCOMFactory) then
    LockManagerCOMFactory := TBoldLockManagerComFactory.Create(ComServer, Instance.LockManagerCLSID, Format('%s.LockManager', [Instance.ServerName]), 'Lock manager');
  if not Assigned(LockManagerAdminCOMFactory) then
    LockManagerAdminCOMFactory := TBoldLockManagerAdminComFactory.Create(ComServer, Instance.LockManagerAdminCLSID, Format('%s.LockManagerAdmin', [Instance.ServerName]), 'Lock manager Admin');
end;

procedure TBoldPropagatorServer.LoadConfiguration;
var
 ConfigFileName: string;
 Section: string;
 ConfigFile: TMemIniFile;
begin
  ConfigFileName := ModuleName;
  ConfigFile := TMemIniFile.Create(ChangeFileExt(ConfigFileName, '.ini'));
  try
    {Section 1: File Logging}
    Section := 'FILE LOGGING';
    FLogFileName := ChangeFileExt(ModuleName, '.log');
    FErrorLogFileName := ChangeFileExt(ModuleName, '.error');
    if ConfigFile.ReadBool(Section, 'THREAD', False) then
      fThreadLogFileName := ChangeFileExt(ModuleName, '.thread')
    else
      fThreadLogFileName := '';
    FEnableLogging := ConfigFile.ReadBool(Section, 'ENABLED', DEFAULT_ENABLELOGGING);
    FMaxLogFileSize := ConfigFile.ReadInteger(Section, 'MAXFILESIZE', DEFAULT_LOGFILESIZE);
    {Section 2: configuration parameters}
    Section := 'CONFIG PARAMS';
    FClientNotifierPoolSize := ConfigFile.ReadInteger(Section, 'THREADPOOLSIZE', DEFAULT_THREADPOOLSIZE);
    fDisconnectClientsOnSendFailure := ConfigFile.ReadBool(Section, 'DISCONNECTCLIENTSONSENDFAILURE', DEFAULT_DISCONNECT_CLIENTS_ON_SENDFAILURE);
    {Section 3: ComServer configuration params}
    Section := 'COM SERVER';
    FServerName := ConfigFile.ReadString(Section, 'SERVERNAME', 'EnterprisePropagator');
    FClientHandlerCLSID := StringToGUID( ConfigFile.ReadString(Section, 'CLSIDCLIENTHANDLER', GUIDToString(CLSID_BOLDCLIENTHANDLER)));
    FEnqueuerCLSID := StringToGUID( ConfigFile.ReadString(Section, 'CLSIDEVENTPROPAGATOR', GUIDToString(CLSID_BOLDENQUEUER)));
    FLockManagerCLSID := StringToGUID( ConfigFile.ReadString(Section, 'CLSIDLOCKMANAGER', GUIDToString(CLSID_LOCKMANAGER)));
    FLockManagerAdminCLSID := StringToGUID( ConfigFile.ReadString(Section, 'CLSIDLOCKMANAGERADMIN', GUIDToString(CLSID_LOCKMANAGERADMIN)));
    FPropagatorCLSID := StringToGUID( ConfigFile.ReadString(Section, 'CLSIDENTERPRISEPROPAGATOR', GUIDToString(BoldPropagatorConnection_CLSID)));
  finally
    FreeAndNil(ConfigFile);
  end;
end;

function TBoldPropagatorServer.getLockManager: TBoldLockManager;
begin
  if not Assigned(fLockManager) then
    fLockManager := TBoldLockManager.Create(AdvancedPropagator);
  Result := fLockManager;
end;

function TBoldPropagatorServer.getLockManagerAdmin: TBoldLockManagerAdmin;
begin
  if not Assigned(fLockManagerAdmin) then
    fLockManagerAdmin := TBoldLockManagerAdmin.Create(LockManager);
  Result := fLockManagerAdmin;
end;

procedure TBoldPropagatorServer.SetConfiguration(const ServerName: string);
var
  aServerName: string;
  ConfigFileName, tempstr: string;
  Section: string;
  ConfigFile: TMemIniFile;
  FileHandle: integer;
begin
  aServerName := Trim(ServerName);
  if Trim(aServerName) <> '' then
  begin
    tempstr := ModuleName;
    ConfigFileName := ChangeFileExt(tempstr, '.ini');
    if not FileExists(ConfigFileName) then
    begin
      FileHandle := FileCreate(ConfigFileName);
      FileClose(FileHandle);
    end;
    ConfigFile := TMemIniFile.Create(ConfigFileName);
    try
      {Section 1: File Logging}
      Section := 'COM SERVER';
      ConfigFile.WriteString(Section, 'SERVERNAME', aServerName);
      {Generate new GUIDs}
      ConfigFile.WriteString(Section, 'CLSIDCLIENTHANDLER', BoldCreateGUIDAsString);
      ConfigFile.WriteString(Section, 'CLSIDEVENTPROPAGATOR', BoldCreateGUIDAsString);
      ConfigFile.WriteString(Section, 'CLSIDLOCKMANAGER', BoldCreateGUIDAsString);
      ConfigFile.WriteString(Section, 'CLSIDLOCKMANAGERADMIN', BoldCreateGUIDAsString);
      ConfigFile.WriteString(Section, 'CLSIDENTERPRISEPROPAGATOR', BoldCreateGUIDAsString);

      ConfigFile.UpdateFile;
    finally
      FreeAndNil(ConfigFile);
    end;
  end
  else
    showmessage(Format('Invalid command line argument %s', [aServerName]));
    ;
end;

function TBoldPropagatorServer.GetAppID: TGuid;
var
  Reg: TRegistry;
  ShortFileName : string;
begin
  Reg := TRegistry.Create;
  try
    ShortFileName := ComServer.ServerFileName;
    if AnsiPos(' ', ShortFileName) <> 0 then
      ShortFileName := ExtractShortPathName(ShortFileName);
    ShortFileName := ExtractFileName(ShortFileName);
    Reg.RootKey := HKEY_CLASSES_ROOT;
    Reg.OpenKey('AppID\' + ShortFileName, false);
    if Reg.ValueExists('AppId') then
       Result := StringToGuid(Reg.ReadString('AppId'))
    else
      raise EBold.Create('AppId not found, run DCOMCNFG to add appid to registry')
  finally
    Reg.CloseKey;
    Reg.Free;
  end;
end;

procedure TBoldPropagatorServer.RegServer(const AppId: TGuid);
var
  Reg: TRegistry;
  ModuleName: string;
  vAppId: TGUID;
begin
 {$IFDEF BOLD_USE_CO_ADVANCED_PROPAGATOR}
   vAppId := CLASS_BoldPropagator;
 {$ELSE}
   vAppId := Instance.PropagatorConnectionCLSID;
 {$ENDIF}
  ModuleName := ExtractFileName(getModuleName);
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CLASSES_ROOT;
    Reg.OpenKey('AppID\' + ModuleName, true);
    Reg.WriteString('AppId', GuidToString(vAppId));
  finally
    Reg.CloseKey;
    Reg.Free;
  end;
end;

function TBoldPropagatorServer.GetComObjectCount: integer;
begin
  Result := FComObjects.Count;
end;

function TBoldPropagatorServer.GetObjectByIndex(
  Index: integer): TBoldComObject;
begin
  Result := FComObjects[Index] as TBoldComObject;
end;

procedure TBoldPropagatorServer.AddComObject(Obj: TBoldComObject);
begin
  FComObjects.Add(Obj);
end;

procedure TBoldPropagatorServer.RemoveComObject(Obj: TBoldComObject);
begin
  FComObjects.Remove(Obj);
end;

end.
