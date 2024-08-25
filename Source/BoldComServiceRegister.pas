
{ Global compiler directives }
{$include bold.inc}
unit BoldComServiceRegister;

interface

uses
  SvcMgr;

type
  {Forward declarations}
  TBoldComServiceRegister = class;

  {TBoldComServiceRegister}
  TBoldComServiceRegister = class
  private
    FAppID: string;
  public
    procedure DoAfterInstall(Name: string); virtual;
    procedure DoAfterUninstall(Name: string); virtual;
    procedure DoServiceShutdown; virtual;
    procedure DoServiceStop; virtual;
    procedure DoServiceStart; virtual;

    property AppID: string read FAppID write FAppID;
  end;

procedure RegisterClassFactories(const Reg: Boolean;
                                 const AppId: string;
                                 ClsIds: array of string);
procedure RegisterServerAsService(const bReg: Boolean; const ClassID, ServiceName: string);                                 

implementation

uses
  sysutils,
  BoldUtils,
  comobj,
  comserv,
  Activex,
  windows,
  registry;

procedure DeleteAppId(const RootKey: DWord; const Key: string);
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := RootKey;
    if Reg.OpenKey(Key, False)
      then Reg.DeleteValue('AppID');
  finally
    Reg.CloseKey;
    Reg.Free;
  end;
end;

{procedure RegisterServerAsService(const bReg: Boolean;
                                  const ClassID, ServiceName: string);
begin
  if bReg then
  begin
    CreateRegKey('AppID\' + ClassID, 'LocalService', ServiceName);
  end
  else
  begin
    DeleteRegKey('AppID\' + ClassID);
    DeleteAppId(HKEY_CLASSES_ROOT, 'CLSID\' + ClassID);
  end;
end;}

procedure RegisterServerAsService(const bReg: Boolean; const ClassID, ServiceName: string);
var
  Reg: TRegistry;
begin
  if bReg then
  begin
    RegisterAsService(ClassID, ServiceName);
  end
  else
  begin
    Reg := TRegistry.Create;
    try
      Reg.RootKey := HKEY_CLASSES_ROOT;
      if Reg.OpenKey('AppID\' + ClassID, False)
        then Reg.DeleteValue('LocalService');
    finally
      Reg.CloseKey;
      Reg.Free;
    end;
  end;
end;

procedure RegisterClassFactories(const Reg: Boolean;
                                 const AppId: string;
                                 ClsIds: array of string);
var
  i: integer;
begin
  if Reg then
  begin
    comserv.comserver.UpdateRegistry(true);
    for i := 0 to High(ClsIds) do
      CreateRegKey(Format('%s\%s', ['CLSID', ClsIds[i]]), 'AppID', AppId);
  end
  else
  begin
    comserv.comserver.UpdateRegistry(false);
    for i := 0 to High(ClsIds) do
      DeleteAppId(HKEY_CLASSES_ROOT, Format('%s\%s', ['CLSID', ClsIds[i]]));
  end;
end;

procedure RegisterAppIdForClass(const Reg: Boolean;
                                const AppId: string;
                                ClsIds: array of string);
var
  i: integer;
begin
  if Reg then
    for i := 0 to High(ClsIds) do
      CreateRegKey(Format('%s\%s', ['CLSID', ClsIds[i]]), 'AppID', AppId)
  else
    for i := 0 to High(ClsIds) do
      DeleteAppId(HKEY_CLASSES_ROOT, Format('%s\%s', ['CLSID', ClsIds[i]]));
end;

procedure DeleteCLSIDs(const RootKey: DWord; ClsIds: array of string);
var
  Reg: TRegistry;
  i: integer;
begin
  Reg := TRegistry.Create;
  Reg.RootKey := RootKey;
  try
    for i := 0 to High(ClsIds) do
      Reg.DeleteKey(Format('%s\%s', ['CLSID', ClsIds[i]]));
  finally
    Reg.CloseKey;
    Reg.Free;
  end;
end;

procedure TBoldComServiceRegister.DoAfterInstall(Name: string);
begin
  RegisterServerAsService(True, AppID, Name);
  RegisterClassFactories(False, AppID, [AppID]);
end;

procedure TBoldComServiceRegister.DoAfterUninstall(Name: string);
begin
  RegisterServerAsService(False, AppId, Name);
  DeleteCLSIDs(HKEY_CLASSES_ROOT, [AppID]);
end;

procedure TBoldComServiceRegister.DoServiceShutdown;
begin
  RegisterClassFactories(False, AppID, [AppID]);
end;

procedure TBoldComServiceRegister.DoServiceStart;
begin
  RegisterAppIdForClass(True, AppID, [AppID]);
end;

procedure TBoldComServiceRegister.DoServiceStop;
begin
  RegisterClassFactories(False, AppID, [AppID]);
end;

end.
