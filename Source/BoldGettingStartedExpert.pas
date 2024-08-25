
{ Global compiler directives }
{$include bold.inc}
unit BoldGettingStartedExpert;

interface

uses
  BoldWSimpleMenuWizard,
  extctrls,
  BoldGettingStartedForm,
  BoldExpertMenus;

const
  BoldGettingStartedDefaultKeyValue = 1;
  BoldGettingStartedRegKey = '\BoldGettingStarted';

type
  { forward declarations }
  TBoldGettingStartedExpert = class;

  { TBoldGettingStartedExpert }
  TBoldGettingStartedExpert = class(TSimpleMenuWizard)
  private
    MainFrm: TFGettingStarted;
    Timer: TTimer;
    function getURLGettingStarted: string;
    function DoShowGettingStarted: Boolean;
    procedure OnTimer(Sender: TObject);
    property URLGettingStarted: string read getURLGettingStarted;
  protected
    procedure Initialize; override;
  public
    destructor Destroy; override;
    procedure SetRegistryValue(const Show: Boolean);
    procedure DisplayGettingStarted(Sender: TObject);
    procedure Execute; override;
  end;

  procedure Register;
  procedure InitExpert;
  procedure DoneExpert;

var
  GettingStartedExpert: TBoldGettingStartedExpert;
  dmMenus: TDmExpertMenus = nil;

implementation

uses
  SysUtils,
  Toolsapi,
  ShellAPI,
  Windows,
  Dialogs,
  BoldCoreConsts,
  BoldUtils,
  BoldDefsDT,
  BoldRegistry,
  BoldIDEMenus,
  BoldGuard;

procedure Register;
begin
  RegisterPackageWizard(GettingStartedExpert);
end;

procedure InitExpert;
begin
  try
    dmMenus := TDMExpertMenus.Create(nil);
    BoldMenuExpert;
    GettingStartedExpert := TBoldGettingStartedExpert.Create('BoldGettingStartedExpert', 'GettingStarted', [], 5, 'Bold');
    GettingStartedExpert.AddMenuItem(dmMenus.GettingStartedMenu);
  except on E: Exception do
    showmessage(Format('InitExpert: ', [E.Message]));
  end;
end;

procedure DoneExpert;
begin
  FreeAndNil(dmMenus);
end;

{ TBoldGettingStartedExpert }

procedure TBoldGettingStartedExpert.Execute;
begin
  MainFrm := TFGettingStarted.Create(nil);
  MainFrm.cbHideGettingStarted.Checked := not DoShowGettingStarted;
  MainFrm.DisplayGettingStarted := DisplayGettingStarted;
  MainFrm.SetGettingStartedFlag := SetRegistryValue;
  MainFrm.Show;
end;

function TBoldGettingStartedExpert.getURLGettingStarted: string;
var
 Modulepath, ModuleName, temp, RootDir: string;
 Buffer: array [0..261] of Char;
 i: integer;
begin
  SetString(ModuleName, Buffer, Windows.GetModuleFileName(HInstance,
   Buffer, SizeOf(Buffer)));
  ModulePath := ExtractFilePath(ModuleName);
  temp := ExcludeTrailingPathDelimiter(ModulePath);
  i := LastDelimiter(PathDelim, temp);
  RootDir := Copy(temp, 1, i - 1);
  Result := RootDir + GETTINGSTARTEDPATH + GETTINGSTARTEDDOCNAME;
end;

procedure TBoldGettingStartedExpert.SetRegistryValue(const Show: Boolean);
var
  BoldRegistry: TBoldRegistry;
begin
  BoldRegistry := TBoldRegistry.Create;
  try
    BoldRegistry.RegistryMode := rmDesignTime;
    BoldRegistry.OpenKey(BoldGettingStartedRegKey);
    BoldRegistry.WriteBool('Show', Show);
    BoldRegistry.CloseKey;
  finally
    FreeAndNil(BoldRegistry);
  end;
end;

function TBoldGettingStartedExpert.DoShowGettingStarted: Boolean;
var
  BoldRegistry: TBoldRegistry;
begin
  BoldRegistry := TBoldRegistry.Create;
  try
    BoldRegistry.RegistryMode :=  rmDesigntime;
    BoldRegistry.OpenKey(BoldGettingStartedRegKey);
    Result := BoldRegistry.ReadBool('Show', True);
    BoldRegistry.CloseKey;
  finally
    FreeAndNil(BoldRegistry);
  end;
end;

procedure TBoldGettingStartedExpert.DisplayGettingStarted(Sender: TObject);
begin
  if not FileExists(UrlGettingStarted) then
  begin
    showmessage(Format('Could not find Bold for Delphi''s GettingStarted document: %s', [UrlGettingStarted]));
  end
  else
    ShellExecute(0, 'open', PChar(URLGettingStarted), '', '', SW_SHOWNORMAL);
end;

procedure TBoldGettingStartedExpert.OnTimer(Sender: TObject);
begin
  Timer.Enabled := false;
  FreeAndNil(Timer);
  Execute;
end;

procedure TBoldGettingStartedExpert.Initialize;
begin
  inherited;
  if DoShowGettingStarted then
  begin
    Timer := TTimer.Create(nil);
    Timer.Enabled := false;
    Timer.OnTimer := OnTimer;
    Timer.Interval := 2000;
    Timer.Enabled := true;
  end;
end;

destructor TBoldGettingStartedExpert.Destroy;
begin
  FreeAndNil(MainFrm);
  inherited;
end;

initialization
  InitExpert;

finalization
  DoneExpert;
end.
