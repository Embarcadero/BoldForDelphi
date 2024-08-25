
{ Global compiler directives }
{$include bold.inc}
unit BoldOTASupport;

interface

uses
  Windows,
  Classes,
  ToolsAPI,
  BoldDefs;

type
  { forward declarations }
  TBoldModuleCreator = class;
  TBoldUnitFile = class;

  {---TBoldModuleCreator---}
  TBoldModuleCreator = class(TInterfacedObject,IOTACreator,IOTAModuleCreator)
  private
    FFileName: string;
    fModuleType: TBoldModuleType;
    fShowInEditor: Boolean;
  protected
    procedure FormCreated(const FormEditor: IOTAFormEditor);
    function GetAncestorName: string;
    function GetCreatorType: string;
    function GetExisting: Boolean;
    function GetFileSystem: string;
    function GetFormName: string;
    function GetImplFileName: string;
    function GetIntfFileName: string;
    function GetMainForm: Boolean;
    function GetOwner: IOTAModule;
    function GetShowForm: Boolean;
    function GetShowSource: Boolean;
    function GetUnnamed: Boolean;
    function NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
    function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    function NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
  public
    constructor Create(const FileName: string; const ModuleType: TBoldModuleType; ShowInEditor: Boolean);
  end;

  TBoldUnitFile = class(TInterfacedObject,IOTAFile)
  private
    FUnitIdent: string;
  protected
    function GetAge: TDateTime;
    function GetSource: string;
  public
    constructor Create(const UnitIdent: string);
  end;


function BoldFilePathForComponent(component: TComponent): string;
function GetOTAProject: IOTAProject;
function FindFileModuleInProject(const fileName: String; Project: IOTAProject): IOTAModule;
function GetProjectPath(Project: IOTAProject): String;
function GetProjectOtherPaths(Project: IOTAProject): string;
function EnsuredModule(FileName: string; ModuleCreator: TBoldModuleCreator; AllowLoadFromDisk: Boolean; var WasOpen: Boolean): IOTAModule;

var
  OTAModuleServices: IOTAModuleServices;
  OTAActionServices: IOTAActionServices;
  OTADEBUG: boolean = False;

implementation

uses
  SysUtils,
  IniFiles,
  Registry,

  BoldCoreConsts,
  BoldLogHandler,
  BoldUtils;

function GetProjectSearchPath(Project: IOTAProject): String;
var
  DofFileName: string;
begin
  result := '';
  if assigned(project) then
  begin
    Result := ExtractFilepath(Project.FileName) + ';';

    DofFileName := ChangeFileExt(project.FileName, '.DOF');

    with TINIFile.Create(DofFileName) do
      try
        Result := Result + ReadString('DIRECTORIES', 'SearchPath', '');
      finally
        free;
      end;
  end;
end;

function GetDelphiSearchPath: String;
begin
  Result := '';
  with TRegistry.Create do
  begin
    try
      if OpenKey(BOLD_HOST_IDE_REGISTRYPATH + 'Library', False) then
        Result := ReadString('Search Path');
      CloseKey;
    finally
      Free;
    end;
  end;
end;

function FindDelphiModuleForFile(FileName: String): IOTAModule;
var
  i: integer;
begin
  for i := 0 to OTAModuleServices.ModuleCount - 1 do
    if SameFileName(ExtractFileName(OTAModuleServices.Modules[i].FileName), FileName) then
    begin
      result := OTAModuleServices.Modules[i];
      exit;
    end;
end;

function OpenExistingFileInDelphi(FileName: String; SearchPath: String): IOTAModule;
var
  p: integer;
  path: string;
begin
  result := nil;
  Ensuretrailing(SearchPath, ';');
  if OTADEBUG then
    BoldLog.LogFmt('Looking with searchpath:  %s', [SearchPath]);

  while SearchPath <> '' do
  begin
    p := pos(';', SearchPath);
    path := copy(SearchPath, 1, p - 1);
    Path := IncludeTrailingPathDelimiter(path);
    Delete(SearchPath, 1, p);
    if FileExists(path + FileName) then
    begin
      OTAActionServices.OpenFile(path + FileName);
      result := FindDelphiModuleForFile(FileName);
      if assigned(result) then
      begin
        if OTADEBUG then
          BoldLog.LogFmt('Loaded and Opened the file %s', [path + filename]);
        exit;
      end;
    end;
  end;
end;

function EnsuredModule(FileName: string; ModuleCreator: TBoldModuleCreator; AllowLoadFromDisk: Boolean; var WasOpen: Boolean): IOTAModule;
var
  Project: IOTAProject;
begin
  result := nil;
  if OTADEBUG then
    BoldLog.LogFmt('Looking in ModuleServices for %s', [FileName]);
  result := FindDelphiModuleForFile(FileName);

  WasOpen := assigned(Result);
  
  if not Assigned(Result) then
  begin
    Project := GetOTAProject;
    if assigned(Project) then
    begin
      if OTADEBUG then
        BoldLog.LogFmt('Looking in Project for %s', [FileName]);
      result := FindFileModuleInProject(FileName, Project);

      if not assigned(result) then
      begin
        if OTADEBUG then
          BoldLog.LogFmt('Looking in Project SearchPath for %s', [FileName]);
        result := OpenExistingFileInDelphi(FileName, GetProjectSearchPath(Project));
      end;

      if not Assigned(Result) then
      begin
        if OTADEBUG then
          BoldLog.LogFmt('Looking in folders of other files in project for %s', [FileName]);
        result := OpenExistingFileInDelphi(FileName, GetProjectOtherPaths(Project));
      end;
    end;
  end;

  if not assigned(result) then
  begin
    if OTADEBUG then
      BoldLog.LogFmt('Looking in Delphi SearchPath for %s', [FileName]);
    result := OpenExistingFileInDelphi(FileName, GetDelphiSearchPath);
  end;

  if not assigned(result) then
  begin
    if OTADEBUG then
      BoldLog.LogFmt('Creating New module for %s', [FileName]);
    result := OTAModuleServices.CreateModule(ModuleCreator);
  end;

  if not assigned(result) then
    raise EBoldDesignTime.CreateFmt(sUnableToGetModule, [filename]);
end;

function BoldFilePathForComponent(component: TComponent): string;
var
  i, j: integer;
  Module: IOTAModule;
  FileCount: integer;
  FormEditor: IOTAFormEditor;
  editor: IOTAEditor;
  RootComponent: IOTAComponent;
  Rootname: string;
  Owner: TComponent;
begin
  owner := Component.Owner;
  result := '';
  if assigned(OTAMOduleServices) then
    for i := 0 to OTAModuleServices.ModuleCount - 1 do
    begin
      Module := OTAModuleServices.Modules[i];
      fileCount:= Module.GetModuleFileCount;
      for j := 0 to FileCount - 1 do
      begin
        Editor := Module.GetModuleFileEditor(j);
        if Editor.QueryInterface(IOTAFormEditor, FormEditor) = S_OK then
        begin
          RootComponent := FormEditor.GetRootComponent;



          if assigned(FormEditor.FindComponent(Component.Name)) and
          RootComponent.GetPropValueByName('Name', RootName) and
            (RootName = Owner.Name) then
          begin
            Result := IncludeTrailingPathDelimiter(Trim(ExtractFilePath(Module.FileName)));
            Exit;
          end;
        end;
      end;
    end
  else
    result := ExtractFilePath(ParamStr(0));
end;

function GetOTAProject: IOTAProject;
begin
  result := (BorlandIDEServices as IOTAModuleServices).GetActiveProject;
  if OTADEBUG then
  begin
    if Assigned(result) then
      BoldLog.LogFmt('CurrentModule:', [result.FileName])
    else
      BoldLog.Log('CurrentModule not found');
        end;
    end;

function FindFileModuleInProject(const fileName: String; Project: IOTAProject): IOTAModule;
var
  i: integer;
begin
  result := nil;
  if assigned(Project) then
    for i := 0 to Project.GetModuleCount - 1 do
      if SameFileName(ExtractFileName(Project.GetModule(i).GetFileName), ExtractFileName(FileName)) then
      begin
        if OTADEBUG then
          BoldLog.Log('Opening existing file from project: '+ Project.GetModule(i).FileName);
        Result := Project.GetModule(i).OpenModule;
        Exit;
      end;
end;

function GetProjectPath(Project: IOTAProject): String;
begin
  Result := '';
  if Assigned(Project) then
    Result := ExtractFilePath(Project.GetFileName);
end;

function GetProjectOtherPaths(Project: IOTAProject): string;
var
  i: integer;
begin
  Result := '';
  if assigned(Project) then
    for i := 0 to Project.GetModuleCount - 1 do
      Result := Result + ExtractFilePath(Project.GetModule(i).GetFileName) + ';';
end;

{---TBoldModuleCreator---}

constructor TBoldModuleCreator.Create(const FileName: string; const ModuleType: TBoldModuleType; ShowInEditor: Boolean);
begin
  FFileName := FileName;
  fModuleType := ModuleType;
  fShowInEditor := ShowInEditor;
end;

function TBoldModuleCreator.GetCreatorType: string;
begin
  case fModuleType of
    mtUnit: Result := sUnit;
    mtText: Result := sText;
    mtIncFile: Result := sText;
  else
    result := '';
  end;
end;

function TBoldModuleCreator.GetExisting: Boolean;
begin
  result := false;
  if FileExists(fFilename) then
    result := true;

  if not result and OTADEBUG then
    BoldLog.LogFmt('%s does not exist', [fFileName]);
end;

function TBoldModuleCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TBoldModuleCreator.GetOwner: IOTAModule;
var
  Module: IOTAModule;
begin
  case fModuleType of
    mtIncFile,
    mtUnit: begin
      Module := (BorlandIDEServices as IOTAModuleServices).CurrentModule;
      if Assigned(Module) and (Module.OwnerCount > 0) then
        Result := Module.Owners[0]
      else
        Result := nil;
    end;
    mtText: result := nil;
  end;
end;

function TBoldModuleCreator.GetUnnamed: Boolean;
begin
  Result := true;
end;

function TBoldModuleCreator.GetAncestorName: string;
begin
  Result := '';
end;

function TBoldModuleCreator.GetImplFileName: string;
begin
  Result := FFileName;
end;

function TBoldModuleCreator.GetIntfFileName: string;
begin
  Result := '';
end;

function TBoldModuleCreator.GetFormName: string;
begin
  Result := '';
end;

function TBoldModuleCreator.GetMainForm: Boolean;
begin
  Result := False;
end;

function TBoldModuleCreator.GetShowForm: Boolean;
begin
  Result := False;
end;

function TBoldModuleCreator.GetShowSource: Boolean;
begin
  Result := true;
end;

function TBoldModuleCreator.NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

function TBoldModuleCreator.NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := TBoldUnitFile.Create(ChangeFileExt(ExtractFileName(FFileName),''));
end;

function TBoldModuleCreator.NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

procedure TBoldModuleCreator.FormCreated(const FormEditor: IOTAFormEditor);
begin
end;   

constructor TBoldUnitFile.Create(const UnitIdent: string);
begin
  FUnitIdent := UnitIdent;
end;

function TBoldUnitFile.GetSource: string;
begin
  Result := '';
end;

function TBoldUnitFile.GetAge: TDateTime;
begin
  Result := -1;
end;

initialization
  OTAModuleServices := BorlandIDEServices as IOTAModuleServices;
  OTAActionServices := BorlandIDEServices as IOTAActionServices;
  if assigned(OTAModuleServices) and Assigned(OTAActionServices) then
    BoldRunningAsDesignTimePackage := true;

end.