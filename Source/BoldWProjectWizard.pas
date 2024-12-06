
{ Global compiler directives }
{$include bold.inc}
unit BoldWProjectWizard;

interface

uses
  BoldWSimpleMenuWizard,
  toolsapi;

type
      {TProjectWizard}
  TProjectWizard = class(TSimpleMenuWizard)
  public
    procedure Execute; override;
    function GetCurrentProject(var Project: IOTAProject): Boolean;
    procedure CreateModule;
  end;

  TOTAProject = class
  private
    IProject: IOTAProject;
  public
     constructor Create(ProjectIntf: IOTAProject);
     function GetModuleCount: Integer;
    { Call this function to add an arbitrary file to the project.  NOTE: some
      files have special meaning to different projects.  For example: adding
      VCL50.DCP will cause a new entry in a package project's "requires" list
      while it will be a raw file to any other project type.  Set IsUnitOrForm
      to true for files that are considered items that the project would
      process directly or indirectly (ie. .pas, .cpp, .rc, etc..) or can be
      opened in the code editor. For all others, including binary files
      (.res, .bpi, .dcp, etc..) set this to False. }
    procedure AddFile(const AFileName: string; IsUnitOrForm: Boolean);
    { Call this function to remove an arbitrary file from the project.  This
      must be a fully qualified filename.  See GetModule() above for info on
      obtaining this information from a Form name or unit name }
    procedure RemoveFile(const AFileName: string);
    { Return the Indexed owned Module Info }
    function GetModule(Index: Integer): IOTAModuleInfo;
    function CloseModule(ForceClosed: Boolean): Boolean;
    function AddNotifier(const ANotifier: IOTAModuleNotifier): Integer;
    { Attempt to close this module. True was successful and all references to
      this module must be released. False if this module was not closed. }
    function Close: Boolean;
    { Return the filename associated with this module.  This is only the base
      name used by the IDE.  Header source and forms are obtained other ways.}
    function GetFileName: string;
    { Call with the index obtained from AddNotifier }
    procedure RemoveNotifier(Index: Integer);
    { Save the module. ChangeName invokes the SaveAs logic.  ForceSave will not
      ask to save if the module is modified. Returns False if canceled
      or an error }
    function Save(ChangeName, ForceSave: Boolean): Boolean;
    function IsForm(Index: Integer): Boolean;
  end;

implementation

uses
  windows;

procedure TProjectWizard.Execute;
begin
end;

function TProjectWizard.GetCurrentProject(var Project: IOTAProject): Boolean;
var
  ModuleServices: IOTAModuleSErvices;
  CurrentModule: IOTAModule;
  i: integer;
begin
  Result := false;
  Project := nil;
  ModuleServices := (BorlandIDEServices as IOTAModuleServices);
  CurrentModule:= ModuleServices.CurrentModule;
  if Assigned(CurrentModule) then
  begin
    if (CurrentModule.QueryInterface(IOTAProject, Project) = S_OK) then
    begin
      Project := CurrentModule as IOTAProject;
      Result := true;
    end;
    if not Result then
      if (CurrentModule.GetOwnerModuleCount > 0) then
        for i:= 0 to CurrentModule.OwnerCount - 1 do
          if (CurrentModule.GetOwnerModule(i).QueryInterface(IOTAProject, Project) = S_OK) then
          begin
            Project := CurrentModule.GetOwnerModule(i) as IOTAProject;
            Result := true;
            Break;
          end;

    if not Result then
    if (CurrentModule.GetOwnerModuleCount > 0) then
      for i:= 0 to CurrentModule.OwnerCount - 1 do
        if (CurrentModule.GetOwnerModule(i).QueryInterface(IOTAProject, Project) = S_OK) then
        begin
          Project := CurrentModule.GetOwnerModule(i) as IOTAProject;
          Result := true;
          Break;
        end;
  end;
end;

procedure TProjectWizard.CreateModule;
begin
end;

{TOTAProject}
constructor TOTAProject.Create(ProjectIntf: IOTAProject);
begin
  IProject := ProjectIntf;
end;

function TOTAProject.GetModuleCount: Integer;
begin
  {delegate call to IProject interface}
  Result := IProject.GetModuleCount;
end;

procedure TOTAProject.AddFile(const AFileName: string; IsUnitOrForm: Boolean);
begin

end;

procedure TOTAProject.RemoveFile(const AFileName: string);
begin

end;

function TOTAProject.GetModule(Index: Integer): IOTAModuleInfo;
begin
  Result := IProject.GetModule(Index);
end;

function TOTAProject.CloseModule(ForceClosed: Boolean): Boolean;
begin

  Result := false;
end;

function TOTAProject.AddNotifier(const ANotifier: IOTAModuleNotifier): Integer;
begin
  Result := IProject.AddNotifier(ANotifier);
end;

function TOTAProject.Close: Boolean;
begin
  Result := IProject.Close;
end;

function TOTAProject.GetFileName: string;
begin
  Result := IProject.GetFileName;
end;

procedure TOTAProject.RemoveNotifier(Index: Integer);
begin
  IProject.RemoveNotifier(Index);
end;

function TOTAProject.Save(ChangeName, ForceSave: Boolean): Boolean;
begin
  Result := IProject.Save(ChangeName, ForceSave);
end;

function TOTAProject.IsForm(Index: Integer): Boolean;
var
  ModuleInfo: IOTAModuleInfo;
begin
  ModuleInfo := IProject.GetModule(Index);
  Result := false;
end;

end.
