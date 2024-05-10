
{ Global compiler directives }
{$include bold.inc}
unit BoldComponentValidatorIDE;

interface

uses
  Menus,
  BoldComponentValidator,
  ToolsApi;

type
  { forward declarations }
  TBoldComponentValidatorIDE = class;

  { TBoldComponentValidatorIDE }
  TBoldComponentValidatorIDE = class(TBoldComponentValidator)
  private
    fMenuItemAll: TMenuItem;
    fMenuItemCurrent: TMenuItem;
    procedure ChangeFocus;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ValidateIOTAModule(Module: IOTAModule);
    procedure ValidateFormEditor(FormEditor: IOTAFormEditor);
    procedure ValidateIOTAModules;
    Procedure ValidateIOTAComponent(Component: IOTAComponent; NamePrefix: String = '');
    procedure ValidateAllMenuAction(Sender: TObject);
    procedure ValidateCurrentMenuAction(Sender: TObject);
  end;

procedure Register;

implementation

uses
  Windows,
  Classes,
  SysUtils,
  BoldLogHandler,
  BoldGuard,
  BoldIDEMenus,
  BoldEnvironment,
  BoldCoreConsts;

var
  G_BoldComponentValidatorIDE: TBoldComponentValidatorIDE = nil;

procedure Register;
begin
    G_BoldComponentValidatorIDE := TBoldComponentValidatorIDE.Create;
end;

{ TBoldComponentValidatorIDE }
constructor TBoldComponentValidatorIDE.Create;
begin
  fMenuItemAll := BoldMenuExpert.AddMenuItem('mnuValidateAllForms', // do not localize
                                             sValidateAllForms,
                                             ValidateAllMenuAction,
                                             True);
  fMenuItemCurrent := BoldMenuExpert.AddMenuItem('mnuValidateCurrentForm', // do not localize
                                                 sValidateCurrentForm,
                                                 ValidateCurrentMenuAction,
                                                 True);
end;

destructor TBoldComponentValidatorIDE.Destroy;
begin
  if BoldMenuExpertAssigned then
  begin
    if assigned(fMenuItemAll) then
      BoldMenuExpert.RemoveAndDestroyMenuItem(fMenuItemAll);
    if assigned(fMenuItemCurrent) then
      BoldMenuExpert.RemoveAndDestroyMenuItem(fMenuItemCurrent);
  end;
  inherited;
end;

procedure TBoldComponentValidatorIDE.ValidateIOTAModule(Module: IOTAModule);
var
  i: integer;
  Editor: IOTAEditor;
  FormEditor: IOTAFormEditor;
  HasEditor: boolean;
begin
  if not Assigned(Module) then
    Exit;

  HasEditor := False;
  try
    for i := 0 to Module.GetModuleFileCount - 1 do
    begin
      Editor := Module.GetModuleFileEditor(i);
      if Editor.QueryInterface(IOTAFormEditor, FormEditor) = S_OK then
      begin
        BoldLog.LogFmtIndent(sValidatingForm, [FormEditor.FileName]);
        ValidateFormEditor(FormEditor);
        HasEditor := True;
        BoldLog.Dedent;
      end;
    end;
  except
    on e: Exception do
      BoldLog.LogFmt(sFailedToValidate, [Module.FileName, e.Message]);
  end;
  if not HasEditor then
    BoldLog.LogFmt(sNothingToValidate, [Module.FileName]);
end;

procedure TBoldComponentValidatorIDE.ValidateIOTAModules;
var
  i: integer;
  ModuleServices: IOTAModuleServices;
  ProjectGroup: IOTAProjectGroup;
  Project: IOTAProject;
  Module: IOTAModule;
  OTAModuleInfo: IOTAModuleInfo;
  FileNames: TStringList;
  Guard: IBoldGuard;
begin
  // Filenames of open modules are stored and not revalidated later.
  Guard := TBoldGuard.Create(FileNames);
  FileNames := TStringList.Create;
  ModuleServices := (BorlandIDEServices as IOTAModuleServices);
  BoldLog.LogIndent(sValidatingAllOpenModules);
  BoldLog.ProgressMax := ModuleServices.GetModuleCount;
  ChangeFocus;
  for i := 0 to ModuleServices.GetModuleCount - 1 do
  begin
    ValidateIOTAModule(ModuleServices.GetModule(i));
    FileNames.Add(ModuleServices.Modules[i].FileName);
    BoldLog.ProgressStep;
  end;
  BoldLog.Dedent;

  ChangeFocus;
  BoldLog.LogIndent(sLookingForDefaultProject);
  for i := 0 to ModuleServices.GetModuleCount - 1 do
  begin
    if ModuleServices.GetModule(i).QueryInterface(IOTAProjectGroup, ProjectGroup) = S_OK then
    begin
      Project := ProjectGroup.ActiveProject;
      BoldLog.LogFmt(sDefaultProject, [Project.FileName]);
      Break;
    end;
  end;
  BoldLog.Dedent;

  ChangeFocus;
  if not Assigned(Project) then
  begin
    BoldLog.LogIndent(sLookingForAnyProject);
    for i := 0 to ModuleServices.GetModuleCount - 1 do
    begin
      if ModuleServices.GetModule(i).QueryInterface(IOTAProject, Project) = S_OK then
      begin
        BoldLog.LogFmt(sFoundProject, [Project.FileName]);
        Break;
      end;
    end;
  end;
  BoldLog.Dedent;

  ChangeFocus;
  BoldLog.ProgressMax := Project.GetModuleCount - 1;
  BoldLog.Progress := 0;
  for i := 0 to Project.GetModuleCount - 1 do
  begin
    OTAModuleInfo := Project.GetModule(i);
    try
      Module := OTAModuleInfo.OpenModule;
      if FileNames.IndexOf(Module.FileName) = - 1 then
      begin
        ValidateIOTAModule(Module);
        Module.CloseModule(True);
      end;
      BoldLog.ProgressStep;
    except
    end;
  end;
  BoldLog.EndLog;
end;

procedure TBoldComponentValidatorIDE.ValidateFormEditor(FormEditor: IOTAFormEditor);
begin
  if assigned(FormEditor) then
  begin
    FormEditor.Show;
    if assigned(FormEditor.GetRootComponent) then
      ValidateIOTAComponent(FormEditor.GetRootComponent);
  end;
end;

procedure TBoldComponentValidatorIDE.ValidateIOTAComponent(Component: IOTAComponent; NamePrefix: String = '');
var
  NTAComponent: INTAComponent;
  RealComponent: TComponent;
  i: integer;
  ComponentName: string;
begin
  RealComponent := nil;
  ComponentName := '';
  if Component.QueryInterface(INTAComponent, NTAComponent) = S_OK then
  begin
    RealComponent := NTAComponent.GetComponent;
    if Assigned(RealComponent) then
      ComponentName := RealComponent.Name;
  end;

  if assigned(RealComponent) then
    ValidateComponent(RealComponent, NamePrefix);

  for i := 0 to Component.GetComponentCount - 1 do
    ValidateIOTAComponent(Component.GetComponent(i),
                          NamePrefix + ComponentName + '.');
end;

procedure TBoldComponentValidatorIDE.ValidateAllMenuAction(Sender: TObject);
begin
  InitializeLog;
  ValidateIOTAModules;
  CompleteLog;

{  if assigned(LastFailedComponent) then
    LastFailedComponent.Select(false);
  fLastFailedComponent := nil;
}
end;

procedure TBoldComponentValidatorIDE.ValidateCurrentMenuAction(Sender: TObject);
var
  ModuleServices: IOTAModuleServices;
  Module: IOTAModule;
begin
  ModuleServices := (BorlandIDEServices as IOTAModuleServices);
  Module := ModuleServices.CurrentModule;
  if Assigned(Module) then
  begin
    InitializeLog;
    ValidateIOTAModule(Module);
    CompleteLog;
  end
  else
    raise Exception.Create(sNoValidateableModuleAvailable);
end;

procedure TBoldComponentValidatorIDE.ChangeFocus;
begin
  BoldEffectiveEnvironment.FocusMainform;
end;

initialization

finalization
  FreeAndNil(G_BoldComponentValidatorIDE);

end.
