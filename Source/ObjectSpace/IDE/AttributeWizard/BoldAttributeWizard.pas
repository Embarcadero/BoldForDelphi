unit BoldAttributeWizard;

// NB! The attribute wizard is disabled in BCB. Check the Register-procedure

interface

uses
  BoldWProjectWizard,
  BoldWAInterfaces,
  BoldTemplateExpander;

type
  { TAttributeWizard }
  TAttributeWizard = class(TProjectWizard, IUnitGenerator)
  private
    procedure GenerateUnit(const UnitName: string; Template: TBoldTemplateHolder);
  public
    procedure Execute; override;
  end;

procedure Register;
procedure InitExpert;
procedure DoneExpert;

var
  AttributeWizard: TAttributeWizard;

implementation

uses
  SysUtils,
  toolsapi,
  BoldOTACodeGen,
  BoldWAdatamodule,
  BoldWAMainForm,
  BoldIDEMenus,
  dialogs,
  BoldCoreConsts;

var
  AttributeWizardInitialized: Boolean = false;

procedure Register;
begin
  {$IFDEF BOLD_DELPHI}
  InitExpert;
  RegisterPackageWizard(AttributeWizard);
  {$ENDIF}
end;

procedure InitExpert;
begin
  dmAttributeWizard := TdmAttributeWizard.Create(nil);
  AttributeWizard := TAttributeWizard.Create('Bold.AttributeWizard', sBoldAttributeWizard, [], 3, 'Bold'); // do not localize
  BoldMenuExpert;  // ensure "Bold" menu has been created
  AttributeWizard.AddMenuItem(dmAttributeWizard.AttributeWizardMenu);
  AttributeWizardInitialized := true;
end;

procedure DoneExpert;
begin
  if AttributeWizardInitialized then
    FreeAndNil(dmAttributeWizard);
  AttributeWizardInitialized := false;
end;

    { TAttributeWizard }
procedure TAttributeWizard.Execute;
begin
  MainForm := TMainForm.Create(nil);
  MainForm.fUnitGenerator := IUnitGenerator(self);
  MainForm.ShowModal;
  FreeAndNil(MainForm);
end;

procedure TAttributeWizard.GenerateUnit(const UnitName: string; Template: TBoldTemplateHolder);
var
//  CurrentProject: IOTAProject;
  Creator: TUnitCreator;
  NewModule: IOTAModule;
  SourceEditor: IOTASourceEditor;
  aUnitName, aFileName, aClassName: string;
  FullName: string;
begin
//  GetCurrentProject(CurrentProject);
  //create a module, needs an IOTACreator
  Creator := TUnitCreator.Create;
  //get a new file and unit name from Delphi
  (BorlandIDEServices as IOTAModuleServices).GetNewModuleAndClassName('', aUnitName, aClassName, aFileName);
  //use the new unitname if user has not specified one.
  if (length(UnitName) <> 0) then
    aUnitName := UnitName;
  Template.Variables.SetVariable('UNITNAME', aUnitName); // do not localize
  NewModule := nil;
  try
    FullName := Format('%s%s',[ExtractFilePath(aFileName), aUnitName]);
//    NewModule := Creator.CreateUnit(Format('%s%s',[ExtractFilePath(aFileName), aUnitName]), Template.ExpandedTemplate.Text, CurrentProject as IOTAModule);
    NewModule := Creator.CreateUnit(FullName, Template.ExpandedTemplate.Text, nil);
  except
    on e: Exception do
      MessageDlg(Format(sUnableToCreateUnit, [FullName, e.Message]), mtError, [mbOk], 0);
  end;
  // save file
//  NewModule.Save(true, true);
  //add to project
//  if Assigned(CurrentProject) then
//    CurrentProject.AddFile(NewModule.GetFileName, true);
  // show in editor
  if assigned(NewModule) then
  begin
    SourceEditor := NewModule.GetModuleFileEditor(0) as IOTASourceEditor;
    (SourceEditor as IOTAEditor).Show;
  end;

  // show the source editor, bring it to front
  //!!Delphi4
//  (BorlandIDEServices as IOTAEditorServices).GetTopView.GetEditWindow.Form.show;
end;

initialization

finalization
  DoneExpert;

end.
