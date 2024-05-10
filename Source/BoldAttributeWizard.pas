
{ Global compiler directives }
{$include bold.inc}
unit BoldAttributeWizard;

interface

uses
  BoldWProjectWizard,
  BoldWAInterfaces,
  BoldTemplateExpander;

type

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
  Dialogs,
  SysUtils,
  ToolsApi,

  BoldCoreConsts,
  BoldOTACodeGen,
  BoldWAdatamodule,
  BoldWAMainForm,
  BoldIDEMenus;

var
  AttributeWizardInitialized: Boolean = false;

procedure Register;
begin
    InitExpert;
    RegisterPackageWizard(AttributeWizard);
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
  Creator: TUnitCreator;
  NewModule: IOTAModule;
  SourceEditor: IOTASourceEditor;
  aUnitName, aFileName, aClassName: string;
  FullName: string;
begin

  Creator := TUnitCreator.Create;
  (BorlandIDEServices as IOTAModuleServices).GetNewModuleAndClassName('', aUnitName, aClassName, aFileName);
  if (length(UnitName) <> 0) then
    aUnitName := UnitName;
  Template.Variables.SetVariable('UNITNAME', aUnitName);
  NewModule := nil;
  try
    FullName := Format('%s%s',[ExtractFilePath(aFileName), aUnitName]);
    NewModule := Creator.CreateUnit(FullName, Template.ExpandedTemplate.Text, nil);
  except
    on e: Exception do
      MessageDlg(Format(sUnableToCreateUnit, [FullName, e.Message]), mtError, [mbOk], 0);
  end;





  if assigned(NewModule) then
  begin
    SourceEditor := NewModule.GetModuleFileEditor(0) as IOTASourceEditor;
    (SourceEditor as IOTAEditor).Show;
  end;


end;

initialization

finalization
  DoneExpert;

end.
